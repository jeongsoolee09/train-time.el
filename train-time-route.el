;;; train-time-route.el --- Plan a journey between two stations -*- lexical-binding: t; -*-

;; Copyright (C) 2026  train-time.el contributors

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; M-x train-time-route plans a journey between any two stations.
;;
;; The network is a graph of scheduled run times: for each route,
;; direction and pair of consecutive stops, the median time a train
;; takes from arriving at one to arriving at the next, worked out from
;; MTA's static timetable (`train-time-mta-data-segments').  Riding
;; on means staying on a route in one direction; changing route or
;; direction, at a station or within a station complex, costs
;; `train-time-transfer-time'.  Dijkstra's algorithm finds the
;; quickest journey in that graph.
;;
;; Times are estimates for the usual daytime service.  They leave out
;; waiting for trains, and do not account for service changes, so a
;; plan is a starting point rather than a promise.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'train-time)
(require 'train-time-commute)

(declare-function train-time-alerts-summary "train-time-alerts" (routes stations now))

;;;; Customization

(defcustom train-time-route-daytime-only t
  "Non-nil means plan journeys with the daytime service only.
Stops a route makes only at night or on weekends, such as the 4
calling at local stations in Brooklyn late at night, are then left
out, as are those not listed under Daytime Routes in Stations.csv."
  :type 'boolean
  :group 'train-time)

(defcustom train-time-route-min-trips 10
  "Fewest weekly scheduled trips a stretch of a route needs to be used.
Stretches run less often, typically by trains rerouted for
engineering work, are left out of journey plans."
  :type 'natnum
  :group 'train-time)

(defcustom train-time-route-change-penalty 300
  "Extra seconds each change counts for when comparing journeys.
It stands for the wait for the next train, which the timetable
cannot predict, and makes plans prefer fewer changes.  It is not
part of the times shown."
  :type 'natnum
  :group 'train-time)

(defface train-time-route-heading
  '((t :inherit bold))
  "Face for the stations at the top of a journey plan.")

;;;; Graph

(cl-defstruct (train-time-route-leg (:copier nil))
  "A ride on one route in one direction."
  (route-id nil :documentation "Route identifier, e.g. \"7X\".")
  (direction nil :documentation "`north' or `south'.")
  (from nil :documentation "Station boarded at.")
  (to nil :documentation "Station left at.")
  (stops nil :documentation "Number of stops ridden.")
  (seconds nil :documentation "Scheduled run time in seconds.")
  (alternatives nil :documentation "Other route IDs about as quick between the stations."))

(cl-defstruct (train-time-route-plan (:copier nil))
  "A journey from one station to another."
  (origin nil :documentation "Station the journey starts at.")
  (destination nil :documentation "Station the journey ends at.")
  (legs nil :documentation "The `train-time-route-leg's, in order.")
  (seconds nil :documentation "Scheduled time, changes included.")
  (cost nil :documentation "Quickest SECONDS plus `train-time-route-change-penalty' a change."))

(defvar train-time-route--graph nil
  "Cached (KEY RIDES BOARDINGS TRIPS); see `train-time-route--graph'.")

(defun train-time-route--serves-p (route-id stop-id)
  "Return non-nil if ROUTE-ID serves STOP-ID in the daytime."
  (let ((row (assoc stop-id train-time-mta-data-stations)))
    (and row
         (member (car (train-time-mta--route route-id))
                 (split-string (or (nth 7 row) ""))))))

(defun train-time-route--graph ()
  "Return (RIDES BOARDINGS TRIPS) for the current settings.
RIDES maps a node (STOP-ID ROUTE-ID DIRECTION), a train at a stop,
to its next stops as (NODE . SECONDS).  BOARDINGS maps a complex ID
to the nodes a train can be boarded at in it.  TRIPS maps a node to
the weekly trains leaving it."
  (let ((key (list train-time-route-daytime-only train-time-route-min-trips
                   train-time-mta-data-version)))
    (unless (equal (car train-time-route--graph) key)
      (let ((rides (make-hash-table :test #'equal))
            (boardings (make-hash-table :test #'equal))
            (frequency (make-hash-table :test #'equal)))
        (pcase-dolist (`(,route ,direction ,from ,to ,seconds ,trips)
                       train-time-mta-data-segments)
          (when (and (>= trips train-time-route-min-trips)
                     (or (not train-time-route-daytime-only)
                         (and (train-time-route--serves-p route from)
                              (train-time-route--serves-p route to)))
                     (train-time-mta-station from)
                     (train-time-mta-station to))
            (let ((node (list from route direction)))
              (unless (gethash node rides)
                (push node (gethash (train-time-station-complex
                                     (train-time-mta-station from))
                                    boardings)))
              (cl-incf (gethash node frequency 0) trips)
              (push (cons (list to route direction) seconds)
                    (gethash node rides)))))
        (setq train-time-route--graph (list key rides boardings frequency))))
    (cdr train-time-route--graph)))

;;;; Search

(defun train-time-route--heap-push (heap item cost)
  "Add ITEM with COST to HEAP, a cons of a vector and a size."
  (let ((size (cdr heap)))
    (when (= size (length (car heap)))
      (setcar heap (vconcat (car heap) (make-vector (max 16 size) nil))))
    (let ((v (car heap)) (i size))
      (aset v i (cons cost item))
      (setcdr heap (1+ size))
      (while (and (> i 0)
                  (< (car (aref v i)) (car (aref v (/ (1- i) 2)))))
        (let ((parent (/ (1- i) 2)))
          (cl-rotatef (aref v i) (aref v parent))
          (setq i parent))))))

(defun train-time-route--heap-pop (heap)
  "Remove and return the cheapest (COST . ITEM) in HEAP, or nil."
  (let ((v (car heap)) (size (cdr heap)))
    (when (> size 0)
      (let ((top (aref v 0)))
        (cl-decf size)
        (aset v 0 (aref v size))
        (aset v size nil)
        (setcdr heap size)
        (let ((i 0) (done nil))
          (while (not done)
            (let* ((l (1+ (* 2 i))) (r (1+ l)) (m i))
              (when (and (< l size) (< (car (aref v l)) (car (aref v m)))) (setq m l))
              (when (and (< r size) (< (car (aref v r)) (car (aref v m)))) (setq m r))
              (if (= m i)
                  (setq done t)
                (cl-rotatef (aref v i) (aref v m))
                (setq i m)))))
        top))))

(defun train-time-route--legs (nodes)
  "Return the legs ridden through NODES, with their RIDES costs.
NODES are (NODE . SECONDS) pairs, SECONDS being the time from the
previous node, or nil where the journey changes trains."
  (let (legs current)
    (pcase-dolist (`((,stop ,route ,direction) . ,seconds) nodes)
      (let ((station (train-time-mta-station stop)))
        (if (and current seconds
                 (equal route (train-time-route-leg-route-id current))
                 (eq direction (train-time-route-leg-direction current)))
            (progn
              (setf (train-time-route-leg-to current) station)
              (cl-incf (train-time-route-leg-stops current))
              (cl-incf (train-time-route-leg-seconds current) seconds))
          (setq current (make-train-time-route-leg
                         :route-id route :direction direction
                         :from station :to station :stops 0 :seconds 0))
          (push current legs))))
    (nreverse (cl-remove-if (lambda (leg) (zerop (train-time-route-leg-stops leg)))
                            legs))))

(defun train-time-route--ride (rides route direction from to)
  "Return (SECONDS . STOPS) riding ROUTE going DIRECTION from FROM to TO.
FROM and TO are stop IDs and RIDES is the graph's; return nil if
ROUTE does not ride from one to the other."
  (let ((best (make-hash-table :test #'equal))
        (queue (list (list from 0 0))))
    (while queue
      (pcase-let ((`(,stop ,seconds ,stops) (pop queue)))
        (when (< seconds (car (gethash stop best (list most-positive-fixnum))))
          (puthash stop (cons seconds stops) best)
          (unless (equal stop to)
            (pcase-dolist (`((,next ,_ ,_) . ,more)
                           (gethash (list stop route direction) rides))
              (push (list next (+ seconds more) (1+ stops)) queue))))))
    (gethash to best)))

(defun train-time-route--choose (leg graph)
  "Make LEG ride the most frequent route about as quick, using GRAPH.
Those are the routes stopping at both ends of LEG in its direction
and taking at most a fifth longer, or a minute; a rush-hour express
then gives way to the local running all day.  LEG's other such
routes become its alternatives."
  (pcase-let* ((`(,rides ,boardings ,frequency) graph)
               (from (train-time-station-id (train-time-route-leg-from leg)))
               (to (train-time-station-id (train-time-route-leg-to leg)))
               (direction (train-time-route-leg-direction leg))
               (fastest (train-time-route-leg-seconds leg))
               (limit (max (+ 60 fastest) (* 1.2 fastest)))
               (candidates nil))
    (pcase-dolist (`(,stop ,route ,way)
                   (gethash (train-time-station-complex (train-time-route-leg-from leg))
                            boardings))
      (when (and (equal stop from) (eq way direction))
        (let ((ride (if (equal route (train-time-route-leg-route-id leg))
                        (cons fastest (train-time-route-leg-stops leg))
                      (train-time-route--ride rides route direction from to))))
          (when (and ride (<= (car ride) limit))
            (push (list route (gethash (list stop route way) frequency 0) ride)
                  candidates)))))
    (setq candidates (sort candidates (lambda (a b)
                                        (or (> (nth 1 a) (nth 1 b))
                                            (and (= (nth 1 a) (nth 1 b))
                                                 (string< (car a) (car b)))))))
    (pcase-let ((`(,route ,_ (,seconds . ,stops)) (car candidates)))
      (setf (train-time-route-leg-route-id leg) route
            (train-time-route-leg-seconds leg) seconds
            (train-time-route-leg-stops leg) stops
            (train-time-route-leg-alternatives leg) (mapcar #'car (cdr candidates))))
    leg))

(defun train-time-route-plan (origin destination &optional transfer-time)
  "Return the quickest scheduled `train-time-route-plan' or nil.
ORIGIN and DESTINATION are `train-time-station's.  Changing trains
takes TRANSFER-TIME seconds, by default `train-time-transfer-time',
as does starting or ending at another station of their complexes.
Journeys are compared counting `train-time-route-change-penalty'
for each change, so a slightly slower one with fewer changes wins."
  (let* ((transfer (or transfer-time train-time-transfer-time))
         (change (+ transfer train-time-route-change-penalty))
         (graph (train-time-route--graph))
         (rides (car graph))
         (boardings (nth 1 graph))
         (complex-of (lambda (stop)
                       (train-time-station-complex (train-time-mta-station stop))))
         (origin-id (train-time-station-id origin))
         (goal-complex (train-time-station-complex destination))
         (goal-id (train-time-station-id destination))
         (best (make-hash-table :test #'equal))
         (previous (make-hash-table :test #'equal))
         (changed (make-hash-table :test #'equal))
         (heap (cons (make-vector 64 nil) 0))
         (relax (lambda (node cost from via-change)
                  (when (< cost (gethash node best most-positive-fixnum))
                    (puthash node cost best)
                    (puthash node from previous)
                    (puthash node via-change changed)
                    (train-time-route--heap-push heap node cost))))
         (found nil))
    (dolist (node (gethash (train-time-station-complex origin) boardings))
      (funcall relax node (if (equal (car node) origin-id) 0 transfer) nil t))
    (while (and (not found) (> (cdr heap) 0))
      (pcase-let ((`(,cost . ,node) (train-time-route--heap-pop heap)))
        (cond
         ((eq node 'goal) (setq found cost))
         ((> cost (gethash node best most-positive-fixnum)))
         (t
          (pcase-dolist (`(,next . ,seconds) (gethash node rides))
            (funcall relax next (+ cost seconds) node nil))
          (let ((stop (car node)))
            (when (and (equal (funcall complex-of stop) goal-complex)
                       (not (gethash node changed)))
              (funcall relax 'goal (+ cost (if (equal stop goal-id) 0 transfer))
                       node nil))
            (dolist (next (gethash (funcall complex-of stop) boardings))
              (unless (or (equal next node)
                          (and (equal (nth 1 next) (nth 1 node))
                               (eq (nth 2 next) (nth 2 node))))
                (funcall relax next (+ cost change) node t))))))))
    (when found
      (let ((nodes nil)
            (node (gethash 'goal previous)))
        (while node
          (let ((from (gethash node previous)))
            (push (cons node (and from (not (gethash node changed))
                                  (- (gethash node best) (gethash from best))))
                  nodes)
            (setq node from)))
        (let* ((legs (train-time-route--legs nodes))
               (ride (apply #'+ (mapcar #'train-time-route-leg-seconds legs)))
               (walks (- found ride (* train-time-route-change-penalty (1- (length legs))))))
          (dolist (leg legs)
            (train-time-route--choose leg graph))
          (make-train-time-route-plan
           :origin origin :destination destination :legs legs :cost found
           :seconds (+ walks (apply #'+ (mapcar #'train-time-route-leg-seconds legs)))))))))

;;;; Display

(defvar-local train-time-route--plan nil "The plan shown in this buffer.")
(defvar-local train-time-route--origin nil "Station the shown journey starts at.")
(defvar-local train-time-route--destination nil "Station the shown journey ends at.")

(defun train-time-route--minutes (seconds)
  "Return SECONDS as rounded minutes, at least 1."
  (max 1 (round seconds 60)))

(defun train-time-route--station-button (station)
  "Return STATION's name as a button opening its board."
  (let ((name (train-time-station-display-name station)))
    (with-temp-buffer
      (insert-text-button name
                          'action (lambda (_) (train-time station))
                          'follow-link t
                          'help-echo (format "mouse-1, RET: trains at %s" name))
      (buffer-string))))

(defun train-time-route--direction-label (leg)
  "Return where LEG's trains are heading, e.g. \"Manhattan\", or nil."
  (alist-get (train-time-route-leg-direction leg)
             (train-time-station-directions (train-time-route-leg-from leg))))

(defun train-time-route--insert-leg (leg)
  "Insert LEG, giving the text the property `train-time-route-leg'."
  (let* ((start (point))
         (route (train-time-route-leg-route-id leg))
         (bullet (train-time--bullet (train-time-mta-route-bullet route) route))
         (indent (+ 2 (string-width bullet)))
         (width (max 20 (1- train-time--width)))
         (direction (train-time-route--direction-label leg))
         (stops (train-time-route-leg-stops leg))
         (alternatives (train-time-route-leg-alternatives leg)))
    (insert (train-time--hang
             (concat " " bullet " "
                     (train-time-route--station-button (train-time-route-leg-from leg))
                     " → "
                     (train-time-route--station-button (train-time-route-leg-to leg)))
             width indent)
            "\n"
            (train-time--hang
             (propertize
              (concat (make-string indent ?\s)
                      (if alternatives
                         (format "or %s · "
                                 (mapconcat #'train-time-mta-route-bullet alternatives " "))
                       "")
                      (if direction (format "toward %s · " direction) "")
                      (format "%d stop%s · %d min" stops (if (= stops 1) "" "s")
                              (train-time-route--minutes (train-time-route-leg-seconds leg))))
              'face 'shadow)
             width indent)
            "\n")
    (put-text-property start (point) 'train-time-route-leg leg)))

(defun train-time-route--change-text (from to &optional changing)
  "Return the line describing a walk from station FROM to station TO.
CHANGING non-nil means the walk is a change of trains."
  (let ((minutes (train-time-route--minutes train-time-transfer-time)))
    (train-time-route--hang-line
     (cond ((and changing (eq from to))
            (format "   Change at %s (allow %d min)\n"
                    (train-time-station-display-name from) minutes))
           (t
            (format "   %s to %s (allow %d min)\n"
                    (if changing "Change: walk" "Walk")
                    (train-time-station-display-name to) minutes))))))

(defun train-time-route--hang-line (line)
  "Return LINE, ending in a newline, italic and filled to the window."
  (concat (train-time--hang (propertize (string-trim-right line) 'face 'italic)
                            (max 20 (1- train-time--width)) 5)
          "\n"))

(defun train-time-route--no-plan-text ()
  "Return why no journey was found."
  (let ((island (lambda (station)
                  (equal (train-time-station-routes station) '("SI")))))
    (if (not (eq (funcall island train-time-route--origin)
                 (funcall island train-time-route--destination)))
        " No journey found: the Staten Island Railway meets the subway
 only by way of the Staten Island Ferry.\n\n"
      " No journey found in the daytime timetable.\n\n")))

(defun train-time-route--summary (plan)
  "Return the one-line summary of PLAN."
  (let ((changes (1- (length (train-time-route-plan-legs plan)))))
    (format "About %d min, %s"
            (train-time-route--minutes (train-time-route-plan-seconds plan))
            (pcase changes
              (0 "no changes")
              (1 "1 change")
              (_ (format "%d changes" changes))))))

(defun train-time-route--routes ()
  "Return the route IDs of the shown plan."
  (and train-time-route--plan
       (delete-dups (mapcar #'train-time-route-leg-route-id
                            (train-time-route-plan-legs train-time-route--plan)))))

(defun train-time-route--stations ()
  "Return the stations where the shown plan boards or leaves trains."
  (and train-time-route--plan
       (delete-dups
        (apply #'append
               (mapcar (lambda (leg) (list (train-time-route-leg-from leg)
                                           (train-time-route-leg-to leg)))
                       (train-time-route-plan-legs train-time-route--plan))))))

(defun train-time-route--footer (now)
  "Return the explanation below the plan at time NOW."
  (concat
   (format "Scheduled run times from MTA's timetable, plus %d min a \
change; waiting for trains is not included.  A plan uses the usual \
daytime service, so check service alerts before you go."
           (train-time-route--minutes train-time-transfer-time))
   (let ((alerts (and train-time-show-alerts (featurep 'train-time-alerts)
                      (train-time-alerts-summary (train-time-route--routes)
                                                 (train-time-route--stations)
                                                 now))))
     (if alerts (concat "\n" alerts) ""))
   "\n"
   (propertize
    (cond ((train-time--touch-bar-p)
           "Tap a leg or station for its trains.")
          ((bound-and-true-p evil-local-mode)
           "RET trains   C-j/C-k next/previous leg   r reverse   s new plan   A alerts   q quit")
          (t
           "RET trains   n/p next/previous leg   r reverse   s new plan   A alerts   q quit"))
    'face 'shadow)
   "\n" train-time-mta-attribution))

(defun train-time-route--render (&optional now)
  "Redisplay the current journey plan buffer as of NOW."
  (let* ((now (or now (funcall train-time-now-function)))
         (width (train-time--window-width))
         (plan train-time-route--plan)
         (inhibit-read-only t)
         (line (line-number-at-pos)))
    (setq train-time--width width
          train-time--compact (train-time--compact-p width))
    (erase-buffer)
    (insert (train-time--touch-bar
             '(("Reverse" train-time-route-reverse)
               ("New" train-time-route)
               ("Board" train-time-route-board)
               ("Alerts" train-time-alerts)
               ("Quit" quit-window)))
            "\n"
            (train-time--hang
             (propertize (format " %s → %s"
                                 (train-time-station-display-name train-time-route--origin)
                                 (train-time-station-display-name train-time-route--destination))
                         'face 'train-time-route-heading)
             (max 20 (1- width)) 1)
            "\n")
    (if (not plan)
        (insert (train-time-route--no-plan-text))
      (insert " " (train-time-route--summary plan) "\n\n")
      (let ((previous nil)
            (legs (train-time-route-plan-legs plan)))
        (unless (eq (train-time-route-leg-from (car legs)) train-time-route--origin)
          (insert (train-time-route--change-text
                   train-time-route--origin (train-time-route-leg-from (car legs)))))
        (dolist (leg legs)
          (when previous
            (insert (train-time-route--change-text
                     (train-time-route-leg-to previous) (train-time-route-leg-from leg) t)))
          (train-time-route--insert-leg leg)
          (setq previous leg))
        (unless (eq (train-time-route-leg-to previous) train-time-route--destination)
          (insert (train-time-route--change-text
                   (train-time-route-leg-to previous) train-time-route--destination))))
      (insert "\n"))
    (insert (train-time--fill (train-time-route--footer now) (max 20 (1- width))) "\n")
    (goto-char (point-min))
    (forward-line (1- line))
    (when (and (= line 1) (not (get-text-property (point) 'train-time-route-leg)))
      (train-time-route-next-leg))))

(defun train-time-route-refresh (&rest _)
  "Plan the shown journey again, for changed settings or alerts."
  (interactive)
  (setq train-time-route--plan
        (train-time-route-plan train-time-route--origin train-time-route--destination))
  (train-time-route--render)
  (when (and train-time-show-alerts (fboundp 'train-time-alerts-fetch))
    (let ((buffer (current-buffer)))
      (train-time-alerts-fetch
       (lambda ()
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (train-time-route--render))))))))

;;;; Commands

(defun train-time-route--leg-starts ()
  "Return the positions where legs start in this buffer."
  (let ((pos (point-min)) starts)
    (while (setq pos (next-single-property-change pos 'train-time-route-leg))
      (when (get-text-property pos 'train-time-route-leg)
        (push pos starts)))
    (nreverse starts)))

(defun train-time-route-next-leg (&optional n)
  "Move to the Nth next leg; backwards if N is negative."
  (interactive "p")
  (let* ((n (or n 1))
         (here (line-beginning-position))
         (starts (train-time-route--leg-starts))
         (target (if (> n 0)
                     (nth (1- n) (cl-remove-if-not (lambda (p) (> p here)) starts))
                   (nth (1- (- n)) (reverse (cl-remove-if-not (lambda (p) (< p here))
                                                              starts))))))
    (when target (goto-char target))))

(defun train-time-route-previous-leg (&optional n)
  "Move to the Nth previous leg."
  (interactive "p")
  (train-time-route-next-leg (- (or n 1))))

(defun train-time-route-board ()
  "Open the board of the station at point.
That is the station under point, else the one where the leg at point
boards, else the origin."
  (interactive)
  (let ((button (button-at (point)))
        (leg (get-text-property (point) 'train-time-route-leg)))
    (cond (button (button-activate button))
          (leg (train-time (train-time-route-leg-from leg)))
          (t (train-time train-time-route--origin)))))

(defun train-time-route-mouse-select (event)
  "Open the board for the leg or station clicked in EVENT."
  (interactive "e")
  (mouse-set-point event)
  (when (get-text-property (point) 'train-time-route-leg)
    (train-time-route-board)))

(defun train-time-route-reverse ()
  "Plan the journey back."
  (interactive)
  (train-time-route train-time-route--destination train-time-route--origin))

(defconst train-time-route--evil-bindings
  `("gr" train-time-route-refresh
    "r" train-time-route-reverse
    "s" train-time-route
    "b" train-time-route-board
    "A" train-time-alerts
    ,(kbd "C-j") train-time-route-next-leg
    ,(kbd "C-k") train-time-route-previous-leg
    ,(kbd "RET") train-time-route-board
    ,(kbd "<return>") train-time-route-board
    "q" quit-window)
  "Evil normal-state bindings for `train-time-route-mode'.")

(defvar train-time-route-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "g" #'train-time-route-refresh)
    (define-key map "r" #'train-time-route-reverse)
    (define-key map "s" #'train-time-route)
    (define-key map "b" #'train-time-route-board)
    (define-key map "A" #'train-time-alerts)
    (define-key map "n" #'train-time-route-next-leg)
    (define-key map "p" #'train-time-route-previous-leg)
    (define-key map (kbd "C-j") #'train-time-route-next-leg)
    (define-key map (kbd "C-k") #'train-time-route-previous-leg)
    (define-key map (kbd "RET") #'train-time-route-board)
    (define-key map [mouse-1] #'train-time-route-mouse-select)
    map)
  "Keymap for `train-time-route-mode'.")

(define-derived-mode train-time-route-mode special-mode "Train-Route"
  "Major mode showing a journey plan.

\\{train-time-route-mode-map}"
  (setq-local revert-buffer-function #'train-time-route-refresh)
  (setq truncate-lines nil)
  (train-time--watch-width #'train-time-route--render)
  (train-time-evil-setup 'train-time-route-mode train-time-route-mode-map
                         train-time-route--evil-bindings))

(defun train-time-route--read (prompt place)
  "Read a station with PROMPT, offering the one for PLACE by default.
PLACE is `home' or `work'; see `train-time-home'."
  (train-time--read-station prompt
                            (if (eq place 'home) train-time-home train-time-work)))

;;;###autoload
(defun train-time-route (origin destination)
  "Show the quickest scheduled journey from ORIGIN to DESTINATION.
Interactively, read both stations, offering home and work."
  (interactive
   (let* ((origin (train-time-route--read "From" 'home))
          (destination (train-time-route--read
                        "To" (if (equal (train-time-station-id origin) train-time-work)
                                 'home
                               'work))))
     (list origin destination)))
  (when (memq destination (train-time-mta-complex origin))
    (user-error "%s and %s are in the same station complex"
                (train-time-station-display-name origin)
                (train-time-station-display-name destination)))
  (let ((buffer (get-buffer-create "*train-time: route*")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'train-time-route-mode)
        (train-time-route-mode))
      (setq train-time-route--origin origin
            train-time-route--destination destination)
      (goto-char (point-min))
      (train-time-route-refresh))
    (pop-to-buffer-same-window buffer)
    buffer))

(provide 'train-time-route)
;;; train-time-route.el ends here
