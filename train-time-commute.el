;;; train-time-commute.el --- Trips between home and work -*- lexical-binding: t; -*-

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

;; M-x train-time-home-to-work and M-x train-time-work-to-home list
;; the next trips between the stations in `train-time-home' and
;; `train-time-work' (set them with M-x train-time-set-home and
;; M-x train-time-set-work).
;;
;; Trips are built from the trains in MTA's real-time feeds: a train
;; that calls at both stations, or two trains with a change between
;; them at a station or station complex.  Departure and arrival times
;; are the feeds' predictions.  Trips needing two or more changes are
;; not found, and the list is not a promise of the fastest trip.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tabulated-list)
(require 'train-time)

(declare-function train-time-alerts-summary "train-time-alerts" (routes stations now))

;;;; Customization

(defcustom train-time-home nil
  "GTFS stop ID of the station you use at home, e.g. \"701\".
Set it with \\[train-time-set-home]."
  :type '(choice (const :tag "Not set" nil) (string :tag "Stop ID"))
  :group 'train-time)

(defcustom train-time-work nil
  "GTFS stop ID of the station you use at work, e.g. \"723\".
Set it with \\[train-time-set-work]."
  :type '(choice (const :tag "Not set" nil) (string :tag "Stop ID"))
  :group 'train-time)

(defcustom train-time-transfer-time 180
  "Seconds allowed to change trains within a station or complex."
  :type 'natnum
  :group 'train-time)

(defcustom train-time-commute-max-rows 12
  "Maximum number of trips listed between home and work."
  :type 'natnum
  :group 'train-time)

;;;; Trips

(cl-defstruct (train-time-leg (:copier nil))
  "A ride on one train from one station to another."
  (route-id nil :documentation "Route identifier, e.g. \"7X\".")
  (trip-id nil :documentation "Trip identifier from the feed.")
  (train-id nil :documentation "Operator train identifier, if any.")
  (assigned nil :documentation "Non-nil if a train is assigned to the trip.")
  (from nil :documentation "Station boarded at.")
  (stop-id nil :documentation "Platform boarded at, e.g. \"701S\".")
  (departure nil :documentation "Predicted POSIX departure time.")
  (to nil :documentation "Station left at.")
  (arrival nil :documentation "Predicted POSIX arrival time.")
  (destination nil :documentation "Name of the train's last stop."))

(defun train-time-commute--departure (call)
  "Return the departure time of CALL, else its arrival time."
  (or (nth 3 call) (nth 2 call)))

(defun train-time-commute--arrival (call)
  "Return the arrival time of CALL, else its departure time."
  (or (nth 2 call) (nth 3 call)))

(defun train-time-commute--complex (station-id)
  "Return the complex of STATION-ID, or STATION-ID for unknown stops."
  (let ((station (train-time-mta-station station-id)))
    (or (and station (train-time-station-complex station)) station-id)))

(defun train-time-commute--leg (trip from to)
  "Return the leg of TRIP from call FROM to call TO."
  (let ((last (car (last (plist-get trip :calls)))))
    (make-train-time-leg
     :route-id (plist-get trip :route-id)
     :trip-id (plist-get trip :trip-id)
     :train-id (plist-get trip :train-id)
     :assigned (plist-get trip :assigned)
     :from (train-time-mta-station (car from))
     :stop-id (nth 1 from)
     :departure (train-time-commute--departure from)
     :to (train-time-mta-station (car to))
     :arrival (train-time-commute--arrival to)
     :destination (let ((s (train-time-mta-station (car last))))
                    (if s (train-time-station-name s) (car last))))))

(defun train-time-commute--legs-departure (legs)
  "Return when the first of LEGS departs."
  (train-time-leg-departure (car legs)))

(defun train-time-commute--legs-arrival (legs)
  "Return when the last of LEGS arrives."
  (train-time-leg-arrival (car (last legs))))

(defun train-time-commute--best (journeys)
  "Return JOURNEYS no other journey beats, sorted by departure.
A journey is beaten by one that leaves no earlier, arrives no later
and changes trains no more often."
  (let ((sorted (sort (copy-sequence journeys)
                      (lambda (a b)
                        (let ((da (train-time-commute--legs-departure a))
                              (db (train-time-commute--legs-departure b)))
                          (if (/= da db) (> da db)
                            (let ((aa (train-time-commute--legs-arrival a))
                                  (ab (train-time-commute--legs-arrival b)))
                              (if (/= aa ab) (< aa ab)
                                (< (length a) (length b)))))))))
        (best-direct most-positive-fixnum)
        (best-any most-positive-fixnum)
        result)
    (dolist (legs sorted)
      (let ((arrival (train-time-commute--legs-arrival legs)))
        (when (< arrival (if (cdr legs) best-any best-direct))
          (push legs result))
        (setq best-any (min best-any arrival))
        (unless (cdr legs) (setq best-direct (min best-direct arrival)))))
    result))

(defun train-time-commute-journeys (trips origins destinations now &optional transfer-time)
  "Return journeys from ORIGINS to DESTINATIONS using TRIPS.
TRIPS are as returned by `train-time-mta-trips'.  ORIGINS and
DESTINATIONS are lists of station IDs, usually whole complexes.
A journey is a list of `train-time-leg's: one train that calls at
both, or two with a change of at least TRANSFER-TIME seconds
\(default `train-time-transfer-time') at a station or complex in
between.  Only journeys leaving after NOW, less 30 seconds, that no
other journey beats are returned, sorted by departure."
  (let ((transfer (or transfer-time train-time-transfer-time))
        (cutoff (- now 30))
        (second-legs (make-hash-table :test #'equal))
        journeys)
    ;; Index the trains that reach a destination by the complexes they
    ;; call at before it.
    (dolist (trip trips)
      (let* ((calls (plist-get trip :calls))
             (end (cl-find-if (lambda (c) (member (car c) destinations)) calls)))
        (when end
          (cl-loop for call in calls
                   until (eq call end)
                   unless (member (car call) origins)
                   do (push (list trip call end)
                            (gethash (train-time-commute--complex (car call))
                                     second-legs))))))
    (dolist (trip trips)
      (let* ((calls (plist-get trip :calls))
             (tail (cl-member-if (lambda (c)
                                   (and (member (car c) origins)
                                        (>= (train-time-commute--departure c) cutoff)))
                                 calls))
             (board (car tail)))
        (when board
          (cl-loop
           for call in (cdr tail)
           do (cond
               ((member (car call) destinations)
                (push (list (train-time-commute--leg trip board call)) journeys)
                (cl-return))
               ((not (member (car call) origins))
                (let ((ready (+ (train-time-commute--arrival call) transfer))
                      best)
                  (dolist (next (gethash (train-time-commute--complex (car call))
                                         second-legs))
                    (pcase-let ((`(,trip2 ,from ,to) next))
                      (when (and (not (eq trip2 trip))
                                 (>= (train-time-commute--departure from) ready)
                                 (or (null best)
                                     (< (train-time-commute--arrival to)
                                        (train-time-commute--arrival (nth 2 best)))))
                        (setq best next))))
                  (when best
                    (push (list (train-time-commute--leg trip board call)
                                (train-time-commute--leg (car best) (nth 1 best)
                                                         (nth 2 best)))
                          journeys)))))))))
    (train-time-commute--best journeys)))

;;;; Buffer state

(defvar-local train-time-commute--from nil "Place trips start from: `home' or `work'.")
(defvar-local train-time-commute--origin nil "Station trips start from.")
(defvar-local train-time-commute--destination nil "Station trips go to.")
(defvar-local train-time-commute--journeys nil "Journeys from the last fetch.")
(defvar-local train-time-commute--feed-results nil
  "Alist of (URL . PLIST) with each feed's :trips, :feed-time, :error.")
(defvar-local train-time-commute--pending 0 "Number of feed requests in flight.")
(defvar-local train-time-commute--request 0 "Counter used to ignore superseded responses.")
(defvar-local train-time-commute--timer nil "Auto-refresh timer.")

;;;; Rendering

(defun train-time-commute--route (leg)
  "Return LEG's route bullet."
  (let ((id (train-time-leg-route-id leg)))
    (train-time--bullet (train-time-mta-route-bullet id) id)))

(defun train-time-commute--via (legs)
  "Describe where LEGS change trains."
  (if (null (cdr legs))
      (propertize "direct" 'face 'shadow)
    (let ((off (train-time-station-name (train-time-leg-to (car legs))))
          (on (train-time-station-name (train-time-leg-from (cadr legs)))))
      (if (equal off on) on (format "%s → %s" off on)))))

(defun train-time-commute--clock (time)
  "Format TIME as a clock time in New York."
  (train-time-format-clock time train-time-mta-time-zone))

(defun train-time-commute--entry (legs now)
  "Return the `tabulated-list-entries' element for LEGS at time NOW."
  (let* ((departure (train-time-commute--legs-departure legs))
         (arrival (train-time-commute--legs-arrival legs))
         (assigned (cl-every #'train-time-leg-assigned legs))
         (dim (lambda (s) (if assigned s (propertize s 'face 'train-time-scheduled)))))
    (list legs
          (vconcat
           (list (funcall dim (train-time-commute--clock departure))
                 (funcall dim (train-time--minutes-label
                               (train-time-minutes-until departure now)))
                 (mapconcat #'train-time-commute--route legs "→")
                 (train-time-commute--via legs)
                 (funcall dim (train-time-commute--clock arrival)))
           (unless train-time--compact
             (list (funcall dim (format "%d min" (round (- arrival departure) 60)))
                   (funcall dim (if assigned "Tracked" "Scheduled"))))))))

(defconst train-time-commute--wide-format
  [("Leave" 6 nil)
   ("In" 8 nil :right-align t)
   ("Trains" 10 nil)
   ("Change at" 26 nil)
   ("Arrive" 7 nil)
   ("Travel" 7 nil :right-align t)
   ("Status" 0 nil)]
  "Trip list columns in the usual layout.")

(defun train-time-commute--set-format (width)
  "Set the trip list's columns for a window WIDTH columns wide."
  (let ((format (if train-time--compact
                    ;; Padding, Leave, In, Trains and Arrive take 31 columns.
                    (vector '("Leave" 5 nil)
                            '("In" 6 nil :right-align t)
                            '("Trains" 9 nil)
                            (list "Change at" (max 8 (- width 32)) nil)
                            '("Arrive" 6 nil))
                  train-time-commute--wide-format)))
    (unless (equal format tabulated-list-format)
      (setq tabulated-list-format format)
      (tabulated-list-init-header))))

(defun train-time-commute--feed-time ()
  "Return the timestamp of the oldest feed, or nil."
  (let ((times (delq nil (mapcar (lambda (r) (plist-get (cdr r) :feed-time))
                                 train-time-commute--feed-results))))
    (and times (apply #'min times))))

(defun train-time-commute--errors ()
  "Return the feed errors of the last refresh, or nil."
  (let ((errors (delq nil (mapcar (lambda (r)
                                    (let ((err (plist-get (cdr r) :error)))
                                      (and err (format "%s: %s"
                                                       (train-time--feed-name (car r))
                                                       err))))
                                  train-time-commute--feed-results))))
    (and errors (string-join errors "; "))))

(defun train-time-commute--place-name (station)
  "Return the name of STATION's complex, or of STATION if it has none."
  (if (cdr (train-time-mta-complex station))
      (train-time-mta-complex-name station)
    (train-time-station-display-name station)))

(defun train-time-commute-routes ()
  "Return the route IDs of this buffer's trips and of its two complexes."
  (delete-dups
   (append (mapcar #'train-time-leg-route-id (apply #'append train-time-commute--journeys))
           (apply #'append
                  (mapcar #'train-time-station-routes
                          (append (train-time-mta-complex train-time-commute--origin)
                                  (train-time-mta-complex train-time-commute--destination)))))))

(defun train-time-commute--status (now)
  "Return the text shown below the trips at time NOW."
  (let ((feed-time (train-time-commute--feed-time))
        (errors (train-time-commute--errors)))
    (concat
     (format "From %s (%s) to %s (%s)\n"
             (train-time-commute--place-name train-time-commute--origin)
             train-time-commute--from
             (train-time-commute--place-name train-time-commute--destination)
             (if (eq train-time-commute--from 'home) 'work 'home))
     (if (> train-time-commute--pending 0) "Updating…  " "")
     (cond
      (feed-time
       (let* ((age (- now feed-time))
              (text (format "Feed time %s (%ds old)"
                            (format-time-string "%H:%M:%S %Z" feed-time
                                                train-time-mta-time-zone)
                            (max 0 age))))
         (if (> age train-time-stale-threshold)
             (propertize (concat text " — STALE, predictions may be wrong")
                         'face 'train-time-stale)
           text)))
      ((zerop train-time-commute--pending) "No data yet"))
     (when errors
       (concat "\n" (propertize (format "Last update failed: %s " errors)
                                'face 'train-time-error)
               (train-time--button "(g to retry)" #'train-time-commute-refresh)))
     (let ((alerts (and train-time-show-alerts (featurep 'train-time-alerts)
                        (train-time-alerts-summary
                         (train-time-commute-routes)
                         (append (train-time-mta-complex train-time-commute--origin)
                                 (train-time-mta-complex train-time-commute--destination))
                         now))))
       (when alerts (concat "\n" alerts)))
     (when train-time-commute--timer
       (format "\nAuto refresh every %ds" train-time-auto-refresh-interval))
     (format "\nTrips use one train or change once (allowing %d min); \
times are predictions, not a promise of the fastest trip."
             (round train-time-transfer-time 60))
     "\n" train-time-mta-attribution)))

(defun train-time-commute--render (&optional now)
  "Redisplay the current trips buffer as of NOW."
  (let* ((now (or now (funcall train-time-now-function)))
         (rows (seq-take train-time-commute--journeys train-time-commute-max-rows))
         (width (train-time--window-width)))
    (setq train-time--width width
          train-time--compact (train-time--compact-p width))
    (train-time-commute--set-format width)
    (setq tabulated-list-entries
          (mapcar (lambda (legs) (train-time-commute--entry legs now)) rows))
    (tabulated-list-print t)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-max))
        (when (and (null rows) (zerop train-time-commute--pending)
                   train-time-commute--feed-results)
          (insert "  No trips found with at most one change in the real-time feeds.\n"))
        (insert "\n" (train-time--fill (train-time-commute--status now) (max 20 (1- width)))
                "\n")
        (goto-char (point-min))
        (insert (train-time--touch-bar
                 `(("Refresh" train-time-commute-refresh)
                   (,(if train-time-commute--timer "Auto: on" "Auto: off")
                    train-time-commute-toggle-auto-refresh)
                   ("Reverse" train-time-commute-reverse)
                   ("Board" train-time-commute-board)
                   ("Alerts" train-time-alerts)
                   ("Quit" quit-window))))))
    (when (bobp) (train-time--skip-touch-bar))
    (setq mode-line-process
          (concat (cond ((> train-time-commute--pending 0) " updating")
                        ((train-time-commute--errors) " error"))
                  (train-time--alerts-mode-line
                   (train-time-commute-routes)
                   (append (train-time-mta-complex train-time-commute--origin)
                           (train-time-mta-complex train-time-commute--destination))
                   now)))
    (force-mode-line-update)))

;;;; Fetching

(defun train-time-commute--feed-urls ()
  "Return the feeds with trains serving the origin or destination complex."
  (delete-dups
   (mapcan #'train-time-mta-feed-urls
           (append (train-time-mta-complex train-time-commute--origin)
                   (train-time-mta-complex train-time-commute--destination)))))

(defun train-time-commute--update ()
  "Recompute the journeys from every feed's trips."
  (setq train-time-commute--journeys
        (train-time-commute-journeys
         (mapcan (lambda (r) (copy-sequence (plist-get (cdr r) :trips)))
                 train-time-commute--feed-results)
         (mapcar #'train-time-station-id
                 (train-time-mta-complex train-time-commute--origin))
         (mapcar #'train-time-station-id
                 (train-time-mta-complex train-time-commute--destination))
         (funcall train-time-now-function))))

(defun train-time-commute--handle-result (buffer request url result)
  "Store RESULT of fetching URL for REQUEST in BUFFER and redisplay it.
A failed feed keeps its previous trips."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (= request train-time-commute--request)
        (cl-decf train-time-commute--pending)
        (let* ((entry (or (assoc url train-time-commute--feed-results)
                          (car (push (list url) train-time-commute--feed-results))))
               (err (plist-get result :error)))
          (unless err
            (condition-case decode-err
                (let ((feed (train-time-gtfs-rt-decode (plist-get result :data))))
                  (setcdr entry (list :trips (train-time-mta-trips feed)
                                      :feed-time (train-time-gtfs-rt-feed-time feed))))
              (error (setq err (format "Could not decode feed: %s"
                                       (error-message-string decode-err))))))
          (when err
            (setcdr entry (plist-put (copy-sequence (cdr entry)) :error err))))
        (when (zerop train-time-commute--pending)
          (train-time-commute--update))
        (train-time-commute--render)
        (let ((errors (train-time-commute--errors)))
          (when (and errors (zerop train-time-commute--pending))
            (message "train-time: %s" errors)))))))

(defun train-time-commute-refresh (&rest _)
  "Fetch fresh predictions for the current trips buffer."
  (interactive)
  (unless (derived-mode-p 'train-time-commute-mode)
    (user-error "Not in a train-time trips buffer"))
  (let ((buffer (current-buffer))
        (request (cl-incf train-time-commute--request))
        (urls (train-time-commute--feed-urls)))
    ;; Errors are per refresh; trips of failing feeds are kept.
    (dolist (entry train-time-commute--feed-results)
      (setcdr entry (plist-put (copy-sequence (cdr entry)) :error nil)))
    (setq train-time-commute--pending (length urls))
    (train-time-commute--render)
    (dolist (url urls)
      (funcall train-time-fetch-function url
               (lambda (result)
                 (train-time-commute--handle-result buffer request url result))))
    (when train-time-show-alerts
      (train-time-alerts-fetch
       (lambda ()
         (when (buffer-live-p buffer)
           (with-current-buffer buffer (train-time-commute--render))))))))

;;;; Commands in the trips buffer

(defun train-time-commute--auto-refresh-tick (buffer)
  "Refresh BUFFER if it is alive, visible and not already updating."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (get-buffer-window buffer t) (zerop train-time-commute--pending))
        (train-time-commute-refresh)))))

(defun train-time-commute--stop-timer ()
  "Cancel this buffer's auto-refresh timer."
  (when train-time-commute--timer
    (cancel-timer train-time-commute--timer)
    (setq train-time-commute--timer nil)))

(defun train-time-commute-toggle-auto-refresh (&optional arg)
  "Toggle periodic refresh of this buffer.
With positive ARG enable it, with zero or negative ARG disable it."
  (interactive "P")
  (let ((enable (if arg (> (prefix-numeric-value arg) 0)
                  (null train-time-commute--timer))))
    (train-time-commute--stop-timer)
    (when enable
      (setq train-time-commute--timer
            (run-at-time train-time-auto-refresh-interval
                         train-time-auto-refresh-interval
                         #'train-time-commute--auto-refresh-tick (current-buffer))))
    (train-time-commute--render)
    (message "Auto refresh %s" (if enable "enabled" "disabled"))))

(defun train-time-commute--describe-leg (leg)
  "Describe LEG in one line."
  (let ((direction (alist-get (train-time-mta--direction (train-time-leg-stop-id leg))
                              (train-time-station-directions (train-time-leg-from leg)))))
    (format "%s %s%s  %s %s → %s %s%s"
            (string-trim (train-time-commute--route leg))
            (if direction (format "toward %s " direction) "")
            (format "(to %s)" (train-time-leg-destination leg))
            (train-time-commute--clock (train-time-leg-departure leg))
            (train-time-station-name (train-time-leg-from leg))
            (train-time-commute--clock (train-time-leg-arrival leg))
            (train-time-station-name (train-time-leg-to leg))
            (if (train-time-leg-assigned leg) "" "  (scheduled)"))))

(defun train-time-commute-describe ()
  "Describe each train of the trip at point."
  (interactive)
  (let ((legs (or (tabulated-list-get-id) (user-error "No trip on this line"))))
    (message "%s" (mapconcat #'train-time-commute--describe-leg legs "\n"))))

(defun train-time-commute-mouse-select (event)
  "Describe the trip clicked in EVENT, which suits touch screens."
  (interactive "e")
  (mouse-set-point event)
  (when (tabulated-list-get-id)
    (train-time-commute-describe)))

(defun train-time-commute-board ()
  "Open the board of the first station of the trip at point.
With no trip at point, open the board of the origin."
  (interactive)
  (let ((legs (tabulated-list-get-id)))
    (train-time (if legs (train-time-leg-from (car legs)) train-time-commute--origin))))

(defun train-time-commute-reverse ()
  "Show trips in the other direction."
  (interactive)
  (if (eq train-time-commute--from 'home)
      (train-time-work-to-home)
    (train-time-home-to-work)))

;;;; Mode

(defconst train-time-commute--evil-bindings
  `("gr" train-time-commute-refresh
    "a" train-time-commute-toggle-auto-refresh
    "r" train-time-commute-reverse
    "b" train-time-commute-board
    "A" train-time-alerts
    ,(kbd "RET") train-time-commute-describe
    "q" quit-window)
  "Evil normal-state bindings for `train-time-commute-mode'.")

(defvar train-time-commute-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" #'train-time-commute-toggle-auto-refresh)
    (define-key map "r" #'train-time-commute-reverse)
    (define-key map "b" #'train-time-commute-board)
    (define-key map "A" #'train-time-alerts)
    (define-key map (kbd "RET") #'train-time-commute-describe)
    (define-key map [mouse-1] #'train-time-commute-mouse-select)
    map)
  "Keymap for `train-time-commute-mode'.")

(define-derived-mode train-time-commute-mode tabulated-list-mode "Train-Trips"
  "Major mode listing trips between home and work.

\\{train-time-commute-mode-map}"
  (setq tabulated-list-format train-time-commute--wide-format)
  (setq tabulated-list-padding 1
        tabulated-list-sort-key nil)
  (setq-local revert-buffer-function #'train-time-commute-refresh)
  (train-time--watch-width #'train-time-commute--render)
  (add-hook 'kill-buffer-hook #'train-time-commute--stop-timer nil t)
  (tabulated-list-init-header)
  (train-time-evil-setup 'train-time-commute-mode train-time-commute-mode-map
                         train-time-commute--evil-bindings))

;;;; Home and work

(defun train-time-commute--variable (place)
  "Return the variable holding PLACE, `home' or `work'."
  (if (eq place 'home) 'train-time-home 'train-time-work))

(defun train-time-commute--save (place station)
  "Make STATION the station for PLACE, saving it for future sessions."
  (let ((variable (train-time-commute--variable place))
        (id (train-time-station-id station)))
    (condition-case nil
        (progn (customize-save-variable variable id)
               (message "Your %s station is now %s" place
                        (train-time-station-display-name station)))
      (error
       (customize-set-variable variable id)
       (message "Your %s station is %s for this session; customizations cannot be saved"
                place (train-time-station-display-name station))))
    station))

(defun train-time-commute--read (place)
  "Read the station for PLACE with completion."
  (train-time--read-station (format "%s station" (capitalize (symbol-name place)))
                            (symbol-value (train-time-commute--variable place))))

(defun train-time-commute--station (place)
  "Return the station for PLACE, asking for one if it is not set."
  (let ((id (symbol-value (train-time-commute--variable place))))
    (or (and id (train-time-mta-station id))
        (progn
          (when id (message "Unknown stop ID %S in `%s'" id
                            (train-time-commute--variable place)))
          (train-time-commute--save place (train-time-commute--read place))))))

;;;###autoload
(defun train-time-set-home (station)
  "Make STATION your home station, saving it with Customize."
  (interactive (list (train-time-commute--read 'home)))
  (train-time-commute--save 'home station))

;;;###autoload
(defun train-time-set-work (station)
  "Make STATION your work station, saving it with Customize."
  (interactive (list (train-time-commute--read 'work)))
  (train-time-commute--save 'work station))

(defun train-time-commute--show (from)
  "Show trips from FROM, `home' or `work', to the other place."
  (let* ((origin (train-time-commute--station from))
         (destination (train-time-commute--station (if (eq from 'home) 'work 'home)))
         (buffer (get-buffer-create
                  (format "*train-time: %s → %s*" from
                          (if (eq from 'home) 'work 'home)))))
    (when (memq destination (train-time-mta-complex origin))
      (user-error "Home and work are in the same station complex"))
    (with-current-buffer buffer
      (unless (derived-mode-p 'train-time-commute-mode)
        (train-time-commute-mode)
        (when train-time-auto-refresh
          (train-time-commute-toggle-auto-refresh 1)))
      (unless (and (eq origin train-time-commute--origin)
                   (eq destination train-time-commute--destination))
        (setq train-time-commute--journeys nil
              train-time-commute--feed-results nil))
      (setq train-time-commute--from from
            train-time-commute--origin origin
            train-time-commute--destination destination)
      (train-time-commute-refresh))
    (pop-to-buffer-same-window buffer)))

;;;###autoload
(defun train-time-home-to-work ()
  "List the next trips from `train-time-home' to `train-time-work'."
  (interactive)
  (train-time-commute--show 'home))

;;;###autoload
(defun train-time-work-to-home ()
  "List the next trips from `train-time-work' to `train-time-home'."
  (interactive)
  (train-time-commute--show 'work))

(provide 'train-time-commute)
;;; train-time-commute.el ends here
