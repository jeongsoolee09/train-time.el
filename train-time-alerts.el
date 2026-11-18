;;; train-time-alerts.el --- Service alerts and elevator outages -*- lexical-binding: t; -*-

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

;; MTA publishes service alerts (delays, suspensions, reroutes and
;; planned work) as GTFS-Realtime, and elevator and escalator outages
;; as JSON.  Both are public.  This file downloads them, keeps them
;; for `train-time-alerts-refresh-interval' seconds, summarizes them
;; on boards and trip lists, and lists a line's alerts with
;; M-x train-time-alerts (A on a board, map or trip list).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'json)
(require 'train-time)

(defcustom train-time-alerts-refresh-interval 120
  "Seconds downloaded alerts and outages are reused before fetching again."
  :type 'natnum
  :group 'train-time)

(defface train-time-alert '((t :inherit warning))
  "Face for the kind of alerts in effect.")

(defface train-time-alert-planned '((t :inherit font-lock-type-face))
  "Face for the kind of planned alerts.")

(defface train-time-alert-heading '((t :inherit bold :height 1.1))
  "Face for section headings in alert lists.")

(defface train-time-alert-time '((t :inherit shadow))
  "Face for when alerts apply.")

;;;; Downloads

(defvar train-time-alerts--cache nil
  "Alist of (SOURCE . PLIST) for `alerts', `outages' and `equipment'.
PLIST has :value, :feed-time, :fetched-at and :error.")

(defvar train-time-alerts--waiting nil
  "Alist of (SOURCE . CALLBACKS) for requests in flight.")

(defun train-time-alerts--json (data)
  "Parse the UTF-8 JSON in unibyte DATA into lists and alists."
  (let ((text (decode-coding-string data 'utf-8)))
    (if (and (fboundp 'json-parse-string)
             (or (not (fboundp 'json-available-p)) (json-available-p)))
        (json-parse-string text :object-type 'alist :array-type 'list
                           :null-object nil :false-object nil)
      (let ((json-object-type 'alist)
            (json-array-type 'list)
            (json-key-type 'symbol))
        (json-read-from-string text)))))

(defun train-time-alerts--source (source)
  "Return (URL PARSER MAX-AGE) for SOURCE."
  (pcase source
    ('alerts
     (list (train-time-mta-alerts-url)
           (lambda (data)
             (let ((feed (train-time-gtfs-rt-decode data)))
               (list (train-time-mta-alerts feed) (train-time-gtfs-rt-feed-time feed))))
           train-time-alerts-refresh-interval))
    ('outages
     (list (train-time-mta-outages-url)
           (lambda (data) (list (train-time-alerts--json data) nil))
           train-time-alerts-refresh-interval))
    ;; The equipment list changes rarely and is large: once a day.
    ('equipment
     (list (train-time-mta-equipment-url)
           (lambda (data)
             (list (train-time-mta-equipment-complexes (train-time-alerts--json data))
                   nil))
           86400))))

(defun train-time-alerts--fresh-p (source now)
  "Return non-nil if SOURCE was downloaded recently enough at NOW."
  (let ((fetched (plist-get (cdr (assq source train-time-alerts--cache)) :fetched-at)))
    (and fetched (< (- now fetched) (nth 2 (train-time-alerts--source source))))))

(defun train-time-alerts--get (source callback &optional force)
  "Call CALLBACK with SOURCE's cache entry, downloading it if needed.
SOURCE is `alerts', `outages' or `equipment'.  Download if the copy
is older than its maximum age, or if FORCE is non-nil.  A failed
download keeps the previous value and records :error."
  (if (and (not force) (train-time-alerts--fresh-p source (funcall train-time-now-function)))
      (funcall callback (cdr (assq source train-time-alerts--cache)))
    (let ((entry (assq source train-time-alerts--waiting)))
      (if entry
          (push callback (cdr entry))
        (push (list source callback) train-time-alerts--waiting)
        (pcase-let ((`(,url ,parser ,_) (train-time-alerts--source source)))
          (funcall train-time-fetch-function url
                   (lambda (result)
                     (train-time-alerts--receive source parser result))))))))

(defun train-time-alerts--receive (source parser result)
  "Store RESULT of downloading SOURCE, parsed by PARSER, and notify."
  (let* ((old (cdr (assq source train-time-alerts--cache)))
         (err (plist-get result :error))
         new)
    (unless err
      (condition-case parse-err
          (pcase-let ((`(,value ,feed-time) (funcall parser (plist-get result :data))))
            (setq new (list :value value :feed-time feed-time
                            :fetched-at (funcall train-time-now-function))))
        (error (setq err (format "Could not read %s: %s" source
                                 (error-message-string parse-err))))))
    (unless new
      (setq new (plist-put (copy-sequence old) :error err)))
    (setf (alist-get source train-time-alerts--cache) new)
    (let ((callbacks (reverse (cdr (assq source train-time-alerts--waiting)))))
      (setq train-time-alerts--waiting (assq-delete-all source train-time-alerts--waiting))
      (dolist (callback callbacks)
        (condition-case callback-err
            (funcall callback new)
          (error (message "train-time: %s" (error-message-string callback-err))))))))

(defun train-time-alerts-fetch (callback &optional force)
  "Download alerts and outages if needed, then call CALLBACK once.
CALLBACK takes no arguments.  With FORCE, download even recent data."
  (let ((pending 3))
    (dolist (source '(alerts outages equipment))
      (train-time-alerts--get source
                              (lambda (_)
                                (when (zerop (cl-decf pending))
                                  (funcall callback)))
                              (and force (not (eq source 'equipment)))))))

(defun train-time-alerts--value (source)
  "Return the last downloaded value of SOURCE, or nil."
  (plist-get (cdr (assq source train-time-alerts--cache)) :value))

(defun train-time-alerts--error (source)
  "Return the error of the last download of SOURCE, or nil."
  (plist-get (cdr (assq source train-time-alerts--cache)) :error))

;;;; Selecting

(defun train-time-alerts-select (routes now &optional stations)
  "Return (CURRENT . PLANNED) alerts for ROUTES at time NOW.
ROUTES are route IDs; nil means every route.  CURRENT are in effect
now, those naming STATIONS (station IDs) first, then the most severe.
PLANNED take effect later, soonest first.  When STATIONS are given,
notices that only matter at other stations are left out."
  (let (current planned)
    (dolist (alert (train-time-alerts--value 'alerts))
      (when (and (or (null routes) (train-time-mta-alert-affects-p alert routes))
                 (not (and stations
                           (train-time-mta-alert-local-p alert)
                           (not (cl-intersection (train-time-alert-stations alert)
                                                 stations :test #'equal)))))
        (cond ((train-time-alert-active-p alert now) (push alert current))
              ((train-time-alert-next-start alert now) (push alert planned)))))
    (let ((here (lambda (a) (and (cl-intersection (train-time-alert-stations a) stations
                                                   :test #'equal)
                                 t))))
      (cons (sort current
                  (lambda (a b)
                    (let ((ha (funcall here a)) (hb (funcall here b))
                          (pa (train-time-alert-priority a))
                          (pb (train-time-alert-priority b)))
                      (cond ((not (eq ha hb)) ha)
                            ((/= pa pb) (> pa pb))
                            (t (> (or (train-time-alert-updated a) 0)
                                  (or (train-time-alert-updated b) 0)))))))
            (sort planned
                  (lambda (a b) (< (train-time-alert-next-start a now)
                                   (train-time-alert-next-start b now))))))))

(defun train-time-alerts-outages (complexes)
  "Return current elevator and escalator outages at station COMPLEXES.
COMPLEXES are station complex IDs.  Elevators come first."
  (let ((table (train-time-alerts--value 'equipment)))
    (when table
      (sort (cl-remove-if-not
             (lambda (o) (and (not (plist-get o :upcoming))
                              (member (plist-get o :complex) complexes)))
             (train-time-mta-outages (train-time-alerts--value 'outages) table))
            (lambda (a b) (and (eq (plist-get a :kind) 'elevator)
                               (not (eq (plist-get b :kind) 'elevator))))))))

;;;; Formatting

(defun train-time-alerts--route-id (name)
  "Return the route ID MTA's alert text means by bracketed NAME, or nil."
  (or (and (train-time-mta-route-info name) name)
      (car (cl-find name train-time-mta-data-routes
                    :key (lambda (r) (nth 1 r)) :test #'equal))))

(defconst train-time-alerts--icons
  '(("accessibility icon" . "♿")
    ("shuttle bus icon" . "🚌")
    ("airplane icon" . "✈"))
  "Symbols for the bracketed icon names in MTA's alert text.")

(defun train-time-alerts--bulletize (text)
  "Return TEXT with route names like \"[7]\" shown as colored bullets.
Icon names like \"[accessibility icon]\" become symbols."
  (replace-regexp-in-string
   "\\[\\([^]\n]+\\)\\]"
   (lambda (match)
     (let* ((name (match-string 1 match))
            (id (train-time-alerts--route-id name)))
       (cond (id (train-time--bullet (train-time-mta-route-bullet id) id))
             ((cdr (assoc name train-time-alerts--icons)))
             (t match))))
   text t t))

(defun train-time-alerts--fill (text indent width)
  "Return TEXT filled to WIDTH, every line indented by INDENT spaces.
Each line of TEXT is filled on its own."
  (train-time--fill text width indent))

(defun train-time-alerts--width ()
  "Return the width to fill alert text in the current buffer."
  (max 20 (- (train-time--window-width) 2)))

(define-button-type 'train-time-alerts-link 'face nil)

(defun train-time-alerts--link (text routes &optional alert)
  "Return TEXT as a link to the alerts of ROUTES, focused on ALERT.
TEXT keeps its own faces; the link only highlights under the mouse."
  (train-time--button text
                      (lambda ()
                        (interactive)
                        (train-time-alerts routes (and alert (train-time-alert-id alert))))
                      :type 'train-time-alerts-link
                      'help-echo "mouse-1, RET: show in the alerts buffer"))

(defun train-time-alerts--routes-bullets (alert)
  "Return the bullets of ALERT's routes, express variants left out."
  (mapconcat (lambda (id) (train-time--bullet (car (train-time-mta--route id)) id))
             (seq-uniq (mapcar #'train-time-mta--family (train-time-alert-routes alert)))
             ""))

(defun train-time-alerts--kind (alert)
  "Return ALERT's kind, in its face."
  (propertize (or (train-time-alert-type alert) "Alert")
              'face (if (train-time-alert-planned alert)
                        'train-time-alert-planned
                      'train-time-alert)))

(defun train-time-alerts--when (alert now)
  "Return the period of ALERT, as text, as of NOW."
  (or (train-time-alert-period-text alert)
      (let* ((periods (train-time-alert-periods alert))
             (period (or (cl-find-if (lambda (p) (and (<= (or (car p) 0) now)
                                                      (or (null (cdr p)) (< now (cdr p)))))
                                     periods)
                         (cl-find (train-time-alert-next-start alert now) periods
                                  :key #'car)))
             (fmt (lambda (time) (format-time-string "%b %-d %H:%M" time
                                                     train-time-mta-time-zone))))
        (cond ((null period) nil)
              ((and (car period) (> (car period) now))
               (if (cdr period)
                   (format "%s to %s" (funcall fmt (car period)) (funcall fmt (cdr period)))
                 (format "From %s" (funcall fmt (car period)))))
              ((cdr period) (format "Until %s" (funcall fmt (cdr period))))))))

(defun train-time-alerts--outage-text (outage)
  "Describe elevator or escalator OUTAGE in one sentence."
  (let ((until (plist-get outage :until)))
    (format "%s %s out of service%s%s%s"
            (capitalize (symbol-name (plist-get outage :kind)))
            (plist-get outage :equipment)
            (if (plist-get outage :serving)
                (format " (%s)" (string-remove-suffix "." (plist-get outage :serving)))
              "")
            (if until
                (format-time-string ", expected back %b %-d %H:%M" until
                                    train-time-mta-time-zone)
              "")
            (if (plist-get outage :reason)
                (format ": %s" (downcase (plist-get outage :reason)))
              ""))))

(defun train-time-alerts-summary (routes stations now)
  "Return a footer describing alerts for ROUTES and outages at STATIONS.
ROUTES are route IDs and STATIONS `train-time-station's; NOW is the
current time.  Return nil before anything has been downloaded."
  (when (assq 'alerts train-time-alerts--cache)
    (pcase-let* ((width (train-time-alerts--width))
                 (`(,current . ,planned)
                  (train-time-alerts-select routes now
                                            (mapcar #'train-time-station-id stations)))
                 (outages (train-time-alerts-outages
                           (delete-dups (mapcar #'train-time-station-complex stations))))
                 (errors (delq nil (mapcar #'train-time-alerts--error
                                           '(alerts outages equipment)))))
      (concat
       (if current
           (concat
            (propertize (format "Service alerts in effect (%d):" (length current))
                        'face 'train-time-alert)
            (mapconcat (lambda (alert)
                         (concat "\n" (train-time-alerts--link
                                       (train-time-alerts--fill
                                        (concat (train-time-alerts--routes-bullets alert)
                                                " "
                                                (train-time-alerts--kind alert)
                                                ": "
                                                (train-time-alerts--bulletize
                                                 (or (train-time-alert-header alert) "")))
                                        2 width)
                                       routes alert)))
                       current ""))
         "No service alerts in effect for these lines.")
       (when planned
         (concat "\n" (train-time-alerts--link
                       (format "%d planned service change%s coming up."
                               (length planned) (if (cdr planned) "s" ""))
                       routes)))
       (mapconcat (lambda (outage)
                    (concat "\n" (train-time-alerts--fill
                                  (propertize (train-time-alerts--outage-text outage)
                                              'face (if (plist-get outage :ada)
                                                        'train-time-alert
                                                      'default))
                                  2 width)))
                  outages "")
       (when errors
         (concat "\n" (propertize (format "Alerts unavailable: %s"
                                          (string-join errors "; "))
                                  'face 'train-time-error)))
       "\n"
       (train-time-alerts--link
        (if (train-time--touch-bar-p)
            "Tap here, or on an alert, for the lines' alerts."
          (substitute-command-keys
           "\\<train-time-mode-map>\\[train-time-alerts] lists the lines' alerts."))
        routes)))))

(defun train-time-alerts-count (routes stations now)
  "Return how many alerts for ROUTES and STATIONS are in effect at NOW.
STATIONS are `train-time-station's, as for `train-time-alerts-summary'."
  (length (car (train-time-alerts-select routes now
                                         (mapcar #'train-time-station-id stations)))))

;;;; Alerts buffer

(defvar-local train-time-alerts--routes nil
  "Route IDs whose alerts this buffer lists; nil for every route.")
(defvar-local train-time-alerts--expanded nil
  "IDs of alerts whose details are shown.")
(defvar-local train-time-alerts--loading nil
  "Non-nil while a download is running.")
(defvar-local train-time-alerts--focus nil
  "ID of the alert to move to once it is shown.")

(defun train-time-alerts--insert-alert (alert now width)
  "Insert ALERT as of NOW, filled to WIDTH."
  (let ((start (point))
        (open (member (train-time-alert-id alert) train-time-alerts--expanded))
        (description (train-time-alert-description alert))
        (when-text (train-time-alerts--when alert now)))
    (insert (if description (if open "▾ " "▸ ") "  ")
            (train-time-alerts--routes-bullets alert) " "
            (train-time-alerts--kind alert))
    (when when-text
      ;; In narrow windows the period goes on a line of its own.
      (insert (if (> (+ (current-column) 2 (string-width when-text)) width)
                  "\n    "
                "  ")
              (propertize when-text 'face 'train-time-alert-time)))
    (insert "\n"
            (train-time-alerts--fill
             (train-time-alerts--bulletize (or (train-time-alert-header alert) ""))
             4 width)
            "\n")
    (when (and open description)
      (insert "\n" (train-time-alerts--fill
                    (train-time-alerts--bulletize description) 4 width)
              "\n"))
    (insert "\n")
    (put-text-property start (point) 'train-time-alert alert)))

(defun train-time-alerts--title ()
  "Return the heading naming this buffer's lines."
  (if train-time-alerts--routes
      (concat "Service alerts for "
              (mapconcat (lambda (id) (train-time--bullet (car (train-time-mta--route id)) id))
                         (seq-uniq (mapcar #'train-time-mta--family train-time-alerts--routes))
                         ""))
    "Service alerts for every line"))

(defun train-time-alerts--start-at (pos)
  "Return where the alert at POS starts."
  (car (last (cl-remove-if-not (lambda (s) (<= s pos)) (train-time-alerts--starts)))))

(defun train-time-alerts--row (window pos)
  "Return how many lines below WINDOW's start POS is; negative if above."
  (let ((start (window-start window)))
    (if (<= start pos)
        (count-lines start pos)
      (- (count-lines pos start)))))

(defun train-time-alerts--render ()
  "Redisplay the alerts buffer, keeping point on the same alert.
The alert also stays at the same height in the window showing it."
  (let* ((inhibit-read-only t)
         (now (funcall train-time-now-function))
         (width (train-time-alerts--width))
         (window-width (train-time--window-width))
         (window (get-buffer-window))
         (pos (if window (window-point window) (point)))
         (at (get-text-property pos 'train-time-alert))
         (at-start (and at (train-time-alerts--start-at pos)))
         (line (and at (count-lines at-start (save-excursion
                                               (goto-char pos)
                                               (line-beginning-position)))))
         (row (and at window (train-time-alerts--row window at-start)))
         (selection (train-time-alerts-select train-time-alerts--routes now))
         (feed-time (plist-get (cdr (assq 'alerts train-time-alerts--cache)) :feed-time))
         (err (train-time-alerts--error 'alerts)))
    (setq train-time--width window-width)
    (erase-buffer)
    (insert (train-time--touch-bar
             '(("Refresh" train-time-alerts-refresh)
               ("Details" train-time-alerts-toggle-all)
               ("Lines" train-time-alerts-choose-lines)
               ("Quit" quit-window)))
            "\n " (propertize (train-time-alerts--title) 'face 'train-time-alert-heading)
            "\n\n")
    (pcase-dolist (`(,title . ,alerts) `(("In effect now" . ,(car selection))
                                         ("Planned" . ,(cdr selection))))
      (insert " " (propertize (format "%s (%d)" title (length alerts))
                              'face 'train-time-group)
              "\n\n")
      (if alerts
          (dolist (alert alerts)
            (train-time-alerts--insert-alert alert now width))
        (insert "  "
                (cond (train-time-alerts--loading "Loading…")
                      ((assq 'alerts train-time-alerts--cache) "None.")
                      (t "No data yet."))
                "\n\n")))
    (insert (cond
             (feed-time
              (let ((age (- now feed-time)))
                (format "Feed time %s (%ds old)"
                        (format-time-string "%H:%M:%S %Z" feed-time train-time-mta-time-zone)
                        (max 0 age))))
             (train-time-alerts--loading "Updating…")
             (t ""))
            (if err
                (concat "\n" (propertize (format "Last update failed: %s " err)
                                         'face 'train-time-error)
                        (train-time--button "(g to retry)" #'train-time-alerts-refresh))
              "")
            "\n"
            (train-time--fill
             (propertize
              (cond ((train-time--touch-bar-p)
                     "Tap an alert to show or hide its details; swipe to scroll.")
                    ((bound-and-true-p evil-local-mode)
                     "RET details   S-TAB all details   C-j/C-k next/previous   gr refresh   q quit")
                    (t
                     "RET details   S-TAB all details   n/p next/previous   g refresh   q quit"))
              'face 'shadow)
             (max 20 (1- window-width)))
            "\n" (train-time--fill train-time-mta-attribution (max 20 (1- window-width)))
            "\n")
    (goto-char (point-min))
    (let* ((find (lambda (id)
                   (and id (cl-find-if
                            (lambda (pos)
                              (equal (train-time-alert-id
                                      (get-text-property pos 'train-time-alert))
                                     id))
                            (train-time-alerts--starts)))))
           (focus (funcall find train-time-alerts--focus))
           (found (and (not focus) at (funcall find (train-time-alert-id at)))))
      (cond (focus (goto-char focus)
                   (setq train-time-alerts--focus nil)
                   (dolist (window (get-buffer-window-list nil nil t))
                     (set-window-point window focus)
                     (set-window-start window (line-beginning-position 0))))
            (found (goto-char found)
                   (forward-line line)
                   ;; Point was in details that are now hidden.
                   (unless (equal (train-time-alert-id
                                   (get-text-property (point) 'train-time-alert))
                                  (train-time-alert-id at))
                     (goto-char found))
                   (when (window-live-p window)
                     (set-window-point window (point))
                     (set-window-start window (min (line-beginning-position)
                                                   (save-excursion
                                                     (goto-char found)
                                                     (forward-line (- row))
                                                     (point))))))
            (t (train-time-alerts-next 1 t))))))

(defun train-time-alerts--refresh (&optional force)
  "Download alerts (all of them with FORCE) and redisplay."
  (let ((buffer (current-buffer)))
    (setq train-time-alerts--loading t)
    (train-time-alerts--render)
    (train-time-alerts--get 'alerts
                            (lambda (_)
                              (when (buffer-live-p buffer)
                                (with-current-buffer buffer
                                  (setq train-time-alerts--loading nil)
                                  (train-time-alerts--render)
                                  (let ((err (train-time-alerts--error 'alerts)))
                                    (when err (message "train-time: %s" err))))))
                            force)))

(defun train-time-alerts-refresh (&rest _)
  "Download the latest alerts and redisplay them."
  (interactive)
  (unless (derived-mode-p 'train-time-alerts-mode)
    (user-error "Not in a train-time alerts buffer"))
  (train-time-alerts--refresh t))

(defun train-time-alerts--alert-at-point ()
  "Return the alert at point, or signal an error."
  (or (get-text-property (point) 'train-time-alert)
      (user-error "No alert here")))

(defun train-time-alerts-toggle ()
  "Show or hide the details of the alert at point."
  (interactive)
  (let ((id (train-time-alert-id (train-time-alerts--alert-at-point))))
    (setq train-time-alerts--expanded
          (if (member id train-time-alerts--expanded)
              (delete id train-time-alerts--expanded)
            (cons id train-time-alerts--expanded)))
    (train-time-alerts--render)))

(defun train-time-alerts-toggle-all ()
  "Show every alert's details, or hide them all if they are shown."
  (interactive)
  (let* ((now (funcall train-time-now-function))
         (selection (train-time-alerts-select train-time-alerts--routes now))
         (ids (mapcar #'train-time-alert-id
                      (cl-remove-if-not #'train-time-alert-description
                                        (append (car selection) (cdr selection))))))
    (setq train-time-alerts--expanded
          (unless (cl-every (lambda (id) (member id train-time-alerts--expanded)) ids)
            ids))
    (train-time-alerts--render)))

(defun train-time-alerts-mouse-select (event)
  "Show or hide the details of the alert clicked in EVENT.
This suits touch screens, where a tap is a click."
  (interactive "e")
  (mouse-set-point event)
  (when (get-text-property (point) 'train-time-alert)
    (train-time-alerts-toggle)))

(defun train-time-alerts-choose-lines ()
  "List the alerts of lines read with completion; none means every line."
  (interactive)
  (train-time-alerts (train-time-alerts--read-routes)))

(defun train-time-alerts--starts ()
  "Return the positions where alerts start, in order."
  (let ((pos (point-min))
        starts)
    (when (get-text-property pos 'train-time-alert) (push pos starts))
    (while (setq pos (next-single-property-change pos 'train-time-alert))
      (when (get-text-property pos 'train-time-alert) (push pos starts)))
    (nreverse starts)))

(defun train-time-alerts-next (&optional n quiet)
  "Move to the start of the Nth next alert, or previous if N is negative.
With QUIET, do not complain when there is none."
  (interactive "p")
  (let* ((n (or n 1))
         (starts (train-time-alerts--starts))
         (target (if (> n 0)
                     (nth (1- n) (cl-remove-if-not (lambda (s) (> s (point))) starts))
                   (let* ((here (and (get-text-property (point) 'train-time-alert)
                                     (car (last (cl-remove-if-not
                                                 (lambda (s) (<= s (point))) starts)))))
                          (limit (or here (point))))
                     (nth (1- (- n)) (reverse (cl-remove-if-not
                                               (lambda (s) (< s limit)) starts)))))))
    (cond (target (goto-char target))
          ((not quiet) (user-error "No %s alert" (if (> n 0) "next" "previous"))))))

(defun train-time-alerts-previous (&optional n)
  "Move to the start of the Nth previous alert."
  (interactive "p")
  (train-time-alerts-next (- (or n 1))))

(defconst train-time-alerts--evil-bindings
  `("gr" train-time-alerts-refresh
    ,(kbd "TAB") train-time-alerts-toggle
    ,(kbd "RET") train-time-alerts-toggle
    ,(kbd "<return>") train-time-alerts-toggle
    "za" train-time-alerts-toggle
    ,(kbd "<backtab>") train-time-alerts-toggle-all
    "zM" train-time-alerts-toggle-all
    "gj" train-time-alerts-next
    "gk" train-time-alerts-previous
    ,(kbd "C-j") train-time-alerts-next
    ,(kbd "C-k") train-time-alerts-previous
    "gl" train-time-alerts-choose-lines
    "q" quit-window)
  "Evil normal-state bindings for `train-time-alerts-mode'.")

(defvar train-time-alerts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'train-time-alerts-toggle)
    (define-key map (kbd "RET") #'train-time-alerts-toggle)
    (define-key map (kbd "<backtab>") #'train-time-alerts-toggle-all)
    (define-key map "n" #'train-time-alerts-next)
    (define-key map "p" #'train-time-alerts-previous)
    (define-key map (kbd "C-j") #'train-time-alerts-next)
    (define-key map (kbd "C-k") #'train-time-alerts-previous)
    (define-key map "l" #'train-time-alerts-choose-lines)
    (define-key map [mouse-1] #'train-time-alerts-mouse-select)
    map)
  "Keymap for `train-time-alerts-mode'.")

(define-derived-mode train-time-alerts-mode special-mode "Train-Alerts"
  "Major mode listing service alerts.

\\{train-time-alerts-mode-map}"
  (setq truncate-lines nil)
  (setq-local revert-buffer-function #'train-time-alerts-refresh)
  (train-time--watch-width #'train-time-alerts--render)
  (train-time-evil-setup 'train-time-alerts-mode train-time-alerts-mode-map
                         train-time-alerts--evil-bindings))

(defun train-time-alerts--context-routes ()
  "Return the route IDs of the train-time buffer at hand, or `none'."
  (cond
   ((derived-mode-p 'train-time-mode)
    (delete-dups (apply #'append (mapcar #'train-time-station-routes train-time--stations))))
   ((and (derived-mode-p 'train-time-map-mode) (boundp 'train-time-map--network))
    (let ((line (plist-get (symbol-value 'train-time-map--network) :line)))
      (if line (list line) 'none)))
   ((and (derived-mode-p 'train-time-commute-mode) (fboundp 'train-time-commute-routes))
    (funcall 'train-time-commute-routes))
   (t 'none)))

(defun train-time-alerts--read-routes ()
  "Read lines with completion; return their route IDs, nil for all."
  (let* ((table (mapcar (lambda (r) (cons (nth 1 r) (car r)))
                        (cl-remove-if (lambda (r) (string-suffix-p "X" (car r)))
                                      train-time-mta-data-routes)))
         (names (let ((completion-ignore-case t))
                  (completing-read-multiple "Lines (empty for all): " table nil t))))
    (delete-dups (cl-loop for name in names
                          append (cl-loop for (n . id) in table
                                          when (equal (downcase n) (downcase name))
                                          collect id)))))

;;;###autoload
(defun train-time-alerts (&optional routes alert-id)
  "List service alerts for ROUTES, a list of route IDs.
Interactively, list them for the lines of the board, map or trip
list at hand; elsewhere, or with a prefix argument, read the lines,
where none means every line.  With ALERT-ID, show that alert's
details and move to it."
  (interactive
   (list (let ((context (train-time-alerts--context-routes)))
           (if (or current-prefix-arg (eq context 'none))
               (train-time-alerts--read-routes)
             context))))
  (let* ((names (sort (seq-uniq (mapcar (lambda (id) (car (train-time-mta--route id)))
                                        routes))
                      #'string<))
         (buffer (get-buffer-create
                  (if names
                      (format "*train-time alerts: %s*" (string-join names " "))
                    "*train-time alerts*"))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'train-time-alerts-mode)
        (train-time-alerts-mode))
      (setq train-time-alerts--routes routes)
      (when alert-id
        (cl-pushnew alert-id train-time-alerts--expanded :test #'equal)
        (setq train-time-alerts--focus alert-id))
      (train-time-alerts--refresh))
    (train-time--display buffer)))

(provide 'train-time-alerts)
;;; train-time-alerts.el ends here
