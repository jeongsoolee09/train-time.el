;;; train-time.el --- Real-time train arrivals and departures -*- lexical-binding: t; -*-

;; Copyright (C) 2026  train-time.el contributors

;; Author: train-time.el contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, comm
;; URL: https://github.com/train-time-el/train-time

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

;; All aboard, without leaving Emacs.
;;
;; M-x train-time shows upcoming trains at a station using official
;; real-time data.  It supports every New York City Subway line and
;; the Staten Island Railway through MTA's public GTFS-Realtime feeds.
;;
;; In the arrivals buffer:
;;   g  refresh             a  toggle auto refresh
;;   d  filter direction    s  filter service (local/express)
;;   l  filter line         c  clear filters
;;   t  toggle terminating  S  select station
;;   RET  show trip details   m  show on OpenStreetMap (osm.el)
;;
;; M-x train-time-map navigates a text diagram of each line, like
;; `undo-tree-visualize'; RET there opens a station's board.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tabulated-list)
(require 'url)
(require 'url-http)
(require 'train-time-core)
(require 'train-time-mta)

(defvar url-http-end-of-headers)
(defvar url-http-response-status)

;;;; Customization

(defcustom train-time-default-station "701"
  "Stop ID of the station offered by default.  \"701\" is Flushing-Main St."
  :type 'string
  :group 'train-time)

(defcustom train-time-auto-refresh nil
  "Non-nil means new arrivals buffers refresh periodically."
  :type 'boolean
  :group 'train-time)

(defcustom train-time-auto-refresh-interval 30
  "Seconds between automatic refreshes.
MTA feeds update roughly every 30 seconds; shorter intervals only add load."
  :type 'natnum
  :group 'train-time)

(defcustom train-time-stale-threshold 120
  "Feed age in seconds after which predictions are flagged as stale."
  :type 'natnum
  :group 'train-time)

(defcustom train-time-request-timeout 20
  "Seconds to wait for a feed before giving up."
  :type 'natnum
  :group 'train-time)

(defcustom train-time-show-terminating nil
  "Non-nil means list trains that end their trip at the station.
Such trains cannot be boarded, so they are hidden by default."
  :type 'boolean
  :group 'train-time)

(defcustom train-time-max-rows 30
  "Maximum number of trains to list, or nil for no limit."
  :type '(choice (const :tag "No limit" nil) natnum)
  :group 'train-time)

(defvar train-time-fetch-function #'train-time-http-fetch
  "Function used to fetch feeds, called as (FUNCTION URL CALLBACK).
CALLBACK receives a plist with either :data (unibyte string) or
:error (message string).  Tests replace this to avoid the network.")

(defvar train-time-now-function #'float-time
  "Function returning the current POSIX time.  Tests replace this.")

(defface train-time-stale '((t :inherit warning))
  "Face for stale data warnings.")

(defface train-time-error '((t :inherit error))
  "Face for fetch errors.")

(defface train-time-scheduled '((t :inherit shadow))
  "Face for scheduled (not yet tracked) trains.")

;;;; HTTP

(defun train-time-http-fetch (url callback)
  "Fetch URL asynchronously and call CALLBACK with a result plist."
  (let* ((done nil)
         (finish (lambda (result)
                   (unless done
                     (setq done t)
                     (funcall callback result))))
         (buffer
          (condition-case err
              (url-retrieve
               url
               (lambda (status)
                 (let ((buf (current-buffer)))
                   (unwind-protect
                       (funcall
                        finish
                        (condition-case err
                            (let ((http-err (plist-get status :error))
                                  (code (bound-and-true-p url-http-response-status)))
                              (cond
                               (http-err
                                (list :error (format "Network error: %s"
                                                     (error-message-string http-err))))
                               ((not (eql code 200))
                                (list :error (format "HTTP status %s" code)))
                               (t
                                (set-buffer-multibyte nil)
                                (list :data (buffer-substring-no-properties
                                             (1+ url-http-end-of-headers)
                                             (point-max))))))
                          (error (list :error (error-message-string err)))))
                     (kill-buffer buf))))
               nil t t)
            (error (funcall finish (list :error (error-message-string err)))
                   nil))))
    (when (and buffer train-time-request-timeout)
      (run-at-time train-time-request-timeout nil
                   (lambda ()
                     (unless done
                       (funcall finish (list :error "Request timed out"))
                       (when (buffer-live-p buffer)
                         (let ((proc (get-buffer-process buffer)))
                           (when proc (delete-process proc)))
                         (kill-buffer buffer))))))))

;;;; Buffer state

(defvar-local train-time--station nil "Station shown in this buffer.")
(defvar-local train-time--arrivals nil "All arrivals from the last fetch.")
(defvar-local train-time--feed-time nil
  "Header timestamp of the oldest feed shown.")
(defvar-local train-time--feed-results nil
  "Alist of (URL . PLIST) with each feed's :arrivals, :feed-time, :error.")
(defvar-local train-time--pending 0 "Number of feed requests in flight.")
(defvar-local train-time--fetched-at nil "When the last feed was received.")
(defvar-local train-time--error nil "Error message from the last fetch.")
(defvar-local train-time--loading nil "Non-nil while a request is running.")
(defvar-local train-time--request 0 "Counter used to ignore superseded responses.")
(defvar-local train-time--timer nil "Auto-refresh timer.")
(defvar-local train-time--show-terminating nil
  "Buffer-local copy of `train-time-show-terminating'.")
(defvar-local train-time--filters nil
  "Plist of active filters: :direction, :service, :route.")

;;;; Rendering

(defun train-time--route-label (arrival)
  "Return the rider-facing route bullet for ARRIVAL, in the route's colors."
  (let* ((route (train-time-arrival-route arrival))
         (label (if (eq (train-time-arrival-service arrival) 'express)
                    (format "<%s>" route)
                  route))
         (id (train-time-arrival-route-id arrival))
         (color (and id (train-time-mta-route-color id))))
    (if color
        (propertize (format " %s " label)
                    'face `(:background ,color
                            :foreground ,(or (train-time-mta-route-text-color id) "white")
                            :weight bold))
      (format " %s " label))))

(defun train-time--service-label (arrival)
  "Return the service pattern of ARRIVAL, or \"\" for routes without one."
  (pcase (train-time-arrival-service arrival)
    ('express "Express") ('local "Local") (_ "")))

(defun train-time--direction-label (arrival)
  "Return a direction description for ARRIVAL."
  (or (train-time-arrival-direction-label arrival)
      (pcase (train-time-arrival-direction arrival)
        ('north "Northbound") ('south "Southbound") (_ ""))))

(defun train-time--minutes-label (minutes)
  "Format MINUTES until a train."
  (cond ((null minutes) "?")
        ((<= minutes 0) "now")
        (t (format "%d min" minutes))))

(defun train-time--entry (arrival now)
  "Return a `tabulated-list-entries' element for ARRIVAL at time NOW."
  (let* ((assigned (train-time-arrival-assigned arrival))
         (minutes (train-time-minutes-until (train-time-arrival-time arrival) now))
         (dim (lambda (s) (if assigned s (propertize s 'face 'train-time-scheduled)))))
    (list (cons (train-time-arrival-trip-id arrival)
                (train-time-arrival-stop-id arrival))
          (vector
           (train-time--route-label arrival)
           (funcall dim (train-time--service-label arrival))
           (funcall dim (train-time--direction-label arrival))
           (funcall dim (if (train-time-arrival-terminating arrival)
                            "Terminates here"
                          (or (train-time-arrival-destination arrival) "?")))
           (funcall dim (if (eq (train-time-arrival-event arrival) 'departure)
                            "Departs" "Arrives"))
           (funcall dim (train-time-format-clock (train-time-arrival-time arrival)
                                                 train-time-mta-time-zone))
           (funcall dim (train-time--minutes-label minutes))
           (funcall dim (if assigned "Tracked" "Scheduled"))
           (funcall dim (or (train-time-arrival-train-id arrival) ""))))))

(defun train-time--sort-by-time (a b)
  "Sort predicate ordering tabulated entries A and B by departure time."
  (< (or (get-text-property 0 'train-time-time (aref (cadr a) 6)) 0)
     (or (get-text-property 0 'train-time-time (aref (cadr b) 6)) 0)))

(defun train-time--filtered (arrivals)
  "Return ARRIVALS that pass the buffer's filters."
  (let ((dir (plist-get train-time--filters :direction))
        (svc (plist-get train-time--filters :service))
        (route (plist-get train-time--filters :route)))
    (cl-remove-if-not
     (lambda (a)
       (and (or train-time--show-terminating
                (not (train-time-arrival-terminating a)))
            (or (null dir) (eq dir (train-time-arrival-direction a)))
            (or (null svc) (eq svc (train-time-arrival-service a)))
            (or (null route) (equal route (train-time-arrival-route a)))))
     arrivals)))

(defun train-time--filter-description ()
  "Describe active filters, or return nil when there are none."
  (let (parts)
    (cl-loop for (key val) on train-time--filters by #'cddr
             when val
             do (push (format "%s=%s" (substring (symbol-name key) 1) val) parts))
    (and parts (string-join (nreverse parts) " "))))

(defun train-time--status (now)
  "Return a status string describing data freshness at NOW."
  (let ((age (and train-time--feed-time (- now train-time--feed-time))))
    (concat
     (cond
      (train-time--loading "Updating…  ")
      (t ""))
     (cond
      (train-time--feed-time
       (let ((text (format "Feed time %s (%ds old)"
                           (format-time-string "%H:%M:%S %Z" train-time--feed-time
                                               train-time-mta-time-zone)
                           (max 0 age))))
         (if (> age train-time-stale-threshold)
             (propertize (concat text " — STALE, predictions may be wrong")
                         'face 'train-time-stale)
           text)))
      ((not train-time--loading) "No data yet"))
     (when train-time--error
       (concat "\n" (propertize (format "Last update failed: %s (g to retry)"
                                        train-time--error)
                                'face 'train-time-error)))
     (let ((hidden (and (not train-time--show-terminating)
                        (cl-count-if #'train-time-arrival-terminating
                                     train-time--arrivals))))
       (when (and hidden (> hidden 0))
         (format "\n%d terminating train%s hidden  (t to show)"
                 hidden (if (= hidden 1) "" "s"))))
     (let ((filters (train-time--filter-description)))
       (when filters (format "\nFilters: %s  (c to clear)" filters)))
     (when train-time--timer
       (format "\nAuto refresh every %ds" train-time-auto-refresh-interval))
     "\n" train-time-mta-attribution)))

(defun train-time--render (&optional now)
  "Redisplay the current arrivals buffer as of NOW."
  (let* ((now (or now (funcall train-time-now-function)))
         (rows (train-time--filtered train-time--arrivals)))
    (when (and train-time-max-rows (> (length rows) train-time-max-rows))
      (setq rows (cl-subseq rows 0 train-time-max-rows)))
    (setq tabulated-list-entries
          (mapcar (lambda (a)
                    (let ((entry (train-time--entry a now)))
                      (aset (cadr entry) 6
                            (propertize (aref (cadr entry) 6)
                                        'train-time-time (train-time-arrival-time a)))
                      entry))
                  rows))
    (tabulated-list-print t)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-max))
        (when (and (null rows) (not train-time--loading))
          (insert (if train-time--arrivals
                      "  No trains match the current filters.\n"
                    "  No upcoming trains in the feed.\n")))
        (insert "\n" (train-time--status now) "\n")))
    (setq mode-line-process
          (cond (train-time--loading " updating")
                (train-time--error " error")
                ((and train-time--feed-time
                      (> (- now train-time--feed-time) train-time-stale-threshold))
                 " stale")))
    (force-mode-line-update)))

;;;; Fetching

(defun train-time--merge-results ()
  "Combine the per-feed results into the buffer's arrivals and status."
  (let (arrivals times errors)
    (dolist (entry train-time--feed-results)
      (let ((r (cdr entry)))
        (setq arrivals (append (plist-get r :arrivals) arrivals))
        (when (plist-get r :feed-time) (push (plist-get r :feed-time) times))
        (when (plist-get r :error)
          (push (if (cdr train-time--feed-results)
                    (format "%s: %s" (train-time--feed-name (car entry))
                            (plist-get r :error))
                  (plist-get r :error))
                errors))))
    (setq train-time--arrivals
          (sort arrivals (lambda (a b) (< (train-time-arrival-time a)
                                          (train-time-arrival-time b))))
          ;; The oldest feed decides staleness.
          train-time--feed-time (and times (apply #'min times))
          train-time--error (and errors (string-join (nreverse errors) "; ")))))

(defun train-time--feed-name (url)
  "Return a short name for feed URL, e.g. \"gtfs-ace\"."
  (replace-regexp-in-string "\\`.*\\(?:/\\|%2F\\)" "" url))

(defun train-time--handle-result (buffer request url result)
  "Store RESULT of fetching URL for REQUEST in BUFFER and redisplay it.
A failed feed keeps its previous arrivals, so one unavailable feed
does not blank a board that other feeds still update."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (= request train-time--request)
        (cl-decf train-time--pending)
        (setq train-time--loading (> train-time--pending 0))
        (let* ((entry (or (assoc url train-time--feed-results)
                          (car (push (list url) train-time--feed-results))))
               (previous (cdr entry))
               (err (plist-get result :error)))
          (unless err
            (condition-case decode-err
                (let* ((feed (train-time-gtfs-rt-decode (plist-get result :data)))
                       (now (funcall train-time-now-function)))
                  (setcdr entry
                          (list :arrivals (train-time-mta-arrivals
                                           feed train-time--station now)
                                :feed-time (train-time-gtfs-rt-feed-time feed)))
                  (setq train-time--fetched-at now))
              (error
               (setq err (format "Could not decode feed: %s"
                                 (error-message-string decode-err))))))
          (when err
            (setcdr entry (plist-put (copy-sequence previous) :error err))))
        (train-time--merge-results)
        (train-time--render)
        (when (and train-time--error (not train-time--loading))
          (message "train-time: %s" train-time--error))))))

(defun train-time-refresh (&rest _)
  "Fetch fresh predictions for the current arrivals buffer."
  (interactive)
  (unless (derived-mode-p 'train-time-mode)
    (user-error "Not in a train-time buffer"))
  (let ((buffer (current-buffer))
        (request (cl-incf train-time--request))
        (urls (train-time-mta-feed-urls train-time--station)))
    (setq train-time--pending (length urls)
          train-time--loading (> train-time--pending 0))
    (train-time--render)
    (dolist (url urls)
      (funcall train-time-fetch-function url
               (lambda (result)
                 (train-time--handle-result buffer request url result))))))

;;;; Auto refresh

(defun train-time--auto-refresh-tick (buffer)
  "Refresh BUFFER if it is still alive and visible."
  (if (not (buffer-live-p buffer))
      nil
    (with-current-buffer buffer
      (when (and (get-buffer-window buffer t) (not train-time--loading))
        (train-time-refresh)))))

(defun train-time--stop-timer ()
  "Cancel this buffer's auto-refresh timer."
  (when train-time--timer
    (cancel-timer train-time--timer)
    (setq train-time--timer nil)))

(defun train-time-toggle-auto-refresh (&optional arg)
  "Toggle periodic refresh of this buffer.
With positive ARG enable it, with zero or negative ARG disable it."
  (interactive "P")
  (let ((enable (if arg (> (prefix-numeric-value arg) 0) (null train-time--timer))))
    (train-time--stop-timer)
    (when enable
      (setq train-time--timer
            (run-at-time train-time-auto-refresh-interval
                         train-time-auto-refresh-interval
                         #'train-time--auto-refresh-tick (current-buffer))))
    (train-time--render)
    (message "Auto refresh %s" (if enable "enabled" "disabled"))))

;;;; Filters

(defun train-time--set-filter (key value)
  "Set filter KEY to VALUE and redisplay."
  (setq train-time--filters (plist-put train-time--filters key value))
  (train-time--render))

(defun train-time-filter-direction ()
  "Show only trains in one direction."
  (interactive)
  (let* ((choices (delq nil
                        (mapcar (lambda (d)
                                  (cons (format "%s (%s)"
                                                (capitalize (symbol-name (car d)))
                                                (or (cdr d) "terminating"))
                                        (car d)))
                                (train-time-station-directions train-time--station))))
         (choice (completing-read "Direction: " (cons "All" (mapcar #'car choices))
                                  nil t)))
    (train-time--set-filter :direction (cdr (assoc choice choices)))))

(defun train-time-filter-service ()
  "Show only local or only express trains."
  (interactive)
  (let ((choice (completing-read "Service: " '("All" "Local" "Express") nil t)))
    (train-time--set-filter :service (pcase choice
                                       ("Local" 'local) ("Express" 'express)))))

(defun train-time-filter-line ()
  "Show only trains on one line."
  (interactive)
  (let* ((routes (or (delete-dups (mapcar #'train-time-arrival-route
                                          train-time--arrivals))
                     (train-time-station-routes train-time--station)))
         (choice (completing-read "Line: " (cons "All" routes) nil t)))
    (train-time--set-filter :route (unless (equal choice "All") choice))))

(defun train-time-toggle-terminating ()
  "Toggle display of trains that terminate at this station."
  (interactive)
  (setq train-time--show-terminating (not train-time--show-terminating))
  (train-time--render))

(defun train-time-clear-filters ()
  "Remove all filters."
  (interactive)
  (setq train-time--filters nil)
  (train-time--render))

(defun train-time-describe-trip ()
  "Show details of the trip at point."
  (interactive)
  (let* ((id (or (tabulated-list-get-id) (user-error "No train on this line")))
         (arrival (cl-find-if (lambda (a)
                                (and (equal (train-time-arrival-trip-id a) (car id))
                                     (equal (train-time-arrival-stop-id a) (cdr id))))
                              train-time--arrivals)))
    (message "Trip %s, platform %s, train %s, %s, %s"
             (train-time-arrival-trip-id arrival)
             (train-time-arrival-stop-id arrival)
             (or (train-time-arrival-train-id arrival) "unknown")
             (format-time-string "%Y-%m-%d %H:%M:%S %Z"
                                 (train-time-arrival-time arrival)
                                 train-time-mta-time-zone)
             (if (train-time-arrival-assigned arrival)
                 "tracked train"
               "scheduled, no train assigned yet"))))

;;;; Evil integration

(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))
(declare-function evil-set-initial-state "evil-core" (mode state))

(defcustom train-time-evil-bindings t
  "Non-nil means train-time buffers get Evil normal-state bindings.
Takes effect the first time each train-time mode starts while Evil
is loaded."
  :type 'boolean
  :group 'train-time)

(defvar train-time--evil-modes nil
  "Modes whose Evil bindings have been installed.")

(defun train-time-evil-setup (mode map bindings)
  "Give MODE Evil normal state and BINDINGS in MAP, once per session.
BINDINGS alternate keys and commands.  Does nothing unless Evil is
loaded and `train-time-evil-bindings' is non-nil.  Call it from the
mode body, which runs before Evil picks the buffer's initial state."
  (when (and train-time-evil-bindings
             (featurep 'evil)
             (not (memq mode train-time--evil-modes)))
    (push mode train-time--evil-modes)
    (evil-set-initial-state mode 'normal)
    (apply #'evil-define-key* 'normal map bindings)))

(defconst train-time--evil-bindings
  `("gr" train-time-refresh
    "a" train-time-toggle-auto-refresh
    "d" train-time-filter-direction
    "s" train-time-filter-service
    "l" train-time-filter-line
    "c" train-time-clear-filters
    "t" train-time-toggle-terminating
    "S" train-time-select-station
    "m" train-time-osm
    ,(kbd "RET") train-time-describe-trip
    "q" quit-window)
  "Evil normal-state bindings for `train-time-mode'.")

;;;; Mode and commands

(defvar train-time-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" #'train-time-toggle-auto-refresh)
    (define-key map "d" #'train-time-filter-direction)
    (define-key map "s" #'train-time-filter-service)
    (define-key map "l" #'train-time-filter-line)
    (define-key map "c" #'train-time-clear-filters)
    (define-key map "t" #'train-time-toggle-terminating)
    (define-key map "S" #'train-time-select-station)
    (define-key map "m" #'train-time-osm)
    (define-key map (kbd "RET") #'train-time-describe-trip)
    map)
  "Keymap for `train-time-mode'.")

(define-derived-mode train-time-mode tabulated-list-mode "Train-Time"
  "Major mode listing upcoming trains at a station.

\\{train-time-mode-map}"
  (setq tabulated-list-format
        [("Line" 5 nil)
         ("Service" 8 nil)
         ("Toward" 18 nil)
         ("Destination" 20 nil)
         ("Event" 8 nil)
         ("Time" 6 nil)
         ("In" 8 train-time--sort-by-time :right-align t)
         ("Status" 10 nil)
         ("Train" 0 nil)])
  (setq tabulated-list-padding 1
        tabulated-list-sort-key nil)
  (setq-local revert-buffer-function #'train-time-refresh)
  (add-hook 'kill-buffer-hook #'train-time--stop-timer nil t)
  (tabulated-list-init-header)
  (train-time-evil-setup 'train-time-mode train-time-mode-map
                         train-time--evil-bindings))

(autoload 'train-time-map "train-time-map"
  "Navigate the transit network diagram." t)
(autoload 'train-time-osm "train-time-osm"
  "Show a station and its line on OpenStreetMap using osm.el." t)

(defun train-time--station-candidate (station)
  "Return the completion candidate for STATION: its label and routes."
  (let ((label (train-time-station-display-name station)))
    (if (equal label (train-time-station-name station))
        (format "%s (%s)" label
                (mapconcat #'identity
                           (delete-dups
                            (mapcar (lambda (r) (car (train-time-mta--route r)))
                                    (train-time-station-routes station)))
                           " "))
      label)))

(defun train-time--read-station ()
  "Read a station with completion."
  (let* ((table (mapcar (lambda (s) (cons (train-time--station-candidate s) s))
                        train-time-mta-stations))
         (default (train-time-mta-station train-time-default-station))
         (default-name (and default (train-time--station-candidate default)))
         (name (completing-read
                (if default
                    (format "Station (default %s): " default-name)
                  "Station: ")
                table nil t nil nil default-name)))
    (cdr (assoc name table))))

(defun train-time--buffer-name (station)
  "Return the name of STATION's arrivals buffer."
  (format "*train-time: %s*" (train-time-station-display-name station)))

(defun train-time--show (station &optional buffer)
  "Display arrivals for STATION in BUFFER or a dedicated buffer."
  (pop-to-buffer-same-window (train-time-board station buffer)))

(defun train-time-board (station &optional buffer)
  "Return an arrivals buffer for STATION, starting a refresh.
Use BUFFER if non-nil, otherwise the station's dedicated buffer.
The buffer is not displayed."
  (let ((buffer (or buffer
                    (get-buffer-create
                     (train-time--buffer-name station)))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'train-time-mode)
        (train-time-mode)
        (setq train-time--show-terminating train-time-show-terminating)
        (when train-time-auto-refresh
          (train-time-toggle-auto-refresh 1)))
      (unless (eq station train-time--station)
        (setq train-time--station station
              train-time--arrivals nil
              train-time--feed-results nil
              train-time--feed-time nil
              train-time--error nil))
      (train-time-refresh))
    buffer))

;;;###autoload
(defun train-time (station)
  "Show upcoming trains at STATION, read with completion."
  (interactive (list (train-time--read-station)))
  (train-time--show station))

;;;###autoload
(defun train-time-select-station (station)
  "Show STATION in the current arrivals buffer, or in a new one."
  (interactive (list (train-time--read-station)))
  (if (derived-mode-p 'train-time-mode)
      (progn
        (train-time--show station (current-buffer))
        (rename-buffer (train-time--buffer-name station) t))
    (train-time--show station)))

(provide 'train-time)
;;; train-time.el ends here
