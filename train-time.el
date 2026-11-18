;;; train-time.el --- Real-time train arrivals and departures -*- lexical-binding: t; -*-

;; Copyright (C) 2026  train-time.el contributors

;; Author: train-time.el contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, comm
;; URL: https://github.com/jeongsoolee09/train-time.el

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
;;   D  board of the train's destination (or click the destination)
;;   A  service alerts for the station's lines
;;
;; Below the trains the board lists MTA service alerts in effect for
;; its lines and elevator or escalator outages at the station (see
;; `train-time-show-alerts').
;;
;; At a station with free transfers the board covers the whole
;; complex (see `train-time-board-scope'), grouped by platform and
;; direction:
;;   TAB  more trains / fold the group   S-TAB  fold all groups
;;   M-n / M-p  next / previous group
;;
;; M-x train-time-map navigates a text diagram of each line, like
;; `undo-tree-visualize'; RET there opens a station's board.
;;
;; M-x train-time-home-to-work and M-x train-time-work-to-home list
;; the next trips between `train-time-home' and `train-time-work'.
;;
;; In text terminals, including Termux on Android, turn on
;; `xterm-mouse-mode' and everything can be done by tapping: buffers
;; get a row of buttons (see `train-time-touch-bar'), and windows
;; narrower than `train-time-compact-width' get a compact layout.

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
  "Maximum number of trains to list at a single station, or nil for no limit.
Complex boards limit each group instead; see `train-time-group-max-rows'."
  :type '(choice (const :tag "No limit" nil) natnum)
  :group 'train-time)

(defcustom train-time-board-scope 'complex
  "What a board shows at a station with free transfers to others.
`complex' lists the trains of every station in the complex, grouped
by platform and direction: Grand Central shows the 4 5 6, the 7 and
the S together.  `station' lists only the station chosen."
  :type '(choice (const :tag "The whole station complex" complex)
                 (const :tag "Only the chosen station" station))
  :group 'train-time)

(defcustom train-time-group-max-rows 3
  "Trains listed per platform and direction on a complex board.
nil means no limit.  \\<train-time-mode-map>\\[train-time-cycle-group] in a group cycles between this
limit, `train-time-max-rows' trains and folding it away."
  :type '(choice (const :tag "No limit" nil) natnum)
  :group 'train-time)

(defcustom train-time-show-alerts t
  "Non-nil means boards and trip lists show alerts for their lines.
These are MTA's service alerts in effect (delays, suspensions,
reroutes and planned work) and elevator and escalator outages at
the stations.  They are downloaded with the trains and reused for
`train-time-alerts-refresh-interval' seconds."
  :type 'boolean
  :group 'train-time)

(defcustom train-time-compact-width 80
  "Windows narrower than this many columns get the compact layout.
Meant for phones, it lists each train's line, destination, time
and minutes only, grouped by direction; trip lists drop their
travel time and status.  0 means never."
  :type 'natnum
  :group 'train-time)

(defcustom train-time-touch-bar 'auto
  "Whether train-time buffers start with a row of buttons.
The buttons run the commonest commands with a click or a tap.
t means always, nil never, and `auto' on text terminals, where
Termux delivers taps as mouse clicks, and on Android."
  :type '(choice (const :tag "On text terminals and Android" auto)
                 (const :tag "Always" t)
                 (const :tag "Never" nil))
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

(defface train-time-group '((t :inherit bold))
  "Face for platform and direction headings on complex boards.")

(defface train-time-group-count '((t :inherit shadow))
  "Face for the train counts on complex board headings.")

(defface train-time-destination '((t))
  "Face for destinations on boards, which are links to their boards.")

(defface train-time-touch-button
  '((((type tty)) :inverse-video t)
    (t :inherit custom-button))
  "Face for the buttons of the touch bar; see `train-time-touch-bar'.")

;;;; Window width, buttons and the touch bar

(defvar-local train-time--width nil
  "Window width the buffer was last drawn for.")
(defvar-local train-time--redraw-function nil
  "Function redrawing the buffer when its window's width changes.")

(defun train-time--window-width ()
  "Return the body width of the window showing the current buffer.
Prefer the selected window; without one, use the frame's width."
  (let ((window (if (eq (window-buffer) (current-buffer))
                    (selected-window)
                  (get-buffer-window nil t))))
    (if window (window-body-width window) (frame-width))))

(defun train-time--compact-p (&optional width)
  "Return non-nil if the compact layout suits WIDTH, by default the window's.
See `train-time-compact-width'."
  (< (or width (train-time--window-width)) train-time-compact-width))

(defun train-time--width-changed (window)
  "Redraw WINDOW's buffer if it was drawn for another width."
  (with-current-buffer (window-buffer window)
    (when (and train-time--redraw-function train-time--width
               (/= train-time--width (window-body-width window)))
      (funcall train-time--redraw-function))))

(defun train-time--narrow-frame-p ()
  "Return non-nil if the frame is too narrow for long echo area messages."
  (train-time--compact-p (frame-width)))

(defun train-time--echo (text)
  "Show TEXT in the echo area, filled to the frame in narrow frames."
  (message "%s" (if (train-time--narrow-frame-p)
                    (train-time--fill text (max 20 (1- (frame-width))))
                  text)))

(defun train-time--watch-width (redraw)
  "Call REDRAW whenever the width of the current buffer's window differs.
REDRAW must set `train-time--width'."
  (setq train-time--redraw-function redraw)
  (add-hook 'window-size-change-functions #'train-time--width-changed nil t))

(defun train-time--fill (text width &optional indent)
  "Return TEXT with each of its lines filled to WIDTH columns.
Continuation lines keep the line's indentation, or with INDENT
every line is indented by that many spaces instead."
  (mapconcat
   (lambda (line)
     (if (string-blank-p line)
         ""
       (let ((prefix (if indent
                         (make-string indent ?\s)
                       (progn (string-match "\\`[ \t]*" line)
                              (match-string 0 line)))))
         (with-temp-buffer
           (let ((fill-column width)
                 (fill-prefix prefix))
             (insert prefix (string-trim line))
             (fill-region (point-min) (point-max))
             (string-trim-right (buffer-string)))))))
   (split-string text "\n")
   "\n"))

(defun train-time--hang (text width indent)
  "Return one-line TEXT filled to WIDTH, continuation lines indented INDENT.
Spacing within TEXT is kept."
  (let ((lead (progn (string-match "\\` *" text) (match-end 0))))
    (with-temp-buffer
      (let ((fill-column width)
            (fill-prefix (make-string indent ?\s))
            (adaptive-fill-mode nil))
        ;; Filling drops the first line's indentation, so hold its place.
        (insert (make-string lead ?x) (substring text lead))
        (fill-region (point-min) (point-max) nil t)
        (goto-char (point-min))
        (delete-char lead)
        (insert (substring text 0 lead))
        (string-trim-right (buffer-string))))))

(defun train-time--button (label command &rest props)
  "Return LABEL as a button running COMMAND interactively.
PROPS are more button properties, such as `face' or `help-echo'."
  (with-temp-buffer
    (apply #'insert-text-button label
           'action (lambda (_) (call-interactively command))
           'follow-link t
           'help-echo (or (plist-get props 'help-echo)
                          (format "mouse-1, RET: %s" command))
           props)
    (buffer-string)))

(defun train-time--touch-bar-p ()
  "Return non-nil if buffers should show the touch bar."
  (if (eq train-time-touch-bar 'auto)
      (or (eq system-type 'android) (not (display-graphic-p)))
    train-time-touch-bar))

(defun train-time--touch-bar (items)
  "Return a touch bar of ITEMS ending in a newline, or \"\" if there is none.
ITEMS are (LABEL COMMAND) lists; the bar wraps at the window width."
  (if (not (train-time--touch-bar-p))
      ""
    (let ((width (train-time--window-width))
          (column 1)
          (parts (list " ")))
      (dolist (item items)
        (let ((button (train-time--button (format " %s " (car item)) (cadr item)
                                          'face 'train-time-touch-button
                                          'train-time-touch-bar t)))
          ;; The last column of a terminal window holds the wrap indicator.
          (when (and (> column 1) (>= (+ column 1 (string-width button)) width))
            (push "\n " parts)
            (setq column 1))
          (when (> column 1)
            (push " " parts)
            (cl-incf column))
          (push button parts)
          (cl-incf column (string-width button))))
      (concat (apply #'concat (nreverse parts)) "\n"))))

(defun train-time--skip-touch-bar ()
  "Move point past the touch bar if it is on it."
  (while (and (not (eobp))
              (save-excursion
                (forward-line 0)
                (text-property-not-all (point) (line-end-position)
                                       'train-time-touch-bar nil)))
    (forward-line 1)))

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

(defvar-local train-time--station nil
  "Station chosen for this buffer; its trains are listed first.")
(defvar-local train-time--stations nil
  "Stations whose trains this buffer lists, `train-time--station' first.
On a complex board these include the stations it has free transfers to.")
(defvar-local train-time--group-states nil
  "Alist of (GROUP-KEY . STATE) on a complex board.
STATE is `all' to list every train of the group or `folded' to
hide them; groups not listed show `train-time-group-max-rows'.")
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
(defvar-local train-time--compact nil
  "Non-nil if the board is drawn in the compact layout.")
(defvar-local train-time--filters nil
  "Plist of active filters: :direction, :service, :route.")
(defvar-local train-time--last-trip nil
  "Entry ID of the train last described, as by a tap.")

;;;; Rendering

(defun train-time--bullet (label route-id)
  "Return LABEL as a bullet in the colors of ROUTE-ID, padded by spaces."
  (let ((color (and route-id (train-time-mta-route-color route-id))))
    (if color
        (propertize (format " %s " label)
                    'face `(:background ,color
                            :foreground ,(or (train-time-mta-route-text-color route-id)
                                             "white")
                            :weight bold))
      (format " %s " label))))

(defun train-time--route-label (arrival)
  "Return the rider-facing route bullet for ARRIVAL, in the route's colors."
  (let ((route (train-time-arrival-route arrival)))
    (train-time--bullet (if (eq (train-time-arrival-service arrival) 'express)
                            (format "<%s>" route)
                          route)
                        (train-time-arrival-route-id arrival))))

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

(defun train-time--destination-cell (arrival assigned)
  "Return ARRIVAL's Destination cell, a link to the destination's board.
ASSIGNED nil dims it as a scheduled trip."
  (let ((id (train-time-arrival-destination-id arrival))
        (name (if (train-time-arrival-terminating arrival)
                  "Terminates here"
                (or (train-time-arrival-destination arrival) "?")))
        (face (if assigned 'train-time-destination 'train-time-scheduled)))
    (if (or (null id) (train-time-arrival-terminating arrival))
        (propertize name 'face face)
      (list name
            'face face
            'mouse-face 'highlight
            'follow-link t
            'train-time-destination id
            'action #'train-time--destination-button
            'help-echo (format "mouse-1, RET: trains at %s" name)))))

(defun train-time--destination-button (button)
  "Show the board of the destination BUTTON links to."
  (train-time-goto-destination (button-get button 'train-time-destination)))

(defun train-time--entry (arrival now)
  "Return a `tabulated-list-entries' element for ARRIVAL at time NOW."
  (let* ((assigned (train-time-arrival-assigned arrival))
         (minutes (train-time-minutes-until (train-time-arrival-time arrival) now))
         (dim (lambda (s) (if assigned s (propertize s 'face 'train-time-scheduled))))
         (time (funcall dim (train-time-format-clock (train-time-arrival-time arrival)
                                                     train-time-mta-time-zone)))
         (in (funcall dim (train-time--minutes-label minutes))))
    (list (cons (train-time-arrival-trip-id arrival)
                (train-time-arrival-stop-id arrival))
          (if train-time--compact
              (vconcat (list (train-time--route-label arrival)
                             (train-time--destination-cell arrival assigned))
                       (and (train-time--clock-column-p) (list time))
                       (list in))
            (vector
             (train-time--route-label arrival)
             (funcall dim (train-time--service-label arrival))
             (funcall dim (train-time--direction-label arrival))
             (train-time--destination-cell arrival assigned)
             (funcall dim (if (eq (train-time-arrival-event arrival) 'departure)
                              "Departs" "Arrives"))
             time in
             (funcall dim (if assigned "Tracked" "Scheduled"))
             (funcall dim (or (train-time-arrival-train-id arrival) "")))))))

(defconst train-time--wide-format
  [("Line" 5 nil)
   ("Service" 8 nil)
   ("Toward" 18 nil)
   ("Destination" 20 nil)
   ("Event" 8 nil)
   ("Time" 6 nil)
   ("In" 8 nil :right-align t)
   ("Status" 10 nil)
   ("Train" 0 nil)]
  "Board columns in the usual layout.")

(defun train-time--clock-column-p ()
  "Return non-nil if the compact board has room for the Time column.
Below 44 columns, as on a phone, the destination needs the room more;
tapping a train still shows its time."
  (>= (or train-time--width 80) 44))

(defun train-time--set-format (width)
  "Set the board's columns for a window WIDTH columns wide."
  (let ((format (if train-time--compact
                    ;; Padding, Line, Time and In take 21 columns, and
                    ;; the last column must not reach the window edge.
                    (vconcat (list '("Line" 5 nil))
                             (if (train-time--clock-column-p)
                                 (list (list "Destination" (max 8 (- width 22)) nil)
                                       '("Time" 5 nil))
                               (list (list "Destination" (max 8 (- width 16)) nil)))
                             (list '("In" 7 nil :right-align t)))
                  train-time--wide-format)))
    (unless (equal format tabulated-list-format)
      (setq tabulated-list-format format)
      (tabulated-list-init-header))))

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
       (concat "\n" (propertize (format "Last update failed: %s " train-time--error)
                                'face 'train-time-error)
               (train-time--button "(g to retry)" #'train-time-refresh)))
     (let ((alerts (and train-time-show-alerts (featurep 'train-time-alerts)
                        (train-time-alerts-summary (train-time--routes)
                                                   train-time--stations now))))
       (when alerts (concat "\n" alerts)))
     (let ((reminders (and (featurep 'train-time-remind)
                           (train-time-remind-summary train-time--stations))))
       (when reminders (concat "\n" reminders)))
     (let ((hidden (and (not train-time--show-terminating)
                        (cl-count-if #'train-time-arrival-terminating
                                     train-time--arrivals))))
       (when (and hidden (> hidden 0))
         (concat (format "\n%d terminating train%s hidden  "
                         hidden (if (= hidden 1) "" "s"))
                 (train-time--button "(t to show)" #'train-time-toggle-terminating))))
     (let ((filters (train-time--filter-description)))
       (when filters
         (concat (format "\nFilters: %s  " filters)
                 (train-time--button "(c to clear)" #'train-time-clear-filters))))
     (when train-time--timer
       (format "\nAuto refresh every %ds" train-time-auto-refresh-interval))
     (when (train-time--grouped-p)
       (cond ((train-time--touch-bar-p)
              "\nTap a heading for more trains or to fold its group.")
             ((bound-and-true-p evil-local-mode)
              "\nTAB more trains or fold   S-TAB fold all   gj/gk next/previous group")
             (t
              "\nTAB more trains or fold   S-TAB fold all   M-n/M-p next/previous group")))
     "\n" train-time-mta-attribution)))

(defun train-time--routes ()
  "Return the route IDs serving this board's stations."
  (delete-dups (apply #'append (mapcar #'train-time-station-routes
                                       train-time--stations))))

(defun train-time--alerts-mode-line (routes stations now)
  "Return a mode line note of alerts in effect for ROUTES at NOW, or nil.
STATIONS are the stations shown, which leave out other stations' notices."
  (let ((count (and train-time-show-alerts (featurep 'train-time-alerts)
                    (train-time-alerts-count routes stations now))))
    (and count (> count 0)
         (propertize (format " ⚠%d" count) 'face 'warning
                     'help-echo "Service alerts in effect"))))

(defun train-time--complex-p ()
  "Return non-nil if this board lists several stations of a complex."
  (cdr train-time--stations))

(defun train-time--grouped-p ()
  "Return non-nil if this board groups trains by platform and direction.
Complex boards always do; others do in the compact layout."
  (or (train-time--complex-p) train-time--compact))

(defun train-time--groups ()
  "Return this complex board's groups as (KEY STATION DIRECTION LABEL).
There is one group per platform direction, the chosen station's
first.  KEY is (STATION-ID . DIRECTION); LABEL is nil at a
terminal, where every train in that direction ends its trip."
  (let (groups)
    (dolist (station train-time--stations)
      (dolist (dir (train-time-station-directions station))
        (push (list (cons (train-time-station-id station) (car dir))
                    station (car dir) (cdr dir))
              groups)))
    (nreverse groups)))

(defun train-time--group-key (arrival)
  "Return the key of the complex board group ARRIVAL belongs to."
  (cons (train-time-mta--parent-stop (train-time-arrival-stop-id arrival))
        (train-time-arrival-direction arrival)))

(defun train-time--station-bullets (station)
  "Return the colored bullets of the routes stopping at STATION."
  (mapconcat (lambda (name)
               (train-time--bullet
                name (cl-find-if (lambda (id) (equal (car (train-time-mta--route id)) name))
                                 (train-time-station-routes station))))
             (train-time-mta-route-names (list station))
             ""))

(defun train-time--group-heading (station label state shown total)
  "Return the heading of a group of STATION toward LABEL.
STATE is the group's entry in `train-time--group-states'; SHOWN of
TOTAL trains are listed, or would be if the group were unfolded.
In the compact layout a long heading is wrapped."
  (concat
   (funcall
    (if train-time--compact
        (lambda (text) (train-time--hang text (max 20 (1- (or train-time--width 80))) 4))
      #'identity)
    (concat
     " "
     (propertize (if (eq state 'folded) "▸ " "▾ ") 'face 'train-time-group)
     (train-time--station-bullets station)
     "  "
     (propertize
      (concat (unless (equal (train-time-station-name station)
                             (train-time-station-name train-time--station))
                (concat (train-time-station-name station) " · "))
              (or label "Terminating here"))
      'face 'train-time-group)
     "  "
     (propertize (cond ((zerop total) "no trains")
                       ((< shown total)
                        (format (if train-time--compact "%d/%d" "%d of %d") shown total))
                       (t (format "%d train%s" total (if (= total 1) "" "s"))))
                 'face 'train-time-group-count)))
   "\n"))

(defun train-time--shown-groups (rows)
  "Return the groups listed for ROWS, each (GROUP . TRAINS).
Groups that cannot have trains now are left out: terminal directions
with arrivals hidden, and the other platforms under a filter."
  (let ((filtered (train-time--filter-description)))
    (cl-loop for group in (train-time--groups)
             for trains = (cl-remove-if-not
                           (lambda (a) (equal (train-time--group-key a) (car group)))
                           rows)
             when (or trains (and (nth 3 group) (not filtered)))
             collect (cons group trains))))

(defun train-time--group-limit (state groups)
  "Return how many trains a group in STATE lists when GROUPS are shown.
A lone group, as at a terminal, lists as many as a plain board."
  (if (or (eq state 'all) (null (cdr groups)))
      train-time-max-rows
    train-time-group-max-rows))

(defun train-time--print-groups (rows now)
  "Print ROWS at time NOW grouped by platform and direction.
Set `tabulated-list-entries' to the entries printed."
  (let* ((groups (train-time--shown-groups rows))
         entries)
    (pcase-dolist (`((,key ,station ,_direction ,label) . ,trains) groups)
      (let* ((state (cdr (assoc key train-time--group-states)))
             (limit (train-time--group-limit state groups))
             (shown (cond ((eq state 'folded) nil)
                          ((and limit (> (length trains) limit))
                           (seq-take trains limit))
                          (t trains)))
             (start (point)))
        (insert (train-time--group-heading
                     station label state
                     (if (eq state 'folded) (length trains) (length shown))
                     (length trains)))
        (dolist (arrival shown)
          (let ((entry (train-time--entry arrival now)))
            (push entry entries)
            (funcall tabulated-list-printer (car entry) (cadr entry))))
        (add-text-properties start (point)
                             (list 'train-time-group key
                                   'train-time-station station))))
    (setq tabulated-list-entries (nreverse entries))))

(defun train-time--restore-point (id group column)
  "Move to the entry with ID, else to the heading of GROUP, at COLUMN."
  (goto-char (point-min))
  (let ((found nil))
    (when id
      (while (and (not found) (not (eobp)))
        (if (equal (tabulated-list-get-id) id)
            (setq found t)
          (forward-line 1))))
    (unless found
      (goto-char (point-min))
      (when group
        (while (and (not found) (not (eobp)))
          (if (equal (get-text-property (point) 'train-time-group) group)
              (setq found t)
            (forward-line 1)))))
    (if found
        (move-to-column column)
      (goto-char (point-min)))))

(defun train-time--board-bar ()
  "Return the touch bar of a board."
  (train-time--touch-bar
   `(("Refresh" train-time-refresh)
     (,(if train-time--timer "Auto: on" "Auto: off") train-time-toggle-auto-refresh)
     ("Station" train-time-select-station)
     ("Line" train-time-filter-line)
     ("Direction" train-time-filter-direction)
     ,@(and train-time--filters '(("Clear" train-time-clear-filters)))
     ("Alerts" train-time-alerts)
     ("Remind" train-time-remind)
     ("Map" train-time-map)
     ("OSM" train-time-osm)
     ("Quit" quit-window))))

(defun train-time--render (&optional now)
  "Redisplay the current arrivals buffer as of NOW."
  (let* ((now (or now (funcall train-time-now-function)))
         (rows (train-time--filtered train-time--arrivals))
         (width (train-time--window-width)))
    (setq train-time--width width
          train-time--compact (train-time--compact-p width))
    (train-time--set-format width)
    (if (train-time--grouped-p)
        (let ((id (tabulated-list-get-id))
              (group (get-text-property (point) 'train-time-group))
              (column (current-column))
              (inhibit-read-only t))
          (erase-buffer)
          (train-time--print-groups rows now)
          (train-time--restore-point id group column))
      (when (and train-time-max-rows (> (length rows) train-time-max-rows))
        (setq rows (cl-subseq rows 0 train-time-max-rows)))
      (setq tabulated-list-entries
            (mapcar (lambda (a) (train-time--entry a now)) rows))
      (tabulated-list-print t))
    (when (featurep 'train-time-remind)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when (train-time-reminded-p (tabulated-list-get-id))
            (tabulated-list-put-tag (propertize "*" 'face 'train-time-reminder)))
          (forward-line 1))))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-max))
        (when (and (null rows) (not train-time--loading))
          (insert (if train-time--arrivals
                      "  No trains match the current filters.\n"
                    "  No upcoming trains in the feed.\n")))
        (insert "\n" (train-time--fill (train-time--status now) (max 20 (1- width)))
                "\n")
        (goto-char (point-min))
        (insert (train-time--board-bar))))
    (when (bobp) (train-time--skip-touch-bar))
    (setq mode-line-process
          (concat (cond (train-time--loading " updating")
                        (train-time--error " error")
                        ((and train-time--feed-time
                              (> (- now train-time--feed-time) train-time-stale-threshold))
                         " stale"))
                  (train-time--alerts-mode-line (train-time--routes)
                                                train-time--stations now)))
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
                          (list :arrivals (mapcan (lambda (station)
                                                    (train-time-mta-arrivals
                                                     feed station now))
                                                  train-time--stations)
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
        (urls (delete-dups (mapcan #'train-time-mta-feed-urls train-time--stations))))
    (setq train-time--pending (length urls)
          train-time--loading (> train-time--pending 0))
    (train-time--render)
    (dolist (url urls)
      (funcall train-time-fetch-function url
               (lambda (result)
                 (train-time--handle-result buffer request url result))))
    (when train-time-show-alerts
      (train-time-alerts-fetch
       (lambda ()
         (when (buffer-live-p buffer)
           (with-current-buffer buffer (train-time--render))))))))

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
  (let* ((labels nil)
         (choices
          (progn
            ;; Platforms of a complex name the same direction differently.
            (dolist (station train-time--stations)
              (dolist (d (train-time-station-directions station))
                (cl-pushnew (or (cdr d) "terminating")
                            (alist-get (car d) labels) :test #'equal)))
            (mapcar (lambda (d)
                      (cons (format "%s (%s)" (capitalize (symbol-name (car d)))
                                    (string-join (reverse (cdr d)) ", "))
                            (car d)))
                    (nreverse labels))))
         (choice (let ((completion-ignore-case t))
                   (completing-read "Direction: " (cons "All" (mapcar #'car choices))
                                    nil t))))
    (train-time--set-filter :direction (cdr (assoc-string choice choices t)))))

(defun train-time-filter-service ()
  "Show only local or only express trains."
  (interactive)
  (let ((choice (let ((completion-ignore-case t))
                  (completing-read "Service: " '("All" "Local" "Express") nil t))))
    (train-time--set-filter :service (pcase (downcase choice)
                                       ("local" 'local) ("express" 'express)))))

(defun train-time-filter-line ()
  "Show only trains on one line."
  (interactive)
  (let* ((routes (or (delete-dups (mapcar #'train-time-arrival-route
                                          train-time--arrivals))
                     (train-time-mta-route-names train-time--stations)))
         (choice (let ((completion-ignore-case t))
                   (completing-read "Line: " (cons "All" routes) nil t))))
    (train-time--set-filter :route (unless (equal (downcase choice) "all")
                                     (or (car (member-ignore-case choice routes))
                                         choice)))))

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

(defun train-time-goto-destination (&optional id)
  "Show the board of the station ID, by default the destination at point.
Clicking a destination does the same."
  (interactive)
  (let* ((id (or id
                 (let ((entry (tabulated-list-get-id)))
                   (and entry
                        (train-time-arrival-destination-id
                         (train-time--arrival-at entry))))
                 (user-error "No destination on this line")))
         (station (or (train-time-mta-station id)
                      (user-error "Unknown station %s" id))))
    (train-time--show station)))

(defun train-time--arrival-at (id)
  "Return the arrival whose entry ID is (TRIP-ID . STOP-ID)."
  (cl-find-if (lambda (a)
                (and (equal (train-time-arrival-trip-id a) (car id))
                     (equal (train-time-arrival-stop-id a) (cdr id))))
              train-time--arrivals))

(defun train-time-describe-trip ()
  "Show details of the trip at point."
  (interactive)
  (let* ((id (or (tabulated-list-get-id) (user-error "No train on this line")))
         (arrival (train-time--arrival-at id))
         (service (train-time--service-label arrival))
         (toward (train-time--direction-label arrival))
         (minutes (train-time-minutes-until (train-time-arrival-time arrival)
                                            (funcall train-time-now-function))))
    (setq train-time--last-trip id)
    (train-time--echo
     (format "%s%s to %s%s\n%s %s (%s), %s%s"
             (string-trim (train-time--route-label arrival))
             (if (string-empty-p service) "" (concat " " service))
             (if (train-time-arrival-terminating arrival)
                 "this station (terminates)"
               (or (train-time-arrival-destination arrival) "?"))
             (if (string-empty-p toward) "" (format ", toward %s" toward))
             (if (eq (train-time-arrival-event arrival) 'departure) "Departs" "Arrives")
             ;; A phone's echo area has room for a few short lines only.
             (format-time-string (if (train-time--narrow-frame-p)
                                     "%H:%M"
                                   "%Y-%m-%d %H:%M:%S %Z")
                                 (train-time-arrival-time arrival)
                                 train-time-mta-time-zone)
             (train-time--minutes-label minutes)
             (cond ((train-time-arrival-assigned arrival) "tracked train")
                   ((train-time--narrow-frame-p) "scheduled")
                   (t "scheduled, no train assigned yet"))
             (if (train-time--narrow-frame-p)
                 ""
               (format "\nTrip %s, platform %s, train %s"
                       (train-time-arrival-trip-id arrival)
                       (train-time-arrival-stop-id arrival)
                       (or (train-time-arrival-train-id arrival) "unknown")))))))

(defun train-time-mouse-select (event)
  "Act on the board line clicked in EVENT.
Clicking a train describes it and clicking a group heading cycles
the group, which suits touch screens."
  (interactive "e")
  (mouse-set-point event)
  (cond ((tabulated-list-get-id) (train-time-describe-trip))
        ((train-time--heading-p) (train-time-cycle-group))))

;;;; Complex board groups

(defun train-time--group-at-point ()
  "Return the key of the complex board group at point, or signal an error."
  (or (get-text-property (point) 'train-time-group)
      (user-error (if (train-time--grouped-p)
                      "No platform here"
                    "This board has no groups; it has them in narrow windows"))))

(defun train-time-cycle-group ()
  "Cycle the group at point: more trains, then folded, then the usual few.
A group lists `train-time-group-max-rows' trains; when it has more,
the first cycle lists up to `train-time-max-rows'."
  (interactive)
  (let* ((key (train-time--group-at-point))
         (state (cdr (assoc key train-time--group-states)))
         (groups (train-time--shown-groups (train-time--filtered train-time--arrivals)))
         (total (length (cdr (cl-assoc key groups :key #'car :test #'equal))))
         (usual (train-time--group-limit nil groups))
         (next (pcase state
                 ('all 'folded)
                 ('folded nil)
                 (_ (if (and usual (> total usual)
                             (not (eql usual (train-time--group-limit 'all groups))))
                        'all
                      'folded)))))
    (setq train-time--group-states
          (cons (cons key next) (cl-remove key train-time--group-states
                                           :key #'car :test #'equal)))
    (train-time--render)
    (train-time--restore-point nil key 0)))

(defun train-time-cycle-all-groups ()
  "Fold every group, or unfold them all if they already are."
  (interactive)
  (unless (train-time--grouped-p)
    (user-error "This board has no groups; it has them in narrow windows"))
  (let* ((keys (mapcar #'car (train-time--groups)))
         (all-folded (cl-every (lambda (k) (eq (cdr (assoc k train-time--group-states))
                                               'folded))
                               keys))
         (group (get-text-property (point) 'train-time-group)))
    (setq train-time--group-states
          (unless all-folded (mapcar (lambda (k) (cons k 'folded)) keys)))
    (train-time--render)
    (train-time--restore-point nil group 0)))

(defun train-time--heading-p ()
  "Return non-nil if point is on a group heading."
  (and (get-text-property (point) 'train-time-group)
       (not (tabulated-list-get-id))))

(defun train-time-next-group (&optional n)
  "Move to the heading of the Nth next group."
  (interactive "p")
  (let ((n (or n 1))
        (step (if (< (or n 1) 0) -1 1)))
    (dotimes (_ (abs n))
      (let ((start (point))
            (found nil))
        (forward-line 0)
        (while (and (not found) (zerop (forward-line step)) (not (eobp)))
          (setq found (train-time--heading-p)))
        (unless found
          (goto-char start)
          (user-error "No %s group" (if (< step 0) "previous" "next")))))))

(defun train-time-previous-group (&optional n)
  "Move to the heading of the Nth previous group."
  (interactive "p")
  (train-time-next-group (- (or n 1))))

(defun train-time-station-at-point ()
  "Return the station of the group at point, else the board's station."
  (or (get-text-property (point) 'train-time-station)
      train-time--station))

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
    "gd" train-time-goto-destination
    "A" train-time-alerts
    "R" train-time-remind
    ,(kbd "TAB") train-time-cycle-group
    "za" train-time-cycle-group
    ,(kbd "<backtab>") train-time-cycle-all-groups
    "zM" train-time-cycle-all-groups
    "gj" train-time-next-group
    "gk" train-time-previous-group
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
    (define-key map "D" #'train-time-goto-destination)
    (define-key map "A" #'train-time-alerts)
    (define-key map "R" #'train-time-remind)
    (define-key map (kbd "TAB") #'train-time-cycle-group)
    (define-key map (kbd "<backtab>") #'train-time-cycle-all-groups)
    (define-key map (kbd "M-n") #'train-time-next-group)
    (define-key map (kbd "M-p") #'train-time-previous-group)
    (define-key map (kbd "RET") #'train-time-describe-trip)
    (define-key map [mouse-1] #'train-time-mouse-select)
    map)
  "Keymap for `train-time-mode'.")

(define-derived-mode train-time-mode tabulated-list-mode "Train-Time"
  "Major mode listing upcoming trains at a station.

\\{train-time-mode-map}"
  (setq tabulated-list-format train-time--wide-format)
  (setq tabulated-list-padding 1
        tabulated-list-sort-key nil)
  (setq-local revert-buffer-function #'train-time-refresh)
  (train-time--watch-width #'train-time--render)
  (add-hook 'kill-buffer-hook #'train-time--stop-timer nil t)
  (tabulated-list-init-header)
  (train-time-evil-setup 'train-time-mode train-time-mode-map
                         train-time--evil-bindings))

(autoload 'train-time-map "train-time-map"
  "Navigate the transit network diagram." t)
(autoload 'train-time-osm "train-time-osm"
  "Show a station and its line on OpenStreetMap using osm.el." t)
(autoload 'train-time-alerts "train-time-alerts"
  "List service alerts for some lines." t)
(autoload 'train-time-alerts-fetch "train-time-alerts"
  "Download alerts and outages if needed, then call CALLBACK once.")
(declare-function train-time-alerts-summary "train-time-alerts" (routes stations now))
(declare-function train-time-alerts-count "train-time-alerts" (routes stations now))
(autoload 'train-time-home-to-work "train-time-commute"
  "List the next trips from home to work." t)
(autoload 'train-time-work-to-home "train-time-commute"
  "List the next trips from work to home." t)
(autoload 'train-time-set-home "train-time-commute"
  "Make a station your home station." t)
(autoload 'train-time-set-work "train-time-commute"
  "Make a station your work station." t)
(autoload 'train-time-remind "train-time-remind"
  "Remind you some minutes before the train at point arrives." t)
(autoload 'train-time-cancel-reminder "train-time-remind"
  "Cancel a pending train reminder." t)
(autoload 'train-time-route "train-time-route"
  "Show the quickest scheduled journey between two stations." t)
(declare-function train-time-reminded-p "train-time-remind" (id))
(declare-function train-time-remind-summary "train-time-remind" (stations))

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

(defun train-time--read-station (&optional prompt default-id)
  "Read a station with completion, prompting with PROMPT.
DEFAULT-ID is the stop ID offered by default, falling back to
`train-time-default-station'."
  (let* ((table (mapcar (lambda (s) (cons (train-time--station-candidate s) s))
                        train-time-mta-stations))
         (prompt (or prompt "Station"))
         (default (train-time-mta-station (or default-id train-time-default-station)))
         (default-name (and default (train-time--station-candidate default)))
         ;; Capitals are a chore on a phone keyboard.
         (completion-ignore-case t)
         (name (completing-read
                (if default
                    (format "%s (default %s): " prompt default-name)
                  (format "%s: " prompt))
                table nil t nil nil default-name)))
    (cdr (assoc-string name table t))))

(defun train-time--board-stations (station)
  "Return the stations a board for STATION lists, STATION first.
See `train-time-board-scope'."
  (if (eq train-time-board-scope 'complex)
      (train-time-mta-complex station)
    (list station)))

(defun train-time--buffer-name (station)
  "Return the name of STATION's arrivals buffer.
Boards for a complex are named after it, so every station in the
complex shares one board."
  (format "*train-time: %s*"
          (if (cdr (train-time--board-stations station))
              (train-time-mta-complex-name station)
            (train-time-station-display-name station))))

(defun train-time--show (station &optional buffer)
  "Display arrivals for STATION in BUFFER or a dedicated buffer."
  (pop-to-buffer-same-window (train-time-board station buffer)))

(defun train-time-board (station &optional buffer)
  "Return an arrivals buffer for STATION, starting a refresh.
Use BUFFER if non-nil, otherwise the station's dedicated buffer.
The buffer is not displayed."
  (let ((buffer (or buffer
                    (get-buffer-create
                     (train-time--buffer-name station))))
        (moved nil))
    (with-current-buffer buffer
      (unless (derived-mode-p 'train-time-mode)
        (train-time-mode)
        (setq train-time--show-terminating train-time-show-terminating)
        (when train-time-auto-refresh
          (train-time-toggle-auto-refresh 1)))
      (unless (eq station train-time--station)
        (setq moved t)
        (let ((stations (train-time--board-stations station)))
          ;; Another station of the same complex only reorders the board.
          (unless (and (= (length stations) (length train-time--stations))
                       (null (cl-set-difference stations train-time--stations)))
            (setq train-time--arrivals nil
                  train-time--feed-results nil
                  train-time--feed-time nil
                  train-time--error nil
                  train-time--group-states nil))
          (setq train-time--station station
                train-time--stations stations)))
      (train-time-refresh)
      ;; The chosen station's platforms are listed first.
      (when moved
        (goto-char (point-min))
        (train-time--skip-touch-bar)))
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
