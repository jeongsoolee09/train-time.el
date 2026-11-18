;;; train-time-remind.el --- Train reminders and desktop notifications -*- lexical-binding: t; -*-

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

;; Reminders for individual trains.  On a board, `train-time-remind'
;; (R) asks to be told some minutes before the train at point reaches
;; the station.  While reminders are pending, the trip's real-time feed
;; is checked every `train-time-remind-interval' seconds, so the
;; reminder follows the train's latest prediction, whether or not a
;; board is open.  If the trip drops out of the feed, you are told so
;; rather than reminded at a guessed time.
;;
;; Notifications go to the echo area and, through
;; `train-time-notify-function', to the desktop: terminal-notifier or
;; osascript on macOS, termux-notification on Android (Termux:API), and
;; D-Bus notifications elsewhere.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'train-time)

(declare-function notifications-notify "notifications" (&rest params))

(defcustom train-time-remind-minutes 5
  "Minutes before a train's arrival to remind you, by default."
  :type 'natnum
  :group 'train-time)

(defcustom train-time-remind-interval 30
  "Seconds between checks of trains you asked to be reminded of."
  :type 'natnum
  :group 'train-time)

(defcustom train-time-notify-function #'train-time-notify-auto
  "Function that shows a desktop notification.
It is called with a title, a message and an ID; a later notification
with the same ID may replace the earlier one.  Notifications are
shown in the echo area as well, whatever this is."
  :type '(choice (function-item :tag "Pick automatically" train-time-notify-auto)
                 (function-item :tag "terminal-notifier (macOS)"
                                train-time-notify-terminal-notifier)
                 (function-item :tag "osascript (macOS)" train-time-notify-osascript)
                 (function-item :tag "termux-notification (Android)"
                                train-time-notify-termux)
                 (function-item :tag "D-Bus" train-time-notify-dbus)
                 (const :tag "Echo area only" ignore)
                 function)
  :group 'train-time)

(defcustom train-time-notify-sound "default"
  "Sound played with desktop notifications, or nil for none.
terminal-notifier and osascript take the name of a macOS sound, such
as \"Glass\"; termux-notification plays its default sound."
  :type '(choice (const :tag "None" nil) string)
  :group 'train-time)

(defface train-time-reminder '((t :inherit warning :weight bold))
  "Face of the mark on trains with a reminder."
  :group 'train-time)

;;;; Notifications

(defun train-time-notify--run (program &rest args)
  "Run PROGRAM with ARGS without waiting for it."
  (apply #'call-process program nil 0 nil args))

(defun train-time-notify-terminal-notifier (title message id)
  "Show TITLE and MESSAGE with terminal-notifier, replacing ID's last one."
  (apply #'train-time-notify--run "terminal-notifier"
         "-title" title
         ;; A leading bracket would be read as an option.
         "-message" (replace-regexp-in-string "\\`\\[" "\\\\[" message)
         "-group" id
         (append (and train-time-notify-sound (list "-sound" train-time-notify-sound))
                 (and (display-graphic-p) (list "-activate" "org.gnu.Emacs")))))

(defun train-time-notify--applescript-string (text)
  "Return TEXT as an AppleScript string literal."
  (concat "\"" (replace-regexp-in-string "[\"\\]" "\\\\\\&" text) "\""))

(defun train-time-notify-osascript (title message _id)
  "Show TITLE and MESSAGE with macOS's osascript."
  (train-time-notify--run
   "osascript" "-e"
   (concat "display notification " (train-time-notify--applescript-string message)
           " with title " (train-time-notify--applescript-string title)
           (if train-time-notify-sound
               (concat " sound name "
                       (train-time-notify--applescript-string train-time-notify-sound))
             ""))))

(defun train-time-notify-termux (title message id)
  "Show TITLE and MESSAGE with Termux:API's termux-notification."
  (apply #'train-time-notify--run "termux-notification"
         "--title" title "--content" message "--id" id
         (and train-time-notify-sound '("--sound"))))

(defun train-time-notify-dbus (title message _id)
  "Show TITLE and MESSAGE with a D-Bus desktop notification."
  (require 'notifications)
  (notifications-notify :title title :body message :app-name "train-time"))

(defun train-time-notify-auto (title message id)
  "Show TITLE and MESSAGE with the first notifier available.
ID lets a later notification replace this one."
  (cond ((executable-find "termux-notification")
         (train-time-notify-termux title message id))
        ((and (eq system-type 'darwin) (executable-find "terminal-notifier"))
         (train-time-notify-terminal-notifier title message id))
        ((and (eq system-type 'darwin) (executable-find "osascript"))
         (train-time-notify-osascript title message id))
        ((and (featurep 'dbusbind) (getenv "DBUS_SESSION_BUS_ADDRESS"))
         (train-time-notify-dbus title message id))))

(defun train-time-notify (title message id)
  "Show TITLE and MESSAGE in the echo area and on the desktop.
ID identifies the notification; see `train-time-notify-function'."
  (message "%s: %s" title message)
  (condition-case err
      (funcall train-time-notify-function title message id)
    (error (message "train-time: could not notify: %s" (error-message-string err)))))

;;;; Reminders

(cl-defstruct (train-time-reminder (:copier nil))
  "A request to be told before a train reaches a station."
  (station nil :documentation "The `train-time-station' the train calls at.")
  (stop-id nil :documentation "Platform the train calls at.")
  (trip-id nil :documentation "Trip identifier from the feed.")
  (route-id nil :documentation "Route identifier, for its feed.")
  (route nil :documentation "Route name shown to riders.")
  (service nil :documentation "`local', `express' or nil.")
  (destination nil :documentation "Name of the trip's last stop.")
  (minutes nil :documentation "Minutes before the train to remind.")
  (time nil :documentation "Latest predicted arrival, POSIX time.")
  (misses 0 :documentation "Checks in a row the trip was missing."))

(defvar train-time-remind--reminders nil
  "Pending reminders.")

(defvar train-time-remind--timer nil
  "Timer checking pending reminders.")

(defconst train-time-remind--max-misses 3
  "Checks a trip may be missing from its feed before the reminder is dropped.")

(defun train-time-remind--find (trip-id stop-id)
  "Return the reminder for TRIP-ID at platform STOP-ID, or nil."
  (cl-find-if (lambda (r) (and (equal (train-time-reminder-trip-id r) trip-id)
                               (equal (train-time-reminder-stop-id r) stop-id)))
              train-time-remind--reminders))

(defun train-time-reminded-p (id)
  "Return non-nil if the board entry ID, (TRIP-ID . STOP-ID), has a reminder."
  (and id (train-time-remind--find (car id) (cdr id))))

(defun train-time-remind--train (reminder)
  "Describe REMINDER's train, e.g. \"7 Express to 34 St-Hudson Yards\"."
  (format "%s%s to %s"
          (train-time-reminder-route reminder)
          (pcase (train-time-reminder-service reminder)
            ('express " Express") ('local " Local") (_ ""))
          (or (train-time-reminder-destination reminder) "?")))

(defun train-time-remind--station-name (reminder)
  "Return the name of REMINDER's station."
  (train-time-station-display-name (train-time-reminder-station reminder)))

(defun train-time-remind--describe (reminder)
  "Describe REMINDER in one line."
  (format "%s at %s, %s (remind %d min before)"
          (train-time-remind--train reminder)
          (train-time-remind--station-name reminder)
          (train-time-format-clock (train-time-reminder-time reminder)
                                   train-time-mta-time-zone)
          (train-time-reminder-minutes reminder)))

(defun train-time-remind--id (reminder)
  "Return the notification ID of REMINDER."
  (format "train-time-%s-%s" (train-time-reminder-trip-id reminder)
          (train-time-reminder-stop-id reminder)))

(defun train-time-remind--redisplay-boards ()
  "Redisplay the boards, so their reminder marks are current."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'train-time-mode)
        (train-time--render)))))

(defun train-time-remind--remove (reminder)
  "Forget REMINDER, stopping the checks if it was the last."
  (setq train-time-remind--reminders (delq reminder train-time-remind--reminders))
  (when (and (null train-time-remind--reminders) train-time-remind--timer)
    (cancel-timer train-time-remind--timer)
    (setq train-time-remind--timer nil))
  (train-time-remind--redisplay-boards))

(defun train-time-remind--add (reminder)
  "Add REMINDER and make sure trains are being checked."
  (push reminder train-time-remind--reminders)
  (unless train-time-remind--timer
    (setq train-time-remind--timer
          (run-at-time train-time-remind-interval train-time-remind-interval
                       #'train-time-remind-check)))
  (train-time-remind--redisplay-boards))

(defun train-time-remind--feed-url (reminder)
  "Return the URL of the feed carrying REMINDER's trip."
  (let ((feed (car (cl-find-if (lambda (f) (member (train-time-reminder-route-id reminder)
                                                   (cdr f)))
                               train-time-mta-feeds))))
    (and feed (concat train-time-mta-feed-base-url feed))))

(defun train-time-remind--due (reminder now)
  "Tell about REMINDER's train, which is due at NOW, and forget it."
  (let ((minutes (train-time-minutes-until (train-time-reminder-time reminder) now)))
    (train-time-notify
     (train-time-remind--train reminder)
     (format "%s %s %s (%s)"
             (if (<= minutes 0) "Arriving at" "Arrives at")
             (train-time-remind--station-name reminder)
             (if (<= minutes 0)
                 "now"
               (format "in %d min" minutes))
             (train-time-format-clock (train-time-reminder-time reminder)
                                      train-time-mta-time-zone))
     (train-time-remind--id reminder))
    (train-time-remind--remove reminder)))

(defun train-time-remind--lost (reminder)
  "Tell that REMINDER's train has left the feed, and forget it."
  (train-time-notify
   (train-time-remind--train reminder)
   (format "No longer in the real-time feed; it was due at %s %s.  Check the board."
           (train-time-remind--station-name reminder)
           (train-time-format-clock (train-time-reminder-time reminder)
                                    train-time-mta-time-zone))
   (train-time-remind--id reminder))
  (train-time-remind--remove reminder))

(defun train-time-remind--update (reminder feed now)
  "Update REMINDER from decoded FEED at NOW; notify if it is time."
  (let ((arrival (cl-find-if
                  (lambda (a) (and (equal (train-time-arrival-trip-id a)
                                          (train-time-reminder-trip-id reminder))
                                   (equal (train-time-arrival-stop-id a)
                                          (train-time-reminder-stop-id reminder))))
                  (train-time-mta-arrivals feed (train-time-reminder-station reminder)
                                           now))))
    (if (and arrival (train-time-arrival-time arrival))
        (setf (train-time-reminder-time reminder) (train-time-arrival-time arrival)
              (train-time-reminder-misses reminder) 0)
      (cl-incf (train-time-reminder-misses reminder)))
    (cond ((> (train-time-reminder-misses reminder) 0)
           (when (>= (train-time-reminder-misses reminder) train-time-remind--max-misses)
             (train-time-remind--lost reminder)))
          ((<= (- (train-time-reminder-time reminder) now)
               (* 60 (train-time-reminder-minutes reminder)))
           (train-time-remind--due reminder now)))))

(defun train-time-remind-check ()
  "Check the trains of pending reminders against their latest predictions."
  (let ((urls (delete-dups (delq nil (mapcar #'train-time-remind--feed-url
                                             train-time-remind--reminders)))))
    (dolist (url urls)
      (funcall
       train-time-fetch-function url
       (lambda (result)
         (let ((feed (and (plist-get result :data)
                          (ignore-errors (train-time-gtfs-rt-decode (plist-get result :data)))))
               (now (funcall train-time-now-function)))
           ;; A failed download says nothing about the trip; try again later.
           (when feed
             (dolist (reminder (copy-sequence train-time-remind--reminders))
               (when (and (memq reminder train-time-remind--reminders)
                          (equal (train-time-remind--feed-url reminder) url))
                 (train-time-remind--update reminder feed now))))))))))

(defun train-time-remind--target ()
  "Return the board entry ID a reminder is for, or nil.
That is the train at point; on the touch bar, the train last tapped."
  (or (tabulated-list-get-id)
      (and (save-excursion
             (forward-line 0)
             (text-property-not-all (point) (line-end-position)
                                    'train-time-touch-bar nil))
           train-time--last-trip)))

;;;###autoload
(defun train-time-remind (&optional minutes)
  "Remind you MINUTES before the train at point reaches the station.
Interactively, read MINUTES, `train-time-remind-minutes' by default.
If the train already has a reminder, cancel it instead.  From the
touch bar, this acts on the train last tapped."
  (interactive
   (let ((existing (and (derived-mode-p 'train-time-mode)
                        (train-time-reminded-p (train-time-remind--target)))))
     (list (unless existing
             (read-number "Remind how many minutes before? " train-time-remind-minutes)))))
  (unless (derived-mode-p 'train-time-mode)
    (user-error "Not in a train-time board"))
  (let* ((id (or (train-time-remind--target)
                 (user-error (if (train-time--touch-bar-p)
                                 "Tap a train first"
                               "No train on this line"))))
         (existing (train-time-reminded-p id))
         (arrival (train-time--arrival-at id))
         (minutes (or minutes train-time-remind-minutes))
         (now (funcall train-time-now-function)))
    (cond
     (existing
      (train-time-remind--remove existing)
      (message "Reminder cancelled: %s" (train-time-remind--describe existing)))
     ((null arrival)
      (user-error "That train is no longer on the board"))
     ((null (train-time-arrival-time arrival))
      (user-error "This train has no predicted time"))
     ((train-time-arrival-terminating arrival)
      (user-error "This train terminates here"))
     ((<= (- (train-time-arrival-time arrival) now) (* 60 minutes))
      (user-error "This train arrives in %s, sooner than that"
                  (train-time--minutes-label
                   (train-time-minutes-until (train-time-arrival-time arrival) now))))
     (t
      (let ((reminder (make-train-time-reminder
                       :station (or (train-time-mta-station
                                     (train-time-mta--parent-stop
                                      (train-time-arrival-stop-id arrival)))
                                    train-time--station)
                       :stop-id (train-time-arrival-stop-id arrival)
                       :trip-id (train-time-arrival-trip-id arrival)
                       :route-id (train-time-arrival-route-id arrival)
                       :route (train-time-arrival-route arrival)
                       :service (train-time-arrival-service arrival)
                       :destination (train-time-arrival-destination arrival)
                       :minutes minutes
                       :time (train-time-arrival-time arrival))))
        (train-time-remind--add reminder)
        (message "Reminder set: %s" (train-time-remind--describe reminder)))))))

;;;###autoload
(defun train-time-cancel-reminder (reminder)
  "Cancel REMINDER, read with completion among the pending ones."
  (interactive
   (let ((choices (mapcar (lambda (r) (cons (train-time-remind--describe r) r))
                          (or train-time-remind--reminders
                              (user-error "No reminders pending")))))
     (list (cdr (assoc (completing-read "Cancel reminder: " choices nil t) choices)))))
  (train-time-remind--remove reminder)
  (message "Reminder cancelled: %s" (train-time-remind--describe reminder)))

(defun train-time-remind-summary (stations)
  "Return a line about pending reminders at STATIONS, or nil."
  (let* ((ids (mapcar #'train-time-station-id stations))
         (reminders (cl-remove-if-not
                     (lambda (r) (member (train-time-station-id (train-time-reminder-station r))
                                         ids))
                     train-time-remind--reminders)))
    (when reminders
      (concat (propertize "*" 'face 'train-time-reminder)
              (format " Reminder%s: " (if (cdr reminders) "s" ""))
              (mapconcat #'train-time-remind--describe (reverse reminders) "; ")))))

(provide 'train-time-remind)
;;; train-time-remind.el ends here
