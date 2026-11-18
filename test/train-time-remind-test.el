;;; train-time-remind-test.el --- Tests for train reminders -*- lexical-binding: t; -*-

;;; Commentary:

;; Reminders are checked against recorded feeds at chosen times, and
;; notifiers are run with `call-process' stubbed out.

;;; Code:

(require 'ert)
(require 'train-time-test)
(require 'train-time-remind)

(defvar train-time-remind-test--notes nil
  "Notifications sent, as (TITLE MESSAGE ID), most recent first.")

(defmacro train-time-remind-test--with (&rest body)
  "Run BODY on the Flushing board with notifications recorded."
  (declare (indent 0))
  `(let ((train-time-remind--reminders nil)
         (train-time-remind--timer nil)
         (train-time-remind-test--notes nil)
         (train-time-notify-function
          (lambda (&rest note) (push note train-time-remind-test--notes))))
     (unwind-protect
         (train-time-test--with-buffer #'train-time-test--fixture-fetch
             train-time-test--fixture-time
           ,@body)
       (when train-time-remind--timer (cancel-timer train-time-remind--timer)))))

(defun train-time-remind-test--goto-train (minutes)
  "Move to the first tracked train more than MINUTES away; return it."
  (goto-char (point-min))
  (let (found)
    (while (and (not found) (not (eobp)))
      (let* ((id (tabulated-list-get-id))
             (arrival (and id (train-time--arrival-at id))))
        (if (and arrival
                 (> (train-time-minutes-until (train-time-arrival-time arrival)
                                              train-time-test--fixture-time)
                    minutes))
            (setq found arrival)
          (forward-line 1))))
    found))

(defun train-time-remind-test--check-at (time &optional fetch)
  "Check reminders as of TIME, fetching with FETCH."
  (let ((train-time-now-function (lambda () time))
        (train-time-fetch-function (or fetch #'train-time-test--fixture-fetch)))
    (train-time-remind-check)))

;;;; Reminders

(ert-deftest train-time-remind-test-due ()
  (train-time-remind-test--with
    (let* ((arrival (train-time-remind-test--goto-train 6))
           (time (train-time-arrival-time arrival)))
      (train-time-remind 5)
      (should (= (length train-time-remind--reminders) 1))
      (should train-time-remind--timer)
      ;; The board marks the train and lists the reminder.
      (should (equal (buffer-substring-no-properties
                      (line-beginning-position) (1+ (line-beginning-position)))
                     "*"))
      (should (string-match-p "Reminder: 7 \\(Express\\|Local\\) to 34 St-Hudson Yards at Flushing-Main St"
                              (train-time-test--text)))
      (train-time-remind-test--check-at (- time 360))
      (should-not train-time-remind-test--notes)
      (train-time-remind-test--check-at (- time 290))
      (should (= (length train-time-remind-test--notes) 1))
      (pcase-let ((`(,title ,message ,id) (car train-time-remind-test--notes)))
        (should (string-match-p "\\`7 .*to 34 St-Hudson Yards\\'" title))
        (should (string-match-p "\\`Arrives at Flushing-Main St in 4 min (" message))
        (should (string-prefix-p "train-time-" id)))
      (should-not train-time-remind--reminders)
      (should-not train-time-remind--timer)
      (should-not (string-match-p "Reminder" (train-time-test--text))))))

(ert-deftest train-time-remind-test-toggle ()
  (train-time-remind-test--with
    (train-time-remind-test--goto-train 6)
    (train-time-remind 5)
    (should train-time-remind--reminders)
    (call-interactively #'train-time-remind)
    (should-not train-time-remind--reminders)
    (should-not train-time-remind--timer)))

(ert-deftest train-time-remind-test-too-soon ()
  (train-time-remind-test--with
    (train-time-remind-test--goto-train 6)
    (should-error (train-time-remind 60) :type 'user-error)
    (goto-char (point-max))
    (should-error (train-time-remind 5) :type 'user-error)
    (should-not train-time-remind--reminders)))

(ert-deftest train-time-remind-test-lost ()
  "A trip that leaves the feed is reported, after a few checks."
  (train-time-remind-test--with
    (let* ((arrival (train-time-remind-test--goto-train 6))
           (time (train-time-arrival-time arrival))
           (broken (lambda (_url callback)
                     (funcall callback (list :error "Request timed out"))))
           (without (lambda (_url callback)
                      (funcall callback
                               (list :data (train-time-test--fixture "gtfs-nqrw"))))))
      (train-time-remind 5)
      ;; Failed downloads say nothing about the trip.
      (dotimes (_ 5) (train-time-remind-test--check-at (- time 600) broken))
      (should (zerop (train-time-reminder-misses (car train-time-remind--reminders))))
      (dotimes (_ 2) (train-time-remind-test--check-at (- time 600) without))
      (should train-time-remind--reminders)
      (should-not train-time-remind-test--notes)
      ;; Back in the feed, the count starts over.
      (train-time-remind-test--check-at (- time 600))
      (should (zerop (train-time-reminder-misses (car train-time-remind--reminders))))
      (dotimes (_ 3) (train-time-remind-test--check-at (- time 600) without))
      (should-not train-time-remind--reminders)
      (should (= (length train-time-remind-test--notes) 1))
      (should (string-match-p "No longer in the real-time feed"
                              (nth 1 (car train-time-remind-test--notes)))))))

(ert-deftest train-time-remind-test-touch-bar ()
  "The touch bar's Remind button acts on the train last tapped."
  (train-time-remind-test--with
    (let ((train-time-touch-bar t))
      (train-time--render)
      (let ((arrival (train-time-remind-test--goto-train 6)))
        (train-time-describe-trip)
        (goto-char (point-min))
        (search-forward "Remind")
        (should (get-text-property (1- (point)) 'train-time-touch-bar))
        (cl-letf (((symbol-function 'read-number) (lambda (&rest _) 5)))
          (push-button (1- (point))))
        (should (equal (train-time-reminder-trip-id (car train-time-remind--reminders))
                       (train-time-arrival-trip-id arrival)))))))

(ert-deftest train-time-remind-test-cancel ()
  (train-time-remind-test--with
    (train-time-remind-test--goto-train 6)
    (train-time-remind 5)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt choices &rest _) (car (car choices)))))
      (call-interactively #'train-time-cancel-reminder))
    (should-not train-time-remind--reminders)
    (should-error (call-interactively #'train-time-cancel-reminder) :type 'user-error)))

;;;; Notifiers

(defmacro train-time-remind-test--capturing (&rest body)
  "Run BODY, returning the program and arguments it ran."
  (declare (indent 0))
  `(let (runs)
     (cl-letf (((symbol-function 'call-process)
                (lambda (program _in destination _display &rest args)
                  (should (eql destination 0))
                  (push (cons program args) runs)
                  0)))
       ,@body)
     (nreverse runs)))

(ert-deftest train-time-remind-test-terminal-notifier ()
  (let ((train-time-notify-sound "Glass"))
    (should (equal (train-time-remind-test--capturing
                     (train-time-notify-terminal-notifier "7 to Hudson Yards"
                                                          "[Arrives] soon" "id1"))
                   '(("terminal-notifier" "-title" "7 to Hudson Yards"
                      "-message" "\\[Arrives] soon" "-group" "id1"
                      "-sound" "Glass"))))))

(ert-deftest train-time-remind-test-osascript ()
  (let ((train-time-notify-sound nil))
    (should (equal (train-time-remind-test--capturing
                     (train-time-notify-osascript "Say \"hi\"" "a\\b" "id"))
                   '(("osascript" "-e"
                      "display notification \"a\\\\b\" with title \"Say \\\"hi\\\"\""))))))

(ert-deftest train-time-remind-test-termux ()
  (let ((train-time-notify-sound "default"))
    (should (equal (train-time-remind-test--capturing
                     (train-time-notify-termux "T" "M" "id"))
                   '(("termux-notification" "--title" "T" "--content" "M"
                      "--id" "id" "--sound"))))))

(ert-deftest train-time-remind-test-auto ()
  (let ((train-time-notify-sound nil))
    (cl-flet ((with-programs (programs system)
                (cl-letf (((symbol-function 'executable-find)
                           (lambda (name &rest _) (and (member name programs) name))))
                  (let ((system-type system))
                    (caar (train-time-remind-test--capturing
                            (train-time-notify-auto "T" "M" "id")))))))
      (should (equal (with-programs '("termux-notification" "terminal-notifier") 'android)
                     "termux-notification"))
      (should (equal (with-programs '("terminal-notifier" "osascript") 'darwin)
                     "terminal-notifier"))
      (should (equal (with-programs '("osascript") 'darwin) "osascript"))
      (should-not (with-programs '("terminal-notifier") 'berkeley-unix)))))

(ert-deftest train-time-remind-test-notify-failure ()
  "A broken notifier still leaves the message in the echo area."
  (let ((train-time-notify-function (lambda (&rest _) (error "No notifier")))
        (inhibit-message t)
        (start (with-current-buffer (messages-buffer) (point-max))))
    (train-time-notify "T" "M" "id")
    (with-current-buffer (messages-buffer)
      (let ((logged (buffer-substring-no-properties start (point-max))))
        (should (string-match-p "^T: M$" logged))
        (should (string-match-p "could not notify: No notifier" logged))))))

(provide 'train-time-remind-test)
;;; train-time-remind-test.el ends here
