;;; train-time-alerts-test.el --- Tests for alerts and outages -*- lexical-binding: t; -*-

;; Copyright (C) 2026  train-time.el contributors

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Uses MTA's subway alerts feed recorded at 16:33 on 2026-11-16 and
;; the elevator and escalator outages recorded at 16:38.  The
;; equipment list fixture is the 16:38 download cut down to the
;; Flushing-Main St, Grand Central and Times Sq complexes.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'train-time-test)
(require 'train-time-alerts)
(require 'train-time-commute)

(defconst train-time-alerts-test--now 1794864682
  "Header time of the recorded alerts feed, 16:31:22 EST.")

(defun train-time-alerts-test--file (name)
  "Return the contents of fixture NAME as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally (expand-file-name (concat "fixtures/" name)
                                                      train-time-test--dir))
    (buffer-string)))

(defvar train-time-alerts-test--requests nil
  "URLs fetched by `train-time-alerts-test--fetch', most recent first.")

(defun train-time-alerts-test--fetch (url callback)
  "Answer URL from the recordings, using CALLBACK."
  (push url train-time-alerts-test--requests)
  (let ((feed (replace-regexp-in-string "\\`.*%2F" "" url)))
    (funcall callback
             (list :data
                   (train-time-alerts-test--file
                    (pcase feed
                      ("subway-alerts" "mta-nyct-subway-alerts-20261116-1633.pb")
                      ("nyct_ene.json" "mta-nyct-ene-20261116-1638.json")
                      ("nyct_ene_equipments.json"
                       "mta-nyct-ene-equipments-20261116-1638-subset.json")
                      (_ (format "mta-nyct-%s-20261116-1612.pb" feed))))))))

(defmacro train-time-alerts-test--with (&rest body)
  "Run BODY with recorded feeds, an empty alerts cache and alerts shown."
  (declare (indent 0))
  `(let ((train-time-fetch-function #'train-time-alerts-test--fetch)
         (train-time-now-function (lambda () train-time-alerts-test--now))
         (train-time-show-alerts t)
         (train-time-auto-refresh nil)
         (train-time-show-terminating nil)
         (train-time-alerts--cache nil)
         (train-time-alerts--waiting nil)
         (train-time-alerts-test--requests nil)
         (inhibit-message t))
     (save-window-excursion
       (unwind-protect (progn ,@body)
         (dolist (b (buffer-list))
           (when (string-prefix-p "*train-time" (buffer-name b))
             (kill-buffer b)))))))

(defvar train-time-alerts-test--alerts nil)
(defun train-time-alerts-test--alerts ()
  "Return the alerts in the recorded feed (cached)."
  (or train-time-alerts-test--alerts
      (setq train-time-alerts-test--alerts
            (train-time-mta-alerts
             (train-time-gtfs-rt-decode
              (train-time-alerts-test--file "mta-nyct-subway-alerts-20261116-1633.pb"))))))

(defun train-time-alerts-test--alert (id)
  "Return the recorded alert with ID."
  (cl-find id (train-time-alerts-test--alerts) :key #'train-time-alert-id :test #'equal))

;;;; Decoding

(ert-deftest train-time-alerts-test-decode ()
  (let ((feed (train-time-gtfs-rt-decode
               (train-time-alerts-test--file "mta-nyct-subway-alerts-20261116-1633.pb"))))
    (should (= (train-time-gtfs-rt-feed-time feed) train-time-alerts-test--now))
    (should (= (length (train-time-gtfs-rt-alerts feed)) 200))
    (should-not (train-time-gtfs-rt-trip-updates feed))))

(ert-deftest train-time-alerts-test-translation ()
  (let ((ts '(:translation ((:text "<p>Hi</p>" :language "en-html")
                            (:text "Hi" :language "en")
                            (:text "Bonjour" :language "fr")))))
    (should (equal (train-time-gtfs-rt-text ts) "Hi"))
    (should (equal (train-time-gtfs-rt-text ts "fr") "Bonjour"))
    (should (equal (train-time-gtfs-rt-text '(:translation ((:text "Only"))) "ko") "Only"))
    (should-not (train-time-gtfs-rt-text nil))))

(ert-deftest train-time-alerts-test-delay ()
  (let ((alert (train-time-alerts-test--alert "lmm:alert:276647:26")))
    (should (equal (train-time-alert-type alert) "Delays"))
    (should-not (train-time-alert-planned alert))
    (should (equal (train-time-alert-routes alert) '("R")))
    (should (= (train-time-alert-priority alert) 26))
    (should (equal (train-time-alert-periods alert) '((1794862612 . 1794864900))))
    (should (string-prefix-p "Bay Ridge-95 St-bound [R] trains are running with delays"
                             (train-time-alert-header alert)))
    (should (string-prefix-p "Downtown [R] trains have resumed"
                             (train-time-alert-description alert)))
    (should (train-time-alert-active-p alert train-time-alerts-test--now))
    (should-not (train-time-alert-active-p alert 1794864900))))

(ert-deftest train-time-alerts-test-planned ()
  (let ((alert (train-time-alerts-test--alert "lmm:planned_work:31871")))
    (should (train-time-alert-planned alert))
    (should (equal (train-time-alert-type alert) "Planned - Express to Local"))
    (should (equal (train-time-alert-routes alert) '("7" "7X")))
    (should (equal (train-time-alert-period-text alert) "Through Spring 2027"))
    (should (train-time-alert-stations alert))
    (should (cl-every #'train-time-mta-station (train-time-alert-stations alert)))))

(ert-deftest train-time-alerts-test-periods ()
  (let ((always (make-train-time-alert))
        (open (make-train-time-alert :periods '((100 . nil))))
        (later (make-train-time-alert :periods '((100 . 200) (300 . 400)))))
    (should (train-time-alert-active-p always 0))
    (should (train-time-alert-active-p open 1000000))
    (should-not (train-time-alert-active-p open 50))
    (should (train-time-alert-active-p later 150))
    (should-not (train-time-alert-active-p later 250))
    (should (= (train-time-alert-next-start later 250) 300))
    (should-not (train-time-alert-next-start later 300))))

(ert-deftest train-time-alerts-test-affects ()
  (let ((seven (make-train-time-alert :routes '("7X")))
        (shuttle (make-train-time-alert :routes '("FS"))))
    (should (train-time-mta-alert-affects-p seven '("7")))
    (should (train-time-mta-alert-affects-p seven '("A" "7X")))
    (should-not (train-time-mta-alert-affects-p seven '("6")))
    ;; Three shuttles are all "S", but they are different lines.
    (should-not (train-time-mta-alert-affects-p shuttle '("GS")))
    (should (train-time-mta-alert-affects-p shuttle '("FS")))))

;;;; Selecting

(ert-deftest train-time-alerts-test-select ()
  (train-time-alerts-test--with
    (train-time-alerts--get 'alerts #'ignore)
    (pcase-let ((`(,current . ,planned)
                 (train-time-alerts-select '("7" "7X") train-time-alerts-test--now)))
      (should (equal (mapcar #'train-time-alert-id current)
                     '("lmm:planned_work:31498" "lmm:planned_work:31871")))
      (should (= (length planned) 8))
      (let ((starts (mapcar (lambda (a) (train-time-alert-next-start
                                         a train-time-alerts-test--now))
                            planned)))
        (should (equal starts (sort (copy-sequence starts) #'<)))))
    ;; The most severe alerts in effect come first, latest first.
    (let ((current (car (train-time-alerts-select nil train-time-alerts-test--now))))
      (should (equal (mapcar #'train-time-alert-priority current)
                     (sort (mapcar #'train-time-alert-priority current) #'>)))
      (should (equal (mapcar #'train-time-alert-id (seq-take current 2))
                     '("lmm:alert:276652:26" "lmm:alert:276651:26"))))))

(ert-deftest train-time-alerts-test-station-notices ()
  (train-time-alerts-test--with
    (train-time-alerts--get 'alerts #'ignore)
    (let* ((notice "lmm:planned_work:33366")
           (ids (lambda (stations)
                  (mapcar #'train-time-alert-id
                          (car (train-time-alerts-select '("E") train-time-alerts-test--now
                                                         stations))))))
      ;; An entrance notice for 5 Av/53 St matters only there.
      (should (equal (train-time-alert-type (train-time-alerts-test--alert notice))
                     "Station Notice"))
      (should (member notice (funcall ids nil)))
      (should-not (member notice (funcall ids '("A27"))))
      (should (member notice (funcall ids (train-time-alert-stations
                                           (train-time-alerts-test--alert notice))))))))

;;;; Outages

(ert-deftest train-time-alerts-test-ene-time ()
  (let ((fmt (lambda (time) (format-time-string "%F %T %Z" time train-time-mta-time-zone))))
    (should (equal (funcall fmt (train-time-mta--ene-time "11/30/2026 11:59:00 PM"))
                   "2026-11-30 23:59:00 EST"))
    (should (equal (funcall fmt (train-time-mta--ene-time "11/17/2026 12:00:00 AM"))
                   "2026-11-17 00:00:00 EST"))
    (should (equal (funcall fmt (train-time-mta--ene-time "07/04/2026 12:30:00 PM"))
                   "2026-07-04 12:30:00 EDT"))
    (should-not (train-time-mta--ene-time "soon"))
    (should-not (train-time-mta--ene-time nil))))

(ert-deftest train-time-alerts-test-outages ()
  (train-time-alerts-test--with
    (train-time-alerts-fetch #'ignore)
    (let ((flushing (train-time-alerts-outages '("447")))
          (times-sq (train-time-alerts-outages '("611"))))
      (should (equal (mapcar (lambda (o) (plist-get o :equipment)) flushing) '("EL414")))
      (let ((el414 (car flushing)))
        (should (eq (plist-get el414 :kind) 'elevator))
        (should (plist-get el414 :ada))
        (should (equal (plist-get el414 :reason) "Capital Replacement"))
        (should (equal (format-time-string "%F %R" (plist-get el414 :until)
                                           train-time-mta-time-zone)
                       "2026-11-30 23:59")))
      ;; Elevators first; upcoming outages are left out.
      (should (equal (mapcar (lambda (o) (plist-get o :equipment)) times-sq)
                     '("EL290X" "ES607X")))
      (should-not (train-time-alerts-outages '("610"))))))

;;;; Downloads

(ert-deftest train-time-alerts-test-cache ()
  (train-time-alerts-test--with
    (let ((calls 0))
      (train-time-alerts-fetch (lambda () (cl-incf calls)))
      (should (= calls 1))
      (should (= (length train-time-alerts-test--requests) 3))
      ;; Recent downloads are reused, except with FORCE.
      (train-time-alerts-fetch (lambda () (cl-incf calls)))
      (should (= (length train-time-alerts-test--requests) 3))
      (train-time-alerts-fetch (lambda () (cl-incf calls)) t)
      (should (= calls 3))
      (should (= (length train-time-alerts-test--requests) 5))
      (should-not (cl-some (lambda (url) (string-match-p "equipments" url))
                           (seq-take train-time-alerts-test--requests 2)))
      (let ((train-time-now-function (lambda () (+ train-time-alerts-test--now 121))))
        (train-time-alerts-fetch #'ignore))
      (should (= (length train-time-alerts-test--requests) 7)))))

(ert-deftest train-time-alerts-test-failure-keeps-alerts ()
  (train-time-alerts-test--with
    (train-time-alerts-fetch #'ignore)
    (let ((before (length (train-time-alerts--value 'alerts)))
          (train-time-fetch-function
           (lambda (_url callback) (funcall callback '(:error "HTTP status 503")))))
      (train-time-alerts-fetch #'ignore t)
      (should (= (length (train-time-alerts--value 'alerts)) before))
      (should (equal (train-time-alerts--error 'alerts) "HTTP status 503")))
    (train-time-alerts-fetch #'ignore t)
    (should-not (train-time-alerts--error 'alerts))))

(ert-deftest train-time-alerts-test-malformed ()
  (train-time-alerts-test--with
    (let ((train-time-fetch-function
           (lambda (_url callback) (funcall callback '(:data "{not json")))))
      (train-time-alerts-fetch #'ignore)
      (should (string-prefix-p "Could not read outages"
                               (train-time-alerts--error 'outages)))
      (should-not (train-time-alerts-outages '("447"))))))

(ert-deftest train-time-alerts-test-shared-request ()
  ;; Callers while a download is in flight wait for it.
  (train-time-alerts-test--with
    (let* ((pending nil)
           (train-time-fetch-function (lambda (url cb) (push (cons url cb) pending)))
           (calls 0))
      (train-time-alerts--get 'alerts (lambda (_) (cl-incf calls)))
      (train-time-alerts--get 'alerts (lambda (_) (cl-incf calls)))
      (should (= (length pending) 1))
      (train-time-alerts-test--fetch (car (car pending)) (cdr (car pending)))
      (should (= calls 2))
      (should (train-time-alerts--value 'alerts)))))

;;;; Formatting

(ert-deftest train-time-alerts-test-bulletize ()
  (let ((text (train-time-alerts--bulletize
               "Take the [7X] or [S] to [accessibility icon] [SIR] or [shuttle bus icon]")))
    (should (equal (substring-no-properties text)
                   "Take the  <7>  or  S  to ♿  SIR  or 🚌"))
    (should (get-text-property (string-match "<7>" text) 'face text))))

(ert-deftest train-time-alerts-test-fill ()
  (should (equal (train-time-alerts--fill "one two three four\n\nfive" 2 12)
                 "  one two\n  three four\n\n  five")))

;;;; Boards and buffers

(ert-deftest train-time-alerts-test-board ()
  (train-time-alerts-test--with
    (train-time (train-time-mta-station "701"))
    (let ((text (train-time-test--text)))
      (should (string-match-p "Service alerts in effect (2):" text))
      (should (string-match-p "Planned - Stops Skipped: In Queens, Flushing-bound +7 +skips"
                              text))
      ;; Boarding changes at other stations are left out here.
      (should (string-match-p "5 planned service changes coming up" text))
      (should (string-match-p "Elevator EL414 out of service" text))
      (should (string-match-p "expected back Nov 30 23:59: capital replacement" text)))
    (should (string-match-p "⚠2" mode-line-process))))

(ert-deftest train-time-alerts-test-board-complex ()
  (train-time-alerts-test--with
    (train-time (train-time-mta-station "725"))
    (let ((text (train-time-test--text)))
      (should (string-match-p "Service alerts in effect (6):" text))
      (should (string-match-p "R +Delays: Bay Ridge" text))
      (should-not (string-match-p "5 Av/53 St" text))
      (should (string-match-p "Elevator EL290X" text))
      (should (string-match-p "Escalator ES607X" text)))
    (should (string-match-p "⚠6" mode-line-process))))

(ert-deftest train-time-alerts-test-board-brief ()
  (train-time-alerts-test--with
    (let ((train-time-compact-width 200)
          (train-time-touch-bar t))
      (train-time (train-time-mta-station "725"))
      (let ((text (train-time-test--text))
            (width (window-body-width)))
        (should (string-match-p "Service alerts in effect (6):" text))
        ;; One line an alert, cut to the window, and counts for the rest.
        (should (string-match-p "^ +R +Delays: Bay Ridge" text))
        (should-not (string-match-p "Elevator EL290X" text))
        (should (string-match-p "^84 planned · 2 outages ♿ · All ▸$" text))
        (dolist (line (split-string text "\n"))
          (should (< (string-width line) width))))
      ;; The header line keeps the data's age and the alerts in view.
      (should (string-match-p "STALE · ⚠6" header-line-format))
      (let ((train-time-alerts-footer 'full))
        (train-time--render)
        (should (string-match-p "Elevator EL290X" (train-time-test--text)))))))

(ert-deftest train-time-alerts-test-board-disabled ()
  (train-time-alerts-test--with
    (let ((train-time-show-alerts nil))
      (train-time (train-time-mta-station "701"))
      (should-not (string-match-p "alerts" (train-time-test--text)))
      (should-not (cl-some (lambda (url) (string-match-p "alerts\\|ene" url))
                           train-time-alerts-test--requests)))))

(ert-deftest train-time-alerts-test-board-unavailable ()
  (train-time-alerts-test--with
    (let ((train-time-fetch-function
           (lambda (url callback)
             (if (string-match-p "alerts\\|ene" url)
                 (funcall callback '(:error "Request timed out"))
               (train-time-alerts-test--fetch url callback)))))
      (train-time (train-time-mta-station "701"))
      (let ((text (train-time-test--text)))
        (should (string-match-p "Alerts unavailable: Request timed out" text))
        (should (string-match-p "34 St-Hudson Yards" text))))))

(ert-deftest train-time-alerts-test-commute ()
  (train-time-alerts-test--with
    (let ((train-time-home "701")
          (train-time-work "R01"))
      (train-time-home-to-work)
      (let ((text (train-time-test--text)))
        (should (string-match-p "Service alerts in effect" text))
        (should (string-match-p "Elevator EL414" text)))
      (should (member "W" (train-time-commute-routes))))))

(ert-deftest train-time-alerts-test-buffer ()
  (train-time-alerts-test--with
    (train-time-alerts '("7"))
    (should (equal (buffer-name) "*train-time alerts: 7*"))
    (should (derived-mode-p 'train-time-alerts-mode))
    (let ((text (train-time-test--text)))
      (should (string-match-p "In effect now (2)" text))
      (should (string-match-p "Planned (8)" text))
      (should (string-match-p "Through Spring 2027" text))
      (should (string-match-p "Nov 17, Tuesday, 12:45 AM to 5:00 AM" text))
      (should-not (string-match-p "What's happening" text)))
    ;; Point starts on the first alert.
    (should (equal (train-time-alert-id (get-text-property (point) 'train-time-alert))
                   "lmm:planned_work:31498"))
    (train-time-alerts-next)
    (should (equal (train-time-alert-id (get-text-property (point) 'train-time-alert))
                   "lmm:planned_work:31871"))
    (train-time-alerts-toggle)
    (should (equal (train-time-alert-id (get-text-property (point) 'train-time-alert))
                   "lmm:planned_work:31871"))
    (should (string-match-p "We're making structural improvements"
                            (train-time-test--text)))
    (train-time-alerts-toggle)
    (should-not (string-match-p "What's happening" (train-time-test--text)))
    (train-time-alerts-toggle-all)
    (should (string-match-p "What's happening" (train-time-test--text)))
    (train-time-alerts-toggle-all)
    (should-not (string-match-p "What's happening" (train-time-test--text)))
    (train-time-alerts-previous)
    (should (equal (train-time-alert-id (get-text-property (point) 'train-time-alert))
                   "lmm:planned_work:31498"))
    (should-error (train-time-alerts-previous) :type 'user-error)))

(ert-deftest train-time-alerts-test-keys ()
  (should (eq (lookup-key train-time-alerts-mode-map (kbd "C-j")) #'train-time-alerts-next))
  (should (eq (lookup-key train-time-alerts-mode-map (kbd "C-k"))
              #'train-time-alerts-previous))
  (let ((evil (cl-loop for (key cmd) on train-time-alerts--evil-bindings by #'cddr
                       collect (cons key cmd))))
    (should (eq (cdr (assoc (kbd "C-j") evil)) 'train-time-alerts-next))
    (should (eq (cdr (assoc (kbd "C-k") evil)) 'train-time-alerts-previous))
    (should (eq (cdr (assoc (kbd "RET") evil)) 'train-time-alerts-toggle))
    (should (eq (cdr (assoc (kbd "<return>") evil)) 'train-time-alerts-toggle))))

(defun train-time-alerts-test--heading-row ()
  "Return the alert at point's ID and its heading's row in the window."
  (list (train-time-alert-id (get-text-property (point) 'train-time-alert))
        (train-time-alerts--row (selected-window)
                                (train-time-alerts--start-at (point)))))

(ert-deftest train-time-alerts-test-toggle-keeps-place ()
  "Showing and hiding details leaves the alert where it was on screen."
  (train-time-alerts-test--with
    (train-time-alerts '("7"))
    (delete-other-windows)
    (train-time-alerts-next 3)
    (set-window-start nil (save-excursion (forward-line -4) (point)))
    (let ((before (train-time-alerts-test--heading-row))
          (start (window-start)))
      (should (equal (cadr before) 4))
      (train-time-alerts-toggle)
      (should (member (car before) train-time-alerts--expanded))
      (should (equal (train-time-alerts-test--heading-row) before))
      (should (= (window-start) start))
      ;; Hiding the details point is in moves it to the heading.
      (goto-char (1- (next-single-property-change (point) 'train-time-alert)))
      (train-time-alerts-toggle)
      (should-not (member (car before) train-time-alerts--expanded))
      (should (equal (train-time-alerts-test--heading-row) before))
      (should (= (point) (train-time-alerts--start-at (point))))
      (should (= (window-start) start)))))

(ert-deftest train-time-alerts-test-from-board ()
  (train-time-alerts-test--with
    (train-time (train-time-mta-station "723"))
    (call-interactively #'train-time-alerts)
    (should (equal (buffer-name) "*train-time alerts: 4 5 6 7 S*"))
    (should (string-match-p "In effect now" (train-time-test--text)))))

(ert-deftest train-time-alerts-test-read-lines ()
  (train-time-alerts-test--with
    (cl-letf (((symbol-function 'completing-read-multiple)
               (lambda (&rest _) '("A" "S"))))
      (call-interactively #'train-time-alerts))
    (should (equal train-time-alerts--routes '("A" "GS" "FS" "H")))
    (should (equal (buffer-name) "*train-time alerts: A S*"))
    (cl-letf (((symbol-function 'completing-read-multiple) (lambda (&rest _) nil)))
      (call-interactively #'train-time-alerts))
    (should (equal (buffer-name) "*train-time alerts*"))
    (should (string-match-p "Service alerts for every line" (train-time-test--text)))))

(provide 'train-time-alerts-test)
;;; train-time-alerts-test.el ends here
