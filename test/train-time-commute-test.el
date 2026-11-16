;;; train-time-commute-test.el --- Tests for home and work trips -*- lexical-binding: t; -*-

;; Copyright (C) 2026  train-time.el contributors

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Journey search runs on hand-made trips; the buffer tests use the
;; feeds recorded at 16:12 on 2026-11-16 (gtfs, gtfs-ace, gtfs-nqrw).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'train-time-test)
(require 'train-time-commute)

;;;; Journey search

(defun train-time-commute-test--trip (id route &rest calls)
  "Return trip ID on ROUTE calling at CALLS, each (STATION TIME)."
  (list :trip-id id :route-id route :train-id nil :assigned t
        :calls (mapcar (lambda (c) (list (car c) (concat (car c) "N")
                                         (cadr c) (cadr c)))
                       calls)))

(defun train-time-commute-test--summary (journeys)
  "Return JOURNEYS as lists of (TRIP-ID DEPARTURE ARRIVAL) per leg."
  (mapcar (lambda (legs)
            (mapcar (lambda (leg)
                      (list (train-time-leg-trip-id leg)
                            (train-time-leg-departure leg)
                            (train-time-leg-arrival leg)))
                    legs))
          journeys))

(defconst train-time-commute-test--queens
  (list (train-time-commute-test--trip "7a" "7" '("701" 100) '("718" 1000) '("723" 1500))
        (train-time-commute-test--trip "W" "W" '("R09" 1100) '("R01" 1700))
        (train-time-commute-test--trip "N" "N" '("R09" 1300) '("R01" 1800)))
  "A 7 to Queensboro Plaza, and an N and a W from there to Astoria.")

(ert-deftest train-time-commute-test-direct ()
  (should (equal (train-time-commute-test--summary
                  (train-time-commute-journeys train-time-commute-test--queens
                                               '("701") '("723") 0))
                 '((("7a" 100 1500))))))

(ert-deftest train-time-commute-test-one-change ()
  ;; The 7 reaches Queensboro Plaza (718) at 1000; with three minutes
  ;; to change, the W at 1100 is missed and the N at 1300 is taken.
  (should (equal (train-time-commute-test--summary
                  (train-time-commute-journeys train-time-commute-test--queens
                                               '("701") '("R01") 0 180))
                 '((("7a" 100 1000) ("N" 1300 1800)))))
  ;; With one minute to change, the earlier-arriving W is caught.
  (should (equal (train-time-commute-test--summary
                  (train-time-commute-journeys train-time-commute-test--queens
                                               '("701") '("R01") 0 60))
                 '((("7a" 100 1000) ("W" 1100 1700))))))

(ert-deftest train-time-commute-test-change-names ()
  (let ((legs (car (train-time-commute-journeys train-time-commute-test--queens
                                                '("701") '("R01") 0 60))))
    (should (equal (mapcar (lambda (l) (train-time-station-id (train-time-leg-from l)))
                           legs)
                   '("701" "R09")))
    (should (equal (train-time-station-id (train-time-leg-to (car legs))) "718"))
    (should (equal (train-time-leg-destination (cadr legs)) "Astoria-Ditmars Blvd"))
    (should (equal (substring-no-properties (train-time-commute--via legs))
                   "Queensboro Plaza"))))

(ert-deftest train-time-commute-test-not-backwards ()
  (should-not (train-time-commute-journeys train-time-commute-test--queens
                                           '("723") '("701") 0)))

(ert-deftest train-time-commute-test-dominated ()
  ;; 7b leaves later and arrives earlier than 7a, which is dropped;
  ;; 7c leaves later still but arrives later, so it is kept.
  (let ((trips (list (train-time-commute-test--trip "7a" "7" '("701" 100) '("723" 1500))
                     (train-time-commute-test--trip "7b" "7X" '("701" 200) '("723" 1400))
                     (train-time-commute-test--trip "7c" "7" '("701" 300) '("723" 1600)))))
    (should (equal (mapcar (lambda (j) (train-time-leg-trip-id (car j)))
                           (train-time-commute-journeys trips '("701") '("723") 0))
                   '("7b" "7c")))))

(ert-deftest train-time-commute-test-direct-beats-equal-change ()
  ;; A change that leaves no later and arrives no earlier than a
  ;; direct train is not worth listing.
  (let ((trips (list (train-time-commute-test--trip "7" "7" '("701" 100) '("718" 1000))
                     (train-time-commute-test--trip "N" "N" '("R09" 1300) '("R01" 1800))
                     (train-time-commute-test--trip "x" "7" '("701" 100) '("R01" 1800)))))
    (should (equal (train-time-commute-test--summary
                    (train-time-commute-journeys trips '("701") '("R01") 0))
                   '((("x" 100 1800)))))))

(ert-deftest train-time-commute-test-departed ()
  (should-not (train-time-commute-journeys train-time-commute-test--queens
                                           '("701") '("723") 200))
  ;; A train leaving within the last 30 seconds is still listed.
  (should (train-time-commute-journeys train-time-commute-test--queens
                                       '("701") '("723") 120)))

(ert-deftest train-time-commute-test-mta-trips ()
  (let* ((trips (train-time-mta-trips
                 (train-time-gtfs-rt-decode
                  (train-time-test--fixture "gtfs" "20261116-1612"))))
         (seven (cl-find-if (lambda (trip)
                              (and (equal (plist-get trip :route-id) "7")
                                   (assoc "701" (plist-get trip :calls))))
                            trips)))
    (should (> (length trips) 50))
    (should seven)
    (should (stringp (plist-get seven :trip-id)))
    (dolist (call (plist-get seven :calls))
      (should (train-time-mta-station (car call)))
      (should (string-prefix-p (car call) (nth 1 call)))
      (should (or (nth 2 call) (nth 3 call))))
    (let ((times (mapcar #'train-time-commute--departure (plist-get seven :calls))))
      (should (equal times (sort (copy-sequence times) #'<))))))

;;;; Buffer

(defmacro train-time-commute-test--with (home work fetch &rest body)
  "Show trips from HOME to WORK fetched with FETCH, then run BODY."
  (declare (indent 3))
  `(let ((train-time-home ,home)
         (train-time-work ,work)
         (train-time-fetch-function ,fetch)
         (train-time-now-function (lambda () 1794863572))
         (train-time-auto-refresh nil)
         (inhibit-message t))
     (save-window-excursion
       (unwind-protect
           (progn (train-time-home-to-work) ,@body)
         (dolist (b (buffer-list))
           (when (string-prefix-p "*train-time" (buffer-name b))
             (kill-buffer b)))))))

(ert-deftest train-time-commute-test-buffer-direct ()
  (train-time-commute-test--with "701" "723" #'train-time-test--times-sq-fetch
    (should (equal (buffer-name) "*train-time: home → work*"))
    (should (derived-mode-p 'train-time-commute-mode))
    (should (= (length tabulated-list-entries) 7))
    (let ((text (train-time-test--text)))
      (should (string-match-p "^ 16:18 +5 min +7 +direct +16:51 +33 min Tracked" text))
      (should (string-match-p "From Flushing-Main St (home) to Grand Central-42 St" text))
      (should (string-match-p "Feed time 16:12:51" text))
      (should (string-match-p "not a promise of the fastest trip" text)))))

(ert-deftest train-time-commute-test-buffer-change ()
  (train-time-commute-test--with "701" "R01" #'train-time-test--times-sq-fetch
    (should (= (length tabulated-list-entries) 4))
    (goto-char (point-min))
    (should (string-match-p "^ 16:26 .*7 *→ *W +Queensboro Plaza +17:05 +38 min Scheduled"
                            (buffer-substring-no-properties
                             (line-beginning-position) (line-end-position))))
    (let ((described nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (_fmt text) (setq described text))))
        (train-time-commute-describe))
      (should (string-match-p
               "\\`7 toward Manhattan (to 34 St-Hudson Yards)  16:26 Flushing-Main St → 16:48 Queensboro Plaza  (scheduled)\nW "
               described)))))

(ert-deftest train-time-commute-test-reverse ()
  (train-time-commute-test--with "701" "R01" #'train-time-test--times-sq-fetch
    (train-time-commute-reverse)
    (should (equal (buffer-name) "*train-time: work → home*"))
    (should (= (length tabulated-list-entries) 6))
    (should (string-match-p "From Astoria-Ditmars Blvd (work) to Flushing-Main St (home)"
                            (train-time-test--text)))
    (train-time-commute-reverse)
    (should (equal (buffer-name) "*train-time: home → work*"))))

(ert-deftest train-time-commute-test-board ()
  (train-time-commute-test--with "701" "R01" #'train-time-test--times-sq-fetch
    (goto-char (point-min))
    (train-time-commute-board)
    (should (derived-mode-p 'train-time-mode))
    (should (equal (buffer-name) "*train-time: Flushing-Main St*"))))

(ert-deftest train-time-commute-test-failed-feed-keeps-trips ()
  (train-time-commute-test--with "701" "723" #'train-time-test--times-sq-fetch
    (let ((train-time-fetch-function
           (lambda (_url callback) (funcall callback '(:error "HTTP 503")))))
      (revert-buffer))
    (should (= (length tabulated-list-entries) 7))
    (should (string-match-p "Last update failed: .*HTTP 503" (train-time-test--text)))
    (revert-buffer)
    (should-not (string-match-p "Last update failed" (train-time-test--text)))))

(ert-deftest train-time-commute-test-no-trips ()
  (train-time-commute-test--with "701" "723"
      (lambda (_url callback) (funcall callback '(:error "HTTP 503")))
    (should-not tabulated-list-entries)
    (let ((text (train-time-test--text)))
      (should (string-match-p "No trips found" text))
      (should (string-match-p "Last update failed" text)))))

(ert-deftest train-time-commute-test-same-complex ()
  (should-error (train-time-commute-test--with "725" "A27" #'ignore)
                :type 'user-error))

;;;; Home and work

(ert-deftest train-time-commute-test-asks-for-unset-place ()
  (let ((saved nil)
        (prompt nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (p &rest _)
                 (setq prompt p)
                 (train-time--station-candidate (train-time-mta-station "723"))))
              ((symbol-function 'customize-save-variable)
               (lambda (var value &rest _) (push (cons var value) saved) (set var value))))
      (train-time-commute-test--with "701" nil #'train-time-test--times-sq-fetch
        (should (equal train-time-work "723"))
        (should (equal (length tabulated-list-entries) 7))))
    (should (string-prefix-p "Work station" prompt))
    (should (equal saved '((train-time-work . "723"))))))

(ert-deftest train-time-commute-test-set-home ()
  (let ((train-time-home nil)
        (saved nil))
    (cl-letf (((symbol-function 'customize-save-variable)
               (lambda (var value &rest _) (push (cons var value) saved) (set var value))))
      (let ((inhibit-message t))
        (train-time-set-home (train-time-mta-station "R01"))))
    (should (equal train-time-home "R01"))
    (should (equal saved '((train-time-home . "R01"))))))

(ert-deftest train-time-commute-test-set-work-unsaved ()
  (let ((train-time-work nil)
        (shown nil))
    (cl-letf (((symbol-function 'customize-save-variable)
               (lambda (&rest _) (error "Cannot save customizations")))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq shown (apply #'format fmt args)))))
      (train-time-set-work (train-time-mta-station "723")))
    (should (equal train-time-work "723"))
    (should (string-match-p "for this session" shown))))

(provide 'train-time-commute-test)
;;; train-time-commute-test.el ends here
