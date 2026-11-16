;;; train-time-test.el --- Tests for train-time -*- lexical-binding: t; -*-

;; Copyright (C) 2026  train-time.el contributors

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Deterministic tests; no network access.  The fixture
;; fixtures/mta-nyct-gtfs-20261116.pb is an unmodified response from
;; https://api-endpoint.mta.info/Dataservice/mtagtfsfeeds/nyct%2Fgtfs
;; with header timestamp 1794859119 (2026-11-16 14:58:39 EST).
;; fixtures/mta-nyct-gtfs-{bdfm,nqrw}-20261116.pb are unmodified
;; responses from the nyct%2Fgtfs-bdfm and nyct%2Fgtfs-nqrw feeds,
;; with header timestamps 1794861313 and 1794861317 (15:35 EST).
;; fixtures/mta-nyct-{gtfs,gtfs-ace,gtfs-nqrw}-20261116-1612.pb were
;; fetched together for the Times Sq-42 St complex; their header
;; timestamps are 1794863571, 1794863562 and 1794863572 (16:12 EST).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'train-time)

;; Most tests answer every URL with a trip feed; alerts and outages
;; have their own tests in train-time-alerts-test.el.
(setq train-time-show-alerts nil)

;; Batch Emacs is a text terminal, where the touch bar is on by default.
(setq train-time-touch-bar nil)

(defconst train-time-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defconst train-time-test--fixture-time 1794859119)

(defconst train-time-test--coney-time 1794861317)

(defun train-time-test--fixture (&optional feed recorded)
  "Return the recorded MTA FEED (\"gtfs\" by default) as a unibyte string.
RECORDED names the recording, \"20261116\" by default."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally
     (expand-file-name (format "fixtures/mta-nyct-%s-%s.pb"
                               (or feed "gtfs") (or recorded "20261116"))
                       train-time-test--dir))
    (buffer-string)))

(defvar train-time-test--feed nil)
(defun train-time-test--feed ()
  "Return the decoded fixture feed (cached)."
  (or train-time-test--feed
      (setq train-time-test--feed
            (train-time-gtfs-rt-decode (train-time-test--fixture)))))

(defun train-time-test--bytes (&rest bytes)
  "Return a unibyte string of BYTES."
  (apply #'unibyte-string bytes))

(defconst train-time-test--schema
  '((M (1 :u uint64) (2 :i int32) (3 :s string) (4 :z sint32)
       (5 :f fixed32) (6 :d double) (7 :r uint32 repeated)
       (8 :sub M) (9 :b bool) (10 :l int64))))

(defun train-time-test--decode (&rest bytes)
  (train-time-pb-decode train-time-test--schema 'M
                        (apply #'train-time-test--bytes bytes)))

;;;; Protobuf wire format

(ert-deftest train-time-test-pb-varint ()
  (should (equal (train-time-test--decode #x08 #x96 #x01) '(:u 150)))
  ;; 2^63 needs a 10-byte varint; Emacs bignums keep it exact.
  (should (equal (train-time-test--decode
                  #x08 #x80 #x80 #x80 #x80 #x80 #x80 #x80 #x80 #x80 #x01)
                 (list :u (expt 2 63)))))

(ert-deftest train-time-test-pb-negative-int32 ()
  ;; -1 as int32 is sign-extended to ten bytes on the wire.
  (should (equal (train-time-test--decode
                  #x10 #xff #xff #xff #xff #xff #xff #xff #xff #xff #x01)
                 '(:i -1)))
  (should (equal (train-time-test--decode
                  #x50 #xfe #xff #xff #xff #xff #xff #xff #xff #xff #x01)
                 '(:l -2))))

(ert-deftest train-time-test-pb-zigzag-and-fixed ()
  (should (equal (train-time-test--decode #x20 #x03) '(:z -2)))
  (should (equal (train-time-test--decode #x2d #x01 #x02 #x00 #x00) '(:f 513)))
  (should (equal (train-time-test--decode
                  #x31 #x00 #x00 #x00 #x00 #x00 #x00 #xf8 #x3f)
                 '(:d 1.5))))

(ert-deftest train-time-test-pb-utf8-string ()
  ;; Station names in other scripts must survive decoding.
  (let* ((name "新宿 / 서울역")
         (bytes (encode-coding-string name 'utf-8))
         (data (concat (unibyte-string #x1a (length bytes)) bytes)))
    (should (equal (train-time-pb-decode train-time-test--schema 'M data)
                   (list :s name)))))

(ert-deftest train-time-test-pb-repeated-packed-and-nested ()
  (should (equal (train-time-test--decode #x38 #x01 #x38 #x02 #x3a #x02 #x03 #x04)
                 '(:r (1 2 3 4))))
  (should (equal (train-time-test--decode #x42 #x02 #x08 #x07 #x48 #x01)
                 '(:sub (:u 7) :b t))))

(ert-deftest train-time-test-pb-unknown-fields-skipped ()
  ;; Field 15 varint, field 16 length-delimited, field 17 fixed32.
  (should (equal (train-time-test--decode
                  #x78 #x05 #x82 #x01 #x02 #x61 #x62 #x8d #x01 1 2 3 4 #x08 #x01)
                 '(:u 1))))

(ert-deftest train-time-test-pb-malformed ()
  (should-error (train-time-test--decode #x08 #x96) :type 'train-time-pb-error)
  (should-error (train-time-test--decode #x1a #x05 #x61) :type 'train-time-pb-error)
  (should-error (train-time-test--decode #x2d #x01) :type 'train-time-pb-error)
  (should-error (train-time-test--decode #x0b) :type 'train-time-pb-error)
  (should-error (train-time-gtfs-rt-decode "<html>Service Unavailable</html>")
                :type 'train-time-pb-error))

;;;; GTFS-Realtime fixture

(ert-deftest train-time-test-gtfs-rt-header ()
  (let ((feed (train-time-test--feed)))
    (should (equal (plist-get (plist-get feed :header) :gtfs-realtime-version) "1.0"))
    (should (= (train-time-gtfs-rt-feed-time feed) train-time-test--fixture-time))
    (should (= (length (plist-get feed :entity)) 509))))

(ert-deftest train-time-test-gtfs-rt-event-time ()
  (should (equal (train-time-gtfs-rt-event-time '(:arrival (:time 10) :departure (:time 20)))
                 '(10 . arrival)))
  (should (equal (train-time-gtfs-rt-event-time '(:departure (:time 20)))
                 '(20 . departure)))
  ;; A zero time is the proto2 default, not a real prediction.
  (should (null (train-time-gtfs-rt-event-time '(:arrival (:time 0)))))
  (should (null (train-time-gtfs-rt-event-time '(:stop-id "701S")))))

;;;; MTA backend

(ert-deftest train-time-test-mta-stations ()
  ;; Every station in MTA's Stations.csv, subway and SIR.
  (should (= (length train-time-mta-stations) 496))
  (let ((flushing (train-time-mta-station "701")))
    (should (equal (train-time-station-name flushing) "Flushing-Main St"))
    (should (equal (train-time-station-display-name flushing) "Flushing-Main St"))
    (should (equal (train-time-station-routes flushing) '("7" "7X")))
    (should (null (alist-get 'north (train-time-station-directions flushing))))
    (should (equal (alist-get 'south (train-time-station-directions flushing))
                   "Manhattan")))
  (let ((dutch-kills (train-time-mta-station "R08")))
    (should (equal (train-time-station-name dutch-kills) "39 Av-Dutch Kills"))
    (should (equal (train-time-station-routes dutch-kills) '("N" "W"))))
  (should (equal (train-time-station-routes (train-time-mta-station "S31")) '("SI")))
  (should (null (train-time-mta-station "999"))))

(ert-deftest train-time-test-mta-station-labels ()
  ;; Names shared by several stations get their daytime routes.
  (should (equal (train-time-station-display-name (train-time-mta-station "725"))
                 "Times Sq-42 St (7)"))
  (should (equal (train-time-station-display-name (train-time-mta-station "R16"))
                 "Times Sq-42 St (N Q R W)"))
  (should (= (length (delete-dups (mapcar #'train-time-station-display-name
                                          train-time-mta-stations)))
             496))
  (should (equal (train-time--station-candidate (train-time-mta-station "701"))
                 "Flushing-Main St (7)"))
  (should (equal (train-time--station-candidate (train-time-mta-station "725"))
                 "Times Sq-42 St (7)")))

(ert-deftest train-time-test-mta-complex ()
  (let* ((times-sq (train-time-mta-station "725"))
         (complex (train-time-mta-complex times-sq)))
    (should (eq (car complex) times-sq))
    (should (equal (sort (mapcar #'train-time-station-id complex) #'string<)
                   '("127" "725" "902" "A27" "R16"))))
  (should (equal (train-time-mta-complex (train-time-mta-station "701"))
                 (list (train-time-mta-station "701")))))

(ert-deftest train-time-test-mta-complex-name ()
  (let ((name (lambda (id) (train-time-mta-complex-name (train-time-mta-station id)))))
    (should (equal (funcall name "725") "Times Sq-42 St"))
    (should (equal (funcall name "A27") "Times Sq-42 St"))
    (should (equal (funcall name "723") "Grand Central-42 St"))
    ;; The G's Fulton St in Brooklyn shares the name.
    (should (equal (funcall name "A38") "Fulton St (A C J Z 2 3 4 5)"))
    (should (equal (funcall name "701") "Flushing-Main St")))
  (should (equal (train-time-mta-route-names
                  (train-time-mta-complex (train-time-mta-station "723")))
                 '("S" "4" "5" "6" "7"))))

(ert-deftest train-time-test-mta-route-classification ()
  (should (equal (train-time-mta--route "7") '("7" . local)))
  (should (equal (train-time-mta--route "7X") '("7" . express)))
  (should (equal (train-time-mta--route "6X") '("6" . express)))
  (should (equal (train-time-mta--route "FX") '("F" . express)))
  ;; No <A> exists, so the A is neither local nor express.
  (should (equal (train-time-mta--route "A") '("A" . nil)))
  (should (equal (train-time-mta--route "GS") '("S" . nil)))
  (should (equal (train-time-mta--route "FS") '("S" . nil)))
  (should (equal (train-time-mta--route "SI") '("SIR" . nil)))
  ;; Routes missing from the static data pass through unchanged.
  (should (equal (train-time-mta--route "SS") '("SS" . nil)))
  (should (equal (train-time-mta-route-bullet "7X") "<7>"))
  (should (equal (train-time-mta-route-color "N") "#F6BC26"))
  (should (equal (train-time-mta-route-text-color "N") "#000000"))
  (should (equal (train-time-mta--parent-stop "701S") "701"))
  (should (eq (train-time-mta--direction "701N") 'north)))

(ert-deftest train-time-test-mta-feeds ()
  (let ((train-time-mta-feed-base-url "https://example.test/"))
    (should (equal (train-time-mta-feed-urls (train-time-mta-station "701"))
                   '("https://example.test/nyct%2Fgtfs")))
    (should (equal (train-time-mta-feed-urls (train-time-mta-station "D43"))
                   '("https://example.test/nyct%2Fgtfs-bdfm"
                     "https://example.test/nyct%2Fgtfs-nqrw")))
    (should (equal (train-time-mta-feed-urls (train-time-mta-station "S31"))
                   '("https://example.test/nyct%2Fgtfs-si"))))
  ;; Every scheduled route is carried by some feed.
  (dolist (route train-time-mta-data-routes)
    (should (cl-find-if (lambda (f) (member (car route) (cdr f)))
                        train-time-mta-feeds))))

(ert-deftest train-time-test-mta-lines ()
  (should (= (length (train-time-mta-line-ids)) 26))
  (should (equal (train-time-mta-line-ids (train-time-mta-station "701")) '("7")))
  (should (equal (train-time-mta-line-ids (train-time-mta-station "D43"))
                 '("D" "F" "N" "Q")))
  (should (equal (train-time-mta-line-title "A") "A  8 Avenue Express"))
  ;; Every station appears on at least one line diagram.
  (should (cl-every #'train-time-mta-line-ids train-time-mta-stations))
  ;; Every line's stopping patterns only use its own rows, in order.
  (dolist (id (train-time-mta-line-ids))
    (let ((rows (plist-get (train-time-mta-line id) :rows)))
      (dolist (column (plist-get (train-time-mta-line id) :columns))
        (let ((positions (mapcar (lambda (s) (cl-position s rows :test #'equal))
                                 (plist-get column :stops))))
          (should (cl-every #'identity positions))
          (should (equal positions (sort (copy-sequence positions) #'<))))))))

(ert-deftest train-time-test-mta-coney-island-arrivals ()
  (let* ((now train-time-test--coney-time)
         (station (train-time-mta-station "D43"))
         (bdfm (train-time-mta-arrivals
                (train-time-gtfs-rt-decode (train-time-test--fixture "gtfs-bdfm"))
                station now))
         (nqrw (train-time-mta-arrivals
                (train-time-gtfs-rt-decode (train-time-test--fixture "gtfs-nqrw"))
                station now)))
    (should (= (length bdfm) 54))
    (should (= (length nqrw) 46))
    (should (equal (sort (delete-dups (mapcar #'train-time-arrival-route-id bdfm)) #'string<)
                   '("D" "F" "FX")))
    (let ((fx (cl-find "FX" bdfm :key #'train-time-arrival-route-id :test #'equal))
          (d (cl-find "D" bdfm :key #'train-time-arrival-route-id :test #'equal)))
      (should (equal (train-time-arrival-route fx) "F"))
      (should (eq (train-time-arrival-service fx) 'express))
      (should (null (train-time-arrival-service d))))
    (should (equal (sort (delete-dups (mapcar #'train-time-arrival-route nqrw)) #'string<)
                   '("N" "Q")))))

(ert-deftest train-time-test-mta-flushing-arrivals ()
  (let* ((now train-time-test--fixture-time)
         (arrivals (train-time-mta-arrivals
                    (train-time-test--feed) (train-time-mta-station "701") now))
         (first (car arrivals)))
    (should (= (length arrivals) 23))
    ;; Sorted, and nothing older than the grace period: the raw feed
    ;; contains Flushing trips from about 13 hours earlier.
    (should (equal arrivals (sort (copy-sequence arrivals)
                                  (lambda (a b) (< (train-time-arrival-time a)
                                                   (train-time-arrival-time b))))))
    (should (cl-every (lambda (a) (>= (train-time-arrival-time a) (- now 30))) arrivals))
    ;; Next train: a tracked local leaving the terminal for Manhattan.
    (should (equal (train-time-arrival-trip-id first) "090100_7..S"))
    (should (equal (train-time-arrival-route first) "7"))
    (should (eq (train-time-arrival-service first) 'local))
    (should (eq (train-time-arrival-event first) 'departure))
    (should (equal (train-time-arrival-direction-label first) "Manhattan"))
    (should (equal (train-time-arrival-destination first) "34 St-Hudson Yards"))
    (should (train-time-arrival-assigned first))
    (should-not (train-time-arrival-terminating first))
    (should (= (train-time-minutes-until (train-time-arrival-time first) now) 2))
    ;; Northbound calls at Flushing all terminate there.
    (should (cl-every #'train-time-arrival-terminating
                      (cl-remove-if-not (lambda (a) (eq (train-time-arrival-direction a) 'north))
                                        arrivals)))
    ;; Express <7> trips are recognised.
    (should (cl-some (lambda (a) (and (equal (train-time-arrival-route-id a) "7X")
                                      (eq (train-time-arrival-service a) 'express)))
                     arrivals))
    ;; Scheduled trips without an assigned train are flagged, not hidden.
    (let ((sched (cl-find "091100_7..S" arrivals
                          :key #'train-time-arrival-trip-id :test #'equal)))
      (should sched)
      (should-not (train-time-arrival-assigned sched)))))

(ert-deftest train-time-test-mta-skipped-and-missing ()
  (let* ((feed '(:header (:timestamp 1000)
                 :entity
                 ((:id "a" :trip-update
                       (:trip (:trip-id "a" :route-id "7" :nyct (:is-assigned t))
                              :stop-time-update
                              ((:stop-id "701S" :departure (:time 1100))
                               (:stop-id "726S" :arrival (:time 3000)))))
                  (:id "b" :trip-update
                       (:trip (:trip-id "b" :route-id "7")
                              :stop-time-update
                              ((:stop-id "701S" :schedule-relationship 1
                                         :departure (:time 1200))
                               (:stop-id "726S" :arrival (:time 3100)))))
                  (:id "c" :trip-update
                       (:trip (:trip-id "c" :route-id "7")
                              :stop-time-update ((:stop-id "701S"))))
                  (:id "d" :is-deleted t :trip-update
                       (:trip (:trip-id "d" :route-id "7")
                              :stop-time-update ((:stop-id "701S" :departure (:time 1300))))))))
         (arrivals (train-time-mta-arrivals feed (train-time-mta-station "701") 1000)))
    (should (equal (mapcar #'train-time-arrival-trip-id arrivals) '("a")))))

(ert-deftest train-time-test-mta-assigned-without-extension ()
  ;; Generic GTFS-RT without the NYCT extension: treat trips as tracked.
  (let* ((feed '(:entity ((:trip-update
                           (:trip (:trip-id "x" :route-id "7")
                                  :stop-time-update ((:stop-id "702S" :arrival (:time 50))))))))
         (a (car (train-time-mta-arrivals feed (train-time-mta-station "702") 0))))
    (should (train-time-arrival-assigned a))))

;;;; Time zones

(ert-deftest train-time-test-clock-time-zone ()
  ;; Displayed in New York time regardless of the Emacs process zone.
  (let ((process-environment (cons "TZ=Asia/Tokyo" process-environment)))
    (should (equal (train-time-format-clock train-time-test--fixture-time
                                            train-time-mta-time-zone)
                   "14:58"))
    ;; Daylight saving time: 2026-07-01 12:00 UTC is 08:00 EDT.
    (should (equal (train-time-format-clock 1782907200 train-time-mta-time-zone)
                   "08:00")))
  (should (equal (train-time-format-clock nil train-time-mta-time-zone) "—"))
  (should (= (train-time-minutes-until 1119 1000) 1))
  (should (null (train-time-minutes-until nil 1000))))

;;;; User interface

(defmacro train-time-test--with-buffer (fetch now &rest body)
  "Open Flushing with FETCH as fetch function at time NOW, then run BODY."
  (declare (indent 2))
  `(let ((train-time-fetch-function ,fetch)
         (train-time-now-function (lambda () ,now))
         (train-time-auto-refresh nil)
         (train-time-show-terminating nil)
         (inhibit-message t))
     (save-window-excursion
       (train-time (train-time-mta-station "701"))
       (unwind-protect
           (progn ,@body)
         (kill-buffer "*train-time: Flushing-Main St*")))))

(defun train-time-test--fixture-fetch (_url callback)
  (funcall callback (list :data (train-time-test--fixture))))

(defun train-time-test--text ()
  (buffer-substring-no-properties (point-min) (point-max)))

(ert-deftest train-time-test-ui-lists-arrivals ()
  (train-time-test--with-buffer #'train-time-test--fixture-fetch
      train-time-test--fixture-time
    (should (derived-mode-p 'train-time-mode))
    (should (= (length tabulated-list-entries) 7))
    (train-time-toggle-terminating)
    (should (= (length tabulated-list-entries) 23))
    (goto-char (point-min))
    (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position))))
      (should (string-match-p "Manhattan" line))
      (should (string-match-p "34 St-Hudson Yards" line))
      (should (string-match-p "Departs" line))
      (should (string-match-p "2 min" line))
      (should (string-match-p "Tracked" line)))
    (should (string-match-p "Feed time 14:58:39 EST (0s old)" (train-time-test--text)))
    (should (string-match-p "Scheduled" (train-time-test--text)))
    (should (string-match-p "<7>" (train-time-test--text)))
    (should (string-match-p "Data: Metropolitan Transportation Authority"
                            (train-time-test--text)))))

(ert-deftest train-time-test-ui-filters ()
  (train-time-test--with-buffer #'train-time-test--fixture-fetch
      train-time-test--fixture-time
    (train-time-toggle-terminating)
    (train-time--set-filter :direction 'south)
    (should (= (length tabulated-list-entries) 7))
    (train-time--set-filter :direction 'north)
    (should (= (length tabulated-list-entries) 16))
    (should (cl-every (lambda (e) (equal (aref (cadr e) 3) "Terminates here"))
                      tabulated-list-entries))
    (train-time--set-filter :service 'express)
    (should (cl-every (lambda (e) (equal (string-trim (substring-no-properties (aref (cadr e) 0)))
                                         "<7>"))
                      tabulated-list-entries))
    (should (string-match-p "Filters: direction=north service=express"
                            (train-time-test--text)))
    (train-time-clear-filters)
    (should (= (length tabulated-list-entries) 23))))

(ert-deftest train-time-test-ui-hides-terminating-by-default ()
  (train-time-test--with-buffer #'train-time-test--fixture-fetch
      train-time-test--fixture-time
    (should (= (length tabulated-list-entries) 7))
    (should (cl-every (lambda (e) (equal (aref (cadr e) 4) "Departs"))
                      tabulated-list-entries))
    (should (string-match-p "16 terminating trains hidden" (train-time-test--text)))
    (train-time-toggle-terminating)
    (should (= (length tabulated-list-entries) 23))))

(ert-deftest train-time-test-ui-stale ()
  (train-time-test--with-buffer #'train-time-test--fixture-fetch
      (+ train-time-test--fixture-time 600)
    (should (string-match-p "STALE" (train-time-test--text)))
    (should (equal mode-line-process " stale"))))

(ert-deftest train-time-test-ui-error-keeps-previous-data ()
  (let ((fail nil))
    (train-time-test--with-buffer
        (lambda (url cb)
          (if fail (funcall cb '(:error "Network error: host unreachable"))
            (train-time-test--fixture-fetch url cb)))
        train-time-test--fixture-time
      (setq fail t)
      (revert-buffer)
      (should (= (length tabulated-list-entries) 7))
      (should (string-match-p "Last update failed: Network error: host unreachable"
                              (train-time-test--text)))
      (should (equal mode-line-process " error")))))

(ert-deftest train-time-test-ui-malformed-response ()
  (train-time-test--with-buffer
      (lambda (_url cb) (funcall cb '(:data "<html>Service Unavailable</html>")))
      train-time-test--fixture-time
    (should (null tabulated-list-entries))
    (should (string-match-p "Could not decode feed" (train-time-test--text)))
    (should (string-match-p "No data yet" (train-time-test--text)))))

(ert-deftest train-time-test-ui-ignores-superseded-response ()
  (let (pending)
    (train-time-test--with-buffer
        (lambda (_url cb) (push cb pending))
        train-time-test--fixture-time
      (should train-time--loading)
      (should (string-match-p "Updating" (train-time-test--text)))
      (revert-buffer)
      (should (= (length pending) 2))
      ;; The older response arrives last and must be ignored.
      (funcall (car pending) (list :data (train-time-test--fixture)))
      (funcall (cadr pending) '(:error "late"))
      (should-not train-time--loading)
      (should-not train-time--error)
      (should (= (length tabulated-list-entries) 7)))))

(ert-deftest train-time-test-ui-auto-refresh-timer ()
  (train-time-test--with-buffer #'train-time-test--fixture-fetch
      train-time-test--fixture-time
    (train-time-toggle-auto-refresh 1)
    (let ((timer train-time--timer))
      (should (timerp timer))
      (should (memq timer timer-list))
      (kill-buffer (current-buffer))
      (should-not (memq timer timer-list))
      (get-buffer-create "*train-time: Flushing-Main St*"))))

(defun train-time-test--feeds-fetch (url callback)
  "Answer URL from the recorded feed it names, using CALLBACK."
  (funcall callback
           (list :data (train-time-test--fixture
                        (replace-regexp-in-string "\\`.*%2F" "" url)))))

(defmacro train-time-test--with-station (id fetch now &rest body)
  "Open station ID with FETCH as fetch function at time NOW, then run BODY."
  (declare (indent 3))
  `(let ((train-time-fetch-function ,fetch)
         (train-time-now-function (lambda () ,now))
         (train-time-auto-refresh nil)
         (train-time-show-terminating nil)
         (inhibit-message t))
     (save-window-excursion
       (train-time (train-time-mta-station ,id))
       (unwind-protect
           (progn ,@body)
         (kill-buffer (current-buffer))))))

(ert-deftest train-time-test-ui-several-feeds ()
  (let (urls)
    (train-time-test--with-station "D43"
        (lambda (url cb) (push url urls) (train-time-test--feeds-fetch url cb))
        train-time-test--coney-time
      (should (equal (buffer-name) "*train-time: Coney Island-Stillwell Av*"))
      (should (= (length urls) 2))
      (should-not train-time--loading)
      (should-not train-time--error)
      ;; Arrivals from both feeds, merged in time order.
      (should (= (length train-time--arrivals) 100))
      (should (= (length tabulated-list-entries) 30))
      (should (equal (mapcar #'train-time-arrival-route (seq-take train-time--arrivals 1))
                     '("D")))
      (let ((routes (delete-dups (mapcar #'train-time-arrival-route train-time--arrivals))))
        (should (equal (sort routes #'string<) '("D" "F" "N" "Q"))))
      ;; The oldest feed decides the freshness shown.
      (should (= train-time--feed-time 1794861313))
      (should (string-match-p "Feed time 15:35:13 EST (4s old)" (train-time-test--text)))
      ;; The D has no <D>, so its service column stays blank.
      (goto-char (point-min))
      (should (string-match-p "\\` +D +Manhattan"
                              (buffer-substring-no-properties (point) (line-end-position))))
      (train-time--set-filter :route "Q")
      (should (cl-every (lambda (e) (equal (string-trim (substring-no-properties
                                                         (aref (cadr e) 0)))
                                           "Q"))
                        tabulated-list-entries)))))

(ert-deftest train-time-test-ui-one-feed-fails ()
  (let ((fail-nqrw nil))
    (train-time-test--with-station "D43"
        (lambda (url cb)
          (if (and fail-nqrw (string-suffix-p "nqrw" url))
              (funcall cb '(:error "HTTP status 503"))
            (train-time-test--feeds-fetch url cb)))
        train-time-test--coney-time
      (should (= (length train-time--arrivals) 100))
      (setq fail-nqrw t)
      (revert-buffer)
      ;; The failing feed's last data is kept; the error names the feed.
      (should (= (length train-time--arrivals) 100))
      (should (string-match-p "Last update failed: gtfs-nqrw: HTTP status 503"
                              (train-time-test--text)))
      (should (equal mode-line-process " error"))
      (setq fail-nqrw nil)
      (revert-buffer)
      (should-not train-time--error))))

(ert-deftest train-time-test-ui-first-fetch-partial-failure ()
  (train-time-test--with-station "D43"
      (lambda (url cb)
        (if (string-suffix-p "bdfm" url)
            (funcall cb '(:error "Request timed out"))
          (train-time-test--feeds-fetch url cb)))
      train-time-test--coney-time
    (should (= (length train-time--arrivals) 46))
    (should (= train-time--feed-time 1794861317))
    (should (string-match-p "gtfs-bdfm: Request timed out" (train-time-test--text)))))

;;;; Station complexes

(defconst train-time-test--times-sq-time 1794863572)

(defun train-time-test--times-sq-fetch (url callback)
  "Answer URL from the Times Sq recordings, using CALLBACK."
  (funcall callback
           (list :data (train-time-test--fixture
                        (replace-regexp-in-string "\\`.*%2F" "" url)
                        "20261116-1612"))))

;;;; Destination links

(ert-deftest train-time-test-destination-link ()
  (train-time-test--with-station "701" #'train-time-test--fixture-fetch
      train-time-test--fixture-time
    (goto-char (point-min))
    (let ((button (next-button (point))))
      (should button)
      (should (equal (button-label button) "34 St-Hudson Yards"))
      (should (equal (button-get button 'train-time-destination) "726"))
      (goto-char button)
      (push-button)
      (unwind-protect
          (progn
            (should (derived-mode-p 'train-time-mode))
            (should (equal (train-time-station-id train-time--station) "726"))
            (should (equal (buffer-name) "*train-time: 34 St-Hudson Yards*")))
        (kill-buffer)))))

(ert-deftest train-time-test-goto-destination-command ()
  (train-time-test--with-station "701" #'train-time-test--fixture-fetch
      train-time-test--fixture-time
    (goto-char (point-min))
    (train-time-goto-destination)
    (unwind-protect
        (should (equal (train-time-station-id train-time--station) "726"))
      (kill-buffer))))

(ert-deftest train-time-test-terminating-not-linked ()
  (train-time-test--with-station "701" #'train-time-test--fixture-fetch
      train-time-test--fixture-time
    (train-time-toggle-terminating)
    (goto-char (point-min))
    (let ((found nil))
      (while (and (not found) (not (eobp)))
        (when (string-match-p "Terminates here"
                              (buffer-substring (line-beginning-position)
                                                (line-end-position)))
          (setq found t)
          (should-not (cl-some (lambda (pos) (button-at pos))
                               (number-sequence (line-beginning-position)
                                                (1- (line-end-position))))))
        (forward-line 1))
      (should found))))

(defun train-time-test--headings ()
  "Return the complex board's group headings, without properties."
  (let (headings)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (train-time--heading-p)
          (push (string-trim (buffer-substring-no-properties
                              (point) (line-end-position)))
                headings))
        (forward-line 1)))
    (nreverse headings)))

(ert-deftest train-time-test-complex-grand-central ()
  (let (urls)
    (train-time-test--with-station "723"
        (lambda (url cb) (push url urls) (train-time-test--times-sq-fetch url cb))
        train-time-test--times-sq-time
      (should (equal (buffer-name) "*train-time: Grand Central-42 St*"))
      (should (equal (mapcar #'train-time-station-id train-time--stations)
                     '("723" "631" "901")))
      ;; The 4 5 6, 7 and S share one feed, fetched once.
      (should (equal urls (list (concat train-time-mta-feed-base-url "nyct%2Fgtfs"))))
      ;; The chosen station's platforms come first; the S's arrivals
      ;; from Times Sq all terminate here, so it has only one group.
      (should (equal (train-time-test--headings)
                     '("▾  7   Queens  3 of 13"
                       "▾  7   34 St - Hudson Yards  3 of 19"
                       "▾  4  5  6   Uptown & The Bronx  3 of 34"
                       "▾  4  5  6   Downtown & Brooklyn  3 of 53"
                       "▾  S   Times Sq  3 of 8")))
      (should (= (length tabulated-list-entries) 15))
      (let ((routes (delete-dups (mapcar #'train-time-arrival-route train-time--arrivals))))
        (should (equal (sort routes #'string<) '("4" "5" "6" "7" "S"))))
      ;; Rows know their platform.
      (goto-char (point-min))
      (search-forward "Pelham Bay Park")
      (should (equal (train-time-station-id (train-time-station-at-point)) "631"))
      (should (equal (cdr (tabulated-list-get-id)) "631N")))))

(ert-deftest train-time-test-complex-times-sq ()
  (let (urls)
    (train-time-test--with-station "725"
        (lambda (url cb) (push url urls) (train-time-test--times-sq-fetch url cb))
        train-time-test--times-sq-time
      (should (equal (buffer-name) "*train-time: Times Sq-42 St*"))
      (should (equal (sort (mapcar #'train-time--feed-name urls) #'string<)
                     '("gtfs" "gtfs-ace" "gtfs-nqrw")))
      (should-not train-time--error)
      (should (= train-time--feed-time 1794863562))
      (should (equal (train-time-test--headings)
                     '("▾  7   Queens  3 of 11"
                       "▾  7   34 St - Hudson Yards  3 of 20"
                       "▾  N  Q  R  W   Uptown & Queens  3 of 57"
                       "▾  N  Q  R  W   Downtown & Brooklyn  3 of 45"
                       "▾  A  C  E   42 St-Port Authority Bus Terminal · Uptown - Queens  3 of 48"
                       "▾  A  C  E   42 St-Port Authority Bus Terminal · Downtown & Brooklyn  3 of 51"
                       "▾  1  2  3   Uptown & The Bronx  3 of 34"
                       "▾  1  2  3   Downtown & Brooklyn  3 of 36"
                       "▾  S   Grand Central  3 of 7")))
      (should (= (length tabulated-list-entries) 27))
      (should (string-match-p "TAB more trains or fold" (train-time-test--text))))))

(ert-deftest train-time-test-complex-groups ()
  (train-time-test--with-station "725" #'train-time-test--times-sq-fetch
      train-time-test--times-sq-time
    (goto-char (point-min))
    (should-error (train-time-previous-group) :type 'user-error)
    (train-time-next-group 2)
    (should (equal (get-text-property (point) 'train-time-group) '("R16" . north)))
    ;; From a train, M-p goes to its own heading first.
    (forward-line 2)
    (train-time-previous-group)
    (should (equal (get-text-property (point) 'train-time-group) '("R16" . north)))
    (should (train-time--heading-p))
    ;; TAB: up to `train-time-max-rows' trains, then folded, then three.
    (train-time-cycle-group)
    (should (member "▾  N  Q  R  W   Uptown & Queens  30 of 57" (train-time-test--headings)))
    (should (= (length tabulated-list-entries) 54))
    (should (equal (get-text-property (point) 'train-time-group) '("R16" . north)))
    (train-time-cycle-group)
    (should (member "▸  N  Q  R  W   Uptown & Queens  57 trains" (train-time-test--headings)))
    (should (= (length tabulated-list-entries) 24))
    (train-time-cycle-group)
    (should (= (length tabulated-list-entries) 27))
    ;; A group with no more trains than the limit folds at once.
    (train-time-next-group 6)
    (let ((train-time-group-max-rows 10))
      (train-time-cycle-group)
      (should (equal (car (last (train-time-test--headings)))
                     "▸  S   Grand Central  7 trains"))
      (train-time-cycle-group))
    (should-error (train-time-next-group) :type 'user-error)
    ;; Folding survives a refresh, which keeps point on the group.
    (train-time-previous-group)
    (train-time-cycle-group)
    (train-time-cycle-group)
    (revert-buffer)
    (should (equal (get-text-property (point) 'train-time-group) '("127" . south)))
    (should (member "▸  1  2  3   Downtown & Brooklyn  36 trains" (train-time-test--headings)))
    ;; S-TAB folds everything, then unfolds everything.
    (train-time-cycle-all-groups)
    (should (null tabulated-list-entries))
    (should (= (length (train-time-test--headings)) 9))
    (train-time-cycle-all-groups)
    (should (= (length tabulated-list-entries) 27))))

(ert-deftest train-time-test-complex-filters ()
  (train-time-test--with-station "725" #'train-time-test--times-sq-fetch
      train-time-test--times-sq-time
    ;; Filtering by line leaves only the platforms that line serves.
    (train-time--set-filter :route "A")
    (should (equal (train-time-test--headings)
                   '("▾  A  C  E   42 St-Port Authority Bus Terminal · Uptown - Queens  3 of 18"
                     "▾  A  C  E   42 St-Port Authority Bus Terminal · Downtown & Brooklyn  3 of 20")))
    (train-time-clear-filters)
    ;; Each platform names directions its own way.
    (let (offered)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _) (setq offered table) "All")))
        (train-time-filter-direction))
      (should (equal offered
                     '("All"
                       "North (Queens, Uptown & Queens, Uptown - Queens, Uptown & The Bronx, terminating)"
                       "South (34 St - Hudson Yards, Downtown & Brooklyn, Grand Central)"))))
    (let (offered)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _) (setq offered table) "All")))
        (train-time-filter-line))
      (should (equal (sort (cdr offered) #'string<)
                     '("1" "2" "3" "7" "A" "C" "E" "N" "Q" "R" "S" "W"))))))

(ert-deftest train-time-test-complex-feed-fails ()
  (train-time-test--with-station "725"
      (lambda (url cb)
        (if (string-suffix-p "ace" url)
            (funcall cb '(:error "HTTP status 503"))
          (train-time-test--times-sq-fetch url cb)))
      train-time-test--times-sq-time
    ;; The A C E platform stays listed, saying it has no trains.
    (should (member "▾  A  C  E   42 St-Port Authority Bus Terminal · Uptown - Queens  no trains"
                    (train-time-test--headings)))
    (should (string-match-p "gtfs-ace: HTTP status 503" (train-time-test--text)))))

(ert-deftest train-time-test-complex-shared-board ()
  (train-time-test--with-station "725" #'train-time-test--times-sq-fetch
      train-time-test--times-sq-time
    (let ((buffer (current-buffer)))
      (train-time-next-group 8)
      (train-time-cycle-group)
      (train-time-cycle-group)
      ;; Another station of the complex opens the same board, its
      ;; platforms first, keeping the groups as they were.
      (should (eq (train-time-board (train-time-mta-station "A27")) buffer))
      (should (equal (train-time-station-id train-time--station) "A27"))
      (should (equal (car (train-time-test--headings))
                     "▾  A  C  E   Uptown - Queens  3 of 48"))
      (should (member "▾  N  Q  R  W   Times Sq-42 St · Uptown & Queens  3 of 57"
                      (train-time-test--headings)))
      (should (member "▸  S   Times Sq-42 St · Grand Central  7 trains"
                      (train-time-test--headings)))
      (should (= (point) (point-min))))))

(ert-deftest train-time-test-complex-scope-station ()
  (let ((train-time-board-scope 'station))
    (train-time-test--with-station "725" #'train-time-test--times-sq-fetch
        train-time-test--times-sq-time
      (should (equal (buffer-name) "*train-time: Times Sq-42 St (7)*"))
      (should (equal train-time--stations (list train-time--station)))
      (should (null (train-time-test--headings)))
      (should (= (length tabulated-list-entries) 30))
      (should-error (train-time-cycle-group) :type 'user-error))))

(provide 'train-time-test)
;;; train-time-test.el ends here
