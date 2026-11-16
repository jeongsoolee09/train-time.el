;;; train-time-mta-generate-test.el --- Tests for the MTA data generator -*- lexical-binding: t; -*-

;; Copyright (C) 2026  train-time.el contributors

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Tests for scripts/train-time-mta-generate.el, using a tiny made-up
;; GTFS feed written to a temporary directory.

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "../scripts/train-time-mta-generate.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(ert-deftest train-time-mta-generate-test-csv ()
  (should (equal (train-time-mta-generate--parse-line "a,b,,c") '("a" "b" "" "c")))
  (should (equal (train-time-mta-generate--parse-line "1,\"x, y\",\"say \"\"hi\"\"\"")
                 '("1" "x, y" "say \"hi\""))))

(ert-deftest train-time-mta-generate-test-family ()
  (let ((ids '("6" "6X" "7" "7X" "FX" "F" "SI")))
    (should (equal (train-time-mta-generate--family "7X" ids) "7"))
    (should (equal (train-time-mta-generate--family "F" ids) "F"))
    (should (equal (train-time-mta-generate--family "SI" ids) "SI"))
    ;; An X route without a base route is a line of its own.
    (should (equal (train-time-mta-generate--family "ZX" ids) "ZX"))))

(ert-deftest train-time-mta-generate-test-choose ()
  (pcase-let ((`(,chosen ,total)
               (train-time-mta-generate--choose
                '(("7" ("a" "b" "c" "d" "e") 100)
                  ;; Short turn: dropped.
                  ("7" ("b" "c" "d") 50)
                  ;; Same ends, one stop fewer: merged into the first.
                  ("7" ("a" "c" "d" "e") 30)
                  ;; Express variant: kept although rare.
                  ("7X" ("a" "c" "e") 2)
                  ;; Rare branch: below 5%, dropped.
                  ("7" ("a" "b" "x") 3)))))
    (should (= total 135))
    (should (equal chosen '(("7" ("a" "b" "c" "d" "e") 130)
                            ("7X" ("a" "c" "e") 2))))))

(ert-deftest train-time-mta-generate-test-rows ()
  (pcase-let ((`(,rows . ,kept)
               (train-time-mta-generate--rows
                '(("A" ("a" "b" "c" "d") 1)
                  ;; A branch leaving after "c", and stops before "a".
                  ("A" ("z" "a" "c" "x" "y") 1)
                  ;; Out of order: dropped.
                  ("A" ("d" "a") 1)))))
    (should (equal rows '("z" "a" "b" "c" "d" "x" "y")))
    (should (= (length kept) 2))))

(defun train-time-mta-generate-test--read (file)
  "Return an alist of the constants FILE defines, without loading it."
  (with-temp-buffer
    (insert-file-contents file)
    (let (result form)
      (while (setq form (ignore-errors (read (current-buffer))))
        (when (eq (car-safe form) 'defconst)
          (push (cons (nth 1 form) (eval (nth 2 form) t)) result)))
      result)))

(defun train-time-mta-generate-test--write (dir name &rest lines)
  (with-temp-file (expand-file-name name dir)
    (insert (mapconcat #'identity lines "\n") "\n")))

(ert-deftest train-time-mta-generate-test-end-to-end ()
  (let* ((dir (make-temp-file "tt-gtfs" t))
         (output (expand-file-name "data.el" dir))
         (inhibit-message t))
    (unwind-protect
        (progn
          (train-time-mta-generate-test--write
           dir "routes.txt"
           "route_id,agency_id,route_short_name,route_long_name,route_desc,route_type,route_url,route_color,route_text_color,route_sort_order"
           "9X,MTA,9X,Nine Express,\"Rush hours, peak direction\",1,,abcdef,FFFFFF,2"
           "9,MTA,9,Nine Local,\"Local, all times\",1,,abcdef,FFFFFF,1")
          (train-time-mta-generate-test--write
           dir "calendar.txt"
           "service_id,monday,tuesday,wednesday,thursday,friday,saturday,sunday,start_date,end_date"
           "Weekday,1,1,1,1,1,0,0,20260101,20261231"
           "Sunday,0,0,0,0,0,0,1,20260101,20261231")
          (train-time-mta-generate-test--write
           dir "trips.txt"
           "route_id,trip_id,service_id,trip_headsign,direction_id,shape_id"
           "9,t1,Weekday,South,1,"
           "9,t2,Sunday,North,0,"
           "9X,t3,Weekday,South,1,")
          (train-time-mta-generate-test--write
           dir "stop_times.txt"
           "trip_id,stop_id,arrival_time,departure_time,stop_sequence"
           "t1,901S,08:00:00,08:00:00,1" "t1,902S,08:02:00,08:02:00,2"
           "t1,903S,08:04:00,08:04:00,3"
           "t2,903N,09:00:00,09:00:00,1" "t2,902N,09:02:00,09:02:00,2"
           "t2,901N,09:04:00,09:04:00,3"
           "t3,901S,08:00:00,08:00:00,1" "t3,903S,08:03:00,08:03:00,2")
          (train-time-mta-generate-test--write
           dir "feed_info.txt"
           "feed_publisher_name,feed_publisher_url,feed_lang,feed_version"
           "MTA,http://mta.info,en,test-1")
          (train-time-mta-generate-test--write
           dir "Stations.csv"
           "\ufeffStation ID,Complex ID,GTFS Stop ID,Division,Line,Stop Name,Borough,Daytime Routes,Structure,GTFS Latitude,GTFS Longitude,North Direction Label,South Direction Label"
           "1,1,901,IRT,Nine,North End,M,9,Subway,40.1,-73.1,,Downtown"
           "2,2,902,IRT,Nine,Middle,M,9,Subway,40.2,-73.2,Uptown,Downtown"
           "3,2,903,IRT,Nine,South End,M,9,Subway,40.3,-73.3,Uptown,")
          (train-time-mta-generate dir (expand-file-name "Stations.csv" dir) output)
          (let ((data (train-time-mta-generate-test--read output)))
            (should (equal (alist-get 'train-time-mta-data-version data) "test-1"))
            (should (equal (alist-get 'train-time-mta-data-routes data)
                           '(("9" "9" "Nine Local" "#ABCDEF" "#FFFFFF")
                             ("9X" "9X" "Nine Express" "#ABCDEF" "#FFFFFF"))))
            (should (equal (car (alist-get 'train-time-mta-data-stations data))
                           '("901" "North End" 40.1 -73.1 nil "Downtown" "1" "9"
                             ("9" "9X"))))
            (let ((line (cdr (assoc "9" (alist-get 'train-time-mta-data-lines data)))))
              (should (equal (plist-get line :routes) '("9" "9X")))
              (should (equal (plist-get line :rows) '("901" "902" "903")))
              ;; The northbound trip is read in southbound order.
              (should (equal (plist-get (car (plist-get line :columns)) :stops)
                             '("901" "902" "903")))
              (should (equal (mapcar (lambda (c) (plist-get c :share))
                                     (plist-get line :columns))
                             '(54.5 45.5))))))
      (delete-directory dir t))))

(provide 'train-time-mta-generate-test)
;;; train-time-mta-generate-test.el ends here
