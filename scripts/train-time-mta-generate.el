;;; train-time-mta-generate.el --- Build train-time-mta-data.el from MTA static data -*- lexical-binding: t; -*-

;; Copyright (C) 2026  train-time.el contributors

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Maintainer tool, not part of the package.  Run `make data', or:
;;
;;   curl -LO https://rrgtfsfeeds.s3.amazonaws.com/gtfs_subway.zip
;;   unzip gtfs_subway.zip -d gtfs
;;   curl -LO https://web.mta.info/developers/data/nyct/subway/Stations.csv
;;   emacs -Q --batch -l scripts/train-time-mta-generate.el \
;;     -f train-time-mta-generate-batch gtfs Stations.csv train-time-mta-data.el
;;
;; Station names, direction labels and complexes come from Stations.csv;
;; routes and stopping patterns from the static GTFS schedule.
;;
;; Each line (a route plus its express variant, e.g. 7 and 7X) gets a
;; few representative stopping patterns:
;;
;; 1. Every scheduled trip becomes a sequence of parent stops, written
;;    in the order a southbound ("S" platform) train visits them.
;; 2. Trips are weighted by the days per week their service runs.
;; 3. Patterns that are an unbroken piece of a longer one (short
;;    turns, trips entering or leaving service) are dropped.
;; 4. Patterns with the same ends that differ by at most two stops are
;;    merged into the more frequent one, which gains the extra stops.
;; 5. The most frequent pattern of each route is kept, plus others with
;;    at least 5% of the line's trips, at most four in total.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defconst train-time-mta-generate--min-share 0.05)
(defconst train-time-mta-generate--max-columns 4)
(defconst train-time-mta-generate--merge-distance 2)

;;;; CSV

(defun train-time-mta-generate--parse-line (line)
  "Split CSV LINE into fields, honoring double quotes."
  (if (not (string-search "\"" line))
      (split-string line ",")
    (let ((fields nil) (field nil) (i 0) (n (length line)) (quoted nil))
      (while (< i n)
        (let ((c (aref line i)))
          (cond
           ((and quoted (eq c ?\") (< (1+ i) n) (eq (aref line (1+ i)) ?\"))
            (push c field) (cl-incf i))
           ((eq c ?\") (setq quoted (not quoted)))
           ((and (not quoted) (eq c ?,))
            (push (concat (nreverse field)) fields) (setq field nil))
           (t (push c field))))
        (cl-incf i))
      (push (concat (nreverse field)) fields)
      (nreverse fields))))

(defun train-time-mta-generate--read-csv (file fn)
  "Call FN with an alist (HEADER-SYMBOL . VALUE) for each row of CSV FILE."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents file))
    (goto-char (point-min))
    (let ((header (mapcar #'intern
                          (train-time-mta-generate--parse-line
                           (string-trim (buffer-substring (point) (line-end-position))
                                        "\ufeff")))))
      (forward-line 1)
      (while (not (eobp))
        (let ((line (string-trim-right (buffer-substring (point) (line-end-position)))))
          (unless (string-empty-p line)
            (funcall fn (cl-mapcar #'cons header
                                   (train-time-mta-generate--parse-line line)))))
        (forward-line 1)))))

(defun train-time-mta-generate--get (row key)
  "Return field KEY (a string) of ROW."
  (cdr (assq (intern key) row)))

(defun train-time-mta-generate--nonempty (s)
  (unless (or (null s) (string-empty-p s)) s))

;;;; Reading

(defun train-time-mta-generate--stations (file)
  "Read Stations.csv FILE into a list of station rows."
  (let (result)
    (train-time-mta-generate--read-csv
     file
     (lambda (r)
       (cl-flet ((get (k) (train-time-mta-generate--get r k)))
         (push (list (get "GTFS Stop ID")
                     (get "Stop Name")
                     (string-to-number (get "GTFS Latitude"))
                     (string-to-number (get "GTFS Longitude"))
                     (train-time-mta-generate--nonempty (get "North Direction Label"))
                     (train-time-mta-generate--nonempty (get "South Direction Label"))
                     (get "Complex ID")
                     (get "Daytime Routes"))
               result))))
    (nreverse result)))

(defun train-time-mta-generate--routes (dir)
  "Read routes.txt in DIR, in the agency's sort order."
  (let (result)
    (train-time-mta-generate--read-csv
     (expand-file-name "routes.txt" dir)
     (lambda (r)
       (cl-flet ((get (k) (train-time-mta-generate--get r k))
                 (color (k) (let ((c (train-time-mta-generate--nonempty
                                      (train-time-mta-generate--get r k))))
                              (and c (concat "#" (upcase c))))))
         (push (list (get "route_id")
                     (get "route_short_name")
                     (get "route_long_name")
                     (color "route_color")
                     (color "route_text_color")
                     (string-to-number (or (get "route_sort_order") "0")))
               result))))
    (sort (nreverse result) (lambda (a b) (< (nth 5 a) (nth 5 b))))))

(defun train-time-mta-generate--service-weights (dir)
  "Return a hash table of service_id -> days per week."
  (let ((weights (make-hash-table :test #'equal)))
    (train-time-mta-generate--read-csv
     (expand-file-name "calendar.txt" dir)
     (lambda (r)
       (puthash (train-time-mta-generate--get r "service_id")
                (cl-loop for day in '("monday" "tuesday" "wednesday" "thursday"
                                      "friday" "saturday" "sunday")
                         sum (string-to-number (train-time-mta-generate--get r day)))
                weights)))
    weights))

(defun train-time-mta-generate--trips (dir)
  "Return a hash table of trip_id -> (ROUTE-ID . SERVICE-ID)."
  (let ((trips (make-hash-table :test #'equal)))
    (train-time-mta-generate--read-csv
     (expand-file-name "trips.txt" dir)
     (lambda (r)
       (puthash (train-time-mta-generate--get r "trip_id")
                (cons (train-time-mta-generate--get r "route_id")
                      (train-time-mta-generate--get r "service_id"))
                trips)))
    trips))

(defun train-time-mta-generate--seconds (time)
  "Return GTFS TIME \"HH:MM:SS\" as seconds; HH may exceed 23."
  (pcase-let ((`(,h ,m ,s) (mapcar #'string-to-number (split-string time ":"))))
    (+ (* 3600 h) (* 60 m) s)))

(defun train-time-mta-generate--weighted-median (samples)
  "Return the weighted median of SAMPLES, a list of (VALUE . WEIGHT)."
  (let* ((sorted (sort (copy-sequence samples) (lambda (a b) (< (car a) (car b)))))
         (half (/ (apply #'+ (mapcar #'cdr sorted)) 2.0))
         (sum 0))
    (cl-loop for (value . weight) in sorted
             do (cl-incf sum weight)
             when (>= sum half) return value)))

(defun train-time-mta-generate--patterns (dir trips weights)
  "Return (PATTERNS STATION-ROUTES SEGMENTS) from stop_times.txt in DIR.
PATTERNS maps (ROUTE . STOPS) to weekly trips, STOPS in southbound
order.  STATION-ROUTES maps a parent stop to the routes calling there.
SEGMENTS maps (ROUTE DIRECTION FROM TO) to (SECONDS . WEEKLY-TRIPS)
samples: the time from a train's arrival at parent stop FROM to its
arrival at the next stop TO, for each trip."
  (let ((patterns (make-hash-table :test #'equal))
        (station-routes (make-hash-table :test #'equal))
        (segments (make-hash-table :test #'equal))
        (current nil) (calls nil))
    (cl-flet ((finish ()
                (when current
                  (let* ((sorted (sort calls (lambda (a b) (< (car a) (car b)))))
                         (platforms (mapcar #'cadr sorted))
                         (stops (mapcar (lambda (p) (substring p 0 -1)) platforms))
                         (trip (gethash current trips))
                         (route (car trip))
                         (weight (gethash (cdr trip) weights 1))
                         (direction (if (string-suffix-p "N" (car platforms)) 'north 'south)))
                    (cl-loop for (a b) on sorted
                             while b
                             do (push (cons (- (nth 2 b) (nth 2 a)) weight)
                                      (gethash (list route direction
                                                     (substring (nth 1 a) 0 -1)
                                                     (substring (nth 1 b) 0 -1))
                                               segments)))
                    (when (eq direction 'north)
                      (setq stops (nreverse stops)))
                    (dolist (s stops)
                      (cl-pushnew route (gethash s station-routes) :test #'equal))
                    (cl-incf (gethash (cons route stops) patterns 0) weight)))))
      (with-temp-buffer
        (insert-file-contents (expand-file-name "stop_times.txt" dir))
        (goto-char (point-min))
        (let* ((header (split-string (buffer-substring (point) (line-end-position)) ","))
               (trip-i (cl-position "trip_id" header :test #'equal))
               (stop-i (cl-position "stop_id" header :test #'equal))
               (arrival-i (cl-position "arrival_time" header :test #'equal))
               (seq-i (cl-position "stop_sequence" header :test #'equal)))
          (forward-line 1)
          (while (not (eobp))
            (let ((f (split-string (buffer-substring (point) (line-end-position)) ",")))
              (when (nth stop-i f)
                (unless (equal (nth trip-i f) current)
                  (finish)
                  (setq current (nth trip-i f) calls nil))
                (push (list (string-to-number (nth seq-i f))
                            (nth stop-i f)
                            (train-time-mta-generate--seconds (nth arrival-i f)))
                      calls)))
            (forward-line 1))
          (finish))))
    (list patterns station-routes segments)))

;;;; Choosing patterns

(defun train-time-mta-generate--sublist-p (short long)
  "Return non-nil if SHORT occurs as an unbroken run inside LONG."
  (let ((n (length short)))
    (cl-loop for tail on long
             while (>= (length tail) n)
             thereis (equal (seq-take tail n) short))))

(defun train-time-mta-generate--family (route-id route-ids)
  "Return the line ROUTE-ID belongs to: \"7X\" belongs to \"7\"."
  (let ((base (and (string-suffix-p "X" route-id) (substring route-id 0 -1))))
    (if (and base (member base route-ids)) base route-id)))

(defun train-time-mta-generate--distance (a b)
  "Number of stops in only one of stop lists A and B."
  (+ (length (cl-set-difference a b :test #'equal))
     (length (cl-set-difference b a :test #'equal))))

(defun train-time-mta-generate--choose (candidates)
  "Pick patterns from CANDIDATES, a list of (ROUTE STOPS WEIGHT).
Return (CHOSEN TOTAL-WEIGHT)."
  (let* ((sorted (sort (copy-sequence candidates) (lambda (a b) (> (nth 2 a) (nth 2 b)))))
         (whole (cl-remove-if
                 (lambda (p)
                   (cl-some (lambda (q)
                              (and (> (length (nth 1 q)) (length (nth 1 p)))
                                   (train-time-mta-generate--sublist-p (nth 1 p) (nth 1 q))))
                            sorted))
                 sorted))
         (merged nil))
    (dolist (p whole)
      (let ((into (cl-find-if
                   (lambda (q)
                     (and (equal (car q) (car p))
                          (equal (car (nth 1 q)) (car (nth 1 p)))
                          (equal (car (last (nth 1 q))) (car (last (nth 1 p))))
                          (<= (train-time-mta-generate--distance (nth 1 p) (nth 1 q))
                              train-time-mta-generate--merge-distance)))
                   merged)))
        (if into
            (progn
              (cl-incf (nth 2 into) (nth 2 p))
              ;; Keep stops that only some trips make.
              (setf (nth 1 into)
                    (car (train-time-mta-generate--rows (list into p)))))
          (setq merged (append merged (list (list (car p) (nth 1 p) (nth 2 p))))))))
    (setq merged (sort merged (lambda (a b) (> (nth 2 a) (nth 2 b)))))
    (let* ((total (apply #'+ (mapcar (lambda (p) (nth 2 p)) merged)))
           (tops (cl-remove-duplicates merged :key #'car :test #'equal :from-end t))
           (others (cl-remove-if (lambda (p)
                                   (or (memq p tops)
                                       (< (nth 2 p) (* total train-time-mta-generate--min-share))))
                                 merged)))
      (list (seq-take (append tops others) train-time-mta-generate--max-columns)
            total))))

(defun train-time-mta-generate--rows (columns)
  "Merge the stop lists of COLUMNS into one drawing order.
Return (ROWS . KEPT-COLUMNS).  A column whose stops would have to
appear out of order is dropped.  New stops before the first shared
stop go to the top, after the last shared stop to the bottom, and
between two shared stops just above the later one."
  (let ((rows (copy-sequence (nth 1 (car columns))))
        (kept (list (car columns))))
    (dolist (col (cdr columns))
      (let* ((stops (nth 1 col))
             (anchors (delq nil (mapcar (lambda (s) (cl-position s rows :test #'equal)) stops))))
        (when (and anchors (equal anchors (sort (copy-sequence anchors) #'<)))
          (let ((new rows) (run nil) (seen-anchor nil))
            (dolist (s stops)
              (if (member s new)
                  (progn
                    (when run
                      (let ((pos (cl-position s new :test #'equal)))
                        (setq new (if seen-anchor
                                      (append (seq-take new pos) (reverse run) (nthcdr pos new))
                                    (append (reverse run) new))
                              run nil)))
                    (setq seen-anchor t))
                (push s run)))
            (when run
              (setq new (append new (reverse run))))
            (setq rows new
                  kept (append kept (list col)))))))
    (cons rows kept)))

;;;; Output

(defun train-time-mta-generate--version (dir)
  (with-temp-buffer
    (insert-file-contents (expand-file-name "feed_info.txt" dir))
    (let ((header (train-time-mta-generate--parse-line
                   (buffer-substring (point) (line-end-position)))))
      (forward-line 1)
      (nth (cl-position "feed_version" header :test #'equal)
           (train-time-mta-generate--parse-line
            (buffer-substring (point) (line-end-position)))))))

(defun train-time-mta-generate (gtfs-dir stations-file output)
  "Write OUTPUT from the static GTFS in GTFS-DIR and STATIONS-FILE."
  (let* ((stations (train-time-mta-generate--stations stations-file))
         (names (let ((h (make-hash-table :test #'equal)))
                  (dolist (s stations) (puthash (car s) (nth 1 s) h))
                  h))
         (routes (train-time-mta-generate--routes gtfs-dir))
         (route-ids (mapcar #'car routes))
         (found (train-time-mta-generate--patterns
                 gtfs-dir
                 (train-time-mta-generate--trips gtfs-dir)
                 (train-time-mta-generate--service-weights gtfs-dir)))
         (station-routes (nth 1 found))
         (segments (let (result)
                     (maphash (lambda (key samples)
                                (push (append key
                                              (list (train-time-mta-generate--weighted-median
                                                     samples)
                                                    (apply #'+ (mapcar #'cdr samples))))
                                      result))
                              (nth 2 found))
                     (sort result (lambda (a b) (string< (format "%S" (seq-take a 4))
                                                         (format "%S" (seq-take b 4)))))))
         (by-family (make-hash-table :test #'equal))
         (version (train-time-mta-generate--version gtfs-dir))
         (route-order (lambda (ids)
                        (sort (copy-sequence ids)
                              (lambda (a b)
                                (< (or (cl-position a route-ids :test #'equal) 999)
                                   (or (cl-position b route-ids :test #'equal) 999))))))
         lines)
    (maphash (lambda (key weight)
               (push (list (car key) (cdr key) weight)
                     (gethash (train-time-mta-generate--family (car key) route-ids)
                              by-family)))
             (car found))
    (dolist (route routes)
      (let ((family (car route)))
        (when (gethash family by-family)
          (pcase-let* ((`(,chosen ,total) (train-time-mta-generate--choose
                                           (gethash family by-family)))
                       (ordered (append
                                 (cl-remove-if-not (lambda (p) (equal (car p) family)) chosen)
                                 (cl-remove-if (lambda (p) (equal (car p) family)) chosen)))
                       (`(,rows . ,kept) (train-time-mta-generate--rows ordered)))
            (push (list family
                        :name (nth 2 route)
                        :color (nth 3 route)
                        :routes (funcall route-order (delete-dups (mapcar #'car kept)))
                        :rows rows
                        :columns
                        (mapcar (lambda (p)
                                  (list :route (car p)
                                        :from (gethash (car (nth 1 p)) names)
                                        :to (gethash (car (last (nth 1 p))) names)
                                        :share (/ (round (* 1000.0 (nth 2 p)) total) 10.0)
                                        :stops (nth 1 p)))
                                kept))
                  lines)))))
    (setq lines (nreverse lines))
    (with-temp-file output
      (let ((print-length nil) (print-level nil) (print-escape-nonascii nil))
        (insert ";;; train-time-mta-data.el --- Generated MTA subway data -*- lexical-binding: t; -*-\n\n"
                ";; Generated by scripts/train-time-mta-generate.el; do not edit.\n"
                ";; Sources: MTA static GTFS (gtfs_subway.zip, feed version\n;; "
                version ") and MTA Stations.csv.\n"
                ";; Data provided by the Metropolitan Transportation Authority (MTA).\n\n"
                ";;; Code:\n\n")
        (insert (format "(defconst train-time-mta-data-version %S\n  \"Static GTFS feed version the data was generated from.\")\n\n"
                        version))
        (insert "(defconst train-time-mta-data-routes\n  '(")
        (dolist (r routes)
          (insert (format "\n    %S" (seq-take r 5))))
        (insert ")\n  \"Routes: (ROUTE-ID SHORT-NAME LONG-NAME COLOR TEXT-COLOR).\")\n\n")
        (insert "(defconst train-time-mta-data-stations\n  '(")
        (dolist (s stations)
          (insert (format "\n    %S"
                          (append s (list (funcall route-order
                                                   (gethash (car s) station-routes)))))))
        (insert ")\n  \"Stations: (STOP-ID NAME LAT LON NORTH-LABEL SOUTH-LABEL COMPLEX-ID
DAYTIME-ROUTES ROUTES).  ROUTES lists every route scheduled to stop.\")\n\n")
        (insert "(defconst train-time-mta-data-lines\n  '(")
        (dolist (l lines)
          (insert (format "\n    (%S" (car l)))
          (cl-loop for (k v) on (cdr l) by #'cddr
                   unless (eq k :columns)
                   do (insert (format "\n     %S %S" k v)))
          (insert "\n     :columns (")
          (dolist (c (plist-get (cdr l) :columns))
            (insert (format "\n       %S" c)))
          (insert "))"))
        (insert ")\n  \"Lines: (LINE-ID :name :color :routes :rows :columns).
:rows orders the stations for drawing.  Each column is a stopping
pattern (:route :from :to :share :stops): stops in southbound order,
:share the percentage of the line's weekly scheduled trips.\")\n\n")
        (insert "(defconst train-time-mta-data-segments\n  '(")
        (dolist (s segments)
          (insert (format "\n    %S" s)))
        (insert ")\n  \"Scheduled run times: (ROUTE-ID DIRECTION FROM TO SECONDS TRIPS).
A train of ROUTE-ID going DIRECTION (`north' or `south') arrives at
station TO next after station FROM, typically SECONDS later (the
median over the schedule's trips).  TRIPS counts those trips per week.\")\n\n")
        (insert "(provide 'train-time-mta-data)\n;;; train-time-mta-data.el ends here\n")))
    (message "Wrote %s: %d stations, %d routes, %d lines"
             output (length stations) (length routes) (length lines))))

(defun train-time-mta-generate-batch ()
  "Batch entry point; arguments GTFS-DIR STATIONS-CSV OUTPUT."
  (pcase command-line-args-left
    (`(,dir ,stations ,output . ,_)
     (setq command-line-args-left nil)
     (train-time-mta-generate dir stations output))
    (_ (error "Usage: -f train-time-mta-generate-batch GTFS-DIR STATIONS-CSV OUTPUT"))))

(provide 'train-time-mta-generate)
;;; train-time-mta-generate.el ends here
