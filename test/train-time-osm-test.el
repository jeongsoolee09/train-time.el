;;; train-time-osm-test.el --- Tests for the osm.el integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026  train-time.el contributors

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; osm.el is not needed: `osm-open' and `osm-goto' are stubbed, so
;; these tests cover the GPX we hand to osm.el and where its window
;; goes.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dom)
(require 'train-time-osm)

;; Batch Emacs is a text terminal, where the touch bar is on by default.
(setq train-time-touch-bar nil)

(defun train-time-osm-test--parse (gpx)
  (with-temp-buffer
    (insert gpx)
    (car (xml-parse-region (point-min) (point-max)))))

(ert-deftest train-time-osm-test-gpx ()
  (let* ((stations (list (train-time-mta-station "701") (train-time-mta-station "702")))
         (gpx (train-time-osm--gpx "7 & <7>" stations (list stations (last stations))))
         (dom (train-time-osm-test--parse gpx))
         (wpts (dom-by-tag dom 'wpt)))
    (should (eq (dom-tag dom) 'gpx))
    (should (= (length wpts) 2))
    (should (equal (mapcar (lambda (s) (length (dom-by-tag s 'trkpt)))
                           (dom-by-tag dom 'trkseg))
                   '(2 1)))
    (should (equal (dom-text (dom-by-tag (car wpts) 'name)) "Flushing-Main St"))
    (should (equal (dom-attr (car wpts) 'lat) "40.759600"))
    (should (equal (dom-attr (car wpts) 'lon) "-73.830030"))
    ;; Names are escaped.
    (should (string-match-p "7 &amp; &lt;7&gt;" gpx))))

(ert-deftest train-time-osm-test-line-gpx ()
  (let* ((dom (train-time-osm-test--parse (train-time-osm--line-gpx "A")))
         (wpts (dom-by-tag dom 'wpt))
         (segments (dom-by-tag dom 'trkseg)))
    ;; Every station drawn on the A, one segment per stopping pattern.
    (should (= (length wpts) 62))
    (should (equal (mapcar (lambda (s) (length (dom-by-tag s 'trkpt))) segments)
                   '(30 37 59)))
    (should (equal (dom-text (dom-by-tag (car wpts) 'name)) "Inwood-207 St")))
  (should (equal (train-time-osm--dataset-name "7") "7 Flushing Local"))
  ;; Slashes would make the file name a path.
  (should (equal (train-time-osm--dataset-name "F")
                 "F Queens Blvd Express-6 Av Local")))

(defvar train-time-osm-test--calls nil)

(defmacro train-time-osm-test--with-stub-osm (&rest body)
  "Run BODY with fake `osm-open' and `osm-goto' and no network."
  (declare (indent 0))
  `(let ((fake-feature (not (featurep 'osm)))
         (train-time-osm-test--calls nil)
         (train-time-fetch-function
          (lambda (_url callback) (funcall callback '(:error "offline"))))
         (train-time-auto-refresh nil)
         (inhibit-message t))
     (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
               ((symbol-function 'osm-open)
                (lambda (file)
                  (push (list 'open file) train-time-osm-test--calls)
                  (with-current-buffer (if (eq major-mode 'osm-mode)
                                           (current-buffer)
                                         (generate-new-buffer "*osm*"))
                    (setq major-mode 'osm-mode)
                    (pop-to-buffer (current-buffer)))))
               ((symbol-function 'osm-goto)
                (lambda (lat lon zoom)
                  (push (list 'goto lat lon zoom (eq major-mode 'osm-mode))
                        train-time-osm-test--calls)
                  (pop-to-buffer (current-buffer)))))
       (save-window-excursion
         (delete-other-windows)
         (when fake-feature (provide 'osm))
         (unwind-protect
             (progn ,@body)
           (when fake-feature (setq features (delq 'osm features)))
           (dolist (b (buffer-list))
             (when (or (string-prefix-p "*train-time" (buffer-name b))
                       (string-prefix-p "*osm" (buffer-name b)))
               (kill-buffer b))))))))

(defun train-time-osm-test--window (name)
  (get-buffer-window name))

(defun train-time-osm-test--osm-window ()
  (cl-find-if (lambda (w) (string-prefix-p "*osm" (buffer-name (window-buffer w))))
              (window-list nil 'nomini)))

(defun train-time-osm-test--left (w) (nth 0 (window-edges w)))
(defun train-time-osm-test--top (w) (nth 1 (window-edges w)))

(ert-deftest train-time-osm-test-map-and-board-side-by-side ()
  "Map | board: OSM goes below the board, in the bottom-right corner."
  (train-time-osm-test--with-stub-osm
    (let ((train-time-map-board-display 'right))
      (train-time-map (train-time-mta-station "701"))
      (train-time-map-open-board)
      (train-time-osm (train-time-mta-station "701"))
      (let ((map (train-time-osm-test--window "*train-time-map*"))
            (board (train-time-osm-test--window "*train-time: Flushing-Main St*"))
            (osm (train-time-osm-test--osm-window)))
        (should (= (length (window-list nil 'nomini)) 3))
        (should (eq (selected-window) osm))
        (should (= (train-time-osm-test--left osm) (train-time-osm-test--left board)))
        (should (> (train-time-osm-test--top osm) (train-time-osm-test--top board)))
        (should (> (train-time-osm-test--left osm) (train-time-osm-test--left map)))
        (should (window-at-side-p osm 'bottom))
        (should (window-at-side-p osm 'right))))
    (should (equal (car train-time-osm-test--calls)
                   (list 'goto 40.7596 -73.83003 train-time-osm-zoom t)))
    (should (equal (cadr train-time-osm-test--calls)
                   (list 'open "7 Flushing Local")))))

(ert-deftest train-time-osm-test-map-and-board-stacked ()
  "Map over board: OSM goes right of the board, in the bottom-right corner."
  (train-time-osm-test--with-stub-osm
    (let ((train-time-map-board-display 'below))
      (train-time-map (train-time-mta-station "701"))
      (train-time-map-open-board)
      (select-window (get-buffer-window "*train-time-map*"))
      (train-time-osm (train-time-mta-station "701"))
      (let ((board (train-time-osm-test--window "*train-time: Flushing-Main St*"))
            (osm (train-time-osm-test--osm-window)))
        (should (= (length (window-list nil 'nomini)) 3))
        (should (= (train-time-osm-test--top osm) (train-time-osm-test--top board)))
        (should (> (train-time-osm-test--left osm) (train-time-osm-test--left board)))
        (should (window-at-side-p osm 'bottom))
        (should (window-at-side-p osm 'right))))))

(ert-deftest train-time-osm-test-only-map ()
  (train-time-osm-test--with-stub-osm
    (train-time-map (train-time-mta-station "701"))
    (train-time-map-next 3)
    (call-interactively #'train-time-osm)
    (let ((map (train-time-osm-test--window "*train-time-map*"))
          (osm (train-time-osm-test--osm-window)))
      (should (= (length (window-list nil 'nomini)) 2))
      (should (= (train-time-osm-test--top osm) (train-time-osm-test--top map)))
      (should (> (train-time-osm-test--left osm) (train-time-osm-test--left map))))
    ;; Interactively, the map's selection is used: 103 St-Corona Plaza.
    (should (equal (cl-subseq (car train-time-osm-test--calls) 0 3)
                   '(goto 40.749865 -73.8627)))))

(ert-deftest train-time-osm-test-only-board ()
  (train-time-osm-test--with-stub-osm
    (train-time (train-time-mta-station "725"))
    (call-interactively #'train-time-osm)
    (let ((board (train-time-osm-test--window "*train-time: Times Sq-42 St*"))
          (osm (train-time-osm-test--osm-window)))
      (should (= (length (window-list nil 'nomini)) 2))
      (should (= (train-time-osm-test--top osm) (train-time-osm-test--top board)))
      (should (> (train-time-osm-test--left osm) (train-time-osm-test--left board))))
    (should (equal (cl-subseq (car train-time-osm-test--calls) 0 3)
                   '(goto 40.755477 -73.987691)))
    (should (equal (cadr train-time-osm-test--calls) '(open "7 Flushing Local")))))

(ert-deftest train-time-osm-test-uses-map-line ()
  (train-time-osm-test--with-stub-osm
    (train-time-map (train-time-mta-station "701"))
    ;; Times Sq-42 St on the N: the N is drawn, not the station's first line.
    (train-time-map--show-line "N" (train-time-mta-station "R16"))
    (call-interactively #'train-time-osm)
    (should (equal (cadr train-time-osm-test--calls) '(open "N Broadway Local")))
    (should (file-exists-p (expand-file-name "N Broadway Local"
                                             (train-time-osm--directory))))
    ;; From a board, the station's first line.
    (should (equal (with-temp-buffer
                     (train-time-osm--line-for (train-time-mta-station "D43")))
                   "D"))))

(ert-deftest train-time-osm-test-reuses-osm-window ()
  (train-time-osm-test--with-stub-osm
    (train-time-map (train-time-mta-station "701"))
    (train-time-osm (train-time-mta-station "701"))
    (let ((osm (selected-window)))
      (select-window (get-buffer-window "*train-time-map*"))
      (train-time-osm (train-time-mta-station "725"))
      (should (eq (selected-window) osm))
      (should (= (length (window-list nil 'nomini)) 2))
      (should (= (cl-count-if (lambda (b) (string-prefix-p "*osm" (buffer-name b)))
                              (buffer-list))
                 1)))))

(ert-deftest train-time-osm-test-url ()
  (let ((station (train-time-mta-station "701"))
        (train-time-osm-zoom 16))
    (should (equal (train-time-osm-url station)
                   (concat "https://www.openstreetmap.org/?mlat=40.759600&mlon=-73.830030"
                           "#map=16/40.759600/-73.830030")))
    (should (equal (train-time-osm-url station 'geo)
                   (concat "geo:40.759600,-73.830030?q=40.759600,-73.830030"
                           "(Flushing-Main%20St)")))))

(ert-deftest train-time-osm-test-terminal-browses ()
  "In a text terminal, the station opens in a browser, not osm.el."
  (let ((opened nil)
        (inhibit-message t)
        (train-time-osm-terminal 'web))
    (cl-letf (((symbol-function 'display-graphic-p) #'ignore)
              ((symbol-function 'executable-find) #'ignore)
              ((symbol-function 'browse-url) (lambda (url &rest _) (push url opened)))
              ((symbol-function 'osm-open) (lambda (_) (error "Should not be called"))))
      (train-time-osm (train-time-mta-station "725"))
      (should (equal opened (list (train-time-osm-url (train-time-mta-station "725"))))))))

(ert-deftest train-time-osm-test-termux-open-url ()
  "On Termux, termux-open-url hands the URI to Android."
  (let ((calls nil)
        (inhibit-message t)
        (train-time-osm-terminal 'geo)
        (browse-url-browser-function #'browse-url-default-browser))
    (cl-letf (((symbol-function 'display-graphic-p) #'ignore)
              ((symbol-function 'executable-find)
               (lambda (name &rest _)
                 (and (equal name "termux-open-url") "/data/bin/termux-open-url")))
              ((symbol-function 'call-process)
               (lambda (program _in _out _display &rest args)
                 (push (cons program args) calls)
                 0))
              ((symbol-function 'browse-url) (lambda (&rest _) (error "Should not be called"))))
      (train-time-osm (train-time-mta-station "701"))
      (should (equal calls `(("/data/bin/termux-open-url"
                              ,(train-time-osm-url (train-time-mta-station "701") 'geo))))))
    ;; A customized browser is respected.
    (let ((browse-url-browser-function #'eww-browse-url)
          (browsed nil))
      (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/data/bin/x"))
                ((symbol-function 'browse-url) (lambda (url &rest _) (setq browsed url))))
        (train-time-osm--browse "https://example.org/")
        (should (equal browsed "https://example.org/"))))))

(ert-deftest train-time-osm-test-keys ()
  (should (eq (lookup-key train-time-mode-map "m") #'train-time-osm))
  (should (eq (lookup-key train-time-map-mode-map "m") #'train-time-osm)))

(provide 'train-time-osm-test)
;;; train-time-osm-test.el ends here
