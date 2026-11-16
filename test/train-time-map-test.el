;;; train-time-map-test.el --- Tests for the network navigator -*- lexical-binding: t; -*-

;; Copyright (C) 2026  train-time.el contributors

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Deterministic tests for train-time-map; no network access.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'train-time-map)

(defmacro train-time-map-test--with-map (&rest body)
  "Open a fresh map at Flushing with an offline fetcher, then run BODY."
  (declare (indent 0))
  `(let ((train-time-fetch-function
          (lambda (_url callback) (funcall callback '(:error "offline"))))
         (train-time-auto-refresh nil)
         (inhibit-message t))
     (when (get-buffer "*train-time-map*")
       (kill-buffer "*train-time-map*"))
     (save-window-excursion
       (delete-other-windows)
       (train-time-map (train-time-mta-station "701"))
       (unwind-protect
           (progn ,@body)
         (dolist (b (buffer-list))
           (when (string-prefix-p "*train-time" (buffer-name b))
             (kill-buffer b)))))))

(defun train-time-map-test--line (name)
  "Return the map line for station NAME, without properties."
  (save-excursion
    (goto-char (point-min))
    (search-forward (concat "  " name "\n"))
    (buffer-substring-no-properties (line-beginning-position 0)
                                    (line-end-position 0))))

(defun train-time-map-test--where ()
  "Return (STATION-NAME . COLUMN) of the selection."
  (cons (train-time-station-name (train-time-map--selected-station))
        train-time-map--col))

(defun train-time-map-test--text ()
  (buffer-substring-no-properties (point-min) (point-max)))

(ert-deftest train-time-map-test-network ()
  (let* ((network (train-time-map--line-network "7"))
         (columns (plist-get network :columns))
         (count (lambda (bv) (cl-count t (append bv nil)))))
    (should (equal (plist-get network :line) "7"))
    (should (equal (plist-get network :title) "7  Flushing Local"))
    (should (= (length (plist-get network :stations)) 22))
    (should (equal (mapcar (lambda (c) (plist-get c :label)) columns)
                   '("7" "<7>" "<7>2")))
    (should (equal (mapcar (lambda (c) (plist-get c :service)) columns)
                   '(local express express)))
    (should (equal (mapcar (lambda (c) (funcall count (plist-get c :stops))) columns)
                   '(22 15 18)))
    (should (= (plist-get network :spacing) 5))
    ;; The <7> runs past 111 St (row 2) on the same tracks.
    (should (eq (aref (plist-get (aref columns 1) :passes) 2) 'skip))
    (should (eq (aref (plist-get (aref columns 1) :passes) 1) 'stop))))

(ert-deftest train-time-map-test-branches ()
  (let* ((network (train-time-map--line-network "A"))
         (columns (plist-get network :columns))
         (stations (plist-get network :stations))
         (row (lambda (id) (cl-position id stations :key #'train-time-station-id
                                        :test #'equal))))
    (should (equal (mapcar (lambda (c) (plist-get c :label)) columns)
                   '("A" "A2" "A3")))
    ;; No <A>: the A is neither local nor express.
    (should (null (plist-get (aref columns 0) :service)))
    (should (equal (plist-get (aref columns 0) :to) "Ozone Park-Lefferts Blvd"))
    (should (equal (plist-get (aref columns 1) :to) "Far Rockaway-Mott Av"))
    ;; The Far Rockaway A runs alongside the Lefferts branch, which
    ;; it does not serve at all...
    (should (eq (aref (plist-get (aref columns 1) :passes) (funcall row "A63")) 'jump))
    ;; ...but runs express past 163 St on the same tracks.
    (should (eq (aref (plist-get (aref columns 1) :passes) (funcall row "A10")) 'skip))
    ;; Only patterns taking a different route are described "via".
    (should (null (plist-get (aref columns 1) :via)))
    (should (null (plist-get (aref columns 2) :via)))))

(ert-deftest train-time-map-test-via ()
  (let ((columns (plist-get (train-time-map--line-network "F") :columns)))
    ;; Two F patterns share terminals but use the 53 St and 63 St tunnels.
    (should (equal (mapcar (lambda (c) (plist-get c :via)) columns)
                   '("Queens Plaza" "67 Av" "21 St-Queensbridge" nil)))))

(ert-deftest train-time-map-test-render ()
  (train-time-map-test--with-map
    (should (derived-mode-p 'train-time-map-mode))
    (should (equal (train-time-map-test--line "Flushing-Main St")
                   "  x----o----o  Flushing-Main St"))
    (should (equal (train-time-map-test--line "111 St") "  o    |    |  111 St"))
    (should (equal (train-time-map-test--line "74 St-Broadway")
                   "  o----|----o  74 St-Broadway"))
    (should (eq (char-after) ?x))
    (should (equal (get-text-property (point) 'train-time-map-node) '(0 . 0)))
    (let ((text (train-time-map-test--text)))
      (should (string-match-p "^  7  Flushing Local$" text))
      (should (string-match-p
               "^  <7>  Flushing-Main St → 34 St-Hudson Yards, 15 stops, 8.7% of trips$"
               text))
      (should (string-match-p "Services: 7 local, <7> express, <7>2 express" text))
      (should (string-match-p "Transfers: none" text))
      (should (string-match-p "Data: Metropolitan Transportation Authority" text))
      (should (string-match-p (regexp-quote train-time-mta-data-version) text)))))

(ert-deftest train-time-map-test-render-branches ()
  (train-time-map-test--with-map
    (train-time-map--show-line "A" nil)
    (should (equal (train-time-map-test--line "104 St") "  o   :   :  104 St"))
    (should (equal (train-time-map-test--line "Aqueduct Racetrack")
                   "      o---o  Aqueduct Racetrack"))
    (should (equal (train-time-map-test--line "Ozone Park-Lefferts Blvd")
                   "  o   :   :  Ozone Park-Lefferts Blvd"))))

(ert-deftest train-time-map-test-navigation ()
  (train-time-map-test--with-map
    (train-time-map-next 2)
    (should (equal (train-time-map-test--where) '("111 St" . 0)))
    (should (eq (char-after) ?x))
    ;; 111 St has no express stop; switching picks the nearest one.
    (train-time-map-right)
    (should (equal (train-time-map-test--where) '("Mets-Willets Point" . 1)))
    ;; Express skips the local-only stops.
    (train-time-map-next)
    (should (equal (train-time-map-test--where) '("Junction Blvd" . 1)))
    (train-time-map-left)
    (should (equal (train-time-map-test--where) '("Junction Blvd" . 0)))
    (train-time-map-previous)
    (should (equal (train-time-map-test--where) '("103 St-Corona Plaza" . 0)))
    (train-time-map-last)
    (should (equal (train-time-map-test--where) '("34 St-Hudson Yards" . 0)))
    (should-error (train-time-map-next) :type 'user-error)
    (should-error (train-time-map-left) :type 'user-error)
    (train-time-map-first)
    (should (equal (train-time-map-test--where) '("Flushing-Main St" . 0)))
    (should-error (train-time-map-previous) :type 'user-error)))

(ert-deftest train-time-map-test-keys ()
  (train-time-map-test--with-map
    (execute-kbd-macro (kbd "C-n C-n C-f C-n C-b C-p"))
    (should (equal (train-time-map-test--where) '("103 St-Corona Plaza" . 0))))
  (should (eq (lookup-key train-time-map-mode-map "L") #'train-time-map-choose-line))
  (should (eq (lookup-key train-time-map-mode-map "t") #'train-time-map-transfer)))

(ert-deftest train-time-map-test-starts-at-board-station ()
  (train-time-map-test--with-map
    (with-current-buffer (train-time-board (train-time-mta-station "725"))
      (train-time-map))
    (should (equal (train-time-map-test--where) '("Times Sq-42 St" . 0)))
    (should (equal (plist-get train-time-map--network :line) "7"))))

(ert-deftest train-time-map-test-switches-to-board-line ()
  (train-time-map-test--with-map
    ;; Coney Island is not on the 7; the map shows its first line.
    (with-current-buffer (train-time-board (train-time-mta-station "D43"))
      (train-time-map))
    (should (equal (plist-get train-time-map--network :line) "D"))
    (should (equal (train-time-map-test--where) '("Coney Island-Stillwell Av" . 0)))))

(ert-deftest train-time-map-test-choose-line ()
  (train-time-map-test--with-map
    (train-time-map-last)
    (train-time-map-previous)
    ;; Times Sq-42 St (7) is not on the N, so the N starts at its top.
    (train-time-map-choose-line "N")
    (should (equal (plist-get train-time-map--network :line) "N"))
    (should (equal (train-time-map-test--where) '("Astoria-Ditmars Blvd" . 0)))
    (should (string-match-p "^  N  Broadway Local$" (train-time-map-test--text)))
    ;; Lines at the selected station are offered first.
    (let (offered)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _)
                   (setq offered (mapcar #'car table))
                   "W  Broadway Local")))
        (call-interactively #'train-time-map-choose-line))
      (should (equal (seq-take offered 2) '("N  Broadway Local" "W  Broadway Local")))
      (should (equal (plist-get train-time-map--network :line) "W"))
      (should (equal (train-time-map-test--where) '("Astoria-Ditmars Blvd" . 0))))))

(ert-deftest train-time-map-test-transfer ()
  (train-time-map-test--with-map
    (should-error (train-time-map-transfer) :type 'user-error)
    (train-time-map-last)
    (train-time-map-previous)
    (let ((station (train-time-map--selected-station))
          offered)
      (should (equal (train-time-station-id station) "725"))
      (should (equal (sort (mapcar #'car (train-time-map--transfers station)) #'string<)
                     '("1" "2" "3" "A" "C" "E" "GS" "N" "Q" "R" "W")))
      (should (string-match-p
               "Transfers: N Q R W 1 2 3 S, A C E (42 St-Port Authority Bus Terminal)"
               (train-time-map-test--text)))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _)
                   (setq offered (mapcar #'car table))
                   "Q  Broadway Express  at Times Sq-42 St (N Q R W)")))
        (train-time-map-transfer))
      (should (member "1  Broadway - 7 Avenue Local  at Times Sq-42 St (1 2 3)" offered))
      (should (equal (plist-get train-time-map--network :line) "Q"))
      (should (equal (train-time-station-id (train-time-map--selected-station)) "R16"))
      ;; And back: the Q's only transfer to the 7 here is at 725.
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "7  Flushing Local  at Times Sq-42 St (7)")))
        (train-time-map-transfer))
      (should (equal (train-time-map-test--where) '("Times Sq-42 St" . 0))))))

(defun train-time-map-test--layout ()
  "Return the buffer names of the frame's windows, left/top first."
  (mapcar (lambda (w) (buffer-name (window-buffer w)))
          (window-list nil 'nomini (frame-first-window))))

(ert-deftest train-time-map-test-board-right ()
  (train-time-map-test--with-map
    (let ((train-time-map-board-display 'right)
          (map-window (selected-window)))
      (train-time-map-next 2)
      (train-time-map-open-board)
      (should (equal (train-time-map-test--layout)
                     '("*train-time-map*" "*train-time: 111 St (7)*")))
      (should (equal (buffer-name) "*train-time: 111 St (7)*"))
      (should (= (window-pixel-top (selected-window)) (window-pixel-top map-window)))
      (should (> (window-pixel-left (selected-window)) (window-pixel-left map-window)))
      ;; A second RET replaces the board instead of splitting again.
      (select-window map-window)
      (train-time-map-next)
      (train-time-map-open-board)
      (should (equal (train-time-map-test--layout)
                     '("*train-time-map*" "*train-time: 103 St-Corona Plaza*")))
      (quit-window)
      (should (equal (train-time-map-test--layout) '("*train-time-map*"))))))

(ert-deftest train-time-map-test-board-below ()
  (train-time-map-test--with-map
    (let ((train-time-map-board-display 'below)
          (map-window (selected-window)))
      (train-time-map-open-board)
      (should (equal (train-time-map-test--layout)
                     '("*train-time-map*" "*train-time: Flushing-Main St*")))
      (should (= (window-pixel-left (selected-window)) (window-pixel-left map-window)))
      (should (> (window-pixel-top (selected-window)) (window-pixel-top map-window))))))

(ert-deftest train-time-map-test-board-replace ()
  (train-time-map-test--with-map
    (let ((train-time-map-board-display 'replace))
      (train-time-map-open-board)
      (should (equal (train-time-map-test--layout)
                     '("*train-time: Flushing-Main St*")))
      (quit-window)
      (should (equal (train-time-map-test--layout) '("*train-time-map*"))))))

(provide 'train-time-map-test)
;;; train-time-map-test.el ends here
