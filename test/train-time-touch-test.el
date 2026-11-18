;;; train-time-touch-test.el --- Tests for narrow windows and taps -*- lexical-binding: t; -*-

;; Copyright (C) 2026  train-time.el contributors

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; The compact layout, the touch bar and mouse-1 bindings, which is
;; what a tap on a touch screen (Termux, Android) becomes.  Taps are
;; simulated with synthetic mouse-1 events at buffer positions.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'train-time-test)
(require 'train-time-alerts-test)
(require 'train-time-map-test)
(require 'train-time-commute-test)

(defun train-time-touch-test--tap (pos)
  "Run the mouse-1 binding at POS in the selected window, as a tap does."
  (let ((event (list 'mouse-1 (list (selected-window) pos '(0 . 0) 0))))
    (funcall (lookup-key (current-local-map) [mouse-1]) event)))

(defun train-time-touch-test--find (regexp)
  "Return the start of the first line matching REGEXP."
  (save-excursion
    (goto-char (point-min))
    (and (re-search-forward regexp nil t) (line-beginning-position))))

(defmacro train-time-touch-test--capture (&rest body)
  "Run BODY and return the messages it shows."
  (declare (indent 0))
  `(let ((messages nil))
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args)
                  (when fmt (push (apply #'format fmt args) messages)))))
       ,@body)
     (nreverse messages)))

;;;; Helpers

(ert-deftest train-time-touch-test-fill ()
  (should (equal (train-time--fill "  aaa bbb ccc\nddd eee" 9)
                 "  aaa bbb\n  ccc\nddd eee"))
  (should (equal (train-time--fill "aaa bbb ccc" 8 2) "  aaa\n  bbb\n  ccc"))
  ;; Blank lines stay empty and text properties survive.
  (let ((text (train-time--fill (concat (propertize "aaa" 'face 'bold) "\n\nb") 20)))
    (should (equal (substring-no-properties text) "aaa\n\nb"))
    (should (eq (get-text-property 0 'face text) 'bold))))

(ert-deftest train-time-touch-test-touch-bar ()
  (let ((train-time-touch-bar nil))
    (should (equal (train-time--touch-bar '(("Refresh" ignore))) "")))
  (let ((train-time-touch-bar 'auto)
        (system-type 'gnu/linux))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
      (should-not (train-time--touch-bar-p)))
    (cl-letf (((symbol-function 'display-graphic-p) #'ignore))
      (should (train-time--touch-bar-p))))
  (let ((train-time-touch-bar t)
        (called nil))
    (cl-letf (((symbol-function 'train-time--window-width) (lambda () 20)))
      (let ((bar (train-time--touch-bar
                  `(("One" ,(lambda () (interactive) (setq called 'one)))
                    ("Two" ignore)
                    ("Three" ignore)))))
        ;; " One   Two " fits in 20 columns, " Three " would reach the
        ;; last column, which a terminal keeps for its wrap indicator.
        (should (equal (substring-no-properties bar) "  One   Two \n  Three \n"))
        (with-temp-buffer
          (insert bar "text\n")
          (goto-char (point-min))
          (let ((button (next-button (point))))
            (should (button-get button 'follow-link))
            (should (button-get button 'train-time-touch-bar))
            (should (eq (button-get button 'face) 'train-time-touch-button))
            (button-activate button)
            (should (eq called 'one)))
          (goto-char (point-min))
          (train-time--skip-touch-bar)
          (should (looking-at "text")))))))

;;;; Arrivals board

(ert-deftest train-time-touch-test-compact-board ()
  (let ((train-time-compact-width 200))
    (train-time-test--with-station "701" #'train-time-test--fixture-fetch
        train-time-test--fixture-time
      (should train-time--compact)
      (should (equal (mapcar #'car tabulated-list-format) '("Line" "Destination" "Time" "In")))
      (should (cl-every (lambda (e) (= (length (cadr e)) 4)) tabulated-list-entries))
      ;; Even a single station is grouped by direction in narrow windows.
      (should (train-time--grouped-p))
      (should (cl-some (lambda (h) (string-match-p "Manhattan" h))
                       (train-time-test--headings)))
      ;; Nothing is wider than the window.
      (goto-char (point-min))
      (let ((width (window-body-width)))
        (while (not (eobp))
          (should (<= (string-width (buffer-substring (point) (line-end-position)))
                      width))
          (forward-line 1))))))

(ert-deftest train-time-touch-test-width-change ()
  (let ((train-time-compact-width 60))
    (train-time-test--with-station "701" #'train-time-test--fixture-fetch
        train-time-test--fixture-time
      (delete-other-windows)
      (should-not train-time--compact)
      (should (= (length tabulated-list-format) 9))
      ;; A narrower window redraws in the compact layout, as
      ;; `window-size-change-functions' does after redisplay.
      (split-window-right)
      (train-time--width-changed (selected-window))
      (should train-time--compact)
      (should (= train-time--width (window-body-width)))
      ;; Under 44 columns, as on a phone, the Time column goes.
      (should (< (window-body-width) 44))
      (should (equal (mapcar #'car tabulated-list-format) '("Line" "Destination" "In")))
      (should (cl-every (lambda (e) (= (length (cadr e)) 3)) tabulated-list-entries))
      (delete-other-windows)
      (train-time--width-changed (selected-window))
      (should-not train-time--compact))))

(ert-deftest train-time-touch-test-board-taps ()
  (let ((train-time-compact-width 200))
    (train-time-test--with-station "701" #'train-time-test--fixture-fetch
        train-time-test--fixture-time
      (should (eq (lookup-key train-time-mode-map [mouse-1]) #'train-time-mouse-select))
      ;; Tapping a train describes it.
      (let* ((row (train-time-touch-test--find "^ +7 +"))
             (messages (train-time-touch-test--capture
                         (train-time-touch-test--tap (+ row 2)))))
        (should (string-match-p "34 St-Hudson Yards" (car messages)))
        (should (string-match-p "Departs [0-9][0-9]:[0-9][0-9] (" (car messages)))
        ;; A narrow frame's echo area gets the short version.
        (should-not (string-match-p "Trip " (car messages))))
      ;; Tapping a heading folds its group; tapping again unfolds it.
      (let ((state (lambda ()
                     (cdr (assoc (get-text-property (point) 'train-time-group)
                                 train-time--group-states))))
            (seen nil))
        (dotimes (_ 2)
          (train-time-touch-test--tap (train-time-touch-test--find "Manhattan"))
          (push (funcall state) seen))
        (should (equal seen '(nil folded)))))))

(ert-deftest train-time-touch-test-describe-wide ()
  (train-time-test--with-station "701" #'train-time-test--fixture-fetch
      train-time-test--fixture-time
    (goto-char (point-min))
    (while (not (tabulated-list-get-id)) (forward-line 1))
    (let ((messages (train-time-touch-test--capture (train-time-describe-trip))))
      (should (string-match-p "Departs 20[0-9-]+ [0-9:]+ E[SD]T" (car messages)))
      (should (string-match-p "^Trip .*, platform 701S, train " (car messages))))))

(ert-deftest train-time-touch-test-echo ()
  (let ((train-time-compact-width 200))
    (let ((messages (train-time-touch-test--capture
                      (train-time--echo (concat (make-string 30 ?a) " "
                                                (make-string 60 ?b))))))
      (should (string-match-p "\\`a+\nb+\\'" (car messages)))))
  (let ((train-time-compact-width 0))
    (should (equal (train-time-touch-test--capture (train-time--echo "a b"))
                   '("a b")))))

(ert-deftest train-time-touch-test-phone-board ()
  "At 36 columns destinations are shown in full."
  (train-time-test--with-station "701" #'train-time-test--fixture-fetch
      train-time-test--fixture-time
    (delete-other-windows)
    (set-window-buffer (split-window-right 37) (get-buffer-create "*scratch*"))
    (train-time--width-changed (selected-window))
    (should (< (window-body-width) 40))
    (should (string-match-p " 7 +34 St-Hudson Yards +[0-9]+ min$" (train-time-test--text)))))

(ert-deftest train-time-touch-test-phone-heading ()
  "A long group heading wraps on a phone and keeps its count together."
  (with-temp-buffer
    (setq-local train-time--compact t)
    (setq-local train-time--width 36)
    (setq-local train-time--station (train-time-mta-station "725"))
    (let* ((heading (train-time--group-heading
                     (train-time-mta-station "A27") "Downtown & Brooklyn" nil 3 42))
           (lines (split-string heading "\n" t)))
      (should (> (length lines) 1))
      (dolist (line lines)
        (should (<= (string-width line) 35)))
      (should (string-match-p "^    " (cadr lines)))
      (should (string-match-p "3/42$" (car (last lines)))))))

(ert-deftest train-time-touch-test-case-insensitive-station ()
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_prompt table &rest _)
               (should completion-ignore-case)
               (car (all-completions "times sq-42 st (7" table)))))
    (should (equal (train-time-station-id (train-time--read-station)) "725")))
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _) "times sq-42 st (7)")))
    (should (equal (train-time-station-id (train-time--read-station)) "725"))))

(ert-deftest train-time-touch-test-board-bar ()
  (let ((train-time-touch-bar t)
        (train-time-compact-width 200))
    (train-time-test--with-station "701" #'train-time-test--fixture-fetch
        train-time-test--fixture-time
      ;; Point starts past the bar.
      (should (> (point) (point-min)))
      (should-not (text-property-not-all (line-beginning-position) (line-end-position)
                                         'train-time-touch-bar nil))
      (goto-char (point-min))
      (should (looking-at "  Refresh "))
      ;; No filter yet, so no Clear button.
      (should-not (string-match-p " Clear " (buffer-substring (point-min) (point-max))))
      (setq train-time--filters (list :direction "N"))
      (train-time--render)
      (should (save-excursion
                (goto-char (point-min))
                (search-forward " Clear " nil t)))
      (goto-char (point-min))
      (search-forward " Clear ")
      (push-button (1- (point)))
      (should-not train-time--filters)
      ;; The footer explains taps instead of keys.
      (should (string-match-p "Tap a heading" (train-time-test--text))))))

(ert-deftest train-time-touch-test-retry-button ()
  (train-time-test--with-station "701"
      (lambda (_url callback) (funcall callback '(:error "Request timed out")))
      train-time-test--fixture-time
    (goto-char (point-min))
    (should (search-forward "(g to retry)" nil t))
    (let ((button (button-at (1- (point)))))
      (should button)
      (should (button-get button 'follow-link)))))

;;;; Network map

(ert-deftest train-time-touch-test-map-taps ()
  (train-time-map-test--with-map
    (let ((train-time-map-board-display 'below))
      (should (eq (lookup-key train-time-map-mode-map [mouse-1])
                  #'train-time-map-mouse-select))
      ;; Tapping another station selects it.
      (let* ((line (train-time-touch-test--find "Mets-Willets Point"))
             (node (text-property-not-all line (save-excursion
                                                 (goto-char line)
                                                 (line-end-position))
                                          'train-time-map-node nil)))
        (should node)
        (train-time-touch-test--tap node)
        (should (equal (train-time-station-name (train-time-map--selected-station))
                       "Mets-Willets Point"))
        (should (= (length (window-list nil 'nomini)) 1))
        ;; Tapping it again opens its board.
        (train-time-touch-test--tap node)
        (should (get-buffer-window "*train-time: Mets-Willets Point*"))))))

(ert-deftest train-time-touch-test-map-legend-and-narrow ()
  (train-time-map-test--with-map
    (train-time-map--show-line "6" (train-time-mta-station "631"))
    (let* ((col train-time-map--col)
           (legend (save-excursion
                     (goto-char (point-min))
                     (let (match)
                       (while (and (not match)
                                   (setq match (text-property-search-forward
                                                'train-time-map-column col
                                                (lambda (col value)
                                                  (and value (/= col value))))))
                         nil)
                       (prop-match-beginning match))))
           (target (get-text-property legend 'train-time-map-column)))
      (should (/= target col))
      (train-time-touch-test--tap legend)
      (should (= train-time-map--col target)))
    ;; A map too narrow for a board beside it gets one below it.
    (let ((train-time-map-board-display 'right))
      (set-window-buffer (split-window-right) (get-buffer-create "*scratch*"))
      (train-time-map-open-board)
      (let ((map (get-buffer-window "*train-time-map*"))
            (board (get-buffer-window "*train-time: Grand Central-42 St*")))
        (should (= (nth 0 (window-edges map)) (nth 0 (window-edges board))))
        (should (> (nth 1 (window-edges board)) (nth 1 (window-edges map))))))))

;;;; Window layout

(defun train-time-touch-test--windows ()
  "Return the buffer names of the frame's windows, left/top first."
  (mapcar (lambda (w) (buffer-name (window-buffer w)))
          (window-list nil 'nomini (frame-first-window))))

(ert-deftest train-time-touch-test-layout-stack-on-phone ()
  ;; The batch frame, 80 columns, is too small to split.
  (train-time-map-test--with-map
    (let ((train-time-window-layout 'auto))
      (train-time-map-open-board)
      (should (equal (train-time-touch-test--windows)
                     '("*train-time: Flushing-Main St*")))
      (quit-window)
      (should (equal (train-time-touch-test--windows) '("*train-time-map*"))))))

(ert-deftest train-time-touch-test-layout-split-right ()
  (train-time-map-test--with-map
    (let ((train-time-window-layout 'auto)
          (train-time-split-width 40)
          (map (selected-window)))
      (train-time-map-open-board)
      (should (equal (train-time-touch-test--windows)
                     '("*train-time-map*" "*train-time: Flushing-Main St*")))
      (should (= (window-pixel-top (selected-window)) (window-pixel-top map)))
      ;; The map, opened from the board beside it, keeps its window.
      (train-time-map)
      (should (eq (selected-window) map))
      (should (= (length (window-list nil 'nomini)) 2)))))

(ert-deftest train-time-touch-test-layout-split-below ()
  (train-time-map-test--with-map
    (let ((train-time-window-layout 'auto)
          (train-time-split-height 8)
          (map (selected-window)))
      (train-time-map-open-board)
      (should (equal (train-time-touch-test--windows)
                     '("*train-time-map*" "*train-time: Flushing-Main St*")))
      (should (> (window-pixel-top (selected-window)) (window-pixel-top map)))
      ;; The board, short while offline, is fitted to its text.
      (should (>= (window-total-height map) 8))
      (should (>= (window-total-height (selected-window)) 8))
      (should (> (window-total-height map) (window-total-height (selected-window))))
      (quit-window)
      (should (equal (train-time-touch-test--windows) '("*train-time-map*"))))))

(ert-deftest train-time-touch-test-layout-alerts ()
  (train-time-alerts-test--with
    (train-time-test--with-station "725" #'train-time-alerts-test--fetch
        train-time-alerts-test--now
      (let ((board (current-buffer)))
        (switch-to-buffer board)
        (delete-other-windows)
        ;; Too small to split even below: alerts replace the board.
        (let ((train-time-window-layout 'auto)
              (train-time-split-height 8))
          (call-interactively #'train-time-alerts)
          (should (= (length (window-list nil 'nomini)) 1))
          (should (derived-mode-p 'train-time-alerts-mode))
          (quit-window)
          (should (eq (window-buffer) board)))
        (let ((train-time-window-layout 'stack))
          (call-interactively #'train-time-alerts)
          (should (= (length (window-list nil 'nomini)) 1))
          (quit-window))
        (let ((train-time-window-layout 'auto)
              (train-time-split-width 40))
          (call-interactively #'train-time-alerts)
          (should (equal (car (train-time-touch-test--windows)) (buffer-name board)))
          (should (derived-mode-p 'train-time-alerts-mode))
          (should (= (length (window-list nil 'nomini)) 2)))))))

;;;; Trip list

(ert-deftest train-time-touch-test-commute ()
  (let ((train-time-compact-width 200))
    (train-time-commute-test--with "701" "723" #'train-time-test--times-sq-fetch
      (should (equal (mapcar #'car tabulated-list-format)
                     '("Leave" "In" "Trains" "Change at" "Arrive")))
      (should (cl-every (lambda (e) (= (length (cadr e)) 5)) tabulated-list-entries))
      (should (eq (lookup-key train-time-commute-mode-map [mouse-1])
                  #'train-time-commute-mouse-select))
      (let ((messages (train-time-touch-test--capture
                        (train-time-touch-test--tap
                         (train-time-touch-test--find "^ 16:18")))))
        (should (string-match-p "Flushing-Main St → .* Grand Central-42 St" (car messages)))))))

;;;; Alerts

(ert-deftest train-time-touch-test-alert-tap ()
  (train-time-alerts-test--with
    (train-time-alerts '("7"))
    (should (eq (lookup-key train-time-alerts-mode-map [mouse-1])
                #'train-time-alerts-mouse-select))
    (should-not (string-match-p "We're making structural improvements"
                                (train-time-test--text)))
    (train-time-touch-test--tap (train-time-touch-test--find "Stops Skipped\\|Planned -"))
    (train-time-touch-test--tap (car (cdr (train-time-alerts--starts))))
    (should (string-match-p "We're making structural improvements"
                            (train-time-test--text)))
    ;; Tapping between alerts does nothing.
    (train-time-touch-test--tap (point-min))))

(ert-deftest train-time-touch-test-alert-focus ()
  (train-time-alerts-test--with
    (train-time-alerts '("7") "lmm:planned_work:31871")
    (should (equal (train-time-alert-id (get-text-property (point) 'train-time-alert))
                   "lmm:planned_work:31871"))
    (should (member "lmm:planned_work:31871" train-time-alerts--expanded))
    (should (string-match-p "We're making structural improvements"
                            (train-time-test--text)))))

(ert-deftest train-time-touch-test-board-alert-links ()
  (train-time-alerts-test--with
    (train-time (train-time-mta-station "701"))
    (goto-char (point-min))
    (search-forward "Stops Skipped")
    (let ((button (button-at (point))))
      (should button)
      ;; The link keeps the text's own faces.
      (should-not (eq (get-text-property (point) 'face) 'button))
      (button-activate button))
    (should (derived-mode-p 'train-time-alerts-mode))
    (should (equal train-time-alerts--routes
                   (with-current-buffer "*train-time: Flushing-Main St*"
                     (train-time--routes))))
    (let ((alert (get-text-property (point) 'train-time-alert)))
      (should alert)
      (should (string-match-p "Stops Skipped" (train-time-alert-type alert)))
      (should (member (train-time-alert-id alert) train-time-alerts--expanded)))
    ;; The planned count opens the list too.
    (with-current-buffer "*train-time: Flushing-Main St*"
      (goto-char (point-min))
      (search-forward "planned service changes")
      (should (button-at (point))))))

(ert-deftest train-time-touch-test-alerts-bar ()
  (let ((train-time-touch-bar t))
    (train-time-alerts-test--with
      (train-time-alerts '("7"))
      (goto-char (point-min))
      (should (looking-at "  Refresh +Details +Lines +Quit"))
      (should (string-match-p "Tap an alert" (train-time-test--text))))))

(provide 'train-time-touch-test)
;;; train-time-touch-test.el ends here
