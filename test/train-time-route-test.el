;;; train-time-route-test.el --- Tests for journey planning -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'train-time-test)
(require 'train-time-route)

(defmacro train-time-route-test--with-defaults (&rest body)
  "Run BODY with the default planning settings and no touch bar."
  (declare (indent 0) (debug t))
  `(let ((train-time-route-daytime-only t)
         (train-time-route-min-trips 10)
         (train-time-route-change-penalty 300)
         (train-time-transfer-time 180)
         (train-time-touch-bar nil)
         (train-time-show-alerts nil)
         (train-time-evil-bindings nil))
     ,@body))

(defun train-time-route-test--plan (from to)
  "Return the plan between stop IDs FROM and TO."
  (train-time-route-plan (train-time-mta-station from) (train-time-mta-station to)))

(defun train-time-route-test--legs (plan)
  "Return PLAN's legs as (ROUTE DIRECTION FROM TO) lists."
  (mapcar (lambda (leg)
            (list (train-time-route-leg-route-id leg)
                  (train-time-route-leg-direction leg)
                  (train-time-station-id (train-time-route-leg-from leg))
                  (train-time-station-id (train-time-route-leg-to leg))))
          (train-time-route-plan-legs plan)))

(ert-deftest train-time-route-test-heap ()
  (let ((heap (cons (make-vector 1 nil) 0))
        (out nil))
    (dolist (n '(5 3 9 1 7 3 8 2))
      (train-time-route--heap-push heap n n))
    (while (> (cdr heap) 0)
      (push (car (train-time-route--heap-pop heap)) out))
    (should (equal (nreverse out) '(1 2 3 3 5 7 8 9)))
    (should-not (train-time-route--heap-pop heap))))

(ert-deftest train-time-route-test-segments-data ()
  (should (> (length train-time-mta-data-segments) 1000))
  ;; The <7> runs from Flushing to 61 St in one hop, the 7 stops on the way.
  (should (assoc-default "701" (mapcar (lambda (s) (cons (nth 2 s) s))
                                       (cl-remove-if-not
                                        (lambda (s) (equal (car s) "7X"))
                                        train-time-mta-data-segments))))
  (dolist (segment train-time-mta-data-segments)
    (should (memq (nth 1 segment) '(north south)))
    (should (natnump (nth 4 segment)))))

(ert-deftest train-time-route-test-one-train ()
  (train-time-route-test--with-defaults
    (let* ((plan (train-time-route-test--plan "701" "725"))
           (leg (car (train-time-route-plan-legs plan))))
      (should (equal (train-time-route-test--legs plan) '(("7" south "701" "725"))))
      ;; The rush-hour express is offered, but the all-day local is the plan.
      (should (equal (train-time-route-leg-alternatives leg) '("7X")))
      (should (= (train-time-route-leg-stops leg) 20))
      (should (< 1500 (train-time-route-plan-seconds plan) 3000)))))

(ert-deftest train-time-route-test-change-penalty ()
  (train-time-route-test--with-defaults
    ;; Ignoring waits, hopping between the 7 and the F saves a minute.
    (let ((train-time-route-change-penalty 0))
      (should (> (length (train-time-route-plan-legs
                          (train-time-route-test--plan "701" "725")))
                 1)))))

(ert-deftest train-time-route-test-walk-within-complex ()
  (train-time-route-test--with-defaults
    (let ((plan (train-time-route-test--plan "G22" "L08")))
      ;; Metropolitan Av (G) and Lorimer St (L) form one complex.
      (should (equal (train-time-route-test--legs plan)
                     '(("G" south "G22" "G29") ("L" north "L10" "L08"))))
      (should (= (train-time-route-plan-seconds plan)
                 (+ 180 (apply #'+ (mapcar #'train-time-route-leg-seconds
                                           (train-time-route-plan-legs plan)))))))))

(ert-deftest train-time-route-test-branches ()
  (train-time-route-test--with-defaults
    ;; From one Rockaway branch to the Lefferts branch the A has to be
    ;; ridden north, then south: one train cannot turn back.
    (should (equal (train-time-route-test--legs (train-time-route-test--plan "H15" "A65"))
                   '(("A" north "H15" "A61") ("A" south "A61" "A65"))))))

(ert-deftest train-time-route-test-daytime-only ()
  (train-time-route-test--with-defaults
    (let ((rides (car (train-time-route--graph))))
      ;; The 4 calls at Kingston Av only late at night.
      (should-not (gethash '("249" "4" south) rides))
      (should (gethash '("249" "3" south) rides))
      (let ((train-time-route-daytime-only nil))
        (should (gethash '("249" "4" south) (car (train-time-route--graph))))))))

(ert-deftest train-time-route-test-unreachable ()
  (train-time-route-test--with-defaults
    (should-not (train-time-route-test--plan "S31" "101"))
    (should (train-time-route-test--plan "S31" "S09"))))

(ert-deftest train-time-route-test-buffer ()
  (train-time-route-test--with-defaults
    (let (opened)
      (cl-letf (((symbol-function 'train-time-board)
                 (lambda (station) (setq opened (train-time-station-id station))))
                ((symbol-function 'train-time--display) #'ignore))
        (with-current-buffer (train-time-route (train-time-mta-station "G22")
                                               (train-time-mta-station "L08"))
          (unwind-protect
              (let ((text (buffer-string)))
                (should (string-match-p "About 14 min, 1 change" text))
                (should (string-match-p "Change: walk to Lorimer St" text))
                (should (string-match-p "waiting for[[:space:]]+trains[[:space:]]+is not included" text))
                ;; Point starts on the first leg; RET opens its station.
                (should (equal (train-time-route-leg-route-id
                                (get-text-property (point) 'train-time-route-leg))
                               "G"))
                (train-time-route-board)
                (should (equal opened "G22"))
                (train-time-route-next-leg)
                (should (equal (train-time-route-leg-route-id
                                (get-text-property (point) 'train-time-route-leg))
                               "L"))
                (train-time-route-previous-leg)
                (should (equal (train-time-route-leg-route-id
                                (get-text-property (point) 'train-time-route-leg))
                               "G"))
                ;; A station's name opens that station.
                (goto-char (point-min))
                (goto-char (search-forward "Metropolitan Av" nil nil))
                (backward-char)
                (train-time-route-board)
                (should (equal opened "G29"))
                (train-time-route-reverse)
                (should (string-match-p "Bedford Av → Court Sq"
                                        (buffer-substring-no-properties
                                         (point-min) (point-max)))))
            (kill-buffer)))))))

(ert-deftest train-time-route-test-same-complex ()
  (train-time-route-test--with-defaults
    (should-error (train-time-route (train-time-mta-station "G29")
                                    (train-time-mta-station "L10"))
                  :type 'user-error)))

(ert-deftest train-time-route-test-touch-bar ()
  (train-time-route-test--with-defaults
    (let ((train-time-touch-bar t))
      (with-current-buffer (train-time-route (train-time-mta-station "701")
                                             (train-time-mta-station "725"))
        (unwind-protect
            (progn
              (goto-char (point-min))
              (should (string-match-p "Reverse.*New.*Board"
                                      (buffer-substring (point) (line-end-position))))
              (should (string-match-p "Tap a leg or station" (buffer-string))))
          (kill-buffer))))))

(provide 'train-time-route-test)
;;; train-time-route-test.el ends here
