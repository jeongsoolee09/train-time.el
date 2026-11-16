;;; train-time-map.el --- Keyboard-driven transit network navigator -*- lexical-binding: t; -*-

;; Copyright (C) 2026  train-time.el contributors

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; M-x train-time-map draws a line as a text diagram in the style of
;; `undo-tree-visualize': stations run down the buffer and each of the
;; line's main stopping patterns (local, express, branches) is a
;; column.  A node `o' means the pattern stops there; `x' is the
;; selected node.  Between stops, `|' passes stations the pattern
;; skips and `:' runs alongside another branch.
;;
;;   C-n / C-p   next / previous stop on the selected service
;;   C-f / C-b   switch to the service on the right / left
;;   RET         open the arrivals board for the selected station
;;   L           show another line
;;   t           transfer to another line at this station or complex
;;   m           show the selected station on OpenStreetMap (osm.el)
;;   q           quit
;;
;; Evil users get h/j/k/l in normal state.  Where the board appears is
;; controlled by `train-time-map-board-display'.

;;; Code:

(require 'cl-lib)
(require 'train-time)
(require 'train-time-mta)

(defcustom train-time-map-board-display 'right
  "Where RET in the network map shows the selected station's board.
`right' splits the map window and shows the board to its right,
`below' splits it and shows the board underneath, and `replace'
shows the board in the map's own window (\\[quit-window] returns)."
  :type '(choice (const :tag "Split, board on the right" right)
                 (const :tag "Split, board below" below)
                 (const :tag "Replace the map window" replace))
  :group 'train-time)

(defface train-time-map-selected
  '((((class color)) :foreground "red" :weight bold)
    (t :inverse-video t))
  "Face for the selected node, like undo-tree's current node.")

(defface train-time-map-selected-station '((t :weight bold :underline t))
  "Face for the selected station's name.")

(defface train-time-map-help '((t :inherit shadow))
  "Face for the key reminder at the bottom of the map.")

(defconst train-time-map--buffer-name "*train-time-map*")
(defconst train-time-map--margin 2 "Columns left of the first service.")

;;;; Network

;; A network is a plist:
;;   :line     line ID, e.g. "A"
;;   :title    string
;;   :color    line color
;;   :stations vector of `train-time-station', in drawing order
;;   :columns  vector of plists (:label :route :service :color :stops
;;             :passes :from :to :count :share :via), where :stops is a
;;             bool-vector indexed like :stations and :passes is a
;;             vector of `stop', `skip', `jump' or nil per station; see
;;             `train-time-map--passes'.
;;   :spacing  columns between two services

(defun train-time-map--passes (stops columns)
  "Return the :passes vector of a column stopping at STOPS.
STOPS is a `bool-vector' indexed by row.
Each row is `stop', `skip' when the column runs past it on the
same tracks (another column stops there and at the stops either
side), `jump' when the column bypasses it on another route, or nil
outside the column's span.  COLUMNS are every column's bool-vector."
  (let* ((n (length stops))
         (passes (make-vector n nil))
         (rows (cl-loop for r below n when (aref stops r) collect r)))
    (dolist (r rows) (aset passes r 'stop))
    (cl-loop for (a b) on rows while b
             do (cl-loop for r from (1+ a) below b
                         do (aset passes r
                                  (if (cl-some (lambda (other)
                                                 (and (not (eq other stops))
                                                      (aref other a) (aref other r)
                                                      (aref other b)))
                                               columns)
                                      'skip 'jump))))
    passes))

(defun train-time-map--via (column columns)
  "Return the name of a stop telling COLUMN's route apart, or nil.
COLUMN is compared with the first other column in COLUMNS that has
the same terminals, or, if COLUMN is that first one, with each of
the others.  It is described as going \"via\" its first stop the
other lacks only if each also has stops the other lacks: a pattern
that merely makes fewer stops is express, not a different route."
  (let* ((twins (cl-remove-if-not
                 (lambda (c) (and (equal (plist-get c :from) (plist-get column :from))
                                  (equal (plist-get c :to) (plist-get column :to))))
                 columns))
         (stops (plist-get column :stops))
         (differs (lambda (other)
                    (let ((theirs (plist-get other :stops)))
                      (and (cl-set-difference theirs stops :test #'equal)
                           (cl-find-if (lambda (s) (not (member s theirs))) stops)))))
         (id (if (eq (car twins) column)
                 (cl-some differs (cdr twins))
               (funcall differs (car twins)))))
    (and id (train-time-station-name (train-time-mta-station id)))))

(defun train-time-map--line-network (line-id)
  "Return the network for MTA line LINE-ID."
  (let* ((line (or (train-time-mta-line line-id)
                   (error "Unknown line %s" line-id)))
         (rows (plist-get line :rows))
         (stations (vconcat (mapcar #'train-time-mta-station rows)))
         (data (plist-get line :columns))
         (seen nil)
         (masks (mapcar (lambda (c)
                          (let ((ids (plist-get c :stops)))
                            (apply #'bool-vector
                                   (mapcar (lambda (r) (and (member r ids) t)) rows))))
                        data))
         (columns
          (cl-mapcar
           (lambda (c mask)
             (let* ((route (plist-get c :route))
                    (bullet (train-time-mta-route-bullet route))
                    (n (cl-incf (alist-get bullet seen 0 nil #'equal))))
               (list :label (if (= n 1) bullet (format "%s%d" bullet n))
                     :route route
                     :service (cdr (train-time-mta--route route))
                     :color (train-time-mta-route-color route)
                     :stops mask
                     :passes (train-time-map--passes mask masks)
                     :from (plist-get c :from)
                     :to (plist-get c :to)
                     :count (length (plist-get c :stops))
                     :share (plist-get c :share)
                     :via (train-time-map--via c data))))
           data masks)))
    (list :line line-id
          :title (train-time-mta-line-title line-id)
          :color (plist-get line :color)
          :stations stations
          :columns (vconcat columns)
          :spacing (max 4 (1+ (apply #'max (mapcar (lambda (c)
                                                     (length (plist-get c :label)))
                                                   columns)))))))

;;;; Buffer state

(defvar-local train-time-map--network nil "Network drawn in this buffer.")
(defvar-local train-time-map--row 0 "Row (station index) of the selection.")
(defvar-local train-time-map--col 0 "Column (service index) of the selection.")
(defvar-local train-time-map--board-window nil
  "Window last used to show an arrivals board from this map.")

(defun train-time-map--stations ()
  (plist-get train-time-map--network :stations))

(defun train-time-map--columns ()
  (plist-get train-time-map--network :columns))

(defun train-time-map--stops-p (row col)
  "Return non-nil if service COL stops at station ROW."
  (let ((columns (train-time-map--columns)))
    (and (< -1 col (length columns))
         (< -1 row (length (train-time-map--stations)))
         (aref (plist-get (aref columns col) :stops) row))))

(defun train-time-map--span (col)
  "Return (FIRST . LAST) rows where service COL stops."
  (let ((stops (plist-get (aref (train-time-map--columns) col) :stops))
        first last)
    (dotimes (i (length stops))
      (when (aref stops i)
        (unless first (setq first i))
        (setq last i)))
    (cons first last)))

(defun train-time-map--pass (row col)
  "Return how service COL passes station ROW; see `train-time-map--passes'."
  (aref (plist-get (aref (train-time-map--columns) col) :passes) row))

(defun train-time-map--link (row col)
  "Return how service COL travels from station ROW to the next one.
The result is `skip' along shared tracks, `jump' on another route,
or nil where the service does not run."
  (let ((here (train-time-map--pass row col))
        (next (train-time-map--pass (1+ row) col)))
    (cond ((not (and here next)) nil)
          ((and (memq here '(stop skip)) (memq next '(stop skip))) 'skip)
          (t 'jump))))

(defun train-time-map--selected-station ()
  "Return the station of the selected node."
  (aref (train-time-map--stations) train-time-map--row))

;;;; Rendering

(defun train-time-map--spacing ()
  "Return the number of characters between two columns."
  (plist-get train-time-map--network :spacing))

(defun train-time-map--column-x (col)
  (+ train-time-map--margin (* col (train-time-map--spacing))))

(defun train-time-map--line-face (col)
  (let ((color (plist-get (aref (train-time-map--columns) col) :color)))
    (and color `(:foreground ,color))))

(defun train-time-map--glyph (kind col)
  "Return the glyph drawing service COL passing a station or gap of KIND."
  (pcase kind
    ('skip (propertize "|" 'face (train-time-map--line-face col)))
    ('jump (propertize ":" 'face (train-time-map--line-face col)))
    (_ " ")))

(defun train-time-map--insert-columns-row (fn)
  "Insert one diagram row, calling FN with each column to get its cell.
FN returns a string of width 1; gaps between columns are FN called
with (COL . COL+1) and must return a string of the gap width."
  (let ((ncols (length (train-time-map--columns))))
    (insert (make-string train-time-map--margin ?\s))
    (dotimes (col ncols)
      (insert (funcall fn col))
      (when (< col (1- ncols))
        (insert (funcall fn (cons col (1+ col))))))))

(defun train-time-map--connected-p (row gap)
  "Return non-nil if services on both sides of GAP stop at station ROW.
GAP is (COL . COL+1)."
  (let ((ncols (length (train-time-map--columns))))
    (and (cl-loop for c from (car gap) downto 0
                  thereis (train-time-map--stops-p row c))
         (cl-loop for c from (cdr gap) below ncols
                  thereis (train-time-map--stops-p row c)))))

(defun train-time-map--station-row (row)
  "Insert the diagram row for station ROW."
  (let* ((width (1- (train-time-map--spacing)))
         (selected-row (= row train-time-map--row))
         (first-stop (cl-loop for c below (length (train-time-map--columns))
                              when (train-time-map--stops-p row c) return c)))
    (train-time-map--insert-columns-row
     (lambda (cell)
       (cond
        ((consp cell)
         (if (train-time-map--connected-p row cell)
             (propertize (make-string width ?-)
                         'face (train-time-map--line-face first-stop))
           (make-string width ?\s)))
        ((train-time-map--stops-p row cell)
         (let ((selected (and selected-row (= cell train-time-map--col))))
           (propertize (if selected "x" "o")
                       'face (if selected
                                 'train-time-map-selected
                               (train-time-map--line-face cell))
                       'train-time-map-node (cons row cell)
                       'help-echo (plist-get (aref (train-time-map--columns) cell)
                                             :label))))
        (t (train-time-map--glyph (train-time-map--pass row cell) cell)))))
    (insert "  "
            (propertize (train-time-station-name (aref (train-time-map--stations) row))
                        'face (and selected-row 'train-time-map-selected-station)
                        'train-time-map-node (cons row nil))
            "\n")))

(defun train-time-map--link-row (row)
  "Insert the connector row between station ROW and the next one."
  (let ((gap (make-string (1- (train-time-map--spacing)) ?\s)))
    (train-time-map--insert-columns-row
     (lambda (cell)
       (if (consp cell) gap
         (train-time-map--glyph (train-time-map--link row cell) cell))))
    (delete-char (- (skip-chars-backward " ")))
    (insert "\n")))

(defun train-time-map--column-face (col)
  "Return the face of service COL's label."
  (let ((face (train-time-map--line-face col)))
    (if face (append face '(:weight bold)) 'bold)))

(defun train-time-map--labels-row ()
  "Insert the service labels centered over their columns."
  (let ((columns (train-time-map--columns))
        (bol (line-beginning-position)))
    (dotimes (col (length columns))
      (let* ((label (plist-get (aref columns col) :label))
             (x (max (- (point) bol)
                     (- (train-time-map--column-x col) (/ (1- (length label)) 2)))))
        (insert (make-string (- x (- (point) bol)) ?\s)
                (propertize label 'face (train-time-map--column-face col)))))
    (insert "\n")))

(defun train-time-map--describe-column (col)
  "Return a one-line description of service COL."
  (let ((c (aref (train-time-map--columns) col)))
    (format "%s → %s%s, %d stops, %s%% of trips"
            (plist-get c :from) (plist-get c :to)
            (if (plist-get c :via) (format " via %s" (plist-get c :via)) "")
            (plist-get c :count)
            (let ((share (plist-get c :share)))
              (if (< share 10) (format "%.1f" share) (format "%d" (round share)))))))

(defun train-time-map--insert-legend ()
  "Insert one line per service describing its stopping pattern."
  (let* ((columns (train-time-map--columns))
         (width (apply #'max (mapcar (lambda (c) (length (plist-get c :label)))
                                     columns))))
    (dotimes (col (length columns))
      (let ((label (plist-get (aref columns col) :label))
            (text (train-time-map--describe-column col)))
        (insert "  "
                (propertize label 'face (train-time-map--column-face col))
                (make-string (- (1+ width) (length label)) ?\s)
                (if (= col train-time-map--col)
                    (propertize text 'face 'bold)
                  text)
                "\n")))))

(defun train-time-map--services-at (row)
  "Return a description of the services stopping at station ROW."
  (let ((columns (train-time-map--columns)) result)
    (dotimes (col (length columns))
      (when (train-time-map--stops-p row col)
        (let ((c (aref columns col)))
          (push (if (plist-get c :service)
                    (format "%s %s" (plist-get c :label) (plist-get c :service))
                  (plist-get c :label))
                result))))
    (string-join (nreverse result) ", ")))

(defun train-time-map--bullet (line-id)
  "Return the colored bullet of LINE-ID."
  (let ((color (train-time-mta-route-color line-id)))
    (propertize (train-time-mta-route-bullet line-id)
                'face (if color `(:foreground ,color :weight bold) 'bold))))

(defun train-time-map--transfers (station)
  "Return a list of (LINE-ID . STATION) transfers available at STATION.
These are the other lines stopping at STATION or at a station in
the same complex."
  (let ((line (plist-get train-time-map--network :line))
        result)
    (dolist (s (train-time-mta-complex station))
      (dolist (id (train-time-mta-line-ids s))
        (unless (and (equal id line) (eq s station))
          (push (cons id s) result))))
    (nreverse result)))

(defun train-time-map--describe-transfers (station)
  "Describe the transfers available at STATION."
  (let ((groups nil))
    (dolist (tr (train-time-map--transfers station))
      (let ((name (train-time-station-name (cdr tr))))
        (push (car tr) (alist-get name groups nil nil #'equal))))
    (if (null groups)
        "none"
      (mapconcat (lambda (g)
                   (concat (mapconcat #'train-time-map--bullet (reverse (cdr g)) " ")
                           (if (equal (car g) (train-time-station-name station))
                               ""
                             (format " (%s)" (car g)))))
                 (nreverse groups) ", "))))

(defun train-time-map--insert-details ()
  "Insert the description of the selected station."
  (let* ((station (train-time-map--selected-station))
         (dirs (train-time-station-directions station))
         (toward (delq nil (list (alist-get 'south dirs) (alist-get 'north dirs)))))
    (insert "\n  "
            (propertize (train-time-station-name station) 'face 'bold)
            (format "  (stop %s)\n" (train-time-station-id station))
            (format "  Services: %s\n" (train-time-map--services-at train-time-map--row))
            (format "  Trains toward: %s\n"
                    (if toward (string-join toward ", ") "none"))
            (format "  Transfers: %s\n" (train-time-map--describe-transfers station))
            (format "  Location: %.5f, %.5f\n"
                    (train-time-station-lat station)
                    (train-time-station-lon station)))))

(defun train-time-map--help-text ()
  (if (bound-and-true-p evil-local-mode)
      "j/k along the line   h/l switch service   RET arrivals   L line   t transfer   A alerts   m OSM   q quit"
    "C-n/C-p along the line   C-f/C-b switch service   RET arrivals   L line   t transfer   A alerts   m OSM   q quit"))

(defun train-time-map--render ()
  "Redraw the map and put point on the selected node."
  (let ((inhibit-read-only t)
        (nstations (length (train-time-map--stations)))
        target)
    (erase-buffer)
    (insert "\n  "
            (propertize (plist-get train-time-map--network :title)
                        'face (let ((color (plist-get train-time-map--network :color)))
                                (if color `(:foreground ,color :weight bold) 'bold)))
            "\n\n")
    (train-time-map--insert-legend)
    (insert "\n")
    (train-time-map--labels-row)
    (dotimes (row nstations)
      (when (= row train-time-map--row)
        (setq target (+ (point) (train-time-map--column-x train-time-map--col))))
      (train-time-map--station-row row)
      (when (< row (1- nstations))
        (train-time-map--link-row row)))
    (train-time-map--insert-details)
    (insert "\n  " (propertize (train-time-map--help-text) 'face 'train-time-map-help)
            "\n  " (propertize (format "%s; schedule %s" train-time-mta-attribution
                                       train-time-mta-data-version)
                               'face 'train-time-map-help)
            "\n")
    (goto-char target)
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (set-window-point window target))))

;;;; Navigation

(defun train-time-map--select (row col)
  "Select the node at ROW, COL and redraw."
  (setq train-time-map--row row
        train-time-map--col col)
  (train-time-map--render))

(defun train-time-map--step (delta)
  "Move DELTA stops along the selected service."
  (let ((row train-time-map--row)
        (n (length (train-time-map--stations))))
    (dotimes (_ (abs delta))
      (let ((next (cl-loop for r = (+ row (cl-signum delta)) then (+ r (cl-signum delta))
                           while (< -1 r n)
                           when (train-time-map--stops-p r train-time-map--col)
                           return r)))
        (unless next
          (user-error (if (> delta 0) "Last stop on this service"
                        "First stop on this service")))
        (setq row next)))
    (train-time-map--select row train-time-map--col)))

(defun train-time-map--switch (delta)
  "Move to the service DELTA columns away, at or near the selected station."
  (let* ((col (+ train-time-map--col delta))
         (n (length (train-time-map--stations)))
         (row train-time-map--row))
    (unless (< -1 col (length (train-time-map--columns)))
      (user-error "No other service in that direction"))
    (let ((target (cl-loop for d from 0 below n
                           thereis (cond ((train-time-map--stops-p (- row d) col) (- row d))
                                         ((train-time-map--stops-p (+ row d) col) (+ row d))))))
      (unless target
        (user-error "Service %s has no stops"
                    (plist-get (aref (train-time-map--columns) col) :label)))
      (train-time-map--select target col))))

(defun train-time-map-next (&optional n)
  "Move to the next stop down the line on the selected service.
With prefix argument N, move N stops."
  (interactive "p")
  (train-time-map--step (or n 1)))

(defun train-time-map-previous (&optional n)
  "Move to the previous stop up the line on the selected service.
With prefix argument N, move N stops."
  (interactive "p")
  (train-time-map--step (- (or n 1))))

(defun train-time-map-right ()
  "Switch to the service to the right, e.g. local to express."
  (interactive)
  (train-time-map--switch 1))

(defun train-time-map-left ()
  "Switch to the service to the left, e.g. express to local."
  (interactive)
  (train-time-map--switch -1))

(defun train-time-map-first ()
  "Move to the first stop of the selected service."
  (interactive)
  (train-time-map--select (car (train-time-map--span train-time-map--col))
                          train-time-map--col))

(defun train-time-map-last ()
  "Move to the last stop of the selected service."
  (interactive)
  (train-time-map--select (cdr (train-time-map--span train-time-map--col))
                          train-time-map--col))

(defun train-time-map--sync-to-point ()
  "Select the node under point, if point is on one."
  (let ((node (get-text-property (point) 'train-time-map-node)))
    (when node
      (let ((row (car node))
            (col (or (cdr node) train-time-map--col)))
        (unless (train-time-map--stops-p row col)
          (setq col (cl-loop for c below (length (train-time-map--columns))
                             when (train-time-map--stops-p row c) return c)))
        (unless (and (= row train-time-map--row) (= col train-time-map--col))
          (train-time-map--select row col))))))

(defun train-time-map-mouse-select (event)
  "Select the node or station clicked in EVENT."
  (interactive "e")
  (let ((pos (event-start event)))
    (with-selected-window (posn-window pos)
      (goto-char (posn-point pos))
      (train-time-map--sync-to-point))))

;;;; Lines and transfers

(defun train-time-map--set-line (line-id)
  "Show line LINE-ID, selecting its first stop."
  (setq train-time-map--network (train-time-map--line-network line-id)
        train-time-map--col 0
        train-time-map--row (car (train-time-map--span 0))))

(defun train-time-map--goto-station (station)
  "Select STATION on the current line, keeping the service if it stops there.
Return non-nil if the line has STATION."
  (let ((row (cl-position (train-time-station-id station)
                          (train-time-map--stations)
                          :key #'train-time-station-id :test #'equal)))
    (when row
      (setq train-time-map--row row
            train-time-map--col
            (if (train-time-map--stops-p row train-time-map--col)
                train-time-map--col
              (cl-loop for c below (length (train-time-map--columns))
                       when (train-time-map--stops-p row c) return c)))
      t)))

(defun train-time-map--show-line (line-id station)
  "Show LINE-ID, selecting STATION if the line has it."
  (let ((current (and train-time-map--network (train-time-map--selected-station))))
    (unless (equal line-id (plist-get train-time-map--network :line))
      (train-time-map--set-line line-id))
    (or (and station (train-time-map--goto-station station))
        (and current (train-time-map--goto-station current))))
  (train-time-map--render))

(defun train-time-map-choose-line (line-id)
  "Show the diagram of LINE-ID, read with completion.
Lines stopping at the selected station are offered first."
  (interactive
   (let* ((here (train-time-mta-line-ids (train-time-map--selected-station)))
          (ids (append here (cl-set-difference (train-time-mta-line-ids) here
                                               :test #'equal)))
          (table (mapcar (lambda (id) (cons (train-time-mta-line-title id) id)) ids)))
     (list (cdr (assoc (completing-read "Line: " table nil t nil nil (caar table))
                       table)))))
  (train-time-map--show-line line-id nil))

(defun train-time-map-transfer ()
  "Switch to another line at the selected station or its complex."
  (interactive)
  (train-time-map--sync-to-point)
  (let* ((station (train-time-map--selected-station))
         (transfers (or (train-time-map--transfers station)
                        (user-error "No transfers at %s"
                                    (train-time-station-name station))))
         (table (mapcar (lambda (tr)
                          (cons (format "%s  at %s"
                                        (train-time-mta-line-title (car tr))
                                        (train-time-station-display-name (cdr tr)))
                                tr))
                        transfers))
         (choice (if (cdr table)
                     (cdr (assoc (completing-read "Transfer to: " table nil t)
                                 table))
                   (cdar table))))
    (train-time-map--show-line (car choice) (cdr choice))))

;;;; Opening a board

(defun train-time-map-open-board ()
  "Show the arrivals board for the selected station.
Where it appears depends on `train-time-map-board-display'."
  (interactive)
  (train-time-map--sync-to-point)
  (let* ((map-buffer (current-buffer))
         (map-window (selected-window))
         (board (train-time-board (train-time-map--selected-station)))
         (reuse train-time-map--board-window))
    (if (eq train-time-map-board-display 'replace)
        (pop-to-buffer-same-window board)
      ;; Replace the board this map opened last, rather than splitting
      ;; again; recreating the window keeps `quit-window' deleting it.
      (when (and (window-live-p reuse)
                 (not (eq reuse map-window))
                 (eq (window-frame reuse) (selected-frame))
                 (not (eq (window-buffer reuse) board))
                 (with-current-buffer (window-buffer reuse)
                   (derived-mode-p 'train-time-mode)))
        (delete-window reuse))
      (pop-to-buffer board
                     `((display-buffer-reuse-window display-buffer-in-direction)
                       (direction . ,train-time-map-board-display)
                       (window . ,map-window)
                       ,(if (eq train-time-map-board-display 'below)
                            '(window-height . 0.5)
                          '(window-width . 0.5))))
      (let ((window (selected-window)))
        (with-current-buffer map-buffer
          (setq train-time-map--board-window window))))))

;;;; Mode

(defconst train-time-map--evil-bindings
  `("j" train-time-map-next
    "k" train-time-map-previous
    "l" train-time-map-right
    "h" train-time-map-left
    ,(kbd "C-n") train-time-map-next
    ,(kbd "C-p") train-time-map-previous
    ,(kbd "C-f") train-time-map-right
    ,(kbd "C-b") train-time-map-left
    "gg" train-time-map-first
    "G" train-time-map-last
    "gr" revert-buffer
    "L" train-time-map-choose-line
    "t" train-time-map-transfer
    ,(kbd "RET") train-time-map-open-board
    "m" train-time-osm
    "A" train-time-alerts
    "q" quit-window)
  "Evil normal-state bindings for `train-time-map-mode'.")

(defvar train-time-map-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (binding
             `((,(kbd "C-n") . train-time-map-next)
               (,(kbd "C-p") . train-time-map-previous)
               (,(kbd "C-f") . train-time-map-right)
               (,(kbd "C-b") . train-time-map-left)
               ("n" . train-time-map-next)
               ("p" . train-time-map-previous)
               ("f" . train-time-map-right)
               ("b" . train-time-map-left)
               ([down] . train-time-map-next)
               ([up] . train-time-map-previous)
               ([right] . train-time-map-right)
               ([left] . train-time-map-left)
               (,(kbd "M-<") . train-time-map-first)
               (,(kbd "M->") . train-time-map-last)
               ("<" . train-time-map-first)
               (">" . train-time-map-last)
               (,(kbd "RET") . train-time-map-open-board)
               ("m" . train-time-osm)
               ("A" . train-time-alerts)
               ("L" . train-time-map-choose-line)
               ("t" . train-time-map-transfer)
               ([mouse-1] . train-time-map-mouse-select)))
      (define-key map (car binding) (cdr binding)))
    map)
  "Keymap for `train-time-map-mode'.")

(define-derived-mode train-time-map-mode special-mode "Train-Map"
  "Major mode for navigating a transit network diagram.
Each column is a stopping pattern and each row a station.

\\{train-time-map-mode-map}"
  (setq truncate-lines t
        buffer-read-only t)
  (setq-local revert-buffer-function
              (lambda (&rest _) (train-time-map--render)))
  (train-time-evil-setup 'train-time-map-mode train-time-map-mode-map
                         train-time-map--evil-bindings))

;;;###autoload
(defun train-time-map (&optional station)
  "Navigate the transit network, starting at STATION.
STATION defaults to the station of the current arrivals buffer,
then to the station selected the last time the map was used, then
to `train-time-default-station'.  The map keeps showing its current
line if STATION is on it, and otherwise switches to the first line
stopping there; \\<train-time-map-mode-map>\\[train-time-map-choose-line] picks another."
  (interactive)
  (let* ((station (or station
                      (and (derived-mode-p 'train-time-mode)
                           (train-time-station-at-point))))
         (buffer (get-buffer-create train-time-map--buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'train-time-map-mode)
        (train-time-map-mode))
      (unless (or station train-time-map--network)
        (setq station (train-time-mta-station train-time-default-station)))
      (when (and station
                 (not (and train-time-map--network
                           (train-time-map--goto-station station))))
        (let ((line (car (train-time-mta-line-ids station))))
          (unless line
            (user-error "No line diagram includes %s"
                        (train-time-station-display-name station)))
          (train-time-map--set-line line)
          (train-time-map--goto-station station))))
    (pop-to-buffer-same-window buffer)
    (with-current-buffer buffer
      (train-time-map--render))))

(provide 'train-time-map)
;;; train-time-map.el ends here
