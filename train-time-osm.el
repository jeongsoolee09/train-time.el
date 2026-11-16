;;; train-time-osm.el --- Show stations on OpenStreetMap via osm.el -*- lexical-binding: t; -*-

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

;; Optional integration with osm.el (https://github.com/minad/osm,
;; GNU ELPA), which needs Emacs 29+ and a graphical display.
;;
;; `train-time-osm' writes the station's line as a GPX file (stations
;; as waypoints, one track segment per stopping pattern drawn by
;; `train-time-map') and loads it with osm.el's public `osm-open',
;; then centers the map on the station with `osm-goto'.  Each line
;; becomes an osm.el data set named after it.  No osm.el internals
;; are used.
;;
;; The track joins station coordinates with straight segments; it
;; shows where the line runs, not the exact alignment.  GTFS
;; shapes.txt geometry is a later improvement.

;;; Code:

(require 'cl-lib)
(require 'xml)
(require 'train-time)
(require 'train-time-map)
(require 'train-time-mta)

(declare-function osm-open "ext:osm" (file))
(declare-function osm-goto "ext:osm" (lat lon zoom))

(defcustom train-time-osm-zoom 16
  "Zoom level used when showing a station in osm.el."
  :type 'natnum
  :group 'train-time)

;;;; GPX

(defun train-time-osm--gpx (name stations segments)
  "Return a GPX document named NAME.
STATIONS become waypoints; SEGMENTS is a list of station lists, each
drawn as one track segment in order."
  (let ((pt (lambda (tag s)
              (format "<%s lat=\"%.6f\" lon=\"%.6f\">"
                      tag (train-time-station-lat s) (train-time-station-lon s)))))
    (concat
     "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
     "<gpx version=\"1.1\" creator=\"train-time.el\""
     " xmlns=\"http://www.topografix.com/GPX/1/1\">\n"
     (format "<metadata><name>%s</name></metadata>\n" (xml-escape-string name))
     (mapconcat (lambda (s)
                  (concat (funcall pt "wpt" s)
                          (format "<name>%s</name></wpt>\n"
                                  (xml-escape-string (train-time-station-name s)))))
                stations "")
     (format "<trk><name>%s</name>\n" (xml-escape-string name))
     (mapconcat (lambda (segment)
                  (concat "<trkseg>\n"
                          (mapconcat (lambda (s) (concat (funcall pt "trkpt" s) "</trkpt>\n"))
                                     segment "")
                          "</trkseg>\n"))
                segments "")
     "</trk>\n</gpx>\n")))

(defun train-time-osm--line-gpx (line-id)
  "Return the GPX document for MTA line LINE-ID."
  (let ((line (train-time-mta-line line-id)))
    (train-time-osm--gpx
     (train-time-osm--dataset-name line-id)
     (mapcar #'train-time-mta-station (plist-get line :rows))
     (mapcar (lambda (c) (mapcar #'train-time-mta-station (plist-get c :stops)))
             (plist-get line :columns)))))

(defun train-time-osm--dataset-name (line-id)
  "Return the data set name of LINE-ID, which is also its file name."
  (replace-regexp-in-string
   "/" "-" (format "%s %s" (train-time-mta-route-bullet line-id)
                   (plist-get (train-time-mta-line line-id) :name))))

(defun train-time-osm--directory ()
  "Return the directory holding generated GPX files, creating it."
  (let ((dir (expand-file-name "train-time" temporary-file-directory)))
    (make-directory dir t)
    dir))

(defun train-time-osm--write-gpx (line-id)
  "Write LINE-ID's GPX file and return its name relative to its directory."
  (let ((name (train-time-osm--dataset-name line-id))
        (coding-system-for-write 'utf-8))
    (with-temp-file (expand-file-name name (train-time-osm--directory))
      (insert (train-time-osm--line-gpx line-id)))
    name))

;;;; Window placement

(defun train-time-osm--board-window ()
  "Return a window in the selected frame showing an arrivals board.
Prefer the selected window."
  (let ((board-p (lambda (w)
                   (with-current-buffer (window-buffer w)
                     (derived-mode-p 'train-time-mode)))))
    (if (funcall board-p (selected-window))
        (selected-window)
      (cl-find-if board-p (window-list nil 'nomini)))))

(defun train-time-osm--placement ()
  "Return (WINDOW . DIRECTION) to split for the OSM map.
With both the network map and a board visible, split whichever is
in the bottom-right so the OSM map lands in that corner: below it
when the two are side by side, to its right when they are stacked.
With only one of them visible, split it to the right."
  (let ((map (get-buffer-window "*train-time-map*"))
        (board (train-time-osm--board-window)))
    (if (and map board)
        (let* ((m (window-edges map))
               (b (window-edges board))
               ;; Edges are (LEFT TOP RIGHT BOTTOM).
               (board-corner (or (> (nth 2 b) (nth 2 m))
                                 (and (= (nth 2 b) (nth 2 m))
                                      (>= (nth 3 b) (nth 3 m)))))
               (corner (if board-corner board map))
               (other (if board-corner m b))
               (side-by-side (>= (car (window-edges corner)) (nth 2 other))))
          (cons corner (if side-by-side 'below 'right)))
      (cons (or map board (selected-window)) 'right))))

(defun train-time-osm--display-action ()
  "Return the `display-buffer' action used for the OSM map."
  (pcase-let ((`(,window . ,direction) (train-time-osm--placement)))
    `((display-buffer-reuse-window
       display-buffer-reuse-mode-window
       display-buffer-in-direction)
      (mode . osm-mode)
      (window . ,window)
      (direction . ,direction)
      ,(if (eq direction 'below) '(window-height . 0.5) '(window-width . 0.5)))))

;;;; Command

(defun train-time-osm--station-at-point ()
  "Return the station of the current board or network map selection."
  (cond ((derived-mode-p 'train-time-mode) train-time--station)
        ((derived-mode-p 'train-time-map-mode)
         (train-time-map--sync-to-point)
         (train-time-map--selected-station))))

(defun train-time-osm--line-for (station)
  "Return the line to draw around STATION.
In the network map that is the line shown; elsewhere the first line
stopping at STATION."
  (let ((shown (and (derived-mode-p 'train-time-map-mode)
                    (plist-get train-time-map--network :line))))
    (or (and shown (member (train-time-station-id station)
                           (plist-get (train-time-mta-line shown) :rows))
             shown)
        (car (train-time-mta-line-ids station)))))

(defun train-time-osm--existing-buffer ()
  "Return an osm.el buffer to reuse, preferring a visible one."
  (let ((osm-buffers (cl-remove-if-not
                      (lambda (b) (eq (buffer-local-value 'major-mode b) 'osm-mode))
                      (buffer-list))))
    (or (cl-find-if #'get-buffer-window osm-buffers)
        (car osm-buffers))))

;;;###autoload
(defun train-time-osm (station)
  "Show STATION and its line on OpenStreetMap using osm.el.
Interactively, use the station of the current arrivals board or
network map, or read one with completion.  The map opens next to
the train-time windows; see `train-time-osm--placement'."
  (interactive (list (or (train-time-osm--station-at-point)
                         (train-time--read-station))))
  (unless (or (featurep 'osm) (require 'osm nil t))
    (user-error "Showing stations on a map needs osm.el (M-x package-install RET osm)"))
  (let* ((line (train-time-osm--line-for station))
         (file (and line (train-time-osm--write-gpx line)))
         (dir (train-time-osm--directory))
         (display-buffer-overriding-action (train-time-osm--display-action))
         (existing (train-time-osm--existing-buffer)))
    ;; A relative file name makes osm.el label the data set with the
    ;; line's name instead of a temporary path.
    (when file
      (with-current-buffer (or existing (current-buffer))
        (let ((default-directory dir))
          (osm-open file))))
    (with-current-buffer (if file
                             (window-buffer (selected-window))
                           (or existing (current-buffer)))
      (osm-goto (train-time-station-lat station)
                (train-time-station-lon station)
                train-time-osm-zoom))
    (message "%s (%s)" (train-time-station-name station)
             (train-time-station-id station))))

(provide 'train-time-osm)
;;; train-time-osm.el ends here
