;;; train-time-mta.el --- MTA New York City Subway backend -*- lexical-binding: t; -*-

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

;; MTA NYC Subway and Staten Island Railway support, using the public
;; GTFS-Realtime feeds listed at https://api.mta.info/ (no API key
;; required).  NYCT splits real time data into eight feeds by route;
;; a station's board fetches the feeds of every route that stops there.
;;
;; Stations, routes, colors and the stopping patterns drawn by
;; `train-time-map' come from `train-time-mta-data', generated from
;; MTA's static GTFS and Stations.csv by
;; scripts/train-time-mta-generate.el.  A station is a GTFS parent
;; stop ("701"); its platforms append "N" or "S".  Stations that share
;; a Complex ID are connected by free transfers.
;;
;; Data provided by the Metropolitan Transportation Authority (MTA).
;; MTA does not endorse this package.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'train-time-core)
(require 'train-time-gtfs-rt)
(require 'train-time-mta-data)

(defcustom train-time-mta-feed-base-url
  "https://api-endpoint.mta.info/Dataservice/mtagtfsfeeds/"
  "URL prefix of the NYCT GTFS-Realtime feeds.
Feed names from `train-time-mta-feeds' are appended to it."
  :type 'string
  :group 'train-time)

(defconst train-time-mta-feeds
  '(("nyct%2Fgtfs" "1" "2" "3" "4" "5" "6" "6X" "7" "7X" "GS")
    ("nyct%2Fgtfs-ace" "A" "C" "E" "H")
    ("nyct%2Fgtfs-bdfm" "B" "D" "F" "FX" "M" "FS")
    ("nyct%2Fgtfs-g" "G")
    ("nyct%2Fgtfs-jz" "J" "Z")
    ("nyct%2Fgtfs-nqrw" "N" "Q" "R" "W")
    ("nyct%2Fgtfs-l" "L")
    ("nyct%2Fgtfs-si" "SI"))
  "Real time feeds and the route IDs each one carries.")

(defconst train-time-mta-time-zone "America/New_York"
  "Time zone of the MTA subway system.")

(defconst train-time-mta-attribution
  "Data: Metropolitan Transportation Authority (MTA)"
  "Attribution shown with MTA data.")

;;;; Routes

(defun train-time-mta-route-info (route-id)
  "Return (ROUTE-ID SHORT-NAME LONG-NAME COLOR TEXT-COLOR), or nil."
  (assoc route-id train-time-mta-data-routes))

(defun train-time-mta--family (route-id)
  "Return the route whose express variant is ROUTE-ID, or ROUTE-ID.
NYCT marks express variants with a trailing X: \"7X\" is the <7>."
  (let ((base (and (> (length route-id) 1)
                   (string-suffix-p "X" route-id)
                   (substring route-id 0 -1))))
    (if (and base (train-time-mta-route-info base)) base route-id)))

(defun train-time-mta--route (route-id)
  "Return (ROUTE . SERVICE) for feed ROUTE-ID.
ROUTE is the name on the bullet, e.g. \"S\" for the shuttles.
SERVICE is `express' for an X variant, `local' for a route that has
an X variant, and nil otherwise: the A is not \"local\" just
because no <A> exists."
  (let* ((family (train-time-mta--family route-id))
         (name (or (nth 1 (train-time-mta-route-info family)) route-id)))
    (cons name
          (cond ((not (equal family route-id)) 'express)
                ((train-time-mta-route-info (concat route-id "X")) 'local)))))

(defun train-time-mta-route-bullet (route-id)
  "Return the rider-facing bullet for ROUTE-ID, e.g. \"<7>\" for 7X."
  (let ((route (train-time-mta--route route-id)))
    (if (eq (cdr route) 'express)
        (format "<%s>" (car route))
      (car route))))

(defun train-time-mta-route-color (route-id)
  "Return the official color of ROUTE-ID as \"#RRGGBB\", or nil."
  (nth 3 (train-time-mta-route-info route-id)))

(defun train-time-mta-route-text-color (route-id)
  "Return the color for text on a ROUTE-ID bullet, or nil."
  (nth 4 (train-time-mta-route-info route-id)))

;;;; Stations

(defconst train-time-mta-stations
  (let ((names (make-hash-table :test #'equal)))
    (dolist (row train-time-mta-data-stations)
      (cl-incf (gethash (nth 1 row) names 0)))
    (mapcar (lambda (row)
              (pcase-let ((`(,id ,name ,lat ,lon ,north ,south ,complex ,daytime ,routes)
                           row))
                (make-train-time-station
                 :id id :name name :lat lat :lon lon :routes routes
                 :label (if (> (gethash name names) 1)
                            (format "%s (%s)" name daytime)
                          name)
                 :complex complex
                 :directions `((north . ,north) (south . ,south)))))
            train-time-mta-data-stations))
  "List of `train-time-station' records served by this backend.")

(defconst train-time-mta--station-table
  (let ((table (make-hash-table :test #'equal :size 600)))
    (dolist (s train-time-mta-stations table)
      (puthash (train-time-station-id s) s table))))

(defun train-time-mta-station (id)
  "Return the station whose parent stop is ID, or nil."
  (gethash id train-time-mta--station-table))

(defun train-time-mta-complex (station)
  "Return the stations sharing STATION's complex, STATION first."
  (let ((complex (train-time-station-complex station)))
    (cons station
          (cl-remove-if-not
           (lambda (s) (and (not (eq s station))
                            (equal (train-time-station-complex s) complex)))
           train-time-mta-stations))))

(defun train-time-mta-feed-urls (station)
  "Return the feed URLs carrying trains that stop at STATION."
  (let (feeds)
    (dolist (route (train-time-station-routes station))
      (let ((feed (car (cl-find-if (lambda (f) (member route (cdr f)))
                                   train-time-mta-feeds))))
        (when feed (cl-pushnew feed feeds :test #'equal))))
    (mapcar (lambda (feed) (concat train-time-mta-feed-base-url feed))
            (nreverse feeds))))

;;;; Lines

(defun train-time-mta-line (id)
  "Return the line plist for line ID (\"A\", \"7\"...), or nil.
See `train-time-mta-data-lines' for its keys."
  (cdr (assoc id train-time-mta-data-lines)))

(defun train-time-mta-line-ids (&optional station)
  "Return line IDs in MTA order, only those stopping at STATION if given."
  (let ((id (and station (train-time-station-id station))))
    (delq nil (mapcar (lambda (line)
                        (and (or (null id)
                                 (member id (plist-get (cdr line) :rows)))
                             (car line)))
                      train-time-mta-data-lines))))

(defun train-time-mta-line-title (id)
  "Return a title such as \"A  8 Avenue Express\" for line ID."
  (format "%s  %s" (train-time-mta-route-bullet id)
          (plist-get (train-time-mta-line id) :name)))

(defun train-time-mta--parent-stop (stop-id)
  "Return the parent stop of platform STOP-ID (\"701S\" -> \"701\")."
  (if (string-match-p "[NS]\\'" stop-id)
      (substring stop-id 0 -1)
    stop-id))

(defun train-time-mta--direction (stop-id)
  "Return the direction symbol encoded in platform STOP-ID."
  (cond ((string-suffix-p "N" stop-id) 'north)
        ((string-suffix-p "S" stop-id) 'south)))

(defun train-time-mta--stop-name (stop-id)
  "Return a display name for platform STOP-ID."
  (let ((station (train-time-mta-station (train-time-mta--parent-stop stop-id))))
    (if station (train-time-station-name station) stop-id)))

(defun train-time-mta-arrivals (feed station now &optional grace)
  "Return upcoming arrivals at STATION from decoded GTFS-Realtime FEED.
NOW is the current POSIX time.  Calls earlier than NOW minus GRACE
seconds (default 30) are dropped: the NYCT feed keeps stale trips
whose last update is many hours old.  Calls the feed marks as
skipped, or that have no time at all, are omitted rather than
guessed.  The result is sorted by time."
  (let ((parent (train-time-station-id station))
        (cutoff (- now (or grace 30)))
        result)
    (dolist (tu (train-time-gtfs-rt-trip-updates feed))
      (let* ((trip (plist-get tu :trip))
             (route (train-time-mta--route (or (plist-get trip :route-id) "")))
             (nyct (plist-get trip :nyct))
             (stus (plist-get tu :stop-time-update))
             (last-stop (plist-get (car (last stus)) :stop-id)))
        (dolist (stu stus)
          (let ((stop-id (plist-get stu :stop-id)))
            (when (and stop-id
                       (equal (train-time-mta--parent-stop stop-id) parent)
                       (not (eql (plist-get stu :schedule-relationship)
                                 train-time-gtfs-rt-stop-skipped)))
              (let ((event (train-time-gtfs-rt-event-time stu))
                    (direction (train-time-mta--direction stop-id)))
                (when (and event (>= (car event) cutoff))
                  (push (make-train-time-arrival
                         :route-id (plist-get trip :route-id)
                         :route (car route)
                         :service (cdr route)
                         :direction direction
                         :direction-label
                         (alist-get direction (train-time-station-directions station))
                         :stop-id stop-id
                         :destination (and last-stop
                                           (train-time-mta--stop-name last-stop))
                         :time (car event)
                         :event (cdr event)
                         :trip-id (plist-get trip :trip-id)
                         :train-id (plist-get nyct :train-id)
                         ;; Feeds without the NYCT extension track every trip.
                         :assigned (if (plist-member nyct :is-assigned)
                                       (plist-get nyct :is-assigned)
                                     (null nyct))
                         :terminating (equal stop-id last-stop))
                        result))))))))
    (sort result (lambda (a b) (< (train-time-arrival-time a)
                                  (train-time-arrival-time b))))))

(provide 'train-time-mta)
;;; train-time-mta.el ends here
