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

(defun train-time-mta-route-names (stations)
  "Return the rider-facing route names serving STATIONS, in MTA order.
X variants share their route's name: the 7 and <7> are both \"7\"."
  (let ((ids (apply #'append (mapcar #'train-time-station-routes stations)))
        names)
    (dolist (route train-time-mta-data-routes)
      (when (member (car route) ids)
        (cl-pushnew (car (train-time-mta--route (car route))) names
                    :test #'equal)))
    (nreverse names)))

(defun train-time-mta-complex-name (station)
  "Return a name for the complex of STATION, unique among stations.
That is the name most of its stations share, followed by the routes
serving the complex when a station elsewhere has the same name, as
the G's Fulton St does for the Fulton St complex in Manhattan."
  (let* ((complex (train-time-mta-complex station))
         (members (cl-remove-if-not (lambda (s) (memq s complex))
                                    train-time-mta-stations))
         (counts nil))
    (dolist (s members)
      (cl-incf (alist-get (train-time-station-name s) counts 0 nil #'equal)))
    (let ((name (car (cl-reduce (lambda (best c) (if (> (cdr c) (cdr best)) c best))
                                (nreverse counts)))))
      (if (cl-some (lambda (s) (and (not (memq s complex))
                                    (equal (train-time-station-name s) name)))
                   train-time-mta-stations)
          (format "%s (%s)" name
                  (string-join (train-time-mta-route-names complex) " "))
        name))))

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
                         :destination-id
                         (and last-stop
                              (let ((id (train-time-mta--parent-stop last-stop)))
                                (and (train-time-mta-station id) id)))
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

(defun train-time-mta-trips (feed)
  "Return the trips in decoded GTFS-Realtime FEED, with their stops.
Each trip is a plist with :trip-id, :route-id, :train-id, :assigned
and :calls, a list of (STATION-ID STOP-ID ARRIVAL DEPARTURE) in stop
order.  ARRIVAL and DEPARTURE are POSIX times or nil; calls the feed
marks as skipped or gives no time are left out."
  (let (trips)
    (dolist (tu (train-time-gtfs-rt-trip-updates feed))
      (let* ((trip (plist-get tu :trip))
             (nyct (plist-get trip :nyct))
             calls)
        (dolist (stu (plist-get tu :stop-time-update))
          (let ((stop-id (plist-get stu :stop-id))
                (arr (plist-get (plist-get stu :arrival) :time))
                (dep (plist-get (plist-get stu :departure) :time)))
            (setq arr (and arr (> arr 0) arr)
                  dep (and dep (> dep 0) dep))
            (when (and stop-id (or arr dep)
                       (not (eql (plist-get stu :schedule-relationship)
                                 train-time-gtfs-rt-stop-skipped)))
              (push (list (train-time-mta--parent-stop stop-id) stop-id arr dep)
                    calls))))
        (when calls
          (push (list :trip-id (plist-get trip :trip-id)
                      :route-id (plist-get trip :route-id)
                      :train-id (plist-get nyct :train-id)
                      :assigned (if (plist-member nyct :is-assigned)
                                    (plist-get nyct :is-assigned)
                                  (null nyct))
                      :calls (nreverse calls))
                trips))))
    (nreverse trips)))

;;;; Service alerts

(defconst train-time-mta-alerts-feed "camsys%2Fsubway-alerts"
  "Name of the subway service alerts feed, after the base URL.")

(defun train-time-mta-alerts-url ()
  "Return the URL of the subway service alerts feed."
  (concat train-time-mta-feed-base-url train-time-mta-alerts-feed))

(defun train-time-mta--alert-priority (entity)
  "Return the priority in informed ENTITY's Mercury sort order, or 0.
The sort order looks like \"MTASBWY:7:30\"; larger is more severe."
  (let ((order (plist-get (plist-get entity :mercury) :sort-order)))
    (or (and order (string-match ":\\([0-9]+\\)\\'" order)
             (string-to-number (match-string 1 order)))
        0)))

(defun train-time-mta--alert (id alert)
  "Return a `train-time-alert' for decoded ALERT with entity ID."
  (let* ((mercury (plist-get alert :mercury))
         (entities (plist-get alert :informed-entity))
         (type (plist-get mercury :alert-type))
         (zero-nil (lambda (n) (and n (> n 0) n))))
    (make-train-time-alert
     :id id
     :type type
     :planned (and type (string-prefix-p "Planned" type))
     :header (train-time-gtfs-rt-text (plist-get alert :header-text))
     :description (let ((text (train-time-gtfs-rt-text
                               (plist-get alert :description-text))))
                    (and text (not (string-blank-p text)) (string-trim text)))
     :periods (mapcar (lambda (p) (cons (funcall zero-nil (plist-get p :start))
                                        (funcall zero-nil (plist-get p :end))))
                      (plist-get alert :active-period))
     :period-text (train-time-gtfs-rt-text
                   (plist-get mercury :human-readable-active-period))
     :routes (delete-dups (delq nil (mapcar (lambda (e) (plist-get e :route-id))
                                            entities)))
     :stations (delete-dups
                (delq nil (mapcar (lambda (e)
                                    (let ((stop (plist-get e :stop-id)))
                                      (and stop (train-time-mta--parent-stop stop))))
                                  entities)))
     :priority (apply #'max 0 (mapcar #'train-time-mta--alert-priority entities))
     :updated (funcall zero-nil (plist-get mercury :updated-at)))))

(defun train-time-mta-alerts (feed)
  "Return the `train-time-alert's in decoded alerts FEED."
  (mapcar (lambda (entry) (train-time-mta--alert (car entry) (cdr entry)))
          (train-time-gtfs-rt-alerts feed)))

(defun train-time-mta-alert-local-p (alert)
  "Return non-nil if ALERT only matters at the stations it names.
Station notices (e.g. a closed entrance) and boarding changes do not
affect riders elsewhere on the line, unlike skipped stops."
  (let ((type (train-time-alert-type alert)))
    (and type (train-time-alert-stations alert)
         (string-match-p "Station Notice\\|Boarding Change" type))))

(defun train-time-mta-alert-affects-p (alert route-ids)
  "Return non-nil if ALERT concerns any of ROUTE-IDS.
Express variants count as their route, so an alert for the 7 also
concerns the <7>."
  (let ((families (mapcar #'train-time-mta--family route-ids)))
    (cl-some (lambda (r) (member (train-time-mta--family r) families))
             (train-time-alert-routes alert))))

;;;; Elevator and escalator outages

(defconst train-time-mta-outages-feed "nyct%2Fnyct_ene.json"
  "Name of the elevator and escalator outages feed (JSON).")

(defconst train-time-mta-equipment-feed "nyct%2Fnyct_ene_equipments.json"
  "Name of the elevator and escalator list (JSON).")

(defun train-time-mta-outages-url ()
  "Return the URL of the elevator and escalator outages feed."
  (concat train-time-mta-feed-base-url train-time-mta-outages-feed))

(defun train-time-mta-equipment-url ()
  "Return the URL of the elevator and escalator list."
  (concat train-time-mta-feed-base-url train-time-mta-equipment-feed))

(defun train-time-mta--json-field (record field)
  "Return string FIELD of JSON RECORD, or nil if it is missing or empty."
  (let ((value (alist-get field record)))
    (and (stringp value) (not (string-blank-p value)) (string-trim value))))

(defun train-time-mta--ene-time (text)
  "Return the POSIX time of TEXT like \"11/17/2026 06:00:00 PM\", or nil.
The time is in New York."
  (when (and text
             (string-match "\\`\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]\\{4\\}\\) +\\([0-9]+\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)? *\\([AP]M\\)"
                           text))
    (let* ((n (lambda (i) (string-to-number (or (match-string i text) "0"))))
           (hour (mod (funcall n 4) 12)))
      (when (equal (match-string 7 text) "PM") (cl-incf hour 12))
      (float-time (encode-time (list (funcall n 6) (funcall n 5) hour
                                     (funcall n 2) (funcall n 1) (funcall n 3)
                                     nil -1 train-time-mta-time-zone))))))

(defun train-time-mta-equipment-complexes (equipment)
  "Return a hash table from equipment number to station complex ID.
EQUIPMENT is the parsed equipment list, a list of alists."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (record equipment)
      (let ((number (train-time-mta--json-field record 'equipmentno))
            (complex (train-time-mta--json-field record 'stationcomplexid)))
        (when (and number complex)
          (puthash number complex table))))
    table))

(defun train-time-mta-outages (outages complexes)
  "Return the elevator and escalator outages in parsed OUTAGES.
COMPLEXES maps equipment numbers to station complex IDs, as returned
by `train-time-mta-equipment-complexes'.  Each outage is a plist
with :equipment, :kind (`elevator' or `escalator'), :complex,
:serving, :ada, :reason, :since, :until (POSIX times or nil) and
:upcoming."
  (delq nil
        (mapcar
         (lambda (record)
           (let ((number (train-time-mta--json-field record 'equipment)))
             (when number
               (list :equipment number
                     :kind (pcase (train-time-mta--json-field record 'equipmenttype)
                             ("EL" 'elevator) ("ES" 'escalator) (_ 'equipment))
                     :complex (gethash number complexes)
                     :station (train-time-mta--json-field record 'station)
                     :serving (train-time-mta--json-field record 'serving)
                     :ada (equal (train-time-mta--json-field record 'ADA) "Y")
                     :reason (train-time-mta--json-field record 'reason)
                     :since (train-time-mta--ene-time
                             (train-time-mta--json-field record 'outagedate))
                     :until (train-time-mta--ene-time
                             (train-time-mta--json-field record 'estimatedreturntoservice))
                     :upcoming (equal (train-time-mta--json-field record 'isupcomingoutage)
                                      "Y")))))
         outages)))

(provide 'train-time-mta)
;;; train-time-mta.el ends here
