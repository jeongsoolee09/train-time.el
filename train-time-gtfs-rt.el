;;; train-time-gtfs-rt.el --- GTFS-Realtime decoding -*- lexical-binding: t; -*-

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

;; Schema for the subset of GTFS-Realtime used by train-time, following
;; https://gtfs.org/realtime/reference/ (gtfs-realtime.proto), plus
;; the NYCT extensions (field 1001) documented in MTA's
;; nyct-subway.proto.  Fields not listed here are skipped by the
;; decoder, so the schema can grow as features need it.

;;; Code:

(require 'train-time-pb)

(defconst train-time-gtfs-rt-schema
  '((FeedMessage
     (1 :header FeedHeader)
     (2 :entity FeedEntity repeated))
    (FeedHeader
     (1 :gtfs-realtime-version string)
     (2 :incrementality enum)
     (3 :timestamp uint64)
     (1001 :nyct NyctFeedHeader))
    (NyctFeedHeader
     (1 :nyct-subway-version string))
    (FeedEntity
     (1 :id string)
     (2 :is-deleted bool)
     (3 :trip-update TripUpdate)
     (4 :vehicle VehiclePosition))
    (TripUpdate
     (1 :trip TripDescriptor)
     (2 :stop-time-update StopTimeUpdate repeated)
     (4 :timestamp uint64)
     (5 :delay int32))
    (TripDescriptor
     (1 :trip-id string)
     (2 :start-time string)
     (3 :start-date string)
     (4 :schedule-relationship enum)
     (5 :route-id string)
     (6 :direction-id uint32)
     (1001 :nyct NyctTripDescriptor))
    (NyctTripDescriptor
     (1 :train-id string)
     (2 :is-assigned bool)
     (3 :direction enum))
    (StopTimeUpdate
     (1 :stop-sequence uint32)
     (2 :arrival StopTimeEvent)
     (3 :departure StopTimeEvent)
     (4 :stop-id string)
     (5 :schedule-relationship enum)
     (1001 :nyct NyctStopTimeUpdate))
    (NyctStopTimeUpdate
     (1 :scheduled-track string)
     (2 :actual-track string))
    (StopTimeEvent
     (1 :delay int32)
     (2 :time int64)
     (3 :uncertainty int32))
    (VehiclePosition
     (1 :trip TripDescriptor)
     (3 :current-stop-sequence uint32)
     (4 :current-status enum)
     (5 :timestamp uint64)
     (7 :stop-id string)))
  "Protobuf schema for GTFS-Realtime with NYCT extensions.")

(defconst train-time-gtfs-rt-stop-skipped 1
  "StopTimeUpdate schedule_relationship value for SKIPPED.")

(defun train-time-gtfs-rt-decode (data)
  "Decode GTFS-Realtime FeedMessage bytes DATA into a plist."
  (train-time-pb-decode train-time-gtfs-rt-schema 'FeedMessage data))

(defun train-time-gtfs-rt-feed-time (feed)
  "Return the header timestamp of decoded FEED, or nil."
  (let ((ts (plist-get (plist-get feed :header) :timestamp)))
    (and ts (> ts 0) ts)))

(defun train-time-gtfs-rt-trip-updates (feed)
  "Return the TripUpdate plists of decoded FEED, skipping deleted entities."
  (delq nil (mapcar (lambda (entity)
                      (and (not (plist-get entity :is-deleted))
                           (plist-get entity :trip-update)))
                    (plist-get feed :entity))))

(defun train-time-gtfs-rt-event-time (stu)
  "Return (TIME . EVENT) for StopTimeUpdate STU, or nil if unknown.
Prefers the arrival time and falls back to the departure time.  A
zero time is treated as missing, since proto2 defaults are zero."
  (let ((arr (plist-get (plist-get stu :arrival) :time))
        (dep (plist-get (plist-get stu :departure) :time)))
    (cond ((and arr (> arr 0)) (cons arr 'arrival))
          ((and dep (> dep 0)) (cons dep 'departure)))))

(provide 'train-time-gtfs-rt)
;;; train-time-gtfs-rt.el ends here
