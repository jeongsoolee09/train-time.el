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
;; nyct-subway.proto and the Mercury alert extensions (also field
;; 1001, on Alert and EntitySelector) from MTA's
;; gtfs-realtime-service-status.proto.  Fields not listed here are
;; skipped by the decoder, so the schema can grow as features need it.

;;; Code:

(require 'cl-lib)
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
     (4 :vehicle VehiclePosition)
     (5 :alert Alert))
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
     (7 :stop-id string))
    (Alert
     (1 :active-period TimeRange repeated)
     (5 :informed-entity EntitySelector repeated)
     (6 :cause enum)
     (7 :effect enum)
     (8 :url TranslatedString)
     (10 :header-text TranslatedString)
     (11 :description-text TranslatedString)
     (1001 :mercury MercuryAlert))
    (TimeRange
     (1 :start uint64)
     (2 :end uint64))
    (EntitySelector
     (1 :agency-id string)
     (2 :route-id string)
     (3 :route-type int32)
     (5 :stop-id string)
     (6 :direction-id uint32)
     (1001 :mercury MercuryEntitySelector))
    (MercuryEntitySelector
     (1 :sort-order string))
    (TranslatedString
     (1 :translation Translation repeated))
    (Translation
     (1 :text string)
     (2 :language string))
    (MercuryAlert
     (1 :created-at uint64)
     (2 :updated-at uint64)
     (3 :alert-type string)
     (7 :display-before-active uint64)
     (8 :human-readable-active-period TranslatedString)
     (10 :affected-stations EntitySelector repeated)))
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

(defun train-time-gtfs-rt-alerts (feed)
  "Return the Alert plists of decoded FEED with their entity IDs.
Each element is (ID . ALERT); deleted entities are skipped."
  (delq nil (mapcar (lambda (entity)
                      (and (not (plist-get entity :is-deleted))
                           (plist-get entity :alert)
                           (cons (plist-get entity :id) (plist-get entity :alert))))
                    (plist-get feed :entity))))

(defun train-time-gtfs-rt-text (translated &optional language)
  "Return the text of TranslatedString TRANSLATED in LANGUAGE.
LANGUAGE defaults to \"en\".  Falls back to an untagged translation,
then to the first one; returns nil if there is none."
  (let ((translations (plist-get translated :translation))
        (language (or language "en")))
    (plist-get (or (cl-find language translations
                            :key (lambda (tr) (plist-get tr :language)) :test #'equal)
                   (cl-find-if (lambda (tr) (member (plist-get tr :language) '(nil "")))
                               translations)
                   (car translations))
               :text)))

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
