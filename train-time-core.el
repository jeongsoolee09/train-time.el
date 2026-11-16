;;; train-time-core.el --- Shared data model for train-time -*- lexical-binding: t; -*-

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

;; Backend-independent records used by every train-time backend.
;;
;; Times are integer POSIX seconds (absolute instants).  Conversion to
;; local wall-clock time happens only at display time, using the time
;; zone of the transit system, so that predictions stay correct no
;; matter what zone Emacs itself runs in.

;;; Code:

(require 'cl-lib)

(defgroup train-time nil
  "Real-time public transit information."
  :group 'applications
  :prefix "train-time-")

(cl-defstruct (train-time-station (:copier nil))
  "A station (a parent stop that groups one or more platforms)."
  (id nil :documentation "Stable backend identifier, e.g. \"701\".")
  (name nil :documentation "User-facing name in the local language.")
  (label nil :documentation
         "Name made unique for completion and buffer names, or nil.")
  (complex nil :documentation
           "Identifier shared by stations connected by free transfers.")
  (lat nil :documentation "WGS84 latitude.")
  (lon nil :documentation "WGS84 longitude.")
  (routes nil :documentation "List of route identifiers serving it.")
  (directions nil :documentation
              "Alist of (DIRECTION . LABEL); LABEL nil at terminals."))

(cl-defstruct (train-time-arrival (:copier nil))
  "One predicted train call at a platform.
TIME is nil when no prediction is available; it is never invented."
  (route-id nil :documentation "Route identifier as published, e.g. \"7X\".")
  (route nil :documentation "Route name shown to riders, e.g. \"7\".")
  (service nil :documentation "Service pattern: `local', `express' or nil.")
  (direction nil :documentation "Direction symbol, e.g. `north'.")
  (direction-label nil :documentation "Rider-facing direction label.")
  (stop-id nil :documentation "Platform identifier, e.g. \"701S\".")
  (destination nil :documentation "Name of the last stop of the trip.")
  (time nil :documentation "Predicted POSIX time, or nil.")
  (event nil :documentation "`arrival' or `departure'.")
  (trip-id nil :documentation "Trip identifier from the feed.")
  (train-id nil :documentation "Operator train identifier, if any.")
  (assigned t :documentation
            "Non-nil if a physical train is assigned to the trip.
Unassigned trips are scheduled departures, not tracked trains.")
  (terminating nil :documentation "Non-nil if the trip ends at this stop."))

(defun train-time-station-display-name (station)
  "Return STATION's unique label, falling back to its name."
  (or (train-time-station-label station) (train-time-station-name station)))

(defun train-time-format-clock (time zone)
  "Format POSIX TIME as local clock time in ZONE (a TZ string)."
  (if time (format-time-string "%H:%M" time zone) "—"))

(defun train-time-minutes-until (time now)
  "Return whole minutes from NOW until TIME, or nil if TIME is nil."
  (and time (floor (- time now) 60)))

(provide 'train-time-core)
;;; train-time-core.el ends here
