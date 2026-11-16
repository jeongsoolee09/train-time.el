;;; train-time-pb.el --- Minimal Protocol Buffers decoder -*- lexical-binding: t; -*-

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

;; A small, schema-driven decoder for the Protocol Buffers binary wire
;; format, written in pure Emacs Lisp.  It supports exactly what is
;; needed to read GTFS-Realtime feeds: varints, 32/64-bit fixed values,
;; length-delimited strings/bytes/sub-messages, and packed repeated
;; scalars.  Encoding is not supported.
;;
;; A schema is an alist mapping message names (symbols) to field
;; specifications of the form (NUMBER KEY TYPE [repeated]), where TYPE
;; is one of the scalar types in `train-time-pb--scalar-types' or the
;; name of another message in the schema.  Unknown fields are skipped,
;; as required by the protobuf compatibility rules.
;;
;; Decoded messages are plists keyed by the KEY keywords.  Repeated
;; fields decode to lists in wire order.  Absent fields are absent from
;; the plist, so `plist-member' distinguishes "unset" from zero.

;;; Code:

(require 'cl-lib)

(define-error 'train-time-pb-error "Malformed protobuf data")

(defconst train-time-pb--scalar-types
  '(string bytes bool enum int32 int64 uint32 uint64 sint32 sint64
           fixed32 fixed64 sfixed32 sfixed64 float double)
  "Scalar types understood by the decoder.")

(defun train-time-pb--read-varint (data pos end)
  "Read a varint from unibyte string DATA at POS, before END.
Return (VALUE . NEXT-POS)."
  (let ((value 0) (shift 0) (done nil))
    (while (not done)
      (when (>= pos end)
        (signal 'train-time-pb-error (list "truncated varint" pos)))
      (when (> shift 63)
        (signal 'train-time-pb-error (list "varint too long" pos)))
      (let ((byte (aref data pos)))
        (setq value (logior value (ash (logand byte #x7f) shift))
              shift (+ shift 7)
              pos (1+ pos)
              done (< byte #x80))))
    (cons value pos)))

(defun train-time-pb--read-fixed (data pos n)
  "Read N bytes at POS in DATA as a little-endian unsigned integer."
  (let ((value 0))
    (dotimes (i n)
      (setq value (logior value (ash (aref data (+ pos i)) (* 8 i)))))
    value))

(defun train-time-pb--signed (value bits)
  "Interpret unsigned VALUE as a two's complement integer of BITS."
  (if (>= value (ash 1 (1- bits))) (- value (ash 1 bits)) value))

(defun train-time-pb--zigzag (value)
  "Decode zigzag-encoded VALUE."
  (if (zerop (logand value 1)) (ash value -1) (- (ash (1+ value) -1))))

(defun train-time-pb--float (bits exponent-bits mantissa-bits)
  "Decode IEEE 754 BITS with EXPONENT-BITS and MANTISSA-BITS."
  (let* ((sign (if (zerop (ash bits (- (+ exponent-bits mantissa-bits)))) 1 -1))
         (bias (1- (ash 1 (1- exponent-bits))))
         (exp (logand (ash bits (- mantissa-bits)) (1- (ash 1 exponent-bits))))
         (mant (logand bits (1- (ash 1 mantissa-bits)))))
    (cond ((= exp (1- (ash 1 exponent-bits)))
           (if (zerop mant) (* sign 1.0e+INF) 0.0e+NaN))
          ((zerop exp) (* sign (ldexp (float mant) (- 1 bias mantissa-bits))))
          (t (* sign (ldexp (float (+ mant (ash 1 mantissa-bits)))
                            (- exp bias mantissa-bits)))))))

(defun train-time-pb--convert-varint (type value)
  "Convert raw varint VALUE to TYPE."
  (pcase type
    ('bool (/= value 0))
    ((or 'int32 'enum) (train-time-pb--signed (logand value #xffffffff) 32))
    ('int64 (train-time-pb--signed value 64))
    ('uint32 (logand value #xffffffff))
    ((or 'sint32 'sint64) (train-time-pb--zigzag value))
    (_ value)))

(defun train-time-pb--convert-fixed (type value)
  "Convert raw fixed-width VALUE to TYPE."
  (pcase type
    ('sfixed32 (train-time-pb--signed value 32))
    ('sfixed64 (train-time-pb--signed value 64))
    ('float (train-time-pb--float value 8 23))
    ('double (train-time-pb--float value 11 52))
    (_ value)))

(defun train-time-pb--decode-packed (type data pos end)
  "Decode packed repeated scalars of TYPE in DATA from POS to END."
  (let (values)
    (while (< pos end)
      (pcase type
        ((or 'fixed32 'sfixed32 'float)
         (push (train-time-pb--convert-fixed
                type (train-time-pb--read-fixed data pos 4))
               values)
         (setq pos (+ pos 4)))
        ((or 'fixed64 'sfixed64 'double)
         (push (train-time-pb--convert-fixed
                type (train-time-pb--read-fixed data pos 8))
               values)
         (setq pos (+ pos 8)))
        (_
         (let ((v (train-time-pb--read-varint data pos end)))
           (push (train-time-pb--convert-varint type (car v)) values)
           (setq pos (cdr v))))))
    (nreverse values)))

(defun train-time-pb--decode-message (schema name data pos end)
  "Decode message NAME using SCHEMA from DATA between POS and END."
  (let ((fields (or (alist-get name schema)
                    (error "Unknown protobuf message type: %s" name)))
        (result nil)
        (repeated-keys nil))
    (while (< pos end)
      (let* ((tag (train-time-pb--read-varint data pos end))
             (field-number (ash (car tag) -3))
             (wire-type (logand (car tag) 7))
             (spec (assq field-number fields))
             (type (nth 2 spec))
             value)
        (setq pos (cdr tag))
        (pcase wire-type
          (0 (let ((v (train-time-pb--read-varint data pos end)))
               (setq pos (cdr v)
                     value (and spec (train-time-pb--convert-varint type (car v))))))
          (1 (when (> (+ pos 8) end)
               (signal 'train-time-pb-error (list "truncated fixed64" pos)))
             (setq value (and spec (train-time-pb--convert-fixed
                                    type (train-time-pb--read-fixed data pos 8)))
                   pos (+ pos 8)))
          (5 (when (> (+ pos 4) end)
               (signal 'train-time-pb-error (list "truncated fixed32" pos)))
             (setq value (and spec (train-time-pb--convert-fixed
                                    type (train-time-pb--read-fixed data pos 4)))
                   pos (+ pos 4)))
          (2 (let* ((len (train-time-pb--read-varint data pos end))
                    (start (cdr len))
                    (stop (+ start (car len))))
               (when (> stop end)
                 (signal 'train-time-pb-error
                         (list "length exceeds message" start)))
               (when spec
                 (setq value
                       (cond
                        ((eq type 'string)
                         (decode-coding-string (substring data start stop) 'utf-8))
                        ((eq type 'bytes) (substring data start stop))
                        ((memq type train-time-pb--scalar-types)
                         (cons 'packed (train-time-pb--decode-packed
                                        type data start stop)))
                        (t (train-time-pb--decode-message
                            schema type data start stop)))))
               (setq pos stop)))
          (_ (signal 'train-time-pb-error
                     (list "unsupported wire type" wire-type pos))))
        (when spec
          (let ((key (nth 1 spec)))
            (if (eq (nth 3 spec) 'repeated)
                (let ((acc (plist-get result key)))
                  (unless (memq key repeated-keys) (push key repeated-keys))
                  (if (eq (car-safe value) 'packed)
                      (dolist (v (cdr value)) (push v acc))
                    (push value acc))
                  (setq result (plist-put result key acc)))
              ;; Last value wins for singular fields, per the spec.
              (setq result (plist-put result key value)))))))
    (dolist (key repeated-keys)
      (setq result (plist-put result key (nreverse (plist-get result key)))))
    result))

(defun train-time-pb-decode (schema name data)
  "Decode unibyte string DATA as message NAME described by SCHEMA.
Signal `train-time-pb-error' if DATA is malformed."
  (when (multibyte-string-p data)
    (setq data (encode-coding-string data 'no-conversion)))
  (condition-case err
      (train-time-pb--decode-message schema name data 0 (length data))
    (args-out-of-range
     (signal 'train-time-pb-error (list "truncated data" (cdr err))))))

(provide 'train-time-pb)
;;; train-time-pb.el ends here
