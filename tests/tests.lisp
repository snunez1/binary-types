;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: BINARY-TYPES/TESTS -*-
;;; Copyright (c) 2024 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: BSD-3-Clause
(in-package #:binary-types/tests)

(defsuite binary-types ())


(deftest vectors (binary-types)

  ;; unsigned 32 bit
  (let* ((binary-types:*endian* :little-endian)
	 (test-vector #(1 2 3 4 10 9 8 7))
	 binary-to			;write lisp vector INTO this variable
	 binary-from)			;read lisp vector OUT of this variable

  (eval `(define-binary-vector binary-seq u32 ,(length test-vector)))
  (setf binary-to (with-output-to-sequence (out)
		    (write-binary 'binary-seq out test-vector)))
  (setf binary-from (with-input-from-sequence (in binary-to)
		      (read-binary 'binary-seq in)))
  (assert-true (num= test-vector binary-from)))


  ;; signed 32 bit
  (let* ((binary-types:*endian* :little-endian)
	 (test-vector #(1 -2 3 -4 10 -9 8 7))
	 binary-to
	 binary-from)

  (eval `(define-binary-vector binary-seq s32 ,(length test-vector)))
  (setf binary-to (with-output-to-sequence (out)
		    (write-binary 'binary-seq out test-vector)))
  (setf binary-from (with-input-from-sequence (in binary-to)
		      (read-binary 'binary-seq in)))
  (assert-true (num= test-vector binary-from)))


  ;; signed 64 bit
  (let* ((binary-types:*endian* :little-endian)
	 (test-vector #(-1152921504606846976 ;most-negative-fixnum (CCL64)
			1 -2 3 -4 10 -9 8 7
			1152921504606846975)) ;most-positive-fixnum
	 binary-to
	 binary-from)

  (eval `(define-binary-vector binary-seq s64 ,(length test-vector)))
  (setf binary-to (with-output-to-sequence (out)
		    (write-binary 'binary-seq out test-vector)))
  (setf binary-from (with-input-from-sequence (in binary-to)
		      (read-binary 'binary-seq in)))
  (assert-true (num= test-vector binary-from)))


  ;; bignum
  ;; Interesting that on CCL this fits into s64
  (let* ((binary-types:*endian* :little-endian)
	 (test-vector `#(,(1- -1152921504606846976)
			1 -2 3 -4 10 -9 8 7
			,(1+ 1152921504606846975)))
	 binary-to
	 binary-from)

  (eval `(define-binary-vector binary-seq s64 ,(length test-vector)))
  (setf binary-to (with-output-to-sequence (out)
		    (write-binary 'binary-seq out test-vector)))
  (setf binary-from (with-input-from-sequence (in binary-to)
		      (read-binary 'binary-seq in)))
  (assert-true (num= test-vector binary-from)))

  ;; Actually, up to (* 8 most-positive/negative-fixnum) fits into s64
  (let* ((binary-types:*endian* :little-endian)
	 (test-vector `#(,(* 8 -1152921504606846976)
			1 -2 3 -4 10 -9 8 7
			,(* 8 1152921504606846975)))
	 binary-to
	 binary-from)

  (eval `(define-binary-vector binary-seq s64 ,(length test-vector)))
  (setf binary-to (with-output-to-sequence (out)
		    (write-binary 'binary-seq out test-vector)))
  (setf binary-from (with-input-from-sequence (in binary-to)
		      (read-binary 'binary-seq in)))
  (assert-true (num= test-vector binary-from)))


  ;; s128
  (let* ((binary-types:*endian* :little-endian)
	 (test-vector `#(,(* 9 -1152921504606846976)
			 1 -2 3 -4 10 -9 8 7
			 ,(* 9 1152921504606846975)))
	 binary-to
	 binary-from)

    (eval `(define-binary-vector binary-seq-s64   s64 ,(length test-vector)))
    (eval `(define-binary-vector binary-seq-s128 s128 ,(length test-vector)))

    (setf binary-to (with-output-to-sequence (out)
		      (write-binary 'binary-seq-s64 out test-vector)))
    (setf binary-from (with-input-from-sequence (in binary-to)
			(read-binary 'binary-seq-s64 in)))
    (assert-false (num= test-vector binary-from)) ;at this point s64 overflows

    (setf binary-to (with-output-to-sequence (out)
		      (write-binary 'binary-seq-s128 out test-vector)))
    (setf binary-from (with-input-from-sequence (in binary-to)
			(read-binary 'binary-seq-s128 in)))
    (assert-true (num= test-vector binary-from)))
)

