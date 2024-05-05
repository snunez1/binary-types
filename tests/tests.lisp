;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: BINARY-TYPES/TESTS -*-
;;; Copyright (c) 2024 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: BSD-3-Clause
(in-package #:binary-types/tests)

(defsuite binary-types ())

;;; If these test are failing on your system, you might want to take
;;; note of the values you get from the following.  The tests were
;;; developed on:
;; CL-USER> (lisp-implementation-type)
;; "Clozure Common Lisp"
;; CL-USER> (lisp-implementation-version)
;; "Version 1.12.2 (v1.12.2-16-gc4df19e6) WindowsX8664"

;; (integer-length most-negative-fixnum) ;=> 60
;; most-negative-fixnum = -1152921504606846976
;; most-positive-fixnum =  1152921504606846975
;; CL-USER> (expt 2 60)
;; 1152921504606846976


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
	 (test-vector `#(,most-negative-fixnum
			1 -2 3 -4 10 -9 8 7
			,most-positive-fixnum))
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
	 (test-vector `#(,(1- most-negative-fixnum)
			1 -2 3 -4 10 -9 8 7
			,(1+ most-positive-fixnum)))
	 binary-to
	 binary-from)

  (eval `(define-binary-vector binary-seq s64 ,(length test-vector)))
  (setf binary-to (with-output-to-sequence (out)
		    (write-binary 'binary-seq out test-vector)))
  (setf binary-from (with-input-from-sequence (in binary-to)
		      (read-binary 'binary-seq in)))
  (assert-true (num= test-vector binary-from)))

  ;; Actually, up to (* 8 most-positive/negative-fixnum) fits into a
  ;; s64 vector, which I find odd since fixnum is 60 bits on this
  ;; platform/implementation.  It breaks at 9 times most-postive-fixnum.
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
    (assert-true (num= test-vector binary-from))))


(deftest arrays (binary-types)

  ;; u32
  (let* ((binary-types:*endian* :little-endian)
	 (test-array #2A((1 2 3 4 10 9 8 7)
			 (10 20 30 40 50 60 70 80)))
	 binary-to
	 binary-from)

    (eval `(define-binary-array binary-arr u32 ',(aops:dims test-array)))
    (setf binary-to (with-output-to-sequence (out)
		      (write-binary 'binary-arr out test-array)))
    (setf binary-from (with-input-from-sequence (in binary-to)
			(read-binary 'binary-arr in)))
    (assert-true (num= test-array binary-from)))

  ;; s32
  (let* ((binary-types:*endian* :little-endian)
	 (test-array #2A((1 -2 3 -4 10 -9 8 7)
			 (-10 20 -30 40 50 60 70 -80)))
	 binary-to
	 binary-from)

  (eval `(define-binary-array binary-arr s32 ',(aops:dims test-array)))
  (setf binary-to (with-output-to-sequence (out)
		    (write-binary 'binary-arr out test-array)))
  (setf binary-from (with-input-from-sequence (in binary-to)
		      (read-binary 'binary-arr in)))
  (assert-true (num= test-array binary-from)))

  ;; u64
  (let* ((binary-types:*endian* :little-endian)
	 (test-array (make-array '(2 8)
				 :initial-contents `((1 2 3 4 10 9 8 7)
						     (10 20 30 40 50 60 70 ,most-positive-fixnum))))
	 binary-to
	 binary-from)

    (eval `(define-binary-array binary-arr u64 ',(aops:dims test-array)))
    (setf binary-to (with-output-to-sequence (out)
		      (write-binary 'binary-arr out test-array)))
    (setf binary-from (with-input-from-sequence (in binary-to)
			(read-binary 'binary-arr in)))
    (assert-true (num= test-array binary-from)))

  ;; s64
  (let* ((binary-types:*endian* :little-endian)
	 (test-array (make-array '(2 8)
				 :initial-contents `((,most-negative-fixnum 2 3 4 10 9 8 7)
						     (10 20 30 40 50 60 70 ,most-positive-fixnum))))
	 binary-to
	 binary-from)

    (eval `(define-binary-array binary-arr s64 ',(aops:dims test-array)))
    (setf binary-to (with-output-to-sequence (out)
		      (write-binary 'binary-arr out test-array)))
    (setf binary-from (with-input-from-sequence (in binary-to)
			(read-binary 'binary-arr in)))
    (assert-true (num= test-array binary-from)))

  ;; u128
  ;; (integer-length (expt 2 125)) => 126
  (let* ((binary-types:*endian* :little-endian)
	 (test-array (make-array '(2 8)
				 :initial-contents `((1 2 3 4 10 9 8 7)
						     (10 20 30 40 50 60 70 ,(expt 2 125)))))
	 binary-to
	 binary-from)

    (eval `(define-binary-array binary-arr u128 ',(aops:dims test-array)))
    (setf binary-to (with-output-to-sequence (out)
		      (write-binary 'binary-arr out test-array)))
    (setf binary-from (with-input-from-sequence (in binary-to)
			(read-binary 'binary-arr in)))
    (assert-true (num= test-array binary-from)))

  ;; multi-dimensional arrays
  (let* ((binary-types:*endian* :little-endian)
	 (test-array #3A(((12 5 9)
			  (6 5 6)
			  (3 8 1))
			 ((7 4 5)
			  (9 59 3)
			  (44 947 88))
			 ((583 13 9561)
			  (067 95 3326)
			  (8634 3364 0605))))
	 binary-to
	 binary-from)

    (eval `(define-binary-array binary-arr u32 ',(aops:dims test-array)))
    (setf binary-to (with-output-to-sequence (out)
		      (write-binary 'binary-arr out test-array)))
    (setf binary-from (with-input-from-sequence (in binary-to)
			(read-binary 'binary-arr in)))
    (assert-true (num= test-array binary-from))))


  ;; s128
  ;; The CLHS spec shows that for bignums, the representation for
  ;; signed and unsigned are identical, so we don't test s128 here.
  ;; When we get around to implementing IEEE 754-2008, which defines
  ;; binary interchange formats for "half precision" (16-bit) and
  ;; "quad precision" (128-bit) we'll add the tests.


