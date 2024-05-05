;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: BINARY-TYPES -*-
;;; Copyright (C) 2011 Luke Gorrie <luke@bluetail.com>
;;; Copyright (c) 2024 by Steven Nunez. All rights reserved.
;;; SPDX-License-identifier: BSD-3-Clause
(in-package #:binary-types)

;;; Partially from: https://github.com/sharplispers/slitch/blob/master/src/binary-types-extra.lisp

(export '(define-binary-vector binary-vector binary-vector-input-state
	  define-binary-array  binary-array  binary-array-input-state))

(defun binary-vector-input-state (stream)
  "Returns two values: the vector being read, and the current input position."
  (values (cdr stream) (1+ (car stream))))

;; ----------------------------------------------------------------------
;; Vectors

(defclass binary-vector (binary-record)
  ((element-type :initarg element-type :reader binary-vector-element-type)
   (size         :initarg size         :reader binary-vector-size)))

(defun binary-vector (element-type size)
  "Directly return a binary-vector type."
  (make-instance 'binary-vector
                 'name `(binary-vector ,element-type ,size)
                 'sizeof (* (sizeof element-type) size)
                 'element-type element-type
                 'size size))

(defmacro define-binary-vector (type-name element-type size)
  (check-type size (integer 1 *))
  `(progn
     (deftype ,type-name () '(array ,element-type ,size))
     (setf (find-binary-type ',type-name)
	   (make-instance 'binary-vector
			  'name ',type-name
			  'sizeof (* (sizeof ',element-type)
				     ,size)
			  'element-type ',element-type
			  'size ,size))
     ',type-name))

(defmethod read-binary ((type binary-vector) stream &key &allow-other-keys)
  (read-binary-vector stream
		      (binary-vector-element-type type)
		      (binary-vector-size type)))

(defun read-binary-vector (stream type size)
  (let ((vec (make-array (list size) :element-type type))
	(read-bytes 0))
    (dotimes (i size)
      (multiple-value-bind (obj bytes)
	  (read-binary type stream)
	(setf (elt vec i) obj)
	(incf read-bytes bytes)))
    (values vec read-bytes)))

(defmethod write-binary ((type binary-vector) stream object
			 &key &allow-other-keys)
  (loop for x across object
	do (write-binary (binary-vector-element-type type) stream x))
  (sizeof type))

(defmethod sizeof ((type binary-vector))
  (with-slots (size element-type) type
    (* size (sizeof element-type))))


;; ----------------------------------------------------------------------
;; Arrays

(defun binary-array-input-state (stream)
  "Returns two values: the vector being read, and the current input position."
  (values (cdr stream) (1+ (car stream))))

(defclass binary-array (binary-record)
  ((element-type :initarg element-type :reader binary-array-element-type)
   (size         :initarg size         :reader binary-array-size)
   (dimensions   :initarg dimensions   :reader binary-array-dimensions)))

(defun binary-array (element-type dims)
  "Directly return a binary-array type."
  (let* ((size (if (listp dims) (apply #'* dims) dims))) ;in future combine binary-array and binary-vector
    (make-instance 'binary-array
		   'name `(binary-array ,element-type ,dims)
		   'sizeof (* (sizeof element-type) size)
		   'element-type element-type
		   'size size
		   'dimensions dims)))

(defmacro define-binary-array (type-name element-type dims)
  `(let* ((size (if (listp ,dims) (apply #'* ,dims) ,dims)))
    (progn
       (deftype ,type-name () '(array ,element-type ,dims))
       (setf (find-binary-type ',type-name)
	     (make-instance 'binary-array
			    'name ',type-name
			    'sizeof (* (sizeof ',element-type)
				       size)
			    'element-type ',element-type
			    'size size
			    'dimensions ,dims))
       ',type-name)))

(defmethod read-binary ((type binary-array) stream &key &allow-other-keys)
  (read-binary-array stream
		     (binary-array-element-type type)
		     (binary-array-size type)
		     (binary-array-dimensions type)))

(defun read-binary-array (stream type size dimensions)
  (let ((arr (make-array dimensions :element-type type))
	(read-bytes 0))
    (dotimes (i size)
      (multiple-value-bind (obj bytes)
	  (read-binary type stream)
	(setf (row-major-aref arr i) obj)
	(incf read-bytes bytes)))
    (values arr read-bytes)))

(defmethod write-binary ((type binary-array) stream object
			 &key &allow-other-keys)
  (loop for x across (aops:flatten object)
     do (write-binary (binary-array-element-type type) stream x))
  (sizeof type))

(defmethod sizeof ((type binary-array))
  (with-slots (size element-type) type
    (* size (sizeof element-type))))
