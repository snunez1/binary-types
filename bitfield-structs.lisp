;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: BINARY-TYPES -*-
;;; Copyright (C) 2011 Luke Gorrie <luke@bluetail.com>
;;; Copyright (c) 2024 by Steven Nunez. All rights reserved.
;;; SPDX-License-identifier: BSD-3-Clause

;;; This code came from the SLITCH system.

;;; It is not part of the BINARY-TYPES system because we're not using
;;; BITFIELD-STRUCTS, but it seems like it may be useful to someone in
;;; future.


;; ----------------------------------------------------------------------
;; Bit fields. Must be used in bitfield structs (see below)

(defclass binary-unsigned-bitint (binary-integer)
  ((bits :initarg bits
	 :reader bits)))

(defmacro define-unsigned-bitint (name bits)
  (check-type bits (integer 1 *))
  `(progn
     (deftype ,name () '(unsigned-byte ,bits))
     (setf (find-binary-type ',name)
	   (make-instance 'binary-unsigned-bitint
			  'name ',name
			  'bits ,bits
			  'sizeof (/ ,bits 8)
			  ))
     ',name))

(defmethod read-binary ((type binary-unsigned-bitint) stream
			&key start stop &allow-other-keys)
  (declare (ignore start stop))
  (values (read-bits stream (bits type))
	  (sizeof type)))

(defmethod write-binary ((type binary-unsigned-bitint) stream object
			 &key &allow-other-keys)
  (write-bits stream (bits type) object)
  (/ (bits type) 8))

(defgeneric bit-sizeof (type)
  (:documentation "Size of a binary type in bits."))

(defmethod bit-sizeof ((type binary-unsigned-bitint))
  (bits type))

(defmethod bit-sizeof ((type binary-type))
  (* 8 (sizeof type)))

(defmethod bit-sizeof ((type symbol))
  (bit-sizeof (find-binary-type type)))

;; Bitfield read/write

(defvar *bitbuffer-size* 0
  "Number of bits in `*bitbuffer'")
(defvar *bitbuffer* 0
  "Bits of the current partial byte.")

(defvar *bits-read-byte-function* nil)
(defvar *bits-write-byte-function* nil)

(declaim (type (integer 0 8) *bitbuffer-size*)
	 (type (unsigned-byte 8) *bitbuffer*))

(defmacro with-bitint-env (&body body)
  "Execute BODY in an environment for buffering bitints."
  `(let ((*bitbuffer-size* 0)
	 (*bitbuffer* 0)
	 (*bits-read-byte-function* *binary-read-byte*)
	 (*bits-write-byte-function* *binary-write-byte*)
	 (*binary-read-byte* (lambda (stream) (read-bits stream 8)))
	 (*binary-write-byte* (lambda (x stream) (write-bits stream 8 x))))
     ,@body))

(defun read-bits (s n)
  (cond ((zerop *bitbuffer-size*)
	 (read-new-bits s)
	 (read-bits s n))
	((<= n *bitbuffer-size*)
	 (let* ((leftover (- *bitbuffer-size* n))
		(result (ldb (byte n leftover) *bitbuffer*)))
	   (setf *bitbuffer-size* leftover)
	   (setf *bitbuffer* (ldb (byte leftover 0) *bitbuffer*))
	   result))
	(t
	 (let ((more (- n *bitbuffer-size*)))
	   (setf *bitbuffer-size* 0)
	   (logior (ash *bitbuffer* more)
		   (read-bits s more))))))

(defun read-new-bits (s)
  "Refill our buffer with a new byte of bits."
  (setf *bitbuffer-size* 8)
  (setf *bitbuffer* (funcall *bits-read-byte-function* s)))

(defun write-bits (s n byte)
  (cond ((zerop n)
	 t)
	((< (+ n *bitbuffer-size*) 8)
	 (write-buffer-bits (byte n 0) byte))
	(t
	 (let* ((nibble (- 8 *bitbuffer-size*))
		(leftover (- n nibble)))
	   (write-buffer-bits (byte nibble (- n nibble)) byte)
	   (write-bits-out s)
	   (write-bits s leftover byte)))))

(defun write-buffer-bits (bytespec bits)
  "Write the BYTESPEC of BITS into the bit buffer."
  (setf *bitbuffer* (dpb (ldb bytespec bits)
			 (byte (byte-size bytespec)
			       (- 8 (byte-size bytespec) *bitbuffer-size*))
			 *bitbuffer*))
  (incf *bitbuffer-size* (byte-size bytespec)))

(defun flush-bits (s)
  (unless (zerop *bitbuffer-size*)
    (funcall *bits-write-byte-function* *bitbuffer* s)))

(defun write-bits-out (s)
  "Write the bitbuffer to the stream S."
  (funcall *bits-write-byte-function* *bitbuffer* s)
  (setf *bitbuffer* 0)
  (setf *bitbuffer-size* 0))

(defclass binary-bitflag (binary-unsigned-bitint) ())

;; Define the binary type 'bit'
(setf (find-binary-type 'bitflag)
      (make-instance 'binary-bitflag
		     'name 'bitflag
		     'bits 1
		     'sizeof (/ 1 8)))

(defmethod read-binary ((type binary-bitflag) stream
			&key start stop &allow-other-keys)
  (declare (ignore start stop))
  (values (= 1 (call-next-method)) 1))
  

(defmethod write-binary ((type binary-bitflag) stream object
			 &key &allow-other-keys)
  (call-next-method type stream (if object 1 0)))

;; ----------------------------------------------------------------------
;; Bitfield structs. These are byte-aligned structs whose fields don't
;; have to be individually byte aligned.

(defclass binary-bitfield-struct (binary-struct) ())

(defmethod read-binary ((type binary-bitfield-struct) stream
			&key start stop &allow-other-keys)
  (declare (ignore start stop))
  (with-bitint-env
      (call-next-method)))

(defmethod write-binary ((type binary-bitfield-struct) stream object
			 &key &allow-other-keys)
  (with-bitint-env
      (call-next-method)))

;; Cut & paste & hack from binary-types.lisp (for now)
(defmacro define-binary-bitfield-struct (name-and-options dummy-options &rest doc-slot-descriptions)
  (declare (ignore dummy-options))	; clisp seems to require this..
  (let (embedded-declarations)
    (flet ((parse-slot-description (slot-description)
	     (cond
	      ((symbolp slot-description)
	       (values slot-description nil slot-description))
	      ((>= 2 (list-length slot-description))
	       (values slot-description nil (first slot-description)))
	      (t (loop for descr on (cddr slot-description) by #'cddr
		     with bintype = nil
		     and typetype = nil
		     if (member (first descr)
				'(:bt :btt :binary-type :binary-lisp-type))
		     do (multiple-value-bind (bt lisp-type nested-form)
			    (parse-bt-spec (second descr))
			  (declare (ignore lisp-type))
			  (setf bintype bt)
			  (when nested-form
			    (push nested-form embedded-declarations))
			  (when (and (symbolp bt)
				     (member (first descr)
					     '(:btt :binary-lisp-type)))
			    (setf typetype bintype)))
		     else nconc
			  (list (first descr) (second descr)) into descriptions
		     finally
		       (return (values (list* (first slot-description)
					      (second slot-description)
					      (if typetype
						  (list* :type typetype descriptions)
						descriptions))
				       bintype
				       (first slot-description))))))))
      (multiple-value-bind (doc slot-descriptions)
	  (if (stringp (first doc-slot-descriptions))
	      (values (list (first doc-slot-descriptions))
		      (rest doc-slot-descriptions))
	    (values nil doc-slot-descriptions))
	(let* ((type-name (if (consp name-and-options)
			      (first name-and-options)
			    name-and-options))
	       (binslots (mapcan (lambda (slot-description)
				   (multiple-value-bind (options bintype slot-name)
				       (parse-slot-description slot-description)
				     (declare (ignore options))
				     (if bintype
					 (list (make-record-slot :name slot-name
								 :type bintype))
				       nil)))
				 slot-descriptions))
	       (slot-types (mapcar #'record-slot-type binslots)))
	  `(progn
	     ,@embedded-declarations
	     (defstruct ,name-and-options
	       ,@doc
	       ,@(mapcar #'parse-slot-description slot-descriptions))
	     (setf (find-binary-type ',type-name)
	       (make-instance 'binary-bitfield-struct
		 'name ',type-name
		 'sizeof (bitfield-struct-size ',slot-types)
		 'slots ',binslots
		 'offset 0
		 'constructor (find-symbol
                               (with-standard-io-syntax
                                 (format nil "~A-~A" '#:make ',type-name)))))
	     ',type-name))))))

(defun bitfield-struct-size (slot-types)
  (let ((bits (loop for s in slot-types sum (bit-sizeof s))))
    (if (zerop (rem bits 8))
	(/ bits 8)
	(error "Bitfield struct is not byte-aligned"))))
