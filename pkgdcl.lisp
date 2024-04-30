;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-USER -*-
;;; Copyright (C) 1999-2001 Department of Computer Science, University of Tromsø, Norway
;;; Copyright (c) 2024 by Steven Nunez. All rights reserved.
;;; SPDX-License-identifier: BSD-3-Clause

(uiop:define-package #:binary-types
  (:use #:common-lisp)
  (:export #:*endian*			; [dynamic-var] must be bound when reading integers
	   #:endianess			; [deftype] The set of endian names
	   ;; built-in types
	   #:char8			; [type-name] 8-bit character
	   #:u8				; [type-name] 8-bit unsigned integer
	   #:u16			; [type-name] 16-bit unsigned integer
	   #:u32			; [type-name] 32-bit unsigned integer
	   #:u64			; [type-name] 64-bit unsigned integer
	   #:u128			; [type-name] 128-bit unsigned integer
	   #:u256			; [type-name] 256-bit unsigned integer
	   #:s8				; [type-name] 8-bit signed integer
	   #:s16			; [type-name] 16-bit signed integer
	   #:s32			; [type-name] 32-bit signed integer
	   #:s64			; [type-name] 64-bit signed integer
	   #:s128			; [type-name] 128-bit signed integer
	   #:s256			; [type-name] 256-bit signed integer
					; (you may define additional integer types
					; of any size yourself.)
	   ;; type defining macros
	   #:define-unsigned		; [macro] declare an unsigned-int type
	   #:define-signed		; [macro] declare a signed-int type
	   #:define-binary-struct	; [macro] declare a binary defstruct type
	   #:define-binary-class	; [macro] declare a binary defclass type
	   #:define-bitfield		; [macro] declare a bitfield (symbolic integer) type
	   #:define-enum		; [macro] declare an enumerated type
	   #:define-binary-string	; [macro] declare a string type
	   #:define-null-terminated-string ; [macro] declare a null-terminated string
	   ;; readers and writers
	   #:read-binary		; [func] reads a binary-type from a stream
	   #:read-binary-record		; [method]
	   #:write-binary		; [func] writes an binary object to a stream
	   #:write-binary-record	; [method]
	   #:read-binary-string
	   ;; record handling
	   #:binary-record-slot-names	; [func] list names of binary slots.
	   #:binary-slot-value		; [func] get "binary" version of slot's value
	   #:binary-slot-type		; [func] get binary slot's binary type
	   #:binary-slot-tags		; [func] get the tags of a binary slot
	   #:slot-offset		; [func] determine offset of slot.
	   ;; misc
	   #:find-binary-type		; [func] accessor to binary-types namespace
	   #:sizeof			; [func] The size in octets of a binary type
	   #:enum-value			; [func] Calculate numeric version of enum value
	   #:enum-symbolic-value	; [func] Inverse of enum-value.
	   #:with-binary-file		; [macro] variant of with-open-file
	   #:with-binary-output-to-list	; [macro]
	   #:with-binary-output-to-vector ; [macro]
	   #:with-binary-input-from-list ; [macro]
	   #:with-binary-input-from-vector ; [macro]
	   #:*binary-write-byte*	; [dynamic-var]
	   #:*binary-read-byte*		; [dynamic-var]
	   #:*padding-byte*		; [dynamic-var] The value filled in when writing paddings
	   #:split-bytes		; [func] utility
	   #:merge-bytes)		; [func] utility
  (:documentation "BINARY-TYPES documenation"))
