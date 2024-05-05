;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: CL-USER -*-
;;; Copyright (C) 1999-2001 Department of Computer Science, University of Tromsø, Norway
;;; Copyright (c) 2024 by Steven Nunez. All rights reserved.
;;; SPDX-License-identifier: BSD-3-Clause

(defsystem "binary-types"
  :description "A library for reading and writing binary records."
  :long-description #.(uiop:read-file-string
		       (uiop:subpathname *load-pathname* "description.text"))
  :version "1.0.0"
  :author "Frode V. Fjeld"
  :maintainer "Steven Nunez"
  :license :BSD-3-Clause
  :depends-on ("ieee-floats"
	       "array-operations")
  :perform (load-op :after (op c)
		    (provide 'binary-types))
  :in-order-to ((test-op (test-op "binary-types/tests")))
  :components ((:file "pkgdcl")
	       (:file "binary-types")
	       (:file "arrays")))

(defsystem "binary-types/tests"
  :description "Unit tests for BINARY-TYPES"
  :author "Steven Nunez"
  :license :BSD-3-Clause
  :depends-on ("binary-types"
               "clunit2"
	       "array-operations"
	       "flexi-streams"
	       "num-utils")
  :pathname #P"tests/"
  :serial t
  :components ((:file "pkgdcl")
	       (:file "tests"))
  :perform (test-op (o s)
		    (let ((*print-pretty* t)) ;work around clunit issue #9
		      (symbol-call :clunit :run-suite
				   (find-symbol* :binary-types
						 :binary-types/tests)
					   :use-debugger nil))))
