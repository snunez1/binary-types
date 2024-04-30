;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: CL-USER -*-
;;; Copyright (C) 1999-2001 Department of Computer Science, University of Tromsø, Norway
;;; Copyright (c) 2024 by Steven Nunez. All rights reserved.
;;; SPDX-License-identifier: BSD-3-Clause

(defsystem "binary-types"
  :name "Binary-types"
  :maintainer "Steven Nunez"
  :author "Frode V. Fjeld"
  :version "1.0.0"
  :license :BSD-3-Clause
  :description "A library for reading and writing binary records."
  :long-description  #.(uiop:read-file-string
			(uiop:subpathname *load-pathname* "description.text"))
  :perform (load-op :after (op c)
		    (provide 'binary-types))
  :components ((:file "pkgdcl")
	       (:file "binary-types")))

