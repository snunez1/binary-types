;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2024 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: BSD-3-Clause

(uiop:define-package :binary-types/tests
    (:use :cl :binary-types :clunit :array-operations :flexi-streams :num-utils.num=))
