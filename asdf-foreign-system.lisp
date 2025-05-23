;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Sun Jan 26 19:43:02 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
;;; USE MK-DEFYSTEM systems with ASDF, by registering them as
;;; "preloaded" asdf foreign systems via asdf::register-system.  See
;;; REGISTER-FOREIGN-SYSTEMS.  The systems should already be loaded
;;; via mk-defsystem.

(cl:defpackage "ASDF-FOREIGN-SYSTEM"
 (:use "CL"  #:asdf)
 (:export "REGISTER-FOREIGN-SYSTEMS"))
(cl:in-package "ASDF-FOREIGN-SYSTEM")

(defclass asdf-foreign-system (system)
  ((foreign-system :initform nil :initarg :foreign-system)))

(defun canonical-system-name-for-asdf (foreign-system)
  ;; NOTE mk::canonicalize-system-name returns upcase.
  ;; mk::canonicalize-component-name returns downcase. asdf allows Foo
  ;; and FOO to be different system names. mk-defsystem does not.
  ;; always use the lower case as the name supplied to asdf.
  (check-type foreign-system (or mk:mk-defsystem mk::foreign-system))
  (mk::canonicalize-component-name foreign-system))

(defmethod initialize-instance :after ((asdf-foreign-system asdf-foreign-system) &key &allow-other-keys)
  (with-slots (foreign-system) asdf-foreign-system
    (check-type foreign-system mk:mk-defsystem)
    (setf (slot-value asdf-foreign-system 'asdf/component:name)
	  (canonical-system-name-for-asdf foreign-system))))

(defun register-foreign-system (system)
  (unless (typep system '(or mk:mk-defsystem mk::foreign-system))
    (setq system (mk:find-system system :error)))
  (or (find-system (canonical-system-name-for-asdf system) nil)
      (asdf::register-system
       (make-instance 'asdf-foreign-system
	 :foreign-system system))))

(defun register-foreign-systems (system-or-systems)
  "register system-or-systems and their dependencies with asdf"
  (mapcar 'register-foreign-system (mk:get-recursive-deps system-or-systems)))

#||
(require 'cffi)
(register-foreign-systems 'cffi)
(asdf:load-system 'trivial-features)
||#