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


;;; ----------------------------------------------------------------------
;;;
;;; DUMP-SERIAL-FILELIST: mk-defsystem cannot resolve
;;; component-depends-on dependencies which are not on the same level,
;;; but asdf allows them. In this case we can produce a list of files
;;; in the asdf system in the serial order in which they would be
;;; compiled and loaded by asdf.

(defun dump-serial-filelist (asdf-system)
  "Return a list of relative namestrings of the lisp files in the given
system in the serial order that asdf would compile and load them."
  (loop for (a . b) in (asdf/plan:plan-actions
			(asdf/plan:make-plan
			 'asdf/plan:sequential-plan
			 'asdf:compile-op asdf-system))
	with source-root = (asdf/system:system-source-directory asdf-system)
	when (and (typep a 'asdf:compile-op)
		  (typep b 'asdf:cl-source-file))
	collect (enough-namestring (asdf:component-pathname b) source-root)))


#||
(load "~/cl/asdf-config.lisp")
(register-foreign-system 'trivial-features)
(asdf:load-asd "/path/to/trial.asd")
(setq $a (asdf:find-system :trial))
(setq $l (remove-if 'consp (asdf/component:component-sideway-dependencies $a)))
(dolist (i $l) (mk:oos i :load) (register-foreign-system i))
(dump-serial-filelist $a)
||#