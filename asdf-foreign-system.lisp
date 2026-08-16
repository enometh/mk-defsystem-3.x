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
;;;
;;; Export ASDF:MK-REGISTER-FOREIGN-SYSTEM (which registers a
;;; mk-defystem system as a foreign preloaded system) and
;;; ASDF:MK-REGISTER-FOREIGN-SYSTEMS (which registers mk-defsystem
;;; systems and all their dependencies) from the ASDF package as
;;; aliases to the functions in this file.


(cl:defpackage "MK-ASDF-FOREIGN-SYSTEM"
 (:use "CL")
 (:export "REGISTER-FOREIGN-SYSTEMS"))
(cl:in-package "ASDF-FOREIGN-SYSTEM")

(defclass asdf-foreign-system (asdf:system)
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

(defun register-foreign-system (system &key replace)
  "SYSTEM is a system designator for an already loaded mk-defsystem
system.  Registers SYSTEM as a \"preloaded\" asdf foreign system
unless a system same name is already loaded.  If REPLACE is non-NIL,
replaces any registered asdf system with the given system."
  (unless (typep system '(or mk:mk-defsystem mk::foreign-system))
    (setq system (mk:find-system system :error)))
  ;; we cannot use find-system to check if system is already loaded
  ;; because it will load an asdf system if it can find it
  (multiple-value-bind (foundp found-system pathname previous previous-time)
      (locate-system (canonical-system-name-for-asdf system))
    (declare (ignorable foundp found-system pathname previous previous-time))
    (cond ((and (not replace) previous) previous)
	  (t (values (asdf::register-system
		      (make-instance 'asdf-foreign-system
			:foreign-system system))
		     previous)))))

(defun register-foreign-systems (system-or-systems &key replace)
  "Register and system-or-systems and all their dependencies with asdf as
foreign systems."
  (mapcar (lambda (sys) (register-foreign-system sys :replace replace))
	  (mk:get-recursive-deps system-or-systems
				 :include-roots t)))

(export '(asdf::mk-register-foreign-system asdf::mk-register-foreign-systems)
	"ASDF")
(setf (fdefinition 'asdf::mk-register-foreign-system)
      #'register-foreign-system)
(setf (fdefinition 'asdf::mk-register-foreign-systems)
      #'register-foreign-systems)

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


;;; ----------------------------------------------------------------------
;;;
;;; B-SIDE 2 - using ASDF foreign system from within defsystem
;;;

(in-package "MAKE")

(mk:register-foreign-system-info
 :asdf
 :constructor-op
 (lambda (&key kind object)
   (assert (eql kind :asdf))
   (check-type object asdf:system)
   (make-foreign-system :kind kind
			:name (asdf/system:primary-system-name object)
			:object object
			:compile-form (lambda ()
					(asdf:compile-system object))
			:load-form (lambda ()
				     (asdf:load-system object))))
 :find-op (lambda (s)
	    (asdf:find-system s nil)))

#||
*foreign-systems-info*
(asdf/system:primary-system-name (asdf:find-system "cl-git"))
(mk:find-foreign-system  "cl-git" :kind :asdf)
(get-system "cl-git")
(mk:undefsystem "cl-git")
(mk:compile-system :cl-git)
(mk:find-system :cl-git)
||#