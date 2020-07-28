;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;Forked-From:/hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/misc:MK-integration.lisp,v_1.12.1.2_2009/09/02_16:48:12.
;;;
;;;
;;;  Touched: <Sun Nov 21 17:28:16 2010 +0530> Madhu <enometh@net.meer>
;;;  Bugs-To: enometh@net.meer
;;;  Status: Experimental. Do not redistribute.
;;;  Copyright (C) 2010 Madhu.  All Rights Reserved.
;;;
;;; WARNING WARNING !!!20 minute-regexp replace solution. DO NOT USE!!
;;;
;; Copyright (c) 1987--2010 LispWorks Ltd. All rights reserved.

;;; Integration of MK into the LispWorks IDE.
;;; This allows the Search Files and System Browser tools
;;; and Editor commands on systems to work on MK systems too.

;;; To use:
;;; 1) load MK and some MK system definitions.
;;; 2) load this file or compile the buffer.

;;; To use with the Search Files tool:
;;;   a) In the option pane at the top, select "System Search".
;;;   b) Move the focus to "System Names".
;;;   c) Press the Down arrow. It displays a list of system names,
;;;      both defined LispWorks DEFSYSTEM and by MK DEFSYSTEM.
;;;      LispWorks systems are prefixed by LW, MK systems by MK.
;;;   d) Enter one or more system names (separated by a semicolon)
;;;      of either LispWorks or MK. You can mix the two.
;;;   e) Enter a search string in the "Regexp Search String" box (e.g.
;;;      defun) and press return.
;;;   f) The tool searches all the files in the specified systems.

;;; To use with the System Browser tool:
;;;   a) When the tool starts or when selecting "All Systems" from the
;;;      "Systems" menu, it displays in "Tree" tab "All Systems" with
;;;      two children: LW and MK. Under each child it shows (when
;;;      expanded) all the system that are defined in this namespace.
;;;   b) In the text-input-pane you can enter more than one system
;;;      name, separated by semi-colon and optional whitespace. If you
;;;      don't prefix the name, it matches in both MK and LW. When
;;;      the names are displayed by the systems, they are prefixed.
;;;   c) You can complete in the text input pane by pressing Down arrow.
;;;   d) You can compile or load LispWorks and MK systems.


;;; Note that MK and LispWorks use different terminology for the same concepts:
;;;     LispWorks:  SYSTEM      MODULE      FILE
;;;          MK:  MODULE      COMPONENT   FILE

;;; The example uses SCM:ADD-SYSTEM-NAMESPACE to add a system
;;; namespace for MK. It also defines MK methods for these
;;; symbols:

;;; Mapping over sources:

;;;   SCM:MAP-SYSTEM-PATHNAMES

;;; Module (component) properties:

;;;   SCM:MODULE-IS-SYSTEM-P
;;;   SCM:MODULE-NAME
;;;   SCM:MODULE-CHILDREN
;;;   SCM:MODULE-PATHNAME
;;;   SCM:MODULE-PRINT-NAME
;;;   SCM:MODULE-FLAG-DESCRIPTION
;;;   SCM:MODULE-NAMESPACE-NAME

;;; Plan and execute:

;;;   SCM:SYSTEM-PLAN
;;;   SCM:EXECUTE-SYSTEM-PLAN
;;;   #+not-needed SCM:SYSTEM-TARGET-DIRECTORY

;;; Display in preview:

;;;   #+not-needed SCM:TREEIFY-SYSTEM-PLAN
;;;   SCM:TREEIFY-PLAN-EVENT
;;;   SCM:EVENT-SOURCE-P
;;;   SCM:EVENT-MODULE

;;;   The example uses these MK symbols:
;;; BOGUS XXX
;;;   MK:COMPONENT-PATHNAME
;;;   MK:SYSTEM
;;;   MK:MODULE
;;;   MK:SOURCE-FILE
;;;   MK:CL-SOURCE-FILE
;;;   MK:OOS
;;;   MK:PERFORM
;;;   MK:COMPONENT-PATHNAME
;;;   MK:MODULE-COMPONENTS
;;;   MK:OPERATION-DONE-P
;;;   MK:LOAD-SOURCE-OP
;;;   MK:COMPILE-OP
;;;   MK:LOAD-OP
;;;   MK:FIND-SYSTEM
;;;   MK:COMPONENT
;;;   MK:COMPONENT-NAME
;;;
;;;   MK::*DEFINED-SYSTEMS*
;;;   MK::COMPONENT-OPERATION-TIMES
;;;   MK::TRAVERSE
;;;   MK::*VERBOSE-OUT*



(in-package "CL-USER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;      Namespace definition       ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; The name-lister takes no argument and must return a list of strings.
;;; This one uses an internal symbol in MK, but the version
;;; that we have (Revision: 1.107) does not have any other way
;;; of doing it.

(defun MK-list-system-names ()
  #+nil
  (let ((list nil))
    (maphash #'(lambda (key value)
		 (declare (ignore value))
		 (push key list))
	     MK::*defined-systems*)
    ;;; should use
    ;;; currently ignoring systems not yet loaded
    ;;; maybe should sort them
    list)
  (mapcar 'car (mk::defined-names-and-systems)))

;;; system-lister takes no argument and returns the system objects.

(defun MK-list-systems ()
  #+nil
  (let ((list nil))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (push (if (consp value)
			   (cdr value)
			 value)
				list))
	     MK::*defined-systems*)
    ;;; currently ignoring ones on the disk.
    ;;; maybe should sort them
    list)
  (mk:defined-systems))

#+nil
(progn
(mk-list-systems)
(mk-list-system-names))

;;; The finder takes a string, and returns a system or a list of
;;; systems. Must not give an error. The match needs to be exact, i.e.
;;; don't do completions.

;;; We need the IGNORE-ERRORS because MK:FIND-SYSTEM gives an
;;; error on a system name that is a wild filename, inside
;;; MK:COMPUTE-SYSTEM-PATH. This needs to be changed
;;; to call WILD-PATHNAME-P before calling PROBE-FILE.

(defun find-system-no-error (name)
  (ignore-errors
    (MK:find-system name :load-or-nil)))

#+nil
(find-system-no-error "cl-http*")

;;; Install the listers and finder for use by LispWorks tools.
(scm:ADD-SYSTEM-NAMESPACE "MK"
			  :name-lister 'MK-list-system-names
			  :system-lister 'MK-list-systems
			  :finder 'find-system-no-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;     mapping over sources        ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SYSTEM      - the result of a call to a finder.
;;; FUNCTION    - function to map: takes one argument,  a pathname.
;;; TYPE        - Either :SOURCE or :OBJECT. In the Search Files tool
;;;               it is always :SOURCE.
;;; MUST-EXIST  - If non-nil, only existsing pathnames should be passed
;;;               to the function.
;;; SKIP-BINARY - If non-nil, skip components where the :SOURCE is
;;;               is in a binary format (LispWorks allows this).

;;; The current definition is pretty minimal, which only copes with
;;; the way it is called by the Search Files tool.


(defmethod scm:MAP-SYSTEM-PATHNAMES ((system MK::component) function &key
				     (type :source)
				     (must-exist nil)
				     skip-binary)
  (declare (ignore type must-exist skip-binary))
  (flet ((f (string)
	   (funcall function (pathname string))))
    (mk::system-map-files system #'f)))


#||
(BOGUS)
(defclass map-source-op (MK:load-source-op) ((function :initarg :function)))
(defmethod MK:operation-done-p ((o map-source-op) (c MK:source-file)) nil)
(defmethod MK:perform ((o map-source-op) (c MK:cl-source-file))
  (let ((source (MK:component-pathname  c)))
    (when (probe-file source)
      (funcall (slot-value o 'function) source))))
||#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;   Module (component) properties   ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Predicate for whether the module (component in MK) is a system
;;; (module in MK). We define a method on MK:component to return T.
;;; There is a default method on T returning nil, which will apply
;;; to all other components.
;;; When this returns non-nil, it tells the caller
;;; that there are applicable methods
;;; for SCM:MAP-SYSTEM-PATHNAMES, SCM:MODULE-NAMESPACE-NAME,
;;; SCM:SYSTEM-PLAN and SCM:EXECUTE-SYSTEM-PLAN on the same object.


(defmethod scm:module-is-system-p ((module MK::component))
  (find (mk::component-type module) '(:defsystem :system)))

;;; Return the name of a module.

(defmethod scm:module-name ((module MK::component))
  (MK::component-name module))

;;; Return the parent of a module.

(defun %mk-find-component-parent (component &optional
				  (list (MK:defined-systems)) (depth 0))
  (loop for x in list
	for children = (MK::component-components x)
	if (find component children) return x
	else return (some (lambda (list)
			    (%mk-find-component-parent component list
						       (1+ depth)))
			  children)))

(defmethod scm:module-parent ((module MK::component))
  (%mk-find-component-parent module))

;;; Return the children of a module.
;;; There is a default method on T returning nil
;;; The results is a list of "children". Each child
;;; is a "module", but provided there is either
;;; SCM:MODULE-PRINT-NAME or SCM:MODULE-NAME defined for it,
;;; it can be anything.

(defmethod scm:module-children ((module MK::component))
  (MK::component-components module))

;;; Returns the pathname of the module.
;;; There is a default method that returns NIL.

(defmethod scm:module-pathname ((module MK::component) parent)
  (declare (ignore parent))
  (case (MK::component-type module)
    ((:system :defsystem)
     (or (MK::compute-system-path (MK::component-name module))
	 (mk::system-definition-pathname (MK::component-name module))))
    (t (and (MK::component-pathname module :source)
	    (pathname (MK::component-full-pathname module :source))))))


;;; Must return a string or NIL. The string is not interpreted.
;;; Probably should contain at least the version.

(defmethod scm:module-flag-description ((module MK::component))
  nil)

;;; What the user sees in the IDE, which is not necessarily the same
;;; as the name.

(defmethod scm:module-print-name ((module MK::component))
  (MK::component-name module))

;;; This needs to match the name given to SCM:ADD-SYSTEM-NAMESPACE above.

(defmethod scm:module-namespace-name ((module MK::component))
  "MK")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;      Plan and execute     ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defstruct MK-event
  op
  component)


;;; Returns a plan, which is interpreted by SCM:EXECUTE-SYSTEM-PLAN below.
;;; ACTIONS is a list contain :COMPILE and :LOAD or both.
;;; In MK LOAD also COMPILEs, so this map both '(:LOAD) and
;;; '(:COMPILE :LOAD) to MK:load-system.
;;; FORCE and SOURCE are booleans.

(defmethod scm:system-plan ((system MK::component) actions
			    &key
			    force
			    source
			    )
  (when-let (op-type (cond ((member :load actions) :load)
			   ((member :compile actions) :compile)))
    (let* ((initargs (append (when force '(:force t))
			     (when source
			       '(:load-source-instead-of-binary t)))))
      (list (make-mk-event :op (list* 'mk:oos system op-type initargs)
			   :component system)))))


;;; This receives the results of scm SCM:SYSTEM-PLAN above, and needs
;;; to execute it.
;;; This is the bottom half of MK:OPERATE.

(defmethod scm:execute-system-plan ((system MK::component) plan)
  (with-compilation-unit ()
    (dolist (event plan)
	(let ((op (MK-event-op event))
	      (component (MK-event-component event)))
	  (loop
	   (restart-case
	       (progn (apply (car op) (cdr op))
		 (return))
	     (retry ()
	       :report
	       (lambda (s)
		 (format s "~@<Retry performing ~S on ~S.~@:>"
			 op component)))
	     (accept ()
	       :report
	       (lambda (s)
		 (format s
			 "~@<Continue, treating ~S on ~S as ~
			       having been successful.~@:>"
			 op component))
	       #+nil			;XXX
	       (setf (gethash (type-of op)
			      (MK::component-operation-times component))
		     (get-universal-time))
	       (return))))))))



;;; This is where the results of compilation go. In MK
;;; it seems it cannot be defined in the module. The
;;; default method just takes the module-pathname, so
;;; this is not needed
#+not-needed
(defmethod scm:system-target-directory ((system MK:module))
  (scm:module-pathname system nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;        Display in preview     ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; This is needed only if the plan that SCM:SYSTEM-PLAN
;;; returns is not a list of events. It needs to return
;;; two values:  a string describing the plan (one line,
;;; which is displayed in the preview tree), and a list of the
;;; events in the plan. In MK the plan is a list, so this
;;; is not needed.

#+not-needed
(defmethod scm:treeify-system-plan ((module MK:module) plan)
  (values "a plan" (plan-events plan)))

;;; This takes the module on which the plan was made (not the one
;;; in the event) and an event in a plan. Needs to return two values:
;;; a string describing the event to show in the preview tree, and
;;; children events.
;;; In MK there are no sub-events, so the second value always NIL.

(defmethod scm:treeify-plan-event ((module MK::component) event)
  (values (format nil "~a on ~a" (third (MK-event-op event))
		  (MK::component-name (MK-event-component event)))
	  nil))

;;; Returns non-nil if the event is a "source" event.  The preview
;;; tree decides which icon to use based on the result.

;; mk:oos name action
(defmethod scm:event-source-p ((event MK-event))
  (find  :load-source-instead-of-binary (cdddr (MK-event-op event))))

;;; Returns the module of the event.

(defmethod scm:event-module ((event MK-event))
  (MK-event-component event))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;        Editor                 ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(editor:define-file-type-hook ("system")
    (buffer type)
  (declare (ignore type))
  (setf (editor:buffer-major-mode buffer) "Lisp"))

;; Use CL-USER as the default package for system files (not quite right: it
;; should be a package that uses MK).
#+nil
(defun set-MK-package (buffer new-mode-p)
  (when new-mode-p
    (let ((pathname (editor:buffer-pathname buffer)))
      (when (and pathname (pathname-match-p pathname "*.system"))
	(editor::set-buffer-current-package buffer (find-package "USER"))))))
#+nil
(editor:add-global-hook editor::LISP-MODE-HOOK 'set-MK-package)
