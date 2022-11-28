;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Touched: Mon Aug 06 13:24:56 2007 +0530 <enometh@net.meer>
;;;   Time-stamp: <2020-05-07 18:47:29 IST>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental. Do not redistribute.
;;;   Copyright (C) 2007-2017 Madhu.  All Rights Reserved.
;;;
;;; Loader for compiling and loading cmucl-init.lisp
;;;
(in-package "CL-USER")

;;; ----------------------------------------------------------------------
;;;
;;; BINARY-DIRECTORY. (LC-LITE)

(progn
(defvar *source-directory-root* nil)

(defvar *binary-directory-fasl-root* "FASL:"
  "Root Directory of fasl files.") ;XXX

(defvar *binary-directory-pattern*
  (or (some (lambda (x)
	      (and (find x *features*)
		   (list (concatenate 'string "binary-" (string-downcase x)))))
	    '(:allegro :lispworks :ccl :clozure :cmu :clisp
	      :gcl :ecl :scl :mkcl :sbcl))
      (cerror "Continue." "Unknown implementation."))
  "List of strings denoting directory components under fasl root.")

; featurelists taken from swank-loader slime-2.23
(defvar *binary-directory-os-feature*
  (or (some (lambda (x)
	      (and (find x *features*) (string-downcase x)))
	      '(:macosx :linux :windows :mswindows :win32 :solaris :darwin
		:sunos :hpux :unix :mezzano))
      (cerror "Continue." "Unknown implementation.")))

(defvar *binary-architecture-feature*
  (or (some (lambda (x)
	      (and (find x *features*) (string-downcase x)))
	      '(:powerpc :ppc :x86 :x86-64 :x86_64 :amd64 :i686 :i586
		:i486 :pc386 :iapx386 :sparc64 :sparc :hppa64 :hppa
		:arm :armv5l :armv6l :armv7l :arm64
		:pentium3 :pentium4
		:mips :mipsel
		:java-1.4 :java-1.5 :java-1.6 :java-1.7))
      (cerror "Continue." "Unknown implementation.")))

(defvar *binary-directory-version*
  (or #+(and nil scl)
      (list
       (let* ((backend (c:backend-name c:*target-backend*))
	      (version (c:backend-fasl-file-version c:*target-backend*)))
	 (format nil "scl-~A-~D.~D.~D.~D" backend
		 (ldb (byte 8 24) version)
		 (ldb (byte 8 16) version)
		 (ldb (byte 8 8) version)
		 (ldb (byte 8 0) version))))
      #+(and nil cmu)
      (list
       (format nil "~(~x~)" (c:backend-fasl-file-version c:*target-backend*)))
     #+clisp
      (let ((s (lisp-implementation-version)))
        (list (subseq s 0 (position #\space s))))
      (list
       (apply #'concatenate
	      'string
	      (substitute-if
	       #\_
	       (lambda (c)
		 (member c (list #+unix #\/
				 ;;XXX #\Space
				 #+mswindows #\;
				 #+mswindows #\:
				 #+mswindows #\,)))
	       (lisp-implementation-version))
	      #+(and ecl nil)
	      (let ((fun (find-symbol "LISP-IMPLEMENTATION-VCS-ID" :ext)))
		(when fun
		  (let ((vcs-id (funcall fun)))
		    (when (>= (length vcs-id) 8)
                      (concatenate 'string "-" (subseq vcs-id 0 8))))))
	      #+ecl64
	       "_x86_64"
	      (or
	       #+clozure nil
	       #+nil
	       (list "-"
		     *binary-directory-os-feature*
		     "-"
		     *binary-architecture-feature*)))))
  "List of strings denoting directory components under implementation's fasl
root.")


(defun resolve-up-directory-components (pathname &aux elem)
  (labels ((rec (com stack depth)
	     (cond ((null com) (nreverse stack))
		   ((and (eq (setq elem (pop com)) :up))
		    (rec com (cdr stack) (1+ depth)))
		   (t (rec com (cons elem stack) (1+ depth))))))
    (let ((com (pathname-directory pathname)))
      (if (find :up (cdr com))
	  (make-pathname :directory (rec com nil 0) :defaults pathname)
	  pathname))))

;; UNUSED
(defun rel-source-path (path)
  (let* ((*default-pathname-defaults* #p"")
	 (rel (enough-namestring path *source-directory-root*))
	 (reld (pathname-directory rel)))
    (when reld
      (ecase (car reld)
	(:absolute (values (cdr reld) t))
	(:relative (values (cdr reld) nil))))))

(defun binary-directory (pathname &rest dirnames)
  (when (or #+allegro (excl::logical-pathname-p (pathname pathname))
	    #+(or clisp lispworks) (system::logical-pathname-p (pathname pathname))
	    #+clozure (CCL::LOGICAL-PATHNAME-P (pathname pathname))
	    #+ecl(SI:LOGICAL-PATHNAME-P (pathname pathname))
	    #+cmu (LISP::LOGICAL-PATHNAME-P (pathname pathname)))
    (setq pathname (translate-logical-pathname pathname)))
  ;; if *source-directory-root* is specified, then our
  ;; binary-directory is a path relative to that. Otherwise it is the
  ;; last directory component in pathname.
  (let* ((*default-pathname-defaults* #p"")
	 (rel (if *source-directory-root*
		  (enough-namestring (resolve-up-directory-components pathname)
				     (resolve-up-directory-components
				      *source-directory-root*))
		  pathname))
	 (reld (pathname-directory rel))
	 (last (and reld
		    (ecase (car reld)
		      (:absolute (last reld))
		      (:relative (cdr reld))))))
    #+cmu
    (when (lisp::search-list-p (car last)) ;handle "host:/skt.lisp"
      (assert (lisp::search-list-defined (cadr reld)))
      (setq last nil))
    (let ((p (make-pathname :name nil :type nil :version nil
			    :defaults *binary-directory-fasl-root*
			    :directory
			    (append
			     (pathname-directory *binary-directory-fasl-root*)
			     last
			     *binary-directory-pattern*
			     *binary-directory-version*
			     dirnames))))
      p)))

(defvar *binary-directory-ensure-directories-exist* t)
(defvar *binary-directory-source-file-types* '("lisp" "l" "cl" "lsp"))

(defun lc (pathname &key library-p
	   force
	   (source-directory *default-pathname-defaults*)
	   (source-file-types *binary-directory-source-file-types*)
	   (create-directories *binary-directory-ensure-directories-exist*)
	   binary-directory
	   dry-run)
  "Compile and load pathnanme. CREATE-DIRECTORIES has no effect during
DRY-RUN."
  (prog* ((*default-pathname-defaults* source-directory)
	  ;; we work with the resolved realpath file except for the
	  ;; purpose of computing the binary-directory path.
	  (source-truename
	   (or (probe-file pathname)
	       (when (null (pathname-type pathname))
		 (some (lambda (source-file-type)
			 (probe-file
			  (make-pathname :type source-file-type
					 :defaults pathname)))
		       source-file-types))))
	  (binary-directory (or binary-directory
				(binary-directory pathname)
				(and source-truename ;nop
				     (binary-directory source-truename))))
	  (fasl
	   (let ((purported-fasl (compile-file-pathname
				  (or pathname source-truename))))
	     (make-pathname :name
			    (if library-p
				(concatenate 'string
					     (pathname-name purported-fasl)
					     "-library")
				(pathname-name purported-fasl))
			    :version (pathname-version purported-fasl)
			    :type (pathname-type purported-fasl)
			    :defaults binary-directory))))
     (if dry-run (return (values fasl pathname source-truename)))
     retry
     (unless source-truename
       (error "Could not find source file: ~A." pathname))
     (if create-directories (ensure-directories-exist fasl))
     (when (or force
	       (< (or (handler-case (file-write-date fasl)
			(error (e)
			  (format t "Error: file-write-date: ~A: ~A." fasl e)
			  0))
		      0)
		  (file-write-date source-truename)))
       (print (list 'compile-file source-truename :output-file fasl))
       (multiple-value-bind (output-truename warnings-p failure-p)
	   (compile-file source-truename :output-file fasl)
	 (if failure-p			;XXX
	     (format t "LC: compile-file returned: ~A."
		     (list output-truename warnings-p failure-p))
	     (assert (equal (truename fasl) (truename output-truename))))))
     (print (list 'load fasl))
     load-fasl
     (restart-case (load fasl)
       (delete-fasl-and-retry ()
	 :report (lambda (stream)
		   (format stream "Delete ~A and retry compilation." fasl))
	 :test (lambda (condition)
		 (declare (ignore condition))
		 (probe-file fasl))
	 (delete-file fasl)
	 (go retry)))))

(setf (symbol-function 'lc-lite) #'lc)

(export '(binary-directory lc))

(defun wildset-lpn-translations (HOST ROOT &key wipe dry-run)
  "Wildify directory ROOT and set that as the logical-pathname
translations for HOST."
  (if (null ROOT)			;special case
      (unless dry-run
	(setf (logical-pathname-translations HOST) nil))
      (when (or wipe (not (ignore-errors (logical-pathname-translations HOST))))
	(let ((form `(("*.*.*"
		       ,(merge-pathnames
			 (make-pathname :host nil :name :wild :type :wild :version :wild
					:directory '(:relative))
			 ROOT nil))
		      ("**;*.*.*"
		       ,(merge-pathnames
			 (make-pathname :host nil :name :wild :type :wild :version :wild
					:directory '(:relative :wild-inferiors))
			 ROOT nil))
		      (";**;*.*.*"
		       ,(merge-pathnames
			 (make-pathname :host nil :name :wild :type :wild :version :wild
					:directory '(:relative :wild-inferiors))
			 ROOT nil)))))
	  (if dry-run
	      form
              (setf (logical-pathname-translations HOST) form ))))))

#+nil
(logical-pathname-translations "PROJECTS")

#+nil
(wildset-lpn-translations "PROJECTS" "~/cl/" :wipe nil :dry-run t)

(export 'wildset-lpn-translations))
