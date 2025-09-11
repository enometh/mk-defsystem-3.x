;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Touched: Mon Aug 06 13:24:56 2007 +0530 <enometh@net.meer>
;;;   Time-stamp: <>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental. Do not redistribute.
;;;   Copyright (C) 2007-2017 Madhu.  All Rights Reserved.
;;;
;;; Loader for compiling and loading cmucl-init.lisp
;;;
(in-package "CL-USER")

(defpackage "LC-LITE"
  (:use "CL"))
(in-package "LC-LITE")

(defvar +exports+
  '(:export
    "BINARY-DIRECTORY"
    "*SOURCE-DIRECTORY-ROOT*"
    "*BINARY-DIRECTORY-FASL-ROOT*"
    "LC-LITE"
    "LC"
    "WILDSET-LPN-TRANSLATIONS"
    "RESOLVE-UP-DIRECTORY-COMPONENTS"
    "SANITIZE-TILDE-IN-PATHNAME"))

(defvar +interns+
  '(:intern
    "*BINARY-DIRECTORY-PATTERN*"
    "*BINARY-DIRECTORY-FEATURE*"
    "*BINARY-DIRECTORY-VERSION*"
    "*BINARY-DIRECTORY-OS-FEATURE*"
    "*BINARY-DIRECTORY-ENSURE-DIRECTORIES-EXIST*"
    "*BINARY-DIRECTORY-SOURCE-FILE-TYPES*"
    "*BINARY-DIRECTORY-ARCHITECTURE-FEATURE*"
    "REL-SOURCE-PATH"
    "WILDIFY"
    "FUNNEL-SYMBOL"
    ))

;; allow variables exported in package LC-LITE and used in CL-USER
;; (via USE-PACKAGE) to be defined in CL-USER even before loading
;; lc-lite.lisp. FUNNEL-SYMBOL will handle dealing with these symbols
;; when lc-lite.lisp.lisp is loaded.  package1 is always cl-user
;; package2 is always lc-lite.

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun funnel-symbol (name package1 package2 export-p &key (debug-p t))
  (assert (not (equal name "NIL")))
  (check-type name string)
  (flet ((import-one (sym package)
	   (check-type sym symbol)
	   (when debug-p
	     (format t "lc-lite: imported ~A::~A~%" sym
		     (package-name (symbol-package sym))))
	   (import (list sym) package))
	 (export-one (sym package)
	   (check-type sym symbol)
	   (when debug-p
	     (format t "lc-lite: exported ~A::~A~%" sym
		     (package-name (symbol-package sym))))
	   (export (list sym) package)))
    (multiple-value-bind (sym1 stat1) (find-symbol name package1)
      (declare (ignorable stat1))
      (multiple-value-bind (sym2 stat2) (find-symbol name package2)
	(if (and sym1 (not sym2))
	    (progn (import-one sym1 package2)
		   (when export-p
		     (assert (not (eql stat2 :external)))
		     (export-one sym1 package2)))
	    (progn (unless sym2
		     (multiple-value-setq (sym2 stat2)
		       (intern name package2)))
		   (if (and export-p (not (eql stat2 :external)))
		       (export-one sym2 package2)))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist (name (cdr +interns+))
    (funnel-symbol name "CL-USER" "LC-LITE" nil))
  (dolist (name (cdr +exports+))
    (funnel-symbol name "CL-USER" "LC-LITE" t))
  (use-package "LC-LITE" "CL-USER"))


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
	      :gcl :ecl :scl :mkcl :sbcl :armedbear :clasp))
      (cerror "Continue." "Unknown implementation."))
  "List of strings denoting directory components under fasl root.")

; featurelists taken from swank-loader slime-2.23
(defvar *binary-directory-os-feature*
  (or (some (lambda (x)
	      (and (find x *features*) (string-downcase x)))
	      '(:macosx :linux :windows :mswindows :win32 :solaris :darwin
		:sunos :hpux :unix :mezzano))
      (cerror "Continue." "Unknown implementation.")))

(defvar *binary-directory-architecture-feature*
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
		     *binary-directory-architecture-feature*)))))
  "List of strings denoting directory components under implementation's fasl
root.")


(defun resolve-up-directory-components (pathname &aux elem)
  (labels ((rec (com stack depth)
	     (cond ((null com) (nreverse stack))
		   ((and (eq (setq elem (pop com)) :up))
		    (rec com (cdr stack) (1+ depth)))
		   ((equal elem ".") (rec com stack (1+ depth)))
		   (t (rec com (cons elem stack) (1+ depth))))))
    (let ((com (pathname-directory pathname)))
      (if (loop for c in (cdr com)
		if (or (eql c :up) (equal c "."))
		return t)
	  (make-pathname :directory (rec com nil 0) :defaults pathname)
	  pathname))))

(defun rel-source-path (path source-directory-root)
  (let* ((*default-pathname-defaults* #p"")
	 (rel (if source-directory-root
		  (enough-namestring
		   (resolve-up-directory-components path)
		   (resolve-up-directory-components source-directory-root))
		  path))
	 (reld (pathname-directory rel)))
    (when reld
      (ecase (car reld)
	(:absolute (values (last reld) t))
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
				(and source-directory
				     (apply #'binary-directory
					    source-directory
					    (rel-source-path pathname
							     source-directory)))
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


;;; ----------------------------------------------------------------------
;;;
;;;
;;;

(defun wildify (root &rest components)
  "COMPONENTS are directory components. Returns a WILD PATHNAME."
  (merge-pathnames (make-pathname :name :wild :type :wild :version
				  #+clisp :newest
				  #-clisp :wild
				  :directory (append '(:relative) components)
				  :host nil)
		   root
		   nil))

(defun wildset-lpn-translations (HOST ROOT &key wipe dry-run)
  "Wildify directory ROOT and set that as the logical-pathname
translations for HOST."
  (if (null ROOT)			;special case
      (unless dry-run
	(setf (logical-pathname-translations HOST) nil))
      (when (or wipe (not (ignore-errors (logical-pathname-translations HOST))))
	(let ((form `(("*.*.*"
		       ,(wildify root))
		      ("**;*.*.*"
		       ,(wildify root :wild-inferiors))
		      (";**;*.*.*"
		       ,(wildify root :wild-inferiors)))))
	  (if dry-run
	      form
              (setf (logical-pathname-translations HOST) form ))))))

#+nil
(logical-pathname-translations "PROJECTS")

#+nil
(wildset-lpn-translations "PROJECTS" "~/cl/" :wipe nil :dry-run t)

(export 'wildset-lpn-translations))


(defun sanitize-tilde-in-pathname (pathname)
  "Replace a ~ at the start of string PATHNAME with the user's home
directory."
  (let* ((path (if (stringp pathname) pathname (namestring pathname)))
	 (rep (and path (> (length path) 1)
		   (eql (elt path 0) #\~)
		   (eql (elt path 1) #\/)
		   (subseq path 2))))
    (cond (rep (let ((home (identity #+nil probe-file (user-homedir-pathname))))
		 (if (pathnamep pathname)
		     (merge-pathnames rep home)
		   (concatenate 'string
				(namestring home)
				rep))))
	  (t pathname))))

(export 'sanitize-tilde-in-pathname)


;;; ----------------------------------------------------------------------
;;;
;;; ;madhu 250815
;;;

(defstruct compile-ctx
  src-root
  fasl-root)

(defun lc-with-compile-ctx (file-name compile-ctx &rest lc-args)
  (apply #'lc
	 #1=(merge-pathnames file-name (compile-ctx-src-root compile-ctx))
	 :binary-directory
	 (merge-pathnames
	  (make-pathname
	   :directory (cons :relative (rel-source-path
				       #1# (compile-ctx-src-root compile-ctx)))
	   :defaults #2=(compile-ctx-fasl-root compile-ctx))
	  #2#)
	 lc-args))

(defun lcn (n file-list compile-ctx &rest lc-args)
  (let* ((f (elt file-list n)))
    (apply #'lc-with-compile-ctx f compile-ctx lc-args)))

#||
(setq $c (mk::%mk-traverse :numcl #'identity t :never))
(setq $fl (mapcar 'mk::component-source-pathname $c))
(setq $ctx (make-compile-ctx
	    :src-root #1="~/cl/extern/Github/numcl/"
	    :fasl-root (binary-directory #1#)))
(lcn 0 $fl $ctx :dry-run t)
||#

#+nil
(export '(LC-LITE::MAKE-COMPILE-CTX
	  LC-LITE::COMPILE-CTX-SRC-ROOT
	  LC-LITE::LC-WITH-COMPILE-CTX
	  LC-LITE::LCN
	  LC-LITE::COMPILE-CTX-FASL-ROOT)
	'LC-LITE)