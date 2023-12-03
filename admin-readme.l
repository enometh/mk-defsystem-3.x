;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Sun Dec 03 08:04:44 2023 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2023 Madhu.  All Rights Reserved.
;;;
;;; maintainer code for readme.org
;;;
(cl:error "JUNK AT EOF")		;not meant to be loaded
(defpackage "MK-LANGUAGE-HOOKS-README"
  (:use "CL" "CL-USER"))
(in-package "MK-LANGUAGE-HOOKS-README")

(defvar $srcs
"~/cl/extern/Github/cl-protobufs/mk-defsystem-protobufs-hooks.lisp
~/cl/extern/Github/eco/src/eco-mk-defsystem-hooks.lisp
~/cl/extern/Github/magicl/mk-defsystem-magicl-expokit-hooks.lisp
~/cl/extern/cffi/grovel/mkdefsystem-hooks.lisp
~/cl/extern/cffi/src/c2ffi/mk-defsystem-c2ffi-hooks.lisp
~/cl/extern/claw-cxx/src/util/mk-defsystem-hooks.lisp")

(defvar $prefixes '("~/cl/extern/Github/" "~/cl/extern/")
  "sorted in descending order of length.")

(defvar $file-paths (with-input-from-string (stream $srcs)
		      (read-lines-from-stream stream)))

(defun sanitize-tilde-in-pathname (path)
  (or (and path (> (length path) 1)
	   (eql (elt path 0) #\~)
	   (eql (elt path 1) #\/)
	   (concatenate 'string
			(namestring (probe-file (user-homedir-pathname)))
			(subseq path 2)))
      path))

(defun strip-prefix (line)
  "whichever matches first"
  (let ((line (sanitize-tilde-in-pathname line)))
    (loop for prefix in (mapcar 'sanitize-tilde-in-pathname $prefixes)
	  if (prefixp prefix line)
	  return (subseq line (length prefix)))))

#+nil
(map 'list #'strip-prefix $file-paths)

(defun rsync-relative-frob (line)
  (let* ((stripped (strip-prefix line))
	 (p (search stripped line)))
    (assert (char= (elt line (1- p)) #\/))
    (concatenate 'string (subseq line 0 p) "./"
		 stripped)))

#+nil
(rsync-relative-frob (sanitize-tilde-in-pathname (elt $file-paths 3)))

(defvar $root "~/cl/extern/defsystem-3.x/language-hooks/")

(defun make-directories ()
  (cons 'progn
	(map 'list (lambda (x)
		     (list 'ensure-directories-exist
			   (merge-pathnames x $root)
			   :verbose t))
	     (map 'list #'strip-prefix $file-paths))))

#+nil
(eval (make-directories))

(defun dump-srcs-list (dump-path)
  "dump-path is written to. a file list, to be used as the
--files-from parameter for rsync."
  (dump-lines
   (map 'list 'rsync-relative-frob
	(map 'list 'sanitize-tilde-in-pathname $file-paths))
   dump-path))

#+nil
(dump-srcs-list "/tmp/1")

#||
rsync -i -avzH --inplace -OJX --files-from=/tmp/1 / ./
||#

(defun dump-link-for (line &optional stream)
  "returns a string if stream is NIL"
  (format stream "[[~A][~A]]" (strip-prefix line) line))

#+nil
(dump-link-for (elt $file-paths 0))

#+nil
(dump-lines (map 'vector 'dump-link-for $file-paths) "/dev/shm/2")
