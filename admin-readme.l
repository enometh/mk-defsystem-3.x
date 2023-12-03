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
      (and (equal path "~") (namestring (user-homedir-pathname)))
      path))

(defun split-on-prefix (line prefixes &key (sanitize-line-p t)
			(sanitize-prefix-p t))
  "Search for a prefix of LINE from among PREFIXES. return as values the
first prefix which matches, and the suffix of the line (stripped off
the prefix."
  (loop with sanitized-line = (if sanitize-line-p
				  (sanitize-tilde-in-pathname line)
				  line)
	for prefix in prefixes
	for sanitized-prefix = (if sanitize-prefix-p
				    (sanitize-tilde-in-pathname prefix)
				    prefix)
;;	do (debug "=> " prefix sanitized-prefix line sanitized-line #\Newline)
	if (prefixp sanitized-prefix sanitized-line)
	return (values prefix
		       (subseq sanitized-line (length sanitized-prefix)))))

(defun strip-prefix (line)
  "whichever matches first. USES $PREFIXES"
  (nth-value 1 (split-on-prefix line $prefixes))

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
  #+nil
  (format stream "- [[file:~A][~A]]" (strip-prefix line) line)
  (format stream "- [[file:~A]]" (strip-prefix line)))

#+nil
(dump-link-for (elt $file-paths 0))

#+nil
(dump-lines (map 'vector 'dump-link-for $file-paths) "/tmp/manifest.org")

;;;
;;;
;;;
(defun split-on-first-dir (file-path prefixes  &rest key-args &key &allow-other-keys)
  "First calls SPLIT-ON-PREFIX on FILE-PATH. The first dir is
the first component of the returned suffix.  Returns as values the
prefixed-path to the first dir, and the file path."
  (multiple-value-bind (prefix suffix)
      (apply #'split-on-prefix file-path prefixes key-args)
    (let ((p (position #\/  suffix)))
      (values (user::join-namestrings prefix (subseq suffix 0 p))
	      (subseq suffix (1+ p))))))

#+nil
(split-on-first-dir "~/cl/extern/clx-portable/clx-portable.system"
		    $prefixes)

#+nil
(progn
(require 'mustache "~/cl/mustache.lisp")
(import 'mustache::mustache )

(defun get-last-commit-info (file-path)
  (multiple-value-bind (first-dir rel-path)
      (split-on-first-dir file-path $prefixes)
    (in-directory (sanitize-tilde-in-pathname first-dir)
      (with-open-pipe (stream
		       (mustache
"echo -n 'Branch: ';
git branch --show-current;
echo -n 'Remote: ';
git config --get remote.origin.url;
echo -n 'Modified: ';
date -r {{rel-path}}
git log -1 --no-decorate --pretty=fuller -- {{rel-path}} |head -5|egrep -v '^(Author|Commit):'"))
	(read-lines-from-stream stream)
	#+nil
	(slurp-stream stream nil :element-type 'character)))))

#+nil
(with-open-file (stream "/tmp/COMMIT_MESSAGE" :direction :output
			:if-does-not-exist :create
			:if-exists :supersede)
  (map nil (lambda (file-path)
	     (format stream "* ~A~&" (strip-prefix file-path))
	     (write-lines-to-stream (get-last-commit-info file-path) stream)
	     (terpri stream))
       $file-paths))
