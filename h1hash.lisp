;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Fri Aug 14 10:58:33 2026 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2026 Madhu.  All Rights Reserved.
;;;
;;; h1: The ("dirhash") of the lisp sources included in an
;;; mk-defsystem system.

(in-package "CL-USER")

#||
(require 'ironclad)
(require 'cl-base64)
(require 'alexandria)
(require 'babel)
||#

#+nil
(defun sha256-data (data &optional (type :hex-string))
  "Return the sha256 digest of DATA in the format specified by TYPE.
DATA is either an array of octets, or a string or a vector which is
coercable to an array of octets.  TYPE is one of :hex-string,
:base64-string or 'vector."
  (etypecase data
    ((simple-array (unsigned-byte 8) (*)) data)
    (string (setq data (babel:string-to-octets data :encoding :utf-8)))
    (vector (setq data (coerce data '(simple-array (unsigned-byte 8))))))
  (let ((digest (ironclad:make-digest :sha256)))
    (ironclad:update-digest digest data)
    (let ((ret (ironclad:produce-digest digest)))
      (ecase type
	((nil 'vector) ret)
	(:hex-string (ironclad:byte-array-to-hex-string ret))
	(:base64-string  (cl-base64:usb8-array-to-base64-string ret))))))

#+nil
(defun sha256-file (path &optional (type :hex-string))
  (sha256-data (alexandria:read-file-into-byte-vector path) type))

(defun h1-summary (real-paths &key root-dir (sha256-file 'sha256-file))
  "Return a multiline \"summary manifest\" string which describes the
given set of files, each line consists of the hexadecimal sha256
digest of the file content, followed by two spaces, followed by the
relative namestring of the file, followed by a newline.  REAL-PATHS is
a list of absolute pathname designators to a list of files.  ROOT-DIR
is a pathname designator of the root directory under which the files
in REAL-PATHS lie.  The files should not have newlines in their names."
  (with-output-to-string (stream)
    (loop for path in (sort (copy-list real-paths) #'string< :key #'namestring)
	  for hash = (funcall sha256-file path :hex-string)
	  do (format stream "~A  ~A~&" hash
		     (enough-namestring path root-dir)))))

(defun h1 (system &key (sha256-data 'sha256-data) (sha256-file 'sha256-file))
  "Compute the H1 (\"dirhash\") of the lisp source files in the given
mk-defsystem SYSTEM.  The second return value is a string describing
the \"summary manifest\" of the files (see \"h1-summary\".)  The first
return value is the string \"h1:\", followed by the base64 encoding of
the sha256 digest of the second return value."
  (let ((man (h1-summary
	      (mk::system-map-files system nil
				    :recursively-handle-deps :never
				    :uniq t)
	      :root-dir (mk::system-relative-pathname system "")
	      :sha256-file sha256-file)))
    (values (concatenate 'string "h1:" (funcall sha256-data man
						:base64-string))
	    man)))


;;; ----------------------------------------------------------------------
;;;
;;; without ironclad but with shell out to openssl dgst
;;;

(defun call-with-temporary-file ;; on loan from uiop
    (thunk &key
     (want-stream-p t) (want-pathname-p t) (direction :io) keep after
     (temporary-directory "/tmp/")
     directory (type "tmp" typep) prefix (suffix (when typep "-tmp"))
     (element-type  (or #+(or abcl cmucl cormanlisp scl xcl) 'character
			#+lispworks 'lw:simple-char
			:default))
     dry-run-p
     (external-format (or (or #+clisp charset:utf-8 :utf-8 :default))))
  "Call a THUNK with stream and/or pathname arguments identifying a temporary file.

The temporary file's pathname will be based on concatenating
PREFIX (or \"tmp\" if it's NIL), a random alphanumeric string,
and optional SUFFIX (defaults to \"-tmp\" if a type was provided)
and TYPE (defaults to \"tmp\", using a dot as separator if not NIL),
within DIRECTORY (defaulting to the TEMPORARY-DIRECTORY) if the PREFIX isn't absolute.

The file will be open with specified DIRECTION (defaults to :IO),
If WANT-STREAM-P is true (the defaults to T), then THUNK will then be CALL'ed
with the stream and the pathname (if WANT-PATHNAME-P is true, defaults to T),
and stream will be closed after the THUNK exits (either normally or abnormally).
If WANT-STREAM-P is false, then WANT-PATHAME-P must be true, and then
THUNK is only CALLED'ed after the stream is closed, with the pathname as argument.
Upon exit of THUNK, the AFTER thunk if defined is CALL'ed with the pathname as argument.
If AFTER is defined, its results are returned, otherwise, the results of THUNK are returned.
Finally, the file will be deleted, unless the KEEP argument when CALL'ed returns true."
  (check-type direction (member :output :io))
  (assert (or want-stream-p want-pathname-p))
  (flet ((call-function (func &rest args)
	   (apply (etypecase func
		    (function func)
		    ((or boolean keyword character number pathname)
		     (constantly func))
		    (symbol (fdefinition func)))
		  args)))
    (let* ((prefix-pn (merge-pathnames (or prefix "tmp")
				       (or directory
					   temporary-directory)))
	   (prefix-nns (namestring prefix-pn))
	   (results (progn (ensure-directories-exist prefix-pn :verbose t) nil)))
      (loop for counter :from (random (expt 36 #-gcl 8 #+gcl 5))
	    for pathname = (pathname
			    (format nil "~A~36R~@[~A~]~@[.~A~]"
				    prefix-nns counter suffix
				    (unless (eq type :unspecific) type)))
	    for okp = nil do
            (unwind-protect
		 (progn
		   (ensure-directories-exist pathname :verbose t)
		   (when dry-run-p
		     (return-from call-with-temporary-file
		       (list 'pathname pathname)))
		   (with-open-file (stream pathname
					   :direction direction
					   :element-type element-type
					   :external-format external-format
					   :if-exists nil :if-does-not-exist :create)
                     (when stream
		       (setq okp pathname)
		       (setq results
			     (multiple-value-list
			      (if want-stream-p
				  (if want-pathname-p
                                      (funcall thunk stream pathname)
                                      (funcall thunk stream))
				  (funcall thunk pathname))))))
		   (cond (dry-run-p t)
			 ((not okp) nil)
			 (after (return (call-function after okp)))
			 ((and want-pathname-p (not want-stream-p))
			  (return (call-function thunk okp)))
			 (t (return (values-list results)))))
	      (when (and okp (not (call-function keep)))
		(ignore-errors (delete-file okp))))))))

(defun %sha256-file (file1 &optional (type :hex-string))
  (flet ((%file->bytes (path)
	   (with-open-file (stream path :element-type '(unsigned-byte 8))
	     (let* ((len (file-length stream))
		    (buf (make-array len :element-type '(unsigned-byte 8)))
		    (beg 0))
	       (loop for nbytes = (read-sequence buf stream :start beg)
		     if (>= (incf beg nbytes) len) return buf)))))
    (call-with-temporary-file
     (lambda (file2)
       (mk::run-shell-command
	"openssl dgst -sha256 -binary -out ~A ~A"
	(list file2 file1)))
     :want-stream-p nil
     :after (lambda (file)
	      (ecase type
		((nil 'vector) (%file->bytes file))
		(:hex-string
		 (with-output-to-string (out)
		   (with-open-file (stream file :element-type '(unsigned-byte 8))
		     (loop for i below 32
			   for c = (read-byte stream)
			   do (format out "~(~2,'0X~)" c)))))
		(:base64-string
		 (call-with-temporary-file
		  (lambda (file3)
		    (make:run-shell-command
		     "openssl enc -base64  -out ~A -in ~A"
		     (list file3 file)))
		  :want-stream-p nil
		  :after (lambda (file)
			   (with-open-file (stream file)
			     (read-line stream nil))))))))))

(defun %sha256-data (data &optional (type :hex-string))
  (if (stringp data) ;; doesn't handle unicode
      (setq data (map 'vector 'char-code data)))
  (call-with-temporary-file
   (lambda (stream file)
     (declare (ignore file))
     (loop for x across data
	   do (write-byte x stream)))
   :element-type '(unsigned-byte 8)
   :after (lambda (file1)
	    (%sha256-file file1 type))))

#||
(equalp
(time
 (multiple-value-list (h1 "alexandria" :sha256-file '%sha256-file
			       :sha256-data '%sha256-data)))
(time (multiple-value-list (h1 "alexandria"))))
||#