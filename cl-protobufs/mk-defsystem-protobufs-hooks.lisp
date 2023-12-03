;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Sun Dec 25 10:43:54 2022 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2023 Madhu.  All Rights Reserved.
;;;
;;; also see copyrights in asdf.lisp. another example of this type of
;;; thing is cffi/src/c2ffi/mk-defsystem-c2ffi-hooks.lisp.

;; define a mk-defsystem language:
;;
;; :LANGUAGE :PROTOBUF
;;
;; mappings asdf -> mk
;; :protobuf-source-file -> :file
;;
;; :proto-pathnane <p1> :proto-search-path <p2> -> (:compiler-option :proto-pathname <p1> :proto-search-path <p2>)
;; (my-replace-regexp ":protobuf-source-file \\(\"[^\"]+\"\\)" ":file \\1\n:language :protobuf")
;;
;;     :compiler #'(lambda (input-file &rest args &key output-file
;;			 &allow-other-keys)
;;
;;     :loader #'(lambda (filespec &rest args))
;;



(defpackage #:protobuf-config
  (:documentation "Configuration information for PROTOBUF.")
  (:use #:common-lisp)
  (:export *protoc-relative-path*))

(in-package #:protobuf-config)

(defvar *protoc-relative-path* nil
  "Supply relative proto file paths to protoc, the protobuf compiler?")

(defpackage #:protobuf-system
  (:documentation "System definitions for protocol buffer code.")
  (:use #:common-lisp
        #:protobuf-config)
  (:export #:protobuf-source-file
           #:proto-pathname
           #:search-path))

(in-package #:protobuf-system)

(defvar *protobuf-source-file-options* 	; via :compiler-options <plist>
  '(:proto-pathname   ; slot relative-proto-pathname reader proto-pathame
    :proto-search-path		 ; slot search-path reader search-path
    )
)

(defun resolve-relative-pathname (path parent-path)
  "When PATH doesn't have an absolute directory component, treat it as relative
to PARENT-PATH."
  (let* ((pathname (pathname path))
         (directory (pathname-directory pathname)))
    (if (and (list directory) (eq (car directory) :absolute))
        pathname
        (let ((resolved-path (merge-pathnames pathname parent-path)))
          (make-pathname :directory (pathname-directory resolved-path)
                         :name nil
                         :type nil
                         :defaults resolved-path)))))

;; of course the design features of mk-defsystem state 1) that the
;; input and outpput paths are resolved at the time the system is
;; read, 2) and components have no notion of their parents, so 3) the
;; (load or compile) operation should not and cannot make decisions on
;; the component state. the cl-protobufs asdf code takes advantage of
;; all those design misfeatures which are present in asdf. so the
;; following two abstractions proto-input (analogous to the asdf code) are
;; wretched hacks at best

(defun proto-input (input-file proto-pathname)
  (if proto-pathname
      (merge-pathnames
       (make-pathname :type "proto")
       (merge-pathnames (pathname proto-pathname) input-file))
      (merge-pathnames (make-pathname :type "proto") input-file)))

(defun resolve-search-paths (full-source-path search-paths)
  (let ((parent-path
	 (make-pathname :name nil :type nil :version nil
			:directory (pathname-directory full-source-path)
			:defaults full-source-path)))
    (mapcar (lambda (path)
	      (resolve-relative-pathname path parent-path))
	    search-paths)))

(defun proto-to-lisp-pathname (input-file &key output-file)
  "The analogue of COMPILE-FILE-PATHNAME"
  (assert (equal (pathname-type input-file) "proto"))
  (make-pathname
   :name (pathname-name input-file)
   :type "lisp"
   :version nil
   :defaults (or output-file
		 (compile-file-pathname (make-pathname
					 :name (pathname-name input-file)
					 :type "lisp"
					 :version nil
					 :defaults input-file)))))


(defun string-escape-chars (string &key
			    (char-bag #(#\Space #\Tab #\Newline #\[ #\] #\( #\) #\"))
			    (test #'eql) (escape-char #\\))
  "Known Broken version"
  (and (atom char-bag) (not (vectorp char-bag)) (setq char-bag (list char-bag)))
  (coerce (loop for x across string ;; for prev = nil then x
		if (find x char-bag :test test) collect escape-char end
		collect x)
	  'string))

;; the other keys are listed in *protobuf-source-file-options*, to wit
;; proto-pathname proto-search-path.
(defun proto-to-lisp-compiler (input-file &rest args &key ((:output-file output-fasl-file)) &allow-other-keys)
  (format t "~%~S~%" `(proto-to-lisp-compiler ,input-file ,@args))
  (let* ((proto-pathname (getf args :proto-pathname))
	 (source-file (proto-input input-file proto-pathname))
	 (source-file-argument (if *protoc-relative-path*
				  (file-namestring source-file)
				  (namestring source-file)))
	 (output-file (proto-to-lisp-pathname input-file :output-file output-fasl-file))
	 (search-path
	  (cons (directory-namestring input-file) ;xxx
		(resolve-search-paths input-file
				      (getf args :proto-search-path))))
	 (command (format nil "protoc --proto_path=~{~A~^:~} --cl-pb_out=output-file=~A:~A ~A ~
                               --experimental_allow_proto3_optional"
                          (mapcar 'string-escape-chars search-path)
                          (string-escape-chars (file-namestring output-file))
                          (string-escape-chars (directory-namestring output-file))
                          (string-escape-chars source-file-argument)))
	)
    (when (or (not (probe-file output-file))
	      (> (file-write-date source-file) (file-write-date output-file)))
      (multiple-value-bind (output error-output status)
	  (uiop:run-program command :output t :error-output :output :ignore-error-status t)
	(declare (ignore output error-output))
	(unless (zerop status)
	  (error "protobuf-compile-failed: Failed to compile proto file.  Command: ~S" command))))
    (when (or (not (probe-file output-fasl-file))
	      (> (file-write-date output-file) (file-write-date output-fasl-file)))
      (compile-file output-file :output-file output-fasl-file))))


(mk:define-language :protobuf
    :source-extension "proto"
    :compiler #'(lambda (input-file &rest args &key output-file
			 &allow-other-keys)
		  (declare (ignorable output-file))
		  ;;(format t "~%~S~%" `(proto-to-lisp ,input-file ,@args))
		  (apply #'proto-to-lisp-compiler input-file args))
    :loader #'(lambda (filespec &rest args)
		(format t "~%~S~%" `(proto-loader ,filespec ,@args))
		(assert (not mk::*load-source-instead-of-binary*) nil "Unsupported")
		(assert (not mk::*load-source-if-no-binary*) nil "Unsupported")
		
		(apply #'load filespec args))
    :output-files #'(lambda (c)
		      (let* ((input (mk::component-full-pathname c :source))
			     (output (mk::component-full-pathname c :binary))
			     (generated (proto-to-lisp-pathname input :output-file output)))
			(list generated output))))





#||
(load "/home/madhu/cl/extern/Github/cl-protobufs/cl-protobufs.system")
(eql (mk::component-language  (setq $a (mk::find-component :cl-protobufs '("well-known-types" "descriptor")))) :protobuf)
(resolve-search-paths (mk::component-full-pathname $a :source)
		      '("google/protobuf/"))
(resolve-relative-pathname  "google/protobuf/descriptor.proto"
			    (mk::component-full-pathname $a :source))
(eql nil (resolve-search-paths "/dev/null" nil))
(mk::component-full-pathname $a :source)
;; =>"/home/madhu/cl/extern/Github/cl-protobufs/descriptor.proto"
(mk::component-full-pathname $a :binary)
;; => "/home/madhu/fasl/cl-protobufs/binary-lispworks/8.0.1/descriptor.64ufasl"
;; this should be side by side with the fasl location
(proto-to-lisp-pathname (mk::component-full-pathname $a :source)
			:output-file (mk::component-full-pathname $a :binary))
;; => #P"/home/madhu/fasl/cl-protobufs/binary-lispworks/8.0.1/descriptor.lisp"
(funcall (mk::language-output-files (mk::find-language :protobuf)) $a)
(probe-file
 (proto-input (mk::component-full-pathname $a :source) ;input-file
	     (getf (mk::component-compiler-options $a) :proto-pathname) ;proto-pathname
	     ))

(user::add-to-env "PATH" (namestring (mk::system-relative-pathname :cl-protobufs "protoc")))
(proto-to-lisp-compiler
  "/home/madhu/cl/extern/Github/cl-protobufs/descriptor.proto"
  :output-file
  "/home/madhu/fasl/cl-protobufs/binary-lispworks/8.0.1/descriptor.64ufasl"
  :proto-pathname "google/protobuf/descriptor.proto"
  :proto-search-path ())
(mk:load-system :cl-protobufs)
(mk:compile-system :cl-protobufs)
(mk::needs-compilation $a t)
(mk::compile-file-operation--internal $a nil)
||#
