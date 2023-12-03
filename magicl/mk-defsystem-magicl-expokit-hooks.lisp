;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Fri Jun 16 17:22:06 2023 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2023 Madhu.  All Rights Reserved.
;;;
;;; also see copyrights in asdf.lisp. another example of this type of
;;; thing is cffi/src/c2ffi/mk-defsystem-c2ffi-hooks.lisp.

;; define a mk-defsystem language:
;;
;; :LANGUAGE :EXPOKIT
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
(defpackage "MAGICL-EXPOKIT-MK-DEFSYSTEM-HOOKS" (:use "CL"))
(in-package "MAGICL-EXPOKIT-MK-DEFSYSTEM-HOOKS")


;;; Adapted from commonqt's qt.asd
#+nil
(defclass f->so (asdf:source-file)
  ()
  (:default-initargs
   :type "f"))

(defun dynamic-library-extension ()
  "Return the dynamic library extension on the current OS as a string."
  (cond
    ((uiop:os-windows-p) "dll")
    ((uiop:os-macosx-p)  "dylib")
    ((uiop:os-unix-p)    "so")
    (t                   (error "unsupported OS"))))


(defun dll-output-file (defaults)
  (make-pathname :name "libexpokit"
		 :type (dynamic-library-extension)
		 :defaults defaults))


(defun output-files (c)
  (let* ((input (mk::component-full-pathname c :source))  ; source fortran
	 (output (mk::component-full-pathname c :binary)) ; object file
	 (generated (dll-output-file output)))
    (declare (ignorable input))
    (list output generated)))

#+nil
(merge-pathnames "a/b/f.c" "/c/d/f.fas")

(defmethod expokit-compile-file
    (input-file &rest args &key output-file &allow-other-keys)
  (declare (ignorable args))
  (assert (eq (car (pathname-directory output-file)) :absolute))
  (flet ((nn (x) (uiop:native-namestring x)))
    (let* ((fortran-file input-file)
	   (object-file output-file)
           (shared-object (dll-output-file output-file)))
      (ensure-directories-exist shared-object :verbose t)
      (uiop:run-program
       (list "gfortran" "-fPIC" "-std=legacy"
             "-c"
             (nn fortran-file)
             "-o"
             (nn object-file)))
      (uiop:run-program
       (list "gfortran" #+darwin "-dynamiclib" #-darwin "-shared"
             "-o" (nn shared-object)
             (nn object-file)
             #+darwin "-lblas"
             #+darwin "-llapack"))
      (delete-file object-file))))


(mk:define-language :expokit
    :source-extension "f"
    :binary-extension "o"
    :compiler #'(lambda (input-file &rest args &key output-file
			 &allow-other-keys)
		  (declare (ignorable output-file))
		  (format t "~%~S~%" `(expokit-compile-file ,input-file ,@args))
		  (apply #'expokit-compile-file input-file args))
    :loader #'(lambda (filespec &rest args)
		(format t "~%~S: NOP~%" `(expokit-load ,filespec ,@args)))
    :output-files #'output-files)


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
