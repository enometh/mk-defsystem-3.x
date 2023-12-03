(in-package "ECO")

(defvar eco-template-options '(:package))

(mk:define-language :eco-template
    :compiler 'eco-template-compile
    :loader 'eco-template-load
    :source-extension "eco"
    :binary-extension "lisp")

(defun eco-template-compile (input-file &rest args &key output-file &allow-other-keys)
  (format t "~%~S~%" `(eco-template-compile ,input-file ,@args))
  (with-open-file (stream output-file :direction :output
			  :if-exists :supersede)
    (let* ((package (or (getf args :package) :eco-template))
	   (parsed (eco.parser:parse-pathname input-file))
	   (compiled (eco.compiler:compile-template parsed package))
	   (*print-circle* t))
      (print compiled stream))))

(defun eco-template-load (filespec &rest args)
  (format t "~%~S~%" `(eco-template-load ,filespec ,@args))
  (load filespec))
