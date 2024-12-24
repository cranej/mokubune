(defpackage #:mokubune-system (:use :common-lisp :asdf))
(in-package #:mokubune-system)

(defsystem "mokubune"
  :author "Jin, ChunHe <crane@cranejin.com>"
  :version (:read-file-form "version.lisp" :at (1 2))
  :description "A static site generator."
  :licence "GPL"
  :serial t
  :components ((:file "src/packages")
               (:file "version")
               (:module "src"
		:serial t
		:components ((:file "gemtext")
	                     (:file "init")
                             (:file "path")
                             (:file "page")
                             (:file "stage")
                             (:file "mokubune"))))
  :depends-on ("str" "cl-ppcre" "cl-glob" "cl-template-trim" "cl-fad"))
