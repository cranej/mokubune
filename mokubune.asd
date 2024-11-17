(defpackage #:mokubune-system (:use :common-lisp :asdf))
(in-package #:mokubune-system)

(defsystem "mokubune"
  :author "cranej <crane@cranejin.com>"
  :version (:read-file-form "version.lisp" :at (1 2))
  :description "A static site generator."
  :licence "GPL"
  :serial t
  :components ((:module "pathnames"
		:serial t
		:components ((:file "packages")
			     (:file "pathnames")))
	       (:file "packages")
	       (:file "pattern")
	       (:file "gemtext")
	       (:file "version")
	       (:file "init")
               (:file "mokubune"))
  :depends-on ("alexandria" "str" "cl-ppcre" "cl-template-trim"))
