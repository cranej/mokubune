(defpackage #:mokubune-system (:use :common-lisp :asdf))
(in-package #:mokubune-system)

(defsystem "mokubune"
  :author "cranej"
  :version (:read-file-form "version.lisp" :at (1 2))
  :description "A static site generator."
  :licence "GPL"
  :serial t
  :components ((:file "packages")
               (:file "mokubune")
	       (:file "init")
	       (:file "version"))
  :depends-on ("alexandria" "str" "pathnames" "cl-ppcre" "cl-template"))
