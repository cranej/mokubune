(defpackage #:mokubune-system (:use :common-lisp :asdf))
(in-package #:mokubune-system)

(defsystem "mokubune"
  :author "cranej"
  :version "0.1.0"
  :description "A static site generator."
  :licence "GPL"
  :serial t
  :components ((:file "packages")
               (:file "mokubune")
	       (:file "init"))
  :depends-on ("alexandria" "str" "pathnames" "cl-ppcre" "cl-template"))
