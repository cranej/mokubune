(defpackage #:mokubune
  (:use :common-lisp :com.gigamonkeys.pathnames :cl-ppcre)
  (:import-from :alexandria-2 :copy-file)
  (:export #:run))

(defpackage #:mokubune/gemtext
  (:use :common-lisp)
  (:export #:parse
	   #:parse-string
	   #:element
	   #:title
	   #:link
	   #:item
	   #:paragraph
	   #:blockquote
	   #:verbatim
	   #:line-text
	   #:link-url
	   #:title-level
	   #:verbatim-alt
	   #:elememt-p
	   #:title-p
	   #:link-p
	   #:item-p
	   #:paragraph-p
	   #:blockquote-p
	   #:verbatim-p
	   #:gemtext->html))
