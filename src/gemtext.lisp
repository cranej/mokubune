#|
Copyright 2020, 2022, 2024 Omar Polo <op@omarpolo.com>

Permission to use, copy, modify, and/or distribute this software for
any purpose with or without fee is hereby granted, provided that the
above copyright notice and this permission notice appear in all
copies.

Modifed work Copyright 2024 Crane Jin <crane@cranejin.com>
|#
(in-package #:mokubune/gemtext)		

(defclass element ()
  ((text :initform ""
	 :initarg :text
	 :accessor line-text
	 :type string)))

(defclass title (element)
  ((level :initarg :level
          :accessor title-level
          :type integer
          :documentation "The nesting level of the title.
Synonymous to the HTML heading levels, i.e. level 1 is <h1> tag, level 2 is <h2> tag etc.")))

(defclass link (element)
  ((url :initarg :url
        :accessor link-url
        :type string)))

(defclass item (element)
  ())

(defclass paragraph (element)
  ())

(defclass blockquote (element)
  ())

(defclass verbatim (element)
  ((alt :initform nil
        :initarg :alt
        :accessor verbatim-alt
        :type (or null string)
        :documentation "The alternative text for the verbatim block.
Is usually put at the same line as the opening backquotes.
Can be a programming language name or alternative text for, e.g., ASCII art.")))

(defun element-p (element) (typep element 'element))
(defun title-p (title) (typep title 'title))
(defun link-p (link) (typep link 'link))
(defun item-p (item) (typep item 'item))
(defun paragraph-p (paragraph) (typep paragraph 'paragraph))
(defun blockquote-p (blockquote) (typep blockquote 'blockquote))
(defun verbatim-p (verbatim) (typep verbatim 'verbatim))

(defun make-link (url &optional text)
  (make-instance 'link :url url
		       :text text))

(defun parse-link (s)
  "Parse a line into link."
  (let ((parts (cl-ppcre:split "\\s+" s :limit 2)))
    (apply #'make-link parts)))

(defun parse-line (s)
  (flet ((strim (s n)
           (string-trim '(#\Space #\Tab) (subseq s n)))
         (prefix-p (prfx str)
           (uiop:string-prefix-p prfx str)))
    (cond ((prefix-p "###" s) (make-instance 'title :level 3
                                                    :text (strim s 3)))
          ((prefix-p "##" s)  (make-instance 'title :level 2
                                                    :text (strim s 2)))
          ((prefix-p "#" s)   (make-instance 'title :level 1
                                                    :text (strim s 1)))
          ((prefix-p "=>" s)  (let ((s (strim s 2)))
                                (if (string-equal s "")
                                    (make-instance 'paragraph :text "=>")
                                    (parse-link s))))
          ((prefix-p "* " s)  (make-instance 'item :text (strim s 1)))
          ((prefix-p ">" s)   (make-instance 'blockquote :text (strim s 1)))
          (t (make-instance 'paragraph :text (strim s 0))))))

(defmacro markerp (line)
  `(uiop:string-prefix-p "```" ,line))

(defun parse (in)
  "Parse gemtext from the stream IN."
  (loop with doc = nil
        for line = (read-line in nil)
        unless line
          return (nreverse doc)
        do (push
            (if (markerp line)
                (loop with label = (subseq line 3)
                      with content = nil
                      for line = (read-line in nil)
                      when (or (not line)
                               (markerp line))
                        return (make-instance 'verbatim
                                              :alt (unless (string-equal label "")
                                                     label)
                                              :text (format nil "~{~A~%~^~}"
                                                            (nreverse content)))
                      do (push line content))
                (parse-line line))
            doc)))

(defun parse-string (str)
  "Parse the string STR as gemtext."
  (with-input-from-string (s str)
    (parse s)))

(defun emptyp (element)
  (or (null (line-text element))
      (string= "" (line-text element))))

(defun escape-html (str)
  (str:replace-all
   ">" "&gt;"
   (str:replace-all
    "<" "&lt;"
    (str:replace-all "&" "&amp;" str))))

(defvar *rewrite-file-types* (list ".gmi"))
(defun url-replace (url)
  (let ((type (and
               (uiop:string-prefix-p "/" url)
               (find-if #'(lambda (type)
                            (uiop:string-suffix-p url type))
                        *rewrite-file-types*))))
    (if type
        (str:concat (str:substring 0 (position #\. url :from-end t) url)
                    ".html")
        url)))

(defun non-item->html (ele)
  (cond ((paragraph-p ele)
	 (if (emptyp ele)
	     (write-line "<div class='empty-line'></div>")
	     (progn
	       (write-string "<p>")
	       (write-string (escape-html (line-text ele)))
	       (write-line "</p>"))))
	((link-p ele)
	 (let ((url (url-replace (link-url ele)))
	       (text (escape-html (line-text ele))))
	   (write-line (format nil "<p><a href='~a'>~a</a></p>"
			       url
			       (if (emptyp ele) url text)))))
	((title-p ele)
	 (write-line (format nil "<h~d>~a</h~d>"
			     (title-level ele)
			     (escape-html (line-text ele))
			     (title-level ele))))
        ((blockquote-p ele)
         (write-line (format nil "<blockquote><p>~a</p></blockquote>"
                             (escape-html (line-text ele)))))
	((verbatim-p ele)
	 (write-line (format nil "<pre>~a</pre>" (line-text ele))))))

(defun gemtext->html (doc)
  (let ((in-item nil))
    (dolist (ele doc)
      (cond ((item-p ele)
	     (unless in-item
	       (write-line "<ul>")
	       (setf in-item t))
	     (write-line (format nil "<li>~a</li>" (escape-html (line-text ele)))))
	    (t
	     (when in-item
	       (write-line "</ul>")
	       (setf in-item nil))
	     (non-item->html ele))))))
