(in-package #:mokubune)

(defvar *cwd* (uiop/os:getcwd))
(defun set-working-directory (wd)
  (setf *cwd* wd))

(defun abs-cwd (pathname)
  (merge-pathnames pathname *cwd*))

(defparameter *page-template-file* "page.clt")
(defparameter *index-template-file* "index.clt")
(defparameter *sub-index-template-file* "sub-index.clt")
(defparameter *html-template-file* "page.html")

(defparameter *default-index-template-content* nil)
(defparameter *default-page-template-content* nil)
(defparameter *default-html-template-content* nil)

(defstruct site
  (title "My brilliant writes" :type string)
  (content-dir "contents/" :type string)
  (template-dir "templates/" :type string)
  (output-dir "public/gemini/" :type string)
  (html-output-dir nil)
  (base-url "" :type string)
  (data (make-hash-table :test 'equal)))

(defvar *site* (make-site))

(defmacro config (slot-fn value)
  `(setf (,slot-fn *site*) ,value))

(defun get-site-data (key)
  (and (site-data *site*)
       (gethash key (site-data *site*))))

(defun set-site-data (key value)
  (unless (site-data *site*)
    (setf (site-data *site*) (make-hash-table :test 'equal)))
  (setf (gethash key (site-data *site*)) value))

(defvar *verbose* nil)
(defun be-verbose () (setf *verbose* t))

(defparameter *tpl-to-init*
  (list (cons *index-template-file* *default-index-template-content*)
	(cons *page-template-file* *default-page-template-content*)
	(cons *html-template-file* *default-html-template-content*)))

(defun do-init ()
  (dolist (dir (list (site-content-dir *site*)
		     (site-template-dir *site*)
		     (site-output-dir *site*)))
    (uiop/run-program:run-program
     (list "mkdir" "-p" dir) :force-shell t))
  (dolist (tpl *tpl-to-init*)
    (write-file-string (merge-pathnames (car tpl) (site-template-dir *site*))
		       (cdr tpl)))
  (format t "Initlized.~%"))

(defun write-file-string (file string)
  (with-open-file (stream file
			  :direction :output
			  :if-exists :error
			  :if-does-not-exist :create)
    (write-sequence string stream)))

;;; intended to be called in Makefile
(defun load-default-templates-content (dir)
  (dolist (tpl *tpl-to-init*)
    (setf (cdr tpl) (read-file-string (merge-pathnames (car tpl) dir)))))

(defun read-file-string (file)
  (with-open-file (stream file)
    (let ((string (make-string (file-length stream))))
      (read-sequence string stream)
      string)))

(deftype dirs () '(member content template output html-output))
(defstruct path
  (identity nil :type string)
  (dir 'content :type dirs))

(defmethod print-object ((object path) stream) 
  (format stream "~s/~a" (path-dir object) (path-identity object)))

(defun dir->pathname (dir)
  (ecase dir
    (content (site-content-dir *site*))
    (template (site-template-dir *site*))
    (output (site-output-dir *site*))
    (html-output (site-html-output-dir *site*))))

(defun abspath (path)
  (let ((dir-path (dir->pathname (path-dir path))))
    (merge-pathnames (path-identity path) (abs-cwd dir-path))))

(defun mapdir (path dir)
  (declare (type dirs dir)
	   (type path path))
  (let ((new-path (copy-structure path)))
    (setf (path-dir new-path) dir)
    new-path))

(defun mapidentity (path mapfn)
  (declare (type path path)
	   (ftype (function (string) string) mapfn))
  (let ((new-path (copy-structure path)))
    (setf (path-identity new-path)
	  (funcall mapfn (path-identity path)))
    new-path))

(defun change-ext (path ext)
  (declare (type path path)
	   (type string ext))
  (mapidentity path
	       #'(lambda (identity)
		   (namestring (make-pathname :type ext :defaults identity)))))

(defun pathname->dir (p)
  (let ((short-path (enough-namestring p *cwd*)))
    (cond ((str:starts-with? (site-content-dir *site*) short-path)
	   'content)
	  ((str:starts-with? (site-template-dir *site*) short-path)
	   'template)
	  ((str:starts-with? (site-output-dir *site*) short-path)
	   'output)
	  ((str:starts-with? (site-html-output-dir *site*) short-path)
	   'html-output)
	  (t (error "unknown path dir of ~a" p)))))

(defun pathname->path (p)
  (when p
    (let* ((dir (pathname->dir p))
	   (dir-path (dir->pathname dir)))
      (make-path :identity (enough-namestring p (abs-cwd dir-path))
		 :dir dir))))
