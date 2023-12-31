(in-package #:mokubune)

(defvar *cwd* (uiop/os:getcwd))
(defun set-working-directory (wd)
  (setf *cwd* wd))

(defun abs-cwd (path)
  (merge-pathnames path *cwd*))

(defparameter *page-template-file* "page.clt")
(defparameter *index-template-file* "index.clt")
(defparameter *sub-index-template-file* "sub-index.clt")

(defparameter *default-index-template-content* nil)
(defparameter *default-page-template-content* nil)

(defstruct site
  (title "My brilliant writes" :type string)
  (content-dir "contents/" :type string)
  (template-dir "templates/" :type string)
  (output-dir "public/" :type string)
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

(defun do-init ()
  (uiop/run-program:run-program
   (list "mkdir" (site-content-dir *site*)) :force-shell t)
  (uiop/run-program:run-program
   (list "mkdir" (site-template-dir *site*)) :force-shell t)
  (uiop/run-program:run-program
   (list "mkdir" (site-output-dir *site*)) :force-shell t)
  (write-file-string (merge-pathnames *index-template-file* (site-template-dir *site*))
		     *default-index-template-content*)
  (write-file-string (merge-pathnames *page-template-file* (site-template-dir *site*))
		     *default-page-template-content*)
  (format t "Initlized.~%"))

(defun write-file-string (file string)
  (with-open-file (stream file
			  :direction :output
			  :if-exists :error
			  :if-does-not-exist :create)
    (write-sequence string stream)))

;;; intended to be called in Makefile
(defun load-default-templates-content (dir)
  (let ((index-tpl-path (merge-pathnames *index-template-file* dir))
	(page-tpl-path (merge-pathnames *page-template-file* dir)))
    (setf *default-index-template-content* (read-file-string index-tpl-path))
    (setf *default-page-template-content* (read-file-string page-tpl-path))))

(defun read-file-string (file)
  (with-open-file (stream file)
    (let ((string (make-string (file-length stream))))
      (read-sequence string stream)
      string)))

;;;; Utilities to deal with path
(defun rel-src (path)
  (enough-namestring path (abs-cwd (site-content-dir *site*))))
(defun abs-src (path)
  (merge-pathnames path (abs-cwd (site-content-dir *site*))))

(defun rel-tpl (path)
  (enough-namestring path (abs-cwd (site-template-dir *site*))))
(defun abs-tpl (path)
  (merge-pathnames path (abs-cwd (site-template-dir *site*))))

(defun rel-target (path)
  (enough-namestring path (abs-cwd (site-output-dir *site*))))
(defun abs-target (path)
  (merge-pathnames path (abs-cwd (site-output-dir *site*))))
