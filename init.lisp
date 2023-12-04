(in-package #:mokubune)

(defparameter *default-index-template-content* nil)
(defparameter *default-page-template-content* nil)

(defun do-init ()
  (uiop/run-program:run-program
   (list "mkdir" (site-content-dir *site*)) :force-shell t)
  (uiop/run-program:run-program
   (list "mkdir" (site-template-dir *site*)) :force-shell t)
  (uiop/run-program:run-program
   (list "mkdir" (site-output-dir *site*)) :force-shell t)
  (write-file-string (merge-pathnames "index.clt" (site-template-dir *site*))
		     *default-index-template-content*)
  (write-file-string (merge-pathnames "page.clt" (site-template-dir *site*))
		     *default-page-template-content*)
  (format t "Initlized.~%"))

(defun write-file-string (file string)
  (with-open-file (stream file
			  :direction :output
			  :if-exists :error
			  :if-does-not-exist :create)
    (write-sequence string stream)))

;;; intent to be called in Makefile
(defun load-default-templates-content (dir)
  (let ((index-tpl-path (merge-pathnames "index.clt" dir))
	(page-tpl-path (merge-pathnames "page.clt" dir)))
    (setf *default-index-template-content* (read-file-string index-tpl-path))
    (setf *default-page-template-content* (read-file-string page-tpl-path))))

(defun read-file-string (file)
  (with-open-file (stream file)
    (let ((string (make-string (file-length stream))))
      (read-sequence string stream)
      string)))
