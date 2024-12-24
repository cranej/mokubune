(in-package #:mokubune)

(defmacro define-constant (name value &optional doc)
  "Make sure VALUE is evaluated only once \(to appease SBCL)."
  `(cl:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

;;; SBCL bug: set default-version to nil when calling merge-pathnames has no effect
(defmacro merge-pathnames* (pathname default)
  `(make-pathname :version nil :defaults (merge-pathnames ,pathname ,default)))

(defvar *cwd* (uiop/os:getcwd))
(defun set-working-directory (wd)
  (setf *cwd* wd))

(define-constant +page-template-file+ "page.clt")
(define-constant +index-template-file+ "index.clt")
(define-constant +sub-index-template-file+ "sub-index.clt")
(define-constant +html-template-file+ "page.html")

(defparameter *default-index-template-content* nil)
(defparameter *default-page-template-content* nil)
(defparameter *default-html-template-content* nil)
(defparameter *default-config-content* nil)

(defstruct site
  (title "My brilliant writes" :type string)
  (content-dir "contents/" :type string)
  (template-dir "templates/" :type string)
  (base-url "" :type string)
  (data (make-hash-table :test 'equal)))

(defvar *site* (make-site))

(defmacro config (slot-fn value)
  (let ((value-form (if (or (eq slot-fn 'site-content-dir)
                            (eq slot-fn 'site-template-dir))
                        `(namestring (cl-fad:pathname-as-directory
                                      (enough-namestring ,value *cwd*)))
                        value)))
   `(setf (,slot-fn *site*) ,value-form)))

(defun get-site-data (key)
  (and (site-data *site*)
       (gethash key (site-data *site*))))

(defun set-site-data (key value)
  (unless (site-data *site*)
    (setf (site-data *site*) (make-hash-table :test 'equal)))
  (setf (gethash key (site-data *site*)) value))

(defvar *verbose* :error)
(defun be-verbose (&optional (level :info)) (setf *verbose* level))
(defun level-to-n (level)
  (case level
    (:debug 1000)
    (:info 100)
    (:warn 10)
    (:error 1)
    (otherwise 1000)))

(defun log-message (level control-string &rest format-arguments)
  (when (>= (level-to-n *verbose*) (level-to-n level))
    (handler-case
        (format t "~A: ~?~%" level control-string format-arguments)
      (error (e)
        (ignore-errors
         (format *trace-output* "error ~A while writting log~%" e)))))
  (values))

(defparameter *tpl-to-init*
  (list (cons +index-template-file+ *default-index-template-content*)
	(cons +page-template-file+ *default-page-template-content*)
	(cons +html-template-file+ *default-html-template-content*)))

(defun do-init ()
  (dolist (dir (list (site-content-dir *site*)
		     (site-template-dir *site*)))
    (uiop/run-program:run-program
     (list "mkdir" "-p" dir) :force-shell t))
  (dolist (tpl *tpl-to-init*)
    (write-file-string (merge-pathnames (car tpl) (site-template-dir *site*))
		       (cdr tpl)))
  (write-file-string "config.lisp" *default-config-content*)
  (format t "Initlized.~%"))

(defun write-file-string (file string)
  (with-open-file (stream file
			  :direction :output
			  :if-exists :error
			  :if-does-not-exist :create)
    (write-sequence string stream)))

;;; Intended to be called in Makefile. Read resource files
;;;  into variables, so their contents are included in the final binary (saved image).
(defun load-resource-files (dir)
  (dolist (tpl *tpl-to-init*)
    (setf (cdr tpl)
          (read-file-string
           (merge-pathnames (car tpl)
                            (merge-pathnames "default-templates/" dir)))))
  (setf *default-config-content*
        (read-file-string (merge-pathnames "examples/config.lisp" dir))))

(defun read-file-string (file)
  (with-open-file (stream file)
    (let ((string (make-string (file-length stream))))
      (read-sequence string stream)
      string)))
