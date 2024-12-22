(in-package #:mokubune)

(defclass page ()
  ((title :accessor page-title :initarg :title)
   (path :accessor page-path :initarg :path)
   (date :accessor page-date :initarg :date)
   (body :accessor page-body :initarg :body)))

(defmethod print-object ((object page) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (title path date body) object
      (format stream "Title: ~a, path: ~a, date: ~a, length of body: ~d"
	      title path date (length body)))))

(define-constant +date-unknown+ "unknown")

(defgeneric parse-file (type pathname)
  (:documentation "Parse body, title, and date from file pathname and file content.")
  (:method (type pathname)
    (values nil
            (file-namestring pathname)
            (parse-date-from-filename pathname))))

(defun parse-date-from-filename (pathname)
  (first
   (all-matches-as-strings
    "\\d{4}-\\d{2}-\\d{2}$" (pathname-name pathname))))

(defmethod parse-file ((type (eql 'file-type-gmi)) pathname)
  (declare (type (or pathname string) pathname))
  (let (title
	(date (parse-date-from-filename pathname)))
    (with-open-file (stream pathname :element-type 'character :direction :input)
      (values
       (with-output-to-string (body)
         (loop for line = (read-line stream nil nil)
               while line
               do (write-line line body)
               when (and (null title) (string/= line ""))
                 do
                    (when (str:starts-with? "#" line)
                      (setf title
                            (str:trim-left line :char-bag '(#\# #\Space #\Tab))))
	       when (and (null date) (string/= line ""))
		 do (register-groups-bind (date-from-body)
			("#+\\s*(\\d{4}-\\d{2}-\\d{2})" line)
		      (setf date date-from-body))))
       title
       date))))

(define-constant +rst-title-tag+ ".. title:: ")
(defmethod parse-file ((type (eql 'file-type-rst)) pathname)
  (declare (type (or pathname string) pathname))
  (let (title
	(date (parse-date-from-filename pathname)))
    (with-open-file (stream pathname :element-type 'character :direction :input)
      (values
       (with-output-to-string (body)
         (loop for line = (read-line stream nil nil)
               while line
               do (write-line line body)
               when (and (null title) (string/= line ""))
                 do
                    (when (str:starts-with? +rst-title-tag+ line)
                      (setf title
                            (subseq line (length +rst-title-tag+))))
	       ))
       title
       date))))

(defun create-page (input output)
  (declare (type path input output))
  (let ((type (read-from-string
	       (format nil "file-type-~a"
		       (pathname-type (path-identity input))))))
    (multiple-value-bind (body title date)
        (parse-file type (path->pathname input))
      (make-instance 'page
                     :title (or title (file-namestring (path-identity input)))
                     :path (path-identity output)
                     :date  (or date +date-unknown+)
                     :body (or body "")))))
