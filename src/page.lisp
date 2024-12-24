(in-package #:mokubune)

(defclass page ()
  ((title :accessor page-title :initarg :title)
   (path :accessor page-path :initarg :path
         :documentation "relative path in output directory, can be used as url")
   (date :accessor page-date :initarg :date)
   (body :accessor page-body :initarg :body))
  (:documentation "Object created from each input file. It is available when template applying."))

(defmethod print-object ((object page) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (title path date body) object
      (format stream "Title: ~a, path: ~a, date: ~a, length of body: ~d"
	      title path date (length body)))))

(define-constant +date-unknown+ "unknown")

(defgeneric parse-file (type pathname)
  (:documentation "Parse body, title, and date from file pathname and file content.")
  (:method (type pathname)
    (let ((date (parse-date-from-filename pathname))
          (title (pathname-name pathname)))
      (values nil
             (if date
                 (subseq title 0 (- (length title) 11))
                 title)
             date))))

(defun parse-date-from-filename (pathname)
  (let ((match (first
                (all-matches-as-strings
                 "-\\d{4}-\\d{2}-\\d{2}$" (pathname-name pathname)))) )
    (and match (subseq match 1))))

(defmethod parse-file ((type (eql 'file-type-gmi)) pathname)
  (declare (type (or pathname string) pathname))
  (multiple-value-bind (_body ftitle fdate) (call-next-method)
    (declare (ignorable _body))
    (let (title
          (date fdate))
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
         (or title ftitle)
         date)))))

(define-constant +rst-title-tag+ ".. title:: ")
(defmethod parse-file ((type (eql 'file-type-rst)) pathname)
  (declare (type (or pathname string) pathname))
  (multiple-value-bind (_body ftitle fdate) (call-next-method)
    (declare (ignorable _body))
    (let (title
	  (date fdate))
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
         (or title ftitle)
         date)))))

(defun create-page (input output)
  (declare (type path input output))
  (let ((type (read-from-string
	       (format nil "file-type-~a"
		       (pathname-type (path-relative input))))))
    (multiple-value-bind (body title date)
        (parse-file type (path->pathname input))
      (make-instance 'page
                     :title (or title "unknown")
                     :path (path-relative output)
                     :date  (or date +date-unknown+)
                     :body (or body "")))))
