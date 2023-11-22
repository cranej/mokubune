(in-package #:mokubune)

;;;; Define global environments
(defparameter *cwd* (uiop/os:getcwd))

(defun absolute-path (path)
  (merge-pathnames path *cwd*))

(defvar *site-title* "A personal blog")
(defvar *template-directory* (absolute-path #p"templates/"))
(defvar *content-directory* (absolute-path #p"contents/"))
(defvar *target-directory* (absolute-path #p"public/"))
(defparameter *page-template-file* "page.clt")
(defparameter *index-template-file* "index.clt")

(defun set-directories (cwd)
  (setf *cwd* cwd)
  (setf *template-directory* (absolute-path #p"templates/"))
  (setf *content-directory* (absolute-path #p"contents/"))
  (setf *target-directory* (absolute-path #p"public/")))

;;;; Rule matching for individual file
(defparameter *rules*
  (list
   (list "*.gmi" :apply-template)
   (list "*" :copy)))

(defun match-rule (path)
  (loop for (pattern fn) in *rules*
        when (match-pattern pattern path)
        return fn))

(defun match-pattern (pattern path)
  (loop for pattern-segment in (split-pattern pattern)
        for pos = 0
        for match = (match-pattern-segment pattern-segment path pos)
        while (< pos (length path))
        unless match
	  return nil
        do (setf pos match)
        finally (return (= pos (length path)))))

(defun match-pattern-segment (pattern-segment path start)
  (cond ((zerop (length pattern-segment))
         (if (>= start (length path))
             (length path)))
        ((string= "*" pattern-segment)
         (length path))
        ((char= #\* (char pattern-segment 0))
         (let ((pos (search pattern-segment path :start1 1 :start2 start)))
           (when pos
             (+ start pos
                (1- (length pattern-segment))))))
        ((str:starts-with? pattern-segment path)
         (+ start (length pattern-segment)))))

(defun split-pattern (pattern)
  (do ((len (length pattern))
       (start 0)
       (pattern-segments nil))
      ((>= start len) (nreverse pattern-segments))
      (multiple-value-bind (pattern-segment next)
	  (read-pattern-segment pattern start)
        (push pattern-segment pattern-segments)
        (setf start next))))

(defun read-pattern-segment (pattern start)
  (let ((pattern-segment
	  (make-array 5 :element-type 'character :adjustable t :fill-pointer 0)))
    (vector-push-extend (char pattern start) pattern-segment)
    (loop for i upfrom (1+ start) below (length pattern)
          while (char/= #\* (char pattern i))
          do (vector-push-extend (char pattern i) pattern-segment)
          finally (return (values pattern-segment i)))))

;;; Functions for locating template, target, etc.
(defun find-template (source &key (fallback t))
  (multiple-value-bind (specific default) (template-file-locations source)
    (or (file-exists-p specific)
        (and fallback (file-exists-p default)))))

(defun template-file-locations (source)
  (let* ((source (abs-src source))
         (filename (file-namestring source))
         (template-file-name
           (if (or (string-equal filename "")
                   (string-equal filename "index.gmi")) ;; TODO: configurable
               *index-template-file*
               *page-template-file*)))
    (values
     (merge-pathnames template-file-name (abs-tpl (rel-src source)))
     (abs-tpl template-file-name))))

(defun find-target (source)
  (let* ((source (abs-src source))
        (target (abs-target (rel-src source))))
    (if (directory-pathname-p target)
        (merge-pathnames "index.gmi" target)
        target)))

(defun set-extension (target extension)
  (make-pathname :type extension :defaults target))

;;;; Page context
(defclass page ()
  ((title :accessor page-title :initarg :title)
   (url :accessor page-url :initarg :url)
   (date :accessor page-date :initarg :date)
   (body :accessor page-body :initarg :body)))

(defun read-body-and-title (file)
  (let (title)
    (with-open-file (stream file :element-type 'character :direction :input)
      (list
        (with-output-to-string (body)
          (loop with read-title = t
                for line = (read-line stream nil nil)
                while line
                do (write-line line body)
                when (and read-title (string/= line ""))
                do
                   (setf read-title nil)
                   (when (str:starts-with? "#" line)
                     (setf title (str:trim-left line :char-bag "# ")))))
        title))))

(defun parse-page (source-file target-file)
  (let* ((date
           (and source-file
                (first
                  (all-matches-as-strings
                    "\\d{4}-\\d{2}-\\d{2}$" (pathname-name source-file)))))
         (body-title (and source-file (read-body-and-title (abs-src source-file))))
         (url (rel-target target-file)))
    (make-instance 'page
                   :title (second body-title)
                   :url url
                   :date date
                   :body (first body-title))))

;;;; Tree like structure of entities that needs to process
;;; op - (:action action :target target-file-path :source source-file-path :template template-file-path)
;;; context - page
;;; entity - (:name "last-path-seg" :op op :ctx context :files (file-entities) :dirs (dir-entities))
(defparameter *root-entity* nil)

(defun reset-root ()
  (setf *root-entity* (list :name "" :op nil :ctx nil :files nil :dirs nil)))

(defun find-entity (path &optional (root *root-entity*))
  (if (string= path "")
      (and (string= (getf root :name) "") root)
      
      (let ((parent (find-parent path root))
	    (name (if (directory-pathname-p path)
		      (car (last (pathname-directory path)))
		      (file-namestring path)))
	    (pkey (if (directory-pathname-p path) :dirs :files)))
	(when parent
	  (find-if #'(lambda (entity) (string= (getf entity :name) name))
		   (getf parent pkey))))))

(defun make-op (action target &key source template)
  (list :action action :target target :source source :template template))

(defun make-file-entity (path op contex)
  (list :name (file-namestring path) :op op :ctx contex))

;;; All properties have to be set even the values are nil - need this to make sure we can updating the nested entity returned by find-parent
(defun make-dir-entity (path &key op context files dirs)
  (let* ((path-dir (pathname-directory path))
	 (name (if (null path-dir) "" (car (last path-dir)))))
    (list :name name :op op :ctx context :files files :dirs dirs)))

(defun find-parent (path root-entity)
  (let ((dirs (cdr (pathname-directory path))))
    (when (directory-pathname-p path)
      (setf dirs (butlast dirs)))
    (cond ((null dirs) root-entity)
	  (t (loop with dir-entities = (getf root-entity :dirs)
		   for (d next) on dirs
		   for entity = (find-if #'(lambda (e)
					     (string= d (getf e :name)))
					 dir-entities)
		   while entity
		   when entity
		     do (setf dir-entities (getf entity :dirs))
		   finally (return entity))))))

(defun insert-entity (entity parent &key dir?)
  (push entity (getf parent (if dir? :dirs :files))))

;;;; walk content directory and build the root entity structure
(defun build-entity ()
  (reset-root)
  (walk-directory *content-directory* #'build-ops :directories t)
  *root-entity*)

(defun build-ops (source)
  (if (directory-p source)
      (build-dir source)
      (when (string/= (pathname-name source) "index")
	(build-file source))))

(defun build-dir (source)
  (let* ((source (rel-src source))
	 (is-root (string= source ""))
         (index-source (file-exists-p (abs-src (merge-pathnames "index.gmi" source))))
         (tpl (find-template source :fallback (or is-root index-source)))
	 (parent (find-parent source *root-entity*)))
    (unless parent
      (error "Unable to find parent for ~a" source))
    (if tpl
        (let* ((target (rel-target (find-target source)))
	       (op (make-op :apply-template target 
			    :source (and index-source
					 (rel-src index-source))
			    :template (rel-tpl tpl)))
	       (context (parse-page index-source target)))
	  (if is-root
	      (progn
		(setf (getf parent :op) op)
		(setf (getf parent :ctx) context))
	      
	      (insert-entity (make-dir-entity source :op op :context context)
			     parent :dir? t)))
	
	(unless is-root
	  (insert-entity (make-dir-entity source) parent :dir? t)))))

(defun build-file (source)
  (let* ((source (rel-src source))
	 (target (rel-target (find-target source)))
	 (matched
           (find-if #'(lambda (r) (match-pattern (first r) source))
		    *rules*)))
    (when matched
      (let ((parent (find-parent target *root-entity*)))
	(unless parent
	  (error "Unable to find parent of ~a~%" target))
	(case (second matched)
          (:copy (insert-entity
		  (make-file-entity target
				    (make-op :copy target :source source)
				    nil)
		  parent))
          (:apply-template
           (let ((tpl (rel-tpl (find-template source))))
             (when tpl
	       (insert-entity
		(make-file-entity target
				  (make-op :apply-template target
					   :source source
					   :template tpl)
				  (parse-page source target))
		parent)))))))))

;;;; Utilities to deal with path
(defun rel-src (path)
  (enough-namestring path *content-directory*))
(defun abs-src (path)
  (merge-pathnames path *content-directory*))

(defun rel-tpl (path)
  (enough-namestring path *template-directory*))
(defun abs-tpl (path)
  (merge-pathnames path *template-directory*))

(defun rel-target (path)
  (enough-namestring path *target-directory*))
(defun abs-target (path)
  (merge-pathnames path *target-directory*))
