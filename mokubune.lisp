(in-package #:mokubune)

;;;; Define global environments
(defparameter *cwd* (uiop/os:getcwd))
(defun set-working-directory (wd)
  (setf *cwd* wd))

(defun abs-cwd (path)
  (merge-pathnames path *cwd*))

(defparameter *page-template-file* "page.clt")
(defparameter *index-template-file* "index.clt")
(defparameter *sub-index-template-file* "sub-index.clt")

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
  (let ((locations (template-file-locations source)))
    (if fallback
	(find-if #'file-exists-p locations)
	(file-exists-p (first locations)))))

(defun template-file-locations (source)
  (labels ((what-path (source)
	     (cond ((string= source "") :root-dir)
		   ((directory-pathname-p source) :sub-dir)
		   (t :page))	     ))
    (let ((source (rel-src source)))
      (ecase (what-path source)
	(:root-dir (list (abs-tpl *index-template-file*)))
	(:sub-dir (list (merge-pathnames *index-template-file* (abs-tpl source))
			(abs-tpl *sub-index-template-file*)
			(abs-tpl *index-template-file*)))
	(:page (list (merge-pathnames *page-template-file* (abs-tpl source))
		     (abs-tpl *page-template-file*)))))))

(defun find-target (source)
  (let* ((source (abs-src source))
         (target (abs-target (rel-src source))))
    (if (directory-pathname-p target)
        (merge-pathnames "index.gmi" target) ;; TODO: configurable
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
                     (setf title (str:trim-left line :char-bag '(#\# #\Space))))))
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
                   :title (or (second body-title) "")
                   :url url
                   :date  (or date "unknown")
                   :body (or (first body-title) ""))))

;;;; Tree like structure of entities that needs to process
;;; op - (:action action :target target-file-path :source source-file-path :template template-file-path)
;;; context - page
;;; entity - (:name "last-path-seg" :op op :ctx context :files (file-entities) :dirs (dir-entities))
(defparameter *root-entity* nil)

(defun reset-root ()
  (setf *root-entity* (list :name "" :op nil :ctx nil :files nil :dirs nil)))

(defun find-entity (path &optional (root *root-entity*))
  (if (string= (namestring path) "")
      (and (string= (getf root :name) "") root)
      
      (let ((parent (find-parent-entity path root))
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

;;; All properties have to be set even the values are nil - need this to make sure we can updating the nested entity returned by find-parent-entity
(defun make-dir-entity (path &key op context files dirs)
  (let* ((path-dir (pathname-directory path))
	 (name (if (null path-dir) "" (car (last path-dir)))))
    (list :name name :op op :ctx context :files files :dirs dirs)))

(defun find-parent-entity (path root-entity)
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

;;;; Walk content directory and build the root entity structure.
;;;; Scanning and processing could be done in one-pass, with a modified
;;;; walk-directory logic instead of pathnames system's, but performance
;;;; is not a concern here, so don't brother it.
(defun scan-entities ()
  (flet ((scan (source)
	   (if (directory-p source)
	       (build-dir-entity source)
	       (when (string/= (pathname-name source) "index")
		 (build-file-entity source)))))
    (reset-root)
    (walk-directory (abs-src "") #'scan :directories t)
    *root-entity*))

(defun build-dir-entity (source)
  (let* ((source (rel-src source))
	 (is-root (string= source ""))
	 ;; TODO: index file extension should be configurable
         (index-source (file-exists-p (abs-src (merge-pathnames "index.gmi" source))))
         (tpl (find-template source :fallback index-source))
	 (parent (find-parent-entity source *root-entity*)))
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

(defun build-file-entity (source)
  (let* ((source (rel-src source))
	 (target (rel-target (find-target source)))
	 (matched
           (find-if #'(lambda (r) (match-pattern (first r) source))
		    *rules*)))
    (when matched
      (let ((parent (find-parent-entity target *root-entity*)))
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

;;;; process entity - a post-order traverse
(defun process-entities (&optional (root *root-entity*))
  (let ((children-out-dated nil))
    (dolist (file (getf root :files))
      (when (process-op (getf file :op))
	(setf children-out-dated t)))
    (dolist (dir (getf root :dirs))
      (when (process-entities dir)
	(setf children-out-dated t)))
    (when (process-op (getf root :op) :dir? t :children-out-dated children-out-dated)
      (setf children-out-dated t))
    children-out-dated))


(defmacro process-target (target &key if-any do log)
  (flet ((if-form (form)
	   (cond ((atom form) form)
		 ((eq :deps (car form))
		  `(apply #'deps-newer-p (abs-target ,target) ,@(cdr form)))
		 (t `(,@form)))))
    (destructuring-bind (act-fn &rest args) do
      `(if (or ,@(mapcar #'if-form if-any))
	   (progn
	     (,act-fn ,@args)
	     ,@(if log
		   (list `(when *verbose* (format t ,@log))))
	     t)
	   (progn
	     (when *verbose* (format t "Skip ~a~%" ,target))
	     nil)))))

(defun process-op (op &key dir? children-out-dated)
  (when op
    (ecase (getf op :action)
      (:copy
       (let ((source (getf op :source))
	     (target (getf op :target)))
	 (process-target
	  target
	  :if-any (children-out-dated (:deps (abs-src source)))
	  :do (copy-file (abs-src source) (abs-target target))
	  :log ("Target: ~a:  copied from ~a~%" target source))))
      (:apply-template
       (let* ((source (getf op :source))
	      (target (getf op :target))
	      (tpl (getf op :template))
	      (deps (list (abs-tpl tpl))))
	 (when source (push (abs-src source) deps))
	 (process-target
	  target
	  :if-any (children-out-dated (:deps deps))
	  :do (apply-template target tpl :dir? dir?)
	  :log ("Target: ~a: applied template ~a to ~a~%" target tpl source)))))))

(defun apply-template (target template &key dir?)
  (let* ((target-rel (rel-target target))
	 (target-abs (abs-target target))
	 (entity (find-entity (if dir? (directory-namestring target-rel) target-rel))))
    (ensure-directories-exist target-abs)
    (with-open-file (stream target-abs
			    :element-type 'character
			    :direction :output
			    :if-exists :supersede)
      (write-string
       ;; It's wired that if template contains emoji, there will be an extra #\Nul
       ;; appended to output string. Which cause emacs cannot display the text properly
       (string-trim
	'(#\Nul)
	(funcall (compile-template template)
		 (list :page (getf entity :ctx)
		       :page-parent (parent-url target-rel :dir? dir?)
		       :children (mapcar #'(lambda (e) (getf e :ctx))
					 (getf entity :files))
		       :site *site*)))
       stream))))

(defun parent-url (target &key dir?)
  (if dir?
      (format nil "/~@[~{~a/~}~]" (butlast (cdr (pathname-directory target))))
      (format nil "/~a" (directory-namestring target))))

(defun deps-newer-p (target &rest deps)
  (if (file-exists-p target)
      (let ((target-write-date (file-write-date target)))
	(some #'(lambda (file) (>= (file-write-date file) target-write-date))
	      deps))
      t))

(defparameter *compiled-templates* (make-hash-table :test #'equal))
(defun compile-template (tpl)
  (let ((key (format nil "~a-~a" (rel-tpl tpl)
		     (or (file-write-date (abs-tpl tpl)) ""))))
    (or (gethash key *compiled-templates*)
	(setf (gethash key *compiled-templates*)
	      (with-open-file (stream (abs-tpl tpl))
		(let ((template (make-string (file-length stream))))
		  (read-sequence template stream)
		  (cl-template:compile-template template)))))))

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

