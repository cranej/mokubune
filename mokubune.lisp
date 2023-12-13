(in-package #:mokubune)

(defvar *default-index-type* "gmi")
(defun default-index-type (type)
  (setf *default-index-type* type))

;;;; Rule matching for individual file
(defparameter *rules*
  (list
   (list "*.gmi" :apply-template)
   (list "*" :copy)))

(defparameter *allowed-actions* '(:apply-template :copy))

(defun add-rule (pattern action)
  (if (find action *allowed-actions*)
      (push (list pattern action) *rules*)
      (format t "Unknown action: ~s~%" action)))

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
		   (t :page))))
    (let ((source (rel-src source)))
      (ecase (what-path source)
	(:root-dir (list (abs-tpl *index-template-file*)))
	(:sub-dir (list (merge-pathnames *index-template-file* (abs-tpl source))
			(abs-tpl *sub-index-template-file*)
			(abs-tpl *index-template-file*)))
	(:page (list (merge-pathnames *page-template-file* (abs-tpl source))
		     (abs-tpl *page-template-file*)))))))

(defun determine-target (source)
  (let* ((source (abs-src source))
         (target (abs-target (rel-src source))))
    (if (directory-pathname-p target)
	(make-pathname :name "index" :type *default-index-type* :defaults target)
        target)))

;;;; Page context
(defclass page ()
  ((title :accessor page-title :initarg :title)
   (url :accessor page-url :initarg :url)
   (date :accessor page-date :initarg :date)
   (body :accessor page-body :initarg :body)))

(defmethod print-object ((object page) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (title url date body) object
      (format stream "Title: ~a, url: ~a, date: ~a, length of body: ~d"
	      title url date (length body)))))

(defparameter *date-unknown* "unknown")

(defgeneric read-body (type path)
  (:documentation "Parse body, title, and date from file pathname and file cointent."))

(defmethod read-body ((type t) path)
  (list nil (file-namestring path) (parse-date-from-filename path)))

(defun parse-date-from-filename (path)
  (first
   (all-matches-as-strings
    "\\d{4}-\\d{2}-\\d{2}$" (pathname-name path))))

(defmethod read-body ((type (eql 'file-type-gmi)) path)
  (let (title
	(date (parse-date-from-filename path)))
    (with-open-file (stream path :element-type 'character :direction :input)
      (list
       (with-output-to-string (body)
         (loop for line = (read-line stream nil nil)
               while line
               do (write-line line body)
               when (and (null title) (string/= line ""))
                 do
                    (when (str:starts-with? "#" line)
                      (setf title (str:trim-left line :char-bag '(#\# #\Space))))
	       when (and (null date) (string/= line ""))
		 do (register-groups-bind (date-from-body)
					  ("#+\\s*(\\d{4}-\\d{2}-\\d{2})" line)
					  (setf date date-from-body))))
       title
       date))))

(defmethod read-body ((type (eql 'file-type-org)) path)
  (let (title
	(date (parse-date-from-filename path)))
    (with-open-file (stream path :element-type 'character :direction :input)
      (list
       (with-output-to-string (body)
         (loop for line = (read-line stream nil nil)
               while line
               do (write-line line body)
               when (and (null title) (string/= line ""))
                 do (register-groups-bind (title-from-body)
			("^#\\+title:\\s*(.*)" line)
		      (setf title title-from-body))
	       when (and (null date) (string/= line ""))
		 do (register-groups-bind (date-from-body)
			("^#\\+date:\\s*(\\d{4}-\\d{2}-\\d{2})" line)
		      (setf date date-from-body))))
       (or title (file-namestring path))
       date))))

(defun parse-page (source-file target-file)
  (let* ((type (and source-file
		    (read-from-string
		     (format nil "file-type-~a" (pathname-type source-file)))))
         (body-title-date (and source-file (read-body type (abs-src source-file))))
	 (date (third body-title-date))
         (url (rel-target target-file)))
    (make-instance 'page
                   :title (or (second body-title-date) "")
                   :url url
                   :date  (or date *date-unknown*)
                   :body (or (first body-title-date) ""))))

;;;; Tree like structure of entities that needs to be processed
;;; op - (:action action :target target-file-path :source source-file-path :template template-file-path)
;;; context - page
;;; entity - (:name "last-path-seg" :op op :ctx context :files (file-entities) :dirs (dir-entities))
(defparameter *root-entity* nil)

(defun find-entity (path &optional (root *root-entity*))
  (if (string= (namestring path) "")
      (and (string= (getf root :name) "") root)

      (let ((directory-parts (rest (pathname-directory path)))
	    (file (if (directory-pathname-p path) nil (file-namestring path))))
	(loop with current = root
	      for current-dir-target = (first directory-parts)
	      while current
	      when (null current-dir-target)
		do (if file
		       (return (find-if #'(lambda (e) (string= file (getf e :name)))
					(getf current :files)))
		       (return current))
	      do
		 (setf current (find-if #'(lambda (e)
					    (string= current-dir-target (getf e :name)))
					(getf current :dirs)))
		 (pop directory-parts)))))

(defun make-op (action target &key source template)
  (list :action action :target target :source source :template template))

(defun make-file-entity (path op contex)
  (list :name (file-namestring path) :op op :ctx contex))

;;; All properties have to be set even the values are nil
;;; - need this to make sure we can updating the nested entity
(defun make-dir-entity (path &key op context files dirs)
  (let* ((path-dir (pathname-directory path))
	 (name (if (null path-dir) "" (car (last path-dir)))))
    (list :name name :op op :ctx context :files files :dirs dirs)))

(defun insert-entity (entity parent &key dir?)
  (push entity (getf parent (if dir? :dirs :files))))

;;;; Entry point
(defun run ()
  (setf *cwd* (uiop/os:getcwd))
  (run-with-args (uiop:command-line-arguments)))
 
(defun run-with-args (args)
  (cond ((string= (first args) "-init")
	 (do-init))
	((string= (first args) "-version")
	    (format t "~a~%" *version*))
	((> (length args) 0)
	 (format t "Usage: mokubune~%")
	 (format t "  Process current directory.~%")
	 (format t "~%")
	 (format t "Usage: mokubune <flag>~%")
	 (format t "  -init             Create default templates and directory layout in current directoy.~%")
	 (format t "  -version          Print version information and exit.~%"))
	(t (do-generating))))

(defparameter *runtime-config-file* nil)
(defun do-generating ()
  (let ((*runtime-config-file* (file-exists-p (abs-cwd "config.lisp"))))
    (when *runtime-config-file*
      (load *runtime-config-file*))
    (scan-entities)
    (sort-entities)
    (process-entities)))

(defparameter *dir-stack* nil)
(defun scan-entities ()
  "Walk content directory and build the root entity structure"
  (flet ((scan (source)
	   (if (directory-pathname-p source)
	       (build-dir-entity source)
	       (when (string/= (pathname-name source) "index")
		 (build-file-entity source)))))
    (setf *root-entity* nil *dir-stack* nil)
    (walk-directory (abs-src "") #'scan :directories t
					:on-leave-dir #'(lambda (dir)
							  (declare (ignore dir))
							  (pop *dir-stack*)))
    *root-entity*))

(defun build-dir-entity (source)
  (let* ((source (rel-src source))       
         (index-source (first (directory (merge-pathnames "index.*" (abs-src source)))))
         (tpl (find-template source :fallback index-source))
	 (parent (first *dir-stack*))
	 (dir-entity (create-dir-entity tpl index-source source)))
    (if parent
	(insert-entity dir-entity parent :dir? t)
	(setf *root-entity* dir-entity))
    (push dir-entity *dir-stack*)))

(defun create-dir-entity (tpl index-source source)
  (if tpl
      (let* ((target (rel-target (determine-target (or index-source source))))
	     (op (make-op :apply-template target 
			  :source (and index-source
				       (rel-src index-source))
			  :template (rel-tpl tpl)))
	     (context (parse-page index-source target)))
	(make-dir-entity source :op op :context context))
      (make-dir-entity source)))

(defun build-file-entity (source)
  (let* ((source (rel-src source))
	 (target (rel-target (determine-target source)))
	 (matched
           (find-if #'(lambda (r) (match-pattern (first r) source))
		    *rules*)))
    (when matched
      (let ((parent (first *dir-stack*)))
	(unless parent
	  (error "dir-stack is nil while processing file ~a~%" target))
	(case (second matched)
          (:copy (insert-entity
		  (make-file-entity target
				    (make-op :copy target :source source)
				    (parse-page source target))
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

(defun sort-entities (&optional (root *root-entity*))
  "Sort files by date desc of each dir entities, static files and files with date \"unknown\" have the least order"
  (setf (getf root :files)
	(sort (getf root :files)
	      #'(lambda (file1 file2)
		  (let ((ctx1 (getf file1 :ctx))
			(ctx2 (getf file2 :ctx)))
		    (cond ((null ctx1) nil)
			  ((null ctx2) t)
			  ((string= (page-date ctx1) *date-unknown*) nil)
			  ((string= (page-date ctx2) *date-unknown*) t)
			  (t 
			   (string> (page-date ctx1)
				    (page-date ctx2))))))))
  (dolist (dir (getf root :dirs))
    (sort-entities dir)))

(defun process-entities (&optional (root *root-entity*))
  "Process entities in a post-order traverse"
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
		 ((eq :deps-outdated (car form))
		  `(apply #'deps-newer-p
			  (abs-target ,target)
			  (with-config-file ,@(cdr form))))
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
	  :if-any (children-out-dated (:deps-outdated (list (abs-src source))))
	  :do (ensure-dir-and-copy-file source target)
	  :log ("Target: ~a:  copied from ~a~%" target source))))
      (:apply-template
       (let* ((source (getf op :source))
	      (target (getf op :target))
	      (tpl (getf op :template))
	      (deps (list (abs-tpl tpl))))
	 (when source (push (abs-src source) deps))
	 (process-target
	  target
	  :if-any (children-out-dated (:deps-outdated deps))
	  :do (apply-template target tpl :dir? dir?)
	  :log ("Target: ~a: applied template ~a to ~a~%" target tpl source)))))))

(defun with-config-file (deps)
  (if *runtime-config-file*
      (cons *runtime-config-file* deps)
      deps))

(defun ensure-dir-and-copy-file (source target)
  (let ((source-abs (abs-src source))
	(target-abs (abs-target target)))
    (ensure-directories-exist target-abs)
    (copy-file source-abs target-abs)))

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
		       :children (get-entity-file-contexts entity) 
		       :site *site*)))
       stream))))

(defun get-entity-file-contexts (entity)
  (mapcar #'(lambda (e) (getf e :ctx))
	  (getf entity :files)))

(defun get-file-contexts (entity-path &optional (root *root-entity*))
  (get-entity-file-contexts (find-entity entity-path root)))

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
