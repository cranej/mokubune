(in-package #:mokubune)

(defvar *default-index-type* "gmi")

;;; Functions for locating template, target, etc.
(defun find-tpl (source &key (fallback t))
  "Find applicable template file for source."
  (let ((locations (find-tpl-locations source)))
    (if fallback
	(find-if #'(lambda (p) (file-exists-p (abspath p))) locations)
	(when (file-exists-p (abspath (first locations)))
	  (first locations)))))

(defun find-tpl-locations (source)
  (let ((what-path (cond
		     ((string= (path-identity source) "") :root-dir)
		     ((directory-pathname-p (path-identity source)) :sub-dir)
		     (t :page))))
    (ecase what-path
      (:root-dir (list (make-path :identity *index-template-file*
				  :dir 'template)))
      (:sub-dir (list (mapidentity (mapdir source 'template)
				   #'(lambda (folder)
				       (namestring
					(merge-pathnames *index-template-file*
							 folder))))		    
		      (make-path :identity *sub-index-template-file*
				 :dir 'template)
		      (make-path :identity *index-template-file*
				 :dir 'template)))
      (:page (list (mapidentity (mapdir source 'template)
				#'(lambda (pagepath)
				    (namestring
				     (merge-pathnames *page-template-file*
						      pagepath))))
		   (make-path :identity *page-template-file*
			      :dir 'template))))))

(defun determine-output (source)
  (let* ((output (mapdir source 'output)))
    (if (directory-pathname-p (abspath output))	
	(pathname->path (make-pathname :name "index"
				       :type *default-index-type*
				       :defaults (abspath output)))
        output)))

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

(defgeneric read-body (type pathname)
  (:documentation "Parse body, title, and date from file pathname and file cointent."))

(defmethod read-body ((type t) pathname)
  (list nil (file-namestring pathname) (parse-date-from-filename pathname)))

(defun parse-date-from-filename (pathname)
  (first
   (all-matches-as-strings
    "\\d{4}-\\d{2}-\\d{2}$" (pathname-name pathname))))

(defmethod read-body ((type (eql 'file-type-gmi)) pathname)
  (declare (type (or pathname string) pathname))
  (let (title
	(date (parse-date-from-filename pathname)))
    (with-open-file (stream pathname :element-type 'character :direction :input)
      (list
       (with-output-to-string (body)
         (loop for line = (read-line stream nil nil)
               while line
               do (write-line line body)
               when (and (null title) (string/= line ""))
                 do
                    (when (str:starts-with? "#" line)
                      (setf title (str:trim-left line :char-bag '(#\# #\Space #\Tab))))
	       when (and (null date) (string/= line ""))
		 do (register-groups-bind (date-from-body)
			("#+\\s*(\\d{4}-\\d{2}-\\d{2})" line)
		      (setf date date-from-body))))
       title
       date))))

(defun create-context (source output)
  (declare (type (or null path) source output))
  (let* ((type (and source
		    (read-from-string
		     (format nil "file-type-~a"
			     (pathname-type (path-identity source))))))
         (body-title-date (and source (read-body type (abspath source))))
	 (date (third body-title-date))
         (url (path-identity output)))
    (make-instance 'page
                   :title (or (second body-title-date) "")
                   :url url
                   :date  (or date *date-unknown*)
                   :body (or (first body-title-date) ""))))

;;;; Tree like structure of entities that needs to be processed
;;; op - (:action action :output target-file-path :source source-file-path :template template-file-path)
;;; context - page
;;; entity - (:name "last-path-seg" :op op :ctx context :files (file-entities) :dirs (dir-entities))
(defparameter *root-entity* nil)

(defun find-entity (path &optional (root *root-entity*))
  (declare (type (or string pathname) path))
  (if (string= (namestring path) "")
      (and (string= (getf root :name) "") root)
      (let ((directory-parts (rest (pathname-directory path)))
	    (file (if (directory-pathname-p path) nil (file-namestring path))))
	(loop with current = root
	      for current-dir-target = (first directory-parts)
	      while current
	      when (null current-dir-target)
		do (if file
		       (return (find-if #'(lambda (e)
					    (string= file (getf e :name)))
					(getf current :files)))
		       (return current))
	      do
		 (setf current (find-if #'(lambda (e)
					    (string= current-dir-target
						     (getf e :name)))
					(getf current :dirs)))
		 (pop directory-parts)))))

(defun make-op (action output &key source template)
  (declare (type (or null path) source output template))
  (list :action action :output output :source source :template template))

(defun make-file-entity (path op contex)
  (declare (type (or string pathname) path))
  (list :name (file-namestring path) :op op :ctx contex))

;;; All properties have to be set even the values are nil
;;; - need this to make sure we can updating the nested entity
(defun make-dir-entity (path &key op context files dirs)
  (declare (type (or string pathname) path))
  (let* ((path-dir (pathname-directory path))
	 (name (if (null path-dir) "" (car (last path-dir)))))
    (list :name name :op op :ctx context :files files :dirs dirs)))

(defun insert-entity (entity parent &key dir?)
  (push entity (getf parent (if dir? :dirs :files))))

;;;; Entry point
(defun run ()
  (sb-ext:disable-debugger)
  (setf *cwd* (uiop/os:getcwd))
  (run-with-args (uiop:command-line-arguments)))
 
(defun run-with-args (args)
  (cond ((string= (first args) "-init")
	 ;; TODO: should load configuration file before do-init
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
  (let ((content-absolute-path (abs-cwd (site-content-dir *site*))))
   (labels ((convert-path (source)
	      (make-path :identity (enough-namestring source content-absolute-path)
			 :dir 'content))
	    (process-entity (source)
	      (if (directory-pathname-p source)
		  (build-dir-entity (convert-path source))
		  (when (string/= (pathname-name source) "index")
		    (build-file-entity (convert-path source))))))
     (setf *root-entity* nil *dir-stack* nil)
     (walk-directory content-absolute-path #'process-entity
		     :directories t
		     :on-leave-dir #'(lambda (dir)
				       (declare (ignore dir))
				       (pop *dir-stack*)))
     *root-entity*)))

(defun build-dir-entity (path)
  (declare (type path path))
  (let* ((index-file
	   (pathname->path
	    (first (directory (merge-pathnames "index.*" (abspath path))))))
         (tpl (find-tpl path :fallback index-file))
	 (parent (first *dir-stack*))
	 (dir-entity (create-dir-entity tpl index-file path)))
    (if parent
	(insert-entity dir-entity parent :dir? t)
	(setf *root-entity* dir-entity))
    (push dir-entity *dir-stack*)))

(defun create-dir-entity (tpl index-file path)
  (declare (type (or null path) index-file tpl)
	   (type path path))
  (if tpl
      (let* ((output (determine-output (or index-file path)))
	     (op (make-op :apply-template output
			  :source index-file
			  :template tpl))
	     (context (create-context index-file output)))
	(make-dir-entity (path-identity path) :op op :context context))
      (make-dir-entity (path-identity path))))

(defun build-file-entity (path)
  (declare (type path path))
  (let ((output (determine-output path))
	(matched
          (find-if #'(lambda (r)
		       (match-pattern (first r) (path-identity path)))
		   *rules*)))
    (when matched
      (let ((parent (first *dir-stack*)))
	(unless parent
	  (error "dir-stack is nil while processing file ~a~%" output))
	(case (second matched)
          (:copy (insert-entity
		  (make-file-entity (path-identity output)
				    (make-op :copy output :source path)
				    (create-context path output))
		  parent))
          (:apply-template
           (let ((tpl (find-tpl path)))
             (when tpl
	       (insert-entity
		(make-file-entity (path-identity output)
				  (make-op :apply-template output
					   :source path
					   :template tpl)
				  (create-context path output))
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
  (let ((tree-outdated nil))
    (dolist (file (getf root :files))
      (when (process-op (getf file :op))
	(setf tree-outdated t)))
    (dolist (dir (getf root :dirs))
      (when (process-entities dir)
	(setf tree-outdated t)))
    (when (process-op (getf root :op) :dir? t :tree-outdated tree-outdated)
      (setf tree-outdated t))
    tree-outdated))

(defmacro gen-output (output &key if-any do-list log)
  (flet ((if-form (form)
	   (cond ((atom form) form)
		 ((eq :deps-outdated (car form))
		  `(apply #'deps-newer-p
			  ,output
			  (with-config-file ,@(cdr form))))
		 (t `(,@form)))))
    (let ((fn-applies
	    (mapcar #'(lambda (fn-apply)
			(destructuring-bind (fn &rest args) fn-apply
			  `(,fn ,@args)))
		    do-list)))
      `(if (or ,@(mapcar #'if-form if-any))
	   (progn
	     ,@fn-applies
	     ,@(if log
		   (list `(when *verbose* (format t ,@log))))
	     t)
	   (progn
	     (when *verbose* (format t "Skip ~a~%" ,output))
	     nil)))))

(defmacro if-www (fn &rest args)
  `(if (site-html-output-dir *site*)
       (,fn ,@args)))

(defun gemtext->html (source output)
  (declare (type path source output))
  (let* ((doc (with-open-file (s (abspath source))
		(mokubune/gemtext:parse s)))
	 (title-line
	   (find-if #'(lambda (line)
			(and (mokubune/gemtext:title-p line)
			     (= (mokubune/gemtext:title-level line) 1)))
		    doc))
	 (page-title (if title-line
			 (mokubune/gemtext:line-text title-line)
			 "Page"))
	 (html (with-output-to-string (s)
		 (let ((*standard-output* s))
		   (mokubune/gemtext:gemtext->html doc))))
	 (tpl (read-file-string
	       (abspath (make-path :identity *html-template-file*
				   :dir 'template))))
	 (abs-output (abspath output)))
    (ensure-directories-exist abs-output)
    (with-open-file (s abs-output :direction :output
				  :if-does-not-exist :create
				  :if-exists :supersede)
      (write-string (str:replace-using (list "__title__" page-title
					     "__content__" html)
				       tpl)
		    s)))
  (format t "converted ~a to ~a~%" source output))

(defun process-op (op &key dir? tree-outdated)
  (when op
    (ecase (getf op :action)
      (:copy
       (let ((source (getf op :source))
	     (output (getf op :output)))
	 (gen-output
	  output
	  :if-any (tree-outdated
		   (:deps-outdated (list source)))
	  :do-list ((ensure-dir-and-copy-file source output)
		    (if-www ensure-dir-and-copy-file
			    output
			    (mapdir output 'html-output)))
	  :log ("Target: ~a:  copied from ~a~%" output source))))
      (:apply-template
       (let* ((source (getf op :source))
	      (output (getf op :output))
	      (tpl (getf op :template))
	      (deps (list tpl)))
	 (when source (push source deps))
	 (gen-output
	  output
	  :if-any (tree-outdated (:deps-outdated deps))
	  :do-list ((apply-template output tpl :dir? dir?)
		    (if-www gemtext->html
			    output
			    (change-ext (mapdir output 'html-output) "html")))
	  :log ("Target: ~a: applied template ~a to ~a~%" output tpl source)))))))

(defun with-config-file (deps)
  (if *runtime-config-file*
      (cons *runtime-config-file* deps)
      deps))

(defun ensure-dir-and-copy-file (source output)
  (ensure-directories-exist (abspath output))
  (copy-file (abspath source) (abspath output)))

(defun apply-template (output template &key dir?)
  (let ((output-abs (abspath output))
	(entity (find-entity (if dir?
				 (directory-namestring (path-identity output))
				 (path-identity output)))))
    (ensure-directories-exist output-abs)
    (with-open-file (stream output-abs
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
		       :page-parent (parent-url (path-identity output)
						:dir? dir?)
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

(defun deps-newer-p (output &rest deps)
  (if (file-exists-p (abspath output))
      (let ((output-write-date (file-write-date (abspath output))))
	(some #'(lambda (file)
		  (>= (file-write-date (if (typep file 'path)
					   (abspath file)
					   file))
		      output-write-date))
	      deps))
      t))

(defparameter *compiled-templates* (make-hash-table :test #'equal))
(defun compile-template (tpl)
  (declare (type path tpl))
  (let ((key (format nil "~a-~a" (path-identity tpl)
		     (or (file-write-date (abspath tpl)) ""))))
    (or (gethash key *compiled-templates*)
	(setf (gethash key *compiled-templates*)
	      (with-open-file (stream (abspath tpl))
		(let ((template (make-string (file-length stream))))
		  (read-sequence template stream)
		  (cl-template:compile-template template)))))))
