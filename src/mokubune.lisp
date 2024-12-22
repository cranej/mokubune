(in-package #:mokubune)

;;; predicate helpers for rules
(defun glob-match (pattern)
  (let ((pat (cl-glob:compile-pattern pattern)))
   (lambda (name)
     (cl-glob:glob-matches-compiled pat name))))

(defun is-type (type)
  (lambda (name)
    (string= type (pathname-type name))))

;;; actions for rules
(defun apply-template (path)
  (let ((tpl (find-template-for path)))
    (unless tpl
      (error "Could not find template for ~a" path))
    
    (let* ((out (with-path path
                  :dir (context-output)))
           (deps (with-config-file (list path tpl)))
           (page (create-page path out)))
      (let ((regen (should-regenerate-p out deps)))
          (if regen
              (do-apply-template page tpl out)
              (log-message :info "skip ~a: up to date." path))
        (values page regen)))))

(defun gemtext->html (input)
  (declare (type path input))
  (let* ((output (with-path input :dir (context-output) :type "html"))         
         (tpl (with-path input
                :dir (site-template-dir *site*)
                :identity +html-template-file+))
         (deps (with-config-file (list input tpl)))
         (regen (should-regenerate-p output deps)))
    (if regen 
        (let* ((doc (with-open-file (s (path->pathname input))
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
               (tpl-content (read-file-string (path->pathname tpl)))
               (output-pathname (path->pathname output)))
          (ensure-directories-exist output-pathname)
          (with-open-file (s output-pathname :direction :output
				             :if-does-not-exist :create
				             :if-exists :supersede)
            (write-string (str:replace-using (list "__title__" page-title
					           "__content__" html)
				             tpl-content)
		          s))
          (log-message :debug "gemtext->html ~a to ~a" input output))
        (log-message :info "skip ~a: up to date." input)))
  nil)

(defun rst->html (input)
  (declare (type path input))
  (let* ((out (with-path input :dir (context-output) :type "html"))
         (deps (with-config-file (list input)))
         (regen (should-regenerate-p out deps)))
    (if regen
        (progn
          (ensure-directories-exist (path->pathname out))
          (uiop:run-program (list "rst2html5"
                                  "--time"
                                  (namestring (path->pathname input))
                                  (namestring (path->pathname out)))))
        (log-message :info "skip ~a: up to date." input))
    nil))

(defun copy (path)
  (let ((out (with-path path
               :dir (context-output))))    
    (let ((updated (do-copy-file path out)))     
      (values (create-page path out)
              updated))))

(defun copy-as-type (type &optional append)
  (lambda (path)
    (let ((out (if append
                   (with-path path :dir (context-output)
                     :filename (format nil "~a.~a"
                                       (file-namestring (path-identity path))
                                       type))
                   (with-path path :dir (context-output) :type type))))
      (let ((updated (do-copy-file path out)))
        (values (create-page path out)
                updated)))))

;; helper utilies
(defun do-copy-file (src dest)
  (let ((src (path->pathname src))
        (dest (path->pathname dest)))
    (if (or (not (probe-file dest))
            (>= (file-write-date src)
                (file-write-date dest)))
        (progn
          (ensure-directories-exist dest)
          (copy-file src dest)
          t)
        nil)))

(defun should-regenerate-p (out deps)
  (or (not (probe-path out))
      (let ((out-write-date (file-write-date (path->pathname out))))
        (some #'(lambda (dep)
	          (or (eq dep t)
                      (>= (file-write-date (path->pathname dep))
		          out-write-date)))
	      deps))))

(defun find-template-for (path)
  (let ((template-dir (site-template-dir *site*)))
    (let ((tpl-pathname
            (cond ((or (string= "" (path-identity path))
                       (and (null (pathname-directory (path-identity path)))
                            (string= "index" (pathname-name (path-identity path)))))
                   (probe-path
                    (make-path :dir template-dir
                               :identity +index-template-file+)))
                  ((string= (pathname-name (path-identity path)) "index")
                   (or (probe-path (with-path path
                                     :dir template-dir
                                     :filename +index-template-file+))
                       (probe-path (with-path path
                                     :dir template-dir
                                     :identity +sub-index-template-file+))
                       (probe-path (with-path path
                                     :dir template-dir
                                     :identity +index-template-file+))))
                  ((cl-fad:directory-pathname-p (path-identity path))
                   (probe-path (with-path path
                                 :dir template-dir
                                 :filename +index-template-file+)))
                  (t (or (probe-path (with-path path
                                       :dir template-dir
                                       :filename +page-template-file+))
                         (probe-path (with-path path
                                       :dir template-dir
                                       :identity +page-template-file+)))))))
      (and tpl-pathname
           (pathname->path tpl-pathname template-dir)))))

(defun parent-url (output-pathname)
  (if (string= "index" (pathname-name output-pathname))
      (format nil "/~@[~{~a/~}~]" (butlast (cdr (pathname-directory output-pathname))))
      (format nil "/~a" (directory-namestring output-pathname))))

(defun do-apply-template (page-obj tpl output &optional children)
  (let ((output-pathname (path->pathname output)))
    (ensure-directories-exist output-pathname)
    (with-open-file (stream output-pathname
			    :element-type 'character
			    :direction :output
			    :if-exists :supersede)
      (write-string
       ;; It's wired that if template contains emoji, there will be an extra #\Nul
       ;; appended to output string. Which cause emacs cannot display the text properly
       (string-trim
        '(#\Nul)
        (funcall (compile-template tpl)
		 (list :page page-obj
		       :page-parent (parent-url (path-identity output))
		       :children children
		       :site *site*)))
       stream))))

(defparameter *compiled-templates* (make-hash-table :test #'equal))
(defun compile-template (tpl)
  (declare (type path tpl))
  (let ((key (format nil "~a-~a" (path-identity tpl)
		     (or (file-write-date (path->pathname tpl)) ""))))
    (or (gethash key *compiled-templates*)
	(setf (gethash key *compiled-templates*)
	      (with-open-file (stream (path->pathname tpl))
		(let ((template (make-string (file-length stream))))
		  (read-sequence template stream)
		  (cl-template:compile-template template)))))))

;;; walking related
(defvar *walking-state*)
(defvar *walking-context* nil)

(defun context-input ()
  (getf *walking-context* :input))

(defun context-output ()
  (stage-output (getf *walking-context* :stage)))

(defun index-config ()
  (stage-index-config (getf *walking-context* :stage)))

(defun visit-file (name)
  (unless (string= (first (index-config)) (file-namestring name))
   (let ((path (pathname->path name (getf *walking-context* :input))))
     (let ((action
             (cadr (find-if #'(lambda (rule)
                                (funcall (car rule) (path-identity path)))
                            (stage-rules (getf *walking-context* :stage))))))
       (if action
           (multiple-value-bind (obj updated) (funcall action path)
             (unless (typep obj 'page)
               (log-message :debug "action returns none Page object: ~a"
                            (type-of obj)))
             (push (list :file path :obj obj :updated updated)
                   *walking-state*))
           (log-message :info "ignored: ~a due to no applicable action" path))))))

(defvar *current-dir-children* nil)
(defun find-file (filename)
  (getf
   (find-if #'(lambda (file)
                (string= (file-namestring (path-identity (getf file :file))) filename))
            (getf *current-dir-children* :files))
   :obj))

(defun get-files-of-dir (dir-path)
  (let ((dir (find-if #'(lambda (dir)
                          (string= (directory-namestring
                                    (path-identity (getf dir :dir)))
                                   dir-path))
                      (getf *current-dir-children* :dirs))))
    (mapcar #'(lambda (f) (getf f :obj))
            (sort-files (getf (getf dir :children) :files)))))

(defun index-directory (name children)
  (let ((index-config (index-config)))
    (when index-config
      (destructuring-bind (index-file output-file) index-config
        (let* ((input (pathname->path (or (probe-file
                                           (merge-pathnames* index-file name))
                                          name)
                                      (context-input)))
               (tpl (find-template-for input))
               (output (with-path input
                         :dir (context-output)
                         :filename output-file))
               (page (if (cl-fad:directory-pathname-p (path-identity input))
                         (make-instance 'page
                                        :body ""
                                        :date +date-unknown+
                                        :path (path-identity output))
                         (create-page input output))))
          (when tpl
            (let ((*current-dir-children* children))
              (do-apply-template page tpl output
                (mapcar #'(lambda (f) (getf f :obj))
                        (sort-files (getf children :files)))))
            page))))))

(defun sort-files (files)
  "Sort files, files with +date-unknown+ go last."
  (sort (copy-list files)
        #'(lambda (a b)
            (cond ((string= a +date-unknown+) nil)
                  ((string= b +date-unknown+) t)
                  (t (string> a b))))
        :key #'(lambda (a)
                 (let ((obj (getf a :obj)))                  
                   (if obj
                       (page-date (getf a :obj))
                       +date-unknown+)))))

(defun collect-children-of (dir)
  (flet ((is-child (state-obj)
           (when state-obj
             (if (eq (car state-obj) :dir)
                 (pathname-equal
                  dir
                  (cl-fad:pathname-parent-directory
                   (path->pathname (cadr state-obj))))
                 (pathname-equal
                  dir
                  (cl-fad:pathname-directory-pathname
                   (path->pathname (cadr state-obj))))))))
   (let ((files nil)
         (dirs nil)
         has-update)
     (loop for obj = (car *walking-state*)
           while (is-child obj)
           do (if (eq (car obj) :dir)
                  (push (pop *walking-state*) dirs)
                  (push (pop *walking-state*) files))
           do (if (getf obj :updated)
                  (setf has-update t)))
     (values (list :dirs dirs :files files)
             has-update))))

(defun visit-directory (name)
  (let ((path (pathname->path name (context-input))))
   (multiple-value-bind (children has-update) (collect-children-of name)
     (when (and (index-config) (not has-update))
       (log-message :info "dir ~a is up to date." path))
     (let ((obj (and has-update
                     (index-directory name children))))
       (push (list :dir path
                   :children children
                   :obj obj
                   :updated has-update)
             *walking-state*)))))

(defun visit (name)
  (if (cl-fad:directory-pathname-p name)
      (visit-directory name)
      (visit-file name)))

(defun walk (name)
  (let ((*walking-state* nil))
    (cl-fad:walk-directory name #'visit :directories :depth-first)
    *walking-state*))

(defun process-stages (stages)
  (loop with prev-output
        for stage in stages
        for i = 1 then (1+ i)
        do (let ((*walking-context* (list :stage stage
                                          :input (merge-pathnames*
                                                  (or prev-output
                                                      (site-content-dir *site*))
                                                  *cwd*))))
             (log-message :info "Stage ~d~@[ ~a~]:"
                          i (stage-name stage))
             (log-message :debug "stage finished:~% ~s~"
                          (walk (getf *walking-context* :input)))
             (setf prev-output (stage-output stage)))))

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
(defparameter *stages*
  (list (mk-stage ("public/gemini/" :name "Default"
                                    :index-config '("index.gmi" "index.gmi"))
                  (is-type "gmi") apply-template
                 t copy)))

(defun set-stages (stages)
  (setf *stages* stages))

(defun rewrite-url-of (path-extension)
  (declare (type string path-extension))
  (pushnew path-extension mokubune/gemtext:*rewrite-file-types*))

(defun do-generating ()
  (let ((*runtime-config-file* (probe-file (merge-pathnames* "config.lisp" *cwd*))))
    (when *runtime-config-file*
      (load *runtime-config-file*))
    (process-stages *stages*)))

(defun with-config-file (deps)
  (if *runtime-config-file*
      (cons (pathname->path *runtime-config-file*) deps)
      deps))