(in-package #:mokubune)

;;; predicate helpers for rules
(defun glob-match (pattern)
  (let ((pat (cl-glob:compile-pattern pattern)))
   (lambda (path)
     (cl-glob:glob-matches-compiled pat (path-relative path)))))

(defun is-type (type)
  (lambda (path)
    (string= type (pathname-type (path-relative path)))))

;;; actions for rules
(defun apply-template (path)
  (let ((tpl (find-template-for path)))
    (unless tpl
      (error "Could not find template for ~a" path))

    (let* ((out (with-path path
                  :base (context-output)))
           (deps (with-config-file (list path tpl)))
           (page (create-page path out)))
      (let ((regen (deps-outdated-p out deps)))
          (if regen
              (prog1 (do-apply-template page tpl out)
                (log-message :info "apply template ~a on ~a, output: ~a"
                             tpl path out))
              (log-message :debug "skip ~a: up to date." path))
        (values page regen)))))

(defun gemtext->html (input)
  (declare (type path input))
  (let* ((output (with-path input :base (context-output) :type "html"))
         (tpl (with-path input
                :base (site-template-dir *site*)
                :relative +html-template-file+))
         (deps (with-config-file (list input tpl)))
         (regen (deps-outdated-p output deps)))
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
          (log-message :info "gemtext->html ~a to ~a" input output))
        (log-message :debug "skip ~a: up to date." input)))
  nil)

(defun rst->html (input)
  (declare (type path input))
  (let* ((out (with-path input :base (context-output) :type "html"))
         (deps (with-config-file (list input)))
         (regen (deps-outdated-p out deps)))
    (if regen
        (progn
          (ensure-directories-exist (path->pathname out))
          (uiop:run-program (list "rst2html5"
                                  "--time"
                                  (namestring (path->pathname input))
                                  (namestring (path->pathname out))))
          (log-message :info "rst->html ~a to ~a" input out))
        (log-message :debug "skip ~a: up to date." input))
    nil))

(defun copy (path)
  (let ((out (with-path path
               :base (context-output))))
    (let ((updated (do-copy-file path out)))
      (values (create-page path out)
              updated))))

(defun copy-as-type (type &optional append)
  (lambda (path)
    (let ((out (if append
                   (with-path path :base (context-output)
                     :filename (format nil "~a.~a"
                                       (file-namestring (path-relative path))
                                       type))
                   (with-path path :base (context-output) :type type))))
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
          (uiop:copy-file src dest)
          (log-message :info "copy ~a to ~a" src dest)
          t)
        (progn
          (log-message :debug "skip ~a: up to date" src)
          nil))))

(defun deps-outdated-p (out deps)
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
            (cond ((or (string= "" (path-relative path))
                       (and (null (pathname-directory (path-relative path)))
                            (string= "index" (pathname-name (path-relative path)))))
                   (probe-path
                    (make-path :base template-dir
                               :relative +index-template-file+)))
                  ((string= (pathname-name (path-relative path)) "index")
                   (or (probe-path (with-path path
                                     :base template-dir
                                     :filename +index-template-file+))
                       (probe-path (with-path path
                                     :base template-dir
                                     :relative +sub-index-template-file+))
                       (probe-path (with-path path
                                     :base template-dir
                                     :relative +index-template-file+))))
                  ((cl-fad:directory-pathname-p (path-relative path))
                   (probe-path (with-path path
                                 :base template-dir
                                 :filename +index-template-file+)))
                  (t (or (probe-path (with-path path
                                       :base template-dir
                                       :filename +page-template-file+))
                         (probe-path (with-path path
                                       :base template-dir
                                       :relative +page-template-file+)))))))
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
		       :page-parent (parent-url (path-relative output))
		       :children children
		       :site *site*)))
       stream))))

(defparameter *compiled-templates* (make-hash-table :test #'equal))
(defun compile-template (tpl)
  (declare (type path tpl))
  (let ((key (format nil "~a-~a" (path-relative tpl)
		     (or (file-write-date (path->pathname tpl)) ""))))
    (or (gethash key *compiled-templates*)
	(setf (gethash key *compiled-templates*)
	      (with-open-file (stream (path->pathname tpl))
		(let ((template (make-string (file-length stream))))
		  (read-sequence template stream)
		  (cl-template:compile-template template)))))))

;;; walking related
(defvar *walking-state*)
(defvar *walking-context* nil
  "Context object while processing a stage, a plist in form ``(:stage stage :input stage-input-directory)``.")

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
                                (funcall (car rule) path))
                            (stage-rules (getf *walking-context* :stage))))))
       (if action
           (multiple-value-bind (obj updated) (funcall action path)
             (unless (typep obj 'page)
               (log-message :debug "action returns none Page object: ~a"
                            (type-of obj)))
             (push (list :file path :obj obj :updated updated)
                   *walking-state*))
           (log-message :info "skip ~a: no applicable action" path))))))

(defvar *current-dir-children* nil
  "This variable is bound only when applying template of directory index file, to make the entire descendant tree of current directory available in the template.")

(defun find-file (filename)
  (getf
   (find-if #'(lambda (file)
                (string= (file-namestring (path-relative (getf file :file))) filename))
            (getf *current-dir-children* :files))
   :obj))

(defun get-files-of-dir (dir-path)
  (let ((dir (find-if #'(lambda (dir)
                          (string= (directory-namestring
                                    (path-relative (getf dir :dir)))
                                   dir-path))
                      (getf *current-dir-children* :dirs))))
    (mapcar #'(lambda (f) (getf f :obj))
            (sort-files (getf (getf dir :children) :files)))))

(defun index-directory (name children children-updated)
  (let ((index-config (index-config)))
    (when index-config
      (destructuring-bind (index-file output-file) index-config
        (let* ((input (pathname->path (or (probe-file
                                           (merge-pathnames* index-file name))
                                          name)
                                      (context-input)))
               (tpl (find-template-for input)))
          (if tpl
              (let* ((output (with-path input
                               :base (context-output)
                               :filename output-file))
                     (deps-outdated (deps-outdated-p output (list tpl))))
                (if (or children-updated deps-outdated)
                    (let ((*current-dir-children* children)
                          (page (if (cl-fad:directory-pathname-p (path-relative input))
                                    (make-instance 'page
                                                   :body ""
                                                   :date +date-unknown+
                                                   :path (path-relative output))
                                    (create-page input output))))
                      (do-apply-template page tpl output
                        (mapcar #'(lambda (f) (getf f :obj))
                                (sort-files (getf children :files))))
                      (log-message :info
                                   "indexing dir ~a by applying template ~a"
                                   input tpl)
                      page)
                    (progn (log-message :debug "skip indexing dir ~a: up to date"
                                        input)
                           nil)))
              (progn (log-message :debug
                                  "skip indexing dir ~a: no applicable template"
                                  input)
                     nil)))))))

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
     (let ((obj (index-directory name children has-update)))
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
             (log-message :info "stage ~d~@[ ~a~] started"
                          i (stage-name stage))
             (log-message :debug "stage finished:~% ~s~"
                          (walk (getf *walking-context* :input)))
             (log-message :info "stage ~d~@[ ~a~] finished~%"
                          i (stage-name stage))
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
