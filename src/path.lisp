(in-package #:mokubune)

(defstruct path
  "Represent a path relative to *cwd*. For exampe, for *cwd* \"/my/path/to/cwd/\", pathname \"/my/path/to/cwd/contents/blog/a-post-2024-11-10.gmi\" is represented as a ``path`` object with \"blog/a-post-2024-11-10.gmi\" as the ``relative`` slot, and \"contents/\" as  the ``base`` slot. "
  (relative "" :type simple-string)
  (base "" :type simple-string))

(defmethod print-object ((object path) stream) 
  (format stream "~a:~a" (path-base object) (path-relative object)))

(defun ensure-absolute (pathspec)
  (if (cl-fad:pathname-absolute-p pathspec)
      pathspec
      (merge-pathnames* pathspec *cwd*)))

(defun pathname->path (pathname &optional (base *cwd*))
  (let ((pathname (ensure-absolute pathname))
        (base (ensure-absolute base)))
    (let ((relative (enough-namestring pathname base))
          (base (enough-namestring base *cwd*)))
      (make-path :relative relative :base base))))

(defun path->pathname (path)
  (merge-pathnames* (path-relative path)
                    (merge-pathnames* (path-base path)
                                      *cwd*)))

;; TODO: get rid of this
(defun pathname-equal (a b)
  (cl-fad:pathname-equal (make-pathname :version nil :defaults a)
                         (make-pathname :version nil :defaults b)))

(defmacro with-path (path &key (base nil base-supplied-p)
                            (relative nil relative-supplied-p)
                            (filename nil filename-supplied-p)
                            (type nil type-supplied-p))
  (let ((newobj (gensym))
        (setfs nil))
    (when base-supplied-p
      (push `(setf (path-base ,newobj) ,base) setfs))
    (if relative-supplied-p
        (push `(setf (path-relative ,newobj) ,relative) setfs)
        (if filename-supplied-p
            (push `(setf (path-relative ,newobj)
                         (namestring
                          (merge-pathnames* ,filename
                                            (path-relative ,path))))
                  setfs)
            (when type-supplied-p
              (push `(setf (path-relative ,newobj)
                           (namestring
                            (make-pathname :type ,type
                                           :defaults (path-relative ,path))))
                    setfs))))
    `(let ((,newobj (copy-path ,path)))
       ,@setfs 
       ,newobj)))

(defmacro probe-path (path)
  `(probe-file (path->pathname ,path)))

