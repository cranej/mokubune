(in-package #:mokubune)

;;; TODO: rename field names
(defstruct path
  (identity "" :type simple-string)
  (dir "" :type simple-string))

(defmethod print-object ((object path) stream) 
  (format stream "~a:~a" (path-dir object) (path-identity object)))

(defun ensure-absolute (pathspec)
  (if (cl-fad:pathname-absolute-p pathspec)
      pathspec
      (merge-pathnames* pathspec *cwd*)))

(defun pathname->path (pathname &optional (base *cwd*))
  (let ((pathname (ensure-absolute pathname))
        (base (ensure-absolute base)))
    (let ((identity (enough-namestring pathname base))
          (dir (enough-namestring base *cwd*)))
      (make-path :identity identity :dir dir))))

(defun path->pathname (path)
  (merge-pathnames* (path-identity path)
                    (merge-pathnames* (path-dir path)
                                      *cwd*)))

;; TODO: get rid of this
(defun pathname-equal (a b)
  (cl-fad:pathname-equal (make-pathname :version nil :defaults a)
                         (make-pathname :version nil :defaults b)))

(defmacro with-path (path &key (dir nil dir-supplied-p)
                            (identity nil identity-supplied-p)
                            (filename nil filename-supplied-p)
                            (type nil type-supplied-p))
  (let ((newobj (gensym))
        (setfs nil))
    (when dir-supplied-p
      (push `(setf (path-dir ,newobj) ,dir) setfs))
    (if identity-supplied-p
        (push `(setf (path-identity ,newobj) ,identity) setfs)
        (if filename-supplied-p
            (push `(setf (path-identity ,newobj)
                         (namestring
                          (merge-pathnames* ,filename
                                            (path-identity ,path))))
                  setfs)
            (when type-supplied-p
              (push `(setf (path-identity ,newobj)
                           (namestring
                            (make-pathname :type ,type
                                           :defaults (path-identity ,path))))
                    setfs))))
    `(let ((,newobj (copy-path ,path)))
       ,@setfs 
       ,newobj)))

(defmacro probe-path (path)
  `(probe-file (path->pathname ,path)))

