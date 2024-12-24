(in-package #:mokubune)

(defstruct stage
  "A stage in the pipeline.

Slot ``output`` is the output directory relative to *cwd*. For example a possible value can be 'public/gemini/'.

Slot ``rules`` is a list, each item itself is a list of two elements ``(predicate action)``:

   * predicate is a function that accepts a sole ``path`` object, returns ``t`` on intrested files, and false value otherwise;
   * action is a fuction that accepts a sole ``path`` object which is the file to be processed, and perform desired action to it.

Slot ``index-config`` is a list of two items ``(index-file-name index-output-file-name)``, if this slot is not supplied, auto indexing of directories will not be performed.

  * item 'index-file-name' is the name of the file which should be treat as index file, not regular page, for example \"index.gmi\";
  * item 'index-output-file-name' is the name of the file into which auto indexing result are written, for example \"index.html\" "
  name output rules
  index-config)

(defmacro mk-stage (options &rest rules)
  (unless (evenp (length rules))
    (error "Odd numbers of rules"))

  (flet ((parse-options (options)
           (cond ((atom options) (values options nil nil))
                 (t (destructuring-bind (output &key name index-config) options
                      (values output name index-config))))))
    (multiple-value-bind (output name index-config) (parse-options options)
      `(make-stage :name ,name
                   :output ,output
                   :index-config ,index-config
                   :rules (list
                           ,@(loop for (pred action) on rules by #'cddr
                                   collect (let ((pred (if (eq pred t)
                                                           '(constantly t)
                                                           pred))
                                                 (action (if (symbolp action)
                                                             `(function ,action)
                                                             action)))
                                             (list 'list pred action))))))))
