(in-package #:mokubune)

(defstruct stage
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
