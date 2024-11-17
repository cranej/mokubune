(in-package #:mokubune)

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

