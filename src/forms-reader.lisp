(uiop:define-package #:40ants-critic/forms-reader
  (:use #:cl)
  (:import-from #:cl-ppcre)
  (:import-from #:eclector))
(in-package #:40ants-critic/forms-reader)


(defclass parse-client (eclector.parse-result:parse-result-client)
  ())

(defmethod eclector.parse-result:make-expression-result
    ((client parse-client) (result t) (children t) (source t))
  result)

(defmethod eclector.parse-result:make-skipped-input-result
    ((client parse-client) (stream t) (reason t) (source t))
  (list :reason reason :source source))


(defun extract-ignored-critiques (comments)
  (loop for comment in comments
        append (cl-ppcre:register-groups-bind (first)
                   (";+ ignore-critiques: (.*)" comment)
                 (cl-ppcre:split ", *" first))))


(defun get-piece-of-file (stream from to)
  (let* ((old-position (prog1 (file-position stream)
                         (file-position stream from)))
         (element-type (stream-element-type stream))
         (length (- to from))
         (result (make-array length :element-type element-type)))
    
    (read-sequence result stream :end length)
    (file-position stream old-position)
    (values result)))


(defun skipped-to-comments (stream skipped-results)
  (loop for result in skipped-results
        for reason = (getf result :reason)
        for comment-p = (and (consp reason)
                             (eql (first reason)
                                  :line-comment))
        for source = (getf result :source)
        for from = (car source)
        for to = (cdr source)
        when comment-p
          collect (get-piece-of-file stream from to)))


;; ignore-critiques: function-too-long
(defun read-forms (filename &aux (client (make-instance 'parse-client)))
  "Returns a list where each item is a list of three items.
   First item is a form and second is a list of critiques to ignore.
   Third item is a package of the file. If there is no IN-PACKAGE
   for, then COMMON-LISP-USER will be returned.

   Ignored critiques are extracted from the preceeding comments.
   Comments should be in form:

   ;; ignore-critiques: function-too-long, sets-parameters
   ;; ignore-critiques: nth-on-list"

  (with-open-file (in filename)
    (handler-bind ((error (lambda (condition)
                            (declare (ignorable condition))
                            (let ((restart (find-restart 'eclector.reader:recover)))
                              (declare (ignorable restart))
                              ;; (format t "Recovering from error:~%~2@T~A~%using~%~2@T~A~%"
                              ;;         condition restart)
                              (eclector.reader:recover)))))
      (flet ((read-form ()
               (multiple-value-bind (form skipped)
                   (eclector.parse-result:read client in nil 'eof)
                 (let* ((comments (skipped-to-comments in skipped))
                        (critiques-to-ignore (extract-ignored-critiques comments)))
                   (list form
                         critiques-to-ignore)))))
        (loop with *package* = (find-package "COMMON-LISP-USER")
              for (code critiques-to-ignore) = (read-form)
                then (read-form)
              until (eql code 'eof)
              when (and (consp code)
                        (eql (car code)
                             'in-package))
                do (setf *package*
                         (find-package (second code)))
              collect (list code critiques-to-ignore *package*))))))
