(uiop:define-package #:40ants-critic
  (:use #:cl)
  (:nicknames #:40ants-critic/critic)
  (:import-from #:lisp-critic)
  (:export #:critique-asdf-system))
(in-package #:40ants-critic)


(defun ensure-asdf-system (name-or-system)
  (etypecase name-or-system
    (asdf:system name-or-system)
    (cons
     (case (first name-or-system)
       (:version
        (ensure-asdf-system
         (second name-or-system)))
       (:feature
        ;; Just ignore these kinds of dependencies for now
        NIL)
       (t
        (error "Unknown form of dependency: ~S"
               name-or-system))))
    ((or symbol string)
     (asdf:find-system name-or-system))))


(defun asdf-system-files (system)
  (let ((primary-system-name (asdf:primary-system-name system)))
    (labels ((recurse (name)
               (let ((system (ensure-asdf-system name)))
                 (when (and system
                            ;; We only interested in components of the
                            ;; same ASDF primary system, because
                            ;; we don't want to critic all system dependencies:
                            (equal (asdf:primary-system-name system)
                                   primary-system-name))
                   (append (asdf:module-components system)
                           (loop for component in (asdf:component-sideway-dependencies system)
                                 ;; for component-system = (asdf:find-system component)
                                 append (recurse component)))))))
      (loop for component in (recurse system)
            when (typep component 'asdf:cl-source-file)
              collect (asdf:component-pathname component)))))


(defun critique-file (file &optional (out *standard-output*) (names (lisp-critic:get-pattern-names)))
  "Returns a number of found problems."
  (with-open-file (in file)
    (let ((eof (list nil))
          (*package* (find-package "COMMON-LISP-USER"))
          (filename-already-printed nil)
          (problems-count 0))
      
      (do ((code (read in nil eof) (read in nil eof)))
          ((eq code eof) (values))

        (when (and (consp code)
                   (eql (car code)
                        'in-package))
          (setf *package*
                (find-package (second code))))
        
        (let ((critiques (lisp-critic::generate-critiques code names)))
          (when critiques
            (unless filename-already-printed
              (pprint file out)
              (setf filename-already-printed t))
            
            (lisp-critic::print-separator out #\*)
            
            (let ((*print-right-margin* lisp-critic::*output-width*))
              (pprint code out))
            (lisp-critic::print-critique-responses critiques out)
            (incf problems-count
                  (length critiques)))))

      (values problems-count))))


(defun critique-asdf-system (name)
  #+quicklisp
  (ql:quickload name)
  #-quicklisp
  (asdf:load-system name)
  
  (loop for filename in (asdf-system-files name)
        for num-problems = (critique-file filename)
        summing num-problems))
