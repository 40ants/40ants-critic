(uiop:define-package #:40ants-critic/main
  (:use #:cl)
  (:import-from #:defmain
                #:defmain)
  (:import-from #:40ants-critic))
(in-package #:40ants-critic/main)


(defmain (main :program-name "lisp-critic")
    (asdf-system)
  (let ((num-problems (40ants-critic:critique-asdf-system asdf-system)))
    (uiop:quit num-problems)))
