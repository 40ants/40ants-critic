(uiop:define-package #:40ants-critic/main
  (:use #:cl)
  (:import-from #:defmain
                #:defmain)
  (:import-from #:40ants-critic)
  (:import-from #:split-sequence
                #:split-sequence))
(in-package #:40ants-critic/main)


(defun split-by-comma (text)
  (loop for item in (split-sequence #\, text)
        collect (string-trim '(#\Space) item)))


(defmain (main :program-name "lisp-critic")
    ((ignore "Comma-separated list of codes to ignore.")
     (whitelist "Comma-separated list of codes to whitelist.")
     asdf-system)
  (let* ((ignore (when ignore
                   (split-by-comma ignore)))
         (whitelist (when whitelist
                      (split-by-comma whitelist)))
         (num-problems (40ants-critic:critique-asdf-system
                        asdf-system
                        :ignore ignore
                        :whitelist whitelist)))
    (unless (zerop num-problems)
      (format t "~2&Total: ~A problem~:P~%"
              num-problems))
    (uiop:quit num-problems)))
