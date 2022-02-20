(uiop:define-package #:40ants-critic/changelog
  (:use #:cl)
  (:import-from #:40ants-doc)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package #:40ants-critic/changelog)


(defchangelog ()
  (0.3.0 2022-02-21
         "Now you can ignore critiques by adding a comment before a top-level form.

          Comments should be in the form like this:

          ```lisp
          ;; ignore-critiques: x-minus-1, optionals, needless-and
          ```")
  (0.2.0 2022-02-20
         "Now it is possible to ignore some critiques, using IGNORE argument
          of 40ANTS-CRITIC:CRITIQUE-ASDF-SYSTEM function or `--ignore` command line option.")
  (0.1.0 2022-02-20
         "Initial version."))
