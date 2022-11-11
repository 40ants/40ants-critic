(uiop:define-package #:40ants-critic/changelog
  (:use #:cl)
  (:import-from #:40ants-doc)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package #:40ants-critic/changelog)


(defchangelog (:ignore-words ("ASDF"))
  (0.4.1 2022-11-10
         "* Internal function asdf-system-files was fixed and now retursn unique filenames.

            Previosly multiple copies of the same file returned for some package inferred ASDF systems
            which produced multiple copies of critiqies and slowed down the shole process.")
  (0.4.0 2022-02-22
         "* Forms are printed in a more readable way now.
            Their symbols are printed relative to the file's package.")
  (0.3.0 2022-02-21
         "* Now you can ignore critiques by adding a comment before a top-level form.

            Comments should be in the form like this:

            ```
            ;; ignore-critiques: x-minus-1, optionals, needless-and
            ```

          * Command utility now outputs total number of found problems.")
  (0.2.0 2022-02-20
         "Now it is possible to ignore some critiques, using IGNORE argument
          of 40ANTS-CRITIC:CRITIQUE-ASDF-SYSTEM function or `--ignore` command line option.")
  (0.1.0 2022-02-20
         "Initial version."))
