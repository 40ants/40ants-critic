(defsystem "40ants-critic"
  :class :package-inferred-system
  :license "MIT"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :description "A wrapper around LISP-CRITIC which provides a better interface to analyze ASDF systems and a command-line interface."
  :homepage "https://40ants.com/40ants-critic/"
  :source-control (:git "https://github.com/40ants/40ants-critic")
  :pathname "src"
  :depends-on ("uiop"
               "40ants-critic/critic"
               "40ants-critic/changelog"))


(register-system-packages "eclector" (list "ECLECTOR.PARSE-RESULT"))
