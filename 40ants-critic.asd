(defsystem "40ants-critic"
  :class :package-inferred-system
  :license "MIT"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :description "A wrapper around LISP-CRITIC which provides a better interface to analyze ASDF systems and a command-line interface."
  :homepage "https://40ants.com/critic/"
  :source-control (:git "https://github.com/40ants/critic")
  :pathname "src"
  :depends-on ("40ants-critic/critic"))
