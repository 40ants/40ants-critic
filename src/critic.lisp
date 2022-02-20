;; Some comment
(uiop:define-package #:40ants-critic
  (:use #:cl)
  (:nicknames #:40ants-critic/critic)
  (:import-from #:lisp-critic)
  (:import-from #:40ants-doc
                #:defsection-copy
                #:defsection)
  (:import-from #:docs-config
                #:docs-config)
  (:export #:critique-asdf-system
           #:@index
           #:@readme))
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


(defun critique-name (note)
  "Returns the \"critique\" code as a lowercased string"
  (string-downcase (symbol-name (lisp-critic::critique-name note))))


(defun remove-ignored (critics ignore)
  (flet ((should-be-ignored (critique)
           (let ((code (critique-name critique)))
             (member code ignore
                     :test #'string-equal))))
    (remove-if #'should-be-ignored critics)))


(defun make-response-string (name response blist)
  (let ((format-string (lisp-critic:response-format-string response))
        (pattern (extend-match::instantiate-pattern (lisp-critic::response-args response)
                                                    blist)))
    (format nil "~&[~A]: ~?"
            (string-downcase (symbol-name name))
            format-string
            pattern)))


(defun print-critique-response (critique
                                &optional (stream *standard-output*))
  (let ((name (lisp-critic::critique-name critique))
        (blist (lisp-critic::critique-blist critique))
        (code (lisp-critic::critique-code critique)))
    (let ((response (lisp-critic::get-response name)))
      (cond ((null response)
             (let ((*print-lines* 2) (*print-pretty* t)
                   (*print-right-margin* lisp-critic::*output-width*))
               (format stream "~&~A: Code: ~W" name code)))
            (t
             (write-wrap:write-wrap stream
                                    (make-response-string name response blist)
                                    lisp-critic::*output-width*)))
      (lisp-critic::print-separator stream))))


(defun print-critique-responses (critiques
                                 &optional (stream *standard-output*))
  (let ((*print-pretty* nil))
    (when critiques
      (lisp-critic::print-separator stream))
    (dolist (critique critiques)
      (print-critique-response critique stream))))


(defun critique-file (file &key (out *standard-output*)
                             (names (lisp-critic:get-pattern-names))
                             (ignore nil))
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
        
        (let ((critiques (remove-ignored (lisp-critic::generate-critiques code names)
                                         ignore)))
          (when critiques
            (unless filename-already-printed
              (pprint file out)
              (setf filename-already-printed t))
            
            (lisp-critic::print-separator out #\*)
            
            (let ((*print-right-margin* lisp-critic::*output-width*))
              (pprint code out))
            (print-critique-responses critiques out)
            (incf problems-count
                  (length critiques)))))

      (values problems-count))))


(defun critique-asdf-system (name &key
                                    (out *standard-output*)
                                    (ignore nil))
  "Outputs advices on how given ASDF system can be improved.
   This function analyzes all lisp files of the given system and
   outputs advices on how code might be improved.

   NAME argument should be a string or symbol designator of ASDF system.

   IGNORE argument can be a list of string. Each string should be a code
   shown in the square brackets in the critique output.

   OUT argument is optional. It should be an output stream to write
   advices to.

   Result of the function is number of found problems."
  #+quicklisp
  (ql:quickload name :silent t)
  #-quicklisp
  (asdf:load-system name)
  
  (loop for filename in (asdf-system-files name)
        for num-problems = (critique-file filename
                                          :out out
                                          :ignore ignore)
        summing num-problems))


;;;;;;;;;;;;;;;;;;;
;; Documentation ;;
;;;;;;;;;;;;;;;;;;; 

(defmethod docs-config ((system (eql (asdf:find-system "40ants-critic"))))
  ;; 40ANTS-DOC-THEME-40ANTS system will bring
  ;; as dependency a full 40ANTS-DOC but we don't want
  ;; unnecessary dependencies here:
  #+quicklisp
  (uiop:symbol-call :ql :quickload :40ants-doc-theme-40ants)
  #-quicklisp
  (asdf:load-system :40ants-doc-theme-40ants)

  (list :theme
        (find-symbol "40ANTS-THEME"
                     (find-package "40ANTS-DOC-THEME-40ANTS"))))


(defsection @index (:title "40ANTS-CRITIC"
                    :ignore-words ("CI"
                                   "ASDF"
                                   "MIT"
                                   "LISP-CRITIC"
                                   "LISP-CRITIC:CRITIQUE-FILE")
                    :external-links (("LISP-CRITIC" . "https://github.com/g000001/lisp-critic")))
  (40ants-critic system)
  (@installation section)
  (@usage section)
  (@api section))

(defsection-copy @readme @index)


(defsection @installation (:title "Installation")
  "This system can be installed from [Ultralisp](https://ultralisp.org) like this:

```lisp
(ql-dist:install-dist \"http://dist.ultralisp.org/\"
                      :prompt nil)
```

If you are going to use this utility from a command line, then you might install it
using [Roswell](https://github.com/roswell/roswell):

```bash
$ ros install  40ants/40ants-critic
Installing from github 40ants/40ants-critic
To load \"40ants-critic\":
  Load 1 ASDF system:
    40ants-critic
; Loading \"40ants-critic\"

; compiling file \"/Users/art/.roswell/local-projects/40ants/critic/src/critic.lisp\" (written 20 FEB 2022 12:54:52 PM):

; wrote /Users/art/.cache/common-lisp/sbcl-2.1.11-macosx-x64/Users/art/.roswell/local-projects/40ants/critic/src/critic-tmp5GEXGEG5.fasl
; compilation finished in 0:00:00.026
[1/3] System '40ants-critic' found. Loading the system..
[2/3] Processing build-hook..
[3/3] Attempting to install the scripts in roswell/ subdirectory of the system...
Found 1 scripts: lisp-critic
/Users/art/.roswell/bin/lisp-critic
```
")


(defsection @usage (:title "Usage")
  "This wrapper provides a simple way to analyze code of a single ASDF system.
   To get some advices, use CRITIQUE-ASDF-SYSTEM function. Difference between
   this function and LISP-CRITIC:CRITIQUE-FILE function is that the latter
   outputs all forms from the file even if there is no any advices.

   Also, CRITIQUE-ASDF-SYSTEM returns a number of found problems which is useful
   for CI pipelines. For example, `lisp-critic` script uses this number to report
   that the unix command was failed:

   ```bash
   lisp-critic reblocks-text-editor


   #P\"/Users/art/projects/lisp/zibaldone/src/utils/text.lisp\"
   **********************************************************************

   (DEFUN REMOVE-HTML-TAGS (HTML-STRING)
     (LET* ((RESULT
             (CL-PPCRE:REGEX-REPLACE-ALL \"<[^>]+>\" HTML-STRING \"\")))
       (IF (STRING= RESULT +ZERO-WIDTH-SPACE+)
           RESULT
           (CL-PPCRE:REGEX-REPLACE-ALL +ZERO-WIDTH-SPACE+ RESULT \"\"))))
   ----------------------------------------------------------------------
   There's no need for LET* here. Use LET unless you can't.
   ----------------------------------------------------------------------
   ```")


(defsection @api (:title "API")
  (critique-asdf-system function))
