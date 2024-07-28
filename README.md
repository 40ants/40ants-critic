<a id="x-2840ANTS-CRITIC-3A-40README-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# 40ANTS-CRITIC

<a id="40-ants-critic-asdf-system-details"></a>

## 40ANTS-CRITIC ASDF System Details

* Description: A wrapper around `LISP-CRITIC` which provides a better interface to analyze `ASDF` systems and a command-line interface.
* Licence: `MIT`
* Author: Alexander Artemenko <svetlyak.40wt@gmail.com>
* Homepage: [https://40ants.com/40ants-critic/][821a]
* Source control: [GIT][4062]
* Depends on: [40ants-doc][2c00], [cl-ppcre][49b9], [docs-config][ce67], [eclector][8f25], [lisp-critic][ee56], [uiop][5274]

<a id="x-2840ANTS-CRITIC-3A-3A-40INSTALLATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Installation

This system can be installed from [Ultralisp][2a0d] like this:

```lisp
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
```
If you are going to use this utility from a command line, then you might install it
using [Roswell][795a]:

```bash
$ ros install  40ants/40ants-critic
Installing from github 40ants/40ants-critic
To load "40ants-critic":
  Load 1 ASDF system:
    40ants-critic
; Loading "40ants-critic"

; compiling file "/Users/art/.roswell/local-projects/40ants/critic/src/critic.lisp" (written 20 FEB 2022 12:54:52 PM):

; wrote /Users/art/.cache/common-lisp/sbcl-2.1.11-macosx-x64/Users/art/.roswell/local-projects/40ants/critic/src/critic-tmp5GEXGEG5.fasl
; compilation finished in 0:00:00.026
[1/3] System '40ants-critic' found. Loading the system..
[2/3] Processing build-hook..
[3/3] Attempting to install the scripts in roswell/ subdirectory of the system...
Found 1 scripts: lisp-critic
/Users/art/.roswell/bin/lisp-critic
```
Also, you might use this checker in your `CI` pipeline on the GitHub.
It might check all pull-requests to ensure the code will remain clean.

To learn more about using it as a part of the GitHub workflow, read
[`40ants-ci-docs/index::@critic`][371b] section.

<a id="x-2840ANTS-CRITIC-3A-3A-40USAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Usage

This wrapper provides a simple way to analyze code of a single `ASDF` system.
To get some advices, use [`critique-asdf-system`][c8a0] function. Difference between
this function and `LISP-CRITIC:CRITIQUE-FILE` function is that the latter
outputs all forms from the file even if there is no any advices.

[`critique-asdf-system`][c8a0] has `IGNORE` and `WHITELIST` keyword parameters. The
arguments can be a list of strings. Each string should be a code
shown in the square brackets in the critique output. `IGNORE` arguments will
be ignored, while `WHITELIST` arguments will be the only results. You can
only supply either `IGNORE` or `WHITELIST`, not both.

```lisp
(critique-asdf-system :lisp-critic :ignore '("let*-single"))
(critique-asdf-system :lisp-critic :whitelist '("let*-single" "needless-shiftf"))
```
Also, [`critique-asdf-system`][c8a0] returns a number of found problems which is useful
for `CI` pipelines. For example, `lisp-critic` script uses this number to report
that the unix command was failed:

```bash
lisp-critic reblocks-text-editor


#P"/Users/art/projects/lisp/zibaldone/src/utils/text.lisp"
**********************************************************************

(DEFUN REMOVE-HTML-TAGS (HTML-STRING)
  (LET* ((RESULT
          (CL-PPCRE:REGEX-REPLACE-ALL "<[^>]+>" HTML-STRING "")))
    (IF (STRING= RESULT +ZERO-WIDTH-SPACE+)
        RESULT
        (CL-PPCRE:REGEX-REPLACE-ALL +ZERO-WIDTH-SPACE+ RESULT ""))))
----------------------------------------------------------------------
[let*-single]: There's no need for LET* here. Use LET unless you can't.
----------------------------------------------------------------------
```
You can ignore all `let*-single` warnings by adding `--ignore 'let*-single'`
command line option or put a special comment before the top-level form:

You can ignore all `let*-single` warnings by adding `--ignore 'let*-single'`

```bash
lisp-critic --ignore 'let*-single' lisp-critic
```
or ignore all `if-no-else` and `needless-shiftf` warnings by adding

```bash
lisp-critic --ignore 'if-no-else,needless-shiftf' lisp-critic
```
in the command line. Alternatively you can use the short version `-i`
instead of `--ignore`.

You can whitelist `let*-single` warnings by adding `--whitelist 'let*-single'`

```bash
lisp-critic --whitelist 'let*-single' lisp-critic
```
or whitelist `if-no-else` and `needless-shiftf` warnings by adding

```bash
lisp-critic --whitelist 'if-no-else,needless-shiftf' lisp-critic
```
in the command line. Alternatively you can use the short version `-w`
instead of `--whitelist`.

```bash
lisp-critic -w 'let*-single' lisp-critic
lisp-critic -w 'if-no-else,needless-shiftf' lisp-critic
```
To ignore a top-level-form, you can put a special comment before:

```lisp
;; ignore-critiques: let*-single
(defun remove-html-tags (html-string)
   (let* ((result
     ...
```
Such comment can enumerate a multiple comma-separated critiques names.

<a id="x-2840ANTS-CRITIC-3A-3A-40API-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## API

<a id="x-2840ANTS-CRITIC-3ACRITIQUE-ASDF-SYSTEM-20FUNCTION-29"></a>

### [function](f849) `40ants-critic:critique-asdf-system` name &key (out \*standard-output\*) (ignore nil) (whitelist nil)

Outputs advices on how given `ASDF` system can be improved.
This function analyzes all lisp files of the given system and
outputs advices on how code might be improved.

`NAME` argument should be a string or symbol designator of `ASDF` system.

`IGNORE` argument can be a list of string. Each string should be a code
shown in the square brackets in the critique output.

`WHITELIST` argument can be a list of string. Each string should be a code
shown in the square brackets in the critique output.

Only `IGNORE` or `WHITELIST` can be used. Not both at the same time.

`OUT` argument is optional. It should be an output stream to write
advices to.

Result of the function is number of found problems.


[821a]: https://40ants.com/40ants-critic/
[c8a0]: https://40ants.com/40ants-critic/#x-2840ANTS-CRITIC-3ACRITIQUE-ASDF-SYSTEM-20FUNCTION-29
[371b]: https://40ants.com/ci/#x-2840ANTS-CI-DOCS-2FINDEX-3A-3A-40CRITIC-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29
[4062]: https://github.com/40ants/40ants-critic
[f849]: https://github.com/40ants/40ants-critic/blob/f456928169cf35214d3bcaa9bf2479b12b96e751/src/critic.lisp#L148
[795a]: https://github.com/roswell/roswell
[2c00]: https://quickdocs.org/40ants-doc
[49b9]: https://quickdocs.org/cl-ppcre
[ce67]: https://quickdocs.org/docs-config
[8f25]: https://quickdocs.org/eclector
[ee56]: https://quickdocs.org/lisp-critic
[5274]: https://quickdocs.org/uiop
[2a0d]: https://ultralisp.org

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
