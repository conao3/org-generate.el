;;; org-generate-tests.el --- Test definitions for org-generate  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; URL: https://github.com/conao3/org-generate.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Test definitions for `org-generate'.


;;; Code:

(require 'cort)
(require 'with-simulated-input)
(require 'org-generate)

(setq cort--dir
      (expand-file-name
       (format "org-generate-%04d" (random (round 1e4)))
       temporary-file-directory))

(defun cort--file-contents (path)
  "Get all contents of file located at PATH from `cort--dir'."
  (let ((path* (expand-file-name path cort--dir)))
    (unless (file-readable-p path*)
      (error "Missing file: %s" path*))
    (with-temp-buffer
      (insert-file-contents path*)
      (buffer-string))))

(defmacro with-cort--org-generate-buffer (contents &rest body)
  "Exec BODY in temp buffer that has CONTENTS."
  (declare (indent 1))
  `(let ((org-generate-root cort--dir)
         (org-generate--file-buffer (get-buffer-create "*temp*"))
         (org-confirm-babel-evaluate nil))
     (with-current-buffer org-generate--file-buffer
       (erase-buffer)
       (insert ,contents)
       (goto-char (point-min))
       ,@body)))

(defmacro cort-deftest--org-generate (name testlst)
  "Define a test case with the NAME.
TESTLST is list of (GIVEN EXPECT)."
  (declare (indent 1))
  `(cort-deftest ,name
     (cort-generate-with-hook :equal
       (lambda () (mkdir cort--dir))
       (lambda () (ignore-errors (delete-directory cort--dir 'force)))
       ,testlst)))


;;; Test definition

(setq org-generate-show-save-message nil)

;; silence test
;; it maybe hide some important message!
(progn
  (require 'ob-emacs-lisp)
  (fset 'message 'ignore)
  (defun org-babel-expand-body:emacs-lisp (body params)
    "Expand BODY according to PARAMS, return the expanded body."
    (let ((vars (org-babel--get-vars params))
          (print-level nil)
          (print-length nil))
      (if (null vars) (concat body "\n")
        (format "(let (%s)\n%s\n)"
                (mapconcat
                 (lambda (var)
                   (format "%S"
                           ;; (print `(,(car var) ',(cdr var)))
                           `(,(car var) ',(cdr var))))
                 vars "\n      ")
                body)))))

(cort-deftest org-generate/simple
  (cort-generate :equal
    '(((+ 2 3) 5))))

(cort-deftest--org-generate org-generate/onefile
  '(((with-cort--org-generate-buffer "\
* hugo
** page
#+begin_src markdown
  ---
  title: \"xxx\"
  ---

  ### 1. First
  xxxx
#+end_src
"
       (buffer-string))
     "\
* hugo
** page
#+begin_src markdown
  ---
  title: \"xxx\"
  ---

  ### 1. First
  xxxx
#+end_src
")

    ((with-cort--org-generate-buffer "\
* hugo
** page
*** page
#+begin_src markdown
  ---
  title: \"xxx\"
  date: xx/xx/xx
  draft: true
  ---

  ### 1. First
  xxxx

  ### 2. Second
  yyyy
#+end_src
"
       (org-generate "hugo/page")
       (cort--file-contents "page"))
     "\
---
title: \"xxx\"
date: xx/xx/xx
draft: true
---

### 1. First
xxxx

### 2. Second
yyyy
")))

(cort-deftest--org-generate org-generate/heading-with-macro
  '(((with-cort--org-generate-buffer "\
#+MACRO: filename page.md
* hugo
** page
*** {{{filename}}}
#+begin_src markdown
  ---
  title: \"xxx\"
  ---

  ### 1. First
  xxxx
#+end_src
"
       (org-generate "hugo/page")
       (cort--file-contents "page.md"))
     "\
---
title: \"xxx\"
---

### 1. First
xxxx
")))

(cort-deftest--org-generate org-generate/heading-with-macro-using-user-input
  '(((with-cort--org-generate-buffer "\
#+MACRO: get-directory (eval (format \"%s/\" (read-string \"Filename: \")))
* hugo
** page
*** {{{get-directory}}}
**** page.md
#+begin_src markdown
  ---
  title: \"xxx\"
  ---

  ### 1. First
  xxxx
#+end_src
"
       (with-simulated-input
           "awesome RET"
         (org-generate "hugo/page"))
       (cort--file-contents "awesome/page.md"))
     "\
---
title: \"xxx\"
---

### 1. First
xxxx
")))

(cort-deftest--org-generate org-generate/set-variable-with-macro
  '(((with-cort--org-generate-buffer (format "\
#+NAME: hugo-root
: %s/
#+MACRO: hugo-root (eval (concat \":org-generate-root: \" (org-sbe \"hugo-root\") $1))
* hugo
** page
:PROPERTIES:
{{{hugo-root(content/blog/)}}}
:END:
*** page.md
#+begin_src markdown
  ---
  title: \"xxx\"
  ---

  ### 1. First
  xxxx
#+end_src
" cort--dir)
       (mkdir (expand-file-name "content/blog" cort--dir) 'parents)
       (let ((org-generate-root nil))
         (org-generate "hugo/page"))
       (cort--file-contents "content/blog/page.md"))
     "\
---
title: \"xxx\"
---

### 1. First
xxxx
")))

(cort-deftest--org-generate org-generate/set-variable-using-property
  '(((with-cort--org-generate-buffer (format "\
#+MACRO: hugo-root-path (eval (concat \":org-generate-root: \" (org-entry-get-with-inheritance \"root\") $1))
* hugo
:PROPERTIES:
:root: %s/
:END:
** page
:PROPERTIES:
{{{hugo-root-path(content/blog/)}}}
:END:
*** page.md
#+begin_src markdown
  ---
  title: \"xxx\"
  ---

  ### 1. First
  xxxx
#+end_src
" cort--dir)
       (mkdir (expand-file-name "content/blog" cort--dir) 'parents)
       (let ((org-generate-root nil))
         (org-generate "hugo/page"))
       (cort--file-contents "content/blog/page.md"))
     "\
---
title: \"xxx\"
---

### 1. First
xxxx
")

    ((with-cort--org-generate-buffer (format "\
* hugo
:PROPERTIES:
:root: %s/
:END:
#+NAME: root
#+BEGIN_SRC emacs-lisp :exports none :results raw :var path=\"\"
  (concat \":org-generate-root: \"
          (org-entry-get-with-inheritance \"root\")
          (format \"%%s\" path))
#+END_SRC
#+MACRO: hugo-root-path (eval (org-sbe \"root\" (path $$1)))
** page
:PROPERTIES:
{{{hugo-root-path(content/blog/)}}}
:END:
" cort--dir)
       (mkdir (expand-file-name "content/blog" cort--dir) 'parents)
       (let ((org-generate-root nil))
         (org-generate "hugo/page"))
       (cort--file-contents "content/blog/page.md"))
     "\
---
title: \"xxx\"
---

### 1. First
xxxx
")))

(cort-deftest--org-generate org-generate/include-file
  '(((with-cort--org-generate-buffer (format "\
* project
** general
*** LICENSE
#+INCLUDE: \"%s/mit.txt\" src text
" cort--dir)
       (with-temp-file (expand-file-name "mit.txt" cort--dir)
         (insert "\
MIT License

Copyright (c) 2020 Naoya Yamashita

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the \"Software\"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
"))
       (org-generate "project/general")
       (cort--file-contents "LICENSE"))
     "\
MIT License

Copyright (c) 2020 Naoya Yamashita

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the \"Software\"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
")))

;; (provide 'org-generate-tests)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; org-generate-tests.el ends here
