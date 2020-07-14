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
(require 'org-generate)

(setq cort--dir
      (expand-file-name
       (format "org-generate-%04d" (random (round 1e4)))
       temporary-file-directory))

(defun cort--file-contents (path)
  "Get all contents of file located at PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defmacro with-cort--org-generate-buffer (contents &rest body)
  "Exec BODY in temp buffer that has CONTENTS."
  (declare (indent 1))
  `(let ((org-generate-root cort--dir)
         (org-generate--file-buffer (get-buffer-create "*temp*")))
     (with-current-buffer org-generate--file-buffer
       (insert ,contents)
       (goto-char (point-min))
       ,@body)))


;;; Test definition

(cort-deftest org-generate/simple
  (cort-generate :equal
    '(((+ 2 3) 5))))

(cort-deftest org-generate/onefile
  (cort-generate-with-hook :equal
    (lambda () (mkdir cort--dir))
    (lambda () (ignore-errors (delete-directory cort--dir 'force)))
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
         (cort--file-contents
          (expand-file-name "page" cort--dir)))
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
"))))

;; (provide 'org-generate-tests)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; org-generate-tests.el ends here
