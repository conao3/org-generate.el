;;; org-generate.el --- Generate template files/folders from org document  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 0.0.1
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1") (org "9.3"))
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

;; Generate template files/folders from org document.


;;; Code:

(require 'org)
(require 'org-element)

(defgroup org-generate nil
  "Generate template files/folders from org document."
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/conao3/org-generate.el"))

(defcustom org-generate-file (expand-file-name "org-generate.org" org-directory)
  "File template definition path."
  :group 'org-generate
  :type 'string)

(defvar org-generate--file-buffer nil)
(defun org-generate-file-buffer ()
  "Return org-generate file buffer."
  (or org-generate--file-buffer
      (setq org-generate--file-buffer
            (find-file-noselect org-generate-file))))

(defun org-generate-candidate ()
  "Get `org' candidate heading for `current-buffer'."
  (with-current-buffer (org-generate-file-buffer)
    (letrec ((fn (lambda (elm)
                   (mapcar
                    (lambda (elm)
                      (when (eq (car elm) 'headline)
                        (cons
                         (plist-get (nth 1 elm) :raw-value)
                         (funcall fn (cddr elm)))))
                    elm))))
      (let ((heading (funcall
                      fn
                      (org-element-contents
                       (org-element-parse-buffer 'headline))))
            res)
        (dolist (h1 heading)
          (dolist (h2 (cdr h1))
            (push (format "%s/%s" (car h1) (car h2)) res)))
        (nreverse res)))))

(defun org-generate (target)
  "Gerenate files from org document using TARGET definition."
  (interactive (list
                (completing-read
                 "Generate: "
                 (org-generate-candidate) nil 'match)))
  target)

(provide 'org-generate)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; org-generate.el ends here
