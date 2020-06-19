;;; org-generate.el --- Generate template files/folders from org document  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 0.0.1
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1") (org "9.3") (mustache "0.23") (ht "2.2"))
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

(require 'subr-x)
(require 'org)
(require 'org-element)
(require 'mustache)
(require 'ht)                           ; for mustache utility

(defgroup org-generate nil
  "Generate template files/folders from org document."
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/conao3/org-generate.el"))

(defcustom org-generate-file (locate-user-emacs-file "org-generate.org")
  "File template definition path."
  :group 'org-generate
  :type 'string)

(defvar org-generate-root nil)
(defvar org-generate-mustache-info nil)
(defvar org-generate--file-buffer nil)
(defun org-generate-file-buffer ()
  "Return org-generate file buffer."
  (or org-generate--file-buffer
      (setq org-generate--file-buffer
            (find-file-noselect org-generate-file))))

(defun org-generate-get-heading ()
  "Get `org' heading."
  (with-current-buffer (org-generate-file-buffer)
    (letrec ((fn (lambda (elm)
                   (mapcar
                    (lambda (elm)
                      (when (eq (car elm) 'headline)
                        (cons
                         (nth 1 elm)
                         (funcall fn (cddr elm)))))
                    elm))))
      (funcall
       fn
       (org-element-contents
        (org-element-parse-buffer 'headline))))))

(defun org-generate-candidate ()
  "Get `org' candidate heading for `current-buffer'."
  (let (res)
    (dolist (h1 (org-generate-get-heading))
      (dolist (h2 (cdr h1))
        (push (format "%s/%s"
                      (plist-get (car h1) :raw-value)
                      (plist-get (car h2) :raw-value))
              res)))
    (nreverse res)))

(defun org-generate-search-heading (queue)
  "Search QUEUE from `org-generate-get-heading'."
  (let* ((fn (lambda (h seq)
               (cl-find-if
                (lambda (elm)
                  (cl-find-if
                   (lambda (e)
                     (string= h (plist-get e :raw-value)))
                   elm))
                seq)))
         (lst (split-string queue "/"))
         (h1 (nth 0 lst))
         (h2 (nth 1 lst)))
    (when-let* ((tmp (funcall fn h1 (org-generate-get-heading)))
                (tmp (funcall fn h2 tmp)))
      tmp)))

;;;###autoload
(defun org-generate-edit ()
  "Open `org-generate-file'."
  (interactive)
  (find-file org-generate-file))

(defun org-generate-1 (root heading)
  "Generate file from HEADING.
If ROOT is non-nil, omit some conditions."
  (if root
      (dolist (elm heading)
        (org-generate-1 nil elm))
    (when-let* ((heading* (car-safe heading))
                (title (plist-get heading* :title))
                (title* (mustache-render title org-generate-mustache-info)))
      (when (and (not (string-suffix-p "/" title*)) (cdr heading))
        (error "Heading %s is not suffixed \"/\", but it have childlen" title*))
      (if (string-suffix-p "/" title*)
          (mkdir (expand-file-name title* default-directory) 'parent)
        (let ((src
               (save-restriction
                 (narrow-to-region
                  (plist-get heading* :begin) (plist-get heading* :end))
                 (goto-char (point-min))
                 (let ((case-fold-search t))
                   (when (search-forward "#+begin_src" nil 'noerror)
                     (goto-char (match-beginning 0))))
                 (org-element-src-block-parser (point-max) nil))))
          (unless src
            (error "Node %s has no src block" title*))
          (let* ((file (expand-file-name title* default-directory))
                 (srcbody (org-remove-indentation (plist-get (cadr src) :value)))
                 (srcbody* (mustache-render srcbody org-generate-mustache-info)))
            (with-temp-file file
              (insert srcbody*))
            (message (format "[org-generate] Saved: %s" file)))))
      (dolist (elm (cdr heading))
        (let ((default-directory
                (expand-file-name title* default-directory)))
          (org-generate-1 nil elm))))))

;;;###autoload
(defun org-generate (target)
  "Gerenate files from org document using TARGET definition."
  (interactive (list
                (completing-read
                 "Generate: "
                 (org-generate-candidate) nil 'match)))
  (with-current-buffer (org-generate-file-buffer)
    (let ((heading (org-generate-search-heading target)))
      (when (not heading)
        (error "%s is not defined at %s" target org-generate-file))
      (let* ((beg (plist-get (car heading) :begin))
             (root (or org-generate-root
                       (org-entry-get beg "org-generate-root")
                       (read-file-name "Generate root: ")))
             (vars (org-entry-get-multivalued-property beg "org-generate-variable")))
        (when (not (file-directory-p root))
          (error "%s is not directory" root))
        (let ((default-directory root)
              (org-generate-mustache-info
               (or org-generate-mustache-info
                   (ht<-alist
                    (mapcar (lambda (elm)
                              (cons elm (read-string (format "%s: " elm))))
                            vars)))))
          (org-generate-1 t heading))
        (when (called-interactively-p 'interactive)
          (dired root))))))

(provide 'org-generate)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; org-generate.el ends here
