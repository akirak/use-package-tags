;;; use-package-tags.el --- Group packages using tags -*- lexical-binding: t -*-

;; Copyright (C) 2020 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (dash "2.12"))
;; Keywords: lisp maint tools
;; URL: https://github.com/akirak/use-package-tags

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library adds :tags keyword to use-package, which can be used
;; to enable only packages that belong to certain groups.
;; You can group packages using the keyword, and select packages
;; to enable by setting `use-package-tags-current-profile' variable.
;;
;; It also provides a function for enumerating packages in a
;; file/buffer based on tags.  This can be used to install packages
;; for your init file without relying on package.el.

;;; Code:

(require 'cl-lib)
(require 'dash)

(defgroup use-package-tags
  nil
  "Group packages using tags."
  :group 'use-package)

(declare-function epkg-provided-by "ext:epkg")
(declare-function epkg-builtin-package-p "ext:epkg")
(declare-function epkg "ext:epkg")
(declare-function use-package-process-keywords "ext:use-package")

(defvar use-package-keywords)

;;;; use-package :tags keyword

(defcustom use-package-tags-current-profile nil
  "List of tags used to select packages to activate."
  :type '(repeat symbol))

(when (require 'use-package nil t)
  (fset 'use-package-handler/:tags 'use-package-tags-handler)
  (fset 'use-package-normalize/:tags 'use-package-normalize-test)
  (fset 'use-package-autoloads/:tags 'use-package-autoloads-mode)

  (add-to-list 'use-package-keywords :tags))

(defun use-package-tags-handler (name _keyword tags rest state)
  "Handler for :tags keyword in use-package.

NAME, _KEYWORD, TAGS, REST, and STATE are arguments that follow
the conventions of use-package."
  (let ((body (use-package-process-keywords name rest state)))
    (if (and load-in-progress tags)
        `((when (cl-intersection ,tags use-package-tags-current-profile)
            ,@body))
      body)))

;;;; Extract package names from an init file

(defcustom use-package-tags-init-files
  (list (or user-init-file
            (expand-file-name "init.el" user-emacs-directory)))
  "List of files to look for package declarations."
  :type '(repeat file))

(defun use-package-tags--source-buffer-list (source)
  "Return a list of buffers for SOURCE.

If the argument is nil, it returns a list containing the current
buffer.

Alternatively, the argument can be one of the following:

 * t for `use-package-tags-init-files'.
 * File name.
 * Buffer.
 * List of files.
 * List of buffers."
  (cl-labels
      ((to-buffer
        (item)
        (cl-etypecase item
          (buffer item)
          ;; (symbol (to-buffer (eval symbol)))
          (file-exists (or (find-buffer-visiting item)
                           (find-file-noselect item)))
          (string (error "File does not exist: %s" item)))))
    (cl-typecase source
      (null (list (current-buffer)))
      (symbol (if (eq t source)
                  (mapcar #'to-buffer use-package-tags-init-files)
                ;; TODO: Return the variable value when a symbol is given
                ;; (list (to-buffer source))
                (error "Symbol is not accepted: %s" source)))
      (list (mapcar #'to-buffer source))
      (otherwise (list (to-buffer source))))))

(defsubst use-package-tags--normalize-query (query)
  "Normalize QUERY into a list or t."
  (cl-etypecase query
    (listp query)
    (symbolp (or (eq t query)
                 (list query)))))

(defmacro use-package-tags--with-package-forms (buffers &rest progn)
  "In BUFFERS, evaluate PROGN at every `use-package' form."
  (declare (indent 1))
  `(dolist (buf ,buffers)
     (with-current-buffer buf
       (save-excursion
         (goto-char (point-min))
         (while (re-search-forward (rx "(use-package" space) nil t)
           (beginning-of-defun-raw)
           ,@progn
           (end-of-defun))))))

;;;###autoload
(cl-defun use-package-tags-select (query &key from installable
                                         (as 'symbols))
  "Get a list of packages declared in `use-package' forms.

This function returns a list of package names declared in a source
based on tags in QUERY.

The query should be a list of tags.  It selects packages that have
at least one tag in the query or have no tags.  Alternatively, you
can select all packages declared in the source by specifying t as
the query.

FROM specifies the source.
See `use-package-tags--source-buffer-list'.

When INSTALLABLE is set to non-nil, it returns a list of packages
available on the Emacs-Mirror.  You will need epkg.el for this feature.

When AS is given, it converts the result to a certain type.
It accepts the following values (the default: symbols):

 * symbols: a list of symbols.

 * strings: a list of strings.

 * lines: a single string joined by newlines."
  (declare (indent 1))
  (let (alist
        (query (use-package-tags--normalize-query query)))
    (cl-labels
        ((get-keyword (prop rest) (-some->> (member prop rest)
                                    (nth 1))))
      (use-package-tags--with-package-forms
          (use-package-tags--source-buffer-list from)
        (let* ((exp (read (current-buffer)))
               (name (nth 1 exp))
               (disabled (get-keyword :disabled exp))
               ;; TODO: Handle dependencies
               ;; (after (get-keyword :after))
               ;; (requires (get-keyword :requires))
               ;; TODO: Add support for :when and :unless
               (if-expr (get-keyword :if exp))
               (tags (get-keyword :tags exp)))
          (when (and (not disabled)
                     (not (and if-expr
                               (not (eval if-expr))))
                     ;; TODO: :requires keyword
                     ;; (or (not requires)
                     ;;     (-all-p (lambda (feature)
                     ;;               (assoc feature alist))
                     ;;             (cl-etypecase requires
                     ;;               (list requires)
                     ;;               (symbol (list requires)))))
                     (or (eq t query)
                         (not tags)
                         (cl-intersection tags query)))
            (push (list name)
                  ;; TODO: Handle dependencies
                  ;; (list name
                  ;;       :after after
                  ;;       :requires requires)
                  alist)))))
    (cl-labels
        ((enabled-p
          ;; The dependency handle is work in progress.
          ;; It must also support :requires keyword based on :after,
          ;; but I won't work on it for now.
          ;;
          ;; (package)
          ;; (let* ((cell (assoc package alist))
          ;;        (plist (cdr cell))
          ;;        (after (plist-get plist :after)))
          ;;   (and cell
          ;;        (or (not after)
          ;;            (test-after after))))
          (_package)
          t)
         (test-after
          (selector)
          (pcase selector
            ((pred symbolp) (enabled-p selector))
            (`(:all . ,conds) (-all-p #'test-after conds))
            (`(:any . ,conds) (-any-p #'test-after conds))
            ((pred listp) (-all-p #'test-after selector))))
         (map-result
          (packages)
          (if installable
              (use-package-tags-map-installable packages)
            packages))
         (convert
          (packages)
          (cl-ecase as
            (symbols (-map #'to-symbol packages))
            (strings (-map #'to-string packages))
            (lines (mapconcat #'to-string packages "\n"))))
         (to-string
          (name)
          (cl-etypecase name
            (symbol (symbol-name name))
            (string name)))
         (to-symbol
          (name)
          (cl-etypecase name
            (symbol name)
            (string (intern name)))))
      (->> (nreverse (-map #'car alist))
           (-filter #'enabled-p)
           (map-result)
           (convert)))))

(defun use-package-tags-map-installable (packages)
  "Get a list of packages to be installed to activate PACKAGES."
  (require 'epkg)
  (cl-labels
      ((query (pkg)
              (let* ((name (symbol-name pkg))
                     (obj (epkg name)))
                (cond
                 ((and obj (epkg-builtin-package-p obj))
                  nil)
                 (obj
                  name)
                 (t
                  (epkg-provided-by pkg))))))
    (->> packages
         (-map #'query)
         (delq nil)
         (-uniq))))

;;;###autoload
(cl-defun use-package-tags-collect-tags (source &key sort)
  "Collect package tags from a source.

For SOURCE, see `use-package-tags--source-buffer-list'.

If SORT is non-nil, the result will be lexicographically sorted."
  (cl-labels
      ((get-keyword (prop rest) (-some->> (member prop rest)
                                  (nth 1)))
       (order (items) (if sort
                          (cl-sort items #'string< :key #'symbol-name)
                        items)))
    (let (result)
      (use-package-tags--with-package-forms
          (use-package-tags--source-buffer-list source)
        (let* ((exp (read (current-buffer)))
               (tags (get-keyword :tags exp)))
          (when tags
            (push tags result))))
      (->> (-flatten-n 1 result)
           (cl-remove-duplicates)
           (order)))))

(defun use-package-tags--unloaded-tags ()
  "Return a list of tags that are not in the current profile."
  (-difference (use-package-tags-collect-tags t :sort t)
               use-package-tags-current-profile))

;;;###autoload
(defun use-package-tags-load (tag)
  "Load packages with a TAG.

This function evalates `use-package' forms with the selected tag
in `use-package-tags-init-files'.

After successfully loading all matching packages, the tag will be
added to `use-package-tags-current-profile', without saving the value
to `custom-file'."
  (interactive (list (completing-read "Load packages with tag: "
                                      (use-package-tags--unloaded-tags)
                                      nil 'match)))
  (cl-labels
      ((get-keyword (prop rest) (-some->> (member prop rest)
                                  (nth 1)))
       (has-tag-p (tag rest)
                  (let ((tags (cdr (get-keyword :tags rest))))
                    (memq tag (cl-etypecase tags
                                (list tags)
                                (symbol (list tags)))))))
    (setq tag (cl-etypecase tag
                (tag tag)
                (string (intern tag))))
    (use-package-tags--with-package-forms
     (use-package-tags--source-buffer-list t)
     (let ((exp (read (current-buffer))))
       (when (has-tag-p tag exp)
         (message "Loading package %s configured at %s..."
                  (nth 1 exp)
                  (abbreviate-file-name (buffer-file-name)))
         (eval exp))))
    (push tag use-package-tags-current-profile)))

(provide 'use-package-tags)
;;; use-package-tags.el ends here
