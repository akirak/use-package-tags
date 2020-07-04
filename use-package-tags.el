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
    (if tags
        `((when (cl-intersection ,tags use-package-tags-current-profile)
            ,@body))
      body)))

;;;; Extract package names from an init file

(cl-defun use-package-tags-select (query &key from installable)
  "Get a list of packages declared in `use-package' forms.

This function returns a list of package names declared in a source
based on tags in QUERY.

The query should be a list of tags.  It selects packages that have
at least one tag in the query or have no tags.  Alternatively, you
can select all packages declared in the source by specifying t as
the query.

By default, the source is the current buffer.
You can specify a file as the source by setting FROM to its file path.

When INSTALLABLE is set to non-nil, it returns a list of packages
available on the Emacs-Mirror.  You will need epkg.el for this feature.

The result is a list of symbols which denote packages."
  (declare (indent 1))
  (let (alist
        (bufs (cl-etypecase from
                (null (list (current-buffer)))
                (file-exists-p (list (or (find-buffer-visiting from)
                                         (find-file-noselect from))))))
        (query (cl-etypecase query
                 (listp query)
                 (symbolp (or (eq t query)
                              (list query))))))
    (cl-labels
        ((get-keyword (prop rest) (-some->> (member prop rest)
                                    (nth 1))))
      (dolist (buf bufs)
        (with-current-buffer buf
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward (rx "(use-package" space) nil t)
              (beginning-of-defun-raw)
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
                        alist)))
              (end-of-defun))))))
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
         (finalize
          (packages)
          (->> (if installable
                   (use-package-tags-map-installable packages)
                 packages)
               (-map #'to-symbol)))
         (to-symbol
          (name)
          (cl-etypecase name
            (symbol name)
            (string (intern name)))))
      (->> (nreverse (-map #'car alist))
           (-filter #'enabled-p)
           (finalize)))))

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

(provide 'use-package-tags)
;;; use-package-tags.el ends here
