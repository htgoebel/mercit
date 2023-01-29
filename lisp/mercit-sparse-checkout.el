;;; mercit-sparse-checkout.el --- Sparse checkout support for Mercit  -*- lexical-binding:t -*-

;; Copyright (C) 2023      The Mercit Project Contributors
;; Copyright (C) 2008-2023 The Magit Project Contributors

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Mercit is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Mercit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Mercit.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides an interface to the `git sparse-checkout'
;; command.  It's been possible to define sparse checkouts since Git
;; v1.7.0 by adding patterns to $GIT_DIR/info/sparse-checkout and
;; calling `git read-tree -mu HEAD' to update the index and working
;; tree.  However, Git v2.25 introduced the `git sparse-checkout'
;; command along with "cone mode", which restricts the possible
;; patterns to directories to provide better performance.
;;
;; The goal of this library is to support the `git sparse-checkout'
;; command operating in cone mode.

;;; Code:

(require 'mercit)

;;; Utilities

(defun mercit-sparse-checkout-enabled-p ()
  "Return non-nil if working tree is a sparse checkout."
  (mercit-get-boolean "core.sparsecheckout"))

(defun mercit-sparse-checkout--assert-version ()
  ;; Older versions of Git have the ability to define sparse checkout
  ;; patterns in .git/info/sparse-checkout, but the sparse-checkout
  ;; command isn't available until 2.25.0.
  (when (mercit-git-version< "2.25.0")
    (user-error "`git sparse-checkout' not available until Git v2.25")))

(defun mercit-sparse-checkout--auto-enable ()
  (if (mercit-sparse-checkout-enabled-p)
      (unless (mercit-get-boolean "core.sparsecheckoutcone")
        (user-error
         "Mercit's sparse checkout functionality requires cone mode"))
    ;; Note: Don't use `mercit-sparse-checkout-enable' because it's
    ;; asynchronous.
    (mercit-run-git "sparse-checkout" "init" "--cone")))

(defun mercit-sparse-checkout-directories ()
  "Return directories that are recursively included in the sparse checkout.
See the `git sparse-checkout' manpage for details about
\"recursive\" versus \"parent\" directories in cone mode."
  (and (mercit-get-boolean "core.sparsecheckoutcone")
       (mapcar #'file-name-as-directory
               (mercit-git-lines "sparse-checkout" "list"))))

;;; Commands

;;;###autoload (autoload 'mercit-sparse-checkout "mercit-sparse-checkout" nil t)
(transient-define-prefix mercit-sparse-checkout ()
  "Create and manage sparse checkouts."
  :man-page "git-sparse-checkout"
  ["Arguments for enabling"
   :if-not mercit-sparse-checkout-enabled-p
   ("-i" "*Use sparse index" "--sparse-index")]
  ["Actions"
   [:if-not mercit-sparse-checkout-enabled-p
    ("e" "*Enable sparse checkout" mercit-sparse-checkout-enable)]
   [:if mercit-sparse-checkout-enabled-p
    ("d" "*Disable sparse checkout" mercit-sparse-checkout-disable)
    ("r" "*Reapply rules" mercit-sparse-checkout-reapply)]
   [("s" "*Set directories" mercit-sparse-checkout-set)
    ("a" "*Add directories" mercit-sparse-checkout-add)]])

;;;###autoload
(defun mercit-sparse-checkout-enable (&optional args)
  "Convert the working tree to a sparse checkout."
  (interactive (list (transient-args 'mercit-sparse-checkout)))
  (mercit-sparse-checkout--assert-version)
  (mercit-run-git-async "sparse-checkout" "init" "--cone" args))

;;;###autoload
(defun mercit-sparse-checkout-set (directories)
  "Restrict working tree to DIRECTORIES.
To extend rather than override the currently configured
directories, call `mercit-sparse-checkout-add' instead."
  (interactive
   (list (mercit-completing-read-multiple*
          "Include these directories: "
          ;; Note: Given that the appeal of sparse checkouts is
          ;; dealing with very large trees, listing all subdirectories
          ;; may need to be reconsidered.
          (mercit-revision-directories "HEAD"))))
  (mercit-sparse-checkout--assert-version)
  (mercit-sparse-checkout--auto-enable)
  (mercit-run-git-async "sparse-checkout" "set" directories))

;;;###autoload
(defun mercit-sparse-checkout-add (directories)
  "Add DIRECTORIES to the working tree.
To override rather than extend the currently configured
directories, call `mercit-sparse-checkout-set' instead."
  (interactive
   (list (mercit-completing-read-multiple*
          "Add these directories: "
          ;; Same performance note as in `mercit-sparse-checkout-set',
          ;; but even more so given the additional processing.
          (seq-remove
           (let ((re (concat
                      "\\`"
                      (regexp-opt (mercit-sparse-checkout-directories)))))
             (lambda (d) (string-match-p re d)))
           (mercit-revision-directories "HEAD")))))
  (mercit-sparse-checkout--assert-version)
  (mercit-sparse-checkout--auto-enable)
  (mercit-run-git-async "sparse-checkout" "add" directories))

;;;###autoload
(defun mercit-sparse-checkout-reapply ()
  "Reapply the sparse checkout rules to the working tree.
Some operations such as merging or rebasing may need to check out
files that aren't included in the sparse checkout.  Call this
command to reset to the sparse checkout state."
  (interactive)
  (mercit-sparse-checkout--assert-version)
  (mercit-run-git-async "sparse-checkout" "reapply"))

;;;###autoload
(defun mercit-sparse-checkout-disable ()
  "Convert sparse checkout to full checkout.
Note that disabling the sparse checkout does not clear the
configured directories.  Call `mercit-sparse-checkout-enable' to
restore the previous sparse checkout."
  (interactive)
  (mercit-sparse-checkout--assert-version)
  (mercit-run-git-async "sparse-checkout" "disable"))

;;; Miscellaneous

(defun mercit-sparse-checkout-insert-header ()
  "Insert header line with sparse checkout information.
This header is not inserted by default.  To enable it, add it to
`mercit-status-headers-hook'."
  (when (mercit-sparse-checkout-enabled-p)
    (insert (propertize (format "%-10s" "Sparse! ")
                        'font-lock-face 'mercit-section-heading))
    (insert
     (let ((dirs (mercit-sparse-checkout-directories)))
       (pcase (length dirs)
         (0 "top-level directory")
         (1 (car dirs))
         (n (format "%d directories" n)))))
    (insert ?\n)))

;;; _
(provide 'mercit-sparse-checkout)
;;; mercit-sparse-checkout.el ends here
