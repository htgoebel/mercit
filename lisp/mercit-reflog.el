;;; mercit-reflog.el --- Inspect ref history  -*- lexical-binding:t -*-

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

;; This library implements support for looking at Git reflogs.

;;; Code:

(require 'mercit-core)
(require 'mercit-log)

;;; Options

(defcustom mercit-reflog-limit 256
  "Maximal number of entries initially shown in reflog buffers.
The limit in the current buffer can be changed using \"+\"
and \"-\"."
  :package-version '(mercit . "3.0.0")
  :group 'mercit-commands
  :type 'number)

(defcustom mercit-reflog-margin
  (list (nth 0 mercit-log-margin)
        (nth 1 mercit-log-margin)
        'mercit-log-margin-width nil
        (nth 4 mercit-log-margin))
  "Format of the margin in `mercit-reflog-mode' buffers.

The value has the form (INIT STYLE WIDTH AUTHOR AUTHOR-WIDTH).

If INIT is non-nil, then the margin is shown initially.
STYLE controls how to format the author or committer date.
  It can be one of `age' (to show the age of the commit),
  `age-abbreviated' (to abbreviate the time unit to a character),
  or a string (suitable for `format-time-string') to show the
  actual date.  Option `mercit-log-margin-show-committer-date'
  controls which date is being displayed.
WIDTH controls the width of the margin.  This exists for forward
  compatibility and currently the value should not be changed.
AUTHOR controls whether the name of the author is also shown by
  default.
AUTHOR-WIDTH has to be an integer.  When the name of the author
  is shown, then this specifies how much space is used to do so."
  :package-version '(mercit . "2.9.0")
  :group 'mercit-log
  :group 'mercit-margin
  :type mercit-log-margin--custom-type
  :initialize #'mercit-custom-initialize-reset
  :set-after '(mercit-log-margin)
  :set (apply-partially #'mercit-margin-set-variable 'mercit-reflog-mode))

;;; Faces

(defface mercit-reflog-commit '((t :foreground "green"))
  "Face for commit commands in reflogs."
  :group 'mercit-faces)

(defface mercit-reflog-amend '((t :foreground "magenta"))
  "Face for amend commands in reflogs."
  :group 'mercit-faces)

(defface mercit-reflog-merge '((t :foreground "green"))
  "Face for merge, checkout and branch commands in reflogs."
  :group 'mercit-faces)

(defface mercit-reflog-checkout '((t :foreground "blue"))
  "Face for checkout commands in reflogs."
  :group 'mercit-faces)

(defface mercit-reflog-reset '((t :foreground "red"))
  "Face for reset commands in reflogs."
  :group 'mercit-faces)

(defface mercit-reflog-rebase '((t :foreground "magenta"))
  "Face for rebase commands in reflogs."
  :group 'mercit-faces)

(defface mercit-reflog-cherry-pick '((t :foreground "green"))
  "Face for cherry-pick commands in reflogs."
  :group 'mercit-faces)

(defface mercit-reflog-remote '((t :foreground "cyan"))
  "Face for pull and clone commands in reflogs."
  :group 'mercit-faces)

(defface mercit-reflog-other '((t :foreground "cyan"))
  "Face for other commands in reflogs."
  :group 'mercit-faces)

;;; Commands

;;;###autoload
(defun mercit-reflog-current ()
  "Display the reflog of the current branch.
If `HEAD' is detached, then show the reflog for that instead."
  (interactive)
  (mercit-reflog-setup-buffer (or (mercit-get-current-branch) "HEAD")))

;;;###autoload
(defun mercit-reflog-other (ref)
  "Display the reflog of a branch or another ref."
  (interactive (list (mercit-read-local-branch-or-ref "Show reflog for")))
  (mercit-reflog-setup-buffer ref))

;;;###autoload
(defun mercit-reflog-head ()
  "Display the `HEAD' reflog."
  (interactive)
  (mercit-reflog-setup-buffer "HEAD"))

;;; Mode

(defvar mercit-reflog-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mercit-log-mode-map)
    (define-key map (kbd "C-c C-n") #'undefined)
    (define-key map (kbd "L")       #'mercit-margin-settings)
    map)
  "Keymap for `mercit-reflog-mode'.")

(define-derived-mode mercit-reflog-mode mercit-mode "Mercit Reflog"
  "Mode for looking at Git reflog.

This mode is documented in info node `(mercit)Reflog'.

\\<mercit-mode-map>\
Type \\[mercit-refresh] to refresh the current buffer.
Type \\[mercit-visit-thing] or \\[mercit-diff-show-or-scroll-up] \
to visit the commit at point.

Type \\[mercit-cherry-pick] to apply the commit at point.
Type \\[mercit-reset] to reset `HEAD' to the commit at point.

\\{mercit-reflog-mode-map}"
  :group 'mercit-log
  (hack-dir-local-variables-non-file-buffer)
  (setq mercit--imenu-item-types 'commit))

(defun mercit-reflog-setup-buffer (ref)
  (require 'mercit)
  (mercit-setup-buffer #'mercit-reflog-mode nil
    (mercit-buffer-refname ref)
    (mercit-buffer-log-args (list (format "-n%s" mercit-reflog-limit)))))

(defun mercit-reflog-refresh-buffer ()
  (mercit-set-header-line-format (concat "Reflog for " mercit-buffer-refname))
  (mercit-insert-section (reflogbuf)
    (mercit-git-wash (apply-partially #'mercit-log-wash-log 'reflog)
      "reflog" "show" "--format=%h%x00%aN%x00%gd%x00%gs" "--date=raw"
      mercit-buffer-log-args mercit-buffer-refname "--")))

(cl-defmethod mercit-buffer-value (&context (major-mode mercit-reflog-mode))
  mercit-buffer-refname)

(defvar mercit-reflog-labels
  '(("commit"      . mercit-reflog-commit)
    ("amend"       . mercit-reflog-amend)
    ("merge"       . mercit-reflog-merge)
    ("checkout"    . mercit-reflog-checkout)
    ("branch"      . mercit-reflog-checkout)
    ("reset"       . mercit-reflog-reset)
    ("rebase"      . mercit-reflog-rebase)
    ("rewritten"   . mercit-reflog-rebase)
    ("cherry-pick" . mercit-reflog-cherry-pick)
    ("initial"     . mercit-reflog-commit)
    ("pull"        . mercit-reflog-remote)
    ("clone"       . mercit-reflog-remote)
    ("autosave"    . mercit-reflog-commit)
    ("restart"     . mercit-reflog-reset)))

(defun mercit-reflog-format-subject (subject)
  (let* ((match (string-match mercit-reflog-subject-re subject))
         (command (and match (match-string 1 subject)))
         (option  (and match (match-string 2 subject)))
         (type    (and match (match-string 3 subject)))
         (label (if (string= command "commit")
                    (or type command)
                  command))
         (text (if (string= command "commit")
                   label
                 (mapconcat #'identity
                            (delq nil (list command option type))
                            " "))))
    (format "%-16s "
            (mercit--propertize-face
             text (or (cdr (assoc label mercit-reflog-labels))
                      'mercit-reflog-other)))))

;;; _
(provide 'mercit-reflog)
;;; mercit-reflog.el ends here
