;;; mercit-reset.el --- Reset functionality  -*- lexical-binding:t -*-

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

;; This library implements reset commands.

;;; Code:

(require 'mercit)

;;; Commands

;;;###autoload (autoload 'mercit-reset "mercit" nil t)
(transient-define-prefix mercit-reset ()
  "Reset the `HEAD', index and/or worktree to a previous state."
  :man-page "git-reset"
  ["Reset"
   ("m" "*mixed    (HEAD and index)"        mercit-reset-mixed)
   ("s" "*soft     (HEAD only)"             mercit-reset-soft)
   ("h" "*hard     (HEAD, index and files)" mercit-reset-hard)
   ("k" "*keep     (HEAD and index, keeping uncommitted)" mercit-reset-keep)
   ("i" "*index    (only)"                  mercit-reset-index)
   ("w" "*worktree (only)"                  mercit-reset-worktree)
   ""
   ("f" "*a file"                           mercit-file-checkout)])

;;;###autoload
(defun mercit-reset-mixed (commit)
  "Reset the `HEAD' and index to COMMIT, but not the working tree.
\n(git reset --mixed COMMIT)"
  (interactive (list (mercit-reset-read-branch-or-commit "Reset %s to")))
  (mercit-reset-internal "--mixed" commit))

;;;###autoload
(defun mercit-reset-soft (commit)
  "Reset the `HEAD' to COMMIT, but not the index and working tree.
\n(git reset --soft REVISION)"
  (interactive (list (mercit-reset-read-branch-or-commit "Soft reset %s to")))
  (mercit-reset-internal "--soft" commit))

;;;###autoload
(defun mercit-reset-hard (commit)
  "Reset the `HEAD', index, and working tree to COMMIT.
\n(git reset --hard REVISION)"
  (interactive (list (mercit-reset-read-branch-or-commit
                      (concat (mercit--propertize-face "Hard" 'bold)
                              " reset %s to"))))
  (mercit-reset-internal "--hard" commit))

;;;###autoload
(defun mercit-reset-keep (commit)
  "Reset the `HEAD' and index to COMMIT, while keeping uncommitted changes.
\n(git reset --keep REVISION)"
  (interactive (list (mercit-reset-read-branch-or-commit "Reset %s to")))
  (mercit-reset-internal "--keep" commit))

;;;###autoload
(defun mercit-reset-index (commit)
  "Reset the index to COMMIT.
Keep the `HEAD' and working tree as-is, so if COMMIT refers to the
head this effectively unstages all changes.
\n(git reset COMMIT .)"
  (interactive (list (mercit-read-branch-or-commit "Reset index to")))
  (mercit-reset-internal nil commit "."))

;;;###autoload
(defun mercit-reset-worktree (commit)
  "Reset the worktree to COMMIT.
Keep the `HEAD' and index as-is."
  (interactive (list (mercit-read-branch-or-commit "Reset worktree to")))
  (mercit-wip-commit-before-change nil " before reset")
  (mercit-with-temp-index commit nil
    (mercit-call-git "checkout-index" "--all" "--force"))
  (mercit-wip-commit-after-apply nil " after reset")
  (mercit-refresh))

;;;###autoload
(defun mercit-reset-quickly (commit &optional hard)
  "Reset the `HEAD' and index to COMMIT, and possibly the working tree.
With a prefix argument reset the working tree otherwise don't.
\n(git reset --mixed|--hard COMMIT)"
  (interactive (list (mercit-reset-read-branch-or-commit
                      (if current-prefix-arg
                          (concat (mercit--propertize-face "Hard" 'bold)
                                  " reset %s to")
                        "Reset %s to"))
                     current-prefix-arg))
  (mercit-reset-internal (if hard "--hard" "--mixed") commit))

(defun mercit-reset-read-branch-or-commit (prompt)
  "Prompt for and return a ref to reset HEAD to.

PROMPT is a format string, where either the current branch name
or \"detached head\" will be substituted for %s."
  (mercit-read-branch-or-commit
   (format prompt (or (mercit-get-current-branch) "detached head"))))

(defun mercit-reset-internal (arg commit &optional path)
  (when (and (not (member arg '("--hard" nil)))
             (equal (mercit-rev-parse commit)
                    (mercit-rev-parse "HEAD~")))
    (with-temp-buffer
      (mercit-git-insert "show" "-s" "--format=%B" "HEAD")
      (when git-commit-major-mode
        (funcall git-commit-major-mode))
      (git-commit-setup-font-lock)
      (git-commit-save-message)))
  (let ((cmd (if (and (equal commit "HEAD") (not arg)) "unstage" "reset")))
    (mercit-wip-commit-before-change nil (concat " before " cmd))
    (mercit-run-git "reset" arg commit "--" path)
    (when (equal cmd "unstage")
      (mercit-wip-commit-after-apply nil " after unstage"))))

;;; _
(provide 'mercit-reset)
;;; mercit-reset.el ends here
