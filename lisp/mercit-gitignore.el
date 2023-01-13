;;; mercit-gitignore.el --- Intentionally untracked files  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2023 The Magit Project Contributors

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Magit is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements gitignore commands.

;;; Code:

(require 'mercit)

;;; Transient

;;;###autoload (autoload 'mercit-gitignore "mercit-gitignore" nil t)
(transient-define-prefix mercit-gitignore ()
  "Instruct Git to ignore a file or pattern."
  :man-page "gitignore"
  ["Gitignore"
   ("t" "shared at toplevel (.gitignore)"
    mercit-gitignore-in-topdir)
   ("s" "shared in subdirectory (path/to/.gitignore)"
    mercit-gitignore-in-subdir)
   ("p" "privately (.git/info/exclude)"
    mercit-gitignore-in-gitdir)
   ("g" mercit-gitignore-on-system
    :if (lambda () (mercit-get "core.excludesfile"))
    :description (lambda ()
                   (format "privately for all repositories (%s)"
                           (mercit-get "core.excludesfile"))))]
  ["Skip worktree"
   (7 "w" "do skip worktree"     mercit-skip-worktree)
   (7 "W" "do not skip worktree" mercit-no-skip-worktree)]
  ["Assume unchanged"
   (7 "u" "do assume unchanged"     mercit-assume-unchanged)
   (7 "U" "do not assume unchanged" mercit-no-assume-unchanged)])

;;; Gitignore Commands

;;;###autoload
(defun mercit-gitignore-in-topdir (rule)
  "Add the Git ignore RULE to the top-level \".gitignore\" file.
Since this file is tracked, it is shared with other clones of the
repository.  Also stage the file."
  (interactive (list (mercit-gitignore-read-pattern)))
  (mercit-with-toplevel
    (mercit--gitignore rule ".gitignore")
    (mercit-run-git "add" ".gitignore")))

;;;###autoload
(defun mercit-gitignore-in-subdir (rule directory)
  "Add the Git ignore RULE to a \".gitignore\" file in DIRECTORY.
Prompt the user for a directory and add the rule to the
\".gitignore\" file in that directory.  Since such files are
tracked, they are shared with other clones of the repository.
Also stage the file."
  (interactive (list (mercit-gitignore-read-pattern)
                     (read-directory-name "Limit rule to files in: ")))
  (mercit-with-toplevel
    (let ((file (expand-file-name ".gitignore" directory)))
      (mercit--gitignore rule file)
      (mercit-run-git "add" (mercit-convert-filename-for-git file)))))

;;;###autoload
(defun mercit-gitignore-in-gitdir (rule)
  "Add the Git ignore RULE to \"$GIT_DIR/info/exclude\".
Rules in that file only affects this clone of the repository."
  (interactive (list (mercit-gitignore-read-pattern)))
  (mercit--gitignore rule (mercit-git-dir "info/exclude"))
  (mercit-refresh))

;;;###autoload
(defun mercit-gitignore-on-system (rule)
  "Add the Git ignore RULE to the file specified by `core.excludesFile'.
Rules that are defined in that file affect all local repositories."
  (interactive (list (mercit-gitignore-read-pattern)))
  (mercit--gitignore rule
                    (or (mercit-get "core.excludesFile")
                        (error "Variable `core.excludesFile' isn't set")))
  (mercit-refresh))

(defun mercit--gitignore (rule file)
  (when-let ((directory (file-name-directory file)))
    (make-directory directory t))
  (with-temp-buffer
    (when (file-exists-p file)
      (insert-file-contents file))
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))
    (insert (replace-regexp-in-string "\\(\\\\*\\)" "\\1\\1" rule))
    (insert "\n")
    (write-region nil nil file)))

(defun mercit-gitignore-read-pattern ()
  (let* ((default (mercit-current-file))
         (base (car mercit-buffer-diff-files))
         (base (and base (file-directory-p base) base))
         (choices
          (delete-dups
           (--mapcat
            (cons (concat "/" it)
                  (and-let* ((ext (file-name-extension it)))
                    (list (concat "/" (file-name-directory it) "*." ext)
                          (concat "*." ext))))
            (sort (nconc
                   (mercit-untracked-files nil base)
                   ;; The untracked section of the status buffer lists
                   ;; directories containing only untracked files.
                   ;; Add those as candidates.
                   (-filter #'directory-name-p
                            (mercit-list-files
                             "--other" "--exclude-standard" "--directory"
                             "--no-empty-directory" "--" base)))
                  #'string-lessp)))))
    (when default
      (setq default (concat "/" default))
      (unless (member default choices)
        (setq default (concat "*." (file-name-extension default)))
        (unless (member default choices)
          (setq default nil))))
    (mercit-completing-read "File or pattern to ignore"
                           choices nil nil nil nil default)))

;;; Skip Worktree Commands

;;;###autoload
(defun mercit-skip-worktree (file)
  "Call \"git update-index --skip-worktree -- FILE\"."
  (interactive
   (list (mercit-read-file-choice "Skip worktree for"
                                 (mercit-with-toplevel
                                   (cl-set-difference
                                    (mercit-list-files)
                                    (mercit-skip-worktree-files)
                                    :test #'equal)))))
  (mercit-with-toplevel
    (mercit-run-git "update-index" "--skip-worktree" "--" file)))

;;;###autoload
(defun mercit-no-skip-worktree (file)
  "Call \"git update-index --no-skip-worktree -- FILE\"."
  (interactive
   (list (mercit-read-file-choice "Do not skip worktree for"
                                 (mercit-with-toplevel
                                   (mercit-skip-worktree-files)))))
  (mercit-with-toplevel
    (mercit-run-git "update-index" "--no-skip-worktree" "--" file)))

;;; Assume Unchanged Commands

;;;###autoload
(defun mercit-assume-unchanged (file)
  "Call \"git update-index --assume-unchanged -- FILE\"."
  (interactive
   (list (mercit-read-file-choice "Assume file to be unchanged"
                                 (mercit-with-toplevel
                                   (cl-set-difference
                                    (mercit-list-files)
                                    (mercit-assume-unchanged-files)
                                    :test #'equal)))))
  (mercit-with-toplevel
    (mercit-run-git "update-index" "--assume-unchanged" "--" file)))

;;;###autoload
(defun mercit-no-assume-unchanged (file)
  "Call \"git update-index --no-assume-unchanged -- FILE\"."
  (interactive
   (list (mercit-read-file-choice "Do not assume file to be unchanged"
                                 (mercit-with-toplevel
                                   (mercit-assume-unchanged-files)))))
  (mercit-with-toplevel
    (mercit-run-git "update-index" "--no-assume-unchanged" "--" file)))

;;; _
(provide 'mercit-gitignore)
;;; mercit-gitignore.el ends here
