;;; mercit-worktree.el --- Worktree support  -*- lexical-binding:t -*-

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

;; This library implements support for `git-worktree'.

;;; Code:

(require 'mercit)

;;; Options

(defcustom mercit-worktree-read-directory-name-function #'read-directory-name
  "Function used to read a directory for worktree commands.
This is called with one argument, the prompt, and can be used
to e.g. use a base directory other than `default-directory'.
Used by `mercit-worktree-checkout' and `mercit-worktree-branch'."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-commands
  :type 'function)

;;; Commands

;;;###autoload (autoload 'mercit-worktree "mercit-worktree" nil t)
(transient-define-prefix mercit-worktree ()
  "Act on a worktree."
  :man-page "git-worktree"
  [["Create new"
    ("b" "*worktree"              mercit-worktree-checkout)
    ("c" "*branch and worktree"   mercit-worktree-branch)]
   ["Commands"
    ("m" "*Move worktree"         mercit-worktree-move)
    ("k" "*Delete worktree"       mercit-worktree-delete)
    ("g" "*Visit worktree"        mercit-worktree-status)]])

;;;###autoload
(defun mercit-worktree-checkout (path branch)
  "Checkout BRANCH in a new worktree at PATH."
  (interactive
   (let ((branch (mercit-read-branch-or-commit "Checkout")))
     (list (funcall mercit-worktree-read-directory-name-function
                    (format "Checkout %s in new worktree: " branch))
           branch)))
  (mercit-run-git "worktree" "add" (mercit--expand-worktree path) branch)
  (mercit-diff-visit-directory path))

;;;###autoload
(defun mercit-worktree-branch (path branch start-point &optional force)
  "Create a new BRANCH and check it out in a new worktree at PATH."
  (interactive
   `(,(funcall mercit-worktree-read-directory-name-function
               "Create worktree: ")
     ,@(mercit-branch-read-args "Create and checkout branch")
     ,current-prefix-arg))
  (mercit-run-git "worktree" "add" (if force "-B" "-b")
                 branch (mercit--expand-worktree path) start-point)
  (mercit-diff-visit-directory path))

;;;###autoload
(defun mercit-worktree-move (worktree path)
  "Move WORKTREE to PATH."
  (interactive
   (list (mercit-completing-read "Move worktree"
                                (cdr (mercit-list-worktrees))
                                nil t nil nil
                                (mercit-section-value-if 'worktree))
         (funcall mercit-worktree-read-directory-name-function
                  "Move worktree to: ")))
  (if (file-directory-p (expand-file-name ".git" worktree))
      (user-error "You may not move the main working tree")
    (let ((preexisting-directory (file-directory-p path)))
      (when (and (zerop (mercit-call-git "worktree" "move" worktree
                                        (mercit--expand-worktree path)))
                 (not (file-exists-p default-directory))
                 (derived-mode-p 'mercit-status-mode))
        (kill-buffer)
        (mercit-diff-visit-directory
         (if preexisting-directory
             (concat (file-name-as-directory path)
                     (file-name-nondirectory worktree))
           path)))
      (mercit-refresh))))

(defun mercit-worktree-delete (worktree)
  "Delete a worktree, defaulting to the worktree at point.
The primary worktree cannot be deleted."
  (interactive
   (list (mercit-completing-read "Delete worktree"
                                (cdr (mercit-list-worktrees))
                                nil t nil nil
                                (mercit-section-value-if 'worktree))))
  (if (file-directory-p (expand-file-name ".git" worktree))
      (user-error "Deleting %s would delete the shared .git directory" worktree)
    (let ((primary (file-name-as-directory (caar (mercit-list-worktrees)))))
      (mercit-confirm-files (if mercit-delete-by-moving-to-trash 'trash 'delete)
                           (list "worktree"))
      (when (file-exists-p worktree)
        (let ((delete-by-moving-to-trash mercit-delete-by-moving-to-trash))
          (delete-directory worktree t mercit-delete-by-moving-to-trash)))
      (if (file-exists-p default-directory)
          (mercit-run-git "worktree" "prune")
        (let ((default-directory primary))
          (mercit-run-git "worktree" "prune"))
        (when (derived-mode-p 'mercit-status-mode)
          (kill-buffer)
          (mercit-status-setup-buffer primary))))))

(defun mercit-worktree-status (worktree)
  "Show the status for the worktree at point.
If there is no worktree at point, then read one in the
minibuffer.  If the worktree at point is the one whose
status is already being displayed in the current buffer,
then show it in Dired instead."
  (interactive
   (list (or (mercit-section-value-if 'worktree)
             (mercit-completing-read
              "Show status for worktree"
              (cl-delete (directory-file-name (mercit-toplevel))
                         (mercit-list-worktrees)
                         :test #'equal :key #'car)))))
  (mercit-diff-visit-directory worktree))

(defun mercit--expand-worktree (path)
  (mercit-convert-filename-for-git (expand-file-name path)))

;;; Sections

(defvar mercit-worktree-section-map
  (let ((map (make-sparse-keymap)))
    (mercit-menu-set map [mercit-visit-thing]  #'mercit-worktree-status "Visit %s")
    (mercit-menu-set map [mercit-delete-thing] #'mercit-worktree-delete "Delete %m")
    (define-key-after map [separator-mercit-worktree] menu-bar-separator)
    (mercit-menu-set map [mercit-worktree ] #'mercit-worktree "Worktree commands...")
    map)
  "Keymap for `worktree' sections.")

(defun mercit-insert-worktrees ()
  "Insert sections for all worktrees.
If there is only one worktree, then insert nothing."
  (let ((worktrees (mercit-list-worktrees)))
    (when (length> worktrees 1)
      (mercit-insert-section (worktrees)
        (mercit-insert-heading "Worktrees:")
        (let* ((cols
                (mapcar
                 (pcase-lambda (`(,path ,barep ,commit ,branch))
                   (cons (cond
                          (branch (propertize
                                   branch 'font-lock-face
                                   (if (equal branch (mercit-get-current-branch))
                                       'mercit-branch-current
                                     'mercit-branch-local)))
                          (commit (propertize (mercit-rev-abbrev commit)
                                              'font-lock-face 'mercit-hash))
                          (barep  "(bare)"))
                         path))
                 worktrees))
               (align (1+ (-max (--map (string-width (car it)) cols)))))
          (pcase-dolist (`(,head . ,path) cols)
            (mercit-insert-section (worktree path)
              (insert head)
              (insert (make-string (- align (length head)) ?\s))
              (insert (let ((r (file-relative-name path))
                            (a (abbreviate-file-name path)))
                        (if (< (string-width r) (string-width a)) r a)))
              (insert ?\n))))
        (insert ?\n)))))

;;; _
(provide 'mercit-worktree)
;;; mercit-worktree.el ends here
