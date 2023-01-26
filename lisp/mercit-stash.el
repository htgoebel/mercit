;;; mercit-stash.el --- Stash support for Mercit  -*- lexical-binding:t -*-

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

;; Support for Git stashes.

;;; Code:

(require 'mercit)
(require 'mercit-reflog)
(require 'mercit-sequence)

;;; Options

(defgroup mercit-stash nil
  "List stashes and show stash diffs."
  :group 'mercit-modes)

;;;; Diff options

(defcustom mercit-stash-sections-hook
  '(mercit-insert-stash-notes
    mercit-insert-stash-worktree
    mercit-insert-stash-index
    mercit-insert-stash-untracked)
  "Hook run to insert sections into stash diff buffers."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-stash
  :type 'hook)

;;;; Log options

(defcustom mercit-stashes-margin
  (list (nth 0 mercit-log-margin)
        (nth 1 mercit-log-margin)
        'mercit-log-margin-width nil
        (nth 4 mercit-log-margin))
  "Format of the margin in `mercit-stashes-mode' buffers.

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
  :package-version '(mercit . "0.0.0")
  :group 'mercit-stash
  :group 'mercit-margin
  :type mercit-log-margin--custom-type
  :initialize #'mercit-custom-initialize-reset
  :set-after '(mercit-log-margin)
  :set (apply-partially #'mercit-margin-set-variable 'mercit-stashes-mode))

;;; Commands

;;;###autoload (autoload 'mercit-stash "mercit-stash" nil t)
(transient-define-prefix mercit-stash ()
  "Stash uncommitted changes."
  :man-page "git-stash"
  ["Arguments"
   ("-u" "Also save untracked files" ("-u" "--include-untracked"))
   ("-a" "Also save untracked and ignored files" ("-a" "--all"))]
  [["Stash"
    ("z" "both"          mercit-stash-both)
    ("i" "index"         mercit-stash-index)
    ("w" "worktree"      mercit-stash-worktree)
    ("x" "keeping index" mercit-stash-keep-index)
    ("P" "push"          mercit-stash-push :level 5)]
   ["Snapshot"
    ("Z" "both"          mercit-snapshot-both)
    ("I" "index"         mercit-snapshot-index)
    ("W" "worktree"      mercit-snapshot-worktree)
    ("r" "to wip ref"    mercit-wip-commit)]
   ["Use"
    ("a" "Apply"         mercit-stash-apply)
    ("p" "Pop"           mercit-stash-pop)
    ("k" "Drop"          mercit-stash-drop)]
   ["Inspect"
    ("l" "List"          mercit-stash-list)
    ("v" "Show"          mercit-stash-show)]
   ["Transform"
    ("b" "Branch"        mercit-stash-branch)
    ("B" "Branch here"   mercit-stash-branch-here)
    ("f" "Format patch"  mercit-stash-format-patch)]])

(defun mercit-stash-arguments ()
  (transient-args 'mercit-stash))

;;;###autoload
(defun mercit-stash-both (message &optional include-untracked)
  "Create a stash of the index and working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'."
  (interactive
   (progn (when (and (mercit-merge-in-progress-p)
                     (not (mercit-y-or-n-p "\
Stashing and resetting during a merge conflict. \
Applying the resulting stash won't restore the merge state. \
Proceed anyway? ")))
            (user-error "Abort"))
          (mercit-stash-read-args)))
  (mercit-stash-save message t t include-untracked t))

;;;###autoload
(defun mercit-stash-index (message)
  "Create a stash of the index only.
Unstaged and untracked changes are not stashed.  The stashed
changes are applied in reverse to both the index and the
worktree.  This command can fail when the worktree is not clean.
Applying the resulting stash has the inverse effect."
  (interactive (list (mercit-stash-read-message)))
  (mercit-stash-save message t nil nil t 'worktree))

;;;###autoload
(defun mercit-stash-worktree (message &optional include-untracked)
  "Create a stash of unstaged changes in the working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'."
  (interactive (mercit-stash-read-args))
  (mercit-stash-save message nil t include-untracked t 'index))

;;;###autoload
(defun mercit-stash-keep-index (message &optional include-untracked)
  "Create a stash of the index and working tree, keeping index intact.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'."
  (interactive (mercit-stash-read-args))
  (mercit-stash-save message t t include-untracked t 'index))

(defun mercit-stash-read-args ()
  (list (mercit-stash-read-message)
        (mercit-stash-read-untracked)))

(defun mercit-stash-read-untracked ()
  (let ((prefix (prefix-numeric-value current-prefix-arg))
        (args   (mercit-stash-arguments)))
    (cond ((or (= prefix 16) (member "--all" args)) 'all)
          ((or (= prefix  4) (member "--include-untracked" args)) t))))

(defun mercit-stash-read-message ()
  (let* ((default (format "On %s: "
                          (or (mercit-get-current-branch) "(no branch)")))
         (input (mercit-read-string "Stash message" default)))
    (if (equal input default)
        (concat default (mercit-rev-format "{node|short} {desc|firstline}"))
      input)))

;;;###autoload
(defun mercit-snapshot-both (&optional include-untracked)
  "Create a snapshot of the index and working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'."
  (interactive (mercit-snapshot-read-args))
  (mercit-snapshot-save t t include-untracked t))

;;;###autoload
(defun mercit-snapshot-index ()
  "Create a snapshot of the index only.
Unstaged and untracked changes are not stashed."
  (interactive)
  (mercit-snapshot-save t nil nil t))

;;;###autoload
(defun mercit-snapshot-worktree (&optional include-untracked)
  "Create a snapshot of unstaged changes in the working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'."
  (interactive (mercit-snapshot-read-args))
  (mercit-snapshot-save nil t include-untracked t))

(defun mercit-snapshot-read-args ()
  (list (mercit-stash-read-untracked)))

(defun mercit-snapshot-save (index worktree untracked &optional refresh)
  (mercit-stash-save (concat "WIP on " (mercit-stash-summary))
                    index worktree untracked refresh t))

;;;###autoload (autoload 'mercit-stash-push "mercit-stash" nil t)
(transient-define-prefix mercit-stash-push (&optional transient args)
  "Create stash using \"git stash push\".

This differs from Mercit's other stashing commands, which don't
use \"git stash\" and are generally more flexible but don't allow
specifying a list of files to be stashed."
  :man-page "git-stash"
  ["Arguments"
   (mercit:-- :reader (lambda (prompt initial-input history)
                       (mercit-read-files prompt initial-input history
                                         #'mercit-modified-files)))
   ("-u" "Also save untracked files" ("-u" "--include-untracked"))
   ("-a" "Also save untracked and ignored files" ("-a" "--all"))
   ("-k" "Keep index" ("-k" "--keep-index"))
   ("-K" "Don't keep index" "--no-keep-index")]
  ["Actions"
   ("P" "push" mercit-stash-push)]
  (interactive (if (eq transient-current-command 'mercit-stash-push)
                   (list nil (transient-args 'mercit-stash-push))
                 (list t)))
  (if transient
      (transient-setup 'mercit-stash-push)
    (mercit-run-git "stash" "push" args)))

;;;###autoload
(defun mercit-stash-apply (stash)
  "Apply a stash to the working tree.
Try to preserve the stash index.  If that fails because there
are staged changes, apply without preserving the stash index."
  (interactive (list (mercit-read-stash "Apply stash")))
  (if (= (mercit-call-git "stash" "apply" "--index" stash) 0)
      (mercit-refresh)
    (mercit-run-git "stash" "apply" stash)))

;;;###autoload
(defun mercit-stash-pop (stash)
  "Apply a stash to the working tree and remove it from stash list.
Try to preserve the stash index.  If that fails because there
are staged changes, apply without preserving the stash index
and forgo removing the stash."
  (interactive (list (mercit-read-stash "Pop stash")))
  (if (= (mercit-call-git "stash" "apply" "--index" stash) 0)
      (mercit-stash-drop stash)
    (mercit-run-git "stash" "apply" stash)))

;;;###autoload
(defun mercit-stash-drop (stash)
  "Remove a stash from the stash list.
When the region is active offer to drop all contained stashes."
  (interactive
   (list (--if-let (mercit-region-values 'stash)
             (mercit-confirm 'drop-stashes nil "Drop %i stashes" nil it)
           (mercit-read-stash "Drop stash"))))
  (dolist (stash (if (listp stash)
                     (nreverse (prog1 stash (setq stash (car stash))))
                   (list stash)))
    (message "Deleted refs/%s (was %s)" stash
             (mercit-rev-parse "--short" stash))
    (mercit-call-git "rev-parse" stash)
    (mercit-call-git "stash" "drop" stash))
  (mercit-refresh))

;;;###autoload
(defun mercit-stash-clear (ref)
  "Remove all stashes saved in REF's reflog by deleting REF."
  (interactive (let ((ref (or (mercit-section-value-if 'stashes) "refs/stash")))
                 (mercit-confirm t (format "Drop all stashes in %s" ref))
                 (list ref)))
  (mercit-run-git "update-ref" "-d" ref))

;;;###autoload
(defun mercit-stash-branch (stash branch)
  "Create and checkout a new BRANCH from STASH."
  (interactive (list (mercit-read-stash "Branch stash")
                     (mercit-read-string-ns "Branch name")))
  (mercit-run-git "stash" "branch" branch stash))

;;;###autoload
(defun mercit-stash-branch-here (stash branch)
  "Create and checkout a new BRANCH and apply STASH.
The branch is created using `mercit-branch-and-checkout', using the
current branch or `HEAD' as the start-point."
  (interactive (list (mercit-read-stash "Branch stash")
                     (mercit-read-string-ns "Branch name")))
  (let ((mercit-inhibit-refresh t))
    (mercit-branch-and-checkout branch (or (mercit-get-current-branch) "HEAD")))
  (mercit-stash-apply stash))

;;;###autoload
(defun mercit-stash-format-patch (stash)
  "Create a patch from STASH"
  (interactive (list (mercit-read-stash "Create patch from stash")))
  (with-temp-file (mercit-rev-format
                   "0001-{sub(' ', '-', desc|firstline|stringify)}.patch" ;; TODO was %f
                   stash)
    (mercit-git-insert "stash" "show" "-p" stash))
  (mercit-refresh))

;;; Plumbing

(defun mercit-stash-save (message index worktree untracked
                                 &optional refresh keep noerror ref)
  (if (or (and index     (mercit-staged-files t))
          (and worktree  (mercit-unstaged-files t))
          (and untracked (mercit-untracked-files (eq untracked 'all))))
      (mercit-with-toplevel
        (mercit-stash-store message (or ref "refs/stash")
                           (mercit-stash-create message index worktree untracked))
        (if (eq keep 'worktree)
            (with-temp-buffer
              (mercit-git-insert "diff" "--cached" "--no-ext-diff")
              (mercit-run-git-with-input
               "apply" "--reverse" "--cached" "--ignore-space-change" "-")
              (mercit-run-git-with-input
               "apply" "--reverse" "--ignore-space-change" "-"))
          (unless (eq keep t)
            (if (eq keep 'index)
                (mercit-call-git "checkout" "--" ".")
              (mercit-call-git "reset" "--hard" "HEAD" "--"))
            (when untracked
              (mercit-call-git "clean" "--force" "-d"
                              (and (eq untracked 'all) "-x")))))
        (when refresh
          (mercit-refresh)))
    (unless noerror
      (user-error "No %s changes to save" (cond ((not index)  "unstaged")
                                                ((not worktree) "staged")
                                                (t "local"))))))

(defun mercit-stash-store (message ref commit)
  (mercit-update-ref ref message commit t))

(defun mercit-stash-create (message index worktree untracked)
  (unless (mercit-rev-parse "--verify" "HEAD")
    (error "You do not have the initial commit yet"))
  (let ((mercit-git-global-arguments (nconc (list "-c" "commit.gpgsign=false")
                                           mercit-git-global-arguments))
        (default-directory (mercit-toplevel))
        (summary (mercit-stash-summary))
        (head "HEAD"))
    (when (and worktree (not index))
      (setq head (or (mercit-commit-tree "pre-stash index" nil "HEAD")
                     (error "Cannot save the current index state"))))
    (or (setq index (mercit-commit-tree (concat "index on " summary) nil head))
        (error "Cannot save the current index state"))
    (and untracked
         (setq untracked (mercit-untracked-files (eq untracked 'all)))
         (setq untracked (mercit-with-temp-index nil nil
                           (or (and (mercit-update-files untracked)
                                    (mercit-commit-tree
                                     (concat "untracked files on " summary)))
                               (error "Cannot save the untracked files")))))
    (mercit-with-temp-index index "-m"
      (when worktree
        (or (mercit-update-files (mercit-git-items "diff" "-z" "--name-only" head))
            (error "Cannot save the current worktree state")))
      (or (mercit-commit-tree message nil head index untracked)
          (error "Cannot save the current worktree state")))))

(defun mercit-stash-summary ()
  (concat (or (mercit-get-current-branch) "(no branch)")
          ": " (mercit-rev-format "{node|short} {desc|firstline}")))

;;; Sections

(defvar mercit-stashes-section-map
  (let ((map (make-sparse-keymap)))
    (mercit-menu-set map [mercit-visit-thing]  #'mercit-stash-list  "List %t")
    (mercit-menu-set map [mercit-delete-thing] #'mercit-stash-clear "Clear %t")
    map)
  "Keymap for `stashes' section.")

(defvar mercit-stash-section-map
  (let ((map (make-sparse-keymap)))
    (mercit-menu-set map [mercit-visit-thing]  #'mercit-stash-show  "Visit %v")
    (mercit-menu-set map [mercit-delete-thing] #'mercit-stash-drop  "Delete %M")
    (mercit-menu-set map [mercit-cherry-apply] #'mercit-stash-apply "Apply %M")
    (mercit-menu-set map [mercit-cherry-pick]  #'mercit-stash-pop   "Pop %M")
    map)
  "Keymap for `stash' sections.")

(mercit-define-section-jumper mercit-jump-to-stashes
  "Stashes" stashes "refs/stash")

(cl-defun mercit-insert-stashes (&optional (ref   "refs/stash")
                                          (heading "Stashes:"))
  "Insert `stashes' section showing reflog for \"refs/stash\".
If optional REF is non-nil, show reflog for that instead.
If optional HEADING is non-nil, use that as section heading
instead of \"Stashes:\"."
  (let ((verified (mercit-rev-verify ref))
        (autostash (mercit-rebase--get-state-lines "autostash")))
    (when (or autostash verified)
      (mercit-insert-section (stashes ref)
        (mercit-insert-heading heading)
        (when autostash
          (pcase-let ((`(,author ,date ,msg)
                       (split-string
                        (car (mercit-git-lines
                              "show" "-q" "--format=%aN%x00%at%x00%s"
                              autostash))
                        "\0")))
            (mercit-insert-section (stash autostash)
              (insert (propertize "AUTOSTASH" 'font-lock-face 'mercit-hash))
              (insert " " msg "\n")
              (save-excursion
                (backward-char)
                (mercit-log-format-margin autostash author date)))))
        (if verified
            (mercit-git-wash (apply-partially #'mercit-log-wash-log 'stash)
              "reflog" "--format=%gd%x00%aN%x00%at%x00%gs" ref)
          (insert ?\n)
          (save-excursion
            (backward-char)
            (mercit-make-margin-overlay)))))))

;;; List Stashes

;;;###autoload
(defun mercit-stash-list ()
  "List all stashes in a buffer."
  (interactive)
  (mercit-stashes-setup-buffer))

(define-derived-mode mercit-stashes-mode mercit-reflog-mode "Mercit Stashes"
  "Mode for looking at lists of stashes."
  :group 'mercit-log
  (hack-dir-local-variables-non-file-buffer))

(defun mercit-stashes-setup-buffer ()
  (mercit-setup-buffer #'mercit-stashes-mode nil
    (mercit-buffer-refname "refs/stash")))

(defun mercit-stashes-refresh-buffer ()
  (mercit-insert-section (stashesbuf)
    (mercit-insert-heading (if (equal mercit-buffer-refname "refs/stash")
                              "Stashes:"
                            (format "Stashes [%s]:" mercit-buffer-refname)))
    (mercit-git-wash (apply-partially #'mercit-log-wash-log 'stash)
      "reflog" "--format=%gd%x00%aN%x00%at%x00%gs" mercit-buffer-refname)))

(cl-defmethod mercit-buffer-value (&context (major-mode mercit-stashes-mode))
  mercit-buffer-refname)

(defvar mercit--update-stash-buffer nil)

(defun mercit-stashes-maybe-update-stash-buffer (&optional _)
  "When moving in the stashes buffer, update the stash buffer.
If there is no stash buffer in the same frame, then do nothing."
  (when (derived-mode-p 'mercit-stashes-mode)
    (mercit--maybe-update-stash-buffer)))

(defun mercit--maybe-update-stash-buffer ()
  (when-let* ((stash  (mercit-section-value-if 'stash))
              (buffer (mercit-get-mode-buffer 'mercit-stash-mode nil t)))
    (if mercit--update-stash-buffer
        (setq mercit--update-stash-buffer (list stash buffer))
      (setq mercit--update-stash-buffer (list stash buffer))
      (run-with-idle-timer
       mercit-update-other-window-delay nil
       (let ((args (with-current-buffer buffer
                     (let ((mercit-direct-use-buffer-arguments 'selected))
                       (mercit-show-commit--arguments)))))
         (lambda ()
           (pcase-let ((`(,stash ,buf) mercit--update-stash-buffer))
             (setq mercit--update-stash-buffer nil)
             (when (buffer-live-p buf)
               (let ((mercit-display-buffer-noselect t))
                 (apply #'mercit-stash-show stash args))))
           (setq mercit--update-stash-buffer nil)))))))

;;; Show Stash

;;;###autoload
(defun mercit-stash-show (stash &optional args files)
  "Show all diffs of a stash in a buffer."
  (interactive (cons (or (and (not current-prefix-arg)
                              (mercit-stash-at-point))
                         (mercit-read-stash "Show stash"))
                     (pcase-let ((`(,args ,files)
                                  (mercit-diff-arguments 'mercit-stash-mode)))
                       (list (delete "--stat" args) files))))
  (mercit-stash-setup-buffer stash args files))

(define-derived-mode mercit-stash-mode mercit-diff-mode "Mercit Stash"
  "Mode for looking at individual stashes."
  :group 'mercit-diff
  (hack-dir-local-variables-non-file-buffer)
  (setq mercit--imenu-group-types '(commit)))

(defun mercit-stash-setup-buffer (stash args files)
  (mercit-setup-buffer #'mercit-stash-mode nil
    (mercit-buffer-revision stash)
    (mercit-buffer-range (format "%s^..%s" stash stash))
    (mercit-buffer-diff-args args)
    (mercit-buffer-diff-files files)))

(defun mercit-stash-refresh-buffer ()
  (mercit-set-header-line-format
   (concat (capitalize mercit-buffer-revision) " "
           (propertize (mercit-rev-format "{desc|firstline}"
                                          mercit-buffer-revision)
                       'font-lock-face
                       (list :weight 'normal :foreground
                             (face-attribute 'default :foreground)))))
  (setq mercit-buffer-revision-hash (mercit-rev-parse mercit-buffer-revision))
  (mercit-insert-section (stash)
    (mercit-run-section-hook 'mercit-stash-sections-hook)))

(cl-defmethod mercit-buffer-value (&context (major-mode mercit-stash-mode))
  mercit-buffer-revision)

(defun mercit-stash-insert-section (commit range message &optional files)
  (mercit-insert-section (commit commit)
    (mercit-insert-heading message)
    (mercit--insert-diff "diff" range "--noprefix" mercit-buffer-diff-args
    			 "--unified=3"  ;; TODO? make num lines configurable?
                        "--" (or files mercit-buffer-diff-files))))

(defun mercit-insert-stash-notes ()
  "Insert section showing notes for a stash.
This shows the notes for stash@{N} but not for the other commits
that make up the stash."
  (mercit-insert-section section (note)
    (mercit-insert-heading "Notes")
    (mercit-git-insert "notes" "show" mercit-buffer-revision)
    (if (= (point)
           (oref section content))
        (mercit-cancel-section)
      (insert "\n"))))

(defun mercit-insert-stash-index ()
  "Insert section showing staged changes of the stash."
  (mercit-stash-insert-section
   (format "%s^2" mercit-buffer-revision)
   (format "%s^..%s^2" mercit-buffer-revision mercit-buffer-revision)
   "Staged"))

(defun mercit-insert-stash-worktree ()
  "Insert section showing unstaged changes of the stash."
  (mercit-stash-insert-section
   mercit-buffer-revision
   (format "%s^2..%s" mercit-buffer-revision mercit-buffer-revision)
   "Unstaged"))

(defun mercit-insert-stash-untracked ()
  "Insert section showing the untracked files commit of the stash."
  (let ((stash mercit-buffer-revision)
        (rev (concat mercit-buffer-revision "^3")))
    (when (mercit-rev-verify rev)
      (mercit-stash-insert-section (format "%s^3" stash)
                                  (format "%s^..%s^3" stash stash)
                                  "Untracked files"
                                  (mercit-git-items "ls-tree" "-z" "--name-only"
                                                   "-r" "--full-tree" rev)))))

;;; _
(provide 'mercit-stash)
;;; mercit-stash.el ends here
