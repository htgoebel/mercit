;;; mercit-merge.el --- Merge functionality  -*- lexical-binding:t -*-

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

;; This library implements merge commands.

;;; Code:

(require 'mercit)
(require 'mercit-diff)

(declare-function mercit-git-push "mercit-push" (branch target args))

;;; Commands

;;;###autoload (autoload 'mercit-merge "mercit" nil t)
(transient-define-prefix mercit-merge ()
  "Merge branches."
  :man-page "git-merge"
  :incompatible '(("--ff-only" "--no-ff"))
  ["Arguments"
   :if-not mercit-merge-in-progress-p
   ("-f" "Fast-forward only" "--ff-only")
   ("-n" "No fast-forward"   "--no-ff")
   (mercit-merge:--strategy)
   (5 mercit-merge:--strategy-option)
   (5 "-b" "Ignore changes in amount of whitespace" "-Xignore-space-change")
   (5 "-w" "Ignore whitespace when comparing lines" "-Xignore-all-space")
   (5 mercit-diff:--diff-algorithm :argument "-Xdiff-algorithm=")
   (5 mercit:--gpg-sign)]
  ["Actions"
   :if-not mercit-merge-in-progress-p
   [("m" "Merge"                  mercit-merge-plain)
    ("e" "Merge and edit message" mercit-merge-editmsg)
    ("n" "Merge but don't commit" mercit-merge-nocommit)
    ("a" "Absorb"                 mercit-merge-absorb)]
   [("p" "Preview merge"          mercit-merge-preview)
    ""
    ("s" "Squash merge"           mercit-merge-squash)
    ("i" "Dissolve"               mercit-merge-into)]]
  ["Actions"
   :if mercit-merge-in-progress-p
   ("m" "Commit merge" mercit-commit-create)
   ("a" "Abort merge"  mercit-merge-abort)])

(defun mercit-merge-arguments ()
  (transient-args 'mercit-merge))

(transient-define-argument mercit-merge:--strategy ()
  :description "Strategy"
  :class 'transient-option
  ;; key for merge and rebase: "-s"
  ;; key for cherry-pick and revert: "=s"
  ;; shortarg for merge and rebase: "-s"
  ;; shortarg for cherry-pick and revert: none
  :key "-s"
  :argument "--strategy="
  :choices '("resolve" "recursive" "octopus" "ours" "subtree"))

(transient-define-argument mercit-merge:--strategy-option ()
  :description "Strategy Option"
  :class 'transient-option
  :key "-X"
  :argument "--strategy-option="
  :choices '("ours" "theirs" "patience"))

;;;###autoload
(defun mercit-merge-plain (rev &optional args nocommit)
  "Merge commit REV into the current branch; using default message.

Unless there are conflicts or a prefix argument is used create a
merge commit using a generic commit message and without letting
the user inspect the result.  With a prefix argument pretend the
merge failed to give the user the opportunity to inspect the
merge.

\(git merge --no-edit|--no-commit [ARGS] REV)"
  (interactive (list (mercit-read-other-branch-or-commit "Merge")
                     (mercit-merge-arguments)
                     current-prefix-arg))
  (mercit-merge-assert)
  (mercit-run-git-async "merge" (if nocommit "--no-commit" "--no-edit") args rev))

;;;###autoload
(defun mercit-merge-editmsg (rev &optional args)
  "Merge commit REV into the current branch; and edit message.
Perform the merge and prepare a commit message but let the user
edit it.
\n(git merge --edit --no-ff [ARGS] REV)"
  (interactive (list (mercit-read-other-branch-or-commit "Merge")
                     (mercit-merge-arguments)))
  (mercit-merge-assert)
  (cl-pushnew "--no-ff" args :test #'equal)
  (apply #'mercit-run-git-with-editor "merge" "--edit"
         (append (delete "--ff-only" args)
                 (list rev))))

;;;###autoload
(defun mercit-merge-nocommit (rev &optional args)
  "Merge commit REV into the current branch; pretending it failed.
Pretend the merge failed to give the user the opportunity to
inspect the merge and change the commit message.
\n(git merge --no-commit --no-ff [ARGS] REV)"
  (interactive (list (mercit-read-other-branch-or-commit "Merge")
                     (mercit-merge-arguments)))
  (mercit-merge-assert)
  (cl-pushnew "--no-ff" args :test #'equal)
  (mercit-run-git-async "merge" "--no-commit" args rev))

;;;###autoload
(defun mercit-merge-into (branch &optional args)
  "Merge the current branch into BRANCH and remove the former.

Before merging, force push the source branch to its push-remote,
provided the respective remote branch already exists, ensuring
that the respective pull-request (if any) won't get stuck on some
obsolete version of the commits that are being merged.  Finally
if `forge-branch-pullreq' was used to create the merged branch,
then also remove the respective remote branch."
  (interactive
   (list (mercit-read-other-local-branch
          (format "Merge `%s' into"
                  (or (mercit-get-current-branch)
                      (mercit-rev-parse "HEAD")))
          nil
          (and-let* ((upstream (mercit-get-upstream-branch))
                     (upstream (cdr (mercit-split-branch-name upstream))))
            (and (mercit-branch-p upstream) upstream)))
         (mercit-merge-arguments)))
  (let ((current (mercit-get-current-branch))
        (head (mercit-rev-parse "HEAD")))
    (when (zerop (mercit-call-git "checkout" branch))
      (if current
          (mercit--merge-absorb current args)
        (mercit-run-git-with-editor "merge" args head)))))

;;;###autoload
(defun mercit-merge-absorb (branch &optional args)
  "Merge BRANCH into the current branch and remove the former.

Before merging, force push the source branch to its push-remote,
provided the respective remote branch already exists, ensuring
that the respective pull-request (if any) won't get stuck on some
obsolete version of the commits that are being merged.  Finally
if `forge-branch-pullreq' was used to create the merged branch,
then also remove the respective remote branch."
  (interactive (list (mercit-read-other-local-branch "Absorb branch")
                     (mercit-merge-arguments)))
  (mercit--merge-absorb branch args))

(defun mercit--merge-absorb (branch args)
  (when (equal branch (mercit-main-branch))
    (unless (yes-or-no-p
             (format "Do you really want to merge `%s' into another branch? "
                     branch))
      (user-error "Abort")))
  (if-let ((target (mercit-get-push-branch branch t)))
      (progn
        (mercit-git-push branch target (list "--force-with-lease"))
        (set-process-sentinel
         mercit-this-process
         (lambda (process event)
           (when (memq (process-status process) '(exit signal))
             (if (not (zerop (process-exit-status process)))
                 (mercit-process-sentinel process event)
               (process-put process 'inhibit-refresh t)
               (mercit-process-sentinel process event)
               (mercit--merge-absorb-1 branch args))))))
    (mercit--merge-absorb-1 branch args)))

(defun mercit--merge-absorb-1 (branch args)
  (if-let ((pr (mercit-get "branch" branch "pullRequest")))
      (mercit-run-git-async
       "merge" args "-m"
       (format "Merge branch '%s'%s [#%s]"
               branch
               (let ((current (mercit-get-current-branch)))
                 (if (equal current (mercit-main-branch))
                     ""
                   (format " into %s" current)))
               pr)
       branch)
    (mercit-run-git-async "merge" args "--no-edit" branch))
  (set-process-sentinel
   mercit-this-process
   (lambda (process event)
     (when (memq (process-status process) '(exit signal))
       (if (> (process-exit-status process) 0)
           (mercit-process-sentinel process event)
         (process-put process 'inhibit-refresh t)
         (mercit-process-sentinel process event)
         (mercit-branch-maybe-delete-pr-remote branch)
         (mercit-branch-unset-pushRemote branch)
         (mercit-run-git "branch" "-D" branch))))))

;;;###autoload
(defun mercit-merge-squash (rev)
  "Squash commit REV into the current branch; don't create a commit.
\n(git merge --squash REV)"
  (interactive (list (mercit-read-other-branch-or-commit "Squash")))
  (mercit-merge-assert)
  (mercit-run-git-async "merge" "--squash" rev))

;;;###autoload
(defun mercit-merge-preview (rev)
  "Preview result of merging REV into the current branch."
  (interactive (list (mercit-read-other-branch-or-commit "Preview merge")))
  (mercit-merge-preview-setup-buffer rev))

;;;###autoload
(defun mercit-merge-abort ()
  "Abort the current merge operation.
\n(git merge --abort)"
  (interactive)
  (unless (file-exists-p (mercit-git-dir "MERGE_HEAD"))
    (user-error "No merge in progress"))
  (mercit-confirm 'abort-merge)
  (mercit-run-git-async "merge" "--abort"))

(defun mercit-checkout-stage (file arg)
  "During a conflict checkout and stage side, or restore conflict."
  (interactive
   (let ((file (mercit-completing-read "Checkout file"
                                      (mercit-tracked-files) nil nil nil
                                      'mercit-read-file-hist
                                      (mercit-current-file))))
     (cond ((member file (mercit-unmerged-files))
            (list file (mercit-checkout-read-stage file)))
           ((yes-or-no-p (format "Restore conflicts in %s? " file))
            (list file "--merge"))
           (t
            (user-error "Quit")))))
  (pcase (cons arg (cddr (car (mercit-file-status file))))
    ((or `("--ours"   ?D ,_)
         '("--ours"   ?U ?A)
         `("--theirs" ,_ ?D)
         '("--theirs" ?A ?U))
     (mercit-run-git "rm" "--" file))
    (_ (if (equal arg "--merge")
           ;; This fails if the file was deleted on one
           ;; side.  And we cannot do anything about it.
           (mercit-run-git "checkout" "--merge" "--" file)
         (mercit-call-git "checkout" arg "--" file)
         (mercit-run-git "add" "-u" "--" file)))))

;;; Utilities

(defun mercit-merge-in-progress-p ()
  (file-exists-p (mercit-git-dir "MERGE_HEAD")))

(defun mercit--merge-range (&optional head)
  (unless head
    (setq head (mercit-get-shortname
                (car (mercit-file-lines (mercit-git-dir "MERGE_HEAD"))))))
  (and head
       (concat (mercit-git-string "merge-base" "--octopus" "HEAD" head)
               ".." head)))

(defun mercit-merge-assert ()
  (or (not (mercit-anything-modified-p t))
      (mercit-confirm 'merge-dirty
        "Merging with dirty worktree is risky.  Continue")))

(defun mercit-checkout-read-stage (file)
  (mercit-read-char-case (format "For %s checkout: " file) t
    (?o "[o]ur stage"   "--ours")
    (?t "[t]heir stage" "--theirs")
    (?c "[c]onflict"    "--merge")))

;;; Sections

(defvar mercit-unmerged-section-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mercit-log-section-map)
    map)
  "Keymap for `unmerged' sections.")

(defun mercit-insert-merge-log ()
  "Insert section for the on-going merge.
Display the heads that are being merged.
If no merge is in progress, do nothing."
  (when (mercit-merge-in-progress-p)
    (let* ((heads (mapcar #'mercit-get-shortname
                          (mercit-file-lines (mercit-git-dir "MERGE_HEAD"))))
           (range (mercit--merge-range (car heads))))
      (mercit-insert-section (unmerged range)
        (mercit-insert-heading
          (format "Merging %s:" (mapconcat #'identity heads ", ")))
        (mercit-insert-log
         range
         (let ((args mercit-buffer-log-args))
           (unless (member "--decorate=full" mercit-buffer-log-args)
             (push "--decorate=full" args))
           args))))))

;;; _
(provide 'mercit-merge)
;;; mercit-merge.el ends here
