;;; mercit-sequence.el --- History manipulation in Magit  -*- lexical-binding:t -*-

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

;; Support for Git commands that replay commits and help the user make
;; changes along the way.  Supports `cherry-pick', `revert', `rebase',
;; `rebase--interactive' and `am'.

;;; Code:

(require 'mercit)

;; For `mercit-rebase--todo'.
(declare-function git-rebase-current-line "git-rebase" ())
(eval-when-compile
  (cl-pushnew 'action-type eieio--known-slot-names)
  (cl-pushnew 'action eieio--known-slot-names)
  (cl-pushnew 'action-options eieio--known-slot-names)
  (cl-pushnew 'target eieio--known-slot-names))

;;; Options
;;;; Faces

(defface mercit-sequence-pick
  '((t :inherit default))
  "Face used in sequence sections."
  :group 'mercit-faces)

(defface mercit-sequence-stop
  '((((class color) (background light)) :foreground "DarkOliveGreen4")
    (((class color) (background dark))  :foreground "DarkSeaGreen2"))
  "Face used in sequence sections."
  :group 'mercit-faces)

(defface mercit-sequence-part
  '((((class color) (background light)) :foreground "Goldenrod4")
    (((class color) (background dark))  :foreground "LightGoldenrod2"))
  "Face used in sequence sections."
  :group 'mercit-faces)

(defface mercit-sequence-head
  '((((class color) (background light)) :foreground "SkyBlue4")
    (((class color) (background dark))  :foreground "LightSkyBlue1"))
  "Face used in sequence sections."
  :group 'mercit-faces)

(defface mercit-sequence-drop
  '((((class color) (background light)) :foreground "IndianRed")
    (((class color) (background dark))  :foreground "IndianRed"))
  "Face used in sequence sections."
  :group 'mercit-faces)

(defface mercit-sequence-done
  '((t :inherit mercit-hash))
  "Face used in sequence sections."
  :group 'mercit-faces)

(defface mercit-sequence-onto
  '((t :inherit mercit-sequence-done))
  "Face used in sequence sections."
  :group 'mercit-faces)

(defface mercit-sequence-exec
  '((t :inherit mercit-hash))
  "Face used in sequence sections."
  :group 'mercit-faces)

;;; Common

;;;###autoload
(defun mercit-sequencer-continue ()
  "Resume the current cherry-pick or revert sequence."
  (interactive)
  (if (mercit-sequencer-in-progress-p)
      (if (mercit-anything-unmerged-p)
          (user-error "Cannot continue due to unresolved conflicts")
        (mercit-run-git-sequencer
         (if (mercit-revert-in-progress-p) "revert" "cherry-pick") "--continue"))
    (user-error "No cherry-pick or revert in progress")))

;;;###autoload
(defun mercit-sequencer-skip ()
  "Skip the stopped at commit during a cherry-pick or revert sequence."
  (interactive)
  (if (mercit-sequencer-in-progress-p)
      (progn (mercit-call-git "reset" "--hard")
             (mercit-sequencer-continue))
    (user-error "No cherry-pick or revert in progress")))

;;;###autoload
(defun mercit-sequencer-abort ()
  "Abort the current cherry-pick or revert sequence.
This discards all changes made since the sequence started."
  (interactive)
  (if (mercit-sequencer-in-progress-p)
      (mercit-run-git-sequencer
       (if (mercit-revert-in-progress-p) "revert" "cherry-pick") "--abort")
    (user-error "No cherry-pick or revert in progress")))

(defun mercit-sequencer-in-progress-p ()
  (or (mercit-cherry-pick-in-progress-p)
      (mercit-revert-in-progress-p)))

;;; Cherry-Pick

(defvar mercit-perl-executable "perl"
  "The Perl executable.")

;;;###autoload (autoload 'mercit-cherry-pick "mercit-sequence" nil t)
(transient-define-prefix mercit-cherry-pick ()
  "Apply or transplant commits."
  :man-page "git-cherry-pick"
  :value '("--ff")
  :incompatible '(("--ff" "-x"))
  ["Arguments"
   :if-not mercit-sequencer-in-progress-p
   (mercit-cherry-pick:--mainline)
   ("=s" mercit-merge:--strategy)
   ("-F" "Attempt fast-forward"               "--ff")
   ("-x" "Reference cherry in commit message" "-x")
   ("-e" "Edit commit messages"               ("-e" "--edit"))
   ("-s" "Add Signed-off-by lines"            ("-s" "--signoff"))
   (5 mercit:--gpg-sign)]
  [:if-not mercit-sequencer-in-progress-p
   ["Apply here"
    ("A" "Pick"    mercit-cherry-copy)
    ("a" "Apply"   mercit-cherry-apply)
    ("h" "Harvest" mercit-cherry-harvest)
    ("m" "Squash"  mercit-merge-squash)]
   ["Apply elsewhere"
    ("d" "Donate"  mercit-cherry-donate)
    ("n" "Spinout" mercit-cherry-spinout)
    ("s" "Spinoff" mercit-cherry-spinoff)]]
  ["Actions"
   :if mercit-sequencer-in-progress-p
   ("A" "Continue" mercit-sequencer-continue)
   ("s" "Skip"     mercit-sequencer-skip)
   ("a" "Abort"    mercit-sequencer-abort)])

(transient-define-argument mercit-cherry-pick:--mainline ()
  :description "Replay merge relative to parent"
  :class 'transient-option
  :shortarg "-m"
  :argument "--mainline="
  :reader #'transient-read-number-N+)

(defun mercit-cherry-pick-read-args (prompt)
  (list (or (nreverse (mercit-region-values 'commit))
            (mercit-read-other-branch-or-commit prompt))
        (transient-args 'mercit-cherry-pick)))

(defun mercit--cherry-move-read-args (verb away fn &optional allow-detached)
  (declare (indent defun))
  (let ((commits (or (nreverse (mercit-region-values 'commit))
                     (list (funcall (if away
                                        #'mercit-read-branch-or-commit
                                      #'mercit-read-other-branch-or-commit)
                                    (format "%s cherry" (capitalize verb))))))
        (current (or (mercit-get-current-branch)
                     (and allow-detached (mercit-rev-parse "HEAD")))))
    (unless current
      (user-error "Cannot %s cherries while HEAD is detached" verb))
    (let ((reachable (mercit-rev-ancestor-p (car commits) current))
          (msg "Cannot %s cherries that %s reachable from HEAD"))
      (pcase (list away reachable)
        ('(nil t) (user-error msg verb "are"))
        ('(t nil) (user-error msg verb "are not"))))
    `(,commits
      ,@(funcall fn commits)
      ,(transient-args 'mercit-cherry-pick))))

(defun mercit--cherry-spinoff-read-args (verb)
  (mercit--cherry-move-read-args verb t
    (lambda (commits)
      (mercit-branch-read-args
       (format "Create branch from %s cherries" (length commits))
       (mercit-get-upstream-branch)))))

;;;###autoload
(defun mercit-cherry-copy (commits &optional args)
  "Copy COMMITS from another branch onto the current branch.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then pick all of them,
without prompting."
  (interactive (mercit-cherry-pick-read-args "Cherry-pick"))
  (mercit--cherry-pick commits args))

;;;###autoload
(defun mercit-cherry-apply (commits &optional args)
  "Apply the changes in COMMITS but do not commit them.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then apply all of them,
without prompting."
  (interactive (mercit-cherry-pick-read-args "Apply changes from commit"))
  (mercit--cherry-pick commits (cons "--no-commit" (remove "--ff" args))))

;;;###autoload
(defun mercit-cherry-harvest (commits branch &optional args)
  "Move COMMITS from another BRANCH onto the current branch.
Remove the COMMITS from BRANCH and stay on the current branch.
If a conflict occurs, then you have to fix that and finish the
process manually."
  (interactive
   (mercit--cherry-move-read-args "harvest" nil
     (lambda (commits)
       (list (let ((branches (mercit-list-containing-branches (car commits))))
               (pcase (length branches)
                 (0 nil)
                 (1 (car branches))
                 (_ (mercit-completing-read
                     (let ((len (length commits)))
                       (if (= len 1)
                           "Remove 1 cherry from branch"
                         (format "Remove %s cherries from branch" len)))
                     branches nil t))))))))
  (mercit--cherry-move commits branch (mercit-get-current-branch) args nil t))

;;;###autoload
(defun mercit-cherry-donate (commits branch &optional args)
  "Move COMMITS from the current branch onto another existing BRANCH.
Remove COMMITS from the current branch and stay on that branch.
If a conflict occurs, then you have to fix that and finish the
process manually.  `HEAD' is allowed to be detached initially."
  (interactive
   (mercit--cherry-move-read-args "donate" t
     (lambda (commits)
       (list (mercit-read-other-branch
              (let ((len (length commits)))
                (if (= len 1)
                    "Move 1 cherry to branch"
                  (format "Move %s cherries to branch" len))))))
     'allow-detached))
  (mercit--cherry-move commits
                      (or (mercit-get-current-branch)
                          (mercit-rev-parse "HEAD"))
                      branch args))

;;;###autoload
(defun mercit-cherry-spinout (commits branch start-point &optional args)
  "Move COMMITS from the current branch onto a new BRANCH.
Remove COMMITS from the current branch and stay on that branch.
If a conflict occurs, then you have to fix that and finish the
process manually."
  (interactive (mercit--cherry-spinoff-read-args "spinout"))
  (mercit--cherry-move commits (mercit-get-current-branch) branch args
                      start-point))

;;;###autoload
(defun mercit-cherry-spinoff (commits branch start-point &optional args)
  "Move COMMITS from the current branch onto a new BRANCH.
Remove COMMITS from the current branch and checkout BRANCH.
If a conflict occurs, then you have to fix that and finish
the process manually."
  (interactive (mercit--cherry-spinoff-read-args "spinoff"))
  (mercit--cherry-move commits (mercit-get-current-branch) branch args
                      start-point t))

(defun mercit--cherry-move (commits src dst args
                                   &optional start-point checkout-dst)
  (let ((current (mercit-get-current-branch)))
    (unless (mercit-branch-p dst)
      (let ((mercit-process-raise-error t))
        (mercit-call-git "branch" dst start-point))
      (--when-let (mercit-get-indirect-upstream-branch start-point)
        (mercit-call-git "branch" "--set-upstream-to" it dst)))
    (unless (equal dst current)
      (let ((mercit-process-raise-error t))
        (mercit-call-git "checkout" dst)))
    (if (not src) ; harvest only
        (mercit--cherry-pick commits args)
      (let ((tip (car (last commits)))
            (keep (concat (car commits) "^")))
        (mercit--cherry-pick commits args)
        (set-process-sentinel
         mercit-this-process
         (lambda (process event)
           (when (memq (process-status process) '(exit signal))
             (if (> (process-exit-status process) 0)
                 (mercit-process-sentinel process event)
               (process-put process 'inhibit-refresh t)
               (mercit-process-sentinel process event)
               (cond
                ((mercit-rev-equal tip src)
                 (mercit-call-git "update-ref"
                                 "-m" (format "reset: moving to %s" keep)
                                 (mercit-ref-fullname src)
                                 keep tip)
                 (if (not checkout-dst)
                     (mercit-run-git "checkout" src)
                   (mercit-refresh)))
                (t
                 (mercit-git "checkout" src)
                 (with-environment-variables
                     (("GIT_SEQUENCE_EDITOR"
                       (format "%s -i -ne '/^pick (%s)/ or print'"
                               mercit-perl-executable
                               (mapconcat #'mercit-rev-abbrev commits "|"))))
                   (mercit-run-git-sequencer "rebase" "-i" keep))
                 (when checkout-dst
                   (set-process-sentinel
                    mercit-this-process
                    (lambda (process event)
                      (when (memq (process-status process) '(exit signal))
                        (if (> (process-exit-status process) 0)
                            (mercit-process-sentinel process event)
                          (process-put process 'inhibit-refresh t)
                          (mercit-process-sentinel process event)
                          (mercit-run-git "checkout" dst))))))))))))))))

(defun mercit--cherry-pick (commits args &optional revert)
  (let ((command (if revert "revert" "cherry-pick")))
    (when (stringp commits)
      (setq commits (if (string-search ".." commits)
                        (split-string commits "\\.\\.")
                      (list commits))))
    (mercit-run-git-sequencer
     (if revert "revert" "cherry-pick")
     (pcase-let ((`(,merge ,non-merge)
                  (-separate #'mercit-merge-commit-p commits)))
       (cond
        ((not merge)
         (--remove (string-prefix-p "--mainline=" it) args))
        (non-merge
         (user-error "Cannot %s merge and non-merge commits at once"
                     command))
        ((--first (string-prefix-p "--mainline=" it) args)
         args)
        (t
         (cons (format "--mainline=%s"
                       (read-number "Replay merges relative to parent: "))
               args))))
     commits)))

(defun mercit-cherry-pick-in-progress-p ()
  ;; .git/sequencer/todo does not exist when there is only one commit left.
  (or (file-exists-p (mercit-git-dir "CHERRY_PICK_HEAD"))
      ;; And CHERRY_PICK_HEAD does not exist when a conflict happens
      ;; while picking a series of commits with --no-commit.
      (when-let ((line (mercit-file-line (mercit-git-dir "sequencer/todo"))))
        (string-prefix-p "pick" line))))

;;; Revert

;;;###autoload (autoload 'mercit-revert "mercit-sequence" nil t)
(transient-define-prefix mercit-revert ()
  "Revert existing commits, with or without creating new commits."
  :man-page "git-revert"
  :value '("--edit")
  ["Arguments"
   :if-not mercit-sequencer-in-progress-p
   (mercit-cherry-pick:--mainline)
   ("-e" "Edit commit message"       ("-e" "--edit"))
   ("-E" "Don't edit commit message" "--no-edit")
   ("=s" mercit-merge:--strategy)
   ("-s" "Add Signed-off-by lines"   ("-s" "--signoff"))
   (5 mercit:--gpg-sign)]
  ["Actions"
   :if-not mercit-sequencer-in-progress-p
   ("V" "Revert commit(s)" mercit-revert-and-commit)
   ("v" "Revert changes"   mercit-revert-no-commit)]
  ["Actions"
   :if mercit-sequencer-in-progress-p
   ("V" "Continue" mercit-sequencer-continue)
   ("s" "Skip"     mercit-sequencer-skip)
   ("a" "Abort"    mercit-sequencer-abort)])

(defun mercit-revert-read-args (prompt)
  (list (or (mercit-region-values 'commit)
            (mercit-read-branch-or-commit prompt))
        (transient-args 'mercit-revert)))

;;;###autoload
(defun mercit-revert-and-commit (commit &optional args)
  "Revert COMMIT by creating a new commit.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then revert all of them,
without prompting."
  (interactive (mercit-revert-read-args "Revert commit"))
  (mercit--cherry-pick commit args t))

;;;###autoload
(defun mercit-revert-no-commit (commit &optional args)
  "Revert COMMIT by applying it in reverse to the worktree.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then revert all of them,
without prompting."
  (interactive (mercit-revert-read-args "Revert changes"))
  (mercit--cherry-pick commit (cons "--no-commit" args) t))

(defun mercit-revert-in-progress-p ()
  ;; .git/sequencer/todo does not exist when there is only one commit left.
  (or (file-exists-p (mercit-git-dir "REVERT_HEAD"))
      ;; And REVERT_HEAD does not exist when a conflict happens while
      ;; reverting a series of commits with --no-commit.
      (when-let ((line (mercit-file-line (mercit-git-dir "sequencer/todo"))))
        (string-prefix-p "revert" line))))

;;; Patch

;;;###autoload (autoload 'mercit-am "mercit-sequence" nil t)
(transient-define-prefix mercit-am ()
  "Apply patches received by email."
  :man-page "git-am"
  :value '("--3way")
  ["Arguments"
   :if-not mercit-am-in-progress-p
   ("-3" "Fall back on 3way merge"           ("-3" "--3way"))
   (mercit-apply:-p)
   ("-c" "Remove text before scissors line"  ("-c" "--scissors"))
   ("-k" "Inhibit removal of email cruft"    ("-k" "--keep"))
   ("-b" "Limit removal of email cruft"      "--keep-non-patch")
   ("-d" "Use author date as committer date" "--committer-date-is-author-date")
   ("-t" "Use current time as author date"   "--ignore-date")
   ("-s" "Add Signed-off-by lines"           ("-s" "--signoff"))
   (5 mercit:--gpg-sign)]
  ["Apply"
   :if-not mercit-am-in-progress-p
   ("m" "maildir"     mercit-am-apply-maildir)
   ("w" "patches"     mercit-am-apply-patches)
   ("a" "plain patch" mercit-patch-apply)]
  ["Actions"
   :if mercit-am-in-progress-p
   ("w" "Continue" mercit-am-continue)
   ("s" "Skip"     mercit-am-skip)
   ("a" "Abort"    mercit-am-abort)])

(defun mercit-am-arguments ()
  (transient-args 'mercit-am))

(transient-define-argument mercit-apply:-p ()
  :description "Remove leading slashes from paths"
  :class 'transient-option
  :argument "-p"
  :allow-empty t
  :reader #'transient-read-number-N+)

;;;###autoload
(defun mercit-am-apply-patches (&optional files args)
  "Apply the patches FILES."
  (interactive (list (or (mercit-region-values 'file)
                         (list (let ((default (mercit-file-at-point)))
                                 (read-file-name
                                  (if default
                                      (format "Apply patch (%s): " default)
                                    "Apply patch: ")
                                  nil default))))
                     (mercit-am-arguments)))
  (mercit-run-git-sequencer "am" args "--"
                           (--map (mercit-convert-filename-for-git
                                   (expand-file-name it))
                                  files)))

;;;###autoload
(defun mercit-am-apply-maildir (&optional maildir args)
  "Apply the patches from MAILDIR."
  (interactive (list (read-file-name "Apply mbox or Maildir: ")
                     (mercit-am-arguments)))
  (mercit-run-git-sequencer "am" args (mercit-convert-filename-for-git
                                      (expand-file-name maildir))))

;;;###autoload
(defun mercit-am-continue ()
  "Resume the current patch applying sequence."
  (interactive)
  (if (mercit-am-in-progress-p)
      (if (mercit-anything-unstaged-p t)
          (error "Cannot continue due to unstaged changes")
        (mercit-run-git-sequencer "am" "--continue"))
    (user-error "Not applying any patches")))

;;;###autoload
(defun mercit-am-skip ()
  "Skip the stopped at patch during a patch applying sequence."
  (interactive)
  (if (mercit-am-in-progress-p)
      (mercit-run-git-sequencer "am" "--skip")
    (user-error "Not applying any patches")))

;;;###autoload
(defun mercit-am-abort ()
  "Abort the current patch applying sequence.
This discards all changes made since the sequence started."
  (interactive)
  (if (mercit-am-in-progress-p)
      (mercit-run-git "am" "--abort")
    (user-error "Not applying any patches")))

(defun mercit-am-in-progress-p ()
  (file-exists-p (mercit-git-dir "rebase-apply/applying")))

;;; Rebase

;;;###autoload (autoload 'mercit-rebase "mercit-sequence" nil t)
(transient-define-prefix mercit-rebase ()
  "Transplant commits and/or modify existing commits."
  :man-page "git-rebase"
  :value '("--autostash")
  ["Arguments"
   :if-not mercit-rebase-in-progress-p
   ("-k" "Keep empty commits"       "--keep-empty")
   ("-p" "Preserve merges"          ("-p" "--preserve-merges")
    :if (lambda () (mercit-git-version< "2.33.0")))
   ("-r" "Rebase merges"            ("-r" "--rebase-merges=")
    mercit-rebase-merges-select-mode
    :if (lambda () (mercit-git-version>= "2.18.0")))
   ("-u" "Update branches"          "--update-refs"
    :if (lambda () (mercit-git-version>= "2.38.0")))
   (7 mercit-merge:--strategy)
   (7 mercit-merge:--strategy-option)
   (7 "=X" mercit-diff:--diff-algorithm :argument "-Xdiff-algorithm=")
   (7 "-f" "Force rebase"           ("-f" "--force-rebase"))
   ("-d" "Use author date as committer date" "--committer-date-is-author-date")
   ("-t" "Use current time as author date"   "--ignore-date")
   ("-a" "Autosquash"               "--autosquash")
   ("-A" "Autostash"                "--autostash")
   ("-i" "Interactive"              ("-i" "--interactive"))
   ("-h" "Disable hooks"            "--no-verify")
   (7 mercit-rebase:--exec)
   (5 mercit:--gpg-sign)]
  [:if-not mercit-rebase-in-progress-p
   :description (lambda ()
                  (format (propertize "Rebase %s onto" 'face 'transient-heading)
                          (propertize (or (mercit-get-current-branch) "HEAD")
                                      'face 'mercit-branch-local)))
   ("p" mercit-rebase-onto-pushremote)
   ("u" mercit-rebase-onto-upstream)
   ("e" "elsewhere" mercit-rebase-branch)]
  ["Rebase"
   :if-not mercit-rebase-in-progress-p
   [("i" "interactively"      mercit-rebase-interactive)
    ("s" "a subset"           mercit-rebase-subset)]
   [("m" "to modify a commit" mercit-rebase-edit-commit)
    ("w" "to reword a commit" mercit-rebase-reword-commit)
    ("k" "to remove a commit" mercit-rebase-remove-commit)
    ("f" "to autosquash"      mercit-rebase-autosquash)
    (6 "t" "to change dates"  mercit-reshelve-since)]]
  ["Actions"
   :if mercit-rebase-in-progress-p
   ("r" "Continue" mercit-rebase-continue)
   ("s" "Skip"     mercit-rebase-skip)
   ("e" "Edit"     mercit-rebase-edit)
   ("a" "Abort"    mercit-rebase-abort)])

(transient-define-argument mercit-rebase:--exec ()
  :description "Run command after commits"
  :class 'transient-option
  :shortarg "-x"
  :argument "--exec="
  :reader #'read-shell-command)

(defun mercit-rebase-merges-select-mode (&rest _ignore)
  (mercit-read-char-case nil t
    (?n "[n]o-rebase-cousins" "no-rebase-cousins")
    (?r "[r]ebase-cousins" "rebase-cousins")))

(defun mercit-rebase-arguments ()
  (transient-args 'mercit-rebase))

(defun mercit-git-rebase (target args)
  (mercit-run-git-sequencer "rebase" args target))

;;;###autoload (autoload 'mercit-rebase-onto-pushremote "mercit-sequence" nil t)
(transient-define-suffix mercit-rebase-onto-pushremote (args)
  "Rebase the current branch onto its push-remote branch.

With a prefix argument or when the push-remote is either not
configured or unusable, then let the user first configure the
push-remote."
  :if #'mercit-get-current-branch
  :description #'mercit-pull--pushbranch-description
  (interactive (list (mercit-rebase-arguments)))
  (pcase-let ((`(,branch ,remote)
               (mercit--select-push-remote "rebase onto that")))
    (mercit-git-rebase (concat remote "/" branch) args)))

;;;###autoload (autoload 'mercit-rebase-onto-upstream "mercit-sequence" nil t)
(transient-define-suffix mercit-rebase-onto-upstream (args)
  "Rebase the current branch onto its upstream branch.

With a prefix argument or when the upstream is either not
configured or unusable, then let the user first configure
the upstream."
  :if #'mercit-get-current-branch
  :description #'mercit-rebase--upstream-description
  (interactive (list (mercit-rebase-arguments)))
  (let* ((branch (or (mercit-get-current-branch)
                     (user-error "No branch is checked out")))
         (upstream (mercit-get-upstream-branch branch)))
    (when (or current-prefix-arg (not upstream))
      (setq upstream
            (mercit-read-upstream-branch
             branch (format "Set upstream of %s and rebase onto that" branch)))
      (mercit-set-upstream-branch branch upstream))
    (mercit-git-rebase upstream args)))

(defun mercit-rebase--upstream-description ()
  (and-let* ((branch (mercit-get-current-branch)))
    (or (mercit-get-upstream-branch branch)
        (let ((remote (mercit-get "branch" branch "remote"))
              (merge  (mercit-get "branch" branch "merge"))
              (u (mercit--propertize-face "@{upstream}" 'bold)))
          (cond
           ((mercit--unnamed-upstream-p remote merge)
            (concat u ", replacing unnamed"))
           ((mercit--valid-upstream-p remote merge)
            (concat u ", replacing non-existent"))
           ((or remote merge)
            (concat u ", replacing invalid"))
           (t
            (concat u ", setting that")))))))

;;;###autoload
(defun mercit-rebase-branch (target args)
  "Rebase the current branch onto a branch read in the minibuffer.
All commits that are reachable from `HEAD' but not from the
selected branch TARGET are being rebased."
  (interactive (list (mercit-read-other-branch-or-commit "Rebase onto")
                     (mercit-rebase-arguments)))
  (message "Rebasing...")
  (mercit-git-rebase target args)
  (message "Rebasing...done"))

;;;###autoload
(defun mercit-rebase-subset (newbase start args)
  "Rebase a subset of the current branch's history onto a new base.
Rebase commits from START to `HEAD' onto NEWBASE.
START has to be selected from a list of recent commits."
  (interactive (list (mercit-read-other-branch-or-commit
                      "Rebase subset onto" nil
                      (mercit-get-upstream-branch))
                     nil
                     (mercit-rebase-arguments)))
  (if start
      (progn (message "Rebasing...")
             (mercit-run-git-sequencer "rebase" "--onto" newbase start args)
             (message "Rebasing...done"))
    (mercit-log-select
      `(lambda (commit)
         (mercit-rebase-subset ,newbase (concat commit "^") (list ,@args)))
      (concat "Type %p on a commit to rebase it "
              "and commits above it onto " newbase ","))))

(defvar mercit-rebase-interactive-include-selected t)

(defun mercit-rebase-interactive-1
    (commit args message &optional editor delay-edit-confirm noassert confirm)
  (declare (indent 2))
  (when commit
    (if (eq commit :merge-base)
        (setq commit
              (and-let* ((upstream (mercit-get-upstream-branch)))
                (mercit-git-string "merge-base" upstream "HEAD")))
      (unless (mercit-rev-ancestor-p commit "HEAD")
        (user-error "%s isn't an ancestor of HEAD" commit))
      (if (mercit-commit-parents commit)
          (when (or (not (eq this-command 'mercit-rebase-interactive))
                    mercit-rebase-interactive-include-selected)
            (setq commit (concat commit "^")))
        (setq args (cons "--root" args)))))
  (when (and commit (not noassert))
    (setq commit (mercit-rebase-interactive-assert
                  commit delay-edit-confirm
                  (--some (string-prefix-p "--rebase-merges" it) args))))
  (if (and commit (not confirm))
      (let ((process-environment process-environment))
        (when editor
          (push (concat "GIT_SEQUENCE_EDITOR="
                        (if (functionp editor)
                            (funcall editor commit)
                          editor))
                process-environment))
        (mercit-run-git-sequencer "rebase" "-i" args
                                 (and (not (member "--root" args)) commit)))
    (mercit-log-select
      `(lambda (commit)
         ;; In some cases (currently just mercit-rebase-remove-commit), "-c
         ;; commentChar=#" is added to the global arguments for git.  Ensure
         ;; that the same happens when we chose the commit via
         ;; mercit-log-select, below.
         (let ((mercit-git-global-arguments (list ,@mercit-git-global-arguments)))
           (mercit-rebase-interactive-1 commit (list ,@args)
             ,message ,editor ,delay-edit-confirm ,noassert)))
      message)))

(defvar mercit--rebase-published-symbol nil)
(defvar mercit--rebase-public-edit-confirmed nil)

(defun mercit-rebase-interactive-assert
    (since &optional delay-edit-confirm rebase-merges)
  (let* ((commit (mercit-rebase--target-commit since))
         (branches (mercit-list-publishing-branches commit)))
    (setq mercit--rebase-public-edit-confirmed
          (delete (mercit-toplevel) mercit--rebase-public-edit-confirmed))
    (when (and branches
               (or (not delay-edit-confirm)
                   ;; The user might have stopped at a published commit
                   ;; merely to add new commits *after* it.  Try not to
                   ;; ask users whether they really want to edit public
                   ;; commits, when they don't actually intend to do so.
                   (not (--all-p (mercit-rev-equal it commit) branches))))
      (let ((m1 "Some of these commits have already been published to ")
            (m2 ".\nDo you really want to modify them"))
        (mercit-confirm (or mercit--rebase-published-symbol 'rebase-published)
          (concat m1 "%s" m2)
          (concat m1 "%i public branches" m2)
          nil branches))
      (push (mercit-toplevel) mercit--rebase-public-edit-confirmed)))
  (if (and (mercit-git-lines "rev-list" "--merges" (concat since "..HEAD"))
           (not rebase-merges))
      (mercit-read-char-case "Proceed despite merge in rebase range?  " nil
        (?c "[c]ontinue" since)
        (?s "[s]elect other" nil)
        (?a "[a]bort" (user-error "Quit")))
    since))

(defun mercit-rebase--target-commit (since)
  (if (string-suffix-p "^" since)
      ;; If SINCE is "REV^", then the user selected
      ;; "REV", which is the first commit that will
      ;; be replaced.  (from^..to] <=> [from..to]
      (substring since 0 -1)
    ;; The "--root" argument is being used.
    since))

;;;###autoload
(defun mercit-rebase-interactive (commit args)
  "Start an interactive rebase sequence."
  (interactive (list (mercit-commit-at-point)
                     (mercit-rebase-arguments)))
  (mercit-rebase-interactive-1 commit args
    "Type %p on a commit to rebase it and all commits above it,"
    nil t))

;;;###autoload
(defun mercit-rebase-autosquash (args)
  "Combine squash and fixup commits with their intended targets."
  (interactive (list (mercit-rebase-arguments)))
  (mercit-rebase-interactive-1 :merge-base
      (nconc (list "--autosquash" "--keep-empty") args)
    "Type %p on a commit to squash into it and then rebase as necessary,"
    "true" nil t))

;;;###autoload
(defun mercit-rebase-edit-commit (commit args)
  "Edit a single older commit using rebase."
  (interactive (list (mercit-commit-at-point)
                     (mercit-rebase-arguments)))
  (mercit-rebase-interactive-1 commit args
    "Type %p on a commit to edit it,"
    (apply-partially #'mercit-rebase--perl-editor 'edit)
    t))

;;;###autoload
(defun mercit-rebase-reword-commit (commit args)
  "Reword a single older commit using rebase."
  (interactive (list (mercit-commit-at-point)
                     (mercit-rebase-arguments)))
  (mercit-rebase-interactive-1 commit args
    "Type %p on a commit to reword its message,"
    (apply-partially #'mercit-rebase--perl-editor 'reword)))

;;;###autoload
(defun mercit-rebase-remove-commit (commit args)
  "Remove a single older commit using rebase."
  (interactive (list (mercit-commit-at-point)
                     (mercit-rebase-arguments)))
  ;; mercit-rebase--perl-editor assumes that the comment character is "#".
  (let ((mercit-git-global-arguments
         (nconc (list "-c" "core.commentChar=#")
                mercit-git-global-arguments)))
    (mercit-rebase-interactive-1 commit args
      "Type %p on a commit to remove it,"
      (apply-partially #'mercit-rebase--perl-editor 'remove)
      nil nil t)))

(defun mercit-rebase--perl-editor (action since)
  (let ((commit (mercit-rev-abbrev (mercit-rebase--target-commit since))))
    (format "%s -i -p -e '++$x if not $x and s/^pick %s/%s %s/'"
            mercit-perl-executable
            commit
            (cl-case action
              (edit   "edit")
              (remove "noop\n# pick")
              (reword "reword")
              (t      (error "unknown action: %s" action)))
            commit)))

;;;###autoload
(defun mercit-rebase-continue (&optional noedit)
  "Restart the current rebasing operation.
In some cases this pops up a commit message buffer for you do
edit.  With a prefix argument the old message is reused as-is."
  (interactive "P")
  (if (mercit-rebase-in-progress-p)
      (if (mercit-anything-unstaged-p t)
          (user-error "Cannot continue rebase with unstaged changes")
        (when (and (mercit-anything-staged-p)
                   (file-exists-p (mercit-git-dir "rebase-merge"))
                   (not (member (mercit-toplevel)
                                mercit--rebase-public-edit-confirmed)))
          (mercit-commit-amend-assert
           (mercit-file-line (mercit-git-dir "rebase-merge/orig-head"))))
        (if noedit
            (with-environment-variables (("GIT_EDITOR" "true"))
              (mercit-run-git-async (mercit--rebase-resume-command) "--continue")
              (set-process-sentinel mercit-this-process
                                    #'mercit-sequencer-process-sentinel)
              mercit-this-process)
          (mercit-run-git-sequencer (mercit--rebase-resume-command) "--continue")))
    (user-error "No rebase in progress")))

;;;###autoload
(defun mercit-rebase-skip ()
  "Skip the current commit and restart the current rebase operation."
  (interactive)
  (unless (mercit-rebase-in-progress-p)
    (user-error "No rebase in progress"))
  (mercit-run-git-sequencer (mercit--rebase-resume-command) "--skip"))

;;;###autoload
(defun mercit-rebase-edit ()
  "Edit the todo list of the current rebase operation."
  (interactive)
  (unless (mercit-rebase-in-progress-p)
    (user-error "No rebase in progress"))
  (mercit-run-git-sequencer "rebase" "--edit-todo"))

;;;###autoload
(defun mercit-rebase-abort ()
  "Abort the current rebase operation, restoring the original branch."
  (interactive)
  (unless (mercit-rebase-in-progress-p)
    (user-error "No rebase in progress"))
  (mercit-confirm 'abort-rebase "Abort this rebase")
  (mercit-run-git (mercit--rebase-resume-command) "--abort"))

(defun mercit-rebase-in-progress-p ()
  "Return t if a rebase is in progress."
  (or (file-exists-p (mercit-git-dir "rebase-merge"))
      (file-exists-p (mercit-git-dir "rebase-apply/onto"))))

(defun mercit--rebase-resume-command ()
  (if (file-exists-p (mercit-git-dir "rebase-recursive")) "rbr" "rebase"))

(defun mercit-rebase--get-state-lines (file)
  (and (mercit-rebase-in-progress-p)
       (mercit-file-line
        (mercit-git-dir
         (concat (if (file-directory-p (mercit-git-dir "rebase-merge"))
                     "rebase-merge/"
                   "rebase-apply/")
                 file)))))

;;; Sections

(defun mercit-insert-sequencer-sequence ()
  "Insert section for the on-going cherry-pick or revert sequence.
If no such sequence is in progress, do nothing."
  (let ((picking (mercit-cherry-pick-in-progress-p)))
    (when (or picking (mercit-revert-in-progress-p))
      (mercit-insert-section (sequence)
        (mercit-insert-heading (if picking "Cherry Picking" "Reverting"))
        (when-let ((lines
                    (cdr (mercit-file-lines (mercit-git-dir "sequencer/todo")))))
          (dolist (line (nreverse lines))
            (when (string-match
                   "^\\(pick\\|revert\\) \\([^ ]+\\) \\(.*\\)$" line)
              (mercit-bind-match-strings (cmd hash msg) line
                (mercit-insert-section (commit hash)
                  (insert (propertize cmd 'font-lock-face 'mercit-sequence-pick)
                          " " (propertize hash 'font-lock-face 'mercit-hash)
                          " " msg "\n"))))))
        (mercit-sequence-insert-sequence
         (mercit-file-line (mercit-git-dir (if picking
                                             "CHERRY_PICK_HEAD"
                                           "REVERT_HEAD")))
         (mercit-file-line (mercit-git-dir "sequencer/head")))
        (insert "\n")))))

(defun mercit-insert-am-sequence ()
  "Insert section for the on-going patch applying sequence.
If no such sequence is in progress, do nothing."
  (when (mercit-am-in-progress-p)
    (mercit-insert-section (rebase-sequence)
      (mercit-insert-heading "Applying patches")
      (let ((patches (nreverse (mercit-rebase-patches)))
            patch commit)
        (while patches
          (setq patch (pop patches))
          (setq commit (mercit-commit-p
                        (cadr (split-string (mercit-file-line patch)))))
          (cond ((and commit patches)
                 (mercit-sequence-insert-commit
                  "pick" commit 'mercit-sequence-pick))
                (patches
                 (mercit-sequence-insert-am-patch
                  "pick" patch 'mercit-sequence-pick))
                (commit
                 (mercit-sequence-insert-sequence commit "ORIG_HEAD"))
                (t
                 (mercit-sequence-insert-am-patch
                  "stop" patch 'mercit-sequence-stop)
                 (mercit-sequence-insert-sequence nil "ORIG_HEAD")))))
      (insert ?\n))))

(defun mercit-sequence-insert-am-patch (type patch face)
  (mercit-insert-section (file patch)
    (let ((title
           (with-temp-buffer
             (insert-file-contents patch nil nil 4096)
             (unless (re-search-forward "^Subject: " nil t)
               (goto-char (point-min)))
             (buffer-substring (point) (line-end-position)))))
      (insert (propertize type 'font-lock-face face)
              ?\s (propertize (file-name-nondirectory patch)
                              'font-lock-face 'mercit-hash)
              ?\s title
              ?\n))))

(defun mercit-insert-rebase-sequence ()
  "Insert section for the on-going rebase sequence.
If no such sequence is in progress, do nothing."
  (when (mercit-rebase-in-progress-p)
    (let* ((interactive (file-directory-p (mercit-git-dir "rebase-merge")))
           (dir  (if interactive "rebase-merge/" "rebase-apply/"))
           (name (thread-first (concat dir "head-name")
                   mercit-git-dir
                   mercit-file-line))
           (onto (thread-first (concat dir "onto")
                   mercit-git-dir
                   mercit-file-line))
           (onto (or (mercit-rev-name onto name)
                     (mercit-rev-name onto "refs/heads/*") onto))
           (name (or (mercit-rev-name name "refs/heads/*") name)))
      (mercit-insert-section (rebase-sequence)
        (mercit-insert-heading (format "Rebasing %s onto %s" name onto))
        (if interactive
            (mercit-rebase-insert-merge-sequence onto)
          (mercit-rebase-insert-apply-sequence onto))
        (insert ?\n)))))

(defun mercit-rebase--todo ()
  "Return `git-rebase-action' instances for remaining rebase actions.
These are ordered in that the same way they'll be sorted in the
status buffer (i.e. the reverse of how they will be applied)."
  (let ((comment-start (or (mercit-get "core.commentChar") "#"))
        lines)
    (with-temp-buffer
      (insert-file-contents (mercit-git-dir "rebase-merge/git-rebase-todo"))
      (while (not (eobp))
        (let ((ln (git-rebase-current-line)))
          (when (oref ln action-type)
            (push ln lines)))
        (forward-line)))
    lines))

(defun mercit-rebase-insert-merge-sequence (onto)
  (dolist (line (mercit-rebase--todo))
    (with-slots (action-type action action-options target) line
      (pcase action-type
        ('commit
         (mercit-sequence-insert-commit action target 'mercit-sequence-pick))
        ((or (or `exec `label)
             (and `merge (guard (not action-options))))
         (insert (propertize action 'font-lock-face 'mercit-sequence-onto) "\s"
                 (propertize target 'font-lock-face 'git-rebase-label) "\n"))
        ('merge
         (if-let ((hash (and (string-match "-[cC] \\([^ ]+\\)" action-options)
                             (match-string 1 action-options))))
             (mercit-insert-section (commit hash)
               (mercit-insert-heading
                 (propertize "merge" 'font-lock-face 'mercit-sequence-pick)
                 "\s"
                 (mercit-format-rev-summary hash) "\n"))
           (error "failed to parse merge message hash"))))))
  (mercit-sequence-insert-sequence
   (mercit-file-line (mercit-git-dir "rebase-merge/stopped-sha"))
   onto
   (and-let* ((lines (mercit-file-lines (mercit-git-dir "rebase-merge/done"))))
     (cadr (split-string (car (last lines)))))))

(defun mercit-rebase-insert-apply-sequence (onto)
  (let ((rewritten
         (--map (car (split-string it))
                (mercit-file-lines (mercit-git-dir "rebase-apply/rewritten"))))
        (stop (mercit-file-line (mercit-git-dir "rebase-apply/original-commit"))))
    (dolist (patch (nreverse (cdr (mercit-rebase-patches))))
      (let ((hash (cadr (split-string (mercit-file-line patch)))))
        (unless (or (member hash rewritten)
                    (equal hash stop))
          (mercit-sequence-insert-commit "pick" hash 'mercit-sequence-pick)))))
  (mercit-sequence-insert-sequence
   (mercit-file-line (mercit-git-dir "rebase-apply/original-commit"))
   onto))

(defun mercit-rebase-patches ()
  (directory-files (mercit-git-dir "rebase-apply") t "^[0-9]\\{4\\}$"))

(defun mercit-sequence-insert-sequence (stop onto &optional orig)
  (let ((head (mercit-rev-parse "HEAD")) done)
    (setq onto (if onto (mercit-rev-parse onto) head))
    (setq done (mercit-git-lines "log" "--format=%H" (concat onto "..HEAD")))
    (when (and stop (not (member (mercit-rev-parse stop) done)))
      (let ((id (mercit-patch-id stop)))
        (--if-let (--first (equal (mercit-patch-id it) id) done)
            (setq stop it)
          (cond
           ((--first (mercit-rev-equal it stop) done)
            ;; The commit's testament has been executed.
            (mercit-sequence-insert-commit "void" stop 'mercit-sequence-drop))
           ;; The faith of the commit is still undecided...
           ((mercit-anything-unmerged-p)
            ;; ...and time travel isn't for the faint of heart.
            (mercit-sequence-insert-commit "join" stop 'mercit-sequence-part))
           ((mercit-anything-modified-p t)
            ;; ...and the dust hasn't settled yet...
            (mercit-sequence-insert-commit
             (let* ((mercit--refresh-cache nil)
                    (staged   (mercit-commit-tree "oO" nil "HEAD"))
                    (unstaged (mercit-commit-worktree "oO" "--reset")))
               (cond
                ;; ...but we could end up at the same tree just by committing.
                ((or (mercit-rev-equal staged   stop)
                     (mercit-rev-equal unstaged stop)) "goal")
                ;; ...but the changes are still there, untainted.
                ((or (equal (mercit-patch-id staged)   id)
                     (equal (mercit-patch-id unstaged) id)) "same")
                ;; ...and some changes are gone and/or others were added.
                (t "work")))
             stop 'mercit-sequence-part))
           ;; The commit is definitely gone...
           ((--first (mercit-rev-equal it stop) done)
            ;; ...but all of its changes are still in effect.
            (mercit-sequence-insert-commit "poof" stop 'mercit-sequence-drop))
           (t
            ;; ...and some changes are gone and/or other changes were added.
            (mercit-sequence-insert-commit "gone" stop 'mercit-sequence-drop)))
          (setq stop nil))))
    (dolist (rev done)
      (apply #'mercit-sequence-insert-commit
             (cond ((equal rev stop)
                    ;; ...but its reincarnation lives on.
                    ;; Or it didn't die in the first place.
                    (list (if (and (equal rev head)
                                   (equal (mercit-patch-id rev)
                                          (mercit-patch-id orig)))
                              "stop" ; We haven't done anything yet.
                            "like")  ; There are new commits.
                          rev (if (equal rev head)
                                  'mercit-sequence-head
                                'mercit-sequence-stop)))
                   ((equal rev head)
                    (list "done" rev 'mercit-sequence-head))
                   (t
                    (list "done" rev 'mercit-sequence-done)))))
    (mercit-sequence-insert-commit "onto" onto
                                  (if (equal onto head)
                                      'mercit-sequence-head
                                    'mercit-sequence-onto))))

(defun mercit-sequence-insert-commit (type hash face)
  (mercit-insert-section (commit hash)
    (mercit-insert-heading
      (propertize type 'font-lock-face face)    "\s"
      (mercit-format-rev-summary hash) "\n")))

;;; _
(provide 'mercit-sequence)
;;; mercit-sequence.el ends here
