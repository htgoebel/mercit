;;; mercit-commit.el --- Create Git commits  -*- lexical-binding:t -*-

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

;; This library implements commands for creating Git commits.  These
;; commands just initiate the commit, support for writing the commit
;; messages is implemented in `git-commit.el'.

;;; Code:

(require 'mercit)
(require 'mercit-sequence)

;;; Options

(defcustom mercit-commit-ask-to-stage 'verbose
  "Whether to ask to stage everything when committing and nothing is staged."
  :package-version '(mercit . "2.3.0")
  :group 'mercit-commands
  :type '(choice (const :tag "Ask" t)
                 (const :tag "Ask showing diff" verbose)
                 (const :tag "Stage without confirmation" stage)
                 (const :tag "Don't ask" nil)))

(defcustom mercit-commit-show-diff t
  "Whether the relevant diff is automatically shown when committing."
  :package-version '(mercit . "2.3.0")
  :group 'mercit-commands
  :type 'boolean)

(defcustom mercit-commit-extend-override-date t
  "Whether using `mercit-commit-extend' changes the committer date."
  :package-version '(mercit . "2.3.0")
  :group 'mercit-commands
  :type 'boolean)

(defcustom mercit-commit-reword-override-date t
  "Whether using `mercit-commit-reword' changes the committer date."
  :package-version '(mercit . "2.3.0")
  :group 'mercit-commands
  :type 'boolean)

(defcustom mercit-commit-squash-confirm t
  "Whether the commit targeted by squash and fixup has to be confirmed.
When non-nil then the commit at point (if any) is used as default
choice, otherwise it has to be confirmed.  This option only
affects `mercit-commit-squash' and `mercit-commit-fixup'.  The
\"instant\" variants always require confirmation because making
an error while using those is harder to recover from."
  :package-version '(mercit . "2.1.0")
  :group 'mercit-commands
  :type 'boolean)

(defcustom mercit-post-commit-hook nil
  "Hook run after creating a commit without the user editing a message.

This hook is run by `mercit-refresh' if `this-command' is a member
of `mercit-post-commit-hook-commands'.  This only includes commands
named `mercit-commit-*' that do *not* require that the user edits
the commit message in a buffer and then finishes by pressing
\\<with-editor-mode-map>\\[with-editor-finish].

Also see `git-commit-post-finish-hook'."
  :package-version '(mercit . "2.90.0")
  :group 'mercit-commands
  :type 'hook)

(defcustom mercit-commit-diff-inhibit-same-window nil
  "Whether to inhibit use of same window when showing diff while committing.

When writing a commit, then a diff of the changes to be committed
is automatically shown.  The idea is that the diff is shown in a
different window of the same frame and for most users that just
works.  In other words most users can completely ignore this
option because its value doesn't make a difference for them.

However for users who configured Emacs to never create a new
window even when the package explicitly tries to do so, then
displaying two new buffers necessarily means that the first is
immediately replaced by the second.  In our case the message
buffer is immediately replaced by the diff buffer, which is of
course highly undesirable.

A workaround is to suppress this user configuration in this
particular case.  Users have to explicitly opt-in by toggling
this option.  We cannot enable the workaround unconditionally
because that again causes issues for other users: if the frame
is too tiny or the relevant settings too aggressive, then the
diff buffer would end up being displayed in a new frame.

Also see https://github.com/mercit/mercit/issues/4132."
  :package-version '(mercit . "3.3.0")
  :group 'mercit-commands
  :type 'boolean)

;;; Popup

;;;###autoload (autoload 'mercit-commit "mercit-commit" nil t)
(transient-define-prefix mercit-commit ()
  "Create a new commit or replace an existing commit."
  :info-manual "(mercit)Initiating a Commit"
  :man-page "git-commit"
  ["Arguments"
   ("-a" "Stage all modified and deleted files"   ("-a" "--all"))
   ("-e" "Allow empty commit"                     "--allow-empty")
   ("-v" "Show diff of changes to be committed"   ("-v" "--verbose"))
   ("-n" "Disable hooks"                          ("-n" "--no-verify"))
   ("-R" "Claim authorship and reset author date" "--reset-author")
   (mercit:--author :description "Override the author")
   (7 "-D" "Override the author date" "--date=" transient-read-date)
   ("-s" "Add Signed-off-by line"                 ("-s" "--signoff"))
   (5 mercit:--gpg-sign)
   (mercit-commit:--reuse-message)]
  [["Create"
    ("c" "Commit"         mercit-commit-create)]
   ["Edit HEAD"
    ("e" "Extend"         mercit-commit-extend)
    ("w" "Reword"         mercit-commit-reword)
    ("a" "Amend"          mercit-commit-amend)
    (6 "n" "Reshelve"     mercit-commit-reshelve)]
   ["Edit"
    ("f" "Fixup"          mercit-commit-fixup)
    ("s" "Squash"         mercit-commit-squash)
    ("A" "Augment"        mercit-commit-augment)
    (6 "x" "Absorb changes" mercit-commit-autofixup)
    (6 "X" "Absorb modules" mercit-commit-absorb-modules)]
   [""
    ("F" "Instant fixup"  mercit-commit-instant-fixup)
    ("S" "Instant squash" mercit-commit-instant-squash)]]
  (interactive)
  (if-let ((buffer (mercit-commit-message-buffer)))
      (switch-to-buffer buffer)
    (transient-setup 'mercit-commit)))

(defun mercit-commit-arguments nil
  (transient-args 'mercit-commit))

(transient-define-argument mercit-commit:--reuse-message ()
  :description "Reuse commit message"
  :class 'transient-option
  :shortarg "-C"
  :argument "--reuse-message="
  :reader #'mercit-read-reuse-message
  :history-key 'mercit-revision-history)

(defun mercit-read-reuse-message (prompt &optional default history)
  (mercit-completing-read prompt (mercit-list-refnames)
                         nil nil nil history
                         (or default
                             (and (mercit-rev-verify "ORIG_HEAD")
                                  "ORIG_HEAD"))))

;;; Commands

;;;###autoload
(defun mercit-commit-create (&optional args)
  "Create a new commit on `HEAD'.
With a prefix argument, amend to the commit at `HEAD' instead.
\n(git commit [--amend] ARGS)"
  (interactive (if current-prefix-arg
                   (list (cons "--amend" (mercit-commit-arguments)))
                 (list (mercit-commit-arguments))))
  (when (member "--all" args)
    (setq this-command 'mercit-commit--all))
  (when (setq args (mercit-commit-assert args))
    (let ((default-directory (mercit-toplevel)))
      (mercit-run-git-with-editor "commit" args))))

;;;###autoload
(defun mercit-commit-amend (&optional args)
  "Amend the last commit.
\n(git commit --amend ARGS)"
  (interactive (list (mercit-commit-arguments)))
  (mercit-commit-amend-assert)
  (mercit-run-git-with-editor "commit" "--amend" args))

;;;###autoload
(defun mercit-commit-extend (&optional args override-date)
  "Amend the last commit, without editing the message.

With a prefix argument keep the committer date, otherwise change
it.  The option `mercit-commit-extend-override-date' can be used
to inverse the meaning of the prefix argument.  \n(git commit
--amend --no-edit)"
  (interactive (list (mercit-commit-arguments)
                     (if current-prefix-arg
                         (not mercit-commit-extend-override-date)
                       mercit-commit-extend-override-date)))
  (when (setq args (mercit-commit-assert args))
    (mercit-commit-amend-assert)
    (if override-date
        (mercit-run-git-with-editor "commit" "--amend" "--no-edit" args)
      (with-environment-variables
          (("GIT_COMMITTER_DATE" (mercit-rev-format "%cD")))
        (mercit-run-git-with-editor "commit" "--amend" "--no-edit" args)))))

;;;###autoload
(defun mercit-commit-reword (&optional args override-date)
  "Reword the last commit, ignoring staged changes.

With a prefix argument keep the committer date, otherwise change
it.  The option `mercit-commit-reword-override-date' can be used
to inverse the meaning of the prefix argument.

Non-interactively respect the optional OVERRIDE-DATE argument
and ignore the option.
\n(git commit --amend --only)"
  (interactive (list (mercit-commit-arguments)
                     (if current-prefix-arg
                         (not mercit-commit-reword-override-date)
                       mercit-commit-reword-override-date)))
  (mercit-commit-amend-assert)
  (cl-pushnew "--allow-empty" args :test #'equal)
  (if override-date
      (mercit-run-git-with-editor "commit" "--amend" "--only" args)
    (with-environment-variables
        (("GIT_COMMITTER_DATE" (mercit-rev-format "%cD")))
      (mercit-run-git-with-editor "commit" "--amend" "--only" args))))

;;;###autoload
(defun mercit-commit-fixup (&optional commit args)
  "Create a fixup commit.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `mercit-commit-squash-confirm'."
  (interactive (list (mercit-commit-at-point)
                     (mercit-commit-arguments)))
  (mercit-commit-squash-internal "--fixup" commit args))

;;;###autoload
(defun mercit-commit-squash (&optional commit args)
  "Create a squash commit, without editing the squash message.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `mercit-commit-squash-confirm'.

If you want to immediately add a message to the squash commit,
then use `mercit-commit-augment' instead of this command."
  (interactive (list (mercit-commit-at-point)
                     (mercit-commit-arguments)))
  (mercit-commit-squash-internal "--squash" commit args))

;;;###autoload
(defun mercit-commit-augment (&optional commit args)
  "Create a squash commit, editing the squash message.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `mercit-commit-squash-confirm'."
  (interactive (list (mercit-commit-at-point)
                     (mercit-commit-arguments)))
  (mercit-commit-squash-internal "--squash" commit args nil t))

;;;###autoload
(defun mercit-commit-instant-fixup (&optional commit args)
  "Create a fixup commit targeting COMMIT and instantly rebase."
  (interactive (list (mercit-commit-at-point)
                     (mercit-commit-arguments)))
  (mercit-commit-squash-internal "--fixup" commit args t))

;;;###autoload
(defun mercit-commit-instant-squash (&optional commit args)
  "Create a squash commit targeting COMMIT and instantly rebase."
  (interactive (list (mercit-commit-at-point)
                     (mercit-commit-arguments)))
  (mercit-commit-squash-internal "--squash" commit args t))

(defun mercit-commit-squash-internal
    (option commit &optional args rebase edit confirmed)
  (when-let ((args (mercit-commit-assert args (not edit))))
    (when commit
      (when (and rebase (not (mercit-rev-ancestor-p commit "HEAD")))
        (mercit-read-char-case
            (format "%s isn't an ancestor of HEAD.  " commit) nil
          (?c "[c]reate without rebasing" (setq rebase nil))
          (?s "[s]elect other"            (setq commit nil))
          (?a "[a]bort"                   (user-error "Quit")))))
    (when commit
      (setq commit (mercit-rebase-interactive-assert commit t)))
    (if (and commit
             (or confirmed
                 (not (or rebase
                          current-prefix-arg
                          mercit-commit-squash-confirm))))
        (let ((mercit-commit-show-diff nil))
          (push (concat option "=" commit) args)
          (unless edit
            (push "--no-edit" args))
          (if rebase
              (mercit-with-editor
                (mercit-call-git
                 "commit" "--no-gpg-sign"
                 (-remove-first
                  (apply-partially #'string-prefix-p "--gpg-sign=")
                  args)))
            (mercit-run-git-with-editor "commit" args))
          t) ; The commit was created; used by below lambda.
      (let ((winconf (and mercit-commit-show-diff
                          (current-window-configuration))))
        (mercit-log-select
          (lambda (commit)
            (when (and (mercit-commit-squash-internal option commit args
                                                     rebase edit t)
                       rebase)
              (mercit-commit-amend-assert commit)
              (mercit-rebase-interactive-1 commit
                  (list "--autosquash" "--autostash" "--keep-empty")
                "" "true" nil t))
            (when winconf
              (set-window-configuration winconf)))
          (format "Type %%p on a commit to %s into it,"
                  (substring option 2))
          nil nil nil commit))
      (when mercit-commit-show-diff
        (let ((mercit-display-buffer-noselect t))
          (apply #'mercit-diff-staged nil (mercit-diff-arguments)))))))

(defun mercit-commit-amend-assert (&optional commit)
  (--when-let (mercit-list-publishing-branches commit)
    (let ((m1 "This commit has already been published to ")
          (m2 ".\nDo you really want to modify it"))
      (mercit-confirm 'amend-published
        (concat m1 "%s" m2)
        (concat m1 "%i public branches" m2)
        nil it))))

(defun mercit-commit-assert (args &optional strict)
  (cond
   ((or (mercit-anything-staged-p)
        (and (mercit-anything-unstaged-p)
             ;; ^ Everything of nothing is still nothing.
             (member "--all" args))
        (and (not strict)
             ;; ^ For amend variants that don't make sense otherwise.
             (or (member "--amend" args)
                 (member "--allow-empty" args)
                 (member "--reset-author" args)
                 (member "--signoff" args)
                 (transient-arg-value "--author=" args)
                 (transient-arg-value "--date=" args))))
    (or args (list "--")))
   ((and (mercit-rebase-in-progress-p)
         (not (mercit-anything-unstaged-p))
         (y-or-n-p "Nothing staged.  Continue in-progress rebase? "))
    (setq this-command #'mercit-rebase-continue)
    (mercit-run-git-sequencer "rebase" "--continue")
    nil)
   ((and (file-exists-p (mercit-git-dir "MERGE_MSG"))
         (not (mercit-anything-unstaged-p)))
    (or args (list "--")))
   ((not (mercit-anything-unstaged-p))
    (user-error "Nothing staged (or unstaged)"))
   (mercit-commit-ask-to-stage
    (when (eq mercit-commit-ask-to-stage 'verbose)
      (mercit-diff-unstaged))
    (prog1 (when (or (eq mercit-commit-ask-to-stage 'stage)
                     (y-or-n-p
                      "Nothing staged.  Commit all uncommitted changes? "))
             (setq this-command 'mercit-commit--all)
             (cons "--all" (or args (list "--"))))
      (when (and (eq mercit-commit-ask-to-stage 'verbose)
                 (derived-mode-p 'mercit-diff-mode))
        (mercit-mode-bury-buffer))))
   (t
    (user-error "Nothing staged"))))

(defvar mercit--reshelve-history nil)

;;;###autoload
(defun mercit-commit-reshelve (date update-author &optional args)
  "Change the committer date and possibly the author date of `HEAD'.

The current time is used as the initial minibuffer input and the
original author or committer date is available as the previous
history element.

Both the author and the committer dates are changes, unless one
of the following is true, in which case only the committer date
is updated:
- You are not the author of the commit that is being reshelved.
- The command was invoked with a prefix argument.
- Non-interactively if UPDATE-AUTHOR is nil."
  (interactive
   (let ((update-author (and (mercit-rev-author-p "HEAD")
                             (not current-prefix-arg))))
     (push (mercit-rev-format (if update-author "%ad" "%cd") "HEAD"
                             (concat "--date=format:%F %T %z"))
           mercit--reshelve-history)
     (list (read-string (if update-author
                            "Change author and committer dates to: "
                          "Change committer date to: ")
                        (cons (format-time-string "%F %T %z") 17)
                        'mercit--reshelve-history)
           update-author
           (mercit-commit-arguments))))
  (with-environment-variables (("GIT_COMMITTER_DATE" date))
    (mercit-run-git "commit" "--amend" "--no-edit"
                   (and update-author (concat "--date=" date))
                   args)))

;;;###autoload
(defun mercit-commit-absorb-modules (phase commit)
  "Spread modified modules across recent commits."
  (interactive (list 'select (mercit-get-upstream-branch)))
  (let ((modules (mercit-list-modified-modules)))
    (unless modules
      (user-error "There are no modified modules that could be absorbed"))
    (when commit
      (setq commit (mercit-rebase-interactive-assert commit t)))
    (if (and commit (eq phase 'run))
        (progn
          (dolist (module modules)
            (when-let ((msg (mercit-git-string
                             "log" "-1" "--format=%s"
                             (concat commit "..") "--" module)))
              (mercit-git "commit" "-m" (concat "fixup! " msg)
                         "--only" "--" module)))
          (mercit-refresh)
          t)
      (mercit-log-select
        (lambda (commit)
          (mercit-commit-absorb-modules 'run commit))
        nil nil nil nil commit))))

;;;###autoload (autoload 'mercit-commit-absorb "mercit-commit" nil t)
(transient-define-prefix mercit-commit-absorb (phase commit args)
  "Spread staged changes across recent commits.
With a prefix argument use a transient command to select infix
arguments.  This command requires git-absorb executable, which
is available from https://github.com/tummychow/git-absorb.
See `mercit-commit-autofixup' for an alternative implementation."
  ["Arguments"
   ("-f" "Skip safety checks"       ("-f" "--force"))
   ("-v" "Display more output"      ("-v" "--verbose"))]
  ["Actions"
   ("x"  "Absorb" mercit-commit-absorb)]
  (interactive (if current-prefix-arg
                   (list 'transient nil nil)
                 (list 'select
                       (mercit-get-upstream-branch)
                       (transient-args 'mercit-commit-absorb))))
  (if (eq phase 'transient)
      (transient-setup 'mercit-commit-absorb)
    (unless (mercit-git-executable-find "git-absorb")
      (user-error "This command requires the git-absorb executable, which %s"
                  "is available from https://github.com/tummychow/git-absorb"))
    (unless (mercit-anything-staged-p)
      (if (mercit-anything-unstaged-p)
          (if (y-or-n-p "Nothing staged.  Absorb all unstaged changes? ")
              (mercit-with-toplevel
                (mercit-run-git "add" "-u" "."))
            (user-error "Abort"))
        (user-error "There are no changes that could be absorbed")))
    (when commit
      (setq commit (mercit-rebase-interactive-assert commit t)))
    (if (and commit (eq phase 'run))
        (progn (mercit-run-git-async "absorb" "-v" args "-b" commit) t)
      (mercit-log-select
        (lambda (commit)
          (with-no-warnings ; about non-interactive use
            (mercit-commit-absorb 'run commit args)))
        nil nil nil nil commit))))

;;;###autoload (autoload 'mercit-commit-autofixup "mercit-commit" nil t)
(transient-define-prefix mercit-commit-autofixup (phase commit args)
  "Spread staged or unstaged changes across recent commits.

If there are any staged then spread only those, otherwise
spread all unstaged changes. With a prefix argument use a
transient command to select infix arguments.

This command requires the git-autofixup script, which is
available from https://github.com/torbiak/git-autofixup.
See `mercit-commit-absorb' for an alternative implementation."
  ["Arguments"
   (mercit-autofixup:--context)
   (mercit-autofixup:--strict)]
  ["Actions"
   ("x"  "Absorb" mercit-commit-autofixup)]
  (interactive (if current-prefix-arg
                   (list 'transient nil nil)
                 (list 'select
                       (mercit-get-upstream-branch)
                       (transient-args 'mercit-commit-autofixup))))
  (if (eq phase 'transient)
      (transient-setup 'mercit-commit-autofixup)
    (unless (mercit-git-executable-find "git-autofixup")
      (user-error "This command requires the git-autofixup script, which %s"
                  "is available from https://github.com/torbiak/git-autofixup"))
    (unless (mercit-anything-modified-p)
      (user-error "There are no changes that could be absorbed"))
    (when commit
      (setq commit (mercit-rebase-interactive-assert commit t)))
    (if (and commit (eq phase 'run))
        (progn (mercit-run-git-async "autofixup" "-vv" args commit) t)
      (mercit-log-select
        (lambda (commit)
          (with-no-warnings ; about non-interactive use
            (mercit-commit-autofixup 'run commit args)))
        nil nil nil nil commit))))

(transient-define-argument mercit-autofixup:--context ()
  :description "Diff context lines"
  :class 'transient-option
  :shortarg "-c"
  :argument "--context="
  :reader #'transient-read-number-N0)

(transient-define-argument mercit-autofixup:--strict ()
  :description "Strictness"
  :class 'transient-option
  :shortarg "-s"
  :argument "--strict="
  :reader #'transient-read-number-N0)

(defvar mercit-post-commit-hook-commands
  '(mercit-commit-extend
    mercit-commit-fixup
    mercit-commit-augment
    mercit-commit-instant-fixup
    mercit-commit-instant-squash))

(defun mercit-run-post-commit-hook ()
  (when (and (not this-command)
             (memq last-command mercit-post-commit-hook-commands))
    (run-hooks 'mercit-post-commit-hook)))

;;; Pending Diff

(defun mercit-commit-diff ()
  (mercit-repository-local-set 'this-commit-command
                              (if (eq this-command 'with-editor-finish)
                                  'mercit-commit--rebase
                                last-command))
  (when (and git-commit-mode mercit-commit-show-diff)
    (when-let ((diff-buffer (mercit-get-mode-buffer 'mercit-diff-mode)))
      ;; This window just started displaying the commit message
      ;; buffer.  Without this that buffer would immediately be
      ;; replaced with the diff buffer.  See #2632.
      (unrecord-window-buffer nil diff-buffer))
    (message "Diffing changes to be committed (C-g to abort diffing)")
    (let ((inhibit-quit nil))
      (condition-case nil
          (mercit-commit-diff-1)
        (quit)))))

(defun mercit-commit-diff-1 ()
  (let ((rev nil)
        (arg "--cached")
        (command (mercit-repository-local-get 'this-commit-command))
        (staged (mercit-anything-staged-p))
        (unstaged
         ;; Escape $GIT_DIR because `mercit-anything-unstaged-p'
         ;; requires a working tree.
         (mercit-with-toplevel
           (mercit-anything-unstaged-p)))
        (squash (let ((f (mercit-git-dir "rebase-merge/rewritten-pending")))
                  (and (file-exists-p f) (length (mercit-file-lines f)))))
        (noalt nil))
    (pcase (list staged unstaged command)
      ((and `(,_ ,_ mercit-commit--rebase)
            (guard (integerp squash)))
       (setq rev (format "HEAD~%s" squash)))
      (`(,_ ,_ mercit-commit-amend)
       (setq rev "HEAD^"))
      ((or `(,_ ,_ mercit-commit-reword)
           `(nil nil ,_))
       (setq rev "HEAD^..HEAD")
       (setq arg nil))
      (`(,_ t mercit-commit--all)
       (setq rev "HEAD")
       (setq arg nil))
      (`(nil t handle-switch-frame)
       ;; Either --all or --allow-empty. Assume it is the former.
       (setq rev "HEAD")
       (setq arg nil)))
    (cond
     ((not
       (and (eq this-command 'mercit-diff-while-committing)
            (and-let* ((buf (mercit-get-mode-buffer
                             'mercit-diff-mode nil 'selected)))
              (and (equal rev (buffer-local-value 'mercit-buffer-range buf))
                   (equal arg (buffer-local-value 'mercit-buffer-typearg buf)))))))
     ((eq command 'mercit-commit-amend)
      (setq rev nil))
     ((or squash (file-exists-p (mercit-git-dir "rebase-merge/amend")))
      (setq rev "HEAD^"))
     (t
      (message "No alternative diff while committing")
      (setq noalt t)))
    (unless noalt
      (let ((mercit-inhibit-save-previous-winconf 'unset)
            (mercit-display-buffer-noselect t)
            (display-buffer-overriding-action
             display-buffer-overriding-action))
        (when mercit-commit-diff-inhibit-same-window
          (setq display-buffer-overriding-action
                '(nil (inhibit-same-window . t))))
        (mercit-diff-setup-buffer rev arg (car (mercit-diff-arguments)) nil)))))

(add-hook 'server-switch-hook #'mercit-commit-diff)
(add-hook 'with-editor-filter-visit-hook #'mercit-commit-diff)

(add-to-list 'with-editor-server-window-alist
             (cons git-commit-filename-regexp #'switch-to-buffer))

(defun mercit-commit--reset-command ()
  (mercit-repository-local-delete 'this-commit-command))

;;; Message Utilities

(defun mercit-commit-message-buffer ()
  (let* ((find-file-visit-truename t) ; git uses truename of COMMIT_EDITMSG
         (topdir (mercit-toplevel)))
    (--first (equal topdir (with-current-buffer it
                             (and git-commit-mode (mercit-toplevel))))
             (append (buffer-list (selected-frame))
                     (buffer-list)))))

(defvar mercit-commit-add-log-insert-function #'mercit-commit-add-log-insert
  "Used by `mercit-commit-add-log' to insert a single entry.")

(defun mercit-commit-add-log ()
  "Add a stub for the current change into the commit message buffer.
If no commit is in progress, then initiate it.  Use the function
specified by variable `mercit-commit-add-log-insert-function' to
actually insert the entry."
  (interactive)
  (pcase-let* ((hunk (and (mercit-section-match 'hunk)
                          (mercit-current-section)))
               (log  (mercit-commit-message-buffer))
               (`(,buf ,pos) (mercit-diff-visit-file--noselect)))
    (unless log
      (unless (mercit-commit-assert nil)
        (user-error "Abort"))
      (mercit-commit-create)
      (while (not (setq log (mercit-commit-message-buffer)))
        (sit-for 0.01)))
    (mercit--with-temp-position buf pos
      (funcall mercit-commit-add-log-insert-function log
               (mercit-file-relative-name)
               (and hunk (add-log-current-defun))))))

(defun mercit-commit-add-log-insert (buffer file defun)
  (with-current-buffer buffer
    (undo-boundary)
    (goto-char (point-max))
    (while (re-search-backward (concat "^" comment-start) nil t))
    (save-restriction
      (narrow-to-region (point-min) (point))
      (cond ((re-search-backward (format "* %s\\(?: (\\([^)]+\\))\\)?: " file)
                                 nil t)
             (when (equal (match-string 1) defun)
               (setq defun nil))
             (re-search-forward ": "))
            (t
             (when (re-search-backward "^[\\*(].+\n" nil t)
               (goto-char (match-end 0)))
             (while (re-search-forward "^[^\\*\n].*\n" nil t))
             (if defun
                 (progn (insert (format "* %s (%s): \n" file defun))
                        (setq defun nil))
               (insert (format "* %s: \n" file)))
             (backward-char)
             (unless (looking-at "\n[\n\\']")
               (insert ?\n)
               (backward-char))))
      (when defun
        (forward-line)
        (let ((limit (save-excursion
                       (and (re-search-forward "^\\*" nil t)
                            (point)))))
          (unless (or (looking-back (format "(%s): " defun)
                                    (line-beginning-position))
                      (re-search-forward (format "^(%s): " defun) limit t))
            (while (re-search-forward "^[^\\*\n].*\n" limit t))
            (insert (format "(%s): \n" defun))
            (backward-char)))))))

;;; _
(provide 'mercit-commit)
;;; mercit-commit.el ends here
