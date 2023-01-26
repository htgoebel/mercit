;;; mercit-extras.el --- Additional functionality for Mercit  -*- lexical-binding:t -*-

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

;; Additional functionality for Mercit.

;;; Code:

(require 'mercit)

;; For `mercit-do-async-shell-command'.
(declare-function dired-read-shell-command "dired-aux" (prompt arg files))
;; For `mercit-project-status'.
(declare-function vc-git-command "vc-git"
                  (buffer okstatus file-or-list &rest flags))

(defvar ido-exit)
(defvar ido-fallback)
(defvar project-prefix-map)
(defvar project-switch-commands)

(defgroup mercit-extras nil
  "Additional functionality for Mercit."
  :group 'mercit-extensions)

;;; Mercurial Tools
;;;; Git-Mergetool

;;;###autoload (autoload 'mercit-git-mergetool "mercit-extras" nil t)
(transient-define-prefix mercit-git-mergetool (file args &optional transient)
  "Resolve conflicts in FILE using \"git mergetool --gui\".
With a prefix argument allow changing ARGS using a transient
popup.  See info node `(mercit) Ediffing' for information about
alternative commands."
  :man-page "git-mergetool"
  ["Settings"
   ("-t" mercit-git-mergetool:--tool)
   ("=t" mercit-merge.guitool)
   ("=T" mercit-merge.tool)
   ("-r" mercit-mergetool.hideResolved)
   ("-b" mercit-mergetool.keepBackup)
   ("-k" mercit-mergetool.keepTemporaries)
   ("-w" mercit-mergetool.writeToTemp)]
  ["Actions"
   (" m" "Invoke mergetool" mercit-git-mergetool)]
  (interactive
   (if (and (not (eq transient-current-prefix 'mercit-git-mergetool))
            current-prefix-arg)
       (list nil nil t)
     (list (mercit-read-unmerged-file "Resolve")
           (transient-args 'mercit-git-mergetool))))
  (if transient
      (transient-setup 'mercit-git-mergetool)
    (mercit-run-git-async "mergetool" "--gui" args "--" file)))

(transient-define-infix mercit-git-mergetool:--tool ()
  :description "Override mergetool"
  :class 'transient-option
  :shortarg "-t"
  :argument "--tool="
  :reader #'mercit--read-mergetool)

(transient-define-infix mercit-merge.guitool ()
  :class 'mercit--git-variable
  :variable "merge.guitool"
  :global t
  :reader #'mercit--read-mergetool)

(transient-define-infix mercit-merge.tool ()
  :class 'mercit--git-variable
  :variable "merge.tool"
  :global t
  :reader #'mercit--read-mergetool)

(defun mercit--read-mergetool (prompt _initial-input history)
  (let ((choices nil)
        (lines (cdr (mercit-git-lines "mergetool" "--tool-help"))))
    (while (string-prefix-p "\t\t" (car lines))
      (push (substring (pop lines) 2) choices))
    (setq choices (nreverse choices))
    (mercit-completing-read (or prompt "Select mergetool")
                           choices nil t nil history)))

(transient-define-infix mercit-mergetool.hideResolved ()
  :class 'mercit--git-variable:boolean
  :variable "mergetool.hideResolved"
  :default "false"
  :global t)

(transient-define-infix mercit-mergetool.keepBackup ()
  :class 'mercit--git-variable:boolean
  :variable "mergetool.keepBackup"
  :default "true"
  :global t)

(transient-define-infix mercit-mergetool.keepTemporaries ()
  :class 'mercit--git-variable:boolean
  :variable "mergetool.keepTemporaries"
  :default "false"
  :global t)

(transient-define-infix mercit-mergetool.writeToTemp ()
  :class 'mercit--git-variable:boolean
  :variable "mergetool.writeToTemp"
  :default "false"
  :global t)

;;;; Git-Gui

;;;###autoload
(defun mercit-run-git-gui-blame (commit filename &optional linenum)
  "Run `git gui blame' on the given FILENAME and COMMIT.
Interactively run it for the current file and the `HEAD', with a
prefix or when the current file cannot be determined let the user
choose.  When the current buffer is visiting FILENAME instruct
blame to center around the line point is on."
  (interactive
   (let (revision filename)
     (when (or current-prefix-arg
               (not (setq revision "HEAD"
                          filename (mercit-file-relative-name nil 'tracked))))
       (setq revision (mercit-read-branch-or-commit "Blame from revision"))
       (setq filename (mercit-read-file-from-rev revision "Blame file")))
     (list revision filename
           (and (equal filename
                       (ignore-errors
                         (mercit-file-relative-name buffer-file-name)))
                (line-number-at-pos)))))
  (mercit-with-toplevel
    (mercit-process-git 0 "gui" "blame"
                       (and linenum (list (format "--line=%d" linenum)))
                       commit
                       filename)))

;;;; Gitk

(defcustom mercit-gitk-executable
  (or (and (eq system-type 'windows-nt)
           (let ((exe (mercit-git-string
                       "-c" "alias.X=!x() { which \"$1\" | cygpath -mf -; }; x"
                       "X" "gitk.exe")))
             (and exe (file-executable-p exe) exe)))
      (executable-find "gitk") "gitk")
  "The Gitk executable."
  :group 'mercit-extras
  :set-after '(mercit-git-executable)
  :type 'string)

;;;###autoload
(defun mercit-run-git-gui ()
  "Run `git gui' for the current git repository."
  (interactive)
  (mercit-with-toplevel (mercit-process-git 0 "gui")))

;;;###autoload
(defun mercit-run-gitk ()
  "Run `gitk' in the current repository."
  (interactive)
  (mercit-process-file mercit-gitk-executable nil 0))

;;;###autoload
(defun mercit-run-gitk-branches ()
  "Run `gitk --branches' in the current repository."
  (interactive)
  (mercit-process-file mercit-gitk-executable nil 0 nil "--branches"))

;;;###autoload
(defun mercit-run-gitk-all ()
  "Run `gitk --all' in the current repository."
  (interactive)
  (mercit-process-file mercit-gitk-executable nil 0 nil "--all"))

;;; Emacs Tools

;;;###autoload
(defun ido-enter-mercit-status ()
  "Drop into `mercit-status' from file switching.

This command does not work in Emacs 26.1.
See https://github.com/mercit/mercit/issues/3634
and https://debbugs.gnu.org/cgi/bugreport.cgi?bug=31707.

To make this command available use something like:

  (add-hook \\='ido-setup-hook
            (lambda ()
              (define-key ido-completion-map
                (kbd \"C-x g\") \\='ido-enter-mercit-status)))

Starting with Emacs 25.1 the Ido keymaps are defined just once
instead of every time Ido is invoked, so now you can modify it
like pretty much every other keymap:

  (define-key ido-common-completion-map
    (kbd \"C-x g\") \\='ido-enter-mercit-status)"
  (interactive)
  (setq ido-exit 'fallback)
  (setq ido-fallback #'mercit-status)                ; for Emacs >= 26.2
  (with-no-warnings (setq fallback #'mercit-status)) ; for Emacs 25
  (exit-minibuffer))

;;;###autoload
(defun mercit-project-status ()
  "Run `mercit-status' in the current project's root."
  (interactive)
  (if (fboundp 'project-root)
      (mercit-status-setup-buffer (project-root (project-current t)))
    (user-error "`mercit-project-status' requires `project' 0.3.0 or greater")))

(defvar mercit-bind-mercit-project-status t
  "Whether to bind \"m\" to `mercit-project-status' in `project-prefix-map'.
If so, then an entry is added to `project-switch-commands' as
well.  If you want to use another key, then you must set this
to nil before loading Mercit to prevent \"m\" from being bound.")

(with-eval-after-load 'project
  ;; Only more recent versions of project.el have `project-prefix-map' and
  ;; `project-switch-commands', though project.el is available in Emacs 25.
  (when (and mercit-bind-mercit-project-status
             (boundp 'project-prefix-map)
             ;; Only modify if it hasn't already been modified.
             (equal project-switch-commands
                    (eval (car (get 'project-switch-commands 'standard-value))
                          t)))
    (define-key project-prefix-map "m" #'mercit-project-status)
    (add-to-list 'project-switch-commands '(mercit-project-status "Mercit") t)))

;;;###autoload
(defun mercit-dired-jump (&optional other-window)
  "Visit file at point using Dired.
With a prefix argument, visit in another window.  If there
is no file at point, then instead visit `default-directory'."
  (interactive "P")
  (dired-jump other-window
              (and-let* ((file (mercit-file-at-point)))
                (expand-file-name (if (file-directory-p file)
                                      (file-name-as-directory file)
                                    file)))))

;;;###autoload
(defun mercit-dired-log (&optional follow)
  "Show log for all marked files, or the current file."
  (interactive "P")
  (if-let ((topdir (mercit-toplevel default-directory)))
      (let ((args (car (mercit-log-arguments)))
            (files (compat-call dired-get-marked-files
                                nil nil #'mercit-file-tracked-p nil
                                "No marked file is being tracked by Mercurial")))
        (when (and follow
                   (not (member "--follow" args))
                   (not (cdr files)))
          (push "--follow" args))
        (mercit-log-setup-buffer
         (list (or (mercit-get-current-branch) "HEAD"))
         args
         (let ((default-directory topdir))
           (mapcar #'file-relative-name files))
         mercit-log-buffer-file-locked))
    (mercit--not-inside-repository-error)))

;;;###autoload
(defun mercit-dired-am-apply-patches (repo &optional arg)
  "In Dired, apply the marked (or next ARG) files as patches.
If inside a repository, then apply in that.  Otherwise prompt
for a repository."
  (interactive (list (or (mercit-toplevel)
                         (mercit-read-repository t))
                     current-prefix-arg))
  (let ((files (compat-call dired-get-marked-files nil arg nil nil t)))
    (mercit-status-setup-buffer repo)
    (mercit-am-apply-patches files)))

;;;###autoload
(defun mercit-do-async-shell-command (file)
  "Open FILE with `dired-do-async-shell-command'.
Interactively, open the file at point."
  (interactive (list (or (mercit-file-at-point)
                         (completing-read "Act on file: "
                                          (mercit-list-files)))))
  (require 'dired-aux)
  (dired-do-async-shell-command
   (dired-read-shell-command "& on %s: " current-prefix-arg (list file))
   nil (list file)))

;;; Shift Selection

(defun mercit--turn-on-shift-select-mode-p ()
  (and shift-select-mode
       this-command-keys-shift-translated
       (not mark-active)
       (not (eq (car-safe transient-mark-mode) 'only))))

;;;###autoload
(defun mercit-previous-line (&optional arg try-vscroll)
  "Like `previous-line' but with Mercit-specific shift-selection.

Mercit's selection mechanism is based on the region but selects an
area that is larger than the region.  This causes `previous-line'
when invoked while holding the shift key to move up one line and
thereby select two lines.  When invoked inside a hunk body this
command does not move point on the first invocation and thereby
it only selects a single line.  Which inconsistency you prefer
is a matter of preference."
  (declare (interactive-only
            "use `forward-line' with negative argument instead."))
  (interactive "p\np")
  (unless arg (setq arg 1))
  (let ((stay (or (mercit-diff-inside-hunk-body-p)
                  (mercit-section-position-in-heading-p))))
    (if (and stay (= arg 1) (mercit--turn-on-shift-select-mode-p))
        (push-mark nil nil t)
      (with-no-warnings
        (handle-shift-selection)
        (previous-line (if stay (max (1- arg) 1) arg) try-vscroll)))))

;;;###autoload
(defun mercit-next-line (&optional arg try-vscroll)
  "Like `next-line' but with Mercit-specific shift-selection.

Mercit's selection mechanism is based on the region but selects
an area that is larger than the region.  This causes `next-line'
when invoked while holding the shift key to move down one line
and thereby select two lines.  When invoked inside a hunk body
this command does not move point on the first invocation and
thereby it only selects a single line.  Which inconsistency you
prefer is a matter of preference."
  (declare (interactive-only forward-line))
  (interactive "p\np")
  (unless arg (setq arg 1))
  (let ((stay (or (mercit-diff-inside-hunk-body-p)
                  (mercit-section-position-in-heading-p))))
    (if (and stay (= arg 1) (mercit--turn-on-shift-select-mode-p))
        (push-mark nil nil t)
      (with-no-warnings
        (handle-shift-selection)
        (next-line (if stay (max (1- arg) 1) arg) try-vscroll)))))

;;; Clean

;;;###autoload
(defun mercit-clean (&optional arg)
  "Remove untracked files from the working tree.
With a prefix argument also remove ignored files,
with two prefix arguments remove ignored files only.
\n(git clean -f -d [-x|-X])"
  (interactive "p")
  (when (yes-or-no-p (format "Remove %s files? "
                             (pcase arg
                               (1 "untracked")
                               (4 "untracked and ignored")
                               (_ "ignored"))))
    (mercit-wip-commit-before-change)
    (mercit-run-git "clean" "-f" "-d" (pcase arg (4 "-x") (16 "-X")))))

(put 'mercit-clean 'disabled t)

;;; ChangeLog

;;;###autoload
(defun mercit-generate-changelog (&optional amending)
  "Insert ChangeLog entries into the current buffer.

The entries are generated from the diff being committed.
If prefix argument, AMENDING, is non-nil, include changes
in HEAD as well as staged changes in the diff to check."
  (interactive "P")
  (unless (mercit-commit-message-buffer)
    (user-error "No commit in progress"))
  (require 'diff-mode) ; `diff-add-log-current-defuns'.
  (require 'vc-git)    ; `vc-git-diff'.
  (require 'add-log)   ; `change-log-insert-entries'.
  (cond
   ((and (fboundp 'change-log-insert-entries)
         (fboundp 'diff-add-log-current-defuns))
    (setq default-directory
          (if (and (file-regular-p "gitdir")
                   (not (mercit-git-true "rev-parse" "--is-inside-work-tree"))
                   (mercit-git-true "rev-parse" "--is-inside-git-dir"))
              (file-name-directory (mercit-file-line "gitdir"))
            (mercit-toplevel)))
    (let ((rev1 (if amending "HEAD^1" "HEAD"))
          (rev2 nil))
      ;; Mercit may have updated the files without notifying vc, but
      ;; `diff-add-log-current-defuns' relies on vc being up-to-date.
      (mapc #'vc-file-clearprops (mercit-staged-files))
      (change-log-insert-entries
       (with-temp-buffer
         (vc-git-command (current-buffer) 1 nil
                         "diff-index" "--exit-code" "--patch"
                         (and (mercit-anything-staged-p) "--cached")
                         rev1 "--")
         ;; `diff-find-source-location' consults these vars.
         (defvar diff-vc-revisions)
         (setq-local diff-vc-revisions (list rev1 rev2))
         (setq-local diff-vc-backend 'Mercurial)
         (diff-add-log-current-defuns)))))
   (t (user-error "`mercit-generate-changelog' requires Emacs 27 or greater"))))

;;;###autoload
(defun mercit-add-change-log-entry (&optional whoami file-name other-window)
  "Find change log file and add date entry and item for current change.
This differs from `add-change-log-entry' (which see) in that
it acts on the current hunk in a Mercit buffer instead of on
a position in a file-visiting buffer."
  (interactive (list current-prefix-arg
                     (prompt-for-change-log-name)))
  (pcase-let ((`(,buf ,pos) (mercit-diff-visit-file--noselect)))
    (mercit--with-temp-position buf pos
      (let ((add-log-buffer-file-name-function
             (lambda ()
               (or mercit-buffer-file-name
                   (buffer-file-name)))))
        (add-change-log-entry whoami file-name other-window)))))

;;;###autoload
(defun mercit-add-change-log-entry-other-window (&optional whoami file-name)
  "Find change log file in other window and add entry and item.
This differs from `add-change-log-entry-other-window' (which see)
in that it acts on the current hunk in a Mercit buffer instead of
on a position in a file-visiting buffer."
  (interactive (and current-prefix-arg
                    (list current-prefix-arg
                          (prompt-for-change-log-name))))
  (mercit-add-change-log-entry whoami file-name t))

;;; Edit Line Commit

;;;###autoload
(defun mercit-edit-line-commit (&optional type)
  "Edit the commit that added the current line.

With a prefix argument edit the commit that removes the line,
if any.  The commit is determined using `git blame' and made
editable using `git rebase --interactive' if it is reachable
from `HEAD', or by checking out the commit (or a branch that
points at it) otherwise."
  (interactive (list (and current-prefix-arg 'removal)))
  (let* ((chunk (mercit-current-blame-chunk (or type 'addition)))
         (rev   (oref chunk orig-rev)))
    (if (string-match-p "\\`0\\{40,\\}\\'" rev)
        (message "This line has not been committed yet")
      (let ((rebase (mercit-rev-ancestor-p rev "HEAD"))
            (file   (expand-file-name (oref chunk orig-file)
                                      (mercit-toplevel))))
        (if rebase
            (let ((mercit--rebase-published-symbol 'edit-published))
              (mercit-rebase-edit-commit rev (mercit-rebase-arguments)))
          (mercit-checkout (or (mercit-rev-branch rev) rev)))
        (unless (and buffer-file-name
                     (file-equal-p file buffer-file-name))
          (let ((blame-type (and mercit-blame-mode mercit-blame-type)))
            (if rebase
                (set-process-sentinel
                 mercit-this-process
                 (lambda (process event)
                   (mercit-sequencer-process-sentinel process event)
                   (when (eq (process-status process) 'exit)
                     (find-file file)
                     (when blame-type
                       (mercit-blame--pre-blame-setup blame-type)
                       (mercit-blame--run (mercit-blame-arguments))))))
              (find-file file)
              (when blame-type
                (mercit-blame--pre-blame-setup blame-type)
                (mercit-blame--run (mercit-blame-arguments))))))))))

(put 'mercit-edit-line-commit 'disabled t)

;;;###autoload
(defun mercit-diff-edit-hunk-commit (file)
  "From a hunk, edit the respective commit and visit the file.

First visit the file being modified by the hunk at the correct
location using `mercit-diff-visit-file'.  This actually visits a
blob.  When point is on a diff header, not within an individual
hunk, then this visits the blob the first hunk is about.

Then invoke `mercit-edit-line-commit', which uses an interactive
rebase to make the commit editable, or if that is not possible
because the commit is not reachable from `HEAD' by checking out
that commit directly.  This also causes the actual worktree file
to be visited.

Neither the blob nor the file buffer are killed when finishing
the rebase.  If that is undesirable, then it might be better to
use `mercit-rebase-edit-command' instead of this command."
  (interactive (list (mercit-file-at-point t t)))
  (let ((mercit-diff-visit-previous-blob nil))
    (with-current-buffer
        (mercit-diff-visit-file--internal file nil #'pop-to-buffer-same-window)
      (mercit-edit-line-commit))))

(put 'mercit-diff-edit-hunk-commit 'disabled t)

;;; Reshelve

(defcustom mercit-reshelve-since-committer-only nil
  "Whether `mercit-reshelve-since' changes only the committer dates.
Otherwise the author dates are also changed."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-commands
  :type 'boolean)

;;;###autoload
(defun mercit-reshelve-since (rev keyid)
  "Change the author and committer dates of the commits since REV.

Ask the user for the first reachable commit whose dates should
be changed.  Then read the new date for that commit.  The initial
minibuffer input and the previous history element offer good
values.  The next commit will be created one minute later and so
on.

This command is only intended for interactive use and should only
be used on highly rearranged and unpublished history.

If KEYID is non-nil, then use that to sign all reshelved commits.
Interactively use the value of the \"--gpg-sign\" option in the
list returned by `mercit-rebase-arguments'."
  (interactive (list nil
                     (transient-arg-value "--gpg-sign="
                                          (mercit-rebase-arguments))))
  (let* ((current (or (mercit-get-current-branch)
                      (user-error "Refusing to reshelve detached head")))
         (backup (concat "refs/original/refs/heads/" current)))
    (cond
     ((not rev)
      (when (and (mercit-ref-p backup)
                 (not (mercit-y-or-n-p
                       (format "Backup ref %s already exists.  Override? " backup))))
        (user-error "Abort"))
      (mercit-log-select
        (lambda (rev)
          (mercit-reshelve-since rev keyid))
        "Type %p on a commit to reshelve it and the commits above it,"))
     (t
      (cl-flet ((adjust (time offset)
                  (format-time-string
                   "%F %T %z"
                   (+ (floor time)
                      (* offset 60)
                      (- (car (decode-time time)))))))
        (let* ((start (concat rev "^"))
               (range (concat start ".." current))
               (time-rev (adjust (float-time (string-to-number
                                              (mercit-rev-format "%at" start)))
                                 1))
               (time-now (adjust (float-time)
                                 (- (string-to-number
                                     (mercit-git-string "rev-list" "--count"
                                                       range))))))
          (push time-rev mercit--reshelve-history)
          (let ((date (floor
                       (float-time
                        (date-to-time
                         (read-string "Date for first commit: "
                                      time-now 'mercit--reshelve-history))))))
            (with-environment-variables (("FILTER_BRANCH_SQUELCH_WARNING" "1"))
              (mercit-with-toplevel
                (mercit-run-git-async
                 "filter-branch" "--force" "--env-filter"
                 (format
                  "case $GIT_COMMIT in %s\nesac"
                  (mapconcat
                   (lambda (rev)
                     (prog1
                         (concat
                          (format "%s) " rev)
                          (and (not mercit-reshelve-since-committer-only)
                               (format "export GIT_AUTHOR_DATE=\"%s\"; " date))
                          (format "export GIT_COMMITTER_DATE=\"%s\";;" date))
                       (cl-incf date 60)))
                   (mercit-git-lines "rev-list" "--reverse" range)
                   " "))
                 (and keyid
                      (list "--commit-filter"
                            (format "git commit-tree --gpg-sign=%s \"$@\";"
                                    keyid)))
                 range "--"))
              (set-process-sentinel
               mercit-this-process
               (lambda (process event)
                 (when (memq (process-status process) '(exit signal))
                   (if (> (process-exit-status process) 0)
                       (mercit-process-sentinel process event)
                     (process-put process 'inhibit-refresh t)
                     (mercit-process-sentinel process event)
                     (mercit-run-git "update-ref" "-d" backup)))))))))))))

;;; Revision Stack

(defvar mercit-revision-stack nil)

(defcustom mercit-pop-revision-stack-format
  '("[%N: %h] "
    "%N: %cs %H\n   %s\n"
    "\\[\\([0-9]+\\)[]:]")
  "Control how `mercit-pop-revision-stack' inserts a revision.

The command `mercit-pop-revision-stack' inserts a representation
of the revision last pushed to the `mercit-revision-stack' into
the current buffer.  It inserts text at point and/or near the end
of the buffer, and removes the consumed revision from the stack.

The entries on the stack have the format (HASH TOPLEVEL) and this
option has the format (POINT-FORMAT EOB-FORMAT INDEX-REGEXP), all
of which may be nil or a string (though either one of EOB-FORMAT
or POINT-FORMAT should be a string, and if INDEX-REGEXP is
non-nil, then the two formats should be too).

First INDEX-REGEXP is used to find the previously inserted entry,
by searching backward from point.  The first submatch must match
the index number.  That number is incremented by one, and becomes
the index number of the entry to be inserted.  If you don't want
to number the inserted revisions, then use nil for INDEX-REGEXP.

If INDEX-REGEXP is non-nil, then both POINT-FORMAT and EOB-FORMAT
should contain \"%N\", which is replaced with the number that was
determined in the previous step.

Both formats, if non-nil and after removing %N, are then expanded
using `git show --format=FORMAT ...' inside TOPLEVEL.

The expansion of POINT-FORMAT is inserted at point, and the
expansion of EOB-FORMAT is inserted at the end of the buffer (if
the buffer ends with a comment, then it is inserted right before
that)."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-commands
  :type '(list (choice (string :tag "Insert at point format")
                       (cons (string :tag "Insert at point format")
                             (repeat (string :tag "Argument to git show")))
                       (const :tag "Don't insert at point" nil))
               (choice (string :tag "Insert at eob format")
                       (cons (string :tag "Insert at eob format")
                             (repeat (string :tag "Argument to git show")))
                       (const :tag "Don't insert at eob" nil))
               (choice (regexp :tag "Find index regexp")
                       (const :tag "Don't number entries" nil))))

(defcustom mercit-copy-revision-abbreviated nil
  "Whether to save abbreviated revision to `kill-ring' and `mercit-revision-stack'."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-miscellaneous
  :type 'boolean)

;;;###autoload
(defun mercit-pop-revision-stack (rev toplevel)
  "Insert a representation of a revision into the current buffer.

Pop a revision from the `mercit-revision-stack' and insert it into
the current buffer according to `mercit-pop-revision-stack-format'.
Revisions can be put on the stack using `mercit-copy-section-value'
and `mercit-copy-buffer-revision'.

If the stack is empty or with a prefix argument, instead read a
revision in the minibuffer.  By using the minibuffer history this
allows selecting an item which was popped earlier or to insert an
arbitrary reference or revision without first pushing it onto the
stack.

When reading the revision from the minibuffer, then it might not
be possible to guess the correct repository.  When this command
is called inside a repository (e.g. while composing a commit
message), then that repository is used.  Otherwise (e.g. while
composing an email) then the repository recorded for the top
element of the stack is used (even though we insert another
revision).  If not called inside a repository and with an empty
stack, or with two prefix arguments, then read the repository in
the minibuffer too."
  (interactive
   (if (or current-prefix-arg (not mercit-revision-stack))
       (let ((default-directory
              (or (and (not (= (prefix-numeric-value current-prefix-arg) 16))
                       (or (mercit-toplevel)
                           (cadr (car mercit-revision-stack))))
                  (mercit-read-repository))))
         (list (mercit-read-branch-or-commit "Insert revision")
               default-directory))
     (push (caar mercit-revision-stack) mercit-revision-history)
     (pop mercit-revision-stack)))
  (if rev
      (pcase-let ((`(,pnt-format ,eob-format ,idx-format)
                   mercit-pop-revision-stack-format))
        (let ((default-directory toplevel)
              (idx (and idx-format
                        (save-excursion
                          (if (re-search-backward idx-format nil t)
                              (number-to-string
                               (1+ (string-to-number (match-string 1))))
                            "1"))))
              pnt-args eob-args)
          (when (listp pnt-format)
            (setq pnt-args (cdr pnt-format))
            (setq pnt-format (car pnt-format)))
          (when (listp eob-format)
            (setq eob-args (cdr eob-format))
            (setq eob-format (car eob-format)))
          (when pnt-format
            (when idx-format
              (setq pnt-format
                    (string-replace "%N" idx pnt-format)))
            (mercit-rev-insert-format pnt-format rev pnt-args)
            (backward-delete-char 1))
          (when eob-format
            (when idx-format
              (setq eob-format
                    (string-replace "%N" idx eob-format)))
            (save-excursion
              (goto-char (point-max))
              (skip-syntax-backward ">s-")
              (beginning-of-line)
              (if (and comment-start (looking-at comment-start))
                  (while (looking-at comment-start)
                    (forward-line -1))
                (forward-line)
                (unless (= (current-column) 0)
                  (insert ?\n)))
              (insert ?\n)
              (mercit-rev-insert-format eob-format rev eob-args)
              (backward-delete-char 1)))))
    (user-error "Revision stack is empty")))

(define-key git-commit-mode-map
  (kbd "C-c C-w") #'mercit-pop-revision-stack)

;;;###autoload
(defun mercit-copy-section-value (arg)
  "Save the value of the current section for later use.

Save the section value to the `kill-ring', and, provided that
the current section is a commit, branch, or tag section, push
the (referenced) revision to the `mercit-revision-stack' for use
with `mercit-pop-revision-stack'.

When `mercit-copy-revision-abbreviated' is non-nil, save the
abbreviated revision to the `kill-ring' and the
`mercit-revision-stack'.

When the current section is a branch or a tag, and a prefix
argument is used, then save the revision at its tip to the
`kill-ring' instead of the reference name.

When the region is active, then save that to the `kill-ring',
like `kill-ring-save' would, instead of behaving as described
above.  If a prefix argument is used and the region is within
a hunk, then strip the diff marker column and keep only either
the added or removed lines, depending on the sign of the prefix
argument."
  (interactive "P")
  (cond
   ((and arg
         (mercit-section-internal-region-p)
         (mercit-section-match 'hunk))
    (kill-new
     (thread-last (buffer-substring-no-properties
                   (region-beginning)
                   (region-end))
       (replace-regexp-in-string
        (format "^\\%c.*\n?" (if (< (prefix-numeric-value arg) 0) ?+ ?-))
        "")
       (replace-regexp-in-string "^[ \\+\\-]" "")))
    (deactivate-mark))
   ((use-region-p)
    (call-interactively #'copy-region-as-kill))
   (t
    (when-let* ((section (mercit-current-section))
                (value (oref section value)))
      (mercit-section-case
        ((branch commit module-commit tag)
         (let ((default-directory default-directory) ref)
           (mercit-section-case
             ((branch tag)
              (setq ref value))
             (module-commit
              (setq default-directory
                    (file-name-as-directory
                     (expand-file-name (mercit-section-parent-value section)
                                       (mercit-toplevel))))))
           (setq value (mercit-rev-parse
                        (and mercit-copy-revision-abbreviated "--short")
                        value))
           (push (list value default-directory) mercit-revision-stack)
           (kill-new (message "%s" (or (and current-prefix-arg ref)
                                       value)))))
        (t (kill-new (message "%s" value))))))))

;;;###autoload
(defun mercit-copy-buffer-revision ()
  "Save the revision of the current buffer for later use.

Save the revision shown in the current buffer to the `kill-ring'
and push it to the `mercit-revision-stack'.

This command is mainly intended for use in `mercit-revision-mode'
buffers, the only buffers where it is always unambiguous exactly
which revision should be saved.

Most other Mercit buffers usually show more than one revision, in
some way or another, so this command has to select one of them,
and that choice might not always be the one you think would have
been the best pick.

In such buffers it is often more useful to save the value of
the current section instead, using `mercit-copy-section-value'.

When the region is active, then save that to the `kill-ring',
like `kill-ring-save' would, instead of behaving as described
above.

When `mercit-copy-revision-abbreviated' is non-nil, save the
abbreviated revision to the `kill-ring' and the
`mercit-revision-stack'."
  (interactive)
  (if (use-region-p)
      (call-interactively #'copy-region-as-kill)
    (when-let ((rev (or mercit-buffer-revision
                        (cl-case major-mode
                          (mercit-diff-mode
                           (if (string-match "\\.\\.\\.?\\(.+\\)"
                                             mercit-buffer-range)
                               (match-string 1 mercit-buffer-range)
                             mercit-buffer-range))
                          (mercit-status-mode "HEAD")))))
      (when (mercit-commit-p rev)
        (setq rev (mercit-rev-parse
                   (and mercit-copy-revision-abbreviated "--short")
                   rev))
        (push (list rev default-directory) mercit-revision-stack)
        (kill-new (message "%s" rev))))))

;;; Buffer Switching

;;;###autoload
(defun mercit-display-repository-buffer (buffer)
  "Display a Mercit buffer belonging to the current Mercurial repository.
The buffer is displayed using `mercit-display-buffer', which see."
  (interactive (list (mercit--read-repository-buffer
                      "Display mercit buffer: ")))
  (mercit-display-buffer buffer))

;;;###autoload
(defun mercit-switch-to-repository-buffer (buffer)
  "Switch to a Mercit buffer belonging to the current Mercurial repository."
  (interactive (list (mercit--read-repository-buffer
                      "Switch to mercit buffer: ")))
  (switch-to-buffer buffer))

;;;###autoload
(defun mercit-switch-to-repository-buffer-other-window (buffer)
  "Switch to a Mercit buffer belonging to the current Mercurial repository."
  (interactive (list (mercit--read-repository-buffer
                      "Switch to mercit buffer in another window: ")))
  (switch-to-buffer-other-window buffer))

;;;###autoload
(defun mercit-switch-to-repository-buffer-other-frame (buffer)
  "Switch to a Mercit buffer belonging to the current Mercurial repository."
  (interactive (list (mercit--read-repository-buffer
                      "Switch to mercit buffer in another frame: ")))
  (switch-to-buffer-other-frame buffer))

(defun mercit--read-repository-buffer (prompt)
  (if-let ((topdir (mercit-rev-parse-safe "--show-toplevel")))
      (read-buffer
       prompt (mercit-get-mode-buffer 'mercit-status-mode) t
       (pcase-lambda (`(,_ . ,buf))
         (and buf
              (with-current-buffer buf
                (and (or (derived-mode-p 'mercit-mode
                                         'mercit-repolist-mode
                                         'mercit-submodule-list-mode
                                         'git-rebase-mode)
                         (and buffer-file-name
                              (string-match-p git-commit-filename-regexp
                                              buffer-file-name)))
                     (equal (mercit-rev-parse-safe "--show-toplevel")
                            topdir))))))
    (user-error "Not inside a Mercurial repository")))

;;; Miscellaneous

;;;###autoload
(defun mercit-abort-dwim ()
  "Abort current operation.
Depending on the context, this will abort a merge, a rebase, a
patch application, a cherry-pick, a revert, or a bisect."
  (interactive)
  (cond ((mercit-merge-in-progress-p)     (mercit-merge-abort))
        ((mercit-rebase-in-progress-p)    (mercit-rebase-abort))
        ((mercit-am-in-progress-p)        (mercit-am-abort))
        ((mercit-sequencer-in-progress-p) (mercit-sequencer-abort))
        ((mercit-bisect-in-progress-p)    (mercit-bisect-reset))))

;;; _
(provide 'mercit-extras)
;;; mercit-extras.el ends here
