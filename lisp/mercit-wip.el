;;; mercit-wip.el --- Commit snapshots to work-in-progress refs  -*- lexical-binding:t -*-

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

;; This library defines tree global modes which automatically commit
;; snapshots to branch-specific work-in-progress refs before and after
;; making changes, and two commands which can be used to do so on
;; demand.

;;; Code:

(require 'mercit-core)
(require 'mercit-log)

;;; Options

(defgroup mercit-wip nil
  "Automatically commit to work-in-progress refs."
  :link '(info-link "(mercit)Wip Modes")
  :group 'mercit-modes
  :group 'mercit-essentials)

(defgroup mercit-wip-legacy nil
  "It is better to not use these modes individually."
  :link '(info-link "(mercit)Legacy Wip Modes")
  :group 'mercit-wip)

(defcustom mercit-wip-mode-lighter " Wip"
  "Lighter for Mercit-Wip mode."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-wip
  :type 'string)

(defcustom mercit-wip-after-save-local-mode-lighter ""
  "Lighter for Mercit-Wip-After-Save-Local mode."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-wip-legacy
  :type 'string)

(defcustom mercit-wip-after-apply-mode-lighter ""
  "Lighter for Mercit-Wip-After-Apply mode."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-wip-legacy
  :type 'string)

(defcustom mercit-wip-before-change-mode-lighter ""
  "Lighter for Mercit-Wip-Before-Change mode."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-wip-legacy
  :type 'string)

(defcustom mercit-wip-initial-backup-mode-lighter ""
  "Lighter for Mercit-Wip-Initial Backup mode."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-wip-legacy
  :type 'string)

(defcustom mercit-wip-merge-branch nil
  "Whether to merge the current branch into its wip ref.

If non-nil and the current branch has new commits, then it is
merged into the wip ref before creating a new wip commit.  This
makes it easier to inspect wip history and the wip commits are
never garbage collected.

If nil and the current branch has new commits, then the wip ref
is reset to the tip of the branch before creating a new wip
commit.  With this setting wip commits are eventually garbage
collected.  This is currently the default."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-wip
  :type 'boolean)

(defcustom mercit-wip-namespace "refs/wip/"
  "Namespace used for work-in-progress refs.
The wip refs are named \"<namespace/>index/<branchref>\"
and \"<namespace/>wtree/<branchref>\".  When snapshots
are created while the `HEAD' is detached then \"HEAD\"
is used as `branch-ref'."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-wip
  :type 'string)

;;; Modes

;;;###autoload
(define-minor-mode mercit-wip-mode
  "Save uncommitted changes to work-in-progress refs.

Whenever appropriate (i.e. when dataloss would be a possibility
otherwise) this mode causes uncommitted changes to be committed
to dedicated work-in-progress refs.

For historic reasons this mode is implemented on top of four
other `mercit-wip-*' modes, which can also be used individually,
if you want finer control over when the wip refs are updated;
but that is discouraged."
  :package-version '(mercit . "0.0.0")
  :lighter mercit-wip-mode-lighter
  :global t
  (let ((arg (if mercit-wip-mode 1 -1)))
    (mercit-wip-after-save-mode arg)
    (mercit-wip-after-apply-mode arg)
    (mercit-wip-before-change-mode arg)
    (mercit-wip-initial-backup-mode arg)))

(define-minor-mode mercit-wip-after-save-local-mode
  "After saving, also commit to a worktree work-in-progress ref.

After saving the current file-visiting buffer this mode also
commits the changes to the worktree work-in-progress ref for
the current branch.

This mode should be enabled globally by turning on the globalized
variant `mercit-wip-after-save-mode'."
  :package-version '(mercit . "0.0.0")
  :lighter mercit-wip-after-save-local-mode-lighter
  (if mercit-wip-after-save-local-mode
      (if (and buffer-file-name (mercit-inside-worktree-p t))
          (add-hook 'after-save-hook #'mercit-wip-commit-buffer-file t t)
        (setq mercit-wip-after-save-local-mode nil)
        (user-error "Need a worktree and a file"))
    (remove-hook 'after-save-hook #'mercit-wip-commit-buffer-file t)))

(defun mercit-wip-after-save-local-mode-turn-on ()
  (and buffer-file-name
       (mercit-inside-worktree-p t)
       (mercit-file-tracked-p buffer-file-name)
       (mercit-wip-after-save-local-mode)))

;;;###autoload
(define-globalized-minor-mode mercit-wip-after-save-mode
  mercit-wip-after-save-local-mode mercit-wip-after-save-local-mode-turn-on
  :package-version '(mercit . "0.0.0")
  :group 'mercit-wip)

(defun mercit-wip-commit-buffer-file (&optional msg)
  "Commit visited file to a worktree work-in-progress ref.

Also see `mercit-wip-after-save-mode' which calls this function
automatically whenever a buffer visiting a tracked file is saved."
  (interactive)
  (--when-let (mercit-wip-get-ref)
    (mercit-with-toplevel
      (let ((file (file-relative-name buffer-file-name)))
        (mercit-wip-commit-worktree
         it (list file)
         (format (cond (msg)
                       ((called-interactively-p 'any)
                        "wip-save %s after save")
                       (t
                        "autosave %s after save"))
                 file))))))

;;;###autoload
(define-minor-mode mercit-wip-after-apply-mode
  "Commit to work-in-progress refs.

After applying a change using any \"apply variant\"
command (apply, stage, unstage, discard, and reverse) commit the
affected files to the current wip refs.  For each branch there
may be two wip refs; one contains snapshots of the files as found
in the worktree and the other contains snapshots of the entries
in the index."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-wip
  :lighter mercit-wip-after-apply-mode-lighter
  :global t)

(defun mercit-wip-commit-after-apply (&optional files msg)
  (when mercit-wip-after-apply-mode
    (mercit-wip-commit files msg)))

;;;###autoload
(define-minor-mode mercit-wip-before-change-mode
  "Commit to work-in-progress refs before certain destructive changes.

Before invoking a revert command or an \"apply variant\"
command (apply, stage, unstage, discard, and reverse) commit the
affected tracked files to the current wip refs.  For each branch
there may be two wip refs; one contains snapshots of the files
as found in the worktree and the other contains snapshots of the
entries in the index.

Only changes to files which could potentially be affected by the
command which is about to be called are committed."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-wip
  :lighter mercit-wip-before-change-mode-lighter
  :global t)

(defun mercit-wip-commit-before-change (&optional files msg)
  (when mercit-wip-before-change-mode
    (mercit-with-toplevel
      (mercit-wip-commit files msg))))

(define-minor-mode mercit-wip-initial-backup-mode
  "Before saving a buffer for the first time, commit to a wip ref."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-wip
  :lighter mercit-wip-initial-backup-mode-lighter
  :global t
  (if mercit-wip-initial-backup-mode
      (add-hook  'before-save-hook #'mercit-wip-commit-initial-backup)
    (remove-hook 'before-save-hook #'mercit-wip-commit-initial-backup)))

(defun mercit--any-wip-mode-enabled-p ()
  "Return non-nil if any global wip mode is enabled."
  (or mercit-wip-mode
      mercit-wip-after-save-mode
      mercit-wip-after-apply-mode
      mercit-wip-before-change-mode
      mercit-wip-initial-backup-mode))

(defvar-local mercit-wip-buffer-backed-up nil)
(put 'mercit-wip-buffer-backed-up 'permanent-local t)

;;;###autoload
(defun mercit-wip-commit-initial-backup ()
  "Before saving, commit current file to a worktree wip ref.

The user has to add this function to `before-save-hook'.

Commit the current state of the visited file before saving the
current buffer to that file.  This backs up the same version of
the file as `backup-buffer' would, but stores the backup in the
worktree wip ref, which is also used by the various Mercit Wip
modes, instead of in a backup file as `backup-buffer' would.

This function ignores the variables that affect `backup-buffer'
and can be used along-side that function, which is recommended
because this function only backs up files that are tracked in
a Mercurial repository."
  (when (and (not mercit-wip-buffer-backed-up)
             buffer-file-name
             (mercit-inside-worktree-p t)
             (mercit-file-tracked-p buffer-file-name))
    (let ((mercit-save-repository-buffers nil))
      (mercit-wip-commit-buffer-file "autosave %s before save"))
    (setq mercit-wip-buffer-backed-up t)))

;;; Core

(defun mercit-wip-commit (&optional files msg)
  "Commit all tracked files to the work-in-progress refs.

Interactively, commit all changes to all tracked files using
a generic commit message.  With a prefix-argument the commit
message is read in the minibuffer.

Non-interactively, only commit changes to FILES using MSG as
commit message."
  (interactive (list nil (if current-prefix-arg
                             (mercit-read-string "Wip commit message")
                           "wip-save tracked files")))
  (--when-let (mercit-wip-get-ref)
    (mercit-wip-commit-index it files msg)
    (mercit-wip-commit-worktree it files msg)))

(defun mercit-wip-commit-index (ref files msg)
  (let* ((wipref (mercit--wip-index-ref ref))
         (parent (mercit-wip-get-parent ref wipref))
         (tree   (mercit-git-string "write-tree")))
    (mercit-wip-update-wipref ref wipref tree parent files msg "index")))

(defun mercit-wip-commit-worktree (ref files msg)
  (when (or (not files)
            ;; `update-index' will either ignore (before Git v2.32.0)
            ;; or fail when passed directories (relevant for the
            ;; untracked files code paths).
            (setq files (seq-remove #'file-directory-p files)))
    (let* ((wipref (mercit--wip-wtree-ref ref))
           (parent (mercit-wip-get-parent ref wipref))
           (tree (mercit-with-temp-index parent (list "--reset" "-i")
                   (if files
                       ;; Note: `update-index' is used instead of `add'
                       ;; because `add' will fail if a file is already
                       ;; deleted in the temporary index.
                       (mercit-call-git
                        "update-index" "--add" "--remove"
                        (and (mercit-git-version>= "2.25.0")
                             "--ignore-skip-worktree-entries")
                        "--" files)
                     (mercit-with-toplevel
                       (mercit-call-git "add" "-u" ".")))
                   (mercit-git-string "write-tree"))))
      (mercit-wip-update-wipref ref wipref tree parent files msg "worktree"))))

(defun mercit-wip-update-wipref (ref wipref tree parent files msg start-msg)
  (cond
   ((and (not (equal parent wipref))
         (or (not mercit-wip-merge-branch)
             (not (mercit-rev-verify wipref))))
    (setq start-msg (concat "start autosaving " start-msg))
    (mercit-update-ref wipref start-msg
                      (mercit-git-string "commit-tree" "--no-gpg-sign"
                                        "-p" parent "-m" start-msg
                                        (concat parent "^{tree}")))
    (setq parent wipref))
   ((and mercit-wip-merge-branch
         (or (not (mercit-rev-ancestor-p ref wipref))
             (not (mercit-rev-ancestor-p
                   (concat (mercit-git-string "log" "--format=%H"
                                             "-1" "--merges" wipref)
                           "^2")
                   ref))))
    (setq start-msg (format "merge %s into %s" ref start-msg))
    (mercit-update-ref wipref start-msg
                      (mercit-git-string "commit-tree" "--no-gpg-sign"
                                        "-p" wipref "-p" ref
                                        "-m" start-msg
                                        (concat ref "^{tree}")))
    (setq parent wipref)))
  (when (mercit-git-failure "diff-tree" "--quiet" parent tree "--" files)
    (unless (and msg (not (= (aref msg 0) ?\s)))
      (let ((len (length files)))
        (setq msg (concat
                   (cond ((= len 0) "autosave tracked files")
                         ((> len 1) (format "autosave %s files" len))
                         (t (concat "autosave "
                                    (file-relative-name (car files)
                                                        (mercit-toplevel)))))
                   msg))))
    (mercit-update-ref wipref msg
                      (mercit-git-string "commit-tree" "--no-gpg-sign"
                                        "-p" parent "-m" msg tree))))

(defun mercit-wip-get-ref ()
  (let ((ref (or (mercit-git-string "symbolic-ref" "HEAD") "HEAD")))
    (and (mercit-rev-verify ref)
         ref)))

(defun mercit-wip-get-parent (ref wipref)
  (if (and (mercit-rev-verify wipref)
           (equal (mercit-git-string "merge-base" wipref ref)
                  (mercit-rev-verify ref)))
      wipref
    ref))

(defun mercit--wip-index-ref (&optional ref)
  (mercit--wip-ref "index/" ref))

(defun mercit--wip-wtree-ref (&optional ref)
  (mercit--wip-ref "wtree/" ref))

(defun mercit--wip-ref (namespace &optional ref)
  (concat mercit-wip-namespace namespace
          (or (and ref (string-prefix-p "refs/" ref) ref)
              (and-let* ((branch (and (not (equal ref "HEAD"))
                                      (or ref (mercit-get-current-branch)))))
                (concat "refs/heads/" branch))
              "HEAD")))

(defun mercit-wip-maybe-add-commit-hook ()
  (when (and mercit-wip-merge-branch
             (mercit-wip-any-enabled-p))
    (add-hook 'git-commit-post-finish-hook #'mercit-wip-commit nil t)))

(defun mercit-wip-any-enabled-p ()
  (or mercit-wip-mode
      mercit-wip-after-save-local-mode
      mercit-wip-after-save-mode
      mercit-wip-after-apply-mode
      mercit-wip-before-change-mode
      mercit-wip-initial-backup-mode))

;;; Log

(defun mercit-wip-log-index (args files)
  "Show log for the index wip ref of the current branch."
  (interactive (mercit-log-arguments))
  (mercit-log-setup-buffer (list (mercit--wip-index-ref)) args files))

(defun mercit-wip-log-worktree (args files)
  "Show log for the worktree wip ref of the current branch."
  (interactive (mercit-log-arguments))
  (mercit-log-setup-buffer (list (mercit--wip-wtree-ref)) args files))

(defun mercit-wip-log-current (branch args files count)
  "Show log for the current branch and its wip refs.
With a negative prefix argument only show the worktree wip ref.
The absolute numeric value of the prefix argument controls how
many \"branches\" of each wip ref are shown."
  (interactive
   (nconc (list (or (mercit-get-current-branch) "HEAD"))
          (mercit-log-arguments)
          (list (prefix-numeric-value current-prefix-arg))))
  (mercit-wip-log branch args files count))

(defun mercit-wip-log (branch args files count)
  "Show log for a branch and its wip refs.
With a negative prefix argument only show the worktree wip ref.
The absolute numeric value of the prefix argument controls how
many \"branches\" of each wip ref are shown."
  (interactive
   (nconc (list (mercit-completing-read
                 "Log branch and its wip refs"
                 (-snoc (mercit-list-local-branch-names) "HEAD")
                 nil t nil 'mercit-revision-history
                 (or (mercit-branch-at-point)
                     (mercit-get-current-branch)
                     "HEAD")))
          (mercit-log-arguments)
          (list (prefix-numeric-value current-prefix-arg))))
  (mercit-log-setup-buffer (nconc (list branch)
                                 (mercit-wip-log-get-tips
                                  (mercit--wip-wtree-ref branch)
                                  (abs count))
                                 (and (>= count 0)
                                      (mercit-wip-log-get-tips
                                       (mercit--wip-index-ref branch)
                                       (abs count))))
                          args files))

(defun mercit-wip-log-get-tips (wipref count)
  (and-let* ((reflog (mercit-git-lines "reflog" wipref)))
    (let (tips)
      (while (and reflog (> count 1))
        ;; "start autosaving ..." is the current message, but it used
        ;; to be "restart autosaving ...", and those messages may
        ;; still be around (e.g., if gc.reflogExpire is to "never").
        (setq reflog (cl-member "^[^ ]+ [^:]+: \\(?:re\\)?start autosaving"
                                reflog :test #'string-match-p))
        (when (and (cadr reflog)
                   (string-match "^[^ ]+ \\([^:]+\\)" (cadr reflog)))
          (push (match-string 1 (cadr reflog)) tips))
        (setq reflog (cddr reflog))
        (cl-decf count))
      (cons wipref (nreverse tips)))))

;;; _
(provide 'mercit-wip)
;;; mercit-wip.el ends here
