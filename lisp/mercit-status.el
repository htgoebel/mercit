;;; mercit-status.el --- The grand overview  -*- lexical-binding:t -*-

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

;; This library implements the status buffer.

;;; Code:

(require 'mercit)

;;; Options

(defgroup mercit-status nil
  "Inspect and manipulate Mercurial repositories."
  :link '(info-link "(mercit)Status Buffer")
  :group 'mercit-modes)

(defcustom mercit-status-mode-hook nil
  "Hook run after entering Mercit-Status mode."
  :group 'mercit-status
  :type 'hook)

(defcustom mercit-status-headers-hook
  '(mercit-insert-error-header
    mercit-insert-diff-filter-header
    mercit-insert-head-branch-header
    mercit-insert-upstream-branch-header
    mercit-insert-push-branch-header
    mercit-insert-tags-header)
  "Hook run to insert headers into the status buffer.

This hook is run by `mercit-insert-status-headers', which in turn
has to be a member of `mercit-status-sections-hook' to be used at
all."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-status
  :type 'hook
  :options '(mercit-insert-error-header
             mercit-insert-diff-filter-header
             mercit-insert-repo-header
             mercit-insert-remote-header
             mercit-insert-head-branch-header
             mercit-insert-upstream-branch-header
             mercit-insert-push-branch-header
             mercit-insert-tags-header))

(defcustom mercit-status-sections-hook
  '(mercit-insert-status-headers
    mercit-insert-merge-log
    mercit-insert-rebase-sequence
    mercit-insert-am-sequence
    mercit-insert-sequencer-sequence
    mercit-insert-bisect-output
    mercit-insert-bisect-rest
    mercit-insert-bisect-log
    mercit-insert-untracked-files
    mercit-insert-unstaged-changes
    mercit-insert-staged-changes
    mercit-insert-stashes
    mercit-insert-unpushed-to-pushremote
    mercit-insert-unpushed-to-upstream-or-recent
    mercit-insert-unpulled-from-pushremote
    mercit-insert-unpulled-from-upstream)
  "Hook run to insert sections into a status buffer."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-status
  :type 'hook)

(defcustom mercit-status-initial-section '(1)
  "The section point is placed on when a status buffer is created.

When such a buffer is merely being refreshed or being shown again
after it was merely buried, then this option has no effect.

If this is nil, then point remains on the very first section as
usual.  Otherwise it has to be a list of integers and section
identity lists.  The members of that list are tried in order
until a matching section is found.

An integer means to jump to the nth section, 1 for example
jumps over the headings.  To get a section's \"identity list\"
use \\[universal-argument] \\[mercit-describe-section-briefly].

If, for example, you want to jump to the commits that haven't
been pulled from the upstream, or else the second section, then
use: (((unpulled . \"..@{upstream}\") (status)) 1).

See option `mercit-section-initial-visibility-alist' for how to
control the initial visibility of the jumped to section."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-status
  :type '(choice (const :tag "as usual" nil)
                 (repeat (choice (number :tag "nth top-level section")
                                 (sexp   :tag "section identity")))))

(defcustom mercit-status-goto-file-position nil
  "Whether to go to position corresponding to file position.

If this is non-nil and the current buffer is visiting a file,
then `mercit-status' tries to go to the position in the status
buffer that corresponds to the position in the file-visiting
buffer.  This jumps into either the diff of unstaged changes
or the diff of staged changes.

If the previously current buffer does not visit a file, or if
the file has neither unstaged nor staged changes then this has
no effect.

The command `mercit-status-here' tries to go to that position,
regardless of the value of this option."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-status
  :type 'boolean)

(defcustom mercit-status-show-hashes-in-headers nil
  "Whether headers in the status buffer show hashes.
The functions which respect this option are
`mercit-insert-head-branch-header',
`mercit-insert-upstream-branch-header', and
`mercit-insert-push-branch-header'."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-status
  :type 'boolean)

(defcustom mercit-status-margin
  (list nil
        (nth 1 mercit-log-margin)
        'mercit-log-margin-width nil
        (nth 4 mercit-log-margin))
  "Format of the margin in `mercit-status-mode' buffers.

The value has the form (INIT STYLE WIDTH AUTHOR AUTHOR-WIDTH).

If INIT is non-nil, then the margin is shown initially.
STYLE controls how to format the author or committer date.
  It can be one of `age' (to show the age of the commit),
  `age-abbreviated' (to abbreviate the time unit to a character),
  or a string (suitable for `format-time-string') to show the
  actual date.
WIDTH controls the width of the margin.  This exists for forward
  compatibility and currently the value should not be changed.
AUTHOR controls whether the name of the author is also shown by
  default.
AUTHOR-WIDTH has to be an integer.  When the name of the author
  is shown, then this specifies how much space is used to do so."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-status
  :group 'mercit-margin
  :type mercit-log-margin--custom-type
  :initialize #'mercit-custom-initialize-reset
  :set-after '(mercit-log-margin)
  :set (apply-partially #'mercit-margin-set-variable 'mercit-status-mode))

(defcustom mercit-status-use-buffer-arguments 'selected
  "Whether `mercit-status' reuses arguments when the buffer already exists.

This option has no effect when merely refreshing the status
buffer using `mercit-refresh'.

Valid values are:

`always': Always use the set of arguments that is currently
  active in the status buffer, provided that buffer exists
  of course.
`selected': Use the set of arguments from the status
  buffer, but only if it is displayed in a window of the
  current frame.  This is the default.
`current': Use the set of arguments from the status buffer,
  but only if it is the current buffer.
`never': Never use the set of arguments from the status
  buffer."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-buffers
  :group 'mercit-commands
  :type '(choice
          (const :tag "always use args from buffer" always)
          (const :tag "use args from buffer if displayed in frame" selected)
          (const :tag "use args from buffer if it is current" current)
          (const :tag "never use args from buffer" never)))

;;; Commands

;;;###autoload
(defun mercit-init (directory)
  "Initialize a Mercurial repository, then show its status.

If the directory is below an existing repository, then the user
has to confirm that a new one should be created inside.  If the
directory is the root of the existing repository, then the user
has to confirm that it should be reinitialized.

Non-interactively DIRECTORY is (re-)initialized unconditionally."
  (interactive
   (let ((directory (file-name-as-directory
                     (expand-file-name
                      (read-directory-name "Create repository in: ")))))
     (when-let ((toplevel (mercit-toplevel directory)))
       (setq toplevel (expand-file-name toplevel))
       (unless (y-or-n-p (if (file-equal-p toplevel directory)
                             (format "Reinitialize existing repository %s? "
                                     directory)
                           (format "%s is a repository.  Create another in %s? "
                                   toplevel directory)))
         (user-error "Abort")))
     (list directory)))
  ;; `git init' does not understand the meaning of "~"!
  (mercit-call-git "init" (mercit-convert-filename-for-git
                          (expand-file-name directory)))
  (mercit-status-setup-buffer directory))

;;;###autoload
(defun mercit-status (&optional directory cache)
  "Show the status of the current Mercurial repository in a buffer.

If the current directory isn't located within a Mercurial repository,
then prompt for an existing repository or an arbitrary directory,
depending on option `mercit-repository-directories', and show the
status of the selected repository instead.

* If that option specifies any existing repositories, then offer
  those for completion and show the status buffer for the
  selected one.

* Otherwise read an arbitrary directory using regular file-name
  completion.  If the selected directory is the top-level of an
  existing working tree, then show the status buffer for that.

* Otherwise offer to initialize the selected directory as a new
  repository.  After creating the repository show its status
  buffer.

These fallback behaviors can also be forced using one or more
prefix arguments:

* With two prefix arguments (or more precisely a numeric prefix
  value of 16 or greater) read an arbitrary directory and act on
  it as described above.  The same could be accomplished using
  the command `mercit-init'.

* With a single prefix argument read an existing repository, or
  if none can be found based on `mercit-repository-directories',
  then fall back to the same behavior as with two prefix
  arguments."
  (interactive
   (let ((mercit--refresh-cache (list (cons 0 0))))
     (list (and (or current-prefix-arg (not (mercit-toplevel)))
                (progn (mercit--assert-usable-git)
                       (mercit-read-repository
                        (>= (prefix-numeric-value current-prefix-arg) 16))))
           mercit--refresh-cache)))
  (let ((mercit--refresh-cache (or cache (list (cons 0 0)))))
    (if directory
        (let ((toplevel (mercit-toplevel directory)))
          (setq directory (file-name-as-directory
                           (expand-file-name directory)))
          (if (and toplevel (file-equal-p directory toplevel))
              (mercit-status-setup-buffer directory)
            (when (y-or-n-p
                   (if toplevel
                       (format "%s is a repository.  Create another in %s? "
                               toplevel directory)
                     (format "Create repository in %s? " directory)))
              ;; Creating a new repository invalidates cached values.
              (setq mercit--refresh-cache nil)
              (mercit-init directory))))
      (mercit-status-setup-buffer default-directory))))

(put 'mercit-status 'interactive-only 'mercit-status-setup-buffer)

;;;###autoload
(defalias 'mercit #'mercit-status
  "An alias for `mercit-status' for better discoverability.

Instead of invoking this alias for `mercit-status' using
\"M-x mercit RET\", you should bind a key to `mercit-status'
and read the info node `(mercit)Getting Started', which
also contains other useful hints.")

;;;###autoload
(defun mercit-status-here ()
  "Like `mercit-status' but with non-nil `mercit-status-goto-file-position'."
  (interactive)
  (let ((mercit-status-goto-file-position t))
    (call-interactively #'mercit-status)))

(put 'mercit-status-here 'interactive-only 'mercit-status-setup-buffer)

;;;###autoload
(defun mercit-status-quick ()
  "Show the status of the current Mercurial repository, maybe without refreshing.

If the status buffer of the current Mercurial repository exists
but isn't being displayed in the selected frame, then display it
without refreshing it.

If the status buffer is being displayed in the selected frame,
then also refresh it.

Prefix arguments have the same meaning as for `mercit-status',
and additionally cause the buffer to be refresh.

To use this function instead of `mercit-status', add this to your
init file: (global-set-key (kbd \"C-x g\") \\='mercit-status-quick)."
  (interactive)
  (if-let ((buffer
            (and (not current-prefix-arg)
                 (not (mercit-get-mode-buffer 'mercit-status-mode nil 'selected))
                 (mercit-get-mode-buffer 'mercit-status-mode))))
      (mercit-display-buffer buffer)
    (call-interactively #'mercit-status)))

;;; Mode

(defvar mercit-status-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mercit-mode-map)
    (define-key map "j" #'mercit-status-jump)
    (define-key map [remap dired-jump] #'mercit-dired-jump)
    map)
  "Keymap for `mercit-status-mode'.")

(transient-define-prefix mercit-status-jump ()
  "In a Mercit-Status buffer, jump to a section."
  ["Jump to"
   [("z " "Stashes" mercit-jump-to-stashes
     :if (lambda () (memq 'mercit-insert-stashes mercit-status-sections-hook)))
    ("t " "Tracked" mercit-jump-to-tracked
     :if (lambda () (memq 'mercit-insert-tracked-files mercit-status-sections-hook)))
    ("n " "Untracked" mercit-jump-to-untracked
     :if (lambda () (memq 'mercit-insert-untracked-files mercit-status-sections-hook)))
    ("u " "Unstaged" mercit-jump-to-unstaged
     :if (lambda () (memq 'mercit-insert-unstaged-changes mercit-status-sections-hook)))
    ("s " "Staged" mercit-jump-to-staged
     :if (lambda () (memq 'mercit-insert-staged-changes mercit-status-sections-hook)))]
   [("fu" "Unpulled from upstream" mercit-jump-to-unpulled-from-upstream
     :if (lambda () (memq 'mercit-insert-unpulled-from-upstream mercit-status-sections-hook)))
    ("fp" "Unpulled from pushremote" mercit-jump-to-unpulled-from-pushremote
     :if (lambda () (memq 'mercit-insert-unpulled-from-pushremote mercit-status-sections-hook)))
    ("pu" mercit-jump-to-unpushed-to-upstream
     :if (lambda ()
           (or (memq 'mercit-insert-unpushed-to-upstream-or-recent mercit-status-sections-hook)
               (memq 'mercit-insert-unpushed-to-upstream mercit-status-sections-hook)))
     :description (lambda ()
                    (let ((upstream (mercit-get-upstream-branch)))
                      (if (or (not upstream)
                              (mercit-rev-ancestor-p "HEAD" upstream))
                          "Recent commits"
                        "Unmerged into upstream"))))
    ("pp" "Unpushed to pushremote" mercit-jump-to-unpushed-to-pushremote
     :if (lambda () (memq 'mercit-insert-unpushed-to-pushremote mercit-status-sections-hook)))
    ("a " "Assumed unstaged" mercit-jump-to-assume-unchanged
     :if (lambda () (memq 'mercit-insert-assume-unchanged-files mercit-status-sections-hook)))
    ("w " "Skip worktree" mercit-jump-to-skip-worktree
     :if (lambda () (memq 'mercit-insert-skip-worktree-files mercit-status-sections-hook)))]
   [("i" "Using Imenu" imenu)]])

(define-derived-mode mercit-status-mode mercit-mode "Mercit"
  "Mode for looking at Mercurial status.

This mode is documented in info node `(mercit)Status Buffer'.

\\<mercit-mode-map>\
Type \\[mercit-refresh] to refresh the current buffer.
Type \\[mercit-section-toggle] to expand or hide the section at point.
Type \\[mercit-visit-thing] to visit the change or commit at point.

Type \\[mercit-dispatch] to invoke major commands.

Staging and applying changes is documented in info node
`(mercit)Staging and Unstaging' and info node `(mercit)Applying'.

\\<mercit-hunk-section-map>Type \
\\[mercit-apply] to apply the change at point, \
\\[mercit-stage] to stage,
\\[mercit-unstage] to unstage, \
\\[mercit-discard] to discard, or \
\\[mercit-reverse] to reverse it.

\\<mercit-status-mode-map>\
Type \\[mercit-commit] to create a commit.

\\{mercit-status-mode-map}"
  :group 'mercit-status
  (hack-dir-local-variables-non-file-buffer)
  (when mercit-status-initial-section
    (add-hook 'mercit-refresh-buffer-hook
              #'mercit-status-goto-initial-section nil t))
  (setq mercit--imenu-group-types '(not branch commit)))

(put 'mercit-status-mode 'mercit-diff-default-arguments
     '("--git"))  ;; was "--no-ext-diff"
(put 'mercit-status-mode 'mercit-log-default-arguments
     '("--limit=256" "--decorate"))

;;;###autoload
(defun mercit-status-setup-buffer (&optional directory)
  (unless directory
    (setq directory default-directory))
  (when (file-remote-p directory)
    (mercit-git-version-assert))
  (let* ((default-directory directory)
         (d (mercit-diff--get-value 'mercit-status-mode
                                   mercit-status-use-buffer-arguments))
         (l (mercit-log--get-value 'mercit-status-mode
                                  mercit-status-use-buffer-arguments))
         (file (and mercit-status-goto-file-position
                    (mercit-file-relative-name)))
         (line (and file (line-number-at-pos)))
         (col  (and file (current-column)))
         (buf  (mercit-setup-buffer #'mercit-status-mode nil
                 (mercit-buffer-diff-args  (nth 0 d))
                 (mercit-buffer-diff-files (nth 1 d))
                 (mercit-buffer-log-args   (nth 0 l))
                 (mercit-buffer-log-files  (nth 1 l)))))
    (when file
      (with-current-buffer buf
        (let ((staged (mercit-get-section '((staged) (status)))))
          (if (and staged
                   (cadr (mercit-diff--locate-hunk file line staged)))
              (mercit-diff--goto-position file line col staged)
            (let ((unstaged (mercit-get-section '((unstaged) (status)))))
              (unless (and unstaged
                           (mercit-diff--goto-position file line col unstaged))
                (when staged
                  (mercit-diff--goto-position file line col staged))))))))
    buf))

(defun mercit-status-refresh-buffer ()
  (mercit-git-exit-code "update-index" "--refresh")
  (mercit-insert-section (status)
    (mercit-run-section-hook 'mercit-status-sections-hook)))

(defun mercit-status-goto-initial-section ()
  "Jump to the section specified by `mercit-status-initial-section'."
  (when-let ((section
              (--some (if (integerp it)
                          (nth (1- it)
                               (mercit-section-siblings (mercit-current-section)
                                                       'next))
                        (mercit-get-section it))
                      mercit-status-initial-section)))
    (goto-char (oref section start))
    (when-let ((vis (cdr (assq 'mercit-status-initial-section
                               mercit-section-initial-visibility-alist))))
      (if (eq vis 'hide)
          (mercit-section-hide section)
        (mercit-section-show section))))
  (remove-hook 'mercit-refresh-buffer-hook
               #'mercit-status-goto-initial-section t))

(defun mercit-status-maybe-update-revision-buffer (&optional _)
  "When moving in the status buffer, update the revision buffer.
If there is no revision buffer in the same frame, then do nothing."
  (when (derived-mode-p 'mercit-status-mode)
    (mercit--maybe-update-revision-buffer)))

(defun mercit-status-maybe-update-stash-buffer (&optional _)
  "When moving in the status buffer, update the stash buffer.
If there is no stash buffer in the same frame, then do nothing."
  (when (derived-mode-p 'mercit-status-mode)
    (mercit--maybe-update-stash-buffer)))

(defun mercit-status-maybe-update-blob-buffer (&optional _)
  "When moving in the status buffer, update the blob buffer.
If there is no blob buffer in the same frame, then do nothing."
  (when (derived-mode-p 'mercit-status-mode)
    (mercit--maybe-update-blob-buffer)))

;;; Sections
;;;; Special Headers

(defun mercit-insert-status-headers ()
  "Insert header sections appropriate for `mercit-status-mode' buffers.
The sections are inserted by running the functions on the hook
`mercit-status-headers-hook'."
  (if (mercit-rev-verify ".")
      (mercit-insert-headers 'mercit-status-headers-hook)
    (insert "In the beginning there was darkness\n\n")))

(defvar mercit-error-section-map
  (let ((map (make-sparse-keymap)))
    (mercit-menu-set map [mercit-visit-thing]
      #'mercit-process-buffer "Visit process output")
    map)
  "Keymap for `error' sections.")

(defun mercit-insert-error-header ()
  "Insert the message about the Mercurial error that just occurred.

This function is only aware of the last error that occur when Mercurial
was run for side-effects.  If, for example, an error occurs while
generating a diff, then that error won't be inserted.  Refreshing
the status buffer causes this section to disappear again."
  (when mercit-this-error
    (mercit-insert-section (error 'git)
      (insert (propertize (format "%-16s" "MercurialError! ")
                          'font-lock-face 'mercit-section-heading))
      (insert (propertize mercit-this-error 'font-lock-face 'error))
      (when-let ((key (car (where-is-internal 'mercit-process-buffer))))
        (insert (format "  [Type `%s' for details]" (key-description key))))
      (insert ?\n))
    (setq mercit-this-error nil)))

(defun mercit-insert-diff-filter-header ()
  "Insert a header line showing the effective diff filters."
  (let ((ignore-modules (mercit-ignore-submodules-p)))
    (when (or ignore-modules
              mercit-buffer-diff-files)
      (insert (propertize (format "%-10s" "Filter! ")
                          'font-lock-face 'mercit-section-heading))
      (when ignore-modules
        (insert ignore-modules)
        (when mercit-buffer-diff-files
          (insert " -- ")))
      (when mercit-buffer-diff-files
        (insert (mapconcat #'identity mercit-buffer-diff-files " ")))
      (insert ?\n))))

;;;; Reference Headers

(defun mercit-insert-head-branch-header (&optional branch)
  "Insert a header line about the current branch.
If `HEAD' is detached, then insert information about that commit
instead.  The optional BRANCH argument is for internal use only."
  (let ((branch (or branch (mercit-get-current-branch)))
        (output (mercit-rev-format "{node|short} {desc|firstline}"
                                   (or branch "."))))
    (string-match "^\\([^ ]+\\) \\(.*\\)" output)
    (mercit-bind-match-strings (commit summary) output
      (when (equal summary "")
        (setq summary "(no commit message)"))
      (if branch
          (mercit-insert-section (branch branch)
            (insert (format "%-10s" "Head: "))
            (when mercit-status-show-hashes-in-headers
              (insert (propertize commit 'font-lock-face 'mercit-hash) ?\s))
            (insert (propertize branch 'font-lock-face 'mercit-branch-local))
            (insert ?\s)
            (insert (funcall mercit-log-format-message-function branch summary))
            (insert ?\n))
        (mercit-insert-section (commit commit)
          (insert (format "%-10s" "Head: "))
          (insert (propertize commit 'font-lock-face 'mercit-hash))
          (insert ?\s)
          (insert (funcall mercit-log-format-message-function nil summary))
          (insert ?\n))))))

(defun mercit-insert-upstream-branch-header (&optional branch upstream keyword)
  "Insert a header line about the upstream of the current branch.
If no branch is checked out, then insert nothing.  The optional
arguments are for internal use only."
  (when-let ((branch (or branch (mercit-get-current-branch))))
    (let ((remote (mercit-get "branch" branch "remote"))
          (merge  (mercit-get "branch" branch "merge"))
          (rebase (mercit-get "branch" branch "rebase")))
      (when (or remote merge)
        (unless upstream
          (setq upstream (mercit-get-upstream-branch branch)))
        (mercit-insert-section (branch upstream)
          (pcase rebase
            ("true")
            ("false" (setq rebase nil))
            (_       (setq rebase (mercit-get-boolean "pull.rebase"))))
          (insert (format "%-10s" (or keyword (if rebase "Rebase: " "Merge: "))))
          (insert
           (if upstream
               (concat (and mercit-status-show-hashes-in-headers
                            (concat (propertize (mercit-rev-format "%h" upstream)
                                                'font-lock-face 'mercit-hash)
                                    " "))
                       upstream " "
                       (funcall mercit-log-format-message-function upstream
                                (funcall mercit-log-format-message-function nil
                                         (or (mercit-rev-format "%s" upstream)
                                             "(no commit message)"))))
             (cond
              ((mercit--unnamed-upstream-p remote merge)
               (concat (propertize merge  'font-lock-face 'mercit-branch-remote)
                       " from "
                       (propertize remote 'font-lock-face 'bold)))
              ((mercit--valid-upstream-p remote merge)
               (if (equal remote ".")
                   (concat
                    (propertize merge 'font-lock-face 'mercit-branch-local) " "
                    (propertize "does not exist"
                                'font-lock-face 'mercit-branch-warning))
                 (format
                  "%s %s %s"
                  (propertize merge 'font-lock-face 'mercit-branch-remote)
                  (propertize "does not exist on"
                              'font-lock-face 'mercit-branch-warning)
                  (propertize remote 'font-lock-face 'mercit-branch-remote))))
              (t
               (propertize "invalid upstream configuration"
                           'font-lock-face 'mercit-branch-warning)))))
          (insert ?\n))))))

(defun mercit-insert-push-branch-header ()
  "Insert a header line about the branch the current branch is pushed to."
  (when-let* ((branch (mercit-get-current-branch))
              (target (mercit-get-push-branch branch)))
    (mercit-insert-section (branch target)
      (insert (format "%-10s" "Push: "))
      (insert
       (if (mercit-rev-verify target)
           (concat (and mercit-status-show-hashes-in-headers
                        (concat (propertize (mercit-rev-format "%h" target)
                                            'font-lock-face 'mercit-hash)
                                " "))
                   target " "
                   (funcall mercit-log-format-message-function target
                            (funcall mercit-log-format-message-function nil
                                     (or (mercit-rev-format "%s" target)
                                         "(no commit message)"))))
         (let ((remote (mercit-get-push-remote branch)))
           (if (mercit-remote-p remote)
               (concat target " "
                       (propertize "does not exist"
                                   'font-lock-face 'mercit-branch-warning))
             (concat remote " "
                     (propertize "remote does not exist"
                                 'font-lock-face 'mercit-branch-warning))))))
      (insert ?\n))))

(defun mercit-insert-tags-header ()
  "Insert a header line about the current and/or next tag."
  (let* ((this-tag (mercit-get-current-tag nil t))
         (next-tag (mercit-get-next-tag nil t))
         (this-cnt (cadr this-tag))
         (next-cnt (cadr next-tag))
         (this-tag (car this-tag))
         (next-tag (car next-tag))
         (both-tags (and this-tag next-tag t)))
    (when (or this-tag next-tag)
      (mercit-insert-section (tag (or this-tag next-tag))
        (insert (format "%-10s" (if both-tags "Tags: " "Tag: ")))
        (cl-flet ((insert-count (tag count face)
                    (insert (concat (propertize tag 'font-lock-face 'mercit-tag)
                                    (and (> count 0)
                                         (format " (%s)"
                                                 (propertize
                                                  (format "%s" count)
                                                  'font-lock-face face)))))))
          (when this-tag  (insert-count this-tag this-cnt 'mercit-branch-local))
          (when both-tags (insert ", "))
          (when next-tag  (insert-count next-tag next-cnt 'mercit-tag)))
        (insert ?\n)))))

;;;; Auxiliary Headers

(defun mercit-insert-user-header ()
  "Insert a header line about the current user."
  (let ((name  (mercit-get "user.name"))
        (email (mercit-get "user.email")))
    (when (and name email)
      (mercit-insert-section (user name)
        (insert (format "%-10s" "User: "))
        (insert (propertize name 'font-lock-face 'mercit-log-author))
        (insert " <" email ">\n")))))

(defun mercit-insert-repo-header ()
  "Insert a header line showing the path to the repository top-level."
  (let ((topdir (mercit-toplevel)))
    (mercit-insert-section (repo topdir)
      (insert (format "%-10s%s\n" "Repo: " (abbreviate-file-name topdir))))))

(defun mercit-insert-remote-header ()
  "Insert a header line about the remote of the current branch.

If no remote is configured for the current branch, then fall back
showing the \"origin\" remote, or if that does not exist the first
remote in alphabetic order."
  (when-let* ((name (mercit-get-some-remote))
              ;; Under certain configurations it's possible for
              ;; url to be nil, when name is not, see #2858.
              (url (mercit-get "remote" name "url")))
    (mercit-insert-section (remote name)
      (insert (format "%-10s" "Remote: "))
      (insert (propertize name 'font-lock-face 'mercit-branch-remote) ?\s)
      (insert url ?\n))))

;;;; File Sections

(defvar mercit-untracked-section-map
  (let ((map (make-sparse-keymap)))
    (mercit-menu-set map [mercit-stage-file]   #'mercit-stage   "Stage files")
    (mercit-menu-set map [mercit-delete-thing] #'mercit-discard "Discard files")
    map)
  "Keymap for the `untracked' section.")

(mercit-define-section-jumper mercit-jump-to-untracked "Untracked files" untracked)

(defun mercit-insert-untracked-files ()
  "Maybe insert a list or tree of untracked files.

Do so depending on the value of `status.showUntrackedFiles'.
Note that even if the value is `all', Mercit still initially
only shows directories.  But the directory sections can then
be expanded using \"TAB\".

If the first element of `mercit-buffer-diff-files' is a
directory, then limit the list to files below that.  The value
value of that variable can be set using \"D -- DIRECTORY RET g\"."
  (let* ((show (or (mercit-get "status.showUntrackedFiles") "normal"))
         (base (car mercit-buffer-diff-files))
         (base (and base (file-directory-p base) base)))
    (unless (equal show "no")
      (if (equal show "all")
          (when-let ((files (mercit-untracked-files nil base)))
            (mercit-insert-section (untracked)
              (mercit-insert-heading "Untracked files:")
              (mercit-insert-files files base)
              (insert ?\n)))
        (when-let ((files
                    (--mapcat (and (eq (aref it 0) ??)
                                   (list (substring it 2)))
                              (mercit-git-items "status" "--print0"
                                               (mercit-ignore-submodules-p t)
                                               "--" base))))
          (mercit-insert-section (untracked)
            (mercit-insert-heading "Untracked files:")
            (dolist (file files)
              (mercit-insert-section (file file)
                (insert (propertize file 'font-lock-face 'mercit-filename) ?\n)))
            (insert ?\n)))))))

(mercit-define-section-jumper mercit-jump-to-tracked "Tracked files" tracked)

(defun mercit-insert-tracked-files ()
  "Insert a tree of tracked files.

If the first element of `mercit-buffer-diff-files' is a
directory, then limit the list to files below that.  The value
value of that variable can be set using \"D -- DIRECTORY RET g\"."
  (when-let ((files (mercit-list-files)))
    (let* ((base (car mercit-buffer-diff-files))
           (base (and base (file-directory-p base) base)))
      (mercit-insert-section (tracked nil t)
        (mercit-insert-heading "Tracked files:")
        (mercit-insert-files files base)
        (insert ?\n)))))

(defun mercit-insert-ignored-files ()
  "Insert a tree of ignored files.

If the first element of `mercit-buffer-diff-files' is a
directory, then limit the list to files below that.  The value
of that variable can be set using \"D -- DIRECTORY RET g\"."
  (when-let ((files (mercit-ignored-files)))
    (let* ((base (car mercit-buffer-diff-files))
           (base (and base (file-directory-p base) base)))
      (mercit-insert-section (tracked nil t)
        (mercit-insert-heading "Ignored files:")
        (mercit-insert-files files base)
        (insert ?\n)))))

(mercit-define-section-jumper mercit-jump-to-skip-worktree "Skip-worktree files" skip-worktree)

(defun mercit-insert-skip-worktree-files ()
  "Insert a tree of skip-worktree files.

If the first element of `mercit-buffer-diff-files' is a
directory, then limit the list to files below that.  The value
of that variable can be set using \"D -- DIRECTORY RET g\"."
  (when-let ((files (mercit-skip-worktree-files)))
    (let* ((base (car mercit-buffer-diff-files))
           (base (and base (file-directory-p base) base)))
      (mercit-insert-section (skip-worktree nil t)
        (mercit-insert-heading "Skip-worktree files:")
        (mercit-insert-files files base)
        (insert ?\n)))))

(mercit-define-section-jumper mercit-jump-to-assume-unchanged "Assume-unchanged files" assume-unchanged)

(defun mercit-insert-assume-unchanged-files ()
  "Insert a tree of files that are assumed to be unchanged.

If the first element of `mercit-buffer-diff-files' is a
directory, then limit the list to files below that.  The value
of that variable can be set using \"D -- DIRECTORY RET g\"."
  (when-let ((files (mercit-assume-unchanged-files)))
    (let* ((base (car mercit-buffer-diff-files))
           (base (and base (file-directory-p base) base)))
      (mercit-insert-section (assume-unchanged nil t)
        (mercit-insert-heading "Assume-unchanged files:")
        (mercit-insert-files files base)
        (insert ?\n)))))

(defun mercit-insert-files (files directory)
  (while (and files (string-prefix-p (or directory "") (car files)))
    (let ((dir (file-name-directory (car files))))
      (if (equal dir directory)
          (let ((file (pop files)))
            (mercit-insert-section (file file)
              (insert (propertize file 'font-lock-face 'mercit-filename) ?\n)))
        (mercit-insert-section (file dir t)
          (insert (propertize dir 'file 'mercit-filename) ?\n)
          (mercit-insert-heading)
          (setq files (mercit-insert-files files dir))))))
  files)

;;; _
(provide 'mercit-status)
;;; mercit-status.el ends here
