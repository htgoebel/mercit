;;; mercit-mode.el --- Create and refresh Mercit buffers  -*- lexical-binding:t -*-

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

;; This library implements the abstract major-mode `mercit-mode' from
;; which almost all other Mercit major-modes derive.  The code in here
;; is mostly concerned with creating and refreshing Mercit buffers.

;;; Code:

(require 'mercit-base)
(require 'mercit-git)

(require 'format-spec)
(require 'help-mode)
(require 'transient)

;;; Options

(defcustom mercit-mode-hook
  '(mercit-load-config-extensions)
  "Hook run when entering a mode derived from Mercit mode."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-modes
  :type 'hook
  :options '(mercit-load-config-extensions
             bug-reference-mode))

(defcustom mercit-setup-buffer-hook
  '(mercit-maybe-save-repository-buffers
    mercit-set-buffer-margin)
  "Hook run by `mercit-setup-buffer'.

This is run right after displaying the buffer and right before
generating or updating its content.  `mercit-mode-hook' and other,
more specific, `mercit-mode-*-hook's on the other hand are run
right before displaying the buffer.  Usually one of these hooks
should be used instead of this one."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-modes
  :type 'hook
  :options '(mercit-maybe-save-repository-buffers
             mercit-set-buffer-margin))

(defcustom mercit-pre-refresh-hook '(mercit-maybe-save-repository-buffers)
  "Hook run before refreshing in `mercit-refresh'.

This hook, or `mercit-post-refresh-hook', should be used
for functions that are not tied to a particular buffer.

To run a function with a particular buffer current, use
`mercit-refresh-buffer-hook' and use `derived-mode-p'
inside your function."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-refresh
  :type 'hook
  :options '(mercit-maybe-save-repository-buffers))

(defcustom mercit-post-refresh-hook
  '(mercit-auto-revert-buffers
    mercit-run-post-commit-hook
    mercit-run-post-stage-hook
    mercit-run-post-unstage-hook)
  "Hook run after refreshing in `mercit-refresh'.

This hook, or `mercit-pre-refresh-hook', should be used
for functions that are not tied to a particular buffer.

To run a function with a particular buffer current, use
`mercit-refresh-buffer-hook' and use `derived-mode-p'
inside your function."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-refresh
  :type 'hook
  :options '(mercit-auto-revert-buffers
             mercit-run-post-commit-hook
             mercit-run-post-stage-hook
             mercit-run-post-unstage-hook))

(defcustom mercit-display-buffer-function #'mercit-display-buffer-traditional
  "The function used to display a Mercit buffer.

All Mercit buffers (buffers whose major-modes derive from
`mercit-mode') are displayed using `mercit-display-buffer',
which in turn uses the function specified here."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-buffers
  :type '(radio (function-item mercit-display-buffer-traditional)
                (function-item mercit-display-buffer-same-window-except-diff-v1)
                (function-item mercit-display-buffer-fullframe-status-v1)
                (function-item mercit-display-buffer-fullframe-status-topleft-v1)
                (function-item mercit-display-buffer-fullcolumn-most-v1)
                (function-item display-buffer)
                (function :tag "Function")))

(defcustom mercit-pre-display-buffer-hook '(mercit-save-window-configuration)
  "Hook run by `mercit-display-buffer' before displaying the buffer."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-buffers
  :type 'hook
  :get #'mercit-hook-custom-get
  :options '(mercit-save-window-configuration))

(defcustom mercit-post-display-buffer-hook '(mercit-maybe-set-dedicated)
  "Hook run by `mercit-display-buffer' after displaying the buffer."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-buffers
  :type 'hook
  :get #'mercit-hook-custom-get
  :options '(mercit-maybe-set-dedicated))

(defcustom mercit-generate-buffer-name-function
  #'mercit-generate-buffer-name-default-function
  "The function used to generate the name for a Mercit buffer."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-buffers
  :type '(radio (function-item mercit-generate-buffer-name-default-function)
                (function :tag "Function")))

(defcustom mercit-buffer-name-format "%x%M%v: %t%x"
  "The format string used to name Mercit buffers.

The following %-sequences are supported:

`%m' The name of the major-mode, but with the `-mode' suffix
     removed.

`%M' Like \"%m\" but abbreviate `mercit-status-mode' as `mercit'.

`%v' The value the buffer is locked to, in parentheses, or an
     empty string if the buffer is not locked to a value.

`%V' Like \"%v\", but the string is prefixed with a space, unless
     it is an empty string.

`%t' The top-level directory of the working tree of the
     repository, or if `mercit-uniquify-buffer-names' is non-nil
     an abbreviation of that.

`%x' If `mercit-uniquify-buffer-names' is nil \"*\", otherwise the
     empty string.  Due to limitations of the `uniquify' package,
     buffer names must end with the path.

`%T' Obsolete, use \"%t%x\" instead.  Like \"%t\", but append an
     asterisk if and only if `mercit-uniquify-buffer-names' is nil.

The value should always contain \"%m\" or \"%M\", \"%v\" or
\"%V\", and \"%t\" (or the obsolete \"%T\").

If `mercit-uniquify-buffer-names' is non-nil, then the value must
end with \"%t\" or \"%t%x\" (or the obsolete \"%T\").  See issue
#2841.

This is used by `mercit-generate-buffer-name-default-function'.
If another `mercit-generate-buffer-name-function' is used, then
it may not respect this option, or on the contrary it may
support additional %-sequences."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-buffers
  :type 'string)

(defcustom mercit-uniquify-buffer-names t
  "Whether to uniquify the names of Mercit buffers."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-buffers
  :type 'boolean)

(defcustom mercit-bury-buffer-function #'mercit-mode-quit-window
  "The function used to bury or kill the current Mercit buffer."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-buffers
  :type '(radio (function-item quit-window)
                (function-item mercit-mode-quit-window)
                (function-item mercit-restore-window-configuration)
                (function :tag "Function")))

(defcustom mercit-prefix-use-buffer-arguments 'selected
  "Whether certain prefix commands reuse arguments active in relevant buffer.

This affects the transient prefix commands `mercit-diff',
`mercit-log' and `mercit-show-refs'.

Valid values are:

`always': Always use the set of arguments that is currently
  active in the respective buffer, provided that buffer exists
  of course.
`selected': Use the set of arguments from the respective
  buffer, but only if it is displayed in a window of the current
  frame.  This is the default.
`current': Use the set of arguments from the respective buffer,
  but only if it is the current buffer.
`never': Never use the set of arguments from the respective
  buffer.

For more information see info node `(mercit)Transient Arguments
and Buffer Variables'."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-buffers
  :group 'mercit-commands
  :group 'mercit-diff
  :group 'mercit-log
  :type '(choice
          (const :tag "always use args from buffer" always)
          (const :tag "use args from buffer if displayed in frame" selected)
          (const :tag "use args from buffer if it is current" current)
          (const :tag "never use args from buffer" never)))

(defcustom mercit-direct-use-buffer-arguments 'selected
  "Whether certain commands reuse arguments active in relevant buffer.

This affects certain commands such as `mercit-show-commit' that
are suffixes of the diff or log transient prefix commands, but
only if they are invoked directly, i.e. *not* as a suffix.

Valid values are:

`always': Always use the set of arguments that is currently
  active in the respective buffer, provided that buffer exists
  of course.
`selected': Use the set of arguments from the respective
  buffer, but only if it is displayed in a window of the current
  frame.  This is the default.
`current': Use the set of arguments from the respective buffer,
  but only if it is the current buffer.
`never': Never use the set of arguments from the respective
  buffer.

For more information see info node `(mercit)Transient Arguments
and Buffer Variables'."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-buffers
  :group 'mercit-commands
  :group 'mercit-diff
  :group 'mercit-log
  :type '(choice
          (const :tag "always use args from buffer" always)
          (const :tag "use args from buffer if displayed in frame" selected)
          (const :tag "use args from buffer if it is current" current)
          (const :tag "never use args from buffer" never)))

(defcustom mercit-region-highlight-hook '(mercit-diff-update-hunk-region)
  "Functions used to highlight the region.

Each function is run with the current section as only argument
until one of them returns non-nil.  If all functions return nil,
then fall back to regular region highlighting."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-refresh
  :type 'hook
  :options '(mercit-diff-update-hunk-region))

(defcustom mercit-create-buffer-hook nil
  "Normal hook run after creating a new `mercit-mode' buffer."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-refresh
  :type 'hook)

(defcustom mercit-refresh-buffer-hook nil
  "Normal hook for `mercit-refresh-buffer' to run after refreshing."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-refresh
  :type 'hook)

(defcustom mercit-refresh-status-buffer t
  "Whether the status buffer is refreshed after running git.

When this is non-nil, then the status buffer is automatically
refreshed after running git for side-effects, in addition to the
current Mercit buffer, which is always refreshed automatically.

Only set this to nil after exhausting all other options to
improve performance."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-refresh
  :group 'mercit-status
  :type 'boolean)

(defcustom mercit-refresh-verbose nil
  "Whether to revert Mercit buffers verbosely."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-refresh
  :type 'boolean)

(defcustom mercit-save-repository-buffers t
  "Whether to save file-visiting buffers when appropriate.

If non-nil, then all modified file-visiting buffers belonging
to the current repository may be saved before running Mercit
commands and before creating or refreshing Mercit buffers.
If `dontask', then this is done without user intervention, for
any other non-nil value the user has to confirm each save.

The default is t to avoid surprises, but `dontask' is the
recommended value."
  :group 'mercit-essentials
  :group 'mercit-buffers
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Ask" t)
                 (const :tag "Save without asking" dontask)))

;;; Key Bindings

(defvar mercit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mercit-section-mode-map)
    ;; Don't function-quote but make sure all commands are autoloaded.
    (define-key map [C-return]    'mercit-visit-thing)
    (define-key map (kbd   "RET") 'mercit-visit-thing)
    (define-key map (kbd "M-TAB") 'mercit-dired-jump)
    (define-key map [M-tab]       'mercit-section-cycle-diffs)
    (define-key map (kbd   "SPC") 'mercit-diff-show-or-scroll-up)
    (define-key map (kbd "S-SPC") 'mercit-diff-show-or-scroll-down)
    (define-key map (kbd   "DEL") 'mercit-diff-show-or-scroll-down)
    (define-key map "+"           'mercit-diff-more-context)
    (define-key map "-"           'mercit-diff-less-context)
    (define-key map "0"           'mercit-diff-default-context)
    (define-key map "a" 'mercit-cherry-apply)
    (define-key map "A" 'mercit-cherry-pick)
    (define-key map "b" 'mercit-branch)
    (define-key map "B" 'mercit-bisect)
    (define-key map "c" 'mercit-commit)
    (define-key map "C" 'mercit-clone)
    (define-key map "d" 'mercit-diff)
    (define-key map "D" 'mercit-diff-refresh)
    (define-key map "e" 'mercit-ediff-dwim)
    (define-key map "E" 'mercit-ediff)
    (define-key map "f" 'mercit-fetch)
    (define-key map "F" 'mercit-pull)
    (define-key map "g" 'mercit-refresh)
    (define-key map "G" 'mercit-refresh-all)
    (define-key map "h" 'mercit-dispatch)
    (define-key map "?" 'mercit-dispatch)
    (define-key map "H" 'mercit-describe-section)
    (define-key map "i" 'mercit-gitignore)
    (define-key map "I" 'mercit-init)
    (define-key map "j" 'mercit-status-quick)
    (define-key map "J" 'mercit-display-repository-buffer)
    (define-key map "k" 'mercit-delete-thing)
    (define-key map "K" 'mercit-file-untrack)
    (define-key map "l" 'mercit-log)
    (define-key map "L" 'mercit-log-refresh)
    (define-key map "m" 'mercit-merge)
    (define-key map "M" 'mercit-remote)
    ;;  section-map "n"  mercit-section-forward
    ;;     reserved "N"  forge-dispatch
    (define-key map "o" 'mercit-submodule)
    (define-key map "O" 'mercit-subtree)
    ;;  section-map "p"  mercit-section-backward
    (define-key map "P" 'mercit-push)
    (define-key map "q" 'mercit-mode-bury-buffer)
    (define-key map "Q" 'mercit-git-command)
    (define-key map ":" 'mercit-git-command)
    (define-key map "r" 'mercit-rebase)
    (define-key map "R" 'mercit-file-rename)
    (define-key map "s" 'mercit-stage-file)
    (define-key map "S" 'mercit-stage-modified)
    (define-key map "t" 'mercit-tag)
    (define-key map "T" 'mercit-notes)
    (define-key map "u" 'mercit-unstage-file)
    (define-key map "U" 'mercit-unstage-all)
    (define-key map "v" 'mercit-revert-no-commit)
    (define-key map "V" 'mercit-revert)
    (define-key map "w" 'mercit-am)
    (define-key map "W" 'mercit-patch)
    (define-key map "x" 'mercit-reset-quickly)
    (define-key map "X" 'mercit-reset)
    (define-key map "y" 'mercit-show-refs)
    (define-key map "Y" 'mercit-cherry)
    (define-key map "z" 'mercit-stash)
    (define-key map "Z" 'mercit-worktree)
    (define-key map "%" 'mercit-worktree)
    (define-key map "$" 'mercit-process-buffer)
    (define-key map "!" 'mercit-run)
    (define-key map ">" 'mercit-sparse-checkout)
    (define-key map (kbd "C-c C-c") 'mercit-dispatch)
    (define-key map (kbd "C-c C-e") 'mercit-edit-thing)
    (define-key map (kbd "C-c C-o") 'mercit-browse-thing)
    (define-key map (kbd "C-c C-w") 'mercit-browse-thing)
    (define-key map (kbd "C-w")     'mercit-copy-section-value)
    (define-key map (kbd "M-w")     'mercit-copy-buffer-revision)
    (define-key map [remap previous-line]      'mercit-previous-line)
    (define-key map [remap next-line]          'mercit-next-line)
    (define-key map [remap evil-previous-line] 'evil-previous-visual-line)
    (define-key map [remap evil-next-line]     'evil-next-visual-line)
    map)
  "Parent keymap for all keymaps of modes derived from `mercit-mode'.")

(defun mercit-delete-thing ()
  "This is a placeholder command.
Where applicable, section-specific keymaps bind another command
which deletes the thing at point."
  (interactive)
  (user-error "There is no thing at point that could be deleted"))

(defun mercit-visit-thing ()
  "This is a placeholder command.
Where applicable, section-specific keymaps bind another command
which visits the thing at point."
  (interactive)
  (if (eq transient-current-command 'mercit-dispatch)
      (call-interactively (key-binding (this-command-keys)))
    (user-error "There is no thing at point that could be visited")))

(defun mercit-edit-thing ()
  "This is a placeholder command.
Where applicable, section-specific keymaps bind another command
which lets you edit the thing at point, likely in another buffer."
  (interactive)
  (if (eq transient-current-command 'mercit-dispatch)
      (call-interactively (key-binding (this-command-keys)))
    (user-error "There is no thing at point that could be edited")))

(defun mercit-browse-thing ()
  "This is a placeholder command.
Where applicable, section-specific keymaps bind another command
which visits the thing at point using `browse-url'."
  (interactive)
  (user-error "There is no thing at point that could be browsed"))

;;;###autoload
(defun mercit-info ()
  "Visit the Mercit manual."
  (interactive)
  (info "mercit"))

(defvar bug-reference-map)
(with-eval-after-load 'bug-reference
  (define-key bug-reference-map [remap mercit-visit-thing]
    'bug-reference-push-button))

(easy-menu-define mercit-mode-menu mercit-mode-map
  "Mercit menu"
  ;; Similar to `mercit-dispatch' but exclude:
  ;; - commands that are available from context menus:
  ;;   apply, reverse, discard, stage, unstage,
  ;;   cherry-pick, revert, reset,
  ;;   describe-section
  ;; - commands that are available from submenus:
  ;;   git-command, ediff-dwim
  ;; - and: refresh-all, status-jump, status-quick.
  '("Mercit"
    "---" "Inspect"
    ["     Bisect..."             mercit-bisect t]
    ["     Cherries..."           mercit-cherry t]
    ["     Diff..."               mercit-diff t]
    ["     Ediff..."              mercit-ediff t]
    ["     Log..."                mercit-log t]
    ["     References..."         mercit-show-refs t]
    "---" "Manipulate"
    ["     Commit..."             mercit-commit t]
    ["     Stash..."              mercit-stash t]
    ["     Tag..."                mercit-tag t]
    "---"
    ["     Branch..."             mercit-branch t]
    ["     Remote..."             mercit-remote t]
    "---"
    ["     Merge..."              mercit-merge t]
    ["     Rebase..."             mercit-rebase t]
    "---" "Transfer"
    ["     Fetch..."              mercit-fetch t]
    ["     Pull..."               mercit-pull t]
    ["     Push..."               mercit-push t]
    "---" "Setup"
    ["     Clone..."              mercit-clone t]
    ["     Ignore..."             mercit-gitignore t]
    ["     Init..."               mercit-init t]
    "---"
    ("Advanced"
     ["Run..."                    mercit-run t]
     "---"
     ["Apply patches..."          mercit-am t]
     ["Format patches..."         mercit-patch t]
     "---"
     ["Note..."                   mercit-notes t]
     "---"
     ["Submodule..."              mercit-submodule t]
     ["Subtree..."                mercit-subtree t]
     ["Worktree..."               mercit-worktree t])
    "---"
    ["Show command dispatcher..." mercit-dispatch t]
    ["Show manual"                mercit-info t]
    ["Show another buffer"        mercit-display-repository-buffer t]
    "---"
    ("Change buffer arguments"
     ["Diff arguments"            mercit-diff-refresh t]
     ["Log arguments"             mercit-log-refresh t])
    ["Refresh buffer"             mercit-refresh t]
    ["Bury buffer"                mercit-mode-bury-buffer t]))

;;; Mode

(defun mercit-load-config-extensions ()
  "Load Mercit extensions that are defined at the Mercurial config layer."
  (dolist (ext (mercit-get-all "mercit.extension"))
    (let ((sym (intern (format "mercit-%s-mode" ext))))
      (when (fboundp sym)
        (funcall sym 1)))))

(define-derived-mode mercit-mode mercit-section-mode "Mercit"
  "Parent major mode from which Mercit major modes inherit.

Mercit is documented in info node `(mercit)'."
  :group 'mercit
  (hack-dir-local-variables-non-file-buffer)
  (face-remap-add-relative 'header-line 'mercit-header-line)
  (setq mode-line-process (mercit-repository-local-get 'mode-line-process))
  (setq-local revert-buffer-function #'mercit-refresh-buffer)
  (setq-local bookmark-make-record-function #'mercit--make-bookmark)
  (setq-local imenu-create-index-function #'mercit--imenu-create-index)
  (setq-local isearch-filter-predicate #'mercit-section--open-temporarily))

;;; Local Variables

(defvar-local mercit-buffer-arguments nil)
(defvar-local mercit-buffer-diff-args nil)
(defvar-local mercit-buffer-diff-files nil)
(defvar-local mercit-buffer-diff-files-suspended nil)
(defvar-local mercit-buffer-file-name nil)
(defvar-local mercit-buffer-files nil)
(defvar-local mercit-buffer-log-args nil)
(defvar-local mercit-buffer-log-files nil)
(defvar-local mercit-buffer-range nil)
(defvar-local mercit-buffer-range-hashed nil)
(defvar-local mercit-buffer-refname nil)
(defvar-local mercit-buffer-revision nil)
(defvar-local mercit-buffer-revision-hash nil)
(defvar-local mercit-buffer-revisions nil)
(defvar-local mercit-buffer-typearg nil)
(defvar-local mercit-buffer-upstream nil)

;; These variables are also used in file-visiting buffers.
;; Because the user may change the major-mode, they have
;; to be permanent buffer-local.
(put 'mercit-buffer-file-name 'permanent-local t)
(put 'mercit-buffer-refname 'permanent-local t)
(put 'mercit-buffer-revision 'permanent-local t)
(put 'mercit-buffer-revision-hash 'permanent-local t)

;; `mercit-status' re-enables mode function but its refresher
;; function does not reinstate this.
(put 'mercit-buffer-diff-files-suspended 'permanent-local t)

(defvar-local mercit-refresh-args nil
  "Obsolete.  Possibly the arguments used to refresh the current buffer.
Some third-party packages might still use this, but Mercit does not.")
(put 'mercit-refresh-args 'permanent-local t)
(make-obsolete-variable 'mercit-refresh-args nil "Mercit 3.0.0")

(defvar mercit-buffer-lock-functions nil
  "Obsolete buffer-locking support for third-party modes.
Implement the generic function `mercit-buffer-value' for
your mode instead of adding an entry to this variable.")
(make-obsolete-variable 'mercit-buffer-lock-functions nil "Mercit 3.0.0")

(cl-defgeneric mercit-buffer-value ()
  (and-let* ((fn (cdr (assq major-mode mercit-buffer-lock-functions))))
    (funcall fn (with-no-warnings mercit-refresh-args))))

(defvar-local mercit-previous-section nil)
(put 'mercit-previous-section 'permanent-local t)

(defvar-local mercit--imenu-group-types nil)
(defvar-local mercit--imenu-item-types nil)

;;; Setup Buffer

(defmacro mercit-setup-buffer (mode &optional locked &rest bindings)
  (declare (indent 2))
  `(mercit-setup-buffer-internal
    ,mode ,locked
    ,(cons 'list (mapcar (pcase-lambda (`(,var ,form))
                           `(list ',var ,form))
                         bindings))))

(defun mercit-setup-buffer-internal (mode locked bindings)
  (let* ((value   (and locked
                       (with-temp-buffer
                         (pcase-dolist (`(,var ,val) bindings)
                           (set (make-local-variable var) val))
                         (let ((major-mode mode))
                           (mercit-buffer-value)))))
         (buffer  (mercit-get-mode-buffer mode value))
         (section (and buffer (mercit-current-section)))
         (created (not buffer)))
    (unless buffer
      (setq buffer (mercit-generate-new-buffer mode value)))
    (with-current-buffer buffer
      (setq mercit-previous-section section)
      (funcall mode)
      (mercit-xref-setup #'mercit-setup-buffer-internal bindings)
      (pcase-dolist (`(,var ,val) bindings)
        (set (make-local-variable var) val))
      (when created
        (run-hooks 'mercit-create-buffer-hook)))
    (mercit-display-buffer buffer)
    (with-current-buffer buffer
      (run-hooks 'mercit-setup-buffer-hook)
      (mercit-refresh-buffer))
    buffer))

(defun mercit-mode-setup (mode &rest args)
  "Setup up a MODE buffer using ARGS to generate its content."
  (declare (obsolete mercit-setup-buffer "Mercit 3.0.0"))
  (with-no-warnings
    (mercit-mode-setup-internal mode args)))

(defun mercit-mode-setup-internal (mode args &optional locked)
  "Setup up a MODE buffer using ARGS to generate its content.
When optional LOCKED is non-nil, then create a buffer that is
locked to its value, which is derived from MODE and ARGS."
  (declare (obsolete mercit-setup-buffer "Mercit 3.0.0"))
  (let* ((value   (and locked
                       (with-temp-buffer
                         (with-no-warnings
                           (setq mercit-refresh-args args))
                         (let ((major-mode mode))
                           (mercit-buffer-value)))))
         (buffer  (mercit-get-mode-buffer mode value))
         (section (and buffer (mercit-current-section)))
         (created (not buffer)))
    (unless buffer
      (setq buffer (mercit-generate-new-buffer mode value)))
    (with-current-buffer buffer
      (setq mercit-previous-section section)
      (with-no-warnings
        (setq mercit-refresh-args args))
      (funcall mode)
      (mercit-xref-setup 'mercit-mode-setup-internal args)
      (when created
        (run-hooks 'mercit-create-buffer-hook)))
    (mercit-display-buffer buffer)
    (with-current-buffer buffer
      (run-hooks 'mercit-mode-setup-hook)
      (mercit-refresh-buffer))))

;;; Display Buffer

(defvar mercit-display-buffer-noselect nil
  "If non-nil, then `mercit-display-buffer' doesn't call `select-window'.")

(defun mercit-display-buffer (buffer &optional display-function)
  "Display BUFFER in some window and maybe select it.

If optional DISPLAY-FUNCTION is non-nil, then use that to display
the buffer.  Otherwise use `mercit-display-buffer-function', which
is the normal case.

Then, unless `mercit-display-buffer-noselect' is non-nil, select
the window which was used to display the buffer.

Also run the hooks `mercit-pre-display-buffer-hook'
and `mercit-post-display-buffer-hook'."
  (with-current-buffer buffer
    (run-hooks 'mercit-pre-display-buffer-hook))
  (let ((window (funcall (or display-function mercit-display-buffer-function)
                         buffer)))
    (unless mercit-display-buffer-noselect
      (let* ((old-frame (selected-frame))
             (new-frame (window-frame window)))
        (select-window window)
        (unless (eq old-frame new-frame)
          (select-frame-set-input-focus new-frame)))))
  (with-current-buffer buffer
    (run-hooks 'mercit-post-display-buffer-hook)))

(defun mercit-display-buffer-traditional (buffer)
  "Display BUFFER the way this has traditionally been done."
  (display-buffer
   buffer (if (and (derived-mode-p 'mercit-mode)
                   (not (memq (with-current-buffer buffer major-mode)
                              '(mercit-process-mode
                                mercit-revision-mode
                                mercit-diff-mode
                                mercit-stash-mode
                                mercit-status-mode))))
              '(display-buffer-same-window)
            nil))) ; display in another window

(defun mercit-display-buffer-same-window-except-diff-v1 (buffer)
  "Display BUFFER in the selected window except for some modes.
If a buffer's `major-mode' derives from `mercit-diff-mode' or
`mercit-process-mode', display it in another window.  Display all
other buffers in the selected window."
  (display-buffer
   buffer (if (with-current-buffer buffer
                (derived-mode-p 'mercit-diff-mode 'mercit-process-mode))
              '(nil (inhibit-same-window . t))
            '(display-buffer-same-window))))

(defun mercit--display-buffer-fullframe (buffer alist)
  (when-let ((window (or (display-buffer-reuse-window buffer alist)
                         (display-buffer-same-window buffer alist)
                         (display-buffer-pop-up-window buffer alist)
                         (display-buffer-use-some-window buffer alist))))
    (delete-other-windows window)
    window))

(defun mercit-display-buffer-fullframe-status-v1 (buffer)
  "Display BUFFER, filling entire frame if BUFFER is a status buffer.
Otherwise, behave like `mercit-display-buffer-traditional'."
  (if (eq (with-current-buffer buffer major-mode)
          'mercit-status-mode)
      (display-buffer buffer '(mercit--display-buffer-fullframe))
    (mercit-display-buffer-traditional buffer)))

(defun mercit--display-buffer-topleft (buffer alist)
  (or (display-buffer-reuse-window buffer alist)
      (when-let ((window2 (display-buffer-pop-up-window buffer alist)))
        (let ((window1 (get-buffer-window))
              (buffer1 (current-buffer))
              (buffer2 (window-buffer window2))
              (w2-quit-restore (window-parameter window2 'quit-restore)))
          (set-window-buffer window1 buffer2)
          (set-window-buffer window2 buffer1)
          (select-window window2)
          ;; Swap some window state that `mercit-mode-quit-window' and
          ;; `quit-restore-window' inspect.
          (set-window-prev-buffers window2 (cdr (window-prev-buffers window1)))
          (set-window-prev-buffers window1 nil)
          (set-window-parameter window2 'mercit-dedicated
                                (window-parameter window1 'mercit-dedicated))
          (set-window-parameter window1 'mercit-dedicated t)
          (set-window-parameter window1 'quit-restore
                                (list 'window 'window
                                      (nth 2 w2-quit-restore)
                                      (nth 3 w2-quit-restore)))
          (set-window-parameter window2 'quit-restore nil)
          window1))))

(defun mercit-display-buffer-fullframe-status-topleft-v1 (buffer)
  "Display BUFFER, filling entire frame if BUFFER is a status buffer.
When BUFFER derives from `mercit-diff-mode' or
`mercit-process-mode', try to display BUFFER to the top or left of
the current buffer rather than to the bottom or right, as
`mercit-display-buffer-fullframe-status-v1' would.  Whether the
split is made vertically or horizontally is determined by
`split-window-preferred-function'."
  (display-buffer
   buffer
   (cond ((eq (with-current-buffer buffer major-mode)
              'mercit-status-mode)
          '(mercit--display-buffer-fullframe))
         ((with-current-buffer buffer
            (derived-mode-p 'mercit-diff-mode 'mercit-process-mode))
          '(mercit--display-buffer-topleft))
         (t
          '(display-buffer-same-window)))))

(defun mercit--display-buffer-fullcolumn (buffer alist)
  (when-let ((window (or (display-buffer-reuse-window buffer alist)
                         (display-buffer-same-window buffer alist)
                         (display-buffer-below-selected buffer alist))))
    (delete-other-windows-vertically window)
    window))

(defun mercit-display-buffer-fullcolumn-most-v1 (buffer)
  "Display BUFFER using the full column except in some cases.
For most cases where BUFFER's `major-mode' derives from
`mercit-mode', display it in the selected window and grow that
window to the full height of the frame, deleting other windows in
that column as necessary.  However, display BUFFER in another
window if 1) BUFFER's mode derives from `mercit-process-mode', or
2) BUFFER's mode derives from `mercit-diff-mode', provided that
the mode of the current buffer derives from `mercit-log-mode' or
`mercit-cherry-mode'."
  (display-buffer
   buffer
   (cond ((and (or (bound-and-true-p git-commit-mode)
                   (derived-mode-p 'mercit-log-mode
                                   'mercit-cherry-mode
                                   'mercit-reflog-mode))
               (with-current-buffer buffer
                 (derived-mode-p 'mercit-diff-mode)))
          nil)
         ((with-current-buffer buffer
            (derived-mode-p 'mercit-process-mode))
          nil)
         (t
          '(mercit--display-buffer-fullcolumn)))))

(defun mercit-maybe-set-dedicated ()
  "Mark the selected window as dedicated if appropriate.

If a new window was created to display the buffer, then remember
that fact.  That information is used by `mercit-mode-quit-window',
to determine whether the window should be deleted when its last
Mercit buffer is buried."
  (let ((window (get-buffer-window (current-buffer))))
    (when (and (window-live-p window)
               (not (window-prev-buffers window)))
      (set-window-parameter window 'mercit-dedicated t))))

;;; Get Buffer

(defvar-local mercit--default-directory nil
  "Value of `default-directory' when buffer is generated.
This exists to prevent a let-bound `default-directory' from
tricking `mercit-get-mode-buffer' or `mercit-mode-get-buffers'
into thinking a buffer belongs to a repo that it doesn't.")
(put 'mercit--default-directory 'permanent-local t)

(defun mercit-mode-get-buffers ()
  (let ((topdir (mercit-toplevel)))
    (--filter (with-current-buffer it
                (and (derived-mode-p 'mercit-mode)
                     (equal mercit--default-directory topdir)))
              (buffer-list))))

(defvar-local mercit-buffer-locked-p nil)
(put 'mercit-buffer-locked-p 'permanent-local t)

(defun mercit-get-mode-buffer (mode &optional value frame)
  "Return buffer belonging to the current repository whose major-mode is MODE.

If no such buffer exists then return nil.  Multiple buffers with
the same major-mode may exist for a repository but only one can
exist that hasn't been locked to its value.  Return that buffer
\(or nil if there is no such buffer) unless VALUE is non-nil, in
which case return the buffer that has been locked to that value.

If FRAME is nil or omitted, then consider all buffers.  Otherwise
  only consider buffers that are displayed in some live window
  on some frame.
If `all', then consider all buffers on all frames.
If `visible', then only consider buffers on all visible frames.
If `selected' or t, then only consider buffers on the selected
  frame.
If a frame, then only consider buffers on that frame."
  (let ((topdir (mercit--toplevel-safe)))
    (cl-flet* ((b (buffer)
                 (with-current-buffer buffer
                   (and (eq major-mode mode)
                        (equal mercit--default-directory topdir)
                        (if value
                            (and mercit-buffer-locked-p
                                 (equal (mercit-buffer-value) value))
                          (not mercit-buffer-locked-p))
                        buffer)))
               (w (window)
                 (b (window-buffer window)))
               (f (frame)
                 (seq-some #'w (window-list frame 'no-minibuf))))
      (pcase-exhaustive frame
        ('nil                   (seq-some #'b (buffer-list)))
        ('all                   (seq-some #'f (frame-list)))
        ('visible               (seq-some #'f (visible-frame-list)))
        ((or 'selected 't)      (seq-some #'w (window-list (selected-frame))))
        ((guard (framep frame)) (seq-some #'w (window-list frame)))))))

(defun mercit-mode-get-buffer (mode &optional create frame value)
  (declare (obsolete mercit-get-mode-buffer "Mercit 3.0.0"))
  (when create
    (error "`mercit-mode-get-buffer's CREATE argument is obsolete"))
  (let ((topdir (mercit--toplevel-safe)))
    (--first (with-current-buffer it
               (and (eq major-mode mode)
                    (equal mercit--default-directory topdir)
                    (if value
                        (and mercit-buffer-locked-p
                             (equal (mercit-buffer-value) value))
                      (not mercit-buffer-locked-p))))
             (if frame
                 (mapcar #'window-buffer
                         (window-list (unless (eq frame t) frame)))
               (buffer-list)))))

(defun mercit-generate-new-buffer (mode &optional value directory)
  (let* ((default-directory (or directory (mercit--toplevel-safe)))
         (name (funcall mercit-generate-buffer-name-function mode value))
         (buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (setq mercit--default-directory default-directory)
      (setq mercit-buffer-locked-p (and value t))
      (mercit-restore-section-visibility-cache mode))
    (when mercit-uniquify-buffer-names
      (add-to-list 'uniquify-list-buffers-directory-modes mode)
      (with-current-buffer buffer
        (setq list-buffers-directory (abbreviate-file-name default-directory)))
      (let ((uniquify-buffer-name-style
             (if (memq uniquify-buffer-name-style '(nil forward))
                 'post-forward-angle-brackets
               uniquify-buffer-name-style)))
        (uniquify-rationalize-file-buffer-names
         name (file-name-directory (directory-file-name default-directory))
         buffer)))
    buffer))

(defun mercit-generate-buffer-name-default-function (mode &optional value)
  "Generate buffer name for a MODE buffer in the current repository.
The returned name is based on `mercit-buffer-name-format' and
takes `mercit-uniquify-buffer-names' and VALUE, if non-nil, into
account."
  (let ((m (substring (symbol-name mode) 0 -5))
        (v (and value (format "%s" (if (listp value) value (list value)))))
        (n (if mercit-uniquify-buffer-names
               (file-name-nondirectory
                (directory-file-name default-directory))
             (abbreviate-file-name default-directory))))
    (format-spec
     mercit-buffer-name-format
     `((?m . ,m)
       (?M . ,(if (eq mode 'mercit-status-mode) "mercit" m))
       (?v . ,(or v ""))
       (?V . ,(if v (concat " " v) ""))
       (?t . ,n)
       (?x . ,(if mercit-uniquify-buffer-names "" "*"))
       (?T . ,(if mercit-uniquify-buffer-names n (concat n "*")))))))

;;; Buffer Lock

(defun mercit-toggle-buffer-lock ()
  "Lock the current buffer to its value or unlock it.

Locking a buffer to its value prevents it from being reused to
display another value.  The name of a locked buffer contains its
value, which allows telling it apart from other locked buffers
and the unlocked buffer.

Not all Mercit buffers can be locked to their values, for example
it wouldn't make sense to lock a status buffer.

There can only be a single unlocked buffer using a certain
major-mode per repository.  So when a buffer is being unlocked
and another unlocked buffer already exists for that mode and
repository, then the former buffer is instead deleted and the
latter is displayed in its place."
  (interactive)
  (if mercit-buffer-locked-p
      (if-let ((unlocked (mercit-get-mode-buffer major-mode)))
          (let ((locked (current-buffer)))
            (switch-to-buffer unlocked nil t)
            (kill-buffer locked))
        (setq mercit-buffer-locked-p nil)
        (rename-buffer (funcall mercit-generate-buffer-name-function
                                major-mode)))
    (if-let ((value (mercit-buffer-value)))
        (if-let ((locked (mercit-get-mode-buffer major-mode value)))
            (let ((unlocked (current-buffer)))
              (switch-to-buffer locked nil t)
              (kill-buffer unlocked))
          (setq mercit-buffer-locked-p t)
          (rename-buffer (funcall mercit-generate-buffer-name-function
                                  major-mode value)))
      (user-error "Buffer has no value it could be locked to"))))

;;; Bury Buffer

(defun mercit-mode-bury-buffer (&optional kill-buffer)
  "Bury the current buffer.
With a prefix argument, kill the buffer instead.
With two prefix arguments, also kill all Mercit buffers associated
with this repository.
This is done using `mercit-bury-buffer-function'."
  (interactive "P")
  ;; Kill all associated Mercit buffers when a double prefix arg is given.
  (when (>= (prefix-numeric-value kill-buffer) 16)
    (let ((current (current-buffer)))
      (dolist (buf (mercit-mode-get-buffers))
        (unless (eq buf current)
          (kill-buffer buf)))))
  (funcall mercit-bury-buffer-function kill-buffer))

(defun mercit-mode-quit-window (kill-buffer)
  "Quit the selected window and bury its buffer.

This behaves similar to `quit-window', but when the window
was originally created to display a Mercit buffer and the
current buffer is the last remaining Mercit buffer that was
ever displayed in the selected window, then delete that
window."
  (if (or (one-window-p)
          (--first (let ((buffer (car it)))
                     (and (not (eq buffer (current-buffer)))
                          (buffer-live-p buffer)
                          (or (not (window-parameter nil 'mercit-dedicated))
                              (with-current-buffer buffer
                                (derived-mode-p 'mercit-mode
                                                'mercit-process-mode)))))
                   (window-prev-buffers)))
      (quit-window kill-buffer)
    (let ((window (selected-window)))
      (quit-window kill-buffer)
      (when (window-live-p window)
        (delete-window window)))))

;;; Refresh Buffers

(defvar mercit-inhibit-refresh nil)

(defun mercit-refresh ()
  "Refresh some buffers belonging to the current repository.

Refresh the current buffer if its major mode derives from
`mercit-mode', and refresh the corresponding status buffer.

Run hooks `mercit-pre-refresh-hook' and `mercit-post-refresh-hook'."
  (interactive)
  (unless mercit-inhibit-refresh
    (unwind-protect
        (let ((start (current-time))
              (mercit--refresh-cache (or mercit--refresh-cache
                                        (list (cons 0 0)))))
          (when mercit-refresh-verbose
            (message "Refreshing mercit..."))
          (mercit-run-hook-with-benchmark 'mercit-pre-refresh-hook)
          (cond ((derived-mode-p 'mercit-mode)
                 (mercit-refresh-buffer))
                ((derived-mode-p 'tabulated-list-mode)
                 (revert-buffer)))
          (--when-let (and mercit-refresh-status-buffer
                           (not (derived-mode-p 'mercit-status-mode))
                           (mercit-get-mode-buffer 'mercit-status-mode))
            (with-current-buffer it
              (mercit-refresh-buffer)))
          (mercit-run-hook-with-benchmark 'mercit-post-refresh-hook)
          (when mercit-refresh-verbose
            (let* ((c (caar mercit--refresh-cache))
                   (a (+ c (cdar mercit--refresh-cache))))
              (message "Refreshing mercit...done (%.3fs, cached %s/%s (%.0f%%))"
                       (float-time (time-subtract (current-time) start))
                       c a (* (/ c (* a 1.0)) 100)))))
      (run-hooks 'mercit-unwind-refresh-hook))))

(defun mercit-refresh-all ()
  "Refresh all buffers belonging to the current repository.

Refresh all Mercit buffers belonging to the current repository,
and revert buffers that visit files located inside the current
repository.

Run hooks `mercit-pre-refresh-hook' and `mercit-post-refresh-hook'."
  (interactive)
  (mercit-run-hook-with-benchmark 'mercit-pre-refresh-hook)
  (dolist (buffer (mercit-mode-get-buffers))
    (with-current-buffer buffer (mercit-refresh-buffer)))
  (mercit-run-hook-with-benchmark 'mercit-post-refresh-hook))

(defvar-local mercit-refresh-start-time nil)

(defun mercit-refresh-buffer (&rest _ignore)
  "Refresh the current Mercit buffer."
  (setq mercit-refresh-start-time (current-time))
  (let ((refresh (intern (format "%s-refresh-buffer"
                                 (substring (symbol-name major-mode) 0 -5))))
        (mercit--refresh-cache (or mercit--refresh-cache (list (cons 0 0)))))
    (when (functionp refresh)
      (when mercit-refresh-verbose
        (message "Refreshing buffer `%s'..." (buffer-name)))
      (let* ((buffer (current-buffer))
             (windows (cl-mapcan
                       (lambda (window)
                         (with-selected-window window
                           (with-current-buffer buffer
                             (and-let* ((section (mercit-section-at)))
                               `(( ,window
                                   ,section
                                   ,@(mercit-section-get-relative-position
                                      section)))))))
                       ;; If it qualifies, then the selected window
                       ;; comes first, but we want to handle it last
                       ;; so that its `mercit-section-movement-hook'
                       ;; run can override the effects of other runs.
                       (or (nreverse (get-buffer-window-list buffer nil t))
                           (list (selected-window))))))
        (deactivate-mark)
        (setq mercit-section-pre-command-section nil)
        (setq mercit-section-highlight-overlays nil)
        (setq mercit-section-highlighted-sections nil)
        (setq mercit-section-unhighlight-sections nil)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (save-excursion
            (apply refresh (with-no-warnings mercit-refresh-args))))
        (pcase-dolist (`(,window . ,args) windows)
          (if (eq buffer (window-buffer window))
              (with-selected-window window
                (apply #'mercit-section-goto-successor args))
            (with-current-buffer buffer
              (let ((mercit-section-movement-hook nil))
                (apply #'mercit-section-goto-successor args)))))
        (run-hooks 'mercit-refresh-buffer-hook)
        (mercit-section-update-highlight)
        (set-buffer-modified-p nil))
      (when mercit-refresh-verbose
        (message "Refreshing buffer `%s'...done (%.3fs)" (buffer-name)
                 (float-time (time-subtract (current-time)
                                            mercit-refresh-start-time)))))))

;;; Save File-Visiting Buffers

(defvar mercit--disable-save-buffers nil)

(defun mercit-pre-command-hook ()
  (setq mercit--disable-save-buffers nil))
(add-hook 'pre-command-hook #'mercit-pre-command-hook)

(defvar mercit-after-save-refresh-buffers nil)

(defun mercit-after-save-refresh-buffers ()
  (dolist (buffer mercit-after-save-refresh-buffers)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (mercit-refresh-buffer))))
  (setq mercit-after-save-refresh-buffers nil)
  (remove-hook 'post-command-hook #'mercit-after-save-refresh-buffers))

(defun mercit-after-save-refresh-status ()
  "Refresh the status buffer of the current repository.

This function is intended to be added to `after-save-hook'.

If the status buffer does not exist or the file being visited in
the current buffer isn't inside the working tree of a repository,
then do nothing.

Note that refreshing a Mercit buffer is done by re-creating its
contents from scratch, which can be slow in large repositories.
If you are not satisfied with Mercit's performance, then you
should obviously not add this function to that hook."
  (when (and (not mercit--disable-save-buffers)
             (mercit-inside-worktree-p t))
    (--when-let (ignore-errors (mercit-get-mode-buffer 'mercit-status-mode))
      (add-to-list 'mercit-after-save-refresh-buffers it)
      (add-hook 'post-command-hook #'mercit-after-save-refresh-buffers))))

(defun mercit-maybe-save-repository-buffers ()
  "Maybe save file-visiting buffers belonging to the current repository.
Do so if `mercit-save-repository-buffers' is non-nil.  You should
not remove this from any hooks, instead set that variable to nil
if you so desire."
  (when (and mercit-save-repository-buffers
             (not mercit--disable-save-buffers))
    (setq mercit--disable-save-buffers t)
    (let ((msg (current-message)))
      (mercit-save-repository-buffers
       (eq mercit-save-repository-buffers 'dontask))
      (when (and msg
                 (current-message)
                 (not (equal msg (current-message))))
        (message "%s" msg)))))

(add-hook 'mercit-pre-refresh-hook #'mercit-maybe-save-repository-buffers)
(add-hook 'mercit-pre-call-git-hook #'mercit-maybe-save-repository-buffers)
(add-hook 'mercit-pre-start-git-hook #'mercit-maybe-save-repository-buffers)

(defvar-local mercit-inhibit-refresh-save nil)

(defun mercit-save-repository-buffers (&optional arg)
  "Save file-visiting buffers belonging to the current repository.
After any buffer where `buffer-save-without-query' is non-nil
is saved without asking, the user is asked about each modified
buffer which visits a file in the current repository.  Optional
argument (the prefix) non-nil means save all with no questions."
  (interactive "P")
  (when-let ((topdir (mercit-identify-safe "--template" "{reporoot}")))
    (let ((remote (file-remote-p default-directory))
          (save-some-buffers-action-alist
           `((?Y (lambda (buffer)
                   (with-current-buffer buffer
                     (setq buffer-save-without-query t)
                     (save-buffer)))
                 "to save the current buffer and remember choice")
             (?N (lambda (buffer)
                   (with-current-buffer buffer
                     (setq mercit-inhibit-refresh-save t)))
                 "to skip the current buffer and remember choice")
             ,@save-some-buffers-action-alist)))
      (save-some-buffers
       arg (lambda ()
             (and buffer-file-name
                  ;; If the current file is modified and resides inside
                  ;; a repository, and a let-binding is in effect, which
                  ;; places us in another repository, then the below
                  ;; let-binding is needed to prevent that file from
                  ;; being saved.
                  (let ((default-directory
                         (file-name-directory buffer-file-name)))
                    (and
                     ;; - Check whether refreshing is disabled.
                     (not mercit-inhibit-refresh-save)
                     ;; - Check whether the visited file is either on the
                     ;;   same remote as the repository, or both are on
                     ;;   the local system.
                     (equal (file-remote-p buffer-file-name) remote)
                     ;; Delayed checks that are more expensive for remote
                     ;; repositories, due to the required network access.
                     ;; - Check whether the file is inside the repository.
                     (equal (mercit-identify "--template" "{reporoot}") topdir)
                     ;; - Check whether the file is actually writable.
                     (file-writable-p buffer-file-name)))))))))

;;; Restore Window Configuration

(defvar mercit-inhibit-save-previous-winconf nil)

(defvar-local mercit-previous-window-configuration nil)
(put 'mercit-previous-window-configuration 'permanent-local t)

(defun mercit-save-window-configuration ()
  "Save the current window configuration.

Later, when the buffer is buried, it may be restored by
`mercit-restore-window-configuration'."
  (if mercit-inhibit-save-previous-winconf
      (when (eq mercit-inhibit-save-previous-winconf 'unset)
        (setq mercit-previous-window-configuration nil))
    (unless (get-buffer-window (current-buffer) (selected-frame))
      (setq mercit-previous-window-configuration
            (current-window-configuration)))))

(defun mercit-restore-window-configuration (&optional kill-buffer)
  "Bury or kill the current buffer and restore previous window configuration."
  (let ((winconf mercit-previous-window-configuration)
        (buffer (current-buffer))
        (frame (selected-frame)))
    (quit-window kill-buffer (selected-window))
    (when (and winconf (equal frame (window-configuration-frame winconf)))
      (set-window-configuration winconf)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq mercit-previous-window-configuration nil))))))

;;; Buffer History

(defun mercit-go-backward ()
  "Move backward in current buffer's history."
  (interactive)
  (if help-xref-stack
      (help-xref-go-back (current-buffer))
    (user-error "No previous entry in buffer's history")))

(defun mercit-go-forward ()
  "Move forward in current buffer's history."
  (interactive)
  (if help-xref-forward-stack
      (help-xref-go-forward (current-buffer))
    (user-error "No next entry in buffer's history")))

(defun mercit-insert-xref-buttons ()
  "Insert xref buttons."
  (when (and (not mercit-buffer-locked-p)
             (or help-xref-stack help-xref-forward-stack))
    (when help-xref-stack
      (mercit-xref-insert-button help-back-label 'mercit-xref-backward))
    (when help-xref-forward-stack
      (when help-xref-stack
        (insert " "))
      (mercit-xref-insert-button help-forward-label 'mercit-xref-forward))))

(defun mercit-xref-insert-button (label type)
  (mercit-insert-section (button label)
    (insert-text-button label 'type type
                        'help-args (list (current-buffer)))))

(define-button-type 'mercit-xref-backward
  :supertype 'help-back
  'mouse-face 'mercit-section-highlight
  'help-echo (purecopy "mouse-2, RET: go back to previous history entry"))

(define-button-type 'mercit-xref-forward
  :supertype 'help-forward
  'mouse-face 'mercit-section-highlight
  'help-echo (purecopy "mouse-2, RET: go back to next history entry"))

(defvar mercit-xref-modes
  '(mercit-log-mode
    mercit-reflog-mode
    mercit-diff-mode
    mercit-revision-mode)
  "List of modes for which to insert navigation buttons.")

(defun mercit-xref-setup (fn args)
  (when (memq major-mode mercit-xref-modes)
    (when help-xref-stack-item
      (push (cons (point) help-xref-stack-item) help-xref-stack)
      (setq help-xref-forward-stack nil))
    (when (called-interactively-p 'interactive)
      (--when-let (nthcdr 10 help-xref-stack)
        (setcdr it nil)))
    (setq help-xref-stack-item
          (list 'mercit-xref-restore fn default-directory args))))

(defun mercit-xref-restore (fn dir args)
  (setq default-directory dir)
  (funcall fn major-mode nil args)
  (mercit-refresh-buffer))

;;; Repository-Local Cache

(defvar mercit-repository-local-cache nil
  "Alist mapping `mercit-toplevel' paths to alists of key/value pairs.")

(defun mercit-repository-local-repository ()
  "Return the key for the current repository."
  (or (bound-and-true-p mercit--default-directory)
      (mercit-toplevel)))

(defun mercit-repository-local-set (key value &optional repository)
  "Set the repository-local VALUE for KEY.

Unless specified, REPOSITORY is the current buffer's repository.

If REPOSITORY is nil (meaning there is no current repository),
then the value is not cached, and we return nil."
  (let* ((repokey (or repository (mercit-repository-local-repository)))
         (cache (assoc repokey mercit-repository-local-cache)))
    ;; Don't cache values for a nil REPOSITORY, as the 'set' and 'get'
    ;; calls for some KEY may happen in unrelated contexts.
    (when repokey
      (if cache
          (let ((keyvalue (assoc key (cdr cache))))
            (if keyvalue
                ;; Update pre-existing value for key.
                (setcdr keyvalue value)
              ;; No such key in repository-local cache.
              (push (cons key value) (cdr cache))))
        ;; No cache for this repository.
        (push (cons repokey (list (cons key value)))
              mercit-repository-local-cache)))))

(defun mercit-repository-local-exists-p (key &optional repository)
  "Non-nil when a repository-local value exists for KEY.

Return a (KEY . VALUE) cons cell.

The KEY is matched using `equal'.

Unless specified, REPOSITORY is the current buffer's repository."
  (and-let* ((cache (assoc (or repository
                               (mercit-repository-local-repository))
                           mercit-repository-local-cache)))
    (assoc key (cdr cache))))

(defun mercit-repository-local-get (key &optional default repository)
  "Return the repository-local value for KEY.

Return DEFAULT if no value for KEY exists.

The KEY is matched using `equal'.

Unless specified, REPOSITORY is the current buffer's repository."
  (if-let ((keyvalue (mercit-repository-local-exists-p key repository)))
      (cdr keyvalue)
    default))

(defun mercit-repository-local-delete (key &optional repository)
  "Delete the repository-local value for KEY.

Unless specified, REPOSITORY is the current buffer's repository."
  (when-let ((cache (assoc (or repository
                               (mercit-repository-local-repository))
                           mercit-repository-local-cache)))
    (setf cache (compat-call assoc-delete-all key cache))))

(defmacro mercit--with-repository-local-cache (key &rest body)
  (declare (indent 1) (debug (form body)))
  (let ((k (cl-gensym)))
    `(let ((,k ,key))
       (if-let ((kv (mercit-repository-local-exists-p ,k)))
           (cdr kv)
         (let ((v ,(macroexp-progn body)))
           (mercit-repository-local-set ,k v)
           v)))))

(defun mercit-preserve-section-visibility-cache ()
  (when (derived-mode-p 'mercit-status-mode 'mercit-refs-mode)
    (mercit-repository-local-set
     (cons major-mode 'mercit-section-visibility-cache)
     mercit-section-visibility-cache)))

(defun mercit-restore-section-visibility-cache (mode)
  (setq mercit-section-visibility-cache
        (mercit-repository-local-get
         (cons mode 'mercit-section-visibility-cache))))

(defun mercit-zap-caches (&optional all)
  "Zap caches for the current repository.

Remove the repository's entry from `mercit-repository-local-cache',
remove the host's entry from `mercit--host-git-version-cache', set
`mercit-section-visibility-cache' to nil for all Mercit buffers of
the repository and set `mercit--libgit-available-p' to `unknown'.

With a prefix argument or if optional ALL is non-nil, discard the
mentioned caches completely."
  (interactive)
  (cond (all
         (setq mercit-repository-local-cache nil)
         (setq mercit--host-git-version-cache nil)
         (dolist (buffer (buffer-list))
           (with-current-buffer buffer
             (when (derived-mode-p 'mercit-mode)
               (setq mercit-section-visibility-cache nil)))))
        (t
         (mercit-with-toplevel
           (setq mercit-repository-local-cache
                 (cl-delete default-directory
                            mercit-repository-local-cache
                            :key #'car :test #'equal))
           (setq mercit--host-git-version-cache
                 (cl-delete (file-remote-p default-directory)
                            mercit--host-git-version-cache
                            :key #'car :test #'equal)))
         (dolist (buffer (mercit-mode-get-buffers))
           (with-current-buffer buffer
             (setq mercit-section-visibility-cache nil)))))
  (setq mercit--libgit-available-p 'unknown))

;;; Imenu Support

(defun mercit--imenu-create-index ()
  ;; If `which-function-mode' is active, then the create-index
  ;; function is called at the time the major-mode is being enabled.
  ;; Modes that derive from `mercit-mode' have not populated the buffer
  ;; at that time yet, so we have to abort.
  (and mercit-root-section
       (or mercit--imenu-group-types
           mercit--imenu-item-types)
       (let ((index
              (cl-mapcan
               (lambda (section)
                 (cond
                  (mercit--imenu-group-types
                   (and (if (eq (car-safe mercit--imenu-group-types) 'not)
                            (not (mercit-section-match
                                  (cdr mercit--imenu-group-types)
                                  section))
                          (mercit-section-match mercit--imenu-group-types section))
                        (and-let* ((children (oref section children)))
                          `((,(mercit--imenu-index-name section)
                             ,@(mapcar (lambda (s)
                                         (cons (mercit--imenu-index-name s)
                                               (oref s start)))
                                       children))))))
                  (mercit--imenu-item-types
                   (and (mercit-section-match mercit--imenu-item-types section)
                        `((,(mercit--imenu-index-name section)
                           . ,(oref section start)))))))
               (oref mercit-root-section children))))
         (if (and mercit--imenu-group-types (symbolp mercit--imenu-group-types))
             (cdar index)
           index))))

(defun mercit--imenu-index-name (section)
  (let ((heading (buffer-substring-no-properties
                  (oref section start)
                  (1- (or (oref section content)
                          (oref section end))))))
    (save-match-data
      (cond
       ((and (mercit-section-match [commit logbuf] section)
             (string-match "[^ ]+\\([ *|]*\\).+" heading))
        (replace-match " " t t heading 1))
       ((mercit-section-match
         '([branch local branchbuf] [tag tags branchbuf]) section)
        (oref section value))
       ((mercit-section-match [branch remote branchbuf] section)
        (concat (oref (oref section parent) value) "/"
                (oref section value)))
       ((string-match " ([0-9]+)\\'" heading)
        (substring heading 0 (match-beginning 0)))
       (t heading)))))

;;; Bookmark support

(declare-function bookmark-get-filename "bookmark" (bookmark-name-or-record))
(declare-function bookmark-make-record-default "bookmark"
                  (&optional no-file no-context posn))
(declare-function bookmark-prop-get "bookmark" (bookmark-name-or-record prop))
(declare-function bookmark-prop-set "bookmark" (bookmark-name-or-record prop val))

(defun mercit--make-bookmark ()
  "Create a bookmark for the current Mercit buffer.
Input values are the major-mode's `mercit-bookmark-name' method,
and the buffer-local values of the variables referenced in its
`mercit-bookmark-variables' property."
  (require 'bookmark)
  (if (plist-member (symbol-plist major-mode) 'mercit-bookmark-variables)
      ;; `bookmark-make-record-default's return value does not match
      ;; (NAME . ALIST), even though it is used as the default value
      ;; of `bookmark-make-record-function', which states that such
      ;; functions must do that.  See #4356.
      (let ((bookmark (cons nil (bookmark-make-record-default 'no-file))))
        (bookmark-prop-set bookmark 'handler  #'mercit--handle-bookmark)
        (bookmark-prop-set bookmark 'mode     major-mode)
        (bookmark-prop-set bookmark 'filename (mercit-toplevel))
        (bookmark-prop-set bookmark 'defaults (list (mercit-bookmark-name)))
        (dolist (var (get major-mode 'mercit-bookmark-variables))
          (bookmark-prop-set bookmark var (symbol-value var)))
        (bookmark-prop-set
         bookmark 'mercit-hidden-sections
         (--keep (and (oref it hidden)
                      (cons (oref it type)
                            (if (derived-mode-p 'mercit-stash-mode)
                                (string-replace mercit-buffer-revision
                                                mercit-buffer-revision-hash
                                                (oref it value))
                              (oref it value))))
                 (oref mercit-root-section children)))
        bookmark)
    (user-error "Bookmarking is not implemented for %s buffers" major-mode)))

(defun mercit--handle-bookmark (bookmark)
  "Open a bookmark created by `mercit--make-bookmark'.
Call the `mercit-*-setup-buffer' function of the the major-mode
with the variables' values as arguments, which were recorded by
`mercit--make-bookmark'.  Ignore `mercit-display-buffer-function'."
  (let ((buffer (let ((default-directory (bookmark-get-filename bookmark))
                      (mode (bookmark-prop-get bookmark 'mode))
                      (mercit-display-buffer-function #'identity)
                      (mercit-display-buffer-noselect t))
                  (apply (intern (format "%s-setup-buffer"
                                         (substring (symbol-name mode) 0 -5)))
                         (--map (bookmark-prop-get bookmark it)
                                (get mode 'mercit-bookmark-variables))))))
    (set-buffer buffer) ; That is the interface we have to adhere to.
    (when-let ((hidden (bookmark-prop-get bookmark 'mercit-hidden-sections)))
      (with-current-buffer buffer
        (dolist (child (oref mercit-root-section children))
          (if (member (cons (oref child type)
                            (oref child value))
                      hidden)
              (mercit-section-hide child)
            (mercit-section-show child)))))
    ;; Compatibility with `bookmark+' package.  See #4356.
    (when (bound-and-true-p bmkp-jump-display-function)
      (funcall bmkp-jump-display-function (current-buffer)))
    nil))

(put 'mercit--handle-bookmark 'bookmark-handler-type "Mercit")

(cl-defgeneric mercit-bookmark-name ()
  "Return name for bookmark to current buffer."
  (format "%s%s"
          (substring (symbol-name major-mode) 0 -5)
          (if-let ((vars (get major-mode 'mercit-bookmark-variables)))
              (cl-mapcan (lambda (var)
                           (let ((val (symbol-value var)))
                             (if (and val (atom val))
                                 (list val)
                               val)))
                         vars)
            "")))

;;; Utilities

(defun mercit-toggle-verbose-refresh ()
  "Toggle whether Mercit refreshes buffers verbosely.
Enabling this helps figuring out which sections are bottlenecks.
The additional output can be found in the *Messages* buffer."
  (interactive)
  (setq mercit-refresh-verbose (not mercit-refresh-verbose))
  (message "%s verbose refreshing"
           (if mercit-refresh-verbose "Enabled" "Disabled")))

(defun mercit-run-hook-with-benchmark (hook)
  (when hook
    (if mercit-refresh-verbose
        (let ((start (current-time)))
          (message "Running %s..." hook)
          (run-hooks hook)
          (message "Running %s...done (%.3fs)" hook
                   (float-time (time-subtract (current-time) start))))
      (run-hooks hook))))

;;; _
(provide 'mercit-mode)
;;; mercit-mode.el ends here
