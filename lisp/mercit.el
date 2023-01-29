;;; mercit.el --- A Mercurial porcelain inside Emacs  -*- lexical-binding:t; coding:utf-8 -*-

;; Copyright (C) 2023      The Mercit Project Contributors
;; Copyright (C) 2008-2023 The Magit Project Contributors

;; Homepage: https://github.com/htgoebel/mercit
;; Keywords: git tools vc

;; Package-Version: 3.3.0.50-git
;; Package-Requires: (
;;     (emacs "25.1")
;;     (compat "29.1.1.0")
;;     (dash "2.19.1")
;;     (git-commit "3.3.0")
;;     (mercit-section "3.3.0")
;;     (transient "0.3.6")
;;     (with-editor "3.0.5"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Mercit is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; Mercit is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Mercit.  If not, see <https://www.gnu.org/licenses/>.

;; You should have received a copy of the AUTHORS.md file, which
;; lists all contributors.  If not, see
;; https://github.com/htgoebel/mercit/docs/AUTHORS.md.

;;; Commentary:

;; Mercit is a text-based Mercurial user interface that puts an unmatched focus
;; on streamlining workflows.  Commands are invoked using short mnemonic
;; key sequences that take the cursor’s position in the highly actionable
;; interface into account to provide context-sensitive behavior.

;; With Mercit you can do nearly everything that you can do when using Mercurial
;; on the command-line, but at greater speed and while taking advantage
;; of advanced features that previously seemed too daunting to use on a
;; daily basis.  Many users will find that by using Mercit they can become
;; more effective Mercurial user.

;;; Code:

(require 'mercit-core)
(require 'mercit-diff)
(require 'mercit-log)
(require 'mercit-wip)
(require 'mercit-apply)
(require 'mercit-repos)
(require 'git-commit)

(require 'format-spec)
(require 'package nil t) ; used in `mercit-version'
(require 'with-editor)

;; For `mercit:--gpg-sign'
(declare-function epg-list-keys "epg" (context &optional name mode))
(declare-function epg-decode-dn "epg" (alist))

;;; Options

(defcustom mercit-openpgp-default-signing-key nil
  "Fingerprint of your default Openpgp key used for signing.
If the specified primary key has signing capacity then it is used
as the value of the `--gpg-sign' argument without prompting, even
when other such keys exist.  To be able to select another key you
must then use a prefix argument."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-commands
  :type 'string)

;;; Faces

(defface mercit-header-line
  '((t :inherit mercit-section-heading))
  "Face for the `header-line' in some Mercit modes.
Note that some modes, such as `mercit-log-select-mode', have their
own faces for the `header-line', or for parts of the
`header-line'."
  :group 'mercit-faces)

(defface mercit-header-line-key
  '((t :inherit font-lock-builtin-face))
  "Face for keys in the `header-line'."
  :group 'mercit-faces)

(defface mercit-dimmed
  '((((class color) (background light)) :foreground "grey50")
    (((class color) (background  dark)) :foreground "grey50"))
  "Face for text that shouldn't stand out."
  :group 'mercit-faces)

(defface mercit-hash
  '((((class color) (background light)) :foreground "grey60")
    (((class color) (background  dark)) :foreground "grey40"))
  "Face for the commit object name in the log output."
  :group 'mercit-faces)

(defface mercit-tag
  '((((class color) (background light)) :foreground "Goldenrod4")
    (((class color) (background  dark)) :foreground "LightGoldenrod2"))
  "Face for tag labels shown in log buffer."
  :group 'mercit-faces)

(defface mercit-branch-remote
  '((((class color) (background light)) :foreground "DarkOliveGreen4")
    (((class color) (background  dark)) :foreground "DarkSeaGreen2"))
  "Face for remote branch head labels shown in log buffer."
  :group 'mercit-faces)

(defface mercit-branch-remote-head
  '((((supports (:box t))) :inherit mercit-branch-remote :box t)
    (t                     :inherit mercit-branch-remote :inverse-video t))
  "Face for current branch."
  :group 'mercit-faces)

(defface mercit-branch-local
  '((((class color) (background light)) :foreground "SkyBlue4")
    (((class color) (background  dark)) :foreground "LightSkyBlue1"))
  "Face for local branches."
  :group 'mercit-faces)

(defface mercit-branch-current
  '((((supports (:box t))) :inherit mercit-branch-local :box t)
    (t                     :inherit mercit-branch-local :inverse-video t))
  "Face for current branch."
  :group 'mercit-faces)

(defface mercit-branch-upstream
  '((t :slant italic))
  "Face for upstream branch.
This face is only used in logs and it gets combined
 with `mercit-branch-local', `mercit-branch-remote'
and/or `mercit-branch-remote-head'."
  :group 'mercit-faces)

(defface mercit-branch-warning
  '((t :inherit warning))
  "Face for warning about (missing) branch."
  :group 'mercit-faces)

(defface mercit-head
  '((((class color) (background light)) :inherit mercit-branch-local)
    (((class color) (background  dark)) :inherit mercit-branch-local))
  "Face for the symbolic ref `HEAD'."
  :group 'mercit-faces)

(defface mercit-refname
  '((((class color) (background light)) :foreground "grey30")
    (((class color) (background  dark)) :foreground "grey80"))
  "Face for refnames without a dedicated face."
  :group 'mercit-faces)

(defface mercit-refname-stash
  '((t :inherit mercit-refname))
  "Face for stash refnames."
  :group 'mercit-faces)

(defface mercit-refname-wip
  '((t :inherit mercit-refname))
  "Face for wip refnames."
  :group 'mercit-faces)

(defface mercit-refname-pullreq
  '((t :inherit mercit-refname))
  "Face for pullreq refnames."
  :group 'mercit-faces)

(defface mercit-keyword
  '((t :inherit font-lock-string-face))
  "Face for parts of commit messages inside brackets."
  :group 'mercit-faces)

(defface mercit-keyword-squash
  '((t :inherit font-lock-warning-face))
  "Face for squash! and fixup! keywords in commit messages."
  :group 'mercit-faces)

(defface mercit-signature-good
  '((t :foreground "green"))
  "Face for good signatures."
  :group 'mercit-faces)

(defface mercit-signature-bad
  '((t :foreground "red" :weight bold))
  "Face for bad signatures."
  :group 'mercit-faces)

(defface mercit-signature-untrusted
  '((t :foreground "medium aquamarine"))
  "Face for good untrusted signatures."
  :group 'mercit-faces)

(defface mercit-signature-expired
  '((t :foreground "orange"))
  "Face for signatures that have expired."
  :group 'mercit-faces)

(defface mercit-signature-expired-key
  '((t :inherit mercit-signature-expired))
  "Face for signatures made by an expired key."
  :group 'mercit-faces)

(defface mercit-signature-revoked
  '((t :foreground "violet red"))
  "Face for signatures made by a revoked key."
  :group 'mercit-faces)

(defface mercit-signature-error
  '((t :foreground "light blue"))
  "Face for signatures that cannot be checked (e.g. missing key)."
  :group 'mercit-faces)

(defface mercit-cherry-unmatched
  '((t :foreground "cyan"))
  "Face for unmatched cherry commits."
  :group 'mercit-faces)

(defface mercit-cherry-equivalent
  '((t :foreground "magenta"))
  "Face for equivalent cherry commits."
  :group 'mercit-faces)

(defface mercit-filename
  '((t :weight normal))
  "Face for filenames."
  :group 'mercit-faces)

;;; Global Bindings

;;;###autoload
(define-obsolete-variable-alias 'global-mercit-file-mode
  'mercit-define-global-key-bindings "Mercit 3.0.0")

;;;###autoload
(defcustom mercit-define-global-key-bindings t
  "Whether to bind some Mercit commands in the global keymap.

If this variable is non-nil, then the following bindings may
be added to the global keymap.  The default is t.

key             binding
---             -------
C-x g           mercit-status
C-x M-g         mercit-dispatch
C-c M-g         mercit-file-dispatch

These bindings may be added when `after-init-hook' is run.
Each binding is added if and only if at that time no other key
is bound to the same command and no other command is bound to
the same key.  In other words we try to avoid adding bindings
that are unnecessary, as well as bindings that conflict with
other bindings.

Adding the above bindings is delayed until `after-init-hook'
is called to allow users to set the variable anywhere in their
init file (without having to make sure to do so before `mercit'
is loaded or autoloaded) and to increase the likelihood that
all the potentially conflicting user bindings have already
been added.

To set this variable use either `setq' or the Custom interface.
Do not use the function `customize-set-variable' because doing
that would cause Mercit to be loaded immediately when that form
is evaluated (this differs from `custom-set-variables', which
doesn't load the libraries that define the customized variables).

Setting this variable to nil has no effect if that is done after
the key bindings have already been added.

We recommend that you bind \"C-c g\" instead of \"C-c M-g\" to
`mercit-file-dispatch'.  The former is a much better binding
but the \"C-c <letter>\" namespace is strictly reserved for
users; preventing Mercit from using it by default.

Also see info node `(mercit)Commands for Buffers Visiting Files'."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-essentials
  :type 'boolean)

;;;###autoload
(progn
  (defun mercit-maybe-define-global-key-bindings (&optional force)
    (when mercit-define-global-key-bindings
      (let ((map (current-global-map)))
        (dolist (elt '(("C-x g"   . mercit-status)
                       ("C-x M-g" . mercit-dispatch)
                       ("C-c M-g" . mercit-file-dispatch)))
          (let ((key (kbd (car elt)))
                (def (cdr elt)))
            (when (or force
                      (not (or (lookup-key map key)
                               (where-is-internal def (make-sparse-keymap) t))))
              (define-key map key def)))))))
  (if after-init-time
      (mercit-maybe-define-global-key-bindings)
    (add-hook 'after-init-hook #'mercit-maybe-define-global-key-bindings t)))

;;; Dispatch Popup

;;;###autoload (autoload 'mercit-dispatch "mercit" nil t)
(transient-define-prefix mercit-dispatch ()
  "Invoke a Mercit command from a list of available commands."
  :info-manual "(mercit)Top"
  ["Commands and arguments not yet implemented are marked with *"]
  ["Transient and dwim commands"
   ;; → bound in mercit-mode-map or mercit-section-mode-map
   ;; ↓ bound below
   [("A" "*Apply"          mercit-cherry-pick)
    ;; a                  ↓
    ("b" "*Branch"         mercit-branch)
    ("B" "*Bisect"         mercit-bisect)
    ("c" "*Commit"         mercit-commit)
    ("C" "*Clone"          mercit-clone)
    ("d" "*Diff"           mercit-diff)
    ("D" "*Diff (change)"  mercit-diff-refresh)
    ("e" "*Ediff (dwim)"   mercit-ediff-dwim)
    ("E" "*Ediff"          mercit-ediff)
    ("f" "*Fetch"          mercit-fetch)
    ("F" "*Pull"           mercit-pull)
    ;; g                  ↓
    ;; G                → mercit-refresh-all
    ("h" "*Help"           mercit-info)
    ("H" "*Section info"   mercit-describe-section :if-derived mercit-mode)]
   [("i" "*Ignore"         mercit-gitignore)
    ("I" "*Init"           mercit-init)
    ("j" "*Jump to section"mercit-status-jump  :if-mode     mercit-status-mode)
    ("j" "*Display status" mercit-status-quick :if-not-mode mercit-status-mode)
    ("J" "*Display buffer" mercit-display-repository-buffer)
    ;; k                  ↓
    ;; K                → mercit-file-untrack
    ("l" "*Log"            mercit-log)
    ("L" "*Log (change)"   mercit-log-refresh)
    ("m" "*Merge"          mercit-merge)
    ("M" "*Remote"         mercit-remote)
    ;; n                → mercit-section-forward
    ;; N       reserved → forge-dispatch
    ("o" "*Submodule"      mercit-submodule)
    ("O" "*Subtree"        mercit-subtree)
    ;; p                → mercit-section-backward
    ("P" "*Push"           mercit-push)
    ;; q                → mercit-mode-bury-buffer
    ("Q" "*Command"        mercit-git-command)]
   [("r" "*Rebase"         mercit-rebase)
    ;; R                → mercit-file-rename
    ;; s                  ↓
    ;; S                  ↓
    ("t" "*Tag"            mercit-tag)
    ("T" "*Note"           mercit-notes)
    ;; u                  ↓
    ;; U                  ↓
    ;; v                  ↓
    ("V" "*Revert"         mercit-revert)
    ("w" "*Apply patches"  mercit-am)
    ("W" "*Format patches" mercit-patch)
    ;; x                → mercit-reset-quickly
    ("X" "*Reset"          mercit-reset)
    ("y" "*Show Refs"      mercit-show-refs)
    ("Y" "*Cherries"       mercit-cherry)
    ("z" "*Stash"          mercit-stash)
    ("Z" "*Worktree"       mercit-worktree)
    ("!" "*Run"            mercit-run)]]
  ["Applying changes"
   :if-derived mercit-mode
   [("a" "*Apply"          mercit-apply)
    ("v" "*Reverse"        mercit-reverse)
    ("k" "*Discard"        mercit-discard)]
   [("s" "*Stage"          mercit-stage)
    ("u" "*Unstage"        mercit-unstage)]
   [("S" "*Stage all"      mercit-stage-modified)
    ("U" "*Unstage all"    mercit-unstage-all)]]
  ["Essential commands"
   :if-derived mercit-mode
   [("g" "       refresh current buffer"   mercit-refresh)
    ("q" "       bury current buffer"      mercit-mode-bury-buffer)
    ("<tab>" "   toggle section at point"  mercit-section-toggle)
    ("<return>" "visit thing at point"     mercit-visit-thing)]
   [("C-x m"    "show all key bindings"    describe-mode)
    ;; ("C-x i"    "show Info manual"         mercit-info)
    ]])

;;; Mercurial Popup

(defcustom mercit-shell-command-verbose-prompt t
  "Whether to show the working directory when reading a command.
This affects `mercit-git-command', `mercit-git-command-topdir',
`mercit-shell-command', and `mercit-shell-command-topdir'."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-commands
  :type 'boolean)

(defvar mercit-git-command-history nil)

;;;###autoload (autoload 'mercit-run "mercit" nil t)
(transient-define-prefix mercit-run ()
  "Run git or another command, or launch a graphical utility."
  [["Run git subcommand"
    ("!" "*in repository root"   mercit-git-command-topdir)
    ("p" "*in working directory" mercit-git-command)]
   ["Run shell command"
    ("s" "*in repository root"   mercit-shell-command-topdir)
    ("S" "*in working directory" mercit-shell-command)]
   ["Launch"
    ("k" "*gitk"                 mercit-run-gitk)
    ("a" "*gitk --all"           mercit-run-gitk-all)
    ("b" "*gitk --branches"      mercit-run-gitk-branches)
    ("g" "*git gui"              mercit-run-git-gui)
    ("m" "*git mergetool --gui"  mercit-git-mergetool)]])

;;;###autoload
(defun mercit-git-command (command)
  "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer. \"git \" is
used as initial input, but can be deleted to run another command.

With a prefix argument COMMAND is run in the top-level directory
of the current working tree, otherwise in `default-directory'."
  (interactive (list (mercit-read-shell-command nil "hg ")))
  (mercit--shell-command command))

;;;###autoload
(defun mercit-git-command-topdir (command)
  "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer. \"git \" is
used as initial input, but can be deleted to run another command.

COMMAND is run in the top-level directory of the current
working tree."
  (interactive (list (mercit-read-shell-command t "hg ")))
  (mercit--shell-command command (mercit-toplevel)))

;;;###autoload
(defun mercit-shell-command (command)
  "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer.  With a
prefix argument COMMAND is run in the top-level directory of
the current working tree, otherwise in `default-directory'."
  (interactive (list (mercit-read-shell-command)))
  (mercit--shell-command command))

;;;###autoload
(defun mercit-shell-command-topdir (command)
  "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer.  COMMAND
is run in the top-level directory of the current working tree."
  (interactive (list (mercit-read-shell-command t)))
  (mercit--shell-command command (mercit-toplevel)))

(defun mercit--shell-command (command &optional directory)
  (let ((default-directory (or directory default-directory)))
    (with-environment-variables (("GIT_PAGER" "cat"))
      (mercit--with-connection-local-variables
       (mercit-start-process shell-file-name nil
                            shell-command-switch command))))
  (mercit-process-buffer))

(defun mercit-read-shell-command (&optional toplevel initial-input)
  (let ((default-directory
         (if (or toplevel current-prefix-arg)
             (or (mercit-toplevel)
                 (mercit--not-inside-repository-error))
           default-directory)))
    (read-shell-command (if mercit-shell-command-verbose-prompt
                            (format "Async shell command in %s: "
                                    (abbreviate-file-name default-directory))
                          "Async shell command: ")
                        initial-input 'mercit-git-command-history)))

;;; Shared Infix Arguments

(transient-define-argument mercit:--gpg-sign ()
  :description "*Sign using gpg"
  :class 'transient-option
  :shortarg "-S"
  :argument "--gpg-sign="
  :allow-empty t
  :reader #'mercit-read-gpg-signing-key)

(defvar mercit-gpg-secret-key-hist nil)

(defun mercit-read-gpg-secret-key
    (prompt &optional initial-input history predicate default)
  (require 'epa)
  (let* ((keys (cl-mapcan
                (lambda (cert)
                  (and (or (not predicate)
                           (funcall predicate cert))
                       (let* ((key (car (epg-key-sub-key-list cert)))
                              (fpr (epg-sub-key-fingerprint key))
                              (id  (epg-sub-key-id key))
                              (author
                               (and-let* ((id-obj
                                           (car (epg-key-user-id-list cert))))
                                 (let ((id-str (epg-user-id-string id-obj)))
                                   (if (stringp id-str)
                                       id-str
                                     (epg-decode-dn id-obj))))))
                         (list
                          (propertize fpr 'display
                                      (concat (substring fpr 0 (- (length id)))
                                              (propertize id 'face 'highlight)
                                              " " author))))))
                (epg-list-keys (epg-make-context epa-protocol) nil t)))
         (choice (or (and (not current-prefix-arg)
                          (or (and (length= keys 1) (car keys))
                              (and default (car (member default keys)))))
                     (completing-read prompt keys nil nil nil
                                      history nil initial-input))))
    (set-text-properties 0 (length choice) nil choice)
    choice))

(defun mercit-read-gpg-signing-key (prompt &optional initial-input history)
  (mercit-read-gpg-secret-key
   prompt initial-input history
   (lambda (cert)
     (cl-some (lambda (key)
                (memq 'sign (epg-sub-key-capability key)))
              (epg-key-sub-key-list cert)))
   mercit-openpgp-default-signing-key))

;;; Font-Lock Keywords

(defconst mercit-font-lock-keywords
  (eval-when-compile
    `((,(concat "(\\(mercit-define-section-jumper\\)\\_>"
                "[ \t'\(]*"
                "\\(\\(?:\\sw\\|\\s_\\)+\\)?")
       (1 'font-lock-keyword-face)
       (2 'font-lock-function-name-face nil t))
      (,(concat "(" (regexp-opt '("mercit-insert-section"
                                  "mercit-section-case"
                                  "mercit-bind-match-strings"
                                  "mercit-with-temp-index"
                                  "mercit-with-blob"
                                  "mercit-with-toplevel") t)
                "\\_>")
       . 1))))

(font-lock-add-keywords 'emacs-lisp-mode mercit-font-lock-keywords)

;;; Version

(defvar mercit-version #'undefined
  "The version of Mercit that you're using.
Use the function by the same name instead of this variable.")

;;;###autoload
(defun mercit-version (&optional print-dest)
  "Return the version of Mercit currently in use.
If optional argument PRINT-DEST is non-nil, output
stream (interactively, the echo area, or the current buffer with
a prefix argument), also print the used versions of Mercit, Mercurial,
and Emacs to it."
  (interactive (list (if current-prefix-arg (current-buffer) t)))
  (let ((mercit-git-global-arguments nil)
        (toplib (or load-file-name buffer-file-name))
        debug)
    (unless (and toplib
                 (member (file-name-nondirectory toplib)
                         '("mercit.el" "mercit.el.gz")))
      (let ((load-suffixes (reverse load-suffixes))) ; prefer .el than .elc
        (setq toplib (locate-library "mercit"))))
    (setq toplib (and toplib (mercit--straight-chase-links toplib)))
    (push toplib debug)
    (when toplib
      (let* ((topdir (file-name-directory toplib))
             (gitdir (expand-file-name
                      ".hg" (file-name-directory
                              (directory-file-name topdir))))
             (static (locate-library "mercit-version.el" nil (list topdir)))
             (static (and static (mercit--straight-chase-links static))))
        (or (progn
              (push 'repo debug)
              (when (and (file-exists-p gitdir)
                         ;; It is a repo, but is it the Mercit repo?
                         (file-exists-p
                          (expand-file-name "../lisp/mercit.el" gitdir)))
                (push t debug)
                ;; Inside the repo the version file should only exist
                ;; while running make.
                (when (and static (not noninteractive))
                  (ignore-errors (delete-file static)))
                (setq mercit-version
                      (let ((default-directory topdir))
                        (mercit-git-string "describe"  ;; FIXME: hg command
                                          "--tags" "--dirty" "--always")))))
            (progn
              (push 'static debug)
              (when (and static (file-exists-p static))
                (push t debug)
                (load-file static)
                mercit-version))
            (when (featurep 'package)
              (push 'elpa debug)
              (ignore-errors
                (--when-let (assq 'mercit package-alist)
                  (push t debug)
                  (setq mercit-version
                        (and (fboundp 'package-desc-version)
                             (package-version-join
                              (package-desc-version (cadr it))))))))
            (progn
              (push 'dirname debug)
              (let ((dirname (file-name-nondirectory
                              (directory-file-name topdir))))
                (when (string-match "\\`mercit-\\([0-9].*\\)" dirname)
                  (setq mercit-version (match-string 1 dirname)))))
            ;; If all else fails, just report the commit hash. It's
            ;; better than nothing and we cannot do better in the case
            ;; of e.g. a shallow clone.
            (progn
              (push 'hash debug)
              ;; Same check as above to see if it's really the Mercit repo.
              (when (and (file-exists-p gitdir)
                         (file-exists-p
                          (expand-file-name "../lisp/mercit.el" gitdir)))
                (setq mercit-version
                      (let ((default-directory topdir))
                        (mercit-git-string "identify"))))))))
    (if (stringp mercit-version)
        (when print-dest
          (princ (format "Mercit %s%s, Mercurial %s, Emacs %s, %s"
                         (or mercit-version "(unknown)")
                         (or (and (ignore-errors
                                    (mercit--version>= mercit-version "2008"))
                                  (ignore-errors
                                    (require 'lisp-mnt)
                                    (and (fboundp 'lm-header)
                                         (format
                                          " [>= %s]"
                                          (with-temp-buffer
                                            (insert-file-contents
                                             (locate-library "mercit.el" t))
                                            (lm-header "Package-Version"))))))
                             "")
                         (mercit--safe-git-version)
                         emacs-version
                         system-type)
                 print-dest))
      (setq debug (reverse debug))
      (setq mercit-version 'error)
      (when mercit-version
        (push mercit-version debug))
      (unless (equal (getenv "CI") "true")
        ;; The repository is a sparse clone.
        (message "Cannot determine Mercit's version %S" debug)))
    mercit-version))

;;; Startup Asserts

(defun mercit-startup-asserts ()
  (when-let ((val (getenv "GIT_DIR")))
    (setenv "GIT_DIR")
    (message
     "Mercit unset $GIT_DIR (was %S).  See %s" val
     ;; Note: Pass URL as argument rather than embedding in the format
     ;; string to prevent the single quote from being rendered
     ;; according to `text-quoting-style'.
     "https://github.com/magit/magit/wiki/Don't-set-$GIT_DIR-and-alike"))
  (when-let ((val (getenv "GIT_WORK_TREE")))
    (setenv "GIT_WORK_TREE")
    (message
     "Mercit unset $GIT_WORK_TREE (was %S).  See %s" val
     ;; See comment above.
     "https://github.com/magit/magit/wiki/Don't-set-$GIT_DIR-and-alike"))
  ;; Mercurial isn't required while building Mercit.
  (unless (bound-and-true-p byte-compile-current-file)
    (mercit-git-version-assert))
  (when (version< emacs-version mercit--minimal-emacs)
    (display-warning 'mercit (format "\
Mercit requires Emacs >= %s, you are using %s.

If this comes as a surprise to you, because you do actually have
a newer version installed, then that probably means that the
older version happens to appear earlier on the `$PATH'.  If you
always start Emacs from a shell, then that can be fixed in the
shell's init file.  If you start Emacs by clicking on an icon,
or using some sort of application launcher, then you probably
have to adjust the environment as seen by graphical interface.
For X11 something like ~/.xinitrc should work.\n"
                                    mercit--minimal-emacs emacs-version)
                     :error)))

;;; Loading Libraries

(provide 'mercit)

(cl-eval-when (load eval)
  (require 'mercit-status)
  (require 'mercit-refs)
  (require 'mercit-files)
  (require 'mercit-reset)
  (require 'mercit-branch)
  (require 'mercit-merge)
  (require 'mercit-tag)
  (require 'mercit-worktree)
  (require 'mercit-notes)
  (require 'mercit-sequence)
  (require 'mercit-commit)
  (require 'mercit-remote)
  (require 'mercit-clone)
  (require 'mercit-fetch)
  (require 'mercit-pull)
  (require 'mercit-push)
  (require 'mercit-bisect)
  (require 'mercit-stash)
  (require 'mercit-blame)
  (require 'mercit-obsolete)
  (require 'mercit-submodule)
  (unless (load "mercit-autoloads" t t)
    (require 'mercit-patch)
    (require 'mercit-subtree)
    (require 'mercit-ediff)
    (require 'mercit-gitignore)
    (require 'mercit-sparse-checkout)
    (require 'mercit-extras)
    (require 'git-rebase)
    (require 'mercit-bookmark)))

(with-eval-after-load 'bookmark
  (require 'mercit-bookmark))

(unless (bound-and-true-p byte-compile-current-file)
  (if after-init-time
      (progn (mercit-startup-asserts)
             (mercit-version))
    (add-hook 'after-init-hook #'mercit-startup-asserts t)
    (add-hook 'after-init-hook #'mercit-version t)))

;;; mercit.el ends here
