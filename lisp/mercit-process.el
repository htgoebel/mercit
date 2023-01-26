;;; mercit-process.el --- Process functionality  -*- lexical-binding:t -*-

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

;; This library implements the tools used to run Mercurial for side-effects.

;; Note that the functions used to run Mercurial and then consume its
;; output, are defined in `mercit-git.el'.  There's a bit of overlap
;; though.

;;; Code:

(require 'mercit-base)
(require 'mercit-git)
(require 'mercit-mode)

(require 'ansi-color)
(require 'with-editor)

;;; Options

(defcustom mercit-process-connection-type (not (eq system-type 'cygwin))
  "Connection type used for the Mercurial process.

If nil, use pipes: this is usually more efficient, and works on Cygwin.
If t, use ptys: this enables Mercit to prompt for passphrases when needed."
  :group 'mercit-process
  :type '(choice (const :tag "pipe" nil)
                 (const :tag "pty" t)))

(defcustom mercit-need-cygwin-noglob
  (and (eq system-type 'windows-nt)
       (with-temp-buffer
         (let ((process-environment
                (append mercit-git-environment process-environment)))
           (condition-case e
               (process-file mercit-git-executable
                             nil (current-buffer) nil
                             "-c" "alias.echo=!echo" "echo" "x{0}")
             (file-error
              (lwarn 'mercit-process :warning
                     "Could not run Mercurial: %S" e))))
         (equal "x0\n" (buffer-string))))
  "Whether to use a workaround for Cygwin's globbing behavior.

If non-nil, add environment variables to `process-environment' to
prevent the git.exe distributed by Cygwin and MSYS2 from
attempting to perform glob expansion when called from a native
Windows build of Emacs.  See #2246."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-process
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)))

(defcustom mercit-process-popup-time -1
  "Popup the process buffer if a command takes longer than this many seconds."
  :group 'mercit-process
  :type '(choice (const :tag "Never" -1)
                 (const :tag "Immediately" 0)
                 (integer :tag "After this many seconds")))

(defcustom mercit-process-log-max 32
  "Maximum number of sections to keep in a process log buffer.
When adding a new section would go beyond the limit set here,
then the older half of the sections are remove.  Sections that
belong to processes that are still running are never removed.
When this is nil, no sections are ever removed."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-process
  :type '(choice (const :tag "Never remove old sections" nil) integer))

(defvar mercit-process-extreme-logging nil
  "Whether `mercit-process-file' logs to the *Messages* buffer.

Only intended for temporary use when you try to figure out how
Mercit uses Mercurial behind the scene.  Output that normally goes to
the mercit-process buffer continues to go there.  Not all output
goes to either of these two buffers.

Also see `mercit-git-debug'.")

(defcustom mercit-process-error-tooltip-max-lines 20
  "The number of lines for `mercit-process-error-lines' to return.

These are displayed in a tooltip for `mode-line-process' errors.

If `mercit-process-error-tooltip-max-lines' is nil, the tooltip
displays the text of `mercit-process-error-summary' instead."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-process
  :type '(choice (const :tag "Use summary line" nil)
                 integer))

(defcustom mercit-credential-cache-daemon-socket
  (--some (pcase-let ((`(,prog . ,args) (split-string it)))
            (if (and prog
                     (string-match-p
                      "\\`\\(?:\\(?:/.*/\\)?git-credential-\\)?cache\\'" prog))
                (or (cl-loop for (opt val) on args
                             if (string= opt "--socket")
                             return val)
                    (expand-file-name "~/.git-credential-cache/socket"))))
          ;; Note: `mercit-process-file' is not yet defined when
          ;; evaluating this form, so we use `process-lines'.
          (ignore-errors
            (let ((process-environment
                   (append mercit-git-environment process-environment)))
              (process-lines mercit-git-executable
                             "config" "--get-all" "credential.helper"))))
  "If non-nil, start a credential cache daemon using this socket.

When using Mercurial's cache credential helper in the normal way, Emacs
sends a SIGHUP to the credential daemon after the git subprocess
has exited, causing the daemon to also quit.  This can be avoided
by starting the `git-credential-cache--daemon' process directly
from Emacs.

The function `mercit-maybe-start-credential-cache-daemon' takes
care of starting the daemon if necessary, using the value of this
option as the socket.  If this option is nil, then it does not
start any daemon.  Likewise if another daemon is already running,
then it starts no new daemon.  This function has to be a member
of the hook variable `mercit-credential-hook' for this to work.
If an error occurs while starting the daemon, most likely because
the necessary executable is missing, then the function removes
itself from the hook, to avoid further futile attempts."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-process
  :type '(choice (file  :tag "Socket")
                 (const :tag "Don't start a cache daemon" nil)))

(defcustom mercit-process-yes-or-no-prompt-regexp
  (concat " [\[(]"
          "\\([Yy]\\(?:es\\)?\\)"
          "[/|]"
          "\\([Nn]o?\\)"
          ;; OpenSSH v8 prints this.  See #3969.
          "\\(?:/\\[fingerprint\\]\\)?"
          "[\])] ?[?:]? ?$")
  "Regexp matching Yes-or-No prompts of Mercurial and its subprocesses."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-process
  :type 'regexp)

(defcustom mercit-process-password-prompt-regexps
  '("^\\(Enter \\)?[Pp]assphrase\\( for \\(RSA \\)?key '.*'\\)?: ?$"
    ;; Match-group 99 is used to identify the "user@host" part.
    "^\\(Enter \\)?[Pp]assword\\( for '?\\(https?://\\)?\\(?99:[^']*\\)'?\\)?: ?$"
    "Please enter the passphrase for the ssh key"
    "Please enter the passphrase to unlock the OpenPGP secret key"
    "^.*'s password: ?$"
    "^Token: $" ; For git-credential-manager-core (#4318).
    "^Yubikey for .*: ?$"
    "^Enter PIN for .*: ?$")
  "List of regexps matching password prompts of Mercurial and its subprocesses.
Also see `mercit-process-find-password-functions'."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-process
  :type '(repeat (regexp)))

(defcustom mercit-process-find-password-functions nil
  "List of functions to try in sequence to get a password.

These functions may be called when git asks for a password, which
is detected using `mercit-process-password-prompt-regexps'.  They
are called if and only if matching the prompt resulted in the
value of the 99th submatch to be non-nil.  Therefore users can
control for which prompts these functions should be called by
putting the host name in the 99th submatch, or not.

If the functions are called, then they are called in the order
given, with the host name as only argument, until one of them
returns non-nil.  If they are not called or none of them returns
non-nil, then the password is read from the user instead."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-process
  :type 'hook
  :options '(mercit-process-password-auth-source))

(defcustom mercit-process-username-prompt-regexps
  '("^Username for '.*': ?$")
  "List of regexps matching username prompts of Mercurial and its subprocesses."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-process
  :type '(repeat (regexp)))

(defcustom mercit-process-prompt-functions nil
  "List of functions used to forward arbitrary questions to the user.

Mercit has dedicated support for forwarding username and password
prompts and Yes-or-No questions asked by Mercurial and its subprocesses
to the user.  This can be customized using other options in the
`mercit-process' customization group.

If you encounter a new question that isn't handled by default,
then those options should be used instead of this hook.

However subprocesses may also ask questions that differ too much
from what the code related to the above options assume, and this
hook allows users to deal with such questions explicitly.

Each function is called with the process and the output string
as arguments until one of the functions returns non-nil.  The
function is responsible for asking the user the appropriate
question using e.g. `read-char-choice' and then forwarding the
answer to the process using `process-send-string'.

While functions such as `mercit-process-yes-or-no-prompt' may not
be sufficient to handle some prompt, it may still be of benefit
to look at the implementations to gain some insights on how to
implement such functions."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-process
  :type 'hook)

(defcustom mercit-process-ensure-unix-line-ending t
  "Whether Mercit should ensure a unix coding system when talking to Mercurial."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-process
  :type 'boolean)

(defcustom mercit-process-display-mode-line-error t
  "Whether Mercit should retain and highlight process errors in the mode line."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-process
  :type 'boolean)

(defface mercit-process-ok
  '((t :inherit mercit-section-heading :foreground "green"))
  "Face for zero exit-status."
  :group 'mercit-faces)

(defface mercit-process-ng
  '((t :inherit mercit-section-heading :foreground "red"))
  "Face for non-zero exit-status."
  :group 'mercit-faces)

(defface mercit-mode-line-process
  '((t :inherit mode-line-emphasis))
  "Face for `mode-line-process' status when Mercurial is running for side-effects."
  :group 'mercit-faces)

(defface mercit-mode-line-process-error
  '((t :inherit error))
  "Face for `mode-line-process' error status.

Used when `mercit-process-display-mode-line-error' is non-nil."
  :group 'mercit-faces)

;;; Process Mode

(defvar mercit-process-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mercit-mode-map)
    map)
  "Keymap for `mercit-process-mode'.")

(define-derived-mode mercit-process-mode mercit-mode "Mercit Process"
  "Mode for looking at Mercurial process output."
  :group 'mercit-process
  (hack-dir-local-variables-non-file-buffer)
  (setq mercit--imenu-item-types 'process))

(defun mercit-process-buffer (&optional nodisplay)
  "Display the current repository's process buffer.

If that buffer doesn't exist yet, then create it.
Non-interactively return the buffer and unless
optional NODISPLAY is non-nil also display it."
  (interactive)
  (let ((topdir (mercit-toplevel)))
    (unless topdir
      (mercit--with-safe-default-directory nil
        (setq topdir default-directory)
        (let (prev)
          (while (not (equal topdir prev))
            (setq prev topdir)
            (setq topdir (file-name-directory (directory-file-name topdir)))))))
    (let ((buffer (or (--first (with-current-buffer it
                                 (and (eq major-mode 'mercit-process-mode)
                                      (equal default-directory topdir)))
                               (buffer-list))
                      (mercit-generate-new-buffer 'mercit-process-mode
                                                 nil topdir))))
      (with-current-buffer buffer
        (if mercit-root-section
            (when mercit-process-log-max
              (mercit-process-truncate-log))
          (mercit-process-mode)
          (let ((inhibit-read-only t)
                (mercit-insert-section--parent  nil)
                (mercit-insert-section--oldroot nil))
            (make-local-variable 'text-property-default-nonsticky)
            (mercit-insert-section (processbuf)
              (insert "\n")))))
      (unless nodisplay
        (mercit-display-buffer buffer))
      buffer)))

(defun mercit-process-kill ()
  "Kill the process at point."
  (interactive)
  (when-let ((process (mercit-section-value-if 'process)))
    (unless (eq (process-status process) 'run)
      (user-error "Process isn't running"))
    (mercit-confirm 'kill-process)
    (kill-process process)))

;;; Synchronous Processes

(defvar mercit-process-raise-error nil)

(defun mercit-git (&rest args)
  "Call Mercurial synchronously in a separate process, for side-effects.

Option `mercit-git-executable' specifies the Mercurial executable.
The arguments ARGS specify arguments to Mercurial, they are flattened
before use.

Process output goes into a new section in the buffer returned by
`mercit-process-buffer'.  If Mercurial exits with a non-zero status,
then raise an error."
  (let ((mercit-process-raise-error t))
    (mercit-call-git args)))

(defun mercit-run-git (&rest args)
  "Call Mercurial synchronously in a separate process, and refresh.

Function `mercit-git-executable' specifies the Mercurial executable and
option `mercit-git-global-arguments' specifies constant arguments.
The arguments ARGS specify arguments to Mercurial, they are flattened
before use.

After Mercurial returns, the current buffer (if it is a Mercit buffer)
as well as the current repository's status buffer are refreshed.

Process output goes into a new section in the buffer returned by
`mercit-process-buffer'."
  (let ((mercit--refresh-cache (list (cons 0 0))))
    (mercit-call-git args)
    (when (member (car args) '("init" "clone"))
      ;; Creating a new repository invalidates the cache.
      (setq mercit--refresh-cache nil))
    (mercit-refresh)))

(defvar mercit-pre-call-git-hook nil)

(defun mercit-call-git (&rest args)
  "Call Mercurial synchronously in a separate process.

Function `mercit-git-executable' specifies the Mercurial executable and
option `mercit-git-global-arguments' specifies constant arguments.
The arguments ARGS specify arguments to Mercurial, they are flattened
before use.

Process output goes into a new section in the buffer returned by
`mercit-process-buffer'."
  (run-hooks 'mercit-pre-call-git-hook)
  (let ((default-process-coding-system (mercit--process-coding-system)))
    (apply #'mercit-call-process
           (mercit-git-executable)
           (mercit-process-git-arguments args))))

(defun mercit-call-process (program &rest args)
  "Call PROGRAM synchronously in a separate process.
Process output goes into a new section in the buffer returned by
`mercit-process-buffer'."
  (pcase-let ((`(,process-buf . ,section)
               (mercit-process-setup program args)))
    (mercit-process-finish
     (let ((inhibit-read-only t))
       (apply #'mercit-process-file program nil process-buf nil args))
     process-buf (current-buffer) default-directory section)))

(defun mercit-process-git (destination &rest args)
  "Call Mercurial synchronously in a separate process, returning its exit code.
DESTINATION specifies how to handle the output, like for
`call-process', except that file handlers are supported.
Enable Cygwin's \"noglob\" option during the call and
ensure unix eol conversion."
  (apply #'mercit-process-file
         (mercit-git-executable)
         nil destination nil
         (mercit-process-git-arguments args)))

(defun mercit-process-file (process &optional infile buffer display &rest args)
  "Process files synchronously in a separate process.
Identical to `process-file' but temporarily enable Cygwin's
\"noglob\" option during the call and ensure unix eol
conversion."
  (when mercit-process-extreme-logging
    (let ((inhibit-message t))
      (message "$ %s" (mercit-process--format-arguments process args))))
  (let ((process-environment (mercit-process-environment))
        (default-process-coding-system (mercit--process-coding-system)))
    (apply #'process-file process infile buffer display args)))

(defun mercit-process-environment ()
  ;; The various w32 hacks are only applicable when running on the
  ;; local machine.  As of Emacs 25.1, a local binding of
  ;; process-environment different from the top-level value affects
  ;; the environment used in
  ;; tramp-sh-handle-{start-file-process,process-file}.
  (let ((local (not (file-remote-p default-directory))))
    (append mercit-git-environment
            (and local
                 (cdr (assoc mercit-git-executable mercit-git-w32-path-hack)))
            (and local mercit-need-cygwin-noglob
                 (mapcar (lambda (var)
                           (concat var "=" (--if-let (getenv var)
                                               (concat it " noglob")
                                             "noglob")))
                         '("CYGWIN" "MSYS")))
            process-environment)))

(defvar mercit-this-process nil)

(defun mercit-run-git-with-input (&rest args)
  "Call Mercurial in a separate process.
ARGS is flattened and then used as arguments to Mercurial.

The current buffer's content is used as the process's standard
input.  The buffer is assumed to be temporary and thus OK to
modify.

Function `mercit-git-executable' specifies the Mercurial executable and
option `mercit-git-global-arguments' specifies constant arguments.
The remaining arguments ARGS specify arguments to Mercurial, they are
flattened before use."
  (when (eq system-type 'windows-nt)
    ;; On w32, git expects UTF-8 encoded input, ignore any user
    ;; configuration telling us otherwise (see #3250).
    (encode-coding-region (point-min) (point-max) 'utf-8-unix))
  (if (file-remote-p default-directory)
      ;; We lack `process-file-region', so fall back to asynch +
      ;; waiting in remote case.
      (progn
        (mercit-start-git (current-buffer) args)
        (while (and mercit-this-process
                    (eq (process-status mercit-this-process) 'run))
          (sleep-for 0.005)))
    (run-hooks 'mercit-pre-call-git-hook)
    (pcase-let* ((process-environment (mercit-process-environment))
                 (default-process-coding-system (mercit--process-coding-system))
                 (flat-args (mercit-process-git-arguments args))
                 (`(,process-buf . ,section)
                  (mercit-process-setup (mercit-git-executable) flat-args))
                 (inhibit-read-only t))
      (mercit-process-finish
       (apply #'call-process-region (point-min) (point-max)
              (mercit-git-executable) nil process-buf nil flat-args)
       process-buf nil default-directory section))))

;;; Asynchronous Processes

(defun mercit-run-git-async (&rest args)
  "Start Mercurial, prepare for refresh, and return the process object.
ARGS is flattened and then used as arguments to Mercurial.

Display the command line arguments in the echo area.

After Mercurial returns some buffers are refreshed: the buffer that was
current when this function was called (if it is a Mercit buffer
and still alive), as well as the respective Mercit status buffer.

See `mercit-start-process' for more information."
  (message "Running %s %s" (mercit-git-executable)
           (let ((m (mapconcat #'identity (flatten-tree args) " ")))
             (remove-list-of-text-properties 0 (length m) '(face) m)
             m))
  (mercit-start-git nil args))

(defun mercit-run-git-with-editor (&rest args)
  "Export GIT_EDITOR and start Mercurial.
Also prepare for refresh and return the process object.
ARGS is flattened and then used as arguments to Mercurial.

Display the command line arguments in the echo area.

After Mercurial returns some buffers are refreshed: the buffer that was
current when this function was called (if it is a Mercit buffer
and still alive), as well as the respective Mercit status buffer.

See `mercit-start-process' and `with-editor' for more information."
  (mercit--record-separated-gitdir)
  (mercit-with-editor (mercit-run-git-async args)))

(defun mercit-run-git-sequencer (&rest args)
  "Export GIT_EDITOR and start Mercurial.
Also prepare for refresh and return the process object.
ARGS is flattened and then used as arguments to Mercurial.

Display the command line arguments in the echo area.

After Mercurial returns some buffers are refreshed: the buffer that was
current when this function was called (if it is a Mercit buffer
and still alive), as well as the respective Mercit status buffer.
If the sequence stops at a commit, make the section representing
that commit the current section by moving `point' there.

See `mercit-start-process' and `with-editor' for more information."
  (apply #'mercit-run-git-with-editor args)
  (set-process-sentinel mercit-this-process #'mercit-sequencer-process-sentinel)
  mercit-this-process)

(defvar mercit-pre-start-git-hook nil)

(defun mercit-start-git (input &rest args)
  "Start Mercurial, prepare for refresh, and return the process object.

If INPUT is non-nil, it has to be a buffer or the name of an
existing buffer.  The buffer content becomes the processes
standard input.

Function `mercit-git-executable' specifies the Mercurial executable and
option `mercit-git-global-arguments' specifies constant arguments.
The remaining arguments ARGS specify arguments to Mercurial, they are
flattened before use.

After Mercurial returns some buffers are refreshed: the buffer that was
current when this function was called (if it is a Mercit buffer
and still alive), as well as the respective Mercit status buffer.

See `mercit-start-process' for more information."
  (run-hooks 'mercit-pre-start-git-hook)
  (let ((default-process-coding-system (mercit--process-coding-system)))
    (apply #'mercit-start-process (mercit-git-executable) input
           (mercit-process-git-arguments args))))

(defun mercit-start-process (program &optional input &rest args)
  "Start PROGRAM, prepare for refresh, and return the process object.

If optional argument INPUT is non-nil, it has to be a buffer or
the name of an existing buffer.  The buffer content becomes the
processes standard input.

The process is started using `start-file-process' and then setup
to use the sentinel `mercit-process-sentinel' and the filter
`mercit-process-filter'.  Information required by these functions
is stored in the process object.  When this function returns the
process has not started to run yet so it is possible to override
the sentinel and filter.

After the process returns, `mercit-process-sentinel' refreshes the
buffer that was current when `mercit-start-process' was called (if
it is a Mercit buffer and still alive), as well as the respective
Mercit status buffer."
  (pcase-let*
      ((`(,process-buf . ,section)
        (mercit-process-setup program args))
       (process
        (let ((process-connection-type
               ;; Don't use a pty, because it would set icrnl
               ;; which would modify the input (issue #20).
               (and (not input) mercit-process-connection-type))
              (process-environment (mercit-process-environment))
              (default-process-coding-system (mercit--process-coding-system)))
          (apply #'start-file-process
                 (file-name-nondirectory program)
                 process-buf program args))))
    (with-editor-set-process-filter process #'mercit-process-filter)
    (set-process-sentinel process #'mercit-process-sentinel)
    (set-process-buffer   process process-buf)
    (when (eq system-type 'windows-nt)
      ;; On w32, git expects UTF-8 encoded input, ignore any user
      ;; configuration telling us otherwise.
      (set-process-coding-system process nil 'utf-8-unix))
    (process-put process 'section section)
    (process-put process 'command-buf (current-buffer))
    (process-put process 'default-dir default-directory)
    (when mercit-inhibit-refresh
      (process-put process 'inhibit-refresh t))
    (oset section process process)
    (with-current-buffer process-buf
      (set-marker (process-mark process) (point)))
    (when input
      (with-current-buffer input
        (process-send-region process (point-min) (point-max))
        (process-send-eof    process)))
    (setq mercit-this-process process)
    (oset section value process)
    (mercit-process-display-buffer process)
    process))

(defun mercit-parse-git-async (&rest args)
  (setq args (mercit-process-git-arguments args))
  (let ((command-buf (current-buffer))
        (process-buf (generate-new-buffer " *temp*"))
        (toplevel (mercit-toplevel)))
    (with-current-buffer process-buf
      (setq default-directory toplevel)
      (let ((process
             (let ((process-connection-type nil)
                   (process-environment (mercit-process-environment))
                   (default-process-coding-system
                    (mercit--process-coding-system)))
               (apply #'start-file-process "git" process-buf
                      (mercit-git-executable) args))))
        (process-put process 'command-buf command-buf)
        (process-put process 'parsed (point))
        (setq mercit-this-process process)
        process))))

;;; Process Internals

(defun mercit-process-setup (program args)
  (mercit-process-set-mode-line program args)
  (let ((pwd default-directory)
        (buf (mercit-process-buffer t)))
    (cons buf (with-current-buffer buf
                (prog1 (mercit-process-insert-section pwd program args nil nil)
                  (backward-char 1))))))

(defun mercit-process-insert-section (pwd program args &optional errcode errlog)
  (let ((inhibit-read-only t)
        (mercit-insert-section--parent mercit-root-section)
        (mercit-insert-section--oldroot nil))
    (goto-char (1- (point-max)))
    (mercit-insert-section (process)
      (insert (if errcode
                  (format "%3s " (propertize (number-to-string errcode)
                                             'font-lock-face 'mercit-process-ng))
                "run "))
      (unless (equal (expand-file-name pwd)
                     (expand-file-name default-directory))
        (insert (file-relative-name pwd default-directory) ?\s))
      (insert (mercit-process--format-arguments program args))
      (mercit-insert-heading)
      (when errlog
        (if (bufferp errlog)
            (insert (with-current-buffer errlog
                      (buffer-substring-no-properties (point-min) (point-max))))
          (insert-file-contents errlog)
          (goto-char (1- (point-max)))))
      (insert "\n"))))

(defun mercit-process--format-arguments (program args)
  (cond
   ((and args (equal program (mercit-git-executable)))
    (setq args (-split-at (length mercit-git-global-arguments) args))
    (concat (propertize (file-name-nondirectory program)
                        'font-lock-face 'mercit-section-heading)
            " "
            (propertize (if (stringp mercit-ellipsis)
                            mercit-ellipsis
                          ;; For backward compatibility.
                          (char-to-string mercit-ellipsis))
                        'font-lock-face 'mercit-section-heading
                        'help-echo (mapconcat #'identity (car args) " "))
            " "
            (propertize (mapconcat #'shell-quote-argument (cadr args) " ")
                        'font-lock-face 'mercit-section-heading)))
   ((and args (equal program shell-file-name))
    (propertize (cadr args)
                'font-lock-face 'mercit-section-heading))
   (t
    (concat (propertize (file-name-nondirectory program)
                        'font-lock-face 'mercit-section-heading)
            " "
            (propertize (mapconcat #'shell-quote-argument args " ")
                        'font-lock-face 'mercit-section-heading)))))

(defun mercit-process-truncate-log ()
  (let* ((head nil)
         (tail (oref mercit-root-section children))
         (count (length tail)))
    (when (> (1+ count) mercit-process-log-max)
      (while (and (cdr tail)
                  (> count (/ mercit-process-log-max 2)))
        (let* ((inhibit-read-only t)
               (section (car tail))
               (process (oref section process)))
          (cond ((not process))
                ((memq (process-status process) '(exit signal))
                 (delete-region (oref section start)
                                (1+ (oref section end)))
                 (cl-decf count))
                (t
                 (push section head))))
        (pop tail))
      (oset mercit-root-section children
            (nconc (reverse head) tail)))))

(defun mercit-process-sentinel (process event)
  "Default sentinel used by `mercit-start-process'."
  (when (memq (process-status process) '(exit signal))
    (setq event (substring event 0 -1))
    (when (string-match "^finished" event)
      (message (concat (capitalize (process-name process)) " finished")))
    (mercit-process-finish process)
    (when (eq process mercit-this-process)
      (setq mercit-this-process nil))
    (unless (process-get process 'inhibit-refresh)
      (let ((command-buf (process-get process 'command-buf)))
        (if (buffer-live-p command-buf)
            (with-current-buffer command-buf
              (mercit-refresh))
          (with-temp-buffer
            (setq default-directory (process-get process 'default-dir))
            (mercit-refresh)))))))

(defun mercit-sequencer-process-sentinel (process event)
  "Special sentinel used by `mercit-run-git-sequencer'."
  (when (memq (process-status process) '(exit signal))
    (mercit-process-sentinel process event)
    (when-let* ((process-buf (process-buffer process))
                (- (buffer-live-p process-buf))
                (status-buf (with-current-buffer process-buf
                              (mercit-get-mode-buffer 'mercit-status-mode))))
      (with-current-buffer status-buf
        (--when-let
            (mercit-get-section
             `((commit . ,(mercit-rev-parse "HEAD"))
               (,(pcase (car (cadr (-split-at
                                    (1+ (length mercit-git-global-arguments))
                                    (process-command process))))
                   ((or "rebase" "am")   'rebase-sequence)
                   ((or "cherry-pick" "revert") 'sequence)))
               (status)))
          (goto-char (oref it start))
          (mercit-section-update-highlight))))))

(defun mercit-process-filter (proc string)
  "Default filter used by `mercit-start-process'."
  (with-current-buffer (process-buffer proc)
    (let ((inhibit-read-only t))
      (goto-char (process-mark proc))
      ;; Find last ^M in string.  If one was found, ignore
      ;; everything before it and delete the current line.
      (when-let ((ret-pos (cl-position ?\r string :from-end t)))
        (cl-callf substring string (1+ ret-pos))
        (delete-region (line-beginning-position) (point)))
      (insert (propertize string 'mercit-section
                          (process-get proc 'section)))
      (set-marker (process-mark proc) (point))
      ;; Make sure prompts are matched after removing ^M.
      (mercit-process-yes-or-no-prompt proc string)
      (mercit-process-username-prompt  proc string)
      (mercit-process-password-prompt  proc string)
      (run-hook-with-args-until-success 'mercit-process-prompt-functions
                                        proc string))))

(defmacro mercit-process-kill-on-abort (proc &rest body)
  (declare (indent 1) (debug (form body)))
  (let ((map (cl-gensym)))
    `(let ((,map (make-sparse-keymap)))
       (set-keymap-parent ,map minibuffer-local-map)
       ;; Note: Leaving (kbd ...) unevaluated leads to the
       ;; mercit-process:password-prompt test failing.
       (define-key ,map ,(kbd "C-g")
         (lambda ()
           (interactive)
           (ignore-errors (kill-process ,proc))
           (abort-recursive-edit)))
       (let ((minibuffer-local-map ,map))
         ,@body))))

(defun mercit-process-yes-or-no-prompt (process string)
  "Forward Yes-or-No prompts to the user."
  (when-let ((beg (string-match mercit-process-yes-or-no-prompt-regexp string)))
    (let ((max-mini-window-height 30))
      (process-send-string
       process
       (downcase
        (concat
         (match-string
          (if (save-match-data
                (mercit-process-kill-on-abort process
                  (yes-or-no-p (substring string 0 beg)))) 1 2)
          string)
         "\n"))))))

(defun mercit-process-password-auth-source (key)
  "Use `auth-source-search' to get a password.
If found, return the password.  Otherwise, return nil.

To use this function add it to the appropriate hook
  (add-hook \\='mercit-process-find-password-functions
            \\='mercit-process-password-auth-source)

KEY typically derives from a prompt such as:
  Password for \\='https://yourname@github.com\\='
in which case it would be the string
  yourname@github.com
which matches the ~/.authinfo.gpg entry
  machine github.com login yourname password 12345
or iff that is undefined, for backward compatibility
  machine yourname@github.com password 12345

On github.com you should not use your password but a
personal access token, see [1].  For information about
the peculiarities of other forges, please consult the
respective documentation.

After manually editing ~/.authinfo.gpg you must reset
the cache using
  M-x auth-source-forget-all-cached RET

The above will save you from having to repeatedly type
your token or password, but you might still repeatedly
be asked for your username.  To prevent that, change an
URL like
  https://github.com/foo/bar.git
to
  https://yourname@github.com/foo/bar.git

Instead of changing all such URLs manually, they can
be translated on the fly by doing this once
  git config --global \
    url.https://yourname@github.com.insteadOf \
    https://github.com

[1]: https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token."
  (require 'auth-source)
  (and (fboundp 'auth-source-search)
       (string-match "\\`\\(.+\\)@\\([^@]+\\)\\'" key)
       (let* ((user (match-string 1 key))
              (host (match-string 2 key))
              (secret
               (plist-get
                (car (or (auth-source-search :max 1 :host host :user user)
                         (auth-source-search :max 1 :host key)))
                :secret)))
         (if (functionp secret)
             (funcall secret)
           secret))))

(defun mercit-process-git-credential-manager-core (process string)
  "Authenticate using `git-credential-manager-core'.

To use this function add it to the appropriate hook
  (add-hook \\='mercit-process-prompt-functions
            \\='mercit-process-git-credential-manager-core)"
  (and (string-match "^option (enter for default): $" string)
       (progn
         (mercit-process-buffer)
         (let ((option (format "%c\n"
                               (read-char-choice "Option: " '(?\r ?\j ?1 ?2)))))
           (insert-before-markers-and-inherit option)
           (process-send-string process option)))))

(defun mercit-process-password-prompt (process string)
  "Find a password based on prompt STRING and send it to git.
Use `mercit-process-password-prompt-regexps' to find a known
prompt.  If and only if one is found, then call functions in
`mercit-process-find-password-functions' until one of them returns
the password.  If all functions return nil, then read the password
from the user."
  (when-let ((prompt (mercit-process-match-prompt
                      mercit-process-password-prompt-regexps string)))
    (process-send-string
     process (mercit-process-kill-on-abort process
               (concat (or (and-let* ((key (match-string 99 string)))
                             (run-hook-with-args-until-success
                              'mercit-process-find-password-functions key))
                           (read-passwd prompt))
                       "\n")))))

(defun mercit-process-username-prompt (process string)
  "Forward username prompts to the user."
  (--when-let (mercit-process-match-prompt
               mercit-process-username-prompt-regexps string)
    (process-send-string
     process (mercit-process-kill-on-abort process
               (concat (read-string it nil nil (user-login-name)) "\n")))))

(defun mercit-process-match-prompt (prompts string)
  "Match STRING against PROMPTS and set match data.
Return the matched string suffixed with \": \", if needed."
  (when (--any-p (string-match it string) prompts)
    (let ((prompt (match-string 0 string)))
      (cond ((string-suffix-p ": " prompt) prompt)
            ((string-suffix-p ":"  prompt) (concat prompt " "))
            (t                             (concat prompt ": "))))))

(defun mercit--process-coding-system ()
  (let ((fro (or mercit-git-output-coding-system
                 (car default-process-coding-system)))
        (to (cdr default-process-coding-system)))
    (if mercit-process-ensure-unix-line-ending
        (cons (coding-system-change-eol-conversion fro 'unix)
              (coding-system-change-eol-conversion to 'unix))
      (cons fro to))))

(defvar mercit-credential-hook nil
  "Hook run before Mercurial needs credentials.")

(defvar mercit-credential-cache-daemon-process nil)

(defun mercit-maybe-start-credential-cache-daemon ()
  "Maybe start a `git-credential-cache--daemon' process.

If such a process is already running or if the value of option
`mercit-credential-cache-daemon-socket' is nil, then do nothing.
Otherwise start the process passing the value of that options
as argument."
  (unless (or (not mercit-credential-cache-daemon-socket)
              (process-live-p mercit-credential-cache-daemon-process)
              (memq mercit-credential-cache-daemon-process
                    (list-system-processes)))
    (setq mercit-credential-cache-daemon-process
          (or (--first (let* ((attr (process-attributes it))
                              (comm (cdr (assq 'comm attr)))
                              (user (cdr (assq 'user attr))))
                         (and (string= comm "git-credential-cache--daemon")
                              (string= user user-login-name)))
                       (list-system-processes))
              (condition-case nil
                  (start-process "git-credential-cache--daemon"
                                 " *git-credential-cache--daemon*"
                                 (mercit-git-executable)
                                 "credential-cache--daemon"
                                 mercit-credential-cache-daemon-socket)
                ;; Some Git implementations (e.g. Windows) won't have
                ;; this program; if we fail the first time, stop trying.
                ((debug error)
                 (remove-hook 'mercit-credential-hook
                              #'mercit-maybe-start-credential-cache-daemon)))))))

(add-hook 'mercit-credential-hook #'mercit-maybe-start-credential-cache-daemon)

(defun tramp-sh-handle-start-file-process--mercit-tramp-process-environment
    (fn name buffer program &rest args)
  (if mercit-tramp-process-environment
      (apply fn name buffer
             (car mercit-tramp-process-environment)
             (append (cdr mercit-tramp-process-environment)
                     (cons program args)))
    (apply fn name buffer program args)))

(advice-add 'tramp-sh-handle-start-file-process :around
            #'tramp-sh-handle-start-file-process--mercit-tramp-process-environment)

(defun tramp-sh-handle-process-file--mercit-tramp-process-environment
    (fn program &optional infile destination display &rest args)
  (if mercit-tramp-process-environment
      (apply fn "env" infile destination display
             (append mercit-tramp-process-environment
                     (cons program args)))
    (apply fn program infile destination display args)))

(advice-add 'tramp-sh-handle-process-file :around
            #'tramp-sh-handle-process-file--mercit-tramp-process-environment)

(defvar mercit-mode-line-process-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mode-line> <mouse-1>")
      'mercit-process-buffer)
    map)
  "Keymap for `mode-line-process'.")

(defun mercit-process-set-mode-line (program args)
  "Display the git command (sans arguments) in the mode line."
  (when (equal program (mercit-git-executable))
    (setq args (nthcdr (length mercit-git-global-arguments) args)))
  (let ((str (concat " " (propertize
                          (concat (file-name-nondirectory program)
                                  (and args (concat " " (car args))))
                          'mouse-face 'highlight
                          'keymap mercit-mode-line-process-map
                          'help-echo "mouse-1: Show process buffer"
                          'font-lock-face 'mercit-mode-line-process))))
    (mercit-repository-local-set 'mode-line-process str)
    (dolist (buf (mercit-mode-get-buffers))
      (with-current-buffer buf
        (setq mode-line-process str)))
    (force-mode-line-update t)))

(defun mercit-process-set-mode-line-error-status (&optional error str)
  "Apply an error face to the string set by `mercit-process-set-mode-line'.

If ERROR is supplied, include it in the `mode-line-process' tooltip.

If STR is supplied, it replaces the `mode-line-process' text."
  (setq str (or str (mercit-repository-local-get 'mode-line-process)))
  (when str
    (setq error (format "%smouse-1: Show process buffer"
                        (if (stringp error)
                            (concat error "\n\n")
                          "")))
    (setq str (concat " " (propertize
                           (substring-no-properties str 1)
                           'mouse-face 'highlight
                           'keymap mercit-mode-line-process-map
                           'help-echo error
                           'font-lock-face 'mercit-mode-line-process-error)))
    (mercit-repository-local-set 'mode-line-process str)
    (dolist (buf (mercit-mode-get-buffers))
      (with-current-buffer buf
        (setq mode-line-process str)))
    (force-mode-line-update t)
    ;; We remove any error status from the mode line when a mercit
    ;; buffer is refreshed (see `mercit-refresh-buffer'), but we must
    ;; ensure that we ignore any refreshes during the remainder of the
    ;; current command -- otherwise a newly-set error status would be
    ;; removed before it was seen.  We set a flag which prevents the
    ;; status from being removed prior to the next command, so that
    ;; the error status is guaranteed to remain visible until then.
    (let ((repokey (mercit-repository-local-repository)))
      ;; The following closure captures the repokey value, and is
      ;; added to `pre-command-hook'.
      (cl-labels ((enable-mercit-process-unset-mode-line ()
                    ;; Remove ourself from the hook variable, so
                    ;; that we only run once.
                    (remove-hook 'pre-command-hook
                                 #'enable-mercit-process-unset-mode-line)
                    ;; Clear the inhibit flag for the repository in
                    ;; which we set it.
                    (mercit-repository-local-set
                     'inhibit-mercit-process-unset-mode-line nil repokey)))
        ;; Set the inhibit flag until the next command is invoked.
        (mercit-repository-local-set
         'inhibit-mercit-process-unset-mode-line t repokey)
        (add-hook 'pre-command-hook
                  #'enable-mercit-process-unset-mode-line)))))

(defun mercit-process-unset-mode-line-error-status ()
  "Remove any current error status from the mode line."
  (let ((status (or mode-line-process
                    (mercit-repository-local-get 'mode-line-process))))
    (when (and status
               (eq (get-text-property 1 'font-lock-face status)
                   'mercit-mode-line-process-error))
      (mercit-process-unset-mode-line))))

(add-hook 'mercit-refresh-buffer-hook
          #'mercit-process-unset-mode-line-error-status)

(defun mercit-process-unset-mode-line (&optional directory)
  "Remove the git command from the mode line."
  (let ((default-directory (or directory default-directory)))
    (unless (mercit-repository-local-get 'inhibit-mercit-process-unset-mode-line)
      (mercit-repository-local-set 'mode-line-process nil)
      (dolist (buf (mercit-mode-get-buffers))
        (with-current-buffer buf (setq mode-line-process nil)))
      (force-mode-line-update t))))

(defvar mercit-process-error-message-regexps
  (list "^\\*ERROR\\*: Canceled by user$"
        "^\\(?:error\\|fatal\\|git\\): \\(.*\\)$"
        "^\\(Cannot rebase:.*\\)$"))

(define-error 'mercit-git-error "Mercurial error")

(defun mercit-process-error-summary (process-buf section)
  "A one-line error summary from the given SECTION."
  (or (and (buffer-live-p process-buf)
           (with-current-buffer process-buf
             (and (oref section content)
                  (save-excursion
                    (goto-char (oref section end))
                    (run-hook-wrapped
                     'mercit-process-error-message-regexps
                     (lambda (re)
                       (save-excursion
                         (and (re-search-backward
                               re (oref section start) t)
                              (or (match-string-no-properties 1)
                                  (and (not mercit-process-raise-error)
                                       'suppressed))))))))))
      "Mercurial failed"))

(defun mercit-process-error-tooltip (process-buf section)
  "Returns the text from SECTION of the PROCESS-BUF buffer.

Limited by `mercit-process-error-tooltip-max-lines'."
  (and (integerp mercit-process-error-tooltip-max-lines)
       (> mercit-process-error-tooltip-max-lines 0)
       (buffer-live-p process-buf)
       (with-current-buffer process-buf
         (save-excursion
           (goto-char (or (oref section content)
                          (oref section start)))
           (buffer-substring-no-properties
            (point)
            (save-excursion
              (forward-line mercit-process-error-tooltip-max-lines)
              (goto-char
               (if (> (point) (oref section end))
                   (oref section end)
                 (point)))
              ;; Remove any trailing whitespace.
              (when (re-search-backward "[^[:space:]\n]"
                                        (oref section start) t)
                (forward-char 1))
              (point)))))))

(defvar-local mercit-this-error nil)

(defvar mercit-process-finish-apply-ansi-colors nil)

(defun mercit-process-finish (arg &optional process-buf command-buf
                                 default-dir section)
  (unless (integerp arg)
    (setq process-buf (process-buffer arg))
    (setq command-buf (process-get arg 'command-buf))
    (setq default-dir (process-get arg 'default-dir))
    (setq section     (process-get arg 'section))
    (setq arg         (process-exit-status arg)))
  (when (fboundp 'dired-uncache)
    (dired-uncache default-dir))
  (when (buffer-live-p process-buf)
    (with-current-buffer process-buf
      (let ((inhibit-read-only t)
            (marker (oref section start)))
        (goto-char marker)
        (save-excursion
          (delete-char 3)
          (set-marker-insertion-type marker nil)
          (insert (propertize (format "%3s" arg)
                              'mercit-section section
                              'font-lock-face (if (= arg 0)
                                                  'mercit-process-ok
                                                'mercit-process-ng)))
          (set-marker-insertion-type marker t))
        (when mercit-process-finish-apply-ansi-colors
          (ansi-color-apply-on-region (oref section content)
                                      (oref section end)))
        (if (= (oref section end)
               (+ (line-end-position) 2))
            (save-excursion
              (goto-char (1+ (line-end-position)))
              (delete-char -1)
              (oset section content nil))
          (let ((buf (mercit-process-buffer t)))
            (when (and (= arg 0)
                       (not (--any-p (eq (window-buffer it) buf)
                                     (window-list))))
              (mercit-section-hide section)))))))
  (if (= arg 0)
      ;; Unset the `mode-line-process' value upon success.
      (mercit-process-unset-mode-line default-dir)
    ;; Otherwise process the error.
    (let ((msg (mercit-process-error-summary process-buf section)))
      ;; Change `mode-line-process' to an error face upon failure.
      (if mercit-process-display-mode-line-error
          (mercit-process-set-mode-line-error-status
           (or (mercit-process-error-tooltip process-buf section)
               msg))
        (mercit-process-unset-mode-line default-dir))
      ;; Either signal the error, or else display the error summary in
      ;; the status buffer and with a message in the echo area.
      (cond
       (mercit-process-raise-error
        (signal 'mercit-git-error (list (format "%s (in %s)" msg default-dir))))
       ((not (eq msg 'suppressed))
        (when (buffer-live-p process-buf)
          (with-current-buffer process-buf
            (when-let ((status-buf (mercit-get-mode-buffer 'mercit-status-mode)))
              (with-current-buffer status-buf
                (setq mercit-this-error msg)))))
        (message "%s ... [%s buffer %s for details]" msg
                 (if-let ((key (and (buffer-live-p command-buf)
                                    (with-current-buffer command-buf
                                      (car (where-is-internal
                                            'mercit-process-buffer))))))
                     (format "Hit %s to see" (key-description key))
                   "See")
                 (buffer-name process-buf))))))
  arg)

(defun mercit-process-display-buffer (process)
  (when (process-live-p process)
    (let ((buf (process-buffer process)))
      (cond ((not (buffer-live-p buf)))
            ((= mercit-process-popup-time 0)
             (if (minibufferp)
                 (switch-to-buffer-other-window buf)
               (pop-to-buffer buf)))
            ((> mercit-process-popup-time 0)
             (run-with-timer mercit-process-popup-time nil
                             (lambda (p)
                               (when (eq (process-status p) 'run)
                                 (let ((buf (process-buffer p)))
                                   (when (buffer-live-p buf)
                                     (if (minibufferp)
                                         (switch-to-buffer-other-window buf)
                                       (pop-to-buffer buf))))))
                             process))))))

(defun mercit--log-action (summary line list)
  (let (heading lines)
    (if (cdr list)
        (progn (setq heading (funcall summary list))
               (setq lines (mapcar line list)))
      (setq heading (funcall line (car list))))
    (with-current-buffer (mercit-process-buffer t)
      (goto-char (1- (point-max)))
      (let ((inhibit-read-only t))
        (mercit-insert-section (message)
          (mercit-insert-heading (concat "  * " heading))
          (when lines
            (dolist (line lines)
              (insert line "\n"))
            (insert "\n"))))
      (let ((inhibit-message t))
        (when heading
          (setq lines (cons heading lines)))
        (message (mapconcat #'identity lines "\n"))))))

;;; _
(provide 'mercit-process)
;;; mercit-process.el ends here
