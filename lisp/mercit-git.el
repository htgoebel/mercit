;;; mercit-git.el --- Mercurial functionality  -*- lexical-binding:t -*-

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

;; This library implements wrappers for various Mercurial plumbing commands.

;;; Code:

(require 'mercit-base)

(require 'format-spec)

;; From `mercit-branch'.
(defvar mercit-branch-prefer-remote-upstream)
(defvar mercit-published-branches)

;; From `mercit-margin'.
(declare-function mercit-maybe-make-margin-overlay "mercit-margin" ())

;; From `mercit-mode'.
(declare-function mercit-get-mode-buffer "mercit-mode"
                  (mode &optional value frame))
(declare-function mercit-refresh "mercit-mode" ())
(defvar mercit-buffer-diff-args)
(defvar mercit-buffer-file-name)
(defvar mercit-buffer-log-args)
(defvar mercit-buffer-log-files)
(defvar mercit-buffer-refname)
(defvar mercit-buffer-revision)

;; From `mercit-process'.
(declare-function mercit-call-git "mercit-process" (&rest args))
(declare-function mercit-process-buffer "mercit-process" (&optional nodisplay))
(declare-function mercit-process-file "mercit-process"
                  (process &optional infile buffer display &rest args))
(declare-function mercit-process-git "mercit-process" (destination &rest args))
(declare-function mercit-process-insert-section "mercit-process"
                  (pwd program args &optional errcode errlog))
(defvar mercit-this-error)
(defvar mercit-process-error-message-regexps)

(eval-when-compile
  (cl-pushnew 'orig-rev eieio--known-slot-names)
  (cl-pushnew 'number eieio--known-slot-names))

;;; Mercurial implementations

(defvar mercit-inhibit-libgit t
  "Whether to inhibit the use of libgit.")

(defvar mercit--libgit-available-p nil
  "Whether libgit is available.
Use the function by the same name instead of this variable.")

(defun mercit--libgit-available-p ()
  (if (eq mercit--libgit-available-p 'unknown)
      (setq mercit--libgit-available-p
            (and module-file-suffix
                 (let ((libgit (locate-library "libgit")))
                   (and libgit
                        (or (locate-library "libegit2")
                            (let ((load-path
                                   (cons (expand-file-name
                                          (convert-standard-filename "build")
                                          (file-name-directory libgit))
                                         load-path)))
                              (locate-library "libegit2")))))))
    mercit--libgit-available-p))

(defun mercit-gitimpl ()
  "Return the Mercurial implementation used in this repository."
  (if (and (not mercit-inhibit-libgit)
           (not (file-remote-p default-directory))
           (mercit--libgit-available-p))
      'libgit
    'git))

;;; Options

;; For now this is shared between `mercit-process' and `mercit-git'.
(defgroup mercit-process nil
  "Mercurial and other external processes used by Mercit."
  :group 'mercit)

(defvar mercit-git-environment
  (list (format "INSIDE_EMACS=%s,mercit" emacs-version))
  "Prepended to `process-environment' while running git.")

(defcustom mercit-git-output-coding-system
  (and (eq system-type 'windows-nt) 'utf-8)
  "Coding system for receiving output from Mercurial.

If non-nil, the Git config value `i18n.logOutputEncoding' should
be set via `mercit-git-global-arguments' to value consistent with
this."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-process
  :type '(choice (coding-system :tag "Coding system to decode Mercurial output")
                 (const :tag "Use system default" nil)))

(defvar mercit-git-w32-path-hack nil
  "Alist of (EXE . (PATHENTRY)).
This specifies what additional PATH setting needs to be added to
the environment in order to run the non-wrapper git executables
successfully.")

(defcustom mercit-git-executable
  (or (and (eq system-type 'windows-nt)
           ;; Avoid the wrappers "cmd/git.exe" and "cmd/git.cmd",
           ;; which are much slower than using "bin/git.exe" directly.
           (and-let* ((exec (executable-find "hg")))
             (ignore-errors
               ;; Git for Windows 2.x provides cygpath so we can
               ;; ask it for native paths.
               (let* ((core-exe
                       (car
                        (process-lines
                         exec "-c"
                         "alias.X=!x() { which \"$1\" | cygpath -mf -; }; x"
                         "X" "hg")))
                      (hack-entry (assoc core-exe mercit-git-w32-path-hack))
                      ;; Running the libexec/git-core executable
                      ;; requires some extra PATH entries.
                      (path-hack
                       (list (concat "PATH="
                                     (car (process-lines
                                           exec "-c"
                                           "alias.P=!cygpath -wp \"$PATH\""
                                           "P"))))))
                 ;; The defcustom STANDARD expression can be
                 ;; evaluated many times, so make sure it is
                 ;; idempotent.
                 (if hack-entry
                     (setcdr hack-entry path-hack)
                   (push (cons core-exe path-hack) mercit-git-w32-path-hack))
                 core-exe))))
      (and (eq system-type 'darwin)
           (executable-find "hg"))
      "hg")
  "The Mercurial executable used by Mercit on the local host.
On remote machines `mercit-remote-git-executable' is used instead."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-process
  :type 'string)

(defcustom mercit-remote-git-executable "hg"
  "The Mercurial executable used by Mercit on remote machines.
On the local host `mercit-git-executable' is used instead.
Consider customizing `tramp-remote-path' instead of this
option."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-process
  :type 'string)

(defcustom mercit-git-global-arguments
  `("--pager" "never"
    ;; "--literal-pathspecs"
    ;; "--config" "core.preloadindex=true"
    ;; "--config" "log.showSignature=false"
    "--config" "color.mode=no"
    "--config" "color.pagermode=no"
    "--encoding" "UTF-8"
    )
  "Global Mercurial arguments.

The arguments set here are used every time the git executable is
run as a subprocess.  They are placed right after the executable
itself and before the git command - as in `git HERE... COMMAND
REST'.  See the manpage `git(1)' for valid arguments.

Be careful what you add here, especially if you are using Tramp
to connect to servers with ancient Mercurial versions.  Never remove
anything that is part of the default value, unless you really
know what you are doing.  And think very hard before adding
something; it will be used every time Mercit runs Mercurial for any
purpose."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-commands
  :group 'mercit-process
  :type '(repeat string))

(defcustom mercit-prefer-remote-upstream nil
  "Whether to favor remote branches when reading the upstream branch.

This controls whether commands that read a branch from the user
and then set it as the upstream branch, offer a local or a remote
branch as default completion candidate, when they have the choice.

This affects all commands that use `mercit-read-upstream-branch'
or `mercit-read-starting-point', which includes most commands
that change the upstream and many that create new branches."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-commands
  :type 'boolean)

(defcustom mercit-list-refs-namespaces
  '("refs/heads"
    "refs/remotes"
    "refs/tags"
    "refs/pullreqs")
  "List of ref namespaces considered when reading a ref.

This controls the order of refs returned by `mercit-list-refs',
which is called by functions like `mercit-list-branch-names' to
generate the collection of refs."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-commands
  :type '(repeat string))

(defcustom mercit-list-refs-sortby nil
  "How to sort the ref collection in the prompt.

This affects commands that read a ref.  More specifically, it
controls the order of refs returned by `mercit-list-refs', which
is called by functions like `mercit-list-branch-names' to generate
the collection of refs.  By default, refs are sorted according to
their full refname (i.e., \"refs/...\").

Any value accepted by the `--sort' flag of \"git for-each-ref\" can
be used.  For example, \"-creatordate\" places refs with more
recent committer or tagger dates earlier in the list.  A list of
strings can also be given in order to pass multiple sort keys to
\"git for-each-ref\".

Note that, depending on the completion framework you use, this
may not be sufficient to change the order in which the refs are
displayed.  It only controls the order of the collection passed
to `mercit-completing-read' or, for commands that support reading
multiple strings, `read-from-minibuffer'.  The completion
framework ultimately determines how the collection is displayed."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-miscellaneous
  :type '(choice string (repeat string)))

;;; Mercurial

(defvar mercit-git-debug nil
  "Whether to enable additional reporting of git errors.

Mercit basically calls git for one of these two reasons: for
side-effects or to do something with its standard output.

When git is run for side-effects then its output, including error
messages, go into the process buffer which is shown when using \
\\<mercit-status-mode-map>\\[mercit-process].

When git's output is consumed in some way, then it would be too
expensive to also insert it into this buffer, but when this
option is non-nil and git returns with a non-zero exit status,
then at least its standard error is inserted into this buffer.

This is only intended for debugging purposes.  Do not enable this
permanently, that would negatively affect performance.  Also note
that just because git exits with a non-zero exit status and prints
an error message that usually doesn't mean that it is an error as
far as Mercit is concerned, which is another reason we usually hide
these error messages.  Whether some error message is relevant in
the context of some unexpected behavior has to be judged on a case
by case basis.

The command `mercit-toggle-git-debug' changes the value of this
variable.

Also see `mercit-process-extreme-logging'.")

(defun mercit-toggle-git-debug ()
  "Toggle whether additional git errors are reported.
See info node `(mercit)Debugging Tools' for more information."
  (interactive)
  (setq mercit-git-debug (not mercit-git-debug))
  (message "Additional reporting of Mercurial errors %s"
           (if mercit-git-debug "enabled" "disabled")))

(defvar mercit--refresh-cache nil)

(defmacro mercit--with-refresh-cache (key &rest body)
  (declare (indent 1) (debug (form body)))
  (let ((k (cl-gensym)))
    `(if mercit--refresh-cache
         (let ((,k ,key))
           (--if-let (assoc ,k (cdr mercit--refresh-cache))
               (progn (cl-incf (caar mercit--refresh-cache))
                      (cdr it))
             (cl-incf (cdar mercit--refresh-cache))
             (let ((value ,(macroexp-progn body)))
               (push (cons ,k value)
                     (cdr mercit--refresh-cache))
               value)))
       ,@body)))

(defvar mercit-with-editor-envvar "EDITOR"
  "The environment variable exported by `mercit-with-editor'.
Set this to \"GIT_SEQUENCE_EDITOR\" if you do not want to use
Emacs to edit commit messages but would like to do so to edit
rebase sequences.")

(defmacro mercit-with-editor (&rest body)
  "Like `with-editor' but let-bind some more variables.
Also respect the value of `mercit-with-editor-envvar'."
  (declare (indent 0) (debug (body)))
  `(let ((mercit-process-popup-time -1)
         ;; The user may have customized `shell-file-name' to
         ;; something which results in `w32-shell-dos-semantics' nil
         ;; (which changes the quoting style used by
         ;; `shell-quote-argument'), but Git for Windows expects shell
         ;; quoting in the dos style.
         (shell-file-name (if (and (eq system-type 'windows-nt)
                                   ;; If we have Cygwin mount points,
                                   ;; the git flavor is cygwin, so dos
                                   ;; shell quoting is probably wrong.
                                   (not mercit-cygwin-mount-points))
                              "cmdproxy"
                            shell-file-name)))
     (with-editor* mercit-with-editor-envvar
       ,@body)))

(defmacro mercit--with-temp-process-buffer (&rest body)
  "Like `with-temp-buffer', but always propagate `process-environment'.
When that var is buffer-local in the calling buffer, it is not
propagated by `with-temp-buffer', so we explicitly ensure that
happens, so that processes will be invoked consistently.  BODY is
as for that macro."
  (declare (indent 0) (debug (body)))
  (let ((p (cl-gensym)))
    `(let ((,p process-environment))
       (with-temp-buffer
         (setq-local process-environment ,p)
         ,@body))))

(defsubst mercit-git-executable ()
  "Return value of `mercit-git-executable' or `mercit-remote-git-executable'.
The variable is chosen depending on whether `default-directory'
is remote."
  (if (file-remote-p default-directory)
      mercit-remote-git-executable
    mercit-git-executable))

(defun mercit-process-git-arguments (args)
  "Prepare ARGS for a function that invokes Mercurial.

Mercit has many specialized functions for running Mercurial; they all
pass arguments through this function before handing them to Mercurial,
to do the following.

* Flatten ARGS, removing nil arguments.
* Prepend `mercit-git-global-arguments' to ARGS.
* On w32 systems, encode to `w32-ansi-code-page'."
  (setq args (append mercit-git-global-arguments (flatten-tree args)))
  (if (and (eq system-type 'windows-nt) (boundp 'w32-ansi-code-page))
      ;; On w32, the process arguments *must* be encoded in the
      ;; current code-page (see #3250).
      (mapcar (lambda (arg)
                (encode-coding-string
                 arg (intern (format "cp%d" w32-ansi-code-page))))
              args)
    args))

(defun mercit-git-exit-code (&rest args)
  "Execute Mercurial with ARGS, returning its exit code."
  (mercit-process-git nil args))

(defun mercit-git-success (&rest args)
  "Execute Mercurial with ARGS, returning t if its exit code is 0."
  (= (mercit-git-exit-code args) 0))

(defun mercit-git-failure (&rest args)
  "Execute Mercurial with ARGS, returning t if its exit code is 1."
  (= (mercit-git-exit-code args) 1))

(defun mercit-git-string-p (&rest args)
  "Execute Mercurial with ARGS, returning the first line of its output.
If the exit code isn't zero or if there is no output, then return
nil.  Neither of these results is considered an error; if that is
what you want, then use `mercit-git-string-ng' instead.

This is an experimental replacement for `mercit-git-string', and
still subject to major changes."
  (mercit--with-refresh-cache (cons default-directory args)
    (mercit--with-temp-process-buffer
      (and (zerop (mercit-process-git t args))
           (not (bobp))
           (progn
             (goto-char (point-min))
             (buffer-substring-no-properties (point) (line-end-position)))))))

(defun mercit-git-string-ng (&rest args)
  "Execute Mercurial with ARGS, returning the first line of its output.
If the exit code isn't zero or if there is no output, then that
is considered an error, but instead of actually signaling an
error, return nil.  Additionally the output is put in the process
buffer (creating it if necessary) and the error message is shown
in the status buffer (provided it exists).

This is an experimental replacement for `mercit-git-string', and
still subject to major changes.  Also see `mercit-git-string-p'."
  (mercit--with-refresh-cache
      (list default-directory 'mercit-git-string-ng args)
    (mercit--with-temp-process-buffer
      (let* ((args (mercit-process-git-arguments args))
             (status (mercit-process-git t args)))
        (if (zerop status)
            (and (not (bobp))
                 (progn
                   (goto-char (point-min))
                   (buffer-substring-no-properties
                    (point) (line-end-position))))
          (let ((buf (current-buffer)))
            (with-current-buffer (mercit-process-buffer t)
              (mercit-process-insert-section default-directory
                                            mercit-git-executable args
                                            status buf)))
          (when-let ((status-buf (mercit-get-mode-buffer 'mercit-status-mode)))
            (let ((msg (mercit--locate-error-message)))
              (with-current-buffer status-buf
                (setq mercit-this-error msg))))
          nil)))))

(defun mercit-git-str (&rest args)
  "Execute Mercurial with ARGS, returning the first line of its output.
If there is no output, return nil.  If the output begins with a
newline, return an empty string.  Like `mercit-git-string' but
ignore `mercit-git-debug'."
  (setq args (flatten-tree args))
  (mercit--with-refresh-cache (cons default-directory args)
    (mercit--with-temp-process-buffer
      (mercit-process-git (list t nil) args)
      (unless (bobp)
        (goto-char (point-min))
        (buffer-substring-no-properties (point) (line-end-position))))))

(defun mercit-git-output (&rest args)
  "Execute Mercurial with ARGS, returning its output."
  (setq args (flatten-tree args))
  (mercit--with-refresh-cache (cons default-directory args)
    (mercit--with-temp-process-buffer
      (mercit-process-git (list t nil) args)
      (buffer-substring-no-properties (point-min) (point-max)))))

(define-error 'mercit-invalid-git-boolean "Not a Mercurial boolean")

(defun mercit--mercurial-bool (value)
  (pcase (downcase value)
    ((or "true"  "true\n"  "on"  "on\n" "yes" "yes\n" "1" "1\n") t)
    ((or "false" "false\n" "off" "off\n" "no" "no\n"  "0" "0\n") nil)
    (output (signal 'mercit-invalid-git-boolean (list output)))))

(defun mercit-git-true (&rest args)
  "Execute Mercurial with ARGS, returning t if it prints any of \"1\",
\"yes\", \"true\", or \"on\" (all case insensitive).  If it
prints \"0\", \"no\", \"false\", or \"off\" (all case
insensitive), then return nil .  For any other output signal
`mercit-invalid-git-boolean'."
  (mercit--mercurial-bool (mercit-git-output args)))

(defun mercit-git-false (&rest args)
  "Execute Mercurial with ARGS, returning t if it prints any of \"0\",
\"no\", \"false\", or \"off\" (all case insensitive).  If it
prints \"1\", \"yes\", \"true\", or \"on\" (all case
insensitive), then return nil.  For any other output signal
`mercit-invalid-git-boolean'."
  (not (mercit--mercurial-bool (mercit-git-output args))))

(defun mercit-git-config-p (variable &optional default)
  "Return the boolean value of the Mercurial variable VARIABLE.
VARIABLE has to be specified as a string.  Return DEFAULT (which
defaults to nil) if VARIABLE is unset.  If VARIABLE's value isn't
a boolean, then raise an error."
  (let ((args (list "config" variable)))
    (mercit--with-refresh-cache (cons default-directory args)
      (mercit--with-temp-process-buffer
        (let ((status (mercit-process-git t args))
              (output (buffer-substring (point-min) (1- (point-max)))))
          (if (zerop status)
              (mercit--mercurial-bool output)
            (if default t nil)))))))

(defun mercit-git-insert (&rest args)
  "Execute Mercurial with ARGS, inserting its output at point.
If Mercurial exits with a non-zero exit status, then show a message and
add a section in the respective process buffer."
  (setq args (mercit-process-git-arguments args))
  (if mercit-git-debug
      (let (log)
        (unwind-protect
            (progn
              (setq log (make-temp-file "mercit-stderr"))
              (delete-file log)
              (let ((exit (mercit-process-git (list t log) args)))
                (when (> exit 0)
                  (let ((msg "mercurial failed"))
                    (when (file-exists-p log)
                      (setq msg (with-temp-buffer
                                  (insert-file-contents log)
                                  (goto-char (point-max))
                                  (if (functionp mercit-git-debug)
                                      (funcall mercit-git-debug (buffer-string))
                                    (mercit--locate-error-message))))
                      (let ((mercit-git-debug nil))
                        (with-current-buffer (mercit-process-buffer t)
                          (mercit-process-insert-section default-directory
                                                        mercit-git-executable
                                                        args exit log))))
                    (message "%s" msg)))
                exit))
          (ignore-errors (delete-file log))))
    (mercit-process-git (list t nil) args)))

(defun mercit--locate-error-message ()
  (goto-char (point-max))
  (and (run-hook-wrapped 'mercit-process-error-message-regexps
                         (lambda (re) (re-search-backward re nil t)))
       (match-string-no-properties 1)))

(defun mercit-git-string (&rest args)
  "Execute Mercurial with ARGS, returning the first line of its output.
If there is no output, return nil.  If the output begins with a
newline, return an empty string."
  (setq args (flatten-tree args))
  (mercit--with-refresh-cache (cons default-directory args)
    (mercit--with-temp-process-buffer
      (apply #'mercit-git-insert args)
      (unless (bobp)
        (goto-char (point-min))
        (buffer-substring-no-properties (point) (line-end-position))))))

(defun mercit-git-lines (&rest args)
  "Execute Mercurial with ARGS, returning its output as a list of lines.
Empty lines anywhere in the output are omitted.

If Mercurial exits with a non-zero exit status, then report show a
message and add a section in the respective process buffer."
  (mercit--with-temp-process-buffer
    (apply #'mercit-git-insert args)
    (split-string (buffer-string) "\n" t)))

(defun mercit-git-items (&rest args)
  "Execute Mercurial with ARGS, returning its null-separated output as a list.
Empty items anywhere in the output are omitted.

If Mercurial exits with a non-zero exit status, then report show a
message and add a section in the respective process buffer."
  (mercit--with-temp-process-buffer
    (apply #'mercit-git-insert args)
    (split-string (buffer-string) "\\0" t)))

(defun mercit-git-wash (washer &rest args)
  "Execute Mercurial with ARGS, inserting washed output at point.
Actually first insert the raw output at point.  If there is no
output, call `mercit-cancel-section'.  Otherwise temporarily narrow
the buffer to the inserted text, move to its beginning, and then
call function WASHER with ARGS as its sole argument."
  (declare (indent 1))
  (let ((beg (point)))
    (setq args (flatten-tree args))
    (mercit-git-insert args)
    (if (= (point) beg)
        (mercit-cancel-section)
      (unless (bolp)
        (insert "\n"))
      (save-restriction
        (narrow-to-region beg (point))
        (goto-char beg)
        (funcall washer args))
      (when (or (= (point) beg)
                (= (point) (1+ beg)))
        (mercit-cancel-section))
      (mercit-maybe-make-margin-overlay))))

(defun mercit-git-executable-find (command)
  "Search for COMMAND in Mercurial's exec path, falling back to `exec-path'.
Like `executable-find', return the absolute file name of the
executable."
  (executable-find "hg"))

;;; Mercurial Version

(defconst mercit--git-version-regexp
  "\\`Mercurial Distributed SCM (version \\([0-9]+\\(\\.[0-9]+\\)\\{1,2\\}\\))")

(defvar mercit--host-git-version-cache nil)

(defun mercit-git-version>= (n)
  "Return t if `mercit-git-version's value is greater than or equal to N."
  (mercit--version>= (mercit-git-version) n))

(defun mercit-git-version< (n)
  "Return t if `mercit-git-version's value is smaller than N."
  (version< (mercit-git-version) n))

(defun mercit-git-version ()
  "Return the Mercurial version used for `default-directory'.
Raise an error if Mercurial cannot be found, if it exits with a
non-zero status, or the output does not have the expected
format."
  (mercit--with-refresh-cache default-directory
    (let ((host (file-remote-p default-directory)))
      (or (cdr (assoc host mercit--host-git-version-cache))
          (mercit--with-temp-process-buffer
            ;; Unset global arguments for ancient Git versions.
            (let* ((mercit-git-global-arguments nil)
                   (status (mercit-process-git t "version"))
                   (output (buffer-string)))
              (cond
               ((not (zerop status))
                (display-warning
                 'mercit
                 (format "%S\n\nRunning \"%s --version\" failed with output:\n\n%s"
                         (if host
                             (format "Mercit cannot find Mercurial on host %S.\n
Check the value of `mercit-remote-git-executable' using
`mercit-debug-git-executable' and consult the info node
`(tramp)Remote programs'." host)
                           "Mercit cannot find Mercurial.\n
Check the values of `mercit-git-executable' and `exec-path'
using `mercit-debug-git-executable'.")
                         (mercit-git-executable)
                         output)))
               ((save-match-data
                  (and (string-match mercit--git-version-regexp output)
                       (let ((version (match-string 1 output)))
                         (push (cons host version)
                               mercit--host-git-version-cache)
                         version))))
               (t (error "Unexpected \"%s --version\" output: %S"
                         (mercit-git-executable)
                         output)))))))))

(defun mercit-git-version-assert (&optional minimal who)
  "Assert that the used Mercurial version is greater than or equal to MINIMAL.
If optional MINIMAL is nil, compare with `mercit--minimal-git'
instead.  Optional WHO if non-nil specifies what functionality
needs at least MINIMAL, otherwise it defaults to \"Mercit\"."
  (when (mercit-git-version< (or minimal mercit--minimal-git))
    (let* ((host (file-remote-p default-directory))
           (msg (format-spec
                 (cond (host "\
%w requires Mercurial %m or greater, but on %h the version is %m.

If multiple Mercurial versions are installed on the host, then the
problem might be that TRAMP uses the wrong executable.

Check the value of `mercit-remote-git-executable' and consult
the info node `(tramp)Remote programs'.\n")
                       (t "\
%w requires Mercurial %m or greater, but you are using %v.

If you have multiple Mercurial versions installed, then check the
values of `mercit-remote-git-executable' and `exec-path'.\n"))
                 `((?w . ,(or who "Mercit"))
                   (?m . ,(or minimal mercit--minimal-git))
                   (?v . ,(mercit-git-version))
                   (?h . ,host)))))
      (display-warning 'mercit msg :error))))

(defun mercit--safe-git-version ()
  "Return the Mercurial version used for `default-directory' or an error message."
  (mercit--with-temp-process-buffer
    (let* ((mercit-git-global-arguments nil)
           (status (mercit-process-git t "version"))
           (output (buffer-string)))
      (cond ((not (zerop status)) output)
            ((save-match-data
               (and (string-match mercit--git-version-regexp output)
                    (match-string 1 output))))
            (t output)))))

(defun mercit-debug-git-executable ()
  "Display a buffer with information about `mercit-git-executable'.
Also include information about `mercit-remote-git-executable'.
See info node `(mercit)Debugging Tools' for more information."
  (interactive)
  (with-current-buffer (get-buffer-create "*mercit-git-debug*")
    (pop-to-buffer (current-buffer))
    (erase-buffer)
    (insert (format "mercit-remote-git-executable: %S\n"
                    mercit-remote-git-executable))
    (insert (concat
             (format "mercit-git-executable: %S" mercit-git-executable)
             (and (not (file-name-absolute-p mercit-git-executable))
                  (format " [%S]" (executable-find mercit-git-executable)))
             (format " (%s)\n" (mercit--safe-git-version))))
    (insert (format "exec-path: %S\n" exec-path))
    (--when-let (cl-set-difference
                 (-filter #'file-exists-p (remq nil (parse-colon-path
                                                     (getenv "PATH"))))
                 (-filter #'file-exists-p (remq nil exec-path))
                 :test #'file-equal-p)
      (insert (format "  entries in PATH, but not in exec-path: %S\n" it)))
    (dolist (execdir exec-path)
      (insert (format "  %s (%s)\n" execdir (car (file-attributes execdir))))
      (when (file-directory-p execdir)
        (dolist (exec (directory-files
                       execdir t (concat
                                  "\\`hg" (regexp-opt exec-suffixes) "\\'")))
          (insert (format "    %s (%s)\n" exec
                          (mercit--safe-git-version))))))))

;;; Variables

(defun mercit-config-get-from-cached-list (key)
  (gethash
   ;; `git config --list' downcases first and last components of the key.
   (let* ((key (replace-regexp-in-string "\\`[^.]+" #'downcase key t t))
          (key (replace-regexp-in-string "[^.]+\\'" #'downcase key t t)))
     key)
   (mercit--with-refresh-cache (cons (mercit-toplevel) 'config)
     (let ((configs (make-hash-table :test #'equal)))
       (dolist (conf (mercit-git-items "config" "--template" "{name}={value}\\0"))
         (let* ((nl-pos (cl-position ?\n conf))
                (key (substring conf 0 nl-pos))
                (val (if nl-pos (substring conf (1+ nl-pos)) "")))
           (puthash key (nconc (gethash key configs) (list val)) configs)))
       configs))))

(defun mercit-get (&rest keys)
  "Return the value of the Mercurial variable specified by KEYS."
  (car (last (apply #'mercit-get-all keys))))

(defun mercit-get-all (&rest keys)
  ;; FIXME: Mercurial has no multi-value config vars
  "Return all values of the Mercurial variable specified by KEYS."
  (let ((mercit-git-debug nil)
        (arg (and (or (null (car keys))
                      (string-prefix-p "--" (car keys)))
                  (pop keys)))
        (key (mapconcat #'identity keys ".")))
    (if (and mercit--refresh-cache (not arg))
        (mercit-config-get-from-cached-list key)
      (mercit-git-items "config" arg "--template" "{value}\\0" key))))  ;; FIXME

(defun mercit-get-boolean (&rest keys)
  "Return the boolean value of the Mercurial variable specified by KEYS.
Also see `mercit-git-config-p'."
  (let ((arg (and (or (null (car keys))
                      (string-prefix-p "--" (car keys)))
                  (pop keys)))
        (key (mapconcat #'identity keys ".")))
    (equal (if mercit--refresh-cache
               (car (last (mercit-config-get-from-cached-list key)))
             (mercit-git-str "config" arg "--bool" key))  ;; FIXME
           "true")))

(defun mercit-set (value &rest keys)
  ;; FIXME Can't set variables using the mercurial command line
  "Set the value of the Mercurial variable specified by KEYS to VALUE."
  (let ((arg (and (or (null (car keys))
                      (string-prefix-p "--" (car keys)))
                  (pop keys)))
        (key (mapconcat #'identity keys ".")))
    (if value
        (mercit-git-success "config" arg key value)
      (mercit-git-success "config" arg "--unset" key))  ;; FIXME
    value))

(gv-define-setter mercit-get (val &rest keys)
  `(mercit-set ,val ,@keys))

(defun mercit-set-all (values &rest keys)
  ;; FIXME Can't set variables using the mercurial command line
  "Set all values of the Mercurial variable specified by KEYS to VALUES."
  (let ((arg (and (or (null (car keys))
                      (string-prefix-p "--" (car keys)))
                  (pop keys)))
        (var (mapconcat #'identity keys ".")))
    (when (mercit-get var)
      (mercit-call-git "config" arg "--unset-all" var))  ;; FIXME
    (dolist (v values)
      (mercit-call-git "config" arg "--add" var v))))  ;; FIXME

;;; Files

(defun mercit--safe-default-directory (&optional file)
  (catch 'unsafe-default-dir
    (let ((dir (file-name-as-directory
                (expand-file-name (or file default-directory))))
          (previous nil))
      (while (not (mercit-file-accessible-directory-p dir))
        (setq dir (file-name-directory (directory-file-name dir)))
        (when (equal dir previous)
          (throw 'unsafe-default-dir nil))
        (setq previous dir))
      dir)))

(defmacro mercit--with-safe-default-directory (file &rest body)
  (declare (indent 1) (debug (form body)))
  `(when-let ((default-directory (mercit--safe-default-directory ,file)))
     ,@body))

(defun mercit-gitdir (&optional directory)
  "Return the absolute and resolved path of the .git directory.

If the `GIT_DIR' environment variable is define then return that.
Otherwise return the .git directory for DIRECTORY, or if that is
nil, then for `default-directory' instead.  If the directory is
not located inside a Mercurial repository, then return nil."
  (let ((default-directory (or directory default-directory)))
    (mercit-git-dir)))

(defun mercit-git-dir (&optional path)
  "Return the absolute and resolved path of the .git directory.

If the `GIT_DIR' environment variable is define then return that.
Otherwise return the .git directory for `default-directory'.  If
the directory is not located inside a Mercurial repository, then return
nil."
  (mercit--with-refresh-cache (list default-directory 'mercit-git-dir path)
    (mercit--with-safe-default-directory nil
      (and-let* ((dir (mercit-identify-safe "--template" "{reporoot}/.hg"))
                 (dir (file-name-as-directory (mercit-expand-git-file-name dir)))
                 (dir (if (file-remote-p dir)
                          dir
                        (concat (file-remote-p default-directory) dir))))
        (if path (expand-file-name (convert-standard-filename path) dir) dir)))))

(defvar mercit--separated-gitdirs nil)

(defun mercit--record-separated-gitdir ()  ;; TODO
  (let ((topdir (mercit-toplevel))
        (gitdir (mercit-git-dir)))
    ;; Kludge: git-annex converts submodule gitdirs to symlinks. See #3599.
    (when (file-symlink-p (directory-file-name gitdir))
      (setq gitdir (file-truename gitdir)))
    ;; We want to delete the entry for `topdir' here, rather than within
    ;; (unless ...), in case a `--separate-git-dir' repository was switched to
    ;; the standard structure (i.e., "topdir/.git/").
    (setq mercit--separated-gitdirs (cl-delete topdir
                                              mercit--separated-gitdirs
                                              :key #'car :test #'equal))
    (unless (equal (file-name-as-directory (expand-file-name ".hg" topdir))
                   gitdir)
      (push (cons topdir gitdir) mercit--separated-gitdirs))))

(defun mercit-toplevel (&optional directory)
  "Return the absolute path to the toplevel of the current repository.

From within the working tree or control directory of a repository
return the absolute path to the toplevel directory of the working
tree.  As a special case, from within a bare repository return
the control directory instead.  When called outside a repository
then return nil.

When optional DIRECTORY is non-nil then return the toplevel for
that directory instead of the one for `default-directory'.

Try to respect the option `find-file-visit-truename', i.e.  when
the value of that option is nil, then avoid needlessly returning
the truename.  When a symlink to a sub-directory of the working
tree is involved, or when called from within a sub-directory of
the gitdir or from the toplevel of a gitdir, which itself is not
located within the working tree, then it is not possible to avoid
returning the truename."
  (mercit--with-refresh-cache
      (cons (or directory default-directory) 'mercit-toplevel)
    (mercit--with-safe-default-directory directory
      (if-let ((topdir (mercit-identify-safe "--template" "{reporoot}")))
          (let (updir)
            (setq topdir (mercit-expand-git-file-name topdir))
            (if (and
                 nil ;; TODO enable fetching the true name
                 ;; Always honor these settings.
                 (not find-file-visit-truename)
                 ;; (not (getenv "GIT_WORK_TREE"))  ;; TODO
                 ;; `--show-cdup' is the relative path to the toplevel
                 ;; from `(file-truename default-directory)'.  Here we
                 ;; pretend it is relative to `default-directory', and
                 ;; go to that directory.  Then we check whether
                 ;; `--show-toplevel' still returns the same value and
                 ;; whether `--show-cdup' now is the empty string.  If
                 ;; both is the case, then we are at the toplevel of
                 ;; the same working tree, but also avoided needlessly
                 ;; following any symlinks.
                 (progn
                   (setq updir (file-name-as-directory
                                (mercit-rev-parse-safe "--show-cdup")))
                   (setq updir (if (file-name-absolute-p updir)
                                   (concat (file-remote-p default-directory) updir)
                                 (expand-file-name updir)))
                   (and-let*
                       ((default-directory updir)
                        (top (and (string-equal
                                   (mercit-rev-parse-safe "--show-cdup") "")
                                  (mercit-identify-safe "--template" "{reporoot}"))))
                     (string-equal (mercit-expand-git-file-name top) topdir))))
                updir
              (concat (file-remote-p default-directory)
                      (file-name-as-directory topdir))))
        (and-let* ((gitdir (mercit-identify-safe "--template" "{reporoot}/.hg"))
                   (gitdir (file-name-as-directory
                            (if (file-name-absolute-p gitdir)
                                ;; We might have followed a symlink.
                                (concat (file-remote-p default-directory)
                                        (mercit-expand-git-file-name gitdir))
                              (expand-file-name gitdir)))))
          (if (mercit-bare-repo-p)
              gitdir
            (let* ((link (expand-file-name "gitdir" gitdir)) ;; TODO
                   (wtree (and (file-exists-p link)
                               (mercit-file-line link))))
              (cond
               ((and wtree
                     ;; Ignore .git/gitdir files that result from a
                     ;; Git bug.  See #2364.
                     (not (equal wtree ".hg")))
                ;; Return the linked working tree.
                (concat (file-remote-p default-directory)
                        (file-name-directory wtree)))
               ;; The working directory may not be the parent directory of
               ;; .git if it was set up with `git init --separate-git-dir'.
               ;; See #2955.
               ((car (rassoc gitdir mercit--separated-gitdirs)))
               (t
                ;; Step outside the control directory to enter the working tree.
                (file-name-directory (directory-file-name gitdir)))))))))))

(defun mercit--toplevel-safe ()
  (or (mercit-toplevel)
      (mercit--not-inside-repository-error)))

(defmacro mercit-with-toplevel (&rest body)
  (declare (indent defun) (debug (body)))
  `(let ((default-directory (mercit--toplevel-safe)))
     ,@body))

(define-error 'mercit-outside-git-repo "Not inside Mercurial repository")
(define-error 'mercit-corrupt-git-config "Corrupt Mercurial configuration")
(define-error 'mercit-git-executable-not-found
  "Mercurial executable cannot be found (see https://mercit.vc/goto/e6a78ed2)")

(defun mercit--assert-usable-git ()
  (if (not (compat-call executable-find (mercit-git-executable) t))
      (signal 'mercit-git-executable-not-found (mercit-git-executable))
    (let ((mercit-git-debug
           (lambda (err)
             (signal 'mercit-corrupt-git-config
                     (format "%s: %s" default-directory err)))))
      (mercit-git-string "config" "ui.interactive"))
    nil))

(defun mercit--not-inside-repository-error ()
  (mercit--assert-usable-git)
  (signal 'mercit-outside-git-repo default-directory))

(defun mercit-inside-gitdir-p (&optional noerror)
  "Return t if `default-directory' is below the repository directory.
If it is below the working directory, then return nil.
If it isn't below either, then signal an error unless NOERROR
is non-nil, in which case return nil."
  (and (mercit--assert-default-directory noerror)
       ;; Below a repository directory that is not located below the
       ;; working directory "git rev-parse --is-inside-git-dir" prints
       ;; "false", which is wrong.
       (let ((gitdir (mercit-git-dir)))
         (cond (gitdir (file-in-directory-p default-directory gitdir))
               (noerror nil)
               (t (signal 'mercit-outside-git-repo default-directory))))))

(defun mercit-inside-worktree-p (&optional noerror)
  "Return t if `default-directory' is below the working directory.
If it is below the repository directory, then return nil.
If it isn't below either, then signal an error unless NOERROR
is non-nil, in which case return nil."
  (and nil  ;; TODO worktree
       (mercit--assert-default-directory noerror)
       (condition-case nil
           (mercit-rev-parse-true "--is-inside-work-tree")
         (mercit-invalid-git-boolean
          (and (not noerror)
               (signal 'mercit-outside-git-repo default-directory))))))

(cl-defgeneric mercit-bare-repo-p (&optional noerror)  ;; TODO
  "Return t if the current repository is bare.
If it is non-bare, then return nil.  If `default-directory'
isn't below a Mercurial repository, then signal an error unless
NOERROR is non-nil, in which case return nil."
  (and nil  ;;  hg status --rev null
       (mercit--assert-default-directory noerror)
       (condition-case nil
           (mercit-rev-parse-true "--is-bare-repository")
         (mercit-invalid-git-boolean
          (and (not noerror)
               (signal 'mercit-outside-git-repo default-directory))))))

(defun mercit--assert-default-directory (&optional noerror)
  (or (file-directory-p default-directory)
      (and (not noerror)
           (let ((exists (file-exists-p default-directory)))
             (signal (if exists 'file-error 'file-missing)
                     (list "Running hg in directory"
                           (if exists
                               "Not a directory"
                             "No such file or directory")
                           default-directory))))))

(defun mercit-git-repo-p (directory &optional non-bare)
  "Return t if DIRECTORY is a Mercurial repository.
When optional NON-BARE is non-nil also return nil if DIRECTORY is
a bare repository."
  (and (file-directory-p directory) ; Avoid archives, see #3397.
       (or (file-regular-p (expand-file-name ".hg" directory))
           (file-directory-p (expand-file-name ".hg" directory))
           (and (not non-bare)
                (file-regular-p (expand-file-name "00changelog.i" directory))
                (file-directory-p (expand-file-name "cache" directory))
                (file-directory-p (expand-file-name "store" directory))))))

(defun mercit-file-relative-name (&optional file tracked)
  "Return the path of FILE relative to the repository root.

If optional FILE is nil or omitted, return the relative path of
the file being visited in the current buffer, if any, else nil.
If the file is not inside a Mercurial repository, then return nil.

If TRACKED is non-nil, return the path only if it matches a
tracked file."
  (unless file
    (with-current-buffer (or (buffer-base-buffer)
                             (current-buffer))
      (setq file (or mercit-buffer-file-name buffer-file-name
                     (and (derived-mode-p 'dired-mode) default-directory)))))
  (when (and file (or (not tracked)
                      (mercit-file-tracked-p (file-relative-name file))))
    (and-let* ((dir (mercit-toplevel
                     (mercit--safe-default-directory
                      (directory-file-name (file-name-directory file))))))
      (file-relative-name file dir))))

(defun mercit-file-tracked-p (file)
  ;; need to use --all files to get a (templated) output, otherwise an untracked file would result in no outout an thus raise an error
  (mercit-git-true "status" "--all"
                   "--template" "{if(strip(status, 'I?'), 1, 0)}"
                   file))

(defun mercit-list-files (&rest args)
  ;; with full path  ;; FIXME:  "--cached"
  (apply #'mercit-git-items "status" "--template" "{path}\\0" args))

(defun mercit-tracked-files ()
  (mercit-list-files "-mardc"))  ;; modified, added, removed, deleted, changed

(defun mercit-untracked-files (&optional all files)
  ;;  TODO: (unless all "--exclude-standard")
  (mercit-list-files "--unknown" "--" files))

(defun mercit-modified-files (&optional nomodules files)
  (mercit-list-files "--modified"  ;; FIXME:  "--cached"
                     (or nomodules "--subrepos")
                     ;; FIXME: (mercit-headish)
                     "--" files))

(defun mercit-unstaged-files (&optional nomodules files)
  (mercit-list-files "--modified"
                     (or nomodules "--subrepos")
                     ;; FIXME: (mercit-headish)
                     "--" files))

(defun mercit-staged-files (&optional nomodules files)
  (mercit-list-files "--modified" ;; FIXME:  "--cached"
                     (or nomodules "--subrepos")
                     ;; FIXME: (mercit-headish)
                     "--" files))

(defun mercit-binary-files (&rest args)  ;; FIXME
  (--mapcat (and (string-match "^-\t-\t\\(.+\\)" it)
                 (list (match-string 1 it)))
            (apply #'mercit-git-items
                   "diff" "-z" "--numstat" "--ignore-submodules"
                   args)))

(defun mercit-unmerged-files ()  ;; FIXME
  (mercit-git-items "diff-files" "-z" "--name-only" "--diff-filter=U"))

(defun mercit-ignored-files ()
  ;;  TODO: "--exclude-standard" "--directory"
  (mercit-list-files "--unknown"))

(defun mercit-skip-worktree-files () ())  ;; mercurial has no worktrees

(defun mercit-assume-unchanged-files () ())  ;; concept unknown to mercurial

(defun mercit-revision-files (rev)  ;; TODO
  (mercit-with-toplevel
    (mercit-git-items "ls-tree" "-z" "-r" "--name-only" rev)))

(defun mercit-revision-directories (rev)  ;; TODO
  "List directories that contain a tracked file in revision REV."
  (mercit-with-toplevel
    (mapcar #'file-name-as-directory
            (mercit-git-items "ls-tree" "-z" "-r" "-d" "--name-only" rev))))

(defun mercit-changed-files (rev-or-range &optional other-rev)
  "Return list of files the have changed between two revisions.
If OTHER-REV is non-nil, REV-OR-RANGE should be a revision, not a
range.  Otherwise, it can be any revision or range accepted by
\"git diff\" (i.e., <rev>, <revA>..<revB>, or <revA>...<revB>)."
  (mercit-with-toplevel
   (mercit-list-files "--rev"
                      (if other-rev
                          (concat rev-or-range ":" other-rev)
                        rev-or-range))))

(defun mercit-renamed-files (revA revB)  ;; TODO
  (mapcar (pcase-lambda (`(,_status ,fileA ,fileB))
            (cons fileA fileB))
          (seq-partition (mercit-git-items "diff" "-z" "--name-status"
                                          "--find-renames"
                                          "--diff-filter=R" revA revB)
                         3)))

(defun mercit--rev-file-name (file rev other-rev)  ;; TODO
  "For FILE, potentially renamed between REV and OTHER-REV, return name in REV.
Return nil, if FILE appears neither in REV nor OTHER-REV,
or if no rename is detected."
  (or (car (member file (mercit-revision-files rev)))
      (and-let* ((renamed (mercit-renamed-files rev other-rev)))
        (car (rassoc file renamed)))))

(defun mercit-file-status (&rest args)  ;; TODO
  (mercit--with-temp-process-buffer
    (save-excursion (mercit-git-insert "status" "--print0" args))
    (let ((pos (point)) status)
      (while (> (skip-chars-forward "[:print:]") 0)
        (let ((x (char-after     pos))
              (y (char-after (1+ pos)))
              (file (buffer-substring (+ pos 3) (point))))
          (forward-char)
          (if (memq x '(?R ?C))  ;; renamed, copied
              (progn
                (setq pos (point))
                (skip-chars-forward "[:print:]")
                (push (list file (buffer-substring pos (point)) x y) status)
                (forward-char))
            (push (list file nil x y) status)))
        (setq pos (point)))
      status)))

(defcustom mercit-cygwin-mount-points
  (when (eq system-type 'windows-nt)
    (cl-sort (--map (if (string-match "^\\(.*\\) on \\(.*\\) type" it)
                        (cons (file-name-as-directory (match-string 2 it))
                              (file-name-as-directory (match-string 1 it)))
                      (lwarn '(mercit) :error
                             "Failed to parse Cygwin mount: %S" it))
                    ;; If --exec-path is not a native Windows path,
                    ;; then we probably have a cygwin git.
                    (let ((process-environment
                           (append mercit-git-environment process-environment)))
                      (and (not (string-match-p
                                 "\\`[a-zA-Z]:"
                                 (car (process-lines
                                       mercit-git-executable "--exec-path"))))
                           (ignore-errors (process-lines "mount")))))
             #'> :key (pcase-lambda (`(,cyg . ,_win)) (length cyg))))
  "Alist of (CYGWIN . WIN32) directory names.
Sorted from longest to shortest CYGWIN name."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-process
  :type '(alist :key-type string :value-type directory))

(defun mercit-expand-git-file-name (filename)
  (unless (file-name-absolute-p filename)
    (setq filename (expand-file-name filename)))
  (if-let ((cyg:win (cl-assoc filename mercit-cygwin-mount-points
                              :test (lambda (f cyg) (string-prefix-p cyg f)))))
      (concat (cdr cyg:win)
              (substring filename (length (car cyg:win))))
    filename))

(defun mercit-convert-filename-for-git (filename)
  "Convert FILENAME so that it can be passed to git.
1. If it's a absolute filename, then pass through `expand-file-name'
   to replace things such as \"~/\" that Mercurial does not understand.
2. If it's a remote filename, then remove the remote part.
3. Deal with an `windows-nt' Emacs vs. Cygwin Mercurial incompatibility."
  (if (file-name-absolute-p filename)
      (if-let ((cyg:win (cl-rassoc filename mercit-cygwin-mount-points
                                   :test (lambda (f win) (string-prefix-p win f)))))
          (concat (car cyg:win)
                  (substring filename (length (cdr cyg:win))))
        (let ((expanded (expand-file-name filename)))
          (or (file-remote-p expanded 'localname)
              expanded)))
    filename))

(defun mercit-decode-git-path (path)
  (if (eq (aref path 0) ?\")
      (decode-coding-string (read path)
                            (or mercit-git-output-coding-system
                                (car default-process-coding-system))
                            t)
    path))

(defun mercit-file-at-point (&optional expand assert)
  (if-let ((file (mercit-section-case
                   (file (oref it value))
                   (hunk (mercit-section-parent-value it)))))
      (if expand
          (expand-file-name file (mercit-toplevel))
        file)
    (when assert
      (user-error "No file at point"))))

(defun mercit-current-file ()
  (or (mercit-file-relative-name)
      (mercit-file-at-point)
      (and (derived-mode-p 'mercit-log-mode)
           (car mercit-buffer-log-files))))

;;; Predicates

(defun mercit-no-commit-p ()
  "Return t if there is no commit in the current Mercurial repository."
  (not (mercit-rev-verify ".")))

(defun mercit-merge-commit-p (commit)
  "Return t if COMMIT is a merge commit."
  (length> (mercit-commit-parents commit) 1))

(defun mercit-anything-staged-p (&optional ignore-submodules &rest files)
  "Return t if there are any staged changes.
If optional FILES is non-nil, then only changes to those files
are considered."
  (mercit-git-failure "diff" "--quiet" "--cached"  ;; FIXME
                     (or ignore-submodules "--subrepos")
                     "--" files))

(defun mercit-anything-unstaged-p (&optional ignore-submodules &rest files)
  "Return t if there are any unstaged changes.
If optional FILES is non-nil, then only changes to those files
are considered."
  (mercit-git-failure "diff" "--quiet"  ;; FIXME
                     (or ignore-submodules "--subrepos")
                     "--" files))

(defun mercit-anything-modified-p (&optional ignore-submodules &rest files)
  "Return t if there are any staged or unstaged changes.
If optional FILES is non-nil, then only changes to those files
are considered."
  (or (apply #'mercit-anything-staged-p   ignore-submodules files)
      (apply #'mercit-anything-unstaged-p ignore-submodules files)))

(defun mercit-anything-unmerged-p (&rest files)
  "Return t if there are any merge conflicts.
If optional FILES is non-nil, then only conflicts in those files
are considered."
  (and (mercit-git-string "ls-files" "--unmerged" files) t)) ;; FIXME

(defun mercit-module-worktree-p (module)
  (mercit-with-toplevel
    (file-exists-p (expand-file-name (expand-file-name ".hg" module)))))

(defun mercit-module-no-worktree-p (module)
  (not (mercit-module-worktree-p module)))

(defun mercit-ignore-submodules-p (&optional return-argument)  ;; TODO
  (or (cl-find-if (lambda (arg)
                    (string-prefix-p "--ignore-submodules" arg))
                  mercit-buffer-diff-args)
      (and-let* ((value (mercit-get "diff.ignoreSubmodules")))
        (if return-argument
            (concat "--ignore-submodules=" value)
          (concat "diff.ignoreSubmodules=" value)))))

;;; Revisions and References

(defun mercit-identify (&rest args)
  "Execute `git rev-parse ARGS', returning first line of output.
If there is no output, return nil."
  (apply #'mercit-git-string "identify" args))

(defun mercit-identify-safe (&rest args)
  "Execute `git rev-parse ARGS', returning first line of output.
If there is no output, return nil.  Like `mercit-rev-parse' but
ignore `mercit-git-debug'."
  (message "mercit-identify-safe %s" args)
  (apply #'mercit-git-str "identify" args))

(defun mercit-rev-parse (&rest args)
  "Execute `git rev-parse ARGS', returning first line of output.
If there is no output, return nil."
  (apply #'mercit-git-string "identify" args))

(defun mercit-rev-parse-safe (&rest args)
  "Execute `git rev-parse ARGS', returning first line of output.
If there is no output, return nil.  Like `mercit-rev-parse' but
ignore `mercit-git-debug'."
  (apply #'mercit-git-str "identify" args))

(defun mercit-rev-parse-true (&rest args)
  "Execute `git rev-parse ARGS', returning t if it prints \"true\".
If it prints \"false\", then return nil.  For any other output
signal an error."
  (mercit-git-true "identify" args))

(defun mercit-rev-parse-false (&rest args)
  "Execute `git rev-parse ARGS', returning t if it prints \"false\".
If it prints \"true\", then return nil.  For any other output
signal an error."
  (mercit-git-false "identify" args))

(defun mercit-rev-parse-p (&rest args)
  "Execute `git rev-parse ARGS', returning t if it prints \"true\".
Return t if the first (and usually only) output line is the
string \"true\", otherwise return nil."
  (equal (mercit-git-str "identify" args) "True"))  ;; TODO: use …-bool

(defun mercit-rev-verify (rev)
  ;; TODO: Rethink whether --rev shall be added here or be expecated
  ;; as part of REV.
  (mercit-git-string-p "identify" "--id" "--rev" rev))

(defun mercit-commit-p (rev)
  "Return full hash for REV if it names an existing commit."
  (mercit-rev-verify (mercit--rev-dereference rev)))

(defalias 'mercit-rev-verify-commit #'mercit-commit-p)

(defalias 'mercit-rev-hash #'mercit-commit-p)

(defun mercit--rev-dereference (rev)  ;; TODO
  "Return a rev that forces Mercurial to interpret REV as a commit.
If REV is nil or has the form \":/TEXT\", return REV itself."
  (cond ((not rev) nil)
        ((string-match-p "^:/" rev) rev)
        (t (concat rev "^0"))))  ;;was "^{commit}"

(defun mercit-rev-equal (a b)
  "Return t if there are no differences between the commits A and B."
  (equal "0\n" (mercit-git-str "diff" "--stat" "--from" a "--to" b)))

(defun mercit-rev-eq (a b)
  "Return t if A and B refer to the same commit."
  (let ((a (mercit-commit-p a))
        (b (mercit-commit-p b)))
    (and a b (equal a b))))

(defun mercit-rev-ancestor-p (a b)
  "Return non-nil if commit A is an ancestor of commit B."  ;; TODO
  (mercit-git-success "merge-base" "--is-ancestor" a b))

(defun mercit-rev-head-p (rev)
  (or (equal rev "HEAD")  ;; TODO
      (and rev
           (not (string-search ".." rev))
           (equal (mercit-rev-parse rev)
                  (mercit-rev-parse "HEAD")))))

(defun mercit-rev-author-p (rev)  ;; TODO
  "Return t if the user is the author of REV.
More precisely return t if `user.name' is equal to the author
name of REV and/or `user.email' is equal to the author email
of REV."
  (or (equal (mercit-get "user.name")  (mercit-rev-format "{author}" rev))
      (equal (mercit-get "user.email") (mercit-rev-format "{author}" rev))))

(defun mercit-rev-name (rev &optional pattern not-anchored)  ;; TODO
  "Return a symbolic name for REV using `git-name-rev'.

PATTERN can be used to limit the result to a matching ref.
Unless NOT-ANCHORED is non-nil, the beginning of the ref must
match PATTERN.

An anchored lookup is done using the arguments
\"--exclude=*/<PATTERN> --exclude=*/HEAD\" in addition to
\"--refs=<PATTERN>\", provided at least version v2.13 of Git is
used.  Older versions did not support the \"--exclude\" argument.
When \"--exclude\" cannot be used and `git-name-rev' returns a
ref that should have been excluded, then that is discarded and
this function returns nil instead.  This is unfortunate because
there might be other refs that do match.  To fix that, update
Mercurial."
  (if (mercit-git-version< "2.13")
      (and-let*
          ((ref (mercit-git-string "name-rev" "--name-only" "--no-undefined"
                                  (and pattern (concat "--refs=" pattern))
                                  rev)))
        (if (and pattern
                 (string-match-p "\\`refs/[^/]+/\\*\\'" pattern))
            (let ((namespace (substring pattern 0 -1)))
              (and (not (or (string-suffix-p "HEAD" ref)
                            (and (string-match-p namespace ref)
                                 (not (mercit-rev-verify
                                       (concat namespace ref))))))
                   ref))
          ref))
    (mercit-git-string "name-rev" "--name-only" "--no-undefined"
                      (and pattern (concat "--refs=" pattern))
                      (and pattern
                           (not not-anchored)
                           (list "--exclude=*/HEAD"
                                 (concat "--exclude=*/" pattern)))
                      rev)))

(defun mercit-rev-branch (rev)  ;; TODO
  (and-let* ((name (mercit-rev-name rev "refs/heads/*")))
    (and (not (string-match-p "[~^]" name)) name)))

(defun mercit-get-shortname (rev)  ;; TODO
  (let* ((fn (apply-partially #'mercit-rev-name rev))
         (name (or (funcall fn "refs/tags/*")
                   (funcall fn "refs/heads/*")
                   (funcall fn "refs/remotes/*"))))
    (cond ((not name)
           (mercit-rev-parse "--if" rev))
          ((string-match "^\\(?:tags\\|remotes\\)/\\(.+\\)" name)
           (if (mercit-ref-ambiguous-p (match-string 1 name))
               name
             (match-string 1 name)))
          (t (mercit-ref-maybe-qualify name)))))

(defun mercit-name-branch (rev &optional lax)
  (or (mercit-name-local-branch rev)
      (mercit-name-remote-branch rev)
      (and lax (or (mercit-name-local-branch rev t)
                   (mercit-name-remote-branch rev t)))))

(defun mercit-name-local-branch (rev &optional lax)  ;; TODO
  (and-let* ((name (mercit-rev-name rev "refs/heads/*")))
    (and (or lax (not (string-match-p "[~^]" name))) name)))

(defun mercit-name-remote-branch (rev &optional lax)  ;; TODO
  (and-let* ((name (mercit-rev-name rev "refs/remotes/*")))
    (and (or lax (not (string-match-p "[~^]" name)))
         (substring name 8))))

(defun mercit-name-tag (rev &optional lax)  ;; TODO
  (when-let* ((name (mercit-rev-name rev "refs/tags/*"))) ;debbugs#31840
    (when (string-suffix-p "^0" name)
      (setq name (substring name 0 -2)))
    (and (or lax (not (string-match-p "[~^]" name)))
         (substring name 5))))

(defun mercit-ref-abbrev (refname)  ;; TODO
  "Return an unambiguous abbreviation of REFNAME."
  ;; (mercit-rev-parse "--verify" "--abbrev-ref" refname))
  (mercit-identify refname))

(defun mercit-ref-fullname (refname)  ;; TODO
  "Return fully qualified refname for REFNAME.
If REFNAME is ambiguous, return nil."
  (mercit-rev-parse "--verify" "--symbolic-full-name" refname))

(defun mercit-ref-ambiguous-p (refname)  ;; TODO
  (save-match-data
    (if (string-match "\\`\\([^^~]+\\)\\(.*\\)" refname)
        (not (mercit-ref-fullname (match-string 1 refname)))
      (error "%S has an unrecognized format" refname))))

(defun mercit-ref-maybe-qualify (refname &optional prefix)  ;; TODO
  "If REFNAME is ambiguous, try to disambiguate it by prepend PREFIX to it.
Return an unambiguous refname, either REFNAME or that prefixed
with PREFIX, nil otherwise.  If REFNAME has an offset suffix
such as \"~1\", then that is preserved.  If optional PREFIX is
nil, then use \"heads/\".  "
  (if (mercit-ref-ambiguous-p refname)
      (let ((refname (concat (or prefix "heads/") refname)))
        (and (not (mercit-ref-ambiguous-p refname)) refname))
    refname))

(defun mercit-ref-exists-p (ref)
  (mercit-git-success "identify" "--rev" ref))

(defun mercit-ref-equal (a b)
  "Return t if the refnames A and B are `equal'.
A symbolic-ref pointing to some ref, is `equal' to that ref,
as are two symbolic-refs pointing to the same ref.  Refnames
may be abbreviated."
  (let ((a (mercit-ref-fullname a))
        (b (mercit-ref-fullname b)))
    (and a b (equal a b))))

(defun mercit-ref-eq (a b)
  "Return t if the refnames A and B are `eq'.
A symbolic-ref is `eq' to itself, but not to the ref it points
to, or to some other symbolic-ref that points to the same ref."
  (let ((symbolic-a (mercit-symbolic-ref-p a))
        (symbolic-b (mercit-symbolic-ref-p b)))
    (or (and symbolic-a
             symbolic-b
             (equal a b))
        (and (not symbolic-a)
             (not symbolic-b)
             (mercit-ref-equal a b)))))

(defun mercit-headish ()    ;; TODO
  "Return the `HEAD' or if that doesn't exist the hash of the empty tree."
  (if (mercit-no-commit-p)
      (mercit-git-string "mktree")
    "HEAD"))

(defun mercit-branch-at-point ()
  (mercit-section-case
    (branch (oref it value))
    (commit (or (mercit--painted-branch-at-point)
                (mercit-name-branch (oref it value))))))

(defun mercit--painted-branch-at-point (&optional type)
  (or (and (not (eq type 'remote))
           (memq (get-text-property (mercit-point) 'font-lock-face)
                 (list 'mercit-branch-local
                       'mercit-branch-current))
           (and-let* ((branch (mercit-thing-at-point 'git-revision t)))
             (cdr (mercit-split-branch-name branch))))
      (and (not (eq type 'local))
           (memq (get-text-property (mercit-point) 'font-lock-face)
                 (list 'mercit-branch-remote
                       'mercit-branch-remote-head))
           (thing-at-point 'git-revision t))))

(defun mercit-local-branch-at-point ()
  (mercit-section-case
    (branch (let ((branch (mercit-ref-maybe-qualify (oref it value))))
              (when (member branch (mercit-list-local-branch-names))
                branch)))
    (commit (or (mercit--painted-branch-at-point 'local)
                (mercit-name-local-branch (oref it value))))))

(defun mercit-remote-branch-at-point ()
  (mercit-section-case
    (branch (let ((branch (oref it value)))
              (when (member branch (mercit-list-remote-branch-names))
                branch)))
    (commit (or (mercit--painted-branch-at-point 'remote)
                (mercit-name-remote-branch (oref it value))))))

(defun mercit-commit-at-point ()
  (or (mercit-section-value-if 'commit)
      (mercit-thing-at-point 'git-revision t)
      (and-let* ((chunk (and (bound-and-true-p mercit-blame-mode)
                             (fboundp 'mercit-current-blame-chunk)
                             (mercit-current-blame-chunk))))
        (oref chunk orig-rev))
      (and (derived-mode-p 'mercit-stash-mode
                           'mercit-merge-preview-mode
                           'mercit-revision-mode)
           mercit-buffer-revision)))

(defun mercit-branch-or-commit-at-point ()   ;; TODO
  (or (mercit-section-case
        (branch (mercit-ref-maybe-qualify (oref it value)))
        (commit (or (mercit--painted-branch-at-point)
                    (let ((rev (oref it value)))
                      (or (mercit-name-branch rev) rev))))
        (tag (mercit-ref-maybe-qualify (oref it value) "tags/"))
        (pullreq (or (and (fboundp 'forge--pullreq-branch)
                          (mercit-branch-p
                           (forge--pullreq-branch (oref it value))))
                     (mercit-ref-p (format "refs/pullreqs/%s"
                                          (oref (oref it value) number))))))
      (mercit-thing-at-point 'git-revision t)
      (and-let* ((chunk (and (bound-and-true-p mercit-blame-mode)
                             (fboundp 'mercit-current-blame-chunk)
                             (mercit-current-blame-chunk))))
        (oref chunk orig-rev))
      (and mercit-buffer-file-name
           mercit-buffer-refname)
      (and (derived-mode-p 'mercit-stash-mode
                           'mercit-merge-preview-mode
                           'mercit-revision-mode)
           mercit-buffer-revision)))

(defun mercit-tag-at-point ()
  (mercit-section-case
    (tag    (oref it value))
    (commit (mercit-name-tag (oref it value)))))

(defun mercit-stash-at-point ()
  (mercit-section-value-if 'stash))

(defun mercit-remote-at-point ()
  (mercit-section-case
    (remote (oref it value))
    ([branch remote] (mercit-section-parent-value it))))

(defun mercit-module-at-point (&optional predicate)
  (when (mercit-section-match 'mercit-module-section)
    (let ((module (oref (mercit-current-section) value)))
      (and (or (not predicate)
               (funcall predicate module))
           module))))

(defun mercit-get-current-branch ()
  "Return the refname of the currently checked out branch.
Return nil if no branch is currently checked out."
  (mercit-git-string "identify" "--branch"))

(defvar mercit-get-previous-branch-timeout 0.5
  "Maximum time to spend in `mercit-get-previous-branch'.
Given as a number of seconds.")

(defun mercit-get-previous-branch ()
  "Return the refname of the previously checked out branch.
Return nil if no branch can be found in the `HEAD' reflog
which is different from the current branch and still exists.
The amount of time spent searching is limited by
`mercit-get-previous-branch-timeout'."
  (let ((t0 (float-time))
        (current (mercit-get-current-branch))
        (i 1) prev)
    (while (if (> (- (float-time) t0) mercit-get-previous-branch-timeout)
               (setq prev nil) ;; Timed out.
             (and (setq prev (mercit-rev-verify (format "--rev=-%i" i)))
                  (or (not (setq prev (mercit-rev-branch prev)))
                      (equal prev current))))
      (cl-incf i))
    prev))

(defun mercit-set-upstream-branch (branch upstream)  ;; TODO
  "Set UPSTREAM as the upstream of BRANCH.
If UPSTREAM is nil, then unset BRANCH's upstream.
Otherwise UPSTREAM has to be an existing branch."
  (if upstream
      (mercit-call-git "branch" "--set-upstream-to" upstream branch)
    (mercit-call-git "branch" "--unset-upstream" branch)))

(defun mercit-get-upstream-ref (&optional branch)  ;; TODO
  "Return the upstream branch of BRANCH as a fully qualified ref.
It BRANCH is nil, then return the upstream of the current branch,
if any, nil otherwise.  If the upstream is not configured, the
configured remote is an url, or the named branch does not exist,
then return nil.  I.e.  return an existing local or
remote-tracking branch ref."
  (and-let* ((branch (or branch (mercit-get-current-branch))))
    (mercit-ref-fullname (concat branch "@{upstream}"))))

(defun mercit-get-upstream-branch (&optional branch)  ;; TODO
  "Return the name of the upstream branch of BRANCH.
It BRANCH is nil, then return the upstream of the current branch
if any, nil otherwise.  If the upstream is not configured, the
configured remote is an url, or the named branch does not exist,
then return nil.  I.e. return the name of an existing local or
remote-tracking branch.  The returned string is colorized
according to the branch type."
  (mercit--with-refresh-cache
   (list default-directory 'mercit-get-upstream-branch branch)
   (or branch (mercit-get-current-branch))))  ;; mercurial: same as lokal
    ;; (and-let* ((branch (or branch (mercit-get-current-branch)))
    ;;            (upstream (mercit-ref-abbrev (concat branch "@{upstream}"))))
    ;;   (mercit--propertize-face
    ;;    upstream (if (equal (mercit-get "branch" branch "remote") ".")
    ;;                 'mercit-branch-local
    ;;               'mercit-branch-remote)))))

(defun mercit-get-indirect-upstream-branch (branch &optional force)  ;; TODO
  (let ((remote (mercit-get "branch" branch "remote")))
    (and remote (not (equal remote "."))
         ;; The user has opted in...
         (or force
             (--some (if (mercit-git-success "check-ref-format" "--branch" it)
                         (equal it branch)
                       (string-match-p it branch))
                     mercit-branch-prefer-remote-upstream))
         ;; and local BRANCH tracks a remote branch...
         (let ((upstream (mercit-get-upstream-branch branch)))
           ;; whose upstream...
           (and upstream
                ;; has the same name as BRANCH...
                (equal (substring upstream (1+ (length remote))) branch)
                ;; and can be fast-forwarded to BRANCH.
                (mercit-rev-ancestor-p upstream branch)
                upstream)))))

(defun mercit-get-upstream-remote (&optional branch allow-unnamed)  ;; TODO
  (and-let* ((branch (or branch (mercit-get-current-branch)))
             (remote (mercit-get "branch" branch "remote")))
    (and (not (equal remote "."))
         (cond ((member remote (mercit-list-remotes))
                (mercit--propertize-face remote 'mercit-branch-remote))
               ((and allow-unnamed
                     (string-match-p "\\(\\`.\\{0,2\\}/\\|[:@]\\)" remote))
                (mercit--propertize-face remote 'bold))))))

(defun mercit-get-unnamed-upstream (&optional branch)  ;; TODO
  (and-let* ((branch (or branch (mercit-get-current-branch)))
             (remote (mercit-get "branch" branch "remote"))
             (merge  (mercit-get "branch" branch "merge")))
    (and (mercit--unnamed-upstream-p remote merge)
         (list (mercit--propertize-face remote 'bold)
               (mercit--propertize-face merge 'mercit-branch-remote)))))

(defun mercit--unnamed-upstream-p (remote merge)  ;; TODO
  (and remote (string-match-p "\\(\\`\\.\\{0,2\\}/\\|[:@]\\)" remote)
       merge  (string-prefix-p "refs/" merge)))

(defun mercit--valid-upstream-p (remote merge)  ;; TODO
  (and (or (equal remote ".")
           (member remote (mercit-list-remotes)))
       (string-prefix-p "refs/" merge)))

(defun mercit-get-current-remote (&optional allow-unnamed)
  (or (mercit-get-upstream-remote nil allow-unnamed)
      (and-let* ((remotes (mercit-list-remotes))
                 (remote (if (length= remotes 1)
                             (car remotes)
                           (mercit-primary-remote))))
        (mercit--propertize-face remote 'mercit-branch-remote))))

(defun mercit-get-push-remote (&optional branch)  ;; TODO
  (and-let* ((remote
              (or (and (or branch (setq branch (mercit-get-current-branch)))
                       (mercit-get "branch" branch "pushRemote"))
                  (mercit-get "remote.pushDefault"))))
    (mercit--propertize-face remote 'mercit-branch-remote)))

(defun mercit-get-push-branch (&optional branch verify)  ;; TODO
  (mercit--with-refresh-cache
      (list default-directory 'mercit-get-push-branch branch verify)
    (and-let* ((branch (or branch (setq branch (mercit-get-current-branch))))
               (remote (mercit-get-push-remote branch))
               (target (concat remote "/" branch)))
      (and (or (not verify)
               (mercit-rev-verify target))
           (mercit--propertize-face target 'mercit-branch-remote)))))

(defun mercit-get-@{push}-branch (&optional branch)  ;; TODO
  (let ((ref (mercit-rev-parse "--symbolic-full-name"
                              (concat branch "@{push}"))))
    (when (and ref (string-prefix-p "refs/remotes/" ref))
      (substring ref 13))))

(defun mercit-get-remote (&optional branch)  ;; TODO
  (when (or branch (setq branch (mercit-get-current-branch)))
    (let ((remote (mercit-get "branch" branch "remote")))
      (unless (equal remote ".")
        remote))))

(defun mercit-get-some-remote (&optional branch)
  (or (mercit-get-remote branch)
      (and-let* ((main (mercit-main-branch)))
        (mercit-get-remote main))
      (mercit-primary-remote)
      (car (mercit-list-remotes))))

(defvar mercit-primary-remote-names
  '("default" "upstream"))

(defun mercit-primary-remote ()
  "Return the primary remote.

The primary remote is the remote that tracks the repository that
other repositories are forked from.  It often is called \"origin\"
but because many people name their own fork \"origin\", using that
term would be ambiguous.  Likewise we avoid the term \"upstream\"
because a branch's @{upstream} branch may be a local branch or a
branch from a remote other than the primary remote.

If a remote exists whose name matches `mercit.primaryRemote', then
that is considered the primary remote.  If no remote by that name
exists, then remotes in `mercit-primary-remote-names' are tried in
order and the first remote from that list that actually exists in
the current repository is considered its primary remote."
  (let ((remotes (mercit-list-remotes)))
    (seq-find (lambda (name)
                (member name remotes))
              (delete-dups
               (delq nil
                     (cons (mercit-get "mercit.primaryRemote")  ;; TODO
                           mercit-primary-remote-names))))))

(defun mercit-branch-merged-p (branch &optional target)  ;; TODO
  "Return non-nil if BRANCH is merged into its upstream and TARGET.

TARGET defaults to the current branch.  If `HEAD' is detached and
TARGET is nil, then always return nil.  As a special case, if
TARGET is t, then return non-nil if BRANCH is merged into any one
of the other local branches.

If, and only if, BRANCH has an upstream, then only return non-nil
if BRANCH is merged into both TARGET (as described above) as well
as into its upstream."
  (and (--if-let (and (mercit-branch-p branch)
                      (mercit-get-upstream-branch branch))
           (mercit-git-success "merge-base" "--is-ancestor" branch it)
         t)
       (if (eq target t)
           (delete (mercit-name-local-branch branch)
                   (mercit-list-containing-branches branch))
         (and-let* ((target (or target (mercit-get-current-branch))))
           (mercit-git-success "merge-base" "--is-ancestor" branch target)))))

(defun mercit-get-tracked (refname)  ;; TODO
  "Return the remote branch tracked by the remote-tracking branch REFNAME.
The returned value has the form (REMOTE . REF), where REMOTE is
the name of a remote and REF is the ref local to the remote."
  (and-let* ((ref (mercit-ref-fullname refname)))
    (save-match-data
      (seq-some (lambda (line)
                  (and (string-match "\
\\`remote\\.\\([^.]+\\)\\.fetch=\\+?\\([^:]+\\):\\(.+\\)" line)
                       (let ((rmt (match-string 1 line))
                             (src (match-string 2 line))
                             (dst (match-string 3 line)))
                         (and (string-match (format "\\`%s\\'"
                                                    (string-replace
                                                     "*" "\\(.+\\)" dst))
                                            ref)
                              (cons rmt (string-replace
                                         "*" (match-string 1 ref) src))))))
                (mercit-git-lines "config" "paths"))))) ;; was  "--local"  "--list"

(defun mercit-split-branch-name (branch)  ;; TODO
  (cond ((member branch (mercit-list-local-branch-names))
         (cons "." branch))
        ((string-match "/" branch)
         (or (seq-some (lambda (remote)
                         (and (string-match
                               (format "\\`\\(%s\\)/\\(.+\\)\\'" remote)
                               branch)
                              (cons (match-string 1 branch)
                                    (match-string 2 branch))))
                       (mercit-list-remotes))
             (error "Invalid branch name %s" branch)))))

(defun mercit-get-current-tag (&optional rev with-distance)  ;; TODO
  "Return the closest tag reachable from REV.

If optional REV is nil, then default to `HEAD'.
If optional WITH-DISTANCE is non-nil then return (TAG COMMITS),
if it is `dirty' return (TAG COMMIT DIRTY). COMMITS is the number
of commits in `HEAD' but not in TAG and DIRTY is t if there are
uncommitted changes, nil otherwise."
  (and-let* ((str (mercit-git-str "describe" "--long" "--tags"
                                 (and (eq with-distance 'dirty) "--dirty")
                                 rev)))
    (save-match-data
      (string-match
       "\\(.+\\)-\\(?:0[0-9]*\\|\\([0-9]+\\)\\)-g[0-9a-z]+\\(-dirty\\)?$" str)
      (if with-distance
          `(,(match-string 1 str)
            ,(string-to-number (or (match-string 2 str) "0"))
            ,@(and (match-string 3 str) (list t)))
        (match-string 1 str)))))

(defun mercit-get-next-tag (&optional rev with-distance)  ;; TODO
  "Return the closest tag from which REV is reachable.

If optional REV is nil, then default to `HEAD'.
If no such tag can be found or if the distance is 0 (in which
case it is the current tag, not the next), return nil instead.
If optional WITH-DISTANCE is non-nil, then return (TAG COMMITS)
where COMMITS is the number of commits in TAG but not in REV."
  (and-let* ((str (mercit-git-str "describe" "--contains" (or rev "HEAD"))))
    (save-match-data
      (when (string-match "^[^^~]+" str)
        (setq str (match-string 0 str))
        (unless (equal str (mercit-get-current-tag rev))
          (if with-distance
              (list str (car (mercit-rev-diff-count str rev)))
            str))))))

(defun mercit-list-refs (&optional namespaces format sortby)  ;; TODO
  "Return list of references.

When NAMESPACES is non-nil, list refs from these namespaces
rather than those from `mercit-list-refs-namespaces'.

FORMAT is passed to the `--format' flag of `git for-each-ref'
and defaults to \"%(refname)\".  If the format is \"{branch}\"
or \"{branch}\", then drop the symbolic-ref `HEAD'.

SORTBY is a key or list of keys to pass to the `--sort' flag of
`git for-each-ref'.  When nil, use `mercit-list-refs-sortby'"
  (unless format
    (setq format "{branch}\n"))
  (let ((refs (mercit-git-lines "branches" "--template" format
                               ;; (--map (concat "--sort=" it)  TODO
                               ;;        (pcase (or sortby mercit-list-refs-sortby)
                               ;;          ((and val (pred stringp)) (list val))
                               ;;          ((and val (pred listp)) val)))
                               ;; (or namespaces mercit-list-refs-namespaces)
                               )))
    (if (member format '("{branch}\n" "{branch}\n"))
        (let ((case-fold-search nil))
          (--remove (string-match-p "\\(\\`\\|/\\)HEAD\\'" it)
                    refs))
      refs)))

(defun mercit-list-branches ()
  ;; local and remote branches
  (mercit-git-lines "branches" "--template" "{branch}\n"))

(defun mercit-list-local-branches ()
  (mercit-git-lines "branches" "--template" "{branch}\n"))

(defun mercit-list-remote-branches (&optional remote)  ;; TODO
  (mercit-list-refs (concat "refs/remotes/" remote)))

(defun mercit-list-related-branches (relation &optional commit &rest args)  ;; TODO
  (--remove (string-match-p "\\(\\`(HEAD\\|HEAD -> \\)" it)
            (--map (substring it 2)
                   (mercit-git-lines "branch" args relation commit))))

(defun mercit-list-containing-branches (&optional commit &rest args)  ;; TODO
  (mercit-list-related-branches "--contains" commit args))

(defun mercit-list-publishing-branches (&optional commit)  ;; TODO
  (--filter (mercit-rev-ancestor-p (or commit ".") it)
            mercit-published-branches))

(defun mercit-list-merged-branches (&optional commit &rest args)  ;; TODO
  (mercit-list-related-branches "--merged" commit args))

(defun mercit-list-unmerged-branches (&optional commit &rest args)  ;; TODO
  (mercit-list-related-branches "--no-merged" commit args))

(defun mercit-list-unmerged-to-upstream-branches ()  ;; TODO
  (--filter (and-let* ((upstream (mercit-get-upstream-branch it)))
              (member it (mercit-list-unmerged-branches upstream)))
            (mercit-list-local-branch-names)))

(defun mercit-list-branches-pointing-at (commit)  ;; TODO
  (let ((re (format "\\`%s refs/\\(heads\\|remotes\\)/\\(.*\\)\\'"
                    (mercit-rev-verify commit))))
    (--keep (and (string-match re it)
                 (let ((name (match-string 2 it)))
                   (and (not (string-suffix-p "HEAD" name))
                        name)))
            (mercit-git-lines "show-ref"))))

(defun mercit-list-refnames (&optional namespaces include-special)  ;; TODO
  (nconc (mercit-list-refs namespaces "{branch}\n")  ;; TODO topics, tags, branch
         (and include-special
              (mercit-list-special-refnames))))

(defvar mercit-special-refnames
  '("HEAD" "ORIG_HEAD" "FETCH_HEAD" "MERGE_HEAD" "CHERRY_PICK_HEAD"))

(defun mercit-list-special-refnames ()
  (let ((gitdir (mercit-gitdir)))
    (cl-mapcan (lambda (name)
                 (and (file-exists-p (expand-file-name name gitdir))
                      (list name)))
               mercit-special-refnames)))

(defun mercit-list-branch-names ()
  ;; local an remote branches
  (mercit-git-lines "branches" "-T" "{branch}\n"))

(defun mercit-list-local-branch-names ()
  (mercit-git-lines "branches" "-T" "{branch}\n"))

(defun mercit-list-remote-branch-names (&optional remote relative) ;; TODO
  (if (and remote relative)
      (let ((regexp (format "^refs/remotes/%s/\\(.+\\)" remote)))
        (--mapcat (when (string-match regexp it)
                    (list (match-string 1 it)))
                  (mercit-list-remote-branches remote)))
    (mercit-list-refnames (concat "refs/remotes/" remote))))

(defun mercit-list-remotes ()
  (mercit-git-lines "remote"))

(defun mercit-list-tags ()
  (mercit-git-lines "tags" "--quiet"))

(defun mercit-list-stashes (&optional format)
  (mercit-git-lines "shelve" "--list" "--template" (or format "%gd")))  ;; TODO

(defun mercit-list-active-notes-refs ()  ;; TODO
  "Return notes refs according to `core.notesRef' and `notes.displayRef'."
  (mercit-git-lines "for-each-ref" "--format=%(refname)"
                   (or (mercit-get "core.notesRef") "refs/notes/commits")
                   (mercit-get-all "notes.displayRef")))

(defun mercit-list-notes-refnames ()  ;; TODO
  (--map (substring it 6) (mercit-list-refnames "refs/notes")))

(defun mercit-remote-list-tags (remote)  ;; TODO
  (--keep (and (not (string-suffix-p "^{}" it))
               (substring it 51))
          (mercit-git-lines "ls-remote" "--tags" remote)))

(defun mercit-remote-list-branches (remote)  ;; TODO
  (--keep (and (not (string-suffix-p "^{}" it))
               (substring it 52))
          (mercit-git-lines "ls-remote" "--heads" remote)))

(defun mercit-remote-list-refs (remote)  ;; TODO
  (--keep (and (not (string-suffix-p "^{}" it))
               (substring it 41))
          (mercit-git-lines "ls-remote" remote)))

(defun mercit-remote-head (remote)  ;; TODO
  (and-let* ((line (cl-find-if
                    (lambda (line)
                      (string-match
                       "\\`ref: refs/heads/\\([^\s\t]+\\)[\s\t]HEAD\\'" line))
                    (mercit-git-lines "ls-remote" "--symref" remote "HEAD"))))
    (match-string 1 line)))

(defun mercit-list-modified-modules ()  ;; TODO
  (--keep (and (string-match "\\`\\+\\([^ ]+\\) \\(.+\\) (.+)\\'" it)
               (match-string 2 it))
          (mercit-git-lines "submodule" "status")))

(defun mercit-list-module-paths ()  ;; TODO
  (mercit-with-toplevel
    (--mapcat (and (string-match "^160000 [0-9a-z]\\{40,\\} 0\t\\(.+\\)$" it)
                   (list (match-string 1 it)))
              (mercit-git-items "ls-files" "-z" "--stage"))))

(defun mercit-list-module-names ()  ;; TODO
  (mapcar #'mercit-get-submodule-name (mercit-list-module-paths)))

(defun mercit-get-submodule-name (path)  ;; TODO
  "Return the name of the submodule at PATH.
PATH has to be relative to the super-repository."
  (if (mercit-git-version>= "2.38.0")
      ;; "git submodule--helper name" was removed,
      ;; but might still come back in another form.
      (substring
       (car (split-string
             (car (or (mercit-git-items
                       "config" "-z"
                       "-f" (expand-file-name ".gitmodules" (mercit-toplevel))
                       "--get-regexp" "^submodule\\..*\\.path$"
                       (concat "^" (regexp-quote (directory-file-name path)) "$"))
                      (error "No such submodule `%s'" path)))
             "\n"))
       10 -5)
    (mercit-git-string "submodule--helper" "name" path)))

(defun mercit-list-worktrees ()  ;; TODO
  (let ((remote (file-remote-p default-directory))
        worktrees worktree)
    (dolist (line (let ((mercit-git-global-arguments
                         ;; KLUDGE At least in v2.8.3 this triggers a segfault.
                         (remove "--no-pager" mercit-git-global-arguments)))
                    (mercit-git-lines "worktree" "list" "--porcelain")))
      (cond ((string-prefix-p "worktree" line)
             (let ((path (substring line 9)))
               (when remote
                 (setq path (concat remote path)))
               ;; If the git directory is separate from the main
               ;; worktree, then "git worktree" returns the git
               ;; directory instead of the worktree, which isn't
               ;; what it is supposed to do and not what we want.
               ;; However, if the worktree has been removed, then
               ;; we want to return it anyway; instead of nil.
               (setq path (or (mercit-toplevel path) path))
               (setq worktree (list path nil nil nil))
               (push worktree worktrees)))
            ((string-equal line "bare")
             (let* ((default-directory (car worktree))
                    (wt (and (not (mercit-get-boolean "core.bare"))
                             (mercit-get "core.worktree"))))
               (if (and wt (file-exists-p (expand-file-name wt)))
                   (progn (setf (nth 0 worktree) (expand-file-name wt))
                          (setf (nth 2 worktree) (mercit-rev-parse "HEAD"))
                          (setf (nth 3 worktree) (mercit-get-current-branch)))
                 (setf (nth 1 worktree) t))))
            ((string-prefix-p "HEAD" line)
             (setf (nth 2 worktree) (substring line 5)))
            ((string-prefix-p "branch" line)
             (setf (nth 3 worktree) (substring line 18)))
            ((string-equal line "detached"))))
    (nreverse worktrees)))

(defun mercit-symbolic-ref-p (name)  ;; TODO
  (mercit-git-success "symbolic-ref" "--quiet" name))

(defun mercit-ref-p (rev)  ;; TODO
  (or (car (member rev (mercit-list-refs "refs/")))
      (car (member rev (mercit-list-refnames "refs/")))))

(defun mercit-branch-p (rev)
  (or (car (member rev (mercit-list-branches)))
      (car (member rev (mercit-list-branch-names)))))

(defun mercit-local-branch-p (rev)
  (or (car (member rev (mercit-list-local-branches)))
      (car (member rev (mercit-list-local-branch-names)))))

(defun mercit-remote-branch-p (rev)
  (or (car (member rev (mercit-list-remote-branches)))
      (car (member rev (mercit-list-remote-branch-names)))))

(defun mercit-branch-set-face (branch)
  (mercit--propertize-face branch (if (mercit-local-branch-p branch)
                                     'mercit-branch-local
                                   'mercit-branch-remote)))

(defun mercit-tag-p (rev)
  (car (member rev (mercit-list-tags))))

(defun mercit-remote-p (string)
  (car (member string (mercit-list-remotes))))

(defvar mercit-main-branch-names
  '("defaul" "main" "trunk" "development")
  "Branch names reserved for use by the primary branch.
Use function `mercit-main-branch' to get the name actually used in
the current repository.")

(defvar mercit-long-lived-branches
  (append mercit-main-branch-names (list "maint" "next"))
  "Branch names reserved for use by long lived branches.")

(defun mercit-main-branch ()
  "Return the main branch.

If a branch exists whose name matches `init.defaultBranch', then
that is considered the main branch.  If no branch by that name
exists, then the branch names in `mercit-main-branch-names' are
tried in order.  The first branch from that list that actually
exists in the current repository is considered its main branch."
  (let ((branches (mercit-list-local-branch-names)))
    (seq-find (lambda (name)
                (member name branches))
              (delete-dups
               (delq nil
                     (cons (mercit-get "init.defaultBranch")  ;; TODO?
                           mercit-main-branch-names))))))

(defun mercit-rev-diff-count (a b)  ;; TODO
  "Return the commits in A but not B and vice versa.
Return a list of two integers: (A>B B>A)."
  (mapcar #'string-to-number
          (split-string (mercit-git-string "rev-list"
                                          "--count" "--left-right"
                                          (concat a "..." b))
                        "\t")))

(defun mercit-abbrev-length ()  ;; TODO
  (let ((abbrev (mercit-get "core.abbrev")))
    (if (and abbrev (not (equal abbrev "auto")))
        (string-to-number abbrev)
      ;; Guess the length git will be using based on an example
      ;; abbreviation.  Actually HEAD's abbreviation might be an
      ;; outlier, so use the shorter of the abbreviations for two
      ;; commits.  See #3034.
      (if-let ((head (mercit-rev-parse "--id" "--rev=."))
               (head-len (length head)))
          (min head-len
               (--if-let (mercit-rev-parse "--id" "--rev=-1")
                   (length it)
                 head-len))
        ;; We're on an unborn branch, but perhaps the repository has
        ;; other commits.  See #4123.
        (if-let ((commits (mercit-git-lines "rev-list" "-n2" "--all"
                                           "--abbrev-commit")))
            (apply #'min (mapcar #'length commits))
          ;; A commit does not exist.  Fall back to the default of 7.
          7)))))

(defun mercit-abbrev-arg (&optional arg)  ;; TODO
  (format "--%s=%d" (or arg "abbrev") (mercit-abbrev-length)))

(defun mercit-rev-abbrev (rev)  ;; TODO
  (mercit-rev-parse (mercit-abbrev-arg "short") rev))

(defun mercit-commit-children (commit &optional args)  ;; TODO
  (mapcar #'car
          (--filter (member commit (cdr it))
                    (--map (split-string it " ")
                           (mercit-git-lines
                            "log" "--format=%H %P"
                            ;; (or args (list "--branches" "--tags" "--remotes"))
                            "--not" commit)))))

(defun mercit-commit-parents (commit)  ;; TODO
  (and-let* ((str (mercit-git-string "rev-list" "-1" "--parents" commit)))
    (cdr (split-string str))))

(defun mercit-patch-id (rev)  ;; TODO
  (mercit--with-connection-local-variables
   (mercit--with-temp-process-buffer
     (mercit-process-file
      shell-file-name nil '(t nil) nil shell-command-switch
      (let ((exec (shell-quote-argument (mercit-git-executable))))
        (format "%s diff-tree -u %s | %s patch-id" exec rev exec)))
     (car (split-string (buffer-string))))))

(defun mercit-rev-format (format &optional rev args)
  ;; Prefer `git log --no-walk' to `git show --no-patch' because it
  ;; performs better in some scenarios.
  (let ((str (mercit-git-string "log" "--limit=1"
                                "--template" format args
                                "--rev"
                                (if rev (mercit--rev-dereference rev) ".")
                                "--")))
    (and (not (string-equal str ""))
         str)))

(defun mercit-rev-insert-format (format &optional rev args)
  (mercit-git-insert "log" "--limit=1"
                     "--template" format args
                     "--rev"
                     (if rev (mercit--rev-dereference rev) ".")
                     "--"))

(defun mercit-format-rev-summary (rev)  ;; TODO
  (when-let* ((str (mercit-rev-format "{node|short} {desc|firstline}" rev)))
    (mercit--put-face 0 (string-match " " str) 'mercit-hash str)
    str))

(defvar mercit-ref-namespaces   ;; TODO
  '(("\\`HEAD\\'"                  . mercit-head)
    ("\\`refs/tags/\\(.+\\)"       . mercit-tag)
    ("\\`refs/heads/\\(.+\\)"      . mercit-branch-local)
    ("\\`refs/remotes/\\(.+\\)"    . mercit-branch-remote)
    ("\\`refs/bisect/\\(bad\\)"    . mercit-bisect-bad)
    ("\\`refs/bisect/\\(skip.*\\)" . mercit-bisect-skip)
    ("\\`refs/bisect/\\(good.*\\)" . mercit-bisect-good)
    ("\\`refs/stash$"              . mercit-refname-stash)
    ("\\`refs/wip/\\(.+\\)"        . mercit-refname-wip)
    ("\\`refs/pullreqs/\\(.+\\)"   . mercit-refname-pullreq)
    ("\\`\\(bad\\):"               . mercit-bisect-bad)
    ("\\`\\(skip\\):"              . mercit-bisect-skip)
    ("\\`\\(good\\):"              . mercit-bisect-good)
    ("\\`\\(.+\\)"                 . mercit-refname))
  "How refs are formatted for display.

Each entry controls how a certain type of ref is displayed, and
has the form (REGEXP . FACE).  REGEXP is a regular expression
used to match full refs.  The first entry whose REGEXP matches
the reference is used.

In log and revision buffers the first regexp submatch becomes the
\"label\" that represents the ref and is propertized with FONT.
In refs buffers the displayed text is controlled by other means
and this option only controls what face is used.")

(defun mercit-format-ref-labels (string)  ;; TODO
  (save-match-data
    (let ((regexp "\\(, \\|tag: \\|HEAD -> \\)")
          names)
      (if (and (derived-mode-p 'mercit-log-mode)
               (member "--simplify-by-decoration" mercit-buffer-log-args))
          (let ((branches (mercit-list-local-branch-names))
                (re (format "^%s/.+" (regexp-opt (mercit-list-remotes)))))
            (setq names
                  (--map (cond ((string-equal it "HEAD")     it)
                               ((string-equal it "tip")     it)
                               ((string-prefix-p "refs/" it) it)
                               ((member it branches) (concat "refs/heads/" it))
                               ((string-match re it) (concat "refs/remotes/" it))
                               (t                    (concat "refs/" it)))
                         (split-string
                          (string-replace "tag: " "refs/tags/" string)
                          regexp t))))
        (setq names (split-string string regexp t)))
      (let (state head upstream tags branches remotes other combined)
        (dolist (ref names)
          (let* ((face (cdr (--first (string-match (car it) ref)
                                     mercit-ref-namespaces)))
                 (name (mercit--propertize-face
                        (or (match-string 1 ref) ref) face)))
            (cl-case face
              ((mercit-bisect-bad mercit-bisect-skip mercit-bisect-good)
               (setq state name))
              (mercit-head
               (setq head (mercit--propertize-face "@" 'mercit-head)))
              (mercit-tag            (push name tags))
              (mercit-branch-local   (push name branches))
              (mercit-branch-remote  (push name remotes))
              (t                    (push name other)))))
        (setq remotes
              (-keep
               (lambda (name)
                 (if (string-match "\\`\\([^/]*\\)/\\(.*\\)\\'" name)
                     (let ((r (match-string 1 name))
                           (b (match-string 2 name)))
                       (and (not (equal b "HEAD"))
                            (if (equal (concat "refs/remotes/" name)
                                       (mercit-git-string
                                        "symbolic-ref"
                                        (format "refs/remotes/%s/HEAD" r)))
                                (mercit--propertize-face
                                 name 'mercit-branch-remote-head)
                              name)))
                   name))
               remotes))
        (let* ((current (mercit-get-current-branch))
               (target  (mercit-get-upstream-branch current)))
          (dolist (name branches)
            (let ((push (car (member (mercit-get-push-branch name) remotes))))
              (when push
                (setq remotes (delete push remotes))
                (string-match "^[^/]*/" push)
                (setq push (substring push 0 (match-end 0))))
              (cond
               ((equal name current)
                (setq head
                      (concat push
                              (mercit--propertize-face
                               name 'mercit-branch-current))))
               ((equal name target)
                (setq upstream
                      (concat push
                              (mercit--propertize-face
                               name '(mercit-branch-upstream
                                      mercit-branch-local)))))
               (t
                (push (concat push name) combined)))))
          (when (and target (not upstream))
            (if (member target remotes)
                (progn
                  (mercit--add-face-text-property
                   0 (length target) 'mercit-branch-upstream nil target)
                  (setq upstream target)
                  (setq remotes  (delete target remotes)))
              (when-let ((target (car (member target combined))))
                (mercit--add-face-text-property
                 0 (length target) 'mercit-branch-upstream nil target)
                (setq upstream target)
                (setq combined (delete target combined))))))
        (mapconcat #'identity
                   (flatten-tree `(,state
                                   ,head
                                   ,upstream
                                   ,@(nreverse tags)
                                   ,@(nreverse combined)
                                   ,@(nreverse remotes)
                                   ,@other))
                   " ")))))

(defun mercit-object-type (object)  ;; TODO
  (mercit-git-string "cat-file" "-t" object))

(defmacro mercit-with-blob (commit file &rest body)  ;; TODO
  (declare (indent 2)
           (debug (form form body)))
  `(mercit--with-temp-process-buffer
     (let ((buffer-file-name ,file))
       (save-excursion
         (mercit-git-insert "cat-file" "-p"
                           (concat ,commit ":" buffer-file-name)))
       (decode-coding-inserted-region
        (point-min) (point-max) buffer-file-name t nil nil t)
       ,@body)))

(defvar mercit-tramp-process-environment nil)

(defmacro mercit-with-temp-index (tree arg &rest body)  ;; TODO
  (declare (indent 2) (debug (form form body)))
  (let ((file (cl-gensym "file")))
    `(let ((mercit--refresh-cache nil)
           (,file (mercit-convert-filename-for-git
                   (make-temp-name (mercit-git-dir "index.mercit.")))))
       (unwind-protect
           (mercit-with-toplevel
             (--when-let ,tree
               (or (mercit-git-success "read-tree" ,arg it
                                      (concat "--index-output=" ,file))
                   (error "Cannot read tree %s" it)))
             (if (file-remote-p default-directory)
                 (let ((mercit-tramp-process-environment
                        (cons (concat "GIT_INDEX_FILE=" ,file)
                              mercit-tramp-process-environment)))
                   ,@body)
               (with-environment-variables (("GIT_INDEX_FILE" ,file))
                 ,@body)))
         (ignore-errors
           (delete-file (concat (file-remote-p default-directory) ,file)))))))

(defun mercit-commit-tree (message &optional tree &rest parents)  ;; TODO
  (mercit-git-string "commit-tree" "--no-gpg-sign" "-m" message
                    (--mapcat (list "-p" it) (delq nil parents))
                    (or tree
                        (mercit-git-string "write-tree")
                        (error "Cannot write tree"))))

(defun mercit-commit-worktree (message &optional arg &rest other-parents)  ;; TODO
  (mercit-with-temp-index "HEAD" arg
    (and (mercit-update-files (mercit-unstaged-files))
         (apply #'mercit-commit-tree message nil "HEAD" other-parents))))

(defun mercit-update-files (files)  ;; TODO
  (mercit-git-success "update-index" "--add" "--remove" "--" files))

(defun mercit-update-ref (ref message rev &optional stashish)  ;; TODO
  (let ((mercit--refresh-cache nil))
    (or (if (mercit-git-version>= "2.6.0")
            (zerop (mercit-call-git "update-ref" "--create-reflog"
                                   "-m" message ref rev
                                   (or (mercit-rev-verify ref) "")))
          ;; `--create-reflog' didn't exist before v2.6.0
          (let ((oldrev  (mercit-rev-verify ref))
                (logfile (mercit-git-dir (concat "logs/" ref))))
            (unless (file-exists-p logfile)
              (when oldrev
                (mercit-git-success "update-ref" "-d" ref oldrev))
              (make-directory (file-name-directory logfile) t)
              (with-temp-file logfile)
              (when (and oldrev (not stashish))
                (mercit-git-success "update-ref" "-m" "enable reflog"
                                   ref oldrev ""))))
          (mercit-git-success "update-ref" "-m" message ref rev
                             (or (mercit-rev-verify ref) "")))
        (error "Cannot update %s with %s" ref rev))))

(defconst mercit-range-re  ;; TODO
  (concat "\\`\\([^ \t]*[^.]\\)?"       ; revA
          "\\(\\.\\.\\.?\\)"            ; range marker
          "\\([^.][^ \t]*\\)?\\'"))     ; revB

(defun mercit-split-range (range)  ;; TODO
  (and (string-match mercit-range-re range)
       (let ((beg (or (match-string 1 range) "HEAD"))
             (end (or (match-string 3 range) "HEAD")))
         (cons (if (string-equal (match-string 2 range) "...")
                   (mercit-git-string "merge-base" beg end)
                 beg)
               end))))

(defun mercit-hash-range (range)
  (if (string-match mercit-range-re range)
      (concat (mercit-rev-hash (match-string 1 range))
              (match-string 2 range)
              (mercit-rev-hash (match-string 3 range)))
    (mercit-rev-hash range)))

(defvar mercit-revision-faces
  '(mercit-hash
    mercit-tag
    mercit-branch-remote
    mercit-branch-remote-head
    mercit-branch-local
    mercit-branch-current
    mercit-branch-upstream
    mercit-branch-warning
    mercit-head
    mercit-refname
    mercit-refname-stash
    mercit-refname-wip
    mercit-refname-pullreq))

(put 'git-revision 'thing-at-point #'mercit-thingatpt--git-revision)
(defun mercit-thingatpt--git-revision ()  ;; TODO
  (and-let* ((bounds
              (let ((c "\s\n\t~^:?*[\\"))
                (cl-letf (((get 'git-revision 'beginning-op)
                           (lambda ()
                             (if (re-search-backward (format "[%s]" c) nil t)
                                 (forward-char)
                               (goto-char (point-min)))))
                          ((get 'git-revision 'end-op)
                           (lambda ()
                             (re-search-forward (format "\\=[^%s]*" c) nil t))))
                  (bounds-of-thing-at-point 'git-revision))))
             (string (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (and (or (and (>= (length string) 7)
                  (string-match-p "[a-z]" string)
                  (mercit-commit-p string))
             (and (mercit-ref-p string)
                  (let ((face (get-text-property (point) 'face)))
                    (or (not face)
                        (member face mercit-revision-faces)))))
         string)))

;;; Completion

(defvar mercit-revision-history nil)

(defun mercit--minibuf-default-add-commit ()
  (let ((fn minibuffer-default-add-function))
    (lambda ()
      (if-let ((commit (with-selected-window (minibuffer-selected-window)
                         (mercit-commit-at-point))))
          (let ((rest (cons commit (delete commit (funcall fn))))
                (def minibuffer-default))
            (if (listp def)
                (append def rest)
              (cons def (delete def rest))))
        (funcall fn)))))

(defun mercit-read-branch (prompt &optional secondary-default)
  (mercit-completing-read prompt (mercit-list-branch-names)
                         nil t nil 'mercit-revision-history
                         (or (mercit-branch-at-point)
                             secondary-default
                             (mercit-get-current-branch))))

(defun mercit-read-branch-or-commit (prompt &optional secondary-default)
  (let ((minibuffer-default-add-function (mercit--minibuf-default-add-commit)))
    (or (mercit-completing-read prompt (mercit-list-refnames nil t)
                               nil nil nil 'mercit-revision-history
                               (or (mercit-branch-or-commit-at-point)
                                   secondary-default
                                   (mercit-get-current-branch)))
        (user-error "Nothing selected"))))

(defun mercit-read-range-or-commit (prompt &optional secondary-default)
  (mercit-read-range
   prompt
   (or (--when-let (mercit-region-values '(commit branch) t)
         (deactivate-mark)
         (concat (car (last it)) ".." (car it)))  ;; TODO
       (mercit-branch-or-commit-at-point)
       secondary-default
       (mercit-get-current-branch))))

(defun mercit-read-range (prompt &optional default)  ;; TODO
  (let ((minibuffer-default-add-function (mercit--minibuf-default-add-commit))
        (crm-separator "\\.\\.\\.?"))
    (mercit-completing-read-multiple*
     (concat prompt ": ")
     (mercit-list-refnames)
     nil nil nil 'mercit-revision-history default nil t)))

(defun mercit-read-remote-branch  ;; TODO
    (prompt &optional remote default local-branch require-match)
  (let ((choice (mercit-completing-read
                 prompt
                 (-union (and local-branch
                              (if remote
                                  (concat remote "/" local-branch)
                                (--map (concat it "/" local-branch)
                                       (mercit-list-remotes))))
                         (mercit-list-remote-branch-names remote t))
                 nil require-match nil 'mercit-revision-history default)))
    (if (or remote (string-match "\\`\\([^/]+\\)/\\(.+\\)" choice))
        choice
      (user-error "`%s' doesn't have the form REMOTE/BRANCH" choice))))

(defun mercit-read-refspec (prompt remote)  ;; TODO
  (mercit-completing-read prompt
                         (prog2 (message "Determining available refs...")
                             (mercit-remote-list-refs remote)
                           (message "Determining available refs...done"))))

(defun mercit-read-local-branch (prompt &optional secondary-default)
  (mercit-completing-read prompt (mercit-list-local-branch-names)
                         nil t nil 'mercit-revision-history
                         (or (mercit-local-branch-at-point)
                             secondary-default
                             (mercit-get-current-branch))))

(defun mercit-read-local-branch-or-commit (prompt)
  (let ((minibuffer-default-add-function (mercit--minibuf-default-add-commit))
        (choices (nconc (mercit-list-local-branch-names)
                        (mercit-list-special-refnames)))
        (commit (mercit-commit-at-point)))
    (when commit
      (push commit choices))
    (or (mercit-completing-read prompt choices
                               nil nil nil 'mercit-revision-history
                               (or (mercit-local-branch-at-point) commit))
        (user-error "Nothing selected"))))

(defun mercit-read-local-branch-or-ref (prompt &optional secondary-default)  ;; TODO
  (mercit-completing-read prompt (nconc (mercit-list-local-branch-names)
                                       (mercit-list-refs "refs/"))
                         nil t nil 'mercit-revision-history
                         (or (mercit-local-branch-at-point)
                             secondary-default
                             (mercit-get-current-branch))))

(defun mercit-read-other-branch
    (prompt &optional exclude secondary-default no-require-match)
  (let* ((current (mercit-get-current-branch))
         (atpoint (mercit-branch-at-point))
         (exclude (or exclude current))
         (default (or (and (not (equal atpoint exclude)) atpoint)
                      (and (not (equal current exclude)) current)
                      secondary-default
                      (mercit-get-previous-branch))))
    (mercit-completing-read prompt (delete exclude (mercit-list-branch-names))
                           nil (not no-require-match)
                           nil 'mercit-revision-history default)))

(defun mercit-read-other-branch-or-commit  ;; TODO
    (prompt &optional exclude secondary-default)
  (let* ((minibuffer-default-add-function (mercit--minibuf-default-add-commit))
         (current (mercit-get-current-branch))
         (atpoint (mercit-branch-or-commit-at-point))
         (exclude (or exclude current))
         (default (or (and (not (equal atpoint exclude))
                           (not (and (not current)
                                     (mercit-rev-equal atpoint "HEAD")))
                           atpoint)
                      (and (not (equal current exclude)) current)
                      secondary-default
                      (mercit-get-previous-branch))))
    (or (mercit-completing-read prompt (delete exclude (mercit-list-refnames))
                               nil nil nil 'mercit-revision-history default)
        (user-error "Nothing selected"))))

(defun mercit-read-other-local-branch
    (prompt &optional exclude secondary-default no-require-match)
  (let* ((current (mercit-get-current-branch))
         (atpoint (mercit-local-branch-at-point))
         (exclude (or exclude current))
         (default (or (and (not (equal atpoint exclude)) atpoint)
                      (and (not (equal current exclude)) current)
                      secondary-default
                      (mercit-get-previous-branch))))
    (mercit-completing-read prompt
                           (delete exclude (mercit-list-local-branch-names))
                           nil (not no-require-match)
                           nil 'mercit-revision-history default)))

(defun mercit-read-branch-prefer-other (prompt)
  (let* ((current (mercit-get-current-branch))
         (commit  (mercit-commit-at-point))
         (atrev   (and commit (mercit-list-branches-pointing-at commit)))
         (atpoint (mercit--painted-branch-at-point)))
    (mercit-completing-read prompt (mercit-list-branch-names)
                           nil t nil 'mercit-revision-history
                           (or (mercit-section-value-if 'branch)
                               atpoint
                               (and (not (cdr atrev)) (car atrev))
                               (--first (not (equal it current)) atrev)
                               (mercit-get-previous-branch)
                               (car atrev)))))

(defun mercit-read-upstream-branch (&optional branch prompt)
  "Read the upstream for BRANCH using PROMPT.
If optional BRANCH is nil, then read the upstream for the
current branch, or raise an error if no branch is checked
out.  Only existing branches can be selected."
  (unless branch
    (setq branch (or (mercit-get-current-branch)
                     (error "Need a branch to set its upstream"))))
  (let ((branches (delete branch (mercit-list-branch-names))))
    (mercit-completing-read
     (or prompt (format "Change upstream of %s to" branch))
     branches nil t nil 'mercit-revision-history
     (or (let ((r (car (member (mercit-remote-branch-at-point) branches)))
               (l (car (member (mercit-local-branch-at-point) branches))))
           (if mercit-prefer-remote-upstream (or r l) (or l r)))
         (and-let* ((main (mercit-main-branch)))
           (let ((r (car (member (concat "origin/" main) branches)))  ;; TODO
                 (l (car (member main branches))))
             (if mercit-prefer-remote-upstream (or r l) (or l r))))
         (car (member (mercit-get-previous-branch) branches))))))

(defun mercit-read-starting-point (prompt &optional branch default)
  (or (mercit-completing-read
       (concat prompt
               (and branch
                    (if (bound-and-true-p ivy-mode)
                        ;; Ivy-mode strips faces from prompt.
                        (format  " `%s'" branch)
                      (concat " " (mercit--propertize-face
                                   branch 'mercit-branch-local))))
               " starting at")
       (nconc (list "HEAD")  ;; TODO
              (mercit-list-refnames)
              (directory-files (mercit-git-dir) nil "_HEAD\\'"))
       nil nil nil 'mercit-revision-history
       (or default (mercit--default-starting-point)))
      (user-error "Nothing selected")))

(defun mercit--default-starting-point ()
  (or (let ((r (mercit-remote-branch-at-point))
            (l (mercit-local-branch-at-point)))
        (if mercit-prefer-remote-upstream (or r l) (or l r)))
      (mercit-commit-at-point)
      (mercit-stash-at-point)
      (mercit-get-current-branch)))

(defun mercit-read-tag (prompt &optional require-match)
  (mercit-completing-read prompt (mercit-list-tags) nil
                         require-match nil 'mercit-revision-history
                         (mercit-tag-at-point)))

(defun mercit-read-stash (prompt)
  (let* ((atpoint (mercit-stash-at-point))
         (default (and atpoint
                       (concat atpoint (mercit-rev-format "  {desc|firstline}"
                                                          atpoint))))
         (choices (mapcar (lambda (c)
                            (pcase-let ((`(,rev ,msg) (split-string c "\\0")))
                              (concat (propertize rev 'face 'mercit-hash)
                                      " " msg)))
                          (mercit-list-stashes "%gd%x00%s")))  ;; TODO
         (choice  (mercit-completing-read prompt choices
                                         nil t nil nil
                                         default
                                         (car choices))))
    (and choice
         (string-match "^\\([^ ]+\\) \\(.+\\)" choice)  ;; TODO
         (substring-no-properties (match-string 1 choice)))))

(defun mercit-read-remote (prompt &optional default use-only)
  (let ((remotes (mercit-list-remotes)))
    (if (and use-only (length= remotes 1))
        (car remotes)
      (mercit-completing-read prompt remotes
                             nil t nil nil
                             (or default
                                 (mercit-remote-at-point)
                                 (mercit-get-remote))))))

(defun mercit-read-remote-or-url (prompt &optional default)
  (mercit-completing-read prompt
                         (nconc (mercit-list-remotes)
                                (list "https://" "ssh://"))
                         nil nil nil nil
                         (or default
                             (mercit-remote-at-point)
                             (mercit-get-remote))))

(defun mercit-read-module-path (prompt &optional predicate)
  (mercit-completing-read prompt (mercit-list-module-paths)
                         predicate t nil nil
                         (mercit-module-at-point predicate)))

(defun mercit-module-confirm (verb &optional predicate)
  (let (modules)
    (if current-prefix-arg
        (progn
          (setq modules (mercit-list-module-paths))
          (when predicate
            (setq modules (-filter predicate modules)))
          (unless modules
            (if predicate
                (user-error "No modules satisfying %s available" predicate)
              (user-error "No modules available"))))
      (setq modules (mercit-region-values 'mercit-module-section))
      (when modules
        (when predicate
          (setq modules (-filter predicate modules)))
        (unless modules
          (user-error "No modules satisfying %s selected" predicate))))
    (if (length> modules 1)
        (mercit-confirm t nil (format "%s %%i modules" verb) nil modules)
      (list (mercit-read-module-path (format "%s module" verb) predicate)))))

;;; _
(provide 'mercit-git)
;;; mercit-git.el ends here
