;;; mercit-clone.el --- Clone a repository  -*- lexical-binding:t -*-

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

;; This library implements clone commands.

;;; Code:

(require 'mercit)

;;; Options

(defcustom mercit-clone-set-remote-head nil
  "Whether cloning creates the symbolic-ref `<remote>/HEAD'."
  :package-version '(mercit . "2.4.2")
  :group 'mercit-commands
  :type 'boolean)

(defcustom mercit-clone-set-remote.pushDefault 'ask
  "Whether to set the value of `remote.pushDefault' after cloning.

If t, then set without asking.  If nil, then don't set.  If
`ask', then ask."
  :package-version '(mercit . "2.4.0")
  :group 'mercit-commands
  :type '(choice (const :tag "set" t)
                 (const :tag "ask" ask)
                 (const :tag "don't set" nil)))

(defcustom mercit-clone-default-directory nil
  "Default directory to use when `mercit-clone' reads destination.
If nil (the default), then use the value of `default-directory'.
If a directory, then use that.  If a function, then call that
with the remote url as only argument and use the returned value."
  :package-version '(mercit . "2.90.0")
  :group 'mercit-commands
  :type '(choice (const     :tag "value of default-directory")
                 (directory :tag "constant directory")
                 (function  :tag "function's value")))

(defcustom mercit-clone-always-transient nil
  "Whether `mercit-clone' always acts as a transient prefix command.
If nil, then a prefix argument has to be used to show the transient
popup instead of invoking the default suffix `mercit-clone-regular'
directly."
  :package-version '(mercit . "3.0.0")
  :group 'mercit-commands
  :type 'boolean)

(defcustom mercit-clone-name-alist
  '(("\\`\\(?:github:\\|gh:\\)?\\([^:]+\\)\\'" "github.com" "github.user")
    ("\\`\\(?:gitlab:\\|gl:\\)\\([^:]+\\)\\'"  "gitlab.com" "gitlab.user")
    ("\\`\\(?:sourcehut:\\|sh:\\)\\([^:]+\\)\\'" "git.sr.ht" "sourcehut.user"))
  "Alist mapping repository names to repository urls.

Each element has the form (REGEXP HOSTNAME USER).  When the user
enters a name when a cloning command asks for a name or url, then
that is looked up in this list.  The first element whose REGEXP
matches is used.

The format specified by option `mercit-clone-url-format' is used
to turn the name into an url, using HOSTNAME and the repository
name.  If the provided name contains a slash, then that is used.
Otherwise if the name omits the owner of the repository, then the
default user specified in the matched entry is used.

If USER contains a dot, then it is treated as a Git variable and
the value of that is used as the username.  Otherwise it is used
as the username itself."
  :package-version '(mercit . "3.4.0")
  :group 'mercit-commands
  :type '(repeat (list regexp
                       (string :tag "Hostname")
                       (string :tag "User name or git variable"))))

(defcustom mercit-clone-url-format
  '(("git.sr.ht" . "git@%h:%n")
    (t . "git@%h:%n.git"))
  "Format(s) used when turning repository names into urls.

In a format string, %h is the hostname and %n is the repository
name, including the name of the owner.

The value can be a string (representing a single static format)
or an alist with elements (HOSTNAME . FORMAT) mapping hostnames
to formats.  When an alist is used, the t key represents the
default.  Also see `mercit-clone-name-alist'."
  :package-version '(mercit . "3.4.0")
  :group 'mercit-commands
  :type '(choice (string :tag "Format")
                 (alist :key-type (choice (string :tag "Host")
                                          (const :tag "Default" t))
                        :value-type (string :tag "Format"))))

;;; Commands

;;;###autoload (autoload 'mercit-clone "mercit-clone" nil t)
(transient-define-prefix mercit-clone (&optional transient)
  "Clone a repository."
  :man-page "git-clone"
  ["Fetch arguments"
   ("-B" "Clone a single branch"  "--single-branch")
   ("-n" "Do not clone tags"      "--no-tags")
   ("-S" "Clones submodules"      "--recurse-submodules" :level 6)
   ("-l" "Do not optimize"        "--no-local" :level 7)]
  ["Setup arguments"
   ("-o" "Set name of remote"     ("-o" "--origin="))
   ("-b" "Set HEAD branch"        ("-b" "--branch="))
   (mercit-clone:--filter
    :if (lambda () (mercit-git-version>= "2.17.0"))
    :level 7)
   ("-g" "Separate git directory" "--separate-git-dir="
    transient-read-directory :level 7)
   ("-t" "Use template directory" "--template="
    transient-read-existing-directory :level 6)]
  ["Local sharing arguments"
   ("-s" "Share objects"          ("-s" "--shared" :level 7))
   ("-h" "Do not use hardlinks"   "--no-hardlinks")]
  ["Clone"
   ("C" "regular"            mercit-clone-regular)
   ("s" "shallow"            mercit-clone-shallow)
   ("d" "shallow since date" mercit-clone-shallow-since :level 7)
   ("e" "shallow excluding"  mercit-clone-shallow-exclude :level 7)
   (">" "sparse checkout"    mercit-clone-sparse
    :if (lambda () (mercit-git-version>= "2.25.0"))
    :level 6)
   ("b" "bare"               mercit-clone-bare)
   ("m" "mirror"             mercit-clone-mirror)]
  (interactive (list (or mercit-clone-always-transient current-prefix-arg)))
  (if transient
      (transient-setup 'mercit-clone)
    (call-interactively #'mercit-clone-regular)))

(transient-define-argument mercit-clone:--filter ()
  :description "Filter some objects"
  :class 'transient-option
  :key "-f"
  :argument "--filter="
  :reader #'mercit-clone-read-filter)

(defun mercit-clone-read-filter (prompt initial-input history)
  (mercit-completing-read prompt
                         (list "blob:none" "tree:0")
                         nil nil initial-input history))

;;;###autoload
(defun mercit-clone-regular (repository directory args)
  "Create a clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository."
  (interactive (mercit-clone-read-args))
  (mercit-clone-internal repository directory args))

;;;###autoload
(defun mercit-clone-shallow (repository directory args depth)
  "Create a shallow clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.
With a prefix argument read the DEPTH of the clone;
otherwise use 1."
  (interactive (append (mercit-clone-read-args)
                       (list (if current-prefix-arg
                                 (read-number "Depth: " 1)
                               1))))
  (mercit-clone-internal repository directory
                        (cons (format "--depth=%s" depth) args)))

;;;###autoload
(defun mercit-clone-shallow-since (repository directory args date)
  "Create a shallow clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.
Exclude commits before DATE, which is read from the
user."
  (interactive (append (mercit-clone-read-args)
                       (list (transient-read-date "Exclude commits before: "
                                                  nil nil))))
  (mercit-clone-internal repository directory
                        (cons (format "--shallow-since=%s" date) args)))

;;;###autoload
(defun mercit-clone-shallow-exclude (repository directory args exclude)
  "Create a shallow clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.
Exclude commits reachable from EXCLUDE, which is a
branch or tag read from the user."
  (interactive (append (mercit-clone-read-args)
                       (list (read-string "Exclude commits reachable from: "))))
  (mercit-clone-internal repository directory
                        (cons (format "--shallow-exclude=%s" exclude) args)))

;;;###autoload
(defun mercit-clone-bare (repository directory args)
  "Create a bare clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository."
  (interactive (mercit-clone-read-args))
  (mercit-clone-internal repository directory (cons "--bare" args)))

;;;###autoload
(defun mercit-clone-mirror (repository directory args)
  "Create a mirror of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository."
  (interactive (mercit-clone-read-args))
  (mercit-clone-internal repository directory (cons "--mirror" args)))

;;;###autoload
(defun mercit-clone-sparse (repository directory args)
  "Clone REPOSITORY into DIRECTORY and create a sparse checkout."
  (interactive (mercit-clone-read-args))
  (mercit-clone-internal repository directory (cons "--no-checkout" args)
                        'sparse))

(defun mercit-clone-internal (repository directory args &optional sparse)
  (let* ((checkout (not (memq (car args) '("--bare" "--mirror"))))
         (remote (or (transient-arg-value "--origin" args)
                     (mercit-get "clone.defaultRemote")
                     "origin"))
         (set-push-default
          (and checkout
               (or (eq  mercit-clone-set-remote.pushDefault t)
                   (and mercit-clone-set-remote.pushDefault
                        (y-or-n-p (format "Set `remote.pushDefault' to %S? "
                                          remote)))))))
    (run-hooks 'mercit-credential-hook)
    (setq directory (file-name-as-directory (expand-file-name directory)))
    (when (file-exists-p directory)
      (if (file-directory-p directory)
          (when (length> (directory-files directory) 2)
            (let ((name (mercit-clone--url-to-name repository)))
              (unless (and name
                           (setq directory (file-name-as-directory
                                            (expand-file-name name directory)))
                           (not (file-exists-p directory)))
                (user-error "%s already exists" directory))))
        (user-error "%s already exists and is not a directory" directory)))
    (mercit-run-git-async "clone" args "--" repository
                         (mercit-convert-filename-for-git directory))
    ;; Don't refresh the buffer we're calling from.
    (process-put mercit-this-process 'inhibit-refresh t)
    (set-process-sentinel
     mercit-this-process
     (lambda (process event)
       (when (memq (process-status process) '(exit signal))
         (let ((mercit-process-raise-error t))
           (mercit-process-sentinel process event)))
       (when (and (eq (process-status process) 'exit)
                  (= (process-exit-status process) 0))
         (when checkout
           (let ((default-directory directory))
             (when set-push-default
               (setf (mercit-get "remote.pushDefault") remote))
             (unless mercit-clone-set-remote-head
               (mercit-remote-unset-head remote))))
         (when (and sparse checkout)
           (when (mercit-git-version< "2.25.0")
             (user-error
              "`git sparse-checkout' not available until Git v2.25"))
           (let ((default-directory directory))
             (mercit-call-git "sparse-checkout" "init" "--cone")
             (mercit-call-git "checkout" (mercit-get-current-branch))))
         (with-current-buffer (process-get process 'command-buf)
           (mercit-status-setup-buffer directory)))))))

(defun mercit-clone-read-args ()
  (let ((repo (mercit-clone-read-repository)))
    (list repo
          (read-directory-name
           "Clone to: "
           (if (functionp mercit-clone-default-directory)
               (funcall mercit-clone-default-directory repo)
             mercit-clone-default-directory)
           nil nil
           (mercit-clone--url-to-name repo))
          (transient-args 'mercit-clone))))

(defun mercit-clone-read-repository ()
  (mercit-read-char-case "Clone from " nil
    (?u "[u]rl or name"
        (let ((str (mercit-read-string-ns "Clone from url or name")))
          (if (string-match-p "\\(://\\|@\\)" str)
              str
            (mercit-clone--name-to-url str))))
    (?p "[p]ath"
        (mercit-convert-filename-for-git
         (read-directory-name "Clone repository: ")))
    (?l "[l]ocal url"
        (concat "file://"
                (mercit-convert-filename-for-git
                 (read-directory-name "Clone repository: file://"))))
    (?b "or [b]undle"
        (mercit-convert-filename-for-git
         (read-file-name "Clone from bundle: ")))))

(defun mercit-clone--url-to-name (url)
  (and (string-match "\\([^/:]+?\\)\\(/?\\.git\\)?$" url)
       (match-string 1 url)))

(defun mercit-clone--name-to-url (name)
  (or (seq-some
       (pcase-lambda (`(,re ,host ,user))
         (and (string-match re name)
              (let ((repo (match-string 1 name)))
                (mercit-clone--format-url host user repo))))
       mercit-clone-name-alist)
      (user-error "Not an url and no matching entry in `%s'"
                  'mercit-clone-name-alist)))

(defun mercit-clone--format-url (host user repo)
  (if-let ((url-format
            (cond ((listp mercit-clone-url-format)
                   (cdr (or (assoc host mercit-clone-url-format)
                            (assoc t mercit-clone-url-format))))
                  ((stringp mercit-clone-url-format)
                   mercit-clone-url-format))))
      (format-spec
       url-format
       `((?h . ,host)
         (?n . ,(if (string-search "/" repo)
                    repo
                  (if (string-search "." user)
                      (if-let ((user (mercit-get user)))
                          (concat user "/" repo)
                        (user-error "Set %S or specify owner explicitly" user))
                    (concat user "/" repo))))))
    (user-error
     "Bogus `mercit-clone-url-format' (bad type or missing default)")))

;;; _
(provide 'mercit-clone)
;;; mercit-clone.el ends here
