;;; mercit-remote.el --- Transfer Mercurial commits  -*- lexical-binding:t -*-

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

;; This library implements remote commands.

;;; Code:

(require 'mercit)

;;; Options

(defcustom mercit-remote-add-set-remote.pushDefault 'ask-if-unset
  "Whether to set the value of `remote.pushDefault' after adding a remote.

If `ask', then always ask.  If `ask-if-unset', then ask, but only
if the variable isn't set already.  If nil, then don't ever set.
If the value is a string, then set without asking, provided that
the name of the added remote is equal to that string and the
variable isn't already set."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-commands
  :type '(choice (const  :tag "ask if unset" ask-if-unset)
                 (const  :tag "always ask" ask)
                 (string :tag "set if named")
                 (const  :tag "don't set")))

(defcustom mercit-remote-direct-configure t
  "Whether the command `mercit-remote' shows Mercurial variables.
When set to nil, no variables are displayed by this transient
command, instead the sub-transient `mercit-remote-configure'
has to be used to view and change remote related variables."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-commands
  :type 'boolean)

(defcustom mercit-prefer-push-default nil
  "Whether to prefer `remote.pushDefault' over per-branch variables."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-commands
  :type 'boolean)

;;; Commands

;;;###autoload (autoload 'mercit-remote "mercit-remote" nil t)
(transient-define-prefix mercit-remote (remote)
  "Add, configure or remove a remote."
  :man-page "git-remote"
  :value '("-f")
  ["Variables"
   :if (lambda ()
         (and mercit-remote-direct-configure
              (oref transient--prefix scope)))
   ("u" mercit-remote.<remote>.url)
   ("U" mercit-remote.<remote>.fetch)
   ("s" mercit-remote.<remote>.pushurl)
   ("S" mercit-remote.<remote>.push)
   ("O" mercit-remote.<remote>.tagopt)]
  ["Arguments for add"
   ("-f" "Fetch after add" "-f")]
  ["Actions"
   [("a" "Add"                  mercit-remote-add)
    ("r" "Rename"               mercit-remote-rename)
    ("k" "Remove"               mercit-remote-remove)]
   [("C" "Configure..."         mercit-remote-configure)
    ("p" "Prune stale branches" mercit-remote-prune)
    ("P" "Prune stale refspecs" mercit-remote-prune-refspecs)
    (7 "z" "Unshallow remote"   mercit-remote-unshallow)]]
  (interactive (list (mercit-get-current-remote)))
  (transient-setup 'mercit-remote nil nil :scope remote))

(defun mercit-read-url (prompt &optional initial-input)
  (let ((url (mercit-read-string-ns prompt initial-input)))
    (if (string-prefix-p "~" url)
        (expand-file-name url)
      url)))

;;;###autoload
(defun mercit-remote-add (remote url &optional args)
  "Add a remote named REMOTE and fetch it."
  (interactive
   (let ((origin (mercit-get "remote.origin.url"))
         (remote (mercit-read-string-ns "Remote name")))
     (list remote
           (mercit-read-url
            "Remote url"
            (and origin
                 (string-match "\\([^:/]+\\)/[^/]+\\(\\.git\\)?\\'" origin)
                 (replace-match remote t t origin 1)))
           (transient-args 'mercit-remote))))
  (if (pcase (list mercit-remote-add-set-remote.pushDefault
                   (mercit-get "remote.pushDefault"))
        (`(,(pred stringp) ,_) t)
        ((or `(ask ,_) '(ask-if-unset nil))
         (y-or-n-p (format "Set `remote.pushDefault' to \"%s\"? " remote))))
      (progn (mercit-call-git "remote" "add" args remote url)
             (setf (mercit-get "remote.pushDefault") remote)
             (mercit-refresh))
    (mercit-run-git-async "remote" "add" args remote url)))

;;;###autoload
(defun mercit-remote-rename (old new)
  "Rename the remote named OLD to NEW."
  (interactive
   (let  ((remote (mercit-read-remote "Rename remote")))
     (list remote (mercit-read-string-ns (format "Rename %s to" remote)))))
  (unless (string= old new)
    (mercit-call-git "remote" "rename" old new)
    (mercit-remote--cleanup-push-variables old new)
    (mercit-refresh)))

;;;###autoload
(defun mercit-remote-remove (remote)
  "Delete the remote named REMOTE."
  (interactive (list (mercit-read-remote "Delete remote")))
  (mercit-call-git "remote" "rm" remote)
  (mercit-remote--cleanup-push-variables remote)
  (mercit-refresh))

(defun mercit-remote--cleanup-push-variables (remote &optional new-name)
  (mercit-with-toplevel
    (when (equal (mercit-get "remote.pushDefault") remote)
      (mercit-set new-name "remote.pushDefault"))
    (dolist (var (mercit-git-lines "config" "--name-only"
                                  "--get-regexp" "^branch\.[^.]*\.pushRemote"
                                  (format "^%s$" remote)))
      (mercit-call-git "config" (and (not new-name) "--unset") var new-name))))

(defconst mercit--refspec-re "\\`\\(\\+\\)?\\([^:]+\\):\\(.*\\)\\'")

;;;###autoload
(defun mercit-remote-prune (remote)
  "Remove stale remote-tracking branches for REMOTE."
  (interactive (list (mercit-read-remote "Prune stale branches of remote")))
  (mercit-run-git-async "remote" "prune" remote))

;;;###autoload
(defun mercit-remote-prune-refspecs (remote)
  "Remove stale refspecs for REMOTE.

A refspec is stale if there no longer exists at least one branch
on the remote that would be fetched due to that refspec.  A stale
refspec is problematic because its existence causes Mercurial to refuse
to fetch according to the remaining non-stale refspecs.

If only stale refspecs remain, then offer to either delete the
remote or to replace the stale refspecs with the default refspec.

Also remove the remote-tracking branches that were created due to
the now stale refspecs.  Other stale branches are not removed."
  (interactive (list (mercit-read-remote "Prune refspecs of remote")))
  (let* ((tracking-refs (mercit-list-remote-branches remote))
         (remote-refs (mercit-remote-list-refs remote))
         (variable (format "remote.%s.fetch" remote))
         (refspecs (mercit-get-all variable))
         stale)
    (dolist (refspec refspecs)
      (when (string-match mercit--refspec-re refspec)
        (let ((theirs (match-string 2 refspec))
              (ours   (match-string 3 refspec)))
          (unless (if (string-match "\\*" theirs)
                      (let ((re (replace-match ".*" t t theirs)))
                        (--some (string-match-p re it) remote-refs))
                    (member theirs remote-refs))
            (push (cons refspec
                        (if (string-match "\\*" ours)
                            (let ((re (replace-match ".*" t t ours)))
                              (--filter (string-match-p re it) tracking-refs))
                          (list (car (member ours tracking-refs)))))
                  stale)))))
    (if (not stale)
        (message "No stale refspecs for remote %S" remote)
      (if (= (length stale)
             (length refspecs))
          (mercit-read-char-case
              (format "All of %s's refspecs are stale.  " remote) nil
            (?s "replace with [d]efault refspec"
                (mercit-set-all
                 (list (format "+refs/heads/*:refs/remotes/%s/*" remote))
                 variable))
            (?r "[r]emove remote"
                (mercit-call-git "remote" "rm" remote))
            (?a "or [a]abort"
                (user-error "Abort")))
        (if (if (length= stale 1)
                (pcase-let ((`(,refspec . ,refs) (car stale)))
                  (mercit-confirm 'prune-stale-refspecs
                    (format "Prune stale refspec %s and branch %%s" refspec)
                    (format "Prune stale refspec %s and %%i branches" refspec)
                    nil refs))
              (mercit-confirm 'prune-stale-refspecs nil
                (format "Prune %%i stale refspecs and %i branches"
                        (length (cl-mapcan (lambda (s) (copy-sequence (cdr s)))
                                           stale)))
                nil
                (mapcar (pcase-lambda (`(,refspec . ,refs))
                          (concat refspec "\n"
                                  (mapconcat (lambda (b) (concat "  " b))
                                             refs "\n")))
                        stale)))
            (pcase-dolist (`(,refspec . ,refs) stale)
              (mercit-call-git "config" "--unset" variable
                              (regexp-quote refspec))
              (mercit--log-action
               (lambda (refs)
                 (format "Deleting %i branches" (length refs)))
               (lambda (ref)
                 (format "Deleting branch %s (was %s)" ref
                         (mercit-rev-parse "--short" ref)))
               refs)
              (dolist (ref refs)
                (mercit-call-git "update-ref" "-d" ref)))
          (user-error "Abort")))
      (mercit-refresh))))

;;;###autoload
(defun mercit-remote-set-head (remote &optional branch)
  "Set the local representation of REMOTE's default branch.
Query REMOTE and set the symbolic-ref refs/remotes/<remote>/HEAD
accordingly.  With a prefix argument query for the branch to be
used, which allows you to select an incorrect value if you fancy
doing that."
  (interactive
   (let  ((remote (mercit-read-remote "Set HEAD for remote")))
     (list remote
           (and current-prefix-arg
                (mercit-read-remote-branch (format "Set %s/HEAD to" remote)
                                          remote nil nil t)))))
  (mercit-run-git "remote" "set-head" remote (or branch "--auto")))

;;;###autoload
(defun mercit-remote-unset-head (remote)
  "Unset the local representation of REMOTE's default branch.
Delete the symbolic-ref \"refs/remotes/<remote>/HEAD\"."
  (interactive (list (mercit-read-remote "Unset HEAD for remote")))
  (mercit-run-git "remote" "set-head" remote "--delete"))

;;;###autoload
(defun mercit-remote-unshallow (remote)
  "Convert a shallow remote into a full one.
If only a single refspec is set and it does not contain a
wildcard, then also offer to replace it with the standard
refspec."
  (interactive (list (or (mercit-get-current-remote)
                         (mercit-read-remote "Delete remote"))))
  (let ((refspecs (mercit-get-all "remote" remote "fetch"))
        (standard (format "+refs/heads/*:refs/remotes/%s/*" remote)))
    (when (and (length= refspecs 1)
               (not (string-search "*" (car refspecs)))
               (yes-or-no-p (format "Also replace refspec %s with %s? "
                                    (car refspecs)
                                    standard)))
      (mercit-set standard "remote" remote "fetch"))
    (mercit-git-fetch "--unshallow" remote)))

;;; Configure

;;;###autoload (autoload 'mercit-remote-configure "mercit-remote" nil t)
(transient-define-prefix mercit-remote-configure (remote)
  "Configure a remote."
  :man-page "git-remote"
  [:description
   (lambda ()
     (concat
      (propertize "Configure " 'face 'transient-heading)
      (propertize (oref transient--prefix scope) 'face 'mercit-branch-remote)))
   ("u" mercit-remote.<remote>.url)
   ("U" mercit-remote.<remote>.fetch)
   ("s" mercit-remote.<remote>.pushurl)
   ("S" mercit-remote.<remote>.push)
   ("O" mercit-remote.<remote>.tagopt)]
  (interactive
   (list (or (and (not current-prefix-arg)
                  (not (and mercit-remote-direct-configure
                            (eq transient-current-command 'mercit-remote)))
                  (mercit-get-current-remote))
             (mercit--read-remote-scope))))
  (transient-setup 'mercit-remote-configure nil nil :scope remote))

(defun mercit--read-remote-scope (&optional obj)
  (mercit-read-remote
   (if obj
       (format "Set %s for remote"
               (format (oref obj variable) "<name>"))
     "Configure remote")))

(transient-define-infix mercit-remote.<remote>.url ()
  :class 'mercit--git-variable:urls
  :scope #'mercit--read-remote-scope
  :variable "remote.%s.url"
  :multi-value t
  :history-key 'mercit-remote.<remote>.*url)

(transient-define-infix mercit-remote.<remote>.fetch ()
  :class 'mercit--git-variable
  :scope #'mercit--read-remote-scope
  :variable "remote.%s.fetch"
  :multi-value t)

(transient-define-infix mercit-remote.<remote>.pushurl ()
  :class 'mercit--git-variable:urls
  :scope #'mercit--read-remote-scope
  :variable "remote.%s.pushurl"
  :multi-value t
  :history-key 'mercit-remote.<remote>.*url
  :seturl-arg "--push")

(transient-define-infix mercit-remote.<remote>.push ()
  :class 'mercit--git-variable
  :scope #'mercit--read-remote-scope
  :variable "remote.%s.push")

(transient-define-infix mercit-remote.<remote>.tagopt ()
  :class 'mercit--git-variable:choices
  :scope #'mercit--read-remote-scope
  :variable "remote.%s.tagOpt"
  :choices '("--no-tags" "--tags"))

;;; Transfer Utilities

(defun mercit--push-remote-variable (&optional branch short)
  (unless branch
    (setq branch (mercit-get-current-branch)))
  (mercit--propertize-face
   (if (or (not branch) mercit-prefer-push-default)
       (if short "pushDefault" "remote.pushDefault")
     (if short "pushRemote" (format "branch.%s.pushRemote" branch)))
   'bold))

(defun mercit--select-push-remote (prompt-suffix)
  (let* ((branch (or (mercit-get-current-branch)
                     (user-error "No branch is checked out")))
         (remote (mercit-get-push-remote branch))
         (changed nil))
    (when (or current-prefix-arg
              (not remote)
              (not (member remote (mercit-list-remotes))))
      (setq changed t)
      (setq remote
            (mercit-read-remote (format "Set %s and %s"
                                       (mercit--push-remote-variable)
                                       prompt-suffix)))
      (setf (mercit-get (mercit--push-remote-variable branch)) remote))
    (list branch remote changed)))

;;; _
(provide 'mercit-remote)
;;; mercit-remote.el ends here
