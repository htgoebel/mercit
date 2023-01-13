;;; mercit-push.el --- Update remote objects and refs  -*- lexical-binding:t -*-

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

;; This library implements push commands.

;;; Code:

(require 'mercit)

;;; Commands

;;;###autoload (autoload 'mercit-push "mercit-push" nil t)
(transient-define-prefix mercit-push ()
  "Push to another repository."
  :man-page "git-push"
  ["Arguments"
   ("-f" "Force with lease" (nil "--force-with-lease"))
   ("-F" "Force"            ("-f" "--force"))
   ("-h" "Disable hooks"    "--no-verify")
   ("-n" "Dry run"          ("-n" "--dry-run"))
   (5 "-u" "Set upstream"   "--set-upstream")
   (7 "-t" "Follow tags"    "--follow-tags")]
  [:if mercit-get-current-branch
   :description (lambda ()
                  (format (propertize "Push %s to" 'face 'transient-heading)
                          (propertize (mercit-get-current-branch)
                                      'face 'mercit-branch-local)))
   ("p" mercit-push-current-to-pushremote)
   ("u" mercit-push-current-to-upstream)
   ("e" "elsewhere" mercit-push-current)]
  ["Push"
   [("o" "another branch"    mercit-push-other)
    ("r" "explicit refspecs" mercit-push-refspecs)
    ("m" "matching branches" mercit-push-matching)]
   [("T" "a tag"             mercit-push-tag)
    ("t" "all tags"          mercit-push-tags)
    (6 "n" "a note ref"      mercit-push-notes-ref)]]
  ["Configure"
   ("C" "Set variables..."  mercit-branch-configure)])

(defun mercit-push-arguments ()
  (transient-args 'mercit-push))

(defun mercit-git-push (branch target args)
  (run-hooks 'mercit-credential-hook)
  ;; If the remote branch already exists, then we do not have to
  ;; qualify the target, which we prefer to avoid doing because
  ;; using the default namespace is wrong in obscure cases.
  (pcase-let ((namespace (if (mercit-get-tracked target) "" "refs/heads/"))
              (`(,remote . ,target)
               (mercit-split-branch-name target)))
    (mercit-run-git-async "push" "-v" args remote
                         (format "%s:%s%s" branch namespace target))))

;;;###autoload (autoload 'mercit-push-current-to-pushremote "mercit-push" nil t)
(transient-define-suffix mercit-push-current-to-pushremote (args)
  "Push the current branch to its push-remote.

When the push-remote is not configured, then read the push-remote
from the user, set it, and then push to it.  With a prefix
argument the push-remote can be changed before pushed to it."
  :if #'mercit-get-current-branch
  :description #'mercit-push--pushbranch-description
  (interactive (list (mercit-push-arguments)))
  (pcase-let ((`(,branch ,remote ,changed)
               (mercit--select-push-remote "push there")))
    (when changed
      (mercit-confirm 'set-and-push
        (string-replace
         "%" "%%"
         (format "Really use \"%s\" as push-remote and push \"%s\" there"
                 remote branch))))
    (run-hooks 'mercit-credential-hook)
    (mercit-run-git-async "push" "-v" args remote
                         (format "refs/heads/%s:refs/heads/%s"
                                 branch branch)))) ; see #3847 and #3872

(defun mercit-push--pushbranch-description ()
  (let* ((branch (mercit-get-current-branch))
         (target (mercit-get-push-branch branch t))
         (remote (mercit-get-push-remote branch))
         (v (mercit--push-remote-variable branch t)))
    (cond
     (target)
     ((member remote (mercit-list-remotes))
      (format "%s, creating it"
              (mercit--propertize-face (concat remote "/" branch)
                                      'mercit-branch-remote)))
     (remote
      (format "%s, replacing invalid" v))
     (t
      (format "%s, setting that" v)))))

;;;###autoload (autoload 'mercit-push-current-to-upstream "mercit-push" nil t)
(transient-define-suffix mercit-push-current-to-upstream (args)
  "Push the current branch to its upstream branch.

With a prefix argument or when the upstream is either not
configured or unusable, then let the user first configure
the upstream."
  :if #'mercit-get-current-branch
  :description #'mercit-push--upstream-description
  (interactive (list (mercit-push-arguments)))
  (let* ((branch (or (mercit-get-current-branch)
                     (user-error "No branch is checked out")))
         (remote (mercit-get "branch" branch "remote"))
         (merge  (mercit-get "branch" branch "merge")))
    (when (or current-prefix-arg
              (not (or (mercit-get-upstream-branch branch)
                       (mercit--unnamed-upstream-p remote merge)
                       (mercit--valid-upstream-p remote merge))))
      (let* ((branches (-union (--map (concat it "/" branch)
                                      (mercit-list-remotes))
                               (mercit-list-remote-branch-names)))
             (upstream (mercit-completing-read
                        (format "Set upstream of %s and push there" branch)
                        branches nil nil nil 'mercit-revision-history
                        (or (car (member (mercit-remote-branch-at-point) branches))
                            (car (member "origin/master" branches)))))
             (upstream* (or (mercit-get-tracked upstream)
                            (mercit-split-branch-name upstream))))
        (setq remote (car upstream*))
        (setq merge  (cdr upstream*))
        (unless (string-prefix-p "refs/" merge)
          ;; User selected a non-existent remote-tracking branch.
          ;; It is very likely, but not certain, that this is the
          ;; correct thing to do.  It is even more likely that it
          ;; is what the user wants to happen.
          (setq merge (concat "refs/heads/" merge)))
        (mercit-confirm 'set-and-push
          (string-replace
           "%" "%%"
           (format "Really use \"%s\" as upstream and push \"%s\" there"
                   upstream branch))))
      (cl-pushnew "--set-upstream" args :test #'equal))
    (run-hooks 'mercit-credential-hook)
    (mercit-run-git-async "push" "-v" args remote (concat branch ":" merge))))

(defun mercit-push--upstream-description ()
  (and-let* ((branch (mercit-get-current-branch)))
    (or (mercit-get-upstream-branch branch)
        (let ((remote (mercit-get "branch" branch "remote"))
              (merge  (mercit-get "branch" branch "merge"))
              (u (mercit--propertize-face "@{upstream}" 'bold)))
          (cond
           ((mercit--unnamed-upstream-p remote merge)
            (format "%s as %s"
                    (mercit--propertize-face remote 'bold)
                    (mercit--propertize-face merge 'mercit-branch-remote)))
           ((mercit--valid-upstream-p remote merge)
            (format "%s creating %s"
                    (mercit--propertize-face remote 'mercit-branch-remote)
                    (mercit--propertize-face merge 'mercit-branch-remote)))
           ((or remote merge)
            (concat u ", creating it and replacing invalid"))
           (t
            (concat u ", creating it")))))))

;;;###autoload
(defun mercit-push-current (target args)
  "Push the current branch to a branch read in the minibuffer."
  (interactive
   (--if-let (mercit-get-current-branch)
       (list (mercit-read-remote-branch (format "Push %s to" it)
                                       nil nil it 'confirm)
             (mercit-push-arguments))
     (user-error "No branch is checked out")))
  (mercit-git-push (mercit-get-current-branch) target args))

;;;###autoload
(defun mercit-push-other (source target args)
  "Push an arbitrary branch or commit somewhere.
Both the source and the target are read in the minibuffer."
  (interactive
   (let ((source (mercit-read-local-branch-or-commit "Push")))
     (list source
           (mercit-read-remote-branch
            (format "Push %s to" source) nil
            (if (mercit-local-branch-p source)
                (or (mercit-get-push-branch source)
                    (mercit-get-upstream-branch source))
              (and (mercit-rev-ancestor-p source "HEAD")
                   (or (mercit-get-push-branch)
                       (mercit-get-upstream-branch))))
            source 'confirm)
           (mercit-push-arguments))))
  (mercit-git-push source target args))

(defvar mercit-push-refspecs-history nil)

;;;###autoload
(defun mercit-push-refspecs (remote refspecs args)
  "Push one or multiple REFSPECS to a REMOTE.
Both the REMOTE and the REFSPECS are read in the minibuffer.  To
use multiple REFSPECS, separate them with commas.  Completion is
only available for the part before the colon, or when no colon
is used."
  (interactive
   (list (mercit-read-remote "Push to remote")
         (mercit-completing-read-multiple*
          "Push refspec,s: "
          (cons "HEAD" (mercit-list-local-branch-names))
          nil nil nil 'mercit-push-refspecs-history)
         (mercit-push-arguments)))
  (run-hooks 'mercit-credential-hook)
  (mercit-run-git-async "push" "-v" args remote refspecs))

;;;###autoload
(defun mercit-push-matching (remote &optional args)
  "Push all matching branches to another repository.
If multiple remotes exist, then read one from the user.
If just one exists, use that without requiring confirmation."
  (interactive (list (mercit-read-remote "Push matching branches to" nil t)
                     (mercit-push-arguments)))
  (run-hooks 'mercit-credential-hook)
  (mercit-run-git-async "push" "-v" args remote ":"))

;;;###autoload
(defun mercit-push-tags (remote &optional args)
  "Push all tags to another repository.
If only one remote exists, then push to that.  Otherwise prompt
for a remote, offering the remote configured for the current
branch as default."
  (interactive (list (mercit-read-remote "Push tags to remote" nil t)
                     (mercit-push-arguments)))
  (run-hooks 'mercit-credential-hook)
  (mercit-run-git-async "push" remote "--tags" args))

;;;###autoload
(defun mercit-push-tag (tag remote &optional args)
  "Push a tag to another repository."
  (interactive
   (let  ((tag (mercit-read-tag "Push tag")))
     (list tag (mercit-read-remote (format "Push %s to remote" tag) nil t)
           (mercit-push-arguments))))
  (run-hooks 'mercit-credential-hook)
  (mercit-run-git-async "push" remote tag args))

;;;###autoload
(defun mercit-push-notes-ref (ref remote &optional args)
  "Push a notes ref to another repository."
  (interactive
   (let ((note (mercit-notes-read-ref "Push notes" nil nil)))
     (list note
           (mercit-read-remote (format "Push %s to remote" note) nil t)
           (mercit-push-arguments))))
  (run-hooks 'mercit-credential-hook)
  (mercit-run-git-async "push" remote ref args))

;;;###autoload (autoload 'mercit-push-implicitly "mercit-push" nil t)
(transient-define-suffix mercit-push-implicitly (args)
  "Push somewhere without using an explicit refspec.

This command simply runs \"git push -v [ARGS]\".  ARGS are the
arguments specified in the popup buffer.  No explicit refspec
arguments are used.  Instead the behavior depends on at least
these Git variables: `push.default', `remote.pushDefault',
`branch.<branch>.pushRemote', `branch.<branch>.remote',
`branch.<branch>.merge', and `remote.<remote>.push'.

If you add this suffix to a transient prefix without explicitly
specifying the description, then an attempt is made to predict
what this command will do.  To add it use something like:

  (transient-insert-suffix \\='mercit-push \"o\"
    \\='(\"i\" mercit-push-implicitly))"
  :description #'mercit-push-implicitly--desc
  (interactive (list (mercit-push-arguments)))
  (run-hooks 'mercit-credential-hook)
  (mercit-run-git-async "push" "-v" args))

(defun mercit-push-implicitly--desc ()
  ;; This implements the logic for git push as documented.
  ;; First, we resolve a remote to use based on various remote and
  ;; pushRemote options.
  ;; Then, we resolve the refspec to use for the remote based on push
  ;; and pushDefault options.
  ;; Note that the remote and refspec to push are handled separately,
  ;; so it doesn't make sense to talk about "pushing to upstream".
  ;; Depending on the options, you could end up pushing to the
  ;; "upstream" remote but not the "upstream" branch, and vice versa.
  (let* ((branch (mercit-get-current-branch))
         (remote (or (mercit-get-push-remote branch)
                     ;; Note: Avoid `mercit-get-remote' because it
                     ;; filters out the local repo case (".").
                     (mercit-get "branch" branch "remote")
                     (let ((remotes (mercit-list-remotes)))
                       (cond
                        ((and (mercit-git-version>= "2.27")
                              (= (length remotes) 1))
                         (car remotes))
                        ((member "origin" remotes) "origin"))))))
    (if (null remote)
        "nothing (no remote)"
      (let ((refspec (mercit-get "remote" remote "push")))
        (if refspec
            (format "to %s with refspecs %s"
                    (mercit--propertize-face remote 'bold)
                    (mercit--propertize-face refspec 'bold))
          (pcase (or (mercit-get "push.default") "simple")
            ("nothing" "nothing (due to push.default)")
            ((or "current" "simple")
             (format "%s to %s"
                     (mercit--propertize-face branch 'mercit-branch-current)
                     (mercit--propertize-face (format "%s/%s" remote branch)
                                             'mercit-branch-remote)))
            ((or "upstream" "tracking")
             (let ((ref (mercit-get "branch" branch "merge")))
               (if ref
                   (format "%s to %s"
                           (mercit--propertize-face branch 'mercit-branch-current)
                           (cond
                            ((string-prefix-p "refs/heads/" ref)
                             (mercit--propertize-face
                              (format "%s/%s" remote
                                      (substring ref (length "refs/heads/")))
                              'mercit-branch-remote))
                            ((not (string-match "/" ref))
                             (mercit--propertize-face (format "%s/%s" remote ref)
                                                     'mercit-branch-remote))
                            (t (format "%s as %s"
                                       (mercit--propertize-face remote 'bold)
                                       (mercit--propertize-face ref 'bold)))))
                 "nothing (no upstream)")))
            ("matching" (format "all matching to %s"
                                (mercit--propertize-face remote 'bold)))))))))

;;;###autoload (autoload 'mercit-push-to-remote "mercit-push" nil t)
(transient-define-suffix mercit-push-to-remote (remote args)
  "Push to REMOTE without using an explicit refspec.
The REMOTE is read in the minibuffer.

This command simply runs \"git push -v [ARGS] REMOTE\".  ARGS
are the arguments specified in the popup buffer.  No refspec
arguments are used.  Instead the behavior depends on at least
these Git variables: `push.default', `remote.pushDefault',
`branch.<branch>.pushRemote', `branch.<branch>.remote',
`branch.<branch>.merge', and `remote.<remote>.push'.

You can add this command as a suffix using something like:

  (transient-insert-suffix \\='mercit-push \"o\"
    \\='(\"x\" mercit-push-to-remote))"
  :description #'mercit-push-to-remote--desc
  (interactive (list (mercit-read-remote "Push to remote")
                     (mercit-push-arguments)))
  (run-hooks 'mercit-credential-hook)
  (mercit-run-git-async "push" "-v" args remote))

(defun mercit-push-to-remote--desc ()
  (format "using %s" (mercit--propertize-face "git push <remote>" 'bold)))

;;; _
(provide 'mercit-push)
;;; mercit-push.el ends here
