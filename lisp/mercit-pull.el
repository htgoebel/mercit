;;; mercit-pull.el --- Update local objects and refs  -*- lexical-binding:t -*-

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

;; This library implements pull commands.

;;; Code:

(require 'mercit)

;;; Options

(defcustom mercit-pull-or-fetch nil
  "Whether `mercit-pull' also offers some fetch suffixes."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-commands
  :type 'boolean)

;;; Commands

;;;###autoload (autoload 'mercit-pull "mercit-pull" nil t)
(transient-define-prefix mercit-pull ()
  "Pull from another repository."
  :man-page "git-pull"
  :incompatible '(("--ff-only" "--rebase"))
  [:description
   (lambda () (if mercit-pull-or-fetch "Pull arguments" "Arguments"))
   ("-f" "Fast-forward only" "--ff-only")
   ("-r" "Rebase local commits" ("-r" "--rebase"))
   ("-A" "Autostash" "--autostash" :level 7)]
  [:description
   (lambda ()
     (if-let ((branch (mercit-get-current-branch)))
         (concat
          (propertize "Pull into " 'face 'transient-heading)
          (propertize branch       'face 'mercit-branch-local)
          (propertize " from"      'face 'transient-heading))
       (propertize "Pull from" 'face 'transient-heading)))
   ("p" mercit-pull-from-pushremote)
   ("u" mercit-pull-from-upstream)
   ("e" "elsewhere"         mercit-pull-branch)]
  ["Fetch from"
   :if-non-nil mercit-pull-or-fetch
   ("f" "remotes"           mercit-fetch-all-no-prune)
   ("F" "remotes and prune" mercit-fetch-all-prune)]
  ["Fetch"
   :if-non-nil mercit-pull-or-fetch
   ("o" "another branch"    mercit-fetch-branch)
   ("s" "explicit refspec"  mercit-fetch-refspec)
   ("m" "submodules"        mercit-fetch-modules)]
  ["Configure"
   ("r" mercit-branch.<branch>.rebase :if mercit-get-current-branch)
   ("C" "variables..." mercit-branch-configure)]
  (interactive)
  (transient-setup 'mercit-pull nil nil :scope (mercit-get-current-branch)))

(defun mercit-pull-arguments ()
  (transient-args 'mercit-pull))

;;;###autoload (autoload 'mercit-pull-from-pushremote "mercit-pull" nil t)
(transient-define-suffix mercit-pull-from-pushremote (args)
  "Pull from the push-remote of the current branch.

With a prefix argument or when the push-remote is either not
configured or unusable, then let the user first configure the
push-remote."
  :if #'mercit-get-current-branch
  :description #'mercit-pull--pushbranch-description
  (interactive (list (mercit-pull-arguments)))
  (pcase-let ((`(,branch ,remote)
               (mercit--select-push-remote "pull from there")))
    (run-hooks 'mercit-credential-hook)
    (mercit-run-git-with-editor "pull" args remote branch)))

(defun mercit-pull--pushbranch-description ()
  ;; Also used by `mercit-rebase-onto-pushremote'.
  (let* ((branch (mercit-get-current-branch))
         (target (mercit-get-push-branch branch t))
         (remote (mercit-get-push-remote branch))
         (v (mercit--push-remote-variable branch t)))
    (cond
     (target)
     ((member remote (mercit-list-remotes))
      (format "%s, replacing non-existent" v))
     (remote
      (format "%s, replacing invalid" v))
     (t
      (format "%s, setting that" v)))))

;;;###autoload (autoload 'mercit-pull-from-upstream "mercit-pull" nil t)
(transient-define-suffix mercit-pull-from-upstream (args)
  "Pull from the upstream of the current branch.

With a prefix argument or when the upstream is either not
configured or unusable, then let the user first configure
the upstream."
  :if #'mercit-get-current-branch
  :description #'mercit-pull--upstream-description
  (interactive (list (mercit-pull-arguments)))
  (let* ((branch (or (mercit-get-current-branch)
                     (user-error "No branch is checked out")))
         (remote (mercit-get "branch" branch "remote"))
         (merge  (mercit-get "branch" branch "merge")))
    (when (or current-prefix-arg
              (not (or (mercit-get-upstream-branch branch)
                       (mercit--unnamed-upstream-p remote merge))))
      (mercit-set-upstream-branch
       branch (mercit-read-upstream-branch
               branch (format "Set upstream of %s and pull from there" branch)))
      (setq remote (mercit-get "branch" branch "remote"))
      (setq merge  (mercit-get "branch" branch "merge")))
    (run-hooks 'mercit-credential-hook)
    (mercit-run-git-with-editor "pull" args remote merge)))

(defun mercit-pull--upstream-description ()
  (and-let* ((branch (mercit-get-current-branch)))
    (or (mercit-get-upstream-branch branch)
        (let ((remote (mercit-get "branch" branch "remote"))
              (merge  (mercit-get "branch" branch "merge"))
              (u (mercit--propertize-face "@{upstream}" 'bold)))
          (cond
           ((mercit--unnamed-upstream-p remote merge)
            (format "%s of %s"
                    (mercit--propertize-face merge 'mercit-branch-remote)
                    (mercit--propertize-face remote 'bold)))
           ((mercit--valid-upstream-p remote merge)
            (concat u ", replacing non-existent"))
           ((or remote merge)
            (concat u ", replacing invalid"))
           (t
            (concat u ", setting that")))))))

;;;###autoload
(defun mercit-pull-branch (source args)
  "Pull from a branch read in the minibuffer."
  (interactive (list (mercit-read-remote-branch "Pull" nil nil nil t)
                     (mercit-pull-arguments)))
  (run-hooks 'mercit-credential-hook)
  (pcase-let ((`(,remote . ,branch)
               (mercit-get-tracked source)))
    (mercit-run-git-with-editor "pull" args remote branch)))

;;; _
(provide 'mercit-pull)
;;; mercit-pull.el ends here
