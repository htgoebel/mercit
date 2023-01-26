;;; mercit-fetch.el --- Download objects and refs  -*- lexical-binding:t -*-

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

;; This library implements fetch commands.

;;; Code:

(require 'mercit)

(defvar mercit-fetch-modules-jobs nil)
(make-obsolete-variable
 'mercit-fetch-modules-jobs
 "invoke `mercit-fetch-modules' with a prefix argument instead."
 "Mercit 3.0.0")

;;; Commands

;;;###autoload (autoload 'mercit-fetch "mercit-fetch" nil t)
(transient-define-prefix mercit-fetch ()
  "Fetch from another repository."
  :man-page "git-fetch"
  ["Arguments"
   ("-p" "Prune deleted branches" ("-p" "--prune"))
   ("-t" "Fetch all tags" ("-t" "--tags"))
   (7 "-u" "Fetch full history" "--unshallow")]
  ["Fetch from"
   ("p" mercit-fetch-from-pushremote)
   ("u" mercit-fetch-from-upstream)
   ("e" "elsewhere"        mercit-fetch-other)
   ("a" "all remotes"      mercit-fetch-all)]
  ["Fetch"
   ("o" "another branch"   mercit-fetch-branch)
   ("r" "explicit refspec" mercit-fetch-refspec)
   ("m" "submodules"       mercit-fetch-modules)]
  ["Configure"
   ("C" "variables..." mercit-branch-configure)])

(defun mercit-fetch-arguments ()
  (transient-args 'mercit-fetch))

(defun mercit-git-fetch (remote args)
  (run-hooks 'mercit-credential-hook)
  (mercit-run-git-async "fetch" remote args))

;;;###autoload (autoload 'mercit-fetch-from-pushremote "mercit-fetch" nil t)
(transient-define-suffix mercit-fetch-from-pushremote (args)
  "Fetch from the current push-remote.

With a prefix argument or when the push-remote is either not
configured or unusable, then let the user first configure the
push-remote."
  :description #'mercit-fetch--pushremote-description
  (interactive (list (mercit-fetch-arguments)))
  (let ((remote (mercit-get-push-remote)))
    (when (or current-prefix-arg
              (not (member remote (mercit-list-remotes))))
      (let ((var (mercit--push-remote-variable)))
        (setq remote
              (mercit-read-remote (format "Set %s and fetch from there" var)))
        (mercit-set remote var)))
    (mercit-git-fetch remote args)))

(defun mercit-fetch--pushremote-description ()
  (let* ((branch (mercit-get-current-branch))
         (remote (mercit-get-push-remote branch))
         (v (mercit--push-remote-variable branch t)))
    (cond
     ((member remote (mercit-list-remotes)) remote)
     (remote
      (format "%s, replacing invalid" v))
     (t
      (format "%s, setting that" v)))))

;;;###autoload (autoload 'mercit-fetch-from-upstream "mercit-fetch" nil t)
(transient-define-suffix mercit-fetch-from-upstream (remote args)
  "Fetch from the \"current\" remote, usually the upstream.

If the upstream is configured for the current branch and names
an existing remote, then use that.  Otherwise try to use another
remote: If only a single remote is configured, then use that.
Otherwise if a remote named \"origin\" exists, then use that.

If no remote can be determined, then this command is not available
from the `mercit-fetch' transient prefix and invoking it directly
results in an error."
  :if          (lambda () (mercit-get-current-remote t))
  :description (lambda () (mercit-get-current-remote t))
  (interactive (list (mercit-get-current-remote t)
                     (mercit-fetch-arguments)))
  (unless remote
    (error "The \"current\" remote could not be determined"))
  (mercit-git-fetch remote args))

;;;###autoload
(defun mercit-fetch-other (remote args)
  "Fetch from another repository."
  (interactive (list (mercit-read-remote "Fetch remote")
                     (mercit-fetch-arguments)))
  (mercit-git-fetch remote args))

;;;###autoload
(defun mercit-fetch-branch (remote branch args)
  "Fetch a BRANCH from a REMOTE."
  (interactive
   (let ((remote (mercit-read-remote-or-url "Fetch from remote or url")))
     (list remote
           (mercit-read-remote-branch "Fetch branch" remote)
           (mercit-fetch-arguments))))
  (mercit-git-fetch remote (cons branch args)))

;;;###autoload
(defun mercit-fetch-refspec (remote refspec args)
  "Fetch a REFSPEC from a REMOTE."
  (interactive
   (let ((remote (mercit-read-remote-or-url "Fetch from remote or url")))
     (list remote
           (mercit-read-refspec "Fetch using refspec" remote)
           (mercit-fetch-arguments))))
  (mercit-git-fetch remote (cons refspec args)))

;;;###autoload
(defun mercit-fetch-all (args)
  "Fetch from all remotes."
  (interactive (list (mercit-fetch-arguments)))
  (mercit-git-fetch nil (cons "--all" args)))

;;;###autoload
(defun mercit-fetch-all-prune ()
  "Fetch from all remotes, and prune.
Prune remote tracking branches for branches that have been
removed on the respective remote."
  (interactive)
  (run-hooks 'mercit-credential-hook)
  (mercit-run-git-async "remote" "update" "--prune"))

;;;###autoload
(defun mercit-fetch-all-no-prune ()
  "Fetch from all remotes."
  (interactive)
  (run-hooks 'mercit-credential-hook)
  (mercit-run-git-async "remote" "update"))

;;;###autoload (autoload 'mercit-fetch-modules "mercit-fetch" nil t)
(transient-define-prefix mercit-fetch-modules (&optional transient args)
  "Fetch all submodules.

Fetching is done using \"git fetch --recurse-submodules\", which
means that the super-repository and recursively all submodules
are also fetched.

To set and potentially save other arguments invoke this command
with a prefix argument."
  :man-page "git-fetch"
  :value (list "--verbose"
               (cond (mercit-fetch-modules-jobs
                      (format "--jobs=%s" mercit-fetch-modules-jobs))
                     (t "--jobs=4")))
  ["Arguments"
   ("-v" "verbose"        "--verbose")
   ("-j" "number of jobs" "--jobs=" :reader transient-read-number-N+)]
  ["Action"
   ("m" "fetch modules" mercit-fetch-modules)]
  (interactive (if current-prefix-arg
                   (list t)
                 (list nil (transient-args 'mercit-fetch-modules))))
  (if transient
      (transient-setup 'mercit-fetch-modules)
    (when (mercit-git-version< "2.8.0")
      (when-let ((value (transient-arg-value "--jobs=" args)))
        (message "Dropping --jobs; not supported by Git v%s"
                 (mercit-git-version))
        (setq args (remove (format "--jobs=%s" value) args))))
    (mercit-with-toplevel
      (mercit-run-git-async "fetch" "--recurse-submodules" args))))

;;; _
(provide 'mercit-fetch)
;;; mercit-fetch.el ends here
