;;; mercit-subtree.el --- Subtree support for Mercit  -*- lexical-binding:t -*-

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

;;; Code:

(require 'mercit)

;;; Commands

;;;###autoload (autoload 'mercit-subtree "mercit-subtree" nil t)
(transient-define-prefix mercit-subtree ()
  "Import or export subtrees."
  :man-page "git-subtree"
  ["Actions"
   ("i" "*Import" mercit-subtree-import)
   ("e" "*Export" mercit-subtree-export)])

;;;###autoload (autoload 'mercit-subtree-import "mercit-subtree" nil t)
(transient-define-prefix mercit-subtree-import ()
  "Import subtrees."
  :man-page "git-subtree"
  ["Arguments"
   (mercit-subtree:--prefix)
   (mercit-subtree:--message)
   ("-s" "*Squash" "--squash")]
  ["Actions"
   [("a" "*Add"        mercit-subtree-add)
    ("c" "*Add commit" mercit-subtree-add-commit)]
   [("m" "*Merge"      mercit-subtree-merge)
    ("f" "*Pull"       mercit-subtree-pull)]])

;;;###autoload (autoload 'mercit-subtree-export "mercit-subtree" nil t)
(transient-define-prefix mercit-subtree-export ()
  "Export subtrees."
  :man-page "git-subtree"
  ["Arguments"
   (mercit-subtree:--prefix)
   (mercit-subtree:--annotate)
   (mercit-subtree:--branch)
   (mercit-subtree:--onto)
   ("-i" "*Ignore joins" "--ignore-joins")
   ("-j" "*Rejoin"       "--rejoin")]
  ["Actions"
   ("p" "*Push"          mercit-subtree-push)
   ("s" "*Split"         mercit-subtree-split)])

(transient-define-argument mercit-subtree:--prefix ()
  :description "*Prefix"
  :class 'transient-option
  :shortarg "-P"
  :argument "--prefix="
  :reader #'mercit-subtree-read-prefix)

(defun mercit-subtree-read-prefix (prompt &optional default _history)
  (let* ((insert-default-directory nil)
         (topdir (mercit-toplevel))
         (prefix (read-directory-name (concat prompt ": ") topdir default)))
    (if (file-name-absolute-p prefix)
        ;; At least `ido-mode's variant is not compatible.
        (if (string-prefix-p topdir prefix)
            (file-relative-name prefix topdir)
          (user-error "%s isn't inside the repository at %s" prefix topdir))
      prefix)))

(transient-define-argument mercit-subtree:--message ()
  :description "*Message"
  :class 'transient-option
  :shortarg "-m"
  :argument "--message=")

(transient-define-argument mercit-subtree:--annotate ()
  :description "*Annotate"
  :class 'transient-option
  :key "-a"
  :argument "--annotate=")

(transient-define-argument mercit-subtree:--branch ()
  :description "*Branch"
  :class 'transient-option
  :shortarg "-b"
  :argument "--branch=")

(transient-define-argument mercit-subtree:--onto ()
  :description "*Onto"
  :class 'transient-option
  :key "-o"
  :argument "--onto="
  :reader #'mercit-transient-read-revision)

(defun mercit-subtree-prefix (transient prompt)
  (--if-let (--first (string-prefix-p "--prefix=" it)
                     (transient-args transient))
      (substring it 9)
    (mercit-subtree-read-prefix prompt)))

(defun mercit-subtree-arguments (transient)
  (--remove (string-prefix-p "--prefix=" it)
            (transient-args transient)))

(defun mercit-git-subtree (subcmd prefix &rest args)
  (mercit-run-git-async "subtree" subcmd (concat "--prefix=" prefix) args))

;;;###autoload
(defun mercit-subtree-add (prefix repository ref args)
  "Add REF from REPOSITORY as a new subtree at PREFIX."
  (interactive
   (cons (mercit-subtree-prefix 'mercit-subtree-import "Add subtree")
         (let ((remote (mercit-read-remote-or-url "From repository")))
           (list remote
                 (mercit-read-refspec "Ref" remote)
                 (mercit-subtree-arguments 'mercit-subtree-import)))))
  (mercit-git-subtree "add" prefix args repository ref))

;;;###autoload
(defun mercit-subtree-add-commit (prefix commit args)
  "Add COMMIT as a new subtree at PREFIX."
  (interactive
   (list (mercit-subtree-prefix 'mercit-subtree-import "Add subtree")
         (mercit-read-string-ns "Commit")
         (mercit-subtree-arguments 'mercit-subtree-import)))
  (mercit-git-subtree "add" prefix args commit))

;;;###autoload
(defun mercit-subtree-merge (prefix commit args)
  "Merge COMMIT into the PREFIX subtree."
  (interactive
   (list (mercit-subtree-prefix 'mercit-subtree-import "Merge into subtree")
         (mercit-read-string-ns "Commit")
         (mercit-subtree-arguments 'mercit-subtree-import)))
  (mercit-git-subtree "merge" prefix args commit))

;;;###autoload
(defun mercit-subtree-pull (prefix repository ref args)
  "Pull REF from REPOSITORY into the PREFIX subtree."
  (interactive
   (cons (mercit-subtree-prefix 'mercit-subtree-import "Pull into subtree")
         (let ((remote (mercit-read-remote-or-url "From repository")))
           (list remote
                 (mercit-read-refspec "Ref" remote)
                 (mercit-subtree-arguments 'mercit-subtree-import)))))
  (mercit-git-subtree "pull" prefix args repository ref))

;;;###autoload
(defun mercit-subtree-push (prefix repository ref args)
  "Extract the history of the subtree PREFIX and push it to REF on REPOSITORY."
  (interactive (list (mercit-subtree-prefix 'mercit-subtree-export "Push subtree")
                     (mercit-read-remote-or-url "To repository")
                     (mercit-read-string-ns "To reference")
                     (mercit-subtree-arguments 'mercit-subtree-export)))
  (mercit-git-subtree "push" prefix args repository ref))

;;;###autoload
(defun mercit-subtree-split (prefix commit args)
  "Extract the history of the subtree PREFIX."
  (interactive (list (mercit-subtree-prefix 'mercit-subtree-export "Split subtree")
                     (mercit-read-string-ns "Commit")
                     (mercit-subtree-arguments 'mercit-subtree-export)))
  (mercit-git-subtree "split" prefix args commit))

;;; _
(provide 'mercit-subtree)
;;; mercit-subtree.el ends here
