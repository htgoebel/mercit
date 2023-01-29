;;; mercit-notes.el --- Notes support  -*- lexical-binding:t -*-

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

;; This library implements support for `git-notes'.

;;; Code:

(require 'mercit)

;;; Commands

;;;###autoload (autoload 'mercit-notes "mercit" nil t)
(transient-define-prefix mercit-notes ()
  "Edit notes attached to commits."
  :man-page "git-notes"
  ["Configure local settings"
   ("c" mercit-core.notesRef)
   ("d" mercit-notes.displayRef)]
  ["Configure global settings"
   ("C" mercit-global-core.notesRef)
   ("D" mercit-global-notes.displayRef)]
  ["Arguments for prune"
   :if-not mercit-notes-merging-p
   ("-n" "*Dry run" ("-n" "--dry-run"))]
  ["Arguments for edit and remove"
   :if-not mercit-notes-merging-p
   (mercit-notes:--ref)]
  ["Arguments for merge"
   :if-not mercit-notes-merging-p
   (mercit-notes:--strategy)]
  ["Actions"
   :if-not mercit-notes-merging-p
   ("T" "*Edit"         mercit-notes-edit)
   ("r" "*Remove"       mercit-notes-remove)
   ("m" "*Merge"        mercit-notes-merge)
   ("p" "*Prune"        mercit-notes-prune)]
  ["Actions"
   :if mercit-notes-merging-p
   ("c" "*Commit merge" mercit-notes-merge-commit)
   ("a" "*Abort merge"  mercit-notes-merge-abort)])

(defun mercit-notes-merging-p ()
  (let ((dir (mercit-git-dir "NOTES_MERGE_WORKTREE")))
    (and (file-directory-p dir)
         (directory-files dir nil "^[^.]"))))

(transient-define-infix mercit-core.notesRef ()
  :class 'mercit--git-variable
  :variable "*core.notesRef"
  :reader #'mercit-notes-read-ref
  :prompt "Set local core.notesRef")

(transient-define-infix mercit-notes.displayRef ()
  :class 'mercit--git-variable
  :variable "*notes.displayRef"
  :multi-value t
  :reader #'mercit-notes-read-refs
  :prompt "Set local notes.displayRef")

(transient-define-infix mercit-global-core.notesRef ()
  :class 'mercit--git-variable
  :variable "*core.notesRef"
  :global t
  :reader #'mercit-notes-read-ref
  :prompt "Set global core.notesRef")

(transient-define-infix mercit-global-notes.displayRef ()
  :class 'mercit--git-variable
  :variable "*notes.displayRef"
  :global t
  :multi-value t
  :reader #'mercit-notes-read-refs
  :prompt "Set global notes.displayRef")

(transient-define-argument mercit-notes:--ref ()
  :description "*Manipulate ref"
  :class 'transient-option
  :key "-r"
  :argument "--ref="
  :reader #'mercit-notes-read-ref)

(transient-define-argument mercit-notes:--strategy ()
  :description "*Merge strategy"
  :class 'transient-option
  :shortarg "-s"
  :argument "--strategy="
  :choices '("manual" "ours" "theirs" "union" "cat_sort_uniq"))

(defun mercit-notes-edit (commit &optional ref)
  "Edit the note attached to COMMIT.
REF is the notes ref used to store the notes.

Interactively or when optional REF is nil use the value of Git
variable `core.notesRef' or \"refs/notes/commits\" if that is
undefined."
  (interactive (mercit-notes-read-args "Edit notes"))
  (mercit-run-git-with-editor "notes" (and ref (concat "--ref=" ref))
                             "edit" commit))

(defun mercit-notes-remove (commit &optional ref)
  "Remove the note attached to COMMIT.
REF is the notes ref from which the note is removed.

Interactively or when optional REF is nil use the value of Git
variable `core.notesRef' or \"refs/notes/commits\" if that is
undefined."
  (interactive (mercit-notes-read-args "Remove notes"))
  (mercit-run-git-with-editor "notes" (and ref (concat "--ref=" ref))
                             "remove" commit))

(defun mercit-notes-merge (ref)
  "Merge the notes ref REF into the current notes ref.

The current notes ref is the value of Git variable
`core.notesRef' or \"refs/notes/commits\" if that is undefined.

When there are conflicts, then they have to be resolved in the
temporary worktree \".git/NOTES_MERGE_WORKTREE\".  When
done use `mercit-notes-merge-commit' to finish.  To abort
use `mercit-notes-merge-abort'."
  (interactive (list (mercit-read-string-ns "Merge reference")))
  (mercit-run-git-with-editor "notes" "merge" ref))

(defun mercit-notes-merge-commit ()
  "Commit the current notes ref merge.
Also see `mercit-notes-merge'."
  (interactive)
  (mercit-run-git-with-editor "notes" "merge" "--commit"))

(defun mercit-notes-merge-abort ()
  "Abort the current notes ref merge.
Also see `mercit-notes-merge'."
  (interactive)
  (mercit-run-git-with-editor "notes" "merge" "--abort"))

(defun mercit-notes-prune (&optional dry-run)
  "Remove notes about unreachable commits."
  (interactive (list (and (member "--dry-run" (transient-args 'mercit-notes)) t)))
  (when dry-run
    (mercit-process-buffer))
  (mercit-run-git-with-editor "notes" "prune" (and dry-run "--dry-run")))

;;; Readers

(defun mercit-notes-read-ref (prompt _initial-input history)
  (and-let* ((ref (mercit-completing-read
                   prompt (mercit-list-notes-refnames) nil nil
                   (and-let* ((def (mercit-get "core.notesRef")))
                     (if (string-prefix-p "refs/notes/" def)
                         (substring def 11)
                       def))
                   history)))
    (if (string-prefix-p "refs/" ref)
        ref
      (concat "refs/notes/" ref))))

(defun mercit-notes-read-refs (prompt &optional _initial-input _history)
  (mapcar (lambda (ref)
            (if (string-prefix-p "refs/" ref)
                ref
              (concat "refs/notes/" ref)))
          (completing-read-multiple
           (concat prompt ": ")
           (mercit-list-notes-refnames) nil nil
           (mapconcat (lambda (ref)
                        (if (string-prefix-p "refs/notes/" ref)
                            (substring ref 11)
                          ref))
                      (mercit-get-all "notes.displayRef")
                      ","))))

(defun mercit-notes-read-args (prompt)
  (list (mercit-read-branch-or-commit prompt (mercit-stash-at-point))
        (and-let* ((str (--first (string-match "^--ref=\\(.+\\)" it)
                                 (transient-args 'mercit-notes))))
          (match-string 1 str))))

;;; _
(provide 'mercit-notes)
;;; mercit-notes.el ends here
