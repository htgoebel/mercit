;;; mercit-patch.el --- Creating and applying patches  -*- lexical-binding:t -*-

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

;; This library implements patch commands.

;;; Code:

(require 'mercit)

;;; Options

(defcustom mercit-patch-save-arguments '(exclude "--stat")
  "Control arguments used by the command `mercit-patch-save'.

`mercit-patch-save' (which see) saves a diff for the changes
shown in the current buffer in a patch file.  It may use the
same arguments as used in the buffer or a subset thereof, or
a constant list of arguments, depending on this option and
the prefix argument."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-diff
  :type '(choice (const :tag "use buffer arguments" buffer)
                 (cons :tag "use buffer arguments except"
                       (const :format "" exclude)
                       (repeat :format "%v%i\n"
                               (string :tag "Argument")))
                 (repeat :tag "use constant arguments"
                         (string :tag "Argument"))))

;;; Commands

;;;###autoload (autoload 'mercit-patch "mercit-patch" nil t)
(transient-define-prefix mercit-patch ()
  "Create or apply patches."
  ["Actions"
   [("c" "*Create patches"     mercit-patch-create)
    ("w" "*Apply patches"      mercit-am)]
   [("a" "*Apply plain patch"  mercit-patch-apply)
    ("s" "*Save diff as patch" mercit-patch-save)]
   [("r" "*Request pull"       mercit-request-pull)]])

;;;###autoload (autoload 'mercit-patch-create "mercit-patch" nil t)
(transient-define-prefix mercit-patch-create (range args files)
  "Create patches for the commits in RANGE.
When a single commit is given for RANGE, create a patch for the
changes introduced by that commit (unlike 'git format-patch'
which creates patches for all commits that are reachable from
`HEAD' but not from the specified commit)."
  :man-page "git-format-patch"
  :incompatible '(("--subject-prefix=" "--rfc"))
  ["Mail arguments"
   (6 mercit-format-patch:--in-reply-to)
   (6 mercit-format-patch:--thread)
   (6 mercit-format-patch:--from)
   (6 mercit-format-patch:--to)
   (6 mercit-format-patch:--cc)]
  ["Patch arguments"
   (mercit-format-patch:--base)
   (mercit-format-patch:--reroll-count)
   (5 mercit-format-patch:--interdiff)
   (mercit-format-patch:--range-diff)
   (mercit-format-patch:--subject-prefix)
   ("C-m r  " "*RFC subject prefix" "--rfc")
   ("C-m l  " "*Add cover letter" "--cover-letter")
   (5 mercit-format-patch:--cover-from-description)
   (mercit-format-patch:--output-directory)]
  ["Diff arguments"
   (mercit-diff:-U)
   (mercit-diff:-M)
   (mercit-diff:-C)
   (mercit-diff:--diff-algorithm)
   (mercit:--)
   (7 "-b" "*Ignore whitespace changes" ("-b" "--ignore-space-change"))
   (7 "-w" "*Ignore all whitespace"     ("-w" "--ignore-all-space"))]
  ["Actions"
   ("c" "Create patches" mercit-patch-create)]
  (interactive
   (if (not (eq transient-current-command 'mercit-patch-create))
       (list nil nil nil)
     (cons (if-let ((revs (mercit-region-values 'commit t)))
               (concat (car (last revs)) "^.." (car revs))
             (let ((range (mercit-read-range-or-commit
                           "Format range or commit")))
               (if (string-search ".." range)
                   range
                 (format "%s~..%s" range range))))
           (let ((args (transient-args 'mercit-patch-create)))
             (list (-filter #'stringp args)
                   (cdr (assoc "--" args)))))))
  (if (not range)
      (transient-setup 'mercit-patch-create)
    (mercit-run-git "format-patch" range args "--" files)
    (when (member "--cover-letter" args)
      (save-match-data
        (find-file
         (expand-file-name
          (concat (and-let* ((v (transient-arg-value "--reroll-count=" args)))
                    (format "v%s-" v))
                  "0000-cover-letter.patch")
          (let ((topdir (mercit-toplevel)))
            (if-let ((dir (transient-arg-value "--output-directory=" args)))
                (expand-file-name dir topdir)
              topdir))))))))

(transient-define-argument mercit-format-patch:--in-reply-to ()
  :description "*In reply to"
  :class 'transient-option
  :key "C-m C-r"
  :argument "--in-reply-to=")

(transient-define-argument mercit-format-patch:--thread ()
  :description "*Thread style"
  :class 'transient-option
  :key "C-m s  "
  :argument "--thread="
  :reader #'mercit-format-patch-select-thread-style)

(defun mercit-format-patch-select-thread-style (&rest _ignore)
  (mercit-read-char-case "Thread style " t
    (?d "*[d]eep" "deep")
    (?s "*[s]hallow" "shallow")))

(transient-define-argument mercit-format-patch:--base ()
  :description "*Insert base commit"
  :class 'transient-option
  :key "C-m b  "
  :argument "--base="
  :reader #'mercit-format-patch-select-base)

(defun mercit-format-patch-select-base (prompt initial-input history)
  (or (mercit-completing-read prompt (cons "auto" (mercit-list-refnames))
                             nil nil initial-input history "auto")
      (user-error "Nothing selected")))

(transient-define-argument mercit-format-patch:--reroll-count ()
  :description "*Reroll count"
  :class 'transient-option
  :key "C-m v  "
  :shortarg "-v"
  :argument "--reroll-count="
  :reader #'transient-read-number-N+)

(transient-define-argument mercit-format-patch:--interdiff ()
  :description "*Insert interdiff"
  :class 'transient-option
  :key "C-m d i"
  :argument "--interdiff="
  :reader #'mercit-transient-read-revision)

(transient-define-argument mercit-format-patch:--range-diff ()
  :description "*Insert range-diff"
  :class 'transient-option
  :key "C-m d r"
  :argument "--range-diff="
  :reader #'mercit-format-patch-select-range-diff)

(defun mercit-format-patch-select-range-diff (prompt _initial-input _history)
  (mercit-read-range-or-commit prompt))

(transient-define-argument mercit-format-patch:--subject-prefix ()
  :description "*Subject Prefix"
  :class 'transient-option
  :key "C-m p  "
  :argument "--subject-prefix=")

(transient-define-argument mercit-format-patch:--cover-from-description ()
  :description "*Use branch description"
  :class 'transient-option
  :key "C-m D  "
  :argument "--cover-from-description="
  :reader #'mercit-format-patch-select-description-mode)

(defun mercit-format-patch-select-description-mode (&rest _ignore)
  (mercit-read-char-case "Use description as " t
    (?m "*[m]essage" "message")
    (?s "*[s]ubject" "subject")
    (?a "*[a]uto"    "auto")
    (?n "*[n]othing" "none")))

(transient-define-argument mercit-format-patch:--from ()
  :description "*From"
  :class 'transient-option
  :key "C-m C-f"
  :argument "--from="
  :reader #'mercit-transient-read-person)

(transient-define-argument mercit-format-patch:--to ()
  :description "*To"
  :class 'transient-option
  :key "C-m C-t"
  :argument "--to="
  :reader #'mercit-transient-read-person)

(transient-define-argument mercit-format-patch:--cc ()
  :description "*CC"
  :class 'transient-option
  :key "C-m C-c"
  :argument "--cc="
  :reader #'mercit-transient-read-person)

(transient-define-argument mercit-format-patch:--output-directory ()
  :description "*Output directory"
  :class 'transient-option
  :key "C-m o  "
  :shortarg "-o"
  :argument "--output-directory="
  :reader #'transient-read-existing-directory)

;;;###autoload (autoload 'mercit-patch-apply "mercit-patch" nil t)
(transient-define-prefix mercit-patch-apply (file &rest args)
  "Apply the patch file FILE."
  :man-page "git-apply"
  ["Arguments"
   ("-i" "*Also apply to index" "--index")
   ("-c" "*Only apply to index" "--cached")
   ("-3" "*Fall back on 3way merge" ("-3" "--3way"))]
  ["Actions"
   ("a"  "Apply patch" mercit-patch-apply)]
  (interactive
   (if (not (eq transient-current-command 'mercit-patch-apply))
       (list nil)
     (list (expand-file-name
            (read-file-name "Apply patch: "
                            default-directory nil nil
                            (and-let* ((file (mercit-file-at-point)))
                              (file-relative-name file))))
           (transient-args 'mercit-patch-apply))))
  (if (not file)
      (transient-setup 'mercit-patch-apply)
    (mercit-run-git "apply" args "--" (mercit-convert-filename-for-git file))))

;;;###autoload
(defun mercit-patch-save (file &optional arg)
  "Write current diff into patch FILE.

What arguments are used to create the patch depends on the value
of `mercit-patch-save-arguments' and whether a prefix argument is
used.

If the value is the symbol `buffer', then use the same arguments
as the buffer.  With a prefix argument use no arguments.

If the value is a list beginning with the symbol `exclude', then
use the same arguments as the buffer except for those matched by
entries in the cdr of the list.  The comparison is done using
`string-prefix-p'.  With a prefix argument use the same arguments
as the buffer.

If the value is a list of strings (including the empty list),
then use those arguments.  With a prefix argument use the same
arguments as the buffer.

Of course the arguments that are required to actually show the
same differences as those shown in the buffer are always used."
  (interactive (list (read-file-name "Write patch file: " default-directory)
                     current-prefix-arg))
  (unless (derived-mode-p 'mercit-diff-mode)
    (user-error "Only diff buffers can be saved as patches"))
  (let ((rev     mercit-buffer-range)
        (typearg mercit-buffer-typearg)
        (args    mercit-buffer-diff-args)
        (files   mercit-buffer-diff-files))
    (cond ((eq mercit-patch-save-arguments 'buffer)
           (when arg
             (setq args nil)))
          ((eq (car-safe mercit-patch-save-arguments) 'exclude)
           (unless arg
             (setq args (-difference args (cdr mercit-patch-save-arguments)))))
          ((not arg)
           (setq args mercit-patch-save-arguments)))
    (with-temp-file file
      (mercit-git-insert "diff" rev "-p" typearg args "--" files)))
  (mercit-refresh))

;;;###autoload
(defun mercit-request-pull (url start end)
  "Request upstream to pull from your public repository.

URL is the url of your publicly accessible repository.
START is a commit that already is in the upstream repository.
END is the last commit, usually a branch name, which upstream
is asked to pull.  START has to be reachable from that commit."
  (interactive
   (list (mercit-get "remote" (mercit-read-remote "Remote") "url")
         (mercit-read-branch-or-commit "Start" (mercit-get-upstream-branch))
         (mercit-read-branch-or-commit "End")))
  (let ((dir default-directory))
    ;; mu4e changes default-directory
    (compose-mail)
    (setq default-directory dir))
  (message-goto-body)
  (mercit-git-insert "request-pull" start url end)
  (set-buffer-modified-p nil))

;;; _
(provide 'mercit-patch)
;;; mercit-patch.el ends here
