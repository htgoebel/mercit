;;; mercit-apply.el --- Apply Git diffs  -*- lexical-binding:t -*-

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

;; This library implements commands for applying Git diffs or parts
;; of such a diff.  The supported "apply variants" are apply, stage,
;; unstage, discard, and reverse - more than Git itself knows about,
;; at least at the porcelain level.

;;; Code:

(require 'mercit-core)
(require 'mercit-diff)
(require 'mercit-wip)

(require 'transient) ; See #3732.

;; For `mercit-apply'
(declare-function mercit-am "mercit-sequence" () t)
(declare-function mercit-patch-apply "mercit-patch" () t)
;; For `mercit-discard-files'
(declare-function mercit-checkout-stage "mercit-merge" (file arg))
(declare-function mercit-checkout-read-stage "mercit-merge" (file))
(defvar auto-revert-verbose)
;; For `mercit-stage-untracked'
(declare-function mercit-submodule-add-1 "mercit-submodule"
                  (url &optional path name args))
(declare-function mercit-submodule-read-name-for-path "mercit-submodule"
                  (path &optional prefer-short))
(defvar borg-user-emacs-directory)

(cl-eval-when (compile load)
  (when (< emacs-major-version 26)
    (defalias 'smerge-keep-upper 'smerge-keep-mine)
    (defalias 'smerge-keep-lower 'smerge-keep-other)))

;;; Options

(defcustom mercit-delete-by-moving-to-trash t
  "Whether Magit uses the system's trash can.

You should absolutely not disable this and also remove `discard'
from `mercit-no-confirm'.  You shouldn't do that even if you have
all of the Magit-Wip modes enabled, because those modes do not
track any files that are not tracked in the proper branch."
  :package-version '(mercit . "2.1.0")
  :group 'mercit-essentials
  :type 'boolean)

(defcustom mercit-unstage-committed t
  "Whether unstaging a committed change reverts it instead.

A committed change cannot be unstaged, because staging and
unstaging are actions that are concerned with the differences
between the index and the working tree, not with committed
changes.

If this option is non-nil (the default), then typing \"u\"
\(`mercit-unstage') on a committed change, causes it to be
reversed in the index but not the working tree.  For more
information see command `mercit-reverse-in-index'."
  :package-version '(mercit . "2.4.1")
  :group 'mercit-commands
  :type 'boolean)

(defcustom mercit-reverse-atomically nil
  "Whether to reverse changes atomically.

If some changes can be reversed while others cannot, then nothing
is reversed if the value of this option is non-nil.  But when it
is nil, then the changes that can be reversed are reversed and
for the other changes diff files are created that contain the
rejected reversals."
  :package-version '(mercit . "2.7.0")
  :group 'mercit-commands
  :type 'boolean)

(defcustom mercit-post-stage-hook nil
  "Hook run after staging changes.
This hook is run by `mercit-refresh' if `this-command'
is a member of `mercit-post-stage-hook-commands'."
  :package-version '(mercit . "2.90.0")
  :group 'mercit-commands
  :type 'hook)

(defcustom mercit-post-unstage-hook nil
  "Hook run after unstaging changes.
This hook is run by `mercit-refresh' if `this-command'
is a member of `mercit-post-unstage-hook-commands'."
  :package-version '(mercit . "2.90.0")
  :group 'mercit-commands
  :type 'hook)

;;; Commands
;;;; Apply

(defun mercit-apply (&rest args)
  "Apply the change at point to the working tree.
With a prefix argument fallback to a 3-way merge.  Doing
so causes the change to be applied to the index as well."
  (interactive (and current-prefix-arg (list "--3way")))
  (--when-let (mercit-apply--get-selection)
    (pcase (list (mercit-diff-type) (mercit-diff-scope))
      (`(,(or 'unstaged 'staged) ,_)
       (user-error "Change is already in the working tree"))
      (`(untracked ,(or 'file 'files))
       (call-interactively #'mercit-am))
      (`(,_ region) (mercit-apply-region it args))
      (`(,_   hunk) (mercit-apply-hunk   it args))
      (`(,_  hunks) (mercit-apply-hunks  it args))
      (`(rebase-sequence file)
       (call-interactively #'mercit-patch-apply))
      (`(,_   file) (mercit-apply-diff   it args))
      (`(,_  files) (mercit-apply-diffs  it args)))))

(defun mercit-apply--section-content (section)
  (buffer-substring-no-properties (if (mercit-hunk-section-p section)
                                      (oref section start)
                                    (oref section content))
                                  (oref section end)))

(defun mercit-apply-diffs (sections &rest args)
  (setq sections (mercit-apply--get-diffs sections))
  (mercit-apply-patch sections args
                     (mapconcat
                      (lambda (s)
                        (concat (mercit-diff-file-header s)
                                (mercit-apply--section-content s)))
                      sections "")))

(defun mercit-apply-diff (section &rest args)
  (setq section (car (mercit-apply--get-diffs (list section))))
  (mercit-apply-patch section args
                     (concat (mercit-diff-file-header section)
                             (mercit-apply--section-content section))))

(defun mercit-apply--adjust-hunk-new-starts (hunks)
  "Adjust new line numbers in headers of HUNKS for partial application.
HUNKS should be a list of ordered, contiguous hunks to be applied
from a file.  For example, if there is a sequence of hunks with
the headers

  @@ -2,6 +2,7 @@
  @@ -10,6 +11,7 @@
  @@ -18,6 +20,7 @@

and only the second and third are to be applied, they would be
adjusted as \"@@ -10,6 +10,7 @@\" and \"@@ -18,6 +19,7 @@\"."
  (let* ((first-hunk (car hunks))
         (offset (if (string-match diff-hunk-header-re-unified first-hunk)
                     (- (string-to-number (match-string 3 first-hunk))
                        (string-to-number (match-string 1 first-hunk)))
                   (error "Header hunks have to be applied individually"))))
    (if (= offset 0)
        hunks
      (mapcar (lambda (hunk)
                (if (string-match diff-hunk-header-re-unified hunk)
                    (replace-match (number-to-string
                                    (- (string-to-number (match-string 3 hunk))
                                       offset))
                                   t t hunk 3)
                  (error "Hunk does not have expected header")))
              hunks))))

(defun mercit-apply--adjust-hunk-new-start (hunk)
  (car (mercit-apply--adjust-hunk-new-starts (list hunk))))

(defun mercit-apply-hunks (sections &rest args)
  (let ((section (oref (car sections) parent)))
    (when (string-match "^diff --cc" (oref section value))
      (user-error "Cannot un-/stage resolution hunks.  Stage the whole file"))
    (mercit-apply-patch
     section args
     (concat (oref section header)
             (mapconcat #'identity
                        (mercit-apply--adjust-hunk-new-starts
                         (mapcar #'mercit-apply--section-content sections))
                        "")))))

(defun mercit-apply-hunk (section &rest args)
  (when (string-match "^diff --cc" (mercit-section-parent-value section))
    (user-error "Cannot un-/stage resolution hunks.  Stage the whole file"))
  (let* ((header (car (oref section value)))
         (header (and (symbolp header) header))
         (content (mercit-apply--section-content section)))
    (mercit-apply-patch
     (oref section parent) args
     (concat (mercit-diff-file-header section (not (eq header 'rename)))
             (if header
                 content
               (mercit-apply--adjust-hunk-new-start content))))))

(defun mercit-apply-region (section &rest args)
  (when (string-match "^diff --cc" (mercit-section-parent-value section))
    (user-error "Cannot un-/stage resolution hunks.  Stage the whole file"))
  (mercit-apply-patch (oref section parent) args
                     (concat (mercit-diff-file-header section)
                             (mercit-apply--adjust-hunk-new-start
                              (mercit-diff-hunk-region-patch section args)))))

(defun mercit-apply-patch (section:s args patch)
  (let* ((files (if (atom section:s)
                    (list (oref section:s value))
                  (--map (oref it value) section:s)))
         (command (symbol-name this-command))
         (command (if (and command (string-match "^mercit-\\([^-]+\\)" command))
                      (match-string 1 command)
                    "apply"))
         (ignore-context (mercit-diff-ignore-any-space-p)))
    (unless (mercit-diff-context-p)
      (user-error "Not enough context to apply patch.  Increase the context"))
    (when (and mercit-wip-before-change-mode (not mercit-inhibit-refresh))
      (mercit-wip-commit-before-change files (concat " before " command)))
    (with-temp-buffer
      (insert patch)
      (mercit-run-git-with-input
       "apply" args "-p0"
       (and ignore-context "-C0")
       "--ignore-space-change" "-"))
    (unless mercit-inhibit-refresh
      (when mercit-wip-after-apply-mode
        (mercit-wip-commit-after-apply files (concat " after " command)))
      (mercit-refresh))))

(defun mercit-apply--get-selection ()
  (or (mercit-region-sections '(hunk file module) t)
      (let ((section (mercit-current-section)))
        (pcase (oref section type)
          ((or 'hunk 'file 'module) section)
          ((or 'staged 'unstaged 'untracked
               'stashed-index 'stashed-worktree 'stashed-untracked)
           (oref section children))
          (_ (user-error "Cannot apply this, it's not a change"))))))

(defun mercit-apply--get-diffs (sections)
  (mercit-section-case
    ([file diffstat]
     (--map (or (mercit-get-section
                 (append `((file . ,(oref it value)))
                         (mercit-section-ident mercit-root-section)))
                (error "Cannot get required diff headers"))
            sections))
    (t sections)))

(defun mercit-apply--diff-ignores-whitespace-p ()
  (and (cl-intersection mercit-buffer-diff-args
                        '("--ignore-space-at-eol"
                          "--ignore-space-change"
                          "--ignore-all-space"
                          "--ignore-blank-lines")
                        :test #'equal)
       t))

;;;; Stage

(defun mercit-stage (&optional intent)
  "Add the change at point to the staging area.
With a prefix argument, INTENT, and an untracked file (or files)
at point, stage the file but not its content."
  (interactive "P")
  (--if-let (and (derived-mode-p 'mercit-mode) (mercit-apply--get-selection))
      (pcase (list (mercit-diff-type)
                   (mercit-diff-scope)
                   (mercit-apply--diff-ignores-whitespace-p))
        (`(untracked     ,_  ,_) (mercit-stage-untracked intent))
        (`(unstaged  region  ,_) (mercit-apply-region it "--cached"))
        (`(unstaged    hunk  ,_) (mercit-apply-hunk   it "--cached"))
        (`(unstaged   hunks  ,_) (mercit-apply-hunks  it "--cached"))
        ('(unstaged    file   t) (mercit-apply-diff   it "--cached"))
        ('(unstaged   files   t) (mercit-apply-diffs  it "--cached"))
        ('(unstaged    list   t) (mercit-apply-diffs  it "--cached"))
        ('(unstaged    file nil) (mercit-stage-1 "-u" (list (oref it value))))
        ('(unstaged   files nil) (mercit-stage-1 "-u" (mercit-region-values nil t)))
        ('(unstaged    list nil) (mercit-stage-modified))
        (`(staged        ,_  ,_) (user-error "Already staged"))
        (`(committed     ,_  ,_) (user-error "Cannot stage committed changes"))
        (`(undefined     ,_  ,_) (user-error "Cannot stage this change")))
    (call-interactively #'mercit-stage-file)))

;;;###autoload
(defun mercit-stage-file (file)
  "Stage all changes to FILE.
With a prefix argument or when there is no file at point ask for
the file to be staged.  Otherwise stage the file at point without
requiring confirmation."
  (interactive
   (let* ((atpoint (mercit-section-value-if 'file))
          (current (mercit-file-relative-name))
          (choices (nconc (mercit-unstaged-files)
                          (mercit-untracked-files)))
          (default (car (member (or atpoint current) choices))))
     (list (if (or current-prefix-arg (not default))
               (mercit-completing-read "Stage file" choices
                                      nil t nil nil default)
             default))))
  (mercit-with-toplevel
    (mercit-stage-1 nil (list file))))

;;;###autoload
(defun mercit-stage-modified (&optional all)
  "Stage all changes to files modified in the worktree.
Stage all new content of tracked files and remove tracked files
that no longer exist in the working tree from the index also.
With a prefix argument also stage previously untracked (but not
ignored) files."
  (interactive "P")
  (when (mercit-anything-staged-p)
    (mercit-confirm 'stage-all-changes))
  (mercit-with-toplevel
    (mercit-stage-1 (if all "--all" "-u") mercit-buffer-diff-files)))

(defun mercit-stage-1 (arg &optional files)
  (mercit-wip-commit-before-change files " before stage")
  (mercit-run-git "add" arg (if files (cons "--" files) "."))
  (when mercit-auto-revert-mode
    (mapc #'mercit-turn-on-auto-revert-mode-if-desired files))
  (mercit-wip-commit-after-apply files " after stage"))

(defun mercit-stage-untracked (&optional intent)
  (let* ((section (mercit-current-section))
         (files (pcase (mercit-diff-scope)
                  ('file  (list (oref section value)))
                  ('files (mercit-region-values nil t))
                  ('list  (mercit-untracked-files))))
         plain repos)
    (dolist (file files)
      (if (and (not (file-symlink-p file))
               (mercit-git-repo-p file t))
          (push file repos)
        (push file plain)))
    (mercit-wip-commit-before-change files " before stage")
    (when plain
      (mercit-run-git "add" (and intent "--intent-to-add")
                     "--" plain)
      (when mercit-auto-revert-mode
        (mapc #'mercit-turn-on-auto-revert-mode-if-desired plain)))
    (dolist (repo repos)
      (save-excursion
        (goto-char (oref (mercit-get-section
                          `((file . ,repo) (untracked) (status)))
                         start))
        (when (and (fboundp 'borg-assimilate)
                   (fboundp 'borg--maybe-absorb-gitdir)
                   (fboundp 'borg--sort-submodule-sections))
          (let* ((topdir (mercit-toplevel))
                 (url (let ((default-directory
                             (file-name-as-directory (expand-file-name repo))))
                        (or (mercit-get "remote" (mercit-get-some-remote) "url")
                            (concat (file-name-as-directory ".") repo))))
                 (package
                  (and (equal borg-user-emacs-directory topdir)
                       (file-name-nondirectory (directory-file-name repo)))))
            (if (and package
                     (y-or-n-p (format "Also assimilate `%s' drone?" package)))
                (borg-assimilate package url)
              (mercit-submodule-add-1
               url repo (mercit-submodule-read-name-for-path repo package))
              (when package
                (borg--sort-submodule-sections
                 (expand-file-name ".gitmodules" topdir))
                (let ((default-directory borg-user-emacs-directory))
                  (borg--maybe-absorb-gitdir package))))))))
    (mercit-wip-commit-after-apply files " after stage")))

(defvar mercit-post-stage-hook-commands
  '(mercit-stage mercit-stage-file mercit-stage-modified))

(defun mercit-run-post-stage-hook ()
  (when (memq this-command mercit-post-stage-hook-commands)
    (mercit-run-hook-with-benchmark 'mercit-post-stage-hook)))

;;;; Unstage

(defun mercit-unstage ()
  "Remove the change at point from the staging area."
  (interactive)
  (--when-let (mercit-apply--get-selection)
    (pcase (list (mercit-diff-type)
                 (mercit-diff-scope)
                 (mercit-apply--diff-ignores-whitespace-p))
      (`(untracked     ,_  ,_) (user-error "Cannot unstage untracked changes"))
      (`(unstaged    file  ,_) (mercit-unstage-intent (list (oref it value))))
      (`(unstaged   files  ,_) (mercit-unstage-intent (mercit-region-values nil t)))
      (`(unstaged      ,_  ,_) (user-error "Already unstaged"))
      (`(staged    region  ,_) (mercit-apply-region it "--reverse" "--cached"))
      (`(staged      hunk  ,_) (mercit-apply-hunk   it "--reverse" "--cached"))
      (`(staged     hunks  ,_) (mercit-apply-hunks  it "--reverse" "--cached"))
      ('(staged      file   t) (mercit-apply-diff   it "--reverse" "--cached"))
      ('(staged     files   t) (mercit-apply-diffs  it "--reverse" "--cached"))
      ('(staged      list   t) (mercit-apply-diffs  it "--reverse" "--cached"))
      ('(staged      file nil) (mercit-unstage-1 (list (oref it value))))
      ('(staged     files nil) (mercit-unstage-1 (mercit-region-values nil t)))
      ('(staged      list nil) (mercit-unstage-all))
      (`(committed     ,_  ,_) (if mercit-unstage-committed
                                   (mercit-reverse-in-index)
                                 (user-error "Cannot unstage committed changes")))
      (`(undefined     ,_  ,_) (user-error "Cannot unstage this change")))))

;;;###autoload
(defun mercit-unstage-file (file)
  "Unstage all changes to FILE.
With a prefix argument or when there is no file at point ask for
the file to be unstaged.  Otherwise unstage the file at point
without requiring confirmation."
  (interactive
   (let* ((atpoint (mercit-section-value-if 'file))
          (current (mercit-file-relative-name))
          (choices (mercit-staged-files))
          (default (car (member (or atpoint current) choices))))
     (list (if (or current-prefix-arg (not default))
               (mercit-completing-read "Unstage file" choices
                                      nil t nil nil default)
             default))))
  (mercit-with-toplevel
    (mercit-unstage-1 (list file))))

(defun mercit-unstage-1 (files)
  (mercit-wip-commit-before-change files " before unstage")
  (if (mercit-no-commit-p)
      (mercit-run-git "rm" "--cached" "--" files)
    (mercit-run-git "reset" "HEAD" "--" files))
  (mercit-wip-commit-after-apply files " after unstage"))

(defun mercit-unstage-intent (files)
  (if-let ((staged (mercit-staged-files))
           (intent (--filter (member it staged) files)))
      (mercit-unstage-1 intent)
    (user-error "Already unstaged")))

;;;###autoload
(defun mercit-unstage-all ()
  "Remove all changes from the staging area."
  (interactive)
  (unless (mercit-anything-staged-p)
    (user-error "Nothing to unstage"))
  (when (or (mercit-anything-unstaged-p)
            (mercit-untracked-files))
    (mercit-confirm 'unstage-all-changes))
  (mercit-wip-commit-before-change nil " before unstage")
  (mercit-run-git "reset" "HEAD" "--" mercit-buffer-diff-files)
  (mercit-wip-commit-after-apply nil " after unstage"))

(defvar mercit-post-unstage-hook-commands
  '(mercit-unstage mercit-unstage-file mercit-unstage-all))

(defun mercit-run-post-unstage-hook ()
  (when (memq this-command mercit-post-unstage-hook-commands)
    (mercit-run-hook-with-benchmark 'mercit-post-unstage-hook)))

;;;; Discard

(defun mercit-discard ()
  "Remove the change at point.

On a hunk or file with unresolved conflicts prompt which side to
keep (while discarding the other).  If point is within the text
of a side, then keep that side without prompting."
  (interactive)
  (--when-let (mercit-apply--get-selection)
    (pcase (list (mercit-diff-type) (mercit-diff-scope))
      (`(committed ,_) (user-error "Cannot discard committed changes"))
      (`(undefined ,_) (user-error "Cannot discard this change"))
      (`(,_    region) (mercit-discard-region it))
      (`(,_      hunk) (mercit-discard-hunk   it))
      (`(,_     hunks) (mercit-discard-hunks  it))
      (`(,_      file) (mercit-discard-file   it))
      (`(,_     files) (mercit-discard-files  it))
      (`(,_      list) (mercit-discard-files  it)))))

(defun mercit-discard-region (section)
  (mercit-confirm 'discard "Discard region")
  (mercit-discard-apply section 'mercit-apply-region))

(defun mercit-discard-hunk (section)
  (mercit-confirm 'discard "Discard hunk")
  (let ((file (mercit-section-parent-value section)))
    (pcase (cddr (car (mercit-file-status file)))
      ('(?U ?U) (mercit-smerge-keep-current))
      (_ (mercit-discard-apply section #'mercit-apply-hunk)))))

(defun mercit-discard-apply (section apply)
  (if (eq (mercit-diff-type section) 'unstaged)
      (funcall apply section "--reverse")
    (if (mercit-anything-unstaged-p
         nil (if (mercit-file-section-p section)
                 (oref section value)
               (mercit-section-parent-value section)))
        (progn (let ((mercit-inhibit-refresh t))
                 (funcall apply section "--reverse" "--cached")
                 (funcall apply section "--reverse" "--reject"))
               (mercit-refresh))
      (funcall apply section "--reverse" "--index"))))

(defun mercit-discard-hunks (sections)
  (mercit-confirm 'discard (format "Discard %s hunks from %s"
                                  (length sections)
                                  (mercit-section-parent-value (car sections))))
  (mercit-discard-apply-n sections #'mercit-apply-hunks))

(defun mercit-discard-apply-n (sections apply)
  (let ((section (car sections)))
    (if (eq (mercit-diff-type section) 'unstaged)
        (funcall apply sections "--reverse")
      (if (mercit-anything-unstaged-p
           nil (if (mercit-file-section-p section)
                   (oref section value)
                 (mercit-section-parent-value section)))
          (progn (let ((mercit-inhibit-refresh t))
                   (funcall apply sections "--reverse" "--cached")
                   (funcall apply sections "--reverse" "--reject"))
                 (mercit-refresh))
        (funcall apply sections "--reverse" "--index")))))

(defun mercit-discard-file (section)
  (mercit-discard-files (list section)))

(defun mercit-discard-files (sections)
  (let ((auto-revert-verbose nil)
        (type (mercit-diff-type (car sections)))
        (status (mercit-file-status))
        files delete resurrect rename discard discard-new resolve)
    (dolist (section sections)
      (let ((file (oref section value)))
        (push file files)
        (pcase (cons (pcase type
                       (`staged ?X)
                       (`unstaged ?Y)
                       (`untracked ?Z))
                     (cddr (assoc file status)))
          ('(?Z) (dolist (f (mercit-untracked-files nil file))
                   (push f delete)))
          ((or '(?Z ?? ??) '(?Z ?! ?!)) (push file delete))
          ('(?Z ?D ? )                  (push file delete))
          (`(,_ ?D ?D)                  (push file resolve))
          ((or `(,_ ?U ,_) `(,_ ,_ ?U)) (push file resolve))
          (`(,_ ?A ?A)                  (push file resolve))
          (`(?X ?M ,(or ?  ?M ?D)) (push section discard))
          (`(?Y ,_         ?M    ) (push section discard))
          ('(?X ?A         ?M    ) (push file discard-new))
          ('(?X ?C         ?M    ) (push file discard-new))
          (`(?X ?A ,(or ?     ?D)) (push file delete))
          (`(?X ?C ,(or ?     ?D)) (push file delete))
          (`(?X ?D ,(or ?  ?M   )) (push file resurrect))
          (`(?Y ,_            ?D ) (push file resurrect))
          (`(?X ?R ,(or ?  ?M ?D)) (push file rename)))))
    (unwind-protect
        (let ((mercit-inhibit-refresh t))
          (mercit-wip-commit-before-change files " before discard")
          (when resolve
            (mercit-discard-files--resolve (nreverse resolve)))
          (when resurrect
            (mercit-discard-files--resurrect (nreverse resurrect)))
          (when delete
            (mercit-discard-files--delete (nreverse delete) status))
          (when rename
            (mercit-discard-files--rename (nreverse rename) status))
          (when (or discard discard-new)
            (mercit-discard-files--discard (nreverse discard)
                                          (nreverse discard-new)))
          (mercit-wip-commit-after-apply files " after discard"))
      (mercit-refresh))))

(defun mercit-discard-files--resolve (files)
  (if-let ((arg (and (cdr files)
                     (mercit-read-char-case
                         (format "For these %i files\n%s\ncheckout:\n"
                                 (length files)
                                 (mapconcat (lambda (file)
                                              (concat "  " file))
                                            files "\n"))
                         t
                       (?o "[o]ur stage"   "--ours")
                       (?t "[t]heir stage" "--theirs")
                       (?c "[c]onflict"    "--merge")
                       (?i "decide [i]ndividually" nil)))))
      (dolist (file files)
        (mercit-checkout-stage file arg))
    (dolist (file files)
      (mercit-checkout-stage file (mercit-checkout-read-stage file)))))

(defun mercit-discard-files--resurrect (files)
  (mercit-confirm-files 'resurrect files)
  (if (eq (mercit-diff-type) 'staged)
      (mercit-call-git "reset"  "--" files)
    (mercit-call-git "checkout" "--" files)))

(defun mercit-discard-files--delete (files status)
  (mercit-confirm-files (if mercit-delete-by-moving-to-trash 'trash 'delete)
                       files)
  (let ((delete-by-moving-to-trash mercit-delete-by-moving-to-trash))
    (dolist (file files)
      (when (string-match-p "\\`\\\\?~" file)
        (error "Refusing to delete %S, too dangerous" file))
      (pcase (nth 3 (assoc file status))
        ((guard (memq (mercit-diff-type) '(unstaged untracked)))
         (dired-delete-file file dired-recursive-deletes
                            mercit-delete-by-moving-to-trash)
         (dired-clean-up-after-deletion file))
        (?\s (delete-file file t)
             (mercit-call-git "rm" "--cached" "--" file))
        (?M  (let ((temp (mercit-git-string "checkout-index" "--temp" file)))
               (string-match
                (format "\\(.+?\\)\t%s" (regexp-quote file)) temp)
               (rename-file (match-string 1 temp)
                            (setq temp (concat file ".~{index}~")))
               (delete-file temp t))
             (mercit-call-git "rm" "--cached" "--force" "--" file))
        (?D  (mercit-call-git "checkout" "--" file)
             (delete-file file t)
             (mercit-call-git "rm" "--cached" "--force" "--" file))))))

(defun mercit-discard-files--rename (files status)
  (mercit-confirm 'rename "Undo rename %s" "Undo %i renames" nil
    (mapcar (lambda (file)
              (setq file (assoc file status))
              (format "%s -> %s" (cadr file) (car file)))
            files))
  (dolist (file files)
    (let ((orig (cadr (assoc file status))))
      (if (file-exists-p file)
          (progn
            (--when-let (file-name-directory orig)
              (make-directory it t))
            (mercit-call-git "mv" file orig))
        (mercit-call-git "rm" "--cached" "--" file)
        (mercit-call-git "reset" "--" orig)))))

(defun mercit-discard-files--discard (sections new-files)
  (let ((files (--map (oref it value) sections)))
    (mercit-confirm-files 'discard (append files new-files)
                         (format "Discard %s changes in" (mercit-diff-type)))
    (if (eq (mercit-diff-type (car sections)) 'unstaged)
        (mercit-call-git "checkout" "--" files)
      (when new-files
        (mercit-call-git "add"   "--" new-files)
        (mercit-call-git "reset" "--" new-files))
      (let ((binaries (mercit-binary-files "--cached")))
        (when binaries
          (setq sections
                (--remove (member (oref it value) binaries)
                          sections)))
        (cond ((length= sections 1)
               (mercit-discard-apply (car sections) 'mercit-apply-diff))
              (sections
               (mercit-discard-apply-n sections #'mercit-apply-diffs)))
        (when binaries
          (let ((modified (mercit-unstaged-files t)))
            (setq binaries (--separate (member it modified) binaries)))
          (when (cadr binaries)
            (mercit-call-git "reset" "--" (cadr binaries)))
          (when (car binaries)
            (user-error
             (concat
              "Cannot discard staged changes to binary files, "
              "which also have unstaged changes.  Unstage instead."))))))))

;;;; Reverse

(defun mercit-reverse (&rest args)
  "Reverse the change at point in the working tree.
With a prefix argument fallback to a 3-way merge.  Doing
so causes the change to be applied to the index as well."
  (interactive (and current-prefix-arg (list "--3way")))
  (--when-let (mercit-apply--get-selection)
    (pcase (list (mercit-diff-type) (mercit-diff-scope))
      (`(untracked ,_) (user-error "Cannot reverse untracked changes"))
      (`(unstaged  ,_) (user-error "Cannot reverse unstaged changes"))
      (`(,_    region) (mercit-reverse-region it args))
      (`(,_      hunk) (mercit-reverse-hunk   it args))
      (`(,_     hunks) (mercit-reverse-hunks  it args))
      (`(,_      file) (mercit-reverse-file   it args))
      (`(,_     files) (mercit-reverse-files  it args))
      (`(,_      list) (mercit-reverse-files  it args)))))

(defun mercit-reverse-region (section args)
  (mercit-confirm 'reverse "Reverse region")
  (mercit-reverse-apply section #'mercit-apply-region args))

(defun mercit-reverse-hunk (section args)
  (mercit-confirm 'reverse "Reverse hunk")
  (mercit-reverse-apply section #'mercit-apply-hunk args))

(defun mercit-reverse-hunks (sections args)
  (mercit-confirm 'reverse
    (format "Reverse %s hunks from %s"
            (length sections)
            (mercit-section-parent-value (car sections))))
  (mercit-reverse-apply sections #'mercit-apply-hunks args))

(defun mercit-reverse-file (section args)
  (mercit-reverse-files (list section) args))

(defun mercit-reverse-files (sections args)
  (pcase-let ((`(,binaries ,sections)
               (let ((bs (mercit-binary-files
                          (cond ((derived-mode-p 'mercit-revision-mode)
                                 mercit-buffer-range)
                                ((derived-mode-p 'mercit-diff-mode)
                                 mercit-buffer-range)
                                (t
                                 "--cached")))))
                 (--separate (member (oref it value) bs)
                             sections))))
    (mercit-confirm-files 'reverse (--map (oref it value) sections))
    (cond ((length= sections 1)
           (mercit-reverse-apply (car sections) #'mercit-apply-diff args))
          (sections
           (mercit-reverse-apply sections #'mercit-apply-diffs args)))
    (when binaries
      (user-error "Cannot reverse binary files"))))

(defun mercit-reverse-apply (section:s apply args)
  (funcall apply section:s "--reverse" args
           (and (not mercit-reverse-atomically)
                (not (member "--3way" args))
                "--reject")))

(defun mercit-reverse-in-index (&rest args)
  "Reverse the change at point in the index but not the working tree.

Use this command to extract a change from `HEAD', while leaving
it in the working tree, so that it can later be committed using
a separate commit.  A typical workflow would be:

0. Optionally make sure that there are no uncommitted changes.
1. Visit the `HEAD' commit and navigate to the change that should
   not have been included in that commit.
2. Type \"u\" (`mercit-unstage') to reverse it in the index.
   This assumes that `mercit-unstage-committed-changes' is non-nil.
3. Type \"c e\" to extend `HEAD' with the staged changes,
   including those that were already staged before.
4. Optionally stage the remaining changes using \"s\" or \"S\"
   and then type \"c c\" to create a new commit."
  (interactive)
  (mercit-reverse (cons "--cached" args)))

;;; Smerge Support

(defun mercit-smerge-keep-current ()
  "Keep the current version of the conflict at point."
  (interactive)
  (mercit-call-smerge #'smerge-keep-current))

(defun mercit-smerge-keep-upper ()
  "Keep the upper/our version of the conflict at point."
  (interactive)
  (mercit-call-smerge #'smerge-keep-upper))

(defun mercit-smerge-keep-base ()
  "Keep the base version of the conflict at point."
  (interactive)
  (mercit-call-smerge #'smerge-keep-base))

(defun mercit-smerge-keep-lower ()
  "Keep the lower/their version of the conflict at point."
  (interactive)
  (mercit-call-smerge #'smerge-keep-lower))

(defun mercit-smerge-keep-all ()
  "Keep all versions of the conflict at point."
  (interactive)
  (mercit-call-smerge #'smerge-keep-all))

(defun mercit-call-smerge (fn)
  (pcase-let* ((file (mercit-file-at-point t t))
               (keep (get-file-buffer file))
               (`(,buf ,pos)
                (let ((mercit-diff-visit-jump-to-change nil))
                  (mercit-diff-visit-file--noselect file))))
    (with-current-buffer buf
      (save-excursion
        (save-restriction
          (unless (<= (point-min) pos (point-max))
            (widen))
          (goto-char pos)
          (condition-case nil
              (smerge-match-conflict)
            (error
             (if (eq fn #'smerge-keep-current)
                 (when (eq this-command #'mercit-discard)
                   (re-search-forward smerge-begin-re nil t)
                   (setq fn
                         (mercit-read-char-case "Keep side: " t
                           (?o "[o]urs/upper"   #'smerge-keep-upper)
                           (?b "[b]ase"         #'smerge-keep-base)
                           (?t "[t]heirs/lower" #'smerge-keep-lower))))
               (re-search-forward smerge-begin-re nil t))))
          (funcall fn)))
      (when (and keep (mercit-anything-unmerged-p file))
        (smerge-start-session))
      (save-buffer))
    (unless keep
      (kill-buffer buf))
    (mercit-refresh)))

;;; _
(provide 'mercit-apply)
;;; mercit-apply.el ends here
