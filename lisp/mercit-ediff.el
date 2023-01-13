;;; mercit-ediff.el --- Ediff extension for Magit  -*- lexical-binding:t -*-

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

;; This library provides basic support for Ediff.

;;; Code:

(require 'mercit)

(require 'ediff)
(require 'smerge-mode)

(defvar smerge-ediff-buf)
(defvar smerge-ediff-windows)

;;; Options

(defgroup mercit-ediff nil
  "Ediff support for Magit."
  :link '(info-link "(mercit)Ediffing")
  :group 'mercit-extensions)

(defcustom mercit-ediff-quit-hook
  '(mercit-ediff-cleanup-auxiliary-buffers
    mercit-ediff-restore-previous-winconf)
  "Hooks to run after finishing Ediff, when that was invoked using Magit.
The hooks are run in the Ediff control buffer.  This is similar
to `ediff-quit-hook' but takes the needs of Magit into account.
The `ediff-quit-hook' is ignored by Ediff sessions which were
invoked using Magit."
  :package-version '(mercit . "2.2.0")
  :group 'mercit-ediff
  :type 'hook
  :get #'mercit-hook-custom-get
  :options '(mercit-ediff-cleanup-auxiliary-buffers
             mercit-ediff-restore-previous-winconf))

(defcustom mercit-ediff-dwim-resolve-function #'mercit-ediff-resolve-rest
  "The function `mercit-ediff-dwim' uses to resolve conflicts."
  :package-version '(mercit . "3.4.0")
  :group 'mercit-ediff
  :type '(choice (const mercit-ediff-resolve-rest)
                 (const mercit-ediff-resolve-all)
                 (const mercit-git-mergetool)))

(defcustom mercit-ediff-dwim-show-on-hunks nil
  "Whether `mercit-ediff-dwim' runs show variants on hunks.
If non-nil, `mercit-ediff-show-staged' or
`mercit-ediff-show-unstaged' are called based on what section the
hunk is in.  Otherwise, `mercit-ediff-dwim' runs
`mercit-ediff-stage' when point is on an uncommitted hunk."
  :package-version '(mercit . "2.2.0")
  :group 'mercit-ediff
  :type 'boolean)

(defcustom mercit-ediff-show-stash-with-index t
  "Whether `mercit-ediff-show-stash' shows the state of the index.

If non-nil, use a third Ediff buffer to distinguish which changes
in the stash were staged.  In cases where the stash contains no
staged changes, fall back to a two-buffer Ediff.

More specifically, a stash is a merge commit, stash@{N}, with
potentially three parents.

* stash@{N}^1 represents the `HEAD' commit at the time the stash
  was created.

* stash@{N}^2 records any changes that were staged when the stash
  was made.

* stash@{N}^3, if it exists, contains files that were untracked
  when stashing.

If this option is non-nil, `mercit-ediff-show-stash' will run
Ediff on a file using three buffers: one for stash@{N}, another
for stash@{N}^1, and a third for stash@{N}^2.

Otherwise, Ediff uses two buffers, comparing
stash@{N}^1..stash@{N}.  Along with any unstaged changes, changes
in the index commit, stash@{N}^2, will be shown in this
comparison unless they conflicted with changes in the working
tree at the time of stashing."
  :package-version '(mercit . "2.6.0")
  :group 'mercit-ediff
  :type 'boolean)

(defvar mercit-ediff-use-indirect-buffers nil
  "Whether to use indirect buffers.
Ediff already does a lot of buffer and file shuffling and I
recommend you do not further complicate that by enabling this.")

;;; Commands

(defvar mercit-ediff-previous-winconf nil)

;;;###autoload (autoload 'mercit-ediff "mercit-ediff" nil)
(transient-define-prefix mercit-ediff ()
  "Show differences using the Ediff package."
  :info-manual "(ediff)"
  ["Ediff"
   [("E" "Dwim"          mercit-ediff-dwim)
    ("s" "Stage"         mercit-ediff-stage)]
   [("m" "Resolve rest"            mercit-ediff-resolve-rest)
    ("M" "Resolve all conflicts"   mercit-ediff-resolve-all)
    ("t" "Resolve using mergetool" mercit-git-mergetool)]
   [("u" "Show unstaged" mercit-ediff-show-unstaged)
    ("i" "Show staged"   mercit-ediff-show-staged)
    ("w" "Show worktree" mercit-ediff-show-working-tree)]
   [("c" "Show commit"   mercit-ediff-show-commit)
    ("r" "Show range"    mercit-ediff-compare)
    ("z" "Show stash"    mercit-ediff-show-stash)]])

(defmacro mercit-ediff-buffers (a b &optional c setup quit file)
  "Run Ediff on two or three buffers.
This is a wrapper around `ediff-buffers-internal'.

A, B and C have the form (GET-BUFFER CREATE-BUFFER).  If
GET-BUFFER returns a non-nil value, then that buffer is used and
it is not killed when exiting Ediff.  Otherwise CREATE-BUFFER
must return a buffer and that is killed when exiting Ediff.

If non-nil, SETUP must be a function.  It is called without
arguments after Ediff is done setting up buffers.

If non-nil, QUIT must be a function.  It is added to
`ediff-quit-hook' and is called without arguments.

If FILE is non-nil, then perform a merge.  The merge result
is put in FILE."
  (let (get make kill (char ?A))
    (dolist (spec (list a b c))
      (if (not spec)
          (push nil make)
        (pcase-let ((`(,g ,m) spec))
          (let ((b (intern (format "buf%c" char))))
            (push `(,b ,g) get)
            ;; This is an unfortunate complication that I have added for
            ;; the benefit of one user.  Pretend we used this instead:
            ;; (push `(or ,b ,m) make)
            (push `(if ,b
                       (if mercit-ediff-use-indirect-buffers
                           (prog1 (make-indirect-buffer
                                   ,b
                                   (generate-new-buffer-name (buffer-name ,b))
                                   t)
                             (setq ,b nil))
                         ,b)
                     ,m)
                  make)
            (push `(unless ,b
                     ;; For merge jobs Ediff switches buffer names around.
                     ;; See (if ediff-merge-job ...) in `ediff-setup'.
                     (let ((var ,(if (and file (= char ?C))
                                     'ediff-ancestor-buffer
                                   (intern (format "ediff-buffer-%c" char)))))
                       (ediff-kill-buffer-carefully var)))
                  kill))
          (cl-incf char))))
    (setq get  (nreverse get))
    (setq make (nreverse make))
    (setq kill (nreverse kill))
    (let ((mconf (cl-gensym "conf"))
          (mfile (cl-gensym "file")))
      `(mercit-with-toplevel
         (let ((,mconf (current-window-configuration))
               (,mfile ,file)
               ,@get)
           (ediff-buffers-internal
            ,@make
            (list ,@(and setup (list setup))
                  (lambda ()
                    ;; We do not want to kill buffers that existed before
                    ;; Ediff was invoked, so we cannot use Ediff's default
                    ;; quit functions.  Ediff splits quitting across two
                    ;; hooks for merge jobs but we only ever use one.
                    (setq-local ediff-quit-merge-hook nil)
                    (setq-local ediff-quit-hook
                                (list
                                 ,@(and quit (list quit))
                                 (lambda ()
                                   ,@kill
                                   (let ((mercit-ediff-previous-winconf ,mconf))
                                     (run-hooks 'mercit-ediff-quit-hook)))))))
            (pcase (list ,(and c t) (and ,mfile t))
              ('(nil nil) 'ediff-buffers)
              ('(nil t)   'ediff-merge-buffers)
              ('(t   nil) 'ediff-buffers3)
              ('(t   t)   'ediff-merge-buffers-with-ancestor))
            ,mfile))))))

;;;###autoload
(defun mercit-ediff-resolve-all (file)
  "Resolve all conflicts in the FILE at point using Ediff.

If there is no file at point or if it doesn't have any unmerged
changes, then prompt for a file.

See info node `(mercit) Ediffing' for more information about this
and alternative commands."
  (interactive (list (mercit-read-unmerged-file)))
  (mercit-with-toplevel
    (let* ((revA  (or (mercit-name-branch "HEAD")
                      (mercit-commit-p "HEAD")))
           (revB  (cl-find-if (lambda (head) (file-exists-p (mercit-git-dir head)))
                              '("MERGE_HEAD" "CHERRY_PICK_HEAD" "REVERT_HEAD")))
           (revB  (or (mercit-name-branch revB)
                      (mercit-commit-p revB)))
           (revC  (mercit-commit-p (mercit-git-string "merge-base" revA revB)))
           (fileA (mercit--rev-file-name file revA revB))
           (fileB (mercit--rev-file-name file revB revA))
           (fileC (or (mercit--rev-file-name file revC revA)
                      (mercit--rev-file-name file revC revB))))
      ;; Ediff assumes that the FILE where it is going to store the merge
      ;; result does not exist yet, so move the existing file out of the
      ;; way.  If a buffer visits FILE, then we have to kill that upfront.
      (when-let ((buffer (find-buffer-visiting file)))
        (when (and (buffer-modified-p buffer)
                   (not (y-or-n-p (format "Save buffer %s %s? "
                                          (buffer-name buffer)
                                          "(cannot continue otherwise)"))))
          (user-error "Abort"))
        (kill-buffer buffer))
      (let ((orig (concat file ".ORIG")))
        (when (file-exists-p orig)
          (rename-file orig (make-temp-name (concat orig "_"))))
        (rename-file file orig))
      (let ((setup (lambda ()
                     ;; Use the same conflict marker style as Git uses.
                     (setq-local ediff-combination-pattern
                                 '("<<<<<<< HEAD" A
                                   ,(format "||||||| %s" revC) Ancestor
                                   "=======" B
                                   ,(format ">>>>>>> %s" revB)))))
            (quit  (lambda ()
                     ;; For merge jobs Ediff switches buffer names around.
                     ;; At this point `ediff-buffer-C' no longer refer to
                     ;; the ancestor buffer but to the merge result buffer.
                     ;; See (if ediff-merge-job ...) in `ediff-setup'.
                     (when (buffer-live-p ediff-buffer-C)
                       (with-current-buffer ediff-buffer-C
                         (save-buffer)
                         (save-excursion
                           (goto-char (point-min))
                           (unless (re-search-forward "^<<<<<<< " nil t)
                             (mercit-stage-file file))))))))
        (if fileC
            (mercit-ediff-buffers
             ((mercit-get-revision-buffer revA fileA)
              (mercit-find-file-noselect  revA fileA))
             ((mercit-get-revision-buffer revB fileB)
              (mercit-find-file-noselect  revB fileB))
             ((mercit-get-revision-buffer revC fileC)
              (mercit-find-file-noselect  revC fileC))
             setup quit file)
          (mercit-ediff-buffers
           ((mercit-get-revision-buffer revA fileA)
            (mercit-find-file-noselect  revA fileA))
           ((mercit-get-revision-buffer revB fileB)
            (mercit-find-file-noselect  revB fileB))
           nil setup quit file))))))

;;;###autoload
(defun mercit-ediff-resolve-rest (file)
  "Resolve outstanding conflicts in the FILE at point using Ediff.

If there is no file at point or if it doesn't have any unmerged
changes, then prompt for a file.

See info node `(mercit) Ediffing' for more information about this
and alternative commands."
  (interactive (list (mercit-read-unmerged-file)))
  (mercit-with-toplevel
    (with-current-buffer (find-file-noselect file)
      (smerge-ediff)
      (setq-local
       ediff-quit-hook
       (lambda ()
         (let ((bufC ediff-buffer-C)
               (bufS smerge-ediff-buf))
           (with-current-buffer bufS
             (when (yes-or-no-p (format "Conflict resolution finished; save %s? "
                                        buffer-file-name))
               (erase-buffer)
               (insert-buffer-substring bufC)
               (save-buffer))))
         (when (buffer-live-p ediff-buffer-A) (kill-buffer ediff-buffer-A))
         (when (buffer-live-p ediff-buffer-B) (kill-buffer ediff-buffer-B))
         (when (buffer-live-p ediff-buffer-C) (kill-buffer ediff-buffer-C))
         (when (buffer-live-p ediff-ancestor-buffer)
           (kill-buffer ediff-ancestor-buffer))
         (let ((mercit-ediff-previous-winconf smerge-ediff-windows))
           (run-hooks 'mercit-ediff-quit-hook)))))))

;;;###autoload
(defun mercit-ediff-stage (file)
  "Stage and unstage changes to FILE using Ediff.
FILE has to be relative to the top directory of the repository."
  (interactive
   (let ((files (mercit-tracked-files)))
     (list (mercit-completing-read "Selectively stage file" files nil t nil nil
                                  (car (member (mercit-current-file) files))))))
  (mercit-with-toplevel
    (let* ((bufA  (mercit-get-revision-buffer "HEAD" file))
           (bufB  (mercit-get-revision-buffer "{index}" file))
           (lockB (and bufB (buffer-local-value 'buffer-read-only bufB)))
           (bufC  (get-file-buffer file))
           ;; Use the same encoding for all three buffers or we
           ;; may end up changing the file in an unintended way.
           (bufC* (or bufC (find-file-noselect file)))
           (coding-system-for-read
            (buffer-local-value 'buffer-file-coding-system bufC*))
           (bufA* (mercit-find-file-noselect-1 "HEAD" file t))
           (bufB* (mercit-find-file-index-noselect file t)))
      (with-current-buffer bufB* (setq buffer-read-only nil))
      (mercit-ediff-buffers
       (bufA bufA*)
       (bufB bufB*)
       (bufC bufC*)
       nil
       (lambda ()
         (when (buffer-live-p ediff-buffer-B)
           (when lockB
             (with-current-buffer bufB (setq buffer-read-only t)))
           (when (buffer-modified-p ediff-buffer-B)
             (with-current-buffer ediff-buffer-B
               (mercit-update-index))))
         (when (and (buffer-live-p ediff-buffer-C)
                    (buffer-modified-p ediff-buffer-C))
           (with-current-buffer ediff-buffer-C
             (when (y-or-n-p (format "Save file %s? " buffer-file-name))
               (save-buffer)))))))))

;;;###autoload
(defun mercit-ediff-compare (revA revB fileA fileB)
  "Compare REVA:FILEA with REVB:FILEB using Ediff.

FILEA and FILEB have to be relative to the top directory of the
repository.  If REVA or REVB is nil, then this stands for the
working tree state.

If the region is active, use the revisions on the first and last
line of the region.  With a prefix argument, instead of diffing
the revisions, choose a revision to view changes along, starting
at the common ancestor of both revisions (i.e., use a \"...\"
range)."
  (interactive
   (pcase-let ((`(,revA ,revB) (mercit-ediff-compare--read-revisions
                                nil current-prefix-arg)))
     (nconc (list revA revB)
            (mercit-ediff-read-files revA revB))))
  (mercit-ediff-buffers
   ((if revA (mercit-get-revision-buffer revA fileA) (get-file-buffer    fileA))
    (if revA (mercit-find-file-noselect  revA fileA) (find-file-noselect fileA)))
   ((if revB (mercit-get-revision-buffer revB fileB) (get-file-buffer    fileB))
    (if revB (mercit-find-file-noselect  revB fileB) (find-file-noselect fileB)))))

(defun mercit-ediff-compare--read-revisions (&optional arg mbase)
  (let ((input (or arg (mercit-diff-read-range-or-commit
                        "Compare range or commit"
                        nil mbase))))
    (--if-let (mercit-split-range input)
        (-cons-to-list it)
      (list input nil))))

(defun mercit-ediff-read-files (revA revB &optional fileB)
  "Read file in REVB, return it and the corresponding file in REVA.
When FILEB is non-nil, use this as REVB's file instead of
prompting for it."
  (unless (and fileB (member fileB (mercit-revision-files revB)))
    (setq fileB
          (or (and fileB
                   mercit-buffer-log-files
                   (derived-mode-p 'mercit-log-mode)
                   (member "--follow" mercit-buffer-log-args)
                   (cdr (assoc fileB
                               (mercit-renamed-files
                                revB
                                (oref (car (oref mercit-root-section children))
                                      value)))))
              (mercit-read-file-choice
               (format "File to compare between %s and %s"
                       revA (or revB "the working tree"))
               (mercit-changed-files revA revB)
               (format "No changed files between %s and %s"
                       revA (or revB "the working tree"))))))
  (list (or (car (member fileB (mercit-revision-files revA)))
            (cdr (assoc fileB (mercit-renamed-files revB revA)))
            (mercit-read-file-choice
             (format "File in %s to compare with %s in %s"
                     revA fileB (or revB "the working tree"))
             (mercit-changed-files revB revA)
             (format "No files have changed between %s and %s"
                     revA revB)))
        fileB))

;;;###autoload
(defun mercit-ediff-dwim ()
  "Compare, stage, or resolve using Ediff.
This command tries to guess what file, and what commit or range
the user wants to compare, stage, or resolve using Ediff.  It
might only be able to guess either the file, or range or commit,
in which case the user is asked about the other.  It might not
always guess right, in which case the appropriate `mercit-ediff-*'
command has to be used explicitly.  If it cannot read the user's
mind at all, then it asks the user for a command to run."
  (interactive)
  (mercit-section-case
    (hunk (save-excursion
            (goto-char (oref (oref it parent) start))
            (mercit-ediff-dwim)))
    (t
     (let ((range (mercit-diff--dwim))
           (file (mercit-current-file))
           command revA revB)
       (pcase range
         ((and (guard (not mercit-ediff-dwim-show-on-hunks))
               (or 'unstaged 'staged))
          (setq command (if (mercit-anything-unmerged-p)
                            mercit-ediff-dwim-resolve-function
                          #'mercit-ediff-stage)))
         ('unstaged (setq command #'mercit-ediff-show-unstaged))
         ('staged (setq command #'mercit-ediff-show-staged))
         (`(commit . ,value)
          (setq command #'mercit-ediff-show-commit)
          (setq revB value))
         (`(stash . ,value)
          (setq command #'mercit-ediff-show-stash)
          (setq revB value))
         ((pred stringp)
          (pcase-let ((`(,a ,b) (mercit-ediff-compare--read-revisions range)))
            (setq command #'mercit-ediff-compare)
            (setq revA a)
            (setq revB b)))
         (_
          (when (derived-mode-p 'mercit-diff-mode)
            (pcase (mercit-diff-type)
              ('committed (pcase-let ((`(,a ,b)
                                       (mercit-ediff-compare--read-revisions
                                        mercit-buffer-range)))
                            (setq revA a)
                            (setq revB b)))
              ((guard (not mercit-ediff-dwim-show-on-hunks))
               (setq command #'mercit-ediff-stage))
              ('unstaged  (setq command #'mercit-ediff-show-unstaged))
              ('staged    (setq command #'mercit-ediff-show-staged))
              ('undefined (setq command nil))
              (_          (setq command nil))))))
       (cond ((not command)
              (call-interactively
               (mercit-read-char-case
                   "Failed to read your mind; do you want to " t
                 (?c "[c]ommit"  #'mercit-ediff-show-commit)
                 (?r "[r]ange"   #'mercit-ediff-compare)
                 (?s "[s]tage"   #'mercit-ediff-stage)
                 (?m "[m] resolve remaining conflicts"
                     #'mercit-ediff-resolve-rest)
                 (?M "[M] resolve all conflicts"
                     #'mercit-ediff-resolve-all))))
             ((eq command #'mercit-ediff-compare)
              (apply #'mercit-ediff-compare revA revB
                     (mercit-ediff-read-files revA revB file)))
             ((eq command #'mercit-ediff-show-commit)
              (mercit-ediff-show-commit revB))
             ((eq command #'mercit-ediff-show-stash)
              (mercit-ediff-show-stash revB))
             (file
              (funcall command file))
             (t
              (call-interactively command)))))))

;;;###autoload
(defun mercit-ediff-show-staged (file)
  "Show staged changes using Ediff.

This only allows looking at the changes; to stage, unstage,
and discard changes using Ediff, use `mercit-ediff-stage'.

FILE must be relative to the top directory of the repository."
  (interactive
   (list (mercit-read-file-choice "Show staged changes for file"
                                 (mercit-staged-files)
                                 "No staged files")))
  (mercit-ediff-buffers ((mercit-get-revision-buffer "HEAD" file)
                        (mercit-find-file-noselect "HEAD" file))
                       ((get-buffer (concat file ".~{index}~"))
                        (mercit-find-file-index-noselect file t))))

;;;###autoload
(defun mercit-ediff-show-unstaged (file)
  "Show unstaged changes using Ediff.

This only allows looking at the changes; to stage, unstage,
and discard changes using Ediff, use `mercit-ediff-stage'.

FILE must be relative to the top directory of the repository."
  (interactive
   (list (mercit-read-file-choice "Show unstaged changes for file"
                                 (mercit-unstaged-files)
                                 "No unstaged files")))
  (mercit-ediff-buffers ((get-buffer (concat file ".~{index}~"))
                        (mercit-find-file-index-noselect file t))
                       ((get-file-buffer file)
                        (find-file-noselect file))))

;;;###autoload
(defun mercit-ediff-show-working-tree (file)
  "Show changes between `HEAD' and working tree using Ediff.
FILE must be relative to the top directory of the repository."
  (interactive
   (list (mercit-read-file-choice "Show changes in file"
                                 (mercit-changed-files "HEAD")
                                 "No changed files")))
  (mercit-ediff-buffers ((mercit-get-revision-buffer "HEAD" file)
                        (mercit-find-file-noselect  "HEAD" file))
                       ((get-file-buffer file)
                        (find-file-noselect file))))

;;;###autoload
(defun mercit-ediff-show-commit (commit)
  "Show changes introduced by COMMIT using Ediff."
  (interactive (list (mercit-read-branch-or-commit "Revision")))
  (let ((revA (concat commit "^"))
        (revB commit))
    (apply #'mercit-ediff-compare
           revA revB
           (mercit-ediff-read-files revA revB (mercit-current-file)))))

;;;###autoload
(defun mercit-ediff-show-stash (stash)
  "Show changes introduced by STASH using Ediff.
`mercit-ediff-show-stash-with-index' controls whether a
three-buffer Ediff is used in order to distinguish changes in the
stash that were staged."
  (interactive (list (mercit-read-stash "Stash")))
  (pcase-let* ((revA (concat stash "^1"))
               (revB (concat stash "^2"))
               (revC stash)
               (`(,fileA ,fileC) (mercit-ediff-read-files revA revC))
               (fileB fileC))
    (if (and mercit-ediff-show-stash-with-index
             (member fileA (mercit-changed-files revB revA)))
        (mercit-ediff-buffers
         ((mercit-get-revision-buffer revA fileA)
          (mercit-find-file-noselect  revA fileA))
         ((mercit-get-revision-buffer revB fileB)
          (mercit-find-file-noselect  revB fileB))
         ((mercit-get-revision-buffer revC fileC)
          (mercit-find-file-noselect  revC fileC)))
      (mercit-ediff-compare revA revC fileA fileC))))

(defun mercit-ediff-cleanup-auxiliary-buffers ()
  (let* ((ctl-buf ediff-control-buffer)
         (ctl-win (ediff-get-visible-buffer-window ctl-buf))
         (ctl-frm ediff-control-frame)
         (main-frame (cond ((window-live-p ediff-window-A)
                            (window-frame ediff-window-A))
                           ((window-live-p ediff-window-B)
                            (window-frame ediff-window-B)))))
    (ediff-kill-buffer-carefully ediff-diff-buffer)
    (ediff-kill-buffer-carefully ediff-custom-diff-buffer)
    (ediff-kill-buffer-carefully ediff-fine-diff-buffer)
    (ediff-kill-buffer-carefully ediff-tmp-buffer)
    (ediff-kill-buffer-carefully ediff-error-buffer)
    (ediff-kill-buffer-carefully ediff-msg-buffer)
    (ediff-kill-buffer-carefully ediff-debug-buffer)
    (when (boundp 'ediff-patch-diagnostics)
      (ediff-kill-buffer-carefully ediff-patch-diagnostics))
    (cond ((and (display-graphic-p)
                (frame-live-p ctl-frm))
           (delete-frame ctl-frm))
          ((window-live-p ctl-win)
           (delete-window ctl-win)))
    (ediff-kill-buffer-carefully ctl-buf)
    (when (frame-live-p main-frame)
      (select-frame main-frame))))

(defun mercit-ediff-restore-previous-winconf ()
  (set-window-configuration mercit-ediff-previous-winconf))

;;; _
(provide 'mercit-ediff)
;;; mercit-ediff.el ends here
