;;; mercit-files.el --- Finding files  -*- lexical-binding:t -*-

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

;; This library implements support for finding blobs, staged files,
;; and Git configuration files.  It also implements modes useful in
;; buffers visiting files and blobs, and the commands used by those
;; modes.

;;; Code:

(require 'mercit)

;;; Find Blob

(defvar mercit-find-file-hook nil)
(add-hook 'mercit-find-file-hook #'mercit-blob-mode)

;;;###autoload
(defun mercit-find-file (rev file)
  "View FILE from REV.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go
to the line and column corresponding to that location."
  (interactive (mercit-find-file-read-args "Find file"))
  (mercit-find-file--internal rev file #'pop-to-buffer-same-window))

;;;###autoload
(defun mercit-find-file-other-window (rev file)
  "View FILE from REV, in another window.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go to
the line and column corresponding to that location."
  (interactive (mercit-find-file-read-args "Find file in other window"))
  (mercit-find-file--internal rev file #'switch-to-buffer-other-window))

;;;###autoload
(defun mercit-find-file-other-frame (rev file)
  "View FILE from REV, in another frame.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go to
the line and column corresponding to that location."
  (interactive (mercit-find-file-read-args "Find file in other frame"))
  (mercit-find-file--internal rev file #'switch-to-buffer-other-frame))

(defun mercit-find-file-read-args (prompt)
  (let ((pseudo-revs '("{worktree}" "{index}")))
    (if-let ((rev (mercit-completing-read "Find file from revision"
                                         (append pseudo-revs
                                                 (mercit-list-refnames nil t))
                                         nil nil nil 'mercit-revision-history
                                         (or (mercit-branch-or-commit-at-point)
                                             (mercit-get-current-branch)))))
        (list rev (mercit-read-file-from-rev (if (member rev pseudo-revs)
                                                "HEAD"
                                              rev)
                                            prompt))
      (user-error "Nothing selected"))))

(defun mercit-find-file--internal (rev file fn)
  (let ((buf (mercit-find-file-noselect rev file))
        line col)
    (when-let ((visited-file (mercit-file-relative-name)))
      (setq line (line-number-at-pos))
      (setq col (current-column))
      (cond
       ((not (equal visited-file file)))
       ((equal mercit-buffer-revision rev))
       ((equal rev "{worktree}")
        (setq line (mercit-diff-visit--offset file mercit-buffer-revision line)))
       ((equal rev "{index}")
        (setq line (mercit-diff-visit--offset file nil line)))
       (mercit-buffer-revision
        (setq line (mercit-diff-visit--offset
                    file (concat mercit-buffer-revision ".." rev) line)))
       (t
        (setq line (mercit-diff-visit--offset file (list "-R" rev) line)))))
    (funcall fn buf)
    (when line
      (with-current-buffer buf
        (widen)
        (goto-char (point-min))
        (forward-line (1- line))
        (move-to-column col)))
    buf))

(defun mercit-find-file-noselect (rev file)
  "Read FILE from REV into a buffer and return the buffer.
REV is a revision or one of \"{worktree}\" or \"{index}\".
FILE must be relative to the top directory of the repository."
  (mercit-find-file-noselect-1 rev file))

(defun mercit-find-file-noselect-1 (rev file &optional revert)
  "Read FILE from REV into a buffer and return the buffer.
REV is a revision or one of \"{worktree}\" or \"{index}\".
FILE must be relative to the top directory of the repository.
Non-nil REVERT means to revert the buffer.  If `ask-revert',
then only after asking.  A non-nil value for REVERT is ignored if REV is
\"{worktree}\"."
  (if (equal rev "{worktree}")
      (find-file-noselect (expand-file-name file (mercit-toplevel)))
    (let ((topdir (mercit-toplevel)))
      (when (file-name-absolute-p file)
        (setq file (file-relative-name file topdir)))
      (with-current-buffer (mercit-get-revision-buffer-create rev file)
        (when (or (not mercit-buffer-file-name)
                  (if (eq revert 'ask-revert)
                      (y-or-n-p (format "%s already exists; revert it? "
                                        (buffer-name))))
                  revert)
          (setq mercit-buffer-revision
                (if (equal rev "{index}")
                    "{index}"
                  (mercit-rev-format "{node}" rev)))
          (setq mercit-buffer-refname rev)
          (setq mercit-buffer-file-name (expand-file-name file topdir))
          (setq default-directory
                (let ((dir (file-name-directory mercit-buffer-file-name)))
                  (if (file-exists-p dir) dir topdir)))
          (setq-local revert-buffer-function #'mercit-revert-rev-file-buffer)
          (revert-buffer t t)
          (run-hooks (if (equal rev "{index}")
                         'mercit-find-index-hook
                       'mercit-find-file-hook)))
        (current-buffer)))))

(defun mercit-get-revision-buffer-create (rev file)
  (mercit-get-revision-buffer rev file t))

(defun mercit-get-revision-buffer (rev file &optional create)
  (funcall (if create #'get-buffer-create #'get-buffer)
           (format "%s.~%s~" file (subst-char-in-string ?/ ?_ rev))))

(defun mercit-revert-rev-file-buffer (_ignore-auto noconfirm)
  (when (or noconfirm
            (and (not (buffer-modified-p))
                 (catch 'found
                   (dolist (regexp revert-without-query)
                     (when (string-match regexp mercit-buffer-file-name)
                       (throw 'found t)))))
            (yes-or-no-p (format "Revert buffer from Git %s? "
                                 (if (equal mercit-buffer-refname "{index}")
                                     "index"
                                   (concat "revision " mercit-buffer-refname)))))
    (let* ((inhibit-read-only t)
           (default-directory (mercit-toplevel))
           (file (file-relative-name mercit-buffer-file-name))
           (coding-system-for-read (or coding-system-for-read 'undecided)))
      (erase-buffer)
      (mercit-git-insert "cat-file" "-p"
                        (if (equal mercit-buffer-refname "{index}")
                            (concat ":" file)
                          (concat mercit-buffer-refname ":" file)))
      (setq buffer-file-coding-system last-coding-system-used))
    (let ((buffer-file-name mercit-buffer-file-name)
          (after-change-major-mode-hook
           (remq 'global-diff-hl-mode-enable-in-buffers
                 after-change-major-mode-hook)))
      (delay-mode-hooks
        (normal-mode t)))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (goto-char (point-min))))

;;; Find Index

(defvar mercit-find-index-hook nil)

(defun mercit-find-file-index-noselect (file &optional revert)
  "Read FILE from the index into a buffer and return the buffer.
FILE must to be relative to the top directory of the repository."
  (mercit-find-file-noselect-1 "{index}" file (or revert 'ask-revert)))

(defun mercit-update-index ()
  "Update the index with the contents of the current buffer.
The current buffer has to be visiting a file in the index, which
is done using `mercit-find-index-noselect'."
  (interactive)
  (let ((file (mercit-file-relative-name)))
    (unless (equal mercit-buffer-refname "{index}")
      (user-error "%s isn't visiting the index" file))
    (if (y-or-n-p (format "Update index with contents of %s" (buffer-name)))
        (let ((index (make-temp-name (mercit-git-dir "mercit-update-index-")))
              (buffer (current-buffer)))
          (when mercit-wip-before-change-mode
            (mercit-wip-commit-before-change (list file) " before un-/stage"))
          (unwind-protect
              (progn
                (let ((coding-system-for-write buffer-file-coding-system))
                  (with-temp-file index
                    (insert-buffer-substring buffer)))
                (mercit-with-toplevel
                  (mercit-call-git
                   "update-index" "--cacheinfo"
                   (substring (mercit-git-string "ls-files" "-s" file)
                              0 6)
                   (mercit-git-string "hash-object" "-t" "blob" "-w"
                                     (concat "--path=" file)
                                     "--" (mercit-convert-filename-for-git index))
                   file)))
            (ignore-errors (delete-file index)))
          (set-buffer-modified-p nil)
          (when mercit-wip-after-apply-mode
            (mercit-wip-commit-after-apply (list file) " after un-/stage")))
      (message "Abort")))
  (--when-let (mercit-get-mode-buffer 'mercit-status-mode)
    (with-current-buffer it (mercit-refresh)))
  t)

;;; Find Config File

(defun mercit-find-git-config-file (filename &optional wildcards)
  "Edit a file located in the current repository's git directory.

When \".git\", located at the root of the working tree, is a
regular file, then that makes it cumbersome to open a file
located in the actual git directory.

This command is like `find-file', except that it temporarily
binds `default-directory' to the actual git directory, while
reading the FILENAME."
  (interactive
   (let ((default-directory (mercit-git-dir)))
     (find-file-read-args "Find file: "
                          (confirm-nonexistent-file-or-buffer))))
  (find-file filename wildcards))

(defun mercit-find-git-config-file-other-window (filename &optional wildcards)
  "Edit a file located in the current repo's git directory, in another window.

When \".git\", located at the root of the working tree, is a
regular file, then that makes it cumbersome to open a file
located in the actual git directory.

This command is like `find-file-other-window', except that it
temporarily binds `default-directory' to the actual git
directory, while reading the FILENAME."
  (interactive
   (let ((default-directory (mercit-git-dir)))
     (find-file-read-args "Find file in other window: "
                          (confirm-nonexistent-file-or-buffer))))
  (find-file-other-window filename wildcards))

(defun mercit-find-git-config-file-other-frame (filename &optional wildcards)
  "Edit a file located in the current repo's git directory, in another frame.

When \".git\", located at the root of the working tree, is a
regular file, then that makes it cumbersome to open a file
located in the actual git directory.

This command is like `find-file-other-frame', except that it
temporarily binds `default-directory' to the actual git
directory, while reading the FILENAME."
  (interactive
   (let ((default-directory (mercit-git-dir)))
     (find-file-read-args "Find file in other frame: "
                          (confirm-nonexistent-file-or-buffer))))
  (find-file-other-frame filename wildcards))

;;; File Dispatch

;;;###autoload (autoload 'mercit-file-dispatch "mercit" nil t)
(transient-define-prefix mercit-file-dispatch ()
  "Invoke a Mercit command that acts on the visited file.
When invoked outside a file-visiting buffer, then fall back
to `mercit-dispatch'."
  :info-manual "(mercit) Minor Mode for Buffers Visiting Files"
  ["Actions"
   [("s" "Stage"      mercit-stage-file)
    ("u" "Unstage"    mercit-unstage-file)
    ("c" "Commit"     mercit-commit)
    ("e" "Edit line"  mercit-edit-line-commit)]
   [("D" "Diff..."    mercit-diff)
    ("d" "Diff"       mercit-diff-buffer-file)
    ("g" "Status"     mercit-status-here)]
   [("L" "Log..."     mercit-log)
    ("l" "Log"        mercit-log-buffer-file)
    ("t" "Trace"      mercit-log-trace-definition)
    (7 "M" "Merged"   mercit-log-merged)]
   [("B" "Blame..."   mercit-blame)
    ("b" "Blame"      mercit-blame-addition)
    ("r" "...removal" mercit-blame-removal)
    ("f" "...reverse" mercit-blame-reverse)
    ("m" "Blame echo" mercit-blame-echo)
    ("q" "Quit blame" mercit-blame-quit)]
   [("p" "Prev blob"  mercit-blob-previous)
    ("n" "Next blob"  mercit-blob-next)
    ("v" "Goto blob"  mercit-find-file)
    ("V" "Goto file"  mercit-blob-visit-file)]
   [(5 "C-c r" "Rename file"   mercit-file-rename)
    (5 "C-c d" "Delete file"   mercit-file-delete)
    (5 "C-c u" "Untrack file"  mercit-file-untrack)
    (5 "C-c c" "Checkout file" mercit-file-checkout)]]
  (interactive)
  (transient-setup
   (if (mercit-file-relative-name)
       'mercit-file-dispatch
     'mercit-dispatch)))

;;; Blob Mode

(defvar mercit-blob-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "p" #'mercit-blob-previous)
    (define-key map "n" #'mercit-blob-next)
    (define-key map "b" #'mercit-blame-addition)
    (define-key map "r" #'mercit-blame-removal)
    (define-key map "f" #'mercit-blame-reverse)
    (define-key map "q" #'mercit-kill-this-buffer)
    map)
  "Keymap for `mercit-blob-mode'.")

(define-minor-mode mercit-blob-mode
  "Enable some Mercit features in blob-visiting buffers.

Currently this only adds the following key bindings.
\n\\{mercit-blob-mode-map}"
  :package-version '(mercit . "2.3.0"))

(defun mercit-blob-next ()
  "Visit the next blob which modified the current file."
  (interactive)
  (if mercit-buffer-file-name
      (mercit-blob-visit (or (mercit-blob-successor mercit-buffer-revision
                                                  mercit-buffer-file-name)
                            mercit-buffer-file-name))
    (if (buffer-file-name (buffer-base-buffer))
        (user-error "You have reached the end of time")
      (user-error "Buffer isn't visiting a file or blob"))))

(defun mercit-blob-previous ()
  "Visit the previous blob which modified the current file."
  (interactive)
  (if-let ((file (or mercit-buffer-file-name
                     (buffer-file-name (buffer-base-buffer)))))
      (--if-let (mercit-blob-ancestor mercit-buffer-revision file)
          (mercit-blob-visit it)
        (user-error "You have reached the beginning of time"))
    (user-error "Buffer isn't visiting a file or blob")))

;;;###autoload
(defun mercit-blob-visit-file ()
  "View the file from the worktree corresponding to the current blob.
When visiting a blob or the version from the index, then go to
the same location in the respective file in the working tree."
  (interactive)
  (if-let ((file (mercit-file-relative-name)))
      (mercit-find-file--internal "{worktree}" file #'pop-to-buffer-same-window)
    (user-error "Not visiting a blob")))

(defun mercit-blob-visit (blob-or-file)
  (if (stringp blob-or-file)
      (find-file blob-or-file)
    (pcase-let ((`(,rev ,file) blob-or-file))
      (mercit-find-file rev file)
      (apply #'message "%s (%s %s ago)"
             (mercit-rev-format "{desc|firstline}" rev)
             (mercit--age (mercit-rev-format "%ct" rev))))))

(defun mercit-blob-ancestor (rev file)
  (let ((lines (mercit-with-toplevel
                 (mercit-git-lines "log" "-2" "--format=%H" "--name-only"
                                  "--follow" (or rev "HEAD") "--" file))))
    (if rev (cddr lines) (butlast lines 2))))

(defun mercit-blob-successor (rev file)
  (let ((lines (mercit-with-toplevel
                 (mercit-git-lines "log" "--format=%H" "--name-only" "--follow"
                                  "HEAD" "--" file))))
    (catch 'found
      (while lines
        (if (equal (nth 2 lines) rev)
            (throw 'found (list (nth 0 lines) (nth 1 lines)))
          (setq lines (nthcdr 2 lines)))))))

;;; File Commands

(defun mercit-file-rename (file newname)
  "Rename or move FILE to NEWNAME.
NEWNAME may be a file or directory name.  If FILE isn't tracked in
Git, fallback to using `rename-file'."
  (interactive
   (let* ((file (mercit-read-file "Rename file"))
          (path (expand-file-name file (mercit-toplevel))))
     (list path (expand-file-name
                 (read-file-name (format "Move %s to destination: " file)
                                 (file-name-directory path))))))
  (let ((oldbuf (get-file-buffer file))
        (dstdir (file-name-directory newname))
        (dstfile (if (directory-name-p newname)
                     (concat newname (file-name-nondirectory file))
                   newname)))
    (when (and oldbuf (buffer-modified-p oldbuf))
      (user-error "Save %s before moving it" file))
    (when (file-exists-p dstfile)
      (user-error "%s already exists" dstfile))
    (unless (file-exists-p dstdir)
      (user-error "Destination directory %s does not exist" dstdir))
    (if (mercit-file-tracked-p (mercit-convert-filename-for-git file))
        (mercit-call-git "mv"
                        (mercit-convert-filename-for-git file)
                        (mercit-convert-filename-for-git newname))
      (rename-file file newname current-prefix-arg))
    (when oldbuf
      (with-current-buffer oldbuf
        (let ((buffer-read-only buffer-read-only))
          (set-visited-file-name dstfile nil t))
        (if (fboundp 'vc-refresh-state)
            (vc-refresh-state)
          (with-no-warnings
            (vc-find-file-hook))))))
  (mercit-refresh))

(defun mercit-file-untrack (files &optional force)
  "Untrack the selected FILES or one file read in the minibuffer.

With a prefix argument FORCE do so even when the files have
staged as well as unstaged changes."
  (interactive (list (or (--if-let (mercit-region-values 'file t)
                             (progn
                               (unless (mercit-file-tracked-p (car it))
                                 (user-error "Already untracked"))
                               (mercit-confirm-files 'untrack it "Untrack"))
                           (list (mercit-read-tracked-file "Untrack file"))))
                     current-prefix-arg))
  (mercit-with-toplevel
    (mercit-run-git "rm" "--cached" (and force "--force") "--" files)))

(defun mercit-file-delete (files &optional force)
  "Delete the selected FILES or one file read in the minibuffer.

With a prefix argument FORCE do so even when the files have
uncommitted changes.  When the files aren't being tracked in
Git, then fallback to using `delete-file'."
  (interactive (list (--if-let (mercit-region-values 'file t)
                         (mercit-confirm-files 'delete it "Delete")
                       (list (mercit-read-file "Delete file")))
                     current-prefix-arg))
  (if (mercit-file-tracked-p (car files))
      (mercit-call-git "rm" (and force "--force") "--" files)
    (let ((topdir (mercit-toplevel)))
      (dolist (file files)
        (delete-file (expand-file-name file topdir) t))))
  (mercit-refresh))

;;;###autoload
(defun mercit-file-checkout (rev file)
  "Checkout FILE from REV."
  (interactive
   (let ((rev (mercit-read-branch-or-commit
               "Checkout from revision" mercit-buffer-revision)))
     (list rev (mercit-read-file-from-rev rev "Checkout file"))))
  (mercit-with-toplevel
    (mercit-run-git "checkout" rev "--" file)))

;;; Read File

(defvar mercit-read-file-hist nil)

(defun mercit-read-file-from-rev (rev prompt &optional default)
  (let ((files (mercit-revision-files rev)))
    (mercit-completing-read
     prompt files nil t nil 'mercit-read-file-hist
     (car (member (or default (mercit-current-file)) files)))))

(defun mercit-read-file (prompt &optional tracked-only)
  (let ((choices (nconc (mercit-list-files)
                        (unless tracked-only (mercit-untracked-files)))))
    (mercit-completing-read
     prompt choices nil t nil nil
     (car (member (or (mercit-section-value-if '(file submodule))
                      (mercit-file-relative-name nil tracked-only))
                  choices)))))

(defun mercit-read-tracked-file (prompt)
  (mercit-read-file prompt t))

(defun mercit-read-unmerged-file (&optional prompt)
  (let ((current  (mercit-current-file))
        (unmerged (mercit-unmerged-files)))
    (unless unmerged
      (user-error "There are no unresolved conflicts"))
    (mercit-completing-read (or prompt "Resolve file")
                           unmerged nil t nil nil
                           (car (member current unmerged)))))

(defun mercit-read-file-choice (prompt files &optional error default)
  "Read file from FILES.

If FILES has only one member, return that instead of prompting.
If FILES has no members, give a user error.  ERROR can be given
to provide a more informative error.

If DEFAULT is non-nil, use this as the default value instead of
`mercit-current-file'."
  (pcase (length files)
    (0 (user-error (or error "No file choices")))
    (1 (car files))
    (_ (mercit-completing-read
        prompt files nil t nil 'mercit-read-file-hist
        (car (member (or default (mercit-current-file)) files))))))

(defun mercit-read-changed-file (rev-or-range prompt &optional default)
  (mercit-read-file-choice
   prompt
   (mercit-changed-files rev-or-range)
   default
   (concat "No file changed in " rev-or-range)))

;;; _
(provide 'mercit-files)
;;; mercit-files.el ends here
