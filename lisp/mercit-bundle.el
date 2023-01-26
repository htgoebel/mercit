;;; mercit-bundle.el --- Bundle support for Mercit  -*- lexical-binding:t -*-

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

;;;###autoload (autoload 'mercit-bundle "mercit-bundle" nil t)
(transient-define-prefix mercit-bundle ()
  "Create or verify Mercurial bundles."
  :man-page "git-bundle"
  ["Actions"
   ("c" "create"     mercit-bundle-create)
   ("v" "verify"     mercit-bundle-verify)
   ("l" "list-heads" mercit-bundle-list-heads)])

;;;###autoload (autoload 'mercit-bundle-import "mercit-bundle" nil t)
(transient-define-prefix mercit-bundle-create (&optional file refs args)
  "Create a bundle."
  :man-page "git-bundle"
  ["Arguments"
   ("-a" "Include all refs" "--all")
   ("-b" "Include branches" "--branches=" :allow-empty t)
   ("-t" "Include tags"     "--tags="     :allow-empty t)
   ("-r" "Include remotes"  "--remotes="  :allow-empty t)
   ("-g" "Include refs"     "--glob=")
   ("-e" "Exclude refs"     "--exclude=")
   (mercit-log:-n)
   (mercit-log:--since)
   (mercit-log:--until)]
  ["Actions"
   ("c" "create regular bundle" mercit-bundle-create)
   ("t" "create tracked bundle" mercit-bundle-create-tracked)
   ("u" "update tracked bundle" mercit-bundle-update-tracked)]
  (interactive
   (and (eq transient-current-command 'mercit-bundle-create)
        (list (read-file-name "Create bundle: " nil nil nil
                              (concat (file-name-nondirectory
                                       (directory-file-name (mercit-toplevel)))
                                      ".bundle"))
              (mercit-completing-read-multiple* "Refnames (zero or more): "
                                               (mercit-list-refnames))
              (transient-args 'mercit-bundle-create))))
  (if file
      (mercit-git-bundle "create" file refs args)
    (transient-setup 'mercit-bundle-create)))

;;;###autoload
(defun mercit-bundle-create-tracked (file tag branch refs args)
  "Create and track a new bundle."
  (interactive
   (let ((tag    (mercit-read-tag "Track bundle using tag"))
         (branch (mercit-read-branch "Bundle branch"))
         (refs   (mercit-completing-read-multiple*
                  "Additional refnames (zero or more): "
                  (mercit-list-refnames))))
     (list (read-file-name "File: " nil nil nil (concat tag ".bundle"))
           tag branch
           (if (equal branch (mercit-get-current-branch))
               (cons "HEAD" refs)
             refs)
           (transient-args 'mercit-bundle-create))))
  (mercit-git-bundle "create" file (cons branch refs) args)
  (mercit-git "tag" "--force" tag branch
             "-m" (concat ";; git-bundle tracking\n"
                          (pp-to-string `((file   . ,file)
                                          (branch . ,branch)
                                          (refs   . ,refs)
                                          (args   . ,args))))))

;;;###autoload
(defun mercit-bundle-update-tracked (tag)
  "Update a bundle that is being tracked using TAG."
  (interactive (list (mercit-read-tag "Update bundle tracked by tag" t)))
  (let (msg)
    (let-alist (mercit--with-temp-process-buffer
                 (save-excursion
                   (mercit-git-insert "for-each-ref" "--format=%(contents)"
                                     (concat "refs/tags/" tag)))
                 (setq msg (buffer-string))
                 (ignore-errors (read (current-buffer))))
      (unless (and .file .branch)
        (error "Tag %s does not appear to track a bundle" tag))
      (mercit-git-bundle "create" .file
                        (cons (concat tag ".." .branch) .refs)
                        .args)
      (mercit-git "tag" "--force" tag .branch "-m" msg))))

;;;###autoload
(defun mercit-bundle-verify (file)
  "Check whether FILE is valid and applies to the current repository."
  (interactive (list (mercit-bundle--read-file-name "Verify bundle: ")))
  (mercit-process-buffer)
  (mercit-git-bundle "verify" file))

;;;###autoload
(defun mercit-bundle-list-heads (file)
  "List the refs in FILE."
  (interactive (list (mercit-bundle--read-file-name "List heads of bundle: ")))
  (mercit-process-buffer)
  (mercit-git-bundle "list-heads" file))

(defun mercit-bundle--read-file-name (prompt)
  (read-file-name prompt nil nil t (mercit-file-at-point) #'file-regular-p))

(defun mercit-git-bundle (command file &optional refs args)
  (mercit-git "bundle" command (mercit-convert-filename-for-git file) refs args))

;;; _
(provide 'mercit-bundle)
;;; mercit-bundle.el ends here
