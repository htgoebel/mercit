;;; mercit-bookmark.el --- Bookmark support for Mercit  -*- lexical-binding:t -*-

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

;; Support for bookmarks for most Mercit buffers.

;;; Code:

(require 'mercit)

;;; Diff
;;;; Diff

(put 'mercit-diff-mode 'mercit-bookmark-variables
     '(mercit-buffer-range-hashed
       mercit-buffer-typearg
       mercit-buffer-diff-args
       mercit-buffer-diff-files))

(cl-defmethod mercit-bookmark-name (&context (major-mode mercit-diff-mode))
  (format "mercit-diff(%s%s)"
          (pcase (mercit-diff-type)
            ('staged "staged")
            ('unstaged "unstaged")
            ('committed mercit-buffer-range)
            ('undefined
             (delq nil (list mercit-buffer-typearg mercit-buffer-range-hashed))))
          (if mercit-buffer-diff-files
              (concat " -- " (mapconcat #'identity mercit-buffer-diff-files " "))
            "")))

;;;; Revision

(put 'mercit-revision-mode 'mercit-bookmark-variables
     '(mercit-buffer-revision-hash
       mercit-buffer-diff-args
       mercit-buffer-diff-files))

(cl-defmethod mercit-bookmark-name (&context (major-mode mercit-revision-mode))
  (format "mercit-revision(%s %s)"
          (mercit-rev-abbrev mercit-buffer-revision)
          (if mercit-buffer-diff-files
              (mapconcat #'identity mercit-buffer-diff-files " ")
            (mercit-rev-format "{desc|firstline}" mercit-buffer-revision))))

;;;; Stash

(put 'mercit-stash-mode 'mercit-bookmark-variables
     '(mercit-buffer-revision-hash
       mercit-buffer-diff-args
       mercit-buffer-diff-files))

(cl-defmethod mercit-bookmark-name (&context (major-mode mercit-stash-mode))
  (format "mercit-stash(%s %s)"
          (mercit-rev-abbrev mercit-buffer-revision)
          (if mercit-buffer-diff-files
              (mapconcat #'identity mercit-buffer-diff-files " ")
            (mercit-rev-format "{desc|firstline}" mercit-buffer-revision))))

;;; Log
;;;; Log

(put 'mercit-log-mode 'mercit-bookmark-variables
     '(mercit-buffer-revisions
       mercit-buffer-log-args
       mercit-buffer-log-files))

(cl-defmethod mercit-bookmark-name (&context (major-mode mercit-log-mode))
  (format "mercit-log(%s%s)"
          (mapconcat #'identity mercit-buffer-revisions " ")
          (if mercit-buffer-log-files
              (concat " -- " (mapconcat #'identity mercit-buffer-log-files " "))
            "")))

;;;; Cherry

(put 'mercit-cherry-mode 'mercit-bookmark-variables
     '(mercit-buffer-refname
       mercit-buffer-upstream))

(cl-defmethod mercit-bookmark-name (&context (major-mode mercit-cherry-mode))
  (format "mercit-cherry(%s > %s)"
          mercit-buffer-refname
          mercit-buffer-upstream))

;;;; Reflog

(put 'mercit-reflog-mode 'mercit-bookmark-variables
     '(mercit-buffer-refname))

(cl-defmethod mercit-bookmark-name (&context (major-mode mercit-reflog-mode))
  (format "mercit-reflog(%s)" mercit-buffer-refname))

;;; Misc

(put 'mercit-status-mode 'mercit-bookmark-variables nil)

(put 'mercit-refs-mode 'mercit-bookmark-variables
     '(mercit-buffer-upstream
       mercit-buffer-arguments))

(put 'mercit-stashes-mode 'mercit-bookmark-variables nil)

(cl-defmethod mercit-bookmark-name (&context (major-mode mercit-stashes-mode))
  (format "mercit-states(%s)" mercit-buffer-refname))

;;; _
(provide 'mercit-bookmark)
;;; mercit-bookmark.el ends here
