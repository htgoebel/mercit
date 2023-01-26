;;; mercit-libgit.el --- (POC) Teach Mercit to use Libgit2  -*- lexical-binding:t -*-

;; Copyright (C) 2023      The Mercit Project Contributors
;; Copyright (C) 2008-2023 The Magit Project Contributors

;; Homepage: https://github.com/htgoebel/mercit
;; Keywords: git tools vc

;; Package-Version: 3.3.0.50-git
;; Package-Requires: (
;;     (emacs "26.1")
;;     (compat "28.1.1.2")
;;     (libgit "0")
;;     (mercit "3.3.0"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Mercit is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; Mercit is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Mercit.  If not, see <https://www.gnu.org/licenses/>.

;; You should have received a copy of the AUTHORS.md file, which
;; lists all contributors.  If not, see
;; https://github.com/htgoebel/mercit/docs/AUTHORS.md.

;;; Commentary:

;; This package teaches Mercit to use functions provided by the
;; `libegit2' module to perform certain tasks.  That module used the
;; Libgit2 implementation of the Git core methods and is implemented
;; in the `libgit' package.

;; The hope is that using a C module instead of calling out to `git'
;; all the time increases performance; especially on Windows where
;; starting a process is unreasonably slow.

;; This package is still experimental and not many functions have been
;; reimplemented to use `libgit' yet.

;;; Code:

(require 'cl-lib)
(require 'compat)
(require 'dash)
(require 'eieio)
(require 'seq)
(require 'subr-x)

(require 'mercit-git)

(require 'libgit)

;;; Utilities

(defun mercit-libgit-repo (&optional directory)
  "Return an object for the repository in DIRECTORY.
If optional DIRECTORY is nil, then use `default-directory'."
  (and-let* ((default-directory
              (let ((mercit-inhibit-libgit t))
                (mercit-gitdir directory))))
    (mercit--with-refresh-cache
        (cons default-directory 'mercit-libgit-repo)
      (libgit-repository-open default-directory))))

;;; Methods

(cl-defmethod mercit-bare-repo-p
  (&context ((mercit-gitimpl) (eql libgit)) &optional noerror)
  (and (mercit--assert-default-directory noerror)
       (if-let ((repo (mercit-libgit-repo)))
           (libgit-repository-bare-p repo)
         (unless noerror
           (signal 'mercit-outside-git-repo default-directory)))))

;;; _
(provide 'mercit-libgit)
;;; mercit-libgit.el ends here
