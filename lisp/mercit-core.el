;;; mercit-core.el --- Core functionality  -*- lexical-binding:t -*-

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

;; This library requires several other libraries, so that yet other
;; libraries can just require this one, instead of having to require
;; all the other ones.  In other words this separates the low-level
;; stuff from the rest.  It also defines some Custom groups.

;;; Code:

(require 'mercit-base)
(require 'mercit-git)
(require 'mercit-mode)
(require 'mercit-margin)
(require 'mercit-process)
(require 'mercit-transient)
(require 'mercit-autorevert)

(when (mercit--libgit-available-p)
  (condition-case err
      (require 'mercit-libgit)
    (error
     (setq mercit-inhibit-libgit 'error)
     (message "Error while loading `mercit-libgit': %S" err)
     (message "That is not fatal.  The `libegit2' module just won't be used."))))

;;; Options

(defgroup mercit nil
  "Controlling Git from Emacs."
  :link '(url-link "https://mercit.vc")
  :link '(info-link "(mercit)FAQ")
  :link '(info-link "(mercit)")
  :group 'tools)

(defgroup mercit-essentials nil
  "Options that every Mercit user should briefly think about.

Each of these options falls into one or more of these categories:

* Options that affect Mercit's behavior in fundamental ways.
* Options that affect safety.
* Options that affect performance.
* Options that are of a personal nature."
  :link '(info-link "(mercit)Essential Settings")
  :group 'mercit)

(defgroup mercit-miscellaneous nil
  "Miscellaneous Mercit options."
  :group 'mercit)

(defgroup mercit-commands nil
  "Options controlling behavior of certain commands."
  :group 'mercit)

(defgroup mercit-modes nil
  "Modes used or provided by Mercit."
  :group 'mercit)

(defgroup mercit-buffers nil
  "Options concerning Mercit buffers."
  :link '(info-link "(mercit)Modes and Buffers")
  :group 'mercit)

(defgroup mercit-refresh nil
  "Options controlling how Mercit buffers are refreshed."
  :link '(info-link "(mercit)Automatic Refreshing of Mercit Buffers")
  :group 'mercit
  :group 'mercit-buffers)

(defgroup mercit-faces nil
  "Faces used by Mercit."
  :group 'mercit
  :group 'faces)

(custom-add-to-group 'mercit-faces 'diff-refine-added   'custom-face)
(custom-add-to-group 'mercit-faces 'diff-refine-removed 'custom-face)

(defgroup mercit-extensions nil
  "Extensions to Mercit."
  :group 'mercit)

(custom-add-to-group 'mercit-modes   'git-commit        'custom-group)
(custom-add-to-group 'mercit-faces   'git-commit-faces  'custom-group)
(custom-add-to-group 'mercit-modes   'git-rebase        'custom-group)
(custom-add-to-group 'mercit-faces   'git-rebase-faces  'custom-group)
(custom-add-to-group 'mercit         'mercit-section     'custom-group)
(custom-add-to-group 'mercit-faces   'mercit-section-faces 'custom-group)
(custom-add-to-group 'mercit-process 'with-editor       'custom-group)

(defgroup mercit-related nil
  "Options that are relevant to Mercit but that are defined elsewhere."
  :link '(custom-group-link vc)
  :link '(custom-group-link smerge)
  :link '(custom-group-link ediff)
  :link '(custom-group-link auto-revert)
  :group 'mercit
  :group 'mercit-extensions
  :group 'mercit-essentials)

(custom-add-to-group 'mercit-related     'auto-revert-check-vc-info 'custom-variable)
(custom-add-to-group 'mercit-auto-revert 'auto-revert-check-vc-info 'custom-variable)

(custom-add-to-group 'mercit-related 'ediff-window-setup-function 'custom-variable)
(custom-add-to-group 'mercit-related 'smerge-refine-ignore-whitespace 'custom-variable)
(custom-add-to-group 'mercit-related 'vc-follow-symlinks 'custom-variable)

;;; _
(provide 'mercit-core)
;;; mercit-core.el ends here
