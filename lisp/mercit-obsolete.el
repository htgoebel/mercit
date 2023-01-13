;;; mercit-obsolete.el --- Obsolete definitions  -*- lexical-binding:t -*-

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

;; This library defines aliases for obsolete variables and functions.

;;; Code:

(require 'mercit)

;;; Obsolete since v3.0.0

(define-obsolete-function-alias 'mercit-diff-visit-file-worktree
  #'mercit-diff-visit-worktree-file "Magit 3.0.0")

(define-obsolete-function-alias 'mercit-status-internal
  #'mercit-status-setup-buffer "Magit 3.0.0")

(define-obsolete-variable-alias 'mercit-mode-setup-hook
  'mercit-setup-buffer-hook "Magit 3.0.0")

(define-obsolete-variable-alias 'mercit-branch-popup-show-variables
  'mercit-branch-direct-configure "Magit 3.0.0")

(define-obsolete-function-alias 'mercit-dispatch-popup
  #'mercit-dispatch "Magit 3.0.0")

(define-obsolete-function-alias 'mercit-repolist-column-dirty
  #'mercit-repolist-column-flag "Magit 3.0.0")

(define-obsolete-variable-alias 'mercit-disable-line-numbers
  'mercit-section-disable-line-numbers "Magit 3.0.0")

(define-obsolete-variable-alias 'inhibit-mercit-refresh
  'mercit-inhibit-refresh "Magit 3.0.0")

(defun mercit--mercit-popup-warning ()
  (display-warning 'mercit "\
Magit no longer uses Magit-Popup.
It now uses Transient.
See https://emacsair.me/2019/02/14/transient-0.1.

However your configuration and/or some third-party package that
you use still depends on the `mercit-popup' package.  But because
`mercit' no longer depends on that, `package' has removed it from
your system.

If some package that you use still depends on `mercit-popup' but
does not declare it as a dependency, then please contact its
maintainer about that and install `mercit-popup' explicitly.

If you yourself use functions that are defined in `mercit-popup'
in your configuration, then the next step depends on what you use
that for.

* If you use `mercit-popup' to define your own popups but do not
  modify any of Magit's old popups, then you have to install
  `mercit-popup' explicitly.  (You can also migrate to Transient,
  but there is no need to rush that.)

* If you add additional arguments and/or actions to Magit's popups,
  then you have to port that to modify the new \"transients\" instead.
  See https://github.com/mercit/mercit/wiki/\
Converting-popup-modifications-to-transient-modifications

To find installed packages that still use `mercit-popup' you can
use e.g. \"M-x rgrep RET mercit-popup RET RET ~/.emacs.d/ RET\"."))
(cl-eval-when (eval load)
  (unless (require (quote mercit-popup) nil t)
    (defun mercit-define-popup-switch (&rest _)
      (mercit--mercit-popup-warning))
    (defun mercit-define-popup-option (&rest _)
      (mercit--mercit-popup-warning))
    (defun mercit-define-popup-variable (&rest _)
      (mercit--mercit-popup-warning))
    (defun mercit-define-popup-action (&rest _)
      (mercit--mercit-popup-warning))
    (defun mercit-define-popup-sequence-action (&rest _)
      (mercit--mercit-popup-warning))
    (defun mercit-define-popup-key (&rest _)
      (mercit--mercit-popup-warning))
    (defun mercit-define-popup-keys-deferred (&rest _)
      (mercit--mercit-popup-warning))
    (defun mercit-change-popup-key (&rest _)
      (mercit--mercit-popup-warning))
    (defun mercit-remove-popup-key (&rest _)
      (mercit--mercit-popup-warning))))

;;; _
(provide 'mercit-obsolete)
;;; mercit-obsolete.el ends here
