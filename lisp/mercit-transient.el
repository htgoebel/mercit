;;; mercit-transient.el --- Support for transients  -*- lexical-binding:t -*-

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

;; This library implements Mercit-specific prefix and suffix classes,
;; and their methods.

;;; Code:

(require 'mercit-git)
(require 'mercit-mode)
(require 'mercit-process)

(require 'transient)

;;; Classes

(defclass mercit--git-variable (transient-variable)
  ((scope       :initarg :scope)
   (global      :initarg :global      :initform nil)))

(defclass mercit--git-variable:choices (mercit--git-variable)
  ((choices     :initarg :choices)
   (fallback    :initarg :fallback    :initform nil)
   (default     :initarg :default     :initform nil)))

(defclass mercit--git-variable:boolean (mercit--git-variable:choices)
  ((choices     :initarg :choices     :initform '("true" "false"))))

(defclass mercit--git-variable:urls (mercit--git-variable)
  ((seturl-arg  :initarg :seturl-arg  :initform nil)))

;;; Methods
;;;; Init

(cl-defmethod transient-init-scope ((obj mercit--git-variable))
  (oset obj scope
        (cond (transient--prefix
               (oref transient--prefix scope))
              ((slot-boundp obj 'scope)
               (funcall (oref obj scope) obj)))))

(cl-defmethod transient-init-value ((obj mercit--git-variable))
  (let ((variable (format (oref obj variable)
                          (oref obj scope)))
        (arg (if (oref obj global) "--global" "--local")))
    (oset obj variable variable)
    (oset obj value
          (cond ((oref obj multi-value)
                 (mercit-get-all arg variable))
                (t
                 (mercit-get arg variable))))))

(cl-defmethod transient-init-value ((obj mercit--git-variable:boolean))
  (let ((variable (format (oref obj variable)
                          (oref obj scope)))
        (arg (if (oref obj global) "--global" "--local")))
    (oset obj variable variable)
    (oset obj value (if (mercit-get-boolean arg variable) "true" "false"))))

;;;; Read

(cl-defmethod transient-infix-read :around ((obj mercit--git-variable:urls))
  (transient--with-emergency-exit
    (transient--with-suspended-override
     (mapcar (lambda (url)
               (if (string-prefix-p "~" url)
                   (expand-file-name url)
                 url))
             (cl-call-next-method obj)))))

(cl-defmethod transient-infix-read ((obj mercit--git-variable:choices))
  (let ((choices (oref obj choices)))
    (when (functionp choices)
      (setq choices (funcall choices)))
    (if-let ((value (oref obj value)))
        (cadr (member value choices))
      (car choices))))

;;;; Readers

(defun mercit-transient-read-person (prompt initial-input history)
  (mercit-completing-read
   prompt
   (mapcar (lambda (line)
             (save-excursion
               (and (string-match "\\`[\s\t]+[0-9]+\t" line)
                    (list (substring line (match-end 0))))))
           (mercit-git-lines "shortlog" "-n" "-s" "-e" "HEAD"))
   nil nil initial-input history))

(defun mercit-transient-read-revision (prompt initial-input history)
  (or (mercit-completing-read prompt (cons "HEAD" (mercit-list-refnames))
                             nil nil initial-input history
                             (or (mercit-branch-or-commit-at-point)
                                 (mercit-get-current-branch)))
      (user-error "Nothing selected")))

;;;; Set

(cl-defmethod transient-infix-set ((obj mercit--git-variable) value)
  (let ((variable (oref obj variable))
        (arg (if (oref obj global) "--global" "--local")))
    (oset obj value value)
    (if (oref obj multi-value)
        (mercit-set-all value arg variable)
      (mercit-set value arg variable))
    (mercit-refresh)
    (unless (or value transient--prefix)
      (message "Unset %s" variable))))

(cl-defmethod transient-infix-set ((obj mercit--git-variable:urls) values)
  (let ((previous (oref obj value))
        (seturl   (oref obj seturl-arg))
        (remote   (oref transient--prefix scope)))
    (oset obj value values)
    (dolist (v (-difference values previous))
      (mercit-call-git "remote" "set-url" seturl "--add" remote v))
    (dolist (v (-difference previous values))
      (mercit-call-git "remote" "set-url" seturl "--delete" remote
                      (concat "^" (regexp-quote v) "$")))
    (mercit-refresh)))

;;;; Draw

(cl-defmethod transient-format-description ((obj mercit--git-variable))
  (or (oref obj description)
      (oref obj variable)))

(cl-defmethod transient-format-value ((obj mercit--git-variable))
  (if-let ((value (oref obj value)))
      (if (oref obj multi-value)
          (if (cdr value)
              (mapconcat (lambda (v)
                           (concat "\n     "
                                   (propertize v 'face 'transient-value)))
                         value "")
            (propertize (car value) 'face 'transient-value))
        (propertize (car (split-string value "\n"))
                    'face 'transient-value))
    (propertize "unset" 'face 'transient-inactive-value)))

(cl-defmethod transient-format-value ((obj mercit--git-variable:choices))
  (let* ((variable (oref obj variable))
         (choices  (oref obj choices))
         (globalp  (oref obj global))
         (value    nil)
         (global   (mercit-git-string "config" variable)) ;; was "--global"
         (defaultp (oref obj default))
         (default  (if (functionp defaultp) (funcall defaultp obj) defaultp))
         (fallback (oref obj fallback))
         (fallback (and fallback
                        (and-let* ((val (mercit-get fallback)))
                          (concat fallback ":" val)))))
    (if (not globalp)
        (setq value (mercit-git-string "config" variable)) ;; was "--local"
      (setq value global)
      (setq global nil))
    (when (functionp choices)
      (setq choices (funcall choices)))
    (concat
     (propertize "[" 'face 'transient-inactive-value)
     (mapconcat (lambda (choice)
                  (propertize choice 'face (if (equal choice value)
                                               (if (member choice choices)
                                                   'transient-value
                                                 'font-lock-warning-face)
                                             'transient-inactive-value)))
                (if (and value (not (member value choices)))
                    (cons value choices)
                  choices)
                (propertize "|" 'face 'transient-inactive-value))
     (and (or global fallback default)
          (concat
           (propertize "|" 'face 'transient-inactive-value)
           (cond (global
                  (propertize (concat "global:" global)
                              'face (cond (value
                                           'transient-inactive-value)
                                          ((member global choices)
                                           'transient-value)
                                          (t
                                           'font-lock-warning-face))))
                 (fallback
                  (propertize fallback
                              'face (if value
                                        'transient-inactive-value
                                      'transient-value)))
                 (default
                  (propertize (if (functionp defaultp)
                                  (concat "dwim:" default)
                                (concat "default:" default))
                              'face (if value
                                        'transient-inactive-value
                                      'transient-value))))))
     (propertize "]" 'face 'transient-inactive-value))))

;;; _
(provide 'mercit-transient)
;;; mercit-transient.el ends here
