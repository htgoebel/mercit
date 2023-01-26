;;; mercit-margin.el --- Margins in Mercit buffers  -*- lexical-binding:t -*-

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

;; This library implements support for showing additional information
;; in the margins of Mercit buffers.  Currently this is only used for
;; commits, for which the committer date or age, and optionally the
;; author name are shown.

;;; Code:

(require 'mercit-base)
(require 'mercit-transient)
(require 'mercit-mode)

;;; Options

(defgroup mercit-margin nil
  "Information Mercit displays in the margin.

You can change the STYLE and AUTHOR-WIDTH of all `mercit-*-margin'
options to the same values by customizing `mercit-log-margin'
*before* `mercit' is loaded.  If you do that, then the respective
values for the other options will default to what you have set
for that variable.  Likewise if you set `mercit-log-margin's INIT
to nil, then that is used in the default of all other options.  But
setting it to t, i.e. re-enforcing the default for that option,
does not carry to other options."
  :link '(info-link "(mercit)Log Margin")
  :group 'mercit-log)

(defvar-local mercit-buffer-margin nil)
(put 'mercit-buffer-margin 'permanent-local t)

(defvar-local mercit-set-buffer-margin-refresh nil)

(defvar mercit--age-spec)

;;; Commands

(transient-define-prefix mercit-margin-settings ()
  "Change what information is displayed in the margin."
  :info-manual "(mercit) Log Margin"
  ["Margin"
   ("L" "Toggle visibility" mercit-toggle-margin      :transient t)
   ("l" "Cycle style"       mercit-cycle-margin-style :transient t)
   ("d" "Toggle details"    mercit-toggle-margin-details)
   ("v" "Change verbosity"  mercit-refs-set-show-commit-count
    :if-derived mercit-refs-mode)])

(defun mercit-toggle-margin ()
  "Show or hide the Mercit margin."
  (interactive)
  (unless (mercit-margin-option)
    (user-error "Mercit margin isn't supported in this buffer"))
  (setcar mercit-buffer-margin (not (mercit-buffer-margin-p)))
  (mercit-set-buffer-margin))

(defvar mercit-margin-default-time-format nil
  "See https://github.com/magit/magit/pull/4605.")

(defun mercit-cycle-margin-style ()
  "Cycle style used for the Mercit margin."
  (interactive)
  (unless (mercit-margin-option)
    (user-error "Mercit margin isn't supported in this buffer"))
  ;; This is only suitable for commit margins (there are not others).
  (setf (cadr mercit-buffer-margin)
        (pcase (cadr mercit-buffer-margin)
          ('age 'age-abbreviated)
          ('age-abbreviated
           (let ((default (or mercit-margin-default-time-format
                              (cadr (symbol-value (mercit-margin-option))))))
             (if (stringp default) default "%Y-%m-%d %H:%M ")))
          (_ 'age)))
  (mercit-set-buffer-margin nil t))

(defun mercit-toggle-margin-details ()
  "Show or hide details in the Mercit margin."
  (interactive)
  (unless (mercit-margin-option)
    (user-error "Mercit margin isn't supported in this buffer"))
  (setf (nth 3 mercit-buffer-margin)
        (not (nth 3 mercit-buffer-margin)))
  (mercit-set-buffer-margin nil t))

;;; Core

(defun mercit-buffer-margin-p ()
  (car mercit-buffer-margin))

(defun mercit-margin-option ()
  (pcase major-mode
    ('mercit-cherry-mode     'mercit-cherry-margin)
    ('mercit-log-mode        'mercit-log-margin)
    ('mercit-log-select-mode 'mercit-log-select-margin)
    ('mercit-reflog-mode     'mercit-reflog-margin)
    ('mercit-refs-mode       'mercit-refs-margin)
    ('mercit-stashes-mode    'mercit-stashes-margin)
    ('mercit-status-mode     'mercit-status-margin)
    ('forge-notifications-mode 'mercit-status-margin)))

(defun mercit-set-buffer-margin (&optional reset refresh)
  (when-let ((option (mercit-margin-option)))
    (let* ((default (symbol-value option))
           (default-width (nth 2 default)))
      (when (or reset (not mercit-buffer-margin))
        (setq mercit-buffer-margin (copy-sequence default)))
      (pcase-let ((`(,enable ,style ,_width ,details ,details-width)
                   mercit-buffer-margin))
        (when (functionp default-width)
          (setf (nth 2 mercit-buffer-margin)
                (funcall default-width style details details-width)))
        (dolist (window (get-buffer-window-list nil nil 0))
          (with-selected-window window
            (mercit-set-window-margin window)
            (if enable
                (add-hook  'window-configuration-change-hook
                           #'mercit-set-window-margin nil t)
              (remove-hook 'window-configuration-change-hook
                           #'mercit-set-window-margin t))))
        (when (and enable (or refresh mercit-set-buffer-margin-refresh))
          (mercit-refresh-buffer))))))

(defun mercit-set-window-margin (&optional window)
  (when (or window (setq window (get-buffer-window)))
    (with-selected-window window
      (set-window-margins
       nil (car (window-margins))
       (and (mercit-buffer-margin-p)
            (nth 2 mercit-buffer-margin))))))

(defun mercit-make-margin-overlay (&optional string previous-line)
  (if previous-line
      (save-excursion
        (forward-line -1)
        (mercit-make-margin-overlay string))
    ;; Don't put the overlay on the complete line to work around #1880.
    (let ((o (make-overlay (1+ (line-beginning-position))
                           (line-end-position)
                           nil t)))
      (overlay-put o 'evaporate t)
      (overlay-put o 'before-string
                   (propertize "o" 'display
                               (list (list 'margin 'right-margin)
                                     (or string " ")))))))

(defun mercit-maybe-make-margin-overlay ()
  (when (or (mercit-section-match
             '(unpulled unpushed recent stashes local cherries)
             mercit-insert-section--current)
            (and (eq major-mode 'mercit-refs-mode)
                 (mercit-section-match
                  '(remote commit tags)
                  mercit-insert-section--current)))
    (mercit-make-margin-overlay nil t)))

;;; Custom Support

(defun mercit-margin-set-variable (mode symbol value)
  (set-default symbol value)
  (message "Updating margins in %s buffers..." mode)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode mode)
        (mercit-set-buffer-margin t)
        (mercit-refresh))))
  (message "Updating margins in %s buffers...done" mode))

(defconst mercit-log-margin--custom-type
  '(list (boolean :tag "Show margin initially")
         (choice  :tag "Show committer"
                  (string :tag "date using time-format" "%Y-%m-%d %H:%M ")
                  (const  :tag "date's age" age)
                  (const  :tag "date's age (abbreviated)" age-abbreviated))
         (const   :tag "Calculate width using mercit-log-margin-width"
                  mercit-log-margin-width)
         (boolean :tag "Show author name by default")
         (integer :tag "Show author name using width")))

;;; Time Utilities

(defvar mercit--age-spec
  `((?Y "year"   "years"   ,(round (* 60 60 24 365.2425)))
    (?M "month"  "months"  ,(round (* 60 60 24 30.436875)))
    (?w "week"   "weeks"   ,(* 60 60 24 7))
    (?d "day"    "days"    ,(* 60 60 24))
    (?h "hour"   "hours"   ,(* 60 60))
    (?m "minute" "minutes" 60)
    (?s "second" "seconds" 1))
  "Time units used when formatting relative commit ages.

The value is a list of time units, beginning with the longest.
Each element has the form (CHAR UNIT UNITS SECONDS).  UNIT is the
time unit, UNITS is the plural of that unit.  CHAR is a character
abbreviation.  And SECONDS is the number of seconds in one UNIT.

This is defined as a variable to make it possible to use time
units for a language other than English.  It is not defined
as an option, because most other parts of Mercit are always in
English.")

(defun mercit--age (date &optional abbreviate)
  (cl-labels ((fn (age spec)
                (pcase-let ((`(,char ,unit ,units ,weight) (car spec)))
                  (let ((cnt (round (/ age weight 1.0))))
                    (if (or (not (cdr spec))
                            (>= (/ age weight) 1))
                        (list cnt (cond (abbreviate char)
                                        ((= cnt 1) unit)
                                        (t units)))
                      (fn age (cdr spec)))))))
    (fn (abs (- (float-time)
                (if (stringp date)
                    (string-to-number date)
                  date)))
        mercit--age-spec)))

;;; _
(provide 'mercit-margin)
;;; mercit-margin.el ends here
