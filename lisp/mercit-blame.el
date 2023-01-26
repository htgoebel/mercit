;;; mercit-blame.el --- Blame support for Mercit  -*- lexical-binding:t -*-

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

;; Annotates each line in file-visiting buffer with information from
;; the revision which last modified the line.

;;; Code:

(require 'mercit)

;;; Options

(defgroup mercit-blame nil
  "Blame support for Mercit."
  :link '(info-link "(mercit)Blaming")
  :group 'mercit-modes)

(defcustom mercit-blame-styles
  '((headings
     (heading-format   . "%-20a %C %s\n"))
    (highlight
     (highlight-face   . mercit-blame-highlight))
    (lines
     (show-lines       . t)
     (show-message     . t)))
  "List of styles used to visualize blame information.

The style used in the current buffer can be cycled from the blame
popup.  Blame commands (except `mercit-blame-echo') use the first
style as the initial style when beginning to blame in a buffer.

Each entry has the form (IDENT (KEY . VALUE)...).  IDENT has
to be a symbol uniquely identifying the style.  The following
KEYs are recognized:

 `show-lines'
    Whether to prefix each chunk of lines with a thin line.
    This has no effect if `heading-format' is non-nil.
 `show-message'
    Whether to display a commit's summary line in the echo area
    when crossing chunks.
 `highlight-face'
    Face used to highlight the first line of each chunk.
    If this is nil, then those lines are not highlighted.
 `heading-format'
    String specifying the information to be shown above each
    chunk of lines.  It must end with a newline character.
 `margin-format'
    String specifying the information to be shown in the left
    buffer margin.  It must NOT end with a newline character.
    This can also be a list of formats used for the lines at
    the same positions within the chunk.  If the chunk has
    more lines than formats are specified, then the last is
    repeated.  WARNING: Adding this key affects performance;
    see the note at the end of this docstring.
 `margin-width'
    Width of the margin, provided `margin-format' is non-nil.
 `margin-face'
    Face used in the margin, provided `margin-format' is
    non-nil.  This face is used in combination with the faces
    that are specific to the used %-specs.  If this is nil,
    then `mercit-blame-margin' is used.
 `margin-body-face'
    Face used in the margin for all but first line of a chunk.
    This face is used in combination with the faces that are
    specific to the used %-specs.  This can also be a list of
    faces (usually one face), in which case only these faces
    are used and the %-spec faces are ignored.  A good value
    might be `(mercit-blame-dimmed)'.  If this is nil, then
    the same face as for the first line is used.

The following %-specs can be used in `heading-format' and
`margin-format':

  %H    hash              using face `mercit-blame-hash'
  %s    summary           using face `mercit-blame-summary'
  %a    author            using face `mercit-blame-name'
  %A    author time       using face `mercit-blame-date'
  %c    committer         using face `mercit-blame-name'
  %C    committer time    using face `mercit-blame-date'

Additionally if `margin-format' ends with %f, then the string
that is displayed in the margin is made at least `margin-width'
characters wide, which may be desirable if the used face sets
the background color.

Blame information is displayed using overlays.  Such extensive
use of overlays is known to slow down even basic operations, such
as moving the cursor. To reduce the number of overlays the margin
style had to be removed from the default value of this option.

Note that the margin overlays are created even if another style
is currently active.  This can only be prevented by not even
defining a style that uses the margin.  If you want to use this
style anyway, you can restore this definition, which used to be
part of the default value:

  (margin
   (margin-format    . (\" %s%f\" \" %C %a\" \" %H\"))
   (margin-width     . 42)
   (margin-face      . mercit-blame-margin)
   (margin-body-face . (mercit-blame-dimmed)))"
  :package-version '(mercit . "2.13.0")
  :group 'mercit-blame
  :type 'string)

(defcustom mercit-blame-echo-style 'lines
  "The blame visualization style used by `mercit-blame-echo'.
A symbol that has to be used as the identifier for one of the
styles defined in `mercit-blame-styles'."
  :package-version '(mercit . "2.13.0")
  :group 'mercit-blame
  :type 'symbol)

(defcustom mercit-blame-time-format "%F %H:%M"
  "Format for time strings in blame headings."
  :group 'mercit-blame
  :type 'string)

(defcustom mercit-blame-read-only t
  "Whether to initially make the blamed buffer read-only."
  :package-version '(mercit . "2.13.0")
  :group 'mercit-blame
  :type 'boolean)

(defcustom mercit-blame-disable-modes '(fci-mode yascroll-bar-mode)
  "List of modes not compatible with Mercit-Blame mode.
This modes are turned off when Mercit-Blame mode is turned on,
and then turned on again when turning off the latter."
  :group 'mercit-blame
  :type '(repeat (symbol :tag "Mode")))

(defcustom mercit-blame-mode-lighter " Blame"
  "The mode-line lighter of the Mercit-Blame mode."
  :group 'mercit-blame
  :type '(choice (const :tag "No lighter" "") string))

(defcustom mercit-blame-goto-chunk-hook
  '(mercit-blame-maybe-update-revision-buffer
    mercit-blame-maybe-show-message)
  "Hook run after point entered another chunk."
  :package-version '(mercit . "2.13.0")
  :group 'mercit-blame
  :type 'hook
  :get #'mercit-hook-custom-get
  :options '(mercit-blame-maybe-update-revision-buffer
             mercit-blame-maybe-show-message))

;;; Faces

(defface mercit-blame-highlight
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "grey80"
     :foreground "black")
    (((class color) (background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "grey25"
     :foreground "white"))
  "Face used for highlighting when blaming.
Also see option `mercit-blame-styles'."
  :group 'mercit-faces)

(defface mercit-blame-margin
  '((t :inherit mercit-blame-highlight
       :weight normal
       :slant normal))
  "Face used for the blame margin by default when blaming.
Also see option `mercit-blame-styles'."
  :group 'mercit-faces)

(defface mercit-blame-dimmed
  '((t :inherit mercit-dimmed
       :weight normal
       :slant normal))
  "Face used for the blame margin in some cases when blaming.
Also see option `mercit-blame-styles'."
  :group 'mercit-faces)

(defface mercit-blame-heading
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :inherit mercit-blame-highlight
       :weight normal
       :slant normal))
  "Face used for blame headings by default when blaming.
Also see option `mercit-blame-styles'."
  :group 'mercit-faces)

(defface mercit-blame-summary '((t nil))
  "Face used for commit summaries when blaming."
  :group 'mercit-faces)

(defface mercit-blame-hash '((t nil))
  "Face used for commit hashes when blaming."
  :group 'mercit-faces)

(defface mercit-blame-name '((t nil))
  "Face used for author and committer names when blaming."
  :group 'mercit-faces)

(defface mercit-blame-date '((t nil))
  "Face used for dates when blaming."
  :group 'mercit-faces)

;;; Variables

(defvar-local mercit-blame-buffer-read-only nil)
(defvar-local mercit-blame-cache nil)
(defvar-local mercit-blame-disabled-modes nil)
(defvar-local mercit-blame-process nil)
(defvar-local mercit-blame-recursive-p nil)
(defvar-local mercit-blame-type nil)
(defvar-local mercit-blame-separator nil)
(defvar-local mercit-blame-previous-chunk nil)

(defvar-local mercit-blame--make-margin-overlays nil)
(defvar-local mercit-blame--style nil)

;;; Chunks

(defclass mercit-blame-chunk ()
  (;; <orig-rev> <orig-line> <final-line> <num-lines>
   (orig-rev   :initarg :orig-rev)
   (orig-line  :initarg :orig-line)
   (final-line :initarg :final-line)
   (num-lines  :initarg :num-lines)
   ;; previous <prev-rev> <prev-file>
   (prev-rev   :initform nil)
   (prev-file  :initform nil)
   ;; filename <orig-file>
   (orig-file)))

(defun mercit-current-blame-chunk (&optional type noerror)
  (or (and (not (and type (not (eq type mercit-blame-type))))
           (mercit-blame-chunk-at (point)))
      (and type
           (let ((rev  (or mercit-buffer-refname mercit-buffer-revision))
                 (file (and (not (derived-mode-p 'dired-mode))
                            (mercit-file-relative-name
                             nil (not mercit-buffer-file-name))))
                 (line (format "%i,+1" (line-number-at-pos))))
             (cond (file (with-temp-buffer
                           (mercit-with-toplevel
                             (mercit-git-insert
                              "blame" "--porcelain"
                              (if (memq mercit-blame-type '(final removal))
                                  (cons "--reverse" (mercit-blame-arguments))
                                (mercit-blame-arguments))
                              "-L" line rev "--" file)
                             (goto-char (point-min))
                             (if (eobp)
                                 (unless noerror
                                   (error "Cannot get blame chunk at eob"))
                               (car (mercit-blame--parse-chunk type))))))
                   (noerror nil)
                   (t (error "Buffer does not visit a tracked file")))))))

(defun mercit-blame-chunk-at (pos)
  (--some (overlay-get it 'mercit-blame-chunk)
          (overlays-at pos)))

(defun mercit-blame--overlay-at (&optional pos key)
  (unless pos
    (setq pos (point)))
  (--first (overlay-get it (or key 'mercit-blame-chunk))
           (nconc (overlays-at pos)
                  (overlays-in pos pos))))

;;; Keymaps

(defvar mercit-blame-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-q") 'mercit-blame-quit)
    map)
  "Keymap for `mercit-blame-mode'.
Note that most blaming key bindings are defined
in `mercit-blame-read-only-mode-map' instead.")

(defvar mercit-blame-read-only-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-m")   #'mercit-show-commit)
    (define-key map (kbd   "p")   #'mercit-blame-previous-chunk)
    (define-key map (kbd   "P")   #'mercit-blame-previous-chunk-same-commit)
    (define-key map (kbd   "n")   #'mercit-blame-next-chunk)
    (define-key map (kbd   "N")   #'mercit-blame-next-chunk-same-commit)
    (define-key map (kbd   "b")   #'mercit-blame-addition)
    (define-key map (kbd   "r")   #'mercit-blame-removal)
    (define-key map (kbd   "f")   #'mercit-blame-reverse)
    (define-key map (kbd   "B")   #'mercit-blame)
    (define-key map (kbd   "c")   #'mercit-blame-cycle-style)
    (define-key map (kbd   "q")   #'mercit-blame-quit)
    (define-key map (kbd "M-w")   #'mercit-blame-copy-hash)
    (define-key map (kbd "SPC")   #'mercit-diff-show-or-scroll-up)
    (define-key map (kbd "S-SPC") #'mercit-diff-show-or-scroll-down)
    (define-key map (kbd "DEL")   #'mercit-diff-show-or-scroll-down)
    map)
  "Keymap for `mercit-blame-read-only-mode'.")

;;; Modes
;;;; Base Mode

(define-minor-mode mercit-blame-mode
  "Display blame information inline."
  :lighter mercit-blame-mode-lighter
  (cond (mercit-blame-mode
         (when (called-interactively-p 'any)
           (setq mercit-blame-mode nil)
           (user-error
            (concat "Don't call `mercit-blame-mode' directly; "
                    "instead use `mercit-blame'")))
         (add-hook 'after-save-hook     #'mercit-blame--refresh t t)
         (add-hook 'post-command-hook   #'mercit-blame-goto-chunk-hook t t)
         (add-hook 'before-revert-hook  #'mercit-blame--remove-overlays t t)
         (add-hook 'after-revert-hook   #'mercit-blame--refresh t t)
         (add-hook 'read-only-mode-hook #'mercit-blame-toggle-read-only t t)
         (setq mercit-blame-buffer-read-only buffer-read-only)
         (when (or mercit-blame-read-only mercit-buffer-file-name)
           (read-only-mode 1))
         (dolist (mode mercit-blame-disable-modes)
           (when (and (boundp mode) (symbol-value mode))
             (funcall mode -1)
             (push mode mercit-blame-disabled-modes)))
         (setq mercit-blame-separator (mercit-blame--format-separator))
         (unless mercit-blame--style
           (setq mercit-blame--style (car mercit-blame-styles)))
         (setq mercit-blame--make-margin-overlays
               (and (cl-find-if (lambda (style)
                                  (assq 'margin-format (cdr style)))
                                mercit-blame-styles)))
         (mercit-blame--update-margin))
        (t
         (when (process-live-p mercit-blame-process)
           (kill-process mercit-blame-process)
           (while mercit-blame-process
             (sit-for 0.01))) ; avoid racing the sentinel
         (remove-hook 'after-save-hook     #'mercit-blame--refresh t)
         (remove-hook 'post-command-hook   #'mercit-blame-goto-chunk-hook t)
         (remove-hook 'before-revert-hook  #'mercit-blame--remove-overlays t)
         (remove-hook 'after-revert-hook   #'mercit-blame--refresh t)
         (remove-hook 'read-only-mode-hook #'mercit-blame-toggle-read-only t)
         (unless mercit-blame-buffer-read-only
           (read-only-mode -1))
         (mercit-blame-read-only-mode -1)
         (dolist (mode mercit-blame-disabled-modes)
           (funcall mode 1))
         (kill-local-variable 'mercit-blame-disabled-modes)
         (kill-local-variable 'mercit-blame-type)
         (kill-local-variable 'mercit-blame--style)
         (mercit-blame--update-margin)
         (mercit-blame--remove-overlays))))

(defun mercit-blame--refresh ()
  (mercit-blame--run (mercit-blame-arguments)))

(defun mercit-blame-goto-chunk-hook ()
  (let ((chunk (mercit-blame-chunk-at (point))))
    (when (cl-typep chunk 'mercit-blame-chunk)
      (unless (eq chunk mercit-blame-previous-chunk)
        (run-hooks 'mercit-blame-goto-chunk-hook))
      (setq mercit-blame-previous-chunk chunk))))

(defun mercit-blame-toggle-read-only ()
  (mercit-blame-read-only-mode (if buffer-read-only 1 -1)))

;;;; Read-Only Mode

(define-minor-mode mercit-blame-read-only-mode
  "Provide keybindings for Mercit-Blame mode.

This minor-mode provides the key bindings for Mercit-Blame mode,
but only when Read-Only mode is also enabled because these key
bindings would otherwise conflict badly with regular bindings.

When both Mercit-Blame mode and Read-Only mode are enabled, then
this mode gets automatically enabled too and when one of these
modes is toggled, then this mode also gets toggled automatically.

\\{mercit-blame-read-only-mode-map}")

;;;; Kludges

(defun mercit-blame-put-keymap-before-view-mode ()
  "Put `mercit-blame-read-only-mode' ahead of `view-mode' in `minor-mode-map-alist'."
  (--when-let (assq 'mercit-blame-read-only-mode
                    (cl-member 'view-mode minor-mode-map-alist :key #'car))
    (setq minor-mode-map-alist
          (cons it (delq it minor-mode-map-alist))))
  (remove-hook 'view-mode-hook #'mercit-blame-put-keymap-before-view-mode))

(add-hook 'view-mode-hook #'mercit-blame-put-keymap-before-view-mode)

;;; Process

(defun mercit-blame--run (args)
  (mercit-with-toplevel
    (unless mercit-blame-mode
      (mercit-blame-mode 1))
    (message "Blaming...")
    (mercit-blame-run-process
     (or mercit-buffer-refname mercit-buffer-revision)
     (mercit-file-relative-name nil (not mercit-buffer-file-name))
     (if (memq mercit-blame-type '(final removal))
         (cons "--reverse" args)
       args)
     (list (line-number-at-pos (window-start))
           (line-number-at-pos (1- (window-end nil t)))))
    (set-process-sentinel mercit-this-process
                          #'mercit-blame-process-quickstart-sentinel)))

(defun mercit-blame-run-process (revision file args &optional lines)
  (let ((process (mercit-parse-git-async
                  "blame" "--incremental" args
                  (and lines (list "-L" (apply #'format "%s,%s" lines)))
                  revision "--" file)))
    (set-process-filter   process #'mercit-blame-process-filter)
    (set-process-sentinel process #'mercit-blame-process-sentinel)
    (process-put process 'arguments (list revision file args))
    (setq mercit-blame-cache (make-hash-table :test #'equal))
    (setq mercit-blame-process process)))

(defun mercit-blame-process-quickstart-sentinel (process event)
  (when (memq (process-status process) '(exit signal))
    (mercit-blame-process-sentinel process event t)
    (mercit-blame-assert-buffer process)
    (with-current-buffer (process-get process 'command-buf)
      (when mercit-blame-mode
        (let ((default-directory (mercit-toplevel)))
          (apply #'mercit-blame-run-process
                 (process-get process 'arguments)))))))

(defun mercit-blame-process-sentinel (process _event &optional quiet)
  (let ((status (process-status process)))
    (when (memq status '(exit signal))
      (kill-buffer (process-buffer process))
      (if (and (eq status 'exit)
               (zerop (process-exit-status process)))
          (unless quiet
            (message "Blaming...done"))
        (mercit-blame-assert-buffer process)
        (with-current-buffer (process-get process 'command-buf)
          (if mercit-blame-mode
              (progn (mercit-blame-mode -1)
                     (message "Blaming...failed"))
            (message "Blaming...aborted"))))
      (kill-local-variable 'mercit-blame-process))))

(defun mercit-blame-process-filter (process string)
  (internal-default-process-filter process string)
  (let ((buf  (process-get process 'command-buf))
        (pos  (process-get process 'parsed))
        (mark (process-mark process))
        type cache)
    (with-current-buffer buf
      (setq type  mercit-blame-type)
      (setq cache mercit-blame-cache))
    (with-current-buffer (process-buffer process)
      (goto-char pos)
      (while (and (< (point) mark)
                  (save-excursion (re-search-forward "^filename .+\n" nil t)))
        (pcase-let* ((`(,chunk ,revinfo)
                      (mercit-blame--parse-chunk type))
                     (rev (oref chunk orig-rev)))
          (if revinfo
              (puthash rev revinfo cache)
            (setq revinfo
                  (or (gethash rev cache)
                      (puthash rev (mercit-blame--commit-alist rev) cache))))
          (mercit-blame--make-overlays buf chunk revinfo))
        (process-put process 'parsed (point))))))

(defun mercit-blame--parse-chunk (type)
  (let (chunk revinfo)
    (unless (looking-at "^\\(.\\{40,\\}\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)")
      (error "Blaming failed due to unexpected output: %s"
             (buffer-substring-no-properties (point) (line-end-position))))
    (with-slots (orig-rev orig-file prev-rev prev-file)
        (setq chunk (mercit-blame-chunk
                     :orig-rev                     (match-string 1)
                     :orig-line  (string-to-number (match-string 2))
                     :final-line (string-to-number (match-string 3))
                     :num-lines  (string-to-number (match-string 4))))
      (forward-line)
      (let (done)
        (while (not done)
          (cond ((looking-at "^filename \\(.+\\)")
                 (setq done t)
                 (setf orig-file (mercit-decode-git-path (match-string 1))))
                ((looking-at "^previous \\(.\\{40,\\}\\) \\(.+\\)")
                 (setf prev-rev  (match-string 1))
                 (setf prev-file (mercit-decode-git-path (match-string 2))))
                ((looking-at "^\\([^ ]+\\) \\(.+\\)")
                 (push (cons (match-string 1)
                             (match-string 2)) revinfo)))
          (forward-line)))
      (when (and (eq type 'removal) prev-rev)
        (cl-rotatef orig-rev  prev-rev)
        (cl-rotatef orig-file prev-file)
        (setq revinfo nil)))
    (list chunk revinfo)))

(defun mercit-blame--commit-alist (rev)
  (cl-mapcar 'cons
             '("summary"
               "author" "author-time" "author-tz"
               "committer" "committer-time" "committer-tz")
             (split-string (mercit-rev-format "%s\v%an\v%ad\v%cn\v%cd" rev
                                             "--date=format:%s\v%z")
                           "\v")))

(defun mercit-blame-assert-buffer (process)
  (unless (buffer-live-p (process-get process 'command-buf))
    (kill-process process)
    (user-error "Buffer being blamed has been killed")))

;;; Display

(defsubst mercit-blame--style-get (key)
  (cdr (assoc key (cdr mercit-blame--style))))

(defun mercit-blame--make-overlays (buf chunk revinfo)
  (with-current-buffer buf
    (save-excursion
      (save-restriction
        (widen)
        (let* ((line (oref chunk final-line))
               (beg (mercit-blame--line-beginning-position line))
               (end (mercit-blame--line-beginning-position
                     (+ line (oref chunk num-lines))))
               (before (mercit-blame-chunk-at (1- beg))))
          (when (and before
                     (equal (oref before orig-rev)
                            (oref chunk orig-rev)))
            (setq beg (mercit-blame--line-beginning-position
                       (oset chunk final-line (oref before final-line))))
            (cl-incf (oref chunk num-lines)
                     (oref before num-lines)))
          (mercit-blame--remove-overlays beg end)
          (when mercit-blame--make-margin-overlays
            (mercit-blame--make-margin-overlays chunk revinfo beg end))
          (mercit-blame--make-heading-overlay chunk revinfo beg end)
          (mercit-blame--make-highlight-overlay chunk beg))))))

(defun mercit-blame--line-beginning-position (line)
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (point)))

(defun mercit-blame--make-margin-overlays (chunk revinfo beg end)
  (save-excursion
    (let ((line 0))
      (goto-char beg)
      (while (< (point) end)
        (mercit-blame--make-margin-overlay chunk revinfo line)
        (forward-line)
        (cl-incf line)))))

(defun mercit-blame--make-margin-overlay (chunk revinfo line)
  (let* ((end (line-end-position))
         ;; If possible avoid putting this on the first character
         ;; of the line to avoid a conflict with the line overlay.
         (beg (min (1+ (line-beginning-position)) end))
         (ov  (make-overlay beg end)))
    (overlay-put ov 'mercit-blame-chunk chunk)
    (overlay-put ov 'mercit-blame-revinfo revinfo)
    (overlay-put ov 'mercit-blame-margin line)
    (mercit-blame--update-margin-overlay ov)))

(defun mercit-blame--make-heading-overlay (chunk revinfo beg end)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'mercit-blame-chunk chunk)
    (overlay-put ov 'mercit-blame-revinfo revinfo)
    (overlay-put ov 'mercit-blame-heading t)
    (mercit-blame--update-heading-overlay ov)))

(defun mercit-blame--make-highlight-overlay (chunk beg)
  (let ((ov (make-overlay beg (save-excursion
                                (goto-char beg)
                                (1+ (line-end-position))))))
    (overlay-put ov 'mercit-blame-chunk chunk)
    (overlay-put ov 'mercit-blame-highlight t)
    (mercit-blame--update-highlight-overlay ov)))

(defun mercit-blame--update-margin ()
  (setq left-margin-width (or (mercit-blame--style-get 'margin-width) 0))
  (set-window-buffer (selected-window) (current-buffer)))

(defun mercit-blame--update-overlays ()
  (save-restriction
    (widen)
    (dolist (ov (overlays-in (point-min) (point-max)))
      (cond ((overlay-get ov 'mercit-blame-heading)
             (mercit-blame--update-heading-overlay ov))
            ((overlay-get ov 'mercit-blame-margin)
             (mercit-blame--update-margin-overlay ov))
            ((overlay-get ov 'mercit-blame-highlight)
             (mercit-blame--update-highlight-overlay ov))))))

(defun mercit-blame--update-margin-overlay (ov)
  (overlay-put
   ov 'before-string
   (and (mercit-blame--style-get 'margin-width)
        (propertize
         "o" 'display
         (list (list 'margin 'left-margin)
               (let ((line   (overlay-get ov 'mercit-blame-margin))
                     (format (mercit-blame--style-get 'margin-format))
                     (face   (mercit-blame--style-get 'margin-face)))
                 (mercit-blame--format-string
                  ov
                  (or (and (atom format)
                           format)
                      (nth line format)
                      (car (last format)))
                  (or (and (not (zerop line))
                           (mercit-blame--style-get 'margin-body-face))
                      face
                      'mercit-blame-margin))))))))

(defun mercit-blame--update-heading-overlay (ov)
  (overlay-put
   ov 'before-string
   (--if-let (mercit-blame--style-get 'heading-format)
       (mercit-blame--format-string ov it 'mercit-blame-heading)
     (and (mercit-blame--style-get 'show-lines)
          (or (not (mercit-blame--style-get 'margin-format))
              (save-excursion
                (goto-char (overlay-start ov))
                ;; Special case of the special case described in
                ;; `mercit-blame--make-margin-overlay'.  For empty
                ;; lines it is not possible to show both overlays
                ;; without the line being to high.
                (not (= (point) (line-end-position)))))
          mercit-blame-separator))))

(defun mercit-blame--update-highlight-overlay (ov)
  (overlay-put ov 'font-lock-face (mercit-blame--style-get 'highlight-face)))

(defun mercit-blame--format-string (ov format face)
  (let* ((chunk   (overlay-get ov 'mercit-blame-chunk))
         (revinfo (overlay-get ov 'mercit-blame-revinfo))
         (key     (list format face))
         (string  (cdr (assoc key revinfo))))
    (unless string
      (setq string
            (and format
                 (mercit-blame--format-string-1 (oref chunk orig-rev)
                                               revinfo format face)))
      (nconc revinfo (list (cons key string))))
    string))

(defun mercit-blame--format-string-1 (rev revinfo format face)
  (let ((str
         (if (string-match-p "\\`0\\{40,\\}\\'" rev)
             (propertize (concat (if (string-prefix-p "\s" format) "\s" "")
                                 "Not Yet Committed"
                                 (if (string-suffix-p "\n" format) "\n" ""))
                         'font-lock-face face)
           (mercit--format-spec
            (propertize format 'font-lock-face face)
            (cl-flet* ((p0 (s f)
                         (propertize s 'font-lock-face
                                     (if face
                                         (if (listp face)
                                             face
                                           (list f face))
                                       f)))
                       (p1 (k f)
                         (p0 (cdr (assoc k revinfo)) f))
                       (p2 (k1 k2 f)
                         (p0 (mercit-blame--format-time-string
                              (cdr (assoc k1 revinfo))
                              (cdr (assoc k2 revinfo)))
                             f)))
              `((?H . ,(p0 rev         'mercit-blame-hash))
                (?s . ,(p1 "summary"   'mercit-blame-summary))
                (?a . ,(p1 "author"    'mercit-blame-name))
                (?c . ,(p1 "committer" 'mercit-blame-name))
                (?A . ,(p2 "author-time"    "author-tz"    'mercit-blame-date))
                (?C . ,(p2 "committer-time" "committer-tz" 'mercit-blame-date))
                (?f . "")))))))
    (if-let ((width (and (string-suffix-p "%f" format)
                         (mercit-blame--style-get 'margin-width))))
        (concat str
                (propertize (make-string (max 0 (- width (length str))) ?\s)
                            'font-lock-face face))
      str)))

(defun mercit-blame--format-separator ()
  (propertize
   (concat (propertize "\s" 'display '(space :height (2)))
           (propertize "\n" 'line-height t))
   'font-lock-face `(:background
                     ,(face-attribute 'mercit-blame-heading
                                      :background nil t)
                     ,@(and (>= emacs-major-version 27) '(:extend t)))))

(defun mercit-blame--format-time-string (time tz)
  (let* ((time-format (or (mercit-blame--style-get 'time-format)
                          mercit-blame-time-format))
         (tz-in-second (and (string-search "%z" time-format)
                            (car (last (parse-time-string tz))))))
    (format-time-string time-format
                        (seconds-to-time (string-to-number time))
                        tz-in-second)))

(defun mercit-blame--remove-overlays (&optional beg end)
  (save-restriction
    (widen)
    (dolist (ov (overlays-in (or beg (point-min))
                             (or end (point-max))))
      (when (overlay-get ov 'mercit-blame-chunk)
        (delete-overlay ov)))))

(defun mercit-blame-maybe-show-message ()
  (when (mercit-blame--style-get 'show-message)
    (let ((message-log-max 0))
      (if-let ((msg (cdr (assoc "summary"
                                (gethash (oref (mercit-current-blame-chunk)
                                               orig-rev)
                                         mercit-blame-cache)))))
          (progn (set-text-properties 0 (length msg) nil msg)
                 (message msg))
        (message "Commit data not available yet.  Still blaming.")))))

;;; Commands

;;;###autoload (autoload 'mercit-blame-echo "mercit-blame" nil t)
(transient-define-suffix mercit-blame-echo (args)
  "For each line show the revision in which it was added.
Show the information about the chunk at point in the echo area
when moving between chunks.  Unlike other blaming commands, do
not turn on `read-only-mode'."
  :if (lambda ()
        (and buffer-file-name
             (or (not mercit-blame-mode)
                 buffer-read-only)))
  (interactive (list (mercit-blame-arguments)))
  (when mercit-buffer-file-name
    (user-error "Blob buffers aren't supported"))
  (setq-local mercit-blame--style
              (assq mercit-blame-echo-style mercit-blame-styles))
  (setq-local mercit-blame-disable-modes
              (cons 'eldoc-mode mercit-blame-disable-modes))
  (if (not mercit-blame-mode)
      (let ((mercit-blame-read-only nil))
        (mercit-blame--pre-blame-assert 'addition)
        (mercit-blame--pre-blame-setup  'addition)
        (mercit-blame--run args))
    (read-only-mode -1)
    (mercit-blame--update-overlays)))

;;;###autoload (autoload 'mercit-blame-addition "mercit-blame" nil t)
(transient-define-suffix mercit-blame-addition (args)
  "For each line show the revision in which it was added."
  (interactive (list (mercit-blame-arguments)))
  (mercit-blame--pre-blame-assert 'addition)
  (mercit-blame--pre-blame-setup  'addition)
  (mercit-blame--run args))

;;;###autoload (autoload 'mercit-blame-removal "mercit-blame" nil t)
(transient-define-suffix mercit-blame-removal (args)
  "For each line show the revision in which it was removed."
  :if-nil 'buffer-file-name
  (interactive (list (mercit-blame-arguments)))
  (unless mercit-buffer-file-name
    (user-error "Only blob buffers can be blamed in reverse"))
  (mercit-blame--pre-blame-assert 'removal)
  (mercit-blame--pre-blame-setup  'removal)
  (mercit-blame--run args))

;;;###autoload (autoload 'mercit-blame-reverse "mercit-blame" nil t)
(transient-define-suffix mercit-blame-reverse (args)
  "For each line show the last revision in which it still exists."
  :if-nil 'buffer-file-name
  (interactive (list (mercit-blame-arguments)))
  (unless mercit-buffer-file-name
    (user-error "Only blob buffers can be blamed in reverse"))
  (mercit-blame--pre-blame-assert 'final)
  (mercit-blame--pre-blame-setup  'final)
  (mercit-blame--run args))

(defun mercit-blame--pre-blame-assert (type)
  (unless (mercit-toplevel)
    (mercit--not-inside-repository-error))
  (if (and mercit-blame-mode
           (eq type mercit-blame-type))
      (if-let ((chunk (mercit-current-blame-chunk)))
          (unless (oref chunk prev-rev)
            (user-error "Chunk has no further history"))
        (user-error "Commit data not available yet.  Still blaming."))
    (unless (mercit-file-relative-name nil (not mercit-buffer-file-name))
      (if buffer-file-name
          (user-error "Buffer isn't visiting a tracked file")
        (user-error "Buffer isn't visiting a file")))))

(defun mercit-blame--pre-blame-setup (type)
  (when mercit-blame-mode
    (if (eq type mercit-blame-type)
        (let ((style mercit-blame--style))
          (mercit-blame-visit-other-file)
          (setq-local mercit-blame--style style)
          (setq-local mercit-blame-recursive-p t)
          ;; Set window-start for the benefit of quickstart.
          (redisplay))
      (mercit-blame--remove-overlays)))
  (setq mercit-blame-type type))

(defun mercit-blame-visit-other-file ()
  "Visit another blob related to the current chunk."
  (interactive)
  (with-slots (prev-rev prev-file orig-line)
      (mercit-current-blame-chunk)
    (unless prev-rev
      (user-error "Chunk has no further history"))
    (mercit-with-toplevel
      (mercit-find-file prev-rev prev-file))
    ;; TODO Adjust line like mercit-diff-visit-file.
    (goto-char (point-min))
    (forward-line (1- orig-line))))

(defun mercit-blame-visit-file ()
  "Visit the blob related to the current chunk."
  (interactive)
  (with-slots (orig-rev orig-file orig-line)
      (mercit-current-blame-chunk)
    (mercit-with-toplevel
      (mercit-find-file orig-rev orig-file))
    (goto-char (point-min))
    (forward-line (1- orig-line))))

(transient-define-suffix mercit-blame-quit ()
  "Turn off Mercit-Blame mode.
If the buffer was created during a recursive blame,
then also kill the buffer."
  :if-non-nil 'mercit-blame-mode
  (interactive)
  (mercit-blame-mode -1)
  (when mercit-blame-recursive-p
    (kill-buffer)))

(defun mercit-blame-next-chunk ()
  "Move to the next chunk."
  (interactive)
  (--if-let (next-single-char-property-change (point) 'mercit-blame-chunk)
      (goto-char it)
    (user-error "No more chunks")))

(defun mercit-blame-previous-chunk ()
  "Move to the previous chunk."
  (interactive)
  (--if-let (previous-single-char-property-change (point) 'mercit-blame-chunk)
      (goto-char it)
    (user-error "No more chunks")))

(defun mercit-blame-next-chunk-same-commit (&optional previous)
  "Move to the next chunk from the same commit.\n\n(fn)"
  (interactive)
  (if-let ((rev (oref (mercit-current-blame-chunk) orig-rev)))
      (let ((pos (point)) ov)
        (save-excursion
          (while (and (not ov)
                      (not (= pos (if previous (point-min) (point-max))))
                      (setq pos (funcall
                                 (if previous
                                     #'previous-single-char-property-change
                                   #'next-single-char-property-change)
                                 pos 'mercit-blame-chunk)))
            (--when-let (mercit-blame--overlay-at pos)
              (when (equal (oref (mercit-blame-chunk-at pos) orig-rev) rev)
                (setq ov it)))))
        (if ov
            (goto-char (overlay-start ov))
          (user-error "No more chunks from same commit")))
    (user-error "This chunk hasn't been blamed yet")))

(defun mercit-blame-previous-chunk-same-commit ()
  "Move to the previous chunk from the same commit."
  (interactive)
  (mercit-blame-next-chunk-same-commit #'previous-single-char-property-change))

(defun mercit-blame-cycle-style ()
  "Change how blame information is visualized.
Cycle through the elements of option `mercit-blame-styles'."
  (interactive)
  (setq mercit-blame--style
        (or (cadr (cl-member (car mercit-blame--style)
                             mercit-blame-styles :key #'car))
            (car mercit-blame-styles)))
  (mercit-blame--update-margin)
  (mercit-blame--update-overlays))

(defun mercit-blame-copy-hash ()
  "Save hash of the current chunk's commit to the kill ring.

When the region is active, then save the region's content
instead of the hash, like `kill-ring-save' would."
  (interactive)
  (if (use-region-p)
      (call-interactively #'copy-region-as-kill)
    (kill-new (message "%s" (oref (mercit-current-blame-chunk) orig-rev)))))

;;; Popup

;;;###autoload (autoload 'mercit-blame "mercit-blame" nil t)
(transient-define-prefix mercit-blame ()
  "Show the commits that added or removed lines in the visited file."
  :man-page "git-blame"
  :value '("-w")
  ["Arguments"
   ("-w" "Ignore whitespace" "-w")
   ("-r" "Do not treat root commits as boundaries" "--root")
   ("-P" "Follow only first parent" "--first-parent")
   (mercit-blame:-M)
   (mercit-blame:-C)]
  ["Actions"
   ("b" "Show commits adding lines" mercit-blame-addition)
   ("r" "Show commits removing lines" mercit-blame-removal)
   ("f" "Show last commits that still have lines" mercit-blame-reverse)
   ("m" "Blame echo" mercit-blame-echo)
   ("q" "Quit blaming" mercit-blame-quit)]
  ["Refresh"
   :if-non-nil mercit-blame-mode
   ("c" "Cycle style" mercit-blame-cycle-style :transient t)])

(defun mercit-blame-arguments ()
  (transient-args 'mercit-blame))

(transient-define-argument mercit-blame:-M ()
  :description "Detect lines moved or copied within a file"
  :class 'transient-option
  :argument "-M"
  :allow-empty t
  :reader #'transient-read-number-N+)

(transient-define-argument mercit-blame:-C ()
  :description "Detect lines moved or copied between files"
  :class 'transient-option
  :argument "-C"
  :allow-empty t
  :reader #'transient-read-number-N+)

;;; Utilities

(defun mercit-blame-maybe-update-revision-buffer ()
  (when-let* ((chunk  (mercit-current-blame-chunk))
              (commit (oref chunk orig-rev))
              (buffer (mercit-get-mode-buffer 'mercit-revision-mode nil t)))
    (if mercit--update-revision-buffer
        (setq mercit--update-revision-buffer (list commit buffer))
      (setq mercit--update-revision-buffer (list commit buffer))
      (run-with-idle-timer
       mercit-update-other-window-delay nil
       (lambda ()
         (pcase-let ((`(,rev ,buf) mercit--update-revision-buffer))
           (setq mercit--update-revision-buffer nil)
           (when (buffer-live-p buf)
             (let ((mercit-display-buffer-noselect t))
               (apply #'mercit-show-commit rev
                      (mercit-diff-arguments 'mercit-revision-mode))))))))))

;;; _
(provide 'mercit-blame)
;;; mercit-blame.el ends here
