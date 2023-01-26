;;; mercit-section.el --- Sections for read-only buffers  -*- lexical-binding:t; coding:utf-8 -*-

;; Copyright (C) 2023      The Mercit Project Contributors
;; Copyright (C) 2008-2023 The Magit Project Contributors

;; Homepage: https://github.com/htgoebel/mercit
;; Keywords: tools

;; Package-Version: 3.3.0.50-git
;; Package-Requires: ((emacs "25.1") (compat "28.1.1.2") (dash "2.19.1"))

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

;; This package implements the main user interface of Mercit — the
;; collapsible sections that make up its buffers.  This package used
;; to be distributed as part of Mercit but now it can also be used by
;; other packages that have nothing to do with Mercit or Mercurial.

;;; Code:

(require 'cl-lib)
(require 'compat)
(require 'dash)
(require 'eieio)
(require 'format-spec)
(require 'seq)
(require 'subr-x)

(eval-when-compile (require 'benchmark))

;; For `mercit-section-get-relative-position'
(declare-function mercit-hunk-section-p "mercit-diff" (section) t)

;;; Hooks

(defvar mercit-section-movement-hook nil
  "Hook run by `mercit-section-goto'.
That function in turn is used by all section movement commands.")

(defvar mercit-section-highlight-hook
  '(mercit-section-highlight
    mercit-section-highlight-selection)
  "Functions used to highlight the current section.
Each function is run with the current section as only argument
until one of them returns non-nil.")

(defvar mercit-section-unhighlight-hook nil
  "Functions used to unhighlight the previously current section.
Each function is run with the current section as only argument
until one of them returns non-nil.  Most sections are properly
unhighlighted without requiring a specialized unhighlighter,
diff-related sections being the only exception.")

(defvar mercit-section-set-visibility-hook
  '(mercit-section-cached-visibility)
  "Hook used to set the initial visibility of a section.
Stop at the first function that returns non-nil.  The returned
value should be `show', `hide' or nil.  If no function returns
non-nil, determine the visibility as usual, i.e. use the
hardcoded section specific default (see `mercit-insert-section').")

;;; Options

(defgroup mercit-section nil
  "Expandable sections."
  :link '(info-link "(mercit)Sections")
  :group 'extensions)

(defcustom mercit-section-show-child-count t
  "Whether to append the number of children to section headings.
This only applies to sections for which doing so makes sense."
  :package-version '(mercit-section . "0.0.0")
  :group 'mercit-section
  :type 'boolean)

(defcustom mercit-section-cache-visibility t
  "Whether to cache visibility of sections.

Sections always retain their visibility state when they are being
recreated during a refresh.  But if a section disappears and then
later reappears again, then this option controls whether this is
the case.

If t, then cache the visibility of all sections.  If a list of
section types, then only do so for matching sections.  If nil,
then don't do so for any sections."
  :package-version '(mercit-section . "0.0.0")
  :group 'mercit-section
  :type '(choice (const  :tag "Don't cache visibility" nil)
                 (const  :tag "Cache visibility of all sections" t)
                 (repeat :tag "Cache visibility for section types" symbol)))

(defcustom mercit-section-initial-visibility-alist
  '((stashes . hide))
  "Alist controlling the initial visibility of sections.

Each element maps a section type or lineage to the initial
visibility state for such sections.  The state has to be one of
`show' or `hide', or a function that returns one of these symbols.
A function is called with the section as the only argument.

Use the command `mercit-describe-section' to determine a section's
lineage or type.  The vector in the output is the section lineage
and the type is the first element of that vector.  Wildcards can
be used, see `mercit-section-match'.

Currently this option is only used to override hardcoded defaults,
but in the future it will also be used set the defaults.

An entry whose key is `mercit-status-initial-section' specifies
the visibility of the section `mercit-status-goto-initial-section'
jumps to.  This does not only override defaults, but also other
entries of this alist."
  :package-version '(mercit-section . "0.0.0")
  :group 'mercit-section
  :type '(alist :key-type (sexp :tag "Section type/lineage")
                :value-type (choice (const hide)
                                    (const show)
                                    function)))

(defcustom mercit-section-visibility-indicator
  (if (window-system)
      '(mercit-fringe-bitmap> . mercit-fringe-bitmapv)
    (cons (if (char-displayable-p ?…) "…" "...")
          t))
  "Whether and how to indicate that a section can be expanded/collapsed.

If nil, then don't show any indicators.
Otherwise the value has to have one of these two forms:

\(EXPANDABLE-BITMAP . COLLAPSIBLE-BITMAP)

  Both values have to be variables whose values are fringe
  bitmaps.  In this case every section that can be expanded or
  collapsed gets an indicator in the left fringe.

  To provide extra padding around the indicator, set
  `left-fringe-width' in `mercit-mode-hook'.

\(STRING . BOOLEAN)

  In this case STRING (usually an ellipsis) is shown at the end
  of the heading of every collapsed section.  Expanded sections
  get no indicator.  The cdr controls whether the appearance of
  these ellipsis take section highlighting into account.  Doing
  so might potentially have an impact on performance, while not
  doing so is kinda ugly."
  :package-version '(mercit-section . "0.0.0")
  :group 'mercit-section
  :type '(choice (const :tag "No indicators" nil)
                 (cons  :tag "Use +- fringe indicators"
                        (const mercit-fringe-bitmap+)
                        (const mercit-fringe-bitmap-))
                 (cons  :tag "Use >v fringe indicators"
                        (const mercit-fringe-bitmap>)
                        (const mercit-fringe-bitmapv))
                 (cons  :tag "Use bold >v fringe indicators)"
                        (const mercit-fringe-bitmap-bold>)
                        (const mercit-fringe-bitmap-boldv))
                 (cons  :tag "Use custom fringe indicators"
                        (variable :tag "Expandable bitmap variable")
                        (variable :tag "Collapsible bitmap variable"))
                 (cons  :tag "Use ellipses at end of headings"
                        (string :tag "Ellipsis" "…")
                        (choice :tag "Use face kludge"
                                (const :tag "Yes (potentially slow)" t)
                                (const :tag "No (kinda ugly)" nil)))))

(define-obsolete-variable-alias 'mercit-keep-region-overlay
  'mercit-section-keep-region-overlay "Mercit-Section 3.4.0")
(defcustom mercit-section-keep-region-overlay nil
  "Whether to keep the region overlay when there is a valid selection.

By default Mercit removes the regular region overlay if, and only
if, that region constitutes a valid selection as understood by
Mercit commands.  Otherwise it does not remove that overlay, and
the region looks like it would in other buffers.

There are two types of such valid selections: hunk-internal
regions and regions that select two or more sibling sections.
In such cases Mercit removes the region overlay and instead
highlights a slightly larger range.  All text (for hunk-internal
regions) or the headings of all sections (for sibling selections)
that are inside that range (not just inside the region) are acted
on by commands such as the staging command.  This buffer range
begins at the beginning of the line on which the region begins
and ends at the end of the line on which the region ends.

Because Mercit acts on this larger range and not the region, it is
actually quite important to visualize that larger range.  If we
don't do that, then one might think that these commands act on
the region instead.  If you want to *also* visualize the region,
then set this option to t.  But please note that when the region
does *not* constitute a valid selection, then the region is
*always* visualized as usual, and that it is usually under such
circumstances that you want to use a non-mercit command to act on
the region.

Besides keeping the region overlay, setting this option to t also
causes all face properties, except for `:foreground', to be
ignored for the faces used to highlight headings of selected
sections.  This avoids the worst conflicts that result from
displaying the region and the selection overlays at the same
time.  We are not interested in dealing with other conflicts.
In fact we *already* provide a way to avoid all of these
conflicts: *not* changing the value of this option.

It should be clear by now that we consider it a mistake to set
this to display the region when the Mercit selection is also
visualized, but since it has been requested a few times and
because it doesn't cost much to offer this option we do so.
However that might change.  If the existence of this option
starts complicating other things, then it will be removed."
  :package-version '(mercit-section . "0.0.0")
  :group 'mercit-section
  :type 'boolean)

(defcustom mercit-section-disable-line-numbers t
  "In Mercit buffers, whether to disable modes that display line numbers.

Some users who turn on `global-display-line-numbers-mode' (or
`global-nlinum-mode' or `global-linum-mode') expect line numbers
to be displayed everywhere except in Mercit buffers.  Other users
do not expect Mercit buffers to be treated differently.  At least
in theory users in the first group should not use the global mode,
but that ship has sailed, thus this option."
  :package-version '(mercit-section . "0.0.0")
  :group 'mercit-section
  :type 'boolean)

(defcustom mercit-section-show-context-menu-for-emacs<28 nil
  "Whether `mouse-3' shows a context menu for Emacs < 28.

This has to be set before loading `mercit-section' or it has
no effect.  This also has no effect for Emacs >= 28, where
`context-menu-mode' should be enabled instead."
  :package-version '(mercit-section . "0.0.0")
  :group 'mercit-section
  :type 'boolean)

;;; Variables

(defvar-local mercit-section-pre-command-region-p nil)
(defvar-local mercit-section-pre-command-section nil)
(defvar-local mercit-section-highlight-force-update nil)
(defvar-local mercit-section-highlight-overlays nil)
(defvar-local mercit-section-highlighted-sections nil)
(defvar-local mercit-section-unhighlight-sections nil)

;;; Faces

(defgroup mercit-section-faces nil
  "Faces used by Mercit-Section."
  :group 'mercit-section
  :group 'faces)

(defface mercit-section-highlight
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "grey95")
    (((class color) (background  dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "grey20"))
  "Face for highlighting the current section."
  :group 'mercit-section-faces)

(defface mercit-section-heading
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "DarkGoldenrod4"
     :weight bold)
    (((class color) (background  dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "LightGoldenrod2"
     :weight bold))
  "Face for section headings."
  :group 'mercit-section-faces)

(defface mercit-section-secondary-heading
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :weight bold))
  "Face for section headings of some secondary headings."
  :group 'mercit-section-faces)

(defface mercit-section-heading-selection
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "salmon4")
    (((class color) (background  dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "LightSalmon3"))
  "Face for selected section headings."
  :group 'mercit-section-faces)

(defface mercit-section-child-count '((t nil))
  "Face used for child counts at the end of some section headings."
  :group 'mercit-section-faces)

;;; Classes

(defvar mercit--current-section-hook nil
  "Internal variable used for `mercit-describe-section'.")

(defvar mercit--section-type-alist nil)

(defclass mercit-section ()
  ((keymap   :initform nil)
   (type     :initform nil :initarg :type)
   (value    :initform nil :initarg :value)
   (start    :initform nil :initarg :start)
   (content  :initform nil)
   (end      :initform nil)
   (hidden   :initform nil)
   (washer   :initform nil)
   (process  :initform nil)
   (heading-highlight-face :initform nil)
   (inserter :initform (symbol-value 'mercit--current-section-hook))
   (parent   :initform nil :initarg :parent)
   (children :initform nil)))

;;; Mode

(defvar symbol-overlay-inhibit-map)

(defvar mercit-section-heading-map
  (let ((map (make-sparse-keymap)))
    (define-key map [double-down-mouse-1] #'ignore)
    (define-key map [double-mouse-1] #'mercit-mouse-toggle-section)
    (define-key map [double-mouse-2] #'mercit-mouse-toggle-section)
    map)
  "Keymap used in the heading line of all expandable sections.
This keymap is used in addition to the section-specific keymap,
if any.")

(defvar mercit-section-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (when (and mercit-section-show-context-menu-for-emacs<28
               (< emacs-major-version 28))
      (define-key map [mouse-3] nil)
      (define-key
       map [down-mouse-3]
       `( menu-item "" ,(make-sparse-keymap)
          :filter ,(lambda (_)
                     (let ((menu (make-sparse-keymap)))
                       (if (fboundp 'context-menu-local)
                           (context-menu-local menu last-input-event)
                         (mercit--context-menu-local menu last-input-event))
                       (mercit-section-context-menu menu last-input-event)
                       menu)))))
    (define-key map [left-fringe mouse-1] #'mercit-mouse-toggle-section)
    (define-key map [left-fringe mouse-2] #'mercit-mouse-toggle-section)
    (define-key map (kbd "TAB") #'mercit-section-toggle)
    (define-key map [C-tab]     #'mercit-section-cycle)
    (define-key map [M-tab]     #'mercit-section-cycle)
    ;; [backtab] is the most portable binding for Shift+Tab.
    (define-key map [backtab]   #'mercit-section-cycle-global)
    (define-key map (kbd   "^") #'mercit-section-up)
    (define-key map (kbd   "p") #'mercit-section-backward)
    (define-key map (kbd   "n") #'mercit-section-forward)
    (define-key map (kbd "M-p") #'mercit-section-backward-sibling)
    (define-key map (kbd "M-n") #'mercit-section-forward-sibling)
    (define-key map (kbd   "1") #'mercit-section-show-level-1)
    (define-key map (kbd   "2") #'mercit-section-show-level-2)
    (define-key map (kbd   "3") #'mercit-section-show-level-3)
    (define-key map (kbd   "4") #'mercit-section-show-level-4)
    (define-key map (kbd "M-1") #'mercit-section-show-level-1-all)
    (define-key map (kbd "M-2") #'mercit-section-show-level-2-all)
    (define-key map (kbd "M-3") #'mercit-section-show-level-3-all)
    (define-key map (kbd "M-4") #'mercit-section-show-level-4-all)
    map)
  "Parent keymap for all keymaps of modes derived from `mercit-section-mode'.")

(define-derived-mode mercit-section-mode special-mode "Mercit-Sections"
  "Parent major mode from which major modes with Mercit-like sections inherit.

Mercit-Section is documented in info node `(mercit-section)'."
  :group 'mercit-section
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq-local line-move-visual t) ; see #1771
  ;; Turn off syntactic font locking, but not by setting
  ;; `font-lock-defaults' because that would enable font locking, and
  ;; not all mercit plugins may be ready for that (see #3950).
  (setq-local font-lock-syntactic-face-function #'ignore)
  (setq show-trailing-whitespace nil)
  (setq-local symbol-overlay-inhibit-map t)
  (setq list-buffers-directory (abbreviate-file-name default-directory))
  ;; (hack-dir-local-variables-non-file-buffer)
  (make-local-variable 'text-property-default-nonsticky)
  (push (cons 'keymap t) text-property-default-nonsticky)
  (add-hook 'pre-command-hook #'mercit-section-pre-command-hook nil t)
  (add-hook 'post-command-hook #'mercit-section-post-command-hook t t)
  (add-hook 'deactivate-mark-hook #'mercit-section-deactivate-mark t t)
  (setq-local redisplay-highlight-region-function
              #'mercit-section--highlight-region)
  (setq-local redisplay-unhighlight-region-function
              #'mercit-section--unhighlight-region)
  (when (fboundp 'mercit-section-context-menu)
    (add-hook 'context-menu-functions #'mercit-section-context-menu 10 t))
  (when mercit-section-disable-line-numbers
    (when (and (fboundp 'linum-mode)
               (bound-and-true-p global-linum-mode))
      (linum-mode -1))
    (when (and (fboundp 'nlinum-mode)
               (bound-and-true-p global-nlinum-mode))
      (nlinum-mode -1))
    (when (and (fboundp 'display-line-numbers-mode)
               (bound-and-true-p global-display-line-numbers-mode))
      (display-line-numbers-mode -1)))
  (when (fboundp 'mercit-preserve-section-visibility-cache)
    (add-hook 'kill-buffer-hook #'mercit-preserve-section-visibility-cache)))

;;; Core

(defvar-local mercit-root-section nil
  "The root section in the current buffer.
All other sections are descendants of this section.  The value
of this variable is set by `mercit-insert-section' and you should
never modify it.")
(put 'mercit-root-section 'permanent-local t)

(defvar-local mercit--context-menu-section nil "For internal use only.")

(defvar mercit--context-menu-buffer nil "For internal use only.")

(defun mercit-point ()
  "Return point or the position where the context menu was invoked.
When using the context menu, return the position the user clicked
on, provided the current buffer is the buffer in which the click
occurred.  Otherwise return the same value as `point'."
  (if mercit--context-menu-section
      (mercit-menu-position)
    (point)))

(defun mercit-thing-at-point (thing &optional no-properties)
  "Return the THING at point or where the context menu was invoked.
When using the context menu, return the thing the user clicked
on, provided the current buffer is the buffer in which the click
occurred.  Otherwise return the same value as `thing-at-point'.
For the meaning of THING and NO-PROPERTIES see that function."
  (if-let ((pos (mercit-menu-position)))
      (save-excursion
        (goto-char pos)
        (thing-at-point thing no-properties))
    (thing-at-point thing no-properties)))

(defun mercit-current-section ()
  "Return the section at point or where the context menu was invoked.
When using the context menu, return the section that the user
clicked on, provided the current buffer is the buffer in which
the click occurred.  Otherwise return the section at point."
  (or mercit--context-menu-section
      (mercit-section-at)
      mercit-root-section))

(defun mercit-section-at (&optional position)
  "Return the section at POSITION, defaulting to point."
  (get-text-property (or position (point)) 'mercit-section))

(defun mercit-section-ident (section)
  "Return an unique identifier for SECTION.
The return value has the form ((TYPE . VALUE)...)."
  (cons (cons (oref section type)
              (mercit-section-ident-value section))
        (and-let* ((parent (oref section parent)))
          (mercit-section-ident parent))))

(cl-defgeneric mercit-section-ident-value (object)
  "Return OBJECT's value, making it constant and unique if necessary.

This is used to correlate different incarnations of the same
section, see `mercit-section-ident' and `mercit-get-section'.

Sections whose values that are not constant and/or unique should
implement a method that return a value that can be used for this
purpose.")

(cl-defmethod mercit-section-ident-value ((section mercit-section))
  "Return the value unless it is an object.

Different object incarnations representing the same value then to
not be equal, so call this generic function on the object itself
to determine a constant value."
  (let ((value (oref section value)))
    (if (eieio-object-p value)
        (mercit-section-ident-value value)
      value)))

(cl-defmethod mercit-section-ident-value ((object eieio-default-superclass))
  "Simply return the object itself.  That likely isn't
good enough, so you need to implement your own method."
  object)

(defun mercit-get-section (ident &optional root)
  "Return the section identified by IDENT.
IDENT has to be a list as returned by `mercit-section-ident'.
If optional ROOT is non-nil, then search in that section tree
instead of in the one whose root `mercit-root-section' is."
  (setq ident (reverse ident))
  (let ((section (or root mercit-root-section)))
    (when (eq (car (pop ident))
              (oref section type))
      (while (and ident
                  (pcase-let ((`(,type . ,value) (car ident)))
                    (setq section
                          (cl-find-if
                           (lambda (section)
                             (and (eq (oref section type) type)
                                  (equal (mercit-section-ident-value section)
                                         value)))
                           (oref section children)))))
        (pop ident))
      section)))

(defun mercit-section-lineage (section)
  "Return the lineage of SECTION.
The return value has the form (TYPE...)."
  (cons (oref section type)
        (and-let* ((parent (oref section parent)))
          (mercit-section-lineage parent))))

(defvar mercit-insert-section--current nil "For internal use only.")
(defvar mercit-insert-section--parent  nil "For internal use only.")
(defvar mercit-insert-section--oldroot nil "For internal use only.")

;;; Menu

(defvar mercit-menu-common-value nil "See function `mercit-menu-common-value'.")
(defvar mercit-menu--desc-values nil "For internal use only.")

(defun mercit-section-context-menu (menu click)
  "Populate MENU with Mercit-Section commands at CLICK."
  (when-let ((section (save-excursion
                        (unless (region-active-p)
                          (mouse-set-point click))
                        (mercit-section-at))))
    (unless (region-active-p)
      (setq mercit--context-menu-buffer (current-buffer))
      (if-let ((alt (save-excursion
                      (mouse-set-point click)
                      (run-hook-with-args-until-success
                       'mercit-menu-alternative-section-hook section))))
          (setq mercit--context-menu-section (setq section alt))
        (setq mercit--context-menu-section section)
        (mercit-section-update-highlight t)))
    (when (mercit-section-content-p section)
      (define-key-after menu [mercit-section-toggle]
        `(menu-item
          ,(if (oref section hidden) "Expand section" "Collapse section")
          mercit-section-toggle))
      (unless (oref section hidden)
        (when-let ((children (oref section children)))
          (when (seq-some #'mercit-section-content-p children)
            (when (seq-some (lambda (c) (oref c hidden)) children)
              (define-key-after menu [mercit-section-show-children]
                `(menu-item "Expand children"
                            mercit-section-show-children)))
            (when (seq-some (lambda (c) (not (oref c hidden))) children)
              (define-key-after menu [mercit-section-hide-children]
                `(menu-item "Collapse children"
                            mercit-section-hide-children))))))
      (define-key-after menu [separator-mercit-1] menu-bar-separator))
    (define-key-after menu [mercit-describe-section]
      `(menu-item "Describe section" mercit-describe-section))
    (when-let ((map (oref section keymap)))
      (define-key-after menu [separator-mercit-2] menu-bar-separator)
      (when (symbolp map)
        (setq map (symbol-value map)))
      (setq mercit-menu-common-value (mercit-menu-common-value section))
      (setq mercit-menu--desc-values (mercit-menu--desc-values section))
      (map-keymap (lambda (key binding)
                    (when (consp binding)
                      (define-key-after menu (vector key)
                        (copy-sequence binding))))
                  (if (fboundp 'menu-bar-keymap)
                      (menu-bar-keymap map)
                    (mercit--menu-bar-keymap map)))))
  menu)

(defun mercit-menu-set (keymap key def desc &optional props after)
  "In KEYMAP, define KEY and a menu entry for DEF.

Add the menu item (menu-item DESC DEF . PROPS) at the end of
KEYMAP, or if optional AFTER is non-nil, then after that.

Because it is so common, and would otherwise result in overlong
lines or else unsightly line wrapping, a definition [remap CMD]
can be written as just [CMD].  As a result KEY might have to be
a string when otherwise a vector would have worked.

If DESC is a string that contains a support %-spec, substitute
the expression (mercit-menu-format-desc DESC) for that.  See
`mercit-menu-format-desc'."
  (declare (indent defun))
  (when (vectorp key)
    ;; Expand the short-hand.
    (unless (eq (aref key 0) 'remap)
      (setq key (vconcat [remap] key)))
    ;; The default binding is RET, but in my configuration it
    ;; is <return>.  In that case the displayed binding would
    ;; be <CMD> instead of <return>, for unknown reasons. The
    ;; same does not happen for similar events, such as <tab>.
    (when (and (equal key [remap mercit-visit-thing])
               (boundp 'mercit-mode-map)
               (ignore-errors (eq (lookup-key mercit-mode-map [return])
                                  'mercit-visit-thing)))
      (setq key [return]))
    ;; `define-key-after' cannot deal with [remap CMD],
    ;; so we have to add the key binding separately.
    (define-key keymap key def)
    (unless (symbolp def)
      (error "When KEY is a remapping, then DEF must be a symbol: %s" def))
    (setq key (vector def)))
  (when (and (stringp desc) (string-match-p "%[tTvsmMx]" desc))
    (setq desc (list 'mercit-menu-format-desc desc)))
  (define-key-after keymap key
    `( menu-item ,desc ,def ,@props
       ;; Without this, the keys for point would be shown instead
       ;; of the relevant ones from where the click occurred.
       ,@(and (not (region-active-p))
              (list :keys
                    (lambda ()
                      (or (ignore-errors
                            (save-excursion
                              (goto-char (mercit-menu-position))
                              (key-description (where-is-internal def nil t))))
                          "")))))
    after))

(defun mercit-menu-position ()
  "Return the position where the context-menu was invoked.
If the current command wasn't invoked using the context-menu,
then return nil."
  (and mercit--context-menu-section
       (ignore-errors
         (posn-point (event-start (aref (this-command-keys-vector) 0))))))

(defun mercit-menu-highlight-point-section ()
  (setq mercit-section-highlight-force-update t)
  (if (eq (current-buffer) mercit--context-menu-buffer)
      (setq mercit--context-menu-section nil)
    (if-let ((window (get-buffer-window mercit--context-menu-buffer)))
        (with-selected-window window
          (setq mercit--context-menu-section nil)
          (mercit-section-update-highlight))
      (with-current-buffer mercit--context-menu-buffer
        (setq mercit--context-menu-section nil))))
  (setq mercit--context-menu-buffer nil))

(defvar mercit--plural-append-es '(branch))

(cl-defgeneric mercit-menu-common-value (_section)
  "Return some value to be used by multiple menu items.
This function is called by `mercit-section-context-menu', which
stores the value in `mercit-menu-common-value'.  Individual menu
items can use it, e.g., in the expression used to set their
description."
  nil)

(defun mercit-menu--desc-values (section)
  (let ((type (oref section type))
        (value (oref section value))
        (multiple (mercit-region-sections nil t)))
    (list type
          value
          (format "%s %s" type value)
          (and multiple (length multiple))
          (if (memq type mercit--plural-append-es) "es" "s"))))

(defun mercit-menu-format-desc (format)
  "Format a string based on FORMAT and menu section or selection.
The following %-specs are allowed:
%t means \"TYPE\".
%T means \"TYPE\", or \"TYPEs\" if multiple sections are selected.
%v means \"VALUE\".
%s means \"TYPE VALUE\".
%m means \"TYPE VALUE\", or \"COUNT TYPEs\" if multiple sections
   are selected.
%M means \"VALUE\", or \"COUNT TYPEs\" if multiple sections are
   selected.
%x means the value of `mercit-menu-common-value'."
  (pcase-let* ((`(,type ,value ,single ,count ,suffix) mercit-menu--desc-values)
               (multiple (and count (format "%s %s%s" count type suffix))))
    (format-spec format
                 `((?t . ,type)
                   (?T . ,(format "%s%s" type (if count suffix "")))
                   (?v . ,value)
                   (?s . ,single)
                   (?m . ,(or multiple single))
                   (?M . ,(or multiple value))
                   (?x . ,(format "%s" mercit-menu-common-value))))))

(defun mercit--menu-bar-keymap (keymap)
  "Backport of `menu-bar-keymap' for Emacs < 28.
Slight trimmed down."
  (let ((menu-bar nil))
    (map-keymap (lambda (key binding)
                  (push (cons key binding) menu-bar))
                keymap)
    (cons 'keymap (nreverse menu-bar))))

(defun mercit--context-menu-local (menu _click)
  "Backport of `context-menu-local' for Emacs < 28."
  (run-hooks 'activate-menubar-hook 'menu-bar-update-hook)
  (define-key-after menu [separator-local] menu-bar-separator)
  (let ((keymap (local-key-binding [menu-bar])))
    (when keymap
      (map-keymap (lambda (key binding)
                    (when (consp binding)
                      (define-key-after menu (vector key)
                        (copy-sequence binding))))
                  (mercit--menu-bar-keymap keymap))))
  menu)

(advice-add 'context-menu-region :around
            (lambda (fn menu click)
              "Disable in `mercit-section-mode' buffers."
              (if (derived-mode-p 'mercit-section-mode)
                  menu
                (funcall fn menu click))))

;;; Commands
;;;; Movement

(defun mercit-section-forward ()
  "Move to the beginning of the next visible section."
  (interactive)
  (if (eobp)
      (user-error "No next section")
    (let ((section (mercit-current-section)))
      (if (oref section parent)
          (let ((next (and (not (oref section hidden))
                           (not (= (oref section end)
                                   (1+ (point))))
                           (car (oref section children)))))
            (while (and section (not next))
              (unless (setq next (car (mercit-section-siblings section 'next)))
                (setq section (oref section parent))))
            (if next
                (mercit-section-goto next)
              (user-error "No next section")))
        (mercit-section-goto 1)))))

(defun mercit-section-backward ()
  "Move to the beginning of the current or the previous visible section.
When point is at the beginning of a section then move to the
beginning of the previous visible section.  Otherwise move to
the beginning of the current section."
  (interactive)
  (if (bobp)
      (user-error "No previous section")
    (let ((section (mercit-current-section)) children)
      (cond
       ((and (= (point)
                (1- (oref section end)))
             (setq children (oref section children)))
        (mercit-section-goto (car (last children))))
       ((and (oref section parent)
             (not (= (point)
                     (oref section start))))
        (mercit-section-goto section))
       (t
        (let ((prev (car (mercit-section-siblings section 'prev))))
          (if prev
              (while (and (not (oref prev hidden))
                          (setq children (oref prev children)))
                (setq prev (car (last children))))
            (setq prev (oref section parent)))
          (cond (prev
                 (mercit-section-goto prev))
                ((oref section parent)
                 (user-error "No previous section"))
                ;; Eob special cases.
                ((not (get-text-property (1- (point)) 'invisible))
                 (mercit-section-goto -1))
                (t
                 (goto-char (previous-single-property-change
                             (1- (point)) 'invisible))
                 (forward-line -1)
                 (mercit-section-goto (mercit-current-section))))))))))

(defun mercit-section-up ()
  "Move to the beginning of the parent section."
  (interactive)
  (--if-let (oref (mercit-current-section) parent)
      (mercit-section-goto it)
    (user-error "No parent section")))

(defun mercit-section-forward-sibling ()
  "Move to the beginning of the next sibling section.
If there is no next sibling section, then move to the parent."
  (interactive)
  (let ((current (mercit-current-section)))
    (if (oref current parent)
        (--if-let (car (mercit-section-siblings current 'next))
            (mercit-section-goto it)
          (mercit-section-forward))
      (mercit-section-goto 1))))

(defun mercit-section-backward-sibling ()
  "Move to the beginning of the previous sibling section.
If there is no previous sibling section, then move to the parent."
  (interactive)
  (let ((current (mercit-current-section)))
    (if (oref current parent)
        (--if-let (car (mercit-section-siblings current 'prev))
            (mercit-section-goto it)
          (mercit-section-backward))
      (mercit-section-goto -1))))

(defun mercit-section-goto (arg)
  (if (integerp arg)
      (progn (forward-line arg)
             (setq arg (mercit-current-section)))
    (goto-char (oref arg start)))
  (run-hook-with-args 'mercit-section-movement-hook arg))

(defun mercit-section-set-window-start (section)
  "Ensure the beginning of SECTION is visible."
  (unless (pos-visible-in-window-p (oref section end))
    (set-window-start (selected-window) (oref section start))))

(defmacro mercit-define-section-jumper (name heading type &optional value)
  "Define an interactive function to go some section.
Together TYPE and VALUE identify the section.
HEADING is the displayed heading of the section."
  (declare (indent defun))
  `(defun ,name (&optional expand) ,(format "\
Jump to the section \"%s\".
With a prefix argument also expand it." heading)
          (interactive "P")
          (--if-let (mercit-get-section
                     (cons (cons ',type ,value)
                           (mercit-section-ident mercit-root-section)))
              (progn (goto-char (oref it start))
                     (when expand
                       (with-local-quit (mercit-section-show it))
                       (recenter 0)))
            (message ,(format "Section \"%s\" wasn't found" heading)))))

;;;; Visibility

(defun mercit-section-show (section)
  "Show the body of the current section."
  (interactive (list (mercit-current-section)))
  (oset section hidden nil)
  (mercit-section--maybe-wash section)
  (when-let ((beg (oref section content)))
    (remove-overlays beg (oref section end) 'invisible t))
  (mercit-section-maybe-update-visibility-indicator section)
  (mercit-section-maybe-cache-visibility section)
  (dolist (child (oref section children))
    (if (oref child hidden)
        (mercit-section-hide child)
      (mercit-section-show child))))

(defun mercit-section--maybe-wash (section)
  (when-let ((washer (oref section washer)))
    (oset section washer nil)
    (let ((inhibit-read-only t)
          (mercit-insert-section--parent section)
          (content (oref section content)))
      (save-excursion
        (if (and content (< content (oref section end)))
            (funcall washer section) ; already partially washed (hunk)
          (goto-char (oref section end))
          (oset section content (point-marker))
          (funcall washer)
          (oset section end (point-marker)))))
    (setq mercit-section-highlight-force-update t)))

(defun mercit-section-hide (section)
  "Hide the body of the current section."
  (interactive (list (mercit-current-section)))
  (if (eq section mercit-root-section)
      (user-error "Cannot hide root section")
    (oset section hidden t)
    (when-let ((beg (oref section content)))
      (let ((end (oref section end)))
        (when (< beg (point) end)
          (goto-char (oref section start)))
        (remove-overlays beg end 'invisible t)
        (let ((o (make-overlay beg end)))
          (overlay-put o 'evaporate t)
          (overlay-put o 'invisible t))))
    (mercit-section-maybe-update-visibility-indicator section)
    (mercit-section-maybe-cache-visibility section)))

(defun mercit-section-toggle (section)
  "Toggle visibility of the body of the current section."
  (interactive (list (mercit-current-section)))
  (cond ((eq section mercit-root-section)
         (user-error "Cannot hide root section"))
        ((oref section hidden)
         (mercit-section-show section))
        (t (mercit-section-hide section))))

(defun mercit-section-toggle-children (section)
  "Toggle visibility of bodies of children of the current section."
  (interactive (list (mercit-current-section)))
  (let* ((children (oref section children))
         (show (--any-p (oref it hidden) children)))
    (dolist (c children)
      (oset c hidden show)))
  (mercit-section-show section))

(defun mercit-section-show-children (section &optional depth)
  "Recursively show the bodies of children of the current section.
With a prefix argument show children that deep and hide deeper
children."
  (interactive (list (mercit-current-section)))
  (mercit-section-show-children-1 section depth)
  (mercit-section-show section))

(defun mercit-section-show-children-1 (section &optional depth)
  (dolist (child (oref section children))
    (oset child hidden nil)
    (if depth
        (if (> depth 0)
            (mercit-section-show-children-1 child (1- depth))
          (mercit-section-hide child))
      (mercit-section-show-children-1 child))))

(defun mercit-section-hide-children (section)
  "Recursively hide the bodies of children of the current section."
  (interactive (list (mercit-current-section)))
  (mapc #'mercit-section-hide (oref section children)))

(defun mercit-section-show-headings (section)
  "Recursively show headings of children of the current section.
Only show the headings, previously shown text-only bodies are
hidden."
  (interactive (list (mercit-current-section)))
  (mercit-section-show-headings-1 section)
  (mercit-section-show section))

(defun mercit-section-show-headings-1 (section)
  (dolist (child (oref section children))
    (oset child hidden nil)
    (when (or (oref child children)
              (not (oref child content)))
      (mercit-section-show-headings-1 child))))

(defun mercit-section-cycle (section)
  "Cycle visibility of current section and its children."
  (interactive (list (mercit-current-section)))
  (if (oref section hidden)
      (progn (mercit-section-show section)
             (mercit-section-hide-children section))
    (let ((children (oref section children)))
      (cond ((and (--any-p (oref it hidden)   children)
                  (--any-p (oref it children) children))
             (mercit-section-show-headings section))
            ((seq-some #'mercit-section-hidden-body children)
             (mercit-section-show-children section))
            (t
             (mercit-section-hide section))))))

(defun mercit-section-cycle-global ()
  "Cycle visibility of all sections in the current buffer."
  (interactive)
  (let ((children (oref mercit-root-section children)))
    (cond ((and (--any-p (oref it hidden)   children)
                (--any-p (oref it children) children))
           (mercit-section-show-headings mercit-root-section))
          ((seq-some #'mercit-section-hidden-body children)
           (mercit-section-show-children mercit-root-section))
          (t
           (mapc #'mercit-section-hide children)))))

(defun mercit-section-hidden-body (section &optional pred)
  (--if-let (oref section children)
      (funcall (or pred #'-any-p) #'mercit-section-hidden-body it)
    (and (oref section content)
         (oref section hidden))))

(defun mercit-section-content-p (section)
  "Return non-nil if SECTION has content or an unused washer function."
  (with-slots (content end washer) section
    (and content (or (not (= content end)) washer))))

(defun mercit-section-invisible-p (section)
  "Return t if the SECTION's body is invisible.
When the body of an ancestor of SECTION is collapsed then
SECTION's body (and heading) obviously cannot be visible."
  (or (oref section hidden)
      (and-let* ((parent (oref section parent)))
        (mercit-section-invisible-p parent))))

(defun mercit-section-show-level (level)
  "Show surrounding sections up to LEVEL.
If LEVEL is negative, show up to the absolute value.
Sections at higher levels are hidden."
  (if (< level 0)
      (let ((s (mercit-current-section)))
        (setq level (- level))
        (while (> (1- (length (mercit-section-ident s))) level)
          (setq s (oref s parent))
          (goto-char (oref s start)))
        (mercit-section-show-children mercit-root-section (1- level)))
    (cl-do* ((s (mercit-current-section)
                (oref s parent))
             (i (1- (length (mercit-section-ident s)))
                (cl-decf i)))
        ((cond ((< i level) (mercit-section-show-children s (- level i 1)) t)
               ((= i level) (mercit-section-hide s) t))
         (mercit-section-goto s)))))

(defun mercit-section-show-level-1 ()
  "Show surrounding sections on first level."
  (interactive)
  (mercit-section-show-level 1))

(defun mercit-section-show-level-1-all ()
  "Show all sections on first level."
  (interactive)
  (mercit-section-show-level -1))

(defun mercit-section-show-level-2 ()
  "Show surrounding sections up to second level."
  (interactive)
  (mercit-section-show-level 2))

(defun mercit-section-show-level-2-all ()
  "Show all sections up to second level."
  (interactive)
  (mercit-section-show-level -2))

(defun mercit-section-show-level-3 ()
  "Show surrounding sections up to third level."
  (interactive)
  (mercit-section-show-level 3))

(defun mercit-section-show-level-3-all ()
  "Show all sections up to third level."
  (interactive)
  (mercit-section-show-level -3))

(defun mercit-section-show-level-4 ()
  "Show surrounding sections up to fourth level."
  (interactive)
  (mercit-section-show-level 4))

(defun mercit-section-show-level-4-all ()
  "Show all sections up to fourth level."
  (interactive)
  (mercit-section-show-level -4))

(defun mercit-mouse-toggle-section (event)
  "Toggle visibility of the clicked section.
Clicks outside either the section heading or the left fringe are
silently ignored."
  (interactive "e")
  (let* ((pos (event-start event))
         (section (mercit-section-at (posn-point pos))))
    (if (eq (posn-area pos) 'left-fringe)
        (when section
          (while (not (mercit-section-content-p section))
            (setq section (oref section parent)))
          (unless (eq section mercit-root-section)
            (goto-char (oref section start))
            (mercit-section-toggle section)))
      (mercit-section-toggle section))))

;;;; Auxiliary

(defun mercit-describe-section-briefly (section &optional ident)
  "Show information about the section at point.
With a prefix argument show the section identity instead of the
section lineage.  This command is intended for debugging purposes."
  (interactive (list (mercit-current-section) current-prefix-arg))
  (let ((str (format "#<%s %S %S %s-%s%s>"
                     (eieio-object-class section)
                     (let ((val (oref section value)))
                       (cond ((stringp val)
                              (substring-no-properties val))
                             ((and (eieio-object-p val)
                                   (fboundp 'cl-prin1-to-string))
                              (cl-prin1-to-string val))
                             (t
                              val)))
                     (if ident
                         (mercit-section-ident section)
                       (apply #'vector (mercit-section-lineage section)))
                     (and-let* ((m (oref section start)))
                       (marker-position m))
                     (if-let ((m (oref section content)))
                         (format "[%s-]" (marker-position m))
                       "")
                     (and-let* ((m (oref section end)))
                       (marker-position m)))))
    (if (called-interactively-p 'any)
        (message "%s" str)
      str)))

(cl-defmethod cl-print-object ((section mercit-section) stream)
  "Print `mercit-describe-section' result of SECTION."
  ;; Used by debug and edebug as of Emacs 26.
  (princ (mercit-describe-section-briefly section) stream))

(defun mercit-describe-section (section &optional interactive-p)
  "Show information about the section at point."
  (interactive (list (mercit-current-section) t))
  (let ((inserter-section section))
    (while (and inserter-section (not (oref inserter-section inserter)))
      (setq inserter-section (oref inserter-section parent)))
    (when (and inserter-section (oref inserter-section inserter))
      (setq section inserter-section)))
  (pcase (oref section inserter)
    (`((,hook ,fun) . ,src-src)
     (help-setup-xref `(mercit-describe-section ,section) interactive-p)
     (with-help-window (help-buffer)
       (with-current-buffer standard-output
         (insert (format-message
                  "%s\n  is inserted by `%s'\n  from `%s'"
                  (mercit-describe-section-briefly section)
                  (make-text-button (symbol-name fun) nil
                                    :type 'help-function
                                    'help-args (list fun))
                  (make-text-button (symbol-name hook) nil
                                    :type 'help-variable
                                    'help-args (list hook))))
         (pcase-dolist (`(,hook ,fun) src-src)
           (insert (format-message
                    ",\n  called by `%s'\n  from `%s'"
                    (make-text-button (symbol-name fun) nil
                                      :type 'help-function
                                      'help-args (list fun))
                    (make-text-button (symbol-name hook) nil
                                      :type 'help-variable
                                      'help-args (list hook)))))
         (insert ".\n\n")
         (insert
          (format-message
           "`%s' is "
           (make-text-button (symbol-name fun) nil
                             :type 'help-function 'help-args (list fun))))
         (describe-function-1 fun))))
    (_ (message "%s, inserter unknown"
                (mercit-describe-section-briefly section)))))

;;; Match

(cl-defun mercit-section-match
    (condition &optional (section (mercit-current-section)))
  "Return t if SECTION matches CONDITION.

SECTION defaults to the section at point.  If SECTION is not
specified and there also is no section at point, then return
nil.

CONDITION can take the following forms:
  (CONDITION...)  matches if any of the CONDITIONs matches.
  [CLASS...]      matches if the section's class is the same
                  as the first CLASS or a subclass of that;
                  the section's parent class matches the
                  second CLASS; and so on.
  [* CLASS...]    matches sections that match [CLASS...] and
                  also recursively all their child sections.
  CLASS           matches if the section's class is the same
                  as CLASS or a subclass of that; regardless
                  of the classes of the parent sections.

Each CLASS should be a class symbol, identifying a class that
derives from `mercit-section'.  For backward compatibility CLASS
can also be a \"type symbol\".  A section matches such a symbol
if the value of its `type' slot is `eq'.  If a type symbol has
an entry in `mercit--section-type-alist', then a section also
matches that type if its class is a subclass of the class that
corresponds to the type as per that alist.

Note that it is not necessary to specify the complete section
lineage as printed by `mercit-describe-section-briefly', unless
of course you want to be that precise."
  (and section (mercit-section-match-1 condition section)))

(defun mercit-section-match-1 (condition section)
  (cl-assert condition)
  (and section
       (if (listp condition)
           (--first (mercit-section-match-1 it section) condition)
         (mercit-section-match-2 (if (symbolp condition)
                                    (list condition)
                                  (cl-coerce condition 'list))
                                section))))

(defun mercit-section-match-2 (condition section)
  (if (eq (car condition) '*)
      (or (mercit-section-match-2 (cdr condition) section)
          (and-let* ((parent (oref section parent)))
            (mercit-section-match-2 condition parent)))
    (and (let ((c (car condition)))
           (if (class-p c)
               (cl-typep section c)
             (if-let ((class (cdr (assq c mercit--section-type-alist))))
                 (cl-typep section class)
               (eq (oref section type) c))))
         (or (not (setq condition (cdr condition)))
             (and-let* ((parent (oref section parent)))
               (mercit-section-match-2 condition parent))))))

(defun mercit-section-value-if (condition &optional section)
  "If the section at point matches CONDITION, then return its value.

If optional SECTION is non-nil then test whether that matches
instead.  If there is no section at point and SECTION is nil,
then return nil.  If the section does not match, then return
nil.

See `mercit-section-match' for the forms CONDITION can take."
  (and-let* ((section (or section (mercit-current-section))))
    (and (mercit-section-match condition section)
         (oref section value))))

(defmacro mercit-section-when (condition &rest body)
  "If the section at point matches CONDITION, evaluate BODY.

If the section matches, then evaluate BODY forms sequentially
with `it' bound to the section and return the value of the last
form.  If there are no BODY forms, then return the value of the
section.  If the section does not match or if there is no section
at point, then return nil.

See `mercit-section-match' for the forms CONDITION can take."
  (declare (obsolete
            "instead use `mercit-section-match' or `mercit-section-value-if'."
            "Mercit 2.90.0")
           (indent 1)
           (debug (sexp body)))
  `(--when-let (mercit-current-section)
     ;; Quoting CONDITION here often leads to double-quotes, which
     ;; isn't an issue because `mercit-section-match-1' implicitly
     ;; deals with that.  We shouldn't force users of this function
     ;; to not quote CONDITION because that would needlessly break
     ;; backward compatibility.
     (when (mercit-section-match ',condition it)
       ,@(or body '((oref it value))))))

(defmacro mercit-section-case (&rest clauses)
  "Choose among clauses on the type of the section at point.

Each clause looks like (CONDITION BODY...).  The type of the
section is compared against each CONDITION; the BODY forms of the
first match are evaluated sequentially and the value of the last
form is returned.  Inside BODY the symbol `it' is bound to the
section at point.  If no clause succeeds or if there is no
section at point, return nil.

See `mercit-section-match' for the forms CONDITION can take.
Additionally a CONDITION of t is allowed in the final clause, and
matches if no other CONDITION match, even if there is no section
at point."
  (declare (indent 0)
           (debug (&rest (sexp body))))
  `(let* ((it (mercit-current-section)))
     (cond ,@(mapcar (lambda (clause)
                       `(,(or (eq (car clause) t)
                              `(and it
                                    (mercit-section-match-1 ',(car clause) it)))
                         ,@(cdr clause)))
                     clauses))))

(defun mercit-section-match-assoc (section alist)
  "Return the value associated with SECTION's type or lineage in ALIST."
  (seq-some (pcase-lambda (`(,key . ,val))
              (and (mercit-section-match-1 key section) val))
            alist))

;;; Create

(defvar mercit-insert-section-hook nil
  "Hook run after `mercit-insert-section's BODY.
Avoid using this hook and only ever do so if you know
what you are doing and are sure there is no other way.")

(defmacro mercit-insert-section (&rest args)
  "Insert a section at point.

Create a section object of type CLASS, storing VALUE in its
`value' slot, and insert the section at point.  CLASS is a
subclass of `mercit-section' or has the form `(eval FORM)', in
which case FORM is evaluated at runtime and should return a
subclass.  In other places a sections class is oftern referred
to as its \"type\".

Many commands behave differently depending on the class of the
current section and sections of a certain class can have their
own keymap, which is specified using the `keymap' class slot.
The value of that slot should be a variable whose value is a
keymap.

For historic reasons Mercit and Forge in most cases use symbols
as CLASS that don't actually identify a class and that lack the
appropriate package prefix.  This works due to some undocumented
kludges, which are not available to other packages.

When optional HIDE is non-nil collapse the section body by
default, i.e. when first creating the section, but not when
refreshing the buffer.  Else expand it by default.  This can be
overwritten using `mercit-section-set-visibility-hook'.  When a
section is recreated during a refresh, then the visibility of
predecessor is inherited and HIDE is ignored (but the hook is
still honored).

BODY is any number of forms that actually insert the section's
heading and body.  Optional NAME, if specified, has to be a
symbol, which is then bound to the object of the section being
inserted.

Before BODY is evaluated the `start' of the section object is set
to the value of `point' and after BODY was evaluated its `end' is
set to the new value of `point'; BODY is responsible for moving
`point' forward.

If it turns out inside BODY that the section is empty, then
`mercit-cancel-section' can be used to abort and remove all
traces of the partially inserted section.  This can happen when
creating a section by washing Mercurial's output and Mercurial
didn't actually output anything this time around.

\(fn [NAME] (CLASS &optional VALUE HIDE) &rest BODY)"
  (declare (indent defun)
           (debug ([&optional symbolp]
                   (&or [("eval" form) &optional form form]
                        [symbolp &optional form form])
                   body)))
  (let ((tp (cl-gensym "type"))
        (s* (and (symbolp (car args))
                 (pop args)))
        (s  (cl-gensym "section")))
    `(let* ((,tp ,(let ((type (nth 0 (car args))))
                    (if (eq (car-safe type) 'eval)
                        (cadr type)
                      `',type)))
            (,s (funcall (if (class-p ,tp)
                             ,tp
                           (or (cdr (assq ,tp mercit--section-type-alist))
                               'mercit-section))
                         :type
                         (or (and (class-p ,tp)
                                  (car (rassq ,tp mercit--section-type-alist)))
                             ,tp)
                         :value ,(nth 1 (car args))
                         :start (point-marker)
                         :parent mercit-insert-section--parent)))
       (oset ,s hidden
             (let ((value (run-hook-with-args-until-success
                           'mercit-section-set-visibility-hook ,s)))
               (if value
                   (eq value 'hide)
                 (let ((incarnation (and mercit-insert-section--oldroot
                                         (mercit-get-section
                                          (mercit-section-ident ,s)
                                          mercit-insert-section--oldroot))))
                   (if incarnation
                       (oref incarnation hidden)
                     (let ((value (mercit-section-match-assoc
                                   ,s mercit-section-initial-visibility-alist)))
                       (if value
                           (progn
                             (when (functionp value)
                               (setq value (funcall value ,s)))
                             (eq value 'hide))
                         ,(nth 2 (car args)))))))))
       (let ((mercit-insert-section--current ,s)
             (mercit-insert-section--parent  ,s)
             (mercit-insert-section--oldroot
              (or mercit-insert-section--oldroot
                  (unless mercit-insert-section--parent
                    (prog1 mercit-root-section
                      (setq mercit-root-section ,s))))))
         (catch 'cancel-section
           ,@(if s*
                 `((let ((,s* ,s))
                     ,@(cdr args)))
               (cdr args))
           ;; `mercit-insert-section-hook' should *not* be run with
           ;; `mercit-run-section-hook' because it's a hook that runs
           ;; on section insertion, not a section inserting hook.
           (run-hooks 'mercit-insert-section-hook)
           (mercit-insert-child-count ,s)
           (set-marker-insertion-type (oref ,s start) t)
           (let* ((end (oset ,s end (point-marker)))
                  (class-map (oref ,s keymap))
                  (mercit-map (intern (format "mercit-%s-section-map"
                                             (oref ,s type))))
                  (forge-map (intern (format "forge-%s-section-map"
                                             (oref ,s type))))
                  (map (and class-map (symbol-value class-map))))
             (unless map
               (setq map (or (and (boundp mercit-map) (symbol-value mercit-map))
                             (and (boundp forge-map) (symbol-value forge-map))))
               (oset ,s keymap map))
             (save-excursion
               (goto-char (oref ,s start))
               (while (< (point) end)
                 (let ((next (or (next-single-property-change
                                  (point) 'mercit-section)
                                 end)))
                   (unless (mercit-section-at)
                     (put-text-property (point) next 'mercit-section ,s)
                     (when map
                       (put-text-property (point) next 'keymap map)))
                   (mercit-section-maybe-add-heading-map ,s)
                   (goto-char next)))))
           (if (eq ,s mercit-root-section)
               (let ((mercit-section-cache-visibility nil))
                 (mercit-section-show ,s))
             (oset (oref ,s parent) children
                   (nconc (oref (oref ,s parent) children)
                          (list ,s)))))
         ,s))))

(defun mercit-cancel-section ()
  "Cancel inserting the section that is currently being inserted.
Remove all traces of that section."
  (when mercit-insert-section--current
    (if (not (oref mercit-insert-section--current parent))
        (insert "(empty)\n")
      (delete-region (oref mercit-insert-section--current start)
                     (point))
      (setq mercit-insert-section--current nil)
      (throw 'cancel-section nil))))

(defun mercit-insert-heading (&rest args)
  "Insert the heading for the section currently being inserted.

This function should only be used inside `mercit-insert-section'.

When called without any arguments, then just set the `content'
slot of the object representing the section being inserted to
a marker at `point'.  The section should only contain a single
line when this function is used like this.

When called with arguments ARGS, which have to be strings, or
nil, then insert those strings at point.  The section should not
contain any text before this happens and afterwards it should
again only contain a single line.  If the `face' property is set
anywhere inside any of these strings, then insert all of them
unchanged.  Otherwise use the `mercit-section-heading' face for
all inserted text.

The `content' property of the section object is the end of the
heading (which lasts from `start' to `content') and the beginning
of the the body (which lasts from `content' to `end').  If the
value of `content' is nil, then the section has no heading and
its body cannot be collapsed.  If a section does have a heading,
then its height must be exactly one line, including a trailing
newline character.  This isn't enforced, you are responsible for
getting it right.  The only exception is that this function does
insert a newline character if necessary."
  (declare (indent defun))
  (when args
    (let ((heading (apply #'concat args)))
      (insert (if (or (text-property-not-all 0 (length heading)
                                             'font-lock-face nil heading)
                      (text-property-not-all 0 (length heading)
                                             'face nil heading))
                  heading
                (propertize heading 'font-lock-face 'mercit-section-heading)))))
  (unless (bolp)
    (insert ?\n))
  (when (fboundp 'mercit-maybe-make-margin-overlay)
    (mercit-maybe-make-margin-overlay))
  (oset mercit-insert-section--current content (point-marker)))

(defmacro mercit-insert-section-body (&rest body)
  "Use BODY to insert the section body, once the section is expanded.
If the section is expanded when it is created, then this is
like `progn'.  Otherwise BODY isn't evaluated until the section
is explicitly expanded."
  (declare (indent 0))
  (let ((f (cl-gensym))
        (s (cl-gensym)))
    `(let ((,f (lambda () ,@body))
           (,s mercit-insert-section--current))
       (if (oref ,s hidden)
           (oset ,s washer
                 (lambda ()
                   (funcall ,f)
                   (mercit-section-maybe-remove-heading-map ,s)
                   (mercit-section-maybe-remove-visibility-indicator ,s)))
         (funcall ,f)))))

(defun mercit-insert-headers (hook)
  (let* ((header-sections nil)
         (mercit-insert-section-hook
          (cons (lambda ()
                  (push mercit-insert-section--current
                        header-sections))
                (if (listp mercit-insert-section-hook)
                    mercit-insert-section-hook
                  (list mercit-insert-section-hook)))))
    (mercit-run-section-hook hook)
    (when header-sections
      (insert "\n")
      ;; Make the first header into the parent of the rest.
      (when (cdr header-sections)
        (cl-callf nreverse header-sections)
        (let* ((1st-header (pop header-sections))
               (header-parent (oref 1st-header parent)))
          (oset header-parent children (list 1st-header))
          (oset 1st-header children header-sections)
          (oset 1st-header content (oref (car header-sections) start))
          (oset 1st-header end (oref (car (last header-sections)) end))
          (dolist (sub-header header-sections)
            (oset sub-header parent 1st-header))
          (mercit-section-maybe-add-heading-map 1st-header))))))

(defun mercit-section-maybe-add-heading-map (section)
  (when (mercit-section-content-p section)
    (let ((start (oref section start))
          (map (oref section keymap)))
      (when (symbolp map)
        (setq map (symbol-value map)))
      (put-text-property
       start
       (save-excursion
         (goto-char start)
         (line-end-position))
       'keymap (if map
                   (make-composed-keymap
                    (list map mercit-section-heading-map))
                 mercit-section-heading-map)))))

(defun mercit-section-maybe-remove-heading-map (section)
  (with-slots (start content end keymap) section
    (when (= content end)
      (put-text-property start end 'keymap keymap))))

(defun mercit-insert-child-count (section)
  "Modify SECTION's heading to contain number of child sections.

If `mercit-section-show-child-count' is non-nil and the SECTION
has children and its heading ends with \":\", then replace that
with \" (N)\", where N is the number of child sections.

This function is called by `mercit-insert-section' after that has
evaluated its BODY.  Admittedly that's a bit of a hack."
  ;; This has to be fast, not pretty!
  (let (content count)
    (when (and mercit-section-show-child-count
               (setq count (length (oref section children)))
               (> count 0)
               (setq content (oref section content))
               (eq (char-before (1- content)) ?:))
      (save-excursion
        (goto-char (- content 2))
        (insert (concat (mercit--propertize-face " " 'mercit-section-heading)
                        (mercit--propertize-face (format "(%s)" count)
                                                'mercit-section-child-count)))
        (delete-char 1)))))

;;; Highlight

(defun mercit-section-pre-command-hook ()
  (when (and (not (bound-and-true-p transient--prefix))
             (or mercit--context-menu-buffer
                 mercit--context-menu-section)
             (not (eq (ignore-errors
                        (event-basic-type (aref (this-command-keys) 0)))
                      'mouse-3)))
    ;; This is the earliest opportunity to clean up after an aborted
    ;; context-menu because that neither causes the command that created
    ;; the menu to abort nor some abortion hook to be run.  It is not
    ;; possible to update highlighting before the first command invoked
    ;; after the menu is aborted.  Here we can only make sure it is
    ;; updated afterwards.
    (mercit-menu-highlight-point-section))
  (setq mercit-section-pre-command-region-p (region-active-p))
  (setq mercit-section-pre-command-section (mercit-current-section)))

(defun mercit-section-post-command-hook ()
  (unless (bound-and-true-p transient--prefix)
    (when (or mercit--context-menu-buffer
              mercit--context-menu-section)
      (mercit-menu-highlight-point-section))
    (unless (memq this-command '(mercit-refresh mercit-refresh-all))
      (mercit-section-update-highlight))))

(defun mercit-section-deactivate-mark ()
  (setq mercit-section-highlight-force-update t))

(defun mercit-section-update-highlight (&optional force)
  (let ((section (mercit-current-section)))
    (when (or force
              mercit-section-highlight-force-update
              (xor mercit-section-pre-command-region-p (region-active-p))
              (not (eq mercit-section-pre-command-section section)))
      (let ((inhibit-read-only t)
            (deactivate-mark nil)
            (selection (mercit-region-sections)))
        (mapc #'delete-overlay mercit-section-highlight-overlays)
        (setq mercit-section-highlight-overlays nil)
        (setq mercit-section-unhighlight-sections
              mercit-section-highlighted-sections)
        (setq mercit-section-highlighted-sections nil)
        (unless (eq section mercit-root-section)
          (run-hook-with-args-until-success
           'mercit-section-highlight-hook section selection))
        (dolist (s mercit-section-unhighlight-sections)
          (run-hook-with-args-until-success
           'mercit-section-unhighlight-hook s selection))
        (restore-buffer-modified-p nil)))
    (setq mercit-section-highlight-force-update nil)
    (mercit-section-maybe-paint-visibility-ellipses)))

(defun mercit-section-highlight (section selection)
  "Highlight SECTION and if non-nil all sections in SELECTION.
This function works for any section but produces undesirable
effects for diff related sections, which by default are
highlighted using `mercit-diff-highlight'.  Return t."
  (when-let ((face (oref section heading-highlight-face)))
    (dolist (section (or selection (list section)))
      (mercit-section-make-overlay
       (oref section start)
       (or (oref section content)
           (oref section end))
       face)))
  (cond (selection
         (mercit-section-make-overlay (oref (car selection) start)
                                     (oref (car (last selection)) end)
                                     'mercit-section-highlight)
         (mercit-section-highlight-selection nil selection))
        (t
         (mercit-section-make-overlay (oref section start)
                                     (oref section end)
                                     'mercit-section-highlight)))
  t)

(defun mercit-section-highlight-selection (_ selection)
  "Highlight the section-selection region.
If SELECTION is non-nil, then it is a list of sections selected by
the region.  The headings of these sections are then highlighted.

This is a fallback for people who don't want to highlight the
current section and therefore removed `mercit-section-highlight'
from `mercit-section-highlight-hook'.

This function is necessary to ensure that a representation of
such a region is visible.  If neither of these functions were
part of the hook variable, then such a region would be
invisible."
  (when (and selection
             (not (and (eq this-command 'mouse-drag-region))))
    (dolist (section selection)
      (mercit-section-make-overlay (oref section start)
                                  (or (oref section content)
                                      (oref section end))
                                  'mercit-section-heading-selection))
    t))

(defun mercit-section-make-overlay (start end face)
  ;; Yes, this doesn't belong here.  But the alternative of
  ;; spreading this hack across the code base is even worse.
  (when (and mercit-section-keep-region-overlay
             (memq face '(mercit-section-heading-selection
                          mercit-diff-file-heading-selection
                          mercit-diff-hunk-heading-selection)))
    (setq face (list :foreground (face-foreground face))))
  (let ((ov (make-overlay start end nil t)))
    (overlay-put ov 'font-lock-face face)
    (overlay-put ov 'evaporate t)
    (push ov mercit-section-highlight-overlays)
    ov))

(cl-defgeneric mercit-section-get-relative-position (section))

(cl-defmethod mercit-section-get-relative-position ((section mercit-section))
  (let ((start (oref section start))
        (point (mercit-point)))
    (list (- (line-number-at-pos point)
             (line-number-at-pos start))
          (- point (line-beginning-position)))))

(cl-defgeneric mercit-section-goto-successor ())

(cl-defmethod mercit-section-goto-successor ((section mercit-section)
                                            line char &optional _arg)
  (or (mercit-section-goto-successor--same section line char)
      (mercit-section-goto-successor--related section)))

(defun mercit-section-goto-successor--same (section line char)
  (let ((ident (mercit-section-ident section)))
    (and-let* ((found (mercit-get-section ident)))
      (let ((start (oref found start)))
        (goto-char start)
        (unless (eq found mercit-root-section)
          (ignore-errors
            (forward-line line)
            (forward-char char))
          (unless (eq (mercit-current-section) found)
            (goto-char start)))
        t))))

(defun mercit-section-goto-successor--related (section)
  (and-let* ((found (mercit-section-goto-successor--related-1 section)))
    (goto-char (if (eq (oref found type) 'button)
                   (point-min)
                 (oref found start)))))

(defun mercit-section-goto-successor--related-1 (section)
  (or (and-let* ((alt (pcase (oref section type)
                        ('staged 'unstaged)
                        ('unstaged 'staged)
                        ('unpushed 'unpulled)
                        ('unpulled 'unpushed))))
        (mercit-get-section `((,alt) (status))))
      (and-let* ((next (car (mercit-section-siblings section 'next))))
        (mercit-get-section (mercit-section-ident next)))
      (and-let* ((prev (car (mercit-section-siblings section 'prev))))
        (mercit-get-section (mercit-section-ident prev)))
      (and-let* ((parent (oref section parent)))
        (or (mercit-get-section (mercit-section-ident parent))
            (mercit-section-goto-successor--related-1 parent)))))

;;; Region

(defvar-local mercit-section--region-overlays nil)

(defun mercit-section--delete-region-overlays ()
  (mapc #'delete-overlay mercit-section--region-overlays)
  (setq mercit-section--region-overlays nil))

(defun mercit-section--highlight-region (start end window rol)
  (mercit-section--delete-region-overlays)
  (if (and (not mercit-section-keep-region-overlay)
           (or (mercit-region-sections)
               (run-hook-with-args-until-success 'mercit-region-highlight-hook
                                                 (mercit-current-section)))
           (not (= (line-number-at-pos start)
                   (line-number-at-pos end)))
           ;; (not (eq (car-safe last-command-event) 'mouse-movement))
           )
      (funcall (default-value 'redisplay-unhighlight-region-function) rol)
    (funcall (default-value 'redisplay-highlight-region-function)
             start end window rol)))

(defun mercit-section--unhighlight-region (rol)
  (mercit-section--delete-region-overlays)
  (funcall (default-value 'redisplay-unhighlight-region-function) rol))

;;; Visibility

(defvar-local mercit-section-visibility-cache nil)
(put 'mercit-section-visibility-cache 'permanent-local t)

(defun mercit-section-cached-visibility (section)
  "Set SECTION's visibility to the cached value."
  (cdr (assoc (mercit-section-ident section)
              mercit-section-visibility-cache)))

(cl-defun mercit-section-cache-visibility
    (&optional (section mercit-insert-section--current))
  (setf (compat-call alist-get
                     (mercit-section-ident section)
                     mercit-section-visibility-cache
                     nil nil #'equal)
        (if (oref section hidden) 'hide 'show)))

(cl-defun mercit-section-maybe-cache-visibility
    (&optional (section mercit-insert-section--current))
  (when (or (eq mercit-section-cache-visibility t)
            (memq (oref section type)
                  mercit-section-cache-visibility))
    (mercit-section-cache-visibility section)))

(defun mercit-section-maybe-update-visibility-indicator (section)
  (when (and mercit-section-visibility-indicator
             (mercit-section-content-p section))
    (let* ((beg (oref section start))
           (eoh (save-excursion
                  (goto-char beg)
                  (line-end-position))))
      (cond
       ((symbolp (car-safe mercit-section-visibility-indicator))
        (let ((ov (mercit--overlay-at beg 'mercit-vis-indicator 'fringe)))
          (unless ov
            (setq ov (make-overlay beg eoh nil t))
            (overlay-put ov 'evaporate t)
            (overlay-put ov 'mercit-vis-indicator 'fringe))
          (overlay-put
           ov 'before-string
           (propertize "fringe" 'display
                       (list 'left-fringe
                             (if (oref section hidden)
                                 (car mercit-section-visibility-indicator)
                               (cdr mercit-section-visibility-indicator))
                             'fringe)))))
       ((stringp (car-safe mercit-section-visibility-indicator))
        (let ((ov (mercit--overlay-at (1- eoh) 'mercit-vis-indicator 'eoh)))
          (cond ((oref section hidden)
                 (unless ov
                   (setq ov (make-overlay (1- eoh) eoh))
                   (overlay-put ov 'evaporate t)
                   (overlay-put ov 'mercit-vis-indicator 'eoh))
                 (overlay-put ov 'after-string
                              (car mercit-section-visibility-indicator)))
                (ov
                 (delete-overlay ov)))))))))

(defvar-local mercit--ellipses-sections nil)

(defun mercit-section-maybe-paint-visibility-ellipses ()
  ;; This is needed because we hide the body instead of "the body
  ;; except the final newline and additionally the newline before
  ;; the body"; otherwise we could use `buffer-invisibility-spec'.
  (when (stringp (car-safe mercit-section-visibility-indicator))
    (let* ((sections (append mercit--ellipses-sections
                             (setq mercit--ellipses-sections
                                   (or (mercit-region-sections)
                                       (list (mercit-current-section))))))
           (beg (--map (oref it start) sections))
           (end (--map (oref it end)   sections)))
      (when (region-active-p)
        ;; This ensures that the region face is removed from ellipses
        ;; when the region becomes inactive, but fails to ensure that
        ;; all ellipses within the active region use the region face,
        ;; because the respective overlay has not yet been updated at
        ;; this time.  The mercit-selection face is always applied.
        (push (region-beginning) beg)
        (push (region-end)       end))
      (setq beg (apply #'min beg))
      (setq end (apply #'max end))
      (dolist (ov (overlays-in beg end))
        (when (eq (overlay-get ov 'mercit-vis-indicator) 'eoh)
          (overlay-put
           ov 'after-string
           (propertize
            (car mercit-section-visibility-indicator) 'font-lock-face
            (let ((pos (overlay-start ov)))
              (delq nil (nconc (--map (overlay-get it 'font-lock-face)
                                      (overlays-at pos))
                               (list (get-char-property
                                      pos 'font-lock-face))))))))))))

(defun mercit-section-maybe-remove-visibility-indicator (section)
  (when (and mercit-section-visibility-indicator
             (= (oref section content)
                (oref section end)))
    (dolist (o (overlays-in (oref section start)
                            (save-excursion
                              (goto-char (oref section start))
                              (1+ (line-end-position)))))
      (when (overlay-get o 'mercit-vis-indicator)
        (delete-overlay o)))))

(defvar-local mercit-section--opened-sections nil)

(defun mercit-section--open-temporarily (beg end)
  (save-excursion
    (goto-char beg)
    (let ((section (mercit-current-section)))
      (while section
        (let ((content (oref section content)))
          (if (and (mercit-section-invisible-p section)
                   (<= (or content (oref section start))
                       beg
                       (oref section end)))
              (progn
                (when content
                  (mercit-section-show section)
                  (push section mercit-section--opened-sections))
                (setq section (oref section parent)))
            (setq section nil))))))
  (or (eq search-invisible t)
      (not (isearch-range-invisible beg end))))

(defun isearch-clean-overlays@mercit-mode (fn)
  (if (derived-mode-p 'mercit-mode)
      (let ((pos (point)))
        (dolist (section mercit-section--opened-sections)
          (unless (<= (oref section content) pos (oref section end))
            (mercit-section-hide section)))
        (setq mercit-section--opened-sections nil))
    (funcall fn)))

(advice-add 'isearch-clean-overlays :around
            #'isearch-clean-overlays@mercit-mode)

;;; Utilities

(cl-defun mercit-section-selected-p (section &optional (selection nil sselection))
  (and (not (eq section mercit-root-section))
       (or  (eq section (mercit-current-section))
            (memq section (if sselection
                              selection
                            (setq selection (mercit-region-sections))))
            (and-let* ((parent (oref section parent)))
              (mercit-section-selected-p parent selection)))))

(defun mercit-section-parent-value (section)
  (and-let* ((parent (oref section parent)))
    (oref parent value)))

(defun mercit-section-siblings (section &optional direction)
  "Return a list of the sibling sections of SECTION.

If optional DIRECTION is `prev', then return siblings that come
before SECTION.  If it is `next', then return siblings that come
after SECTION.  For all other values, return all siblings
excluding SECTION itself."
  (and-let* ((parent (oref section parent))
             (siblings (oref parent children)))
    (pcase direction
      ('prev  (cdr (member section (reverse siblings))))
      ('next  (cdr (member section siblings)))
      (_      (remq section siblings)))))

(defun mercit-region-values (&optional condition multiple)
  "Return a list of the values of the selected sections.

Return the values that themselves would be returned by
`mercit-region-sections' (which see)."
  (--map (oref it value)
         (mercit-region-sections condition multiple)))

(defun mercit-region-sections (&optional condition multiple)
  "Return a list of the selected sections.

When the region is active and constitutes a valid section
selection, then return a list of all selected sections.  This is
the case when the region begins in the heading of a section and
ends in the heading of the same section or in that of a sibling
section.  If optional MULTIPLE is non-nil, then the region cannot
begin and end in the same section.

When the selection is not valid, then return nil.  In this case,
most commands that can act on the selected sections will instead
act on the section at point.

When the region looks like it would in any other buffer then
the selection is invalid.  When the selection is valid then the
region uses the `mercit-section-highlight' face.  This does not
apply to diffs where things get a bit more complicated, but even
here if the region looks like it usually does, then that's not
a valid selection as far as this function is concerned.

If optional CONDITION is non-nil, then the selection not only
has to be valid; all selected sections additionally have to match
CONDITION, or nil is returned.  See `mercit-section-match' for the
forms CONDITION can take."
  (when (region-active-p)
    (let* ((rbeg (region-beginning))
           (rend (region-end))
           (sbeg (mercit-section-at rbeg))
           (send (mercit-section-at rend)))
      (when (and send
                 (not (eq send mercit-root-section))
                 (not (and multiple (eq send sbeg))))
        (let ((siblings (cons sbeg (mercit-section-siblings sbeg 'next)))
              sections)
          (when (and (memq send siblings)
                     (mercit-section-position-in-heading-p sbeg rbeg)
                     (mercit-section-position-in-heading-p send rend))
            (while siblings
              (push (car siblings) sections)
              (when (eq (pop siblings) send)
                (setq siblings nil)))
            (setq sections (nreverse sections))
            (when (or (not condition)
                      (--all-p (mercit-section-match condition it) sections))
              sections)))))))

(defun mercit-section-position-in-heading-p (&optional section pos)
  "Return t if POSITION is inside the heading of SECTION.
POSITION defaults to point and SECTION defaults to the
current section."
  (unless section
    (setq section (mercit-current-section)))
  (unless pos
    (setq pos (point)))
  (and section
       (>= pos (oref section start))
       (<  pos (or (oref section content)
                   (oref section end)))
       t))

(defun mercit-section-internal-region-p (&optional section)
  "Return t if the region is active and inside SECTION's body.
If optional SECTION is nil, use the current section."
  (and (region-active-p)
       (or section (setq section (mercit-current-section)))
       (let ((beg (mercit-section-at (region-beginning))))
         (and (eq beg (mercit-section-at (region-end)))
              (eq beg section)))
       (not (or (mercit-section-position-in-heading-p section (region-beginning))
                (mercit-section-position-in-heading-p section (region-end))))
       t))

(defun mercit-wash-sequence (function)
  "Repeatedly call FUNCTION until it returns nil or eob is reached.
FUNCTION has to move point forward or return nil."
  (while (and (not (eobp)) (funcall function))))

(defun mercit-add-section-hook (hook function &optional at append local)
  "Add to the value of section hook HOOK the function FUNCTION.

Add FUNCTION at the beginning of the hook list unless optional
APPEND is non-nil, in which case FUNCTION is added at the end.
If FUNCTION already is a member, then move it to the new location.

If optional AT is non-nil and a member of the hook list, then
add FUNCTION next to that instead.  Add before or after AT, or
replace AT with FUNCTION depending on APPEND.  If APPEND is the
symbol `replace', then replace AT with FUNCTION.  For any other
non-nil value place FUNCTION right after AT.  If nil, then place
FUNCTION right before AT.  If FUNCTION already is a member of the
list but AT is not, then leave FUNCTION where ever it already is.

If optional LOCAL is non-nil, then modify the hook's buffer-local
value rather than its global value.  This makes the hook local by
copying the default value.  That copy is then modified.

HOOK should be a symbol.  If HOOK is void, it is first set to nil.
HOOK's value must not be a single hook function.  FUNCTION should
be a function that takes no arguments and inserts one or multiple
sections at point, moving point forward.  FUNCTION may choose not
to insert its section(s), when doing so would not make sense.  It
should not be abused for other side-effects.  To remove FUNCTION
again use `remove-hook'."
  (unless (boundp hook)
    (error "Cannot add function to undefined hook variable %s" hook))
  (unless (default-boundp hook)
    (set-default hook nil))
  (let ((value (if local
                   (if (local-variable-p hook)
                       (symbol-value hook)
                     (unless (local-variable-if-set-p hook)
                       (make-local-variable hook))
                     (copy-sequence (default-value hook)))
                 (default-value hook))))
    (if at
        (when (setq at (member at value))
          (setq value (delq function value))
          (cond ((eq append 'replace)
                 (setcar at function))
                (append
                 (push function (cdr at)))
                (t
                 (push (car at) (cdr at))
                 (setcar at function))))
      (setq value (delq function value)))
    (unless (member function value)
      (setq value (if append
                      (append value (list function))
                    (cons function value))))
    (when (eq append 'replace)
      (setq value (delq at value)))
    (if local
        (set hook value)
      (set-default hook value))))

(defvar-local mercit-disabled-section-inserters nil)

(defun mercit-disable-section-inserter (fn)
  "Disable the section inserter FN in the current repository.
It is only intended for use in \".dir-locals.el\" and
\".dir-locals-2.el\".  Also see info node `(mercit)Per-Repository
Configuration'."
  (cl-pushnew fn mercit-disabled-section-inserters))

(put 'mercit-disable-section-inserter 'safe-local-eval-function t)

(defun mercit-run-section-hook (hook &rest args)
  "Run HOOK with ARGS, warning about invalid entries."
  (let ((entries (symbol-value hook)))
    (unless (listp entries)
      (setq entries (list entries)))
    (--when-let (-remove #'functionp entries)
      (message "`%s' contains entries that are no longer valid.
%s\nUsing standard value instead.  Please re-configure hook variable."
               hook
               (mapconcat (lambda (sym) (format "  `%s'" sym)) it "\n"))
      (sit-for 5)
      (setq entries (eval (car (get hook 'standard-value)))))
    (dolist (entry entries)
      (let ((mercit--current-section-hook (cons (list hook entry)
                                               mercit--current-section-hook)))
        (unless (memq entry mercit-disabled-section-inserters)
          (if (bound-and-true-p mercit-refresh-verbose)
              (let ((time (benchmark-elapse (apply entry args))))
                (message "  %-50s %s %s" entry time
                         (cond ((> time 0.03) "!!")
                               ((> time 0.01) "!")
                               (t ""))))
            (apply entry args)))))))

(cl-defun mercit--overlay-at (pos prop &optional (val nil sval) testfn)
  (cl-find-if (lambda (o)
                (let ((p (overlay-properties o)))
                  (and (plist-member p prop)
                       (or (not sval)
                           (funcall (or testfn #'eql)
                                    (plist-get p prop)
                                    val)))))
              (overlays-at pos t)))

(defun mercit-face-property-all (face string)
  "Return non-nil if FACE is present in all of STRING."
  (catch 'missing
    (let ((pos 0))
      (while (setq pos (next-single-property-change pos 'font-lock-face string))
        (let ((val (get-text-property pos 'font-lock-face string)))
          (unless (if (consp val)
                      (memq face val)
                    (eq face val))
            (throw 'missing nil))))
      (not pos))))

(defun mercit--add-face-text-property (beg end face &optional append object)
  "Like `add-face-text-property' but for `font-lock-face'."
  (while (< beg end)
    (let* ((pos (next-single-property-change beg 'font-lock-face object end))
           (val (get-text-property beg 'font-lock-face object))
           (val (if (listp val) val (list val))))
      (put-text-property beg pos 'font-lock-face
                         (if append
                             (append val (list face))
                           (cons face val))
                         object)
      (setq beg pos))))

(defun mercit--propertize-face (string face)
  (propertize string 'face face 'font-lock-face face))

(defun mercit--put-face (beg end face string)
  (put-text-property beg end 'face face string)
  (put-text-property beg end 'font-lock-face face string))

;;; Bitmaps

(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'mercit-fringe-bitmap+
    [#b00000000
     #b00011000
     #b00011000
     #b01111110
     #b01111110
     #b00011000
     #b00011000
     #b00000000])
  (define-fringe-bitmap 'mercit-fringe-bitmap-
    [#b00000000
     #b00000000
     #b00000000
     #b01111110
     #b01111110
     #b00000000
     #b00000000
     #b00000000])

  (define-fringe-bitmap 'mercit-fringe-bitmap>
    [#b01100000
     #b00110000
     #b00011000
     #b00001100
     #b00011000
     #b00110000
     #b01100000
     #b00000000])
  (define-fringe-bitmap 'mercit-fringe-bitmapv
    [#b00000000
     #b10000010
     #b11000110
     #b01101100
     #b00111000
     #b00010000
     #b00000000
     #b00000000])

  (define-fringe-bitmap 'mercit-fringe-bitmap-bold>
    [#b11100000
     #b01110000
     #b00111000
     #b00011100
     #b00011100
     #b00111000
     #b01110000
     #b11100000])
  (define-fringe-bitmap 'mercit-fringe-bitmap-boldv
    [#b10000001
     #b11000011
     #b11100111
     #b01111110
     #b00111100
     #b00011000
     #b00000000
     #b00000000])
  )

;;; _
(provide 'mercit-section)
;;; mercit-section.el ends here
