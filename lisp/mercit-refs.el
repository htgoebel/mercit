;;; mercit-refs.el --- Listing references  -*- lexical-binding:t -*-

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

;; This library implements support for listing references in a buffer.

;;; Code:

(require 'mercit)

;;; Options

(defgroup mercit-refs nil
  "Inspect and manipulate Mercurial branches and tags."
  :link '(info-link "(mercit)References Buffer")
  :group 'mercit-modes)

(defcustom mercit-refs-mode-hook nil
  "Hook run after entering Mercit-Refs mode."
  :package-version '(mercit . "2.1.0")
  :group 'mercit-refs
  :type 'hook)

(defcustom mercit-refs-sections-hook
  '(mercit-insert-error-header
    mercit-insert-branch-description
    mercit-insert-local-branches
    mercit-insert-remote-branches
    mercit-insert-tags)
  "Hook run to insert sections into a references buffer."
  :package-version '(mercit . "2.1.0")
  :group 'mercit-refs
  :type 'hook)

(defcustom mercit-refs-show-commit-count nil
  "Whether to show commit counts in Mercit-Refs mode buffers.

all    Show counts for branches and tags.
branch Show counts for branches only.
nil    Never show counts.

To change the value in an existing buffer use the command
`mercit-refs-set-show-commit-count'."
  :package-version '(mercit . "2.1.0")
  :group 'mercit-refs
  :safe (lambda (val) (memq val '(all branch nil)))
  :type '(choice (const all    :tag "For branches and tags")
                 (const branch :tag "For branches only")
                 (const nil    :tag "Never")))
(put 'mercit-refs-show-commit-count 'safe-local-variable 'symbolp)
(put 'mercit-refs-show-commit-count 'permanent-local t)

(defcustom mercit-refs-pad-commit-counts nil
  "Whether to pad all counts on all sides in `mercit-refs-mode' buffers.

If this is nil, then some commit counts are displayed right next
to one of the branches that appear next to the count, without any
space in between.  This might look bad if the branch name faces
look too similar to `mercit-dimmed'.

If this is non-nil, then spaces are placed on both sides of all
commit counts."
  :package-version '(mercit . "2.12.0")
  :group 'mercit-refs
  :type 'boolean)

(defvar mercit-refs-show-push-remote nil
  "Whether to show the push-remotes of local branches.
Also show the commits that the local branch is ahead and behind
the push-target.  Unfortunately there is a bug in Mercurial that makes
this useless (the commits ahead and behind the upstream are
shown), so this isn't enabled yet.")

(defcustom mercit-refs-show-remote-prefix nil
  "Whether to show the remote prefix in lists of remote branches.

This is redundant because the name of the remote is already shown
in the heading preceding the list of its branches."
  :package-version '(mercit . "2.12.0")
  :group 'mercit-refs
  :type 'boolean)

(defcustom mercit-refs-margin
  (list nil
        (nth 1 mercit-log-margin)
        'mercit-log-margin-width nil
        (nth 4 mercit-log-margin))
  "Format of the margin in `mercit-refs-mode' buffers.

The value has the form (INIT STYLE WIDTH AUTHOR AUTHOR-WIDTH).

If INIT is non-nil, then the margin is shown initially.
STYLE controls how to format the author or committer date.
  It can be one of `age' (to show the age of the commit),
  `age-abbreviated' (to abbreviate the time unit to a character),
  or a string (suitable for `format-time-string') to show the
  actual date.  Option `mercit-log-margin-show-committer-date'
  controls which date is being displayed.
WIDTH controls the width of the margin.  This exists for forward
  compatibility and currently the value should not be changed.
AUTHOR controls whether the name of the author is also shown by
  default.
AUTHOR-WIDTH has to be an integer.  When the name of the author
  is shown, then this specifies how much space is used to do so."
  :package-version '(mercit . "2.9.0")
  :group 'mercit-refs
  :group 'mercit-margin
  :safe (lambda (val) (memq val '(all branch nil)))
  :type mercit-log-margin--custom-type
  :initialize #'mercit-custom-initialize-reset
  :set-after '(mercit-log-margin)
  :set (apply-partially #'mercit-margin-set-variable 'mercit-refs-mode))

(defcustom mercit-refs-margin-for-tags nil
  "Whether to show information about tags in the margin.

This is disabled by default because it is slow if there are many
tags."
  :package-version '(mercit . "2.9.0")
  :group 'mercit-refs
  :group 'mercit-margin
  :type 'boolean)

(defcustom mercit-refs-primary-column-width (cons 16 32)
  "Width of the focus column in `mercit-refs-mode' buffers.

The primary column is the column that contains the name of the
branch that the current row is about.

If this is an integer, then the column is that many columns wide.
Otherwise it has to be a cons-cell of two integers.  The first
specifies the minimal width, the second the maximal width.  In that
case the actual width is determined using the length of the names
of the shown local branches.  (Remote branches and tags are not
taken into account when calculating to optimal width.)"
  :package-version '(mercit . "2.12.0")
  :group 'mercit-refs
  :type '(choice (integer :tag "Constant wide")
                 (cons    :tag "Wide constrains"
                          (integer :tag "Minimum")
                          (integer :tag "Maximum"))))

(defcustom mercit-refs-focus-column-width 5
  "Width of the focus column in `mercit-refs-mode' buffers.

The focus column is the first column, which marks one
branch (usually the current branch) as the focused branch using
\"*\" or \"@\".  For each other reference, this column optionally
shows how many commits it is ahead of the focused branch and \"<\", or
if it isn't ahead then the commits it is behind and \">\", or if it
isn't behind either, then a \"=\".

This column may also display only \"*\" or \"@\" for the focused
branch, in which case this option is ignored.  Use \"L v\" to
change the verbosity of this column."
  :package-version '(mercit . "2.12.0")
  :group 'mercit-refs
  :type 'integer)

(defcustom mercit-refs-filter-alist nil
  "Alist controlling which refs are omitted from `mercit-refs-mode' buffers.

The purpose of this option is to forgo displaying certain refs
based on their name.  If you want to not display any refs of a
certain type, then you should remove the appropriate function
from `mercit-refs-sections-hook' instead.

All keys are tried in order until one matches.  Then its value
is used and subsequent elements are ignored.  If the value is
non-nil, then the reference is displayed, otherwise it is not.
If no element matches, then the reference is displayed.

A key can either be a regular expression that the refname has to
match, or a function that takes the refname as only argument and
returns a boolean.  A remote branch such as \"origin/master\" is
displayed as just \"master\", however for this comparison the
former is used."
  :package-version '(mercit . "2.12.0")
  :group 'mercit-refs
  :type '(alist :key-type   (choice  :tag "Key" regexp function)
                :value-type (boolean :tag "Value"
                                     :on  "show (non-nil)"
                                     :off "omit (nil)")))

(defcustom mercit-visit-ref-behavior nil
  "Control how `mercit-visit-ref' behaves in `mercit-refs-mode' buffers.

By default `mercit-visit-ref' behaves like `mercit-show-commit',
in all buffers, including `mercit-refs-mode' buffers.  When the
type of the section at point is `commit' then \"RET\" is bound to
`mercit-show-commit', and when the type is either `branch' or
`tag' then it is bound to `mercit-visit-ref'.

\"RET\" is one of Mercit's most essential keys and at least by
default it should behave consistently across all of Mercit,
especially because users quickly learn that it does something
very harmless; it shows more information about the thing at point
in another buffer.

However \"RET\" used to behave differently in `mercit-refs-mode'
buffers, doing surprising things, some of which cannot really be
described as \"visit this thing\".  If you have grown accustomed
to such inconsistent, but to you useful, behavior, then you can
restore that by adding one or more of the below symbols to the
value of this option.  But keep in mind that by doing so you
don't only introduce inconsistencies, you also lose some
functionality and might have to resort to `M-x mercit-show-commit'
to get it back.

`mercit-visit-ref' looks for these symbols in the order in which
they are described here.  If the presence of a symbol applies to
the current situation, then the symbols that follow do not affect
the outcome.

`focus-on-ref'

  With a prefix argument update the buffer to show commit counts
  and lists of cherry commits relative to the reference at point
  instead of relative to the current buffer or `HEAD'.

  Instead of adding this symbol, consider pressing \"C-u y o RET\".

`create-branch'

  If point is on a remote branch, then create a new local branch
  with the same name, use the remote branch as its upstream, and
  then check out the local branch.

  Instead of adding this symbol, consider pressing \"b c RET RET\",
  like you would do in other buffers.

`checkout-any'

  Check out the reference at point.  If that reference is a tag
  or a remote branch, then this results in a detached `HEAD'.

  Instead of adding this symbol, consider pressing \"b b RET\",
  like you would do in other buffers.

`checkout-branch'

  Check out the local branch at point.

  Instead of adding this symbol, consider pressing \"b b RET\",
  like you would do in other buffers."
  :package-version '(mercit . "2.9.0")
  :group 'mercit-refs
  :group 'mercit-commands
  :options '(focus-on-ref create-branch checkout-any checkout-branch)
  :type '(list :convert-widget custom-hook-convert-widget))

;;; Mode

(defvar mercit-refs-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mercit-mode-map)
    (define-key map (kbd "C-y") #'mercit-refs-set-show-commit-count)
    (define-key map (kbd "L")   #'mercit-margin-settings)
    map)
  "Keymap for `mercit-refs-mode'.")

(define-derived-mode mercit-refs-mode mercit-mode "Mercit Refs"
  "Mode which lists and compares references.

This mode is documented in info node `(mercit)References Buffer'.

\\<mercit-mode-map>\
Type \\[mercit-refresh] to refresh the current buffer.
Type \\[mercit-section-toggle] to expand or hide the section at point.
Type \\[mercit-visit-thing] or \\[mercit-diff-show-or-scroll-up] \
to visit the commit or branch at point.

Type \\[mercit-branch] to see available branch commands.
Type \\[mercit-merge] to merge the branch or commit at point.
Type \\[mercit-cherry-pick] to apply the commit at point.
Type \\[mercit-reset] to reset `HEAD' to the commit at point.

\\{mercit-refs-mode-map}"
  :group 'mercit-refs
  (hack-dir-local-variables-non-file-buffer)
  (setq mercit--imenu-group-types '(local remote tags)))

(defun mercit-refs-setup-buffer (ref args)
  (mercit-setup-buffer #'mercit-refs-mode nil
    (mercit-buffer-upstream ref)
    (mercit-buffer-arguments args)))

(defun mercit-refs-refresh-buffer ()
  (setq mercit-set-buffer-margin-refresh (not (mercit-buffer-margin-p)))
  (unless (mercit-rev-verify mercit-buffer-upstream)
    (setq mercit-refs-show-commit-count nil))
  (mercit-set-header-line-format
   (format "%s %s" mercit-buffer-upstream
           (mapconcat #'identity mercit-buffer-arguments " ")))
  (mercit-insert-section (branchbuf)
    (mercit-run-section-hook 'mercit-refs-sections-hook))
  (add-hook 'kill-buffer-hook #'mercit-preserve-section-visibility-cache))

(cl-defmethod mercit-buffer-value (&context (major-mode mercit-refs-mode))
  (cons mercit-buffer-upstream mercit-buffer-arguments))

;;; Commands

;;;###autoload (autoload 'mercit-show-refs "mercit-refs" nil t)
(transient-define-prefix mercit-show-refs (&optional transient)
  "List and compare references in a dedicated buffer."
  :man-page "git-branch"
  :value (lambda ()
           (mercit-show-refs-arguments mercit-prefix-use-buffer-arguments))
  ["Arguments"
   (mercit-for-each-ref:--contains)
   ("-M" "Merged"               "--merged=" mercit-transient-read-revision)
   ("-m" "Merged to HEAD"       "--merged")
   ("-N" "Not merged"           "--no-merged=" mercit-transient-read-revision)
   ("-n" "Not merged to HEAD"   "--no-merged")
   (mercit-for-each-ref:--sort)]
  ["Actions"
   ("y" "Show refs, comparing them with HEAD"           mercit-show-refs-head)
   ("c" "Show refs, comparing them with current branch" mercit-show-refs-current)
   ("o" "Show refs, comparing them with other branch"   mercit-show-refs-other)
   ("r" "Show refs, changing commit count display"
    mercit-refs-set-show-commit-count)]
  (interactive (list (or (derived-mode-p 'mercit-refs-mode)
                         current-prefix-arg)))
  (if transient
      (transient-setup 'mercit-show-refs)
    (mercit-refs-setup-buffer "HEAD" (mercit-show-refs-arguments))))

(defun mercit-show-refs-arguments (&optional use-buffer-args)
  (unless use-buffer-args
    (setq use-buffer-args mercit-direct-use-buffer-arguments))
  (let (args)
    (cond
     ((eq transient-current-command 'mercit-show-refs)
      (setq args (transient-args 'mercit-show-refs)))
     ((eq major-mode 'mercit-refs-mode)
      (setq args mercit-buffer-arguments))
     ((and (memq use-buffer-args '(always selected))
           (when-let* ((buffer (mercit-get-mode-buffer ;debbugs#31840
                               'mercit-refs-mode nil
                               (eq use-buffer-args 'selected))))
             (setq args (buffer-local-value 'mercit-buffer-arguments buffer))
             t)))
     (t
      (setq args (alist-get 'mercit-show-refs transient-values))))
    args))

(transient-define-argument mercit-for-each-ref:--contains ()
  :description "Contains"
  :class 'transient-option
  :key "-c"
  :argument "--contains="
  :reader #'mercit-transient-read-revision)

(transient-define-argument mercit-for-each-ref:--sort ()
  :description "Sort"
  :class 'transient-option
  :key "-s"
  :argument "--sort="
  :reader #'mercit-read-ref-sort)

(defun mercit-read-ref-sort (prompt initial-input _history)
  (mercit-completing-read prompt
                         '("-committerdate" "-authordate"
                           "committerdate" "authordate")
                         nil nil initial-input))

;;;###autoload
(defun mercit-show-refs-head (&optional args)
  "List and compare references in a dedicated buffer.
Compared with `HEAD'."
  (interactive (list (mercit-show-refs-arguments)))
  (mercit-refs-setup-buffer "HEAD" args))

;;;###autoload
(defun mercit-show-refs-current (&optional args)
  "List and compare references in a dedicated buffer.
Compare with the current branch or `HEAD' if it is detached."
  (interactive (list (mercit-show-refs-arguments)))
  (mercit-refs-setup-buffer (mercit-get-current-branch) args))

;;;###autoload
(defun mercit-show-refs-other (&optional ref args)
  "List and compare references in a dedicated buffer.
Compared with a branch read from the user."
  (interactive (list (mercit-read-other-branch "Compare with")
                     (mercit-show-refs-arguments)))
  (mercit-refs-setup-buffer ref args))

(defun mercit-refs-set-show-commit-count ()
  "Change for which refs the commit count is shown."
  (interactive)
  (setq-local mercit-refs-show-commit-count
              (mercit-read-char-case "Show commit counts for " nil
                (?a "[a]ll refs" 'all)
                (?b "[b]ranches only" t)
                (?n "[n]othing" nil)))
  (mercit-refresh))

(defun mercit-visit-ref ()
  "Visit the reference or revision at point in another buffer.
If there is no revision at point or with a prefix argument prompt
for a revision.

This command behaves just like `mercit-show-commit', except if
point is on a reference in a `mercit-refs-mode' buffer (a buffer
listing branches and tags), in which case the behavior may be
different, but only if you have customized the option
`mercit-visit-ref-behavior' (which see).  When invoked from a
menu this command always behaves like `mercit-show-commit'."
  (interactive)
  (if (and (derived-mode-p 'mercit-refs-mode)
           (mercit-section-match '(branch tag))
           (not (mercit-menu-position)))
      (let ((ref (oref (mercit-current-section) value)))
        (cond (current-prefix-arg
               (cond ((memq 'focus-on-ref mercit-visit-ref-behavior)
                      (mercit-refs-setup-buffer ref (mercit-show-refs-arguments)))
                     (mercit-visit-ref-behavior
                      ;; Don't prompt for commit to visit.
                      (let ((current-prefix-arg nil))
                        (call-interactively #'mercit-show-commit)))))
              ((and (memq 'create-branch mercit-visit-ref-behavior)
                    (mercit-section-match [branch remote]))
               (let ((branch (cdr (mercit-split-branch-name ref))))
                 (if (mercit-branch-p branch)
                     (if (mercit-rev-eq branch ref)
                         (mercit-call-git "checkout" branch)
                       (setq branch (propertize branch 'face 'mercit-branch-local))
                       (setq ref (propertize ref 'face 'mercit-branch-remote))
                       (pcase (prog1 (read-char-choice (format (propertize "\
Branch %s already exists.
  [c]heckout %s as-is
  [r]reset %s to %s and checkout %s
  [a]bort " 'face 'minibuffer-prompt) branch branch branch ref branch)
                                                       '(?c ?r ?a))
                                (message "")) ; otherwise prompt sticks
                         (?c (mercit-call-git "checkout" branch))
                         (?r (mercit-call-git "checkout" "-B" branch ref))
                         (?a (user-error "Abort"))))
                   (mercit-call-git "checkout" "-b" branch ref))
                 (setq mercit-buffer-upstream branch)
                 (mercit-refresh)))
              ((or (memq 'checkout-any mercit-visit-ref-behavior)
                   (and (memq 'checkout-branch mercit-visit-ref-behavior)
                        (mercit-section-match [branch local])))
               (mercit-call-git "checkout" ref)
               (setq mercit-buffer-upstream ref)
               (mercit-refresh))
              (t
               (call-interactively #'mercit-show-commit))))
    (call-interactively #'mercit-show-commit)))

;;; Sections

(defvar mercit-remote-section-map
  (let ((map (make-sparse-keymap)))
    (mercit-menu-set map [mercit-delete-thing] #'mercit-remote-remove "Remove %m")
    (mercit-menu-set map [mercit-file-rename]  #'mercit-remote-rename "Rename %s")
    map)
  "Keymap for `remote' sections.")

(defvar mercit-branch-section-map
  (let ((map (make-sparse-keymap)))
    (mercit-menu-set map [mercit-visit-thing]  #'mercit-visit-ref     "Visit commit")
    (mercit-menu-set map [mercit-delete-thing] #'mercit-branch-delete "Delete %m")
    (mercit-menu-set map [mercit-file-rename]  #'mercit-branch-rename "Rename %s")
    map)
  "Keymap for `branch' sections.")

(defvar mercit-tag-section-map
  (let ((map (make-sparse-keymap)))
    (mercit-menu-set map [mercit-visit-thing]  #'mercit-visit-ref  "Visit %s")
    (mercit-menu-set map [mercit-delete-thing] #'mercit-tag-delete "Delete %m")
    map)
  "Keymap for `tag' sections.")

(defun mercit--painted-branch-as-menu-section (section)
  (and-let* ((branch (and (mercit-section-match 'commit)
                          (mercit--painted-branch-at-point))))
    (let ((dummy (mercit-section :type 'branch :value branch)))
      (oset dummy keymap mercit-branch-section-map)
      (dolist (slot '(start content hidden parent children))
        (when (slot-boundp section slot)
          (setf (eieio-oref dummy slot)
                (eieio-oref section slot))))
      dummy)))

(add-hook 'mercit-menu-alternative-section-hook
          #'mercit--painted-branch-as-menu-section)

(defun mercit-insert-branch-description ()
  "Insert header containing the description of the current branch.
Insert a header line with the name and description of the
current branch.  The description is taken from the Git variable
`branch.<NAME>.description'; if that is undefined then no header
line is inserted at all."
  (when-let* ((branch (mercit-get-current-branch))
              (desc (mercit-get "branch" branch "description"))
              (desc (split-string desc "\n")))
    (when (equal (car (last desc)) "")
      (setq desc (butlast desc)))
    (mercit-insert-section (branchdesc branch t)
      (mercit-insert-heading branch ": " (car desc))
      (when (cdr desc)
        (insert (mapconcat #'identity (cdr desc) "\n"))
        (insert "\n\n")))))

(defun mercit-insert-tags ()
  "Insert sections showing all tags."
  (when-let ((tags (mercit-git-lines "tag" "--list" "-n" mercit-buffer-arguments)))
    (let ((_head (mercit-rev-parse "HEAD")))
      (mercit-insert-section (tags)
        (mercit-insert-heading "Tags:")
        (dolist (tag tags)
          (string-match "^\\([^ \t]+\\)[ \t]+\\([^ \t\n].*\\)?" tag)
          (let ((tag (match-string 1 tag))
                (msg (match-string 2 tag)))
            (when (mercit-refs--insert-refname-p tag)
              (mercit-insert-section (tag tag t)
                (mercit-insert-heading
                  (mercit-refs--format-focus-column tag 'tag)
                  (propertize tag 'font-lock-face 'mercit-tag)
                  (make-string
                   (max 1 (- (if (consp mercit-refs-primary-column-width)
                                 (car mercit-refs-primary-column-width)
                               mercit-refs-primary-column-width)
                             (length tag)))
                   ?\s)
                  (and msg (mercit-log-propertize-keywords nil msg)))
                (when (and mercit-refs-margin-for-tags (mercit-buffer-margin-p))
                  (mercit-refs--format-margin tag))
                (mercit-refs--insert-cherry-commits tag)))))
        (insert ?\n)
        (mercit-make-margin-overlay nil t)))))

(defun mercit-insert-remote-branches ()
  "Insert sections showing all remote-tracking branches."
  (dolist (remote (mercit-list-remotes))
    (mercit-insert-section (remote remote)
      (mercit-insert-heading
        (let ((pull (mercit-get "remote" remote "url"))
              (push (mercit-get "remote" remote "pushurl")))
          (format (propertize "Remote %s (%s):"
                              'font-lock-face 'mercit-section-heading)
                  (propertize remote 'font-lock-face 'mercit-branch-remote)
                  (concat pull (and pull push ", ") push))))
      (let (head)
        (dolist (line (mercit-git-lines "for-each-ref" "--format=\
%(symref:short)%00%(refname:short)%00%(refname)%00%(subject)"
                                       (concat "refs/remotes/" remote)
                                       mercit-buffer-arguments))
          (pcase-let ((`(,head-branch ,branch ,ref ,msg)
                       (-replace "" nil (split-string line "\0"))))
            (if head-branch
                (progn (cl-assert (equal branch (concat remote "/HEAD")))
                       (setq head head-branch))
              (when (mercit-refs--insert-refname-p branch)
                (mercit-insert-section (branch branch t)
                  (let ((headp (equal branch head))
                        (abbrev (if mercit-refs-show-remote-prefix
                                    branch
                                  (substring branch (1+ (length remote))))))
                    (mercit-insert-heading
                      (mercit-refs--format-focus-column branch)
                      (mercit-refs--propertize-branch
                       abbrev ref (and headp 'mercit-branch-remote-head))
                      (make-string
                       (max 1 (- (if (consp mercit-refs-primary-column-width)
                                     (car mercit-refs-primary-column-width)
                                   mercit-refs-primary-column-width)
                                 (length abbrev)))
                       ?\s)
                      (and msg (mercit-log-propertize-keywords nil msg))))
                  (when (mercit-buffer-margin-p)
                    (mercit-refs--format-margin branch))
                  (mercit-refs--insert-cherry-commits branch)))))))
      (insert ?\n)
      (mercit-make-margin-overlay nil t))))

(defun mercit-insert-local-branches ()
  "Insert sections showing all local branches."
  (mercit-insert-section (local nil)
    (mercit-insert-heading "Branches:")
    (dolist (line (mercit-refs--format-local-branches))
      (pcase-let ((`(,branch . ,strings) line))
        (mercit-insert-section
          ((eval (if branch 'branch 'commit))
           (or branch (mercit-rev-parse "HEAD"))
           t)
          (apply #'mercit-insert-heading strings)
          (when (mercit-buffer-margin-p)
            (mercit-refs--format-margin branch))
          (mercit-refs--insert-cherry-commits branch))))
    (insert ?\n)
    (mercit-make-margin-overlay nil t)))

(defun mercit-refs--format-local-branches ()
  (let ((lines (-keep #'mercit-refs--format-local-branch
                      (mercit-git-lines
                       "for-each-ref"
                       (concat "--format=\
%(HEAD)%00%(refname:short)%00%(refname)%00\
%(upstream:short)%00%(upstream)%00%(upstream:track)%00"
                               (if mercit-refs-show-push-remote "\
%(push:remotename)%00%(push)%00%(push:track)%00%(subject)"
                                 "%00%00%00%(subject)"))
                       "refs/heads"
                       mercit-buffer-arguments))))
    (unless (mercit-get-current-branch)
      (push (mercit-refs--format-local-branch
             (concat "*\0\0\0\0\0\0\0\0" (mercit-rev-format "%s")))
            lines))
    (setq-local mercit-refs-primary-column-width
                (let ((def (default-value 'mercit-refs-primary-column-width)))
                  (if (atom def)
                      def
                    (pcase-let ((`(,min . ,max) def))
                      (min max (apply #'max min (mapcar #'car lines)))))))
    (mapcar (pcase-lambda (`(,_ ,branch ,focus ,branch-desc ,u:ahead ,p:ahead
                                ,u:behind ,upstream ,p:behind ,push ,msg))
              (list branch focus branch-desc u:ahead p:ahead
                    (make-string (max 1 (- mercit-refs-primary-column-width
                                           (length (concat branch-desc
                                                           u:ahead
                                                           p:ahead
                                                           u:behind))))
                                 ?\s)
                    u:behind upstream p:behind push
                    msg))
            lines)))

(defun mercit-refs--format-local-branch (line)
  (pcase-let ((`(,head ,branch ,ref ,upstream ,u:ref ,u:track
                       ,push ,p:ref ,p:track ,msg)
               (-replace "" nil (split-string line "\0"))))
    (when (or (not branch)
              (mercit-refs--insert-refname-p branch))
      (let* ((headp (equal head "*"))
             (pushp (and push
                         mercit-refs-show-push-remote
                         (mercit-rev-verify p:ref)
                         (not (equal p:ref u:ref))))
             (branch-desc
              (if branch
                  (mercit-refs--propertize-branch
                   branch ref (and headp 'mercit-branch-current))
                (mercit--propertize-face "(detached)" 'mercit-branch-warning)))
             (u:ahead  (and u:track
                            (string-match "ahead \\([0-9]+\\)" u:track)
                            (mercit--propertize-face
                             (concat (and mercit-refs-pad-commit-counts " ")
                                     (match-string 1 u:track)
                                     ">")
                             'mercit-dimmed)))
             (u:behind (and u:track
                            (string-match "behind \\([0-9]+\\)" u:track)
                            (mercit--propertize-face
                             (concat "<"
                                     (match-string 1 u:track)
                                     (and mercit-refs-pad-commit-counts " "))
                             'mercit-dimmed)))
             (p:ahead  (and pushp p:track
                            (string-match "ahead \\([0-9]+\\)" p:track)
                            (mercit--propertize-face
                             (concat (match-string 1 p:track)
                                     ">"
                                     (and mercit-refs-pad-commit-counts " "))
                             'mercit-branch-remote)))
             (p:behind (and pushp p:track
                            (string-match "behind \\([0-9]+\\)" p:track)
                            (mercit--propertize-face
                             (concat "<"
                                     (match-string 1 p:track)
                                     (and mercit-refs-pad-commit-counts " "))
                             'mercit-dimmed))))
        (list (1+ (length (concat branch-desc u:ahead p:ahead u:behind)))
              branch
              (mercit-refs--format-focus-column branch headp)
              branch-desc u:ahead p:ahead u:behind
              (and upstream
                   (concat (if (equal u:track "[gone]")
                               (mercit--propertize-face upstream 'error)
                             (mercit-refs--propertize-branch upstream u:ref))
                           " "))
              (and pushp
                   (concat p:behind
                           (mercit--propertize-face
                            push 'mercit-branch-remote)
                           " "))
              (and msg (mercit-log-propertize-keywords nil msg)))))))

(defun mercit-refs--format-focus-column (ref &optional type)
  (let ((focus mercit-buffer-upstream)
        (width (if mercit-refs-show-commit-count
                   mercit-refs-focus-column-width
                 1)))
    (format
     (format "%%%ss " width)
     (cond ((or (equal ref focus)
                (and (eq type t)
                     (equal focus "HEAD")))
            (mercit--propertize-face (concat (if (equal focus "HEAD") "@" "*")
                                            (make-string (1- width) ?\s))
                                    'mercit-section-heading))
           ((if (eq type 'tag)
                (eq mercit-refs-show-commit-count 'all)
              mercit-refs-show-commit-count)
            (pcase-let ((`(,behind ,ahead)
                         (mercit-rev-diff-count mercit-buffer-upstream ref)))
              (mercit--propertize-face
               (cond ((> ahead  0) (concat "<" (number-to-string ahead)))
                     ((> behind 0) (concat (number-to-string behind) ">"))
                     (t "="))
               'mercit-dimmed)))
           (t "")))))

(defun mercit-refs--propertize-branch (branch ref &optional head-face)
  (let ((face (cdr (cl-find-if (pcase-lambda (`(,re . ,_))
                                 (string-match-p re ref))
                               mercit-ref-namespaces))))
    (mercit--propertize-face
     branch (if head-face (list face head-face) face))))

(defun mercit-refs--insert-refname-p (refname)
  (--if-let (-first (pcase-lambda (`(,key . ,_))
                      (if (functionp key)
                          (funcall key refname)
                        (string-match-p key refname)))
                    mercit-refs-filter-alist)
      (cdr it)
    t))

(defun mercit-refs--insert-cherry-commits (ref)
  (mercit-insert-section-body
    (let ((start (point))
          (mercit-insert-section--current nil))
      (mercit-git-wash (apply-partially #'mercit-log-wash-log 'cherry)
        "cherry" "-v" (mercit-abbrev-arg) mercit-buffer-upstream ref)
      (if (= (point) start)
          (message "No cherries for %s" ref)
        (mercit-make-margin-overlay nil t)))))

(defun mercit-refs--format-margin (commit)
  (save-excursion
    (goto-char (line-beginning-position 0))
    (let ((line (mercit-rev-format "%ct%cN" commit)))
      (mercit-log-format-margin commit
                               (substring line 10)
                               (substring line 0 10)))))

;;; _
(provide 'mercit-refs)
;;; mercit-refs.el ends here
