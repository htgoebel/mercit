;;; mercit-diff.el --- Inspect Mercurial diffs  -*- lexical-binding:t -*-

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

;; This library implements support for looking at Mercurial diffs and
;; commits.

;;; Code:

(require 'mercit-core)
(require 'git-commit)

(eval-when-compile (require 'ansi-color))
(require 'diff-mode)
(require 'image)
(require 'smerge-mode)

;; For `mercit-diff-popup'
(declare-function mercit-stash-show "mercit-stash" (stash &optional args files))
;; For `mercit-diff-visit-file'
(declare-function mercit-find-file-noselect "mercit-files" (rev file))
(declare-function mercit-status-setup-buffer "mercit-status" (&optional directory))
;; For `mercit-diff-while-committing'
(declare-function mercit-commit-diff-1 "mercit-commit" ())
(declare-function mercit-commit-message-buffer "mercit-commit" ())
;; For `mercit-insert-revision-gravatar'
(defvar gravatar-size)
;; For `mercit-show-commit' and `mercit-diff-show-or-scroll'
(declare-function mercit-current-blame-chunk "mercit-blame" (&optional type noerror))
(declare-function mercit-blame-mode "mercit-blame" (&optional arg))
(defvar mercit-blame-mode)
;; For `mercit-diff-show-or-scroll'
(declare-function git-rebase-current-line "git-rebase" ())
;; For `mercit-diff-unmerged'
(declare-function mercit-merge-in-progress-p "mercit-merge" ())
(declare-function mercit--merge-range "mercit-merge" (&optional head))
;; For `mercit-diff--dwim'
(declare-function forge--pullreq-range "forge-pullreq"
                  (pullreq &optional endpoints))
(declare-function forge--pullreq-ref "forge-pullreq" (pullreq))
;; For `mercit-diff-wash-diff'
(declare-function ansi-color-apply-on-region "ansi-color")
;; For `mercit-diff-wash-submodule'
(declare-function mercit-log-wash-log "mercit-log" (style args))
;; For keymaps and menus
(declare-function mercit-apply "mercit-apply" (&rest args))
(declare-function mercit-stage "mercit-apply" (&optional indent))
(declare-function mercit-unstage "mercit-apply" ())
(declare-function mercit-discard "mercit-apply" ())
(declare-function mercit-reverse "mercit-apply" (&rest args))
(declare-function mercit-file-rename "mercit-files" (file newname))
(declare-function mercit-file-untrack "mercit-files" (files &optional force))
(declare-function mercit-commit-add-log "mercit-commit" ())
(declare-function mercit-diff-trace-definition "mercit-log" ())
(declare-function mercit-patch-save "mercit-patch" (files &optional arg))
(declare-function mercit-do-async-shell-command "mercit-extras" (file))
(declare-function mercit-add-change-log-entry "mercit-extras"
                  (&optional whoami file-name other-window))
(declare-function mercit-add-change-log-entry-other-window "mercit-extras"
                  (&optional whoami file-name))
(declare-function mercit-diff-edit-hunk-commit "mercit-extras" (file))
(declare-function mercit-smerge-keep-current "mercit-apply" ())
(declare-function mercit-smerge-keep-upper "mercit-apply" ())
(declare-function mercit-smerge-keep-base "mercit-apply" ())
(declare-function mercit-smerge-keep-lower "mercit-apply" ())

(eval-when-compile
  (cl-pushnew 'orig-rev eieio--known-slot-names)
  (cl-pushnew 'action-type eieio--known-slot-names)
  (cl-pushnew 'target eieio--known-slot-names))

;;; Options
;;;; Diff Mode

(defgroup mercit-diff nil
  "Inspect and manipulate Mercurial diffs."
  :link '(info-link "(mercit)Diffing")
  :group 'mercit-commands
  :group 'mercit-modes)

(defcustom mercit-diff-mode-hook nil
  "Hook run after entering Mercit-Diff mode."
  :group 'mercit-diff
  :type 'hook)

(defcustom mercit-diff-sections-hook
  '(mercit-insert-diff
    mercit-insert-xref-buttons)
  "Hook run to insert sections into a `mercit-diff-mode' buffer."
  :package-version '(mercit . "2.3.0")
  :group 'mercit-diff
  :type 'hook)

(defcustom mercit-diff-expansion-threshold 60
  "After how many seconds not to expand anymore diffs.

Except in status buffers, diffs usually start out fully expanded.
Because that can take a long time, all diffs that haven't been
fontified during a refresh before the threshold defined here are
instead displayed with their bodies collapsed.

Note that this can cause sections that were previously expanded
to be collapsed.  So you should not pick a very low value here.

The hook function `mercit-diff-expansion-threshold' has to be a
member of `mercit-section-set-visibility-hook' for this option
to have any effect."
  :package-version '(mercit . "2.9.0")
  :group 'mercit-diff
  :type 'float)

(defcustom mercit-diff-highlight-hunk-body t
  "Whether to highlight bodies of selected hunk sections.
This only has an effect if `mercit-diff-highlight' is a
member of `mercit-section-highlight-hook', which see."
  :package-version '(mercit . "2.1.0")
  :group 'mercit-diff
  :type 'boolean)

(defcustom mercit-diff-highlight-hunk-region-functions
  '(mercit-diff-highlight-hunk-region-dim-outside
    mercit-diff-highlight-hunk-region-using-overlays)
  "The functions used to highlight the hunk-internal region.

`mercit-diff-highlight-hunk-region-dim-outside' overlays the outside
of the hunk internal selection with a face that causes the added and
removed lines to have the same background color as context lines.
This function should not be removed from the value of this option.

`mercit-diff-highlight-hunk-region-using-overlays' and
`mercit-diff-highlight-hunk-region-using-underline' emphasize the
region by placing delimiting horizontal lines before and after it.
The underline variant was implemented because Eli said that is
how we should do it.  However the overlay variant actually works
better.  Also see https://github.com/mercit/mercit/issues/2758.

Instead of, or in addition to, using delimiting horizontal lines,
to emphasize the boundaries, you may wish to emphasize the text
itself, using `mercit-diff-highlight-hunk-region-using-face'.

In terminal frames it's not possible to draw lines as the overlay
and underline variants normally do, so there they fall back to
calling the face function instead."
  :package-version '(mercit . "2.9.0")
  :set-after '(mercit-diff-show-lines-boundaries)
  :group 'mercit-diff
  :type 'hook
  :options '(mercit-diff-highlight-hunk-region-dim-outside
             mercit-diff-highlight-hunk-region-using-underline
             mercit-diff-highlight-hunk-region-using-overlays
             mercit-diff-highlight-hunk-region-using-face))

(defcustom mercit-diff-unmarked-lines-keep-foreground t
  "Whether `mercit-diff-highlight-hunk-region-dim-outside' preserves foreground.
When this is set to nil, then that function only adjusts the
foreground color but added and removed lines outside the region
keep their distinct foreground colors."
  :package-version '(mercit . "2.9.0")
  :group 'mercit-diff
  :type 'boolean)

(defcustom mercit-diff-refine-hunk nil
  "Whether to show word-granularity differences within diff hunks.

nil    Never show fine differences.
t      Show fine differences for the current diff hunk only.
`all'  Show fine differences for all displayed diff hunks."
  :group 'mercit-diff
  :safe (lambda (val) (memq val '(nil t all)))
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Current" t)
                 (const :tag "All" all)))

(defcustom mercit-diff-refine-ignore-whitespace smerge-refine-ignore-whitespace
  "Whether to ignore whitespace changes in word-granularity differences."
  :package-version '(mercit . "3.0.0")
  :set-after '(smerge-refine-ignore-whitespace)
  :group 'mercit-diff
  :safe 'booleanp
  :type 'boolean)

(put 'mercit-diff-refine-hunk 'permanent-local t)

(defcustom mercit-diff-adjust-tab-width nil
  "Whether to adjust the width of tabs in diffs.

Determining the correct width can be expensive if it requires
opening large and/or many files, so the widths are cached in
the variable `mercit-diff--tab-width-cache'.  Set that to nil
to invalidate the cache.

nil       Never adjust tab width.  Use `tab-width's value from
          the Mercit buffer itself instead.

t         If the corresponding file-visiting buffer exits, then
          use `tab-width's value from that buffer.  Doing this is
          cheap, so this value is used even if a corresponding
          cache entry exists.

`always'  If there is no such buffer, then temporarily visit the
          file to determine the value.

NUMBER    Like `always', but don't visit files larger than NUMBER
          bytes."
  :package-version '(mercit . "2.12.0")
  :group 'mercit-diff
  :type '(choice (const   :tag "Never" nil)
                 (const   :tag "If file-visiting buffer exists" t)
                 (integer :tag "If file isn't larger than N bytes")
                 (const   :tag "Always" always)))

(defcustom mercit-diff-paint-whitespace t
  "Specify where to highlight whitespace errors.

nil            Never highlight whitespace errors.
t              Highlight whitespace errors everywhere.
`uncommitted'  Only highlight whitespace errors in diffs
               showing uncommitted changes.

For backward compatibility `status' is treated as a synonym
for `uncommitted'.

The option `mercit-diff-paint-whitespace-lines' controls for
what lines (added/remove/context) errors are highlighted.

The options `mercit-diff-highlight-trailing' and
`mercit-diff-highlight-indentation' control what kind of
whitespace errors are highlighted."
  :group 'mercit-diff
  :safe (lambda (val) (memq val '(t nil uncommitted status)))
  :type '(choice (const :tag "In all diffs" t)
                 (const :tag "Only in uncommitted changes" uncommitted)
                 (const :tag "Never" nil)))

(defcustom mercit-diff-paint-whitespace-lines t
  "Specify in what kind of lines to highlight whitespace errors.

t         Highlight only in added lines.
`both'    Highlight in added and removed lines.
`all'     Highlight in added, removed and context lines."
  :package-version '(mercit . "3.0.0")
  :group 'mercit-diff
  :safe (lambda (val) (memq val '(t both all)))
  :type '(choice (const :tag "in added lines" t)
                 (const :tag "in added and removed lines" both)
                 (const :tag "in added, removed and context lines" all)))

(defcustom mercit-diff-highlight-trailing t
  "Whether to highlight whitespace at the end of a line in diffs.
Used only when `mercit-diff-paint-whitespace' is non-nil."
  :group 'mercit-diff
  :safe 'booleanp
  :type 'boolean)

(defcustom mercit-diff-highlight-indentation nil
  "Highlight the \"wrong\" indentation style.
Used only when `mercit-diff-paint-whitespace' is non-nil.

The value is an alist of the form ((REGEXP . INDENT)...).  The
path to the current repository is matched against each element
in reverse order.  Therefore if a REGEXP matches, then earlier
elements are not tried.

If the used INDENT is `tabs', highlight indentation with tabs.
If INDENT is an integer, highlight indentation with at least
that many spaces.  Otherwise, highlight neither."
  :group 'mercit-diff
  :type `(repeat (cons (string :tag "Directory regexp")
                       (choice (const :tag "Tabs" tabs)
                               (integer :tag "Spaces" :value ,tab-width)
                               (const :tag "Neither" nil)))))

(defcustom mercit-diff-hide-trailing-cr-characters
  (and (memq system-type '(ms-dos windows-nt)) t)
  "Whether to hide ^M characters at the end of a line in diffs."
  :package-version '(mercit . "2.6.0")
  :group 'mercit-diff
  :type 'boolean)

(defcustom mercit-diff-highlight-keywords t
  "Whether to highlight bracketed keywords in commit messages."
  :package-version '(mercit . "2.12.0")
  :group 'mercit-diff
  :type 'boolean)

(defcustom mercit-diff-extra-stat-arguments nil
  "Additional arguments to be used alongside `--stat'.

A list of zero or more arguments or a function that takes no
argument and returns such a list.  These arguments are allowed
here: `--stat-width', `--stat-name-width', `--stat-graph-width'
and `--compact-summary'.  See the git-diff(1) manpage."
  :package-version '(mercit . "3.0.0")
  :group 'mercit-diff
  :type '(radio (function-item mercit-diff-use-window-width-as-stat-width)
                function
                (list string)
                (const :tag "None" nil)))

;;;; File Diff

(defcustom mercit-diff-buffer-file-locked t
  "Whether `mercit-diff-buffer-file' uses a dedicated buffer."
  :package-version '(mercit . "2.7.0")
  :group 'mercit-commands
  :group 'mercit-diff
  :type 'boolean)

;;;; Revision Mode

(defgroup mercit-revision nil
  "Inspect and manipulate Mercurial commits."
  :link '(info-link "(mercit)Revision Buffer")
  :group 'mercit-modes)

(defcustom mercit-revision-mode-hook
  '(bug-reference-mode
    goto-address-mode)
  "Hook run after entering Mercit-Revision mode."
  :group 'mercit-revision
  :type 'hook
  :options '(bug-reference-mode
             goto-address-mode))

(defcustom mercit-revision-sections-hook
  '(mercit-insert-revision-tag
    mercit-insert-revision-headers
    mercit-insert-revision-message
    mercit-insert-revision-notes
    mercit-insert-revision-diff
    mercit-insert-xref-buttons)
  "Hook run to insert sections into a `mercit-revision-mode' buffer."
  :package-version '(mercit . "2.3.0")
  :group 'mercit-revision
  :type 'hook)

(defcustom mercit-revision-headers-format "\
Author:     %aN <%aE>
AuthorDate: %ad
Commit:     %cN <%cE>
CommitDate: %cd
"
  "Format string used to insert headers in revision buffers.

All headers in revision buffers are inserted by the section
inserter `mercit-insert-revision-headers'.  Some of the headers
are created by calling `git show --format=FORMAT' where FORMAT
is the format specified here.  Other headers are hard coded or
subject to option `mercit-revision-insert-related-refs'."
  :package-version '(mercit . "2.3.0")
  :group 'mercit-revision
  :type 'string)

(defcustom mercit-revision-insert-related-refs t
  "Whether to show related branches in revision buffers

`nil'   Don't show any related branches.
`t'     Show related local branches.
`all'   Show related local and remote branches.
`mixed' Show all containing branches and local merged branches."
  :package-version '(mercit . "2.1.0")
  :group 'mercit-revision
  :type '(choice (const :tag "don't" nil)
                 (const :tag "local only" t)
                 (const :tag "all related" all)
                 (const :tag "all containing, local merged" mixed)))

(defcustom mercit-revision-use-hash-sections 'quicker
  "Whether to turn hashes inside the commit message into sections.

If non-nil, then hashes inside the commit message are turned into
`commit' sections.  There is a trade off to be made between
performance and reliability:

- `slow' calls git for every word to be absolutely sure.
- `quick' skips words less than seven characters long.
- `quicker' additionally skips words that don't contain a number.
- `quickest' uses all words that are at least seven characters
  long and which contain at least one number as well as at least
  one letter.

If nil, then no hashes are turned into sections, but you can
still visit the commit at point using \"RET\"."
  :package-version '(mercit . "2.12.0")
  :group 'mercit-revision
  :type '(choice (const :tag "Use sections, quickest" quickest)
                 (const :tag "Use sections, quicker" quicker)
                 (const :tag "Use sections, quick" quick)
                 (const :tag "Use sections, slow" slow)
                 (const :tag "Don't use sections" nil)))

(defcustom mercit-revision-show-gravatars nil
  "Whether to show gravatar images in revision buffers.

If nil, then don't insert any gravatar images.  If t, then insert
both images.  If `author' or `committer', then insert only the
respective image.

If you have customized the option `mercit-revision-header-format'
and want to insert the images then you might also have to specify
where to do so.  In that case the value has to be a cons-cell of
two regular expressions.  The car specifies where to insert the
author's image.  The top half of the image is inserted right
after the matched text, the bottom half on the next line in the
same column.  The cdr specifies where to insert the committer's
image, accordingly.  Either the car or the cdr may be nil."
  :package-version '(mercit . "2.3.0")
  :group 'mercit-revision
  :type '(choice (const :tag "Don't show gravatars" nil)
                 (const :tag "Show gravatars" t)
                 (const :tag "Show author gravatar" author)
                 (const :tag "Show committer gravatar" committer)
                 (cons  :tag "Show gravatars using custom pattern."
                        (regexp :tag "Author regexp"    "^Author:     ")
                        (regexp :tag "Committer regexp" "^Commit:     "))))

(defcustom mercit-revision-use-gravatar-kludge nil
  "Whether to work around a bug which affects display of gravatars.

Gravatar images are spliced into two halves which are then
displayed on separate lines.  On macOS the splicing has a bug in
some Emacs builds, which causes the top and bottom halves to be
interchanged.  Enabling this option works around this issue by
interchanging the halves once more, which cancels out the effect
of the bug.

See https://github.com/mercit/mercit/issues/2265
and https://debbugs.gnu.org/cgi/bugreport.cgi?bug=7847.

Starting with Emacs 26.1 this kludge should not be required for
any build."
  :package-version '(mercit . "2.3.0")
  :group 'mercit-revision
  :type 'boolean)

(defcustom mercit-revision-fill-summary-line nil
  "Whether to fill excessively long summary lines.

If this is an integer, then the summary line is filled if it is
longer than either the limit specified here or `window-width'.

You may want to only set this locally in \".dir-locals-2.el\" for
repositories known to contain bad commit messages.

The body of the message is left alone because (a) most people who
write excessively long summary lines usually don't add a body and
(b) even people who have the decency to wrap their lines may have
a good reason to include a long line in the body sometimes."
  :package-version '(mercit . "2.90.0")
  :group 'mercit-revision
  :type '(choice (const   :tag "Don't fill" nil)
                 (integer :tag "Fill if longer than")))

(defcustom mercit-revision-filter-files-on-follow nil
  "Whether to honor file filter if log arguments include --follow.

When a commit is displayed from a log buffer, the resulting
revision buffer usually shares the log's file arguments,
restricting the diff to those files.  However, there's a
complication when the log arguments include --follow: if the log
follows a file across a rename event, keeping the file
restriction would mean showing an empty diff in revision buffers
for commits before the rename event.

When this option is nil, the revision buffer ignores the log's
filter if the log arguments include --follow.  If non-nil, the
log's file filter is always honored."
  :package-version '(mercit . "3.0.0")
  :group 'mercit-revision
  :type 'boolean)

;;;; Visit Commands

(defcustom mercit-diff-visit-previous-blob t
  "Whether `mercit-diff-visit-file' may visit the previous blob.

When this is t and point is on a removed line in a diff for a
committed change, then `mercit-diff-visit-file' visits the blob
from the last revision which still had that line.

Currently this is only supported for committed changes, for
staged and unstaged changes `mercit-diff-visit-file' always
visits the file in the working tree."
  :package-version '(mercit . "2.9.0")
  :group 'mercit-diff
  :type 'boolean)

(defcustom mercit-diff-visit-avoid-head-blob nil
  "Whether `mercit-diff-visit-file' avoids visiting a blob from `HEAD'.

By default `mercit-diff-visit-file' always visits the blob that
added the current line, while `mercit-diff-visit-worktree-file'
visits the respective file in the working tree.  For the `HEAD'
commit, the former command used to visit the worktree file too,
but that made it impossible to visit a blob from `HEAD'.

When point is on a removed line and that change has not been
committed yet, then `mercit-diff-visit-file' now visits the last
blob that still had that line, which is a blob from `HEAD'.
Previously this function used to visit the worktree file not
only for added lines but also for such removed lines.

If you prefer the old behaviors, then set this to t."
  :package-version '(mercit . "3.0.0")
  :group 'mercit-diff
  :type 'boolean)

;;; Faces

(defface mercit-diff-file-heading
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :weight bold))
  "Face for diff file headings."
  :group 'mercit-faces)

(defface mercit-diff-file-heading-highlight
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :inherit mercit-section-highlight))
  "Face for current diff file headings."
  :group 'mercit-faces)

(defface mercit-diff-file-heading-selection
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :inherit mercit-diff-file-heading-highlight
     :foreground "salmon4")
    (((class color) (background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :inherit mercit-diff-file-heading-highlight
     :foreground "LightSalmon3"))
  "Face for selected diff file headings."
  :group 'mercit-faces)

(defface mercit-diff-hunk-heading
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "grey90"
     :foreground "grey20")
    (((class color) (background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "grey25"
     :foreground "grey95"))
  "Face for diff hunk headings."
  :group 'mercit-faces)

(defface mercit-diff-hunk-heading-highlight
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "grey80"
     :foreground "grey20")
    (((class color) (background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "grey35"
     :foreground "grey95"))
  "Face for current diff hunk headings."
  :group 'mercit-faces)

(defface mercit-diff-hunk-heading-selection
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :inherit mercit-diff-hunk-heading-highlight
     :foreground "salmon4")
    (((class color) (background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :inherit mercit-diff-hunk-heading-highlight
     :foreground "LightSalmon3"))
  "Face for selected diff hunk headings."
  :group 'mercit-faces)

(defface mercit-diff-hunk-region
  `((t :inherit bold
       ,@(and (>= emacs-major-version 27)
              (list :extend (ignore-errors (face-attribute 'region :extend))))))
  "Face used by `mercit-diff-highlight-hunk-region-using-face'.

This face is overlaid over text that uses other hunk faces,
and those normally set the foreground and background colors.
The `:foreground' and especially the `:background' properties
should be avoided here.  Setting the latter would cause the
loss of information.  Good properties to set here are `:weight'
and `:slant'."
  :group 'mercit-faces)

(defface mercit-diff-revision-summary
  '((t :inherit mercit-diff-hunk-heading))
  "Face for commit message summaries."
  :group 'mercit-faces)

(defface mercit-diff-revision-summary-highlight
  '((t :inherit mercit-diff-hunk-heading-highlight))
  "Face for highlighted commit message summaries."
  :group 'mercit-faces)

(defface mercit-diff-lines-heading
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :inherit mercit-diff-hunk-heading-highlight
     :background "LightSalmon3")
    (((class color) (background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :inherit mercit-diff-hunk-heading-highlight
     :foreground "grey80"
     :background "salmon4"))
  "Face for diff hunk heading when lines are marked."
  :group 'mercit-faces)

(defface mercit-diff-lines-boundary
  `((t ,@(and (>= emacs-major-version 27) '(:extend t)) ; !important
       :inherit mercit-diff-lines-heading))
  "Face for boundary of marked lines in diff hunk."
  :group 'mercit-faces)

(defface mercit-diff-conflict-heading
  '((t :inherit mercit-diff-hunk-heading))
  "Face for conflict markers."
  :group 'mercit-faces)

(defface mercit-diff-added
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "#ddffdd"
     :foreground "#22aa22")
    (((class color) (background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "#335533"
     :foreground "#ddffdd"))
  "Face for lines in a diff that have been added."
  :group 'mercit-faces)

(defface mercit-diff-removed
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "#ffdddd"
     :foreground "#aa2222")
    (((class color) (background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "#553333"
     :foreground "#ffdddd"))
  "Face for lines in a diff that have been removed."
  :group 'mercit-faces)

(defface mercit-diff-our
  '((t :inherit mercit-diff-removed))
  "Face for lines in a diff for our side in a conflict."
  :group 'mercit-faces)

(defface mercit-diff-base
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "#ffffcc"
     :foreground "#aaaa11")
    (((class color) (background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "#555522"
     :foreground "#ffffcc"))
  "Face for lines in a diff for the base side in a conflict."
  :group 'mercit-faces)

(defface mercit-diff-their
  '((t :inherit mercit-diff-added))
  "Face for lines in a diff for their side in a conflict."
  :group 'mercit-faces)

(defface mercit-diff-context
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "grey50")
    (((class color) (background  dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "grey70"))
  "Face for lines in a diff that are unchanged."
  :group 'mercit-faces)

(defface mercit-diff-added-highlight
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "#cceecc"
     :foreground "#22aa22")
    (((class color) (background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "#336633"
     :foreground "#cceecc"))
  "Face for lines in a diff that have been added."
  :group 'mercit-faces)

(defface mercit-diff-removed-highlight
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "#eecccc"
     :foreground "#aa2222")
    (((class color) (background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "#663333"
     :foreground "#eecccc"))
  "Face for lines in a diff that have been removed."
  :group 'mercit-faces)

(defface mercit-diff-our-highlight
  '((t :inherit mercit-diff-removed-highlight))
  "Face for lines in a diff for our side in a conflict."
  :group 'mercit-faces)

(defface mercit-diff-base-highlight
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "#eeeebb"
     :foreground "#aaaa11")
    (((class color) (background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "#666622"
     :foreground "#eeeebb"))
  "Face for lines in a diff for the base side in a conflict."
  :group 'mercit-faces)

(defface mercit-diff-their-highlight
  '((t :inherit mercit-diff-added-highlight))
  "Face for lines in a diff for their side in a conflict."
  :group 'mercit-faces)

(defface mercit-diff-context-highlight
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "grey95"
     :foreground "grey50")
    (((class color) (background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "grey20"
     :foreground "grey70"))
  "Face for lines in the current context in a diff."
  :group 'mercit-faces)

(defface mercit-diff-whitespace-warning
  '((t :inherit trailing-whitespace))
  "Face for highlighting whitespace errors added lines."
  :group 'mercit-faces)

(defface mercit-diffstat-added
  '((((class color) (background light)) :foreground "#22aa22")
    (((class color) (background  dark)) :foreground "#448844"))
  "Face for plus sign in diffstat."
  :group 'mercit-faces)

(defface mercit-diffstat-removed
  '((((class color) (background light)) :foreground "#aa2222")
    (((class color) (background  dark)) :foreground "#aa4444"))
  "Face for minus sign in diffstat."
  :group 'mercit-faces)

;;; Arguments
;;;; Prefix Classes

(defclass mercit-diff-prefix (transient-prefix)
  ((history-key :initform 'mercit-diff)
   (major-mode  :initform 'mercit-diff-mode)))

(defclass mercit-diff-refresh-prefix (mercit-diff-prefix)
  ((history-key :initform 'mercit-diff)
   (major-mode  :initform nil)))

;;;; Prefix Methods

(cl-defmethod transient-init-value ((obj mercit-diff-prefix))
  (pcase-let ((`(,args ,files)
               (mercit-diff--get-value 'mercit-diff-mode
                                      mercit-prefix-use-buffer-arguments)))
    (unless (eq transient-current-command 'mercit-dispatch)
      (when-let ((file (mercit-file-relative-name)))
        (setq files (list file))))
    (oset obj value (if files `(("--" ,@files) ,args) args))))

(cl-defmethod transient-init-value ((obj mercit-diff-refresh-prefix))
  (oset obj value (if mercit-buffer-diff-files
                      `(("--" ,@mercit-buffer-diff-files)
                        ,mercit-buffer-diff-args)
                    mercit-buffer-diff-args)))

(cl-defmethod transient-set-value ((obj mercit-diff-prefix))
  (mercit-diff--set-value obj))

(cl-defmethod transient-save-value ((obj mercit-diff-prefix))
  (mercit-diff--set-value obj 'save))

;;;; Argument Access

(defun mercit-diff-arguments (&optional mode)
  "Return the current diff arguments."
  (if (memq transient-current-command '(mercit-diff mercit-diff-refresh))
      (pcase-let ((`(,args ,alist)
                   (-separate #'atom (transient-get-value))))
        (list args (cdr (assoc "--" alist))))
    (mercit-diff--get-value (or mode 'mercit-diff-mode))))

(defun mercit-diff--get-value (mode &optional use-buffer-args)
  (unless use-buffer-args
    (setq use-buffer-args mercit-direct-use-buffer-arguments))
  (let (args files)
    (cond
     ((and (memq use-buffer-args '(always selected current))
           (eq major-mode mode))
      (setq args  mercit-buffer-diff-args)
      (setq files mercit-buffer-diff-files))
     ((and (memq use-buffer-args '(always selected))
           (when-let ((buffer (mercit-get-mode-buffer
                               mode nil
                               (eq use-buffer-args 'selected))))
             (setq args  (buffer-local-value 'mercit-buffer-diff-args buffer))
             (setq files (buffer-local-value 'mercit-buffer-diff-files buffer))
             t)))
     ((plist-member (symbol-plist mode) 'mercit-diff-current-arguments)
      (setq args (get mode 'mercit-diff-current-arguments)))
     ((when-let ((elt (assq (intern (format "mercit-diff:%s" mode))
                            transient-values)))
        (setq args (cdr elt))
        t))
     (t
      (setq args (get mode 'mercit-diff-default-arguments))))
    (list args files)))

(defun mercit-diff--set-value (obj &optional save)
  (pcase-let* ((obj  (oref obj prototype))
               (mode (or (oref obj major-mode) major-mode))
               (key  (intern (format "mercit-diff:%s" mode)))
               (`(,args ,alist)
                (-separate #'atom (transient-get-value)))
               (files (cdr (assoc "--" alist))))
    (put mode 'mercit-diff-current-arguments args)
    (when save
      (setf (alist-get key transient-values) args)
      (transient-save-values))
    (transient--history-push obj)
    (setq mercit-buffer-diff-args args)
    (setq mercit-buffer-diff-files files)
    (mercit-refresh)))

;;; Commands
;;;; Prefix Commands

;;;###autoload (autoload 'mercit-diff "mercit-diff" nil t)
(transient-define-prefix mercit-diff ()
  "Show changes between different versions."
  :man-page "git-diff"
  :class 'mercit-diff-prefix
  ["Limit arguments"
   (mercit:--)
   (mercit-diff:--ignore-submodules)
   ("-b" "Ignore whitespace changes"      ("-b" "--ignore-space-change"))
   ("-w" "Ignore all whitespace"          ("-w" "--ignore-all-space"))
   (5 "-D" "Omit preimage for deletes"    ("-D" "--irreversible-delete"))]
  ["Context arguments"
   (mercit-diff:-U)
   ("-W" "Show surrounding functions"     ("-W" "--function-context"))]
  ["Tune arguments"
   (mercit-diff:--diff-algorithm)
   (mercit-diff:-M)
   (mercit-diff:-C)
   ("-x" "Disallow external diff drivers" "--no-ext-diff")
   ("-s" "Show stats"                     "--stat")
   ("=g" "Show signature"                 "--show-signature")
   (5 "-R" "Reverse sides"                "-R")
   (5 mercit-diff:--color-moved)
   (5 mercit-diff:--color-moved-ws)]
  ["Actions"
   [("d" "Dwim"          mercit-diff-dwim)
    ("r" "Diff range"    mercit-diff-range)
    ("p" "Diff paths"    mercit-diff-paths)]
   [("u" "Diff unstaged" mercit-diff-unstaged)
    ("s" "Diff staged"   mercit-diff-staged)
    ("w" "Diff worktree" mercit-diff-working-tree)]
   [("c" "Show commit"   mercit-show-commit)
    ("t" "Show stash"    mercit-stash-show)]])

;;;###autoload (autoload 'mercit-diff-refresh "mercit-diff" nil t)
(transient-define-prefix mercit-diff-refresh ()
  "Change the arguments used for the diff(s) in the current buffer."
  :man-page "git-diff"
  :class 'mercit-diff-refresh-prefix
  ["Limit arguments"
   (mercit:--)
   (mercit-diff:--ignore-submodules)
   ("-b" "Ignore whitespace changes"      ("-b" "--ignore-space-change"))
   ("-w" "Ignore all whitespace"          ("-w" "--ignore-all-space"))
   (5 "-D" "Omit preimage for deletes"    ("-D" "--irreversible-delete"))]
  ["Context arguments"
   (mercit-diff:-U)
   ("-W" "Show surrounding functions"     ("-W" "--function-context"))]
  ["Tune arguments"
   (mercit-diff:--diff-algorithm)
   (mercit-diff:-M)
   (mercit-diff:-C)
   ("-x" "Disallow external diff drivers" "--no-ext-diff")
   ("-s" "Show stats"                     "--stat"
    :if-derived mercit-diff-mode)
   ("=g" "Show signature"                 "--show-signature"
    :if-derived mercit-diff-mode)
   (5 "-R" "Reverse sides"                "-R"
      :if-derived mercit-diff-mode)
   (5 mercit-diff:--color-moved)
   (5 mercit-diff:--color-moved-ws)]
  [["Refresh"
    ("g" "buffer"                   mercit-diff-refresh)
    ("s" "buffer and set defaults"  transient-set  :transient nil)
    ("w" "buffer and save defaults" transient-save :transient nil)]
   ["Toggle"
    ("t" "hunk refinement"          mercit-diff-toggle-refine-hunk)
    ("F" "file filter"              mercit-diff-toggle-file-filter)
    ("b" "buffer lock"              mercit-toggle-buffer-lock
     :if-mode (mercit-diff-mode mercit-revision-mode mercit-stash-mode))]
   [:if-mode mercit-diff-mode
    :description "Do"
    ("r" "switch range type"        mercit-diff-switch-range-type)
    ("f" "flip revisions"           mercit-diff-flip-revs)]]
  (interactive)
  (if (not (eq transient-current-command 'mercit-diff-refresh))
      (transient-setup 'mercit-diff-refresh)
    (pcase-let ((`(,args ,files) (mercit-diff-arguments)))
      (setq mercit-buffer-diff-args args)
      (setq mercit-buffer-diff-files files))
    (mercit-refresh)))

;;;; Infix Commands

(transient-define-argument mercit:-- ()
  :description "Limit to files"
  :class 'transient-files
  :key "--"
  :argument "--"
  :prompt "Limit to file,s: "
  :reader #'mercit-read-files
  :multi-value t)

(defun mercit-read-files (prompt initial-input history &optional list-fn)
  (mercit-completing-read-multiple* prompt
                                   (funcall (or list-fn #'mercit-list-files))
                                   nil nil
                                   (or initial-input (mercit-file-at-point))
                                   history))

(transient-define-argument mercit-diff:-U ()
  :description "Context lines"
  :class 'transient-option
  :argument "-U"
  :reader #'transient-read-number-N0)

(transient-define-argument mercit-diff:-M ()
  :description "Detect renames"
  :class 'transient-option
  :argument "-M"
  :allow-empty t
  :reader #'transient-read-number-N+)

(transient-define-argument mercit-diff:-C ()
  :description "Detect copies"
  :class 'transient-option
  :argument "-C"
  :allow-empty t
  :reader #'transient-read-number-N+)

(transient-define-argument mercit-diff:--diff-algorithm ()
  :description "Diff algorithm"
  :class 'transient-option
  :key "-A"
  :argument "--diff-algorithm="
  :reader #'mercit-diff-select-algorithm)

(defun mercit-diff-select-algorithm (&rest _ignore)
  (mercit-read-char-case nil t
    (?d "[d]efault"   "default")
    (?m "[m]inimal"   "minimal")
    (?p "[p]atience"  "patience")
    (?h "[h]istogram" "histogram")))

(transient-define-argument mercit-diff:--ignore-submodules ()
  :description "Ignore submodules"
  :class 'transient-option
  :key "-i"
  :argument "--ignore-submodules="
  :reader #'mercit-diff-select-ignore-submodules)

(defun mercit-diff-select-ignore-submodules (&rest _ignored)
  (mercit-read-char-case "Ignore submodules " t
    (?u "[u]ntracked" "untracked")
    (?d "[d]irty"     "dirty")
    (?a "[a]ll"       "all")))

(transient-define-argument mercit-diff:--color-moved ()
  :description "Color moved lines"
  :class 'transient-option
  :key "-m"
  :argument "--color-moved="
  :reader #'mercit-diff-select-color-moved-mode)

(defun mercit-diff-select-color-moved-mode (&rest _ignore)
  (mercit-read-char-case "Color moved " t
    (?d "[d]efault" "default")
    (?p "[p]lain"   "plain")
    (?b "[b]locks"  "blocks")
    (?z "[z]ebra"   "zebra")
    (?Z "[Z] dimmed-zebra" "dimmed-zebra")))

(transient-define-argument mercit-diff:--color-moved-ws ()
  :description "Whitespace treatment for --color-moved"
  :class 'transient-option
  :key "=w"
  :argument "--color-moved-ws="
  :reader #'mercit-diff-select-color-moved-ws-mode)

(defun mercit-diff-select-color-moved-ws-mode (&rest _ignore)
  (mercit-read-char-case "Ignore whitespace " t
    (?i "[i]ndentation"  "allow-indentation-change")
    (?e "[e]nd of line"  "ignore-space-at-eol")
    (?s "[s]pace change" "ignore-space-change")
    (?a "[a]ll space"    "ignore-all-space")
    (?n "[n]o"           "no")))

;;;; Setup Commands

;;;###autoload
(defun mercit-diff-dwim (&optional args files)
  "Show changes for the thing at point."
  (interactive (mercit-diff-arguments))
  (let ((default-directory default-directory)
        (section (mercit-current-section)))
    (cond
     ((mercit-section-match 'module section)
      (setq default-directory
            (expand-file-name
             (file-name-as-directory (oref section value))))
      (mercit-diff-range (oref section range)))
     (t
      (when (mercit-section-match 'module-commit section)
        (setq args nil)
        (setq files nil)
        (setq default-directory
              (expand-file-name
               (file-name-as-directory (mercit-section-parent-value section)))))
      (pcase (mercit-diff--dwim)
        ('unmerged (mercit-diff-unmerged args files))
        ('unstaged (mercit-diff-unstaged args files))
        ('staged
         (let ((file (mercit-file-at-point)))
           (if (and file (equal (cddr (car (mercit-file-status file))) '(?D ?U)))
               ;; File was deleted by us and modified by them.  Show the latter.
               (mercit-diff-unmerged args (list file))
             (mercit-diff-staged nil args files))))
        (`(stash . ,value) (mercit-stash-show value args))
        (`(commit . ,value)
         (mercit-diff-range (format "%s^..%s" value value) args files))
        ((and range (pred stringp))
         (mercit-diff-range range args files))
        (_ (call-interactively #'mercit-diff-range)))))))

(defun mercit-diff--dwim ()
  "Return information for performing DWIM diff.

The information can be in three forms:
1. TYPE
   A symbol describing a type of diff where no additional information
   is needed to generate the diff.  Currently, this includes `staged',
   `unstaged' and `unmerged'.
2. (TYPE . VALUE)
   Like #1 but the diff requires additional information, which is
   given by VALUE.  Currently, this includes `commit' and `stash',
   where VALUE is the given commit or stash, respectively.
3. RANGE
   A string indicating a diff range.

If no DWIM context is found, nil is returned."
  (cond
   ((when-let* ((commits (mercit-region-values '(commit branch) t)))
      ;; Cannot use and-let* because of debbugs#31840.
      (deactivate-mark)
      (concat (car (last commits)) ".." (car commits))))
   (mercit-buffer-refname
    (cons 'commit mercit-buffer-refname))
   ((derived-mode-p 'mercit-stash-mode)
    (cons 'commit
          (mercit-section-case
            (commit (oref it value))
            (file (thread-first it
                    (oref parent)
                    (oref value)))
            (hunk (thread-first it
                    (oref parent)
                    (oref parent)
                    (oref value))))))
   ((derived-mode-p 'mercit-revision-mode)
    (cons 'commit mercit-buffer-revision))
   ((derived-mode-p 'mercit-diff-mode)
    mercit-buffer-range)
   (t
    (mercit-section-case
      ([* unstaged] 'unstaged)
      ([* staged] 'staged)
      (unmerged 'unmerged)
      (unpushed (mercit-diff--range-to-endpoints (oref it value)))
      (unpulled (mercit-diff--range-to-endpoints (oref it value)))
      (branch (let ((current (mercit-get-current-branch))
                    (atpoint (oref it value)))
                (if (equal atpoint current)
                    (--if-let (mercit-get-upstream-branch)
                        (format "%s...%s" it current)
                      (if (mercit-anything-modified-p)
                          current
                        (cons 'commit current)))
                  (format "%s...%s"
                          (or current "HEAD")
                          atpoint))))
      (commit (cons 'commit (oref it value)))
      ([file commit] (cons 'commit (oref (oref it parent) value)))
      ([hunk file commit]
       (cons 'commit (oref (oref (oref it parent) parent) value)))
      (stash (cons 'stash (oref it value)))
      (pullreq (forge--pullreq-range (oref it value) t))))))

(defun mercit-diff--range-to-endpoints (range)
  (cond ((string-match "\\.\\.\\." range) (replace-match ".."  nil nil range))
        ((string-match "\\.\\."    range) (replace-match "..." nil nil range))
        (t range)))

(defun mercit-diff--region-range (&optional interactive mbase)
  (when-let* ((commits (mercit-region-values '(commit branch) t)) ;debbugs#31840
              (revA (car (last commits)))
              (revB (car commits)))
    (when interactive
      (deactivate-mark))
    (if mbase
        (let ((base (mercit-git-string "merge-base" revA revB)))
          (cond
           ((string= (mercit-rev-parse revA) base)
            (format "%s..%s" revA revB))
           ((string= (mercit-rev-parse revB) base)
            (format "%s..%s" revB revA))
           (interactive
            (let ((main (mercit-completing-read "View changes along"
                                               (list revA revB)
                                               nil t nil nil revB)))
              (format "%s...%s"
                      (if (string= main revB) revA revB) main)))
           (t "%s...%s" revA revB)))
      (format "%s..%s" revA revB))))

(defun mercit-diff-read-range-or-commit (prompt &optional secondary-default mbase)
  "Read range or revision with special diff range treatment.
If MBASE is non-nil, prompt for which rev to place at the end of
a \"revA...revB\" range.  Otherwise, always construct
\"revA..revB\" range."
  (or (mercit-diff--region-range t mbase)
      (mercit-read-range prompt
                        (or (pcase (mercit-diff--dwim)
                              (`(commit . ,value)
                               (format "%s^..%s" value value))
                              ((and range (pred stringp))
                               range))
                            secondary-default
                            (mercit-get-current-branch)))))

;;;###autoload
(defun mercit-diff-range (rev-or-range &optional args files)
  "Show differences between two commits.

REV-OR-RANGE should be a range or a single revision.  If it is a
revision, then show changes in the working tree relative to that
revision.  If it is a range, but one side is omitted, then show
changes relative to `HEAD'.

If the region is active, use the revisions on the first and last
line of the region as the two sides of the range.  With a prefix
argument, instead of diffing the revisions, choose a revision to
view changes along, starting at the common ancestor of both
revisions (i.e., use a \"...\" range)."
  (interactive (cons (mercit-diff-read-range-or-commit "Diff for range"
                                                      nil current-prefix-arg)
                     (mercit-diff-arguments)))
  (mercit-diff-setup-buffer rev-or-range nil args files))

;;;###autoload
(defun mercit-diff-working-tree (&optional rev args files)
  "Show changes between the current working tree and the `HEAD' commit.
With a prefix argument show changes between the working tree and
a commit read from the minibuffer."
  (interactive
   (cons (and current-prefix-arg
              (mercit-read-branch-or-commit "Diff working tree and commit"))
         (mercit-diff-arguments)))
  (mercit-diff-setup-buffer (or rev "HEAD") nil args files))

;;;###autoload
(defun mercit-diff-staged (&optional rev args files)
  "Show changes between the index and the `HEAD' commit.
With a prefix argument show changes between the index and
a commit read from the minibuffer."
  (interactive
   (cons (and current-prefix-arg
              (mercit-read-branch-or-commit "Diff index and commit"))
         (mercit-diff-arguments)))
  (mercit-diff-setup-buffer rev "--cached" args files))

;;;###autoload
(defun mercit-diff-unstaged (&optional args files)
  "Show changes between the working tree and the index."
  (interactive (mercit-diff-arguments))
  (mercit-diff-setup-buffer nil nil args files))

;;;###autoload
(defun mercit-diff-unmerged (&optional args files)
  "Show changes that are being merged."
  (interactive (mercit-diff-arguments))
  (unless (mercit-merge-in-progress-p)
    (user-error "No merge is in progress"))
  (mercit-diff-setup-buffer (mercit--merge-range) nil args files))

;;;###autoload
(defun mercit-diff-while-committing ()
  "While committing, show the changes that are about to be committed.
While amending, invoking the command again toggles between
showing just the new changes or all the changes that will
be committed."
  (interactive)
  (unless (mercit-commit-message-buffer)
    (user-error "No commit in progress"))
  (mercit-commit-diff-1))

(define-key git-commit-mode-map
  (kbd "C-c C-d") #'mercit-diff-while-committing)

;;;###autoload
(defun mercit-diff-buffer-file ()
  "Show diff for the blob or file visited in the current buffer.

When the buffer visits a blob, then show the respective commit.
When the buffer visits a file, then show the differences between
`HEAD' and the working tree.  In both cases limit the diff to
the file or blob."
  (interactive)
  (require 'mercit)
  (if-let ((file (mercit-file-relative-name)))
      (if mercit-buffer-refname
          (mercit-show-commit mercit-buffer-refname
                             (car (mercit-show-commit--arguments))
                             (list file))
        (save-buffer)
        (let ((line (line-number-at-pos))
              (col (current-column)))
          (with-current-buffer
              (mercit-diff-setup-buffer (or (mercit-get-current-branch) "HEAD")
                                       nil
                                       (car (mercit-diff-arguments))
                                       (list file)
                                       mercit-diff-buffer-file-locked)
            (mercit-diff--goto-position file line col))))
    (user-error "Buffer isn't visiting a file")))

;;;###autoload
(defun mercit-diff-paths (a b)
  "Show changes between any two files on disk."
  (interactive (list (read-file-name "First file: " nil nil t)
                     (read-file-name "Second file: " nil nil t)))
  (mercit-diff-setup-buffer nil "--no-index"
                           nil (list (mercit-convert-filename-for-git
                                      (expand-file-name a))
                                     (mercit-convert-filename-for-git
                                      (expand-file-name b)))))

(defun mercit-show-commit--arguments ()
  (pcase-let ((`(,args ,diff-files)
               (mercit-diff-arguments 'mercit-revision-mode)))
    (list args (if (derived-mode-p 'mercit-log-mode)
                   (and (or mercit-revision-filter-files-on-follow
                            (not (member "--follow" mercit-buffer-log-args)))
                        mercit-buffer-log-files)
                 diff-files))))

;;;###autoload
(defun mercit-show-commit (rev &optional args files module)
  "Visit the revision at point in another buffer.
If there is no revision at point or with a prefix argument prompt
for a revision."
  (interactive
   (pcase-let* ((mcommit (mercit-section-value-if 'module-commit))
                (atpoint (or mcommit
                             (mercit-thing-at-point 'git-revision t)
                             (mercit-branch-or-commit-at-point)))
                (`(,args ,files) (mercit-show-commit--arguments)))
     (list (or (and (not current-prefix-arg) atpoint)
               (mercit-read-branch-or-commit "Show commit" atpoint))
           args
           files
           (and mcommit
                (mercit-section-parent-value (mercit-current-section))))))
  (require 'mercit)
  (let* ((file (mercit-file-relative-name))
         (ln (and file (line-number-at-pos))))
    (mercit-with-toplevel
      (when module
        (setq default-directory
              (expand-file-name (file-name-as-directory module))))
      (unless (mercit-commit-p rev)
        (user-error "%s is not a commit" rev))
      (when file
        (save-buffer))
      (let ((buf (mercit-revision-setup-buffer rev args files)))
        (when file
          (let ((line (mercit-diff-visit--offset file (list "-R" rev) ln))
                (col (current-column)))
            (with-current-buffer buf
              (mercit-diff--goto-position file line col))))))))

(defun mercit-diff--locate-hunk (file line &optional parent)
  (and-let* ((diff (cl-find-if (lambda (section)
                                 (and (cl-typep section 'mercit-file-section)
                                      (equal (oref section value) file)))
                               (oref (or parent mercit-root-section) children))))
    (let (hunk (hunks (oref diff children)))
      (cl-block nil
        (while (setq hunk (pop hunks))
          (when-let ((range (oref hunk to-range)))
            (pcase-let* ((`(,beg ,len) range)
                         (end (+ beg len)))
              (cond ((>  beg line)     (cl-return (list diff nil)))
                    ((<= beg line end) (cl-return (list hunk t)))
                    ((null hunks)      (cl-return (list hunk nil)))))))))))

(defun mercit-diff--goto-position (file line column &optional parent)
  (when-let ((pos (mercit-diff--locate-hunk file line parent)))
    (pcase-let ((`(,section ,exact) pos))
      (cond ((cl-typep section 'mercit-file-section)
             (goto-char (oref section start)))
            (exact
             (goto-char (oref section content))
             (let ((pos (car (oref section to-range))))
               (while (or (< pos line)
                          (= (char-after) ?-))
                 (unless (= (char-after) ?-)
                   (cl-incf pos))
                 (forward-line)))
             (forward-char (1+ column)))
            (t
             (goto-char (oref section start))
             (setq section (oref section parent))))
      (while section
        (when (oref section hidden)
          (mercit-section-show section))
        (setq section (oref section parent))))
    (mercit-section-update-highlight)
    t))

;;;; Setting Commands

(defun mercit-diff-switch-range-type ()
  "Convert diff range type.
Change \"revA..revB\" to \"revA...revB\", or vice versa."
  (interactive)
  (if (and mercit-buffer-range
           (derived-mode-p 'mercit-diff-mode)
           (string-match mercit-range-re mercit-buffer-range))
      (setq mercit-buffer-range
            (replace-match (if (string= (match-string 2 mercit-buffer-range) "..")
                               "..."
                             "..")
                           t t mercit-buffer-range 2))
    (user-error "No range to change"))
  (mercit-refresh))

(defun mercit-diff-flip-revs ()
  "Swap revisions in diff range.
Change \"revA..revB\" to \"revB..revA\"."
  (interactive)
  (if (and mercit-buffer-range
           (derived-mode-p 'mercit-diff-mode)
           (string-match mercit-range-re mercit-buffer-range))
      (progn
        (setq mercit-buffer-range
              (concat (match-string 3 mercit-buffer-range)
                      (match-string 2 mercit-buffer-range)
                      (match-string 1 mercit-buffer-range)))
        (mercit-refresh))
    (user-error "No range to swap")))

(defun mercit-diff-toggle-file-filter ()
  "Toggle the file restriction of the current buffer's diffs.
If the current buffer's mode is derived from `mercit-log-mode',
toggle the file restriction in the repository's revision buffer
instead."
  (interactive)
  (cl-flet ((toggle ()
              (if (or mercit-buffer-diff-files
                      mercit-buffer-diff-files-suspended)
                  (cl-rotatef mercit-buffer-diff-files
                              mercit-buffer-diff-files-suspended)
                (setq mercit-buffer-diff-files
                      (transient-infix-read 'mercit:--)))
              (mercit-refresh)))
    (cond
     ((derived-mode-p 'mercit-log-mode
                      'mercit-cherry-mode
                      'mercit-reflog-mode)
      (if-let ((buffer (mercit-get-mode-buffer 'mercit-revision-mode)))
          (with-current-buffer buffer (toggle))
        (message "No revision buffer")))
     ((local-variable-p 'mercit-buffer-diff-files)
      (toggle))
     (t
      (user-error "Cannot toggle file filter in this buffer")))))

(defun mercit-diff-less-context (&optional count)
  "Decrease the context for diff hunks by COUNT lines."
  (interactive "p")
  (mercit-diff-set-context (lambda (cur) (max 0 (- (or cur 0) count)))))

(defun mercit-diff-more-context (&optional count)
  "Increase the context for diff hunks by COUNT lines."
  (interactive "p")
  (mercit-diff-set-context (lambda (cur) (+ (or cur 0) count))))

(defun mercit-diff-default-context ()
  "Reset context for diff hunks to the default height."
  (interactive)
  (mercit-diff-set-context #'ignore))

(defun mercit-diff-set-context (fn)
  (let* ((def (--if-let (mercit-get "diff.context") (string-to-number it) 3))
         (val mercit-buffer-diff-args)
         (arg (--first (string-match "^-U\\([0-9]+\\)?$" it) val))
         (num (--if-let (and arg (match-string 1 arg)) (string-to-number it) def))
         (val (delete arg val))
         (num (funcall fn num))
         (arg (and num (not (= num def)) (format "-U%i" num)))
         (val (if arg (cons arg val) val)))
    (setq mercit-buffer-diff-args val))
  (mercit-refresh))

(defun mercit-diff-context-p ()
  (if-let ((arg (--first (string-match "^-U\\([0-9]+\\)$" it)
                         mercit-buffer-diff-args)))
      (not (equal arg "-U0"))
    t))

(defun mercit-diff-ignore-any-space-p ()
  (--any-p (member it mercit-buffer-diff-args)
           '("--ignore-cr-at-eol"
             "--ignore-space-at-eol"
             "--ignore-space-change" "-b"
             "--ignore-all-space" "-w"
             "--ignore-blank-space")))

(defun mercit-diff-toggle-refine-hunk (&optional style)
  "Turn diff-hunk refining on or off.

If hunk refining is currently on, then hunk refining is turned off.
If hunk refining is off, then hunk refining is turned on, in
`selected' mode (only the currently selected hunk is refined).

With a prefix argument, the \"third choice\" is used instead:
If hunk refining is currently on, then refining is kept on, but
the refining mode (`selected' or `all') is switched.
If hunk refining is off, then hunk refining is turned on, in
`all' mode (all hunks refined).

Customize variable `mercit-diff-refine-hunk' to change the default mode."
  (interactive "P")
  (setq-local mercit-diff-refine-hunk
              (if style
                  (if (eq mercit-diff-refine-hunk 'all) t 'all)
                (not mercit-diff-refine-hunk)))
  (mercit-diff-update-hunk-refinement))

;;;; Visit Commands
;;;;; Dwim Variants

(defun mercit-diff-visit-file (file &optional other-window)
  "From a diff visit the appropriate version of FILE.

Display the buffer in the selected window.  With a prefix
argument OTHER-WINDOW display the buffer in another window
instead.

Visit the worktree version of the appropriate file.  The location
of point inside the diff determines which file is being visited.
The visited version depends on what changes the diff is about.

1. If the diff shows uncommitted changes (i.e. stage or unstaged
   changes), then visit the file in the working tree (i.e. the
   same \"real\" file that `find-file' would visit.  In all other
   cases visit a \"blob\" (i.e. the version of a file as stored
   in some commit).

2. If point is on a removed line, then visit the blob for the
   first parent of the commit that removed that line, i.e. the
   last commit where that line still exists.

3. If point is on an added or context line, then visit the blob
   that adds that line, or if the diff shows from more than a
   single commit, then visit the blob from the last of these
   commits.

In the file-visiting buffer also go to the line that corresponds
to the line that point is on in the diff.

Note that this command only works if point is inside a diff.
In other cases `mercit-find-file' (which see) has to be used."
  (interactive (list (mercit-file-at-point t t) current-prefix-arg))
  (mercit-diff-visit-file--internal file nil
                                   (if other-window
                                       #'switch-to-buffer-other-window
                                     #'pop-to-buffer-same-window)))

(defun mercit-diff-visit-file-other-window (file)
  "From a diff visit the appropriate version of FILE in another window.
Like `mercit-diff-visit-file' but use
`switch-to-buffer-other-window'."
  (interactive (list (mercit-file-at-point t t)))
  (mercit-diff-visit-file--internal file nil #'switch-to-buffer-other-window))

(defun mercit-diff-visit-file-other-frame (file)
  "From a diff visit the appropriate version of FILE in another frame.
Like `mercit-diff-visit-file' but use
`switch-to-buffer-other-frame'."
  (interactive (list (mercit-file-at-point t t)))
  (mercit-diff-visit-file--internal file nil #'switch-to-buffer-other-frame))

;;;;; Worktree Variants

(defun mercit-diff-visit-worktree-file (file &optional other-window)
  "From a diff visit the worktree version of FILE.

Display the buffer in the selected window.  With a prefix
argument OTHER-WINDOW display the buffer in another window
instead.

Visit the worktree version of the appropriate file.  The location
of point inside the diff determines which file is being visited.

Unlike `mercit-diff-visit-file' always visits the \"real\" file in
the working tree, i.e the \"current version\" of the file.

In the file-visiting buffer also go to the line that corresponds
to the line that point is on in the diff.  Lines that were added
or removed in the working tree, the index and other commits in
between are automatically accounted for."
  (interactive (list (mercit-file-at-point t t) current-prefix-arg))
  (mercit-diff-visit-file--internal file t
                                   (if other-window
                                       #'switch-to-buffer-other-window
                                     #'pop-to-buffer-same-window)))

(defun mercit-diff-visit-worktree-file-other-window (file)
  "From a diff visit the worktree version of FILE in another window.
Like `mercit-diff-visit-worktree-file' but use
`switch-to-buffer-other-window'."
  (interactive (list (mercit-file-at-point t t)))
  (mercit-diff-visit-file--internal file t #'switch-to-buffer-other-window))

(defun mercit-diff-visit-worktree-file-other-frame (file)
  "From a diff visit the worktree version of FILE in another frame.
Like `mercit-diff-visit-worktree-file' but use
`switch-to-buffer-other-frame'."
  (interactive (list (mercit-file-at-point t t)))
  (mercit-diff-visit-file--internal file t #'switch-to-buffer-other-frame))

;;;;; Internal

(defun mercit-diff-visit-file--internal (file force-worktree fn)
  "From a diff visit the appropriate version of FILE.
If FORCE-WORKTREE is non-nil, then visit the worktree version of
the file, even if the diff is about a committed change.  Use FN
to display the buffer in some window."
  (if (mercit-file-accessible-directory-p file)
      (mercit-diff-visit-directory file force-worktree)
    (pcase-let ((`(,buf ,pos)
                 (mercit-diff-visit-file--noselect file force-worktree)))
      (funcall fn buf)
      (mercit-diff-visit-file--setup buf pos)
      buf)))

(defun mercit-diff-visit-directory (directory &optional other-window)
  "Visit DIRECTORY in some window.
Display the buffer in the selected window unless OTHER-WINDOW is
non-nil.  If DIRECTORY is the top-level directory of the current
repository, then visit the containing directory using Dired and
in the Dired buffer put point on DIRECTORY.  Otherwise display
the Mercit-Status buffer for DIRECTORY."
  (if (equal (mercit-toplevel directory)
             (mercit-toplevel))
      (dired-jump other-window (concat directory "/."))
    (let ((display-buffer-overriding-action
           (if other-window
               '(nil (inhibit-same-window . t))
             '(display-buffer-same-window))))
      (mercit-status-setup-buffer directory))))

(defun mercit-diff-visit-file--setup (buf pos)
  (if-let ((win (get-buffer-window buf 'visible)))
      (with-selected-window win
        (when pos
          (unless (<= (point-min) pos (point-max))
            (widen))
          (goto-char pos))
        (when (and buffer-file-name
                   (mercit-anything-unmerged-p buffer-file-name))
          (smerge-start-session))
        (run-hooks 'mercit-diff-visit-file-hook))
    (error "File buffer is not visible")))

(defun mercit-diff-visit-file--noselect (&optional file goto-worktree)
  (unless file
    (setq file (mercit-file-at-point t t)))
  (let* ((hunk (mercit-diff-visit--hunk))
         (goto-from (and hunk
                         (mercit-diff-visit--goto-from-p hunk goto-worktree)))
         (line (and hunk (mercit-diff-hunk-line   hunk goto-from)))
         (col  (and hunk (mercit-diff-hunk-column hunk goto-from)))
         (spec (mercit-diff--dwim))
         (rev  (if goto-from
                   (mercit-diff-visit--range-from spec)
                 (mercit-diff-visit--range-to spec)))
         (buf  (if (or goto-worktree
                       (and (not (stringp rev))
                            (or mercit-diff-visit-avoid-head-blob
                                (not goto-from))))
                   (or (get-file-buffer file)
                       (find-file-noselect file))
                 (mercit-find-file-noselect (if (stringp rev) rev "HEAD")
                                           file))))
    (if line
        (with-current-buffer buf
          (cond ((eq rev 'staged)
                 (setq line (mercit-diff-visit--offset file nil line)))
                ((and goto-worktree
                      (stringp rev))
                 (setq line (mercit-diff-visit--offset file rev line))))
          (list buf (save-restriction
                      (widen)
                      (goto-char (point-min))
                      (forward-line (1- line))
                      (move-to-column col)
                      (point))))
      (list buf nil))))

(defun mercit-diff-visit--hunk ()
  (when-let* ((scope (mercit-diff-scope)) ;debbugs#31840
             (section (mercit-current-section)))
    (cl-case scope
      ((file files)
       (setq section (car (oref section children))))
      (list
       (setq section (car (oref section children)))
       (when section
         (setq section (car (oref section children))))))
    (and
     ;; Unmerged files appear in the list of staged changes
     ;; but unlike in the list of unstaged changes no diffs
     ;; are shown here.  In that case `section' is nil.
     section
     ;; Currently the `hunk' type is also abused for file
     ;; mode changes, which we are not interested in here.
     (not (equal (oref section value) '(chmod)))
     section)))

(defun mercit-diff-visit--goto-from-p (section in-worktree)
  (and mercit-diff-visit-previous-blob
       (not in-worktree)
       (not (oref section combined))
       (not (< (mercit-point) (oref section content)))
       (= (char-after (line-beginning-position)) ?-)))

(defvar mercit-diff-visit-jump-to-change t)

(defun mercit-diff-hunk-line (section goto-from)
  (save-excursion
    (goto-char (line-beginning-position))
    (with-slots (content combined from-ranges from-range to-range) section
      (when (or from-range to-range)
        (when (and mercit-diff-visit-jump-to-change (< (point) content))
          (goto-char content)
          (re-search-forward "^[-+]"))
        (+ (car (if goto-from from-range to-range))
           (let ((prefix (if combined (length from-ranges) 1))
                 (target (point))
                 (offset 0))
             (goto-char content)
             (while (< (point) target)
               (unless (string-search
                        (if goto-from "+" "-")
                        (buffer-substring (point) (+ (point) prefix)))
                 (cl-incf offset))
               (forward-line))
             offset))))))

(defun mercit-diff-hunk-column (section goto-from)
  (if (or (< (mercit-point)
             (oref section content))
          (and (not goto-from)
               (= (char-after (line-beginning-position)) ?-)))
      0
    (max 0 (- (+ (current-column) 2)
              (length (oref section value))))))

(defun mercit-diff-visit--range-from (spec)
  (cond ((consp spec)
         (concat (cdr spec) "^"))
        ((stringp spec)
         (car (mercit-split-range spec)))
        (t
         spec)))

(defun mercit-diff-visit--range-to (spec)
  (if (symbolp spec)
      spec
    (let ((rev (if (consp spec)
                   (cdr spec)
                 (cdr (mercit-split-range spec)))))
      (if (and mercit-diff-visit-avoid-head-blob
               (mercit-rev-head-p rev))
          'unstaged
        rev))))

(defun mercit-diff-visit--offset (file rev line)
  (let ((offset 0))
    (with-temp-buffer
      (save-excursion
        (mercit-with-toplevel
          (mercit-git-insert "diff" rev "--" file)))
      (catch 'found
        (while (re-search-forward
                "^@@ -\\([0-9]+\\),\\([0-9]+\\) \\+\\([0-9]+\\),\\([0-9]+\\) @@.*\n"
                nil t)
          (let ((from-beg (string-to-number (match-string 1)))
                (from-len (string-to-number (match-string 2)))
                (  to-len (string-to-number (match-string 4))))
            (if (<= from-beg line)
                (if (< (+ from-beg from-len) line)
                    (cl-incf offset (- to-len from-len))
                  (let ((rest (- line from-beg)))
                    (while (> rest 0)
                      (pcase (char-after)
                        (?\s                  (cl-decf rest))
                        (?-  (cl-decf offset) (cl-decf rest))
                        (?+  (cl-incf offset)))
                      (forward-line))))
              (throw 'found nil))))))
    (+ line offset)))

;;;; Scroll Commands

(defun mercit-diff-show-or-scroll-up ()
  "Update the commit or diff buffer for the thing at point.

Either show the commit or stash at point in the appropriate
buffer, or if that buffer is already being displayed in the
current frame and contains information about that commit or
stash, then instead scroll the buffer up.  If there is no
commit or stash at point, then prompt for a commit."
  (interactive)
  (mercit-diff-show-or-scroll #'scroll-up))

(defun mercit-diff-show-or-scroll-down ()
  "Update the commit or diff buffer for the thing at point.

Either show the commit or stash at point in the appropriate
buffer, or if that buffer is already being displayed in the
current frame and contains information about that commit or
stash, then instead scroll the buffer down.  If there is no
commit or stash at point, then prompt for a commit."
  (interactive)
  (mercit-diff-show-or-scroll #'scroll-down))

(defun mercit-diff-show-or-scroll (fn)
  (let (rev cmd buf win)
    (cond
     ((and (bound-and-true-p mercit-blame-mode)
           (fboundp 'mercit-current-blame-chunk))
      (setq rev (oref (mercit-current-blame-chunk) orig-rev))
      (setq cmd #'mercit-show-commit)
      (setq buf (mercit-get-mode-buffer 'mercit-revision-mode)))
     ((derived-mode-p 'git-rebase-mode)
      (with-slots (action-type target)
          (git-rebase-current-line)
        (if (not (eq action-type 'commit))
            (user-error "No commit on this line")
          (setq rev target)
          (setq cmd #'mercit-show-commit)
          (setq buf (mercit-get-mode-buffer 'mercit-revision-mode)))))
     (t
      (mercit-section-case
        (branch
         (setq rev (mercit-ref-maybe-qualify (oref it value)))
         (setq cmd #'mercit-show-commit)
         (setq buf (mercit-get-mode-buffer 'mercit-revision-mode)))
        (commit
         (setq rev (oref it value))
         (setq cmd #'mercit-show-commit)
         (setq buf (mercit-get-mode-buffer 'mercit-revision-mode)))
        (stash
         (setq rev (oref it value))
         (setq cmd #'mercit-stash-show)
         (setq buf (mercit-get-mode-buffer 'mercit-stash-mode))))))
    (if rev
        (if (and buf
                 (setq win (get-buffer-window buf))
                 (with-current-buffer buf
                   (and (equal rev mercit-buffer-revision)
                        (equal (mercit-rev-parse rev)
                               mercit-buffer-revision-hash))))
            (with-selected-window win
              (condition-case nil
                  (funcall fn)
                (error
                 (goto-char (pcase fn
                              ('scroll-up   (point-min))
                              ('scroll-down (point-max)))))))
          (let ((mercit-display-buffer-noselect t))
            (if (eq cmd #'mercit-show-commit)
                (apply #'mercit-show-commit rev (mercit-show-commit--arguments))
              (funcall cmd rev))))
      (call-interactively #'mercit-show-commit))))

;;;; Section Commands

(defun mercit-section-cycle-diffs ()
  "Cycle visibility of diff-related sections in the current buffer."
  (interactive)
  (when-let ((sections
              (cond ((derived-mode-p 'mercit-status-mode)
                     (--mapcat
                      (when it
                        (when (oref it hidden)
                          (mercit-section-show it))
                        (oref it children))
                      (list (mercit-get-section '((staged)   (status)))
                            (mercit-get-section '((unstaged) (status))))))
                    ((derived-mode-p 'mercit-diff-mode)
                     (-filter #'mercit-file-section-p
                              (oref mercit-root-section children))))))
    (if (--any-p (oref it hidden) sections)
        (dolist (s sections)
          (mercit-section-show s)
          (mercit-section-hide-children s))
      (let ((children (--mapcat (oref it children) sections)))
        (cond ((and (--any-p (oref it hidden)   children)
                    (--any-p (oref it children) children))
               (mapc #'mercit-section-show-headings sections))
              ((seq-some #'mercit-section-hidden-body children)
               (mapc #'mercit-section-show-children sections))
              (t
               (mapc #'mercit-section-hide sections)))))))

;;; Diff Mode

(defvar mercit-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mercit-mode-map)
    (define-key map (kbd "C-c C-d") #'mercit-diff-while-committing)
    (define-key map (kbd "C-c C-b") #'mercit-go-backward)
    (define-key map (kbd "C-c C-f") #'mercit-go-forward)
    (define-key map (kbd "SPC")     #'scroll-up)
    (define-key map (kbd "DEL")     #'scroll-down)
    (define-key map (kbd "j")       #'mercit-jump-to-diffstat-or-diff)
    (define-key map [remap write-file] #'mercit-patch-save)
    map)
  "Keymap for `mercit-diff-mode'.")

(define-derived-mode mercit-diff-mode mercit-mode "Mercit Diff"
  "Mode for looking at a Mercurial diff.

This mode is documented in info node `(mercit)Diff Buffer'.

\\<mercit-mode-map>\
Type \\[mercit-refresh] to refresh the current buffer.
Type \\[mercit-section-toggle] to expand or hide the section at point.
Type \\[mercit-visit-thing] to visit the hunk or file at point.

Staging and applying changes is documented in info node
`(mercit)Staging and Unstaging' and info node `(mercit)Applying'.

\\<mercit-hunk-section-map>Type \
\\[mercit-apply] to apply the change at point, \
\\[mercit-stage] to stage,
\\[mercit-unstage] to unstage, \
\\[mercit-discard] to discard, or \
\\[mercit-reverse] to reverse it.

\\{mercit-diff-mode-map}"
  :group 'mercit-diff
  (hack-dir-local-variables-non-file-buffer)
  (setq mercit--imenu-item-types 'file))

(put 'mercit-diff-mode 'mercit-diff-default-arguments
     '("--stat" "--no-ext-diff"))

(defun mercit-diff-setup-buffer (range typearg args files &optional locked)
  (require 'mercit)
  (mercit-setup-buffer #'mercit-diff-mode locked
    (mercit-buffer-range range)
    (mercit-buffer-typearg typearg)
    (mercit-buffer-diff-args args)
    (mercit-buffer-diff-files files)
    (mercit-buffer-diff-files-suspended nil)))

(defun mercit-diff-refresh-buffer ()
  "Refresh the current `mercit-diff-mode' buffer."
  (mercit-set-header-line-format
   (if (equal mercit-buffer-typearg "--no-index")
       (apply #'format "Differences between %s and %s" mercit-buffer-diff-files)
     (concat (if mercit-buffer-range
                 (if (string-match-p "\\(\\.\\.\\|\\^-\\)"
                                     mercit-buffer-range)
                     (format "Changes in %s" mercit-buffer-range)
                   (let ((msg "Changes from %s to %s")
                         (end (if (equal mercit-buffer-typearg "--cached")
                                  "index"
                                "working tree")))
                     (if (member "-R" mercit-buffer-diff-args)
                         (format msg end mercit-buffer-range)
                       (format msg mercit-buffer-range end))))
               (cond ((equal mercit-buffer-typearg "--cached")
                      "Staged changes")
                     ((and (mercit-repository-local-get 'this-commit-command)
                           (not (mercit-anything-staged-p)))
                      "Uncommitting changes")
                     (t "Unstaged changes")))
             (pcase (length mercit-buffer-diff-files)
               (0)
               (1 (concat " in file " (car mercit-buffer-diff-files)))
               (_ (concat " in files "
                          (mapconcat #'identity mercit-buffer-diff-files ", ")))))))
  (setq mercit-buffer-range-hashed
        (and mercit-buffer-range (mercit-hash-range mercit-buffer-range)))
  (mercit-insert-section (diffbuf)
    (mercit-run-section-hook 'mercit-diff-sections-hook)))

(cl-defmethod mercit-buffer-value (&context (major-mode mercit-diff-mode))
  (nconc (cond (mercit-buffer-range
                (delq nil (list mercit-buffer-range mercit-buffer-typearg)))
               ((equal mercit-buffer-typearg "--cached")
                (list 'staged))
               (t
                (list 'unstaged mercit-buffer-typearg)))
         (and mercit-buffer-diff-files (cons "--" mercit-buffer-diff-files))))

(cl-defmethod mercit-menu-common-value ((_section mercit-diff-section))
  (mercit-diff-scope))

(define-obsolete-variable-alias 'mercit-diff-section-base-map
  'mercit-diff-section-map "Mercit-Section 3.4.0")
(defvar mercit-diff-section-map
  (let ((map (make-sparse-keymap)))
    (mercit-menu-set map [mercit-cherry-apply]
      #'mercit-apply "Apply %x"
      '(:enable (not (memq (mercit-diff-type) '(unstaged staged)))))
    (mercit-menu-set map [mercit-stage-file]
      #'mercit-stage "Stage %x"
      '(:enable (eq (mercit-diff-type) 'unstaged)))
    (mercit-menu-set map [mercit-unstage-file]
      #'mercit-unstage "Unstage %x"
      '(:enable (eq (mercit-diff-type) 'staged)))
    (mercit-menu-set map [mercit-delete-thing]
      #'mercit-discard "Discard %x"
      '(:enable (not (memq (mercit-diff-type) '(committed undefined)))))
    (mercit-menu-set map [mercit-revert-no-commit]
      #'mercit-reverse "Reverse %x"
      '(:enable (not (memq (mercit-diff-type) '(untracked unstaged)))))
    (mercit-menu-set map [mercit-visit-thing]
      #'mercit-diff-visit-file "Visit file")
    (mercit-menu-set map [mercit-file-untrack]
      #'mercit-file-untrack "Untrack %x"
      '(:enable (memq (mercit-diff-scope) '(file files))))
    (mercit-menu-set map [mercit-file-rename]
      #'mercit-file-rename "Rename file"
      '(:enable (eq (mercit-diff-scope) 'file)))
    (define-key map (kbd "C-j")            #'mercit-diff-visit-worktree-file)
    (define-key map (kbd "C-<return>")     #'mercit-diff-visit-worktree-file)
    (define-key map (kbd "C-x 4 <return>") #'mercit-diff-visit-file-other-window)
    (define-key map (kbd "C-x 5 <return>") #'mercit-diff-visit-file-other-frame)
    (define-key map "&"             #'mercit-do-async-shell-command)
    (define-key map "C"             #'mercit-commit-add-log)
    (define-key map (kbd "C-x a")   #'mercit-add-change-log-entry)
    (define-key map (kbd "C-x 4 a") #'mercit-add-change-log-entry-other-window)
    (define-key map (kbd "C-c C-t") #'mercit-diff-trace-definition)
    (define-key map (kbd "C-c C-e") #'mercit-diff-edit-hunk-commit)
    map)
  "Keymap for diff sections.
The classes `mercit-file-section' and `mercit-hunk-section' derive
from the abstract `mercit-diff-section' class.  Accordingly this
keymap is the parent of their keymaps.")

(defvar mercit-file-section-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mercit-diff-section-base-map)
    map)
  "Keymap for `file' sections.")

(defvar mercit-hunk-section-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mercit-diff-section-base-map)
    (let ((m (make-sparse-keymap)))
      (define-key m (kbd "RET") #'mercit-smerge-keep-current)
      (define-key m (kbd "u")   #'mercit-smerge-keep-upper)
      (define-key m (kbd "b")   #'mercit-smerge-keep-base)
      (define-key m (kbd "l")   #'mercit-smerge-keep-lower)
      (define-key map smerge-command-prefix m))
    map)
  "Keymap for `hunk' sections.")

(defconst mercit-diff-conflict-headline-re
  (concat "^" (regexp-opt
               ;; Defined in merge-tree.c in this order.
               '("merged"
                 "added in remote"
                 "added in both"
                 "added in local"
                 "removed in both"
                 "changed in both"
                 "removed in local"
                 "removed in remote"))))

(defconst mercit-diff-headline-re
  (concat "^\\(@@@?\\|diff\\|Submodule\\|"
          "\\* Unmerged path\\|"
          (substring mercit-diff-conflict-headline-re 1)
          "\\)"))

(defconst mercit-diff-statline-re
  (concat "^ ?"
          "\\(.*\\)"     ; file
          "\\( +| +\\)"  ; separator
          "\\([0-9]+\\|Bin\\(?: +[0-9]+ -> [0-9]+ bytes\\)?$\\) ?"
          "\\(\\+*\\)"   ; add
          "\\(-*\\)$"))  ; del

(defvar mercit-diff--reset-non-color-moved
  (list
   "-c" "color.diff.context=normal"
   "-c" "color.diff.plain=normal" ; historical synonym for context
   "-c" "color.diff.meta=normal"
   "-c" "color.diff.frag=normal"
   "-c" "color.diff.func=normal"
   "-c" "color.diff.old=normal"
   "-c" "color.diff.new=normal"
   "-c" "color.diff.commit=normal"
   "-c" "color.diff.whitespace=normal"
   ;; "git-range-diff" does not support "--color-moved", so we don't
   ;; need to reset contextDimmed, oldDimmed, newDimmed, contextBold,
   ;; oldBold, and newBold.
   ))

(defun mercit-insert-diff ()
  "Insert the diff into this `mercit-diff-mode' buffer."
  (mercit--insert-diff
    "diff" mercit-buffer-range "-p" "--noprefix"
    (and (member "--stat" mercit-buffer-diff-args) "--numstat")  ;; TODO
    mercit-buffer-typearg
    mercit-buffer-diff-args "--"
    mercit-buffer-diff-files))

(defun mercit--insert-diff (&rest args)
  (declare (indent 0))
  (pcase-let ((`(,cmd . ,args)
               (flatten-tree args))
              (mercit-git-global-arguments
               (remove "--literal-pathspecs" mercit-git-global-arguments)))
    ;; ;; As of Git 2.19.0, we need to generate diffs with
    ;; ;; --ita-visible-in-index so that `mercit-stage' can work with
    ;; ;; intent-to-add files (see #4026).
    ;; (when (and (not (equal cmd "merge-tree"))
    ;;            (mercit-git-version>= "2.19.0"))
    ;;   (push "--ita-visible-in-index" args))
    (setq args (mercit-diff--maybe-add-stat-arguments args))
    (when (cl-member-if (lambda (arg) (string-prefix-p "--color-moved" arg)) args)
      (push "--config=color.mode=terminfo" args)
      (setq mercit-git-global-arguments
            (append mercit-diff--reset-non-color-moved
                    mercit-git-global-arguments)))
    (mercit-git-wash #'mercit-diff-wash-diffs cmd args)))

(defun mercit-diff--maybe-add-stat-arguments (args)
  (if (member "--stat" args)
      (append (if (functionp mercit-diff-extra-stat-arguments)
                  (funcall mercit-diff-extra-stat-arguments)
                mercit-diff-extra-stat-arguments)
              args)
    args))

(defun mercit-diff-use-window-width-as-stat-width ()
  "Use the `window-width' as the value of `--stat-width'."
  (and-let* ((window (get-buffer-window (current-buffer) 'visible)))
    (list (format "--stat-width=%d" (window-width window)))))

(defun mercit-diff-wash-diffs (args &optional limit)
  (run-hooks 'mercit-diff-wash-diffs-hook)
  (when (member "--show-signature" args)
    (mercit-diff-wash-signature mercit-buffer-revision-hash))
  (when (member "--stat" args)
    (mercit-diff-wash-diffstat))
  (when (re-search-forward mercit-diff-headline-re limit t)
    (goto-char (line-beginning-position))
    (mercit-wash-sequence (apply-partially #'mercit-diff-wash-diff args))
    (insert ?\n)))

(defun mercit-jump-to-diffstat-or-diff ()
  "Jump to the diffstat or diff.
When point is on a file inside the diffstat section, then jump
to the respective diff section, otherwise jump to the diffstat
section or a child thereof."
  (interactive)
  (--if-let (mercit-get-section
             (append (mercit-section-case
                       ([file diffstat] `((file . ,(oref it value))))
                       (file `((file . ,(oref it value)) (diffstat)))
                       (t '((diffstat))))
                     (mercit-section-ident mercit-root-section)))
      (mercit-section-goto it)
    (user-error "No diffstat in this buffer")))

(defun mercit-diff-wash-signature (object)
  (when (looking-at "^gpg: ")
    (let (title end)
      (save-excursion
        (while (looking-at "^gpg: ")
          (cond
           ((looking-at "^gpg: Good signature from")
            (setq title (propertize
                         (buffer-substring (point) (line-end-position))
                         'face 'mercit-signature-good)))
           ((looking-at "^gpg: Can't check signature")
            (setq title (propertize
                         (buffer-substring (point) (line-end-position))
                         'face '(italic bold)))))
          (forward-line))
        (setq end (point-marker)))
      (mercit-insert-section (signature object title)
        (when title
          (mercit-insert-heading title))
        (goto-char end)
        (set-marker end nil)
        (insert "\n")))))

(defun mercit-diff-wash-diffstat ()
  (let (heading (beg (point)))
    (when (re-search-forward "^ ?\\([0-9]+ +files? change[^\n]*\n\\)" nil t)
      (setq heading (match-string 1))
      (mercit-delete-match)
      (goto-char beg)
      (mercit-insert-section (diffstat)
        (insert (propertize heading 'font-lock-face 'mercit-diff-file-heading))
        (mercit-insert-heading)
        (let (files)
          (while (looking-at "^[-0-9]+\t[-0-9]+\t\\(.+\\)$")
            (push (mercit-decode-git-path
                   (let ((f (match-string 1)))
                     (cond
                      ((string-match "{.+ => \\(.+\\)}" f)
                       (replace-match (match-string 1 f) nil t f))
                      ((string-match " => " f)
                       (substring f (match-end 0)))
                      (t f))))
                  files)
            (mercit-delete-line))
          (setq files (nreverse files))
          (while (looking-at mercit-diff-statline-re)
            (mercit-bind-match-strings (file sep cnt add del) nil
              (mercit-delete-line)
              (when (string-match " +$" file)
                (setq sep (concat (match-string 0 file) sep))
                (setq file (substring file 0 (match-beginning 0))))
              (let ((le (length file)) ld)
                (setq file (mercit-decode-git-path file))
                (setq ld (length file))
                (when (> le ld)
                  (setq sep (concat (make-string (- le ld) ?\s) sep))))
              (mercit-insert-section (file (pop files))
                (insert (propertize file 'font-lock-face 'mercit-filename)
                        sep cnt " ")
                (when add
                  (insert (propertize add 'font-lock-face
                                      'mercit-diffstat-added)))
                (when del
                  (insert (propertize del 'font-lock-face
                                      'mercit-diffstat-removed)))
                (insert "\n")))))
        (if (looking-at "^$") (forward-line) (insert "\n"))))))

(defun mercit-diff-wash-diff (args)
  (when (cl-member-if (lambda (arg) (string-prefix-p "--color-moved" arg)) args)
    (require 'ansi-color)
    (ansi-color-apply-on-region (point-min) (point-max)))
  (cond
   ((looking-at "^Submodule")
    (mercit-diff-wash-submodule))
   ((looking-at "^\\* Unmerged path \\(.*\\)")
    (let ((file (mercit-decode-git-path (match-string 1))))
      (mercit-delete-line)
      (unless (and (derived-mode-p 'mercit-status-mode)
                   (not (member "--cached" args)))
        (mercit-insert-section (file file)
          (insert (propertize
                   (format "unmerged   %s%s" file
                           (pcase (cddr (car (mercit-file-status file)))
                             ('(?D ?D) " (both deleted)")
                             ('(?D ?U) " (deleted by us)")
                             ('(?U ?D) " (deleted by them)")
                             ('(?A ?A) " (both added)")
                             ('(?A ?U) " (added by us)")
                             ('(?U ?A) " (added by them)")
                             ('(?U ?U) "")))
                   'font-lock-face 'mercit-diff-file-heading))
          (insert ?\n))))
    t)
   ((looking-at mercit-diff-conflict-headline-re)
    (let ((long-status (match-string 0))
          (status "BUG")
          file orig base)
      (if (equal long-status "merged")
          (progn (setq status long-status)
                 (setq long-status nil))
        (setq status (pcase-exhaustive long-status
                       ("added in remote"   "new file")
                       ("added in both"     "new file")
                       ("added in local"    "new file")
                       ("removed in both"   "removed")
                       ("changed in both"   "changed")
                       ("removed in local"  "removed")
                       ("removed in remote" "removed"))))
      (mercit-delete-line)
      (while (looking-at
              "^  \\([^ ]+\\) +[0-9]\\{6\\} \\([a-z0-9]\\{40,\\}\\) \\(.+\\)$")
        (mercit-bind-match-strings (side _blob name) nil
          (pcase side
            ("result" (setq file name))
            ("our"    (setq orig name))
            ("their"  (setq file name))
            ("base"   (setq base name))))
        (mercit-delete-line))
      (when orig (setq orig (mercit-decode-git-path orig)))
      (when file (setq file (mercit-decode-git-path file)))
      (mercit-diff-insert-file-section
       (or file base) orig status nil nil nil nil long-status)))
   ;; The files on this line may be ambiguous due to whitespace.
   ;; That's okay. We can get their names from subsequent headers.
   ((looking-at "^diff --\
\\(?:\\(?1:git\\) \\(?:\\(?2:.+?\\) \\2\\)?\
\\|\\(?:cc\\|combined\\) \\(?3:.+\\)\\)")
    (let ((status (cond ((equal (match-string 1) "git")        "modified")
                        ((derived-mode-p 'mercit-revision-mode) "resolved")
                        (t                                     "unmerged")))
          (orig nil)
          (file (or (match-string 2) (match-string 3)))
          (header (list (buffer-substring-no-properties
                         (line-beginning-position) (1+ (line-end-position)))))
          (modes nil)
          (rename nil)
          (binary nil))
      (mercit-delete-line)
      (while (not (or (eobp) (looking-at mercit-diff-headline-re)))
        (cond
         ((looking-at "old mode \\(?:[^\n]+\\)\nnew mode \\(?:[^\n]+\\)\n")
          (setq modes (match-string 0)))
         ((looking-at "deleted file .+\n")
          (setq status "deleted"))
         ((looking-at "new file .+\n")
          (setq status "new file"))
         ((looking-at "rename from \\(.+\\)\nrename to \\(.+\\)\n")
          (setq rename (match-string 0))
          (setq orig (match-string 1))
          (setq file (match-string 2))
          (setq status "renamed"))
         ((looking-at "copy from \\(.+\\)\ncopy to \\(.+\\)\n")
          (setq orig (match-string 1))
          (setq file (match-string 2))
          (setq status "new file"))
         ((looking-at "similarity index .+\n"))
         ((looking-at "dissimilarity index .+\n"))
         ((looking-at "index .+\n"))
         ((looking-at "--- \\(.+?\\)\t?\n")
          (unless (equal (match-string 1) "/dev/null")
            (setq orig (match-string 1))))
         ((looking-at "\\+\\+\\+ \\(.+?\\)\t?\n")
          (unless (equal (match-string 1) "/dev/null")
            (setq file (match-string 1))))
         ((looking-at "Binary files .+ and .+ differ\n")
          (setq binary t))
         ((looking-at "Binary files differ\n")
          (setq binary t))
         ;; TODO Use all combined diff extended headers.
         ((looking-at "mode .+\n"))
         ((error "BUG: Unknown extended header: %S"
                 (buffer-substring (point) (line-end-position)))))
        ;; These headers are treated as some sort of special hunk.
        (unless (or (string-prefix-p "old mode" (match-string 0))
                    (string-prefix-p "rename"   (match-string 0)))
          (push (match-string 0) header))
        (mercit-delete-match))
      (when orig
        (setq orig (mercit-decode-git-path orig)))
      (setq file (mercit-decode-git-path file))
      (setq header (nreverse header))
      ;; KLUDGE `git-log' ignores `--no-prefix' when `-L' is used.
      (when (and (derived-mode-p 'mercit-log-mode)
                 (seq-some (lambda (arg) (string-prefix-p "-L" arg))
                           mercit-buffer-log-args))
        (when orig
          (setq orig (substring orig 2)))
        (setq file (substring file 2))
        (setq header (list (save-excursion
                             (string-match "diff [^ ]+" (car header))
                             (format "%s %s %s\n"
                                     (match-string 0 (car header))
                                     (or orig file)
                                     (or file orig)))
                           (format "--- %s\n" (or orig "/dev/null"))
                           (format "+++ %s\n" (or file "/dev/null")))))
      (setq header (mapconcat #'identity header ""))
      (mercit-diff-insert-file-section
       file orig status modes rename header binary nil)))))

(defun mercit-diff-insert-file-section
    (file orig status modes rename header binary long-status)
  (mercit-insert-section section
    (file file (or (equal status "deleted")
                   (derived-mode-p 'mercit-status-mode)))
    (insert (propertize (format "%-10s %s" status
                                (if (or (not orig) (equal orig file))
                                    file
                                  (format "%s -> %s" orig file)))
                        'font-lock-face 'mercit-diff-file-heading))
    (cond ((and binary long-status)
           (insert (format " (%s, binary)" long-status)))
          ((or binary long-status)
           (insert (format " (%s)" (if binary "binary" long-status)))))
    (mercit-insert-heading)
    (unless (equal orig file)
      (oset section source orig))
    (oset section header header)
    (when modes
      (mercit-insert-section (hunk '(chmod))
        (insert modes)
        (mercit-insert-heading)))
    (when rename
      (mercit-insert-section (hunk '(rename))
        (insert rename)
        (mercit-insert-heading)))
    (mercit-wash-sequence #'mercit-diff-wash-hunk)))

(defun mercit-diff-wash-submodule ()
  ;; See `show_submodule_summary' in submodule.c and "this" commit.
  (when (looking-at "^Submodule \\([^ ]+\\)")
    (let ((module (match-string 1))
          untracked modified)
      (when (looking-at "^Submodule [^ ]+ contains untracked content$")
        (mercit-delete-line)
        (setq untracked t))
      (when (looking-at "^Submodule [^ ]+ contains modified content$")
        (mercit-delete-line)
        (setq modified t))
      (cond
       ((and (looking-at "^Submodule \\([^ ]+\\) \\([^ :]+\\)\\( (rewind)\\)?:$")
             (equal (match-string 1) module))
        (mercit-bind-match-strings (_module range rewind) nil
          (mercit-delete-line)
          (while (looking-at "^  \\([<>]\\) \\(.*\\)$")
            (mercit-delete-line))
          (when rewind
            (setq range (replace-regexp-in-string "[^.]\\(\\.\\.\\)[^.]"
                                                  "..." range t t 1)))
          (mercit-insert-section (mercit-module-section module t)
            (mercit-insert-heading
              (propertize (concat "modified   " module)
                          'font-lock-face 'mercit-diff-file-heading)
              " ("
              (cond (rewind "rewind")
                    ((string-search "..." range) "non-ff")
                    (t "new commits"))
              (and (or modified untracked)
                   (concat ", "
                           (and modified "modified")
                           (and modified untracked " and ")
                           (and untracked "untracked")
                           " content"))
              ")")
            (let ((default-directory
                   (file-name-as-directory
                    (expand-file-name module (mercit-toplevel)))))
              (mercit-git-wash (apply-partially #'mercit-log-wash-log 'module)
                "log" "--oneline" "--left-right" range)
              (delete-char -1)))))
       ((and (looking-at "^Submodule \\([^ ]+\\) \\([^ ]+\\) (\\([^)]+\\))$")
             (equal (match-string 1) module))
        (mercit-bind-match-strings (_module _range msg) nil
          (mercit-delete-line)
          (mercit-insert-section (mercit-module-section module)
            (mercit-insert-heading
              (propertize (concat "submodule  " module)
                          'font-lock-face 'mercit-diff-file-heading)
              " (" msg ")"))))
       (t
        (mercit-insert-section (mercit-module-section module)
          (mercit-insert-heading
            (propertize (concat "modified   " module)
                        'font-lock-face 'mercit-diff-file-heading)
            " ("
            (and modified "modified")
            (and modified untracked " and ")
            (and untracked "untracked")
            " content)")))))))

(defun mercit-diff-wash-hunk ()
  (when (looking-at "^@\\{2,\\} \\(.+?\\) @\\{2,\\}\\(?: \\(.*\\)\\)?")
    (let* ((heading  (match-string 0))
           (ranges   (mapcar
                      (lambda (str)
                        (let ((range
                               (mapcar #'string-to-number
                                       (split-string (substring str 1) ","))))
                          ;; A single line is +1 rather than +1,1.
                          (if (length= range 1)
                              (nconc range (list 1))
                            range)))
                      (split-string (match-string 1))))
           (about    (match-string 2))
           (combined (length= ranges 3))
           (value    (cons about ranges)))
      (mercit-delete-line)
      (mercit-insert-section section (hunk value)
        (insert (propertize (concat heading "\n")
                            'font-lock-face 'mercit-diff-hunk-heading))
        (mercit-insert-heading)
        (while (not (or (eobp) (looking-at "^[^-+\s\\]")))
          (forward-line))
        (oset section end (point))
        (oset section washer #'mercit-diff-paint-hunk)
        (oset section combined combined)
        (if combined
            (oset section from-ranges (butlast ranges))
          (oset section from-range (car ranges)))
        (oset section to-range (car (last ranges)))
        (oset section about about)))
    t))

(defun mercit-diff-expansion-threshold (section)
  "Keep new diff sections collapsed if washing takes too long."
  (and (mercit-file-section-p section)
       (> (float-time (time-subtract (current-time) mercit-refresh-start-time))
          mercit-diff-expansion-threshold)
       'hide))

(add-hook 'mercit-section-set-visibility-hook #'mercit-diff-expansion-threshold)

;;; Revision Mode

(define-derived-mode mercit-revision-mode mercit-diff-mode "Mercit Rev"
  "Mode for looking at a Mercurial commit.

This mode is documented in info node `(mercit)Revision Buffer'.

\\<mercit-mode-map>\
Type \\[mercit-refresh] to refresh the current buffer.
Type \\[mercit-section-toggle] to expand or hide the section at point.
Type \\[mercit-visit-thing] to visit the hunk or file at point.

Staging and applying changes is documented in info node
`(mercit)Staging and Unstaging' and info node `(mercit)Applying'.

\\<mercit-hunk-section-map>Type \
\\[mercit-apply] to apply the change at point, \
\\[mercit-stage] to stage,
\\[mercit-unstage] to unstage, \
\\[mercit-discard] to discard, or \
\\[mercit-reverse] to reverse it.

\\{mercit-revision-mode-map}"
  :group 'mercit-revision
  (hack-dir-local-variables-non-file-buffer))

(put 'mercit-revision-mode 'mercit-diff-default-arguments
     '("--stat" "--no-ext-diff"))

(defun mercit-revision-setup-buffer (rev args files)
  (mercit-setup-buffer #'mercit-revision-mode nil
    (mercit-buffer-revision rev)
    (mercit-buffer-range (format "%s^..%s" rev rev))
    (mercit-buffer-diff-args args)
    (mercit-buffer-diff-files files)
    (mercit-buffer-diff-files-suspended nil)))

(defun mercit-revision-refresh-buffer ()
  (setq mercit-buffer-revision-hash (mercit-rev-hash mercit-buffer-revision))
  (mercit-set-header-line-format
   (concat (mercit-object-type mercit-buffer-revision-hash)
           " "  mercit-buffer-revision
           (pcase (length mercit-buffer-diff-files)
             (0)
             (1 (concat " limited to file " (car mercit-buffer-diff-files)))
             (_ (concat " limited to files "
                        (mapconcat #'identity mercit-buffer-diff-files ", "))))))
  (mercit-insert-section (commitbuf)
    (mercit-run-section-hook 'mercit-revision-sections-hook)))

(cl-defmethod mercit-buffer-value (&context (major-mode mercit-revision-mode))
  (cons mercit-buffer-revision mercit-buffer-diff-files))

(defun mercit-insert-revision-diff ()  ;; TODO
  "Insert the diff into this `mercit-revision-mode' buffer."
  (mercit--insert-diff
    "show" "-p" "--cc" "--format=" "--noprefix"
    (and (member "--stat" mercit-buffer-diff-args) "--numstat") ;; TODO
    mercit-buffer-diff-args
    (mercit--rev-dereference mercit-buffer-revision)
    "--" mercit-buffer-diff-files))

(defun mercit-insert-revision-tag ()
  "Insert tag message and headers into a revision buffer.
This function only inserts anything when `mercit-show-commit' is
called with a tag as argument, when that is called with a commit
or a ref which is not a branch, then it inserts nothing."
  (when (equal (mercit-object-type mercit-buffer-revision) "tag")
    (mercit-insert-section (taginfo)
      (let ((beg (point)))
        ;; "git verify-tag -v" would output what we need, but the gpg
        ;; output is send to stderr and we have no control over the
        ;; order in which stdout and stderr are inserted, which would
        ;; make parsing hard.  We are forced to use "git cat-file tag"
        ;; instead, which inserts the signature instead of verifying
        ;; it.  We remove that later and then insert the verification
        ;; output using "git verify-tag" (without the "-v").
        (mercit-git-insert "cat-file" "tag" mercit-buffer-revision)
        (goto-char beg)
        (forward-line 3)
        (delete-region beg (point)))
      (looking-at "^tagger \\([^<]+\\) <\\([^>]+\\)")
      (let ((heading (format "Tagger: %s <%s>"
                             (match-string 1)
                             (match-string 2))))
        (mercit-delete-line)
        (insert (propertize heading 'font-lock-face
                            'mercit-section-secondary-heading)))
      (mercit-insert-heading)
      (forward-line)
      (mercit-insert-section section (message)
        (oset section heading-highlight-face
              'mercit-diff-revision-summary-highlight)
        (let ((beg (point)))
          (forward-line)
          (mercit--add-face-text-property
           beg (point) 'mercit-diff-revision-summary))
        (mercit-insert-heading)
        (if (re-search-forward "-----BEGIN PGP SIGNATURE-----" nil t)
            (goto-char (match-beginning 0))
          (goto-char (point-max)))
        (insert ?\n))
      (if (re-search-forward "-----BEGIN PGP SIGNATURE-----" nil t)
          (progn
            (let ((beg (match-beginning 0)))
              (re-search-forward "-----END PGP SIGNATURE-----\n")
              (delete-region beg (point)))
            (save-excursion
              (mercit-process-git t "verify-tag" mercit-buffer-revision))
            (mercit-diff-wash-signature mercit-buffer-revision))
        (goto-char (point-max)))
      (insert ?\n))))

(defvar mercit-commit-message-section-map
  (let ((map (make-sparse-keymap)))
    (mercit-menu-set map [mercit-visit-thing] #'mercit-show-commit "Visit %t"
      '(:enable (mercit-thing-at-point 'git-revision t)))
    map)
  "Keymap for `commit-message' sections.")

(defun mercit-insert-revision-message ()
  "Insert the commit message into a revision buffer."
  (mercit-insert-section section (commit-message)
    (oset section heading-highlight-face 'mercit-diff-revision-summary-highlight)
    (let ((beg (point))
          (rev mercit-buffer-revision))
      (insert (with-temp-buffer
                (mercit-rev-insert-format "{desc}" rev)
                (mercit-revision--wash-message)))
      (if (= (point) (+ beg 2))
          (progn (backward-delete-char 2)
                 (insert "(no message)\n"))
        (goto-char beg)
        (save-excursion
          (while (search-forward "\r\n" nil t) ; Remove trailing CRs.
            (delete-region (match-beginning 0) (1+ (match-beginning 0)))))
        (when mercit-revision-fill-summary-line
          (let ((fill-column (min mercit-revision-fill-summary-line
                                  (window-width))))
            (fill-region (point) (line-end-position))))
        (when mercit-revision-use-hash-sections
          (save-excursion
            ;; Start after beg to prevent a (commit text) section from
            ;; starting at the same point as the (commit-message)
            ;; section.
            (goto-char (1+ beg))
            (while (not (eobp))
              (re-search-forward "\\_<" nil 'move)
              (let ((beg (point)))
                (re-search-forward "\\_>" nil t)
                (when (> (point) beg)
                  (let ((text (buffer-substring-no-properties beg (point))))
                    (when (pcase mercit-revision-use-hash-sections
                            ('quickest ; false negatives and positives
                             (and (>= (length text) 7)
                                  (string-match-p "[0-9]" text)
                                  (string-match-p "[a-z]" text)))
                            ('quicker  ; false negatives (number-less hashes)
                             (and (>= (length text) 7)
                                  (string-match-p "[0-9]" text)
                                  (mercit-commit-p text)))
                            ('quick    ; false negatives (short hashes)
                             (and (>= (length text) 7)
                                  (mercit-commit-p text)))
                            ('slow
                             (mercit-commit-p text)))
                      (put-text-property beg (point)
                                         'font-lock-face 'mercit-hash)
                      (let ((end (point)))
                        (goto-char beg)
                        (mercit-insert-section (commit text)
                          (goto-char end))))))))))
        (save-excursion
          (forward-line)
          (mercit--add-face-text-property
           beg (point) 'mercit-diff-revision-summary)
          (mercit-insert-heading))
        (when mercit-diff-highlight-keywords
          (save-excursion
            (while (re-search-forward "\\[[^[]*\\]" nil t)
              (let ((beg (match-beginning 0))
                    (end (match-end 0)))
                (put-text-property
                 beg end 'font-lock-face
                 (if-let ((face (get-text-property beg 'font-lock-face)))
                     (list face 'mercit-keyword)
                   'mercit-keyword))))))
        (goto-char (point-max))))))

(defun mercit-insert-revision-notes ()
  "Insert commit notes into a revision buffer."
  (let* ((var "core.notesRef")
         (def (or (mercit-get var) "refs/notes/commits")))
    (dolist (ref (or (mercit-list-active-notes-refs)))
      (mercit-insert-section section (notes ref (not (equal ref def)))
        (oset section heading-highlight-face 'mercit-diff-hunk-heading-highlight)
        (let ((beg (point))
              (rev mercit-buffer-revision))
          (insert (with-temp-buffer
                    (mercit-git-insert "-c" (concat "core.notesRef=" ref)
                                      "notes" "show" rev)
                    (mercit-revision--wash-message)))
          (if (= (point) beg)
              (mercit-cancel-section)
            (goto-char beg)
            (end-of-line)
            (insert (format " (%s)"
                            (propertize (if (string-prefix-p "refs/notes/" ref)
                                            (substring ref 11)
                                          ref)
                                        'font-lock-face 'mercit-refname)))
            (forward-char)
            (mercit--add-face-text-property beg (point) 'mercit-diff-hunk-heading)
            (mercit-insert-heading)
            (goto-char (point-max))
            (insert ?\n)))))))

(defun mercit-revision--wash-message ()
  (let ((major-mode 'git-commit-mode))
    (hack-dir-local-variables)
    (hack-local-variables-apply))
  (unless (memq git-commit-major-mode '(nil text-mode))
    (funcall git-commit-major-mode)
    (font-lock-ensure))
  (buffer-string))

(defun mercit-insert-revision-headers ()
  "Insert headers about the commit into a revision buffer."
  (mercit-insert-section (headers)
    (--when-let (mercit-rev-format "%D" mercit-buffer-revision "--decorate=full")
      (insert (mercit-format-ref-labels it) ?\s))
    (insert (propertize
             (mercit-rev-parse (mercit--rev-dereference mercit-buffer-revision))
             'font-lock-face 'mercit-hash))
    (mercit-insert-heading)
    (let ((beg (point)))
      (mercit-rev-insert-format mercit-revision-headers-format
                               mercit-buffer-revision)
      (mercit-insert-revision-gravatars mercit-buffer-revision beg))
    (when mercit-revision-insert-related-refs
      (dolist (parent (mercit-commit-parents mercit-buffer-revision))
        (mercit-insert-section (commit parent)
          (let ((line (mercit-rev-format "{node|short} {desc|firstline}" parent)))
            (string-match "^\\([^ ]+\\) \\(.*\\)" line)
            (mercit-bind-match-strings (hash msg) line
              (insert "Parent:     ")
              (insert (propertize hash 'font-lock-face 'mercit-hash))
              (insert " " msg "\n")))))
      (mercit--insert-related-refs
       mercit-buffer-revision "--merged" "Merged"
       (eq mercit-revision-insert-related-refs 'all))
      (mercit--insert-related-refs
       mercit-buffer-revision "--contains" "Contained"
       (memq mercit-revision-insert-related-refs '(all mixed)))
      (when-let ((follows (mercit-get-current-tag mercit-buffer-revision t)))
        (let ((tag (car  follows))
              (cnt (cadr follows)))
          (mercit-insert-section (tag tag)
            (insert
             (format "Follows:    %s (%s)\n"
                     (propertize tag 'font-lock-face 'mercit-tag)
                     (propertize (number-to-string cnt)
                                 'font-lock-face 'mercit-branch-local))))))
      (when-let ((precedes (mercit-get-next-tag mercit-buffer-revision t)))
        (let ((tag (car  precedes))
              (cnt (cadr precedes)))
          (mercit-insert-section (tag tag)
            (insert (format "Precedes:   %s (%s)\n"
                            (propertize tag 'font-lock-face 'mercit-tag)
                            (propertize (number-to-string cnt)
                                        'font-lock-face 'mercit-tag))))))
      (insert ?\n))))

(defun mercit--insert-related-refs (rev arg title remote)
  (when-let ((refs (mercit-list-related-branches arg rev (and remote "-a"))))
    (insert title ":" (make-string (- 10 (length title)) ?\s))
    (dolist (branch refs)
      (if (<= (+ (current-column) 1 (length branch))
              (window-width))
          (insert ?\s)
        (insert ?\n (make-string 12 ?\s)))
      (mercit-insert-section (branch branch)
        (insert (propertize branch 'font-lock-face
                            (if (string-prefix-p "remotes/" branch)
                                'mercit-branch-remote
                              'mercit-branch-local)))))
    (insert ?\n)))

(defun mercit-insert-revision-gravatars (rev beg)
  (when (and mercit-revision-show-gravatars
             (window-system))
    (require 'gravatar)
    (pcase-let ((`(,author . ,committer)
                 (pcase mercit-revision-show-gravatars
                   ('t '("^Author:     " . "^Commit:     "))
                   ('author '("^Author:     " . nil))
                   ('committer '(nil . "^Commit:     "))
                   (_ mercit-revision-show-gravatars))))
      (--when-let (and author (mercit-rev-format "%aE" rev))
        (mercit-insert-revision-gravatar beg rev it author))
      (--when-let (and committer (mercit-rev-format "%cE" rev))
        (mercit-insert-revision-gravatar beg rev it committer)))))

(defun mercit-insert-revision-gravatar (beg rev email regexp)
  (save-excursion
    (goto-char beg)
    (when (re-search-forward regexp nil t)
      (when-let ((window (get-buffer-window)))
        (let* ((column   (length (match-string 0)))
               (font-obj (query-font (font-at (point) window)))
               (size     (* 2 (+ (aref font-obj 4)
                                 (aref font-obj 5))))
               (align-to (+ column
                            (ceiling (/ size (aref font-obj 7) 1.0))
                            1))
               (gravatar-size (- size 2)))
          (ignore-errors ; service may be unreachable
            (gravatar-retrieve email #'mercit-insert-revision-gravatar-cb
                               (list gravatar-size rev
                                     (point-marker)
                                     align-to column))))))))

(defun mercit-insert-revision-gravatar-cb (image size rev marker align-to column)
  (unless (eq image 'error)
    (when-let ((buffer (marker-buffer marker)))
      (with-current-buffer buffer
        (save-excursion
          (goto-char marker)
          ;; The buffer might display another revision by now or
          ;; it might have been refreshed, in which case another
          ;; process might already have inserted the image.
          (when (and (equal rev mercit-buffer-revision)
                     (not (eq (car-safe
                               (car-safe
                                (get-text-property (point) 'display)))
                              'image)))
            (setf (image-property image :ascent) 'center)
            (setf (image-property image :relief) 1)
            (setf (image-property image :scale)  1)
            (setf (image-property image :height) size)
            (let ((top (list image '(slice 0.0 0.0 1.0 0.5)))
                  (bot (list image '(slice 0.0 0.5 1.0 1.0)))
                  (align `((space :align-to ,align-to))))
              (when mercit-revision-use-gravatar-kludge
                (cl-rotatef top bot))
              (let ((inhibit-read-only t))
                (insert (propertize " " 'display top))
                (insert (propertize " " 'display align))
                (forward-line)
                (forward-char column)
                (insert (propertize " " 'display bot))
                (insert (propertize " " 'display align))))))))))

;;; Merge-Preview Mode

(define-derived-mode mercit-merge-preview-mode mercit-diff-mode "Mercit Merge"
  "Mode for previewing a merge."
  :group 'mercit-diff
  (hack-dir-local-variables-non-file-buffer))

(put 'mercit-merge-preview-mode 'mercit-diff-default-arguments
     '("--no-ext-diff"))

(defun mercit-merge-preview-setup-buffer (rev)
  (mercit-setup-buffer #'mercit-merge-preview-mode nil
    (mercit-buffer-revision rev)
    (mercit-buffer-range (format "%s^..%s" rev rev))))

(defun mercit-merge-preview-refresh-buffer ()
  (let* ((branch (mercit-get-current-branch))
         (head (or branch (mercit-rev-verify "HEAD"))))
    (mercit-set-header-line-format (format "Preview merge of %s into %s"
                                          mercit-buffer-revision
                                          (or branch "HEAD")))
    (mercit-insert-section (diffbuf)
      (mercit--insert-diff
        "merge-tree" (mercit-git-string "merge-base" head mercit-buffer-revision)
        head mercit-buffer-revision))))

(cl-defmethod mercit-buffer-value (&context (major-mode mercit-merge-preview-mode))
  mercit-buffer-revision)

;;; Hunk Section

(defun mercit-hunk-set-window-start (section)
  "When SECTION is a `hunk', ensure that its beginning is visible.
It the SECTION has a different type, then do nothing."
  (when (mercit-hunk-section-p section)
    (mercit-section-set-window-start section)))

(add-hook 'mercit-section-movement-hook #'mercit-hunk-set-window-start)

(cl-defmethod mercit-section-get-relative-position ((_section mercit-hunk-section))
  (nconc (cl-call-next-method)
         (and (region-active-p)
              (progn
                (goto-char (line-beginning-position))
                (when  (looking-at "^[-+]") (forward-line))
                (while (looking-at "^[ @]") (forward-line))
                (let ((beg (mercit-point)))
                  (list (cond
                         ((looking-at "^[-+]")
                          (forward-line)
                          (while (looking-at "^[-+]") (forward-line))
                          (while (looking-at "^ ")    (forward-line))
                          (forward-line -1)
                          (regexp-quote (buffer-substring-no-properties
                                         beg (line-end-position))))
                         (t t))))))))

(cl-defmethod mercit-section-goto-successor ((section mercit-hunk-section)
                                            line char &optional arg)
  (or (mercit-section-goto-successor--same section line char)
      (and-let* ((parent (mercit-get-section
                          (mercit-section-ident
                           (oref section parent)))))
        (let* ((children (oref parent children))
               (siblings (mercit-section-siblings section 'prev))
               (previous (nth (length siblings) children)))
          (if (not arg)
              (when-let ((sibling (or previous (car (last children)))))
                (mercit-section-goto sibling)
                t)
            (when previous
              (mercit-section-goto previous))
            (if (and (stringp arg)
                     (re-search-forward arg (oref parent end) t))
                (goto-char (match-beginning 0))
              (goto-char (oref (car (last children)) end))
              (forward-line -1)
              (while (looking-at "^ ")    (forward-line -1))
              (while (looking-at "^[-+]") (forward-line -1))
              (forward-line)))))
      (mercit-section-goto-successor--related section)))

;;; Diff Sections

(defvar mercit-unstaged-section-map
  (let ((map (make-sparse-keymap)))
    (mercit-menu-set map [mercit-visit-thing]  #'mercit-diff-unstaged "Visit diff")
    (mercit-menu-set map [mercit-stage-file]   #'mercit-stage         "Stage all")
    (mercit-menu-set map [mercit-delete-thing] #'mercit-discard       "Discard all")
    map)
  "Keymap for the `unstaged' section.")

(mercit-define-section-jumper mercit-jump-to-unstaged "Unstaged changes" unstaged)

(defun mercit-insert-unstaged-changes ()
  "Insert section showing unstaged changes."
  (mercit-insert-section (unstaged)
    (mercit-insert-heading "Unstaged changes:")
    (mercit--insert-diff
      "diff" mercit-buffer-diff-args "--noprefix"
      "--" mercit-buffer-diff-files)))

(defvar mercit-staged-section-map
  (let ((map (make-sparse-keymap)))
    (mercit-menu-set map [mercit-visit-thing]  #'mercit-diff-staged "Visit diff")
    (mercit-menu-set map [mercit-unstage-file]     #'mercit-unstage "Unstage all")
    (mercit-menu-set map [mercit-delete-thing]     #'mercit-discard "Discard all")
    (mercit-menu-set map [mercit-revert-no-commit] #'mercit-reverse "Reverse all")
    map)
  "Keymap for the `staged' section.")

(mercit-define-section-jumper mercit-jump-to-staged "Staged changes" staged)

(defun mercit-insert-staged-changes ()
  "Insert section showing staged changes."
  ;; Avoid listing all files as deleted when visiting a bare repo.
  (unless (mercit-bare-repo-p)
    (mercit-insert-section (staged)
      (mercit-insert-heading "Staged changes:")
      (mercit--insert-diff
        "diff" mercit-buffer-diff-args "--noprefix" ;; FIXME was "--cached"
        "--" mercit-buffer-diff-files))))

;;; Diff Type

(defun mercit-diff-type (&optional section)
  "Return the diff type of SECTION.

The returned type is one of the symbols `staged', `unstaged',
`committed', or `undefined'.  This type serves a similar purpose
as the general type common to all sections (which is stored in
the `type' slot of the corresponding `mercit-section' struct) but
takes additional information into account.  When the SECTION
isn't related to diffs and the buffer containing it also isn't
a diff-only buffer, then return nil.

Currently the type can also be one of `tracked' and `untracked'
but these values are not handled explicitly everywhere they
should be and a possible fix could be to just return nil here.

The section has to be a `diff' or `hunk' section, or a section
whose children are of type `diff'.  If optional SECTION is nil,
return the diff type for the current section.  In buffers whose
major mode is `mercit-diff-mode' SECTION is ignored and the type
is determined using other means.  In `mercit-revision-mode'
buffers the type is always `committed'.

Do not confuse this with `mercit-diff-scope' (which see)."
  (--when-let (or section (mercit-current-section))
    (cond ((derived-mode-p 'mercit-revision-mode 'mercit-stash-mode) 'committed)
          ((derived-mode-p 'mercit-diff-mode)
           (let ((range mercit-buffer-range)
                 (const mercit-buffer-typearg))
             (cond ((equal const "--no-index") 'undefined)
                   ((or (not range)
                        (mercit-rev-eq range "HEAD"))
                    (if (equal const "--cached")
                        'staged
                      'unstaged))
                   ((equal const "--cached")
                    (if (mercit-rev-head-p range)
                        'staged
                      'undefined)) ; i.e. committed and staged
                   (t 'committed))))
          ((derived-mode-p 'mercit-status-mode)
           (let ((stype (oref it type)))
             (if (memq stype '(staged unstaged tracked untracked))
                 stype
               (pcase stype
                 ((or 'file 'module)
                  (let* ((parent (oref it parent))
                         (type   (oref parent type)))
                    (if (memq type '(file module))
                        (mercit-diff-type parent)
                      type)))
                 ('hunk (thread-first it
                          (oref parent)
                          (oref parent)
                          (oref type)))))))
          ((derived-mode-p 'mercit-log-mode)
           (if (or (and (mercit-section-match 'commit section)
                        (oref section children))
                   (mercit-section-match [* file commit] section))
               'committed
             'undefined))
          (t 'undefined))))

(cl-defun mercit-diff-scope (&optional (section nil ssection) strict)
  "Return the diff scope of SECTION or the selected section(s).

A diff's \"scope\" describes what part of a diff is selected, it is
a symbol, one of `region', `hunk', `hunks', `file', `files', or
`list'.  Do not confuse this with the diff \"type\", as returned by
`mercit-diff-type'.

If optional SECTION is non-nil, then return the scope of that,
ignoring the sections selected by the region.  Otherwise return
the scope of the current section, or if the region is active and
selects a valid group of diff related sections, the type of these
sections, i.e. `hunks' or `files'.  If SECTION, or if that is nil
the current section, is a `hunk' section; and the region region
starts and ends inside the body of a that section, then the type
is `region'.  If the region is empty after a mouse click, then
`hunk' is returned instead of `region'.

If optional STRICT is non-nil, then return nil if the diff type of
the section at point is `untracked' or the section at point is not
actually a `diff' but a `diffstat' section."
  (let ((siblings (and (not ssection) (mercit-region-sections nil t))))
    (setq section (or section (car siblings) (mercit-current-section)))
    (when (and section
               (or (not strict)
                   (and (not (eq (mercit-diff-type section) 'untracked))
                        (not (eq (--when-let (oref section parent)
                                   (oref it type))
                                 'diffstat)))))
      (pcase (list (oref section type)
                   (and siblings t)
                   (mercit-diff-use-hunk-region-p)
                   ssection)
        (`(hunk nil   t  ,_)
         (if (mercit-section-internal-region-p section) 'region 'hunk))
        ('(hunk   t   t nil) 'hunks)
        (`(hunk  ,_  ,_  ,_) 'hunk)
        ('(file   t   t nil) 'files)
        (`(file  ,_  ,_  ,_) 'file)
        ('(module   t   t nil) 'files)
        (`(module  ,_  ,_  ,_) 'file)
        (`(,(or 'staged 'unstaged 'untracked) nil ,_ ,_) 'list)))))

(defun mercit-diff-use-hunk-region-p ()
  (and (region-active-p)
       ;; TODO implement this from first principals
       ;; currently it's trial-and-error
       (not (and (or (eq this-command #'mouse-drag-region)
                     (eq last-command #'mouse-drag-region)
                     ;; When another window was previously
                     ;; selected then the last-command is
                     ;; some byte-code function.
                     (byte-code-function-p last-command))
                 (eq (region-end) (region-beginning))))))

;;; Diff Highlight

(add-hook 'mercit-section-unhighlight-hook #'mercit-diff-unhighlight)
(add-hook 'mercit-section-highlight-hook #'mercit-diff-highlight)

(defun mercit-diff-unhighlight (section selection)
  "Remove the highlighting of the diff-related SECTION."
  (when (mercit-hunk-section-p section)
    (mercit-diff-paint-hunk section selection nil)
    t))

(defun mercit-diff-highlight (section selection)
  "Highlight the diff-related SECTION.
If SECTION is not a diff-related section, then do nothing and
return nil.  If SELECTION is non-nil, then it is a list of sections
selected by the region, including SECTION.  All of these sections
are highlighted."
  (if (and (mercit-section-match 'commit section)
           (oref section children))
      (progn (if selection
                 (dolist (section selection)
                   (mercit-diff-highlight-list section selection))
               (mercit-diff-highlight-list section))
             t)
    (when-let ((scope (mercit-diff-scope section t)))
      (cond ((eq scope 'region)
             (mercit-diff-paint-hunk section selection t))
            (selection
             (dolist (section selection)
               (mercit-diff-highlight-recursive section selection)))
            (t
             (mercit-diff-highlight-recursive section)))
      t)))

(defun mercit-diff-highlight-recursive (section &optional selection)
  (pcase (mercit-diff-scope section)
    ('list (mercit-diff-highlight-list section selection))
    ('file (mercit-diff-highlight-file section selection))
    ('hunk (mercit-diff-highlight-heading section selection)
           (mercit-diff-paint-hunk section selection t))
    (_     (mercit-section-highlight section nil))))

(defun mercit-diff-highlight-list (section &optional selection)
  (let ((beg (oref section start))
        (cnt (oref section content))
        (end (oref section end)))
    (when (or (eq this-command #'mouse-drag-region)
              (not selection))
      (unless (and (region-active-p)
                   (<= (region-beginning) beg))
        (mercit-section-make-overlay beg cnt 'mercit-section-highlight))
      (if (oref section hidden)
          (oset section washer #'ignore)
        (dolist (child (oref section children))
          (when (or (eq this-command #'mouse-drag-region)
                    (not (and (region-active-p)
                              (<= (region-beginning)
                                  (oref child start)))))
            (mercit-diff-highlight-recursive child selection)))))
    (when mercit-diff-highlight-hunk-body
      (mercit-section-make-overlay (1- end) end 'mercit-section-highlight))))

(defun mercit-diff-highlight-file (section &optional selection)
  (mercit-diff-highlight-heading section selection)
  (when (or (not (oref section hidden))
            (cl-typep section 'mercit-module-section))
    (dolist (child (oref section children))
      (mercit-diff-highlight-recursive child selection))))

(defun mercit-diff-highlight-heading (section &optional selection)
  (mercit-section-make-overlay
   (oref section start)
   (or (oref section content)
       (oref section end))
   (pcase (list (oref section type)
                (and (member section selection)
                     (not (eq this-command #'mouse-drag-region))))
     ('(file     t) 'mercit-diff-file-heading-selection)
     ('(file   nil) 'mercit-diff-file-heading-highlight)
     ('(module   t) 'mercit-diff-file-heading-selection)
     ('(module nil) 'mercit-diff-file-heading-highlight)
     ('(hunk     t) 'mercit-diff-hunk-heading-selection)
     ('(hunk   nil) 'mercit-diff-hunk-heading-highlight))))

;;; Hunk Paint

(cl-defun mercit-diff-paint-hunk
    (section &optional selection
             (highlight (mercit-section-selected-p section selection)))
  (let (paint)
    (unless mercit-diff-highlight-hunk-body
      (setq highlight nil))
    (cond (highlight
           (unless (oref section hidden)
             (add-to-list 'mercit-section-highlighted-sections section)
             (cond ((memq section mercit-section-unhighlight-sections)
                    (setq mercit-section-unhighlight-sections
                          (delq section mercit-section-unhighlight-sections)))
                   (mercit-diff-highlight-hunk-body
                    (setq paint t)))))
          (t
           (cond ((and (oref section hidden)
                       (memq section mercit-section-unhighlight-sections))
                  (add-to-list 'mercit-section-highlighted-sections section)
                  (setq mercit-section-unhighlight-sections
                        (delq section mercit-section-unhighlight-sections)))
                 (t
                  (setq paint t)))))
    (when paint
      (save-excursion
        (goto-char (oref section start))
        (let ((end (oref section end))
              (merging (looking-at "@@@"))
              (diff-type (mercit-diff-type))
              (stage nil)
              (tab-width (mercit-diff-tab-width
                          (mercit-section-parent-value section))))
          (forward-line)
          (while (< (point) end)
            (when (and mercit-diff-hide-trailing-cr-characters
                       (char-equal ?\r (char-before (line-end-position))))
              (put-text-property (1- (line-end-position)) (line-end-position)
                                 'invisible t))
            (put-text-property
             (point) (1+ (line-end-position)) 'font-lock-face
             (cond
              ((looking-at "^\\+\\+?\\([<=|>]\\)\\{7\\}")
               (setq stage (pcase (list (match-string 1) highlight)
                             ('("<" nil) 'mercit-diff-our)
                             ('("<"   t) 'mercit-diff-our-highlight)
                             ('("|" nil) 'mercit-diff-base)
                             ('("|"   t) 'mercit-diff-base-highlight)
                             ('("=" nil) 'mercit-diff-their)
                             ('("="   t) 'mercit-diff-their-highlight)
                             ('(">" nil) nil)))
               'mercit-diff-conflict-heading)
              ((looking-at (if merging "^\\(\\+\\| \\+\\)" "^\\+"))
               (mercit-diff-paint-tab merging tab-width)
               (mercit-diff-paint-whitespace merging 'added diff-type)
               (or stage
                   (if highlight 'mercit-diff-added-highlight 'mercit-diff-added)))
              ((looking-at (if merging "^\\(-\\| -\\)" "^-"))
               (mercit-diff-paint-tab merging tab-width)
               (mercit-diff-paint-whitespace merging 'removed diff-type)
               (if highlight 'mercit-diff-removed-highlight 'mercit-diff-removed))
              (t
               (mercit-diff-paint-tab merging tab-width)
               (mercit-diff-paint-whitespace merging 'context diff-type)
               (if highlight 'mercit-diff-context-highlight 'mercit-diff-context))))
            (forward-line))))))
  (mercit-diff-update-hunk-refinement section))

(defvar mercit-diff--tab-width-cache nil)

(defun mercit-diff-tab-width (file)
  (setq file (expand-file-name file))
  (cl-flet ((cache (value)
              (let ((elt (assoc file mercit-diff--tab-width-cache)))
                (if elt
                    (setcdr elt value)
                  (setq mercit-diff--tab-width-cache
                        (cons (cons file value)
                              mercit-diff--tab-width-cache))))
              value))
    (cond
     ((not mercit-diff-adjust-tab-width)
      tab-width)
     ((and-let* ((buffer (find-buffer-visiting file)))
        (cache (buffer-local-value 'tab-width buffer))))
     ((and-let* ((elt (assoc file mercit-diff--tab-width-cache)))
        (or (cdr elt)
            tab-width)))
     ((or (eq mercit-diff-adjust-tab-width 'always)
          (and (numberp mercit-diff-adjust-tab-width)
               (>= mercit-diff-adjust-tab-width
                   (nth 7 (file-attributes file)))))
      (cache (buffer-local-value 'tab-width (find-file-noselect file))))
     (t
      (cache nil)
      tab-width))))

(defun mercit-diff-paint-tab (merging width)
  (save-excursion
    (forward-char (if merging 2 1))
    (while (= (char-after) ?\t)
      (put-text-property (point) (1+ (point))
                         'display (list (list 'space :width width)))
      (forward-char))))

(defun mercit-diff-paint-whitespace (merging line-type diff-type)
  (when (and mercit-diff-paint-whitespace
             (or (not (memq mercit-diff-paint-whitespace '(uncommitted status)))
                 (memq diff-type '(staged unstaged)))
             (cl-case line-type
               (added   t)
               (removed (memq mercit-diff-paint-whitespace-lines '(all both)))
               (context (memq mercit-diff-paint-whitespace-lines '(all)))))
    (let ((prefix (if merging "^[-\\+\s]\\{2\\}" "^[-\\+\s]"))
          (indent
           (if (local-variable-p 'mercit-diff-highlight-indentation)
               mercit-diff-highlight-indentation
             (setq-local
              mercit-diff-highlight-indentation
              (cdr (--first (string-match-p (car it) default-directory)
                            (nreverse
                             (default-value
                              'mercit-diff-highlight-indentation))))))))
      (when (and mercit-diff-highlight-trailing
                 (looking-at (concat prefix ".*?\\([ \t]+\\)?$")))
        (let ((ov (make-overlay (match-beginning 1) (match-end 1) nil t)))
          (overlay-put ov 'font-lock-face 'mercit-diff-whitespace-warning)
          (overlay-put ov 'priority 2)
          (overlay-put ov 'evaporate t)))
      (when (or (and (eq indent 'tabs)
                     (looking-at (concat prefix "\\( *\t[ \t]*\\)")))
                (and (integerp indent)
                     (looking-at (format "%s\\([ \t]* \\{%s,\\}[ \t]*\\)"
                                         prefix indent))))
        (let ((ov (make-overlay (match-beginning 1) (match-end 1) nil t)))
          (overlay-put ov 'font-lock-face 'mercit-diff-whitespace-warning)
          (overlay-put ov 'priority 2)
          (overlay-put ov 'evaporate t))))))

(defun mercit-diff-update-hunk-refinement (&optional section)
  (if section
      (unless (oref section hidden)
        (pcase (list mercit-diff-refine-hunk
                     (oref section refined)
                     (eq section (mercit-current-section)))
          ((or `(all nil ,_) '(t nil t))
           (oset section refined t)
           (save-excursion
             (goto-char (oref section start))
             ;; `diff-refine-hunk' does not handle combined diffs.
             (unless (looking-at "@@@")
               (let ((smerge-refine-ignore-whitespace
                      mercit-diff-refine-ignore-whitespace)
                     ;; Avoid fsyncing many small temp files
                     (write-region-inhibit-fsync t))
                 (diff-refine-hunk)))))
          ((or `(nil t ,_) '(t t nil))
           (oset section refined nil)
           (remove-overlays (oref section start)
                            (oref section end)
                            'diff-mode 'fine))))
    (cl-labels ((recurse (section)
                  (if (mercit-section-match 'hunk section)
                      (mercit-diff-update-hunk-refinement section)
                    (dolist (child (oref section children))
                      (recurse child)))))
      (recurse mercit-root-section))))


;;; Hunk Region

(defun mercit-diff-hunk-region-beginning ()
  (save-excursion (goto-char (region-beginning))
                  (line-beginning-position)))

(defun mercit-diff-hunk-region-end ()
  (save-excursion (goto-char (region-end))
                  (line-end-position)))

(defun mercit-diff-update-hunk-region (section)
  "Highlight the hunk-internal region if any."
  (when (and (eq (oref section type) 'hunk)
             (eq (mercit-diff-scope section t) 'region))
    (mercit-diff--make-hunk-overlay
     (oref section start)
     (1- (oref section content))
     'font-lock-face 'mercit-diff-lines-heading
     'display (mercit-diff-hunk-region-header section)
     'after-string (mercit-diff--hunk-after-string 'mercit-diff-lines-heading))
    (run-hook-with-args 'mercit-diff-highlight-hunk-region-functions section)
    t))

(defun mercit-diff-highlight-hunk-region-dim-outside (section)
  "Dim the parts of the hunk that are outside the hunk-internal region.
This is done by using the same foreground and background color
for added and removed lines as for context lines."
  (let ((face (if mercit-diff-highlight-hunk-body
                  'mercit-diff-context-highlight
                'mercit-diff-context)))
    (when mercit-diff-unmarked-lines-keep-foreground
      (setq face `(,@(and (>= emacs-major-version 27) '(:extend t))
                   :background ,(face-attribute face :background))))
    (mercit-diff--make-hunk-overlay (oref section content)
                                   (mercit-diff-hunk-region-beginning)
                                   'font-lock-face face
                                   'priority 2)
    (mercit-diff--make-hunk-overlay (1+ (mercit-diff-hunk-region-end))
                                   (oref section end)
                                   'font-lock-face face
                                   'priority 2)))

(defun mercit-diff-highlight-hunk-region-using-face (_section)
  "Highlight the hunk-internal region by making it bold.
Or rather highlight using the face `mercit-diff-hunk-region', though
changing only the `:weight' and/or `:slant' is recommended for that
face."
  (mercit-diff--make-hunk-overlay (mercit-diff-hunk-region-beginning)
                                 (1+ (mercit-diff-hunk-region-end))
                                 'font-lock-face 'mercit-diff-hunk-region))

(defun mercit-diff-highlight-hunk-region-using-overlays (section)
  "Emphasize the hunk-internal region using delimiting horizontal lines.
This is implemented as single-pixel newlines places inside overlays."
  (if (window-system)
      (let ((beg (mercit-diff-hunk-region-beginning))
            (end (mercit-diff-hunk-region-end))
            (str (propertize
                  (concat (propertize "\s" 'display '(space :height (1)))
                          (propertize "\n" 'line-height t))
                  'font-lock-face 'mercit-diff-lines-boundary)))
        (mercit-diff--make-hunk-overlay beg (1+ beg) 'before-string str)
        (mercit-diff--make-hunk-overlay end (1+ end) 'after-string  str))
    (mercit-diff-highlight-hunk-region-using-face section)))

(defun mercit-diff-highlight-hunk-region-using-underline (section)
  "Emphasize the hunk-internal region using delimiting horizontal lines.
This is implemented by overlining and underlining the first and
last (visual) lines of the region."
  (if (window-system)
      (let* ((beg (mercit-diff-hunk-region-beginning))
             (end (mercit-diff-hunk-region-end))
             (beg-eol (save-excursion (goto-char beg)
                                      (end-of-visual-line)
                                      (point)))
             (end-bol (save-excursion (goto-char end)
                                      (beginning-of-visual-line)
                                      (point)))
             (color (face-background 'mercit-diff-lines-boundary nil t)))
        (cl-flet ((ln (b e &rest face)
                    (mercit-diff--make-hunk-overlay
                     b e 'font-lock-face face 'after-string
                     (mercit-diff--hunk-after-string face))))
          (if (= beg end-bol)
              (ln beg beg-eol :overline color :underline color)
            (ln beg beg-eol :overline color)
            (ln end-bol end :underline color))))
    (mercit-diff-highlight-hunk-region-using-face section)))

(defun mercit-diff--make-hunk-overlay (start end &rest args)
  (let ((ov (make-overlay start end nil t)))
    (overlay-put ov 'evaporate t)
    (while args (overlay-put ov (pop args) (pop args)))
    (push ov mercit-section--region-overlays)
    ov))

(defun mercit-diff--hunk-after-string (face)
  (propertize "\s"
              'font-lock-face face
              'display (list 'space :align-to
                             `(+ (0 . right)
                                 ,(min (window-hscroll)
                                       (- (line-end-position)
                                          (line-beginning-position)))))
              ;; This prevents the cursor from being rendered at the
              ;; edge of the window.
              'cursor t))

;;; Hunk Utilities

(defun mercit-diff-inside-hunk-body-p ()
  "Return non-nil if point is inside the body of a hunk."
  (and (mercit-section-match 'hunk)
       (and-let* ((content (oref (mercit-current-section) content)))
         (> (mercit-point) content))))

;;; Diff Extract

(defun mercit-diff-file-header (section &optional no-rename)
  (when (mercit-hunk-section-p section)
    (setq section (oref section parent)))
  (and (mercit-file-section-p section)
       (let ((header (oref section header)))
         (if no-rename
             (replace-regexp-in-string
              "^--- \\(.+\\)" (oref section value) header t t 1)
           header))))

(defun mercit-diff-hunk-region-header (section)
  (let ((patch (mercit-diff-hunk-region-patch section)))
    (string-match "\n" patch)
    (substring patch 0 (1- (match-end 0)))))

(defun mercit-diff-hunk-region-patch (section &optional args)
  (let ((op (if (member "--reverse" args) "+" "-"))
        (sbeg (oref section start))
        (rbeg (mercit-diff-hunk-region-beginning))
        (rend (region-end))
        (send (oref section end))
        (patch nil))
    (save-excursion
      (goto-char sbeg)
      (while (< (point) send)
        (looking-at "\\(.\\)\\([^\n]*\n\\)")
        (cond ((or (string-match-p "[@ ]" (match-string-no-properties 1))
                   (and (>= (point) rbeg)
                        (<= (point) rend)))
               (push (match-string-no-properties 0) patch))
              ((equal op (match-string-no-properties 1))
               (push (concat " " (match-string-no-properties 2)) patch)))
        (forward-line)))
    (let ((buffer-list-update-hook nil)) ; #3759
      (with-temp-buffer
        (insert (mapconcat #'identity (reverse patch) ""))
        (diff-fixup-modifs (point-min) (point-max))
        (setq patch (buffer-string))))
    patch))

;;; _
(provide 'mercit-diff)
;;; mercit-diff.el ends here
