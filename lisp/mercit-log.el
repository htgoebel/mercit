;;; mercit-log.el --- Inspect Mercurial history  -*- lexical-binding:t; coding:utf-8 -*-

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

;; This library implements support for looking at Mercurial logs, including
;; special logs like cherry-logs, as well as for selecting a commit
;; from a log.

;;; Code:

(require 'mercit-core)
(require 'mercit-diff)

(declare-function mercit-blob-visit "mercit-files" (blob-or-file))
(declare-function mercit-cherry-apply "mercit-sequence" (commit &optional args))
(declare-function mercit-insert-head-branch-header "mercit-status"
                  (&optional branch))
(declare-function mercit-insert-upstream-branch-header "mercit-status"
                  (&optional branch pull keyword))
(declare-function mercit-read-file-from-rev "mercit-files"
                  (rev prompt &optional default))
(declare-function mercit-rebase--get-state-lines "mercit-sequence"
                  (file))
(declare-function mercit-show-commit "mercit-diff"
                  (arg1 &optional arg2 arg3 arg4))
(declare-function mercit-reflog-format-subject "mercit-reflog" (subject))
(defvar mercit-refs-focus-column-width)
(defvar mercit-refs-margin)
(defvar mercit-refs-show-commit-count)
(defvar mercit-buffer-margin)
(defvar mercit-status-margin)
(defvar mercit-status-sections-hook)

(require 'ansi-color)
(require 'crm)
(require 'which-func)

;;; Options
;;;; Log Mode

(defgroup mercit-log nil
  "Inspect and manipulate Mercurial history."
  :link '(info-link "(mercit)Logging")
  :group 'mercit-commands
  :group 'mercit-modes)

(defcustom mercit-log-mode-hook nil
  "Hook run after entering Mercit-Log mode."
  :group 'mercit-log
  :type 'hook)

(defcustom mercit-log-remove-graph-args '("--follow" "--grep" "-G" "-S" "-L")
  "The log arguments that cause the `--graph' argument to be dropped."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-log
  :type '(repeat (string :tag "Argument"))
  :options '("--follow" "--grep" "-G" "-S" "-L"))

(defcustom mercit-log-revision-headers-format "\
{if(desc, '\n{desc}'}
Author:    {author}"  ;; TODO was {body}{notes} author {committer}
  "Additional format string used with the `++header' argument."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-log
  :type 'string)

(defcustom mercit-log-auto-more nil
  "Insert more log entries automatically when moving past the last entry.
Only considered when moving past the last entry with
`mercit-goto-*-section' commands."
  :group 'mercit-log
  :type 'boolean)

(defcustom mercit-log-margin '(t age mercit-log-margin-width t 18)
  "Format of the margin in `mercit-log-mode' buffers.

The value has the form (INIT STYLE WIDTH AUTHOR AUTHOR-WIDTH).

If INIT is non-nil, then the margin is shown initially.
STYLE controls how to format the author or committer date.
  It can be one of `age' (to show the age of the commit),
  `age-abbreviated' (to abbreviate the time unit to a character),
  or a string (suitable for `format-time-string') to show the
  actual date.
WIDTH controls the width of the margin.  This exists for forward
  compatibility and currently the value should not be changed.
AUTHOR controls whether the name of the author is also shown by
  default.
AUTHOR-WIDTH has to be an integer.  When the name of the author
  is shown, then this specifies how much space is used to do so."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-log
  :group 'mercit-margin
  :type mercit-log-margin--custom-type
  :initialize #'mercit-custom-initialize-reset
  :set (apply-partially #'mercit-margin-set-variable 'mercit-log-mode))

(defcustom mercit-log-show-refname-after-summary nil
  "Whether to show refnames after commit summaries.
This is useful if you use really long branch names."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-log
  :type 'boolean)

(defcustom mercit-log-highlight-keywords t
  "Whether to highlight bracketed keywords in commit summaries."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-log
  :type 'boolean)

(defcustom mercit-log-header-line-function #'mercit-log-header-line-sentence
  "Function used to generate text shown in header line of log buffers."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-log
  :type '(choice (function-item mercit-log-header-line-arguments)
                 (function-item mercit-log-header-line-sentence)
                 function))

(defcustom mercit-log-trace-definition-function #'mercit-which-function
  "Function used to determine the function at point.
This is used by the command `mercit-log-trace-definition'.
You should prefer `mercit-which-function' over `which-function'
because the latter may make use of Imenu's outdated cache."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-log
  :type '(choice (function-item mercit-which-function)
                 (function-item which-function)
                 (function-item add-log-current-defun)
                 function))

(defface mercit-log-graph
  '((((class color) (background light)) :foreground "grey30")
    (((class color) (background  dark)) :foreground "grey80"))
  "Face for the graph part of the log output."
  :group 'mercit-faces)

(defface mercit-log-author
  '((((class color) (background light))
     :foreground "firebrick"
     :slant normal
     :weight normal)
    (((class color) (background  dark))
     :foreground "tomato"
     :slant normal
     :weight normal))
  "Face for the author part of the log output."
  :group 'mercit-faces)

(defface mercit-log-date
  '((((class color) (background light))
     :foreground "grey30"
     :slant normal
     :weight normal)
    (((class color) (background  dark))
     :foreground "grey80"
     :slant normal
     :weight normal))
  "Face for the date part of the log output."
  :group 'mercit-faces)

(defface mercit-header-line-log-select
  '((t :inherit bold))
  "Face for the `header-line' in `mercit-log-select-mode'."
  :group 'mercit-faces)

;;;; File Log

(defcustom mercit-log-buffer-file-locked t
  "Whether `mercit-log-buffer-file-quick' uses a dedicated buffer."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-commands
  :group 'mercit-log
  :type 'boolean)

;;;; Select Mode

(defcustom mercit-log-select-show-usage 'both
  "Whether to show usage information when selecting a commit from a log.
The message can be shown in the `echo-area' or the `header-line', or in
`both' places.  If the value isn't one of these symbols, then it should
be nil, in which case no usage information is shown."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-log
  :type '(choice (const :tag "in echo-area" echo-area)
                 (const :tag "in header-line" header-line)
                 (const :tag "in both places" both)
                 (const :tag "nowhere")))

(defcustom mercit-log-select-margin
  (list (nth 0 mercit-log-margin)
        (nth 1 mercit-log-margin)
        'mercit-log-margin-width t
        (nth 4 mercit-log-margin))
  "Format of the margin in `mercit-log-select-mode' buffers.

The value has the form (INIT STYLE WIDTH AUTHOR AUTHOR-WIDTH).

If INIT is non-nil, then the margin is shown initially.
STYLE controls how to format the author or committer date.
  It can be one of `age' (to show the age of the commit),
  `age-abbreviated' (to abbreviate the time unit to a character),
  or a string (suitable for `format-time-string') to show the
  actual date.
WIDTH controls the width of the margin.  This exists for forward
  compatibility and currently the value should not be changed.
AUTHOR controls whether the name of the author is also shown by
  default.
AUTHOR-WIDTH has to be an integer.  When the name of the author
  is shown, then this specifies how much space is used to do so."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-log
  :group 'mercit-margin
  :type mercit-log-margin--custom-type
  :initialize #'mercit-custom-initialize-reset
  :set-after '(mercit-log-margin)
  :set (apply-partially #'mercit-margin-set-variable 'mercit-log-select-mode))

;;;; Cherry Mode

(defcustom mercit-cherry-sections-hook
  '(mercit-insert-cherry-headers
    mercit-insert-cherry-commits)
  "Hook run to insert sections into the cherry buffer."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-log
  :type 'hook)

(defcustom mercit-cherry-margin
  (list (nth 0 mercit-log-margin)
        (nth 1 mercit-log-margin)
        'mercit-log-margin-width t
        (nth 4 mercit-log-margin))
  "Format of the margin in `mercit-cherry-mode' buffers.

The value has the form (INIT STYLE WIDTH AUTHOR AUTHOR-WIDTH).

If INIT is non-nil, then the margin is shown initially.
STYLE controls how to format the author or committer date.
  It can be one of `age' (to show the age of the commit),
  `age-abbreviated' (to abbreviate the time unit to a character),
  or a string (suitable for `format-time-string') to show the
  actual date.
WIDTH controls the width of the margin.  This exists for forward
  compatibility and currently the value should not be changed.
AUTHOR controls whether the name of the author is also shown by
  default.
AUTHOR-WIDTH has to be an integer.  When the name of the author
  is shown, then this specifies how much space is used to do so."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-log
  :group 'mercit-margin
  :type mercit-log-margin--custom-type
  :initialize #'mercit-custom-initialize-reset
  :set-after '(mercit-log-margin)
  :set (apply-partially #'mercit-margin-set-variable 'mercit-cherry-mode))

;;;; Log Sections

(defcustom mercit-log-section-commit-count 10
  "How many recent commits to show in certain log sections.
How many recent commits `mercit-insert-recent-commits' and
`mercit-insert-unpulled-from-upstream-or-recent' (provided
the upstream isn't ahead of the current branch) show."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-status
  :type 'number)

(defcustom mercit-log-merged-commit-count 20
  "How many surrounding commits to show for `mercit-log-merged'.
`mercit-log-merged' will shows approximately half of this number
commits before and half after."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-log
  :type 'integer)

;;; Arguments
;;;; Prefix Classes

(defclass mercit-log-prefix (transient-prefix)
  ((history-key :initform 'mercit-log)
   (major-mode  :initform 'mercit-log-mode)))

(defclass mercit-log-refresh-prefix (mercit-log-prefix)
  ((history-key :initform 'mercit-log)
   (major-mode  :initform nil)))

;;;; Prefix Methods

(cl-defmethod transient-init-value ((obj mercit-log-prefix))
  (pcase-let ((`(,args ,files)
               (mercit-log--get-value 'mercit-log-mode
                                     mercit-prefix-use-buffer-arguments)))
    (unless (eq transient-current-command 'mercit-dispatch)
      (when-let ((file (mercit-file-relative-name)))
        (setq files (list file))))
    (oset obj value (if files `(("--" ,@files) ,args) args))))

(cl-defmethod transient-init-value ((obj mercit-log-refresh-prefix))
  (oset obj value (if mercit-buffer-log-files
                      `(("--" ,@mercit-buffer-log-files)
                        ,mercit-buffer-log-args)
                    mercit-buffer-log-args)))

(cl-defmethod transient-set-value ((obj mercit-log-prefix))
  (mercit-log--set-value obj))

(cl-defmethod transient-save-value ((obj mercit-log-prefix))
  (mercit-log--set-value obj 'save))

;;;; Argument Access

(defun mercit-log-arguments (&optional mode)
  "Return the current log arguments."
  (if (memq transient-current-command '(mercit-log mercit-log-refresh))
      (pcase-let ((`(,args ,alist)
                   (-separate #'atom (transient-get-value))))
        (list args (cdr (assoc "--" alist))))
    (mercit-log--get-value (or mode 'mercit-log-mode))))

(defun mercit-log--get-value (mode &optional use-buffer-args)
  (unless use-buffer-args
    (setq use-buffer-args mercit-direct-use-buffer-arguments))
  (let (args files)
    (cond
     ((and (memq use-buffer-args '(always selected current))
           (eq major-mode mode))
      (setq args  mercit-buffer-log-args)
      (setq files mercit-buffer-log-files))
     ((and (memq use-buffer-args '(always selected))
           (when-let ((buffer (mercit-get-mode-buffer
                               mode nil
                               (eq use-buffer-args 'selected))))
             (setq args  (buffer-local-value 'mercit-buffer-log-args buffer))
             (setq files (buffer-local-value 'mercit-buffer-log-files buffer))
             t)))
     ((plist-member (symbol-plist mode) 'mercit-log-current-arguments)
      (setq args (get mode 'mercit-log-current-arguments)))
     ((when-let ((elt (assq (intern (format "mercit-log:%s" mode))
                            transient-values)))
        (setq args (cdr elt))
        t))
     (t
      (setq args (get mode 'mercit-log-default-arguments))))
    (list args files)))

(defun mercit-log--set-value (obj &optional save)
  (pcase-let* ((obj  (oref obj prototype))
               (mode (or (oref obj major-mode) major-mode))
               (key  (intern (format "mercit-log:%s" mode)))
               (`(,args ,alist)
                (-separate #'atom (transient-get-value)))
               (files (cdr (assoc "--" alist))))
    (put mode 'mercit-log-current-arguments args)
    (when save
      (setf (alist-get key transient-values) args)
      (transient-save-values))
    (transient--history-push obj)
    (setq mercit-buffer-log-args args)
    (unless (derived-mode-p 'mercit-log-select-mode)
      (setq mercit-buffer-log-files files))
    (mercit-refresh)))

;;; Commands
;;;; Prefix Commands

;;;###autoload (autoload 'mercit-log "mercit-log" nil t)
(transient-define-prefix mercit-log ()
  "Show a commit or reference log."
  :man-page "git-log"
  :class 'mercit-log-prefix
  ;; The grouping in git-log(1) appears to be guided by implementation
  ;; details, so our logical grouping only follows it to an extend.
  ;; Arguments that are "misplaced" here:
  ;;   1. From "Commit Formatting".
  ;;   2. From "Common Diff Options".
  ;;   3. From unnamed first group.
  ;;   4. Implemented by Mercit.
  ["Commit limiting"
   (mercit-log:-n)
   (mercit:--author)
   (7 mercit-log:--since)
   (7 mercit-log:--until)
   (mercit-log:--grep)
   (7 "-i" "*Search case-insensitive" ("-i" "--regexp-ignore-case"))
   (7 "-I" "*Invert search pattern"   "--invert-grep")
   (mercit-log:-G)     ;2
   (mercit-log:-S)     ;2
   (mercit-log:-L)     ;2
   (7 "=m" "*Omit merges"            "--no-merges")
   (7 "=p" "*First parent"           "--first-parent")]
  ["History simplification"
   (  "-D" "*Simplify by decoration"                  "--simplify-by-decoration")
   (mercit:--)
   (  "-f" "Follow renames when showing single-file log"     "--follow") ;3
   (6 "/s" "*Only commits changing given paths"               "--sparse")
   (7 "/d" "*Only selected commits plus meaningful history"   "--dense")
   (7 "/a" "*Only commits existing directly on ancestry path" "--ancestry-path")
   (6 "/f" "*Do not prune history"                            "--full-history")
   (7 "/m" "*Prune some history"                              "--simplify-merges")]
  ;; ["Commit ordering"  TODO
  ;;  (mercit-log:--*-order)
  ;;  ("-r" "*Reverse order" "--reverse")]
  ["Formatting"
   ("-g" "Show graph"          "--graph")          ;1
   ("-c" "*Show graph in color" "--color")          ;2
   ("-d" "Show refnames"       "--decorate")       ;3
   ("=S" "*Show signatures"     "--show-signature") ;1
   ("-h" "*Show header"         "++header")         ;4
   ("-p" "*Show diffs"          ("-p" "--patch"))   ;2
   ("-s" "Show diffstats"      "--stat")]          ;2
  [["Log"
    ("l" "*current"             mercit-log-current)
    ("h" "*HEAD"                mercit-log-head)
    ("u" "*related"             mercit-log-related)
    ("o" "*other"               mercit-log-other)]
   [""
    ("L" "*local branches"      mercit-log-branches)
    ("b" "*all branches"        mercit-log-all-branches)
    ("a" "*all references"      mercit-log-all)
    (7 "B" "*matching branches" mercit-log-matching-branches)
    (7 "T" "*matching tags"     mercit-log-matching-tags)
    (7 "m" "*merged"            mercit-log-merged)]
   ["Reflog"
    ("r" "*current"             mercit-reflog-current)
    ("H" "*HEAD"                mercit-reflog-head)
    ("O" "*other"               mercit-reflog-other)]
   [:if (lambda ()
          (and (fboundp 'mercit--any-wip-mode-enabled-p)
               (mercit--any-wip-mode-enabled-p)))
    :description "Wiplog"
    ("i" "*index"          mercit-wip-log-index)
    ("w" "*worktree"       mercit-wip-log-worktree)]
   ["Other"
    (5 "s" "*shortlog"    mercit-shortlog)]])

;;;###autoload (autoload 'mercit-log-refresh "mercit-log" nil t)
(transient-define-prefix mercit-log-refresh ()
  "Change the arguments used for the log(s) in the current buffer."
  :man-page "git-log"
  :class 'mercit-log-refresh-prefix
  [:if-mode mercit-log-mode
   :class transient-subgroups
   ["Commit limiting"
    (mercit-log:-n)
    (mercit:--author)
    (mercit-log:--grep)
    (7 "-i" "*Search case-insensitive" ("-i" "--regexp-ignore-case"))
    (7 "-I" "*Invert search pattern"   "--invert-grep")
    (mercit-log:-G)
    (mercit-log:-S)
    (mercit-log:-L)]
   ["History simplification"
    (  "-D" "*Simplify by decoration"                  "--simplify-by-decoration")
    (mercit:--)
    (  "-f" "Follow renames when showing single-file log"     "--follow") ;3
    (6 "/s" "*Only commits changing given paths"               "--sparse")
    (7 "/d" "*Only selected commits plus meaningful history"   "--dense")
    (7 "/a" "*Only commits existing directly on ancestry path" "--ancestry-path")
    (6 "/f" "*Do not prune history"                            "--full-history")
    (7 "/m" "*Prune some history"                              "--simplify-merges")]
   ["Commit ordering"
    (mercit-log:--*-order)
    ("-r" "*Reverse order" "--reverse")]
   ["Formatting"
    ("-g" "Show graph"              "--graph")
    ("-c" "*Show graph in color"     "--color")
    ("-d" "Show refnames"           "--decorate")
    ("=S" "*Show signatures"         "--show-signature")
    ("-h" "*Show header"             "++header")
    ("-p" "*Show diffs"              ("-p" "--patch"))
    ("-s" "Show diffstats"          "--stat")]]
  [:if-not-mode mercit-log-mode
   :description "Arguments"
   (mercit-log:-n)
   ;; (mercit-log:--*-order)
   ("-g" "Show graph"               "--graph")
   ("-c" "*Show graph in color"      "--color")
   ("-d" "Show refnames"            "--decorate")]
  [["Refresh"
    ("g" "*buffer"                   mercit-log-refresh)
    ("s" "*buffer and set defaults"  transient-set  :transient nil)
    ("w" "*buffer and save defaults" transient-save :transient nil)]
   ["Margin"
    ("L" "*toggle visibility"        mercit-toggle-margin      :transient t)
    ("l" "*cycle style"              mercit-cycle-margin-style :transient t)
    ("d" "*toggle details"           mercit-toggle-margin-details)
    ("x" "*toggle shortstat"         mercit-toggle-log-margin-style)]
   [:if-mode mercit-log-mode
    :description "Toggle"
    ("b" "*buffer lock"              mercit-toggle-buffer-lock)]]
  (interactive)
  (cond
   ((not (eq transient-current-command 'mercit-log-refresh))
    (pcase major-mode
      ('mercit-reflog-mode
       (user-error "Cannot change log arguments in reflog buffers"))
      ('mercit-cherry-mode
       (user-error "Cannot change log arguments in cherry buffers")))
    (transient-setup 'mercit-log-refresh))
   (t
    (pcase-let ((`(,args ,files) (mercit-log-arguments)))
      (setq mercit-buffer-log-args args)
      (unless (derived-mode-p 'mercit-log-select-mode)
        (setq mercit-buffer-log-files files)))
    (mercit-refresh))))

;;;; Infix Commands

(transient-define-argument mercit-log:-n ()
  :description "*Limit number of commits"
  :class 'transient-option
  ;; For historic reasons (and because it easy to guess what "-n"
  ;; stands for) this is the only argument where we do not use the
  ;; long argument ("--max-count").
  :shortarg "-n"
  :argument "--limit="
  :reader #'transient-read-number-N+)

(transient-define-argument mercit:--author ()
  :description "*Limit to author"
  :class 'transient-option
  :key "-u"
  :argument "--user="
  :reader #'mercit-transient-read-person)

(transient-define-argument mercit-log:--since ()
  :description "*Limit to commits since"
  :class 'transient-option
  :key "=s"
  :argument "--since="
  :reader #'transient-read-date)

(transient-define-argument mercit-log:--until ()
  :description "*Limit to commits until"
  :class 'transient-option
  :key "=u"
  :argument "--until="
  :reader #'transient-read-date)

(transient-define-argument mercit-log:--*-order ()  ;; TODO
  :description "*Order commits by"
  :class 'transient-switches
  :key "-o"
  :argument-format "--%s-order"
  :argument-regexp "\\(--\\(topo\\|author-date\\|date\\)-order\\)"
  :choices '("topo" "author-date" "date"))

(transient-define-argument mercit-log:--grep ()
  :description "*Search messages"
  :class 'transient-option
  :key "-F"
  :argument "--grep=")

(transient-define-argument mercit-log:-G ()
  :description "*Search changes"
  :class 'transient-option
  :argument "-G")

(transient-define-argument mercit-log:-S ()
  :description "*Search occurrences"
  :class 'transient-option
  :argument "-S")

(transient-define-argument mercit-log:-L ()
  :description "*Trace line evolution"
  :class 'transient-option
  :argument "-L"
  :reader #'mercit-read-file-trace)

(defun mercit-read-file-trace (&rest _ignored)
  (let ((file  (mercit-read-file-from-rev "--rev=." "File"))
        (trace (mercit-read-string "Trace")))
    (concat trace ":" file)))

;;;; Setup Commands

(defvar mercit-log-read-revs-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map crm-local-completion-map)
    (define-key map "\s" #'self-insert-command)
    map))

(defun mercit-log-read-revs (&optional use-current)
  (or (and use-current (and-let* ((buf (mercit-get-current-branch))) (list buf)))
      (let ((crm-separator "\\(\\.\\.\\.?\\|[, ]\\)")
            (crm-local-completion-map mercit-log-read-revs-map))
        (split-string (mercit-completing-read-multiple*
                       "Log rev,s: "
                       (mercit-list-refnames nil t)
                       nil nil nil 'mercit-revision-history
                       (or (mercit-branch-or-commit-at-point)
                           (unless use-current
                             (mercit-get-previous-branch)))
                       nil t)
                      "[, ]" t))))

(defun mercit-log-read-pattern (option)
  "Read a string from the user to pass as parameter to OPTION."
  (mercit-read-string (format "Type a pattern to pass to %s" option)))

;;;###autoload
(defun mercit-log-current (revs &optional args files)
  "Show log for the current branch.
When `HEAD' is detached or with a prefix argument show log for
one or more revs read from the minibuffer."
  (interactive (cons (mercit-log-read-revs t)
                     (mercit-log-arguments)))
  (mercit-log-setup-buffer revs args files))

;;;###autoload
(defun mercit-log-head (&optional args files)
  "Show log for `HEAD'."
  (interactive (mercit-log-arguments))
  (mercit-log-setup-buffer (list "--rev=.") args files))

;;;###autoload
(defun mercit-log-related (revs &optional args files)  ;; TODO
  "Show log for the current branch, its upstream and its push target.
When the upstream is a local branch, then also show its own
upstream.  When `HEAD' is detached, then show log for that, the
previously checked out branch and its upstream and push-target."
  (interactive
   (cons (let ((current (mercit-get-current-branch))
               head rebase target upstream upup)
           (unless current
             (setq rebase (mercit-rebase--get-state-lines "head-name"))
             (cond (rebase
                    (setq rebase (mercit-ref-abbrev rebase))
                    (setq current rebase)
                    (setq head "--rev=."))
                   (t (setq current (mercit-get-previous-branch)))))
           (cond (current
                  (setq current
                        (mercit--propertize-face current'mercit-branch-local))
                  (setq target (mercit-get-push-branch current t))
                  (setq upstream (mercit-get-upstream-branch current))
                  (when upstream
                    (setq upup (and (mercit-local-branch-p upstream)
                                    (mercit-get-upstream-branch upstream)))))
                 (t (setq head "--rev=.")))
           (delq nil (list current head target upstream upup)))
         (mercit-log-arguments)))
  (mercit-log-setup-buffer revs args files))

;;;###autoload
(defun mercit-log-other (revs &optional args files)
  "Show log for one or more revs read from the minibuffer.
The user can input any revision or revisions separated by a
space, or even ranges, but only branches and tags, and a
representation of the commit at point, are available as
completion candidates."
  (interactive (cons (mercit-log-read-revs)
                     (mercit-log-arguments)))
  (mercit-log-setup-buffer revs args files))

;;;###autoload
(defun mercit-log-branches (&optional args files)  ;; TODO?
  "Show log for all local branches and `HEAD'."
  (interactive (mercit-log-arguments))
  (mercit-log-setup-buffer (if (mercit-get-current-branch)
                              (list "--git")
                            (list "--rev=." "--git"))
                          args files))

;;;###autoload
(defun mercit-log-matching-branches (pattern &optional args files)
  "Show log for all branches matching PATTERN and `HEAD'."  ;; TODO: branches AND head
  (interactive (cons (mercit-log-read-pattern "--branches") (mercit-log-arguments)))
  (mercit-log-setup-buffer
   (list "--rev=." "--git" "--branch" pattern)  ;; TODO: actually handle pattern
   args files))

;;;###autoload
(defun mercit-log-matching-tags (pattern &optional args files)
  "Show log for all tags matching PATTERN and `HEAD'."  ;; TODO: tags AND head
  (interactive (cons (mercit-log-read-pattern "--tags") (mercit-log-arguments)))
  (mercit-log-setup-buffer
   (list "--rev=." (format "--tags=%s" pattern))
   args files))

;;;###autoload
(defun mercit-log-all-branches (&optional args files)  ;; TODO
  "Show log for all local and remote branches and `HEAD'."
  (interactive (mercit-log-arguments))
  (mercit-log-setup-buffer (if (mercit-get-current-branch)
                              (list "--branches" "--remotes")
                            (list "--rev=." "--branches" "--remotes"))
                          args files))

;;;###autoload
(defun mercit-log-all (&optional args files)  ;;TODO
  "Show log for all references and `HEAD'."
  (interactive (mercit-log-arguments))
  (mercit-log-setup-buffer (if (mercit-get-current-branch)
                              (list "--git" "--all")
                            (list "--rev=." "--git" "--all"))
                          args files))

;;;###autoload
(defun mercit-log-buffer-file (&optional follow beg end)
  "Show log for the blob or file visited in the current buffer.
With a prefix argument or when `--follow' is an active log
argument, then follow renames.  When the region is active,
restrict the log to the lines that the region touches."
  (interactive
   (cons current-prefix-arg
         (and (region-active-p)
              (mercit-file-relative-name)
              (save-restriction
                (widen)
                (list (line-number-at-pos (region-beginning))
                      (line-number-at-pos
                       (let ((end (region-end)))
                         (if (char-after end)
                             end
                           ;; Ensure that we don't get the line number
                           ;; of a trailing newline.
                           (1- end)))))))))
  (require 'mercit)
  (if-let ((file (mercit-file-relative-name)))
      (mercit-log-setup-buffer
       (list (or mercit-buffer-refname
                 (mercit-get-current-branch)
                 "--rev=."))
       (let ((args (car (mercit-log-arguments))))
         (when (and follow (not (member "--follow" args)))
           (push "--follow" args))
         (when (and (file-regular-p
                     (expand-file-name file (mercit-toplevel)))
                    beg end)
           (setq args (cons (format "-L%s,%s:%s" beg end file)  ;; TODO
                            (cl-delete "-L" args :test
                                       #'string-prefix-p)))
           (setq file nil))
         args)
       (and file (list file))
       mercit-log-buffer-file-locked)
    (user-error "Buffer isn't visiting a file")))

;;;###autoload
(defun mercit-log-trace-definition (file fn rev)
  "Show log for the definition at point."
  (interactive (list (or (mercit-file-relative-name)
                         (user-error "Buffer isn't visiting a file"))
                     (or (funcall mercit-log-trace-definition-function)
                         (user-error "No function at point found"))
                     (or mercit-buffer-refname
                         (mercit-get-current-branch)
                         "--rev=.")))
  (require 'mercit)
  (mercit-log-setup-buffer
   (list rev)
   (cons (format "-L:%s%s:%s"  ;; TODO
                 (string-replace ":" "\\:" (regexp-quote fn))
                 (if (derived-mode-p 'lisp-mode 'emacs-lisp-mode)
                     ;; Git doesn't treat "-" the same way as
                     ;; "_", leading to false-positives such as
                     ;; "foo-suffix" being considered a match
                     ;; for "foo".  Wing it.
                     "\\( \\|$\\)"
                   ;; We could use "\\b" here, but since Git
                   ;; already does something equivalent, that
                   ;; isn't necessary.
                   "")
                 file)
         (cl-delete "-L" (car (mercit-log-arguments))
                    :test #'string-prefix-p))
   nil mercit-log-buffer-file-locked))

(defun mercit-diff-trace-definition ()
  "Show log for the definition at point in a diff."
  (interactive)
  (pcase-let ((`(,buf ,pos) (mercit-diff-visit-file--noselect)))
    (mercit--with-temp-position buf pos
      (call-interactively #'mercit-log-trace-definition))))

;;;###autoload
(defun mercit-log-merged (commit branch &optional args files)  ;; TODO
  "Show log for the merge of COMMIT into BRANCH.

More precisely, find merge commit M that brought COMMIT into
BRANCH, and show the log of the range \"M^1..M\". If COMMIT is
directly on BRANCH, then show approximately
`mercit-log-merged-commit-count' surrounding commits instead.

This command requires git-when-merged, which is available from
https://github.com/mhagger/git-when-merged."
  (interactive
   (append (let ((commit (mercit-read-branch-or-commit "Log merge of commit")))
             (list commit
                   (mercit-read-other-branch "Merged into" commit)))
           (mercit-log-arguments)))
  (unless (mercit-git-executable-find "git-when-merged")
    (user-error "This command requires git-when-merged (%s)"
                "https://github.com/mhagger/git-when-merged"))
  (let (exit m)
    (with-temp-buffer
      (save-excursion
        (setq exit (mercit-process-git t "when-merged" "-c"
                                      (mercit-abbrev-arg)
                                      commit branch)))
      (setq m (buffer-substring-no-properties (point) (line-end-position))))
    (if (zerop exit)
        (mercit-log-setup-buffer (list (format "%s^1..%s" m m))
                                args files nil commit)
      ;; Output: "<ref><lots of spaces><message>".
      ;; This is not the same as `string-trim'.
      (setq m (string-trim-left (substring m (string-match " " m))))
      (if (equal m "Commit is directly on this branch.")
          (let* ((from (format "%s~%d" commit
                               (/ mercit-log-merged-commit-count 2)))
                 (to (- (car (mercit-rev-diff-count branch commit))
                        (/ mercit-log-merged-commit-count 2)))
                 (to (if (<= to 0)
                         branch
                       (format "%s~%s" branch to))))
            (unless (mercit-rev-verify-commit from)
              (setq from (mercit-git-string "rev-list" "--max-parents=0"
                                           commit)))
            (mercit-log-setup-buffer (list (concat from ".." to))
                                    (cons "--first-parent" args)
                                    files nil commit))
        (user-error "Could not find when %s was merged into %s: %s"
                    commit branch m)))))

;;;; Limit Commands

(defun mercit-log-toggle-commit-limit ()
  "Toggle the number of commits the current log buffer is limited to.
If the number of commits is currently limited, then remove that
limit.  Otherwise set it to 256."
  (interactive)
  (mercit-log-set-commit-limit (lambda (&rest _) nil)))

(defun mercit-log-double-commit-limit ()
  "Double the number of commits the current log buffer is limited to."
  (interactive)
  (mercit-log-set-commit-limit '*))

(defun mercit-log-half-commit-limit ()
  "Half the number of commits the current log buffer is limited to."
  (interactive)
  (mercit-log-set-commit-limit '/))

(defun mercit-log-set-commit-limit (fn)
  (let* ((val mercit-buffer-log-args)
         (arg (--first (string-match "^--limit=\\([0-9]+\\)?$" it) val))
         (num (and arg (string-to-number (match-string 1 arg))))
         (num (if num (funcall fn num 2) 256)))
    (setq val (delete arg val))
    (setq mercit-buffer-log-args
          (if (and num (> num 0))
              (cons (format "--limit=%i" num) val)
            val)))
  (mercit-refresh))

(defun mercit-log-get-commit-limit ()
  (and-let* ((str (--first (string-match "^--limit=\\([0-9]+\\)?$" it)
                           mercit-buffer-log-args)))
    (string-to-number (match-string 1 str))))

;;;; Mode Commands

(defun mercit-log-bury-buffer (&optional arg)
  "Bury the current buffer or the revision buffer in the same frame.
Like `mercit-mode-bury-buffer' (which see) but with a negative
prefix argument instead bury the revision buffer, provided it
is displayed in the current frame."
  (interactive "p")
  (if (< arg 0)
      (let* ((buf (mercit-get-mode-buffer 'mercit-revision-mode))
             (win (and buf (get-buffer-window buf (selected-frame)))))
        (if win
            (with-selected-window win
              (with-current-buffer buf
                (mercit-mode-bury-buffer (> (abs arg) 1))))
          (user-error "No revision buffer in this frame")))
    (mercit-mode-bury-buffer (> arg 1))))

;;;###autoload
(defun mercit-log-move-to-parent (&optional n)
  "Move to the Nth parent of the current commit."
  (interactive "p")
  (when (derived-mode-p 'mercit-log-mode)
    (when (mercit-section-match 'commit)
      (let* ((section (mercit-current-section))
             (parent-rev (format "%s^%s" (oref section value) (or n 1))))
        (if-let ((parent-hash (mercit-rev-parse "-T" "{node:short}" parent-rev)))
            (if-let ((parent (--first (equal (oref it value)
                                             parent-hash)
                                      (mercit-section-siblings section 'next))))
                (mercit-section-goto parent)
              (user-error
               (substitute-command-keys
                (concat "Parent " parent-hash " not found.  Try typing "
                        "\\[mercit-log-double-commit-limit] first"))))
          (user-error "Parent %s does not exist" parent-rev))))))

(defun mercit-log-move-to-revision (rev)
  "Read a revision and move to it in current log buffer.

If the chosen reference or revision isn't being displayed in
the current log buffer, then inform the user about that and do
nothing else.

If invoked outside any log buffer, then display the log buffer
of the current repository first; creating it if necessary."
  (interactive (list (mercit-read-branch-or-commit "In log, jump to")))
  (with-current-buffer
      (cond ((derived-mode-p 'mercit-log-mode)
             (current-buffer))
            ((and-let* ((buf (mercit-get-mode-buffer 'mercit-log-mode)))
               (pop-to-buffer-same-window buf)))
            (t
             (apply #'mercit-log-all-branches (mercit-log-arguments))))
    (unless (mercit-log-goto-commit-section (mercit-rev-abbrev rev))
      (user-error "%s isn't visible in the current log buffer" rev))))

;;;; Shortlog Commands

;;;###autoload (autoload 'mercit-shortlog "mercit-log" nil t)
(transient-define-prefix mercit-shortlog ()  ;; TODO
  "Show a history summary."
  :man-page "git-shortlog"
  :value '("--numbered" "--summary")
  ["Arguments"
   ("-n" "*Sort by number of commits"      ("-n" "--numbered"))
   ("-s" "*Show commit count summary only" ("-s" "--summary"))
   ("-e" "*Show email addresses"           ("-e" "--email"))
   ("-g" "*Group commits by" "--group="
    :choices ("author" "committer" "trailer:"))
   (7 "-f" "*Format string" "--format=")
   (7 "-w" "*Linewrap" "-w" :class transient-option)]
  ["Shortlog"
   ("s" "*since" mercit-shortlog-since)
   ("r" "*range" mercit-shortlog-range)])

(defun mercit-git-shortlog (rev args)  ;; TODO
  (let ((dir default-directory))
    (with-current-buffer (get-buffer-create "*mercit-shortlog*")
      (setq default-directory dir)
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (save-excursion
          (mercit-git-insert "shortlog" args rev))
        (switch-to-buffer-other-window (current-buffer))))))

;;;###autoload
(defun mercit-shortlog-since (rev args)
  "Show a history summary for commits since REV."
  (interactive
   (list (mercit-read-branch-or-commit "Shortlog since" (mercit-get-current-tag))
         (transient-args 'mercit-shortlog)))
  (mercit-git-shortlog (concat rev "..") args))

;;;###autoload
(defun mercit-shortlog-range (rev-or-range args)
  "Show a history summary for commit or range REV-OR-RANGE."
  (interactive
   (list (mercit-read-range-or-commit "Shortlog for revision or range")
         (transient-args 'mercit-shortlog)))
  (mercit-git-shortlog rev-or-range args))

;;; Log Mode

(defvar mercit-log-disable-graph-hack-args
  '("-G" "--grep" "--author")
  "Arguments which disable the graph speedup hack.")

(defvar mercit-log-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mercit-mode-map)
    (define-key map (kbd "C-c C-b") #'mercit-go-backward)
    (define-key map (kbd "C-c C-f") #'mercit-go-forward)
    (define-key map (kbd "C-c C-n") #'mercit-log-move-to-parent)
    (define-key map "j" #'mercit-log-move-to-revision)
    (define-key map "=" #'mercit-log-toggle-commit-limit)
    (define-key map "+" #'mercit-log-double-commit-limit)
    (define-key map "-" #'mercit-log-half-commit-limit)
    (define-key map "q" #'mercit-log-bury-buffer)
    map)
  "Keymap for `mercit-log-mode'.")

(define-derived-mode mercit-log-mode mercit-mode "Mercit Log"
  "Mode for looking at Mercurial log.

This mode is documented in info node `(mercit)Log Buffer'.

\\<mercit-mode-map>\
Type \\[mercit-refresh] to refresh the current buffer.
Type \\[mercit-visit-thing] or \\[mercit-diff-show-or-scroll-up] \
to visit the commit at point.

Type \\[mercit-branch] to see available branch commands.
Type \\[mercit-merge] to merge the branch or commit at point.
Type \\[mercit-cherry-pick] to apply the commit at point.
Type \\[mercit-reset] to reset `HEAD' to the commit at point.

\\{mercit-log-mode-map}"
  :group 'mercit-log
  (hack-dir-local-variables-non-file-buffer)
  (setq mercit--imenu-item-types 'commit))

(put 'mercit-log-mode 'mercit-log-default-arguments
     '("--graph" "--decorate" "--limit=256"))

(defun mercit-log-setup-buffer (revs args files &optional locked focus)
  (require 'mercit)
  (with-current-buffer
      (mercit-setup-buffer #'mercit-log-mode locked
        (mercit-buffer-revisions revs)
        (mercit-buffer-log-args args)
        (mercit-buffer-log-files files))
    (when (if focus
              (mercit-log-goto-commit-section focus)
            (mercit-log-goto-same-commit))
      (mercit-section-update-highlight))
    (current-buffer)))

(defun mercit-log-refresh-buffer ()
  (let ((revs  mercit-buffer-revisions)
        (args  mercit-buffer-log-args)
        (files mercit-buffer-log-files))
    (mercit-set-header-line-format
     (funcall mercit-log-header-line-function revs args files))
    (unless (length= files 1)
      (setq args (remove "--follow" args)))
    (when (and (car mercit-log-remove-graph-args)
               (--any-p (string-match-p
                         (concat "^" (regexp-opt mercit-log-remove-graph-args)) it)
                        args))
      (setq args (remove "--graph" args)))
    (unless (member "--graph" args)
      (setq args (remove "--color" args)))
    (when-let* ((limit (mercit-log-get-commit-limit))
                (limit (* 2 limit)) ; increase odds for complete graph
                (count (and (length= revs 1)
                            (> limit 1024) ; otherwise it's fast enough
                            (setq revs (car revs))
                            (not (string-search ".." revs))
                            (not (string-search "::" revs))
                            (not (member revs '("--all" "--branches"))) ;; TODO
                            (-none-p (lambda (arg)
                                       (--any-p
                                        (string-prefix-p it arg)
                                        mercit-log-disable-graph-hack-args))
                                     args)
                            (mercit-git-string "rev-list" "--count"  ;; TODO
                                              "--first-parent" args revs))))
      (setq revs (if (< (string-to-number count) limit)
                    (mapcan (lambda (x) (format "..%s" x)) revs)  ;; TODO no format?
                   (format "%s~%s..%s" revs limit revs))))
    ;; FIXME: only if when-let does not trigger
    (setq revs (mapcan (lambda (x) (format "..%s" x)) revs))
    (mercit-insert-section (logbuf)
      (mercit-insert-log revs args files))))

(cl-defmethod mercit-buffer-value (&context (major-mode mercit-log-mode))
  (append mercit-buffer-revisions
          (if (and mercit-buffer-revisions mercit-buffer-log-files)
              (cons "--" mercit-buffer-log-files)
            mercit-buffer-log-files)))

(defun mercit-log-header-line-arguments (revs args files)
  "Return string describing some of the used arguments."
  (mapconcat (lambda (arg)
               (if (string-search " " arg)
                 arg))
             `("hg" "log" ,@args ,@revs "--" ,@files)
             " "))

(defun mercit-log-header-line-sentence (revs args files)
  "Return string containing all arguments."
  (concat "Commits in "
          (mapconcat #'identity revs " ")
          (and (member "--reverse" args)
               " in reverse")
          (and files (concat " touching "
                             (mapconcat #'identity files " ")))
          (--some (and (string-prefix-p "-L" it)
                       (concat " " it))
                  args)))

(defun mercit-insert-log (revs &optional args files)
  "Insert a log section.
Do not add this to a hook variable."
  (let ((mercit-git-global-arguments
         (remove "--literal-pathspecs" mercit-git-global-arguments)))
    (mercit-git-wash (apply-partially #'mercit-log-wash-log 'log)
      "log"
      "--template"  ;; TODO refine template
      (concat
       (if (and (member "--left-right" args)
                (not (member "--graph" args)))
           "%m " ;; left (<), right (>) or boundary (-) mark  TODO
         "")
       "{node|short}"
       (if (member "--decorate" args)
           (concat "{ifcontains(rev, revset('.'), "
                   "   ifcontains(rev, revset('head()'), 'HEAD -> ', 'HEAD,'))}"
                   "{ifcontains(rev, revset('head()'), " ;; branch/topic heads only
                   "   indent(if(topic, indent(topic, 'topic/') , branch),"
                   "          'refs/heads/'))}"
                   "{if(tags, indent(join(tags), 'tag: refs/tags/', ', tag: refs/tags/'))}"
                   ))
       ""
       ;; ref names without the " (", ")" wrapping.
       (if (member "--show-signature" args)
           (progn (setq args (remove "--show-signature" args)) "%G?")  ;; TODO
         "")
       "{author|person}{date(date, '%s')}{desc|firstline}"
       (if (member "++header" args)
           (if (member "--graph" (setq args (remove "++header" args)))
               (concat "\n" mercit-log-revision-headers-format "\n")
             (concat "\n" mercit-log-revision-headers-format "\n"))
         "\n"))
      (progn
        ;; (--when-let (--first (string-match "^\\+\\+order=\\(.+\\)$" it) args)
        ;;   (setq args (cons (format "--%s-order" (match-string 1 it))  ;; TODO
        ;;                    (remove it args))))
        (when (member "--decorate" args)
          (setq args (remove "--decorate" args)))
        (when (member "--reverse" args)
          (setq args (remove "--graph" args)))
        (setq args (mercit-diff--maybe-add-stat-arguments args))
        args)
      ;; TODO "--use-mailmap" function: mailmap(author)
      "--rev" revs "--" files)))  ;; TODO handel several revs

(cl-defmethod mercit-menu-common-value ((_section mercit-commit-section))
  (or (mercit-diff--region-range)
      (oref (mercit-current-section) value)))

(defvar mercit-commit-section-map
  (let ((map (make-sparse-keymap)))
    ;; The second remapping overrides the first but we still get two menu
    ;; items, though only one of them will be available at any given time.
    (mercit-menu-set map [mercit-visit-thing]
      #'mercit-diff-range   "Diff %x"
      '(:visible (region-active-p)))
    (mercit-menu-set map [mercit-visit-thing]
      #'mercit-show-commit  "Show commit %x"
      '(:visible (not (region-active-p))))
    (mercit-menu-set map [mercit-cherry-apply]
      #'mercit-cherry-apply "Apply %x")
    map)
  "Keymap for `commit' sections.")

(defvar mercit-module-commit-section-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mercit-commit-section-map)
    map)
  "Keymap for `module-commit' sections.")

(defconst mercit-log-heading-re
  ;; Note: A form feed instead of a null byte is used as the delimiter
  ;; because using the latter interferes with the graph prefix when
  ;; ++header is used.
  (concat "^"
          "\\(?4:[-_/|\\*o<>. ]*\\)"               ; graph
          "\\(?1:[0-9a-fA-F]+\\)?"               ; hash
          "\\(?3:[^\n]+\\)?"                   ; refs (branch, topic, tags)
          "\\(?7:[BGUXYREN]\\)?"                 ; gpg
          "\\(?5:[^\n]*\\)"                    ; author
          "\\(?6:[^\n]*\\)"                    ; date
          "\\(?2:.*\\)$"))                         ; msg

(defconst mercit-log-cherry-re
  (concat "^"
          "\\(?8:[-+]\\) "                         ; cherry
          "\\(?1:[0-9a-fA-F]+\\) "                 ; hash
          "\\(?2:.*\\)$"))                         ; msg

(defconst mercit-log-module-re
  (concat "^"
          "\\(?:\\(?11:[<>]\\) \\)?"               ; side
          "\\(?1:[0-9a-fA-F]+\\) "                 ; hash
          "\\(?2:.*\\)$"))                         ; msg

(defconst mercit-log-bisect-vis-re
  (concat "^"
          "\\(?4:[-_/|\\*o<>. ]*\\)"               ; graph
          "\\(?1:[0-9a-fA-F]+\\)?\0"               ; hash
          "\\(?3:[^\0\n]+\\)?\0"                   ; refs
          "\\(?2:.*\\)$"))                         ; msg

(defconst mercit-log-bisect-log-re
  (concat "^# "
          "\\(?3:[^: \n]+:\\) "                    ; "refs"
          "\\[\\(?1:[^]\n]+\\)\\] "                ; hash
          "\\(?2:.*\\)$"))                         ; msg

(defconst mercit-log-reflog-re
  (concat "^"
          "\\(?1:[^\0\n]+\\)\0"                    ; hash
          "\\(?5:[^\0\n]*\\)\0"                    ; author
          "\\(?:\\(?:[^@\n]+@{\\(?6:[^}\n]+\\)}\0" ; date
                                                 ;;; refsub
          "\\(?10:merge \\|autosave \\|restart \\|rewritten \\|[^:\n]+: \\)?"
          "\\(?2:.*\\)?\\)\\|\0\\)$"))             ; msg

(defconst mercit-reflog-subject-re
  (concat "\\(?1:[^ ]+\\) ?"                       ; command
          "\\(?2:\\(?: ?-[^ ]+\\)+\\)?"            ; option
          "\\(?: ?(\\(?3:[^)]+\\))\\)?"))          ; type

(defconst mercit-log-stash-re
  (concat "^"
          "\\(?1:[^\0\n]+\\)\0"                    ; "hash"
          "\\(?5:[^\0\n]*\\)\0"                    ; author
          "\\(?6:[^\0\n]+\\)\0"                    ; date
          "\\(?2:.*\\)$"))                         ; msg

(defvar mercit-log-count nil)

(defvar mercit-log-format-message-function #'mercit-log-propertize-keywords)

(defun mercit-log-wash-log (style args)
  (setq args (flatten-tree args))
  (when (and (member "--graph" args)
             (member "--color" args))
    (let ((ansi-color-apply-face-function
           (lambda (beg end face)
             (put-text-property beg end 'font-lock-face
                                (or face 'mercit-log-graph)))))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (when (eq style 'cherry)
    (reverse-region (point-min) (point-max)))
  (let ((mercit-log-count 0))
    (when (looking-at "^\\.\\.\\.")
      (mercit-delete-line))
    (mercit-wash-sequence (apply-partially #'mercit-log-wash-rev style
                                          (mercit-abbrev-length)))
    (if (derived-mode-p 'mercit-log-mode 'mercit-reflog-mode)
        (when (eq mercit-log-count (mercit-log-get-commit-limit))
          (mercit-insert-section (longer)
            (insert-text-button
             (substitute-command-keys
              (format "Type \\<%s>\\[%s] to show more history"
                      'mercit-log-mode-map
                      'mercit-log-double-commit-limit))
             'action (lambda (_button)
                       (mercit-log-double-commit-limit))
             'follow-link t
             'mouse-face 'mercit-section-highlight)))
      (insert ?\n))))

(cl-defun mercit-log-wash-rev (style abbrev)
  (when (derived-mode-p 'mercit-log-mode 'mercit-reflog-mode)
    (cl-incf mercit-log-count))
  (looking-at (pcase style
                ('log        mercit-log-heading-re)
                ('cherry     mercit-log-cherry-re)
                ('module     mercit-log-module-re)
                ('reflog     mercit-log-reflog-re)
                ('stash      mercit-log-stash-re)
                ('bisect-vis mercit-log-bisect-vis-re)
                ('bisect-log mercit-log-bisect-log-re)))
  (mercit-bind-match-strings
      (hash msg refs graph author date gpg cherry _ refsub side) nil
    (setq msg (substring-no-properties msg))
    (when refs
      (setq refs (substring-no-properties refs)))
    (let ((align (or (eq style 'cherry)
                     (not (member "--stat" mercit-buffer-log-args))))
          (non-graph-re (if (eq style 'bisect-vis)
                            mercit-log-bisect-vis-re
                          mercit-log-heading-re)))
      (mercit-delete-line)
      ;; If the reflog entries have been pruned, the output of `git
      ;; reflog show' includes a partial line that refers to the hash
      ;; of the youngest expired reflog entry.
      (when (and (eq style 'reflog) (not date))
        (cl-return-from mercit-log-wash-rev t))
      (mercit-insert-section section (commit hash)
        (pcase style
          ('stash      (oset section type 'stash))
          ('module     (oset section type 'module-commit))
          ('bisect-log (setq hash (mercit-rev-parse "--short" hash))))
        (setq hash (propertize hash 'font-lock-face
                               (pcase (and gpg (aref gpg 0))
                                 (?G 'mercit-signature-good)
                                 (?B 'mercit-signature-bad)
                                 (?U 'mercit-signature-untrusted)
                                 (?X 'mercit-signature-expired)
                                 (?Y 'mercit-signature-expired-key)
                                 (?R 'mercit-signature-revoked)
                                 (?E 'mercit-signature-error)
                                 (?N 'mercit-hash)
                                 (_  'mercit-hash))))
        (when cherry
          (when (and (derived-mode-p 'mercit-refs-mode)
                     mercit-refs-show-commit-count)
            (insert (make-string (1- mercit-refs-focus-column-width) ?\s)))
          (insert (propertize cherry 'font-lock-face
                              (if (string= cherry "-")
                                  'mercit-cherry-equivalent
                                'mercit-cherry-unmatched)))
          (insert ?\s))
        (when side
          (insert (propertize side 'font-lock-face
                              (if (string= side "<")
                                  'mercit-cherry-equivalent
                                'mercit-cherry-unmatched)))
          (insert ?\s))
        (when align
          (insert hash ?\s))
        (when graph
          (insert graph))
        (unless align
          (insert hash ?\s))
        (when (and refs (not mercit-log-show-refname-after-summary))
          (insert (mercit-format-ref-labels refs) ?\s))
        (when (eq style 'reflog)
          (insert (format "%-2s " (1- mercit-log-count)))
          (when refsub
            (insert (mercit-reflog-format-subject
                     (substring refsub 0
                                (if (string-search ":" refsub) -2 -1))))))
        (when msg
          (insert (funcall mercit-log-format-message-function hash msg)))
        (when (and refs mercit-log-show-refname-after-summary)
          (insert ?\s)
          (insert (mercit-format-ref-labels refs)))
        (insert ?\n)
        (when (memq style '(log reflog stash))
          (goto-char (line-beginning-position))
          (when (and refsub
                     (string-match "\\`\\([^ ]\\) \\+\\(..\\)\\(..\\)" date))
            (setq date (+ (string-to-number (match-string 1 date))
                          (* (string-to-number (match-string 2 date)) 60 60)
                          (* (string-to-number (match-string 3 date)) 60))))
          (save-excursion
            (backward-char)
            (mercit-log-format-margin hash author date)))
        (when (and (eq style 'cherry)
                   (mercit-buffer-margin-p))
          (save-excursion
            (backward-char)
            (apply #'mercit-log-format-margin hash
                   (split-string
                    (mercit-rev-format "{author|person}\0{date(date, '%s')}" hash)
                    "\0"))))
        (when (and graph
                   (not (eobp))
                   (not (looking-at non-graph-re)))
          (when (looking-at "")
            (mercit-insert-heading)
            (delete-char 1)
            (mercit-insert-section (commit-header)
              (forward-line)
              (mercit-insert-heading)
              (re-search-forward "")
              (backward-delete-char 1)
              (forward-char)
              (insert ?\n))
            (delete-char 1))
          (if (looking-at "^\\(---\\|\n\s\\|\ndiff\\)")
              (let ((limit (save-excursion
                             (and (re-search-forward non-graph-re nil t)
                                  (match-beginning 0)))))
                (unless (oref mercit-insert-section--current content)
                  (mercit-insert-heading))
                (delete-char (if (looking-at "\n") 1 4))
                (mercit-diff-wash-diffs (list "--stat") limit))
            (when align
              (setq align (make-string (1+ abbrev) ? )))
            (when (and (not (eobp)) (not (looking-at non-graph-re)))
              (when align
                (setq align (make-string (1+ abbrev) ? )))
              (while (and (not (eobp)) (not (looking-at non-graph-re)))
                (when align
                  (save-excursion (insert align)))
                (mercit-make-margin-overlay)
                (forward-line))
              ;; When `--format' is used and its value isn't one of the
              ;; predefined formats, then `git-log' does not insert a
              ;; separator line.
              (save-excursion
                (forward-line -1)
                (looking-at "[-_/|\\*o<>. ]*"))
              (setq graph (match-string 0))
              (unless (string-match-p "[/\\.]" graph)
                (insert graph ?\n))))))))
  t)

(defun mercit-log-propertize-keywords (_rev msg)
  (let ((boundary 0))
    (when (string-match "^\\(?:squash\\|fixup\\)! " msg boundary)
      (setq boundary (match-end 0))
      (mercit--put-face (match-beginning 0) (1- boundary)
                       'mercit-keyword-squash msg))
    (when mercit-log-highlight-keywords
      (while (string-match "\\[[^[]*?]" msg boundary)
        (setq boundary (match-end 0))
        (mercit--put-face (match-beginning 0) boundary
                         'mercit-keyword msg))))
  msg)

(defun mercit-log-maybe-show-more-commits (section)
  "When point is at the end of a log buffer, insert more commits.

Log buffers end with a button \"Type + to show more history\".
When the use of a section movement command puts point on that
button, then automatically show more commits, without the user
having to press \"+\".

This function is called by `mercit-section-movement-hook' and
exists mostly for backward compatibility reasons."
  (when (and (eq (oref section type) 'longer)
             mercit-log-auto-more)
    (mercit-log-double-commit-limit)
    (forward-line -1)
    (mercit-section-forward)))

(add-hook 'mercit-section-movement-hook #'mercit-log-maybe-show-more-commits)

(defvar mercit--update-revision-buffer nil)

(defun mercit-log-maybe-update-revision-buffer (&optional _)
  "When moving in a log or cherry buffer, update the revision buffer.
If there is no revision buffer in the same frame, then do nothing."
  (when (derived-mode-p 'mercit-log-mode 'mercit-cherry-mode 'mercit-reflog-mode)
    (mercit--maybe-update-revision-buffer)))

(add-hook 'mercit-section-movement-hook #'mercit-log-maybe-update-revision-buffer)

(defun mercit--maybe-update-revision-buffer ()
  (when-let* ((commit (mercit-section-value-if 'commit))
              (buffer (mercit-get-mode-buffer 'mercit-revision-mode nil t)))
    (if mercit--update-revision-buffer
        (setq mercit--update-revision-buffer (list commit buffer))
      (setq mercit--update-revision-buffer (list commit buffer))
      (run-with-idle-timer
       mercit-update-other-window-delay nil
       (let ((args (let ((mercit-direct-use-buffer-arguments 'selected))
                     (mercit-show-commit--arguments))))
         (lambda ()
           (pcase-let ((`(,rev ,buf) mercit--update-revision-buffer))
             (setq mercit--update-revision-buffer nil)
             (when (buffer-live-p buf)
               (let ((mercit-display-buffer-noselect t))
                 (apply #'mercit-show-commit rev args))))
           (setq mercit--update-revision-buffer nil)))))))

(defvar mercit--update-blob-buffer nil)

(defun mercit-log-maybe-update-blob-buffer (&optional _)
  "When moving in a log or cherry buffer, update the blob buffer.
If there is no blob buffer in the same frame, then do nothing."
  (when (derived-mode-p 'mercit-log-mode 'mercit-cherry-mode 'mercit-reflog-mode)
    (mercit--maybe-update-blob-buffer)))

(defun mercit--maybe-update-blob-buffer ()
  (when-let* ((commit (mercit-section-value-if 'commit))
              (buffer (--first (with-current-buffer it
                                 (eq revert-buffer-function
                                     'mercit-revert-rev-file-buffer))
                               (mapcar #'window-buffer (window-list)))))
    (if mercit--update-blob-buffer
        (setq mercit--update-blob-buffer (list commit buffer))
      (setq mercit--update-blob-buffer (list commit buffer))
      (run-with-idle-timer
       mercit-update-other-window-delay nil
       (lambda ()
         (pcase-let ((`(,rev ,buf) mercit--update-blob-buffer))
           (setq mercit--update-blob-buffer nil)
           (when (buffer-live-p buf)
             (with-selected-window (get-buffer-window buf)
               (with-current-buffer buf
                 (save-excursion
                   (mercit-blob-visit (list (mercit-rev-parse rev)
                                           (mercit-file-relative-name
                                            mercit-buffer-file-name)))))))))))))

(defun mercit-log-goto-commit-section (rev)
  (let ((abbrev (mercit-rev-format "{node|short}" rev)))
    (when-let ((section (--first (equal (oref it value) abbrev)
                                 (oref mercit-root-section children))))
      (goto-char (oref section start)))))

(defun mercit-log-goto-same-commit ()
  (when (and mercit-previous-section
             (mercit-section-match '(commit branch)
                                  mercit-previous-section))
    (mercit-log-goto-commit-section (oref mercit-previous-section value))))

;;; Log Margin

(defvar-local mercit-log-margin-show-shortstat nil)

(defun mercit-toggle-log-margin-style ()
  "Toggle between the regular and the shortstat margin style.
The shortstat style is experimental and rather slow."
  (interactive)
  (setq mercit-log-margin-show-shortstat
        (not mercit-log-margin-show-shortstat))
  (mercit-set-buffer-margin nil t))

(defun mercit-log-format-margin (rev author date)
  (when (mercit-margin-option)
    (if mercit-log-margin-show-shortstat
        (mercit-log-format-shortstat-margin rev)
      (mercit-log-format-author-margin author date))))

(defun mercit-log-format-author-margin (author date &optional previous-line)
  (pcase-let ((`(,_ ,style ,width ,details ,details-width)
               (or mercit-buffer-margin
                   (symbol-value (mercit-margin-option)))))
    (mercit-make-margin-overlay
     (concat (and details
                  (concat (mercit--propertize-face
                           (truncate-string-to-width
                            (or author "")
                            details-width
                            nil ?\s
                            (if (char-displayable-p ?…) "…" ">"))
                           'mercit-log-author)
                          " "))
             (mercit--propertize-face
              (if (stringp style)
                  (format-time-string
                   style
                   (seconds-to-time (string-to-number date)))
                (pcase-let* ((abbr (eq style 'age-abbreviated))
                             (`(,cnt ,unit) (mercit--age date abbr)))
                  (format (format (if abbr "%%2i%%-%ic" "%%2i %%-%is")
                                  (- width (if details (1+ details-width) 0)))
                          cnt unit)))
              'mercit-log-date))
     previous-line)))

(defun mercit-log-format-shortstat-margin (rev)
  (mercit-make-margin-overlay
   (if-let ((line (and rev (mercit-git-string
                            "show" "--format=" "--shortstat" rev))))
       (if (string-match "\
\\([0-9]+\\) files? changed, \
\\(?:\\([0-9]+\\) insertions?(\\+)\\)?\
\\(?:\\(?:, \\)?\\([0-9]+\\) deletions?(-)\\)?\\'" line)
           (mercit-bind-match-strings (files add del) line
             (format
              "%5s %5s%4s"
              (if add
                  (mercit--propertize-face (format "%s+" add)
                                          'mercit-diffstat-added)
                "")
              (if del
                  (mercit--propertize-face (format "%s-" del)
                                          'mercit-diffstat-removed)
                "")
              files))
         "")
     "")))

(defun mercit-log-margin-width (style details details-width)
  (if mercit-log-margin-show-shortstat
      16
    (+ (if details (1+ details-width) 0)
       (if (stringp style)
           (length (format-time-string style))
         (+ 2 ; two digits
            1 ; trailing space
            (if (eq style 'age-abbreviated)
                1  ; single character
              (+ 1 ; gap after digits
                 (apply #'max (--map (max (length (nth 1 it))
                                          (length (nth 2 it)))
                                     mercit--age-spec)))))))))

;;; Select Mode

(defvar mercit-log-select-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mercit-log-mode-map)
    (define-key map (kbd "C-c C-b") #'undefined)
    (define-key map (kbd "C-c C-f") #'undefined)
    (define-key map (kbd ".")       #'mercit-log-select-pick)
    (define-key map (kbd "e")       #'mercit-log-select-pick)
    (define-key map (kbd "C-c C-c") #'mercit-log-select-pick)
    (define-key map (kbd "q")       #'mercit-log-select-quit)
    (define-key map (kbd "C-c C-k") #'mercit-log-select-quit)
    map)
  "Keymap for `mercit-log-select-mode'.")

(put 'mercit-log-select-pick :advertised-binding [?\C-c ?\C-c])
(put 'mercit-log-select-quit :advertised-binding [?\C-c ?\C-k])

(define-derived-mode mercit-log-select-mode mercit-log-mode "Mercit Select"
  "Mode for selecting a commit from history.

This mode is documented in info node `(mercit)Select from Log'.

\\<mercit-mode-map>\
Type \\[mercit-refresh] to refresh the current buffer.
Type \\[mercit-visit-thing] or \\[mercit-diff-show-or-scroll-up] \
to visit the commit at point.

\\<mercit-log-select-mode-map>\
Type \\[mercit-log-select-pick] to select the commit at point.
Type \\[mercit-log-select-quit] to abort without selecting a commit."
  :group 'mercit-log
  (hack-dir-local-variables-non-file-buffer))

(put 'mercit-log-select-mode 'mercit-log-default-arguments
     '("--graph" "--decorate" "--limit=256"))

(defun mercit-log-select-setup-buffer (revs args)
  (mercit-setup-buffer #'mercit-log-select-mode nil
    (mercit-buffer-revisions revs)
    (mercit-buffer-log-args args)))

(defun mercit-log-select-refresh-buffer ()
  (mercit-insert-section (logbuf)
    (mercit-insert-log mercit-buffer-revisions
                      mercit-buffer-log-args)))

(cl-defmethod mercit-buffer-value (&context (major-mode mercit-log-select-mode))
  mercit-buffer-revisions)

(defvar-local mercit-log-select-pick-function nil)
(defvar-local mercit-log-select-quit-function nil)

(defun mercit-log-select (pick &optional msg quit branch args initial)
  (declare (indent defun))
  (unless initial
    (setq initial (mercit-commit-at-point)))
  (mercit-log-select-setup-buffer
   (or branch (mercit-get-current-branch) "--rev=.")
   (append args
           (car (mercit-log--get-value 'mercit-log-select-mode
                                      mercit-direct-use-buffer-arguments))))
  (when initial
    (mercit-log-goto-commit-section initial))
  (setq mercit-log-select-pick-function pick)
  (setq mercit-log-select-quit-function quit)
  (when mercit-log-select-show-usage
    (let ((pick (propertize (substitute-command-keys
                             "\\[mercit-log-select-pick]")
                            'font-lock-face
                            'mercit-header-line-key))
          (quit (propertize (substitute-command-keys
                             "\\[mercit-log-select-quit]")
                            'font-lock-face
                            'mercit-header-line-key)))
      (setq msg (format-spec
                 (if msg
                     (if (string-suffix-p "," msg)
                         (concat msg " or %q to abort")
                       msg)
                   "Type %p to select commit at point, or %q to abort")
                 `((?p . ,pick)
                   (?q . ,quit)))))
    (mercit--add-face-text-property
     0 (length msg) 'mercit-header-line-log-select t msg)
    (when (memq mercit-log-select-show-usage '(both header-line))
      (mercit-set-header-line-format msg))
    (when (memq mercit-log-select-show-usage '(both echo-area))
      (message "%s" (substring-no-properties msg)))))

(defun mercit-log-select-pick ()
  "Select the commit at point and act on it.
Call `mercit-log-select-pick-function' with the selected
commit as argument."
  (interactive)
  (let ((fun mercit-log-select-pick-function)
        (rev (mercit-commit-at-point)))
    (mercit-mode-bury-buffer 'kill)
    (funcall fun rev)))

(defun mercit-log-select-quit ()
  "Abort selecting a commit, don't act on any commit.
Call `mercit-log-select-quit-function' if set."
  (interactive)
  (let ((fun mercit-log-select-quit-function))
    (mercit-mode-bury-buffer 'kill)
    (when fun (funcall fun))))

;;; Cherry Mode

(defvar mercit-cherry-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mercit-mode-map)
    (define-key map "q" #'mercit-log-bury-buffer)
    (define-key map "L" #'mercit-margin-settings)
    map)
  "Keymap for `mercit-cherry-mode'.")

(define-derived-mode mercit-cherry-mode mercit-mode "Mercit Cherry"
  "Mode for looking at commits not merged upstream.

\\<mercit-mode-map>\
Type \\[mercit-refresh] to refresh the current buffer.
Type \\[mercit-visit-thing] or \\[mercit-diff-show-or-scroll-up] \
to visit the commit at point.

Type \\[mercit-cherry-pick] to apply the commit at point.

\\{mercit-cherry-mode-map}"
  :group 'mercit-log
  (hack-dir-local-variables-non-file-buffer)
  (setq mercit--imenu-group-types 'cherries))

(defun mercit-cherry-setup-buffer (head upstream)
  (mercit-setup-buffer #'mercit-cherry-mode nil
    (mercit-buffer-refname head)
    (mercit-buffer-upstream upstream)
    (mercit-buffer-range (concat upstream ".." head))))

(defun mercit-cherry-refresh-buffer ()
  (mercit-insert-section (cherry)
    (mercit-run-section-hook 'mercit-cherry-sections-hook)))

(cl-defmethod mercit-buffer-value (&context (major-mode mercit-cherry-mode))
  mercit-buffer-range)

;;;###autoload
(defun mercit-cherry (head upstream)
  "Show commits in a branch that are not merged in the upstream branch."
  (interactive
   (let  ((head (mercit-read-branch "Cherry head")))
     (list head (mercit-read-other-branch "Cherry upstream" head
                                         (mercit-get-upstream-branch head)))))
  (require 'mercit)
  (mercit-cherry-setup-buffer head upstream))

(defun mercit-insert-cherry-headers ()
  "Insert headers appropriate for `mercit-cherry-mode' buffers."
  (let ((branch (propertize mercit-buffer-refname
                            'font-lock-face 'mercit-branch-local))
        (upstream (propertize mercit-buffer-upstream 'font-lock-face
                              (if (mercit-local-branch-p mercit-buffer-upstream)
                                  'mercit-branch-local
                                'mercit-branch-remote))))
    (mercit-insert-head-branch-header branch)
    (mercit-insert-upstream-branch-header branch upstream "Upstream: ")
    (insert ?\n)))

(defun mercit-insert-cherry-commits ()
  "Insert commit sections into a `mercit-cherry-mode' buffer."
  (mercit-insert-section (cherries)
    (mercit-insert-heading "Cherry commits:")
    (mercit-git-wash (apply-partially #'mercit-log-wash-log 'cherry)
      "cherry" "-v" "--abbrev"
      mercit-buffer-upstream
      mercit-buffer-refname)))

;;; Log Sections
;;;; Standard Log Sections

(defvar mercit-log-section-map
  (let ((map (make-sparse-keymap)))
    (mercit-menu-set map [mercit-visit-thing] #'mercit-diff-dwim "Visit diff")
    map)
  "Keymap for log sections.
The classes `mercit-{unpulled,unpushed,unmerged}-section' derive
from the abstract `mercit-log-section' class.  Accordingly this
keymap is the parent of their keymaps.")

(defvar mercit-unpulled-section-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mercit-log-section-map)
    map)
  "Keymap for `unpulled' sections.")

(cl-defmethod mercit-section-ident-value ((section mercit-unpulled-section))
  "\"..@{push}\" cannot be used as the value because that is
ambiguous if `push.default' does not allow a 1:1 mapping, and
many commands would fail because of that.  But here that does
not matter and we need an unique value so we use that string
in the pushremote case."
  (let ((value (oref section value)))
    (if (equal value "..@{upstream}") value "..@{push}")))

(mercit-define-section-jumper mercit-jump-to-unpulled-from-upstream
  "Unpulled from @{upstream}" unpulled "..@{upstream}")

(defun mercit-insert-unpulled-from-upstream ()
  "Insert commits that haven't been pulled from the upstream yet."
  (when-let ((upstream (mercit-get-upstream-branch)))
    (mercit-insert-section (unpulled (concat "--rev=" upstream ":. "
                                             "and not 000000000000")
                                     t)
      (mercit-insert-heading
       (format (propertize "Unpulled from %s."  ;; TODO: name of remote repo?
                           'font-lock-face 'mercit-section-heading)
               upstream))
      (mercit-insert-log (concat ".:" upstream)
                         mercit-buffer-log-args)
      (mercit-log-insert-child-count))))

(mercit-define-section-jumper mercit-jump-to-unpulled-from-pushremote
  "Unpulled from <push-remote>" unpulled
  (concat ".." (mercit-get-push-branch)))

(defun mercit-insert-unpulled-from-pushremote ()  ;; TODO
  "Insert commits that haven't been pulled from the push-remote yet."
  (--when-let (mercit-get-push-branch)
    (when (mercit--insert-pushremote-log-p)
      (mercit-insert-section (unpulled (concat ".." it) t)
        (mercit-insert-heading
          (format (propertize "Unpulled from %s."  ;; TODO: name of remote repo?
                              'font-lock-face 'mercit-section-heading)
                  (propertize it 'font-lock-face 'mercit-branch-remote)))
        (mercit-insert-log (concat ".." it) mercit-buffer-log-args)
        (mercit-log-insert-child-count)))))

(defvar mercit-unpushed-section-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mercit-log-section-map)
    map)
  "Keymap for `unpushed' sections.")

(cl-defmethod mercit-section-ident-value ((section mercit-unpushed-section))
  "\"..@{push}\" cannot be used as the value because that is
ambiguous if `push.default' does not allow a 1:1 mapping, and
many commands would fail because of that.  But here that does
not matter and we need an unique value so we use that string
in the pushremote case."
  (let ((value (oref section value)))
    (if (equal value "@{upstream}..") value "@{push}..")))  ;; TODO

(mercit-define-section-jumper mercit-jump-to-unpushed-to-upstream
  "Unpushed to @{upstream}" unpushed "@{upstream}..")

(defun mercit-insert-unpushed-to-upstream-or-recent ()  ;; TODO
  "Insert section showing unpushed or other recent commits.
If an upstream is configured for the current branch and it is
behind of the current branch, then show the commits that have
not yet been pushed into the upstream branch.  If no upstream is
configured or if the upstream is not behind of the current branch,
then show the last `mercit-log-section-commit-count' commits."
  (let ((upstream (mercit-get-upstream-branch)))
    (if (or (not upstream)
            (mercit-rev-ancestor-p "--rev=." upstream))
        (mercit-insert-recent-commits 'unpushed "@{upstream}..")
      (mercit-insert-unpushed-to-upstream))))

(defun mercit-insert-unpushed-to-upstream ()
  "Insert commits that haven't been pushed to the upstream yet."
  (when (mercit-git-success "rev-parse" "@{upstream}")
    (mercit-insert-section (unpushed "@{upstream}..")
      (mercit-insert-heading
        (format (propertize "Unmerged into %s."
                            'font-lock-face 'mercit-section-heading)
                (mercit-get-upstream-branch)))
      (mercit-insert-log "@{upstream}.." mercit-buffer-log-args)
      (mercit-log-insert-child-count))))

(defun mercit-insert-recent-commits (&optional type value)  ;; TODO refine
  "Insert section showing recent commits.
Show the last `mercit-log-section-commit-count' commits."
  (let* ((start (format "-%s" mercit-log-section-commit-count))
         (range (and (mercit-rev-verify start)
                     (concat "--rev=.:" start))))
    (mercit-insert-section ((eval (or type 'recent))
                           (or value range)
                           t)
      (mercit-insert-heading "Recent commits")
      (mercit-insert-log range
                        (cons (format "--limit=%d" mercit-log-section-commit-count)
                              (--remove (string-prefix-p "--limit=" it)
                                        mercit-buffer-log-args))))))

(mercit-define-section-jumper mercit-jump-to-unpushed-to-pushremote ;; TODO
  "Unpushed to <push-remote>" unpushed
  (concat (mercit-get-push-branch) ":"))

(defun mercit-insert-unpushed-to-pushremote () ;; TODO
  "Insert commits that haven't been pushed to the push-remote yet."
  (--when-let (mercit-get-push-branch)
    (when (mercit--insert-pushremote-log-p)
      (mercit-insert-section (unpushed (concat it "..") t)
        (mercit-insert-heading
          (format (propertize "Unpushed to %s."
                              'font-lock-face 'mercit-section-heading)
                  (propertize it 'font-lock-face 'mercit-branch-remote)))
        (mercit-insert-log (concat it "..") mercit-buffer-log-args)
        (mercit-log-insert-child-count)))))

(defun mercit--insert-pushremote-log-p ()
  (mercit--with-refresh-cache
      (cons default-directory 'mercit--insert-pushremote-log-p)
    (not (and (equal (mercit-get-push-branch)
                     (mercit-get-upstream-branch))
              (or (memq 'mercit-insert-unpulled-from-upstream
                        mercit-status-sections-hook)
                  (memq 'mercit-insert-unpulled-from-upstream-or-recent
                        mercit-status-sections-hook))))))

(defun mercit-log-insert-child-count ()
  (when mercit-section-show-child-count
    (let ((count (length (oref mercit-insert-section--current children))))
      (when (> count 0)
        (when (eq count (mercit-log-get-commit-limit))
          (setq count (format "%s+" count)))
        (save-excursion
          (goto-char (- (oref mercit-insert-section--current content) 2))
          (insert (format " (%s)" count))
          (delete-char 1))))))

;;;; Auxiliary Log Sections

(defun mercit-insert-unpulled-cherries ()
  "Insert section showing unpulled commits.
Like `mercit-insert-unpulled-from-upstream' but prefix each commit
which has not been applied yet (i.e. a commit with a patch-id
not shared with any local commit) with \"+\", and all others with
\"-\"."
  (when (mercit-git-success "rev-parse" "@{upstream}")
    (mercit-insert-section (unpulled "..@{upstream}")
      (mercit-insert-heading "Unpulled commits:")
      (mercit-git-wash (apply-partially #'mercit-log-wash-log 'cherry)
        "cherry" "-v" (mercit-abbrev-arg)
        (mercit-get-current-branch) "@{upstream}"))))

(defun mercit-insert-unpushed-cherries ()
  "Insert section showing unpushed commits.
Like `mercit-insert-unpushed-to-upstream' but prefix each commit
which has not been applied to upstream yet (i.e. a commit with
a patch-id not shared with any upstream commit) with \"+\", and
all others with \"-\"."
  (when (mercit-git-success "rev-parse" "@{upstream}")
    (mercit-insert-section (unpushed "@{upstream}..")
      (mercit-insert-heading "Unpushed commits:")
      (mercit-git-wash (apply-partially #'mercit-log-wash-log 'cherry)
        "cherry" "-v" (mercit-abbrev-arg) "@{upstream}"))))

;;; _
(provide 'mercit-log)
;;; mercit-log.el ends here
