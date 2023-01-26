;;; mercit-submodule.el --- Submodule support for Mercit  -*- lexical-binding:t -*-

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

;;; Code:

(require 'mercit)

(defvar x-stretch-cursor)

;;; Options

(defcustom mercit-module-sections-hook
  '(mercit-insert-modules-overview
    mercit-insert-modules-unpulled-from-upstream
    mercit-insert-modules-unpulled-from-pushremote
    mercit-insert-modules-unpushed-to-upstream
    mercit-insert-modules-unpushed-to-pushremote)
  "Hook run by `mercit-insert-modules'.

That function isn't part of `mercit-status-sections-hook's default
value, so you have to add it yourself for this hook to have any
effect."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-status
  :type 'hook)

(defcustom mercit-module-sections-nested t
  "Whether `mercit-insert-modules' wraps inserted sections.

If this is non-nil, then only a single top-level section
is inserted.  If it is nil, then all sections listed in
`mercit-module-sections-hook' become top-level sections."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-status
  :type 'boolean)

(defcustom mercit-submodule-list-mode-hook '(hl-line-mode)
  "Hook run after entering Mercit-Submodule-List mode."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-repolist
  :type 'hook
  :get 'mercit-hook-custom-get
  :options '(hl-line-mode))

(defcustom mercit-submodule-list-columns
  '(("Path"     25 mercit-modulelist-column-path   nil)
    ("Version"  25 mercit-repolist-column-version
     ((:sort mercit-repolist-version<)))
    ("Branch"   20 mercit-repolist-column-branch   nil)
    ("B<U" 3 mercit-repolist-column-unpulled-from-upstream
     ((:right-align t)
      (:sort <)))
    ("B>U" 3 mercit-repolist-column-unpushed-to-upstream
     ((:right-align t)
      (:sort <)))
    ("B<P" 3 mercit-repolist-column-unpulled-from-pushremote
     ((:right-align t)
      (:sort <)))
    ("B>P" 3 mercit-repolist-column-unpushed-to-pushremote
     ((:right-align t)
      (:sort <)))
    ("B"   3 mercit-repolist-column-branches
     ((:right-align t)
      (:sort <)))
    ("S"   3 mercit-repolist-column-stashes
     ((:right-align t)
      (:sort <))))
  "List of columns displayed by `mercit-list-submodules'.

Each element has the form (HEADER WIDTH FORMAT PROPS).

HEADER is the string displayed in the header.  WIDTH is the width
of the column.  FORMAT is a function that is called with one
argument, the repository identification (usually its basename),
and with `default-directory' bound to the toplevel of its working
tree.  It has to return a string to be inserted or nil.  PROPS is
an alist that supports the keys `:right-align', `:pad-right' and
`:sort'.

The `:sort' function has a weird interface described in the
docstring of `tabulated-list--get-sort'.  Alternatively `<' and
`mercit-repolist-version<' can be used as those functions are
automatically replaced with functions that satisfy the interface.
Set `:sort' to nil to inhibit sorting; if unspecifed, then the
column is sortable using the default sorter.

You may wish to display a range of numeric columns using just one
character per column and without any padding between columns, in
which case you should use an appropriat HEADER, set WIDTH to 1,
and set `:pad-right' to 0.  \"+\" is substituted for numbers higher
than 9."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-repolist
  :type `(repeat (list :tag "Column"
                       (string   :tag "Header Label")
                       (integer  :tag "Column Width")
                       (function :tag "Inserter Function")
                       (repeat   :tag "Properties"
                                 (list (choice :tag "Property"
                                               (const :right-align)
                                               (const :pad-right)
                                               (const :sort)
                                               (symbol))
                                       (sexp   :tag "Value"))))))

(defcustom mercit-submodule-list-sort-key '("Path" . nil)
  "Initial sort key for buffer created by `mercit-list-submodules'.
If nil, no additional sorting is performed.  Otherwise, this
should be a cons cell (NAME . FLIP).  NAME is a string matching
one of the column names in `mercit-submodule-list-columns'.  FLIP,
if non-nil, means to invert the resulting sort."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-repolist
  :type '(choice (const nil)
                 (cons (string :tag "Column name")
                       (boolean :tag "Flip order"))))

(defvar mercit-submodule-list-format-path-functions nil)

(defcustom mercit-submodule-remove-trash-gitdirs nil
  "Whether `mercit-submodule-remove' offers to trash module gitdirs.

If this is nil, then that command does not offer to do so unless
a prefix argument is used.  When this is t, then it does offer to
do so even without a prefix argument.

In both cases the action still has to be confirmed unless that is
disabled using the option `mercit-no-confirm'.  Doing the latter
and also setting this variable to t will lead to tears."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-commands
  :type 'boolean)

;;; Popup

;;;###autoload (autoload 'mercit-submodule "mercit-submodule" nil t)
(transient-define-prefix mercit-submodule ()
  "Act on a submodule."
  :man-page "git-submodule"
  ["Arguments"
   ("-f" "Force"            ("-f" "--force"))
   ("-r" "Recursive"        "--recursive")
   ("-N" "Do not fetch"     ("-N" "--no-fetch"))
   ("-C" "Checkout tip"     "--checkout")
   ("-R" "Rebase onto tip"  "--rebase")
   ("-M" "Merge tip"        "--merge")
   ("-U" "Use upstream tip" "--remote")]
  ["One module actions"
   ("a" mercit-submodule-add)
   ("r" mercit-submodule-register)
   ("p" mercit-submodule-populate)
   ("u" mercit-submodule-update)
   ("s" mercit-submodule-synchronize)
   ("d" mercit-submodule-unpopulate)
   ("k" "Remove" mercit-submodule-remove)]
  ["All modules actions"
   ("l" "List all modules"  mercit-list-submodules)
   ("f" "Fetch all modules" mercit-fetch-modules)])

(defun mercit-submodule-arguments (&rest filters)
  (--filter (and (member it filters) it)
            (transient-args 'mercit-submodule)))

(defclass mercit--git-submodule-suffix (transient-suffix)
  ())

(cl-defmethod transient-format-description ((obj mercit--git-submodule-suffix))
  (let ((value (delq nil (mapcar #'transient-infix-value transient--suffixes))))
    (replace-regexp-in-string
     "\\[--[^]]+\\]"
     (lambda (match)
       (format (propertize "[%s]" 'face 'transient-inactive-argument)
               (mapconcat (lambda (arg)
                            (propertize arg 'face
                                        (if (member arg value)
                                            'transient-argument
                                          'transient-inactive-argument)))
                          (save-match-data
                            (split-string (substring match 1 -1) "|"))
                          (propertize "|" 'face 'transient-inactive-argument))))
     (cl-call-next-method obj))))

;;;###autoload (autoload 'mercit-submodule-add "mercit-submodule" nil t)
(transient-define-suffix mercit-submodule-add (url &optional path name args)
  "Add the repository at URL as a module.

Optional PATH is the path to the module relative to the root of
the superproject.  If it is nil, then the path is determined
based on the URL.  Optional NAME is the name of the module.  If
it is nil, then PATH also becomes the name."
  :class 'mercit--git-submodule-suffix
  :description "Add            git submodule add [--force]"
  (interactive
   (mercit-with-toplevel
     (let* ((url (mercit-read-string-ns "Add submodule (remote url)"))
            (path (let ((read-file-name-function
                         (if (or (eq read-file-name-function 'ido-read-file-name)
                                 (advice-function-member-p
                                  'ido-read-file-name
                                  read-file-name-function))
                             ;; The Ido variant doesn't work properly here.
                             #'read-file-name-default
                           read-file-name-function)))
                    (directory-file-name
                     (file-relative-name
                      (read-directory-name
                       "Add submodules at path: " nil nil nil
                       (and (string-match "\\([^./]+\\)\\(\\.git\\)?$" url)
                            (match-string 1 url))))))))
       (list url
             (directory-file-name path)
             (mercit-submodule-read-name-for-path path)
             (mercit-submodule-arguments "--force")))))
  (mercit-submodule-add-1 url path name args))

(defun mercit-submodule-add-1 (url &optional path name args)
  (mercit-with-toplevel
    (mercit-submodule--maybe-reuse-gitdir name path)
    (mercit-run-git-async "submodule" "add"
                         (and name (list "--name" name))
                         args "--" url path)
    (set-process-sentinel
     mercit-this-process
     (lambda (process event)
       (when (memq (process-status process) '(exit signal))
         (if (> (process-exit-status process) 0)
             (mercit-process-sentinel process event)
           (process-put process 'inhibit-refresh t)
           (mercit-process-sentinel process event)
           (when (mercit-git-version>= "2.12.0")
             (mercit-call-git "submodule" "absorbgitdirs" path))
           (mercit-refresh)))))))

;;;###autoload
(defun mercit-submodule-read-name-for-path (path &optional prefer-short)
  (let* ((path (directory-file-name (file-relative-name path)))
         (name (file-name-nondirectory path)))
    (push (if prefer-short path name) minibuffer-history)
    (mercit-read-string-ns
     "Submodule name" nil (cons 'minibuffer-history 2)
     (or (--keep (pcase-let ((`(,var ,val) (split-string it "=")))
                   (and (equal val path)
                        (cadr (split-string var "\\."))))
                 (mercit-git-lines "config" "--list" "-f" ".gitmodules"))
         (if prefer-short name path)))))

;;;###autoload (autoload 'mercit-submodule-register "mercit-submodule" nil t)
(transient-define-suffix mercit-submodule-register (modules)
  "Register MODULES.

With a prefix argument act on all suitable modules.  Otherwise,
if the region selects modules, then act on those.  Otherwise, if
there is a module at point, then act on that.  Otherwise read a
single module from the user."
  ;; This command and the underlying "git submodule init" do NOT
  ;; "initialize" modules.  They merely "register" modules in the
  ;; super-projects $GIT_DIR/config file, the purpose of which is to
  ;; allow users to change such values before actually initializing
  ;; the modules.
  :description "Register       git submodule init"
  (interactive
   (list (mercit-module-confirm "Register" 'mercit-module-no-worktree-p)))
  (mercit-with-toplevel
    (mercit-run-git-async "submodule" "init" "--" modules)))

;;;###autoload (autoload 'mercit-submodule-populate "mercit-submodule" nil t)
(transient-define-suffix mercit-submodule-populate (modules)
  "Create MODULES working directories, checking out the recorded commits.

With a prefix argument act on all suitable modules.  Otherwise,
if the region selects modules, then act on those.  Otherwise, if
there is a module at point, then act on that.  Otherwise read a
single module from the user."
  ;; This is the command that actually "initializes" modules.
  ;; A module is initialized when it has a working directory,
  ;; a gitlink, and a .gitmodules entry.
  :description "Populate       git submodule update --init"
  (interactive
   (list (mercit-module-confirm "Populate" 'mercit-module-no-worktree-p)))
  (mercit-with-toplevel
    (mercit-run-git-async "submodule" "update" "--init" "--" modules)))

;;;###autoload (autoload 'mercit-submodule-update "mercit-submodule" nil t)
(transient-define-suffix mercit-submodule-update (modules args)
  "Update MODULES by checking out the recorded commits.

With a prefix argument act on all suitable modules.  Otherwise,
if the region selects modules, then act on those.  Otherwise, if
there is a module at point, then act on that.  Otherwise read a
single module from the user."
  ;; Unlike `git-submodule's `update' command ours can only update
  ;; "initialized" modules by checking out other commits but not
  ;; "initialize" modules by creating the working directories.
  ;; To do the latter we provide the "setup" command.
  :class 'mercit--git-submodule-suffix
  :description "Update         git submodule update [--force] [--no-fetch]
                     [--remote] [--recursive] [--checkout|--rebase|--merge]"
  (interactive
   (list (mercit-module-confirm "Update" 'mercit-module-worktree-p)
         (mercit-submodule-arguments
          "--force" "--remote" "--recursive" "--checkout" "--rebase" "--merge"
          "--no-fetch")))
  (mercit-with-toplevel
    (mercit-run-git-async "submodule" "update" args "--" modules)))

;;;###autoload (autoload 'mercit-submodule-synchronize "mercit-submodule" nil t)
(transient-define-suffix mercit-submodule-synchronize (modules args)
  "Synchronize url configuration of MODULES.

With a prefix argument act on all suitable modules.  Otherwise,
if the region selects modules, then act on those.  Otherwise, if
there is a module at point, then act on that.  Otherwise read a
single module from the user."
  :class 'mercit--git-submodule-suffix
  :description "Synchronize    git submodule sync [--recursive]"
  (interactive
   (list (mercit-module-confirm "Synchronize" 'mercit-module-worktree-p)
         (mercit-submodule-arguments "--recursive")))
  (mercit-with-toplevel
    (mercit-run-git-async "submodule" "sync" args "--" modules)))

;;;###autoload (autoload 'mercit-submodule-unpopulate "mercit-submodule" nil t)
(transient-define-suffix mercit-submodule-unpopulate (modules args)
  "Remove working directories of MODULES.

With a prefix argument act on all suitable modules.  Otherwise,
if the region selects modules, then act on those.  Otherwise, if
there is a module at point, then act on that.  Otherwise read a
single module from the user."
  ;; Even though a package is "uninitialized" (it has no worktree)
  ;; the super-projects $GIT_DIR/config may never-the-less set the
  ;; module's url.  This may happen if you `deinit' and then `init'
  ;; to register (NOT initialize).  Because the purpose of `deinit'
  ;; is to remove the working directory AND to remove the url, this
  ;; command does not limit itself to modules that have no working
  ;; directory.
  :class 'mercit--git-submodule-suffix
  :description "Unpopulate     git submodule deinit [--force]"
  (interactive
   (list (mercit-module-confirm "Unpopulate")
         (mercit-submodule-arguments "--force")))
  (mercit-with-toplevel
    (mercit-run-git-async "submodule" "deinit" args "--" modules)))

;;;###autoload
(defun mercit-submodule-remove (modules args trash-gitdirs)
  "Unregister MODULES and remove their working directories.

For safety reasons, do not remove the gitdirs and if a module has
uncommitted changes, then do not remove it at all.  If a module's
gitdir is located inside the working directory, then move it into
the gitdir of the superproject first.

With the \"--force\" argument offer to remove dirty working
directories and with a prefix argument offer to delete gitdirs.
Both actions are very dangerous and have to be confirmed.  There
are additional safety precautions in place, so you might be able
to recover from making a mistake here, but don't count on it."
  (interactive
   (list (if-let ((modules (mercit-region-values 'mercit-module-section t)))
             (mercit-confirm 'remove-modules nil "Remove %i modules" nil modules)
           (list (mercit-read-module-path "Remove module")))
         (mercit-submodule-arguments "--force")
         current-prefix-arg))
  (when (mercit-git-version< "2.12.0")
    (error "This command requires Git v2.12.0"))
  (when mercit-submodule-remove-trash-gitdirs
    (setq trash-gitdirs t))
  (mercit-with-toplevel
    (when-let
        ((modified
          (-filter (lambda (module)
                     (let ((default-directory (file-name-as-directory
                                               (expand-file-name module))))
                       (and (cddr (directory-files default-directory))
                            (mercit-anything-modified-p))))
                   modules)))
      (if (member "--force" args)
          (if (mercit-confirm 'remove-dirty-modules
                "Remove dirty module %s"
                "Remove %i dirty modules"
                t modified)
              (dolist (module modified)
                (let ((default-directory (file-name-as-directory
                                          (expand-file-name module))))
                  (mercit-git "stash" "push"
                             "-m" "backup before removal of this module")))
            (setq modules (cl-set-difference modules modified :test #'equal)))
        (if (cdr modified)
            (message "Omitting %s modules with uncommitted changes: %s"
                     (length modified)
                     (mapconcat #'identity modified ", "))
          (message "Omitting module %s, it has uncommitted changes"
                   (car modified)))
        (setq modules (cl-set-difference modules modified :test #'equal))))
    (when modules
      (let ((alist
             (and trash-gitdirs
                  (--map (split-string it "\0")
                         (mercit-git-lines "submodule" "foreach" "-q"
                                          "printf \"$sm_path\\0$name\n\"")))))
        (mercit-git "submodule" "absorbgitdirs" "--" modules)
        (mercit-git "submodule" "deinit" args "--" modules)
        (mercit-git "rm" args "--" modules)
        (when (and trash-gitdirs
                   (mercit-confirm 'trash-module-gitdirs
                     "Trash gitdir of module %s"
                     "Trash gitdirs of %i modules"
                     t modules))
          (dolist (module modules)
            (if-let ((name (cadr (assoc module alist))))
                ;; Disregard if `mercit-delete-by-moving-to-trash'
                ;; is nil.  Not doing so would be too dangerous.
                (delete-directory (mercit-git-dir
                                   (convert-standard-filename
                                    (concat "modules/" name)))
                                  t t)
              (error "BUG: Weird module name and/or path for %s" module)))))
      (mercit-refresh))))

;;; Sections

;;;###autoload
(defun mercit-insert-modules ()
  "Insert submodule sections.
Hook `mercit-module-sections-hook' controls which module sections
are inserted, and option `mercit-module-sections-nested' controls
whether they are wrapped in an additional section."
  (when-let ((modules (mercit-list-module-paths)))
    (if mercit-module-sections-nested
        (mercit-insert-section (modules nil t)
          (mercit-insert-heading
            (format "%s (%s)"
                    (propertize "Modules"
                                'font-lock-face 'mercit-section-heading)
                    (length modules)))
          (mercit-insert-section-body
            (mercit--insert-modules)))
      (mercit--insert-modules))))

(defun mercit--insert-modules (&optional _section)
  (mercit-run-section-hook 'mercit-module-sections-hook))

;;;###autoload
(defun mercit-insert-modules-overview ()
  "Insert sections for all modules.
For each section insert the path and the output of `git describe --tags',
or, failing that, the abbreviated HEAD commit hash."
  (when-let ((modules (mercit-list-module-paths)))
    (mercit-insert-section (modules nil t)
      (mercit-insert-heading
        (format "%s (%s)"
                (propertize "Modules overview"
                            'font-lock-face 'mercit-section-heading)
                (length modules)))
      (mercit-insert-section-body
        (mercit--insert-modules-overview)))))

(defvar mercit-modules-overview-align-numbers t)

(defun mercit--insert-modules-overview (&optional _section)
  (mercit-with-toplevel
    (let* ((modules (mercit-list-module-paths))
           (path-format (format "%%-%is "
                                (min (apply #'max (mapcar #'length modules))
                                     (/ (window-width) 2))))
           (branch-format (format "%%-%is " (min 25 (/ (window-width) 3)))))
      (dolist (module modules)
        (let ((default-directory
               (expand-file-name (file-name-as-directory module))))
          (mercit-insert-section (mercit-module-section module t)
            (insert (propertize (format path-format module)
                                'font-lock-face 'mercit-diff-file-heading))
            (if (not (file-exists-p ".git"))
                (insert "(unpopulated)")
              (insert (format
                       branch-format
                       (--if-let (mercit-get-current-branch)
                           (propertize it 'font-lock-face 'mercit-branch-local)
                         (propertize "(detached)" 'font-lock-face 'warning))))
              (--if-let (mercit-git-string "describe" "--tags")
                  (progn (when (and mercit-modules-overview-align-numbers
                                    (string-match-p "\\`[0-9]" it))
                           (insert ?\s))
                         (insert (propertize it 'font-lock-face 'mercit-tag)))
                (--when-let (mercit-rev-format "%h")
                  (insert (propertize it 'font-lock-face 'mercit-hash)))))
            (insert ?\n))))))
  (insert ?\n))

(defvar mercit-modules-section-map
  (let ((map (make-sparse-keymap)))
    (mercit-menu-set map [remap mercit-visit-thing]
      #'mercit-list-submodules "List %t")
    map)
  "Keymap for `modules' sections.")

(defvar mercit-module-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-j") #'mercit-submodule-visit)
    (define-key map [C-return]  #'mercit-submodule-visit)
    (mercit-menu-set map [mercit-visit-thing]
      #'mercit-submodule-visit "Visit %s")
    (mercit-menu-set map [mercit-stage-file]
      #'mercit-stage "Stage %T"
      '(:visible (eq (mercit-diff-type) 'unstaged)))
    (mercit-menu-set map [mercit-unstage-file]
      #'mercit-unstage "Unstage %T"
      '(:visible (eq (mercit-diff-type) 'staged)))
    (define-key-after map [separator-mercit-submodule] menu-bar-separator)
    (mercit-menu-set map [mercit-submodule] #'mercit-submodule "Module commands...")
    map)
  "Keymap for `module' sections.")

(defun mercit-submodule-visit (module &optional other-window)
  "Visit MODULE by calling `mercit-status' on it.
Offer to initialize MODULE if it's not checked out yet.
With a prefix argument, visit in another window."
  (interactive (list (or (mercit-section-value-if 'module)
                         (mercit-read-module-path "Visit module"))
                     current-prefix-arg))
  (mercit-with-toplevel
    (let ((path (expand-file-name module)))
      (cond
       ((file-exists-p (expand-file-name ".git" module))
        (mercit-diff-visit-directory path other-window))
       ((y-or-n-p (format "Initialize submodule '%s' first?" module))
        (mercit-run-git-async "submodule" "update" "--init" "--" module)
        (set-process-sentinel
         mercit-this-process
         (lambda (process event)
           (let ((mercit-process-raise-error t))
             (mercit-process-sentinel process event))
           (when (and (eq (process-status      process) 'exit)
                      (=  (process-exit-status process) 0))
             (mercit-diff-visit-directory path other-window)))))
       ((file-exists-p path)
        (dired-jump other-window (concat path "/.")))))))

;;;###autoload
(defun mercit-insert-modules-unpulled-from-upstream ()
  "Insert sections for modules that haven't been pulled from the upstream.
These sections can be expanded to show the respective commits."
  (mercit--insert-modules-logs "Modules unpulled from @{upstream}"
                              'modules-unpulled-from-upstream
                              "HEAD..@{upstream}"))

;;;###autoload
(defun mercit-insert-modules-unpulled-from-pushremote ()
  "Insert sections for modules that haven't been pulled from the push-remote.
These sections can be expanded to show the respective commits."
  (mercit--insert-modules-logs "Modules unpulled from @{push}"
                              'modules-unpulled-from-pushremote
                              "HEAD..@{push}"))

;;;###autoload
(defun mercit-insert-modules-unpushed-to-upstream ()
  "Insert sections for modules that haven't been pushed to the upstream.
These sections can be expanded to show the respective commits."
  (mercit--insert-modules-logs "Modules unmerged into @{upstream}"
                              'modules-unpushed-to-upstream
                              "@{upstream}..HEAD"))

;;;###autoload
(defun mercit-insert-modules-unpushed-to-pushremote ()
  "Insert sections for modules that haven't been pushed to the push-remote.
These sections can be expanded to show the respective commits."
  (mercit--insert-modules-logs "Modules unpushed to @{push}"
                              'modules-unpushed-to-pushremote
                              "@{push}..HEAD"))

(defun mercit--insert-modules-logs (heading type range)
  "For internal use, don't add to a hook."
  (unless (mercit-ignore-submodules-p)
    (when-let ((modules (mercit-list-module-paths)))
      (mercit-insert-section section ((eval type) nil t)
        (string-match "\\`\\(.+\\) \\([^ ]+\\)\\'" heading)
        (mercit-insert-heading
          (propertize (match-string 1 heading)
                      'font-lock-face 'mercit-section-heading)
          " "
          (propertize (match-string 2 heading)
                      'font-lock-face 'mercit-branch-remote)
          ":")
        (mercit-with-toplevel
          (dolist (module modules)
            (when (mercit-module-worktree-p module)
              (let ((default-directory
                     (expand-file-name (file-name-as-directory module))))
                (when (mercit-file-accessible-directory-p default-directory)
                  (mercit-insert-section sec (mercit-module-section module t)
                    (mercit-insert-heading
                      (propertize module
                                  'font-lock-face 'mercit-diff-file-heading)
                      ":")
                    (oset sec range range)
                    (mercit-git-wash
                        (apply-partially #'mercit-log-wash-log 'module)
                      "-c" "push.default=current" "log" "--oneline" range)
                    (when (> (point)
                             (oref sec content))
                      (delete-char -1))))))))
        (if (> (point)
               (oref section content))
            (insert ?\n)
          (mercit-cancel-section))))))

;;; List

;;;###autoload
(defun mercit-list-submodules ()
  "Display a list of the current repository's submodules."
  (interactive)
  (mercit-submodule-list-setup mercit-submodule-list-columns))

(defvar mercit-submodule-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mercit-repolist-mode-map)
    map)
  "Local keymap for Mercit-Submodule-List mode buffers.")

(define-derived-mode mercit-submodule-list-mode tabulated-list-mode "Modules"
  "Major mode for browsing a list of Mercurial submodules."
  :group 'mercit-repolist-mode
  (setq-local x-stretch-cursor nil)
  (setq tabulated-list-padding 0)
  (add-hook 'tabulated-list-revert-hook #'mercit-submodule-list-refresh nil t)
  (setq imenu-prev-index-position-function
        #'mercit-repolist--imenu-prev-index-position)
  (setq imenu-extract-index-name-function #'tabulated-list-get-id))

(defvar-local mercit-submodule-list-predicate nil)

(defun mercit-submodule-list-setup (columns &optional predicate)
  (mercit-display-buffer
   (or (mercit-get-mode-buffer 'mercit-submodule-list-mode)
       (mercit-generate-new-buffer 'mercit-submodule-list-mode)))
  (mercit-submodule-list-mode)
  (setq-local mercit-repolist-columns columns)
  (setq-local mercit-repolist-sort-key mercit-submodule-list-sort-key)
  (setq-local mercit-submodule-list-predicate predicate)
  (mercit-repolist-setup-1)
  (mercit-submodule-list-refresh))

(defun mercit-submodule-list-refresh ()
  (setq tabulated-list-entries
        (-keep (lambda (module)
                 (let ((default-directory
                        (expand-file-name (file-name-as-directory module))))
                   (and (file-exists-p ".git")
                        (or (not mercit-submodule-list-predicate)
                            (funcall mercit-submodule-list-predicate module))
                        (list module
                              (vconcat
                               (mapcar (pcase-lambda (`(,title ,width ,fn ,props))
                                         (or (funcall fn `((:path  ,module)
                                                           (:title ,title)
                                                           (:width ,width)
                                                           ,@props))
                                             ""))
                                       mercit-repolist-columns))))))
               (mercit-list-module-paths)))
  (message "Listing submodules...")
  (tabulated-list-init-header)
  (tabulated-list-print t)
  (message "Listing submodules...done"))

(defun mercit-modulelist-column-path (spec)
  "Insert the relative path of the submodule."
  (let ((path (cadr (assq :path spec))))
    (or (run-hook-with-args-until-success
         'mercit-submodule-list-format-path-functions path)
        path)))

;;; Utilities

(defun mercit-submodule--maybe-reuse-gitdir (name path)
  (let ((gitdir
         (mercit-git-dir (convert-standard-filename (concat "modules/" name)))))
    (when (and (file-exists-p gitdir)
               (not (file-exists-p path)))
      (pcase (read-char-choice
              (concat
               gitdir " already exists.\n"
               "Type [u] to use the existing gitdir and create the working tree\n"
               "     [r] to rename the existing gitdir and clone again\n"
               "     [t] to trash the existing gitdir and clone again\n"
               "   [C-g] to abort ")
              '(?u ?r ?t))
        (?u (mercit-submodule--restore-worktree (expand-file-name path) gitdir))
        (?r (rename-file gitdir (concat gitdir "-"
                                        (format-time-string "%F-%T"))))
        (?t (delete-directory gitdir t t))))))

(defun mercit-submodule--restore-worktree (worktree gitdir)
  (make-directory worktree t)
  (with-temp-file (expand-file-name ".git" worktree)
    (insert "gitdir: " (file-relative-name gitdir worktree) "\n"))
  (let ((default-directory worktree))
    (mercit-call-git "reset" "--hard" "HEAD" "--")))

;;; _
(provide 'mercit-submodule)
;;; mercit-submodule.el ends here
