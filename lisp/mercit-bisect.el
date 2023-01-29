;;; mercit-bisect.el --- Bisect support for Mercit  -*- lexical-binding:t -*-

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

;; Use a binary search to find the commit that introduced a bug.

;;; Code:

(require 'mercit)

;;; Options

(defcustom mercit-bisect-show-graph t
  "Whether to use `--graph' in the log showing commits yet to be bisected."
  :package-version '(mercit . "0.0.0")
  :group 'mercit-status
  :type 'boolean)

(defface mercit-bisect-good
  '((t :foreground "DarkOliveGreen"))
  "Face for good bisect revisions."
  :group 'mercit-faces)

(defface mercit-bisect-skip
  '((t :foreground "DarkGoldenrod"))
  "Face for skipped bisect revisions."
  :group 'mercit-faces)

(defface mercit-bisect-bad
  '((t :foreground "IndianRed4"))
  "Face for bad bisect revisions."
  :group 'mercit-faces)

;;; Commands

;;;###autoload (autoload 'mercit-bisect "mercit-bisect" nil t)
(transient-define-prefix mercit-bisect ()
  "Narrow in on the commit that introduced a bug."
  :man-page "git-bisect"
  [:class transient-subgroups
   :if-not mercit-bisect-in-progress-p
   ["Arguments"
    ("-n" "*Don't checkout commits"              "--no-checkout")
    ("-p" "*Follow only first parent of a merge" "--first-parent"
     :if (lambda () (mercit-git-version>= "2.29")))
    (6 mercit-bisect:--term-old
       :if (lambda () (mercit-git-version>= "2.7")))
    (6 mercit-bisect:--term-new
       :if (lambda () (mercit-git-version>= "2.7")))]
   ["Actions"
    ("B" "*Start"        mercit-bisect-start)
    ("s" "*Start script" mercit-bisect-run)]]
  ["Actions"
   :if mercit-bisect-in-progress-p
   ("B" "*Bad"          mercit-bisect-bad)
   ("g" "*Good"         mercit-bisect-good)
   (6 "m" "*Mark"       mercit-bisect-mark
      :if (lambda () (mercit-git-version>= "2.7")))
   ("k" "*Skip"         mercit-bisect-skip)
   ("r" "*Reset"        mercit-bisect-reset)
   ("s" "*Run script"   mercit-bisect-run)])

(transient-define-argument mercit-bisect:--term-old ()
  :description "*Old/good term"
  :class 'transient-option
  :key "=o"
  :argument "--term-old=")

(transient-define-argument mercit-bisect:--term-new ()
  :description "*New/bad term"
  :class 'transient-option
  :key "=n"
  :argument "--term-new=")

;;;###autoload
(defun mercit-bisect-start (bad good args)
  "Start a bisect session.

Bisecting a bug means to find the commit that introduced it.
This command starts such a bisect session by asking for a known
good and a known bad commit.  To move the session forward use the
other actions from the bisect transient command (\
\\<mercit-status-mode-map>\\[mercit-bisect])."
  (interactive (if (mercit-bisect-in-progress-p)
                   (user-error "Already bisecting")
                 (mercit-bisect-start-read-args)))
  (unless (mercit-rev-ancestor-p good bad)
    (user-error
     "The %s revision (%s) has to be an ancestor of the %s one (%s)"
     (or (transient-arg-value "--term-old=" args) "good")
     good
     (or (transient-arg-value "--term-new=" args) "bad")
     bad))
  (when (mercit-anything-modified-p)
    (user-error "Cannot bisect with uncommitted changes"))
  (mercit-git-bisect "start" (list args bad good) t))

(defun mercit-bisect-start-read-args ()
  (let* ((args (transient-args 'mercit-bisect))
         (bad (mercit-read-branch-or-commit
               (format "Start bisect with %s revision"
                       (or (transient-arg-value "--term-new=" args)
                           "bad")))))
    (list bad
          (mercit-read-other-branch-or-commit
           (format "%s revision" (or (transient-arg-value "--term-old=" args)
                                     "Good"))
           bad)
          args)))

;;;###autoload
(defun mercit-bisect-reset ()
  "After bisecting, cleanup bisection state and return to original `HEAD'."
  (interactive)
  (mercit-confirm 'reset-bisect)
  (mercit-run-git "bisect" "reset")
  (ignore-errors (delete-file (mercit-git-dir "BISECT_CMD_OUTPUT"))))

;;;###autoload
(defun mercit-bisect-good ()
  "While bisecting, mark the current commit as good.
Use this after you have asserted that the commit does not contain
the bug in question."
  (interactive)
  (mercit-git-bisect (or (cadr (mercit-bisect-terms))
                        (user-error "Not bisecting"))))

;;;###autoload
(defun mercit-bisect-bad ()
  "While bisecting, mark the current commit as bad.
Use this after you have asserted that the commit does contain the
bug in question."
  (interactive)
  (mercit-git-bisect (or (car (mercit-bisect-terms))
                        (user-error "Not bisecting"))))

;;;###autoload
(defun mercit-bisect-mark ()
  "While bisecting, mark the current commit with a bisect term.
During a bisect using alternate terms, commits can still be
marked with `mercit-bisect-good' and `mercit-bisect-bad', as those
commands map to the correct term (\"good\" to --term-old's value
and \"bad\" to --term-new's).  However, in some cases, it can be
difficult to keep that mapping straight in your head; this
command provides an interface that exposes the underlying terms."
  (interactive)
  (mercit-git-bisect
   (pcase-let ((`(,term-new ,term-old) (or (mercit-bisect-terms)
                                           (user-error "Not bisecting"))))
     (pcase (read-char-choice
             (format "Mark HEAD as %s ([n]ew) or %s ([o]ld)"
                     term-new term-old)
             (list ?n ?o))
       (?n term-new)
       (?o term-old)))))

;;;###autoload
(defun mercit-bisect-skip ()
  "While bisecting, skip the current commit.
Use this if for some reason the current commit is not a good one
to test.  This command lets Mercurial choose a different one."
  (interactive)
  (mercit-git-bisect "skip"))

;;;###autoload
(defun mercit-bisect-run (cmdline &optional bad good args)
  "Bisect automatically by running commands after each step.

Unlike `git bisect run' this can be used before bisecting has
begun.  In that case it behaves like `git bisect start; git
bisect run'."
  (interactive (let ((args (and (not (mercit-bisect-in-progress-p))
                                (mercit-bisect-start-read-args))))
                 (cons (read-shell-command "Bisect shell command: ") args)))
  (when (and bad good)
    ;; Avoid `mercit-git-bisect' because it's asynchronous, but the
    ;; next `git bisect run' call requires the bisect to be started.
    (mercit-with-toplevel
      (mercit-process-git
       (list :file (mercit-git-dir "BISECT_CMD_OUTPUT"))
       (mercit-process-git-arguments
        (list "bisect" "start" bad good args)))
      (mercit-refresh)))
  (mercit--with-connection-local-variables
   (mercit-git-bisect "run" (list shell-file-name
                                 shell-command-switch cmdline))))

(defun mercit-git-bisect (subcommand &optional args no-assert)
  (unless (or no-assert (mercit-bisect-in-progress-p))
    (user-error "Not bisecting"))
  (message "Bisecting...")
  (mercit-with-toplevel
    (mercit-run-git-async "bisect" subcommand args))
  (set-process-sentinel
   mercit-this-process
   (lambda (process event)
     (when (memq (process-status process) '(exit signal))
       (if (> (process-exit-status process) 0)
           (mercit-process-sentinel process event)
         (process-put process 'inhibit-refresh t)
         (mercit-process-sentinel process event)
         (when (buffer-live-p (process-buffer process))
           (with-current-buffer (process-buffer process)
             (when-let* ((section (mercit-section-at))
                         (output (buffer-substring-no-properties
                                  (oref section content)
                                  (oref section end))))
               (with-temp-file (mercit-git-dir "BISECT_CMD_OUTPUT")
                 (insert output)))))
         (mercit-refresh))
       (message "Bisecting...done")))))

;;; Sections

(defun mercit-bisect-in-progress-p ()
  (file-exists-p (mercit-git-dir "BISECT_LOG")))

(defun mercit-bisect-terms ()
  (mercit-file-lines (mercit-git-dir "BISECT_TERMS")))

(defun mercit-insert-bisect-output ()
  "While bisecting, insert section with output from `git bisect'."
  (when (mercit-bisect-in-progress-p)
    (let* ((lines
            (or (mercit-file-lines (mercit-git-dir "BISECT_CMD_OUTPUT"))
                (list "Bisecting: (no saved bisect output)"
                      "It appears you have invoked `git bisect' from a shell."
                      "There is nothing wrong with that, we just cannot display"
                      "anything useful here.  Consult the shell output instead.")))
           (done-re "^\\([a-z0-9]\\{40,\\}\\) is the first bad commit$")
           (bad-line (or (and (string-match done-re (car lines))
                              (pop lines))
                         (--first (string-match done-re it) lines))))
      (mercit-insert-section ((eval (if bad-line 'commit 'bisect-output))
                             (and bad-line (match-string 1 bad-line)))
        (mercit-insert-heading
          (propertize (or bad-line (pop lines))
                      'font-lock-face 'mercit-section-heading))
        (dolist (line lines)
          (insert line "\n"))))
    (insert "\n")))

(defun mercit-insert-bisect-rest ()
  "While bisecting, insert section visualizing the bisect state."
  (when (mercit-bisect-in-progress-p)
    (mercit-insert-section (bisect-view)
      (mercit-insert-heading "Bisect Rest:")
      (mercit-git-wash (apply-partially #'mercit-log-wash-log 'bisect-vis)
        "bisect" "visualize" "git" "log"
        "--format=%h%x00%D%x00%s" "--decorate=full"
        (and mercit-bisect-show-graph "--graph")))))

(defun mercit-insert-bisect-log ()
  "While bisecting, insert section logging bisect progress."
  (when (mercit-bisect-in-progress-p)
    (mercit-insert-section (bisect-log)
      (mercit-insert-heading "Bisect Log:")
      (mercit-git-wash #'mercit-wash-bisect-log "bisect" "log")
      (insert ?\n))))

(defun mercit-wash-bisect-log (_args)
  (let (beg)
    (while (progn (setq beg (point-marker))
                  (re-search-forward "^\\(git bisect [^\n]+\n\\)" nil t))
      (mercit-bind-match-strings (heading) nil
        (mercit-delete-match)
        (save-restriction
          (narrow-to-region beg (point))
          (goto-char (point-min))
          (mercit-insert-section (bisect-item heading t)
            (insert (propertize heading 'font-lock-face
                                'mercit-section-secondary-heading))
            (mercit-insert-heading)
            (mercit-wash-sequence
             (apply-partially #'mercit-log-wash-rev 'bisect-log
                              (mercit-abbrev-length)))
            (insert ?\n)))))
    (when (re-search-forward
           "# first bad commit: \\[\\([a-z0-9]\\{40,\\}\\)\\] [^\n]+\n" nil t)
      (mercit-bind-match-strings (hash) nil
        (mercit-delete-match)
        (mercit-insert-section (bisect-item)
          (insert hash " is the first bad commit\n"))))))

;;; _
(provide 'mercit-bisect)
;;; mercit-bisect.el ends here
