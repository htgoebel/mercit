;;; mercit-tests.el --- Tests for Magit  -*- lexical-binding:t; coding:utf-8 -*-

;; Copyright (C) 2008-2023 The Magit Project Contributors

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

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'ert)
(require 'tramp)
(require 'tramp-sh)

(require 'mercit)

(defun mercit-test-init-repo (dir &rest args)
  (let ((mercit-git-global-arguments
         (nconc (list "-c" "init.defaultBranch=master")
                mercit-git-global-arguments)))
    (mercit-git "init" args dir)))

(defmacro mercit-with-test-directory (&rest body)
  (declare (indent 0) (debug t))
  (let ((dir (make-symbol "dir")))
    `(let ((,dir (file-name-as-directory (make-temp-file "mercit-" t)))
           (process-environment process-environment)
           (mercit-git-global-arguments
            (nconc (list "-c" "protocol.file.allow=always")
                   mercit-git-global-arguments)))
       (push "GIT_AUTHOR_NAME=A U Thor" process-environment)
       (push "GIT_AUTHOR_EMAIL=a.u.thor@example.com" process-environment)
       (condition-case err
           (cl-letf (((symbol-function #'message) (lambda (&rest _))))
             (let ((default-directory (file-truename ,dir)))
               ,@body))
         (error (message "Keeping test directory:\n  %s" ,dir)
                (signal (car err) (cdr err))))
       (delete-directory ,dir t))))

(defmacro mercit-with-test-repository (&rest body)
  (declare (indent 0) (debug t))
  `(mercit-with-test-directory (mercit-test-init-repo ".") ,@body))

(defmacro mercit-with-bare-test-repository (&rest body)
  (declare (indent 1) (debug t))
  `(mercit-with-test-directory (mercit-test-init-repo "." "--bare") ,@body))

;;; Git

(ert-deftest mercit--with-safe-default-directory ()
  (mercit-with-test-directory
    (let ((find-file-visit-truename nil))
      (should (equal (mercit-toplevel "repo/")
                     (mercit-toplevel (expand-file-name "repo/"))))
      (should (equal (mercit-toplevel "repo")
                     (mercit-toplevel (expand-file-name "repo/")))))))

(ert-deftest mercit-toplevel:basic ()
  (let ((find-file-visit-truename nil))
    (mercit-with-test-directory
      (mercit-test-init-repo "repo")
      (mercit-test-mercit-toplevel)
      (should (equal (mercit-toplevel   "repo/.git/")
                     (expand-file-name "repo/")))
      (should (equal (mercit-toplevel   "repo/.git/objects/")
                     (expand-file-name "repo/")))
      (should (equal (mercit-toplevel   "repo-link/.git/")
                     (expand-file-name "repo-link/")))
      (should (equal (mercit-toplevel   "repo-link/.git/objects/")
                     ;; We could theoretically return "repo-link/"
                     ;; here by going up until `--git-dir' gives us
                     ;; "." .  But that would be a bit risky and Magit
                     ;; never goes there anyway, so it's not worth it.
                     ;; But in the doc-string we say we cannot do it.
                     (expand-file-name "repo/"))))))

;; FIXME (ert-deftest mercit-toplevel:tramp ()
;;   (cl-letf* ((find-file-visit-truename nil)
;;              ;; Override tramp method so that we don't actually
;;              ;; require a functioning `sudo'.
;;              (sudo-method (cdr (assoc "sudo" tramp-methods)))
;;              ((cdr (assq 'tramp-login-program sudo-method))
;;               (list (if (file-executable-p "/bin/sh")
;;                         "/bin/sh"
;;                       shell-file-name)))
;;              ((cdr (assq 'tramp-login-args sudo-method)) nil))
;;     (mercit-with-test-directory
;;      (setq default-directory
;;            (concat (format "/sudo:%s@localhost:" (user-login-name))
;;                    default-directory))
;;      (mercit-test-init-repo "repo")
;;      (mercit-test-mercit-toplevel)
;;      (should (equal (mercit-toplevel   "repo/.git/")
;;                     (expand-file-name "repo/")))
;;      (should (equal (mercit-toplevel   "repo/.git/objects/")
;;                     (expand-file-name "repo/")))
;;      (should (equal (mercit-toplevel   "repo-link/.git/")
;;                     (expand-file-name "repo-link/")))
;;      (should (equal (mercit-toplevel   "repo-link/.git/objects/")
;;                     (expand-file-name "repo/"))))))

(ert-deftest mercit-toplevel:submodule ()
  (let ((find-file-visit-truename nil))
    (mercit-with-test-directory
      (mercit-test-init-repo "remote")
      (let ((default-directory (expand-file-name "remote/")))
        (mercit-git "commit" "-m" "init" "--allow-empty"))
      (mercit-test-init-repo "super")
      (setq default-directory (expand-file-name "super/"))
      (mercit-git "submodule" "add" "../remote" "repo/")
      (mercit-test-mercit-toplevel)
      (should (equal (mercit-toplevel   ".git/modules/repo/")
                     (expand-file-name "repo/")))
      (should (equal (mercit-toplevel   ".git/modules/repo/objects/")
                     (expand-file-name "repo/"))))))

(defun mercit-test-mercit-toplevel ()
  ;; repo
  (make-directory "repo/subdir/subsubdir" t)
  (should (equal (mercit-toplevel   "repo/")
                 (expand-file-name "repo/")))
  (should (equal (mercit-toplevel   "repo/")
                 (expand-file-name "repo/")))
  (should (equal (mercit-toplevel   "repo/subdir/")
                 (expand-file-name "repo/")))
  (should (equal (mercit-toplevel   "repo/subdir/subsubdir/")
                 (expand-file-name "repo/")))
  ;; repo-link
  (make-symbolic-link "repo" "repo-link")
  (should (equal (mercit-toplevel   "repo-link/")
                 (expand-file-name "repo-link/")))
  (should (equal (mercit-toplevel   "repo-link/subdir/")
                 (expand-file-name "repo-link/")))
  (should (equal (mercit-toplevel   "repo-link/subdir/subsubdir/")
                 (expand-file-name "repo-link/")))
  ;; *subdir-link
  (make-symbolic-link "repo/subdir"           "subdir-link")
  (make-symbolic-link "repo/subdir/subsubdir" "subsubdir-link")
  (should (equal (mercit-toplevel   "subdir-link/")
                 (expand-file-name "repo/")))
  (should (equal (mercit-toplevel   "subdir-link/subsubdir/")
                 (expand-file-name "repo/")))
  (should (equal (mercit-toplevel   "subsubdir-link")
                 (expand-file-name "repo/")))
  ;; subdir-link-indirect
  (make-symbolic-link "subdir-link" "subdir-link-indirect")
  (should (equal (mercit-toplevel   "subdir-link-indirect")
                 (expand-file-name "repo/")))
  ;; wrap/*link
  (mercit-test-init-repo "wrap")
  (make-symbolic-link "../repo"                  "wrap/repo-link")
  (make-symbolic-link "../repo/subdir"           "wrap/subdir-link")
  (make-symbolic-link "../repo/subdir/subsubdir" "wrap/subsubdir-link")
  (should (equal (mercit-toplevel   "wrap/repo-link/")
                 (expand-file-name "wrap/repo-link/")))
  (should (equal (mercit-toplevel   "wrap/subdir-link")
                 (expand-file-name "repo/")))
  (should (equal (mercit-toplevel   "wrap/subsubdir-link")
                 (expand-file-name "repo/"))))

(defun mercit-test-mercit-get ()
  (should (equal (mercit-get-all "a.b") '("val1" "val2")))
  (should (equal (mercit-get "a.b") "val2"))
  (let ((default-directory (expand-file-name "../remote/")))
    (should (equal (mercit-get "a.b") "remote-value")))
  (should (equal (mercit-get "CAM.El.Case.VAR") "value"))
  (should (equal (mercit-get "a.b2") "line1\nline2")))

(ert-deftest mercit-get ()
  (mercit-with-test-directory
   (mercit-test-init-repo "remote")
   (let ((default-directory (expand-file-name "remote/")))
     (mercit-git "commit" "-m" "init" "--allow-empty")
     (mercit-git "config" "a.b" "remote-value"))
   (mercit-test-init-repo "super")
   (setq default-directory (expand-file-name "super/"))
   ;; Some tricky cases:
   ;; Multiple config values.
   (mercit-git "config" "a.b" "val1")
   (mercit-git "config" "--add" "a.b" "val2")
   ;; CamelCase variable names.
   (mercit-git "config" "Cam.El.Case.Var" "value")
   ;; Values with newlines.
   (mercit-git "config" "a.b2" "line1\nline2")
   ;; Config variables in submodules.
   (mercit-git "submodule" "add" "../remote" "repo/")

   (mercit-test-mercit-get)
   (let ((mercit--refresh-cache (list (cons 0 0))))
     (mercit-test-mercit-get))))

(ert-deftest mercit-get-boolean ()
  (mercit-with-test-repository
    (mercit-git "config" "a.b" "true")
    (should     (mercit-get-boolean "a.b"))
    (should     (mercit-get-boolean "a" "b"))
    (mercit-git "config" "a.b" "false")
    (should-not (mercit-get-boolean "a.b"))
    (should-not (mercit-get-boolean "a" "b"))
    ;; Multiple values, last one wins.
    (mercit-git "config" "--add" "a.b" "true")
    (should     (mercit-get-boolean "a.b"))
    (let ((mercit--refresh-cache (list (cons 0 0))))
     (should    (mercit-get-boolean "a.b")))))

(ert-deftest mercit-get-{current|next}-tag ()
  (mercit-with-test-repository
    (mercit-git "commit" "-m" "1" "--allow-empty")
    (should (equal (mercit-get-current-tag) nil))
    (should (equal (mercit-get-next-tag)    nil))
    (mercit-git "tag" "1")
    (should (equal (mercit-get-current-tag) "1"))
    (should (equal (mercit-get-next-tag)    nil))
    (mercit-git "commit" "-m" "2" "--allow-empty")
    (mercit-git "tag" "2")
    (should (equal (mercit-get-current-tag) "2"))
    (should (equal (mercit-get-next-tag)    nil))
    (mercit-git "commit" "-m" "3" "--allow-empty")
    (should (equal (mercit-get-current-tag) "2"))
    (should (equal (mercit-get-next-tag)    nil))
    (mercit-git "commit" "-m" "4" "--allow-empty")
    (mercit-git "tag" "4")
    (mercit-git "reset" "HEAD~")
    (should (equal (mercit-get-current-tag) "2"))
    (should (equal (mercit-get-next-tag)    "4"))))

(ert-deftest mercit-list-{|local-|remote-}branch-names ()
  (mercit-with-test-repository
    (mercit-git "commit" "-m" "init" "--allow-empty")
    (mercit-git "update-ref" "refs/remotes/foobar/master" "master")
    (mercit-git "update-ref" "refs/remotes/origin/master" "master")
    (should (equal (mercit-list-branch-names)
                   (list "master" "foobar/master" "origin/master")))
    (should (equal (mercit-list-local-branch-names)
                   (list "master")))
    (should (equal (mercit-list-remote-branch-names)
                   (list "foobar/master" "origin/master")))
    (should (equal (mercit-list-remote-branch-names "origin")
                   (list "origin/master")))
    (should (equal (mercit-list-remote-branch-names "origin" t)
                   (list "master")))))

(ert-deftest mercit-process:match-prompt-nil-when-no-match ()
  (should (null (mercit-process-match-prompt '("^foo: ?$") "bar: "))))

(ert-deftest mercit-process:match-prompt-non-nil-when-match ()
  (should (mercit-process-match-prompt '("^foo: ?$") "foo: ")))

(ert-deftest mercit-process:match-prompt-match-non-first-prompt ()
  (should (mercit-process-match-prompt '("^bar: ?$ " "^foo: ?$") "foo: ")))

(ert-deftest mercit-process:match-prompt-suffixes-prompt ()
  (let ((prompts '("^foo: ?$")))
    (should (equal (mercit-process-match-prompt prompts "foo:")  "foo: "))
    (should (equal (mercit-process-match-prompt prompts "foo: ") "foo: "))))

(ert-deftest mercit-process:match-prompt-preserves-match-group ()
  (let* ((prompts '("^foo '\\(?99:.*\\)': ?$"))
         (prompt (mercit-process-match-prompt prompts "foo 'bar':")))
    (should (equal prompt "foo 'bar': "))
    (should (equal (match-string 99 "foo 'bar':") "bar"))))

(ert-deftest mercit-process:password-prompt ()
  (let ((mercit-process-find-password-functions
         (list (lambda (host) (when (string= host "www.host.com") "mypasswd")))))
    (cl-letf (((symbol-function 'process-send-string)
               (lambda (process string) string)))
      (should (string-equal (mercit-process-password-prompt
                             nil "Password for 'www.host.com':")
                            "mypasswd\n")))))

(ert-deftest mercit-process:password-prompt-observed ()
  (with-temp-buffer
    (cl-letf* ((test-proc (start-process
                           "dummy-proc" (current-buffer)
                           (concat invocation-directory invocation-name)
                           "-Q" "--batch" "--eval" "(read-string \"\")"))
               ((symbol-function 'read-passwd)
                (lambda (_) "mypasswd"))
               (sent-strings nil)
               ((symbol-function 'process-send-string)
                (lambda (_proc string) (push string sent-strings))))
      ;; Don't get stuck when we close the buffer.
      (set-process-query-on-exit-flag test-proc nil)
      ;; Try some example passphrase prompts, reported by users.
      (dolist (prompt '("
Enter passphrase for key '/home/user/.ssh/id_rsa': "
                        ;; Openssh 8.0 sends carriage return.
                        "\
\rEnter passphrase for key '/home/user/.ssh/id_ed25519': "))
        (mercit-process-filter test-proc prompt)
        (should (equal (pop sent-strings) "mypasswd\n")))
      (should (null sent-strings)))))

;;; Clone

(ert-deftest mercit-clone:--name-to-url-format-defaults ()
  (mercit-with-test-repository
   (mercit-git "config" "--add" "sourcehut.user" "~shuser")
   (mercit-git "config" "--add" "github.user" "ghuser")
   (mercit-git "config" "--add" "gitlab.user" "gluser")
   ;; No explicit service
   (should (string-equal (mercit-clone--name-to-url "a/b")
                         "git@github.com:a/b.git"))
   (should (string-equal (mercit-clone--name-to-url "b")
                         "git@github.com:ghuser/b.git"))
   ;; User in config
   (should (string-equal (mercit-clone--name-to-url "gh:b")
                         "git@github.com:ghuser/b.git"))
   (should (string-equal (mercit-clone--name-to-url "gl:n")
                         "git@gitlab.com:gluser/n.git"))
   (should (string-equal (mercit-clone--name-to-url "sh:l")
                         "git@git.sr.ht:~shuser/l"))
   ;; Explicit user (abbreviated service names)
   (should (string-equal (mercit-clone--name-to-url "gh:a/b")
                         "git@github.com:a/b.git"))
   (should (string-equal (mercit-clone--name-to-url "gl:t/s")
                         "git@gitlab.com:t/s.git"))
   (should (string-equal (mercit-clone--name-to-url "sh:~x/y")
                         "git@git.sr.ht:~x/y"))
   ;; Explicit user (long service names)
   (should (string-equal (mercit-clone--name-to-url "github:a1/b1")
                         "git@github.com:a1/b1.git"))
   (should (string-equal (mercit-clone--name-to-url "gitlab:t1/s1")
                         "git@gitlab.com:t1/s1.git"))
   (should (string-equal (mercit-clone--name-to-url "sourcehut:~x1/y1")
                         "git@git.sr.ht:~x1/y1"))))

(ert-deftest mercit-clone:--name-to-url-format-single-string ()
  (let ((mercit-clone-url-format "bird@%h:%n.git")
        (mercit-clone-name-alist
         '(("\\`\\(?:github:\\|gh:\\)?\\([^:]+\\)\\'" "github.com" "u")
           ("\\`\\(?:gitlab:\\|gl:\\)\\([^:]+\\)\\'" "gitlab.com" "u"))))
    (should (string-equal (mercit-clone--name-to-url "gh:a/b")
                          "bird@github.com:a/b.git"))
    (should (string-equal (mercit-clone--name-to-url "gl:a/b")
                          "bird@gitlab.com:a/b.git"))
    (should (string-equal (mercit-clone--name-to-url "github:c/d")
                          "bird@github.com:c/d.git"))
    (should (string-equal (mercit-clone--name-to-url "gitlab:c/d")
                          "bird@gitlab.com:c/d.git"))))

(ert-deftest mercit-clone:--name-to-url-format-bad-type-throws-error ()
  (let ((mercit-clone-url-format 3))
    (should-error (mercit-clone--name-to-url "gh:a/b")
                  :type 'user-error)))

(ert-deftest mercit-clone:--name-to-url-format-alist-different-urls-per-hostname ()
  (let ((mercit-clone-name-alist
         '(("\\`\\(?:example:\\|ex:\\)\\([^:]+\\)\\'" "git.example.com" "foouser")
           ("\\`\\(?:gh:\\)?\\([^:]+\\)\\'" "github.com" "u")))
        (mercit-clone-url-format
         '(("git.example.com" . "cow@%h:~%n")
           (t . "git@%h:%n.git"))))
    (should (string-equal (mercit-clone--name-to-url "gh:a/b")
                          "git@github.com:a/b.git"))
    (should (string-equal (mercit-clone--name-to-url "ex:a/b")
                          "cow@git.example.com:~a/b"))
    (should (string-equal (mercit-clone--name-to-url "example:x/y")
                          "cow@git.example.com:~x/y"))
    (should (string-equal (mercit-clone--name-to-url "ex:c")
                          "cow@git.example.com:~foouser/c"))))

(ert-deftest mercit-clone:--name-to-url-format-alist-no-fallback-throws-error ()
  (let ((mercit-clone-url-format '(("fail.example.com" . "git@%h:~%n"))))
    (should-error (mercit-clone--name-to-url "gh:a/b")
                  :type 'user-error)))

;;; Status

(defun mercit-test-get-section (list file)
  (mercit-status-internal default-directory)
  (--first (equal (oref it value) file)
           (oref (mercit-get-section `(,list (status)))
                 children)))

(ert-deftest mercit-status:file-sections ()
  (mercit-with-test-repository
    (cl-flet ((modify (file) (with-temp-file file
                               (insert (make-temp-name "content")))))
      (modify "file")
      (modify "file with space")
      (modify "file with äöüéλ")
      (should (mercit-test-get-section '(untracked) "file"))
      (should (mercit-test-get-section '(untracked) "file with space"))
      (should (mercit-test-get-section '(untracked) "file with äöüéλ"))
      (mercit-stage-modified t)
      (should (mercit-test-get-section '(staged) "file"))
      (should (mercit-test-get-section '(staged) "file with space"))
      (should (mercit-test-get-section '(staged) "file with äöüéλ"))
      (mercit-git "add" ".")
      (modify "file")
      (modify "file with space")
      (modify "file with äöüéλ")
      (should (mercit-test-get-section '(unstaged) "file"))
      (should (mercit-test-get-section '(unstaged) "file with space"))
      (should (mercit-test-get-section '(unstaged) "file with äöüéλ")))))

(ert-deftest mercit-status:log-sections ()
  (mercit-with-test-repository
    (mercit-git "commit" "-m" "common" "--allow-empty")
    (mercit-git "commit" "-m" "unpulled" "--allow-empty")
    (mercit-git "remote" "add" "origin" "/origin")
    (mercit-git "update-ref" "refs/remotes/origin/master" "master")
    (mercit-git "branch" "--set-upstream-to=origin/master")
    (mercit-git "reset" "--hard" "HEAD~")
    (mercit-git "commit" "-m" "unpushed" "--allow-empty")
    (should (mercit-test-get-section
             '(unpulled . "..@{upstream}")
             (mercit-rev-parse "--short" "origin/master")))
    (should (mercit-test-get-section
             '(unpushed . "@{upstream}..")
             (mercit-rev-parse "--short" "master")))))

;;; libgit

(ert-deftest mercit-in-bare-repo ()
  "Test `mercit-bare-repo-p' in a bare repository."
  (mercit-with-bare-test-repository
    (should (mercit-bare-repo-p))))

(ert-deftest mercit-in-non-bare-repo ()
  "Test `mercit-bare-repo-p' in a non-bare repository."
  (mercit-with-test-repository
    (should-not (mercit-bare-repo-p))))

;;; Utils

(ert-deftest mercit-utils:add-face-text-property ()
  (let ((str (concat (propertize "ab" 'font-lock-face 'highlight) "cd")))
    (mercit--add-face-text-property 0 (length str) 'bold nil str)
    (should (equal (get-text-property 0 'font-lock-face str) '(bold highlight)))
    (should (equal (get-text-property 2 'font-lock-face str) '(bold)))))

;;; mercit-tests.el ends soon
(provide 'mercit-tests)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; mercit-tests.el ends here
