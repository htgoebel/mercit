-include ../config.mk
include ../default.mk

## ###################################################################

.PHONY: mercit-version.el check-declare install

all: lisp

## Build order #######################################################

git-commit.elc:
mercit-section.elc:
mercit-base.elc:            mercit-section.elc
ifeq "$(BUILD_MAGIT_LIBGIT)" "true"
mercit-libgit.elc:
mercit-git.elc:             mercit-base.elc mercit-libgit.elc
else
mercit-git.elc:             mercit-base.elc
endif
mercit-mode.elc:            mercit-base.elc mercit-git.elc
mercit-margin.elc:          mercit-base.elc mercit-mode.elc
mercit-process.elc:         mercit-base.elc mercit-git.elc mercit-mode.elc
mercit-transient.elc:       mercit-git.elc mercit-mode.elc mercit-process.elc
mercit-autorevert.elc:      mercit-git.elc mercit-process.elc
mercit-core.elc:            mercit-base.elc mercit-git.elc mercit-mode.elc \
                           mercit-margin.elc mercit-process.elc \
                           mercit-transient.elc mercit-autorevert.elc
mercit-diff.elc:            git-commit.elc mercit-core.elc
mercit-log.elc:             mercit-core.elc mercit-diff.elc
mercit-wip.elc:             mercit-core.elc mercit-log.elc
mercit-reflog.elc:          mercit-core.elc mercit-log.elc
mercit-apply.elc:           mercit-core.elc mercit-diff.elc mercit-wip.elc
mercit-repos.elc:           mercit-core.elc
mercit.elc:                 git-commit.elc mercit-core.elc mercit-diff.elc \
                           mercit-log.elc mercit-apply.elc mercit-repos.elc
mercit-status.elc:          mercit.elc
mercit-refs.elc:            mercit.elc
mercit-files.elc:           mercit.elc
mercit-reset.elc:           mercit.elc
mercit-branch.elc:          mercit.elc mercit-reset.elc
mercit-merge.elc:           mercit.elc mercit-diff.elc
mercit-tag.elc:             mercit.elc
mercit-worktree.elc:        mercit.elc
mercit-notes.elc:           mercit.elc
mercit-sequence.elc:        mercit.elc
mercit-commit.elc:          mercit.elc mercit-sequence.elc
mercit-remote.elc:          mercit.elc
mercit-clone.elc:           mercit.elc
mercit-fetch.elc:           mercit.elc
mercit-pull.elc:            mercit.elc mercit-remote.elc
mercit-push.elc:            mercit.elc
mercit-bisect.elc:          mercit.elc
mercit-stash.elc:           mercit.elc mercit-sequence.elc mercit-reflog.elc
mercit-blame.elc:           mercit.elc
mercit-obsolete.elc:        mercit.elc
mercit-submodule.elc:       mercit.elc
mercit-patch.elc:           mercit.elc
mercit-subtree.elc:         mercit.elc
mercit-ediff.elc:           mercit.elc
mercit-gitignore.elc:       mercit.elc
mercit-sparse-checkout.elc: mercit.elc
mercit-bundle.elc:          mercit.elc
mercit-extras.elc:          mercit.elc mercit-merge.elc
git-rebase.elc:            mercit.elc
mercit-bookmark.elc:        mercit.elc
mercit-obsolete.elc:        mercit.elc

## Build #############################################################

lisp:       $(ELCS) loaddefs $(LISP_EXTRA_TARGETS)
loaddefs:   $(PKG)-autoloads.el
versionlib: $(PKG)-version.el

%.elc: %.el
	@printf "Compiling $<\n"
	@$(BATCH) --eval "(progn\
	(when (file-exists-p \"$@\")\
	  (delete-file \"$@\"))\
	(setq with-editor-emacsclient-executable nil)\
	$$suppress_warnings)" \
	-f batch-byte-compile $<

$(PKG)-autoloads.el: $(ELS)
	@printf " Creating $@\n"
	@$(EMACS) -Q --batch -l autoload -l cl-lib --eval "\
(let ((file (expand-file-name \"$@\"))\
      (autoload-timestamps nil) \
      (backup-inhibited t)\
      (version-control 'never)\
      (coding-system-for-write 'utf-8-emacs-unix))\
  (write-region (autoload-rubric file \"package\" nil) nil file nil 'silent)\
  (cl-letf (((symbol-function 'progress-reporter-do-update) (lambda (&rest _)))\
            ((symbol-function 'progress-reporter-done) (lambda (_))))\
    (let ((generated-autoload-file file))\
      (update-directory-autoloads default-directory))))" \
	2>&1 | sed "/^Package autoload is deprecated$$/d"

$(PKG)-version.el:
	@printf " Creating $@\n"
	@printf "%s" "$$VERSIONLIB_TMPL" > $@

check-declare:
	@printf "Checking function declarations\n"
	@$(BATCH) -L $(TOP)../borg -L $(TOP)../forge/lisp \
	--eval "(check-declare-directory default-directory)"

## Install ###########################################################

install: lisp versionlib
	@$(MKDIR) $(DESTDIR)$(lispdir)
	$(CP) $(ELS) mercit-autoloads.el mercit-version.el $(ELCS) $(DESTDIR)$(lispdir)

## Clean #############################################################

clean:
	@printf " Cleaning lisp/*...\n"
	@$(RM) *.elc $(ELGS)

## Templates #########################################################

define VERSIONLIB_TMPL
;;; $(PKG)-version.el --- the Magit version you are using

(setq $(PKG)-version "$(VERSION)")

(provide '$(PKG)-version)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; $(PKG)-version.el ends here

endef
export VERSIONLIB_TMPL
#'

define suppress_warnings
(fset 'original-message (symbol-function 'message))
(fset 'message ;'
      (lambda (f &rest a)
        (unless (or (equal f "Wrote %s")
                    (equal f "pcase-memoize: equal first branch, yet different")
                    (and (equal f "Warning: Unknown defun property `%S' in %S")
                         (memq (car a) '(pure side-effect-free interactive-only))))
          (apply 'original-message f a))))
endef
export suppress_warnings
