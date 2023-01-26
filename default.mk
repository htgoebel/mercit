TOP := $(dir $(lastword $(MAKEFILE_LIST)))

## User options ######################################################
#
# You can override these settings in "config.mk" or on the command
# line.
#
# You might also want to set LOAD_PATH.  If you do, then it must
# contain "-L .".
#
# If you don't do so, then the default is set in the "Load-Path"
# section below.  The default assumes that all dependencies are
# installed either at "../<DEPENDENCY>", or when using package.el
# at "ELPA_DIR/<DEPENDENCY>-<HIGHEST-VERSION>".

PREFIX   ?= /usr/local
sharedir ?= $(PREFIX)/share
lispdir  ?= $(sharedir)/emacs/site-lisp/mercit
infodir  ?= $(sharedir)/info
docdir   ?= $(sharedir)/doc/mercit

CP       ?= install -p -m 644
MKDIR    ?= install -p -m 755 -d
RMDIR    ?= rm -rf
TAR      ?= tar
SED      ?= sed

EMACS    ?= emacs
BATCH     = $(EMACS) -Q --batch $(LOAD_PATH)

LISP_EXTRA_TARGETS ?= check-declare

INSTALL_INFO     ?= $(shell command -v ginstall-info || printf install-info)
MAKEINFO         ?= makeinfo
MANUAL_HTML_ARGS ?= --css-ref /assets/page.css

GITSTATS      ?= gitstats
GITSTATS_DIR  ?= $(TOP)docs/stats
GITSTATS_ARGS ?= -c style=https://mercit.vc/assets/stats.css -c max_authors=999

BUILD_MERCIT_LIBGIT ?= false

## Files #############################################################

PKG       = mercit
PACKAGES  = mercit mercit-section git-commit

TEXIPAGES = $(addsuffix .texi,$(filter-out git-commit,$(PACKAGES)))
INFOPAGES = $(addsuffix .info,$(filter-out git-commit,$(PACKAGES)))
HTMLFILES = $(addsuffix .html,$(filter-out git-commit,$(PACKAGES)))
HTMLDIRS  = $(filter-out git-commit,$(PACKAGES))
PDFFILES  = $(addsuffix .pdf,$(filter-out git-commit,$(PACKAGES)))
EPUBFILES = $(addsuffix .epub,$(filter-out git-commit,$(PACKAGES)))

ELS  = git-commit.el
ELS += mercit-section.el
ELS += mercit-base.el
ifeq "$(BUILD_MERCIT_LIBGIT)" "true"
ELS += mercit-libgit.el
endif
ELS += mercit-git.el
ELS += mercit-mode.el
ELS += mercit-margin.el
ELS += mercit-process.el
ELS += mercit-transient.el
ELS += mercit-autorevert.el
ELS += mercit-core.el
ELS += mercit-diff.el
ELS += mercit-log.el
ELS += mercit-wip.el
ELS += mercit-reflog.el
ELS += mercit-apply.el
ELS += mercit-repos.el
ELS += mercit.el
ELS += mercit-status.el
ELS += mercit-refs.el
ELS += mercit-files.el
ELS += mercit-reset.el
ELS += mercit-branch.el
ELS += mercit-merge.el
ELS += mercit-tag.el
ELS += mercit-worktree.el
ELS += mercit-notes.el
ELS += mercit-obsolete.el
ELS += mercit-sequence.el
ELS += mercit-commit.el
ELS += mercit-remote.el
ELS += mercit-clone.el
ELS += mercit-fetch.el
ELS += mercit-pull.el
ELS += mercit-push.el
ELS += mercit-patch.el
ELS += mercit-bisect.el
ELS += mercit-stash.el
ELS += mercit-blame.el
ELS += mercit-sparse-checkout.el
ELS += mercit-submodule.el
ELS += mercit-subtree.el
ELS += mercit-ediff.el
ELS += mercit-gitignore.el
ELS += mercit-bundle.el
ELS += mercit-extras.el
ELS += git-rebase.el
ELS += mercit-bookmark.el
ELCS = $(ELS:.el=.elc)
ELMS = mercit.el $(filter-out $(addsuffix .el,$(PACKAGES)),$(ELS))
ELGS = mercit-autoloads.el mercit-version.el

## Versions ##########################################################

VERSION ?= $(shell \
  test -e $(TOP).git && \
  git describe --tags --abbrev=0 --always | cut -c2-)
TIMESTAMP = 20211004

COMPAT_VERSION        = 29.1.1.0
DASH_VERSION          = 2.19.1
GIT_COMMIT_VERSION    = $(VERSION)
LIBGIT_VERSION        = 0
MERCIT_VERSION         = $(VERSION)
MERCIT_LIBGIT_VERSION  = $(VERSION)
MERCIT_SECTION_VERSION = $(VERSION)
TRANSIENT_VERSION     = 0.3.6
WITH_EDITOR_VERSION   = 3.0.5

COMPAT_SNAPSHOT              = 29.1.1.0
DASH_MELPA_SNAPSHOT          = 20210826
GIT_COMMIT_MELPA_SNAPSHOT    = $(TIMESTAMP)
LIBGIT_MELPA_SNAPSHOT        = 0
MERCIT_MELPA_SNAPSHOT         = $(TIMESTAMP)
MERCIT_LIBGIT_MELPA_SNAPSHOT  = $(TIMESTAMP)
MERCIT_SECTION_MELPA_SNAPSHOT = $(TIMESTAMP)
TRANSIENT_MELPA_SNAPSHOT     = 20210920
WITH_EDITOR_MELPA_SNAPSHOT   = 20211001

EMACS_VERSION        = 25.1
LIBGIT_EMACS_VERSION = 26.1

EMACSOLD := $(shell $(BATCH) --eval \
  "(and (version< emacs-version \"$(EMACS_VERSION)\") (princ \"true\"))")
ifeq "$(EMACSOLD)" "true"
  $(error At least version $(EMACS_VERSION) of Emacs is required)
endif

## Load-Path #########################################################

# Remember to also update mercit-emacs-Q-command!

ifndef LOAD_PATH

USER_EMACS_DIR = $(HOME)/.emacs.d
ifeq "$(wildcard $(USER_EMACS_DIR))" ""
  XDG_CONFIG_DIR = $(or $(XDG_CONFIG_HOME),$(HOME)/.config)
  ifneq "$(wildcard $(XDG_CONFIG_DIR)/emacs)" ""
    USER_EMACS_DIR = $(XDG_CONFIG_DIR)/emacs
  endif
endif

ELPA_DIR ?= $(USER_EMACS_DIR)/elpa

COMPAT_DIR ?= $(shell \
  find -L $(ELPA_DIR) -maxdepth 1 -regex '.*/compat-[.0-9]*' 2> /dev/null | \
  sort | tail -n 1)
ifeq "$(COMPAT_DIR)" ""
  COMPAT_DIR = $(TOP)../compat
endif

DASH_DIR ?= $(shell \
  find -L $(ELPA_DIR) -maxdepth 1 -regex '.*/dash-[.0-9]*' 2> /dev/null | \
  sort | tail -n 1)
ifeq "$(DASH_DIR)" ""
  DASH_DIR = $(TOP)../dash
endif

LIBGIT_DIR ?= $(shell \
  find -L $(ELPA_DIR) -maxdepth 1 -regex '.*/libgit-[.0-9]*' 2> /dev/null | \
  sort | tail -n 1)
ifeq "$(LIBGIT_DIR)" ""
  LIBGIT_DIR = $(TOP)../libgit
endif

TRANSIENT_DIR ?= $(shell \
  find -L $(ELPA_DIR) -maxdepth 1 -regex '.*/transient-[.0-9]*' 2> /dev/null | \
  sort | tail -n 1)
ifeq "$(TRANSIENT_DIR)" ""
  TRANSIENT_DIR = $(TOP)../transient/lisp
endif

WITH_EDITOR_DIR ?= $(shell \
  find -L $(ELPA_DIR) -maxdepth 1 -regex '.*/with-editor-[.0-9]*' 2> /dev/null | \
  sort | tail -n 1)
ifeq "$(WITH_EDITOR_DIR)" ""
  WITH_EDITOR_DIR = $(TOP)../with-editor/lisp
endif

MERCIT_SECTION_DIR ?= $(shell \
  find -L $(ELPA_DIR) -maxdepth 1 -regex '.*/mercit-section-[.0-9]*' 2> /dev/null | \
  sort | tail -n 1)

SYSTYPE := $(shell $(EMACS) -Q --batch --eval "(princ system-type)")
ifeq ($(SYSTYPE), windows-nt)
  CYGPATH := $(shell cygpath --version 2>/dev/null)
endif

LOAD_PATH = -L $(TOP)lisp

# When making changes here, then don't forget to adjust "Makefile",
# ".github/workflows/test.yml", ".github/ISSUE_TEMPLATE/bug_report.md",
# `mercit-emacs-Q-command' and the "Installing from the Mercurial Repository"
# info node accordingly.  Also don't forget to "rgrep \b<pkg>\b".

ifdef CYGPATH
  LOAD_PATH += -L $(shell cygpath --mixed $(COMPAT_DIR))
  LOAD_PATH += -L $(shell cygpath --mixed $(DASH_DIR))
  LOAD_PATH += -L $(shell cygpath --mixed $(LIBGIT_DIR))
  LOAD_PATH += -L $(shell cygpath --mixed $(TRANSIENT_DIR))
  LOAD_PATH += -L $(shell cygpath --mixed $(WITH_EDITOR_DIR))
  ifneq "$(MERCIT_SECTION_DIR)" ""
    LOAD_PATH += -L $(shell cygpath --mixed $(MERCIT_SECTION_DIR))
  endif
else
  LOAD_PATH += -L $(COMPAT_DIR)
  LOAD_PATH += -L $(DASH_DIR)
  LOAD_PATH += -L $(LIBGIT_DIR)
  LOAD_PATH += -L $(TRANSIENT_DIR)
  LOAD_PATH += -L $(WITH_EDITOR_DIR)
  ifneq "$(MERCIT_SECTION_DIR)" ""
    LOAD_PATH += -L $(MERCIT_SECTION_DIR)
  endif
endif

endif # ifndef LOAD_PATH

ifndef ORG_LOAD_PATH
ORG_LOAD_PATH = -L ../../org/lisp
endif

## Publish ###########################################################

DOMAIN      ?= mercit.vc
CFRONT_DIST ?= E2LUHBKU1FBV02

PUBLISH_TARGETS ?= html html-dir pdf

DOCBOOK_XSL ?= /usr/share/xml/docbook/stylesheet/docbook-xsl/epub/docbook.xsl

EPUBTRASH = epub.xml META-INF OEBPS
