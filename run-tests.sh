#!/bin/bash


if [[ ! "${1}" =~ ^(all|package-init|byte-compile|package-lint|test)$ ]]; then
    >&2 echo "Unsupported mode: '${1}'."
    >&2 echo "Usage ${0} [all|package-init|byte-compile|package-lint|test]"
    exit 1
fi

set -x
set -e

mode="${1}"

EMACS="${EMACS:=emacs}"

NEEDED_PACKAGES="magit compat package-lint el-mock"

INIT_PACKAGE_EL="(progn \
  (require 'package) \
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
  (package-initialize) \
  (unless package-archive-contents \
     (package-refresh-contents)) \
  (dolist (pkg '(${NEEDED_PACKAGES})) \
    (unless (package-installed-p pkg) \
      (package-install pkg))))"

if [[ ${mode} =~ ^(all|package-init)$ ]]; then
    # Refresh package archives, because the test suite needs to see at least
    # ${NEEDED_PACKAGES}.
    "$EMACS" -Q --batch \
             --eval "$INIT_PACKAGE_EL"
fi

# Byte compile, failing on byte compiler errors, or on warnings unless ignored
if [ -n "${EMACS_LINT_IGNORE+x}" ]; then
    ERROR_ON_WARN=nil
else
    ERROR_ON_WARN=t
fi

if [[ ${mode} =~ ^(all|byte-compile)$ ]]; then
    "$EMACS" -Q --batch \
             --eval "$INIT_PACKAGE_EL" \
             --load difftastic.el \
             --eval "(setq byte-compile-error-on-warn ${ERROR_ON_WARN})" \
             --funcall batch-byte-compile \
             difftastic.el
    "$EMACS" -Q --batch \
             --eval "$INIT_PACKAGE_EL" \
             --load difftastic.el \
             --load test/difftastic.t.el \
             --eval "(setq byte-compile-error-on-warn ${ERROR_ON_WARN})" \
             --funcall batch-byte-compile \
             test/difftastic.t.el
fi

# Lint ourselves
# Lint failures are ignored if EMACS_LINT_IGNORE is defined, so that lint
# failures on Emacs 24.2 and below don't cause the tests to fail, as these
# versions have buggy imenu that reports (defvar foo) as a definition of foo.
if [[ ${mode} =~ ^(all|package-lint)$ ]]; then
    "$EMACS" -Q --batch \
             --eval "$INIT_PACKAGE_EL" \
             -L . \
             --eval "(require 'package-lint)" \
             --eval '(setq package-lint-batch-fail-on-warnings nil)' \
             --funcall package-lint-batch-and-exit \
             difftastic.el || [ -n "${EMACS_LINT_IGNORE+x}" ]
fi


if [[ ${mode} =~ ^(all|test)$ ]]; then
    "$EMACS" -Q --batch \
             --eval "$INIT_PACKAGE_EL" \
             --load difftastic.el \
             --load test/difftastic.t.el \
             --eval "(let ((default-directory
                            (file-name-concat default-directory \"test\")))
                       (ert-run-tests-batch-and-exit))"
fi
