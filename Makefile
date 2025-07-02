export EMACS ?= $(shell command -v emacs 2>/dev/null)
CASK_DIR := $(shell cask package-directory)

files = $$(cask files | grep -Ev 'difftastic-(pkg|autoloads).el')
test_files = $(wildcard test/difftastic*.t.el)

cask_filename = $(or $(CASK_FILENAME),Cask) # Until Emacs-28

.PHONY: cask-install
cask-install:
	if [ -z "${CASK_FILENAME}" ]; then \
       cask install; \
    else \
       cask eval "(progn \
                    (require 'cask) \
                    (let ((cask-filename \"${CASK_FILENAME}\")) \
                      (cask-install (cask-setup \"${GITHUB_WORKSPACE}\"))))"; \
    fi

$(CASK_DIR): $(cask_filename)
    # Since Emacs-29: cask install
	$(MAKE) cask-install
	@touch $(CASK_DIR)

.PHONY: cask
cask: $(CASK_DIR)

.PHONY: bytecompile
bytecompile: cask
	cask emacs -batch -L . -L test \
      --eval "(setq byte-compile-error-on-warn t)" \
      -f batch-byte-compile $(files) $(test_files)
	(ret=$$? ; cask clean-elc ; rm -f test/*.elc ; exit $$ret)

.PHONY: lint
lint: cask
	cask emacs -batch -L . \
      --load package-lint \
      --eval '(setq package-lint-main-file "difftastic.el")' \
      --funcall package-lint-batch-and-exit $(files)

.PHONY: relint
relint: cask
	cask emacs -batch -L . -L test \
      --load relint \
      --funcall relint-batch $(files) $(test_files)

.PHONY: checkdoc
checkdoc: cask
	cask emacs -batch -L . \
      --load checkdoc-batch \
      --funcall checkdoc-batch $(files)

.PHONY: commentary
commentary: cask
	cask emacs -batch -L . \
      --load org-commentary \
      --funcall org-commentary-check-batch

.PHONY: checktoc
checktoc: cask
	cask emacs -batch \
       --eval "(progn \
                 (find-file \"README.org\") \
                 (let ((before (buffer-string)) \
                       (readme-buffer (current-buffer))) \
                   (org-make-toc) \
                   (when (not (equal before (buffer-string))) \
                     (require 'diff) \
                     (with-temp-buffer \
                       (insert before) \
                       (with-current-buffer \
                         (diff-no-select (current-buffer) readme-buffer nil t) \
                         (error \"Table of contens has not been updated\n%s\" \
                                (buffer-string)))))))"

.PHONY: test
test: cask
	cask emacs -batch \
      $(foreach test_file,$(test_files),--load $(test_file)) \
      --eval "(setq print-level 50 \
                    eval-expression-print-level 50 \
                    eval-expression-print-length 1000 \
                    edebug-print-level 50 \
                    edebug-print-length 1000 \
                    ert-batch-print-level 50 \
                    ert-batch-print-length 1000 \
                    ert-batch-backtrace-line-length 1000 \
                    ert-batch-backtrace-right-margin 1000)" \
      --funcall ert-run-tests-batch-and-exit
