export EMACS ?= $(shell command -v emacs 2>/dev/null)
CASK_DIR := $(shell cask package-directory)

$(CASK_DIR): Cask
	cask install
	@touch $(CASK_DIR)

.PHONY: cask
cask: $(CASK_DIR)

.PHONY: compile
compile: cask
	cask emacs -batch -L . -L test \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile $$(cask files) test/difftastic.t.el; \
	  (ret=$$? ; cask clean-elc ; rm -f test/*.elc && exit $$ret)

.PHONY: lint
lint: cask
	cask emacs -batch -L . \
	  --eval '(setq package-lint-batch-fail-on-warnings nil)' \
	  --load package-lint \
	  --funcall package-lint-batch-and-exit difftastic.el

.PHONY: test
test: cask compile
	cask emacs -batch \
	  --load test/difftastic.t.el \
	  --eval "(let ((print-level 50) \
	                (eval-expression-print-level 50)\
	                (ert-batch-print-level 50) \
	                (ert-batch-print-length 5000) \
	                (ert-batch-backtrace-line-length 5000) \
	                (ert-batch-backtrace-right-margin 5000)) \
	            (ert-run-tests-batch-and-exit '(not (tag interactive))))"
