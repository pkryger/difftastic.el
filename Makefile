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
	  -f batch-byte-compile $$(cask files) \
                            test/difftastic.t.el \
                            test/difftastic-bindings.t.el; \
	  (ret=$$? ; cask clean-elc ; rm -f test/*.elc && exit $$ret)

.PHONY: lint
lint: cask
	cask emacs -batch -L . \
	  --load package-lint \
      --eval '(setq package-lint-main-file "difftastic.el")' \
	  --funcall package-lint-batch-and-exit \
        difftastic.el \
        difftastic-bindings.el

.PHONY: test
test: cask compile
	cask emacs -batch \
	  --load test/difftastic.t.el \
	  --load test/difftastic-bindings.t.el \
	  --eval "(let ((print-level 50) \
	                (eval-expression-print-level 50) \
                    (eval-expression-print-length 1000) \
                    (edebug-print-level 50) \
                    (edebug-print-length 1000) \
	                (ert-batch-print-level 50) \
	                (ert-batch-print-length 1000) \
	                (ert-batch-backtrace-line-length 1000) \
	                (ert-batch-backtrace-right-margin 1000)) \
	            (ert-run-tests-batch-and-exit '(not (tag interactive))))"
