export EMACS ?= $(shell command -v emacs 2>/dev/null)
CASK_DIR := $(shell cask package-directory)

files = $$(cask files | grep -Ev 'difftastic-(pkg|autoloads).el')
test_files = test/difftastic.t.el test/difftastic-bindings.t.el

$(CASK_DIR): Cask
	cask install
	@touch $(CASK_DIR)

.PHONY: cask
cask: $(CASK_DIR)

.PHONY: bytecompile
bytecompile: cask
	cask emacs -batch -L . -L test \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile $(files) $(test_files)
	  (ret=$$? ; cask clean-elc ; rm -f test/*.elc && exit $$ret)

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

.PHONY: test
test: cask bytecompile
	cask emacs -batch \
      $(foreach test_file,$(test_files),--load $(test_file)) \
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
