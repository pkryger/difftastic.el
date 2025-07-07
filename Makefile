export EMACS ?= $(shell command -v emacs 2>/dev/null)
CASK_DIR := $(shell cask package-directory)

files = $$(cask files | grep -Ev 'difftastic-(pkg|autoloads).el')
test_files = $(wildcard test/difftastic*.t.el)

cask_filename = $(or $(CASK_FILENAME),Cask) # Until Emacs-28

define cask_install
"(progn                                                         \
   (require 'cask)                                              \
   (let ((cask-filename \"${CASK_FILENAME}\"))                  \
     (cask-install (cask-setup \"${GITHUB_WORKSPACE}\"))))";
endef

.PHONY: cask-install
cask-install:
	if [ -z "${CASK_FILENAME}" ]; then          \
       cask install;                            \
    else                                        \
       cask eval $(cask_install)                \
    fi

$(CASK_DIR): $(cask_filename)
    # Since Emacs-29: cask install
	$(MAKE) cask-install
	@touch $(CASK_DIR)

.PHONY: cask
cask: $(CASK_DIR)

.PHONY: bytecompile
bytecompile: cask
	cask emacs -batch -L . -L test                  \
      --eval "(setq byte-compile-error-on-warn t)"  \
      -f batch-byte-compile $(files) $(test_files)
	(ret=$$? ; cask clean-elc ; rm -f test/*.elc ; exit $$ret)

.PHONY: lint
lint: cask
	cask emacs -batch -L .                                      \
      --load package-lint                                       \
      --eval '(setq package-lint-main-file "difftastic.el")'    \
      --funcall package-lint-batch-and-exit $(files)

.PHONY: relint
relint: cask
	cask emacs -batch -L . -L test                  \
      --load relint                                 \
      --funcall relint-batch $(files) $(test_files)

.PHONY: checkdoc
checkdoc: cask
	cask emacs -batch -L .                      \
      --load checkdoc-batch                     \
      --funcall checkdoc-batch $(files)

.PHONY: commentary
commentary: cask
	cask emacs -batch -L .                      \
      --load org-commentary                     \
      --funcall org-commentary-check-batch

define checktoc_batch
"(progn                                                             \
   (find-file \"README.org\")                                       \
   (let ((before (buffer-string))                                   \
         (readme-buffer (current-buffer)))                          \
     (org-make-toc)                                                 \
     (when (not (equal before (buffer-string)))                     \
       (require 'diff)                                              \
       (with-temp-buffer                                            \
         (insert before)                                            \
         (with-current-buffer                                       \
           (diff-no-select (current-buffer) readme-buffer nil t)    \
           (error \"Table of contens has not been updated\n%s\"     \
                  (buffer-string)))))))"
endef

.PHONY: checktoc
checktoc: cask
	cask emacs -batch                           \
       --eval $(checktoc_batch)

ifeq ($(RUNNER_DEBUG),1)
    define test_debug
"(setq print-level 50                            \
       print-length 100                          \
       backtrace-line-length 5000                \
       eval-expression-print-level nil           \
       eval-expression-print-length nil          \
       ert-batch-print-level print-level         \
       ert-batch-print-length print-length       \
       ert-batch-backtrace-right-margin nil)"
    endef
else
    define test_debug
'(message "Set RUNNER_DEBUG=1 to enable debugging in tests")'
    endef
endif

.PHONY: test
test: cask
	cask emacs -batch                                                               \
      $(foreach test_file,$(filter-out %.i.t.el,$(test_files)),--load $(test_file)) \
      --eval $(test_debug)                                                          \
      --funcall ert-run-tests-batch-and-exit

.PHONY: itest
itest: cask
	cask emacs -batch                                                           \
      $(foreach test_file,$(filter %.i.t.el,$(test_files)),--load $(test_file)) \
      --eval $(test_debug)                                                      \
      --funcall ert-run-tests-batch-and-exit
