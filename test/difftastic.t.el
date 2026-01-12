;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when-let* ((dir (file-name-directory (or load-file-name
                                          byte-compile-current-file
                                          buffer-file-name))))
  (load-file (file-name-concat dir "undercover-init.el")))

(require 'ert)
(require 'ert-x)
(require 'difftastic)
(require 'el-mock)
(require 'treesit nil t)


(ert-deftest difftastic--copy-tree:basic ()
  (let* ((vec (make-vector 1 nil))
         (bool-vec (make-bool-vector 1 nil))
         (tree (cons vec bool-vec))
         (copy (difftastic--copy-tree tree)))
    (aset vec 0 t)
    (aset bool-vec 0 t)
    (should-not (eq tree copy))
    (should (eq (car tree) vec))
    (should-not (eq (car copy) vec))
    (should (eq (cdr tree) bool-vec))
    (should-not (eq (cdr copy) bool-vec))))

(ert-deftest difftastic--with-temp-advice:basic-case-removes-advice ()
  (should (eval
           '(difftastic--with-temp-advice
                'identity :around (lambda (&rest _) t)
              (identity nil))))
  (should-not (identity nil)))


(ert-deftest difftastic--with-temp-advice:error-signaled-removes-advice ()
  (should-error (eval
                 '(difftastic--with-temp-advice
                      'identity :around (lambda (&rest _)
                                          (signal 'error "test-error"))
                    (identity nil))))
  (should-not (identity nil)))


(ert-deftest difftastic--make-suggestion:basic ()
  (let ((languages '("Emacs Lisp" "C++" "Text"))
        elisp-buffer c++-buffer difftastic-buffer)
    (ert-with-test-buffer (:name "elisp")
      (emacs-lisp-mode)
      (setq elisp-buffer (current-buffer))
      (ert-with-test-buffer (:name "c++")
        (if (fboundp 'c++-ts-mode) ;; since Emacs-29
            ;; suppress warnings
            (difftastic--with-temp-advice 'treesit-ready-p
                :filter-args (lambda (&rest args)
                               (list (car args) t))
              (let ((treesit-auto-install-grammar t))
                (ignore treesit-auto-install-grammar) ;; until Emacs-30.2
                (c++-ts-mode)))
          (c++-mode))
        (setq c++-buffer (current-buffer))
        (ert-with-test-buffer (:name "difftastic")
          (difftastic-mode)
          (setq difftastic-buffer (current-buffer))

          (should
           (string= "Emacs Lisp"
                    (difftastic--make-suggestion languages
                                                 elisp-buffer)))
          (should-not
           (difftastic--make-suggestion languages
                                        difftastic-buffer))

          (should
           (string= "Emacs Lisp"
                    (difftastic--make-suggestion languages
                                                 elisp-buffer c++-buffer)))
          (should
           (string= "Emacs Lisp"
                    (difftastic--make-suggestion languages
                                                 elisp-buffer difftastic-buffer)))
          (should
           (string= "Emacs Lisp"
                    (difftastic--make-suggestion languages
                                                 difftastic-buffer elisp-buffer)))
          (should
           (string= "C++"
                    (difftastic--make-suggestion languages
                                                 c++-buffer difftastic-buffer)))
          (should
           (string= "C++"
                    (difftastic--make-suggestion languages
                                                 c++-buffer elisp-buffer)))
          (should
           (string= "C++"
                    (difftastic--make-suggestion languages
                                                 difftastic-buffer c++-buffer)))
          (should-not
           (difftastic--make-suggestion languages
                                        difftastic-buffer difftastic-buffer)))))))


(ert-deftest difftastic--transform-diff-arguments:basic ()
  (should (equal '(("--ignore-submodules=test-submodule")
                   ("--context=1" "--context=2"))
                 (difftastic--transform-diff-arguments
                  '("--stat" "--no-ext-diff"
                    "-U1" "--unified=2"
                    "-M" "-M3" "--find-renames" "--find-renames=4"
                    "--ignore-submodules=test-submodule")
                  nil)))
  (should (equal '(("--ignore-submodules=test-submodule")
                   ("--context=5" "--width=10"))
                 (difftastic--transform-diff-arguments
                  '("--stat" "--no-ext-diff"
                    "-U1" "--unified=2"
                    "-M" "-M3" "--find-renames" "--find-renames=4"
                    "--ignore-submodules=test-submodule")
                  '("--context=5" "--width=10")))))


(ert-deftest difftastic--file-extension-for-mode:parse-output ()
  (let ((file "difft--list-languages.out")
        (difftastic-executable "test-difftastic-executable")
        out)
    (should (or (file-exists-p file)
                (file-exists-p (format "test/%s" file))))
    (setq out (if (file-exists-p file)
                  file
                (format "test/%s" file)))
    (eval
     `(mocklet (((process-lines ,difftastic-executable "--list-languages")
                 => (ert-with-test-buffer ()
                      (insert-file-contents ,out)
                      (compat-call ;; Since Emacs-29
                       string-split (buffer-string) "\n"))))
        ;; Hints for updating the test when difft output changes:
        ;; $ difft --list-languages > difft--list-languages.out
        ;; (insert (format "%S" (difftastic--get-languages)))
        (should (equal ".c" (difftastic--file-extension-for-mode 'c-mode)))
        (should (equal ".c" (difftastic--file-extension-for-mode 'c-ts-mode)))))))


(ert-deftest difftastic--get-file-buf:buffer-visiting-file-no-temporary-created ()
  (let (temp-file)
    (unwind-protect
        (mocklet ((difftastic--file-extension-for-mode not-called))
          (ert-with-test-buffer ()
            (setq temp-file (make-temp-file "difftastic.t"))
            (write-file temp-file)
            (should-not (buffer-modified-p))
            (should (equal `(,(buffer-file-name) . nil)
                           (difftastic--get-file-buf "test" (current-buffer))))
            (should-not (buffer-modified-p))))

      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest difftastic--get-file-buf:modified-buffer-visiting-file--temporary-created ()
  (let (temp-file
        file-buf)
    (unwind-protect
        (mocklet (((difftastic--file-extension-for-mode 'c-mode) => ".c"))
          (ert-with-test-buffer ()
            (setq temp-file (make-temp-file "difftastic-t"))
            (set-visited-file-name temp-file t)
            (c-mode)
            (should (buffer-modified-p))
            (setq file-buf (difftastic--get-file-buf "test" (current-buffer)))
            (should (buffer-modified-p))
            (set-buffer-modified-p nil)
            (should (consp file-buf))
            (should (string-match-p
                     (eval '(rx string-start
                                (literal temporary-file-directory)
                                "difftastic-test-"
                                (one-or-more (not "/"))
                                ".c"
                                string-end))
                     (car file-buf)))
            (should (equal (current-buffer) (cdr file-buf)))))

      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest difftastic--get-file-buf:modified-buffer-visiting-file-with-extension--temporary-created ()
  (let (temp-file
        file-buf)
    (unwind-protect
        (mocklet ((difftastic--file-extension-for-mode not-called))
          (ert-with-test-buffer ()
            (setq temp-file (make-temp-file "difftastic-t" nil ".el"))
            (c-mode)
            (set-visited-file-name temp-file t)
            (should (buffer-modified-p))
            (setq file-buf (difftastic--get-file-buf "test" (current-buffer)))
            (should (buffer-modified-p))
            (set-buffer-modified-p nil)
            (should (consp file-buf))
            (should (string-match-p
                     (eval '(rx string-start
                                (literal temporary-file-directory)
                                "difftastic-test-"
                                (one-or-more (not "/"))
                                ".el"
                                string-end))
                     (car file-buf)))
            (should (equal (current-buffer) (cdr file-buf)))))

      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest difftastic--get-file-buf:buffer-not-visiting-file-temporary-created ()
  (let (file-buf)
    (unwind-protect
        (mocklet (((difftastic--file-extension-for-mode 'c-mode) => ".c"))
          (ert-with-test-buffer ()
            (c-mode)
            (setq file-buf (difftastic--get-file-buf "test" (current-buffer)))
            (should (consp file-buf))
            (should (string-match-p
                     (eval '(rx string-start
                                (literal temporary-file-directory)
                                "difftastic-test-"
                                (one-or-more (not "/"))
                                ".c"
                                string-end))
                     (car file-buf)))
            (should (equal (current-buffer) (cdr file-buf)))))

      (when (and (cdr file-buf) (file-exists-p (car file-buf)))
        (delete-file (car file-buf))))))


(ert-deftest difftastic--with-file-bufs:basic-temporary-file-bufs-are-preserved ()
  (let (temp-file-A temp-file-B temp-file-C)
    (unwind-protect
        (progn
          (setq temp-file-A (make-temp-file "difftastic.t")
                temp-file-B (make-temp-file "difftastic.t")
                temp-file-C (make-temp-file "difftastic.t"))
          (should
           (equal 42
                  (eval
                   `(difftastic--with-file-bufs (file-buf-A file-buf-B file-buf-C)
                      (should-not file-buf-A)
                      (should-not file-buf-B)
                      (should-not file-buf-C)
                      (setq file-buf-A (cons ,temp-file-A t)
                            file-buf-B (cons ,temp-file-B nil)
                            file-buf-C (cons ,temp-file-C t))
                      42))))
          (should (file-exists-p temp-file-A))
          (should (file-exists-p temp-file-B))
          (should (file-exists-p temp-file-C)))

      (dolist (temp-file (list temp-file-A temp-file-B temp-file-C))
        (when (file-exists-p temp-file)
          (delete-file temp-file))))))

(ert-deftest difftastic--with-file-bufs:basic-body-error-temporary-file-bufs-are-deleted ()
  (let (temp-file-A temp-file-B temp-file-C)
    (unwind-protect
        (progn
          (setq temp-file-A (make-temp-file "difftastic.t")
                temp-file-B (make-temp-file "difftastic.t")
                temp-file-C (make-temp-file "difftastic.t"))
          (should-error
           (eval
            `(difftastic--with-file-bufs (file-buf-A file-buf-B file-buf-C)
               (setq file-buf-A (cons ,temp-file-A t)
                     file-buf-B (cons ,temp-file-B nil)
                     file-buf-C (cons ,temp-file-C t))
               (signal 'error "test-error"))))
          (should-not (file-exists-p temp-file-A))
          (should (file-exists-p temp-file-B))
          (should-not (file-exists-p temp-file-C)))

      (dolist (temp-file (list temp-file-A temp-file-B temp-file-C))
        (when (file-exists-p temp-file)
          (delete-file temp-file))))))

(ert-deftest difftastic--with-file-bufs:valueform-temporary-file-bufs-are-preserved ()
  (let (temp-file-A temp-file-B temp-file-C)
    (unwind-protect
        (progn
          (setq temp-file-A (make-temp-file "difftastic.t")
                temp-file-B (make-temp-file "difftastic.t")
                temp-file-C (make-temp-file "difftastic.t"))
          (should
           (equal 42
                  (eval
                   `(difftastic--with-file-bufs ((file-buf-A (cons ,temp-file-A t))
                                                 (file-buf-B (cons ,temp-file-B nil))
                                                 (file-buf-C (cons ,temp-file-C t)))
                      (should (equal file-buf-A (cons ,temp-file-A t)))
                      (should (equal file-buf-B (cons ,temp-file-B nil)))
                      (should (equal file-buf-C (cons ,temp-file-C t)))
                      42))))
          (should (file-exists-p temp-file-A))
          (should (file-exists-p temp-file-B))
          (should (file-exists-p temp-file-C)))

      (dolist (temp-file (list temp-file-A temp-file-B temp-file-C))
        (when (file-exists-p temp-file)
          (delete-file temp-file))))))

(ert-deftest difftastic--with-file-bufs:valueform-body-error-temporary-file-bufs-are-deleted ()
  (let (temp-file-A temp-file-B temp-file-C)
    (unwind-protect
        (progn
          (setq temp-file-A (make-temp-file "difftastic.t")
                temp-file-B (make-temp-file "difftastic.t")
                temp-file-C (make-temp-file "difftastic.t"))
          (should-error
           (eval
            `(difftastic--with-file-bufs ((file-buf-A (cons ,temp-file-A t))
                                          (file-buf-B (cons ,temp-file-B nil))
                                          (file-buf-C (cons ,temp-file-C t)))
               (should (equal file-buf-A (cons ,temp-file-A t)))
               (should (equal file-buf-B (cons ,temp-file-B nil)))
               (should (equal file-buf-C (cons ,temp-file-C t)))
               (signal 'error "test-error"))))
          (should-not (file-exists-p temp-file-A))
          (should (file-exists-p temp-file-B))
          (should-not (file-exists-p temp-file-C)))

      (dolist (temp-file (list temp-file-A temp-file-B temp-file-C))
        (when (file-exists-p temp-file)
          (delete-file temp-file))))))

(ert-deftest difftastic--with-file-bufs:valueform-error-temporary-file-bufs-are-deleted ()
  (let (temp-file-A temp-file-B temp-file-C)
    (unwind-protect
        (progn
          (setq temp-file-A (make-temp-file "difftastic.t")
                temp-file-B (make-temp-file "difftastic.t")
                temp-file-C (make-temp-file "difftastic.t"))
          (should-error
           (eval
            `(difftastic--with-file-bufs ((file-buf-A (cons ,temp-file-A t))
                                          (file-buf-B (cons ,temp-file-B nil))
                                          (file-buf-C (cons ,temp-file-C t))
                                          (file-buf-D (signal 'error "test-error"))))))
          (should-not (file-exists-p temp-file-A))
          (should (file-exists-p temp-file-B))
          (should-not (file-exists-p temp-file-C)))

      (dolist (temp-file (list temp-file-A temp-file-B temp-file-C))
        (when (file-exists-p temp-file)
          (delete-file temp-file))))))

(ert-deftest difftastic--with-file-bufs:valueform-error ()
  (should-error (eval `(difftastic--with-file-bufs 42)))
  (should-error (eval `(difftastic--with-file-bufs (nil))))
  (should-error (eval `(difftastic--with-file-bufs (t))))
  (should-error (eval `(difftastic--with-file-bufs (42))))
  (should-error (eval `(difftastic--with-file-bufs ("foo"))))
  (should-error (eval `(difftastic--with-file-bufs ((42 t)))))
  (should-error (eval `(difftastic--with-file-bufs ((nil t)))))
  (should-error (eval `(difftastic--with-file-bufs ((t t)))))
  (should-error (eval `(difftastic--with-file-bufs (("foo" t)))))
  (should-error (eval `(difftastic--with-file-bufs ((foo)))))
  (should-error (eval `(difftastic--with-file-bufs ((foo bar baz))))))


(ert-deftest difftastic--delete-temp-file-buf:nil-does-nothing ()
  (should-not (difftastic--delete-temp-file-buf nil)))

(ert-deftest difftastic--delete-temp-file-buf:not-a-temporary-is-presered ()
  (let (temp-file)
    (unwind-protect
        (progn
          (setq temp-file (make-temp-file "difftastic.t"))
          (difftastic--delete-temp-file-buf (cons temp-file nil))
          (should (file-exists-p temp-file)))
      (when (file-exists-p temp-file)

        (delete-file temp-file)))))

(ert-deftest difftastic--delete-temp-file-buf:temporary-is-deleted ()
  (let (temp-file)
    (unwind-protect
        (progn
          (setq temp-file (make-temp-file "difftastic.t"))
          (difftastic--delete-temp-file-buf (cons temp-file t))
          (should-not (file-exists-p temp-file)))

      (when (file-exists-p temp-file)
        (delete-file temp-file)))))


(ert-deftest difftastic--override-binary-available-p:basic ()
  (with-temp-buffer
    (insert "test")
    (goto-char (point-min))
    (re-search-forward "test")
    (let ((match-data-orig (copy-sequence (match-data))))
      (mocklet ((difftastic--get-version => "0.65.0"))
        (should (difftastic--override-binary-available-p)))
      (should (equal match-data-orig (match-data)))
      (mocklet ((difftastic--get-version => "0.64.999"))
        (should-not (difftastic--override-binary-available-p)))
      (should (equal match-data-orig (match-data))))))


(ert-deftest difftastic--transient-arguments-to-difftastic:basic ()
  (mocklet ((difftastic--override-binary-available-p => t))
    (should (equal '("--override=lang1" "--override=lang2" "--context=5")
                   (difftastic--transient-arguments-to-difftastic
                    '(("--override=" "lang1" "lang2") "--context=5"))))
    (should (equal '("--override-binary=foo" "--override-binary=bar" "--context=5")
                   (difftastic--transient-arguments-to-difftastic
                    '(("--override-binary=" "foo" "bar") "--context=5"))))
    (should (equal '(("--foo=" "bar" "baz"))
                   (difftastic--transient-arguments-to-difftastic
                    '(("--foo=" "bar" "baz")))))
    (should (equal '("--width=42" "--context=5")
                   (difftastic--transient-arguments-to-difftastic
                    '("--width=42" "--context=5"))))
    (should-not (difftastic--transient-arguments-to-difftastic nil)))
  (mocklet ((difftastic--override-binary-available-p => nil))
    (should  (equal '("--context=5")
                    (difftastic--transient-arguments-to-difftastic
                     '(("--override-binary=" "foo" "bar") "--context=5"))))
    (should-not (difftastic--transient-arguments-to-difftastic
                  '(("--override-binary=" "foo" "bar"))))))


(ert-deftest difftastic--build-git-process-environment:without-difftastic-args ()
  (mocklet ((difftastic--override-binary-available-p => t))
    (let (difftastic-use-transient-arguments)
      (should (equal
               (format "GIT_EXTERNAL_DIFF=%s %s %s %s"
                       difftastic-executable
                       (shell-quote-argument "--color=always")
                       (shell-quote-argument "--width=42")
                       (shell-quote-argument (format "--background=%s"
                                                     (frame-parameter nil 'background-mode))))
               (car (difftastic--build-git-process-environment 42)))))))

(ert-deftest difftastic--build-git-process-environment:with-transient-args ()
  (mocklet ((difftastic--override-binary-available-p => t))
    (let ((difftastic-use-transient-arguments t)
          (transient-values '((difftastic--with-extra-arguments . (("--override=" "*:Java"))))))
      (should (equal
               (format
                "GIT_EXTERNAL_DIFF=%s %s %s %s %s"
                difftastic-executable
                (shell-quote-argument "--color=always")
                (shell-quote-argument "--width=42")
                (shell-quote-argument (format "--background=%s"
                                              (frame-parameter nil 'background-mode)))
                (shell-quote-argument "--override=*:Java"))
               (car
                (difftastic--build-git-process-environment 42)))))))

(ert-deftest difftastic--build-git-process-environment:with-suppressed-transient-args ()
  (mocklet ((difftastic--override-binary-available-p => t))
    (let (difftastic-use-transient-arguments
          (transient-values '((difftastic--with-extra-arguments . (("--override=" "*:Java"))))))
      (should (equal
               (format
                "GIT_EXTERNAL_DIFF=%s %s %s %s"
                difftastic-executable
                (shell-quote-argument "--color=always")
                (shell-quote-argument "--width=42")
                (shell-quote-argument (format "--background=%s"
                                              (frame-parameter nil 'background-mode))))
               (car
                (difftastic--build-git-process-environment 42)))))))

(ert-deftest difftastic--build-git-process-environment:with-transient-and-difftastic-args ()
  (mocklet ((difftastic--override-binary-available-p => t))
    (let ((difftastic-use-transient-arguments t)
          (transient-values '((difftastic--with-extra-arguments . (("--override=" "*:Java"))))))
      (should (equal
               (format
                "GIT_EXTERNAL_DIFF=%s %s %s %s %s"
                difftastic-executable
                (shell-quote-argument "--color=always")
                (shell-quote-argument "--width=42")
                (shell-quote-argument (format "--background=%s"
                                              (frame-parameter nil 'background-mode)))
                (shell-quote-argument "--override=*:C++"))
               (car
                (difftastic--build-git-process-environment
                 42
                 '("--override=*:C++"))))))))

(ert-deftest difftastic--build-git-process-environment:with-difftastic-args ()
  (mocklet ((difftastic--override-binary-available-p => t))
    (should (equal
             (format
              "GIT_EXTERNAL_DIFF=%s %s %s %s %s"
              difftastic-executable
              (shell-quote-argument "--color=always")
              (shell-quote-argument "--width=42")
              (shell-quote-argument (format "--background=%s"
                                            (frame-parameter nil 'background-mode)))
              (shell-quote-argument "--override=*:C++"))
             (car
              (difftastic--build-git-process-environment
               42
               '("--override=*:C++")))))))

(ert-deftest difftastic--build-git-process-environment:with-difftastic-args-override ()
  (mocklet ((difftastic--override-binary-available-p => t))
    (should (equal
             (format
              "GIT_EXTERNAL_DIFF=%s %s %s %s %s"
              difftastic-executable
              (shell-quote-argument "--color=foo")
              (shell-quote-argument "--width=bar")
              (shell-quote-argument "--background=baz")
              (shell-quote-argument "--override=*:Emacs Lisp"))
             (car
              (difftastic--build-git-process-environment
               42
               '("--color=foo"
                 "--width=bar"
                 "--background=baz"
                 "--override=*:Emacs Lisp")))))))

(ert-deftest difftastic--build-git-process-environment:with-difftastic-difft-environment ()
  (mocklet ((difftastic--override-binary-available-p => t))
    (let (difftastic-use-transient-arguments
          (difftastic-difft-environment '("TEST_VAR=test-value"))
          (process-environment '("ENV_VAR=env-value")))
      (should (equal
               (list (format "GIT_EXTERNAL_DIFF=%s %s %s %s"
                             difftastic-executable
                             (shell-quote-argument "--color=always")
                             (shell-quote-argument "--width=42")
                             (shell-quote-argument (format "--background=%s"
                                                           (frame-parameter nil 'background-mode))))
                     "TEST_VAR=test-value"
                     "ENV_VAR=env-value")
               (difftastic--build-git-process-environment 42))))))


(ert-deftest difftastic--build-files-command:without-difftastic-args ()
  (mocklet ((difftastic--override-binary-available-p => t))
    (let (difftastic-use-transient-arguments)
      (should (equal
               `(,difftastic-executable
                 "--color=always"
                 "--width=42"
                 ,(format "--background=%s" (frame-parameter nil 'background-mode))
                 "test-file-A" "test-file-B")
               (difftastic--build-files-command (cons "test-file-A" nil)
                                                (cons "test-file-B" nil)
                                                42))))))

(ert-deftest difftastic--build-files-command:with-transient-args ()
  (mocklet ((difftastic--override-binary-available-p => t))
    (let ((difftastic-use-transient-arguments t)
          (transient-values '((difftastic--with-extra-arguments . (("--override=" "*:Java"))))))
      (should (equal
               `(,difftastic-executable
                 "--color=always"
                 "--width=42"
                 ,(format "--background=%s" (frame-parameter nil 'background-mode))
                 "--override=*:Java"
                 "test-file-A" "test-file-B")
               (difftastic--build-files-command (cons "test-file-A" nil)
                                                (cons "test-file-B" nil)
                                                42))))))

(ert-deftest difftastic--build-files-command:with-suppressed-transient-args ()
  (mocklet ((difftastic--override-binary-available-p => t))
    (let (difftastic-use-transient-arguments
          (transient-values '((difftastic--with-extra-arguments . (("--override=" "*:Java"))))))
      (should (equal
               `(,difftastic-executable
                 "--color=always"
                 "--width=42"
                 ,(format "--background=%s" (frame-parameter nil 'background-mode))
                 "test-file-A" "test-file-B")
               (difftastic--build-files-command (cons "test-file-A" nil)
                                                (cons "test-file-B" nil)
                                                42))))))

(ert-deftest difftastic--build-files-command:with-transient-and-difftastic-args ()
  (mocklet ((difftastic--override-binary-available-p => t))
    (let ((difftastic-use-transient-arguments t)
          (transient-values '((difftastic--with-extra-arguments . (("--override=" "*:Java"))))))
      (should (equal
               `(,difftastic-executable
                 "--color=always"
                 "--width=42"
                 ,(format "--background=%s" (frame-parameter nil 'background-mode))
                 "--override=*:test-language"
                 "test-file-A" "test-file-B")
               (difftastic--build-files-command (cons "test-file-A" nil)
                                                (cons "test-file-B" nil)
                                                42
                                                '("--override=*:test-language")))))))

(ert-deftest difftastic--build-files-command:with-difftastic-args ()
  (mocklet ((difftastic--override-binary-available-p => t))
    (should (equal
             `(,difftastic-executable
               "--color=always"
               "--width=42"
               ,(format "--background=%s" (frame-parameter nil 'background-mode))
               "--override=*:test-language"
               "test-file-A" "test-file-B")
             (difftastic--build-files-command (cons "test-file-A" nil)
                                              (cons "test-file-B" nil)
                                              42
                                              '("--override=*:test-language"))))))

(ert-deftest difftastic--build-files-command:with-difftastic-args-override ()
  (mocklet ((difftastic--override-binary-available-p => t))
    (should (equal
             `(,difftastic-executable
               "--color=foo"
               "--width=bar"
               "--background=baz"
               "--override=*:test-language"
               "test-file-A" "test-file-B")
             (difftastic--build-files-command (cons "test-file-A" nil)
                                              (cons "test-file-B" nil)
                                              42
                                              '("--color=foo"
                                                "--width=bar"
                                                "--background=baz"
                                                "--override=*:test-language"))))))


(ert-deftest difftastic--rerun-file-buf:non-temporary-no-temporary-created ()
  (let (file-buf)
    (unwind-protect
        (let* ((metadata '((file-buf-test . ("test-file" . nil))))
               (orig-rerun-alist (copy-tree metadata)))
          (setq file-buf
                (difftastic--rerun-file-buf
                 "test"
                 (alist-get 'file-buf-test metadata)
                 metadata))
          (should (equal (alist-get 'file-buf-test orig-rerun-alist)
                         file-buf))
          (should (equal orig-rerun-alist metadata)))

      (when (and (cdr file-buf) (file-exists-p (car file-buf)))
        (delete-file (car file-buf))))))

(ert-deftest difftastic--rerun-file-buf:temporary-live-buffer-new-temporary-created ()
  (let (file-buf)
    (unwind-protect
        (let ((metadata '((file-buf-test . ("test-file" . t)))))
          (mocklet ((difftastic--file-extension-for-mode))
            (ert-with-test-buffer ()
              (setq file-buf
                    (difftastic--rerun-file-buf
                     "test"
                     (cons "test-file" (current-buffer))
                     metadata))
              (should-not (equal "test-file" (car file-buf)))
              (should (file-exists-p (car file-buf)))
              (should (equal file-buf
                             (alist-get 'file-buf-test metadata))))))

      (when (and (cdr file-buf) (file-exists-p (car file-buf)))
        (delete-file (car file-buf))))))

(ert-deftest difftastic--rerun-file-buf:temporary-live-modified-buffer-matching-other-new-temporary-created ()
  (let (file-buf
        (test-file (make-temp-file "difftastic-t")))
    (unwind-protect
        (let ((metadata '((file-buf-test . (test-file . t)))))
          (mocklet ((difftastic--file-extension-for-mode))
            (ert-with-test-buffer ()
              (set-visited-file-name test-file)
              (should (buffer-modified-p))
              (setq file-buf
                    (difftastic--rerun-file-buf
                     "test"
                     (cons test-file (current-buffer))
                     metadata
                     test-file))
              (should (buffer-modified-p))
              (set-buffer-modified-p nil)
              (should-not (equal test-file (car file-buf)))
              (should (file-exists-p (car file-buf)))
              (should (equal file-buf
                             (alist-get 'file-buf-test metadata))))))

      (when (and (cdr file-buf) (file-exists-p (car file-buf)))
        (delete-file (car file-buf)))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest difftastic--rerun-file-buf:temporary-live-non-modified-buffer-matching-error-signaled ()
  (let (file-buf
        (test-file (make-temp-file "difftastic-t")))
    (unwind-protect
        (let* ((metadata '((file-buf-test . (test-file . t))))
               (orig-rerun-alist (copy-tree metadata))
               (text-quoting-style 'straight))
          (ert-with-test-buffer ()
            (write-file test-file)
            (let ((data (cadr
                         (should-error
                          (setq file-buf
                                (difftastic--rerun-file-buf
                                 "test"
                                 (cons test-file (current-buffer))
                                 metadata
                                 test-file))
                          :type 'user-error))))
              (should
               (equal data
                      "Buffer has the same contents a visited file")))
            (should (equal orig-rerun-alist metadata))))

      (when (and (cdr file-buf) (file-exists-p (car file-buf)))
        (delete-file (car file-buf)))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest difftastic--rerun-file-buf:temporary-non-live-buffer-error-signaled ()
  (let (file-buf)
    (unwind-protect
        (let ((text-quoting-style 'straight)
              buffer metadata orig-rerun-alist)
          (ert-with-test-buffer ()
            (setq buffer (current-buffer)))

          (setq metadata `((file-buf-test . ("test-file" . ,buffer)))
                orig-rerun-alist (copy-tree metadata))
          (let ((data (cadr
                       (should-error
                        (setq file-buf
                              (difftastic--rerun-file-buf
                               "test"
                               (alist-get 'file-buf-test metadata)
                               metadata))
                        :type 'user-error))))
            (should
             (equal data
                    "Buffer test [#<killed buffer>] doesn't exist anymore")))

          (should (equal orig-rerun-alist metadata)))

      (when (and (cdr file-buf) (file-exists-p (car file-buf)))
        (delete-file (car file-buf))))))


(ert-deftest difftastic--url-matcher:basic ()
  ;; N.B. no / in punctuation [sic!]
  (let ((difftastic-buttonize-urls t)
        (punctuation '("" "." "," ":" ";" "?" "!" "@" "#" "$" "%" "^" "&" "*"
                       "(" ")" "[" "]" "{" "}" "<" ">"
                       "\"" "'" "-" "_" "~" "|" "\\")))
    (dolist (prefix '("http://" "https://" "ftp://" "www."))
      (dolist (domain '("gnu.org" "gnu-with-minus.org" "gnu_with_underscore.org"))
        (dolist (suffix '("" "/" "/gnu/gnu.html" "/gnu/gnu.html?arg1=1&arg2=2" "/gnu/gnu.html#hash"))
          (dolist (trailer-1 punctuation)
            (dolist (trailer-2 punctuation)
              (let ((address (concat prefix domain suffix))
                    (trailer (concat trailer-1 trailer-2)))
                (ert-with-test-buffer (:name (format "address: %s trailer: '%s' (no new line)"
                                                     address trailer))
                  (insert "test text\n")
                  (insert address)
                  (insert trailer)
                  (goto-char (point-min))
                  (should (difftastic--url-matcher (point-max)))
                  (should (equal address (match-string-no-properties 1))))
                (ert-with-test-buffer (:name (format "address: %s trailer: '%s' (new line)"
                                                     address trailer))
                  (insert "test text \n")
                  (insert address)
                  (insert trailer)
                  (insert "\n")
                  (goto-char (point-min))
                  (should (difftastic--url-matcher (point-max)))
                  (should (equal address (match-string-no-properties 1))))))))))))

(ert-deftest difftastic--url-matcher:invalid-url ()
  (let ((difftastic-buttonize-urls t))
    (ert-with-test-buffer ()
      (insert "brokenhttps://gnu.org")
      (goto-char (point-min))
      (should-not (difftastic--url-matcher (point-max))))))

(ert-deftest difftastic--url-matcher:suppress ()
  (let (difftastic-buttonize-urls)
    (ert-with-test-buffer ()
      (insert "https://gnu.org")
      (goto-char (point-min))
      (should-not (difftastic--url-matcher (point-max))))))

(ert-deftest difftastic--url-matcher:beyond-limit ()
  (let ((difftastic-buttonize-urls t))
    (ert-with-test-buffer ()
      (insert "foo")
      (insert "https://gnu.org")
      (goto-char (point-min))
      (should-not (difftastic--url-matcher 4)))))


(ert-deftest difftastic--make-url-button:match ()
  (let ((difftastic-buttonize-urls t))
    (ert-with-test-buffer ()
      (insert "before https://gnu.org, and after")
      (goto-char (point-min))
      (should (re-search-forward (rx (group "https://gnu.org")) nil t))
      (mocklet (((make-button (~= (lambda (beg)
                                    (equal beg (match-beginning 1))))
                              (~= (lambda (end)
                                    (equal end (match-end 1))))
                              (~= (lambda (&rest args)
                                    (should (plist-get args 'follow-link))
                                    (let ((action (plist-get args 'action)))
                                      (should (functionp action))
                                      (funcall action 'ignore))
                                    t))))
                ((browse-url "https://gnu.org")))
        (difftastic--make-url-button)))))


(ert-deftest difftastic--chunk-regexp:file-chunk-extracted ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (dolist (file '("difftastic.el"
                    "test/difftastic.t.el"
                    "/foo/bar"
                    "with space"))
      (dolist (chunk '(nil
                       "1/2"
                       "2/2"
                       "3/30"
                       "40/40"))
        (dolist (language '("Emacs Lisp"
                            "C++"
                            "Java"))
          (dolist (suffix
                   '(nil
                     "(1 %s parse error, exceeded DFT_PARSE_ERROR_LIMIT)"
                     "(2 %s parse errors, exceeded DFT_PARSE_ERROR_LIMIT)"
                     "(30 %s parse errors, exceeded DFT_PARSE_ERROR_LIMIT)"
                     "(exceeded DFT_GRAPH_LIMIT)"
                     "(1 B exceeded DFT_BYTE_LIMIT)"
                     "(2 KiB exceeded DFT_BYTE_LIMIT)"
                     "(3.00 KiB exceeded DFT_BYTE_LIMIT)"
                     "(4.44 MiB exceeded DFT_BYTE_LIMIT)"
                     "(50.00 GiB exceeded DFT_BYTE_LIMIT)"
                     "(66.66 TiB exceeded DFT_BYTE_LIMIT)"))

            (let ((header (format "%s%s--- %s%s"
                                  file
                                  (if chunk (format " --- %s " chunk) " ")
                                  (if suffix "Text" language)
                                  (if suffix
                                      (format " %s"  (format suffix language))
                                    ""))))
              (ert-with-test-buffer (:name (format "%s-%s-%s-%s"
                                                   file
                                                   chunk
                                                   language
                                                   suffix))
                (difftastic-mode)
                (should-not difftastic--chunk-regexp-chunk)
                (should-not difftastic--chunk-regexp-file)
                (should (string-match (difftastic--chunk-regexp t) header))
                (cond
                 ((not chunk)
                  (should-not (match-string 2 header)))
                 ((string= "1/2" chunk)
                  (should (string-equal "1" (match-string 2 header))))
                 ((string= "2/2" chunk)
                  (should (string-equal "2" (match-string 2 header))))
                 ((string= "3/30" chunk)
                  (should (string-equal "3" (match-string 2 header))))
                 ((string= "40/40" chunk)
                  (should (string-equal "40" (match-string 2 header)))))
                (should-not difftastic--chunk-regexp-chunk)
                (should difftastic--chunk-regexp-file)))))))))

(ert-deftest difftastic--chunk-regexp:no-file-chunk ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (dolist (file '("difftastic.el"
                    "test/difftastic.t.el"
                    "/foo/bar"
                    "with space"))
      (dolist (chunk '(nil
                       "1/2"
                       "2/2"
                       "3/30"
                       "40/40"))
        (dolist (language '("Emacs Lisp"
                            "C++"
                            "Java"))
          (dolist (suffix
                   '(nil
                     "(1 %s parse error, exceeded DFT_PARSE_ERROR_LIMIT)"
                     "(2 %s parse errors, exceeded DFT_PARSE_ERROR_LIMIT)"
                     "(30 %s parse errors, exceeded DFT_PARSE_ERROR_LIMIT)"
                     "(exceeded DFT_GRAPH_LIMIT)"
                     "(1 B exceeded DFT_BYTE_LIMIT)"
                     "(2 KiB exceeded DFT_BYTE_LIMIT)"
                     "(3.00 KiB exceeded DFT_BYTE_LIMIT)"
                     "(4.44 MiB exceeded DFT_BYTE_LIMIT)"
                     "(50.00 GiB exceeded DFT_BYTE_LIMIT)"
                     "(66.66 TiB exceeded DFT_BYTE_LIMIT)"))

            (let ((header (format "%s%s--- %s%s"
                                  file
                                  (if chunk (format " --- %s " chunk) " ")
                                  (if suffix "Text" language)
                                  (if suffix
                                      (format " %s"  (format suffix language))
                                    ""))))
              (ert-with-test-buffer ()
                (difftastic-mode)
                (should-not difftastic--chunk-regexp-chunk)
                (should-not difftastic--chunk-regexp-file)
                (should (string-match (difftastic--chunk-regexp nil) header))
                (should-not (match-string 2 header))
                (should difftastic--chunk-regexp-chunk)
                (should-not difftastic--chunk-regexp-file)))))))))

(ert-deftest difftastic--chunk-regexp:wrong-suffix-not-matched ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (dolist (file '("difftastic.el"
                    "test/difftastic.t.el"
                    "/foo/bar"
                    "with space"))
      (dolist (chunk '(nil
                       "1/2"
                       "2/2"
                       "3/30"
                       "40/40"))
        (dolist (language '("Emacs Lisp"
                            "C++"
                            "Java"))
          (dolist (suffix
                   '(;;@todo "(1 %s parse errors, exceeded DFT_PARSE_ERROR_LIMIT)"
                     "(x %s parse error, exceeded DFT_PARSE_ERROR_LIMIT)"
                     "(x %s parse errors, exceeded DFT_PARSE_ERROR_LIMIT)"
                     "(1.5 %s parse error, exceeded DFT_PARSE_ERROR_LIMIT)"
                     "(1.5 %s parse errors, exceeded DFT_PARSE_ERROR_LIMIT)"
                     "(1x %s parse error, exceeded DFT_PARSE_ERROR_LIMIT)"
                     "(1x %s parse errors, exceeded DFT_PARSE_ERROR_LIMIT)"
                     "(1 Text parse error, exceeded DFT_PARSE_ERROR_LIMIT)"
                     "(3 Text parse errors, exceeded DFT_PARSE_ERROR_LIMIT)"
                     "1 %s parse error, exceeded DFT_PARSE_ERROR_LIMIT"
                     "3 %s parse errors, exceeded DFT_PARSE_ERROR_LIMIT"
                     "exceeded DFT_GRAPH_LIMIT"
                     "1 B exceeded DFT_BYTE_LIMIT"
                     "(1.00 B exceeded DFT_BYTE_LIMIT)"
                     "(1.0 B exceeded DFT_BYTE_LIMIT)"
                     "(2.0 KiB exceeded DFT_BYTE_LIMIT)"
                     "(2.1 KiB exceeded DFT_BYTE_LIMIT)"
                     "(2 XiB exceeded DFT_BYTE_LIMIT)"
                     "(2.10 XiB exceeded DFT_BYTE_LIMIT)"
                     "(3.0x KiB exceeded DFT_BYTE_LIMIT)"
                     "(x.44 MiB exceeded DFT_BYTE_LIMIT)"
                     "(50.001 GiB exceeded DFT_BYTE_LIMIT)"))

            (let ((header (format "%s%s--- %s%s"
                                  file
                                  (if chunk (format " --- %s " chunk) " ")
                                  (if suffix "Text" language)
                                  (if suffix
                                      (format " %s"  (format suffix language))
                                    ""))))
              (ert-with-test-buffer ()
                (difftastic-mode)
                (should-not
                 (string-match-p (difftastic--chunk-regexp t) header))
                (should-not
                 (string-match-p (difftastic--chunk-regexp nil) header))))))))))

(ert-deftest difftastic--chunk-regexp:unknown-language-not-matched ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (dolist (file '("difftastic.el"
                    "test/difftastic.t.el"
                    "/foo/bar"
                    "with space"))
      (dolist (chunk '(nil
                       "1/2"
                       "2/2"
                       "3/30"
                       "40/40"))
        (dolist (language '("foo"
                            "Not A Language"))
          (dolist (suffix
                   '(nil
                     "(1 %s parse error, exceeded DFT_PARSE_ERROR_LIMIT)"
                     "(2 %s parse errors, exceeded DFT_PARSE_ERROR_LIMIT)"
                     "(30 %s parse errors, exceeded DFT_PARSE_ERROR_LIMIT)"))

            (let ((header (format "%s%s--- %s%s"
                                  file
                                  (if chunk (format " --- %s " chunk) " ")
                                  (if suffix "Text" language)
                                  (if suffix
                                      (format " %s"  (format suffix language))
                                    ""))))
              (ert-with-test-buffer ()
                (difftastic-mode)
                (should-not
                 (string-match-p (difftastic--chunk-regexp t) header))
                (should-not
                 (string-match-p (difftastic--chunk-regexp nil) header))))))))))

(ert-deftest difftastic--chunk-regexp:wrong-chunk-not-extracted()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (dolist (file '("difftastic.el"
                    "test/difftastic.t.el"
                    "/foo/bar"
                    "with space"))
      (dolist (chunk '("x/2"
                       "2/x"
                       "3x/3"
                       "4/4x"
                       "foo"))
        (dolist (language '("Emacs Lisp"
                            "C++"
                            "Java"))
          (dolist (suffix
                   '(nil
                     "(1 %s parse error, exceeded DFT_PARSE_ERROR_LIMIT)"
                     "(2 %s parse errors, exceeded DFT_PARSE_ERROR_LIMIT)"
                     "(30 %s parse errors, exceeded DFT_PARSE_ERROR_LIMIT)"
                     "(exceeded DFT_GRAPH_LIMIT)"
                     "(1 B exceeded DFT_BYTE_LIMIT)"
                     "(2 KiB exceeded DFT_BYTE_LIMIT)"
                     "(3.00 KiB exceeded DFT_BYTE_LIMIT)"
                     "(4.44 MiB exceeded DFT_BYTE_LIMIT)"
                     "(50.00 GiB exceeded DFT_BYTE_LIMIT)"
                     "(66.66 TiB exceeded DFT_BYTE_LIMIT)"))

            (let ((header (format "%s%s--- %s%s"
                                  file
                                  (if chunk (format " --- %s " chunk) " ")
                                  (if suffix "Text" language)
                                  (if suffix
                                      (format " %s"  (format suffix language))
                                    ""))))
              (ert-with-test-buffer ()
                (difftastic-mode)
                (should (string-match (difftastic--chunk-regexp t) header))
                (should-not (match-string 2 header))
                (should
                 (string-match-p (difftastic--chunk-regexp nil) header))))))))))


(ert-deftest difftastic--point-at-added-removed-p:chunk-header ()
  (dolist (chunk-header '("difftastic.el --- Emacs Lisp"
                          "difftastic.el --- 1/2 --- Emacs Lisp"
                          "./difftastic.el --- Emacs Lisp"
                          "../difftastic.el --- Emacs Lisp"
                          "../difftastic.el --- Emacs Lisp"
                          "1difftastic.el --- Emacs Lisp"
                          "12difftastic.el --- Emacs Lisp"
                          "1/difftastic.el --- Emacs Lisp"
                          "12/difftastic.el --- Emacs Lisp"
                          "a . difftastic.el --- Emacs Lisp"
                          "1a .. difftastic.el --- Emacs Lisp"))
    (ert-with-test-buffer ()
      (insert chunk-header)
      (should-not
       (let ((added-removed (difftastic--point-at-added-removed-p)))
         (when added-removed
           (message "failed chunk-header: %s" chunk-header))
         added-removed)))))

(ert-deftest difftastic--point-at-added-removed-p:added-removed ()
  (dolist (added-removed '(". difftastic.el --- Emacs Lisp"
                           ". a difftastic.el --- Emacs Lisp"
                           ". 1 difftastic.el --- Emacs Lisp"
                           ". 12 difftastic.el --- Emacs Lisp"
                           ".. 1a difftastic.el --- Emacs Lisp"
                           ".. 12 difftastic.el --- Emacs Lisp"
                           ".. 1a difftastic.el --- Emacs Lisp"
                           ".. 123 difftastic.el --- Emacs Lisp"
                           "... 123 difftastic.el --- Emacs Lisp"
                           "12 difftastic.el --- Emacs Lisp"
                           "12 . difftastic.el --- Emacs Lisp"
                           "12 .. difftastic.el --- Emacs Lisp"
                           "123 .. difftastic.el --- Emacs Lisp"
                           "123 ... difftastic.el --- Emacs Lisp"))
    (ert-with-test-buffer ()
      (insert added-removed)
      (should (let ((added-removed (difftastic--point-at-added-removed-p)))
                (unless added-removed
                  (message "failed added-removed: %s" added-removed))
                added-removed)))))


(ert-deftest difftastic-next-chunk:erts-scenarios ()
  (when (fboundp 'ert-test-erts-file) ;; since Emacs-29
    (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
      (let ((file "difftastic-next-chunk.erts")
            scenarios)
        (should (or (file-exists-p file)
                    (file-exists-p (format "test/%s" file))))
        (setq scenarios (if (file-exists-p file)
                            file
                          (format "test/%s" file)))
        (ert-test-erts-file scenarios)))))

(ert-deftest difftastic-next-chunk:empty-buffer-error-signaled ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (ert-with-test-buffer ()
      (difftastic-mode)
      (let ((data (cadr
                   (should-error (difftastic-next-chunk)
                                 :type 'user-error))))
        (should (equal data "No more chunks"))))))

(ert-deftest difftastic-next-chunk:last-chunk-error-signaled ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (ert-with-test-buffer ()
      (insert "difftastic.el --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-")
      (difftastic-mode)
      (goto-char (point-min))
      (let ((data (cadr (should-error (difftastic-next-chunk)
                                      :type 'user-error))))
        (should (equal data "No more chunks")))
      (should (equal (point-min) (point))))))

(ert-deftest difftastic-next-chunk:skip-over-hidden ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (ert-with-test-buffer ()
      (insert (propertize "difftastic.el --- 1/2 --- Emacs Lisp"
                          'difftastic '(:hidden :file))
              (propertize "
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/2 --- Emacs Lisp
24 ;;; Commentary:"
                          'invisible 'difftastic)
              "

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-")
      (difftastic-mode)
      (goto-char (point-min))
      (difftastic-next-chunk)
      (should (equal 178 (point))))))


(ert-deftest difftastic-next-file:erts-scenarios ()
  (when (fboundp 'ert-test-erts-file) ;; since Emacs-29
    (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
      (let ((file "difftastic-next-file.erts")
            scenarios)
        (should (or (file-exists-p file)
                    (file-exists-p (format "test/%s" file))))
        (setq scenarios (if (file-exists-p file)
                            file
                          (format "test/%s" file)))
        (ert-test-erts-file scenarios)))))

(ert-deftest difftastic-next-file:empty-buffer-error-signaled ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (ert-with-test-buffer ()
      (difftastic-mode)
      (let ((data (cadr (should-error (difftastic-next-file)
                                      :type 'user-error))))
        (should (equal data "No more files"))))))

(ert-deftest difftastic-next-file:last-chunk-error-signaled ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (ert-with-test-buffer ()
      (insert "difftastic.el --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-")
      (difftastic-mode)
      (goto-char (point-min))
      (let ((data (cadr (should-error (difftastic-next-file)
                                      :type 'user-error))))
        (should (equal data "No more files")))
      (should (equal (point-min) (point))))))


(ert-deftest difftastic-previous-chunk:erts-scenarios ()
  (when (fboundp 'ert-test-erts-file) ;; since Emacs-29
    (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
      (let ((file "difftastic-previous-chunk.erts")
            scenarios)
        (should (or (file-exists-p file)
                    (file-exists-p (format "test/%s" file))))
        (setq scenarios (if (file-exists-p file)
                            file
                          (format "test/%s" file)))
        (ert-test-erts-file scenarios)))))

(ert-deftest difftastic-previous-chunk:empty-buffer-error-signaled ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (ert-with-test-buffer ()
      (difftastic-mode)
      (let ((data (cadr (should-error (difftastic-previous-chunk)
                                      :type 'user-error))))
        (should (equal data "No more chunks"))))))

(ert-deftest difftastic-previous-chunk:first-chunk-error-signaled ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (ert-with-test-buffer ()
      (insert "difftastic.el --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-")
      (difftastic-mode)
      (goto-char (point-min))
      (let ((data (cadr (should-error (difftastic-previous-chunk)
                                      :type 'user-error))))
        (should (equal data "No more chunks")))
      (should (equal (point-min) (point))))))

(ert-deftest difftastic-previous-chunk:skip-over-hidden ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (ert-with-test-buffer ()
      (insert (propertize "difftastic.el --- 1/2 --- Emacs Lisp"
                          'difftastic '(:hidden :file))
              (propertize "
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/2 --- Emacs Lisp
24 ;;; Commentary:"
                          'invisible 'difftastic)
              "

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-")
      (difftastic-mode)
      (goto-char 177)
      (difftastic-previous-chunk)
      (should (equal (point-min) (point))))))


(ert-deftest difftastic-previous-file:erts-scenarios ()
  (when (fboundp 'ert-test-erts-file) ;; since Emacs-29
    (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
      (let ((file "difftastic-previous-file.erts")
            scenarios)
        (should (or (file-exists-p file)
                    (file-exists-p (format "test/%s" file))))
        (setq scenarios (if (file-exists-p file)
                            file
                          (format "test/%s" file)))
        (ert-test-erts-file scenarios)))))

(ert-deftest difftastic-previous-file:empty-buffer-error-signaled ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (ert-with-test-buffer ()
      (difftastic-mode)
      (let ((data (cadr (should-error (difftastic-previous-file)
                                      :type 'user-error))))
        (should (equal data "No more files"))))))

(ert-deftest difftastic-previous-file:first-file-error-signaled ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (ert-with-test-buffer ()
      (insert "difftastic.el --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-")
      (difftastic-mode)
      (goto-char (point-min))
      (let ((data (cadr (should-error (difftastic-previous-file)
                                      :type 'user-error))))
        (should (equal data "No more files")))
      (should (equal (point-min) (point))))))


(ert-deftest difftastic--update-visibility-indcators:basic ()
  (ert-with-test-buffer ()
    (insert "foobar")
    (let ((ov-1 (make-overlay 1 2))
          (ov-2 (make-overlay 2 3))
          (ov-3 (make-overlay 3 4))
          (ov-4 (make-overlay 4 5))
          (ov-5 (make-overlay 5 6)))
      (overlay-put ov-1 'before-string "test-ov-1")
      (overlay-put ov-1 'difftastic-visibility-indicator t)
      (overlay-put ov-2 'before-string "test-ov-2")
      (overlay-put ov-2 'difftastic-visibility-indicator t)
      (overlay-put ov-3 'before-string "test-ov-3")
      (overlay-put ov-4 'before-string "test-ov-4")
      (overlay-put ov-4 'difftastic-visibility-indicator t)
      (overlay-put ov-5 'before-string "test-ov-5")
      (overlay-put ov-5 'difftastic-visibility-indicator t)
      (mocklet (((fringe-bitmap-p "test-indicator") => t))
        (difftastic--update-visibility-indcators "test-indicator" 2 5))
      (if (and (fboundp 'ert-equal-including-properties)
               (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ; Until Emacs-28
          (progn
            (should (ert-equal-including-properties
                     (overlay-get ov-1 'before-string)
                     "test-ov-1"))
            (should (ert-equal-including-properties
                     (overlay-get ov-2 'before-string)
                     (propertize "fringe"
                                 'display '(left-fringe "test-indicator" fringe))))
            (should (ert-equal-including-properties
                     (overlay-get ov-3 'before-string)
                     "test-ov-3"))
            (should (ert-equal-including-properties
                     (overlay-get ov-4 'before-string)
                     (propertize "fringe"
                                 'display '(left-fringe "test-indicator" fringe))))
            (should (ert-equal-including-properties
                     (overlay-get ov-5 'before-string)
                     "test-ov-5")))
        (should (equal-including-properties
                 (overlay-get ov-1 'before-string)
                 "test-ov-1"))
        (should (equal-including-properties
                 (overlay-get ov-2 'before-string)
                 (propertize "fringe"
                             'display '(left-fringe "test-indicator" fringe))))
        (should (equal-including-properties
                 (overlay-get ov-3 'before-string)
                 "test-ov-3"))
        (should (equal-including-properties
                 (overlay-get ov-4 'before-string)
                 (propertize "fringe"
                             'display '(left-fringe "test-indicator" fringe))))
        (should (equal-including-properties
                 (overlay-get ov-5 'before-string)
                 "test-ov-5"))))))


(ert-deftest difftastic-hide-chunk:chunk-hidden ()
  (let ((difftastic-visibility-indicator (cons "test-hidden" "test-shown"))
        (expected
         (concat
          "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

"
          (propertize "difftastic.el --- 2/2 --- Emacs Lisp"
                      'difftastic '(:hidden :chunk))
          (propertize "
24 ;;; Commentary:"
                      'invisible 'difftastic)
          "

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-")))
    (eval
     `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java"))
                ((difftastic--update-visibility-indcators "test-hidden" 121 176)))
        (ert-with-test-buffer ()
          (insert "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/2 --- Emacs Lisp
24 ;;; Commentary:

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-")
          (difftastic-mode)
          (goto-char 121)
          (difftastic-hide-chunk)
          (if (version< "29" emacs-version) ;; since Emacs-29
              (should
               (equal-including-properties (buffer-string) ,expected))
            (should
             (ert-equal-including-properties (buffer-string) ,expected))))))))

(ert-deftest difftastic-hide-chunk:last-chunk-hidden ()
  (let ((difftastic-visibility-indicator (cons "test-hidden" "test-shown"))
        (expected
         (concat
          "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

"
          (propertize "difftastic.el --- 2/2 --- Emacs Lisp"
                      'difftastic '(:hidden :chunk))
          (propertize "
24 ;;; Commentary:"
                      'invisible 'difftastic))))
    (eval
     `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java"))
                ((difftastic--update-visibility-indcators "test-hidden" 121 176)))
        (ert-with-test-buffer ()
          (insert "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/2 --- Emacs Lisp
24 ;;; Commentary:")
          (difftastic-mode)
          (goto-char 121)
          (difftastic-hide-chunk)
          (if (version< "29" emacs-version) ;; since Emacs-29
              (should
               (equal-including-properties (buffer-string) ,expected))
            (should
             (ert-equal-including-properties (buffer-string) ,expected))))))))

(ert-deftest difftastic-hide-chunk:file-chunk-hidden ()
  (let ((difftastic-visibility-indicator (cons "test-hidden" "test-shown"))
        (expected
         (concat
          (propertize "difftastic.el --- 1/2 --- Emacs Lisp"
                      'difftastic '(:hidden :chunk))
          (propertize "
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-"
                      'invisible 'difftastic)
          "

difftastic.el --- 2/2 --- Emacs Lisp
24 ;;; Commentary:

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-")))
    (eval
     `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java"))
                ((difftastic--update-visibility-indcators "test-hidden" 1 119)))
        (ert-with-test-buffer ()
          (insert "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/2 --- Emacs Lisp
24 ;;; Commentary:

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-")
          (difftastic-mode)
          (goto-char (point-min))
          (difftastic-hide-chunk)
          (if (version< "29" emacs-version) ;; since Emacs-29
              (should
               (equal-including-properties (buffer-string) ,expected))
            (should
             (ert-equal-including-properties (buffer-string) ,expected))))))))

(ert-deftest difftastic-hide-chunk:file-header-with-file-file-hidden ()
  (let ((difftastic-visibility-indicator (cons "test-hidden" "test-shown"))
        (expected
         (concat (propertize "difftastic.el --- 1/2 --- Emacs Lisp"
                             'difftastic '(:hidden :file))
                 (propertize "
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/2 --- Emacs Lisp
24 ;;; Commentary:"
                             'invisible 'difftastic)
                 "

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-")))
    (eval `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java"))
                ((difftastic--update-visibility-indcators "test-hidden" 1 176)))
             (ert-with-test-buffer ()
               (insert "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/2 --- Emacs Lisp
24 ;;; Commentary:

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-")
               (difftastic-mode)
               (goto-char (point-min))
               (difftastic-hide-chunk t)
               (if (version< "29" emacs-version) ;; since Emacs-29
                   (should
                    (equal-including-properties (buffer-string) ,expected))
                 (should
                  (ert-equal-including-properties (buffer-string) ,expected))))))))

(ert-deftest difftastic-hide-chunk:last-file-header-with-file-file-hidden ()
  (let ((difftastic-visibility-indicator (cons "test-hidden" "test-shown"))
        (expected
         (concat (propertize "difftastic.el --- 1/2 --- Emacs Lisp"
                             'difftastic '(:hidden :file))
                 (propertize "
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/2 --- Emacs Lisp
24 ;;; Commentary:"
                             'invisible 'difftastic))))
    (eval `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java"))
                ((difftastic--update-visibility-indcators "test-hidden" 1 176)))
             (ert-with-test-buffer ()
               (insert "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/2 --- Emacs Lisp
24 ;;; Commentary:")
               (difftastic-mode)
               (goto-char (point-min))
               (difftastic-hide-chunk t)
               (if (version< "29" emacs-version) ;; since Emacs-29
                   (should
                    (equal-including-properties (buffer-string) ,expected))
                 (should
                  (ert-equal-including-properties (buffer-string) ,expected))))))))

(ert-deftest difftastic-hide-chunk:chunk-header-with-file-rest-of-file-hidden ()
  (let ((difftastic-visibility-indicator (cons "test-hidden" "test-shown"))
        (expected
         (concat "difftastic.el --- 1/3 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

"
                 (propertize "difftastic.el --- 2/3 --- Emacs Lisp"
                             'difftastic '(:hidden :file))
                 (propertize "
24 ;;; Commentary:

difftastic.el --- 3/3 --- Emacs Lisp
231 ;;; Code:"
                             'invisible 'difftastic)
                 "

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-")))
    (eval `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java"))
                ((difftastic--update-visibility-indcators "test-hidden" 121 228)))
             (ert-with-test-buffer ()
               (insert "difftastic.el --- 1/3 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/3 --- Emacs Lisp
24 ;;; Commentary:

difftastic.el --- 3/3 --- Emacs Lisp
231 ;;; Code:

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-")
               (difftastic-mode)
               (goto-char 121)
               (difftastic-hide-chunk t)
               (if (version< "29" emacs-version) ;; since Emacs-29
                   (should
                    (equal-including-properties (buffer-string) ,expected))
                 (should
                  (ert-equal-including-properties (buffer-string) ,expected))))))))

(ert-deftest difftastic-hide-chunk:with-file-with-chunk-hidden-file-hidden ()
  (let ((difftastic-visibility-indicator (cons "test-hidden" "test-shown"))
        (expected
         (concat (propertize "difftastic.el --- 1/2 --- Emacs Lisp"
                             'difftastic '(:hidden :file))
                 (propertize "
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

"
                             'invisible 'difftastic)
                 (propertize "difftastic.el --- 2/2 --- Emacs Lisp"
                             'difftastic '(:hidden :chunk)
                             'invisible 'difftastic)
                 (propertize "
24 ;;; Commentary:"
                             'invisible 'difftastic)
                 "

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-")))
    (eval `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java"))
                ((difftastic--update-visibility-indcators "test-hidden" 1 176)))
             (ert-with-test-buffer ()
               (insert "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

"
                       (propertize "difftastic.el --- 2/2 --- Emacs Lisp"
                                   'difftastic '(:hidden :chunk))
                       (propertize "
24 ;;; Commentary:"
                                   'invisible 'difftastic)
                       "

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-")
               (difftastic-mode)
               (goto-char (point-min))
               (difftastic-hide-chunk t)
               (if (version< "29" emacs-version) ;; since Emacs-29
                   (should
                    (equal-including-properties (buffer-string) ,expected))
                 (should
                  (ert-equal-including-properties (buffer-string) ,expected))))))))


(ert-deftest difftastic-show-chunk:chunk-header-chunk-shown ()
  (let ((difftastic-visibility-indicator (cons "test-hidden" "test-shown"))
        (expected "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/2 --- Emacs Lisp
24 ;;; Commentary:

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-"))
    (eval
     `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java"))
                ((difftastic--update-visibility-indcators "test-shown" 121 176)))
        (ert-with-test-buffer ()
          (insert "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

"
                  (propertize "difftastic.el --- 2/2 --- Emacs Lisp"
                              'difftastic '(:hidden :chunk))
                  (propertize
                   "
24 ;;; Commentary:"
                   'invisible 'difftastic)
                  "

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-")
          (difftastic-mode)
          (goto-char 121)
          (difftastic-show-chunk)
          (should (equal-including-properties (buffer-string) ,expected)))))))

(ert-deftest difftastic-show-chunk:last-chunk-header-chunk-shown ()
  (let ((difftastic-visibility-indicator (cons "test-hidden" "test-shown"))
        (expected "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/2 --- Emacs Lisp
24 ;;; Commentary:"))
    (eval
     `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java"))
                ((difftastic--update-visibility-indcators "test-shown" 121 176)))
        (ert-with-test-buffer ()
          (insert "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

"
                  (propertize "difftastic.el --- 2/2 --- Emacs Lisp"
                              'difftastic '(:hidden :chunk))
                  (propertize
                   "
24 ;;; Commentary:"
                   'invisible 'difftastic))
          (difftastic-mode)
          (goto-char 121)
          (difftastic-show-chunk)
          (should (equal-including-properties (buffer-string) ,expected)))))))

(ert-deftest difftastic-show-chunk:file-header-chunk-shown ()
  (let ((difftastic-visibility-indicator (cons "test-hidden" "test-shown"))
        (expected
         (concat "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

"
                 (propertize "difftastic.el --- 2/2 --- Emacs Lisp"
                             'difftastic '(:hidden :chunk))
                 (propertize "
24 ;;; Commentary:"
                             'invisible 'difftastic)

                 "

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-")))
    (eval
     `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java"))
                ((difftastic--update-visibility-indcators "test-shown" 1 119)))
        (ert-with-test-buffer ()
          (insert (propertize "difftastic.el --- 1/2 --- Emacs Lisp"
                              'difftastic '(:hidden :chunk))
                  (propertize "
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-"
                              'invisible 'difftastic)
                  "

"
                  (propertize "difftastic.el --- 2/2 --- Emacs Lisp"
                              'difftastic '(:hidden :chunk))
                  (propertize
                   "
24 ;;; Commentary:"
                   'invisible 'difftastic)
                  "

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-")
          (difftastic-mode)
          (goto-char (point-min))
          (difftastic-show-chunk)
          (if (version< "29" emacs-version) ;; since Emacs-29
              (should
               (equal-including-properties (buffer-string) ,expected))
            (should
             (ert-equal-including-properties (buffer-string) ,expected))))))))

(ert-deftest difftastic-show-chunk:file-header-file-shown ()
  (let ((difftastic-visibility-indicator (cons "test-hidden" "test-shown"))
        (expected "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/2 --- Emacs Lisp
24 ;;; Commentary:

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-"))
    (eval
     `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java"))
                ((difftastic--update-visibility-indcators "test-shown" 1 176)))
        (ert-with-test-buffer ()
          (insert (propertize "difftastic.el --- 1/2 --- Emacs Lisp"
                              'difftastic '(:hidden :file))
                  (propertize "
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/2 --- Emacs Lisp
24 ;;; Commentary:"
                              'invisible 'difftastic)
                  "

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-")
          (difftastic-mode)
          (goto-char (point-min))
          (difftastic-show-chunk)
          (should (equal-including-properties (buffer-string) ,expected)))))))

(ert-deftest difftastic-show-chunk:last-file-header-file-shown ()
  (let ((difftastic-visibility-indicator (cons "test-hidden" "test-shown"))
        (expected "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/2 --- Emacs Lisp
24 ;;; Commentary:"))
    (eval
     `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java"))
                ((difftastic--update-visibility-indcators "test-shown" 1 176)))
        (ert-with-test-buffer ()
          (insert (propertize "difftastic.el --- 1/2 --- Emacs Lisp"
                              'difftastic '(:hidden :file))
                  (propertize "
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/2 --- Emacs Lisp
24 ;;; Commentary:"
                              'invisible 'difftastic))
          (difftastic-mode)
          (goto-char (point-min))
          (difftastic-show-chunk)
          (should (equal-including-properties (buffer-string) ,expected)))))))

(ert-deftest difftastic-show-chunk:file-header-with-hidden-chunk-file-shown ()
  (let ((difftastic-visibility-indicator (cons "test-hidden" "test-shown"))
        (expected "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/2 --- Emacs Lisp
24 ;;; Commentary:

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-"))
    (eval
     `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java"))
                ((difftastic--update-visibility-indcators "test-shown" 1 176)))
        (ert-with-test-buffer ()
          (insert (propertize "difftastic.el --- 1/2 --- Emacs Lisp"
                              'difftastic '(:hidden :file))
                  (propertize "
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

"
                              'invisible 'difftastic)
                  (propertize "difftastic.el --- 2/2 --- Emacs Lisp"
                              'difftastic '(:hidden :chunk)
                              'invisible 'difftastic)
                  (propertize "
24 ;;; Commentary:"
                              'invisible 'difftastic)
                  "

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-")
          (difftastic-mode)
          (goto-char (point-min))
          (difftastic-show-chunk)
          (should (equal-including-properties (buffer-string) ,expected)))))))


(ert-deftest difftastic-toggle-chunk:chunk-hidden ()
  (let ((difftastic-visibility-indicator (cons "test-hidden" "test-shown"))
        (expected
         (concat
          "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

"
          (propertize "difftastic.el --- 2/2 --- Emacs Lisp"
                      'difftastic '(:hidden :chunk))
          (propertize "
24 ;;; Commentary:"
                      'invisible 'difftastic)
          "

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-")))
    (eval
     `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java"))
                ((difftastic--update-visibility-indcators "test-hidden" 121 176)))
        (ert-with-test-buffer ()
          (insert "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/2 --- Emacs Lisp
24 ;;; Commentary:

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-")
          (difftastic-mode)
          (goto-char 121)
          (difftastic-toggle-chunk)
          (if (version< "29" emacs-version) ;; since Emacs-29
              (should
               (equal-including-properties (buffer-string) ,expected))
            (should
             (ert-equal-including-properties (buffer-string) ,expected))))))))

(ert-deftest difftastic-toggle-chunk:file-chunk-hidden ()
  (let ((difftastic-visibility-indicator (cons "test-hidden" "test-shown"))
        (expected
         (concat
          (propertize "difftastic.el --- 1/2 --- Emacs Lisp"
                      'difftastic '(:hidden :chunk))
          (propertize "
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-"
                      'invisible 'difftastic)
          "

difftastic.el --- 2/2 --- Emacs Lisp
24 ;;; Commentary:

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-")))
    (eval
     `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java"))
                ((difftastic--update-visibility-indcators "test-hidden" 1 119)))
        (ert-with-test-buffer ()
          (insert "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/2 --- Emacs Lisp
24 ;;; Commentary:

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-")
          (difftastic-mode)
          (goto-char (point-min))
          (difftastic-toggle-chunk)
          (if (version< "29" emacs-version) ;; since Emacs-29
              (should
               (equal-including-properties (buffer-string) ,expected))
            (should
             (ert-equal-including-properties (buffer-string) ,expected))))))))

(ert-deftest difftastic-toggle-chunk:visible-file-header-with-file-rest-of-file-hidden ()
  (let ((difftastic-visibility-indicator (cons "test-hidden" "test-shown"))
        (expected
         (concat (propertize "difftastic.el --- 1/3 --- Emacs Lisp"
                             'difftastic '(:hidden :file))
                 (propertize "
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/3 --- Emacs Lisp
24 ;;; Commentary:

difftastic.el --- 3/3 --- Emacs Lisp
231 ;;; Code:"
                             'invisible 'difftastic)
                 "

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-")))
    (eval
     `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java"))
                ((difftastic--update-visibility-indcators "test-hidden" 1 228)))
        (ert-with-test-buffer ()
          (insert "difftastic.el --- 1/3 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/3 --- Emacs Lisp
24 ;;; Commentary:

difftastic.el --- 3/3 --- Emacs Lisp
231 ;;; Code:

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-")
          (difftastic-mode)
          (goto-char (point-min))
          (difftastic-toggle-chunk t)
          (if (version< "29" emacs-version) ;; since Emacs-29
              (should
               (equal-including-properties (buffer-string) ,expected))
            (should
             (ert-equal-including-properties (buffer-string) ,expected))))))))

(ert-deftest difftastic-toggle-chunk:visible-chunk-header-with-file-rest-of-file-hidden ()
  (let ((difftastic-visibility-indicator (cons "test-hidden" "test-shown"))
        (expected
         (concat "difftastic.el --- 1/3 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

"
                 (propertize "difftastic.el --- 2/3 --- Emacs Lisp"
                             'difftastic '(:hidden :file))
                 (propertize "
24 ;;; Commentary:

difftastic.el --- 3/3 --- Emacs Lisp
231 ;;; Code:"
                             'invisible 'difftastic)
                 "

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-")))
    (eval
     `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java"))
                ((difftastic--update-visibility-indcators "test-hidden" 121 228)))
        (ert-with-test-buffer ()
          (insert "difftastic.el --- 1/3 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/3 --- Emacs Lisp
24 ;;; Commentary:

difftastic.el --- 3/3 --- Emacs Lisp
231 ;;; Code:

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-")
          (difftastic-mode)
          (goto-char 121)
          (difftastic-toggle-chunk t)
          (if (version< "29" emacs-version) ;; since Emacs-29
              (should
               (equal-including-properties (buffer-string) ,expected))
            (should
             (ert-equal-including-properties (buffer-string) ,expected))))))))

(ert-deftest difftastic-toggle-chunk:chunk-header-chunk-shown ()
  (let ((difftastic-visibility-indicator (cons "test-hidden" "test-shown"))
        (expected "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/2 --- Emacs Lisp
24 ;;; Commentary:

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-"))
    (eval
     `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java"))
                ((difftastic--update-visibility-indcators "test-shown" 121 176)))
        (ert-with-test-buffer ()
          (insert "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

"
                  (propertize "difftastic.el --- 2/2 --- Emacs Lisp"
                              'difftastic '(:hidden :chunk))
                  (propertize
                   "
24 ;;; Commentary:"
                   'invisible 'difftastic)
                  "

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-")
          (difftastic-mode)
          (goto-char 121)
          (difftastic-toggle-chunk)
          (should (equal-including-properties (buffer-string) ,expected)))))))

(ert-deftest difftastic-toggle-chunk:file-header-chunk-shown ()
  (let ((difftastic-visibility-indicator (cons "test-hidden" "test-shown"))
        (expected
         (concat "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

"
                 (propertize "difftastic.el --- 2/2 --- Emacs Lisp"
                             'difftastic '(:hidden :chunk))
                 (propertize "
24 ;;; Commentary:"
                             'invisible 'difftastic)

                 "

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-")))
    (eval
     `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java"))
                ((difftastic--update-visibility-indcators "test-shown" 1 119)))
        (ert-with-test-buffer ()
          (insert (propertize "difftastic.el --- 1/2 --- Emacs Lisp"
                              'difftastic '(:hidden :chunk))
                  (propertize "
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-"
                              'invisible 'difftastic)
                  "

"
                  (propertize "difftastic.el --- 2/2 --- Emacs Lisp"
                              'difftastic '(:hidden :chunk))
                  (propertize
                   "
24 ;;; Commentary:"
                   'invisible 'difftastic)
                  "

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-")
          (difftastic-mode)
          (goto-char (point-min))
          (difftastic-toggle-chunk)
          (if (version< "29" emacs-version) ;; since Emacs-29
              (should
               (equal-including-properties (buffer-string) ,expected))
            (should
             (ert-equal-including-properties (buffer-string) ,expected))))))))

(ert-deftest difftastic-toggle-chunk:file-header-file-shown ()
  (let ((difftastic-visibility-indicator (cons "test-hidden" "test-shown"))
        (expected "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/2 --- Emacs Lisp
24 ;;; Commentary:

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-"))
    (eval
     `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java"))
                ((difftastic--update-visibility-indcators "test-shown" 1 176)))
        (ert-with-test-buffer ()
          (insert (propertize "difftastic.el --- 1/2 --- Emacs Lisp"
                              'difftastic '(:hidden :file))
                  (propertize "
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/2 --- Emacs Lisp
24 ;;; Commentary:"
                              'invisible 'difftastic)
                  "

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-")
          (difftastic-mode)
          (goto-char (point-min))
          (difftastic-toggle-chunk)
          (should (equal-including-properties (buffer-string) ,expected)))))))


(ert-deftest difftastic--forward-chunk:erts-scenarios ()
  (when (fboundp 'ert-test-erts-file) ;; since Emacs-29
    (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
      (let ((file "difftastic--forward-chunk.erts")
            scenarios)
        (should (or (file-exists-p file)
                    (file-exists-p (format "test/%s" file))))
        (setq scenarios (if (file-exists-p file)
                            file
                          (format "test/%s" file)))
        (ert-test-erts-file scenarios)))))


(ert-deftest difftastic--chunk-bounds:file-chunk-point-at-beginning ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (ert-with-test-buffer ()
      (insert "difftastic.el --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-
")
      (goto-char (point-min))
      (should (equal (cons (point-min) (1- (point-max)))
                     (difftastic--chunk-bounds))))))

(ert-deftest difftastic--chunk-bounds:file-chunk-point-at-end ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (ert-with-test-buffer ()
      (insert "difftastic.el --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-
")
      (goto-char (1- (point-max)))
      (should (equal (cons (point-min) (1- (point-max)))
                     (difftastic--chunk-bounds))))))

(ert-deftest difftastic--chunk-bounds:last-file-chunk-point-at-beginning ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (ert-with-test-buffer ()
      (insert "difftastic.el --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

")
      (goto-char (point-min))
      (should (equal (cons (point-min) (- (point-max) 2))
                     (difftastic--chunk-bounds))))))

(ert-deftest difftastic--chunk-bounds:last-file-chunk-point-at-end ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (ert-with-test-buffer ()
      (insert "difftastic.el --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

")
      (goto-char (- (point-max) 2))
      (should (equal (cons (point-min) (- (point-max) 2))
                     (difftastic--chunk-bounds))))))

(ert-deftest difftastic--chunk-bounds:file-chunk-point-after ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (ert-with-test-buffer ()
      (insert "difftastic.el --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-
")
      (goto-char (point-max))
      (should-not (difftastic--chunk-bounds)))))

(ert-deftest difftastic--chunk-bounds:chunk-point-at-beginning ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (ert-with-test-buffer ()
      (insert "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-
")
      (goto-char (point-min))
      (should (equal (cons (point-min) (1- (point-max)))
                     (difftastic--chunk-bounds))))))

(ert-deftest difftastic--chunk-bounds:chunk-point-at-end ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (ert-with-test-buffer ()
      (insert "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-
")
      (goto-char (1- (point-max)))
      (should (equal (cons (point-min) (1- (point-max)))
                     (difftastic--chunk-bounds))))))

(ert-deftest difftastic--chunk-bounds:last-chunk-point-at-beginning ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (ert-with-test-buffer ()
      (insert "difftastic.el --- 2/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

")
      (goto-char (point-min))
      (should (equal (cons (point-min) (- (point-max) 2))
                     (difftastic--chunk-bounds))))))

(ert-deftest difftastic--chunk-bounds:last-chunk-point-at-end ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (ert-with-test-buffer ()
      (insert "difftastic.el --- 2/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

")
      (goto-char (- (point-max) 2))
      (should (equal (cons (point-min) (- (point-max) 2))
                     (difftastic--chunk-bounds))))))

(ert-deftest difftastic--chunk-bounds:chunk-point-after ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (ert-with-test-buffer ()
      (insert "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-
")
      (goto-char (point-max))
      (should-not (difftastic--chunk-bounds)))))

(ert-deftest difftastic--chunk-bounds:chunk-chunk-point-between ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (ert-with-test-buffer ()
      (insert "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-
")
      (let ((pos (point)))
        (insert "\ndifftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-
")
        (goto-char pos))
      (should-not (difftastic--chunk-bounds)))))

(ert-deftest difftastic--chunk-bounds:file-chunk-chunk-point-between ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (ert-with-test-buffer ()
      (insert "difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-
")
      (let ((pos (point)))
        (insert "\ndifftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-
")
        (goto-char pos))
      (should-not (difftastic--chunk-bounds)))))

(ert-deftest difftastic--chunk-bounds:chunk-file-chunk-point-between ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (ert-with-test-buffer ()
      (insert "difftastic.t.el --- 2/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-
")
      (let ((pos (point)))
        (insert "\ndifftastic.el --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-
")
        (goto-char pos))
      (should-not (difftastic--chunk-bounds)))))

(ert-deftest difftastic--chunk-bounds:file-chunk-file-chunk-point-between ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (ert-with-test-buffer ()
      (insert "difftastic.el --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-
")
      (let ((pos (point)))
        (insert "\ndifftastic.t.el --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-
")
        (goto-char pos))
      (should-not (difftastic--chunk-bounds)))))


(ert-deftest difftastic--chunk-file-name:file-chunk ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (ert-with-test-buffer ()
      (insert "difftastic.el --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-")
      (should (equal "difftastic.el"
                     (difftastic--chunk-file-name
                      (cons (point-min) (point-max))))))))

(ert-deftest difftastic--chunk-file-name:chunk ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (ert-with-test-buffer ()
      (insert "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-")
      (should (equal "difftastic.el"
                     (difftastic--chunk-file-name
                      (cons (point-min) (point-max))))))))


(ert-deftest difftastic--line-num-rx:match ()
  (should (eql 0
               (string-match-p (rx-to-string
                                `(seq string-start
                                      ,(difftastic--line-num-rx 1)))
                               "1")))
  (should (eql 0
               (string-match-p (rx-to-string
                                `(seq string-start
                                      ,(difftastic--line-num-rx 1)))
                               "1 ")))
  (should (eql 0
               (string-match-p (rx-to-string
                                `(seq string-start
                                      ,(difftastic--line-num-rx 1)))
                               "1\n")))
  (should (eql 0
               (string-match-p (rx-to-string
                                `(seq string-start
                                      ,(difftastic--line-num-rx 1)))
                               ".")))
  (should (eql 0
               (string-match-p (rx-to-string
                                `(seq string-start
                                      ,(difftastic--line-num-rx 1)))
                               ". ")))
  (should (eql 0
               (string-match-p (rx-to-string
                                `(seq string-start
                                      ,(difftastic--line-num-rx 1)))
                               ".\n")))
  (should (eql 0
               (string-match-p (rx-to-string
                                `(seq string-start
                                      ,(difftastic--line-num-rx 2)))
                               "22")))
  (should (eql 0
               (string-match-p (rx-to-string
                                `(seq string-start
                                      ,(difftastic--line-num-rx 2)))
                               "22 ")))
  (should (eql 0
               (string-match-p (rx-to-string
                                `(seq string-start
                                      ,(difftastic--line-num-rx 2)))
                               "22\n")))
  (should (eql 0
               (string-match-p (rx-to-string
                                `(seq string-start
                                      ,(difftastic--line-num-rx 2)))
                               " 2")))
  (should (eql 0
               (string-match-p (rx-to-string
                                `(seq string-start
                                      ,(difftastic--line-num-rx 2)))
                               " 2 ")))
  (should (eql 0
               (string-match-p (rx-to-string
                                `(seq string-start
                                      ,(difftastic--line-num-rx 2)))
                               " 2\n")))
  (should (eql 0
               (string-match-p (rx-to-string
                                `(seq string-start
                                      ,(difftastic--line-num-rx 3)))
                               " 3")))
  (should (eql 0
               (string-match-p (rx-to-string
                                `(seq string-start
                                      ,(difftastic--line-num-rx 3)))
                               " 3 ")))
  (should (eql 0
               (string-match-p (rx-to-string
                                `(seq string-start
                                      ,(difftastic--line-num-rx 3)))
                               " 3\n")))
  (should (eql 0
               (string-match-p (rx-to-string
                                `(seq string-start
                                      ,(difftastic--line-num-rx 3)))
                               "  3")))
  (should (eql 0
               (string-match-p (rx-to-string
                                `(seq string-start
                                      ,(difftastic--line-num-rx 3)))
                               "  3 ")))
  (should (eql 0
               (string-match-p (rx-to-string
                                `(seq string-start
                                      ,(difftastic--line-num-rx 3)))
                               "  3\n")))
  (should (eql 0
               (string-match-p (rx-to-string
                                `(seq string-start
                                      ,(difftastic--line-num-rx 2)))
                               "..")))
  (should (eql 0
               (string-match-p (rx-to-string
                                `(seq string-start
                                      ,(difftastic--line-num-rx 2)))
                               ".. ")))
  (should (eql 0
               (string-match-p (rx-to-string
                                `(seq string-start
                                      ,(difftastic--line-num-rx 2)))
                               "..\n")))
  (should (eql 0
               (string-match-p (rx-to-string
                                `(seq string-start
                                      ,(difftastic--line-num-rx 2)))
                               " .")))
  (should (eql 0
               (string-match-p (rx-to-string
                                `(seq string-start
                                      ,(difftastic--line-num-rx 2)))
                               " . ")))
  (should (eql 0
               (string-match-p (rx-to-string
                                `(seq string-start
                                      ,(difftastic--line-num-rx 2)))
                               " .\n")))
  (should (eql 0
               (string-match-p (rx-to-string
                                `(seq string-start
                                      ,(difftastic--line-num-rx 3)))
                               "  .")))
  (should (eql 0
               (string-match-p (rx-to-string
                                `(seq string-start
                                      ,(difftastic--line-num-rx 3)))
                               "  . ")))
  (should (eql 0
               (string-match-p (rx-to-string
                                `(seq string-start
                                      ,(difftastic--line-num-rx 3)))
                               "  .\n")))
  (should (eql 0
               (string-match-p (rx-to-string
                                `(seq string-start
                                      ,(difftastic--line-num-rx 3)))
                               " .")))
  (should (eql 0
               (string-match-p (rx-to-string
                                `(seq string-start
                                      ,(difftastic--line-num-rx 3)))
                               " . ")))
  (should (eql 0
               (string-match-p (rx-to-string
                                `(seq string-start
                                      ,(difftastic--line-num-rx 3)))
                               " .\n"))))

(ert-deftest difftastic--line-num-rx:no-match ()
  (should-not (string-match-p (rx-to-string
                               `(seq string-start
                                     ,(difftastic--line-num-rx 1)))
                              " "))
  (should-not (string-match-p (rx-to-string
                               `(seq string-start
                                     ,(difftastic--line-num-rx 1)))
                              " 1"))
  (should-not (string-match-p (rx-to-string
                               `(seq string-start
                                     ,(difftastic--line-num-rx 1)))
                              " ."))
  (should-not (string-match-p (rx-to-string
                               `(seq string-start
                                     ,(difftastic--line-num-rx 1)))
                              "22"))
  (should-not (string-match-p (rx-to-string
                               `(seq string-start
                                     ,(difftastic--line-num-rx 1)))
                              ".."))
  (should-not (string-match-p (rx-to-string
                               `(seq string-start
                                     ,(difftastic--line-num-rx 2)))
                              " 22"))
  (should-not (string-match-p (rx-to-string
                               `(seq string-start
                                     ,(difftastic--line-num-rx 2)))
                              " .."))
  (should-not (string-match-p (rx-to-string
                               `(seq string-start
                                     ,(difftastic--line-num-rx 2)))
                              ".2"))
  (should-not (string-match-p (rx-to-string
                               `(seq string-start
                                     ,(difftastic--line-num-rx 2)))
                              "2."))
  (should-not (string-match-p (rx-to-string
                               `(seq string-start
                                     ,(difftastic--line-num-rx 3)))
                              " 2."))
  (should-not (string-match-p (rx-to-string
                               `(seq string-start
                                     ,(difftastic--line-num-rx 3)))
                              " .2")))


(ert-deftest difftastic--classify-chunk:single-column-no-left-first ()
  (ert-with-test-buffer ()
    (insert "  1         bar
1 2         foo
. 3         bar
2 4         foo
")
    (should (eq (difftastic--classify-chunk (cons (point-min) (point-max)))
                'single-column))))

(ert-deftest difftastic--classify-chunk:single-column-no-right-first ()
  (ert-with-test-buffer ()
    (insert "1           bar
2 1         foo
3 .         bar
4 2         foo
")
    (should (eq (difftastic--classify-chunk (cons (point-min) (point-max)))
                'single-column))))

(ert-deftest difftastic--classify-chunk:single-column-no-left-last ()
  (ert-with-test-buffer ()
    (insert "1 1         foo
2 2         foo
  3         qux
")
    (should (eq (difftastic--classify-chunk (cons (point-min) (point-max)))
                'single-column))))

(ert-deftest difftastic--classify-chunk:single-column-no-right-last ()
  (ert-with-test-buffer ()
    (insert "1 1         foo
2 2         foo
3           qux
")
    (should (eq (difftastic--classify-chunk (cons (point-min) (point-max)))
                'single-column))))

(ert-deftest difftastic--classify-chunk:side-by-side-no-left-first ()
  (ert-with-test-buffer ()
    (insert "                              1         baz
1         foo                 2         foo
2         foo                 3         baz
")
    (should (eq (difftastic--classify-chunk (cons (point-min) (point-max)))
                'side-by-side))))

(ert-deftest difftastic--classify-chunk:side-by-side-dot-left-first ()
  (ert-with-test-buffer ()
    (insert ".                             1         baz
1         foo                 2         foo
2         foo                 3         baz
3         qux
")
    (should (eq (difftastic--classify-chunk (cons (point-min) (point-max)))
                'side-by-side))))

(ert-deftest difftastic--classify-chunk:side-by-side-no-right-first ()
  (ert-with-test-buffer ()
    (insert "1         baz
2         foo                 1         foo
3         baz                 2         foo
")
    (should (eq (difftastic--classify-chunk (cons (point-min) (point-max)))
                'side-by-side))))

(ert-deftest difftastic--classify-chunk:side-by-side-dot-right-first ()
  (ert-with-test-buffer ()
    (insert "1         baz                 .
2         foo                 1         foo
3         baz                 2         foo
")
    (should (eq (difftastic--classify-chunk (cons (point-min) (point-max)))
                'side-by-side))))

(ert-deftest difftastic--classify-chunk:side-by-side-no-left-last ()
  (ert-with-test-buffer ()
    (insert "1         baz                 1         qux
2         foo                 1         foo
3         baz                 2         foo
                              3         qux
")
    (should (eq (difftastic--classify-chunk (cons (point-min) (point-max)))
                'side-by-side))))

(ert-deftest difftastic--classify-chunk:side-by-side-no-right-last ()
  (ert-with-test-buffer ()
    (insert "1         bar                 1         baz
2         foo                 2         foo
3         bar                 3         baz
4         foo
")
    (should (eq (difftastic--classify-chunk (cons (point-min) (point-max)))
                'side-by-side))))


(ert-deftest difftastic--parse-side-by-side-chunk:no-left-first ()
  (ert-with-test-buffer ()
    (insert "foo --- Text
                              1         baz
1         foo                 2         foo
2         foo                 3         baz
")
    (should (equal
             (difftastic--parse-side-by-side-chunk
              (cons (point-min) (point-max)))
             '(((14 57)   nil         (1 44 45))
               ((58 101)  (1 58 59)   (2 88 89))
               ((102 145) (2 102 103) (3 132 133)))))))

(ert-deftest difftastic--parse-side-by-side-chunk:dot-left-first ()
  (ert-with-test-buffer ()
    (insert "foo --- Text
.                             1         baz
1         foo                 2         foo
2         foo                 3         baz
3         qux
")
    (should (equal
             (difftastic--parse-side-by-side-chunk
              (cons (point-min) (point-max)))
             '(((14 57)   (nil 14 15) (1 44 45))
               ((58 101)  (1 58 59)   (2 88 89))
               ((102 145) (2 102 103) (3 132 133))
               ((146 159) (3 146 147) nil))))))

(ert-deftest difftastic--parse-side-by-side-chunk:no-right-first ()
  (ert-with-test-buffer ()
    (insert "foo --- Text
1         baz
2         foo                 1         foo
3         baz                 2         foo
")
    (should (equal
             (difftastic--parse-side-by-side-chunk
              (cons (point-min) (point-max)))
             '(((14 27)  (1 14 15) nil)
               ((28 71)  (2 28 29) (1 58 59))
               ((72 115) (3 72 73) (2 102 103)))))))

(ert-deftest difftastic--parse-side-by-side-chunk:dot-right-first ()
  (ert-with-test-buffer ()
    (insert "foo --- Text
1         baz                 .
2         foo                 1         foo
3         baz                 2         foo
")
    (should (equal
             (difftastic--parse-side-by-side-chunk
              (cons (point-min) (point-max)))
             '(((14 45)  (1 14 15) (nil 44 45))
               ((46 89)  (2 46 47) (1 76 77))
               ((90 133) (3 90 91) (2 120 121)))))))

(ert-deftest difftastic--parse-side-by-side-chunk:no-left-last ()
  (ert-with-test-buffer ()
    (insert "foo --- Text
1         baz                 1         qux
2         foo                 1         foo
3         baz                 2         foo
                              3         qux
")
    (should (equal
             (difftastic--parse-side-by-side-chunk
              (cons (point-min) (point-max)))
             '(((14 57)   (1 14 15)   (1 44 45))
               ((58 101)  (2 58 59)   (1 88 89))
               ((102 145) (3 102 103) (2 132 133))
               ((146 189) nil         (3 176 177)))))))

(ert-deftest difftastic--parse-side-by-side-chunk:no-right-last ()
  (ert-with-test-buffer ()
    (insert "foo --- Text
1         bar                 1         baz
2         foo                 2         foo
3         bar                 3         baz
4         foo
")
    (should (equal
             (difftastic--parse-side-by-side-chunk
              (cons (point-min) (point-max)))
             '(((14 57)   (1 14 15)   (1 44 45))
               ((58 101)  (2 58 59)   (2 88 89))
               ((102 145) (3 102 103) (3 132 133))
               ((146 159) (4 146 147) nil))))))

(ert-deftest difftastic--parse-side-by-side-chunk:no-middle-left ()
  (ert-with-test-buffer ()
    (insert "foo --- Text
1         foo                 2         foo
.                             3         bar
2         foo                 4         foo
")
    (should (equal
             (difftastic--parse-side-by-side-chunk
              (cons (point-min) (point-max)))
             '(((14 57)   (1 14 15)   (2 44 45))
               ((58 101)  (1 58 59)   (3 88 89))
               ((102 145) (2 102 103) (4 132 133)))))))

(ert-deftest difftastic--parse-side-by-side-chunk:no-middle-right ()
  (ert-with-test-buffer ()
    (insert "foo --- Text
2         foo                 1         foo
3         bar                 .
4         foo                 2         foo
")
    (should (equal
             (difftastic--parse-side-by-side-chunk
              (cons (point-min) (point-max)))
             '(((14 57)  (2 14 15) (1 44 45))
               ((58 89)  (3 58 59) (1 88 89))
               ((90 133) (4 90 91) (2 120 121)))))))

(ert-deftest difftastic--parse-side-by-side-chunk:different-line-num ()
  (ert-with-test-buffer ()
    (insert "foo --- Text
 9        bar                  99        baz
10        foo                 100        foo
11        bar                 101        baz
")
    (should (equal
             (difftastic--parse-side-by-side-chunk
              (cons (point-min) (point-max)))
             '(((14 58)   (9 15 16)    (99 45 47))
               ((59 103)  (10 59 61)   (100 89 92))
               ((104 148) (11 104 106) (101 134 137)))))))


(ert-deftest difftastic--parse-single-column-chunk:no-left-first ()
  (ert-with-test-buffer ()
    (insert "foo --- Text
  1         bar
1 2         foo
. 3         bar
2 4         foo
")
    (should (equal
             (difftastic--parse-single-column-chunk
              (cons (point-min) (point-max)))
             '(((14 29) nil         (1 16 17))
               ((30 45) (1 30 31)   (2 32 33))
               ((46 61) (1 46 47)   (3 48 49))
               ((62 77) (2 62 63)   (4 64 65)))))))

(ert-deftest difftastic--parse-single-column-chunk:no-right-first ()
  (ert-with-test-buffer ()
    (insert "foo --- Text
1           bar
2 1         foo
3 .         bar
4 2         foo
")
    (should (equal
             (difftastic--parse-single-column-chunk
              (cons (point-min) (point-max)))
             '(((14 29) (1 14 15) nil)
               ((30 45) (2 30 31) (1 32 33))
               ((46 61) (3 46 47) (1 48 49))
               ((62 77) (4 62 63) (2 64 65)))))))

(ert-deftest difftastic--parse-single-column-chunk:no-left-last ()
  (ert-with-test-buffer ()
    (insert "foo --- Text
1 1         foo
2 2         foo
  3         qux
")
    (should (equal
             (difftastic--parse-single-column-chunk
              (cons (point-min) (point-max)))
             '(((14 29) (1 14 15) (1 16 17))
               ((30 45) (2 30 31) (2 32 33))
               ((46 61) nil       (3 48 49)))))))

(ert-deftest difftastic--parse-single-column-chunk:no-right-last ()
  (ert-with-test-buffer ()
    (insert "foo --- Text
1 1         foo
2 2         foo
3           qux
")
    (should (equal
             (difftastic--parse-single-column-chunk
              (cons (point-min) (point-max)))
             '(((14 29) (1 14 15) (1 16 17))
               ((30 45) (2 30 31) (2 32 33))
               ((46 61) (3 46 47) nil))))))

(ert-deftest difftastic--parse-single-column-chunk:different-line-num ()
  (ert-with-test-buffer ()
    (insert "foo --- Text
 9  99         foo
10 100         bar
11 101         foo
")
    (should (equal
             (difftastic--parse-single-column-chunk
              (cons (point-min) (point-max)))
             '(((14 32) (9 15 16)  (99 18 20))
               ((33 51) (10 33 35) (100 36 39))
               ((52 70) (11 52 54) (101 55 58)))))))


(ert-deftest difftastic--chunk-file-at-point:side-by-side-no-right ()
  (mocklet (((difftastic--get-languages) => '("Text" "Emacs Lisp" "C++" "Java")))
    (ert-with-test-buffer ()
      (insert "foo --- Text
 7        bar
 8        bar                  99        baz
 .        foo                  ..        foo
 9        bar                 100        baz
10        bar
")
      (goto-char (point-min))
      (should (equal (difftastic--chunk-file-at-point) '("foo" nil 0 right)))
      (goto-char (compat-call pos-eol)) ;  Since Emacs-29
      (should (equal (difftastic--chunk-file-at-point) '("foo" nil 0 right)))

      (re-search-forward (rx line-start " 7"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 7 0 left)))
      (goto-char (compat-call pos-bol)) ; Since Emacs-29
      (should (equal (cons 'bol (difftastic--chunk-file-at-point))
                     '(bol "foo" 7 0 left)))
      (goto-char (compat-call pos-eol)) ; Since Emacs-29
      (should (equal (cons 'eol (difftastic--chunk-file-at-point))
                     '(eol "foo" 7 10 left)))

      (re-search-forward (rx line-start " 8"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 8 0 left)))
      (goto-char (compat-call pos-bol)) ; Since Emacs-29
      (should (equal (cons 'bol (difftastic--chunk-file-at-point))
                     '(bol "foo" 8 0 left)))
      (re-search-forward (rx " 99"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 99 0 right)))
      (re-search-backward (rx "99"))
      (should (equal (cons 'line-num-beg (difftastic--chunk-file-at-point))
                     '(line-num-beg "foo" 99 0 right)))
      (goto-char (compat-call pos-eol)) ; Since Emacs-29
      (should (equal (cons 'eol (difftastic--chunk-file-at-point))
                     '(eol "foo" 99 10 right)))

      (re-search-forward (rx line-start " ."))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 8 0 left)))
      (goto-char (compat-call pos-bol)) ; Since Emacs-29
      (should (equal (cons 'bol (difftastic--chunk-file-at-point))
                     '(bol "foo" 8 0 left)))
      (re-search-forward (rx " .."))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 99 0 right)))
      (re-search-backward (rx ".."))
      (should (equal (cons 'line-num-beg (difftastic--chunk-file-at-point))
                     '(line-num-beg "foo" 99 0 right)))
      (goto-char (compat-call pos-eol)) ; Since Emacs-29
      (should (equal (cons 'eol (difftastic--chunk-file-at-point))
                     '(eol "foo" 99 10 right)))

      (re-search-forward (rx line-start " 9"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 9  0 left)))
      (goto-char (compat-call pos-bol)) ; Since Emacs-29
      (should (equal (cons 'bol (difftastic--chunk-file-at-point))
                     '(bol "foo" 9 0 left)))
      (re-search-forward (rx " 100"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 100 0 right)))
      (re-search-backward (rx "100"))
      (should (equal (cons 'line-num-beg (difftastic--chunk-file-at-point))
                     '(line-num-beg "foo" 100 0 right)))
      (goto-char (compat-call pos-eol)) ; Since Emacs-29
      (should (equal (cons 'eol (difftastic--chunk-file-at-point))
                     '(eol "foo" 100 10 right)))

      (re-search-forward (rx line-start "10"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 10 0 left)))
      (goto-char (compat-call pos-bol)) ; Since Emacs-29
      (should (equal (cons 'bol (difftastic--chunk-file-at-point))
                     '(bol "foo" 10 0 left)))
      (goto-char (compat-call pos-eol)) ; Since Emacs-29
      (should (equal (cons 'eol (difftastic--chunk-file-at-point))
                     '(eol "foo" 10 10 left))))))

(ert-deftest difftastic--chunk-file-at-point:side-by-side-dot-right ()
  (mocklet (((difftastic--get-languages) => '("Text" "Emacs Lisp" "C++" "Java")))
    (ert-with-test-buffer ()
      (insert "foo --- Text
 7        bar                  ..
 8        bar                  99        baz
 .        foo                  ..        foo
 9        bar                 100        baz
10        bar                 ...
")
      (goto-char (point-min))
      (should (equal (difftastic--chunk-file-at-point) '("foo" nil 0 right)))
      (goto-char (compat-call pos-eol)) ;  Since Emacs-29
      (should (equal (difftastic--chunk-file-at-point) '("foo" nil 0 right)))

      (re-search-forward (rx line-start " 7"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 7 0 left)))
      (goto-char (compat-call pos-bol)) ; Since Emacs-29
      (should (equal (cons 'bol (difftastic--chunk-file-at-point))
                     '(bol "foo" 7 0 left)))
      (re-search-forward (rx " .."))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" nil 0 right)))
      (re-search-backward (rx ".."))
      (should (equal (cons 'line-num-beg (difftastic--chunk-file-at-point))
                     '(line-num-beg "foo" nil 0 right)))
      (goto-char (compat-call pos-eol)) ; Since Emacs-29
      (should (equal (cons 'eol (difftastic--chunk-file-at-point))
                     '(eol "foo" nil 0 right)))

      (re-search-forward (rx line-start " 8"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 8 0 left)))
      (goto-char (compat-call pos-bol)) ; Since Emacs-29
      (should (equal (cons 'bol (difftastic--chunk-file-at-point))
                     '(bol "foo" 8 0 left)))
      (re-search-forward (rx " 99"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 99 0 right)))
      (re-search-backward (rx "99"))
      (should (equal (cons 'line-num-beg (difftastic--chunk-file-at-point))
                     '(line-num-beg "foo" 99 0 right)))
      (goto-char (compat-call pos-eol)) ; Since Emacs-29
      (should (equal (cons 'eol (difftastic--chunk-file-at-point))
                     '(eol "foo" 99 10 right)))

      (re-search-forward (rx line-start " ."))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 8 0 left)))
      (goto-char (compat-call pos-bol)) ; Since Emacs-29
      (should (equal (cons 'bol (difftastic--chunk-file-at-point))
                     '(bol "foo" 8 0 left)))
      (re-search-forward (rx " .."))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 99 0 right)))
      (re-search-backward (rx ".."))
      (should (equal (cons 'line-num-beg (difftastic--chunk-file-at-point))
                     '(line-num-beg "foo" 99 0 right)))
      (goto-char (compat-call pos-eol)) ; Since Emacs-29
      (should (equal (cons 'eol (difftastic--chunk-file-at-point))
                     '(eol "foo" 99 10 right)))

      (re-search-forward (rx line-start " 9"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 9 0 left)))
      (goto-char (compat-call pos-bol)) ; Since Emacs-29
      (should (equal (cons 'bol (difftastic--chunk-file-at-point))
                     '(bol "foo" 9 0 left)))
      (re-search-forward (rx " 100"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 100 0 right)))
      (re-search-backward (rx "100"))
      (should (equal (cons 'line-num-beg (difftastic--chunk-file-at-point))
                     '(line-num-beg "foo" 100 0 right)))
      (goto-char (compat-call pos-eol)) ; Since Emacs-29
      (should (equal (cons 'eol (difftastic--chunk-file-at-point))
                     '(eol "foo" 100 10 right)))

      (re-search-forward (rx line-start "10"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 10 0 left)))
      (goto-char (compat-call pos-bol)) ; Since Emacs-29
      (should (equal (cons 'bol (difftastic--chunk-file-at-point))
                     '(bol "foo" 10 0 left)))
      (re-search-forward (rx " ..."))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 100 0 right)))
      (re-search-backward (rx "..."))
      (should (equal (cons 'line-num-beg (difftastic--chunk-file-at-point))
                     '(line-num-beg "foo" 100 0 right)))
      (goto-char (compat-call pos-eol)) ; Since Emacs-29
      (should (equal (cons 'eol (difftastic--chunk-file-at-point))
                     '(eol "foo" 100 0 right))))))

(ert-deftest difftastic--chunk-file-at-point:side-by-side-no-left ()
  (mocklet (((difftastic--get-languages) => '("Text" "Emacs Lisp" "C++" "Java")))
    (ert-with-test-buffer ()
      (insert "foo --- Text
                               98        bar
 8        bar                  99        baz
 .        foo                  ..        foo
 9        bar                 100        baz
                              101        bar
")
      (goto-char (point-min))
      (should (equal (difftastic--chunk-file-at-point) '("foo" nil 0 right)))
      (goto-char (compat-call pos-eol)) ;  Since Emacs-29
      (should (equal (difftastic--chunk-file-at-point) '("foo" nil 0 right)))

      (re-search-forward (rx " 98"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 98 0 right)))
      (re-search-backward (rx "98"))
      (should (equal (cons 'line-num-beg (difftastic--chunk-file-at-point))
                     '(line-num-beg "foo" 98 0 right)))
      (goto-char (compat-call pos-bol)) ; Since Emacs-29
      (should (equal (cons 'bol (difftastic--chunk-file-at-point))
                     '(bol "foo" 98 0 right)))
      (goto-char (compat-call pos-eol)) ; Since Emacs-29
      (should (equal (cons 'eol (difftastic--chunk-file-at-point))
                     '(eol "foo" 98 10 right)))

      (re-search-forward (rx line-start " 8"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 8 0 left)))
      (goto-char (compat-call pos-bol)) ; Since Emacs-29
      (should (equal (cons 'bol (difftastic--chunk-file-at-point))
                     '(bol "foo" 8 0 left)))
      (re-search-forward (rx " 99"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 99 0 right)))
      (re-search-backward (rx "99"))
      (should (equal (cons 'line-num-beg (difftastic--chunk-file-at-point))
                     '(line-num-beg "foo" 99 0 right)))
      (goto-char (compat-call pos-eol)) ; Since Emacs-29
      (should (equal (cons 'eol (difftastic--chunk-file-at-point))
                     '(eol "foo" 99 10 right)))

      (re-search-forward (rx line-start " ."))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 8 0 left)))
      (goto-char (compat-call pos-bol)) ; Since Emacs-29
      (should (equal (cons 'bol (difftastic--chunk-file-at-point))
                     '(bol "foo" 8 0 left)))
      (re-search-forward (rx " .."))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 99 0 right)))
      (re-search-backward (rx ".."))
      (should (equal (cons 'line-num-beg (difftastic--chunk-file-at-point))
                     '(line-num-beg "foo" 99 0 right)))
      (goto-char (compat-call pos-eol)) ; Since Emacs-29
      (should (equal (cons 'eol (difftastic--chunk-file-at-point))
                     '(eol "foo" 99 10 right)))

      (re-search-forward (rx line-start " 9"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 9 0 left)))
      (goto-char (compat-call pos-bol)) ; Since Emacs-29
      (should (equal (cons 'bol (difftastic--chunk-file-at-point))
                     '(bol "foo" 9 0 left)))
      (re-search-forward (rx " 100"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 100 0 right)))
      (re-search-backward (rx "100"))
      (should (equal (cons 'line-num-beg (difftastic--chunk-file-at-point))
                     '(line-num-beg "foo" 100 0 right)))
      (goto-char (compat-call pos-eol)) ; Since Emacs-29
      (should (equal (cons 'eol (difftastic--chunk-file-at-point))
                     '(eol "foo" 100 10 right)))

      (re-search-forward (rx " 101"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 101 0 right)))
      (re-search-backward (rx "101"))
      (should (equal (cons 'line-num-beg (difftastic--chunk-file-at-point))
                     '(line-num-beg "foo" 101 0 right)))
      (goto-char (compat-call pos-bol)) ; Since Emacs-29
      (should (equal (cons 'bol (difftastic--chunk-file-at-point))
                     '(bol "foo" 101 0 right)))
      (goto-char (compat-call pos-eol)) ; Since Emacs-29
      (should (equal (cons 'eol (difftastic--chunk-file-at-point))
                     '(eol "foo" 101 10 right))))))

(ert-deftest difftastic--chunk-file-at-point:side-by-side-dot-left ()
  (mocklet (((difftastic--get-languages) => '("Text" "Emacs Lisp" "C++" "Java")))
    (ert-with-test-buffer ()
      (insert "foo --- Text
 .                             98        bar
 8        bar                  99        baz
 .        foo                  ..        foo
 9        bar                 100        baz
 .                            101        bar
")
      (goto-char (point-min))
      (should (equal (difftastic--chunk-file-at-point) '("foo" nil 0 right)))
      (goto-char (compat-call pos-eol)) ;  Since Emacs-29
      (should (equal (difftastic--chunk-file-at-point) '("foo" nil 0 right)))

      (re-search-forward (rx line-start " ."))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" nil 0 left)))
      (re-search-backward (rx "."))
      (should (equal (cons 'line-num-beg (difftastic--chunk-file-at-point))
                     '(line-num-beg "foo" nil 0 left)))
      (goto-char (compat-call pos-bol)) ; Since Emacs-29
      (should (equal (cons 'bol (difftastic--chunk-file-at-point))
                     '(bol "foo" nil 0 left)))
      (re-search-forward (rx " 98"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 98 0 right)))
      (re-search-backward (rx "98"))
      (should (equal (cons 'line-num-beg (difftastic--chunk-file-at-point))
                     '(line-num-beg "foo" 98 0 right)))
      (goto-char (compat-call pos-eol)) ; Since Emacs-29
      (should (equal (cons 'eol (difftastic--chunk-file-at-point))
                     '(eol "foo" 98 10 right)))

      (re-search-forward (rx line-start " 8"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 8 0 left)))
      (goto-char (compat-call pos-bol)) ; Since Emacs-29
      (should (equal (cons 'bol (difftastic--chunk-file-at-point))
                     '(bol "foo" 8 0 left)))
      (re-search-forward (rx " 99"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 99 0 right)))
      (re-search-backward (rx "99"))
      (should (equal (cons 'line-num-beg (difftastic--chunk-file-at-point))
                     '(line-num-beg "foo" 99 0 right)))
      (goto-char (compat-call pos-eol)) ; Since Emacs-29
      (should (equal (cons 'eol (difftastic--chunk-file-at-point))
                     '(eol "foo" 99 10 right)))

      (re-search-forward (rx line-start " ."))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 8 0 left)))
      (goto-char (compat-call pos-bol)) ; Since Emacs-29
      (should (equal (cons 'bol (difftastic--chunk-file-at-point))
                     '(bol "foo" 8 0 left)))
      (re-search-forward (rx " .."))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 99 0 right)))
      (re-search-backward (rx ".."))
      (should (equal (cons 'line-num-beg (difftastic--chunk-file-at-point))
                     '(line-num-beg "foo" 99 0 right)))
      (goto-char (compat-call pos-eol)) ; Since Emacs-29
      (should (equal (cons 'eol (difftastic--chunk-file-at-point))
                     '(eol "foo" 99 10 right)))

      (re-search-forward (rx line-start " 9"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 9 0 left)))
      (goto-char (compat-call pos-bol)) ; Since Emacs-29
      (should (equal (cons 'bol (difftastic--chunk-file-at-point))
                     '(bol "foo" 9 0 left)))
      (re-search-forward (rx " 100"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 100 0 right)))
      (re-search-backward (rx "100"))
      (should (equal (cons 'line-num-beg (difftastic--chunk-file-at-point))
                     '(line-num-beg "foo" 100 0 right)))
      (goto-char (compat-call pos-eol)) ; Since Emacs-29
      (should (equal (cons 'eol (difftastic--chunk-file-at-point))
                     '(eol "foo" 100 10 right)))

      (re-search-forward (rx " 101"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 101 0 right)))
      (re-search-backward (rx "101"))
      (should (equal (cons 'line-num-beg (difftastic--chunk-file-at-point))
                     '(line-num-beg "foo" 101 0 right)))
      (goto-char (compat-call pos-bol)) ; Since Emacs-29
      (should (equal (cons 'bol (difftastic--chunk-file-at-point))
                     '(bol "foo" 9 0 left)))
      (goto-char (compat-call pos-eol)) ; Since Emacs-29
      (should (equal (cons 'eol (difftastic--chunk-file-at-point))
                     '(eol "foo" 101 10 right))))))

(ert-deftest difftastic--chunk-file-at-point:single-column-no-right ()
  (mocklet (((difftastic--get-languages) => '("Text" "Emacs Lisp" "C++" "Java")))
    (ert-with-test-buffer ()
      (insert "foo --- Text
 7            foo
 8  99        foo
 .  ..        foo
 9 100        foo
10            foo
")
      (goto-char (point-min))
      (should (equal (difftastic--chunk-file-at-point) '("foo" nil 0 right)))
      (goto-char (compat-call pos-eol)) ;  Since Emacs-29
      (should (equal (difftastic--chunk-file-at-point) '("foo" nil 0 right)))

      (re-search-forward (rx line-start " 7"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 7 0 left)))
      (goto-char (compat-call pos-bol)) ; Since Emacs-29
      (should (equal (cons 'bol (difftastic--chunk-file-at-point))
                     '(bol "foo" 7 0 left)))
      (goto-char (compat-call pos-eol)) ; Since Emacs-29
      (should (equal (cons 'eol (difftastic--chunk-file-at-point))
                     '(eol "foo" 7 14 left))) ; TODO: this one should be in col 10

      (re-search-forward (rx line-start " 8"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 8 0 left)))
      (goto-char (compat-call pos-bol)) ; Since Emacs-29
      (should (equal (cons 'bol (difftastic--chunk-file-at-point))
                     '(bol "foo" 8 0 left)))
      (re-search-forward (rx " 99"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 99 0 right)))
      (re-search-backward (rx "99"))
      (should (equal (cons 'line-num-beg (difftastic--chunk-file-at-point))
                     '(line-num-beg "foo" 99 0 right)))
      (goto-char (compat-call pos-eol)) ; Since Emacs-29
      (should (equal (cons 'eol (difftastic--chunk-file-at-point))
                     '(eol "foo" 99 10 right)))

      (re-search-forward (rx line-start " ."))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 8 0 left)))
      (goto-char (compat-call pos-bol)) ; Since Emacs-29
      (should (equal (cons 'bol (difftastic--chunk-file-at-point))
                     '(bol "foo" 8 0 left)))
      (re-search-forward (rx " .."))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 99 0 right)))
      (re-search-backward (rx ".."))
      (should (equal (cons 'line-num-beg (difftastic--chunk-file-at-point))
                     '(line-num-beg "foo" 99 0 right)))
      (goto-char (compat-call pos-eol)) ; Since Emacs-29
      (should (equal (cons 'eol (difftastic--chunk-file-at-point))
                     '(eol "foo" 99 10 right)))

      (re-search-forward (rx line-start " 9"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 9 0 left)))
      (goto-char (compat-call pos-bol)) ; Since Emacs-29
      (should (equal (cons 'bol (difftastic--chunk-file-at-point))
                     '(bol "foo" 9 0 left)))
      (re-search-forward (rx " 100"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 100 0 right)))
      (re-search-backward (rx "100"))
      (should (equal (cons 'line-num-beg (difftastic--chunk-file-at-point))
                     '(line-num-beg "foo" 100 0 right)))
      (goto-char (compat-call pos-eol)) ; Since Emacs-29
      (should (equal (cons 'eol (difftastic--chunk-file-at-point))
                     '(eol "foo" 100 10 right)))

      (re-search-forward (rx line-start "10"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 10 0 left)))
      (goto-char (compat-call pos-bol)) ; Since Emacs-29
      (should (equal (cons 'bol (difftastic--chunk-file-at-point))
                     '(bol "foo" 10 0 left)))
      (goto-char (compat-call pos-eol)) ; Since Emacs-29
      (should (equal (cons 'eol (difftastic--chunk-file-at-point))
                     '(eol "foo" 10 14 left)))))) ; TODO: this one should be in col 10

(ert-deftest difftastic--chunk-file-at-point:single-column-no-left ()
  (mocklet (((difftastic--get-languages) => '("Text" "Emacs Lisp" "C++" "Java")))
    (ert-with-test-buffer ()
      (insert "foo --- Text
    98        foo
 8  99        foo
 .  ..        foo
 9 100        foo
   101        foo
")
      (goto-char (point-min))
      (should (equal (difftastic--chunk-file-at-point) '("foo" nil 0 right)))
      (goto-char (compat-call pos-eol)) ;  Since Emacs-29
      (should (equal (difftastic--chunk-file-at-point) '("foo" nil 0 right)))

      (re-search-forward (rx " 98"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 98 0 right)))
      (goto-char (compat-call pos-bol)) ; Since Emacs-29
      (should (equal (cons 'bol (difftastic--chunk-file-at-point))
                     '(bol "foo" 98 0 right)))
      (goto-char (compat-call pos-eol)) ; Since Emacs-29
      (should (equal (cons 'eol (difftastic--chunk-file-at-point))
                     '(eol "foo" 98 10 right)))

      (re-search-forward (rx line-start " 8"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 8 0 left)))
      (goto-char (compat-call pos-bol)) ; Since Emacs-29
      (should (equal (cons 'bol (difftastic--chunk-file-at-point))
                     '(bol "foo" 8 0 left)))
      (re-search-forward (rx " 99"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 99 0 right)))
      (re-search-backward (rx "99"))
      (should (equal (cons 'line-num-beg (difftastic--chunk-file-at-point))
                     '(line-num-beg "foo" 99 0 right)))
      (goto-char (compat-call pos-eol)) ; Since Emacs-29
      (should (equal (cons 'eol (difftastic--chunk-file-at-point))
                     '(eol "foo" 99 10 right)))

      (re-search-forward (rx line-start " ."))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 8 0 left)))
      (goto-char (compat-call pos-bol)) ; Since Emacs-29
      (should (equal (cons 'bol (difftastic--chunk-file-at-point))
                     '(bol "foo" 8 0 left)))
      (re-search-forward (rx " .."))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 99 0 right)))
      (re-search-backward (rx ".."))
      (should (equal (cons 'line-num-beg (difftastic--chunk-file-at-point))
                     '(line-num-beg "foo" 99 0 right)))
      (goto-char (compat-call pos-eol)) ; Since Emacs-29
      (should (equal (cons 'eol (difftastic--chunk-file-at-point))
                     '(eol "foo" 99 10 right)))

      (re-search-forward (rx line-start " 9"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 9 0 left)))
      (goto-char (compat-call pos-bol)) ; Since Emacs-29
      (should (equal (cons 'bol (difftastic--chunk-file-at-point))
                     '(bol "foo" 9 0 left)))
      (re-search-forward (rx " 100"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 100 0 right)))
      (re-search-backward (rx "100"))
      (should (equal (cons 'line-num-beg (difftastic--chunk-file-at-point))
                     '(line-num-beg "foo" 100 0 right)))
      (goto-char (compat-call pos-eol)) ; Since Emacs-29
      (should (equal (cons 'eol (difftastic--chunk-file-at-point))
                     '(eol "foo" 100 10 right)))

      (re-search-forward (rx " 101"))
      (should (equal (cons 'line-num-end (difftastic--chunk-file-at-point))
                     '(line-num-end "foo" 101 0 right)))
      (re-search-backward (rx "101"))
      (should (equal (cons 'line-num-beg (difftastic--chunk-file-at-point))
                     '(line-num-beg "foo" 101 0 right)))
      (goto-char (compat-call pos-bol)) ; Since Emacs-29
      (should (equal (cons 'bol (difftastic--chunk-file-at-point))
                     '(bol "foo" 101 0 right)))
      (goto-char (compat-call pos-eol)) ; Since Emacs-29
      (should (equal (cons 'eol (difftastic--chunk-file-at-point))
                     '(eol "foo" 101 10 right))))))


(ert-deftest difftastic-diff-visit-file-setup:goto-line-col ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window))))
      (insert "bar")
      (eval
       `(mocklet (((get-buffer-window ,buffer 'visible) => ,win))
          (difftastic--diff-visit-file-setup ,buffer 1 0)
          (should (equal (point) 1))
          (difftastic--diff-visit-file-setup ,buffer 1 2)
          (should (equal (point) 3))
          (difftastic--diff-visit-file-setup ,buffer 2 0)
          (should (equal (point) 5))
          (difftastic--diff-visit-file-setup ,buffer 2 2)
          (should (equal (point) 7))
          (difftastic--diff-visit-file-setup ,buffer nil 0)
          (should (equal (point) 1))
          (difftastic--diff-visit-file-setup ,buffer nil 2)
          (should (equal (point) 3))
          (difftastic--diff-visit-file-setup ,buffer 1 7)
          (should (equal (point) 4))
          (difftastic--diff-visit-file-setup ,buffer 2 7)
          (should (equal (point) 8))
          (difftastic--diff-visit-file-setup ,buffer nil 7)
          (should (equal (point) 4)))))))

(ert-deftest difftastic-diff-visit-file-setup:start-smerge ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (buffer-file-name "test-file"))
      (insert "bar")
      (eval
       `(mocklet (((get-buffer-window ,buffer 'visible) => ,win)
                  ((magit-anything-unmerged-p "test-file") => t)
                  ((smerge-start-session)))
          (difftastic--diff-visit-file-setup ,buffer 1 0)
          (should (equal (point) 1)))))))

(ert-deftest difftastic-diff-visit-file-setup:run-hooks ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window))))
      (insert "bar")
      (eval
       `(mocklet (((get-buffer-window ,buffer 'visible) => ,win)
                  ((test-hook)))
          (let ((difftastic-diff-visit-file-hook '(test-hook)))
            (difftastic--diff-visit-file-setup ,buffer 1 0)
            (should (equal (point) 1))))))))

(ert-deftest difftastic-diff-visit-file-setup:widen ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window))))
      (insert "bar")
      (narrow-to-region 2 4)
      (eval
       `(mocklet (((get-buffer-window ,buffer 'visible) => ,win))
          (difftastic--diff-visit-file-setup ,buffer 2 0)
          (should (equal (point) 5))
          (should (equal (point-min) 1))
          (should (equal (point-max) 8)))))))

(ert-deftest difftastic-diff-visit-file-setup:no-window-error ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer)))
      (insert "bar")
      (eval
       `(mocklet (((get-buffer-window ,buffer 'visible)))
          (let ((data (cadr
                       (should-error
                        (difftastic--diff-visit-file-setup ,buffer 2 0)))))
            (should (equal "File buffer is not visible" data))))))))


(ert-deftest difftastic--diff-visit-file-or-buffer:left-buffer ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (pos (point))
           (difftastic--metadata `((file-buf-A . ("test-file-A" . ,buffer))
                                   (file-buf-B . ("test-file-B" . "test-buf-B")))))
      (insert "bar")
      (eval
       `(mocklet (((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-file-or-buffer
                          '("foo" 2 0 left) #'fn)))
          (should (equal (point) ,pos)))))))

(ert-deftest difftastic--diff-visit-file-or-buffer:right-buffer ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (pos (point))
           (difftastic--metadata `((file-buf-A . ("test-file-A" . "test-buf-A"))
                                   (file-buf-B . ("test-file-B" . ,buffer)))))
      (insert "bar")
      (eval
       `(mocklet (((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-file-or-buffer
                          '("foo" 2 0 right) #'fn)))
          (should (equal (point) ,pos)))))))

(ert-deftest difftastic--diff-visit-file-or-buffer:left-buffer-not-live ()
  (let (buffer
        (text-quoting-style 'straight))
    (ert-with-test-buffer ()
      (setq buffer (current-buffer)))
    (let ((difftastic--metadata `((file-buf-A . ("test-file-A" . ,buffer))
                                  (file-buf-B . ("test-file-B" . "test-buf-B")))))
      (eval
       `(mocklet ((fn not-called))
          (let ((data (cadr
                       (should-error (difftastic--diff-visit-file-or-buffer
                                      '("foo" 2 0 left) #'fn)))))
            (should (equal data
                           "Buffer A [#<killed buffer>] doesn't exist anymore"))))))))

(ert-deftest difftastic--diff-visit-file-or-buffer:right-buffer-not-live ()
  (let (buffer
        (text-quoting-style 'straight))
    (ert-with-test-buffer ()
      (setq buffer (current-buffer)))
    (let ((difftastic--metadata `((file-buf-A . ("test-file-A" . "test-buf-A"))
                                  (file-buf-B . ("test-file-B" . ,buffer)))))
      (eval
       `(mocklet ((fn not-called))
          (let ((data (cadr
                       (should-error (difftastic--diff-visit-file-or-buffer
                                      '("foo" 2 0 right) #'fn)))))
            (should (equal data
                           "Buffer B [#<killed buffer>] doesn't exist anymore"))))))))

(ert-deftest difftastic--diff-visit-file-or-buffer:left-visiting ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (pos (+ 2 (point)))
           (difftastic--metadata '((file-buf-A . ("test-file-A" . nil))
                                   (file-buf-B . ("test-file-B" . nil)))))
      (insert "bar")
      (eval
       `(mocklet (((get-file-buffer "test-file-A") => ,buffer)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-file-or-buffer
                          '("foo" 2 2 left) #'fn)))
          (should (equal (point) ,pos)))))))

(ert-deftest difftastic--diff-visit-file-or-buffer:right-visiting ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (pos (+ 2 (point)))
           (difftastic--metadata '((file-buf-A . ("test-file-A" . nil))
                                   (file-buf-B . ("test-file-B" . nil)))))
      (insert "bar")
      (eval
       `(mocklet (((get-file-buffer "test-file-B") => ,buffer)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-file-or-buffer
                          '("foo" 2 2 right) #'fn)))
          (should (equal (point) ,pos)))))))

(ert-deftest difftastic--diff-visit-file-or-buffer:left-not-visiting ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (pos (point))
           (difftastic--metadata '((file-buf-A . ("test-file-A" . nil))
                                   (file-buf-B . ("test-file-B" . nil)))))
      (insert "bar")
      (eval
       `(mocklet (((get-file-buffer "test-file-A"))
                  ((find-file-noselect "test-file-A") => ,buffer)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-file-or-buffer
                          '("foo" 2 0 left) #'fn)))
          (should (equal (point) ,pos)))))))

(ert-deftest difftastic--diff-visit-file-or-buffer:right-not-visiting ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (pos (point))
           (difftastic--metadata '((file-buf-A . ("test-file-A" . nil))
                                   (file-buf-B . ("test-file-B" . nil)))))
      (insert "bar")
      (eval
       `(mocklet (((get-file-buffer "test-file-B"))
                  ((find-file-noselect "test-file-B") => ,buffer)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-file-or-buffer
                          '("foo" 2 0 right) #'fn)))
          (should (equal (point) ,pos)))))))


(ert-deftest difftastic--diff-visit-git-file:left-revision ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (pos (point))
           (difftastic--metadata '((rev-or-range . "test-rev"))))
      (insert "bar")
      (eval
       `(mocklet (((magit-find-file-noselect "test-rev^" "test-file") => ,buffer)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 0 left) #'fn)))
          (should (equal (point) ,pos)))))))

(ert-deftest difftastic--diff-visit-git-file:right-revision ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (pos (point))
           (difftastic--metadata '((rev-or-range . "test-rev"))))
      (insert "bar")
      (eval
       `(mocklet (((magit-find-file-noselect "test-rev" "test-file") => ,buffer)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 0 right) #'fn)))
          (should (equal (point) ,pos)))))))

(ert-deftest difftastic--diff-visit-git-file:left-range ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (pos (+ 2 (point)))
           (difftastic--metadata '((rev-or-range . "test-range-from..test-range-to"))))
      (insert "bar")
      (eval
       `(mocklet (((magit-find-file-noselect "test-range-from" "test-file") => ,buffer)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 2 left) #'fn)))
          (should (equal (point) ,pos)))))))

(ert-deftest difftastic--diff-visit-git-file:right-range ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (pos (+ 2 (point)))
           (difftastic--metadata '((rev-or-range . "test-range-from..test-range-to"))))
      (insert "bar")
      (eval
       `(mocklet (((magit-find-file-noselect "test-range-to" "test-file") => ,buffer)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 2 right) #'fn)))
          (should (equal (point) ,pos)))))))

(ert-deftest difftastic--diff-visit-git-file:left-staged ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (difftastic--metadata '((rev-or-range . staged))))
      (insert "bar")
      (eval
       `(mocklet (((magit-find-file-noselect "HEAD" "test-file") => ,buffer)
                  ((magit-diff-visit--offset "test-file" nil 2) => 3)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 0 left) #'fn)))
          (should (equal (point) (point-max))))))))

(ert-deftest difftastic--diff-visit-git-file:right-staged-visiting ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (difftastic--metadata '((rev-or-range . staged))))
      (insert "bar")
      (eval
       `(mocklet (((get-file-buffer "test-file") => ,buffer)
                  ((fn ,buffer))
                  ((magit-diff-visit--offset "test-file" nil 2) => 3)
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 0 right) #'fn)))
          (should (equal (point) (point-max))))))))

(ert-deftest difftastic--diff-visit-git-file:right-staged-not-visiting ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (difftastic--metadata '((rev-or-range . staged))))
      (insert "bar")
      (eval
       `(mocklet (((get-file-buffer "test-file"))
                  ((find-file-noselect "test-file") => ,buffer)
                  ((fn ,buffer))
                  ((magit-diff-visit--offset "test-file" nil 2) => 3)
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 0 right) #'fn)))
          (should (equal (point) (point-max))))))))

(ert-deftest difftastic--diff-visit-git-file:left-staged-avoid-head-blob-visiting ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (difftastic--metadata '((rev-or-range . staged)))
           (difftastic-diff-visit-avoid-head-blob t))
      (insert "bar")
      (eval
       `(mocklet (((get-file-buffer "test-file") => ,buffer)
                  ((magit-diff-visit--offset "test-file" nil 2) => 3)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 0 left) #'fn)))
          (should (equal (point) (point-max))))))))

(ert-deftest difftastic--diff-visit-git-file:right-staged-avoid-head-blob-visiting ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (difftastic--metadata '((rev-or-range . staged)))
           (difftastic-diff-visit-avoid-head-blob t))
      (insert "bar")
      (eval
       `(mocklet (((get-file-buffer "test-file") => ,buffer)
                  ((magit-diff-visit--offset "test-file" nil 2) => 3)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 0 right) #'fn)))
          (should (equal (point) (point-max))))))))

(ert-deftest difftastic--diff-visit-git-file:left-staged-avoid-head-blob-not-visiting ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (difftastic--metadata '((rev-or-range . staged)))
           (difftastic-diff-visit-avoid-head-blob t))
      (insert "bar")
      (eval
       `(mocklet (((get-file-buffer "test-file"))
                  ((find-file-noselect "test-file") => ,buffer)
                  ((magit-diff-visit--offset "test-file" nil 2) => 3)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 0 left) #'fn)))
          (should (equal (point) (point-max))))))))

(ert-deftest difftastic--diff-visit-git-file:right-staged-avoid-head-blob-not-visiting ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (difftastic--metadata '((rev-or-range . staged)))
           (difftastic-diff-visit-avoid-head-blob t))
      (insert "bar")
      (eval
       `(mocklet (((get-file-buffer "test-file"))
                  ((find-file-noselect "test-file") => ,buffer)
                  ((magit-diff-visit--offset "test-file" nil 2) => 3)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 0 right) #'fn)))
          (should (equal (point) (point-max))))))))

(ert-deftest difftastic--diff-visit-git-file:left-unstaged ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (pos (point))
           (difftastic--metadata '((rev-or-range . unstaged))))
      (insert "bar")
      (eval
       `(mocklet (((magit-find-file-noselect "HEAD" "test-file") => ,buffer)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 0 left) #'fn)))
          (should (equal (point) ,pos)))))))

(ert-deftest difftastic--diff-visit-git-file:right-unstaged-visiting ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (pos (point))
           (difftastic--metadata '((rev-or-range . unstaged))))
      (insert "bar")
      (eval
       `(mocklet (((get-file-buffer "test-file") => ,buffer)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 0 right) #'fn)))
          (should (equal (point) ,pos)))))))

(ert-deftest difftastic--diff-visit-git-file:right-unstaged-not-visiting ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (pos (point))
           (difftastic--metadata '((rev-or-range . unstaged))))
      (insert "bar")
      (eval
       `(mocklet (((get-file-buffer "test-file"))
                  ((find-file-noselect "test-file") => ,buffer)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 0 right) #'fn)))
          (should (equal (point) ,pos)))))))

(ert-deftest difftastic--diff-visit-git-file:left-unstaged-avoid-head-blob-visiting ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (pos (point))
           (difftastic--metadata '((rev-or-range . unstaged)))
           (difftastic-diff-visit-avoid-head-blob t))
      (insert "bar")
      (eval
       `(mocklet (((get-file-buffer "test-file") => ,buffer)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 0 left) #'fn)))
          (should (equal (point) ,pos)))))))

(ert-deftest difftastic--diff-visit-git-file:right-unstaged-avoid-head-blob-visiting ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (pos (point))
           (difftastic--metadata '((rev-or-range . unstaged)))
           (difftastic-diff-visit-avoid-head-blob t))
      (insert "bar")
      (eval
       `(mocklet (((get-file-buffer "test-file") => ,buffer)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 0 right) #'fn)))
          (should (equal (point) ,pos)))))))

(ert-deftest difftastic--diff-visit-git-file:left-unstaged-avoid-head-blob-not-visiting ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (pos (point))
           (difftastic--metadata '((rev-or-range . unstaged)))
           (difftastic-diff-visit-avoid-head-blob t))
      (insert "bar")
      (eval
       `(mocklet (((get-file-buffer "test-file"))
                  ((find-file-noselect "test-file") => ,buffer)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 0 left) #'fn)))
          (should (equal (point) ,pos)))))))

(ert-deftest difftastic--diff-visit-git-file:right-unstaged-avoid-head-blob-not-visiting ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (pos (point))
           (difftastic--metadata '((rev-or-range . unstaged)))
           (difftastic-diff-visit-avoid-head-blob t))
      (insert "bar")
      (eval
       `(mocklet (((get-file-buffer "test-file"))
                  ((find-file-noselect "test-file") => ,buffer)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 0 right) #'fn)))
          (should (equal (point) ,pos)))))))

(ert-deftest difftastic--diff-visit-git-file:left-revision-force-worktree-visiting ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (difftastic--metadata '((rev-or-range . "test-rev"))))
      (insert "bar")
      (eval
       `(mocklet (((get-file-buffer "test-file") => ,buffer)
                  ((magit-diff-visit--offset "test-file" "test-rev^" 2) => 3)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 0 left) #'fn t)))
          (should (equal (point) (point-max))))))))

(ert-deftest difftastic--diff-visit-git-file:right-revision-force-worktree-visiting ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (difftastic--metadata '((rev-or-range . "test-rev"))))
      (insert "bar")
      (eval
       `(mocklet (((get-file-buffer "test-file") => ,buffer)
                  ((magit-diff-visit--offset "test-file" "test-rev" 2) => 3)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 0 right) #'fn t)))
          (should (equal (point) (point-max))))))))

(ert-deftest difftastic--diff-visit-git-file:left-revision-force-worktree-not-visiting ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (difftastic--metadata '((rev-or-range . "test-rev"))))
      (insert "bar")
      (eval
       `(mocklet (((get-file-buffer "test-file"))
                  ((find-file-noselect "test-file") => ,buffer)
                  ((magit-diff-visit--offset "test-file" "test-rev^" 2) => 3)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 0 left) #'fn t)))
          (should (equal (point) (point-max))))))))

(ert-deftest difftastic--diff-visit-git-file:right-revision-force-worktree-not-visiting ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (difftastic--metadata '((rev-or-range . "test-rev"))))
      (insert "bar")
      (eval
       `(mocklet (((get-file-buffer "test-file"))
                  ((find-file-noselect "test-file") => ,buffer)
                  ((magit-diff-visit--offset "test-file" "test-rev" 2) => 3)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 0 right) #'fn t)))
          (should (equal (point) (point-max))))))))

(ert-deftest difftastic--diff-visit-git-file:left-range-force-worktree-visiting ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (difftastic--metadata '((rev-or-range . "test-range-from..test-range-to"))))
      (insert "bar")
      (eval
       `(mocklet (((get-file-buffer "test-file") => ,buffer)
                  ((magit-diff-visit--offset "test-file" "test-range-from" 2) => 3)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 0 left) #'fn t)))
          (should (equal (point) (point-max))))))))

(ert-deftest difftastic--diff-visit-git-file:right-range-force-worktree-visiting ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (difftastic--metadata '((rev-or-range . "test-range-from..test-range-to"))))
      (insert "bar")
      (eval
       `(mocklet (((get-file-buffer "test-file") => ,buffer)
                  ((magit-diff-visit--offset "test-file" "test-range-to" 2) => 3)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 0 right) #'fn t)))
          (should (equal (point) (point-max))))))))

(ert-deftest difftastic--diff-visit-git-file:left-range-force-worktree-not-visiting ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (difftastic--metadata '((rev-or-range . "test-range-from..test-range-to"))))
      (insert "bar")
      (eval
       `(mocklet (((get-file-buffer "test-file"))
                  ((find-file-noselect "test-file") => ,buffer)
                  ((magit-diff-visit--offset "test-file" "test-range-from" 2) => 3)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 0 left) #'fn t)))
          (should (equal (point) (point-max))))))))

(ert-deftest difftastic--diff-visit-git-file:right-range-force-worktree-not-visiting ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (difftastic--metadata '((rev-or-range . "test-range-from..test-range-to"))))
      (insert "bar")
      (eval
       `(mocklet (((get-file-buffer "test-file"))
                  ((find-file-noselect "test-file") => ,buffer)
                  ((magit-diff-visit--offset "test-file" "test-range-to" 2) => 3)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 0 right) #'fn t)))
          (should (equal (point) (point-max))))))))

(ert-deftest difftastic--diff-visit-git-file:left-staged-force-worktree-visiting ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (difftastic--metadata '((rev-or-range . staged))))
      (insert "bar")
      (eval
       `(mocklet (((get-file-buffer "test-file") => ,buffer)
                  ((magit-diff-visit--offset "test-file" nil 2) => 3)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 0 left) #'fn t)))
          (should (equal (point) (point-max))))))))

(ert-deftest difftastic--diff-visit-git-file:right-staged-force-worktree-visiting ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (difftastic--metadata '((rev-or-range . staged))))
      (insert "bar")
      (eval
       `(mocklet (((get-file-buffer "test-file") => ,buffer)
                  ((magit-diff-visit--offset "test-file" nil 2) => 3)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 0 right) #'fn t)))
          (should (equal (point) (point-max))))))))

(ert-deftest difftastic--diff-visit-git-file:left-staged-force-worktree-not-visiting ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (difftastic--metadata '((rev-or-range . staged))))
      (insert "bar")
      (eval
       `(mocklet (((get-file-buffer "test-file"))
                  ((find-file-noselect "test-file") => ,buffer)
                  ((magit-diff-visit--offset "test-file" nil 2) => 3)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 0 left) #'fn t)))
          (should (equal (point) (point-max))))))))

(ert-deftest difftastic--diff-visit-git-file:right-staged-force-worktree-not-visiting ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (difftastic--metadata '((rev-or-range . staged))))
      (insert "bar")
      (eval
       `(mocklet (((get-file-buffer "test-file"))
                  ((find-file-noselect "test-file") => ,buffer)
                  ((magit-diff-visit--offset "test-file" nil 2) => 3)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 0 right) #'fn t)))
          (should (equal (point) (point-max))))))))

(ert-deftest difftastic--diff-visit-git-file:left-unstaged-froce-worktree-visiting ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (pos (point))
           (difftastic--metadata '((rev-or-range . unstaged))))
      (insert "bar")
      (eval
       `(mocklet (((get-file-buffer "test-file") => ,buffer)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 0 left) #'fn t)))
          (should (equal (point) ,pos)))))))

(ert-deftest difftastic--diff-visit-git-file:right-unstaged-froce-worktree-visiting ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (pos (point))
           (difftastic--metadata '((rev-or-range . unstaged))))
      (insert "bar")
      (eval
       `(mocklet (((get-file-buffer "test-file") => ,buffer)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 0 right) #'fn t)))
          (should (equal (point) ,pos)))))))

(ert-deftest difftastic--diff-visit-git-file:left-unstaged-froce-worktree-notvisiting ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (pos (point))
           (difftastic--metadata '((rev-or-range . unstaged))))
      (insert "bar")
      (eval
       `(mocklet (((get-file-buffer "test-file"))
                  ((find-file-noselect "test-file") => ,buffer)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 0 left) #'fn t)))
          (should (equal (point) ,pos)))))))

(ert-deftest difftastic--diff-visit-git-file:right-unstaged-froce-worktree-notvisiting ()
  (ert-with-test-buffer ()
    (insert "foo\n")
    (let* ((buffer (current-buffer))
           (win (progn
                  (switch-to-buffer buffer)
                  (selected-window)))
           (pos (point))
           (difftastic--metadata '((rev-or-range . unstaged))))
      (insert "bar")
      (eval
       `(mocklet (((get-file-buffer "test-file"))
                  ((find-file-noselect "test-file") => ,buffer)
                  ((fn ,buffer))
                  ((get-buffer-window ,buffer 'visible) => ,win))
          (should (equal ,buffer
                         (difftastic--diff-visit-git-file
                          '("test-file" 2 0 right) #'fn t)))
          (should (equal (point) ,pos)))))))


(ert-deftest difftastic--diff-visit-file:git-file ()
  (mocklet (((difftastic--diff-visit-git-file "test-chunk-file" #'ignore t))
            (difftastic--diff-visit-file-or-buffer not-called))
    (let ((difftastic--metadata '((git-command "test-git-command"))))
      (difftastic--diff-visit-file "test-chunk-file" #'ignore t))))

(ert-deftest difftastic--diff-visit-file:file-or-buffer ()
  (mocklet ((difftastic--diff-visit-git-file not-called)
            ((difftastic--diff-visit-file-or-buffer "test-chunk-file" #'ignore)))
    (let (difftastic--metadata)
      (difftastic--diff-visit-file "test-chunk-file" #'ignore))))

(ert-deftest difftastic--diff-visit-file:no-file ()
  (mocklet ((difftastic--diff-visit-git-file not-called)
            (difftastic--diff-visit-file-or-buffer not-called))
    (let ((data (cadr
                 (should-error
                  (difftastic--diff-visit-file nil #'ignore t)))))
      (should (equal data "No chunk file at point")))))


(ert-deftest difftastic-diff-visit-file:basic ()
  (mocklet (((difftastic--chunk-file-at-point) => "test-chunk-file")
            ((difftastic--diff-visit-file
              "test-chunk-file" #'pop-to-buffer-same-window)))
    (call-interactively #'difftastic-diff-visit-file)))

(ert-deftest difftastic-diff-visit-file:prefix-arg ()
  (mocklet (((difftastic--chunk-file-at-point) => "test-chunk-file")
            ((difftastic--diff-visit-file
              "test-chunk-file" #'switch-to-buffer-other-window)))
    (let ((current-prefix-arg '(4)))
      (call-interactively #'difftastic-diff-visit-file))))


(ert-deftest difftastic-diff-visit-file-other-window:basic ()
  (mocklet (((difftastic--chunk-file-at-point) => "test-chunk-file")
            ((difftastic--diff-visit-file
              "test-chunk-file" #'switch-to-buffer-other-window)))
    (call-interactively #'difftastic-diff-visit-file-other-window)))


(ert-deftest difftastic-diff-visit-file-other-frame:basic ()
  (mocklet (((difftastic--chunk-file-at-point) => "test-chunk-file")
            ((difftastic--diff-visit-file
              "test-chunk-file" #'switch-to-buffer-other-frame)))
    (call-interactively #'difftastic-diff-visit-file-other-frame)))


(ert-deftest difftastic-diff-visit-worktree-file:basic ()
  (mocklet (((difftastic--chunk-file-at-point) => "test-chunk-file")
            ((difftastic--diff-visit-file
              "test-chunk-file" #'pop-to-buffer-same-window t)))
    (call-interactively #'difftastic-diff-visit-worktree-file)))

(ert-deftest difftastic-diff-visit-worktree-file:prefix-arg ()
  (mocklet (((difftastic--chunk-file-at-point) => "test-chunk-file")
            ((difftastic--diff-visit-file
              "test-chunk-file" #'switch-to-buffer-other-window t)))
    (let ((current-prefix-arg '(4)))
      (call-interactively #'difftastic-diff-visit-worktree-file))))


(ert-deftest difftastic-diff-visit-worktree-file-other-window:basic ()
  (mocklet (((difftastic--chunk-file-at-point) => "test-chunk-file")
            ((difftastic--diff-visit-file
              "test-chunk-file" #'switch-to-buffer-other-window t)))
    (call-interactively #'difftastic-diff-visit-worktree-file-other-window)))


(ert-deftest difftastic-diff-visit-worktree-file-other-frame:basic ()
  (mocklet (((difftastic--chunk-file-at-point) => "test-chunk-file")
            ((difftastic--diff-visit-file
              "test-chunk-file" #'switch-to-buffer-other-frame t)))
    (call-interactively #'difftastic-diff-visit-worktree-file-other-frame)))


;; When running in noninteractive batch mode there are no faces defined, ergo:
;; no colors.  See this discussion:
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2024-02/msg00095.html For
;; this reason only a couple sanity tests are run in CI.

(ert-deftest difftastic--ansi-color-apply:file-ansi-colors-applied ()
  (let* (difftastic--ansi-color-add-background-cache
         (expected-fg
          (if noninteractive
              "unspecified-fg"
            (face-foreground (aref difftastic-normal-colors-vector 3) nil t))))
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[1m[33mdifftastic.el[39m[0m[2m --- 1/2 --- Emacs Lisp[0m")
          (concat
           (propertize
            "difftastic.el"
            'font-lock-face `(ansi-color-bold (:foreground ,expected-fg)))
           (propertize
            " --- 1/2 --- Emacs Lisp"
            'font-lock-face 'ansi-color-faint))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[1m[33mdifftastic.el[39m[0m[2m --- 1/2 --- Emacs Lisp[0m")
        (concat
         (propertize
          "difftastic.el"
          'font-lock-face `(ansi-color-bold (:foreground ,expected-fg)))
         (propertize
          " --- 1/2 --- Emacs Lisp"
          'font-lock-face 'ansi-color-faint)))))))

(ert-deftest difftastic--ansi-color-apply:chunk-ansi-colors-applied ()
  (let (difftastic--ansi-color-add-background-cache)
    (should
     (equal-including-properties
      (difftastic--ansi-color-apply
       "[1mdifftastic.el[0m[2m --- 2/2 --- Emacs Lisp[0m")
      (concat
       (propertize
        "difftastic.el"
        'font-lock-face 'ansi-color-bold)
       (propertize
        " --- 2/2 --- Emacs Lisp"
        'font-lock-face 'ansi-color-faint))))))

;; The following few tests are meant to check for default face color values
;; such that it's easier to debug and update tests when these change upstream.

(defun difftastic-t--face-attribute-or (face attribute default)
  "Get FACE ATTRIBUTE or set it to DEFAULT if it is `unspecified'.
This only happens when `noninteractive' to avoid messing up with faces."
  (let ((value (face-attribute face attribute nil t)))
    (if (not (eq value 'unspecified))
        value
      (when noninteractive
        (set-face-attribute face nil attribute default)
        default))))

(defconst difftastic-t-removed-fg
  (difftastic-t--face-attribute-or 'magit-diff-removed :foreground "removed-fg"))
(defconst difftastic-t-removed-bg
  (difftastic-t--face-attribute-or 'magit-diff-removed :background "removed-bg"))
(defconst difftastic-t-removed-highlight-fg
  (difftastic-t--face-attribute-or
   (alist-get 'magit-diff-removed difftastic-highlight-alist)
   :foreground "removed-highlight-fg"))
(defconst difftastic-t-removed-highlight-bg
  (difftastic-t--face-attribute-or
   (alist-get 'magit-diff-removed difftastic-highlight-alist)
   :background "removed-highlight-bg"))
(defconst difftastic-t-added-fg
  (difftastic-t--face-attribute-or 'magit-diff-added :foreground "added-fg"))
(defconst difftastic-t-added-bg
  (difftastic-t--face-attribute-or 'magit-diff-added :background "added-bg"))
(defconst difftastic-t-added-highlight-fg
  (difftastic-t--face-attribute-or
   (alist-get 'magit-diff-added difftastic-highlight-alist)
   :foreground "added-highlight-fg"))
(defconst difftastic-t-added-highlight-bg
  (difftastic-t--face-attribute-or
   (alist-get 'magit-diff-added difftastic-highlight-alist)
   :background "added-highlight-bg"))
(defconst difftastic-t-comment-fg
  (difftastic-t--face-attribute-or 'font-lock-comment-face :foreground "comment-fg"))
(defconst difftastic-t-comment-bg
  (difftastic-t--face-attribute-or 'font-lock-comment-face :background "comment-bg"))
(defconst difftastic-t-string-fg
  (difftastic-t--face-attribute-or 'font-lock-string-face :foreground "string-fg"))
(defconst difftastic-t-string-bg
  (difftastic-t--face-attribute-or 'font-lock-string-face :background "string-bg"))

(ert-deftest difftastic--ansi-color-apply:removed-ansi-colors-applied ()
  (let (difftastic--ansi-color-add-background-cache)
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply "[31mremoved[0m")
          (propertize "removed"
                      'font-lock-face
                      `((:background ,difftastic-t-removed-bg)
                        (:foreground ,difftastic-t-removed-fg)))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply "[31mremoved[0m")
        (propertize "removed"
                    'font-lock-face
                    `((:background ,difftastic-t-removed-bg)
                      (:foreground ,difftastic-t-removed-fg))))))))

(ert-deftest difftastic--ansi-color-apply:removed-bold-ansi-colors-applied ()
  (let (difftastic--ansi-color-add-background-cache)
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[31;1mremoved[0m")
          (propertize "removed"
                      'font-lock-face
                      `(ansi-color-bold
                        ((:background ,difftastic-t-removed-bg)
                         (:foreground ,difftastic-t-removed-fg))))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[31;1mremoved[0m")
        (propertize "removed"
                    'font-lock-face
                    `((:background ,difftastic-t-removed-bg)
                      ansi-color-bold
                      (:foreground ,difftastic-t-removed-fg))))))))

(ert-deftest difftastic--ansi-color-apply:removed-italic-ansi-colors-applied ()
  (let (difftastic--ansi-color-add-background-cache)
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[31;3mremoved[0m")
          (propertize "removed"
                      'font-lock-face
                      `(ansi-color-italic
                        ((:background ,difftastic-t-removed-bg)
                         (:foreground ,difftastic-t-removed-fg))))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[31;3mremoved[0m")
        (propertize "removed"
                    'font-lock-face
                    `((:background ,difftastic-t-removed-bg)
                      ansi-color-italic
                      (:foreground ,difftastic-t-removed-fg))))))))

(ert-deftest difftastic--ansi-color-apply:removed-bold-italic-ansi-colors-applied ()
  (let (difftastic--ansi-color-add-background-cache)
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[31;1;3mremoved[0m")
          (propertize "removed"
                      'font-lock-face
                      `(ansi-color-bold
                        ansi-color-italic
                        ((:background ,difftastic-t-removed-bg)
                         (:foreground ,difftastic-t-removed-fg))))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[31;1;3mremoved[0m")
        (propertize "removed"
                    'font-lock-face
                    `((:background ,difftastic-t-removed-bg)
                      ansi-color-bold
                      ansi-color-italic
                      (:foreground ,difftastic-t-removed-fg))))))))

(ert-deftest difftastic--ansi-color-apply:added-ansi-colors-applied ()
  (let (difftastic--ansi-color-add-background-cache)
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[32madded[0m")
          (propertize "added"
                      'font-lock-face
                      `((:background ,difftastic-t-added-bg)
                        (:foreground ,difftastic-t-added-fg)))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[32madded[0m")
        (propertize "added"
                    'font-lock-face
                    `((:background ,difftastic-t-added-bg)
                      (:foreground ,difftastic-t-added-fg))))))))

(ert-deftest difftastic--ansi-color-apply:added-bold-ansi-colors-applied ()
  (let (difftastic--ansi-color-add-background-cache)
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[32;1madded[0m")
          (propertize "added"
                      'font-lock-face
                      `(ansi-color-bold
                        ((:background ,difftastic-t-added-bg)
                         (:foreground ,difftastic-t-added-fg))))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[32;1madded[0m")
        (propertize "added"
                    'font-lock-face
                    `((:background ,difftastic-t-added-bg)
                      ansi-color-bold
                      (:foreground ,difftastic-t-added-fg))))))))

(ert-deftest difftastic--ansi-color-apply:added-italic-ansi-colors-applied ()
  (let (difftastic--ansi-color-add-background-cache)
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[32;3madded[0m")
          (propertize "added"
                      'font-lock-face
                      `(ansi-color-italic
                        ((:background ,difftastic-t-added-bg)
                         (:foreground ,difftastic-t-added-fg))))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[32;3madded[0m")
        (propertize "added"
                    'font-lock-face
                    `((:background ,difftastic-t-added-bg)
                      ansi-color-italic
                      (:foreground ,difftastic-t-added-fg))))))))

(ert-deftest difftastic--ansi-color-apply:added-bold-italic-ansi-colors-applied ()
  (let (difftastic--ansi-color-add-background-cache)
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[32;1;3madded[0m")
          (propertize "added"
                      'font-lock-face
                      `(ansi-color-bold
                        ansi-color-italic
                        ((:background ,difftastic-t-added-bg)
                         (:foreground ,difftastic-t-added-fg))))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[32;1;3madded[0m")
        (propertize "added"
                    'font-lock-face
                    `((:background ,difftastic-t-added-bg)
                      ansi-color-bold
                      ansi-color-italic
                      (:foreground ,difftastic-t-added-fg))))))))

(ert-deftest difftastic--ansi-color-apply:removed-highlight-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     :failed)
  (let (difftastic--ansi-color-add-background-cache)
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[31;4mremoved[0m")
          (propertize "removed"
                      'font-lock-face
                      `((:background ,difftastic-t-removed-highlight-bg)
                        (:foreground ,difftastic-t-removed-highlight-fg)))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[31;4mremoved[0m")
        (propertize "removed"
                    'font-lock-face
                    `((:background ,difftastic-t-removed-highlight-bg)
                      (:foreground ,difftastic-t-removed-highlight-fg))))))))

(ert-deftest difftastic--ansi-color-apply:removed-highlight-bold-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     :failed)
  (let (difftastic--ansi-color-add-background-cache)
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[31;1;4mremoved[0m")
          (propertize "removed"
                      'font-lock-face
                      `((:background ,difftastic-t-removed-highlight-bg)
                        (:foreground ,difftastic-t-removed-highlight-fg)))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[31;1;4mremoved[0m")
        (propertize "removed"
                    'font-lock-face
                    `((:background ,difftastic-t-removed-highlight-bg)
                      (:foreground ,difftastic-t-removed-highlight-fg))))))))

(ert-deftest difftastic--ansi-color-apply:removed-highlight-italic-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     :failed)
  (let (difftastic--ansi-color-add-background-cache)
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[31;3;4mremoved[0m")
          (propertize "removed"
                      'font-lock-face
                      `(ansi-color-italic
                        ((:background ,difftastic-t-removed-highlight-bg)
                         (:foreground ,difftastic-t-removed-highlight-fg))))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[31;3;4mremoved[0m")
        (propertize "removed"
                    'font-lock-face
                    `((:background ,difftastic-t-removed-highlight-bg)
                      (:foreground ,difftastic-t-removed-highlight-fg)
                      ansi-color-italic)))))))

(ert-deftest difftastic--ansi-color-apply:removed-highlight-bold-italic-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     :failed)
  (let (difftastic--ansi-color-add-background-cache)
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[31;1;3;4mremoved[0m")
          (propertize "removed"
                      'font-lock-face
                      `(ansi-color-italic
                        ((:background ,difftastic-t-removed-highlight-bg)
                         (:foreground ,difftastic-t-removed-highlight-fg))))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[31;1;3;4mremoved[0m")
        (propertize "removed"
                    'font-lock-face
                    `((:background ,difftastic-t-removed-highlight-bg)
                      (:foreground ,difftastic-t-removed-highlight-fg)
                      ansi-color-italic)))))))

(ert-deftest difftastic--ansi-color-apply:removed-highlight-no-strip-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     :failed)
  (let (difftastic--ansi-color-add-background-cache
        difftastic-highlight-strip-face-properties)
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[31;1;2;3;4mremoved[0m")
          (propertize "removed"
                      'font-lock-face
                      `(ansi-color-bold
                        ansi-color-faint
                        ansi-color-italic
                        ansi-color-underline
                        ((:background ,difftastic-t-removed-highlight-bg)
                         (:foreground ,difftastic-t-removed-highlight-fg))))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[31;1;2;3;4mremoved[0m")
        (propertize "removed"
                    'font-lock-face
                    `((:background ,difftastic-t-removed-highlight-bg)
                      (:foreground ,difftastic-t-removed-highlight-fg)
                      ansi-color-bold
                      ansi-color-faint
                      ansi-color-italic
                      ansi-color-underline)))))))

(ert-deftest difftastic--ansi-color-apply:removed-highlight-strip-all-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     :failed)
  (let (difftastic--ansi-color-add-background-cache
        (difftastic-highlight-strip-face-properties '(:bold :faint :italic :underline)))
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[31;1;2;3;4mremoved[0m")
          (propertize "removed"
                      'font-lock-face
                      `((:background ,difftastic-t-removed-highlight-bg)
                        (:foreground ,difftastic-t-removed-highlight-fg)))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[31;1;2;3;4mremoved[0m")
        (propertize "removed"
                    'font-lock-face
                    `((:background ,difftastic-t-removed-highlight-bg)
                      (:foreground ,difftastic-t-removed-highlight-fg))))))))

(ert-deftest difftastic--ansi-color-apply:added-highlight-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     :failed)
  (let (difftastic--ansi-color-add-background-cache)
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[32;4madded[0m")
          (propertize "added"
                      'font-lock-face
                      `((:background ,difftastic-t-added-highlight-bg)
                        (:foreground ,difftastic-t-added-highlight-fg)))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[32;4madded[0m")
        (propertize "added"
                    'font-lock-face
                    `((:background ,difftastic-t-added-highlight-bg)
                      (:foreground ,difftastic-t-added-highlight-fg))))))))

(ert-deftest difftastic--ansi-color-apply:added-highlight-bold-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     :failed)
  (let (difftastic--ansi-color-add-background-cache)
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[32;1;4madded[0m")
          (propertize "added"
                      'font-lock-face
                      `((:background ,difftastic-t-added-highlight-bg)
                        (:foreground ,difftastic-t-added-highlight-fg)))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[32;1;4madded[0m")
        (propertize "added"
                    'font-lock-face
                    `((:background ,difftastic-t-added-highlight-bg)
                      (:foreground ,difftastic-t-added-highlight-fg))))))))

(ert-deftest difftastic--ansi-color-apply:added-highlight-italic-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     :failed)
  (let (difftastic--ansi-color-add-background-cache)
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[32;3;4madded[0m")
          (propertize "added"
                      'font-lock-face
                      `(ansi-color-italic
                        ((:background ,difftastic-t-added-highlight-bg)
                         (:foreground ,difftastic-t-added-highlight-fg))))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[32;3;4madded[0m")
        (propertize "added"
                    'font-lock-face
                    `((:background ,difftastic-t-added-highlight-bg)
                      (:foreground ,difftastic-t-added-highlight-fg)
                      ansi-color-italic)))))))

(ert-deftest difftastic--ansi-color-apply:added-highlight-bold-italic-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     :failed)
  (let (difftastic--ansi-color-add-background-cache)
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[32;1;3;4madded[0m")
          (propertize "added"
                      'font-lock-face
                      `(ansi-color-italic
                        ((:background ,difftastic-t-added-highlight-bg)
                         (:foreground ,difftastic-t-added-highlight-fg))))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[32;1;3;4madded[0m")
        (propertize "added"
                    'font-lock-face
                    `((:background ,difftastic-t-added-highlight-bg)
                      (:foreground ,difftastic-t-added-highlight-fg)
                      ansi-color-italic)))))))

(ert-deftest difftastic--ansi-color-apply:added-highlight-no-strip-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     :failed)
  (let (difftastic--ansi-color-add-background-cache
        difftastic-highlight-strip-face-properties)
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[32;1;2;3;4madded[0m")
          (propertize "added"
                      'font-lock-face
                      `(ansi-color-bold
                        ansi-color-faint
                        ansi-color-italic
                        ansi-color-underline
                        ((:background ,difftastic-t-added-highlight-bg)
                         (:foreground ,difftastic-t-added-highlight-fg))))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[32;1;2;3;4madded[0m")
        (propertize "added"
                    'font-lock-face
                    `((:background ,difftastic-t-added-highlight-bg)
                      (:foreground ,difftastic-t-added-highlight-fg)
                      ansi-color-bold
                      ansi-color-faint
                      ansi-color-italic
                      ansi-color-underline)))))))

(ert-deftest difftastic--ansi-color-apply:added-highlight-strip-all-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     :failed)
  (let (difftastic--ansi-color-add-background-cache
        (difftastic-highlight-strip-face-properties '(:bold :faint :italic :underline)))
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[32;1;2;3;4madded[0m")
          (propertize "added"
                      'font-lock-face
                      `((:background ,difftastic-t-added-highlight-bg)
                        (:foreground ,difftastic-t-added-highlight-fg)))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[32;1;2;3;4madded[0m")
        (propertize "added"
                    'font-lock-face
                    `((:background ,difftastic-t-added-highlight-bg)
                      (:foreground ,difftastic-t-added-highlight-fg))))))))

(ert-deftest difftastic--ansi-color-apply:removed-no-highlight-ansi-colors-applied ()
  (let (difftastic--ansi-color-add-background-cache
        difftastic-highlight-alist)
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[31;4mremoved[0m")
          (propertize "removed"
                      'font-lock-face
                      `(ansi-color-underline
                        ((:background ,difftastic-t-removed-bg)
                         (:foreground ,difftastic-t-removed-fg))))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[31;4mremoved[0m")
        (propertize "removed"
                    'font-lock-face
                    `((:background ,difftastic-t-removed-bg)
                      ansi-color-underline
                      (:foreground ,difftastic-t-removed-fg))))))))

(ert-deftest difftastic--ansi-color-apply:removed-no-highlight-bold-ansi-colors-applied ()
  (let (difftastic--ansi-color-add-background-cache
        difftastic-highlight-alist)
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[31;1;4mremoved[0m")
          (propertize "removed"
                      'font-lock-face
                      `(ansi-color-bold
                        ansi-color-underline
                        ((:background ,difftastic-t-removed-bg)
                         (:foreground ,difftastic-t-removed-fg))))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[31;1;4mremoved[0m")
        (propertize "removed"
                    'font-lock-face
                    `((:background ,difftastic-t-removed-bg)
                      ansi-color-bold
                      ansi-color-underline
                      (:foreground ,difftastic-t-removed-fg))))))))

(ert-deftest difftastic--ansi-color-apply:removed-no-highlight-italic-ansi-colors-applied ()
  (let (difftastic--ansi-color-add-background-cache
        difftastic-highlight-alist)
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[31;3;4mremoved[0m")
          (propertize "removed"
                      'font-lock-face
                      `(ansi-color-italic
                        ansi-color-underline
                        ((:background ,difftastic-t-removed-bg)
                         (:foreground ,difftastic-t-removed-fg))))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[31;3;4mremoved[0m")
        (propertize "removed"
                    'font-lock-face
                    `((:background ,difftastic-t-removed-bg)
                      ansi-color-italic
                      ansi-color-underline
                      (:foreground ,difftastic-t-removed-fg))))))))

(ert-deftest difftastic--ansi-color-apply:removed-no-highlight-bold-italic-ansi-colors-applied ()
  (let (difftastic--ansi-color-add-background-cache
        difftastic-highlight-alist)
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[31;1;3;4mremoved[0m")
          (propertize "removed"
                      'font-lock-face
                      `(ansi-color-bold
                        ansi-color-italic
                        ansi-color-underline
                        ((:background ,difftastic-t-removed-bg)
                         (:foreground ,difftastic-t-removed-fg))))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[31;1;3;4mremoved[0m")
        (propertize "removed"
                    'font-lock-face
                    `((:background ,difftastic-t-removed-bg)
                      ansi-color-bold
                      ansi-color-italic
                      ansi-color-underline
                      (:foreground ,difftastic-t-removed-fg))))))))

(ert-deftest difftastic--ansi-color-apply:added-no-highlight-ansi-colors-applied ()
  (let (difftastic--ansi-color-add-background-cache
        difftastic-highlight-alist)
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[32;4madded[0m")
          (propertize "added"
                      'font-lock-face
                      `(ansi-color-underline
                        ((:background ,difftastic-t-added-bg)
                         (:foreground ,difftastic-t-added-fg))))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[32;4madded[0m")
        (propertize "added"
                    'font-lock-face
                    `((:background ,difftastic-t-added-bg)
                      ansi-color-underline
                      (:foreground ,difftastic-t-added-fg))))))))

(ert-deftest difftastic--ansi-color-apply:added-no-highlight-bold-ansi-colors-applied ()
  (let (difftastic--ansi-color-add-background-cache
        difftastic-highlight-alist)
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[32;1;4madded[0m")
          (propertize "added"
                      'font-lock-face
                      `(ansi-color-bold
                        ansi-color-underline
                        ((:background ,difftastic-t-added-bg)
                         (:foreground ,difftastic-t-added-fg))))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[32;1;4madded[0m")
        (propertize "added"
                    'font-lock-face
                    `((:background ,difftastic-t-added-bg)
                      ansi-color-bold
                      ansi-color-underline
                      (:foreground ,difftastic-t-added-fg))))))))

(ert-deftest difftastic--ansi-color-apply:added-no-highlight-italic-ansi-colors-applied ()
  (let (difftastic--ansi-color-add-background-cache
        difftastic-highlight-alist)
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[32;3;4madded[0m")
          (propertize "added"
                      'font-lock-face
                      `(ansi-color-italic
                        ansi-color-underline
                        ((:background ,difftastic-t-added-bg)
                         (:foreground ,difftastic-t-added-fg))))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[32;3;4madded[0m")
        (propertize "added"
                    'font-lock-face
                    `((:background ,difftastic-t-added-bg)
                      ansi-color-italic
                      ansi-color-underline
                      (:foreground ,difftastic-t-added-fg))))))))

(ert-deftest difftastic--ansi-color-apply:added-no-highlight-bold-italic-ansi-colors-applied ()
  (let (difftastic--ansi-color-add-background-cache
        difftastic-highlight-alist)
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[32;1;3;4madded[0m")
          (propertize "added"
                      'font-lock-face
                      `(ansi-color-bold
                        ansi-color-italic
                        ansi-color-underline
                        ((:background ,difftastic-t-added-bg)
                         (:foreground ,difftastic-t-added-fg))))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[32;1;3;4madded[0m")
        (propertize "added"
                    'font-lock-face
                    `((:background ,difftastic-t-added-bg)
                      ansi-color-bold
                      ansi-color-italic
                      ansi-color-underline
                      (:foreground ,difftastic-t-added-fg))))))))

(ert-deftest difftastic--ansi-color-apply:comment-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     (if noninteractive
                         :passed
                       :failed)) ;; never checked interactively
  (let (difftastic--ansi-color-add-background-cache)
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[34mcomment[0m")
          (propertize "comment"
                      'font-lock-face
                      `((:background ,difftastic-t-comment-bg)
                        (:foreground ,difftastic-t-comment-fg)))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[34mcomment[0m")
        (propertize "comment"
                    'font-lock-face
                    (if (face-background 'font-lock-comment-face)
                        `((:background ,difftastic-t-comment-bg)
                          (:foreground ,difftastic-t-comment-fg))
                      `(:foreground ,difftastic-t-comment-fg))))))))

(ert-deftest difftastic--ansi-color-apply:comment-bold-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     (if noninteractive
                         :passed
                       :failed)) ;; never checked interactively
  (let (difftastic--ansi-color-add-background-cache)
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[34;1mcomment[0m")
          (propertize "comment"
                      'font-lock-face
                      `(ansi-color-bold
                        ((:background ,difftastic-t-comment-bg)
                         (:foreground ,difftastic-t-comment-fg))))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[34;1mcomment[0m")
        (propertize "comment"
                    'font-lock-face
                    `(,@(when (face-background 'font-lock-comment-face)
                          `((:background ,difftastic-t-comment-bg)))
                      ansi-color-bold
                      (:foreground ,difftastic-t-comment-fg))))))))

(ert-deftest difftastic--ansi-color-apply:comment-italic-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     (if noninteractive
                         :passed
                       :failed)) ;; never checked interactively
  (let (difftastic--ansi-color-add-background-cache)
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[34;3mcomment[0m")
          (propertize "comment"
                      'font-lock-face
                      `(ansi-color-italic
                        ((:background ,difftastic-t-comment-bg)
                         (:foreground ,difftastic-t-comment-fg))))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[34;3mcomment[0m")
        (propertize "comment"
                    'font-lock-face
                    `(,@(when (face-background 'font-lock-comment-face)
                          `((:background ,difftastic-t-comment-bg)))
                      ansi-color-italic
                      (:foreground ,difftastic-t-comment-fg))))))))

(ert-deftest difftastic--ansi-color-apply:comment-bold-italic-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     (if noninteractive
                         :passed
                       :failed)) ;; never checked interactively
  (let (difftastic--ansi-color-add-background-cache)
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[34;1;3mcomment[0m")
          (propertize "comment"
                      'font-lock-face
                      `(ansi-color-bold
                        ansi-color-italic
                        ((:background ,difftastic-t-comment-bg)
                         (:foreground ,difftastic-t-comment-fg))))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[34;1;3mcomment[0m")
        (propertize "comment"
                    'font-lock-face
                    `(,@(when (face-background 'font-lock-comment-face)
                          `((:background ,difftastic-t-comment-bg)))
                      ansi-color-bold
                      ansi-color-italic
                      (:foreground ,difftastic-t-comment-fg))))))))

(ert-deftest difftastic--ansi-color-apply:string-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     (if noninteractive
                         :passed
                       :failed)) ;; never checked interactively
  (let (difftastic--ansi-color-add-background-cache)
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[35mstring[0m")
          (propertize "string"
                      'font-lock-face
                      `((:background ,difftastic-t-string-bg)
                        (:foreground ,difftastic-t-string-fg)))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[35mstring[0m")
        (propertize "string"
                    'font-lock-face
                    (if (face-background 'font-lock-string-face)
                        `((:background ,difftastic-t-string-bg)
                          (:foreground ,difftastic-t-string-fg))
                      `(:foreground ,difftastic-t-string-fg))))))))

(ert-deftest difftastic--ansi-color-apply:string-bold-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     (if noninteractive
                         :passed
                       :failed)) ;; never checked interactively
  (let (difftastic--ansi-color-add-background-cache)
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[35;1mstring[0m")
          (propertize "string"
                      'font-lock-face
                      `(ansi-color-bold
                        ((:background ,difftastic-t-string-bg)
                         (:foreground ,difftastic-t-string-fg))))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[35;1mstring[0m")
        (propertize "string"
                    'font-lock-face
                    `(,@(when (face-background 'font-lock-string-face)
                          `((:background ,difftastic-t-string-bg)))
                      ansi-color-bold
                      (:foreground ,difftastic-t-string-fg))))))))

(ert-deftest difftastic--ansi-color-apply:string-italic-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     (if noninteractive
                         :passed
                       :failed)) ;; never checked interactively
  (if (and (fboundp 'ert-equal-including-properties)
           (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
      (should
       (ert-equal-including-properties
        (difftastic--ansi-color-apply
         "[35;3mstring[0m")
        (propertize "string"
                    'font-lock-face
                    `(ansi-color-italic
                      ((:background ,difftastic-t-string-bg)
                       (:foreground ,difftastic-t-string-fg))))))
    (should
     (equal-including-properties
      (difftastic--ansi-color-apply
       "[35;3mstring[0m")
      (propertize "string"
                  'font-lock-face
                  `(,@(when (face-background 'font-lock-string-face)
                        `((:background ,difftastic-t-string-bg)))
                    ansi-color-italic
                    (:foreground ,difftastic-t-string-fg)))))))

(ert-deftest difftastic--ansi-color-apply:string-bold-italic-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     (if noninteractive
                         :passed
                       :failed)) ;; never checked interactively
  (let (difftastic--ansi-color-add-background-cache)
    (if (and (fboundp 'ert-equal-including-properties)
             (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ;; until Emacs-28
        (should
         (ert-equal-including-properties
          (difftastic--ansi-color-apply
           "[35;1;3mstring[0m")
          (propertize "string"
                      'font-lock-face
                      `(ansi-color-bold
                        ansi-color-italic
                        ((:background ,difftastic-t-string-bg)
                         (:foreground ,difftastic-t-string-fg))))))
      (should
       (equal-including-properties
        (difftastic--ansi-color-apply
         "[35;1;3mstring[0m")
        (propertize "string"
                    'font-lock-face
                    `(,@(when (face-background 'font-lock-string-face)
                          `((:background ,difftastic-t-string-bg)))
                      ansi-color-bold
                      ansi-color-italic
                      (:foreground ,difftastic-t-string-fg))))))))


(ert-deftest difftastic--add-visibility-indicators:from-beginning ()
  (ert-with-test-buffer ()
    (insert "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/2 --- Emacs Lisp
24 ;;; Commentary:

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-")
    (let ((difftastic-visibility-indicator '("test-hidden" . "test-shown")))
      (mocklet (((fringe-bitmap-p "test-shown") => t)
                ((difftastic--get-languages) => '("Text" "Emacs Lisp" "C++" "Java")))
        (difftastic--add-visibility-indicators (point-min))))
    (let ((overlays (cl-remove-if-not
                     (lambda (ov)
                       (overlay-get ov 'difftastic-visibility-indicator))
                     (overlays-in (point-min) (point-max)))))
      (should (= 3 (length overlays)))
      (should (equal nil
                     (cl-find-if-not
                      (lambda (ov)
                        (overlay-get ov 'evaporate))
                      overlays)))
      (should (equal nil
                     (cl-find-if-not
                      (lambda (ov)
                        (if (and (fboundp 'ert-equal-including-properties)
                                 (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ; Until Emacs-28
                            (ert-equal-including-properties
                             (overlay-get ov 'before-string)
                             (propertize "fringe"
                                         'display '(left-fringe "test-shown" fringe)))
                          (equal-including-properties
                           (overlay-get ov 'before-string)
                           (propertize "fringe"
                                       'display '(left-fringe "test-shown" fringe)))))
                      overlays))))))

(ert-deftest difftastic--add-visibility-indicators:from-middle-of-a-line ()
  (ert-with-test-buffer ()
    (insert "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/2 --- Emacs Lisp
24 ;;; Commentary:

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-")
    (let ((difftastic-visibility-indicator '("test-hidden" . "test-shown")))
      (mocklet (((fringe-bitmap-p "test-shown") => t)
                ((difftastic--get-languages) => '("Text" "Emacs Lisp" "C++" "Java")))
        (difftastic--add-visibility-indicators 125)))
    (let ((overlays (cl-remove-if-not
                     (lambda (ov)
                       (overlay-get ov 'difftastic-visibility-indicator))
                     (overlays-in (point-min) (point-max)))))
      (should (= 2 (length overlays)))
      (should (equal nil
                     (cl-find-if-not
                      (lambda (ov)
                        (overlay-get ov 'evaporate))
                      overlays)))
      (should (equal nil
                     (cl-find-if-not
                      (lambda (ov)
                        (if (and (fboundp 'ert-equal-including-properties)
                                 (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ; Until Emacs-28
                            (ert-equal-including-properties
                             (overlay-get ov 'before-string)
                             (propertize "fringe"
                                         'display '(left-fringe "test-shown" fringe)))
                          (equal-including-properties
                           (overlay-get ov 'before-string)
                           (propertize "fringe"
                                       'display '(left-fringe "test-shown" fringe)))))
                      overlays))))))

(ert-deftest difftastic--add-visibility-indicators:header-in-last-line ()
  (ert-with-test-buffer ()
    (insert "difftastic.el --- 1/2 --- Emacs Lisp")
    (let ((difftastic-visibility-indicator '("test-hidden" . "test-shown")))
      (mocklet (((fringe-bitmap-p "test-shown") => t))
        (difftastic--add-visibility-indicators (point-min))))
    (should-not (cl-remove-if-not
                     (lambda (ov)
                       (overlay-get ov 'difftastic-visibility-indicator))
                     (overlays-in (point-min) (point-max))))))

(ert-deftest difftastic--add-visibility-indicators:split-header-in-last-line ()
  (ert-with-test-buffer ()
    (insert "difftastic.el --- 1/2 --- Emacs Lisp\n")
    (let ((difftastic-visibility-indicator '("test-hidden" . "test-shown")))
      (mocklet (((fringe-bitmap-p "test-shown") => t)
                ((difftastic--get-languages) => '("Text" "Emacs Lisp" "C++" "Java")))
        (difftastic--add-visibility-indicators (point-min))))
    (let ((overlays (cl-remove-if-not
                     (lambda (ov)
                       (overlay-get ov 'difftastic-visibility-indicator))
                     (overlays-in (point-min) (point-max)))))
      (should (= 1 (length overlays)))
      (should (equal nil
                     (cl-find-if-not
                      (lambda (ov)
                        (overlay-get ov 'evaporate))
                      overlays)))
      (should (equal nil
                     (cl-find-if-not
                      (lambda (ov)
                        (if (and (fboundp 'ert-equal-including-properties)
                                 (not (get 'ert-equal-including-properties 'byte-obsolete-info))) ; Until Emacs-28
                            (ert-equal-including-properties
                             (overlay-get ov 'before-string)
                             (propertize "fringe"
                                         'display '(left-fringe "test-shown" fringe)))
                          (equal-including-properties
                           (overlay-get ov 'before-string)
                           (propertize "fringe"
                                       'display '(left-fringe "test-shown" fringe)))))
                      overlays))))))


(ert-deftest difftastic--run-command-filter:no-movement ()
  (ert-with-test-buffer ()
    (let ((process (make-process :name "test-process"
                                 :buffer (current-buffer)
                                 :command nil
                                 :noquery t)))
      (unwind-protect
          (eval
           `(mocklet (((difftastic--ansi-color-apply "test-string") => "test-buffer-string")
                      ((difftastic--add-visibility-indicators 9)))
              (insert "foo\n")
              (let ((point (point)))
                (insert "bar\n")
                (set-marker (process-mark ,process) (point))
                (goto-char point)
                (difftastic--run-command-filter ,process "test-string")
                (should (equal (buffer-string) "foo\nbar\ntest-buffer-string"))
                (should (equal (marker-position (process-mark ,process))
                               (point-max)))
                (should (equal (point) point)))))
        (when (process-live-p process)
          (kill-process process))))))

(ert-deftest difftastic--run-command-filter:movement ()
  (ert-with-test-buffer ()
    (let ((process (make-process :name "test-process"
                                 :buffer (current-buffer)
                                 :command nil
                                 :noquery t)))
      (unwind-protect
          (eval
           `(mocklet (((difftastic--ansi-color-apply "test-string") => "test-buffer-string")
                      ((difftastic--add-visibility-indicators 9)))
              (insert "foo\n")
              (insert "bar\n")
              (set-marker (process-mark ,process) (point))
              (difftastic--run-command-filter ,process "test-string")
              (should (equal (buffer-string) "foo\nbar\ntest-buffer-string"))
              (should (equal (marker-position (process-mark ,process))
                             (point-max)))
              (should (equal (point) (point-max)))))
        (when (process-live-p process)
          (kill-process process))))))


(ert-deftest difftastic--run-command-sentinel:with-output-action-called ()
  (ert-with-test-buffer ()
    (insert "test output")
    (eval
     `(mocklet (((process-status 'process) => 'exit)
                ((process-buffer 'process) => ,(current-buffer))
                ((action))
                ((message nil)))
        (difftastic--run-command-sentinel 'process #'action nil)
        (should (equal major-mode 'difftastic-mode))
        (should (equal (point) (point-min)))
        (should-not (buffer-modified-p))))))

(ert-deftest difftastic--run-command-sentinel:without-output-action-not-called ()
  (ert-with-test-buffer ()
    (eval
     `(mocklet (((process-status 'process) => 'exit)
                ((process-buffer 'process) => ,(current-buffer))
                (action not-called)
                ((message "Process '%s' returned no output" "test command")))
        (difftastic--run-command-sentinel 'process #'action '("test" "command"))
        (should (equal major-mode 'difftastic-mode))
        (should (equal (point) (point-min)))
        (should-not (buffer-modified-p))))))

(ert-deftest difftastic--run-command-sentinel:process-not-exit ()
  (ert-with-test-buffer ()
    (insert "test output")
    (eval
     '(mocklet (((process-status 'process) => 'run)
                (process-buffer not-called)
                (action not-called)
                (message not-called))
        (difftastic--run-command-sentinel 'process #'action '("test" "command"))
        (should-not (equal major-mode 'difftastic-mode))
        (should-not (equal (point) (point-min)))
        (should (buffer-modified-p))))))


(ert-deftest difftastic--run-command:basic ()
  (let* ((expected-fg
          (if noninteractive
              "unspecified-fg"
            (face-foreground (aref difftastic-normal-colors-vector 3) nil t)))
         (expected
          (concat
           (propertize
            "difftastic.el"
            'font-lock-face `(ansi-color-bold (:foreground ,expected-fg)))
           (propertize
            " --- 1/2 --- Emacs Lisp"
            'font-lock-face 'ansi-color-faint))))
    (ert-with-test-buffer ()
      (insert "test output")
      (setq buffer-read-only t)
      (eval
       '(mocklet (((action)))
          (let ((process
                 (difftastic--run-command
                  (current-buffer)
                  '("echo" "-n"
                    "[1m[33mdifftastic.el[39m[0m[2m --- 1/2 --- Emacs Lisp[0m")
                  #'action)))
            (with-timeout (5
                           (signal-process process 'SIGKILL)
                           (ert-fail "timeout"))
              (while (accept-process-output process))))))
      (if (version< "29" emacs-version) ;; since Emacs-29
          (should
           (equal-including-properties (buffer-string) expected))
        (should
         (eval `(ert-equal-including-properties (buffer-string) ,expected)))))))


(defvar split-window-preferred-direction) ;; Until Emacs-31

(ert-deftest difftastic-requested-window-width:other-window ()
  (mocklet (((count-windows) => 2)
            ((window-max-chars-per-line) => 42))
    (should (equal (difftastic-requested-window-width)
                   42))))

(ert-deftest difftastic-rerun-requested-window-width:longest-landscape-splittable ()
  (let ((split-window-preferred-direction 'longest))
    (mocklet (((count-windows) => 1)
              ((selected-window) => 'test-window)
              ((window-splittable-p 'test-window t) => t)
              ((frame-width) => 84)
              ((frame-height) => 24)
              ((window-max-chars-per-line) => 80))
      (should (equal (difftastic-requested-window-width)
                     38)))))

(ert-deftest difftastic-rerun-requested-window-width:longest-landscape-not-splittable ()
  (let ((split-window-preferred-direction 'longest))
    (mocklet (((count-windows) => 1)
              ((selected-window) => 'test-window)
              ((window-splittable-p 'test-window t))
              ((frame-width) => 84)
              ((frame-height) => 24)
              ((window-max-chars-per-line) => 80))
      (should (equal (difftastic-requested-window-width)
                     80)))))

(ert-deftest difftastic-rerun-requested-window-width:longest-portrait ()
  (let ((split-window-preferred-direction 'longest))
    (mocklet (((count-windows) => 1)
              ((selected-window) => 'test-window)
              ((frame-width) => 84)
              ((frame-height) => 90)
              ((window-max-chars-per-line) => 80))
      (should (equal (difftastic-requested-window-width)
                     80)))))

(ert-deftest difftastic-rerun-requested-window-width:horizontal-splittable ()
  (let ((split-window-preferred-direction 'horizontal))
    (mocklet (((count-windows) => 1)
              ((selected-window) => 'test-window)
              ((window-splittable-p 'test-window t) => t)
              ((frame-width) => 84)
              ((window-max-chars-per-line) => 80))
      (should (equal (difftastic-requested-window-width)
                     38)))))

(ert-deftest difftastic-rerun-requested-window-width:horizontal-not-splittable ()
  (let ((split-window-preferred-direction 'horizontal))
    (mocklet (((count-windows) => 1)
              ((selected-window) => 'test-window)
              ((window-splittable-p 'test-window t))
              ((frame-width) => 84)
              ((window-max-chars-per-line) => 80))
      (should (equal (difftastic-requested-window-width)
                     80)))))

(ert-deftest difftastic-rerun-requested-window-width:vertical ()
  (let ((split-window-preferred-direction 'vertical))
    (mocklet (((count-windows) => 1)
              ((selected-window) => 'test-window)
              ((window-max-chars-per-line) => 80))
      (should (equal (difftastic-requested-window-width)
                     80)))))

(ert-deftest difftastic-rerun-requested-window-width:no-direction-horizontal ()
  (let (split-window-preferred-direction)
    (mocklet (((count-windows) => 1)
              ((selected-window) => 'test-window)
              ((frame-width) => 84)
              ((window-max-chars-per-line) => 80))
      (cl-letf (((symbol-function #'window-splittable-p)
                 (lambda (window &optional horizontal)
                   (should (eq window 'test-window))
                   (if horizontal t nil))))
        (should (equal (difftastic-requested-window-width)
                       38))))))

(ert-deftest difftastic-rerun-requested-window-width:no-direction-vertical ()
  (let (split-window-preferred-direction)
    (mocklet (((count-windows) => 1)
              ((selected-window) => 'test-window)
              ((frame-width) => 84)
              ((window-max-chars-per-line) => 80))
      (cl-letf (((symbol-function #'window-splittable-p)
                 (lambda (window &optional horizontal)
                   (should (eq window 'test-window))
                   (if horizontal nil t))))
        (should (equal (difftastic-requested-window-width)
                       80))))))


(ert-deftest difftastic-rerun-requested-window-width:basic ()
  (mocklet (((window-max-chars-per-line) => 160))
    (should (equal (difftastic-rerun-requested-window-width)
                   160))))


(ert-deftest difftastic-pop-to-buffer:actual-bigger-than-requested-at-bottom ()
  (ert-with-test-buffer ()
    (insert "0123456789")
    (eval `(mocklet (((pop-to-buffer ,(current-buffer)
                                     (list #'display-buffer-at-bottom))))
             (difftastic-pop-to-buffer ,(current-buffer) 9)))))

(ert-deftest difftastic-pop-to-buffer:actual-not-bigger-than-requested ()
  (ert-with-test-buffer ()
    (insert "0123456789")
    (eval `(mocklet (((pop-to-buffer ,(current-buffer) (list nil))))
             (difftastic-pop-to-buffer ,(current-buffer) 10)))))


(ert-deftest difftastic--rerun:not-difftastic-mode-error-signaled ()
  (ert-with-test-buffer ()
    (let ((data (cadr (should-error (difftastic--rerun nil)
                                    :type 'user-error))))
      (should (equal data "Nothing to rerun")))))

(ert-deftest difftastic--rerun:no-rerun-alist-mode-error-signaled ()
  (ert-with-test-buffer ()
    (difftastic-mode)
    (setq difftastic--metadata nil)
    (let ((data (cadr (should-error (difftastic--rerun nil)
                                    :type 'user-error))))
      (should (equal data "Nothing to rerun")))))

(ert-deftest difftastic--rerun:git-command-rerun-requested-width ()
  (let ((metadata '((default-directory . "test-default-directory")
                    (git-command . "test-command")
                    (difftastic-args . ("test-difftastic-args"))
                    (difft-environment . nil)))
        (difftastic-rerun-requested-window-width-function
         (lambda ()
           "test-difftastic-width")))
    (ert-with-test-buffer ()
      (difftastic-mode)
      (setq difftastic--metadata metadata)
      (eval
       `(mocklet (((difftastic--build-git-process-environment
                    "test-difftastic-width" '("test-difftastic-args"))
                   => "test-process-environment")
                  ((difftastic--run-command
                    ,(current-buffer)
                    "test-command"
                    (~= (lambda (sentinel)
                          (should (equal default-directory "test-default-directory"))
                          (should (equal process-environment "test-process-environment"))
                          (should (functionp sentinel))
                          (funcall sentinel)
                          t)))))
          (difftastic--rerun nil))
       t)
      (should-not (eq difftastic--metadata metadata))
      (should (equal difftastic--metadata metadata)))))

(ert-deftest difftastic--rerun:git-command-requested-width ()
  (let ((metadata '((default-directory . "test-default-directory")
                    (git-command . "test-command")
                    (difftastic-args . ("test-difftastic-args"))
                    (difft-environment . nil)))
        (difftastic-rerun-requested-window-width-function nil)
        (difftastic-requested-window-width-function
         (lambda ()
           "test-difftastic-width")))
    (ert-with-test-buffer ()
      (difftastic-mode)
      (setq difftastic--metadata metadata)
      (eval
       `(mocklet (((difftastic--build-git-process-environment
                    "test-difftastic-width" '("test-difftastic-args"))
                   => "test-process-environment")
                  ((difftastic--run-command
                    ,(current-buffer)
                    "test-command"
                    (~= (lambda (sentinel)
                          (should (equal default-directory "test-default-directory"))
                          (should (equal process-environment "test-process-environment"))
                          (should (functionp sentinel))
                          (funcall sentinel)
                          t)))))
          (difftastic--rerun nil))
       t)
      (should-not (eq difftastic--metadata metadata))
      (should (equal difftastic--metadata metadata)))))

(ert-deftest difftastic--rerun:git-command-with-lang-override ()
  (let ((metadata '((default-directory . "test-default-directory")
                    (git-command . "test-command")
                    (difftastic-args . ("--override=*:test-lang-override-1"
                                        "test-difftastic-args"))
                    (difft-environment . nil)))
        (difftastic-rerun-requested-window-width-function
         (lambda ()
           "test-difftastic-width")))
    (ert-with-test-buffer ()
      (difftastic-mode)
      (setq difftastic--metadata metadata)
      (eval
       `(mocklet (((difftastic--build-git-process-environment
                    "test-difftastic-width" '("--override=*:test-lang-override-2"
                                              "test-difftastic-args"))
                   => "test-process-environment")
                  ((difftastic--run-command
                    ,(current-buffer)
                    "test-command"
                    (~= (lambda (sentinel)
                          (should (equal default-directory "test-default-directory"))
                          (should (equal process-environment "test-process-environment"))
                          (should (functionp sentinel))
                          (funcall sentinel)
                          t)))))
          (difftastic--rerun "test-lang-override-2"))
       t)
      (should-not (eq difftastic--metadata metadata))
      (setcar (alist-get 'difftastic-args metadata)
              "--override=*:test-lang-override-2")
      (should (equal difftastic--metadata metadata)))))

(ert-deftest difftastic--rerun:git-command-with-difftastic-args ()
  (let ((metadata '((default-directory . "test-default-directory")
                    (git-command . "test-command")
                    (difftastic-args . ("test-difftastic-args-1"
                                        "--override=*:test-lang-override-1"))
                    (difft-environment . nil)))
        (difftastic-rerun-requested-window-width-function
         (lambda ()
           "test-difftastic-width")))
    (ert-with-test-buffer ()
      (difftastic-mode)
      (setq difftastic--metadata metadata)
      (eval
       `(mocklet (((difftastic--build-git-process-environment
                    "test-difftastic-width" '("test-difftastic-args-2"))
                   => "test-process-environment")
                  ((difftastic--run-command
                    ,(current-buffer)
                    "test-command"
                    (~= (lambda (sentinel)
                          (should (equal default-directory "test-default-directory"))
                          (should (equal process-environment "test-process-environment"))
                          (should (functionp sentinel))
                          (funcall sentinel)
                          t)))))
          (difftastic--rerun '("test-difftastic-args-2")))
       t)
      (should-not (eq difftastic--metadata metadata))
      (setf (alist-get 'difftastic-args metadata)
            '("test-difftastic-args-2"))
      (should (equal difftastic--metadata metadata)))))

(ert-deftest difftastic--rerun:git-command-with-difftastic-difft-environment ()
  (let ((metadata '((default-directory . "test-default-directory")
                    (git-command . "test-command")
                    (difftastic-args . ("test-difftastic-args"))
                    (difft-environment . ("TEST_VAR=test=value"))))
        (difftastic-rerun-requested-window-width-function
         (lambda ()
           "test-difftastic-width"))
        (difftastic-difft-environment '("TEST_VAR=new-test-value")))
    (ert-with-test-buffer ()
      (difftastic-mode)
      (setq difftastic--metadata metadata)
      (eval
       `(mocklet (((difftastic--run-command
                    ,(current-buffer)
                    "test-command"
                    (~= (lambda (sentinel)
                          (should (equal default-directory "test-default-directory"))
                          (should (member "TEST_VAR=new-test-value" process-environment))
                          (should-not (member "TEST_VAR=test-value" process-environment))
                          (should (functionp sentinel))
                          (funcall sentinel)
                          t)))))
          (difftastic--rerun nil))
       t)
      (should-not (eq difftastic--metadata metadata))
      (setf (alist-get 'difft-environment metadata)
            '("TEST_VAR=new-test-value"))
      (should (equal difftastic--metadata metadata)))))

(ert-deftest difftastic--rerun:files-command-rerun-requested-width ()
  (let ((metadata '((default-directory . "test-default-directory")
                    (difftastic-args . ("test-difftastic-arg-1"))
                    (difft-environment . nil)
                    (file-buf-A . ("test-file-buf-A" . nil))
                    (file-buf-B . ("test-file-buf-B" . nil))))
        (difftastic-rerun-requested-window-width-function
         (lambda ()
           "test-difftastic-width")))
    (ert-with-test-buffer ()
      (difftastic-mode)
      (setq difftastic--metadata metadata)
      (eval
       `(mocklet (((difftastic--build-files-command
                    '("test-file-buf-A" . nil) '("test-file-buf-B". nil)
                    "test-difftastic-width" '("test-difftastic-arg-1"))
                   => "test-command")
                  ((difftastic--run-command
                    ,(current-buffer)
                    "test-command"
                    (~= (lambda (sentinel)
                          (should (equal default-directory "test-default-directory"))
                          (should-not (member "TEST_VAR=test-value" process-environment))
                          (should (functionp sentinel))
                          (funcall sentinel)
                          t)))))
          (difftastic--rerun nil))
       t)
      (should-not (eq difftastic--metadata metadata))
      (should (equal difftastic--metadata metadata)))))

(ert-deftest difftastic--rerun:files-command-requested-width ()
  (let ((metadata '((default-directory . "test-default-directory")
                    (difftastic-args . ("test-difftastic-arg-1"))
                    (difft-environment . nil)
                    (file-buf-A . ("test-file-buf-A" . nil))
                    (file-buf-B . ("test-file-buf-B" . nil))))
        (difftastic-rerun-requested-window-width-function nil)
        (difftastic-requested-window-width-function
         (lambda ()
           "test-difftastic-width")))
    (ert-with-test-buffer ()
      (difftastic-mode)
      (setq difftastic--metadata metadata)
      (eval
       `(mocklet (((difftastic--build-files-command
                    '("test-file-buf-A" . nil) '("test-file-buf-B". nil)
                    "test-difftastic-width" '("test-difftastic-arg-1"))
                   => "test-command")
                  ((difftastic--run-command
                    ,(current-buffer)
                    "test-command"
                    (~=(lambda (sentinel)
                         (should (equal default-directory "test-default-directory"))
                         (should-not (member "TEST_VAR=test-value" process-environment))
                         (should (functionp sentinel))
                         (funcall sentinel)
                         t)))))
          (difftastic--rerun nil))
       t)
      (should-not (eq difftastic--metadata metadata))
      (should (equal difftastic--metadata metadata)))))

(ert-deftest difftastic--rerun:files-command-with-lang-override ()
  (let ((metadata '((default-directory . "test-default-directory")
                    (difftastic-args . ("--override=*:test-lang-override-1"
                                        "test-difftastic-arg-1"))
                    (difft-environment . ("TEST_VAR=test-value"))
                    (file-buf-A . ("test-file-buf-A" . nil))
                    (file-buf-B . ("test-file-buf-B" . nil))))
        (difftastic-rerun-requested-window-width-function
         (lambda ()
           "test-difftastic-width")))
    (ert-with-test-buffer ()
      (difftastic-mode)
      (setq difftastic--metadata metadata)
      (eval
       `(mocklet (((difftastic--build-files-command
                    '("test-file-buf-A" . nil) '("test-file-buf-B". nil)
                    "test-difftastic-width" '("--override=*:test-lang-override-2"
                                              "test-difftastic-arg-1"))
                   => "test-command")
                  ((difftastic--run-command
                    ,(current-buffer)
                    "test-command"
                    (~= (lambda (sentinel)
                          (should (equal default-directory "test-default-directory"))
                          (should (member "TEST_VAR=test-value" process-environment))
                          (should (functionp sentinel))
                          (funcall sentinel)
                          t)))))
          (difftastic--rerun "test-lang-override-2"))
       t)
      (should-not (eq difftastic--metadata metadata))
      (setcar (alist-get 'difftastic-args metadata)
              "--override=*:test-lang-override-2")
      (should (equal difftastic--metadata metadata)))))

(ert-deftest difftastic--rerun:files-command-with-difftastic-args ()
  (let ((metadata '((default-directory . "test-default-directory")
                    (difftastic-args . ("--override=*:test-lang-override-1"
                                        "test-difftastic-arg-1"))
                    (difft-environment . ("TEST_VAR=test-value"))
                    (file-buf-A . ("test-file-buf-A" . nil))
                    (file-buf-B . ("test-file-buf-B" . nil))))
        (difftastic-rerun-requested-window-width-function
         (lambda ()
           "test-difftastic-width")))
    (ert-with-test-buffer ()
      (difftastic-mode)
      (setq difftastic--metadata metadata)
      (eval
       `(mocklet (((difftastic--build-files-command
                    '("test-file-buf-A" . nil) '("test-file-buf-B". nil)
                    "test-difftastic-width" '("test-difftastic-arg-2"))
                   => "test-command")
                  ((difftastic--run-command
                    ,(current-buffer)
                    "test-command"
                    (~= (lambda (sentinel)
                          (should (equal default-directory "test-default-directory"))
                          (should (member "TEST_VAR=test-value" process-environment))
                          (should (functionp sentinel))
                          (funcall sentinel)
                          t)))))
          (difftastic--rerun '("test-difftastic-arg-2")))
       t)
      (should-not (eq difftastic--metadata metadata))
      (setf (alist-get 'difftastic-args metadata)
            '("test-difftastic-arg-2"))
      (should (equal difftastic--metadata metadata)))))

(ert-deftest difftastic--rerun:files-command-with-difftastic-difft-environment ()
  (let ((metadata '((default-directory . "test-default-directory")
                    (difftastic-args . ("test-difftastic-arg-1"))
                    (difft-environment . ("TEST_VAR=test-value"))
                    (file-buf-A . ("test-file-buf-A" . nil))
                    (file-buf-B . ("test-file-buf-B" . nil))))
        (difftastic-rerun-requested-window-width-function
         (lambda ()
           "test-difftastic-width"))
        (difftastic-difft-environment '("TEST_VAR=new-test-value")))
    (ert-with-test-buffer ()
      (difftastic-mode)
      (setq difftastic--metadata metadata)
      (eval
       `(mocklet (((difftastic--build-files-command
                    '("test-file-buf-A" . nil) '("test-file-buf-B". nil)
                    "test-difftastic-width" '("test-difftastic-arg-1"))
                   => "test-command")
                  ((difftastic--run-command
                    ,(current-buffer)
                    "test-command"
                    (~= (lambda (sentinel)
                          (should (equal default-directory "test-default-directory"))
                          (should (member "TEST_VAR=new-test-value" process-environment))
                          (should-not (member "TEST_VAR=test-value" process-environment))
                          (should (functionp sentinel))
                          (funcall sentinel)
                          t)))))
          (difftastic--rerun nil))
       t)
      (should-not (eq difftastic--metadata metadata))
      (setf (alist-get 'difft-environment metadata)
            '("TEST_VAR=new-test-value"))
      (should (equal difftastic--metadata metadata)))))


(ert-deftest difftastic-rerun:no-prefix ()
  (mocklet (((difftastic--rerun nil)))
    (call-interactively #'difftastic-rerun)))

(ert-deftest difftastic-rerun:with-prefix ()
  (mocklet (((completing-read "Language: " "test-languages" nil t) => "test-language")
            ((difftastic--get-languages) => "test-languages")
            ((difftastic--rerun "test-language")))
    (let ((current-prefix-arg '(4)))
      (call-interactively #'difftastic-rerun))))

(ert-deftest difftastic-rerun:double-prefix ()
  (mocklet ((difftastic--rerun not-called)
            ((difftastic--with-extra-arguments nil
                                               #'difftastic--rerun)))
    (let ((current-prefix-arg '(16)))
      (call-interactively #'difftastic-rerun))))


(ert-deftest difftastic--git-with-difftastic:basic ()
  (let ((metadata '((default-directory . "test-default-directory")
                    (rev-or-range . "test-rev-or-range")
                    (git-command . "test-command")
                    (difftastic-args . "test-difftastic-args")
                    (difft-environment . "test-difft-environment")))
        (difftastic-difft-environment "test-difft-environment")
        (default-directory "test-default-directory")
        (difftastic-requested-window-width-function
         (lambda ()
           "test-difftastic-width"))
        (run-command-call-count 0)
        (display-buffer-call-count 0))
    (ert-with-test-buffer ()
      (eval
       `(mocklet (((difftastic--build-git-process-environment
                    "test-difftastic-width" "test-difftastic-args")
                   => "test-process-environment")
                  ((test-action)))
          (let ((difftastic-display-buffer-function
                 (lambda (buffer requested-width)
                   (should (equal buffer ,(current-buffer)))
                   (should (equal requested-width "test-difftastic-width"))
                   ,(cl-incf display-buffer-call-count))))
            (difftastic--with-temp-advice
                'difftastic--run-command
                :override
                (lambda (buffer command sentinel)
                  (should (equal process-environment "test-process-environment"))
                  (should (equal buffer ,(current-buffer)))
                  (should (equal command "test-command"))
                  (should (functionp sentinel))
                  (funcall sentinel)
                  ,(cl-incf run-command-call-count))
              (difftastic--git-with-difftastic ,(current-buffer)
                                               "test-command"
                                               "test-rev-or-range"
                                               "test-difftastic-args"
                                               #'test-action)))))
      (should (eq run-command-call-count 1))
      (should (eq display-buffer-call-count 1))
      (should (equal difftastic--metadata metadata)))))


(ert-deftest difftastic--magit-show:nil-error-signaled ()
  (let ((data (cadr (should-error (difftastic--magit-show nil)
                                  :type 'user-error))))
    (should (equal data "No revision specified"))))

(ert-deftest difftastic--magit-show:basic ()
  (mocklet (((get-buffer-create "*difftastic git show test-rev*")
             => "test-buffer")
            ((difftastic--git-with-difftastic
              "test-buffer"
              '("git" "--no-pager" "show" "--ext-diff" "test-rev")
              "test-rev"
              "test-difftastic-args")))
    (difftastic--magit-show "test-rev" "test-difftastic-args")))


(ert-deftest difftastic-magit-show:no-prefix-no-thing-no-branch ()
  (mocklet (((magit-thing-at-point 'git-revision t))
            ((magit-branch-or-commit-at-point))
            ((magit-read-branch-or-commit "Revision") => "test-rev")
            ((difftastic--magit-show "test-rev")))
    (call-interactively #'difftastic-magit-show)))

(ert-deftest difftastic-magit-show:no-prefix-no-thing ()
  (mocklet (((magit-thing-at-point 'git-revision t))
            ((magit-branch-or-commit-at-point) => "test-rev")
            (magit-read-branch-or-commit not-called)
            ((difftastic--magit-show "test-rev")))
    (call-interactively #'difftastic-magit-show)))

(ert-deftest difftastic-magit-show:no-prefix ()
  (mocklet (((magit-thing-at-point 'git-revision t) => "test-rev")
            (magit-branch-or-commit-at-point not-called)
            (magit-read-branch-or-commit not-called)
            ((difftastic--magit-show "test-rev")))
    (call-interactively #'difftastic-magit-show)))

(ert-deftest difftastic-magit-show:with-prefix ()
  (mocklet ((magit-thing-at-point not-called)
            (magit-branch-or-commit-at-point not-called)
            ((magit-read-branch-or-commit "Revision") => "test-rev")
            ((difftastic--magit-show "test-rev")))
    (let ((current-prefix-arg '(4)))
      (call-interactively #'difftastic-magit-show))))

(ert-deftest difftastic-magit-show:double-prefix ()
  (mocklet (((magit-thing-at-point 'git-revision t) => "test-rev")
            (magit-branch-or-commit-at-point not-called)
            (magit-read-branch-or-commit not-called)
            ((difftastic--with-extra-arguments nil
                                               #'difftastic--magit-show
                                               "test-rev")))
    (let ((current-prefix-arg '(16)))
      (call-interactively #'difftastic-magit-show))))


(ert-deftest difftastic--forge-pullreq-show-diff-args:single-prefix-argument ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     :failed)
  (mocklet (((forge-create-pullreq--read-args) => '("head" "base")))
    (let ((current-prefix-arg '(4)))
      (should (equal '("base" "head")
                     (difftastic--forge-pullreq-show-diff-args))))))

(ert-deftest difftastic--forge-pullreq-show-diff-args:triple-prefix-argument ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     :failed)
  (mocklet (((forge-create-pullreq--read-args) => '("head" "base")))
    (let ((current-prefix-arg '(64)))
      (should (equal '("base" "head")
                     (difftastic--forge-pullreq-show-diff-args))))))

(ert-deftest difftastic--forge-pullreq-show-diff-args:new-pullreq-with-base-and-head ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     :failed)
  (let ((forge--buffer-base-branch "base")
        (forge--buffer-head-branch "head")
        (forge-edit-post-action 'new-pullreq))
    (ignore forge--buffer-base-branch ;; until Emacs-29
            forge--buffer-head-branch
            forge-edit-post-action)
    (should (equal '("base" "head")
                   (difftastic--forge-pullreq-show-diff-args)))))


(ert-deftest difftastic--forge-pullreq-show-diff-args:current-topic ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     :failed)
  (let ((topic (when (fboundp 'forge-pullreq) ;; until Emacs-29
                 (forge-pullreq))))
    (oset topic base-ref "test-ref")
    (eval
     `(mocklet (((forge-current-topic) => ,topic)
                ((forge--get-remote) => "test-remote")
                ((forge--pullreq-ref ,topic) => "test-pullreq-ref"))
        (should (equal '("test-remote/test-ref" "test-pullreq-ref")
                       (difftastic--forge-pullreq-show-diff-args)))))))

(ert-deftest difftastic--forge-pullreq-show-diff-args:buffer-post-object ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     :failed)
  (let* ((topic (when (fboundp 'forge-pullreq) ;; until Emacs-29
                  (forge-pullreq)))
         (forge--buffer-post-object topic))
    (ignore forge--buffer-post-object) ;; until Emacs-29
    (oset topic base-ref "test-ref")
    (eval
     `(mocklet ((forge-current-topic)
                ((forge--get-remote) => "test-remote")
                ((forge--pullreq-ref ,topic) => "test-pullreq-ref"))
        (should (equal '("test-remote/test-ref" "test-pullreq-ref")
                       (difftastic--forge-pullreq-show-diff-args)))))))


(ert-deftest difftastic--forge-pullreq-show-diff:basic ()
  (mocklet (((difftastic--git-diff-range "base...head" nil nil nil)))
    (difftastic--forge-pullreq-show-diff "base" "head")))

(ert-deftest difftastic--forge-pullreq-show-diff:basic-with-args ()
  (mocklet (((difftastic--git-diff-range "base...head" nil nil "test-args")))
    (difftastic--forge-pullreq-show-diff "base" "head" "test-args")))

(ert-deftest difftastic--forge-pullreq-show-diff:no-head ()
  (mocklet ((difftastic--git-diff-range not-called))
    (difftastic--forge-pullreq-show-diff "base")))

(ert-deftest difftastic--forge-pullreq-show-diff:no-base ()
  (mocklet ((difftastic--git-diff-range not-called))
    (difftastic--forge-pullreq-show-diff nil "head")))

(ert-deftest difftastic--forge-pullreq-show-diff:no-base-no-head ()
  (mocklet ((difftastic--git-diff-range not-called))
    (difftastic--forge-pullreq-show-diff nil nil "test-args")))


(ert-deftest difftastic-forge-create-pulreq-show-diff:basic ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     :failed)
  (let ((forge--buffer-base-branch "base")
        (forge--buffer-head-branch "head")
        (forge-edit-post-action 'new-pullreq))
    (ignore forge--buffer-base-branch ;; until Emacs-29
            forge--buffer-head-branch
            forge-edit-post-action)
    (mocklet (((difftastic--forge-pullreq-show-diff "base" "head")))
      (difftastic-forge-create-pulreq-show-diff))))

(ert-deftest difftastic-forge-create-pulreq-show-diff:not-new-pullreq ()
  (let ((forge--buffer-base-branch "base")
        (forge--buffer-head-branch "head")
        (forge-edit-post-action 'not-new-pullreq))
    (ignore forge--buffer-base-branch ;; until Emacs-29
            forge--buffer-head-branch
            forge-edit-post-action)
    (mocklet ((difftastic--forge-pullreq-show-diff not-called))
      (difftastic-forge-create-pulreq-show-diff))))


(ert-deftest difftastic-forge-pullreq-show-diff:basic ()
  (skip-unless (version< "29" emacs-version)) ;; Since Emacs-29
  (mocklet (((difftastic--forge-pullreq-show-diff-args) => '("base" "head"))
            ((difftastic--forge-pullreq-show-diff "base" "head")))
    (call-interactively #'difftastic-forge-pullreq-show-diff)))

(ert-deftest difftastic-forge-pullreq-show-diff:single-prefix-arg ()
  (skip-unless (version< "29" emacs-version)) ;; Since Emacs-29
  (let ((current-prefix-arg '(4)))
    (mocklet (((difftastic--forge-pullreq-show-diff-args) => '("base" "head"))
              ((difftastic--forge-pullreq-show-diff "base" "head")))
      (call-interactively #'difftastic-forge-pullreq-show-diff))))

(ert-deftest difftastic-forge-pullreq-show-diff:double-prefix-arg ()
  (skip-unless (version< "29" emacs-version)) ;; Since Emacs-29
  (let ((current-prefix-arg '(16)))
    (mocklet (((difftastic--forge-pullreq-show-diff-args) => '("base" "head"))
              ((difftastic--with-extra-arguments
                nil #'difftastic--forge-pullreq-show-diff "base" "head")))
      (call-interactively #'difftastic-forge-pullreq-show-diff))))

(ert-deftest difftastic-forge-pullreq-show-diff:triple-prefix-arg ()
  (skip-unless (version< "29" emacs-version)) ;; Since Emacs-29
  (let ((current-prefix-arg '(64)))
    (mocklet (((difftastic--forge-pullreq-show-diff-args) => '("base" "head"))
              ((difftastic--with-extra-arguments
                nil #'difftastic--forge-pullreq-show-diff "base" "head")))
      (call-interactively #'difftastic-forge-pullreq-show-diff))))

(ert-deftest difftastic-forge-pullreq-show-diff:error-when-no-forge ()
  (mocklet (((featurep 'forge)))
    (let* ((text-quoting-style 'straight)
           (data
            (cadr (should-error (difftastic-forge-pullreq-show-diff)))))
      (should (equal data "Package forge is not available")))))


(ert-deftest difftastic--goto-line-col-in-chunk:single-column ()
  (ert-with-test-buffer ()
    (insert "1 1 foo
2 2 barbaz
3 3 qux
")
    (goto-char (point-min))
    (difftastic--goto-line-col-in-chunk 2 2)
    (should (equal 15 (point)))
    (goto-char (point-min))
    (difftastic--goto-line-col-in-chunk "2" 0)
    (should (equal 13 (point)))))


(ert-deftest difftastic--goto-line-col-in-chunk:single-column-no-column ()
  (ert-with-test-buffer ()
    (insert "1 1 foo
2 2 barbaz
3 3 qux
")
    (goto-char (point-min))
    (difftastic--goto-line-col-in-chunk 2 20)
    (should (equal 19 (point)))
    (goto-char (point-min))
    (difftastic--goto-line-col-in-chunk "3" 20)
    (should (equal 27 (point)))))

(ert-deftest difftastic--goto-line-col-in-chunk:single-column-no-line ()
  (ert-with-test-buffer ()
    (insert "1 1 foo
2 2 barbaz
3 3 qux
")
    (goto-char (point-min))
    (difftastic--goto-line-col-in-chunk 7 0)
    (should (equal 1 (point)))
    (goto-char (point-min))
    (difftastic--goto-line-col-in-chunk "5" 0)
    (should (equal 1 (point)))))

(ert-deftest difftastic--goto-line-col-in-chunk:side-by-side ()
  (ert-with-test-buffer ()
    (insert "1 foo    1 foo
2 barbaz 2 barbaz
3        3 qux
")
    (goto-char (point-min))
    (difftastic--goto-line-col-in-chunk 2 2)
    (should (equal 29 (point)))
    (goto-char (point-min))
    (difftastic--goto-line-col-in-chunk "2" 0)
    (should (equal 27 (point)))))

(ert-deftest difftastic--goto-line-col-in-chunk:side-by-side-column-no-column ()
  (ert-with-test-buffer ()
    (insert "1 foo    1 foo
2 barbaz 2 barbaz
3        3 qux
")
    (goto-char (point-min))
    (difftastic--goto-line-col-in-chunk 2 20)
    (should (equal 33 (point)))
    (goto-char (point-min))
    (difftastic--goto-line-col-in-chunk "3" 20)
    (should (equal 48 (point)))))

(ert-deftest difftastic--goto-line-col-in-chunk:side-by-side-no-line ()
  (ert-with-test-buffer ()
    (insert "1 foo    1 foo
2 barbaz 2 barbaz
3        3 qux
")
    (goto-char (point-min))
    (difftastic--goto-line-col-in-chunk 7 0)
    (should (equal 1 (point)))
    (goto-char (point-min))
    (difftastic--goto-line-col-in-chunk "5" 0)
    (should (equal 1 (point)))))


(ert-deftest difftastic--magit-diff-buffer-file:blob ()
  (let ((magit-buffer-refname "test-rev"))
    (mocklet (((magit-file-relative-name) => "test-file")
              ((magit-toplevel) => "test-toplevel")
              ((get-buffer-create "*difftastic git show test-rev*") => "test-buffer")
              ((difftastic--git-with-difftastic
                "test-buffer"
                '("git" "--no-pager" "show" "--ext-diff" "test-rev" "--" "test-file")
                "test-rev"
                '("test-difftastic-args"))))
      (difftastic--magit-diff-buffer-file '("test-difftastic-args")))))

(ert-deftest difftastic--magit-diff-buffer-file:file-on-branch ()
  (let ((magit-buffer-refname nil))
    (mocklet (((magit-file-relative-name) => "test-file")
              ((magit-toplevel) => "test-toplevel")
              ;; mock native functions `line-number-at-pos' and `current-column'
              ;; before `get-buffer-create' to avoid shaeningans when running
              ;; tests with cask on laptop
              ((line-number-at-pos) => 42)
              ((current-column) => 17)
              ((get-buffer-create "*difftastic git diff unstaged*")  => "test-buffer")
              ((magit-get-current-branch) => "test-branch")
              ((difftastic--git-with-difftastic
                "test-buffer"
                '("git" "--no-pager" "diff" "--ext-diff" "test-branch" "--" "test-file")
                'unstaged
                '("test-difftastic-args")
                (~= (lambda (action)
                      (when (functionp action)
                        (funcall action)
                        t)))))
              ((difftastic--goto-line-col-in-chunk 42 17)))
      (difftastic--magit-diff-buffer-file '("test-difftastic-args")))))

(ert-deftest difftastic--magit-diff-buffer-file:file ()
  (let ((magit-buffer-refname nil))
    (mocklet (((magit-file-relative-name) => "test-file")
              ((magit-toplevel) => "test-toplevel")
              ;; mock native functions `line-number-at-pos' and `current-column'
              ;; before `get-buffer-create' to avoid shaeningans when running
              ;; tests with cask on laptop
              ((line-number-at-pos) => 42)
              ((current-column) => 17)
              ((get-buffer-create "*difftastic git diff unstaged*")  => "test-buffer")
              ((magit-get-current-branch))
              ((difftastic--git-with-difftastic
                "test-buffer"
                '("git" "--no-pager" "diff" "--ext-diff" "HEAD" "--" "test-file")
                'unstaged
                '("test-difftastic-args")
                (~= (lambda (action)
                      (when (functionp action)
                        (funcall action)
                        t)))))
              ((difftastic--goto-line-col-in-chunk 42 17)))
      (difftastic--magit-diff-buffer-file '("test-difftastic-args")))))

(ert-deftest difftastic--magit-diff-buffer-file:no-file ()
  (mocklet (((magit-file-relative-name)))
    (let* ((text-quoting-style 'straight)
           (data
            (cadr (should-error (difftastic--magit-diff-buffer-file)))))
      (should (equal data "Buffer isn't visiting a file")))))


(ert-deftest difftastic-magit-diff-buffer-file:basic ()
  (mocklet ((difftastic--magit-diff-buffer-file))
    (call-interactively #'difftastic-magit-diff-buffer-file)))

(ert-deftest difftastic-magit-diff-buffer-file:double-prefix ()
  (mocklet (((difftastic--with-extra-arguments nil
                                               #'difftastic--magit-diff-buffer-file)))
    (let ((current-prefix-arg '(16)))
      (call-interactively #'difftastic-magit-diff-buffer-file))))


(ert-deftest difftastic--get-languages:parse-output ()
  (let ((file "difft--list-languages.out")
        (difftastic-executable "test-difft-executable")
        out)
    (should (or (file-exists-p file)
                (file-exists-p (format "test/%s" file))))
    (setq out (if (file-exists-p file)
                  file
                (format "test/%s" file)))
    (eval
     `(mocklet (((process-lines ,difftastic-executable "--list-languages")
                 => (ert-with-test-buffer ()
                      (insert-file-contents ,out)
                      (compat-call ;; Since Emacs-29
                       string-split (buffer-string) "\n" t))))
        ;; Hints for updating the test when difft output changes:
        ;; $ difft --list-languages > difft--list-languages.out
        ;; (insert (format "%S" (difftastic--get-languages)))
        (should (equal '("Text" "Ada" "Apex" "Bash" "C" "Clojure" "CMake"
                         "Common Lisp" "C++" "C#" "CSS" "Dart" "Elixir" "Elm"
                         "Elvish" "Emacs Lisp" "Erlang" "Gleam" "Go" "Hack"
                         "Hare" "Haskell" "HCL" "HTML" "Janet" "Java"
                         "JavaScript" "JavaScript JSX" "JSON" "Julia" "Kotlin"
                         "LaTeX" "Lua" "Make" "Newick" "Nix" "Objective-C"
                         "OCaml" "OCaml Interface" "Pascal" "Perl" "PHP"
                         "Python" "QML" "R" "Racket" "Ruby" "Rust" "Scala"
                         "SCSS" "Solidity" "SQL" "Swift" "TOML" "TypeScript"
                         "TypeScript TSX" "VHDL" "XML" "YAML" "Zig")
                       (difftastic--get-languages)))))))


(ert-deftest difftastic--get-version:parse-output ()
  (let ((file "difft--version.out")
        (difftastic-executable "test-difft-executable")
        out)
    (should (or (file-exists-p file)
                (file-exists-p (format "test/%s" file))))
    (setq out (if (file-exists-p file)
                  file
                (format "test/%s" file)))
    (eval
     `(mocklet (((process-lines ,difftastic-executable "--version")
                 => (ert-with-test-buffer ()
                      (insert-file-contents ,out)
                      (compat-call ;; Since Emacs-29
                       string-split (buffer-string) "\n" t))))
        ;; Hints for updating the test when difft output changes:
        ;; $ difft --version > difft--version.out
        (should (equal "0.65.0"
                       (difftastic--get-version)))))))


(ert-deftest difftastic--extra-arguments-completing-overrides:try ()
  (let ((fun (difftastic--extra-arguments-completing-overrides '("bar" "baz" "qux"))))
    (should (equal "*:" (funcall fun "" nil nil)))
    (should (equal "*:" (funcall fun "*" nil nil)))
    (should (equal "*:" (funcall fun "*:" nil nil)))
    (should (equal "*:ba" (funcall fun ":b" nil nil)))
    (should (equal "*:ba" (funcall fun "*:b" nil nil)))
    (should (equal "*:ba" (funcall fun "*:ba" nil nil)))
    (should (equal "*:qux" (funcall fun "*:q" nil nil)))
    (should (eq t (funcall fun "*:qux" nil nil)))
    (should-not (funcall fun "*:foo" nil nil))))


(ert-deftest difftastic--extra-arguments-completing-overrides:all ()
  (let ((fun (difftastic--extra-arguments-completing-overrides '("bar" "baz" "qux"))))
    (should (equal '("*:bar" "*:baz" "*:qux")
                   (funcall fun "" nil t)))
    (should (equal '("*:bar" "*:baz" "*:qux")
                   (funcall fun "*" nil t)))
    (should (equal '("*:bar" "*:baz" "*:qux")
                   (funcall fun "*:" nil t)))
    (should (equal '("*:bar" "*:baz")
                   (funcall fun ":b" nil t)))
    (should (equal '("*:bar" "*:baz")
                   (funcall fun "*:b" nil t)))
    (should (equal '("*:bar" "*:baz")
                   (funcall fun "*:ba" nil t)))
    (should (equal '("*:qux")
                   (funcall fun "*:q" nil t)))
    (should (equal '("*:qux") (funcall fun "*:qux" nil t)))
    (should-not (funcall fun "*:foo" nil t))))

(ert-deftest difftastic--extra-arguments-completing-overrides:boundaries ()
  (let* ((test-languages '("bar" "baz" "qux"))
         (fun (difftastic--extra-arguments-completing-overrides test-languages)))
    (should
     (equal '(0 . 0)
            (funcall fun "" nil '(boundaries . ""))))
    (should
     (equal '(0 . 1)
            (funcall fun "*" nil '(boundaries . "*"))))
    (should
     (equal '(0 . 1)
            (funcall fun "b" nil '(boundaries . "b"))))
    (should
     (equal '(0 . 3)
            (funcall fun "*:b" nil '(boundaries . "*:b"))))))

(ert-deftest difftastic--extra-arguments-completing-overrides:test ()
  (mocklet ((sit-for))
    (let ((text-quoting-style 'straight)
          (fun (difftastic--extra-arguments-completing-overrides '("bar" "baz" "qux"))))
      (ert-with-message-capture messages
        (should-not (funcall fun "bar" nil 'lambda))
        (should (equal
                 messages
                 "'bar' doesn't match GLOB:LANGUAGE pattern\n")))
      (ert-with-message-capture messages
        (should-not (funcall fun ":bar" nil 'lambda))
        (should (equal
                 messages
                 "':bar' doesn't match GLOB:LANGUAGE pattern\n")))
      (ert-with-message-capture messages
        (should-not (funcall fun "*:b" nil 'lambda))
        (should (equal
                 messages
                 "'*:b' doesn't match GLOB:LANGUAGE pattern\n")))
      (ert-with-message-capture messages
        (should-not (funcall fun "*:*:bar" nil 'lambda))
        (should (equal
                 messages
                 "'*:*:bar' doesn't match GLOB:LANGUAGE pattern\n")))
      (ert-with-message-capture messages
        (should-not (funcall fun "*:foo" nil 'lambda))
        (should (equal
                 messages
                 "'*:foo' doesn't match GLOB:LANGUAGE pattern\n")))
      (ert-with-message-capture messages
        (should (eq t (funcall fun "*:bar" nil 'lambda)))
        (should (equal messages ""))))))


(ert-deftest difftastic--extra-arguments-read-overrides:basic ()
  (mocklet (((difftastic--get-languages) => "test-languages")
            ((difftastic--extra-arguments-completing-overrides "test-languages")
             => "test-completing")
            ((magit-completing-read-multiple "test-prompt"
                                             "test-completing"
                                             nil
                                             t
                                             "test-initial-input"
                                             "test-history")
             => '("foo:bar" "baz:qux")))
    (should (equal
             (difftastic--extra-arguments-read-overrides "test-prompt"
                                                         "test-initial-input"
                                                         "test-history")
             '("foo:bar" "baz:qux")))))


(ert-deftest difftastic--extra-arguments-override-language-prompt:basic ()
  (should (equal (concat "Comma separated list of GLOB:LANGUAGE, type "
                         (propertize "TAB"
                                     'face 'help-key-binding
                                     'font-lock-face 'help-key-binding)
                         " for languages: ")
                 (difftastic--extra-arguments-override-language-prompt nil))))


(ert-deftest difftastic--extra-arguments-override-init-value:metadata ()
  (let ((obj (transient-option))
        (difftastic--metadata
         '((difftastic-args . ("--override-binary=*.foo-1"
                               "--override-binary=*.foo-2")))))
    (difftastic--extra-arguments-override-init-value obj)
    (should (equal (oref obj value) '("*.foo-1"
                                      "*.foo-2")))))

(ert-deftest difftastic--extra-arguments-override-init-value:transient-values ()
  (let ((obj (transient-option))
        (transient-values '((difftastic--with-extra-arguments
                             . (("--override-binary=" "*.foo-1" "*.foo-2")
                                "--context=5"))))
        difftastic--metadata)
    (difftastic--extra-arguments-override-init-value obj)
    (should (equal (oref obj value) '("*.foo-1"
                                      "*.foo-2")))))

(ert-deftest difftastic--extra-arguments-override-init-value:nil ()
  (let ((obj (transient-option))
        difftastic--metadata
        transient-values)
    (difftastic--extra-arguments-override-init-value obj)
    (should-not (oref obj value))))


(ert-deftest difftastic--extra-arguments-override-language-init-value:string ()
  (let ((obj (transient-option))
        (difftastic--metadata
         '((difftastic-args . ("--override=*.1:test-language-3"
                               "--override=*.2:test-language-4")))))
    (mocklet (((transient-scope) => '("test-language" "test-fun" "test-args")))
      (difftastic--extra-arguments-override-language-init-value obj))
    (should (equal (oref obj value) '("*:test-language")))))

(ert-deftest difftastic--extra-arguments-override-language-init-value:list ()
  (let ((obj (transient-option))
        (difftastic--metadata
         '((difftastic-args . ("--override=*.1:test-language-3"
                               "--override=*.2:test-language-4")))))
    (mocklet (((transient-scope) => '(("--override=*.1:test-language-1"
                                       "--override=*.2:test-language-2")
                                      "test-fun" "test-args")))
      (difftastic--extra-arguments-override-language-init-value obj))
    (should (equal (oref obj value) '("*.1:test-language-1"
                                      "*.2:test-language-2")))))

(ert-deftest difftastic--extra-arguments-override-language-init-value:metadata ()
  (let ((obj (transient-option))
        (difftastic--metadata
         '((difftastic-args . ("--override=*.1:test-language-1"
                               "--override=*.2:test-language-2")))))
    (mocklet (((transient-scope) => '(nil
                                      "test-fun" "test-args")))
      (difftastic--extra-arguments-override-language-init-value obj))
    (should (equal (oref obj value) '("*.1:test-language-1"
                                      "*.2:test-language-2")))))

(ert-deftest difftastic--extra-arguments-override-language-init-value:transient-values ()
  (let ((obj (transient-option))
        (transient-values '((difftastic--with-extra-arguments
                             . (("--override=" "*.1:test-language-1" "*.2:test-language-2")
                                "--context=5"))))
        difftastic--metadata)
    (mocklet (((transient-scope) => '(nil
                                      "test-fun" "test-args")))
      (difftastic--extra-arguments-override-language-init-value obj))
    (should (equal (oref obj value) '("*.1:test-language-1"
                                      "*.2:test-language-2")))))

(ert-deftest difftastic--extra-arguments-override-language-init-value:nil ()
  (let ((obj (transient-option))
        difftastic--metadata
        transient-values)
    (mocklet (((transient-scope) => '(nil "test-fun" "test-args")))
      (difftastic--extra-arguments-override-language-init-value obj))
    (should-not (oref obj value))))


(ert-deftest transient-init-value:no-metadata ()
  (let (transient-values
        (obj (difftastic--extra-arguments-prefix)))
    (transient-init-value obj)
    (should (slot-boundp obj 'value))
    (should-not (oref obj value))))

(ert-deftest transient-init-value:metadata ()
  (let ((obj (difftastic--extra-arguments-prefix))
        (difftastic--metadata '((difftastic-args . ("--override=test-lang-2"
                                                    "--context=5")))))
    (transient-init-value obj)
    (should (slot-boundp obj 'value))
    (should (equal '("--context=5")
                   (oref obj value)))))

(ert-deftest transient-init-value:transient-value ()
  (let ((obj (difftastic--extra-arguments-prefix))
        (transient-values '((difftastic--with-extra-arguments
                             . (("--override=" "test-lang-1" "test-lang-2")
                                "--context=5"))))
        difftastic--metadata)
    (transient-init-value obj)
    (should (slot-boundp obj 'value))
    (should (equal '("--context=5")
                   (oref obj value)))))


(ert-deftest difftastic--format-override-arg:basic ()
  (should (equal '("--override=*:test-override")
                 (difftastic--format-override-arg "test-override")))
  (should (equal '("--override=*.1:test-override-1"
                   "--override=*.2:test-override-2")
                 (difftastic--format-override-arg
                  '("--override=*.1:test-override-1"
                    "--override=*.2:test-override-2"))))
  (should-not (difftastic--format-override-arg nil)))


(ert-deftest difftastic--extra-arguments-call-command-description:basic ()
  (let ((expected (concat "run difftastic ("
                          (propertize "some-function"
                                      'face 'font-lock-constant-face)
                          ")")))
    (eval
     `(mocklet (((transient-scope) => '(nil some--function)))
        (if (version< "29" emacs-version) ;; since Emacs-29
            (should
             (equal-including-properties
              (difftastic--extra-arguments-call-command-description)
              ,expected))
          (should
           (ert-equal-including-properties
            (difftastic--extra-arguments-call-command-description)
            ,expected)))))))

(ert-deftest difftastic--extra-arguments-call-command-description:no-scope ()
  (let ((expected "run difftastic"))
    (eval
     `(mocklet (((transient-scope)))
        (if (version< "29" emacs-version) ;; since Emacs-29
            (should
             (equal-including-properties
              (difftastic--extra-arguments-call-command-description)
              ,expected))
          (should
           (ert-equal-including-properties
            (difftastic--extra-arguments-call-command-description)
            ,expected)))))))


(ert-deftest difftastic--extra-arguments-call-command:basic ()
  (let ((transient-current-prefix (transient-prefix :command "test-command")))
    (mocklet ((difftastic--override-binary-available-p => t)
              ((transient-args "test-command")
               => '(("--override=" "foo" "bar")
                    ("--override-binary=" "foo" "bar")
                    "--foo" "--bar"
                    ("--baz" "qux" "quux")))
              ((difftastic--test-fun "test-arg1" "test-arg2"
                                     '("--override=foo" "--override=bar"
                                       "--override-binary=foo" "--override-binary=bar"
                                       "--foo" "--bar"
                                       ("--baz" "qux" "quux")))
               => "test-value")
              ((transient-scope) => '("test-language"
                                      difftastic--test-fun
                                      ("test-arg1" "test-arg2"))))
      (should (equal "test-value"
                     (call-interactively
                      #'difftastic--extra-arguments-call-command))))))


(ert-deftest difftastic--with-extra-arguments:basic ()
  (mocklet (((transient-setup 'difftastic--with-extra-arguments
                              nil nil
                              :scope '("test-language"
                                       "test-fun"
                                       ("test-arg1" "test-arg2")))))
    (funcall-interactively #'difftastic--with-extra-arguments
                           "test-language"
                           "test-fun"
                           "test-arg1"
                           "test-arg2")))


(ert-deftest difftastic--git-diff-range:no-args ()
  (mocklet (((get-buffer-create "*difftastic git diff*") => "test-buffer")
            ((difftastic--git-with-difftastic
              "test-buffer"
              '("git" "--no-pager" "diff" "--ext-diff")
              nil
              nil)))
    (difftastic--git-diff-range nil nil nil)))

(ert-deftest difftastic--git-diff-range:with-args-string-rev-or-range ()
  (mocklet (((get-buffer-create
              "*difftastic git diff --ignore-submodules=all test-rev-or-range -- test-path*")
             => "test-buffer")
            ((difftastic--git-with-difftastic
              "test-buffer"
              '("git" "--no-pager" "diff" "--ext-diff" "--ignore-submodules=all"
                "test-rev-or-range" "--" "test-path")
              "test-rev-or-range"
              '("--context=42"))))
    (difftastic--git-diff-range "test-rev-or-range"
                                '("--ignore-submodules=all" "-U42")
                                '("test-path"))))

(ert-deftest difftastic--git-diff-range:with-args-symbol-rev-or-range ()
  (mocklet (((get-buffer-create
              "*difftastic git diff --ignore-submodules=all test-rev-or-range -- test-path*")
             => "test-buffer")
            ((difftastic--git-with-difftastic
              "test-buffer"
              '("git" "--no-pager" "diff" "--ext-diff" "--ignore-submodules=all"
                "--" "test-path")
              'test-rev-or-range
              '("--context=42"))))
    (difftastic--git-diff-range 'test-rev-or-range
                                '("--ignore-submodules=all" "-U42")
                                '("test-path"))))


(ert-deftest difftastic-git-diff-range:basic ()
  (mocklet (((magit-diff-read-range-or-commit "Diff for range" nil nil) => "test-rev-or-range")
            ((magit-diff-arguments) => '("test-args" "test-files"))
            ((difftastic--git-diff-range "test-rev-or-range" "test-args" "test-files")))
    (call-interactively #'difftastic-git-diff-range)))

(ert-deftest difftastic-git-diff-range:prefix ()
  (let ((current-prefix-arg '(4)))
    (mocklet (((magit-diff-read-range-or-commit "Diff for range" nil '(4)) => "test-rev-or-range")
              ((magit-diff-arguments) => '("test-args" "test-files"))
              ((difftastic--git-diff-range "test-rev-or-range" "test-args" "test-files")))
      (call-interactively #'difftastic-git-diff-range))))

(ert-deftest difftastic-git-diff-range:double-prefix ()
  (let ((current-prefix-arg '(16)))
    (mocklet (((magit-diff-read-range-or-commit "Diff for range" nil nil) => "test-rev-or-range")
              ((magit-diff-arguments) => '("test-args" "test-files"))
              ((difftastic--with-extra-arguments nil
                                                 #'difftastic--git-diff-range
                                                 "test-rev-or-range"
                                                 "test-args"
                                                 "test-files")))
      (call-interactively #'difftastic-git-diff-range))))


(ert-deftest difftastic--magit-diff:module-section ()
  (eval
   `(let ((section (magit-module-section :type 'module
                                         :range "test-range"))
          (difftastic--git-diff-range-called 0))
      (oset section value "/test-value")
      (mocklet (((magit-toplevel) => "magit-toplevel")
                ((magit-current-section) => section))
        (cl-letf (((symbol-function #'difftastic--git-diff-range)
                   (lambda (&optional rev-or-range args files difftastic-args)
                     (should (equal rev-or-range "test-range"))
                     (should-not args)
                     (should-not files)
                     (should (equal difftastic-args '("test-difftastic-args")))
                     (should (equal default-directory "/test-value/"))
                     (cl-incf difftastic--git-diff-range-called))))
          (difftastic--magit-diff "test-args" "test-files" '("test-difftastic-args"))))
      (should (equal 1 difftastic--git-diff-range-called)))))

(ert-deftest difftastic--magit-diff:module-commit-section ()
  (eval
   `(let ((parent (magit-section))
          (section (magit-section :type 'module-commit))
          (difftastic--git-diff-range-called 0))
      (oset parent value "/test-value")
      (oset section parent parent)
      (mocklet (((magit-toplevel) => "magit-toplevel")
                ((magit-current-section) => section)
                ((magit-diff--dwim) => '(commit . "test-commit")))
        (cl-letf (((symbol-function #'difftastic--git-diff-range)
                   (lambda (&optional rev-or-range args files difftastic-args)
                     (should (equal rev-or-range "test-commit^..test-commit"))
                     (should-not args)
                     (should-not files)
                     (should (equal difftastic-args '("test-difftastic-args")))
                     (should (equal default-directory "/test-value/"))
                     (cl-incf difftastic--git-diff-range-called))))
          (difftastic--magit-diff "test-args" "test-files" '("test-difftastic-args"))))
      (should (equal 1 difftastic--git-diff-range-called)))))

(ert-deftest difftastic--magit-diff:unmerged ()
  (let ((difftastic--git-diff-range-called 0))
    (mocklet (((magit-toplevel) => "magit-toplevel")
              ((magit-diff--dwim) => 'unmerged)
              ((magit-merge-in-progress-p) => t)
              ((magit--merge-range) => "test-range"))
      (cl-letf (((symbol-function #'difftastic--git-diff-range)
                 (lambda (&optional rev-or-range args files difftastic-args)
                   (should (equal rev-or-range "test-range"))
                   (should (equal args "test-args"))
                   (should (equal files "test-files"))
                   (should (equal difftastic-args '("test-difftastic-args")))
                   (should (equal default-directory "magit-toplevel"))
                   (cl-incf difftastic--git-diff-range-called))))
        (difftastic--magit-diff "test-args" "test-files" '("test-difftastic-args"))))
    (should (equal 1 difftastic--git-diff-range-called))))

(ert-deftest difftastic--magit-diff:unmerged-no-merge-in-progress ()
  (mocklet (((magit-toplevel) => "magit-toplevel")
            ((magit-diff--dwim) => 'unmerged)
            ((magit-merge-in-progress-p) => nil)
            (magit--merge-range not-called)
            (difftastic--git-diff-range not-called))
    (let ((data (cadr (should-error
                       (difftastic--magit-diff "test-args" "test-files")))))
      (should (equal data "No merge is in progress")))))

(ert-deftest difftastic--magit-diff:unstaged ()
  (let ((difftastic--git-diff-range-called 0))
    (mocklet (((magit-toplevel) => "magit-toplevel")
              ((magit-diff--dwim) => 'unstaged))
      (cl-letf (((symbol-function #'difftastic--git-diff-range)
                 (lambda (&optional rev-or-range args files difftastic-args)
                   (should (equal 'unstaged rev-or-range))
                   (should (equal args "test-args"))
                   (should (equal files "test-files"))
                   (should (equal difftastic-args '("test-difftastic-args")))
                   (should (equal default-directory "magit-toplevel"))
                   (cl-incf difftastic--git-diff-range-called))))
        (difftastic--magit-diff "test-args" "test-files" '("test-difftastic-args"))))
    (should (equal 1 difftastic--git-diff-range-called))))

(ert-deftest difftastic--magit-diff:staged-deleted-modified ()
  (let ((difftastic--git-diff-range-called 0))
    (mocklet (((magit-toplevel) => "magit-toplevel")
              ((magit-diff--dwim) => 'staged)
              ((magit-merge-in-progress-p) => t)
              ((magit-file-at-point) => "test-file-at-point")
              ((magit-file-status "test-file-at-point") => '((nil nil ?D ?U)))
              ((magit--merge-range) => "test-merge-range"))
      (cl-letf (((symbol-function #'difftastic--git-diff-range)
                 (lambda (&optional rev-or-range args files difftastic-args)
                   (should (equal rev-or-range "test-merge-range"))
                   (should (equal args "test-args"))
                   (should (equal files '("test-file-at-point")))
                   (should (equal difftastic-args '("test-difftastic-args")))
                   (should (equal default-directory "magit-toplevel"))
                   (cl-incf difftastic--git-diff-range-called))))
        (difftastic--magit-diff "test-args" "test-files" '("test-difftastic-args"))))
    (should (equal 1 difftastic--git-diff-range-called))))

(ert-deftest difftastic--magit-diff:staged-deleted-modified-no-merge-in-progress ()
  (mocklet (((magit-toplevel) => "magit-toplevel")
            ((magit-diff--dwim) => 'staged)
            ((magit-merge-in-progress-p) => nil)
            ((magit-file-at-point) => "test-file-at-point")
            ((magit-file-status "test-file-at-point") => '((nil nil ?D ?U)))
            (magit--merge-range not-called)
            (difftastic--git-diff-range not-called))
    (let ((data (cadr (should-error
                       (difftastic--magit-diff "test-args" "test-files")))))
      (should (equal data "No merge is in progress")))))

(ert-deftest difftastic--magit-diff:staged-no-file ()
  (let ((difftastic--git-diff-range-called 0))
    (mocklet (((magit-toplevel) => "magit-toplevel")
              ((magit-diff--dwim) => 'staged)
              (magit-merge-in-progress-p not-called)
              ((magit-file-at-point) => nil)
              (magit-file-status not-called)
              (magit--merge-range not-called))
      (cl-letf (((symbol-function #'difftastic--git-diff-range)
                 (lambda (&optional rev-or-range args files difftastic-args)
                   (should (equal 'staged rev-or-range))
                   (should (equal args '("--cached" "test-args")))
                   (should (equal files "test-files"))
                   (should (equal difftastic-args '("test-difftastic-args")))
                   (should (equal default-directory "magit-toplevel"))
                   (cl-incf difftastic--git-diff-range-called))))
        (difftastic--magit-diff '("test-args") "test-files" '("test-difftastic-args"))))
    (should (equal 1 difftastic--git-diff-range-called))))

(ert-deftest difftastic--magit-diff:staged-no-file-already-cached ()
  (let ((difftastic--git-diff-range-called 0))
    (mocklet (((magit-toplevel) => "magit-toplevel")
              ((magit-diff--dwim) => 'staged)
              (magit-merge-in-progress-p not-called)
              ((magit-file-at-point) => nil)
              (magit-file-status not-called)
              (magit--merge-range not-called))
      (cl-letf (((symbol-function #'difftastic--git-diff-range)
                 (lambda (&optional rev-or-range args files difftastic-args)
                   (should (equal 'staged rev-or-range))
                   (should (equal args '("test-args" "--cached")))
                   (should (equal files "test-files"))
                   (should (equal difftastic-args '("test-difftastic-args")))
                   (should (equal default-directory "magit-toplevel"))
                   (cl-incf difftastic--git-diff-range-called))))
        (difftastic--magit-diff '("test-args" "--cached") "test-files" '("test-difftastic-args"))))
    (should (equal 1 difftastic--git-diff-range-called))))

(ert-deftest difftastic--magit-diff:staged-not-deleted-modified ()
  (let ((difftastic--git-diff-range-called 0))
    (mocklet (((magit-toplevel) => "magit-toplevel")
              ((magit-diff--dwim) => 'staged)
              (magit-merge-in-progress-p not-called)
              ((magit-file-at-point) => "test-file-at-point")
              ((magit-file-status "test-file-at-point") => nil)
              (magit--merge-range not-called))
      (cl-letf (((symbol-function #'difftastic--git-diff-range)
                 (lambda (&optional rev-or-range args files difftastic-args)
                   (should (equal 'staged rev-or-range))
                   (should (equal args '("--cached" "test-args")))
                   (should (equal files "test-files"))
                   (should (equal difftastic-args '("test-difftastic-args")))
                   (should (equal default-directory "magit-toplevel"))
                   (cl-incf difftastic--git-diff-range-called))))
        (difftastic--magit-diff '("test-args") "test-files" '("test-difftastic-args"))))
    (should (equal 1 difftastic--git-diff-range-called))))

(ert-deftest difftastic--magit-diff:staged-not-deleted-modified-already-cached ()
  (let ((difftastic--git-diff-range-called 0))
    (mocklet (((magit-toplevel) => "magit-toplevel")
              ((magit-diff--dwim) => 'staged)
              (magit-merge-in-progress-p not-called)
              ((magit-file-at-point) => "test-file-at-point")
              ((magit-file-status "test-file-at-point") => nil)
              (magit--merge-range not-called))
      (cl-letf (((symbol-function #'difftastic--git-diff-range)
                 (lambda (&optional rev-or-range args files difftastic-args)
                   (should (equal 'staged rev-or-range))
                   (should (equal args '("test-args" "--cached")))
                   (should (equal files "test-files"))
                   (should (equal difftastic-args '("test-difftastic-args")))
                   (should (equal default-directory "magit-toplevel"))
                   (cl-incf difftastic--git-diff-range-called))))
        (difftastic--magit-diff '("test-args" "--cached") "test-files" '("test-difftastic-args"))))
    (should (equal 1 difftastic--git-diff-range-called))))

(ert-deftest difftastic--magit-diff:stash ()
  (let ((difftastic--git-diff-range-called 0))
    (mocklet (((magit-toplevel) => "magit-toplevel")
              ((magit-diff--dwim) => '(stash . "test-commit")))
      (cl-letf (((symbol-function #'difftastic--git-diff-range)
                 (lambda (&optional rev-or-range args files difftastic-args)
                   (should (equal rev-or-range "test-commit^..test-commit"))
                   (should (equal args "test-args"))
                   (should (equal files "test-files"))
                   (should (equal difftastic-args '("test-difftastic-args")))
                   (should (equal default-directory "magit-toplevel"))
                   (cl-incf difftastic--git-diff-range-called))))
        (difftastic--magit-diff "test-args" "test-files" '("test-difftastic-args"))))
    (should (equal 1 difftastic--git-diff-range-called))))

(ert-deftest difftastic--magit-diff:commit ()
  (let ((difftastic--git-diff-range-called 0))
    (mocklet (((magit-toplevel) => "magit-toplevel")
              ((magit-diff--dwim) => '(commit . "test-commit")))
      (cl-letf (((symbol-function #'difftastic--git-diff-range)
                 (lambda (&optional rev-or-range args files difftastic-args)
                   (should (equal rev-or-range "test-commit^..test-commit"))
                   (should (equal args "test-args"))
                   (should (equal files "test-files"))
                   (should (equal difftastic-args '("test-difftastic-args")))
                   (should (equal default-directory "magit-toplevel"))
                   (cl-incf difftastic--git-diff-range-called))))
        (difftastic--magit-diff "test-args" "test-files" '("test-difftastic-args"))))
    (should (equal 1 difftastic--git-diff-range-called))))

(ert-deftest difftastic--magit-diff:range ()
  (let ((difftastic--git-diff-range-called 0))
    (mocklet (((magit-toplevel) => "magit-toplevel")
              ((magit-diff--dwim) => "test-range"))
      (cl-letf (((symbol-function #'difftastic--git-diff-range)
                 (lambda (&optional rev-or-range args files difftastic-args)
                   (should (equal rev-or-range "test-range"))
                   (should (equal args "test-args"))
                   (should (equal files "test-files"))
                   (should (equal difftastic-args '("test-difftastic-args")))
                   (should (equal default-directory "magit-toplevel"))
                   (cl-incf difftastic--git-diff-range-called))))
        (difftastic--magit-diff "test-args" "test-files" '("test-difftastic-args"))))
    (should (equal 1 difftastic--git-diff-range-called))))

(ert-deftest difftastic--magit-diff:fallback ()
  (mocklet (((magit-toplevel) => "magit-toplevel")
            ((magit-diff--dwim) => nil)
            ((magit-diff-read-range-or-commit "Diff for range" nil nil) => "test-rev")
            ((difftastic--git-diff-range "test-rev" "test-args" "test-files" '("test-difftastic-args"))))
    (difftastic--magit-diff "test-args" "test-files" '("test-difftastic-args"))))

(ert-deftest difftastic--magit-diff:fallback-with-prefix ()
  (let ((current-prefix-arg '(4)))
    (mocklet (((magit-toplevel) => "magit-toplevel")
              ((magit-diff--dwim) => nil)
              ((magit-diff-read-range-or-commit "Diff for range" nil '(4)) => "test-rev")
              ((difftastic--git-diff-range "test-rev" "test-args" "test-files" nil)))
      (difftastic--magit-diff "test-args" "test-files"))))


(ert-deftest difftastic-magit-diff:basic ()
  (mocklet (((magit-diff-arguments) => '("test-args" "test-files"))
            ((difftastic--magit-diff "test-args" "test-files" nil)))
    (call-interactively #'difftastic-magit-diff)))

(ert-deftest difftastic-magit-diff:double-prefix ()
  (mocklet (((magit-diff-arguments) => '("test-args" "test-files"))
            ((difftastic--with-extra-arguments nil #'difftastic--magit-diff "test-args" "test-files")))
    (let ((current-prefix-arg '(16)))
      (call-interactively #'difftastic-magit-diff))))


(ert-deftest difftastic-mode--do-exit:basic ()
  (ert-with-test-buffer ()
    (let ((buffer (current-buffer))
          difftastic-exits-all-viewing-windows)
      (eval `(mocklet (((window-buffer) => ,buffer)
                       ((quit-window)))
               (difftastic-mode--do-exit))))))

(ert-deftest difftastic-mode--do-exit:all-windows ()
  (ert-with-test-buffer ()
    (let ((buffer (current-buffer))
          difftastic-exits-all-viewing-windows)
      (eval `(mocklet (((window-buffer) => ,buffer)
                       ((get-buffer-window-list) => '("window" "window"))
                       ((quit-window nil "window") :times 2))
               (difftastic-mode--do-exit nil t))))))

(ert-deftest difftastic-mode--do-exit:exits-all-viewing-windows ()
  (ert-with-test-buffer ()
    (let ((buffer (current-buffer))
          (difftastic-exits-all-viewing-windows t))
      (eval `(mocklet (((window-buffer) => ,buffer)
                       ((get-buffer-window-list) => '("window" "window"))
                       ((quit-window nil "window") :times 2))
               (difftastic-mode--do-exit))))))

(ert-deftest difftastic-mode--do-exit:basic-and-exit-action ()
  (ert-with-test-buffer ()
    (let ((buffer (current-buffer))
          difftastic-exits-all-viewing-windows)
      (eval `(mocklet (((window-buffer) => ,buffer)
                       ((quit-window))
                       ((exit-action ,buffer)))
               (difftastic-mode--do-exit #'exit-action))))))

(ert-deftest difftastic-mode--do-exit:all-windows-and-exit-action ()
  (ert-with-test-buffer ()
    (let ((buffer (current-buffer))
          difftastic-exits-all-viewing-windows)
      (eval `(mocklet (((window-buffer) => ,buffer)
                       ((get-buffer-window-list) => '("window" "window"))
                       ((quit-window nil "window") :times 2)
                       ((exit-action ,buffer)))
               (difftastic-mode--do-exit #'exit-action t))))))

(ert-deftest difftastic-mode--do-exit:exits-all-viewing-windows-and-exit-action ()
  (ert-with-test-buffer ()
    (let ((buffer (current-buffer))
          (difftastic-exits-all-viewing-windows t))
      (eval `(mocklet (((window-buffer) => ,buffer)
                       ((get-buffer-window-list) => '("window" "window"))
                       ((quit-window nil "window") :times 2)
                       ((exit-action ,buffer)))
               (difftastic-mode--do-exit #'exit-action))))))


(ert-deftest difftasitc-leave:basic ()
  (mocklet (((difftastic-mode--do-exit)))
    (funcall-interactively #'difftastic-leave)))


(ert-deftest difftasitc-quit:basic ()
  (mocklet (((difftastic-mode--do-exit 'kill-buffer)))
    (funcall-interactively #'difftastic-quit)))


(ert-deftest difftasitc-quit-all:basic ()
  (mocklet (((difftastic-mode--do-exit 'kill-buffer t)))
    (funcall-interactively #'difftastic-quit-all)))


(ert-deftest difftastic--file-buffer-args:basic ()
  (ert-with-test-buffer ()
    (eval
     `(mocklet (((difftastic--get-languages) => "test-languages")
                ((difftastic--make-suggestion "test-languages"
                                              ,(current-buffer))
                 => "test-language")
                ((completing-read "Language: "
                                  "test-languages"
                                  nil
                                  t
                                  "test-language")
                 => "test-lang"))
        (let ((current-prefix-arg '(4)))
          (should (equal '("test-lang")
                         (difftastic--file-buffer-args))))))))

(ert-deftest difftastic--file-buffer-args:no-prefix ()
  (should-not (difftastic--file-buffer-args)))


(ert-deftest difftastic--file-buffer:basic ()
  (let (temp-file)
    (unwind-protect
        (ert-with-test-buffer ()
          (setq temp-file (make-temp-file "difftastic-t"))
          (set-visited-file-name temp-file)
          (should (buffer-modified-p))
          (eval
           `(mocklet (((get-buffer-create (format "*difftastic file buffer %s*"
                                                  (buffer-name)))
                       => "test-buffer")
                      ((difftastic--make-temp-file "buffer-content"
                                                   ,(current-buffer))
                       => "test-file-buf")
                      ((difftastic--files-internal "test-buffer"
                                                   (cons ,temp-file nil)
                                                   (cons "test-file-buf"
                                                         ,(current-buffer))
                                                   '("--override=*:test-lang"))
                       => "test-val"))
              (should (equal "test-val"
                             (difftastic--file-buffer "test-lang")))))
          (set-buffer-modified-p nil))
      (when (and temp-file (file-exists-p temp-file))
        (delete-file temp-file)))))

(ert-deftest difftastic--file-buffer:buffer-not-modified ()
  (let (temp-file)
    (unwind-protect
        (ert-with-test-buffer ()
          (setq temp-file (make-temp-file "difftastic-t"))
          (write-file temp-file)
          (should-not (buffer-modified-p))
          (let ((data (cadr
                       (should-error
                        (difftastic--file-buffer "test-lang")
                        :type 'user-error))))
            (should
             (equal data
                    "Buffer has the same contents as a visited file"))))
      (when (and temp-file (file-exists-p temp-file))
        (delete-file temp-file)))))


(ert-deftest difftastic-file-buffer:basic ()
  (mocklet (((buffer-file-name) => "test-file-name")
            ((difftastic--file-buffer nil)))
    (difftastic-file-buffer)))

(ert-deftest difftastic-file-buffer:basic-with-lang-override ()
  (mocklet (((buffer-file-name) => "test-file-name")
            ((difftastic--file-buffer "test-lang")))
    (difftastic-file-buffer "test-lang")))

(ert-deftest difftastic-file-buffer:interactive ()
  (mocklet (((buffer-file-name) => "test-file-name")
            ((difftastic--file-buffer-args))
            ((difftastic--file-buffer nil)))
    (call-interactively #'difftastic-file-buffer)))

(ert-deftest difftastic-file-buffer:interactive-with-lang-override ()
  (let ((current-prefix-arg '(4)))
    (mocklet (((buffer-file-name) => "test-file-name")
              ((difftastic--file-buffer-args) => '("test-lang"))
              ((difftastic--file-buffer "test-lang")))
      (call-interactively #'difftastic-file-buffer))))

(ert-deftest difftastic-file-buffer:basic-with-double-prefix ()
  (let ((current-prefix-arg '(16)))
    (mocklet (((buffer-file-name) => "test-file-name")
              (difftastic--file-buffer not-called)
              ((difftastic--with-extra-arguments nil
                                                 #'difftastic--file-buffer)))
      (call-interactively #'difftastic-file-buffer))))

(ert-deftest difftastic-file-buffer:buffer-not-visiting-a-file ()
  (mocklet (((buffer-file-name))
            ((buffer-name) => "test-buffer-name"))
    (let ((data (cadr
                 (should-error
                  (difftastic-file-buffer)
                  :type 'user-error))))
      (should
       (equal data
              (concat "Buffer [test-buffer-name] is not visiting a file"))))))


(ert-deftest difftastic--dired-diff:basic ()
  (mocklet (((dired-diff "test-file")))
    (difftastic--dired-diff "test-file" nil)))

(ert-deftest difftastic--dired-diff:basic-and-lang-override ()
  (let ((dired-diff-called 0))
    (mocklet (((difftastic-files "current" "test-file" "test-lang")))
      (difftastic--with-temp-advice 'dired-diff
          :override
          (lambda (file &rest _)
            (should (equal file "test-file"))
            (diff "current" file)
            (cl-incf dired-diff-called))
        (difftastic--dired-diff "test-file" "test-lang")))
    (should (equal 1 dired-diff-called))))

(ert-deftest difftastic--dired-diff:interactive ()
  (let ((dired-diff-called 0))
    (mocklet (((difftastic-files "current" "test-file" "test-lang")))
      (difftastic--with-temp-advice 'dired-diff
          :override
          (lambda (&rest _)
            (interactive)
            (let ((called-interactively (called-interactively-p 'any)))
              (should called-interactively))
            (diff "current" "test-file")
            (cl-incf dired-diff-called))
        (difftastic--dired-diff 'interactive "test-lang"))
      (should (equal 1 dired-diff-called)))))

(ert-deftest difftastic--dired-diff:interactive-and-lang-override ()
  (let ((dired-diff-called 0))
    (mocklet (((difftastic-files "current" "test-file" "test-lang")))
      (difftastic--with-temp-advice 'dired-diff
          :override
          (lambda (&rest _)
            (interactive)
            (let ((called-interactively (called-interactively-p 'any)))
              (should called-interactively))
            (diff "current" "test-file")
            (cl-incf dired-diff-called))
        (difftastic--dired-diff 'interactive "test-lang"))
      (should (equal 1 dired-diff-called)))))


(ert-deftest difftastic-dired-diff:basic ()
  (mocklet (((difftastic--dired-diff "test-file" nil)))
    (difftastic-dired-diff "test-file")))

(ert-deftest difftastic-dired-diff:basic-with-lang-override ()
  (mocklet (((difftastic--dired-diff "test-file" "test-lang")))
    (difftastic-dired-diff "test-file" "test-lang")))

(ert-deftest difftastic-dired-diff:interactive ()
  (mocklet (((difftastic--dired-diff 'interactive nil))
            (completing-read not-called))
    (call-interactively #'difftastic-dired-diff)))

(ert-deftest difftastic-dired-diff:interactive-with-lang-override ()
  (let ((current-prefix-arg '(4)))
    (mocklet (((difftastic--dired-diff 'interactive "test-lang"))
              ((difftastic--get-languages) => "test-langs")
              ((completing-read "Language: " "test-langs" nil t) => "test-lang"))
      (call-interactively #'difftastic-dired-diff))))

(ert-deftest difftastic-dired-diff:interactive-with-double-prefix ()
  (let ((current-prefix-arg '(16)))
    (mocklet (((difftastic--with-extra-arguments nil
                                                 #'difftastic--dired-diff
                                                 'interactive))
              (difftastic--get-languages not-called)
              (completing-read not-called))
      (call-interactively #'difftastic-dired-diff))))


(ert-deftest difftastic--files-internal:basic ()
  (ert-with-test-buffer ()
    (let* ((display-buffer-called 0)
           (buffer (current-buffer))
           (file-buf-A (cons (make-temp-file "difftastic.t") t))
           (file-buf-B (cons (make-temp-file "difftastic.t") t))
           (difftastic-requested-window-width-function (lambda () "test-width"))
           (difftastic-display-buffer-function (lambda (buf width)
                                                 (should (equal buffer buf))
                                                 (should (equal width "test-width"))
                                                 (cl-incf display-buffer-called))))
      (unwind-protect
          (eval
           `(mocklet (((difftastic--build-files-command
                        ',file-buf-A ',file-buf-B "test-width"
                        '("test-arg-1" "test-arg-2"))
                       => '("echo" "-n" "test output")))
              (let ((process
                     (difftastic--files-internal ,buffer
                                                 ',file-buf-A
                                                 ',file-buf-B
                                                 '("test-arg-1" "test-arg-2"))))
                (with-timeout (5
                               (signal-process process 'SIGKILL)
                               (ert-fail "timeout"))
                  (while (accept-process-output process))))
              (should (equal (buffer-string) "test output"))
              (should (equal (alist-get 'default-directory difftastic--metadata)
                             default-directory))
              (should (equal (alist-get 'difftastic-args difftastic--metadata)
                             '("test-arg-1" "test-arg-2")))
              (should (equal (alist-get 'file-buf-A difftastic--metadata)
                             ',file-buf-A))
              (should (equal (alist-get 'file-buf-B difftastic--metadata)
                             ',file-buf-B))
              (should-not (file-exists-p (car ',file-buf-A)))
              (should-not (file-exists-p (car ',file-buf-B)))))
        (when (file-exists-p (car file-buf-A))
          (delete-file (car file-buf-A)))
        (when (file-exists-p (car file-buf-B))
          (delete-file (car file-buf-B))))
      (should (equal display-buffer-called 1)))))

(ert-deftest difftastic--files-internal:with-difftastic-difft-environment ()
  (ert-with-test-buffer ()
    (let* ((display-buffer-called 0)
           (buffer (current-buffer))
           (file-buf-A (cons (make-temp-file "difftastic.t") t))
           (file-buf-B (cons (make-temp-file "difftastic.t") t))
           (difftastic-requested-window-width-function (lambda () "test-width"))
           (difftastic-display-buffer-function (lambda (buf width)
                                                 (should (equal buffer buf))
                                                 (should (equal width "test-width"))
                                                 (cl-incf display-buffer-called)))
           (difftastic-difft-environment '("TEST_VAR=test-value")))
      (unwind-protect
          (eval
           `(mocklet (((difftastic--build-files-command
                        ',file-buf-A ',file-buf-B "test-width"
                        '("test-arg-1" "test-arg-2"))
                       => '("sh" "-c" "printf '%s' $TEST_VAR")))
              (let ((process
                     (difftastic--files-internal ,buffer
                                                 ',file-buf-A
                                                 ',file-buf-B
                                                 '("test-arg-1" "test-arg-2"))))
                (with-timeout (5
                               (signal-process process 'SIGKILL)
                               (ert-fail "timeout"))
                  (while (accept-process-output process))))
              (should (equal (buffer-string) "test-value"))
              (should (equal (alist-get 'default-directory difftastic--metadata)
                             default-directory))
              (should (equal (alist-get 'difftastic-args difftastic--metadata)
                             '("test-arg-1" "test-arg-2")))
              (should (equal (alist-get 'file-buf-A difftastic--metadata)
                             ',file-buf-A))
              (should (equal (alist-get 'file-buf-B difftastic--metadata)
                             ',file-buf-B))
              (should (equal (alist-get 'difft-environment difftastic--metadata)
                             '("TEST_VAR=test-value")))
              (should-not (file-exists-p (car ',file-buf-A)))
              (should-not (file-exists-p (car ',file-buf-B)))))
        (when (file-exists-p (car file-buf-A))
          (delete-file (car file-buf-A)))
        (when (file-exists-p (car file-buf-B))
          (delete-file (car file-buf-B))))
      (should (equal display-buffer-called 1)))))


(ert-deftest difftastic--buffers-args-read-buffer-predicate ()
  (let ((pred (difftastic--buffers-args-read-buffer-predicate "test-buffer-A")))
    (should (functionp pred))
    (should (funcall pred "test-buffer-B"))
    (should (funcall pred (cons "test-buffer-C" nil)))
    (should-not (funcall pred "test-buffer-A"))
    (should-not (funcall pred (cons "test-buffer-A" nil)))))


(ert-deftest difftastic--buffers-args:buffers-with-files ()
  (cl-letf* ((file-A (make-temp-file "difftastic.t"))
             (file-B (make-temp-file "difftastic.t"))
             (buffer-A (let ((buffer (generate-new-buffer "*temp*" t)))
                         (with-current-buffer buffer
                           (write-region (point-min) (point-max) file-A nil t))
                         buffer))
             (buffer-B (let ((buffer (generate-new-buffer "*temp*" t)))
                         (with-current-buffer buffer
                           (write-region (point-min) (point-max) file-B nil t))
                         buffer))
             (read-buffer-args `((,(buffer-name buffer-A)
                                  "Buffer A to compare: " "default-buffer-A" t)
                                 (,(buffer-name buffer-B)
                                  "Buffer B to compare: " "default-buffer-B" t "test-predicate")))
             (read-buffer-called 0)
             ((symbol-function #'difftastic--buffers-args-read-buffer-predicate)
              (lambda (buf)
                (should (equal buf (buffer-name buffer-A)))
                "test-predicate"))
             ((symbol-function #'read-buffer)
              (lambda (&rest args)
                (let ((current (car read-buffer-args)))
                  (setq read-buffer-args (cdr read-buffer-args))
                  (should (equal args (cdr current)))
                  (cl-incf read-buffer-called)
                  (car current))))
             (ediff-other-buffer-args `(("default-buffer-A" "")
                                        ("default-buffer-B" ,(buffer-name buffer-A))))
             (ediff-other-buffer-called 0)
             ((symbol-function #'ediff-other-buffer)
              (lambda (&rest args)
                (let ((current (car ediff-other-buffer-args)))
                  (setq ediff-other-buffer-args (cdr ediff-other-buffer-args))
                  (should (equal args (cdr current)))
                  (cl-incf ediff-other-buffer-called)
                  (car current)))))
    (unwind-protect
        (progn
          (should (equal (difftastic--buffers-args)
                         (list
                          (buffer-name buffer-A)
                          (buffer-name buffer-B)
                          nil)))
          (should (equal read-buffer-called 2))
          (should (equal ediff-other-buffer-called 2)))
      (when (buffer-name buffer-B)
        (kill-buffer buffer-B))
      (when (buffer-name buffer-A)
        (kill-buffer buffer-A))
      (when (file-exists-p file-B)
        (delete-file file-B))
      (when (file-exists-p file-A)
        (delete-file file-A)))))

(ert-deftest difftastic--buffers-args:buffers-with-files-and-prefix ()
  (cl-letf* ((file-A (make-temp-file "difftastic.t"))
             (file-B (make-temp-file "difftastic.t"))
             (buffer-A (let ((buffer (generate-new-buffer "*temp*" t)))
                         (with-current-buffer buffer
                           (write-region (point-min) (point-max) file-A nil t))
                         buffer))
             (buffer-B (let ((buffer (generate-new-buffer "*temp*" t)))
                         (with-current-buffer buffer
                           (write-region (point-min) (point-max) file-B nil t))
                         buffer))
             (read-buffer-args `((,(buffer-name buffer-A)
                                  "Buffer A to compare: " "default-buffer-A" t)
                                 (,(buffer-name buffer-B)
                                  "Buffer B to compare: " "default-buffer-B" t "test-predicate")))
             (read-buffer-called 0)
             ((symbol-function #'difftastic--buffers-args-read-buffer-predicate)
              (lambda (buf)
                (should (equal buf (buffer-name buffer-A)))
                "test-predicate"))
             ((symbol-function #'read-buffer)
              (lambda (&rest args)
                (let ((current (car read-buffer-args)))
                  (setq read-buffer-args (cdr read-buffer-args))
                  (should (equal args (cdr current)))
                  (cl-incf read-buffer-called)
                  (car current))))
             (ediff-other-buffer-args `(("default-buffer-A" "")
                                        ("default-buffer-B" ,(buffer-name buffer-A))))
             (ediff-other-buffer-called 0)
             ((symbol-function #'ediff-other-buffer)
              (lambda (&rest args)
                (let ((current (car ediff-other-buffer-args)))
                  (setq ediff-other-buffer-args (cdr ediff-other-buffer-args))
                  (should (equal args (cdr current)))
                  (cl-incf ediff-other-buffer-called)
                  (car current))))
             (current-prefix-arg '(4)))
    (unwind-protect
        (progn
          (eval
           `(mocklet
                (((difftastic--get-languages) => "test-languages")
                 ((difftastic--make-suggestion
                   "test-languages" ,buffer-A ,buffer-B)
                  => "test-suggestion")
                 ((completing-read
                   "Language: " "test-languages" nil t "test-suggestion")
                  => "test-lang"))
              (should (equal (difftastic--buffers-args)
                             (list
                              (buffer-name ,buffer-A)
                              (buffer-name ,buffer-B)
                              "test-lang")))))
          (should (equal read-buffer-called 2))
          (should (equal ediff-other-buffer-called 2)))
      (when (buffer-name buffer-B)
        (kill-buffer buffer-B))
      (when (buffer-name buffer-A)
        (kill-buffer buffer-A))
      (when (file-exists-p file-B)
        (delete-file file-B))
      (when (file-exists-p file-A)
        (delete-file file-A)))))

(ert-deftest difftastic--buffers-args:buffer-A-with-file ()
  (cl-letf* ((file-A (make-temp-file "difftastic.t"))
             (buffer-A (let ((buffer (generate-new-buffer "*temp*" t)))
                         (with-current-buffer buffer
                           (write-region (point-min) (point-max) file-A nil t))
                         buffer))
             (buffer-B (generate-new-buffer "*temp*" t))
             (read-buffer-args `((,(buffer-name buffer-A)
                                  "Buffer A to compare: " "default-buffer-A" t)
                                 (,(buffer-name buffer-B)
                                  "Buffer B to compare: " "default-buffer-B" t "test-predicate")))
             (read-buffer-called 0)
             ((symbol-function #'difftastic--buffers-args-read-buffer-predicate)
              (lambda (buf)
                (should (equal buf (buffer-name buffer-A)))
                "test-predicate"))
             ((symbol-function #'read-buffer)
              (lambda (&rest args)
                (let ((current (car read-buffer-args)))
                  (setq read-buffer-args (cdr read-buffer-args))
                  (should (equal args (cdr current)))
                  (cl-incf read-buffer-called)
                  (car current))))
             (ediff-other-buffer-args `(("default-buffer-A" "")
                                        ("default-buffer-B" ,(buffer-name buffer-A))))
             (ediff-other-buffer-called 0)
             ((symbol-function #'ediff-other-buffer)
              (lambda (&rest args)
                (let ((current (car ediff-other-buffer-args)))
                  (setq ediff-other-buffer-args (cdr ediff-other-buffer-args))
                  (should (equal args (cdr current)))
                  (cl-incf ediff-other-buffer-called)
                  (car current)))))
    (unwind-protect
        (progn
          (should (equal (difftastic--buffers-args)
                         (list
                          (buffer-name buffer-A)
                          (buffer-name buffer-B)
                          nil)))
          (should (equal read-buffer-called 2))
          (should (equal ediff-other-buffer-called 2)))
      (when (buffer-name buffer-B)
        (kill-buffer buffer-B))
      (when (buffer-name buffer-A)
        (kill-buffer buffer-A))
      (when (file-exists-p file-A)
        (delete-file file-A)))))

(ert-deftest difftastic--buffers-args:buffer-B-with-file ()
  (cl-letf* ((file-B (make-temp-file "difftastic.t"))
             (buffer-A (generate-new-buffer "*temp*" t))
             (buffer-B (let ((buffer (generate-new-buffer "*temp*" t)))
                         (with-current-buffer buffer
                           (write-region (point-min) (point-max) file-B nil t))
                         buffer))
             (read-buffer-args `((,(buffer-name buffer-A)
                                  "Buffer A to compare: " "default-buffer-A" t)
                                 (,(buffer-name buffer-B)
                                  "Buffer B to compare: " "default-buffer-B" t "test-predicate")))
             (read-buffer-called 0)
             ((symbol-function #'difftastic--buffers-args-read-buffer-predicate)
              (lambda (buf)
                (should (equal buf (buffer-name buffer-A)))
                "test-predicate"))
             ((symbol-function #'read-buffer)
              (lambda (&rest args)
                (let ((current (car read-buffer-args)))
                  (setq read-buffer-args (cdr read-buffer-args))
                  (should (equal args (cdr current)))
                  (cl-incf read-buffer-called)
                  (car current))))
             (ediff-other-buffer-args `(("default-buffer-A" "")
                                        ("default-buffer-B" ,(buffer-name buffer-A))))
             (ediff-other-buffer-called 0)
             ((symbol-function #'ediff-other-buffer)
              (lambda (&rest args)
                (let ((current (car ediff-other-buffer-args)))
                  (setq ediff-other-buffer-args (cdr ediff-other-buffer-args))
                  (should (equal args (cdr current)))
                  (cl-incf ediff-other-buffer-called)
                  (car current)))))
    (unwind-protect
        (progn
          (should (equal (difftastic--buffers-args)
                         (list
                          (buffer-name buffer-A)
                          (buffer-name buffer-B)
                          nil)))
          (should (equal read-buffer-called 2))
          (should (equal ediff-other-buffer-called 2)))
      (when (buffer-name buffer-B)
        (kill-buffer buffer-B))
      (when (buffer-name buffer-A)
        (kill-buffer buffer-A))
      (when (file-exists-p file-B)
        (delete-file file-B)))))

(ert-deftest difftastic--buffers-args:buffers-without-files ()
  (cl-letf* ((buffer-A (generate-new-buffer "*temp*" t))
             (buffer-B (generate-new-buffer "*temp*" t))
             (read-buffer-args `((,(buffer-name buffer-A)
                                  "Buffer A to compare: " "default-buffer-A" t)
                                 (,(buffer-name buffer-B)
                                  "Buffer B to compare: " "default-buffer-B" t "test-predicate")))
             (read-buffer-called 0)
             ((symbol-function #'difftastic--buffers-args-read-buffer-predicate)
              (lambda (buf)
                (should (equal buf (buffer-name buffer-A)))
                "test-predicate"))
             ((symbol-function #'read-buffer)
              (lambda (&rest args)
                (let ((current (car read-buffer-args)))
                  (setq read-buffer-args (cdr read-buffer-args))
                  (should (equal args (cdr current)))
                  (cl-incf read-buffer-called)
                  (car current))))
             (ediff-other-buffer-args `(("default-buffer-A" "")
                                        ("default-buffer-B" ,(buffer-name buffer-A))))
             (ediff-other-buffer-called 0)
             ((symbol-function #'ediff-other-buffer)
              (lambda (&rest args)
                (let ((current (car ediff-other-buffer-args)))
                  (setq ediff-other-buffer-args (cdr ediff-other-buffer-args))
                  (should (equal args (cdr current)))
                  (cl-incf ediff-other-buffer-called)
                  (car current)))))
    (unwind-protect
        (progn
          (eval
           `(mocklet
                (((difftastic--get-languages) => "test-languages")
                 ((difftastic--make-suggestion
                   "test-languages" ,buffer-A ,buffer-B)
                  => "test-suggestion")
                 ((completing-read
                   "Language: " "test-languages" nil t "test-suggestion")
                  => "test-lang"))
              (should (equal (difftastic--buffers-args)
                             (list
                              (buffer-name ,buffer-A)
                              (buffer-name ,buffer-B)
                              "test-lang")))))
          (should (equal read-buffer-called 2))
          (should (equal ediff-other-buffer-called 2)))
      (when (buffer-name buffer-B)
        (kill-buffer buffer-B))
      (when (buffer-name buffer-A)
        (kill-buffer buffer-A)))))


(ert-deftest difftastic--buffers:with-lang-override ()
  (let* ((file-A (make-temp-file "difftastic.t"))
         (file-B (make-temp-file "difftastic.t"))
         (buffer-A (let ((buffer (generate-new-buffer "*temp*" t)))
                     (with-current-buffer buffer
                       (write-region (point-min) (point-max) file-A nil t))
                     buffer))
         (buffer-B (let ((buffer (generate-new-buffer "*temp*" t)))
                     (with-current-buffer buffer
                       (write-region (point-min) (point-max) file-B nil t))
                     buffer)))
    (unwind-protect
        (eval
         `(mocklet (((get-buffer-create ,(format "*difftastic %s %s*"
                                                 (buffer-name buffer-A)
                                                 (buffer-name buffer-B)))
                     => "test-buffer")
                    ((difftastic--files-internal "test-buffer"
                                                 ',(cons file-A nil)
                                                 ',(cons file-B nil)
                                                 '("--override=*:test-language"))))
            (difftastic--buffers ,(buffer-name buffer-A)
                                 ,(buffer-name buffer-B)
                                 "test-language")))
      (when (buffer-name buffer-B)
        (kill-buffer buffer-B))
      (when (buffer-name buffer-A)
        (kill-buffer buffer-A))
      (when (file-exists-p file-B)
        (delete-file file-B))
      (when (file-exists-p file-A)
        (delete-file file-A)))))

(ert-deftest difftastic--buffers:with-difftastic-args ()
  (let* ((file-A (make-temp-file "difftastic.t"))
         (file-B (make-temp-file "difftastic.t"))
         (buffer-A (let ((buffer (generate-new-buffer "*temp*" t)))
                     (with-current-buffer buffer
                       (write-region (point-min) (point-max) file-A nil t))
                     buffer))
         (buffer-B (let ((buffer (generate-new-buffer "*temp*" t)))
                     (with-current-buffer buffer
                       (write-region (point-min) (point-max) file-B nil t))
                     buffer)))
    (unwind-protect
        (eval
         `(mocklet (((get-buffer-create ,(format "*difftastic %s %s*"
                                                 (buffer-name buffer-A)
                                                 (buffer-name buffer-B)))
                     => "test-buffer")
                    ((difftastic--files-internal "test-buffer"
                                                 ',(cons file-A nil)
                                                 ',(cons file-B nil)
                                                 '("test-difftastic-arg"))))
            (difftastic--buffers ,(buffer-name buffer-A)
                                 ,(buffer-name buffer-B)
                                 '("test-difftastic-arg"))))
      (when (buffer-name buffer-B)
        (kill-buffer buffer-B))
      (when (buffer-name buffer-A)
        (kill-buffer buffer-A))
      (when (file-exists-p file-B)
        (delete-file file-B))
      (when (file-exists-p file-A)
        (delete-file file-A)))))

(ert-deftest difftastic--buffers:same-buffer ()
  (let ((data (cadr
               (should-error
                (difftastic--buffers "test-buffer" "test-buffer" nil)
                :type 'user-error))))
    (should
     (equal data
            "Buffers have to be different"))))


(ert-deftest difftastic-buffers:basic ()
  (mocklet (((difftastic--buffers-args) => '("test-buffer-A"
                                             "test-buffer-B"
                                             "test-language"))
            ((difftastic--buffers "test-buffer-A" "test-buffer-B" "test-language")))
    (call-interactively #'difftastic-buffers)))

(ert-deftest difftastic-buffers:double-prefix ()
  (let ((current-prefix-arg '(16)))
    (mocklet (((difftastic--buffers-args) => '("test-buffer-A"
                                               "test-buffer-B"
                                               "test-language"))
              ((difftastic--with-extra-arguments "test-language"
                                                 #'difftastic--buffers
                                                 "test-buffer-A"
                                                 "test-buffer-B")))
      (call-interactively #'difftastic-buffers))))


(ert-deftest difftastic--files-args:use-last-dir ()
  (cl-letf* ((orig-file-name-history (copy-tree file-name-history))
             (difftastic-use-last-dir t)
             (difftastic--last-dir-A "/last-dir-A")
             (difftastic--last-dir-B "/last-dir-B")
             (ediff-read-file-name-args '(("/last-dir-A/file-A"
                                           "File A to compare" "/last-dir-A" "default-A")
                                          ("/last-dir-B/file-B"
                                           "File B to compare" "/last-dir-B" "default-B")))
             (ediff-read-file-name-called 0)
             ((symbol-function #'ediff-read-file-name)
              (lambda (&rest args)
                (let ((current (car ediff-read-file-name-args)))
                  (setq ediff-read-file-name-args
                        (cdr ediff-read-file-name-args))
                  (should (equal args (cdr current)))
                  (cl-incf ediff-read-file-name-called)
                  (car current))))
             (ediff-get-default-file-name-args '(("default-A")
                                                 ("default-B"
                                                  "/last-dir-A/file-A" 1)))
             (ediff-get-default-file-name-called 0)
             ((symbol-function #'ediff-get-default-file-name)
              (lambda (&rest args)
                (let ((current (car ediff-get-default-file-name-args)))
                  (setq ediff-get-default-file-name-args
                        (cdr ediff-get-default-file-name-args))
                  (should (equal args (cdr current)))
                  (cl-incf ediff-get-default-file-name-called)
                  (when (cdr current)
                    (should (equal (car file-name-history)
                                   "/last-dir-B/file-A"))) ; [sic!]
                  (car current)))))
    (unwind-protect
        (progn
          (should (equal (difftastic--files-args)
                         (list "/last-dir-A/file-A"
                               "/last-dir-B/file-B"
                               nil)))
          (should-not (cl-set-exclusive-or
                       (cl-subseq file-name-history 0 2)
                       '("/last-dir-A/file-A"
                         "/last-dir-B/file-B")
                       :test #'equal))
          (should (equal (caddr file-name-history) (car orig-file-name-history)))
          (should (equal difftastic--last-dir-A "/last-dir-A/"))
          (should (equal difftastic--last-dir-B "/last-dir-B/"))
          (should (equal ediff-read-file-name-called 2))
          (should (equal ediff-get-default-file-name-called 2)))
      (setq file-name-history orig-file-name-history))))

(ert-deftest difftastic--files-args:use-default-dir ()
  (cl-letf* ((orig-file-name-history (copy-tree file-name-history))
             (difftastic-use-last-dir nil)
             (default-directory "/test-directory")
             (ediff-read-file-name-args '(("/test-directory/file-A"
                                           "File A to compare" "/test-directory" "default-A")
                                          ("/test-directory/file-B"
                                           "File B to compare" "/test-directory/" "default-B")))
             (ediff-read-file-name-called 0)
             ((symbol-function #'ediff-read-file-name)
              (lambda (&rest args)
                (let ((current (car ediff-read-file-name-args)))
                  (setq ediff-read-file-name-args
                        (cdr ediff-read-file-name-args))
                  (should (equal args (cdr current)))
                  (cl-incf ediff-read-file-name-called)
                  (car current))))
             (ediff-get-default-file-name-args '(("default-A")
                                                 ("default-B"
                                                  "/test-directory/file-A" 1)))
             (ediff-get-default-file-name-called 0)
             ((symbol-function #'ediff-get-default-file-name)
              (lambda (&rest args)
                (let ((current (car ediff-get-default-file-name-args)))
                  (setq ediff-get-default-file-name-args
                        (cdr ediff-get-default-file-name-args))
                  (should (equal args (cdr current)))
                  (cl-incf ediff-get-default-file-name-called)
                  (when (cdr current)
                    (should (equal (car file-name-history)
                                   "/test-directory/file-A")))
                  (car current)))))
    (unwind-protect
        (progn
          (should (equal (difftastic--files-args)
                         (list "/test-directory/file-A"
                               "/test-directory/file-B"
                               nil)))
          (should-not (cl-set-exclusive-or
                       (cl-subseq file-name-history 0 2)
                       '("/test-directory/file-A"
                         "/test-directory/file-B")
                       :test #'equal))
          (should (equal (caddr file-name-history) (car orig-file-name-history)))
          (should (equal difftastic--last-dir-A "/test-directory/"))
          (should (equal difftastic--last-dir-B "/test-directory/"))
          (should (equal ediff-read-file-name-called 2))
          (should (equal ediff-get-default-file-name-called 2)))
      (setq file-name-history orig-file-name-history))))

(ert-deftest difftastic--files-args:use-last-dir-with-prefix ()
  (cl-letf* ((orig-file-name-history (copy-tree file-name-history))
             (difftastic-use-last-dir t)
             (difftastic--last-dir-A "/last-dir-A")
             (difftastic--last-dir-B "/last-dir-B")
             (ediff-read-file-name-args '(("/last-dir-A/file-A"
                                           "File A to compare" "/last-dir-A" "default-A")
                                          ("/last-dir-B/file-B"
                                           "File B to compare" "/last-dir-B" "default-B")))
             (ediff-read-file-name-called 0)
             ((symbol-function #'ediff-read-file-name)
              (lambda (&rest args)
                (let ((current (car ediff-read-file-name-args)))
                  (setq ediff-read-file-name-args
                        (cdr ediff-read-file-name-args))
                  (should (equal args (cdr current)))
                  (cl-incf ediff-read-file-name-called)
                  (car current))))
             (ediff-get-default-file-name-args '(("default-A")
                                                 ("default-B"
                                                  "/last-dir-A/file-A" 1)))
             (ediff-get-default-file-name-called 0)
             ((symbol-function #'ediff-get-default-file-name)
              (lambda (&rest args)
                (let ((current (car ediff-get-default-file-name-args)))
                  (setq ediff-get-default-file-name-args
                        (cdr ediff-get-default-file-name-args))
                  (should (equal args (cdr current)))
                  (cl-incf ediff-get-default-file-name-called)
                  (when (cdr current)
                    (should (equal (car file-name-history)
                                   "/last-dir-B/file-A"))) ; [sic!]
                  (car current))))
             (current-prefix-arg '(4)))
    (unwind-protect
        (progn
          (mocklet (((difftastic--get-languages) => "test-languages")
                    ((completing-read "Language: " "test-languages" nil t) => "test-lang"))
            (should (equal (difftastic--files-args)
                           (list "/last-dir-A/file-A"
                                 "/last-dir-B/file-B"
                                 "test-lang"))))
          (should-not (cl-set-exclusive-or
                       (cl-subseq file-name-history 0 2)
                       '("/last-dir-A/file-A"
                         "/last-dir-B/file-B")
                       :test #'equal))
          (should (equal (caddr file-name-history) (car orig-file-name-history)))
          (should (equal difftastic--last-dir-A "/last-dir-A/"))
          (should (equal difftastic--last-dir-B "/last-dir-B/"))
          (should (equal ediff-read-file-name-called 2))
          (should (equal ediff-get-default-file-name-called 2)))
      (setq file-name-history orig-file-name-history))))

(ert-deftest difftastic--files-args:use-default-dir-with-prefix ()
  (cl-letf* ((orig-file-name-history (copy-tree file-name-history))
             (difftastic-use-last-dir nil)
             (default-directory "/test-directory")
             (ediff-read-file-name-args '(("/test-directory/file-A"
                                           "File A to compare" "/test-directory" "default-A")
                                          ("/test-directory/file-B"
                                           "File B to compare" "/test-directory/" "default-B")))
             (ediff-read-file-name-called 0)
             ((symbol-function #'ediff-read-file-name)
              (lambda (&rest args)
                (let ((current (car ediff-read-file-name-args)))
                  (setq ediff-read-file-name-args
                        (cdr ediff-read-file-name-args))
                  (should (equal args (cdr current)))
                  (cl-incf ediff-read-file-name-called)
                  (car current))))
             (ediff-get-default-file-name-args '(("default-A")
                                                 ("default-B"
                                                  "/test-directory/file-A" 1)))
             (ediff-get-default-file-name-called 0)
             ((symbol-function #'ediff-get-default-file-name)
              (lambda (&rest args)
                (let ((current (car ediff-get-default-file-name-args)))
                  (setq ediff-get-default-file-name-args
                        (cdr ediff-get-default-file-name-args))
                  (should (equal args (cdr current)))
                  (cl-incf ediff-get-default-file-name-called)
                  (when (cdr current)
                    (should (equal (car file-name-history)
                                   "/test-directory/file-A")))
                  (car current))))
             (current-prefix-arg '(4)))
    (unwind-protect
        (progn
          (mocklet (((difftastic--get-languages) => "test-languages")
                    ((completing-read "Language: " "test-languages" nil t) => "test-lang"))
            (should (equal (difftastic--files-args)
                           (list "/test-directory/file-A"
                                 "/test-directory/file-B"
                                 "test-lang"))))
          (should-not (cl-set-exclusive-or
                       (cl-subseq file-name-history 0 2)
                       '("/test-directory/file-A"
                         "/test-directory/file-B")
                       :test #'equal))
          (should (equal (caddr file-name-history) (car orig-file-name-history)))
          (should (equal difftastic--last-dir-A "/test-directory/"))
          (should (equal difftastic--last-dir-B "/test-directory/"))
          (should (equal ediff-read-file-name-called 2))
          (should (equal ediff-get-default-file-name-called 2)))
      (setq file-name-history orig-file-name-history))))


(ert-deftest difftastic--files:with-lang-override ()
  (mocklet (((get-buffer-create "*difftastic file-A file-B*") => "test-buffer")
            ((difftastic--files-internal "test-buffer"
                                         '("/dir/file-A" . nil)
                                         '("/dir/file-B" . nil)
                                         '("--override=*:test-lang"))))
    (difftastic--files  "/dir/file-A" "/dir/file-B" "test-lang")))

(ert-deftest difftastic--files:with-difftastic-args ()
  (mocklet (((get-buffer-create "*difftastic file-A file-B*") => "test-buffer")
            ((difftastic--files-internal "test-buffer"
                                         '("/dir/file-A" . nil)
                                         '("/dir/file-B" . nil)
                                         '("test-difftastic-args"))))
    (difftastic--files  "/dir/file-A" "/dir/file-B" '("test-difftastic-args"))))


(ert-deftest difftastic-files:basic ()
  (mocklet (((difftastic--files-args) => '("/dir/file-A" "/dir/file-B" "test-lang"))
            ((difftastic--files  "/dir/file-A" "/dir/file-B" "test-lang")))
    (call-interactively #'difftastic-files)))

(ert-deftest difftastic--files:double-prefix ()
  (let ((current-prefix-arg '(16)))
    (mocklet (((difftastic--files-args) => '("/dir/file-A" "/dir/file-B" "test-lang"))
              ((difftastic--with-extra-arguments "test-lang"
                                                 #'difftastic--files
                                                 "/dir/file-A"
                                                 "/dir/file-B")))
      (call-interactively #'difftastic-files))))


;; LocalWords: README el

(provide 'difftastic.t)

;;; difftastic.t.el ends here
