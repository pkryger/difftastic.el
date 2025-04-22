;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when-let* ((dir (file-name-directory (or load-file-name
                                          byte-compile-current-file
                                          buffer-file-name))))
  (load-file (file-name-concat dir "undercover-init.el")))

(require 'difftastic)
(require 'el-mock)
(require 'org)
(require 'ox-ascii)

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
        elisp-buffer c++-buffer org-buffer)
    (with-temp-buffer
      (emacs-lisp-mode)
      (setq elisp-buffer (current-buffer))
      (with-temp-buffer
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
        (with-temp-buffer
          (org-mode)
          (setq org-buffer (current-buffer))

          (should
           (string= "Emacs Lisp"
                    (difftastic--make-suggestion languages
                                                 elisp-buffer c++-buffer)))
          (should
           (string= "Emacs Lisp"
                    (difftastic--make-suggestion languages
                                                 elisp-buffer org-buffer)))
          (should
           (string= "Emacs Lisp"
                    (difftastic--make-suggestion languages
                                                 org-buffer elisp-buffer)))
          (should
           (string= "C++"
                    (difftastic--make-suggestion languages
                                                 c++-buffer org-buffer)))
          (should
           (string= "C++"
                    (difftastic--make-suggestion languages
                                                 c++-buffer elisp-buffer)))
          (should
           (string= "C++"
                    (difftastic--make-suggestion languages
                                                 org-buffer c++-buffer)))
          (should-not
           (difftastic--make-suggestion languages
                                        org-buffer org-buffer)))))))

(ert-deftest difftastic--transform-diff-arguments:basic ()
  (should (equal '(("--ignore-submodules=test-submodule")
                   ("--context 1" "--context 2"))
                 (difftastic--transform-diff-arguments
                  '("--stat" "--no-ext-diff"
                    "-U1" "--unified=2"
                    "-M" "-M3" "--find-renames" "--find-renames=4"
                    "--ignore-submodules=test-submodule")))))

(ert-deftest difftastic--get-file:buffer-visiting-file-no-temporary-created ()
  (let (temp-file)
    (unwind-protect
        (with-temp-buffer
          (setq temp-file (make-temp-file "difftastic.t"))
          (write-region (point-min) (point-max) temp-file nil t)
          (should (equal `(,(buffer-file-name) . nil)
                         (difftastic--get-file-buf "test" (current-buffer)))))

      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest difftastic--get-file:buffer-not-visiting-file-temporary-created ()
  (let (file-buf)
    (unwind-protect
        (with-temp-buffer
          (setq file-buf (difftastic--get-file-buf "test" (current-buffer)))
          (should (consp file-buf))
          (should (string-match-p
                   (eval '(rx string-start
                              (literal temporary-file-directory)
                              "difftastic-test-"
                              (one-or-more (not "/"))))
                   (car file-buf)))
          (should (equal (current-buffer) (cdr file-buf))))

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

(ert-deftest difftastic--build-git-process-environment:without-difftastic-args ()
  (should (equal
           (format "GIT_EXTERNAL_DIFF=%s --color always --width 42 --background %s"
                   difftastic-executable
                   (frame-parameter nil 'background-mode))
           (car (difftastic--build-git-process-environment 42)))))

(ert-deftest difftastic--build-git-process-environment:with-difftastic-args ()
  (should (equal
           (format
            "GIT_EXTERNAL_DIFF=%s --color always --width 42 --background %s --override *:C++"
            difftastic-executable
            (frame-parameter nil 'background-mode))
           (car
            (difftastic--build-git-process-environment
             42
             '("--override" "*:C++"))))))

(ert-deftest difftastic--build-files-command:without-lang-override ()
  (should (equal
           `(,difftastic-executable
             "--color" "always"
             "--width" "42"
             "--background" ,(format "%s" (frame-parameter nil 'background-mode))
             "test-file-A" "test-file-B")
           (difftastic--build-files-command (cons "test-file-A" nil)
                                            (cons "test-file-B" nil)
                                            42))))

(ert-deftest difftastic--build-files-command:with-lang-override ()
  (should (equal
           `(,difftastic-executable
             "--color" "always"
             "--width" "42"
             "--background" ,(format "%s" (frame-parameter nil 'background-mode))
             "--override" "*:test-language"
             "test-file-A" "test-file-B")
           (difftastic--build-files-command (cons "test-file-A" nil)
                                            (cons "test-file-B" nil)
                                            42
                                            "test-language"))))

(ert-deftest difftastic--rerun-file-buf:non-temporary-no-temporary-created ()
  (let (file-buf)
    (unwind-protect
        (let* ((rerun-alist '((file-buf-test . ("test-file" . nil))))
               (orig-rerun-alist (copy-tree rerun-alist)))
          (setq file-buf
                (difftastic--rerun-file-buf
                 "test"
                 (alist-get 'file-buf-test rerun-alist)
                 rerun-alist))
          (should (equal (alist-get 'file-buf-test orig-rerun-alist)
                         file-buf))
          (should (equal orig-rerun-alist rerun-alist)))

      (when (and (cdr file-buf) (file-exists-p (car file-buf)))
        (delete-file (car file-buf))))))

(ert-deftest difftastic--rerun-file-buf:temporary-live-buffer-new-temporary-created ()
  (let (file-buf)
    (unwind-protect
        (let ((rerun-alist '((file-buf-test . ("test-file" . t)))))
          (with-temp-buffer
            (setq file-buf
                  (difftastic--rerun-file-buf
                   "test"
                   (cons "test-file" (current-buffer))
                   rerun-alist))
            (should-not (equal "test-file" (car file-buf)))
            (should (file-exists-p (car file-buf)))
            (should (equal file-buf
                           (alist-get 'file-buf-test rerun-alist)))))

      (when (and (cdr file-buf) (file-exists-p (car file-buf)))
        (delete-file (car file-buf))))))

(ert-deftest difftastic--rerun-file-buf:temporary-non-live-buffer-error-signaled ()
  (let (file-buf)
    (unwind-protect
        (let ((text-quoting-style 'straight)
              buffer rerun-alist orig-rerun-alist)
          (with-temp-buffer
            (setq buffer (current-buffer)))

          (setq rerun-alist `((file-buf-test . ("test-file" . ,buffer)))
                orig-rerun-alist (copy-tree rerun-alist))
          (let ((data (cadr
                       (should-error
                        (setq file-buf
                              (difftastic--rerun-file-buf
                               "test"
                               (alist-get 'file-buf-test rerun-alist)
                               rerun-alist))
                        :type 'user-error))))
            (should
             (equal data
                    "Buffer test [#<killed buffer>] doesn't exist anymore")))

          (should (equal orig-rerun-alist rerun-alist)))

      (when (and (cdr file-buf) (file-exists-p (car file-buf)))
        (delete-file (car file-buf))))))

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
              (with-temp-buffer
                (difftastic-mode)
                (should-not difftastic--chunk-regexp-chunk)
                (should-not difftastic--chunk-regexp-file)
                (should (string-match (difftastic--chunk-regexp t) header))
                (cond
                 ((not chunk)
                  (should-not (match-string 1 header)))
                 ((string= "1/2" chunk)
                  (should (string-equal "1" (match-string 1 header))))
                 ((string= "2/2" chunk)
                  (should (string-equal "2" (match-string 1 header))))
                 ((string= "3/30" chunk)
                  (should (string-equal "3" (match-string 1 header))))
                 ((string= "40/40" chunk)
                  (should (string-equal "40" (match-string 1 header)))))
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
              (with-temp-buffer
                (difftastic-mode)
                (should-not difftastic--chunk-regexp-chunk)
                (should-not difftastic--chunk-regexp-file)
                (should (string-match (difftastic--chunk-regexp nil) header))
                (should-not (match-string 1 header))
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
              (with-temp-buffer
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
              (with-temp-buffer
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
              (with-temp-buffer
                (difftastic-mode)
                (should (string-match (difftastic--chunk-regexp t) header))
                (should-not (match-string 1 header))
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
    (with-temp-buffer
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
    (with-temp-buffer
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
    (with-temp-buffer
      (difftastic-mode)
      (let ((data (cadr
                   (should-error (difftastic-next-chunk)
                                 :type 'user-error))))
        (should (equal data "No more chunks"))))))

(ert-deftest difftastic-next-chunk:last-chunk-error-signaled ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (with-temp-buffer
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
    (with-temp-buffer
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
    (with-temp-buffer
      (difftastic-mode)
      (let ((data (cadr (should-error (difftastic-next-file)
                                      :type 'user-error))))
        (should (equal data "No more files"))))))

(ert-deftest difftastic-next-file:last-chunk-error-signaled ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (with-temp-buffer
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
    (with-temp-buffer
      (difftastic-mode)
      (let ((data (cadr (should-error (difftastic-previous-chunk)
                                      :type 'user-error))))
        (should (equal data "No more chunks"))))))

(ert-deftest difftastic-previous-chunk:first-chunk-error-signaled ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (with-temp-buffer
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
    (with-temp-buffer
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
    (with-temp-buffer
      (difftastic-mode)
      (let ((data (cadr (should-error (difftastic-previous-file)
                                      :type 'user-error))))
        (should (equal data "No more files"))))))

(ert-deftest difftastic-previous-file:first-file-error-signaled ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (with-temp-buffer
      (insert "difftastic.el --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-")
      (difftastic-mode)
      (goto-char (point-min))
      (let ((data (cadr (should-error (difftastic-previous-file)
                                      :type 'user-error))))
        (should (equal data "No more files")))
      (should (equal (point-min) (point))))))

(ert-deftest difftastic-hide-chunk:chunk-hidden ()
  (let ((expected
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
     `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
        (with-temp-buffer
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
  (let ((expected
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
     `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
        (with-temp-buffer
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
  (let ((expected
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
     `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
        (with-temp-buffer
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
  (let ((expected
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
    (eval `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
             (with-temp-buffer
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
  (let ((expected
         (concat (propertize "difftastic.el --- 1/2 --- Emacs Lisp"
                             'difftastic '(:hidden :file))
                 (propertize "
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/2 --- Emacs Lisp
24 ;;; Commentary:"
                             'invisible 'difftastic))))
    (eval `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
             (with-temp-buffer
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
  (let ((expected
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
    (eval `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
             (with-temp-buffer
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
  (let ((expected
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
    (eval `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
             (with-temp-buffer
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
  (let ((expected "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/2 --- Emacs Lisp
24 ;;; Commentary:

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-"))
    (eval
     `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
        (with-temp-buffer
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
  (let ((expected "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/2 --- Emacs Lisp
24 ;;; Commentary:"))
    (eval
     `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
        (with-temp-buffer
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
  (let ((expected
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
     `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
        (with-temp-buffer
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
  (let ((expected "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/2 --- Emacs Lisp
24 ;;; Commentary:

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-"))
    (eval
     `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
        (with-temp-buffer
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
  (let ((expected "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/2 --- Emacs Lisp
24 ;;; Commentary:"))
    (eval
     `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
        (with-temp-buffer
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
  (let ((expected "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/2 --- Emacs Lisp
24 ;;; Commentary:

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-"))
    (eval
     `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
        (with-temp-buffer
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
  (let ((expected
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
     `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
        (with-temp-buffer
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
  (let ((expected
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
     `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
        (with-temp-buffer
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
  (let ((expected
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
    (eval `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
             (with-temp-buffer
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
  (let ((expected
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
    (eval `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
             (with-temp-buffer
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
  (let ((expected "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/2 --- Emacs Lisp
24 ;;; Commentary:

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-"))
    (eval
     `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
        (with-temp-buffer
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
  (let ((expected
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
     `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
        (with-temp-buffer
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
  (let ((expected "difftastic.el --- 1/2 --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

difftastic.el --- 2/2 --- Emacs Lisp
24 ;;; Commentary:

test/difftastic.t.el --- Emacs Lisp
1 ;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-"))
    (eval
     `(mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
        (with-temp-buffer
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

(ert-deftest difftastic--get-languages:parse-output ()
  (let ((file "difft--list-languages.out")
        out)
    (should (or (file-exists-p file)
                (file-exists-p (format "test/%s" file))))
    (setq out (if (file-exists-p file)
                  file
                (format "test/%s" file)))
    (eval
     `(mocklet ((shell-command-to-string => (with-temp-buffer
                                              (insert-file-contents ,out)
                                              (buffer-string))))
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

;; When running in noninteractive batch mode there are no faces defined, ergo:
;; no colors.  See this discussion:
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2024-02/msg00095.html For
;; this reason only a couple sanity tests are run in CI.

(ert-deftest difftastic--run-command-filter:file-ansi-colors-applied ()
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
    (with-temp-buffer
      (eval
       `(mocklet ((process-buffer => ,(current-buffer)))
          (difftastic--run-command-filter
           'test-process
           "[1m[33mdifftastic.el[39m[0m[2m --- 1/2 --- Emacs Lisp[0m")
          (if (version< "29" emacs-version) ;; since Emacs-29
              (should
               (equal-including-properties (buffer-string) ,expected))
            (should
             (ert-equal-including-properties (buffer-string) ,expected))))))))

(ert-deftest difftastic--run-command-filter:chunk-ansi-colors-applied ()
  (let ((expected
         (concat
          (propertize
           "difftastic.el"
           'font-lock-face 'ansi-color-bold)
          (propertize
           " --- 2/2 --- Emacs Lisp"
           'font-lock-face 'ansi-color-faint))))
    (with-temp-buffer
      (eval
       `(mocklet ((process-buffer => ,(current-buffer)))
          (difftastic--run-command-filter
           'test-process
           "[1mdifftastic.el[0m[2m --- 2/2 --- Emacs Lisp[0m")
          (should
           (equal-including-properties
            (buffer-string)
            ,expected)))))))

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

(ert-deftest difftastic--run-command-filter:removed-ansi-colors-applied ()
  (with-temp-buffer
    (eval
     '(mocklet ((process-buffer => (current-buffer)))
        (difftastic--run-command-filter
         'test-process
         "[31mremoved[0m")
        (if (version< "29" emacs-version) ;; since Emacs-29
            (should
             (equal-including-properties
              (buffer-string)
              (propertize "removed"
                          'font-lock-face
                          `((:background ,difftastic-t-removed-bg)
                            (:foreground ,difftastic-t-removed-fg)))))
          (should
           (ert-equal-including-properties
            (buffer-string)
            (propertize "removed"
                        'font-lock-face
                        `((:background ,difftastic-t-removed-bg)
                          (:foreground ,difftastic-t-removed-fg))))))))))

(ert-deftest difftastic--run-command-filter:removed-bold-ansi-colors-applied ()
  (with-temp-buffer
    (eval
     '(mocklet ((process-buffer => (current-buffer)))
        (difftastic--run-command-filter
         'test-process
         "[31;1mremoved[0m")
        (if (version< "29" emacs-version) ;; since Emacs-29
            (should
             (equal-including-properties
              (buffer-string)
              (propertize "removed"
                          'font-lock-face
                          `((:background ,difftastic-t-removed-bg)
                            ansi-color-bold
                            (:foreground ,difftastic-t-removed-fg)))))
          (should
           (ert-equal-including-properties
            (buffer-string)
            (propertize "removed"
                        'font-lock-face
                        `(ansi-color-bold
                          ((:background ,difftastic-t-removed-bg)
                           (:foreground ,difftastic-t-removed-fg)))))))))))

(ert-deftest difftastic--run-command-filter:removed-italic-ansi-colors-applied ()
  (with-temp-buffer
    (eval
     '(mocklet ((process-buffer => (current-buffer)))
        (difftastic--run-command-filter
         'test-process
         "[31;3mremoved[0m")
        (if (version< "29" emacs-version) ;; since Emacs-29
            (should
             (equal-including-properties
              (buffer-string)
              (propertize "removed"
                          'font-lock-face
                          `((:background ,difftastic-t-removed-bg)
                            ansi-color-italic
                            (:foreground ,difftastic-t-removed-fg)))))
          (should
           (ert-equal-including-properties
            (buffer-string)
            (propertize "removed"
                        'font-lock-face
                        `(ansi-color-italic
                          ((:background ,difftastic-t-removed-bg)
                           (:foreground ,difftastic-t-removed-fg)))))))))))

(ert-deftest difftastic--run-command-filter:removed-bold-italic-ansi-colors-applied ()
  (with-temp-buffer
    (eval
     '(mocklet ((process-buffer => (current-buffer)))
        (difftastic--run-command-filter
         'test-process
         "[31;1;3mremoved[0m")
        (if (version< "29" emacs-version) ;; since Emacs-29
            (should
             (equal-including-properties
              (buffer-string)
              (propertize "removed"
                          'font-lock-face
                          `((:background ,difftastic-t-removed-bg)
                            ansi-color-bold
                            ansi-color-italic
                            (:foreground ,difftastic-t-removed-fg)))))
          (should
           (ert-equal-including-properties
            (buffer-string)
            (propertize "removed"
                        'font-lock-face
                        `(ansi-color-bold
                          ansi-color-italic
                          ((:background ,difftastic-t-removed-bg)
                           (:foreground ,difftastic-t-removed-fg)))))))))))

(ert-deftest difftastic--run-command-filter:added-ansi-colors-applied ()
  (with-temp-buffer
    (eval
     '(mocklet ((process-buffer => (current-buffer)))
        (difftastic--run-command-filter
         'test-process
         "[32madded[0m")
        (if (version< "29" emacs-version) ;; since Emacs-29
            (should
             (equal-including-properties
              (buffer-string)
              (propertize "added"
                          'font-lock-face
                          `((:background ,difftastic-t-added-bg)
                            (:foreground ,difftastic-t-added-fg)))))
          (should
           (ert-equal-including-properties
            (buffer-string)
            (propertize "added"
                        'font-lock-face
                        `((:background ,difftastic-t-added-bg)
                          (:foreground ,difftastic-t-added-fg))))))))))

(ert-deftest difftastic--run-command-filter:added-bold-ansi-colors-applied ()
  (with-temp-buffer
    (eval
     '(mocklet ((process-buffer => (current-buffer)))
        (difftastic--run-command-filter
         'test-process
         "[32;1madded[0m")
        (if (version< "29" emacs-version) ;; since Emacs-29
            (should
             (equal-including-properties
              (buffer-string)
              (propertize "added"
                          'font-lock-face
                          `((:background ,difftastic-t-added-bg)
                            ansi-color-bold
                            (:foreground ,difftastic-t-added-fg)))))
          (should
           (ert-equal-including-properties
            (buffer-string)
            (propertize "added"
                        'font-lock-face
                        `(ansi-color-bold
                          ((:background ,difftastic-t-added-bg)
                           (:foreground ,difftastic-t-added-fg)))))))))))

(ert-deftest difftastic--run-command-filter:added-italic-ansi-colors-applied ()
  (with-temp-buffer
    (eval
     '(mocklet ((process-buffer => (current-buffer)))
        (difftastic--run-command-filter
         'test-process
         "[32;3madded[0m")
        (if (version< "29" emacs-version) ;; since Emacs-29
            (should
             (equal-including-properties
              (buffer-string)
              (propertize "added"
                          'font-lock-face
                          `((:background ,difftastic-t-added-bg)
                            ansi-color-italic
                            (:foreground ,difftastic-t-added-fg)))))
          (should
           (ert-equal-including-properties
            (buffer-string)
            (propertize "added"
                        'font-lock-face
                        `(ansi-color-italic
                          ((:background ,difftastic-t-added-bg)
                           (:foreground ,difftastic-t-added-fg)))))))))))

(ert-deftest difftastic--run-command-filter:added-bold-italic-ansi-colors-applied ()
  (with-temp-buffer
    (eval
     '(mocklet ((process-buffer => (current-buffer)))
        (difftastic--run-command-filter
         'test-process
         "[32;1;3madded[0m")
        (if (version< "29" emacs-version) ;; since Emacs-29
            (should
             (equal-including-properties
              (buffer-string)
              (propertize "added"
                          'font-lock-face
                          `((:background ,difftastic-t-added-bg)
                            ansi-color-bold
                            ansi-color-italic
                            (:foreground ,difftastic-t-added-fg)))))
          (should
           (ert-equal-including-properties
            (buffer-string)
            (propertize "added"
                        'font-lock-face
                        `(ansi-color-bold
                          ansi-color-italic
                          ((:background ,difftastic-t-added-bg)
                           (:foreground ,difftastic-t-added-fg)))))))))))

(ert-deftest difftastic--run-command-filter:removed-highlight-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     :failed)
  (with-temp-buffer
    (eval
     '(mocklet ((process-buffer => (current-buffer)))
        (difftastic--run-command-filter
         'test-process
         "[31;4mremoved[0m")
        (if (version< "29" emacs-version) ;; since Emacs-29
            (should
             (equal-including-properties
              (buffer-string)
              (propertize "removed"
                          'font-lock-face
                          `((:background ,difftastic-t-removed-highlight-bg)
                            (:foreground ,difftastic-t-removed-highlight-fg)))))
          (should
           (ert-equal-including-properties
            (buffer-string)
            (propertize "removed"
                        'font-lock-face
                        `((:background ,difftastic-t-removed-highlight-bg)
                          (:foreground ,difftastic-t-removed-highlight-fg))))))))))

(ert-deftest difftastic--run-command-filter:removed-highlight-bold-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     :failed)
  (with-temp-buffer
    (eval
     '(mocklet ((process-buffer => (current-buffer)))
        (difftastic--run-command-filter
         'test-process
         "[31;1;4mremoved[0m")
        (if (version< "29" emacs-version) ;; since Emacs-29
            (should
             (equal-including-properties
              (buffer-string)
              (propertize "removed"
                          'font-lock-face
                          `((:background ,difftastic-t-removed-highlight-bg)
                            (:foreground ,difftastic-t-removed-highlight-fg)))))
          (should
           (ert-equal-including-properties
            (buffer-string)
            (propertize "removed"
                        'font-lock-face
                        `((:background ,difftastic-t-removed-highlight-bg)
                          (:foreground ,difftastic-t-removed-highlight-fg))))))))))

(ert-deftest difftastic--run-command-filter:removed-highlight-italic-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     :failed)
  (with-temp-buffer
    (eval
     '(mocklet ((process-buffer => (current-buffer)))
        (difftastic--run-command-filter
         'test-process
         "[31;3;4mremoved[0m")
        (if (version< "29" emacs-version) ;; since Emacs-29
            (should
             (equal-including-properties
              (buffer-string)
              (propertize "removed"
                          'font-lock-face
                          `((:background ,difftastic-t-removed-highlight-bg)
                            (:foreground ,difftastic-t-removed-highlight-fg)
                            ansi-color-italic))))
          (should
           (ert-equal-including-properties
            (buffer-string)
            (propertize "removed"
                        'font-lock-face
                        `(ansi-color-italic
                          ((:background ,difftastic-t-removed-highlight-bg)
                           (:foreground ,difftastic-t-removed-highlight-fg)))))))))))

(ert-deftest difftastic--run-command-filter:removed-highlight-bold-italic-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     :failed)
  (with-temp-buffer
    (eval
     '(mocklet ((process-buffer => (current-buffer)))
        (difftastic--run-command-filter
         'test-process
         "[31;1;3;4mremoved[0m")
        (if (version< "29" emacs-version) ;; since Emacs-29
            (should
             (equal-including-properties
              (buffer-string)
              (propertize "removed"
                          'font-lock-face
                          `((:background ,difftastic-t-removed-highlight-bg)
                            (:foreground ,difftastic-t-removed-highlight-fg)
                            ansi-color-italic))))
          (should
           (ert-equal-including-properties
            (buffer-string)
            (propertize "removed"
                        'font-lock-face
                        `(ansi-color-italic
                          ((:background ,difftastic-t-removed-highlight-bg)
                           (:foreground ,difftastic-t-removed-highlight-fg)))))))))))

(ert-deftest difftastic--run-command-filter:removed-highlight-no-strip-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     :failed)
  (let (difftastic-highlight-strip-face-properties)
    (with-temp-buffer
      (eval
       '(mocklet ((process-buffer => (current-buffer)))
          (difftastic--run-command-filter
           'test-process
           "[31;1;2;3;4mremoved[0m")
          (if (version< "29" emacs-version) ;; since Emacs-29
              (should
               (equal-including-properties
                (buffer-string)
                (propertize "removed"
                            'font-lock-face
                            `((:background ,difftastic-t-removed-highlight-bg)
                              (:foreground ,difftastic-t-removed-highlight-fg)
                              ansi-color-bold
                              ansi-color-faint
                              ansi-color-italic
                              ansi-color-underline))))
            (should
             (ert-equal-including-properties
              (buffer-string)
              (propertize "removed"
                          'font-lock-face
                          `(ansi-color-bold
                            ansi-color-faint
                            ansi-color-italic
                            ansi-color-underline
                            ((:background ,difftastic-t-removed-highlight-bg)
                             (:foreground ,difftastic-t-removed-highlight-fg))))))))))))

(ert-deftest difftastic--run-command-filter:removed-highlight-strip-all-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     :failed)
  (let ((difftastic-highlight-strip-face-properties '(:bold :faint :italic :underline)))
    (with-temp-buffer
      (eval
       '(mocklet ((process-buffer => (current-buffer)))
          (difftastic--run-command-filter
           'test-process
           "[31;1;2;3;4mremoved[0m")
          (if (version< "29" emacs-version) ;; since Emacs-29
              (should
               (equal-including-properties
                (buffer-string)
                (propertize "removed"
                            'font-lock-face
                            `((:background ,difftastic-t-removed-highlight-bg)
                              (:foreground ,difftastic-t-removed-highlight-fg)))))
            (should
             (ert-equal-including-properties
              (buffer-string)
              (propertize "removed"
                          'font-lock-face
                          `((:background ,difftastic-t-removed-highlight-bg)
                            (:foreground ,difftastic-t-removed-highlight-fg)))))))))))

(ert-deftest difftastic--run-command-filter:added-highlight-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     :failed)
  (with-temp-buffer
    (eval
     '(mocklet ((process-buffer => (current-buffer)))
        (difftastic--run-command-filter
         'test-process
         "[32;4madded[0m")
        (if (version< "29" emacs-version) ;; since Emacs-29
            (should
             (equal-including-properties
              (buffer-string)
              (propertize "added"
                          'font-lock-face
                          `((:background ,difftastic-t-added-highlight-bg)
                            (:foreground ,difftastic-t-added-highlight-fg)))))
          (should
           (ert-equal-including-properties
            (buffer-string)
            (propertize "added"
                        'font-lock-face
                        `((:background ,difftastic-t-added-highlight-bg)
                          (:foreground ,difftastic-t-added-highlight-fg))))))))))

(ert-deftest difftastic--run-command-filter:added-highlight-bold-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     :failed)
  (with-temp-buffer
    (eval
     '(mocklet ((process-buffer => (current-buffer)))
        (difftastic--run-command-filter
         'test-process
         "[32;1;4madded[0m")
        (if (version< "29" emacs-version) ;; since Emacs-29
            (should
             (equal-including-properties
              (buffer-string)
              (propertize "added"
                          'font-lock-face
                          `((:background ,difftastic-t-added-highlight-bg)
                            (:foreground ,difftastic-t-added-highlight-fg)))))
          (should
           (ert-equal-including-properties
            (buffer-string)
            (propertize "added"
                        'font-lock-face
                        `((:background ,difftastic-t-added-highlight-bg)
                          (:foreground ,difftastic-t-added-highlight-fg))))))))))

(ert-deftest difftastic--run-command-filter:added-highlight-italic-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     :failed)
  (with-temp-buffer
    (eval
     '(mocklet ((process-buffer => (current-buffer)))
        (difftastic--run-command-filter
         'test-process
         "[32;3;4madded[0m")
        (if (version< "29" emacs-version) ;; since Emacs-29
            (should
             (equal-including-properties
              (buffer-string)
              (propertize "added"
                          'font-lock-face
                          `((:background ,difftastic-t-added-highlight-bg)
                            (:foreground ,difftastic-t-added-highlight-fg)
                            ansi-color-italic))))
          (should
           (ert-equal-including-properties
            (buffer-string)
            (propertize "added"
                        'font-lock-face
                        `(ansi-color-italic
                          ((:background ,difftastic-t-added-highlight-bg)
                           (:foreground ,difftastic-t-added-highlight-fg)))))))))))

(ert-deftest difftastic--run-command-filter:added-highlight-bold-italic-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     :failed)
  (with-temp-buffer
    (eval
     '(mocklet ((process-buffer => (current-buffer)))
        (difftastic--run-command-filter
         'test-process
         "[32;1;3;4madded[0m")
        (if (version< "29" emacs-version) ;; since Emacs-29
            (should
             (equal-including-properties
              (buffer-string)
              (propertize "added"
                          'font-lock-face
                          `((:background ,difftastic-t-added-highlight-bg)
                            (:foreground ,difftastic-t-added-highlight-fg)
                            ansi-color-italic))))
          (should
           (ert-equal-including-properties
            (buffer-string)
            (propertize "added"
                        'font-lock-face
                        `(ansi-color-italic
                          ((:background ,difftastic-t-added-highlight-bg)
                           (:foreground ,difftastic-t-added-highlight-fg)))))))))))

(ert-deftest difftastic--run-command-filter:added-highlight-no-strip-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     :failed)
  (let (difftastic-highlight-strip-face-properties)
    (with-temp-buffer
      (eval
       '(mocklet ((process-buffer => (current-buffer)))
          (difftastic--run-command-filter
           'test-process
           "[32;1;2;3;4madded[0m")
          (if (version< "29" emacs-version) ;; since Emacs-29
              (should
               (equal-including-properties
                (buffer-string)
                (propertize "added"
                            'font-lock-face
                            `((:background ,difftastic-t-added-highlight-bg)
                              (:foreground ,difftastic-t-added-highlight-fg)
                              ansi-color-bold
                              ansi-color-faint
                              ansi-color-italic
                              ansi-color-underline))))
            (should
             (ert-equal-including-properties
              (buffer-string)
              (propertize "added"
                          'font-lock-face
                          `(ansi-color-bold
                            ansi-color-faint
                            ansi-color-italic
                            ansi-color-underline
                            ((:background ,difftastic-t-added-highlight-bg)
                             (:foreground ,difftastic-t-added-highlight-fg))))))))))))

(ert-deftest difftastic--run-command-filter:added-highlight-strip-all-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     :failed)
  (let ((difftastic-highlight-strip-face-properties '(:bold :faint :italic :underline)))
    (with-temp-buffer
      (eval
       '(mocklet ((process-buffer => (current-buffer)))
          (difftastic--run-command-filter
           'test-process
           "[32;1;2;3;4madded[0m")
          (if (version< "29" emacs-version) ;; since Emacs-29
              (should
               (equal-including-properties
                (buffer-string)
                (propertize "added"
                            'font-lock-face
                            `((:background ,difftastic-t-added-highlight-bg)
                              (:foreground ,difftastic-t-added-highlight-fg)))))
            (should
             (ert-equal-including-properties
              (buffer-string)
              (propertize "added"
                          'font-lock-face
                          `((:background ,difftastic-t-added-highlight-bg)
                            (:foreground ,difftastic-t-added-highlight-fg)))))))))))

(ert-deftest difftastic--run-command-filter:removed-no-highlight-ansi-colors-applied ()
  (let (difftastic-highlight-alist)
    (with-temp-buffer
      (eval
       '(mocklet ((process-buffer => (current-buffer)))
          (difftastic--run-command-filter
           'test-process
           "[31;4mremoved[0m")
          (if (version< "29" emacs-version) ;; since Emacs-29
              (should
               (equal-including-properties
                (buffer-string)
                (propertize "removed"
                            'font-lock-face
                            `((:background ,difftastic-t-removed-bg)
                              ansi-color-underline
                              (:foreground ,difftastic-t-removed-fg)))))
            (should
             (ert-equal-including-properties
              (buffer-string)
              (propertize "removed"
                          'font-lock-face
                          `(ansi-color-underline
                            ((:background ,difftastic-t-removed-bg)
                             (:foreground ,difftastic-t-removed-fg))))))))))))

(ert-deftest difftastic--run-command-filter:removed-no-highlight-bold-ansi-colors-applied ()
  (let (difftastic-highlight-alist)
    (with-temp-buffer
      (eval
       '(mocklet ((process-buffer => (current-buffer)))
          (difftastic--run-command-filter
           'test-process
           "[31;1;4mremoved[0m")
          (if (version< "29" emacs-version) ;; since Emacs-29
              (should
               (equal-including-properties
                (buffer-string)
                (propertize "removed"
                            'font-lock-face
                            `((:background ,difftastic-t-removed-bg)
                              ansi-color-bold
                              ansi-color-underline
                              (:foreground ,difftastic-t-removed-fg)))))
            (should
             (ert-equal-including-properties
              (buffer-string)
              (propertize "removed"
                          'font-lock-face
                          `(ansi-color-bold
                            ansi-color-underline
                            ((:background ,difftastic-t-removed-bg)
                             (:foreground ,difftastic-t-removed-fg))))))))))))

(ert-deftest difftastic--run-command-filter:removed-no-highlight-italic-ansi-colors-applied ()
  (let (difftastic-highlight-alist)
    (with-temp-buffer
      (eval
       '(mocklet ((process-buffer => (current-buffer)))
          (difftastic--run-command-filter
           'test-process
           "[31;3;4mremoved[0m")
          (if (version< "29" emacs-version) ;; since Emacs-29
              (should
               (equal-including-properties
                (buffer-string)
                (propertize "removed"
                            'font-lock-face
                            `((:background ,difftastic-t-removed-bg)
                              ansi-color-italic
                              ansi-color-underline
                              (:foreground ,difftastic-t-removed-fg)))))
            (should
             (ert-equal-including-properties
              (buffer-string)
              (propertize "removed"
                          'font-lock-face
                          `(ansi-color-italic
                            ansi-color-underline
                            ((:background ,difftastic-t-removed-bg)
                             (:foreground ,difftastic-t-removed-fg))))))))))))

(ert-deftest difftastic--run-command-filter:removed-no-highlight-bold-italic-ansi-colors-applied ()
  (let (difftastic-highlight-alist)
    (with-temp-buffer
      (eval
       '(mocklet ((process-buffer => (current-buffer)))
          (difftastic--run-command-filter
           'test-process
           "[31;1;3;4mremoved[0m")
          (if (version< "29" emacs-version) ;; since Emacs-29
              (should
               (equal-including-properties
                (buffer-string)
                (propertize "removed"
                            'font-lock-face
                            `((:background ,difftastic-t-removed-bg)
                              ansi-color-bold
                              ansi-color-italic
                              ansi-color-underline
                              (:foreground ,difftastic-t-removed-fg)))))
            (should
             (ert-equal-including-properties
              (buffer-string)
              (propertize "removed"
                          'font-lock-face
                          `(ansi-color-bold
                            ansi-color-italic
                            ansi-color-underline
                            ((:background ,difftastic-t-removed-bg)
                             (:foreground ,difftastic-t-removed-fg))))))))))))

(ert-deftest difftastic--run-command-filter:added-no-highlight-ansi-colors-applied ()
  (let (difftastic-highlight-alist)
    (with-temp-buffer
      (eval
       '(mocklet ((process-buffer => (current-buffer)))
          (difftastic--run-command-filter
           'test-process
           "[32;4madded[0m")
          (if (version< "29" emacs-version) ;; since Emacs-29
              (should
               (equal-including-properties
                (buffer-string)
                (propertize "added"
                            'font-lock-face
                            `((:background ,difftastic-t-added-bg)
                              ansi-color-underline
                              (:foreground ,difftastic-t-added-fg)))))
            (should
             (ert-equal-including-properties
              (buffer-string)
              (propertize "added"
                          'font-lock-face
                          `(ansi-color-underline
                            ((:background ,difftastic-t-added-bg)
                             (:foreground ,difftastic-t-added-fg))))))))))))

(ert-deftest difftastic--run-command-filter:added-no-highlight-bold-ansi-colors-applied ()
  (let (difftastic-highlight-alist)
    (with-temp-buffer
      (eval
       '(mocklet ((process-buffer => (current-buffer)))
          (difftastic--run-command-filter
           'test-process
           "[32;1;4madded[0m")
          (if (version< "29" emacs-version) ;; since Emacs-29
              (should
               (equal-including-properties
                (buffer-string)
                (propertize "added"
                            'font-lock-face
                            `((:background ,difftastic-t-added-bg)
                              ansi-color-bold
                              ansi-color-underline
                              (:foreground ,difftastic-t-added-fg)))))
            (should
             (ert-equal-including-properties
              (buffer-string)
              (propertize "added"
                          'font-lock-face
                          `(ansi-color-bold
                            ansi-color-underline
                            ((:background ,difftastic-t-added-bg)
                             (:foreground ,difftastic-t-added-fg))))))))))))

(ert-deftest difftastic--run-command-filter:added-no-highlight-italic-ansi-colors-applied ()
  (let (difftastic-highlight-alist)
    (with-temp-buffer
      (eval
       '(mocklet ((process-buffer => (current-buffer)))
          (difftastic--run-command-filter
           'test-process
           "[32;3;4madded[0m")
          (if (version< "29" emacs-version) ;; since Emacs-29
              (should
               (equal-including-properties
                (buffer-string)
                (propertize "added"
                            'font-lock-face
                            `((:background ,difftastic-t-added-bg)
                              ansi-color-italic
                              ansi-color-underline
                              (:foreground ,difftastic-t-added-fg)))))
            (should
             (ert-equal-including-properties
              (buffer-string)
              (propertize "added"
                          'font-lock-face
                          `(ansi-color-italic
                            ansi-color-underline
                            ((:background ,difftastic-t-added-bg)
                             (:foreground ,difftastic-t-added-fg))))))))))))

(ert-deftest difftastic--run-command-filter:added-no-highlight-bold-italic-ansi-colors-applied ()
  (let (difftastic-highlight-alist)
    (with-temp-buffer
      (eval
       '(mocklet ((process-buffer => (current-buffer)))
          (difftastic--run-command-filter
           'test-process
           "[32;1;3;4madded[0m")
          (if (version< "29" emacs-version) ;; since Emacs-29
              (should
               (equal-including-properties
                (buffer-string)
                (propertize "added"
                            'font-lock-face
                            `((:background ,difftastic-t-added-bg)
                              ansi-color-bold
                              ansi-color-italic
                              ansi-color-underline
                              (:foreground ,difftastic-t-added-fg)))))
            (should
             (ert-equal-including-properties
              (buffer-string)
              (propertize "added"
                          'font-lock-face
                          `(ansi-color-bold
                            ansi-color-italic
                            ansi-color-underline
                            ((:background ,difftastic-t-added-bg)
                             (:foreground ,difftastic-t-added-fg))))))))))))

(ert-deftest difftastic--run-command-filter:comment-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     (if noninteractive
                         :passed
                       :failed)) ;; never checked interactively
  (with-temp-buffer
    (eval
     '(mocklet ((process-buffer => (current-buffer)))
        (difftastic--run-command-filter
         'test-process
         "[34mcomment[0m")
        (if (version< "29" emacs-version) ;; since Emacs-29
            (should
             (equal-including-properties
              (buffer-string)
              (propertize "comment"
                          'font-lock-face
                          (if (face-background 'font-lock-comment-face)
                              `((:background ,difftastic-t-comment-bg)
                                (:foreground ,difftastic-t-comment-fg))
                            `(:foreground ,difftastic-t-comment-fg)))))
          (should
           (ert-equal-including-properties
            (buffer-string)
            (propertize "comment"
                        'font-lock-face
                        `((:background ,difftastic-t-comment-bg)
                          (:foreground ,difftastic-t-comment-fg))))))))))

(ert-deftest difftastic--run-command-filter:comment-bold-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     (if noninteractive
                         :passed
                       :failed)) ;; never checked interactively
  (with-temp-buffer
    (eval
     '(mocklet ((process-buffer => (current-buffer)))
        (difftastic--run-command-filter
         'test-process
         "[34;1mcomment[0m")
        (if (version< "29" emacs-version) ;; since Emacs-29
            (should
             (equal-including-properties
              (buffer-string)
              (propertize "comment"
                          'font-lock-face
                          `(,@(when (face-background 'font-lock-comment-face)
                                `((:background ,difftastic-t-comment-bg)))
                            ansi-color-bold
                            (:foreground ,difftastic-t-comment-fg)))))
          (should
           (ert-equal-including-properties
            (buffer-string)
            (propertize "comment"
                        'font-lock-face
                        `(ansi-color-bold
                          ((:background ,difftastic-t-comment-bg)
                           (:foreground ,difftastic-t-comment-fg)))))))))))

(ert-deftest difftastic--run-command-filter:comment-italic-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     (if noninteractive
                         :passed
                       :failed)) ;; never checked interactively
  (with-temp-buffer
    (eval
     '(mocklet ((process-buffer => (current-buffer)))
        (difftastic--run-command-filter
         'test-process
         "[34;3mcomment[0m")
        (if (version< "29" emacs-version) ;; since Emacs-29
            (should
             (equal-including-properties
              (buffer-string)
              (propertize "comment"
                          'font-lock-face
                          `(,@(when (face-background 'font-lock-comment-face)
                                `((:background ,difftastic-t-comment-bg)))
                            ansi-color-italic
                            (:foreground ,difftastic-t-comment-fg)))))
          (should
           (ert-equal-including-properties
            (buffer-string)
            (propertize "comment"
                        'font-lock-face
                        `(ansi-color-italic
                          ((:background ,difftastic-t-comment-bg)
                           (:foreground ,difftastic-t-comment-fg)))))))))))

(ert-deftest difftastic--run-command-filter:comment-bold-italic-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     (if noninteractive
                         :passed
                       :failed)) ;; never checked interactively
  (with-temp-buffer
    (eval
     '(mocklet ((process-buffer => (current-buffer)))
        (difftastic--run-command-filter
         'test-process
         "[34;1;3mcomment[0m")
        (if (version< "29" emacs-version) ;; since Emacs-29
            (should
             (equal-including-properties
              (buffer-string)
              (propertize "comment"
                          'font-lock-face
                          `(,@(when (face-background 'font-lock-comment-face)
                                `((:background ,difftastic-t-comment-bg)))
                            ansi-color-bold
                            ansi-color-italic
                            (:foreground ,difftastic-t-comment-fg)))))
          (should
           (ert-equal-including-properties
            (buffer-string)
            (propertize "comment"
                        'font-lock-face
                        `(ansi-color-bold
                          ansi-color-italic
                          ((:background ,difftastic-t-comment-bg)
                           (:foreground ,difftastic-t-comment-fg)))))))))))

(ert-deftest difftastic--run-command-filter:string-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     (if noninteractive
                         :passed
                       :failed)) ;; never checked interactively
  (with-temp-buffer
    (eval
     '(mocklet ((process-buffer => (current-buffer)))
        (difftastic--run-command-filter
         'test-process
         "[35mstring[0m")
        (if (version< "29" emacs-version) ;; since Emacs-29
            (should
             (equal-including-properties
              (buffer-string)
              (propertize "string"
                          'font-lock-face
                          (if (face-background 'font-lock-string-face)
                              `((:background ,difftastic-t-string-bg)
                                (:foreground ,difftastic-t-string-fg))
                            `(:foreground ,difftastic-t-string-fg)))))
          (should
           (ert-equal-including-properties
            (buffer-string)
            (propertize "string"
                        'font-lock-face
                        `((:background ,difftastic-t-string-bg)
                          (:foreground ,difftastic-t-string-fg))))))))))

(ert-deftest difftastic--run-command-filter:string-bold-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     (if noninteractive
                         :passed
                       :failed)) ;; never checked interactively
  (with-temp-buffer
    (eval
     '(mocklet ((process-buffer => (current-buffer)))
        (difftastic--run-command-filter
         'test-process
         "[35;1mstring[0m")
        (if (version< "29" emacs-version) ;; since Emacs-29
            (should
             (equal-including-properties
              (buffer-string)
              (propertize "string"
                          'font-lock-face
                          `(,@(when (face-background 'font-lock-string-face)
                                `((:background ,difftastic-t-string-bg)))
                            ansi-color-bold
                            (:foreground ,difftastic-t-string-fg)))))
          (should
           (ert-equal-including-properties
            (buffer-string)
            (propertize "string"
                        'font-lock-face
                        `(ansi-color-bold
                          ((:background ,difftastic-t-string-bg)
                           (:foreground ,difftastic-t-string-fg)))))))))))

(ert-deftest difftastic--run-command-filter:string-italic-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     (if noninteractive
                         :passed
                       :failed)) ;; never checked interactively
  (with-temp-buffer
    (eval
     '(mocklet ((process-buffer => (current-buffer)))
        (difftastic--run-command-filter
         'test-process
         "[35;3mstring[0m")
        (if (version< "29" emacs-version) ;; since Emacs-29
            (should
             (equal-including-properties
              (buffer-string)
              (propertize "string"
                          'font-lock-face
                          `(,@(when (face-background 'font-lock-string-face)
                                `((:background ,difftastic-t-string-bg)))
                            ansi-color-italic
                            (:foreground ,difftastic-t-string-fg)))))
          (should
           (ert-equal-including-properties
            (buffer-string)
            (propertize "string"
                        'font-lock-face
                        `(ansi-color-italic
                          ((:background ,difftastic-t-string-bg)
                           (:foreground ,difftastic-t-string-fg)))))))))))

(ert-deftest difftastic--run-command-filter:string-bold-italic-ansi-colors-applied ()
  :expected-result (if (version< "29" emacs-version) ;; since Emacs-29
                       :passed
                     (if noninteractive
                         :passed
                       :failed)) ;; never checked interactively
  (with-temp-buffer
    (eval
     '(mocklet ((process-buffer => (current-buffer)))
        (difftastic--run-command-filter
         'test-process
         "[35;1;3mstring[0m")
        (if (version< "29" emacs-version) ;; since Emacs-29
            (should
             (equal-including-properties
              (buffer-string)
              (propertize "string"
                          'font-lock-face
                          `(,@(when (face-background 'font-lock-string-face)
                                `((:background ,difftastic-t-string-bg)))
                            ansi-color-bold
                            ansi-color-italic
                            (:foreground ,difftastic-t-string-fg)))))
          (should
           (ert-equal-including-properties
            (buffer-string)
            (propertize "string"
                        'font-lock-face
                        `(ansi-color-bold
                          ansi-color-italic
                          ((:background ,difftastic-t-string-bg)
                           (:foreground ,difftastic-t-string-fg)))))))))))

(ert-deftest difftastic--run-command-sentinel:with-output-action-called ()
  (with-temp-buffer
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
  (with-temp-buffer
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
  (with-temp-buffer
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
    (with-temp-buffer
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

(ert-deftest difftastic-rerun-requested-window-width:basic ()
  (eval
   '(mocklet (((window-width) => 160)
              ((fringe-columns *) => 2 :times 2))
      (should (equal (difftastic-rerun-requested-window-width)
                     156)))))

(ert-deftest difftastic-requested-window-width:other-window ()
  (eval
   '(mocklet (((count-windows) => 2)
              ((window-width) => 42)
              ((fringe-columns *) => 2 :times 2))
      (should (equal (difftastic-requested-window-width)
                     38)))))

(ert-deftest difftastic-requested-window-width:within-split-width-threshold ()
  (let ((split-width-threshold 37))
    (eval
     '(mocklet (((count-windows) => 1)
                ((window-width) => 42 :times 2)
                ((fringe-columns *) => 2 :times 4))
        (should (equal (difftastic-requested-window-width)
                       17))))))

(ert-deftest difftastic-requested-window-width:outside-split-width-threshold ()
  (let ((split-width-threshold 38))
    (eval
     '(mocklet (((count-windows) => 1)
                ((window-width) => 42 :times 2)
                ((fringe-columns *) => 2 :times 4))
        (should (equal (difftastic-requested-window-width)
                       38))))))

(ert-deftest difftastic-requested-window-width:nil-split-width-threshold ()
  (let (split-width-threshold)
    (eval
     '(mocklet (((count-windows) => 1)
                ((window-width) => 42)
                ((fringe-columns *) => 2 :times 2))
        (should (equal (difftastic-requested-window-width)
                       38))))))

(ert-deftest difftastic-pop-to-buffer:actual-bigger-than-requested-at-bottom ()
  (with-temp-buffer
    (insert "0123456789")
    (eval `(mocklet (((pop-to-buffer ,(current-buffer)
                                     (list #'display-buffer-at-bottom))))
             (difftastic-pop-to-buffer ,(current-buffer) 9)))))

(ert-deftest difftastic-pop-to-buffer:actual-not-bigger-than-requested ()
  (with-temp-buffer
    (insert "0123456789")
    (eval `(mocklet (((pop-to-buffer ,(current-buffer) (list nil))))
             (difftastic-pop-to-buffer ,(current-buffer) 10)))))

(ert-deftest difftastic--rerun:not-difftastic-mode-error-signaled ()
  (with-temp-buffer
    (let ((data (cadr (should-error (difftastic--rerun nil)
                                    :type 'user-error))))
      (should (equal data "Nothing to rerun")))))


(ert-deftest difftastic--rerun:no-rerun-alist-mode-error-signaled ()
  (with-temp-buffer
    (difftastic-mode)
    (setq difftastic--rerun-alist nil)
    (let ((data (cadr (should-error (difftastic--rerun nil)
                                    :type 'user-error))))
      (should (equal data "Nothing to rerun")))))

(ert-deftest difftastic--rerun:git-command-rerun-requested-width ()
  (let ((rerun-alist '((default-directory . "test-default-directory")
                       (git-command . "test-command")
                       (difftastic-args . ("test-difftastic-args"))))
        (difftastic-rerun-requested-window-width-function
         (lambda ()
           "test-difftastic-width"))
        (run-command-call-count 0))
    (with-temp-buffer
      (difftastic-mode)
      (setq difftastic--rerun-alist rerun-alist)
      (mocklet (((difftastic--build-git-process-environment
                  "test-difftastic-width" '("test-difftastic-args"))
                 => "test-process-environment"))
        (eval
         `(difftastic--with-temp-advice
              'difftastic--run-command
              :override
              (lambda (buffer command sentinel)
                (should (equal default-directory "test-default-directory"))
                (should (equal process-environment "test-process-environment"))
                (should (equal buffer ,(current-buffer)))
                (should (equal command "test-command"))
                (should (functionp sentinel))
                (funcall sentinel)
                ,(cl-incf run-command-call-count))
            (difftastic--rerun nil))))
      (should (eq run-command-call-count 1))
      (should-not (eq difftastic--rerun-alist rerun-alist))
      (should (equal difftastic--rerun-alist rerun-alist)))))

(ert-deftest difftastic--rerun:git-command-requested-width ()
  (let ((rerun-alist '((default-directory . "test-default-directory")
                       (git-command . "test-command")
                       (difftastic-args . ("test-difftastic-args"))))
        (difftastic-rerun-requested-window-width-function nil)
        (difftastic-requested-window-width-function
         (lambda ()
           "test-difftastic-width"))
        (run-command-call-count 0))
    (with-temp-buffer
      (difftastic-mode)
      (setq difftastic--rerun-alist rerun-alist)
      (mocklet (((difftastic--build-git-process-environment
                  "test-difftastic-width" '("test-difftastic-args"))
                 => "test-process-environment"))
        (eval
         `(difftastic--with-temp-advice
              'difftastic--run-command
              :override
              (lambda (buffer command sentinel)
                (should (equal default-directory "test-default-directory"))
                (should (equal process-environment "test-process-environment"))
                (should (equal buffer ,(current-buffer)))
                (should (equal command "test-command"))
                (should (functionp sentinel))
                (funcall sentinel)
                ,(cl-incf run-command-call-count))
            (difftastic--rerun nil))))
      (should (eq run-command-call-count 1))
      (should-not (eq difftastic--rerun-alist rerun-alist))
      (should (equal difftastic--rerun-alist rerun-alist)))))

(ert-deftest difftastic--rerun:git-command-with-lang-override ()
  (let ((rerun-alist '((default-directory . "test-default-directory")
                       (git-command . "test-command")
                       (difftastic-args . ("test-difftastic-args"))))
        (difftastic-rerun-requested-window-width-function
         (lambda ()
           "test-difftastic-width"))
        (run-command-call-count 0))
    (with-temp-buffer
      (difftastic-mode)
      (setq difftastic--rerun-alist rerun-alist)
      (mocklet (((difftastic--build-git-process-environment
                  "test-difftastic-width" '("test-difftastic-args"
                                            "--override"
                                            "*:test-lang-override"))
                 => "test-process-environment"))
        (eval
         `(difftastic--with-temp-advice
              'difftastic--run-command
              :override
              (lambda (buffer command sentinel)
                (should (equal default-directory "test-default-directory"))
                (should (equal process-environment "test-process-environment"))
                (should (equal buffer ,(current-buffer)))
                (should (equal command "test-command"))
                (should (functionp sentinel))
                (funcall sentinel)
                ,(cl-incf run-command-call-count))
            (difftastic--rerun "test-lang-override"))))
      (should (eq run-command-call-count 1))
      (should-not (eq difftastic--rerun-alist rerun-alist))
      (should (equal difftastic--rerun-alist rerun-alist)))))

(ert-deftest difftastic--rerun:files-command-rerun-requested-width ()
  (let ((rerun-alist '((default-directory . "test-default-directory")
                       (lang-override . "test-lang-override")
                       (file-buf-A . ("test-file-buf-A" . nil))
                       (file-buf-B . ("test-file-buf-B" . nil))))
        (difftastic-rerun-requested-window-width-function
         (lambda ()
           "test-difftastic-width"))
        (run-command-call-count 0))
    (with-temp-buffer
      (difftastic-mode)
      (setq difftastic--rerun-alist rerun-alist)
      (mocklet (((difftastic--build-files-command
                  '("test-file-buf-A" . nil) '("test-file-buf-B". nil)
                  "test-difftastic-width" "test-lang-override")
                 => "test-command"))
        (eval
         `(difftastic--with-temp-advice
              'difftastic--run-command
              :override
              (lambda (buffer command sentinel)
                (should (equal default-directory "test-default-directory"))
                (should (equal buffer ,(current-buffer)))
                (should (equal command "test-command"))
                (should (functionp sentinel))
                (funcall sentinel)
                ,(cl-incf run-command-call-count))
            (difftastic--rerun nil))))
      (should (eq run-command-call-count 1))
      (should-not (eq difftastic--rerun-alist rerun-alist))
      (should (equal difftastic--rerun-alist rerun-alist)))))

(ert-deftest difftastic--rerun:files-command-requested-width ()
  (let ((rerun-alist '((default-directory . "test-default-directory")
                       (lang-override . "test-lang-override")
                       (file-buf-A . ("test-file-buf-A" . nil))
                       (file-buf-B . ("test-file-buf-B" . nil))))
        (difftastic-rerun-requested-window-width-function nil)
        (difftastic-requested-window-width-function
         (lambda ()
           "test-difftastic-width"))
        (run-command-call-count 0))
    (with-temp-buffer
      (difftastic-mode)
      (setq difftastic--rerun-alist rerun-alist)
      (mocklet (((difftastic--build-files-command
                  '("test-file-buf-A" . nil) '("test-file-buf-B". nil)
                  "test-difftastic-width" "test-lang-override")
                 => "test-command"))
        (eval
         `(difftastic--with-temp-advice
              'difftastic--run-command
              :override
              (lambda (buffer command sentinel)
                (should (equal default-directory "test-default-directory"))
                (should (equal buffer ,(current-buffer)))
                (should (equal command "test-command"))
                (should (functionp sentinel))
                (funcall sentinel)
                ,(cl-incf run-command-call-count))
            (difftastic--rerun nil))))
      (should (eq run-command-call-count 1))
      (should-not (eq difftastic--rerun-alist rerun-alist))
      (should (equal difftastic--rerun-alist rerun-alist)))))

(ert-deftest difftastic--rerun:files-command-with-lang-override ()
  (let ((rerun-alist '((default-directory . "test-default-directory")
                       (lang-override . "lang-override")
                       (file-buf-A . ("test-file-buf-A" . nil))
                       (file-buf-B . ("test-file-buf-B" . nil))))
        (difftastic-rerun-requested-window-width-function
         (lambda ()
           "test-difftastic-width"))
        (run-command-call-count 0))
    (with-temp-buffer
      (difftastic-mode)
      (setq difftastic--rerun-alist rerun-alist)
      (mocklet (((difftastic--build-files-command
                  '("test-file-buf-A" . nil) '("test-file-buf-B". nil)
                  "test-difftastic-width" "test-lang-override")
                 => "test-command"))
        (eval
         `(difftastic--with-temp-advice
              'difftastic--run-command
              :override
              (lambda (buffer command sentinel)
                (should (equal default-directory "test-default-directory"))
                (should (equal buffer ,(current-buffer)))
                (should (equal command "test-command"))
                (should (functionp sentinel))
                (funcall sentinel)
                ,(cl-incf run-command-call-count))
            (difftastic--rerun "test-lang-override"))))
      (should (eq run-command-call-count 1))
      (should-not (eq difftastic--rerun-alist rerun-alist))
      (should (equal difftastic--rerun-alist rerun-alist)))))

(ert-deftest difftastic-rerun:no-prefix ()
  (mocklet (((difftastic--rerun nil)))
    (call-interactively #'difftastic-rerun)))

(ert-deftest difftastic-rerun:with-prefix ()
  (mocklet (((completing-read "Language: " "test-languages" nil t))
            ((difftastic--get-languages) => "test-languages")
            ((difftastic--rerun nil)))
    (let ((current-prefix-arg 4))
      (call-interactively #'difftastic-rerun))))

(ert-deftest difftastic--git-with-difftastic:basic ()
  (let ((rerun-alist '((default-directory . "test-default-directory")
                       (git-command . "test-command")
                       (difftastic-args . "test-difftastic-args")))
        (default-directory "test-default-directory")
        (difftastic-requested-window-width-function
         (lambda ()
           "test-difftastic-width"))
        (run-command-call-count 0)
        (display-buffer-call-count 0))
    (with-temp-buffer
      (eval
       `(mocklet (((difftastic--build-git-process-environment
                    "test-difftastic-width" "test-difftastic-args")
                   => "test-process-environment"))
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
                                               "test-difftastic-args")))))
      (should (eq run-command-call-count 1))
      (should (eq display-buffer-call-count 1))
      (should (equal difftastic--rerun-alist rerun-alist)))))

(ert-deftest difftastic--magit-show:nil-error-signaled ()
  (let ((data (cadr (should-error (difftastic--magit-show nil)
                                  :type 'user-error))))
    (should (equal data "No revision specified"))))

(ert-deftest difftastic--magit-show:basic ()
  (mocklet (((get-buffer-create "*difftastic git show test-rev*")
             => "test-buffer")
            ((difftastic--git-with-difftastic
              "test-buffer"
              '("git" "--no-pager" "show" "--ext-diff" "test-rev"))))
    (difftastic--magit-show "test-rev")))

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
    (let ((current-prefix-arg 4))
      (call-interactively #'difftastic-magit-show))))

(ert-deftest difftastic--git-diff-range:no-args ()
  (mocklet (((get-buffer-create "*difftastic git diff*") => "test-buffer")
            ((difftastic--git-with-difftastic
              "test-buffer"
              '("git" "--no-pager" "diff" "--ext-diff")
              nil)))
    (difftastic--git-diff-range nil nil nil)))

(ert-deftest difftastic--git-diff-range:with-args ()
  (mocklet (((get-buffer-create
              "*difftastic git diff --ignore-submodules=all test-rev-or-range -- test-path*")
             => "test-buffer")
            ((difftastic--git-with-difftastic
              "test-buffer"
              '("git" "--no-pager" "diff" "--ext-diff" "--ignore-submodules=all"
                "test-rev-or-range" "--" "test-path")
              '("--context 42"))))
    (difftastic--git-diff-range "test-rev-or-range"
                                '("--ignore-submodules=all" "-U42")
                                '("test-path"))))

(ert-deftest difftastic-git-diff-range:basic ()
  (let ((current-prefix-arg 4))
    (mocklet (((magit-diff-read-range-or-commit "Diff for range" nil 4) => "test-rev-or-range")
              ((magit-diff-arguments) => '("test-args" "test-files"))
              ((difftastic--git-diff-range "test-rev-or-range" "test-args" "test-files")))
      (call-interactively #'difftastic-git-diff-range))))

(ert-deftest difftastic--magit-diff:module-section ()
  (eval
   `(let ((section (magit-module-section :type 'module
                                         :range "test-range"))
          (difftastic-git-diff-range-called 0))
      (oset section value "/test-value")
      (mocklet (((magit-toplevel) => "magit-toplevel")
                ((magit-current-section) => section))
        (difftastic--with-temp-advice 'difftastic-git-diff-range
            :override (lambda (&optional rev-or-range args files)
                        (should (equal rev-or-range "test-range"))
                        (should-not args)
                        (should-not files)
                        (should (equal default-directory "/test-value/"))
                        (cl-incf difftastic-git-diff-range-called))
         (difftastic--magit-diff "test-args" "test-files")))
      (should (equal 1 difftastic-git-diff-range-called)))))

(ert-deftest difftastic--magit-diff:module-commit-section ()
  (eval
   `(let ((parent (magit-section))
          (section (magit-section :type 'module-commit))
          (difftastic-git-diff-range-called 0))
      (oset parent value "/test-value")
      (oset section parent parent)
      (mocklet (((magit-toplevel) => "magit-toplevel")
                ((magit-current-section) => section)
                ((magit-diff--dwim) => '(commit . "test-commit")))
        (difftastic--with-temp-advice 'difftastic-git-diff-range
            :override (lambda (&optional rev-or-range args files)
                        (should (equal rev-or-range "test-commit^..test-commit"))
                        (should-not args)
                        (should-not files)
                        (should (equal default-directory "/test-value/"))
                        (cl-incf difftastic-git-diff-range-called))
         (difftastic--magit-diff "test-args" "test-files")))
      (should (equal 1 difftastic-git-diff-range-called)))))

(ert-deftest difftastic--magit-diff:unmerged ()
  (let ((difftastic-git-diff-range-called 0))
    (mocklet (((magit-toplevel) => "magit-toplevel")
              ((magit-diff--dwim) => 'unmerged)
              ((magit-merge-in-progress-p) => t)
              ((magit--merge-range) => "test-range"))
      (difftastic--with-temp-advice 'difftastic-git-diff-range
          :override (lambda (&optional rev-or-range args files)
                      (should (equal rev-or-range "test-range"))
                      (should (equal args "test-args"))
                      (should (equal files "test-files"))
                      (should (equal default-directory "magit-toplevel"))
                      (cl-incf difftastic-git-diff-range-called))
        (difftastic--magit-diff "test-args" "test-files")))
    (should (equal 1 difftastic-git-diff-range-called))))

(ert-deftest difftastic--magit-diff:unmerged-no-merge-in-progress ()
  (mocklet (((magit-toplevel) => "magit-toplevel")
            ((magit-diff--dwim) => 'unmerged)
            ((magit-merge-in-progress-p) => nil)
            (magit--merge-range not-called)
            (difftastic-git-diff-range not-called))
    (let ((data (cadr (should-error
                       (difftastic--magit-diff "test-args" "test-files")))))
      (should (equal data "No merge is in progress")))))

(ert-deftest difftastic--magit-diff:unstaged ()
  (let ((difftastic-git-diff-range-called 0))
    (mocklet (((magit-toplevel) => "magit-toplevel")
              ((magit-diff--dwim) => 'unstaged))
      (difftastic--with-temp-advice 'difftastic-git-diff-range
          :override (lambda (&optional rev-or-range args files)
                      (should-not rev-or-range)
                      (should (equal args "test-args"))
                      (should (equal files "test-files"))
                      (should (equal default-directory "magit-toplevel"))
                      (cl-incf difftastic-git-diff-range-called))
        (difftastic--magit-diff "test-args" "test-files")))
    (should (equal 1 difftastic-git-diff-range-called))))

(ert-deftest difftastic--magit-diff:staged-deleted-modified ()
  (let ((difftastic-git-diff-range-called 0))
    (mocklet (((magit-toplevel) => "magit-toplevel")
              ((magit-diff--dwim) => 'staged)
              ((magit-merge-in-progress-p) => t)
              ((magit-file-at-point) => "test-file-at-point")
              ((magit-file-status "test-file-at-point") => '((nil nil ?D ?U)))
              ((magit--merge-range) => "test-merge-range"))
      (difftastic--with-temp-advice 'difftastic-git-diff-range
          :override (lambda (&optional rev-or-range args files)
                      (should (equal rev-or-range "test-merge-range"))
                      (should (equal args "test-args"))
                      (should (equal files '("test-file-at-point")))
                      (should (equal default-directory "magit-toplevel"))
                      (cl-incf difftastic-git-diff-range-called))
        (difftastic--magit-diff "test-args" "test-files")))
    (should (equal 1 difftastic-git-diff-range-called))))

(ert-deftest difftastic--magit-diff:staged-deleted-modified-no-merge-in-progress ()
  (mocklet (((magit-toplevel) => "magit-toplevel")
            ((magit-diff--dwim) => 'staged)
            ((magit-merge-in-progress-p) => nil)
            ((magit-file-at-point) => "test-file-at-point")
            ((magit-file-status "test-file-at-point") => '((nil nil ?D ?U)))
            (magit--merge-range not-called)
            (difftastic-git-diff-range not-called))
    (let ((data (cadr (should-error
                       (difftastic--magit-diff "test-args" "test-files")))))
      (should (equal data "No merge is in progress")))))

(ert-deftest difftastic--magit-diff:staged-no-file ()
  (let ((difftastic-git-diff-range-called 0))
    (mocklet (((magit-toplevel) => "magit-toplevel")
              ((magit-diff--dwim) => 'staged)
              (magit-merge-in-progress-p not-called)
              ((magit-file-at-point) => nil)
              (magit-file-status not-called)
              (magit--merge-range not-called))
      (difftastic--with-temp-advice 'difftastic-git-diff-range
          :override (lambda (&optional rev-or-range args files)
                      (should-not rev-or-range)
                      (should (equal args '("--cached" "test-args")))
                      (should (equal files "test-files"))
                      (should (equal default-directory "magit-toplevel"))
                      (cl-incf difftastic-git-diff-range-called))
        (difftastic--magit-diff '("test-args") "test-files")))
    (should (equal 1 difftastic-git-diff-range-called))))

(ert-deftest difftastic--magit-diff:staged-no-file-already-cached ()
  (let ((difftastic-git-diff-range-called 0))
    (mocklet (((magit-toplevel) => "magit-toplevel")
              ((magit-diff--dwim) => 'staged)
              (magit-merge-in-progress-p not-called)
              ((magit-file-at-point) => nil)
              (magit-file-status not-called)
              (magit--merge-range not-called))
      (difftastic--with-temp-advice 'difftastic-git-diff-range
          :override (lambda (&optional rev-or-range args files)
                      (should-not rev-or-range)
                      (should (equal args '("test-args" "--cached")))
                      (should (equal files "test-files"))
                      (should (equal default-directory "magit-toplevel"))
                      (cl-incf difftastic-git-diff-range-called))
        (difftastic--magit-diff '("test-args" "--cached") "test-files")))
    (should (equal 1 difftastic-git-diff-range-called))))

(ert-deftest difftastic--magit-diff:staged-not-deleted-modified ()
  (let ((difftastic-git-diff-range-called 0))
    (mocklet (((magit-toplevel) => "magit-toplevel")
              ((magit-diff--dwim) => 'staged)
              (magit-merge-in-progress-p not-called)
              ((magit-file-at-point) => "test-file-at-point")
              ((magit-file-status "test-file-at-point") => nil)
              (magit--merge-range not-called))
      (difftastic--with-temp-advice 'difftastic-git-diff-range
          :override (lambda (&optional rev-or-range args files)
                      (should-not rev-or-range)
                      (should (equal args '("--cached" "test-args")))
                      (should (equal files "test-files"))
                      (should (equal default-directory "magit-toplevel"))
                      (cl-incf difftastic-git-diff-range-called))
        (difftastic--magit-diff '("test-args") "test-files")))
    (should (equal 1 difftastic-git-diff-range-called))))

(ert-deftest difftastic--magit-diff:staged-not-deleted-modified-already-cached ()
  (let ((difftastic-git-diff-range-called 0))
    (mocklet (((magit-toplevel) => "magit-toplevel")
              ((magit-diff--dwim) => 'staged)
              (magit-merge-in-progress-p not-called)
              ((magit-file-at-point) => "test-file-at-point")
              ((magit-file-status "test-file-at-point") => nil)
              (magit--merge-range not-called))
      (difftastic--with-temp-advice 'difftastic-git-diff-range
          :override (lambda (&optional rev-or-range args files)
                      (should-not rev-or-range)
                      (should (equal args '("test-args" "--cached")))
                      (should (equal files "test-files"))
                      (should (equal default-directory "magit-toplevel"))
                      (cl-incf difftastic-git-diff-range-called))
        (difftastic--magit-diff '("test-args" "--cached") "test-files")))
    (should (equal 1 difftastic-git-diff-range-called))))

(ert-deftest difftastic--magit-diff:stash ()
  (let ((difftastic-git-diff-range-called 0))
    (mocklet (((magit-toplevel) => "magit-toplevel")
              ((magit-diff--dwim) => '(stash . "test-commit")))
      (difftastic--with-temp-advice 'difftastic-git-diff-range
          :override (lambda (&optional rev-or-range args files)
                      (should (equal rev-or-range "test-commit^..test-commit"))
                      (should (equal args "test-args"))
                      (should (equal files "test-files"))
                      (should (equal default-directory "magit-toplevel"))
                      (cl-incf difftastic-git-diff-range-called))
        (difftastic--magit-diff "test-args" "test-files")))
    (should (equal 1 difftastic-git-diff-range-called))))

(ert-deftest difftastic--magit-diff:commit ()
  (let ((difftastic-git-diff-range-called 0))
    (mocklet (((magit-toplevel) => "magit-toplevel")
              ((magit-diff--dwim) => '(commit . "test-commit")))
      (difftastic--with-temp-advice 'difftastic-git-diff-range
          :override (lambda (&optional rev-or-range args files)
                      (should (equal rev-or-range "test-commit^..test-commit"))
                      (should (equal args "test-args"))
                      (should (equal files "test-files"))
                      (should (equal default-directory "magit-toplevel"))
                      (cl-incf difftastic-git-diff-range-called))
        (difftastic--magit-diff "test-args" "test-files")))
    (should (equal 1 difftastic-git-diff-range-called))))

(ert-deftest difftastic--magit-diff:range ()
  (let ((difftastic-git-diff-range-called 0))
    (mocklet (((magit-toplevel) => "magit-toplevel")
              ((magit-diff--dwim) => "test-range"))
      (difftastic--with-temp-advice 'difftastic-git-diff-range
          :override (lambda (&optional rev-or-range args files)
                      (should (equal rev-or-range "test-range"))
                      (should (equal args "test-args"))
                      (should (equal files "test-files"))
                      (should (equal default-directory "magit-toplevel"))
                      (cl-incf difftastic-git-diff-range-called))
        (difftastic--magit-diff "test-args" "test-files")))
    (should (equal 1 difftastic-git-diff-range-called))))

(ert-deftest difftastic--magit-diff:fallback ()
  (mocklet (((magit-toplevel) => "magit-toplevel")
            ((magit-diff--dwim) => nil)
            ((call-interactively #'difftastic-git-diff-range)))
    (difftastic--magit-diff "test-args" "test-files")))

(ert-deftest difftastic-magit-diff:basic ()
  (mocklet (((magit-diff-arguments) => '("test-args" "test-files"))
            ((difftastic--magit-diff "test-args" "test-files")))
    (call-interactively #'difftastic-magit-diff)))

(ert-deftest difftastic-mode--do-exit:basic ()
  (with-temp-buffer
    (let ((buffer (current-buffer))
          difftastic-exits-all-viewing-windows)
      (eval `(mocklet (((window-buffer) => ,buffer)
                       ((quit-window) :times 1))
               (difftastic-mode--do-exit))))))

(ert-deftest difftastic-mode--do-exit:all-windows ()
  (with-temp-buffer
    (let ((buffer (current-buffer))
          difftastic-exits-all-viewing-windows)
      (eval `(mocklet (((window-buffer) => ,buffer)
                       ((get-buffer-window-list) => '("window" "window"))
                       ((quit-window nil "window") :times 2))
               (difftastic-mode--do-exit nil t))))))

(ert-deftest difftastic-mode--do-exit:exits-all-viewing-windows ()
  (with-temp-buffer
    (let ((buffer (current-buffer))
          (difftastic-exits-all-viewing-windows t))
      (eval `(mocklet (((window-buffer) => ,buffer)
                       ((get-buffer-window-list) => '("window" "window"))
                       ((quit-window nil "window") :times 2))
               (difftastic-mode--do-exit))))))

(ert-deftest difftastic-mode--do-exit:basic-and-exit-action ()
  (with-temp-buffer
    (let ((buffer (current-buffer))
          difftastic-exits-all-viewing-windows)
      (eval `(mocklet (((window-buffer) => ,buffer)
                       ((quit-window) :times 1)
                       ((exit-action ,buffer) :times 1))
               (difftastic-mode--do-exit #'exit-action))))))

(ert-deftest difftastic-mode--do-exit:all-windows-and-exit-action ()
  (with-temp-buffer
    (let ((buffer (current-buffer))
          difftastic-exits-all-viewing-windows)
      (eval `(mocklet (((window-buffer) => ,buffer)
                       ((get-buffer-window-list) => '("window" "window"))
                       ((quit-window nil "window") :times 2)
                       ((exit-action ,buffer) :times 1))
               (difftastic-mode--do-exit #'exit-action t))))))

(ert-deftest difftastic-mode--do-exit:exits-all-viewing-windows-and-exit-action ()
  (with-temp-buffer
    (let ((buffer (current-buffer))
          (difftastic-exits-all-viewing-windows t))
      (eval `(mocklet (((window-buffer) => ,buffer)
                       ((get-buffer-window-list) => '("window" "window"))
                       ((quit-window nil "window") :times 2)
                       ((exit-action ,buffer) :times 1))
               (difftastic-mode--do-exit #'exit-action))))))

(ert-deftest difftascit-leave:basic ()
  (mocklet (((difftastic-mode--do-exit) :times 1))
    (funcall-interactively #'difftastic-leave)))

(ert-deftest difftascit-quit:basic ()
  (mocklet (((difftastic-mode--do-exit 'kill-buffer) :times 1))
    (funcall-interactively #'difftastic-quit)))

(ert-deftest difftascit-quit-all:basic ()
  (mocklet (((difftastic-mode--do-exit 'kill-buffer t) :times 1))
    (funcall-interactively #'difftastic-quit-all)))

(ert-deftest difftastic--dired-diff:basic ()
  (mocklet (((dired-diff "test-file") :times 1))
    (difftastic--dired-diff "test-file" nil)))

(ert-deftest difftastic--dired-diff:basic-and-lang-override ()
  (let ((dired-diff-called 0))
    (mocklet (((difftastic-files "current" "test-file" "test-lang") :times 1))
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
    (mocklet (((difftastic-files "current" "test-file" "test-lang") :times 1))
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
    (mocklet (((difftastic-files "current" "test-file" "test-lang") :times 1))
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
  (mocklet (((difftastic--dired-diff "test-file" nil) :times 1))
    (difftastic-dired-diff "test-file")))

(ert-deftest difftastic-dired-diff:basic-with-lang-override ()
  (mocklet (((difftastic--dired-diff "test-file" "test-lang") :times 1))
    (difftastic-dired-diff "test-file" "test-lang")))

(ert-deftest difftastic-dired-diff:interactive ()
  (mocklet (((difftastic--dired-diff 'interactive nil) :times 1)
            (completing-read not-called))
    (call-interactively #'difftastic-dired-diff)))

(ert-deftest difftastic-dired-diff:interactive-with-lang-override ()
  (let ((current-prefix-arg 4))
    (mocklet (((difftastic--dired-diff 'interactive "test-lang") :times 1)
              ((difftastic--get-languages) => "test-langs")
              ((completing-read "Language: " "test-langs" nil t) => "test-lang"))
      (call-interactively #'difftastic-dired-diff))))

(ert-deftest difftastic--files-internal:basic ()
  (with-temp-buffer
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
                        ',file-buf-A ',file-buf-B "test-width" "test-lang")
                       => '("echo" "-n" "test output")))
              (let ((process
                     (difftastic--files-internal ,buffer
                                                 ',file-buf-A
                                                 ',file-buf-B
                                                 "test-lang")))
                (with-timeout (5
                               (signal-process process 'SIGKILL)
                               (ert-fail "timeout"))
                  (while (accept-process-output process))))
              (should (equal (buffer-string) "test output"))
              (should (equal (alist-get 'default-directory difftastic--rerun-alist)
                             default-directory))
              (should (equal (alist-get 'lang-override difftastic--rerun-alist)
                             "test-lang"))
              (should (equal (alist-get 'file-buf-A difftastic--rerun-alist)
                             ',file-buf-A))
              (should (equal (alist-get 'file-buf-B difftastic--rerun-alist)
                             ',file-buf-B))
              (should-not (file-exists-p (car ',file-buf-A)))
              (should-not (file-exists-p (car ',file-buf-B)))))
        (when (file-exists-p (car file-buf-A))
          (delete-file (car file-buf-A)))
        (when (file-exists-p (car file-buf-B))
          (delete-file (car file-buf-B))))
      (should (equal display-buffer-called 1)))))

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
                                  "Buffer B to compare: " "default-buffer-B" t)))
             (read-buffer-called 0)
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
                                  "Buffer B to compare: " "default-buffer-B" t)))
             (read-buffer-called 0)
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
             (current-prefix-arg 4))
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
                                  "Buffer B to compare: " "default-buffer-B" t)))
             (read-buffer-called 0)
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
                                  "Buffer B to compare: " "default-buffer-B" t)))
             (read-buffer-called 0)
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
                                  "Buffer B to compare: " "default-buffer-B" t)))
             (read-buffer-called 0)
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

(ert-deftest difftastic-buffers:basic ()
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
         `(mocklet (((difftastic--buffers-args) => '(,(buffer-name buffer-A)
                                                     ,(buffer-name buffer-B)
                                                     "test-lang"))
                    ((get-buffer-create ,(format "*difftastic %s %s*"
                                                 (buffer-name buffer-A)
                                                 (buffer-name buffer-B)))
                     => "test-buffer")
                    ((difftastic--files-internal "test-buffer"
                                                 ',(cons file-A nil)
                                                 ',(cons file-B nil)
                                                 "test-lang")
                     :times 1))
            (call-interactively #'difftastic-buffers)))
      (when (buffer-name buffer-B)
        (kill-buffer buffer-B))
      (when (buffer-name buffer-A)
        (kill-buffer buffer-A))
      (when (file-exists-p file-B)
        (delete-file file-B))
      (when (file-exists-p file-A)
        (delete-file file-A)))))

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
    (should (equal (difftastic--files-args)
                   (list "/last-dir-A/file-A"
                         "/last-dir-B/file-B"
                         nil)))
    (should (equal file-name-history orig-file-name-history))
    (should (equal difftastic--last-dir-A "/last-dir-A/"))
    (should (equal difftastic--last-dir-B "/last-dir-B/"))
    (should (equal ediff-read-file-name-called 2))
    (should (equal ediff-get-default-file-name-called 2))))

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
    (should (equal (difftastic--files-args)
                   (list "/test-directory/file-A"
                         "/test-directory/file-B"
                         nil)))
    (should (equal file-name-history orig-file-name-history))
    (should (equal difftastic--last-dir-A "/test-directory/"))
    (should (equal difftastic--last-dir-B "/test-directory/"))
    (should (equal ediff-read-file-name-called 2))
    (should (equal ediff-get-default-file-name-called 2))))

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
             (current-prefix-arg 4))
    (mocklet (((difftastic--get-languages) => "test-languages")
              ((completing-read "Language: " "test-languages" nil t) => "test-lang"))
      (should (equal (difftastic--files-args)
                   (list "/last-dir-A/file-A"
                         "/last-dir-B/file-B"
                         "test-lang"))))
    (should (equal file-name-history orig-file-name-history))
    (should (equal difftastic--last-dir-A "/last-dir-A/"))
    (should (equal difftastic--last-dir-B "/last-dir-B/"))
    (should (equal ediff-read-file-name-called 2))
    (should (equal ediff-get-default-file-name-called 2))))

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
             (current-prefix-arg 4))
    (mocklet (((difftastic--get-languages) => "test-languages")
              ((completing-read "Language: " "test-languages" nil t) => "test-lang"))
      (should (equal (difftastic--files-args)
                   (list "/test-directory/file-A"
                         "/test-directory/file-B"
                         "test-lang"))))
    (should (equal file-name-history orig-file-name-history))
    (should (equal difftastic--last-dir-A "/test-directory/"))
    (should (equal difftastic--last-dir-B "/test-directory/"))
    (should (equal ediff-read-file-name-called 2))
    (should (equal ediff-get-default-file-name-called 2))))

(ert-deftest difftastic-files:basic ()
  (mocklet (((difftastic--files-args) => '("/dir/file-A" "/dir/file-B" "test-lang"))
            ((get-buffer-create "*difftastic file-A file-B*") => "test-buffer")
            ((difftastic--files-internal
              "test-buffer" '("/dir/file-A" . nil) '("/dir/file-B" . nil) "test-lang")
             :times 1))
    (call-interactively #'difftastic-files)))

;; LocalWords: README el

(provide 'difftastic.t)

;;; difftastic.t.el ends here
