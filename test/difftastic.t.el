;;; difftastic.t.el --- Tests for difftastic        -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'difftastic)
(require 'el-mock)

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
            (c++-ts-mode)
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

(ert-deftest difftastic--with-file-bufs:basic-case-temporary-file-bufs-are-preserved ()
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

(ert-deftest difftastic--with-file-bufs:error-mode-temporary-file-bufs-are-deleted ()
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
           (format "GIT_EXTERNAL_DIFF=%s --width 42 --background %s"
                   difftastic-executable
                   (frame-parameter nil 'background-mode))
           (car (difftastic--build-git-process-environment 42)))))

(ert-deftest difftastic--build-git-process-environment:with-difftastic-args ()
  (should (equal
           (format
            "GIT_EXTERNAL_DIFF=%s --width 42 --background %s --override *:C++"
            difftastic-executable
            (frame-parameter nil 'background-mode))
           (car
            (difftastic--build-git-process-environment
             42
             '("--override" "*:C++"))))))

(ert-deftest difftastic--build-files-command:without-lang-override ()
  (should (equal
           `(,difftastic-executable
             "--width" "42"
             "--background" ,(format "%s" (frame-parameter nil 'background-mode))
             "test-file-A" "test-file-B")
           (difftastic--build-files-command (cons "test-file-A" nil)
                                            (cons "test-file-B" nil)
                                            42))))

(ert-deftest difftastic--build-files-command:with-lang-override ()
  (should (equal
           `(,difftastic-executable
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
        (let (buffer rerun-alist orig-rerun-alist)
          (with-temp-buffer
            (setq buffer (current-buffer)))

          (setq rerun-alist `((file-buf-test . ("test-file" . ,buffer)))
                orig-rerun-alist (copy-tree rerun-alist))
          (should-error
           (setq file-buf
                 (difftastic--rerun-file-buf
                  "test"
                  (alist-get 'file-buf-test rerun-alist)
                  rerun-alist))
           :type 'user-error)

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
      (should-error (difftastic-next-chunk)
                    :type 'user-error))))

(ert-deftest difftastic-next-chunk:last-chunk-error-signaled ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (with-temp-buffer
      (insert "difftastic.el --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-")
      (difftastic-mode)
      (goto-char (point-min))
      (should-error (difftastic-next-chunk)
                    :type 'user-error)
      (should (equal (point-min) (point))))))

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
      (should-error (difftastic-next-file)
                    :type 'user-error))))

(ert-deftest difftastic-next-file:last-chunk-error-signaled ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (with-temp-buffer
      (insert "difftastic.el --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-")
      (difftastic-mode)
      (goto-char (point-min))
      (should-error (difftastic-next-file)
                    :type 'user-error)
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
      (should-error (difftastic-previous-chunk)
                    :type 'user-error))))

(ert-deftest difftastic-previous-chunk:first-chunk-error-signaled ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (with-temp-buffer
      (insert "difftastic.el --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-")
      (difftastic-mode)
      (goto-char (point-min))
      (should-error (difftastic-previous-chunk)
                    :type 'user-error)
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
      (should-error (difftastic-previous-file)
                    :type 'user-error))))

(ert-deftest difftastic-previous-file:first-file-error-signaled ()
  (mocklet ((difftastic--get-languages => '("Text" "Emacs Lisp" "C++" "Java")))
    (with-temp-buffer
      (insert "difftastic.el --- Emacs Lisp
1 ;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-")
      (difftastic-mode)
      (goto-char (point-min))
      (should-error (difftastic-previous-file)
                    :type 'user-error)
      (should (equal (point-min) (point))))))


(provide 'difftastic.t)

;;; difftastic.t.el ends here
