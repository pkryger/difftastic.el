;;; difftastic-bindings.t.el --- Tests for difftastic-bindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when-let* ((dir (file-name-directory (or load-file-name
                                          byte-compile-current-file
                                          buffer-file-name))))
  (load-file (file-name-concat dir "undercover-init.el")))

(require 'ert)
(require 'difftastic-bindings)
(require 'el-mock)
(require 'transient)
(require 'cl-lib)


(ert-deftest difftastic-bindings--parse-bindings:transient-column ()
  (should (equal (difftastic-bindings--parse-bindings '(("A" fun-a "Do A")
                                                        ("B" 'fun-b "")
                                                        ("C" #'fun-c)))
                 [("A" "Do A" fun-a)
                  ("B" "fun-b" fun-b)
                  ("C" "fun-c" fun-c)])))

(ert-deftest difftastic-bindings--parse-bindings:transient-suffix ()
  (should (equal (difftastic-bindings--parse-bindings '(("A" fun-a "Do A")))
                 '("A" "Do A" fun-a)))
  (should (equal (difftastic-bindings--parse-bindings '(("B" 'fun-b "")))
                 '("B" "fun-b" fun-b)))
  (should (equal (difftastic-bindings--parse-bindings '(("C" #'fun-c)))
                 '("C" "fun-c" fun-c))))

(ert-deftest difftastic-bindings--add-to-installed:first-element ()
  (let (difftastic-bindings--installed-plist)
    (difftastic-bindings--add-to-installed :prefixes 'foo)
    (should-not (cl-set-exclusive-or
                 (plist-get difftastic-bindings--installed-plist :prefixes)
                 '(foo)))))

(ert-deftest difftastic-bindings--add-to-installed:second-element ()
  (let ((difftastic-bindings--installed-plist '(:prefixes (foo))))
    (difftastic-bindings--add-to-installed :prefixes 'bar)
    (should-not (cl-set-exclusive-or
                 (plist-get difftastic-bindings--installed-plist :prefixes)
                 '(bar foo)))))

(ert-deftest difftastic-bindings--add-to-installed:duplicate-element ()
  (let ((difftastic-bindings--installed-plist '(:prefixes (foo))))
    (difftastic-bindings--add-to-installed :prefixes 'foo)
    (should-not (cl-set-exclusive-or
                 (plist-get difftastic-bindings--installed-plist :prefixes)
                 '(foo)))))

(ert-deftest difftastic-bindings--add-to-installed:other-type ()
  (let ((difftastic-bindings--installed-plist '(:prefixes (foo))))
    (difftastic-bindings--add-to-installed :keymaps 'foo)
    (should-not (cl-set-exclusive-or
                 (plist-get difftastic-bindings--installed-plist :prefixes)
                 '(foo)))
    (should-not (cl-set-exclusive-or
                 (plist-get difftastic-bindings--installed-plist :keymaps)
                 '(foo)))))

(ert-deftest difftastic-bindings--remove-from-installed:not-existing-element ()
  (let ((difftastic-bindings--installed-plist '(:prefixes (foo))))
    (difftastic-bindings--remove-from-installed :prefixes 'bar)
    (should-not (cl-set-exclusive-or
                 (plist-get difftastic-bindings--installed-plist :prefixes)
                 '(foo)))))

(ert-deftest difftastic-bindings--remove-from-installed:not-existing-type ()
  (let ((difftastic-bindings--installed-plist '(:prefixes (foo))))
    (difftastic-bindings--remove-from-installed :keymaps 'bar)
    (should-not (cl-set-exclusive-or
                 (plist-get difftastic-bindings--installed-plist :prefixes)
                 '(foo)))))

(ert-deftest difftastic-bindings--remove-from-installed:only-element ()
  (let ((difftastic-bindings--installed-plist '(:prefixes (foo))))
    (difftastic-bindings--remove-from-installed :prefixes 'foo)
    (should-not (plist-get difftastic-bindings--installed-plist :prefixes))))

(ert-deftest difftastic-bindings--remove-from-installed:not-only-element ()
  (let ((difftastic-bindings--installed-plist '(:prefixes (foo bar))))
    (difftastic-bindings--remove-from-installed :prefixes 'foo)
    (should-not (cl-set-exclusive-or
                 (plist-get difftastic-bindings--installed-plist :prefixes)
                 '(bar)))))

(ert-deftest difftastic-bindings--remove-from-installed:other-type ()
  (let ((difftastic-bindings--installed-plist
         '(:prefixes (foo) :keymaps (foo))))
    (difftastic-bindings--remove-from-installed :prefixes 'foo)
    (should-not (plist-get difftastic-bindings--installed-plist :prefixes))
    (should-not (cl-set-exclusive-or
                 (plist-get difftastic-bindings--installed-plist :keymaps)
                 '(foo)))))


(ert-deftest difftastic-bindings--append-suffix:non-existing ()
  (unwind-protect
      (let ((suffix [("A" "Do A" fun-a)])
            difftastic-bindings--installed-plist)
        (transient-define-prefix dummy-prefix nil [("B" "Do B" fun-b)])
        (difftastic-bindings--append-suffix 'dummy-prefix '(-1) suffix)
        (should (equal (ignore-errors
                         (transient-get-suffix 'dummy-prefix '(-1)))
                       (transient-parse-suffix 'dummy-prefix suffix)))
        (should (equal (function-get 'dummy-prefix 'difftastic--installed)
                       (cons '(-1) suffix)))
        (should (memq 'dummy-prefix
                      (plist-get difftastic-bindings--installed-plist
                                 :prefixes))))
    (fmakunbound 'dummy-prefix)))

(ert-deftest difftastic-bindings--append-suffix:existing ()
  (unwind-protect
      (let ((suffix [("A" "Do A" fun-a)])
            difftastic-bindings--installed-plist)
        (transient-define-prefix dummy-prefix nil [("A" "Do A" fun-a)])
        (difftastic-bindings--append-suffix 'dummy-prefix '(-1) suffix)
        (should (equal (ignore-errors
                         (transient-get-suffix 'dummy-prefix '(-1)))
                       (transient-parse-suffix 'dummy-prefix suffix)))
        (should-not (function-get 'dummy-prefix 'difftastic--installed))
        (should-not (memq 'dummy-prefix
                          (plist-get difftastic-bindings--installed-plist
                                     :prefixes))))
    (fmakunbound 'dummy-prefix)))

(ert-deftest difftastic-bindings--remove-suffix:non-existing ()
  (unwind-protect
      (let ((suffix [("B" "Do B" fun-b)])
            difftastic-bindings--installed-plist)
        (transient-define-prefix dummy-prefix nil [("B" "Do B" fun-b)])
        (difftastic-bindings--remove-suffix 'dummy-prefix)
        (should (equal (ignore-errors
                         (transient-get-suffix 'dummy-prefix '(-1)))
                       (transient-parse-suffix 'dummy-prefix suffix)))
        (should-not (function-get 'dummy-prefix 'difftastic--installed))
        (should-not (memq 'dummy-prefix
                          (plist-get difftastic-bindings--installed-plist
                                     :prefixes))))
    (fmakunbound 'dummy-prefix)))

(ert-deftest difftastic-bindings--remove-suffix:existing ()
  (unwind-protect
      (let ((suffix [("A" "Do A" fun-a)])
            (difftastic-bindings--installed-plist '(:prefixes (dummy-prefix))))
        (transient-define-prefix dummy-prefix nil [[("B" "Do B" fun-b)]
                                                   [("A" "Do A" fun-a)]])
        (function-put 'dummy-prefix 'difftastic--installed (cons '(-1 -1)
                                                                 suffix))
        (difftastic-bindings--remove-suffix 'dummy-prefix)
        (should-not (equal (ignore-errors
                             (transient-get-suffix 'dummy-prefix '(-1 -1)))
                           (transient-parse-suffix 'dummy-prefix suffix)))
        (should-not (function-get 'dummy-prefix 'difftastic--installed))
        (should-not (memq 'dummy-prefix
                          (plist-get difftastic-bindings--installed-plist
                                     :prefixes))))
    (fmakunbound 'dummy-prefix)))

(ert-deftest difftastic-bindings--remove-suffix:existing-fallback-suffix ()
  (unwind-protect
      (let ((suffix '("A" "Do A" fun-a))
            (difftastic-bindings--installed-plist '(:prefixes (dummy-prefix))))
        (transient-define-prefix dummy-prefix nil [("A" "Do A" fun-a)
                                                   [("B" "Do B" fun-b)]])
        (function-put 'dummy-prefix 'difftastic--installed (cons '(-1 -1)
                                                                 suffix))
        (difftastic-bindings--remove-suffix 'dummy-prefix)
        (should-not (ignore-errors
                      (transient-get-suffix 'dummy-prefix "A")))
        (should-not (function-get 'dummy-prefix 'difftastic--installed))
        (should-not  (memq 'dummy-prefix
                           (plist-get difftastic-bindings--installed-plist
                                      :prefixes))))
    (fmakunbound 'dummy-prefix)))

(ert-deftest difftastic-bindings--remove-suffix:existing-fallback-column ()
  (unwind-protect
      (let ((suffix [("A" "Do A" fun-a)])
            (difftastic-bindings--installed-plist '(:prefixes (dummy-prefix))))
        (transient-define-prefix dummy-prefix nil [[("A" "Do A" fun-a)]
                                                   [("B" "Do B" fun-b)]])
        (function-put 'dummy-prefix 'difftastic--installed (cons '(-1 -1)
                                                                 suffix))
        (difftastic-bindings--remove-suffix 'dummy-prefix)
        (should-not (ignore-errors
                      (transient-get-suffix 'dummy-prefix "A")))
        (should-not (function-get 'dummy-prefix 'difftastic--installed))
        (should-not  (memq 'dummy-prefix
                           (plist-get difftastic-bindings--installed-plist
                                      :prefixes))))
    (fmakunbound 'dummy-prefix)))


(defvar difftastic-bindings-t-keymap-1 nil)
(defvar difftastic-bindings-t-keymap-2 nil)

(ert-deftest difftastic-bindings--bind-keys:non-existing ()
  (unwind-protect
      (let ((bindings '(("A" fun-a)
                        ("B" fun-b)))
            difftastic-bindings--installed-plist
            installed)
        (setq difftastic-bindings-t-keymap-1 (make-sparse-keymap))
        (difftastic-bindings--bind-keys 'difftastic-bindings-t-keymap-1
                                        bindings)
        (map-keymap (lambda (key fun)
                      (push (list (format "%c" key) fun) installed))
                    difftastic-bindings-t-keymap-1)
        (should-not (cl-set-exclusive-or bindings installed :test #'equal))
        (should (equal (get 'difftastic-bindings-t-keymap-1 '
                            difftastic--installed)
                       bindings))
        (should (memq 'difftastic-bindings-t-keymap-1
                      (plist-get difftastic-bindings--installed-plist
                                 :keymaps))))
    (setq difftastic-bindings-t-keymap-1 nil)
    (put 'difftastic-bindings-t-keymap-1 'difftastic--installed nil)))

(ert-deftest difftastic-bindings--bind-keys:existing ()
  (unwind-protect
      (let ((bindings '(("A" fun-b)
                        ("B" fun-a)))
            difftastic-bindings--installed-plist
            installed)
        (setq difftastic-bindings-t-keymap-1 (define-keymap
                                               "A" 'fun-x
                                               "B" 'fun-y))
        (difftastic-bindings--bind-keys 'difftastic-bindings-t-keymap-1
                                        bindings)
        (map-keymap (lambda (key fun)
                      (push (list (format "%c" key) fun) installed))
                    difftastic-bindings-t-keymap-1)
        (should-not (cl-set-exclusive-or bindings installed :test #'equal))
        (should (equal (get 'difftastic-bindings-t-keymap-1
                            'difftastic--installed)
                       bindings))
        (should (memq 'difftastic-bindings-t-keymap-1
                      (plist-get difftastic-bindings--installed-plist
                                 :keymaps))))
    (setq difftastic-bindings-t-keymap-1 nil)
    (put 'difftastic-bindings-t-keymap-1 'difftastic--installed nil)))

(ert-deftest difftastic-bindings--unbind-keys:non-existing ()
  (unwind-protect
      (let (difftastic-bindings--installed-plist
            installed)
        (setq difftastic-bindings-t-keymap-1 (make-sparse-keymap))
        (difftastic-bindings--unbind-keys 'difftastic-bindings-t-keymap-1)
        (map-keymap (lambda (key fun)
                      (push (list (format "%c" key) fun) installed))
                    difftastic-bindings-t-keymap-1)
        (should-not installed)
        (should-not (get 'difftastic-bindings-t-keymap-1
                         'difftastic--installed))
        (should-not (memq 'difftastic-bindings-t-keymap-1
                          (plist-get difftastic-bindings--installed-plist
                                     :keymaps))))
    (setq difftastic-bindings-t-keymap-1 nil)
    (put 'difftastic-bindings-t-keymap-1 'difftastic--installed nil)))

(ert-deftest difftastic-bindings--unbind-keys:existing ()
  (unwind-protect
      (let ((bindings '(("A" fun-b)
                        ("B" fun-a)))
            (difftastic-bindings--installed-plist
             '(:keymaps (difftastic-bindings-t-keymap-1)))
            installed)
        (setq difftastic-bindings-t-keymap-1 (define-keymap
                                               "A" 'fun-a
                                               "B" 'fun-b))
        (put 'difftastic-bindings-t-keymap-1 'difftastic--installed bindings)
        (difftastic-bindings--unbind-keys 'difftastic-bindings-t-keymap-1)
        (map-keymap (lambda (key fun)
                      (push (list (format "%c" key) fun) installed))
                    difftastic-bindings-t-keymap-1)
        (should-not installed)
        (should-not (get 'difftastic-bindings-t-keymap-1
                         'difftastic--installed))
        (should-not (memq 'difftastic-bindings-t-keymap-1
                          (plist-get difftastic-bindings--installed-plist
                                     :keymaps))))
    (setq difftastic-bindings-t-keymap-1 nil)
    (put 'difftastic-bindings-t-keymap-1 'difftastic--installed nil)))


(ert-deftest difftastic-bindings--prefix-specs:basic ()
  (let ((bindings-alist '((((prefixes .  ((magit-diff (-1 -1) magit-diff)
                                          (magit-blame (-1) magit-blame)))
                            (keymaps . ((magit-blame-read-only-mode-map . magit-blame))))
                           .
                           (("D" difftastic-magit-diff "Difftastic diff (dwim)")
                            ("S" difftastic-magit-show "Difftastic show")))
                          (((prefixes . ((magit-file-dispatch (0 1 1) magit-file))))
                           .
                           (("M-d" difftastic-magit-diff-buffer-file "Difftastic")))
                          (((keymaps . ((dired-mode-map . dired))))
                           .
                           (("M-=" difftastic-dired-diff))))))
    (should-not (cl-set-exclusive-or
                 '((magit-diff (-1 -1) magit-diff)
                   (magit-blame (-1) magit-blame)
                   (magit-file-dispatch (0 1 1) magit-file))
                 (difftastic-bindings--prefix-specs
                  bindings-alist)))))


(ert-deftest difftastic-bindings--keymaps:basic ()
  (let ((bindings-alist '((((prefixes .  ((magit-diff (-1 -1) magit-diff)
                                          (magit-blame (-1) magit-blame)))
                            (keymaps . ((magit-blame-read-only-mode-map . magit-blame))))
                           .
                           (("D" difftastic-magit-diff "Difftastic diff (dwim)")
                            ("S" difftastic-magit-show "Difftastic show")))
                          (((prefixes . ((magit-file-dispatch (0 1 1) magit-file))))
                           .
                           (("M-d" difftastic-magit-diff-buffer-file "Difftastic")))
                          (((keymaps . ((dired-mode-map . dired))))
                           .
                           (("M-=" difftastic-dired-diff))))))
    (should-not (cl-set-exclusive-or
                 '((magit-blame-read-only-mode-map . magit-blame)
                   (dired-mode-map . dired))
                 (difftastic-bindings--keymaps
                  bindings-alist)))))


(ert-deftest difftastic-bindings--prefix-specs-bindings:prefix ()
  (let ((bindings-alist '((((prefixes .  ((magit-diff (-1 -1) magit-diff)
                                          (magit-blame (-1) magit-blame)))
                            (keymaps . ((magit-blame-read-only-mode-map . magit-blame))))
                           .
                           (("D" difftastic-magit-diff "Difftastic diff (dwim)")
                            ("S" difftastic-magit-show "Difftastic show")))
                          (((prefixes . ((magit-file-dispatch (0 1 1) magit-file))))
                           .
                           (("M-d" difftastic-magit-diff-buffer-file "Difftastic")))
                          (((keymaps . ((dired-mode-map . dired))))
                           .
                           (("M-=" difftastic-dired-diff))))))
    (should (equal
             '(((magit-diff (-1 -1) magit-diff)
                .
                (("D" difftastic-magit-diff "Difftastic diff (dwim)")
                 ("S" difftastic-magit-show "Difftastic show"))))
             (difftastic-bindings--prefix-specs-bindings
              bindings-alist
              'magit-diff)))
    (should (equal
             '(((magit-file-dispatch (0 1 1) magit-file)
                .
                (("M-d" difftastic-magit-diff-buffer-file "Difftastic"))))
             (difftastic-bindings--prefix-specs-bindings
              bindings-alist
              'magit-file-dispatch)))
    (should-not (difftastic-bindings--prefix-specs-bindings
                 bindings-alist
                 'magit-blame-read-only-mode-map))))

(ert-deftest difftastic-bindings--prefix-specs-bindings:all ()
  (let ((bindings-alist '((((prefixes .  ((magit-diff (-1 -1) magit-diff)
                                          (magit-blame (-1) magit-blame)))
                            (keymaps . ((magit-blame-read-only-mode-map . magit-blame))))
                           .
                           (("D" difftastic-magit-diff "Difftastic diff (dwim)")
                            ("S" difftastic-magit-show "Difftastic show")))
                          (((prefixes . ((magit-file-dispatch (0 1 1) magit-file))))
                           .
                           (("M-d" difftastic-magit-diff-buffer-file "Difftastic")))
                          (((keymaps . ((dired-mode-map . dired))))
                           .
                           (("M-=" difftastic-dired-diff))))))
    (should-not (cl-set-exclusive-or
                 '(((magit-diff (-1 -1) magit-diff)
                    .
                    (("D" difftastic-magit-diff "Difftastic diff (dwim)")
                     ("S" difftastic-magit-show "Difftastic show")))
                   ((magit-blame (-1) magit-blame)
                    .
                    (("D" difftastic-magit-diff "Difftastic diff (dwim)")
                     ("S" difftastic-magit-show "Difftastic show")))
                   ((magit-file-dispatch (0 1 1) magit-file)
                    .
                    (("M-d" difftastic-magit-diff-buffer-file "Difftastic"))))
                 (difftastic-bindings--prefix-specs-bindings
                  bindings-alist)))))


(ert-deftest difftastic-bindings--keymap-bindings:keymap ()
  (let ((bindings-alist '((((prefixes .  ((magit-diff (-1 -1) magit-diff)
                                          (magit-blame (-1) magit-blame)))
                            (keymaps . ((magit-blame-read-only-mode-map . magit-blame))))
                           .
                           (("D" difftastic-magit-diff "Difftastic diff (dwim)")
                            ("S" difftastic-magit-show "Difftastic show")))
                          (((prefixes . ((magit-file-dispatch (0 1 1) magit-file))))
                           .
                           (("M-d" difftastic-magit-diff-buffer-file "Difftastic")))
                          (((keymaps . ((dired-mode-map . dired))))
                           .
                           (("M-=" difftastic-dired-diff))))))
    (should (equal
             '(((magit-blame-read-only-mode-map . magit-blame)
                .
                (("D" difftastic-magit-diff "Difftastic diff (dwim)")
                 ("S" difftastic-magit-show "Difftastic show"))))
             (difftastic-bindings--keymap-bindings
              bindings-alist
              'magit-blame-read-only-mode-map)))
    (should (equal
             '(((dired-mode-map . dired)
                .
                (("M-=" difftastic-dired-diff))))
             (difftastic-bindings--keymap-bindings
              bindings-alist
              'dired-mode-map)))
    (should-not (difftastic-bindings--keymap-bindings
                 bindings-alist
                 'magit-diff))))

(ert-deftest difftastic-bindings--keymap-bindings:all ()
  (let ((bindings-alist '((((prefixes .  ((magit-diff (-1 -1) magit-diff)
                                          (magit-blame (-1) magit-blame)))
                            (keymaps . ((magit-blame-read-only-mode-map . magit-blame))))
                           .
                           (("D" difftastic-magit-diff "Difftastic diff (dwim)")
                            ("S" difftastic-magit-show "Difftastic show")))
                          (((prefixes . ((magit-file-dispatch (0 1 1) magit-file))))
                           .
                           (("M-d" difftastic-magit-diff-buffer-file "Difftastic")))
                          (((keymaps . ((dired-mode-map . dired))))
                           .
                           (("M-=" difftastic-dired-diff))))))
    (should-not (cl-set-exclusive-or
                 '(((magit-blame-read-only-mode-map . magit-blame)
                    .
                    (("D" difftastic-magit-diff "Difftastic diff (dwim)")
                     ("S" difftastic-magit-show "Difftastic show")))
                   ((dired-mode-map . dired)
                    .
                    (("M-=" difftastic-dired-diff))))
                 (difftastic-bindings--keymap-bindings
                  bindings-alist)))))


(ert-deftest difftastic-bindings--add-to-after-load:one-feature ()
  (let (difftastic-bindings--after-load-alist)
    (difftastic-bindings--add-to-after-load 'dummy-feature
                                            :prefixes 'dummy-prefix)
    (let ((entry (assq 'dummy-feature difftastic-bindings--after-load-alist)))
      (should entry)
      (should-not (cl-set-exclusive-or (plist-get (cdr entry) :prefixes)
                                       '(dummy-prefix))))))

(ert-deftest difftastic-bindings--add-to-after-load:one-file ()
  (let (difftastic-bindings--after-load-alist)
    (difftastic-bindings--add-to-after-load "dummy-feature"
                                            :prefixes 'dummy-prefix)
    (let ((entry (assq 'dummy-feature difftastic-bindings--after-load-alist)))
      (should entry)
      (should-not (cl-set-exclusive-or (plist-get (cdr entry) :prefixes)
                                       '(dummy-prefix))))))

(ert-deftest difftastic-bindings--add-to-after-load:feature-and-file-same-symbol ()
  (let (difftastic-bindings--after-load-alist)
    (difftastic-bindings--add-to-after-load 'dummy-feature
                                            :prefixes 'dummy-prefix)
    (difftastic-bindings--add-to-after-load "dummy-feature"
                                            :prefixes 'dummy-prefix)
    (let ((entry (assq 'dummy-feature difftastic-bindings--after-load-alist)))
      (should entry)
      (should-not (cl-set-exclusive-or (plist-get (cdr entry) :prefixes)
                                       '(dummy-prefix))))))

(ert-deftest difftastic-bindings--add-to-after-load:feature-and-file-different-symbols ()
  (let (difftastic-bindings--after-load-alist)
    (difftastic-bindings--add-to-after-load 'dummy-feature
                                            :prefixes 'dummy-prefix-1)
    (difftastic-bindings--add-to-after-load "dummy-feature"
                                            :prefixes 'dummy-prefix-2)
    (let ((entry (assq 'dummy-feature difftastic-bindings--after-load-alist)))
      (should entry)
      (should-not (cl-set-exclusive-or (plist-get (cdr entry) :prefixes)
                                       '(dummy-prefix-1 dummy-prefix-2))))))


(ert-deftest difftastic-bindings--add-to-after-load:feature-and-file-different-types ()
  (let (difftastic-bindings--after-load-alist)
    (difftastic-bindings--add-to-after-load 'dummy-feature
                                            :prefixes 'dummy-prefix)
    (difftastic-bindings--add-to-after-load "dummy-feature"
                                            :keymaps 'dummy-prefix)
    (let ((entry (assq 'dummy-feature difftastic-bindings--after-load-alist)))
      (should entry)
      (should-not (cl-set-exclusive-or (plist-get (cdr entry) :prefixes)
                                       '(dummy-prefix)))
      (should-not (cl-set-exclusive-or (plist-get (cdr entry) :keymaps)
                                       '(dummy-prefix))))))


(ert-deftest difftastic-bindings--after-load:matching ()
  (unwind-protect
      ;; Arrange
      (let ((difftastic-bindings-mode t)
            (difftastic-bindings-alist
             '((((prefixes . ((dummy-prefix-1 (-1 -1) difftastic-bindings-t-dummy)))
                 (keymaps . ((difftastic-bindings-t-keymap-1 . difftastic-bindings-t-dummy)
                             (difftastic-bindings-t-keymap-2 . difftastic-bindings-t-dummy))))
                .
                (("A" fun-a "Do A")))))
            (difftastic-bindings--after-load-alist
             (list
              (cons 'difftastic-bindings-t-dummy
                    '(:prefixes
                      (dummy-prefix-1)
                      :keymaps
                      (difftastic-bindings-t-keymap-1
                       difftastic-bindings-t-keymap-2
                       difftastic-bindings-t-keymap-3))))))
        (provide 'difftastic-bindings-t-dummy)
        (setq difftastic-bindings-t-keymap-1 (make-sparse-keymap))
        (setq difftastic-bindings-t-keymap-2 (make-sparse-keymap))
        (put 'difftastic-bindings-t-keymap-2 'difftastic--installed
             difftastic-bindings-alist)
        (eval
         ;; Expect
         `(mocklet (((difftastic-bindings--append-suffix
                      'dummy-prefix-1 '(-1 -1)
                      `,(difftastic-bindings--parse-bindings
                         '(("A" fun-a "Do A"))))
                     :times 1)
                    ((difftastic-bindings--bind-keys
                      'difftastic-bindings-t-keymap-1
                      '(("A" fun-a "Do A")))
                     :times 1))
            ;; Act
            (difftastic-bindings--after-load
             "/a/path/difftastic-bindings-t-dummy.elc"))))
    ;; Cleanup
    (setq features (delq 'difftastic-bindings-t-dummy features))
    (setq difftastic-bindings-t-keymap-1 nil)
    (put 'difftastic-bindings-t-keymap-1 'difftastic--installed nil)
    (setq difftastic-bindings-t-keymap-2 nil)
    (put 'difftastic-bindings-t-keymap-2 'difftastic--installed nil)))

(ert-deftest difftastic-bindings--after-load:no-feature ()
  (unwind-protect
      ;; Arrange
      (let ((difftastic-bindings-mode t)
            (difftastic-bindings-alist
             '((((prefixes . ((dummy-prefix-1 (-1 -1) difftastic-bindings-t-dummy)))
                 (keymaps . ((difftastic-bindings-t-keymap-1 . difftastic-bindings-t-dummy)
                             (difftastic-bindings-t-keymap-2 . difftastic-bindings-t-dummy))))
                .
                (("A" fun-a "Do A")))))
            (difftastic-bindings--after-load-alist
             (list
              (cons 'difftastic-bindings-t-dummy
                    '(:prefixes
                      (dummy-prefix-1)
                      :keymaps
                      (difftastic-bindings-t-keymap-1
                       difftastic-bindings-t-keymap-2
                       difftastic-bindings-t-keymap-3))))))
        ;; no feature (provide 'difftastic-bindings-t-dummy)
        (setq difftastic-bindings-t-keymap-1 (make-sparse-keymap))
        (setq difftastic-bindings-t-keymap-2 (make-sparse-keymap))
        (put 'difftastic-bindings-t-keymap-2 'difftastic--installed
             difftastic-bindings-alist)
        (eval
         ;; Expect
         `(mocklet ((difftastic-bindings--append-suffix not-called)
                    (difftastic-bindings--bind-keys not-called))
            ;; Act
            (difftastic-bindings--after-load
             "/a/path/difftastic-bindings-t-dummy.elc"))))
    ;; Cleanup
    (setq features (delq 'difftastic-bindings-t-dummy features))
    (setq difftastic-bindings-t-keymap-1 nil)
    (put 'difftastic-bindings-t-keymap-1 'difftastic--installed nil)
    (setq difftastic-bindings-t-keymap-2 nil)
    (put 'difftastic-bindings-t-keymap-2 'difftastic--installed nil)))

(ert-deftest difftastic-bindings--after-load:not-matching-prefix ()
  (unwind-protect
      ;; Arrange
      (let ((difftastic-bindings-mode t)
            (difftastic-bindings-alist
             '((((prefixes . ((dummy-prefix-1 (-1 -1) difftastic-bindings-t-dummy))))
                .
                (("A" fun-a "Do A")))))
            (difftastic-bindings--after-load-alist
             (list
              (cons 'difftastic-bindings-t-dummy
                    '(:prefixes
                      (dummy-prefix-2))))))
        (provide 'difftastic-bindings-t-dummy)
        (setq difftastic-bindings-t-keymap-1 (make-sparse-keymap))
        (setq difftastic-bindings-t-keymap-2 (make-sparse-keymap))
        (put 'difftastic-bindings-t-keymap-2 'difftastic--installed
             difftastic-bindings-alist)
        (eval
         ;; Expect
         `(mocklet ((difftastic-bindings--append-suffix not-called)
                    (difftastic-bindings--bind-keys not-called))
            ;; Act
            (difftastic-bindings--after-load
             "/a/path/difftastic-bindings-t-dummy.elc"))))
    ;; Cleanup
    (setq features (delq 'difftastic-bindings-t-dummy features))
    (setq difftastic-bindings-t-keymap-1 nil)
    (put 'difftastic-bindings-t-keymap-1 'difftastic--installed nil)
    (setq difftastic-bindings-t-keymap-2 nil)
    (put 'difftastic-bindings-t-keymap-2 'difftastic--installed nil)))


(ert-deftest difftastic-bindings-mode--turn-on:basic ()
  (unwind-protect
      ;; Arrange
      (let ((difftastic-bindings-alist
             '((((prefixes . ((dummy-prefix-1 (-1 -1) difftastic-bindings-t-dummy)
                              (dummy-prefix-2 (-1) difftastic-bindings-t-dummy-other)
                              (dummy-prefix-3 (-1) "difftastic-bindings-t-dummy-other")))
                 (keymaps . ((difftastic-bindings-t-keymap-1 . "difftastic-bindings-t-dummy")
                             (difftastic-bindings-t-keymap-2 . difftastic-bindings-t-dummy-other)
                             (difftastic-bindings-t-keymap-3 . difftastic-bindings-t-dummy))))
                . (("A" fun-a "Do A")))))
            difftastic-bindings--after-load-alist
            after-load-functions)
        (provide 'difftastic-bindings-t-dummy)
        (setq difftastic-bindings-t-keymap-1 (make-sparse-keymap))
        (setq difftastic-bindings-t-keymap-2 (make-sparse-keymap))
        (put 'difftastic-bindings-t-keymap-2 'difftastic--installed
             difftastic-bindings-alist)
        (eval
         ;; Expect
         `(mocklet (((difftastic-bindings--append-suffix
                      'dummy-prefix-1 '(-1 -1)
                      `,(difftastic-bindings--parse-bindings
                         '(("A" fun-a "Do A"))))
                     :times 1)
                    ((difftastic-bindings--bind-keys
                      'difftastic-bindings-t-keymap-1
                      '(("A" fun-a "Do A")))
                     :times 1))
            ;; Act
            (difftastic-bindings-mode--turn-on)))
        ;; Then
        (should (assq 'difftastic-bindings-t-dummy
                      difftastic-bindings--after-load-alist))
        (let ((prefixes-keymaps
               (alist-get 'difftastic-bindings-t-dummy
                          difftastic-bindings--after-load-alist)))
          (should-not (cl-set-exclusive-or (plist-get prefixes-keymaps
                                                      :prefixes)
                                           '(dummy-prefix-1)))
          (should-not (cl-set-exclusive-or (plist-get prefixes-keymaps
                                                      :keymaps)
                                           '(difftastic-bindings-t-keymap-1
                                             difftastic-bindings-t-keymap-3))))
        (should (assq 'difftastic-bindings-t-dummy
                      difftastic-bindings--after-load-alist))
        (let ((prefixes-keymaps
               (alist-get 'difftastic-bindings-t-dummy-other
                          difftastic-bindings--after-load-alist)))
          (should-not (cl-set-exclusive-or (plist-get prefixes-keymaps
                                                      :prefixes)
                                           '(dummy-prefix-2
                                             dummy-prefix-3)))
          (should-not (cl-set-exclusive-or (plist-get prefixes-keymaps
                                                      :keymaps)
                                           '(difftastic-bindings-t-keymap-2))))
        (should (memq 'difftastic-bindings--after-load after-load-functions)))
    ;; Cleanup
    (setq features (delq 'difftastic-bindings-t-dummy features))
    (setq difftastic-bindings-t-keymap-1 nil)
    (put 'difftastic-bindings-t-keymap-1 'difftastic--installed nil)
    (setq difftastic-bindings-t-keymap-2 nil)
    (put 'difftastic-bindings-t-keymap-2 'difftastic--installed nil)))


(ert-deftest difftastic-bindings-mode--turn-off:basic ()
  ;; Arrange
  (let ((after-load-functions '(something
                                difftastic-bindings--after-load
                                something-else))
        (difftastic-bindings--after-load-alist
         (list
          (cons 'dummy-feature
                '(:prefixes (dummy-prefix-1) :keymaps (dummy-keymap1)))))
        (difftastic-bindings--installed-plist
         '(:prefixes (dummy-prefix-1) :keymaps (dummy-keymap-1))))
    (eval
     ;; Expect
     `(mocklet (((difftastic-bindings--remove-suffix
                  'dummy-prefix-1)
                 :times 1)
                ((difftastic-bindings--unbind-keys
                  'dummy-keymap-1)
                 :times 1))
        ;; Act
        (difftastic-bindings-mode--turn-off)))
    ;; Then
    (should-not (cl-set-exclusive-or after-load-functions
                                     '(something something-else)))
    (should-not difftastic-bindings--after-load-alist)))


(ert-deftest difftastic-bindings-mode:basic ()
  ;; toggle twice, so it remains the same when running interactively
  (let ((difftastic-bindings-mode t))
    (eval '(mocklet (((difftastic-bindings-mode--turn-off) :times 1)
                     (difftastic-bindings-mode--turn-on not-called))
             (difftastic-bindings-mode 'toggle)))
    (eval '(mocklet (((difftastic-bindings-mode--turn-on) :times 1)
                     (difftastic-bindings-mode--turn-off not-called))
             (difftastic-bindings-mode 'toggle)))))


(provide 'difftastic-bindings.t)

;;; difftastic-bindings.t.el ends here
