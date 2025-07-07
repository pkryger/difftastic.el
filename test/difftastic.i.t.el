;;; difftastic.i.t.el --- Integration tests for difftastic -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when-let* ((dir (file-name-directory (or load-file-name
                                          byte-compile-current-file
                                          buffer-file-name))))
  (load-file (file-name-concat dir "undercover-init.el")))

(require 'ert)
(require 'ert-x)
(require 'difftastic)
(require 'cl-lib)


(ert-deftest difftastic--get-languages:integration ()
  (let ((languages (difftastic--get-languages)))
    (should languages)
    (should (listp languages))
    (should (member "Text" languages))
    (should (< 1 (length languages)))
    (should-not (cl-find-if-not #'stringp languages))
    (should (cl-every (lambda (language)
                        (< 0 (length language)))
                      languages))))

(ert-deftest difftastic--file-extension-for-mode:integration ()
  (should (equal ".c" (difftastic--file-extension-for-mode 'c-mode)))
  (should (equal ".el" (difftastic--file-extension-for-mode 'emacs-lisp-mode)))
  (should-not (difftastic--file-extension-for-mode 'difftastic-mode)))


(ert-deftest difftastic--run-command:integration ()
  (ert-with-test-buffer ()
    (let* (action-called
           (process (difftastic--run-command
                     (current-buffer)
                     '("difft" "--version")
                     (lambda ()
                       (setq action-called t)))))
      (unwind-protect
          (progn
            (should process)
            (with-timeout (5
                           (signal-process process 'SIGKILL)
                           (ert-fail "timeout"))
              (while (accept-process-output process)))
            (goto-char (point-min))
            (should (search-forward "Difftastic" nil t))
            (should action-called))
        (when (process-live-p process)
          (kill-process process))))))



(provide 'difftastic.i.t)

;;; difftastic.i.t.el ends here
