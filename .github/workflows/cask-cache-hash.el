;;; cask-cache-hash.el --- Calculate a hash for packages to be installed -*- lexical-binding: t -*-

;;; Commentary:
;;

(require 'cl-lib)
(require 'package)
(require 'cask)

;;; Code:

(defun cask-cache-hash--default-branch (url)
  "Return default branch name in repository at URL."
  (string-trim
   (replace-regexp-in-string
    (rx string-start
        "ref: refs/heads/"
        (group (one-or-more (not whitespace)))
        (one-or-more whitespace)
        "HEAD" (zero-or-more any))
    "\\1"
    (shell-command-to-string
     (format "git ls-remote --symref %s HEAD | grep -e '^ref:'| head -1" url)))))

(defun cask-cache-hash--commit-hash (url branch)
  "Return last commit hash on BRANCH in repository at URL."
  (string-trim
   (replace-regexp-in-string
    (rx (group (one-or-more hex))
        (zero-or-more any))
    "\\1"
    (shell-command-to-string
     (format "git ls-remote %s refs/heads/%s" url branch)))))

(defun cask-cache-hash--packages-to-install (bundle)
  "Return list of versioned dependencies that will be installed for BUNDLE."
  (cask--with-environment bundle
    :refresh t
    (let ((dependencies (cask--dependencies bundle)))
      (cl-sort
       (delq nil
             (mapcar (lambda (dependency)
                       (when-let* ((version
                                    (or
                                     (when-let* ((url (cask-dependency-url dependency))
                                                 (branch (or (cask-dependency-branch dependency)
                                                             (cask-cache-hash--default-branch url))))
                                       (cask-cache-hash--commit-hash url branch))
                                     (when-let* ((dependecy-name (cask-dependency-name dependency))
                                                 (desc (cadr (assq dependecy-name
                                                                   package-archive-contents)))
                                                 (ver (package-desc-version desc))
                                                 ((not (package-built-in-p dependecy-name ver))))
                                       (package-version-join ver)))))
                         (format "%s-%s" (cask-dependency-name dependency) version)))
                     dependencies))
       #'string<))))


(defun cask-cache-hash ()
  "Calculate hash for a cask install in current git repository.
The hash is a sha256 of all dependencies that would be installed from
archives, including their would be installed versions."
  (let* ((default-directory (string-trim
                             (shell-command-to-string
                              "git rev-parse --show-toplevel")))
         (bundle (cask-setup default-directory))
         (packages (cask-cache-hash--packages-to-install bundle)))
    (princ (secure-hash 'sha256 (format "%S"  packages)))))

(provide 'cask-cache-hash)

;;; cask-cache-hash.el ends here
