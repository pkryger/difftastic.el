;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Przemyslaw Kryger

;; Author: Przemyslaw Kryger <pkryger@gmail.com>
;; Keywords: tools diff
;; Homepage: https://github.com/pkryger/difftastic.el
;; Package-Requires: ((emacs "27.1") (compat "29.1.4.2") (magit "20220326"))
;; Version: 0.0.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The difftastic is designed to integrate difftastic - a structural diff tool
;; - (https://github.com/wilfred/difftastic) into your Emacs workflow,
;; enhancing your code review and comparison experience.  This package
;; automatically displays difftastic's output within Emacs using faces from
;; your user theme, ensuring consistency with your overall coding environment.
;;
;; Configuration
;;
;; To configure the `difftastic` commands in `magit-diff` prefix, use the
;; following code snippet in your Emacs configuration:
;;
;; (require 'difftastic)
;;
;; ;; Add commands to a `magit-difftastic'
;; (eval-after-load 'magit-diff
;;   '(transient-append-suffix 'magit-diff '(-1 -1)
;;      [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
;;       ("S" "Difftastic show" difftastic-magit-show)]))
;; (add-hook 'magit-blame-read-only-mode-hook
;;           (lambda ()
;;             (kemap-set magit-blame-read-only-mode-map
;;                        "D" #'difftastic-magit-show)
;;             (kemap-set magit-blame-read-only-mode-map
;;                        "S" #'difftastic-magit-show)))
;;
;; Or, if you use `use-package':
;;
;; (use-package difftastic
;;   :bind (:map magit-blame-read-only-mode-map
;;          ("D" . difftastic-magit-show)
;;          ("S" . difftastic-magit-show))
;;   :config
;;   (eval-after-load 'magit-diff
;;     '(transient-append-suffix 'magit-diff '(-1 -1)
;;        [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
;;         ("S" "Difftastic show" difftastic-magit-show)])))
;;
;; Usage
;;
;; There are four commands to interact with difftastic:
;;
;; - `difftastic-magit-diff' - show the result of 'git diff ARG' with
;;   difftastic.  It tries to guess ARG, and ask for it when can't. When called
;;   with prefix argument it will ask for ARG.
;;
;; - `difftastic-magit-show' - show the result of 'git show ARG' with
;;   difftastic.  It tries to guess ARG, and ask for it when can't. When called
;;   with prefix argument it will ask for ARG.
;;
;; - `difftastic-files' - show the result of 'difft FILE-A FILE-B'.  When
;;   called with prefix argument it will ask for language to use, instead of
;;   relaying on difftastic's detection mechanism.
;;
;; - `difftastic-buffers' - show the result of 'difft BUFFER-A BUFFER-B'.
;;   Language is guessed based on buffers modes.  When called with prefix
;;   argument it will ask for language to use.

;;; Code:

(require 'ansi-color)
(require 'cl-lib)
(require 'ediff)
(require 'font-lock)
(require 'magit)
(require 'view)

(eval-when-compile
  (require 'compat)
  (require 'fringe))

(defgroup difftastic nil
  "Integration with difftastic."
  :group 'tools)

(defun difftastic--ansi-color-face (vector offset name)
  "Get face from VECTOR with OFFSET or make a new one with NAME suffix.
New face is made when VECTOR is not bound."
  ;;This is for backward compatibility with Emacs-27.  When dropping
  ;; compatibility, calls should be replaced with `(aref VECTOR offset)'.
  (if (version< emacs-version "28")
      (custom-declare-face
       `,(intern (concat "difftastic--ansi-color-" name))
       `((t :foreground ,(cdr (aref (with-no-warnings
                                      (ansi-color-make-color-map))
                                    (+ 30 offset)))))
       (concat "Face used to render " name " color code.")
       :group 'difftastic)
    (aref (eval vector) offset)))

(defun difftastic-requested-window-width ()
  "Get a window width for difftastic call."
  (- (if (< 1 (count-windows))
         (save-window-excursion
           (other-window 1)
           (window-width))
       (if (and split-width-threshold
                (< split-width-threshold (window-width)))
           (/ (window-width) 2)
         (window-width)))
     (fringe-columns 'left)
     (fringe-columns 'rigth)))

(defun difftastic-pop-to-buffer (buffer-or-name requested-width)
  "Display BUFFER-OR-NAME with REQUESTED-WIDTH and select its window.
When actual window width is greater than REQUESTED-WIDTH then
display buffer at bottom."
  (with-current-buffer buffer-or-name
    ;; difftastic diffs are usually 2-column side-by-side,
    ;; so ensure our window is wide enough.
    (let ((actual-width (if (fboundp 'buffer-line-statistics)
                            ;; since Emacs-28
                            (cadr (buffer-line-statistics))
                          (save-excursion
                            (goto-char (point-min))
                            (let ((max 0)
                                  (to (point-max)))
                              (while (< (point) to)
                                (end-of-line)
                                (setq max (max max (current-column)))
                                (forward-line))
                              max)))))
      (pop-to-buffer
       (current-buffer)
       `(,(when (< requested-width actual-width)
            #'display-buffer-at-bottom))))))

(defcustom difftastic-executable "difft"
  "Location of difftastic executable."
  :type 'file
  :group 'difftastic)

(defcustom difftastic-normal-colors-vector
  (vector
   (difftastic--ansi-color-face 'ansi-color-normal-colors-vector 0 "black")
   'magit-diff-removed
   'magit-diff-added
   'magit-diff-file-heading
   font-lock-comment-face
   font-lock-string-face
   font-lock-warning-face
   (difftastic--ansi-color-face 'ansi-color-normal-colors-vector 7 "white"))
  "Faces to use for colors on difftastic output (normal).

N.B. only foreground and background properties will be used."
  :type '(vector face face face face face face face face)
  :group 'difftastic)

(defcustom difftastic-bright-colors-vector
  (vector
   (difftastic--ansi-color-face 'ansi-color-bright-colors-vector 0 "black")
   'magit-diff-removed
   'magit-diff-added
   'magit-diff-file-heading
   font-lock-comment-face
   font-lock-string-face
   font-lock-warning-face
   (difftastic--ansi-color-face 'ansi-color-bright-colors-vector 7 "white"))
  "Faces to use for colors on difftastic output (bright).

N.B. only foreground and background properties will be used."
  :type '(vector face face face face face face face face)
  :group 'difftastic)

(defcustom difftastic-highlight-alist
  '((magit-diff-added . magit-diff-added-highlight)
    (magit-diff-removed . magit-diff-removed-highlight))
  "Faces to replace underlined highlight in difftastic output.
This is an alist, where each association defines a mapping
between a non-highlighted face to a highlighted face.  Set to nil if
you prefer unaltered difftastic output.

N.B. only foreground and background properties will be used."
  :type '(alist :key-type face :value-type face)
  :group 'difftastic)

(defcustom difftastic-requested-window-width-function
  #'difftastic-requested-window-width
  "Function used to calculate a requested width for difftastic call."
  :type 'function
  :group 'difftastic)

(defcustom difftastic-display-buffer-function
  #'difftastic-pop-to-buffer
  "Function used diplay buffer with output of difftastic call.
It will be called with two arguments: BUFFER-OR-NAME: a buffer to
display and REQUESTED-WIDTH: a with requested for difftastic
call."
  :type 'function
  :group 'difftastic)

(defmacro difftastic--with-temp-advice (symbol how function &rest body)
  ;; checkdoc-params: (symbol how function)
  "Execute BODY with advice temporarily enabled.
See `advice-add' for explanation of SYMBOL, HOW, and FUNCTION arguments."
  (declare (indent 3))
  `(let ((fn-advice-var ,function))
     (unwind-protect
         (progn
           (advice-add ,symbol ,how fn-advice-var)
           ,@body)
       (advice-remove ,symbol fn-advice-var))))

(define-derived-mode difftastic-mode fundamental-mode "difftastic"
  "Major mode to display output of difftastic.

It uses `view-mode' to provide a familiar behaviour to view diffs."
  :group 'difftastic
  (view-mode)
  (setq buffer-read-only t))

(defun difftastic--copy-tree (tree)
  "Make a copy of TREE.
If TREE is a cons cell, this recursively copies both its car and
its cdr.  Contrast to `copy-sequence', which copies only along
the cdrs.  This copies vectors and bool vectors as well as
conses."
  ;; adapted from `copy-tree'
  (if (consp tree)
      (let (result)
        (while (consp tree)
          (let ((newcar (car tree)))
            (when (or (consp newcar)
                      (or (vectorp newcar)
                          (bool-vector-p newcar)))
              (setq newcar (difftastic--copy-tree newcar)))
            (push newcar result))
          (setq tree (cdr tree)))
        (nconc (nreverse result)
               (if (or (vectorp tree)
                       (bool-vector-p tree))
                   (difftastic--copy-tree tree)
                 tree)))
    (cond
     ((vectorp tree)
      (let ((i (length (setq tree (copy-sequence tree)))))
	    (while (>= (setq i (1- i)) 0)
	      (aset tree i (difftastic--copy-tree (aref tree i))))
	    tree))
     ;; Optimisation: bool vector doesn't need a deep copy
     ((bool-vector-p tree)
      (copy-sequence tree))
     (t tree))))

(defun difftastic--ansi-color-add-background (face)
  "Add :background to FACE.
N.B.  This is meant to filter-result of either
`ansi-color--face-vec-face' or `ansi-color-get-face-1' by
adding background to faces if they have a foreground set."
  (if-let ((difftastic-face
            (and (listp face)
                 (cl-find-if
                  (lambda (difftastic-face)
                    (and (string=
                          (face-foreground difftastic-face)
                          (or
                           (plist-get face :foreground)
                           (plist-get
                            (cl-find-if (lambda (elt)
                                          (and (listp elt)
                                               (plist-get elt :foreground)))
                                        face)
                            :foreground)))
                         ;; ansi-color-* faces have the same
                         ;; foreground and background - don't use them
                         (not (string= (face-foreground difftastic-face)
                                       (face-background difftastic-face)))
                         (face-background difftastic-face)))
                  (vconcat difftastic-normal-colors-vector
                           difftastic-bright-colors-vector)))))
      ;; difftastic uses underline to highlight some changes;
      ;; it uses bold as well, but it's not as unambiguous as underline
      (if-let ((highlight-face (and (cl-member 'ansi-color-underline face)
                                    (alist-get difftastic-face
                                               difftastic-highlight-alist))))
          (append (cl-remove-if (lambda (elt)
                                  (and (listp elt)
                                       (plist-get elt :foreground)))
                                (cl-remove 'ansi-color-underline
                                           (cl-remove 'ansi-color-bold face)))
                  (list :background
                        (face-background highlight-face nil 'default))
                  (list :foreground
                        (face-foreground highlight-face nil 'default)))
        (append face
                (list :background
                      (face-background difftastic-face nil 'default))))
    face))

;; In practice there are only dozens or so different faces used,
;; so we can cache them each time anew.
(defvar-local difftastic--ansi-color-add-background-cache nil)

(defun difftastic--ansi-color-add-background-cached (orig-fun face-vec)
  "Memoise ORIG-FUN based on FACE-VEC.
Utilise `difftastic--ansi-color-add-background-cache' to cache
`ansi-color--face-vec-face' calls."
  (if-let ((cached (assoc face-vec
                          difftastic--ansi-color-add-background-cache)))
      (cdr cached)
    (let ((face (difftastic--ansi-color-add-background
                 (funcall orig-fun face-vec))))
      (push (cons (difftastic--copy-tree face-vec) face)
            difftastic--ansi-color-add-background-cache)
      face)))

(defun difftastic--magit-with-difftastic (buffer command)
  "Run COMMAND with GIT_EXTERNAL_DIFF then show result in BUFFER."
  (let* ((requested-width (funcall difftastic-requested-window-width-function))
         (process-environment
          (cons (format "GIT_EXTERNAL_DIFF=%s --width %s --background %s"
                        difftastic-executable
                        requested-width
                        (frame-parameter nil 'background-mode))
                process-environment)))
    (difftastic--run-command
     buffer
     command
     (lambda ()
       (funcall difftastic-display-buffer-function buffer requested-width)))))

(defun difftastic--run-command (buffer command action)
  "Run COMMAND, show its results in BUFFER, then execute ACTION.
The ACTION is meant to display the BUFFER in some window and, optionally,
perform cleanup."
  ;; Clear the result buffer (we might regenerate a diff, e.g., for
  ;; the current changes in our working directory).
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)))
  ;; Now spawn a process calling the git COMMAND.
  (message "Running: %s..." (mapconcat #'identity command " "))
  (make-process
   :name (buffer-name buffer)
   :buffer buffer
   :command command
   :noquery t
   :filter
   ;; Apply ANSI color sequences as they come
   (lambda (process string)
     (when-let ((buffer (and string
                             (process-buffer process))))
       (with-current-buffer buffer
         (let ((inhibit-read-only t)
               (ansi-color-normal-colors-vector
                difftastic-normal-colors-vector)
               (ansi-color-bright-colors-vector
                difftastic-bright-colors-vector))
           (ignore ansi-color-normal-colors-vector
                   ansi-color-bright-colors-vector)
           (if (fboundp 'ansi-color--face-vec-face) ;; since Emacs-29
               (difftastic--with-temp-advice
                   'ansi-color--face-vec-face
                   :around
                   #'difftastic--ansi-color-add-background-cached
                 (insert (ansi-color-apply string)))
             (difftastic--with-temp-advice
                 'ansi-color-get-face-1
                 :filter-return
                 #'difftastic--ansi-color-add-background
               (insert (ansi-color-apply string))))))))
   ;; Disable write access and call `action' when process is finished.
   :sentinel
   (lambda (proc _event)
     (let (output)
       (when (eq (process-status proc) 'exit)
         (with-current-buffer (process-buffer proc)
           (difftastic-mode)
           (goto-char (point-min))
           (setq output (not (eq (point-min) (point-max)))))
         (if output
             (progn
               (funcall action)
               (message nil))
           (message "Process %s returned no output"
                    (mapconcat #'identity command " "))))))))

;;;###autoload
(defun difftastic-magit-show (rev)
  "Show the result of \\='git show REV\\=' with difftastic.
When REV couldn't be guessed or called with prefix arg ask for REV."
  (interactive
   (list (or
          ;; If not invoked with prefix arg, try to guess the REV from
          ;; point's position.
          (and (not current-prefix-arg)
               (or (magit-thing-at-point 'git-revision t)
                   (magit-branch-or-commit-at-point)))
          ;; Otherwise, query the user.
          (magit-read-branch-or-commit "Revision"))))
  (if (not rev)
      (error "No revision specified")
    (difftastic--magit-with-difftastic
     (get-buffer-create (concat "*difftastic git show " rev "*"))
     (list "git" "--no-pager" "show" "--ext-diff" rev))))

;;;###autoload
(defun difftastic-magit-diff (arg)
  "Show the result of \\='git diff ARG\\=' with difftastic.
When ARG couldn't be guessed or called with prefix arg ask for ARG."
  (interactive
   (list (or
          (and current-prefix-arg
               (magit-diff-read-range-or-commit "Range/Commit"))
          ;; Otherwise, auto-guess based on position of point, e.g., in a file
          ;; or based on the Staged or Unstaged section.
          (when-let ((file (magit-file-relative-name)))
            (save-buffer)
            file)
          (pcase (magit-diff--dwim)
            ('unmerged (error "Unmerged is not yet implemented"))
            ('unstaged nil)
            ('staged "--cached")
            (`(stash . ,_) (error "Stash is not yet implemented"))
            (`(commit . ,value) (format "%s^..%s" value value))
            ((and range (pred stringp)) range)
            (_ (magit-diff-read-range-or-commit "Range/Commit"))))))
  (let* ((name (concat "*difftastic git diff"
                       (if arg (concat " " arg) "")
                       "*"))
         (file (magit-file-relative-name))
         (default-directory (if file
                                (magit-toplevel)
                              default-directory)))
    (difftastic--magit-with-difftastic
     (get-buffer-create name)
     `("git" "--no-pager" "diff" "--ext-diff"
       ,@(when file (list "--"))
       ,@(when arg (list arg))))))

(defun difftastic---make-temp-file (prefix buffer)
  "Make a temp file for BUFFER content that with PREFIX included in file name."
  ;; adapted from `make-auto-save-file-name'
  (with-current-buffer buffer
    (let ((buffer-name (buffer-name))
          (limit 0))
      (while (string-match "[^A-Za-z0-9_.~#+-]" buffer-name limit)
        (let* ((character (aref buffer-name (match-beginning 0)))
               (replacement
                ;; For multibyte characters, this will produce more than
                ;; 2 hex digits, so is not true URL encoding.
                (format "%%%02X" character)))
          (setq buffer-name (replace-match replacement t t buffer-name))
          (setq limit (1+ (match-end 0)))))
      (make-temp-file (format "difftastic-%s-%s-" prefix buffer-name)
                      nil nil (buffer-string)))))

(defun difftastic--get-file (prefix buffer)
  "If BUFFER visits a file return it else create a temporary file with PREFIX.
The return value is a cons where car is the file and cdr is non
nil if a temporary file has been created."
  (let* (temp
         (file
          (if-let ((buffer-file (buffer-file-name buffer)))
              (progn
                (save-buffer buffer)
                buffer-file)
            (setq temp
                  (difftastic---make-temp-file prefix buffer)))))
    (cons file temp)))

(defun difftastic--delete-temp-file (file-temp)
  "Delete FILE-TEMP when it is a temporary file.
The FILE-TEMP is a cons where car is the file and cdr is non nil
when it is a temporary file."
  (let ((file (car file-temp))
        (temp (cdr file-temp)))
    (when (and temp (stringp file) (file-exists-p file))
      (delete-file file))))

(defun difftastic--languages ()
  "Return list of language overrides supported by difftastic."
  (append
   '("Text")
   (cl-remove-if (lambda (line)
                   (string-match-p "^ \\*" line))
                 (compat-call ;; since Emacs-29
                  string-split
                  (shell-command-to-string
                   (concat difftastic-executable " --list-languages"))
                  "\n" t))))

(defun difftastic--make-suggestion (languages buffer-A buffer-B)
  "Suggest one of LANGUAGES based on mode of BUFFER-A and BUFFER-B."
  (when-let ((mode
              (or (with-current-buffer buffer-A
                    (when (derived-mode-p 'prog-mode)
                      major-mode))
                  (with-current-buffer buffer-B
                    (when (derived-mode-p 'prog-mode)
                      major-mode)))))
    (cl-find-if (lambda (language)
                  (string= (downcase language)
                           (downcase (compat-call ;; since Emacs-28
                                      string-replace
                                      "-" " "
                                      (replace-regexp-in-string
                                       "-mode$" ""
                                       (symbol-name mode))))))
                languages)))

(defun difftastic--files-internal (buffer file-temp-A file-temp-B &optional lang-override)
  "Run difftastic on files FILE-TEMP-A and FILE-TEMP-B and show results in BUFFER.
The FILE-TEMP-A and FILE-TEMB-B are conses where car is the file
and cdr is non nil when it is a temporary file.  LANG-OVERRIDE is
passed to difftastic as \\='--override\\=' argument."
  (let ((requested-width (funcall difftastic-requested-window-width-function)))
    (difftastic--run-command
     buffer
     `(,difftastic-executable
       "--width" ,(number-to-string requested-width)
       "--background" ,(format "%s" (frame-parameter nil 'background-mode))
       ,@(when lang-override (list "--override"
                                   (format "*:%s" lang-override)))
       ,(car file-temp-A)
       ,(car file-temp-B))
     (lambda ()
       (funcall difftastic-display-buffer-function buffer requested-width)
       (difftastic--delete-temp-file file-temp-A)
       (difftastic--delete-temp-file file-temp-B)))))

;;;###autoload
(defun difftastic-buffers (buffer-A buffer-B &optional lang-override)
  "Run difftastic on a pair of buffers, BUFFER-A and BUFFER-B.
Optionally, provide a LANG-OVERRIDE to override language used.
See \\='difft --list-languages\\=' for language list.

When:
- either LANG-OVERRIDE is nil and neither of BUFFER-A nor
BUFFER-B is a file buffer,
- or function is called with a prefix arg,

then ask for language before running difftastic."
  ;; adapted from `ediff-buffers'
  (interactive
   (let (bf-A bf-B)
     (list (setq bf-A (read-buffer "Buffer A to compare: "
                                   (ediff-other-buffer "") t))
           (setq bf-B (read-buffer "Buffer B to compare: "
                                   (progn
                                     ;; realign buffers so that two visible
                                     ;; buffers will be at the top
                                     (save-window-excursion (other-window 1))
                                     (ediff-other-buffer bf-A))
                                   t))
           (when (or current-prefix-arg
                     (and (not (buffer-file-name (get-buffer bf-A)))
                          (not (buffer-file-name (get-buffer bf-B)))))
             (let* ((languages (difftastic--languages))
                    (suggested (difftastic--make-suggestion
                                languages
                                (get-buffer bf-A)
                                (get-buffer bf-B))))
               (completing-read "Language: " languages nil t suggested))))))

  (let (file-temp-A file-temp-B)
    (condition-case err
        (progn
          (setq file-temp-A (difftastic--get-file "A" (get-buffer buffer-A))
                file-temp-B (difftastic--get-file "B" (get-buffer buffer-B)))
          (difftastic--files-internal
           (get-buffer-create
            (concat "*difftastic " buffer-A " " buffer-B "*"))
           file-temp-A
           file-temp-B
           lang-override))
      ((error debug)
       (difftastic--delete-temp-file file-temp-A)
       (difftastic--delete-temp-file file-temp-B)
       (signal (car err) (cdr err))))))

;;;###autoload
(defun difftastic-files (file-A file-B &optional lang-override)
  "Run difftastic on a pair of files, FILE-A and FILE-B.
Optionally, provide a LANG-OVERRIDE to override language used.
See \\='difft --list-languages\\=' for language list.  When
function is called with a prefix arg then ask for language before
running difftastic."
  ;; adapted from `ediff-files'
  (interactive
   (let ((dir-A (if ediff-use-last-dir
                    ediff-last-dir-A
                  default-directory))
         dir-B f)
     (list (setq f (ediff-read-file-name
                    "File A to compare"
                    dir-A
                    (ediff-get-default-file-name)
                    'no-dirs))
           (ediff-read-file-name "File B to compare"
                                 (setq dir-B
                                       (if ediff-use-last-dir
                                           ediff-last-dir-B
                                         (file-name-directory f)))
                                 (progn
                                   (add-to-history
                                    'file-name-history
                                    (ediff-abbreviate-file-name
                                     (expand-file-name
                                      (file-name-nondirectory f)
                                      dir-B)))
                                   (ediff-get-default-file-name f 1)))
           (when current-prefix-arg
             (completing-read "Language: " (difftastic--languages) nil t)))))
  (difftastic--files-internal
   (get-buffer-create (concat "*difftastic "
                              (file-name-nondirectory file-A)
                              " "
                              (file-name-nondirectory file-B)
                              "*"))
   (cons file-A nil)
   (cons file-B nil)
   lang-override))

(provide 'difftastic)
;;; difftastic.el ends here
