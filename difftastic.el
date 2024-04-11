;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Przemyslaw Kryger

;; Author: Przemyslaw Kryger <pkryger@gmail.com>
;; Keywords: tools diff
;; Homepage: https://github.com/pkryger/difftastic.el
;; Package-Requires: ((emacs "28.1") (compat "29.1.4.2") (magit "20220326"))
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

;; Description
;; ===========
;;
;; The `difftastic' Emacs package is designed to integrate [difftastic] - a
;; structural diff tool - into your Emacs workflow, enhancing your code review
;; and comparison experience.  This package automatically displays
;; `difftastic''s output within Emacs using faces from your user theme,
;; ensuring consistency with your overall coding environment.
;;
;;
;; [difftastic] <https://github.com/wilfred/difftastic>
;;
;;
;; Features
;; ========
;;
;; - Configure faces to your likening.  By default `magit-diff-*' faces from
;;   your user them are used for consistent visual experience.
;; - Chunks and file navigation using `n' / `N' and `p' / `P' in generated
;;   diffs.
;; - DWIM workflows from `magit'.
;; - Rerun `difftastic' with `g' to use current window width to "reflow"
;;   content and/or to force language change (when called with prefix).
;;
;;
;; Installation
;; ============
;;
;; Installing from MELPA
;; ~~~~~~~~~~~~~~~~~~~~~
;;
;; The easiest way to install and keep `difftastic' up-to-date is using Emacs'
;; built-in package manager.  `difftastic' is available in the MELPA
;; repository.  Refer to <https://melpa.org/#/getting-started> for how to
;; install a package from MELPA.
;;
;;
;; Installing from GitHub
;; ~~~~~~~~~~~~~~~~~~~~~~
;;
;; 1. Clone this repository to a directory of your choice.
;; 2. Add the following lines to your Emacs configuration file (usually
;;    `~/.emacs' or `~/.emacs.d/init.el'):
;;
;; (add-to-list 'load-path "/path/to/difftastic.el")
;; (require 'difftastic)
;;
;;
;; Configuration
;; =============
;;
;; To configure `difftastic' commands in `magit-diff' prefix, use the
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
;;             (keymap-set magit-blame-read-only-mode-map
;;                         "D" #'difftastic-magit-show)
;;             (keymap-set magit-blame-read-only-mode-map
;;                         "S" #'difftastic-magit-show)))
;;
;; Or, if you use `use-package':
;;
;; (use-package difftastic
;;   :demand t
;;   :bind (:map magit-blame-read-only-mode-map
;;          ("D" . difftastic-magit-show)
;;          ("S" . difftastic-magit-show))
;;   :config
;;   (eval-after-load 'magit-diff
;;     '(transient-append-suffix 'magit-diff '(-1 -1)
;;        [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
;;         ("S" "Difftastic show" difftastic-magit-show)])))
;;
;;
;; Usage
;; =====
;;
;; The following commands are meant to help to interact with `difftastic'.
;; Commands are followed by their default keybindings in `difftastic-mode' (in
;; parenthesis).
;;
;; - `difftastic-magit-diff' - show the result of `git diff ARGS -- FILES'
;;   with `difftastic'.  This is the main entry point for DWIM action, so it
;;   tries to guess revision or range.
;; - `difftastic-magit-show' - show the result of `git show ARG' with
;;   `difftastic'.  It tries to guess `ARG', and ask for it when can't. When
;;   called with prefix argument it will ask for `ARG'.
;; - `difftastic-files' - show the result of `difft FILE-A FILE-B'.  When
;;   called with prefix argument it will ask for language to use, instead of
;;   relaying on `difftastic''s detection mechanism.
;; - `difftastic-buffers' - show the result of `difft BUFFER-A BUFFER-B'.
;;   Language is guessed based on buffers modes.  When called with prefix
;;   argument it will ask for language to use.
;; - `difftastic-dired-diff' - same as `dired-diff', but with
;;   `difftastic-files' instead of the built-in `diff'.
;; - `difftastic-rerun' (`g') - rerun difftastic for the current buffer.  It
;;   runs difftastic again in the current buffer, but respects the window
;;   configuration.  It uses
;;   `difftastic-rerun-requested-window-width-function' which, by default,
;;   returns current window width (instead of
;;   `difftastic-requested-window-width-function').  It will also reuse
;;   current buffer and will not call `difftastic-display-buffer-function'.
;;   When called with prefix argument it will ask for language to use.
;; - `difftastic-next-chunk' (`n'), `difftastic-next-file' (`N') - move point
;;   to a next logical chunk or a next file respectively.
;; - `difftastic-previous-chunk' (`p'), `difftastic-previous-file' (`P') -
;;   move point to a previous logical chunk or a previous file respectively.
;; - `difftastic-git-diff-range' - transform `ARGS' for difftastic and show
;;   the result of `git diff ARGS REV-OR-RANGE -- FILES' with `difftastic'.
;;
;;
;; Customization
;; =============
;;
;; Face Customization
;; ~~~~~~~~~~~~~~~~~~
;;
;; You can customize the appearance of `difftastic' output by adjusting the
;; faces used for highlighting.  To customize a faces, use the following code
;; snippet in your configuration:
;;
;; ;; Customize faces used to display difftastic output.
;; (setq difftastic-normal-colors-vector
;;   (vector
;;    ;; use black face from `ansi-color'
;;    (aref ansi-color-normal-colors-vector 0)
;;    ;; use face for removed marker from `difftastic'
;;    (aref difftastic-normal-colors-vector 1)
;;    ;; use face for added marker from `difftastic'
;;    (aref difftastic-normal-colors-vector 2)
;;    'my-section-face
;;    'my-comment-face
;;    'my-string-face
;;    'my-warning-face
;;    ;; use white face from `ansi-color'
;;    (aref ansi-color-normal-colors-vector 7)))
;;
;; ;; Customize highlight faces
;; (setq difftastic-highlight-alist
;;   `((,(aref difftastic-normal-colors-vector 2) . my-added-highlight)
;;     (,(aref difftastic-normal-colors-vector 1) . my-removed-highlight)))
;;
;; ;; Disable highlight faces (use difftastic's default)
;; (setq difftastic-highlight-alist nil)
;;
;;
;; Window management
;; ~~~~~~~~~~~~~~~~~
;;
;; The `difftastic' relies on the `difft' command line tool to produce an
;; output that can be displayed in an Emacs buffer window.  In short: it runs
;; the `difft', converts ANSI codes into user defined colors and displays it
;; in window.  The `difft' can be instructed with a hint to help it produce a
;; content that can fit into user output, by specifying a requested width.
;; However, the latter is not always respected.
;;
;; The `difftastic' provides a few variables to let you customize these
;; aspects of interaction with `difft':
;; - `difftastic-requested-window-width-function' - this function is called
;;   for a first (i.e., not a rerun) call to `difft'.  It shall return the
;;   requested width of the output.  For example this can be a half of a
;;   current frame (or a window) if the output is meant to be presented side
;;   by side.
;; - `difftastic-rerun-requested-window-width-function' - this function is
;;   called for a rerun (i.e., not a first) call to `difft'.  It shall return
;;   requested window width of the output.  For example this can be a current
;;   window width if the output is meant to fill the whole window.
;; - `difftastic-display-buffer-function' - this function is called after a
;;   first call to `difft'.  It is meant to select an appropriate Emacs
;;   mechanism to display the `difft' output.
;;
;;
;; Contributing
;; ============
;;
;; Contributions are welcome! Feel free to submit issues and pull requests on
;; the [GitHub repository].
;;
;;
;; [GitHub repository] <https://github.com/pkryger/difftastic.el>
;;
;; Testing
;; ~~~~~~~
;;
;; When creating a pull request make sure all tests in
;; <file:test/difftastic.t.el> are passing.  When adding a new functionality,
;; please strive to add tests for it as well.
;;
;; To run tests:
;; - open the <file:test/difftastic.t.el>
;; - type `M-x eval-buffer <RET>'
;; - type `M-x ert <RET> t <RET>'

;;; Code:

(require 'ansi-color)
(require 'cl-lib)
(require 'dired)
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

(defun difftastic-requested-window-width ()
  "Get a window width for a first difftastic call.
It returns a number that will let difftastic to fit content
into (in the following order):
 - other window if it exists,
 - side by side by inspecting `split-width-threshold',
 - current window."
  (- (if (< 1 (count-windows))
         (save-window-excursion
           (other-window 1)
           (window-width))
       (if (and split-width-threshold
                (< split-width-threshold (- (window-width)
                                            (fringe-columns 'left)
                                            (fringe-columns 'right))))
           (/ (window-width) 2)
         (window-width)))
     (fringe-columns 'left)
     (fringe-columns 'rigth)))

(defun difftastic-rerun-requested-window-width ()
  "Get a window width for a rerun of a difftastic call.
It returns the current window width, to let difftastic fit content into it."
  (- (window-width)
     (fringe-columns 'left)
     (fringe-columns 'right)))

(defun difftastic-pop-to-buffer (buffer-or-name requested-width)
  "Display BUFFER-OR-NAME with REQUESTED-WIDTH and select its window.
When actual window width is greater than REQUESTED-WIDTH then
display buffer at bottom."
  (with-current-buffer buffer-or-name
    ;; difftastic diffs are usually 2-column side-by-side,
    ;; so ensure our window is wide enough.
    (let ((actual-width (cadr (buffer-line-statistics))))
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
   (aref ansi-color-normal-colors-vector 0)
   'magit-diff-removed
   'magit-diff-added
   'magit-diff-file-heading
   'font-lock-comment-face
   'font-lock-string-face
   'font-lock-warning-face
   (aref ansi-color-normal-colors-vector 7))
  "Faces to use for colors on difftastic output (normal).

N.B. only foreground and background properties will be used."
  :type '(vector face face face face face face face face)
  :group 'difftastic)

(defcustom difftastic-bright-colors-vector
  (vector
   (aref ansi-color-bright-colors-vector 0)
   'magit-diff-removed
   'magit-diff-added
   'magit-diff-file-heading
   'font-lock-comment-face
   'font-lock-string-face
   'font-lock-warning-face
   (aref ansi-color-bright-colors-vector 7))
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
  "Function used to calculate a requested width for a first difftastic call."
  :type 'function
  :group 'difftastic)

(defcustom difftastic-rerun-requested-window-width-function
  #'difftastic-rerun-requested-window-width
  "Function used to calculate a requested width for a rerun of a difftastic call.
When it is set to nil, the value of
`difftastic-requested-window-width-function' is used."
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

(defcustom difftastic-exits-all-viewing-windows nil
  "Non-nil means restore all windows used to view buffer.
Commands that restore windows when finished viewing a buffer,
apply to all windows that display the buffer and have restore
information.  If `difftastic-exits-all-viewing-windows' is nil, only
the selected window is considered for restoring."
  :type 'boolean
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

(defun difftastic-next-file ()
  "Move to the next file."
  (interactive nil difftastic-mode)
  (if-let ((next (difftastic--next-chunk t)))
      (goto-char next)
    (user-error "No more files")))

(defun difftastic-next-chunk ()
  "Move to the next chunk."
  (interactive nil difftastic-mode)
  (if-let ((next (difftastic--next-chunk)))
      (goto-char next)
    (user-error "No more chunks")))

(defun difftastic-previous-file ()
  "Move to the previous file."
  (interactive nil difftastic-mode)
  (if-let ((previous (difftastic--prev-chunk t)))
      (goto-char previous)
    (user-error "No more files")))

(defun difftastic-previous-chunk ()
  "Move to the previous chunk."
  (interactive nil difftastic-mode)
  (if-let ((previous (difftastic--prev-chunk)))
      (goto-char previous)
    (user-error "No more chunks")))

(compat-call ;; since Emacs-29
 defvar-keymap difftastic-mode-map
  :doc "Keymap for `difftastic-mode'."
  "n"     #'difftastic-next-chunk
  "N"     #'difftastic-next-file
  "p"     #'difftastic-previous-chunk
  "P"     #'difftastic-previous-file
  "g"     #'difftastic-rerun
  ;; some keys from `view-mode'
  "C"     #'difftastic-quit-all
  "c"     #'difftastic-leave
  "Q"     #'difftastic-quit-all
  "e"     #'difftastic-leave
  "q"     #'difftastic-quit
  ;; "?"  #'View-search-regexp-backward ; Less does this.
  "\\"    #'View-search-regexp-backward
  "/"     #'View-search-regexp-forward
  "r"     #'isearch-backward
  "s"     #'isearch-forward
  "m"     #'point-to-register
  "'"     #'register-to-point
  "x"     #'exchange-point-and-mark
  "@"     #'View-back-to-mark
  "."     #'set-mark-command
  "%"     #'View-goto-percent
  "G"     #'View-goto-line
  "="     #'what-line
  "F"     #'View-revert-buffer-scroll-page-forward
  "y"     #'View-scroll-line-backward
  "C-j"   #'View-scroll-line-forward
  "RET"   #'View-scroll-line-forward
  "u"     #'View-scroll-half-page-backward
  "d"     #'View-scroll-half-page-forward
  "z"     #'View-scroll-page-forward-set-page-size
  "w"     #'View-scroll-page-backward-set-page-size
  "DEL"   #'View-scroll-page-backward
  "SPC"   #'View-scroll-page-forward
  "S-SPC" #'View-scroll-page-backward
  "o"     #'View-scroll-to-buffer-end
  ">"     #'end-of-buffer
  "<"     #'beginning-of-buffer
  "-"     #'negative-argument
  "9"     #'digit-argument
  "8"     #'digit-argument
  "7"     #'digit-argument
  "6"     #'digit-argument
  "5"     #'digit-argument
  "4"     #'digit-argument
  "3"     #'digit-argument
  "2"     #'digit-argument
  "1"     #'digit-argument
  "0"     #'digit-argument
  "H"     #'describe-mode
  "?"     #'describe-mode	; Maybe do as less instead? See above.
  "h"     #'describe-mode)

(define-derived-mode difftastic-mode fundamental-mode "difftastic"
  "Major mode to display output of difftastic.
It uses many keybindings from `view-mode' to provide a familiar
behaviour to view diffs."
  :group 'difftastic
  (setq buffer-read-only t))

(defvar-local difftastic--chunk-regexp-chunk nil)
(defvar-local difftastic--chunk-regexp-file nil)

(defun difftastic--chunk-regexp (file-chunk)
  "Build a regexp that mathes a chunk.
When FILE-CHUNK is t the regexp contains optional chunk match
data."
  (let ((chunk-regexp (if file-chunk
                          'difftastic--chunk-regexp-file
                        'difftastic--chunk-regexp-chunk))
        (languages (difftastic--get-languages)))
    (or (eval chunk-regexp)
        (set chunk-regexp
             (rx-to-string
              `(seq
                ;; non greedy filename to let following group match
                bol (not " ") ,(if file-chunk '(+? any) '(+ any))
                ;; search for optional chunk info only when searching for a
                ;; file-chunk
                ,@(when file-chunk
                    '((optional " --- " (group (1+ digit)) "/" (1+ digit))))
                ;; language or error at the end
                (or
                 (seq " --- " (or ,@languages))
                 (seq " --- Text ("
                      (or
                       (seq (1+ digit)
                            " " (or ,@(cl-remove "Text" languages
                                                 :test #'string=))
                            " parse error" (? "s")
                            ", exceeded DFT_PARSE_ERROR_LIMIT")
                       (seq "exceeded DFT_GRAPH_LIMIT")
                       (seq (1+ digit)
                            (or (seq "." (= 2 digit) " " (any "KMGTPE") "iB")
                                (seq " " (? (any "KMGTPE") "i") "B"))
                            " exceeded DFT_BYTE_LIMIT"))
                      ")"))
                eol))))))

(defun difftastic--chunk-bol (file-chunk)
  "Find line beginning position.
When FILE-CHUNK is t the line beginning position is only found
when match data indicates this is the chunk number 1.  This
function must be called with match data set by searching for a
regexp from `difftastic--chunk-regexp'."
  (if file-chunk
      (when (let ((chunk-no (match-string 1)))
              (or (not chunk-no)
                  (string-equal "1" chunk-no)))
        (line-beginning-position))
    (line-beginning-position)))

(defun difftastic--next-chunk (&optional file-chunk)
  "Find line beginning position of next chunk.
When FILE-CHUNK is t only first file chunks are searched
for.  Return nil when no chunk is found."
  (let ((chunk-regexp (difftastic--chunk-regexp file-chunk)))
    (save-excursion
      (goto-char (line-end-position))
      (cl-block searching-next-chunk
        (while (re-search-forward chunk-regexp nil t)
          (when-let ((chunk-bol
                      (difftastic--chunk-bol file-chunk)))
            (cl-return-from searching-next-chunk chunk-bol)))))))

(defun difftastic--prev-chunk (&optional file-chunk)
  "Find line beginning position of previous chunk.
When FILE-CHUNK is t only first file chunks are searched
for.  Return nil when no chunk is found."
  (let ((chunk-regexp (difftastic--chunk-regexp file-chunk)))
    (save-excursion
      (goto-char (line-beginning-position))
      (when (> (point) (point-min))
        (backward-char)
        (cl-block searching-prev-chunk
          (while (re-search-backward chunk-regexp nil t)
            (when-let ((chunk-bol
                        (difftastic--chunk-bol file-chunk)))
              (cl-return-from searching-prev-chunk chunk-bol))))))))

;; From `view-mode'

;; This is awful because it assumes that the selected window shows the
;; current buffer when this is called.
(defun difftastic-mode--do-exit (&optional exit-action all-windows)
  "Exit difftastic mode in various ways.
If all arguments are nil, remove the current buffer from the
selected window using the `quit-restore' information associated
with the selected window.  If optional argument ALL-WINDOWS or
`difftastic-exits-all-viewing-windows' are non-nil, remove the
current buffer from all windows showing it.

EXIT-ACTION, if non-nil, must specify a function that is called
with the current buffer as argument and is called after disabling
`view-mode' and removing any associations of windows with the
current buffer."
  (let ((buffer (window-buffer)))
	(cond
	 ((or all-windows difftastic-exits-all-viewing-windows)
	  (dolist (window (get-buffer-window-list))
	    (quit-window nil window)))
	 ((eq (window-buffer) (current-buffer))
	  (quit-window)))
	(when exit-action
	  (funcall exit-action buffer))))

(defun difftastic-leave ()
  "Quit difftastic mode and maybe switch buffers, but don't kill this buffer."
  (interactive nil difftastic-mode)
  (difftastic-mode--do-exit))

(defun difftastic-quit ()
  "Quit difftastic mode, kill current buffer trying to restore window and buffer.
Try to restore selected window to previous state and go to
previous buffer or window."
  (interactive nil difftastic-mode)
  (difftastic-mode--do-exit 'kill-buffer))

(defun difftastic-quit-all ()
  "Quit difftastic mode, kill current buffer trying to restore windows and buffers.
Try to restore all windows viewing buffer to previous state and
go to previous buffer or window."
  (interactive nil difftastic-mode)
  (difftastic-mode--do-exit 'kill-buffer t))

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
  (when-let ((difftastic-face
              (and (listp face)
                   (cl-find-if
                    (lambda (difftastic-face)
                      (and (string=
                            (face-foreground difftastic-face)
                            (or
                             (plist-get face :foreground)
                             (car (alist-get :foreground face))))
                           ;; ansi-color-* faces have the same
                           ;; foreground and background - don't use them
                           (not (string= (face-foreground difftastic-face)
                                         (face-background difftastic-face)))
                           (face-background difftastic-face)))
                    (vconcat difftastic-normal-colors-vector
                             difftastic-bright-colors-vector)))))
    ;; difftastic uses underline to highlight some changes.
    ;; It uses bold as well, but it's not as unambiguous as underline.
    ;; Use underline to detect highlight, but remove both: bold and underline.
    (if-let ((highlight-face (and (cl-member 'ansi-color-underline face)
                                  (alist-get difftastic-face
                                             difftastic-highlight-alist))))
        (progn
          (cl-remf face :foreground)
          (setq face (cl-delete 'ansi-color-underline face))
          (setq face (cl-delete 'ansi-color-bold face))
          (setq face
                (cl-delete-if (lambda (elt)
                                (and (listp elt)
                                     (plist-get elt :foreground)))
                              face))
          (push `(:foreground
                  ,(face-foreground highlight-face nil 'default))
                face)
          (push `(:background
                  ,(face-background highlight-face nil 'default))
                face))
      (when-let ((fg (plist-get face :foreground)))
        (cl-remf face :foreground)
        (push `(:foreground ,fg) face))
      (push `(:background
              ,(face-background difftastic-face nil 'default))
            face)))
  face)

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

(defvar-local difftastic--rerun-alist nil)

(defun difftastic--build-git-process-environment (requested-width
                                                  &optional difftastic-args)
  "Build a difftastic git command with REQUESTED-WIDTH.
The DIFFTASTIC-ARGS is a list of extra arguments to pass to
`difftastic-executable'."
  (cons (format
         "GIT_EXTERNAL_DIFF=%s --color always --width %s --background %s%s"
         difftastic-executable
         requested-width
         (frame-parameter nil 'background-mode)
         (if difftastic-args
             (mapconcat #'identity
                        (cons "" difftastic-args)
                        " ")
           ""))
        process-environment))

(defun difftastic--git-with-difftastic (buffer command
                                               &optional difftastic-args)
  "Run COMMAND with GIT_EXTERNAL_DIFF then show result in BUFFER.
The DIFFTASTIC-ARGS is a list of extra arguments to pass to
`difftastic-executable'."
  (let* ((requested-width (funcall difftastic-requested-window-width-function))
         (process-environment (difftastic--build-git-process-environment
                               requested-width
                               difftastic-args))
         (difftastic-display-buffer-function difftastic-display-buffer-function))
    (difftastic--run-command
     buffer
     command
     (lambda ()
       (setq difftastic--rerun-alist
             `((default-directory . ,default-directory)
               (git-command . ,command)
               (difftastic-args . ,difftastic-args)))
       (funcall difftastic-display-buffer-function buffer requested-width)))))

(defun difftastic--run-command-filter (process string)
  "A process filter for `difftastic--run-command'.
It applies ANSI colors with `apply-ansi-colors' using difftastic
custom colors vectors.  The PROCESS and STRING are filter
arguments, like in `make-process''s filter."
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

(defun difftastic--run-command-sentinel (process action command)
  "A sentinel for `difftastic--run-command'.
When PROCESS\\=' status is `exit' and there's output in
PROCESS\\=' buffer it calls the ACTION with current buffer set to
the PROCESS\\=' buffer.  The COMMAND is the original PROCESS\\='
command."
  (when (eq (process-status process) 'exit)
    (with-current-buffer (process-buffer process)
      (difftastic-mode)
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (if (eq (point-min) (point-max))
          (message "Process '%s' returned no output"
                   (mapconcat #'identity command " "))
        (when action (funcall action))
        (message nil)))))

(defun difftastic--run-command (buffer command &optional action)
  "Run COMMAND, show its results in BUFFER, then execute ACTION.
The ACTION is meant to display the BUFFER in some window and, optionally,
perform cleanup.  It returns a process created by `make-process'."
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
   :filter #'difftastic--run-command-filter
   :sentinel
   (lambda (process _event)
     (difftastic--run-command-sentinel process action command))))

(defvar difftastic--transform-git-to-difft
  '(("^-\\(?:U\\|-unified=\\)\\([0-9]+\\)$" . "--context \\1"))
  "Alist with entries in a from of (GIT-ARG-REGEXP . DIFFT-REPLACEMENT).
When git argument matches GIT-ARG-REGEXP it will be replaces with
DIFFT-REPLACEMENT.")

(defvar difftastic--transform-incompatible
  '("^--stat$"
    "^--no-ext-diff$"
    "^--ignore-space-change$"
    "^--ignore-all-space$"
    "^--irreversible-delete$"
    "^--function-context$"
    "^-\\(?:M\\|-find-renames=?\\)\\(?:[0-9]+%?\\)?$"
    "^-\\(?:C\\|-find-copies=?\\)\\(?:[0-9]+%?\\)?$"
    "^-R$"
    "^--show-signature$")
  "List of git arguments that are incompatible in a context of difftastic.
Each argument is matched as regexp.")

(defun difftastic--transform-diff-arguments (args)
  "Transform \\='git diff\\=' ARGS to be compatible with difftastic.
This removes arguments converts some arguments to be compatible
with difftastic (i.e., \\='-U\\=' to \\='--context\\=') and
removes some that are incompatible (i.e., \\='--stat\\=',
\\='--no-ext-diff\\=').  The return value is a list in a form
of (GIT-ARGS DIFFT-ARGS), where GIT-ARGS are arguments to be
passed to \\='git\\=', and DIFFT-ARGS are arguments to be passed
to difftastic."
  (let (case-fold-search)
    (list
     (cl-remove-if
      (lambda (arg)
        (cl-member arg
                   (append
                    difftastic--transform-incompatible
                    (cl-mapcar #'car difftastic--transform-git-to-difft))
                   :test (lambda (arg regexp)
                           (string-match regexp arg))))
      args)
     (cl-remove
      nil
      (cl-mapcar
       (lambda (arg)
         (cl-dolist (regexp-replacement difftastic--transform-git-to-difft)
           (when (string-match (car regexp-replacement) arg)
             (cl-return
              (replace-match (cdr regexp-replacement) t nil arg)))))
       args)))))

;;;###autoload
(defun difftastic-git-diff-range (&optional rev-or-range args files)
  "Show difference between two commits using difftastic.
The meaning of REV-OR-RANGE, ARGS, and FILES is like in
`magit-diff-range', but ARGS are adjusted for difftastic with
`difftastic--transform-diff-arguments'."
  (interactive (cons (magit-diff-read-range-or-commit "Diff for range"
                                                      nil current-prefix-arg)
                     (magit-diff-arguments)))
  (pcase-let* ((`(,git-args ,difftastic-args)
                (difftastic--transform-diff-arguments args))
               (buffer-name
                (concat
                 "*difftastic git diff"
                 (if git-args
                     (mapconcat #'identity (cons "" git-args) " ")
                   "")
                 (if rev-or-range (concat " " rev-or-range)
                   "")
                 (if files
                     (mapconcat #'identity (cons " --" files) " ")
                   "")
                 "*")))
    (difftastic--git-with-difftastic
     (get-buffer-create buffer-name)
     `("git" "--no-pager" "diff" "--ext-diff"
       ,@(when git-args git-args)
       ,@(when rev-or-range (list rev-or-range))
       ,@(when files (cons "--" files)))
     difftastic-args)))

;;;###autoload
(defun difftastic-magit-diff (&optional args files)
  "Show the result of \\='git diff ARGS -- FILES\\=' with difftastic."
  (interactive (magit-diff-arguments))
  (let ((default-directory (magit-toplevel))
        (section (magit-current-section)))
    (cond
     ((magit-section-match 'module section)
      (setq default-directory
            (expand-file-name
             (file-name-as-directory (oref section value))))
      (difftastic-git-diff-range (oref section range)))
     (t
      (when (magit-section-match 'module-commit section)
        (setq args nil)
        (setq files nil)
        (setq default-directory
              (expand-file-name
               (file-name-as-directory (magit-section-parent-value section)))))
      (pcase (magit-diff--dwim)
        ('unmerged
         (unless (magit-merge-in-progress-p)
           (user-error "No merge is in progress"))
         (difftastic-git-diff-range (magit--merge-range) args files))
        ('unstaged
         (difftastic-git-diff-range nil args files))
        ('staged
         (let ((file (magit-file-at-point)))
           (if (and file (equal (cddr (car (magit-file-status file)))
                                '(?D ?U)))
               ;; File was deleted by us and modified by them.  Show the latter.
               (progn
                 (unless (magit-merge-in-progress-p)
                   (user-error "No merge is in progress"))
                 (difftastic-git-diff-range
                  (magit--merge-range) args (list file)))
             (difftastic-git-diff-range
              nil (cl-pushnew "--cached" args :test #'string=) files))))
        (`(stash . ,value)
         ;; ATM, `magit-diff--dwim' evaluates to `commit' when point is on stash
         ;; section
         (difftastic-git-diff-range (format "%s^..%s" value value) args files))
        (`(commit . ,value)
         (difftastic-git-diff-range (format "%s^..%s" value value) args files))
        ((and range (pred stringp))
         (difftastic-git-diff-range range args files))
        (_
         (call-interactively #'difftastic-git-diff-range)))))))

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
    (difftastic--git-with-difftastic
     (get-buffer-create (concat "*difftastic git show " rev "*"))
     (list "git" "--no-pager" "show" "--ext-diff" rev))))

(defun difftastic--make-temp-file (prefix buffer)
  "Make a temporary file for BUFFER content with PREFIX included in file name."
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

(defun difftastic--get-file-buf (prefix buffer)
  "If BUFFER visits a file return it else create a temporary file with PREFIX.
The return value is a cons in a form of (FILE . BUF) where FILE
is the file and BUF either is  nil if this is non temporary file,
or BUF is set to BUFFER if this is a temporary file."
  (let* (buf
         (file
          (if-let ((buffer-file (buffer-file-name buffer)))
              (progn
                (save-buffer buffer)
                buffer-file)
            (setq buf buffer)
            (difftastic--make-temp-file prefix buffer))))
    (cons file buf)))

(defun difftastic--delete-temp-file-buf (file-buf)
  "Delete FILE-BUF when it is a temporary file.
The FILE-BUF is a cons where car is the file and cdr is non-nil
when it is a temporary or nil otherwise."
  (when-let ((file (car file-buf)))
    (when (and (cdr file-buf) (stringp file) (file-exists-p file))
      (delete-file file))))

(defun difftastic--get-languages ()
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
                           (downcase (string-replace
                                      "-" " "
                                      (replace-regexp-in-string
                                       "\\(?:-ts\\)?-mode$" ""
                                       (symbol-name mode))))))
                languages)))

(defun difftastic--build-files-command (file-buf-A file-buf-B requested-width
                                                    &optional lang-override)
  "Build a difftastic command to compare files from FILE-BUF-A and FILE-BUF-B.
The FILE-BUF-A and FILE-BUF-B are conses where car is the file
and cdr is a buffer when it is a temporary file and nil otherwise.
REQUESTED-WIDTH is passed to difftastic as \\='--width\\=' argument.
LANG-OVERRIDE is passed to difftastic as \\='--override\\=' argument."
  `(,difftastic-executable
    "--color" "always"
    "--width" ,(number-to-string requested-width)
    "--background" ,(format "%s" (frame-parameter nil 'background-mode))
    ,@(when lang-override (list "--override"
                                (format "*:%s" lang-override)))
    ,(car file-buf-A)
    ,(car file-buf-B)))

(defun difftastic--files-internal (buffer file-buf-A file-buf-B
                                          &optional lang-override)
  "Run difftastic on FILE-BUF-A and FILE-BUF-B and show results in BUFFER.
The FILE-BUF-A and FILE-BUF-B are conses where car is the file
and cdr is a buffer when it is a temporary file and nil otherwise.
LANG-OVERRIDE is passed to difftastic as \\='--override\\='
argument."
  (let ((requested-width (funcall difftastic-requested-window-width-function))
        (difftastic-display-buffer-function difftastic-display-buffer-function))
    (difftastic--run-command
     buffer
     (difftastic--build-files-command file-buf-A
                                      file-buf-B
                                      requested-width
                                      lang-override)
     (lambda ()
       (setq difftastic--rerun-alist
             `((default-directory . ,default-directory)
               (lang-override . ,lang-override)
               (file-buf-A . ,file-buf-A)
               (file-buf-B . ,file-buf-B)))
       (funcall difftastic-display-buffer-function buffer requested-width)
       (difftastic--delete-temp-file-buf file-buf-A)
       (difftastic--delete-temp-file-buf file-buf-B)))))

(defmacro difftastic--with-file-bufs (file-bufs &rest body)
  "Exectue the forms in BODY with each symbol in FILE-BUFS list `let'-bound to nil.
If any form in BODY signals an error each element in FILE-BUFS will be passed
to `difftastic--delete-temp-file-buf'."
  (declare (indent 1) (debug t))
  `(let ,file-bufs
     (condition-case err
         (progn ,@body)
       (error
        (dolist (file-buf (list ,@file-bufs))
          (difftastic--delete-temp-file-buf file-buf))
        (signal (car err) (cdr err))))))

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
             (let* ((languages (difftastic--get-languages))
                    (suggested (difftastic--make-suggestion
                                languages
                                (get-buffer bf-A)
                                (get-buffer bf-B))))
               (completing-read "Language: " languages nil t suggested))))))

  (difftastic--with-file-bufs (file-buf-A file-buf-B)
    (setq file-buf-A (difftastic--get-file-buf "A" (get-buffer buffer-A))
          file-buf-B (difftastic--get-file-buf "B" (get-buffer buffer-B)))
    (difftastic--files-internal
     (get-buffer-create
      (concat "*difftastic " buffer-A " " buffer-B "*"))
     file-buf-A
     file-buf-B
     lang-override)))

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
             (completing-read "Language: " (difftastic--get-languages) nil t)))))
  (difftastic--files-internal
   (get-buffer-create (concat "*difftastic "
                              (file-name-nondirectory file-A)
                              " "
                              (file-name-nondirectory file-B)
                              "*"))
   (cons file-A nil)
   (cons file-B nil)
   lang-override))

;;;###autoload
(defun difftastic-dired-diff (file &optional lang-override)
  "Compare file at point with FILE using difftastic.
The behavior is the same as `dired-diff', except for the prefix argument, which
makes the function prompt for LANG-OVERRIDE.  See \\='difft
--list-languages\\=' for language list."
  (interactive
   (list 'interactive
         (when current-prefix-arg
           (completing-read "Language: " (difftastic--get-languages) nil t)))
   dired-mode)
  (cl-letf (((symbol-function 'diff)
             (lambda (current file _switches)
               (difftastic-files current file lang-override)))
            (current-prefix-arg nil))
    (if (eq file 'interactive)
        (call-interactively #'dired-diff))
    (funcall #'dired-diff file)))

(defun difftastic--rerun-file-buf (prefix file-buf rerun-alist)
  "Create a new temporary file for the FILE-BUF with PREFIX if needed.
The new FILE-BUF is additionally set in RERUN-ALIST.  The FILE-BUF
is a cons where car is the file and cdr is a buffer when it is a
temporary file or nil otherwise."
  (if-let ((buffer (cdr file-buf)))
      (if (buffer-live-p buffer)
          (setf (alist-get (intern (concat "file-buf-" prefix)) rerun-alist)
                (difftastic--get-file-buf prefix buffer))
        (user-error "Buffer %s [%s] doesn't exist anymore" prefix buffer))
    file-buf))

;;;###autoload
(defun difftastic-rerun (&optional lang-override)
  "Rerun difftastic in the current buffer.
Optionally, provide a LANG-OVERRIDE to override language used.
See \\='difft --list-languages\\=' for language list.  When
function is called with a prefix arg then ask for language before
running difftastic.

In order to determine requested width for difftastic a call to
`difftastic-rerun-requested-window-width-function' is made.  When
the latter is set to nil the call is made to
`difftastic-requested-window-width-function'."
  (interactive (list
                (or (when current-prefix-arg
                      (completing-read "Language: "
                                       (difftastic--get-languages) nil t))))
               difftastic-mode)
  (if-let (((eq major-mode 'difftastic-mode))
           (rerun-alist (copy-tree difftastic--rerun-alist)))
      (difftastic--with-file-bufs (file-buf-A file-buf-B)
        (let-alist rerun-alist
          (setq file-buf-A
                (difftastic--rerun-file-buf "A" .file-buf-A rerun-alist)
                file-buf-B
                (difftastic--rerun-file-buf "B" .file-buf-B rerun-alist))
          (let* ((default-directory .default-directory)
                 (lang-override (or lang-override
                                    (alist-get 'lang-override rerun-alist)))
                 (requested-width
                  (funcall (or
                            difftastic-rerun-requested-window-width-function
                            difftastic-requested-window-width-function)))
                 (process-environment
                  (if .git-command
                      (difftastic--build-git-process-environment
                       requested-width
                       (append .difftastic-args
                               (when lang-override
                                 (list "--override"
                                       (format "*:%s" lang-override)))))
                    process-environment))
                 (command (or .git-command
                              (difftastic--build-files-command
                               file-buf-A
                               file-buf-B
                               requested-width
                               lang-override)))
                 (buffer (current-buffer)))
            (difftastic--run-command
             buffer
             command
             (lambda ()
               (setq difftastic--rerun-alist rerun-alist)
               (difftastic--delete-temp-file-buf file-buf-A)
               (difftastic--delete-temp-file-buf file-buf-B))))))
    (user-error "Nothing to rerun")))

(provide 'difftastic)
;;; difftastic.el ends here
