;;; difftastic.el --- Wrapper for difftastic        -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Przemyslaw Kryger

;; Author: Przemyslaw Kryger <pkryger@gmail.com>
;; Keywords: tools diff
;; Homepage: https://github.com/pkryger/difftastic.el
;; Package-Requires: ((emacs "28.1") (compat "29.1.4.2") (magit "4.0.0") (transient "0.4.0"))
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
;; - Use difftastic do compare files and buffers (also directly from `dired').
;; - Rerun `difftastic' with `g' to use current window width to "reflow"
;;   content and/or to force language change (when called with prefix).
;; - Use double prefix argument to specify all `difftastic' arguments.
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
;; Please see [Configuration] section for example configuration.
;;
;; You can use any of the package managers that supports installation from
;; MELPA.  It can be one of (but not limited to): one of the built-in
;; `package', `use-package', or any other package manger that handles
;; autoloads generation, for example (in alphabetical order) [Borg], [Elpaca],
;; [Quelpa], or [straight.el].
;;
;;
;; [Configuration] See section Configuration
;;
;; [Borg] <https://github.com/emacscollective/borg>
;;
;; [Elpaca] <https://github.com/progfolio/elpaca>
;;
;; [Quelpa] <https://github.com/quelpa/quelpa>
;;
;; [straight.el] <https://github.com/radian-software/straight.el>
;;
;;
;; Installing from GitHub
;; ~~~~~~~~~~~~~~~~~~~~~~
;;
;; The preferred method is to use built-in `use-package'.  Add the following
;; to your Emacs configuration file (usually `~/.emacs' or
;; `~/.emacs.d/init.el'):
;;
;; (use-package difftastic
;;   :defer t
;;   :vc (:url "https://github.com/pkryger/difftastic.el.git"
;;        :rev :newest)))
;;
;; Alternatively, you can do a manual checkout and install it from there, for
;; example:
;;
;; 1. Clone this repository to a directory of your choice, for example
;;    `~/src/difftastic'.
;; 2. Add the following lines to your Emacs configuration file:
;;
;; (use-package difftastic
;;   :defer t
;;   :vc t
;;   :load-path "~/src/difftastic")
;;
;; Yet another option is to use any of the package managers that supports
;; installation from GitHub or a an existing checkout.  That could be
;; `package-vc', or any of package managers listed in [Installing from MELPA].
;;
;;
;; [Installing from MELPA] See section Installing from MELPA
;;
;; Manual Installation
;; -------------------
;;
;; Note, that this method does not generate autoloads.  As a consequence it
;; will cause the whole package and it's dependencies (including `magit') to
;; be loaded at startup.  If you want to avoid this, ensure autoloads set up
;; on Emacs startup.  See [Installing from MELPA] a few package managers that
;; can generate autoloads when package is installed.
;;
;; 1. Clone this repository to a directory of your choice, for example
;;    `~/src/difftastic'.
;; 2. Add the following line to your Emacs configuration file:
;;
;;    (add-to-list 'load-path "~/src/difftastic")
;;    (require 'difftastic)
;;    (require 'difftastic-bindings)
;;
;;
;; [Installing from MELPA] See section Installing from MELPA
;;
;;
;; Configuration
;; =============
;;
;; This section assumes you have `difftastic''s autoloads set up at Emacs
;; startup.  If you have installed `difftastic' using built-in `package' or
;; `use-package' then you should be all set.
;;
;; To configure `difftastic' commands in relevant `magit' prefixes and
;; keymaps, use the following code snippet in your Emacs configuration:
;;
;; (difftastic-bindings-mode)
;;
;; Or, if you use `use-package':
;;
;; (use-package difftastic-bindings
;;   :ensure difftastic ;; or nil if you prefer manual installation
;;   :config (difftastic-bindings-mode))
;;
;; By default this will bind:
;; - `M-d' to `difftastic-magit-diff' and `M-c' to `difftastic-magit-show' in
;;   `magit-diff' prefix,
;; - `M-RET' to `difftastic-magit-show' in `magit-blame' transient prefix and
;;   in `magit-blame-read-only-map' keymap,
;; - `M-d' to `difftastic-magit-diff-buffer-file' in `magit-file-dispatch'
;;   prefix,
;; - `M-\=' to `difftastic-dired-diff' in `dired-mode-map'.
;;
;; Please refer to `difftastic-bindings-alist' documentation to see how to
;; change default bindings.  You need to toggle the `difftastic-bindings-mode'
;; off and on again to apply the changes.
;;
;; The `difftastic-bindings=mode' was designed to have minimal dependencies
;; and be reasonably fast to load, while providing a mechanism to bind
;; `difftastic' commands, such that they are available in relevant contexts.
;;
;;
;; Manual Key Bindings Configuration
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
;; If you don't want to use mechanism delivered by `difftastic-bindings-mode'
;; you can write your own configuration.  As a starting point the following
;; snippets demonstrate how to achieve partial effect similar to the one
;; provided by `difftastic-bindings-mode' in default configuration:
;;
;; (require 'difftastic)
;; (require 'transient)
;;
;; (let ((suffix [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
;;                ("S" "Difftastic show" difftastic-magit-show)]))
;;   (with-eval-after-load 'magit-diff
;;     (unless (equal (transient-parse-suffix 'magit-diff suffix)
;;                    (transient-get-suffix 'magit-diff '(-1 -1)))
;;       (transient-append-suffix 'magit-diff '(-1 -1) suffix))))
;; (let ((suffix '("M-RET" "Difftastic show" difftastic-magit-show)))
;;   (with-eval-after-load 'magit-blame
;;     (unless (equal (transient-parse-suffix 'magit-blame suffix)
;;                    (transient-get-suffix 'magit-blame "b"))
;;       (transient-append-suffix 'magit-blame "b" suffix))
;;     (keymap-set magit-blame-read-only-mode-map
;;                 "M-RET" #'difftastic-magit-show)))
;;
;; Or, if you use `use-package':
;;
;; (use-package difftastic
;;   :defer t
;;   :init
;;   (use-package transient               ; to silence compiler warnings
;;     :autoload (transient-get-suffix
;;                transient-parse-suffix))
;;
;;     (use-package magit-blame
;;       :defer t :ensure magit
;;       :bind
;;       (:map magit-blame-read-only-mode-map
;;             ("M-RET" . #'difftastic-magit-show))
;;       :config
;;       (let ((suffix '("M-RET" "Difftastic show" difftastic-magit-show)))
;;         (unless (equal (transient-parse-suffix 'magit-blame suffix)
;;                        (transient-get-suffix 'magit-blame "b"))
;;           (transient-append-suffix 'magit-blame "b" suffix)))
;;     (use-package magit-diff
;;       :defer t :ensure magit
;;       :config
;;       (let ((suffix [("M-d" "Difftastic diff (dwim)" difftastic-magit-diff)
;;                      ("M-c" "Difftastic show" difftastic-magit-show)]))
;;         (unless (equal (transient-parse-suffix 'magit-diff suffix)
;;                        (transient-get-suffix 'magit-diff '(-1 -1)))
;;           (transient-append-suffix 'magit-diff '(-1 -1) suffix)))))
;;
;;
;; Usage
;; =====
;;
;; General Usage
;; ~~~~~~~~~~~~~
;;
;; The following commands are meant to help invoking `difftastic' depending on
;; context and desired outcome.
;;
;; - `difftastic-magit-diff' - show the result of `git diff ARGS -- FILES'
;;   with `difftastic'.  This is the main entry point for DWIM action, so it
;;   tries to guess revision or range.
;; - `difftastic-magit-show' - show the result of `git show ARG' with
;;   `difftastic'.  It tries to guess `ARG', and ask for it when can't. When
;;   called with prefix argument it will ask for `ARG'.
;; - `difftastic-magit-diff-buffer-file' - show diff for the blob or file
;;   visited in the current buffer with `difftastic'.  When the buffer visits
;;   a blob, then show the respective commit.  When the buffer visits a file,
;;   then show the differences between `HEAD' and the working tree.
;; - `difftastic-files' - show the result of `difft FILE-A FILE-B'.  When
;;   called with prefix argument it will ask for language to use, instead of
;;   relaying on `difftastic''s detection mechanism.
;; - `difftastic-buffers' - show the result of `difft BUFFER-A BUFFER-B'.
;;   Language is guessed based on buffers modes.  When called with prefix
;;   argument it will ask for language to use.
;; - `difftastic-dired-diff' - same as `dired-diff', but with
;;   `difftastic-files' instead of the built-in `diff'.
;; - `difftastic-git-diff-range' - transform `ARGS' for difftastic and show
;;   the result of `git diff ARGS REV-OR-RANGE -- FILES' with `difftastic'.
;;
;;
;; Specifying `difftastic' Arguments
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
;; All [above] commands (and `difftastic-rerun' described [below]) support
;; specification of `difft' arguments.  When a command is called with a double
;; prefix argument a popup is presented allowing to specify desired arguments.
;; This is in addition to a command specific handling of a single prefix
;; argument.
;;
;; In order to aid arguments entry and provide similarity to workflows in
;; `magit' and `forge', a `transient' prefix is used for the popup.  For
;; example, some - less commonly used - arguments are not visible in default
;; configuration.  Type `C-x l' in the menu to make them visible.  Type `C-h
;; C-h' for `difftastic' help (`man difft').  Any other `transient' commands
;; should work as well.
;;
;; Note that in some cases arguments will take precedence over standard and
;; computed values, for example `--width' is one such a argument.
;;
;;
;; [above] See section General Usage
;;
;; [below] See section `difftastic-mode' Commands
;;
;;
;; `difftastic-mode' Commands
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
;; When a buffer shows `difftastic' output the following commands can be used.
;; Commands are followed by their default keybindings (in parenthesis).
;;
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
;; - `difftastic-toggle-chunk' (`TAB' or `C-i') - toggle visibility of a chunk
;;   at point.  The point has to be in a chunk header.  When called with a
;;   prefix toggle all file chunks from the header to the end of the file.
;;   See also `difftastic-hide-chunk' and `difftastic=show-chunk'.
;; - `difftastic-diff-visit-file' (`RET'),
;;   `difftastic-diff-visit-file-other-window',
;;   `difftastic-diff-visit-file-other-frame' - from a diff visit appropriate
;;   version of a chunk file.  This has been modeled after
;;   `magit-diff-visit-file', but there are some differences, please see
;;   documentation for `difftastic-diff-visit-file'.
;; - `difftastic-diff-visit-worktree-file' (`C-RET', `C-j'),
;;   `difftastic-diff-visit-worktree-file-other-window',
;;   `difftastic-diff-visit-worktree-file-other-frame' - from a diff visit
;;   appropriate version of a chunk file.  This has been modeled after
;;   `magit-diff-visit-worktree-file', but there are some differences, please
;;   see documentation for `difftastic-diff-visit-worktree-file'.
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
;; Window Management
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
;; `difftastic-mode' Behavior
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
;; - `difftastic-diff-visit-avoid-head-blob' - controls whether to avoid
;;   visiting blob of a `HEAD' revision when visiting file form a
;;   `difftastic-mode' buffer.
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
;; To run tests interactively:
;; - open the <file:test/difftastic.t.el>
;; - type `M-x eval-buffer <RET>'
;; - open the <file:test/difftastic-bindings.t.el>
;; - type `M-x eval-buffer <RET>'
;; - type `M-x ert <RET> t <RET>'
;;
;; Alternatively you can use [Cask] to run tests in batch mode.  There's a
;; convenience <file:Makefile> with a `test' target, so you can just type
;; `make test'.
;;
;; It seems that byte compilation interferres with [el-mock].  In order to get
;; the tests to pass you may need to:
;; - type `M-x eval-buffer <RET>' in <file:difftastic.el> and in
;;   <file:difftastic-bindings.el> when running test interactively with `M-x
;;   <RET> ert <RET>',
;; - remove all `.elc' files in the development directory when running tests
;;   in batch mode.
;;
;; This repository uses [Coveralls] to track test coverage.  After a PR has
;; been approved for a Gighub Action run, a report will be published
;; [Coveralls difftastic repo].  Please check it out if there's no outstanding
;; relevant lines.
;;
;; You can run all checks performed by Github Actions, by typing: `make
;; bytecompile lint relint checkdoc commentary test'.
;;
;;
;; [Cask] <https://github.com/cask/cask>
;;
;; [el-mock] <https://github.com/rejeep/el-mock.el>
;;
;; [Coveralls] <https://coveralls.io>
;;
;; [Coveralls difftastic repo]
;; <https://coveralls.io/github/pkryger/difftastic.el>
;;
;;
;; Documentation Autoring
;; ~~~~~~~~~~~~~~~~~~~~~~
;;
;; This package uses [org-commentary.el] (different from the one available on
;; MELPA!) to generate and validate commentary section in `difftastic.el'.
;; Please see the package documentation for usage instructions.
;;
;;
;; [org-commentary.el] <https://github.com/pkryger/org-commentary.el>

;;; Code:

(require 'ansi-color)
(require 'cl-lib)
(require 'dired)
(require 'ediff)
(require 'font-lock)
(require 'magit)
(require 'view)
(require 'transient)
(require 'crm)

(eval-when-compile
  (require 'compat)
  (require 'fringe))

(defgroup difftastic nil
  "Integration with difftastic."
  :link '(emacs-commentary-link "difftastic")
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
Note that only foreground and background properties will be used."
  :type '(vector
          (face :tag "Black")
          (face :tag "Removed")
          (face :tag "Added")
          (face :tag "Heading")
          (face :tag "Comment")
          (face :tag "String")
          (face :tag "Warning")
          (face :tag "White"))
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
Note that only foreground and background properties will be used."
  :type '(vector
          (face :tag "Black")
          (face :tag "Removed")
          (face :tag "Added")
          (face :tag "Heading")
          (face :tag "Comment")
          (face :tag "String")
          (face :tag "Warning")
          (face :tag "White"))
  :group 'difftastic)

(defcustom difftastic-highlight-alist
  '((magit-diff-added . magit-diff-added-highlight)
    (magit-diff-removed . magit-diff-removed-highlight))
  "Faces to replace underlined highlight in difftastic output.
This is an alist, where each association defines a mapping
between a non-highlighted face to a highlighted face.  Set to nil if
you prefer unaltered difftastic output.

Note that only foreground and background properties will be used."
  :type '(alist :key-type (face :tag "Non-highlighted")
                :value-type (face :tag "Highlighted"))
  :group 'difftastic)

(defcustom difftastic-highlight-strip-face-properties '(:bold :underline)
  "Face properties to be stripped in highlights.
Difftastic uses underline, and sometimes bold, face properties to
highlight.  When a distinct highlight face is used (i.e., defined
in `difftastic-highlight-alist'), then some of these properties
may be cluttering output."
  :type '(set (const :bold) (const :underline) (const :italic) (const :faint))
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

(defcustom difftastic-diff-visit-avoid-head-blob nil
  "Whether `difftastic-diff-visit-file' avoids visiting a blob from `HEAD'.

By default `difftastic-diff-visit-file' always visits the blob that
modified the current line (or is a context line of current
modification), while `difftastic-diff-visit-worktree-file' visits the
respective file in the working tree.  When the value of this option is
non-nil, then for the `HEAD' commit, the former command visits the
worktree file too, but that renders visiting a blob from `HEAD'
impossible.

This is similar to `magit-diff-visit-avoid-head-blob', which see."
  :type 'boolean
  :group 'difftastic)

(defcustom difftastic-use-last-dir ediff-use-last-dir
  "When non-nil difftastic will use previous directory when reading file name.
Like `ediff-use-last-dir', which see."
  :type 'boolean
  :group 'difftastic)

(defvar difftastic--last-dir-A nil)
(defvar difftastic--last-dir-B nil)
(defvar-local difftastic--metadata nil)

(defmacro difftastic--with-temp-advice (symbol how function &rest body)
  ;; checkdoc-params: (symbol how function)
  "Execute BODY with advice temporarily enabled.
See `advice-add' for explanation of SYMBOL, HOW, and FUNCTION arguments."
  (declare (indent 3) (debug t))
  `(let ((fn-advice-var ,function))
     (unwind-protect
         (progn
           (advice-add ,symbol ,how fn-advice-var)
           ,@body)
       (advice-remove ,symbol fn-advice-var))))

(defun difftastic-next-file ()
  "Move to the next file."
  (interactive nil difftastic-mode)
  (if-let* ((next (difftastic--next-chunk t)))
      (goto-char next)
    (user-error "No more files")))

(defun difftastic-next-chunk ()
  "Move to the next chunk."
  (interactive nil difftastic-mode)
  (if-let* ((next (difftastic--next-chunk)))
      (goto-char next)
    (user-error "No more chunks")))

(defun difftastic-previous-file ()
  "Move to the previous file."
  (interactive nil difftastic-mode)
  (if-let* ((previous (difftastic--prev-chunk t)))
      (goto-char previous)
    (user-error "No more files")))

(defun difftastic-previous-chunk ()
  "Move to the previous chunk."
  (interactive nil difftastic-mode)
  (if-let* ((previous (difftastic--prev-chunk)))
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
 "TAB"   #'difftastic-toggle-chunk
 "RET"   #'difftastic-diff-visit-file
 "C-<return>" #'difftastic-diff-visit-worktree-file
 "C-j"   #'difftastic-diff-visit-worktree-file
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
  (setq buffer-read-only t)
  (setq font-lock-defaults '(nil t))
  (add-to-invisibility-spec '(difftastic . t)))

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
                line-start
                ;; non greedy filename to let following group match
                (group (not " ") ,(if file-chunk '(+? any) '(+ any)))
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
function can be called only after a successfull searching for a
regexp from `difftastic--chunk-regexp'."
  (when-let* ((chunk-bol (if file-chunk
                             (when (let ((chunk-no (match-string 2)))
                                     (or (not chunk-no)
                                         (string-equal "1" chunk-no)))
                               (compat-call pos-bol)) ; Since Emacs-29
                           (compat-call pos-bol)))) ; Since Emacs-29
    (unless (or (difftastic--point-at-added-removed-p)
                (get-text-property chunk-bol 'invisible))
      chunk-bol)))

(defun difftastic--point-at-added-removed-p ()
  "Return whether a point is in added or removed line."
  (save-excursion
    (goto-char (compat-call pos-bol)) ; Since Emacs-29
    (save-match-data
      (looking-at (rx bol
                      (or (1+ ".")
                          (1+ digit))
                      " " (1+ any))))))

(defun difftastic--next-chunk (&optional file-chunk)
  "Find line beginning position of next chunk.
When FILE-CHUNK is t only first file chunks are searched
for.  Return nil when no chunk is found."
  (let ((chunk-regexp (difftastic--chunk-regexp file-chunk)))
    (save-excursion
      (goto-char (compat-call pos-eol)) ; Since Emacs-29
      (cl-block searching-next-chunk
        (while (re-search-forward chunk-regexp nil t)
          (when-let* ((chunk-bol
                       (difftastic--chunk-bol file-chunk)))
            (cl-return-from searching-next-chunk chunk-bol)))))))

(defun difftastic--prev-chunk (&optional file-chunk)
  "Find line beginning position of previous chunk.
When FILE-CHUNK is t only first file chunks are searched
for.  Return nil when no chunk is found."
  (let ((chunk-regexp (difftastic--chunk-regexp file-chunk)))
    (save-excursion
      (goto-char (compat-call pos-bol)) ; Since Emacs-29
      (when (> (point) (point-min))
        (backward-char)
        (cl-block searching-prev-chunk
          (while (re-search-backward chunk-regexp nil t)
            (when-let* ((chunk-bol
                         (difftastic--chunk-bol file-chunk)))
              (cl-return-from searching-prev-chunk chunk-bol))))))))

(defun difftastic--point-at-chunk-header-p (&optional file-chunk)
  "Return whether a point is in a chunk header.
When FILE-CHUNK is non nil the header has to be a file header."
  (when (not (eq (compat-call pos-bol) (compat-call pos-eol))) ; Since Emacs-29
    (save-excursion
      (goto-char (compat-call pos-bol)) ; Since Emacs-29
      (and (looking-at-p (difftastic--chunk-regexp file-chunk))
           (not (difftastic--point-at-added-removed-p))))))

(defun difftastic-hide-chunk (&optional file-chunk)
  "Hide chunk at point.
The point needs to be in chunk header.  When called with
FILE-CHUNK prefix hide all file chunks from the header to the end
of the file."
  (interactive "P" difftastic-mode)
  (when (difftastic--point-at-chunk-header-p file-chunk)
    (let ((inhibit-read-only t))
      (add-text-properties
       (compat-call pos-bol) ; Since Emacs-29
       (compat-call pos-eol) ; Since Emacs-29
       `(difftastic (:hidden ,(if file-chunk :file :chunk))))
      (add-text-properties
       (compat-call pos-eol) ; Since Emacs-29
       (if-let*  ((next-chunk
                   (difftastic--next-chunk file-chunk)))
           (save-excursion
             (goto-char next-chunk)
             (compat-call pos-eol -1)) ; Since Emacs-29
         (point-max))
       '(invisible difftastic)))))

(defun difftastic-show-chunk ()
  "Show chunk at point.
The point needs to be in chunk header."
  (interactive nil difftastic-mode)
  (when (difftastic--point-at-chunk-header-p)
    (let ((inhibit-read-only t)
          (file-chunk
           (memq :file
                 (get-text-property (compat-call pos-bol) ; Since Emacs-29
                                    'difftastic))))
      ;; This is not ideal as it doesn't just undo how the chunk has been
      ;; hidden, but it bluntly shows everything when showing a file.  But it
      ;; allows to show all chunks that were hidden twice - first time as a
      ;; chunk, second as a file.
      (remove-list-of-text-properties
       (compat-call pos-bol) ; Since Emacs-29
       (if-let* ((next-chunk
                  (difftastic--next-chunk file-chunk)))
           (save-excursion
             (goto-char next-chunk)
             (compat-call pos-eol -1)) ; Since Emacs-29
         (point-max))
       '(invisible difftastic)))))

(defun difftastic-toggle-chunk (&optional file-chunk)
  "Toggle visibility of chunk at point.
The point needs to be in chunk header.  When called with
FILE-CHUNK prefix hide all file chunks from the header to the end
of the file."
  (interactive "P" difftastic-mode)
  (when (difftastic--point-at-chunk-header-p file-chunk)
    (if (memq :hidden
              (get-text-property (compat-call pos-bol) ; Since Emacs-29
                                 'difftastic))
        (difftastic-show-chunk)
      (difftastic-hide-chunk file-chunk))))

(defun difftastic--chunk-bounds ()
  "Find bounds of a chunk at point.
The return value is cons wher car is the chunk beginning position and
cdr is chunk end position."
  (when-let* (((not (looking-at (rx line-start line-end))))
              (start-pos (or
                          (save-excursion
                            (goto-char (compat-call pos-bol)) ; Since Emacs-29
                            (when (looking-at (difftastic--chunk-regexp t))
                              (point)))
                          (difftastic--prev-chunk)))
              (end-pos (save-excursion
                         (goto-char (or (difftastic--next-chunk)
                                        (point-max)))
                         (goto-char (compat-call pos-eol 0)) ; Since Emacs-29
                         (while (looking-at (rx line-start line-end))
                           (goto-char (compat-call pos-eol 0))) ; Since Emacs-29
                         (point))))
    (cons start-pos end-pos)))

(defun difftastic--chunk-file-name (bounds)
  "Return name of the file of chunk at BOUNDS."
  (when-let* ((file-name (save-excursion
                           (goto-char (car bounds))
                           (when (looking-at (difftastic--chunk-regexp t))
                             (match-string-no-properties 1)))))
    (string-trim file-name)))

(eval-and-compile
  (defvar difftastic--line-num-digits 6
    "Maximum number of digits in line numbers in difftastic output.
The value of 6 allows for line numbers of up to 999,999.")

  (defun difftastic--line-num-rx (digits)
    "Return `rx' form for a up DIGITS long line number."
    `(seq ,(append
            ;; favor only digits or only dots up to given length
            `(or (group (** 1 ,digits digit))
                 (group (** 1 ,digits ".")))
            (let (num)
              (dotimes (spaces (1- digits))
                (push `(seq
                        ,(if (< 0 spaces)
                             `(** 1 ,(1+ spaces) " ")
                           `(= ,(1+ spaces) " "))
                        (or (group (= ,(- digits (1+ spaces)) digit))
                            (group (= ,(- digits (1+ spaces)) "."))))
                      num))
              ;; favor more digits (or dots)
              (nreverse num)))
          (or " " line-end)))

  (rx-define difftastic--line-num-rx
    (eval (difftastic--line-num-rx difftastic--line-num-digits)))

  (defun difftastic--line-num-or-spaces-rx (digits)
    "Return `rx' form for a up DIGITS long line number or up to DIGITS spaces."
    `(or
      ;; search for spaces first so they can be accounted as a left column
      (** 2 ,(1+ digits) " ")
      ,(difftastic--line-num-rx digits)))

  (rx-define difftastic--line-num-or-spaces-rx
    (eval (difftastic--line-num-or-spaces-rx difftastic--line-num-digits))))

(defun difftastic--classify-chunk (bounds)
  "Classify chunk at BOUNDS as either `single-column' or `side-by-side'."
  (save-excursion
    (goto-char (car bounds))
    (let ((single-column 0)
          (side-by-side 0))
      (while (< (progn
                  (goto-char (compat-call pos-bol 2)) ; Since Emacs-29
                  (point))
                (cdr bounds))
        (cond
         ((and
           (looking-at (rx line-start
                           difftastic--line-num-rx))
           (save-excursion
             (goto-char (match-end 0))
             (and
              (not (looking-at (rx difftastic--line-num-rx)))
              (looking-at (rx (one-or-more any)
                              difftastic--line-num-rx))))
           (cl-incf side-by-side)))
         ((looking-at (rx line-start
                          difftastic--line-num-or-spaces-rx
                          difftastic--line-num-or-spaces-rx))
          (cl-incf single-column))))
      (if (< side-by-side single-column)
          'single-column
        'side-by-side))))

(defun difftastic--parse-line-num (subexp prev)
  "Parse line number in current match data.
Return a list in a from (LINE-NUM BEG END), where LINE-NUM is a line
number (as a number) and BEG and END are positions where the number
begins and ends respectively.  LINE-NUM is extracted form a SUBEXP + M
match in current match data, where M is the first non-nil match after
the SUBEXP.  If no LINE-NUM can be extracted from SUBEXP + M match, then
PREV is used instead."
  (let ((m 1)
        beg)
    (while (and (<= m (* 2 difftastic--line-num-digits))
                (not (setq beg (match-beginning (+ subexp m)))))
      (cl-incf m))
    (when beg
      (let* ((end (match-end (+ subexp m)))
             (num (buffer-substring beg end)))
        (if (string-match-p (rx (one-or-more digit)) num)
            (list (string-to-number num) beg end)
          (list prev beg end))))))

(defun difftastic--parse-side-by-side-chunk (bounds)
  "Parse a `side-by-side-column' chunk at BOUNDS.
Return a list where each element is a list in a form (BEG-END LEFT
RIGHT).  BEG-END is a list in a from (BEG END) where BEG is a line begin
position and END is line end position.  Both LEFT and RIGHT are lists in
a form (LINE-NUM BEG END), where LINE-NUM is a line number and BEG and
END are positions where the line number begins and ends respectively."
  (save-excursion
    (goto-char (car bounds))
    (goto-char (compat-call pos-bol 2)) ; Since Emacs-29
    (let (lines
          prev-num-left)
      (while (re-search-forward
              (rx line-start difftastic--line-num-or-spaces-rx)
              (cdr bounds)
              t)
        (let ((left (difftastic--parse-line-num 0 prev-num-left))
              (beg-end (list (compat-call pos-bol) (compat-call pos-eol))) ; Since Emacs-29
              rights)
          ;; collect candidates for a right line number
          (while (re-search-forward
                  (rx (group " " (group (or (** 1 6 ".")
                                            (** 1 6 digit)))
                             (or " " line-end)))
                  (compat-call pos-eol) ; Since Emacs-29
                  t)
            (let ((right (difftastic--parse-line-num 1 nil)))
              ;; append column number
              (push (cons (1+ (- (caddr right) (compat-call pos-bol))) ; Since Emacs-29
                          right)
                    rights)))
          (setq prev-num-left (or (car left)
                                  prev-num-left))
          (push (list beg-end left rights) lines)))
      ;; find a common column accounting for missing line numbers
      (let (cols
            prev-num-right)
        (dolist (line lines)
          (when-let* ((right-cols (mapcar (lambda (candidate)
                                            (car candidate))
                                          (caddr line))))
            (setq cols
                  (cl-intersection (or cols right-cols)
                                   right-cols))))
        (setq lines (nreverse lines))
        ;; use the first common column that has been found,
        ;; also update missing line numbers in right
        (dolist (line lines)
          (setcdr (cdr line)
                  (list
                   (when-let* ((right (cdr (cl-find-if
                                            (lambda (candidate)
                                              (equal (car cols)
                                                     (car candidate)))
                                            (caddr line)))))
                     (setcar right (or (car right)
                                       prev-num-right))
                     (setq prev-num-right (car right))
                     right))))
        lines))))

(defun difftastic--parse-single-column-chunk (bounds)
  "Parse a `single-column' chunk at BOUNDS.
Return a list where each element is a list in a form (BEG-END LEFT
RIGHT).  BEG-END is a list in a from (BEG END) where BEG is a line begin
position and END is line end position.  Both LEFT and RIGHT are lists in
a form (LINE-NUM BEG END), where LINE-NUM is a line number and BEG and
END are positions where the line number begins and ends respectively."
  (save-excursion
    (goto-char (car bounds))
    (goto-char (compat-call pos-bol 2)) ; Since Emacs-29
    (let (lines
          prev-num-left
          prev-num-right)
      (while (re-search-forward
              (rx line-start
                  difftastic--line-num-or-spaces-rx
                  difftastic--line-num-or-spaces-rx)
              (cdr bounds)
              t)
        (let ((left (difftastic--parse-line-num 0 prev-num-left))
              (right (difftastic--parse-line-num
                      (* 2 difftastic--line-num-digits)
                      prev-num-right))
              (beg-end (list (compat-call pos-bol) (compat-call pos-eol)))) ; Since Emacs-29
          (setq prev-num-left (or (car left) prev-num-left)
                prev-num-right (or (car right) prev-num-right))
          (push (list beg-end left right) lines)))
      (nreverse lines))))

(defun difftastic--chunk-file-at-point ()
  "Return a chunk file at point.
Chunk file is a list in a form of (FILE LINE-NUM COL SIDE), where FILE
is the chunk file name, LINE-NUM is an optional line number within the
FILE (1 based), COL is a column number within the LINE (0 based), and
SIDE is either `left' or `right'."
  (when-let* ((bounds (difftastic--chunk-bounds))
              (file (difftastic--chunk-file-name bounds))
              (lines (pcase (difftastic--classify-chunk bounds)
                       ('side-by-side
                        (difftastic--parse-side-by-side-chunk bounds))
                       ('single-column
                        (difftastic--parse-single-column-chunk bounds))))
              (point (point)))
    (if (< point (caaar lines))
        ;; use right when point is in chunk header
        (list file nil 0 'right)
      (cl-block search-chunk-file
        (while lines
          (pcase-let* ((`((,bol ,eol) ,left ,right) (car lines)))
            (when (and (<= bol point eol))
              (if (and left
                       (or (not right)
                           (< point (cadr right))))
                  (cl-return-from search-chunk-file
                    (list file
                          (car left)
                          (max 0
                               (- point (caddr left) 1))
                          'left))
                (cl-return-from search-chunk-file
                  (list file
                        (car right)
                        (max 0
                             (- point (caddr right) 1))
                        'right))))
            (setq lines (cdr lines))))))))

(defun difftastic--diff-visit-file-setup (buffer line col)
  "Setup the BUFFER after visiting it.
Go to LINE and COL in the BUFFER, widening it if necessary.  After that
start a smerge session (if there are unmerged changes) and run
`difftastic-diff-visit-file-hook'."
  (if-let* ((win (get-buffer-window buffer 'visible)))
      (let ((pos (save-restriction
                   (widen)
                   (goto-char (point-min))
                   (min (+ (compat-call pos-bol (or line 1)) ; Since Emacs-29
                           col)
                        (compat-call pos-eol (or line 1)))))) ; Since Emacs-29
        (with-selected-window win
          (unless (<= (point-min) pos (point-max))
            (widen))
          (goto-char pos))
        (when (and buffer-file-name
                   (magit-anything-unmerged-p buffer-file-name))
          (smerge-start-session))
        (run-hooks 'difftastic-diff-visit-file-hook))
    (error "File buffer is not visible")))

(defun difftastic--diff-visit-file-or-buffer (chunk-file fn)
  "From a diff visit the appropriate file or buffer CHUNK-FILE.
The CHUNK-FILE is a list in a form of (FILE LINE-NUM COL SIDE), where
FILE is the chunk file name, LINE-NUM is an optional line number within
the FILE (1 based), COL is a column number within the LINE (0 based),
and SIDE is either `left' or `right'.  Use FN to display the buffer in
some window.  After visiting the FILE start a smerge session (if there
are unmerged changes) and run `difftastic-diff-visit-file-hook'."
  (pcase-let* ((`(,_ ,line ,col ,side) chunk-file)
               (file-buf (alist-get (if (eq side 'left)
                                        'file-buf-A
                                      'file-buf-B)
                                    difftastic--metadata))
               (file (car file-buf))
               (buf (or (when-let* ((buf (cdr file-buf)))
                          (if (buffer-live-p buf)
                              buf
                            (user-error "Buffer %s [%s] doesn't exist anymore"
                                        (if (eq side 'left) "A" "B") buf)))
                        (get-file-buffer file)
                        (find-file-noselect file))))
    (funcall fn buf)
    (difftastic--diff-visit-file-setup buf line col)
    buf))

(defun difftastic--diff-visit-git-file (chunk-file fn &optional force-worktree)
  "From a diff visit the appropriate version of CHUNK-FILE in git repository.
If FORCE-WORKTREE is non-nil, then visit the worktree version of the
file, even if the diff is about a committed change.  The CHUNK-FILE is a
list in a form of (FILE LINE-NUM COL SIDE), where FILE is the chunk file
name, LINE-NUM is an optional line number within the FILE (1 based), COL
is a column number within the LINE (0 based), and SIDE is either `left'
or `right'.  Use FN to display the buffer in some window.  After
visiting the FILE start a smerge session (if there are unmerged changes)
and run `difftastic-diff-visit-file-hook'."
  (pcase-let* ((`(,file ,line ,col ,side) chunk-file)
               (default-directory (alist-get 'default-directory
                                             difftastic--metadata))
               (rev (if-let* ((rev-or-range (alist-get 'rev-or-range
                                                       difftastic--metadata))
                              (range (cond
                                      ((and
                                        (stringp rev-or-range)
                                        (string-match-p (rx "..") rev-or-range))
                                       (string-split rev-or-range (rx "..")))
                                      ((stringp rev-or-range)
                                       (list (concat rev-or-range "^")
                                             rev-or-range)))))
                        (if (eq side 'left)
                            (car range)
                          (cadr range))
                      rev-or-range))
               (buf (if (or force-worktree
                            (and (not (stringp rev))
                                 (or (eq side 'right)
                                     difftastic-diff-visit-avoid-head-blob)))
                        (or (get-file-buffer file)
                            (find-file-noselect file))
                      (magit-find-file-noselect (if (stringp rev) rev "HEAD")
                                                file))))
    (when line
      (with-current-buffer buf
        (cond ((eq rev 'staged)
               (setq line
                     (magit-diff-visit--offset file nil line)))
              ((and force-worktree
                    (stringp rev))
               (setq line
                     (magit-diff-visit--offset file rev line))))))
    (funcall fn buf)
    (difftastic--diff-visit-file-setup buf line col)
    buf))

(defun difftastic--diff-visit-file (chunk-file fn &optional force-worktree)
  "From a diff visit the appropriate version of CHUNK-FILE.
If FORCE-WORKTREE is non-nil, then visit the worktree version of the
file, even if the diff is about a committed change.  The CHUNK-FILE is a
list in a form of (FILE LINE-NUM COL SIDE), where FILE is the chunk file
name, LINE-NUM is an optional line number within the FILE (1 based), COL
is a column number within the LINE (0 based), and SIDE is either `left'
or `right'.  Use FN to display the buffer in some window.  After
visiting the FILE start a smerge session (if there are unmerged changes)
and run `difftastic-diff-visit-file-hook'."
  (if chunk-file
      (if (assq 'git-command difftastic--metadata)
          (difftastic--diff-visit-git-file chunk-file fn force-worktree)
        (difftastic--diff-visit-file-or-buffer chunk-file fn))
    (user-error "No chunk file at point")))

(defun difftastic-diff-visit-file (chunk-file &optional other-window)
  "From a diff visit the appropriate version of CHUNK-FILE.
When called interactively, the point location inside the diff determines
which file (at which version) or which buffer is being visited.

1. If the diff shows differeces between files or buffers, for example a
   result of `difftastic-files' or `difftastic-buffers' then:
   a.  If the diff is a side-by-side diff (two columns) then:
       i.   if point is in \"left\" column then visit first file
            or first buffer,
       ii.  if point is in \"right\" column then visit second
            file or second buffer.
   b.  If the diff is a single column diff then:
       i.   if point is at first line number (\"left\") then visit
            first file or first buffer,
       ii.  if point is at second number (\"right\") or in diff
            content then visit second file or second buffer.

2. If the diff shows uncommitted changes (i.e., staged or unstaged
   changes), and point is in the \"right\" column (or at \"right\" line
   number) or `difftastic-diff-visit-avoid-head-blob' is non-nil then
   visit file in the working tree (i.e., the same \"real\" file that
   `find-file' would visit).

3. In all other cases visit a \"blob\" (i.e., the version of a file as
   stored in some commit).
   a.  If the diff is a side-by-side diff (two columns) then:
       i.   if point is in \"left\" column then visit blob at
            \"from\" endpoint,
       ii.  if point is in \"right\" column then visit blob at
            \"to\" endpoint.
   b.  If the diff is a single column diff then:
       i.   if point is at first line number (\"left\") then visit
            blob at \"from\" endpoint,
       ii.  if point is at second number (\"right\") or in the diff
            content then visit the blob \"to\" endpoint.

In the file-visiting buffer also go to the line that corresponds to the
line that point is on in the diff.

Display the buffer in the selected window.  With a prefix argument
OTHER-WINDOW display the buffer in another window instead.

After visiting the FILE start a smerge session (if there are unmerged
changes) and run `difftastic-diff-visit-file-hook'.

The CHUNK-FILE is a list in a form of (FILE LINE-NUM COL SIDE), where
FILE is the chunk file name, LINE-NUM is an optional line number within
the FILE (1 based), COL is a column number within the LINE (0 based),
and SIDE is either `left' or `right'.

Note that this command only works if point is inside a diff."
  (interactive (list (difftastic--chunk-file-at-point)
                     (when (equal current-prefix-arg '(4))
                       current-prefix-arg))
               difftastic-mode)
  (difftastic--diff-visit-file chunk-file (if other-window
                                              #'switch-to-buffer-other-window
                                            #'pop-to-buffer-same-window)))

(defun difftastic-diff-visit-file-other-window (chunk-file)
  "From a diff visit the appropriate version CHUNK-FILE in other window.
Like `difftastic-diff-visit-file', which see, but use
`switch-to-buffer-other-window'."
  (interactive (list (difftastic--chunk-file-at-point))
               difftastic-mode)
  (difftastic--diff-visit-file chunk-file #'switch-to-buffer-other-window))

(defun difftastic-diff-visit-file-other-frame (chunk-file)
  "From a diff visit the appropriate version CHUNK-FILE in other frame.
Like `difftastic-diff-visit-file', which see, but use
`switch-to-buffer-other-frame'."
  (interactive (list (difftastic--chunk-file-at-point))
               difftastic-mode)
  (difftastic--diff-visit-file chunk-file #'switch-to-buffer-other-frame))

(defun difftastic-diff-visit-worktree-file (chunk-file &optional other-window)
  "From a diff visit worktree version of CHUNK-FILE.
When called interactively the location of point inside the diff
determines which file is being visited.  This is similar to
`difftastic-diff-visit-file', which see, but always use the \"real\"
file when diff shows version control changes.

In the file-visiting buffer also go to the line that corresponds
to the line that point is on in the diff.  Lines that were added
or removed in the working tree, the index and other commits in
between are automatically accounted for.

Display the buffer in the selected window.  With a prefix
argument OTHER-WINDOW display the buffer in another window
instead.

After visiting the FILE start a smerge session (if there are unmerged
changes) and run `difftastic-diff-visit-file-hook'.

The CHUNK-FILE is a list in a form of (FILE LINE-NUM COL SIDE), where
FILE is the chunk file name, LINE-NUM is an optional line number within
the FILE (1 based), COL is a column number within the LINE (0 based),
and SIDE is either `left' or `right'."
  (interactive (list (difftastic--chunk-file-at-point)
                     (when (equal current-prefix-arg '(4))
                       current-prefix-arg))
               difftastic-mode)
  (difftastic--diff-visit-file chunk-file
                               (if other-window
                                   #'switch-to-buffer-other-window
                                 #'pop-to-buffer-same-window)
                               t))

(defun difftastic-diff-visit-worktree-file-other-window (chunk-file)
  "From a diff visit the worktree version of CHUNK-FILE in other window.
Like `difftastic-diff-visit-worktree-file', which see, but use
`switch-to-buffer-other-window'."
  (interactive (list (difftastic--chunk-file-at-point))
               difftastic-mode)
  (difftastic--diff-visit-file chunk-file #'switch-to-buffer-other-window t))

(defun difftastic-diff-visit-worktree-file-other-frame (chunk-file)
  "From a diff visit the worktree version of CHUNK-FILE in other frame.
Like `difftastic-diff-visit-worktree-file', which see, but use
`switch-to-buffer-other-frame'."
  (interactive (list (difftastic--chunk-file-at-point))
               difftastic-mode)
  (difftastic--diff-visit-file chunk-file #'switch-to-buffer-other-frame t))

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
  (when-let* ((difftastic-face
               (and (listp face)
                    (cl-find-if
                     (lambda (difftastic-face)
                       (and (string=
                             (face-foreground difftastic-face nil t)
                             (or
                              (plist-get face :foreground)
                              (car (alist-get :foreground face))))
                            (face-background difftastic-face nil t)
                            ;; ansi-color-* faces have the same
                            ;; foreground and background - don't use them
                            (not (string=
                                  (face-foreground difftastic-face nil t)
                                  (face-background difftastic-face nil t)))))
                     (vconcat difftastic-normal-colors-vector
                              difftastic-bright-colors-vector)))))
    ;; difftastic uses underline to highlight some changes.  It uses bold as
    ;; well, but it's not as unambiguous as underline.  Use underline to detect
    ;; highlight, but remove all attributes that are in
    ;; `difftastic-highlight-strip-face-properties'.
    (if-let* ((highlight-face (and (memq 'ansi-color-underline face)
                                   (alist-get difftastic-face
                                              difftastic-highlight-alist))))
        (let ((to-strip (delq
                         nil
                         (mapcar
                          (lambda (value-prop)
                            (when (memq
                                   (cdr value-prop)
                                   difftastic-highlight-strip-face-properties)
                              (car value-prop)))
                          '((ansi-color-underline . :underline)
                            (ansi-color-bold . :bold)
                            (ansi-color-italic . :italic)
                            (ansi-color-faint . :faint))))))
          (setq face (cl-delete-if
                      (lambda (value)
                        (or (memq value to-strip)
                            (and (listp value)
                                 (plist-member value :foreground))))
                      face))
          (cl-remf face :foreground)

          (push `(:foreground
                  ,(face-foreground highlight-face nil t))
                face)
          (push `(:background
                  ,(face-background highlight-face nil t))
                face))
      (when-let* ((fg (plist-get face :foreground)))
        (cl-remf face :foreground)
        (push `(:foreground ,fg) face))
      (push `(:background
              ,(face-background difftastic-face nil t))
            face)))
  face)

;; In practice there are only dozens or so different faces used,
;; so we can cache them each time anew.
(defvar-local difftastic--ansi-color-add-background-cache nil)

(defun difftastic--ansi-color-add-background-cached (orig-fun face-vec)
  "Memoise ORIG-FUN based on FACE-VEC.
Utilise `difftastic--ansi-color-add-background-cache' to cache
`ansi-color--face-vec-face' calls."
  (if-let* ((cached (assoc face-vec
                           difftastic--ansi-color-add-background-cache)))
      (cdr cached)
    (let ((face (difftastic--ansi-color-add-background
                 (funcall orig-fun face-vec))))
      (push (cons (difftastic--copy-tree face-vec) face)
            difftastic--ansi-color-add-background-cache)
      face)))

(defun difftastic--add-standard-args (difftastic-args requested-width)
  "Add standard arguments to DIFFTASTIC-ARGS, unless these are already present.
It adds \\='--color=always\\=', \\='--background=(light|dark)\\=', and
\\='--width=REQUESTED-WIDTH\\='."
  (save-match-data
    (unless (cl-find-if (lambda (arg)
                          (string-match (rx string-start "--background=") arg))
                        difftastic-args)
      (setq difftastic-args (cons (format "--background=%s"
                                          (frame-parameter nil 'background-mode))
                                  difftastic-args))
      (unless (cl-find-if (lambda (arg)
                            (string-match (rx string-start "--width=") arg))
                          difftastic-args)
        (setq difftastic-args (cons (format "--width=%s" requested-width)
                                    difftastic-args)))
      (unless (cl-find-if (lambda (arg)
                            (string-match (rx string-start "--color=") arg))
                          difftastic-args)
        (setq difftastic-args (cons "--color=always"
                                    difftastic-args)))))
  difftastic-args)

(defun difftastic--build-git-process-environment (requested-width
                                                  &optional difftastic-args)
  "Build a difftastic git command with REQUESTED-WIDTH.
The DIFFTASTIC-ARGS is a list of extra arguments to pass to
`difftastic-executable'."
  (let ((difftastic-args
         (mapcar (lambda (arg)
                   (replace-regexp-in-string (rx " ") "\\\\ " arg))
                 (difftastic--add-standard-args difftastic-args
                                                requested-width))))
    (cons (format
           "GIT_EXTERNAL_DIFF=%s%s"
           difftastic-executable
           (if difftastic-args
               (format " %s" (string-join difftastic-args " "))
             ""))
          process-environment)))

(defun difftastic--git-with-difftastic (buffer command rev-or-range
                                               &optional difftastic-args action)
  "Run COMMAND with GIT_EXTERNAL_DIFF then show result in BUFFER.
REV-OR-RANGE is the current git revision or range used by the COMMAND.
The DIFFTASTIC-ARGS is a list of extra arguments to pass to
`difftastic-executable'.  When ACTION is non-nil, it is exectued in
process sentinel."
  (let* ((requested-width (funcall difftastic-requested-window-width-function))
         (process-environment (difftastic--build-git-process-environment
                               requested-width
                               difftastic-args))
         (difftastic-display-buffer-function difftastic-display-buffer-function))
    (difftastic--run-command
     buffer
     command
     (lambda ()
       (setq difftastic--metadata
             `((default-directory . ,default-directory)
               (rev-or-range . ,rev-or-range)
               (git-command . ,command)
               (difftastic-args . ,difftastic-args)))
       (when action (funcall action))
       (funcall difftastic-display-buffer-function buffer requested-width)))))

(defun difftastic--run-command-filter (process string)
  "A process filter for `difftastic--run-command'.
It applies ANSI colors with `apply-ansi-colors' using difftastic
custom colors vectors.  The PROCESS and STRING are filter
arguments, like in `make-process''s filter."
  (when-let* ((buffer (and string
                           (process-buffer process))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (ansi-color-normal-colors-vector
             difftastic-normal-colors-vector)
            (ansi-color-bright-colors-vector
             difftastic-bright-colors-vector))
        (ignore ansi-color-normal-colors-vector
                ansi-color-bright-colors-vector)
        (if (fboundp 'ansi-color--face-vec-face) ;; Since Emacs-29
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
  '(("^-\\(?:U\\|-unified=\\)\\([0-9]+\\)$" . "--context=\\1"))
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

(defun difftastic--transform-diff-arguments (args difftastic-args)
  "Transform \\='git diff\\=' ARGS to be compatible with difftastic.
This converts some arguments to be compatible with difftastic (i.e.,
\\='-U\\=' to \\='--context\\=') and removes some that are
incompatible (i.e., \\='--stat\\=', \\='--no-ext-diff\\=').  The return
value is a list in a form of (GIT-ARGS DIFFT-ARGS), where GIT-ARGS are
arguments to be passed to \\='git\\=', and DIFFT-ARGS are arguments to
be passed to difftastic (merged with DIFFTASTIC-ARGS superseeding any
argument extracted from ARGS with the one from DIFFTASTIC-ARGS."
  (save-match-data
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
       (append
        (cl-remove-if
         (lambda (arg)
           (when-let* ((arg (car (string-split arg "=" t))))
             (member arg
                     (mapcar (lambda (difftastic-arg)
                               (car (string-split difftastic-arg "=" t)))
                             difftastic-args))))
         (delq
          nil
          (cl-mapcar
           (lambda (arg)
             (cl-dolist (regexp-replacement difftastic--transform-git-to-difft)
               (when (string-match (car regexp-replacement) arg)
                 (cl-return
                  (replace-match (cdr regexp-replacement) t nil arg)))))
           args)))
        difftastic-args)))))

(defun difftastic--git-diff-range (rev-or-range args files &optional difftastic-args)
  ;; checkdoc-params: (rev-or-range args files difftastic-args)
  "Implementation for `difftastic-git-diff-range', which see."
  (pcase-let* ((`(,git-args ,difftastic-args)
                (difftastic--transform-diff-arguments args difftastic-args))
               (buffer-name
                (concat
                 "*difftastic git diff"
                 (if git-args
                     (mapconcat #'identity (cons "" git-args) " ")
                   "")
                 (if rev-or-range (format " %s" rev-or-range)
                   "")
                 (if files
                     (mapconcat #'identity (cons " --" files) " ")
                   "")
                 "*")))
    (difftastic--git-with-difftastic
     (get-buffer-create buffer-name)
     `("git" "--no-pager" "diff" "--ext-diff"
       ,@(when git-args git-args)
       ,@(when (stringp rev-or-range) (list rev-or-range))
       ,@(when files (cons "--" files)))
     rev-or-range
     difftastic-args)))

(defun difftastic--get-languages ()
  "Return list of language overrides supported by difftastic."
  (append
   '("Text")
   (cl-remove-if (lambda (line)
                   (string-match-p "^ \\*" line))
                 (compat-call ;; Since Emacs-29
                  string-split
                  (shell-command-to-string
                   (concat difftastic-executable " --list-languages"))
                  "\n" t))))

(defclass difftastic--extra-arguments-prefix (transient-prefix)
  ())

(defun difftastic--extra-arguments-completing-overrides (languages)
  "Return a completion function for LANGUAGES overrides.
The returned function is designed for `completing-read' family of
functions to read language overrides."
  (let ((languages languages))
    (lambda (string predicate flag)
      (let ((case-fold-search t))
        (pcase (cons flag
                     (if (string-match (rx string-start
                                           (group (zero-or-more (not ":")))
                                           ":"
                                           (group (zero-or-more (not ":"))))
                                       string)
                         (list (if-let* ((glob (match-string 1 string))
                                         ((not (equal glob ""))))
                                   glob
                                 "*")
                               (match-string 2 string))
                       (list (if (equal string "") "*" string)
                             "")))
          (`(nil ,glob ,language)
           (let ((res (try-completion language languages predicate)))
             (if (stringp res)
                 (concat glob ":" res)
               res)))
          (`(t  ,glob ,language)
           (let ((res (all-completions language languages predicate)))
             (mapcar (lambda (c)
                       (concat glob ":" c))
                     res)))
          (`(lambda ,_ ,_)
           (if (string-match (rx-to-string `(seq
                                             string-start
                                             (one-or-more (not ":"))
                                             ":"
                                             (or ,@languages)))
                             string)
               t
             (message "'%s' doesn't match GLOB:LANGUAGE pattern" string)
             (sit-for 1)
             nil))
          (`((boundaries . ,suffix) ,_ ,language)
           (completion-boundaries language languages predicate suffix)))))))

(defvar helm-crm-default-separator)

(defun difftastic--extra-arguments-read-overrides (prompt initial-input history)
  ;; checkdoc-params: (prompt initial-input history)
  "Read language overrides."
  (let ((crm-separator ",")
        (helm-crm-default-separator ","))
    (magit-completing-read-multiple
     prompt
     (difftastic--extra-arguments-completing-overrides
      (difftastic--get-languages))
     nil
     t
     initial-input
     history)))

(defun difftastic--extra-arguments-override-prompt (_obj)
  "Return a prompt for `difftastic--extra-arguments-override-infix'."
  (with-temp-buffer
    (let ((overriding-local-map crm-local-completion-map))
      (concat
       "Comma separated list of glob:language, type "
       (substitute-command-keys "\\[crm-complete]")
       " for languages: "))))

(defun difftastic--extra-arguments-override-init-value (obj)
  "Initialize value of override OBJ with language override from `transient-scope'.
When the language override is a string, use it with * glob pattern.
Otherwise, when the language override is a list use it removing
\\='--override\\=' prefix from each element."
  (if-let* ((lang-override (car (transient-scope))))
      (oset obj value (if (stringp lang-override)
                          (list (format "*:%s" lang-override))
                        (mapcar (lambda (o)
                                  (string-remove-prefix "--override=" o))
                                lang-override)))
    (oset obj value (delq
                     nil
                     (mapcar
                      (lambda (arg)
                        (when (string-match (rx string-start
                                                "--override="
                                                (group (one-or-more any)))
                                            arg)
                          (match-string 1 arg)))
                      (alist-get 'difftastic-args difftastic--metadata))))))

(cl-defmethod transient-init-value ((obj difftastic--extra-arguments-prefix))
  "Set default values in OBJ from `difftastic-metadata'.
The \\='--override\\=' argument is handled in
`difftastic--extra-arguments-override-init-value', which see."
  (oset obj value
        (cl-remove-if (lambda (arg)
                        (string-match (rx string-start
                                          "--override=")
                                      arg))
                      (alist-get 'difftastic-args difftastic--metadata))))

(transient-define-infix difftastic--extra-arguments-override-infix ()
  :prompt #'difftastic--extra-arguments-override-prompt
  :multi-value t
  :class 'transient-option
  :reader #'difftastic--extra-arguments-read-overrides
  :argument "--override="
  :init-value #'difftastic--extra-arguments-override-init-value)

(defun difftastic--extra-arguments-call-command-description ()
  "Return description for call command suffix."
  (format "run difftastic%s"
          (if-let* ((command (cadr (transient-scope))))
              (concat " ("
                      (propertize (replace-regexp-in-string
                                   (rx "--") "-" (symbol-name command))
                                  'face 'font-lock-constant-face)
                      ")")
            "")))

(transient-define-suffix difftastic--extra-arguments-call-command ()
  "Call command from `transient-scope' extra difftastic arguments.
Difftastic arguments are like `transient-args', but ensure the
\\='--override\\=' argument is exploded."
  :transient 'transient--do-exit
  :key "C-c C-c"
  :description #'difftastic--extra-arguments-call-command-description
  (interactive)
  (pcase-let ((`(,_ ,fun ,args) (transient-scope))
              (difft-args
               (apply #'append
                      (mapcar (lambda (arg)
                                (if (and (listp arg)
                                         (equal "--override=" (car arg)))
                                    (mapcar (lambda (o)
                                              (format "--override=%s" o))
                                            (cdr arg))
                                  (list arg)))
                              (transient-args
                               (oref transient-current-prefix command))))))
    (when (functionp fun)
      (apply fun (append args (list difft-args))))))

(transient-define-prefix difftastic--with-extra-arguments (lang-override fun &rest args)
  "Call FUN with ARGS and extra difftastic arguments.
Number of ARGS must be equal to number of arguments that FUN takes minus
1. The last argument will be a list of extra difftastic arguments.
The LANG-OVERRIDE will be used to initialize language overrides."
  :class 'difftastic--extra-arguments-prefix
  :man-page "difft"
  ["Difftastic arguments"
   ("-o" "language overrides" difftastic--extra-arguments-override-infix)
   ("-s" "strip cr" "--strip-cr="
    :choices ("on" "off"))
   ("-c" "context" "--context="
    :reader transient-read-number-N+)
   ("-t" "tab width" "--tab-width="
    :reader transient-read-number-N+)
   ("-i" "ignore comments" "--ignore-comments")
   ("-d" "display mode" "--display="
    :choices ("side-by-side" "side-by-side-show-both" "inline"))
   ("-h" "syntax highlight" "--syntax-highlight="
    :choices ("on" "off"))
   ("-p" "sort paths" "--sort-paths")
   ("--skip-unchanged" "skip unchanged" "--skip-unchanged"
    :level 5)
   ("--color" "use color" "--color="
    :choices ("always" "auto" "never")
    :level 5)
   ("--background" "background brightness" "--background"
    :choices ("dark" "light")
    :level 5)
   ("--width" "line wraping columng" "--width"
    :reader transient-read-number-N+
    :level 5)
   ("--byte-limit" "byte limit" "--byte-limit="
    :reader transient-read-number-N+
    :level 5)
   ("--graph-limit" "graph limit" "--graph-limit="
    :reader transient-read-number-N+
    :level 5)
   ("--error-limit" "parse error limit" "--parse-error-limit="
    :reader transient-read-number-N+
    :level 5)]

  [(difftastic--extra-arguments-call-command)]
  (interactive)
  (transient-setup #'difftastic--with-extra-arguments nil nil
                   :scope (append (list lang-override fun) (list args))))

(defun difftastic--format-override-arg (lang-or-args)
  "Format LANG-OR-ARGS to be usable with difftastic call."
  (if (stringp lang-or-args)
      (list (format "--override=*:%s" lang-or-args))
    lang-or-args))

;;;###autoload
(defun difftastic-git-diff-range (&optional rev-or-range args files)
  "Show difference between two commits using difftastic.
The meaning of REV-OR-RANGE, ARGS, and FILES is like in
`magit-diff-range', but ARGS are adjusted for difftastic with
`difftastic--transform-diff-arguments'.  When called with double prefix
argument ask for extra arguments for difftastic call."
  (interactive (cons (magit-diff-read-range-or-commit
                      "Diff for range"
                      nil
                      (when (equal current-prefix-arg '(4))
                        current-prefix-arg))
                     (magit-diff-arguments)))
  (if (equal current-prefix-arg '(16))
      (difftastic--with-extra-arguments nil
                                        #'difftastic--git-diff-range
                                        rev-or-range
                                        args
                                        files)
    (difftastic--git-diff-range rev-or-range args files)))

(defun difftastic--magit-diff (args files &optional difftastic-args)
  ;; checkdoc-params: (args files difftastic-args)
  "Implementation for `difftastic-magit-diff', which see."
  (let ((default-directory (magit-toplevel))
        (section (magit-current-section)))
    (cond
     ((magit-section-match 'module section)
      (setq default-directory
            (expand-file-name
             (file-name-as-directory (oref section value))))
      (difftastic--git-diff-range (oref section range) nil nil difftastic-args))
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
         (difftastic--git-diff-range (magit--merge-range)
                                     args
                                     files
                                     difftastic-args))
        ('unstaged
         (difftastic--git-diff-range 'unstaged args files difftastic-args))
        ('staged
         (let ((file (magit-file-at-point)))
           (if (and file (equal (cddr (car (magit-file-status file)))
                                '(?D ?U)))
               ;; File was deleted by us and modified by them.  Show the latter.
               (progn
                 (unless (magit-merge-in-progress-p)
                   (user-error "No merge is in progress"))
                 (difftastic--git-diff-range
                  (magit--merge-range) args (list file) difftastic-args))
             (difftastic--git-diff-range
              'staged (cl-pushnew "--cached" args :test #'string=)
              files
              difftastic-args))))
        (`(stash . ,value)
         ;; ATM, `magit-diff--dwim' evaluates to `commit' when point is on stash
         ;; section
         (difftastic--git-diff-range (format "%s^..%s" value value)
                                     args
                                     files
                                     difftastic-args))
        (`(commit . ,value)
         (difftastic--git-diff-range (format "%s^..%s" value value)
                                     args
                                     files
                                     difftastic-args))
        ((and range (pred stringp))
         (difftastic--git-diff-range range
                                     args
                                     files
                                     difftastic-args))
        (_
         (apply #'difftastic--git-diff-range
                (magit-diff-read-range-or-commit
                 "Diff for range"
                 nil
                 (when (equal current-prefix-arg '(4))
                   current-prefix-arg))
                args
                files
                (list difftastic-args))))))))

;;;###autoload
(defun difftastic-magit-diff (&optional args files)
  "Show the result of \\='git diff ARGS -- FILES\\=' with difftastic.
When called with double prefix argument ask for extra arguments for
difftastic call.  When called with double prefix argument ask for extra
arguments for difftastic call."
  (interactive (magit-diff-arguments))
  (if (equal current-prefix-arg '(16))
      (difftastic--with-extra-arguments nil
                                        #'difftastic--magit-diff
                                        args
                                        files)
    (difftastic--magit-diff args files nil)))

(defun difftastic--magit-show (rev &optional difftastic-args)
  ;; checkdoc-params: (rev difftastic-args)
  "Implementation for `difftastic-magit-show', which see."
  (if rev
      (difftastic--git-with-difftastic
       (get-buffer-create (concat "*difftastic git show " rev "*"))
       (list "git" "--no-pager" "show" "--ext-diff" rev)
       rev
       difftastic-args)
    (user-error "No revision specified")))

;;;###autoload
(defun difftastic-magit-show (rev)
  "Show the result of \\='git show REV\\=' with difftastic.
When REV couldn't be guessed or called with prefix arg ask for REV.
When called with double prefix argument ask for extra arguments for
difftastic call."
  (interactive
   (list (or
          ;; If not invoked with prefix arg, try to guess the REV from
          ;; point's position.
          (and (not (equal current-prefix-arg '(4)))
               (or (magit-thing-at-point 'git-revision t)
                   (magit-branch-or-commit-at-point)))
          ;; Otherwise, query the user.
          (magit-read-branch-or-commit "Revision"))))
  (if (equal current-prefix-arg '(16))
      (difftastic--with-extra-arguments nil #'difftastic--magit-show rev)
    (difftastic--magit-show rev)))

(defun difftastic--goto-line-col-in-chunk (line col)
  "Goto LINE and COLUMN in a `difftastic' buffer.
LINE is the line in right side of a chunk (if exists), not the line in
the buffer.  COL is is column in right side of the chunk."
  (let ((line (if (numberp line)
                  (format "%d" line)
                line)))
    (when (re-search-forward
           (rx-to-string `(seq line-start
                               difftastic--line-num-or-spaces-rx
                               (zero-or-one (seq (+? not-newline) " "))
                               ,line (or " " line-end)))
           nil t)
      (goto-char (min (compat-call pos-eol) ; Since Emacs-29
                      (+ (point) col))))))

(defun difftastic--magit-diff-buffer-file (&optional difftastic-args)
  ;; checkdoc-params: (difftastic-args)
  "Implementation for `difftastic-magit-diff-buffer-file'."
  (if-let* ((default-directory (magit-toplevel))
            (file (magit-file-relative-name)))
      (if-let* ((rev magit-buffer-refname))
          (difftastic--git-with-difftastic
           (get-buffer-create
            (concat "*difftastic git show " rev "*"))
           (list "git" "--no-pager" "show" "--ext-diff" rev "--" file)
           rev
           difftastic-args)
        (save-buffer)
        (let ((line (line-number-at-pos))
              (col (current-column)))
          (difftastic--git-with-difftastic
           (get-buffer-create
            "*difftastic git diff unstaged*")
           (list "git" "--no-pager" "diff" "--ext-diff"
                 (or (magit-get-current-branch) "HEAD")
                 "--" file)
           'unstaged
           difftastic-args
           (lambda ()
             (difftastic--goto-line-col-in-chunk line col)))))
    (user-error "Buffer isn't visiting a file")))

;;;###autoload
(defun difftastic-magit-diff-buffer-file ()
  "Show diff for the blob or file visited in the current buffer.
When the buffer visits a blob, then show the respective commit.  When
the buffer visits a file, then show the differences between `HEAD' and
the working tree.  In both cases limit the diff to the file or blob.
When called with double prefix argument ask for extra arguments for
difftastic call."
  (interactive)
  (if (equal current-prefix-arg '(16))
      (difftastic--with-extra-arguments nil
                                        #'difftastic--magit-diff-buffer-file)
    (difftastic--magit-diff-buffer-file)))

(defun difftastic--file-extension-for-mode (mode)
  "Return a file extension for MODE."
  (alist-get
   mode
   (cl-remove-duplicates
    (delq
     nil
     (apply
      #'append
      (mapcar
       (lambda (ext)
         (when-let* ((mode (assoc-default ext
                                          auto-mode-alist
                                          'string-match))
                     ((functionp mode))
                     (ts-mode (intern (string-replace
                                       "-mode"
                                       "-ts-mode"
                                       (symbol-name mode))))
                     (ext (string-replace "*" "" ext)))
           (list (cons mode ext)
                 (cons ts-mode ext))))
       (cl-remove-if-not
        (lambda (str)
          (string-match (rx string-start "*.") str))
        (string-split
         (shell-command-to-string
          (concat difftastic-executable " --list-languages")))))))
    :test (lambda (lhs rhs)
            (eq (car lhs) (car rhs))))))

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
                      nil
                      (difftastic--file-extension-for-mode major-mode)
                      (buffer-string)))))

(defun difftastic--get-file-buf (prefix buffer)
  "If BUFFER visits a file return it else create a temporary file with PREFIX.
The return value is a cons in a form of (FILE . BUF) where FILE
is the file and BUF either is  nil if this is non temporary file,
or BUF is set to BUFFER if this is a temporary file."
  (let* (buf
         (file
          (if-let* ((buffer-file (buffer-file-name buffer)))
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
  (when-let* ((file (car file-buf)))
    (when (and (cdr file-buf) (stringp file) (file-exists-p file))
      (delete-file file))))

(defun difftastic--make-suggestion (languages buffer-A buffer-B)
  "Suggest one of LANGUAGES based on mode of BUFFER-A and BUFFER-B."
  (when-let* ((mode
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
                                                   &optional difftastic-args)
  "Build a difftastic command to compare files from FILE-BUF-A and FILE-BUF-B.
The FILE-BUF-A and FILE-BUF-B are conses where car is the file
and cdr is a buffer when it is a temporary file and nil otherwise.
REQUESTED-WIDTH is passed to difftastic as \\='--width\\=' argument.
DIFFTASTIC-ARGS are passed to difftastic."
  `(,difftastic-executable
    ,@(difftastic--add-standard-args difftastic-args requested-width)
    ,(car file-buf-A)
    ,(car file-buf-B)))

(defun difftastic--files-internal (buffer file-buf-A file-buf-B
                                          &optional difftastic-args)
  "Run difftastic on FILE-BUF-A and FILE-BUF-B and show results in BUFFER.
The FILE-BUF-A and FILE-BUF-B are conses where car is the file
and cdr is a buffer when it is a temporary file and nil otherwise.
DIFFTASTIS-ARGS are passed to difftastic."
  (let ((requested-width (funcall difftastic-requested-window-width-function))
        (difftastic-display-buffer-function difftastic-display-buffer-function))
    (difftastic--run-command
     buffer
     (difftastic--build-files-command file-buf-A
                                      file-buf-B
                                      requested-width
                                      difftastic-args)
     (lambda ()
       (setq difftastic--metadata
             `((default-directory . ,default-directory)
               (difftastic-args . ,difftastic-args)
               (file-buf-A . ,file-buf-A)
               (file-buf-B . ,file-buf-B)))
       (funcall difftastic-display-buffer-function buffer requested-width)
       (difftastic--delete-temp-file-buf file-buf-A)
       (difftastic--delete-temp-file-buf file-buf-B)))))

(defmacro difftastic--with-file-bufs (file-bufs &rest body)
  "Exectue the forms in BODY with each spec in FILE-BUFS list `let'-bound.
Each element in FILE-BUFS is in a form of SYMBOL (which is a
symbol bound to nil) or a list (SYMBOL VALUEFORM) (which binds
SYMBOL to the value of VALUEFORM).  Each VALUEFORM value is bound
sequentially and can reffer to symbols already bound by this
FILE-BUFS.  If any of VALUEFORM or any form in BODY signals an
error each symbol in FILE-BUFS will be passed to
`difftastic--delete-temp-file-buf'."
  (declare (indent 1) (debug ((&rest &or (sexp form) sexp) body)))
  `(let ,(mapcar (lambda (file-buf)
                   (cond
                    ((symbolp file-buf) file-buf)
                    ((and (listp file-buf)
                          (equal (length file-buf) 2)
                          (symbolp (car file-buf)))
                     (car file-buf))
                    (t (error (concat "Wrong type argument: symbolp or "
                                      "(SYMBOL VALUEFORM), %S")
                              file-buf))))
                 file-bufs)
     (condition-case err
         (progn
           ,(apply #'append
                   '(setq) ; empty `setq' should be fine
                   (mapcar (lambda (file-buf)
                             (when (listp file-buf)
                               file-buf))
                           file-bufs))
           ,@body)
       (error
        (dolist (file-buf
                 (list ,@(mapcar
                          (lambda (file-buf)
                            (cond
                             ((symbolp file-buf)  file-buf)
                             ((listp file-buf) (car file-buf))))
                          file-bufs)))
          (difftastic--delete-temp-file-buf file-buf))
        (signal (car err) (cdr err))))))

(defun difftastic--buffers-args ()
  "Return arguments for `difftastic-buffers'."
  ;; adapted from `ediff-buffers'
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
          (when (or (equal current-prefix-arg '(4))
                    (and (not (buffer-file-name (get-buffer bf-A)))
                         (not (buffer-file-name (get-buffer bf-B)))))
            (let* ((languages (difftastic--get-languages))
                   (suggested (difftastic--make-suggestion
                               languages
                               (get-buffer bf-A)
                               (get-buffer bf-B))))
              (completing-read "Language: " languages nil t suggested))))))

(defun difftastic--buffers (buffer-A buffer-B lang-or-args)
  ;; checkdoc-params: (buffer-A buffer-B lang-or-args)
  "Implementation of `difftastic-buffers', which see."
  (difftastic--with-file-bufs ((file-buf-A (difftastic--get-file-buf
                                            "A" (get-buffer buffer-A)))
                               (file-buf-B (difftastic--get-file-buf
                                            "B" (get-buffer buffer-B))))
    (difftastic--files-internal
     (get-buffer-create
      (concat "*difftastic " buffer-A " " buffer-B "*"))
     file-buf-A
     file-buf-B
     (difftastic--format-override-arg lang-or-args))))

;;;###autoload
(defun difftastic-buffers (buffer-A buffer-B &optional lang-override)
  "Run difftastic on a pair of buffers, BUFFER-A and BUFFER-B.
Optionally, provide a LANG-OVERRIDE to override language used.
See \\='difft --list-languages\\=' for language list.

When:
- either LANG-OVERRIDE is nil and neither of BUFFER-A nor
BUFFER-B is a file buffer,
- or function is called with a prefix arg,

then ask for language before running difftastic.  When called with
double prefix argument ask for extra arguments for difftastic call."
  (interactive (difftastic--buffers-args))
  (if (equal current-prefix-arg '(16))
      (difftastic--with-extra-arguments lang-override
                                        #'difftastic--buffers
                                        buffer-A
                                        buffer-B)
    (difftastic--buffers buffer-A buffer-B lang-override)))

(defun difftastic--files-args ()
  "Return arguments for `difftastic-files'."
  ;; adapted from `ediff-files'
  (let ((dir-A (if difftastic-use-last-dir
                   difftastic--last-dir-A
                 default-directory))
        dir-B f ff)
    (prog1
        (list (setq f (ediff-read-file-name
                       "File A to compare"
                       dir-A
                       (ediff-get-default-file-name)))
              (let ((file-name-history file-name-history))
                (setq dir-B (if difftastic-use-last-dir
                                difftastic--last-dir-B
                              (file-name-directory f)))
                (add-to-history 'file-name-history
                                (ediff-abbreviate-file-name
                                 (expand-file-name
                                  (file-name-nondirectory f)
                                  dir-B)))
                (setq ff (ediff-read-file-name
                          "File B to compare"
                          dir-B
                          (ediff-get-default-file-name f 1))))
              (when (equal current-prefix-arg '(4))
                (completing-read "Language: "
                                 (difftastic--get-languages)
                                 nil
                                 t)))
      (add-to-history 'file-name-history (abbreviate-file-name f))
      (add-to-history 'file-name-history (abbreviate-file-name ff))
      (setq difftastic--last-dir-A (file-name-as-directory
                                    (file-name-directory f)))
      (setq difftastic--last-dir-B (file-name-as-directory
                                    (file-name-directory ff))))))

(defun difftastic--files (file-A file-B &optional lang-or-args)
  ;; checkdoc-params: (file-A file-B lang-or-args)
  "Implementation for `difftastic-files', which see."
  (difftastic--files-internal
   (get-buffer-create (concat "*difftastic "
                              (file-name-nondirectory file-A)
                              " "
                              (file-name-nondirectory file-B)
                              "*"))
   (cons file-A nil)
   (cons file-B nil)
   (difftastic--format-override-arg lang-or-args)))

;;;###autoload
(defun difftastic-files (file-A file-B &optional lang-override)
  "Run difftastic on a pair of files, FILE-A and FILE-B.
Optionally, provide a LANG-OVERRIDE to override language used.  See
\\='difft --list-languages\\=' for language list.  When function is
called with a prefix arg then ask for language before running
difftastic.  When called with double prefix argument ask for extra
arguments for difftastic call."
  (interactive (difftastic--files-args))
  (if (equal current-prefix-arg '(16))
      (difftastic--with-extra-arguments lang-override
                                        #'difftastic--files
                                        file-A
                                        file-B)
    (difftastic--files file-A
                       file-B
                       lang-override)))

(defun difftastic--dired-diff (file lang-override)
  ;; checkdoc-params: (file lang-override)
  "Implementation for `difftastic--dired-diff', which see."
  (cl-letf (((symbol-function 'diff)
             (lambda (current file &rest _)
               (difftastic-files current file lang-override)))
            (current-prefix-arg nil))
    (if (eq file 'interactive)
        (call-interactively #'dired-diff)
      (funcall #'dired-diff file))))

;;;###autoload
(defun difftastic-dired-diff (file &optional lang-override)
  "Compare file at point with FILE using difftastic.
The behavior is the same as `dired-diff', except for the prefix argument, which
makes the function prompt for LANG-OVERRIDE.  See \\='difft
--list-languages\\=' for language list.   When called with double prefix
argument ask for extra arguments for difftastic call."
  (interactive
   (list 'interactive
         (when (equal current-prefix-arg '(4))
           (completing-read "Language: " (difftastic--get-languages) nil t)))
   dired-mode)
  (if (equal current-prefix-arg '(16))
      (difftastic--with-extra-arguments lang-override
                                        #'difftastic--dired-diff
                                        file)
    (difftastic--dired-diff file lang-override)))

(defun difftastic--rerun-file-buf (prefix file-buf metadata)
  "Create a new temporary file for the FILE-BUF with PREFIX if needed.
The new FILE-BUF is additionally set in RERUN-ALIST.  The FILE-BUF
is a cons where car is the file and cdr is a buffer when it is a
temporary file or nil otherwise."
  (if-let* ((buffer (cdr file-buf)))
      (if (buffer-live-p buffer)
          (setf (alist-get (intern (concat "file-buf-" prefix)) metadata)
                (difftastic--get-file-buf prefix buffer))
        (user-error "Buffer %s [%s] doesn't exist anymore" prefix buffer))
    file-buf))

(defun difftastic--rerun (lang-or-args)
  ;; checkdoc-params: (lang-or-args)
  "Implementation for `difftastic-rerun', which see."
  (if-let* (((eq major-mode 'difftastic-mode))
            (metadata (copy-tree difftastic--metadata)))
      (let-alist metadata
        (difftastic--with-file-bufs ((file-buf-A (difftastic--rerun-file-buf
                                                  "A" .file-buf-A metadata))
                                     (file-buf-B (difftastic--rerun-file-buf
                                                  "B" .file-buf-B metadata)))
          (let* ((default-directory .default-directory)
                 (difftastic-args
                  (if-let* (((stringp lang-or-args))
                            (override (difftastic--format-override-arg
                                       lang-or-args)))
                      (save-match-data
                        (append override
                                (cl-remove-if
                                 (lambda (arg)
                                   (string-match (rx string-start
                                                     "--override=")
                                                 arg))
                                 .difftastic-args)))
                    (or lang-or-args .difftastic-args)))
                 (requested-width
                  (funcall (or
                            difftastic-rerun-requested-window-width-function
                            difftastic-requested-window-width-function)))
                 (process-environment
                  (if .git-command
                      (difftastic--build-git-process-environment
                       requested-width
                       difftastic-args)
                    process-environment))
                 (command (or .git-command
                              (difftastic--build-files-command
                               file-buf-A
                               file-buf-B
                               requested-width
                               difftastic-args)))
                 (buffer (current-buffer)))
            (setf (alist-get 'difftastic-args metadata)
                  difftastic-args)
            (difftastic--run-command
             buffer
             command
             (lambda ()
               (setq difftastic--metadata metadata)
               (difftastic--delete-temp-file-buf file-buf-A)
               (difftastic--delete-temp-file-buf file-buf-B))))))
    (user-error "Nothing to rerun")))

;;;###autoload
(defun difftastic-rerun (&optional lang-override)
  "Rerun difftastic in the current buffer.
Optionally, provide a LANG-OVERRIDE to override language used.  See
\\='difft --list-languages\\=' for language list.  When function is
called with a prefix arg (single of double) then ask for language before
running difftastic.

In order to determine requested width for difftastic a call to
`difftastic-rerun-requested-window-width-function' is made.  When the
latter is set to nil the call is made to
`difftastic-requested-window-width-function'.  When called with double
prefix argument ask for extra arguments for difftastic call."
  (interactive (list
                (when (equal current-prefix-arg '(4))
                  (completing-read "Language: "
                                   (difftastic--get-languages) nil t)))
               difftastic-mode)
  (if (equal current-prefix-arg '(16))
      (difftastic--with-extra-arguments lang-override #'difftastic--rerun)
    (difftastic--rerun lang-override)))

;;; LocalWords: unmerged unstaged smerge

(provide 'difftastic)
;;; difftastic.el ends here
