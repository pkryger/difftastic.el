# difftastic.el - Wrapper for difftastic

[![MELPA](https://melpa.org/packages/difftastic-badge.svg)](https://melpa.org/#/difftastic)

## Description

The `difftastic` Emacs package is designed to integrate [`difftastic`](https://github.com/wilfred/difftastic) - a structural diff tool - into your Emacs workflow, enhancing your code review and comparison experience. This package automatically displays `difftastic`'s output within Emacs using faces from your user theme, ensuring consistency with your overall coding environment.

## Features

- Configure faces to your likening. By default `magit-diff-*` faces from your user them are used for consistent visual experience.
- Chunks and file navigation using <kbd>n</kbd>/<kbd>N</kbd> and <kbd>p</kbd>/<kbd>P</kbd> in generated diffs.
- DWIM workflows from `magit`.
- Rerun `difftastic` with <kbd>g</kbd> to use your window width and/or to force language change (when called with prefix).

## Installation

1. Clone this repository to a directory of your choice.
2. Add the following lines to your Emacs configuration file (usually `~/.emacs` or `~/.emacs.d/init.el`):

```emacs-lisp
(add-to-list 'load-path "/path/to/difftastic.el")
(require 'difftastic)
```

## Configuration

To configure `difftastic` commands in `magit-diff` prefix, use the following code snippet in your Emacs configuration:

```emacs-lisp
(require 'difftastic)

;; Add commands to a `magit-difftastic'
(eval-after-load 'magit-diff
  '(transient-append-suffix 'magit-diff '(-1 -1)
     [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
      ("S" "Difftastic show" difftastic-magit-show)]))
(add-hook 'magit-blame-read-only-mode-hook
          (lambda ()
            (keymap-set magit-blame-read-only-mode-map
                        "D" #'difftastic-magit-show)
            (keymap-set magit-blame-read-only-mode-map
                        "S" #'difftastic-magit-show)))
```

Or, if you use `use-package`:
```emacs-lisp
(use-package difftastic
  :demand t
  :bind (:map magit-blame-read-only-mode-map
         ("D" . difftastic-magit-show)
         ("S" . difftastic-magit-show))
  :config
  (eval-after-load 'magit-diff
    '(transient-append-suffix 'magit-diff '(-1 -1)
       [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
        ("S" "Difftastic show" difftastic-magit-show)])))
```

## Usage
The following commands are meant to help to interact with `difftastic`. Commands are followed by their default keybindings in `difftastic-mode` (in parenthesis).

- `difftastic-magit-diff` - show the result of `git diff ARGS -- FILES` with `difftastic`. This is the main entry point for DWIM action, so it tries to guess revision or range.
- `difftastic-magit-show` - show the result of `git show ARG` with `difftastic`. It tries to guess `ARG`, and ask for it when can't. When called with prefix argument it will ask for `ARG`.
- `difftastic-files` - show the result of `difft FILE-A FILE-B`. When called with prefix argument it will ask for language to use, instead of relaying on `difftastic`'s detection mechanism.
- `difftastic-buffers` - show the result of `difft BUFFER-A BUFFER-B`. Language is guessed based on buffers modes. When called with prefix argument it will ask for language to use.
- `difftastic-rerun` (<kbd>g</kbd>) - rerun difftastic for the current buffer. It runs difftastic again in the current buffer, but respects the window configuration.
It uses `difftastic-rerun-requested-window-width-function` which, by default, returns current window width (instead of `difftastic-requested-window-width-function`). It will also reuse current buffer and will not call `difftastic-display-buffer-function`. When called with prefix argument it will ask for language to use.
- `difftastic-next-chunk` (<kbd>n</kbd>), `difftastic-next-file` (<kbd>N</kbd>) - move point to a next logical chunk or a next file respectively.
- `difftastic-previous-chunk` (<kbd>p</kbd>), `difftastic-previous-file` (<kbd>P</kbd>) - move point to a previous logical chunk or a previous file respectively.
- `difftastic-git-diff-range` - transform `ARGS` for difftastic and show the result of `git diff ARGS REV-OR-RANGE -- FILES` with `difftastic`.

## Customization

### Face Customization

You can customize the appearance of `difftastic` output by adjusting the faces used for highlighting. To customize a faces, use the following code snippet in your configuration:

```emacs-lisp
;; Customize faces used to display difftastic output.
(setq difftastic-normal-colors-vector
  (vector
   (aref ansi-color-normal-colors-vector 0) ; use black face from `ansi-color'
   (aref difftastic-normal-colors-vector 1) ; use face for removed marker from `difftastic'
   (aref difftastic-normal-colors-vector 2) ; use face for added markder from `difftastic'
   'my-section-face
   'my-comment-face
   'my-string-face
   'my-warning-face
   (aref ansi-color-normal-colors-vector 7) ; use white face from `ansi-color'
   )

;; Customize highlight faces
(setq difftastic-highlight-alist
  `((,(aref difftastic-normal-colors-vector 2) . my-added-highlight)
    (,(aref difftastic-normal-colors-vector 1) . my-removed-highlight)))

;; Disable highlight faces (use difftastic's default)
(setq difftastic-highlight-alist nil)
```

### Window management

The `difftastic` relies on the `difft` command line tool to produce an output that can be displayed in an Emacs buffer window. In short: it runs the `difft`, converts ANSI codes into user defined colors and displays it in window. The `difft` can be instructed with a hint to help it produce a content that can fit into user output, by specifying a requested width. However, the latter is not always respected.

The `difftastic` provides a few variables to let you customize these aspects of interaction with `difft`:
- `difftastic-requested-window-width-function` - this function is called for a first (i.e., not a rerun) call to `difft`. It shall return the requested width of the output. For example this can be a half of a current frame (or a window) if the output is meant to be presented side by side.
- `difftastic-rerun-requested-window-width-function` - this function is called for a rerun (i.e., not a first) call to `difft`. It shall return requested window width of the output. For example this can be a current window width if the output is meant to fill the whole window.
- `difftastic-display-buffer-function` - this function is called after a first call to `difft`. It is meant to select an appropriate Emacs mechanism to display the `difft` output.

## Acknowledgments

This package was inspired by the need for an integration of `difftastic` within Emacs, enhancing the code review process for developers.

This work is based on Tassilo Horn's [blog entry](https://tsdh.org/posts/2022-08-01-difftastic-diffing-with-magit.html).

`magit-diff` keybindings and a concept of updating faces comes from a Shiv Jha-Mathur's [blog entry](https://shivjm.blog/better-magit-diffs/).

This all has been strongly influenced by - a class in itself - Magit and Transient Emacs packages by Jonas Bernoulli.

## Similar packages

### Diff ANSI

There's a [`diff-ansi`](https://codeberg.org/ideasman42/emacs-diff-ansi) package available. I haven't spent much time on it, but at a first glance it doesn't seem that it supports `difftastic` out of box. Perhaps it is possible to configure it to support `difftastic` as a custom tool.

## Contributing

Contributions are welcome! Feel free to submit issues and pull requests on the [GitHub repository](https://github.com/pkryger/difftastic.el).

## License

This package is licensed under the [GPLv3 License](https://www.gnu.org/licenses/gpl-3.0.en.html).

---

Happy coding! If you encounter any issues or have suggestions for improvements, please don't hesitate to reach out on the [GitHub repository](https://github.com/pkryger/difftastic.el). Your feedback is highly appreciated.
