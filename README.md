# difftastic.el - Wrapper for difftastic

[![MELPA](https://melpa.org/packages/difftastic-badge.svg)](https://melpa.org/#/difftastic)

## Description

The `difftastic` Emacs package is designed to integrate [`difftastic`](https://github.com/wilfred/difftastic) - a structural diff tool - into your Emacs workflow, enhancing your code review and comparison experience. This package automatically displays `difftastic`'s output within Emacs using faces from your user theme, ensuring consistency with your overall coding environment.

## Features

- Configure faces to your likening. By default `magit-diff-*` faces from your user them are used for consistent visual experience.
- Chunks and file navigation using `n`/`N` and `p`/`P` in generated diffs.
- DWIM workflows from `magit`.

## Installation

1. Clone this repository to a directory of your choice.
2. Add the following lines to your Emacs configuration file (usually `~/.emacs` or `~/.emacs.d/init.el`):

```emacs-lisp
(add-to-list 'load-path "/path/to/difftastic.el")
(require 'difftastic)
```

## Configuration

To configure the `difftastic` commands in `magit-diff` prefix, use the following code snippet in your Emacs configuration:

```emacs-lisp
(require 'difftastic)

;; Add commands to a `magit-difftastic'
(eval-after-load 'magit-diff
  '(transient-append-suffix 'magit-diff '(-1 -1)
     [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
      ("S" "Difftastic show" difftastic-magit-show)]))
(add-hook 'magit-blame-read-only-mode-hook
          (lambda ()
            (kemap-set magit-blame-read-only-mode-map
                       "D" #'difftastic-magit-show)
            (kemap-set magit-blame-read-only-mode-map
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
The following commands are meant to help to interact with `difftastic`:

- `difftastic-magit-diff` - show the result of `git diff ARGS -- FILES` with `difftastic`. This is the main entry point for DWIM action, so it tries to guess revision or range.
- `difftastic-magit-show` - show the result of `git show ARG` with `difftastic`. It tries to guess `ARG`, and ask for it when can't. When called with prefix argument it will ask for `ARG`.
- `difftastic-files` - show the result of `difft FILE-A FILE-B`. When called with prefix argument it will ask for language to use, instead of relaying on `difftastic`'s detection mechanism.
- `difftastic-buffers` - show the result of `difft BUFFER-A BUFFER-B`. Language is guessed based on buffers modes. When called with prefix argument it will ask for language to use.
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

## Acknowledgments

This package was inspired by the need for an integration of `difftastic` within Emacs, enhancing the code review process for developers.

This work is based on [a Tassilo Horn blog entry](https://tsdh.org/posts/2022-08-01-difftastic-diffing-with-magit.html). `magit-diff` keybindings and a concept of updating faces comes from a [Shiv J.M. blog](https://shivjm.blog/better-magit-diffs/).

## Similar packages

### Diff ANSI

There's a [`diff-ansi`](https://codeberg.org/ideasman42/emacs-diff-ansi) package available. I haven't spent much time on it, but at a first glance it doesn't seem that it supports `difftastic` out of box. Perhaps it is possible to configure it to support `difftastic` as a custom tool.

## Contributing

Contributions are welcome! Feel free to submit issues and pull requests on the [GitHub repository](https://github.com/pkryger/difftastic.el).

## License

This package is licensed under the [GPLv3 License](https://www.gnu.org/licenses/gpl-3.0.en.html).

---

Happy coding! If you encounter any issues or have suggestions for improvements, please don't hesitate to reach out on the [GitHub repository](https://github.com/pkryger/difftastic.el). Your feedback is highly appreciated.
