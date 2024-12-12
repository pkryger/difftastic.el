;;; difftastic-bindings.el --- Manage difftastic key bindings -*- lexical-binding: t -*-

;;; Commentary:
;; TODO: refer to difftastic.el

;;; Code:

(require 'transient)
(require 'rx)

(defgroup difftastic-bindings nil
  "Key bindings for difftastic."
  :link '(emacs-commentary-link "difftastic")
  :group 'difftastic)

(defvar difftastic-bindings--installed nil
  "Last installed bindings.

Each entry is in a from of (PREFIX-LOC-MAP . SUFFIX-BINDINGS)
where SUFFIX-BINDINGS is either a suffis or bindings and
PREFIX-LOC-MAP is either a KEYMAP (symbol) or PREFIC-LOC.  The
latter is in a form of PREFIX (symbol) and LOC (list of
integers).")

(defvar difftastic-bindings--after-load-alist nil
  "List of symbols to watch for in `after-load-function' hook.

Each entry is in a form of (SYM . LIST) where SYM is a
symbol (file name sans extension) to watch for load and LIST is a
list of either a KEYMAP (symbol) or a PREFIX-LOC.  The latter is
in a form of PREFIX (symbol) and LOC (list of integers).

This is a workaround to the fact that `with-eval-after-load' (and
`eval-after-load') are discouraged in packages (see
https://github.com/purcell/package-lint/issues/125), while
`transient''s prefixes clobber existing definition when files
that contain them are reloaded, which may happen when a package
is upgraded or reinstalled.  Such a behavior stems from the fact
that `transient-define-prefix' uses `defalias' to create a
prefix, see
https://github.com/magit/transient/blob/e1126a6/lisp/transient.el#L991-L996.")

(defun difftastic-bindings--parse-bindings (bindings)
  "Parse BINDINGS into a suffix group specification."
  (vconcat
   (mapcar (lambda (spec)
             (list (car spec)
                   (or (when-let* ((desc (caddr spec))
                                   ((stringp desc))
                                   ((< 0 (length desc))))
                         desc)
                       (if-let* ((fun (cadr spec))
                                 ((symbolp fun)))
                           ;; it is unquoted, i.e., '("X" car)
                           (symbol-name fun)
                         ;; it is quoted, i.e., '("X" 'car) or '("X" #'car)
                         (symbol-name (cadr fun))))
                   (cadr spec)))
           bindings)))

(defun difftastic-bindings--append-suffix (prefix-loc suffix)
  "Append SUFFIX to PREFIX-LOC.
PREFIX-LOC is a cons in a form of (PREFIX . LOC), where PREFIX is
a prefix to which to append SUFFIX and LOC specifies coordinates
in PREFIX after which the SUFFIX should be appended.  SUFFIX
won't be appended if it is an already a suffix in the PREFIX at LOC.
After SUFFIX has been appended, store PREFIX-LOC in
`difftastic-bindings--installed'."
  (pcase-let ((`(,prefix ,loc) prefix-loc))
    (when (and suffix
               (fboundp prefix)
               (not (autoloadp prefix))
               (not (equal (transient-parse-suffix prefix suffix)
                           (transient-get-suffix prefix loc))))
      (transient-append-suffix prefix loc suffix)
      (setf (alist-get prefix-loc difftastic-bindings--installed
                       nil nil #'equal)
            suffix))))

(defun difftastic-bindings--remove-suffix (prefix-loc suffix)
  "Remove SUFFIX from PREFIX-LOC.
PREFIX-LOC is a cons in a form of (PREFIX . LOC), where PREFIX is
a prefix from which to remove SUFFIX and LOC specifies
coordinates in PREFIX from which the SUFFIX should be removed.
SUFFIX won't be removed unless it is a suffix in the PREFIX at
LOC.  After SUFFIX has been removed from PREFIX, remove
PREFIX-LOC from `difftastic-bindings--installed' as well."
  (pcase-let ((`(,prefix ,loc) prefix-loc))
    (when (and suffix
               (fboundp prefix)
               (not (autoloadp prefix))
               (equal (transient-parse-suffix prefix suffix)
                      (transient-get-suffix prefix loc)))
      (transient-remove-suffix prefix loc)
      (setf (alist-get prefix-loc difftastic-bindings--installed
                       nil 'remove #'equal)
            nil))))


(defun difftastic-bindings--bind-keys (keymap bindings)
  "Extract key and function from BINDINGS and set them in KEYMAP.
KEYMAP passed as symbol that evals into a keymap to set keys
into.  After BINDINGS have been set they are stored in
`difftastic-bindings--installed'."
  (when (and bindings
             (boundp keymap))
    (dolist (spec bindings)
      (keymap-set (eval keymap) (car spec) (cadr spec)))
    (setf (alist-get keymap difftastic-bindings--installed)
          bindings)))

(defun difftastic-bindings--unbind-keys (keymap bindings)
  "Remove keys in BINDINGS from KEYMAP.
KEYMAP passed as symbol that evals into a keymap to set keys
into.  After BINDINGS have been removed from KEYMAP they are
removed from `difftastic-bindings--installed' as well."
  (when (and bindings
             (boundp keymap))
    (dolist (spec bindings)
      (keymap-unset (eval keymap) (car spec) 'remove))
    (setf (alist-get keymap difftastic-bindings--installed
                     nil 'remove)
          nil)))

;;;###autoload
(defun difftastic-bindings-prefixes-set (symbol value)
  "TODO: SYMBOL VALUE."
  ;; TODO add value check
  (when (bound-and-true-p difftastic-bindings-mode)
    (dolist (installed difftastic-bindings--installed)
      (let* ((prefix-loc (car installed))
             (prefix (car prefix-loc)))
        (when (and
               (not (assq prefix value))
               (assq prefix (bound-and-true-p
                             difftastic-bindings-prefixes)))
          (difftastic-bindings--remove-suffix prefix-loc
                                              (cdr installed)))))

    (dolist (splms difftastic-bindings--after-load-alist)
      (setcdr splms
              (delq nil
                    (mapcar (lambda (plm)
                              (when (or (symbolp plm)
                                        (when-let* ((val-pl
                                                     (assq (car plm) value)))
                                          (equal (cdr val-pl) (cdr plm))))
                                plm))
                            (cdr splms)))))

    (dolist (v value)
      (pcase-let* ((`(,prefix ,loc ,sym) v)
                   (prefix-loc (cons prefix loc))
                   (installed (alist-get
                               prefix-loc difftastic-bindings--installed
                               nil nil #'equal))
                   (suffix (difftastic-bindings--parse-bindings
                            (alist-get prefix (bound-and-true-p
                                               difftastic-bindings-alist)))))
        (unless (equal installed suffix)
          (difftastic-bindings--remove-suffix prefix-loc
                                              installed)
            (difftastic-bindings--append-suffix prefix
                                                suffix))

        (if-let* ((entry (assq sym difftastic-bindings--after-load-alist)))
            (unless (member prefix-loc (cdr entry))
              (setcdr entry (cons prefix-loc (cdr entry))))
          (push (cons sym (list prefix-loc))
                difftastic-bindings--after-load-alist)))))
  (set-default-toplevel-value symbol value))

(defcustom difftastic-bindings-prefixes
  '((magit-diff (-1 -1) magit-diff)
    (magit-blame (-2) magit-blame))
  "Prefixes and locations within them where to install `difftastic' bindings."
  ;; TODO: typecheck for value
  :type '(repeat (list (symbol :tag "Prefix")
                       (repeat :tag "Location" (integer))
                       (symbol :tag "Feature/File")))
  :set #'difftastic-bindings-prefixes-set
  :group 'difftastic-bindings)


;;;###autoload
(defun difftastic-bindings-keymaps-set (symbol value)
  "TODO: SYMBOL VALUE."
  ;; TODO: add typecheck for value
  (when (bound-and-true-p difftastic-bindings-mode)
    (dolist (installed difftastic-bindings--installed)
      (let ((map (car installed)))
        (when (and
               (not (assq map value))
               (memq map (bound-and-true-p
                          difftastic-bindings-keymaps)))
          (difftastic-bindings--unbind-keys map
                                            (cdr installed)))))

    (dolist (splms difftastic-bindings--after-load-alist)
      (setcdr splms
              (delq nil
                    (mapcar (lambda (plm)
                              (when (or (not (symbolp plm))
                                        (assq plm value))
                                plm))
                            (cdr splms)))))

    (dolist (v value)
      (pcase-let* ((`(,map . ,sym) v)
                   (installed (alist-get map difftastic-bindings--installed)))
        (unless (equal installed value)
          (difftastic-bindings--unbind-keys map
                                            installed)
          (difftastic-bindings--bind-keys map (bound-and-true-p
                                               difftastic-bindings-alist)))

        ;; TODO: this will cause the list to grow over time, as files that are
        ;; no longer referenced won't be purged.
        (if-let* ((entry (assq sym difftastic-bindings--after-load-alist)))
            (unless (memq map (cdr entry))
              (setcdr entry (cons map (cdr entry))))
          (push (cons sym (list map))
                difftastic-bindings--after-load-alist))))

  (set-default-toplevel-value symbol value)))

;;;###autoload
(defcustom difftastic-bindings-keymaps
  '((magit-blame-read-only-mode-map . magit-blame))
  "TODO: List of keymaps to add `difftastic' bindings to."
  :type '(alist :key-type (symbol :tag "Keymap")
                :value-type (symbol :tag "Feature/File"))
  :group 'difftastic-bindings)

;;;###autoload
(defun difftastic-bindings-alist-set (symbol value)
  "TODO: SYMBOL VALUE."
  ;; TODO: add typecheck for value
  (when (bound-and-true-p difftastic-bindings-mode)
    (dolist (pls difftastic-bindings-prefixes)
      (pcase-let* ((`(,prefix ,loc) pls)
                   (prefix-loc (cons prefix loc))
                   (installed (alist-get
                               prefix-loc difftastic-bindings--installed
                               nil nil #'equal))
            (suffix (difftastic-bindings--parse-bindings
                     value)))
        (unless (equal installed suffix)
          (difftastic-bindings--remove-suffix prefix-loc
                                              installed)
          (difftastic-bindings--append-suffix prefix-loc
                                              suffix))))

    (dolist (ms difftastic-bindings-keymaps)
      (pcase-let* ((`(,map) ms)
                   (installed (alist-get map difftastic-bindings--installed)))
        (unless (equal installed value)
          (difftastic-bindings--unbind-keys map
                                            installed)
          (difftastic-bindings--bind-keys map
                                          value)))))
  (set-default-toplevel-value symbol value))

;;;###autoload
(defcustom difftastic-bindings-alist
  '(("D" difftastic-magit-diff "Difftastic diff (dwim)")
    ("S" difftastic-magit-show "Difftastic show"))
  "Define magit bindings.
TODO: describe meaning and how to set i.e., via `setopt'
TODO: add setter"
  :type '(repeat
          (list (key :tag "Key")
                (function :tag "Command")
                (choice :tag "Description"
                        (const :tag "Command Name" nil)
                        (string :tag "Literal Text"))))
  :set #'difftastic-bindings-alist-set
  :group 'difftastic-bindings)

(defun difftastic-bindings--after-load (load-file)
  "TODO: LOAD-FILE."
  (when (bound-and-true-p difftastic-bindings-mode)
    (when-let* ((difftastic-bindings-alist)
                (sym
                 (intern (file-name-nondirectory (file-name-sans-extension
                                                  load-file))))
                ((featurep sym))
                (prefix-loc-map
                 (cdr (assq sym difftastic-bindings--after-load-alist)))
                (suffix (difftastic-bindings--parse-bindings
                         difftastic-bindings-alist)))
      (dolist (plm prefix-loc-map)
        (if (symbolp plm)
            (unless (assq plm difftastic-bindings--installed)
              (difftastic-bindings--bind-keys plm
                                              difftastic-bindings-alist))
          (difftastic-bindings--append-suffix plm
                                              suffix))))))


;;;###autoload
(define-minor-mode difftastic-bindings-mode
  "Ensure key bindings to `difftastic' commands in `magit'.
Use bindings specified in `difftastic-bindings' (which see) to
create a suffix in `magit-diff' prefix and install them into
`magit-blame-read-only-mode-map'."
  :global t
  :group 'difftastic-bindings
  (if difftastic-bindings-mode
      (progn
        (add-hook 'after-load-functions #'difftastic-bindings--append-suffix)
        ;; Ensure bindings are processed every time `magit-diff' and
        ;; `magit-blame' are reloaded.
        (dolist (pls difftastic-bindings-prefixes)
          (pcase-let* ((`(,prefix ,loc ,sym) pls)
                       (prefix-loc (cons prefix loc)))
            (when (featurep sym)
              (difftastic-bindings--append-suffix
               prefix-loc
               (difftastic-bindings--parse-bindings
                difftastic-bindings-alist)))))

        (dolist (ms difftastic-bindings-keymaps)
          (pcase-let* ((`(,map ,sym) ms))
            (when (featurep sym)
              (difftastic-bindings--bind-keys
               map
               difftastic-bindings-alist)))))

    (remove-hook 'after-load-functions #'difftastic-bindings--append-suffix)
    (dolist (pls difftastic-bindings-prefixes)
      (pcase-let* ((`(,prefix ,loc) pls)
                   (prefix-loc (cons prefix loc)))
        (difftastic-bindings--remove-suffix
         prefix-loc
         (alist-get prefix-loc difftastic-bindings--installed
                    nil nil #'equal))))

    (dolist (ms difftastic-bindings-keymaps)
      (pcase-let* ((`(,map) ms))
        (difftastic-bindings--unbind-keys
         map
         (alist-get map difftastic-bindings--installed))))))


(provide 'difftastic-bindings)

;;; difftastic-bindings.el ends here

;; Local Variables:
;; package-lint-main-file: "difftastic.el"
;; End:
