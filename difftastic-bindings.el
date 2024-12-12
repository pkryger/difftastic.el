;;; difftastic-bindings.el --- Manage difftastic key bindings -*- lexical-binding: t -*-

;;; Commentary:
;;
;; A minor global mode to ensure difftastic bindings.  See commentary in
;; difftastic.el for more information.

;;; Code:

(require 'transient)
(require 'seq)

(defgroup difftastic-bindings nil
  "Key bindings for difftastic."
  :link '(emacs-commentary-link "difftastic")
  :group 'difftastic)

(defun difftastic-bindings--parse-bindings (bindings)
  "Parse BINDINGS into a suffix group specification."
  (vconcat
   (mapcar (lambda (spec)
             (let ((fun (if-let* ((f (cadr spec))
                                  ((symbolp f)))
                            ;; it is unquoted, i.e., '("X" fun-x)
                            f
                          ;; it is quoted, i.e., '("X" 'fun-x) or '("X" #'fun-x)
                          (cadr f))))
               (list (car spec)
                     (or (when-let* ((desc (caddr spec))
                                     ((stringp desc))
                                     ((< 0 (length desc))))
                           desc)
                         (symbol-name fun))
                     fun)))
           bindings)))

(defvar difftastic-bindings--installed-plist nil
  "Prefixes and keymaps that have difftastic bindings installed.")
(put 'difftastic-bindings--installed-plist
     'risky-local-variable t)

(defun difftastic-bindings--add-to-installed (type symbol)
  "Add SYMBOL of TYPE to `difftastic-bindings--installed-plist'.
TYPE should be one of `:prefixes' or `:keymaps'."
  (let ((installed (plist-get difftastic-bindings--installed-plist
                              type)))
    (unless (memq symbol installed)
      (setq difftastic-bindings--installed-plist
            (plist-put difftastic-bindings--installed-plist
                 type
                 (cons symbol installed))))))

(defun difftastic-bindings--remove-from-installed (type symbol)
  "Remove SYMBOL of TYPE from `difftastic-bindings--installed-plist'.
TYPE should be one of `:prefixes' or `:keymaps'."
  (let ((installed (plist-get difftastic-bindings--installed-plist
                              type)))
    (when (memq symbol installed)
      (setq difftastic-bindings--installed-plist
          (plist-put difftastic-bindings--installed-plist
               type
               (delq symbol installed))))))

(defun difftastic-bindings--append-suffix (prefix loc suffix)
  "Append bindings in SUFFIX to PREFIX at LOC.
PREFIX is a prefix to which to append SUFFIX and LOC specifies
coordinates in PREFIX after which the SUFFIX should be appended,
SUFFIX won't be appended if it is an already a suffix in the
PREFIX at LOC.  After SUFFIX has been appended register it in PREFIX
`difftastic--installed' property and in
`difftastic-bindings--installed-plist'."
  (when (and suffix
             (functionp prefix)
             (not (autoloadp prefix))
             (not (equal (transient-parse-suffix prefix suffix)
                         (ignore-errors (transient-get-suffix prefix loc)))))
    (transient-append-suffix prefix loc suffix)
    (function-put prefix 'difftastic--installed (cons loc suffix))
    (difftastic-bindings--add-to-installed :prefixes prefix)))

(defun difftastic-bindings--remove-suffix (prefix)
  "Remove installed bindings from PREFIX.
Use `difftastic--installed' property of PREFIX to determine which
suffix should be removed.  After suffix has been removed clear
the `difftastic--installed' property and remove it from
`difftastic-bindings--installed-plist'."
  (when-let* (((functionp prefix))
              ((not (autoloadp prefix)))
              (installed (function-get prefix 'difftastic--installed)))
    (pcase-let* ((`(,loc . ,suffix) installed))
      (if (equal (transient-parse-suffix prefix suffix)
                 (ignore-errors (transient-get-suffix prefix loc)))
          ;; Try remove the whole suffix if it is still a suffix at loc
          (transient-remove-suffix prefix loc)
        ;; Fall back to key by key removal
        (seq-do (lambda (binding)
                  (transient-remove-suffix prefix (car binding)))
                suffix)))
    (function-put prefix 'difftastic--installed nil)
    (difftastic-bindings--remove-from-installed :prefixes prefix)))

(defun difftastic-bindings--bind-keys (keymap bindings)
  "Extract key and function from BINDINGS and set them in KEYMAP.
KEYMAP passed as symbol that evals into a keymap to set keys
into.  After BINDINGS have been appended register it in KEYMAP
`difftastic--installed' property and in
`difftastic-bindings--installed-plist'."
  (when-let* ((bindings)
              ((boundp keymap))
              (map (ignore-errors (eval keymap)))
              ((keymapp map)))
    (dolist (spec bindings)
      (keymap-set map (car spec) (cadr spec)))
    (put keymap 'difftastic--installed bindings)
    (difftastic-bindings--add-to-installed :keymaps keymap)))

(defun difftastic-bindings--unbind-keys (keymap)
  "Remove keys in BINDINGS from KEYMAP.
KEYMAP passed as symbol that evals into a keymap to set keys
into.  Use `difftastic--installed' property of KEYMAP to
determine which bindings should be removed.  After bindings have
been removed clear the `difftastic--installed' property and
remove them from `difftastic-bindings--installed-plist'."
  (when-let* (((boundp keymap))
              (map (ignore-errors (eval keymap)))
              ((keymapp map))
              (bindings (get keymap 'difftastic--installed)))
    (dolist (spec bindings)
      (keymap-unset map (car spec) 'remove))
    (put keymap 'difftastic--installed nil)
    (difftastic-bindings--remove-from-installed :keymaps keymap)))

;;;###autoload
(defcustom difftastic-bindings-prefixes
  '((magit-diff (-1 -1) magit-diff)
    (magit-blame (-1) magit-blame))
  "List of prefixes to install `difftastic' bindings.
Each entry in the list is in a from of (PREFIX LOC FEATURE),
where PREFIX is a `transient' prefix to which to install
bindings, LOC is a location within the prefix and FEATURE is a
feature (symbol) or file (string) that defines the prefix.  LOC
can be in any form accepted by `transient-get-suffix', which see."
  :type '(repeat (list (symbol :tag "Prefix")
                       (repeat :tag "Location" (integer))
                       (choice
                        (string :tag "File")
                        (symbol :tag "Feature"))))
  :link '(emacs-commentary-link "difftastic")
  :risky t
  :group 'difftastic-bindings)

;;;###autoload
(defcustom difftastic-bindings-keymaps
  '((magit-blame-read-only-mode-map . magit-blame))
  "List of keymaps to add `difftastic' bindings to.
Each entry in the list is in a form of (MAP . FEATURE), where MAP
is a keymap to set bindings to and FEATURE is a feature (symbol)
or file (string) that defines the MAP."
  :type '(alist :key-type (symbol :tag "Keymap")
                :value-type (choice
                             (string :tag "File")
                             (symbol :tag "Feature")))
  :link '(emacs-commentary-link "difftastic")
  :risky t
  :group 'difftastic-bindings)

;;;###autoload
(defcustom difftastic-bindings-alist
  '(("D" difftastic-magit-diff "Difftastic diff (dwim)")
    ("S" difftastic-magit-show "Difftastic show"))
  "Define `difftastic' bindings.
Each entry is in a form of (KEY COMMAND DESCRIPTION), where KEY
is a key that should be bound, COMMAND is a command that should
be executed when KEY has been pressed, and DESCRIPTION is a
description that should be used for suffixes that are added to
prefixes as defined in `difftastic-bindings-prefixes'.  KEY needs
to be a valid key according to `key-valid-p' and in a form
accepted by `transient-append-suffix'."
  :type '(repeat
          (list (key :tag "Key")
                (function :tag "Command")
                (choice :tag "Description"
                        (const :tag "Command Name" nil)
                        (string :tag "Literal Text"))))
  :link '(emacs-commentary-link "difftastic")
  :risky t
  :group 'difftastic-bindings)

(defvar difftastic-bindings--after-load-alist nil
  "Features and prefixes and keymaps that should have bindings managed.
Each entry is in a form of (FEATURE . PLIST) where feature is a
feature that defines prefixes and keymaps in PLIST.  PLIST is a
plist wit `:prefixes' and `:keymaps' that should have bindings
managed.")
(put 'difftastic-bindings--after-load-alist
     'risky-local-variable t)

(defun difftastic-bindings--add-to-after-load (file-feature type symbol)
  "Add SYMBOL to list of keyword TYPE for FILE-FEATURE.
The FILE-FEATURE is a file (string) or a feature (symbol).  The
type should be one of `:prefixes' or `:keymaps'."
  (let* ((feature (if (stringp file-feature)
                      (intern file-feature)
                    file-feature))
         (entry (assq feature
                      difftastic-bindings--after-load-alist))
         (symbols (plist-get (cdr entry) type)))
    (if entry
        (unless (memq symbol symbols)
          (setcdr entry
                  (plist-put (cdr entry)
                     type
                     (cons symbol symbols))))
      (push (cons feature (list type (list symbol)))
            difftastic-bindings--after-load-alist))))


(defun difftastic-bindings--after-load (load-file)
  "Ensure difftastic bindings are set up after LOAD-FILE has been loaded.
This function is designed as an `after-load-functions' hook."
  (when-let* (((bound-and-true-p difftastic-bindings-mode))
              (sym
               (intern (file-name-nondirectory (file-name-sans-extension
                                                load-file))))
              ((featurep sym))
              (prefixes-keymaps
               (cdr (assq sym difftastic-bindings--after-load-alist))))
    (dolist (prefix (plist-get prefixes-keymaps :prefixes))
      (when-let* ((loc (car (alist-get prefix
                                       difftastic-bindings-prefixes)))
                  (suffix (difftastic-bindings--parse-bindings
                           difftastic-bindings-alist)))
        (difftastic-bindings--append-suffix prefix
                                            loc
                                            suffix)))
    (dolist (keymap (plist-get prefixes-keymaps :keymaps))
      (when (boundp keymap)
        (unless (get keymap 'difftastic--installed)
          (difftastic-bindings--bind-keys keymap
                                          difftastic-bindings-alist))))))

(defun difftastic-bindings-mode--turn-on ()
  "Install difftastic bindings and register an `after-load-functions' hook."
  (let ((suffix (difftastic-bindings--parse-bindings
                 difftastic-bindings-alist)))
    (dolist (prefix-spec difftastic-bindings-prefixes)
      (pcase-let* ((`(,prefix ,loc ,feature-file) prefix-spec)
                   (feature (if (stringp feature-file)
                                (intern feature-file)
                              feature-file)))
        (when (featurep feature)
          (difftastic-bindings--append-suffix prefix
                                              loc
                                              suffix)))))

  (dolist (keymap-spec difftastic-bindings-keymaps)
    (pcase-let* ((`(,keymap . ,feature-file) keymap-spec)
                 (feature (if (stringp feature-file)
                              (intern feature-file)
                            feature-file)))
      (when (and (featurep feature)
                 (boundp keymap))
        (difftastic-bindings--bind-keys keymap
                                        difftastic-bindings-alist))))

  (dolist (prefix-spec difftastic-bindings-prefixes)
    (pcase-let* ((`(,prefix ,_ ,file-feature) prefix-spec))
      (difftastic-bindings--add-to-after-load
       file-feature
       :prefixes
       prefix)))
  (dolist (keymap-spec difftastic-bindings-keymaps)
    (pcase-let* ((`(,keymap . ,file-feature) keymap-spec))
      (difftastic-bindings--add-to-after-load
       file-feature
       :keymaps
       keymap)))
  (add-hook 'after-load-functions #'difftastic-bindings--after-load))

(defun difftastic-bindings-mode--turn-off ()
  "Remove `after-load-functions' hook and difftastic bindings."
  (remove-hook 'after-load-functions #'difftastic-bindings--after-load)
  (setq difftastic-bindings--after-load-alist nil)

  (dolist (keymap (plist-get difftastic-bindings--installed-plist
                             :keymaps))
    (difftastic-bindings--unbind-keys keymap))
  (dolist (prefix (plist-get difftastic-bindings--installed-plist
                             :prefixes))
    (difftastic-bindings--remove-suffix prefix)))

;;;###autoload
(define-minor-mode difftastic-bindings-mode
  "Ensure key bindings to `difftastic' commands.
Use bindings specified in `difftastic-bindings' (which see) to
create a suffixes in prefixes defined in
`difftastic-bindings-prefixes' (which see) and install them into
`difftastic-bindings-keymaps' (which see)."
  :global t
  :group 'difftastic-bindings
  (if difftastic-bindings-mode
      (difftastic-bindings-mode--turn-on)
    (difftastic-bindings-mode--turn-off)))

(provide 'difftastic-bindings)

;;; difftastic-bindings.el ends here

;; Local Variables:
;; package-lint-main-file: "difftastic.el"
;; End:
