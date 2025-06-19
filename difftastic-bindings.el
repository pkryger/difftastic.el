;;; difftastic-bindings.el --- Manage difftastic key bindings -*- lexical-binding: t -*-

;;; Commentary:
;;
;; A minor global mode to ensure difftastic bindings.  See commentary in
;; difftastic.el for more information.

;;; Code:

(require 'transient)
(require 'seq)
(require 'compat)

(eval-and-compile ;; Until Emacs-28
  (unless (fboundp 'if-let*)
    (require 'subr-x)))

(defgroup difftastic-bindings nil
  "Key bindings for difftastic."
  :link '(emacs-commentary-link "difftastic")
  :group 'difftastic)

(defun difftastic-bindings--parse-bindings (bindings)
  "Parse BINDINGS into a suffix group specification."
  (let ((bindings
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
    (if (eql 1 (length bindings))
        (car bindings)
      (vconcat bindings))))

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
        (if (vectorp suffix)
            (seq-do (lambda (binding)
                      (transient-remove-suffix prefix (car binding)))
                    suffix)
          (transient-remove-suffix prefix (car suffix)))))
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
      (compat-call ;; Since Emacs-29
       keymap-set map (car spec) (cadr spec)))
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
      (compat-call ;; Since Emacs-29
       keymap-unset map (car spec) 'remove))
    (put keymap 'difftastic--installed nil)
    (difftastic-bindings--remove-from-installed :keymaps keymap)))

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
(make-obsolete-variable 'difftastic-bindings-prefixes
                        'difftastic-bindings-alist
                        "20250506")

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
(make-obsolete-variable 'difftastic-bindings-keymaps
                        'difftastic-bindings-alist
                        "20250506")

;;;###autoload
(defcustom difftastic-bindings-alist
  '((((prefixes .  ((magit-diff (-1 -1) magit-diff))))
     .
     (("M-d" difftastic-magit-diff "Difftastic diff (dwim)")
      ("M-c" difftastic-magit-show "Difftastic show")))
    (((prefixes . ((magit-blame "b" magit-blame)))
      (keymaps . ((magit-blame-read-only-mode-map . magit-blame))))
     .
     (("M-RET" difftastic-magit-show "Difftastic show")))
    (((prefixes . ((magit-file-dispatch (0 1 -1) magit-files))))
     .
     (("M-d" difftastic-magit-diff-buffer-file "Difftastic")))
    (((keymaps . ((dired-mode-map . dired))))
     .
     (("M-=" difftastic-dired-diff)))
    (((prefixes . ((forge-post-menu "C-c" forge-post))))
     .
     (("C-M-d" difftastic-forge-pullreq-show-diff "Difftastic diff")))
    (((keymaps . ((forge-post-mode-map . forge-post))))
     .
     (("C-c C-M-d" difftastic-forge-pullreq-show-diff))))
  "Define `difftastic' bindings.
This variable defines all bindings together with prefixes and keymaps
where they should be installed.  It is an alist where each entry in in a
form (SPECS . BINDINGS).  The SPECS is an alist where each entry is in a
form (TYPE . SPEC).  The TYPE is either `prefixes' or `keymaps' and:

- When TYPE is `prefixes' then SPEC is a list where each element is in a
  from of (PREFIX LOC FEATURE), where PREFIX is a `transient' prefix to
  which to install bindings, LOC is a location within the prefix and
  FEATURE is a feature (symbol) or file (string) that defines the
  prefix.  When length of SPEC is equal to 1 the binding is installed a
  transient suffix, otherwise it's installed as a transient column.  LOC
  can be in any form accepted by `transient-get-suffix', which see.

- When TYPE is `keymaps' then SPEC is a list where each element is in a
  form of (MAP . FEATURE), where MAP is a keymap to set bindings to and
  FEATURE is a feature (symbol) or file (string) that defines the MAP.

BINDINGS is a list where each enlement is in a form of (KEY COMMAND
DESCRIPTION), where KEY is a key that should be bound, COMMAND is a
command that should be executed when KEY has been pressed, and
DESCRIPTION is a description that should be used for suffixes that are
added to prefixes.  KEY needs to be a valid key according to
`key-valid-p' and in a form accepted by `transient-append-suffix'.

Note: this variable is applied only when `difftastic-bindings-mode' is
turned on.  This means that the mode may need to be turned off and on
again."
  :type '(alist :key-type
                (set
                 (cons :tag "Prefixes"
                       (const prefixes)
                       (repeat (list (symbol :tag "Prefix")
                                     (choice :tag "Location"
                                             (key :tag "Key")
                                             (string :tag "Command")
                                             (repeat (integer :tag "Coordinate")))
                                     (choice
                                      (string :tag "File")
                                      (symbol :tag "Feature")))))
                 (cons :tag "Keymaps"
                       (const keymaps)
                       (alist :key-type (symbol :tag "Keymap")
                              :value-type (choice
                                           (string :tag "File")
                                           (symbol :tag "Feature")))))
                :value-type
                (repeat
                 (list (key :tag "Key")
                       (function :tag "Command")
                       (choice :tag "Description"
                               (const :tag "Command Name" nil)
                               (string :tag "Literal Text")))))
  :link '(emacs-commentary-link "difftastic")
  :risky t
  :group 'difftastic-bindings)

(defun difftastic-bindings--prefix-specs (bindings-alist)
  "Return a list of prefixes specs from BINDINGS-ALIST.
BINDINGS-ALIST is in the same form as `difftastic-bindings-alilst',
which see."
  (apply #'append
         (mapcar (lambda (elt)
                   (alist-get 'prefixes (car elt)))
                 bindings-alist)))

(defun difftastic-bindings--keymaps (bindings-alist)
  "Return a list of keymaps from BINDINGS-ALIST.
BINDINGS-ALIST is in the same form as `difftastic-bindings-alilst',
which see."
  (apply #'append
         (mapcar (lambda (elt)
                   (alist-get 'keymaps (car elt)))
                 bindings-alist)))

(defun difftastic-bindings--prefix-specs-bindings (bindings-alist &optional prefix)
  "Return a list of PREFIX specs and bindings from BINDINGS-ALIST.
When PREFIX is nil return a list of all prefix specs and bindings.
BINDINGS-ALIST is in the same form as `difftastic-bindings-alilst',
which see."
  (apply #'append
         (mapcar (lambda (elt)
                   (when-let* ((prefixes (alist-get 'prefixes (car elt))))
                     (if prefix
                         (when-let* ((spec (assq prefix prefixes)))
                           (list (cons spec (cdr elt))))
                       (mapcar (lambda (spec)
                                 (cons spec (cdr elt)))
                               prefixes))))
                 bindings-alist)))

(defun difftastic-bindings--keymap-bindings (bindings-alist &optional keymap)
  "Return a list of KEYMAP specs and bindings from BINDINGS-ALIST.
When KEYMAP is nil return a list of all keymaps and bindings.
BINDINGS-ALIST is in the same form as `difftastic-bindings-alilst',
which see."
  (apply #'append
         (mapcar (lambda (elt)
                   (when-let* ((keymaps (alist-get 'keymaps (car elt))))
                     (if keymap
                         (when-let* ((spec (assq keymap keymaps)))
                           (list (cons spec (cdr elt))))
                       (mapcar (lambda (spec)
                                 (cons spec (cdr elt)))
                               keymaps))))
                 bindings-alist)))

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
      (dolist (prefix-spec-bindings (difftastic-bindings--prefix-specs-bindings
                                     difftastic-bindings-alist prefix))
        (when-let* ((loc (cadr (car prefix-spec-bindings)))
                    (suffix (difftastic-bindings--parse-bindings
                             (cdr prefix-spec-bindings))))
          (difftastic-bindings--append-suffix prefix
                                              loc
                                              suffix))))
    (dolist (keymap (plist-get prefixes-keymaps :keymaps))
      (when-let* (((boundp keymap))
                  ((not (get keymap 'difftastic--installed)))
                  (bindings (cdar (difftastic-bindings--keymap-bindings
                                   difftastic-bindings-alist
                                   keymap))))
        (difftastic-bindings--bind-keys keymap bindings)))))

(defun difftastic-bindings-mode--turn-on ()
  "Install difftastic bindings and register an `after-load-functions' hook."
  (dolist (prefix-spec-bindings (difftastic-bindings--prefix-specs-bindings
                                 difftastic-bindings-alist))
    (pcase-let* ((`((,prefix ,loc ,feature-file) . ,bindings)
                  prefix-spec-bindings)
                 (feature (if (stringp feature-file)
                              (intern feature-file)
                            feature-file)))
      (when-let* (((featurep feature))
                  (suffix (difftastic-bindings--parse-bindings bindings)))
        (difftastic-bindings--append-suffix prefix
                                            loc
                                            suffix))))

  (dolist (keymap-spec-bindings (difftastic-bindings--keymap-bindings
                                 difftastic-bindings-alist))
    (pcase-let* ((`((,keymap . ,feature-file) . ,bindings) keymap-spec-bindings)
                 (feature (if (stringp feature-file)
                              (intern feature-file)
                            feature-file)))
      (when (and (featurep feature)
                 (boundp keymap))
        (difftastic-bindings--bind-keys keymap bindings))))

  (dolist (prefix-spec (difftastic-bindings--prefix-specs
                        difftastic-bindings-alist))
    (pcase-let* ((`(,prefix ,_ ,file-feature) prefix-spec))
      (difftastic-bindings--add-to-after-load
       file-feature
       :prefixes
       prefix)))
  (dolist (keymap-spec (difftastic-bindings--keymaps
                        difftastic-bindings-alist))
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
Use bindings specified in `difftastic-bindings-alist' (which see) to
create defined suffixes in prefixes and bindings in keymaps."
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
