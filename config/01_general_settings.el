;;;; Align
;;;;
(defun align-repeat (start end regexp)
  "Repeat alignment w.r.t REGEXP."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end
                (concat "\\(\\s-*\\)" regexp) 1 1 t))


;;;; Auto Save / Backups
;;;;
(setq auto-save-default t
      auto-save-interval 500)


;;;; Disable the bell
;;;;
(setq ring-bell-function (lambda () nil))

;;;; Word motion
;;;;
(global-subword-mode 1)

;; Make motion the same as in my shell
(defun forward-to-whitespace (&optional arg)
  "Move until the next whitespace character."
  (interactive "^p")
  (let ((motion-function (if (< arg 0) #'skip-chars-backward
                           #'skip-chars-forward))
        (arg (abs arg)))
    (while (/= arg 0)
      (funcall motion-function " \r\n\t\f")
      (funcall motion-function "^ \r\n\t\f")
      (setq arg (1- arg)))))

(defun backward-to-whitespace (&optional arg)
  (interactive "^p")
  (forward-to-whitespace (- (or arg 1))))

(global-set-key (kbd "M-F") 'forward-to-whitespace)
(global-set-key (kbd "M-B") 'backward-to-whitespace)

;; Don't automatically activate the mark when holding down shift.
(setq shift-select-mode nil)


;;;; Compile Settings
;;;;
(global-set-key (kbd "<f9>") 'compile)
(global-set-key (kbd "<C-f9>") 'recompile)
(global-set-key (kbd "M-g M-t") 'first-error)
(global-set-key (kbd "M-g t") 'first-error)
(define-key minibuffer-local-map (kbd "M-i")
  (lambda ()
    (interactive)
    (insert (expand-file-name (buffer-name (window-buffer
                                            (minibuffer-selected-window)))))))


;;;; Enable commands
;;;;
(setq disabled-command-function nil)


;;;; Environment Variables
;;;;
(setenv "EDITOR" "emacs")
(setq shell-file-name "bash")


;;;; File Handling
;;;;
(defun remove-buffer-and-file (&optional buffer-or-name)
  "Removes `get-buffer' BUFFER-OR-NAME kills it too."
  (interactive)
  (let* ((buffer (get-buffer (or buffer-or-name (current-buffer))))
         (filename (buffer-file-name buffer)))
    (unless (and filename (file-exists-p filename))
      (error "Buffer '%s' is not visiting a file!" (buffer-name buffer)))
    (delete-file filename)
    (kill-buffer buffer)
    (message "File '%s' removed" filename)))

(defun rename-buffer-and-file (&optional buffer-or-name)
  "Rename BUFFER-OR-NAME and file it's visiting."
  (interactive)
  (let* ((buffer (get-buffer (or buffer-or-name (current-buffer))))
         (filename (buffer-file-name buffer))
         (name (buffer-name buffer)))
    (unless (and filename (file-exists-p filename))
      (error "Buffer '%s' is not visiting a file!" name))
    (let ((new-name (read-file-name "New name: " filename)))
      (rename-file filename new-name 1)
      (rename-buffer new-name t)
      (set-visited-file-name new-name)
      (set-buffer-modified-p nil)
      (message "File '%s' successfully renamed to '%s'" name
               (file-name-nondirectory new-name)))))

(global-set-key (kbd "C-x f") 'find-file)
(global-set-key (kbd "C-x C-f") 'dired)


;;;; Help settings
;;;;
(setq help-window-select nil)
(define-key help-map "I" 'info-apropos)
(define-key help-map "A" 'apropos)


;;;; Indentation Motion
;;;;
(defun beginning-of-line-or-indentation ()
  "Toggle between beginning of the line and indentation."
  (interactive)
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (beginning-of-line))))

(global-set-key (kbd "C-a") 'beginning-of-line-or-indentation)


;;;; Lines
;;;;
;;; New lines
(defun open-line-below ()
  "Add a new line below current one."
  (interactive)
  (end-of-line)
  (indent-new-comment-line))

(defun open-line-above ()
  "Add new line above the current one."
  (interactive)
  (beginning-of-line)
  (indent-new-comment-line)
  (forward-line -1)
  (indent-for-tab-command)
  (end-of-line))

(global-set-key (kbd "C-o") 'open-line-below)
(global-set-key (kbd "C-S-o") 'open-line-above)

;;; Moving lines
;; Taken from the old version of http://www.emacswiki.org/emacs/move-text.el
;; as the change in TRANSPOSE-LINES in 24.3 is negated by my advice on
;; TRANSPOSE-SUBR
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(global-set-key (kbd "C-s-<up>") 'move-text-up)
(global-set-key (kbd "C-s-<down>") 'move-text-down)

(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "RET") 'indent-new-comment-line)


;;;; Make scripts executeable automatically
;;;;
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)


;;;; Minibuffer -- escape quits it
(define-key minibuffer-local-map (kbd "<escape>") 'abort-recursive-edit)


;;;; Generalise finding definition
;;;;
(defvar-local find-definition-function 'find-tag
  "Function which `find-this-definition' calls to move to the
  definition of the `symbol-at-point'.

The default is `find-tag', but should be changed for each mode.

Think `completion-at-point' functions, but only one function at a time")

(defun find-this-definition-helper (&optional symbol function)
  (if symbol
      (funcall function symbol)
    (call-interactively function)))

(defun find-this-definition (&optional symbol)
  (interactive)
  (find-this-definition-helper symbol find-definition-function))

(defun find-this-definition-other-window (&optional symbol)
  (interactive)
  (let ((existing-function
         (intern-soft (format "%s-other-window"
                              (symbol-name find-definition-function)))))
    (if existing-function
        (find-this-definition-helper symbol existing-function)
      (let ((current-symbol (thing-at-point 'symbol t)))
        (run-function-other-window #'find-this-definition nil
                               current-symbol)))))

(global-set-key (kbd "M-.") 'find-this-definition)
(global-set-key (kbd "C-x 4 .") 'find-this-definition-other-window)


;;;; Mouse navigation
;;;;
(defun get-clicked-symbol (event)
  "Move to event point, and find the symbol at point."
  (mouse-set-point event)
  (let ((current-symbol (thing-at-point 'symbol t)))
    current-symbol))

(defmacro mouse-function-on-symbol (&rest body)
  "Put EVENT and CURRENT-SYMBOL in lexical environment for BODY."
  `(lambda (event) (interactive "e")
     (let ((current-symbol (get-clicked-symbol event)))
       (if current-symbol
           ,@body))))

(define-key prog-mode-map [mouse-1] (mouse-function-on-symbol (find-this-definition current-symbol)))
(define-key prog-mode-map [mouse-2] (mouse-function-on-symbol
                                     (occur (concat "\\_<" current-symbol "\\_>"))))
(define-key prog-mode-map [mouse-3] (lambda (event) (interactive "e") (pop-tag-mark)))


;;;; Move more quickly
;;;;
(global-set-key (kbd "C-S-n") (lambda (numtimes) (interactive "p")
                                (ignore-errors (next-line (* numtimes 5)))))
(global-set-key (kbd "C-S-p") (lambda (numtimes) (interactive "p")
                                (ignore-errors (previous-line (* numtimes 5)))))
(define-key search-map "O"
  (lambda () (interactive)
    (occur (concat "\\_<" (thing-at-point 'symbol t) "\\_>"))))

;; Taken from http://oremacs.com/2015/01/26/occur-dwim/ -- makes a guess for
;; the default search pattern with occur.
(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (call-interactively 'occur))

(define-key search-map "o" 'occur-dwim)


;;;; Recursive minibuffers
;;;;
(setq enable-recursive-minibuffers t)


;;;; Redefining sexp motion
;;;;
(defun backward-down-list (&optional arg)
  "Does `down-list' but with a negative argument"
  (interactive "^p")
  (down-list (- (or arg 1))))

(defun backward-up-sexp (&optional arg)
  "Does `backward-up-list' accounting for strings."
  (interactive "^p")
  (up-sexp (- (or arg 1))))

(defun up-sexp (&optional arg)
  "Move up whatever sexp we're in."
  (interactive "^p")
  (let ((ppss (syntax-ppss)))
    (cond ((nth 3 ppss)
           (goto-char (nth 8 ppss))
           (if (< arg 0)
               (up-sexp (1+ arg))
             (forward-sexp)
             (up-sexp (1- arg))))
          ((up-list arg)))))

(defun wrap-parentheses-always (&optional arg)
  "Call `insert-parentheses' with ARG a default of 1."
  (interactive "P")
  (insert-parentheses (if arg arg 1)))

(global-set-key [remap insert-parentheses] 'wrap-parentheses-always)
(global-set-key [remap up-list] 'up-sexp)
(global-set-key [remap backward-up-list] 'backward-up-sexp)
(global-set-key (kbd "C-M-n") 'up-sexp)
(global-set-key (kbd "C-M-p") 'backward-down-list)
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)


;;;; Replace yes/no by y/n
;;;;
(fset 'yes-or-no-p 'y-or-n-p)


;;;; Scrolling
;;;;
(require 'view)
(global-set-key (kbd "C-v") 'View-scroll-half-page-forward)
(global-set-key (kbd "M-v") 'View-scroll-half-page-backward)
(global-set-key (kbd "C-S-v") 'scroll-up-command)
(global-set-key (kbd "M-V") 'scroll-down-command)
(global-set-key (kbd "C-q") 'move-to-window-line-top-bottom)
(setq scroll-conservatively 101
      scroll-margin 3
      auto-window-vscroll nil
      next-screen-context-lines 3)


;;;; Customize/Abbreviation/Backups/Autosave Directory Settings
;;;;
(setq custom-file "~/.emacs.d/customize.el"
      abbrev-file-name "~/.emacs.d/abbrev_defs"
      backup-directory-alist `((".*" . "~/.emacs.d/backups/"))
      backup-by-copying-when-linked t
      auto-save-file-name-transforms `((".*" "~/.emacs.d/autosaves/" t)))
(load custom-file)


;;;; Transpose things (negative)
;;;;
;; I want negative arguments in transpose-* to "drag" the current object back
;; with repeated calls. To do this I need the point to end up at the end of the
;; same object it was called at the end of.
(defadvice transpose-subr (after bubble-back activate)
  (when (< arg 0)
    (if special
        (goto-char (car (funcall mover arg)))
      (funcall mover arg))))


;;;; User Interface
;;;;
(setq inhibit-startup-message t
      default-frame-alist '((font . "Tamsyn-10"))
      minibuffer-message-timeout 0.8
      column-number-mode t)
(setq-default major-mode nil)
(set-frame-font "Tamsyn-10")
(mouse-avoidance-mode 'none)
(global-linum-mode t)
(show-paren-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
;; Remove face commands, and emacs suspension commands
(global-unset-key (kbd "M-o"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
;; Some general key bindings
(global-set-key (kbd "<escape>") 'execute-extended-command)
(global-set-key (kbd "TAB") 'completion-at-point)
(global-set-key (kbd "C-M-i") 'indent-for-tab-command)


;;;; Whitespace and indent
;;;;
;; Automatically break long lines
;; use spaces instead of tabs
;; Don't show lines wrapped if longer than screen width
(setq-default auto-fill-function 'do-auto-fill
              indent-tabs-mode nil
              fill-column 80
              tab-width 4
              truncate-lines t
              visual-line-mode nil)
(setq indent-line-function 'insert-tab)

(defun cleanup-buffer-safe ()
  "Perform some safe whitespace operations on `current-buffer'.

Does not indent buffer, as it's used for a `before-save-hook',
and that might be bad."
  (interactive)
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun cleanup-buffer ()
  "Perform unsafe whitespace operations on `current-buffer'.

Include `indent-buffer' and `untabify', which should not be
called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (untabify (point-min) (point-max))
  (indent-region (point-min) (point-max)))

(global-set-key (kbd "C-c w") 'cleanup-buffer)
(add-hook 'before-save-hook 'cleanup-buffer-safe)


;;;; Programming shifted number keys
;;;;
(defmacro run-with-specified-command (new-event command)
  "Returns a `lambda' function that runs command with
`last-command-event' set to `new-event'"
  `(lambda (&optional arg return-command)
     "MY CHAR COMMAND WRAPPER"
     (interactive "p")
     (if return-command
         ,command
       (let ((last-command-event ,new-event))
         (call-interactively ,command)))))

(defun equivalent-current-binding (key)
  "NOTE -- this function is broken but useful.

At the moment I can't find a way to fix it, but I'm using it with
all its kludges anyway.

Finds the command that is run when `key' is pressed.

If it's a function with the `documentation' \"MY CHAR COMMAND
WRAPPER\" assumes it's a wrapper created with
`run-with-specified-command' and returns the result of (command
nil t).

Otherwise creates a `lambda' function using
`run-with-specified-command' that runs the command bound under
the false environment where `last-command-event' is KEY"
  (let ((current-binding (key-binding key)))
    (if (and (listp current-binding)
             (string-equal "MY CHAR COMMAND WRAPPER" (documentation current-binding)))
        (funcall current-binding nil t)
      (lexical-let ((current-key (aref key 0)) (old-binding current-binding))
        (run-with-specified-command current-key old-binding)))))

(defmacro anaphoric-get-bindings (left-key right-key keymap &rest body)
  (declare (indent 3))
  `(let ((left-function (equivalent-current-binding ,left-key))
         (right-function (equivalent-current-binding ,right-key))
         (keymap (or ,keymap (current-local-map))))
     ,@body))

(defun swap-these-keys (left-key right-key &optional keymap)
  "Swaps the active bindings of LEFT-KEY and RIGHT-KEY.

LEFT-KEY and RIGHT-KEY should be two objects valid in a call to `key-binding'.
Makes a new map in the local keymap, as used by `local-set-key'.

If either mapping is bound to `self-insert-command', binds the other to a `lambda'
function using `insert-char', and assumes the first is a vector of a single
character to insert.

If KEYMAP is defined, binds keys in that map, else uses `current-local-map'"
  (anaphoric-get-bindings left-key right-key keymap
    ;; By defaulting to `current-local-map' I can use a buffer local map by
    ;; default instead of making everything set up
    (define-key keymap left-key right-function)
    (define-key keymap right-key left-function)))

(defvar-local keyswap-pairs
  (list '(?1 . ?!) '(?2 . ?@) '(?3 . ?#) '(?4 . ?$) '(?5 . ?%)
        '(?6 . ?^) '(?7 . ?&) '(?8 . ?*) '(?9 . ?\() '(?0 . ?\))
        '(?- . ?_))
  "Pairs of characters to swap when calling the `toggle-shifted-keys' function.")

(defvar-local keyswap-currently-shifted nil
  "Flag variable determining whether keys are default or not.")

(defun dumb-swapping-method (&optional arg)
  "The easiest way to "
  (dolist (key-pair keyswap-pairs)
    (swap-these-keys (vector (car key-pair)) (vector (cdr key-pair)))))

(defvar-local keyswap-function 'dumb-swapping-method
  "Function that should be called to toggle the meaning of the keys in
`keyswap-pairs'.")

(defun toggle-shifted-keys (&optional arg)
  "Swaps the bindings between each key pair in `keyswap-pairs'.

Does this by calling `keyswap-function', which has the default
value of `dumb-swapping-method'."
  (interactive)
  (funcall keyswap-function arg)
  (setq keyswap-currently-shifted (not keyswap-currently-shifted)))

(defun keyswap-include-braces ()
  "Hook so `toggle-shifted-keys' includes {,[, and },]"
  (swap-these-keys [?\[] [?\{])
  (swap-these-keys [?\]] [?\}])
  (setq-local keyswap-pairs
              (append keyswap-pairs '((?\[ . ?\{) (?\] . ?\})))))

(defun keyswap-tac-underscore-exception ()
  "Hook so `toggle-shifted-keys' ignores - and _"
  (define-key (current-local-map) "-" 'self-insert-command)
  (define-key (current-local-map) "_" 'self-insert-command)
  (setq-local keyswap-pairs
              (remove '(?- . ?_) keyswap-pairs)))

;; Binding things in `prog-mode-map', which have to be overridden by minor modes
;; for special bindings.
(add-hook 'prog-mode-hook 'toggle-shifted-keys)

;; Some handy functions to wrap a currently highlighted region `wrap-region'
;; when it doesn't play nicely with my `toggle-shifted-keys' function
;;
;; (could also have `move-past-close-and-reindent' for the closing character of
;; each, and always insert a pair (instead of a single character) but I don't
;; think I want that).
(global-set-key (kbd "M-[") 'insert-pair)
(global-set-key (kbd "M-]") 'delete-pair)
(global-set-key (kbd "M-{") 'insert-pair)
(global-set-key (kbd "M-\"") 'insert-pair)
