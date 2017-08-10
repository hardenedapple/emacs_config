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


;;;; No blinking cursor
;;;;
(blink-cursor-mode -1)

;;;; Disable the bell
;;;;
(setq ring-bell-function (lambda () nil))


;;;; Fill Sentence
;;;;
(defun fill-sentence (&optional arg)
  (interactive "P")
  (save-excursion
    (backward-sentence arg)
    (set-mark-command arg)
    (forward-sentence arg)
    (call-interactively #'fill-region)))

(global-set-key (kbd "C-M-q") 'fill-sentence)


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
(define-key goto-map (kbd "M-t") 'first-error)
(define-key goto-map "t" 'first-error)
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

;;;; Get rid of Goal Column
;;;;
(define-key ctl-x-map (kbd "C-n") nil)

;;;; Help settings
;;;;
(setq help-window-select nil)
(define-key help-map "I" 'info-apropos)
(define-key help-map "A" 'apropos)


;;;; Tab completion
;;;;
(setq tab-always-indent 'complete)

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

;; Things I've tried that have some problem (for future reference)
;;
;; 1. This doesn't leave a comment leader at the beginning of the new line.
;; (progn
;;   (beginning-of-line)
;;   (indent-new-comment-line)
;;   (forward-line -1)
;;   (indent-for-tab-command)
;;   (end-of-line))
;;
;; 2. This doubly indents the line compared to `open-line-below'
;;    Usually this doesn't matter, but that depends on the value of
;;    `indent-line-function', which depends on mode
;; (defun open-line-above ()
;;   "Add new line above the current one."
;;   (interactive)
;;   (open-line-below)
;;   (transpose-lines 1)
;;   (forward-line -2)
;;   (indent-for-tab-command)
;;   (end-of-line))
;; 3. This means that the line above is aligned so that it will fit below the
;;current one. This is usually more annoying than the above.
;; (defun open-line-above ()
;;   "Add new line above the current one."
;;   (interactive)
;;   (open-line-below)
;;   (transpose-lines 1)
;;   (forward-line -2)
;;   (end-of-line))
(defun open-line-above ()
  "Add new line above the current one."
  (interactive)
  (open-line-below)
  (transpose-lines 1)
  (forward-line -2)
  ;; If `indent-line-function' is something that actually determines the
  ;; indentation to be used, we use that function.
  ;; If it just inserts a tab we remove that tab under tha assumption that most
  ;; lines will not have indentation.
  ;; If `indent-line-function' is to indent based on the line above, we remove
  ;; the indentation that has already been inserted in order to indent based on
  ;; the line above.
  (end-of-line)
  (when (member indent-line-function '(insert-tab indent-relative))
    (delete-horizontal-space t))
  (unless (eq indent-line-function 'insert-tab)
    ;; Don't call `indent-for-tab-command' because the setting of
    ;; `tab-always-indent' can make it start completion instead of just
    ;; indenting.
    (funcall indent-line-function)))

(global-set-key (kbd "C-o") 'open-line-below)
(global-set-key (kbd "C-S-o") 'open-line-above)

;;; Moving lines
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

(defconst mouse-follow-definition-key [mouse-3])
(defconst mouse-go-back-key [M-mouse-3])
(defconst mouse-get-help-key [C-mouse-3])

;; The default down-mouse event opens up a menu, which makes triggering the
;; up-mouse event a pain.
(global-set-key [C-down-mouse-3] nil)

(define-key prog-mode-map [mouse-2]
  (mouse-function-on-symbol (occur (concat "\\_<" current-symbol "\\_>"))))
(define-key prog-mode-map mouse-follow-definition-key
  (mouse-function-on-symbol (find-this-definition current-symbol)))
(define-key prog-mode-map mouse-go-back-key
  (lambda (event) (interactive "e") (pop-tag-mark)))
(define-key prog-mode-map mouse-get-help-key
  (mouse-function-on-symbol (man (symbol-name current-symbol))))


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

;;;; Multiple search-and-replace at once (like :S/{a,b}/{b,c} with vim-abolish.
;;;;
(defun query-search-and-replace-multi (pairs)
  "Replace multiple pairs of find/replace strings in region or entire buffer.

PAIRS is a sequence of pairs listed literally.

e.g. ((\"interactive\" \"Iswapped\")(\"concat\" \"Cswapped\"))
"
  (interactive "x")
  (let ((full-regexp
         (concat
          (store-substring
           (apply 'concat (map 'list (lambda (x) (concat "\\|" (car x))) pairs))
           1 ?\()
          "\\)")))
    (query-replace-regexp
     full-regexp
     (quote
      (replace-eval-replacement
       replace-quote
       (second
        (car
         (member-if (lambda (x) (string-match-p (car x) (match-string 0)))
                    pairs)))))
     nil
     (if (use-region-p) (region-beginning))
     (if (use-region-p) (region-end))
     nil
     nil)

    ;; ;; If want to do the same but without querying, can use the below.
    ;; (while (search-forward-regexp full-regexp (point-max) t)
    ;;   ;; n.b. help on match-string mentions need to be in the same buffer as the
    ;;   ;; search was made in. Don't think that's going to cause any problem, but
    ;;   ;; it might, and I'm mentioning it just in case.
    ;;   (let ((curmatch
    ;;          (member-if (lambda (x) (string-match-p (match-string 0) (car x)))
    ;;                     pairs)))
    ;;     (when curmatch
    ;;       (replace-match (second (car curmatch))))))
    ))


;;;; Quoted Insert
;;;;
;; I've used the normal mapping for `quoted-insert' for the
;; `move-to-window-line-top-bottom' function that `paredit' overwrites.
;; It turns out that I sometimes need the function, so I'll map it somewhere in
;; the global map.
(global-set-key (kbd "C-c q") 'quoted-insert)

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
(defun linum-on ()
  "Override of the default `linum-on' function.

This doesn't enable `linum-mode' if in `org-mode' or in the
`minibuffer'"
  (unless (or (minibufferp)
              (member major-mode '(org-mode)))
    (linum-mode 1)))

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
              visual-line-mode nil
              indent-line-function 'indent-relative)

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

;; Some handy functions to wrap a currently highlighted region.
;;
;; These are almost superfluous given the `wrap-region' package, but I've gotten
;; used to them during the time that I couldn't get `wrap-region' and
;; `keyswap-mode' to work nicely with each other.
;; I also like to use them when I haven't loaded any plugins, hence they stick
;; around.
;;
;; (could also have `move-past-close-and-reindent' for the closing character of
;; each, and always insert a pair (instead of a single character) but I don't
;; think I want that).
(global-set-key (kbd "M-[") 'insert-pair)
(global-set-key (kbd "M-]") 'delete-pair)
(global-set-key (kbd "M-{") 'insert-pair)
(global-set-key (kbd "M-\"") 'insert-pair)
