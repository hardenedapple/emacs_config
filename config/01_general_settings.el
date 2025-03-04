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
      auto-save-interval 5000)


;;;; No blinking cursor
;;;;
(blink-cursor-mode -1)

;;;; Disable the bell
;;;;
(setq ring-bell-function (lambda () nil))


;;;; Clipboard
;;;;
;; Don't want selecting a region to set the clipboard.
;; This has confused me enough times, and I'm not getting used to it -- just
;; thinking hard to remember to not activate the region while I have something I
;; want in the clipboard.
(setq select-active-regions 'only)

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
(defun goto-error-on-linenum (linenum)
  "Implementation of `first-error' that goes based on line number rather than
how many errors.  This means that some arguments are not valid, but it means you
can simply find the argument for the item you want based on the line numbers
shown in a given compile buffer."
  (interactive "P")
  (if (numberp linenum)
      (with-current-buffer (next-error-find-buffer)
        (goto-line linenum)
        ;; There are unfortunately a bunch of different functions that one might
        ;; have to use.  Fortunately, they're all under <RET>.  So just find
        ;; "whatever function is bound to RET" and call that.
        (funcall (key-binding (kbd "RET"))))
    (if linenum
        (first-error)
      (next-error 0))))
(define-key goto-map (kbd "M-t") 'goto-error-on-linenum)
(define-key goto-map "t" 'goto-error-on-linenum)
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


;;;; Completion (both minibuffer *and* in-buffer).
;;;;
(setq ;; icomplete-in-buffer t ; <-- doesn't seem quite what I want
      tab-always-indent 'complete
      completion-styles '(basic partial-completion initials)
      minibuffer-completion-confirm 'confirm-after-completion
      completion-auto-select 'second-tab)
(fido-vertical-mode t)

;; Up/down when completing in the minibuffer
(define-key minibuffer-local-map (kbd "C-p") #'minibuffer-previous-completion)
(define-key minibuffer-local-map (kbd "C-n") #'minibuffer-next-completion)

;; Up/down when competing in a normal buffer
;; Originally wanted to use C-n and C-p, but that interferes normal typing too
;; much when not in the minibuffer.  Similar for using RET to choose completion.
;; Defaults are M-<down> and M-<up>, but I find those harder to use than the
;; below.
(define-key completion-in-region-mode-map (kbd "M-p") 'minibuffer-previous-completion)
(define-key completion-in-region-mode-map (kbd "M-n") 'minibuffer-next-completion)

;; N.b. reminder: icomplete-fido-exit is on M-j, that's what you use in order to
;; accept current input even if there is no matching completion.

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
  (unless (memq indent-line-function '(insert-tab vsh-indent-function))
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

;;;; Mark word mapping
;;;;
;; Just makes sense for symmetry purposes that M-SPC should also perform
;; mark-word if M-@ does.
;;
;; n.b. this overrides the default mapping for `just-one-space', which is
;; actually a pretty useful command, so I move that onto the `ctl-x-map'
(global-set-key (kbd "M-SPC") 'mark-word)
(define-key ctl-x-map (kbd "M-SPC") 'just-one-space)

;;;; Minibuffer -- escape quits it
(define-key minibuffer-local-map (kbd "<escape>") 'abort-recursive-edit)
(add-hook 'minibuffer-setup-hook 'turn-off-auto-fill)


;;;; Mouse navigation
;;;;
(defmacro mouse-function-on-symbol (&rest body)
  "Put EVENT and CURRENT-SYMBOL in lexical environment for BODY."
  `(lambda (event) (interactive "e")
     (mouse-set-point event)
     (let ((current-symbol (thing-at-point 'symbol t)))
       (if current-symbol
           ,@body))))
(defun symbol->regexp (given-symbol) (concat "\\_<" given-symbol "\\_>"))

;; The default commands for these mappings is just strange when used to the
;; new commands, remove them so I don't come across them by accident.
(global-set-key [M-mouse-3] nil)

;; Down mouse events tend to open menus, and are hence a right pain when mapping
;; things.
;; TODO Search int the global map for any and all 'down mouse events and remove
;; without having to manually come across them and add them here.
(global-set-key [S-down-mouse-1] nil)
(global-set-key [C-down-mouse-1] nil)
(global-set-key [M-down-mouse-1] nil)
(global-set-key [S-down-mouse-2] nil)
(global-set-key [C-down-mouse-2] nil)
(global-set-key [M-down-mouse-2] nil)
(global-set-key [S-down-mouse-3] nil)
(global-set-key [C-down-mouse-3] nil)
(global-set-key [M-down-mouse-3] nil)



;; Unmodified mouse-1 does the basic "click here" stuff
(defconst mouse-get-help-key [C-mouse-1])
(defconst mouse-run-macro-here [M-mouse-1])
(defconst mouse-run-user-action [S-mouse-1]
  "This marks what key is kept available for the user to temporarily change
depending on current needs.

e.g. if you want to remove words from a file that don't match a regex but can
easily be seen by eye, you might want to run
  (global-set-key mouse-run-user-action 'delete-current-symbol)")

(global-set-key mouse-get-help-key
  (mouse-function-on-symbol (man (symbol-name current-symbol))))
(global-set-key mouse-run-macro-here 'kmacro-end-call-mouse)
;; Remove any default mapping on mouse-run-user-action
(global-set-key mouse-run-user-action nil)

;; SEARCH-FOR mouse buttons
(defconst mouse-occur-key [mouse-2])
(defconst mouse-lgrep-key [C-mouse-2])
(defconst mouse-rgrep-key [M-mouse-2])
(global-set-key mouse-occur-key
  (mouse-function-on-symbol (occur (symbol->regexp current-symbol))))

;; Make some functions that
(defun dummy-completing-read-function
    (prompt collection
            &optional predicate require-match initial-input hist default
            inherit-input-method)
  ;; Just return the default
  (cond ((listp default) (car default))
        (default default)
        (t "")))

(defun run-grep-on-symbol (symbol function)
  (grep-compute-defaults)
  (funcall function
           symbol
           (let ((completing-read-function 'dummy-completing-read-function)
                 ;; Ensure that we're looking for whole words.
                 ;; n.b. I'm assuming a `grep-template' geared towards GNU grep
                 ;; here, hence the choice of flags directly.
                 ;; I don't plan on accounting for anything else.
                 (grep-template (replace-regexp-in-string "-e" "-w -e" grep-template)))
             (grep-read-files ""))
           default-directory))
(global-set-key
 mouse-lgrep-key
 (mouse-function-on-symbol (run-grep-on-symbol current-symbol 'lgrep)))
(global-set-key
 mouse-rgrep-key
 (mouse-function-on-symbol (run-grep-on-symbol current-symbol 'rgrep)))

;; GOTO mouse buttons
(defconst mouse-follow-definition-key [mouse-3])
(defconst mouse-go-back-key [M-mouse-3])
(defconst mouse-go-to-next [S-mouse-3])
(defconst mouse-go-to-prev [C-mouse-3])
(defconst mouse-buffer-menu-button [M-C-mouse-3])

(define-key prog-mode-map mouse-follow-definition-key
  (mouse-function-on-symbol (find-this-definition current-symbol)))
(define-key prog-mode-map mouse-go-back-key
  (lambda (event) (interactive "e") (pop-tag-mark)))

(defun mouse-go-to-symbol (symbol direction)
  "Search in `direction' to find the next occurance of `symbol'.

Move `point' according to `re-search-{forward,backward}', and move mouse cursor
to new point."
  (let ((search-fn (if (eq direction 'forwards)
                       're-search-forward
                     're-search-backward)))
    (funcall search-fn (symbol->regexp symbol))
    ;; If the point is not currently visible, call `redisplay' before
    ;; `set-mouse-absolute-pixel-position'.
    ;; We need to do this because the scrolling that automatically happens when
    ;; the point goes off-screen doesn't happen in the middle of a command.
    ;; For example ...
    ;; (let ((original-point (point)))
    ;;    (goto-char (+ 5 (window-end)))
    ;;    (goto-char original-point))
    ;; doesn't cause any scrolling to happen.
    ;; This means that `window-absolute-pixel-position' will return `nil', and
    ;; we can't move the mouse to where `point' is.
    ;;
    ;; We can't limit calling redisplay to when we're off-screen, as sometimes
    ;; the automatic redisplay that happens after our command will scroll the
    ;; screen anyway (e.g. we're within `scroll-margin' lines of the bottom).
    (redisplay)
    (let ((pos (window-absolute-pixel-position)))
      (if pos
          ;; n.b. we move to the top-left of the last character in the match.
          ;; Would like to move the cursor to the center of the last character
          ;; in the match, but I haven't found anything letting me do that and
          ;; this works well enough (i.e. I can just keep clicking without
          ;; having to move the mouse).
          ;; (could find the position of characters below and to the right, then
          ;; go to the position part way between ... but would then need to
          ;; check for end of buffer etc and would probably have a bunch of
          ;; other problems too).
          (set-mouse-absolute-pixel-position (car pos) (cdr pos))
        (message "mouse-go-to-symbol didn't get a position")))))

(global-set-key mouse-go-to-next
  (mouse-function-on-symbol (mouse-go-to-symbol current-symbol 'forwards)))
(global-set-key mouse-go-to-prev
  (mouse-function-on-symbol (mouse-go-to-symbol current-symbol 'backwards)))
(global-set-key mouse-buffer-menu-button 'mouse-buffer-menu)


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
(global-set-key (kbd "C-c m") 'move-to-window-line-top-bottom)
;; Used to have this, but missed 'quoted-insert on a key that would still work
;; within isearch (and other modes that quit when you type something outside of
;; the map).
;; (global-set-key (kbd "C-q") 'move-to-window-line-top-bottom)
(setq scroll-conservatively 101
      scroll-margin 3
      auto-window-vscroll nil
      next-screen-context-lines 3)


;;;; Abbreviation/Backups/Autosave Directory Settings
;;;;
(setq abbrev-file-name "~/.emacs.d/abbrev_defs"
      backup-directory-alist `((".*" . "~/.emacs.d/backups/"))
      backup-by-copying-when-linked t
      auto-save-file-name-transforms `((".*" "~/.emacs.d/autosaves/" t)))

;;;; User Interface
;;;;
(setq inhibit-startup-message t
      minibuffer-message-timeout 0.8
      column-number-mode t)
(setq-default major-mode nil)

(defun adjust-for-home-monitor ()
  (set-frame-font "Liberation Mono 8")
  (modify-all-frames-parameters '((font . "Liberation Mono 8"))))
;; This changes depending on the monitor I get given, but don't expect to change
;; for a while so we should be fine for the moment.
(defun adjust-for-work-monitor ()
  (set-frame-font "Liberation Mono 11")
  (modify-all-frames-parameters '((font . "Liberation Mono 11"))))
;; N.b. Setting default for in the office (making a guess where is the most
;; important).
(adjust-for-work-monitor)

(mouse-avoidance-mode 'none)
(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode)
  (global-linum-mode t))

(defun linum-on ()
  "Override of the default `linum-on' function.

This doesn't enable `linum-mode' if in `org-mode' or in the
`minibuffer'"
  (unless (or (minibufferp)
              (member major-mode '(org-mode)))
    (linum-mode 1)))

;;;; I read in the wiki this suggested function.
;;;; https://www.emacswiki.org/emacs/LineNumbers
;;;; 
;;;; Initial testing shows numbers aren't displaying in the minibuffer anyway,
;;;; so I'm not yet turning it on.
;; (defun display-line-numbers--turn-on ()
;;   "Override of the default `display-line-numbers--turn-on' function.
;;
;; This avoids enabling line numbers in the minnibuffer."
;;   (when (not (minibufferp))
;;     (display-line-numbers-mode)))

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
;; Remove `downcase-region' keybinding since I only ever accidentally use it and
;; that accident happens quite a lot.  Remove `upcase-region' keybinding too
;; for symmetry.
(global-unset-key (kbd "C-x C-l"))
(global-unset-key (kbd "C-x C-u"))

(setq x-select-enable-primary t)
(setq x-select-enable-clipboard t)

;;;; Whitespace and indent
;;;;
;; Automatically break long lines
;; use spaces instead of tabs
;; Don't show lines wrapped if longer than screen width
(setq-default auto-fill-function 'do-auto-fill
              indent-tabs-mode nil
              fill-column 80
              tab-width 8
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
;; (add-hook 'before-save-hook 'cleanup-buffer-safe)

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


;;;; Case-sensitive zap-to-char
;;;;
;; I like `case-fold-search' with `isearch' and the like, but for `zap-to-char'
;; I find it annoying.
(global-set-key (kbd "M-z")
                (lambda () (interactive)
                  "Run `zap-to-char' case-sensitively."
                  (let ((case-fold-search nil))
                    (call-interactively 'zap-to-char))))

;;;; Query search and replace to remove right-prompts from Zsh pasted sessions.
;;;;
;; This is something I do often enough that I want to easily remember how to do
;; it.  Usually I have a session open where I've done it before and can just use
;; that history.  However, sometimes I lose that session, and in losing that
;; session have to remember the regex again.
;;
;; N.b. there is the basic [<exit-code>,<history-number>] pattern which is at
;; the end of every line, then there is an optional [git][<branch-name>] before
;; it, and if the current git worktree is modified from the branch name we have
;; a `U` in between the two.
(defun config-remove-trailing-zsh-prompt ()
  (interactive)
  (let ((start (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max))))
    (query-replace-regexp "\\s-*\\(\\[git\\]\\[[^[:space:]]+\\] \\(U\\)? \\)?\\[[[:digit:]]+,[[:digit:]]+\\]$" "" nil start end)))

(defun config-remove-trailing-perf-tokens ()
  (interactive)
  (let ((start (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max))))
    (query-replace-regexp "\\s-*\\(▒\\|◆\\)$" "" nil start end)))
