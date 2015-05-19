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


;;;; CamelCase word motion
;;;;
(global-subword-mode 1)


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

(define-key prog-mode-map [mouse-1] (mouse-function-on-symbol (find-tag current-symbol)))
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


;;;; Recursive minibuffers
;;;;
(setq enable-recursive-minibuffers t)


;;;; Redefining sexp motion
;;;;
(defun backward-up-sexp (arg)
  "Move up whatever sexp we're in."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'backward-up-sexp)
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
(mouse-avoidance-mode 'exile)
(global-linum-mode t)
(show-paren-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
;; Remove face commands, and emacs suspension commands
(global-unset-key (kbd "M-o"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))


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


;;;; Window history buffer switch
;;;;
(defvar buffer-choose-default-function 'switch-to-buffer
  "Function to call with unprefixed C-x b")

(defun window-history-buffer-choose (&optional prefix)
  "Select a buffer from the history of `current-window'."
  (interactive "P")
  (if prefix
      (let ((buffer-names
             (mapcar (lambda (list-thing) (buffer-name (car list-thing)))
                     (append (window-prev-buffers) (window-next-buffers)))))
        (if buffer-names
            (switch-to-buffer
             (completing-read "Buffer previous " buffer-names
                              nil t nil nil (car buffer-names) nil))
          (call-interactively buffer-choose-default-function)))
    (call-interactively buffer-choose-default-function)))

(define-key ctl-x-map "b" 'window-history-buffer-choose)


;;;; Window Layout
;;;;
(define-key ctl-x-map "+" 'what-cursor-position)
(define-key ctl-x-map "=" 'balance-windows)

(defun fix-window-horizontal-size (&optional num-columns)
  "Set `current-window' size to 90 NUM-COLUMNS columns wide."
  (interactive "P")
  (enlarge-window (- (or num-columns 90) (window-width)) 'horizontal))

(define-key ctl-x-4-map "w" 'fix-window-horizontal-size)
(define-key ctl-x-4-map "g" 'delete-other-windows-vertically)

;;; DISPLAY-BUFFER settings
(defun display-buffer-some/pop-window (buffer alist)
  "If `one-window-p' display in new window.

Otherwise try `display-buffer-use-some-window'."
  (if (one-window-p)
      (display-buffer-pop-up-window buffer alist)
    ;; Note below uses the LEAST recencly used window
    ;;
    ;; For some things this might be wanted (only notice things that are
    ;; annoying -- haven't had this the other way round), I know for helm
    ;; buffers, this is annoying as it means the window the buffer comes up in
    ;; cycles through all other windows in turn.
    ;;
    ;; Have changed `helm-display-function', so this isn't a problem, just
    ;; leaving a note here for the future.
    (display-buffer-use-some-window buffer alist)))

(defvar display-buffer-here-commands
  (list 'previous-error 'next-error 'first-error 'push-button 'occur-mode-goto-occurrence)
  "Commands to use same window when calling `pop-to-buffer'")

(defun display-buffer-same-window-from-command (buffer alist)
  "Open BUFFER in `current-window' if `this-command' is in
`display-buffer-here-commands'"
  (when (memq this-command display-buffer-here-commands)
    (display-buffer-same-window buffer alist)))

(defun display-buffer-previous-other-window (buffer alist)
  "Call `display-buffer-in-previous-window' with
`inhibit-same-window' set so never open in `current-window'."
  (let ((alist (cons '(inhibit-same-window . t) alist)))
    (display-buffer-in-previous-window buffer alist)))

(setq window-combination-resize t
      display-buffer-base-action (list (list
                                        'display-buffer-reuse-window
                                        'display-buffer-same-window-from-command
                                        'display-buffer-previous-other-window
                                        'display-buffer-some/pop-window)))

;;; Make it more likely that split-window-sensibly will split vertically
(setq fit-window-to-buffer-horizontally t
      split-height-threshold 27
      split-width-threshold 175      ; 2 * 80 columns of text + line numbers etc
      compilation-window-height 10)

;;; Splice window functions
(defun splice-window--get-split-type (&optional window forwards)
  "Return split direction for WINDOW.
Returns nil if WINDOW is either the root window or the minibuffer window."
  (cond
   ((window-combined-p window) (if forwards 'below 'above))
   ((window-combined-p window t) (if forwards 'right 'left))))

(defun splice-window--get-all-window-siblings
    (&optional direction window)
  "Return list of WINDOW's siblings in given DIRECTION.

Default direction is next."
  (let* ((window-iterator-function (case direction
                                     (prev 'window-prev-sibling)
                                     (t 'window-next-sibling)))
         ;; Here selected window takes over when there are no siblings
         (current-sibling (or window (selected-window)))
         return-list)
    (while (setq current-sibling
                 (funcall window-iterator-function current-sibling))
      (push
       (window-state-get current-sibling t)
       return-list))
    return-list))

(defun splice-window--add-back-windows (base-window to-add forwards
                                                    &optional direction)
  "Add window specification TO-ADD into BASE-WINDOW's config."
  (let ((direction
         (or direction
             (splice-window--get-split-type base-window forwards)
             (if (>= (/ (window-body-width base-window) split-width-threshold)
                     (/ (window-body-height base-window) split-height-threshold))
                 (if forwards 'right 'left)
               (if forwards 'below 'above)))))
    ;; Recursion needed
    ;; Split current window, work on new one
    ;; set window-combination-limit to t
    ;; Do splice-window--add-back-windows on first child
    (dolist (conf to-add)
      (window-state-put
       conf (split-window base-window nil direction) 'safe))))

(defun splice-window-upwards (&optional window)
  "Splice current level of WINDOW ancestry up one.

If WINDOW is not eq to or a child of `frame-root-window', then it
  must be at least one level of nesting down.

This function remove one level of nesting e.g., given this frame

    +------------+------------+
    |            |     B      |
    |     A      +------------+
    |            |     C      |
    +------------+------------+
    |            D            |
    +-------------------------+

when called on window B leaves the frame as

    +--------+--------+-------+
    |        |        |       |
    |    A   |    B   |   C   |
    |        |        |       |
    +--------+--------+-------+
    |            D            |
    +-------------------------+

when called on window A, leave the frame as
not scaled correctly

    +-------------------------+
    |                         |
    |            A            +
    |                         |
    +-------------------------+
    |            C            |
    +-------------------------+
    |            B            |
    +-------------------------+
    |                         |
    |            D            |
    |                         |
    +-------------------------+

This function leaves any preexisting subnests like the B/C window
in the example."
  (interactive)
  (let ((cur-win (or window (selected-window)))
        (original-win (selected-window)))
    (unless (or (one-window-p)
                (frame-root-window-p (window-parent cur-win)))
      (let ((forward-siblings
             (splice-window--get-all-window-siblings 'next cur-win))
            (backward-siblings
             (splice-window--get-all-window-siblings 'prev cur-win)))
        ;; Remove current siblings
        ;; once all siblings are closed, emacs automatically splices the
        ;; remaining window into the above level.
        (delete-other-windows-internal cur-win (window-parent cur-win))
        ;; If selected window is in the windows to put back, `window-state-put'
        ;; deals with selecting the window. If it isn't
        ;; `delete-other-windows-internal' has just selected the wrong window,
        ;; and won't change it back, so we deal with that here
        (when (window-live-p original-win) (select-window original-win))
        (splice-window--add-back-windows cur-win forward-siblings t)
        (splice-window--add-back-windows cur-win backward-siblings nil)))))

(defun merge-windows (window1 &rest windows)
  "Merge all WINDOWS into WINDOW1.

This is the opposite of `splice-window-upwards'."
  (let ((confs (mapcar #'window-state-get windows))
        (direction (if (window-combined-p window1) 'right 'below)))
    (dolist (add-window windows)
      (delete-window add-window))
    (dolist (add-conf confs)
      (window-state-put add-conf (split-window window1 nil direction)))))

(define-key ctl-x-4-map "s" 'splice-window-upwards)

;;; Swap windows
(defun swap-windows-properly (window1 &optional window2)
  "Swap WINDOW1 and WINDOW2 respecting any splits

Takes two windows, and swaps them, works if one or both of the
windows are internal ones, i.e. if `window-live-p' returns `nil'
on them."
  (unless (windowp window1)
    (error "Must supply valid WINDOW1 to SWAP-WINDOWS-PROPERLY"))
  (let ((window2 (or window2 (selected-window))))
    (let ((state1 (window-state-get window1 t))
          (state2 (window-state-get window2 t)))
      (window-state-put state1 window2 'safe)
      (window-state-put state2 window1 'safe))))

;;; Run command in other window
;; For a similar function see my configuration for smart-window
(defun run-function-other-window (function &optional window &rest args)
  "Call FUNCTION with arguments ARGS in a different window.

Takes a function to run, and executes it with WINDOW selected.

If WINDOW is `nil', and `one-window-p' split the current window
and select the result, else select WINDOW or `other-window'.

Then `apply' ARGS to FUNCTION."
  (if (window-live-p window)
      (select-window window)
    (if (one-window-p)
        (select-window (split-window-sensibly))
      (other-window 1)))
  (apply function args))

(defun run-command-other-window (command &optional window)
  (interactive "C")
  (run-function-other-window #'call-interactively window command))

(define-key ctl-x-4-map (kbd "M-x") 'run-command-other-window)


;;;; Plugins and everything not enabled by default
;;;;

;;; Set up packages and load configurations.
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;;; I keep single file packages in this directory
(add-to-list 'load-path "~/.emacs.d/packages/")

(when (not package-archive-contents)
  (package-refresh-contents))

(let
    ((download-only '(monokai-theme tangotango-theme))

     (elpa-packages
      '(ace-jump-mode arduino-mode buffer-move elisp-slime-nav expand-region
                      goto-chg ido-ubiquitous ido-at-point ido-vertical-mode
                      jump-char list-register magit monky multiple-cursors
                      paredit projectile python-pylint quack smart-tab
                      smart-window smartscan smex undo-tree vimrc-mode
                      window-number wrap-region xcscope yasnippet
                      transpose-mark
                      ;; I occasionally use this, but not usually -- shows currently
                      ;; unbound keys, which is useful for deciding on a keybinding.
                      ;; unbound
                      ))

     (require-only
      '(desktop dired-x eldoc em-smart eshell le-eval-and-insert-results
                nameses transpose-frame uniquify epa-file)))

  (let
      ((require-packages
        (append require-only elpa-packages)))

    ;; Install packages, require packages
    (dolist (p (append elpa-packages download-only))
      (unless (package-installed-p p)
        (package-install p)))

    (dolist (p require-packages)
      (require p))))

;;; Load all files in the directory plugin_configurations
;;; Name of file denotes order it's loaded in.
;;; Note order matters in way here:
;;;    wrap-region after paredit to not overwrite '('
(dolist (conf-file
         (directory-files "~/.emacs.d/plugin_configurations" t "^[^.].+\\.elc?$"))
  (load conf-file))

;;; Remove some settings from MINOR-MODE-ALIST
;;;
(defvar minor-mode-show-list
  (list
   'compilation-in-progress
   'compilation-minor-mode
   'compilation-shell-minor-mode
   'diff-minor-mode
   'overwrite-mode
   'projectile-mode
   'undo-tree-visualizer-selection-mode
   'window-number-mode
   )
  "List of minor-modes to show in modeline")
(setq minor-mode-alist
      (cl-remove-if-not
       (lambda (val)
         (member (car val) minor-mode-show-list))
       minor-mode-alist))
