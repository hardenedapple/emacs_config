;;;; Align
;;;;
(defun align-repeat (start end regexp)
  "repeat alignment with respect to
     the given regular expression"
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


;;;; Enable commands
;;;;
(setq disabled-command-function nil)


;;;; File Handling
;;;;
(defun remove-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (unless (and filename (file-exists-p filename))
      (error "Buffer '%s' is not visiting a file!" name))
    (delete-file filename)
    (kill-buffer buffer)
    (message "File '%s' removed" filename)))

(defun rename-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless (and filename (file-exists-p filename))
      (error "Buffer '%s' is not visiting a file!" name))
    (let ((new-name (read-file-name "New name: " filename)))
      (rename-file filename new-name 1)
      (rename-buffer new-name t)
      (set-visited-file-name new-name)
      (set-buffer-modified-p nil)
      (message "File '%s' successfully renamed to '%s'" name
               (file-name-nondirectory new-name)))))


;;;; Help settings
;;;;
(setq help-window-select nil)


;;;; Indentation Motion
;;;;
(defun beginning-of-line-or-indentation ()
  "Move to the beginning of the line or indentation."
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
  (interactive)
  (end-of-line)
  (indent-new-comment-line))

(defun open-line-above ()
  (interactive)
  (end-of-line)
  (indent-new-comment-line)
  (transpose-lines 1)
  (forward-line -2)
  (end-of-line))

(global-set-key (kbd "C-o") 'open-line-below)
(global-set-key (kbd "C-S-o") 'open-line-above)

;;; Move lines around
(defun move-this-line-down (numlines)
  (interactive "p")
  (let ((col (current-column)))
    (forward-line)
    (transpose-lines numlines)
    (forward-line -1)
    (move-to-column col)))

(defun move-this-line-up (numlines)
  (interactive "p")
  (let ((col (current-column)))
    (forward-line)
    (transpose-lines (- numlines))
    ;; Note: I have advised `transpose-subr', which means I need to call
    ;;       `forward-line' with argument -1, if I hadn't I'd need to call it
    ;;       with argument (- (1+ NUMLINES))
    (forward-line -1)
    ;;(forward-line (- (1+ numlines)))
    (move-to-column col)))

(global-set-key (kbd "<C-s-up>") 'move-this-line-up)
(global-set-key (kbd "<C-s-down>") 'move-this-line-down)
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "RET") 'indent-new-comment-line)


;;;; Make scripts executeable automatically
;;;;
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)


;;;; Move more quickly
;;;;
(global-set-key (kbd "C-S-n") (lambda (numtimes) (interactive "p")
                                (ignore-errors (next-line (* numtimes 5)))))
(global-set-key (kbd "C-S-p") (lambda (numtimes) (interactive "p")
                                (ignore-errors (previous-line (* numtimes 5)))))


;;;; Recursive minibuffers
;;;;
(setq enable-recursive-minibuffers t)


;;;; Redefining sexp motion
;;;;
(defun backward-up-sexp (arg)
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
      column-number-mode t)
(setq-default major-mode nil)
(set-default-font "Tamsyn-10")
(mouse-avoidance-mode 'exile)
(global-linum-mode t)
(show-paren-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
;; Remove face commands, and emacs suspension commands
(global-unset-key (kbd "M-o"))
(global-unset-key (kbd "M-k"))
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
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))

(global-set-key (kbd "C-c w") 'cleanup-buffer)
(add-hook 'before-save-hook 'cleanup-buffer-safe)


;;;; Window history buffer switch
;;;;
(defvar buffer-choose-default-function 'switch-to-buffer
  "Function to call with the key C-x b  with no prefix.")

(defun window-history-buffer-choose (&optional prefix)
  "Select a buffer from the current window history."
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
  "Set the window's size to 80 (or prefix arg WIDTH) columns wide."
  (interactive)
  (enlarge-window (- (or num-columns 82) (window-width)) 'horizontal))

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
  (list 'previous-error 'next-error 'push-button)
  "Commands to use same window when calling `pop-to-buffer'")

(defun display-buffer-same-window-from-command (buffer alist)
  "Opens BUFFER in `current-window' if `this-command' is in
`display-buffer-here-commands'"
  (when (memq this-command display-buffer-here-commands)
    (display-buffer-same-window buffer alist)))

(defun display-buffer-previous-other-window (buffer alist)
  "Calls `display-buffer-in-previous-window' with
  `inhibit-same-window' set so never open in the current window.

Not included in the `display-buffer-base-action' by default, but
  kept here for when I want this sort of thing."
  (let ((alist (cons '(inhibit-same-window . t) alist)))
    (display-buffer-in-previous-window buffer alist)))

(setq window-combination-resize t
      display-buffer-base-action (list (list
                                        'display-buffer-reuse-window
                                        'display-buffer-same-window-from-command
                                        'display-buffer-in-previous-window
                                        'display-buffer-some/pop-window)))

;;; Make it more likely that split-window-sensibly will split vertically
(setq fit-window-to-buffer-horizontally t
      split-height-threshold 27
      split-width-threshold 175      ; 2 * 80 columns of text + line numbers etc
      compilation-window-height 10)

;;; Splice window functions
(defun splice-window--get-split-type (&optional window forwards)
  "Return the direction to split WINDOW.
Returns nil if WINDOW is either the root window or the minibuffer window."
  (cond
   ((window-combined-p window) (if forwards 'below 'above))
   ((window-combined-p window t) (if forwards 'right 'left))))

(defun splice-window--get-all-window-siblings
    (&optional direction window)
  "Return a list of WINDOW's siblings in given DIRECTION.

Default direction is forward."
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
  "Add window specification TO-ADD into the BASE-WINDOW's config."
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
(defun run-command-other-window (command &optional window)
  "Run COMMAND in a different window.

Takes a function to run, and calls it interactively in a
different window.

If WINDOW is nil, and `one-window-p' split the current window and
select the result, else select WINDOW or `other-window'.

Then run the COMMAND."
  (interactive "C")
  (if (window-live-p window)
      (select-window window)
    (if (one-window-p)
        (select-window (split-window-sensibly))
      (other-window 1)))
  (call-interactively command))

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

;;; In a let as I don't like polluting the namespace.
(let
    ((download-only '(helm monokai-theme tangotango-theme))

     (elpa-packages
      '(ace-jump-mode arduino-mode elisp-slime-nav expand-region goto-chg
                      helm-descbinds jump-char key-chord list-register magit monky
                      multiple-cursors paredit projectile python-pylint quack smart-tab
                      smart-window smartscan undo-tree vimrc-mode window-number wrap-region
                      xcscope yasnippet
                      ;; I occasionally use this, but not usually -- shows currently
                      ;; unbound keys, which is useful for deciding on a keybinding.
                      ;; unbound
                      ))

     (require-only
      '(buffer-move desktop eldoc em-smart eshell le-eval-and-insert-results
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
