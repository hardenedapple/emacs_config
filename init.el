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


;;;; CamelCase word motion
;;;;
(global-subword-mode 1)


;;;; Compile Shortcut
;;;;
(global-set-key (kbd "<f9>") 'compile)
(global-set-key (kbd "<C-f9>") 'recompile)


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


;;;; Info
;;;;
(defun info-goto-page-in-region (startpt endpt)
  (interactive "r")
  (info (buffer-substring startpt endpt)))


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
    ;; Note: I have advised TRANSPOSE-SUBR, which means I need to call
    ;; FORWARD-LINE with argument -1, if I hadn't I'd need to call it with
    ;; argument (- (1+ NUMLINES))
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


;;;; Set the files to use
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
(set-default-font "Tamsyn-10")
(mouse-avoidance-mode 'exile)
(global-linum-mode t)
(show-paren-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-unset-key (kbd "M-o"))


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

;; Set up how new/different buffers are displayed
(defun display-buffer-some/pop-window (buffer alist)
  "If `selected-window' is only one in this frame, display in new
window. Otherwise try `display-buffer-use-some-window'."
  (if (frame-root-window-p (selected-window))
      (display-buffer-pop-up-window buffer alist)
    (display-buffer-use-some-window buffer alist)))

(defun display-buffer-same-windows (buffer alist)
  "If we're currently in a *Help* window, display BUFFER here.

Exception if the buffer to display is if `buffer-name' of BUFFER
is *Help*. When calling the help function, `buffer-name' of
`current-buffer' is *Help*, which means this function would act
even though it's not what I want, so this exception is added."
  (when (and (string-equal "*Help*" (buffer-name (current-buffer)))
             (not (string-equal "*Help*" (buffer-name buffer))))
    (display-buffer-same-window buffer alist)))

(setq window-combination-resize t
      display-buffer-base-action (list (list
                                        'display-buffer-reuse-window
                                        'display-buffer-same-windows
                                        'display-buffer-some/pop-window)))

;;; Make it more likely that split-window-sensibly will split vertically
(setq fit-window-to-buffer-horizontally t
      split-height-threshold 27
      split-width-threshold 175      ; 2 * 80 columns of text + line numbers etc
      compilation-window-height 10)

;;; Splice current windows into parent tree
(defun splice-window--get-split-type (&optional window forwards)
  "Return the direction to split WINDOW.
Returns nil if WINDOW is either the root window or the minibuffer window."
  (catch 'configured
    (when (window-combined-p window)
      (throw 'configured (if forwards 'below 'above)))
    (when (window-combined-p window t)
      (throw 'configured (if forwards 'right 'left)))))

(defun splice-window--get-all-window-siblings
    (&optional direction window recursive)
  "Return a list of WINDOW's siblings in given DIRECTION.

Default direction is forward.

The RECURSIVE parameter decides what to do if any siblings don't
satisfy `window-live-p', if RECURSIVE is nil (the default),
return nil, otherwise recurse into the non-live window, storing
its configuration as a sublist. "
  (catch 'dead-window
    ;; Check if the window given is a
    (let* ((window-iterator-function (case direction
                                       (prev 'window-prev-sibling)
                                       (t 'window-next-sibling)))
           ;; Here selected window takes over when there are no siblings
           (current-sibling (or window (selected-window)))
           return-list)
      (while current-sibling
        (push
         (if (window-live-p current-sibling)
             (list
              (window-buffer current-sibling)
              (window-start current-sibling)
              (window-point current-sibling)
              (window-hscroll current-sibling)
              (window-dedicated-p current-sibling)
              (window-redisplay-end-trigger)
              current-sibling)
           (if recursive
               (let ((first-child (window-child current-sibling)))
                 (cons (splice-window--get-split-type first-child t)
                       (splice-window--get-all-window-siblings
                        'next first-child recursive)))
             (throw 'dead-window nil)))
         return-list)
        (setq current-sibling
              (funcall window-iterator-function current-sibling)))
      return-list)))

(defun splice-window--setup-window (window conf forwards direction)
  "Helper function for `splice-window--add-back-windows'

Applies the configuration CONF to WINDOW, if CONF is a subtree,
recurse into `splice-window--add-back-windows'"
  (let ((buffer (pop conf)))
    (if (symbolp buffer)
        (let ((window-combination-limit t))
          (splice-window--add-back-windows window conf forwards buffer))
      (set-window-buffer window buffer)
      (set-window-start window (pop conf))
      (set-window-point window (pop conf))
      (set-window-hscroll window (pop conf))
      (set-window-dedicated-p window (pop conf))
      (set-window-redisplay-end-trigger window (pop conf))
      (let ((orig-window (pop conf))
            (ol-func (lambda (ol)
                       (if (eq (overlay-get ol 'window) orig-window)
                           (overlay-put ol 'window window))))
            (ol-lists (with-current-buffer buffer
                        (overlay-lists))))
        (mapc ol-func (car ol-lists))
        (mapc ol-func (cdr ol-lists))))))

(defun splice-window--add-back-windows (base-window to-add forwards
                                                    &optional direction)
  "Add window specification TO-ADD into the BASE-WINDOW's config."
  (let ((direction
         (or direction
             (splice-window--get-split-type base-window forwards)
             (if (>= (/ (window-body-width base-window) split-width-threshold)
                     (/ (window-body-height base-window) split-height-threshold))
                 (if forwards 'right 'left)
               (if forwards 'below 'above))))
        (original-combination-limit window-combination-limit))
    ;; Recursion needed
    ;; Split current window, work on new one
    ;; set window-combination-limit to t
    ;; Do splice-window--add-back-windows on first child
    (let* ((conf (pop to-add)))
      (if to-add
          ;; Have more stuff to do -- continue
          (let ((window (split-window base-window nil direction)))
            (splice-window--setup-window
             window conf forwards direction)
            (splice-window--add-back-windows
             base-window to-add forwards direction))
        ;; Are on the last window of this tree, set up current window with the
        ;; configuration, and leave
        (splice-window--setup-window
         base-window conf forwards direction)))))

(defun splice-window--remove-current-siblings (window)
  "Delete all siblings of WINDOW

Actually, splits `window-parent' of WINDOW, then deletes it,
  returning the remaining window."
  (let ((window (window-parent window)))
    (prog1
        (split-window window nil (splice-window--get-split-type window))
      (delete-window window))))

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
(not scaled correctly)

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

This function leaves any preexisting subnests like the B/C window in the
example.

As this function doesn't yet take account of original window sizes, it's
advisable to have `window-combination-resize' set to `t' when using this
function."
  (interactive)
  (let ((forward-siblings (splice-window--get-all-window-siblings 'next nil t))
        (backward-siblings (splice-window--get-all-window-siblings 'prev nil t))
        (cur-win (or window (selected-window))))
    ;; Check it makes sense to call this function in the current environment
    (unless (or (frame-root-window-p cur-win)
                (frame-root-window-p (window-parent cur-win)))
      ;; Remove current siblings
      ;; once all siblings are closed, emacs automatically splices the remaining
      ;; window into the above level.
      (let ((cur-win (splice-window--remove-current-siblings cur-win)))
        (splice-window--add-back-windows cur-win forward-siblings t)
        (splice-window--add-back-windows cur-win backward-siblings nil)
        (select-window cur-win)))))

(define-key ctl-x-4-map "s" 'splice-window-upwards)


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
    ((download-only '(monokai-theme tangotango-theme helm))

     (elpa-packages
      '(undo-tree paredit yasnippet key-chord goto-chg
                  ace-jump-mode wrap-region magit multiple-cursors expand-region
                  elisp-slime-nav jump-char quack monky python-pylint
                  smart-window projectile arduino-mode
                  list-register vimrc-mode xcscope smart-tab helm-descbinds
                  smartscan window-number
                  ;; I occasionally use this, but not usually -- shows currently
                  ;; unbound keys, which is useful for deciding on a keybinding.
                  ;; unbound
                  ))

     (require-only
      '(epa-file eldoc desktop uniquify
                 buffer-move transpose-frame
                 eshell em-smart
                 nameses le-eval-and-insert-results)))

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
