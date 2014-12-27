;;;; User Interface
;;;;
(setq inhibit-startup-message t)
(setq default-frame-alist '((font . "Tamsyn-10")))
(set-default-font "Tamsyn-10")
(setq column-number-mode t)
(setq scroll-conservatively 1000
      scroll-step 1
      scroll-margin 3
      auto-window-vscroll nil)
(mouse-avoidance-mode 'exile)
(global-linum-mode t)
(show-paren-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-unset-key (kbd "M-o"))

;;;; Recursive minibuffers
;;;;
(setq enable-recursive-minibuffers t)

;;;; Auto Save / Backups
;;;;
(setq auto-save-default t)
(setq auto-save-interval 500)


;;;; Set the files to use
;;;;
(setq custom-file "~/.emacs.d/customize.el")
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
(setq backup-directory-alist
      `((".*" . "~/.emacs.d/backups/"))
      backup-by-copying-when-linked t)
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/autosaves/" t)))
(load custom-file)


;;;; Enable commands
;;;;
(setq disabled-command-function nil)


;;;; CamelCase word motion
;;;;
(global-subword-mode 1)


;;;; Replace yes/no by y/n
;;;;
(fset 'yes-or-no-p 'y-or-n-p)


;;;; Set Major Mode on filename
;;;;
;; Lies to set-auto-mode function so it sets major mode based on buffer name
(setq default-major-mode (lambda ()
                           (let ((buffer-file-name (or buffer-file-name (buffer-name))))
                             (set-auto-mode))))


;;;; Make scripts executeable automatically
;;;;
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)


;;;; Info
;;;;
(defun info-goto-page-in-region (startpt endpt)
  (interactive "r")
  (info (buffer-substring startpt endpt)))


;;;; Align
;;;;
(defun align-repeat (start end regexp)
  "repeat alignment with respect to
     the given regular expression"
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end
                (concat "\\(\\s-*\\)" regexp) 1 1 t))


;;;; Window Layout
;;;;
(define-key ctl-x-map "+" 'what-cursor-position)
(define-key ctl-x-map "=" 'balance-windows)

(defun fix-window-horizontal-size ()
  "Set the window's size to 80 (or prefix arg WIDTH) columns wide."
  (interactive)
  (enlarge-window (- 82 (window-width)) 'horizontal))

(define-key ctl-x-4-map "w" 'fix-window-horizontal-size)
(define-key ctl-x-4-map "g" 'delete-other-windows-vertically)

(defun split-window-horizontally-equal ()
  "I get a little annoyed every time I split windows without this."
  (interactive)
  (split-window-horizontally)
  (balance-windows))

(define-key ctl-x-map "3" 'split-window-horizontally-equal)

;;; Make it more likely that split-window-sensibly will split vertically
(setq split-height-threshold 27)
(setq split-width-threshold 175) ; 2 * 80 columns of text + line numbers etc

(setq compilation-window-height 10)


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


;;;; Whitespace and indent
;;;;
;; Automatically break long lines
;; use spaces instead of tabs
;; Don't show lines wrapped if longer than screen width
(setq-default auto-fill-function 'do-auto-fill)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 80)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq-default truncate-lines t)
(setq-default visual-line-mode nil)

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
    (forward-line (- (1+ numlines)))
    (move-to-column col)))

(global-set-key (kbd "<C-s-up>") 'move-this-line-up)
(global-set-key (kbd "<C-s-down>") 'move-this-line-down)

(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "RET") 'indent-new-comment-line)


;;;; File Handling
;;;;
(defun remove-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name))
    (delete-file filename)
    (kill-buffer buffer)
    (message "File '%s' removed" filename)))

(defun rename-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name))
    (let ((new-name (read-file-name "New name: " filename)))
      (rename-file filename new-name 1)
      (rename-buffer new-name t)
      (set-visited-file-name new-name)
      (set-buffer-modified-p nil)
      (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name)))))



;;;; Compile Shortcut
;;;;
(global-set-key (kbd "<f10>") 'compile)
(global-set-key (kbd "<C-f10>") 'recompile)


;;;; Scrolling
;;;;
(global-set-key (kbd "C-v") 'View-scroll-half-page-forward)
(global-set-key (kbd "M-v") 'View-scroll-half-page-backward)
(global-set-key (kbd "C-S-v") 'scroll-up-command)
(global-set-key (kbd "M-V") 'scroll-down-command)
(global-set-key (kbd "C-q") 'move-to-window-line-top-bottom)


;;;; Move more quickly
;;;;
(global-set-key (kbd "C-S-n") (lambda (numtimes) (interactive "p")
                                (ignore-errors (next-line (* numtimes 5)))))
(global-set-key (kbd "C-S-p") (lambda (numtimes) (interactive "p")
                                (ignore-errors (previous-line (* numtimes 5)))))


;;;; Plugins and everything not enabled by default
;;;;

;;; Set up packages and load configurations.
(require 'package)
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;;; I keep single file packages in this directory
(add-to-list 'load-path "~/.emacs.d/packages/")

(when (not package-archive-contents)
  (package-refresh-contents))

;;; Have to have this set before requiring evil
(setq evil-toggle-key "M-e")

;;; Remove the default comment mappings.
(setq evilnc-hotkey-comment-operator "")

;;; If you ask it not to remap keys, it maps them to the default function
;;; instead of just leaving them alone.
;;; Follow this variable and remove the offending maps
(setq smart-window-remap-keys nil)

;;; In a let as I don't like polluting the namespace.
(let
    ((download-only '(monokai-theme tangotango-theme pos-tip auto-complete helm
                                    ac-slime))

     (elpa-packages
      '(undo-tree paredit yasnippet key-chord goto-chg elscreen
                  ace-jump-mode wrap-region magit multiple-cursors expand-region
                  elisp-slime-nav jump-char jedi monky python-pylint
                  smart-window projectile helm-projectile arduino-mode
                  list-register ac-etags vimrc-mode xcscope
                  window-number evil-leader evil evil-exchange evil-args
                  evil-surround evil-visualstar evil-numbers evil-nerd-commenter
                  evil-jumper))

     (require-only
      '(epa-file eldoc desktop uniquify
                 buffer-move transpose-frame evil-elscreen nameses idomenu
                 le-eval-and-insert-results)))

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
;;; Note order matters in two ways here:
;;;    wrap-region after paredit to not overwrite '('
;;;    evil-leader before evil so works in initial buffers.
(dolist (conf-file
         (directory-files "~/.emacs.d/plugin_configurations" t "^.+\\.elc?$"))
  (load conf-file))
