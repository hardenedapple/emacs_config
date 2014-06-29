;; Set up packages and load configurations.
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; I keep single file packages in this directory
(add-to-list 'load-path "~/.emacs.d/packages/")

(when (not package-archive-contents)
  (package-refresh-contents))

;; Have to have this set before requiring evil
(setq evil-toggle-key "C-c C-z")

;; In a let as I don't like polluting the namespace.
(let
    ((elpa-packages
      '(smex undo-tree paredit yasnippet key-chord goto-chg elscreen
             ace-jump-mode wrap-region magit multiple-cursors expand-region
             elisp-slime-nav jump-char
             window-number evil-leader evil evil-exchange evil-args
             evil-surround))
     (require-only
      '(epa-file eldoc desktop uniquify
                 buffer-move transpose-frame evil-elscreen nameses
                 le-eval-and-insert-results)))

  (let
      ((require-packages
        (append require-only elpa-packages)))

    ;; Install packages, require packages
    (dolist (p elpa-packages)
      (when (not (package-installed-p p))
        (package-install p)))

    (dolist (p require-packages)
      (require p))))

;; Load all files in the directory plugin_configurations
;; Name of file denotes order it's loaded in.
;; Note order matters in two ways here:
;;    wrap-region after paredit to not overwrite '('
;;    evil-leader before evil so works in initial buffers.
(dolist (conf-file
         (directory-files "~/.emacs.d/plugin_configurations" t "^.+\\.elc?$"))
  (load conf-file))


;; Settings always run regardless of extra plugins.
;; Have loaded the extra plugins and done all configurations for the extras.
;; Now I do general settings.

(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
(setq inhibit-startup-message t)
(setq default-frame-alist '((font . "Tamsyn-10")))
(set-default-font "Tamsyn-10")
(setq auto-save-default nil)

(setq backup-directory-alist
      `((".*" . "~/.emacs.d/backup_files/")))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/backup_files/" t)))

;; Add line numbers to buffer and show column number in status line
(global-linum-mode t)
(setq column-number-mode t)

;; Highlight matching brackets
(show-paren-mode 1)

;; Automatically break long lines
;; use spaces instead of tabs
(setq-default auto-fill-function 'do-auto-fill)
(setq-default fill-column 80)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; remove scrollbar, menubar, and toolbar in gui
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)


;; Enable some useful commands disabled by default
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'erase-buffer 'disabled nil)

;; Make it more likely that split-window-sensibly will split vertically
(setq split-height-threshold 27)
(setq split-width-threshold 175) ; 2 * 80 columns of text + line numbers etc

;; C mode specific things.
(setq c-default-style "linux"
      c-basic-offset 4)
(c-set-offset 'case-label '+)

(setq inferior-lisp-program "/usr/bin/sbcl")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(ansi-color-names-vector ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(ibuffer-saved-filter-groups (quote (("Split by files" ("with files" (filename . ".*"))))))
 '(ibuffer-saved-filters (quote (("gnus" ((or (mode . message-mode) (mode . mail-mode) (mode . gnus-group-mode) (mode . gnus-summary-mode) (mode . gnus-article-mode)))) ("programming" ((or (mode . emacs-lisp-mode) (mode . cperl-mode) (mode . c-mode) (mode . java-mode) (mode . idl-mode) (mode . lisp-mode))))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;; Functions
(defun info-goto-page-in-region (startpt endpt)
  (interactive "r")
  (info (buffer-substring startpt endpt)))

(defun backward-up-sexp (arg)
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(defun beginning-of-line-or-indentation ()
  "Move to the beginning of the line or indentation."
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))


;; Whitespace and indent
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


;; New line functions
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


;; Move lines around

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
    (forward-line -1)
    (move-to-column col)))


;; File Handling
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


;; Bindings

(global-set-key (kbd "C-a") 'beginning-of-line-or-indentation)
(global-set-key (kbd "C-c w") 'cleanup-buffer)

(global-set-key (kbd "<C-s-up>") 'move-this-line-up)
(global-set-key (kbd "<C-s-down>") 'move-this-line-down)

(define-key ctl-x-4-map "g" 'delete-other-windows-vertically)

;; Remap C-M-u to account for comments and strings
(global-set-key [remap backward-up-list] 'backward-up-sexp)

;; Lines
(global-set-key (kbd "C-o") 'open-line-below)
(global-set-key (kbd "C-S-o") 'open-line-above)
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "RET") 'indent-new-comment-line)

;; Move more quickly
(global-set-key (kbd "C-S-n") (lambda () (interactive) (ignore-errors (next-line 5))))

;; Scrolling
(global-set-key (kbd "C-v") 'View-scroll-half-page-forward)
(global-set-key (kbd "M-v") 'View-scroll-half-page-backward)
(global-set-key (kbd "C-S-v") 'cua-scroll-up)
(global-set-key (kbd "M-S-v") 'cua-scroll-down)

;;; Remaps for Dvorak keyboard
;; This would be C-S-p if not dvorak
(global-set-key (kbd "C-S-h") (lambda () (interactive) (ignore-errors (previous-line 5))))
(global-set-key (kbd "C-;") ctl-x-map)
(keyboard-translate ?\C-h ?\C-p)
(keyboard-translate ?\C-p ?\C-h)
(keyboard-translate ?\C-z ?\C-x)
(keyboard-translate ?\C-x ?\C-z)
(add-hook 'after-make-frame-functions
          (lambda (f) (with-selected-frame f
                        (keyboard-translate ?\C-h ?\C-p)
                        (keyboard-translate ?\C-p ?\C-h)
                        (keyboard-translate ?\C-z ?\C-x)
                        (keyboard-translate ?\C-x ?\C-z))))

;;; Hooks
;; Various superfluous white-space.
(add-hook 'before-save-hook 'cleanup-buffer-safe)
