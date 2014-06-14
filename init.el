;; Emacs package managers
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; Add command line option to use evil or not
(setq my-switch-found (member "--no-use-evil" command-line-args))
(setq command-line-args (delete "--no-use-evil" command-line-args))

(when (not package-archive-contents)
   (package-refresh-contents))

;; In a let as I don't like polluting the namespace.
(let*
    ;; Note order matters here (evil-leader has to be loaded before evil)
    ((always-loaded
      '(undo-tree paredit yasnippet goto-chg))

     (evil-extras
      '(evil-leader evil evil-exchange evil-args surround))

     (common-packages
         (if my-switch-found always-loaded (append always-loaded evil-extras)))

     (install-packages
      common-packages)

     (require-packages
      (append '(epa-file eldoc) common-packages)))


  (defun symbol-to-packages-name (package)
    (concat "~/.emacs.d/plugin_configurations/"
            (replace-regexp-in-string "-" "_" (symbol-name package))
            "_conf.el"))

  ;; Install packages, require packages, load configuration
  (dolist (p install-packages)
    (when (not (package-installed-p p))
      (package-install p)))

  (dolist (p require-packages)
    (require p))

  ;; Note that slime is requires in the _conf file, as need to add it's load
  ;;      path the list
  (dolist (p (mapcar #'symbol-to-packages-name
                     (append '(windmove ido slime)
                             common-packages)))
    (load p)))

;; General settings
(setq inhibit-startup-message t)
(set-frame-font "Tamsyn 10" nil t)
(setq auto-save-default nil)
(setq backup-inhibited t)

;; Add line numbers to buffer and show column number in status line
(global-linum-mode t)
(setq column-number-mode t)

;; Highlight matching brackets
(show-paren-mode 1)

;; Automatically break long lines
;; use spaces instead of tabs
(setq-default auto-fill-function 'do-auto-fill)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 80)

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
(setq split-width-threshold 87) ; 87 is 80 columns of text + line numbers etc


;; make searches case-sensitive by default
(setq-default case-fold-search nil)

;; Filetype specific things.
(setq c-default-style "linux"
      c-basic-offset 4)

(setq inferior-lisp-program "/usr/bin/clisp")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (wombat))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; Functions
(defun backward-up-sexp (arg)
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

;; Remap C-M-u to account for comments and strings
(global-set-key [remap backward-up-list] 'backward-up-sexp)
(global-set-key (kbd "RET") 'newline-and-indent)
