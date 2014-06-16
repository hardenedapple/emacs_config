;; Emacs package managers
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; I keep single file packages in this directory
(add-to-list 'load-path "~/.emacs.d/packages/")


;; ;; Add command line option to choose something
;; (setq my-switch-found (member "--my-switch" command-line-args))
;; (setq command-line-args (delete "--my-switch" command-line-args))

(when (not package-archive-contents)
   (package-refresh-contents))

;; In a let as I don't like polluting the namespace.
(let
    ((common-packages
      '(undo-tree paredit yasnippet goto-chg wrap-region magit
                  evil-leader evil evil-exchange evil-args surround))

     (require-and-configure
      '(buffer-move transpose-frame))

     (require-only
      '(epa-file eldoc))

     ;; NOTE slime is only here because the configuration file requires slime
     ;;      itself, it does this because the load path has to be modified
     ;;      before can get at the slime folder
     (configure-only
     '(windmove ido slime ediff cua)))

  (let
      ((install-packages
        common-packages)

       (require-packages
        (append require-only require-and-configure common-packages))

       (config-packages
        (append configure-only require-and-configure common-packages)))



    (defun package-to-config-file (package)
      (concat "~/.emacs.d/plugin_configurations/"
              (replace-regexp-in-string "-" "_" (symbol-name package))
              "_conf.el"))

    ;; Install packages, require packages, load configuration

    (dolist (p install-packages)
      (when (not (package-installed-p p))
        (package-install p)))

    (dolist (p require-packages)
      (require p))

    (dolist (p (mapcar #'package-to-config-file config-packages))
      (load p))))

;; General settings
(setq inhibit-startup-message t)
(setq default-frame-alist '((font . "Tamsyn-10")))
(set-default-font "Tamsyn-10")
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

(setq inferior-lisp-program "/usr/bin/sbcl")

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

;; Remove M-r as move-to-window-line-top-bottom, and replace with M-p
;;   (done because paredit binds M-r)
(global-set-key (kbd "M-p") 'move-to-window-line-top-bottom)
