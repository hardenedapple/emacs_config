;;;; Colour theme
;;;;
(load-theme 'monokai)

;;;; Wrap Region Settings
;;;;
;;; Have to be before paredit so the exception works
(wrap-region-global-mode t)
(add-to-list 'wrap-region-except-modes 'paredit-mode)


;;;; Ace jump Mode Settings
;;;;
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

(key-chord-define-global ";-" 'ace-jump-mode)
(key-chord-define-global ";," 'ace-jump-char-mode)

(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

(evil-leader/set-key
  "mw" 'evil-ace-jump-word-mode
  "mc" 'evil-ace-jump-char-mode)


;;;; Auto Complete Settings
;;;;
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
             "~/.emacs.d/elpa/auto-complete-20140618.2217/dict")
(ac-config-default)

;;; C-n and C-p to choose options
(setq ac-use-menu-map t)
(define-key ac-menu-map (kbd "C-n") 'ac-next)
(define-key ac-menu-map (kbd "C-p") 'ac-previous)
;;; Only complete when I ask you to
(setq ac-auto-start nil)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
(define-key evil-insert-state-map (kbd "C-SPC") 'auto-complete)

;; Add ac-source-filename to all buffers
(defun ac-common-setup ()
  (add-to-list 'ac-sources 'ac-source-filename))


;;;; Auto Complete Etags Settings
;;;;
(ac-etags-setup)
(add-hook 'c-mode-common-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-etags)))


;;;; Buffer Move Settings
;;;;
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;;; Set some keys for evil
(evil-leader/set-key
  "wh" 'buf-move-left
  "wj" 'buf-move-down
  "wk" 'buf-move-up
  "wl" 'buf-move-right)


;;;; Elisp Slime Nav Settings
;;;;
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))


;;;; Elscreen Settings
;;;;
;;; For introduction look at https://github.com/shosti/elscreen
;; Redefine elscreen-split to stay at the new tab
(defun elscreen-split ()
  (interactive)
  (if (and (null (one-window-p))
           (< (elscreen-get-number-of-screens) 10))
      (let ((elscreen-split-buffer (current-buffer)))
        (delete-window)
        (elscreen-create)
        (switch-to-buffer elscreen-split-buffer))
    (elscreen-message "cannot split screen!")))

(elscreen-start)


;;;; Expand Region Settings
;;;;
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-+") 'er/contract-region)


;;;; Goto chg Settings
;;;;
(global-set-key [(control ?.)] 'goto-last-change)
(global-set-key [(control ?,)] 'goto-last-change-reverse)


;;;; Helm Settings
;;;;
(require 'helm)
(require 'helm-config)
(require 'helm-eshell)
(require 'helm-files)
(require 'helm-grep)

(define-key helm-map (kbd "C-z") 'helm-select-action)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

(define-key helm-grep-mode-map (kbd "<return>") 'helm-grep-mode-jump-other-window)
(define-key helm-grep-mode-map (kbd "n") 'helm-grep-mode-jump-other-window-forward)
(define-key helm-grep-mode-map (kbd "p") 'helm-grep-mode-jump-other-window-backward)

(setq
 helm-google-suggest-use-curl-p t
 helm-scroll-amount 4
 helm-quick-update t
 helm-ff-search-library-in-sexp t
 helm-move-to-line-cycle-in-source t
 helm-buffers-fuzzy-matching t)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "<menu>") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(define-key ctl-x-map "b" 'helm-mini)
(define-key ctl-x-map (kbd "C-f") 'helm-find-files)
(define-key helm-command-prefix "g" 'helm-do-grep)
(define-key helm-command-prefix "o" 'helm-occur)

(define-key 'help-command (kbd "C-f") 'helm-apropos)
(define-key 'help-command (kbd "r") 'helm-info-emacs)

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "M-l") 'helm-eshell-history)))

(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)
(helm-mode 1)


;;;; Jedi Settings
;;;;
(setq jedi:use-shortcuts t)
(add-hook 'python-mode-hook 'jedi:setup)
;;; See [[http://tkf.github.io/emacs-jedi/latest/][jedi docs]] for setting
;;; python2


;;;; Jump Char Settings
;;;;
(global-set-key (kbd "M-m") 'jump-char-forward)
(global-set-key (kbd "M-M") 'jump-char-backward)


;;;; Key Chord Settings
;;;;
(key-chord-mode 1)

(key-chord-define-global "gt" 'goto-line)


;;;; List Registers Settings
;;;;
(define-key ctl-x-r-map "v" 'list-register)


;;;; Magit Settings
;;;;
(setq magit-repo-dirs '("~/.emacs.d" "~/share/repos/useful-files"))
(defun magit-stage-this-file ()
  "Stage the file the current buffer is visiting."
  (interactive)
  (magit-stage-item (buffer-file-name)))

(key-chord-define-global ";g" 'magit-status)
;; git commit mode usually starts flyspell
(setq git-commit-mode-hook '(turn-on-auto-fill))

;;; Set some keys for evil
(evil-leader/set-key
  "gg" 'magit-log
  "gw" 'magit-stage-this-file
  "gd" 'magit-diff-unstaged
  "gp" 'magit-push
  "gf" 'magit-pull
  "gs" 'magit-status)


;;;; Multiple Cursors Settings
;;;;
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(define-key evil-visual-state-map "mn" 'mc/mark-next-like-this)
(define-key evil-visual-state-map "mp" 'mc/mark-previous-like-this)
(define-key evil-visual-state-map "ma" 'mc/mark-all-like-this)


;;;; Paredit Settings
;;;;
(dolist (hook '(eval-expression-minibuffer-setup-hook
                emacs-lisp-mode-hook ielm-mode-hook lisp-mode-hook
                lisp-interaction-mode-hook scheme-mode-hook))
  (add-hook hook #'enable-paredit-mode))

(defun paredit--is-at-start-of-sexp ()
  (and (looking-at "(\\|\\[")
       (not (nth 3 (syntax-ppss))) ;; inside string
       (not (nth 4 (syntax-ppss))))) ;; inside comment

(defun paredit-duplicate-closest-sexp ()
  (interactive)
  ;; skips to start of current sexp
  (while (not (paredit--is-at-start-of-sexp))
    (paredit-backward))
  (set-mark-command nil)
  ;; while we find sexps we move forward on the line
  (while (and (bounds-of-thing-at-point 'sexp)
              (<= (point) (car (bounds-of-thing-at-point 'sexp)))
              (not (= (point) (line-end-position))))
    (forward-sexp)
    (while (looking-at " ")
      (forward-char)))
  (kill-ring-save (mark) (point))
  ;; go to the next line and copy the sexprs we encountered
  (paredit-newline)
  (yank)
  (exchange-point-and-mark))

;;; Add keybinding C-c d to run paredit-duplicate-closest-sexp in paredit
(define-key paredit-mode-map (kbd "C-c d") 'paredit-duplicate-closest-sexp)

;;; paredit with eldoc
(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)



;;;; Projectile Settings
;;;;
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'helm)
(setq projectile-use-git-grep t)
(setq projectile-remember-window-configs t)

(defun try-open-projectile-file ()
  "Attempt to open file using projectile, otherwise normal open-file"
  (interactive)
  (if (projectile-project-p)
      (helm-projectile)
    (helm-find-files)))


;;;; Slime Settings
;;;;
(add-to-list 'load-path "~/.emacs.d/packages/slime")
(require 'slime-autoloads)
(setq slime-contribs '(slime-fancy))

(define-key global-map (kbd "C-c s") 'slime-selector)

(add-hook 'slime-mode-hook
          (lambda ()
            (define-key slime-mode-map (kbd "C-c h") 'slime-highlight-edits-mode)))

(setq slime-autodoc-use-multiline-p t)


;;;; Auto Complete Slime
;;;;
(require 'ac-slime)
(add-hook 'slime-mode-hook
          (lambda ()
            (set-up-slime-ac)
            (define-key slime-mode-map (kbd "M-TAB") 'auto-complete)))

(add-hook 'slime-repl-mode-hook
          (lambda ()
            (set-up-slime-ac)
            (define-key slime-repl-mode-map (kbd "M-TAB") 'auto-complete)))

(add-to-list 'ac-modes 'slime-repl-mode)


;;;; Transpose Frame Settings
;;;;
(define-key ctl-x-4-map "t" 'transpose-frame)
(define-key ctl-x-4-map "v" 'flip-frame)
(define-key ctl-x-4-map "h" 'flop-frame)
(define-key ctl-x-4-map "r" 'rotate-frame-clockwise)


;;;; Undo Tree Settings
;;;;
(global-undo-tree-mode)

;;; Keep region when undoing in region
(defadvice undo-tree-undo (around keep-region activate)
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
            (p (set-marker (make-marker) (point))))
        ad-do-it
        (goto-char p)
        (set-mark m)
        (set-marker p nil)
        (set-marker m nil))
    ad-do-it))



;;;: Vimrc Syntax Settings
;;;;
(add-to-list 'auto-mode-alist '(".vim\\(rc\\)?$" . vimrc-mode))


;;;; Xcscope Settings
;;;;
(add-hook 'c-mode-hook 'cscope-setup)
(add-hook 'c++-mode-hook 'cscope-setup)


;;;; Yasnippet Settings
;;;;
(yas-global-mode t)

(setq yas/prompt-functions '(yas/ido-prompt
                             yas/dropdown-prompt
                             yas/x-prompt
                             yas/completing-prompt
                             yas/no-prompt))
