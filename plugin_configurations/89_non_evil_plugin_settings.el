;;; Colour theme
;;;
(load-theme 'monokai)

;;; Wrap Region Settings
;;;
;; Have to be before paredit so the exception works
(wrap-region-global-mode t)
(add-to-list 'wrap-region-except-modes 'paredit-mode)


;;; Ace jump Mode Settings
;;;
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

(key-chord-define-global ";q" 'ace-jump-mode)
(key-chord-define-global ";j" 'ace-jump-char-mode)

(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

(evil-leader/set-key
  "mw" 'evil-ace-jump-word-mode
  "mc" 'evil-ace-jump-char-mode)


;;; Auto complete Settings
;;;
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
             "~/.emacs.d/elpa/auto-complete-20140618.2217/dict")
(ac-config-default)

;; C-n and C-p to choose options
(setq ac-use-menu-map t)
(define-key ac-menu-map (kbd "C-n") 'ac-next)
(define-key ac-menu-map (kbd "C-p") 'ac-previous)
;; Only complete when I ask you to
(setq ac-auto-start nil)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
(define-key evil-insert-state-map (kbd "C-SPC") 'auto-complete)


;;; Buffer Move Settings
;;;
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; Set some keys for evil
(evil-leader/set-key
  "wh" 'buf-move-left
  "wj" 'buf-move-down
  "wk" 'buf-move-up
  "wl" 'buf-move-right)


;;; Elisp Slime Nav Settings
;;;
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
        (add-hook hook 'turn-on-elisp-slime-nav-mode))


;;; Elscreen Settings
;;;
;; For introduction look at https://github.com/shosti/elscreen
(elscreen-start)


;;; Expand Region Settings
;;;
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-+") 'er/contract-region)


;;; Jedi Settings
;;;
(setq jedi:use-shortcuts t)
(add-hook 'python-mode-hook 'jedi:setup)
;; See [[http://tkf.github.io/emacs-jedi/latest/][jedi docs]] for setting
;; python2


;;; Multiple Cursors Settings
;;;
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(define-key evil-visual-state-map "mn" 'mc/mark-next-like-this)
(define-key evil-visual-state-map "mp" 'mc/mark-previous-like-this)
(define-key evil-visual-state-map "ma" 'mc/mark-all-like-this)

;;; Goto chg Settings
;;;
(global-set-key [(control ?.)] 'goto-last-change)
(global-set-key [(control ?,)] 'goto-last-change-reverse)


;;; Jump Char Settings
;;;
(global-set-key (kbd "M-m") 'jump-char-forward)
(global-set-key (kbd "M-M") 'jump-char-backward)


;;; Key Chord Settings
;;;
(key-chord-mode 1)

(key-chord-define-global "gt" 'goto-line)
;; delete-other-windows-vertically


;;; Magit Settings
;;;
(setq magit-repo-dirs '("~/.emacs.d" "~/share/repos/useful-files"))
(defun magit-stage-this-file ()
  "Stage the file the current buffer is visiting."
  (interactive)
  (magit-stage-item (buffer-file-name)))

(key-chord-define-global ";g" 'magit-status)
;; git commit mode usually starts flyspell
(setq git-commit-mode-hook '(turn-on-auto-fill))

;; Set some keys for evil
(evil-leader/set-key
  "gg" 'magit-log
  "gw" 'magit-stage-this-file
  "gd" 'magit-diff-unstaged
  "gp" 'magit-push
  "gf" 'magit-pull
  "gs" 'magit-status)


;;; Paredit Settings
;;;
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

;; Add keybinding C-c d to run paredit-duplicate-closest-sexp in paredit
(define-key paredit-mode-map (kbd "C-c d") 'paredit-duplicate-closest-sexp)

;; paredit with eldoc
(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)


;;; Python mode Settings
;;;
(when (featurep 'python) (unload-feature 'python t))
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(setq
 py-set-fill-column-p t
 py-electric-colon-active t)
(setq-default
 py-shell-name "ipython"
 py-sexp-function 'py-expression)

;;; Slime Settings
;;;
(add-to-list 'load-path "~/.emacs.d/packages/slime")
(require 'slime-autoloads)
(setq slime-contribs '(slime-fancy slime-highlight-edits))

(define-key global-map (kbd "C-c s") 'slime-selector)
(add-hook 'slime-mode-hook
          (lambda ()
            (slime-highlight-edits-mode nil)
            (define-key slime-mode-map (kbd "C-c h") 'slime-highlight-edits-mode)))
(setq slime-autodoc-use-multiline-p t)

; Stop SLIME's REPL from grabbing DEL (keep balanced paranthesis)
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)



;;; Smex Settings
;;;
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "<menu>") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update)


;;; Transpose Frame Settings
;;;
(define-key ctl-x-4-map "t" 'transpose-frame)
(define-key ctl-x-4-map "v" 'flip-frame)
(define-key ctl-x-4-map "h" 'flop-frame)
(define-key ctl-x-4-map "r" 'rotate-frame-clockwise)


;;; Undo Tree Settings
;;;
(global-undo-tree-mode)

;; Keep region when undoing in region
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


;;; Yasnippet Settings
;;;
(yas-global-mode t)

(setq yas/prompt-functions '(yas/ido-prompt
                             yas/dropdown-prompt
                             yas/x-prompt
                             yas/completing-prompt
                             yas/no-prompt))
