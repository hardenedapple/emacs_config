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
(key-chord-define-global ";-" 'ace-jump-word-mode)
(key-chord-define-global ";," 'ace-jump-char-mode)

(setq ace-jump-mode-scope 'window)


;;;; Buffer Move Settings
;;;;
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)


;;;; Elisp Slime Nav Settings
;;;;
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))


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

(define-key helm-map (kbd "C-q") 'helm-maybe-exit-minibuffer)

(define-key helm-grep-mode-map (kbd "<return>") 'helm-grep-mode-jump-other-window)
(define-key helm-grep-mode-map (kbd "n") 'helm-grep-mode-jump-other-window-forward)
(define-key helm-grep-mode-map (kbd "p") 'helm-grep-mode-jump-other-window-backward)

(when (executable-find "curl")
  (setq  helm-google-suggest-use-curl-p t))

(setq
 helm-scroll-amount 4
 helm-split-window-default-side 'other
 helm-split-window-in-side-p t
 helm-always-two-windows t
 helm-quick-update t
 helm-truncate-lines t
 helm-ff-search-library-in-sexp t
 helm-ff-file-name-history-use-recentf t
 helm-move-to-line-cycle-in-source t
 helm-recentf-fuzzy-match t
 helm-buffers-fuzzy-matching t
 helm-M-x-fuzzy-match t
 helm-locate-fuzzy-match t
 helm-semantic-fuzzy-match t
 helm-imenu-fuzzy-match t
 helm-apropos-fuzzy-match t
 helm-lisp-fuzzy-completion t
 helm-candidate-number-limit 50)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "<menu>") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(setq buffer-choose-default-function 'helm-mini)
(define-key ctl-x-map (kbd "C-f") 'helm-find-files)
(define-key helm-command-prefix "g" 'helm-do-grep)
(define-key helm-command-prefix "o" 'helm-occur)

(define-key 'help-command (kbd "C-f") 'helm-apropos)
(define-key 'help-command (kbd "r") 'helm-info-emacs)

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "M-r") 'helm-eshell-history)
              (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)))

(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

(helm-autoresize-mode 0)
(helm-mode 1)


;;;; Helm descbinds
;;;;
(helm-descbinds-mode)


;;;; Jump Char Settings
;;;;
(global-set-key (kbd "M-m") 'jump-char-forward)
(global-set-key (kbd "M-M") 'jump-char-backward)


;;;; Key Chord Settings
;;;;
(key-chord-mode 1)


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

;;; Intercept calling git in Eshell and parse some into Magit
;;
;; TODO:
;;     Add something for my shell aliases gr and hr
;;     Possibly get the output from 'start-file-process, then use that in the
;;     function to change directory
(setq eshell-magit->git-transformations
      (list
       '("log" . (lambda (args)
                   (if args (magit-log (cadr args)) (magit-log))
                   nil))
       '("diff" . (lambda (args)
                    (magit-diff (or (cadr args) "HEAD"))
                    nil))
       '("graph" . (lambda (args)
                     (if args (magit-log-long (cadr args)) (magit-log-long))
                     nil))
       '("show" . (lambda (args)
                    (magit-show-commit (cadr args))
                    nil))
       '("status" . (lambda (args)
                      (magit-status default-directory)
                      nil))))

(defun eshell-delegate-external (command args)
  "Call an external command "
  (eshell-wait-for-process (eshell-external-command command args)))

(defun eshell/git (&rest args)
  "Function to use some of magit abilities in eshell.

Checks if  the subcommand is  one of the  keys in the  assoc list
  `eshell-magit->git-transformations' if it isn't, we simply pass
  it  on to  the external  git command,  otherwise, do  something
  almost equivalent with `magit'"
  (let ((function-to-call
         (or (cdr (assoc (car args) eshell-magit->git-transformations))
             (lambda (args)
               (eshell-delegate-external (eshell-search-path "git") args)))))
    (funcall function-to-call args)))


;;;; Monky Settings
(key-chord-define-global ";h" 'monky-status)

;;;; Multiple Cursors Settings
;;;;
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;;;; Nameses Settins
;;;;
;; Don't enable nameses ido mode, as I use helm, and that works with completing-read
(setq nameses-ido-mode nil)


;;;; Paredit Settings
;;;;
(dolist (hook '(eval-expression-minibuffer-setup-hook
                emacs-lisp-mode-hook ielm-mode-hook lisp-mode-hook
                lisp-interaction-mode-hook scheme-mode-hook
                slime-repl-mode-hook))
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

(defun try-projectile-compile ()
  "Attempt to use projectile compile, otherwise just use compile."
  (interactive)
  (if (projectile-project-p)
      (call-interactively 'projectile-compile-project)
    (call-interactively 'compile)))

(global-set-key [remap compile] 'try-projectile-compile)


;;;; Smart Tab
;;;;
(global-smart-tab-mode 1)
(setq smart-tab-disabled-major-modes
      (append smart-tab-disabled-major-modes '(lisp-mode
                                               slime-repl-mode
                                               inferior-python-mode)))
(setq smart-tab-using-hippie-expand t)
(setq smart-tab-completion-functions-alist
      '((emacs-lisp-mode . helm-lisp-completion-at-point)
        (inferior-emacs-lisp-mode . helm-lisp-completion-at-point)))


;;;; Smart Window
;; Once loaded, overwrite with my own mappings
(define-key ctl-x-map "2"
  (lambda (numlines) (interactive "P")
    (if (not (consp numlines)) (call-interactively 'sw-below)
      (split-window-below))))

(define-key ctl-x-map "3"
  (lambda (numlines) (interactive "P")
    (if (not (consp numlines)) (call-interactively 'sw-right)
      (split-window-right))))


;;;; Slime Settings
;;;;
(add-to-list 'load-path "~/.emacs.d/packages/slime")
(require 'slime-autoloads)
(setq slime-contribs '(slime-fancy))
(setq slime-complete-symbol*-fancy t)

(define-key global-map (kbd "C-c s") 'slime-selector)

(add-hook 'slime-mode-hook
          (lambda ()
            (define-key slime-mode-map (kbd "C-c h") 'slime-highlight-edits-mode)
            (define-key slime-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)))

(add-hook 'slime-repl-mode-hook
          (lambda ()
            (define-key slime-repl-mode-map "(" 'self-insert-command)
            (define-key slime-repl-mode-map ")" 'self-insert-command)))

(setq slime-autodoc-use-multiline-p t)


;;;; Smartscan Settings
;;;;
(global-smartscan-mode 1)

(define-key smartscan-map (kbd "M-i") 'smartscan-symbol-go-forward)
(define-key smartscan-map (kbd "M-o") 'smartscan-symbol-go-backward)
(define-key smartscan-map (kbd "M-n") nil)
(define-key smartscan-map (kbd "M-p") nil)
(define-key smartscan-map (kbd "M-'") 'smartscan-symbol-replace)


;;;; Transpose Frame Settings
;;;;
(define-key ctl-x-5-map "t" 'transpose-frame)
(define-key ctl-x-5-map "v" 'flip-frame)
(define-key ctl-x-5-map "h" 'flop-frame)
(define-key ctl-x-5-map "r" 'rotate-frame-clockwise)


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


;;;; Vimrc Syntax Settings
;;;;
(add-to-list 'auto-mode-alist '("vimrc$" . vimrc-mode))
(add-to-list 'auto-mode-alist '("\\.vim$" . vimrc-mode))


;;;; Window Number Settings
;;;;
;; Colors
(setq window-number-active-background nil)
(setq window-number-active-foreground nil)
(setq window-number-inactive-foreground nil)
(setq window-number-inactive-background nil)

(window-number-mode 1)

;; Key bindings
(defun window-number-select-call (number)
  `(lambda ()
     (interactive)
     (window-number-select ,number)))

(dotimes (winnum 5)
  (define-key ctl-x-map
    (format "j%d" (1+ winnum))
    (window-number-select-call (1+ winnum))))


;;;; Xcscope Settings
;;;;
(add-hook 'c-mode-hook 'cscope-setup)
(add-hook 'c++-mode-hook 'cscope-setup)


;;;; Yasnippet Settings
;;;;
(yas-global-mode t)
(setq yas-also-auto-indent-first-line t
      yas-use-menu nil
      yas/prompt-functions '(yas/completing-prompt
                             yas/ido-prompt))

(define-key yas-keymap (kbd "C-;") 'yas-next-field-or-maybe-expand)
(define-key yas-keymap (kbd "C-:") 'yas-prev-field)
(define-key yas-minor-mode-map (kbd "C-;") 'yas-expand)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map [(tab)] nil)
