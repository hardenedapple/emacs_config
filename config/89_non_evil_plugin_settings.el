;;;; My packages
;;;;
(define-key ctl-x-4-map "s" 'splice-window-upwards)


;;;; Colour theme
;;;;
(load-theme 'monokai)

;;;; Wrap Region Settings
;;;;
;;; Have to be before paredit so the exception works
(wrap-region-global-mode t)
(add-to-list 'wrap-region-except-modes 'paredit-mode 'tex-mode)


;;;; Ace jump Mode Settings
;;;;
(global-set-key (kbd "M-g M-s") 'ace-jump-word-mode)
(global-set-key (kbd "M-g s") 'ace-jump-word-mode)
(setq ace-jump-mode-scope 'window)


;;;; Buffer Move Settings
;;;;
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)


;;;; C-ELDOC-MODE
;;;;
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)


;;;; Elisp Slime Nav Settings
;;;;
;; Elisp find thing at point in other window.
(defun elisp-slime-nav-find-thing-at-point-other-window ()
  "Go to definition of `symbol-at-point' in other window."
  (interactive)
  (let ((current-symbol (thing-at-point 'symbol t)))
    (run-function-other-window #'elisp-slime-nav-find-elisp-thing-at-point nil
                               current-symbol)))

(define-key elisp-slime-nav-mode-map (kbd "C-x 4 M-.")
  'elisp-slime-nav-find-thing-at-point-other-window)

;; Left click finds elisp thing
(define-key elisp-slime-nav-mode-map [mouse-1]
  (mouse-function-on-symbol
   (elisp-slime-nav-find-elisp-thing-at-point current-symbol)))

(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))


;;;; Expand Region Settings
;;;;
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-+") 'er/contract-region)


;;;; Goto chg Settings
;;;;
(global-set-key (kbd "C-.") 'goto-last-change)
(global-set-key (kbd "C-,") 'goto-last-change-reverse)


;;;; Ido at point Settings
;;;;
(ido-at-point-mode)


;;;; Ido Ubiquitous Settings
;;;;
(ido-ubiquitous-mode)


;;;; Ido Vertical Mode
;;;;
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)


;;;; Jump Char Settings
;;;;
(global-set-key (kbd "M-m") 'jump-char-forward)
(global-set-key (kbd "M-M") 'jump-char-backward)


;;;; List Registers Settings
;;;;
(define-key ctl-x-r-map "v" 'list-register)


;;;; Magit Settings
;;;;
(setq magit-repo-dirs '("~/.emacs.d" "~/share/repos/useful-files"))
(defun magit-stage-this-file ()
  "Stage file the current buffer is visiting."
  (interactive)
  (magit-stage-item (buffer-file-name)))

(global-set-key (kbd "C-c g") 'magit-status)
;; git commit mode usually starts flyspell
(setq git-commit-mode-hook '(turn-on-auto-fill))
(setq magit-last-seen-setup-instructions "1.4.0")

;;; Intercept calling git in Eshell and parse some into Magit
;;
;; NOTE:
;;     Some of these functions are taken directly from magit.el, while changing
;;     the SWITCH-FUNC argument to `magit-mode-setup' macro so that always use
;;     the current window.
;;
;; TODO:
;;     Add something for my shell aliases gr and hr
;;     Possibly get the output from 'start-file-process, then use that in the
;;     function to change directory
(defun eshell--magit-log-function (range &optional type arguments)
  "Runs `magit-log', but with optional arguments.

Just a reworking of `magit-log' and `magit-log-long' for my
`eshell' customisation ease."
  (let ((range (cadr range))
        (type (if type type 'oneline)))
    (cond ((not range) (setq range "HEAD"))
          ;; Forward compatibility kludge.
          ((listp range) (setq range (car range))))
    (magit-mode-setup magit-log-buffer-name
                      #'switch-to-buffer
                      #'magit-log-mode
                      #'magit-refresh-log-buffer
                      type range arguments)))

(setq eshell-magit->git-transformations
      (list
       '("log" . eshell--magit-log-function)
       '("diff" . (lambda (args)
                    (magit-mode-setup magit-diff-buffer-name
                                      #'switch-to-buffer
                                      #'magit-diff-mode
                                      #'magit-refresh-diff-buffer
                                      (or (cadr args) "HEAD") nil nil)
                    nil))
       '("status" . (lambda (args)
                      (magit-status default-directory
                                    #'switch-to-buffer)))
       '("graph" . (lambda (range)
                     (eshell--magit-log-function range 'long (list "--graph"))))))

(defun eshell-delegate-external (command args)
  "Call external command "
  (eshell-wait-for-process (eshell-external-command command args)))

(defun eshell/git (&rest args)
  "Function to use some of `magit' abilities in `eshell'.

Checks if the subcommand is one of the keys in the assoc list
`eshell-magit->git-transformations' if it isn't, we simply pass
it on to the external git command, otherwise, do something almost
equivalent with `magit'"
  (let ((function-to-call
         (or (cdr (assoc (car args) eshell-magit->git-transformations))
             (lambda (args)
               (eshell-delegate-external (eshell-search-path "git") args)))))
    (funcall function-to-call args)
    nil))

(defun eshell/gr (&rest args)
  "Go to the current repositorys' root dir.

Calls `eshell/cd' to the value of `magit-get-top-dir'"
  (let ((git-root (magit-get-top-dir)))
    (when git-root
      (eshell/cd git-root))))


;;;; Monky Settings
(global-set-key (kbd "C-c h") 'monky-status)

(defun eshell/hr (&rest args)
  "Go to the current mercurial repositorys' root dir.

Calls `eshell/cd' to the value of `magit-get-top-dir'"
  (let ((hg-root (monky-get-root-dir)))
    (when hg-root
      (eshell/cd hg-root))))


;;;; Multiple Cursors Settings
;;;;
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;;;; Nameses Settins
;;;;
(setq nameses-ido-mode t)


;;;; Paredit Settings
;;;;
(dolist (hook '(eval-expression-minibuffer-setup-hook
                emacs-lisp-mode-hook ielm-mode-hook lisp-mode-hook
                lisp-interaction-mode-hook scheme-mode-hook
                slime-repl-mode-hook))
  (add-hook hook #'enable-paredit-mode))

(defun paredit--is-at-end-of-list ()
  (and (looking-back ")\\|\\]")
       (not (nth 3 (syntax-ppss))) ;; inside string
       (not (nth 4 (syntax-ppss))))) ;; inside comment

(defun paredit-duplicate-closest-list ()
  "Duplicate sexp the point is at."
  (interactive)
  ;; Go to end of current sexp.
  ;; Not sure when the loop will be needed, but better safe than sorry
  (while (not (paredit--is-at-end-of-list))
    (paredit-forward-up))
  (let* ((end (point))
         (dup-sexp (buffer-substring (progn (paredit-backward) (point)) end)))
    ;; Insert copy, make sure we're on a new line, put us at the end of both
    ;; sexps
    (insert dup-sexp)
    (paredit-newline)
    (paredit-forward)))

;;; Add keybinding C-c d to run paredit-duplicate-closest-sexp in paredit
(define-key paredit-mode-map (kbd "C-c d") 'paredit-duplicate-closest-list)
(define-key paredit-mode-map (kbd "C-j") 'nil)
;; Keep M-s search commands by moving PAREDIT-SPLICE-SEXP, so the keymaps join
(define-key paredit-mode-map (kbd "M-s") nil)
(define-key paredit-mode-map (kbd "M-s M-s") 'paredit-splice-sexp)
(define-key paredit-mode-map (kbd "M-s s") 'paredit-splice-sexp)
(define-key paredit-mode-map (kbd "<delete>") nil)

;; Paredit M-r overrides M-r in comint
;; Want comint-history-isearch-backward-regexp, so remap it to C-q
(with-eval-after-load 'ielm
    (define-key ielm-map (kbd "C-q") 'comint-history-isearch-backward-regexp))

;;; paredit with eldoc
(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

(defun paredit-downsexp-newline-and-parenthesis (&optional arg)
  "Do same as `downlist-newline-and-parentheses', but using
paredit functions on the assumption they'll be more robust."
  (interactive "^p")
  (paredit-forward-up arg)
  (paredit-newline)
  (paredit-open-round))

(define-key paredit-mode-map (kbd "<C-return>")
  'paredit-downsexp-newline-and-parenthesis)


;; TRY-EXPAND-LINE and TRY-EXPAND-LIST add an extra ")" character when in
;; paredit-mode, fix this with an advice (as suggested on the HIPPIE-EXPAND
;; emacs wiki page)
(defadvice he-substitute-string (after he-paredit-fix activate)
  "Remove extra paren when expanding line in paredit"
  (if (and paredit-mode (equal (substring str -1) ")"))
      (backward-delete-char 1)))


;;;; Projectile Settings
;;;;
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'ido)
(setq projectile-use-git-grep t)
(setq projectile-remember-window-configs t)


;;;; Smart Tab
;;;;
(global-smart-tab-mode 1)
(setq smart-tab-disabled-major-modes
      (append smart-tab-disabled-major-modes '(lisp-mode
                                               slime-repl-mode
                                               inferior-python-mode)))
(setq smart-tab-using-hippie-expand t)
(setq smart-tab-completion-functions-alist
      '((emacs-lisp-mode . completion-at-point)
        (inferior-emacs-lisp-mode . completion-at-point)))


;;;; Smart Window
;; Once loaded, overwrite with my own mappings
(defun run-command-split-window (direction)
  "Return a function that calls `split-window' in DIRECTION, then
runs a user defined command."
  `(lambda (command)
     "Split the current window in ,direction then run COMMAND in
that window."
     (interactive "C")
     (select-window (split-window (selected-window) nil ',direction))
     (call-interactively command)))

(define-key ctl-x-map "2"
  (lambda (arg) (interactive "P")
    "

Do one of three actions depending on ARG.
Calling directly, split the window below, and switch to a buffer
in that window

With the universal argument, call `split-window-below'

With the universal argument twice, split the window to the below,
and run a command given by the user in that window.

"
    (cond
     ((null arg) (call-interactively 'sw-below))
     ((= (car arg) 4) (select-window (split-window-below)))
     ((> (car arg) 4)
      (call-interactively (run-command-split-window 'below))))))

(define-key ctl-x-map "3"
  (lambda (arg) (interactive "P")
    "

Do one of three actions depending on ARG.
Calling directly, split the window right, and switch to a buffer
in that window

With the universal argument, call `split-window-right'

With the universal argument twice, split the window to the right,
and run a command given by the user in that window.

"
    (cond
     ((null arg) (call-interactively 'sw-right))
     ((= (car arg) 4) (select-window (split-window-right)))
     ((> (car arg) 4)
      (call-interactively (run-command-split-window 'right))))))


;;;; Smex Settings
;;;;
(setq smex-history-length 20)
(setq smex-save-file "~/.emacs.d/smex-items")
(smex-initialize)
;; Want to remap the keys I'm currently using for `execute-extended-command',
;; but leave one just in case everything goes wrong (e.g. my keymappings have
;; all broken somehow, and `smex' isn't opening, I want to be able to run some
;; kind of command, so leave <menu> as `execute-extended-command' for this
;; hypothetical scenario)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "<delete>") 'smex)
;;(global-set-key (kbd "<menu>") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


;;;; Slime Settings
;;;;
(add-to-list 'load-path "~/.emacs.d/packages/slime")
(require 'slime-autoloads)
(setq slime-contribs '(slime-fancy slime-asdf))
(setq slime-complete-symbol*-fancy t)
;; note with this set up of info, also need to go inte the directory and run
;; install-info --dir=dir slime.info
(add-to-list 'Info-directory-list "~/.emacs.d/packages/slime/doc")

(global-set-key (kbd "C-c s") 'slime-selector)

(with-eval-after-load 'slime
  ;; Make TAB do my completion, and since I don't have any way to
  ;; press C-j, remap `slime-eval-last-expression-in-repl' to C-c C-i
  ;; (C-i for "interpreter") instead of `slime-complete-symbol', which
  ;; I'll now use TAB for.
  (define-key slime-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)
  (define-key slime-mode-map (kbd "C-c C-i") 'slime-eval-last-expression-in-repl)
  ;; Remove the M-? mapping for `slime-edit-uses', can still use M-_,
  ;; but `paredit-convolute-sexp' is now unshadowed.
  (define-key slime-mode-map (kbd "M-?") nil)
  (define-key slime-mode-map [mouse-1]
    (mouse-function-on-symbol (slime-edit-definition current-symbol)))
  (define-key slime-mode-map [mouse-3]
    (lambda (event) (interactive "e") (slime-pop-find-definition-stack))))

(with-eval-after-load 'slime-repl
  (define-key slime-repl-mode-map "(" 'self-insert-command)
  (define-key slime-repl-mode-map ")" 'self-insert-command))

(setq slime-autodoc-use-multiline-p t)


;;;; Transpose Frame Settings
;;;;
(define-key ctl-x-5-map "t" 'transpose-frame)
(define-key ctl-x-5-map "v" 'flip-frame)
(define-key ctl-x-5-map "h" 'flop-frame)
(define-key ctl-x-5-map "r" 'rotate-frame-clockwise)


;;;; Transpose Mark Settings
;;;;
(global-set-key (kbd "C-c t") 'transpose-mark)


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
    (format "p%d" (1+ winnum))
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
