;;; Wrap Region Settings
;;;
;; Have to be before paredit so the exception works
(wrap-region-global-mode t)
(add-to-list 'wrap-region-except-modes 'paredit-mode)

;;; Ace jump Mode Settings
;;;
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

(defun ace-jump-char-mode ()
  (interactive)
  (ace-jump-mode 4))

(key-chord-define-global ";q" 'ace-jump-mode)
(key-chord-define-global ";j" 'ace-jump-char-mode)

(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

(evil-leader/set-key
  "mw" 'evil-ace-jump-word-mode
  "mc" 'evil-ace-jump-char-mode)


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


;;; Calendar Settings
;;;
(setq calendar-week-start-day 1)

(setq calendar-latitude 54)
(setq calendar-longitude 0)


;;; Cua Settings
;;;
(cua-mode t)
(setq cua-auto-tabify-rectangles nil)
(transient-mark-mode 1)
(cua-selection-mode t)


;;; Diary Settings
;;;
(setq diary-file "~/.emacs.d/diary")
(setq calendar-date-style 'european)


;;; Ediff Settings
;;;
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)


;;; Elscreen Settings
;;;
;; For introduction look at https://github.com/shosti/elscreen
(elscreen-start)


;;; Expand Region Settings
;;;
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-+") 'er/contract-region)


;;; Multiple Cursors Settings
;;;
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;;; Filesets Settings
;;;
(filesets-init)
(setq filesets-data `(("Emacs Config"
                       (:files "~/.emacs.d/TODO.txt"
                               "~/.emacs.d/init.el"
                               ,@(directory-files
                                  "~/.emacs.d/plugin_configurations" t
                                  "^.+\\.elc?$")))))

; Note can run any command on all files in a set once command is in the variable
; "filesets_commands"
; http://stackoverflow.com/questions/7071915/emacs-filesets-how-to-run-other-elisp-not-shell-commands


;;; Goto chg Settings
;;;
(global-set-key [(control ?.)] 'goto-last-change)
(global-set-key [(control ?,)] 'goto-last-change-reverse)


;;; List Buffer Settings
;;;
(global-set-key [remap list-buffers] 'ibuffer)


;;; Ido Settings
;;;
(ido-mode t)
(setq ido-enable-flex-matching t)

(add-hook 'ido-setup-hook
          (lambda ()
            ;; Go straight home
            (define-key ido-file-completion-map
              (kbd "~")
              (lambda ()
                (interactive)
                (if (looking-back "/")
                    (insert "~/")
                  (call-interactively 'self-insert-command))))))


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
(autoload 'enable-paredit-mode "paredit" "Turn on paredit." t)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

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


;;; Smex Settings
;;;
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
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


;;; Windmove Settings
;;;
(windmove-default-keybindings)
(setq windmove-wrap-around t)


;;; Winner Mode Settings
;;;
(winner-mode 1)


;;; Yasnippet Settings
;;;
(yas-global-mode t)