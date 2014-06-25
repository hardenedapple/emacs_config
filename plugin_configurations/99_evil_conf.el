;;; Evil-mode, I hate the emacs keybindings
;; Move <Return> and <Space> from the "motion" keymap to the normal one.
;; This allows other emacs plugins to remap them if they ever want to.
(evil-mode 0)

;;; Variables
(setq evil-want-C-u-scroll t)
(setq evil-find-skip-newlines t)
(setq evil-flash-delay 5)

;; Set the default mode for certain buffers
(dolist (mode-state-pair '((inferior-emacs-lisp-mode . emacs)
                           (nrepl-mode . insert)
                           (pylookup-mode . emacs)
                           (comint-mode . normal)
                           (shell-mode . insert)
                           (git-commit-mode . insert)
                           (git-rebase-mode . emacs)
                           (term-mode . emacs)
                           (help-mode . emacs)
                           (helm-grep-mode . emacs)
                           (grep-mode . emacs)
                           (bc-menu-mode . emacs)
                           (rdictcc-buffer-mode . emacs)
                           (dired-mode . emacs)
                           (wdired-mode . normal)
                           (magit-mode . emacs)
                           (info-mode . emacs)
                           (woman-mode . emacs)
                           (man-mode . emacs)))
  (evil-set-initial-state (car mode-state-pair) (cdr mode-state-pair)))



;;; Mappings
(define-key evil-motion-state-map " " nil)
(define-key evil-motion-state-map (kbd "RET") nil)

;; Normal mode mappings
(defun copy-to-end-of-line ()
  (interactive)
  (evil-yank (point) (point-at-eol)))

(define-key evil-normal-state-map "Y" 'copy-to-end-of-line)

;; swap the goto mark commands
(define-key evil-motion-state-map "'" 'evil-goto-mark)
(define-key evil-motion-state-map "`" 'evil-goto-mark-line)

;;; Insert mode mappings
(define-key evil-insert-state-map (kbd "C-u") (lambda () (interactive) (kill-line 0)))
(define-key evil-insert-state-map (kbd "C-x C-f") 'comint-dynamic-complete-filename)

;;; Remove keychords and wrap-region when in evil-mode
(add-hook 'evil-normal-state-entry-hook
          (lambda ()
            (key-chord-mode -1)
            (wrap-region-mode -1)))
(add-hook 'evil-emacs-state-entry-hook
          (lambda ()
            (key-chord-mode 1)
            (wrap-region-mode 1)))


(evil-define-motion evil-little-word (count)
  :type exclusive
  (let* ((case-fold-search nil)
         (count (if count count 1)))
    (while (> count 0)
      (forward-char)
      (search-forward-regexp "[_A-Z]\\|\\W" nil t)
      (backward-char)
      (decf count))))

(define-key evil-operator-state-map "lw" 'evil-little-word)

(defun whitespace-only-p (string)
  (equal "" (replace-regexp-in-string "[ \t\n]" "" string)))

(defadvice evil-delete (around evil-delete-yank activate)
  (if (whitespace-only-p (buffer-substring beg end))
      (ad-set-arg 3 ?_))
  ad-do-it)

;; All below will be moved into an "evil-unimpaired.el" plugin with extras.
;; Remember to add counts to this function.
(defun evil-unimpaired-newline-below (numlines)
  "Insert a new line below the point without moving it."
  (interactive "P")
  (save-excursion
    (if numlines
        (progn
          (end-of-line)
          (dotimes (nullvar numlines)
            (newline)))
      (progn
        (end-of-line)
        (newline)))))

(defun evil-unimpaired-newline-above (numlines)
  "Insert a new line below the point without moving it."
  (interactive "P")
  (let ((save-col (current-column)))
    (if numlines
        (progn
          (beginning-of-line)
          (dotimes (nullvar numlines)
            (newline)))
      (progn
        (beginning-of-line)
        (newline)))
    (move-to-column save-col)))


(define-key evil-normal-state-map "] " 'evil-unimpaired-newline-below)
(define-key evil-normal-state-map "[ " 'evil-unimpaired-newline-above)
(define-key evil-normal-state-map "]e" 'move-this-line-down)
(define-key evil-normal-state-map "[e" 'move-this-line-up)
(define-key evil-normal-state-map "]q" 'next-error)
(define-key evil-normal-state-map "[q" 'previous-error)
