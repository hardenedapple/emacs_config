;;; Evil Leader Settings
;;;
; Has to be initialised before evil-mode so is available in *scratch*,
; *Messages* etc (i.e. in initial buffers)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "bs" 'ibuffer)


;;; Evil Mode Settings
;;;
(evil-mode 0)

;; Variables
(setq evil-want-C-u-scroll t)
(setq evil-want-C-i-jump t)
(setq evil-cross-lines t)
(setq evil-find-skip-newlines t)
(setq evil-flash-delay 5)
(setq evil-want-change-word-to-end nil)

;; Set the default mode for certain buffers
(dolist (mode-state-pair '((git-commit-mode . insert)
                           (git-rebase-mode . emacs)
                           (helm-grep-mode . emacs)))
  (evil-set-initial-state (car mode-state-pair) (cdr mode-state-pair)))

;; Mappings
(define-key evil-motion-state-map " " nil)
(define-key evil-motion-state-map (kbd "RET") nil)

; Normal mode mappings
(defun copy-to-end-of-line ()
  (interactive)
  (evil-yank (point) (point-at-eol)))

(define-key evil-normal-state-map "Y" 'copy-to-end-of-line)

(define-key evil-motion-state-map "'" 'evil-goto-mark)
(define-key evil-motion-state-map "`" 'evil-goto-mark-line)

;; C-6 go to previous buffer
(defun switch-to-last-seen-buffer ()
  "Switch to last open buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(define-key evil-motion-state-map (kbd "C-6") 'switch-to-last-seen-buffer)


;; indent keeping selection
(defun indent-keeping-selection (beg end)
  "Using > or < in visual mode indents and keeps selection at the same time"
  (interactive "r")
  (let (deactivate-mark)
    (evil-shift-right beg end)))

(defun dedent-keeping-selection (beg end)
  "Using > or < in visual mode indents and keeps selection at the same time"
  (interactive "r")
  (let (deactivate-mark)
    (evil-shift-left beg end)))

(define-key evil-visual-state-map ">" 'indent-keeping-selection)
(define-key evil-visual-state-map "<" 'dedent-keeping-selection)

; Insert mode mappings
(define-key evil-insert-state-map (kbd "C-u") (lambda () (interactive) (kill-line 0)))
(define-key evil-insert-state-map (kbd "C-x C-f") 'comint-dynamic-complete-filename)

; Remove keychords and wrap-region when in evil-mode
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


;;; Evil Unimpaired Settings
;;;
;; maybe evil-define-command for these
(defun evil-unimpaired-newline-below (numlines)
  "Insert a new line below the point without moving it."
  (interactive "p")
  (save-excursion
    (progn
      (end-of-line)
      (dotimes (nullvar numlines)
        (newline)))))

(defun evil-unimpaired-newline-above (numlines)
  "Insert a new line below the point without moving it."
  (interactive "p")
  (let ((save-col (current-column)))
    (progn
      (beginning-of-line)
      (dotimes (nullvar numlines)
        (newline)))
    (move-to-column save-col)))


(define-key evil-normal-state-map "] " 'evil-unimpaired-newline-below)
(define-key evil-normal-state-map "[ " 'evil-unimpaired-newline-above)
(define-key evil-normal-state-map "[b" 'next-buffer)
(define-key evil-normal-state-map "]b" 'previous-buffer)
(define-key evil-normal-state-map "]e" 'move-this-line-down)
(define-key evil-normal-state-map "[e" 'move-this-line-up)
(define-key evil-normal-state-map "]q" 'next-error)
(define-key evil-normal-state-map "[q" 'previous-error)
(define-key evil-normal-state-map "[Q" (lambda () (interactive) (next-error 1 t)))
; Make sure counts work on the error cycling
; Add [Q and ]Q


;;; Evil Args Settings
;;;
;; Text objects
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

;; Leader mappings
(evil-leader/set-key
  "ah" 'evil-backward-arg
  "al" 'evil-forward-arg
  "ak" 'evil-jump-out-args)


;;; Evil Exchange Settings
;;;
; (use gx<motion> to select a range, once selected two ranges they are swapped
(evil-exchange-install)


;;; Evil Surround Settings
;;;
(global-evil-surround-mode 1)


;;; Window Number Settings
;;;
; Colors
(setq window-number-active-background nil)
(setq window-number-active-foreground nil)
(setq window-number-inactive-foreground nil)
(setq window-number-inactive-background nil)

(window-number-mode 1)

; Key bindings
(defun window-number-select-call (number)
  `(lambda ()
     (interactive)
     (window-number-select ,number)))

(dotimes (winnum 5)
  (define-key evil-motion-state-map
    (format "g%d" (1+ winnum))
    (window-number-select-call (1+ winnum))))
