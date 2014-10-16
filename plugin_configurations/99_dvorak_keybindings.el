;;;; Remaps for Dvorak keyboard
;;;;
;;;; NOTE:
;;;;      While there are a lot of different modes that have mapped 'n' and 'p'
;;;;      to up and down in some way, I'm not going to change these for two
;;;;      reasons.
;;;;
;;;;      (1) When the mapping is on unmodified 'n' and 'p', the problem I'm
;;;;          trying to avoid of switching which hand is holding down the
;;;;          modifier key doesn't apply
;;;;      (2) It would be a hell of a lot of work to find every occurance of
;;;;          this particular mapping in every mode -- not worth the mapping
;;;;          consistency of having them remapped.
;;;;

(global-set-key (kbd "C-S-h") (lambda (numtimes) (interactive "p")
                                (ignore-errors (previous-line (* numtimes 5)))))

;; Can't use keyboard-translate here as C-' is not a single ascii character.
(global-set-key (kbd "C-'") ctl-x-map)
;; Make the switch between "h" and "p" more thorough
(global-set-key (kbd "C-M-p") 'mark-defun)
(global-set-key (kbd "C-M-h") 'backward-list)
(global-set-key (kbd "M-g M-h") 'previous-error)
(global-set-key (kbd "M-g h") 'previous-error)
(global-set-key (kbd "M-p") 'mark-paragraph)

;;; Using M-f and M-b for word motion is a pain, swap with M-a and M-e
(global-set-key (kbd "M-a") 'subword-backward)
(global-set-key (kbd "M-e") 'subword-forward)
(global-set-key (kbd "M-b") 'backward-sentence)
(global-set-key (kbd "M-f") 'forward-sentence)


(defvar dvorak-keyswaps
  '((?\C-h . ?\C-p)
    (?\C-p . ?\C-h)
    (?\C-z . ?\C-x)
    (?\C-x . ?\C-z)
    (?\C-j . ?\C-c)
    (?\C-w . ?\C-c)
    (?\C-c . ?\C-w)))

(defun apply-my-keyswaps ()
  (dolist (key-pair dvorak-keyswaps)
    (keyboard-translate (car key-pair) (cdr key-pair))))

(apply-my-keyswaps)

(add-hook 'after-make-frame-functions
          (lambda (f) (with-selected-frame f
                        (apply-my-keyswaps))))

;;;; Comint Settings
;;;
(define-key comint-mode-map (kbd "M-h") 'comint-previous-input)
(define-key comint-mode-map (kbd "M-p") nil)

;;;; Org mode Dvorak settings
;;;
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "M-a") nil)
            (define-key org-mode-map (kbd "M-e") nil)
            (define-key org-mode-map (kbd "M-f") 'org-forward-sentence)
            (define-key org-mode-map (kbd "M-b") 'org-backward-sentence)))



;;;; Make the Paredit C-M-p keybinding switch to C-M-h
(define-key paredit-mode-map (kbd "C-M-h") 'paredit-backward-down)
(define-key paredit-mode-map (kbd "C-M-p") nil)


;;;; Slime mappings
(add-hook 'slime-mode-hook
          (lambda ()
            (define-key slime-mode-map (kbd "M-p") nil)
            (define-key slime-mode-map (kbd "M-h") 'slime-previous-note)))

(add-hook 'slime-repl-mode-hook
          (lambda ()
            (define-key slime-repl-mode-map (kbd "M-h") 'slime-repl-previous-input)
            (define-key slime-repl-mode-map (kbd "M-p") nil)))
