;; Evil-mode, I hate the emacs keybindings
;; Move <Return> and <Space> from the "motion" keymap to the normal one.
;; This allows other emacs plugins to remap them if they ever want to.
(evil-mode 1)

(defun my-move-key (keymap-from keymap-to key)
  "Move key binding from one keymap to another, deleting from the old location."
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))

(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-normal-state-map evil-normal-state-map " ")

;; Normal mode mappings
(defun copy-to-end-of-line ()
  (interactive)
  (evil-yank (point) (point-at-eol)))

(define-key evil-normal-state-map "Y" 'copy-to-end-of-line)

;; Override the prefix key to allow the scrolling.
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)

;; Remove some of the scrolling commands in evil to leave the default emacs ones
(define-key evil-motion-state-map (kbd "'") 'evil-goto-mark)
(define-key evil-motion-state-map (kbd "`") 'evil-goto-mark-line)


;; All below will be moved into an "evil-unimpaired.el" plugin with extras.
;; Remember to add counts to this function.
(defun evil-unimpaired-newline-below ()
  "Insert a new line below the point without moving it."
  (interactive)
  (save-excursion
    (evil-insert-newline-below)
    (evil-force-normal-state)))

(defun evil-unimpaired-newline-above ()
  "Insert a new line below the point without moving it."
  (interactive)
  (save-excursion
    (evil-insert-newline-above)
    (evil-force-normal-state)))

;; (defun evil-unimpaired-swap-lines ()
;;   (interactive)
;;   (evil-delete-line)
;;   (evil-paste-after 1))

(define-key evil-normal-state-map "] " 'evil-unimpaired-newline-below)
(define-key evil-normal-state-map "[ " 'evil-unimpaired-newline-above)

;; (define-key evil-normal-state-map "]e" 'evil-unimpaired-swap-lines)
