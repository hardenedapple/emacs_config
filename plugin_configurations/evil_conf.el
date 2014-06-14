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

;; Remove some of the scrolling commands in evil to leave the default emacs ones
(define-key evil-motion-state-map (kbd "C-e") nil)
(define-key evil-motion-state-map (kbd "C-y") nil)

;; Override the prefix key to allow the scrolling.
;; (define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)

;; Change the insert mode mappings to reflect the fact I'm in emacs
(define-key evil-insert-state-map (kbd "C-k") nil)
(define-key evil-insert-state-map (kbd "C-a") nil)
(define-key evil-insert-state-map (kbd "C-e") nil)
(define-key evil-insert-state-map (kbd "C-y") nil)
;; (define-key evil-insert-state-map "  " 'execute-extended-command)

(define-key evil-insert-state-map (kbd "RET") nil)
