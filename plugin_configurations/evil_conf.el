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
