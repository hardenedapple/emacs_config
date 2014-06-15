;; Buffer move settings
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

(if (boundp 'evil-normal-state-map)
    (evil-leader/set-key
     "H" 'buf-move-left
     "J" 'buf-move-down
     "K" 'buf-move-up
     "L" 'buf-move-right))
