;; Buffer move settings - These will always be valid
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)


;; Set some keys for evil
(evil-leader/set-key
 "H" 'buf-move-left
 "J" 'buf-move-down
 "K" 'buf-move-up
 "L" 'buf-move-right)
