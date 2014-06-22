;; Buffer move settings - These will always be valid
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
