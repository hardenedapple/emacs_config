;; Load transpose-frames and set keybindings
(define-key ctl-x-4-map (kbd "t") 'transpose-frame)
(define-key ctl-x-4-map (kbd "v") 'flip-frame)
(define-key ctl-x-4-map (kbd "h") 'flop-frame)
(define-key ctl-x-4-map (kbd "r") 'rotate-frame-clockwise)
