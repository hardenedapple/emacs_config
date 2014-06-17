;; Evil args (motion of arguments)
;; Text objects
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

;; bind evil-forward/backward-args
(evil-leader/set-key
  "ah" 'evil-backward-arg
  "al" 'evil-forward-arg
  "ak" 'evil-jump-out-args)
