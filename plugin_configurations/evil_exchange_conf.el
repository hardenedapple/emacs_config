;; Evil-exchange
;; (use gx<motion> to select a range, once selected two ranges they are swapped
(evil-exchange-install)

;; Change the insert mode mappings to reflect the fact I'm in emacs
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)
(define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
;; (define-key evil-insert-state-map "  " 'execute-extended-command)
