;;; Ace jump configurations
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

(defun ace-jump-char-mode ()
  (interactive)
  (ace-jump-mode 4))

(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

(evil-leader/set-key
  "aw" 'ace-jump-mode
  "ac" 'ace-jump-char-mode)

