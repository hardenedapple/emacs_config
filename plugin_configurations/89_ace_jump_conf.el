;;; Ace jump configurations
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

(defun ace-jump-char-mode ()
  (interactive)
  (ace-jump-mode 4))
(key-chord-define-global ";q" 'ace-jump-mode)
(key-chord-define-global ";j" 'ace-jump-char-mode)

(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

(evil-leader/set-key
  "mw" 'evil-ace-jump-mode
  "mc" 'evil-ace-jump-char-mode)
