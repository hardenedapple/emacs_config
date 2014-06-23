;; ido mode - more options for selecting buffers/files
(ido-mode t)
(setq ido-enable-flex-matching t)

;;; Go straight to home
(add-hook 'ido-setup-hook
 (lambda ()
   ;; Go straight home
   (define-key ido-file-completion-map
     (kbd "~")
     (lambda ()
       (interactive)
       (if (looking-back "/")
           (insert "~/")
         (call-interactively 'self-insert-command))))))
