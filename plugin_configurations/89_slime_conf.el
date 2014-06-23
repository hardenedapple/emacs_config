;; Load slime, the extra contrib's and put some settings up.
(add-to-list 'load-path "~/.emacs.d/packages/slime")
(require 'slime-autoloads)
(setq slime-contribs '(slime-fancy slime-highlight-edits))

(define-key global-map (kbd "C-c s") 'slime-selector)
(add-hook 'slime-mode-hook
          (lambda ()
            (slime-highlight-edits-mode nil)
            (define-key slime-mode-map (kbd "C-c h") 'slime-highlight-edits-mode)))
(setq slime-autodoc-use-multiline-p t)
