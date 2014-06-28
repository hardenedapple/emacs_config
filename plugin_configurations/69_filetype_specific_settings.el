;;; Latex Settings
;;;
(add-hook 'latex-mode-hook
          '(lambda ()
             (set-fill-column 125))) ; usually only write latex on large screens
