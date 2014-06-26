;;; Window selection by number
;;; Don't like the colors
(setq window-number-active-background nil)
(setq window-number-active-foreground nil)
(setq window-number-inactive-foreground nil)
(setq window-number-inactive-background nil)

(window-number-mode 1)

;;; Evil key bindings
(defun window-number-select-call (number)
  `(lambda ()
     (interactive)
     (window-number-select ,number)))

(dotimes (winnum 6)
  (define-key evil-normal-state-map
    (format "g%d" winnum)
    (window-number-select-call winnum)))
