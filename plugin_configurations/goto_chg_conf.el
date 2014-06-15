;; Goto chg - these mappings are for emacs mode (i.e. not evil)
(if (not (boundp 'evil-normal-state-map))
    (global-set-key [(control ?.)] 'goto-last-change)
  (global-set-key [(control ?,)] 'goto-last-change-reverse))

