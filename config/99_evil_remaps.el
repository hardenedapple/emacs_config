;;;; Remove the swap of C-c and C-w done in 99_dvorak_keybindings.el
;;; Do this as C-w is used more than C-c in vim, so moving it away from the
;;; Ctrl key isn't as good an idea as otherwise.
;;; As removing the code from 99_dvorak_keybindings.el means that
;;; without_plugins.el also loses the maps (despite that initialisation file not
;;; using evil) I'm removing the maps in a separate file, only loaded when the
;;; rest of evil is loaded as well.

(keyboard-translate ?\C-c nil)
(keyboard-translate ?\C-w nil)
