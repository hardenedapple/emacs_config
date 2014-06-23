;;; Mappings defined elsewhere
;;; Motion  - ace-jump-mode and ace-jump-mode with a prefix ;q and ;j respectivly
;;; Magit   - Open up magit-status  ;g
(key-chord-mode 1)

(key-chord-define-global ".f" 'ido-find-file)
(key-chord-define-global ";b" 'ido-switch-buffer)
(key-chord-define-global ";s" 'save-buffer)
(key-chord-define-global "gt" 'goto-line)
;;; delete-window
;;; delete-other-windows-vertically
