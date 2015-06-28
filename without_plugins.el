(require 'help-mode)
(require 'ibuffer)
;;;; Load Dvorak keybindings
(dolist (conf-file (list
                    "~/.emacs.d/config/01_general_settings.el"
                    "~/.emacs.d/config/02_window_commands.el"
                    "~/.emacs.d/config/03_splice_windows.el"
                    "~/.emacs.d/config/69_filetype_specific_settings.el"
                    "~/.emacs.d/config/79_built_in_packages.el"
                    "~/.emacs.d/config/99_dvorak_keybindings.el"))
  (load conf-file))

;; Color with an existing default emacs theme.
(load-theme 'wombat)

;;; New window commands
;;;
(defun run-command-split-window (direction)
  "Return a function that calls `split-window' in DIRECTION, then
runs a user defined command."
  `(lambda (command)
     "Split the current window in ,direction then run COMMAND in
that window."
     (interactive "C")
     (select-window (split-window (selected-window) nil ',direction))
     (call-interactively command)))

(define-key ctl-x-map "2"
  (lambda (arg) (interactive "P")
    "

Without argument, call `split-window-below'

With the universal argument, split the window below, and run a
command given by the user in that window.

"
    (if arg (call-interactively (run-command-split-window 'below))
      (select-window (split-window-below)))))

(define-key ctl-x-map "3"
  (lambda (arg) (interactive "P")
    "

Without argument, call `split-window-right'

With the universal argument, split the window to the right, and
run a command given by the user in that window.

"
    (if arg (call-interactively (run-command-split-window 'right))
      (select-window (split-window-right)))))
