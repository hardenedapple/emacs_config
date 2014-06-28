
;;; Calendar Settings
;;;
(setq calendar-week-start-day 1)

(setq calendar-latitude 54)
(setq calendar-longitude 0)


;;; Cua Settings
;;;
(cua-mode t)
(setq cua-auto-tabify-rectangles nil)
(transient-mark-mode 1)
(cua-selection-mode t)


;;; Diary Settings
;;;
(setq diary-file "~/.emacs.d/diary")
(setq calendar-date-style 'european)


;;; Ediff Settings
;;;
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)


;;; Filesets Settings
;;;
(filesets-init)
(setq filesets-data `(("Emacs Config"
                       (:files "~/.emacs.d/TODO.txt"
                               "~/.emacs.d/init.el"
                               ,@(directory-files
                                  "~/.emacs.d/plugin_configurations" t
                                  "^.+\\.elc?$")))))

; Note can run any command on all files in a set once command is in the variable
; "filesets_commands"
; http://stackoverflow.com/questions/7071915/emacs-filesets-how-to-run-other-elisp-not-shell-commands


;;; List Buffer Settings
;;;
(global-set-key [remap list-buffers] 'ibuffer)


;;; Ido Settings
;;;
(ido-mode t)
(setq ido-enable-flex-matching t)

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


;;; Windmove Settings
;;;
(windmove-default-keybindings)
(setq windmove-wrap-around t)


;;; Winner Mode Settings
;;;
(winner-mode 1)
