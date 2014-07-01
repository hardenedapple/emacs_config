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


;;; Eldoc Settings
;;;
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)


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
; [[http://stackoverflow.com/questions/7071915/emacs-filesets-how-to-run-other-elisp-not-shell-commands][source]]


;;; List Buffer Settings
;;;
(global-set-key [remap list-buffers] 'ibuffer)


;;; Ido Settings
;;;
(ido-everywhere 1)
(setq ido-save-directory-list-file "~/.emacs.d/ido.last")
(ido-mode t)
(setq ido-everywhere t
      ido-enable-flex-matching t)

;;; Choose extensions to inore
;; (setq completion-ignored-extensions
;;       (cons ".jpg" completion-ignored-extensions))


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


;;; Org Mode Settings
;;;
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c L") 'org-insert-link-global)
(global-set-key (kbd "C-c o") 'org-open-at-point-global)
;; (setq org-drawers (cons "THOUGHTS" org-drawers))
(setq org-hide-block-startup t)


;;; Uniquify Settings
;;;
(setq uniquify-buffer-name-style 'post-forward)


;;; Windmove Settings
;;;
(windmove-default-keybindings)
(setq windmove-wrap-around t)


;;; Winner Mode Settings
;;;
(winner-mode 1)
