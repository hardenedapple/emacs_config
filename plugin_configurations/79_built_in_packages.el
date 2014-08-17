;;;; Calendar Settings
;;;;
(setq calendar-week-start-day 1)

(setq calendar-latitude 54)
(setq calendar-longitude 0)


;;;; Cua Settings
;;;;
(cua-mode t)
(setq cua-auto-tabify-rectangles nil)
(transient-mark-mode 1)
(cua-selection-mode t)


;;;; Diary Settings
;;;;
(setq diary-file "~/.emacs.d/diary")
(setq calendar-date-style 'european)


;;;; Ediff Settings
;;;;
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)


;;;; Eldoc Settings
;;;;
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)


;;;; Filesets Settings
;;;;
(filesets-init)
(setq filesets-data `(("Emacs Config"
                       (:files "~/.emacs.d/TODO.txt"
                               "~/.emacs.d/init.el"
                               ,@(directory-files
                                  "~/.emacs.d/plugin_configurations" t
                                  "^.+\\.elc?$")))))

;; Note can run any command on all files in a set once command is in the variable
;; "filesets_commands"
;; [[http://stackoverflow.com/questions/7071915/emacs-filesets-how-to-run-other-elisp-not-shell-commands][look here]]


;;;; List Buffer Settings
;;;;
(global-set-key [remap list-buffers] 'ibuffer)


;;;; Org Mode Settings
;;;;
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c L") 'org-insert-link-global)
(global-set-key (kbd "C-c o") 'org-open-at-point-global)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c b") 'org-iswitchb)
;;; (setq org-drawers (cons "THOUGHTS" org-drawers))
(setq org-directory "~/TODO"
      org-hide-block-startup t
      org-startup-indented t
      org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t
      org-completion-use-ido t
      org-default-notes-file (concat org-directory "/notes.org")
      org-agenda-files (list "~/TODO/Today.org"
                             "~/TODO/Soon.org"
                             "~/TODO/Someday.org"))

(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "C-'") nil)
            (define-key org-mode-map (kbd "C-;") 'org-cycle-agenda-files)))


;;;; Outline Minor Mode Settings
;;;;
;;; Outline-minor-mode key map
(define-prefix-command 'cm-map nil "Outline-")
;;; HIDE
(define-key cm-map "q" 'hide-sublevels)
(define-key cm-map "t" 'hide-body)
(define-key cm-map "o" 'hide-other)
(define-key cm-map "c" 'hide-entry)
(define-key cm-map "l" 'hide-leaves)
(define-key cm-map "d" 'hide-subtree)
;;; SHOW
(define-key cm-map "a" 'show-all)
(define-key cm-map "e" 'show-entry)
(define-key cm-map "i" 'show-children)
(define-key cm-map "k" 'show-branches)
(define-key cm-map "s" 'show-subtree)
;;; MOVE
(define-key cm-map "u" 'outline-up-heading)
(define-key cm-map "n" 'outline-next-visible-heading)
(define-key cm-map "p" 'outline-previous-visible-heading)
(define-key cm-map "f" 'outline-forward-same-level)
(define-key cm-map "b" 'outline-backward-same-level)
(define-key cm-map "v" 'outline-move-subtree-down)
(define-key cm-map "^" 'outline-move-subtree-up)
;;(global-set-key (kbd "M-o") cm-map)
(defun outline-minor-mode-with-hook (arg)
  (interactive "P")
  (if arg
    (outline-minor-mode arg)
    (outline-minor-mode))
  (when outline-minor-mode
      (define-key outline-minor-mode-map (kbd "M-o") cm-map)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (outline-minor-mode-with-hook 1)))


;;;; Hs-minor-mode Settings
;;;;
(add-hook 'c-mode-hook
          (lambda ()
            (hs-minor-mode 1)))

(define-prefix-command 'my-hs-mappings nil "hs-")
(define-key my-hs-mappings "q" 'hs-hide-all)
(define-key my-hs-mappings "d" 'hs-hide-block)
(define-key my-hs-mappings "a" 'hs-show-all)
(define-key my-hs-mappings "s" 'hs-show-block)

(add-hook 'hs-minor-mode-hook
          (lambda ()
            (define-key hs-minor-mode-map (kbd "M-o") my-hs-mappings)))


;;;; Python Settings
;;;;
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")


;;; Remove the mavy lines sent to the python shell so the REPL looks nice
;;; (see python.el for any updates)
(add-hook 'python-mode-hook
          (lambda ()
            (defun python-shell-send-region (start end)
              "Send the region delimited by START and END to inferior Python process."
              (interactive "r")
              (save-excursion
                (goto-char start)
                (if (looking-at "[ \t]")
                    (python-shell-send-string
                     (concat
                      (let ((line-num (line-number-at-pos start)))
                        (make-string (1+ line-num) ?\n))
                      (let* ((lines (split-string (buffer-substring start end) "\n" t))
                             (initial-indent (string-match "[^ \t]" (nth 0 lines)))
                             (newlist))
                        (dolist (line lines)
                          (if (> initial-indent (string-match "[^ \t]" line))
                              (error "Indentation of region is wrong %d" initial-indent)
                            (push (substring line initial-indent) newlist)))
                        (mapconcat 'identity newlist "\n"))))
                  (python-shell-send-string
                   (buffer-substring start end) nil t))))))


;;;; Uniquify Settings
;;;;
(setq uniquify-buffer-name-style 'post-forward)


;;;; Windmove Settings
;;;;
(windmove-default-keybindings)
(setq windmove-wrap-around t)


;;;; Winner Mode Settings
;;;;
(winner-mode 1)
