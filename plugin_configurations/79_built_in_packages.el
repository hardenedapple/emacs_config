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


;;;; Eshell Settings
;;;;
(require 'eshell)

;; Prompt
(setq eshell-prompt-function
      (lambda ()
        (concat (file-name-nondirectory (directory-file-name (eshell/pwd)))
         (format-time-string " [%H:%M:%S]")
         (if (= (user-uid) 0) " # " " $ "))))

;; These commands are the ones I don't want running in eshell.
;; I either want them to be run in ansi-term or by some personally defined
;; function
(setq eshell-visual-subcommands (list
                                 (list "git" "log" "diff" "show" "graph")
                                 (list "hg" "log" "diff")))

(defun eshell/emacs (&rest args)
  "Open a file in emacs. Some habits die hard."
  (if (null args)
      ;; If I just ran "emacs", I probably expect to be launching
      ;; Emacs, which is rather silly since I'm already in Emacs.
      ;; So just pretend to do what I ask.
      (bury-buffer)
    ;; We have to expand the file names or else naming a directory in an
    ;; argument causes later arguments to be looked for in that directory,
    ;; not the starting directory
    (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

;; Have the smart option set up how I like it, but not enabled by default.
(require 'em-smart)
(setq eshell-where-to-jump 'after)
(setq eshell-review-quick-commands 'not-even-short-output)


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


;;;; Hippie Expand Settings
;;;;
;; While I have no mapping for HIPPIE-EXPAND, I set up smart tab to use it by
;; default whenever autocomplete is not available. Hence, when there isn't
;; autocomplete, this is the default completion, available on the TAB key.
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-line
        try-expand-list))

(fset 'hippie-complete-file (make-hippie-expand-function
                             '(try-complete-file-name-partially
                               try-complete-file-name)))
(fset 'hippie-expand-dabbrev (make-hippie-expand-function
                              '(try-expand-dabbrev
                                try-expand-dabbrev-all-buffers
                                try-expand-dabbrev-from-kill)))
;; This is my default special HIPPIE-EXPANION function, default here means ready
;; for {e,}lisp (as that's what I mainly use emacs for).
;; The idea is this keybinding is set on a per buffer basis, with customisations
;; based on mode, while filename completion and dabbrev completion are more
;; general.
(fset 'hippie-expand-special (make-hippie-expand-function
                              '(try-expand-list
                                try-expand-list-all-buffers
                                try-expand-line
                                try-expand-line-all-buffers)))
(global-set-key (kbd "M-/") 'hippie-complete-file)
(global-set-key (kbd "M-\\") 'hippie-expand-dabbrev)
(global-set-key (kbd "C-M-/") 'hippie-expand-special)
(global-set-key (kbd "C-\\") 'dabbrev-completion)

;; TRY-EXPAND-LINE and TRY-EXPAND-LIST add an extra ")" character when in
;; paredit-mode, fix this with an advice (as suggested on the HIPPIE-EXPAND
;; emacs wiki page)
(defadvice he-substitute-string (after he-paredit-fix activate)
  "remove extra paren when expanding line in paredit"
  (if (and paredit-mode (equal (substring str -1) ")"))
      (progn (backward-delete-char 1) (forward-char))))


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


;;; Have to do this when org-mode is leaded as org-mode-map isn't defined
;;; otherwise.
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "C-'") nil)
            (define-key org-mode-map (kbd "C-;") 'org-cycle-agenda-files)))


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

(add-hook 'inferior-python-mode-hook
          (lambda ()
            (define-key inferior-python-mode-map (kbd "TAB")
              'python-shell-completion-complete-or-indent)))


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
