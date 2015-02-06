;;;; Calendar Settings
;;;;
(setq calendar-week-start-day 1)

(setq calendar-latitude 54)
(setq calendar-longitude 0)


;;;; Dired Additions
;;;;
(defun dired-load-marked-files ()
  "Lead all the marked files into emacs"
  (interactive)
  (dolist (current-file (dired-get-marked-files))
    (find-file-noselect current-file)))

(defun dired-emacs-command-marked-files (command-form)
  "Load marked files into emacs, and run COMMAND on them all"
  (interactive "C")
  (dolist (current-file (dired-get-marked-files))
    (with-current-buffer (find-file-noselect current-file)
      (save-excursion
        (call-interactively command-form)))))

(defun dired-do-grep (command-args)
  "Run `grep' on marked files.
A prefix argument behaves according to the ARG argument of
`dired-get-marked-files'.

This function is a stripped down version of `diredp-do-grep' from
dired+.el as I wanted this function, but not the rest of the
package."
  (interactive
   (progn
     (grep-compute-defaults)
     (list
      (read-from-minibuffer
       "grep <pattern> <files> :  "
       (cons (concat grep-command "  "
                     (mapconcat 'shell-quote-argument
                                (dired-get-marked-files nil current-prefix-arg)
                                " "))
             (+ 1 (length grep-command)))
       nil nil 'grep-history (grep-default-command)))))
  (grep command-args))

;;; Setting filesets from a dired buffer
(defun dired-set-fileset (flset-name)
  "Set files in fileset named FLSE-NAME to
`dired-get-marked-files'.

If that fileset doesn't exist, create it."
  (interactive (list (completing-read "Set fileset: " filesets-data)))
  (filesets-entry-set-files (or (assoc-string flset-name filesets-data)
                                (progn
                                  (add-to-list 'filesets-data
                                               (list flset-name '(:files)))
                                  (car filesets-data)))
                            (dired-get-marked-files) t)
  (filesets-set-config flset-name 'filesets-data filesets-data))

(defun dired-add-to-fileset (flset-name)
  "Add `dired-get-marked-files' to
`fileset-get-fileset-from-name' on FLSET-NAME

Ensure no duplicates in the fileset.

If there is no fileset by the name FLSET-NAME, create it."
  (interactive (list (completing-read "Set fileset: " filesets-data)))
  (let ((entry (assoc-string flset-name filesets-data)))
    (cond
     ((null entry) (dired-set-fileset flset-name))
     ((equal (filesets-entry-mode entry) ':files)
      (filesets-entry-set-files
       entry
       (cl-union (filesets-entry-get-files entry)
                 (dired-get-marked-files)
                 :test 'filesets-files-equalp) t)
      (filesets-set-config flset-name 'filesets-data filesets-data))
     (t
      (message "Fileset is of wrong type to set manually.")))))

(defun dired-remove-from-fileset (flset-name)
  "Remove `dired-get-marked-files' from
  `fileset-get-fileset-from-name' on FLSET-NAME

Ignore any files that aren't in the fileset."
  (interactive (list (completing-read "Set fileset: " filesets-data nil t)))
  (let ((entry (assoc-string flset-name filesets-data)))
    (when (equal (filesets-entry-mode entry) ':files)
      (filesets-entry-set-files
       entry
       (cl-set-difference (filesets-entry-get-files entry)
                          (dired-get-marked-files)
                          :test 'filesets-files-equalp) t)
      (filesets-set-config flset-name 'filesets-data filesets-data))))


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
  "Open a file in emacs."
  (if (null args)
      ;; If I just ran "emacs", I probably expect to be launching
      ;; Emacs, which is rather silly since I'm already in Emacs.
      ;; So just pretend to do what I ask.
      (bury-buffer)
    ;; We have to expand the file names or else naming a directory in an
    ;; argument causes later arguments to be looked for in that directory,
    ;; not the starting directory
    (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

(defun eshell--get-last-input ()
  "Inserts the last command argument at point.

Uses `eshell-last-arguments', so can't go past the last command
as yet."
  (interactive)
  (insert (or (car (last eshell-last-arguments)) "")))

(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map (kbd "M-.") 'eshell--get-last-input)
            (define-key eshell-mode-map (kbd "C-c M-.") 'eshell-find-tag)))

;; Have the smart option set up how I like it, but not enabled by default.
(setq eshell-where-to-jump 'after)
(setq eshell-review-quick-commands 'not-even-short-output)


;;;; Filesets Settings
;;;;
(filesets-init)
;; Save current state of filesets between sessions
(push 'filesets-data filesets-menu-cache-contents)


;;;; Flymake Settings
;;;;
;; Python flymake
;; Manually set the flymake configuration
;; At the moment the pylint flymake option in python-mode isn't doing anything
(setq pylint "epylint")

(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list pylint (list local-file)))))

(add-hook 'flymake-allowed-file-name-masks
          '("\\.py\\'" flymake-pylint-init))


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
  "Remove extra paren when expanding line in paredit"
  (if (and paredit-mode (equal (substring str -1) ")"))
      (backward-delete-char 1)))


;;;; Imenu Settings
;;;;
(global-set-key (kbd "C-c i") 'imenu)
(defadvice imenu (after move-to-top activate)
  (recenter 10))

;;; Python imenu
(add-hook 'python-mode-hook
          (lambda ()
            (setq imenu-create-index-function 'python-imenu-create-index)))

;;; Elisp imenu
;; don't like how this requires [:cntrl:] to avoid newlines
;; look for something better later.
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (push '("Section" "^;;;;\\s-?\\([^;[:cntrl:]].+\\)$" 1)
                  imenu-generic-expression)
            (push '("Subsection" "^;;;\\s-?\\([^;[:cntrl:]].+\\)$" 1)
                  imenu-generic-expression)))


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
;; (setq org-drawers (cons "THOUGHTS" org-drawers))
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


;; Have to do this when org-mode is leaded as org-mode-map isn't defined
;; otherwise.
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


;;;; Regexp Builder
;;;;
(setq reb-re-syntax 'string)


;;; Transient Mark Mode Settings
;;;
(transient-mark-mode 1)
(defun push-mark-no-activate ()
  "Push `point' to `mark-ring' and does not activate the region.
Equivalent to \\[set-mark-command] when \\[transient-mark-mode]
is disabled."
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun jump-to-mark ()
  "Jump to local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix
argument, when \\[transient-mark-mode] is disabled"
  (interactive)
  (set-mark-command 1))

(defun exchange-point-and-mark-keep-activation (&optional arg)
  "Identical to \\[exchange-point-and-mark] but will keep current
region activation state witowithout ARG, and toggle activation
state otherwise."
  (interactive "P")
  (let ((should-deactivate (eq mark-active (not (not arg)))))
    (exchange-point-and-mark should-deactivate)))

(global-set-key (kbd "C-`") 'push-mark-no-activate)
(global-set-key (kbd "M-`") 'jump-to-mark)
;; Can't use the 'remap' thing, as there is already a remap from
;; cua-exchange-point-and-mark
(define-key ctl-x-map (kbd "C-x") 'exchange-point-and-mark-keep-activation)


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
