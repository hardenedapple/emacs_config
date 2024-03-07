;;; -*- lexical-binding: t -*-
;;;; Calendar Settings
;;;;
(setq calendar-week-start-day 1)

(setq calendar-latitude 54)
(setq calendar-longitude 0)


;;;; Dired Additions
;;;;
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

;;; View filesets in a dired buffer
;;; Almost direct copy from dired+.el
(defun dired-fileset (flset-name)
  "Open Dired on the files in fileset FLSET-NAME."
  (interactive (list (completing-read "Open Dired on fileset: " filesets-data)))
  (dired--fileset-1 flset-name))

(defun dired-fileset-other-window (flset-name)
  "Open Dired in another window on the files in fileset FLSET-NAME."
  (interactive (list (completing-read "Open Dired on fileset: " filesets-data)))
  (dired--fileset-1 flset-name 'OTHER-WINDOW))

(defun dired--fileset-1 (flset-name &optional other-window-p)
  "Helper for `dired-fileset(-other-window)'."
  (let ((flset   (filesets-get-fileset-from-name flset-name))
        (dirfun  (if other-window-p #'dired-other-window #'dired))
        mode)
    (unless (setq mode (filesets-entry-mode flset))
      (error "Bad fileset: %S" flset-name))
    (message "Gathering file names...")
    (let ((files (filesets-entry-get-files flset)))
      (funcall dirfun (cons (generate-new-buffer-name flset-name)
                            (nreverse (mapcar (lambda (file)
                                                (if (file-name-absolute-p file)
                                                    (expand-file-name file)
                                                  file))
                                              files)))))))

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


;;;; Dired-X Settings
;;;;
(setq dired-bind-info nil
      dired-bind-jump nil
      dired-bind-man nil
      dired-bind-vm nil
      dired-clean-up-buffers-too t)
(global-set-key (kbd "C-x d") 'dired-jump)


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


;;;; EPA File Settings
;;;;
;; Interface to gnupg
(epa-file-enable)

;;;; Eshell Settings
;;;;
;; Prompt
(setq eshell-prompt-function
      (lambda ()
        (concat (file-name-nondirectory (directory-file-name (eshell/pwd)))
                (format-time-string " [%H:%M:%S]")
                (if (= (user-uid) 0) " # " " $ "))))

(defun eshell--get-last-input ()
  "Inserts the last command argument at point.

Uses `eshell-last-arguments', so can't go past the last command
as yet."
  (interactive)
  (insert (or (car (last eshell-last-arguments)) "")))

;; This has to be in a hook rather than in `with-eval-after-load', as eshell
;; creates a new, local, keymap every time it's invoked.
;; (search for "FIXME: What the hell?" in esh-mode.el.gz
(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map (kbd "M-.") 'eshell--get-last-input)
            (define-key eshell-mode-map (kbd "C-c M-.") 'eshell-find-tag)
            (define-key eshell-mode-map (kbd "M-r") 'eshell-isearch-backward)
            ;; TODO put `eshell-isearch-repeat-backward' under M-r somewhere in
            ;; the `isearch' map so that I can find the next matching command.
            ;; (it does work with "M-x eshell-isearch-repeat-backward" and then
            ;; "C-x z" a few times, which shows that all I need to do is put a
            ;; mapping accessible in the map, but I don't want the command
            ;; accessible for `isearch' done in any buffers other than in
            ;; `eshell'.
            ))

;; Have the smart option set up how I like it, but not enabled by default.
(setq eshell-where-to-jump 'after)
(setq eshell-review-quick-commands 'not-even-short-output)


;;;; ETags Settings
;;;;
;; Match keybindings for slime, and elisp-slime.
(global-set-key (kbd "M-,") 'pop-tag-mark)
(global-set-key (kbd "M-*") 'tags-loop-continue)


;;;; Filesets Settings
;;;;
(filesets-init)
;; Save current state of filesets between sessions
(push 'filesets-data filesets-menu-cache-contents)

(defun filesets-remove (name)
  "Remove a fileset from `filesets-data'"
   (interactive (list (completing-read "Remove fileset: " filesets-data nil t)))
   (setq filesets-data (delete* name filesets-data :test #'string= :key #'car)))


;;;; Flymake Settings
;;;;
;;;; Eglot Settings
;;;;
;; Both the timers below are a bit of a trade-off.  I really don't like it when
;; flymake and eglot start updating in the middle of my typing -- both delaying
;; my typing and adding a bunch of unnecessary complaints about not finished
;; code.  However, I can also imagine wanting to type then ask about something,
;; and the server not having any way to tell me what's going on because it
;; didn't understand.
;; Setting both to the same thing so I never forget to update both at the same
;; time.
(setq eglot-send-changes-idle-time
      (setq flymake-no-changes-timeout 2))

;;;; Find File at Point
;;;;
(ffap-bindings)

;;;; HideShow Settings
;;;;
(add-hook 'prog-mode-hook 'hs-minor-mode)

;;;; Special Expand Functions
;;;;
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
(global-set-key (kbd "C-M-/") 'hippie-expand-special)
(global-set-key (kbd "M-/") 'comint-dynamic-complete-filename)
(global-set-key (kbd "M-\\") 'dabbrev-completion)

;;;; Ibuffer Settings
;;;;
(with-eval-after-load 'ibuffer
  (define-key ibuffer-mode-map (kbd "M-g") nil))


;;;; Imenu Settings
;;;;
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


;;;; ISearch Settings
;;;;
(defun snappy-isearch-symbol-at-point (forwards)
  "Do the same as `isearch-forward-symbol-at-point', but snappier.

Is a mostly copy of the `isearch-forward-symbol-at-point'
function, but instead of calling `isearch-forward-symbol', calls
`isearch-mode' directly.

Also move `point' so it's not at either end of the symbol, meaning we
move on the first invokation."
  (isearch-mode forwards nil nil nil 'isearch-symbol-regexp)
  (let ((bounds (find-tag-default-bounds)))
    (if bounds
        (progn
          (goto-char (+ 1 (car bounds)))
          (isearch-yank-string
           (buffer-substring-no-properties (car bounds) (cdr bounds))))
      (setq isearch-error "No symbol at point")
      (isearch-update))))

(defun snappy-isearch-symbol-at-point-forwards ()
  (interactive)
  (snappy-isearch-symbol-at-point t))

(defun snappy-isearch-symbol-at-point-backwards ()
  (interactive)
  (snappy-isearch-symbol-at-point nil))

(global-set-key (kbd "M-i") 'snappy-isearch-symbol-at-point-forwards)
(global-set-key (kbd "M-o") 'snappy-isearch-symbol-at-point-backwards)
(define-key isearch-mode-map (kbd "M-i") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "M-o") 'isearch-repeat-backward)

(defun symbol-replace-this (arg)
  "Does the same as `smartscan-symbol-replace', but searches for
entire symbols instead of words.

Also doesn't use any `smartscan' functions, as I use an `isearch'
wrapper called `snappy-isearch-symbol-at-point' instead and hence
don't have the package installed."
  (interactive "P")
  (save-excursion
    (let* ((oldsymbol (find-tag-default))
           (newsymbol (query-replace-read-to
                       oldsymbol (format "%sSymbol replace"
                                         (if arg "[Defun] " "")) nil))
           (counter 0))
      (if arg (goto-char (save-excursion (beginning-of-defun) (point)))
        ;; go to the beginning of the buffer as it's assumed you want to
        ;; apply it from there onwards. beginning
        (goto-char (point-min)))
      (while (re-search-forward
              (isearch-symbol-regexp oldsymbol)
              (if arg (save-excursion (end-of-defun) (point)) nil) t nil)
        (replace-match newsymbol t) (cl-incf counter 1))
      (message "Have replaced %d matches" counter))))

(global-set-key (kbd "M-'") 'symbol-replace-this)


;;;; List Buffer Settings
;;;;
(global-set-key [remap list-buffers] 'ibuffer)

;; Sometimes it's easier to sort by something, then put some files in the region
;; and mark all those.
(defun ibuffer-mark-by-region (start end)
  "Mark all files in the current region."
  (interactive "r")
  ;; `ibuffer-set-mark' inserts text, so save-excursion can't restore `point'
  ;; properly. It doesn't change the number of characters in the buffer though,
  ;; so we can easily reset the point.
  (let ((old-point (point)))
    (setq end (progn (goto-char end)
                     (if (null (bolp))
                         end
                       (ibuffer-forward-line 0 t)
                       (point))))
    (goto-char start)
    (ibuffer-forward-line 0 t)
    (while (< (point) end)
      (ibuffer-set-mark ibuffer-marked-char)
      (ibuffer-forward-line 1 t))
    (goto-char old-point)))

(with-eval-after-load 'ibuffer
  (define-key ibuffer-mode-map "m"
    (lambda ()
      (interactive)
      (if mark-active
          (call-interactively 'ibuffer-mark-by-region)
        (call-interactively 'ibuffer-mark-forward)))))


;;;; Org Mode Settings
;;;;
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c L") 'org-insert-link-global)
(global-set-key (kbd "C-c o") 'org-open-at-point-global)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c b") 'org-iswitchb)
;; (setq org-drawers (cons "THOUGHTS" org-drawers))
(setq org-directory "~/NOTES"
      org-hide-block-startup t
      org-startup-indented t
      org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t
      org-default-notes-file (concat org-directory "/Random.org"))

(with-eval-after-load 'org
    ;; Remove the CYCLE-ORG-AGENDA-FILES mapping to leave the
    ;; GOTO-LAST-CHANGE-REVERSE global mapping
    ;; Don't really use ORG-MODE's agenda functionality anyway
  (define-key org-mode-map (kbd "C-,") nil)
  (add-to-list 'org-modules 'org-habit))

;;;; Process Menu Settings
;;;;
;; Be able to send the kill signal to the process at point
;; Code gotten from Joao Tavora's answer in
;; http://stackoverflow.com/questions/10627289/emacs-internal-process-killing-any-command
(defun process-menu-delete-process-at-point ()
  "Kill the process at point in a `process-menu' buffer."
  (interactive)
  (let ((process (get-text-property (point) 'tabulated-list-id)))
    (cond ((and process
                (processp process))
           (delete-process process)
           (revert-buffer))
          (t
           (error "no process at point!")))))

(define-key process-menu-mode-map "k" 'process-menu-delete-process-at-point)


;;;; Python Settings
;;;;
(with-eval-after-load 'python
    (define-key inferior-python-mode-map (kbd "TAB")
      'python-shell-completion-complete-or-indent)
    (setq python-shell-process-environment (list "PAGER=")))


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

(defun toggle-current-mark-activation ()
  "Set `mark-active' to `t'.

Sometimes you want to reselect the current area, run this function."
  (interactive)
  (setq mark-active (not mark-active)))

(global-set-key (kbd "C-`") 'push-mark-no-activate)
(global-set-key (kbd "C-~") 'toggle-current-mark-activation)
(global-set-key (kbd "M-`") 'jump-to-mark)
;; Can't use the 'remap' thing, as there is already a remap from
;; cua-exchange-point-and-mark
(define-key ctl-x-map (kbd "C-x") 'exchange-point-and-mark-keep-activation)


;;;; Uniquify Settings
;;;;
(setq uniquify-buffer-name-style 'post-forward)


;;;; Winner Mode Settings
;;;;
(winner-mode 1)
