;;;; C Settings
;;;;
(setq c-default-style "linux"
      c-basic-offset 4)
(c-set-offset 'case-label '+)

;;; Use double quotes by default in awk, C, C++ and some others.
(with-eval-after-load 'cc-vars
  (add-hook 'c-mode-common-hook 'keyswap-include-quotes))


;;;; Comint Mode
;;;;
(add-hook 'comint-mode-hook
          (lambda ()
            (auto-fill-mode -1)))


;;;; Help mode settings
;;;;
(defun help-follow-source-link ()
  "Follow \"defined in ...\" link at top of *Help* buffer.

This function is really dumb about doing this -- it goes to first
window with the *Help* buffer showing, moves to the first link in
that buffer, and follows it."
  (interactive)
  (when (string-match "^\*Help\.*"  (buffer-name (current-buffer)))
      (let ((first-button (button-at (next-button 1))))
        (when (not (string-match "C source code" (button-label first-button)))
          ;; Act fully as if the user pressed the button themselves
          ;; (here as I look for the value of THIS-COMMAND in my custom
          ;; DISPLAY-BUFFER functions, because of precisely these commands)
          (let ((this-command 'push-button))
            (button-activate first-button))))))

(define-key help-mode-map (kbd "M-g M-f") 'help-follow-source-link)


;;;; Latex Settings
;;;;
(setq latex-run-command "pdflatex")

(with-eval-after-load 'tex-mode
  ;; Note -- order here is important
  (add-hook 'latex-mode-hook 'turn-on-shifted-keys t)
  (add-hook 'latex-mode-hook 'keyswap-include-braces t))


;;;; Lisp Settings
;;;;
(defun upsexp-newline-and-parentheses (&optional arg)
  "Move outside the current sexp, start a newline, leave `point'
inside a new sexp.

Argument determines how many lists to move down before starting
the new line.

Taken from \"speed-of-thought-lisp\"
https://github.com/Malabarba/speed-of-thought-lisp"
  (interactive "^p")
  (up-sexp arg)
  (newline-and-indent)
  (insert "()")
  (forward-char -1))

(define-key lisp-mode-shared-map (kbd "<C-return>") 'upsexp-newline-and-parentheses)
(define-key lisp-mode-shared-map (kbd "M-r") 'kill-backward-up-list)
(define-key lisp-mode-shared-map (kbd "M-s M-s") 'delete-pair)


(with-eval-after-load 'lisp-mode
  (define-key emacs-lisp-mode-map [mouse-3]
    (mouse-function-on-symbol (help-xref-interned (intern current-symbol))
                              (pop-tag-mark)))
  ;; Really don't know why emacs makes this a local key binding when there's
  ;; already a key-binding for it in the global map.
  (define-key emacs-lisp-mode-map (kbd "M-C-i") nil)
  (define-key lisp-interaction-mode-map (kbd "M-C-i") nil)
  (add-hook 'emacs-lisp-mode-hook 'keyswap-tac-underscore-exception)
  (add-hook 'lisp-mode-hook 'keyswap-tac-underscore-exception))

(add-hook 'eval-expression-minibuffer-setup-hook 'turn-on-shifted-keys)
(add-hook 'eval-expression-minibuffer-setup-hook 'keyswap-tac-underscore-exception)

(with-eval-after-load 'ielm
    (add-hook 'ielm-mode-hook 'keyswap-tac-underscore-exception)
    (add-hook 'ielm-mode-hook 'turn-on-shifted-keys))

(setq inferior-lisp-program "/usr/bin/sbcl")


;;;; Man Settings
;;;;
(setq Man-notify-method 'pushy)


;;;; Python Settings
;;;;
(defun python-shell-restart-inferior-python (&optional buffer)
  "Restarts the python shell currently running.

If `buffer' is given, restarts the process in that buffer, if `arg' is non-null
stops the current python process using `delete-process' rather than
`process-send-eof'"
  (interactive)
  ;; Be on the lookout for having to do something different with dedicated and
  ;; non-dedicated processes.
  (let* ((python-buffer (or (python-shell-get-buffer) buffer))
         (python-process (get-buffer-process python-buffer)))
    ;; If there's currently no process, then just start one.
    ;; Otherwise, call `delete-process' on it and start a new python shell with
    ;; the same arguments as the current one.
    (if (not python-process)
        (python-shell-get-or-create-process)
      (delete-process python-process)
      (python-shell-get-or-create-process
       (mapconcat 'identity (process-command python-process) " ")))))

;;; Python doesn't use semicolons very much, so make them all colons
(add-hook 'python-mode-hook
          (lambda ()
            (swap-these-keys [?\;] [?:])
            (setq-local keyswap-pairs
                        (append keyswap-pairs '((?: . ?\;))))))

;;;; Scheme Settings
;;;;
;; I'm using guile at the moment
(setq scheme-program-name "guile")

;
;;;; Javascript Settings
;;;;
(with-eval-after-load 'js
  (add-hook 'js-mode-hook 'keyswap-include-braces)
  (add-hook 'js-mode-hook 'keyswap-tac-underscore-exception))
