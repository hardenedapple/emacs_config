;;;; My non-standard-stuff
;;;;

;;;; Splice Window Settings
(define-key ctl-x-4-map "s" 'splice-window-upwards)

;;;; Keyswap Mode Settings
;; Binding things in `prog-mode-map', which have to be overridden by minor modes
;; for special bindings.
(add-hook 'prog-mode-hook 'keyswap-mode)

;;; Use double quotes by default in awk, C, C++ and some others.
(with-eval-after-load 'cc-vars
  (add-hook 'c-mode-common-hook 'keyswap-include-quotes))

(with-eval-after-load 'tex-mode
  (add-hook 'latex-mode-hook 'keyswap-mode t)
  (add-hook 'latex-mode-hook 'keyswap-include-braces t))

(with-eval-after-load 'lisp-mode
  (add-hook 'emacs-lisp-mode-hook 'keyswap-tac-underscore-exception)
  (add-hook 'lisp-mode-hook 'keyswap-tac-underscore-exception)
  (add-hook 'erts-mode-hook 'keyswap-tac-underscore-exception))

(with-eval-after-load 'inf-lisp
  (add-hook 'inferior-lisp-mode-hook 'keyswap-mode)
  (add-hook 'inferior-lisp-mode-hook 'keyswap-tac-underscore-exception))

(with-eval-after-load 'scheme
  (add-hook 'scheme-mode-hook 'keyswap-tac-underscore-exception))

(with-eval-after-load 'cmuscheme
  (add-hook 'inferior-scheme-mode-hook 'keyswap-tac-underscore-exception)
  (add-hook 'inferior-scheme-mode-hook 'keyswap-mode))

(add-hook 'eval-expression-minibuffer-setup-hook 'keyswap-tac-underscore-exception)
(add-hook 'eval-expression-minibuffer-setup-hook 'keyswap-mode)

(with-eval-after-load 'ielm
  (add-hook 'ielm-mode-hook 'keyswap-tac-underscore-exception)
  (add-hook 'ielm-mode-hook 'keyswap-mode))

;; Python doesn't use semicolons very much, so make them all colons
(add-hook 'python-mode-hook 'keyswap-colon-semicolon)

(with-eval-after-load 'sh-script
  (add-hook 'sh-mode-hook 'keyswap-tac-underscore-exception)
  (add-hook 'sh-mode-hook 'keyswap-include-braces))

(with-eval-after-load 'js
  (add-hook 'js-mode-hook 'keyswap-include-braces)
  (add-hook 'js-mode-hook 'keyswap-tac-underscore-exception))

(add-hook 'isearch-mode-hook 'keyswap-isearch-start-hook)

(with-eval-after-load 'python
  (add-hook 'inferior-python-mode-hook 'keyswap-colon-semicolon)
  (add-hook 'inferior-python-mode-hook 'keyswap-mode))

(with-eval-after-load 'avy (keyswap-avy-integrate))

;;;; Random settings for my keyboard tricks.
;;;;
;; I have a keyboard which switches the numbers and symbols.
;; This can get a pain when I want to use prefix arguments.
;; Just have all prefix arguments mapped on the control-symbol values too.
;; The M-<symbol> things are largely mapped to things that I would use, but the
;; C-<symbol> stuff is not, and that's the one that trips me up the most
;; (especially C-0 C-k in org-mode).
(defun digit-shifted-argument (arg index)
  "Part of the numeric argument for the next command.
\\[universal-argument] following digits or minus sign ends the argument."
  (prefix-command-preserve-state)
  (setq prefix-arg (cond ((integerp arg)
                          (+ (* arg 10)
                             (if (< arg 0) (- index) index)))
                         ((eq arg '-)
                          ;; Treat -0 as just -, so that -01 will work.
                          (if (zerop index) '- (- index)))
                         (t index)))
  (universal-argument--mode))
(let ((index 0))
  (dolist (k '("C-)" "C-!" "C-@" "C-#" "C-$" "C-%" "C-^" "C-&" "C-*" "C-("))
    (global-set-key (kbd k) `(lambda (arg) (interactive "P")
                               (digit-shifted-argument arg ,index)))
    (setq index (+ index 1))))

(global-set-key (kbd "C-_") 'negative-argument)
(global-set-key (kbd "M-_") 'negative-argument)


;;;; Colour theme
;;;;
(load-theme 'monokai)


;;;; Avy Mode Settings
;;;;
(define-key goto-map (kbd "M-s") 'avy-goto-word-1)
(define-key goto-map "s" 'avy-goto-word-1)
(setq avy-all-windows 'nil)
;; Dvorak order, plus allow all keys on the keyboard -- makes it more likely to
;; give just one value.
(setq avy-keys '(?d ?h ?t ?n ?s ?a ?o ?e ?u ?i
                    ?f ?g ?c ?r ?l ?, ?. ?- ?p ?y
                    ?b ?m ?w ?v ?z ?' ?q ?j ?k ?x))


;;;; Buffer Move Settings
;;;;
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)


;;;; C-ELDOC-MODE
;;;;
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)


;;;; Dot Mode Settings
;;;;
;; Have to remap everything because C-. is `goto-last-change' for me.
(require 'dot-mode)
(define-key dot-mode-map (kbd "C-.") nil)
(define-key dot-mode-map (kbd "C-M-.") nil)
(define-key dot-mode-map (kbd "C-M-.") nil)
(define-key dot-mode-map (kbd "C-c .") nil)
(define-key dot-mode-map (kbd "C-z") 'dot-mode-execute)
(define-key dot-mode-map (kbd "C-M-z") 'dot-mode-override)
(define-key dot-mode-map (kbd "C-c x") 'dot-mode-copy-to-last-kbd-macro)
(global-dot-mode t)
(setq dot-mode-verbose nil)


;;;; Elisp Slime Nav Settings
;;;;
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq find-definition-function 'elisp-slime-nav-find-elisp-thing-at-point)))

(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))


;;;; Expand Region Settings
;;;;
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-+") 'er/contract-region)


;;;; Feature Mode Settings
;;;;
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))


;;;; Goto chg Settings
;;;;
(global-set-key (kbd "C-.") 'goto-last-change)
(global-set-key (kbd "C-,") 'goto-last-change-reverse)


;;;; Ido at point Settings
;;;;
(ido-at-point-mode)


;;;; Ido Ubiquitous Settings
;;;;
(ido-ubiquitous-mode)


;;;; Ido Vertical Mode
;;;;
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)

;;;; Ido completing read +
;;;;
;; Want to have completing read in Info menu entries
(with-eval-after-load 'info
  (defun ido-info-menu ()
    (interactive)
    (let ((ido-cr+-force-on-functional-collection t))
      (call-interactively 'Info-menu)))
  (defun ido-info-index ()
    (interactive)
    (let ((ido-cr+-force-on-functional-collection t))
      (call-interactively 'Info-index)))
  (define-key Info-mode-map "m" 'ido-info-menu)
  (define-key Info-mode-map "i" 'ido-info-index))

;;;; Jump Char Settings
;;;;
(global-set-key (kbd "M-m") 'jump-char-forward)
(global-set-key (kbd "M-M") 'jump-char-backward)
;; To match the relative direction of these keys on my specific keyboard.
(setq jump-char-forward-key ",")
(setq jump-char-backward-key ";")

;; Strange things about jump-char
;; C-c C-c  comes out as C-j C-c


;;;; List Registers Settings
;;;;
(define-key ctl-x-r-map "v" 'list-register)


;;;; Magit Settings
;;;;
(setq magit-repo-dirs '("~/.emacs.d" "~/share/repos/useful-files"))
(defun magit-stage-this-file ()
  "Stage file the current buffer is visiting."
  (interactive)
  (magit-stage-item (buffer-file-name)))

(global-set-key (kbd "C-c g") 'magit-status)
;; git commit mode usually starts flyspell
(defun fill-at-70 () (setq fill-column 70) (turn-on-auto-fill))
(with-eval-after-load 'git-commit (add-hook 'git-commit-setup-hook 'fill-at-70))
(setq magit-last-seen-setup-instructions "1.4.0")


;;; Intercept calling git in Eshell and parse some into Magit
;;
;; NOTE:
;;     Some of these functions are taken directly from magit.el, while changing
;;     the SWITCH-FUNC argument to `magit-mode-setup' macro so that always use
;;     the current window.
;;
;; TODO:
;;     Add something for my shell aliases gr and hr
;;     Possibly get the output from 'start-file-process, then use that in the
;;     function to change directory
(defun eshell--magit-log-function (range &optional type arguments)
  "Runs `magit-log', but with optional arguments.

Just a reworking of `magit-log' and `magit-log-long' for my
`eshell' customisation ease."
  (let ((range (cadr range))
        (type (if type type 'oneline)))
    (cond ((not range) (setq range "HEAD"))
          ;; Forward compatibility kludge.
          ((listp range) (setq range (car range))))
    (magit-mode-setup magit-log-buffer-name
                      #'switch-to-buffer
                      #'magit-log-mode
                      #'magit-refresh-log-buffer
                      type range arguments)))

(setq eshell-magit->git-transformations
      (list
       '("log" . eshell--magit-log-function)
       '("diff" . (lambda (args)
                    (magit-mode-setup magit-diff-buffer-name
                                      #'switch-to-buffer
                                      #'magit-diff-mode
                                      #'magit-refresh-diff-buffer
                                      (or (cadr args) "HEAD") nil nil)
                    nil))
       '("status" . (lambda (args)
                      (magit-status default-directory
                                    #'switch-to-buffer)))
       '("graph" . (lambda (range)
                     (eshell--magit-log-function range 'long (list "--graph"))))))

(defun eshell-delegate-external (command args)
  "Call external command "
  (eshell-wait-for-process (eshell-external-command command args)))

(defun eshell/git (&rest args)
  "Function to use some of `magit' abilities in `eshell'.

Checks if the subcommand is one of the keys in the assoc list
`eshell-magit->git-transformations' if it isn't, we simply pass
it on to the external git command, otherwise, do something almost
equivalent with `magit'"
  (let ((function-to-call
         (or (cdr (assoc (car args) eshell-magit->git-transformations))
             (lambda (args)
               (eshell-delegate-external (eshell-search-path "git") args)))))
    (funcall function-to-call args)
    nil))

(defun eshell/gr (&rest args)
  "Go to the current repositorys' root dir.

Calls `eshell/cd' to the value of `magit-get-top-dir'"
  (let ((git-root (magit-get-top-dir)))
    (when git-root
      (eshell/cd git-root))))


;;;; Move Text
;;;;
(global-set-key (kbd "C-s-<up>") 'move-text-up)
(global-set-key (kbd "C-s-<down>") 'move-text-down)


;;;; Monky Settings
(global-set-key (kbd "C-c h") 'monky-status)
(with-eval-after-load 'monky (add-hook monky-commit-mode-hook 'fill-at-70))

(defun eshell/hr (&rest args)
  "Go to the current mercurial repositorys' root dir.

Calls `eshell/cd' to the value of `magit-get-top-dir'"
  (let ((hg-root (monky-get-root-dir)))
    (when hg-root
      (eshell/cd hg-root))))


;;;; Multiple Cursors Settings
;;;;
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;;;; Nameses Settins
;;;;
(setq nameses-ido-mode t)

;;;; Org Mode Settings
;;;;

;; Redefinition of `org-comment-line-break-function' since the real
;; implementation unconditionally uses `insert-before-markers-and-inherit' in
;; with `fill-prefix' without checking if it is `nil'.
(defun org-comment-line-break-function (&optional soft)
  "Break line at point and indent, continuing comment if within one.
The inserted newline is marked hard if variable
`use-hard-newlines' is true, unless optional argument SOFT is
non-nil."
  (if soft (insert-and-inherit ?\n) (newline 1))
  (save-excursion (forward-char -1) (delete-horizontal-space))
  (delete-horizontal-space)
  (indent-to-left-margin)
  (if fill-prefix (insert-before-markers-and-inherit fill-prefix)
    (insert-before-markers-and-inherit "")))
;; Avoid overriding the control character digit argument mappings I have in the
;; global map.
(with-eval-after-load 'org-mode
  (define-key org-mode-map (kbd "C-#") 'nil))


;;;; Rust Mode Settings
;;;;
;; Remove the mapping that shadows my `fill-sentence' command.
(with-eval-after-load 'rust-mode
  (define-key rust-mode-map (kbd "C-M-q") nil))

;;;; Wrap Region Settings
;;;;
(wrap-region-global-mode t)
(add-hook 'wrap-region-mode-hook 'keyswap-update-keys)
(defadvice wrap-region-fallback (around keyswap-negate protect activate)
  "Ensure that `keyswap-mode' is not active when
  `wrap-region-fallback' is getting called."
  (let ((currently-on keyswap-mode))
    (when currently-on (keyswap-mode 0))
    ad-do-it
    (when currently-on (keyswap-mode 1))))


;;;; Paredit Settings
;;;;
(dolist (hook '(eval-expression-minibuffer-setup-hook
                emacs-lisp-mode-hook ielm-mode-hook lisp-mode-hook
                scheme-mode-hook
                inferior-scheme-mode-hook slime-repl-mode-hook))
  (add-hook hook #'enable-paredit-mode))

;; N.b. this seems to be causing problems now that I've updated to emacs 28.
;; Looks like the paredit-RET is overriding the original RET for the minibuffer
;; that was bound to `read--expression-try-read'.
;; I would be interested in figuring out how to ensure that particular binding
;; does not get priority, but for now I can use `C-j' (which can be used via the
;; translation mapping from `C-c j') in order to execute the code in the
;; minibuffer.
;;
;; You can't unmap RET in `paredit-mode-map' just for the minibuffer, so it
;; seems the most natural option is to take the mapping that I *want* and put
;; that in a higher-priority keymap for the minibuffer ... not yet done since I
;; dislike adding extra layers and the `C-c j' approach is working enough for
;; me.
;; Used the below to test this out (observe that the RET mapping is also lost in
;; standard lisp buffers).
;; (defun temp-function () (define-key paredit-mode-map (kbd "RET") 'nil))
;; (add-hook 'eval-expression-minibuffer-setup-hook #'temp-function 99)
;; (remove-hook 'eval-expression-minibuffer-setup-hook #'temp-function)


(defun paredit--is-at-end-of-list ()
  (and (looking-back ")\\|\\]")
       (not (nth 3 (syntax-ppss)))   ;; inside string
       (not (nth 4 (syntax-ppss))))) ;; inside comment

(defun paredit-duplicate-closest-list ()
  "Duplicate sexp the point is at."
  (interactive)
  ;; Go to end of current sexp.
  ;; Not sure when the loop will be needed, but better safe than sorry
  (while (not (paredit--is-at-end-of-list))
    (paredit-forward-up))
  (let* ((end (point))
         (dup-sexp (buffer-substring (progn (paredit-backward) (point)) end)))
    ;; Insert copy, make sure we're on a new line, put us at the end of both
    ;; sexps
    (insert dup-sexp)
    (paredit-newline)
    (paredit-forward)))

(defun paredit-downsexp-newline-and-parenthesis (&optional arg)
  "Do same as `downlist-newline-and-parentheses', but using
paredit functions on the assumption they'll be more robust."
  (interactive "^p")
  (paredit-forward-up arg)
  (paredit-newline)
  (paredit-open-round))

;;; Add keybinding C-c d to run paredit-duplicate-closest-sexp in paredit
(with-eval-after-load 'paredit
  (define-key paredit-mode-map (kbd "C-c d") 'paredit-duplicate-closest-list)
  (define-key paredit-mode-map (kbd "C-j") 'nil)
  ;; Remove mappings which interfere with prefix argument switching around above.
  (define-key paredit-mode-map (kbd "C-(") 'nil)
  (define-key paredit-mode-map (kbd "C-)") 'nil)
  ;; Keep M-s search commands by moving PAREDIT-SPLICE-SEXP, so the keymaps join
  (define-key paredit-mode-map (kbd "M-s") nil)
  (define-key paredit-mode-map (kbd "M-s M-s") 'paredit-splice-sexp)
  (define-key paredit-mode-map (kbd "M-s s") 'paredit-splice-sexp)
  (define-key paredit-mode-map (kbd "<escape>") nil)
  (define-key paredit-mode-map (kbd "<C-return>")
              'paredit-downsexp-newline-and-parenthesis)
  ;; TRY-EXPAND-LINE and TRY-EXPAND-LIST add an extra ")" character when in
  ;; paredit-mode, fix this with an advice (as suggested on the HIPPIE-EXPAND
  ;; emacs wiki page)
  (defadvice he-substitute-string (after he-paredit-fix activate)
    "Remove extra paren when expanding line in paredit"
    (if (and paredit-mode (member (substring str -1) '(")" "]" "\"" "}")))
        (delete-char -1))))
;; When this hook is being run, sometimes the keys have already been
;; swapped.
;; This would mean that the `keyswap-map' created when `keyswap-minor-mode' was
;; created does not contain mappings to swap the keys that `paredit-mode' has
;; defined.
;; To counter this, whenever `paredit-mode' is turned on, we refresh the
;; `keyswap-mode' keymap.
(add-hook 'paredit-mode-hook 'keyswap-update-keys)

;; Paredit M-r overrides M-r in comint
;; Want comint-history-isearch-backward-regexp, so remap it to C-q
(with-eval-after-load 'ielm
  (define-key ielm-map (kbd "C-q") 'comint-history-isearch-backward-regexp))

;;; paredit with eldoc
(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)



;;;; Projectile Settings
;;;;
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'ido)
(setq projectile-use-git-grep t)
(setq projectile-remember-window-configs t)

;;;; Smart Window
;; Once loaded, overwrite with my own mappings
(defun run-command-split-window (direction)
  "Return a function that calls `split-window' in DIRECTION, then
runs a user defined command."
  `(lambda (command)
     "Split the current window in ,direction then run COMMAND in
that window."
     (interactive "C")
     (select-window (split-window (selected-window) nil ',direction))
     (call-interactively command)))

;; n.b. `smart-window' must be loaded before its bindings take affect.
;; Moreover, when loading the file, it unconditionally sets global key bindings.
;; This means that without the `require' below, my functions could be run once
;; without a prefix argument before `smart-window' would override them with
;; `sw-below' and `sw-right'.
;; Similarly, any call to `smart-window-move', `smart-window-buffer-split',
;; `smart-window-file-split', `sw-above', or `sw-left' would also override my
;; functions with `sw-below' and `sw-right'.
(require 'smart-window)
(define-key ctl-x-map "2"
  (lambda (arg) (interactive "P")
    "

Do one of three actions depending on ARG.
Calling directly, split the window below, and switch to a buffer
in that window

With the universal argument, call `split-window-below'

With the universal argument twice, split the window to the below,
and run a command given by the user in that window.

"
    (cond
     ((null arg) (call-interactively 'sw-below))
     ((= (car arg) 4) (select-window (split-window-below)))
     ((> (car arg) 4)
      (call-interactively (run-command-split-window 'below))))))

(define-key ctl-x-map "3"
  (lambda (arg) (interactive "P")
    "

Do one of three actions depending on ARG.
Calling directly, split the window right, and switch to a buffer
in that window

With the universal argument, call `split-window-right'

With the universal argument twice, split the window to the right,
and run a command given by the user in that window.

"
    (cond
     ((null arg) (call-interactively 'sw-right))
     ((= (car arg) 4) (select-window (split-window-right)))
     ((> (car arg) 4)
      (call-interactively (run-command-split-window 'right))))))


;;;; Smex Settings
;;;;
(setq smex-history-length 20)
(setq smex-save-file "~/.emacs.d/smex-items")
(smex-initialize)
;; Want to remap the keys I'm currently using for `execute-extended-command',
;; but leave one just in case everything goes wrong (e.g. my keymappings have
;; all broken somehow, and `smex' isn't opening, I want to be able to run some
;; kind of command, so leave <menu> as `execute-extended-command' for this
;; hypothetical scenario)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "<escape>") 'smex)
;;(global-set-key (kbd "<menu>") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


;;;; Slime Settings
;;;;
(setq slime-contribs '(slime-fancy slime-asdf))
(setq slime-complete-symbol*-fancy t)

(global-set-key (kbd "C-c s") 'slime-selector)

(with-eval-after-load 'slime
  ;; Remove the M-? mapping for `slime-edit-uses', can still use M-_,
  ;; but `paredit-convolute-sexp' is now unshadowed.
  (define-key slime-mode-map (kbd "M-?") nil)
  (define-key slime-mode-map mouse-go-back-key
    (lambda (event) (interactive "e") (slime-pop-find-definition-stack)))
  (add-hook 'slime-mode-hook
            (lambda () (setq find-definition-function 'slime-edit-definition))))

(with-eval-after-load 'slime-repl
  (add-hook 'slime-repl-mode-hook 'keyswap-tac-underscore-exception t)
  (add-hook 'slime-repl-mode-hook 'keyswap-mode t)
  (define-key slime-repl-mode-map (kbd "DEL") nil)
  (define-key slime-repl-mode-map (kbd "M-r") nil))

(setq slime-autodoc-use-multiline-p t)


;;;; Transpose Frame Settings
;;;;
(define-key ctl-x-5-map "t" 'transpose-frame)
(define-key ctl-x-5-map "v" 'flip-frame)
(define-key ctl-x-5-map "h" 'flop-frame)
(define-key ctl-x-5-map "r" 'rotate-frame-clockwise)


;;;; Transpose Mark Settings
;;;;
(global-set-key (kbd "C-c t") 'transpose-mark)


;;;; Undo Tree Settings
;;;;
(global-undo-tree-mode)

;;; Keep region when undoing in region
(defadvice undo-tree-undo (around keep-region activate)
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
            (p (set-marker (make-marker) (point))))
        ad-do-it
        (goto-char p)
        (set-mark m)
        (set-marker p nil)
        (set-marker m nil))
    ad-do-it))
;; Avoid overriding the global key-swapping maps I have above.
(define-key undo-tree-map (kbd "C-_") nil)
(define-key undo-tree-map (kbd "M-_") nil)

;;;; Vsh-mode settings
;;;;
(setq vsh-find-file-function 'ido-find-file)
;; I introduced a special mapping for C-j to accomodate for the fact that I lost
;; it when defining two C-c keys.  I currently use this mapping to trigger
;; execution in the minibuffer.  I find it a little confusing that I have
;; different but similar mappings to "execute" between the eval-expression and
;; vsh modes.  Hence add this in vsh.
;; (Would *like* to remove the problem with the `eval-expression' thing, working
;; on it).
(with-eval-after-load 'vsh
  (define-key vsh-mode-map (kbd "C-j") 'vsh-execute-command))


;;;; Window Number Settings
;;;;
;; Colors
(setq window-number-active-background nil)
(setq window-number-active-foreground nil)
(setq window-number-inactive-foreground nil)
(setq window-number-inactive-background nil)

(window-number-mode 1)

;; Key bindings
(defun window-number-select-call (number)
  `(lambda ()
     (interactive)
     (window-number-select ,number)))

(dotimes (winnum 5)
  (define-key ctl-x-map
    (format "p%d" (1+ winnum))
    (window-number-select-call (1+ winnum))))


;;;; Xcscope Settings
;;;;
(add-hook 'c-mode-hook 'cscope-setup)
(add-hook 'c++-mode-hook 'cscope-setup)
