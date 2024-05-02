;;;; Remaps for Dvorak keyboard
;;;;
;;;; NOTE:
;;;;      While there are a lot of different modes that have mapped 'n' and 'p'
;;;;      to up and down in some way, I'm not going to change these for two
;;;;      reasons.
;;;;
;;;;      (1) When the mapping is on unmodified 'n' and 'p', the problem I'm
;;;;          trying to avoid of switching which hand is holding down the
;;;;          modifier key doesn't apply
;;;;      (2) It would be a hell of a lot of work to find every occurance of
;;;;          this particular mapping in every mode -- not worth the mapping
;;;;          consistency of having them remapped.
;;;;

(define-key goto-map "h" 'previous-error)

(defvar dvorak-keyswaps
  '(("M-p" . "M-h")
    ("M-h" . "M-p")
    ("C-M-p" . "C-M-h")
    ("C-M-h" . "C-M-p")
    ("C-S-p" . "C-S-h")
    ("C-S-h" . "C-S-p")
    ("C-'" . "C-x")
    ;; Just to be able to still use the C-j key
    ;; It's not available because I map it away below.
    ;; This ends up a little problematic, because while I have a way to press
    ;; C-j, I have no way to insert C-j -- that's handled by the
    ;; `minibuffer-local-map' binding at the bottom of this file.
    ;; One related thing is that `paredit' overwrites `RET' when in the
    ;; minibuffer eval mode which means that I need to press C-j in order to
    ;; "exec" the lisp.  Need to solve that in my paredit config (but mentioning
    ;; it here since I often come here when thinking about it).
    ("C-c j" . "C-j")))

(defvar dvorak-key-translations
  '((?\C-x . ?\C-z)
    (?\C-z . ?\C-x)
    (?\C-h . ?\C-p)
    (?\C-p . ?\C-h)
    (?\C-w . ?\C-c)
    (?\C-j . ?\C-c)
    (?\C-c . ?\C-w)))

(defun apply-my-keyswaps ()
  (dolist (key-pair dvorak-keyswaps)
    (define-key key-translation-map (kbd (car key-pair)) (kbd (cdr key-pair))))
  (dolist (key-characters dvorak-key-translations)
    (keyboard-translate (car key-characters) (cdr key-characters))))

(apply-my-keyswaps)

(add-hook 'after-make-frame-functions
          (lambda (f) (with-selected-frame f
                        (apply-my-keyswaps))))

(define-key minibuffer-local-map (kbd "C-c q")
            (lambda () (interactive) (insert-and-inherit 10)))
