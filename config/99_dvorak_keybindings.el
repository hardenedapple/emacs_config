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

(global-set-key (kbd "M-g h") 'previous-error)

(defvar dvorak-keyswaps
  '(("M-p" . "M-h")
    ("M-h" . "M-p")
    ("C-M-p" . "C-M-h")
    ("C-M-h" . "C-M-p")
    ("C-S-p" . "C-S-h")
    ("C-S-h" . "C-S-p")
    ("C-'" . "C-x")))

(defvar dvorak-key-translations
  '((?\C-x . ?\C-z)
    (?\C-z . ?\C-x)
    (?\C-h . ?\C-p)
    (?\C-p . ?\C-h)
    (?\C-j . ?\C-c)))

(defun apply-my-keyswaps ()
  (dolist (key-pair dvorak-keyswaps)
    (define-key key-translation-map (kbd (car key-pair)) (kbd (cdr key-pair))))
  (dolist (key-characters dvorak-key-translations)
    (keyboard-translate (car key-characters) (cdr key-characters))))

(apply-my-keyswaps)

(add-hook 'after-make-frame-functions
          (lambda (f) (with-selected-frame f
                        (apply-my-keyswaps))))
