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
    ("C-c j" . "C-j")))

(defvar dvorak-key-translations
  '((?\C-x . ?\C-z)
    (?\C-z . ?\C-x)
    (?\C-h . ?\C-p)
    (?\C-p . ?\C-h)
    (?\C-w . ?\C-c)
    (?\C-j . ?\C-c)
    (?\C-c . ?\C-w)))

(defvar halmak-keyswaps
  '(("M-p" . "M-h")
    ("M-h" . "M-p")
    ("C-M-p" . "C-M-h")
    ("C-M-h" . "C-M-p")
    ("C-S-p" . "C-S-h")
    ("C-S-h" . "C-S-p")
    ("C-'" . "C-v")
    ;; Just to be able to still use the C-j key
    ;; It's not available because I map it away below.
    ("C-c j" . "C-j")))

(defvar halmak-key-translations
  '((?\C-v . ?\C-x)
    (?\C-h . ?\C-p)
    (?\C-p . ?\C-c)
    (?\C-j . ?\C-h)))

(defun apply-dvorak-keyswaps ()
  (dolist (key-pair dvorak-keyswaps)
    (define-key key-translation-map (kbd (car key-pair)) (kbd (cdr key-pair))))
  (dolist (key-characters dvorak-key-translations)
    (keyboard-translate (car key-characters) (cdr key-characters))))

(defun apply-halmak-keyswaps ()
  ;; We're lucky that the halmak keys I swap here are all *from* the same key.
  ;; It means there will be no key "left over" mapped to 'nil in our keymap
  ;; when switching between the two.
  (dolist (key-pair halmak-keyswaps)
    (define-key key-translation-map (kbd (car key-pair)) (kbd (cdr key-pair))))
  (dolist (key-characters halmak-key-translations)
    (keyboard-translate (car key-characters) (cdr key-characters))))

(cl-defun remove-personal-keyswaps (&optional (translations dvorak-key-translations))
  (dolist (key-characters translations)
    (keyboard-translate (car key-characters) 'nil)))

(defun switch-to-halmak ()
  (interactive)
  (remove-personal-keyswaps)
  (apply-halmak-keyswaps))

(apply-dvorak-keyswaps)

(add-hook 'after-make-frame-functions
          (lambda (f) (with-selected-frame f
                        (apply-dvorak-keyswaps))))
