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

;;; Using M-f and M-b for word motion is a pain, add M-a and M-e for word motion
(global-set-key (kbd "M-a") 'subword-backward)
(global-set-key (kbd "M-e") 'subword-forward)

(defvar dvorak-keyswaps
  '(("C-h" . "C-p")
    ("C-p" . "C-h")
    ("M-p" . "M-h")
    ("M-h" . "M-p")
    ("C-M-p" . "C-M-h")
    ("C-M-h" . "C-M-p")
    ("C-S-p" . "C-S-h")
    ("C-S-h" . "C-S-p")
    ("C-z" . "C-x")
    ("C-'" . "C-x")
    ("C-x" . "C-j")
    ("C-j" . "C-c")
    ("C-w" . "C-c")
    ("C-c" . "C-w")))

(defun apply-my-keyswaps ()
  (dolist (key-pair dvorak-keyswaps)
    (define-key key-translation-map (kbd (car key-pair)) (kbd (cdr key-pair)))))

(apply-my-keyswaps)

(add-hook 'after-make-frame-functions
          (lambda (f) (with-selected-frame f
                        (apply-my-keyswaps))))

;;;; Org mode Dvorak settings
;;;
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "M-a") nil)
            (define-key org-mode-map (kbd "M-e") nil)))
