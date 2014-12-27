;;;; C Settings
;;;;
(setq c-default-style "linux"
      c-basic-offset 4)
(c-set-offset 'case-label '+)

;;;; Latex Settings
;;;;
(add-hook 'latex-mode-hook
          (lambda ()
            (set-fill-column 125)))  ; usually only write latex on large screens


;;;; Lisp Settings
;;;;
(setq inferior-lisp-program "/usr/bin/sbcl")


;;;; Python Settings
;;;;
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

;;;; Scheme Settings
;; I'm using guile at the moment
(setq scheme-program-name "guile")
;;; Want to add the mapping to complete symmetry with C-x C-e and C-' C-e
;;; Unfortunately, at the moment I can't be bothered doing that right now, as
;;; there doesn't seem to be a scheme-mode-map.
;; (add-hook 'scheme-mode-hook
;;           (lambda ()
;;             (define-key WHATEVER-MAP (kbd "C-' C-e") 'scheme-send-last-sexp)))
