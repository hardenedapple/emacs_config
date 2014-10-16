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
