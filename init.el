;;; Setup packages
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.gnu.org/nongnu/")))

;; Things to improve:
;;  - LSP
;;    - Which should influence both the below.
;;    - Can it also add `eldoc` like functionality?
;;      - Yes, and it naturally ties in with `eldoc', which means I don't have
;;        to do very much to make this all work.
;;  - Folding
;;    - Probably origami -- not sure if anything else around.
;;      - Currently using hideshow (`hs-show-block' etc).
;;  - Completion
;;    - Don't yet know what the options are, would like to avoid installing a
;;      package if possible.
;;    - eglot does make this pretty nice out of the box.

;; Packages that I want to ensure are downloaded.
;; Point of this is to ensure setting emacs up on a new machine is very easy.
(setq custom-file "~/.emacs.d/customize.el")
(load custom-file)
;; First level approximation of whether this is a fresh install or not.
(unless (package-installed-p (car package-selected-packages))
  (package-refresh-contents))
(package-install-selected-packages t)

;;; I keep single file packages in this directory
(add-to-list 'load-path "~/.emacs.d/packages/")

(dolist (p '(
             desktop ;; <-- required so that nameses can be required.
             nameses ;; <-- No autoload
             splice-window ;; <-- Required as not installed as package.
             slime  ;; <-- autoload doesn't seem to work?
             keyswap ;; <-- I want to add as a hook.
                     ;;     (can't use autoload because that would add as hook
                     ;;     for everyone using this package).
             window-number ;; <-- autoload doesn't seem to work.
             ))
  (require p))

;;; Load all files in the directory config
;;; Name of file denotes order it's loaded in.
;;; Note order matters in way here:
;;;    wrap-region after paredit to not overwrite '('
(dolist (conf-file (directory-files "~/.emacs.d/config/" t "^[^.].+\\.elc?$"))
  (load conf-file))

;;; Remove some settings from MINOR-MODE-ALIST
;;;
(defvar minor-mode-show-list
  (list
   'compilation-in-progress
   'diff-minor-mode
   'overwrite-mode
   'undo-tree-visualizer-selection-mode
   'window-number-mode
   )
  "List of minor-modes to show in modeline")
;; Would be nice to understand why this doesn't happen late enough.  A bunch of
;; other elements are left in the `minor-mode-alist' even though this code seems
;; to work fine.  Hence I believe we run this then later add all those things to
;; the list.  Don't know when or where.
(setq minor-mode-alist
      (cl-remove-if-not
       (lambda (val)
         (member (car val) minor-mode-show-list))
       minor-mode-alist))
