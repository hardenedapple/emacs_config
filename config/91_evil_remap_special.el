;; Vim keyboard items that would usually start some kind of change
;; d  c  s  i  a  u  o  x  r  . p v m < >

(defun remove-mappings (mappings map)
  (dolist (key mappings)
    (define-key map key nil)))

;;; Help Mode evil mappings
(evil-define-key 'motion help-mode-map
  "o" 'help-go-back
  "i" 'help-go-forward
  "u" 'View-scroll-half-page-forward
  "d" 'View-scroll-half-page-backward
  (kbd "C-l") 'revert-buffer
  (kbd "<tab>")'forward-button
  )


;;; Info Mode evil mappings
;; Make an equivalent function for `Info-nth-menu-item'
(defun Info-nth-menu-item-modified (arg)
  "Go to the node of the Nth menu item.
N here is ARG given in the argument instead of the key used to
run this command."
  (interactive "p")
  (Info-goto-node (Info-extract-menu-counting arg)))

(evil-define-key 'motion Info-mode-map
  "o" 'Info-history-back
  "i" 'Info-history-forward
  "O" 'Info-history
  "d" 'Info-scroll-up
  "u" 'Info-scroll-down
  (kbd "C-n") 'Info-next
  (kbd "C-p") 'Info-prev
  "c" 'Info-toc
  "D" 'Info-directory
  "^" 'Info-up
  (kbd "C-]") 'Info-follow-reference
  "<return>" 'Info-follow-nearest-node
  "gi" 'Info-nth-menu-item-modified
  "gn" 'Info-goto-node
  "yc" 'Info-copy-current-node-name
  "S" 'Info-search
  "x" 'Info-index
  "X" 'Info-virtual-index
  "a" 'Info-index-next
  )

;; Remove troublesome Info-mode mappings
(remove-mappings (list "g"  "n"  "p"  "w"  ",") Info-mode-map)
(dotimes (digit 9)
  (define-key Info-mode-map (format "%d" (1+ digit)) nil))


;;; Man Mode evil mappings
(evil-define-key 'motion Man-mode-map
  "]]" 'Man-next-section
  "[[" 'Man-previous-section
  (kbd "C-n") 'Man-next-section
  (kbd "C-p") 'Man-previous-section
  "gs" 'Man-goto-section
  "ga" 'Man-goto-see-also-section
  "gr" 'Man-follow-manual-reference
  (kbd "C-l") 'Man-update-manpage
  )

(add-hook 'Man-mode-hook
          (lambda ()
            (remove-mappings (list "g" "n" "?" " ") Man-mode-map)))


;;; Apropos mode
(evil-define-key 'motion apropos-mode-map
  (kbd "<tab>") 'forward-button
  (kbd "C-l") 'revert-buffer)
(add-hook 'apropos-mode-hook
          (lambda ()
            (remove-mappings (list "g" " ") apropos-mode-map)))


;;; Buffer menu mode
(evil-define-key 'motion Buffer-menu-mode-map
  (kbd "C-l") 'revert-buffer
  "P" 'Buffer-menu-switch-other-window
  "O" 'Buffer-menu-1-window
  "p" 'Buffer-menu-2-window
  "r" 'Buffer-menu-execute
  "x" 'Buffer-menu-delete-backwards
  "D" 'Buffer-menu-bury
  )

(remove-mappings (list (kbd "C-k") (kbd "C-o") "1" "2" "t" "T" "g" "b") Buffer-menu-mode-map)


;;; Diff Mode
(evil-define-key 'normal diff-mode-map
  "q" 'View-quit
  (kbd "C-l") 'revert-buffer
  "]]" 'diff-hunk-next
  "[[" 'diff-hunk-prev
  (kbd "C-n") 'diff-hunk-next
  (kbd "C-p") 'diff-hunk-prev)

(remove-mappings (list "n" "g" "?" "k") diff-mode-map)
(dotimes (digit 9)
  (define-key diff-mode-map (format "%d" (1+ digit)) nil))
