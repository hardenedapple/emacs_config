(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(connection-local-criteria-alist
   '(((:application tramp :protocol "kubernetes")
      tramp-kubernetes-connection-local-default-profile)
     ((:application tramp) tramp-connection-local-default-system-profile
      tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((tramp-kubernetes-connection-local-default-profile
      (tramp-config-check . tramp-kubernetes--current-context-data)
      (tramp-extra-expand-args 97
                               (tramp-kubernetes--container
                                (car tramp-current-connection))
                               104
                               (tramp-kubernetes--pod
                                (car tramp-current-connection))
                               120
                               (tramp-kubernetes--context-namespace
                                (car tramp-current-connection))))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
                                        "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o" "state=abcde" "-o"
                                        "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number) (euid . number)
                                          (user . string) (egid . number)
                                          (comm . 52) (state . 5)
                                          (ppid . number) (pgrp . number)
                                          (sess . number) (ttname . string)
                                          (tpgid . number) (minflt . number)
                                          (majflt . number)
                                          (time . tramp-ps-time) (pri . number)
                                          (nice . number) (vsize . number)
                                          (rss . number) (etime . tramp-ps-time)
                                          (pcpu . number) (pmem . number) (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o"
                                        "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o" "stat=abcde" "-o"
                                        "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format (pid . number) (user . string)
                                          (group . string) (comm . 52)
                                          (state . 5) (ppid . number)
                                          (pgrp . number) (ttname . string)
                                          (time . tramp-ps-time) (nice . number)
                                          (etime . tramp-ps-time) (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
                                        "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o"
                                        "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number) (euid . number)
                                          (user . string) (egid . number)
                                          (group . string) (comm . 52)
                                          (state . string) (ppid . number)
                                          (pgrp . number) (sess . number)
                                          (ttname . string) (tpgid . number)
                                          (minflt . number) (majflt . number)
                                          (time . tramp-ps-time) (pri . number)
                                          (nice . number) (vsize . number)
                                          (rss . number) (etime . number)
                                          (pcpu . number) (pmem . number) (args)))
     (tramp-connection-local-default-shell-profile (shell-file-name . "/bin/sh")
                                                   (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile (path-separator . ":")
                                                    (null-device . "/dev/null"))))
 '(custom-safe-themes
   '("763bf89898a06b03f7b65fbc29857a1c292e4350246093702fdbd6c4e46e2cf0"
     "37c8c2817010e59734fe1f9302a7e6a2b5e8cc648cf6a6cc8b85f3bf17fececf"
     "78e6be576f4a526d212d5f9a8798e5706990216e9be10174e3f3b015b8662e27"
     "a800120841da457aa2f86b98fb9fd8df8ba682cebde033d7dbf8077c1b7d677a"
     "1fc1fdf975c8c8c3767c29787a063eee50cbceef903644a0771fa66568ee8777"
     "c567c85efdb584afa78a1e45a6ca475f5b55f642dfcd6277050043a568d1ac6f"
     "38ba6a938d67a452aeb1dada9d7cdeca4d9f18114e9fc8ed2b972573138d4664"
     "196cc00960232cfc7e74f4e95a94a5977cb16fd28ba7282195338f68c84058ec"
     "30b7087fdd149a523aa614568dc6bacfab884145f4a67d64c80d6011d4c90837"
     "05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a"
     "4e262566c3d57706c70e403d440146a5440de056dfaeb3062f004da1711d83fc" default))
 '(ibuffer-saved-filter-groups '(("Split by files" ("with files" (filename . ".*")))))
 '(ibuffer-saved-filters
   '(("gnus"
      ((or (mode . message-mode) (mode . mail-mode) (mode . gnus-group-mode)
           (mode . gnus-summary-mode) (mode . gnus-article-mode))))
     ("programming"
      ((or (mode . emacs-lisp-mode) (mode . cperl-mode) (mode . c-mode)
           (mode . java-mode) (mode . idl-mode) (mode . lisp-mode)
           (mode . python-mode))))))
 '(package-selected-packages
   '(avy buffer-move c-eldoc cmake-mode dot-mode editorconfig elisp-slime-nav
         expand-region feature-mode goto-chg graphviz-dot-mode jump-char keyswap
         lua-mode magit markdown-mode monokai-theme move-text multiple-cursors
         notmuch paredit pueue register-list rust-mode slime smart-window
         transpose-frame transpose-mark undo-tree vimrc-mode vsh-mode
         window-number wrap-region xcscope yasnippet yasnippet-snippets))
 '(safe-local-variable-values
   '((epa-file-encript-to "hardenedapple@gmail.com") (Base . 10)
     (Package . LET-OVER-LAMBDA) (Syntax . COMMON-LISP)))
 '(send-mail-function 'sendmail-send-it))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
