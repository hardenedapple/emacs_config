#!/bin/bash

nohup emacs --no-init-file --no-site-file --eval='(load "~/.emacs.d/slime_only.el")' > /dev/null 2>&1 &
disown
