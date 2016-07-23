;;; keyswap-mode.el -- swap key bindings (toggle programmers keyboard version)

;; Copyright (C) 2016 Matthew Malcomson


;; Author: Matthew Malcomson <hardenedapple@gmail.com>
;; Maintainer: Matthew Malcomson <hardenedapple@gmail.com>
;; Version: 0.1.0
;; Package-Version: 20160722.2100
;; Keywords: speed, convenience
;; URL: http://github.com/hardenedapple/keyswap-mode
;; TODO -- Add 'cl as a package requirement -- not sure about the syntax at the
;; moment
;; Package-Requires: ()

;; This file is NOT part of GNU Emacs.


;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:

;; keyswap-mode is a minor mode that allows swapping the commands of two keys.
;; It comes with a default set of keys to swap of the number keys and their
;; shifted counterparts along with the '-' and '_' key.
;; This is generally useful in programming languages where symbols are more
;; often used than numbers.
;;
;; To use keyswap-mode, make sure this file is in the Emacs load-path:
;;   (add-to-list 'load-path "/path/to/directory/or/file")
;;
;; Then require keyswap-mode:
;;   (require 'keyswap-mode)

;; To toggle between swapped and not-swapped sets of keys, use the command
;; (keyswap-mode) or M-x keyswap-mode'



;;; Code:

(eval-when-compile (require 'cl))

(defconst command-event-wrapper-marker "CHAR COMMAND WRAPPER ")

;; `lexical-let' my current understanding of it:
;;   It creates a symbol that is not interned in the global scope.
;;   It assigns the value of this symbol to the value of the symbol that we are
;;   lexically capturing.
;;   It modifies any lambda function it is given to take an extra number of
;;   arguments for each of the captured lexical variables.
;;   It returns an enclosing lambda fnuction that takes arguments for the
;;   original function into a &rest list, and runs `apply' on the modified
;;   lambda function above.
;;   The arguments passed with this `apply' call are the unbound symbol and the
;;   &rest parameter.
;;   This unbound symbol cannot be overrided from outside because we have no way
;;   of accessing it.
;;   It is hence called a hidden symbol.
(defun equivalent-current-binding (key)
  "NOTE -- this function is broken but useful.

At the moment I can't find a way to fix it, but I'm using it with
all its kludges anyway.

Finds the command that is run when `key' is pressed.

If this commands `documentation' starts with
`command-event-wrapper-marker' assume it is a wrapper previously
created by this function.
Then return the result of (command nil t).

Otherwise create a `lambda' function that runs that command under
the false environment where `last-command-event' is KEY"
  (let ((current-binding (key-binding key)))
    (if (and (listp current-binding)
             (string-prefix-p command-event-wrapper-marker
                              (documentation current-binding)))
        (funcall current-binding nil t)
      ;; `lexical-let' uses `cl--function-convert' which wraps any lambda functions
      ;; in another lambda function, passing in arguments to pretend that we're
      ;; using lexical variables.
      ;; `cl--function-convert' uses a special case for docstrings and (interactive)
      ;; forms to bring them outside the `lambda' form being enclosed.
      ;; This special case doesn't work unless the docstring is recogniseable as a
      ;; string at compile time.
      ;; Hence I'm separating creating the form and evaluating it into two steps
      ;; so that I can create the form with a string literal for
      ;; `cl--function-convert' to recognise.
      ;; That's the reason for the `eval' layer of indirection.
      (let ((docstring (format (concat command-event-wrapper-marker "\"%c\"")
                               (aref key 0))))
        (eval
         `(lexical-let ((current-key (aref key 0)) (old-binding current-binding))
            (lambda (&optional arg return-command)
              ,docstring
              (interactive "p")
              (if return-command
                  old-binding
                (let ((last-command-event current-key))
                  (call-interactively old-binding))))))))))

(defun swap-these-keys (left-key right-key keymap)
  "Puts alternate bindings of LEFT-KEY and RIGHT-KEY into KEYMAP.

LEFT-KEY and RIGHT-KEY should be two objects valid in a call to
`key-binding'.  Makes a new binding in KEYMAP or the local
keymap, as used by `local-set-key'.

If a mapping has a normal function, we bind the other key to a
`lambda' function that calls the original with a masked
`last-command-event' pretending to be the first key.

These `lambda' functions are marked by their documentation
string (*very* hacky -- I know), and if this documentation string
is noticed, they are called with arguments so they return their
wrapped function.

If KEYMAP is defined, binds keys in that map, else uses
`current-local-map'"
  (let ((left-function (equivalent-current-binding left-key))
        (right-function (equivalent-current-binding right-key)))
    (define-key keymap left-key right-function)
    (define-key keymap right-key left-function)))

(defun swapped-keymap ()
  "Create a swapped keymap for this buffer.
Take the keys currently active, and create a keymap that takes
inverts the bindings of those key pairs in `keyswap-pairs'.
Returns the resulting keymap with these bindings, but doesn't do
anything other than create and return the keymap."
  (let ((return-map (make-sparse-keymap)))
    (dolist (key-pair keyswap-pairs return-map)
      (swap-these-keys (vector (car key-pair))
                       (vector (cdr key-pair))
                       return-map))))

(defvar-local keyswap-pairs
  (list '(?1 . ?!) '(?2 . ?@) '(?3 . ?#) '(?4 . ?$) '(?5 . ?%)
        '(?6 . ?^) '(?7 . ?&) '(?8 . ?*) '(?9 . ?\() '(?0 . ?\))
        '(?- . ?_))
  "Pairs of characters to swap when calling the `toggle-shifted-keys' function.")

(defun keyswap-update-keys ()
  "Update the buffer-local keymap currently used for `keyswap-mode'"
  (when (assoc 'keyswap-mode minor-mode-overriding-map-alist)
    (let ((currently-on keyswap-mode))
      (when currently-on (keyswap-mode 0))
      (setf (cdr (assoc 'keyswap-mode minor-mode-overriding-map-alist))
            (swapped-keymap))
      (when currently-on (keyswap-mode t)))))

(define-minor-mode keyswap-mode
  "Minor mode for programming where number keys are swapped with their shifted
counterparts.

This effectively makes the keyboard a \"programmers\" version of the keyboard.

In order to to have a different set of keys swapped for each
buffer, I abuse `minor-mode-overriding-map-alist' and never
actually have a minor mode map in the main `minor-mode-map-alist'
variable.

When this mode is activated, it first checks whether the current
buffer already has a local overriding keymap in
`minor-mode-overriding-map-alist', and if so does nothing but
activate that keymap.

If there is no relevant keymap in
`minor-mode-overriding-map-alist' it finds the mappings in the
`current-buffer' that relate to the keys to be swapped in
`keyswap-pairs', creates a keymap that has functionaly swapped
these keys, and stores that as the keymap in `minor-mode-overriding-map-alist'
to be used in all future invocations of this minor-mode.

When using this minor-mode along with others there are a few things to watch out
for.
First off, if this minor mode is activated before others that change the current
"
  nil
  " keyswap"
  nil
  ;; Body is executed every time the mode is toggled
  ;; If keyswap-mode is not in the `minor-mode-overriding-map-alist' variable,
  ;; then create a new map with `swapped-keymap', and add that to the list of
  ;; `minor-mode-overriding-map-alist'.
  (unless (assoc 'keyswap-mode minor-mode-overriding-map-alist)
    (push (cons 'keyswap-mode (swapped-keymap)) minor-mode-overriding-map-alist)))

(defun keyswap-include-braces ()
  "Hook so `toggle-shifted-keys' includes {,[, and },]"
  (setq-local keyswap-pairs
              (append keyswap-pairs '((?\[ . ?\{) (?\] . ?\}))))
  (keyswap-update-keys))

(defun keyswap-include-quotes ()
  "Hook so `toggle-shifted-keys' includes \" and '"
  (setq-local keyswap-pairs
              (append keyswap-pairs '((?\' . ?\"))))
  (keyswap-update-keys))

(defun keyswap-tac-underscore-exception ()
  "Hook so `toggle-shifted-keys' ignores - and _"
  (setq-local keyswap-pairs
              (remove '(?- . ?_) keyswap-pairs))
  (keyswap-update-keys))

(provide 'keyswap-mode)
