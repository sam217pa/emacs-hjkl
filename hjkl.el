;;; hjkl.el --- Move in emacs using context dependent keybindings.

;; Copyright (C) 2017, Samuel Barreto

;; Author: Samuel Barreto <samuel.barreto8@gmail.com>
;; Maintainer: Samuel Barreto <samue.barreto8@gmail.com>
;; Keywords: editing
;; Version: 0.1
;; URL: TODO

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `hjkl` provides an easy way to navigate source files using context dependent
;; keybindings, _ie_ keybindings that trigger an action depending on characters
;; that follow or precede the point (cursor). The idea is to move swiftly by
;; semantically relevant units in a language agnostic way. You use `hjkl-up`,
;; `hjkl-down` and other keybindings to jump to the previous or next semantic
;; unit (a sexp, a paragraph for example) when point is after or behind a
;; regular expression.
;;
;; In a way, `hjkl` was born as the result of my frustration not being able to
;; use [lispy](https://github.com/abo-abo/lispy) keybindings in all major modes.
;;
;; ![hjkl](img/hjkl.gif)

;;; Installation:
;;
;; This module is currently in beta version and not (yet) available on MELPA.
;;
;; To install use
;;
;;	$ git clone https://github.com/sam217pa/emacs-hjkl
;;
;; and add the following `use-package` declaration into your `.emacs` file:
;;
;; ```elisp
;; (use-package hjkl
;;   :load-path "path/to/emacs-hjkl"
;;   :config
;;   (setq hjkl-up (kbd "K")) ; if k does not suits you.
;;   (hjkl-define-keys ...))
;; ```

;;; Usage:
;;
;; The following code tells emacs to jump to the next sentence when the cursor
;; is positioned after a period, an exclamation or question mark, or to jump to
;; the next outline heading if the cursor is before the value of
;; `outline-regexp', when the key defined by hjkl-down is pressed. Otherwise the
;; key calling hjkl-down acts normally.

;; ```elisp
;; (hjkl-define-keys
;;  '((:keymap text-mode-map
;;     :bind (:down (("\\.\\|?\\|!" #'forward-sentence back)
;;                   (outline-regexp #'outline-next-heading))))))
;; ```

;; It results into the evaluation of the following snippet:

;; ```elisp
;; (define-key text-mode-map hjkl-down
;;   (lambda () (interactive)
;;     (cond ((looking-back "\\.\\|?\\|!")
;;            (funcall #'forward-sentence))
;;           ((looking-at outline-regexp)
;;            (outline-next-heading))
;;           (t (self-insert-command 1)))))
;; ```

;; (More information on context-dependent keybindings can be found
;; [here](http://endlessparentheses.com/define-context-aware-keys-in-emacs.html).)

;;; hjkl Directions
;;
;; `hjkl` provides several keybindings by default, corresponding to
;; `hjkl-directions':
;;

;; - `:up`: bound to `k` by default, useful to goes up a semantic unit. It can
;;    be used to jump to the beginning of the previous paragraph, SEXP or
;;    outline-heading.

;; - `:down`: bound to `j` by default, it makes sense to bind function that does
;;    the opposite of those bound to `hjkl-up`.

;; - `:left` or `:right`: bound to `h` and `l` by default, useful to goes out of
;;    a semantic unit. It can be used to jump to the enclosing parent delimiter
;;    when the cursor is before a closing delimiter, or to hide the body of the
;;    current outline heading.

;; - `:jump`: bound to `a` by default, useful to jump to a specific location
;;    using [`a`vy](https://github.com/abo-abo/avy) for example.

;; - `:eval`: bound to `e` by default, used to evaluate SEXP or expression in a
;;    REPL. It is useful in emacs-lisp, ESS-modes, python and languages that
;;    interact with a REPL.

;; - `:mark`: bound to `m` by default, used to mark expressions, paragraphs and
;; - stuff like that.

;; - `:other`: bound to `o` by default, jump to the corresponding delimiter.

;;; Example config
;;

;;; Contributions
;;
;; I would really appreciate contributions as this is my first package. I am
;; sure it does some really wrong things, and that it can be improved a lot.
;; I'll be glad to have feedbacks on it !

;;; Disclaimer
;;
;; hjkl still feels a lot like a hack to me, but it does kinda work as I
;; expected, so I felt I would put it in the open. Maybe it will break things in
;; your config, but as the GPL says, it is distributed without any garantee … ;)

;;; Code:
;;
;; -*- lexical-binding: t -*-

(eval-when-compile
  (require 'cl))

(setq hjkl-left  (kbd "h"))
(setq hjkl-down  (kbd "j"))
(setq hjkl-up    (kbd "k"))
(setq hjkl-right (kbd "l"))
(setq hjkl-other (kbd "d"))
(setq hjkl-eval  (kbd "e"))
(setq hjkl-jump  (kbd "a"))
(setq hjkl-mark  (kbd "m"))

(defvar hjkl-directions-binding
  '(:up hjkl-up
    :down hjkl-down
    :left hjkl-left
    :right hjkl-right
    :other hjkl-other
    :jump hjkl-jump
    :eval hjkl-eval
    :mark hjkl-mark))

(defvar hjkl-directions
  (hjkl--plist-keys hjkl-directions-binding)
  "The list of directions that hjkl defines.

A list of example of what each keybinding does:


:up		goes up in a semantic unit.

:down	goes down in a semantic unit.

:left	the hjkl left direction, useful to get out of a sexp tree
		when looking at an opening delimiter for example, or down
		in the sexp tree when looking at a closing delimiter.

:right	the hjkl right direction, that usually does the opposite
		as hjkl-left.

:other	goes to the corresponding delimiter, like a corresponding
		opening square, round or curly bracket.

:jump	jump to a specific direction using avy.

:eval	evaluate last sexp, last paragraph or following sexp

:mark	mark the previous or following sexp
")

;; from https://www.emacswiki.org/emacs/mon-plist-utils.el
(defun hjkl--plist-keys (in-plist)
  "Extract keys from a plist."
  (if (null in-plist)
      in-plist
    (cons (car in-plist)
          (hjkl--plist-keys (cddr in-plist)))))

(defun hjkl--parse-keybinding (key-bind)
  "Generate the interactive anonymous function triggered by
hjkl-* keybindings."
  `(lambda ()
     (interactive)
     (cond ,@(mapcar
              #'hjkl--parse-re-def
              key-bind)
           (t (self-insert-command 1)))))

(defun hjkl--parse-re-def (re-def)
  "Generate the cond calls for each regular expression in RE-DEF."
  (let ((back (cl-third re-def))
        (re (cl-first re-def))
        (fn (cl-second re-def)))
    (if back
        `((looking-back ,re (point-at-bol))
          (funcall ,fn))
      `((looking-at ,re)
        (funcall ,fn)))))

(defun hjkl--define-key (keydef)
  "Generate the define-key call and evaluate them for each
direction in ``hjkl-directions-binding''."
  (let ((keymapvar (plist-get keydef :keymap))
        (keybindvar (plist-get keydef :bind)))
    (mapc #'eval
          (mapcar
           (lambda (dir)
             `(define-key
                ,keymapvar
                ,(plist-get hjkl-directions-binding dir)
                ,(hjkl--parse-keybinding (plist-get keybindvar dir))))
           (hjkl--plist-keys hjkl-directions-binding)))))

;;;###autoload
(defun hjkl-define-keys (key-definition)
  "Call `hjkl--define-key' on each sublist of KEY-DEFINITION.

KEY-DEFINITION should be a list of the form

	'((:keymap KEYMAP
   	   :bind (:DIRECTION ((RE FUNCTION-CALL BACK)))))

:keymap should be a valid emacs keymap.

:bind is mandatory, it corresponds to the list of condition to
check for each hjkl direction.

RE is a valid regular expression, passed to `looking-at' if BACK
is nil (the default) or `looking-back' if BACK is t.

FUNCTION-CALL can be a simple function call of the form
#'function or a lambda expression.

An example usage would be the following snippet:

	(hjkl-define-keys
	 '((:keymap text-mode-map
	    :bind (:up ((\"\\.\\|?\\|!\" (lambda ()
	                                   (backward-sentence)
	                                   (backward-char 1))
	                 back))
	           :down ((\"\\.\\|?\\|!\" #'forward-sentence back))))))

It bind `hjkl-up' to jump to the beginning of the previous
sentence when the cursor is after a `.`, a `?` or a `!` and
`hjkl-down' to go the next sentence. The \"back\" mention is
optional but indicate that the characters that matters precedes
the cursor.
"
  (mapc  #'hjkl--define-key key-definition))



;;;###autoload
(defun hjkl-update-keys ()
  "Update keybinding definition if ``hjkl-bindings'' is defined
by calling ``hjkl-define-keys'' on it."
  (interactive)
  (if (boundp 'hjkl-bindings)
      (hjkl-define-keys hjkl-bindings)
    (error "hjkl-bindings is not defined.")))


;; ---------- TESTS -------------------------------------------------------

(ert-deftest plist-parsing ()
  (should (equal (hjkl--plist-keys '(:a a :b b)) '(:a :b)))
  (should (equal (hjkl--plist-keys '(:a a :b a :b)) '(:a :b :b)))
  (should (equal (hjkl--plist-keys '(:a a :b b b b)) '(:a :b b))))

(ert-deftest re-def-parsing ()
  (should
   (equal (hjkl--parse-re-def '(";;;" (lambda () (message "cuicui")) t))
          '((looking-back ";;;" (point-at-bol)) (funcall (lambda nil (message "cuicui"))))))
  (should
   (equal (hjkl--parse-re-def '(";;;" (lambda () (message "cuicui"))))
          '((looking-at ";;;") (funcall (lambda nil (message "cuicui")))))))

(ert-deftest generate-lambda-for-direction ()
  (should
   (equal
    (hjkl--parse-keybinding '((")" #'forward-list t)))
    '(lambda nil
       (interactive)
       (cond
        ((looking-back ")" (point-at-bol))
         (funcall #'forward-list))
        (t
         (self-insert-command 1))))))
  (should
   (equal
    (hjkl--parse-keybinding '(("(" #'forward-list)))
    '(lambda nil
       (interactive)
       (cond
        ((looking-at "(")
         (funcall #'forward-list))
        (t
         (self-insert-command 1)))))))

(provide 'hjkl)

;;; hjkl.el ends here.
