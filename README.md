## hjkl.el
*Move in emacs using context dependent keybindings.*

---
[![License GPLv3](https://img.shields.io/badge/license-GPL_v3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

`hjkl` provides an easy way to navigate source files using context dependent
keybindings, _ie_ keybindings that trigger an action depending on characters
that follow or precede the point (cursor). The idea is to move swiftly by
semantically relevant units in a language agnostic way. You use `hjkl-up`,
`hjkl-down` and other keybindings to jump to the previous or next semantic
unit (a sexp, a paragraph for example) when point is after or behind a
regular expression.

In a way, `hjkl` was born as the result of my frustration not being able to
use [lispy](https://github.com/abo-abo/lispy) keybindings in all major modes.

![hjkl](img/hjkl.gif)

### Installation


This module is currently in beta version and not (yet) available on MELPA.

To install use

	$ git clone https://github.com/sam217pa/emacs-hjkl

and add the following `use-package` declaration into your `.emacs` file:

```elisp
(use-package hjkl
  :load-path "path/to/emacs-hjkl"
  :config
  (setq hjkl-up (kbd "K")) ; if k does not suits you.
  (hjkl-define-keys ...))
```

### Usage


The following code tells emacs to jump to the next sentence when the cursor
is positioned after a period, an exclamation or question mark, or to jump to
the next outline heading if the cursor is before the value of
`outline-regexp`, when the key defined by hjkl-down is pressed. Otherwise the
key calling hjkl-down acts normally.

```elisp
(hjkl-define-keys
 '((:keymap text-mode-map
    :bind (:down (("\\.\\|?\\|!" #'forward-sentence back)
                  (outline-regexp #'outline-next-heading))))))
```

It results into the evaluation of the following snippet:

```elisp
(define-key text-mode-map hjkl-down
  (lambda () (interactive)
    (cond ((looking-back "\\.\\|?\\|!")
           (funcall #'forward-sentence))
          ((looking-at outline-regexp)
           (outline-next-heading))
          (t (self-insert-command 1)))))
```

(More information on context-dependent keybindings can be found
[here](http://endlessparentheses.com/define-context-aware-keys-in-emacs.html).)

### hjkl Directions


`hjkl` provides several keybindings by default, corresponding to
`hjkl-directions`:


- `:up`: bound to `k` by default, useful to goes up a semantic unit. It can
   be used to jump to the beginning of the previous paragraph, SEXP or
   outline-heading.

- `:down`: bound to `j` by default, it makes sense to bind function that does
   the opposite of those bound to `hjkl-up`.

- `:left` or `:right`: bound to `h` and `l` by default, useful to goes out of
   a semantic unit. It can be used to jump to the enclosing parent delimiter
   when the cursor is before a closing delimiter, or to hide the body of the
   current outline heading.

- `:jump`: bound to `a` by default, useful to jump to a specific location
   using [`a`vy](https://github.com/abo-abo/avy) for example.

- `:eval`: bound to `e` by default, used to evaluate SEXP or expression in a
   REPL. It is useful in emacs-lisp, ESS-modes, python and languages that
   interact with a REPL.

- `:mark`: bound to `m` by default, used to mark expressions, paragraphs and
- stuff like that.

- `:other`: bound to `o` by default, jump to the corresponding delimiter.

### Example config



### Contributions


I would really appreciate contributions as this is my first package. I am
sure it does some really wrong things, and that it can be improved a lot.
I'll be glad to have feedbacks on it !

### Disclaimer


hjkl still feels a lot like a hack to me, but it does kinda work as I
expected, so I felt I would put it in the open. Maybe it will break things in
your config, but as the GPL says, it is distributed without any garantee … ;)

### Function Documentation


#### `(hjkl-define-keys KEY-DEFINITION)`

Call ‘hjkl--define-key’ on each sublist of KEY-DEFINITION.

KEY-DEFINITION should be a list of the form

	’((:keymap KEYMAP
   	   :bind (:DIRECTION ((RE FUNCTION-CALL BACK)))))

:keymap should be a valid emacs keymap.

:bind is mandatory, it corresponds to the list of condition to
check for each hjkl direction.

RE is a valid regular expression, passed to ‘looking-at’ if BACK
is nil (the default) or ‘looking-back’ if BACK is t.

FUNCTION-CALL can be a simple function call of the form
#’function or a lambda expression.

An example usage would be the following snippet:

	(hjkl-define-keys
	 ’((:keymap text-mode-map
	    :bind (:up (("\.\|?\|!" (lambda ()
	                                   (backward-sentence)
	                                   (backward-char 1))
	                 back))
	           :down (("\.\|?\|!" #’forward-sentence back))))))

It bind ‘hjkl-up’ to jump to the beginning of the previous
sentence when the cursor is after a ‘.‘, a ‘?‘ or a ‘!‘ and
‘hjkl-down’ to go the next sentence. The "back" mention is
optional but indicate that the characters that matters precedes
the cursor.


#### `(hjkl-update-keys)`

Update keybinding definition if ‘‘hjkl-bindings’’ is defined
by calling ‘‘hjkl-define-keys’’ on it.

-----
<div style="padding-top:15px;color: #d0d0d0;">
Markdown README file generated by
<a href="https://github.com/mgalgs/make-readme-markdown">make-readme-markdown.el</a>
</div>
