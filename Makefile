README.md: make-readme-markdown.el hjkl.el
	emacs --script $< < hjkl.el >$@ 2>/dev/null

make-readme-markdown.el:
	wget -q -O $@ https://raw.github.com/mgalgs/make-readme-markdown/master/make-readme-markdown.el

.INTERMEDIATE: make-readme-markdown.el
