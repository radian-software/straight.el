EMACS ?= emacs

ELS := $(sort $(wildcard *.el))
ELCS := $(ELS:.el=.elc)

.PHONY: all compile checkdoc longlines toc clean travis

all: compile checkdoc longlines

compile: $(ELCS)

%.elc: %.el
	@printf "Compiling $<\n"
	@! $(EMACS) -Q --batch -L . --eval "(setq byte-compile-error-on-warn t)" \
	-f batch-byte-compile $< 2>&1 \
	| grep -E "^$<:[0-9]+:[0-9]+:(Warning|Error): "

checkdoc:
	@printf "Checkdoc...\n"
	@! $(EMACS) -Q --batch \
	--eval "(or (fboundp 'checkdoc-file) (kill-emacs))" \
	--eval "(setq sentence-end-double-space nil)" \
	--eval '(checkdoc-file "straight.el")' 2>&1 | grep .

longlines: $(ELS) README.md Makefile .travis.yml
	@printf "longlines...\n"
	@! echo "$^" | xargs sed '/<!-- toc -->/,/<!-- tocstop -->/d' \
	| sed '/longlines-start/,/longlines-stop/d' \
	| grep -E '.{80}'                           \
	| grep -E -v '\[.+\]: (#|http)'             \
	| sed 's/^/long line: /' | grep .

toc:
	markdown-toc -i README.md

clean:
	@printf "Cleaning...\n"
	@rm -f *.elc

travis: compile checkdoc longlines
	@mkdir -p ~/.emacs.d/straight/repos/
	@ln -s $(PWD) ~/.emacs.d/straight/repos/
	$(EMACS) --batch -l ~/.emacs.d/straight/repos/straight.el/bootstrap.el \
	--eval "(straight-use-package 'use-package)" \
	--eval "(use-package el-patch :straight t)"
