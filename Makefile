SHELL := bash

EMACS ?= emacs

# The order is important for compilation.
for_compile := straight.el bootstrap.el install.el straight-x.el
for_checkdoc := straight.el
for_longlines := $(wildcard *.el *.md *.yml) Makefile
for_checkindent := $(wildcard *.el)

.PHONY: all
all: compile checkdoc toc longlines

.PHONY: compile
compile:
	@for file in $(for_compile); do \
	    echo "[compile] $$file" ;\
	    $(EMACS) -Q --batch -L . -f batch-byte-compile $$file 2>&1 \
	        | grep -v "^Wrote" \
	        | grep . && exit 1 || true ;\
	done

.PHONY: checkdoc
checkdoc:
	@for file in $(for_checkdoc); do \
	    echo "[checkdoc] $$file" ;\
	    $(EMACS) -Q --batch \
	        --eval "(or (fboundp 'checkdoc-file) (kill-emacs))" \
	        --eval "(setq sentence-end-double-space nil)" \
	        --eval "(checkdoc-file \"$$file\")" 2>&1 \
	        | grep . && exit 1 || true ;\
	done

.PHONY: longlines
longlines:
	@echo "[longlines] $(for_longlines)"
	@for file in $(for_longlines); do \
	    cat "$$file" \
	        | sed '/[<]!-- toc -->/,/<!-- tocstop -->/d' \
	        | sed '/[l]onglines-start/,/longlines-stop/d' \
	        | grep -E '.{80}' \
	        | grep -E -v '\[.+\]: (#|http)' \
	        | sed "s/^/$$file:long line: /" \
	        | grep . && exit 1 || true ;\
	done

.PHONY: checkindent
checkindent:
	@echo "[checkindent] $(for_checkindent)"
	@tmpdir="$$(mktemp -d)"; for file in $(for_checkindent); do \
	    $(EMACS) -Q --batch \
	        --eval "(setq inhibit-message t)" \
	        --eval "(load (expand-file-name \"indent.el\"  ) nil t)" \
	        --eval "(load (expand-file-name \"straight.el\") nil t)" \
	        --eval "(find-file \"$$file\")" \
	        --eval "(indent-region (point-min) (point-max))" \
	        --eval "(write-file \"$$tmpdir/$$file\")"; \
	    (diff <(cat          "$$file" | nl -v1 -ba | \
                           sed "s/\t/: /" | sed "s/^ */$$file:/") \
	          <(cat "$$tmpdir/$$file" | nl -v1 -ba | \
                           sed "s/\t/: /" | sed "s/^ */$$file:/") \
	         && exit 1 || true) | grep -F ">" | grep -o "[a-z].*"; \
	done

.PHONY: toc
toc: README.md
	@echo "[toc] $^"
	@if command -v markdown-toc >/dev/null; then \
	    markdown-toc -i $^ ; \
	else \
	    echo "  --> markdown-toc missing, skipping" ; \
	fi

.PHONY: clean
clean:
	@echo "[clean]" *.elc
	@rm -f *.elc

# Make sure to test with a package that supports Emacs 24.4 here.
travis: compile checkdoc longlines checkindent
	mkdir -p ~/.emacs.d/straight/repos/
	ln -s $(PWD) ~/.emacs.d/straight/repos/
	$(EMACS) --batch -l ~/.emacs.d/straight/repos/straight.el/bootstrap.el \
		--eval "(straight-use-package 'use-package)" \
		--eval "(use-package clojure-mode :straight t)"
