.PHONY: all
all: compile toc

.PHONY: compile
compile:
	emacs -Q --batch --eval                  \
          "(progn                                \
             (setq byte-compile-error-on-warn t) \
             (push default-directory load-path)  \
             (batch-byte-compile))"              \
          straight.el bootstrap.el install.el

.PHONY: toc
toc:
	markdown-toc -i README.md

.PHONY: clean
clean:
	rm -f *.elc
