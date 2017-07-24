.PHONY: all
all: compile checkdoc longlines toc

.PHONY: travis
travis: compile checkdoc longlines

.PHONY: compile
compile:
	! emacs -Q --batch --eval                  \
            "(progn                                \
               (setq byte-compile-error-on-warn t) \
               (push default-directory load-path)  \
               (batch-byte-compile))"              \
            straight.el bootstrap.el install.el    \
            2>&1 | grep .

.PHONY: checkdoc
checkdoc:
	! emacs --batch --eval                      \
            "(progn                                 \
               (setq sentence-end-double-space nil) \
               (checkdoc-file \"straight.el\"))"    \
            2>&1 | grep .

.PHONY: longlines
longlines:
	scripts/longlines.sh

.PHONY: toc
toc:
	markdown-toc -i README.md

.PHONY: clean
clean:
	rm -f *.elc
