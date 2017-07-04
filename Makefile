.PHONY: all
all: compile

.PHONY: compile
compile:
	emacs -Q --batch --eval                  \
          "(progn                                \
             (setq byte-compile-error-on-warn t) \
             (push default-directory load-path)  \
             (batch-byte-compile))"              \
          *.el

.PHONY: clean
clean:
	rm -f *.elc
