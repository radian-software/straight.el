.PHONY: all
all:
	@elint/elint checkdoc compile longlines toc

.PHONY: travis
travis:
	@elint/elint checkdoc compile longlines

.PHONY: checkdoc
checkdoc:
	@elint/elint checkdoc

.PHONY: compile
compile:
	@elint/elint compile

.PHONY: longlines
longlines:
	@elint/elint longlines

.PHONY: toc
toc:
	@elint/elint toc

.PHONY: clean
clean:
	@rm -f *.elc
