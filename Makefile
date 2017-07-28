.PHONY: all
all:
	@scripts/check.bash compile checkdoc shellcheck longlines toc

.PHONY: travis
travis:
	@scripts/check.bash compile checkdoc shellcheck longlines

.PHONY: compile
compile:
	@scripts/check.bash compile

.PHONY: checkdoc
checkdoc:
	@scripts/check.bash checkdoc

.PHONY: shellcheck
shellcheck:
	@scripts/check.bash shellcheck

.PHONY: longlines
longlines:
	@scripts/check.bash longlines

.PHONY: toc
toc:
	@markdown-toc -i README.md

.PHONY: clean
clean:
	@rm -f *.elc
