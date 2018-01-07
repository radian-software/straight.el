.PHONY: all
all:
	@elint/elint checkdoc compile longlines toc

.PHONY: travis
travis:
	@elint/elint checkdoc compile longlines
	@mkdir -p ~/.emacs.d/straight/repos/
	@ln -s $(PWD) ~/.emacs.d/straight/repos/
	@emacs --batch -l ~/.emacs.d/straight/repos/straight.el/bootstrap.el \
	--eval "(straight-use-package 'use-package)" \
	--eval "(use-package el-patch :straight t)"

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
