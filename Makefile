EMACS=emacs

all: compile README.org

README.org: grammarbot.el
	$(EMACS) -Q -batch -L ../ellit-org.el -l ellit-org --eval '(ellit-org-export "grammarbot.el" "README.org")'

compile: grammarbot.elc

grammarbot.elc: grammarbot.el
	$(EMACS) -Q --batch -f batch-byte-compile grammarbot.el

clean:
	@rm -vf grammarbot.elc

.PHONY: clean
