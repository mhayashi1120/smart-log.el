EMACS = emacs

check: compile
	$(EMACS) -q -batch -L . -l smart-log.el -l smart-log-test.el \
		-f ert-run-tests-batch-and-exit
	$(EMACS) -q -batch -L . -l smart-log.elc -l smart-log-test.el \
		-f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -q -batch -f batch-byte-compile smart-log.el
