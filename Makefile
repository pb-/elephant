develop:
	clojure -M:nrepl
.PHONY: develop

run:
	clojure -J-Dcom.googlecode.lanterna.terminal.UnixTerminal.sttyCommand=$(shell which stty) -M -m elephant.core
.PHONY: run
