develop:
	clojure -M:nrepl
.PHONY: develop

run:
	clojure -M -m elephant.core
.PHONY: run
