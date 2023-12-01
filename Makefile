bin/mokubune: mokubune.lisp mokubune.asd packages.lisp
	mkdir -p "$(@D)"
	sbcl --noinform --eval "(asdf:load-system \"mokubune\")" \
		--eval "(sb-ext:save-lisp-and-die \"$@\" :toplevel 'mokubune:run :executable t)" \
		--eval '(exit)'
