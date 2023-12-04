bin/mokubune: mokubune.lisp mokubune.asd packages.lisp
	mkdir -p "$(@D)"
	sbcl --noinform --eval "(asdf:load-system \"mokubune\")" \
		--eval "(in-package #:mokubune)" \
		--eval "(load-default-templates-content \"$(CURDIR)/default-templates/\")" \
		--eval "(sb-ext:save-lisp-and-die \"$@\" :toplevel 'run :executable t :save-runtime-options t)" \
		--eval '(exit)'
