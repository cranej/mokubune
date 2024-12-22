.POSIX:

ifndef PREFIX
  PREFIX = /usr/local
endif

bin/mokubune: src/*.lisp mokubune.asd default-templates/* examples/*
	mkdir -p "$(@D)"
	sbcl --noinform --eval "(ql:quickload \"mokubune\")" \
		--eval "(in-package #:mokubune)" \
		--eval "(load-resource-files  \"$(CURDIR)/\")" \
		--eval "(sb-ext:save-lisp-and-die \"$@\" :toplevel 'run :executable t :save-runtime-options t :compression t)" \
		--eval '(exit)'

clean:
	@rm -f bin/mokubune

install: bin/mokubune
	mkdir -p $(DESTDIR)$(PREFIX)/bin
	cp bin/mokubune $(DESTDIR)$(PREFIX)/bin/

uninstall:
	rm -f $(DESTDIR)$(PREFIX)/bin/mokubune

.PHONY: install uninstall clean

