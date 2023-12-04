.POSIX:

ifndef PREFIX
  PREFIX = /usr/local
endif

bin/mokubune: *.lisp mokubune.asd default-templates/*
	mkdir -p "$(@D)"
	sbcl --noinform --eval "(asdf:load-system \"mokubune\")" \
		--eval "(in-package #:mokubune)" \
		--eval "(load-default-templates-content \"$(CURDIR)/default-templates/\")" \
		--eval "(sb-ext:save-lisp-and-die \"$@\" :toplevel 'run :executable t :save-runtime-options t)" \
		--eval '(exit)'

clean:
	@rm -f bin/mokubune

install: bin/mokubune
	mkdir -p $(DESTDIR)$(PREFIX)/bin
	cp bin/mokubune $(DESTDIR)$(PREFIX)/bin/

uninstall:
	rm -f $(DESTDIR)$(PREFIX)/bin/mokubune

.PHONY: install uninstall clean

