mkfile_path := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

prefix=/usr/local

install:
	ln -fs $(mkfile_path)/bin/boot-vim-tidal $(prefix)/bin
	ln -fs $(mkfile_path)/bin/tidalvim $(prefix)/bin
	ln -fs $(mkfile_path)/bin/boot-superDirt $(prefix)/bin

uninstall:
	rm -f $(prefix)/bin/boot-vim-tidal $(prefix)/bin/tidalvim

.PHONY: install uninstall
