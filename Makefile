mkfile_path := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

prefix=/usr/local

install:
	ln -fs $(mkfile_path)/bin/boot-vim-tidal $(prefix)/bin
	ln -fs $(mkfile_path)/bin/tidalvim $(prefix)/bin
	ln -fs $(mkfile_path)/bin/boot-superDirt $(prefix)/bin
	ln -fs $(mkfile_path)/p5jsDirt/p5jsDirt-linux $(prefix)/bin

uninstall:
	rm -f $(prefix)/bin/boot-vim-tidal $(prefix)/bin/tidalvim $(prefix)/bin/boot-superDirt $(prefix)/bin/p5jsDirt-linux

.PHONY: install uninstall
