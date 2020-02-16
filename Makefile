mkfile_path := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

prefix=/usr/local


# todo: add checks for the installation of neovim, supercollider, ghc, tidal, and P5hs
install:
	ln -fs $(mkfile_path)/bin/boot-vim-tidal $(prefix)/bin
	ln -fs $(mkfile_path)/bin/tidalvim $(prefix)/bin
	ln -fs $(mkfile_path)/bin/boot-superDirt $(prefix)/bin
	ln -fs $(mkfile_path)/p5jsDirt/p5jsDirt-linux $(prefix)/bin
	cat plugin/tidal.vim syntax/*.vim ftdetect/tidal.vim ftplugin/tidal.vim auto-pairs/plugin/auto-pairs.vim >> tidalvim/big-tidal.vim
	# honestly, ^^^ this hack-y line has saved me so much trouble trying to manage vim plugins

.PHONY: install uninstall
   
