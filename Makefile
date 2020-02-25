mkfile_path := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

prefix=/usr/local


# todo: add checks for the installation of neovim, supercollider, ghc, tidal, and P5hs
install:
	ln -fs $(mkfile_path)/bin/boot-vim-tidal $(prefix)/bin
	ln -fs $(mkfile_path)/bin/tidalvim $(prefix)/bin
	ln -fs $(mkfile_path)/bin/boot-superDirt $(prefix)/bin
	ln -fs $(mkfile_path)/p5jsDirt/p5jsDirt-linux $(prefix)/bin
	cat auto-pairs/plugin/auto-pairs.vim plugin/tidal.vim syntax/*.vim ftdetect/tidal.vim ftplugin/tidal.vim config/vimrc config/vimrc > tidalvim/big-tidal.vim
	# cat  > tidalvim/big-vimrc
	# currently, because of issues with plugins, the makefile just puts all the plugins and the config into one file, then loads that file
	# it's poor practice and hacky, but it works for now

.PHONY: install uninstall
   
