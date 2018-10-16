.PHONY: build run

build:
	stack build

run: build
	stack exec refocus $(COMMAND)

install: build
	sudo cp $(PWD)/.stack-work/install/x86_64-osx/lts-12.13/8.4.3/bin/refocus /usr/local/bin

