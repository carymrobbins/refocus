.PHONY: clean build run install

build: RefocusAppKitUtil.dylib
	stack build

clean:
	rm -f RefocusAppKitUtil.dylib
	stack clean

RefocusAppKitUtil.dylib: RefocusAppKitUtil.m
	clang -Werror -shared \
	  -framework Foundation \
	  -framework AppKit \
	  RefocusAppKitUtil.m -o RefocusAppKitUtil.dylib

run: build
	stack exec refocus $(COMMAND)

install: build
	sudo cp $(PWD)/.stack-work/install/x86_64-osx/lts-12.13/8.4.3/bin/refocus /usr/local/bin
	sudo cp $(PWD)/RefocusAppKitUtil.dylib /usr/local/lib
