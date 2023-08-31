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
	sudo cp $(shell stack exec which refocus) /usr/local/bin
	sudo cp $(PWD)/RefocusAppKitUtil.dylib /usr/local/lib
