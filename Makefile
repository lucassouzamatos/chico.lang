all: compile

compile: clean
	mkdir ebin 
	erl -make

install: compile
	sudo mkdir -p /usr/arlang
	sudo cp -r ebin /usr/arlang/bin
	sudo cp app /usr/bin/ar

uninstall: 
	sudo rm -rf /usr/arlang
	sudo rm -rf /usr/bin/ar

clean:
	rm -rf ebin
	rm -f erl_crash.dump

run:
	./app

