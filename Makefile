all: compile

install: uninstall compile
	sudo mkdir -p /usr/chicolang
	sudo cp -r ebin /usr/chicolang/bin
	sudo cp app /usr/bin/chico

compile: clean
	mkdir ebin
	erl -make

uninstall: 
	sudo rm -rf /usr/chicolang
	sudo rm -rf /usr/bin/chico

clean:
	rm -rf ebin
	rm -f erl_crash.dump

run:
	./app
