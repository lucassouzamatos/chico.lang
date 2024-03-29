all: format compile bootstrap test

compile: clean
	mkdir ebin
	erl -make

install: uninstall compile
	sudo mkdir -p /usr/chicolang
	sudo cp -r ebin /usr/chicolang/ebin
	sudo cp app /usr/bin/chico

	sudo chico --install

bootstrap:
	sudo ./app --install

test: compile
	sudo ./app_test

format:
	rebar3 steamroll

uninstall: 
	sudo rm -rf /usr/chicolang
	sudo rm -rf /usr/bin/chico

clean:
	rm -rf ebin
	rm -f erl_crash.dump

run:
	./app
