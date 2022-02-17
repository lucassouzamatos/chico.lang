all: compile

compile: clean
	mkdir ebin
	erl -make

uninstall: 
	sudo rm -rf /usr/arlang
	sudo rm -rf /usr/bin/ar

clean:
	rm -rf ebin
	rm -f erl_crash.dump

run:
	./app
