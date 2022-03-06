all: compile

compile: clean
	mkdir ebin
	erl -make

# Installation disabled at moment
# install: uninstall compile
# 	sudo mkdir -p /usr/chicolang
# 	sudo cp -r ebin /usr/chicolang/bin
# 	sudo cp app /usr/bin/chico

# uninstall: 
# 	sudo rm -rf /usr/chicolang
# 	sudo rm -rf /usr/bin/chico

clean:
	rm -rf ebin
	rm -f erl_crash.dump

run:
	./app
