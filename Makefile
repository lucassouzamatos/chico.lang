all: compile

compile: clean
	mkdir ebin 
	erl -make

clean:
	rm -rf ebin
	rm -f erl_crash.dump

run: app
	./app

