all: compile

compile: clean
	mkdir ebin 
	erl -make
	cp ./app ./ebin/app

clean:
	rm -rf ebin
	rm -f erl_crash.dump

run: 
	cd ebin && \
	erl -eval 'ar_main:execute().' -noshell
