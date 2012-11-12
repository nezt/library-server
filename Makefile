DIALYZER=dialyzer

.PHONY: start

all: compile 

compile:
	@echo "Compiling library server...... "
	@erlc +debug_info -I include/ -o ebin/ src/*.erl
	@echo "library server it's ok........ "
clean:
	@echo "Cleaning data...... "
	@rm -rf ebin/*.beam
	@rm -rf Mnesia.nonode@nohost
	@echo "Terminated............ "

start:
	@erl -pa ebin/ -eval 'mnesia:create_schema(nodes()).' -eval 'ok = application:start(gen_app).'


