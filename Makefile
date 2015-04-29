
# find the directory in which the file.erl file resides (this will be inside the kernel app supplied with erlang)
ERLKERNELDIR=$(shell dirname `erl -noshell -eval 'io:format("~s~n",[filename:dirname(code:which(file))])' -eval 'init:stop()'`)

all: compile

INCLUDES=-p deps/grib_ingest/ebin -p deps/afm_ingest/ebin -p deps/raws_ingest/ebin -p deps/steward/ebin -p deps/pgsql/ebin


compile:
	rebar compile

get-deps:
	rebar get-deps

clean:
	rm -f ebin/*.beam
	rebar clean

