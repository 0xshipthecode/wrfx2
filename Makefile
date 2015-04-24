
# find the directory in which the file.erl file resides (this will be inside the kernel app supplied with erlang)
ERLKERNELDIR=$(shell dirname `erl -noshell -eval 'io:format("~s~n",[filename:dirname(code:which(file))])' -eval 'init:stop()'`)
JOXA_HOME=$(shell pwd)/deps/joxa

all: joxa compile-deps pre-compile compile

INCLUDES=-p deps/grib_ingest/ebin -p deps/afm_ingest/ebin -p deps/raws_ingest/ebin -p deps/steward/ebin -p deps/pgsql/ebin


BEAMFILES =	ebin/file_info.beam \
		ebin/utils.beam \
		ebin/nlscanner.beam \
		ebin/nlparser.beam \
		ebin/perftrack.beam \
		ebin/nlist.beam \
		ebin/nllib.beam \
		ebin/logsrv.beam \
		ebin/scheduler.beam \
		ebin/filesys.beam \
		ebin/sysmon.beam \
		ebin/fmda.beam \
		ebin/wrf-monitor.beam \
		ebin/wrfxlib.beam \
		ebin/testjob.beam \
		ebin/nasa-fire-job.beam \
		ebin/firejob.beam \
		ebin/wfcjob.beam \
		ebin/wrfx2.beam


pre-compile: ebin/nlparser.beam ebin/file_info.beam

ebin/file_info.beam: src/file_info.jxa
	deps/joxa/joxa -p ebin $(INCLUDES) -o ebin -c $<

src/file_info.jxa:
	deps/jxautorec/jxautorec $(ERLKERNELDIR)/include/file.hrl src/file_info.jxa file_info true

ebin/nlparser.beam: src/nlparser.yrl
	if [ ! -d "ebin" ] ; then mkdir ebin; fi
	erlc -o src src/nlparser.yrl
	erlc -o ebin src/nlparser.erl

ebin/%.beam: src/%.jxa
	deps/joxa/joxa -p ebin $(INCLUDES) -o ebin -c $<

compile: pre-compile $(BEAMFILES) 

get-deps:
	rebar get-deps

compile-deps: compile_jxautorec
	rebar compile

compile_jxautorec: deps/jxautorec
	cd deps/jxautorec && JOXA_HOME=$(JOXA_HOME) make

joxa: get-deps
	if [ ! -d "deps/joxa" ] ; then git clone https://github.com/vejmelkam/joxa.git deps/joxa; fi
	cd deps/joxa && rebar get-deps
	cd deps/joxa && make escript

clean:
	rm -f ebin/*.beam
	rebar clean

