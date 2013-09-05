
ERLPREFIX=/usr/local/Cellar/erlang-r16/R16B01/lib/erlang

all: pre-compile compile


BEAMFILES =	ebin/grib-src-def.beam \
		ebin/raws-src-def.beam \
		ebin/utils.beam \
		ebin/plist.beam \
		ebin/nlscanner.beam \
		ebin/time-arith.beam \
		ebin/nlist.beam \
		ebin/config-srv.beam \
		ebin/grib-retr.beam \
		ebin/grib-srv.beam \
		ebin/mwest-retr.beam \
		ebin/mwest-srv.beam \
		ebin/ingest-srv.beam \
		ebin/log-stream.beam \
		ebin/log-srv.beam \
		ebin/ext-utils.beam \
		ebin/ext-proc.beam \
		ebin/filesys.beam \
		ebin/wrfxlib.beam \
		ebin/wrfx2-app.beam


pre-compile: ebin/nlparser.beam ebin/file_info.beam

ebin/file_info.beam: src/file_info.jxa

src/file_info.jxa:
	deps/jxautorec/jxautorec $(ERLPREFIX)/lib/kernel-*/include/file.hrl src/file_info.jxa file_info true

ebin/nlparser.beam: src/nlparser.yrl
	erlc -o src src/nlparser.yrl
	erlc -o ebin src/nlparser.erl

ebin/%.beam: src/%.jxa
	joxa -p ebin -p deps/mochiweb/ebin -p deps/mochiweb_xpath/ebin -o ebin -c $<

compile: $(BEAMFILES)

get-deps:
	rebar get-deps

compile-deps: compile_jxautorec
	rebar compile

compile_jxautorec: deps/jxautorec
	cd deps/jxautorec && make

clean:
	rm -f ebin/*.beam
