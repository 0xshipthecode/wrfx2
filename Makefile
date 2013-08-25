
all: compile


BEAMFILES = 	ebin/grib-src-def.beam \
		ebin/raws-src-def.beam \
		ebin/utils.beam \
		ebin/plist.beam \
		ebin/time-arith.beam \
            	ebin/grib-retr.beam \
            	ebin/grib-srv.beam \
        	ebin/grib-man.beam \
        	ebin/mwest-retr.beam \
        	ebin/mwest-srv.beam \
		ebin/ingest-srv.beam \
        	ebin/log-stream.beam \
        	ebin/log-srv.beam \
		ebin/task-exec.beam \
        	ebin/tasks-fsys.beam \
		ebin/wrfx2-app.beam


ebin/%.beam: src/%.jxa
	joxa -p ebin -p deps/mochiweb/ebin -p deps/mochiweb_xpath/ebin -o ebin -c $<

compile: $(BEAMFILES)


get-deps: deps/mochiweb deps/mochiweb_xpath

deps/mochiweb:
	cd deps && git clone git://github.com/mochi/mochiweb.git

deps/mochiweb_xpath:
	cd deps && git clone git://github.com/retnuh/mochiweb_xpath.git

compile-deps: compile_mochiweb compile_xpath
	
compile_mochiweb: deps/mochiweb
	cd deps/mochiweb && rebar compile

compile_xpath: deps/mochiweb_xpath
	cd deps/mochiweb_xpath && rebar compile


clean:
	rm -f ebin/*.beam
