
all: compile


BEAMFILES = ebin/utils.beam \
	    ebin/plist.beam \
	    ebin/grib-time.beam \
	    ebin/grib-retr.beam \
	    ebin/grib-srv.beam


ebin/%.beam: src/%.jxa
	joxa -p ebin -o ebin -c $<

compile: $(BEAMFILES)


clean:
	rm -f ebin/*.beam