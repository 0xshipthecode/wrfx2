
all: compile


BEAMFILES = ebin/grib-time.beam \
	    ebin/grib-retr.beam


ebin/%.beam: src/%.jxa
	joxa -p ebin -o ebin -c $<

compile: $(BEAMFILES)


clean:
	rm -f ebin/*.beam