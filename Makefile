
all: compile


BEAMFILES = ebin/math-utils.beam \
        ebin/plist.beam \
        ebin/time-arith.beam \
        ebin/grib-retr.beam \
        ebin/grib-srv.beam \
        ebin/grib-man.beam \
        ebin/log-stream.beam \
        ebin/log-srv.beam \
        ebin/grib-app.beam


ebin/%.beam: src/%.jxa
	joxa -p ebin -o ebin -c $<

compile: $(BEAMFILES)


clean:
	rm -f ebin/*.beam
