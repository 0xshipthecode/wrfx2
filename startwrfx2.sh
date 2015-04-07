#!/usr/bin/env bash
HOST=`hostname -s`
erl \
 -boot start_sasl \
 -sasl sasl_error_logger tty \
 -kernel error_logger tty \
 -sname wrfx2@$HOST \
 -pa ebin deps/*/ebin $HOME/Packages/joxa/ebin \
 -s wrfx2 start

