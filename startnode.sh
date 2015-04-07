#!/usr/bin/env bash
HOST=`hostname -s`
erl \
 -sasl sasl_error_logger '{file,"log/system.log"}' \
 -detached \
 -sname wrfx2@$HOST \
 -pa ebin deps/*/ebin \
 -s wrfx2

