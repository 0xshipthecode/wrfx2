#!/usr/bin/env bash
HOST=`hostname -s`
erl -sasl -sname wrfx2@$HOST -pa $JOXA_HOME/ebin ebin deps/*/ebin -s wrfx2

