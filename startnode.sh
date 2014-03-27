#!/usr/bin/env bash
HOST=`hostname -s`
erl -sasl -detached -sname wrfx2@$HOST -pa deps/joxa/ebin ebin deps/*/ebin -s wrfx2

