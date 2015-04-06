#!/usr/bin/env bash
HOST=`hostname -s`
erl -sasl -sname wrfx2@$HOST -pa ebin deps/*/ebin $HOME/Packages/joxa/ebin -s wrfx2

