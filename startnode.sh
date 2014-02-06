#!/usr/bin/env bash
erl -sasl -detached -sname wrfx2 -pa $HOME/Packages/joxa/ebin ebin deps/*/ebin -s wrfx2

