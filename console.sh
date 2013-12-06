#!/usr/bin/env bash
HOST=`hostname -s`
erl -sname wrfx2_cons -remsh wrfx2@$HOST
