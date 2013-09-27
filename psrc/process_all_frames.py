#!/usr/bin/env python

import netCDF4
import sys
import string
import pytz
from datetime import datetime
import os


def load_times(d):
    gmt_tz = pytz.timezone('GMT')
    tm = d.variables['Times'][:,...]
    tp = []
    for t in tm:
        dt = datetime.strptime(''.join(t), '%Y-%m-%d_%H:%M:%S')
        dt = dt.replace(tzinfo = gmt_tz)
        tp.append(dt)
    return tp


def load_str_times(d):
    tm = d.variables['Times'][:,...]
    tp = []
    for t in tm:
      tp.append(''.join(t))
    return tp


if __name__ == '__main__':
    wrfout = sys.argv[1]
    tgt_dir = sys.argv[2]

    d = netCDF4.Dataset(wrfout)
    times = load_str_times(d)
    for t in times:
        cmd = "deps/wrf2kmz/wrf2kml.py %s FGRNHFX %s %s/%s.kmz" % (wrfout, t, tgt_dir, t)
        print(cmd)
        os.system(cmd)

    d.close()

