#!/usr/bin/env python

import netCDF4
import sys
import string
import pytz
from datetime import datetime
import cPickle

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
    fname = sys.argv[2]
    ts = sys.argv[1]

    d = netCDF4.Dataset(fname)
    times = load_str_times(d)
    try:
        ndx = times.index(ts)
        T2 = d.variables['T2'][:ndx, :, :]
        with open("T2_" + ts + ".bin", 'w') as f:
            cPickle.dump(T2, f)
        print('Success reading T2 for time %s' % ts)
        sys.exit(0)
    except Exception as e:
        print('Error encountered %s' % e)
        sys.exit(1)

