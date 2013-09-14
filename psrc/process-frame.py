#!/usr/bin/env python

import netCDF4
import sys
import string
import pytz
from datetime import datetime

def load_times(d):
    gmt_tz = pytz.timezone('GMT')
    tm = d.variables['Times'][:,...]
    tp = []
    for t in tm:
        dt = datetime.strptime(''.join(t), '%Y-%m-%d_%H:%M:%S')
        dt = dt.replace(tzinfo = gmt_tz)
        tp.append(dt)
    
    return tp

d = netCDF4.Dataset("wksp/25b55327-2c43-4d62-beb9-a314ccf91c9f/wrf/wrfout_d01_2013-09-01_03:00:00")
times = load_times(d)
if len(times) > 0:
  print("The last ESMF timestamp in the file is %s" % str(times[-1]))

