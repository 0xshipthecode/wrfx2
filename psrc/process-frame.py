#!/usr/bin/env python

import netCDF4
import sys
import string

d = netCDF4.Dataset("wksp/25b55327-2c43-4d62-beb9-a314ccf91c9f/wrf/wrfout_d01_2013-09-01_04:00:00")
times = d.variables["Times"][:]
if len(times) > 0:
  print("The last ESMF timestamp in the file is %s" % string.join(times[-1], ''))


