wrfx2
=====

The second generation wrfx2 system is a platform that is to provide clearly
defined services useful towards running WPS/WRF either operationally or on-demand. 

  * develop new logging infrastructure, support message priority and out-of-band delivery
  * isolate and separate different subsystems and their configuration mechanisms (typically config files in etc/)
  * define clear interfaces/contracts between subsystems

wrfx2 platform services
=======================

  * logging subsystem
  * WRF/WPS/arbitrary external process execution
  * a library of functions to manipulate the filesystem
  * a scheduler
  * external data ingest (e.g. GRIB download)
  * namelist parsing/rewriting, verification against a specification
  * job monitoring/execution/watchdog services
  * job state storage/query mechanism
 
