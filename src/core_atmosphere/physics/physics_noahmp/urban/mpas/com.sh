#!/bin/bash

source /disk/r182/wzhangcy/yeer/.bashrc_cye --force
make clean
make
cp NoahmpUrbanDriverMainMod.o /disk/r182/wzhangcy/yeer/cye_MPAS_test/MPAS-Model-master/src/core_atmosphere/libphys/
cp /disk/r182/wzhangcy/yeer/cye_MPAS_test/MPAS-Model-master/src/core_atmosphere/physics/physics_noahmp/urban/mpas/*.TBL /disk/r182/wzhangcy/yeer/cye_MPAS_test/MPAS-Model-master
cp /disk/r182/wzhangcy/yeer/cye_MPAS_test/MPAS-Model-master/DBL_dir/* /disk/r182/wzhangcy/yeer/cye_MPAS_test/MPAS-Model-master 
