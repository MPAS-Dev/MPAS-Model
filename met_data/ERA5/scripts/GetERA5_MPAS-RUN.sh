#!/bin/bash -l
#
# Created by:
#    Danilo Couto de Souza
#    Universidade de São Paulo (USP)
#    Instituto de Astornomia, Ciências Atmosféricas e Geociências
#    São Paulo - Brazil
#    
# Contact:
#    danilo.oceano@gmail.com
#
# Adapted from https://dreambooker.site/2019/10/03/Initializing-the-WRF-model-with-ERA5-pressure-level/
#
#
# ##### README #####
#
#   Remmeber to check Python environment before using this scripts
#   Usage: provided correct paths for scripts and data directories and
# specify correct dates.
#
#   The MPAS-A uses just the first time step for model initialization, so this
# script will download initial conditions just for this timestep (DATE1-HH1).
# However, the model updates the SST conditions for all time steps, if that 
# is specified in the initialization process. So, this script will download
# land-sea and sea-ice mask and SST data for all timesteps from DATE1 to DATE2


# Set environment
CODEDIR=$MPAS_DIR/met_data/ERA5/scripts
DATADIR=$MPAS_DIR/met_data/ERA5/DATA
cd $CODEDIR

# Specify dates
DATE1=20040323
DATE2=20040323
HH1=00
YY1=`echo $DATE1 | cut -c1-4`
MM1=`echo $DATE1 | cut -c5-6`
DD1=`echo $DATE1 | cut -c7-8`
YY2=`echo $DATE2 | cut -c1-4`
MM2=`echo $DATE2 | cut -c5-6`
DD2=`echo $DATE2 | cut -c7-8`


# Grib data
Nort=90
West=-180
Sout=-90
East=180


# Get surface and pressure level data just for the initial time
sed -e "s/DATE1/${DATE1}/g;s/HH1/${HH1}/g;s/Nort/${Nort}/g;s/West/${West}/g;s/Sout/${Sout}/g;s/East/${East}/g;" GetERA5-sl_init.py > GetERA5-${DATE1}${HH1}00-sl_init.py
python GetERA5-${DATE1}${HH1}00-sl_init.py
sed -e "s/DATE1/${DATE1}/g;s/HH1/${HH1}/g;s/Nort/${Nort}/g;s/West/${West}/g;s/Sout/${Sout}/g;s/East/${East}/g;" GetERA5-pl_init.py > GetERA5-${DATE1}${HH1}00-pl_init.py
python GetERA5-${DATE1}${HH1}00-pl_init.py

# get SST, land-sea and sea-ice data for all time steps
sed -e "s/DATE1/${DATE1}/g;s/DATE2/${DATE2}/g;s/Nort/${Nort}/g;s/West/${West}/g;s/Sout/${Sout}/g;s/East/${East}/g;" GetERA5-sst.py > GetERA5-${DATE1}-${DATE2}-sst.py
python GetERA5-${DATE1}-${DATE2}-sst.py

# Make directory to store data
mkdir -p ${DATADIR}/$YY1

# Move data to DATA directory
mv ERA5-${DATE1}-${HH1}00-sl.grib ERA5-${DATE1}-${HH1}00-pl.grib ERA5-${DATE1}-${DATE2}-sst.grib ${DATADIR}/$YY1/

# Store scripts for downloading data for future use 
mkdir -p ${CODEDIR}/APIs
mv GetERA5-${DATE1}${HH1}00-sl_init.py GetERA5-${DATE1}${HH1}00-pl_init.py GetERA5-${DATE1}-${DATE2}-sst.py ${CODEDIR}/APIs/

exit 0
