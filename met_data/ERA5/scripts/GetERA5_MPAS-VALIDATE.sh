#!/bin/bash -l

# Adapted from https://dreambooker.site/2019/10/03/Initializing-the-WRF-model-with-ERA5-pressure-level/

CODEDIR=$MPAS_DIR/met_data/ERA5/scripts
DATADIR=$MPAS_DIR/met_data/ERA5/DATA

# Set your python environment
# export PATH=/home/guib/anaconda3/bin:$PATH
source activate root
cd $CODEDIR

DATE1=20040324
DATE2=20040329
YY1=`echo $DATE1 | cut -c1-4`
MM1=`echo $DATE1 | cut -c5-6`
DD1=`echo $DATE1 | cut -c7-8`
YY2=`echo $DATE2 | cut -c1-4`
MM2=`echo $DATE2 | cut -c5-6`
DD2=`echo $DATE2 | cut -c7-8`

# Grib data
Nort=-10
West=-70
Sout=-50
East=0-10

sed -e "s/DATE1/${DATE1}/g;s/DATE2/${DATE2}/g;s/Nort/${Nort}/g;s/West/${West}/g;s/Sout/${Sout}/g;s/East/${East}/g;" GetERA5-sl.py > GetERA5-${DATE1}-${DATE2}-sl.py

python GetERA5-${DATE1}-${DATE2}-sl.py

mkdir -p ${DATADIR}/$YY1

mv ERA5-${DATE1}-${DATE2}-sl.grib ${DATADIR}/$YY1/

# move the generated files
mkdir -p ${CODEDIR}/APIs

mv GetERA5-${DATE1}-${DATE2}-sl.py ${CODEDIR}/APIs/

exit 0
