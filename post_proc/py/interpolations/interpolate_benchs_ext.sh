#!/bin/bash
for exp in /p1-nemo/danilocs/mpas/MPAS-BR/benchmarks/Catarina_physics-test/Catarina_250-8km.best-physics_sst_ext/run*;
do 
    ls "$exp"/latlon.nc;
    expname=$( echo ${exp##*/} )
    IFS=. read -r str1 str2 str3 <<< "$expname"
    IFS=_ read -r tmp1 tmp2 microp <<< "$str2"
    IFS=_ read -r tmp1 tmp2 cumulus <<< "$str3"
    outname=$microp-$cumulus
    python to_Lorenz.py -i "$exp"/latlon.nc -o "$outname"_MPAS.nc -n 0
done
