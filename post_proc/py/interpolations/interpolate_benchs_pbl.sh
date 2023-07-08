#!/bin/bash
for exp in /p1-nemo/danilocs/mpas/MPAS-BR/benchmarks/Catarina_physics-test/Catarina_250-8km.physics-pbl_sst/*;
do 
    ls "$exp"/latlon.nc;
    expname=$( echo ${exp##*/} )
    echo $expname
    IFS=. read -r str1 str2 str3 str4  str5 <<< "$expname"
    IFS=_ read -r tmp1 microp <<< "$str3"
    IFS=_ read -r tmp1 cumulus <<< "$str4"
    IFS=_ read -r tmp1 pbl <<< "$str5"
    outname=$microp-$cumulus-$pbl
    echo $outname
    python to_Lorenz.py -i "$exp"/latlon.nc -o "$outname"_MPAS.nc -n 0
done
