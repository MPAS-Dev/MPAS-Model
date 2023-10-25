#!/usr/bin/env bash

# Downloads INMET data
# requires wget and unzip on your path
#
# usage: ./download_inmet_data.sh [year1 year2 year3 ...]
# no argument:   Downloads data from 2000 to current year
#
# added by F.A.V. de Bragança Alves on 08/2023

my_unzip () {
	# Test if data is already inside a diretory with the year value
	unzip -l $1.zip | grep "$i/" &> /dev/null
	if [ $? == 0 ]; then
		unzip $1.zip
	else
		unzip $1.zip -d $1
	fi
}

nargs=$#

if [[ $nargs == 0 ]]; then
	cyear=$(date +'%Y')
	for (( i=2000; i<=$cyear; i++ )); do
		wget https://portal.inmet.gov.br/uploads/dadoshistoricos/$i.zip &&
		my_unzip $i &&
		rm $i.zip
	done
else
	for i in "$@"; do
		wget https://portal.inmet.gov.br/uploads/dadoshistoricos/$i.zip &&
		my_unzip $i &&
		rm $i.zip
	done
fi	
