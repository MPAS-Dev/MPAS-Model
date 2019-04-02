#!/usr/bin/env sh

################################################################################
# File: checkout_data_files.sh
#
# The purpose of this script is to obtain lookup tables used by the WRF physics
#   packages.  At present, the only method for acquiring these tables is through
#   the MPAS-Dev github repository using either git, svn, or curl.
#
# If none of the methods used in this script are successful in acquiring the 
#   tables, please attempt to manually download the files from the MPAS-Data 
#   repository at https://github.com/MPAS-Dev/MPAS-Data/.  All *.TBL and *DATA* 
#   files, as well as the COMPATIBILITY file, should be copied into 
#   a subdirectory named src/core_atmosphere/physics/physics_wrf/files before 
#   continuing the build process.
#
# If all else fails, please contact the MPAS-A developers 
#   via "mpas-atmosphere-help@googlegroups.com".
#
################################################################################

mpas_vers="6.0"

if [ -s physics_wrf/files/COMPATIBILITY ]; then

   compatible=0

   compat=`cat physics_wrf/files/COMPATIBILITY | grep -v "#"`
   for ver in $compat; do
      if [ "$ver" = "$mpas_vers" ]; then
         compatible=1
      fi
   done

   if [ $compatible -eq 1 ]; then
      echo "*** Compatible versions of WRF physics tables appear to already exist; no need to obtain them again ***"
      exit 0
   else
      echo "*** Existing WRF physics tables appear to be incompatible with MPAS v$mpas_vers; downloading the latest tables ***"
   fi

else

   echo "*** No compatible version of WRF physics tables found; downloading the latest tables ***"

fi


if [ ! -d physics_wrf/files ]; then
   mkdir -p physics_wrf/files
fi

#
# Try using 'git'
#
which git
if [ $? -eq 0 ]; then
   echo "*** trying git to obtain Laura WRF physics tables ***"
   git clone git://github.com/ldfowler58/MPAS-Data.git
   if [ $? -eq 0 ]; then
      mv MPAS-Data/atmosphere/physics_wrf/files/* physics_wrf/files
      rm -rf MPAS-Data
      exit 0
   else
      echo "*** failed to obtain Laura WRF physics tables using git ***"
   fi
else
   echo "*** git not in path ***"
fi


#
# Try using 'svn'
#
which svn
if [ $? -eq 0 ]; then
   echo "*** trying svn to obtain WRF physics tables ***"
   svn checkout --non-interactive --trust-server-cert https://github.com/MPAS-Dev/MPAS-Data.git
   if [ $? -eq 0 ]; then
      mv MPAS-Data.git/trunk/atmosphere/physics_wrf/files/* physics_wrf/files
      rm -rf MPAS-Data.git
      exit 0
   else
      echo "*** failed to obtain WRF physics tables using svn ***"
   fi
else
   echo "*** svn not in path ***"
fi


#
# Try using 'curl'
#
which curl
if [ $? -eq 0 ]; then
   echo "*** trying curl to obtain WRF physics tables ***"
   curl -o master.zip https://codeload.github.com/MPAS-Dev/MPAS-Data/zip/master
   if [ $? -eq 0 ]; then
      which unzip
      if [ $? -eq 0 ]; then
         unzip master.zip
         mv MPAS-Data-master/atmosphere/physics_wrf/files/* physics_wrf/files
         rm -rf master.zip MPAS-Data-master
         exit 0
      else
         echo "*** unzip not in path -- unable to unzip WRF physics tables"
         rm -f master.zip
      fi
   else
      echo "*** failed to obtain WRF physics tables using curl ***"
   fi
else
   echo "*** curl not in path ***"
fi


echo "***************************************************************"
echo "Unable to obtain WRF physics tables using git, svn, or curl."
echo "This may be because 'git', 'svn', and 'curl' are not installed,"
echo "or it could be due to network connectivity problems."
echo " "
echo "Please see src/core_atmosphere/physics/checkout_data_files.sh"
echo "for suggestions on how to remedy this issue."
echo "***************************************************************"

exit 1
