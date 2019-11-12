# - Try to find PnetCDF
# Once done this will define
#  PNETCDF_FOUND - System has PnetCDF
#  PNETCDF_INCLUDE_DIRS - The PnetCDF include directories
#  PNETCDF_LIBRARIES - The libraries needed to use PnetCDF

find_path(PNETCDF_INCLUDE_DIR pnetcdf.mod PNETCDF.mod HINTS ENV PNETCDF ENV PNETCDF_PATH PATH_SUFFIXES include)
find_library(PNETCDF_LIBRARY libpnetcdf.a HINTS ENV PNETCDF ENV PNETCDF_PATH PATH_SUFFIXES lib)

set(PNETCDF_INCLUDE_DIRS ${PNETCDF_INCLUDE_DIR})
set(PNETCDF_LIBRARIES ${PNETCDF_LIBRARY})

include(FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set PNETCDF_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(PNETCDF DEFAULT_MSG PNETCDF_LIBRARIES PNETCDF_INCLUDE_DIRS)
