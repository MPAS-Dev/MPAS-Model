# Set the name of the directory where to compile NetCDF-C
# netcdf_c_4_7_4 represents netcdf-c-4.7.4
set(NETCDF_C_PREFIX netcdf_c_4_7_4)

# set a variable to point to the URL of the NetCDF source.
# since we manually downloaded this, it will look like below
set(NETCDF_C_URL ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/v4.7.4.tar.gz)

# calculate the MD5 sum of the file downloaded and set it in a variable
set(NETCDF_C_URL_MD5 33979e8f0cf4ee31323fc0934282111b)

message(STATUS "Could NOT find NetCDF. Will build it.")

set(NETCDF_C_CONFIGURE "./configure")

ExternalProject_Add(${NETCDF_C_PREFIX}
    PREFIX ${NETCDF_C_PREFIX}
    URL ${NETCDF_C_URL}
    URL_MD5 ${NETCDF_C_URL_MD5}
    CONFIGURE_COMMAND ./configure --disable-netcdf4 --disable-byterange --prefix=${CMAKE_BINARY_DIR}/${NETCDF_C_PREFIX}
    BUILD_COMMAND  make -j ${NUMPROCS} 
    BUILD_IN_SOURCE 1
    INSTALL_COMMAND make install
    LOG_DOWNLOAD 1
    LOG_CONFIGURE 1
    LOG_BUILD 1
    LOG_INSTALL 1
)

# get the unpacked source directory path
ExternalProject_Get_Property(${NETCDF_C_PREFIX} SOURCE_DIR)

# set the variables that would be set from find_package(NetCDF) or can be queried
set(NetCDF_FOUND true)
set(NetCDF_C_FOUND ON)
#set(NetCDF_Fortran_FOUND ON)

set(NetCDF_C_IS_SHARED false)
set(NetCDF_C_INCLUDE_DIR ${CMAKE_BINARY_DIR}/netcdf_c_4_7_4/include)
set(NetCDF_C_INCLUDE_DIRS ${CMAKE_BINARY_DIR}/netcdf_c_4_7_4/include)
set(NetCDF_C_LIBRARY ${CMAKE_BINARY_DIR}/netcdf_c_4_7_4/lib/libnetcdf.a)
set(NetCDF_C_LIBRARIES ${CMAKE_BINARY_DIR}/netcdf_c_4_7_4/lib/libnetcdf.a)

#set(NetCDF_Fortran_IS_SHARED false)
#set(NetCDF_Fortran_INCLUDE_DIR ${CMAKE_BINARY_DIR}/netcdf_c_4_7_4/include)
#set(NetCDF_Fortran_INCLUDE_DIRS ${CMAKE_BINARY_DIR}/netcdf_c_4_7_4/include)
#set(NetCDF_Fortran_LIBRARY ${CMAKE_BINARY_DIR}/netcdf_c_4_7_4/lib/libnetcdf.a)
#set(NetCDF_Fortran_LIBRARIES ${CMAKE_BINARY_DIR}/netcdf_c_4_7_4/lib/libnetcdf.a)

set(NetCDF_C_HAS_VARN true)
#set(NetCDF_Fortran_HAS_VARN true)
