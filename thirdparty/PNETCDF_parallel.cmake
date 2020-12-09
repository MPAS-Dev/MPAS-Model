# Set the name of the directory where to compile PnetCDF
# pnetcdf_1_12_1 represents pnetcdf-1.12.1
set(PNETCDF_PARALLEL_PREFIX pnetcdf_1_12_1_parallel)

# set a variable to point to the URL of the PnetCDF source.
# since we manually downloaded this, it will look like below
#set(PNETCDF_URL ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/pnetcdf-1.12.1.tar.gz)
set(PNETCDF_URL https://parallel-netcdf.github.io/Release/pnetcdf-1.12.1.tar.gz)

# calculate the MD5 sum of the file downloaded and set it in a variable
set(PNETCDF_URL_MD5 878192db34e436cbdd869fff3c7bf0c9)

message(STATUS "Could NOT find PnetCDF. Will build it.")

include_directories(${MPI_Fortran_INCLUDE_PATH})
link_libraries(${MPI_Fortran_LIBRARIES})

set(PNETCDF_MPI_CONFIGURE MPICC=${MPI_C_COMPILER} MPICXX=${MPI_CXX_COMPILER} MPIF77=${MPI_Fortran_COMPILER} MPIF90=${MPI_Fortran_COMPILER})
set(PNETCDF_PARALLEL_CONFIGURE ./configure ${PNETCDF_MPI_CONFIGURE} --enable-fortran --prefix=${CMAKE_BINARY_DIR}/${PNETCDF_PARALLEL_PREFIX})

ExternalProject_Add(${PNETCDF_PARALLEL_PREFIX}
    PREFIX ${PNETCDF_PARALLEL_PREFIX}
    URL ${PNETCDF_URL}
    URL_MD5 ${PNETCDF_URL_MD5}
    CONFIGURE_COMMAND ${PNETCDF_PARALLEL_CONFIGURE}
    BUILD_COMMAND  make -j ${NUMPROCS}
    BUILD_IN_SOURCE 1
    INSTALL_COMMAND make install
    LOG_DOWNLOAD 1
    LOG_CONFIGURE 1
    LOG_BUILD 1
    LOG_INSTALL 1
)

# get the unpacked source directory path
ExternalProject_Get_Property(${PNETCDF_PARALLEL_PREFIX} SOURCE_DIR)

# Set separate directories for building in Debug or Release mode

# set the variables that would be set from find_package(NetCDF) or can be queried
set(PnetCDF_FOUND true)
set(PnetCDF_C_FOUND ON)
set(PnetCDF_Fortran_FOUND ON)

set(PnetCDF_C_IS_SHARED false)
set(PnetCDF_C_INCLUDE_DIR ${CMAKE_BINARY_DIR}/pnetcdf_1_12_1_parallel/include)
set(PnetCDF_C_INCLUDE_DIRS ${CMAKE_BINARY_DIR}/pnetcdf_1_12_1_parallel/include)
# Not setting this because it breaks pio build
#set(PnetCDF_C_LIBRARY ${CMAKE_BINARY_DIR}/pnetcdf_1_12_1_parallel/lib/libpnetcdf.a)
set(PnetCDF_C_LIBRARIES ${CMAKE_BINARY_DIR}/pnetcdf_1_12_1_parallel/lib/libpnetcdf.a)

set(PnetCDF_Fortran_IS_SHARED false)
set(PnetCDF_Fortran_INCLUDE_DIR ${CMAKE_BINARY_DIR}/pnetcdf_1_12_1_parallel/include)
set(PnetCDF_Fortran_INCLUDE_DIRS ${CMAKE_BINARY_DIR}/pnetcdf_1_12_1_parallel/include)
# Not setting this because to be symmetric with C
#set(PnetCDF_Fortran_LIBRARY ${CMAKE_BINARY_DIR}/pnetcdf_1_12_1_parallel/lib/libpnetcdf.a)
set(PnetCDF_Fortran_LIBRARIES ${CMAKE_BINARY_DIR}/pnetcdf_1_12_1_parallel/lib/libpnetcdf.a)

set(PnetCDF_C_HAS_VARN true)
set(PnetCDF_Fortran_HAS_VARN true)
