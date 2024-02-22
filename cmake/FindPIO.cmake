# - Try to find PIO
# Once done this will define
#  PIO_FOUND - System has PIO
#  PIO_INCLUDE_DIRS - The PIO include directories
#  PIO_LIBRARIES - The libraries needed to use PIO
#  PIO_VERSION - A global property indicating the PIO API: either 1 or 2 if PIO was found, and 0 otherwise

find_path(PIO_INCLUDE_DIR pio.mod PIO.mod HINTS ENV PIO ENV PIO_PATH PATH_SUFFIXES include)
find_library(PIO1_C_LIBRARY libpio.a HINTS ENV PIO ENV PIO_PATH PATH_SUFFIXES lib)
find_library(PIO2_C_LIBRARY libpioc.a libpioc.so HINTS ENV PIO ENV PIO_PATH PATH_SUFFIXES lib)
find_library(PIO_F_LIBRARY libpiof.a libpiof.so HINTS ENV PIO ENV PIO_PATH PATH_SUFFIXES lib)

if(PIO1_C_LIBRARY)
   set(PIO_C_LIBRARY ${PIO1_C_LIBRARY})
   set_property(GLOBAL PROPERTY PIO_VERSION 1)

   #
   # PIO 1.x may not have a separate Fortran and C library, in which case, we can just
   # use libpio.a as the only PIO library
   #
   if (${PIO_F_LIBRARY} STREQUAL "PIO_F_LIBRARY-NOTFOUND")
      set(PIO_LIBRARIES ${PIO_C_LIBRARY})
   else()
      set(PIO_LIBRARIES ${PIO_F_LIBRARY} ${PIO_C_LIBRARY})
   endif()
   set(PIO_INCLUDE_DIRS ${PIO_INCLUDE_DIR})
elseif(PIO2_C_LIBRARY)
   set(PIO_C_LIBRARY ${PIO2_C_LIBRARY})
   set_property(GLOBAL PROPERTY PIO_VERSION 2)
   set(PIO_LIBRARIES ${PIO_F_LIBRARY} ${PIO_C_LIBRARY})
   set(PIO_INCLUDE_DIRS ${PIO_INCLUDE_DIR})
else()
   message("No PIO library found...")
   set_property(GLOBAL PROPERTY PIO_VERSION 0)
   set(PIO_LIBRARIES "")
   set(PIO_INCLUDE_DIRS "")
endif()


include(FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set PIO_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(PIO DEFAULT_MSG PIO_LIBRARIES PIO_INCLUDE_DIRS)
