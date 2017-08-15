# GNU compilers
#FC = gfortran
#FFLAGS = -ffree-line-length-none

# Intel compilers
FC = $(serial_FC)
FFLAGS = -FR


############### There is generally no need to make modifications below this line ###############


# Attempt to detect whether there is a separate Fortran interface library for NetCDF
NCLIBS =
NCLIBC = -lnetcdf
NCLIBF = -lnetcdff
ifneq ($(wildcard $(NETCDF)/lib/libnetcdff.*), )
    NCLIBS += $(NCLIBF)
endif
NCLIBS += $(NCLIBC)


all: grid_rotate

grid_rotate: grid_rotate.f90
	$(FC) grid_rotate.f90 -o grid_rotate $(FFLAGS) -I$(NETCDF)/include -L$(NETCDF)/lib $(NCLIBS)

clean:
	rm grid_rotate
