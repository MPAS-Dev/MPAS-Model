# GNU compilers
FC = gfortran
FFLAGS = -ffree-line-length-none

# Intel compilers
#FC = ifort
#FFLAGS = -FR

NETCDF=/usr
############### There is generally no need to make modifications below this line ###############


# Attempt to detect whether there is a separate Fortran interface library for NetCDF
NCLIBS =
NCLIBC = -lnetcdf
NCLIBF = -lnetcdff
#ifneq ($(wildcard $(NETCDF)/lib/libnetcdff.*), )
    NCLIBS += $(NCLIBF)
#endif
NCLIBS += $(NCLIBC)


all: convert_grid_to_xyz

convert_grid_to_xyz: convert_grid_to_xyz.f90
	$(FC) convert_grid_to_xyz.f90 -o convert_grid_to_xyz $(FFLAGS) -I$(NETCDF)/include -L$(NETCDF)/lib $(NCLIBS)

clean:
	rm convert_grid_to_hcm
