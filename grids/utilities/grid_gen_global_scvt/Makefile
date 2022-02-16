#FC = ifort
#FFLAGS = -FR -m64 -O3 -fast -ipo -openmp
#F77FLAGS = -FI -m64 -O3 -fast -ipo -openmp
#CPPFLAGS = -DRKIND=8
#PROMOTION = -r8
#LDFLAGS = -m64 -O3 -fast -ipo -openmp

FC = gfortran-8
FFLAGS = -ffree-form -O3 -fopenmp -ffree-line-length-none
F77FLAGS = -ffixed-form -O3 -fopenmp -fsecond-underscore
CPPFLAGS = -DRKIND=8
PROMOTION = -fdefault-real-8
LDFLAGS = -O3 -fopenmp

#FC = pgf90
#FFLAGS = -Mfree -O3 -mp -byteswapio
#F77FLAGS = -O3 -byteswapio
#CPPFLAGS = -DRKIND=8
#PROMOTION = -r8
#LDFLAGS = -O3 -mp -byteswapio


# Attempt to detect whether there is a separate Fortran interface library for NetCDF
NCLIBS =
NCLIBC = -lnetcdf
NCLIBF = -lnetcdff
ifneq ($(wildcard $(NETCDF)/lib/libnetcdff.*), )
    NCLIBS += $(NCLIBF)
endif
NCLIBS += $(NCLIBC)


all: main_exe

main_exe:
	( cd src; $(MAKE) FC="$(FC)" FFLAGS="$(FFLAGS)" F77FLAGS="$(F77FLAGS)" CPPFLAGS="$(CPPFLAGS)" PROMOTION="$(PROMOTION)" LDFLAGS="$(LDFLAGS)" NCLIBS="$(NCLIBS)")
	if [ ! -e grid_gen ]; then ln -s src/grid_gen .; fi

clean:
	( cd src; make clean )
	rm -f grid_gen

