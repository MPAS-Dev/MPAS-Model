.SUFFIXES: .F .c .o

all: esmf_time ezxml-lib smiol-lib

esmf_time:
	( cd esmf_time_f90; $(MAKE) FC="$(FC)" FFLAGS="$(FFLAGS)" CPP="$(CPP)" CPPFLAGS="$(CPPFLAGS) -DHIDE_MPI" GEN_F90=$(GEN_F90) )

ezxml-lib:
	( cd ezxml; $(MAKE) OBJFILE="ezxml.o" )

smiol-lib:
	$(MAKE) -C SMIOL

clean:
	( cd esmf_time_f90; $(MAKE) clean )
	( cd ezxml; $(MAKE) clean )
	$(MAKE) -C SMIOL clean
