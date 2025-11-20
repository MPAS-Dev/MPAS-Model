.SUFFIXES: .F .o

.PHONY: mpas_musica mpas_musica_lib

all: dummy mpas_musica

dummy:
	echo "****** compiling mpas_musica ******"

OBJS = \
  mpas_musica.o

mpas_musica: $(OBJS)

mpas_musica_lib:
	ar -ru ./../libchem.a $(OBJS)

clean:
	$(RM) *.f90 *.o *.mod
	@# Certain systems with intel compilers generate *.i files
	@# This removes them during the clean process
	$(RM) *.i

.F.o:
ifeq "$(GEN_F90)" "true"
	$(CPP) $(CPPFLAGS) $(COREDEF) $(CPPINCLUDES) $< > $*.f90
	$(FC) $(FFLAGS) -c $*.f90 $(FCINCLUDES) -I.. -I../../../framework -I../../../external/esmf_time_f90
else
	$(FC) $(CPPFLAGS) $(COREDEF) $(FFLAGS) -c $*.F $(CPPINCLUDES) $(FCINCLUDES) -I.. -I../../../framework -I../../../external/esmf_time_f90
endif

