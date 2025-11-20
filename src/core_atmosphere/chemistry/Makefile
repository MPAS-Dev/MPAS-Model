.SUFFIXES: .F .o

all:
ifeq ($(MUSICA),true)
	echo "****** building MUSICA ******"
	$(MAKE) core_mpas_musica core_chemistry
else
	echo "****** not building MUSICA ******"
	$(MAKE) core_chemistry
endif

dummy:
	echo "****** compiling chemistry ******"

OBJS = \
	mpas_atm_chemistry.o

ifeq ($(MUSICA),true)
core_mpas_musica:
	echo "****** building CORE MPAS MUSICA ******"
	(cd musica; $(MAKE) all)
endif

core_chemistry: $(if $(filter true,$(MUSICA)),core_mpas_musica)
	($(MAKE) chem_interface)
	ar -ru libchem.a $(OBJS)
ifeq ($(MUSICA),true)
	($(MAKE) -C ./musica mpas_musica_lib)
endif

chem_interface: $(OBJS)

clean:
	$(RM) *.o *.mod *.f90 libchem.a
	( cd musica; $(MAKE) clean )
	@# Certain systems with intel compilers generate *.i files
	@# This removes them during the clean process
	$(RM) *.i

.F.o:
	$(RM) $@ $*.mod
ifeq "$(GEN_F90)" "true"
	$(CPP) $(CPPFLAGS) $(CPPINCLUDES) $< > $*.f90
	$(FC) $(FFLAGS) -c $*.f90 $(FCINCLUDES) -I./musica -I.. -I../../framework -I../../../external/esmf_time_f90
else
	$(FC) $(CPPFLAGS) $(FFLAGS) -c $*.F $(CPPINCLUDES) $(FCINCLUDES) -I./musica -I.. -I../../framework -I../../../external/esmf_time_f90
endif
