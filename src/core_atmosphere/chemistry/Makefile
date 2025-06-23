.SUFFIXES: .F .o

ifeq ($(CORE),atmosphere)
COREDEF = -Dmpas
endif

all:
	./../tools/manage_externals/checkout_externals --externals ./../Externals.cfg
	$(MAKE) core_mpas_musica core_chemistry

dummy:
	echo "****** compiling chemistry ******"

OBJS = \
	mpas_atmchem_control.o

core_mpas_musica:
	(cd musica; $(MAKE) all COREDEF="$(COREDEF)")

core_chemistry: core_mpas_musica
	($(MAKE) chem_interface COREDEF="$(COREDEF)")
	ar -ru libchem.a $(OBJS)
	($(MAKE) -C ./musica mpas_musica_lib)

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
	$(CPP) $(CPPFLAGS) $(COREDEF) $(HYDROSTATIC) $(CPPINCLUDES) $< > $*.f90
	$(FC) $(FFLAGS) -c $*.f90 $(FCINCLUDES) -I./musica -I.. -I../../../framework -I../../../external/esmf_time_f90
else
	$(FC) $(CPPFLAGS) $(COREDEF) $(HYDROSTATIC) $(FFLAGS) -c $*.F $(CPPINCLUDES) $(FCINCLUDES) -I./musica -I.. -I../../../framework -I../../../external/esmf_time_f90
endif
