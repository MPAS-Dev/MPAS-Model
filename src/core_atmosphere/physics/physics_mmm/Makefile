.SUFFIXES: .F .o

all: dummy physics_mmm

dummy:
	echo "****** compiling physics_mmm ******"

OBJS = \
	bl_gwdo.o \
	bl_ysu.o \
	cu_ntiedtke.o \
	sf_sfclayrev.o \
	module_libmassv.o \
	mp_radar.o \
	mp_wsm6_effectRad.o \
	mp_wsm6.o

physics_mmm: $(OBJS)
	ar -ru ./../libphys.a $(OBJS)

# DEPENDENCIES:
mp_wsm6_effectRad.o: \
	mp_wsm6.o

mp_wsm6.o: \
	mp_radar.o \
	module_libmassv.o

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
