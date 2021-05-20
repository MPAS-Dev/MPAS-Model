.SUFFIXES: .F .o

all: dummy physics_sima_init physics_sima

dummy:
	echo "****** compiling physics_sima ******"

OBJS_init = \
	./../physics_wrf/module_cam_error_function.o

OBJS = \
	bl_mynn.o \
	bl_mynn_mpas.o \
	bl_mynn_subroutines.o \
	mynn_shared.o \
	sf_mynn.o \
	sf_mynn_mpas.o

physics_sima_init: $(OBJS_init)

physics_sima: $(OBJS) physics_sima_init
	ar -ru ./../libphys.a $(OBJS)

# DEPENDENCIES:
bl_mynn.o: \
	bl_mynn_subroutines.o

bl_mynn_mpas.o: \
	bl_mynn_subroutines.o

sf_mynn.o: \
	mynn_shared.o

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
