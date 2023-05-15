.SUFFIXES: .F .o

all: dummy physics_mmm

dummy:
	echo "****** compiling physics_mmm ******"

OBJS = \
	bl_gwdo.o \
	bl_ysu.o

physics_mmm: $(OBJS)
	ar -ru ./../libphys.a $(OBJS)

# DEPENDENCIES:

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
