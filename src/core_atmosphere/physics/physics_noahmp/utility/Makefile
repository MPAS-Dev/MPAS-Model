# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F90

include ../../hrldas/user_build_options

OBJS = Machine.o \
       CheckNanMod.o
#      ErrorHandleMod.o

all:	$(OBJS)

.F90.o:
	@echo ""
	$(RM) $(*).f90
	$(CPP) $(CPPFLAGS) $(*).F90 > $(*).f90
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) $(*).f90
	@echo ""

#
# This command cleans up object (etc) files:
#

clean:
	rm -f *.o *.mod *.exe *.f90

#
# Dependencies:
#
CheckNanMod.o: Machine.o


