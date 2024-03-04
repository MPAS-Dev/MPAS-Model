.SUFFIXES: .F90 .o

all: dummy utility

dummy:
	echo "****** compiling physics_noahmp/utility ******"

OBJS =  Machine.o \
	CheckNanMod.o

utility: $(OBJS)
	ar -ru ./../../libphys.a $(OBJS)

# DEPENDENCIES:
CheckNanMod.o: \
	Machine.o

clean:
	$(RM) *.f90 *.o *.mod
	@# Certain systems with intel compilers generate *.i files
	@# This removes them during the clean process
	$(RM) *.i

.F90.o:
	$(FC) $(CPPFLAGS) $(COREDEF) $(FFLAGS) -c $*.F90 $(CPPINCLUDES) $(FCINCLUDES) -I../../../../framework

