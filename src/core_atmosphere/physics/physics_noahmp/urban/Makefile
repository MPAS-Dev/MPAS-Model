.SUFFIXES: .F90 .o

.PHONY: urban urban_lib

#
# The Noah-MP code fails to build with the GNU compilers with -std=f2008,
# so remove that flag here if it is present in FFLAGS
#
FFLAGS_NONSTD = $(shell printf "%s" "$(FFLAGS)" | sed -e 's/-std=f2008//g' )

all: dummy urban

dummy:
	echo "****** compiling physics_noahmp/utility ******"

OBJS = NoahmpUrbanDriverMainMod.o

urban: $(OBJS)

urban_lib:
	ar -ru ./../../libphys.a $(OBJS)

# DEPENDENCIES:


NoahmpUrbanDriverMainMod.o: \
				 ../utility/Machine.o \
                ../drivers/mpas/NoahmpIOVarType.o \
			    ../../mpas_atmphys_date_time.o \
			    ../../mpas_atmphys_constants.o \
			    ../../physics_wrf/module_sf_urban.o \
				../../physics_wrf/module_sf_bep.o  \
				../../physics_wrf/module_sf_bep_bem.o

clean:
	$(RM) *.f90 *.o *.mod
	@# Certain systems with intel compilers generate *.i files
	@# This removes them during the clean process
	$(RM) *.i

.F90.o:
	$(FC) $(CPPFLAGS) $(COREDEF) $(FFLAGS_NONSTD) -c $*.F90 $(CPPINCLUDES) $(FCINCLUDES) -I../utility -I../../../../framework \
	-I../../physics_wrf  -I../drivers/mpas -I../../

