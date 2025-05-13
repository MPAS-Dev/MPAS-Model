# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F

include ./user_build_options

OBJS = \
        module_bep_bem_helper.o \
	module_sf_urban.o \
	module_sf_bep.o \
	module_sf_bem.o \
	module_sf_bep_bem.o \
	NoahmpUrbanDriverMainMod.o

# CPPHRLDAS = -D_HRLDAS_OFFLINE_

all:	$(OBJS)

# NoahmpUrbanDriverMainMod.o: NoahmpUrbanDriverMainMod.F
# 	@echo ""
# 	$(RM) NoahmpUrbanDriverMainMod.f90
# 	$(CPP) $(CPPFLAGS) $(*).F > $(*).f90
# 	$(COMPILERF90) -c -I../../noahmp/drivers/hrldas -I. -I../../hrldas/Utility_routines \
#         -I../../noahmp/utility $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) $(*).f90
# 	@echo ""


NoahmpUrbanDriverMainMod.o: NoahmpUrbanDriverMainMod.F
	@echo ""
	$(RM) NoahmpUrbanDriverMainMod.f90
	$(CPP) $(CPPFLAGS) $(*).F > $(*).f90
	$(COMPILERF90) -c -I/disk/r182/wzhangcy/yeer/cye_MPAS_test/MPAS-Model-master/src/core_atmosphere/physics/physics_noahmp/drivers/mpas \
	 -I. -I/disk/r182/wzhangcy/yeer/cye_MPAS_test/MPAS-Model-master/src/core_atmosphere/physics \
        -I/disk/r182/wzhangcy/yeer/cye_MPAS_test/MPAS-Model-master/src/core_atmosphere/physics/physics_noahmp/utility \
		-I/disk/r182/wzhangcy/yeer/cye_MPAS_test/MPAS-Model-master/src/core_atmosphere/physics/physics_mmm \
		-I/disk/r182/wzhangcy/yeer/cye_MPAS_test/MPAS-Model-master/src/core_atmosphere/physics/physics_wrf \
		-I/disk/r182/wzhangcy/yeer/cye_MPAS_test/MPAS-Model-master/src/core_atmosphere/physics/physics_noahmp \
		-I/disk/r182/wzhangcy/yeer/cye_MPAS_test/MPAS-Model-master/src/core_atmosphere/physics/physics_noahmp/src \
		-I/disk/r182/wzhangcy/yeer/cye_MPAS_test/MPAS-Model-master/src/framework \
-I..  $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) $(*).f90
	@echo ""


.F.o:
	@echo ""
	$(RM) $(*).f90
	$(CPP) $(CPPFLAGS) $(CPPHRLDAS) $(*).F > $(*).f90
	$(COMPILERF90) -c  -I. -I/disk/r182/wzhangcy/yeer/cye_MPAS_test/MPAS-Model-master/src/core_atmosphere/physics \
        $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) $(*).f90
	@echo ""

#
# Dependencies:
#

# NoahmpUrbanDriverMainMod.o: ../../noahmp/utility/Machine.o \
#                             ../../noahmp/drivers/hrldas/NoahmpIOVarType.o \
# 			    ../../hrldas/Utility_routines/module_ra_gfdleta.o \
# 			    ../../hrldas/Utility_routines/module_model_constants.o \
# 			    module_sf_urban.o module_sf_bep.o module_sf_bep_bem.o

NoahmpUrbanDriverMainMod.o: /disk/r182/wzhangcy/yeer/cye_MPAS_test/MPAS-Model-master/src/core_atmosphere/physics/physics_noahmp/utility/Machine.o \
                            /disk/r182/wzhangcy/yeer/cye_MPAS_test/MPAS-Model-master/src/core_atmosphere/physics/physics_noahmp/drivers/mpas/NoahmpIOVarType.o \
			    /disk/r182/wzhangcy/yeer/cye_MPAS_test/MPAS-Model-master/src/core_atmosphere/physics/mpas_atmphys_date_time.o \
			    /disk/r182/wzhangcy/yeer/cye_MPAS_test/MPAS-Model-master/src/core_atmosphere/physics/mpas_atmphys_constants.o \
			    module_sf_urban.o module_sf_bep.o module_sf_bep_bem.o


module_sf_bep.o:	module_sf_urban.o module_bep_bem_helper.o 
module_sf_bep_bem.o:	module_sf_urban.o module_sf_bem.o module_bep_bem_helper.o

#
# This command cleans up object (etc) files:
#

clean:
	$(RM) *.o *.mod *.stb *~ *.f90

