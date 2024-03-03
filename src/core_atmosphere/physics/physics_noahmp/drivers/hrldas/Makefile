# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F90

include ../../../hrldas/user_build_options

OBJS = NoahmpGroundwaterInitMod.o \
       NoahmpSnowInitMod.o \
       NoahmpInitMainMod.o \
       NoahmpDriverMainMod.o \
       NoahmpIOVarType.o \
       NoahmpIOVarInitMod.o \
       NoahmpReadTableMod.o \
       NoahmpReadNamelistMod.o \
       ConfigVarOutTransferMod.o \
       ForcingVarOutTransferMod.o \
       EnergyVarOutTransferMod.o \
       WaterVarOutTransferMod.o \
       BiochemVarOutTransferMod.o \
       ConfigVarInTransferMod.o \
       ForcingVarInTransferMod.o \
       EnergyVarInTransferMod.o \
       WaterVarInTransferMod.o \
       BiochemVarInTransferMod.o \
       PedoTransferSR2006Mod.o

all:	$(OBJS)

.F90.o:
	@echo ""
	$(RM) $(*).f90
	$(CPP) $(CPPFLAGS) $(*).F90 > $(*).f90
	$(COMPILERF90) -c -I. -I../../src -I../../utility -I../../../hrldas/Utility_routines \
                          -I../../../hrldas/MPP $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) $(*).f90
	@echo ""

clean:
	rm -f *.o *.mod *.exe *.f90

#
# Dependencies:
#

NoahmpIOVarType.o:          ../../utility/Machine.o
NoahmpIOVarInitMod.o:       ../../utility/Machine.o NoahmpIOVarType.o
NoahmpReadTableMod.o:       ../../utility/Machine.o NoahmpIOVarType.o
NoahmpReadNamelistMod.o:    ../../utility/Machine.o NoahmpIOVarType.o
PedoTransferSR2006Mod.o:    ../../utility/Machine.o NoahmpIOVarType.o
ConfigVarOutTransferMod.o:  ../../utility/Machine.o NoahmpIOVarType.o ../../src/NoahmpVarType.o
ForcingVarOutTransferMod.o: ../../utility/Machine.o NoahmpIOVarType.o ../../src/NoahmpVarType.o
EnergyVarOutTransferMod.o:  ../../utility/Machine.o NoahmpIOVarType.o ../../src/NoahmpVarType.o
WaterVarOutTransferMod.o:   ../../utility/Machine.o NoahmpIOVarType.o ../../src/NoahmpVarType.o
BiochemVarOutTransferMod.o: ../../utility/Machine.o NoahmpIOVarType.o ../../src/NoahmpVarType.o
ConfigVarInTransferMod.o:   ../../utility/Machine.o NoahmpIOVarType.o ../../src/NoahmpVarType.o
ForcingVarInTransferMod.o:  ../../utility/Machine.o NoahmpIOVarType.o ../../src/NoahmpVarType.o
EnergyVarInTransferMod.o:   ../../utility/Machine.o NoahmpIOVarType.o ../../src/NoahmpVarType.o
BiochemVarInTransferMod.o:  ../../utility/Machine.o NoahmpIOVarType.o ../../src/NoahmpVarType.o
WaterVarInTransferMod.o:    ../../utility/Machine.o NoahmpIOVarType.o ../../src/NoahmpVarType.o PedoTransferSR2006Mod.o
NoahmpGroundwaterInitMod.o: ../../../hrldas/Utility_routines/module_domain.o \
                            ../../utility/Machine.o NoahmpIOVarType.o ../../src/GroundWaterMmfMod.o 
NoahmpSnowInitMod.o:        ../../utility/Machine.o NoahmpIOVarType.o
NoahmpInitMainMod.o:        ../../utility/Machine.o NoahmpIOVarType.o NoahmpSnowInitMod.o \
                            ../../../hrldas/Utility_routines/module_wrf_utilities.o
NoahmpDriverMainMod.o:      ../../utility/Machine.o ../../src/NoahmpVarType.o NoahmpIOVarType.o \
                            ../../src/ConfigVarInitMod.o \
                            ../../src/EnergyVarInitMod.o ../../src/ForcingVarInitMod.o \
                            ../../src/WaterVarInitMod.o ../../src/BiochemVarInitMod.o \
                            ../../src/NoahmpMainMod.o ../../src/NoahmpMainGlacierMod.o \
                            ConfigVarOutTransferMod.o EnergyVarOutTransferMod.o \
                            WaterVarOutTransferMod.o BiochemVarOutTransferMod.o \
                            ForcingVarOutTransferMod.o ConfigVarInTransferMod.o \
                            ForcingVarInTransferMod.o EnergyVarInTransferMod.o \
                            WaterVarInTransferMod.o BiochemVarInTransferMod.o \
                            ../../../hrldas/Utility_routines/module_ra_gfdleta.o

