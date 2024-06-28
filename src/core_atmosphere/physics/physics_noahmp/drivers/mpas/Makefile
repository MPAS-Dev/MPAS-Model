.SUFFIXES: .o .F90

.PHONY: driver driver_lib

all: dummy driver

dummy:
	echo "****** compiling physics_noahmp/drivers ******"

OBJS =  NoahmpSnowInitMod.o \
	NoahmpInitMainMod.o \
	NoahmpDriverMainMod.o \
	NoahmpIOVarType.o \
	NoahmpIOVarInitMod.o \
	NoahmpIOVarFinalizeMod.o \
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

driver: $(OBJS)

driver_lib:
	ar -ru ./../../../libphys.a $(OBJS)

# DEPENDENCIES:

NoahmpIOVarType.o:          ../../utility/Machine.o
NoahmpIOVarInitMod.o:       ../../utility/Machine.o NoahmpIOVarType.o
NoahmpIOVarFinalizeMod.o:   ../../utility/Machine.o NoahmpIOVarType.o
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
NoahmpSnowInitMod.o:        ../../utility/Machine.o NoahmpIOVarType.o
NoahmpInitMainMod.o:        ../../utility/Machine.o NoahmpIOVarType.o NoahmpSnowInitMod.o
NoahmpDriverMainMod.o:      ../../utility/Machine.o ../../src/NoahmpVarType.o NoahmpIOVarType.o \
                            ../../src/ConfigVarInitMod.o \
                            ../../src/EnergyVarInitMod.o ../../src/ForcingVarInitMod.o \
                            ../../src/WaterVarInitMod.o ../../src/BiochemVarInitMod.o \
                            ../../src/NoahmpMainMod.o ../../src/NoahmpMainGlacierMod.o \
                            ConfigVarOutTransferMod.o EnergyVarOutTransferMod.o \
                            WaterVarOutTransferMod.o BiochemVarOutTransferMod.o \
                            ForcingVarOutTransferMod.o ConfigVarInTransferMod.o \
                            ForcingVarInTransferMod.o EnergyVarInTransferMod.o \
                            WaterVarInTransferMod.o BiochemVarInTransferMod.o

clean:
	$(RM) *.f90 *.o *.mod
	@# Certain systems with intel compilers generate *.i files
	@# This removes them during the clean process
	$(RM) *.i

.F90.o:
	$(FC) $(CPPFLAGS) $(COREDEF) $(FFLAGS) -c $*.F90 $(CPPINCLUDES) $(FCINCLUDES) -I. -I../../utility -I../../src -I../../../../../framework -I../../../../../external/esmf_time_f90

