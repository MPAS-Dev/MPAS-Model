# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F90

include ../../hrldas/user_build_options

OBJS = ConstantDefineMod.o \
       ConfigVarType.o \
       ForcingVarType.o \
       EnergyVarType.o \
       WaterVarType.o \
       BiochemVarType.o \
       NoahmpVarType.o \
       ConfigVarInitMod.o \
       ForcingVarInitMod.o \
       EnergyVarInitMod.o \
       WaterVarInitMod.o \
       BiochemVarInitMod.o \
       CanopyHydrologyMod.o \
       GroundWaterTopModelMod.o \
       IrrigationFloodMod.o \
       IrrigationInfilPhilipMod.o \
       IrrigationMicroMod.o \
       MatrixSolverTriDiagonalMod.o \
       RunoffSubSurfaceDrainageMod.o \
       RunoffSubSurfaceEquiWaterTableMod.o \
       RunoffSubSurfaceGroundWaterMod.o \
       RunoffSubSurfaceShallowMmfMod.o \
       RunoffSurfaceBatsMod.o \
       RunoffSurfaceDynamicVicMod.o \
       RunoffSurfaceExcessDynamicVicMod.o \
       RunoffSurfaceFreeDrainMod.o \
       RunoffSurfaceTopModelEquiMod.o \
       RunoffSurfaceTopModelGrdMod.o \
       RunoffSurfaceTopModelMmfMod.o \
       RunoffSurfaceVicMod.o \
       RunoffSurfaceXinAnJiangMod.o \
       ShallowWaterTableMmfMod.o \
       SnowfallBelowCanopyMod.o \
       SnowLayerCombineMod.o \
       SnowLayerDivideMod.o \
       SnowLayerWaterComboMod.o \
       SnowpackCompactionMod.o \
       SnowpackHydrologyMod.o \
       SnowWaterMainMod.o \
       SoilHydraulicPropertyMod.o \
       SoilMoistureSolverMod.o \
       SoilWaterDiffusionRichardsMod.o \
       SoilWaterInfilGreenAmptMod.o \
       SoilWaterInfilPhilipMod.o \
       SoilWaterInfilSmithParlangeMod.o \
       SoilWaterMainMod.o \
       TileDrainageEquiDepthMod.o \
       TileDrainageHooghoudtMod.o \
       TileDrainageSimpleMod.o \
       WaterMainMod.o \
       WaterTableDepthSearchMod.o \
       WaterTableEquilibriumMod.o \
       IrrigationTriggerMod.o \
       IrrigationSprinklerMod.o \
       CanopyWaterInterceptMod.o \
       PrecipitationHeatAdvectMod.o \
       SnowThermalPropertyMod.o \
       SoilThermalPropertyMod.o \
       GroundThermalPropertyMod.o \
       EnergyMainMod.o \
       NoahmpMainMod.o \
       SnowAgingBatsMod.o \
       SnowAlbedoBatsMod.o \
       SnowAlbedoClassMod.o \
       GroundAlbedoMod.o \
       CanopyRadiationTwoStreamMod.o \
       SurfaceAlbedoMod.o \
       SurfaceRadiationMod.o \
       HumiditySaturationMod.o \
       ResistanceAboveCanopyChen97Mod.o \
       ResistanceAboveCanopyMostMod.o \
       ResistanceCanopyStomataBallBerryMod.o \
       ResistanceCanopyStomataJarvisMod.o \
       ResistanceLeafToGroundMod.o \
       VaporPressureSaturationMod.o \
       SurfaceEnergyFluxVegetatedMod.o \
       ResistanceBareGroundChen97Mod.o \
       ResistanceBareGroundMostMod.o \
       SurfaceEnergyFluxBareGroundMod.o \
       SoilSnowTemperatureMainMod.o \
       SoilSnowTemperatureSolverMod.o \
       SoilSnowThermalDiffusionMod.o \
       SoilSnowWaterPhaseChangeMod.o \
       SoilWaterSupercoolKoren99Mod.o \
       SoilWaterSupercoolNiu06Mod.o \
       SnowCoverGroundNiu07Mod.o \
       GroundRoughnessPropertyMod.o \
       SurfaceEmissivityMod.o \
       PsychrometricVariableMod.o \
       ResistanceGroundEvaporationMod.o \
       SoilWaterTranspirationMod.o \
       AtmosForcingMod.o \
       PhenologyMainMod.o \
       BiochemCropMainMod.o \
       BiochemNatureVegMainMod.o \
       CarbonFluxCropMod.o \
       CarbonFluxNatureVegMod.o \
       CropGrowDegreeDayMod.o \
       CropPhotosynthesisMod.o \
       IrrigationPrepareMod.o \
       BalanceErrorCheckMod.o \
       GeneralInitMod.o \
       GroundWaterMmfMod.o \
       BalanceErrorCheckGlacierMod.o \
       EnergyMainGlacierMod.o \
       GeneralInitGlacierMod.o \
       GlacierIceThermalPropertyMod.o \
       GlacierPhaseChangeMod.o \
       GlacierTemperatureMainMod.o \
       GlacierTemperatureSolverMod.o \
       GlacierThermalDiffusionMod.o \
       GroundAlbedoGlacierMod.o \
       GroundRoughnessPropertyGlacierMod.o \
       GroundThermalPropertyGlacierMod.o \
       NoahmpMainGlacierMod.o \
       PrecipitationHeatAdvectGlacierMod.o \
       PsychrometricVariableGlacierMod.o \
       ResistanceGroundEvaporationGlacierMod.o \
       SnowCoverGlacierMod.o \
       SnowWaterMainGlacierMod.o \
       SnowpackHydrologyGlacierMod.o \
       SurfaceAlbedoGlacierMod.o \
       SurfaceEmissivityGlacierMod.o \
       SurfaceEnergyFluxGlacierMod.o \
       SurfaceRadiationGlacierMod.o \
       WaterMainGlacierMod.o

all:	$(OBJS)

GroundWaterMmfMod.o: GroundWaterMmfMod.F90
	@echo ""
	$(RM) GroundWaterMmfMod.f90
	$(CPP) $(CPPFLAGS) $(*).F90 > $(*).f90
	$(COMPILERF90) -c -I../../hrldas/MPP -I. -I../../hrldas/Utility_routines \
        -I../utility -I../drivers/hrldas $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) $(*).f90
	@echo ""

.F90.o:
	@echo ""
	$(RM) $(*).f90
	$(CPP) $(CPPFLAGS) $(*).F90 > $(*).f90
	$(COMPILERF90) -c -I. -I../utility -I../drivers/hrldas $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) $(*).f90
	@echo ""

clean:
	rm -f *.o *.mod *.exe *.f90

#
# Dependencies:
#

ConstantDefineMod.o:                  ../utility/Machine.o
ConfigVarType.o:                      ../utility/Machine.o
ForcingVarType.o:                     ../utility/Machine.o
EnergyVarType.o:                      ../utility/Machine.o
WaterVarType.o:                       ../utility/Machine.o
BiochemVarType.o:                     ../utility/Machine.o
NoahmpVarType.o:                      ConfigVarType.o ForcingVarType.o EnergyVarType.o \
                                      WaterVarType.o BiochemVarType.o
ConfigVarInitMod.o:                   ../utility/Machine.o NoahmpVarType.o
ForcingVarInitMod.o:                  ../utility/Machine.o NoahmpVarType.o
EnergyVarInitMod.o:                   ../utility/Machine.o NoahmpVarType.o
WaterVarInitMod.o:                    ../utility/Machine.o NoahmpVarType.o
BiochemVarInitMod.o:                  ../utility/Machine.o NoahmpVarType.o
CanopyHydrologyMod.o:                 ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
GroundWaterTopModelMod.o:             ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
IrrigationFloodMod.o:                 ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                      IrrigationInfilPhilipMod.o
IrrigationInfilPhilipMod.o:           ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                      SoilHydraulicPropertyMod.o
IrrigationMicroMod.o:                 ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                      IrrigationInfilPhilipMod.o
MatrixSolverTriDiagonalMod.o:         ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
RunoffSubSurfaceDrainageMod.o:        ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
RunoffSubSurfaceEquiWaterTableMod.o:  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                      WaterTableEquilibriumMod.o
RunoffSubSurfaceGroundWaterMod.o:     ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                      GroundWaterTopModelMod.o
RunoffSubSurfaceShallowMmfMod.o:      ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                      ShallowWaterTableMmfMod.o
RunoffSurfaceBatsMod.o:               ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
RunoffSurfaceDynamicVicMod.o:         ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                      SoilWaterInfilPhilipMod.o RunoffSurfaceExcessDynamicVicMod.o \
                                      SoilWaterInfilSmithParlangeMod.o SoilWaterInfilGreenAmptMod.o
RunoffSurfaceExcessDynamicVicMod.o:   ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
RunoffSurfaceFreeDrainMod.o:          ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                      SoilHydraulicPropertyMod.o
RunoffSurfaceTopModelEquiMod.o:       ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
RunoffSurfaceTopModelGrdMod.o:        ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
RunoffSurfaceTopModelMmfMod.o:        ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
RunoffSurfaceVicMod.o:                ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
RunoffSurfaceXinAnJiangMod.o:         ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
ShallowWaterTableMmfMod.o:            ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
SnowfallBelowCanopyMod.o:             ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
SnowLayerCombineMod.o:                ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                      SnowLayerWaterComboMod.o
SnowLayerDivideMod.o:                 ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                      SnowLayerWaterComboMod.o
SnowLayerWaterComboMod.o:             ../utility/Machine.o ConstantDefineMod.o
SnowpackCompactionMod.o:              ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
SnowpackHydrologyMod.o:               ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                      SnowLayerCombineMod.o
SnowWaterMainMod.o:                   ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                      SnowfallBelowCanopyMod.o SnowpackCompactionMod.o SnowLayerDivideMod.o \
                                      SnowLayerCombineMod.o SnowpackHydrologyMod.o
SoilHydraulicPropertyMod.o:           ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
SoilMoistureSolverMod.o:              ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                      MatrixSolverTriDiagonalMod.o
SoilWaterDiffusionRichardsMod.o:      ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                      SoilHydraulicPropertyMod.o
SoilWaterInfilGreenAmptMod.o:         ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                      SoilHydraulicPropertyMod.o
SoilWaterInfilPhilipMod.o:            ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                      SoilHydraulicPropertyMod.o
SoilWaterInfilSmithParlangeMod.o:     ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                      SoilHydraulicPropertyMod.o
SoilWaterMainMod.o:                   ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                      RunoffSurfaceTopModelGrdMod.o RunoffSurfaceTopModelEquiMod.o \
                                      RunoffSurfaceFreeDrainMod.o RunoffSurfaceBatsMod.o \
                                      RunoffSurfaceTopModelMmfMod.o RunoffSurfaceVicMod.o \
                                      RunoffSurfaceXinAnJiangMod.o RunoffSurfaceDynamicVicMod.o \
                                      RunoffSubSurfaceEquiWaterTableMod.o RunoffSubSurfaceGroundWaterMod.o \
                                      RunoffSubSurfaceDrainageMod.o RunoffSubSurfaceShallowMmfMod.o \
                                      SoilWaterDiffusionRichardsMod.o SoilMoistureSolverMod.o \
                                      TileDrainageSimpleMod.o TileDrainageHooghoudtMod.o
TileDrainageEquiDepthMod.o:           ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
TileDrainageHooghoudtMod.o:           ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                      TileDrainageEquiDepthMod.o WaterTableDepthSearchMod.o \
                                      WaterTableEquilibriumMod.o
TileDrainageSimpleMod.o:              ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
WaterMainMod.o:                       ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                      CanopyHydrologyMod.o SnowWaterMainMod.o IrrigationFloodMod.o \
                                      IrrigationMicroMod.o SoilWaterMainMod.o
WaterTableDepthSearchMod.o:           ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
WaterTableEquilibriumMod.o:           ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
IrrigationTriggerMod.o:               ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
IrrigationSprinklerMod.o:             ../utility/Machine.o ../utility/CheckNanMod.o \
                                      NoahmpVarType.o ConstantDefineMod.o IrrigationInfilPhilipMod.o
CanopyWaterInterceptMod.o:            ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
PrecipitationHeatAdvectMod.o:         ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
SnowThermalPropertyMod.o:             ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
SoilThermalPropertyMod.o:             ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
GroundThermalPropertyMod.o:           ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                      SnowThermalPropertyMod.o SoilThermalPropertyMod.o
CanopyRadiationTwoStreamMod.o:        ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
GroundAlbedoMod.o:                    ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
SnowAgingBatsMod.o:                   ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
SnowAlbedoBatsMod.o:                  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
SnowAlbedoClassMod.o:                 ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
SurfaceAlbedoMod.o:                   ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                      SnowAgingBatsMod.o SnowAlbedoBatsMod.o SnowAlbedoClassMod.o \
                                      GroundAlbedoMod.o CanopyRadiationTwoStreamMod.o
SurfaceRadiationMod.o:                ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
EnergyMainMod.o:                      ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                      GroundThermalPropertyMod.o SurfaceEnergyFluxVegetatedMod.o \
                                      SurfaceEnergyFluxBareGroundMod.o SoilSnowTemperatureMainMod.o \
                                      SoilSnowWaterPhaseChangeMod.o SnowCoverGroundNiu07Mod.o SurfaceEmissivityMod.o \
                                      GroundRoughnessPropertyMod.o PsychrometricVariableMod.o ResistanceGroundEvaporationMod.o \
                                      SoilWaterTranspirationMod.o SurfaceAlbedoMod.o SurfaceRadiationMod.o
NoahmpMainMod.o:                      ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                      IrrigationPrepareMod.o IrrigationSprinklerMod.o CanopyWaterInterceptMod.o \
                                      PrecipitationHeatAdvectMod.o EnergyMainMod.o WaterMainMod.o AtmosForcingMod.o \
                                      BiochemCropMainMod.o BiochemNatureVegMainMod.o PhenologyMainMod.o BalanceErrorCheckMod.o \
                                      GeneralInitMod.o
HumiditySaturationMod.o:                 ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
ResistanceAboveCanopyChen97Mod.o:        ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
ResistanceAboveCanopyMostMod.o:          ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
ResistanceCanopyStomataBallBerryMod.o:   ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
ResistanceCanopyStomataJarvisMod.o:      ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                         HumiditySaturationMod.o
ResistanceLeafToGroundMod.o:             ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
VaporPressureSaturationMod.o:            ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
SurfaceEnergyFluxVegetatedMod.o:         ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                         VaporPressureSaturationMod.o ResistanceAboveCanopyMostMod.o \
                                         ResistanceAboveCanopyChen97Mod.o ResistanceLeafToGroundMod.o \
                                         ResistanceCanopyStomataBallBerryMod.o ResistanceCanopyStomataJarvisMod.o
ResistanceBareGroundChen97Mod.o:         ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
ResistanceBareGroundMostMod.o:           ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
SurfaceEnergyFluxBareGroundMod.o:        ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                         VaporPressureSaturationMod.o ResistanceBareGroundMostMod.o \
                                         ResistanceBareGroundChen97Mod.o
SoilSnowTemperatureSolverMod.o:          ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                         MatrixSolverTriDiagonalMod.o
SoilSnowThermalDiffusionMod.o:           ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
SoilSnowTemperatureMainMod.o:            ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                         SoilSnowTemperatureSolverMod.o SoilSnowThermalDiffusionMod.o
SoilWaterSupercoolKoren99Mod.o:          ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
SoilWaterSupercoolNiu06Mod.o:            ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
SoilSnowWaterPhaseChangeMod.o:           ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                         SoilWaterSupercoolKoren99Mod.o SoilWaterSupercoolNiu06Mod.o
GroundRoughnessPropertyMod.o:            ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
PsychrometricVariableMod.o:              ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
ResistanceGroundEvaporationMod.o:        ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
SnowCoverGroundNiu07Mod.o:               ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
SoilWaterTranspirationMod.o:             ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
SurfaceEmissivityMod.o:                  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
AtmosForcingMod.o:                       ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
PhenologyMainMod.o:                      ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
CropPhotosynthesisMod.o:                 ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
CropGrowDegreeDayMod.o:                  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
CarbonFluxNatureVegMod.o:                ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
CarbonFluxCropMod.o:                     ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
BiochemNatureVegMainMod.o:               ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o CarbonFluxNatureVegMod.o
BiochemCropMainMod.o:                    ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o CarbonFluxCropMod.o \
                                         CropGrowDegreeDayMod.o CropPhotosynthesisMod.o
IrrigationPrepareMod.o:                  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o IrrigationTriggerMod.o
BalanceErrorCheckMod.o:                  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
GeneralInitMod.o:                        ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
GroundWaterMmfMod.o:                     ../utility/Machine.o NoahmpVarType.o ../drivers/hrldas/NoahmpIOVarType.o
BalanceErrorCheckGlacierMod.o:           ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
EnergyMainGlacierMod.o:                  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o SnowCoverGlacierMod.o \
                                         GroundRoughnessPropertyGlacierMod.o GroundThermalPropertyGlacierMod.o \
                                         SurfaceAlbedoGlacierMod.o SurfaceRadiationGlacierMod.o SurfaceEmissivityGlacierMod.o \
                                         ResistanceGroundEvaporationGlacierMod.o PsychrometricVariableGlacierMod.o \
                                         SurfaceEnergyFluxGlacierMod.o GlacierTemperatureMainMod.o GlacierPhaseChangeMod.o
GeneralInitGlacierMod.o:                 ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
GlacierIceThermalPropertyMod.o:          ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
GlacierPhaseChangeMod.o:                 ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
GlacierTemperatureMainMod.o:             ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                         GlacierTemperatureSolverMod.o GlacierThermalDiffusionMod.o
GlacierTemperatureSolverMod.o:           ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o MatrixSolverTriDiagonalMod.o
GlacierThermalDiffusionMod.o:            ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
GroundAlbedoGlacierMod.o:                ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
GroundRoughnessPropertyGlacierMod.o:     ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
GroundThermalPropertyGlacierMod.o:       ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                         SnowThermalPropertyMod.o GlacierIceThermalPropertyMod.o
NoahmpMainGlacierMod.o:                  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o AtmosForcingMod.o \
                                         GeneralInitGlacierMod.o PrecipitationHeatAdvectGlacierMod.o EnergyMainGlacierMod.o \
                                         WaterMainGlacierMod.o BalanceErrorCheckGlacierMod.o
PrecipitationHeatAdvectGlacierMod.o:     ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
PsychrometricVariableGlacierMod.o:       ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
ResistanceGroundEvaporationGlacierMod.o: ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
SnowCoverGlacierMod.o:                   ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
SnowWaterMainGlacierMod.o:               ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o SnowfallBelowCanopyMod.o \
                                         SnowpackCompactionMod.o SnowLayerCombineMod.o SnowLayerDivideMod.o \
                                         SnowpackHydrologyGlacierMod.o 
SnowpackHydrologyGlacierMod.o:           ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o SnowLayerCombineMod.o
SurfaceAlbedoGlacierMod.o:               ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o SnowAgingBatsMod.o \
                                         SnowAlbedoBatsMod.o SnowAlbedoClassMod.o GroundAlbedoGlacierMod.o
SurfaceEmissivityGlacierMod.o:           ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
SurfaceEnergyFluxGlacierMod.o:           ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                         VaporPressureSaturationMod.o ResistanceBareGroundMostMod.o
SurfaceRadiationGlacierMod.o:            ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
WaterMainGlacierMod.o:                   ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o SnowWaterMainGlacierMod.o

