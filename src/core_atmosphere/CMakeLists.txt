
## Source files
# physics/
set(ATMOSPHERE_CORE_PHYSICS_SOURCES
    ccpp_kind_types.F
    mpas_atmphys_camrad_init.F
    mpas_atmphys_constants.F
    mpas_atmphys_control.F
    mpas_atmphys_date_time.F
    mpas_atmphys_driver_cloudiness.F
    mpas_atmphys_driver_microphysics.F
    mpas_atmphys_driver_oml.F
    mpas_atmphys_finalize.F
    mpas_atmphys_functions.F
    mpas_atmphys_init_microphysics.F
    mpas_atmphys_interface.F
    mpas_atmphys_landuse.F
    mpas_atmphys_lsm_noahinit.F
    mpas_atmphys_manager.F
    mpas_atmphys_o3climatology.F
    mpas_atmphys_rrtmg_lwinit.F
    mpas_atmphys_rrtmg_swinit.F
    mpas_atmphys_sfc_diagnostics.F
    mpas_atmphys_update.F
    mpas_atmphys_update_surface.F
    mpas_atmphys_utilities.F
    mpas_atmphys_driver.F
    mpas_atmphys_driver_convection.F
    mpas_atmphys_driver_gwdo.F
    mpas_atmphys_driver_lsm.F
    mpas_atmphys_driver_pbl.F
    mpas_atmphys_driver_radiation_lw.F
    mpas_atmphys_driver_radiation_sw.F
    mpas_atmphys_driver_seaice.F
    mpas_atmphys_driver_sfclayer.F
    mpas_atmphys_init.F
    mpas_atmphys_lsm_shared.F
    mpas_atmphys_packages.F
    mpas_atmphys_todynamics.F
    mpas_atmphys_vars.F
    mpas_atmphys_driver_lsm_noahmp.F
    mpas_atmphys_lsm_noahmpfinalize.F
    mpas_atmphys_lsm_noahmpinit.F
)
list(TRANSFORM ATMOSPHERE_CORE_PHYSICS_SOURCES PREPEND physics/)

## Unused
# physics/physics_wrf/
set(ATMOSPHERE_CORE_PHYSICS_WRF_SOURCES
    libmassv.F
    cu_ntiedtke_post.F
    cu_ntiedtke_pre.F
    module_bep_bem_helper.F
    module_bl_gwdo.F
    module_bl_ugwp_gwdo.F
    module_bl_ysu.F
    module_cam_error_function.F
    module_cam_shr_kind_mod.F
    module_cam_support.F
    module_cu_gf.mpas.F
    module_mp_kessler.F
    module_mp_radar.F
    module_mp_thompson.F
    module_mp_thompson_cldfra3.F
    module_mp_thompson_aerosols.F
    module_mp_wsm6.F
    module_ra_rrtmg_sw_aerosols.F
    module_ra_cam_support.F
    module_ra_rrtmg_lw.F
    module_ra_rrtmg_sw.F
    module_ra_rrtmg_vinterp.F
    module_sf_bem.F
    module_sf_bep.F
    module_sf_bep_bem.F
    module_sf_noah_seaice.F
    module_sf_noah_seaice_drv.F
    module_sf_noahdrv.F
    module_sf_noahlsm.F
    module_sf_noahlsm_glacial_only.F
    module_sf_oml.F
    module_sf_sfcdiags.F
    module_sf_sfclay.F
    module_sf_sfclayrev.F
    module_sf_urban.F
    bl_mynn_post.F
    bl_mynn_pre.F
    module_bl_mynn.F
    module_cu_kfeta.F
    module_cu_ntiedtke.F
    module_cu_tiedtke.F
    module_ra_cam.F
    module_sf_mynn.F
    sf_mynn_pre.F
    sf_sfclayrev_pre.F
)

list(TRANSFORM ATMOSPHERE_CORE_PHYSICS_WRF_SOURCES PREPEND physics/physics_wrf/)


set(ATMOSPHERE_CORE_PHYSICS_MMM_DIR ${CMAKE_CURRENT_SOURCE_DIR}/physics/physics_mmm)

if(NOT EXISTS ${ATMOSPHERE_CORE_PHYSICS_MMM_DIR})
    set(PHYSICS_MMM_REPO_URL "https://github.com/NCAR/MMM-physics")
    execute_process(COMMAND git clone ${PHYSICS_MMM_REPO_URL} ${ATMOSPHERE_CORE_PHYSICS_MMM_DIR}
                    RESULT_VARIABLE GIT_CLONE_RESULT
                    OUTPUT_VARIABLE GIT_CLONE_OUTPUT
                    ERROR_VARIABLE GIT_CLONE_ERROR)
    if(NOT GIT_CLONE_RESULT EQUAL 0)
        message(FATAL_ERROR "Git clone failed with error: ${GIT_CLONE_ERROR}")
    endif()

else()
    message(STATUS "Directory ${DIR_TO_CHECK} already exists, skipping clone")
endif()

set(ATMOSPHERE_CORE_PHYSICS_MMM_SOURCES
    bl_gwdo.F90
    bl_ysu.F90
    cu_ntiedtke.F90
    module_libmassv.F90
    mp_wsm6.F90
    mp_wsm6_effectRad.F90
    bl_mynn.F90
    bl_mynn_subroutines.F90
    mp_radar.F90
    mynn_shared.F90
    sf_mynn.F90
    sf_sfclayrev.F90
)

list(TRANSFORM ATMOSPHERE_CORE_PHYSICS_MMM_SOURCES PREPEND physics/physics_mmm/)

set(ATMOSPHERE_CORE_PHYSICS_NOAA_DIR ${CMAKE_CURRENT_SOURCE_DIR}/physics/physics_noaa/UGWP)

if(NOT EXISTS ${ATMOSPHERE_CORE_PHYSICS_NOAA_DIR})
    set(PHYSICS_NOAA_REPO_URL "https://github.com/NOAA-GSL/UGWP.git")
    execute_process(COMMAND git clone ${PHYSICS_NOAA_REPO_URL} ${ATMOSPHERE_CORE_PHYSICS_NOAA_DIR}
                    RESULT_VARIABLE GIT_CLONE_RESULT
                    OUTPUT_VARIABLE GIT_CLONE_OUTPUT
                    ERROR_VARIABLE GIT_CLONE_ERROR)
    if(NOT GIT_CLONE_RESULT EQUAL 0)
        message(FATAL_ERROR "Git clone failed with error: ${GIT_CLONE_ERROR}")
    endif()

else()
    message(STATUS "Directory ${DIR_TO_CHECK} already exists, skipping clone")
endif()

set(ATMOSPHERE_CORE_PHYSICS_NOAA_SOURCES
    bl_ugwp.F
    bl_ugwpv1_ngw.F
    cires_tauamf_data.F
    cires_ugwpv1_initialize.F
    cires_ugwpv1_module.F
    cires_ugwpv1_solv2.F
    cires_ugwpv1_triggers.F
)

list(TRANSFORM ATMOSPHERE_CORE_PHYSICS_NOAA_SOURCES PREPEND physics/physics_noaa/UGWP/)

set(ATMOSPHERE_CORE_PHYSICS_NOAMP_UTILITY_SOURCES
    CheckNanMod.F90
    Machine.F90
)
list(TRANSFORM ATMOSPHERE_CORE_PHYSICS_NOAMP_UTILITY_SOURCES PREPEND physics/physics_noahmp/utility/)

set(ATMOSPHERE_CORE_PHYSICS_NOAMP_MPAS_DRIVER_SOURCES
    BiochemVarInTransferMod.F90
    ConfigVarOutTransferMod.F90
    ForcingVarInTransferMod.F90
    NoahmpDriverMainMod.F90
    NoahmpIOVarFinalizeMod.F90
    NoahmpReadNamelistMod.F90
    PedoTransferSR2006Mod.F90
    BiochemVarOutTransferMod.F90
    EnergyVarInTransferMod.F90
    ForcingVarOutTransferMod.F90
    NoahmpIOVarInitMod.F90
    NoahmpReadTableMod.F90
    WaterVarInTransferMod.F90
    ConfigVarInTransferMod.F90
    EnergyVarOutTransferMod.F90
    NoahmpInitMainMod.F90
    NoahmpIOVarType.F90
    NoahmpSnowInitMod.F90
    WaterVarOutTransferMod.F90
)
list(TRANSFORM ATMOSPHERE_CORE_PHYSICS_NOAMP_MPAS_DRIVER_SOURCES PREPEND physics/physics_noahmp/drivers/mpas/)

set(ATMOSPHERE_CORE_PHYSICS_NOAMP_SRC_SOURCES
    AtmosForcingMod.F90
    BalanceErrorCheckGlacierMod.F90
    BalanceErrorCheckMod.F90
    BiochemCropMainMod.F90
    BiochemNatureVegMainMod.F90
    BiochemVarInitMod.F90
    BiochemVarType.F90
    CanopyHydrologyMod.F90
    CanopyRadiationTwoStreamMod.F90
    CanopyWaterInterceptMod.F90
    CarbonFluxCropMod.F90
    CarbonFluxNatureVegMod.F90
    ConfigVarInitMod.F90
    ConfigVarType.F90
    ConstantDefineMod.F90
    CropGrowDegreeDayMod.F90
    CropPhotosynthesisMod.F90
    EnergyMainGlacierMod.F90
    EnergyMainMod.F90
    EnergyVarInitMod.F90
    EnergyVarType.F90
    ForcingVarInitMod.F90
    ForcingVarType.F90
    GeneralInitGlacierMod.F90
    GeneralInitMod.F90
    GlacierIceThermalPropertyMod.F90
    GlacierPhaseChangeMod.F90
    GlacierTemperatureMainMod.F90
    GlacierTemperatureSolverMod.F90
    GlacierThermalDiffusionMod.F90
    GroundAlbedoGlacierMod.F90
    GroundAlbedoMod.F90
    GroundRoughnessPropertyGlacierMod.F90
    GroundRoughnessPropertyMod.F90
    GroundThermalPropertyGlacierMod.F90
    GroundThermalPropertyMod.F90
    GroundWaterMmfMod.F90
    GroundWaterTopModelMod.F90
    HumiditySaturationMod.F90
    IrrigationFloodMod.F90
    IrrigationInfilPhilipMod.F90
    IrrigationMicroMod.F90
    IrrigationPrepareMod.F90
    IrrigationSprinklerMod.F90
    IrrigationTriggerMod.F90
    Makefile
    MatrixSolverTriDiagonalMod.F90
    NoahmpMainGlacierMod.F90
    NoahmpMainMod.F90
    NoahmpVarType.F90
    PhenologyMainMod.F90
    PrecipitationHeatAdvectGlacierMod.F90
    PrecipitationHeatAdvectMod.F90
    PsychrometricVariableGlacierMod.F90
    PsychrometricVariableMod.F90
    ResistanceAboveCanopyChen97Mod.F90
    ResistanceAboveCanopyMostMod.F90
    ResistanceBareGroundChen97Mod.F90
    ResistanceBareGroundMostMod.F90
    ResistanceCanopyStomataBallBerryMod.F90
    ResistanceCanopyStomataJarvisMod.F90
    ResistanceGroundEvaporationGlacierMod.F90
    ResistanceGroundEvaporationMod.F90
    ResistanceLeafToGroundMod.F90
    RunoffSubSurfaceDrainageMod.F90
    RunoffSubSurfaceEquiWaterTableMod.F90
    RunoffSubSurfaceGroundWaterMod.F90
    RunoffSubSurfaceShallowMmfMod.F90
    RunoffSurfaceBatsMod.F90
    RunoffSurfaceDynamicVicMod.F90
    RunoffSurfaceExcessDynamicVicMod.F90
    RunoffSurfaceFreeDrainMod.F90
    RunoffSurfaceTopModelEquiMod.F90
    RunoffSurfaceTopModelGrdMod.F90
    RunoffSurfaceTopModelMmfMod.F90
    RunoffSurfaceVicMod.F90
    RunoffSurfaceXinAnJiangMod.F90
    ShallowWaterTableMmfMod.F90
    SnowAgingBatsMod.F90
    SnowAlbedoBatsMod.F90
    SnowAlbedoClassMod.F90
    SnowCoverGlacierMod.F90
    SnowCoverGroundNiu07Mod.F90
    SnowfallBelowCanopyMod.F90
    SnowLayerCombineMod.F90
    SnowLayerDivideMod.F90
    SnowLayerWaterComboMod.F90
    SnowpackCompactionMod.F90
    SnowpackHydrologyGlacierMod.F90
    SnowpackHydrologyMod.F90
    SnowThermalPropertyMod.F90
    SnowWaterMainGlacierMod.F90
    SnowWaterMainMod.F90
    SoilHydraulicPropertyMod.F90
    SoilMoistureSolverMod.F90
    SoilSnowTemperatureMainMod.F90
    SoilSnowTemperatureSolverMod.F90
    SoilSnowThermalDiffusionMod.F90
    SoilSnowWaterPhaseChangeMod.F90
    SoilThermalPropertyMod.F90
    SoilWaterDiffusionRichardsMod.F90
    SoilWaterInfilGreenAmptMod.F90
    SoilWaterInfilPhilipMod.F90
    SoilWaterInfilSmithParlangeMod.F90
    SoilWaterMainMod.F90
    SoilWaterSupercoolKoren99Mod.F90
    SoilWaterSupercoolNiu06Mod.F90
    SoilWaterTranspirationMod.F90
    SurfaceAlbedoGlacierMod.F90
    SurfaceAlbedoMod.F90
    SurfaceEmissivityGlacierMod.F90
    SurfaceEmissivityMod.F90
    SurfaceEnergyFluxBareGroundMod.F90
    SurfaceEnergyFluxGlacierMod.F90
    SurfaceEnergyFluxVegetatedMod.F90
    SurfaceRadiationGlacierMod.F90
    SurfaceRadiationMod.F90
    TileDrainageEquiDepthMod.F90
    TileDrainageHooghoudtMod.F90
    TileDrainageSimpleMod.F90
    VaporPressureSaturationMod.F90
    WaterMainGlacierMod.F90
    WaterMainMod.F90
    WaterTableDepthSearchMod.F90
    WaterTableEquilibriumMod.F90
    WaterVarInitMod.F90
    WaterVarType.F90
)
list(TRANSFORM ATMOSPHERE_CORE_PHYSICS_NOAMP_SRC_SOURCES PREPEND physics/physics_noahmp/src/)

# diagnostics/
set(ATMOSPHERE_CORE_DIAGNOSTIC_SOURCES
    mpas_atm_diagnostic_template.F
    mpas_atm_diagnostics_manager.F
    mpas_atm_diagnostics_utils.F
    mpas_cloud_diagnostics.F
    mpas_convective_diagnostics.F
    mpas_isobaric_diagnostics.F
    mpas_pv_diagnostics.F
    mpas_soundings.F
)

list(TRANSFORM ATMOSPHERE_CORE_DIAGNOSTIC_SOURCES PREPEND diagnostics/)

# dynamics/
set(ATMOSPHERE_CORE_DYNAMICS_SOURCES
    mpas_atm_boundaries.F
    mpas_atm_iau.F
    mpas_atm_time_integration.F)
list(TRANSFORM ATMOSPHERE_CORE_DYNAMICS_SOURCES PREPEND dynamics/)

# utils/
set(ATMOSPHERE_CORE_UTILS_SOURCES
    atmphys_build_tables_thompson.F
    build_tables.F)
list(TRANSFORM ATMOSPHERE_CORE_UTILS_SOURCES PREPEND utils/)

# core_atosphere
set(ATMOSPHERE_CORE_SOURCES
    mpas_atm_dimensions.F
    mpas_atm_threading.F
    mpas_atm_core.F
    mpas_atm_core_interface.F
    mpas_atm_halos.F
)

## Generated includes
set(ATMOSPHERE_CORE_INCLUDES
    block_dimension_routines.inc
    core_variables.inc
    define_packages.inc
    domain_variables.inc
    namelist_call.inc
    namelist_defines.inc
    setup_immutable_streams.inc
    structs_and_variables.inc)


add_library(core_atmosphere ${ATMOSPHERE_CORE_SOURCES}
            ${ATMOSPHERE_CORE_PHYSICS_NOAMP_UTILITY_SOURCES}
            ${ATMOSPHERE_CORE_PHYSICS_NOAMP_MPAS_DRIVER_SOURCES}
            ${ATMOSPHERE_CORE_PHYSICS_NOAMP_SRC_SOURCES}
            ${ATMOSPHERE_CORE_PHYSICS_SOURCES}
            ${ATMOSPHERE_CORE_PHYSICS_MMM_SOURCES}
            ${ATMOSPHERE_CORE_PHYSICS_WRF_SOURCES}
            ${ATMOSPHERE_CORE_PHYSICS_NOAA_SOURCES}
            ${ATMOSPHERE_CORE_DIAGNOSTIC_SOURCES}
            ${ATMOSPHERE_CORE_DYNAMICS_SOURCES})

set(CORE_ATMOSPHERE_COMPILE_DEFINITIONS
    mpas=1
    MPAS_NATIVE_TIMERS
)
if (${DO_PHYSICS})
    list(APPEND CORE_ATMOSPHERE_COMPILE_DEFINITIONS DO_PHYSICS)
endif ()
target_compile_definitions(core_atmosphere PRIVATE ${CORE_ATMOSPHERE_COMPILE_DEFINITIONS})
set_MPAS_DEBUG_flag(core_atmosphere)
mpas_core_target(CORE atmosphere TARGET core_atmosphere INCLUDES ${ATMOSPHERE_CORE_INCLUDES})

#Get physics_wrf tables from MPAS-Data
include(FetchContent)
if (${PROJECT_VERSION} VERSION_GREATER_EQUAL 7.0)
    set(MPAS_DATA_GIT_TAG v${PROJECT_VERSION_MAJOR}.0)
else ()
    set(MPAS_DATA_GIT_TAG master)
endif ()

FetchContent_Declare(mpas_data
                     GIT_REPOSITORY https://github.com/MPAS-Dev/MPAS-Data.git
                     GIT_TAG ${MPAS_DATA_GIT_TAG}
                     GIT_PROGRESS True
                     GIT_SHALLOW True)
FetchContent_Populate(mpas_data)
message(STATUS "MPAS-Data source dir: ${mpas_data_SOURCE_DIR}")
set(PHYSICS_WRF_DATA_DIR ${mpas_data_SOURCE_DIR}/atmosphere/physics_wrf/files)
file(GLOB PHYSICS_WRF_DATA RELATIVE ${PHYSICS_WRF_DATA_DIR} "${PHYSICS_WRF_DATA_DIR}/*")
file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/${PROJECT_NAME}/core_atmosphere)
foreach (data_file IN LISTS PHYSICS_WRF_DATA)
    execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink ${PHYSICS_WRF_DATA_DIR}/${data_file}
                    ${CMAKE_BINARY_DIR}/${PROJECT_NAME}/core_atmosphere/${data_file})
endforeach ()
install(DIRECTORY ${PHYSICS_WRF_DATA_DIR}/ DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}/core_atmosphere)

add_executable(mpas_atmosphere_build_tables ${ATMOSPHERE_CORE_UTILS_SOURCES})
target_link_libraries(mpas_atmosphere_build_tables PUBLIC core_atmosphere)
mpas_fortran_target(mpas_atmosphere_build_tables)
install(TARGETS mpas_atmosphere_build_tables EXPORT ${PROJECT_NAME}ExportsCore
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
