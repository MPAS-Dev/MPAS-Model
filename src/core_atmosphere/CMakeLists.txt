
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
    mpas_atmphys_driver_lsm_colm2024.F
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


set(CHECKOUT ${CMAKE_CURRENT_SOURCE_DIR}/tools/manage_externals/checkout_externals )
execute_process( COMMAND
                 ${CHECKOUT} --externals ${CMAKE_CURRENT_SOURCE_DIR}/Externals.cfg
                 WORKING_DIRECTORY       ${CMAKE_CURRENT_SOURCE_DIR}/physics/
                 RESULT_VARIABLE         MANAGE_EXTERNALS_STATUS
                )
if ( ${MANAGE_EXTERNALS_STATUS} AND NOT ${MANAGE_EXTERNALS_STATUS} EQUAL 0 )
    message( FATAL_ERROR "Failed to checkout external repos via manage_externals" )
else()
    message( STATUS "Finished checking out external repos via manage_externals" )
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

if(MPAS_COLM2024)
    set(COLM2024_SOURCE_ROOT ${CMAKE_CURRENT_SOURCE_DIR}/physics/physics_colm2024)
    set_property(DIRECTORY APPEND PROPERTY CMAKE_CONFIGURE_DEPENDS
        ${COLM2024_SOURCE_ROOT}/include/define.h)
    set(COLM2024_SOURCE_SEARCH_DIRS
        share
        main
        main/HYDRO
        main/BGC
        main/URBAN
        main/DA
        main/ParaOpt
        drivers/mpas
    )
    foreach(_colm2024_dir IN LISTS COLM2024_SOURCE_SEARCH_DIRS)
        file(GLOB _colm2024_dir_sources CONFIGURE_DEPENDS
            ${COLM2024_SOURCE_ROOT}/${_colm2024_dir}/*.F90)
        list(APPEND COLM2024_SOURCE_CANDIDATES ${_colm2024_dir_sources})
    endforeach()
    list(REMOVE_DUPLICATES COLM2024_SOURCE_CANDIDATES)
    list(SORT COLM2024_SOURCE_CANDIDATES)
    foreach(_colm2024_src IN LISTS COLM2024_SOURCE_CANDIDATES)
        file(RELATIVE_PATH _colm2024_src_rel ${CMAKE_CURRENT_SOURCE_DIR} ${_colm2024_src})
        execute_process(
            COMMAND ${CPP_EXECUTABLE} -traditional-cpp -P -I${COLM2024_SOURCE_ROOT}/include ${_colm2024_src}
            OUTPUT_VARIABLE _colm2024_preprocessed_src
            ERROR_VARIABLE _colm2024_preprocess_error
            RESULT_VARIABLE _colm2024_preprocess_result
        )
        if(NOT _colm2024_preprocess_result EQUAL 0)
            message(FATAL_ERROR "Could not preprocess CoLM2024 source ${_colm2024_src}: ${_colm2024_preprocess_error}")
        endif()
        string(TOUPPER "${_colm2024_preprocessed_src}" _colm2024_preprocessed_src_upper)
        if(NOT "${_colm2024_preprocessed_src_upper}" MATCHES "(^|\n)[ \t]*(MODULE|SUBROUTINE|FUNCTION|PROGRAM)[ \t]+[A-Z0-9_]+")
            list(APPEND ATMOSPHERE_CORE_PHYSICS_COLM2024_SKIPPED_EMPTY_SOURCES ${_colm2024_src_rel})
            continue()
        endif()
        list(APPEND COLM2024_AVAILABLE_SOURCES ${_colm2024_src_rel})
        string(REGEX MATCHALL "(^|\n)[ \t]*MODULE[ \t]+[A-Z][A-Z0-9_]*" _colm2024_module_lines
            "${_colm2024_preprocessed_src_upper}")
        foreach(_colm2024_module_line IN LISTS _colm2024_module_lines)
            if(_colm2024_module_line MATCHES "MODULE[ \t]+PROCEDURE")
                continue()
            endif()
            string(REGEX REPLACE ".*MODULE[ \t]+([A-Z][A-Z0-9_]*).*" "\\1" _colm2024_module_name
                "${_colm2024_module_line}")
            set(COLM2024_MODULE_SOURCE_${_colm2024_module_name} ${_colm2024_src_rel})
        endforeach()
        string(REGEX MATCHALL "(^|\n)[ \t]*USE[^\n]*" _colm2024_use_lines
            "${_colm2024_preprocessed_src_upper}")
        set(_colm2024_source_uses)
        foreach(_colm2024_use_line IN LISTS _colm2024_use_lines)
            if(_colm2024_use_line MATCHES "INTRINSIC")
                continue()
            endif()
            if(_colm2024_use_line MATCHES "USE[ \t]*(,[^:]*)?(::)?[ \t]*([A-Z][A-Z0-9_]*)")
                list(APPEND _colm2024_source_uses ${CMAKE_MATCH_3})
            endif()
        endforeach()
        if(_colm2024_source_uses)
            list(REMOVE_DUPLICATES _colm2024_source_uses)
        endif()
        string(MAKE_C_IDENTIFIER "${_colm2024_src_rel}" _colm2024_src_key)
        set(COLM2024_SOURCE_USES_${_colm2024_src_key} ${_colm2024_source_uses})
    endforeach()
    set(ATMOSPHERE_CORE_PHYSICS_COLM2024_SOURCES
        physics/physics_colm2024/drivers/mpas/MOD_CoLM_MPAS_Interface.F90
        physics/physics_colm2024/main/CoLMDRIVER.F90
        physics/physics_colm2024/main/CoLMMAIN.F90
        physics/physics_colm2024/main/MOD_MonthlyinSituCO2MaunaLoa.F90
    )
    foreach(_colm2024_root IN LISTS ATMOSPHERE_CORE_PHYSICS_COLM2024_SOURCES)
        if(NOT "${_colm2024_root}" IN_LIST COLM2024_AVAILABLE_SOURCES)
            message(FATAL_ERROR "Required embedded CoLM2024 source is unavailable: ${_colm2024_root}")
        endif()
    endforeach()

    set(_colm2024_previous_source_count -1)
    list(LENGTH ATMOSPHERE_CORE_PHYSICS_COLM2024_SOURCES _colm2024_source_count)
    while(NOT _colm2024_previous_source_count EQUAL _colm2024_source_count)
        set(_colm2024_previous_source_count ${_colm2024_source_count})
        foreach(_colm2024_src IN LISTS ATMOSPHERE_CORE_PHYSICS_COLM2024_SOURCES)
            string(MAKE_C_IDENTIFIER "${_colm2024_src}" _colm2024_src_key)
            foreach(_colm2024_use IN LISTS COLM2024_SOURCE_USES_${_colm2024_src_key})
                if(DEFINED COLM2024_MODULE_SOURCE_${_colm2024_use})
                    list(APPEND ATMOSPHERE_CORE_PHYSICS_COLM2024_SOURCES
                        ${COLM2024_MODULE_SOURCE_${_colm2024_use}})
                endif()
            endforeach()
        endforeach()
        list(REMOVE_DUPLICATES ATMOSPHERE_CORE_PHYSICS_COLM2024_SOURCES)
        list(LENGTH ATMOSPHERE_CORE_PHYSICS_COLM2024_SOURCES _colm2024_source_count)
    endwhile()
    list(SORT ATMOSPHERE_CORE_PHYSICS_COLM2024_SOURCES)

    list(LENGTH ATMOSPHERE_CORE_PHYSICS_COLM2024_SKIPPED_EMPTY_SOURCES _colm2024_skipped_empty_count)
    list(LENGTH COLM2024_AVAILABLE_SOURCES _colm2024_available_source_count)
    message(STATUS "CoLM2024 embedded CMake sources: ${_colm2024_source_count} of ${_colm2024_available_source_count}")
    message(STATUS "CoLM2024 embedded CMake skipped empty sources: ${_colm2024_skipped_empty_count}")

    function(_colm2024_object_path out source)
        set(${out} "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/core_atmosphere.dir/${source}.o" PARENT_SCOPE)
    endfunction()

    function(_colm2024_source_depends source)
        if(NOT "${source}" IN_LIST ATMOSPHERE_CORE_PHYSICS_COLM2024_SOURCES)
            return()
        endif()
        set(_colm2024_deps)
        foreach(_colm2024_dep IN LISTS ARGN)
            if(NOT "${_colm2024_dep}" IN_LIST ATMOSPHERE_CORE_PHYSICS_COLM2024_SOURCES)
                continue()
            endif()
            _colm2024_object_path(_colm2024_dep_obj ${_colm2024_dep})
            list(APPEND _colm2024_deps ${_colm2024_dep_obj})
        endforeach()
        if(_colm2024_deps)
            set_property(SOURCE ${source} APPEND PROPERTY OBJECT_DEPENDS "${_colm2024_deps}")
        endif()
    endfunction()

    foreach(_colm2024_src IN LISTS ATMOSPHERE_CORE_PHYSICS_COLM2024_SOURCES)
        string(MAKE_C_IDENTIFIER "${_colm2024_src}" _colm2024_src_key)
        set(_colm2024_dep_sources)
        set(_colm2024_source_uses ${COLM2024_SOURCE_USES_${_colm2024_src_key}})
        foreach(_colm2024_use IN LISTS _colm2024_source_uses)
            if(DEFINED COLM2024_MODULE_SOURCE_${_colm2024_use})
                set(_colm2024_dep_source ${COLM2024_MODULE_SOURCE_${_colm2024_use}})
                if(NOT _colm2024_dep_source STREQUAL _colm2024_src)
                    list(APPEND _colm2024_dep_sources ${_colm2024_dep_source})
                endif()
            endif()
        endforeach()
        if(_colm2024_dep_sources)
            list(REMOVE_DUPLICATES _colm2024_dep_sources)
            _colm2024_source_depends(${_colm2024_src} ${_colm2024_dep_sources})
        endif()
    endforeach()
endif()

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
    mpas_atm_dissipation_models.F
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
            ${ATMOSPHERE_CORE_PHYSICS_COLM2024_SOURCES}
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
if (MPAS_COLM2024)
    list(APPEND CORE_ATMOSPHERE_COMPILE_DEFINITIONS MPAS_COLM2024)
endif ()
target_compile_definitions(core_atmosphere PRIVATE ${CORE_ATMOSPHERE_COMPILE_DEFINITIONS})
if (MPAS_COLM2024)
    target_include_directories(core_atmosphere PRIVATE ${COLM2024_SOURCE_ROOT}/include)
    target_link_libraries(core_atmosphere PUBLIC NetCDF::NetCDF_Fortran NetCDF::NetCDF_C)
    set_property(SOURCE ${ATMOSPHERE_CORE_PHYSICS_COLM2024_SOURCES}
                 PROPERTY Fortran_PREPROCESS ON)
    if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
        set(_colm2024_compile_options -fdefault-real-8 -fdefault-double-8 -ffree-line-length-none)
    elseif(CMAKE_Fortran_COMPILER_ID MATCHES Intel)
        set(_colm2024_compile_options -real-size 64)
    elseif(CMAKE_Fortran_COMPILER_ID MATCHES NVHPC)
        set(_colm2024_compile_options -r8)
    endif()
    if(_colm2024_compile_options)
        set_property(SOURCE ${ATMOSPHERE_CORE_PHYSICS_COLM2024_SOURCES}
                     APPEND PROPERTY COMPILE_OPTIONS ${_colm2024_compile_options})
    endif()
endif ()
set_MPAS_DEBUG_flag(core_atmosphere)
set(ATMOSPHERE_REGISTRY_DEPENDS
    ${CMAKE_CURRENT_SOURCE_DIR}/diagnostics/Registry_diagnostics.xml
    ${CMAKE_CURRENT_SOURCE_DIR}/physics/Registry_noahmp.xml
    ${CMAKE_CURRENT_SOURCE_DIR}/physics/Registry_colm2024.xml)
mpas_core_target(CORE atmosphere TARGET core_atmosphere
                 INCLUDES ${ATMOSPHERE_CORE_INCLUDES}
                 REGISTRY_DEPENDS ${ATMOSPHERE_REGISTRY_DEPENDS})

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
