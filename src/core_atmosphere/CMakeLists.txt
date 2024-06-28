
## Source files
# physics/
set(ATMOSPHERE_CORE_PHYSICS_SOURCES
        ccpp_kinds.F
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
)
list(TRANSFORM ATMOSPHERE_CORE_PHYSICS_SOURCES PREPEND physics/)

## Unused
# physics/physics_wrf/
set(ATMOSPHERE_CORE_PHYSICS_WRF_SOURCES
        libmassv.F
        module_bep_bem_helper.F
        module_bl_gwdo.F
        module_bl_ysu.F
        module_cam_error_function.F
        module_cam_shr_kind_mod.F
        module_cam_support.F
        module_cu_gf.mpas.F
        module_mp_kessler.F
        module_mp_radar.F
        module_mp_thompson.F
        module_mp_thompson_cldfra3.F
        module_mp_wsm6.F
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
)

list(TRANSFORM ATMOSPHERE_CORE_PHYSICS_WRF_SOURCES PREPEND physics/physics_wrf/)

set(ATMOSPHERE_CORE_PHYSICS_MMM_SOURCES
        bl_gwdo.F
        bl_ysu.F
        cu_ntiedtke.F
        module_libmassv.F
        mp_wsm6.F
        mp_wsm6_effectRad.F
        bl_mynn.F
        bl_mynn_subroutines.F
        mp_radar.F
        mynn_shared.F
        sf_mynn.F
        sf_sfclayrev.F
)

list(TRANSFORM ATMOSPHERE_CORE_PHYSICS_MMM_SOURCES PREPEND physics/physics_mmm/)

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
        ${ATMOSPHERE_CORE_PHYSICS_SOURCES}
        ${ATMOSPHERE_CORE_PHYSICS_MMM_SOURCES}
        ${ATMOSPHERE_CORE_PHYSICS_WRF_SOURCES}
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
