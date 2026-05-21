
set(_esmf_time_src
    ESMF_AlarmClockMod.F90
    ESMF_AlarmMod.F90
    ESMF_BaseMod.F90
    ESMF_BaseTimeMod.F90
    ESMF_CalendarMod.F90
    ESMF_ClockMod.F90
    ESMF.F90
    ESMF_FractionMod.F90
    ESMF_Macros.inc
    ESMF_ShrTimeMod.F90
    ESMF_Stubs.F90
    ESMF_TimeIntervalMod.F90
    ESMF_TimeMgr.inc
    ESMF_TimeMod.F90
    MeatMod.F90
    wrf_error_fatal.F90
    wrf_message.F90)

add_library(esmf ${_esmf_time_src})
mpas_fortran_target(esmf)
add_library(${PROJECT_NAME}::external::esmf ALIAS esmf)

target_compile_definitions(esmf PRIVATE HIDE_MPI=1)

target_include_directories(esmf PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>)

target_link_libraries(esmf PUBLIC MPI::MPI_Fortran)

install(TARGETS esmf EXPORT ${PROJECT_NAME}ExportsExternal
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR})

