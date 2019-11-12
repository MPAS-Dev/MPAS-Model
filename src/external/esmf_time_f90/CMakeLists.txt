add_library(esmf_time)

target_sources(esmf_time
               PRIVATE
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

set_property(GLOBAL APPEND PROPERTY MPAS_INTERNAL_INCLUDE_DIRS ${CMAKE_BINARY_DIR}/src/external/esmf_time_f90)
set_property(GLOBAL APPEND PROPERTY MPAS_INTERNAL_LIBRARIES esmf_time)
