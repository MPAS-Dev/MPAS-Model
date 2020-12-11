set(F_SRCS ESMF_AlarmClockMod.F90
    ESMF_AlarmMod.F90
    ESMF_BaseMod.F90
    ESMF_BaseTimeMod.F90
    ESMF_CalendarMod.F90
    ESMF_ClockMod.F90
    ESMF.F90
    ESMF_FractionMod.F90
    ESMF_ShrTimeMod.F90
    ESMF_Stubs.F90
    ESMF_TimeIntervalMod.F90
    ESMF_TimeMod.F90
    MeatMod.F90
    wrf_error_fatal.F90
    wrf_message.F90
    )

add_library(esmf_time ${F_SRCS})
set_target_properties(esmf_time PROPERTIES COMPILE_DEFINITIONS "${Defines};HIDE_MPI")
