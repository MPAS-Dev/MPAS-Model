!  stfod_diagnostics
!
!  Created by John Wong, August / 2019
!  Copyright (c) 2019 The Weather Company. All rights reserved.
!
!  This module provides additional diagnostics for short-term FOD.
!
!----------------------------------------------------------------------->

module stfod_diagnostics

    use mpas_derived_types, only : MPAS_pool_type, MPAS_clock_type
    use mpas_kind_types, only : RKIND

    public :: stfod_diagnostics_setup, &
              stfod_diagnostics_update, &
              stfod_diagnostics_compute, &
              stfod_diagnostics_reset, &
              stfod_diagnostics_cleanup

    private

    type (MPAS_pool_type), pointer :: mesh, state, diag, diag_physics
    integer, pointer :: nCells
    real (kind=RKIND), pointer :: dt
    logical :: acc15m ! if rainc15m or rainnc15m are in any output stream

    type (MPAS_clock_type), pointer :: simulationClock

    contains


    !-----------------------------------------------------------------------
    !  routine stfod_diagnostics_setup
    !
    !> \brief   Initialize the stfod_diagnostics module
    !> \author  John Wong
    !> \details
    !>  Initialize the diagnostic module.
    !
    !-----------------------------------------------------------------------
    subroutine stfod_diagnostics_setup(configs, all_pools, simulation_clock)

        use mpas_derived_types, only : MPAS_clock_type, MPAS_STREAM_OUTPUT
        use mpas_pool_routines, only : mpas_pool_get_subpool, &
                                       mpas_pool_get_dimension, &
                                       mpas_pool_get_config, &
                                       mpas_pool_get_array
        use mpas_atm_diagnostics_utils, only : mpas_stream_inclusion_count
        use mpas_timekeeping, only : MPAS_timeInterval_type, MPAS_time_type, &
                                     MPAS_get_clock_time, MPAS_set_timeInterval, &
                                     MPAS_add_clock_alarm, MPAS_NOW

        implicit none

        type (MPAS_pool_type), pointer :: configs
        type (MPAS_pool_type), pointer :: all_pools
        type (MPAS_clock_type), pointer :: simulation_clock

        type (MPAS_timeInterval_type) :: intv15
        type (MPAS_time_type) :: now

        real (kind=RKIND), dimension(:), pointer :: p

        call mpas_pool_get_subpool(all_pools, 'mesh', mesh)
        call mpas_pool_get_subpool(all_pools, 'state', state)
        call mpas_pool_get_subpool(all_pools, 'diag', diag)
        call mpas_pool_get_subpool(all_pools, 'diag_physics', diag_physics)
        call mpas_pool_get_dimension(mesh, 'nCells', nCells)
        call mpas_pool_get_config(configs, 'config_dt', dt)
        ! Setting precip alarms and initializing buckets
        acc15m = (mpas_stream_inclusion_count('rainc15m', direction=MPAS_STREAM_OUTPUT) > 0) .or. &
                 (mpas_stream_inclusion_count('rainnc15m', direction=MPAS_STREAM_OUTPUT) > 0)
        simulationClock => simulation_clock
        if (acc15m) then
            call MPAS_set_timeInterval(intv15, M=15)
            now = MPAS_get_clock_time(simulationClock, MPAS_NOW)
            call MPAS_add_clock_alarm(simulationClock, 'p15mAlarm', now, alarmTimeInterval=intv15)

            call mpas_pool_get_array(diag, 'rainc15m', p)
            p(1:nCells) = 0.0_RKIND
            call mpas_pool_get_array(diag, 'rainnc15m', p)
            p(1:nCells) = 0.0_RKIND
        end if
    end subroutine stfod_diagnostics_setup


    subroutine stfod_diagnostics_update()
        use mpas_pool_routines, only : mpas_pool_get_array
        implicit none
        real (kind=RKIND), dimension(:), pointer :: p_v, p

        ! We are assuming output frequency matching multiples of 15-minutes
        if (acc15m) then
            call mpas_pool_get_array(diag_physics, 'rainncv', p_v) ! time-step value
            call mpas_pool_get_array(diag, 'rainnc15m', p) ! accumulated
            p(1:nCells) = p(1:nCells) + p_v(1:nCells)

            call mpas_pool_get_array(diag_physics, 'raincv', p_v)
            call mpas_pool_get_array(diag, 'rainc15m', p)
            p(1:nCells) = p(1:nCells) + p_v(1:nCells)
        end if
    end subroutine stfod_diagnostics_update

    !-----------------------------------------------------------------------
    !  routine stfod_diagnostics_compute
    !
    !> \brief  Compute short-term FOD diagnostics outputs
    !> \author John Wong
    !> \date   22 Aug 2019
    !
    !-----------------------------------------------------------------------
    subroutine stfod_diagnostics_compute()

        use mpas_pool_routines, only : mpas_pool_get_array
        use mpas_atm_diagnostics_utils, only : MPAS_field_will_be_written

        implicit none

        real (kind=RKIND), dimension(:), pointer :: ncuprec, rainncv

        ! Non-convective grid-scale precipitation rate diagnostics
        if (MPAS_field_will_be_written('ncuprec')) then
            call mpas_pool_get_array(diag, 'ncuprec', ncuprec)
            call mpas_pool_get_array(diag_physics, 'rainncv', rainncv)

            ncuprec(1:nCells) = rainncv(1:nCells) / dt
        end if
    end subroutine stfod_diagnostics_compute


    subroutine stfod_diagnostics_reset()
        use mpas_pool_routines, only : mpas_pool_get_array
        use mpas_timekeeping, only : MPAS_is_alarm_ringing, MPAS_reset_clock_alarm

        implicit none

        real (kind=RKIND), dimension(:), pointer :: p

        if (acc15m .and. MPAS_is_alarm_ringing(simulationClock, 'p15mAlarm')) then
            call MPAS_reset_clock_alarm(simulationClock, 'p15mAlarm')

            call mpas_pool_get_array(diag, 'rainc15m', p)
            p(1:nCells) = 0.0_RKIND
            call mpas_pool_get_array(diag, 'rainnc15m', p)
            p(1:nCells) = 0.0_RKIND
        end if
    end subroutine stfod_diagnostics_reset


    subroutine stfod_diagnostics_cleanup()
        implicit none
        ! Nothing to cleanup
    end subroutine stfod_diagnostics_cleanup


end module stfod_diagnostics

