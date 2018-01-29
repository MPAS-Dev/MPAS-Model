! Copyright (c) 2016,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
! John Wong, TWC / IBM, 01/27/2018
! This routine computes diagnostics of accumulated variables.

module accumulated_diagnostics

    use mpas_kind_types, only : RKIND, StrKIND
    use mpas_derived_types, only : MPAS_pool_type, MPAS_clock_type

    type (MPAS_pool_type), pointer :: mesh
    type (MPAS_pool_type), pointer :: state
    type (MPAS_pool_type), pointer :: diag
    type (MPAS_pool_type), pointer :: diag_physics

    type (MPAS_clock_type), pointer :: simulationClock

    public :: accumulated_diagnostics_setup, &
              accumulated_diagnostics_update, &
              accumulated_diagnostics_reset

    private :: increment_bucket, reset_bucket

    contains


    !-----------------------------------------------------------------------
    !  routine accumulated_diagnostics_setup
    !
    !> \brief Set-up the accumulated diagnostics module
    !> \author John Wong
    !> \date   27 Jan 2018
    !> \details
    !>  Caching pointers, setting up bucket reset alarms.
    !
    !-----------------------------------------------------------------------
    subroutine accumulated_diagnostics_setup(configs, all_pools, simulation_clock)

        use mpas_derived_types, only : MPAS_pool_type, MPAS_clock_type, MPAS_STREAM_OUTPUT, MPAS_STREAM_INPUT, &
                                       MPAS_STREAM_INPUT_OUTPUT
        use mpas_pool_routines, only : mpas_pool_get_subpool, mpas_pool_get_config      
        use mpas_atm_diagnostics_utils, only : mpas_stream_inclusion_count
        use mpas_timekeeping, only : MPAS_timeInterval_type, MPAS_time_type, MPAS_set_timeInterval, &                                         
                                     MPAS_get_clock_time, MPAS_add_clock_alarm, MPAS_NOW   

        implicit none

        type (MPAS_pool_type), pointer :: configs
        type (MPAS_pool_type), pointer :: all_pools
        type (MPAS_clock_type), pointer :: simulation_clock

        type (MPAS_timeInterval_type) :: intv15, intv30, intv1h
        type (MPAS_time_type) :: now

        simulationClock => simulation_clock
        
        call mpas_pool_get_subpool(all_pools, 'mesh', mesh)
        call mpas_pool_get_subpool(all_pools, 'state', state)
        call mpas_pool_get_subpool(all_pools, 'diag', diag)
        call mpas_pool_get_subpool(all_pools, 'diag_physics', diag_physics)

        if (trim(config_bucket_precip_interval) == 'none') then
            return
        end if

        ! Set precipitation bucket alarm for resets
        call MPAS_set_timeInterval(intv15, M=15)
        now = MPAS_get_clock_time(simulationClock, MPAS_NOW)
        call MPAS_add_clock_alarm(simulationClock, 'acc15mAlarm', now, alarmTimeInterval=intv15)
        call MPAS_set_timeInterval(intv30, M=30)
        call MPAS_add_clock_alarm(simulationClock, 'acc30mAlarm', now, alarmTimeInterval=intv30)
        call MPAS_set_timeInterval(intv1h, H=1)
        call MPAS_add_clock_alarm(simulationClock, 'acc01h_00Alarm', now, alarmTimeInterval=intv1h)
        call MPAS_add_clock_alarm(simulationClock, 'acc01h_30Alarm', now + intv30, alarmTimeInterval=intv1h)

    end subroutine accumulated_diagnostics_setup


    !-----------------------------------------------------------------------
    !  routine accumulated_diagnostics_update
    !
    !> \brief Updates accumulated diagnostics
    !> \author John Wong
    !> \date   27 Jan 2018
    !> \details
    !>  This routine updates the accumulated variables at 15-min intervals
    !
    !-----------------------------------------------------------------------
    subroutine accumulated_diagnostics_update()

        use mpas_pool_routines, only : mpas_pool_get_dimension, mpas_pool_get_array
        use mpas_derived_types, only : mpas_pool_type

        implicit none
    
        integer :: iCell, k
        integer, pointer :: nCells

        ! Precipitation diagnostics (in)
        real (kind=RKIND), dimension(:), pointer :: rainncv, raincv, snowncv, graupelncv, snow_ratio

        ! Precipitation accumulations (inout)
        real (kind=RKIND), dimension(:), pointer :: precip15m, snow15m, ice15m, ice_total

        real (kind=RKIND) :: precip, snow, ice

        call mpas_pool_get_dimension(mesh, 'nCells', nCells)
        call mpas_pool_get_array(diag_physics, 't2m', t2m)
        call mpas_pool_get_array(diag_physics, 'rainncv', rainncv)
        call mpas_pool_get_array(diag_physics, 'raincv', raincv)
        call mpas_pool_get_array(diag_physics, 'snowncv', snowncv)
        call mpas_pool_get_array(diag_physics, 'graupelncv', graupelncv)
        call mpas_pool_get_array(diag, 'snow_ratio', snow_ratio)

        ! Total precipiation (convective + non-convective, incl frozen)
        call mpas_pool_get_array(diag, 'precip15m', precip15m)

        ! Snow (+graupel)
        call mpas_pool_get_array(diag, 'snow15m', snow15m)

        ! Ice accretion (freezing rain)
        call mpas_pool_get_array(diag, 'ice15m', ice15m)
        call mpas_pool_get_array(diag, 'ice_total', ice_total)

        do iCell = 1,nCells

            ! Precipitation accumulation
            precip = max(0., rainncv(iCell) + raincv(iCell))
            precip15m(iCell) = precip15m(iCell) + precip

            ! Snow accumulation
            snow = snowncv(iCell) + graupelncv(iCell)
            snow15m(iCell) = snow15m(iCell) + snow * snow_ratio(iCell)

            ! Ice/freezing rain accumulation
            if (t2m(iCell) < 273.16) then
                ice = precip - snow
                if (t2m(iCell) < 270.37) then ! 27 F
                    ice = ice * 0.8
                else
                    ice = ice * 0.66
                end if
                ice15m(iCell) = ice15m(iCell) + ice
                ice_total(iCell) = ice_total(iCell) + ice
            end if
        end do



        ! Every 15 minutes, copy 15-min total accum to 30-min buckets
        if (MPAS_is_alarm_ringing(simulationClock, 'acc15mAlarm')) then
            call increment_bucket('precip15m', 'precip30m')
            call increment_bucket('snow15m', 'snow30m')
            call increment_bucket('ice15m', 'ice30m')
        end if

        ! Every 30 minutes, copy 30-min total accum to hourly buckets
        if (MPAS_is_alarm_ringing(simulationClock, 'acc30mAlarm')) then
            call increment_bucket('precip30m', 'precip01h_00')
            call increment_bucket('snow30m', 'snow01h_00')
            call increment_bucket('ice30m', 'ice01h_00')

            call increment_bucket('precip30m', 'precip01h_30')
            call increment_bucket('snow30m', 'snow01h_30')
            call increment_bucket('ice30m', 'ice01h_30')
        end if

    end subroutine accumulated_diagnostics_update

    !-----------------------------------------------------------------------
    !  routine increment_bucket
    !
    !> \brief Adds the value from one array to another
    !> \author John Wong
    !> \date   27 Jan 2017
    !> \details
    !>  Adds the vaule from an array referenced by key_in to an array
    !>  referenced by key_out.
    !
    !-----------------------------------------------------------------------
    subroutine increment_bucket(key_in, key_out)
        use mpas_pool_routines, only : mpas_pool_get_array

        implicit none
        character (len=*), intent(in) :: key_in, key_out
        real (kind=RKIND), dimension(:), pointer :: bucket_in, bucket_out

        call mpas_pool_get_array(diag, key_in, bucket_in)
        call mpas_pool_get_array(diag, key_out, bucket_out)
        bucket_out(:) = bucket_out(:) + bucket_in(:)

    end subroutine increment_bucket

    !-----------------------------------------------------------------------
    !  routine accumulated_diagnostics_reset
    !
    !> \brief Reset values for buckets
    !> \author John Wong
    !> \date   27 Jan 2017
    !> \details
    !>  Resets the bucket values for precipitation
    !
    !-----------------------------------------------------------------------
    subroutine accumulated_diagnostics_reset()

        use mpas_pool_routines, only : mpas_pool_get_array
        use mpas_timekeeping, only : MPAS_is_alarm_ringing, MPAS_reset_clock_alarm   

        implicit none

        ! Reset 15-minute buckets
        if (MPAS_is_alarm_ringing(simulationClock, 'acc15mAlarm')) then
            call reset_bucket('precip15m')
            call reset_bucket('snow15m')
            call reset_bucket('ice15m')

            call MPAS_reset_clock_alarm(simulationClock, 'acc15mAlarm')
        end if

        ! Reset 30-minute buckets
        if (MPAS_is_alarm_ringing(simulationClock, 'acc30mAlarm')) then
            call reset_bucket('precip30m')
            call reset_bucket('snow30m')
            call reset_bucket('ice30m')

            call MPAS_reset_clock_alarm(simulationClock, 'acc30mAlarm')
        end if

        ! Reset hourly buckets at :00
        if (MPAS_is_alarm_ringing(simulationClock, 'acc01h_00Alarm')) then
            call reset_bucket('precip01h_00')
            call reset_bucket('snow01h_00')
            call reset_bucket('ice01h_00')

            call MPAS_reset_clock_alarm(simulationClock, 'acc01h_00Alarm')
        end if

        ! Reset hourly buckets at :30
        if (MPAS_is_alarm_ringing(simulationClock, 'acc01h_30Alarm')) then
            call reset_bucket('precip01h_30')
            call reset_bucket('snow01h_30')
            call reset_bucket('ice01h_30')

            call MPAS_reset_clock_alarm(simulationClock, 'acc01h_30Alarm')
        end if
   
    end subroutine accumulated_diagnostics_reset


    subroutine reset_bucket(key)
        use mpas_pool_routines, only : mpas_pool_get_array

        implicit none
        character (len=*), intent(in) :: key
        real (kind=RKIND), dimension(:), pointer :: bucket

        call mpas_pool_get_array(diag, key, bucket)
        bucket(:) = 0._RKIND

    end subroutine reset_bucket

end module accumulated_diagnostics
