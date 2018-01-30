! Copyright (c) 2016,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
! John Wong, TWC / IBM, 01/27/2018
! This module provides subroutines for computing diagnostics of accumulated variables
! valid at prescribed intervals.
!
! * 15-minute accumulated total precipitation, snow, and ice every 15 minutes
! * 30-minute accumulated total precipitation, snow, and ice every 30 minutes
! * Houry accumulated total precipitation, snow, and ice at :00 and :30
! * Total ice accumulation since model start time
!

module accumulated_diagnostics

    use mpas_kind_types, only : RKIND
    use mpas_derived_types, only : MPAS_pool_type, MPAS_clock_type

    type (MPAS_pool_type), pointer :: mesh
    type (MPAS_pool_type), pointer :: state
    type (MPAS_pool_type), pointer :: diag
    type (MPAS_pool_type), pointer :: diag_physics

    type (MPAS_clock_type), pointer :: simulationClock

    public :: accumulated_diagnostics_setup, &
              accumulated_diagnostics_update, &
              accumulated_diagnostics_compute, &
              accumulated_diagnostics_reset, &
              accumulated_diagnostics_cleanup, &

    private

    ! Dimensions shall be nCells x 3 time levels for 30min, 01h_00, and 01h_30
    integer, parameter :: nTimes = 3
    real (kind=RKIND), allocatable, dimension(:,:) :: arain, arainc, arainnc, asnow, aice

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

        use mpas_derived_types, only : MPAS_pool_type, MPAS_clock_type
        use mpas_pool_routines, only : mpas_pool_get_subpool, mpas_pool_get_dimension
        use mpas_timekeeping, only : MPAS_timeInterval_type, MPAS_time_type, operator (+), &
                                     MPAS_get_clock_time, MPAS_set_timeInterval, &
                                     MPAS_add_clock_alarm, MPAS_NOW

        implicit none

        type (MPAS_pool_type), pointer :: configs
        type (MPAS_pool_type), pointer :: all_pools
        type (MPAS_clock_type), pointer :: simulation_clock

        type (MPAS_timeInterval_type) :: intv15, intv30, intv1h
        type (MPAS_time_type) :: now

        integer, pointer :: nCells

        simulationClock => simulation_clock
        
        call mpas_pool_get_subpool(all_pools, 'mesh', mesh)
        call mpas_pool_get_subpool(all_pools, 'state', state)
        call mpas_pool_get_subpool(all_pools, 'diag', diag)
        call mpas_pool_get_subpool(all_pools, 'diag_physics', diag_physics)

        ! Allocate internal buckets for hourly accumulation
        call mpas_pool_get_dimension(mesh, 'nCells', nCells)
        allocate(arain(nCells, nTimes))
        allocate(arainc(nCells, nTimes))
        allocate(arainnc(nCells, nTimes))
        allocate(asnow(nCells, nTimes))
        allocate(aice(nCells, nTimes))

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
        use mpas_timekeeping, only : MPAS_is_alarm_ringing

        implicit none
    
        integer :: iCell
        integer, pointer :: nCells

        ! Precipitation diagnostics (in)
        real (kind=RKIND), dimension(:), pointer :: t2m, rainncv, raincv, snowncv, graupelncv, snow_ratio

        ! Precipitation accumulations (inout)
        real (kind=RKIND), dimension(:), pointer :: rain15m, rainc15m, rainnc15m
        real (kind=RKIND), dimension(:), pointer :: snow15m, ice15m, ice_total

        real (kind=RKIND) :: rain, snow, ice

        call mpas_pool_get_dimension(mesh, 'nCells', nCells)
        call mpas_pool_get_array(diag_physics, 't2m', t2m)
        call mpas_pool_get_array(diag_physics, 'rainncv', rainncv)
        call mpas_pool_get_array(diag_physics, 'raincv', raincv)
        call mpas_pool_get_array(diag_physics, 'snowncv', snowncv)
        call mpas_pool_get_array(diag_physics, 'graupelncv', graupelncv)
        call mpas_pool_get_array(diag, 'snow_ratio', snow_ratio)

        ! Precipiation (convective + non-convective, incl frozen)
        call mpas_pool_get_array(diag, 'rain15m', rain15m)
        call mpas_pool_get_array(diag, 'rainc15m', rainc15m)
        call mpas_pool_get_array(diag, 'rainnc15m', rainnc15m)

        ! Snow (+graupel)
        call mpas_pool_get_array(diag, 'snow15m', snow15m)

        ! Ice accretion (freezing rain)
        call mpas_pool_get_array(diag, 'ice15m', ice15m)
        call mpas_pool_get_array(diag, 'ice_total', ice_total)

        do iCell = 1,nCells

            ! Precipitation accumulation
            rain = raincv(iCell) + rainncv(iCell)
            rainc15m(iCell) = rainc15m(iCell) + raincv(iCell)
            rainnc15m(iCell) = rainnc15m(iCell) + rainncv(iCell)

            ! Snow accumulation
            snow = snowncv(iCell) + graupelncv(iCell)
            snow15m(iCell) = snow15m(iCell) + snow * snow_ratio(iCell)

            ! Ice/freezing rain accumulation
            ice = 0._RKIND
            if (t2m(iCell) < 273.16) then
                ice = rain - snow
                ! Rainfall is multipled by factors of 0.8, and 0.66 
                !   in order to reduce the amount of ice accumulation
                !   depending on temperature.  These were best guesses
                !   as to how much ice would aggregate on surfaces 
                !   at given temperatures.  These certainly could
                !   be better tuned.
                if (t2m(iCell) < 270.37) then ! 27 F
                    ice = ice * 0.8
                else
                    ice = ice * 0.66
                end if
                ice15m(iCell) = ice15m(iCell) + ice
                ice_total(iCell) = ice_total(iCell) + ice
            end if

            ! Final non-frozen precipitation
            rain15m(iCell) = rain15m(iCell) + rain - snow - ice
        end do

        ! Every 15 minutes, copy 15-min total accum to 30-min buckets
        if (MPAS_is_alarm_ringing(simulationClock, 'acc15mAlarm')) then
            arain(:,1)   = arain(:,1)   + rain15m(:)
            arainc(:,1)  = arainc(:,1)  + rainc15m(:)
            arainnc(:,1) = arainnc(:,1) + rainnc15m(:)
            asnow(:,1)   = asnow(:,1)   + snow15m(:)
            aice(:,1)    = aice(:,1)    + ice15m(:)
        end if

        ! Every 30 minutes, copy 30-min total accum to hourly buckets
        if (MPAS_is_alarm_ringing(simulationClock, 'acc30mAlarm')) then
            arain(:,2)   = arain(:,2)   + arain(:,1)
            arainc(:,2)  = arainc(:,2)  + arainc(:,1)
            arainnc(:,2) = arainnc(:,2) + arainnc(:,1)
            asnow(:,2)   = asnow(:,2)   + asnow(:,1)
            aice(:,2)    = aice(:,2)    + aice(:,1)

            arain(:,3)   = arain(:,3)   + arain(:,1)
            arainc(:,3)  = arainc(:,3)  + arainc(:,1)
            arainnc(:,3) = arainnc(:,3) + arainnc(:,1)
            asnow(:,3)   = asnow(:,3)   + asnow(:,1)
            aice(:,3)    = aice(:,3)    + aice(:,1)
        end if

    end subroutine accumulated_diagnostics_update

    !-----------------------------------------------------------------------
    !  routine accumulated_diagnostics_compute
    !
    !> \brief  Resolve internal buckets for output
    !> \author John Wong
    !> \date   27 Jan 2017
    !> \details
    !>  Copy internal buckets of different output offset to the output
    !>  variables.
    !
    !-----------------------------------------------------------------------
    subroutine accumulated_diagnostics_compute()
        use mpas_timekeeping, only : MPAS_is_alarm_ringing

        implicit none

        real (kind=RKIND), dimension(:), pointer :: precip, snow, ice
        logical :: alarm30m, alarm00, alarm30

        ! Determine which internal bucket to use for output
        alarm30m = MPAS_is_alarm_ringing(simulationClock, 'acc30mAlarm')
        alarm00  = MPAS_is_alarm_ringing(simulationClock, 'acc01h_00Alarm')
        alarm30  = MPAS_is_alarm_ringing(simulationClock, 'acc01h_30Alarm')

        ! Rain (non-frozen)
        call output_choice('rain30m', alarm30m, arain(:,1))
        call output_choice('rain01h', alarm00, arain(:,2), alarm30, arain(:,3))

        ! Rain (convective)
        call output_choice('rainc30m', alarm30m, arainc(:,1))
        call output_choice('rainc01h', alarm00, arainc(:,2), alarm30, arainc(:,3))

        ! Rain (non-convective)
        call output_choice('rainnc30m', alarm30m, arainnc(:,1))
        call output_choice('rainnc01h', alarm00, arainnc(:,2), alarm30, arainnc(:,3))

        ! Snow
        call output_choice('snow30m', alarm30m, asnow(:,1))
        call output_choice('snow01h', alarm00, asnow(:,2), alarm30, asnow(:,3))

        ! Ice
        call output_choice('ice30m', alarm30m, aice(:,1))
        call output_choice('ice01h', alarm00, aice(:,2), alarm30, aice(:,3))

    end subroutine accumulated_diagnostics_compute


    !-----------------------------------------------------------------------
    !  routine output_choice
    !
    !> \brief Sets the field to input arrays based on the given conditions
    !> \author John Wong
    !> \date   27 Jan 2017
    !> \details
    !>  Sets the array referenced by field to the values in arr1 or arr2
    !>  depending on the corresponding conditions cond1 and cond2. If
    !>  neither condition is true, the field is reset using reset_bucket_arr.
    !
    !-----------------------------------------------------------------------
    subroutine output_choice(field, cond1, arr1, cond2, arr2)
        use mpas_pool_routines, only : mpas_pool_get_array
        use mpas_atm_diagnostics_utils, only : MPAS_field_will_be_written
        implicit none

        character (len=*), intent(in) :: field
        logical, intent(in) :: cond1
        logical, intent(in), optional :: cond2
        real (kind=RKIND), allocatable, dimension(:), intent(in) :: arr1
        real (kind=RKIND), allocatable, dimension(:), intent(in), optional :: arr2
        real (kind=RKIND), dimension(:), pointer :: arr

        if ( MPAS_field_will_be_written(field) ) then
            call mpas_pool_get_array(diag, field, arr)
            if (cond1 .and. allocated(arr1)) then
                arr(:) = arr1(:)
            else if (present(cond2) .and. cond2 .and. allocated(arr2)) then
                arr(:) = arr2(:)
            else
                call reset_bucket_arr(arr)
            end if
        end if
    end subroutine output_choice

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
        use mpas_timekeeping, only : MPAS_is_alarm_ringing, MPAS_reset_clock_alarm

        implicit none

        ! Reset 15-minute buckets
        if (MPAS_is_alarm_ringing(simulationClock, 'acc15mAlarm')) then
            call reset_bucket('rain15m')
            call reset_bucket('rainc15m')
            call reset_bucket('rainnc15m')
            call reset_bucket('snow15m')
            call reset_bucket('ice15m')

            call MPAS_reset_clock_alarm(simulationClock, 'acc15mAlarm')
        end if

        ! Reset 30-minute buckets
        if (MPAS_is_alarm_ringing(simulationClock, 'acc30mAlarm')) then
            call reset_bucket_arr(arain(:,1))
            call reset_bucket_arr(arainc(:,1))
            call reset_bucket_arr(arainnc(:,1))
            call reset_bucket_arr(asnow(:,1))
            call reset_bucket_arr(aice(:,1))

            call MPAS_reset_clock_alarm(simulationClock, 'acc30mAlarm')
        end if

        ! Reset hourly buckets at :00
        if (MPAS_is_alarm_ringing(simulationClock, 'acc01h_00Alarm')) then
            call reset_bucket_arr(arain(:,2))
            call reset_bucket_arr(arainc(:,2))
            call reset_bucket_arr(arainnc(:,2))
            call reset_bucket_arr(asnow(:,2))
            call reset_bucket_arr(aice(:,2))

            call MPAS_reset_clock_alarm(simulationClock, 'acc01h_00Alarm')
        end if

        ! Reset hourly buckets at :30
        if (MPAS_is_alarm_ringing(simulationClock, 'acc01h_30Alarm')) then
            call reset_bucket_arr(arain(:,3))
            call reset_bucket_arr(arainc(:,3))
            call reset_bucket_arr(arainnc(:,3))
            call reset_bucket_arr(asnow(:,3))
            call reset_bucket_arr(aice(:,3))

            call MPAS_reset_clock_alarm(simulationClock, 'acc01h_30Alarm')
        end if
   
    end subroutine accumulated_diagnostics_reset


    !-----------------------------------------------------------------------
    !  routine reset_bucket
    !
    !> \brief Sets the array to value
    !> \author John Wong
    !> \date   29 Jan 2017
    !> \details
    !>  Sets all the values in an array to the given value, or zero if omitted.
    !
    !-----------------------------------------------------------------------
    subroutine reset_bucket(key, value)
        use mpas_pool_routines, only : mpas_pool_get_array

        implicit none
        character (len=*), intent(in) :: key
        real (kind=RKIND), intent(in), optional :: value
        real (kind=RKIND), dimension(:), pointer :: bucket

        call mpas_pool_get_array(diag, key, bucket)
        call reset_bucket_arr(bucket, value)
    end subroutine reset_bucket

    subroutine reset_bucket_arr(bucket, value)
        implicit none
        real (kind=RKIND), dimension(:), intent(inout) :: bucket
        real (kind=RKIND), intent(in), optional :: value

        if ( present(value) ) then
            bucket(:) = value
        else
            bucket(:) = 0._RKIND
        endif
    end subroutine reset_bucket_arr

    !-----------------------------------------------------------------------
    !  routine accumulated_diagnostics_cleanup
    !
    !> \brief Deallocates allocated memory for buckets
    !> \author John Wong
    !> \date   29 Jan 2017
    !> \details
    !>  This routine should be called last among public routines in this module
    !>  to release any allocated memory used in the multi-bucketing of hourly
    !>  precipitation available at shorter intervals
    !
    !-----------------------------------------------------------------------
    subroutine accumulated_diagnostics_cleanup()
        implicit none

        deallocate(arain)
        deallocate(arainc)
        deallocate(arainnc)
        deallocate(asnow)
        deallocate(aice)
    end subroutine accumulated_diagnostics_cleanup

end module accumulated_diagnostics
