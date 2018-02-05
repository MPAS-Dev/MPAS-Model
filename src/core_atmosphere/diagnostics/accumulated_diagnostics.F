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
    real (kind=RKIND), allocatable, dimension(:,:) :: alh

    ! 15-minute history in the past hour
    integer, parameter :: nHistory = 5
    real (kind=RKIND), allocatable, dimension(:,:) :: acswdnb_hist
    integer, allocatable, dimension(:,:) :: i_acswdnb_hist

    real    :: bucket_radt         ! bucket size for accumulated radiation
    integer :: i_lh01h_00, i_lh01h_30   ! Incidence of latent heat accumulation
    integer :: iHistory ! Index to current point in time

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
        use mpas_pool_routines, only : mpas_pool_get_subpool, mpas_pool_get_dimension, mpas_pool_get_config
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
        allocate(alh(nCells, 2:nTimes))

        ! Allocate historical buffers
        allocate(acswdnb_hist(nCells, 0:nHistory-1))
        allocate(i_acswdnb_hist(nCells, 0:nHistory-1))
        acswdnb_hist(:,:) = 0._RKIND
        i_acswdnb_hist(:,:) = 0

        ! Set precipitation bucket alarm for resets
        call MPAS_set_timeInterval(intv15, M=15)
        now = MPAS_get_clock_time(simulationClock, MPAS_NOW)
        call MPAS_add_clock_alarm(simulationClock, 'acc15mAlarm', now, alarmTimeInterval=intv15)
        call MPAS_set_timeInterval(intv30, M=30)
        call MPAS_add_clock_alarm(simulationClock, 'acc30mAlarm', now, alarmTimeInterval=intv30)
        call MPAS_set_timeInterval(intv1h, H=1)
        call MPAS_add_clock_alarm(simulationClock, 'acc01h_00Alarm', now, alarmTimeInterval=intv1h)
        call MPAS_add_clock_alarm(simulationClock, 'acc01h_30Alarm', now + intv30, alarmTimeInterval=intv1h)

        ! Config
        call mpas_pool_get_config(configs, 'config_bucket_radt', bucket_radt)

        ! Initialize counters
        i_lh01h_00 = 0
        i_lh01h_30 = 0
        iHistory = 0

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
        use mpas_pool_routines, only : mpas_pool_get_array
        use mpas_timekeeping, only : MPAS_is_alarm_ringing

        implicit none

        real (kind=RKIND), dimension(:), pointer :: u10, v10, lh, wspd15m
        real (kind=RKIND) :: wspd

        ! Precip buckets
        call accumulate_precips()

        ! Wind speed
        call mpas_pool_get_array(diag_physics, 'u10', u10) ! in
        call mpas_pool_get_array(diag_physics, 'v10', v10) ! in
        call mpas_pool_get_array(diag, 'wspd10_max15m', wspd15m) !inout
        do iCell = 1,nCells
            wspd = sqrt(u10(iCell)*u10(iCell) + v10(iCell)*v10(iCell))
            wspd15m(iCell) = max(wspd15m(iCell), wspd)
        end do

        ! Incrementing latent heat flux for hourly averaging at :00 and :30
        call mpas_pool_get_array(diag_physics, 'lh', lh) ! in
        i_lh01h_00 = i_lh01h_00 + 1
        i_lh01h_30 = i_lh01h_30 + 1
        alh(:,2) = alh(:,2) + lh(:)
        alh(:,3) = alh(:,3) + lh(:)

        ! Update shortwave historical accumulated values
        if (MPAS_is_alarm_ringing(simulationClock, 'acc15mAlarm')) then
            iHistory = (iHistory + 1)%nHistory
            call mpas_pool_get_array(diag_physics, 'acswdnb', acswdnb(:,iHistory))
            call mpas_pool_get_array(diag_physics, 'i_acswdnb', i_acswdnb_hist(:,iHistory))
        end if

    end subroutine accumulated_diagnostics_update

    !-----------------------------------------------------------------------
    !  routine accumulate_precips
    !
    !> \brief  Resolve internal buckets for output
    !> \author John Wong
    !> \date   27 Jan 2017
    !> \details
    !>  Copy internal buckets of different output offset to the output
    !>  variables.
    !
    !-----------------------------------------------------------------------
    subroutine accumulate_precips()
        use mpas_pool_routines, only : mpas_pool_get_dimension, mpas_pool_get_array
        use mpas_timekeeping, only : MPAS_is_alarm_ringing

        implicit none
    
        integer :: iCell
        integer, pointer :: nCells

        ! Precipitation diagnostics (in)
        real (kind=RKIND), dimension(:), pointer :: rainncv, raincv, snowncv, graupelncv, snow_ratio
        real (kind=RKIND), dimension(:), pointer :: t2m

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
    end subroutine accumulate_precips

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

        ! Latent heat flux
        call output_choice('lh01h', alarm00, alh(:,2), alarm30, alh(:,3), &
                            div1=i_lh01h_00, div2=i_lh01h_30)

        ! Downward shortwave flux
        call output_dff('swdnb01h', 4, acswdnb_hist, i_acswdnb_hist, bucket_radt, div=3600)

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
    !>  Optional dividers can be provided for dividing the final array.
    !
    !-----------------------------------------------------------------------
    subroutine output_choice(field, cond1, arr1, cond2, arr2, div1, div2)
        use mpas_pool_routines, only : mpas_pool_get_array
        use mpas_atm_diagnostics_utils, only : MPAS_field_will_be_written
        implicit none

        character (len=*), intent(in) :: field
        logical, intent(in) :: cond1
        logical, intent(in), optional :: cond2
        real (kind=RKIND), dimension(:), intent(in) :: arr1
        real (kind=RKIND), dimension(:), intent(in), optional :: arr2
        real (kind=RKIND), dimension(:), pointer :: arr
        integer, intent(in), optional :: div1, div2

        if ( MPAS_field_will_be_written(field) ) then
            call mpas_pool_get_array(diag, field, arr)
            if (cond1) then
                ! Condition 1 satisfied, settting arr to arr1 with optional divider div1
                if (present(div1)) then
                    arr(:) = arr1(:)/div1
                else
                    arr(:) = arr1(:)
                end if
            else if (present(cond2) .and. cond2) then
                ! Condition 2 satisfied, settting arr to arr2 with optional divider div2
                if (present(div2)) then
                    arr(:) = arr2(:)/div2
                else
                    arr(:) = arr2(:)
                end if
            else
                ! Neither condition is satistified, setting to empty
                call reset_bucket_arr(arr)
            end if
        end if
    end subroutine output_choice

    !-----------------------------------------------------------------------
    !  routine output_dff
    !
    !> \brief Sets the field to input arrays by taking a diff back history
    !> \author John Wong
    !> \date   27 Jan 2017
    !> \details
    !>  Sets the array referenced by field to the difference in historical array
    !>  plus an optional integer overflow bucket. An optional divider can be
    !>  provided to divide the final array.
    !
    !-----------------------------------------------------------------------
    subroutine output_dff(field, steps, historical, i_bucket, bucket_size, div)
        use mpas_pool_routines, only : mpas_pool_get_array
        use mpas_atm_diagnostics_utils, only : MPAS_field_will_be_written
        implicit none

        character (len=*), intent(in) :: field
        integer, intent(in) :: steps                              ! number of time steps
        real (kind=RKIND), dimension(:), intent(in) :: historical ! historical real array
        integer, dimension(:), intent(in), optional :: i_bucket   ! historical integer buckets
        real (kind=RKIND), intent(in), optional :: bucket_size    ! bucket sizes
        real (kind=RKIND), intent(in), optional :: div            ! divider

        real (kind=RKIND), dimension(:), pointer :: arr           ! output array

        integer :: iHist0

        if ( MPAS_field_will_be_written(field) ) then
            call mpas_pool_get_array(diag, field, arr)
            iHist0 = (iHistory + nHistory - steps)%nHistory
            arr(:) = historical(:,iHistory) - historical(:,iHist0)
            arr(:) = arr(:) + (ibucket(:,iHistory) - ibucket(:,iHist0))*bucket_size
            if (present(div)) then
                arr(:) = arr(:)/div
            end if
        end if

    end subroutine output_dff

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
            call reset_bucket('wspd10_max15m')

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
            call reset_bucket_arr(alh(:,2))

            i_lh01h_00 = 0

            call MPAS_reset_clock_alarm(simulationClock, 'acc01h_00Alarm')
        end if

        ! Reset hourly buckets at :30
        if (MPAS_is_alarm_ringing(simulationClock, 'acc01h_30Alarm')) then
            call reset_bucket_arr(arain(:,3))
            call reset_bucket_arr(arainc(:,3))
            call reset_bucket_arr(arainnc(:,3))
            call reset_bucket_arr(asnow(:,3))
            call reset_bucket_arr(aice(:,3))
            call reset_bucket_arr(alh(:,3))

            i_lh01h_30 = 0

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
        deallocate(alh)
        deallocate(acswdnb_hist)
        deallocate(i_acswdnb_hist)
    end subroutine accumulated_diagnostics_cleanup

end module accumulated_diagnostics
