! Copyright (c) 2016,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
! John Wong, TWC / IBM, 01/27/2018
! This module provides subroutines for computing diagnostics of accumulated variables
! valid every 15 minutes
!
! * Every 15 minutes, accumulated grid-scale precip, convective precip, rainfall, snow, and ice
! * Total ice accumulation and rainfall
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

    ! 15-minute history in the past hour + current
    integer, parameter :: nHistory = 5
    integer :: iHistory
    ! Total accumulation since model start every 15 minutes, i.e. total = arr(:,t1)-arr(:,t0)
    real (kind=RKIND), allocatable, dimension(:,:) :: rainc, rainnc
    ! Total incidence of overflow buckets every 15 minutes
    integer, allocatable, dimension(:,:) :: i_rainc, i_rainnc
    ! 15-minute calculated accumulations, i.e. total = SUM{arr(:,...)}
    real (kind=RKIND), allocatable, dimension(:,:) :: lh15m, rain15m, snow15m, ice15m, sw15m, dni15m

    ! Model configurations
    integer, pointer :: nCells
    real (kind=RKIND), pointer :: bucket_rainc, bucket_rainnc ! rain buckets
    real (kind=RKIND), pointer :: bucket_radt     ! bucket size for accumulated radiation
    real (kind=RKIND), pointer :: dt              ! model timestep size

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
        use mpas_timekeeping, only : MPAS_timeInterval_type, MPAS_time_type, &
                                     MPAS_get_clock_time, MPAS_set_timeInterval, &
                                     MPAS_add_clock_alarm, MPAS_NOW

        implicit none

        type (MPAS_pool_type), pointer :: configs
        type (MPAS_pool_type), pointer :: all_pools
        type (MPAS_clock_type), pointer :: simulation_clock

        type (MPAS_timeInterval_type) :: intv15
        type (MPAS_time_type) :: now


        simulationClock => simulation_clock
        
        call mpas_pool_get_subpool(all_pools, 'mesh', mesh)
        call mpas_pool_get_subpool(all_pools, 'state', state)
        call mpas_pool_get_subpool(all_pools, 'diag', diag)
        call mpas_pool_get_subpool(all_pools, 'diag_physics', diag_physics)

        ! Allocate internal buckets for hourly accumulation
        call mpas_pool_get_dimension(mesh, 'nCells', nCells)

        ! Allocate historical buffers
        iHistory = 0
        call init_history_real(rainc)
        call init_history_int(i_rainc)
        call init_history_real(rainnc)
        call init_history_int(i_rainnc)
        call init_history_real(rain15m)
        call init_history_real(snow15m)
        call init_history_real(ice15m)
        call init_history_real(lh15m)
        call init_history_real(sw15m)
        call init_history_real(dni15m)

        ! Set precipitation bucket alarm for resets
        call MPAS_set_timeInterval(intv15, M=15)
        now = MPAS_get_clock_time(simulationClock, MPAS_NOW)
        call MPAS_add_clock_alarm(simulationClock, 'acc15mAlarm', now, alarmTimeInterval=intv15)

        ! Config
        call mpas_pool_get_config(configs, 'config_bucket_radt', bucket_radt)
        call mpas_pool_get_config(configs, 'config_bucket_rainc', bucket_rainc)
        call mpas_pool_get_config(configs, 'config_bucket_rainnc', bucket_rainnc)
        call mpas_pool_get_config(configs, 'config_dt', dt)

    end subroutine accumulated_diagnostics_setup

    subroutine init_history_real(arr)
        real (kind=RKIND), allocatable, dimension(:,:), intent(inout) :: arr

        allocate(arr(nCells, 0:nHistory-1))
        arr(:,:) = 0._RKIND
    end subroutine

    subroutine init_history_int(arr)
        integer, allocatable, dimension(:,:), intent(inout) :: arr

        allocate(arr(nCells, 0:nHistory-1))
        arr(:,:) = 0
    end subroutine


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

        real (kind=RKIND), dimension(:), pointer :: u10, v10, lhflx, wspd15m, swdnb, swdnbdn, arr
        real (kind=RKIND) :: wspd

        integer :: iHistp1, iCell

        ! Next step in the history array
        iHistp1 = mod((iHistory + 1),nHistory)

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

        ! Incrementing latent heat for hourly averaging
        call mpas_pool_get_array(diag_physics, 'lh', lhflx) ! in
        lh15m(:,iHistp1) = lh15m(:,iHistp1) + lhflx(:) * dt

        ! Update swdnb and swdnbdn, total and direct normal shortwave
        call mpas_pool_get_array(diag_physics,   'swdnb',   swdnb) ! in
        call mpas_pool_get_array(diag_physics, 'swdnbdn', swdnbdn) ! in
        sw15m(:,iHistp1)  =  sw15m(:,iHistp1) +   swdnb(:) * dt
        dni15m(:,iHistp1) = dni15m(:,iHistp1) + swdnbdn(:) * dt

        ! Step forward in time and copy accumulated values
        if (MPAS_is_alarm_ringing(simulationClock, 'acc15mAlarm')) then
            iHistory = iHistp1
            call copy_diag_real('rainc', rainc)
            call copy_diag_int('i_rainc', i_rainc)
            call copy_diag_real('rainnc', rainnc)
            call copy_diag_int('i_rainnc', i_rainnc)
        end if

    end subroutine accumulated_diagnostics_update

    subroutine copy_diag_real(field, arr_out)
        use mpas_pool_routines, only : mpas_pool_get_array
        implicit none

        character (len=*), intent(in) :: field
        real (kind=RKIND), dimension(nCells,0:nHistory-1), intent(inout) :: arr_out
        real (kind=RKIND), dimension(:), pointer :: arr

        call mpas_pool_get_array(diag_physics, field, arr)
        arr_out(:,iHistory) = arr(:)
    end subroutine

    subroutine copy_diag_int(field, arr_out)
        use mpas_pool_routines, only : mpas_pool_get_array
        implicit none

        character (len=*), intent(in) :: field
        integer, dimension(nCells,0:nHistory-1), intent(inout) :: arr_out
        integer, dimension(:), pointer :: arr

        call mpas_pool_get_array(diag_physics, field, arr)
        arr_out(:,iHistory) = arr(:)
    end subroutine

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
        real (kind=RKIND), dimension(:), pointer :: ice_total

        real (kind=RKIND) :: rain, snow, ice
        integer :: iHistp1

        ! Next step in the history array
        iHistp1 = mod((iHistory + 1), nHistory)

        call mpas_pool_get_dimension(mesh, 'nCells', nCells)
        call mpas_pool_get_array(diag_physics, 't2m', t2m)
        call mpas_pool_get_array(diag_physics, 'rainncv', rainncv)
        call mpas_pool_get_array(diag_physics, 'raincv', raincv)
        call mpas_pool_get_array(diag_physics, 'snowncv', snowncv)
        call mpas_pool_get_array(diag_physics, 'graupelncv', graupelncv)
        call mpas_pool_get_array(diag, 'snow_ratio', snow_ratio)

        ! Ice accretion (freezing rain)
        call mpas_pool_get_array(diag, 'ice_total', ice_total)

        do iCell = 1,nCells

            ! Precipitation accumulation
            rain = raincv(iCell) + rainncv(iCell)

            ! Snow accumulation
            snow = snowncv(iCell) + graupelncv(iCell)
            snow15m(iCell,iHistp1) = snow15m(iCell,iHistp1) + snow * snow_ratio(iCell)

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
                ice15m(iCell,iHistp1) = ice15m(iCell,iHistp1) + ice
                ice_total(iCell) = ice_total(iCell) + ice
            end if

            ! Final non-frozen precipitation
            rain15m(iCell,iHistp1) = rain15m(iCell,iHistp1) + rain - snow - ice
        end do
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
        implicit none

        ! Rain (non-frozen)
        call output_accum('rain15m', 1, rain15m)
        call output_accum('rain30m', 2, rain15m)
        call output_accum('rain01h', 4, rain15m)

        ! Rain (convective)
        call output_diff('rainc15m', 1, rainc, i_rainc, bucket_rainc)
        call output_diff('rainc30m', 2, rainc, i_rainc, bucket_rainc)
        call output_diff('rainc01h', 4, rainc, i_rainc, bucket_rainc)

        ! Rain (non-convective)
        call output_diff('rainnc15m', 1, rainnc, i_rainnc, bucket_rainnc)
        call output_diff('rainnc30m', 2, rainnc, i_rainnc, bucket_rainnc)
        call output_diff('rainnc01h', 4, rainnc, i_rainnc, bucket_rainnc)

        ! Snow
        call output_accum('snow15m', 1, snow15m)
        call output_accum('snow30m', 2, snow15m)
        call output_accum('snow01h', 4, snow15m)

        ! Ice
        call output_accum('ice15m', 1, ice15m)
        call output_accum('ice30m', 2, ice15m)
        call output_accum('ice01h', 4, ice15m)

        ! Latent heat flux, averaging
        call output_accum('lh01h', 4, lh15m, div=3600._RKIND)

        ! Downward shortwave flux, averaging
        call output_accum(  'swdnb01h', 4,  sw15m, div=3600._RKIND)
        call output_accum('swdnbdn01h', 4, dni15m, div=3600._RKIND)

    end subroutine accumulated_diagnostics_compute

    !-----------------------------------------------------------------------
    !  routine output_diff
    !
    !> \brief Sets the field to input arrays by taking a diff back history
    !> \author John Wong
    !> \date   5 Feb 2017
    !> \details
    !>  Sets the array referenced by field to the difference in historical array
    !>  plus an optional integer overflow bucket. An optional divider can be
    !>  provided to divide the final array. Used when the historical array
    !>  is provided as total accum up from start to current time step.
    !
    !-----------------------------------------------------------------------
    subroutine output_diff(field, steps, historical, i_bucket, bucket_size, div)
        use mpas_pool_routines, only : mpas_pool_get_array
        use mpas_atm_diagnostics_utils, only : MPAS_field_will_be_written
        implicit none

        character (len=*), intent(in) :: field
        integer, intent(in) :: steps                              ! number of time steps
        real (kind=RKIND), dimension(nCells,0:nHistory-1), intent(in) :: historical ! historical accum array
        integer, dimension(nCells,0:nHistory-1), intent(in), optional :: i_bucket   ! historical bucket incidence
        real (kind=RKIND), intent(in), optional :: bucket_size    ! bucket size
        real (kind=RKIND), intent(in), optional :: div            ! divider

        real (kind=RKIND), dimension(:), pointer :: arr           ! output array

        integer :: iHist0

        if ( MPAS_field_will_be_written(field) ) then
            call mpas_pool_get_array(diag, field, arr)
            iHist0 = mod((iHistory + nHistory - steps),nHistory)
            arr(:) = historical(:,iHistory) - historical(:,iHist0)
            if (present(i_bucket) .and. present(bucket_size)) then
                arr(:) = arr(:) + (i_bucket(:,iHistory) - i_bucket(:,iHist0))*bucket_size
            end if
            if (present(div)) then
                arr(:) = arr(:)/div
            end if
        end if

    end subroutine output_diff

    !-----------------------------------------------------------------------
    !  routine output_accum
    !
    !> \brief Sets the field to input arrays by accumulating back history
    !> \author John Wong
    !> \date   5 Feb 2017
    !> \details
    !>  Sets the array referenced by field to the accumulation from the
    !>  historical array backwards. Used when the historical array is
    !>  provided as the total accum for the last 15 minutes only.
    !
    !-----------------------------------------------------------------------
    subroutine output_accum(field, steps, historical, div)
        use mpas_pool_routines, only : mpas_pool_get_array
        use mpas_atm_diagnostics_utils, only : MPAS_field_will_be_written
        implicit none

        character (len=*), intent(in) :: field
        integer, intent(in) :: steps                                ! number of time steps
        real (kind=RKIND), dimension(nCells,0:nHistory-1), intent(in) :: historical ! historical accum array
        real (kind=RKIND), intent(in), optional :: div              ! divider

        real (kind=RKIND), dimension(:), pointer :: arr           ! output array

        integer :: iHist0, iStep

        if ( MPAS_field_will_be_written(field) ) then
            call mpas_pool_get_array(diag, field, arr)
            arr(:) = historical(:,iHistory)
            do iStep = 1, steps-1
                iHist0 = mod((iHistory + nHistory - iStep), nHistory)
                arr(:) = arr(:) + historical(:,iHist0)
            end do
            if (present(div)) then
                arr(:) = arr(:)/div
            end if
        end if

    end subroutine output_accum

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
        use mpas_pool_routines, only : mpas_pool_get_array
        implicit none

        real (kind=RKIND), dimension(:), pointer :: bucket

        ! Reset 15-minute buckets
        if (MPAS_is_alarm_ringing(simulationClock, 'acc15mAlarm')) then
            call mpas_pool_get_array(diag, 'wspd10_max15m', bucket)
            bucket(:) = 0._RKIND

            call MPAS_reset_clock_alarm(simulationClock, 'acc15mAlarm')
        end if
    end subroutine accumulated_diagnostics_reset


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

        deallocate(rain15m)
        deallocate(snow15m)
        deallocate(ice15m)
        deallocate(lh15m)
        deallocate(sw15m)
        deallocate(dni15m)
        deallocate(rainc)
        deallocate(i_rainc)
        deallocate(rainnc)
        deallocate(i_rainnc)
    end subroutine accumulated_diagnostics_cleanup

end module accumulated_diagnostics
