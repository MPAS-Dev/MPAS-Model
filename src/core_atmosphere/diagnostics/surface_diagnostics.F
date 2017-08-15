! Copyright (c) 2016,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
! James Cipriani, Brett Wilt, TWC / IBM, 03/24/2017
! This routine computes 2-m dew point right now ... 
! For instructions on how to add a diagnostic to MPAS-Atmosphere, see:
! src/core_atmosphere/diagnostics/README

module surface_diagnostics

    use mpas_kind_types, only : RKIND, StrKIND
    use mpas_derived_types, only : MPAS_pool_type, MPAS_clock_type

    type (MPAS_pool_type), pointer :: mesh
    type (MPAS_pool_type), pointer :: state
    type (MPAS_pool_type), pointer :: diag
    type (MPAS_pool_type), pointer :: diag_physics

    type (MPAS_clock_type), pointer :: simulationClock

    public :: surface_diagnostics_setup, &
              surface_diagnostics_compute, &
              surface_diagnostics_reset

    private

    contains


    !-----------------------------------------------------------------------
    !  routine surface_diagnostics_setup
    !
    !> \brief Set-up the surface diagnostics module
    !> \author James Cipriani / Brett Wilt
    !> \date   24 March 2017
    !> \details
    !>  To avoid later work in dereferencing pointers to various pools,
    !>  this routine saves pool pointers for use by
    !>  the surface_diagnostics_compute routine.
    !
    !-----------------------------------------------------------------------
    subroutine surface_diagnostics_setup(configs, all_pools, simulation_clock)

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
        character(len=StrKIND), pointer :: config_bucket_precip_interval

        type (MPAS_timeInterval_type) :: intv
        type (MPAS_time_type) :: now

        simulationClock => simulation_clock

        call mpas_pool_get_subpool(all_pools, 'mesh', mesh)
        call mpas_pool_get_subpool(all_pools, 'state', state)
        call mpas_pool_get_subpool(all_pools, 'diag', diag)
        call mpas_pool_get_subpool(all_pools, 'diag_physics', diag_physics)
        call mpas_pool_get_config(configs, 'config_bucket_precip_interval', config_bucket_precip_interval)

        if (trim(config_bucket_precip_interval) == 'none') then
            return
        end if

        ! Set precipitation bucket alarm
        call MPAS_set_timeInterval(intv, timeString=trim(config_bucket_precip_interval))                                                                    
        now = MPAS_get_clock_time(simulationClock, MPAS_NOW)
        call MPAS_add_clock_alarm(simulationClock, 'precipAlarm', now, alarmTimeInterval=intv)

    end subroutine surface_diagnostics_setup



    !-----------------------------------------------------------------------
    !  routine surface_diagnostics_compute
    !
    !> \brief Computes surface diagnostic - 2m dewpoint for now ...
    !> \author James Cipriani / Brett Wilt
    !> \date   24 March 2017
    !> \details
    !>  This routine computes surface diagnostics ... 2 m dew point for now ...
    !>    dewpoint_2m
    !
    !-----------------------------------------------------------------------
    subroutine surface_diagnostics_compute()

        use mpas_constants, only : rvord
        use mpas_pool_routines, only : mpas_pool_get_dimension, mpas_pool_get_array

        implicit none

        integer :: iCell, k
        integer, pointer :: nCells

        ! Precipitation diagnostics (in)
        real (kind=RKIND), dimension(:), pointer :: rainncv
        real (kind=RKIND), dimension(:), pointer :: raincv
        real (kind=RKIND), dimension(:), pointer :: snowncv
        real (kind=RKIND), dimension(:), pointer :: graupelncv
        real (kind=RKIND), dimension(:), pointer :: snow_ratio

        ! Precipitation diagnostics (inout)
        real (kind=RKIND), dimension(:), pointer :: rain_bucket
        real (kind=RKIND), dimension(:), pointer :: conv_bucket
        real (kind=RKIND), dimension(:), pointer :: snow_bucket
        real (kind=RKIND), dimension(:), pointer :: zrain_bucket
        real (kind=RKIND), dimension(:), pointer :: snow_total

        ! Surface diagnostics (in)
        real (kind=RKIND), dimension(:), pointer :: psfc
        real (kind=RKIND), dimension(:), pointer :: q2
   
        ! Surface diagnostics (inout)
        real (kind=RKIND), dimension(:), pointer :: dewpoint_2m

        ! Surface diagnostics (local)
        real (kind=RKIND) :: evp

        ! Diagnostics (shared in)
        real (kind=RKIND), dimension(:), pointer :: t2m

        call mpas_pool_get_dimension(mesh, 'nCells', nCells)
        call mpas_pool_get_array(diag_physics, 't2m', t2m)
        call mpas_pool_get_array(diag_physics, 'rainncv', rainncv)
        call mpas_pool_get_array(diag_physics, 'raincv', raincv)
        call mpas_pool_get_array(diag_physics, 'snowncv', snowncv)
        call mpas_pool_get_array(diag_physics, 'graupelncv', graupelncv)
        call mpas_pool_get_array(diag, 'rain_bucket', rain_bucket)
        call mpas_pool_get_array(diag, 'conv_bucket', conv_bucket)
        call mpas_pool_get_array(diag, 'snow_bucket', snow_bucket)
        call mpas_pool_get_array(diag, 'zrain_bucket', zrain_bucket)
        call mpas_pool_get_array(diag, 'snow_total', snow_total)
        call mpas_pool_get_array(diag, 'snow_ratio', snow_ratio)
        call mpas_pool_get_array(diag, 'surface_pressure', psfc)
        call mpas_pool_get_array(diag_physics, 'q2', q2)
        call mpas_pool_get_array(diag, 'dewpoint_2m', dewpoint_2m)

        do iCell = 1,nCells

            ! Precipitation diagnostics
            rain_bucket(iCell) = rain_bucket(iCell) + max(0., rainncv(iCell) - snowncv(iCell) - graupelncv(iCell))
            conv_bucket(iCell) = conv_bucket(iCell) + raincv(iCell)
            snow_bucket(iCell) = snow_bucket(iCell) + snowncv(iCell) + graupelncv(iCell)
            if (t2m(iCell) < 273.16) then
                zrain_bucket(iCell) = zrain_bucket(iCell) + max(0., rainncv(iCell) + raincv(iCell) - snowncv(iCell) - graupelncv(iCell))
            end if
            snow_total(iCell) = snow_total(iCell) + (snowncv(iCell) + graupelncv(iCell)) * snow_ratio(iCell) * 0.001

            ! Surface diagnostics
            evp = 0.01_RKIND * psfc(iCell) * q2(iCell) / (q2(iCell) + 0.622_RKIND)              
            evp = max(evp, 1.0e-8_RKIND)

            ! Dewpoint temperature following Bolton (1980)
            dewpoint_2m(iCell) = (243.5_RKIND * log(evp/6.112_RKIND)) / (17.67_RKIND - log(evp/6.112_RKIND)) 
            dewpoint_2m(iCell) = dewpoint_2m(iCell) + 273.15
            dewpoint_2m(iCell) = min(dewpoint_2m(iCell), t2m(iCell))

        end do

    end subroutine surface_diagnostics_compute


    !-----------------------------------------------------------------------
    !  routine surface_diagnostics_reset
    !
    !> \brief Reset values for surface diagnostics
    !> \author Brett Wilt
    !> \date   4 April 2017
    !> \details
    !>  Resets the bucket values for precipitation
    !
    !-----------------------------------------------------------------------
    subroutine surface_diagnostics_reset()

        use mpas_pool_routines, only : mpas_pool_get_array
        use mpas_timekeeping, only : MPAS_is_alarm_ringing, MPAS_reset_clock_alarm   

        implicit none

        real (kind=RKIND), dimension(:), pointer :: rain_bucket
        real (kind=RKIND), dimension(:), pointer :: conv_bucket
        real (kind=RKIND), dimension(:), pointer :: snow_bucket
        real (kind=RKIND), dimension(:), pointer :: zrain_bucket

        if (MPAS_is_alarm_ringing(simulationClock, 'precipAlarm')) then
            call mpas_pool_get_array(diag, 'rain_bucket', rain_bucket) 
            call mpas_pool_get_array(diag, 'conv_bucket', conv_bucket)
            call mpas_pool_get_array(diag, 'snow_bucket', snow_bucket)
            call mpas_pool_get_array(diag, 'zrain_bucket', zrain_bucket)                               
            rain_bucket(:) = 0.
            conv_bucket(:) = 0.
            snow_bucket(:) = 0.
            zrain_bucket(:) = 0.
        end if

        call MPAS_reset_clock_alarm(simulationClock, 'precipAlarm')
   
    end subroutine surface_diagnostics_reset

end module surface_diagnostics
