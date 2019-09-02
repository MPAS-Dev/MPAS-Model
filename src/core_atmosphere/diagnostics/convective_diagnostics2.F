! Copyright (c) 2016,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
module convective_diagnostics2

    use mpas_derived_types, only : MPAS_pool_type, MPAS_clock_type, MPAS_LOG_ERR, MPAS_LOG_CRIT
    use mpas_kind_types, only : RKIND
    use mpas_log, only : mpas_log_write
    use convective_diagnostics, only : getcape

    type (MPAS_pool_type), pointer :: mesh
    type (MPAS_pool_type), pointer :: state
    type (MPAS_pool_type), pointer :: diag
    type (MPAS_pool_type), pointer :: diag_physics

    type (MPAS_clock_type), pointer :: clock

    public :: convective_diagnostics2_setup, &
              convective_diagnostics2_compute

    contains


    !-----------------------------------------------------------------------
    !  routine convective_diagnostics2_setup
    !
    !> \brief Set-up the convective diagnostics module
    !> \author Michael Duda
    !> \date   14 October 2016
    !> \details
    !>  To avoid later work in dereferencing pointers to various pools,
    !>  this routine saves pool pointers for use by
    !>  the convective_diagnostics2_compute routine.
    !
    !-----------------------------------------------------------------------
    subroutine convective_diagnostics2_setup(all_pools, simulation_clock)

        use mpas_derived_types, only : MPAS_pool_type, MPAS_clock_type, MPAS_STREAM_OUTPUT, MPAS_STREAM_INPUT, &
                                       MPAS_STREAM_INPUT_OUTPUT
        use mpas_pool_routines, only : mpas_pool_get_subpool
        use mpas_atm_diagnostics_utils, only : mpas_stream_inclusion_count

        implicit none

        type (MPAS_pool_type), pointer :: all_pools
        type (MPAS_clock_type), pointer :: simulation_clock

        call mpas_pool_get_subpool(all_pools, 'mesh', mesh)
        call mpas_pool_get_subpool(all_pools, 'state', state)
        call mpas_pool_get_subpool(all_pools, 'diag', diag)
        call mpas_pool_get_subpool(all_pools, 'diag_physics', diag_physics)

        clock => simulation_clock
   
    end subroutine convective_diagnostics2_setup


    !-----------------------------------------------------------------------
    !  routine convective_diagnostics2_compute
    !
    !> \brief Computes convective diagnostic
    !> \author Brett Wilt
    !> \date   20 August 2019
    !> \details
    !>  The following fields are computed by this routine:
    !>    tpi  (Thunderstorm Potential Index)
    !
    !-----------------------------------------------------------------------
    subroutine convective_diagnostics2_compute()

        use mpas_atm_diagnostics_utils, only : MPAS_field_will_be_written
        use mpas_constants, only : rvord
        use mpas_pool_routines, only : mpas_pool_get_dimension, mpas_pool_get_array

        implicit none

        integer :: iCell, k
        integer, pointer :: nCells, nVertLevels

        real (kind=RKIND), dimension(:), pointer :: tpi

        real (kind=RKIND), dimension(:),     pointer :: precipw
        real (kind=RKIND), dimension(:,:),   pointer :: uzonal
        real (kind=RKIND), dimension(:,:),   pointer :: umeridional
        real (kind=RKIND), dimension(:,:),   pointer :: exner
        real (kind=RKIND), dimension(:,:),   pointer :: theta_m
        real (kind=RKIND), dimension(:,:),   pointer :: pressure_p
        real (kind=RKIND), dimension(:,:),   pointer :: pressure_base
        real (kind=RKIND), dimension(:,:,:), pointer :: scalars
        real (kind=RKIND), dimension(:),     pointer :: latCell
        integer, pointer :: index_qv

        real (kind=RKIND) :: cape, cin, li, tp
        real (kind=RKIND) :: ki, tti, sweat, t850, t700, t500, td850, td700
        real (kind=RKIND) :: u850, v850, u500, v500, wspd850, wspd500, wdir850, wdir500, wdir
        real (kind=RKIND), dimension(:), allocatable :: temperature, dewpoint, pressure
        real (kind=RKIND) :: evp, pot_penalty, tpirange, tpiproportion, potrange, tpiamount
        real (kind=RKIND), parameter :: mu = 300., li_offset = 2.5, li_denom = 0.5

        logical :: need_tpi

        need_tpi = MPAS_field_will_be_written('tpi')

        if (need_tpi) then

            call mpas_pool_get_array(diag, 'tpi', tpi)

            call mpas_pool_get_dimension(mesh, 'nCells', nCells)
            call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)
            call mpas_pool_get_array(mesh,'latCell',latCell)

            call mpas_pool_get_array(diag, 'uReconstructMeridional', umeridional)
            call mpas_pool_get_array(diag, 'uReconstructZonal', uzonal)
            call mpas_pool_get_array(state, 'theta_m', theta_m, 1)
            call mpas_pool_get_array(state, 'scalars', scalars, 1)
            call mpas_pool_get_array(diag, 'exner', exner)
            call mpas_pool_get_array(diag, 'pressure_base', pressure_base)
            call mpas_pool_get_array(diag, 'pressure_p', pressure_p)
            call mpas_pool_get_array(diag_physics,'precipw', precipw)

            call mpas_pool_get_dimension(state, 'index_qv', index_qv)

            allocate(temperature(nVertLevels))
            allocate(dewpoint(nVertLevels))
            allocate(pressure(nVertLevels))

            do iCell = 1,nCells

                do k = 1,nVertLevels
                    pressure(k) = (pressure_p(k,iCell) + pressure_base(k,iCell)) / 100.0_RKIND
                    temperature(k) = (theta_m(k,iCell)/(1._RKIND+rvord*scalars(index_qv,k,iCell)))*exner(k,iCell)-273.15
                    evp = 0.01_RKIND * (pressure_base(k,iCell) + pressure_p(k,iCell)) &
                                     * scalars(index_qv,k,iCell) / (scalars(index_qv,k,iCell) + 0.622_RKIND)
                    evp = max(evp, 1.0e-8_RKIND)
                    dewpoint(k) = (243.5_RKIND * log(evp/6.112_RKIND)) / (17.67_RKIND - log(evp/6.112_RKIND))
                enddo

                ! best lifted index
                call getcape( nVertLevels, pressure, temperature, dewpoint, cape, cin, li )

                ! tpi for elevations below 850mb
                if ( pressure(1) .gt. 850. ) then

                  t850 = interp_to_p(nVertLevels, temperature, pressure, 850._RKIND)
                  t700 = interp_to_p(nVertLevels, temperature, pressure, 700._RKIND)
                  t500 = interp_to_p(nVertLevels, temperature, pressure, 500._RKIND)
                  td850 = interp_to_p(nVertLevels, dewpoint, pressure, 850._RKIND)
                  td700 = interp_to_p(nVertLevels, dewpoint, pressure, 700._RKIND)
                  u850 = interp_to_p(nVertLevels, uzonal(:,iCell), pressure, 850._RKIND)
                  v850 = interp_to_p(nVertLevels, umeridional(:,iCell), pressure, 850._RKIND)
                  u500 = interp_to_p(nVertLevels, uzonal(:,iCell), pressure, 500._RKIND)
                  v500 = interp_to_p(nVertLevels, umeridional(:,iCell), pressure, 500._RKIND)

                  ! k index
                  ki = (t850 - t500) + td850 - (t700 - td700)

                  ! total totals index
                  tti = (t850 - t500) + (td850 - t500)

                  ! sweat index
                  wspd850 = (u850*u850 + v850*v850)**0.5 * 1.94384
                  wspd500 = (u500*u500 + v500*v500)**0.5 * 1.94384

                  ! northern hemisphere
                  if( latCell(iCell) .ge. 0 ) then
                    wdir850 = 270.-atan2(v850,u850) * 57.29578
                    wdir500 = 270.-atan2(v500,u500) * 57.29578

                    if( wdir850 .ge. 130 .and. wdir850 .le. 250 .and. wdir500 .ge. 210 .and. wdir500 .le. 310 .and. wdir500-wdir850 .gt. 0 .and. wspd850 .ge. 15 .and. wspd500 .ge. 15) then
                       wdir = sin((wdir500-wdir850)/57.29578) + 0.2
                    else
                       wdir = 0.
                    end if 

                  ! southern hemisphere
                  else
                    wdir850 = 270.-atan2(-v850,u850) * 57.29578
                    wdir500 = 270.-atan2(-v500,u500) * 57.29578

                    if( (wdir850 .le. 50 .or. wdir850 .ge. 290) .and. wdir500 .le. 330 .and. wdir500 .ge. 230 .and. wdir500-wdir850 .lt. 0 .and. wspd850 .ge. 15 .and. wspd500 .ge. 15) then
                       wdir = sin((wdir500-wdir850)/57.29578) + 0.2
                    else
                       wdir = 0.
                    end if

                  end if

                  sweat = 20.*max(0.,tti-49.) + 12.0*max(0.,td850) + 2.0*wspd850 + wspd500 + 125.0*wdir

                  tpi(iCell) = ( 0.1795 + 0.0073*ki - 0.0149*li + 0.0008*sweat )

                ! tpi for elevations above 850mb
                else

                  tpi(iCell) = ( 0.2101 + 0.7611*precipw(iCell)*0.0394 - 0.054*li )

                end if

                ! tpi scaling
                ! pot_penalty = 1. - (mu / (mu + exp((li + li_offset)/li_denom)))
                ! tpi(iCell) = tpi(iCell) - pot_penalty
                tpi(iCell) = max(0.,min(1.,tpi(iCell)))*100.
                if( tpi(iCell) .lt. 47 ) then
                  tpirange = 47.
                  tpiproportion = tpi(iCell) / tpirange
                  potrange = 30.
                  tpi(iCell) = tpiproportion * potrange
                else
                  tpirange = 53.
                  tpiamount = tpi(iCell) - 47.
                  tpiproportion = tpiamount / tpirange
                  tpi(iCell) = 30. + tpiproportion * 70.
                end if

            end do

            deallocate(pressure)
            deallocate(temperature)
            deallocate(dewpoint)

        end if

    end subroutine convective_diagnostics2_compute


    pure function interp_to_p(nlev, field_in, p_in, p_out)
        ! input arguments
        integer, intent(in) :: nlev
        real(kind=RKIND), intent(in), dimension(nlev) :: field_in, p_in
        real(kind=RKIND), intent(in) :: p_out

        ! output arguments
        real(kind=RKIND) :: interp_to_p

        ! Local
        integer :: z
        real(kind=RKIND) :: dpu, dpl

        if (p_out .gt. p_in(1)) then
            ! Below surface level
            interp_to_p = field_in(1)
        else if (p_out .lt. p_in(nlev)) then
            ! Above highest model level
            interp_to_p = field_in(nlev)
        else
            do k = 1,nlev-1
                if ( p_out .gt. p_in(k+1) ) then
                    dpu = p_out - p_in(k)
                    dpl = p_in(k+1) - p_out
                    interp_to_p = (field_in(k) * dpl + field_in(k+1) * dpu)/(dpl + dpu)
                    return
                end if
            end do

            ! Should not reach here
        end if

    end function interp_to_p


end module convective_diagnostics2
