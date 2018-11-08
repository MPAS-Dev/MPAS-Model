!  aviation_diagnostics.F
!
!  Created by Cathryn Meyer, December / 2017
!  Copyright (c) 2017 The Weather Company. All rights reserved.
!
!  This module computes high altitude icing based on a method developed
!  by CMAP members with its roots shown in this google presentation:
!  https://docs.google.com/a/weather.com/presentation/d/
!        1_HRI2kUq2SmoBD9EXZdYgLowmafIJmrIzD7OPBPYN90/edit?usp=sharing
!
!----------------------------------------------------------------------->

module aviation_diagnostics

    use mpas_derived_types
    use mpas_kind_types
    use mpas_pool_routines
    use mpas_constants

    type (MPAS_pool_type), pointer :: mesh
    type (MPAS_pool_type), pointer :: state
    type (MPAS_pool_type), pointer :: diag, diag_physics, sfc_input

    public :: aviation_diagnostics_setup, &
              aviation_diagnostics_update, &
              aviation_diagnostics_compute, &
              aviation_diagnostics_reset, &
              aviation_diagnostics_cleanup

    private

    logical :: is_needed_avcnvmsk
    logical :: is_needed_echotop

    ! Smoothing
    integer :: smooth_radius ! Radius for HIWC smoothing, 0 = no smoothing
    integer, dimension(:,:), allocatable :: neighborhood
    integer, dimension(:), allocatable :: nNeighbors

    contains


    !-----------------------------------------------------------------------
    !  routine aviation_diagnostics_setup
    !
    !> \brief   Initialize the aviation_diagnostics module
    !> \author  Meyer
    !> \date    Dec 2017
    !> \details
    !>  Initialize the diagnostic module.
    !
    !-----------------------------------------------------------------------
    subroutine aviation_diagnostics_setup(all_pools)
        use mpas_derived_types, only : MPAS_pool_type, MPAS_clock_type, MPAS_STREAM_OUTPUT
        use mpas_pool_routines, only : mpas_pool_get_subpool, mpas_pool_get_array
        use mpas_atm_diagnostics_utils, only : mpas_stream_inclusion_count

        implicit none

        real (kind=RKIND), dimension(:), pointer :: invAreaCell 

        type (MPAS_pool_type), pointer :: all_pools

        call mpas_pool_get_subpool(all_pools, 'mesh', mesh)
        call mpas_pool_get_subpool(all_pools, 'state', state)
        call mpas_pool_get_subpool(all_pools, 'diag', diag)
        call mpas_pool_get_subpool(all_pools, 'diag_physics', diag_physics)
        call mpas_pool_get_subpool(all_pools, 'sfc_input', sfc_input)

        is_needed_avcnvmsk = (mpas_stream_inclusion_count('avcnvmsk', direction=MPAS_STREAM_OUTPUT) > 0)
        is_needed_echotop = (mpas_stream_inclusion_count('echotop', direction=MPAS_STREAM_OUTPUT) > 0)

        ! Initialize smoother if necessary
        if (mpas_stream_inclusion_count('hiwc', direction=MPAS_STREAM_OUTPUT) > 0) then
            call mpas_pool_get_array(mesh, 'invAreaCell', invAreaCell)
            ! Area with radius = 3x18km in from RPM = 9.161e9
            smooth_radius = NINT(SQRT(invAreaCell(1) * 9.161e9))/2
            if (smooth_radius > 0) then
                call initialize_hiwc_smoother()
            end if
        end if

    end subroutine aviation_diagnostics_setup


    subroutine initialize_hiwc_smoother
        use mpas_pool_routines, only : mpas_pool_get_dimension, mpas_pool_get_array

        implicit none

        integer, allocatable, dimension(:) :: tmpNeighbors

        integer, pointer :: nCells, nCellsSolve, maxEdges
        integer, dimension(:,:), pointer :: cellsOnCell
        integer, dimension(:), pointer :: nEdgesOnCell
        integer :: iRadius, maxNeighborhood, iCell, nn

        call mpas_pool_get_dimension(mesh, 'nCells', nCells)
        call mpas_pool_get_dimension(mesh, 'nCellsSolve', nCellsSolve)
        call mpas_pool_get_dimension(mesh, 'maxEdges', maxEdges)
        call mpas_pool_get_array(mesh, 'cellsOnCell', cellsOnCell)
        call mpas_pool_get_array(mesh, 'nEdgesOnCell', nEdgesOnCell)

        ! Calculate upper bound for neighborhood size
        maxNeighborhood = 1
        do iRadius=1,smooth_radius
            maxNeighborhood = maxNeighborhood + iRadius * maxEdges
        end do

        ! Allocate smoother mapping
        allocate(tmpNeighbors(maxNeighborhood))
        allocate(neighborhood(maxNeighborhood, nCellsSolve))
        allocate(nNeighbors(nCellsSolve))

        ! Define the neighborhood for each cell
        do iCell=1,nCellsSolve
            nn = 1
            call find_neighbors(iCell, cellsOnCell, nCells, nEdgesOnCell, smooth_radius, tmpNeighbors, nn)
            nNeighbors(iCell) = nn
            neighborhood(1:nn,iCell) = tmpNeighbors(1:nn)
        end do

        deallocate(tmpNeighbors)

    end subroutine initialize_hiwc_smoother

    ! Finds all neighbors within radius from iCenter
    subroutine find_neighbors(iCenter, cellsOnCell, nCells, nEdgesOnCell, radius, neighborhood, nNeighbors)

        integer, intent(in) :: iCenter, radius, nCells
        integer, dimension(:,:), intent(in) :: cellsOnCell
        integer, dimension(:), intent(in) :: nEdgesOnCell
        integer, dimension(:), intent(out) :: neighborhood
        integer, intent(out) :: nNeighbors
        integer :: itr_start, itr_end, iRadius, itr_cell, iNeighbor, cell, iCur, nCellsP1
        logical :: found

        nNeighbors = 1
        itr_start = 1
        itr_end = 1
        nCellsP1 = nCells + 1

        neighborhood(1) = iCenter
        do iRadius=1,radius
            do itr_cell=itr_start,itr_end
                ! Check each neighbor in cellsOnCell
                do iNeighbor=1,nEdgesOnCell(neighborhood(itr_cell))
                    cell = cellsOnCell(iNeighbor,neighborhood(itr_cell))
                    if (.not.ANY(neighborhood(1:nNeighbors) == cell) .and. cell.ne.nCellsP1) then
                        nNeighbors = nNeighbors + 1
                        neighborhood(nNeighbors) = cell
                    end if
                end do
            end do
            itr_start = itr_end + 1
            itr_end = nNeighbors
        end do

    end subroutine


    !-----------------------------------------------------------------------
    !  routine aviation_diagnostics_update
    !
    !> \brief  Update aviation diagnostics
    !> \author Cathryn Meyer
    !> \date   Dec 2017
    !> \details
    !>  Update aviation diagnostics at each timestep
    !
    !-----------------------------------------------------------------------
    subroutine aviation_diagnostics_update()
        implicit none

        if (is_needed_avcnvmsk .or. is_needed_echotop) then
            call update_avcnvmsk()
        end if

    end subroutine aviation_diagnostics_update


    !-----------------------------------------------------------------------
    !  routine aviation_diagnostics_compute
    !
    !> \brief  Compute aviation diagnostics for model output
    !> \author Cathryn Meyer
    !> \date   Dec 2017
    !> \details
    !>  Compute HIWC before model output is written.
    !
    !-----------------------------------------------------------------------
    subroutine aviation_diagnostics_compute()

        use mpas_pool_routines, only : mpas_pool_get_dimension, mpas_pool_get_array
        use mpas_atm_diagnostics_utils, only : MPAS_field_will_be_written

        implicit none

        integer, pointer :: nCells, nCellsSolve, nVertLevels
        real (kind=RKIND), dimension(:), allocatable :: qi_200hPa
        real (kind=RKIND), dimension(:), allocatable :: qi_250hPa
        real (kind=RKIND), dimension(:), allocatable :: virtualT_400hPa
        real (kind=RKIND), dimension(:), allocatable :: hiwc_calc

        real(kind=RKIND) :: w200, w250

        ! These are in degrees C
        real (kind=RKIND), parameter :: warm_boundary_temp = -15.
        real (kind=RKIND), parameter :: cold_boundary_temp = -17.

        integer, dimension(:), pointer :: hiwc

        integer :: iCell, k

        if ( MPAS_field_will_be_written('hiwc') ) then
            ! Mesh information
            call mpas_pool_get_dimension(mesh, 'nCells', nCells)
            call mpas_pool_get_dimension(mesh, 'nCellsSolve', nCellsSolve)
            allocate(virtualT_400hPa(nCells))    
            allocate(qi_200hPa(nCells))
            allocate(qi_250hPa(nCells))
            allocate(hiwc_calc(nCells))

            ! Get cloud ice mixing ratio at 200hPa and 250hPa 
            ! and virtual Temperature at 400 hPa
            call interp_diagnostics(qi_200hPa, qi_250hPa, virtualT_400hPa)


            do iCell=1,nCells
                w200 = MIN(1.0, MAX(0.0, &
                           (warm_boundary_temp - virtualT_400hPa(iCell)) / &
                           (warm_boundary_temp - cold_boundary_temp) ) )

                w250 = 1.0 - w200
                hiwc_calc(iCell) = MAX(0.0, MIN(3.0, &
                                    ( (w250 * qi_250hPa(iCell) * 1.0E6 +  &
                                       w200 * qi_200hPa(iCell) * 1.0E6) - 25. ) / 80. ))

            end do

            if(allocated(qi_200hPa)) deallocate(qi_200hPa)
            if(allocated(qi_250hPa)) deallocate(qi_250hPa)
            if(allocated(virtualT_400hPa)) deallocate(virtualT_400hPa)

            if (smooth_radius > 0) then
                call smooth(hiwc_calc, nCellsSolve)
            end if

            ! Round to the nearest integer for output array
            call mpas_pool_get_array(diag, 'hiwc', hiwc)
            hiwc(1:nCellsSolve) = NINT(hiwc_calc(1:nCellsSolve))

            deallocate(hiwc_calc)

        end if

    end subroutine aviation_diagnostics_compute

    ! Simple mean smoothing within smooth_radius
    subroutine smooth(arr, nCellsSolve)
        implicit none

        real (kind=RKIND), dimension(:), intent(inout) :: arr
        integer, intent(in) :: nCellsSolve
        real (kind=RKIND), dimension(nCellsSolve) :: smoothed

        integer :: iCell, iNeighbor

        do iCell=1,nCellsSolve
            smoothed(iCell) = 0.0
            do iNeighbor=1,nNeighbors(iCell)
                smoothed(iCell) = smoothed(iCell) + arr(neighborhood(iNeighbor, iCell))
            end do
        end do
        arr(1:nCellsSolve) = smoothed(1:nCellsSolve)/nNeighbors(1:nCellsSolve)
    end subroutine

    !-----------------------------------------------------------------------
    !  routine update_avcnvmsk
    !
    !> \brief  Update Aviation Convective Mask
    !> \author John Wong
    !> \date   Feb 2017
    !> \details Update convective masks for TWC aviation diagnostics at each
    !>          timestep
    !
    !-----------------------------------------------------------------------
    subroutine update_avcnvmsk()

        use mpas_pool_routines, only : mpas_pool_get_dimension, mpas_pool_get_array

        implicit none

! ---------------
        integer, pointer :: nCells, nVertLevels
        real (kind=RKIND), dimension(:,:), pointer :: refl10cm, z, w   ! in
        real (kind=RKIND), dimension(:), pointer :: z0, rainncv, cutop ! in
        real (kind=RKIND), dimension(:), pointer :: avcnvmsk  ! out
        real (kind=RKIND), dimension(:), pointer :: echotop ! out

        integer :: iCell, k, k_echotop
        real :: dbzmax, wmax, wgt, w_fct, dbz_fct, et_fct, et

        real, parameter :: dbzcrit_min = 18.0
        real, parameter :: dbzcrit_max = 60.0
        real, parameter :: pcpcrit_min = 0.0       ! timestep precip, mm
        real, parameter :: wcrit_min   = 0.30      ! updraft, m/s
        real, parameter :: wcrit_max   = 20.
!       real, parameter :: etcrit_min  = 7620.0    ! echo top, m, = 25000ft
!       real, parameter :: etcrit_max  = 18288.0   ! = 60000ft
        real, parameter :: etcrit_min  = 25.       ! echo top, = 25000ft
        real, parameter :: etcrit_max  = 60.       ! = 60000ft

! --------------

        call mpas_pool_get_dimension(mesh, 'nCells', nCells)
        call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)
        call mpas_pool_get_array(diag_physics, 'refl10cm', refl10cm)
        call mpas_pool_get_array(diag_physics, 'cutop', cutop)
        call mpas_pool_get_array(diag_physics, 'rainncv', rainncv)
        call mpas_pool_get_array(state, 'w', w, 1)
        call mpas_pool_get_array(diag, 'zgrid_unstag', z)
        call mpas_pool_get_array(sfc_input, 'ter', z0)
        call mpas_pool_get_array(diag, 'avcnvmsk', avcnvmsk)
        call mpas_pool_get_array(diag, 'echotop', echotop)

        do iCell=1,nCells
            ! Find the max reflectivity and max updraft
            dbzmax = MAXVAL(refl10cm(:,iCell))
            wmax = MAXVAL(w(:,iCell))

            ! Find the convective top either as echo top or parameterized convective top
            k_echotop = 1
            if (dbzmax > dbzcrit_min) then
                do k=1,nVertLevels-1
                    if (refl10cm(k,iCell) > dbzcrit_min) then
                        k_echotop = k
                    end if
                end do
            end if
!           k_echotop = MAX(k_echotop, MIN(nVertLevels-1, int(cutop(iCell))))

            ! Convert echotop index to height AGL
            if (k_echotop <= 1) then
                et = 0.
            else
                ! Linearly interpolate to find the echotop height between two levels
                wgt = (refl10cm(k_echotop,iCell) - dbzcrit_min) / &
                      (refl10cm(k_echotop,iCell) - refl10cm(k_echotop+1,iCell))
                wgt = MAX(0., MIN(1., wgt))
                et = ((wgt*z(k_echotop+1,iCell)+(1.-wgt)*z(k_echotop,iCell))-z0(iCell))*3.28084*0.001
            end if
            echotop(iCell)=MAX(echotop(iCell),et)

            ! Calculate convective mask
            if( rainncv(iCell).ge.pcpcrit_min .and. wmax.ge.wcrit_min .and. et.ge.etcrit_min ) then
                w_fct   = MAX(0., MIN(1., (wmax-wcrit_min)/(wcrit_max-wcrit_min)))
                dbz_fct = MAX(0., MIN(1., (dbzmax-dbzcrit_min)/(dbzcrit_max-dbzcrit_min)))
                et_fct  = MAX(0., MIN(1., (et-etcrit_min)/(etcrit_max-etcrit_min)))
                avcnvmsk(iCell) = MAX(avcnvmsk(iCell), 1.+w_fct+dbz_fct+et_fct )
            end if
        end do

    end subroutine

   !==================================================================================================
    subroutine interp_diagnostics(qi_200hPa, qi_250hPa, virtualT_400hPa)
   !==================================================================================================
        use mpas_derived_types, only : field2DReal, field3DReal
        use mpas_dmpar, only : mpas_dmpar_exch_halo_field

        ! output arguments:
        real (kind=RKIND), dimension(:), intent(out) :: qi_200hPa
        real (kind=RKIND), dimension(:), intent(out) :: qi_250hPa
        real (kind=RKIND), dimension(:), intent(out) :: virtualT_400hPa

        ! local variables:
        integer :: iCell
        integer, pointer :: nCells, nVertLevels
        integer, pointer :: index_qv, index_qi

        ! prognostics
        real (kind=RKIND), dimension(:,:), pointer :: exner, theta_m, pressure_b, pressure_p
        real (kind=RKIND), dimension(:,:,:), pointer :: scalars

        ! diagnostics
        real (kind=RKIND), dimension(:), allocatable :: t_400hPa, qv_400hPa
        real (kind=RKIND), dimension(:,:), allocatable :: pressure, temperature

        type (field3DReal), pointer :: scalar_f
        type (field2DReal), pointer :: theta_f, exner_f, pb_f, pp_f

        ! Halo needs to be exchanged if smoothing is performed
        if (smooth_radius > 0) then
            call mpas_pool_get_field(state, 'scalars', scalar_f)
            call mpas_pool_get_field(state, 'theta_m', theta_f)
            call mpas_pool_get_field(diag , 'exner',   exner_f)
            call mpas_pool_get_field(diag , 'pressure_base', pb_f)
            call mpas_pool_get_field(diag , 'pressure_p', pp_f)

            call mpas_dmpar_exch_halo_field(scalar_f)
            call mpas_dmpar_exch_halo_field(theta_f)
            call mpas_dmpar_exch_halo_field(exner_f)
            call mpas_dmpar_exch_halo_field(pb_f)
            call mpas_dmpar_exch_halo_field(pp_f)
        end if

        call mpas_pool_get_dimension(mesh, 'nCells', nCells)
        call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)
        call mpas_pool_get_dimension(state, 'index_qv', index_qv)
        call mpas_pool_get_dimension(state, 'index_qi', index_qi)

        call mpas_pool_get_array(state, 'theta_m', theta_m, 1)
        call mpas_pool_get_array(state, 'scalars', scalars, 1)
       
        call mpas_pool_get_array(diag, 'exner', exner)
        call mpas_pool_get_array(diag, 'pressure_base', pressure_b)
        call mpas_pool_get_array(diag, 'pressure_p', pressure_p)

        allocate(pressure(nVertLevels,nCells)      )
        allocate(temperature(nVertLevels,nCells)   )
       
        ! Calculation of total pressure at cell centers (at mass points):
        pressure(:,:) = (pressure_p(:,:) + pressure_b(:,:)) / 100._RKIND
              
        ! Calculation of temperature at cell centers:
        temperature(:,:) = theta_m(:,:)/(1._RKIND+rvord*scalars(index_qv,:,:)) * exner(:,:)

        allocate(qv_400hPa(nCells))
        allocate(t_400hPa(nCells))
       
        ! Interpolation to fixed pressure levels for fields located at cells centers and at mass points:
        do iCell = 1, nCells
            qi_200hPa(iCell) = interp_to_p(nVertLevels, scalars(index_qi,:,iCell), pressure(:,iCell), 200._RKIND)
            qi_250hPa(iCell) = interp_to_p(nVertLevels, scalars(index_qi,:,iCell), pressure(:,iCell), 250._RKIND)
            qv_400hPa(iCell) = interp_to_p(nVertLevels, scalars(index_qv,:,iCell), pressure(:,iCell), 400._RKIND)
            t_400hPa(iCell)  = interp_to_p(nVertLevels, temperature(:,iCell),      pressure(:,iCell), 400._RKIND)
        enddo

        ! Calculate virtual temperature at 400mb     
        virtualT_400hPa(:) = t_400hPa(:) * (1.0 + qv_400hPa(:) * rvord) / (1.0 + qv_400hPa(:))

        deallocate(pressure)
        deallocate(temperature)
        deallocate(qv_400hPa)
        deallocate(t_400hPa)

    end subroutine interp_diagnostics

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

    subroutine aviation_diagnostics_reset()
        use mpas_atm_diagnostics_utils, only : MPAS_field_will_be_written
        use mpas_pool_routines, only : mpas_pool_get_array

        implicit none

        real (kind=RKIND), dimension(:), pointer :: avcnvmsk
        real (kind=RKIND), dimension(:), pointer :: echotop

        if (is_needed_avcnvmsk .and. MPAS_field_will_be_written('avcnvmsk')) then
            call mpas_pool_get_array(diag, 'avcnvmsk', avcnvmsk)
            avcnvmsk(:) = 0._RKIND
        end if
        if (is_needed_echotop .and. MPAS_field_will_be_written('echotop')) then
            call mpas_pool_get_array(diag, 'echotop', echotop)
            echotop(:) = 0._RKIND
        end if

    end subroutine aviation_diagnostics_reset


    subroutine aviation_diagnostics_cleanup()
        implicit none

        if (allocated(neighborhood)) deallocate(neighborhood)
        if (allocated(nNeighbors))   deallocate(nNeighbors)
    end subroutine aviation_diagnostics_cleanup

end module aviation_diagnostics



