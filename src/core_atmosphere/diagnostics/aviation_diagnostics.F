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

        ! Initialize smoother if necessary
        if (mpas_stream_inclusion_count('hiwc', direction=MPAS_STREAM_OUTPUT) > 0) then
            call mpas_pool_get_array(mesh, 'invAreaCell', invAreaCell)
            smooth_radius = NINT(SQRT(invAreaCell(1) * 9.161e9)) ! area with radius of 3x18km
            if (smooth_radius > 0) then
                call initialize_hiwc_smoother()
            end if
        end if

    end subroutine aviation_diagnostics_setup


    subroutine initialize_hiwc_smoother
        use mpas_pool_routines, only : mpas_pool_get_dimension, mpas_pool_get_array

        implicit none

        integer, allocatable, dimension(:) :: tmpNeighbors

        integer, pointer :: nCells, maxEdges
        integer, dimension(:,:), pointer :: cellsOnCell
        integer, dimension(:), pointer :: nEdgesOnCell
        integer :: iRadius, maxNeighborhood, iCell, nn

        call mpas_pool_get_dimension(mesh, 'nCells', nCells)
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
        allocate(neighborhood(maxNeighborhood, nCells))
        allocate(nNeighbors(nCells))

        ! Define the neighborhood for each cell
        do iCell=1,nCells
            nn = 1
            call find_neighbors(iCell, cellsOnCell, nEdgesOnCell, smooth_radius, tmpNeighbors, nn)
            nNeighbors(iCell) = nn
            neighborhood(1:nn,iCell) = tmpNeighbors(1:nn)
        end do

        deallocate(tmpNeighbors)

    end subroutine initialize_hiwc_smoother

    ! Finds all neighbors within radius from iCenter
    subroutine find_neighbors(iCenter, cellsOnCell, nEdgesOnCell, radius, neighborhood, nNeighbors)

        integer, intent(in) :: iCenter, radius
        integer, dimension(:,:), intent(in) :: cellsOnCell
        integer, dimension(:), intent(in) :: nEdgesOnCell
        integer, dimension(:), intent(out) :: neighborhood
        integer, intent(out) :: nNeighbors
        integer :: itr_start, itr_end, iRadius, itr_cell, iNeighbor, cell, iCur
        logical :: found

        nNeighbors = 1
        itr_start = 1
        itr_end = 1

        neighborhood(1) = iCenter
        do iRadius=1,radius
            do itr_cell=itr_start,itr_end
                ! Check each neighbor in cellsOnCell
                do iNeighbor=1,nEdgesOnCell(neighborhood(itr_cell))
                    cell = cellsOnCell(iNeighbor,neighborhood(itr_cell))
                    if (.not.ANY(neighborhood(1:nNeighbors) == cell)) then
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

        if (is_needed_avcnvmsk) then
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

        integer, pointer :: nCells, nVertLevels
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
            allocate(virtualT_400hPa(nCells))    
            allocate(qi_200hPa(nCells))
            allocate(qi_250hPa(nCells))
            allocate(hiwc_calc(nCells))

            ! Get cloud ice mixing ratio at 200hPa and 250hPa 
            ! and virtual Temperature at 400 hPa
            call interp_diagnostics(mesh, state, 1, diag, qi_200hPa, qi_250hPa, virtualT_400hPa)


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
                call smooth(hiwc_calc, nCells)
            end if

            ! Round to the nearest integer for output array
            call mpas_pool_get_array(diag, 'hiwc', hiwc)
            hiwc(:) = NINT(hiwc_calc(:))

            deallocate(hiwc_calc)

        end if

    end subroutine aviation_diagnostics_compute

    ! Simple mean smoothing within smooth_radius
    subroutine smooth(arr, nCells)
        implicit none

        real (kind=RKIND), dimension(:), intent(inout) :: arr
        integer, intent(in) :: nCells
        real (kind=RKIND), allocatable, dimension(:) :: smoothed

        integer :: iCell, iNeighbor

        allocate(smoothed(nCells))
        do iCell=1,nCells
            smoothed(iCell) = 0.0
            do iNeighbor=1,nNeighbors(iCell)
                smoothed(iCell) = smoothed(iCell) + arr(neighborhood(iNeighbor, iCell))
            end do
            arr(iCell) = smoothed(iCell)/nNeighbors(iCell)
        end do
        deallocate(smoothed)
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
        real (kind=RKIND), dimension(:,:), pointer :: refl10cm, z, w ! in
        real (kind=RKIND), dimension(:), pointer :: z0, rainncv, cutop ! in
        real (kind=RKIND), dimension(:), pointer :: avcnvmsk ! out

        integer :: iCell, k, k_echotop
        real :: dbzmax, wmax, echotop, wgt, w_fct, dbz_fct, et_fct

        real, parameter :: dbzcrit_min = 18.0
        real, parameter :: dbzcrit_max = 60.0
        real, parameter :: pcpcrit_min = 0.0       ! timestep precip, mm
        real, parameter :: wcrit_min   = 0.30      ! updraft, m/s
        real, parameter :: wcrit_max   = 20.
        real, parameter :: etcrit_min  = 7620.0    ! echo top, m, = 25000ft
        real, parameter :: etcrit_max  = 18288.0   ! = 60000ft

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
            k_echotop = MAX(k_echotop, MIN(nVertLevels-1, int(cutop(iCell))))

            ! Convert echotop index to height AGL
            if (k_echotop <= 1) then
                echotop = 0.
            else
                ! Linearly interpolate to find the echotop height between two levels
                wgt = (refl10cm(k_echotop,iCell) - dbzcrit_min) / &
                      (refl10cm(k_echotop,iCell) - refl10cm(k_echotop+1,iCell))
                wgt = MAX(0., MIN(1., wgt))
                echotop = wgt * z(k_echotop+1,iCell) + (1.-wgt) * z(k_echotop,iCell) ! MSL
                echotop = echotop - z0(iCell) ! AGL
            end if

            ! Calculate convective mask
            if( rainncv(iCell).ge.pcpcrit_min .and. wmax.ge.wcrit_min .and. echotop.ge.etcrit_min ) then
                w_fct   = MAX(0., MIN(1., (wmax-wcrit_min)/(wcrit_max-wcrit_min)))
                dbz_fct = MAX(0., MIN(1., (dbzmax-dbzcrit_min)/(dbzcrit_max-dbzcrit_min)))
                et_fct  = MAX(0., MIN(1., (echotop-etcrit_min)/(etcrit_max-etcrit_min)))

                avcnvmsk(iCell) = MAX(avcnvmsk(iCell), 1.+w_fct+dbz_fct+et_fct )
            end if
        end do

    end subroutine

   !==================================================================================================
    subroutine interp_diagnostics(mesh, state, time_lev, diag, qi_200hPa, qi_250hPa, virtualT_400hPa)
   !==================================================================================================

       !input arguments:
        type (mpas_pool_type), intent(in)  :: mesh
        type (mpas_pool_type), intent(in) :: state
        integer, intent(in) :: time_lev              ! which time level to use from state
       
       !inout arguments:
        type (mpas_pool_type), intent(inout) :: diag
       
       !output arguments:
        real (kind=RKIND), dimension(:), intent(out) :: qi_200hPa
        real (kind=RKIND), dimension(:), intent(out) :: qi_250hPa
        real (kind=RKIND), dimension(:), intent(out) :: virtualT_400hPa

       !local variables:
        integer :: iCell,iVert,iVertD,k,kk
        integer, pointer :: nCells, nCellsSolve, nVertLevels, nVertices, vertexDegree
        integer :: nVertLevelsP1
        integer, pointer :: index_qv, index_qi, num_scalars
       
        real (kind=RKIND), dimension(:,:), pointer :: exner
        real (kind=RKIND), dimension(:,:), pointer :: theta_m

        real (kind=RKIND), dimension(:,:), pointer :: pressure_b, pressure_p 
        real (kind=RKIND), dimension(:,:,:), pointer :: scalars
       
        real (kind=RKIND), dimension(:), allocatable :: temperature_400hPa
        real (kind=RKIND), dimension(:), allocatable :: qv_400hPa

        real (kind=RKIND), dimension(:,:), allocatable :: pressure, temperature
       !local interpolated fields:
        integer :: nIntP
        real (kind=RKIND), dimension(:,:), allocatable :: press_in,qi_in,qv_in,temp_in
        real (kind=RKIND), dimension(:,:), allocatable :: field_interp,press_interp

        call mpas_pool_get_dimension(mesh, 'nCells', nCells)
        call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)
        call mpas_pool_get_dimension(mesh, 'nVertices', nVertices)
        call mpas_pool_get_dimension(state, 'index_qv', index_qv)
        call mpas_pool_get_dimension(state, 'index_qi', index_qi)
        call mpas_pool_get_dimension(state, 'num_scalars', num_scalars)
       
        nVertLevelsP1 = nVertLevels + 1

        call mpas_pool_get_array(state, 'theta_m', theta_m, time_lev)
        call mpas_pool_get_array(state, 'scalars', scalars, time_lev)
       
        call mpas_pool_get_array(diag, 'exner', exner)
        call mpas_pool_get_array(diag, 'pressure_base', pressure_b)
        call mpas_pool_get_array(diag, 'pressure_p', pressure_p)

        if(.not.allocated(pressure)    ) allocate(pressure(nVertLevels,nCells)      )
        if(.not.allocated(temperature) ) allocate(temperature(nVertLevels,nCells)   )
       
       !calculation of total pressure at cell centers (at mass points):
        do iCell = 1, nCells
        do k = 1, nVertLevels
           pressure(k,iCell)    = (pressure_p(k,iCell) + pressure_b(k,iCell)) / 100._RKIND
        enddo
        enddo
              
       !calculation of temperature at cell centers:
        do iCell = 1,nCells
        do k = 1,nVertLevels
            temperature(k,iCell) = (theta_m(k,iCell)/(1._RKIND+rvord*scalars(index_qv,k,iCell)))*exner(k,iCell) 
        enddo
        enddo
       
       !interpolation to fixed pressure levels for fields located at cells centers and at mass points:
        nIntP = 2
        if(.not.allocated(field_interp)) allocate(field_interp(nCells,nIntP) )
        if(.not.allocated(press_interp)) allocate(press_interp(nCells,nIntP) )
        do iCell = 1, nCells
           press_interp(iCell,1) = 200.0_RKIND
           press_interp(iCell,2) = 250.0_RKIND
        enddo
       
        if(.not.allocated(press_in)) allocate(press_in(nCells,nVertLevels))
        if(.not.allocated(temp_in)) allocate(temp_in(nCells,nVertLevels))
        if(.not.allocated(qi_in)) allocate(qi_in(nCells,nVertLevels))
        if(.not.allocated(qv_in)) allocate(qv_in(nCells,nVertLevels))

        do iCell = 1, nCells
        do k = 1, nVertLevels
           kk = nVertLevels+1-k
           press_in(iCell,kk) = pressure(k,iCell)
           temp_in(iCell,kk) = temperature(k,iCell)
           ! Cloud Ice and Water Vapor Mixing Ratios
           qi_in(iCell,kk) = scalars(index_qi,kk,iCell)
           qv_in(iCell,kk) = scalars(index_qv,kk,iCell)
        enddo
        enddo

        call interp_tofixed_pressure(nCells,nVertLevels,nIntP,press_in,qi_in,press_interp,field_interp)
        do iCell = 1, nCells
            qi_200hPa(iCell) = field_interp(iCell,1)
            qi_250hPa(iCell) = field_interp(iCell,2)
        enddo
      
        if(allocated(temperature) ) deallocate(temperature )
        if(allocated(pressure)    ) deallocate(pressure    ) 
        if(allocated(qi_in)       ) deallocate(qi_in)       
        if(allocated(field_interp)) deallocate(field_interp)
        if(allocated(press_interp)) deallocate(press_interp)
       
        ! temperature and qv at 400hPa
        allocate(field_interp(nCells, 1))
        allocate(press_interp(nCells, 1))

        ! We only interpolate T and qv to 400mb
        press_interp(:,1) = 400.0_RKIND
        allocate(qv_400hPa(nCells))
        allocate(temperature_400hPa(nCells))
        call interp_tofixed_pressure(nCells,nVertLevels,1,press_in,qv_in,press_interp,field_interp)
        do iCell=1, nCells
            qv_400hPa(iCell) = field_interp(iCell,1)
        enddo
        call interp_tofixed_pressure(nCells,nVertLevels,1,press_in,temp_in,press_interp,field_interp)
        do iCell=1, nCells
            temperature_400hPa(iCell) = field_interp(iCell,1)
        enddo

        ! Calculate virtual temperature at 400mb     
        !call compute_virtual_temperature(temperature_400hPa, qv_400hPa, virtualT_400hPa)
        do iCell = 1, nCells
            virtualT_400hPa(iCell) = temperature_400hPa(iCell) * (1.0 + qv_400hPa(iCell) * inveps) / (1.0 + qv_400hPa(iCell))
        enddo
 
        deallocate(qv_400hPa)
        deallocate(temperature_400hPa)
        deallocate(qv_in)
        deallocate(press_in)
        deallocate(temp_in)
        deallocate(press_interp)
        deallocate(field_interp)

    end subroutine interp_diagnostics


   !***********************************************************************
   !
   !  subroutine compute_virtual_temperature
   !
   !> \brief   Computes virtual Temperature given T and mixing ratio
   !> \author  Cathryn Meyer
   !> \date    Dec 2017
   !> \details
   !>  Given 1d temperature and mixing ratio fields, this routine
   !>  will compute the virtual temperature
   !
   !----------------------------------------------------------------------- 
    subroutine compute_virtual_temperature(t, mr, vt)
   
       implicit none
   
       real(kind=RKIND), dimension(:), intent(out) :: vt
       real(kind=RKIND), dimension(:), intent(in) :: t
       real(kind=RKIND), dimension(:), intent(in) :: mr
   
       integer :: nCells
       integer :: iCell, k
       real :: Rd = 287.
       real :: Rv = 461.5
       real :: eps, inveps
       eps = Rd / Rv    !0.622 dimensionless
       inveps = 1. / eps

       !
       ! Get dimensions of input arrays
       !
       nCells = size(t, 1) 
       !if(.not.allocated(vt)) allocate(vt(nCells)) 

       !
       !
       do iCell=1,nCells
   
          vt(iCell) = t(iCell) * (1.0 + mr(iCell) * inveps) / (1.0 + mr(iCell))
      
       end do
   
    end subroutine compute_virtual_temperature

   !==================================================================================================
    subroutine interp_tofixed_pressure(ncol,nlev_in,nlev_out,pres_in,field_in,pres_out,field_out)
   !==================================================================================================
   
   !input arguments:
    integer,intent(in):: ncol,nlev_in,nlev_out
   
    real(kind=RKIND),intent(in),dimension(ncol,nlev_in) :: pres_in,field_in
    real(kind=RKIND),intent(in),dimension(ncol,nlev_out):: pres_out
   
   !output arguments:
    real(kind=RKIND),intent(out),dimension(ncol,nlev_out):: field_out
   
   !local variables:
    integer:: icol,k,kk
    integer:: kkstart,kount
    integer,dimension(ncol):: kupper
   
    real(kind=RKIND):: dpl,dpu
   
   !--------------------------------------------------------------------------------------------------
      
    do icol = 1, ncol
       kupper(icol) = 1
    enddo
   
    do k = 1, nlev_out 
   
       kkstart = nlev_in
       do icol = 1, ncol
          kkstart = min0(kkstart,kupper(icol))
       enddo
       kount = 0
   
       do kk = kkstart, nlev_in-1
          do icol = 1, ncol
             if(pres_out(icol,k).gt.pres_in(icol,kk).and.pres_out(icol,k).le.pres_in(icol,kk+1)) then
                kupper(icol) = kk
                kount = kount + 1
   !            write(0,201) kupper(icol),pres_out(icol,k),pres_in(icol,kk),pres_in(icol,kk+1)
             endif
          enddo
   
          if(kount.eq.ncol) then
             do icol = 1, ncol
                dpu = pres_out(icol,k) - pres_in(icol,kupper(icol))
                dpl = pres_in(icol,kupper(icol)+1) - pres_out(icol,k)
                field_out(icol,k) = (field_in(icol,kupper(icol))*dpl &
                                  + field_in(icol,kupper(icol)+1)*dpu)/(dpl + dpu)
             end do
             goto 35
           end if
       enddo
   
       do icol = 1, ncol
          if(pres_out(icol,k) .lt. pres_in(icol,1)) then
             field_out(icol,k) = field_in(icol,1)*pres_out(icol,k)/pres_in(icol,1)
          elseif(pres_out(icol,k) .gt. pres_in(icol,nlev_in)) then
             field_out(icol,k) = field_in(icol,nlev_in)
          else
             dpu = pres_out(icol,k) - pres_in(icol,kupper(icol))
             dpl = pres_in(icol,kupper(icol)+1) - pres_out(icol,k)
             field_out(icol,k) = (field_in(icol,kupper(icol))*dpl &
                               + field_in(icol,kupper(icol)+1)*dpu)/(dpl + dpu)
          endif
       enddo
   
    35 continue
   
    enddo
   
    end subroutine interp_tofixed_pressure



    subroutine aviation_diagnostics_reset()
        use mpas_atm_diagnostics_utils, only : MPAS_field_will_be_written
        use mpas_pool_routines, only : mpas_pool_get_array

        implicit none

        real (kind=RKIND), dimension(:), pointer :: avcnvmsk

        if (is_needed_avcnvmsk .and. MPAS_field_will_be_written('avcnvmsk')) then
            call mpas_pool_get_array(diag, 'avcnvmsk', avcnvmsk)
            avcnvmsk(:) = 0._RKIND
        end if
    end subroutine aviation_diagnostics_reset


    subroutine aviation_diagnostics_cleanup()
        implicit none

        if (allocated(neighborhood)) deallocate(neighborhood)
        if (allocated(nNeighbors))   deallocate(nNeighbors)
    end subroutine aviation_diagnostics_cleanup

end module aviation_diagnostics



