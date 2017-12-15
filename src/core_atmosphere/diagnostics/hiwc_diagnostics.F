!  hiwc_diagnostics.F
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

module hiwc_diagnostics

    use mpas_derived_types, only : MPAS_pool_type
    use mpas_kind_types, only : RKIND

    type (MPAS_pool_type), pointer :: mesh
    type (MPAS_pool_type), pointer :: state
    type (MPAS_pool_type), pointer :: diag

    public :: hiwc_diagnostics_setup, &
              hiwc_diagnostics_update, &
              hiwc_diagnostics_compute, &
              hiwc_diagnostics_reset, &
              hiwc_diagnostics_cleanup

    private

    contains


    !-----------------------------------------------------------------------
    !  routine hiwc_diagnostics_setup
    !
    !> \brief   Initialize the hiwc_diagnostics module
    !> \author  Meyer
    !> \date    Dec 2017
    !> \details
    !>  Initialize the diagnostic module.
    !
    !-----------------------------------------------------------------------
    subroutine hiwc_diagnostics_setup(all_pools)

        use mpas_derived_types, only : MPAS_pool_type, MPAS_clock_type
        use mpas_pool_routines, only : mpas_pool_get_subpool

        implicit none

        type (MPAS_pool_type), pointer :: all_pools

        call mpas_pool_get_subpool(all_pools, 'mesh', mesh)
        call mpas_pool_get_subpool(all_pools, 'state', state)
        call mpas_pool_get_subpool(all_pools, 'diag', diag)

    end subroutine hiwc_diagnostics_setup


    subroutine hiwc_diagnostics_update()
        implicit none
        ! Nothing to update (every timestep) for hiwc
    end subroutine hiwc_diagnostics_update


    !-----------------------------------------------------------------------
    !  routine hiwc_diagnostics_compute
    !
    !> \brief  Compute HIWC for model output
    !> \author Cathryn Meyer
    !> \date   Dec 2017
    !> \details
    !>  Compute HIWC before model output is written.
    !
    !-----------------------------------------------------------------------
    subroutine hiwc_diagnostics_compute()

        use mpas_pool_routines, only : mpas_pool_get_dimension, mpas_pool_get_array
        use mpas_atm_diagnostics_utils, only : MPAS_field_will_be_written

        implicit none

        integer, pointer :: nCells, nVertLevels, nEdges
        integer, dimension(:), pointer :: nEdgesOnCell
        integer, dimension(:,:), pointer :: edgesOnCell

        real(kind=RKIND) :: w200, w250

        ! These are in degrees C
        real (kind=RKIND), parameter :: warm_boundary_temp = -15.
        real (kind=RKIND), parameter :: cold_boundary_temp = -17.

        real (kind=RKIND), dimension(:,:), pointer :: hiwc

        integer :: iEdge, iCell, k, e

        if ( MPAS_field_will_be_written('hiwc') ) then
            ! Mesh information
            call mpas_pool_get_dimension(mesh, 'nCells', nCells)
            
            ! Get cloud ice mixing ratio at 200hPa and 250hPa 
            ! and virtual Temperature at 400 hPa
            call interp_diagnostics(mesh, state, 1, diag)

            ! Target array
            call mpas_pool_get_array(diag, 'hiwc', hiwc)

            ! Allocate arrays for holding diagonal and off-diagonal deformation terms

            do iCell=1,nCells
                w200 = MIN(1.0, MAX(0.0, 
                           (warm_boundary_temp - virtualT_400hPa(iCell)) / 
                           (warm_boundary_temp - cold_boundary_temp) ) )

                w250 = 1.0 - w200
                hiwc(iCell) = MAX(0, MIN(3.0, 
                                  ( (w250 * qi_250hPa * 1.0E6 + 
                                     w200 * qi_200hPa * 1.0E6) - 25 ) / 80 ) )

            end do

        end if

    end subroutine hiwc_diagnostics_compute


   !==================================================================================================
    subroutine interp_diagnostics(mesh, state, time_lev, diag)
   !==================================================================================================

       !input arguments:
        type (mpas_pool_type), intent(in)  :: mesh
        type (mpas_pool_type), intent(in) :: state
        integer, intent(in) :: time_lev              ! which time level to use from state
       
       !inout arguments:
        type (mpas_pool_type), intent(inout) :: diag
       
       !local variables:
        integer :: iCell,iVert,iVertD,k,kk
        integer, pointer :: nCells, nCellsSolve, nVertLevels, nVertices, vertexDegree
        integer :: nVertLevelsP1
        integer, pointer :: index_qv, num_scalars
       
        type (field2DReal), pointer:: pressure_p_field
       
        real (kind=RKIND), dimension(:,:), pointer :: exner
        real (kind=RKIND), dimension(:,:), pointer :: qi, qv, theta_m

        real (kind=RKIND), dimension(:,:), pointer :: pressure_b, pressure_p 
        real (kind=RKIND), dimension(:,:,:), pointer :: scalars
       
        real (kind=RKIND), dimension(:), pointer :: qi_200hPa
        real (kind=RKIND), dimension(:), pointer :: qi_250hPa
        real (kind=RKIND), dimension(:), pointer :: temperature_400hPa
        real (kind=RKIND), dimension(:), pointer :: qv_400hPa
        real (kind=RKIND), dimension(:), pointer :: virtualT_400hPa

        real (kind=RKIND), dimension(:,:), allocatable :: pressure, temperature
       !local interpolated fields:
        integer :: nIntP
        real (kind=RKIND), dimension(:,:), allocatable :: field_in,press_in
        real (kind=RKIND), dimension(:,:), allocatable :: field_interp,press_interp

        call mpas_pool_get_dimension(mesh, 'nCells', nCells)
        call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)
        call mpas_pool_get_dimension(mesh, 'nVertices', nVertices)
        call mpas_pool_get_dimension(state, 'index_qv', index_qv)
        call mpas_pool_get_dimension(state, 'num_scalars', num_scalars)
        call mpas_pool_get_array(state, 'qi', qi, time_lev)
        call mpas_pool_get_array(state, 'qv', qv, time_lev)
       
        nVertLevelsP1 = nVertLevels + 1

        call mpas_pool_get_array(state, 'theta_m', theta_m, time_lev)
        call mpas_pool_get_array(state, 'scalars', scalars, time_lev)
        call mpas_pool_get_field(diag, 'pressure_p', pressure_p_field)
        call mpas_dmpar_exch_halo_field(pressure_p_field)
       
        call mpas_pool_get_array(diag, 'exner', exner)
        call mpas_pool_get_array(diag, 'pressure_base', pressure_b)
        call mpas_pool_get_array(diag, 'pressure_p', pressure_p)

        call mpas_pool_get_array(diag, 'virtualT_400hPa', virtualT_400hPa)
        call mpas_pool_get_array(diag, 'qi_200hPa', qi_200hPa)
        call mpas_pool_get_array(diag, 'qi_250hPa', qi_250hPa)
        call mpas_pool_get_array(diag, 'temperature_400hPa', temperature_400hPa)
        call mpas_pool_get_array(diag, 'qv_400hPa', qv_400hPa)

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
            ! Vapor pressure (NB: pressure here is already in hPa)
            !evp = pressure(k,iCell) * scalars(index_qv,k,iCell) / (scalars(index_qv,k,iCell) + 0.622_RKIND)
            !evp = max(evp, 1.0e-8_RKIND)
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
        do iCell = 1, nCells
        do k = 1, nVertLevels
           kk = nVertLevels+1-k
           press_in(iCell,kk) = pressure(k,iCell)
        enddo
        enddo
       
        if(.not.allocated(qi_in)) allocate(qi_in(nCells,nVertLevels))
        if(.not.allocated(qv_in)) allocate(qv_in(nCells,nVertLevels))

       ! Cloud Ice and Water Vapor Mixing Ratios
        do iCell = 1, nCells
        do k = 1, nVertLevels
            kk = nVertLevels+1-k
            qi_in(iCell,kk) = qi(k,iCell)
            qv_in(iCell,kk) = qv(k,iCell)
        enddo
        enddo
        call interp_tofixed_pressure(nCells,nVertLevels,nIntP,press_in,qi_in,press_interp,field_interp)
        qi_200hPa(1:nCells) = field_interp(1:nCells,1)
        qi_250hPa(1:nCells) = field_interp(1:nCells,2)
       
        if(allocated(qi_in)) deallocate(qi_in)       
        if(allocated(field_interp)) deallocate(field_interp)
        if(allocated(press_interp)) deallocate(press_interp)
       
        ! temperature and qv at 400hPa
        allocate(field_interp(nCells, 1))
        allocate(press_interp(nCells, 1))

        ! We only interpolate T and qv to 400mb
        press_interp(:,1) = 400.0_RKIND
        call interp_tofixed_pressure(nCells,nVertLevels,1,press_in,qv_in,press_interp,field_interp)
        qv_400hPa(1:nCells) = field_interp(1:nCells,1)
        call interp_tofixed_pressure(nCells,nVertLevels,1,press_in,temperature,press_interp,field_interp)
        temperature_400hPa(1:nCells) = field_interp(1:nCells,1)

        ! Calculate virtual temperature at 400mb     
        !call compute_virtual_temperature(temperature_400hPa, qv_400hPa, virtualT)
     
     
        deallocate(qv_in)
        deallocate(field_interp)
        deallocate(press_in)
        deallocate(press_interp)
      
        if(allocated(temperature) ) deallocate(temperature )
        if(allocated(pressure)    ) deallocate(pressure    )
       
   
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
    subroutine compute_virtual_temperature(temperature, mixing_ratio, virtualT)
   
       implicit none
   
       real(kind=RKIND), dimension(:), intent(out) :: virtualT
       real(kind=RKIND), dimension(:), intent(in) :: temperature
       real(kind=RKIND), dimension(:), intent(in) :: mixing_ratio
   
       integer :: nCells
       integer :: iCell, k
   
       !
       ! Get dimensions of input arrays
       !
       nCells = size(temperature, 1) 
   
       !
       !
       do iCell=1,nCells
   
          virtualT(iCell) = 0.0  
      
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



    subroutine hiwc_diagnostics_reset()
        implicit none
        ! Nothing to reset for hiwc
    end subroutine hiwc_diagnostics_reset


    subroutine deformation_hiwc_cleanup()
        implicit none
        ! Nothing to cleanup for hiwc
    end subroutine hiwc_diagnostics_cleanup

end module hiwc_diagnostics

