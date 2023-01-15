! Copyright (c) 2016,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the
! LICENSE file
! distributed with this code, or at
! http://mpas-dev.github.com/license.html
!=================================================================================================================

module pv_diagnostics

    use mpas_derived_types, only : MPAS_pool_type, MPAS_clock_type
    use mpas_kind_types, only : RKIND

    type (MPAS_pool_type), pointer :: mesh
    type (MPAS_pool_type), pointer :: state
    type (MPAS_pool_type), pointer :: diag
    type (MPAS_pool_type), pointer :: tend
    type (MPAS_pool_type), pointer :: tend_physics

    type (MPAS_clock_type), pointer :: clock

    public :: pv_diagnostics_setup, &                          ! Order these are called in mpas_atm_core.F is setup, reset, update, compute, reset, etc. 
              pv_diagnostics_compute, &
              pv_diagnostics_reset, &
              pv_diagnostics_update

    private

    logical :: need_ertel_pv, need_u_pv, need_v_pv, need_theta_pv, need_vort_pv, need_iLev_DT, &
               need_advection_zonal, need_advection_merid, need_advection_vertical, need_wcell, &                  
               need_tend_lw, need_tend_sw, need_tend_bl, need_tend_cu, need_tend_mix, need_tend_mp, &
               need_tend_diab, need_tend_fric, need_tend_diab_pv, need_tend_fric_pv, need_dtheta_mp, & 
               need_uzonal_prev, need_umerid_prev, need_wcell_prev, need_rho_prev, need_theta_prev, &                   
               need_pvvert_prev, need_epv_prev, need_iLev_prev

    !=================================================================================================================
    ! MPAS code to compute Ertel's potential vorticity and tendency contributions
    ! Original diagnostics code was written by Nick Szapiro - 2016
    ! Significant changes to the code were made by Manda Chasteen (chasteen@ucar.edu) - 2022
    !
    ! subroutines contained in pv_diagnostics.F:
    ! ------------------------------------------
    ! pv_diagnostics_setup    :
    ! pv_diagnostics_update   :  
    ! pv_diagnostics_compute  :
    ! pv_diagnostics_reset    :


    ! Changes made by Manda Chasteen include:
    ! * Different formulation for calculation of horizontal gradients on native MPAS grid. The updated method is based on 
    !   Eq. 22 in Ringler et al. (2010) and is more robust than the previous method implemented by NS 
    ! * Reconstruction of horizontal gradients on each cell's edges to the cell center following the same method as the 
    !   horizontal wind reconstruction in mpas_vector_reconstruction.F
    ! * Incorporation of simple zonal, meridional, and vertical PV advection calculatons using the reconstructed PV gradients 
    ! * Changes to the calculation of the PV tendency terms to ensure that the potential temperature, wind, and density 
    !   fields from the beginning of the time step are used alongside the theta and wind tendencies in the calculations.
    !   In other words, the 3D gradient of the theta tendencies from a given time step should be dotted with the curl of 
    !   the UNMODIFIED 3D wind field at the beginning of the time step before any wind tendencies were added, and the curl 
    !   of the wind tendencies from a given time step should be dotted with the 3D gradient of the UNMODIFIED theta field
    !   at the beginning of the time step. This follows from the product rule.  
    !   This important change requires storing fields from the beginning of the time step to be used in the PV tendency 
    !   calculations because the model state and diagnostic fields are updated and assigned to time level 1 before the PV 
    !   diagnostics are called at the end of the time step in mpas_atm_core.F. Thus, before this change was implemented, 
    !   the updated variables from the end of the time step were incorrectly used alongside these tendencies.
    !   
    ! Modifications/additions needed:
    ! * Sum up tendencies over all over timesteps in outfile to compute averages over period? 
    ! * Incorporate PV into model integration so that it is advected like other state variables and/or make calculation
    !   of advection terms more sophisticated so the budget will balance?  

    contains

    !*********************************************************************************************************************
    ! pv_diagnostics_setup: initialize the PV diagnostics when called in mpas_atm_diagnostics_manager.F
    !*********************************************************************************************************************

    subroutine pv_diagnostics_setup(all_pools, simulation_clock)

        use mpas_derived_types, only : MPAS_pool_type, MPAS_clock_type, MPAS_STREAM_OUTPUT, MPAS_STREAM_INPUT, &
                                       MPAS_STREAM_INPUT_OUTPUT
        use mpas_pool_routines, only : mpas_pool_get_subpool

        implicit none

        type (MPAS_pool_type), pointer :: all_pools
        type (MPAS_clock_type), pointer :: simulation_clock

        call mpas_pool_get_subpool(all_pools, 'mesh', mesh)
        call mpas_pool_get_subpool(all_pools, 'state', state)
        call mpas_pool_get_subpool(all_pools, 'diag', diag)
        call mpas_pool_get_subpool(all_pools, 'tend', tend)
        call mpas_pool_get_subpool(all_pools, 'tend_physics', tend_physics)

        clock => simulation_clock
  
    end subroutine pv_diagnostics_setup

    !*********************************************************************************************************************
    ! pv_diagnostics_update: compute the PV diagnostics over each time step when called in mpas_atm_diagnostics_manager.F
    !*********************************************************************************************************************

    subroutine pv_diagnostics_update()
        use mpas_log, only : mpas_log_write

        implicit none

        ! Calculate PV diagnostics at each time step
        call mpas_log_write("Computing Ertel's PV and PV advection")
        call atm_compute_pv_diagnostics(state, 1, diag, mesh)
 
        call mpas_log_write('Computing PV tendency diagnostics.')
        call atm_compute_pvBudget_diagnostics(state, 1, diag, mesh, tend, tend_physics)

    end subroutine pv_diagnostics_update

    !*********************************************************************************************************************
    ! pv_diagnostics_compute: compute PV diagnostic fields at the end of the timestep and/or write PV diagnostic fields to 
    !                         model output when called in mpas_atm_diagnostics_manager.F
    !
    ! MC - since PV diagnostics are now computed at each time step, subroutine was changed to only write fields to outfiles                            
    !*********************************************************************************************************************

    subroutine pv_diagnostics_compute()

        use mpas_atm_diagnostics_utils, only : MPAS_field_will_be_written
        use mpas_log, only : mpas_log_write

        implicit none

        logical :: need_any_diags, need_any_budget, need_prev_fields

        need_any_diags = .false.
        need_any_budget = .false.

        ! Ertel's PV and fields interpolated to dynamic tropopause
        need_ertel_pv = MPAS_field_will_be_written('ertel_pv')
        need_any_diags = need_any_diags .or. need_ertel_pv
        need_u_pv = MPAS_field_will_be_written('u_pv')
        need_any_diags = need_any_diags .or. need_u_pv
        need_v_pv = MPAS_field_will_be_written('v_pv')
        need_any_diags = need_any_diags .or. need_v_pv
        need_theta_pv = MPAS_field_will_be_written('theta_pv')
        need_any_diags = need_any_diags .or. need_theta_pv
        need_vort_pv = MPAS_field_will_be_written('vort_pv')
        need_any_diags = need_any_diags .or. need_vort_pv
        need_iLev_DT = MPAS_field_will_be_written('iLev_DT')
        need_any_diags = need_any_diags .or. need_iLev_DT
        need_wCell = MPAS_field_will_be_written('wCell')
        need_any_diags = need_any_diags .or. need_wCell

        ! PV advection terms 
        need_advection_zonal = MPAS_field_will_be_written('advectionPV_zonal')                 ! MC added
        need_any_diags = need_any_diags .or. need_advection_zonal                              ! MC added
        need_advection_merid = MPAS_field_will_be_written('advectionPV_meridional')            ! MC added
        need_any_diags = need_any_diags .or. need_advection_merid                              ! MC added
        need_advection_vertical = MPAS_field_will_be_written('advectionPV_vertical')           ! MC added
        need_any_diags = need_any_diags .or. need_advection_vertical

        ! PV tendency terms 
        need_tend_lw = MPAS_field_will_be_written('depv_dt_lw')
        need_any_budget = need_any_budget .or. need_tend_lw
        need_tend_sw = MPAS_field_will_be_written('depv_dt_sw')
        need_any_budget = need_any_budget .or. need_tend_sw
        need_tend_bl = MPAS_field_will_be_written('depv_dt_bl')
        need_any_budget = need_any_budget .or. need_tend_bl
        need_tend_cu = MPAS_field_will_be_written('depv_dt_cu')
        need_any_budget = need_any_budget .or. need_tend_cu
        need_tend_mix = MPAS_field_will_be_written('depv_dt_mix')
        need_any_budget = need_any_budget .or. need_tend_mix
        need_dtheta_mp = MPAS_field_will_be_written('dtheta_dt_mp')
        need_any_budget = need_any_budget .or. need_dtheta_mp
        need_tend_mp = MPAS_field_will_be_written('depv_dt_mp')
        need_any_budget = need_any_budget .or. need_tend_mp
        need_tend_diab = MPAS_field_will_be_written('depv_dt_diab')
        need_any_budget = need_any_budget .or. need_tend_diab
        need_tend_fric = MPAS_field_will_be_written('depv_dt_fric')
        need_any_budget = need_any_budget .or. need_tend_fric
        need_tend_diab_pv = MPAS_field_will_be_written('depv_dt_diab_pv')
        need_any_budget = need_any_budget .or. need_tend_diab_pv
        need_tend_fric_pv = MPAS_field_will_be_written('depv_dt_fric_pv')
        need_any_budget = need_any_budget .or. need_tend_fric_pv

        ! MC - If fields do not need to be computed each time step, then uncomment the lines below and 
        ! comment the same lines in pv_diagnostics_update above
        ! Caution: only make this change if the PV tendency variables are not needed because they require
        ! stored fields from the previous time step!         
      
        ! Calculate PV diagnostics before outfiles are written at end of each time step
        !if (need_any_diags) then
            !call mpas_log_write("Computing Ertel's PV and PV advection")
            !call atm_compute_pv_diagnostics(state, 1, diag, mesh)
        !end if
        !if (need_any_budget) then
            !call mpas_log_write('Computing PV tendency diagnostics.')
            !call atm_compute_pvBudget_diagnostics(state, 1, diag, mesh, tend, tend_physics)
        !end if

        ! Save fields from beginning of time step to outfile before updating them 
        call mpas_log_write('Saving beginning of time step variables to outfile.')

        need_uzonal_prev = MPAS_field_will_be_written('uReconstructZonal_prev')
        need_umerid_prev = MPAS_field_will_be_written('uReconstructMeridional_prev')
        need_wcell_prev = MPAS_field_will_be_written('wCell_prev')
        need_theta_prev = MPAS_field_will_be_written('theta_prev')
        need_rho_prev = MPAS_field_will_be_written('rho_prev')
        need_pvvert_prev = MPAS_field_will_be_written('pv_vertex_prev')
        need_epv_prev = MPAS_field_will_be_written('ertel_pv_prev')
        need_iLev_prev = MPAS_field_will_be_written('iLev_DT_prev')

    end subroutine pv_diagnostics_compute

    !*********************************************************************************************************************
    ! pv_diagnostics_reset: Update beginning of time step fields after they have been written to outfile for use in next 
    !                       time step PV tendency calculations when called in mpas_atm_diagnostics_manager.F
    !*********************************************************************************************************************

    subroutine pv_diagnostics_reset()

        use mpas_atm_diagnostics_utils, only : MPAS_field_will_be_written
        use mpas_log, only : mpas_log_write

        implicit none

        ! Called immediately after diagnostics have been written
        ! Update previous variables for next timestep calculation

        call mpas_log_write('Updating previous fields.')
        call store_previous_vars(mesh, 2, state, diag)

    end subroutine pv_diagnostics_reset
     
    !*********************************************************************************************************************
    ! NS: Below are two subroutines (floodFill_strato and floodFill_tropo), designed to determine the first model level 
    ! above the dynamic tropopause, iLev_DT, which is designated as the 2-PVU isosurface. Only one of these subroutines 
    ! is used (toggled with "call floodFill_strato(mesh, diag, pvuVal, stratoPV)" and "call floodFill_tropo(mesh,diag,pvuVal)"
    ! in the atm_compute_pv_diagnostics subroutine below. The routines *should* produce equivalent estimates for iLev_DT. 
    !********************************************************************************************************************* 
   
    subroutine floodFill_strato(mesh, diag, pvuVal, stratoPV)
       !To find model level of dynamic tropopause: 
       !Simply searching down from TOA within each column to find first
       !instance of 2-PVU surface (i.e., where the PV drops below values
       !characteristic of the stratosphere) 
       !is buggy due to stratospheric wave breaking, which may induce
       !regions of low PV (i.e., PV < 2 PVU) within the stratosphere and
       !thus yield artifically
       !high estimations of the tropopause height. This seems to be more
       !problematic as the mesh gets finer and the vertical vorticity
       !field exhibits greater variability
       !or jumps. 
       !Note that these low-PV anomalies in the stratosphere may persist
       !for long times w/ slow mixing downstream of mountains or deep
       !convection.
       !A few quicker fixes (e.g., make sure PV < 2 PVU for a number of
       !layers; search down from 10 PVU instead of TOA) are hacky and not
       !robust.
       
       !To (hopefully) alleviate the problems resulting from wave
       !breaking, we can flood fill from a known
       !stratosphere region (e.g., where the model top > 2 PVU) and
       !filter down and around any problematic regions.
       !The problem w/ using only the flood fill is that strong surface
       !PV anomalies can connect to the 2-PVU surface aloft,
       !and the resulting "flood-filled 2 PVU" region can have sizeable
       !areas that are located just at/near the surface, while there is
       !clearly a 
       !tropopause above (i.e., as evident in a vertical cross-section).
       !To address the large near-surface blobs of PV > 2 PVU, will take
       !the flood fill mask and try to move upward from near the surface
       !to 10 PVU within a vertical column. 
       !If this can be done, then the low-level PV anomaly extends to the
       !stratosphere. Else, remove the stratospheric designation to
       !disconnect the "surface blob".
       
       !The "output" is iLev_DT, which is the vertical index for the
       !model level just above the dynamic tropopause (i.e., where PV >=
       !pvuVal, which is set below in atm_compute_pv_diagnostics to 2
       !PVU). 
       !If iLev_DT > nVertLevels, then pvuVal is found only above the
       !column (i.e., entire column is in troposphere). If iLev_DT < 1,
       !PV >= pvuVal extends vertically through the entire column 
       !(i.e., the entire column is within the stratosphere).
       !Communication between blocks during the flood fill may be needed
       !to treat some edge cases appropriately.
 
       use mpas_pool_routines, only : mpas_pool_get_dimension, mpas_pool_get_array, mpas_pool_get_field
       use mpas_dmpar, only : mpas_dmpar_max_int,mpas_dmpar_exch_halo_field
       use mpas_derived_types, only : dm_info, field2DInteger
      
       implicit none
       
       type (mpas_pool_type), intent(in) :: mesh
       type (mpas_pool_type), intent(inout) :: diag
       real(kind=RKIND), intent(in) :: pvuVal, stratoPV
       
       integer :: iCell, k, nChanged, iNbr, iCellNbr, levInd, haloChanged, global_haloChanged !INCORPORATE LEVEL INDEX FOR REMOVING SFC BLOB
       integer, pointer :: nCells, nVertLevels
       integer, dimension(:), pointer :: nEdgesOnCell, iLev_DT
       integer, dimension(:,:), pointer :: cellsOnCell, inStrato ! inStrato wasn't here in original procedure
       
       type (field2DInteger), pointer :: inStrato_f ! line added to match troposphere procedure workflow
       
       real(kind=RKIND) :: sgnHemi, sgn
       real(kind=RKIND),dimension(:),pointer:: latCell
       real(kind=RKIND), dimension(:,:), pointer :: ertel_pv
       
       type (dm_info), pointer :: dminfo
       
       integer, dimension(:,:), allocatable :: candInStrato ! whether point is potentially inStrato
       
       call mpas_pool_get_dimension(mesh, 'nCells', nCells)
       call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)
       call mpas_pool_get_array(mesh, 'nEdgesOnCell', nEdgesOnCell)
       call mpas_pool_get_array(mesh, 'cellsOnCell', cellsOnCell)
       call mpas_pool_get_array(mesh, 'latCell', latCell)
 
       call mpas_pool_get_array(diag, 'ertel_pv', ertel_pv)
       call mpas_pool_get_array(diag, 'iLev_DT', iLev_DT)
       call mpas_pool_get_array(diag, 'inStrato', inStrato) ! was allocated below in original script
       
       allocate(candInStrato(nVertLevels, nCells+1))
       !allocate(inStrato(nVertLevels, nCells+1))
       candInStrato(:,:) = 0
       inStrato(:,:) = 0
       
       !store whether each grid point has |PV| >= pvuVal to avoid
       !repeating logic. we'll use candInStrato as a isVisited marker for
       !potential stratosphere grid points further below.
       do iCell=1,nCells
          sgnHemi = sign(1.0_RKIND, latCell(iCell)) !at the equator, sign(0)=0
          if (sgnHemi .EQ. 0.0) sgnHemi = 1.0_RKIND
          do k=1,nVertLevels
             sgn = ertel_pv(k,iCell)*sgnHemi-pvuVal
             if (sgn .GE. 0) candInStrato(k,iCell) = 1
          end do
       end do
       
       !loop over cells and top 5 model levels to seed flood fill with
       !model top that's located above DT (i.e., where |PV| >= pvuVal).
       !can have model top with PV below 2 PVU (e.g., in tropics)
       nChanged = 0
       do iCell=1,nCells
          do k=nVertLevels-5,nVertLevels
             if (candInStrato(k,iCell) .GT. 0) then
                inStrato(k,iCell) = 1
                !candInStrato(k,iCell) = 0
                nChanged = nChanged+1
             end if
          end do
       end do
       
       !flood fill from the determined seeds. since I don't know enough
       !fortran,
       !we'll just brute force a continuing loop rather than queue.
       !here is where the changes to account for domain communication are
       !needed.
       
       call mpas_pool_get_field(diag, 'inStrato', inStrato_f)
       dminfo => inStrato_f % block % domain % dminfo
       global_haloChanged = 1
       
       do while(global_haloChanged .GT. 0) !any cell in a halo has changed, to propagate to other domains
         global_haloChanged = 0 !aggregate the number of changed cells w/in the loop below
         do while(nChanged .GT. 0)
           nChanged = 0
           do iCell=1,nCells !should we look for neighbors of halo cells?
             do k=nVertLevels,1,-1 ! loop over vertical levels from top down
                !update if candidate and neighbor in strato
                if ((candInStrato(k,iCell) .GT. 0) .AND. (inStrato(k,iCell).LT.1) ) then ! modified to match trop routine
                   !nbr above
                   if (k .LT. nVertLevels) then
                     if (inStrato(k+1,iCell) .GT. 0) then
                       inStrato(k,iCell) = 1
                       !candInStrato(k,iCell) = 0 ! commented out to be
                       !consistent with trop routine
                       nChanged = nChanged+1
                       cycle
                     end if
                   end if
                 
                   !side nbrs
                   do iNbr = 1, nEdgesOnCell(iCell)
                     iCellNbr = cellsOnCell(iNbr,iCell)
                     if (inStrato(k,iCellNbr) .GT. 0) then
                       inStrato(k,iCell) = 1
                       !candInStrato(k,iCell) = 0 ! commented out to be
                       !consistent with trop routine
                       nChanged = nChanged+1
                       !exit ! was cycle, but tropspheric loop has exit
                       !here. why?
                       cycle
                     end if
                   end do
                 
                   !nbr below
                   if (k .GT. 1) then
                     if (inStrato(k-1,iCell) .GT. 0) then
                       inStrato(k,iCell) = 1
                       !candInStrato(k,iCell) = 0 ! commented out to be
                       !consistent with trop routine
                       nChanged = nChanged+1
                       cycle
                     end if
                   end if
                     
                end if !candInStrato
             end do !levels
           end do !cells
           global_haloChanged = global_haloChanged+nChanged
         end do !while w/in domain
         
         !communicate to other domains for edge case where a chunk of a
         !block hasn't gotten to fill
         nChanged = global_haloChanged
         call mpas_dmpar_max_int(dminfo, nChanged, global_haloChanged)
         if (global_haloChanged .GT. 0) then !communicate inStrato everywhere
           call mpas_dmpar_exch_halo_field(inStrato_f)
         end if
         nChanged = global_haloChanged !so each block will iterate again if anything changed
       end do !while haloChanged
       deallocate(candInStrato)
       
       
       !Detach high surface PV blobs w/o vertical connection to
       !"stratosphere"
       do iCell=1,nCells
         if (inStrato(1,iCell) .GT. 0) then
           !see how high up we can walk in the column
           do k=2,nVertLevels
             if (inStrato(k,iCell) .LT. 1) then
               exit
             end if !k is highest connected level to sfc
             sgnHemi = sign(1.0_RKIND, latCell(iCell)) !at the equator, sign(0)=0
             if (sgnHemi .EQ. 0.0) sgnHemi = 1.0_RKIND
             sgn = ertel_pv(k,iCell)*sgnHemi-stratoPV
             if (sgn .LT. 0) then !not actually connected to "stratosphere"
               inStrato(1:k,iCell) = 0
             end if
           end do !k
         end if !inStrato at sfc
       end do !iCell
       
       !Fill iLev_DT with the lowest level above the tropopause (If DT
       !above column, iLev>nVertLevels. If DT below column, iLev=0.
       nChanged = 0
       do iCell=1,nCells
         do k=1,nVertLevels
           if (inStrato(k,iCell) .GT. 0) then
             nChanged = 1
             exit
           end if
         end do !k
         if (nChanged .GT. 0) then !found lowest level
           if (k .EQ. 1) then 
             sgnHemi = sign(1.0_RKIND, latCell(iCell))
             sgn = ertel_pv(k,iCell)*sgnHemi-pvuVal
             if (sgn .GT. 0) then !whole column above DT
               iLev_DT(iCell) = 0
             end if
           else
             iLev_DT(iCell) = k
           end if
         else !whole column below DT
           iLev_DT(iCell) = nVertLevels+2
         end if
       end do !iCell
      
    end subroutine floodFill_strato
   
    !*********************************************************************************************************************
    ! Alternative approach: 
    !*********************************************************************************************************************

    subroutine floodFill_tropo(mesh, diag, pvuVal)
       !To find model level of dynamic tropopause: 
       !Simply searching down from TOA within each column to find first
       !instance of 2-PVU surface (i.e., where the PV drops below values
       !characteristic of the stratosphere) 
       !is buggy due to stratospheric wave breaking, which may induce
       !regions of low PV (i.e., PV < 2 PVU) within the stratosphere and
       !thus yield artifically
       !high estimations of the tropopause height. This seems to be more
       !problematic as the mesh gets finer and the vertical vorticity
       !field exhibits greater variability
       !or jumps. 
       !Note that these low-PV anomalies in the stratosphere may persist
       !for long times w/ slow mixing downstream of mountains or deep
       !convection.
       !A few quicker fixes (e.g., make sure PV < 2 PVU for a number of
       !layers; search down from 10 PVU instead of TOA) are hacky and not
       !robust.
       
       !Two flood fill subroutine options are to:
       ! (1) floodFill_strato: flood fill the stratosphere (PV >= 2 PVU)
       ! from stratosphere seeds placed near model top. Strong surface PV
       ! anomalies can connect to 2-PVU region aloft, 
       !     and the resulting "flood-filled 2 PVU" can have sizeable
       !     areas that are located just at/near the surface, while there
       !     is clearly a tropopause above
       !     (i.e., as evident in a vertical cross-section). To address
       !     the large near-surface blobs of PV > 2 PVU, will take the
       !     flood fill mask and try to move upward from
       !     near the surface to 10 PVU within a vertical column. If this
       !     can be done, then the low-level PV anomaly extends to the
       !     stratosphere. Else, remove the stratospheric
       !     designation to disconnect the "surface blob".
       ! (2) floodFill_tropo: flood fill the troposphere (PV < 2 PVU)
       ! from troposphere seeds placed near the surface.
       !
       ! Comparing the two procedures... Somewhat paradoxically, the
       ! bottom of the stratosphere is located lower than the top of the
       ! troposphere.  
      
       !The "output" is iLev_DT, which is the vertical index for the
       !model level just above the dynamic tropopause (i.e., where PV >=
       !pvuVal, which is set below in atm_compute_pv_diagnostics to 2
       !PVU). 
       !If iLev_DT > nVertLevels, then pvuVal is found only above the
       !column (i.e., entire column is in troposphere). If iLev_DT < 1,
       !PV >= pvuVal extends vertically through the entire column 
       !(i.e., the entire column is within the stratosphere).
       !Communication between blocks during the flood fill may be needed
       !to treat some edge cases appropriately.
       
       !Originally, it was assumed that each (MPI) domain would have > 0
       !cells with "right" DT found by flood filling.
       !However, for "small" domains (especially over the poles -- for
       !example, in the Arctic say during winter, when the entire surface
       !can be capped by high PV), 
       !this becomes problematic. So, we need to communicate between
       !domains during the flood fill procedure or else we will find the
       !DT located at/near the surface.
       !The extreme limiting case is if we had every cell as its own
       !domain; then, it's clear that there has to be communication.
 
       use mpas_pool_routines, only : mpas_pool_get_dimension, mpas_pool_get_array, mpas_pool_get_field
       use mpas_dmpar, only : mpas_dmpar_max_int,mpas_dmpar_exch_halo_field
       use mpas_derived_types, only : dm_info, field2DInteger
      
       implicit none
       
       type (mpas_pool_type), intent(in) :: mesh
       type (mpas_pool_type), intent(inout) :: diag
       real(kind=RKIND), intent(in) :: pvuVal
 
       integer :: iCell, k, nChanged, iNbr, iCellNbr, levInd, haloChanged, global_haloChanged
       integer, pointer :: nCells, nVertLevels
       integer, dimension(:), pointer :: nEdgesOnCell, iLev_DT
       integer, dimension(:,:), pointer :: cellsOnCell, inTropo
 
       type (field2DInteger), pointer :: inTropo_f
 
       real(kind=RKIND) :: sgnHemi, sgn
       real(kind=RKIND),dimension(:),pointer:: latCell
       real(kind=RKIND), dimension(:,:), pointer :: ertel_pv
       
       type (dm_info), pointer :: dminfo
 
       integer, dimension(:,:), allocatable :: candInTropo !whether in troposphere
       
       call mpas_pool_get_dimension(mesh, 'nCells', nCells)
       call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)
       call mpas_pool_get_array(mesh, 'nEdgesOnCell', nEdgesOnCell)
       call mpas_pool_get_array(mesh, 'cellsOnCell', cellsOnCell)
       call mpas_pool_get_array(mesh, 'latCell', latCell)
 
       call mpas_pool_get_array(diag, 'ertel_pv', ertel_pv)
       !call mpas_pool_get_array(diag, 'iLev_DT_trop', iLev_DT)
       call mpas_pool_get_array(diag, 'iLev_DT', iLev_DT)
       call mpas_pool_get_array(diag, 'inTropo', inTropo)
       
       allocate(candInTropo(nVertLevels, nCells+1))
       candInTropo(:,:) = 0
       inTropo(:,:) = 0
       !store whether each level above DT to avoid repeating logic. we'll
       !use cand as a isVisited marker further below.
       do iCell=1,nCells
          sgnHemi = sign(1.0_RKIND, latCell(iCell)) !at the equator, sign(0)=0
          if (sgnHemi .EQ. 0.0) sgnHemi = 1.0_RKIND
          do k=1,nVertLevels
             sgn = ertel_pv(k,iCell)*sgnHemi-pvuVal
             if (sgn .LT. 0) candInTropo(k,iCell) = 1
          end do
       end do
       
       !seed flood fill with near surface that's below DT (can have
       !surface above 2pvu from pv anoms).
       !Note that this would be wrong if low PV "stratospheric" blobs are
       !right above the surface
       nChanged = 0
       levInd = min(nVertLevels, 3)
       do iCell=1,nCells
          do k=1,levInd
             if (candInTropo(k,iCell) .GT. 0) then
                inTropo(k,iCell) = 1
                !candInTropo(k,iCell) = 0
                nChanged = nChanged+1
             end if
          end do
       end do
       
       !flood fill from the given seeds. since I don't know enough
       !fortran,
       !we'll just brute force a continuing loop rather than queue.
       call mpas_pool_get_field(diag, 'inTropo', inTropo_f)
       dminfo => inTropo_f % block % domain % dminfo
       global_haloChanged = 1
       do while(global_haloChanged .GT. 0) !any cell in a halo has changed, to propagate to other domains
         global_haloChanged = 0 !aggregate the number of changed cells w/in the loop below
         do while(nChanged .GT. 0)
           nChanged = 0
           do iCell=1,nCells !should we look for neighbors of halo cells?
             do k=1,nVertLevels
                !update if candidate and neighbor in troposphere
                if ((candInTropo(k,iCell) .GT. 0) .AND. (inTropo(k,iCell).LT.1) ) then
                   !nbr below
                   if (k .GT. 1) then
                     if (inTropo(k-1,iCell) .GT. 0) then
                       inTropo(k,iCell) = 1
                       !candInTropo(k,iCell) = 0
                       nChanged = nChanged+1
                       cycle
                     end if
                   end if
 
                   !side nbrs
                   do iNbr = 1, nEdgesOnCell(iCell)
                     iCellNbr = cellsOnCell(iNbr,iCell)
                     if (inTropo(k,iCellNbr) .GT. 0) then
                       inTropo(k,iCell) = 1
                       !candInTropo(k,iCell) = 0
                       nChanged = nChanged+1
                       exit
                     end if
                   end do
 
                   !nbr above
                   if (k .LT. nVertLevels) then
                     if (inTropo(k+1,iCell) .GT. 0) then
                       inTropo(k,iCell) = 1
                       !candInTropo(k,iCell) = 0
                       nChanged = nChanged+1
                       cycle
                     end if
                   end if
 
                end if !candIn
             end do !levels
           end do !cells
           global_haloChanged = global_haloChanged+nChanged
         end do !while w/in domain
         !communicate to other domains for edge case where a chunk of a
         !block hasn't gotten to fill
         nChanged = global_haloChanged
         call mpas_dmpar_max_int(dminfo, nChanged, global_haloChanged)
         if (global_haloChanged .GT. 0) then !communicate inTropo everywhere
           call mpas_dmpar_exch_halo_field(inTropo_f)
         end if
         nChanged = global_haloChanged !so each block will iterate again if anything changed
       end do !while haloChanged
       deallocate(candInTropo)
       
       !Fill iLev_DT with the lowest level above the tropopause (If DT
       !above column, iLev>nVertLevels. If DT below column, iLev=0.
       do iCell=1,nCells
         nChanged = 0
         do k=nVertLevels,1,-1
           if (inTropo(k,iCell) .GT. 0) then
             nChanged = 1
             exit
           end if
         end do !k
         
         if (nChanged .GT. 0) then !found troposphere's highest level
           iLev_DT(iCell) = k+1 !level above troposphere (>nVertLevels if whole column below 2pvu; e.g., tropics)
         else !whole column above DT (e.g., arctic pv tower)
           iLev_DT(iCell) = 0
         end if
       end do !iCell
      
    end subroutine floodFill_tropo

    !*********************************************************************************************************************
    ! NS: Subroutine to compute various fields on 2-PVU surface using the calculated PV field - potential temperature, 
    !     uZonal, uMeridional, vertical vorticity
    ! MC: Modified interpolation of vorticity to cell centers procedure to be consistent with other changes  
    !     This routine should use PV field at end of time step! 
    !********************************************************************************************************************* 
  
    subroutine interp_pv_diagnostics(mesh, diag, pvuVal, missingVal)
 
       use mpas_pool_routines, only : mpas_pool_get_dimension, mpas_pool_get_array
       
       IMPLICIT NONE
       
       type (mpas_pool_type), intent(in) :: mesh
       type (mpas_pool_type), intent(inout) :: diag
       real(kind=RKIND) :: pvuVal, missingVal
       
       integer :: iCell, k
       integer, pointer :: nCells, nVertLevels
       integer, dimension(:), pointer :: nEdgesOnCell, iLev_DT
       integer, dimension(:,:), pointer :: cellsOnCell, cellsOnEdge, verticesOnCell, cellsOnVertex 
                                           
       real(kind=RKIND), dimension(:), pointer:: areaCell, latCell, u_pv, v_pv, theta_pv, vort_pv
       real(kind=RKIND), dimension(:,:), pointer:: uReconstructZonal, uReconstructMeridional, vorticity, theta, ertel_pv, &
                                                   kiteAreasOnVertex
       real(kind=RKIND), dimension(:,:), allocatable :: vVort
       
       call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)
       call mpas_pool_get_dimension(mesh, 'nCells', nCells)
       call mpas_pool_get_array(mesh, 'nEdgesOnCell', nEdgesOnCell)
       call mpas_pool_get_array(mesh, 'cellsOnCell', cellsOnCell)
       call mpas_pool_get_array(mesh, 'cellsOnEdge', cellsOnEdge)
       call mpas_pool_get_array(mesh, 'verticesOnCell', verticesOnCell)
       call mpas_pool_get_array(mesh, 'kiteAreasOnVertex', kiteAreasOnVertex)
       call mpas_pool_get_array(mesh, 'cellsOnVertex', cellsOnVertex)
       call mpas_pool_get_array(mesh, 'areaCell', areaCell)
       call mpas_pool_get_array(mesh, 'latCell', latCell)
       
       call mpas_pool_get_array(diag, 'ertel_pv', ertel_pv)
       call mpas_pool_get_array(diag, 'theta', theta)
       call mpas_pool_get_array(diag, 'vorticity', vorticity)
       call mpas_pool_get_array(diag, 'uReconstructZonal', uReconstructZonal)
       call mpas_pool_get_array(diag, 'uReconstructMeridional', uReconstructMeridional)
       call mpas_pool_get_array(diag, 'u_pv', u_pv)
       call mpas_pool_get_array(diag, 'v_pv', v_pv)
       call mpas_pool_get_array(diag, 'theta_pv', theta_pv)
       call mpas_pool_get_array(diag, 'vort_pv', vort_pv)
       call mpas_pool_get_array(diag, 'iLev_DT', iLev_DT)

       allocate(vVort(nVertLevels,nCells))                         ! MC: Nick's code and other parts of model allocate variables 
                                                                   ! with dims of nCells+1... not sure why?
      
       call interp_pv(nCells, nVertLevels, pvuVal, latCell, &
                      ertel_pv, uReconstructZonal, u_pv, missingVal, iLev_DT)

       call interp_pv(nCells, nVertLevels, pvuVal, latCell, &
                      ertel_pv, uReconstructMeridional, v_pv, missingVal, iLev_DT)

       call interp_pv(nCells, nVertLevels, pvuVal, latCell, &
                      ertel_pv, theta, theta_pv, missingVal, iLev_DT)

       call interp_absVertVort(vorticity, nCells, nEdgesOnCell, verticesOnCell, &
                               cellsOnVertex, areaCell, kiteAreasOnVertex, vVort)
               
       call interp_pv(nCells, nVertLevels, pvuVal, latCell, ertel_pv, vVort, &
                      vort_pv, missingVal, iLev_DT)

       deallocate(vVort)

    end subroutine interp_pv_diagnostics     

    !*********************************************************************************************************************
    ! NS: Subroutine to compute various tendency fields on 2-PVU surface using the calculated PV field
    ! MC: This routine should use PV field and dynamic tropopause from beginning of time step, so this has been modified 
    !     accordingly. 
    !********************************************************************************************************************* 
 
    subroutine interp_pvBudget_diagnostics(mesh, diag, pvuVal, missingVal)
 
       use mpas_pool_routines, only : mpas_pool_get_dimension, mpas_pool_get_array
       
       IMPLICIT NONE
      
       type (mpas_pool_type), intent(in) :: mesh
       type (mpas_pool_type), intent(inout) :: diag
       real(kind=RKIND) :: pvuVal, missingVal
       
       integer :: iCell, k
       integer, pointer :: nCells, nVertLevels
       integer, dimension(:), pointer :: iLev_DT_prev
                                           
       real(kind=RKIND), dimension(:),pointer :: latCell, depv_dt_diab_pv, depv_dt_fric_pv
       real(kind=RKIND), dimension(:,:),pointer :: depv_dt_diab, depv_dt_fric, ertel_pv_prev      ! MC changed
      
       call mpas_pool_get_dimension(mesh, 'nCells', nCells) 
       call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)
       
       call mpas_pool_get_array(mesh, 'latCell', latCell)
       call mpas_pool_get_array(diag, 'ertel_pv_prev', ertel_pv_prev)                             ! MC changed
       call mpas_pool_get_array(diag, 'depv_dt_diab', depv_dt_diab)
       call mpas_pool_get_array(diag, 'depv_dt_fric', depv_dt_fric)
       call mpas_pool_get_array(diag, 'depv_dt_diab_pv', depv_dt_diab_pv)
       call mpas_pool_get_array(diag, 'depv_dt_fric_pv', depv_dt_fric_pv)
       call mpas_pool_get_array(diag, 'iLev_DT_prev', iLev_DT_prev)                               ! MC changed
       
       call interp_pv(nCells, nVertLevels, pvuVal, latCell, &
                      ertel_pv_prev, depv_dt_diab, depv_dt_diab_pv, missingVal, iLev_DT_prev)

       call interp_pv(nCells, nVertLevels, pvuVal, latCell, &
                      ertel_pv_prev, depv_dt_fric, depv_dt_fric_pv, missingVal, iLev_DT_prev)

    end subroutine interp_pvBudget_diagnostics
   
    !*********************************************************************************************************************
    ! NS: Subroutine to linearly interpolate columns of field1 to where field0 is interpVal*sign(lat) using level above 
    !     tropopause already diagnosed 
    !********************************************************************************************************************* 

    subroutine interp_pv(nCells, nLevels, interpVal, latCell, field0, &
                           field1, field, missingVal, iLev_DT)
        
       IMPLICIT NONE  
 
       integer :: nCells, nLevels
       integer, intent(in) :: iLev_DT(nCells)
       real(kind=RKIND) :: interpVal, missingVal
       real(kind=RKIND), intent(in) :: latCell(nCells)
       real(kind=RKIND), intent(in) :: field0(nLevels,nCells), field1(nLevels,nCells)
       real(kind=RKIND), intent(out) :: field(nCells)
 
       !  local variables
       integer :: iCell, iLev, levInd, indlNbr
       real(kind=RKIND) :: valh, vall, vallNbr, sgnh, sgnl, sgnlNbr
       real(kind=RKIND) :: dv_dl, levFrac, valInterpCell, sgnHemi
 
       do iCell = 1,nCells
         !starting from top, trap val if values on opposite side
         levInd = -1 ! what should happen with missing values?
         levFrac = 0.0
         sgnHemi = sign(1.0_RKIND, latCell(iCell)) !problem at the equator...is sign(0)=0?
         if (sgnHemi .EQ. 0.0) sgnHemi = 1.0
         valInterpCell = interpVal*sgnHemi
         
         iLev = iLev_DT(iCell)
         if (iLev .GT. nLevels) then
           levInd = -1
           sgnl = -1.0
         else if (iLev .LT. 1) then
           levInd = -1
           sgnl = 1.0
         else
           valh = field0(iLev,iCell)
           vall = field0(iLev-1,iCell)
           !sandwiched value. equal in case val0 is a vals[l].
           !get linear interpolation: val0 = vals[l]+dvals/dl * dl
           !Avoid divide by 0 by just assuming value is 
           !halfway between...
    
           dv_dl = valh-vall;
           if (abs(dv_dl)<1.e-6) then
             levFrac = 0.5;
           else
             levFrac = (valInterpCell-vall)/dv_dl
           end if
           
           levInd = iLev-1
         end if !iLev in column
 
         !find value of field using index we just found
         if (levInd<0) then !didn't trap value
           if (sgnl>0.0) then !column above value, take surface
             field(iCell) = field1(1,iCell)
           else !column below value, take top
             !field(iCell) = missingVal
             field(iCell) = field1(nLevels,iCell)
           end if
         else
           valh = field1(levInd+1,iCell)
           vall = field1(levInd,iCell)
         
           dv_dl = valh-vall
           field(iCell) = vall+dv_dl*levFrac
         end if
       end do
       
    end subroutine interp_pv
   
    !*********************************************************************************************************************
    ! MC: Subroutine to calculate the dot product between two 3D vectors 
    !*********************************************************************************************************************

    subroutine calc_dotProduct_3D(vec1, vec2, nCells, nVertLevels, dotResult)

       IMPLICIT NONE

       integer, intent(in) :: nVertLevels, nCells
       real(kind=RKIND), dimension(:,:,:), intent(in) :: vec1, vec2
       real(kind=RKIND), dimension(:,:),  intent(out) :: dotResult
       integer :: iCell, k

       dotResult(:,:) = 0.0_RKIND

       do iCell=1,nCells
          do k=1,nVertLevels
             dotResult(k,iCell) = vec1(k,iCell,1)*vec2(k,iCell,1) + vec1(k,iCell,2)*vec2(k,iCell,2) + vec1(k,iCell,3)*vec2(k,iCell,3)
          end do
       end do

    end subroutine calc_dotProduct_3D

    !*********************************************************************************************************************
    ! MC: Subroutine to interpolate the absolute vertical vorticity to cell centers from the absolute 
    !     vertical vorticity at the vertices (pv_vertex) 
    !     Note: pv_vertex variable was defined incorrectly in MPAS registry -- there is no density dependence
    !********************************************************************************************************************* 

    subroutine interp_absVertVort(pv_vertex, nCells, nEdgesOnCell, verticesOnCell, &
                                cellsOnVertex, areaCell, kiteAreasOnVertex, absVort)
 
       IMPLICIT NONE
 
       integer, intent(in) :: nCells
       integer, dimension(:), intent(in) :: nEdgesOnCell
       integer, dimension(:,:), intent(in) :: verticesOnCell, cellsOnVertex
       real(kind=RKIND), dimension(:), intent(in) :: areaCell
       real(kind=RKIND), dimension(:,:), intent(in) :: pv_vertex, kiteAreasOnVertex
       real(kind=RKIND), dimension(:,:), intent(out) :: absVort
       integer :: i, j, cellIndOnVertex, iVertex
 
       absVort(:,:) = 0.0_RKIND
 
       do i=1,nCells
          do j=1,nEdgesOnCell(i)
             iVertex = verticesOnCell(j,i)
             cellIndOnVertex = FINDLOC(cellsOnVertex(:,iVertex),VALUE=i,DIM=1)
             absVort(:,i) = absVort(:,i) + kiteAreasOnVertex(cellIndOnVertex,iVertex) * pv_vertex(:,iVertex)
          end do
          absVort(:,i) = absVort(:,i) / areaCell(i)
       end do
 
    end subroutine interp_absVertVort

    !*********************************************************************************************************************    
    ! MC: Subroutine to calculate the horizontal gradient of a field on the cell edges using field values at the 
    !     adjacent cell centers as:
    !
    ! varGrad(edgeUse,kLev) = (cellVar(cellsOnEdge(edgeUse,2),kLev)-cellVar(cellsOnEdge(edgeUse,1),kLev))/dcEdge(edgeUse)
    !
    !     and then assign the correct sign based on its direction (i.e., into or out of the cell), the value of 
    !     edgesOnCell_sign, and the convention for the u winds: "Positive u (normal) velocity is always defined as 
    !     flow from cellsOnEdge(1,jEdge) to cellsOnEdge(2,jEdge) for edge iEdge" (MPAS tutorial 2019).
    !
    !     The expression for calculating the gradient on each edge comes from Eq. 22 in Ringler et al. (2010)
    !*********************************************************************************************************************    
 
    subroutine calc_gradOnEdges(cellVar, nCells, nEdges, nVertLevels, nEdgesOnCell, &
                                edgesOnCell, edgesOnCell_sign, cellsOnEdge, dcEdge, varGrad)
 
       IMPLICIT NONE
 
       integer, intent(in) :: nCells, nEdges, nVertLevels
       integer, dimension(:,:), intent(in) :: cellsOnEdge, edgesOnCell
       integer, dimension(:), intent(in) :: nEdgesOnCell
       real(kind=RKIND), dimension(:), intent(in) :: dcEdge
       real(kind=RKIND), dimension(:,:), intent(in) :: cellVar, edgesOnCell_sign
       real(kind=RKIND), dimension(:,:), intent(out) :: varGrad
       integer :: iCell, jEdge, kLev, edgeSign, edgeUse, index_j1, index_j2, sign_j1, sign_j2
 
       varGrad(:,:) = 0.0_RKIND
 
       cell_loop: do iCell=1,nCells
         edge_loop: do jEdge=1,nEdgesOnCell(iCell)
            lev_loop: do kLev=1,nVertLevels
 
               ! Edges and edge signs for jEdge along parent iCell
               edgeSign = edgesOnCell_sign(jEdge,iCell)
               edgeUse = edgesOnCell(jEdge,iCell) 
 
               ! The indices of edgeUse likely differ in edgesOnCell array for each
               ! cell. Need to find the correct indices and the sign of the normal
               ! vector for each edge in edgesOnCell
               !
               ! -- if sign_j1 > 0, normal vector points out of cellsOnEdge(edgeUse,1) 
               ! -- if sign_j2 > 0, normal vector points out of cellsOnEdge(edgeUse,2) 
 
               index_j1 = FINDLOC(edgesOnCell(:,cellsOnEdge(1,edgeUse)),VALUE=edgeUse, DIM=1)
               index_j2 = FINDLOC(edgesOnCell(:,cellsOnEdge(2,edgeUse)),VALUE=edgeUse, DIM=1)
               sign_j1 = edgesOnCell_sign(index_j1,cellsOnEdge(1,edgeUse))
               sign_j2 = edgesOnCell_sign(index_j2,cellsOnEdge(2,edgeUse))
 
               ! Calculate gradient of field by taking the difference of the values
               ! at the adjacent cell centers divided by the distance between the
               ! cells
 
               varGrad(kLev,edgeUse) = cellVar(kLev,cellsOnEdge(2,edgeUse)) - cellVar(kLev,cellsOnEdge(1,edgeUse))
               varGrad(kLev,edgeUse) = varGrad(kLev,edgeUse)/dcEdge(edgeUse)
 
               ! Ensure that the sign of the gradient is consistent with the
               ! convention for the u (normal winds). Note: I think the signs are
               ! correct without doing this procedure, but I will keep it here just
               ! in case. 
     
               IF (varGrad(kLev,edgeUse) .gt. 0) THEN
                  ! Gradient vector points toward cellsOnEdge(edgeUse,2) -> 
                  ! should be directed inward for cellsOnEdge(edgeUse,2)
 
                  ! What is sign of normal vector along edgeUse for each cell? 
                  IF (sign_j2 .lt. 0) THEN       ! Normal vector points inward for cellsOnEdge(edgeUse,2) 
                                                 ! and outward for cellsOnEdge(edgeUse,1)
                      varGrad(kLev,edgeUse) = ABS(varGrad(kLev,edgeUse))
                  ELSE
                      varGrad(kLev,edgeUse) = -ABS(varGrad(kLev,edgeUse))
                  END IF
 
               ELSE IF (varGrad(kLev,edgeUse) .lt. 0) THEN
                  ! Gradient vector points toward cellsOnEdge(edgeUse,1) -> 
                  ! should be directed inward for cellsOnEdge(edgeUse,1)
 
                  ! What is sign of normal vector along edgeUse for each cell? 
                  IF (sign_j1 .lt. 0) THEN      ! Normal vector points inward for cellsOnEdge(edgeUse,1) 
                                                ! and outward for cellsOnEdge(edgeUse,2)
                      varGrad(kLev,edgeUse) = ABS(varGrad(kLev,edgeUse))
                  ELSE
                      varGrad(kLev,edgeUse) = -ABS(varGrad(kLev,edgeUse))
                  END IF
 
               END IF
 
            end do lev_loop
          end do edge_loop
       end do cell_loop
 
    end subroutine calc_gradOnEdges

    !*********************************************************************************************************************
    ! MC: Subroutine takes gradient field valid on cell edges and reconstructs the horizontal gradient vectors at the cell 
    !     center in a manner analogous to the u reconstruction of mpas_reconstruct_2d in mpas_vector_reconstruction.F
    !*********************************************************************************************************************
 
    subroutine mpas_reconstruct_grad(gradEdge, latCell, lonCell, coeffs_reconstruct, nCells, nVertLevels, &
                                     edgesOnCell, nEdgesOnCell, &
                                     gradReconstructZonal, gradReconstructMeridional)
 
       IMPLICIT NONE
 
       integer, intent(in) :: nVertLevels, nCells
       integer, dimension(:,:), intent(in) :: edgesOnCell
       integer, dimension(:), intent(in) :: nEdgesOnCell
 
       real(kind=RKIND), dimension(:),   intent(in)   :: latCell, lonCell
       real(kind=RKIND), dimension(:,:), intent(in)   :: gradEdge
       real(kind=RKIND), dimension(:,:,:), intent(in) :: coeffs_reconstruct
       real(kind=RKIND), dimension(:,:), intent(out)  :: gradReconstructZonal, gradReconstructMeridional
 
       ! local variables
       integer :: iCell, jEdge, edgeUse, kLev
       real(kind=RKIND) :: clat, slat, clon, slon
       real(kind=RKIND), dimension(:,:), allocatable :: gradReconstructX, gradReconstructY, gradReconstructZ
 
       allocate(gradReconstructX(nVertLevels,nCells))
       allocate(gradReconstructY(nVertLevels,nCells))
       allocate(gradReconstructZ(nVertLevels,nCells))
 
       gradReconstructX(nVertLevels,nCells) = 0.0_RKIND
       gradReconstructY(nVertLevels,nCells) = 0.0_RKIND
       gradReconstructZ(nVertLevels,nCells) = 0.0_RKIND
       gradReconstructZonal(nVertLevels,nCells) = 0.0_RKIND
       gradReconstructMeridional(nVertLevels,nCells) = 0.0_RKIND
 
       cell_loop: do iCell=1,nCells
          edge_loop: do jEdge=1,nEdgesOnCell(iCell)
 
             edgeUse = edgesOnCell(jEdge,iCell)
 
             gradReconstructX(:,iCell) = gradReconstructX(:,iCell) &
               + coeffs_reconstruct(1,jEdge,iCell) * gradEdge(:,edgeUse)
             gradReconstructY(:,iCell) = gradReconstructY(:,iCell) &
               + coeffs_reconstruct(2,jEdge,iCell) * gradEdge(:,edgeUse)
             gradReconstructZ(:,iCell) = gradReconstructZ(:,iCell) &
               + coeffs_reconstruct(3,jEdge,iCell) * gradEdge(:,edgeUse)
 
           end do edge_loop
 
           clat = COS(latCell(iCell))
           slat = SIN(latCell(iCell))
           clon = COS(lonCell(iCell))
           slon = SIN(lonCell(iCell))
 
           gradReconstructZonal(:,iCell) = -gradReconstructX(:,iCell)*slon + &
                                           gradReconstructY(:,iCell)*clon
 
 
           gradReconstructMeridional(:,iCell) = -(gradReconstructX(:,iCell)*clon + &
                                               gradReconstructY(:,iCell)*slon)*slat + &
                                               gradReconstructZ(:,iCell)*clat
 
       end do cell_loop
 
       deallocate(gradReconstructX)
       deallocate(gradReconstructY)
       deallocate(gradReconstructZ)
 
    end subroutine mpas_reconstruct_grad

    !*********************************************************************************************************************
    ! MC: Combined subroutines to calculate the horizontal gradient of a field on the cell edges using values at 
    !     the adjacent cell centers 
    !
    ! varGrad(edgeUse,kLev) = (cellVar(cellsOnEdge(edgeUse,2),kLev) - cellVar(cellsOnEdge(edgeUse,1),kLev)) / dcEdge(edgeUse)
    !
    !     and then assign the correct sign based on its direction (i.e., into or out of the cell), the value of 
    !     edgesOnCell_sign, and the convention for the u winds: "Positive u (normal) velocity is always defined as 
    !     flow from cellsOnEdge(1,jEdge) to cellsOnEdge(2,jEdge) for edge iEdge" (MPAS tutorial 2019).
    !
    !     The expression for calculating the gradient on each edge comes from Eq. 22 in Ringler et al. (2010)
    !
    !     Following the gradient on edge calculation, the gradient is then reconstructed to the cell centers in a 
    !     manner analogous to the u reconstruction of mpas_reconstruct_2d in mpas_vector_reconstruction.F
    !
    !     The purpose of combining these into one subroutine is to reduce the number of stored intermediate variables, 
    !     which are needed for the halo communication to work properly
    !*********************************************************************************************************************
 
    subroutine calc_gradOnEdges_reconCellCenter(cellVar, nCells, nEdges, nVertLevels, nEdgesOnCell, &
                                edgesOnCell, edgesOnCell_sign, cellsOnEdge, dcEdge, coeffs_reconstruct, & 
                                latCell, lonCell, gradReconstructZonal, gradReconstructMeridional)

       IMPLICIT NONE

       integer, intent(in) :: nCells, nEdges, nVertLevels
       integer, dimension(:), intent(in) :: nEdgesOnCell
       integer, dimension(:,:), intent(in) :: cellsOnEdge, edgesOnCell
 
       real(kind=RKIND), dimension(:), intent(in) :: dcEdge, latCell, lonCell
       real(kind=RKIND), dimension(:,:), intent(in) :: cellVar, edgesOnCell_sign
       real(kind=RKIND), dimension(:,:,:), intent(in) :: coeffs_reconstruct
       real(kind=RKIND), dimension(:,:), intent(out)  :: gradReconstructZonal, gradReconstructMeridional

       ! local variables
       real(kind=RKIND) :: clat, slat, clon, slon
       real(kind=RKIND), dimension(:,:), allocatable :: varGrad, gradReconstructX, gradReconstructY, gradReconstructZ
       integer :: iCell, jEdge, kLev, edgeSign, edgeUse, index_j1, index_j2, sign_j1, sign_j2
 
       allocate(varGrad(nVertLevels,nEdges))
       allocate(gradReconstructX(nVertLevels,nCells))
       allocate(gradReconstructY(nVertLevels,nCells))
       allocate(gradReconstructZ(nVertLevels,nCells))
 
       varGrad(:,:) = 0.0_RKIND
       gradReconstructX(:,:) = 0.0_RKIND
       gradReconstructY(:,:) = 0.0_RKIND
       gradReconstructZ(:,:) = 0.0_RKIND
       gradReconstructZonal(:,:) = 0.0_RKIND
       gradReconstructMeridional(:,:) = 0.0_RKIND
 
       cell_loop: do iCell=1,nCells
         edge_loop: do jEdge=1,nEdgesOnCell(iCell)
            lev_loop: do kLev=1,nVertLevels
 
               ! Edges and edge signs for jEdge along parent iCell
               edgeSign = edgesOnCell_sign(jEdge,iCell)
               edgeUse = edgesOnCell(jEdge,iCell) 
 
               ! The indices of edgeUse likely differ in edgesOnCell array for each
               ! cell. Need to find the correct indices and the sign of the normal
               ! vector for each edge in edgesOnCell
               !
               ! -- if sign_j1 > 0, normal vector points out of cellsOnEdge(edgeUse,1) 
               ! -- if sign_j2 > 0, normal vector points out of cellsOnEdge(edgeUse,2) 
 
               index_j1 = FINDLOC(edgesOnCell(:,cellsOnEdge(1,edgeUse)),VALUE=edgeUse, DIM=1)
               index_j2 = FINDLOC(edgesOnCell(:,cellsOnEdge(2,edgeUse)),VALUE=edgeUse, DIM=1)
               sign_j1 = edgesOnCell_sign(index_j1,cellsOnEdge(1,edgeUse))
               sign_j2 = edgesOnCell_sign(index_j2,cellsOnEdge(2,edgeUse))
 
               ! Calculate gradient of field by taking the difference of the values
               ! at the adjacent cell centers divided by the distance between the
               ! cells
 
               varGrad(kLev,edgeUse) = cellVar(kLev,cellsOnEdge(2,edgeUse)) - cellVar(kLev,cellsOnEdge(1,edgeUse))
               varGrad(kLev,edgeUse) = varGrad(kLev,edgeUse)/dcEdge(edgeUse)
 
               ! Ensure that the sign of the gradient is consistent with the
               ! convention for the u (normal winds). Note: I think the signs are
               ! correct without doing this procedure, but I will keep it here just
               ! in case. 
 
               IF (varGrad(kLev,edgeUse) .gt. 0) THEN
                  ! Gradient vector points toward cellsOnEdge(edgeUse,2) -> 
                  ! should be directed inward for cellsOnEdge(edgeUse,2)
 
                  ! What is sign of normal vector along edgeUse for each cell? 
                  IF (sign_j2 .lt. 0) THEN       ! Normal vector points inward for cellsOnEdge(edgeUse,2) 
                                                 ! and outward for cellsOnEdge(edgeUse,1)
                      varGrad(kLev,edgeUse) = ABS(varGrad(kLev,edgeUse))
                  ELSE
                      varGrad(kLev,edgeUse) = -ABS(varGrad(kLev,edgeUse))
                  END IF
 
               ELSE IF (varGrad(kLev,edgeUse) .lt. 0) THEN
                  ! Gradient vector points toward cellsOnEdge(edgeUse,1) -> 
                  ! should be directed inward for cellsOnEdge(edgeUse,1)
 
                  ! What is sign of normal vector along edgeUse for each cell? 
                  IF (sign_j1 .lt. 0) THEN      ! Normal vector points inward for cellsOnEdge(edgeUse,1) 
                                                ! and outward for cellsOnEdge(edgeUse,2)
                      varGrad(kLev,edgeUse) = ABS(varGrad(kLev,edgeUse))
                  ELSE
                      varGrad(kLev,edgeUse) = -ABS(varGrad(kLev,edgeUse))
                  END IF
 
               END IF
 
            end do lev_loop
 
            gradReconstructX(:,iCell) = gradReconstructX(:,iCell) &
              + coeffs_reconstruct(1,jEdge,iCell) * varGrad(:,edgeUse)
            gradReconstructY(:,iCell) = gradReconstructY(:,iCell) &
              + coeffs_reconstruct(2,jEdge,iCell) * varGrad(:,edgeUse)
            gradReconstructZ(:,iCell) = gradReconstructZ(:,iCell) &
              + coeffs_reconstruct(3,jEdge,iCell) * varGrad(:,edgeUse)
 
          end do edge_loop
 
          clat = COS(latCell(iCell))
          slat = SIN(latCell(iCell))
          clon = COS(lonCell(iCell))
          slon = SIN(lonCell(iCell))
 
          gradReconstructZonal(:,iCell) = -gradReconstructX(:,iCell)*slon + &
                                           gradReconstructY(:,iCell)*clon
 
 
          gradReconstructMeridional(:,iCell) = -(gradReconstructX(:,iCell)*clon + &
                                                gradReconstructY(:,iCell)*slon)*slat + &
                                                gradReconstructZ(:,iCell)*clat
 
       end do cell_loop
 
       deallocate(gradReconstructX)
       deallocate(gradReconstructY)
       deallocate(gradReconstructZ)
       deallocate(varGrad)
 
    end subroutine calc_gradOnEdges_reconCellCenter

   !*********************************************************************************************************************
   ! MC: Modified original curl subroutine to include calculation over all vertical levels
   !
   ! NS: Adapted from computation of circulation and relative vorticity at each vertex in atm_compute_solve_diagnostics()
   !     This takes scvt face values and computes finite volume curl at scvt vertices (triangle cell centers)
   !*********************************************************************************************************************

   subroutine calc_vertical_curl(uEdge, nEdges, nVertices, dcEdge, areaTriangle, verticesOnEdge, curlVert)

      implicit none

      integer, intent(in) :: nEdges, nVertices
      integer, dimension(:,:), intent(in) :: verticesOnEdge
      real (kind=RKIND), dimension(:), intent(in) :: dcEdge, areaTriangle
      real (kind=RKIND), dimension(:,:), intent(in) :: uEdge
      real (kind=RKIND), dimension(:,:), intent(out) :: curlVert

      integer :: jEdge, iVert
     
      curlVert(:,:) = 0.0_RKIND

      do jEdge=1,nEdges
          curlVert(:,verticesOnEdge(1,jEdge)) = curlVert(:,verticesOnEdge(1,jEdge)) - dcEdge(jEdge) * uEdge(:,jEdge)
          curlVert(:,verticesOnEdge(2,jEdge)) = curlVert(:,verticesOnEdge(2,jEdge)) + dcEdge(jEdge) * uEdge(:,jEdge)
      end do

      do iVert=1,nVertices
          curlVert(:,iVert) = curlVert(:,iVert) / areaTriangle(iVert)
      end do

   end subroutine calc_vertical_curl

   !*********************************************************************************************************************
   ! MC: Subroutine combining NS's original functions for calculating vertical derivatives, which finds values at adjacent 
   !     theta/mass levels and then calculates one-sided difference between center level and the levels above and below. 
   !     For all levels except k=1 and k=nVertLevels, these differences are then averaged to give the center difference at 
   !     the center level. Else, the one-sided differences are used. 
   !*********************************************************************************************************************

   subroutine calc_vertDeriv(var, nCells, nVertLevels, dzu, dvar_dz)

      IMPLICIT NONE

      integer, intent(in) :: nCells, nVertLevels
      real(kind=RKIND), dimension(:), intent(in) ::  dzu
      real(kind=RKIND), dimension(:,:), intent(in) :: var
      real(kind=RKIND), dimension(:,:), intent(out) :: dvar_dz
      integer :: iCell, k
      real(kind=RKIND) :: dvar_dz_top, dvar_dz_bot

      dvar_dz(:,:) = 0.0_RKIND

      do iCell=1,nCells
         dvar_dz(1,iCell) = (var(2,iCell) - var(1,iCell)) / dzu(2)
         dvar_dz(nVertLevels,iCell) = (var(nVertLevels,iCell) - var(nVertLevels-1,iCell)) / &
                                       dzu(nVertLevels)
         do k=2,nVertLevels-1
            dvar_dz_top = (var(k+1,iCell) - var(k,iCell)) / dzu(k+1)
            dvar_dz_bot = (var(k,iCell) - var(k-1,iCell)) / dzu(k)
            dvar_dz(k,iCell) = 0.5 * (dvar_dz_top + dvar_dz_bot)
         end do
      end do
      
   end subroutine calc_vertDeriv

   !*********************************************************************************************************************   
   ! MC: Subroutine to interpolate variable from w levels (vertical cell faces) to theta levels (cell centers)
   !*********************************************************************************************************************

   subroutine interp_wLev_thetaLev(w, nCells, nVertLevels, wCell)

      IMPLICIT NONE

      integer, intent(in) :: nCells, nVertLevels
      real(kind=RKIND), dimension(:,:), intent(in) :: w
      real(kind=RKIND), dimension(:,:), intent(out) :: wCell
      integer :: iCell, k

      do iCell=1,nCells
        do k=1,nVertLevels
           wCell(k,iCell) = 0.5*(w(k+1,iCell) + w(k,iCell))
        end do
      end do

   end subroutine interp_wLev_thetaLev

   !*********************************************************************************************************************   
   ! MC: Subroutine to store variables from the beginning of the time step to use in next timestep tendency calculations 
   !*********************************************************************************************************************

   subroutine store_previous_vars(mesh, time_lev, state, diag)

      use mpas_pool_routines, only : mpas_pool_get_dimension, mpas_pool_get_array
      use mpas_log, only : mpas_log_write
 
      IMPLICIT NONE
   
      type (mpas_pool_type), intent(in) :: state
      integer, intent(in) :: time_lev                                ! which time level to use from state: 1 = beginning of time step; 2 = end of time step
      type (mpas_pool_type), intent(inout) :: diag
      type (mpas_pool_type), intent(in) :: mesh

      integer, pointer :: nCells, nVertLevels, nEdges, nVertices

      real(kind=RKIND), dimension(:,:), pointer :: uReconstructZonal, uReconstructMeridional, wCell, theta, rho, &
                                                   pv_vertex, ertel_pv, rho_edge
      real(kind=RKIND), dimension(:,:), pointer :: uReconstructZonal_prev, uReconstructMeridional_prev, wCell_prev, &
                                                   theta_prev, rho_prev, pv_vertex_prev, ertel_pv_prev, &
                                                   rho_edge_prev

      integer, dimension(:), pointer :: iLev_DT, iLev_DT_prev

      call mpas_pool_get_dimension(mesh, 'nCells', nCells)
      call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)
      call mpas_pool_get_dimension(mesh, 'nEdges', nEdges)
      call mpas_pool_get_dimension(mesh, 'nVertices', nVertices)

      call mpas_pool_get_array(diag, 'uReconstructZonal', uReconstructZonal)
      call mpas_pool_get_array(diag, 'uReconstructMeridional', uReconstructMeridional)
      call mpas_pool_get_array(diag, 'wCell', wCell)
      call mpas_pool_get_array(diag, 'theta', theta)
      call mpas_pool_get_array(diag, 'rho', rho)
      call mpas_pool_get_array(diag, 'pv_vertex', pv_vertex)
      call mpas_pool_get_array(diag, 'ertel_pv', ertel_pv)
      call mpas_pool_get_array(diag, 'iLev_DT', iLev_DT)
      call mpas_pool_get_array(diag, 'rho_edge', rho_edge)

      call mpas_pool_get_array(diag, 'uReconstructZonal_prev', uReconstructZonal_prev, time_lev)
      call mpas_pool_get_array(diag, 'uReconstructMeridional_prev', uReconstructMeridional_prev, time_lev)
      call mpas_pool_get_array(diag, 'wCell_prev', wCell_prev, time_lev)
      call mpas_pool_get_array(diag, 'theta_prev', theta_prev, time_lev)
      call mpas_pool_get_array(diag, 'rho_prev', rho_prev, time_lev)      
      call mpas_pool_get_array(diag, 'pv_vertex_prev', pv_vertex_prev, time_lev)
      call mpas_pool_get_array(diag, 'ertel_pv_prev', ertel_pv_prev, time_lev)
      call mpas_pool_get_array(diag, 'iLev_DT_prev', iLev_DT_prev, time_lev)
      call mpas_pool_get_array(diag, 'rho_edge_prev', rho_edge_prev, time_lev)

      !call mpas_log_write("maxval ertel_pv_prev before resetting is $r", realArgs=(/real(MAXVAL(ertel_pv_prev), kind=RKIND)/))

      uReconstructZonal_prev(:,:) = uReconstructZonal(:,:)
      uReconstructMeridional_prev(:,:) = uReconstructMeridional(:,:)
      wCell_prev(:,:) = wCell(:,:)
      theta_prev(:,:) = theta(:,:)
      rho_prev(:,:) = rho(:,:)
      pv_vertex_prev(:,:) = pv_vertex(:,:)
      ertel_pv_prev(:,:) = ertel_pv(:,:)
      iLev_DT_prev(:) = iLev_DT(:)
      rho_edge_prev(:,:) = rho_edge(:,:)

      !call mpas_log_write("maxval ertel_pv_prev after resetting is $r", realArgs=(/real(MAXVAL(ertel_pv_prev), kind=RKIND)/))

   end subroutine store_previous_vars

   !*********************************************************************************************************************
   ! MC: Modified subroutine to calculate Ertel's potential vorticity
   !     PV = 1/density * [curl(wind) . grad(theta)] 
   !*********************************************************************************************************************

   subroutine calc_epv(mesh, time_lev, state, diag)

      use mpas_pool_routines, only : mpas_pool_get_dimension, mpas_pool_get_array

      IMPLICIT NONE

      type (mpas_pool_type), intent(in) :: state
      integer, intent(in) :: time_lev                     ! which time level to use from state: 1 = beginning of time step; 2 = end of time step
      type (mpas_pool_type), intent(inout) :: diag        ! MC: since these subroutines are called after state/diag vars have been updated, time_lev 
      type (mpas_pool_type), intent(in) :: mesh           !     specification here doesn't really matter

      ! input vars
      integer, pointer :: nCells, nVertLevels, nEdges, R3
      integer, dimension(:), pointer :: nEdgesOnCell
      integer, dimension(:,:), pointer :: cellsOnEdge, edgesOnCell, verticesOnCell, &
                                          cellsOnVertex

      real(kind=RKIND), dimension(:), pointer :: dzu, areaCell, latCell, lonCell, dcEdge
      real(kind=RKIND), dimension(:,:), pointer :: w, rho, theta, pv_vertex, zgrid, kiteAreasOnVertex, &
                                                   uReconstructZonal, uReconstructMeridional, ertel_pv, & 
                                                   edgesOnCell_sign, wCell
      real(kind=RKIND), dimension(:,:,:), pointer :: coeffs_reconstruct 

      ! local vars
      real(kind=RKIND), dimension(:,:), allocatable :: duZonal_dz, duMerid_dz
      real(kind=RKIND), dimension(:,:), allocatable :: dTheta_dxZonal, dTheta_dyMerid, dTheta_dz
      real(kind=RKIND), dimension(:,:), allocatable :: dW_dxZonal, dW_dyMerid 
      real(kind=RKIND), dimension(:,:), allocatable :: absVort
      real(kind=RKIND), dimension(:,:,:), allocatable :: absVort3D, gradTheta

      call mpas_pool_get_dimension(mesh, 'nCells', nCells)
      call mpas_pool_get_dimension(mesh, 'R3', R3)
      call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)
      call mpas_pool_get_dimension(mesh, 'nEdges', nEdges)
      call mpas_pool_get_array(mesh, 'nEdgesOnCell', nEdgesOnCell)
      call mpas_pool_get_array(mesh, 'cellsOnEdge', cellsOnEdge)
      call mpas_pool_get_array(mesh, 'edgesOnCell', edgesOnCell)
      call mpas_pool_get_array(mesh, 'verticesOnCell', verticesOnCell)
      call mpas_pool_get_array(mesh, 'kiteAreasOnVertex', kiteAreasOnVertex)
      call mpas_pool_get_array(mesh, 'cellsOnVertex', cellsOnVertex)
      call mpas_pool_get_array(mesh, 'edgesOnCell_sign', edgesOnCell_sign)
      call mpas_pool_get_array(mesh, 'latCell', latCell)
      call mpas_pool_get_array(mesh, 'lonCell', lonCell)
      call mpas_pool_get_array(mesh, 'dcEdge', dcEdge)
      call mpas_pool_get_array(mesh, 'areaCell', areaCell)
      call mpas_pool_get_array(mesh, 'coeffs_reconstruct', coeffs_reconstruct)
      call mpas_pool_get_array(mesh, 'zgrid', zgrid)
      call mpas_pool_get_array(mesh, 'dzu', dzu)
      call mpas_pool_get_array(state, 'w', w, time_lev)     
      call mpas_pool_get_array(diag, 'theta', theta)  
      call mpas_pool_get_array(diag, 'rho', rho)
      call mpas_pool_get_array(diag, 'pv_vertex', pv_vertex)
      call mpas_pool_get_array(diag, 'uReconstructZonal', uReconstructZonal)
      call mpas_pool_get_array(diag, 'uReconstructMeridional', uReconstructMeridional)
      call mpas_pool_get_array(diag, 'ertel_pv', ertel_pv)
      call mpas_pool_get_array(diag, 'wCell', wCell)    

      ! Allocate memory to intermediate vars 
      allocate(absVort(nVertLevels,nCells)) 
      allocate(duZonal_dz(nVertLevels,nCells))
      allocate(duMerid_dz(nVertLevels,nCells))
      allocate(dTheta_dxZonal(nVertLevels,nCells))
      allocate(dTheta_dyMerid(nVertLevels,nCells))
      allocate(dTheta_dz(nVertLevels,nCells))
      allocate(dW_dxZonal(nVertLevels,nCells))
      allocate(dW_dyMerid(nVertLevels,nCells))
      allocate(absVort3D(nVertLevels,nCells,3))
      allocate(gradTheta(nVertLevels,nCells,3))

      ertel_pv(:,:) = 0.0_RKIND
      gradTheta(:,:,:) = 0.0_RKIND
      absVort3D(:,:,:) = 0.0_RKIND

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Calculate the 3D potential temperature gradient 
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ! (1) Calculate and reconstruct horizontal potential temperature gradient to get zonal and meridional
      !     gradients at cell centers: dth_dx, dth_dy

      call calc_gradOnEdges_reconCellCenter(theta, nCells, nEdges, nVertLevels, nEdgesOnCell, &
                                edgesOnCell, edgesOnCell_sign, cellsOnEdge, dcEdge, coeffs_reconstruct, &
                                latCell, lonCell, dTheta_dxZonal, dTheta_dyMerid)


      ! (2) Calculate the vertical potential temperature gradient: dth_dz

      call calc_vertDeriv(theta, nCells, nVertLevels, dzu, dTheta_dz)

      ! (3) Combine theta derivatives into 3D vector

      gradTheta(:,:,1) = dTheta_dxZonal
      gradTheta(:,:,2) = dTheta_dyMerid
      gradTheta(:,:,3) = dTheta_dz

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Calculate the 3D absolute vorticity vector  
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ! (1) Calculate the vertical shear of uReconstructZonal and uReconstructMeridional: du_dz and dv_dz

      call calc_vertDeriv(uReconstructZonal, nCells, nVertLevels, dzu, duZonal_dz)
      call calc_vertDeriv(uReconstructMeridional, nCells, nVertLevels, dzu, duMerid_dz)

      ! (2) Interpolate w to cell centers, calculate gradient of w on edges, and then reconstruct to get 
      !     zonal and meridional gradients at cell centers: dw_dx, dw_dy 

      call interp_wLev_thetaLev(w, nCells, nVertLevels, wCell)

      call calc_gradOnEdges_reconCellCenter(wCell, nCells, nEdges, nVertLevels, nEdgesOnCell, &
                                edgesOnCell, edgesOnCell_sign, cellsOnEdge, dcEdge, coeffs_reconstruct, &
                                latCell, lonCell, dW_dxZonal, dW_dyMerid)

      ! (3) Recconstruct absolute vertical vorticity at vertices pv_vertex to cell centers

      !     Note: currently, pv_vertex is the absolute vertical vorticity on the cell vertices. If this 
      !     variable changes at some point, then the absolute vertical vorticity on the vertices needs 
      !     to be computed as follows: 
      !     do iVert=1,nVertices
      !        vorticity(:,iVert) = vorticity(:,iVert) + fVertex(iVert)
      !     end do

      call interp_absVertVort(pv_vertex, nCells, nEdgesOnCell, verticesOnCell, &
                                  cellsOnVertex, areaCell, kiteAreasOnVertex, absVort)

      ! (4) Combine three components into vorticity vector

      absVort3D(:,:,1) = dW_dyMerid - duMerid_dz                   ! dw/dy - dv/dz
      absVort3D(:,:,2) = duZonal_dz - dW_dxZonal                   ! du/dz - dw/dx
      absVort3D(:,:,3) = absVort                                   ! dv/dy - du/dx + f

      ! (5) Take dot product between 3D theta vector and absolute vorticity / density to compute 
      !     Ertel's PV
  
      call calc_dotProduct_3D(gradTheta, absVort3D, nCells, nVertLevels, ertel_pv)
 
      ertel_pv = ertel_pv / rho * 1.0e6                            !SI to PVUs

      deallocate(absVort)
      deallocate(absVort3D)
      deallocate(gradTheta)
      deallocate(duZonal_dz)
      deallocate(duMerid_dz)
      deallocate(dTheta_dz)
      deallocate(dTheta_dxZonal)
      deallocate(dTheta_dyMerid)
      deallocate(dW_dxZonal)
      deallocate(dW_dyMerid)

   end subroutine calc_epv

   !***********************************************************************************************
   ! MC: Subroutine to calculate Ertel's potential vorticity advection terms in zonal, meridional, 
   !     vertical directions  
   !***********************************************************************************************

   subroutine calc_pvAdvection(state, time_lev, diag, mesh)

      use mpas_pool_routines, only : mpas_pool_get_dimension, mpas_pool_get_array

      implicit none

      type (mpas_pool_type), intent(in) :: state
      integer, intent(in) :: time_lev                        
      type (mpas_pool_type), intent(inout) :: diag
      type (mpas_pool_type), intent(in) :: mesh

      integer, pointer :: nCells, nVertLevels, nEdges
      integer, dimension(:), pointer :: nEdgesOnCell
      integer, dimension(:,:), pointer :: cellsOnEdge, edgesOnCell

      real(kind=RKIND), dimension(:),   pointer :: dzu, latCell, lonCell, dcEdge

      real(kind=RKIND), dimension(:,:), pointer :: uReconstructZonal, uReconstructMeridional, edgesOnCell_sign, &
                                                   wCell, ertel_pv
      real(kind=RKIND), dimension(:,:,:), pointer :: coeffs_reconstruct

      ! tendency variables - PV advection
      real(kind=RKIND), dimension(:,:), pointer :: dEPV_dxZonal, dEPV_dyMerid, dEPV_dz, &
                                                   advectionPV_zonal, advectionPV_meridional, advectionPV_vertical

      call mpas_pool_get_dimension(mesh, 'nCells', nCells)
      call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)
      call mpas_pool_get_dimension(mesh, 'nEdges', nEdges)
      call mpas_pool_get_array(mesh, 'nEdgesOnCell', nEdgesOnCell)
      call mpas_pool_get_array(mesh, 'cellsOnEdge', cellsOnEdge)
      call mpas_pool_get_array(mesh, 'edgesOnCell', edgesOnCell)
      call mpas_pool_get_array(mesh, 'edgesOnCell_sign', edgesOnCell_sign)
      call mpas_pool_get_array(mesh, 'latCell', latCell)
      call mpas_pool_get_array(mesh, 'lonCell', lonCell)
      call mpas_pool_get_array(mesh, 'dcEdge', dcEdge)
      call mpas_pool_get_array(mesh, 'coeffs_reconstruct', coeffs_reconstruct)
      call mpas_pool_get_array(mesh, 'dzu', dzu)
      call mpas_pool_get_array(diag, 'uReconstructZonal', uReconstructZonal)
      call mpas_pool_get_array(diag, 'uReconstructMeridional', uReconstructMeridional)
      call mpas_pool_get_array(diag, 'wCell', wCell)

      call mpas_pool_get_array(diag, 'ertel_pv', ertel_pv)                
      call mpas_pool_get_array(diag, 'dEPV_dxZonal', dEPV_dxZonal)
      call mpas_pool_get_array(diag, 'dEPV_dyMerid', dEPV_dyMerid)
      call mpas_pool_get_array(diag, 'dEPV_dz', dEPV_dz)

      call mpas_pool_get_array(diag, 'advectionPV_zonal', advectionPV_zonal)   
      call mpas_pool_get_array(diag, 'advectionPV_meridional', advectionPV_meridional)
      call mpas_pool_get_array(diag, 'advectionPV_vertical', advectionPV_vertical)

      dEPV_dxZonal(:,:) = 0.0_RKIND
      dEPV_dyMerid(:,:) = 0.0_RKIND
      dEPV_dz(:,:) = 0.0_RKIND
      advectionPV_zonal(:,:) = 0.0_RKIND
      advectionPV_meridional(:,:) = 0.0_RKIND
      advectionPV_vertical(:,:) = 0.0_RKIND

      !***********************************************************************************************
      ! Calculate the PV advection terms
      ! Note: these are computed using the reconstructed edge gradients and zonal/meridional wind 
      ! components, which is much different and simpler than how advection is computed in the MPAS 
      ! dynamics code.
      !***********************************************************************************************

      ! (1) Calculate and reconstruct horizontal potential vorticity gradient to get zonal and meridional
      !     gradients at cell centers: dPV_dx, dPV_dy

      call calc_gradOnEdges_reconCellCenter(ertel_pv, nCells, nEdges, nVertLevels, nEdgesOnCell, &
                                edgesOnCell, edgesOnCell_sign, cellsOnEdge, dcEdge, coeffs_reconstruct, &
                                latCell, lonCell, dEPV_dxZonal, dEPV_dyMerid)

      ! (2) Calculate the vertical potential vorticity gradient: dEPV_dz

      call calc_vertDeriv(ertel_pv, nCells, nVertLevels, dzu, dEPV_dz)

      ! (3) Calculate advection terms: u*dPV/dx, v*dPV/dy, w*dPV/dz

      advectionPV_zonal = uReconstructZonal * dEPV_dxZonal
      advectionPV_meridional = uReconstructMeridional * dEPV_dyMerid
      advectionPV_vertical = wCell * dEPV_dz

   end subroutine calc_pvAdvection

   !*********************************************************************************************************************
   ! MC: Modified subroutine to call PV and PV advection calculations and interpolation of diagnostic fields to dynamic 
   !     tropopause
   !********************************************************************************************************************* 

   subroutine atm_compute_pv_diagnostics(state, time_lev, diag, mesh) 
   
      use mpas_constants
      use mpas_derived_types, only : field2DReal
      use mpas_pool_routines, only : mpas_pool_get_dimension, mpas_pool_get_array, mpas_pool_get_field
      use mpas_dmpar, only : mpas_dmpar_exch_halo_field
      use mpas_log, only : mpas_log_write
 
      implicit none
   
      type (mpas_pool_type), intent(inout) :: state
      integer, intent(in) :: time_lev                                           
      type (mpas_pool_type), intent(inout) :: diag
      type (mpas_pool_type), intent(in) :: mesh
   
      integer :: iCell, k
      integer, pointer :: nCells, nVertLevels, index_qv
      real (kind=RKIND) :: pvuVal, missingVal, stratoPV

      type (field2DReal), pointer :: theta_f, uReconstructZonal_f, uReconstructMeridional_f, w_f, epv_f, pv_vertex_f, &
                                     wCell_f !, vorticity_f

      !!!!!!!!!!!!!!!!!!!!

      call mpas_pool_get_field(state, 'w', w_f, time_lev)                                          
      call mpas_pool_get_field(diag, 'uReconstructZonal', uReconstructZonal_f)                     ! MC added
      call mpas_pool_get_field(diag, 'uReconstructMeridional', uReconstructMeridional_f)           ! MC added
      call mpas_pool_get_field(diag, 'wCell', wCell_f)                                             ! MC added 
      call mpas_pool_get_field(diag, 'theta', theta_f)
      call mpas_pool_get_field(diag, 'pv_vertex', pv_vertex_f)                                     ! MC added
      !call mpas_pool_get_field(diag, 'vorticity', vorticity_f) 

      !need halo cells for everything w/ horizontal derivative 
      call mpas_dmpar_exch_halo_field(w_f)
      call mpas_dmpar_exch_halo_field(uReconstructZonal_f)                               ! MC added
      call mpas_dmpar_exch_halo_field(uReconstructMeridional_f)                          ! MC added
      call mpas_dmpar_exch_halo_field(wCell_f)                                           ! MC added
      call mpas_dmpar_exch_halo_field(theta_f)
      call mpas_dmpar_exch_halo_field(pv_vertex_f)                                       ! MC added
      !call mpas_dmpar_exch_halo_field(vorticity_f)
 
      ! call calc_epv subroutine 
      call calc_epv(mesh, time_lev, state, diag)                  

      ! halo cells need to be valid for flood fill routines called below
      call mpas_pool_get_field(diag, 'ertel_pv', epv_f)                       
      call mpas_dmpar_exch_halo_field(epv_f)
    
      ! call calc_pvAdvection subroutine
      call calc_pvAdvection(state, time_lev, diag, mesh)

      pvuVal = 2.0_RKIND
      missingVal = -99999.0_RKIND
      stratoPV = 10.0_RKIND

      !***********************************************************************************************
      ! Uncomment one of the following to either call floodFill_strato or floodFill_tropo
      ! to estimate the level of the dynamic tropopause.
      !***********************************************************************************************
     
      !call floodFill_strato(mesh, diag, pvuVal, stratoPV)    
      call floodFill_tropo(mesh,diag,pvuVal)

      ! call interp_pv_diagnostics subroutine to interpolate fields to dynamic tropopause
      call interp_pv_diagnostics(mesh, diag, pvuVal, missingVal)
   
   end subroutine atm_compute_pv_diagnostics

   !*********************************************************************************************************************
   ! MC: Modified subroutine to calculate Ertel's potential vorticity tendency using state and diagnostic variables at 
   !     the BEGINNING of the previous time step (i.e., before they are updated by the tendencies from dynamics and physics) 
   !     and the tendencies from dynamics and physics over the previous time step. This is necessary via product rule: 
   ! 
   !     dPV/dt = 1/density * [curl(wind) . grad(theta_tendency) + curl(wind_tendency) . grad(theta)] 
   !
   !     and differs from the original formulation, which incorrectly used the updated state and diagnostic variables 
   !     at the end of the time step and the tendencies responsible for updating them!
   !*********************************************************************************************************************

   subroutine calc_pvBudget(state, time_lev, diag, mesh, tend, tend_physics)
 
      use mpas_vector_reconstruction
      use mpas_pool_routines, only : mpas_pool_get_dimension, mpas_pool_get_array
      use mpas_log, only : mpas_log_write
 
      implicit none
      
      type (mpas_pool_type), intent(in) :: state
      integer, intent(in) :: time_lev                                            ! which time level to use from state
      type (mpas_pool_type), intent(inout) :: diag
      type (mpas_pool_type), intent(in) :: mesh
      type (mpas_pool_type), intent(in) :: tend_physics
      type (mpas_pool_type), intent(inout) :: tend                               ! modify tend_w_euler to uncouple with density

      ! input vars
      integer, pointer :: nCells, nVertLevels, nEdges, R3, nVertices
      integer, dimension(:), pointer :: nEdgesOnCell
      integer, dimension(:,:), pointer :: cellsOnEdge, edgesOnCell, verticesOnCell, &
                                          cellsOnVertex, verticesOnEdge   

      real(kind=RKIND), dimension(:),   pointer :: dzu, areaCell, latCell, lonCell, dcEdge, areaTriangle

      ! need to call stored variables from beginning of previous time step 
      real(kind=RKIND), dimension(:,:), pointer :: rho_prev, theta_prev, pv_vertex_prev, zgrid, kiteAreasOnVertex, &
                                                   uReconstructZonal_prev, uReconstructMeridional_prev, edgesOnCell_sign, &
                                                   wCell_prev, ertel_pv_prev, rho_edge_prev 

      real(kind=RKIND), dimension(:,:,:), pointer :: coeffs_reconstruct

      real(kind=RKIND), dimension(:,:), pointer :: depv_dt_lw, depv_dt_sw, depv_dt_bl, depv_dt_cu, depv_dt_mp, depv_dt_mix
      real(kind=RKIND), dimension(:,:), pointer :: depv_dt_diab, depv_dt_fric
      real(kind=RKIND), dimension(:,:), pointer :: tend_u_phys, tend_u_euler, rho_edge, tend_w_euler
      real(kind=RKIND), dimension(:,:), pointer :: rthblten, rthcuten, rthratenlw, rthratensw, &
                                                   dtheta_dt_mp, dtheta_dt_mix

      ! local static variables 
      real(kind=RKIND), dimension(:,:), allocatable :: duZonal_dz, duMerid_dz
      real(kind=RKIND), dimension(:,:), allocatable :: dTheta_dxZonal, dTheta_dyMerid, dTheta_dz 
      real(kind=RKIND), dimension(:,:), allocatable :: dW_dxZonal, dW_dyMerid
      real(kind=RKIND), dimension(:,:), allocatable :: absVort
      real(kind=RKIND), dimension(:,:,:), allocatable :: absVort3D, gradTheta

      ! diabatic tendency variables
      real(kind=RKIND), dimension(:,:), allocatable :: dLWtend_dxZonal, dLWtend_dyMerid, dLWtend_dz      ! Gradients of theta tendencies from LW radiation

      real(kind=RKIND), dimension(:,:), allocatable :: dSWtend_dxZonal, dSWtend_dyMerid, dSWtend_dz      ! Gradients of theta tendencies from SW radiation

      real(kind=RKIND), dimension(:,:), allocatable :: dBLtend_dxZonal, dBLtend_dyMerid, dBLtend_dz      ! Gradients of theta tendencies from PBL scheme

      real(kind=RKIND), dimension(:,:), allocatable :: dCUtend_dxZonal, dCUtend_dyMerid, dCUtend_dz      ! Gradients of theta tendencies from cumulus scheme

      real(kind=RKIND), dimension(:,:), allocatable :: dMPtend_dxZonal, dMPtend_dyMerid, dMPtend_dz      ! Gradients of theta tendencies from microphysics

      real(kind=RKIND), dimension(:,:), allocatable :: dMXtend_dxZonal, dMXtend_dyMerid, dMXtend_dz      ! Gradients of theta tendencies from explicit horiz mixing 

      real(kind=RKIND), dimension(:,:,:), allocatable :: grad_diabatic_LW, grad_diabatic_SW, grad_diabatic_BL, &
                                                         grad_diabatic_CU, grad_diabatic_MP, grad_diabatic_MX

      ! friction tendency vars 
      real(kind=RKIND), dimension(:,:), allocatable :: dWtend_dxZonal, dWtend_dyMerid, duZonalTend_dz, duMeridTend_dz, &
                                                       vertVortTend, tenduX, tenduY, tenduZ, tend_uZonal, tend_uMerid
      real(kind=RKIND), dimension(:,:,:), allocatable :: vortTend3D
      real(kind=RKIND), dimension(:,:), pointer :: tend_wCell, uTend_curl

      call mpas_pool_get_dimension(mesh, 'nCells', nCells)
      call mpas_pool_get_dimension(mesh, 'R3', R3)
      call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)
      call mpas_pool_get_dimension(mesh, 'nEdges', nEdges)
      call mpas_pool_get_dimension(mesh, 'nVertices', nVertices)
      call mpas_pool_get_array(mesh, 'nEdgesOnCell', nEdgesOnCell)
      call mpas_pool_get_array(mesh, 'cellsOnEdge', cellsOnEdge)
      call mpas_pool_get_array(mesh, 'edgesOnCell', edgesOnCell)
      call mpas_pool_get_array(mesh, 'verticesOnCell', verticesOnCell)
      call mpas_pool_get_array(mesh, 'verticesOnEdge', verticesOnEdge)
      call mpas_pool_get_array(mesh, 'kiteAreasOnVertex', kiteAreasOnVertex)
      call mpas_pool_get_array(mesh, 'areaTriangle', areaTriangle)
      call mpas_pool_get_array(mesh, 'cellsOnVertex', cellsOnVertex)
      call mpas_pool_get_array(mesh, 'edgesOnCell_sign', edgesOnCell_sign)
      call mpas_pool_get_array(mesh, 'latCell', latCell)
      call mpas_pool_get_array(mesh, 'lonCell', lonCell)
      call mpas_pool_get_array(mesh, 'dcEdge', dcEdge)
      call mpas_pool_get_array(mesh, 'areaCell', areaCell)
      call mpas_pool_get_array(mesh, 'coeffs_reconstruct', coeffs_reconstruct)
      call mpas_pool_get_array(mesh, 'zgrid', zgrid)
      call mpas_pool_get_array(mesh, 'dzu', dzu)

!      call mpas_pool_get_array(state, 'w', w, time_lev)
      call mpas_pool_get_array(diag, 'theta_prev', theta_prev)    
      call mpas_pool_get_array(diag, 'rho_prev', rho_prev)
      call mpas_pool_get_array(diag, 'pv_vertex_prev', pv_vertex_prev)
      call mpas_pool_get_array(diag, 'uReconstructZonal_prev', uReconstructZonal_prev)
      call mpas_pool_get_array(diag, 'uReconstructMeridional_prev', uReconstructMeridional_prev)
      call mpas_pool_get_array(diag, 'wCell_prev', wCell_prev)
      
      call mpas_pool_get_array(tend_physics, 'rthblten', rthblten)
      call mpas_pool_get_array(tend_physics, 'rthcuten', rthcuten)
      call mpas_pool_get_array(tend_physics, 'rthratenlw', rthratenlw)
      call mpas_pool_get_array(tend_physics, 'rthratensw', rthratensw)
      call mpas_pool_get_array(diag, 'dtheta_dt_mp', dtheta_dt_mp)
      call mpas_pool_get_array(diag, 'dtheta_dt_mix', dtheta_dt_mix)
      
      call mpas_pool_get_array(diag, 'depv_dt_lw', depv_dt_lw)
      call mpas_pool_get_array(diag, 'depv_dt_sw', depv_dt_sw)
      call mpas_pool_get_array(diag, 'depv_dt_bl', depv_dt_bl)
      call mpas_pool_get_array(diag, 'depv_dt_cu', depv_dt_cu)
      call mpas_pool_get_array(diag, 'depv_dt_mp', depv_dt_mp)
      call mpas_pool_get_array(diag, 'depv_dt_mix', depv_dt_mix)
      call mpas_pool_get_array(diag, 'depv_dt_diab', depv_dt_diab)
      call mpas_pool_get_array(diag, 'depv_dt_fric', depv_dt_fric)

      call mpas_pool_get_array(diag, 'tend_u_phys', tend_u_phys)
      call mpas_pool_get_array(diag, 'tend_wCell', tend_wCell)
      call mpas_pool_get_array(diag, 'rho_edge_prev', rho_edge_prev)
      call mpas_pool_get_array(tend, 'u_euler', tend_u_euler)
      call mpas_pool_get_array(tend, 'w_euler', tend_w_euler)
      call mpas_pool_get_array(diag, 'uTend_curl', uTend_curl)

      ! allocate local static variables
      allocate(duZonal_dz(nVertLevels,nCells))
      allocate(duMerid_dz(nVertLevels,nCells))
      allocate(dTheta_dxZonal(nVertLevels,nCells))
      allocate(dTheta_dyMerid(nVertLevels,nCells))
      allocate(dTheta_dz(nVertLevels,nCells))
      allocate(dW_dxZonal(nVertLevels,nCells))
      allocate(dW_dyMerid(nVertLevels,nCells))
      allocate(absVort(nVertLevels,nCells))
      allocate(absVort3D(nVertLevels,nCells,3))
      allocate(gradTheta(nVertLevels,nCells,3))
     
      ! allocate diabatic tendency variables
      allocate(dLWtend_dxZonal(nVertLevels,nCells))
      allocate(dLWtend_dyMerid(nVertLevels,nCells))
      allocate(dLWtend_dz(nVertLevels,nCells))
      allocate(dSWtend_dxZonal(nVertLevels,nCells))
      allocate(dSWtend_dyMerid(nVertLevels,nCells))
      allocate(dSWtend_dz(nVertLevels,nCells))
      allocate(dBLtend_dxZonal(nVertLevels,nCells))
      allocate(dBLtend_dyMerid(nVertLevels,nCells))
      allocate(dBLtend_dz(nVertLevels,nCells))
      allocate(dCUtend_dxZonal(nVertLevels,nCells))
      allocate(dCUtend_dyMerid(nVertLevels,nCells))
      allocate(dCUtend_dz(nVertLevels,nCells))
      allocate(dMPtend_dxZonal(nVertLevels,nCells))
      allocate(dMPtend_dyMerid(nVertLevels,nCells))
      allocate(dMPtend_dz(nVertLevels,nCells))
      allocate(dMXtend_dxZonal(nVertLevels,nCells))
      allocate(dMXtend_dyMerid(nVertLevels,nCells))
      allocate(dMXtend_dz(nVertLevels,nCells))
      allocate(grad_diabatic_LW(nVertLevels,nCells,R3))
      allocate(grad_diabatic_SW(nVertLevels,nCells,R3))
      allocate(grad_diabatic_BL(nVertLevels,nCells,R3))
      allocate(grad_diabatic_CU(nVertLevels,nCells,R3))
      allocate(grad_diabatic_MX(nVertLevels,nCells,R3))
      allocate(grad_diabatic_MP(nVertLevels,nCells,R3))

      ! allocate friction tendency variables
      allocate(dWtend_dxZonal(nVertLevels,nCells))
      allocate(dWtend_dyMerid(nVertLevels,nCells))
      allocate(duZonalTend_dz(nVertLevels,nCells))
      allocate(duMeridTend_dz(nVertLevels,nCells))
      allocate(vertVortTend(nVertLevels,nCells))
      allocate(tenduX(nVertLevels,nCells+1))
      allocate(tenduY(nVertLevels,nCells+1))
      allocate(tenduZ(nVertLevels,nCells+1))
      allocate(tend_uZonal(nVertLevels,nCells+1))
      allocate(tend_uMerid(nVertLevels,nCells+1))
      allocate(vortTend3D(nVertLevels,nCells,3))

      depv_dt_lw(:,:) = 0.0_RKIND
      depv_dt_sw(:,:) = 0.0_RKIND
      depv_dt_bl(:,:) = 0.0_RKIND
      depv_dt_cu(:,:) = 0.0_RKIND
      depv_dt_mp(:,:) = 0.0_RKIND 
      depv_dt_mix(:,:) = 0.0_RKIND
      depv_dt_diab(:,:) = 0.0_RKIND
      depv_dt_fric(:,:) = 0.0_RKIND
      dWtend_dxZonal(:,:) =  0.0_RKIND
      dWtend_dyMerid(:,:) =  0.0_RKIND

      !***********************************************************************************************
      ! Calculate terms needed for PV tendency equation
      !***********************************************************************************************

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Calculate the 3D potential temperature gradient using theta at beginning of time step  
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ! (1) Calculate and reconstruct horizontal potential temperature gradient to get zonal and meridional
      !     gradients at cell centers: dth_dx, dth_dy

      call calc_gradOnEdges_reconCellCenter(theta_prev, nCells, nEdges, nVertLevels, nEdgesOnCell, &
                                edgesOnCell, edgesOnCell_sign, cellsOnEdge, dcEdge, coeffs_reconstruct, &
                                latCell, lonCell, dTheta_dxZonal, dTheta_dyMerid)

      ! (2) Calculate the vertical potential temperature gradient: dth_dz

      call calc_vertDeriv(theta_prev, nCells, nVertLevels, dzu, dTheta_dz)

      ! (3) Combine theta derivatives into 3D vector

      gradTheta(:,:,1) = dTheta_dxZonal
      gradTheta(:,:,2) = dTheta_dyMerid
      gradTheta(:,:,3) = dTheta_dz

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Calculate the 3D absolute vorticity vector using winds at beginning of time step  
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ! (1) Calculate the vertical shear of uReconstructZonal and uReconstructMeridional: du_dz and dv_dz

      call calc_vertDeriv(uReconstructZonal_prev, nCells, nVertLevels, dzu, duZonal_dz)
      call calc_vertDeriv(uReconstructMeridional_prev, nCells, nVertLevels, dzu, duMerid_dz)

      ! (2) Interpolate w to cell centers, calculate gradient of w on edges, and then reconstruct to get 
      !     zonal and meridional gradients at cell centers: dw_dx, dw_dy 

      call calc_gradOnEdges_reconCellCenter(wCell_prev, nCells, nEdges, nVertLevels, nEdgesOnCell, &
                                edgesOnCell, edgesOnCell_sign, cellsOnEdge, dcEdge, coeffs_reconstruct, &
                                latCell, lonCell, dW_dxZonal, dW_dyMerid)

      ! (3) Reconstruct absolute vertical vorticity at vertices pv_vertex to cell centers
   
      !     Note: currently, pv_vertex is the absolute vertical vorticity on the cell vertices. If this 
      !     variable changes at some point, then the absolute vertical vorticity on the vertices needs 
      !     to be computed as follows: 
      !     do iVert=1,nVertices
      !        vorticity(:,iVert) = vorticity(:,iVert) + fVertex(iVert)
      !     end do

      call interp_absVertVort(pv_vertex_prev, nCells, nEdgesOnCell, verticesOnCell, &
                                cellsOnVertex, areaCell, kiteAreasOnVertex, absVort)

      ! (4) Combine three components into vorticity vector

      absVort3D(:,:,1) = dW_dyMerid - duMerid_dz        ! dw/dy - dv/dz
      absVort3D(:,:,2) = duZonal_dz - dW_dxZonal        ! du/dz - dw/dx
      absVort3D(:,:,3) = absVort                        ! dv/dy - du/dx + f

      !***********************************************************************************************
      ! Calculate diabatic PV tendency terms:
      !***********************************************************************************************

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Longwave radiation tendency: depv_dt_lw
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (associated(rthratenlw)) then
         ! Calculate and reconstruct horizontal gradients of theta tendency

         call calc_gradOnEdges_reconCellCenter(rthratenlw, nCells, nEdges, nVertLevels, nEdgesOnCell, &
                                  edgesOnCell, edgesOnCell_sign, cellsOnEdge, dcEdge, coeffs_reconstruct, &
                                  latCell, lonCell, dLWtend_dxZonal, & 
                                  dLWtend_dyMerid)

         ! Calculate vertical gradient of theta tendency
         call calc_vertDeriv(rthratenlw, nCells, nVertLevels, dzu, dLWtend_dz)
          
         ! Combine into 3D theta tendency gradient vector 
         grad_diabatic_LW(:,:,1) = dLWtend_dxZonal
         grad_diabatic_LW(:,:,2) = dLWtend_dyMerid
         grad_diabatic_LW(:,:,3) = dLWtend_dz
         
         call calc_dotProduct_3D(grad_diabatic_LW, absVort3D, nCells, nVertLevels, depv_dt_lw)
         
         depv_dt_lw = depv_dt_lw / rho_prev * 1.0e6                             ! MC: using density from beginning of time step here... 
      else
         depv_dt_lw = 0.0_RKIND
      end if

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Shortwave radiation tendency: depv_dt_sw
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (associated(rthratensw)) then
         ! Calculate and reconstruct horizontal gradients of theta tendency
 
          call calc_gradOnEdges_reconCellCenter(rthratensw, nCells, nEdges, nVertLevels, nEdgesOnCell, &
                                   edgesOnCell, edgesOnCell_sign, cellsOnEdge, dcEdge, coeffs_reconstruct, &
                                   latCell, lonCell, dSWtend_dxZonal, &
                                   dSWtend_dyMerid)
 
          ! Calculate vertical gradient of theta tendency
          call calc_vertDeriv(rthratensw, nCells, nVertLevels, dzu, dSWtend_dz)
 
          ! Combine into 3D theta tendency gradient vector 
          grad_diabatic_SW(:,:,1) = dSWtend_dxZonal
          grad_diabatic_SW(:,:,2) = dSWtend_dyMerid
          grad_diabatic_SW(:,:,3) = dSWtend_dz
 
          call calc_dotProduct_3D(grad_diabatic_SW, absVort3D, nCells, nVertLevels, depv_dt_sw)
 
          depv_dt_sw = depv_dt_sw / rho_prev * 1.0e6
      else
          depv_dt_sw = 0.0_RKIND
      end if
 
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! PBL diabatic tendency: depv_dt_bl
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (associated(rthblten)) then
         ! Calculate and reconstruct horizontal gradients of theta tendency
 
          call calc_gradOnEdges_reconCellCenter(rthblten, nCells, nEdges, nVertLevels, nEdgesOnCell, &
                                   edgesOnCell, edgesOnCell_sign, cellsOnEdge, dcEdge, coeffs_reconstruct, &
                                   latCell, lonCell, dBLtend_dxZonal, &
                                   dBLtend_dyMerid)
 
          ! Calculate vertical gradient of theta tendency
          call calc_vertDeriv(rthblten, nCells, nVertLevels, dzu, dBLtend_dz)
 
          ! Combine into 3D theta tendency gradient vector 
          grad_diabatic_BL(:,:,1) = dBLtend_dxZonal
          grad_diabatic_BL(:,:,2) = dBLtend_dyMerid
          grad_diabatic_BL(:,:,3) = dBLtend_dz
 
          call calc_dotProduct_3D(grad_diabatic_BL, absVort3D, nCells, nVertLevels, depv_dt_bl)
 
          depv_dt_bl = depv_dt_bl / rho_prev * 1.0e6
      else
          depv_dt_bl = 0.0_RKIND
      end if
 
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Cumulus diabatic tendency: depv_dt_cu
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (associated(rthcuten)) then
         ! Calculate and reconstruct horizontal gradients of theta tendency
 
          call calc_gradOnEdges_reconCellCenter(rthcuten, nCells, nEdges, nVertLevels, nEdgesOnCell, &
                                   edgesOnCell, edgesOnCell_sign, cellsOnEdge, dcEdge, coeffs_reconstruct, &
                                   latCell, lonCell, dCUtend_dxZonal, &
                                   dCUtend_dyMerid)
  
          ! Calculate vertical gradient of theta tendency
          call calc_vertDeriv(rthcuten, nCells, nVertLevels, dzu, dCUtend_dz)
 
          ! Combine into 3D theta tendency gradient vector 
          grad_diabatic_CU(:,:,1) = dCUtend_dxZonal
          grad_diabatic_CU(:,:,2) = dCUtend_dyMerid
          grad_diabatic_CU(:,:,3) = dCUtend_dz
 
          call calc_dotProduct_3D(grad_diabatic_CU, absVort3D, nCells, nVertLevels, depv_dt_cu)
 
          depv_dt_cu = depv_dt_cu / rho_prev * 1.0e6
      else
          depv_dt_cu = 0.0_RKIND
      end if
 
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Microphysics diabatic tendency: depv_dt_mp
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (associated(dtheta_dt_mp)) then
         ! Calculate and reconstruct horizontal gradients of theta tendency
 
          call calc_gradOnEdges_reconCellCenter(dtheta_dt_mp, nCells, nEdges, nVertLevels, nEdgesOnCell, &
                                   edgesOnCell, edgesOnCell_sign, cellsOnEdge, dcEdge, coeffs_reconstruct, &
                                   latCell, lonCell, dMPtend_dxZonal, &
                                   dMPtend_dyMerid)
 
          ! Calculate vertical gradient of theta tendency
          call calc_vertDeriv(dtheta_dt_mp, nCells, nVertLevels, dzu, dMPtend_dz)
 
          ! Combine into 3D theta tendency gradient vector 
          grad_diabatic_MP(:,:,1) = dMPtend_dxZonal
          grad_diabatic_MP(:,:,2) = dMPtend_dyMerid
          grad_diabatic_MP(:,:,3) = dMPtend_dz
 
          call calc_dotProduct_3D(grad_diabatic_MP, absVort3D, nCells, nVertLevels, depv_dt_mp)
 
          depv_dt_mp = depv_dt_mp / rho_prev * 1.0e6
      else
          depv_dt_mp = 0.0_RKIND
      end if
 
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Diabatic tendency from explicit mixing: depv_dt_mix
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (associated(dtheta_dt_mix)) then
         ! Calculate and reconstruct horizontal gradients of theta tendency
  
          call calc_gradOnEdges_reconCellCenter(dtheta_dt_mix, nCells, nEdges, nVertLevels, nEdgesOnCell, &
                                   edgesOnCell, edgesOnCell_sign, cellsOnEdge, dcEdge, coeffs_reconstruct, &
                                   latCell, lonCell, dMXtend_dxZonal, &
                                   dMXtend_dyMerid)
 
          ! Calculate vertical gradient of theta tendency
          call calc_vertDeriv(dtheta_dt_mix, nCells, nVertLevels, dzu, dMXtend_dz)
 
          ! Combine into 3D theta tendency gradient vector 
          grad_diabatic_MX(:,:,1) = dMXtend_dxZonal
          grad_diabatic_MX(:,:,2) = dMXtend_dyMerid
          grad_diabatic_MX(:,:,3) = dMXtend_dz
 
          call calc_dotProduct_3D(grad_diabatic_MX, absVort3D, nCells, nVertLevels, depv_dt_mix)
 
          depv_dt_mix = depv_dt_mix / rho_prev * 1.0e6
      else
          depv_dt_mix = 0.0_RKIND
      end if
 
      ! Sum of all diabatic contributions to PV through potential temperature tendencies   
      depv_dt_diab = depv_dt_lw + depv_dt_sw + depv_dt_bl + depv_dt_cu + depv_dt_mp + depv_dt_mix
 
      !***********************************************************************************************
      ! Calculate friction tendency terms:
      !***********************************************************************************************
 
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Friction terms are essentially the vorticity tendency due to friction. Need to use the u, v, w
      ! tendencies 
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
      ! (1) Uncouple the u tendency on cell edges from dynamics and add to the u tendency from physics 
 
      tend_u_phys(:,:) = tend_u_phys(:,:) + tend_u_euler(:,:) / rho_edge_prev(:,:)
 
 
      ! (2) Reconstruct tend_u_phys to cell center and calculate vertical derivative of 
      !     tend_uZonal and tend_uMerid
 
      call mpas_reconstruct(mesh, tend_u_phys, tenduX, tenduY, tenduZ, tend_uZonal, tend_uMerid)
 
      call calc_vertDeriv(tend_uZonal, nCells, nVertLevels, dzu, duZonalTend_dz)
      call calc_vertDeriv(tend_uMerid, nCells, nVertLevels, dzu, duMeridTend_dz)
 
      ! (3) Interpolate w tendency from dynamics to cell center, uncouple from density, calclulate gradient 
      !     of tend_wCell on edges, and then reconstruct to get zonal and meridional gradients at cell center:
      !     dwTend_dx, dwTend_dy  
      !
      !     Note: Nick's procedure interpolated density to w levels before uncoupling, but this method 
      !           is consistent with the procedure in the vorticity calculation 
 
      call interp_wLev_thetaLev(tend_w_euler, nCells, nVertLevels, tend_wCell)
     
      tend_wCell(:,:) = tend_wCell(:,:) / rho_prev(:,:) 
 
      call calc_gradOnEdges_reconCellCenter(tend_wCell, nCells, nEdges, nVertLevels, nEdgesOnCell, &
                                edgesOnCell, edgesOnCell_sign, cellsOnEdge, dcEdge, coeffs_reconstruct, &
                                latCell, lonCell, dWtend_dxZonal, dWtend_dyMerid)
 
      ! (4) Calculate vertical vorticity tendency on vertices and reconstruct to cell center
      !     Note: the analogy of this procedure in the ertel_pv calculation is missing because vertical vorticity
      !           is already computed and output in MPAS 
       
      call calc_vertical_curl(tend_u_phys, nEdges, nVertices, dcEdge, areaTriangle, verticesOnEdge, uTend_curl)
 
      call interp_absVertVort(uTend_curl, nCells, nEdgesOnCell, verticesOnCell, &
                               cellsOnVertex, areaCell, kiteAreasOnVertex, vertVortTend)
 
      ! (5) Combine three components into vorticity tendency vector
 
      vortTend3D(:,:,1) = dWtend_dyMerid - duMeridTend_dz        ! dFz/dy - dFy/dz
      vortTend3D(:,:,2) = duZonalTend_dz - dWtend_dxZonal        ! dFx/dz - dFz/dx
      vortTend3D(:,:,3) = vertVortTend                           ! dFy/dy - dFx/dx 
 
      call calc_dotProduct_3D(gradTheta, vortTend3D, nCells, nVertLevels, depv_dt_fric)
 
      depv_dt_fric = depv_dt_fric / rho_prev * 1.0e6

      deallocate(duZonal_dz)
      deallocate(duMerid_dz)
      deallocate(dTheta_dxZonal)
      deallocate(dTheta_dyMerid)
      deallocate(dTheta_dz)
      deallocate(dW_dxZonal)
      deallocate(dW_dyMerid)
      deallocate(absVort)
      deallocate(absVort3D)
      deallocate(gradTheta)
      deallocate(dLWtend_dxZonal)
      deallocate(dLWtend_dyMerid)
      deallocate(dLWtend_dz)
      deallocate(dSWtend_dxZonal)
      deallocate(dSWtend_dyMerid)
      deallocate(dSWtend_dz)
      deallocate(dBLtend_dxZonal)
      deallocate(dBLtend_dyMerid)
      deallocate(dBLtend_dz)
      deallocate(dCUtend_dxZonal)
      deallocate(dCUtend_dyMerid)
      deallocate(dCUtend_dz)
      deallocate(dMPtend_dxZonal)
      deallocate(dMPtend_dyMerid)
      deallocate(dMPtend_dz)
      deallocate(dMXtend_dxZonal)
      deallocate(dMXtend_dyMerid)
      deallocate(dMXtend_dz)
      deallocate(grad_diabatic_LW)
      deallocate(grad_diabatic_SW)
      deallocate(grad_diabatic_BL)
      deallocate(grad_diabatic_CU)
      deallocate(grad_diabatic_MX)
      deallocate(grad_diabatic_MP)
      deallocate(dWtend_dxZonal)
      deallocate(dWtend_dyMerid)
      deallocate(duZonalTend_dz)
      deallocate(duMeridTend_dz)
      deallocate(vertVortTend)
      deallocate(tenduX)
      deallocate(tenduY)
      deallocate(tenduZ)
      deallocate(tend_uZonal)
      deallocate(tend_uMerid)
      deallocate(vortTend3D)

   end subroutine calc_pvBudget

   !*********************************************************************************************************************
   ! MC: Modified subroutine to call PV tendency calculations and interpolation of tendency fields to dynamic 
   !     tropopause at beginning of time step 
   !********************************************************************************************************************* 
   
   subroutine atm_compute_pvBudget_diagnostics(state, time_lev, diag, mesh, tend, tend_physics)
      
      use mpas_constants
      use mpas_derived_types, only : field2DReal
      use mpas_pool_routines, only : mpas_pool_get_dimension, mpas_pool_get_array, mpas_pool_get_field
      use mpas_dmpar, only : mpas_dmpar_exch_halo_field
 
      implicit none
      
      type (mpas_pool_type), intent(inout) :: diag, tend
      type (mpas_pool_type), intent(in) :: state, mesh, tend_physics
      integer, intent(in) :: time_lev                                  
   
      integer :: iCell, k
      integer, pointer :: nCells, nVertLevels, index_qv
      real (kind=RKIND) :: pvuVal, missingVal 
      real (kind=RKIND), dimension(:,:), pointer :: dtheta_dt_mix, tend_theta_euler 
      type (field2DReal), pointer :: rthratenlw_f, rthratensw_f, rthcuten_f, rthblten_f, dtheta_dt_mp_f, theta_euler_f, dtheta_dt_mix_f
      type (field2DReal), pointer :: tend_u_phys_f, tend_u_euler_f, tend_w_euler_f, tend_wCell_f, uTend_curl_f

      real (kind=RKIND), dimension(:,:,:), pointer :: scalars

      ! need halo communication for beginning of time step variables for PV tendency calculations 
      type (field2DReal), pointer :: theta_prev_f, uReconstructZonal_prev_f, uReconstructMeridional_prev_f, wCell_prev_f, pv_vertex_prev_f
      !!!!!!!
      call mpas_pool_get_dimension(mesh, 'nCells', nCells)
      call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)
      call mpas_pool_get_dimension(state, 'index_qv', index_qv)
      call mpas_pool_get_array(state, 'scalars', scalars, time_lev)

      !need halo cells for everything w/ horizontal derivative             
      !Dtheta/Dt
      call mpas_pool_get_array(tend, 'theta_euler', tend_theta_euler)
      call mpas_pool_get_array(diag, 'dtheta_dt_mix', dtheta_dt_mix)       

      ! NS: With modified moist potential temperature being the model state variable being mixed,
      !     assume qv field is not mixed and so there's no tend_qv to consider
      ! MC: This variable still needs to be checked as of 01/04/23, but horizontal mixing contribution to PV tendency is 
      !     much smaller than other terms, so it's likely fine regardless of mixing of moisture 
      !     Are the theta tendencies and scalars valid at the same time here? 

      do iCell = 1,nCells
         do k = 1,nVertLevels
            dtheta_dt_mix(k,iCell) = tend_theta_euler(k,iCell) / (1._RKIND + rvord * scalars(index_qv,k,iCell))
         end do
      end do

      call mpas_pool_get_field(diag, 'uReconstructZonal_prev', uReconstructZonal_prev_f)           ! MC added
      call mpas_pool_get_field(diag, 'uReconstructMeridional_prev', uReconstructMeridional_prev_f) ! MC added
      call mpas_pool_get_field(diag, 'theta_prev', theta_prev_f)
      call mpas_pool_get_field(diag, 'wCell_prev', wCell_prev_f)                                   ! MC added 
      call mpas_pool_get_field(diag, 'pv_vertex_prev', pv_vertex_prev_f)                           ! MC added
     
      call mpas_dmpar_exch_halo_field(theta_prev_f)
      call mpas_dmpar_exch_halo_field(uReconstructZonal_prev_f)                               ! MC added
      call mpas_dmpar_exch_halo_field(uReconstructMeridional_prev_f)                          ! MC added
      call mpas_dmpar_exch_halo_field(wCell_prev_f)                                           ! MC added
      call mpas_dmpar_exch_halo_field(pv_vertex_prev_f)                                       ! MC added

      call mpas_pool_get_field(tend_physics, 'rthratenlw', rthratenlw_f)
      call mpas_pool_get_field(tend_physics, 'rthratensw', rthratensw_f)
      call mpas_pool_get_field(tend_physics, 'rthcuten', rthcuten_f)
      call mpas_pool_get_field(tend_physics, 'rthblten', rthblten_f)
      call mpas_pool_get_field(diag, 'dtheta_dt_mp', dtheta_dt_mp_f)
      call mpas_pool_get_field(diag, 'dtheta_dt_mix', dtheta_dt_mix_f)
 
      call mpas_dmpar_exch_halo_field(rthratenlw_f)
      call mpas_dmpar_exch_halo_field(rthratensw_f)
      call mpas_dmpar_exch_halo_field(rthcuten_f)
      call mpas_dmpar_exch_halo_field(rthblten_f)
      call mpas_dmpar_exch_halo_field(dtheta_dt_mp_f)
      call mpas_dmpar_exch_halo_field(dtheta_dt_mix_f)

      !friction
      call mpas_pool_get_field(diag, 'tend_u_phys', tend_u_phys_f)
      call mpas_pool_get_field(tend, 'u_euler', tend_u_euler_f)
      call mpas_pool_get_field(tend, 'w_euler', tend_w_euler_f)
      call mpas_pool_get_field(diag, 'tend_wCell', tend_wCell_f)
      call mpas_pool_get_field(diag, 'uTend_curl', uTend_curl_f)

      call mpas_dmpar_exch_halo_field(tend_u_phys_f)
      call mpas_dmpar_exch_halo_field(tend_u_euler_f)
      call mpas_dmpar_exch_halo_field(tend_w_euler_f)
      call mpas_dmpar_exch_halo_field(tend_wCell_f)
      call mpas_dmpar_exch_halo_field(uTend_curl_f)
 
      call calc_pvBudget(state, time_lev, diag, mesh, tend, tend_physics)

      pvuVal = 2.0_RKIND
      missingVal = -99999.0_RKIND

      ! need to interpolate PV tendencies to dynamic tropopause at BEGINNING of time step!
      call interp_pvBudget_diagnostics(mesh, diag, pvuVal, missingVal)
   
   end subroutine atm_compute_pvBudget_diagnostics


!=================================================================================================================
end module pv_diagnostics
!=================================================================================================================
