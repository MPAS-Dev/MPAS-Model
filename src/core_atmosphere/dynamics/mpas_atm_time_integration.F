! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!

#ifdef MPAS_OPENACC
#define MPAS_ACC_TIMER_START(X) call mpas_timer_start(X)
#define MPAS_ACC_TIMER_STOP(X) call mpas_timer_stop(X)
#else
#define MPAS_ACC_TIMER_START(X)
#define MPAS_ACC_TIMER_STOP(X)
#endif

module atm_time_integration

   use mpas_derived_types
   use mpas_pool_routines
   use mpas_kind_types
   use mpas_constants
   use mpas_dmpar
   use mpas_vector_reconstruction
   ! Added only clause to keep xlf90 from getting confused from the overloaded abs intrinsic in mpas_timekeeping
   use mpas_derived_types, only : MPAS_Time_type, MPAS_TimeInterval_type, MPAS_NOW
   use mpas_timekeeping, only: mpas_set_time, mpas_set_timeInterval, mpas_get_time, operator(+)
   use mpas_timer

#ifdef DO_PHYSICS
   use mpas_atmphys_driver_microphysics
   use mpas_atmphys_todynamics
   use mpas_atmphys_utilities
#endif

   use mpas_atm_boundaries, only : nSpecZone, nRelaxZone, nBdyZone, mpas_atm_get_bdy_state, mpas_atm_get_bdy_tend  ! regional_MPAS addition
   
   use mpas_atm_iau  

   !
   ! Abstract interface for routine used to communicate halos of fields
   ! in a named group
   !
   abstract interface
      subroutine halo_exchange_routine(domain, halo_group, ierr)

         use mpas_derived_types, only : domain_type

         type (domain_type), intent(inout) :: domain
         character(len=*), intent(in) :: halo_group
         integer, intent(out), optional :: ierr

      end subroutine halo_exchange_routine
   end interface

   integer :: timerid, secs, u_secs

   ! Used to store physics tendencies for dynamics variables
   real (kind=RKIND), dimension(:,:), pointer :: tend_ru_physics, tend_rtheta_physics, tend_rho_physics
   
   ! Used in compute_dyn_tend
   real (kind=RKIND), allocatable, dimension(:,:) :: qtot 
   real (kind=RKIND), allocatable, dimension(:,:) :: delsq_theta, delsq_w, delsq_divergence
   real (kind=RKIND), allocatable, dimension(:,:) :: delsq_u
!   real (kind=RKIND), allocatable, dimension(:,:) :: delsq_circulation    ! no longer used -> removed 
   real (kind=RKIND), allocatable, dimension(:,:) :: delsq_vorticity
   real (kind=RKIND), allocatable, dimension(:,:) :: dpdz

   ! Used in atm_advance_scalars
   real (kind=RKIND), dimension(:,:,:), allocatable :: horiz_flux_array

   ! Used in atm_advance_scalars_mono
   real (kind=RKIND), dimension(:,:), allocatable :: scalar_old_arr, scalar_new_arr
   real (kind=RKIND), dimension(:,:), allocatable :: s_max_arr, s_min_arr
   real (kind=RKIND), dimension(:,:), allocatable :: flux_array
   real (kind=RKIND), dimension(:,:), allocatable :: flux_upwind_tmp_arr
   real (kind=RKIND), dimension(:,:), allocatable :: flux_tmp_arr
   real (kind=RKIND), dimension(:,:), allocatable :: wdtn_arr
   real (kind=RKIND), dimension(:,:), allocatable :: rho_zz_int

   real (kind=RKIND), dimension(:,:,:), allocatable :: scalars_driving ! regional_MPAS addition 
   real (kind=RKIND), dimension(:,:), allocatable :: ru_driving_tend ! regional_MPAS addition 
   real (kind=RKIND), dimension(:,:), allocatable :: rt_driving_tend ! regional_MPAS addition 
   real (kind=RKIND), dimension(:,:), allocatable :: rho_driving_tend ! regional_MPAS addition 
   real (kind=RKIND), dimension(:,:), allocatable :: ru_driving_values ! regional_MPAS addition 
   real (kind=RKIND), dimension(:,:), allocatable :: rt_driving_values ! regional_MPAS addition 
   real (kind=RKIND), dimension(:,:), allocatable :: rho_driving_values ! regional_MPAS addition 
   integer, dimension(:), pointer :: bdyMaskEdge ! regional_MPAS addition
   logical, pointer :: config_apply_lbcs
   
   ! Used in compute_solve_diagnostics
   real (kind=RKIND), allocatable, dimension(:,:) :: ke_vertex
   real (kind=RKIND), allocatable, dimension(:,:) :: ke_edge

   type (MPAS_Clock_type), pointer, private :: clock
   type (block_type), pointer, private :: block


   ! Used for Rayleigh damping on u
   ! NB: We do not necessarily want this to vary with calendar, as it is used to set
   ! a timescale in seconds given a timescale in days, and it could be rather confusing
   ! if damping in the model changed with the simulation calendar
   real (kind=RKIND), parameter, private :: seconds_per_day = 86400.0_RKIND


   contains


   !***********************************************************************
   !
   !  routine mpas_atm_dynamics_checks
   !
   !> \brief   Checks compatibility of dynamics settings
   !> \author  Michael Duda
   !> \date    14 June 2023
   !> \details
   !>  This routine checks that dynamics settings are valid.
   !>  Specifically,the following are checked by this routine:
   !>
   !>  1) config_positive_definite == .false.
   !>
   !>  At present only a warning is printed in the case of a failed check,
   !>  and a value of 0 is always returned by the ierr argument. However,
   !>  warnings may be escalated to errors in future.
   !
   !-----------------------------------------------------------------------
   subroutine mpas_atm_dynamics_checks(dminfo, blockList, streamManager, ierr)

       use mpas_log, only : mpas_log_write
       use mpas_derived_types, only : dm_info, block_type, MPAS_LOG_WARN
       use mpas_pool_routines, only : mpas_pool_get_config

       implicit none

       type (dm_info), pointer :: dminfo
       type (block_type), pointer :: blockList
       type (MPAS_streamManager_type), pointer :: streamManager
       integer, intent(out) :: ierr

       logical, pointer :: config_positive_definite


       call mpas_log_write('')
       call mpas_log_write('Checking consistency of dynamics settings...')

       !
       ! Check that config_positive_definite == .false., since the positive-definite advection
       ! option is currently unimplemented.
       !
       nullify(config_positive_definite)
       call mpas_pool_get_config(blocklist % configs, 'config_positive_definite', config_positive_definite)

       if (config_positive_definite) then
           call mpas_log_write('The positive definite advection option is currently unimplemented, and', &
                               messageType=MPAS_LOG_WARN)
           call mpas_log_write('setting config_positive_definite = true will enable monotonic advection.', &
                               messageType=MPAS_LOG_WARN)
           call mpas_log_write('Please remove the specification of config_positive_definite from the', &
                               messageType=MPAS_LOG_WARN)
           call mpas_log_write('&nhyd_model namelist group.', &
                               messageType=MPAS_LOG_WARN)
       end if

       call mpas_log_write(' ----- done checking dynamics settings -----')
       call mpas_log_write('')

       ierr = 0

   end subroutine mpas_atm_dynamics_checks


   !----------------------------------------------------------------------------
   !  routine MPAS_atm_dynamics_init
   !
   !> \brief Initialize the dynamics
   !> \date  28 July 2021
   !> \details
   !>  Prepare the dynamics component of MPAS-Atmosphere for time integration.
   !>  This may involve, for example, allocating dynamics-local storage or
   !>  initializing data structures used throughout the dynamics. Since this
   !>  routine is called once before the first integration step, the work done
   !>  by this routine is generally persistent across all calls to the dynamical
   !>  core, in contrast to work that is performed at the beginning of each call
   !>  to the dynamical core.
   !
   !----------------------------------------------------------------------------
   subroutine mpas_atm_dynamics_init(domain)

      implicit none

      type (domain_type), intent(inout) :: domain

#ifdef MPAS_CAM_DYCORE
      ! Used in allocating scratch fields for physics tendencies
      type (mpas_pool_type), pointer :: tend_physics
      type (field2DReal), pointer :: tend_ru_physicsField, tend_rtheta_physicsField, tend_rho_physicsField
#endif

#ifdef MPAS_OPENACC
      type (mpas_pool_type), pointer :: mesh

      real (kind=RKIND), dimension(:), pointer :: dvEdge
      integer, dimension(:,:), pointer :: cellsOnCell
      integer, dimension(:,:), pointer :: cellsOnEdge
      integer, dimension(:,:), pointer :: advCellsForEdge
      integer, dimension(:,:), pointer :: edgesOnCell
      integer, dimension(:), pointer :: nAdvCellsForEdge
      integer, dimension(:), pointer :: nEdgesOnCell
      real (kind=RKIND), dimension(:,:), pointer :: adv_coefs
      real (kind=RKIND), dimension(:,:), pointer :: adv_coefs_3rd
      real (kind=RKIND), dimension(:,:), pointer :: edgesOnCell_sign
      real (kind=RKIND), dimension(:), pointer :: invAreaCell
      integer, dimension(:), pointer :: bdyMaskCell
      integer, dimension(:), pointer :: bdyMaskEdge
#endif


#ifdef MPAS_CAM_DYCORE
      nullify(tend_physics)
      call mpas_pool_get_subpool(domain % blocklist % structs, 'tend_physics', tend_physics)

      call mpas_pool_get_field(tend_physics, 'tend_rtheta_physics', tend_rtheta_physicsField)
      call mpas_allocate_scratch_field(tend_rtheta_physicsField)

      call mpas_pool_get_field(tend_physics, 'tend_rho_physics', tend_rho_physicsField)
      call mpas_allocate_scratch_field(tend_rho_physicsField)

      call mpas_pool_get_field(tend_physics, 'tend_ru_physics', tend_ru_physicsField)
      call mpas_allocate_scratch_field(tend_ru_physicsField)
#endif

#ifdef MPAS_OPENACC
      nullify(mesh)
      call mpas_pool_get_subpool(domain % blocklist % structs, 'mesh', mesh)

      call mpas_pool_get_array(mesh, 'dvEdge', dvEdge)
      !$acc enter data copyin(dvEdge)

      call mpas_pool_get_array(mesh, 'cellsOnCell', cellsOnCell)
      !$acc enter data copyin(cellsOnCell)

      call mpas_pool_get_array(mesh, 'cellsOnEdge', cellsOnEdge)
      !$acc enter data copyin(cellsOnEdge)

      call mpas_pool_get_array(mesh, 'advCellsForEdge', advCellsForEdge)
      !$acc enter data copyin(advCellsForEdge)

      call mpas_pool_get_array(mesh, 'edgesOnCell', edgesOnCell)
      !$acc enter data copyin(edgesOnCell)

      call mpas_pool_get_array(mesh, 'nAdvCellsForEdge', nAdvCellsForEdge)
      !$acc enter data copyin(nAdvCellsForEdge)

      call mpas_pool_get_array(mesh, 'nEdgesOnCell', nEdgesOnCell)
      !$acc enter data copyin(nEdgesOnCell)

      call mpas_pool_get_array(mesh, 'adv_coefs', adv_coefs)
      !$acc enter data copyin(adv_coefs)

      call mpas_pool_get_array(mesh, 'adv_coefs_3rd', adv_coefs_3rd)
      !$acc enter data copyin(adv_coefs_3rd)

      call mpas_pool_get_array(mesh, 'edgesOnCell_sign', edgesOnCell_sign)
      !$acc enter data copyin(edgesOnCell_sign)

      call mpas_pool_get_array(mesh, 'invAreaCell', invAreaCell)
      !$acc enter data copyin(invAreaCell)

      call mpas_pool_get_array(mesh, 'bdyMaskCell', bdyMaskCell)
      !$acc enter data copyin(bdyMaskCell)

      call mpas_pool_get_array(mesh, 'bdyMaskEdge', bdyMaskEdge)
      !$acc enter data copyin(bdyMaskEdge)
#endif

   end subroutine mpas_atm_dynamics_init


   !----------------------------------------------------------------------------
   !  routine MPAS_atm_dynamics_finalize
   !
   !> \brief Finalize the dynamics
   !> \author Michael Duda
   !> \date   28 July 2021
   !> \details
   !>  Finalizes the dynamics component of MPAS-Atmosphere by, for example,
   !>  freeing up dynamics-local memory and shut down infrastructure used only
   !>  in the dynamics component of MPAS-Atmosphere. This routine is called once
   !>  after the last integration step, and the work done here is usually the
   !>  inverse of that done in the mpas_atm_dynamics_init routine (e.g.,
   !>  deallocating memory that was allocated by mpas_atm_dynamics_init).
   !
   !----------------------------------------------------------------------------
   subroutine mpas_atm_dynamics_finalize(domain)

      implicit none

      type (domain_type), intent(inout) :: domain

#ifdef MPAS_CAM_DYCORE
      ! Used in allocating scratch fields for physics tendencies
      type (mpas_pool_type), pointer :: tend_physics
      type (field2DReal), pointer :: tend_ru_physicsField, tend_rtheta_physicsField, tend_rho_physicsField
#endif

#ifdef MPAS_OPENACC
      type (mpas_pool_type), pointer :: mesh

      real (kind=RKIND), dimension(:), pointer :: dvEdge
      integer, dimension(:,:), pointer :: cellsOnCell
      integer, dimension(:,:), pointer :: cellsOnEdge
      integer, dimension(:,:), pointer :: advCellsForEdge
      integer, dimension(:,:), pointer :: edgesOnCell
      integer, dimension(:), pointer :: nAdvCellsForEdge
      integer, dimension(:), pointer :: nEdgesOnCell
      real (kind=RKIND), dimension(:,:), pointer :: adv_coefs
      real (kind=RKIND), dimension(:,:), pointer :: adv_coefs_3rd
      real (kind=RKIND), dimension(:,:), pointer :: edgesOnCell_sign
      real (kind=RKIND), dimension(:), pointer :: invAreaCell
      integer, dimension(:), pointer :: bdyMaskCell
      integer, dimension(:), pointer :: bdyMaskEdge
#endif


#ifdef MPAS_CAM_DYCORE
      nullify(tend_physics)
      call mpas_pool_get_subpool(domain % blocklist % structs, 'tend_physics', tend_physics)

      call mpas_pool_get_field(tend_physics, 'tend_rtheta_physics', tend_rtheta_physicsField)
      call mpas_deallocate_scratch_field(tend_rtheta_physicsField)

      call mpas_pool_get_field(tend_physics, 'tend_rho_physics', tend_rho_physicsField)
      call mpas_deallocate_scratch_field(tend_rho_physicsField)

      call mpas_pool_get_field(tend_physics, 'tend_ru_physics', tend_ru_physicsField)
      call mpas_deallocate_scratch_field(tend_ru_physicsField)
#endif

#ifdef MPAS_OPENACC
      nullify(mesh)
      call mpas_pool_get_subpool(domain % blocklist % structs, 'mesh', mesh)

      call mpas_pool_get_array(mesh, 'dvEdge', dvEdge)
      !$acc exit data delete(dvEdge)

      call mpas_pool_get_array(mesh, 'cellsOnCell', cellsOnCell)
      !$acc exit data delete(cellsOnCell)

      call mpas_pool_get_array(mesh, 'cellsOnEdge', cellsOnEdge)
      !$acc exit data delete(cellsOnEdge)

      call mpas_pool_get_array(mesh, 'advCellsForEdge', advCellsForEdge)
      !$acc exit data delete(advCellsForEdge)

      call mpas_pool_get_array(mesh, 'edgesOnCell', edgesOnCell)
      !$acc exit data delete(edgesOnCell)

      call mpas_pool_get_array(mesh, 'nAdvCellsForEdge', nAdvCellsForEdge)
      !$acc exit data delete(nAdvCellsForEdge)

      call mpas_pool_get_array(mesh, 'nEdgesOnCell', nEdgesOnCell)
      !$acc exit data delete(nEdgesOnCell)

      call mpas_pool_get_array(mesh, 'adv_coefs', adv_coefs)
      !$acc exit data delete(adv_coefs)

      call mpas_pool_get_array(mesh, 'adv_coefs_3rd', adv_coefs_3rd)
      !$acc exit data delete(adv_coefs_3rd)

      call mpas_pool_get_array(mesh, 'edgesOnCell_sign', edgesOnCell_sign)
      !$acc exit data delete(edgesOnCell_sign)

      call mpas_pool_get_array(mesh, 'invAreaCell', invAreaCell)
      !$acc exit data delete(invAreaCell)

      call mpas_pool_get_array(mesh, 'bdyMaskCell', bdyMaskCell)
      !$acc exit data delete(bdyMaskCell)

      call mpas_pool_get_array(mesh, 'bdyMaskEdge', bdyMaskEdge)
      !$acc exit data delete(bdyMaskEdge)
#endif

   end subroutine mpas_atm_dynamics_finalize


   subroutine atm_timestep(domain, dt, nowTime, itimestep, exchange_halo_group)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Advance model state forward in time by the specified time step
   !
   ! Input: domain - current model state in time level 1 (e.g., time_levs(1)state%h(:,:)) 
   !                 plus grid meta-data
   ! Output: domain - upon exit, time level 2 (e.g., time_levs(2)%state%h(:,:)) contains 
   !                  model state advanced forward in time by dt seconds
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 

      implicit none

      type (domain_type), intent(inout) :: domain
      real (kind=RKIND), intent(in) :: dt
      type (MPAS_Time_type), intent(in) :: nowTime
      integer, intent(in) :: itimestep
      procedure (halo_exchange_routine) :: exchange_halo_group


      type (MPAS_Time_type) :: currTime
      type (MPAS_TimeInterval_type) :: dtInterval
      character (len=StrKIND), pointer :: xtime
      character (len=StrKIND) :: xtime_new
      real (kind=RKIND), pointer :: Time
      real (kind=RKIND) :: Time_new
      type (mpas_pool_type), pointer :: state
      character (len=StrKIND), pointer :: config_time_integration


      clock => domain % clock
      block => domain % blocklist

      call mpas_pool_get_config(block % configs, 'config_time_integration', config_time_integration)
      call mpas_pool_get_config(block % configs, 'config_apply_lbcs', config_apply_lbcs)

      if (trim(config_time_integration) == 'SRK3') then
         call atm_srk3(domain, dt, itimestep, exchange_halo_group)
      else
         call mpas_log_write('Unknown time integration option '//trim(config_time_integration), messageType=MPAS_LOG_ERR)
         call mpas_log_write('Currently, only ''SRK3'' is supported.', messageType=MPAS_LOG_CRIT)
      end if

      call mpas_set_timeInterval(dtInterval, dt=dt)
      currTime = nowTime + dtInterval
      call mpas_get_time(currTime, dateTimeString=xtime_new)

      call mpas_pool_get_subpool(block % structs, 'state', state)
      call mpas_pool_get_array(state, 'xtime', xtime, 2)
      xtime = xtime_new

      ! Get CF-compliant time at current timestep
      call mpas_pool_get_array(state, 'Time', Time, 1)
      Time_new = Time + dt

      ! Write CF-compliant time for advanced timestep
      call mpas_pool_get_array(state, 'Time', Time, 2)
      Time = Time_new

   end subroutine atm_timestep


   subroutine atm_srk3(domain, dt, itimestep, exchange_halo_group)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Advance model state forward in time by the specified time step using 
   !   time-split RK3 scheme
   !
   ! Nonhydrostatic atmospheric solver
   !
   ! Input: domain - current model state in time level 1 (e.g., time_levs(1)state%h(:,:)) 
   !                 plus grid meta-data and some diagnostics of state.
   ! Output: domain - upon exit, time level 2 (e.g., time_levs(2)%state%h(:,:)) contains 
   !                  model state advanced forward in time by dt seconds,
   !                  and some diagnostics in diag 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 

      implicit none

      type (domain_type), intent(inout) :: domain
      real (kind=RKIND), intent(in) :: dt
      integer, intent(in) :: itimestep
      procedure (halo_exchange_routine) :: exchange_halo_group

      integer :: thread
      integer :: iCell, k, iEdge

      integer, pointer :: nThreads
      integer, dimension(:), pointer :: cellThreadStart, cellThreadEnd
      integer, dimension(:), pointer :: cellSolveThreadStart, cellSolveThreadEnd
      integer, dimension(:), pointer :: edgeThreadStart, edgeThreadEnd
      integer, dimension(:), pointer :: edgeSolveThreadStart, edgeSolveThreadEnd
      integer, dimension(:), pointer :: vertexThreadStart, vertexThreadEnd
      integer, dimension(:), pointer :: vertexSolveThreadStart, vertexSolveThreadEnd

      integer :: rk_step, number_of_sub_steps
      integer :: iScalar

      real (kind=RKIND), dimension(3) :: rk_timestep, rk_sub_timestep
      integer, dimension(3) :: number_sub_steps
      integer :: small_step

      !  additions for splitting scalar transport from dynamics, WCS 18 November 2014
      logical, pointer :: config_split_dynamics_transport
      integer, pointer :: config_dynamics_split
      integer :: dynamics_substep, dynamics_split
      real (kind=RKIND) :: dt_dynamics

      integer, pointer :: config_number_of_sub_steps
      integer, pointer :: config_time_integration_order
      logical, pointer :: config_scalar_advection
      logical, pointer :: config_positive_definite
      logical, pointer :: config_monotonic
      real (kind=RKIND), pointer :: config_dt
      character (len=StrKIND), pointer :: config_microp_scheme
      character (len=StrKIND), pointer :: config_convection_scheme

      integer, pointer :: num_scalars, index_qv, nCells, nCellsSolve, nEdges, nEdgesSolve, nVertices, nVerticesSolve, nVertLevels
      integer, pointer :: index_qc, index_qr, index_qi, index_qs, index_qg, index_nr, index_ni, index_nc, index_nifa, index_nwfa

      character(len=StrKIND), pointer :: config_IAU_option

      type (mpas_pool_type), pointer :: state
      type (mpas_pool_type), pointer :: diag
      type (mpas_pool_type), pointer :: diag_physics
      type (mpas_pool_type), pointer :: mesh
      type (mpas_pool_type), pointer :: tend
      type (mpas_pool_type), pointer :: tend_physics => null()
      type (mpas_pool_type), pointer :: lbc  ! regional_MPAS addition

      real (kind=RKIND), dimension(:,:), pointer :: w
      real (kind=RKIND), dimension(:,:), pointer :: u, uReconstructZonal, uReconstructMeridional, uReconstructX, uReconstructY, uReconstructZ
      real (kind=RKIND), dimension(:,:,:), pointer :: scalars, scalars_1, scalars_2

      real (kind=RKIND), dimension(:,:), pointer :: rqvdynten, rthdynten, theta_m
      real (kind=RKIND) :: theta_local, fac_m

#ifndef MPAS_CAM_DYCORE
      ! Used in allocating scratch fields for physics tendencies
      type (field2DReal), pointer :: tend_ru_physicsField, tend_rtheta_physicsField, tend_rho_physicsField
#endif

      ! For ITM and PV tendency diagnostics
      logical, pointer :: config_tend, config_pv_tend
      real (kind=RKIND), dimension(:,:), pointer :: acc_u_tend_dyn_small, acc_u_tend_dyn_small_ReconstructZonal, &
                                                    acc_u_tend_dyn_small_ReconstructMeridional, &
                                                    acc_u_tend_dyn_small_ReconstructX, acc_u_tend_dyn_small_ReconstructY, &
                                                    acc_u_tend_dyn_small_ReconstructZ
      real (kind=RKIND), dimension(:,:), pointer :: acc_qv_tend_dyn_large, dqv_dt_dyn
      !end MW 

      real (kind=RKIND)  :: time_dyn_step
      logical, parameter :: debug = .false.


      !
      ! Retrieve configuration options
      !
      call mpas_pool_get_config(block % configs, 'config_number_of_sub_steps', config_number_of_sub_steps)
      call mpas_pool_get_config(block % configs, 'config_time_integration_order', config_time_integration_order)
      call mpas_pool_get_config(block % configs, 'config_scalar_advection', config_scalar_advection)
      call mpas_pool_get_config(block % configs, 'config_positive_definite', config_positive_definite)
      call mpas_pool_get_config(block % configs, 'config_monotonic', config_monotonic)
      call mpas_pool_get_config(block % configs, 'config_dt', config_dt)
      call mpas_pool_get_config(block % configs, 'config_IAU_option', config_IAU_option)
      !  config variables for dynamics-transport splitting, WCS 18 November 2014
      call mpas_pool_get_config(block % configs, 'config_split_dynamics_transport', config_split_dynamics_transport)
      call mpas_pool_get_config(block % configs, 'config_dynamics_split_steps', config_dynamics_split)
      !  config variables for cloud microphysics
#ifdef DO_PHYSICS
      call mpas_pool_get_config(block % configs, 'config_microp_scheme', config_microp_scheme)
      call mpas_pool_get_config(block % configs, 'config_convection_scheme', config_convection_scheme)
#endif
      call mpas_pool_get_config(block % configs, 'config_tend', config_tend)        ! MW: for tendency diagnostics
      call mpas_pool_get_config(block % configs, 'config_pv_tend', config_pv_tend)  ! MC: for PV tendency diagnostics 

      !
      ! Retrieve field structures
      !
      call mpas_pool_get_subpool(block % structs, 'state', state)
      call mpas_pool_get_subpool(block % structs, 'mesh', mesh)
      call mpas_pool_get_subpool(block % structs, 'diag', diag)
      call mpas_pool_get_subpool(block % structs, 'tend', tend)
      call mpas_pool_get_subpool(block % structs, 'tend_physics', tend_physics)
#ifdef DO_PHYSICS
      call mpas_pool_get_subpool(block % structs, 'diag_physics', diag_physics)
#endif

      !
      ! Retrieve dimensions
      ! Note: nCellsSolve and nVerticesSolve are not currently used in this function
      !
      call mpas_pool_get_dimension(mesh, 'nCells', nCells)
      call mpas_pool_get_dimension(mesh, 'nCellsSolve', nCellsSolve)
      call mpas_pool_get_dimension(mesh, 'nEdges', nEdges)
      call mpas_pool_get_dimension(mesh, 'nVertices', nVertices)
      call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)

      !call mpas_pool_get_dimension(mesh, 'nCellsSolve', nCellsSolve)
      call mpas_pool_get_dimension(mesh, 'nEdgesSolve', nEdgesSolve)
      !call mpas_pool_get_dimension(mesh, 'nVerticesSolve', nVerticesSolve)

      call mpas_pool_get_dimension(block % dimensions, 'nThreads', nThreads)

      call mpas_pool_get_dimension(block % dimensions, 'cellThreadStart', cellThreadStart)
      call mpas_pool_get_dimension(block % dimensions, 'cellThreadEnd', cellThreadEnd)
      call mpas_pool_get_dimension(block % dimensions, 'cellSolveThreadStart', cellSolveThreadStart)
      call mpas_pool_get_dimension(block % dimensions, 'cellSolveThreadEnd', cellSolveThreadEnd)

      call mpas_pool_get_dimension(block % dimensions, 'vertexThreadStart', vertexThreadStart)
      call mpas_pool_get_dimension(block % dimensions, 'vertexThreadEnd', vertexThreadEnd)
      call mpas_pool_get_dimension(block % dimensions, 'vertexSolveThreadStart', vertexSolveThreadStart)
      call mpas_pool_get_dimension(block % dimensions, 'vertexSolveThreadEnd', vertexSolveThreadEnd)

      call mpas_pool_get_dimension(block % dimensions, 'edgeThreadStart', edgeThreadStart)
      call mpas_pool_get_dimension(block % dimensions, 'edgeThreadEnd', edgeThreadEnd)
      call mpas_pool_get_dimension(block % dimensions, 'edgeSolveThreadStart', edgeSolveThreadStart)
      call mpas_pool_get_dimension(block % dimensions, 'edgeSolveThreadEnd', edgeSolveThreadEnd)


#ifdef DO_PHYSICS
      call mpas_pool_get_dimension(state, 'index_qv', index_qv)
#endif
      if (config_apply_lbcs) then
         call mpas_pool_get_dimension(state, 'num_scalars', num_scalars)
         call mpas_pool_get_dimension(state, 'index_qv', index_qv)
         call mpas_pool_get_dimension(state, 'index_qc', index_qc)
         call mpas_pool_get_dimension(state, 'index_qr', index_qr)
         call mpas_pool_get_dimension(state, 'index_qi', index_qi)
         call mpas_pool_get_dimension(state, 'index_qs', index_qs)
         call mpas_pool_get_dimension(state, 'index_qg', index_qg)  
         call mpas_pool_get_dimension(state, 'index_nr', index_nr)
         call mpas_pool_get_dimension(state, 'index_ni', index_ni)
         call mpas_pool_get_dimension(state, 'index_nc', index_nc)
         call mpas_pool_get_dimension(state, 'index_nifa', index_nifa)
         call mpas_pool_get_dimension(state, 'index_nwfa', index_nwfa)
      endif

      !
      ! allocate storage for physics tendency save
      !
      allocate(qtot(nVertLevels,nCells+1))
      qtot(:,nCells+1) = 0.0_RKIND

#ifndef MPAS_CAM_DYCORE
      call mpas_pool_get_field(tend_physics, 'tend_rtheta_physics', tend_rtheta_physicsField)
      call mpas_allocate_scratch_field(tend_rtheta_physicsField)

      call mpas_pool_get_field(tend_physics, 'tend_rho_physics', tend_rho_physicsField)
      call mpas_allocate_scratch_field(tend_rho_physicsField)

      call mpas_pool_get_field(tend_physics, 'tend_ru_physics', tend_ru_physicsField)
      call mpas_allocate_scratch_field(tend_ru_physicsField)
#endif

      call mpas_pool_get_array(tend_physics, 'tend_rtheta_physics', tend_rtheta_physics)
      tend_rtheta_physics(:,nCells+1) = 0.0_RKIND
      call mpas_pool_get_array(tend_physics, 'tend_rho_physics', tend_rho_physics)
      tend_rho_physics(:,nCells+1) = 0.0_RKIND
      call mpas_pool_get_array(tend_physics, 'tend_ru_physics', tend_ru_physics)
      tend_ru_physics(:,nEdges+1) = 0.0_RKIND

      !
      ! Initialize RK weights
      !

      dynamics_split = config_dynamics_split
      if (config_split_dynamics_transport) then
        dt_dynamics = dt/real(dynamics_split)
        call mpas_log_write(' split dynamics-transport integration $i', intArgs=(/dynamics_split/))
      else
        dynamics_split = 1
        dt_dynamics = dt
        call mpas_log_write(' coupled RK3 dynamics-transport integration ')
      end if
      if (.not. config_scalar_advection ) call mpas_log_write(' scalar advection turned off ')

      number_of_sub_steps = config_number_of_sub_steps

      if(config_time_integration_order == 3) then

        rk_timestep(1) = dt_dynamics/3.
        rk_timestep(2) = dt_dynamics/2.
        rk_timestep(3) = dt_dynamics

        rk_sub_timestep(1) = dt_dynamics/3.
        rk_sub_timestep(2) = dt_dynamics/real(number_of_sub_steps)
        rk_sub_timestep(3) = dt_dynamics/real(number_of_sub_steps)

        number_sub_steps(1) = 1
        number_sub_steps(2) = max(1,number_of_sub_steps/2)
        number_sub_steps(3) = number_of_sub_steps

      else if (config_time_integration_order == 2) then

        rk_timestep(1) = dt_dynamics/2.
        rk_timestep(2) = dt_dynamics/2.
        rk_timestep(3) = dt_dynamics

        rk_sub_timestep(1) = dt_dynamics/real(number_of_sub_steps)
        rk_sub_timestep(2) = dt_dynamics/real(number_of_sub_steps)
        rk_sub_timestep(3) = dt_dynamics/real(number_of_sub_steps)

        number_sub_steps(1) = max(1,number_of_sub_steps/2)
        number_sub_steps(2) = max(1,number_of_sub_steps/2)
        number_sub_steps(3) = number_of_sub_steps

      end if

      !
      ! Communicate halos for theta_m, scalars, pressure_p, and rtheta_p
      !
      call exchange_halo_group(domain, 'dynamics:theta_m,scalars,pressure_p,rtheta_p')

      call mpas_timer_start('atm_rk_integration_setup')

!$OMP PARALLEL DO
      do thread=1,nThreads
         call atm_rk_integration_setup(state, diag, &
                                       cellThreadStart(thread), cellThreadEnd(thread), &
                                       vertexThreadStart(thread), vertexThreadEnd(thread), &
                                       edgeThreadStart(thread), edgeThreadEnd(thread), &
                                       cellSolveThreadStart(thread), cellSolveThreadEnd(thread), &
                                       vertexSolveThreadStart(thread), vertexSolveThreadEnd(thread), &
                                       edgeSolveThreadStart(thread), edgeSolveThreadEnd(thread))
      end do
!$OMP END PARALLEL DO

      call mpas_timer_stop('atm_rk_integration_setup')

      call mpas_timer_start('atm_compute_moist_coefficients')

!$OMP PARALLEL DO
      do thread=1,nThreads
         call atm_compute_moist_coefficients( block % dimensions, state, diag, mesh, &     !MGD could do away with dimensions arg
                                              cellThreadStart(thread), cellThreadEnd(thread), &
                                              vertexThreadStart(thread), vertexThreadEnd(thread), &
                                              edgeThreadStart(thread), edgeThreadEnd(thread), &
                                              cellSolveThreadStart(thread), cellSolveThreadEnd(thread), &
                                              vertexSolveThreadStart(thread), vertexSolveThreadEnd(thread), &
                                              edgeSolveThreadStart(thread), edgeSolveThreadEnd(thread))
      end do
!$OMP END PARALLEL DO

      call mpas_timer_stop('atm_compute_moist_coefficients')

#ifdef DO_PHYSICS
      call mpas_timer_start('physics_get_tend')
      rk_step = 1
      dynamics_substep = 1
      call physics_get_tend( block, mesh, state, diag, tend, tend_physics, &
                             diag_physics, &                                       ! MW on ITM for GWDO tends
                             block % configs, rk_step, dynamics_substep, &
                             tend_ru_physics, tend_rtheta_physics, tend_rho_physics, &
                             exchange_halo_group )
      call mpas_timer_stop('physics_get_tend')
#else
#ifndef MPAS_CAM_DYCORE
      !
      ! If no physics are being used, simply zero-out the physics tendency fields
      !
      tend_ru_physics(:,:) = 0.0_RKIND
      tend_rtheta_physics(:,:) = 0.0_RKIND
      tend_rho_physics(:,:) = 0.0_RKIND
#endif
#endif

      !
      ! IAU - Incremental Analysis Update
      !
      if (trim(config_IAU_option) /= 'off') then
         call atm_add_tend_anal_incr(block % configs, block % structs, itimestep, dt, &
                                     tend_ru_physics, tend_rtheta_physics, tend_rho_physics)
      end if


      DYNAMICS_SUBSTEPS : do dynamics_substep = 1, dynamics_split

         !  Compute the coefficients for the vertically implicit solve in the acoustic step.
         !  These coefficients will work for the first acoustic step in all cases.
         call mpas_timer_start('atm_compute_vert_imp_coefs')

         rk_step = 1
!$OMP PARALLEL DO
         do thread=1,nThreads
            call atm_compute_vert_imp_coefs( state, mesh, diag, block % configs, nVertLevels, rk_sub_timestep(rk_step), &
                                       cellThreadStart(thread), cellThreadEnd(thread), &
                                       edgeThreadStart(thread), edgeThreadEnd(thread), &
                                       cellSolveThreadStart(thread), cellSolveThreadEnd(thread), &
                                       edgeSolveThreadStart(thread), edgeSolveThreadEnd(thread))
         end do
!$OMP END PARALLEL DO
         call mpas_timer_stop('atm_compute_vert_imp_coefs')

         call exchange_halo_group(domain, 'dynamics:exner')


         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
         ! BEGIN Runge-Kutta loop 
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 

         RK3_DYNAMICS : do rk_step = 1, 3  ! Runge-Kutta loop

            ! recompute vertically implicit coefficients if necessary
            if( (config_time_integration_order == 3) .and. (rk_step == 2)) then

              !  Compute the coefficients for the vertically implicit solve in the acoustic step.
              !  These coefficients will work for the 2nd and 3rd acoustic steps (dt is the same for both).
!$OMP PARALLEL DO
              do thread=1,nThreads
                 call atm_compute_vert_imp_coefs( state, mesh, diag, block % configs, nVertLevels, rk_sub_timestep(rk_step), &
                                                  cellThreadStart(thread), cellThreadEnd(thread), &
                                                  edgeThreadStart(thread), edgeThreadEnd(thread), &
                                                  cellSolveThreadStart(thread), cellSolveThreadEnd(thread), &
                                                  edgeSolveThreadStart(thread), edgeSolveThreadEnd(thread))
              end do
!$OMP END PARALLEL DO
            end if  

            call mpas_timer_start('atm_compute_dyn_tend')
   
            allocate(delsq_theta(nVertLevels,nCells+1))
            delsq_theta(:,nCells+1) = 0.0_RKIND
            allocate(delsq_w(nVertLevels,nCells+1))
            delsq_w(:,nCells+1) = 0.0_RKIND
!!            allocate(qtot(nVertLevels,nCells+1))  ! initializing this earlier in solution sequence
            allocate(delsq_divergence(nVertLevels,nCells+1))
            delsq_divergence(:,nCells+1) = 0.0_RKIND
            allocate(delsq_u(nVertLevels,nEdges+1))
            delsq_u(:,nEdges+1) = 0.0_RKIND
!!            allocate(delsq_circulation(nVertLevels,nVertices+1))  ! no longer used -> removed 
            allocate(delsq_vorticity(nVertLevels,nVertices+1))
            delsq_vorticity(:,nVertices+1) = 0.0_RKIND
            allocate(dpdz(nVertLevels,nCells+1))
            dpdz(:,nCells+1) = 0.0_RKIND

!$OMP PARALLEL DO
            do thread=1,nThreads
               call atm_compute_dyn_tend( tend, tend_physics, state, diag, mesh, block % configs, nVertLevels, rk_step, dt, & 
                                          rk_timestep(rk_step), &                               ! MW on ITM
                                          cellThreadStart(thread), cellThreadEnd(thread), &
                                          vertexThreadStart(thread), vertexThreadEnd(thread), &
                                          edgeThreadStart(thread), edgeThreadEnd(thread), &
                                          cellSolveThreadStart(thread), cellSolveThreadEnd(thread), &
                                          vertexSolveThreadStart(thread), vertexSolveThreadEnd(thread), &
                                          edgeSolveThreadStart(thread), edgeSolveThreadEnd(thread))
            end do
!$OMP END PARALLEL DO

            deallocate(delsq_theta)
            deallocate(delsq_w)
!!            deallocate(qtot)  ! deallocation after dynamics step complete, see below
            deallocate(delsq_divergence)
            deallocate(delsq_u)
!!            deallocate(delsq_circulation)    ! no longer used -> removed 
            deallocate(delsq_vorticity)
            deallocate(dpdz)
   
            call mpas_timer_stop('atm_compute_dyn_tend')


            !***********************************
            !  need tendencies at all edges of owned cells -
            !  we are solving for all edges of owned cells to minimize communications
            !  during the acoustic substeps
            !***********************************

! tend_u
            call exchange_halo_group(domain, 'dynamics:tend_u')
   
            call mpas_timer_start('small_step_prep')
   
!$OMP PARALLEL DO
            do thread=1,nThreads
               call atm_set_smlstep_pert_variables( tend, diag, mesh, block % configs, &
                                          cellThreadStart(thread), cellThreadEnd(thread), &
                                          edgeThreadStart(thread), edgeThreadEnd(thread), &
                                          cellSolveThreadStart(thread), cellSolveThreadEnd(thread), &
                                          edgeSolveThreadStart(thread), edgeSolveThreadEnd(thread))
            end do
!$OMP END PARALLEL DO
            call mpas_timer_stop('small_step_prep')

          
!------------------------------------------------------------------------------------------------------------------------

            if (config_apply_lbcs) then  ! adjust boundary tendencies for regional_MPAS dry dynamics in the specified zone
   
               allocate(ru_driving_tend(nVertLevels,nEdges+1))
               allocate(rt_driving_tend(nVertLevels,nCells+1))
               allocate(rho_driving_tend(nVertLevels,nCells+1))
               ru_driving_tend(1:nVertLevels,1:nEdges+1) =  mpas_atm_get_bdy_tend( clock, block, nVertLevels, nEdges, 'ru', 0.0_RKIND )
               rt_driving_tend(1:nVertLevels,1:nCells+1) =  mpas_atm_get_bdy_tend( clock, block, nVertLevels, nCells, 'rtheta_m', 0.0_RKIND )
               rho_driving_tend(1:nVertLevels,1:nCells+1) =  mpas_atm_get_bdy_tend( clock, block, nVertLevels, nCells, 'rho_zz', 0.0_RKIND )
!$OMP PARALLEL DO
               do thread=1,nThreads
                  call atm_bdy_adjust_dynamics_speczone_tend( tend, mesh, block % configs, nVertLevels,                 &
                                                              ru_driving_tend, rt_driving_tend, rho_driving_tend,       &
                                                              cellThreadStart(thread), cellThreadEnd(thread),           &
                                                              edgeThreadStart(thread), edgeThreadEnd(thread),           &
                                                              cellSolveThreadStart(thread), cellSolveThreadEnd(thread), &
                                                              edgeSolveThreadStart(thread), edgeSolveThreadEnd(thread) )
               end do
!$OMP END PARALLEL DO

               deallocate(ru_driving_tend)
               deallocate(rt_driving_tend)
               deallocate(rho_driving_tend)

! -------- next, add in the tendencies for the horizontal filters and Rayleigh damping.  We will keep the spec zone and relax zone adjustments separate for now...

               allocate(ru_driving_values(nVertLevels,nEdges+1))
               allocate(rt_driving_values(nVertLevels,nCells+1))
               allocate(rho_driving_values(nVertLevels,nCells+1))

               time_dyn_step = dt_dynamics*real(dynamics_substep-1) + rk_timestep(rk_step)
               ru_driving_values(1:nVertLevels,1:nEdges+1) =  mpas_atm_get_bdy_state( clock, block, nVertLevels, nEdges, 'ru', time_dyn_step )
               rt_driving_values(1:nVertLevels,1:nCells+1) =  mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'rtheta_m', time_dyn_step )
               rho_driving_values(1:nVertLevels,1:nCells+1) =  mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'rho_zz', time_dyn_step )

!$OMP PARALLEL DO
               do thread=1,nThreads
                  call atm_bdy_adjust_dynamics_relaxzone_tend( block % configs, tend, state, diag, mesh, nVertLevels, dt,  &
                                                               ru_driving_values, rt_driving_values, rho_driving_values,   &
                                                               cellThreadStart(thread), cellThreadEnd(thread),             &
                                                               edgeThreadStart(thread), edgeThreadEnd(thread),             &
                                                               cellSolveThreadStart(thread), cellSolveThreadEnd(thread),   &
                                                               edgeSolveThreadStart(thread), edgeSolveThreadEnd(thread) )
               end do
!$OMP END PARALLEL DO

               deallocate(ru_driving_values)
               deallocate(rt_driving_values)
               deallocate(rho_driving_values)

            end if  ! regional_MPAS addition

!------------------------------------------------------------------------------------------------------------------------

            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            ! begin acoustic steps loop
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            do small_step = 1, number_sub_steps(rk_step)

               call exchange_halo_group(domain, 'dynamics:rho_pp')

               call mpas_timer_start('atm_advance_acoustic_step')

!$OMP PARALLEL DO
               do thread=1,nThreads
                  call atm_advance_acoustic_step( state, diag, tend,  mesh, block % configs, nCells, nVertLevels, &
                                          rk_sub_timestep(rk_step), small_step, &
                                          cellThreadStart(thread), cellThreadEnd(thread), &
                                          vertexThreadStart(thread), vertexThreadEnd(thread), &
                                          edgeThreadStart(thread), edgeThreadEnd(thread), &
                                          cellSolveThreadStart(thread), cellSolveThreadEnd(thread), &
                                          vertexSolveThreadStart(thread), vertexSolveThreadEnd(thread), &
                                          edgeSolveThreadStart(thread), edgeSolveThreadEnd(thread))
               end do
!$OMP END PARALLEL DO

               call mpas_timer_stop('atm_advance_acoustic_step')

  
! rtheta_pp
! This is the only communications needed during the acoustic steps because we solve for u on all edges of owned cells

               call exchange_halo_group(domain, 'dynamics:rtheta_pp')

!  complete update of horizontal momentum by including 3d divergence damping at the end of the acoustic step

               call mpas_timer_start('atm_divergence_damping_3d')

!$OMP PARALLEL DO
               do thread=1,nThreads
                  call atm_divergence_damping_3d( state, diag, mesh, block % configs, rk_sub_timestep(rk_step), &
                                                  edgeThreadStart(thread), edgeThreadEnd(thread) )
               end do
!$OMP END PARALLEL DO

               call mpas_timer_stop('atm_divergence_damping_3d')

            end do  ! end of acoustic steps loop

            !
            ! Communicate halos for rw_p[1,2], ru_p[1,2], rho_pp[1,2], rtheta_pp[2]
            !
            call exchange_halo_group(domain, 'dynamics:rw_p,ru_p,rho_pp,rtheta_pp')

            call mpas_timer_start('atm_recover_large_step_variables')

!$OMP PARALLEL DO
            do thread=1,nThreads
               call atm_recover_large_step_variables( state, diag, tend, mesh, block % configs, rk_timestep(rk_step), &
                                          number_sub_steps(rk_step), rk_step, &
                                          cellThreadStart(thread), cellThreadEnd(thread), &
                                          vertexThreadStart(thread), vertexThreadEnd(thread), &
                                          edgeThreadStart(thread), edgeThreadEnd(thread), &
                                          cellSolveThreadStart(thread), cellSolveThreadEnd(thread), &
                                          vertexSolveThreadStart(thread), vertexSolveThreadEnd(thread), &
                                          edgeSolveThreadStart(thread), edgeSolveThreadEnd(thread))
            end do
!$OMP END PARALLEL DO

            call mpas_timer_stop('atm_recover_large_step_variables')

!-------------------------------------------------------------------

            if (config_apply_lbcs) then

               ! First, (re)set the value of u and ru in the specified zone at the outermost edge (we will reset all for now).
               ! atm_recover_large_step_variables will not have set outermost edge velocities correctly.
               call mpas_pool_get_array(state, 'u', u, 2)
               call mpas_pool_get_array(mesh, 'bdyMaskEdge', bdyMaskEdge)

               allocate(ru_driving_values(nVertLevels,nEdges+1))

               time_dyn_step = dt_dynamics*real(dynamics_substep-1) + rk_timestep(rk_step)

               ru_driving_values(1:nVertLevels,1:nEdges+1) =  mpas_atm_get_bdy_state( clock, block, nVertLevels, nEdges, 'u', time_dyn_step )
               ! do this inline at present - it is simple enough
               do iEdge = 1, nEdgesSolve
                  if(bdyMaskEdge(iEdge) > nRelaxZone) then
                     do k = 1, nVertLevels
                        u(k,iEdge) = ru_driving_values(k,iEdge)
                     end do
                  end if
               end do

               ru_driving_values(1:nVertLevels,1:nEdges+1) =  mpas_atm_get_bdy_state( clock, block, nVertLevels, nEdges, 'ru', time_dyn_step )
               call mpas_pool_get_array(diag, 'ru', u)
               ! do this inline at present - it is simple enough
               do iEdge = 1, nEdges
                  if(bdyMaskEdge(iEdge) > nRelaxZone) then
                     do k = 1, nVertLevels
                        u(k,iEdge) = ru_driving_values(k,iEdge)
                     end do
                  end if
               end do
               
               deallocate(ru_driving_values)

            end if  ! regional_MPAS addition

!-------------------------------------------------------------------
            
            ! u
            if (config_apply_lbcs) then
               call exchange_halo_group(domain, 'dynamics:u_123')
            else
               call exchange_halo_group(domain, 'dynamics:u_3')
            end if

            ! scalar advection: RK3 scheme of Skamarock and Gassmann (2011). 
            ! PD or monotonicity constraints applied only on the final Runge-Kutta substep.

            if (config_scalar_advection .and. (.not. config_split_dynamics_transport) ) then

               call advance_scalars('scalars', domain, rk_step, rk_timestep, config_monotonic, config_positive_definite, &
                                    config_time_integration_order, config_split_dynamics_transport, exchange_halo_group)

               if (config_apply_lbcs) then  ! adjust boundary tendencies for regional_MPAS scalar transport

                  call exchange_halo_group(domain, 'dynamics:scalars')

                  allocate(scalars_driving(num_scalars,nVertLevels,nCells+1))

                  !  get the scalar values driving the regional boundary conditions
                  !
                  if (index_qv > 0) then
                     scalars_driving(index_qv,1:nVertLevels,1:nCells+1) = mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'qv', rk_timestep(rk_step) )
                  end if
                  if (index_qc > 0) then
                     scalars_driving(index_qc,1:nVertLevels,1:nCells+1) = mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'qc', rk_timestep(rk_step) )
                  end if
                  if (index_qr > 0) then
                     scalars_driving(index_qr,1:nVertLevels,1:nCells+1) = mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'qr', rk_timestep(rk_step) )
                  end if
                  if (index_qi > 0) then
                     scalars_driving(index_qi,1:nVertLevels,1:nCells+1) = mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'qi', rk_timestep(rk_step) )
                  end if
                  if (index_qs > 0) then
                     scalars_driving(index_qs,1:nVertLevels,1:nCells+1) = mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'qs', rk_timestep(rk_step) )
                  end if
                  if (index_qg > 0) then
                     scalars_driving(index_qg,1:nVertLevels,1:nCells+1) = mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'qg', rk_timestep(rk_step) )
                  end if
                  if (index_nr > 0) then
                     scalars_driving(index_nr,1:nVertLevels,1:nCells+1) = mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'nr', rk_timestep(rk_step) )
                  end if
                  if (index_ni > 0) then
                     scalars_driving(index_ni,1:nVertLevels,1:nCells+1) = mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'ni', rk_timestep(rk_step) )
                  end if
                  if (index_nc > 0) then
                     scalars_driving(index_nc,1:nVertLevels,1:nCells+1) = mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'nc', rk_timestep(rk_step) )
                  end if
                  if (index_nifa > 0) then
                     scalars_driving(index_nifa,1:nVertLevels,1:nCells+1) = mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'nifa', rk_timestep(rk_step) )
                  end if
                  if (index_nwfa > 0) then
                     scalars_driving(index_nwfa,1:nVertLevels,1:nCells+1) = mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'nwfa', rk_timestep(rk_step) )
                  end if
                  !$OMP PARALLEL DO
                  do thread=1,nThreads
                     call atm_bdy_adjust_scalars( state, diag, mesh, block % configs, scalars_driving, nVertLevels, dt, rk_timestep(rk_step), &
                                                  cellThreadStart(thread), cellThreadEnd(thread), &
                                                  cellSolveThreadStart(thread), cellSolveThreadEnd(thread) )
                  end do
                  !$OMP END PARALLEL DO

                  deallocate(scalars_driving)


               end if  ! regional_MPAS addition

            end if

            call mpas_timer_start('atm_compute_solve_diagnostics')

            allocate(ke_vertex(nVertLevels,nVertices+1))
            ke_vertex(:,nVertices+1) = 0.0_RKIND
            allocate(ke_edge(nVertLevels,nEdges+1))
            ke_edge(:,nEdges+1) = 0.0_RKIND
   
!$OMP PARALLEL DO
            do thread=1,nThreads
               call atm_compute_solve_diagnostics(dt, state, 2, diag, mesh, block % configs, &
                                                  cellThreadStart(thread), cellThreadEnd(thread), &
                                                  vertexThreadStart(thread), vertexThreadEnd(thread), &
                                                  edgeThreadStart(thread), edgeThreadEnd(thread), rk_step)
            end do
!$OMP END PARALLEL DO

            deallocate(ke_vertex)
            deallocate(ke_edge)

            call mpas_timer_stop('atm_compute_solve_diagnostics')

            if (config_scalar_advection .and. (.not. config_split_dynamics_transport) ) then
               !
               ! Communicate halos for w[1,2], pv_edge[1,2], rho_edge[1,2], scalars[1,2]
               !
               call exchange_halo_group(domain, 'dynamics:w,pv_edge,rho_edge,scalars')
            else
               !
               ! Communicate halos for w[1,2], pv_edge[1,2], rho_edge[1,2]
               !
               call exchange_halo_group(domain, 'dynamics:w,pv_edge,rho_edge')
            end if

            ! set the zero-gradient condition on w for regional_MPAS
            
            if ( config_apply_lbcs ) then  ! regional_MPAS addition

!$OMP PARALLEL DO
              do thread=1,nThreads
                 call atm_zero_gradient_w_bdy( state, mesh, &
                                               cellSolveThreadStart(thread), cellSolveThreadEnd(thread) )
              end do
!$OMP END PARALLEL DO

              ! w halo values needs resetting after regional boundary update
              call exchange_halo_group(domain, 'dynamics:w')

            end if ! end of regional_MPAS addition 

         end do RK3_DYNAMICS

         if (dynamics_substep < dynamics_split) then

            !
            ! Communicate halos for theta_m[1,2], pressure_p[1,2], and rtheta_p[1,2]
            !
            call exchange_halo_group(domain, 'dynamics:theta_m,pressure_p,rtheta_p')

            !
            ! Note: A halo exchange for 'exner' here as well as after the call
            ! to driver_microphysics() can substitute for the exchange at
            ! the beginning of each dynamics subcycle. Placing halo exchanges
            ! here and after microphysics may in future allow for aggregation of
            ! the 'exner' exchange with other exchanges.
            !
         end if

         !
         ! MW for ITM: accumulate tendencies over each dynamic substep  
         !
         if ( config_tend ) then  
           call mpas_timer_start('Tendency and PV diagnostics')
           call mpas_timer_start('atm_accumulate_tend')  
!$OMP PARALLEL DO
           do thread=1,nThreads
              call atm_accumulate_tend( state, diag, mesh, block % configs, nCells, nVertLevels, &
                                        cellThreadStart(thread), cellThreadEnd(thread), &
                                        vertexThreadStart(thread), vertexThreadEnd(thread), &
                                        edgeThreadStart(thread), edgeThreadEnd(thread), &
                                        cellSolveThreadStart(thread), cellSolveThreadEnd(thread), &
                                        vertexSolveThreadStart(thread), vertexSolveThreadEnd(thread), &
                                        edgeSolveThreadStart(thread), edgeSolveThreadEnd(thread), &
                                        dt_dynamics, dynamics_substep, dynamics_split )
           end do
!$OMP END PARALLEL DO
           call mpas_timer_stop('atm_accumulate_tend')
           call mpas_timer_stop('Tendency and PV diagnostics')
         end if 


         !  dynamics-transport split, WCS 18 November 2014
         !  (1) time level 1 needs to be set to time level 2
         !  (2) need to accumulate ruAvg and wwAvg over the dynamics substeps, prepare for use in transport
         !  Notes:  physics tendencies for scalars should be OK coming out of dynamics

         call mpas_timer_start('atm_rk_dynamics_substep_finish')

!$OMP PARALLEL DO
         do thread=1,nThreads
            call atm_rk_dynamics_substep_finish(state, diag, dynamics_substep, dynamics_split, &
                                             cellThreadStart(thread), cellThreadEnd(thread), &
                                             vertexThreadStart(thread), vertexThreadEnd(thread), &
                                             edgeThreadStart(thread), edgeThreadEnd(thread), &
                                             cellSolveThreadStart(thread), cellSolveThreadEnd(thread), &
                                             vertexSolveThreadStart(thread), vertexSolveThreadEnd(thread), &
                                             edgeSolveThreadStart(thread), edgeSolveThreadEnd(thread))
         end do
!$OMP END PARALLEL DO

         call mpas_timer_stop('atm_rk_dynamics_substep_finish')

      end do DYNAMICS_SUBSTEPS

     !
     ! MW on ITM: reconstruct zonal and meridional components of the horizontal momentum tendencies 
     !
      if ( config_tend ) then
        call mpas_timer_start('Tendency and PV diagnostics')        
        call mpas_timer_start('atm_reconstruct_tend')
!$OMP PARALLEL DO
        do thread=1,nThreads
           call atm_reconstruct_tend(domain, diag, mesh, block % configs, nCells, nVertLevels, &
                                      cellThreadStart(thread), cellThreadEnd(thread), &
                                      vertexThreadStart(thread), vertexThreadEnd(thread), &
                                      edgeThreadStart(thread), edgeThreadEnd(thread), &
                                      cellSolveThreadStart(thread), cellSolveThreadEnd(thread), &
                                      vertexSolveThreadStart(thread), vertexSolveThreadEnd(thread), &
                                      edgeSolveThreadStart(thread), edgeSolveThreadEnd(thread), &
                                      dt_dynamics, dynamics_substep, dynamics_split, &
                                      exchange_halo_group)
        end do
!$OMP END PARALLEL DO
        call mpas_timer_stop('atm_reconstruct_tend')
        call mpas_timer_stop('Tendency and PV diagnostics')
      end if


      deallocate(qtot)  !  we are finished with these now

#ifndef MPAS_CAM_DYCORE
      call mpas_deallocate_scratch_field(tend_rtheta_physicsField)
      call mpas_deallocate_scratch_field(tend_rho_physicsField)
      call mpas_deallocate_scratch_field(tend_ru_physicsField)
#endif


      !
      !  split transport, at present RK3
      !

      if (config_scalar_advection .and. config_split_dynamics_transport) then

         rk_timestep(1) = dt/3.
         rk_timestep(2) = dt/2.
         rk_timestep(3) = dt
         !  switch for 2nd order time integration for scalar transport
         if(config_time_integration_order == 2) rk_timestep(1) = dt/2.

         RK3_SPLIT_TRANSPORT : do rk_step = 1, 3  ! Runge-Kutta loop


            call advance_scalars('scalars', domain, rk_step, rk_timestep, config_monotonic, config_positive_definite, &
                                 config_time_integration_order, config_split_dynamics_transport, exchange_halo_group)

            if (config_apply_lbcs) then  ! adjust boundary tendencies for regional_MPAS scalar transport

               ! need to fill halo for horizontal filter
               call exchange_halo_group(domain, 'dynamics:scalars')
   
               allocate(scalars_driving(num_scalars,nVertLevels,nCells+1))

               !  get the scalar values driving the regional boundary conditions
               !
               if (index_qv > 0) then
                  scalars_driving(index_qv,1:nVertLevels,1:nCells+1) = mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'qv', rk_timestep(rk_step) )
               end if
               if (index_qc > 0) then
                  scalars_driving(index_qc,1:nVertLevels,1:nCells+1) = mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'qc', rk_timestep(rk_step) )
               end if
               if (index_qr > 0) then
                  scalars_driving(index_qr,1:nVertLevels,1:nCells+1) = mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'qr', rk_timestep(rk_step) )
               end if
               if (index_qi > 0) then
                  scalars_driving(index_qi,1:nVertLevels,1:nCells+1) = mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'qi', rk_timestep(rk_step) )
               end if
               if (index_qs > 0) then
                  scalars_driving(index_qs,1:nVertLevels,1:nCells+1) = mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'qs', rk_timestep(rk_step) )
               end if
               if (index_qg > 0) then
                  scalars_driving(index_qg,1:nVertLevels,1:nCells+1) = mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'qg', rk_timestep(rk_step) )
               end if
               if (index_nr > 0) then
                  scalars_driving(index_nr,1:nVertLevels,1:nCells+1) = mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'nr', rk_timestep(rk_step) )
               end if
               if (index_ni > 0) then
                  scalars_driving(index_ni,1:nVertLevels,1:nCells+1) = mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'ni', rk_timestep(rk_step) )
               end if
               if (index_nc > 0) then
                  scalars_driving(index_nc,1:nVertLevels,1:nCells+1) = mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'nc', rk_timestep(rk_step) )
               end if
               if (index_nifa > 0) then
                  scalars_driving(index_nifa,1:nVertLevels,1:nCells+1) = mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'nifa', rk_timestep(rk_step) )
               end if
               if (index_nwfa > 0) then
                  scalars_driving(index_nwfa,1:nVertLevels,1:nCells+1) = mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'nwfa', rk_timestep(rk_step) )
               end if
!$OMP PARALLEL DO
               do thread=1,nThreads
                  call atm_bdy_adjust_scalars( state, diag, mesh, block % configs, scalars_driving, nVertLevels, dt, rk_timestep(rk_step), &
                                               cellThreadStart(thread), cellThreadEnd(thread), &
                                               cellSolveThreadStart(thread), cellSolveThreadEnd(thread) )
               end do
!$OMP END PARALLEL DO

               deallocate(scalars_driving)


            end if  ! regional_MPAS addition

!------------------------------------------------------------------------------------------------------------------------

            if (rk_step < 3) then
               call exchange_halo_group(domain, 'dynamics:scalars')
            end if

         end do RK3_SPLIT_TRANSPORT

      end if

      !
      ! reconstruct full velocity vectors at cell centers:
      !
      call mpas_pool_get_array(state, 'u', u, 2)
      call mpas_pool_get_array(diag, 'uReconstructX', uReconstructX)
      call mpas_pool_get_array(diag, 'uReconstructY', uReconstructY)
      call mpas_pool_get_array(diag, 'uReconstructZ', uReconstructZ)
      call mpas_pool_get_array(diag, 'uReconstructZonal', uReconstructZonal)
      call mpas_pool_get_array(diag, 'uReconstructMeridional', uReconstructMeridional)

      call mpas_reconstruct(mesh, u,                &
                            uReconstructX,          &
                            uReconstructY,          &
                            uReconstructZ,          &
                            uReconstructZonal,      &
                            uReconstructMeridional  &
                           )


      !
      ! call to parameterizations of cloud microphysics. calculation of the tendency of water vapor to horizontal and
      ! vertical advection needed for the Tiedtke parameterization of convection.
      !

#ifdef DO_PHYSICS
      call mpas_pool_get_array(state, 'scalars', scalars_1, 1)
      call mpas_pool_get_array(state, 'scalars', scalars_2, 2)

      if(config_convection_scheme == 'cu_grell_freitas' .or. &
         config_convection_scheme == 'cu_ntiedtke') then

         call mpas_pool_get_array(tend_physics, 'rqvdynten', rqvdynten)
         call mpas_pool_get_array(state, 'theta_m', theta_m, 2)

         call mpas_pool_get_array(tend_physics, 'rthdynten', rthdynten)

         !NOTE: The calculation of the tendency due to horizontal and vertical advection for the water vapor mixing ratio
         !requires that the subroutine atm_advance_scalars_mono was called on the third Runge Kutta step, so that a halo
         !update for the scalars at time_levs(1) is applied. A halo update for the scalars at time_levs(2) is done above. 
         if (config_monotonic) then
            rqvdynten(:,:) = ( scalars_2(index_qv,:,:) - scalars_1(index_qv,:,:) ) / config_dt
         else
            rqvdynten(:,:) = 0._RKIND
         end if

         do k = 1, nVertLevels
            do iCell = 1, nCellsSolve
               fac_m = 1._RKIND/(1._RKIND + rv/rgas*scalars_2(index_qv,k,iCell))
               theta_local = theta_m(k,iCell)*fac_m
               rthdynten(k,iCell) = fac_m*(rthdynten(k,iCell)-theta_local*rv/rgas*rqvdynten(k,iCell))
            end do
         end do

      end if


      ! MW on ITM 
      if ( config_tend ) then
         call mpas_timer_start('Tendency and PV diagnostics')     
         call mpas_pool_get_array(diag, 'acc_qv_tend_dyn_large', acc_qv_tend_dyn_large)
         acc_qv_tend_dyn_large(:,:) = acc_qv_tend_dyn_large(:,:) + ( scalars_2(index_qv,:,:) - scalars_1(index_qv,:,:) ) / config_dt

         if ( config_pv_tend ) then      
            call mpas_pool_get_array(diag, 'dqv_dt_dyn', dqv_dt_dyn) 
            dqv_dt_dyn(:,:) = 0.0
            dqv_dt_dyn(:,:) =  ( scalars_2(index_qv,:,:) - scalars_1(index_qv,:,:) ) / config_dt  ! MW: added for PV
         end if   

         call mpas_timer_stop('Tendency and PV diagnostics')
      endif
      !-----

      !simply set to zero negative mixing ratios of different water species (for now):
      where ( scalars_2(:,:,:) < 0.0)  &
         scalars_2(:,:,:) = 0.0

      !call microphysics schemes:
      if (trim(config_microp_scheme) /= 'off')  then
        call mpas_timer_start('microphysics')
!$OMP PARALLEL DO
        do thread=1,nThreads
           call driver_microphysics ( block % configs, mesh, state, 2, diag, diag_physics, tend_physics, tend, itimestep, &
                                      cellSolveThreadStart(thread), cellSolveThreadEnd(thread))
        end do
!$OMP END PARALLEL DO
        call mpas_timer_stop('microphysics')
      end if

      !
      ! Note: A halo exchange for 'exner' here as well as at the end of
      ! the first (n-1) dynamics subcycles can substitute for the exchange at
      ! the beginning of each dynamics subcycle. Placing halo exchanges here
      ! and at the end of dynamics subcycles may in future allow for aggregation
      ! of the 'exner' exchange with other exchanges.
      !
#endif

      if (config_apply_lbcs) then  ! reset boundary values of rtheta in the specified zone - microphysics has messed with them

        allocate(rt_driving_values(nVertLevels,nCells+1))
        allocate(rho_driving_values(nVertLevels,nCells+1))
        time_dyn_step = dt  ! end of full timestep values

        rt_driving_values(1:nVertLevels,1:nCells+1) =  mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'rtheta_m', time_dyn_step )
        rho_driving_values(1:nVertLevels,1:nCells+1) =  mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'rho_zz', time_dyn_step )

!$OMP PARALLEL DO
        do thread=1,nThreads
           call atm_bdy_reset_speczone_values( state, diag, mesh, nVertLevels, &
                                               rt_driving_values, rho_driving_values,                        &
                                               cellThreadStart(thread), cellThreadEnd(thread),               &
                                               cellSolveThreadStart(thread), cellSolveThreadEnd(thread) )
        end do
!$OMP END PARALLEL DO

        deallocate(rt_driving_values)
        deallocate(rho_driving_values)

      end if  ! regional_MPAS addition


      if (config_apply_lbcs) then  ! adjust boundary values for regional_MPAS scalar transport

         call exchange_halo_group(domain, 'dynamics:scalars')

         allocate(scalars_driving(num_scalars,nVertLevels,nCells+1))

         !  get the scalar values driving the regional boundary conditions
         !
         if (index_qv > 0) then
            scalars_driving(index_qv,1:nVertLevels,1:nCells+1) = mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'qv', dt )
         end if
         if (index_qc > 0) then
            scalars_driving(index_qc,1:nVertLevels,1:nCells+1) = mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'qc', dt )
         end if
         if (index_qr > 0) then
            scalars_driving(index_qr,1:nVertLevels,1:nCells+1) = mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'qr', dt )
         end if
         if (index_qi > 0) then
            scalars_driving(index_qi,1:nVertLevels,1:nCells+1) = mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'qi', dt )
         end if
         if (index_qs > 0) then
            scalars_driving(index_qs,1:nVertLevels,1:nCells+1) = mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'qs', dt )
         end if
         if (index_qg > 0) then
            scalars_driving(index_qg,1:nVertLevels,1:nCells+1) = mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'qg', dt )
         end if
         if (index_nr > 0) then
            scalars_driving(index_nr,1:nVertLevels,1:nCells+1) = mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'nr', dt )
         end if
         if (index_ni > 0) then
            scalars_driving(index_ni,1:nVertLevels,1:nCells+1) = mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'ni', dt )
         end if
         if (index_nc > 0) then
            scalars_driving(index_nc,1:nVertLevels,1:nCells+1) = mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'nc', dt )
         end if
         if (index_nifa > 0) then
            scalars_driving(index_nifa,1:nVertLevels,1:nCells+1) = mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'nifa', dt )
         end if
         if (index_nwfa > 0) then
            scalars_driving(index_nwfa,1:nVertLevels,1:nCells+1) = mpas_atm_get_bdy_state( clock, block, nVertLevels, nCells, 'nwfa', dt )
         end if
!$OMP PARALLEL DO
         do thread=1,nThreads
            call atm_bdy_set_scalars( state, mesh, scalars_driving, nVertLevels, &
                                      cellThreadStart(thread), cellThreadEnd(thread), &
                                      cellSolveThreadStart(thread), cellSolveThreadEnd(thread) )
         end do
!$OMP END PARALLEL DO

         deallocate(scalars_driving)

      end if  ! regional_MPAS addition

      call summarize_timestep(domain)

   end subroutine atm_srk3


   !-----------------------------------------------------------------------
   !  routine advance_scalars
   !
   !> \brief Advance the scalar fields
   !> \date 10 February 2020
   !> \details
   !>  Manages the advance of the model scalar fields, taking into account
   !>  runtime selection of monotonicity and scalar transport splitting.
   !>
   !>  The first argument, field_name, indicates the base name for the array
   !>  of scalars to be advected. It is assumed that, if the name of
   !>  the array is XYZ, then there will exist:
   !>
   !>  (1) An array in the 'state' pool named XYZ with dimensions
   !>      (num_XYZ, nVertLevels, nCells) and two time levels
   !>
   !>  (2) A dimension, num_XYZ, in the 'state' pool
   !>
   !>  (3) An array in the 'tend' pool named XYZ_tend with dimensions
   !>      (num_XYZ, nVertLevels, nCells) and one time level
   !>
   !>  The scalars arrays can either be var_arrays formed from multiple
   !>  constituents, each with dimensions (nVertLevels, nCells), or they can
   !>  simply be vars with dimensions (num_???, nVertLevels, nCells).
   !
   !-----------------------------------------------------------------------
   subroutine advance_scalars(field_name, domain, rk_step, rk_timestep, config_monotonic, config_positive_definite, &
                              config_time_integration_order, config_split_dynamics_transport, exchange_halo_group)

      implicit none

      ! Arguments
      character(len=*), intent(in) :: field_name
      type (domain_type), intent(inout) :: domain
      integer, intent(in) :: rk_step
      real(kind=RKIND), dimension(:), intent(in) :: rk_timestep
      logical, intent(in) :: config_monotonic
      logical, intent(in) :: config_positive_definite
      integer, intent(in) :: config_time_integration_order
      logical, intent(in) :: config_split_dynamics_transport
      procedure (halo_exchange_routine) :: exchange_halo_group

      ! Local variables
      integer :: thread

      type (mpas_pool_type), pointer :: tend
      type (mpas_pool_type), pointer :: state
      type (mpas_pool_type), pointer :: diag
      type (mpas_pool_type), pointer :: mesh
      type (mpas_pool_type), pointer :: halo_scratch

      integer, pointer :: nCells
      integer, pointer :: nEdges
      integer, pointer :: nVertLevels
      integer, pointer :: num_scalars

      integer, pointer :: nThreads
      integer, dimension(:), pointer :: cellThreadStart
      integer, dimension(:), pointer :: cellThreadEnd
      integer, dimension(:), pointer :: cellSolveThreadStart
      integer, dimension(:), pointer :: cellSolveThreadEnd
      integer, dimension(:), pointer :: edgeThreadStart
      integer, dimension(:), pointer :: edgeThreadEnd


      if (rk_step < 3 .or. (.not. config_monotonic .and. .not. config_positive_definite)) then
         call mpas_timer_start('atm_advance_scalars')
      else
         call mpas_timer_start('atm_advance_scalars_mono')
      end if

      call mpas_pool_get_subpool(block % structs, 'tend', tend)
      call mpas_pool_get_subpool(block % structs, 'state', state)
      call mpas_pool_get_subpool(block % structs, 'diag', diag)
      call mpas_pool_get_subpool(block % structs, 'mesh', mesh)
      call mpas_pool_get_subpool(block % structs, 'halo_scratch', halo_scratch)

      call mpas_pool_get_dimension(mesh, 'nCells', nCells)
      call mpas_pool_get_dimension(mesh, 'nEdges', nEdges)
      call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)
      call mpas_pool_get_dimension(state, 'num_'//trim(field_name), num_scalars)

      call mpas_pool_get_dimension(block % dimensions, 'nThreads', nThreads)

      call mpas_pool_get_dimension(block % dimensions, 'cellThreadStart', cellThreadStart)
      call mpas_pool_get_dimension(block % dimensions, 'cellThreadEnd', cellThreadEnd)
      call mpas_pool_get_dimension(block % dimensions, 'cellSolveThreadStart', cellSolveThreadStart)
      call mpas_pool_get_dimension(block % dimensions, 'cellSolveThreadEnd', cellSolveThreadEnd)

      call mpas_pool_get_dimension(block % dimensions, 'edgeThreadStart', edgeThreadStart)
      call mpas_pool_get_dimension(block % dimensions, 'edgeThreadEnd', edgeThreadEnd)

      allocate(scalar_old_arr(nVertLevels,nCells+1))
      scalar_old_arr(:,nCells+1) = 0.0_RKIND
      allocate(scalar_new_arr(nVertLevels,nCells+1))
      scalar_new_arr(:,nCells+1) = 0.0_RKIND
      allocate(s_max_arr(nVertLevels,nCells+1))
      s_max_arr(:,nCells+1) = 0.0_RKIND
      allocate(s_min_arr(nVertLevels,nCells+1))
      s_min_arr(:,nCells+1) = 0.0_RKIND
      allocate(flux_array(nVertLevels,nEdges+1))
      flux_array(:,nEdges+1) = 0.0_RKIND
      allocate(wdtn_arr(nVertLevels+1,nCells+1))
      wdtn_arr(:,nCells+1) = 0.0_RKIND
      if (config_split_dynamics_transport) then
         allocate(rho_zz_int(nVertLevels,nCells+1))
         rho_zz_int(:,nCells+1) = 0.0_RKIND
      else
         allocate(rho_zz_int(1,1))
      end if
      if (rk_step < 3 .or. (.not. config_monotonic .and. .not. config_positive_definite)) then
         allocate(horiz_flux_array(num_scalars,nVertLevels,nEdges+1))
         horiz_flux_array(:,:,nEdges+1) = 0.0_RKIND
      else
         allocate(flux_upwind_tmp_arr(nVertLevels,nEdges+1))
         flux_upwind_tmp_arr(:,nEdges+1) = 0.0_RKIND
         allocate(flux_tmp_arr(nVertLevels,nEdges+1))
         flux_tmp_arr(:,nEdges+1) = 0.0_RKIND
      end if

      !
      ! Note: The advance_scalars_mono routine can be used without limiting, and thus, encompasses 
      !       the functionality of the advance_scalars routine; however, it is noticeably slower, 
      !       so we use the advance_scalars routine for the first two RK substeps.
      !
      !$OMP PARALLEL DO
      do thread=1,nThreads
         if (rk_step < 3 .or. (.not. config_monotonic .and. .not. config_positive_definite)) then
            call atm_advance_scalars(field_name, tend, state, diag, mesh, block % configs, rk_timestep(rk_step), &
                                     edgeThreadStart(thread), edgeThreadEnd(thread), &
                                     cellSolveThreadStart(thread), cellSolveThreadEnd(thread), &
                                     horiz_flux_array, rk_step, config_time_integration_order, &
                                     advance_density=config_split_dynamics_transport)
         else
            call atm_advance_scalars_mono(field_name, block, tend, state, diag, mesh, halo_scratch, &
                                          block % configs, rk_timestep(rk_step), &
                                          cellThreadStart(thread), cellThreadEnd(thread), &
                                          edgeThreadStart(thread), edgeThreadEnd(thread), &
                                          cellSolveThreadStart(thread), cellSolveThreadEnd(thread), &
                                          scalar_old_arr, scalar_new_arr, s_max_arr, s_min_arr, wdtn_arr, &
                                          flux_array, flux_upwind_tmp_arr, flux_tmp_arr, &
                                          exchange_halo_group, &
                                          advance_density=config_split_dynamics_transport, rho_zz_int=rho_zz_int)
         end if
      end do
      !$OMP END PARALLEL DO

      deallocate(scalar_old_arr)
      deallocate(scalar_new_arr)
      deallocate(s_max_arr)
      deallocate(s_min_arr)
      deallocate(flux_array)
      deallocate(wdtn_arr)
      deallocate(rho_zz_int)

      if (rk_step < 3 .or. (.not. config_monotonic .and. .not. config_positive_definite)) then
         deallocate(horiz_flux_array)
      else
         deallocate(flux_upwind_tmp_arr)
         deallocate(flux_tmp_arr)
      end if

      if (rk_step < 3 .or. (.not. config_monotonic .and. .not. config_positive_definite)) then
         call mpas_timer_stop('atm_advance_scalars')
      else
         call mpas_timer_stop('atm_advance_scalars_mono')
      end if

   end subroutine advance_scalars


   subroutine atm_rk_integration_setup( state, diag, &
                                   cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd, &
                                   cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd)

      implicit none

      type (mpas_pool_type), intent(inout) :: state
      type (mpas_pool_type), intent(inout) :: diag
      integer, intent(in) :: cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd
      integer, intent(in) :: cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd

      real (kind=RKIND), dimension(:,:), pointer :: ru
      real (kind=RKIND), dimension(:,:), pointer :: ru_save
      real (kind=RKIND), dimension(:,:), pointer :: rw
      real (kind=RKIND), dimension(:,:), pointer :: rw_save
      real (kind=RKIND), dimension(:,:), pointer :: rtheta_p
      real (kind=RKIND), dimension(:,:), pointer :: rtheta_p_save
      real (kind=RKIND), dimension(:,:), pointer :: rho_p
      real (kind=RKIND), dimension(:,:), pointer :: rho_p_save
      real (kind=RKIND), dimension(:,:), pointer :: rho_zz_old_split

      real (kind=RKIND), dimension(:,:), pointer :: u_1, u_2
      real (kind=RKIND), dimension(:,:), pointer :: w_1, w_2
      real (kind=RKIND), dimension(:,:), pointer :: theta_m_1, theta_m_2
      real (kind=RKIND), dimension(:,:), pointer :: rho_zz_1, rho_zz_2
      real (kind=RKIND), dimension(:,:,:), pointer :: scalars_1, scalars_2

      call mpas_pool_get_array(diag, 'ru', ru)
      call mpas_pool_get_array(diag, 'ru_save', ru_save)
      call mpas_pool_get_array(diag, 'rw', rw)
      call mpas_pool_get_array(diag, 'rw_save', rw_save)
      call mpas_pool_get_array(diag, 'rtheta_p', rtheta_p)
      call mpas_pool_get_array(diag, 'rtheta_p_save', rtheta_p_save)
      call mpas_pool_get_array(diag, 'rho_p', rho_p)
      call mpas_pool_get_array(diag, 'rho_p_save', rho_p_save)
      call mpas_pool_get_array(diag, 'rho_zz_old_split', rho_zz_old_split)

      call mpas_pool_get_array(state, 'u', u_1, 1)
      call mpas_pool_get_array(state, 'u', u_2, 2)
      call mpas_pool_get_array(state, 'w', w_1, 1)
      call mpas_pool_get_array(state, 'w', w_2, 2)
      call mpas_pool_get_array(state, 'theta_m', theta_m_1, 1)
      call mpas_pool_get_array(state, 'theta_m', theta_m_2, 2)
      call mpas_pool_get_array(state, 'rho_zz', rho_zz_1, 1)
      call mpas_pool_get_array(state, 'rho_zz', rho_zz_2, 2)
      call mpas_pool_get_array(state, 'scalars', scalars_1, 1)
      call mpas_pool_get_array(state, 'scalars', scalars_2, 2)

      ru_save(:,edgeStart:edgeEnd) = ru(:,edgeStart:edgeEnd)
      rw_save(:,cellStart:cellEnd) = rw(:,cellStart:cellEnd)
      rtheta_p_save(:,cellStart:cellEnd) = rtheta_p(:,cellStart:cellEnd)
      rho_p_save(:,cellStart:cellEnd) = rho_p(:,cellStart:cellEnd)

      u_2(:,edgeStart:edgeEnd) = u_1(:,edgeStart:edgeEnd)
      w_2(:,cellStart:cellEnd) = w_1(:,cellStart:cellEnd)
      theta_m_2(:,cellStart:cellEnd) = theta_m_1(:,cellStart:cellEnd)
      rho_zz_2(:,cellStart:cellEnd) = rho_zz_1(:,cellStart:cellEnd)
      rho_zz_old_split(:,cellStart:cellEnd) =  rho_zz_1(:,cellStart:cellEnd)
      scalars_2(:,:,cellStart:cellEnd) = scalars_1(:,:,cellStart:cellEnd)

   end subroutine atm_rk_integration_setup


   subroutine atm_compute_moist_coefficients( dims, state, diag, mesh, &
                                   cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd, &
                                   cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd)

      ! the moist coefficients cqu and cqw serve to transform the inverse dry density (1/rho_d) 
      ! into the inverse full (moist) density (1/rho_m).

      implicit none

      type (mpas_pool_type), intent(in) :: dims
      type (mpas_pool_type), intent(inout) :: state
      type (mpas_pool_type), intent(inout) :: diag
      type (mpas_pool_type), intent(inout) :: mesh
      integer, intent(in) :: cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd
      integer, intent(in) :: cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd

      integer :: iEdge, iCell, k, cell1, cell2, iq
      integer, pointer :: nCells, nEdges, nVertLevels, nCellsSolve
      real (kind=RKIND) :: qtotal
      integer, dimension(:,:), pointer :: cellsOnEdge
      integer, pointer :: moist_start, moist_end
      real (kind=RKIND), dimension(:,:,:), pointer :: scalars
      real (kind=RKIND), dimension(:,:), pointer :: cqw
      real (kind=RKIND), dimension(:,:), pointer :: cqu

      call mpas_pool_get_dimension(dims, 'nCells', nCells)
      call mpas_pool_get_dimension(dims, 'nEdges', nEdges)
      call mpas_pool_get_dimension(dims, 'nVertLevels', nVertLevels)
      call mpas_pool_get_dimension(dims, 'nCellsSolve', nCellsSolve)
      call mpas_pool_get_dimension(state, 'moist_start', moist_start)
      call mpas_pool_get_dimension(state, 'moist_end', moist_end)

      call mpas_pool_get_array(mesh, 'cellsOnEdge', cellsOnEdge)
      call mpas_pool_get_array(state, 'scalars', scalars, 2)
      call mpas_pool_get_array(diag, 'cqw', cqw)
      call mpas_pool_get_array(diag, 'cqu', cqu)

!      do iCell = cellSolveStart,cellSolveEnd
      do iCell = cellStart,cellEnd
         qtot(1:nVertLevels,iCell) = 0.0
         do k = 1,nVertLevels
            do iq = moist_start, moist_end
               qtot(k,iCell) = qtot(k,iCell) + scalars(iq, k, iCell)
            end do
         end do
      end do    

!      do iCell = cellSolveStart,cellSolveEnd
      do iCell = cellStart,cellEnd
         do k = 2, nVertLevels
            qtotal = 0.5*(qtot(k,iCell)+qtot(k-1,iCell))
            cqw(k,iCell) = 1.0 / (1.0 + qtotal)
         end do
      end do

!  would need to compute qtot for all cells and an openmp barrier to use qtot below.

      do iEdge = edgeStart,edgeEnd
         cell1 = cellsOnEdge(1,iEdge)
         cell2 = cellsOnEdge(2,iEdge)
         if (cell1 <= nCellsSolve .or. cell2 <= nCellsSolve) then
            do k = 1, nVertLevels
               qtotal = 0.0
               do iq = moist_start, moist_end
                  qtotal = qtotal + 0.5 * ( scalars(iq, k, cell1) + scalars(iq, k, cell2) )
               end do
               cqu(k,iEdge) = 1.0 / (1.0 + qtotal)
            end do
         end if
      end do

   end subroutine atm_compute_moist_coefficients


   subroutine atm_compute_vert_imp_coefs(state, mesh, diag, configs, nVertLevels, dts, &
                                   cellStart, cellEnd, edgeStart, edgeEnd, &
                                   cellSolveStart, cellSolveEnd, edgeSolveStart, edgeSolveEnd)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Compute coefficients for vertically implicit gravity-wave/acoustic computations
   !
   ! Input: state - current model state
   !        mesh - grid metadata
   !
   ! Output: diag - cofrz, cofwr, cofwz, coftz, cofwt, a, alpha and gamma
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      type (mpas_pool_type), intent(in)    :: state
      type (mpas_pool_type), intent(in)    :: mesh
      type (mpas_pool_type), intent(inout) :: diag
      type (mpas_pool_type), intent(in)    :: configs
      integer, intent(in)                  :: nVertLevels          ! for allocating stack variables
      real (kind=RKIND), intent(in)        :: dts
      integer, intent(in) :: cellStart, cellEnd, edgeStart, edgeEnd
      integer, intent(in) :: cellSolveStart, cellSolveEnd, edgeSolveStart, edgeSolveEnd


      real (kind=RKIND), dimension(:,:), pointer :: zz, cqw, p, t, rb, rtb, pb, rt
      real (kind=RKIND), dimension(:,:), pointer :: cofwr, cofwz, coftz, cofwt, a_tri, alpha_tri, gamma_tri
      real (kind=RKIND), dimension(:), pointer :: cofrz, rdzw, fzm, fzp, rdzu
      real (kind=RKIND), dimension(:,:,:), pointer :: scalars

      real (kind=RKIND), pointer :: epssm

      integer, pointer :: nCells, moist_start, moist_end


      call mpas_pool_get_config(configs, 'config_epssm', epssm)

      call mpas_pool_get_array(mesh, 'rdzu', rdzu)
      call mpas_pool_get_array(mesh, 'rdzw', rdzw)
      call mpas_pool_get_array(mesh, 'fzm', fzm)
      call mpas_pool_get_array(mesh, 'fzp', fzp)
      call mpas_pool_get_array(mesh, 'zz', zz)

      call mpas_pool_get_array(diag, 'cqw', cqw)
      call mpas_pool_get_array(diag, 'exner', p)
      call mpas_pool_get_array(diag, 'exner_base', pb)
      call mpas_pool_get_array(diag, 'rtheta_p', rt)
      call mpas_pool_get_array(diag, 'rtheta_base', rtb)
      call mpas_pool_get_array(diag, 'rho_base', rb)

      call mpas_pool_get_array(diag, 'alpha_tri', alpha_tri)
      call mpas_pool_get_array(diag, 'gamma_tri', gamma_tri)
      call mpas_pool_get_array(diag, 'a_tri', a_tri)
      call mpas_pool_get_array(diag, 'cofwr', cofwr)
      call mpas_pool_get_array(diag, 'cofwz', cofwz)
      call mpas_pool_get_array(diag, 'coftz', coftz)
      call mpas_pool_get_array(diag, 'cofwt', cofwt)
      call mpas_pool_get_array(diag, 'cofrz', cofrz)

      call mpas_pool_get_array(state, 'theta_m', t, 2)
      call mpas_pool_get_array(state, 'scalars', scalars, 2)

      call mpas_pool_get_dimension(state, 'nCells', nCells)
      call mpas_pool_get_dimension(state, 'moist_start', moist_start)
      call mpas_pool_get_dimension(state, 'moist_end', moist_end)


      call atm_compute_vert_imp_coefs_work(nCells, moist_start, moist_end, dts, epssm, &
                                   zz, cqw, p, t, rb, rtb, pb, rt, cofwr, cofwz, coftz, cofwt, &
                                   a_tri, alpha_tri, gamma_tri, cofrz, rdzw, fzm, fzp, rdzu, scalars, &
                                   cellStart, cellEnd, edgeStart, edgeEnd, &
                                   cellSolveStart, cellSolveEnd, edgeSolveStart, edgeSolveEnd)


   end subroutine atm_compute_vert_imp_coefs


   subroutine atm_compute_vert_imp_coefs_work(nCells, moist_start, moist_end, dts, epssm, &
                                   zz, cqw, p, t, rb, rtb, pb, rt, cofwr, cofwz, coftz, cofwt, &
                                   a_tri, alpha_tri, gamma_tri, cofrz, rdzw, fzm, fzp, rdzu, scalars, &
                                   cellStart, cellEnd, edgeStart, edgeEnd, &
                                   cellSolveStart, cellSolveEnd, edgeSolveStart, edgeSolveEnd)

      use mpas_atm_dimensions
 
      implicit none


      !
      ! Dummy arguments
      !
      integer, intent(in) :: nCells, moist_start, moist_end
      real (kind=RKIND), intent(in) :: dts
      real (kind=RKIND), intent(in) :: epssm

      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: zz
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: cqw
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: p
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: t
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rb
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rtb
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: pb
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rt
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: cofwr
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: cofwz
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: coftz
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: cofwt
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: a_tri
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: alpha_tri
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: gamma_tri
      real (kind=RKIND), dimension(nVertLevels) :: cofrz
      real (kind=RKIND), dimension(nVertLevels) :: rdzw
      real (kind=RKIND), dimension(nVertLevels) :: fzm
      real (kind=RKIND), dimension(nVertLevels) :: fzp
      real (kind=RKIND), dimension(nVertLevels) :: rdzu
      real (kind=RKIND), dimension(num_scalars,nVertLevels,nCells+1) :: scalars

      integer, intent(in) :: cellStart, cellEnd, edgeStart, edgeEnd
      integer, intent(in) :: cellSolveStart, cellSolveEnd, edgeSolveStart, edgeSolveEnd


      !
      ! Local variables
      !
      integer :: iCell, k, iq
      real (kind=RKIND) :: dtseps, c2, qtotal, rcv
      real (kind=RKIND), dimension( nVertLevels ) :: b_tri, c_tri


      !  set coefficients
      dtseps = .5*dts*(1.+epssm)
      rcv = rgas/(cp-rgas)
      c2 = cp*rcv

! MGD bad to have all threads setting this variable?
      do k=1,nVertLevels
         cofrz(k) = dtseps*rdzw(k)
      end do

      do iCell = cellSolveStart,cellSolveEnd  !  we only need to do cells we are solving for, not halo cells

!DIR$ IVDEP
         do k=2,nVertLevels
            cofwr(k,iCell) =.5*dtseps*gravity*(fzm(k)*zz(k,iCell)+fzp(k)*zz(k-1,iCell))
         end do
         coftz(1,iCell) = 0.0
!DIR$ IVDEP
         do k=2,nVertLevels
            cofwz(k,iCell) = dtseps*c2*(fzm(k)*zz(k,iCell)+fzp(k)*zz(k-1,iCell))  &
                 *rdzu(k)*cqw(k,iCell)*(fzm(k)*p (k,iCell)+fzp(k)*p (k-1,iCell))
            coftz(k,iCell) = dtseps*   (fzm(k)*t (k,iCell)+fzp(k)*t (k-1,iCell))
         end do
         coftz(nVertLevels+1,iCell) = 0.0
!DIR$ IVDEP
         do k=1,nVertLevels

!            qtotal = 0.
!            do iq = moist_start, moist_end
!               qtotal = qtotal + scalars(iq, k, iCell)
!            end do
            qtotal = qtot(k,iCell)

            cofwt(k,iCell) = .5*dtseps*rcv*zz(k,iCell)*gravity*rb(k,iCell)/(1.+qtotal)  &
                                *p(k,iCell)/((rtb(k,iCell)+rt(k,iCell))*pb(k,iCell))
!            cofwt(k,iCell) = 0.
         end do

         a_tri(1,iCell) = 0.  ! note, this value is never used
         b_tri(1) = 1.    ! note, this value is never used
         c_tri(1) = 0.    ! note, this value is never used
         gamma_tri(1,iCell) = 0.
         alpha_tri(1,iCell) = 0.  ! note, this value is never used

!DIR$ IVDEP
         do k=2,nVertLevels
            a_tri(k,iCell) = -cofwz(k  ,iCell)* coftz(k-1,iCell)*rdzw(k-1)*zz(k-1,iCell)   &
                         +cofwr(k  ,iCell)* cofrz(k-1  )                       &
                         -cofwt(k-1,iCell)* coftz(k-1,iCell)*rdzw(k-1)
            b_tri(k) = 1.                                                  &
                         +cofwz(k  ,iCell)*(coftz(k  ,iCell)*rdzw(k  )*zz(k  ,iCell)   &
                                      +coftz(k  ,iCell)*rdzw(k-1)*zz(k-1,iCell))   &
                         -coftz(k  ,iCell)*(cofwt(k  ,iCell)*rdzw(k  )             &
                                       -cofwt(k-1,iCell)*rdzw(k-1))            &
                         +cofwr(k,  iCell)*(cofrz(k    )-cofrz(k-1))
            c_tri(k) =   -cofwz(k  ,iCell)* coftz(k+1,iCell)*rdzw(k  )*zz(k  ,iCell)   &
                         -cofwr(k  ,iCell)* cofrz(k    )                       &
                         +cofwt(k  ,iCell)* coftz(k+1,iCell)*rdzw(k  )
         end do
!MGD VECTOR DEPENDENCE
         do k=2,nVertLevels
            alpha_tri(k,iCell) = 1./(b_tri(k)-a_tri(k,iCell)*gamma_tri(k-1,iCell))
            gamma_tri(k,iCell) = c_tri(k)*alpha_tri(k,iCell)
         end do

      end do ! loop over cells

   end subroutine atm_compute_vert_imp_coefs_work


   subroutine atm_set_smlstep_pert_variables( tend, diag, mesh, configs, &
                                   cellStart, cellEnd, edgeStart, edgeEnd, &
                                   cellSolveStart, cellSolveEnd, edgeSolveStart, edgeSolveEnd)

      ! following Klemp et al MWR 2007, we use preturbation variables
      ! in the acoustic-step integration.  This routine computes those 
      ! perturbation variables.  state variables are reconstituted after 
      ! the acousstic steps in subroutine atm_recover_large_step_variables


      implicit none

      type (mpas_pool_type), intent(inout) :: tend
      type (mpas_pool_type), intent(inout) :: diag
      type (mpas_pool_type), intent(inout) :: mesh
      type (mpas_pool_type), intent(in) :: configs
      integer, intent(in) :: cellStart, cellEnd, edgeStart, edgeEnd
      integer, intent(in) :: cellSolveStart, cellSolveEnd, edgeSolveStart, edgeSolveEnd

      integer, pointer :: nCells, nEdges, nCellsSolve
      integer, dimension(:), pointer :: nEdgesOnCell
      integer, dimension(:,:), pointer :: cellsOnEdge, edgesOnCell
      real (kind=RKIND), dimension(:), pointer :: fzm, fzp
      real (kind=RKIND), dimension(:,:), pointer :: ruAvg, wwAvg
      real (kind=RKIND), dimension(:,:,:), pointer :: zb, zb3, zb_cell, zb3_cell
      real (kind=RKIND), dimension(:,:), pointer :: zz
      real (kind=RKIND), dimension(:,:), pointer :: w_tend, u_tend
      real (kind=RKIND), dimension(:,:), pointer :: rho_pp, rho_p_save, rho_p
      real (kind=RKIND), dimension(:,:), pointer :: ru_p, ru, ru_save
      real (kind=RKIND), dimension(:,:), pointer :: rtheta_pp, rtheta_p_save, rtheta_p, rtheta_pp_old
      real (kind=RKIND), dimension(:,:), pointer :: rw_p, rw_save, rw
      real (kind=RKIND), dimension(:,:), pointer :: edgesOnCell_sign

      integer, dimension(:), pointer :: bdyMaskCell  ! regional_MPAS

      call mpas_pool_get_dimension(mesh, 'nCells', nCells)
      call mpas_pool_get_dimension(mesh, 'nCellsSolve', nCellsSolve)
      call mpas_pool_get_dimension(mesh, 'nEdges', nEdges)

! regional_MPAS: get specified zone cell mask
      call mpas_pool_get_array(mesh, 'bdyMaskCell', bdyMaskCell)

      call mpas_pool_get_array(mesh, 'zz', zz)
      call mpas_pool_get_array(mesh, 'zb', zb)
      call mpas_pool_get_array(mesh, 'zb3', zb3)
      call mpas_pool_get_array(mesh, 'zb_cell', zb_cell)
      call mpas_pool_get_array(mesh, 'zb3_cell', zb3_cell)
      call mpas_pool_get_array(mesh, 'fzm', fzm)
      call mpas_pool_get_array(mesh, 'fzp', fzp)
      call mpas_pool_get_array(mesh, 'cellsOnEdge', cellsOnEdge)
      call mpas_pool_get_array(mesh, 'nEdgesOnCell', nEdgesOnCell)
      call mpas_pool_get_array(mesh, 'edgesOnCell', edgesOnCell)
      call mpas_pool_get_array(mesh, 'edgesOnCell_sign', edgesOnCell_sign)

      call mpas_pool_get_array(tend, 'w', w_tend)
      call mpas_pool_get_array(tend, 'u', u_tend)

      call mpas_pool_get_array(diag, 'ruAvg', ruAvg)
      call mpas_pool_get_array(diag, 'wwAvg', wwAvg)

      call mpas_pool_get_array(diag, 'rho_pp', rho_pp)
      call mpas_pool_get_array(diag, 'rho_p_save', rho_p_save)
      call mpas_pool_get_array(diag, 'rho_p', rho_p)

      call mpas_pool_get_array(diag, 'ru_p', ru_p)
      call mpas_pool_get_array(diag, 'ru_save', ru_save)
      call mpas_pool_get_array(diag, 'ru', ru)

      call mpas_pool_get_array(diag, 'rtheta_pp', rtheta_pp)
      call mpas_pool_get_array(diag, 'rtheta_p_save', rtheta_p_save)
      call mpas_pool_get_array(diag, 'rtheta_p', rtheta_p)
      call mpas_pool_get_array(diag, 'rtheta_pp_old', rtheta_pp_old)

      call mpas_pool_get_array(diag, 'rw_p', rw_p)
      call mpas_pool_get_array(diag, 'rw_save', rw_save)
      call mpas_pool_get_array(diag, 'rw', rw)

      call atm_set_smlstep_pert_variables_work(nCells, nEdges, nCellsSolve, &
                                   nEdgesOnCell, cellsOnEdge, edgesOnCell, fzm, fzp, ruAvg, wwAvg, zb, zb3, zb_cell, zb3_cell, &
                                   zz, w_tend, u_tend, rho_pp, rho_p_save, rho_p, ru_p, ru, ru_save, &
                                   rtheta_pp, rtheta_p_save, rtheta_p, rtheta_pp_old, rw_p, rw_save, rw, &
                                   bdyMaskCell, &  ! added for regional_MPAS
                                   edgesOnCell_sign, &
                                   cellStart, cellEnd, edgeStart, edgeEnd, &
                                   cellSolveStart, cellSolveEnd, edgeSolveStart, edgeSolveEnd)


   end subroutine atm_set_smlstep_pert_variables


   subroutine atm_set_smlstep_pert_variables_work(nCells, nEdges, nCellsSolve, &
                                   nEdgesOnCell, cellsOnEdge, edgesOnCell, fzm, fzp, ruAvg, wwAvg, zb, zb3, zb_cell, zb3_cell, &
                                   zz, w_tend, u_tend, rho_pp, rho_p_save, rho_p, ru_p, ru, ru_save, &
                                   rtheta_pp, rtheta_p_save, rtheta_p, rtheta_pp_old, rw_p, rw_save, rw, &
                                   bdyMaskCell, &  ! added for regional_MPAS
                                   edgesOnCell_sign, &
                                   cellStart, cellEnd, edgeStart, edgeEnd, &
                                   cellSolveStart, cellSolveEnd, edgeSolveStart, edgeSolveEnd)

      use mpas_atm_dimensions

      implicit none


      !
      ! Dummy arguments
      !
      integer, intent(in) :: nCells, nEdges, nCellsSolve

      integer, intent(in) :: cellStart, cellEnd, edgeStart, edgeEnd
      integer, intent(in) :: cellSolveStart, cellSolveEnd, edgeSolveStart, edgeSolveEnd

      integer, dimension(nCells+1) :: nEdgesOnCell
      integer, dimension(2,nEdges+1) :: cellsOnEdge
      integer, dimension(maxEdges,nCells+1) :: edgesOnCell
      real (kind=RKIND), dimension(nVertLevels) :: fzm
      real (kind=RKIND), dimension(nVertLevels) :: fzp
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: ruAvg
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: wwAvg
      real (kind=RKIND), dimension(nVertLevels+1,2,nEdges+1) :: zb
      real (kind=RKIND), dimension(nVertLevels+1,2,nEdges+1) :: zb3
      real (kind=RKIND), dimension(nVertLevels+1,maxEdges,nCells+1) :: zb_cell
      real (kind=RKIND), dimension(nVertLevels+1,maxEdges,nCells+1) :: zb3_cell
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: zz
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: w_tend
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: u_tend
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rho_pp
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rho_p_save
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rho_p
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: ru_p
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: ru
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: ru_save
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rtheta_pp
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rtheta_p_save
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rtheta_p
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rtheta_pp_old
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: rw_p
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: rw_save
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: rw
      real (kind=RKIND), dimension(maxEdges,nCells+1) :: edgesOnCell_sign

      integer, dimension(nCells+1), intent(in) :: bdyMaskCell  !  added for regional_MPAS

      !
      ! Local variables
      !
      integer :: iCell, iEdge, i, k
      real (kind=RKIND) :: flux

      ! we solve for omega instead of w (see Klemp et al MWR 2007),
      ! so here we change the w_p tendency to an omega_p tendency

      ! here we need to compute the omega tendency in a manner consistent with our diagnosis of omega.
      ! this requires us to use the same flux divergence as is used in the theta eqn - see Klemp et al MWR 2003.

!!      do iCell=cellStart,cellEnd
      do iCell=cellSolveStart,cellSolveEnd

         if (bdyMaskCell(iCell) <= nRelaxZone) then  !  no conversion in specified zone, regional_MPAS
         do i=1,nEdgesOnCell(iCell)
            iEdge = edgesOnCell(i,iCell)
!DIR$ IVDEP
            do k = 2, nVertLevels
               flux = edgesOnCell_sign(i,iCell) * (fzm(k) * u_tend(k,iEdge) + fzp(k) * u_tend(k-1,iEdge))
               w_tend(k,iCell) = w_tend(k,iCell)   &
                        - (zb_cell(k,i,iCell) + sign(1.0_RKIND, u_tend(k,iEdge)) * zb3_cell(k,i,iCell)) * flux
            end do
         end do
!DIR$ IVDEP
         do k = 2, nVertLevels
            w_tend(k,iCell) = ( fzm(k) * zz(k,iCell) + fzp(k) * zz(k-1,iCell)   ) * w_tend(k,iCell)
         end do
         end if ! no conversion in specified zone
      end do

   end subroutine atm_set_smlstep_pert_variables_work


   subroutine atm_advance_acoustic_step( state, diag, tend, mesh, configs, nCells, nVertLevels, dts, small_step, &
                                   cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd, &
                                   cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd)

      !  This subroutine performs the entire acoustic step update, following Klemp et al MWR 2007,
      !  using forward-backward vertically implicit integration.  
      !  The gravity-waves are included in the acoustic-step integration.
      !  The input state variables that are updated are ru_p, rw_p (note that this is (rho*omega)_p here),
      !  rtheta_p, and rho_pp.  The time averaged mass flux is accumulated in ruAvg and wwAvg

      implicit none

      type (mpas_pool_type), intent(inout) :: state
      type (mpas_pool_type), intent(inout) :: diag
      type (mpas_pool_type), intent(inout) :: tend
      type (mpas_pool_type), intent(inout) :: mesh
      type (mpas_pool_type), intent(in)    :: configs
      integer, intent(in) :: small_step              ! acoustic step number
      integer, intent(in) :: nCells                  ! for allocating stack variables
      integer, intent(in) :: nVertLevels             ! for allocating stack variables
      real (kind=RKIND), intent(in) :: dts
      integer, intent(in) :: cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd
      integer, intent(in) :: cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd


      real (kind=RKIND), dimension(nVertLevels) :: du
      real (kind=RKIND), dimension(nVertLevels+1) :: dpzx

      real (kind=RKIND), dimension(:,:), pointer :: rho_zz, theta_m, ru_p, rw_p, rtheta_pp,  &
                                                    rtheta_pp_old, zz, exner, cqu, ruAvg, &
                                                    wwAvg, rho_pp, cofwt, coftz, zxu,        &
                                                    a_tri, alpha_tri, gamma_tri, dss,        &
                                                    tend_ru, tend_rho, tend_rt, tend_rw,     &
                                                    zgrid, cofwr, cofwz, w

! redefine ru_p to be perturbation from time t, change 3a  ! temporary
      real (kind=RKIND), dimension(:,:), pointer :: ru
      real (kind=RKIND), dimension(:,:), pointer :: ru_save
! redefine rw_p to be perturbation from time t, change 3a  ! temporary
      real (kind=RKIND), dimension(:,:), pointer :: rw
      real (kind=RKIND), dimension(:,:), pointer :: rw_save

      real (kind=RKIND), dimension(:), pointer :: fzm, fzp, rdzw, dcEdge, invDcEdge, invAreaCell, cofrz, dvEdge

      integer, dimension(:), pointer :: nEdgesOnCell
      real (kind=RKIND), dimension(:), pointer :: specZoneMaskCell, specZoneMaskEdge
      integer, dimension(:,:), pointer :: cellsOnEdge, edgesOnCell
      real (kind=RKIND), dimension(:,:), pointer :: edgesOnCell_sign

      real (kind=RKIND), pointer :: epssm

      real (kind=RKIND), pointer :: cf1, cf2, cf3

      integer, pointer :: nEdges, nCellsSolve

      ! MW on ITM: tendency variables 
      logical, pointer :: config_tend
      real (kind=RKIND), dimension(:,:), pointer :: ru_tend_dyn_small, rth_tend_dyn_small

      call mpas_pool_get_array(mesh, 'cellsOnEdge', cellsOnEdge)
      call mpas_pool_get_array(mesh, 'edgesOnCell', edgesOnCell)
      call mpas_pool_get_array(mesh, 'nEdgesOnCell', nEdgesOnCell)
      call mpas_pool_get_array(mesh, 'edgesOnCell_sign', edgesOnCell_sign)
      call mpas_pool_get_array(mesh, 'specZoneMaskEdge', specZoneMaskEdge)
      call mpas_pool_get_array(mesh, 'specZoneMaskCell', specZoneMaskCell)

      call mpas_pool_get_array(state, 'rho_zz', rho_zz, 2)
!      call mpas_pool_get_array(state, 'theta_m', theta_m, 2)
      call mpas_pool_get_array(state, 'theta_m', theta_m, 1)
!  change needed for rw_p, change 6 (see rayleigh damping)
      call mpas_pool_get_array(state, 'w', w, 2)
!      call mpas_pool_get_array(state, 'w', w, 1)

      call mpas_pool_get_array(diag, 'rtheta_pp', rtheta_pp)
      call mpas_pool_get_array(diag, 'rtheta_pp_old', rtheta_pp_old)
      call mpas_pool_get_array(diag, 'ru_p', ru_p)
      call mpas_pool_get_array(diag, 'rw_p', rw_p)
      call mpas_pool_get_array(diag, 'exner', exner)
      call mpas_pool_get_array(diag, 'cqu', cqu)
      call mpas_pool_get_array(diag, 'ruAvg', ruAvg)
      call mpas_pool_get_array(diag, 'wwAvg', wwAvg)
      call mpas_pool_get_array(diag, 'rho_pp', rho_pp)
      call mpas_pool_get_array(diag, 'cofwt', cofwt)
      call mpas_pool_get_array(diag, 'coftz', coftz)
      call mpas_pool_get_array(diag, 'cofrz', cofrz)
      call mpas_pool_get_array(diag, 'cofwr', cofwr)
      call mpas_pool_get_array(diag, 'cofwz', cofwz)
      call mpas_pool_get_array(diag, 'a_tri', a_tri)
      call mpas_pool_get_array(diag, 'alpha_tri', alpha_tri)
      call mpas_pool_get_array(diag, 'gamma_tri', gamma_tri)

      call mpas_pool_get_array(mesh, 'dss', dss)

      call mpas_pool_get_array(tend, 'u', tend_ru)
      call mpas_pool_get_array(tend, 'rho_zz', tend_rho)
      call mpas_pool_get_array(tend, 'theta_m', tend_rt)
      call mpas_pool_get_array(tend, 'w', tend_rw)

      call mpas_pool_get_array(mesh, 'zz', zz)
      call mpas_pool_get_array(mesh, 'zxu', zxu)
      call mpas_pool_get_array(mesh, 'zgrid', zgrid)
      call mpas_pool_get_array(mesh, 'fzm', fzm)
      call mpas_pool_get_array(mesh, 'fzp', fzp)
      call mpas_pool_get_array(mesh, 'rdzw', rdzw)
      call mpas_pool_get_array(mesh, 'dcEdge', dcEdge)
      call mpas_pool_get_array(mesh, 'invDcEdge', invDcEdge)
      call mpas_pool_get_array(mesh, 'dvEdge', dvEdge)
      call mpas_pool_get_array(mesh, 'invAreaCell', invAreaCell)

      call mpas_pool_get_dimension(mesh, 'nEdges', nEdges)
      call mpas_pool_get_dimension(mesh, 'nCellsSolve', nCellsSolve)

      call mpas_pool_get_array(mesh, 'cf1', cf1)
      call mpas_pool_get_array(mesh, 'cf2', cf2)
      call mpas_pool_get_array(mesh, 'cf3', cf3)

! redefine ru_p to be perturbation from time t, change 3b ! temporary
      call mpas_pool_get_array(diag, 'ru', ru)
      call mpas_pool_get_array(diag, 'ru_save', ru_save)
! redefine rw_p to be perturbation from time t, change 3b ! temporary
      call mpas_pool_get_array(diag, 'rw', rw)
      call mpas_pool_get_array(diag, 'rw_save', rw_save)

      ! epssm is the offcentering coefficient for the vertically implicit integration.
      call mpas_pool_get_config(configs, 'config_epssm', epssm) 


      ! MW on ITM: accumulate acoustic time-step dynamic tendencies 
      call mpas_pool_get_config(configs, 'config_tend', config_tend)

      if ( config_tend ) then
         call mpas_pool_get_array(diag, 'ru_tend_dyn_small', ru_tend_dyn_small)
         call mpas_pool_get_array(diag, 'rth_tend_dyn_small', rth_tend_dyn_small)
      else
         allocate(ru_tend_dyn_small(nVertLevels,nEdges+1))
         allocate(rth_tend_dyn_small(nVertLevels,nCells+1))
      endif


      call atm_advance_acoustic_step_work(nCells, nEdges, nCellsSolve, cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd, &
                                   cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd, &
                                   rho_zz, theta_m, ru_p, rw_p, rtheta_pp, rtheta_pp_old, zz, exner, cqu, ruAvg, wwAvg, &
                                   rho_pp, cofwt, coftz, zxu, a_tri, alpha_tri, gamma_tri, dss, tend_ru, tend_rho, tend_rt, &
                                   tend_rw, zgrid, cofwr, cofwz, w, ru, ru_save, rw, rw_save, fzm, fzp, rdzw, dcEdge, invDcEdge, &
                                   invAreaCell, cofrz, dvEdge, nEdgesOnCell, cellsOnEdge, edgesOnCell, edgesOnCell_sign, &
                                   dts, small_step, epssm, cf1, cf2, cf3, &
                                   specZoneMaskEdge, specZoneMaskCell, &
                                   config_tend, ru_tend_dyn_small=ru_tend_dyn_small, rth_tend_dyn_small=rth_tend_dyn_small &  ! MW ITM
                                   )


       !MC: deallocation for ITM variables
       if (.not. config_tend) then
          deallocate(ru_tend_dyn_small)
          deallocate(rth_tend_dyn_small)
       endif


   end subroutine atm_advance_acoustic_step


   subroutine atm_advance_acoustic_step_work(nCells, nEdges, nCellsSolve, cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd, &
                                   cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd, &
                                   rho_zz, theta_m, ru_p, rw_p, rtheta_pp, rtheta_pp_old, zz, exner, cqu, ruAvg, wwAvg, &
                                   rho_pp, cofwt, coftz, zxu, a_tri, alpha_tri, gamma_tri, dss, tend_ru, tend_rho, tend_rt, &
                                   tend_rw, zgrid, cofwr, cofwz, w, ru, ru_save, rw, rw_save, fzm, fzp, rdzw, dcEdge, invDcEdge, &
                                   invAreaCell, cofrz, dvEdge, nEdgesOnCell, cellsOnEdge, edgesOnCell, edgesOnCell_sign, &
                                   dts, small_step, epssm, cf1, cf2, cf3, &
                                   specZoneMaskEdge, specZoneMaskCell, &
                                   config_tend, ru_tend_dyn_small, rth_tend_dyn_small &        ! MW on ITM
                                   )

      use mpas_atm_dimensions

      implicit none


      !
      ! Dummy arguments
      !
      integer, intent(in) :: nCells, nEdges, nCellsSolve
      integer, intent(in) :: cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd
      integer, intent(in) :: cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd

      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rho_zz
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: theta_m
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: ru_p
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: rw_p
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rtheta_pp

      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rtheta_pp_old
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: zz
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: exner
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: cqu
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: ruAvg
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: wwAvg
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rho_pp
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: cofwt
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: coftz
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: zxu

      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: a_tri
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: alpha_tri
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: gamma_tri
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: dss
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: tend_ru
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: tend_rho
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: tend_rt
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: tend_rw
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: zgrid
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: cofwr
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: cofwz

      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: w
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: ru
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: ru_save
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: rw
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: rw_save

      real (kind=RKIND), dimension(nVertLevels) :: fzm
      real (kind=RKIND), dimension(nVertLevels) :: fzp
      real (kind=RKIND), dimension(nVertLevels) :: rdzw
      real (kind=RKIND), dimension(nEdges+1) :: dcEdge
      real (kind=RKIND), dimension(nEdges+1) :: invDcEdge
      real (kind=RKIND), dimension(nCells+1) :: invAreaCell
      real (kind=RKIND), dimension(nVertLevels) :: cofrz
      real (kind=RKIND), dimension(nEdges+1) :: dvEdge

      integer, dimension(nCells+1) :: nEdgesOnCell
      integer, dimension(2,nEdges+1) :: cellsOnEdge
      integer, dimension(maxEdges,nCells+1) :: edgesOnCell
      real (kind=RKIND), dimension(maxEdges,nCells+1) :: edgesOnCell_sign

      real (kind=RKIND), dimension(nCells+1) :: specZoneMaskCell
      real (kind=RKIND), dimension(nEdges+1) :: specZoneMaskEdge
      

      integer, intent(in) :: small_step
      real (kind=RKIND), intent(in) :: dts, epssm,cf1, cf2, cf3
      real (kind=RKIND), dimension(nVertLevels) :: ts, rs

  
      ! MW variables for ITM 
      ! MC: optional args don't work as intended; code will break without var allocations
      logical, intent(in) :: config_tend
      real (kind=RKIND), dimension(nVertLevels,nEdges+1), optional :: ru_tend_dyn_small
      real (kind=RKIND), dimension(nVertLevels,nCells+1), optional :: rth_tend_dyn_small


      !
      ! Local variables
      !
      integer :: cell1, cell2, iEdge, iCell, i, k
      real (kind=RKIND) :: c2, rcv, rtheta_pp_tmp
      real (kind=RKIND) :: pgrad, flux, resm, rdts


      rcv = rgas / (cp - rgas)
      c2 = cp * rcv
      resm = (1.0 - epssm) / (1.0 + epssm)
      rdts = 1./dts

      if(small_step /= 1) then  !  not needed on first small step 

        ! forward-backward acoustic step integration.
        ! begin by updating the horizontal velocity u, 
        ! and accumulating the contribution from the updated u to the other tendencies.

        ! we are looping over all edges, but only computing on edges of owned cells. This will include updates of
        ! all owned edges plus some edges that are owned by other blocks.  We perform these redundant computations
        ! so that we do not have to communicate updates of u to update the cell variables (rho, w, and theta). 

        !MGD this loop will not be very load balanced with if-test below

        do iEdge=edgeStart,edgeEnd ! MGD do we really just need edges touching owned cells?
 
           cell1 = cellsOnEdge(1,iEdge)
           cell2 = cellsOnEdge(2,iEdge)

           ! update edges for block-owned cells
           if (cell1 <= nCellsSolve .or. cell2 <= nCellsSolve ) then

!DIR$ IVDEP
              do k=1,nVertLevels
                 pgrad = ((rtheta_pp(k,cell2)-rtheta_pp(k,cell1))*invDcEdge(iEdge) )/(.5*(zz(k,cell2)+zz(k,cell1)))
                 pgrad = cqu(k,iEdge)*0.5*c2*(exner(k,cell1)+exner(k,cell2))*pgrad
                 pgrad = pgrad + 0.5*zxu(k,iEdge)*gravity*(rho_pp(k,cell1)+rho_pp(k,cell2))
                 ru_p(k,iEdge) = ru_p(k,iEdge) + dts*(tend_ru(k,iEdge) - (1.0_RKIND - specZoneMaskEdge(iEdge))*pgrad)

                 if ( config_tend ) then      ! MW on ITM
                    ru_tend_dyn_small(k,iEdge) = ru_tend_dyn_small(k,iEdge) -dts*pgrad 
                 endif
              end do

   
              ! accumulate ru_p for use later in scalar transport
!DIR$ IVDEP
              do k=1,nVertLevels
                 ruAvg(k,iEdge) = ruAvg(k,iEdge) + ru_p(k,iEdge)
              end do

           end if ! end test for block-owned cells

        end do ! end loop over edges

      else !  this is all that us needed for ru_p update for first acoustic step in RK substep

        do iEdge=edgeStart,edgeEnd ! MGD do we really just need edges touching owned cells?

           cell1 = cellsOnEdge(1,iEdge)
           cell2 = cellsOnEdge(2,iEdge)

           ! update edges for block-owned cells
           if (cell1 <= nCellsSolve .or. cell2 <= nCellsSolve ) then

!DIR$ IVDEP
              do k=1,nVertLevels
                 ru_p(k,iEdge) = dts*tend_ru(k,iEdge)

                 if (config_tend) then          
                    ru_tend_dyn_small(k,iEdge) = 0.  ! MW on ITM: zero-ing for first small timestep; only accumulate during RK_STEP
                 endif
              end do
!DIR$ IVDEP
              do k=1,nVertLevels
                 ruAvg(k,iEdge) = ru_p(k,iEdge)                 
              end do

           end if ! end test for block-owned cells

        end do ! end loop over edges

      end if ! test for first acoustic step

      if (small_step == 1) then  ! initialize here on first small timestep.
         do iCell=cellStart,cellEnd
            rtheta_pp_old(1:nVertLevels,iCell) = 0.0
         end do
      else
         do iCell=cellStart,cellEnd
            rtheta_pp_old(1:nVertLevels,iCell) = rtheta_pp(1:nVertLevels,iCell)
         end do
      end if

!$OMP BARRIER

      do iCell=cellSolveStart,cellSolveEnd  ! loop over all owned cells to solve

         if(small_step == 1) then  ! initialize here on first small timestep.
            wwAvg(1:nVertLevels+1,iCell) = 0.0            
            rho_pp(1:nVertLevels,iCell) = 0.0            
            rtheta_pp(1:nVertLevels,iCell) = 0.0            
            rw_p(:,iCell) = 0.0

            if (config_tend) rth_tend_dyn_small(1:nVertLevels,iCell) = 0.0 ! MW on ITM
         end if
            
         if(specZoneMaskCell(iCell) == 0.0) then  ! not specified zone, compute...

         ts(:) = 0.0
         rs(:) = 0.0

         do i=1,nEdgesOnCell(iCell) 
            iEdge = edgesOnCell(i,iCell)
            cell1 = cellsOnEdge(1,iEdge)
            cell2 = cellsOnEdge(2,iEdge)
!DIR$ IVDEP
            do k=1,nVertLevels
               flux = edgesOnCell_sign(i,iCell)*dts*dvEdge(iEdge)*ru_p(k,iEdge) * invAreaCell(iCell)
               rs(k) = rs(k)-flux
               ts(k) = ts(k)-flux*0.5*(theta_m(k,cell2)+theta_m(k,cell1))
            end do
         end do

      ! vertically implicit acoustic and gravity wave integration.
      ! this follows Klemp et al MWR 2007, with the addition of an implicit Rayleigh damping of w
      ! serves as a gravity-wave absorbing layer, from Klemp et al 2008.

!DIR$ IVDEP
         do k=1, nVertLevels

           ! MW on ITM 
            if (config_tend) then 
               rth_tend_dyn_small(k,iCell) = rth_tend_dyn_small(k,iCell) + ts(k)               &
                                            - resm*rdzw(k)*( coftz(k+1,iCell)*rw_p(k+1,iCell)  &
                                            - coftz(k,iCell)*rw_p(k,iCell))
            endif

            rs(k) = rho_pp(k,iCell) + dts*tend_rho(k,iCell) + rs(k)                  &
                            - cofrz(k)*resm*(rw_p(k+1,iCell)-rw_p(k,iCell)) 
            ts(k) = rtheta_pp(k,iCell) + dts*tend_rt(k,iCell) + ts(k)                &
                               - resm*rdzw(k)*( coftz(k+1,iCell)*rw_p(k+1,iCell)     &
                                               -coftz(k,iCell)*rw_p(k,iCell))
         end do

!DIR$ IVDEP
         do k=2, nVertLevels
            wwavg(k,iCell) = wwavg(k,iCell) + 0.5*(1.0-epssm)*rw_p(k,iCell)
         end do

!DIR$ IVDEP
         do k=2, nVertLevels
            rw_p(k,iCell) = rw_p(k,iCell) +  dts*tend_rw(k,iCell)                       &
                       - cofwz(k,iCell)*((zz(k  ,iCell)*ts(k)                           &
                                     -zz(k-1,iCell)*ts(k-1))                            &
                               +resm*(zz(k  ,iCell)*rtheta_pp(k  ,iCell)                &
                                     -zz(k-1,iCell)*rtheta_pp(k-1,iCell)))              &
                       - cofwr(k,iCell)*((rs(k)+rs(k-1))                                &
                               +resm*(rho_pp(k,iCell)+rho_pp(k-1,iCell)))               &
                       + cofwt(k  ,iCell)*(ts(k  )+resm*rtheta_pp(k  ,iCell))           &
                       + cofwt(k-1,iCell)*(ts(k-1)+resm*rtheta_pp(k-1,iCell))
         end do

         ! tridiagonal solve sweeping up and then down the column

!MGD VECTOR DEPENDENCE
         do k=2,nVertLevels
            rw_p(k,iCell) = (rw_p(k,iCell)-a_tri(k,iCell)*rw_p(k-1,iCell))*alpha_tri(k,iCell)
         end do

!MGD VECTOR DEPENDENCE
         do k=nVertLevels,1,-1
            rw_p(k,iCell) = rw_p(k,iCell) - gamma_tri(k,iCell)*rw_p(k+1,iCell)     
         end do

         ! the implicit Rayleigh damping on w (gravity-wave absorbing) 

!DIR$ IVDEP
         do k=2,nVertLevels
            rw_p(k,iCell) = (rw_p(k,iCell) + (rw_save(k,iCell) - rw(k,iCell)) -dts*dss(k,iCell)  &
                            * (fzm(k)*zz(k,iCell)+fzp(k)*zz(k-1,iCell))                          &
                            * (fzm(k)*rho_zz(k,iCell)+fzp(k)*rho_zz(k-1,iCell))                  &
                            * w(k,iCell))/(1.0+dts*dss(k,iCell))                                 &
                            - (rw_save(k,iCell) - rw(k,iCell))
         end do

         ! accumulate (rho*omega)' for use later in scalar transport
!DIR$ IVDEP
         do k=2,nVertLevels
            wwAvg(k,iCell) = wwAvg(k,iCell) + 0.5*(1.0+epssm)*rw_p(k,iCell)
         end do

!DIR$ IVDEP
         do k=1,nVertLevels
            ! MW on ITM 
            ! note: rth_tend_dyn_small possibly needs to be recalculated using full theta values
            !       to account for round-off errors. This can be done after the last small time-step 
            !       in atm_recover_large_step_variables. 
            if (config_tend) then 
               rth_tend_dyn_small(k,iCell) = rth_tend_dyn_small(k,iCell)                 &
                                            - rdzw(k)*(coftz(k+1,iCell)*rw_p(k+1,iCell)  &
                                            - coftz(k,iCell)*rw_p(k,iCell))  
            endif

            ! update rho_pp and theta_pp given updated rw_p  
            rho_pp(k,iCell) = rs(k) - cofrz(k) *(rw_p(k+1,iCell)-rw_p(k,iCell))
            rtheta_pp(k,iCell) = ts(k) - rdzw(k)*(coftz(k+1,iCell)*rw_p(k+1,iCell)  &
                               -coftz(k,iCell)*rw_p(k,iCell))
         end do

         else ! specifed zone in regional_MPAS

            do k=1,nVertLevels
               rho_pp(k,iCell) = rho_pp(k,iCell) + dts*tend_rho(k,iCell)
               rtheta_pp(k,iCell) = rtheta_pp(k,iCell) + dts*tend_rt(k,iCell)
               rw_p(k,iCell) = rw_p(k,iCell) + dts*tend_rw(k,iCell)
               wwAvg(k,iCell) = wwAvg(k,iCell) + 0.5*(1.0+epssm)*rw_p(k,iCell)
            end do

         end if

      end do !  end of loop over cells

   end subroutine atm_advance_acoustic_step_work


   subroutine atm_divergence_damping_3d( state, diag, mesh, configs, dts, edgeStart, edgeEnd )

      !  This subroutine updates the horizontal momentum with the 3d divergence damping component.

      implicit none

      type (mpas_pool_type), intent(inout) :: state
      type (mpas_pool_type), intent(inout) :: diag
      type (mpas_pool_type), intent(inout) :: mesh
      type (mpas_pool_type), intent(in) :: configs
      real (kind=RKIND), intent(in) :: dts
      integer, intent(in) :: edgeStart, edgeEnd

      real (kind=RKIND), dimension(:,:), pointer :: theta_m, ru_p, rtheta_pp, rtheta_pp_old
!      real (kind=RKIND), dimension(:), pointer :: dcEdge
      real (kind=RKIND), pointer :: smdiv, config_len_disp
      real (kind=RKIND), dimension(:), pointer :: specZoneMaskEdge

      integer, dimension(:,:), pointer :: cellsOnEdge
      integer, pointer :: nCellsSolve
      integer, pointer :: nVertLevels

      real (kind=RKIND) :: divCell1, divCell2, rdts, coef_divdamp
      integer :: cell1, cell2, iEdge, k

      !MW on ITM
      logical, pointer :: config_tend
      real (kind=RKIND), dimension(:,:), pointer :: ru_tend_smdiv

      call mpas_pool_get_array(mesh, 'cellsOnEdge', cellsOnEdge)
      call mpas_pool_get_array(mesh, 'specZoneMaskEdge', specZoneMaskEdge)
!      call mpas_pool_get_array(mesh, 'dcEdge', dcEdge)
      call mpas_pool_get_array(state, 'theta_m', theta_m, 1)
      call mpas_pool_get_array(diag, 'rtheta_pp', rtheta_pp)
      call mpas_pool_get_array(diag, 'rtheta_pp_old', rtheta_pp_old)
      call mpas_pool_get_array(diag, 'ru_p', ru_p)

      call mpas_pool_get_dimension(mesh, 'nCellsSolve', nCellsSolve)
      call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)

      call mpas_pool_get_config(configs, 'config_smdiv', smdiv) 
      call mpas_pool_get_config(configs, 'config_len_disp', config_len_disp)

      ! MW on ITM 
      call mpas_pool_get_config(configs, 'config_tend', config_tend)
      if (config_tend) call mpas_pool_get_array(diag, 'ru_tend_smdiv', ru_tend_smdiv)


      rdts = 1.0_RKIND / dts
      coef_divdamp = 2.0_RKIND * smdiv * config_len_disp * rdts

      do iEdge=edgeStart,edgeEnd ! MGD do we really just need edges touching owned cells?

         cell1 = cellsOnEdge(1,iEdge)
         cell2 = cellsOnEdge(2,iEdge)

         ! update edges for block-owned cells
         if (cell1 <= nCellsSolve .or. cell2 <= nCellsSolve ) then

!DIR$ IVDEP
            do k=1,nVertLevels

!!  unscaled 3d divergence damping
!!               divCell1 = -(rtheta_pp(k,cell1)-rtheta_pp_old(k,cell1))*rdts
!!               divCell2 = -(rtheta_pp(k,cell2)-rtheta_pp_old(k,cell2))*rdts
!!               ru_p(k,iEdge) = ru_p(k,iEdge) + 2.*smdiv*dcEdge(iEdge)*(divCell2-divCell1) &
!!                                                      /(theta_m(k,cell1)+theta_m(k,cell2))

!!  scaled 3d divergence damping
               divCell1 = -(rtheta_pp(k,cell1)-rtheta_pp_old(k,cell1))
               divCell2 = -(rtheta_pp(k,cell2)-rtheta_pp_old(k,cell2))
               ru_p(k,iEdge) = ru_p(k,iEdge) + coef_divdamp*(divCell2-divCell1)*(1.0_RKIND - specZoneMaskEdge(iEdge)) &
                                                      /(theta_m(k,cell1)+theta_m(k,cell2))

               ! MW on ITM 
               if (config_tend) then 
                  ru_tend_smdiv(k,iEdge) = ru_tend_smdiv(k,iEdge) + coef_divdamp*(divCell2-divCell1) &
                                           / (theta_m(k,cell1)+theta_m(k,cell2))
               end if

            end do
         end if ! edges for block-owned cells
      end do ! end loop over edges

   end subroutine atm_divergence_damping_3d


   subroutine atm_recover_large_step_variables( state, diag, tend, mesh, configs, dt, ns, rk_step, &
                                       cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd, &
                                       cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd)

      ! reconstitute state variables from acoustic-step perturbation variables 
      ! after the acoustic steps.  The perturbation variables were originally set in
      ! subroutine atm_set_smlstep_pert_variables prior to their acoustic-steps update.
      ! we are also computing a few other state-derived variables here.

      implicit none

      type (mpas_pool_type), intent(inout) :: state
      type (mpas_pool_type), intent(inout) :: diag
      type (mpas_pool_type), intent(inout) :: tend
      type (mpas_pool_type), intent(inout) :: mesh
      type (mpas_pool_type), intent(in) :: configs
      integer, intent(in) :: ns, rk_step
      real (kind=RKIND), intent(in) :: dt
      integer, intent(in) :: cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd
      integer, intent(in) :: cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd


      real (kind=RKIND), dimension(:,:), pointer :: wwAvg, rw_save, w, rw, rw_p, rtheta_p, rtheta_pp,   &
                                                    rtheta_p_save, rt_diabatic_tend, rho_p, rho_p_save, &
                                                    rho_pp, rho_zz, rho_base, ruAvg, ru_save, ru_p, u, ru, &
                                                    exner, exner_base, rtheta_base, pressure_p,         &
                                                    zz, theta_m, pressure_b
      real (kind=RKIND), dimension(:,:,:), pointer :: scalars
      real (kind=RKIND), dimension(:), pointer :: fzm, fzp
      real (kind=RKIND), dimension(:,:,:), pointer :: zb, zb3, zb_cell, zb3_cell
      real (kind=RKIND), dimension(:,:), pointer :: edgesOnCell_sign
      integer, dimension(:,:), pointer :: cellsOnEdge, edgesOnCell
      integer, dimension(:), pointer :: nEdgesOnCell

      ! MW for ITM and PV tendencies  
      logical, pointer :: config_tend, config_pv_tend
      real (kind=RKIND), dimension(:,:), pointer :: u_tend_dcpl, w_tend_dcpl, th_tend_dcpl, rho_zz_1
      real (kind=RKIND), dimension(:,:), pointer :: ru_tend_dyn_small, rw_tend_dyn_small              ! for PV but currently in tendency subroutines
      real (kind=RKIND), dimension(:,:), pointer :: w1                                                ! for PV but currently in tendency subroutines

      integer :: i, iCell, iEdge, k, cell1, cell2
      integer, pointer :: nVertLevels, nCells, nCellsSolve, nEdges, nEdgesSolve
      real (kind=RKIND) :: invNs, rcv, p0, flux
      real (kind=RKIND), pointer :: cf1, cf2, cf3

      integer, dimension(:), pointer :: bdyMaskCell  ! MPAS_regional addition

      call mpas_pool_get_array(diag, 'wwAvg', wwAvg)
      call mpas_pool_get_array(diag, 'rw_save', rw_save)
      call mpas_pool_get_array(diag, 'rw', rw)
      call mpas_pool_get_array(diag, 'rw_p', rw_p)
      call mpas_pool_get_array(state, 'w', w, 2)

      call mpas_pool_get_array(diag, 'rtheta_p', rtheta_p)
      call mpas_pool_get_array(diag, 'rtheta_p_save', rtheta_p_save)
      call mpas_pool_get_array(diag, 'rtheta_pp', rtheta_pp)
      call mpas_pool_get_array(diag, 'rtheta_base', rtheta_base)
      call mpas_pool_get_array(tend, 'rt_diabatic_tend', rt_diabatic_tend)
      call mpas_pool_get_array(state, 'theta_m', theta_m, 2)
      call mpas_pool_get_array(state, 'scalars', scalars, 2)

      call mpas_pool_get_array(state, 'rho_zz', rho_zz, 2)
      call mpas_pool_get_array(diag, 'rho_p', rho_p)
      call mpas_pool_get_array(diag, 'rho_p_save', rho_p_save)
      call mpas_pool_get_array(diag, 'rho_pp', rho_pp)
      call mpas_pool_get_array(diag, 'rho_base', rho_base)

      call mpas_pool_get_array(diag, 'ruAvg', ruAvg)
      call mpas_pool_get_array(diag, 'ru_save', ru_save)
      call mpas_pool_get_array(diag, 'ru_p', ru_p)
      call mpas_pool_get_array(diag, 'ru', ru)
      call mpas_pool_get_array(state, 'u', u, 2)

      call mpas_pool_get_array(diag, 'exner', exner)
      call mpas_pool_get_array(diag, 'exner_base', exner_base)

      call mpas_pool_get_array(diag, 'pressure_p', pressure_p)
      call mpas_pool_get_array(diag, 'pressure_base', pressure_b)

      call mpas_pool_get_array(mesh, 'bdyMaskCell', bdyMaskCell)  ! addition for regional_MPAS

      call mpas_pool_get_array(mesh, 'zz', zz)
      call mpas_pool_get_array(mesh, 'zb', zb)
      call mpas_pool_get_array(mesh, 'zb3', zb3)
      call mpas_pool_get_array(mesh, 'zb_cell', zb_cell)
      call mpas_pool_get_array(mesh, 'zb3_cell', zb3_cell)
      call mpas_pool_get_array(mesh, 'fzm', fzm)
      call mpas_pool_get_array(mesh, 'fzp', fzp)
      call mpas_pool_get_array(mesh, 'cellsOnEdge', cellsOnEdge)
      call mpas_pool_get_array(mesh, 'edgesOnCell', edgesOnCell)
      call mpas_pool_get_array(mesh, 'edgesOnCell_sign', edgesOnCell_sign)
      call mpas_pool_get_array(mesh, 'nEdgesOnCell', nEdgesOnCell)

      call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)
      call mpas_pool_get_dimension(mesh, 'nCells', nCells)
      call mpas_pool_get_dimension(mesh, 'nCellsSolve', nCellsSolve)
      call mpas_pool_get_dimension(mesh, 'nEdges', nEdges)
      call mpas_pool_get_dimension(mesh, 'nEdgesSolve', nEdgesSolve)

      call mpas_pool_get_array(mesh, 'cf1', cf1)
      call mpas_pool_get_array(mesh, 'cf2', cf2)
      call mpas_pool_get_array(mesh, 'cf3', cf3)

      ! For tendency and PV tendency diagnostics:
      !
      ! MC: Conditional variables based on config flags
      call mpas_pool_get_config(configs, 'config_tend', config_tend)
      call mpas_pool_get_config(configs, 'config_pv_tend', config_pv_tend)

      call mpas_pool_get_array(state, 'rho_zz', rho_zz_1, 1)
      call mpas_pool_get_array(state, 'w', w1, 1)

      if (config_tend) then
         call mpas_pool_get_array(diag, 'u_tend_dcpl', u_tend_dcpl)
         call mpas_pool_get_array(diag, 'w_tend_dcpl', w_tend_dcpl)
         call mpas_pool_get_array(diag, 'th_tend_dcpl', th_tend_dcpl)
         call mpas_pool_get_array(diag, 'ru_tend_dyn_small', ru_tend_dyn_small)
         call mpas_pool_get_array(diag, 'rw_tend_dyn_small', rw_tend_dyn_small)
      else
         allocate(u_tend_dcpl(nVertLevels,nEdges+1))
         allocate(w_tend_dcpl(nVertLevels+1,nCells+1))
         allocate(th_tend_dcpl(nVertLevels,nCells+1))
         allocate(ru_tend_dyn_small(nVertLevels,nEdges+1))
         allocate(rw_tend_dyn_small(nVertLevels+1,nCells+1))
      endif
      ! end diagnostics


      call atm_recover_large_step_variables_work(nCells, nEdges, nCellsSolve, nEdgesSolve, dt, ns, rk_step, &
                             wwAvg, rw_save, w, rw, rw_p, rtheta_p, rtheta_pp, rtheta_p_save, rt_diabatic_tend, rho_p, &
                             rho_p_save, rho_pp, rho_zz, rho_base, ruAvg, ru_save, ru_p, u, ru, exner, exner_base, &
                             rtheta_base, pressure_p, zz, theta_m, pressure_b, scalars, fzm, fzp, &
                             zb, zb3, zb_cell, zb3_cell, edgesOnCell_sign, cellsOnEdge, edgesOnCell, nEdgesOnCell, &
                             cf1, cf2, cf3, &
                             bdyMaskCell, &  !  addition for regional_MPAS
                             cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd, &
                             cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd, &
                             config_tend, config_pv_tend, &
                             u_tend_dcpl=u_tend_dcpl, w_tend_dcpl=w_tend_dcpl, th_tend_dcpl=th_tend_dcpl, rho_zz_1=rho_zz_1, & ! MW on ITM
                             ru_tend_dyn_small=ru_tend_dyn_small, rw_tend_dyn_small=rw_tend_dyn_small, & ! MW - PV; to recover dw/dt from d(omega)/dt
                             w1=w1 &      ! MW: added for PV-wbudget
                             )


      ! MC: variable deallocation
      if (.not. config_tend) then
         deallocate(u_tend_dcpl)
         deallocate(w_tend_dcpl)
         deallocate(th_tend_dcpl)
         deallocate(ru_tend_dyn_small)
         deallocate(rw_tend_dyn_small)
      endif


   end subroutine atm_recover_large_step_variables


   subroutine atm_recover_large_step_variables_work(nCells, nEdges, nCellsSolve, nEdgesSolve, dt, ns, rk_step, &
                             wwAvg, rw_save, w, rw, rw_p, rtheta_p, rtheta_pp, rtheta_p_save, rt_diabatic_tend, rho_p, &
                             rho_p_save, rho_pp, rho_zz, rho_base, ruAvg, ru_save, ru_p, u, ru, exner, exner_base, &
                             rtheta_base, pressure_p, zz, theta_m, pressure_b, scalars, fzm, fzp, &
                             zb, zb3, zb_cell, zb3_cell, edgesOnCell_sign, cellsOnEdge, edgesOnCell, nEdgesOnCell, &
                             cf1, cf2, cf3, &
                             bdyMaskCell, &  ! addition for regional_MPAS
                             cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd, &
                             cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd, &
                             config_tend, config_pv_tend, &                      ! MW - tendency and PV tendencies
                             u_tend_dcpl, w_tend_dcpl, th_tend_dcpl, rho_zz_1, & ! MW on ITM, PV
                             ru_tend_dyn_small, rw_tend_dyn_small, & ! PV; to recover dw/dt from d(omega)/dt 
                             w1 &  ! MW: added for PV-wbudget
                             )

      use mpas_atm_dimensions

      implicit none


      !
      ! Dummy arguments
      !
      integer, intent(in) :: nCells, nEdges, nCellsSolve, nEdgesSolve
      integer, intent(in) :: ns, rk_step
      real (kind=RKIND), intent(in) :: dt

      integer, dimension(nCells+1), intent(in) :: bdyMaskCell
      
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: wwAvg
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: rw_save
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: w
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: rw
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: rw_p
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rtheta_p
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rtheta_pp
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rtheta_p_save
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rt_diabatic_tend
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rho_p
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rho_p_save
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rho_pp
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rho_zz
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rho_base
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: ruAvg
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: ru_save
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: ru_p
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: u
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: ru
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: exner
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: exner_base
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rtheta_base
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: pressure_p
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: zz
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: theta_m
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: pressure_b
      real (kind=RKIND), dimension(num_scalars,nVertLevels,nCells+1) :: scalars
      real (kind=RKIND), dimension(nVertLevels) :: fzm
      real (kind=RKIND), dimension(nVertLevels) :: fzp
      real (kind=RKIND), dimension(nVertLevels+1,2,nEdges+1) :: zb
      real (kind=RKIND), dimension(nVertLevels+1,2,nEdges+1) :: zb3
      real (kind=RKIND), dimension(nVertLevels+1,maxEdges,nCells+1) :: zb_cell
      real (kind=RKIND), dimension(nVertLevels+1,maxEdges,nCells+1) :: zb3_cell
      real (kind=RKIND), dimension(maxEdges,nCells+1) :: edgesOnCell_sign
      integer, dimension(2,nEdges+1) :: cellsOnEdge
      integer, dimension(maxEdges,nCells+1) :: edgesOnCell
      integer, dimension(nCells+1) :: nEdgesOnCell

      real (kind=RKIND) :: cf1, cf2, cf3

      integer, intent(in) :: cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd
      integer, intent(in) :: cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd

      ! For tendency and PV diagnostics
      logical, intent(in) :: config_tend, config_pv_tend
      real (kind=RKIND), dimension(nVertLevels,nCells+1),   optional :: rho_zz_1 ! from beginning of dynamic time step
      real (kind=RKIND), dimension(nVertLevels,nEdges+1),   optional :: u_tend_dcpl, ru_tend_dyn_small !, u_tend   ! grab from (tend, 'u'), need to make sure units for ru_dyn_small is ok for rw_dyn_small
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1), optional :: w_tend_dcpl, rw_tend_dyn_small !, rw_tend_dyn_large  ! here, dyn_large includes diffusion too
      real (kind=RKIND), dimension(nVertLevels,nCells+1),   optional :: th_tend_dcpl
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1), optional :: w1

      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rho_zz_save  ! local; save the before-update rho_zz for diabatic theta tendency
      ! diag end

      !
      ! Local variables
      !
      integer :: i, iCell, iEdge, k, cell1, cell2
      real (kind=RKIND) :: invNs, rcv, p0, flux


      rcv = rgas/(cp-rgas)
      p0 = 1.0e+05  ! this should come from somewhere else...

      ! Avoid FP errors caused by a potential division by zero below by 
      ! initializing the "garbage cell" of rho_zz to a non-zero value
      do k=1,nVertLevels
         rho_zz(k,nCells+1) = 1.0
         rho_zz_save(k,nCells+1) = 1.0         ! MW  
      end do

      ! compute new density everywhere so we can compute u from ru.
      ! we will also need it to compute theta_m below

      invNs = 1 / real(ns,RKIND)

      do iCell=cellStart,cellEnd

!DIR$ IVDEP
         do k = 1, nVertLevels
            rho_p(k,iCell) = rho_p_save(k,iCell) + rho_pp(k,iCell)
            rho_zz_save(k,iCell) = rho_zz(k,iCell)                    ! MW: saving time-level t rho_zz  

            rho_zz(k,iCell) = rho_p(k,iCell) + rho_base(k,iCell)
         end do

         w(1,iCell) = 0.0

!DIR$ IVDEP
         do k = 2, nVertLevels
            wwAvg(k,iCell) = rw_save(k,iCell) + (wwAvg(k,iCell) * invNs)
            rw(k,iCell) = rw_save(k,iCell) + rw_p(k,iCell)

          ! pick up part of diagnosed w from omega - divide by density later
            w(k,iCell) = rw(k,iCell)/(fzm(k)*zz(k,iCell)+fzp(k)*zz(k-1,iCell))
                                      
         end do

         w(nVertLevels+1,iCell) = 0.0

         if (rk_step == 3) then
!DIR$ IVDEP
            do k = 1, nVertLevels
               ! MW bug fix:
               !  rho_zz is not consistent with the rho_zz used in computing rt_diabatic_tend at time-level t 
               !  rho_zz_save (see above) is the time-level t rho_zz value before it is updated. 

               !rtheta_p(k,iCell) = rtheta_p_save(k,iCell) + rtheta_pp(k,iCell) &
               !                  - dt * rho_zz(k,iCell) * rt_diabatic_tend(k,iCell)
               !
               rtheta_p(k,iCell) = rtheta_p_save(k,iCell) + rtheta_pp(k,iCell) &
                                 - dt * rho_zz_save(k,iCell) * rt_diabatic_tend(k,iCell)

               theta_m(k,iCell) = (rtheta_p(k,iCell) + rtheta_base(k,iCell))/rho_zz(k,iCell)
               exner(k,iCell) = (zz(k,iCell)*(rgas/p0)*(rtheta_p(k,iCell)+rtheta_base(k,iCell)))**rcv
               ! pressure_p is perturbation pressure
               pressure_p(k,iCell) = zz(k,iCell) * rgas * (exner(k,iCell)*rtheta_p(k,iCell)+rtheta_base(k,iCell)  &
                                                          * (exner(k,iCell)-exner_base(k,iCell)))

               ! MW on ITM: compute decoupling term (change in rho_zz)
               if (config_tend) then 
                  th_tend_dcpl(k,iCell)  = (rho_zz_1(k,iCell) - rho_zz(k,iCell))/rho_zz_1(k,iCell) * &
                                            theta_m(k,iCell)
               end if 

            end do
         else
!DIR$ IVDEP
            do k = 1, nVertLevels
               rtheta_p(k,iCell) = rtheta_p_save(k,iCell) + rtheta_pp(k,iCell)
               theta_m(k,iCell) = (rtheta_p(k,iCell) + rtheta_base(k,iCell))/rho_zz(k,iCell)
            end do
         end if

      end do

      ! recover time-averaged ruAvg on all edges of owned cells (for upcoming scalar transport).  
      ! we solved for these in the acoustic-step loop.  
      ! we will compute ru and u here also, given we are here, even though we only need them on nEdgesSolve

!$OMP BARRIER

      do iEdge=edgeStart,edgeEnd

         cell1 = cellsOnEdge(1,iEdge)
         cell2 = cellsOnEdge(2,iEdge)

!DIR$ IVDEP
         do k = 1, nVertLevels
            ruAvg(k,iEdge) = ru_save(k,iEdge) + (ruAvg(k,iEdge) * invNs)
            ru(k,iEdge) = ru_save(k,iEdge) + ru_p(k,iEdge)
            u(k,iEdge) = 2.*ru(k,iEdge)/(rho_zz(k,cell1)+rho_zz(k,cell2))

            ! MW on ITM: compute decoupling term 
            if (rk_step == 3 .and. config_tend) then
               u_tend_dcpl(k,iEdge) = ( 0.5*(rho_zz_1(k,cell1)+rho_zz_1(k,cell2)) - &
                              0.5*(rho_zz(k,cell1)+rho_zz(k,cell2)))/(0.5*(rho_zz_1(k,cell1)+rho_zz_1(k,cell2)))*u(k,iEdge)
            end if

         end do
      end do

!$OMP BARRIER

      do iCell=cellStart,cellEnd

         !  finish recovering w from (rho*omega)_p.  as when we formed (rho*omega)_p from u and w, we need
         !  to use the same flux-divergence operator as is used for the horizontal theta transport
         !  (See Klemp et al 2003).

         if (bdyMaskCell(iCell) <= nRelaxZone) then  ! addition for regional_MPAS, no spec zone update

         do i=1,nEdgesOnCell(iCell)
            iEdge=edgesOnCell(i,iCell)

            flux = (cf1*ru(1,iEdge) + cf2*ru(2,iEdge) + cf3*ru(3,iEdge))
            w(1,iCell) = w(1,iCell) + edgesOnCell_sign(i,iCell) * &
                                   (zb_cell(1,i,iCell) + sign(1.0_RKIND,flux)*zb3_cell(1,i,iCell))*flux

!DIR$ IVDEP
            do k = 2, nVertLevels
               flux = (fzm(k)*ru(k,iEdge)+fzp(k)*ru(k-1,iEdge))
               w(k,iCell) = w(k,iCell) + edgesOnCell_sign(i,iCell) * &
                                    (zb_cell(k,i,iCell)+sign(1.0_RKIND,flux)*zb3_cell(k,i,iCell))*flux
            end do

         end do

         w(1,iCell) = w(1,iCell)/(cf1*rho_zz(1,iCell)+cf2*rho_zz(2,iCell)+cf3*rho_zz(3,iCell))

         !MW: no change in density at the surface; vertical flux <=> horizontal fluxes  
         if (config_pv_tend)  w_tend_dcpl(1,iCell) = 0.0_RKIND      

         !DIR$ IVDEP
         do k = 2, nVertLevels
           w(k,iCell) = w(k,iCell)/(fzm(k)*rho_zz(k,iCell)+fzp(k)*rho_zz(k-1,iCell))
         end do


         if (config_pv_tend) then     
            !MW: assign this back to rw_tend_dyn_small for output
            !DIR$ IVDEP
            do k = 1, nVertLevels
               rw_tend_dyn_small(k,iCell) = w(k,iCell) - w1(k,iCell)  ! this is the full tendency
            end do
         end if


         end if ! addition for regional_MPAS, no spec zone update

      end do

   end subroutine atm_recover_large_step_variables_work


   !-----------------------------------------------------------------------
   !  routine atm_advance_scalars
   !
   !> \brief Integrate scalar equations - explicit transport plus other tendencies
   !> \date 18 November 2014
   !> \details
   !>  This routine is a wrapper for atm_advance_scalars_work and is primarily
   !>  intended to allow pointers to fields to be dereferenced through the call
   !>  to the work routine.
   !
   !-----------------------------------------------------------------------
   subroutine atm_advance_scalars(field_name, tend, state, diag, mesh, configs, dt, &
                                  edgeStart, edgeEnd, &
                                  cellSolveStart, cellSolveEnd, &
                                  horiz_flux_arr, rk_step, config_time_integration_order, advance_density)

      implicit none

      ! Arguments
      character(len=*), intent(in) :: field_name
      type (mpas_pool_type), intent(in) :: tend
      type (mpas_pool_type), intent(inout) :: state
      type (mpas_pool_type), intent(in) :: diag
      type (mpas_pool_type), intent(in) :: mesh
      type (mpas_pool_type), intent(in) :: configs
      integer, intent(in) :: rk_step    !  rk substep we are integrating
      integer, intent(in) :: config_time_integration_order  ! time integration order
      real (kind=RKIND) :: dt
      integer, intent(in) :: edgeStart, edgeEnd
      integer, intent(in) :: cellSolveStart, cellSolveEnd
      logical, intent(in), optional :: advance_density


      ! Local variables
      real (kind=RKIND), dimension(:), pointer :: invAreaCell

      real (kind=RKIND), dimension(:,:,:), pointer :: scalar_old, scalar_new, scalar_tend_save
      real (kind=RKIND), dimension(:,:), pointer :: uhAvg, rho_zz_old, rho_zz_new, wwAvg
      real (kind=RKIND), dimension(:), pointer :: dvEdge
      integer, dimension(:,:), pointer :: cellsOnEdge
      real (kind=RKIND), dimension(:,:,:), intent(inout) :: horiz_flux_arr

      integer, dimension(:,:), pointer :: advCellsForEdge, edgesOnCell
      integer, dimension(:), pointer :: nAdvCellsForEdge, nEdgesOnCell
      real (kind=RKIND), dimension(:,:), pointer :: adv_coefs, adv_coefs_3rd, edgesOnCell_sign

      integer, pointer :: nCells
      integer, pointer :: nEdges
      integer, pointer :: num_scalars

      real (kind=RKIND), dimension(:), pointer :: fnm, fnp, rdnw
      real (kind=RKIND), pointer :: coef_3rd_order

      integer, dimension(:), pointer :: bdyMaskCell, bdyMaskEdge    ! regional_MPAS addition

      logical :: local_advance_density


      if (present(advance_density)) then
         local_advance_density = advance_density
      else
         local_advance_density = .true.
      end if

      call mpas_pool_get_config(configs, 'config_coef_3rd_order', coef_3rd_order)

      call mpas_pool_get_array(state, trim(field_name), scalar_old, 1)
      call mpas_pool_get_array(state, trim(field_name), scalar_new, 2)
      call mpas_pool_get_array(state, 'rho_zz', rho_zz_old, 1)
      call mpas_pool_get_array(state, 'rho_zz', rho_zz_new, 2)

      call mpas_pool_get_array(diag, 'ruAvg', uhAvg)
      call mpas_pool_get_array(diag, 'wwAvg', wwAvg)

      call mpas_pool_get_array(mesh, 'dvEdge', dvEdge)
      call mpas_pool_get_array(mesh, 'cellsOnEdge', cellsOnEdge)
      call mpas_pool_get_array(mesh, 'edgesOnCell', edgesOnCell)
      call mpas_pool_get_array(mesh, 'nEdgesOnCell', nEdgesOnCell)
      call mpas_pool_get_array(mesh, 'edgesOnCell_sign', edgesOnCell_sign)
      call mpas_pool_get_array(mesh, 'invAreaCell', invAreaCell)
      call mpas_pool_get_array(tend, trim(field_name)//'_tend', scalar_tend_save)
      
      call mpas_pool_get_array(mesh, 'fzm', fnm)
      call mpas_pool_get_array(mesh, 'fzp', fnp)
      call mpas_pool_get_array(mesh, 'rdzw', rdnw)

      call mpas_pool_get_array(mesh, 'nAdvCellsForEdge', nAdvCellsForEdge)
      call mpas_pool_get_array(mesh, 'advCellsForEdge', advCellsForEdge)
      call mpas_pool_get_array(mesh, 'adv_coefs', adv_coefs)
      call mpas_pool_get_array(mesh, 'adv_coefs_3rd', adv_coefs_3rd)

      call mpas_pool_get_dimension(mesh, 'nCells', nCells)
      call mpas_pool_get_dimension(mesh, 'nEdges', nEdges)
      call mpas_pool_get_dimension(state, 'num_'//trim(field_name), num_scalars)

      call mpas_pool_get_array(mesh, 'bdyMaskCell', bdyMaskCell)
      call mpas_pool_get_array(mesh, 'bdyMaskEdge', bdyMaskEdge)

      call atm_advance_scalars_work(nCells, num_scalars, dt, &
             edgeStart, edgeEnd, &
             cellSolveStart, cellSolveEnd, &
             coef_3rd_order, scalar_old, scalar_new, rho_zz_old, rho_zz_new, &
             uhAvg, wwAvg, dvEdge, &
             cellsOnEdge, edgesOnCell, nEdgesOnCell, edgesOnCell_sign, invAreaCell, &
             scalar_tend_save, fnm, fnp, rdnw, &
             bdyMaskCell, bdyMaskEdge, &
             nAdvCellsForEdge, advCellsForEdge, adv_coefs, adv_coefs_3rd, &
             nEdges, horiz_flux_arr, rk_step, config_time_integration_order, &
             local_advance_density)

   end subroutine atm_advance_scalars


   !-----------------------------------------------------------------------
   !  routine atm_advance_scalars_work
   !
   !> \brief Integrate scalar equations - explicit transport plus other tendencies
   !> \date 18 November 2014
   !> \details
   !>  This transport routine is similar to the original atm_advance_scalars, except
   !>  it also advances (re-integrates) the density.  This re-integration allows the scalar
   !>  transport routine to use a different timestep than the dry dynamics, and also makes
   !>  possible a spatial splitting of the scalar transport integration (and density
   !>  integration).  The current integration is, however, not spatially split.
   !>
   !>  WCS 18 November 2014
   !>
   !>  Input: s - current model state, 
   !>             including tendencies from sources other than resolved transport.
   !>         grid - grid metadata
   !>
   !>  input scalars in state are uncoupled (i.e. not mulitplied by density)
   !>
   !>  Output: updated uncoupled scalars (scalars in state).
   !>  Note: scalar tendencies are also modified by this routine.
   !>
   !>  This routine DOES NOT apply any positive definite or monotonic renormalizations.
   !>
   !>  The transport scheme is from Skamarock and Gassmann MWR 2011.
   !
   !-----------------------------------------------------------------------
   subroutine atm_advance_scalars_work(nCells, num_scalars, dt, &
             edgeStart, edgeEnd, &
             cellSolveStart, cellSolveEnd, &
             coef_3rd_order, scalar_old, scalar_new, rho_zz_old, rho_zz_new, &
             uhAvg, wwAvg, dvEdge, &
             cellsOnEdge, edgesOnCell, nEdgesOnCell, edgesOnCell_sign, invAreaCell, &
             scalar_tend_save, fnm, fnp, rdnw, &
             bdyMaskCell, bdyMaskEdge, &
             nAdvCellsForEdge, advCellsForEdge, adv_coefs, adv_coefs_3rd, &
             nEdges, horiz_flux_arr, rk_step, config_time_integration_order, &
             advance_density)

      use mpas_atm_dimensions, only : nVertLevels

      implicit none

      integer, intent(in) :: nCells           ! for allocating stack variables
      integer, intent(in) :: nEdges            ! for allocating stack variables
      integer, intent(in) :: num_scalars
      real (kind=RKIND), intent(in) :: dt
      integer, intent(in) :: edgeStart, edgeEnd
      integer, intent(in) :: cellSolveStart, cellSolveEnd
      integer, intent(in) :: rk_step, config_time_integration_order
      logical, intent(in) :: advance_density
      real (kind=RKIND), dimension(:,:,:), intent(in) :: scalar_old
      real (kind=RKIND), dimension(:,:,:), intent(inout) :: scalar_new
      real (kind=RKIND), dimension(num_scalars,nVertLevels,nCells+1), intent(inout) :: scalar_tend_save
      real (kind=RKIND), dimension(:,:), intent(in) :: rho_zz_old
      real (kind=RKIND), dimension(:,:), intent(in) :: uhAvg, wwAvg, rho_zz_new
      real (kind=RKIND), dimension(:), intent(in) :: dvEdge
      integer, dimension(:,:), intent(in) :: cellsOnEdge
      integer, dimension(:,:), intent(in) :: advCellsForEdge, edgesOnCell
      integer, dimension(:), intent(in) :: nAdvCellsForEdge, nEdgesOnCell
      real (kind=RKIND), dimension(:,:), intent(in) :: adv_coefs, adv_coefs_3rd, edgesOnCell_sign
      real (kind=RKIND), dimension(:), intent(in) :: fnm, fnp, rdnw
      real (kind=RKIND), intent(in) :: coef_3rd_order
      real (kind=RKIND), dimension(num_scalars,nVertLevels,nEdges+1), intent(inout) :: horiz_flux_arr
      real (kind=RKIND), dimension(:), intent(in) :: invAreaCell
      integer, dimension(:), intent(in) :: bdyMaskCell, bdyMaskEdge ! regional_MPAS addition

      integer :: i, j, iCell, iAdvCell, iEdge, k, iScalar, cell1, cell2
      real (kind=RKIND) :: rho_zz_new_inv

      real (kind=RKIND) :: scalar_weight

      real (kind=RKIND), dimension( num_scalars, nVertLevels + 1 ) :: wdtn

      real (kind=RKIND), dimension(nVertLevels,10) :: scalar_weight2
      integer, dimension(10) :: ica

      real (kind=RKIND) :: flux3, flux4
      real (kind=RKIND) :: q_im2, q_im1, q_i, q_ip1, ua, coef3

      logical :: local_advance_density

      real (kind=RKIND) :: weight_time_old, weight_time_new
      real (kind=RKIND), dimension(num_scalars,nVertLevels) :: scalar_tend_column  ! local storage to accumulate tendency
      real (kind=RKIND) :: u_direction, u_positive, u_negative

      flux4(q_im2, q_im1, q_i, q_ip1, ua) =                     &
          ua*( 7.*(q_i + q_im1) - (q_ip1 + q_im2) )/12.0

      flux3(q_im2, q_im1, q_i, q_ip1, ua, coef3) =              &
                flux4(q_im2, q_im1, q_i, q_ip1, ua) +           &
                coef3*abs(ua)*((q_ip1 - q_im2)-3.*(q_i-q_im1))/12.0

      local_advance_density = advance_density

      !
      ! Runge Kutta integration, so we compute fluxes from scalar_new values, update starts from scalar_old
      !


      !  horiz_flux_arr stores the value of the scalar at the edge.
      !  a better name perhaps would be scalarEdge

      !  weights for the time interpolation of the input density
      !
      if (.not. advance_density ) then
         weight_time_new = 1.
      else
         if((rk_step == 1) .and. config_time_integration_order == 3) weight_time_new = 1./3
         if((rk_step == 1) .and. config_time_integration_order == 2) weight_time_new = 1./2
         if(rk_step == 2) weight_time_new = 1./2
         if(rk_step == 3) weight_time_new = 1.
      end if
      weight_time_old = 1. - weight_time_new


      MPAS_ACC_TIMER_START('atm_advance_scalars [ACC_data_xfer]')
      !$acc enter data create(horiz_flux_arr)
      !$acc enter data copyin(uhAvg, scalar_new)
      MPAS_ACC_TIMER_STOP('atm_advance_scalars [ACC_data_xfer]')

      !$acc parallel async
      !$acc loop gang worker private(scalar_weight2, ica)
      do iEdge=edgeStart,edgeEnd

         if ((.not.config_apply_lbcs) &
             .or. (bdyMaskEdge(iEdge) < nRelaxZone-1)) then  ! full flux calculation

            select case(nAdvCellsForEdge(iEdge))

            case(10)

               !$acc loop vector collapse(2)
               do j=1,10
                  do k=1,nVertLevels
                     scalar_weight2(k,j) = adv_coefs(j,iEdge) + sign(1.0_RKIND,uhAvg(k,iEdge))*adv_coefs_3rd(j,iEdge)
                  end do
               end do

               !$acc loop vector
               do j=1,10
                  ica(j) = advCellsForEdge(j,iEdge)
               end do

               !$acc loop vector collapse(2)
               do k = 1,nVertLevels
                  do iScalar = 1,num_scalars
                     horiz_flux_arr(iScalar,k,iEdge) = &
                          scalar_weight2(k,1)  * scalar_new(iScalar,k,ica(1)) + &
                          scalar_weight2(k,2)  * scalar_new(iScalar,k,ica(2)) + &
                          scalar_weight2(k,3)  * scalar_new(iScalar,k,ica(3)) + &
                          scalar_weight2(k,4)  * scalar_new(iScalar,k,ica(4)) + &
                          scalar_weight2(k,5)  * scalar_new(iScalar,k,ica(5)) + &
                          scalar_weight2(k,6)  * scalar_new(iScalar,k,ica(6)) + &
                          scalar_weight2(k,7)  * scalar_new(iScalar,k,ica(7)) + &
                          scalar_weight2(k,8)  * scalar_new(iScalar,k,ica(8)) + &
                          scalar_weight2(k,9)  * scalar_new(iScalar,k,ica(9)) + &
                          scalar_weight2(k,10) * scalar_new(iScalar,k,ica(10))
                  end do
               end do

            case default

               !$acc loop vector collapse(2)
               do k=1,nVertLevels
                  do iScalar=1,num_scalars
                     horiz_flux_arr(iScalar,k,iEdge) = 0.0_RKIND
                  end do
               end do

               !$acc loop seq
               do j=1,nAdvCellsForEdge(iEdge)
                  iAdvCell = advCellsForEdge(j,iEdge)

                  !$acc loop vector collapse(2)
                  do k=1,nVertLevels
                     do iScalar=1,num_scalars
                        scalar_weight = adv_coefs(j,iEdge) + sign(1.0_RKIND,uhAvg(k,iEdge))*adv_coefs_3rd(j,iEdge)
                        horiz_flux_arr(iScalar,k,iEdge) = horiz_flux_arr(iScalar,k,iEdge) &
                                                        + scalar_weight * scalar_new(iScalar,k,iAdvCell)
                     end do
                  end do
               end do
            end select

         else if(config_apply_lbcs &
                 .and. (bdyMaskEdge(iEdge) >= nRelaxZone-1) &
                 .and. (bdyMaskEdge(iEdge) <= nRelaxZone)) then

            !  upwind flux evaluation for outermost 2 edges in specified zone
            cell1 = cellsOnEdge(1,iEdge)
            cell2 = cellsOnEdge(2,iEdge)

            !$acc loop vector collapse(2)
            do k=1,nVertLevels
               do iScalar=1,num_scalars
                  u_direction = sign(0.5_RKIND,uhAvg(k,iEdge))
                  u_positive = dvEdge(iEdge)*abs(u_direction + 0.5_RKIND)
                  u_negative = dvEdge(iEdge)*abs(u_direction - 0.5_RKIND)
                  horiz_flux_arr(iScalar,k,iEdge) = u_positive*scalar_new(iScalar,k,cell1) + u_negative*scalar_new(iScalar,k,cell2)
               end do
            end do

          end if ! end of regional MPAS test
      end do
      !$acc end parallel

!$OMP BARRIER

      !
      ! scalar update, for each column sum fluxes over horizontal edges, add physics tendency,
      ! and add vertical flux divergence in update.
      !

      MPAS_ACC_TIMER_START('atm_advance_scalars [ACC_data_xfer]')
#ifndef DO_PHYSICS
      !$acc enter data create(scalar_tend_save)
#else
      !$acc enter data copyin(scalar_tend_save)
#endif
      !$acc enter data copyin(scalar_old, fnm, fnp, rdnw, wwAvg, rho_zz_old, rho_zz_new)
      !$acc enter data create(scalar_tend_column, wdtn)
      MPAS_ACC_TIMER_STOP('atm_advance_scalars [ACC_data_xfer]')

      !$acc parallel wait
      !$acc loop gang worker private(scalar_tend_column, wdtn)
      do iCell=cellSolveStart,cellSolveEnd

         if(bdyMaskCell(iCell) <= nRelaxZone) then  ! specified zone for regional_MPAS is not updated in this routine

            !$acc loop vector collapse(2)
            do k=1,nVertLevels
            do iScalar=1,num_scalars
               scalar_tend_column(iScalar,k) = 0.0_RKIND
#ifndef DO_PHYSICS
               scalar_tend_save(iScalar,k,iCell) = 0.0_RKIND  !  testing purposes - we have no sources or sinks
#endif
            end do
            end do

            !$acc loop seq
            do i=1,nEdgesOnCell(iCell)
               iEdge = edgesOnCell(i,iCell)

               ! here we add the horizontal flux divergence into the scalar tendency.
               ! note that the scalar tendency is modified.
               !$acc loop vector collapse(2)
               do k=1,nVertLevels
                  do iScalar=1,num_scalars
                        scalar_tend_column(iScalar,k) = scalar_tend_column(iScalar,k) &
                               - edgesOnCell_sign(i,iCell) * uhAvg(k,iEdge)*horiz_flux_arr(iScalar,k,iEdge)
                  end do
               end do

            end do

            !$acc loop vector collapse(2)
            do k=1,nVertLevels
               do iScalar=1,num_scalars
                     scalar_tend_column(iScalar,k) = scalar_tend_column(iScalar,k) * invAreaCell(iCell) &
                                                   + scalar_tend_save(iScalar,k,iCell)
               end do
            end do


            !
            !  vertical flux divergence and update of the scalars
            !

            !$acc loop vector
            do iScalar=1,num_scalars
               wdtn(iScalar,1) = 0.0
               wdtn(iScalar,2) = wwAvg(2,iCell)*(fnm(2)*scalar_new(iScalar,2,iCell)+fnp(2)*scalar_new(iScalar,2-1,iCell))
               wdtn(iScalar,nVertLevels) = wwAvg(nVertLevels,iCell) * &
                                           ( fnm(nVertLevels)*scalar_new(iScalar,nVertLevels,iCell) &
                                            +fnp(nVertLevels)*scalar_new(iScalar,nVertLevels-1,iCell) )
               wdtn(iScalar,nVertLevels+1) = 0.0
            end do

            !$acc loop vector collapse(2)
            do k=3,nVertLevels-1
               do iScalar=1,num_scalars
                  wdtn(iScalar,k) = flux3( scalar_new(iScalar,k-2,iCell),scalar_new(iScalar,k-1,iCell),  &
                                           scalar_new(iScalar,k  ,iCell),scalar_new(iScalar,k+1,iCell),  &
                                           wwAvg(k,iCell), coef_3rd_order )
               end do
            end do

            !$acc loop vector collapse(2)
            do k=1,nVertLevels
               do iScalar=1,num_scalars
                  rho_zz_new_inv = 1.0_RKIND / (weight_time_old*rho_zz_old(k,iCell) + weight_time_new*rho_zz_new(k,iCell))
                  scalar_new(iScalar,k,iCell) = (   scalar_old(iScalar,k,iCell)*rho_zz_old(k,iCell) &
                        + dt*( scalar_tend_column(iScalar,k) -rdnw(k)*(wdtn(iScalar,k+1)-wdtn(iScalar,k)) ) ) * rho_zz_new_inv
               end do
            end do

         end if ! specified zone regional_MPAS test

      end do
      !$acc end parallel

      MPAS_ACC_TIMER_START('atm_advance_scalars [ACC_data_xfer]')
      !$acc exit data copyout(scalar_new)
      !$acc exit data delete(scalar_tend_column, wdtn, uhAvg, wwAvg, scalar_old, fnm, fnp, &
      !$acc                  rdnw, rho_zz_old, rho_zz_new, horiz_flux_arr, scalar_tend_save)
      MPAS_ACC_TIMER_STOP('atm_advance_scalars [ACC_data_xfer]')

   end subroutine atm_advance_scalars_work


   !-----------------------------------------------------------------------
   !  routine atm_advance_scalars_mono
   !
   !> \brief Integrate scalar equations - transport plus other tendencies
   !> \date 18 November 2014
   !> \details
   !>  This routine is a wrapper for atm_advance_scalars_mono_work and is primarily
   !>  intended to allow pointers to fields to be dereferenced through the call
   !>  to the work routine.
   !
   !-----------------------------------------------------------------------
   subroutine atm_advance_scalars_mono(field_name, block, tend, state, diag, mesh, halo_scratch, configs, dt, &
                                       cellStart, cellEnd, edgeStart, edgeEnd, &
                                       cellSolveStart, cellSolveEnd, &
                                       scalar_old, scalar_new, s_max, s_min, wdtn, flux_arr, &
                                       flux_upwind_tmp, flux_tmp, exchange_halo_group, advance_density, rho_zz_int)

      implicit none

      ! Arguments
      character(len=*), intent(in) :: field_name
      type (block_type), intent(inout), target :: block
      type (mpas_pool_type), intent(in)    :: tend
      type (mpas_pool_type), intent(inout) :: state
      type (mpas_pool_type), intent(in)    :: diag
      type (mpas_pool_type), intent(in)    :: mesh
      type (mpas_pool_type), intent(in)    :: halo_scratch
      type (mpas_pool_type), intent(in)    :: configs
      real (kind=RKIND), intent(in)        :: dt
      integer, intent(in) :: cellStart, cellEnd, edgeStart, edgeEnd
      integer, intent(in) :: cellSolveStart, cellSolveEnd
      real (kind=RKIND), dimension(:,:), intent(inout) :: scalar_old, scalar_new
      real (kind=RKIND), dimension(:,:), intent(inout) :: s_max, s_min
      real (kind=RKIND), dimension(:,:), intent(inout) :: wdtn
      real (kind=RKIND), dimension(:,:), intent(inout) :: flux_arr
      real (kind=RKIND), dimension(:,:), intent(inout) :: flux_upwind_tmp, flux_tmp
      procedure (halo_exchange_routine) :: exchange_halo_group
      logical, intent(in), optional :: advance_density
      real (kind=RKIND), dimension(:,:), intent(inout), optional :: rho_zz_int

      ! Local variables
      real (kind=RKIND), dimension(:,:,:), pointer :: scalar_tend
      real (kind=RKIND), dimension(:,:), pointer :: uhAvg, rho_zz_old, rho_zz_new, wwAvg
      real (kind=RKIND), dimension(:), pointer :: dvEdge, invAreaCell
      integer, dimension(:,:), pointer :: cellsOnEdge, cellsOnCell, edgesOnCell
      real (kind=RKIND), dimension(:,:), pointer :: edgesOnCell_sign

      integer, dimension(:,:), pointer :: advCellsForEdge
      integer, dimension(:), pointer :: nAdvCellsForEdge
      real (kind=RKIND), dimension(:,:), pointer :: adv_coefs, adv_coefs_3rd
      real (kind=RKIND), dimension(:,:,:), pointer :: scalars_old, scalars_new

      integer, dimension(:), pointer :: bdyMaskCell, bdyMaskEdge  ! regional_MPAS addition

      integer, pointer :: nCells
      integer, pointer :: nEdges
      integer, pointer :: nCellsSolve
      integer, pointer :: num_scalars

      real (kind=RKIND), dimension(:), pointer :: fnm, fnp, rdnw
      integer, dimension(:), pointer :: nEdgesOnCell
      real (kind=RKIND), pointer :: coef_3rd_order

      type (field3DReal), pointer :: scale
      real (kind=RKIND), dimension(:,:,:), pointer :: scale_arr


      call mpas_pool_get_config(configs, 'config_coef_3rd_order', coef_3rd_order)

      call mpas_pool_get_dimension(mesh, 'nCells', nCells)
      call mpas_pool_get_dimension(mesh, 'nEdges', nEdges)
      call mpas_pool_get_dimension(mesh, 'nCellsSolve', nCellsSolve)
      call mpas_pool_get_dimension(state, 'num_'//trim(field_name), num_scalars)

      call mpas_pool_get_array(diag, 'ruAvg', uhAvg)
      call mpas_pool_get_array(diag, 'wwAvg', wwAvg)

      call mpas_pool_get_array(tend, trim(field_name)//'_tend', scalar_tend)

      call mpas_pool_get_array(state, 'rho_zz', rho_zz_old, 1)
      call mpas_pool_get_array(state, 'rho_zz', rho_zz_new, 2)
      call mpas_pool_get_array(state, trim(field_name), scalars_old, 1)
      call mpas_pool_get_array(state, trim(field_name), scalars_new, 2)

      call mpas_pool_get_array(mesh, 'invAreaCell', invAreaCell)
      call mpas_pool_get_array(mesh, 'dvEdge', dvEdge)
      call mpas_pool_get_array(mesh, 'cellsOnEdge', cellsOnEdge)
      call mpas_pool_get_array(mesh, 'cellsOnCell', cellsOnCell)
      call mpas_pool_get_array(mesh, 'edgesOnCell', edgesOnCell)
      call mpas_pool_get_array(mesh, 'edgesOnCell_sign', edgesOnCell_sign)
      call mpas_pool_get_array(mesh, 'nEdgesOnCell', nEdgesOnCell)
      call mpas_pool_get_array(mesh, 'fzm', fnm)
      call mpas_pool_get_array(mesh, 'fzp', fnp)
      call mpas_pool_get_array(mesh, 'rdzw', rdnw)
      call mpas_pool_get_array(mesh, 'nAdvCellsForEdge', nAdvCellsForEdge)
      call mpas_pool_get_array(mesh, 'advCellsForEdge', advCellsForEdge)
      call mpas_pool_get_array(mesh, 'adv_coefs', adv_coefs)
      call mpas_pool_get_array(mesh, 'adv_coefs_3rd', adv_coefs_3rd)

      call mpas_pool_get_array(mesh, 'bdyMaskCell', bdyMaskCell)  ! MPAS_regional addition
      call mpas_pool_get_array(mesh, 'bdyMaskEdge', bdyMaskEdge)  ! MPAS_regional addition

      call mpas_pool_get_field(halo_scratch, 'scale', scale)
      call mpas_allocate_scratch_field(scale)
      call mpas_pool_get_array(halo_scratch, 'scale', scale_arr)

      call atm_advance_scalars_mono_work(field_name, block, state, nCells, nEdges, num_scalars, dt, &
                                   cellStart, cellEnd, edgeStart, edgeEnd, &
                                   cellSolveStart, cellSolveEnd, &
                                   coef_3rd_order, nCellsSolve, uhAvg, wwAvg, scalar_tend, rho_zz_old, &
                                   rho_zz_new, scalars_old, scalars_new, invAreaCell, dvEdge, cellsOnEdge, cellsOnCell, &
                                   edgesOnCell, edgesOnCell_sign, nEdgesOnCell, fnm, fnp, rdnw, nAdvCellsForEdge, &
                                   advCellsForEdge, adv_coefs, adv_coefs_3rd, scalar_old, scalar_new, s_max, s_min, &
                                   wdtn, scale_arr, flux_arr, flux_upwind_tmp, flux_tmp, &
                                   bdyMaskCell, bdyMaskEdge, &
                                   exchange_halo_group, advance_density, rho_zz_int)

      call mpas_deallocate_scratch_field(scale)

   end subroutine atm_advance_scalars_mono


   !-----------------------------------------------------------------------
   !  routine atm_advance_scalars_mono_work
   !
   !> \brief Integrate scalar equations - transport plus other tendencies
   !> \date 18 November 2014
   !> \details
   !>  This transport routine is similar to the original atm_advance_scalars_mono_work,
   !>  except it also advances (re-integrates) the density.  This re-integration allows
   !>  the scalar transport routine to use a different timestep than the dry dynamics,
   !>  and also makes possible a spatial splitting of the scalar transport integration
   !>  (and density integration).  The current integration is, however, not spatially split.
   !>
   !>  WCS 18 November 2014
   !>
   !>
   !>  Input: s - current model state, 
   !>             including tendencies from sources other than resolved transport.
   !>         grid - grid metadata
   !>
   !>  input scalars in state are uncoupled (i.e. not mulitplied by density)
   !>
   !>  Output: updated uncoupled scalars (scalars in s_new).
   !>  Note: scalar tendencies are also modified by this routine.
   !>
   !>  This routine DOES apply positive definite or monotonic renormalizations.
   !>
   !>  The transport scheme is from Skamarock and Gassmann MWR 2011.
   !>
   !>  The positive-definite or monotonic renormalization is from Zalesak JCP 1979
   !>  as used in the RK3 scheme as described in Wang et al MWR 2009
   !
   !-----------------------------------------------------------------------
   subroutine atm_advance_scalars_mono_work(field_name, block, state, nCells, nEdges, num_scalars, dt, &
                                   cellStart, cellEnd, edgeStart, edgeEnd, &
                                   cellSolveStart, cellSolveEnd, &
                                   coef_3rd_order, nCellsSolve, uhAvg, wwAvg, scalar_tend, rho_zz_old, &
                                   rho_zz_new, scalars_old, scalars_new, invAreaCell, dvEdge, cellsOnEdge, cellsOnCell, &
                                   edgesOnCell, edgesOnCell_sign, nEdgesOnCell, fnm, fnp, rdnw, nAdvCellsForEdge, &
                                   advCellsForEdge, adv_coefs, adv_coefs_3rd, scalar_old, scalar_new, s_max, s_min, &
                                   wdtn, scale_arr, flux_arr, flux_upwind_tmp, flux_tmp, &
                                   bdyMaskCell, bdyMaskEdge, &
                                   exchange_halo_group, advance_density, rho_zz_int)

      use mpas_atm_dimensions, only : nVertLevels

      implicit none

      character(len=*), intent(in) :: field_name
      type (block_type), intent(inout), target :: block
      type (mpas_pool_type), intent(inout) :: state
      integer, intent(in)                  :: nCells           ! for allocating stack variables
      integer, intent(in)                  :: nEdges           ! for allocating stack variables
      integer, intent(in)                  :: num_scalars
      real (kind=RKIND), intent(in)        :: dt
      integer, intent(in) :: cellStart, cellEnd, edgeStart, edgeEnd
      integer, intent(in) :: cellSolveStart, cellSolveEnd
      procedure (halo_exchange_routine) :: exchange_halo_group
      logical, intent(in), optional :: advance_density
      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(inout), optional :: rho_zz_int

      integer :: ii,jj
      integer, dimension(10) :: ica
      real (kind=RKIND), dimension(10,2) :: swa

      integer :: i, iCell, iEdge, k, iScalar, cell1, cell2
      real (kind=RKIND) :: flux, scalar_weight

      real (kind=RKIND), dimension(num_scalars,nVertLevels,nCells+1), intent(inout) :: scalar_tend
      real (kind=RKIND), dimension(nVertLevels,nEdges+1), intent(in) :: uhAvg
      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(in) :: rho_zz_old, rho_zz_new
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1), intent(in) :: wwAvg
      real (kind=RKIND), dimension(:), intent(in) :: dvEdge, invAreaCell
      integer, dimension(:,:), intent(in) :: cellsOnEdge, cellsOnCell, edgesOnCell
      integer, dimension(:) :: bdyMaskCell, bdyMaskEdge
      real (kind=RKIND), dimension(:,:), intent(in) :: edgesOnCell_sign

      integer, dimension(:,:), intent(in) :: advCellsForEdge
      integer, dimension(:), intent(in) :: nAdvCellsForEdge
      real (kind=RKIND), dimension(:,:), intent(in) :: adv_coefs, adv_coefs_3rd
      real (kind=RKIND), dimension(num_scalars,nVertLevels,nCells+1), intent(inout) :: scalars_old, scalars_new
      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(inout) :: scalar_old, scalar_new
      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(inout) :: s_max, s_min
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1), intent(inout) :: wdtn
      real (kind=RKIND), dimension(nVertLevels,2,nCells+1), intent(inout) :: scale_arr
      real (kind=RKIND), dimension(nVertLevels,nEdges+1), intent(inout) :: flux_arr
      real (kind=RKIND), dimension(nVertLevels,nEdges+1), intent(inout) :: flux_upwind_tmp, flux_tmp

      integer, parameter :: SCALE_IN = 1, SCALE_OUT = 2

      integer, intent(in) :: nCellsSolve
#ifdef DEBUG_TRANSPORT
      integer :: icellmax, kmax
#endif

      real (kind=RKIND), dimension(nVertLevels), intent(in) :: fnm, fnp, rdnw
      integer, dimension(:), intent(in) :: nEdgesOnCell
      real (kind=RKIND), intent(in) :: coef_3rd_order


      real (kind=RKIND), dimension(nVertLevels) :: flux_upwind_arr
      real (kind=RKIND) :: flux3, flux4, flux_upwind
      real (kind=RKIND) :: q_im2, q_im1, q_i, q_ip1, ua, coef3
#ifdef DEBUG_TRANSPORT
      real (kind=RKIND) :: scmin,scmax
#endif
      real (kind=RKIND) :: scale_factor

      logical :: local_advance_density

      real (kind=RKIND), parameter :: eps=1.e-20

      flux4(q_im2, q_im1, q_i, q_ip1, ua) =                     &
          ua*( 7.*(q_i + q_im1) - (q_ip1 + q_im2) )/12.0

      flux3(q_im2, q_im1, q_i, q_ip1, ua, coef3) =              &
                flux4(q_im2, q_im1, q_i, q_ip1, ua) +           &
                coef3*abs(ua)*((q_ip1 - q_im2)-3.*(q_i-q_im1))/12.0

      if (present(advance_density)) then
         local_advance_density = advance_density
      else
         local_advance_density = .true.
      end if

      !  for positive-definite or monotonic option, we first update scalars using the tendency from sources other than
      !  the resolved transport (these should constitute a positive definite update).  
      !  Note, however, that we enforce positive-definiteness in this update.
      !  The transport will maintain this positive definite solution and optionally, shape preservation (monotonicity).


      MPAS_ACC_TIMER_START('atm_advance_scalars_mono [ACC_data_xfer]')
      !$acc data present(nEdgesOnCell, edgesOnCell, edgesOnCell_sign, &
      !$acc              invAreaCell, cellsOnCell, cellsOnEdge, nAdvCellsForEdge, &
      !$acc              advCellsForEdge, adv_coefs, adv_coefs_3rd, dvEdge, bdyMaskCell)

#ifdef DO_PHYSICS
      !$acc enter data copyin(scalar_tend)
#else
      !$acc enter data create(scalar_tend)
#endif
      if (local_advance_density) then
         !$acc enter data copyin(rho_zz_int)
      end if
      !$acc enter data copyin(scalars_old, rho_zz_old, rdnw, uhAvg, wwAvg)
      MPAS_ACC_TIMER_STOP('atm_advance_scalars_mono [ACC_data_xfer]')

      !$acc parallel

      !$acc loop gang worker
      do iCell=cellSolveStart,cellSolveEnd

         !$acc loop vector collapse(2)
         do k = 1,nVertLevels
            do iScalar = 1,num_scalars

#ifndef DO_PHYSICS
               scalar_tend(iScalar,k,iCell) = 0.0_RKIND  !  testing purposes - we have no sources or sinks
#endif
               scalars_old(iScalar,k,iCell) = scalars_old(iScalar,k,iCell)+dt*scalar_tend(iScalar,k,iCell) / rho_zz_old(k,iCell)
               scalar_tend(iScalar,k,iCell) = 0.0_RKIND
         end do
      end do

      end do

      !$acc end parallel

      MPAS_ACC_TIMER_START('atm_advance_scalars_mono [ACC_data_xfer]')
      !$acc exit data copyout(scalar_tend)

      !$acc update self(scalars_old)
      MPAS_ACC_TIMER_STOP('atm_advance_scalars_mono [ACC_data_xfer]')

!$OMP BARRIER
!$OMP MASTER
      call exchange_halo_group(block % domain, 'dynamics:'//trim(field_name)//'_old')
!$OMP END MASTER
!$OMP BARRIER

      MPAS_ACC_TIMER_START('atm_advance_scalars_mono [ACC_data_xfer]')
      !$acc update device(scalars_old)
      MPAS_ACC_TIMER_STOP('atm_advance_scalars_mono [ACC_data_xfer]')

      !
      ! Runge Kutta integration, so we compute fluxes from scalar_new values, update starts from scalar_old
      !

      if (local_advance_density) then
         if (.not.present(rho_zz_int)) then
            call mpas_log_write('Error: rho_zz_int not supplied to atm_advance_scalars_mono_work( ) when advance_density=.true.', messageType=MPAS_LOG_CRIT)
         end if

         !$acc parallel

         !  begin with update of density

         !$acc loop gang worker
         do iCell=cellSolveStart,cellSolveEnd

            !$acc loop vector
            do k=1,nVertLevels
               rho_zz_int(k,iCell) = 0.0_RKIND
            end do

            !$acc loop seq
            do i=1,nEdgesOnCell(iCell)
               iEdge = edgesOnCell(i,iCell)
   
               !$acc loop vector
               do k=1,nVertLevels
                  rho_zz_int(k,iCell) = rho_zz_int(k,iCell) - edgesOnCell_sign(i,iCell) &
                                                            * uhAvg(k,iEdge) * dvEdge(iEdge) * invAreaCell(iCell)
               end do
               
            end do
         end do

         !$acc loop gang worker
         do iCell=cellSolveStart,cellSolveEnd

            !$acc loop vector
            do k=1,nVertLevels
               rho_zz_int(k,iCell) = rho_zz_old(k,iCell) + dt*(rho_zz_int(k,iCell) - rdnw(k)*(wwAvg(k+1,iCell)-wwAvg(k,iCell)))
            end do
         end do

         !$acc end parallel

!$OMP BARRIER

      end if

      MPAS_ACC_TIMER_START('atm_advance_scalars_mono [ACC_data_xfer]')
      if (.not. local_advance_density) then
         !$acc enter data copyin(rho_zz_new)
      end if
      !$acc enter data copyin(scalars_new, fnm, fnp)
      !$acc enter data create(scalar_old, scalar_new, scale_arr, s_min, s_max, &
      !$acc                   flux_arr, flux_tmp, flux_upwind_tmp, wdtn)
      MPAS_ACC_TIMER_STOP('atm_advance_scalars_mono [ACC_data_xfer]')

      do iScalar = 1, num_scalars

         !$acc parallel

         !$acc loop gang worker
         do iCell=cellStart,cellEnd

            !$acc loop vector
            do k=1,nVertLevels
               scalar_old(k,iCell) = scalars_old(iScalar,k,iCell)
               scalar_new(k,iCell) = scalars_new(iScalar,k,iCell)
            end do
         end do

#ifndef MPAS_OPENACC
         do k=1,nVertLevels
            scalar_old(k,nCells+1) = 0.0_RKIND
            scalar_new(k,nCells+1) = 0.0_RKIND
         end do
#endif

         !$acc end parallel

!$OMP BARRIER

#ifdef DEBUG_TRANSPORT
         !$acc update self(scalar_old)

         scmin = scalar_old(1,1)
         scmax = scalar_old(1,1)
         do iCell = 1, nCells
         do k=1, nVertLevels
            scmin = min(scmin,scalar_old(k,iCell))
            scmax = max(scmax,scalar_old(k,iCell))
         end do
         end do
         call mpas_log_write(' scmin, scmin old in $r $r', realArgs=(/scmin,scmax/))

         !$acc update self(scalar_new)

         scmin = scalar_new(1,1)
         scmax = scalar_new(1,1)
         do iCell = 1, nCells
         do k=1, nVertLevels
            scmin = min(scmin,scalar_new(k,iCell))
            scmax = max(scmax,scalar_new(k,iCell))
         end do
         end do
         call mpas_log_write(' scmin, scmin new in ', realArgs=(/scmin,scmax/))
#endif

         !$acc parallel

         !
         !  vertical flux divergence, and min and max bounds for flux limiter
         !
         !$acc loop gang worker
         do iCell=cellSolveStart,cellSolveEnd

            ! zero flux at top and bottom
            wdtn(1,iCell) = 0.0_RKIND
            wdtn(nVertLevels+1,iCell) = 0.0_RKIND

            k = 1
            s_max(k,iCell) = max(scalar_old(1,iCell),scalar_old(2,iCell))
            s_min(k,iCell) = min(scalar_old(1,iCell),scalar_old(2,iCell))

            k = 2
            wdtn(k,iCell) = wwAvg(k,iCell)*(fnm(k)*scalar_new(k,iCell)+fnp(k)*scalar_new(k-1,iCell))
            s_max(k,iCell) = max(scalar_old(k-1,iCell),scalar_old(k,iCell),scalar_old(k+1,iCell))
            s_min(k,iCell) = min(scalar_old(k-1,iCell),scalar_old(k,iCell),scalar_old(k+1,iCell))
             
            !$acc loop vector
            do k=3,nVertLevels-1
               wdtn(k,iCell) = flux3( scalar_new(k-2,iCell),scalar_new(k-1,iCell),  &
                                      scalar_new(k  ,iCell),scalar_new(k+1,iCell),  &
                                      wwAvg(k,iCell), coef_3rd_order )
               s_max(k,iCell) = max(scalar_old(k-1,iCell),scalar_old(k,iCell),scalar_old(k+1,iCell))
               s_min(k,iCell) = min(scalar_old(k-1,iCell),scalar_old(k,iCell),scalar_old(k+1,iCell))
            end do
 
            k = nVertLevels
            wdtn(k,iCell) = wwAvg(k,iCell)*(fnm(k)*scalar_new(k,iCell)+fnp(k)*scalar_new(k-1,iCell))
            s_max(k,iCell) = max(scalar_old(k,iCell),scalar_old(k-1,iCell))
            s_min(k,iCell) = min(scalar_old(k,iCell),scalar_old(k-1,iCell))

            !
            ! pull s_min and s_max from the (horizontal) surrounding cells
            !

            ! speclal treatment of calculations involving hexagonal cells
            ! original code retained in select "default" case
            select case(nEdgesOnCell(iCell))
            case(6)
               !$acc loop vector
               do k=1, nVertLevels
                  s_max(k,iCell) = max(s_max(k,iCell), &
                       scalar_old(k, cellsOnCell(1,iCell)), &
                       scalar_old(k, cellsOnCell(2,iCell)), &
                       scalar_old(k, cellsOnCell(3,iCell)), &
                       scalar_old(k, cellsOnCell(4,iCell)), &
                       scalar_old(k, cellsOnCell(5,iCell)), &
                       scalar_old(k, cellsOnCell(6,iCell)))
                  s_min(k,iCell) = min(s_min(k,iCell), &
                       scalar_old(k, cellsOnCell(1,iCell)), &
                       scalar_old(k, cellsOnCell(2,iCell)), &
                       scalar_old(k, cellsOnCell(3,iCell)), &
                       scalar_old(k, cellsOnCell(4,iCell)), &
                       scalar_old(k, cellsOnCell(5,iCell)), &
                       scalar_old(k, cellsOnCell(6,iCell)))
               end do

            case default
               !$acc loop seq
               do i=1, nEdgesOnCell(iCell)

                  !$acc loop vector
                  do k=1, nVertLevels
                     s_max(k,iCell) = max(s_max(k,iCell),scalar_old(k, cellsOnCell(i,iCell)))
                     s_min(k,iCell) = min(s_min(k,iCell),scalar_old(k, cellsOnCell(i,iCell)))
                  end do
               end do
            end select

         end do

         !$acc end parallel

!$OMP BARRIER

         !$acc parallel

         !
         !  horizontal flux divergence
         !
         !$acc loop gang worker private(ica, swa)
         do iEdge=edgeStart,edgeEnd

            cell1 = cellsOnEdge(1,iEdge)
            cell2 = cellsOnEdge(2,iEdge)

            if (cell1 <= nCellsSolve .or. cell2 <= nCellsSolve) then  ! only for owned cells
  
               ! special treatment of calculations involving edges between hexagonal cells
               ! original code retained in select "default" case
               ! be sure to see additional declarations near top of subroutine
               select case(nAdvCellsForEdge(iEdge))
               case(10)
                  !$acc loop vector
                  do jj=1,10
                     ica(jj)    = advCellsForEdge(jj,iEdge)
                     swa(jj,1)  = adv_coefs(jj,iEdge) + adv_coefs_3rd(jj,iEdge)
                     swa(jj,2)  = adv_coefs(jj,iEdge) - adv_coefs_3rd(jj,iEdge)
                  end do

                  !$acc loop vector
                  do k=1,nVertLevels
                     ii = merge(1, 2, uhAvg(k,iEdge) > 0)
                     flux_arr(k,iEdge) = uhAvg(k,iEdge)*( &
                          swa(1,ii)*scalar_new(k,ica(1)) + swa(2,ii)*scalar_new(k,ica(2)) + &
                          swa(3,ii)*scalar_new(k,ica(3)) + swa(4,ii)*scalar_new(k,ica(4)) + &
                          swa(5,ii)*scalar_new(k,ica(5)) + swa(6,ii)*scalar_new(k,ica(6)) + &
                          swa(7,ii)*scalar_new(k,ica(7)) + swa(8,ii)*scalar_new(k,ica(8)) + &
                          swa(9,ii)*scalar_new(k,ica(9)) + swa(10,ii)*scalar_new(k,ica(10)))
                  end do

               case default
                  !$acc loop vector
                  do k=1,nVertLevels
                     flux_arr(k,iEdge) = 0.0_RKIND
                  end do

                  !$acc loop seq
                  do i=1,nAdvCellsForEdge(iEdge)
                     iCell = advCellsForEdge(i,iEdge)

                     !$acc loop vector
                     do k=1,nVertLevels
                        scalar_weight = uhAvg(k,iEdge)*(adv_coefs(i,iEdge) + sign(1.0_RKIND,uhAvg(k,iEdge))*adv_coefs_3rd(i,iEdge))
                        flux_arr(k,iEdge) = flux_arr(k,iEdge) + scalar_weight* scalar_new(k,iCell)
                     end do
                  end do
               end select

            else

               !$acc loop vector
               do k=1,nVertLevels
                  flux_arr(k,iEdge) = 0.0_RKIND
               end do

            end if

         end do

         !$acc end parallel

!$OMP BARRIER

         !$acc parallel

         !
         !  vertical flux divergence for upwind update, we will put upwind update into scalar_new, and put factor of dt in fluxes
         !

         !$acc loop gang worker private(flux_upwind_arr)
         do iCell=cellSolveStart,cellSolveEnd

            k = 1
            scalar_new(k,iCell) = scalar_old(k,iCell) * rho_zz_old(k,iCell)

            !$acc loop vector
            do k = 2, nVertLevels
               scalar_new(k,iCell) = scalar_old(k,iCell)*rho_zz_old(k,iCell)
               flux_upwind_arr(k) = dt*(max(0.0_RKIND,wwAvg(k,iCell))*scalar_old(k-1,iCell) + min(0.0_RKIND,wwAvg(k,iCell))*scalar_old(k,iCell))
            end do

            !$acc loop vector
            do k = 1, nVertLevels-1
               scalar_new(k,iCell) = scalar_new(k,iCell) - flux_upwind_arr(k+1)*rdnw(k)
            end do

            !$acc loop vector
            do k = 2, nVertLevels
               scalar_new(k  ,iCell) = scalar_new(k  ,iCell) + flux_upwind_arr(k)*rdnw(k)
               wdtn(k,iCell) = dt*wdtn(k,iCell) - flux_upwind_arr(k)
            end do

            !
            ! scale_arr(SCALE_IN,:,:) and scale_arr(SCALE_OUT:,:) are used here to store the incoming and outgoing perturbation flux 
            ! contributions to the update:  first the vertical flux component, then the horizontal
            !

            !$acc loop vector
            do k=1,nVertLevels
               scale_arr(k,SCALE_IN, iCell) = - rdnw(k)*(min(0.0_RKIND,wdtn(k+1,iCell))-max(0.0_RKIND,wdtn(k,iCell)))
               scale_arr(k,SCALE_OUT,iCell) = - rdnw(k)*(max(0.0_RKIND,wdtn(k+1,iCell))-min(0.0_RKIND,wdtn(k,iCell)))
            end do

         end do

         !
         !  horizontal flux divergence for upwind update
         !

         !  upwind flux computation
         !$acc loop gang worker
         do iEdge=edgeStart,edgeEnd

            cell1 = cellsOnEdge(1,iEdge)
            cell2 = cellsOnEdge(2,iEdge)

            !$acc loop vector
            do k=1,nVertLevels
               flux_upwind_tmp(k,iEdge) = dvEdge(iEdge) * dt *   &
                      (max(0.0_RKIND,uhAvg(k,iEdge))*scalar_old(k,cell1) + min(0.0_RKIND,uhAvg(k,iEdge))*scalar_old(k,cell2))
               flux_tmp(k,iEdge) = dt * flux_arr(k,iEdge) - flux_upwind_tmp(k,iEdge)
            end do

            if( config_apply_lbcs .and. (bdyMaskEdge(iEdge) == nRelaxZone) .or. (bdyMaskEdge(iEdge) == nRelaxZone-1) ) then
               !$acc loop vector
               do k=1,nVertLevels
                  flux_tmp(k,iEdge) = 0.0_RKIND
                  flux_arr(k,iEdge) = flux_upwind_tmp(k,iEdge)
               end do
            end if

         end do

         !$acc end parallel

!$OMP BARRIER

         !$acc parallel

         !$acc loop gang worker
         do iCell=cellSolveStart,cellSolveEnd

            !$acc loop seq
            do i=1,nEdgesOnCell(iCell)
               iEdge = edgesOnCell(i,iCell)

               !$acc loop vector
               do k=1, nVertLevels
                  scalar_new(k,iCell) = scalar_new(k,iCell) - edgesOnCell_sign(i,iCell) * flux_upwind_tmp(k,iEdge) * invAreaCell(iCell)
 
                  scale_arr(k,SCALE_OUT,iCell) = scale_arr(k,SCALE_OUT,iCell) &
                                                 - max(0.0_RKIND,edgesOnCell_sign(i,iCell)*flux_tmp(k,iEdge)) * invAreaCell(iCell)
                  scale_arr(k,SCALE_IN, iCell) = scale_arr(k,SCALE_IN, iCell) &
                                                 - min(0.0_RKIND,edgesOnCell_sign(i,iCell)*flux_tmp(k,iEdge)) * invAreaCell(iCell)
               end do

            end do
         end do


         !
         !  next, the limiter
         !

         ! simplification of limiter calculations
         ! worked through algebra and found equivalent form
         ! added benefit that it should address ifort single prec overflow issue
         if (local_advance_density) then
            !$acc loop gang worker
            do iCell=cellSolveStart,cellSolveEnd

               !$acc loop vector
               do k = 1, nVertLevels
                  scale_factor = (s_max(k,iCell)*rho_zz_int(k,iCell) - scalar_new(k,iCell)) / &
                       (scale_arr(k,SCALE_IN,iCell)  + eps)
                  scale_arr(k,SCALE_IN,iCell) = min( 1.0_RKIND, max( 0.0_RKIND, scale_factor) )

                  scale_factor = (s_min(k,iCell)*rho_zz_int(k,iCell) - scalar_new(k,iCell)) / &
                       (scale_arr(k,SCALE_OUT,iCell) - eps)
                  scale_arr(k,SCALE_OUT,iCell) = min( 1.0_RKIND, max( 0.0_RKIND, scale_factor) )
               end do
            end do
         else
            !$acc loop gang worker
            do iCell=cellSolveStart,cellSolveEnd

               !$acc loop vector
               do k = 1, nVertLevels
                  scale_factor = (s_max(k,iCell)*rho_zz_new(k,iCell) - scalar_new(k,iCell)) / &
                       (scale_arr(k,SCALE_IN,iCell)  + eps)
                  scale_arr(k,SCALE_IN,iCell) = min( 1.0_RKIND, max( 0.0_RKIND, scale_factor) )

                  scale_factor = (s_min(k,iCell)*rho_zz_new(k,iCell) - scalar_new(k,iCell)) / &
                       (scale_arr(k,SCALE_OUT,iCell) - eps)
                  scale_arr(k,SCALE_OUT,iCell) = min( 1.0_RKIND, max( 0.0_RKIND, scale_factor) )
               end do
            end do
         end if

         !$acc end parallel

         !
         !  communicate scale factors here.
         !  communicate only first halo row in these next two exchanges
         !

         MPAS_ACC_TIMER_START('atm_advance_scalars_mono [ACC_data_xfer]')
         !$acc update self(scale_arr)
         MPAS_ACC_TIMER_STOP('atm_advance_scalars_mono [ACC_data_xfer]')

!$OMP BARRIER
!$OMP MASTER
         call exchange_halo_group(block % domain, 'dynamics:scale')
!$OMP END MASTER
!$OMP BARRIER

         MPAS_ACC_TIMER_START('atm_advance_scalars_mono [ACC_data_xfer]')
         !$acc update device(scale_arr)
         MPAS_ACC_TIMER_STOP('atm_advance_scalars_mono [ACC_data_xfer]')

         !$acc parallel

         !$acc loop gang worker
         do iEdge=edgeStart,edgeEnd
            cell1 = cellsOnEdge(1,iEdge)
            cell2 = cellsOnEdge(2,iEdge)

            if (cell1 <= nCellsSolve .or. cell2 <= nCellsSolve) then  ! only for owned cells

               !$acc loop vector
               do k=1, nVertLevels
                  flux_upwind = dvEdge(iEdge) * dt *   &
                         (max(0.0_RKIND,uhAvg(k,iEdge))*scalar_old(k,cell1) + min(0.0_RKIND,uhAvg(k,iEdge))*scalar_old(k,cell2))
                  flux_arr(k,iEdge) = dt*flux_arr(k,iEdge) - flux_upwind
               end do

               if( config_apply_lbcs .and. (bdyMaskEdge(iEdge) == nRelaxZone) .or. (bdyMaskEdge(iEdge) == nRelaxZone-1) ) then
                  !$acc loop vector
                  do k=1,nVertLevels
                     flux_arr(k,iEdge) = 0.0_RKIND
                  end do
               end if

            end if
         end do

         !
         !  rescale the fluxes
         !

         ! moved assignment to scalar_new from separate loop (see commented code below)
         ! into the following loops. Avoids having to save elements of flux array
         !$acc loop gang worker
         do iEdge=edgeStart,edgeEnd
            cell1 = cellsOnEdge(1,iEdge)
            cell2 = cellsOnEdge(2,iEdge)

            if (cell1 <= nCellsSolve .or. cell2 <= nCellsSolve) then

               !$acc loop vector
               do k = 1, nVertLevels
                  flux = flux_arr(k,iEdge)
                  flux = max(0.0_RKIND,flux) * min(scale_arr(k,SCALE_OUT,cell1), scale_arr(k,SCALE_IN, cell2)) &
                       + min(0.0_RKIND,flux) * min(scale_arr(k,SCALE_IN, cell1), scale_arr(k,SCALE_OUT,cell2))
                  flux_arr(k,iEdge) = flux
               end do
            end if
         end do

         !$acc end parallel

         !
         ! rescale the vertical flux
         !

!$OMP BARRIER

         !$acc parallel
 
         !$acc loop gang worker
         do iCell=cellSolveStart,cellSolveEnd

            !$acc loop vector
            do k = 2, nVertLevels
               flux = wdtn(k,iCell)
               flux = max(0.0_RKIND,flux) * min(scale_arr(k-1,SCALE_OUT,iCell), scale_arr(k  ,SCALE_IN,iCell)) &
                    + min(0.0_RKIND,flux) * min(scale_arr(k  ,SCALE_OUT,iCell), scale_arr(k-1,SCALE_IN,iCell))
               wdtn(k,iCell) = flux
            end do
         end do

         !
         !  do the scalar update now that we have the fluxes
         !
         !$acc loop gang worker
         do iCell=cellSolveStart,cellSolveEnd

            !$acc loop seq
            do i=1,nEdgesOnCell(iCell)
               iEdge = edgesOnCell(i,iCell)

               !$acc loop vector
               do k=1,nVertLevels
                  scalar_new(k,iCell) = scalar_new(k,iCell) - edgesOnCell_sign(i,iCell)*flux_arr(k,iEdge) * invAreaCell(iCell)
               end do
            end do

            if (local_advance_density) then
               !$acc loop vector
               do k=1,nVertLevels
                  scalar_new(k,iCell) = (scalar_new(k,iCell) + (-rdnw(k)*(wdtn(k+1,iCell)-wdtn(k,iCell)) ) )/rho_zz_int(k,iCell)
               end do
            else
               !$acc loop vector
               do k=1,nVertLevels
                  scalar_new(k,iCell) = (scalar_new(k,iCell) + (-rdnw(k)*(wdtn(k+1,iCell)-wdtn(k,iCell)) ) )/rho_zz_new(k,iCell)
               end do
            end if
         end do

         !$acc end parallel

#ifdef DEBUG_TRANSPORT
         !$acc update self(scalar_new)
         !$acc update self(s_max)
         !$acc update self(s_min)

         scmin = scalar_new(1,1)
         scmax = scalar_new(1,1)
         do iCell = 1, nCellsSolve
         do k=1, nVertLevels
            scmax = max(scmax,scalar_new(k,iCell))
            scmin = min(scmin,scalar_new(k,iCell))
            if (s_max(k,iCell) < scalar_new(k,iCell)) then
               call mpas_log_write(' over - k,iCell,s_min,s_max,scalar_new ', intArgs=(/k,iCell/), realArgs=(/s_min(k,iCell),s_max(k,iCell),scalar_new(k,iCell)/))
            end if
            if (s_min(k,iCell) > scalar_new(k,iCell)) then
               call mpas_log_write(' under - k,iCell,s_min,s_max,scalar_new ', intArgs=(/k,iCell/), realArgs=(/s_min(k,iCell),s_max(k,iCell),scalar_new(k,iCell)/))
            end if
         end do
         end do
         call mpas_log_write(' scmin, scmax new out $r $r', realArgs=(/scmin,scmax/))
         call mpas_log_write(' icell_min, k_min ', intArgs=(/icellmax, kmax/))
#endif

         ! the update should be positive definite. but roundoff can sometimes leave small negative values
         ! hence the enforcement of PD in the copy back to the model state.
!$OMP BARRIER

         !$acc parallel

         !$acc loop gang worker
         do iCell=cellStart,cellEnd
            if(bdyMaskCell(iCell) <= nSpecZone) then ! regional_MPAS does spec zone update after transport.
               !$acc loop vector
               do k=1,nVertLevels
                  scalars_new(iScalar,k,iCell) = max(0.0_RKIND,scalar_new(k,iCell))
               end do
            end if
         end do

         !$acc end parallel

      end do !  loop over scalars

      MPAS_ACC_TIMER_START('atm_advance_scalars_mono [ACC_data_xfer]')
      if (local_advance_density) then
         !$acc exit data copyout(rho_zz_int)
      else
         !$acc exit data delete(rho_zz_new)
      end if
      !$acc exit data copyout(scalars_new)
      !$acc exit data delete(scalars_old, scalar_old, scalar_new, scale_arr, s_min, s_max, &
      !$acc                  rho_zz_old, flux_arr, flux_tmp, flux_upwind_tmp, wdtn, wwAvg, &
      !$acc                  uhAvg, fnm, fnp, rdnw)

      !$acc end data
      MPAS_ACC_TIMER_STOP('atm_advance_scalars_mono [ACC_data_xfer]')

   end subroutine atm_advance_scalars_mono_work


   subroutine atm_compute_dyn_tend(tend, tend_physics, state, diag, mesh, configs, nVertLevels, rk_step, dt, &
                                   dt_rk, &  ! MW on ITM  
                                   cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd, &
                                   cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Compute height and normal wind tendencies, as well as diagnostic variables
   !
   ! Input: state - current model state
   !        mesh - grid metadata
   !        diag - some grid diagnostics
   !
   ! Output: tend - tendencies: tend_u, tend_w, tend_theta and tend_rho
   !                these are all coupled-variable tendencies.
   !         various other quantities in diag: Smagorinsky eddy viscosity
   !                
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 

      implicit none

      !
      ! Dummy arguments
      !
      type (mpas_pool_type), intent(inout) :: tend
      type (mpas_pool_type), pointer :: tend_physics
      type (mpas_pool_type), intent(in) :: state
      type (mpas_pool_type), intent(in) :: diag
      type (mpas_pool_type), intent(in) :: mesh
      type (mpas_pool_type), intent(in) :: configs
      integer, intent(in) :: nVertLevels              ! for allocating stack variables
      integer, intent(in) :: rk_step
      real (kind=RKIND), intent(in) :: dt
      real (kind=RKIND), intent(in) :: dt_rk   ! MW on ITM
      integer, intent(in) :: cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd
      integer, intent(in) :: cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd

      !
      ! Local variables
      !
      integer, pointer :: nCells, nEdges, nVertices, nCellsSolve, nEdgesSolve, vertexDegree, maxEdges, maxEdges2
      integer, pointer :: moist_start, moist_end, num_scalars

      real (kind=RKIND), dimension(:), pointer ::  fEdge, dvEdge, dcEdge, invDcEdge, invDvEdge, invAreaCell, invAreaTriangle, &
                                                   meshScalingDel2, meshScalingDel4
      real (kind=RKIND), dimension(:,:), pointer :: weightsOnEdge, zgrid, rho_edge, rho_zz, ru, u, v, tend_u, &
                                                    divergence, vorticity, ke, pv_edge, theta_m, rw, tend_rho, &
                                                    rt_diabatic_tend, tend_theta, tend_w, w, cqw, rb, rr, pp, pressure_b, zz, zxu, cqu, & 
                                                    h_divergence, kdiff, edgesOnCell_sign, edgesOnVertex_sign, rw_save, ru_save

      real (kind=RKIND), dimension(:,:), pointer :: theta_m_save

      real (kind=RKIND), dimension(:,:), pointer :: exner

      real (kind=RKIND), dimension(:,:), pointer :: rr_save

      real (kind=RKIND), dimension(:,:), pointer :: rthdynten

      real (kind=RKIND), dimension(:,:,:), pointer :: scalars

      real (kind=RKIND), dimension(:,:), pointer :: tend_u_euler, tend_w_euler, tend_theta_euler

      real (kind=RKIND), dimension(:,:,:), pointer :: deriv_two
      integer, dimension(:,:), pointer :: cellsOnEdge, verticesOnEdge, edgesOnCell, edgesOnEdge, cellsOnCell, edgesOnVertex
      integer, dimension(:), pointer :: nEdgesOnCell, nEdgesOnEdge
      real (kind=RKIND), dimension(:), pointer :: latCell, latEdge, angleEdge, u_init, v_init

      integer, dimension(:,:), pointer :: advCellsForEdge
      integer, dimension(:), pointer :: nAdvCellsForEdge
      real (kind=RKIND), dimension(:,:), pointer :: adv_coefs, adv_coefs_3rd

      real (kind=RKIND), dimension(:), pointer :: rdzu, rdzw, fzm, fzp, qv_init
      real (kind=RKIND), dimension(:,:), pointer :: t_init 

      real (kind=RKIND), pointer :: cf1, cf2, cf3

      real (kind=RKIND), pointer :: r_earth
      real (kind=RKIND), dimension(:,:), pointer :: ur_cell, vr_cell

      real (kind=RKIND), dimension(:,:), pointer :: defc_a, defc_b

      real(kind=RKIND), dimension(:,:), pointer :: tend_w_pgf, tend_w_buoy

      real (kind=RKIND), pointer :: coef_3rd_order, c_s
      logical, pointer :: config_mix_full
      character (len=StrKIND), pointer :: config_horiz_mixing
      real (kind=RKIND), pointer :: config_del4u_div_factor
      real (kind=RKIND), pointer :: config_h_theta_eddy_visc4
      real (kind=RKIND), pointer :: config_h_mom_eddy_visc4
      real (kind=RKIND), pointer :: config_visc4_2dsmag
      real (kind=RKIND), pointer :: config_len_disp
      real (kind=RKIND), pointer :: config_h_mom_eddy_visc2, config_v_mom_eddy_visc2
      real (kind=RKIND), pointer :: config_h_theta_eddy_visc2, config_v_theta_eddy_visc2

      real (kind=RKIND), pointer :: config_mpas_cam_coef
      logical, pointer :: config_rayleigh_damp_u
      real (kind=RKIND), pointer :: config_rayleigh_damp_u_timescale_days
      integer, pointer :: config_number_rayleigh_damp_u_levels, config_number_cam_damping_levels

      ! MW on ITM: dynamic and physics tendencies 
      logical, pointer :: config_tend, config_pv_tend
      real (kind=RKIND), dimension(:,:), pointer :: ru_tend_dyn_large, ru_tend_physics, &
                                                    ru_tend_diff, ru_tend_smdiv
      real (kind=RKIND), dimension(:,:), pointer :: rw_tend_diff      ! adding this for PV friction term
      real (kind=RKIND), dimension(:,:), pointer :: rw_tend_dyn_large ! MW added for PV 
      real (kind=RKIND), dimension(:,:), pointer :: rth_tend_dyn_large, rth_tend_physics, &
                                                    rth_tend_diff


      call mpas_pool_get_config(mesh, 'sphere_radius', r_earth)
      call mpas_pool_get_config(configs, 'config_coef_3rd_order', coef_3rd_order)
      call mpas_pool_get_config(configs, 'config_mix_full', config_mix_full)
      call mpas_pool_get_config(configs, 'config_horiz_mixing', config_horiz_mixing)
      call mpas_pool_get_config(configs, 'config_del4u_div_factor', config_del4u_div_factor)
      call mpas_pool_get_config(configs, 'config_h_theta_eddy_visc4', config_h_theta_eddy_visc4)
      call mpas_pool_get_config(configs, 'config_h_mom_eddy_visc4', config_h_mom_eddy_visc4)
      call mpas_pool_get_config(configs, 'config_h_theta_eddy_visc2', config_h_theta_eddy_visc2)
      call mpas_pool_get_config(configs, 'config_h_mom_eddy_visc2', config_h_mom_eddy_visc2)
      call mpas_pool_get_config(configs, 'config_v_theta_eddy_visc2', config_v_theta_eddy_visc2)
      call mpas_pool_get_config(configs, 'config_v_mom_eddy_visc2', config_v_mom_eddy_visc2)
      call mpas_pool_get_config(configs, 'config_visc4_2dsmag', config_visc4_2dsmag)
      call mpas_pool_get_config(configs, 'config_len_disp', config_len_disp)
      call mpas_pool_get_config(configs, 'config_smagorinsky_coef', c_s)
      call mpas_pool_get_config(configs, 'config_mpas_cam_coef', config_mpas_cam_coef)
      call mpas_pool_get_config(configs, 'config_rayleigh_damp_u', config_rayleigh_damp_u)
      call mpas_pool_get_config(configs, 'config_rayleigh_damp_u_timescale_days', config_rayleigh_damp_u_timescale_days)
      call mpas_pool_get_config(configs, 'config_number_rayleigh_damp_u_levels', config_number_rayleigh_damp_u_levels)
      call mpas_pool_get_config(configs, 'config_number_cam_damping_levels', config_number_cam_damping_levels)

      call mpas_pool_get_array(state, 'rho_zz', rho_zz, 2)
      call mpas_pool_get_array(state, 'u', u, 2)
      call mpas_pool_get_array(state, 'w', w, 2)
      call mpas_pool_get_array(state, 'theta_m', theta_m, 2)
      call mpas_pool_get_array(state, 'theta_m', theta_m_save, 1)
      call mpas_pool_get_array(state, 'scalars', scalars, 2)

      call mpas_pool_get_array(diag, 'uReconstructZonal', ur_cell)
      call mpas_pool_get_array(diag, 'uReconstructMeridional', vr_cell)
      call mpas_pool_get_array(diag, 'rho_edge', rho_edge)
      call mpas_pool_get_array(diag, 'rho_base', rb)
      call mpas_pool_get_array(diag, 'rho_p', rr)
      call mpas_pool_get_array(diag, 'rho_p_save', rr_save)
      call mpas_pool_get_array(diag, 'v', v)
      call mpas_pool_get_array(diag, 'kdiff', kdiff)
      call mpas_pool_get_array(diag, 'ru', ru)
      call mpas_pool_get_array(diag, 'ru_save', ru_save)
      call mpas_pool_get_array(diag, 'rw', rw)
      call mpas_pool_get_array(diag, 'rw_save', rw_save)
      call mpas_pool_get_array(diag, 'divergence', divergence)
      call mpas_pool_get_array(diag, 'vorticity', vorticity)
      call mpas_pool_get_array(diag, 'ke', ke)
      call mpas_pool_get_array(diag, 'pv_edge', pv_edge)
      call mpas_pool_get_array(diag, 'pressure_p', pp)
      call mpas_pool_get_array(diag, 'pressure_base', pressure_b)
      call mpas_pool_get_array(diag, 'h_divergence', h_divergence)
      call mpas_pool_get_array(diag, 'exner', exner)

      call mpas_pool_get_array(tend_physics, 'rthdynten', rthdynten)

      call mpas_pool_get_array(mesh, 'weightsOnEdge', weightsOnEdge)
      call mpas_pool_get_array(mesh, 'cellsOnEdge', cellsOnEdge)
      call mpas_pool_get_array(mesh, 'cellsOnCell', cellsOnCell)
      call mpas_pool_get_array(mesh, 'verticesOnEdge', verticesOnEdge)
      call mpas_pool_get_array(mesh, 'nEdgesOnEdge', nEdgesOnEdge)
      call mpas_pool_get_array(mesh, 'edgesOnEdge', edgesOnEdge)
      call mpas_pool_get_array(mesh, 'edgesOnCell', edgesOnCell)
      call mpas_pool_get_array(mesh, 'edgesOnCell_sign', edgesOnCell_sign)
      call mpas_pool_get_array(mesh, 'edgesOnVertex', edgesOnVertex)
      call mpas_pool_get_array(mesh, 'edgesOnVertex_sign', edgesOnVertex_sign)
      call mpas_pool_get_array(mesh, 'dcEdge', dcEdge)
      call mpas_pool_get_array(mesh, 'dvEdge', dvEdge)
      call mpas_pool_get_array(mesh, 'invDcEdge', invDcEdge)
      call mpas_pool_get_array(mesh, 'invDvEdge', invDvEdge)
      call mpas_pool_get_array(mesh, 'invAreaCell', invAreaCell)
      call mpas_pool_get_array(mesh, 'invAreaTriangle', invAreaTriangle)
      call mpas_pool_get_array(mesh, 'fEdge', fEdge)
      call mpas_pool_get_array(mesh, 'deriv_two', deriv_two)
      call mpas_pool_get_array(mesh, 'zz', zz)
      call mpas_pool_get_array(mesh, 'zxu', zxu)
      call mpas_pool_get_array(mesh, 'latCell', latCell)
      call mpas_pool_get_array(mesh, 'latEdge', latEdge)
      call mpas_pool_get_array(mesh, 'angleEdge', angleEdge)
      call mpas_pool_get_array(mesh, 'defc_a', defc_a)
      call mpas_pool_get_array(mesh, 'defc_b', defc_b)
      call mpas_pool_get_array(mesh, 'meshScalingDel2', meshScalingDel2)
      call mpas_pool_get_array(mesh, 'meshScalingDel4', meshScalingDel4)
      call mpas_pool_get_array(mesh, 'u_init', u_init)
      call mpas_pool_get_array(mesh, 'v_init', v_init)
      call mpas_pool_get_array(mesh, 't_init', t_init)
      call mpas_pool_get_array(mesh, 'qv_init', qv_init)

      call mpas_pool_get_array(mesh, 'rdzu', rdzu)
      call mpas_pool_get_array(mesh, 'rdzw', rdzw)
      call mpas_pool_get_array(mesh, 'fzm', fzm)
      call mpas_pool_get_array(mesh, 'fzp', fzp)
      call mpas_pool_get_array(mesh, 'zgrid', zgrid)

      call mpas_pool_get_array(tend, 'u', tend_u)
      call mpas_pool_get_array(tend, 'theta_m', tend_theta)
      call mpas_pool_get_array(tend, 'w', tend_w)
      call mpas_pool_get_array(tend, 'rho_zz', tend_rho)
      call mpas_pool_get_array(tend, 'rt_diabatic_tend', rt_diabatic_tend)
      call mpas_pool_get_array(tend, 'u_euler', tend_u_euler)
      call mpas_pool_get_array(tend, 'theta_euler', tend_theta_euler)
      call mpas_pool_get_array(tend, 'w_euler', tend_w_euler)
      call mpas_pool_get_array(tend, 'w_pgf', tend_w_pgf)
      call mpas_pool_get_array(tend, 'w_buoy', tend_w_buoy)

      call mpas_pool_get_array(diag, 'cqw', cqw)
      call mpas_pool_get_array(diag, 'cqu', cqu)

      call mpas_pool_get_dimension(mesh, 'nCells', nCells)
      call mpas_pool_get_dimension(mesh, 'nEdges', nEdges)
      call mpas_pool_get_dimension(mesh, 'nVertices', nVertices)
      call mpas_pool_get_dimension(mesh, 'nCellsSolve', nCellsSolve)
      call mpas_pool_get_dimension(mesh, 'nEdgesSolve', nEdgesSolve)
      call mpas_pool_get_dimension(mesh, 'vertexDegree', vertexDegree)
      call mpas_pool_get_dimension(mesh, 'maxEdges', maxEdges)
      call mpas_pool_get_dimension(mesh, 'maxEdges2', maxEdges2)

      call mpas_pool_get_dimension(state, 'num_scalars', num_scalars)
      call mpas_pool_get_dimension(state, 'moist_start', moist_start)
      call mpas_pool_get_dimension(state, 'moist_end', moist_end)

      call mpas_pool_get_array(mesh, 'nEdgesOnCell', nEdgesOnCell)
      call mpas_pool_get_array(mesh, 'nAdvCellsForEdge', nAdvCellsForEdge)
      call mpas_pool_get_array(mesh, 'advCellsForEdge', advCellsForEdge)
      call mpas_pool_get_array(mesh, 'adv_coefs', adv_coefs)
      call mpas_pool_get_array(mesh, 'adv_coefs_3rd', adv_coefs_3rd)

      call mpas_pool_get_array(mesh, 'cf1', cf1)
      call mpas_pool_get_array(mesh, 'cf2', cf2)
      call mpas_pool_get_array(mesh, 'cf3', cf3)


      ! MW on ITM 
      call mpas_pool_get_config(configs, 'config_tend', config_tend)
      call mpas_pool_get_config(configs, 'config_pv_tend', config_pv_tend)

      if (config_tend) then
         call mpas_pool_get_array(diag, 'ru_tend_dyn_large', ru_tend_dyn_large)
         call mpas_pool_get_array(diag, 'ru_tend_diff', ru_tend_diff)
         call mpas_pool_get_array(diag, 'ru_tend_physics', ru_tend_physics)
         call mpas_pool_get_array(diag, 'ru_tend_smdiv', ru_tend_smdiv)
         call mpas_pool_get_array(diag, 'rth_tend_dyn_large', rth_tend_dyn_large)
         call mpas_pool_get_array(diag, 'rth_tend_diff', rth_tend_diff)
         call mpas_pool_get_array(diag, 'rth_tend_physics', rth_tend_physics)
      else  ! MC: adding allocation
         allocate(ru_tend_dyn_large(nVertLevels,nEdges+1))
         allocate(ru_tend_diff(nVertLevels,nEdges+1))
         allocate(ru_tend_physics(nVertLevels,nEdges+1))
         allocate(ru_tend_smdiv(nVertLevels,nEdges+1))
         allocate(rth_tend_dyn_large(nVertLevels,nCells+1))
         allocate(rth_tend_diff(nVertLevels,nCells+1))
         allocate(rth_tend_physics(nVertLevels,nCells+1))
      end if

      if (config_pv_tend) then
         call mpas_pool_get_array(diag, 'rw_tend_dyn_large', rw_tend_dyn_large)
         call mpas_pool_get_array(diag, 'rw_tend_diff', rw_tend_diff)
      else   ! MC: adding allocation
         allocate(rw_tend_dyn_large(nVertLevels+1,nCells+1))
         allocate(rw_tend_diff(nVertLevels+1,nCells+1))
      end if
      ! end ITM and PV


      call atm_compute_dyn_tend_work(nCells, nEdges, nVertices, nVertLevels, &
                  nCellsSolve, nEdgesSolve, vertexDegree, maxEdges, maxEdges2, num_scalars, moist_start, moist_end, &
                  fEdge, dvEdge, dcEdge, invDcEdge, invDvEdge, invAreaCell, invAreaTriangle, meshScalingDel2, meshScalingDel4, &
                  weightsOnEdge, zgrid, rho_edge, rho_zz, ru, u, v, tend_u, &
                  divergence, vorticity, ke, pv_edge, theta_m, rw, tend_rho, &
                  rt_diabatic_tend, tend_theta, tend_w, w, cqw, rb, rr, pp, pressure_b, zz, zxu, cqu, &
                  h_divergence, kdiff, edgesOnCell_sign, edgesOnVertex_sign, rw_save, ru_save, &
                  theta_m_save, exner, rr_save, scalars, tend_u_euler, tend_w_euler, tend_theta_euler, deriv_two, &
                  cellsOnEdge, verticesOnEdge, edgesOnCell, edgesOnEdge, cellsOnCell, edgesOnVertex, nEdgesOnCell, nEdgesOnEdge, &
                  latCell, latEdge, angleEdge, u_init, v_init, advCellsForEdge, nAdvCellsForEdge, adv_coefs, adv_coefs_3rd, &
                  rdzu, rdzw, fzm, fzp, qv_init, t_init, cf1, cf2, cf3, r_earth, ur_cell, vr_cell, defc_a, defc_b, &
                  tend_w_pgf, tend_w_buoy, coef_3rd_order, c_s, config_mix_full, config_horiz_mixing, config_del4u_div_factor, &
                  config_h_mom_eddy_visc2, config_v_mom_eddy_visc2, config_h_theta_eddy_visc2, config_v_theta_eddy_visc2, &
                  config_h_theta_eddy_visc4, config_h_mom_eddy_visc4, config_visc4_2dsmag, config_len_disp, rk_step, dt, &
                  config_mpas_cam_coef, &
                  config_rayleigh_damp_u, config_rayleigh_damp_u_timescale_days, &
                  config_number_rayleigh_damp_u_levels, config_number_cam_damping_levels, &
                  rthdynten, &
                  cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd, &
                  cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd, &
                  ! MW on ITM
                  dt_rk, config_tend, config_pv_tend, &
                  ru_tend_dyn_large=ru_tend_dyn_large, ru_tend_diff=ru_tend_diff, ru_tend_physics=ru_tend_physics, ru_tend_smdiv=ru_tend_smdiv, &  ! optional
                  rth_tend_dyn_large=rth_tend_dyn_large, rth_tend_diff=rth_tend_diff, rth_tend_physics=rth_tend_physics,  &  ! optional
                  rw_tend_dyn_large=rw_tend_dyn_large, rw_tend_diff=rw_tend_diff & ! for PV, optional
                  )


      ! MC: diagnostic deallocation
      if (.not. config_tend) then
         deallocate(ru_tend_dyn_large)
         deallocate(ru_tend_diff)
         deallocate(ru_tend_physics)
         deallocate(ru_tend_smdiv)
         deallocate(rth_tend_dyn_large)
         deallocate(rth_tend_diff)
         deallocate(rth_tend_physics)
      end if
      if (.not. config_pv_tend) then
         deallocate(rw_tend_dyn_large)
         deallocate(rw_tend_diff)
      end if

   end subroutine atm_compute_dyn_tend


   subroutine atm_compute_dyn_tend_work(nCells, nEdges, nVertices, nVertLevels_dummy, &
               nCellsSolve, nEdgesSolve, vertexDegree, maxEdges_dummy, maxEdges2_dummy, num_scalars_dummy, moist_start, moist_end, &
               fEdge, dvEdge, dcEdge, invDcEdge, invDvEdge, invAreaCell, invAreaTriangle, meshScalingDel2, meshScalingDel4, &
               weightsOnEdge, zgrid, rho_edge, rho_zz, ru, u, v, tend_u, &
               divergence, vorticity, ke, pv_edge, theta_m, rw, tend_rho, &
               rt_diabatic_tend, tend_theta, tend_w, w, cqw, rb, rr, pp, pressure_b, zz, zxu, cqu, &
               h_divergence, kdiff, edgesOnCell_sign, edgesOnVertex_sign, rw_save, ru_save, &
               theta_m_save, exner, rr_save, scalars, tend_u_euler, tend_w_euler, tend_theta_euler, deriv_two, &
               cellsOnEdge, verticesOnEdge, edgesOnCell, edgesOnEdge, cellsOnCell, edgesOnVertex, nEdgesOnCell, nEdgesOnEdge, &
               latCell, latEdge, angleEdge, u_init, v_init, advCellsForEdge, nAdvCellsForEdge, adv_coefs, adv_coefs_3rd, &
               rdzu, rdzw, fzm, fzp, qv_init, t_init, cf1, cf2, cf3, r_earth, ur_cell, vr_cell, defc_a, defc_b, &
               tend_w_pgf, tend_w_buoy, coef_3rd_order, c_s, config_mix_full, config_horiz_mixing, config_del4u_div_factor, &
               config_h_mom_eddy_visc2, config_v_mom_eddy_visc2, config_h_theta_eddy_visc2, config_v_theta_eddy_visc2, &
               config_h_theta_eddy_visc4, config_h_mom_eddy_visc4, config_visc4_2dsmag, config_len_disp, rk_step, dt, &
               config_mpas_cam_coef, &
               config_rayleigh_damp_u, config_rayleigh_damp_u_timescale_days, &
               config_number_rayleigh_damp_u_levels, config_number_cam_damping_levels, &
               rthdynten, &
               cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd, &
               cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd, &
               ! MW on ITM  
               dt_rk, config_tend, config_pv_tend, &
               ru_tend_dyn_large, ru_tend_diff, ru_tend_physics, ru_tend_smdiv, &
               rth_tend_dyn_large, rth_tend_diff, rth_tend_physics, &
               rw_tend_dyn_large, rw_tend_diff & ! adding this for PV friction term
               !
               )


      use mpas_atm_dimensions


      implicit none


      !
      ! Dummy arguments
      !
      integer :: nCells, nEdges, nVertices, nVertLevels_dummy, nCellsSolve, nEdgesSolve, vertexDegree, &
                 maxEdges_dummy, maxEdges2_dummy, num_scalars_dummy, moist_start, moist_end

      real (kind=RKIND), dimension(nEdges+1) :: fEdge
      real (kind=RKIND), dimension(nEdges+1) :: dvEdge
      real (kind=RKIND), dimension(nEdges+1) :: dcEdge
      real (kind=RKIND), dimension(nEdges+1) :: invDcEdge
      real (kind=RKIND), dimension(nEdges+1) :: invDvEdge
      real (kind=RKIND), dimension(nCells+1) :: invAreaCell
      real (kind=RKIND), dimension(nVertices+1) :: invAreaTriangle
      real (kind=RKIND), dimension(nEdges+1) :: meshScalingDel2
      real (kind=RKIND), dimension(nEdges+1) :: meshScalingDel4
      real (kind=RKIND), dimension(maxEdges2,nEdges+1) :: weightsOnEdge
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: zgrid
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: rho_edge
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rho_zz
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: ru
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: u
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: v
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: tend_u
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: divergence
      real (kind=RKIND), dimension(nVertLevels,nVertices+1) :: vorticity
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: ke
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: pv_edge
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: theta_m
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: rw
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: tend_rho
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rt_diabatic_tend
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: tend_theta
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: tend_w
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: w
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: cqw
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rb
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rr
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: pp
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: pressure_b
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: zz
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: zxu
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: cqu
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: h_divergence
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: kdiff
      real (kind=RKIND), dimension(maxEdges,nCells+1) :: edgesOnCell_sign
      real (kind=RKIND), dimension(vertexDegree,nVertices+1) :: edgesOnVertex_sign
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: rw_save
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: ru_save

      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: theta_m_save
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: exner
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rr_save
      real (kind=RKIND), dimension(num_scalars,nVertLevels,nCells+1) :: scalars
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: tend_u_euler
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: tend_w_euler
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: tend_theta_euler
      real (kind=RKIND), dimension(15,2,nEdges+1) :: deriv_two
      integer, dimension(2,nEdges+1) :: cellsOnEdge
      integer, dimension(2,nEdges+1) :: verticesOnEdge
      integer, dimension(maxEdges,nCells+1) :: edgesOnCell
      integer, dimension(maxEdges2,nEdges+1) :: edgesOnEdge
      integer, dimension(maxEdges,nCells+1) :: cellsOnCell
      integer, dimension(vertexDegree,nVertices+1) :: edgesOnVertex
      integer, dimension(nCells+1) :: nEdgesOnCell
      integer, dimension(nEdges+1) :: nEdgesOnEdge
      real (kind=RKIND), dimension(nCells+1) :: latCell
      real (kind=RKIND), dimension(nEdges+1) :: latEdge
      real (kind=RKIND), dimension(nEdges+1) :: angleEdge
      real (kind=RKIND), dimension(nVertLevels) :: u_init, v_init

      integer, dimension(15,nEdges+1) :: advCellsForEdge
      integer, dimension(nEdges+1) :: nAdvCellsForEdge
      real (kind=RKIND), dimension(15,nEdges+1) :: adv_coefs
      real (kind=RKIND), dimension(15,nEdges+1) :: adv_coefs_3rd

      real (kind=RKIND), dimension(nVertLevels) :: rdzu
      real (kind=RKIND), dimension(nVertLevels) :: rdzw
      real (kind=RKIND), dimension(nVertLevels) :: fzm
      real (kind=RKIND), dimension(nVertLevels) :: fzp
      real (kind=RKIND), dimension(nVertLevels) :: qv_init
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: t_init 

      real (kind=RKIND) :: cf1, cf2, cf3
      real (kind=RKIND) :: prandtl_inv, r_areaCell, rgas_cprcv

      real (kind=RKIND) :: r_earth
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: ur_cell
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: vr_cell

      real (kind=RKIND), dimension(maxEdges,nCells+1) :: defc_a
      real (kind=RKIND), dimension(maxEdges,nCells+1) :: defc_b

      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: tend_w_pgf
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: tend_w_buoy

      real (kind=RKIND) :: coef_3rd_order, c_s
      logical :: config_mix_full
      character (len=StrKIND) :: config_horiz_mixing
      real (kind=RKIND) :: config_del4u_div_factor
      real (kind=RKIND) :: config_h_theta_eddy_visc4
      real (kind=RKIND) :: config_h_mom_eddy_visc4
      real (kind=RKIND) :: config_visc4_2dsmag
      real (kind=RKIND) :: config_len_disp
      real (kind=RKIND) :: config_h_mom_eddy_visc2, config_v_mom_eddy_visc2, config_h_theta_eddy_visc2, config_v_theta_eddy_visc2

      integer, intent(in) :: rk_step
      real (kind=RKIND), intent(in) :: dt

      real (kind=RKIND) :: config_mpas_cam_coef

      logical, intent(in) :: config_rayleigh_damp_u
      real (kind=RKIND), intent(in) :: config_rayleigh_damp_u_timescale_days
      integer, intent(in) :: config_number_rayleigh_damp_u_levels, config_number_cam_damping_levels

      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rthdynten

      integer, intent(in) :: cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd
      integer, intent(in) :: cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd

      !
      ! MW on ITM 
      !
      logical, intent(in) :: config_tend, config_pv_tend
      real (kind=RKIND), intent(in) :: dt_rk
      real (kind=RKIND), dimension(nVertLevels,nEdges+1), optional :: ru_tend_dyn_large, ru_tend_diff, ru_tend_physics, &
                                                            ru_tend_smdiv
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1), optional ::rw_tend_dyn_large, rw_tend_diff
      real (kind=RKIND), dimension(nVertLevels,nCells+1), optional :: rth_tend_dyn_large, rth_tend_diff, rth_tend_physics

      !
      ! Local variables
      !
      integer :: iEdge, iCell, iVertex, k, cell1, cell2, vertex1, vertex2, eoe, i, j, iq, iAdvCell

      !real (kind=RKIND), parameter :: c_s = 0.125
      real (kind=RKIND), dimension( nVertLevels+1 ) :: d_diag, d_off_diag, flux_arr
      real (kind=RKIND), dimension( nVertLevels + 1 ) :: wduz, wdwz, wdtz, dpzx
      real (kind=RKIND), dimension( nVertLevels ) :: ru_edge_w, q, u_mix
      real (kind=RKIND) :: theta_turb_flux, w_turb_flux, r
      real (kind=RKIND) :: scalar_weight
      real (kind=RKIND) :: inv_r_earth

      real (kind=RKIND) :: invDt, flux, workpv
      real (kind=RKIND) :: edge_sign, pr_scale, r_dc, r_dv, u_mix_scale
      real (kind=RKIND) :: h_mom_eddy_visc4, v_mom_eddy_visc2
      real (kind=RKIND) :: h_theta_eddy_visc4, v_theta_eddy_visc2
      real (kind=RKIND) :: u_diffusion

      real (kind=RKIND) :: kdiffu, z1, z2, z3, z4, zm, z0, zp, rayleigh_coef_inverse, visc2cam

      real (kind=RKIND), dimension( nVertLevels ) :: rayleigh_damp_coef

      real (kind=RKIND) :: flux3, flux4
      real (kind=RKIND) :: q_im2, q_im1, q_i, q_ip1, ua, coef3

      flux4(q_im2, q_im1, q_i, q_ip1, ua) =                     &
                ua*( 7.*(q_i + q_im1) - (q_ip1 + q_im2) )/12.0

      flux3(q_im2, q_im1, q_i, q_ip1, ua, coef3) =              &
                flux4(q_im2, q_im1, q_i, q_ip1, ua) +           &
                coef3*abs(ua)*((q_ip1 - q_im2)-3.*(q_i-q_im1))/12.0

      prandtl_inv = 1.0_RKIND / prandtl
      invDt = 1.0_RKIND / dt
      inv_r_earth = 1.0_RKIND / r_earth

       v_mom_eddy_visc2   = config_v_mom_eddy_visc2
       v_theta_eddy_visc2 = config_v_theta_eddy_visc2

      if (rk_step == 1) then

!         tend_u_euler(1:nVertLevels,edgeStart:edgeEnd) = 0.0

         ! Smagorinsky eddy viscosity, based on horizontal deformation (in this case on model coordinate surfaces).
         ! The integration coefficients were precomputed and stored in defc_a and defc_b

         if(config_horiz_mixing == "2d_smagorinsky") then
            do iCell = cellStart,cellEnd
               d_diag(1:nVertLevels) = 0.0
               d_off_diag(1:nVertLevels) = 0.0
               do iEdge=1,nEdgesOnCell(iCell)
                  do k=1,nVertLevels
                     d_diag(k)     = d_diag(k)     + defc_a(iEdge,iCell)*u(k,EdgesOnCell(iEdge,iCell))  &
                                                   - defc_b(iEdge,iCell)*v(k,EdgesOnCell(iEdge,iCell))
                     d_off_diag(k) = d_off_diag(k) + defc_b(iEdge,iCell)*u(k,EdgesOnCell(iEdge,iCell))  &
                                                   + defc_a(iEdge,iCell)*v(k,EdgesOnCell(iEdge,iCell))
                  end do
               end do
!DIR$ IVDEP
               do k=1, nVertLevels
                  ! here is the Smagorinsky formulation, 
                  ! followed by imposition of an upper bound on the eddy viscosity
                  kdiff(k,iCell) = min((c_s * config_len_disp)**2 * sqrt(d_diag(k)**2 + d_off_diag(k)**2),(0.01*config_len_disp**2) * invDt)
               end do
            end do

            h_mom_eddy_visc4   = config_visc4_2dsmag * config_len_disp**3
            h_theta_eddy_visc4 = h_mom_eddy_visc4

         else if(config_horiz_mixing == "2d_fixed") then

            kdiff(1:nVertLevels,cellStart:cellEnd) = config_h_theta_eddy_visc2
            h_mom_eddy_visc4 = config_h_mom_eddy_visc4
            h_theta_eddy_visc4 = config_h_theta_eddy_visc4

         end if

         if (config_mpas_cam_coef > 0.0) then

            do iCell = cellStart,cellEnd
               !
               ! 2nd-order filter for top absorbing layer similar to that in CAM-SE :  WCS 10 May 2017, modified 7 April 2023
               ! From MPAS-CAM V4.0 code, with addition to config-specified coefficient (V4.0_coef = 0.2; SE_coef = 1.0)
               !
               do k = nVertLevels-config_number_cam_damping_levels + 1, nVertLevels
                  visc2cam = 4.0*2.0833*config_len_disp*config_mpas_cam_coef
                  visc2cam = visc2cam*(1.0-real(nVertLevels-k)/real(config_number_cam_damping_levels))
                  kdiff(k  ,iCell) = max(kdiff(nVertLevels  ,iCell),visc2cam)
               end do
            end do

         end if
            
      end if

      ! tendency for density.
      ! accumulate total water here for later use in w tendency calculation.

      ! accumulate horizontal mass-flux

      do iCell=cellStart,cellEnd
         h_divergence(1:nVertLevels,iCell) = 0.0
         do i=1,nEdgesOnCell(iCell)
            iEdge = edgesOnCell(i,iCell)
            edge_sign = edgesOnCell_sign(i,iCell) * dvEdge(iEdge)
!DIR$ IVDEP
            do k=1,nVertLevels
               h_divergence(k,iCell) = h_divergence(k,iCell) + edge_sign * ru(k,iEdge)
            end do
         end do
      end do

      ! compute horiontal mass-flux divergence, add vertical mass flux divergence to complete tend_rho

      do iCell = cellStart,cellEnd
         r = invAreaCell(iCell)
         do k = 1,nVertLevels
            h_divergence(k,iCell) = h_divergence(k,iCell) * r
         end do
      end do    

      !
      ! dp / dz and tend_rho
      !
      ! only needed on first rk_step with pert variables defined a pert from time t
      !
      if(rk_step == 1) then

        rgas_cprcv = rgas*cp/cv
        do iCell = cellStart,cellEnd

!DIR$ IVDEP
          do k = 1,nVertLevels
            tend_rho(k,iCell) = -h_divergence(k,iCell)-rdzw(k)*(rw(k+1,iCell)-rw(k,iCell)) + tend_rho_physics(k,iCell)
            dpdz(k,iCell) = -gravity*(rb(k,iCell)*(qtot(k,iCell)) + rr_save(k,iCell)*(1.+qtot(k,iCell)))
          end do
        end do
      end if

!$OMP BARRIER

      !
      ! Compute u (normal) velocity tendency for each edge (cell face)
      !

      do iEdge=edgeSolveStart,edgeSolveEnd

         cell1 = cellsOnEdge(1,iEdge)
         cell2 = cellsOnEdge(2,iEdge)

         ! horizontal pressure gradient 

         if(rk_step == 1) then
!DIR$ IVDEP
            do k=1,nVertLevels
               tend_u_euler(k,iEdge) =  - cqu(k,iEdge)*( (pp(k,cell2)-pp(k,cell1))*invDcEdge(iEdge)/(.5*(zz(k,cell2)+zz(k,cell1))) &
                                              -0.5*zxu(k,iEdge)*(dpdz(k,cell1)+dpdz(k,cell2)) )
            end do

         end if

         ! vertical transport of u

         wduz(1) = 0.

         k = 2
         wduz(k) =  0.5*( rw(k,cell1)+rw(k,cell2))*(fzm(k)*u(k,iEdge)+fzp(k)*u(k-1,iEdge))
         do k=3,nVertLevels-1
            wduz(k) = flux3( u(k-2,iEdge),u(k-1,iEdge),u(k,iEdge),u(k+1,iEdge),0.5*(rw(k,cell1)+rw(k,cell2)), 1.0_RKIND )
         end do
         k = nVertLevels
         wduz(k) =  0.5*( rw(k,cell1)+rw(k,cell2))*(fzm(k)*u(k,iEdge)+fzp(k)*u(k-1,iEdge))

         wduz(nVertLevels+1) = 0.

!DIR$ IVDEP
         do k=1,nVertLevels
            tend_u(k,iEdge) = - rdzw(k)*(wduz(k+1)-wduz(k)) !  first use of tend_u
         end do

         ! Next, nonlinear Coriolis term (q) following Ringler et al JCP 2009

         q(:) = 0.0
         do j = 1,nEdgesOnEdge(iEdge)
            eoe = edgesOnEdge(j,iEdge)
            do k=1,nVertLevels
               workpv = 0.5 * (pv_edge(k,iEdge) + pv_edge(k,eoe))
!  the original definition of pv_edge had a factor of 1/density.  We have removed that factor
!  given that it was not integral to any conservation property of the system
               q(k) = q(k) + weightsOnEdge(j,iEdge) * u(k,eoe) * workpv
            end do
         end do

!DIR$ IVDEP
         do k=1,nVertLevels

            ! horizontal ke gradient and vorticity terms in the vector invariant formulation
            ! of the horizontal momentum equation
            tend_u(k,iEdge) = tend_u(k,iEdge) + rho_edge(k,iEdge)* (q(k) - (ke(k,cell2) - ke(k,cell1))       &
                                                                 * invDcEdge(iEdge))                            &
                                             - u(k,iEdge)*0.5*(h_divergence(k,cell1)+h_divergence(k,cell2)) 
#ifdef CURVATURE
            ! curvature terms for the sphere
            tend_u(k,iEdge) = tend_u(k,iEdge) &
                             - 2.*omega*cos(angleEdge(iEdge))*cos(latEdge(iEdge))  &
                               *rho_edge(k,iEdge)*.25*(w(k,cell1)+w(k+1,cell1)+w(k,cell2)+w(k+1,cell2))          & 
                             - u(k,iEdge)*.25*(w(k+1,cell1)+w(k,cell1)+w(k,cell2)+w(k+1,cell2))                  &
                               *rho_edge(k,iEdge) * inv_r_earth
#endif
         end do

      end do


      !
      !  horizontal mixing for u
      !  mixing terms are integrated using forward-Euler, so this tendency is only computed in the
      !  first Runge-Kutta substep and saved for use in later RK substeps 2 and 3.
      !

      if (rk_step == 1) then

!$OMP BARRIER

         ! Initialize tendency diagnostic variables 
         if (config_tend)     ru_tend_diff(1:nVertLevels,edgeStart:edgeEnd)   = 0.0 ! MW on ITM
         if (config_pv_tend)  rw_tend_diff(1:nVertLevels+1,cellStart:cellEnd) = 0.0 ! MW: adding this for PV friction term


         ! del^4 horizontal filter.  We compute this as del^2 ( del^2 (u) ).
         ! First, storage to hold the result from the first del^2 computation.

         delsq_u(1:nVertLevels,edgeStart:edgeEnd) = 0.0

         do iEdge=edgeStart,edgeEnd
            cell1 = cellsOnEdge(1,iEdge)
            cell2 = cellsOnEdge(2,iEdge)
            vertex1 = verticesOnEdge(1,iEdge)
            vertex2 = verticesOnEdge(2,iEdge)
            r_dc = invDcEdge(iEdge)
            r_dv = min(invDvEdge(iEdge), 4*invDcEdge(iEdge))

!DIR$ IVDEP
            do k=1,nVertLevels

               ! Compute diffusion, computed as \nabla divergence - k \times \nabla vorticity
               !                    only valid for h_mom_eddy_visc4 == constant
              u_diffusion =   ( divergence(k,cell2)  - divergence(k,cell1) ) * r_dc  &
                              -( vorticity(k,vertex2) - vorticity(k,vertex1) ) * r_dv

               delsq_u(k,iEdge) = delsq_u(k,iEdge) + u_diffusion

               kdiffu = 0.5*(kdiff(k,cell1)+kdiff(k,cell2))

               ! include 2nd-orer diffusion here 
               tend_u_euler(k,iEdge) = tend_u_euler(k,iEdge) &
                                       + rho_edge(k,iEdge)* kdiffu * u_diffusion * meshScalingDel2(iEdge)

               if (config_tend) then
                  ru_tend_diff(k,iEdge) = rho_edge(k,iEdge)* kdiffu * u_diffusion * meshScalingDel2(iEdge) ! MW   
               end if             
            end do
         end do

         if (h_mom_eddy_visc4 > 0.0) then  ! 4th order mixing is active

!$OMP BARRIER

            do iVertex=vertexStart,vertexEnd
               delsq_vorticity(1:nVertLevels,iVertex) = 0.0
               do i=1,vertexDegree
                  iEdge = edgesOnVertex(i,iVertex)
                  edge_sign = invAreaTriangle(iVertex) * dcEdge(iEdge) * edgesOnVertex_sign(i,iVertex)
                  do k=1,nVertLevels
                     delsq_vorticity(k,iVertex) = delsq_vorticity(k,iVertex) + edge_sign * delsq_u(k,iEdge)
                  end do
               end do
            end do

            do iCell=cellStart,cellEnd
               delsq_divergence(1:nVertLevels,iCell) = 0.0
               r = invAreaCell(iCell)
               do i=1,nEdgesOnCell(iCell)
                  iEdge = edgesOnCell(i,iCell)
                  edge_sign = r * dvEdge(iEdge) * edgesOnCell_sign(i,iCell)
                  do k=1,nVertLevels
                     delsq_divergence(k,iCell) = delsq_divergence(k,iCell) + edge_sign * delsq_u(k,iEdge)
                  end do
               end do
            end do



         
!$OMP BARRIER

            do iEdge=edgeSolveStart,edgeSolveEnd
               cell1 = cellsOnEdge(1,iEdge)
               cell2 = cellsOnEdge(2,iEdge)
               vertex1 = verticesOnEdge(1,iEdge)
               vertex2 = verticesOnEdge(2,iEdge)

               u_mix_scale = meshScalingDel4(iEdge)*h_mom_eddy_visc4
               r_dc = u_mix_scale * config_del4u_div_factor * invDcEdge(iEdge)
               r_dv = u_mix_scale * min(invDvEdge(iEdge), 4*invDcEdge(iEdge))

!DIR$ IVDEP
               do k=1,nVertLevels

                  ! Compute diffusion, computed as \nabla divergence - k \times \nabla vorticity
                  !                    only valid for h_mom_eddy_visc4 == constant
                  !
                  ! Here, we scale the diffusion on the divergence part a factor of config_del4u_div_factor 
                  !    relative to the rotational part.  The stability constraint on the divergence component is much less
                  !    stringent than the rotational part, and this flexibility may be useful.
                  !
                  u_diffusion =  rho_edge(k,iEdge) *  ( ( delsq_divergence(k,cell2)  - delsq_divergence(k,cell1) ) * r_dc  &
                                                       -( delsq_vorticity(k,vertex2) - delsq_vorticity(k,vertex1) ) * r_dv )
                  tend_u_euler(k,iEdge) = tend_u_euler(k,iEdge) - u_diffusion

                  ! MW: for ITM
                  if (config_tend) then 
                     ru_tend_diff(k,iEdge) = ru_tend_diff(k,iEdge) - u_diffusion
                  end if

               end do
            end do
         
         end if ! 4th order mixing is active 

      !
      !  vertical mixing for u - 2nd order filter in physical (z) space
      !
         if ( v_mom_eddy_visc2 > 0.0 ) then

            if (config_mix_full) then  ! mix full state

               do iEdge=edgeSolveStart,edgeSolveEnd

                  cell1 = cellsOnEdge(1,iEdge)
                  cell2 = cellsOnEdge(2,iEdge)

                  do k=2,nVertLevels-1

                     z1 = 0.5*(zgrid(k-1,cell1)+zgrid(k-1,cell2))
                     z2 = 0.5*(zgrid(k  ,cell1)+zgrid(k  ,cell2))
                     z3 = 0.5*(zgrid(k+1,cell1)+zgrid(k+1,cell2))
                     z4 = 0.5*(zgrid(k+2,cell1)+zgrid(k+2,cell2))

                     zm = 0.5*(z1+z2)
                     z0 = 0.5*(z2+z3)
                     zp = 0.5*(z3+z4)

                     tend_u_euler(k,iEdge) = tend_u_euler(k,iEdge) + rho_edge(k,iEdge) * v_mom_eddy_visc2*(  &
                                        (u(k+1,iEdge)-u(k,iEdge))/(zp-z0)                      &
                                       -(u(k,iEdge)-u(k-1,iEdge))/(z0-zm) )/(0.5*(zp-zm))

                     ! MW for ITM          
                     if (config_tend) then 
                        ru_tend_diff(k,iEdge) = ru_tend_diff(k,iEdge) + rho_edge(k,iEdge) * v_mom_eddy_visc2 & 
                                       *( (u(k+1,iEdge) - u(k,iEdge))/(zp-z0)                                &
                                       -(u(k,iEdge)-u(k-1,iEdge))/(z0-zm) )/(0.5*(zp-zm))  
                     end if

                  end do
               end do

            else  ! idealized cases where we mix on the perturbation from the initial 1-D state

               do iEdge=edgeSolveStart,edgeSolveEnd

                  cell1 = cellsOnEdge(1,iEdge)
                  cell2 = cellsOnEdge(2,iEdge)

                  do k=1,nVertLevels
                     u_mix(k) = u(k,iEdge) - u_init(k) * cos( angleEdge(iEdge) ) &
                                           - v_init(k) * sin( angleEdge(iEdge) )
                  end do

                  do k=2,nVertLevels-1

                     z1 = 0.5*(zgrid(k-1,cell1)+zgrid(k-1,cell2))
                     z2 = 0.5*(zgrid(k  ,cell1)+zgrid(k  ,cell2))
                     z3 = 0.5*(zgrid(k+1,cell1)+zgrid(k+1,cell2))
                     z4 = 0.5*(zgrid(k+2,cell1)+zgrid(k+2,cell2))

                     zm = 0.5*(z1+z2)
                     z0 = 0.5*(z2+z3)
                     zp = 0.5*(z3+z4)

                     tend_u_euler(k,iEdge) = tend_u_euler(k,iEdge) + rho_edge(k,iEdge) * v_mom_eddy_visc2*(  &
                                        (u_mix(k+1)-u_mix(k  ))/(zp-z0)                      &
                                       -(u_mix(k  )-u_mix(k-1))/(z0-zm) )/(0.5*(zp-zm))

             
                     ! MW on ITM
                     if (config_tend) then 
                        ru_tend_diff(k,iEdge) = ru_tend_diff(k,iEdge)  + rho_edge(k,iEdge) * v_mom_eddy_visc2 & 
                                       *( (u_mix(k+1)-u_mix(k))/(zp-z0)                                       &
                                       -(u_mix(k)-u_mix(k-1))/(z0-zm) )/(0.5*(zp-zm)) 
                     end if
                               
                  end do
               end do

            end if  ! mix perturbation state

         end if  ! vertical mixing of horizontal momentum

      end if ! (rk_step 1 test for computing mixing terms)

!$OMP BARRIER

!  add in mixing and physics tendency for u

!  Rayleigh damping on u
      if (config_rayleigh_damp_u) then
         rayleigh_coef_inverse = 1.0 / ( real(config_number_rayleigh_damp_u_levels) &
                                         * (config_rayleigh_damp_u_timescale_days*seconds_per_day) )
         do k=nVertLevels-config_number_rayleigh_damp_u_levels+1,nVertLevels
            rayleigh_damp_coef(k) = real(k - (nVertLevels-config_number_rayleigh_damp_u_levels))*rayleigh_coef_inverse
         end do

         do iEdge=edgeSolveStart,edgeSolveEnd
!DIR$ IVDEP
            do k=nVertlevels-config_number_rayleigh_damp_u_levels+1,nVertLevels
               tend_u(k,iEdge) = tend_u(k,iEdge) - rho_edge(k,iEdge)*u(k,iEdge)*rayleigh_damp_coef(k)
            end do
         end do
      end if

      do iEdge=edgeSolveStart,edgeSolveEnd
!DIR$ IVDEP
         do k=1,nVertLevels

            ! MW on ITM: large time-step dynamic tendency 
            if (rk_step == 3 .and. config_tend) then
               ru_tend_smdiv(k,iEdge) = 0.
               ru_tend_dyn_large(k,iEdge) = dt_rk*(tend_u(k,iEdge) + tend_u_euler(k,iEdge) - ru_tend_diff(k,iEdge))
               ru_tend_diff(k,iEdge)      = dt_rk*ru_tend_diff(k,iEdge)
               ru_tend_physics(k,iEdge)   = dt_rk*tend_ru_physics(k,iEdge) ! will eventually use each scheme's tendency
            end if

!            tend_u(k,iEdge) = tend_u(k,iEdge) + tend_u_euler(k,iEdge)
            tend_u(k,iEdge) = tend_u(k,iEdge) + tend_u_euler(k,iEdge) + tend_ru_physics(k,iEdge)
         end do
      end do


!----------- rhs for w


      !
      !  horizontal advection for w
      !

      do iCell=cellSolveStart,cellSolveEnd    ! Technically updating fewer cells than before...
         tend_w(1:nVertLevels+1,iCell) = 0.0
         do i=1,nEdgesOnCell(iCell)
            iEdge = edgesOnCell(i,iCell)
            edge_sign = edgesOnCell_sign(i,iCell) * dvEdge(iEdge) * 0.5

            do k=2,nVertLevels
               ru_edge_w(k) = fzm(k)*ru(k,iEdge) + fzp(k)*ru(k-1,iEdge)
            end do

            flux_arr(1:nVertLevels) = 0.0

            ! flux_arr stores the value of w at the cell edge used in the horizontal transport

            do j=1,nAdvCellsForEdge(iEdge)
               iAdvCell = advCellsForEdge(j,iEdge)
               do k=2,nVertLevels
                  scalar_weight = adv_coefs(j,iEdge) + sign(1.0_RKIND,ru_edge_w(k)) * adv_coefs_3rd(j,iEdge)
                  flux_arr(k) = flux_arr(k) + scalar_weight * w(k,iAdvCell)
               end do
            end do

!DIR$ IVDEP
            do k=2,nVertLevels
               tend_w(k,iCell) = tend_w(k,iCell) - edgesOnCell_sign(i,iCell) * ru_edge_w(k)*flux_arr(k)
            end do

         end do
      end do

#ifdef CURVATURE
      do iCell = cellSolveStart, cellSolveEnd
!DIR$ IVDEP
         do k=2,nVertLevels
            tend_w(k,iCell) = tend_w(k,iCell) + (rho_zz(k,iCell)*fzm(k)+rho_zz(k-1,iCell)*fzp(k))*          &
                                      ( (fzm(k)*ur_cell(k,iCell)+fzp(k)*ur_cell(k-1,iCell))**2.             &
                                       +(fzm(k)*vr_cell(k,iCell)+fzp(k)*vr_cell(k-1,iCell))**2. )/r_earth   &
                                + 2.*omega*cos(latCell(iCell))                                              &
                                       *(fzm(k)*ur_cell(k,iCell)+fzp(k)*ur_cell(k-1,iCell))                 &
                                       *(rho_zz(k,iCell)*fzm(k)+rho_zz(k-1,iCell)*fzp(k))

         end do
      end do
#endif


      !
      !  horizontal mixing for w - we could combine this with advection directly (i.e. as a turbulent flux),
      !  but here we can also code in hyperdiffusion if we wish (2nd order at present)
      !

      if (rk_step == 1) then

!  !OMP BARRIER  why is this openmp barrier here???

         ! del^4 horizontal filter.  We compute this as del^2 ( del^2 (u) ).
         !
         ! First, storage to hold the result from the first del^2 computation.
         !  we copied code from the theta mixing, hence the theta* names.


         delsq_w(1:nVertLevels,cellStart:cellEnd) = 0.0

         do iCell=cellStart,cellEnd
            tend_w_euler(1:nVertLevels+1,iCell) = 0.0
            r_areaCell = invAreaCell(iCell)
            do i=1,nEdgesOnCell(iCell)
               iEdge = edgesOnCell(i,iCell)

               edge_sign = 0.5 * r_areaCell*edgesOnCell_sign(i,iCell) * dvEdge(iEdge) * invDcEdge(iEdge)

               cell1 = cellsOnEdge(1,iEdge)
               cell2 = cellsOnEdge(2,iEdge)

!DIR$ IVDEP
              do k=2,nVertLevels

                  w_turb_flux =  edge_sign*(rho_edge(k,iEdge)+rho_edge(k-1,iEdge))*(w(k,cell2) - w(k,cell1))
                  delsq_w(k,iCell) = delsq_w(k,iCell) + w_turb_flux
                  w_turb_flux = w_turb_flux * meshScalingDel2(iEdge) * 0.25 * &
                                  (kdiff(k,cell1)+kdiff(k,cell2)+kdiff(k-1,cell1)+kdiff(k-1,cell2))
                  tend_w_euler(k,iCell) = tend_w_euler(k,iCell) + w_turb_flux

                  ! MW: added for PV friction term
                  if (config_pv_tend) then 
                     rw_tend_diff(k,iCell) = rw_tend_diff(k,iCell) + w_turb_flux  
                  end if
                     
               end do
            end do
         end do

!$OMP BARRIER

         if (h_mom_eddy_visc4 > 0.0) then  ! 4th order mixing is active

            do iCell=cellSolveStart,cellSolveEnd    ! Technically updating fewer cells than before...
               r_areaCell = h_mom_eddy_visc4 * invAreaCell(iCell)
               do i=1,nEdgesOnCell(iCell)
                  iEdge = edgesOnCell(i,iCell)
                  cell1 = cellsOnEdge(1,iEdge)
                  cell2 = cellsOnEdge(2,iEdge)

                  edge_sign = meshScalingDel4(iEdge)*r_areaCell*dvEdge(iEdge)*edgesOnCell_sign(i,iCell) * invDcEdge(iEdge)

                  do k=2,nVertLevels
                     tend_w_euler(k,iCell) = tend_w_euler(k,iCell) - edge_sign * (delsq_w(k,cell2) - delsq_w(k,cell1))

                     ! MW: added for PV friction term
                     if (config_pv_tend) then 
                        rw_tend_diff(k,iCell) = rw_tend_diff(k,iCell) - edge_sign * (delsq_w(k,cell2) - delsq_w(k,cell1)) 
                     end if

                  end do
           
               end do
            end do

         end if ! 4th order mixing is active 

      end if ! horizontal mixing for w computed in first rk_step

! Note for OpenMP parallelization: We could avoid allocating the delsq_w scratch
!   array, and just use the delsq_theta array as was previously done; however,
!   particularly when oversubscribing cores with threads, there is the risk that
!   some threads may reach code further below that re-uses the delsq_theta array, 
!   in which case we would need a barrier somewhere between here and that code 
!   below to ensure correct behavior.

      !
      !  vertical advection, pressure gradient and buoyancy for w
      !

      do iCell=cellSolveStart,cellSolveEnd

         wdwz(1) = 0.0

         k = 2
         wdwz(k) =  0.25*(rw(k,icell)+rw(k-1,iCell))*(w(k,iCell)+w(k-1,iCell))
         do k=3,nVertLevels-1
            wdwz(k) = flux3( w(k-2,iCell),w(k-1,iCell),w(k,iCell),w(k+1,iCell),0.5*(rw(k,iCell)+rw(k-1,iCell)), 1.0_RKIND )
         end do
         k = nVertLevels
         wdwz(k) =  0.25*(rw(k,icell)+rw(k-1,iCell))*(w(k,iCell)+w(k-1,iCell))

         wdwz(nVertLevels+1) = 0.0

      !  Note: next we are also dividing through by the cell area after the horizontal flux divergence

!DIR$ IVDEP
         do k=2,nVertLevels
            tend_w(k,iCell) = tend_w(k,iCell) * invAreaCell(iCell) -rdzu(k)*(wdwz(k+1)-wdwz(k))
         end do

         if(rk_step == 1) then
!DIR$ IVDEP
            do k=2,nVertLevels
              tend_w_euler(k,iCell) = tend_w_euler(k,iCell) - cqw(k,iCell)*(   &
                                           rdzu(k)*(pp(k,iCell)-pp(k-1,iCell)) &
                                         - (fzm(k)*dpdz(k,iCell) + fzp(k)*dpdz(k-1,iCell)) )  ! dpdz is the buoyancy term here.
            end do
          end if

      end do

      if (rk_step == 1) then

         if ( v_mom_eddy_visc2 > 0.0 ) then

            do iCell=cellSolveStart,cellSolveEnd
!DIR$ IVDEP
               do k=2,nVertLevels
                  tend_w_euler(k,iCell) = tend_w_euler(k,iCell) + v_mom_eddy_visc2*0.5*(rho_zz(k,iCell)+rho_zz(k-1,iCell))*(  &
                                           (w(k+1,iCell)-w(k  ,iCell))*rdzw(k)                              &
                                          -(w(k  ,iCell)-w(k-1,iCell))*rdzw(k-1) )*rdzu(k)

                  ! MW: added for PV tendencies                
                  if (config_pv_tend) then 
                     rw_tend_diff(k,iCell) = rw_tend_diff(k,iCell) + v_mom_eddy_visc2*0.5*(rho_zz(k,iCell)+rho_zz(k-1,iCell)) &
                                          *( (w(k+1,iCell)-w(k,iCell))*rdzw(k)                                                &
                                          -(w(k,iCell)-w(k-1,iCell))*rdzw(k-1) )*rdzu(k)
                  end if

               end do
            end do

         end if

      end if ! mixing term computed first rk_step

      ! add in mixing terms for w

      do iCell = cellSolveStart,cellSolveEnd
!DIR$ IVDEP
         do k=2,nVertLevels

            ! MW added for PV tendencies 
            if ( rk_step == 3 .and. config_pv_tend ) then
               rw_tend_dyn_large(k,iCell) = dt_rk*(tend_w(k,iCell) + tend_w_euler(k,iCell) - rw_tend_diff(k,iCell))
               rw_tend_diff(k,iCell) = dt_rk*rw_tend_diff(k,iCell)
            end if        

            tend_w(k,iCell) = tend_w(k,iCell) + tend_w_euler(k,iCell)
         end do
      end do

!----------- rhs for theta

      !
      !  horizontal advection for theta
      !

      do iCell=cellSolveStart,cellSolveEnd    ! Technically updating fewer cells than before...
         tend_theta(1:nVertLevels,iCell) = 0.0
         do i=1,nEdgesOnCell(iCell)
            iEdge = edgesOnCell(i,iCell)

            flux_arr(1:nVertLevels) = 0.0

            do j=1,nAdvCellsForEdge(iEdge)
               iAdvCell = advCellsForEdge(j,iEdge)
               do k=1,nVertLevels
                  scalar_weight = adv_coefs(j,iEdge) + sign(1.0_RKIND,ru(k,iEdge))*adv_coefs_3rd(j,iEdge)
                  flux_arr(k) = flux_arr(k) + scalar_weight* theta_m(k,iAdvCell)
               end do
            end do

!DIR$ IVDEP
            do k=1,nVertLevels
               tend_theta(k,iCell) = tend_theta(k,iCell) - edgesOnCell_sign(i,iCell) * ru(k,iEdge) * flux_arr(k)
            end do

         end do
      end do

!  addition to pick up perturbation flux for rtheta_pp equation

      if(rk_step > 1) then
        do iCell=cellSolveStart,cellSolveEnd
          do i=1,nEdgesOnCell(iCell) 
            iEdge = edgesOnCell(i,iCell)
            cell1 = cellsOnEdge(1,iEdge)
            cell2 = cellsOnEdge(2,iEdge)
!DIR$ IVDEP
            do k=1,nVertLevels
               flux = edgesOnCell_sign(i,iCell)*dvEdge(iEdge)*(ru_save(k,iEdge)-ru(k,iEdge))*0.5*(theta_m_save(k,cell2)+theta_m_save(k,cell1))
               tend_theta(k,iCell) = tend_theta(k,iCell)-flux  ! division by areaCell picked up down below
            end do
          end do
        end do
      end if

      !
      !  horizontal mixing for theta_m - we could combine this with advection directly (i.e. as a turbulent flux),
      !  but here we can also code in hyperdiffusion if we wish (2nd order at present)
      !

      if (rk_step == 1) then

         delsq_theta(1:nVertLevels,cellStart:cellEnd) = 0.0

         do iCell=cellStart,cellEnd
            tend_theta_euler(1:nVertLevels,iCell) = 0.0
            r_areaCell = invAreaCell(iCell)
            do i=1,nEdgesOnCell(iCell)
               iEdge = edgesOnCell(i,iCell)
               edge_sign = r_areaCell*edgesOnCell_sign(i,iCell) * dvEdge(iEdge) * invDcEdge(iEdge)
               pr_scale = prandtl_inv * meshScalingDel2(iEdge)
               cell1 = cellsOnEdge(1,iEdge)
               cell2 = cellsOnEdge(2,iEdge)
!DIR$ IVDEP
               do k=1,nVertLevels

!  we are computing the Smagorinsky filter at more points than needed here so as to pick up the delsq_theta for 4th order filter below

                  theta_turb_flux = edge_sign*(theta_m(k,cell2) - theta_m(k,cell1))*rho_edge(k,iEdge)
                  delsq_theta(k,iCell) = delsq_theta(k,iCell) + theta_turb_flux
                  theta_turb_flux = theta_turb_flux*0.5*(kdiff(k,cell1)+kdiff(k,cell2)) * pr_scale
                  tend_theta_euler(k,iCell) = tend_theta_euler(k,iCell) + theta_turb_flux

               end do
            end do
          end do

!$OMP BARRIER
            
         if (h_theta_eddy_visc4 > 0.0) then  ! 4th order mixing is active

            do iCell=cellSolveStart,cellSolveEnd    ! Technically updating fewer cells than before...
               r_areaCell = h_theta_eddy_visc4 * prandtl_inv * invAreaCell(iCell)
               do i=1,nEdgesOnCell(iCell)

                  iEdge = edgesOnCell(i,iCell)
                  edge_sign = meshScalingDel4(iEdge)*r_areaCell*dvEdge(iEdge)*edgesOnCell_sign(i,iCell)*invDcEdge(iEdge)

                  cell1 = cellsOnEdge(1,iEdge)
                  cell2 = cellsOnEdge(2,iEdge)

                  do k=1,nVertLevels
                     tend_theta_euler(k,iCell) = tend_theta_euler(k,iCell) - edge_sign*(delsq_theta(k,cell2) - delsq_theta(k,cell1))
                  end do
               end do
            end do

         end if ! 4th order mixing is active 

      end if ! theta mixing calculated first rk_step

      !
      !  vertical advection plus diabatic term
      !  Note: we are also dividing through by the cell area after the horizontal flux divergence
      !
      do iCell = cellSolveStart,cellSolveEnd

         wdtz(1) = 0.0

         k = 2
         wdtz(k) =  rw(k,icell)*(fzm(k)*theta_m(k,iCell)+fzp(k)*theta_m(k-1,iCell))  
         wdtz(k) =  wdtz(k)+(rw_save(k,icell)-rw(k,icell))*(fzm(k)*theta_m_save(k,iCell)+fzp(k)*theta_m_save(k-1,iCell))
         do k=3,nVertLevels-1
            wdtz(k) = flux3( theta_m(k-2,iCell),theta_m(k-1,iCell),theta_m(k,iCell),theta_m(k+1,iCell), rw(k,iCell), coef_3rd_order )
            wdtz(k) =  wdtz(k) + (rw_save(k,icell)-rw(k,iCell))*(fzm(k)*theta_m_save(k,iCell)+fzp(k)*theta_m_save(k-1,iCell))  ! rtheta_pp redefinition
         end do
         k = nVertLevels
         wdtz(k) =  rw_save(k,icell)*(fzm(k)*theta_m(k,iCell)+fzp(k)*theta_m(k-1,iCell))  ! rtheta_pp redefinition

         wdtz(nVertLevels+1) = 0.0

!DIR$ IVDEP
         do k=1,nVertLevels
            tend_theta(k,iCell) = tend_theta(k,iCell)*invAreaCell(iCell) -rdzw(k)*(wdtz(k+1)-wdtz(k))
            rthdynten(k,iCell)  = (tend_theta(k,iCell)-tend_rho(k,iCell)*theta_m(k,iCell))/rho_zz(k,iCell)

            ! MW on ITM: note tendency saved BEFORE new rt_diabatic_tend is added  
            if (config_tend) then
               rth_tend_dyn_large(k,iCell) = dt_rk*tend_theta(k,iCell)        
            end if
               
            tend_theta(k,iCell) = tend_theta(k,iCell) + rho_zz(k,iCell)*rt_diabatic_tend(k,iCell)
         end do
      end do

      !
      !  vertical mixing for theta - 2nd order 
      !

      if (rk_step == 1) then

         if ( v_theta_eddy_visc2 > 0.0 ) then  ! vertical mixing for theta_m

            if (config_mix_full) then

               do iCell = cellSolveStart,cellSolveEnd
                  do k=2,nVertLevels-1
                     z1 = zgrid(k-1,iCell)
                     z2 = zgrid(k  ,iCell)
                     z3 = zgrid(k+1,iCell)
                     z4 = zgrid(k+2,iCell)

                     zm = 0.5*(z1+z2)
                     z0 = 0.5*(z2+z3)
                     zp = 0.5*(z3+z4)

                     tend_theta_euler(k,iCell) = tend_theta_euler(k,iCell) + v_theta_eddy_visc2*prandtl_inv*rho_zz(k,iCell)*(&
                                              (theta_m(k+1,iCell)-theta_m(k  ,iCell))/(zp-z0)                 &
                                             -(theta_m(k  ,iCell)-theta_m(k-1,iCell))/(z0-zm) )/(0.5*(zp-zm))
                  end do
               end do

         else  ! idealized cases where we mix on the perturbation from the initial 1-D state

               do iCell = cellSolveStart,cellSolveEnd
                  do k=2,nVertLevels-1
                     z1 = zgrid(k-1,iCell)
                     z2 = zgrid(k  ,iCell)
                     z3 = zgrid(k+1,iCell)
                     z4 = zgrid(k+2,iCell)

                     zm = 0.5*(z1+z2)
                     z0 = 0.5*(z2+z3)
                     zp = 0.5*(z3+z4)

                     tend_theta_euler(k,iCell) = tend_theta_euler(k,iCell) + v_theta_eddy_visc2*prandtl_inv*rho_zz(k,iCell)*(&
                                              ((theta_m(k+1,iCell)-t_init(k+1,iCell))-(theta_m(k  ,iCell)-t_init(k,iCell)))/(zp-z0)      &
                                             -((theta_m(k  ,iCell)-t_init(k,iCell))-(theta_m(k-1,iCell)-t_init(k-1,iCell)))/(z0-zm) )/(0.5*(zp-zm))
                  end do
               end do

            end if

         end if

      end if ! compute vertical theta mixing on first rk_step

      do iCell = cellSolveStart,cellSolveEnd
!DIR$ IVDEP
         do k=1,nVertLevels
!            tend_theta(k,iCell) = tend_theta(k,iCell) + tend_theta_euler(k,iCell)
            tend_theta(k,iCell) = tend_theta(k,iCell) + tend_theta_euler(k,iCell) + tend_rtheta_physics(k,iCell)

            ! MW on ITM
            if (rk_step == 3 .and. config_tend) then
                 rth_tend_diff(k,iCell)    = dt_rk*tend_theta_euler(k,iCell)
                 rth_tend_physics(k,iCell) = dt_rk*tend_rtheta_physics(k,iCell)
            end if
   
         end do
      end do

   end subroutine atm_compute_dyn_tend_work


   subroutine atm_compute_solve_diagnostics(dt, state, time_lev, diag, mesh, configs, &
                                            cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd, &
                                            rk_step )
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Compute diagnostic fields used in the tendency computations
   !
   ! Input: state (s), grid - grid metadata
   !
   ! Output: diag - computed diagnostics
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 

      implicit none

      real (kind=RKIND), intent(in) :: dt
      type (mpas_pool_type), intent(inout) :: state
      integer, intent(in) :: time_lev                   ! which time level of state to use
      integer, intent(in), optional :: rk_step          ! which rk_step
      type (mpas_pool_type), intent(inout) :: diag
      type (mpas_pool_type), intent(in) :: mesh
      type (mpas_pool_type), intent(in) :: configs
      integer, intent(in) :: cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd


      integer, pointer :: nCells, nEdges, nVertices, nVertLevels, vertexDegree
      real (kind=RKIND), dimension(:), pointer :: fVertex, fEdge, invAreaTriangle, invAreaCell
      real (kind=RKIND), dimension(:), pointer :: dvEdge, dcEdge, invDvEdge, invDcEdge
      real (kind=RKIND), dimension(:,:), pointer :: weightsOnEdge, kiteAreasOnVertex, h_edge, h, u, v, &
                                                    vorticity, ke, pv_edge, pv_vertex, pv_cell, gradPVn, gradPVt, &
                                                    divergence
      integer, dimension(:,:), pointer :: cellsOnEdge, cellsOnVertex, verticesOnEdge, edgesOnCell, edgesOnEdge, edgesOnVertex, &
                                          kiteForCell, verticesOnCell
      real (kind=RKIND), dimension(:,:), pointer :: edgesOnVertex_sign, edgesOnCell_sign
      integer, dimension(:), pointer :: nEdgesOnCell, nEdgesOnEdge

      real (kind=RKIND), pointer :: config_apvm_upwinding


      call mpas_pool_get_config(configs, 'config_apvm_upwinding', config_apvm_upwinding)

      call mpas_pool_get_array(state, 'rho_zz', h, time_lev)
      call mpas_pool_get_array(state, 'u', u, time_lev)

      call mpas_pool_get_array(diag, 'v', v)
      call mpas_pool_get_array(diag, 'rho_edge', h_edge)
      call mpas_pool_get_array(diag, 'vorticity', vorticity)
      call mpas_pool_get_array(diag, 'divergence', divergence)
      call mpas_pool_get_array(diag, 'ke', ke)
      call mpas_pool_get_array(diag, 'pv_edge', pv_edge)
      call mpas_pool_get_array(diag, 'pv_vertex', pv_vertex)
      call mpas_pool_get_array(diag, 'pv_cell', pv_cell)
      call mpas_pool_get_array(diag, 'gradPVn', gradPVn)
      call mpas_pool_get_array(diag, 'gradPVt', gradPVt)

      call mpas_pool_get_array(mesh, 'weightsOnEdge', weightsOnEdge)
      call mpas_pool_get_array(mesh, 'kiteAreasOnVertex', kiteAreasOnVertex)
      call mpas_pool_get_array(mesh, 'cellsOnEdge', cellsOnEdge)
      call mpas_pool_get_array(mesh, 'cellsOnVertex', cellsOnVertex)
      call mpas_pool_get_array(mesh, 'verticesOnEdge', verticesOnEdge)
      call mpas_pool_get_array(mesh, 'verticesOnCell', verticesOnCell)
      call mpas_pool_get_array(mesh, 'nEdgesOnCell', nEdgesOnCell)
      call mpas_pool_get_array(mesh, 'edgesOnCell', edgesOnCell)
      call mpas_pool_get_array(mesh, 'nEdgesOnEdge', nEdgesOnEdge)
      call mpas_pool_get_array(mesh, 'edgesOnEdge', edgesOnEdge)
      call mpas_pool_get_array(mesh, 'edgesOnVertex', edgesOnVertex)
      call mpas_pool_get_array(mesh, 'edgesOnVertex_sign', edgesOnVertex_sign)
      call mpas_pool_get_array(mesh, 'edgesOnCell_sign', edgesOnCell_sign)
      call mpas_pool_get_array(mesh, 'kiteForCell', kiteForCell)
      call mpas_pool_get_array(mesh, 'dcEdge', dcEdge)
      call mpas_pool_get_array(mesh, 'dvEdge', dvEdge)
      call mpas_pool_get_array(mesh, 'invDcEdge', invDcEdge)
      call mpas_pool_get_array(mesh, 'invDvEdge', invDvEdge)
      call mpas_pool_get_array(mesh, 'invAreaCell', invAreaCell)
      call mpas_pool_get_array(mesh, 'invAreaTriangle', invAreaTriangle)
      call mpas_pool_get_array(mesh, 'fVertex', fVertex)
      call mpas_pool_get_array(mesh, 'fEdge', fEdge)

      call mpas_pool_get_dimension(mesh, 'nCells', nCells)
      call mpas_pool_get_dimension(mesh, 'nEdges', nEdges)
      call mpas_pool_get_dimension(mesh, 'nVertices', nVertices)
      call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)
      call mpas_pool_get_dimension(mesh, 'vertexDegree', vertexDegree)

      call atm_compute_solve_diagnostics_work(nCells, nEdges, nVertices, &
               vertexDegree, dt, config_apvm_upwinding, &
               fVertex, fEdge, invAreaTriangle, invAreaCell, dvEdge, dcEdge, invDvEdge, invDcEdge, &
               weightsOnEdge, kiteAreasOnVertex, h_edge, h, u, v, vorticity, ke, pv_edge, pv_vertex, pv_cell, &
               gradPVn, gradPVt, divergence, cellsOnEdge, cellsOnVertex, verticesOnEdge, edgesOnCell, edgesOnEdge, &
               edgesOnVertex, kiteForCell, verticesOnCell, edgesOnVertex_sign, edgesOnCell_sign, nEdgesOnCell, nEdgesOnEdge, &
               cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd, &
               rk_step)

   end subroutine atm_compute_solve_diagnostics


   subroutine atm_compute_solve_diagnostics_work(nCells, nEdges, nVertices, &
            vertexDegree, dt, config_apvm_upwinding, &
            fVertex, fEdge, invAreaTriangle, invAreaCell, dvEdge, dcEdge, invDvEdge, invDcEdge, &
            weightsOnEdge, kiteAreasOnVertex, h_edge, h, u, v, vorticity, ke, pv_edge, pv_vertex, pv_cell, &
            gradPVn, gradPVt, divergence, cellsOnEdge, cellsOnVertex, verticesOnEdge, edgesOnCell, edgesOnEdge, &
            edgesOnVertex, kiteForCell, verticesOnCell, edgesOnVertex_sign, edgesOnCell_sign, nEdgesOnCell, nEdgesOnEdge, &
            cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd, &
            rk_step)

      use mpas_atm_dimensions

      implicit none

      !
      ! Dummy arguments
      !
      integer, intent(in) :: nCells, nEdges, nVertices, vertexDegree
      real (kind=RKIND), intent(in) :: dt, config_apvm_upwinding
      real (kind=RKIND), dimension(nVertices+1) :: fVertex
      real (kind=RKIND), dimension(nEdges+1) :: fEdge
      real (kind=RKIND), dimension(nVertices+1) :: invAreaTriangle
      real (kind=RKIND), dimension(nCells+1) :: invAreaCell
      real (kind=RKIND), dimension(nEdges+1) :: dvEdge
      real (kind=RKIND), dimension(nEdges+1) :: dcEdge
      real (kind=RKIND), dimension(nEdges+1) :: invDvEdge
      real (kind=RKIND), dimension(nEdges+1) :: invDcEdge
      real (kind=RKIND), dimension(maxEdges2,nEdges+1) :: weightsOnEdge
      real (kind=RKIND), dimension(3,nVertices+1) :: kiteAreasOnVertex
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: h_edge
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: h
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: u
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: v
      real (kind=RKIND), dimension(nVertLevels,nVertices+1) :: vorticity
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: ke
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: pv_edge
      real (kind=RKIND), dimension(nVertLevels,nVertices+1) :: pv_vertex
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: pv_cell
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: gradPVn
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: gradPVt
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: divergence
      integer, dimension(2,nEdges+1) :: cellsOnEdge
      integer, dimension(3,nVertices+1) :: cellsOnVertex
      integer, dimension(2,nEdges+1) :: verticesOnEdge
      integer, dimension(maxEdges,nCells+1) :: edgesOnCell
      integer, dimension(maxEdges2,nEdges+1) :: edgesOnEdge
      integer, dimension(3,nVertices+1) :: edgesOnVertex
      integer, dimension(maxEdges,nCells+1) :: kiteForCell
      integer, dimension(maxEdges,nCells+1) :: verticesOnCell
      real (kind=RKIND), dimension(3,nVertices+1) :: edgesOnVertex_sign
      real (kind=RKIND), dimension(maxEdges,nCells+1) :: edgesOnCell_sign
      integer, dimension(nCells+1) :: nEdgesOnCell
      integer, dimension(nEdges+1) :: nEdgesOnEdge

      integer, intent(in) :: cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd

      integer, intent(in), optional :: rk_step

      !
      ! Local variables
      !
      integer :: iEdge, iCell, iVertex, k, cell1, cell2, eoe, i, j
      real (kind=RKIND) :: h_vertex, r, s
      real (kind=RKIND) :: r1, r2

      logical, parameter :: hollingsworth=.true.
      real (kind=RKIND) :: ke_fact, efac
      logical :: reconstruct_v


      !
      ! Compute height on cell edges at velocity locations
      !
      do iEdge=edgeStart,edgeEnd
         cell1 = cellsOnEdge(1,iEdge)
         cell2 = cellsOnEdge(2,iEdge)
!DIR$ IVDEP
         do k=1,nVertLevels
            h_edge(k,iEdge) = 0.5 * (h(k,cell1) + h(k,cell2))
         end do

!  the first openmp barrier below is set so that ke_edge is computed
!  it would be good to move this somewhere else?

         efac = dcEdge(iEdge)*dvEdge(iEdge)
         do k=1,nVertLevels
            ke_edge(k,iEdge) = efac*u(k,iEdge)**2
         end do

      end do

      !
      ! Compute circulation and relative vorticity at each vertex
      !
      do iVertex=vertexStart,vertexEnd
         vorticity(1:nVertLevels,iVertex) = 0.0
         do i=1,vertexDegree
            iEdge = edgesOnVertex(i,iVertex)
            s = edgesOnVertex_sign(i,iVertex) * dcEdge(iEdge)
!DIR$ IVDEP
            do k=1,nVertLevels
               vorticity(k,iVertex) = vorticity(k,iVertex) + s * u(k,iEdge)
            end do
         end do
!DIR$ IVDEP
         do k=1,nVertLevels
            vorticity(k,iVertex) = vorticity(k,iVertex) * invAreaTriangle(iVertex)
         end do
      end do


      !
      ! Compute the divergence at each cell center
      !
      do iCell=cellStart,cellEnd
         divergence(1:nVertLevels,iCell) = 0.0
         do i=1,nEdgesOnCell(iCell)
            iEdge = edgesOnCell(i,iCell)
            s = edgesOnCell_sign(i,iCell) * dvEdge(iEdge)
!DIR$ IVDEP
            do k=1,nVertLevels
              divergence(k,iCell) = divergence(k,iCell) + s * u(k,iEdge)
            end do
         end do
         r = invAreaCell(iCell)
         do k = 1,nVertLevels
            divergence(k,iCell) = divergence(k,iCell) * r
         end do
      end do


!$OMP BARRIER

      !
      ! Compute kinetic energy in each cell (Ringler et al JCP 2009)
      !
      ! Replace 2.0 with 2 in exponentiation to avoid outside chance that
      ! compiler will actually allow "float raised to float" operation
      do iCell=cellStart,cellEnd
         ke(1:nVertLevels,iCell) = 0.0
         do i=1,nEdgesOnCell(iCell)
            iEdge = edgesOnCell(i,iCell)
            do k=1,nVertLevels
!               ke(k,iCell) = ke(k,iCell) + 0.25 * dcEdge(iEdge) * dvEdge(iEdge) * u(k,iEdge)**2
               ke(k,iCell) = ke(k,iCell) + 0.25 * ke_edge(k,iEdge)
            end do
         end do
!DIR$ IVDEP
         do k=1,nVertLevels
            ke(k,iCell) = ke(k,iCell) * invAreaCell(iCell)
         end do
      end do


      if (hollingsworth) then

         ! Compute ke at cell vertices - AG's new KE construction, part 1
         ! *** approximation here because we don't have inner triangle areas
         !

         ! Replace 2.0 with 2 in exponentiation to avoid outside chance that
         ! compiler will actually allow "float raised to float" operation
         do iVertex=vertexStart,vertexEnd
            r = 0.25 * invAreaTriangle(iVertex) 
            do k=1,nVertLevels

!               ke_vertex(k,iVertex) = (  dcEdge(EdgesOnVertex(1,iVertex))*dvEdge(EdgesOnVertex(1,iVertex))*u(k,EdgesOnVertex(1,iVertex))**2  &
!                                        +dcEdge(EdgesOnVertex(2,iVertex))*dvEdge(EdgesOnVertex(2,iVertex))*u(k,EdgesOnVertex(2,iVertex))**2  &
!                                        +dcEdge(EdgesOnVertex(3,iVertex))*dvEdge(EdgesOnVertex(3,iVertex))*u(k,EdgesOnVertex(3,iVertex))**2  &
!                                      ) * r

               ke_vertex(k,iVertex) = (  ke_edge(k,EdgesOnVertex(1,iVertex))+ke_edge(k,EdgesOnVertex(2,iVertex))+ke_edge(k,EdgesOnVertex(3,iVertex)) )*r

            end do
         end do

!$OMP BARRIER

         ! adjust ke at cell vertices - AG's new KE construction, part 2
         !

         ke_fact = 1.0 - .375

         do iCell=cellStart,cellEnd
            do k=1,nVertLevels
               ke(k,iCell) = ke_fact * ke(k,iCell)
            end do
         end do


         do iCell=cellStart,cellEnd
            r = invAreaCell(iCell)
            do i=1,nEdgesOnCell(iCell)
               iVertex = verticesOnCell(i,iCell)
               j = kiteForCell(i,iCell)
!DIR$ IVDEP
               do k = 1,nVertLevels
                  ke(k,iCell) = ke(k,iCell) + (1.-ke_fact)*kiteAreasOnVertex(j,iVertex) * ke_vertex(k,iVertex) * r
               end do
            end do
         end do

      end if

      !
      ! Compute v (tangential) velocities following Thuburn et al JCP 2009
      ! The tangential velocity is only used to compute the Smagorinsky coefficient

      reconstruct_v = .true.
      if(present(rk_step)) then
        if(rk_step /= 3) reconstruct_v = .false.
      end if

      if (reconstruct_v) then
        do iEdge = edgeStart,edgeEnd
          v(1:nVertLevels,iEdge) = 0.0
          do i=1,nEdgesOnEdge(iEdge)
            eoe = edgesOnEdge(i,iEdge)
!DIR$ IVDEP
            do k = 1,nVertLevels
              v(k,iEdge) = v(k,iEdge) + weightsOnEdge(i,iEdge) * u(k, eoe)
            end do
          end do
        end do
      end if

      !
      ! Compute height at vertices, pv at vertices, and average pv to edge locations
      !  ( this computes pv_vertex at all vertices bounding real cells )
      !
      ! Avoid dividing h_vertex by areaTriangle and move areaTriangle into
      ! numerator for the pv_vertex calculation
      do iVertex = vertexStart,vertexEnd
!DIR$ IVDEP
         do k=1,nVertLevels
!
! the following commented code is for the PV conserving shallow water solver.  
!            h_vertex = 0.0
!            do i=1,vertexDegree
!               h_vertex = h_vertex + h(k,cellsOnVertex(i,iVertex)) * kiteAreasOnVertex(i,iVertex)
!            end do
!            pv_vertex(k,iVertex) = (fVertex(iVertex) + vorticity(k,iVertex)) * areaTriangle(iVertex) / h_vertex
            pv_vertex(k,iVertex) = (fVertex(iVertex) + vorticity(k,iVertex))
         end do
      end do

!$OMP BARRIER

      !
      ! Compute pv at the edges
      !   ( this computes pv_edge at all edges bounding real cells )
      !
      do iEdge = edgeStart,edgeEnd
!DIR$ IVDEP
         do k=1,nVertLevels
            pv_edge(k,iEdge) =  0.5 * (pv_vertex(k,verticesOnEdge(1,iEdge)) + pv_vertex(k,verticesOnEdge(2,iEdge)))
         end do
      end do

      if (config_apvm_upwinding > 0.0) then

      !
      ! Compute pv at cell centers
      !    ( this computes pv_cell for all real cells )
      !  only needed for APVM upwinding
      !
      do iCell=cellStart,cellEnd
         pv_cell(1:nVertLevels,iCell) = 0.0
         r = invAreaCell(iCell)
         do i=1,nEdgesOnCell(iCell)
            iVertex = verticesOnCell(i,iCell)
            j = kiteForCell(i,iCell)
!DIR$ IVDEP
            do k = 1,nVertLevels
               pv_cell(k,iCell) = pv_cell(k,iCell) + kiteAreasOnVertex(j,iVertex) * pv_vertex(k,iVertex) * r
            end do
         end do
      end do


!$OMP BARRIER

         !
         ! Modify PV edge with upstream bias. 
         !
         ! Compute gradient of PV in the tangent direction
         !   ( this computes gradPVt at all edges bounding real cells )
         !
         ! Compute gradient of PV in normal direction
         !   (tdr: 2009-10-02: this is not correct because the pv_cell in the halo is not correct)
         !
         ! Modify PV edge with upstream bias.
         !
         ! Merged loops for calculating gradPVt, gradPVn and pv_edge
         ! Also precomputed inverses of dvEdge and dcEdge to avoid repeated divisions
         !
         r = config_apvm_upwinding * dt
         do iEdge = edgeStart,edgeEnd
            r1 = 1.0_RKIND * invDvEdge(iEdge)
            r2 = 1.0_RKIND * invDcEdge(iEdge)
!DIR$ IVDEP
            do k = 1,nVertLevels
               gradPVt(k,iEdge) = (pv_vertex(k,verticesOnEdge(2,iEdge)) - pv_vertex(k,verticesOnEdge(1,iEdge))) * r1
               gradPVn(k,iEdge) = (pv_cell(k,cellsOnEdge(2,iEdge)) - pv_cell(k,cellsOnEdge(1,iEdge))) * r2
               pv_edge(k,iEdge) = pv_edge(k,iEdge) - r * (v(k,iEdge) * gradPVt(k,iEdge) + u(k,iEdge) * gradPVn(k,iEdge))
            end do
         end do

      end if  ! apvm upwinding

   end subroutine atm_compute_solve_diagnostics_work


   subroutine atm_init_coupled_diagnostics(state, time_lev, diag, mesh, configs, &
                                       cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd, &
                                       cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd)

      implicit none
   
      type (mpas_pool_type), intent(inout) :: state
      integer, intent(in) :: time_lev                    ! which time level to use from state
      type (mpas_pool_type), intent(inout) :: diag
      type (mpas_pool_type), intent(inout) :: mesh
      type (mpas_pool_type), intent(in) :: configs
      integer, intent(in) :: cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd
      integer, intent(in) :: cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd

      integer :: i, k, iCell, iEdge, cell1, cell2
      integer, pointer :: nCells, nEdges, nVertLevels
      integer, pointer :: index_qv
      real (kind=RKIND) :: p0, rcv, flux
      integer, dimension(:), pointer :: nEdgesOnCell
      integer, dimension(:,:), pointer :: cellsOnEdge, edgesOnCell
      real (kind=RKIND), dimension(:,:), pointer :: edgesOnCell_sign
      real (kind=RKIND), dimension(:,:), pointer :: theta_m
      real (kind=RKIND), dimension(:,:), pointer :: theta
      real (kind=RKIND), dimension(:,:), pointer :: rho_zz
      real (kind=RKIND), dimension(:,:), pointer :: rho
      real (kind=RKIND), dimension(:,:), pointer :: rho_p
      real (kind=RKIND), dimension(:,:), pointer :: rho_base
      real (kind=RKIND), dimension(:,:), pointer :: rtheta_base
      real (kind=RKIND), dimension(:,:), pointer :: theta_base
      real (kind=RKIND), dimension(:,:), pointer :: rtheta_p
      real (kind=RKIND), dimension(:,:), pointer :: zz
      real (kind=RKIND), dimension(:,:,:), pointer :: scalars
      real (kind=RKIND), dimension(:,:), pointer :: ru
      real (kind=RKIND), dimension(:,:), pointer :: rw
      real (kind=RKIND), dimension(:,:), pointer :: u
      real (kind=RKIND), dimension(:,:), pointer :: w
      real (kind=RKIND), dimension(:,:), pointer :: pressure_p
      real (kind=RKIND), dimension(:,:), pointer :: pressure_base
      real (kind=RKIND), dimension(:,:), pointer :: exner
      real (kind=RKIND), dimension(:,:), pointer :: exner_base
      real (kind=RKIND), dimension(:), pointer :: fzm, fzp
      real (kind=RKIND), dimension(:,:,:), pointer :: zb, zb3, zb_cell, zb3_cell


      call mpas_pool_get_dimension(mesh, 'nCells', nCells)
      call mpas_pool_get_dimension(mesh, 'nEdges', nEdges)
      call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)
      call mpas_pool_get_dimension(state, 'index_qv', index_qv)

      call mpas_pool_get_array(mesh, 'cellsOnEdge', cellsOnEdge)
      call mpas_pool_get_array(mesh, 'nEdgesOnCell', nEdgesOnCell)
      call mpas_pool_get_array(mesh, 'edgesOnCell', edgesOnCell)
      call mpas_pool_get_array(mesh, 'edgesOnCell_sign', edgesOnCell_sign)

      call mpas_pool_get_array(state, 'theta_m', theta_m, time_lev)
      call mpas_pool_get_array(diag, 'theta', theta)
      call mpas_pool_get_array(state, 'rho_zz', rho_zz, time_lev)
      call mpas_pool_get_array(diag, 'rho', rho)
      call mpas_pool_get_array(diag, 'rho_p', rho_p)
      call mpas_pool_get_array(diag, 'rho_base', rho_base)
      call mpas_pool_get_array(diag, 'rtheta_base', rtheta_base)
      call mpas_pool_get_array(diag, 'theta_base', theta_base)
      call mpas_pool_get_array(diag, 'rtheta_p', rtheta_p)
      call mpas_pool_get_array(mesh, 'zz', zz)
      call mpas_pool_get_array(state, 'scalars', scalars, time_lev)
      call mpas_pool_get_array(diag, 'ru', ru)
      call mpas_pool_get_array(diag, 'rw', rw)
      call mpas_pool_get_array(state, 'u', u, time_lev)
      call mpas_pool_get_array(state, 'w', w, time_lev)
      call mpas_pool_get_array(diag, 'pressure_p', pressure_p)
      call mpas_pool_get_array(diag, 'pressure_base', pressure_base)
      call mpas_pool_get_array(diag, 'exner', exner)
      call mpas_pool_get_array(diag, 'exner_base', exner_base)
      call mpas_pool_get_array(mesh, 'fzm', fzm)
      call mpas_pool_get_array(mesh, 'fzp', fzp)
      call mpas_pool_get_array(mesh, 'zb', zb)
      call mpas_pool_get_array(mesh, 'zb3', zb3)
      call mpas_pool_get_array(mesh, 'zb_cell', zb_cell)
      call mpas_pool_get_array(mesh, 'zb3_cell', zb3_cell)


      rcv = rgas / (cp-rgas)
      p0 = 1.e5  ! this should come from somewhere else...

      do iCell=cellStart,cellEnd
         do k=1,nVertLevels
            theta_m(k,iCell) = theta(k,iCell) * (1._RKIND + rvord * scalars(index_qv,k,iCell))
            rho_zz(k,iCell) = rho(k,iCell) / zz(k,iCell)
         end do
      end do

!$OMP BARRIER

      do iEdge=edgeStart,edgeEnd
         cell1 = cellsOnEdge(1,iEdge)
         cell2 = cellsOnEdge(2,iEdge)
         do k=1,nVertLevels
            ru(k,iEdge) = 0.5 * u(k,iEdge) * (rho_zz(k,cell1) + rho_zz(k,cell2))
         end do
      end do

!$OMP BARRIER

      ! Compute rw (i.e. rho_zz * omega) from rho_zz, w, and ru.
      ! We are reversing the procedure we use in subroutine atm_recover_large_step_variables.
      ! first, the piece that depends on w.
      do iCell=cellStart,cellEnd
         rw(1,iCell) = 0.0
         rw(nVertLevels+1,iCell) = 0.0
         do k=2,nVertLevels
            rw(k,iCell) = w(k,iCell)     &
                          * (fzp(k) * rho_zz(k-1,iCell) + fzm(k) * rho_zz(k,iCell)) &
                          * (fzp(k) * zz(k-1,iCell) + fzm(k) * zz(k,iCell))
         end do
      end do
  
      ! next, the piece that depends on ru
      do iCell=cellStart,cellEnd
         do i=1,nEdgesOnCell(iCell)
            iEdge = edgesOnCell(i,iCell)
            do k = 2,nVertLevels
            flux = (fzm(k)*ru(k,iEdge) + fzp(k)*ru(k-1,iEdge))
            rw(k,iCell) = rw(k,iCell)   &
                          - edgesOnCell_sign(i,iCell) * (zb_cell(k,i,iCell) + sign(1.0_RKIND,flux) * zb3_cell(k,i,iCell))*flux   &
                          * (fzp(k) * zz(k-1,iCell) + fzm(k) * zz(k,iCell))
            end do
         end do
      end do

      do iCell=cellStart,cellEnd
         do k=1,nVertLevels
            rho_p(k,iCell) = rho_zz(k,iCell) - rho_base(k,iCell)
         end do
      end do

      do iCell=cellStart,cellEnd
         do k=1,nVertLevels
            rtheta_base(k,iCell) = theta_base(k,iCell) * rho_base(k,iCell)
         end do
      end do

      do iCell=cellStart,cellEnd
         do k=1,nVertLevels
            rtheta_p(k,iCell) = theta_m(k,iCell) * rho_p(k,iCell)  &
                                             + rho_base(k,iCell)   * (theta_m(k,iCell) - theta_base(k,iCell))
         end do
      end do

      do iCell=cellStart,cellEnd
         do k=1,nVertLevels
            exner(k,iCell) = (zz(k,iCell) * (rgas/p0) * (rtheta_p(k,iCell) + rtheta_base(k,iCell)))**rcv
            exner_base(k,iCell) = (zz(k,iCell) * (rgas/p0) * (rtheta_base(k,iCell)))**rcv  ! WCS addition 20180403
         end do
      end do

      do iCell=cellStart,cellEnd
         do k=1,nVertLevels
            pressure_p(k,iCell) = zz(k,iCell) * rgas &
                                               * (  exner(k,iCell) * rtheta_p(k,iCell) &
                                                  + rtheta_base(k,iCell) * (exner(k,iCell) - exner_base(k,iCell)) &
                                                 )
            pressure_base(k,iCell) = zz(k,iCell) * rgas * exner_base(k,iCell) * rtheta_base(k,iCell)      ! WCS addition 20180403
         end do
      end do

   end subroutine atm_init_coupled_diagnostics


   subroutine atm_rk_dynamics_substep_finish( state, diag, dynamics_substep, dynamics_split, &
                                       cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd, &
                                       cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd)

      implicit none

      !  this routine resets the dry dynamics variables at the end of an rk3 substep for the case
      !  where the dry dynamics is split from the scalar transport (i.e. where the dry dynamics is
      !  using a different, usually smaller, timestep.
      !
      !  WCS 18 November 2014

      type (mpas_pool_type), intent(inout) :: state
      type (mpas_pool_type), intent(inout) :: diag
      integer, intent(in) :: dynamics_substep, dynamics_split
      integer, intent(in) :: cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd
      integer, intent(in) :: cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd

      real (kind=RKIND) :: inv_dynamics_split
      
      real (kind=RKIND), dimension(:,:), pointer :: ru
      real (kind=RKIND), dimension(:,:), pointer :: ru_save
      real (kind=RKIND), dimension(:,:), pointer :: rw
      real (kind=RKIND), dimension(:,:), pointer :: rw_save
      real (kind=RKIND), dimension(:,:), pointer :: rtheta_p
      real (kind=RKIND), dimension(:,:), pointer :: rtheta_p_save
      real (kind=RKIND), dimension(:,:), pointer :: rho_p
      real (kind=RKIND), dimension(:,:), pointer :: rho_p_save

      real (kind=RKIND), dimension(:,:), pointer :: u_1, u_2
      real (kind=RKIND), dimension(:,:), pointer :: w_1, w_2
      real (kind=RKIND), dimension(:,:), pointer :: theta_m_1, theta_m_2
      real (kind=RKIND), dimension(:,:), pointer :: rho_zz_1, rho_zz_2, rho_zz_old_split
      real (kind=RKIND), dimension(:,:), pointer :: ruAvg, wwAvg, ruAvg_split, wwAvg_split

      call mpas_pool_get_array(diag, 'ru', ru)
      call mpas_pool_get_array(diag, 'ru_save', ru_save)
      call mpas_pool_get_array(diag, 'rw', rw)
      call mpas_pool_get_array(diag, 'rw_save', rw_save)
      call mpas_pool_get_array(diag, 'rtheta_p', rtheta_p)
      call mpas_pool_get_array(diag, 'rtheta_p_save', rtheta_p_save)
      call mpas_pool_get_array(diag, 'rho_p', rho_p)
      call mpas_pool_get_array(diag, 'rho_p_save', rho_p_save)
      call mpas_pool_get_array(diag, 'rho_zz_old_split', rho_zz_old_split)
      call mpas_pool_get_array(diag, 'ruAvg', ruAvg)
      call mpas_pool_get_array(diag, 'ruAvg_split', ruAvg_split)
      call mpas_pool_get_array(diag, 'wwAvg', wwAvg)
      call mpas_pool_get_array(diag, 'wwAvg_split', wwAvg_split)

      call mpas_pool_get_array(state, 'u', u_1, 1)
      call mpas_pool_get_array(state, 'u', u_2, 2)
      call mpas_pool_get_array(state, 'w', w_1, 1)
      call mpas_pool_get_array(state, 'w', w_2, 2)
      call mpas_pool_get_array(state, 'theta_m', theta_m_1, 1)
      call mpas_pool_get_array(state, 'theta_m', theta_m_2, 2)
      call mpas_pool_get_array(state, 'rho_zz', rho_zz_1, 1)
      call mpas_pool_get_array(state, 'rho_zz', rho_zz_2, 2)

      inv_dynamics_split = 1.0_RKIND / real(dynamics_split)
      
      if (dynamics_substep < dynamics_split) then

         ru_save(:,edgeStart:edgeEnd) = ru(:,edgeStart:edgeEnd)
         rw_save(:,cellStart:cellEnd) = rw(:,cellStart:cellEnd)
         rtheta_p_save(:,cellStart:cellEnd) = rtheta_p(:,cellStart:cellEnd)
         rho_p_save(:,cellStart:cellEnd) = rho_p(:,cellStart:cellEnd)

         u_1(:,edgeStart:edgeEnd) = u_2(:,edgeStart:edgeEnd)
         w_1(:,cellStart:cellEnd) = w_2(:,cellStart:cellEnd)
         theta_m_1(:,cellStart:cellEnd) = theta_m_2(:,cellStart:cellEnd)
         rho_zz_1(:,cellStart:cellEnd) = rho_zz_2(:,cellStart:cellEnd)

      end if

      if (dynamics_substep == 1) then
         ruAvg_split(:,edgeStart:edgeEnd) = ruAvg(:,edgeStart:edgeEnd)
         wwAvg_split(:,cellStart:cellEnd) = wwAvg(:,cellStart:cellEnd)
      else
         ruAvg_split(:,edgeStart:edgeEnd) = ruAvg(:,edgeStart:edgeEnd)+ruAvg_split(:,edgeStart:edgeEnd)
         wwAvg_split(:,cellStart:cellEnd) = wwAvg(:,cellStart:cellEnd)+wwAvg_split(:,cellStart:cellEnd)
      end if

      if (dynamics_substep == dynamics_split) then
         ruAvg(:,edgeStart:edgeEnd) = ruAvg_split(:,edgeStart:edgeEnd) * inv_dynamics_split
         wwAvg(:,cellStart:cellEnd) = wwAvg_split(:,cellStart:cellEnd) * inv_dynamics_split
         rho_zz_1(:,cellStart:cellEnd) = rho_zz_old_split(:,cellStart:cellEnd)
      end if

   end subroutine atm_rk_dynamics_substep_finish


!-------------------------------------------------------------------------
!
! these next 2 routines set an approximate zero gradient boundary condition for w for regional_MPAS
!   
   subroutine atm_zero_gradient_w_bdy( state, mesh, cellSolveStart, cellSolveEnd )

      ! reconstitute state variables from acoustic-step perturbation variables 
      ! after the acoustic steps.  The perturbation variables were originally set in
      ! subroutine atm_set_smlstep_pert_variables prior to their acoustic-steps update.
      ! we are also computing a few other state-derived variables here.

      implicit none

      type (mpas_pool_type), intent(inout) :: state
      type (mpas_pool_type), intent(inout) :: mesh
      integer, intent(in) :: cellSolveStart, cellSolveEnd

      real (kind=RKIND), dimension(:,:), pointer :: w

      integer, dimension(:), pointer :: bdyMaskCell, nearestRelaxationCell
      integer, pointer :: nCells

      call mpas_pool_get_array(state, 'w', w, 2)
      call mpas_pool_get_array(mesh, 'bdyMaskCell', bdyMaskCell)
      call mpas_pool_get_array(mesh, 'nearestRelaxationCell', nearestRelaxationCell)
      call mpas_pool_get_dimension(mesh, 'nCells', nCells)
      
      call atm_zero_gradient_w_bdy_work( w, bdyMaskCell, nearestRelaxationCell, nCells, cellSolveStart, cellSolveEnd )

   end subroutine atm_zero_gradient_w_bdy

!-------------------------------------------------------------------------

   subroutine atm_zero_gradient_w_bdy_work( w, bdyMaskCell, nearestRelaxationCell, nCells, cellSolveStart, cellSolveEnd )

      use mpas_atm_dimensions

      implicit none

      !
      ! Dummy arguments
      !
      integer, intent(in) :: cellSolveStart, cellSolveEnd, nCells
      integer, dimension(nCells+1), intent(in) :: bdyMaskCell, nearestRelaxationCell
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1), intent(inout) :: w

      ! local variables

      integer :: iCell, k

      do iCell=cellSolveStart,cellSolveEnd
         if (bdyMaskCell(iCell) > nRelaxZone) then
!DIR$ IVDEP
            do k = 2, nVertLevels
              w(k,iCell) = w(k,nearestRelaxationCell(iCell))
            end do
         end if  
      end do

   end subroutine atm_zero_gradient_w_bdy_work

!-------------------------------------------------------------------------

   subroutine atm_bdy_adjust_dynamics_speczone_tend( tend, mesh, config, nVertLevels,               &
                                                     ru_driving_tend, rt_driving_tend, rho_driving_tend,         &
                                                     cellStart, cellEnd, edgeStart, edgeEnd,                     &
                                                     cellSolveStart, cellSolveEnd, edgeSolveStart, edgeSolveEnd )

      implicit none

      !  this routine resets the dry dynamics variables at the end of an rk3 substep for the case
      !  where the dry dynamics is split from the scalar transport (i.e. where the dry dynamics is
      !  using a different, usually smaller, timestep.
      !
      !  WCS Fall 2016

      type (mpas_pool_type), intent(inout) :: tend
      type (mpas_pool_type), intent(in) :: mesh
      type (mpas_pool_type), intent(in) :: config
      integer, intent(in) :: nVertLevels
      integer, intent(in) :: cellStart, cellEnd, edgeStart, edgeEnd
      integer, intent(in) :: cellSolveStart, cellSolveEnd, edgeSolveStart, edgeSolveEnd

      real (kind=RKIND), dimension(:,:), intent(in) :: ru_driving_tend, rt_driving_tend, rho_driving_tend
      real (kind=RKIND), dimension(:,:), pointer :: tend_ru, tend_rt, tend_rho, tend_rw, rt_diabatic_tend
      integer, dimension(:), pointer :: bdyMaskCell, bdyMaskEdge

      integer :: iCell, iEdge, k

      call mpas_pool_get_array(tend, 'u', tend_ru)
      call mpas_pool_get_array(tend, 'rho_zz', tend_rho)
      call mpas_pool_get_array(tend, 'theta_m', tend_rt)
      call mpas_pool_get_array(tend, 'w', tend_rw)
      call mpas_pool_get_array(mesh, 'bdyMaskCell', bdyMaskCell)
      call mpas_pool_get_array(mesh, 'bdyMaskEdge', bdyMaskEdge)
      call mpas_pool_get_array(tend, 'rt_diabatic_tend', rt_diabatic_tend)
      
      do iCell = cellSolveStart, cellSolveEnd
         if(bdyMaskCell(iCell) > nRelaxZone) then
            do k=1, nVertLevels
               tend_rho(k,iCell) = rho_driving_tend(k,iCell)
               tend_rt(k,iCell) = rt_driving_tend(k,iCell)
               tend_rw(k,iCell) = 0.
               rt_diabatic_tend(k,iCell) = 0.
            end do
         end if
      end do

      do iEdge = edgeSolveStart, edgeSolveEnd
         if(bdyMaskEdge(iEdge) > nRelaxZone) then
            do k=1, nVertLevels
               tend_ru(k,iEdge) = ru_driving_tend(k,iEdge)
            end do
         end if
      end do
      
    end subroutine atm_bdy_adjust_dynamics_speczone_tend

!-------------------------------------------------------------------------

   subroutine atm_bdy_adjust_dynamics_relaxzone_tend( config, tend, state, diag, mesh, nVertLevels, dt,                                             &
                                                      ru_driving_values, rt_driving_values, rho_driving_values,                                     &
                                                      cellStart, cellEnd, edgeStart, edgeEnd,                               &
                                                      cellSolveStart, cellSolveEnd, edgeSolveStart, edgeSolveEnd )

      implicit none

      !  this routine resets the dry dynamics variables at the end of an rk3 substep for the case
      !  where the dry dynamics is split from the scalar transport (i.e. where the dry dynamics is
      !  using a different, usually smaller, timestep.
      !
      !  WCS Fall 2016

      type (mpas_pool_type), intent(in) :: config
      type (mpas_pool_type), intent(in) :: state
      type (mpas_pool_type), intent(inout) :: tend
      type (mpas_pool_type), intent(in) :: diag
      type (mpas_pool_type), intent(in) :: mesh
      integer, intent(in) :: nVertLevels
      integer, intent(in) :: cellStart, cellEnd, edgeStart, edgeEnd
      integer, intent(in) :: cellSolveStart, cellSolveEnd, edgeSolveStart, edgeSolveEnd

      real (kind=RKIND), intent(in) :: dt

      real (kind=RKIND), dimension(:,:), intent(in) :: ru_driving_values, rt_driving_values, rho_driving_values

      real (kind=RKIND), dimension(:,:), pointer :: tend_ru, tend_rt, tend_rho, tend_rw, ru, theta_m, rho_zz
      real (kind=RKIND), dimension(:), pointer :: dcEdge, dvEdge, invDcEdge, invDvEdge, invAreaCell, invAreaTriangle
      real (kind=RKIND), dimension(:,:), pointer :: edgesOnCell_sign, edgesOnVertex_sign
      integer, dimension(:), pointer :: bdyMaskCell, bdyMaskEdge, nEdgesOnCell
      integer, dimension(:,:), pointer :: cellsOnEdge, verticesOnEdge, edgesOnCell, edgesOnVertex
      integer, pointer :: vertexDegree
      

      real (kind=RKIND) :: edge_sign, laplacian_filter_coef, rayleigh_damping_coef, r_dc, r_dv, invArea
      real (kind=RKIND), pointer :: divdamp_coef
      real (kind=RKIND), dimension(nVertLevels) :: divergence1, divergence2, vorticity1, vorticity2      
      integer :: iCell, iEdge, i, k, cell1, cell2, iEdge_vort, iEdge_div
      integer :: vertex1, vertex2, iVertex

      real (kind=RKIND), dimension(:), pointer :: meshScalingRegionalCell, meshScalingRegionalEdge

      call mpas_pool_get_array(tend, 'u', tend_ru)
      call mpas_pool_get_array(tend, 'rho_zz', tend_rho)
      call mpas_pool_get_array(tend, 'theta_m', tend_rt)
      call mpas_pool_get_array(tend, 'w', tend_rw)
      call mpas_pool_get_array(mesh, 'bdyMaskCell', bdyMaskCell)
      call mpas_pool_get_array(mesh, 'bdyMaskEdge', bdyMaskEdge)

      call mpas_pool_get_array(mesh, 'meshScalingRegionalCell', meshScalingRegionalCell)
      call mpas_pool_get_array(mesh, 'meshScalingRegionalEdge', meshScalingRegionalEdge)

      call mpas_pool_get_array(diag, 'ru', ru)
      call mpas_pool_get_array(state, 'theta_m', theta_m, 2)
      call mpas_pool_get_array(state, 'rho_zz', rho_zz, 2)

      call mpas_pool_get_dimension(mesh, 'vertexDegree', vertexDegree)
      call mpas_pool_get_array(mesh, 'dcEdge', dcEdge)
      call mpas_pool_get_array(mesh, 'dvEdge', dvEdge)
      call mpas_pool_get_array(mesh, 'invDcEdge', invDcEdge)
      call mpas_pool_get_array(mesh, 'invDvEdge', invDvEdge)
      call mpas_pool_get_array(mesh, 'invAreaCell', invAreaCell)
      call mpas_pool_get_array(mesh, 'invAreaTriangle', invAreaTriangle)
      call mpas_pool_get_array(mesh, 'edgesOnCell_sign', edgesOnCell_sign)
      call mpas_pool_get_array(mesh, 'edgesOnVertex_sign', edgesOnVertex_sign)
      call mpas_pool_get_array(mesh, 'cellsOnEdge', cellsOnEdge)
      call mpas_pool_get_array(mesh, 'edgesOnCell', edgesOnCell)
      call mpas_pool_get_array(mesh, 'edgesOnVertex', edgesOnVertex)
      call mpas_pool_get_array(mesh, 'nEdgesOnCell',nEdgesOnCell)
      call mpas_pool_get_array(mesh, 'verticesOnEdge', verticesOnEdge)

      call mpas_pool_get_config(config, 'config_relax_zone_divdamp_coef', divdamp_coef)
      
      !  First, Rayleigh damping terms for ru, rtheta_m and rho_zz

      do iCell = cellSolveStart, cellSolveEnd
         if( (bdyMaskCell(iCell) > 1) .and. (bdyMaskCell(iCell) <= nRelaxZone) ) then
            rayleigh_damping_coef = (real(bdyMaskCell(iCell)) - 1.)/real(nRelaxZone)/(50.*dt*meshScalingRegionalCell(iCell))
            do k=1, nVertLevels
               tend_rho(k,iCell) = tend_rho(k,iCell) - rayleigh_damping_coef * (rho_zz(k,iCell) - rho_driving_values(k,iCell))
               tend_rt(k,iCell)  = tend_rt(k,iCell)  - rayleigh_damping_coef * (rho_zz(k,iCell)*theta_m(k,iCell) - rt_driving_values(k,iCell))
            end do
         end if
      end do

      do iEdge = edgeStart, edgeEnd
         if( (bdyMaskEdge(iEdge) > 1) .and. (bdyMaskEdge(iEdge) <= nRelaxZone) ) then
            rayleigh_damping_coef = (real(bdyMaskEdge(iEdge)) - 1.)/real(nRelaxZone)/(50.*dt*meshScalingRegionalEdge(iEdge))
            do k=1, nVertLevels
               tend_ru(k,iEdge) = tend_ru(k,iEdge) - rayleigh_damping_coef * (ru(k,iEdge) - ru_driving_values(k,iEdge))
            end do
         end if
      end do
      
      !  Second, the horizontal filter for rtheta_m and rho_zz

      do iCell = cellSolveStart, cellSolveEnd ! threaded over cells

         if ( (bdyMaskCell(iCell) > 1) .and. (bdyMaskCell(iCell) <= nRelaxZone) ) then ! relaxation zone

            laplacian_filter_coef = (real(bdyMaskCell(iCell)) - 1.)/real(nRelaxZone)/(10.*dt*meshScalingRegionalCell(iCell))
            !
            do i=1,nEdgesOnCell(iCell)
               iEdge = edgesOnCell(i,iCell)
               !  edge_sign = r_areaCell*edgesOnCell_sign(i,iCell) * dvEdge(iEdge) * invDcEdge(iEdge) * laplacian_filter_coef
               ! this is a dimensionless laplacian, so we leave out the r_areaCell
               edge_sign = edgesOnCell_sign(i,iCell) * dvEdge(iEdge) * invDcEdge(iEdge) * laplacian_filter_coef
               cell1 = cellsOnEdge(1,iEdge)
               cell2 = cellsOnEdge(2,iEdge)
!DIR$ IVDEP
               do k=1,nVertLevels
                  tend_rt(k,iCell)  = tend_rt(k,iCell)  + edge_sign*(   (rho_zz(k,cell2)*theta_m(k,cell2)-rt_driving_values(k,cell2)) &
                                                                    - (rho_zz(k,cell1)*theta_m(k,cell1)-rt_driving_values(k,cell1)) )
                  tend_rho(k,iCell) = tend_rho(k,iCell) + edge_sign*(   (rho_zz(k,cell2)-rho_driving_values(k,cell2)) &
                                                                    - (rho_zz(k,cell1)-rho_driving_values(k,cell1)) )
               end do
            end do

         end if

      end do

      !  Third (and last), the horizontal filter for ru

      do iEdge = edgeStart, edgeEnd

         if ( (bdyMaskEdge(iEdge) > 1) .and. (bdyMaskEdge(iEdge) <= nRelaxZone) ) then ! relaxation zone

            laplacian_filter_coef = dcEdge(iEdge)**2 * (real(bdyMaskEdge(iEdge)) - 1.)/   &
                                                real(nRelaxZone)/(10.*dt*meshScalingRegionalEdge(iEdge))

            cell1 = cellsOnEdge(1,iEdge)
            cell2 = cellsOnEdge(2,iEdge)
            vertex1 = verticesOnEdge(1,iEdge)
            vertex2 = verticesOnEdge(2,iEdge)
            r_dc = invDcEdge(iEdge)
            r_dv = min(invDvEdge(iEdge), 4*invDcEdge(iEdge))

            iCell = cell1
            invArea = invAreaCell(iCell)
            divergence1(1:nVertLevels) = 0.
            do i=1,nEdgesOnCell(iCell)
               iEdge_div = edgesOnCell(i,iCell)
               edge_sign = invArea * dvEdge(iEdge_div) * edgesOnCell_sign(i,iCell)
               do k=1,nVertLevels
                  divergence1(k) = divergence1(k) + edge_sign * (ru(k,iEdge_div) - ru_driving_values(k,iEdge_div))
               end do
            end do

            iCell = cell2
            invArea = invAreaCell(iCell)
            divergence2(1:nVertLevels) = 0.
            do i=1,nEdgesOnCell(iCell)
               iEdge_div = edgesOnCell(i,iCell)
               edge_sign = invArea * dvEdge(iEdge_div) * edgesOnCell_sign(i,iCell)
               do k=1,nVertLevels
                  divergence2(k) = divergence2(k) + edge_sign * (ru(k,iEdge_div) - ru_driving_values(k,iEdge_div))
               end do
            end do
            
            iVertex = vertex1
            vorticity1(1:nVertLevels) = 0.
            do i=1,vertexDegree
               iEdge_vort = edgesOnVertex(i,iVertex)
               edge_sign = invAreaTriangle(iVertex) * dcEdge(iEdge_vort) * edgesOnVertex_sign(i,iVertex)
               do k=1,nVertLevels
                  vorticity1(k) = vorticity1(k) + edge_sign * (ru(k,iEdge_vort) - ru_driving_values(k,iEdge_vort))
               end do
            end do

            iVertex = vertex2
            vorticity2(1:nVertLevels) = 0.
            do i=1,vertexDegree
               iEdge_vort = edgesOnVertex(i,iVertex)
               edge_sign = invAreaTriangle(iVertex) * dcEdge(iEdge_vort) * edgesOnVertex_sign(i,iVertex)
               do k=1,nVertLevels
                  vorticity2(k) = vorticity2(k) + edge_sign * (ru(k,iEdge_vort) - ru_driving_values(k,iEdge_vort))
               end do
            end do

            ! Compute diffusion, computed as \nabla divergence - k \times \nabla vorticity
            !
             do k=1,nVertLevels
                tend_ru(k,iEdge) = tend_ru(k,iEdge) + laplacian_filter_coef &
                                                      * (divdamp_coef * (divergence2(k) - divergence1(k)) * r_dc  &
                                                                       -(vorticity2(k)  - vorticity1(k))  * r_dv)
             end do

          end if  !  end test for relaxation-zone edge

       end do  !  end of loop over edges
       
   end subroutine atm_bdy_adjust_dynamics_relaxzone_tend
      

   subroutine atm_bdy_reset_speczone_values( state, diag, mesh, nVertLevels, &
                                             rt_driving_values, rho_driving_values, &
                                             cellStart, cellEnd, &
                                             cellSolveStart, cellSolveEnd )

      implicit none

      !  this routine resets theta_m and rtheta_m after the microphysics, i.e. at the very end of the timestep
      !
      !  WCS 24 February 2017

      type (mpas_pool_type), intent(in) :: state
      type (mpas_pool_type), intent(in) :: diag
      type (mpas_pool_type), intent(in) :: mesh
      integer, intent(in) :: nVertLevels
      integer, intent(in) :: cellStart, cellEnd
      integer, intent(in) :: cellSolveStart, cellSolveEnd

      real (kind=RKIND), dimension(:,:), intent(in) :: rt_driving_values, rho_driving_values

      real (kind=RKIND), dimension(:,:), pointer :: theta_m, rtheta_p, rtheta_base
      integer, dimension(:), pointer :: bdyMaskCell
      
      integer :: iCell, k

      call mpas_pool_get_array(mesh, 'bdyMaskCell', bdyMaskCell)
      call mpas_pool_get_array(state, 'theta_m', theta_m, 2)
      call mpas_pool_get_array(diag, 'rtheta_p', rtheta_p)
      call mpas_pool_get_array(diag, 'rtheta_base', rtheta_base)
      
      do iCell = cellSolveStart, cellSolveEnd
         if( bdyMaskCell(iCell) > nRelaxZone) then
            do k=1, nVertLevels
              theta_m(k,iCell) = rt_driving_values(k,iCell)/rho_driving_values(k,iCell)
              rtheta_p(k,iCell) = rt_driving_values(k,iCell) - rtheta_base(k,iCell)
            end do
         end if
      end do

   end subroutine atm_bdy_reset_speczone_values

!-------------------------------------------------------------------------
   subroutine atm_bdy_adjust_scalars( state, diag, mesh, config, scalars_driving, nVertLevels, dt, dt_rk, &
                                       cellStart, cellEnd, &
                                       cellSolveStart, cellSolveEnd )

      implicit none

      !  this routine resets the dry dynamics variables at the end of an rk3 substep for the case
      !  where the dry dynamics is split from the scalar transport (i.e. where the dry dynamics is
      !  using a different, usually smaller, timestep.
      !
      !  WCS 24 February 2017

      type (mpas_pool_type), intent(inout) :: state
      type (mpas_pool_type), intent(in) :: diag
      type (mpas_pool_type), intent(in) :: mesh
      type (mpas_pool_type), intent(in) :: config
      integer, intent(in) :: nVertLevels
      integer, intent(in) :: cellStart, cellEnd
      integer, intent(in) :: cellSolveStart, cellSolveEnd

      real (kind=RKIND), intent(in) :: dt, dt_rk
      real (kind=RKIND), dimension(:,:,:), intent(in) :: scalars_driving
      real (kind=RKIND), dimension(:,:,:), pointer :: scalars_new
      real (kind=RKIND), dimension(:,:), pointer :: edgesOnCell_sign

      real (kind=RKIND), dimension(:), pointer :: invDcEdge, dvEdge, meshScalingRegionalCell
      integer, dimension(:), pointer :: nEdgesOnCell
      integer, dimension(:,:), pointer :: edgesOnCell, cellsOnEdge
      integer, pointer :: nCells, maxEdges, num_scalars
      integer, dimension(:), pointer :: bdyMaskCell

      call mpas_pool_get_array(state, 'scalars', scalars_new, 2)

      call mpas_pool_get_array(mesh, 'invDcEdge', invDcEdge )
      call mpas_pool_get_array(mesh, 'dvEdge', dvEdge )
      call mpas_pool_get_array(mesh, 'edgesOnCell', edgesOnCell)
      call mpas_pool_get_array(mesh, 'nEdgesOnCell', nEdgesOnCell)
      call mpas_pool_get_array(mesh, 'cellsOnEdge', cellsOnEdge)
      call mpas_pool_get_array(mesh, 'bdyMaskCell', bdyMaskCell)
      call mpas_pool_get_array(mesh, 'meshScalingRegionalCell', meshScalingRegionalCell)
      call mpas_pool_get_array(mesh, 'edgesOnCell_sign', edgesOnCell_sign)

      call mpas_pool_get_dimension(mesh, 'nCells', nCells)
      call mpas_pool_get_dimension(mesh, 'maxEdges', maxEdges)

      call mpas_pool_get_dimension(state, 'num_scalars', num_scalars)

      call atm_bdy_adjust_scalars_work( scalars_new, scalars_driving, dt, dt_rk, &
                                        nVertLevels, nCells, num_scalars, &
                                        nEdgesOnCell, edgesOnCell, EdgesOnCell_sign, cellsOnEdge, dvEdge, invDcEdge, bdyMaskCell, &
                                        meshScalingRegionalCell,  &
                                        cellStart, cellEnd, &
                                        cellSolveStart, cellSolveEnd )

   end subroutine atm_bdy_adjust_scalars

!-------------------------------------------------------------------------

   subroutine atm_bdy_adjust_scalars_work( scalars_new, scalars_driving, dt, dt_rk, &
                                           nVertLevels, nCells, num_scalars, &
                                           nEdgesOnCell, edgesOnCell, EdgesOnCell_sign, cellsOnEdge, dvEdge, invDcEdge, bdyMaskCell, &
                                           meshScalingRegionalCell,  &
                                           cellStart, cellEnd, &
                                           cellSolveStart, cellSolveEnd )

      implicit none

      real (kind=RKIND), dimension(:,:,:), intent(in) :: scalars_driving
      real (kind=RKIND), dimension(:,:,:), intent(inout) :: scalars_new
      real (kind=RKIND), dimension(:,:), intent(in) :: edgesOnCell_sign
      integer, intent(in) :: nVertLevels, nCells, num_scalars
      integer, intent(in) :: cellStart, cellEnd
      integer, intent(in) :: cellSolveStart, cellSolveEnd
      integer, dimension(:), intent(in) :: nEdgesOnCell, bdyMaskCell
      integer, dimension(:,:), intent(in) :: edgesOnCell, cellsOnEdge
      real (kind=RKIND), dimension(:), intent(in) :: dvEdge, invDcEdge, meshScalingRegionalCell
      real (kind=RKIND), intent(in) :: dt, dt_rk

      ! local variables

      real (kind=RKIND), dimension(1:num_scalars,1:nVertLevels, cellSolveStart:cellSolveEnd) :: scalars_tmp
      real (kind=RKIND) :: edge_sign, laplacian_filter_coef, rayleigh_damping_coef, filter_flux
      integer :: iCell, iEdge, iScalar, i, k, cell1, cell2

      !---

      do iCell = cellSolveStart, cellSolveEnd ! threaded over cells

         if ( (bdyMaskCell(iCell) > 1) .and. (bdyMaskCell(iCell) <= nRelaxZone) ) then ! relaxation zone

            laplacian_filter_coef = dt_rk*(real(bdyMaskCell(iCell)) - 1.)/real(nRelaxZone)/(10.*dt*meshScalingRegionalCell(iCell))
            rayleigh_damping_coef = laplacian_filter_coef/5.0
            scalars_tmp(1:num_scalars,1:nVertLevels,iCell) = scalars_new(1:num_scalars,1:nVertLevels,iCell)

            !  first, we compute the 2nd-order laplacian filter
            !
            do i=1,nEdgesOnCell(iCell)
               iEdge = edgesOnCell(i,iCell)
               !  edge_sign = r_areaCell*edgesOnCell_sign(i,iCell) * dvEdge(iEdge) * invDcEdge(iEdge) * laplacian_filter_coef
               ! this is a dimensionless laplacian, so we leave out the r_areaCell
               edge_sign = edgesOnCell_sign(i,iCell) * dvEdge(iEdge) * invDcEdge(iEdge) * laplacian_filter_coef
               cell1 = cellsOnEdge(1,iEdge)
               cell2 = cellsOnEdge(2,iEdge)
!DIR$ IVDEP
               do k=1,nVertLevels
                  do iScalar = 1, num_scalars
                     filter_flux = edge_sign*(   (scalars_new(iScalar,k,cell2)-scalars_driving(iScalar,k,cell2)) &
                                               - (scalars_new(iScalar,k,cell1)-scalars_driving(iScalar,k,cell1)) )
                     scalars_tmp(iScalar,k,iCell) = scalars_tmp(iScalar,k,iCell) + filter_flux
                  end do
               end do
            end do

            !  second, we compute the Rayleigh damping component
            !
!DIR$ IVDEP
            do k=1,nVertLevels
               do iScalar = 1, num_scalars
                  scalars_tmp(iScalar,k,iCell) =scalars_tmp(iScalar,k,iCell) - rayleigh_damping_coef * (scalars_new(iScalar,k,iCell)-scalars_driving(iScalar,k,iCell))
               end do
            end do

         else  if ( bdyMaskCell(iCell) > nRelaxZone) then ! specified zone
            
            !  update the specified-zone values
            !
!DIR$ IVDEP
            do k=1,nVertLevels
               do iScalar = 1, num_scalars
                  scalars_tmp(iScalar,k,iCell) = scalars_driving(iScalar,k,iCell)
               end do
            end do

         end if

      end do  ! updates now in temp storage
            
!$OMP BARRIER

      do iCell = cellSolveStart, cellSolveEnd ! threaded over cells
         if (bdyMaskCell(iCell) > 1) then ! update values
!DIR$ IVDEP
            do k=1,nVertLevels
               do iScalar = 1, num_scalars
                  scalars_new(iScalar,k,iCell) = scalars_tmp(iScalar,k,iCell)
               end do
            end do
         end if
      end do

   end subroutine atm_bdy_adjust_scalars_work

!-------------------------------------------------------------------------

   subroutine atm_bdy_set_scalars( state, mesh, scalars_driving, nVertLevels, &
                                   cellStart, cellEnd, &
                                   cellSolveStart, cellSolveEnd )

      implicit none

      !  this routine resets the dry dynamics variables at the end of an rk3 substep for the case
      !  where the dry dynamics is split from the scalar transport (i.e. where the dry dynamics is
      !  using a different, usually smaller, timestep.
      !
      !  WCS 24 February 2017

      type (mpas_pool_type), intent(inout) :: state
      type (mpas_pool_type), intent(in) :: mesh
      integer, intent(in) :: nVertLevels
      integer, intent(in) :: cellStart, cellEnd
      integer, intent(in) :: cellSolveStart, cellSolveEnd

      real (kind=RKIND), dimension(:,:,:), intent(in) :: scalars_driving

      real (kind=RKIND), dimension(:,:,:), pointer :: scalars_new
      integer, pointer :: nCells, num_scalars
      integer, dimension(:), pointer :: bdyMaskCell

      call mpas_pool_get_array(mesh, 'bdyMaskCell', bdyMaskCell)

      call mpas_pool_get_dimension(mesh, 'nCells', nCells)

      call mpas_pool_get_dimension(state, 'num_scalars', num_scalars)

      call mpas_pool_get_array(state, 'scalars', scalars_new, 2)

      call atm_bdy_set_scalars_work( scalars_driving, scalars_new, &
                                        nVertLevels, nCells, num_scalars, &
                                        bdyMaskCell, &
                                        cellStart, cellEnd, &
                                        cellSolveStart, cellSolveEnd )

   end subroutine atm_bdy_set_scalars

!-------------------------------------------------------------------------

   subroutine atm_bdy_set_scalars_work( scalars_driving, scalars_new, &
                                           nVertLevels, nCells, num_scalars, &
                                           bdyMaskCell, &
                                           cellStart, cellEnd, &
                                           cellSolveStart, cellSolveEnd )

      implicit none

      real (kind=RKIND), dimension(:,:,:), intent(in) :: scalars_driving
      real (kind=RKIND), dimension(:,:,:), intent(inout) :: scalars_new
      integer, intent(in) :: nVertLevels, nCells, num_scalars
      integer, intent(in) :: cellStart, cellEnd
      integer, intent(in) :: cellSolveStart, cellSolveEnd
      integer, dimension(:), intent(in) :: bdyMaskCell

      ! local variables

      real (kind=RKIND) :: laplacian_filter_coef, rayleigh_damping_coef, filter_flux
      integer :: iCell, iScalar, i, k, cell1, cell2

      !---

      do iCell = cellSolveStart, cellSolveEnd ! threaded over cells

         if ( bdyMaskCell(iCell) > nRelaxZone) then ! specified zone
            
            !  update the specified-zone values
            !
!DIR$ IVDEP
            do k=1,nVertLevels
               do iScalar = 1, num_scalars
                  scalars_new(iScalar,k,iCell) = scalars_driving(iScalar,k,iCell)
               end do
            end do

         end if

      end do  ! updates now in temp storage
            
   end subroutine atm_bdy_set_scalars_work

!-------------------------------------------------------------------------

   subroutine summarize_timestep(domain)

       use ieee_arithmetic, only : ieee_is_nan

       implicit none

       type (domain_type), intent(inout) :: domain

       real (kind=RKIND), parameter :: pi_const = 2.0_RKIND*asin(1.0_RKIND)

       logical, pointer :: config_print_global_minmax_vel
       logical, pointer :: config_print_detailed_minmax_vel
       logical, pointer :: config_print_global_minmax_sca

       integer :: iCell, k, iEdge, iScalar
       integer, pointer :: num_scalars, nCellsSolve, nEdgesSolve, nVertLevels

       type (mpas_pool_type), pointer :: state
       type (mpas_pool_type), pointer :: diag
       type (mpas_pool_type), pointer :: mesh

       real (kind=RKIND) :: scalar_min, scalar_max
       real (kind=RKIND) :: global_scalar_min, global_scalar_max

       real (kind=RKIND), dimension(:), pointer :: latCell
       real (kind=RKIND), dimension(:), pointer :: lonCell
       real (kind=RKIND), dimension(:), pointer :: latEdge
       real (kind=RKIND), dimension(:), pointer :: lonEdge
       integer, dimension(:), pointer :: indexToCellID
       integer :: indexMax, indexMax_global
       integer :: kMax, kMax_global
       real (kind=RKIND) :: latMax, latMax_global
       real (kind=RKIND) :: lonMax, lonMax_global
       real (kind=RKIND), dimension(5) :: localVals, globalVals

       real (kind=RKIND) :: spd
       real (kind=RKIND), dimension(:,:), pointer :: w
       real (kind=RKIND), dimension(:,:), pointer :: u, v, uReconstructZonal, uReconstructMeridional, uReconstructX, uReconstructY, uReconstructZ
       real (kind=RKIND), dimension(:,:,:), pointer :: scalars, scalars_1, scalars_2

       call mpas_pool_get_config(block % configs, 'config_print_global_minmax_vel', config_print_global_minmax_vel)
       call mpas_pool_get_config(block % configs, 'config_print_detailed_minmax_vel', config_print_detailed_minmax_vel)
       call mpas_pool_get_config(block % configs, 'config_print_global_minmax_sca', config_print_global_minmax_sca)

      if (config_print_detailed_minmax_vel) then
         call mpas_log_write('')

         call mpas_pool_get_subpool(block % structs, 'state', state)
         call mpas_pool_get_subpool(block % structs, 'diag', diag)
         call mpas_pool_get_subpool(block % structs, 'mesh', mesh)

         call mpas_pool_get_array(state, 'w', w, 2)
         call mpas_pool_get_array(state, 'u', u, 2)
         call mpas_pool_get_array(diag, 'v', v)
         call mpas_pool_get_array(mesh, 'indexToCellID', indexToCellID)
         call mpas_pool_get_array(mesh, 'latCell', latCell)
         call mpas_pool_get_array(mesh, 'lonCell', lonCell)
         call mpas_pool_get_array(mesh, 'latEdge', latEdge)
         call mpas_pool_get_array(mesh, 'lonEdge', lonEdge)
         call mpas_pool_get_dimension(state, 'nCellsSolve', nCellsSolve)
         call mpas_pool_get_dimension(state, 'nEdgesSolve', nEdgesSolve)
         call mpas_pool_get_dimension(state, 'nVertLevels', nVertLevels)

         scalar_min = 1.0e20
         indexMax = -1
         kMax = -1
         latMax = 0.0
         lonMax = 0.0
         do iCell = 1, nCellsSolve
         do k = 1, nVertLevels
            if (w(k,iCell) < scalar_min) then
               scalar_min = w(k,iCell)
               indexMax = iCell
               kMax = k
               latMax = latCell(iCell)
               lonMax = lonCell(iCell)
            end if
         end do
         end do
         localVals(1) = scalar_min
         localVals(2) = real(indexMax,kind=RKIND)
         localVals(3) = real(kMax,kind=RKIND)
         localVals(4) = latMax
         localVals(5) = lonMax
         call mpas_dmpar_minattributes_real(domain % dminfo, scalar_min, localVals, globalVals)
         global_scalar_min = globalVals(1)
         indexMax_global = int(globalVals(2))
         kMax_global = int(globalVals(3))
         latMax_global = globalVals(4)
         lonMax_global = globalVals(5)
         latMax_global = latMax_global * 180.0_RKIND / pi_const
         lonMax_global = lonMax_global * 180.0_RKIND / pi_const
         if (lonMax_global > 180.0) then
            lonMax_global = lonMax_global - 360.0
         end if
         ! format statement should be '(a,f9.4,a,i4,a,f7.3,a,f8.3,a)'
         call mpas_log_write(' global min w: $r k=$i, $r lat, $r lon', intArgs=(/kMax_global/), &
                             realArgs=(/global_scalar_min, latMax_global, lonMax_global/))

         scalar_max = -1.0e20
         indexMax = -1
         kMax = -1
         latMax = 0.0
         lonMax = 0.0
         do iCell = 1, nCellsSolve
         do k = 1, nVertLevels
            if (w(k,iCell) > scalar_max) then
               scalar_max = w(k,iCell)
               indexMax = iCell
               kMax = k
               latMax = latCell(iCell)
               lonMax = lonCell(iCell)
            end if
         end do
         end do
         localVals(1) = scalar_max
         localVals(2) = real(indexMax,kind=RKIND)
         localVals(3) = real(kMax,kind=RKIND)
         localVals(4) = latMax
         localVals(5) = lonMax
         call mpas_dmpar_maxattributes_real(domain % dminfo, scalar_max, localVals, globalVals)
         global_scalar_max = globalVals(1)
         indexMax_global = int(globalVals(2))
         kMax_global = int(globalVals(3))
         latMax_global = globalVals(4)
         lonMax_global = globalVals(5)
         latMax_global = latMax_global * 180.0_RKIND / pi_const
         lonMax_global = lonMax_global * 180.0_RKIND / pi_const
         if (lonMax_global > 180.0) then
            lonMax_global = lonMax_global - 360.0
         end if
         ! format statement should be '(a,f9.4,a,i4,a,f7.3,a,f8.3,a)'
         call mpas_log_write(' global max w: $r k=$i, $r lat, $r lon', intArgs=(/kMax_global/), &
                             realArgs=(/global_scalar_max, latMax_global, lonMax_global/))

         scalar_min = 1.0e20
         indexMax = -1
         kMax = -1
         latMax = 0.0
         lonMax = 0.0
         do iEdge = 1, nEdgesSolve
         do k = 1, nVertLevels
            if (u(k,iEdge) < scalar_min) then
               scalar_min = u(k,iEdge)
               indexMax = iEdge
               kMax = k
               latMax = latEdge(iEdge)
               lonMax = lonEdge(iEdge)
            end if
         end do
         end do
         localVals(1) = scalar_min
         localVals(2) = real(indexMax,kind=RKIND)
         localVals(3) = real(kMax,kind=RKIND)
         localVals(4) = latMax
         localVals(5) = lonMax
         call mpas_dmpar_minattributes_real(domain % dminfo, scalar_min, localVals, globalVals)
         global_scalar_min = globalVals(1)
         indexMax_global = int(globalVals(2))
         kMax_global = int(globalVals(3))
         latMax_global = globalVals(4)
         lonMax_global = globalVals(5)
         latMax_global = latMax_global * 180.0_RKIND / pi_const
         lonMax_global = lonMax_global * 180.0_RKIND / pi_const
         if (lonMax_global > 180.0) then
            lonMax_global = lonMax_global - 360.0
         end if
         ! format statement should be '(a,f9.4,a,i4,a,f7.3,a,f8.3,a)'
         call mpas_log_write(' global min u: $r k=$i, $r lat, $r lon', intArgs=(/kMax_global/), &
                             realArgs=(/global_scalar_min, latMax_global, lonMax_global/))

         scalar_max = -1.0e20
         indexMax = -1
         kMax = -1
         latMax = 0.0
         lonMax = 0.0
         do iEdge = 1, nEdgesSolve
         do k = 1, nVertLevels
            if (u(k,iEdge) > scalar_max) then
               scalar_max = u(k,iEdge)
               indexMax = iEdge
               kMax = k
               latMax = latEdge(iEdge)
               lonMax = lonEdge(iEdge)
            end if
         end do
         end do
         localVals(1) = scalar_max
         localVals(2) = real(indexMax,kind=RKIND)
         localVals(3) = real(kMax,kind=RKIND)
         localVals(4) = latMax
         localVals(5) = lonMax
         call mpas_dmpar_maxattributes_real(domain % dminfo, scalar_max, localVals, globalVals)
         global_scalar_max = globalVals(1)
         indexMax_global = int(globalVals(2))
         kMax_global = int(globalVals(3))
         latMax_global = globalVals(4)
         lonMax_global = globalVals(5)
         latMax_global = latMax_global * 180.0_RKIND / pi_const
         lonMax_global = lonMax_global * 180.0_RKIND / pi_const
         if (lonMax_global > 180.0) then
            lonMax_global = lonMax_global - 360.0
         end if
         ! format statement should be '(a,f9.4,a,i4,a,f7.3,a,f8.3,a)'
         call mpas_log_write(' global max u: $r k=$i, $r lat, $r lon', intArgs=(/kMax_global/), &
                             realArgs=(/global_scalar_max, latMax_global, lonMax_global/))

         scalar_max = -1.0e20
         indexMax = -1
         kMax = -1
         latMax = 0.0
         lonMax = 0.0
         do iEdge = 1, nEdgesSolve
         do k = 1, nVertLevels
            spd = sqrt(u(k,iEdge)*u(k,iEdge) + v(k,iEdge)*v(k,iEdge))
            if (spd > scalar_max) then
               scalar_max = spd
               indexMax = iEdge
               kMax = k
               latMax = latEdge(iEdge)
               lonMax = lonEdge(iEdge)
            end if
         end do
         end do
         localVals(1) = scalar_max
         localVals(2) = real(indexMax,kind=RKIND)
         localVals(3) = real(kMax,kind=RKIND)
         localVals(4) = latMax
         localVals(5) = lonMax
         call mpas_dmpar_maxattributes_real(domain % dminfo, scalar_max, localVals, globalVals)
         global_scalar_max = globalVals(1)
         indexMax_global = int(globalVals(2))
         kMax_global = int(globalVals(3))
         latMax_global = globalVals(4)
         lonMax_global = globalVals(5)
         latMax_global = latMax_global * 180.0_RKIND / pi_const
         lonMax_global = lonMax_global * 180.0_RKIND / pi_const
         if (lonMax_global > 180.0) then
            lonMax_global = lonMax_global - 360.0
         end if
         ! format statement should be '(a,f9.4,a,i4,a,f7.3,a,f8.3,a)'
         call mpas_log_write(' global max wsp: $r k=$i, $r lat, $r lon', intArgs=(/kMax_global/), &
                             realArgs=(/global_scalar_max, latMax_global, lonMax_global/))

         !
         ! Check for NaNs
         !
         do iCell = 1, nCellsSolve
         do k = 1, nVertLevels
            if (ieee_is_nan(w(k,iCell))) then
               call mpas_log_write('NaN detected in ''w'' field.', messageType=MPAS_LOG_CRIT)
            end if
         end do
         end do

         do iEdge = 1, nEdgesSolve
         do k = 1, nVertLevels
            if (ieee_is_nan(u(k,iEdge))) then
               call mpas_log_write('NaN detected in ''u'' field.', messageType=MPAS_LOG_CRIT)
            end if
         end do
         end do

      else if (config_print_global_minmax_vel) then
         call mpas_log_write('')

         call mpas_pool_get_subpool(block % structs, 'state', state)
         call mpas_pool_get_array(state, 'w', w, 2)
         call mpas_pool_get_array(state, 'u', u, 2)
         call mpas_pool_get_dimension(state, 'nCellsSolve', nCellsSolve)
         call mpas_pool_get_dimension(state, 'nEdgesSolve', nEdgesSolve)
         call mpas_pool_get_dimension(state, 'nVertLevels', nVertLevels)

         scalar_min = 0.0
         scalar_max = 0.0
         do iCell = 1, nCellsSolve
         do k = 1, nVertLevels
            scalar_min = min(scalar_min, w(k,iCell))
            scalar_max = max(scalar_max, w(k,iCell))
         end do
         end do
         call mpas_dmpar_min_real(domain % dminfo, scalar_min, global_scalar_min)
         call mpas_dmpar_max_real(domain % dminfo, scalar_max, global_scalar_max)
         call mpas_log_write('global min, max w $r $r', realArgs=(/global_scalar_min, global_scalar_max/))

         scalar_min = 0.0
         scalar_max = 0.0
         do iEdge = 1, nEdgesSolve
         do k = 1, nVertLevels
            scalar_min = min(scalar_min, u(k,iEdge))
            scalar_max = max(scalar_max, u(k,iEdge))
         end do
         end do
         call mpas_dmpar_min_real(domain % dminfo, scalar_min, global_scalar_min)
         call mpas_dmpar_max_real(domain % dminfo, scalar_max, global_scalar_max)
         call mpas_log_write('global min, max u $r $r', realArgs=(/global_scalar_min, global_scalar_max/))
      end if

      if (config_print_global_minmax_sca) then
         if (.not. (config_print_global_minmax_vel .or. config_print_detailed_minmax_vel)) then
            call mpas_log_write('')
         end if

         call mpas_pool_get_subpool(block % structs, 'state', state)
         call mpas_pool_get_array(state, 'scalars', scalars, 2)
         call mpas_pool_get_dimension(state, 'nCellsSolve', nCellsSolve)
         call mpas_pool_get_dimension(state, 'nVertLevels', nVertLevels)
         call mpas_pool_get_dimension(state, 'num_scalars', num_scalars)

         do iScalar = 1, num_scalars
            scalar_min = 0.0
            scalar_max = 0.0
            do iCell = 1, nCellsSolve
            do k = 1, nVertLevels
               scalar_min = min(scalar_min, scalars(iScalar,k,iCell))
               scalar_max = max(scalar_max, scalars(iScalar,k,iCell))
            end do
            end do
            call mpas_dmpar_min_real(domain % dminfo, scalar_min, global_scalar_min)
            call mpas_dmpar_max_real(domain % dminfo, scalar_max, global_scalar_max)
            call mpas_log_write(' global min, max scalar $i $r $r', intArgs=(/iScalar/), realArgs=(/global_scalar_min, global_scalar_max/))
         end do
      end if

   end subroutine summarize_timestep


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   !  Accumulate tendencies for initial tendency diagnostics and PV tendency
   !  diagnostics
   !
   !  Author: May Wong (mwong@ucar.edu)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine atm_accumulate_tend( state, diag, mesh, configs, nCells, nVertLevels, &
                                   cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd, &
                                   cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd, &
                                   dt_dyn, dynamics_substep, dynamics_split)
      implicit none

      type (mpas_pool_type), intent(inout) :: state
      type (mpas_pool_type), intent(inout) :: diag
      type (mpas_pool_type), intent(inout) :: mesh
      type (mpas_pool_type), intent(in)    :: configs
      integer, intent(in) :: nCells                  ! for allocating stack variables
      integer, intent(in) :: nVertLevels             ! for allocating stack variables
      integer, intent(in) :: cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd
      integer, intent(in) :: cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd
      real (kind=RKIND), intent(in) :: dt_dyn
      integer, intent(in) :: dynamics_substep, dynamics_split

      integer, pointer :: nCellsSolve, nEdges
      integer, dimension(:,:), pointer :: cellsOnEdge

      real (kind=RKIND), dimension(:), pointer :: fzm, fzp
      real (kind=RKIND), dimension(:,:), pointer :: zz
      real (kind=RKIND), dimension(:,:), pointer :: u_1, w_1
      real (kind=RKIND), dimension(:,:), pointer :: rho_zz, rho_zz_1

      ! For tendency diagnostics (also used for PV diagnostics):
      real (kind=RKIND), dimension(:,:), pointer :: ru_tend_dyn_small, ru_tend_dyn_large, ru_tend_diff, &
                                                    ru_tend_physics, rublten_tend, rugwdo_tend, rucuten_tend, &
                                                    ru_tend_smdiv, u_tend_dcpl
      real (kind=RKIND), dimension(:,:), pointer :: rth_tend_dyn_small, rth_tend_dyn_large, rth_tend_diff, &
                                                    rth_tend_physics, rthblten_tend, rthcuten_tend, rthratensw_tend, rthratenlw_tend, &
                                                    th_tend_dcpl
      real (kind=RKIND), dimension(:,:), pointer :: rqv_tend_diff, qvblten_tend, qvcuten_tend
      real (kind=RKIND), dimension(:,:), pointer :: acc_u_tend_dyn_small, acc_u_tend_dyn_large, acc_u_tend_diff, &
                                                    acc_u_tend_physics, acc_ublten, acc_ugwdoten, acc_ucuten, acc_u_tend_smdiv, &
                                                    acc_u_tend_dcpl
      real (kind=RKIND), dimension(:,:), pointer :: acc_th_tend_dyn_small, acc_th_tend_dyn_large, acc_th_tend_diff, &
                                                    acc_th_tend_physics, acc_thblten, acc_thcuten, acc_thratensw, acc_thratenlw, &
                                                    acc_th_tend_dcpl
      real (kind=RKIND), dimension(:,:), pointer :: acc_qv_tend_dyn_large, acc_qv_tend_diff, acc_qvblten, acc_qvcuten

      ! For PV diagnostics: 
      logical, pointer :: config_pv_tend

      real (kind=RKIND), dimension(:,:), pointer :: u_tend_diff, w_tend_diff
      real (kind=RKIND), dimension(:,:), pointer :: rw_tend_dyn_large, rw_tend_dyn_small, rw_tend_diff, w_tend_dcpl
      real (kind=RKIND), dimension(:,:), pointer :: du_dt_dyn, dw_dt_dyn
      real (kind=RKIND), dimension(:,:), pointer :: dthetam_dt_dyn, dthetam_dt_mix
      real (kind=RKIND), dimension(:,:), pointer :: dqv_dt_dyn ! Added for PV 

      ! PV -- physics tendencies:
      real (kind=RKIND), dimension(:,:), pointer :: tend_u_pbl, tend_u_cu
      real (kind=RKIND), dimension(:,:), pointer :: thmblten, qvblten, thmcuten, qvcuten
      real (kind=RKIND), dimension(:,:), pointer :: thmswten, thmlwten

      call mpas_pool_get_dimension(mesh, 'nEdges', nEdges)
      call mpas_pool_get_dimension(mesh, 'nCellsSolve', nCellsSolve)
      call mpas_pool_get_array(mesh, 'cellsOnEdge', cellsOnEdge)

      ! MW: get fzm and fzp for computing rho_zz at w-levels for PV diagnostics
      call mpas_pool_get_array(mesh, 'fzm', fzm)
      call mpas_pool_get_array(mesh, 'fzp', fzp)
      call mpas_pool_get_array(mesh, 'zz', zz)

      call mpas_pool_get_array(state, 'w', w_1, 1)
      call mpas_pool_get_array(state, 'u', u_1, 1)
      call mpas_pool_get_array(state, 'rho_zz', rho_zz_1, 1) ! for debugging 
      call mpas_pool_get_array(state, 'rho_zz', rho_zz, 2) ! for debugging 

      call mpas_pool_get_array( diag, 'ru_tend_dyn_small', ru_tend_dyn_small)
      call mpas_pool_get_array( diag, 'ru_tend_dyn_large', ru_tend_dyn_large)
      call mpas_pool_get_array( diag, 'ru_tend_diff', ru_tend_diff)
      call mpas_pool_get_array( diag, 'ru_tend_physics', ru_tend_physics)
      call mpas_pool_get_array( diag, 'rublten_tend', rublten_tend)
      call mpas_pool_get_array( diag, 'rugwdo_tend', rugwdo_tend)
      call mpas_pool_get_array( diag, 'rucuten_tend', rucuten_tend)
      call mpas_pool_get_array( diag, 'ru_tend_smdiv', ru_tend_smdiv)
      call mpas_pool_get_array( diag, 'u_tend_dcpl', u_tend_dcpl)

      call mpas_pool_get_array( diag, 'acc_u_tend_dyn_small', acc_u_tend_dyn_small)
      call mpas_pool_get_array( diag, 'acc_u_tend_dyn_large', acc_u_tend_dyn_large)
      call mpas_pool_get_array( diag, 'acc_u_tend_diff', acc_u_tend_diff)
      call mpas_pool_get_array( diag, 'acc_u_tend_physics', acc_u_tend_physics)
      call mpas_pool_get_array( diag, 'acc_ublten', acc_ublten )
      call mpas_pool_get_array( diag, 'acc_ugwdoten', acc_ugwdoten )
      call mpas_pool_get_array( diag, 'acc_ucuten', acc_ucuten )
      call mpas_pool_get_array( diag, 'acc_u_tend_smdiv', acc_u_tend_smdiv)
      call mpas_pool_get_array( diag, 'acc_u_tend_dcpl', acc_u_tend_dcpl)

      call mpas_pool_get_array( diag, 'rth_tend_dyn_small', rth_tend_dyn_small)
      call mpas_pool_get_array( diag, 'rth_tend_dyn_large', rth_tend_dyn_large)
      call mpas_pool_get_array( diag, 'rth_tend_diff', rth_tend_diff)
      call mpas_pool_get_array( diag, 'rth_tend_physics', rth_tend_physics)
      call mpas_pool_get_array( diag, 'rthblten_tend', rthblten_tend)
      call mpas_pool_get_array( diag, 'rthcuten_tend', rthcuten_tend)
      call mpas_pool_get_array( diag, 'rthratensw_tend', rthratensw_tend)
      call mpas_pool_get_array( diag, 'rthratenlw_tend', rthratenlw_tend)
      call mpas_pool_get_array( diag, 'th_tend_dcpl', th_tend_dcpl)

      !MW note: acc_th_tend_diabatic is accumulated in physics/mpas_atmphys_interface.F
      call mpas_pool_get_array( diag, 'acc_th_tend_dyn_small', acc_th_tend_dyn_small)
      call mpas_pool_get_array( diag, 'acc_th_tend_dyn_large', acc_th_tend_dyn_large)
      call mpas_pool_get_array( diag, 'acc_th_tend_diff', acc_th_tend_diff)
      call mpas_pool_get_array( diag, 'acc_th_tend_physics', acc_th_tend_physics)
      call mpas_pool_get_array( diag, 'acc_thblten', acc_thblten)
      call mpas_pool_get_array( diag, 'acc_thcuten', acc_thcuten)
      call mpas_pool_get_array( diag, 'acc_thratensw', acc_thratensw)
      call mpas_pool_get_array( diag, 'acc_thratenlw', acc_thratenlw)
      call mpas_pool_get_array( diag, 'acc_th_tend_dcpl', acc_th_tend_dcpl)

      call mpas_pool_get_array( diag, 'rqv_tend_diff', rqv_tend_diff)
      call mpas_pool_get_array( diag, 'qvblten_tend', qvblten_tend)
      call mpas_pool_get_array( diag, 'qvcuten_tend', qvcuten_tend)

      !MW note: acc_qv_mp is accumulated in physics/mpas_atmphys_interface.F
      call mpas_pool_get_array( diag, 'acc_qv_tend_dyn_large', acc_qv_tend_dyn_large)
      call mpas_pool_get_array( diag, 'acc_qv_tend_diff', acc_qv_tend_diff)
      call mpas_pool_get_array( diag, 'acc_qvblten', acc_qvblten)
      call mpas_pool_get_array( diag, 'acc_qvcuten', acc_qvcuten)

      ! For PV diagnostics: 
      call mpas_pool_get_config(configs, 'config_pv_tend', config_pv_tend)
      if (config_pv_tend) then
        call mpas_pool_get_array( diag, 'u_tend_diff', u_tend_diff)
        call mpas_pool_get_array( diag, 'w_tend_diff', w_tend_diff)
        call mpas_pool_get_array( diag, 'rw_tend_diff', rw_tend_diff)
        call mpas_pool_get_array( diag, 'rw_tend_dyn_large', rw_tend_dyn_large)
        call mpas_pool_get_array( diag, 'rw_tend_dyn_small', rw_tend_dyn_small)
        call mpas_pool_get_array( diag, 'w_tend_dcpl', w_tend_dcpl)
        call mpas_pool_get_array( diag, 'dthetam_dt_dyn', dthetam_dt_dyn)
        call mpas_pool_get_array( diag, 'dthetam_dt_mix', dthetam_dt_mix)
        call mpas_pool_get_array( diag, 'dqv_dt_dyn', dqv_dt_dyn)
        call mpas_pool_get_array( diag, 'du_dt_dyn', du_dt_dyn)
        call mpas_pool_get_array( diag, 'dw_dt_dyn', dw_dt_dyn)

        ! Physics for PV: 
        call mpas_pool_get_array( diag, 'tend_u_cu', tend_u_cu)
        call mpas_pool_get_array( diag, 'tend_u_pbl', tend_u_pbl)
        call mpas_pool_get_array( diag, 'thmblten', thmblten)
        call mpas_pool_get_array( diag, 'qvblten', qvblten)
        call mpas_pool_get_array( diag, 'thmcuten', thmcuten)
        call mpas_pool_get_array( diag, 'qvcuten', qvcuten)
        call mpas_pool_get_array( diag, 'thmswten', thmswten)
        call mpas_pool_get_array( diag, 'thmlwten', thmlwten)
      else
        allocate(u_tend_diff(nVertLevels,nEdges+1))
        allocate(w_tend_diff(nVertLevels+1,nCells+1))
        allocate(rw_tend_diff(nVertLevels+1,nCells+1))
        allocate(rw_tend_dyn_large(nVertLevels+1,nCells+1))
        allocate(rw_tend_dyn_small(nVertLevels+1,nCells+1))
        allocate(w_tend_dcpl(nVertLevels+1,nCells+1))
        allocate(dthetam_dt_dyn(nVertLevels,nCells+1))
        allocate(dthetam_dt_mix(nVertLevels,nCells+1))
        allocate(dqv_dt_dyn(nVertLevels,nCells+1))
        allocate(du_dt_dyn(nVertLevels,nEdges+1))
        allocate(dw_dt_dyn(nVertLevels+1,nCells+1))

        allocate(tend_u_cu(nVertLevels,nEdges+1))
        allocate(tend_u_pbl(nVertLevels,nEdges+1))
        allocate(thmblten(nVertLevels,nCells+1))
        allocate(qvblten(nVertLevels,nCells+1))
        allocate(thmcuten(nVertLevels,nCells+1))
        allocate(qvcuten(nVertLevels,nCells+1))
        allocate(thmswten(nVertLevels,nCells+1))
        allocate(thmlwten(nVertLevels,nCells+1))
      end if

      call atm_accumulate_tend_work(nCells, nEdges, nCellsSolve, cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd, &
                                cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd, &
                                fzm, fzp, zz, &
                                ru_tend_dyn_small, ru_tend_dyn_large, ru_tend_diff, ru_tend_physics, &
                                rublten_tend, rugwdo_tend, rucuten_tend, ru_tend_smdiv, u_tend_dcpl, &
                                rth_tend_dyn_small, rth_tend_dyn_large, rth_tend_diff, rth_tend_physics, &
                                rthblten_tend, rthcuten_tend, rthratensw_tend, rthratenlw_tend, th_tend_dcpl, &
                                rqv_tend_diff, qvblten_tend, qvcuten_tend, &
                                acc_u_tend_dyn_small, acc_u_tend_dyn_large, acc_u_tend_diff, acc_u_tend_physics, &
                                acc_ublten, acc_ugwdoten, acc_ucuten, acc_u_tend_smdiv, acc_u_tend_dcpl, &
                                acc_th_tend_dyn_small, acc_th_tend_dyn_large, acc_th_tend_diff, acc_th_tend_physics, &
                                acc_thblten, acc_thcuten, acc_thratensw, acc_thratenlw, &
                                acc_th_tend_dcpl, &
                                acc_qv_tend_dyn_large, acc_qv_tend_diff, acc_qvblten, acc_qvcuten, &
                                cellsOnEdge, dt_dyn, dynamics_substep, dynamics_split, rho_zz_1, rho_zz, &
                                config_pv_tend, &
                                u_tend_diff=u_tend_diff, w_tend_diff=w_tend_diff, rw_tend_dyn_small=rw_tend_dyn_small, rw_tend_dyn_large=rw_tend_dyn_large, &       ! MW: Added for PV
                                rw_tend_diff=rw_tend_diff, w_tend_dcpl=w_tend_dcpl, &                                                                               ! MW: Added for PV
                                dthetam_dt_dyn=dthetam_dt_dyn, dthetam_dt_mix=dthetam_dt_mix, dqv_dt_dyn=dqv_dt_dyn, du_dt_dyn=du_dt_dyn, dw_dt_dyn=dw_dt_dyn, &    ! MW: Added for PV
                                tend_u_cu=tend_u_cu, tend_u_pbl=tend_u_pbl, thmblten=thmblten, qvblten=qvblten, thmcuten=thmcuten, qvcuten=qvcuten, &               ! MC: Added for PV
                                thmswten=thmswten, thmlwten=thmlwten )


        ! MC: deallocate PV variables if allocated above                
        if (.not. config_pv_tend) then
           deallocate(u_tend_diff)
           deallocate(w_tend_diff)
           deallocate(rw_tend_diff)
           deallocate(rw_tend_dyn_large)
           deallocate(rw_tend_dyn_small)
           deallocate(w_tend_dcpl)
           deallocate(dthetam_dt_dyn)
           deallocate(dthetam_dt_mix)
           deallocate(dqv_dt_dyn)
           deallocate(du_dt_dyn)
           deallocate(dw_dt_dyn)

           deallocate(tend_u_cu)
           deallocate(tend_u_pbl)
           deallocate(thmblten)
           deallocate(qvblten)
           deallocate(thmcuten)
           deallocate(qvcuten)
           deallocate(thmswten)
           deallocate(thmlwten)
        end if


   end subroutine atm_accumulate_tend



   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   !  Accumulate tendencies for initial tendency diagnostics and PV tendency
   !  diagnostics
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine atm_accumulate_tend_work(nCells, nEdges, nCellsSolve, cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd, &
                                cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd, &
                                fzm, fzp, zz, &
                                ru_tend_dyn_small, ru_tend_dyn_large, ru_tend_diff, ru_tend_physics, &
                                rublten_tend, rugwdo_tend, rucuten_tend, ru_tend_smdiv, u_tend_dcpl, &
                                rth_tend_dyn_small, rth_tend_dyn_large, rth_tend_diff, rth_tend_physics, &
                                rthblten_tend, rthcuten_tend, rthratensw_tend, rthratenlw_tend, th_tend_dcpl, &
                                rqv_tend_diff, qvblten_tend, qvcuten_tend, &
                                acc_u_tend_dyn_small, acc_u_tend_dyn_large, acc_u_tend_diff, acc_u_tend_physics, &
                                acc_ublten, acc_ugwdoten, acc_ucuten, acc_u_tend_smdiv, acc_u_tend_dcpl, &
                                acc_th_tend_dyn_small, acc_th_tend_dyn_large, acc_th_tend_diff, acc_th_tend_physics, &
                                acc_thblten, acc_thcuten, acc_thratensw, acc_thratenlw, &
                                acc_th_tend_dcpl, &
                                acc_qv_tend_dyn_large, acc_qv_tend_diff, acc_qvblten, acc_qvcuten, &
                                cellsOnEdge, dt_dyn, dynamics_substep, dynamics_split, rho_zz_1, rho_zz, &
                                config_pv_tend, &
                                u_tend_diff, w_tend_diff, rw_tend_dyn_small, rw_tend_dyn_large, rw_tend_diff, w_tend_dcpl, &  ! MW: Added for PV
                                dthetam_dt_dyn, dthetam_dt_mix, dqv_dt_dyn, du_dt_dyn, dw_dt_dyn, &                           ! MW: Added for PV
                                tend_u_cu, tend_u_pbl, thmblten, qvblten, thmcuten, qvcuten, &                                ! MC: Added for PV
                                thmswten, thmlwten )

      use mpas_atm_dimensions

      implicit none
      !
      ! Dummy arguments
      !
      integer, intent(in) :: nCells, nEdges, nCellsSolve
      integer, intent(in) :: cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd
      integer, intent(in) :: cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd

      real (kind=RKIND), dimension(nVertLevels), intent(in) :: fzm
      real (kind=RKIND), dimension(nVertLevels), intent(in) :: fzp
      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(in) :: zz
      real (kind=RKIND), dimension(nVertLevels,nEdges+1), optional :: u_tend_diff
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1), optional :: w_tend_diff, rw_tend_dyn_small, rw_tend_dyn_large, rw_tend_diff, w_tend_dcpl

      real (kind=RKIND), intent(in) :: dt_dyn
      integer, intent(in) :: dynamics_substep, dynamics_split

      ! Local variables
      integer :: iEdge, k, cell1, cell2
      integer :: iCell
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: ru_tend_dyn_small, ru_tend_dyn_large, ru_tend_diff, &
                                                            ru_tend_physics, rublten_tend, rugwdo_tend, rucuten_tend, &
                                                            ru_tend_smdiv, u_tend_dcpl
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: acc_u_tend_dyn_small, acc_u_tend_dyn_large, acc_u_tend_diff, &
                                                            acc_u_tend_physics, acc_ublten, acc_ugwdoten, acc_ucuten, acc_u_tend_smdiv, &
                                                            acc_u_tend_dcpl
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rth_tend_dyn_small, rth_tend_dyn_large, rth_tend_diff, &
                                                            rth_tend_physics, rthblten_tend, rthcuten_tend, rthratensw_tend, rthratenlw_tend, &
                                                            th_tend_dcpl
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: acc_th_tend_dyn_small, acc_th_tend_dyn_large, acc_th_tend_diff, &
                                                            acc_th_tend_physics, acc_thblten, acc_thcuten, acc_thratensw, acc_thratenlw, &
                                                            acc_th_tend_dcpl
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rqv_tend_diff, qvblten_tend, qvcuten_tend
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: acc_qv_tend_dyn_large, acc_qv_tend_diff, &
                                                            acc_qvblten, acc_qvcuten

      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rho_zz_1, rho_zz

      ! Adding for PV
      logical, intent(in) :: config_pv_tend
      real (kind=RKIND), dimension(nVertLevels,nCells+1), optional :: dthetam_dt_dyn, dthetam_dt_mix, dqv_dt_dyn   ! adding for PV
      real (kind=RKIND), dimension(nVertLevels,nEdges+1), optional :: du_dt_dyn                                    ! adding for PV
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1), optional :: dw_dt_dyn                                  ! adding for PV
      real (kind=RKIND), dimension(nVertLevels,nEdges+1), optional :: tend_u_pbl, tend_u_cu                        ! adding for PV
      real (kind=RKIND), dimension(nVertLevels,nCells+1), optional :: thmblten, qvblten, thmcuten, qvcuten         ! adding for PV
      real (kind=RKIND), dimension(nVertLevels,nCells+1), optional :: thmswten, thmlwten                           ! adding for PV

      integer, dimension(2,nEdges+1) :: cellsOnEdge
      real (kind=RKIND) :: drho_zz, drho_zz_W

      !MW: Reset if at the beginning of dynamics_split (adding for PV friction term)
      if ( (dynamics_substep == 1)  .and. config_pv_tend ) then
         do iEdge=edgeStart,edgeEnd
            cell1 = cellsOnEdge(1,iEdge)
            cell2 = cellsOnEdge(2,iEdge)

            ! update edges for block-owned cells
            if (cell1 <= nCellsSolve .or. cell2 <= nCellsSolve ) then
!DIR$ IVDEP
               do k=1,nVertLevels
                  u_tend_diff(k,iEdge)  = 0.0
                  tend_u_pbl(k,iEdge)   = 0.0   ! MC added MC_TODO: check if these are needed here...
                  tend_u_cu(k,iEdge)    = 0.0   ! MC added
                  du_dt_dyn(k,iEdge)    = 0.0   ! MC initializing this here instead of in PV code
               enddo
            end if
         end do

         do iCell=cellSolveStart,cellSolveEnd  ! loop over all owned cells to solve
!DIR$ IVDEP
           ! MC_TODO: these are also initialized in PV code. only needs to be done once
           do k=1,nVertLevels
              w_tend_diff(k,iCell)     = 0.0
              dw_dt_dyn(k,iCell)       = 0.0
              dthetam_dt_dyn(k,iCell)  = 0.0
              dthetam_dt_mix(k,iCell)  = 0.0
              thmblten(k,iCell)        = 0.0
              qvblten(k,iCell)         = 0.0
              thmcuten(k,iCell)        = 0.0
              qvcuten(k,iCell)         = 0.0
              thmswten(k,iCell)        = 0.0
              thmlwten(k,iCell)        = 0.0
           end do
           w_tend_diff(nVertLevels+1,iCell)    = 0.0
           dw_dt_dyn(nVertLevels+1,iCell)      = 0.0
         end do
      end if


      do iEdge=edgeStart,edgeEnd ! MGD do we really just need edges touching owned cells?
         cell1 = cellsOnEdge(1,iEdge)
         cell2 = cellsOnEdge(2,iEdge)

         ! update edges for block-owned cells
         if (cell1 <= nCellsSolve .or. cell2 <= nCellsSolve ) then
!DIR$ IVDEP
            do k=1,nVertLevels

               drho_zz = 2./(rho_zz_1(k,cell1)+rho_zz_1(k,cell2))

               ! note: individual physics terms are already tendencies; other tendencies are integrated over dt_dyn
               acc_u_tend_dyn_small(k,iEdge) = acc_u_tend_dyn_small(k,iEdge) + ru_tend_dyn_small(k,iEdge)/(dt_dyn*dynamics_split)*drho_zz
               acc_u_tend_dyn_large(k,iEdge) = acc_u_tend_dyn_large(k,iEdge) + ru_tend_dyn_large(k,iEdge)/(dt_dyn*dynamics_split)*drho_zz
               acc_u_tend_diff(k,iEdge)      = acc_u_tend_diff(k,iEdge) + ru_tend_diff(k,iEdge)/(dt_dyn*dynamics_split)*drho_zz
               acc_u_tend_physics(k,iEdge)   = acc_u_tend_physics(k,iEdge) + ru_tend_physics(k,iEdge)/(dt_dyn*dynamics_split)*drho_zz
               acc_ublten(k,iEdge)           = acc_ublten(k,iEdge) + rublten_tend(k,iEdge)/dynamics_split*drho_zz
               acc_ugwdoten(k,iEdge)         = acc_ugwdoten(k,iEdge) + rugwdo_tend(k,iEdge)/dynamics_split*drho_zz
               acc_ucuten(k,iEdge)           = acc_ucuten(k,iEdge) + rucuten_tend(k,iEdge)/dynamics_split*drho_zz
               acc_u_tend_smdiv(k,iEdge)     = acc_u_tend_smdiv(k,iEdge) + ru_tend_smdiv(k,iEdge)/(dt_dyn*dynamics_split)*drho_zz
               acc_u_tend_dcpl(k,iEdge)      = acc_u_tend_dcpl(k,iEdge) + u_tend_dcpl(k,iEdge)/(dt_dyn*dynamics_split)

               if (config_pv_tend) then
                  u_tend_diff(k,iEdge)       =    u_tend_diff(k,iEdge) + ru_tend_diff(k,iEdge)/(dt_dyn*dynamics_split)*drho_zz   ! Added for PV

                  ! Adding alternative way to calculate horizontal winds dynamics term using the decoupled ru budget terms, but we don't
                  ! want it accumulated over integration, only over the full model time step
                  ! This term is zero-ed in pv_diagnostics_reset at every time step
                  du_dt_dyn(k,iEdge)         = du_dt_dyn(k,iEdge) + ru_tend_dyn_small(k,iEdge)/(dt_dyn*dynamics_split)*drho_zz &
                                              + ru_tend_dyn_large(k,iEdge)/(dt_dyn*dynamics_split)*drho_zz &
                                              + ru_tend_smdiv(k,iEdge)/(dt_dyn*dynamics_split)*drho_zz &
                                              + u_tend_dcpl(k,iEdge)/(dt_dyn*dynamics_split)

                  ! MC: -- adding for PV friction tends
                  tend_u_pbl(k,iEdge)        =   tend_u_pbl(k,iEdge) + rublten_tend(k,iEdge)/dynamics_split*drho_zz
                  tend_u_cu(k,iEdge)         =   tend_u_cu(k,iEdge) + rucuten_tend(k,iEdge)/dynamics_split*drho_zz
               end if

            end do
         end if ! end test for block-owned cells
      end do ! end loop over edges


      do iCell=cellSolveStart,cellSolveEnd  ! loop over all owned cells to solve

         if (config_pv_tend) then
            dw_dt_dyn(1,iCell)  = dw_dt_dyn(1,iCell) + rw_tend_dyn_small(1,iCell)/(dt_dyn*dynamics_split)  ! this is total tendency of w (see recover_xxx)
         end if        
    
!DIR$ IVDEP
         do k=1,nVertLevels

            drho_zz = 1./rho_zz_1(k,iCell)

            ! theta_m
            ! MW note: acc_th_tend_diabatic is accumulated in physics/mpas_atmphys_interface.F
            acc_th_tend_dyn_small(k,iCell) = acc_th_tend_dyn_small(k,iCell) + rth_tend_dyn_small(k,iCell)/(dt_dyn*dynamics_split)*drho_zz
            acc_th_tend_dyn_large(k,iCell) = acc_th_tend_dyn_large(k,iCell) + rth_tend_dyn_large(k,iCell)/(dt_dyn*dynamics_split)*drho_zz
            acc_th_tend_diff(k,iCell)      = acc_th_tend_diff(k,iCell) + rth_tend_diff(k,iCell)/(dt_dyn*dynamics_split)*drho_zz
            acc_th_tend_physics(k,iCell)   = acc_th_tend_physics(k,iCell) + rth_tend_physics(k,iCell)/(dt_dyn*dynamics_split)*drho_zz
            acc_thblten(k,iCell)           = acc_thblten(k,iCell) + rthblten_tend(k,iCell)/dynamics_split*drho_zz
            acc_thcuten(k,iCell)           = acc_thcuten(k,iCell) + rthcuten_tend(k,iCell)/dynamics_split*drho_zz
            acc_thratensw(k,iCell)         = acc_thratensw(k,iCell) + rthratensw_tend(k,iCell)/dynamics_split*drho_zz
            acc_thratenlw(k,iCell)         = acc_thratenlw(k,iCell) + rthratenlw_tend(k,iCell)/dynamics_split*drho_zz
            acc_th_tend_dcpl(k,iCell)      = acc_th_tend_dcpl(k,iCell) + th_tend_dcpl(k,iCell)/(dt_dyn*dynamics_split)

            ! qv
            acc_qvblten(k,iCell)           = acc_qvblten(k,iCell) + qvblten_tend(k,iCell)/dynamics_split
            acc_qvcuten(k,iCell)           = acc_qvcuten(k,iCell) + qvcuten_tend(k,iCell)/dynamics_split

            ! For PV tendencies:
            if (config_pv_tend) then
               ! MC: theta_m and qv tendencies from physics parameterizations
               ! Note: thmmpten, qvmpten are calculated in physics/mpas_atmphys_interface.F
               qvblten(k,iCell)            = qvblten(k,iCell) + qvblten_tend(k,iCell)/dynamics_split
               thmblten(k,iCell)           = thmblten(k,iCell) + rthblten_tend(k,iCell)/dynamics_split*drho_zz

               qvcuten(k,iCell)            = qvcuten(k,iCell) + qvcuten_tend(k,iCell)/dynamics_split
               thmcuten(k,iCell)           = thmcuten(k,iCell) + rthcuten_tend(k,iCell)/dynamics_split*drho_zz

               ! Shortwave and longwave radiation (no moisture tendencies)
               thmswten(k,iCell)           = thmswten(k,iCell) + rthratensw_tend(k,iCell)/dynamics_split*drho_zz
               thmlwten(k,iCell)           = thmlwten(k,iCell) + rthratenlw_tend(k,iCell)/dynamics_split*drho_zz

               ! Vertical velocity
               if ( k > 1 ) then
                  drho_zz_w                = 1./( fzm(k)*rho_zz_1(k,iCell) + fzp(k)*rho_zz_1(k-1,iCell))
                  dw_dt_dyn(k,iCell)       = dw_dt_dyn(k,iCell) + rw_tend_dyn_small(k,iCell)/(dt_dyn*dynamics_split) - (rw_tend_diff(k,iCell)*drho_zz_w/(dt_dyn*dynamics_split))
                  w_tend_diff(k,iCell)     = w_tend_diff(k,iCell) + rw_tend_diff(k,iCell)*drho_zz_w/(dt_dyn*dynamics_split) ! only for use with PV diagnostics (not accumulated)
               endif

               ! Adding alternative way to calculate theta dynamics term using theta_m budget term, but we don't
               ! want it accumulated over integration, only over the full model time step
               ! This term is zero-ed in pv_diagnostics_reset at every time step
               dthetam_dt_dyn(k,iCell)     = dthetam_dt_dyn(k,iCell) + (rth_tend_dyn_small(k,iCell)/(dt_dyn*dynamics_split)*drho_zz) + &
                                             (rth_tend_dyn_large(k,iCell)/(dt_dyn*dynamics_split)*drho_zz) + &
                                             (th_tend_dcpl(k,iCell)/(dt_dyn*dynamics_split))
               dthetam_dt_mix(k,iCell)     = dthetam_dt_mix(k,iCell) + rth_tend_diff(k,iCell)/(dt_dyn*dynamics_split)*drho_zz

           end if   ! config_pv_tend
         end do     ! end loop over vertical levels
      end do        ! end of loop over cells

   end subroutine atm_accumulate_tend_work

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   !  For tendency diagnostics: reconstruct horizontal momentum tendencies to its
   !  zonal/meridional components and return at cell centers
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine atm_reconstruct_tend(domain, diag, mesh, configs, nCells, nVertLevels, &
                                   cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd, &
                                   cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd, &
                                   dt_dyn, dynamics_substep, dynamics_split, &
                                   exchange_halo_group)

      implicit none

      type (domain_type), intent(inout) :: domain    ! MC added for new halo
      type (mpas_pool_type), intent(inout) :: diag
      type (mpas_pool_type), intent(inout) :: mesh
      type (mpas_pool_type), intent(in)    :: configs
      integer, intent(in) :: nCells                  ! for allocating stack variables
      integer, intent(in) :: nVertLevels             ! for allocating stack variables
      integer, intent(in) :: cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd
      integer, intent(in) :: cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd
      real (kind=RKIND), intent(in) :: dt_dyn
      integer, intent(in) :: dynamics_substep, dynamics_split
      procedure (halo_exchange_routine) :: exchange_halo_group     ! MC added

      real (kind=RKIND), dimension(:,:), pointer :: ru_tend_dyn_small, ru_tend_dyn_large, ru_tend_diff, &
                                                    ru_tend_physics, rublten_tend, rucuten_tend, &
                                                    ru_tend_smdiv, u_tend_dcpl
      real (kind=RKIND), dimension(:,:), pointer :: u_1, u_2
      real (kind=RKIND), dimension(:,:), pointer :: rho_zz
      real (kind=RKIND), dimension(:,:), pointer :: acc_u_tend_dyn_small, &
                                                    acc_u_tend_dyn_small_ReconstructX, acc_u_tend_dyn_small_ReconstructY, &
                                                    acc_u_tend_dyn_small_ReconstructZ, &
                                                    acc_u_tend_dyn_small_ReconstructZonal, acc_u_tend_dyn_small_ReconstructMeridional, &
                                                    acc_u_tend_dyn_large, &
                                                    acc_u_tend_dyn_large_ReconstructX, acc_u_tend_dyn_large_ReconstructY, &
                                                    acc_u_tend_dyn_large_ReconstructZ, &
                                                    acc_u_tend_dyn_large_ReconstructZonal, acc_u_tend_dyn_large_ReconstructMeridional, &
                                                    acc_u_tend_diff, &
                                                    acc_u_tend_diff_ReconstructX, acc_u_tend_diff_ReconstructY, &
                                                    acc_u_tend_diff_ReconstructZ, &
                                                    acc_u_tend_diff_ReconstructZonal, acc_u_tend_diff_ReconstructMeridional, &
                                                    acc_u_tend_physics, &
                                                    acc_u_tend_physics_ReconstructX, acc_u_tend_physics_ReconstructY, &
                                                    acc_u_tend_physics_ReconstructZ, &
                                                    acc_u_tend_physics_ReconstructZonal, acc_u_tend_physics_ReconstructMeridional, &
                                                    acc_ublten, &
                                                    acc_ublten_ReconstructX, acc_ublten_ReconstructY, &
                                                    acc_ublten_ReconstructZ, &
                                                    acc_ublten_ReconstructZonal, acc_ublten_ReconstructMeridional, &
                                                    acc_ugwdoten, &
                                                    acc_ugwdoten_ReconstructX, acc_ugwdoten_ReconstructY, &
                                                    acc_ugwdoten_ReconstructZ, &
                                                    acc_ugwdoten_ReconstructZonal, acc_ugwdoten_ReconstructMeridional, &
                                                    acc_ucuten, &
                                                    acc_ucuten_ReconstructX, acc_ucuten_ReconstructY, &
                                                    acc_ucuten_ReconstructZ, &
                                                    acc_ucuten_ReconstructZonal, acc_ucuten_ReconstructMeridional, &
                                                    acc_u_tend_smdiv, &
                                                    acc_u_tend_smdiv_ReconstructX, acc_u_tend_smdiv_ReconstructY, &
                                                    acc_u_tend_smdiv_ReconstructZ, &
                                                    acc_u_tend_smdiv_ReconstructZonal, acc_u_tend_smdiv_ReconstructMeridional, &
                                                    acc_u_tend_dcpl, &
                                                    acc_u_tend_dcpl_ReconstructX, acc_u_tend_dcpl_ReconstructY, &
                                                    acc_u_tend_dcpl_ReconstructZ, &
                                                    acc_u_tend_dcpl_ReconstructZonal, acc_u_tend_dcpl_ReconstructMeridional


      integer, pointer :: nCellsSolve, nEdges
      integer, dimension(:,:), pointer :: cellsOnEdge

      ! MC: Updated halo exchange
      call exchange_halo_group(domain, 'diagnostics:u_tend')


      call mpas_pool_get_array( diag, 'acc_u_tend_dyn_small', acc_u_tend_dyn_small)
      call mpas_pool_get_array( diag, 'acc_u_tend_dyn_small_ReconstructX', acc_u_tend_dyn_small_ReconstructX)
      call mpas_pool_get_array( diag, 'acc_u_tend_dyn_small_ReconstructY', acc_u_tend_dyn_small_ReconstructY)
      call mpas_pool_get_array( diag, 'acc_u_tend_dyn_small_ReconstructZ', acc_u_tend_dyn_small_ReconstructZ)
      call mpas_pool_get_array( diag, 'acc_u_tend_dyn_small_ReconstructZonal', acc_u_tend_dyn_small_ReconstructZonal)
      call mpas_pool_get_array( diag, 'acc_u_tend_dyn_small_ReconstructMeridional', acc_u_tend_dyn_small_ReconstructMeridional)

      call mpas_reconstruct(mesh, acc_u_tend_dyn_small,                 &
                            acc_u_tend_dyn_small_ReconstructX,          &
                            acc_u_tend_dyn_small_ReconstructY,          &
                            acc_u_tend_dyn_small_ReconstructZ,          &
                            acc_u_tend_dyn_small_ReconstructZonal,      &
                            acc_u_tend_dyn_small_ReconstructMeridional  &
                           )
                           

      call mpas_pool_get_array( diag, 'acc_u_tend_dyn_large', acc_u_tend_dyn_large)
      call mpas_pool_get_array( diag, 'acc_u_tend_dyn_large_ReconstructX', acc_u_tend_dyn_large_ReconstructX)
      call mpas_pool_get_array( diag, 'acc_u_tend_dyn_large_ReconstructY', acc_u_tend_dyn_large_ReconstructY)
      call mpas_pool_get_array( diag, 'acc_u_tend_dyn_large_ReconstructZ', acc_u_tend_dyn_large_ReconstructZ)
      call mpas_pool_get_array( diag, 'acc_u_tend_dyn_large_ReconstructZonal', acc_u_tend_dyn_large_ReconstructZonal)
      call mpas_pool_get_array( diag, 'acc_u_tend_dyn_large_ReconstructMeridional', acc_u_tend_dyn_large_ReconstructMeridional)

      call mpas_reconstruct(mesh, acc_u_tend_dyn_large,                 &
                            acc_u_tend_dyn_large_ReconstructX,          &
                            acc_u_tend_dyn_large_ReconstructY,          &
                            acc_u_tend_dyn_large_ReconstructZ,          &
                            acc_u_tend_dyn_large_ReconstructZonal,      &
                            acc_u_tend_dyn_large_ReconstructMeridional  &
                           )

             
      call mpas_pool_get_array( diag, 'acc_u_tend_diff', acc_u_tend_diff)
      call mpas_pool_get_array( diag, 'acc_u_tend_diff_ReconstructX', acc_u_tend_diff_ReconstructX)
      call mpas_pool_get_array( diag, 'acc_u_tend_diff_ReconstructY', acc_u_tend_diff_ReconstructY)
      call mpas_pool_get_array( diag, 'acc_u_tend_diff_ReconstructZ', acc_u_tend_diff_ReconstructZ)
      call mpas_pool_get_array( diag, 'acc_u_tend_diff_ReconstructZonal', acc_u_tend_diff_ReconstructZonal)
      call mpas_pool_get_array( diag, 'acc_u_tend_diff_ReconstructMeridional', acc_u_tend_diff_ReconstructMeridional)

      call mpas_reconstruct(mesh, acc_u_tend_diff,                 &
                            acc_u_tend_diff_ReconstructX,          &
                            acc_u_tend_diff_ReconstructY,          &
                            acc_u_tend_diff_ReconstructZ,          &
                            acc_u_tend_diff_ReconstructZonal,      &
                            acc_u_tend_diff_ReconstructMeridional  &
                           )


      call mpas_pool_get_array( diag, 'acc_u_tend_physics', acc_u_tend_physics)
      call mpas_pool_get_array( diag, 'acc_u_tend_physics_ReconstructX', acc_u_tend_physics_ReconstructX)
      call mpas_pool_get_array( diag, 'acc_u_tend_physics_ReconstructY', acc_u_tend_physics_ReconstructY)
      call mpas_pool_get_array( diag, 'acc_u_tend_physics_ReconstructZ', acc_u_tend_physics_ReconstructZ)
      call mpas_pool_get_array( diag, 'acc_u_tend_physics_ReconstructZonal', acc_u_tend_physics_ReconstructZonal)
      call mpas_pool_get_array( diag, 'acc_u_tend_physics_ReconstructMeridional', acc_u_tend_physics_ReconstructMeridional)

      call mpas_reconstruct(mesh, acc_u_tend_physics,                 &
                            acc_u_tend_physics_ReconstructX,          &
                            acc_u_tend_physics_ReconstructY,          &
                            acc_u_tend_physics_ReconstructZ,          &
                            acc_u_tend_physics_ReconstructZonal,      &
                            acc_u_tend_physics_ReconstructMeridional  &
                           )


      call mpas_pool_get_array( diag, 'acc_ublten', acc_ublten) 
      call mpas_pool_get_array( diag, 'acc_ublten_ReconstructX', acc_ublten_ReconstructX)
      call mpas_pool_get_array( diag, 'acc_ublten_ReconstructY', acc_ublten_ReconstructY)
      call mpas_pool_get_array( diag, 'acc_ublten_ReconstructZ', acc_ublten_ReconstructZ)
      call mpas_pool_get_array( diag, 'acc_ublten_ReconstructZonal', acc_ublten_ReconstructZonal)
      call mpas_pool_get_array( diag, 'acc_ublten_ReconstructMeridional', acc_ublten_ReconstructMeridional)

      call mpas_reconstruct(mesh, acc_ublten,                 &
                            acc_ublten_ReconstructX,          &
                            acc_ublten_ReconstructY,          &
                            acc_ublten_ReconstructZ,          &
                            acc_ublten_ReconstructZonal,      &
                            acc_ublten_ReconstructMeridional  &
                           )
 

      call mpas_pool_get_array( diag, 'acc_ugwdoten', acc_ugwdoten)
      call mpas_pool_get_array( diag, 'acc_ugwdoten_ReconstructX', acc_ugwdoten_ReconstructX)
      call mpas_pool_get_array( diag, 'acc_ugwdoten_ReconstructY', acc_ugwdoten_ReconstructY)
      call mpas_pool_get_array( diag, 'acc_ugwdoten_ReconstructZ', acc_ugwdoten_ReconstructZ)
      call mpas_pool_get_array( diag, 'acc_ugwdoten_ReconstructZonal', acc_ugwdoten_ReconstructZonal)
      call mpas_pool_get_array( diag, 'acc_ugwdoten_ReconstructMeridional', acc_ugwdoten_ReconstructMeridional)

      call mpas_reconstruct(mesh, acc_ugwdoten,                 &
                            acc_ugwdoten_ReconstructX,          &
                            acc_ugwdoten_ReconstructY,          &
                            acc_ugwdoten_ReconstructZ,          &
                            acc_ugwdoten_ReconstructZonal,      &
                            acc_ugwdoten_ReconstructMeridional  &
                           )


      call mpas_pool_get_array( diag, 'acc_ucuten', acc_ucuten)
      call mpas_pool_get_array( diag, 'acc_ucuten_ReconstructX', acc_ucuten_ReconstructX)
      call mpas_pool_get_array( diag, 'acc_ucuten_ReconstructY', acc_ucuten_ReconstructY)
      call mpas_pool_get_array( diag, 'acc_ucuten_ReconstructZ', acc_ucuten_ReconstructZ)
      call mpas_pool_get_array( diag, 'acc_ucuten_ReconstructZonal', acc_ucuten_ReconstructZonal)
      call mpas_pool_get_array( diag, 'acc_ucuten_ReconstructMeridional', acc_ucuten_ReconstructMeridional)

      call mpas_reconstruct(mesh, acc_ucuten,                 &
                            acc_ucuten_ReconstructX,          &
                            acc_ucuten_ReconstructY,          &
                            acc_ucuten_ReconstructZ,          &
                            acc_ucuten_ReconstructZonal,      &
                            acc_ucuten_ReconstructMeridional  &
                           )


      call mpas_pool_get_array( diag, 'acc_u_tend_smdiv', acc_u_tend_smdiv)
      call mpas_pool_get_array( diag, 'acc_u_tend_smdiv_ReconstructX', acc_u_tend_smdiv_ReconstructX)
      call mpas_pool_get_array( diag, 'acc_u_tend_smdiv_ReconstructY', acc_u_tend_smdiv_ReconstructY)
      call mpas_pool_get_array( diag, 'acc_u_tend_smdiv_ReconstructZ', acc_u_tend_smdiv_ReconstructZ)
      call mpas_pool_get_array( diag, 'acc_u_tend_smdiv_ReconstructZonal', acc_u_tend_smdiv_ReconstructZonal)
      call mpas_pool_get_array( diag, 'acc_u_tend_smdiv_ReconstructMeridional', acc_u_tend_smdiv_ReconstructMeridional)

      call mpas_reconstruct(mesh, acc_u_tend_smdiv,                 &
                            acc_u_tend_smdiv_ReconstructX,          &
                            acc_u_tend_smdiv_ReconstructY,          &
                            acc_u_tend_smdiv_ReconstructZ,          &
                            acc_u_tend_smdiv_ReconstructZonal,      &
                            acc_u_tend_smdiv_ReconstructMeridional  &
                           )


      call mpas_pool_get_array( diag, 'acc_u_tend_dcpl', acc_u_tend_dcpl)
      call mpas_pool_get_array( diag, 'acc_u_tend_dcpl_ReconstructX', acc_u_tend_dcpl_ReconstructX)
      call mpas_pool_get_array( diag, 'acc_u_tend_dcpl_ReconstructY', acc_u_tend_dcpl_ReconstructY)
      call mpas_pool_get_array( diag, 'acc_u_tend_dcpl_ReconstructZ', acc_u_tend_dcpl_ReconstructZ)
      call mpas_pool_get_array( diag, 'acc_u_tend_dcpl_ReconstructZonal', acc_u_tend_dcpl_ReconstructZonal)
      call mpas_pool_get_array( diag, 'acc_u_tend_dcpl_ReconstructMeridional', acc_u_tend_dcpl_ReconstructMeridional)

      call mpas_reconstruct(mesh, acc_u_tend_dcpl,                 &
                            acc_u_tend_dcpl_ReconstructX,          &
                            acc_u_tend_dcpl_ReconstructY,          &
                            acc_u_tend_dcpl_ReconstructZ,          &
                            acc_u_tend_dcpl_ReconstructZonal,      &
                            acc_u_tend_dcpl_ReconstructMeridional  &
                           )


   end subroutine atm_reconstruct_tend


end module atm_time_integration
