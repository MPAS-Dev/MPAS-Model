! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
module atm_core

   use mpas_derived_types
   use mpas_pool_routines
   use mpas_dmpar
   use mpas_log, only : mpas_log_write, mpas_log_info
   use mpas_io_units, only : mpas_new_unit, mpas_release_unit

   ! Provides definition of halo_exchange_routine
#include "mpas_halo_interface.inc"

   type (MPAS_Clock_type), pointer :: clock


   contains


   function atm_core_init(domain, startTimeStamp) result(ierr)

      use mpas_timekeeping
      use mpas_kind_types
      use mpas_stream_manager
      use mpas_atm_dimensions, only : mpas_atm_set_dims
      use mpas_atm_diagnostics_manager, only : mpas_atm_diag_setup
      use mpas_atm_threading, only : mpas_atm_threading_init
      use atm_time_integration, only : mpas_atm_dynamics_init, &
                              mpas_atm_pre_dynamics, mpas_atm_post_dynamics
      use mpas_timer, only : mpas_timer_start, mpas_timer_stop
      use mpas_attlist, only : mpas_modify_att
      use mpas_string_utils, only : mpas_string_replace
      use mpas_atm_halos, only: atm_build_halo_groups, exchange_halo_group
#ifdef DO_CHEMISTRY
      use mpas_atm_chemistry, only: chemistry_init
#endif

      implicit none

      type (domain_type), intent(inout) :: domain
      character(len=*), intent(out) :: startTimeStamp
      integer :: ierr

      real (kind=RKIND), pointer :: dt
      type (block_type), pointer :: block

      logical, pointer :: config_do_restart

      type (mpas_pool_type), pointer :: state
      type (mpas_pool_type), pointer :: mesh
      type (mpas_pool_type), pointer :: diag
      type (field2DReal), pointer :: u_field, pv_edge_field, ru_field, rw_field
      type (field0DReal), pointer :: Time_field
      character (len=StrKIND), pointer :: xtime
      character (len=StrKIND), pointer :: initial_time1, initial_time2
      type (MPAS_Time_Type) :: startTime

      real (kind=RKIND), pointer :: nominalMinDc
      real (kind=RKIND), pointer :: config_len_disp
      real (kind=RKIND), pointer :: Time

      integer, pointer :: nVertLevels, maxEdges, maxEdges2, num_scalars
      character (len=ShortStrKIND) :: init_stream_name
      real (kind=R8KIND) :: input_start_time, input_stop_time


      ierr = 0


      !
      ! Setup threading
      !
      call mpas_atm_threading_init(domain % blocklist, ierr)
      if ( ierr /= 0 ) then
         call mpas_log_write('Threading setup failed for core '//trim(domain % core % coreName), messageType=MPAS_LOG_CRIT)
      end if


      !
      ! Set up inner dimensions used by arrays in optimized dynamics routines
      !
      call mpas_pool_get_subpool(domain % blocklist % structs, 'state', state)
      call mpas_pool_get_dimension(state, 'nVertLevels', nVertLevels)
      call mpas_pool_get_dimension(state, 'maxEdges', maxEdges)
      call mpas_pool_get_dimension(state, 'maxEdges2', maxEdges2)
      call mpas_pool_get_dimension(state, 'num_scalars', num_scalars)
      call mpas_atm_set_dims(nVertLevels, maxEdges, maxEdges2, num_scalars)

      !
      ! Set "local" clock to point to the clock contained in the domain type
      !
      clock => domain % clock
      mpas_log_info => domain % logInfo

      !
      ! Build halo exchange groups and set method for exchanging halos in a group
      !
      call atm_build_halo_groups(domain, ierr)
      if (ierr /= 0) then
         call mpas_log_write('Failed to build halo exchange groups.', messageType=MPAS_LOG_ERR)
         return
      end if

      call mpas_pool_get_config(domain % blocklist % configs, 'config_do_restart', config_do_restart)
      call mpas_pool_get_config(domain % blocklist % configs, 'config_dt', dt)

      !
      ! By default, the 'invariant' stream has an input_interval of "none", so
      ! the following stream read has no effect. However, in case the 'invariant'
      ! stream is defined with an input_interval of "initial_only", we read
      ! time-invariant fields first.
      !
      call MPAS_stream_mgr_read(domain % streamManager, streamID='invariant', whence=MPAS_STREAM_NEAREST, ierr=ierr)
      call MPAS_stream_mgr_reset_alarms(domain % streamManager, streamID='invariant', direction=MPAS_STREAM_INPUT, ierr=ierr)

      !
      ! If this is a restart run, read the restart stream, else read the input
      ! stream.
      ! Regardless of which stream we read for initial conditions, reset the
      ! input alarms for both input and restart before reading any remaining
      ! input streams.
      !
      call mpas_timer_start('read_ICs')
      if (config_do_restart) then
         init_stream_name = 'restart'
      else
         init_stream_name = 'input'
      end if
      call mpas_log_write('Reading initial state from '''//trim(init_stream_name)//''' stream')

      call mpas_dmpar_get_time(input_start_time)
      call MPAS_stream_mgr_read(domain % streamManager, streamID=trim(init_stream_name), ierr=ierr)
      call mpas_dmpar_get_time(input_stop_time)
      call mpas_timer_stop('read_ICs')

      if (ierr /= MPAS_STREAM_MGR_NOERR) then
         call mpas_log_write('********************************************************************************', messageType=MPAS_LOG_ERR)
         call mpas_log_write('Error reading initial conditions',                                                 messageType=MPAS_LOG_ERR)
         call mpas_log_write('********************************************************************************', messageType=MPAS_LOG_CRIT)
      end if

      call mpas_log_write(' Timing for read of '''//trim(init_stream_name)//''' stream: $r s', &
                          realArgs=(/real(input_stop_time - input_start_time, kind=RKIND)/))

      call MPAS_stream_mgr_reset_alarms(domain % streamManager, streamID='input', direction=MPAS_STREAM_INPUT, ierr=ierr)
      call MPAS_stream_mgr_reset_alarms(domain % streamManager, streamID='restart', direction=MPAS_STREAM_INPUT, ierr=ierr)
      call mpas_log_write(' ----- done reading initial state -----')


      !
      ! Determine horizontal length scale used by horizontal diffusion and 3-d divergence damping
      !
      call mpas_pool_get_subpool(domain % blocklist % structs, 'mesh', mesh)
      nullify(nominalMinDc)
      call mpas_pool_get_array(mesh, 'nominalMinDc', nominalMinDc)

      nullify(config_len_disp)
      call mpas_pool_get_config(domain % blocklist % configs, 'config_len_disp', config_len_disp)

      call mpas_log_write('')

      !
      ! If config_len_disp was specified as a valid value, use that
      !
      if (config_len_disp > 0.0_RKIND) then
         call mpas_log_write('Setting nominalMinDc to $r based on namelist option config_len_disp', realArgs=[config_len_disp])

         !
         ! But if nominalMinDc was available in the input file and is different, print a warning
         !
         if (nominalMinDc > 0.0_RKIND .and. abs(nominalMinDc - config_len_disp) > 1.0e-6_RKIND * config_len_disp) then
            call mpas_log_write('nominalMinDc was read from input file as a positive value ($r) that differs', &
                                realArgs=[nominalMinDc], messageType=MPAS_LOG_WARN)
            call mpas_log_write('from the specified config_len_disp value ($r)', &
                                realArgs=[config_len_disp], messageType=MPAS_LOG_WARN)
         end if

         nominalMinDc = config_len_disp

      !
      ! Otherwise, try to use nominalMinDc
      !
      else
         if (nominalMinDc > 0.0_RKIND) then
            call mpas_log_write('Setting config_len_disp to $r based on nominalMinDc value in input file', realArgs=[nominalMinDc])
            config_len_disp = nominalMinDc
         else
            call mpas_log_write('Both config_len_disp and nominalMinDc are <= 0.0.', messageType=MPAS_LOG_ERR)
            call mpas_log_write('Please either specify config_len_disp in the &nhyd_model namelist group,', &
                                messageType=MPAS_LOG_ERR)
            call mpas_log_write('or use an input file that provides a valid value for the nominalMinDc variable.', &
                                messageType=MPAS_LOG_ERR)
            ierr = 1
            return
         end if
      end if


      !
      ! Read all other inputs
      ! For now we don't do this here to match results with previous code; to match requires 
      ! that we read in SST and seaice fields after the call to atm_mpas_init_block()
      !
!      call MPAS_stream_mgr_read(domain % streamManager, ierr=ierr)
!      call MPAS_stream_mgr_reset_alarms(domain % streamManager, direction=MPAS_STREAM_INPUT, ierr=ierr) 

      if (.not. config_do_restart) then
         block => domain % blocklist
         do while (associated(block))
            call mpas_pool_get_subpool(block % structs, 'state', state)
            call mpas_pool_initialize_time_levels(state)
            block => block % next
         end do
      end if

      !
      ! Read a new data stream for Incremental Analysis Update (IAU), if config_IAU_option /= 'off'    : HA (June-15-2016)
      ! FIXME: should I check xtime for the IAU fields? Maybe not.
      ! Note: Because the 'iau' stream has the 'iau' package attached to it, the MPAS_stream_mgr_read( )
      !       call here will actually try to read the stream only if IAU is being used in the run.
      !
      call MPAS_stream_mgr_read(domain % streamManager, streamID='iau', whence=MPAS_STREAM_NEAREST, ierr=ierr)
      if (ierr /= MPAS_STREAM_MGR_NOERR) then
         call mpas_log_write('********************************************************************************', messageType=MPAS_LOG_ERR)
         call mpas_log_write('Error reading IAU files',                                                          messageType=MPAS_LOG_ERR)
         call mpas_log_write('********************************************************************************', messageType=MPAS_LOG_CRIT)
      end if
      call MPAS_stream_mgr_reset_alarms(domain % streamManager, streamID='iau', ierr=ierr)

      !
      ! Set startTimeStamp based on the start time of the simulation clock
      !
      startTime = mpas_get_clock_time(clock, MPAS_START_TIME, ierr)
      call mpas_get_time(startTime, dateTimeString=startTimeStamp) 

      call exchange_halo_group(domain, 'initialization:u')


      !
      ! Perform basic compatibility checks among the fields that were read and the run-time options that were selected
      !
      call mpas_atm_run_compatibility(domain % dminfo, domain % blocklist, domain % streamManager, ierr)
      if (ierr /= 0) then
          call mpas_log_write('Please correct issues with the model input fields and/or namelist.', messageType=MPAS_LOG_ERR)
          return
      end if


      block => domain % blocklist
      do while (associated(block))
         call mpas_pool_get_subpool(block % structs, 'mesh', mesh)
         call mpas_pool_get_subpool(block % structs, 'state', state)

         call atm_mpas_init_block(domain % dminfo, domain % streamManager, block, mesh, dt)

         call mpas_pool_get_array(state, 'xtime', xtime, 1)
         xtime = startTimeStamp

         ! Initialize initial_time in second time level. We need to do this because initial state
         ! is read into time level 1, and if we write output from the set of state arrays that
         ! represent the original time level 2, the initial_time field will be invalid.
         call mpas_pool_get_array(state, 'initial_time', initial_time1, 1)
         call mpas_pool_get_array(state, 'initial_time', initial_time2, 2)
         initial_time2 = initial_time1

         ! Set the units to be cf compliant 'seconds since <cf-timestamp>'
         call mpas_pool_get_field(state, 'Time', Time_field)
         call mpas_modify_att(Time_field % attLists(1) % attList, 'units', &
              'seconds since ' // mpas_string_replace(initial_time1, '_', ' '))

          block => block % next
      end do

      call exchange_halo_group(domain, 'initialization:pv_edge,ru,rw')

      call mpas_atm_diag_setup(domain % streamManager, domain % blocklist % configs, &
                               domain % blocklist % structs, domain % clock, domain % dminfo)

      !
      ! Prepare the dynamics for integration
      !
      call mpas_atm_dynamics_init(domain)

      !
      ! Initialize the chemistry package
      !
#ifdef DO_CHEMISTRY
      call chemistry_init(domain % blocklist % configs, domain % blocklist % dimensions)
#endif

   end function atm_core_init


   subroutine atm_simulation_clock_init(core_clock, configs, ierr)

      use mpas_timekeeping

      implicit none

      type (MPAS_Clock_type), intent(inout) :: core_clock
      type (mpas_pool_type), intent(inout) :: configs
      integer, intent(out) :: ierr

      type (MPAS_Time_Type) :: startTime, stopTime
      type (MPAS_TimeInterval_type) :: runDuration, timeStep
      integer :: local_err
      real (kind=RKIND), pointer :: config_dt
      character (len=StrKIND), pointer :: config_start_time
      character (len=StrKIND), pointer :: config_restart_timestamp_name
      character (len=StrKIND), pointer :: config_run_duration
      character (len=StrKIND), pointer :: config_stop_time
      character (len=StrKIND) :: startTimeStamp
      integer :: iounit


      ierr = 0

      call mpas_pool_get_config(configs, 'config_dt', config_dt)
      call mpas_pool_get_config(configs, 'config_start_time', config_start_time)
      call mpas_pool_get_config(configs, 'config_restart_timestamp_name', config_restart_timestamp_name)
      call mpas_pool_get_config(configs, 'config_run_duration', config_run_duration)
      call mpas_pool_get_config(configs, 'config_stop_time', config_stop_time)

      if(trim(config_start_time) == 'file') then
         call mpas_new_unit(iounit)
         open(iounit,file=trim(config_restart_timestamp_name),form='formatted',status='old')
         read(iounit,*) startTimeStamp
         close(iounit)
         call mpas_release_unit(iounit)
      else
        startTimeStamp = config_start_time
      end if
      call mpas_set_time(curr_time=startTime, dateTimeString=startTimeStamp, ierr=local_err)
      call mpas_set_timeInterval(timeStep, dt=config_dt, ierr=local_err)

      if (trim(config_run_duration) /= "none") then
         call mpas_set_timeInterval(runDuration, timeString=config_run_duration, ierr=local_err)
         call mpas_create_clock(core_clock, startTime=startTime, timeStep=timeStep, runDuration=runDuration, ierr=local_err)

         if (trim(config_stop_time) /= "none") then
            call mpas_set_time(curr_time=stopTime, dateTimeString=config_stop_time, ierr=local_err)
            if(startTime + runduration /= stopTime) then
               call mpas_log_write('config_run_duration and config_stop_time are inconsistent: using config_run_duration.', messageType=MPAS_LOG_WARN)
            end if
         end if
      else if (trim(config_stop_time) /= "none") then
         call mpas_set_time(curr_time=stopTime, dateTimeString=config_stop_time, ierr=local_err)
         call mpas_create_clock(core_clock, startTime=startTime, timeStep=timeStep, stopTime=stopTime, ierr=local_err)
      else
          call mpas_log_write('Neither config_run_duration nor config_stop_time were specified.', messageType=MPAS_LOG_ERR)
          ierr = 1
      end if

      !TODO: set phyics alarms here...
      !....
      !....

   end subroutine atm_simulation_clock_init


   subroutine atm_mpas_init_block(dminfo, stream_manager, block, mesh, dt)
   
      use atm_time_integration
      use mpas_rbf_interpolation
      use mpas_vector_reconstruction
      use mpas_stream_manager
      use mpas_atm_boundaries, only : mpas_atm_setup_bdy_masks
#ifdef DO_PHYSICS
!     use mpas_atmphys_aquaplanet
      use mpas_atmphys_control
      use mpas_atmphys_init
      use mpas_atmphys_manager
#endif

      implicit none
   
      type (dm_info), intent(in) :: dminfo
      type (MPAS_streamManager_type), intent(inout) :: stream_manager
      type (block_type), intent(inout) :: block
      type (mpas_pool_type), intent(inout) :: mesh     !MGD does this need to be a pointer?
      real (kind=RKIND), intent(in) :: dt

      type (mpas_pool_type), pointer :: state
      type (mpas_pool_type), pointer :: diag
      type (mpas_pool_type), pointer :: tend
      type (mpas_pool_type), pointer :: tend_physics
      type (mpas_pool_type), pointer :: sfc_input
      type (mpas_pool_type), pointer :: diag_physics
      type (mpas_pool_type), pointer :: diag_physics_noahmp
      type (mpas_pool_type), pointer :: ngw_input
      type (mpas_pool_type), pointer :: atm_input
      type (mpas_pool_type), pointer :: output_noahmp

      integer :: iCell,iEdge,iVertex,k
      
      real (kind=RKIND), dimension(:,:), pointer :: u, uReconstructX, uReconstructY, uReconstructZ, uReconstructZonal, uReconstructMeridional
      real (kind=RKIND), dimension(:), pointer :: meshScalingDel2, meshScalingDel4
      real (kind=RKIND), dimension(:), pointer :: areaCell, invAreaCell
      real (kind=RKIND), dimension(:), pointer :: dvEdge, invDvEdge
      real (kind=RKIND), dimension(:), pointer :: dcEdge, invDcEdge
      real (kind=RKIND), dimension(:), pointer :: areaTriangle, invAreaTriangle
      integer, pointer :: nCells_ptr, nEdges_ptr, nVertices_ptr, nVertLevels_ptr, nEdgesSolve
      integer :: nCells, nEdges, nVertices, nVertLevels
      integer :: thread
      character(len=StrKIND), pointer :: mminlu

      integer, pointer :: nThreads
      integer, dimension(:), pointer :: cellThreadStart, cellThreadEnd
      integer, dimension(:), pointer :: cellSolveThreadStart, cellSolveThreadEnd
      integer, dimension(:), pointer :: edgeThreadStart, edgeThreadEnd
      integer, dimension(:), pointer :: edgeSolveThreadStart, edgeSolveThreadEnd
      integer, dimension(:), pointer :: vertexThreadStart, vertexThreadEnd
      integer, dimension(:), pointer :: vertexSolveThreadStart, vertexSolveThreadEnd


      logical, pointer :: config_do_restart, config_do_DAcycling

      logical, pointer :: on_a_sphere
      real (kind=RKIND), pointer :: sphere_radius


      call atm_compute_signs(mesh)
   
      call mpas_pool_get_subpool(block % structs, 'diag', diag)
      call mpas_pool_get_subpool(block % structs, 'state', state)

      call mpas_pool_get_subpool(block % structs, 'state', state)

      call mpas_pool_get_config(block % configs, 'config_do_restart', config_do_restart)
      call mpas_pool_get_config(block % configs, 'config_do_DAcycling', config_do_DAcycling)

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!!! Compute inverses to avoid divides later
      
      call mpas_pool_get_array(mesh, 'areaCell', areaCell)
      call mpas_pool_get_array(mesh, 'invAreaCell', invAreaCell)
      
      call mpas_pool_get_array(mesh, 'dvEdge', dvEdge)
      call mpas_pool_get_array(mesh, 'dcEdge', dcEdge)      
      call mpas_pool_get_array(mesh, 'invDvEdge', invDvEdge)
      call mpas_pool_get_array(mesh, 'invDcEdge', invDcEdge)      
      
      call mpas_pool_get_array(mesh, 'areaTriangle', areaTriangle)
      call mpas_pool_get_array(mesh, 'invAreaTriangle', invAreaTriangle)
      
      call mpas_pool_get_dimension(mesh, 'nCells', nCells_ptr)      
      call mpas_pool_get_dimension(mesh, 'nEdges', nEdges_ptr)
      call mpas_pool_get_dimension(mesh, 'nVertices', nVertices_ptr)

      nCells = nCells_ptr
      nEdges = nEdges_ptr
      nVertices = nVertices_ptr
 
      do iCell=1,nCells 
         invAreaCell(iCell) = 1.0_RKIND / areaCell(iCell)
      end do
        
      do iEdge=1,nEdges 
         invDvEdge(iEdge) = 1.0_RKIND / dvEdge(iEdge)
      end do
      
      do iEdge=1,nEdges 
         invDcEdge(iEdge) = 1.0_RKIND / dcEdge(iEdge)
      end do
      
      do iVertex=1,nVertices
         invAreaTriangle(iVertex) = 1.0_RKIND / areaTriangle(iVertex)
      end do
      
      !!!!! End compute inverses
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      call mpas_pool_get_config(mesh, 'on_a_sphere', on_a_sphere)
      call mpas_pool_get_config(mesh, 'sphere_radius', sphere_radius)
      call atm_initialize_deformation_weights(mesh, nCells, on_a_sphere, sphere_radius)

      call atm_adv_coef_compression(mesh)

      call atm_couple_coef_3rd_order(mesh, block % configs)
      
      call mpas_pool_get_dimension(state, 'nVertices', nVertices_ptr)
      call mpas_pool_get_dimension(state, 'nVertLevels', nVertLevels_ptr)

      nVertices = nVertices_ptr
      nVertLevels = nVertLevels_ptr

      allocate(ke_vertex(nVertLevels,nVertices+1))  ! ke_vertex is a module variable defined in mpas_atm_time_integration.F
      allocate(ke_edge(nVertLevels,nEdges+1))       ! ke_edge is a module variable defined in mpas_atm_time_integration.F
      !$acc parallel default(present)
      !$acc loop vector
      do k = 1, nVertLevels
         ke_vertex(k,nVertices+1) = 0.0_RKIND
         ke_edge(k,nEdges+1) = 0.0_RKIND
      end do
      !$acc end parallel

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

      call mpas_atm_pre_compute_solve_diagnostics(block)
!$OMP PARALLEL DO
      do thread=1,nThreads
         if (.not. config_do_restart .or. (config_do_restart .and. config_do_DAcycling)) then
            call atm_init_coupled_diagnostics(state, 1, diag, mesh, block % configs, &
                                              cellThreadStart(thread), cellThreadEnd(thread), &
                                              vertexThreadStart(thread), vertexThreadEnd(thread), &
                                              edgeThreadStart(thread), edgeThreadEnd(thread), &
                                              cellSolveThreadStart(thread), cellSolveThreadEnd(thread), &
                                              vertexSolveThreadStart(thread), vertexSolveThreadEnd(thread), &
                                              edgeSolveThreadStart(thread), edgeSolveThreadEnd(thread))
         end if

         call atm_compute_solve_diagnostics(dt, state, 1, diag, mesh, block % configs, &
                                            cellThreadStart(thread), cellThreadEnd(thread), &
                                            vertexThreadStart(thread), vertexThreadEnd(thread), &
                                            edgeThreadStart(thread), edgeThreadEnd(thread))
      end do
!$OMP END PARALLEL DO
      call mpas_atm_post_compute_solve_diagnostics(block)

      deallocate(ke_vertex)
      deallocate(ke_edge)

      call mpas_rbf_interp_initialize(mesh)
      call mpas_init_reconstruct(mesh)

      call mpas_pool_get_array(state, 'u', u, 1)
      call mpas_pool_get_array(diag, 'uReconstructX', uReconstructX)
      call mpas_pool_get_array(diag, 'uReconstructY', uReconstructY)
      call mpas_pool_get_array(diag, 'uReconstructZ', uReconstructZ)
      call mpas_pool_get_array(diag, 'uReconstructZonal', uReconstructZonal)
      call mpas_pool_get_array(diag, 'uReconstructMeridional', uReconstructMeridional)
      call mpas_reconstruct_2d_h2d(mesh, u, uReconstructX, uReconstructY, uReconstructZ, &
                                   uReconstructZonal, uReconstructMeridional)
      call mpas_reconstruct(mesh, u,                   &
                            uReconstructX,             &
                            uReconstructY,             &
                            uReconstructZ,             &
                            uReconstructZonal,         &
                            uReconstructMeridional,    &
                            lACC = .true.              &
                           )
      call mpas_reconstruct_2d_d2h(mesh, u, uReconstructX, uReconstructY, uReconstructZ, &
               uReconstructZonal, uReconstructMeridional)
   
#ifdef DO_PHYSICS
      !proceed with initialization of physics parameterization if moist_physics is set to true:
      call mpas_pool_get_subpool(block % structs, 'sfc_input', sfc_input)

      ! Before calling physics_init, ensure that mminlu contains the name of the land use dataset
      call mpas_pool_get_array(sfc_input, 'mminlu', mminlu)
      if (len_trim(mminlu) == 0) then
            call mpas_log_write('****************************************************************')
            call mpas_log_write('No information on land use dataset is available.')
            call mpas_log_write('Assume that we are using ''USGS''.')
            call mpas_log_write('****************************************************************')
            write(mminlu,'(a)') 'USGS'
      end if


      if (moist_physics) then
         !initialization of some input variables in registry:
         call mpas_pool_get_subpool(block % structs, 'tend', tend)
         call mpas_pool_get_subpool(block % structs, 'tend_physics', tend_physics)
         call mpas_pool_get_subpool(block % structs, 'diag_physics', diag_physics)
         call mpas_pool_get_subpool(block % structs, 'diag_physics_noahmp', diag_physics_noahmp)
         call mpas_pool_get_subpool(block % structs, 'ngw_input', ngw_input)
         call mpas_pool_get_subpool(block % structs, 'atm_input', atm_input)
         call mpas_pool_get_subpool(block % structs, 'output_noahmp', output_noahmp)
         call physics_tables_init(dminfo, block % configs)
         call physics_registry_init(mesh, block % configs, sfc_input)
         call physics_run_init(block % configs, mesh, state, clock, stream_manager)

         !initialization of all physics:
         call physics_init(dminfo, stream_manager, clock, block % configs, mesh, diag, tend, tend_physics, state, 1, &
                           diag_physics, diag_physics_noahmp, ngw_input, atm_input, sfc_input, output_noahmp)
      endif
#endif
   
      call atm_compute_mesh_scaling(mesh, block % configs)

      call atm_compute_damping_coefs(mesh, block % configs)

      !
      ! Set up mask fields used in limited-area simulations
      !
      call mpas_atm_setup_bdy_masks(mesh, block % configs)

      call mpas_pool_get_dimension(mesh, 'nEdgesSolve', nEdgesSolve)
      call mpas_pool_get_array(mesh, 'meshScalingDel2', meshScalingDel2)
      call mpas_pool_get_array(mesh, 'meshScalingDel4', meshScalingDel4)

      call mpas_log_write('min/max of meshScalingDel2 = $r $r', &
                          realArgs=(/minval(meshScalingDel2(1:nEdgesSolve)), maxval(meshScalingDel2(1:nEdgesSolve))/))
      call mpas_log_write('min/max of meshScalingDel4 = $r $r', &
                          realArgs=(/minval(meshScalingDel4(1:nEdgesSolve)), maxval(meshScalingDel4(1:nEdgesSolve))/))

   end subroutine atm_mpas_init_block
   
   
   function atm_core_run(domain) result(ierr)
   
      use mpas_timekeeping
      use mpas_kind_types
      use mpas_stream_manager
      use mpas_derived_types, only : MPAS_STREAM_LATEST_BEFORE, MPAS_STREAM_INPUT, MPAS_STREAM_INPUT_OUTPUT
      use mpas_timer, only : mpas_timer_start, mpas_timer_stop
      use mpas_atm_boundaries, only : mpas_atm_update_bdy_tend
      use mpas_atm_diagnostics_manager, only : mpas_atm_diag_update, mpas_atm_diag_compute, mpas_atm_diag_reset
   
      implicit none
   
      type (domain_type), intent(inout) :: domain
      integer :: ierr
   
      real (kind=RKIND) :: dt
      logical, pointer :: config_do_restart
      logical, pointer :: config_apply_lbcs
      type (block_type), pointer :: block_ptr

      type (MPAS_Time_Type) :: currTime
      character(len=StrKIND) :: timeStamp
      character (len=StrKIND), pointer :: config_restart_timestamp_name
      integer :: itimestep
      integer :: iounit

      integer :: stream_dir
      character(len=StrKIND) :: input_stream, read_time

      type (mpas_pool_type), pointer :: state, diag, mesh, diag_physics, tend, tend_physics

      ! For timing information
      real (kind=R8KIND) :: integ_start_time, integ_stop_time
      real (kind=R8KIND) :: diag_start_time, diag_stop_time
      real (kind=R8KIND) :: input_start_time, input_stop_time
      real (kind=R8KIND) :: output_start_time, output_stop_time
      
      ierr = 0

      clock => domain % clock
      mpas_log_info => domain % logInfo
      
      call mpas_get_timeInterval(mpas_get_clock_timestep(clock, ierr), dt=dt)
      call mpas_pool_get_config(domain % blocklist % configs, 'config_do_restart', config_do_restart)
      call mpas_pool_get_config(domain % blocklist % configs, 'config_restart_timestamp_name', config_restart_timestamp_name)

      ! Avoid writing a restart file at the initial time
      call MPAS_stream_mgr_reset_alarms(domain % streamManager, streamID='restart', direction=MPAS_STREAM_OUTPUT, ierr=ierr)

      ! Also, for restart runs, avoid writing the initial history or diagnostics fields to avoid overwriting those from the preceding run
      if (config_do_restart) then
         call MPAS_stream_mgr_reset_alarms(domain % streamManager, streamID='output', direction=MPAS_STREAM_OUTPUT, ierr=ierr)
         call MPAS_stream_mgr_reset_alarms(domain % streamManager, streamID='diagnostics', direction=MPAS_STREAM_OUTPUT, ierr=ierr)
      end if

      call mpas_dmpar_get_time(diag_start_time)

      if (MPAS_stream_mgr_ringing_alarms(domain % streamManager, direction=MPAS_STREAM_OUTPUT, ierr=ierr)) then
         block_ptr => domain % blocklist
         do while (associated(block_ptr))
            call mpas_pool_get_subpool(block_ptr % structs, 'state', state)
            call mpas_pool_get_subpool(block_ptr % structs, 'diag', diag)
#ifdef DO_PHYSICS
            call mpas_pool_get_subpool(block_ptr % structs, 'diag_physics', diag_physics)
#endif
            call mpas_pool_get_subpool(block_ptr % structs, 'mesh', mesh)
            call atm_compute_output_diagnostics(state, 1, diag, mesh)

            block_ptr => block_ptr % next
         end do
      end if
      call mpas_timer_start('diagnostic_fields')
      call mpas_atm_diag_reset()
      call mpas_atm_diag_update()
      call mpas_atm_diag_compute()
      call mpas_timer_stop('diagnostic_fields')

      call mpas_dmpar_get_time(diag_stop_time)

      call mpas_timer_start('stream_output')
      call mpas_dmpar_get_time(output_start_time)
      call mpas_stream_mgr_write(domain % streamManager, ierr=ierr)
      call mpas_dmpar_get_time(output_stop_time)
      call mpas_timer_stop('stream_output')
      if (ierr /= MPAS_STREAM_MGR_NOERR .and. &
          ierr /= MPAS_STREAM_MGR_ERR_CLOBBER_FILE .and. &
          ierr /= MPAS_STREAM_MGR_ERR_CLOBBER_REC) then
         call mpas_log_write('********************************************************************************', messageType=MPAS_LOG_ERR)
         call mpas_log_write('Error writing one or more output streams',                                         messageType=MPAS_LOG_ERR)
         call mpas_log_write('********************************************************************************', messageType=MPAS_LOG_CRIT)
      end if
      if (MPAS_stream_mgr_ringing_alarms(domain % streamManager, direction=MPAS_STREAM_OUTPUT, ierr=ierr)) then
         call mpas_log_write('Timing for diagnostic computation: $r s', realArgs=(/real(diag_stop_time - diag_start_time, kind=RKIND)/))
         call mpas_log_write('Timing for stream output: $r s', realArgs=(/real(output_stop_time - output_start_time, kind=RKIND)/))
      end if

      call mpas_timer_start('diagnostic_fields')
      call mpas_atm_diag_reset()
      call mpas_timer_stop('diagnostic_fields')

      call mpas_stream_mgr_reset_alarms(domain % streamManager, direction=MPAS_STREAM_OUTPUT, ierr=ierr)

      block_ptr => domain % blocklist
      call mpas_pool_get_subpool(block_ptr % structs, 'mesh', mesh)
      call mpas_pool_get_subpool(block_ptr % structs, 'state', state)
      call mpas_pool_get_subpool(block_ptr % structs, 'diag', diag)
#ifdef DO_PHYSICS
      call mpas_pool_get_subpool(block_ptr % structs, 'diag_physics', diag_physics)
#endif

      call mpas_pool_get_config(domain % blocklist % configs, 'config_apply_lbcs', config_apply_lbcs)

      !
      ! Read initial boundary state
      !
      if (config_apply_lbcs .and. &
          MPAS_stream_mgr_ringing_alarms(domain % streamManager, streamID='lbc_in', direction=MPAS_STREAM_INPUT, ierr=ierr)) then
         block_ptr => domain % blocklist
         do while (associated(block_ptr))
            call mpas_atm_update_bdy_tend(clock, domain % streamManager, block_ptr, .true., ierr)
            if (ierr /= 0) then
               currTime = mpas_get_clock_time(clock, MPAS_NOW, ierr)
               call mpas_get_time(curr_time=currTime, dateTimeString=timeStamp)
               call mpas_log_write('Failed to process LBC data on or before '//trim(timeStamp), messageType=MPAS_LOG_ERR)
               return
            end if

            block_ptr => block_ptr % next
         end do
      end if

      ! During integration, time level 1 stores the model state at the beginning of the
      !   time step, and time level 2 stores the state advanced dt in time by timestep(...)
      itimestep = 1
      do while (.not. mpas_is_clock_stop_time(clock))

         currTime = mpas_get_clock_time(clock, MPAS_NOW, ierr)
         call mpas_get_time(curr_time=currTime, dateTimeString=timeStamp, ierr=ierr)         

         call mpas_log_write('')
         call mpas_log_write('Begin timestep '//trim(timeStamp))

         !
         ! Read future boundary state and compute boundary tendencies
         !
         if (config_apply_lbcs .and. &
             MPAS_stream_mgr_ringing_alarms(domain % streamManager, streamID='lbc_in', direction=MPAS_STREAM_INPUT, ierr=ierr)) then
            block_ptr => domain % blocklist
            do while (associated(block_ptr))
               call mpas_atm_update_bdy_tend(clock, domain % streamManager, block_ptr, .false., ierr)
               if (ierr /= 0) then
                  call mpas_log_write('Failed to process LBC data at next time after '//trim(timeStamp), messageType=MPAS_LOG_ERR)
                  return
               end if

               block_ptr => block_ptr % next
            end do
         end if

         ! Regardless of whether boundary tendencies were updated, above, we do not want to read the 'lbc_in' stream
         ! as a general input stream, below.
         call MPAS_stream_mgr_reset_alarms(domain % streamManager, streamID='lbc_in', direction=MPAS_STREAM_INPUT, ierr=ierr)


         !
         ! Read external field updates
         !
         call MPAS_stream_mgr_begin_iteration(domain % streamManager, ierr=ierr)
         do while (MPAS_stream_mgr_get_next_stream(domain % streamManager, streamID=input_stream, directionProperty=stream_dir))
            if (stream_dir == MPAS_STREAM_INPUT .or. stream_dir == MPAS_STREAM_INPUT_OUTPUT) then
               if (MPAS_stream_mgr_ringing_alarms(domain % streamManager, streamID=input_stream, &
                                                  direction=MPAS_STREAM_INPUT, ierr=ierr)) then
                  call mpas_timer_start('stream_input')
                  call mpas_dmpar_get_time(input_start_time)
                  call MPAS_stream_mgr_read(domain % streamManager, streamID=input_stream, whence=MPAS_STREAM_LATEST_BEFORE, &
                                            actualWhen=read_time, ierr=ierr)
                  call mpas_dmpar_get_time(input_stop_time)
                  call mpas_timer_stop('stream_input')
                  if (ierr /= MPAS_STREAM_MGR_NOERR) then
                     call mpas_log_write('********************************************************************************', messageType=MPAS_LOG_ERR)
                     call mpas_log_write('Error reading input stream '//trim(input_stream),                                  messageType=MPAS_LOG_ERR)
                     call mpas_log_write('********************************************************************************', messageType=MPAS_LOG_CRIT)
                  end if

                  call mpas_log_write('----------------------------------------------------------------------')
                  call mpas_log_write('  Read '''//trim(input_stream)//''' input stream valid at '//trim(read_time))
                  call mpas_log_write('   Timing for stream input: $r s', realArgs=(/real(input_stop_time - input_start_time, kind=RKIND)/))
                  call mpas_log_write('----------------------------------------------------------------------')

                  call MPAS_stream_mgr_reset_alarms(domain % streamManager, streamID=input_stream, direction=MPAS_STREAM_INPUT, ierr=ierr)
               end if
            end if
         end do

         call mpas_timer_start("time integration")
         call mpas_dmpar_get_time(integ_start_time)
         call atm_do_timestep(domain, dt, itimestep)
         call mpas_dmpar_get_time(integ_stop_time)
         call mpas_timer_stop("time integration")   
         call mpas_log_write(' Timing for integration step: $r s', realArgs=(/real(integ_stop_time - integ_start_time, kind=RKIND)/))

         ! Move time level 2 fields back into time level 1 for next time step
         call mpas_pool_get_subpool(domain % blocklist % structs, 'state', state)
         call mpas_pool_shift_time_levels(state)
         
         ! Advance clock before writing output
         itimestep = itimestep + 1
         call mpas_advance_clock(clock)
         currTime = mpas_get_clock_time(clock, MPAS_NOW, ierr)


         call mpas_dmpar_get_time(diag_start_time)

         !
         ! Write any output streams that have alarms ringing, after computing diagnostics fields
         !
         call mpas_get_time(curr_time=currTime, dateTimeString=timeStamp, ierr=ierr)
         if (MPAS_stream_mgr_ringing_alarms(domain % streamManager, direction=MPAS_STREAM_OUTPUT, ierr=ierr)) then
           block_ptr => domain % blocklist
           do while (associated(block_ptr))
              call mpas_pool_get_subpool(block_ptr % structs, 'state', state)
              call mpas_pool_get_subpool(block_ptr % structs, 'diag', diag)
#ifdef DO_PHYSICS
              call mpas_pool_get_subpool(block_ptr % structs, 'diag_physics', diag_physics)
              call mpas_pool_get_subpool(block_ptr % structs, 'tend_physics', tend_physics)
#endif
              call mpas_pool_get_subpool(block_ptr % structs, 'mesh', mesh)
              call mpas_pool_get_subpool(block_ptr % structs, 'tend', tend)
              call atm_compute_output_diagnostics(state, 1, diag, mesh)

              block_ptr => block_ptr % next
           end do
         end if

         call mpas_timer_start('diagnostic_fields')
         call mpas_atm_diag_update()
         call mpas_atm_diag_compute()
         call mpas_timer_stop('diagnostic_fields')
         call mpas_dmpar_get_time(diag_stop_time)

         call mpas_timer_start('stream_output')
         call mpas_dmpar_get_time(output_start_time)
         call mpas_stream_mgr_write(domain % streamManager, ierr=ierr)
         call mpas_dmpar_get_time(output_stop_time)
         call mpas_timer_stop('stream_output')
         if (ierr /= MPAS_STREAM_MGR_NOERR .and. &
             ierr /= MPAS_STREAM_MGR_ERR_CLOBBER_FILE .and. &
             ierr /= MPAS_STREAM_MGR_ERR_CLOBBER_REC) then
            call mpas_log_write('********************************************************************************', messageType=MPAS_LOG_ERR)
            call mpas_log_write('Error writing one or more output streams',                                         messageType=MPAS_LOG_ERR)
            call mpas_log_write('********************************************************************************', messageType=MPAS_LOG_CRIT)
         end if
         if (MPAS_stream_mgr_ringing_alarms(domain % streamManager, direction=MPAS_STREAM_OUTPUT, ierr=ierr)) then
            call mpas_log_write('Timing for diagnostic computation: $r s', realArgs=(/real(diag_stop_time - diag_start_time, kind=RKIND)/))
            call mpas_log_write('Timing for stream output: $r s', realArgs=(/real(output_stop_time - output_start_time, kind=RKIND)/))
         end if

         ! reset any diagnostics here

         if (MPAS_stream_mgr_ringing_alarms(domain % streamManager, streamID='diagnostics', direction=MPAS_STREAM_OUTPUT, ierr=ierr)) then
            block_ptr => domain % blocklist
            do while (associated(block_ptr))

#ifdef DO_PHYSICS
               call mpas_pool_get_subpool(block_ptr % structs, 'diag_physics', diag_physics)
#endif
               call atm_reset_diagnostics(diag_physics)

               block_ptr => block_ptr % next
            end do
         end if


         ! Only after we've successfully written the restart file should we we
         !    write the restart_timestamp file
         if (MPAS_stream_mgr_ringing_alarms(domain % streamManager, streamID='restart', direction=MPAS_STREAM_OUTPUT, ierr=ierr)) then
            if (domain % dminfo % my_proc_id == 0) then
               call mpas_new_unit(iounit)
               open(iounit,file=trim(config_restart_timestamp_name),form='formatted',status='replace')
               write(iounit,*) trim(timeStamp)
               close(iounit)
               call mpas_release_unit(iounit)
            end if
         end if

         call mpas_timer_start('diagnostic_fields')
         call mpas_atm_diag_reset()
         call mpas_timer_stop('diagnostic_fields')

         call mpas_stream_mgr_reset_alarms(domain % streamManager, direction=MPAS_STREAM_OUTPUT, ierr=ierr)

      end do
   
   end function atm_core_run

   
   subroutine atm_compute_output_diagnostics(state, time_lev, diag, mesh)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Compute diagnostic fields for a domain to be written to history files
   !
   ! Input: state - contains model prognostic fields
   !        mesh  - contains grid metadata
   !
   ! Output: state - upon returning, diagnostic fields will have be computed
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
      use mpas_constants
   
      implicit none
   
      type (mpas_pool_type), intent(inout) :: state
      integer, intent(in) :: time_lev            ! which time level to use from state
      type (mpas_pool_type), intent(inout) :: diag
      type (mpas_pool_type), intent(in) :: mesh
   
      integer :: iCell, k
      integer, pointer :: nCells, nVertLevels, index_qv
      real (kind=RKIND), dimension(:,:), pointer :: theta, rho, theta_m, rho_zz, zz
      real (kind=RKIND), dimension(:,:), pointer :: pressure_base, pressure_p, pressure
      real (kind=RKIND), dimension(:,:,:), pointer :: scalars

      call mpas_pool_get_dimension(mesh, 'nCells', nCells)
      call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)
      call mpas_pool_get_dimension(state, 'index_qv', index_qv)

      call mpas_pool_get_array(state, 'theta_m', theta_m, time_lev)
      call mpas_pool_get_array(state, 'rho_zz', rho_zz, time_lev)
      call mpas_pool_get_array(state, 'scalars', scalars, time_lev)

      call mpas_pool_get_array(diag, 'theta', theta)
      call mpas_pool_get_array(diag, 'rho', rho)
      call mpas_pool_get_array(diag, 'pressure_p', pressure_p)
      call mpas_pool_get_array(diag, 'pressure_base', pressure_base)
      call mpas_pool_get_array(diag, 'pressure', pressure)

      call mpas_pool_get_array(mesh, 'zz', zz)

      do iCell=1,nCells
         do k=1,nVertLevels
            theta(k,iCell) = theta_m(k,iCell) / (1._RKIND + rvord * scalars(index_qv,k,iCell))
            rho(k,iCell) = rho_zz(k,iCell) * zz(k,iCell)
            pressure(k,iCell) = pressure_base(k,iCell) + pressure_p(k,iCell)
         end do
      end do

   end subroutine atm_compute_output_diagnostics
   
   
   subroutine atm_reset_diagnostics(diag_physics)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! reset some diagnostics after output
   !
   ! Input: diag_physics  - contains physics diagnostic fields
   !
   ! Output: whatever diagnostics need resetting after output
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
      implicit none
   
      type (mpas_pool_type), pointer :: diag_physics
   
      real (kind=RKIND), dimension(:), pointer :: refl10cm_1km_max

#ifdef DO_PHYSICS
      call mpas_pool_get_array(diag_physics, 'refl10cm_1km_max', refl10cm_1km_max)
      if(associated(refl10cm_1km_max)) then
         refl10cm_1km_max(:) = 0.
      endif
#endif
   
   end subroutine atm_reset_diagnostics


   subroutine atm_do_timestep(domain, dt, itimestep)
   
      use mpas_timekeeping
      use mpas_kind_types
      use atm_time_integration
#ifdef DO_PHYSICS
      use mpas_atmphys_control
      use mpas_atmphys_driver
      use mpas_atmphys_manager
      use mpas_atmphys_update
#endif
#ifdef DO_CHEMISTRY
      use mpas_atm_chemistry, only: chemistry_step
#endif
      use mpas_atm_halos, only: exchange_halo_group

      implicit none
   
      type (domain_type), intent(inout) :: domain 
      real (kind=RKIND), intent(in) :: dt
      integer, intent(in) :: itimestep
      
      type (MPAS_Time_Type) :: startTime, currTime
      type (MPAS_TimeInterval_Type) :: xtimeTime
      integer :: s, s_n, s_d
      real (kind=RKIND) :: xtime_s
      integer :: ierr

      clock => domain % clock
      mpas_log_info => domain % logInfo

      startTime = mpas_get_clock_time(clock, MPAS_START_TIME, ierr)
      currTime = mpas_get_clock_time(clock, MPAS_NOW, ierr)
         
      xtimeTime = currTime - startTime
      call mpas_get_timeInterval(interval=xtimeTime, S=s, S_n=s_n, S_d=s_d, ierr=ierr)         
      xtime_s = (s + s_n / s_d)


#ifdef DO_PHYSICS
      !proceed with physics if moist_physics is set to true:
      if(moist_physics) then
         call physics_timetracker(domain,dt,clock,itimestep,xtime_s)
         call physics_driver(domain,itimestep,xtime_s)
      endif
#endif

#ifdef DO_CHEMISTRY
      call chemistry_step()
#endif

      call atm_timestep(domain, dt, currTime, itimestep, exchange_halo_group)

   end subroutine atm_do_timestep
   
   
   function atm_core_finalize(domain) result(ierr)
   
      use mpas_decomp
      use mpas_timekeeping
      use mpas_atm_diagnostics_manager, only : mpas_atm_diag_cleanup
      use mpas_atm_threading, only : mpas_atm_threading_finalize
      use atm_time_integration, only : mpas_atm_dynamics_finalize
      use mpas_atm_halos, only: atm_destroy_halo_groups
   
#ifdef DO_PHYSICS
      use mpas_atmphys_finalize
#endif

#ifdef DO_CHEMISTRY
      use mpas_atm_chemistry, only: chemistry_finalize
#endif

      implicit none
   
      type (domain_type), intent(inout) :: domain
      type (block_type), pointer :: block_ptr 
      integer :: ierr

      ierr = 0

      clock => domain % clock
      mpas_log_info => domain % logInfo

      !
      ! Finalize the dynamics
      !
      call mpas_atm_dynamics_finalize(domain)

      !
      ! Finalize chemistry
      !
#ifdef DO_CHEMISTRY
      call chemistry_finalize()
#endif

      call mpas_atm_diag_cleanup()

      call mpas_destroy_clock(clock, ierr)
      call mpas_decomp_destroy_decomp_list(domain % decompositions)

#ifdef DO_PHYSICS
         block_ptr => domain % blocklist
         do while (associated(block_ptr))
            call atmphys_finalize(block_ptr%configs)

            block_ptr => block_ptr%next
         end do
#endif

      !
      ! Destroy halo exchange groups
      !
      call atm_destroy_halo_groups(domain, ierr)
      if (ierr /= 0) then
         call mpas_log_write('Failed to destroy halo exchange groups.', messageType=MPAS_LOG_ERR)
      end if

      !
      ! Finalize threading
      !
      call mpas_atm_threading_finalize(domain % blocklist)

      call mpas_log_write('')
      call mpas_log_write('********************************************************')
      call mpas_log_write('   Finished running the atmosphere core')
      call mpas_log_write('********************************************************')
   
   end function atm_core_finalize


   subroutine atm_compute_mesh_scaling(mesh, configs)

      implicit none

      type (mpas_pool_type), intent(inout) :: mesh
      type (mpas_pool_type), intent(in) :: configs

      integer :: iCell,iEdge, cell1, cell2
      integer, pointer :: nEdges, nCells
      integer, dimension(:,:), pointer :: cellsOnEdge
      real (kind=RKIND), dimension(:), pointer :: meshDensity, meshScalingDel2, meshScalingDel4
      real (kind=RKIND), dimension(:), pointer :: meshScalingRegionalCell, meshScalingRegionalEdge
      logical, pointer :: config_h_ScaleWithMesh

      call mpas_pool_get_array(mesh, 'meshDensity', meshDensity)
      call mpas_pool_get_array(mesh, 'meshScalingDel2', meshScalingDel2)
      call mpas_pool_get_array(mesh, 'meshScalingDel4', meshScalingDel4)
      call mpas_pool_get_array(mesh, 'meshScalingRegionalCell', meshScalingRegionalCell)
      call mpas_pool_get_array(mesh, 'meshScalingRegionalEdge', meshScalingRegionalEdge)
      call mpas_pool_get_array(mesh, 'cellsOnEdge', cellsOnEdge)

      call mpas_pool_get_dimension(mesh, 'nEdges', nEdges)
      call mpas_pool_get_dimension(mesh, 'nCells', nCells)

      call mpas_pool_get_config(configs, 'config_h_ScaleWithMesh', config_h_ScaleWithMesh)

      !
      ! Compute the scaling factors to be used in the del2 and del4 dissipation
      !
      meshScalingDel2(:) = 1.0
      meshScalingDel4(:) = 1.0
      if (config_h_ScaleWithMesh) then
         do iEdge=1,nEdges
            cell1 = cellsOnEdge(1,iEdge)
            cell2 = cellsOnEdge(2,iEdge)
            meshScalingDel2(iEdge) = 1.0 / ( (meshDensity(cell1) + meshDensity(cell2) )/2.0)**0.25
            meshScalingDel4(iEdge) = 1.0 / ( (meshDensity(cell1) + meshDensity(cell2) )/2.0)**0.75
         end do
      end if

      !
      ! Compute the scaling factors to be used in relaxation zone of regional configuration
      !
      meshScalingRegionalCell(:) = 1.0
      meshScalingRegionalEdge(:) = 1.0
      if (config_h_ScaleWithMesh) then
         do iEdge=1,nEdges
            cell1 = cellsOnEdge(1,iEdge)
            cell2 = cellsOnEdge(2,iEdge)
            meshScalingRegionalEdge(iEdge) = 1.0 / ( (meshDensity(cell1) + meshDensity(cell2) )/2.0)**0.25
         end do

         do iCell=1,nCells
            meshScalingRegionalCell(iCell) = 1.0 / (meshDensity(iCell))**0.25
         end do
      end if

   end subroutine atm_compute_mesh_scaling


   subroutine atm_compute_signs(mesh)

      implicit none

      type (mpas_pool_type), intent(inout) :: mesh

      integer :: i, j, iCell, iVtx
      integer, pointer :: nCells, nVertices, nEdges, vertexDegree
      integer, dimension(:), pointer :: nEdgesOnCell
      integer, dimension(:,:), pointer :: edgesOnVertex, verticesOnEdge, cellsOnEdge, edgesOnCell
      integer, dimension(:,:), pointer :: verticesOnCell, cellsOnVertex, kiteForCell
      real (kind=RKIND), dimension(:,:), pointer :: edgesOnVertex_sign, edgesOnCell_sign
      real (kind=RKIND), dimension(:,:,:), pointer :: zb, zb3, zb_cell, zb3_cell


      call mpas_pool_get_array(mesh, 'edgesOnVertex', edgesOnVertex)
      call mpas_pool_get_array(mesh, 'verticesOnEdge', verticesOnEdge)
      call mpas_pool_get_array(mesh, 'nEdgesOnCell', nEdgesOnCell)
      call mpas_pool_get_array(mesh, 'edgesOnCell', edgesOnCell)
      call mpas_pool_get_array(mesh, 'cellsOnEdge', cellsOnEdge)
      call mpas_pool_get_array(mesh, 'verticesOnCell', verticesOnCell)
      call mpas_pool_get_array(mesh, 'cellsOnVertex', cellsOnVertex)
      call mpas_pool_get_array(mesh, 'edgesOnVertex_sign', edgesOnVertex_sign)
      call mpas_pool_get_array(mesh, 'edgesOnCell_sign', edgesOnCell_sign)
      call mpas_pool_get_array(mesh, 'kiteForCell', kiteForCell)
      call mpas_pool_get_array(mesh, 'zb', zb)
      call mpas_pool_get_array(mesh, 'zb3', zb3)
      call mpas_pool_get_array(mesh, 'zb_cell', zb_cell)
      call mpas_pool_get_array(mesh, 'zb3_cell', zb3_cell)

      call mpas_pool_get_dimension(mesh, 'nCells', nCells)
      call mpas_pool_get_dimension(mesh, 'nVertices', nVertices)
      call mpas_pool_get_dimension(mesh, 'nEdges', nEdges)
      call mpas_pool_get_dimension(mesh, 'vertexDegree', vertexDegree)


      do iVtx=1,nVertices
         do i=1,vertexDegree
            if (edgesOnVertex(i,iVtx) <= nEdges) then
               if (iVtx == verticesOnEdge(2,edgesOnVertex(i,iVtx))) then
                  edgesOnVertex_sign(i,iVtx) = 1.0
               else
                  edgesOnVertex_sign(i,iVtx) = -1.0
               end if
            else
               edgesOnVertex_sign(i,iVtx) = 0.0
            end if
         end do
      end do

      do iCell=1,nCells
         do i=1,nEdgesOnCell(iCell)
            if (edgesOnCell(i,iCell) <= nEdges) then
               if (iCell == cellsOnEdge(1,edgesOnCell(i,iCell))) then
                  edgesOnCell_sign(i,iCell) = 1.0
                  zb_cell(:,i,iCell) = zb(:,1,edgesOnCell(i,iCell))
                  zb3_cell(:,i,iCell) = zb3(:,1,edgesOnCell(i,iCell))
               else
                  edgesOnCell_sign(i,iCell) = -1.0
                  zb_cell(:,i,iCell) = zb(:,2,edgesOnCell(i,iCell))
                  zb3_cell(:,i,iCell) = zb3(:,2,edgesOnCell(i,iCell))
               end if
            else
               edgesOnCell_sign(i,iCell) = 0.0
            end if
         end do
      end do

      do iCell=1,nCells
         do i=1,nEdgesOnCell(iCell)
            iVtx = verticesOnCell(i,iCell)
            if (iVtx <= nVertices) then
               do j=1,vertexDegree
                  if (iCell == cellsOnVertex(j,iVtx)) then
                     kiteForCell(i,iCell) = j
                     exit
                  end if
               end do
               if (j > vertexDegree) then
                  call mpas_log_write('Unexpected error while identifying kiteForCell', messageType=MPAS_LOG_ERR)
               end if
            else
               kiteForCell(i,iCell) = 1
            end if
         end do
      end do

   end subroutine atm_compute_signs


   subroutine atm_compute_damping_coefs(mesh, configs)

      implicit none

      type (mpas_pool_type), intent(inout) :: mesh
      type (mpas_pool_type), intent(in) :: configs

      integer :: iCell, k
      integer, pointer :: nCells, nVertLevels
      real (kind=RKIND), pointer :: config_xnutr, config_zd
      real (kind=RKIND) :: z, zt, m1, pii
      real (kind=RKIND), dimension(:,:), pointer :: dss, zgrid

      real (kind=RKIND), dimension(:), pointer :: rdzw, etp, etm, ewp, ewm
      real (kind=RKIND), pointer  :: max_coeff, min_coeff, transition_lower_bound, transition_upper_bound
      real (kind=RKIND)  ::  transition_width 
      real (kind=RKIND), allocatable, dimension(:) :: height_u_levels, epssm_coeff_u, height_w_levels, epssm_coeff_w

      m1 = -1.0
      pii = acos(m1)

      call mpas_pool_get_dimension(mesh, 'nCells', nCells)
      call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)

      call mpas_pool_get_array(mesh, 'dss', dss)
      call mpas_pool_get_array(mesh, 'zgrid', zgrid)

      call mpas_pool_get_config(configs, 'config_zd', config_zd)
      call mpas_pool_get_config(configs, 'config_xnutr', config_xnutr)

      dss(:,:) = 0.0
      do iCell=1,nCells
         zt = zgrid(nVertLevels+1,iCell)
         do k=1,nVertLevels
            z = 0.5*(zgrid(k,iCell) + zgrid(k+1,iCell))
            if (z > config_zd) then
               dss(k,iCell) = config_xnutr*sin(0.5*pii*(z-config_zd)/(zt-config_zd))**2.0
            end if
         end do
      end do

      call mpas_pool_get_array(mesh, 'rdzw', rdzw)
      call mpas_pool_get_array(mesh, 'etp', etp)
      call mpas_pool_get_array(mesh, 'etm', etm)
      call mpas_pool_get_array(mesh, 'ewp', ewp)
      call mpas_pool_get_array(mesh, 'ewm', ewm)
      call mpas_pool_get_config(configs, 'config_epssm_minimum', min_coeff)
      call mpas_pool_get_config(configs, 'config_epssm_maximum', max_coeff)
      call mpas_pool_get_config(configs, 'config_epssm_transition_bottom_z', transition_lower_bound)
      call mpas_pool_get_config(configs, 'config_epssm_transition_top_z', transition_upper_bound)

      allocate(height_u_levels(nVertLevels))
      allocate(epssm_coeff_u(nVertLevels))
      allocate(height_w_levels(nVertLevels+1))
      allocate(epssm_coeff_w(nVertLevels+1))

      transition_width = transition_upper_bound - transition_lower_bound

! initialization for heights of coordinate at u and w levels  
! These are the heights of the computational coordinate zeta
      
      height_w_levels(:) = 0.0_RKIND
      do k =1, nVertLevels
        height_w_levels(k+1) = height_w_levels(k) + 1.0_RKIND/rdzw(k)
        height_u_levels(k) = 0.5*(height_w_levels(k+1) + height_w_levels(k)) 
      enddo
 
! Height dependent values of epssm; profiles stored in etp, etm, ewp, and ewm, 

#ifdef MPAS_DEBUG
      call mpas_log_write(' setting epssm coefficients ')
      call mpas_log_write(' minimum epssm: $r ',realArgs=(/min_coeff/))
      call mpas_log_write(' maximum epssm: $r ',realArgs=(/max_coeff/))
      call mpas_log_write(' transition lower bound (m): $r ',realArgs=(/transition_lower_bound/))
      call mpas_log_write(' transition upper bound (m): $r ',realArgs=(/transition_upper_bound/))
      call mpas_log_write(' ')
#endif

      do k = 1,nVertLevels
         if(height_u_levels(k).le.transition_lower_bound) then
            epssm_coeff_u(k) = min_coeff
         else if(height_u_levels(k).ge.transition_upper_bound)  then
            epssm_coeff_u(k) = max_coeff
         else   
            z = (height_u_levels(k)-transition_lower_bound)/transition_width
            epssm_coeff_u(k) = min_coeff + sin(0.5_RKIND*pii*z)**2*(max_coeff-min_coeff)
         end if 
         etp(k) = 0.5*(1.0 + epssm_coeff_u(k)) 
         etm(k) = 0.5*(1.0 - epssm_coeff_u(k))
#ifdef MPAS_DEBUG
         call mpas_log_write('k, etp, etm $i $r $r ',intArgs=(/k/),realArgs=(/etp(k),etm(k)/))
#endif
      end do
      do k= 1,nVertlevels+1
         if(height_w_levels(k).le.transition_lower_bound) then
            epssm_coeff_w(k) = min_coeff
         else if(height_w_levels(k).ge.transition_upper_bound)  then
            epssm_coeff_w(k) = max_coeff
         else   
            z = (height_w_levels(k)-transition_lower_bound)/transition_width
            epssm_coeff_w(k) = min_coeff + sin(0.5_RKIND*pii*z)**2*(max_coeff-min_coeff)
         end if 
         ewp(k) = 0.5*(1.0 + epssm_coeff_w(k)) 
         ewm(k) = 0.5*(1.0 - epssm_coeff_w(k)) 
#ifdef MPAS_DEBUG
         call mpas_log_write('k, ewp, ewm $i $r $r ',intArgs=(/k/),realArgs=(/ewp(k),ewm(k)/))
#endif
      end do

      deallocate(height_u_levels)
      deallocate(epssm_coeff_u)
      deallocate(height_w_levels)
      deallocate(epssm_coeff_w)

   end subroutine atm_compute_damping_coefs


   subroutine atm_adv_coef_compression( mesh )

      implicit none

      type (mpas_pool_type), intent(inout) :: mesh


      real (kind=RKIND), dimension(:,:,:), pointer :: deriv_two
      real (kind=RKIND), dimension(:,:), pointer :: adv_coefs, adv_coefs_3rd
      integer, dimension(:,:), pointer :: cellsOnCell, cellsOnEdge, advCellsForEdge
      integer, dimension(:), pointer :: nEdgesOnCell, nAdvCellsForEdge
      real (kind=RKIND), dimension(:), pointer :: dcEdge, dvEdge

      integer :: cell1, cell2, iEdge, n, i, j, j_in, iCell
      integer, pointer :: nCells, nEdges
      integer :: cell_list(20)
      logical :: addcell


      call mpas_pool_get_array(mesh, 'deriv_two', deriv_two)
      call mpas_pool_get_array(mesh, 'adv_coefs', adv_coefs)
      call mpas_pool_get_array(mesh, 'adv_coefs_3rd', adv_coefs_3rd)
      call mpas_pool_get_array(mesh, 'cellsOnCell', cellsOnCell)
      call mpas_pool_get_array(mesh, 'cellsOnEdge', cellsOnEdge)
      call mpas_pool_get_array(mesh, 'advCellsForEdge', advCellsForEdge)
      call mpas_pool_get_array(mesh, 'nEdgesOnCell', nEdgesOnCell)
      call mpas_pool_get_array(mesh, 'nAdvCellsForEdge', nAdvCellsForEdge)
      call mpas_pool_get_array(mesh, 'dcEdge', dcEdge)
      call mpas_pool_get_array(mesh, 'dvEdge', dvEdge)

      call mpas_pool_get_dimension(mesh, 'nCells', nCells)
      call mpas_pool_get_dimension(mesh, 'nEdges', nEdges)

      do iEdge = 1, nEdges
         nAdvCellsForEdge(iEdge) = 0
         cell1 = cellsOnEdge(1,iEdge)
         cell2 = cellsOnEdge(2,iEdge)
         !
         ! do only if this edge flux is needed to update owned cells
         !
         if (cell1 <= nCells .or. cell2 <= nCells) then
 
            cell_list(1) = cell1
            cell_list(2) = cell2
            n = 2 
  
          !  add cells surrounding cell 1.  n is number of cells currently in list
            do i = 1, nEdgesOnCell(cell1)
               if (cellsOnCell(i,cell1) /= cell2) then
                  n = n + 1
                  cell_list(n) = cellsOnCell(i,cell1)
               end if
            end do
  
          !  add cells surrounding cell 2 (brute force approach)
            do iCell = 1, nEdgesOnCell(cell2)
               addcell = .true.
               do i=1,n
                  if (cell_list(i) == cellsOnCell(iCell,cell2)) addcell = .false.
               end do
               if (addcell) then
                  n = n+1
                  cell_list(n) = cellsOnCell(iCell,cell2)
               end if
            end do
  
  
            nAdvCellsForEdge(iEdge) = n
            do iCell = 1, nAdvCellsForEdge(iEdge)
               advCellsForEdge(iCell,iEdge) = cell_list(iCell)
            end do
  
          ! we have the ordered list, now construct coefficients
  
            adv_coefs(:,iEdge) = 0.
            adv_coefs_3rd(:,iEdge) = 0.
          
          ! pull together third and fourth order contributions to the flux
          ! first from cell1
  
            j_in = 0
            do j=1, n
               if( cell_list(j) == cell1 ) j_in = j
            end do
            adv_coefs    (j_in,iEdge) = adv_coefs    (j_in,iEdge) + deriv_two(1,1,iEdge)
            adv_coefs_3rd(j_in,iEdge) = adv_coefs_3rd(j_in,iEdge) + deriv_two(1,1,iEdge)
  
            do iCell = 1, nEdgesOnCell(cell1)
               j_in = 0
               do j=1, n
                 if( cell_list(j) == cellsOnCell(iCell,cell1) ) j_in = j
               end do
               adv_coefs    (j_in,iEdge) = adv_coefs    (j_in,iEdge) + deriv_two(iCell+1,1,iEdge)
               adv_coefs_3rd(j_in,iEdge) = adv_coefs_3rd(j_in,iEdge) + deriv_two(iCell+1,1,iEdge)
            end do
  
          ! pull together third and fourth order contributions to the flux
          ! now from cell2
  
            j_in = 0
            do j=1, n
               if( cell_list(j) == cell2 ) j_in = j
            end do
            adv_coefs    (j_in,iEdge) = adv_coefs    (j_in,iEdge) + deriv_two(1,2,iEdge)
            adv_coefs_3rd(j_in,iEdge) = adv_coefs_3rd(j_in,iEdge) - deriv_two(1,2,iEdge)
  
            do iCell = 1, nEdgesOnCell(cell2)
               j_in = 0
               do j=1, n
                  if( cell_list(j) == cellsOnCell(iCell,cell2) ) j_in = j
               end do
               adv_coefs    (j_in,iEdge) = adv_coefs    (j_in,iEdge) + deriv_two(iCell+1,2,iEdge)
               adv_coefs_3rd(j_in,iEdge) = adv_coefs_3rd(j_in,iEdge) - deriv_two(iCell+1,2,iEdge)
            end do
  
            do j = 1,n
               adv_coefs    (j,iEdge) = - (dcEdge(iEdge) **2) * adv_coefs    (j,iEdge) / 12.
               adv_coefs_3rd(j,iEdge) = - (dcEdge(iEdge) **2) * adv_coefs_3rd(j,iEdge) / 12.
            end do
  
          ! 2nd order centered contribution - place this in the main flux weights
  
            j_in = 0
            do j=1, n
               if( cell_list(j) == cell1 ) j_in = j
            end do
            adv_coefs(j_in,iEdge) = adv_coefs(j_in,iEdge) + 0.5
  
            j_in = 0
            do j=1, n
               if( cell_list(j) == cell2 ) j_in = j
            end do
            adv_coefs(j_in,iEdge) = adv_coefs(j_in,iEdge) + 0.5
  
          !  multiply by edge length - thus the flux is just dt*ru times the results of the vector-vector multiply
  
            do j=1,n
               adv_coefs    (j,iEdge) = dvEdge(iEdge) * adv_coefs    (j,iEdge)
               adv_coefs_3rd(j,iEdge) = dvEdge(iEdge) * adv_coefs_3rd(j,iEdge)
            end do
 
         end if  ! only do for edges of owned-cells
         
      end do ! end loop over edges

   end subroutine atm_adv_coef_compression


   subroutine atm_couple_coef_3rd_order(mesh, configs)

      implicit none

      type (mpas_pool_type), intent(inout) :: mesh
      type (mpas_pool_type), intent(in) :: configs

      real (kind=RKIND), dimension(:,:), pointer :: adv_coefs_3rd
      real (kind=RKIND), dimension(:,:,:), pointer :: zb3_cell
      real (kind=RKIND), pointer :: config_coef_3rd_order

      call mpas_pool_get_array(mesh, 'zb3_cell', zb3_cell)
      call mpas_pool_get_array(mesh, 'adv_coefs_3rd', adv_coefs_3rd)

      call mpas_pool_get_config(configs, 'config_coef_3rd_order', config_coef_3rd_order)

      adv_coefs_3rd(:,:) = config_coef_3rd_order * adv_coefs_3rd(:,:)
      zb3_cell(:,:,:) = config_coef_3rd_order * zb3_cell(:,:,:)

   end subroutine atm_couple_coef_3rd_order


   !-----------------------------------------------------------------------
   !  routine mpas_atm_run_compatibility
   !
   !> \brief Checks input fields and options for compatibility
   !> \author Michael Duda
   !> \date   22 October 2018
   !> \details
   !>  This routine checks the input fields and run-time options provided
   !>  by the user for compatibility. For example, two run-time options may
   !>  be mutually exclusive, or an option may require that a certain input
   !>  field is provided.
   !>
   !>  A value of 0 is returned if there are no incompatibilities among
   !>  the provided input fields and run-time options, and a non-zero value
   !>  otherwise.
   !
   !-----------------------------------------------------------------------
   subroutine mpas_atm_run_compatibility(dminfo, blockList, streamManager, ierr)

#ifdef DO_PHYSICS
      use mpas_atmphys_control, only : physics_compatibility_check
#endif
      use mpas_atm_boundaries, only : mpas_atm_bdy_checks
      use atm_time_integration, only : mpas_atm_dynamics_checks

      implicit none

      type (dm_info), pointer :: dminfo
      type (block_type), pointer :: blockList
      type (MPAS_streamManager_type), pointer :: streamManager
      integer, intent(out) :: ierr

      integer :: local_ierr

      ierr = 0

#ifdef DO_PHYSICS
      !
      ! Physics specific checks found in physics/mpas_atmphys_control.F
      !
      call physics_compatibility_check(dminfo, blockList, streamManager, local_ierr)
      ierr = ierr + local_ierr
#endif

      !
      ! Checks for limited-area simulations
      !
      call mpas_atm_bdy_checks(dminfo, blockList, streamManager, local_ierr)
      ierr = ierr + local_ierr

      !
      ! Checks for dynamics options
      !
      call mpas_atm_dynamics_checks(dminfo, blockList, streamManager, local_ierr)
      ierr = ierr + local_ierr

   end subroutine mpas_atm_run_compatibility


   subroutine atm_initialize_deformation_weights(mesh, nCells, on_a_sphere, sphere_radius)

!
! compute the cell coefficients for the deformation calculations
! WCS, 13 July 2010
!

      use mpas_vector_operations, only : mpas_fix_periodicity
      use mpas_timer, only : mpas_timer_start, mpas_timer_stop
      use mpas_geometry_utils, only : mpas_sphere_angle, mpas_plane_angle, mpas_arc_length

      implicit none

      type (mpas_pool_type), intent(inout) :: mesh
      integer, intent(in) :: nCells
      logical, intent(in) :: on_a_sphere
      real (kind=RKIND), intent(in) :: sphere_radius

!  local variables

      real (kind=RKIND), dimension(:,:), pointer :: deformation_coef_c2, deformation_coef_s2, deformation_coef_cs
      real (kind=RKIND), dimension(:,:), pointer :: deformation_coef_c, deformation_coef_s
      integer, dimension(:,:), pointer :: cellsOnEdge, edgesOnCell, cellsOnCell, verticesOnCell
      integer, dimension(:), pointer :: nEdgesOnCell
      real (kind=RKIND), dimension(:), pointer :: xCell, yCell, zCell
      real (kind=RKIND), dimension(:), pointer :: xVertex, yVertex, zVertex
      real (kind=RKIND), dimension(:), pointer :: xEdge, yEdge, zEdge, angleEdge

      real (kind=RKIND), dimension(nCells) :: theta_abs

      real (kind=RKIND), dimension(25) :: xc, yc, zc ! cell center coordinates
      real (kind=RKIND), dimension(25) :: thetav, thetat, dl_sphere
      real (kind=RKIND) :: dl
      integer :: i, ip1, ip2, n
      integer :: iCell
      real (kind=RKIND) :: pii
      real (kind=RKIND), dimension(25) :: xp, yp
      real (kind=RKIND) :: xe, ye

      integer, dimension(25) :: cell_list

      integer :: iv, ie
      logical :: do_the_cell
      real (kind=RKIND) :: area_cell, sint2, cost2, sint_cost, dx, dy

      real(kind=RKIND), pointer :: x_period, y_period


      call mpas_pool_get_config(mesh, 'x_period', x_period)
      call mpas_pool_get_config(mesh, 'y_period', y_period)

      call mpas_pool_get_array(mesh, 'deformation_coef_c2', deformation_coef_c2)
      call mpas_pool_get_array(mesh, 'deformation_coef_s2', deformation_coef_s2)
      call mpas_pool_get_array(mesh, 'deformation_coef_cs', deformation_coef_cs)
      call mpas_pool_get_array(mesh, 'deformation_coef_c', deformation_coef_c)
      call mpas_pool_get_array(mesh, 'deformation_coef_s', deformation_coef_s)
      call mpas_pool_get_array(mesh, 'nEdgesOnCell', nEdgesOnCell)
      call mpas_pool_get_array(mesh, 'cellsOnEdge', cellsOnEdge)
      call mpas_pool_get_array(mesh, 'edgesOnCell', edgesOnCell)
      call mpas_pool_get_array(mesh, 'cellsOnCell', cellsOnCell)
      call mpas_pool_get_array(mesh, 'verticesOnCell', verticesOnCell)
      call mpas_pool_get_array(mesh, 'xCell', xCell)
      call mpas_pool_get_array(mesh, 'yCell', yCell)
      call mpas_pool_get_array(mesh, 'zCell', zCell)
      call mpas_pool_get_array(mesh, 'xVertex', xVertex)
      call mpas_pool_get_array(mesh, 'yVertex', yVertex)
      call mpas_pool_get_array(mesh, 'zVertex', zVertex)
      call mpas_pool_get_array(mesh, 'xEdge', xEdge)
      call mpas_pool_get_array(mesh, 'yEdge', yEdge)
      call mpas_pool_get_array(mesh, 'zEdge', zEdge)
      call mpas_pool_get_array(mesh, 'angleEdge', angleEdge)

      deformation_coef_c2(:,:) = 0.
      deformation_coef_s2(:,:) = 0.
      deformation_coef_cs(:,:) = 0.
      deformation_coef_c(:,:) = 0.
      deformation_coef_s(:,:) = 0.

      pii = 2.*asin(1.0)

      do iCell = 1, nCells

         cell_list(1) = iCell
         do i=2,nEdgesOnCell(iCell)+1
            cell_list(i) = cellsOnCell(i-1,iCell)
         end do
         n = nEdgesOnCell(iCell) + 1

!  check to see if we are reaching outside the halo

         do_the_cell = .true.
         do i=1,n
            if (cell_list(i) > nCells) do_the_cell = .false.
         end do


         if (.not. do_the_cell) cycle

         !  compute poynomial fit for this cell if all needed neighbors exist

         if (on_a_sphere) then

            ! xc holds the center point and the vertex points of the cell,
            ! normalized to a sphere or radius 1.

            xc(1) = xCell(iCell)/sphere_radius
            yc(1) = yCell(iCell)/sphere_radius
            zc(1) = zCell(iCell)/sphere_radius

            do i=2,n
               iv = verticesOnCell(i-1,iCell)
               xc(i) = xVertex(iv)/sphere_radius
               yc(i) = yVertex(iv)/sphere_radius
               zc(i) = zVertex(iv)/sphere_radius
            end do

            !
            ! In case the current cell center lies at exactly z=1.0, the sphere_angle() routine
            !    may generate an FPE since the triangle it is given will have a zero side length
            !    adjacent to the vertex whose angle we are trying to find; in this case, simply
            !    set the value of theta_abs directly
            !
            if (zc(1) == 1.0) then
               theta_abs(iCell) = pii/2.
            else
               ! theta_abs is the angle to the first vertex from the center, normalized so that
               ! an eastward pointing vector has a angle of 0.
               theta_abs(iCell) =  pii/2. - mpas_sphere_angle( xc(1), yc(1), zc(1),  &
                                                               xc(2), yc(2), zc(2),  &
                                                               0.0_RKIND, 0.0_RKIND, 1.0_RKIND )
            end if

            ! here we are constructing the tangent-plane cell.
            ! thetat is the angle in the (x,y) tangent-plane coordinate from
            ! the cell center to each vertex, normalized so that an
            ! eastward pointing vector has a angle of 0.

            ! dl_sphere is the spherical distance from the cell center
            ! to the sphere vertex points for the cell.

            thetat(1) = theta_abs(iCell)
            do i=1,n-1

               ip2 = i+2
               if (ip2 > n) ip2 = 2

               thetav(i) = mpas_sphere_angle( xc(1),   yc(1),   zc(1),    &
                                              xc(i+1), yc(i+1), zc(i+1),  &
                                              xc(ip2), yc(ip2), zc(ip2)   )
               dl_sphere(i) = sphere_radius*mpas_arc_length( xc(1),   yc(1),   zc(1),  &
                                                             xc(i+1), yc(i+1), zc(i+1) )
               if(i.gt.1) thetat(i) = thetat(i-1)+thetav(i-1)
            end do

            ! xp and yp are the tangent-plane vertex points with the cell center at (0,0)

            do i=1,n-1
               xp(i) = cos(thetat(i)) * dl_sphere(i)
               yp(i) = sin(thetat(i)) * dl_sphere(i)
            end do

         else     ! On an x-y plane

            do i=1,n-1
               iv = verticesOnCell(i,iCell)
               xp(i) = mpas_fix_periodicity(xVertex(iv),xCell(iCell),x_period) - xCell(iCell)
               yp(i) = mpas_fix_periodicity(yVertex(iv),yCell(iCell),y_period) - yCell(iCell)
            end do

            do i=1,n-1
               ie = edgesOnCell(i,iCell)
               xe = mpas_fix_periodicity(xEdge(ie),xCell(iCell),x_period) - xCell(iCell)
               ye = mpas_fix_periodicity(yEdge(ie),yCell(iCell),y_period) - yCell(iCell)
               thetat(i) = atan2(ye,xe)
            end do

            theta_abs(iCell) = thetat(1)

         end if

         ! (1) compute cell area on the tangent plane used in the integrals
         ! (2) compute angle of cell edge normal vector.  here we are repurposing thetat
         thetat(1) = theta_abs(iCell)

         do i=2,n-1
            ip1 = i+1
            if (ip1 == n) ip1 = 1
            thetat(i) = mpas_plane_angle( 0.0_RKIND, 0.0_RKIND, 0.0_RKIND,  &
                                          xp(i)-xp(i-1), yp(i)-yp(i-1), 0.0_RKIND,  &
                                          xp(ip1)-xp(i), yp(ip1)-yp(i), 0.0_RKIND,  &
                                          0.0_RKIND, 0.0_RKIND, 1.0_RKIND)
            thetat(i) = thetat(i) + thetat(i-1)
         end do

         area_cell = 0.
         do i=1,n-1
            ip1 = i+1
            if (ip1 == n) ip1 = 1
            dx = xp(ip1)-xp(i)
            dy = yp(ip1)-yp(i)
            area_cell = area_cell + 0.25*(xp(i)+xp(ip1))*(yp(ip1)-yp(i)) - 0.25*(yp(i)+yp(ip1))*(xp(ip1)-xp(i))
            thetat(i) = atan2(dy,dx)-pii/2.
         end do

         ! coefficients - see documentation for the formulas.

         do i=1,n-1
            ip1 = i+1
            if (ip1 == n) ip1 = 1
            dl = sqrt((xp(ip1)-xp(i))**2 + (yp(ip1)-yp(i))**2)
            sint2 = (sin(thetat(i)))**2
            cost2 = (cos(thetat(i)))**2
            sint_cost = sin(thetat(i))*cos(thetat(i))
            deformation_coef_c2(i,iCell) = dl*cost2/area_cell
            deformation_coef_s2(i,iCell) = dl*sint2/area_cell
            deformation_coef_cs(i,iCell) = dl*sint_cost/area_cell
            deformation_coef_c(i,iCell) = dl*cos(thetat(i))/area_cell
            deformation_coef_s(i,iCell) = dl*sin(thetat(i))/area_cell
            if (cellsOnEdge(1,EdgesOnCell(i,iCell)) /= iCell) then
               deformation_coef_c2(i,iCell) = - deformation_coef_c2(i,iCell)
               deformation_coef_s2(i,iCell) = - deformation_coef_s2(i,iCell)
               deformation_coef_cs(i,iCell) = - deformation_coef_cs(i,iCell)
!               deformation_coef_c(i,iCell) = - deformation_coef_c(i,iCell)
!               deformation_coef_s(i,iCell) = - deformation_coef_s(i,iCell)
            end if

         end do

      end do

   end subroutine atm_initialize_deformation_weights

end module atm_core

