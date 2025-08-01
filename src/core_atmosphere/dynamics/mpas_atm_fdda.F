!==================================================================================================
!  MPAS four-dimensional data assimilation (FDDA) grid nudging.
!
!  O. Russell Bullock Jr. (send comments to bullock.russell@epa.gov).
!  2016-03-30.
!
!   subroutines in mpas_atm_fdda:
!   ----------------------------------------------
!   apply_fdda      : main fdda subroutine (called from subroutine physics_driver).
!   mpas_atm_update_fdda_state : subroutine to read in fdda input stream and save/shift to correct time-level
!   
!  
     
!   add-ons and modifications to sourcecode:
!   ----------------------------------------
!   Original sourcecode by O. Russell Bullock Jr. (bullock.russell@epa.gov) / 2016-03-30. 
!  
!   * Added various controls for length-scaled FDDA application 
!     O. Russell Bullock Jr. (bullock.russell@epa.gov) / 2016-08-11. 
!   * Modified to only require reads of "fdda_new" data arrays.  "fdda_old" is initially 
!     set to initial conditions.  At end of FDDA data interval, "fdda_old" is set to 
!     "fdda_new" and updated "fdda_new" data comes from FDDA input stream.
!     O. Russell Bullock Jr. (bullock.russell@epa.gov) / 2016-08-30.
!   * Modified to add variable transformation and adding nudging tendencies to tend_ru, tend_rtheta
!     May Wong (mwong@ucar.edu) / 2025-07-30
!   * Modified to add subroutine mpas_atm_update_fdda_state to update the fdda target data arrays
!     from the input stream.
!     May Wong (mwong@ucar.edu) / 2025-07-30
 
 module mpas_atm_fdda

      use mpas_kind_types
      use mpas_derived_types
      use mpas_pool_routines
     
      use mpas_atmphys_constants
      use mpas_atmphys_utilities
      use mpas_atmphys_vars
      use mpas_atmphys_todynamics, only : tend_toEdges
      use mpas_log, only : mpas_log_write
     
      implicit none
      private
      public:: apply_fdda, mpas_atm_update_fdda_state
     
      integer, private:: i,k
     
     
      contains

      !***********************************************************************
      !
      !  routine apply_fdda 
      !
      !> \brief   Applies FDDA nudging tendencies to the RHS tendencies for dynamics 
      !> \author  O. Russell Bullock Jr. 
      !> \date    30 March 2016
      !> \details 
      !>  This routine computes the FDDA weighted nudging tendencies and adds them 
      !>  to the 'physics' tendencies for the dynamics. 
      !> 
      !>  Variable transformation and adding to the physics tendencies were added.
      !>  May Wong (mwong@ucar.edu) / 2025-07-30
      !
      !-----------------------------------------------------------------------
      subroutine apply_fdda(block,xtime_s,configs,mesh,state,time_lev,fdda,diag,diag_physics,tend,tend_physics,tend_fdda, &
                       tend_ru, tend_rtheta)

	!input variables:
	 type(block_type), intent(in) :: block
	 type(mpas_pool_type),intent(in):: configs
	 type(mpas_pool_type),intent(in):: mesh
	 type(mpas_pool_type),intent(in):: state
	 type(mpas_pool_type),intent(in):: fdda 
	 type(mpas_pool_type),intent(in):: diag
	 type(mpas_pool_type),intent(in):: diag_physics
	 integer,intent(in):: time_lev
	 real(kind=RKIND),intent(in):: xtime_s

	!inout variables:
	 type(mpas_pool_type),intent(inout):: tend_physics
	 type(mpas_pool_type),intent(inout):: tend
	 type(mpas_pool_type),intent(inout):: tend_fdda

	 real (kind=RKIND), dimension(:,:), intent(inout) :: tend_ru
	 real (kind=RKIND), dimension(:,:), intent(inout) :: tend_rtheta

	!local pointers:
	 integer,pointer:: index_qv,fdda_t_min_layer,fdda_q_min_layer,fdda_uv_min_layer
	 integer,dimension(:),pointer :: kpbl
	 integer, pointer :: nCellsSolve, nVertLevels, nEdgesSolve 
	 real(kind=RKIND),pointer:: fdda_t_coef,fdda_q_coef,fdda_uv_coef
	 real(kind=RKIND),pointer:: fdda_int,fdda_scale_min,fdda_scale_max
	 real(kind=RKIND),pointer:: config_dt
	 real(kind=RKIND),pointer:: len_disp
	 real(kind=RKIND),dimension(:),pointer    :: meshDensity
	 real(kind=RKIND),dimension(:,:),pointer  :: rthfddaten,rqvfddaten
	 real(kind=RKIND),dimension(:,:),pointer  :: rufddaten,rvfddaten,rufddaten_Edge
	 real(kind=RKIND),dimension(:,:),pointer  :: qv_fdda_old,theta_fdda_old,u_fdda_old,v_fdda_old
	 real(kind=RKIND),dimension(:,:),pointer  :: qv_fdda_new,theta_fdda_new,u_fdda_new,v_fdda_new
	 real(kind=RKIND),dimension(:,:),pointer  :: u,v
	 real(kind=RKIND),dimension(:,:),pointer  :: qv
	 real(kind=RKIND),dimension(:,:),pointer  :: theta_m
	 real(kind=RKIND),dimension(:,:,:),pointer:: scalars, tend_scalars
	 real(kind=RKIND),dimension(:,:),pointer:: rho_zz
	 real(kind=RKIND),dimension(:,:),pointer:: rho_edge 
	 real (kind=RKIND), dimension(:,:), pointer :: tend_th  ! tendency for potential temperature
	 character(len=StrKIND),pointer:: fdda_scheme
	 logical,pointer:: fdda_t, fdda_t_in_pbl
	 logical,pointer:: fdda_q, fdda_q_in_pbl
	 logical,pointer:: fdda_uv,fdda_uv_in_pbl
	 logical,pointer:: config_do_restart

	!local variables:
	 real(kind=RKIND)  :: tfrac
	 real(kind=RKIND)  :: t_target,q_target,u_target,v_target
	 real(kind=RKIND)  :: sfac
	 real(kind=RKIND)  :: theta_p
	 real(kind=RKIND)  :: srange,dcEdge_m

	 integer :: i,k

	 logical :: first_call = .true.

	!local parameters:

	!==================================================================================================

	 call mpas_pool_get_config(configs,'config_do_restart' ,config_do_restart )
	 call mpas_pool_get_config(configs,'config_dt' ,config_dt )
	 call mpas_pool_get_config(configs,'config_fdda_scheme', fdda_scheme)
	 call mpas_pool_get_config(configs,'config_fdda_int',fdda_int)
	 call mpas_pool_get_config(configs,'config_fdda_t',fdda_t)
	 call mpas_pool_get_config(configs,'config_fdda_q',fdda_q)
	 call mpas_pool_get_config(configs,'config_fdda_uv',fdda_uv)
	 call mpas_pool_get_config(configs,'config_fdda_t_in_pbl',fdda_t_in_pbl)
	 call mpas_pool_get_config(configs,'config_fdda_q_in_pbl',fdda_q_in_pbl)
	 call mpas_pool_get_config(configs,'config_fdda_uv_in_pbl',fdda_uv_in_pbl)
	 call mpas_pool_get_config(configs,'config_fdda_t_min_layer',fdda_t_min_layer)
	 call mpas_pool_get_config(configs,'config_fdda_q_min_layer',fdda_q_min_layer)
	 call mpas_pool_get_config(configs,'config_fdda_uv_min_layer',fdda_uv_min_layer)
	 call mpas_pool_get_config(configs,'config_fdda_t_coef',fdda_t_coef)
	 call mpas_pool_get_config(configs,'config_fdda_q_coef',fdda_q_coef)
	 call mpas_pool_get_config(configs,'config_fdda_uv_coef',fdda_uv_coef)
	 call mpas_pool_get_config(configs,'config_fdda_scale_min',fdda_scale_min)
	 call mpas_pool_get_config(configs,'config_fdda_scale_max',fdda_scale_max)
	 call mpas_pool_get_config(configs,'config_len_disp'   ,len_disp)

	 call mpas_pool_get_array(mesh,'meshDensity',meshDensity)
	 call mpas_pool_get_dimension(mesh, 'nCellsSolve', nCellsSolve)
	 call mpas_pool_get_dimension(mesh, 'nEdgesSolve', nEdgesSolve)
	 call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)

	 call mpas_pool_get_array(fdda,'qv_fdda',qv_fdda_old      , 1)
	 call mpas_pool_get_array(fdda,'theta_fdda',theta_fdda_old, 1)
	 call mpas_pool_get_array(fdda,'u_fdda', u_fdda_old       , 1)
	 call mpas_pool_get_array(fdda,'v_fdda', v_fdda_old       , 1)
	 call mpas_pool_get_array(fdda,'qv_fdda',qv_fdda_new      , 2)
	 call mpas_pool_get_array(fdda,'theta_fdda',theta_fdda_new, 2)
	 call mpas_pool_get_array(fdda,'u_fdda', u_fdda_new       , 2)
	 call mpas_pool_get_array(fdda,'v_fdda', v_fdda_new       , 2)
	 call mpas_pool_get_array(tend_fdda,'rthfddaten',rthfddaten)
	 call mpas_pool_get_array(tend_fdda,'rqvfddaten',rqvfddaten)
	 call mpas_pool_get_array(tend_fdda,'rufddaten_Edge' ,rufddaten_Edge )
	 call mpas_pool_get_array(tend_fdda,'rufddaten' ,rufddaten )
	 call mpas_pool_get_array(tend_fdda,'rvfddaten' ,rvfddaten )
	 call mpas_pool_get_array(diag_physics,'kpbl',kpbl)
	 call mpas_pool_get_array(diag,'uReconstructZonal'     ,u)
	 call mpas_pool_get_array(diag,'uReconstructMeridional',v)
	 call mpas_pool_get_array(tend,'scalars_tend' , tend_scalars )

	 call mpas_pool_get_dimension(state,'index_qv',index_qv)
	 call mpas_pool_get_array(state,'scalars',scalars,time_lev)
	 qv => scalars(index_qv,:,:)
	 call mpas_pool_get_array(state,'theta_m',theta_m,time_lev)

	 call mpas_pool_get_array(state, 'rho_zz',   rho_zz, 2)
	 call mpas_pool_get_array(diag , 'rho_edge', rho_edge)

	 allocate(tend_th(nVertLevels,nCellsSolve))


	!calculate time fraction within FDDA target data interval
	 tfrac = mod(xtime_s,fdda_int)/fdda_int
         call mpas_log_write('inside FDDA: tfrac = $r', realArgs=(/tfrac/))

	 fdda_select: select case(fdda_scheme)

	    case ("scaled")

	       srange = fdda_scale_max-fdda_scale_min

	       DO i=1, nCellsSolve

		  dcEdge_m  = len_disp / meshDensity(i)**0.25_RKIND

		  if (dcEdge_m .lt. fdda_scale_max) then
		     if (dcEdge_m .lt. fdda_scale_min) then
			sfac = 0.0_RKIND
		     else
			sfac = (dcEdge_m-fdda_scale_min)/srange
		     endif
		  else
		     sfac = 1.0_RKIND
		  endif

		  DO k=1,nVertLevels
	   
		     theta_p = theta_m(k,i) / (1._RKIND + rvord * qv(k,i))
	   
		     if (fdda_t .and. k.gt.fdda_t_min_layer .and. (fdda_t_in_pbl .or. k.gt.kpbl(i))) then
			t_target = (1.0_RKIND-tfrac)*theta_fdda_old(k,i)+tfrac*theta_fdda_new(k,i)
			rthfddaten(k,i) = fdda_t_coef * ( t_target - theta_p ) * sfac
 
		     else
			rthfddaten(k,i) = 0._RKIND
		     endif
	   
		     if (fdda_q .and. k.gt.fdda_q_min_layer .and. (fdda_q_in_pbl .or. k.gt.kpbl(i))) then
			q_target = (1.0_RKIND-tfrac)*qv_fdda_old(k,i)+tfrac*qv_fdda_new(k,i)
			rqvfddaten(k,i) = fdda_q_coef * ( q_target - qv(k,i) ) * sfac
		     else
			rqvfddaten(k,i) = 0._RKIND
		     endif
	   
		     if (fdda_uv .and. k.gt.fdda_uv_min_layer .and. (fdda_uv_in_pbl .or. k.gt.kpbl(i))) then
			u_target = (1.0_RKIND-tfrac)*u_fdda_old(k,i)+tfrac*u_fdda_new(k,i)
			rufddaten(k,i) = fdda_uv_coef * ( u_target - u(k,i) ) * sfac
			v_target = (1.0_RKIND-tfrac)*v_fdda_old(k,i)+tfrac*v_fdda_new(k,i)
			rvfddaten(k,i) = fdda_uv_coef * ( v_target - v(k,i) ) * sfac
		     else
			rufddaten(k,i) = 0._RKIND
			rvfddaten(k,i) = 0._RKIND
		     endif
	   
		  ENDDO
	       ENDDO

	    case("analysis")

	       DO i = 1, nCellsSolve
		  DO k = 1, nVertLevels 
	 
		   theta_p = theta_m(k,i) / (1._RKIND + rvord * qv(k,i))
	 
		   if (fdda_t .and. k.gt.fdda_t_min_layer .and. (fdda_t_in_pbl .or. k.gt.kpbl(i))) then
		      t_target = (1.0_RKIND-tfrac)*theta_fdda_old(k,i)+tfrac*theta_fdda_new(k,i)
		      rthfddaten(k,i) = fdda_t_coef * ( t_target - theta_p )
                      if (i.eq.1 .and. k.eq.1 ) call mpas_log_write('t_target, theta_p, theta_fdda_old, theta_fdda_new = $r, $r, $r, $r', &
                                                   realArgs=(/t_target, theta_p, theta_fdda_old(k,i), theta_fdda_new(k,i)/) )
		   else
		      rthfddaten(k,i) = 0._RKIND
		   endif
	 
		   if (fdda_q .and. k.gt.fdda_q_min_layer .and. (fdda_q_in_pbl .or. k.gt.kpbl(i))) then
		      q_target = (1.0_RKIND-tfrac)*qv_fdda_old(k,i)+tfrac*qv_fdda_new(k,i)
		      rqvfddaten(k,i) = fdda_q_coef * ( q_target - qv(k,i) )
		   else
		      rqvfddaten(k,i) = 0._RKIND
		   endif
	 
		   if (fdda_uv .and. k.gt.fdda_uv_min_layer .and. (fdda_uv_in_pbl .or. k.gt.kpbl(i))) then
		      u_target = (1.0_RKIND-tfrac)*u_fdda_old(k,i)+tfrac*u_fdda_new(k,i)
		      rufddaten(k,i) = fdda_uv_coef * ( u_target - u(k,i) )
		      v_target = (1.0_RKIND-tfrac)*v_fdda_old(k,i)+tfrac*v_fdda_new(k,i)
		      rvfddaten(k,i) = fdda_uv_coef * ( v_target - v(k,i) )
		   else
		      rufddaten(k,i) = 0._RKIND
		      rvfddaten(k,i) = 0._RKIND
		   endif
	 
		  ENDDO
	       ENDDO
               call mpas_log_write('in apply_fdda: min,max rthfddaten = $r, $r', realArgs=(/minval(rthfddaten), maxval(rthfddaten)/) )
               call mpas_log_write('in apply_fdda: min,max rqvfddaten = $r, $r', realArgs=(/minval(rqvfddaten), maxval(rqvfddaten)/) )
               call mpas_log_write('in apply_fdda: min,max rufddaten = $r, $r', realArgs=(/minval(rufddaten), maxval(rufddaten)/) )
               call mpas_log_write('in apply_fdda: min,max rvfddaten = $r, $r', realArgs=(/minval(rvfddaten), maxval(rvfddaten)/) )

	    case default

	 end select fdda_select


	! MW: add transformation of the u,v wind nudging tendencies to those for horizontal normal velocity at the edge 
	 rufddaten_Edge(:,:) = 0.0_RKIND
	 call tend_toEdges(block, mesh, rufddaten, rvfddaten, rufddaten_Edge)

	! MW: add coupled tendencies for u on edges 
	 do i = 1, nEdgesSolve
	   do k  = 1, nVertLevels
	      tend_ru(k,i)=tend_ru(k,i)+rufddaten_Edge(k,i)*rho_edge(k,i)
	   enddo
	 enddo

	! MW: add coupled tendencies for other variables at cell centers 
	 tend_th = 0.0_RKIND
	 do i = 1, nCellsSolve
	   do k = 1, nVertLevels
	      tend_th(k,i) = tend_th(k,i) + rthfddaten(k,i)*rho_zz(k,i)
	      tend_scalars(index_qv,k,i) = tend_scalars(index_qv,k,i) + rqvfddaten(k,i)*rho_zz(k,i)
	   enddo
	 enddo


        ! if non-hydrostatic core, convert the tendency for the potential temperature to a 
        ! tendency for the modified potential temperature
         do i = 1, nCellsSolve
           do k = 1,nVertLevels
              tend_th(k,i) = (1. + rvord * scalars(index_qv,k,i)) * tend_th(k,i) &
                                 + rvord * theta_m(k,i) * rqvfddaten(k,i)*rho_zz(k,i) 
              tend_rtheta(k,i) = tend_rtheta(k,i) + tend_th(k,i)
           end do
         end do


         deallocate(tend_th)

      end subroutine apply_fdda


    !***********************************************************************
    !
    !  routine mpas_atm_update_fdda_state
    !
    !> \brief   Reads new fdda input data and updates the FDDA tendencies
    !> \author  Michael Duda, May Wong
    !> \date    30 July 2025
    !> \details 
    !>  This routine reads from the 'fdda' stream all variables in the 'fdda'
    !>  pool. When called with firstCall=.true., the latest time before the
    !>  present is read into time level 2 of the fdda pool; otherwise, the
    !>  contents of time level 2 are shifted to time level 1, the earliest
    !>  time strictly later than the present is read into time level 2, and
    !>  the tendencies for all fields in the fdda pool are computed and stored
    !>  in time level 1.
    !>
    !>  This subroutine for FDDA tendencies is based on mpas_atm_update_bdy_tend.
    !
    !-----------------------------------------------------------------------
    subroutine mpas_atm_update_fdda_state(clock, streamManager, block, firstCall, ierr)

        use mpas_constants, only : rvord
        use mpas_stream_manager, only : mpas_stream_mgr_read
        use mpas_log, only : mpas_log_write
        use mpas_derived_types, only : MPAS_STREAM_MGR_NOERR, MPAS_LOG_ERR
        use mpas_timekeeping, only : mpas_get_time

        implicit none

        type (mpas_clock_type), intent(in) :: clock
        type (MPAS_streamManager_type), intent(inout) :: streamManager
        type (block_type), intent(inout) :: block
        logical, intent(in) :: firstCall
        integer, intent(out) :: ierr

        character(len=StrKIND) :: lbc_intv_start_string
        character(len=StrKIND) :: lbc_intv_end_string

        type (mpas_pool_type), pointer :: mesh
        type (mpas_pool_type), pointer :: state
        type (mpas_pool_type), pointer :: fdda 
        real (kind=RKIND) :: dt

        integer, pointer :: nCells
        integer, pointer :: nEdges
        integer, pointer :: index_qv

        real (kind=RKIND), dimension(:,:), pointer :: u
        real (kind=RKIND), dimension(:,:), pointer :: ru
        real (kind=RKIND), dimension(:,:), pointer :: rho_edge
        real (kind=RKIND), dimension(:,:), pointer :: w
        real (kind=RKIND), dimension(:,:), pointer :: theta
        real (kind=RKIND), dimension(:,:), pointer :: rtheta_m
        real (kind=RKIND), dimension(:,:), pointer :: rho_zz
        real (kind=RKIND), dimension(:,:), pointer :: rho
        real (kind=RKIND), dimension(:,:,:), pointer :: scalars

        real (kind=RKIND), dimension(:,:), pointer :: qv_fdda 
        real (kind=RKIND), dimension(:,:), pointer :: theta_fdda 
        real (kind=RKIND), dimension(:,:), pointer :: u_fdda 
        real (kind=RKIND), dimension(:,:), pointer :: v_fdda 


        integer :: dd_intv, s_intv, sn_intv, sd_intv
        type (MPAS_Time_Type) :: currTime
        type (MPAS_TimeInterval_Type) :: lbc_interval
        character(len=StrKIND) :: read_time
        integer :: iEdge
        integer :: cell1, cell2


        ierr = 0

        call mpas_pool_get_subpool(block % structs, 'mesh', mesh)
        call mpas_pool_get_subpool(block % structs, 'state', state)
        call mpas_pool_get_subpool(block % structs, 'fdda', fdda)

        if (firstCall) then
            call MPAS_stream_mgr_read(streamManager, streamID='fdda', timeLevel=1, whence=MPAS_STREAM_LATEST_BEFORE, &
                                      actualWhen=read_time, ierr=ierr)
            if (ierr /= MPAS_STREAM_MGR_NOERR) then
               call mpas_log_write('Could not read from ''fdda'' stream on or before the current date '// &
                                   'to update target FDDA fields', messageType=MPAS_LOG_ERR)
               ierr = 1
            end if

            call MPAS_stream_mgr_read(streamManager, streamID='fdda', timeLevel=2, whence=MPAS_STREAM_EARLIEST_STRICTLY_AFTER, &
                                      actualWhen=read_time, ierr=ierr)
            if (ierr /= MPAS_STREAM_MGR_NOERR) then
               call mpas_log_write('Could not read from ''fdda'' stream after the current date '// &
                                   'to update target FDDA fields', messageType=MPAS_LOG_ERR)

               ierr = 1
            end if

        else
            call mpas_pool_shift_time_levels(fdda)
            call MPAS_stream_mgr_read(streamManager, streamID='fdda', timeLevel=2, whence=MPAS_STREAM_EARLIEST_STRICTLY_AFTER, &
                                      actualWhen=read_time, ierr=ierr)
            if (ierr /= MPAS_STREAM_MGR_NOERR) then
               call mpas_log_write('Could not read from ''fdda'' stream after the current date '// &
                                   'to update target FDDA fields', messageType=MPAS_LOG_ERR)

               ierr = 1
            end if
        end if
        if (ierr /= 0) then
           return
        end if

    end subroutine mpas_atm_update_fdda_state 


 end module mpas_atm_fdda
