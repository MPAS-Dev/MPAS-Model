!==================================================================================================
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
 public:: apply_fdda

 integer, private:: i,k


!MPAS grid nudging.
!O. Russell Bullock Jr. (send comments to bullock.russell@epa.gov).
!2016-03-30.
!
! subroutines in mpas_atm_fdda:
! ----------------------------------------------
! apply_fdda      : main fdda subroutine (called from subroutine physics_driver).
!

! add-ons and modifications to sourcecode:
! ----------------------------------------
! Original sourcecode by O. Russell Bullock Jr. (bullock.russell@epa.gov) / 2016-03-30. 
!
! * Added various controls for length-scaled FDDA application 
!   O. Russell Bullock Jr. (bullock.russell@epa.gov) / 2016-08-11. 
! * Modified to only require reads of "fdda_new" data arrays.  "fdda_old" is initially 
!   set to initial conditions.  At end of FDDA data interval, "fdda_old" is set to 
!   "fdda_new" and updated "fdda_new" data comes from FDDA input stream.
!   O. Russell Bullock Jr. (bullock.russell@epa.gov) / 2016-08-30.

 contains

!==================================================================================================
 subroutine apply_fdda(block,xtime_s,configs,mesh,state,time_lev,fdda,diag,diag_physics,tend,tend_physics, &
                       tend_ru, tend_rtheta)
!==================================================================================================

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
	 real(kind=RKIND),dimension(:,:),pointer  :: qv_fdda_old,th_fdda_old,u_fdda_old,v_fdda_old
	 real(kind=RKIND),dimension(:,:),pointer  :: qv_fdda_new,th_fdda_new,u_fdda_new,v_fdda_new
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

	 call mpas_pool_get_array(fdda,'qv_fdda_old',qv_fdda_old)
	 call mpas_pool_get_array(fdda,'th_fdda_old',th_fdda_old)
	 call mpas_pool_get_array(fdda,'u_fdda_old', u_fdda_old )
	 call mpas_pool_get_array(fdda,'v_fdda_old', v_fdda_old )
	 call mpas_pool_get_array(fdda,'qv_fdda_new',qv_fdda_new)
	 call mpas_pool_get_array(fdda,'th_fdda_new',th_fdda_new)
	 call mpas_pool_get_array(fdda,'u_fdda_new', u_fdda_new )
	 call mpas_pool_get_array(fdda,'v_fdda_new', v_fdda_new )
	 call mpas_pool_get_array(tend_physics,'rthfddaten',rthfddaten)
	 call mpas_pool_get_array(tend_physics,'rqvfddaten',rqvfddaten)
	 call mpas_pool_get_array(tend_physics,'rufddaten_Edge' ,rufddaten_Edge )
	 call mpas_pool_get_array(tend_physics,'rufddaten' ,rufddaten )
	 call mpas_pool_get_array(tend_physics,'rvfddaten' ,rvfddaten )
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

	!check to see if 'old' FDDA target values need to be defined from initial conditions
	 if(first_call .and. .not. config_do_restart) then
	    qv_fdda_old = qv
	    th_fdda_old = theta_m / (1._RKIND + rvord * qv)
	    u_fdda_old = u
	    v_fdda_old = v
	    first_call = .false.
	 endif

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
			t_target = (1.0_RKIND-tfrac)*th_fdda_old(k,i)+tfrac*th_fdda_new(k,i)
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
		      t_target = (1.0_RKIND-tfrac)*th_fdda_old(k,i)+tfrac*th_fdda_new(k,i)
		      rthfddaten(k,i) = fdda_t_coef * ( t_target - theta_p )
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
         call mpas_log_write('fdda: min/max rufddaten = $r, $r', realArgs=(/minval(rufddaten), maxval(rufddaten)/) )
         call mpas_log_write('fdda: min/max rvfddaten = $r, $r', realArgs=(/minval(rvfddaten), maxval(rvfddaten)/) )
         call mpas_log_write('fdda: min/max rufddaten_Edge = $r, $r', realArgs=(/minval(rufddaten_Edge), maxval(rufddaten_Edge)/) )

	! MW: add coupled tendencies for other variables at cell centers 
	 tend_th = 0.0_RKIND
	 do i = 1, nCellsSolve
	   do k = 1, nVertLevels
	      tend_th(k,i) = tend_th(k,i) + rthfddaten(k,i)*rho_zz(k,i)
	      tend_scalars(index_qv,k,i) = tend_scalars(index_qv,k,i) + rqvfddaten(k,i)*rho_zz(k,i)
	   enddo
	 enddo
         call mpas_log_write('fdda: min/max rthfddaten = $r, $r', realArgs=(/minval(rthfddaten), maxval(rthfddaten)/) )
         call mpas_log_write('fdda: min/max rqvfddaten = $r, $r', realArgs=(/minval(rqvfddaten), maxval(rqvfddaten)/) )


        ! if non-hydrostatic core, convert the tendency for the potential temperature to a 
        ! tendency for the modified potential temperature
         do i = 1, nCellsSolve
           do k = 1,nVertLevels
              tend_th(k,i) = (1. + rvord * scalars(index_qv,k,i)) * tend_th(k,i) &
                                 + rvord * theta_m(k,i) * rqvfddaten(k,i)*rho_zz(k,i) 
              tend_rtheta(k,i) = tend_rtheta(k,i) + tend_th(k,i)
           end do
         end do

	!check to see if this is the last time step within the current FDDA target data interval and 
	!if so, save the current "new" targets as the "old" targets for the next time step
	 if(mod(xtime_s+config_dt,fdda_int)/fdda_int .lt. tfrac) then
	    th_fdda_old = th_fdda_new
	    qv_fdda_old = qv_fdda_new
	    u_fdda_old = u_fdda_new
	    v_fdda_old = v_fdda_new
	 endif

         deallocate(tend_th)

 end subroutine apply_fdda

!==================================================================================================
 end module mpas_atm_fdda
!==================================================================================================
