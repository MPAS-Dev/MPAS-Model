!==================================================================================================
 module mpas_atmphys_fdda
 use mpas_kind_types
 use mpas_derived_types
 use mpas_pool_routines

 use mpas_atmphys_constants
 use mpas_atmphys_utilities
 use mpas_atmphys_vars

 implicit none
 private
 public:: apply_fdda

 integer, private:: i,k


!MPAS grid nudging.
!O. Russell Bullock Jr. (send comments to bullock.russell@epa.gov).
!2016-03-30.
!
! subroutines in mpas_atmphys_fdda:
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
 subroutine apply_fdda(xtime_s,configs,mesh,state,time_lev,fdda,diag,diag_physics,tend_physics, &
                       its,ite)
!==================================================================================================

!input variables:
 type(mpas_pool_type),intent(in):: configs
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(in):: state
 type(mpas_pool_type),intent(in):: fdda 
 type(mpas_pool_type),intent(in):: diag
 type(mpas_pool_type),intent(in):: diag_physics
 integer,intent(in):: its,ite
 integer,intent(in):: time_lev
 real(kind=RKIND),intent(in):: xtime_s

!inout variables:
 type(mpas_pool_type),intent(inout):: tend_physics

!local pointers:
 integer,pointer:: index_qv,fdda_t_min_layer,fdda_q_min_layer,fdda_uv_min_layer
 integer,dimension(:),pointer :: kpbl
 real(kind=RKIND),pointer:: fdda_t_coef,fdda_q_coef,fdda_uv_coef
 real(kind=RKIND),pointer:: fdda_int,fdda_scale_min,fdda_scale_max
 real(kind=RKIND),pointer:: config_dt
 real(kind=RKIND),pointer:: len_disp
 real(kind=RKIND),dimension(:),pointer    :: meshDensity
 real(kind=RKIND),dimension(:,:),pointer  :: rthfddaten,rqvfddaten
 real(kind=RKIND),dimension(:,:),pointer  :: rufddaten,rvfddaten 
 real(kind=RKIND),dimension(:,:),pointer  :: qv_fdda_old,th_fdda_old,u_fdda_old,v_fdda_old
 real(kind=RKIND),dimension(:,:),pointer  :: qv_fdda_new,th_fdda_new,u_fdda_new,v_fdda_new
 real(kind=RKIND),dimension(:,:),pointer  :: u,v
 real(kind=RKIND),dimension(:,:),pointer  :: qv
 real(kind=RKIND),dimension(:,:),pointer  :: theta_m
 real(kind=RKIND),dimension(:,:,:),pointer:: scalars
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
 real(kind=RKIND),parameter:: rvrd   = R_v/R_d

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
 call mpas_pool_get_array(tend_physics,'rufddaten' ,rufddaten )
 call mpas_pool_get_array(tend_physics,'rvfddaten' ,rvfddaten )
 call mpas_pool_get_array(diag_physics,'kpbl',kpbl)
 call mpas_pool_get_array(diag,'uReconstructZonal'     ,u)
 call mpas_pool_get_array(diag,'uReconstructMeridional',v)

 call mpas_pool_get_dimension(state,'index_qv',index_qv)
 call mpas_pool_get_array(state,'scalars',scalars,time_lev)
 qv => scalars(index_qv,:,:)
 call mpas_pool_get_array(state,'theta_m',theta_m,time_lev)

!check to see if 'old' FDDA target values need to be defined from initial conditions
 if(first_call .and. .not. config_do_restart) then
    qv_fdda_old = qv
    th_fdda_old = theta_m / (1._RKIND + rvrd * qv)
    u_fdda_old = u
    v_fdda_old = v
    first_call = .false.
 endif

!calculate time fraction within FDDA target data interval
 tfrac = mod(xtime_s,fdda_int)/fdda_int

 fdda_select: select case(fdda_scheme)

    case ("scaled")

       srange = fdda_scale_max-fdda_scale_min

       DO i=its,ite

          dcEdge_m  = len_disp / meshDensity(i)**0.25

          if (dcEdge_m .lt. fdda_scale_max) then
             if (dcEdge_m .lt. fdda_scale_min) then
                sfac = 0.0
             else
                sfac = (dcEdge_m-fdda_scale_min)/srange
             endif
          else
             sfac = 1.0
          endif

       DO k=kts,kte

          theta_p = theta_m(k,i) / (1._RKIND + rvrd * qv(k,i))

          if (fdda_t .and. k.gt.fdda_t_min_layer .and. (fdda_t_in_pbl .or. k.gt.kpbl(i))) then
             t_target = (1.0-tfrac)*th_fdda_old(k,i)+tfrac*th_fdda_new(k,i)
             rthfddaten(k,i) = fdda_t_coef * ( t_target - theta_p ) * sfac
          else
             rthfddaten(k,i) = 0.
          endif

          if (fdda_q .and. k.gt.fdda_q_min_layer .and. (fdda_q_in_pbl .or. k.gt.kpbl(i))) then
             q_target = (1.0-tfrac)*qv_fdda_old(k,i)+tfrac*qv_fdda_new(k,i)
             rqvfddaten(k,i) = fdda_q_coef * ( q_target - qv(k,i) ) * sfac
          else
             rqvfddaten(k,i) = 0.
          endif

          if (fdda_uv .and. k.gt.fdda_uv_min_layer .and. (fdda_uv_in_pbl .or. k.gt.kpbl(i))) then
             u_target = (1.0-tfrac)*u_fdda_old(k,i)+tfrac*u_fdda_new(k,i)
             rufddaten(k,i) = fdda_uv_coef * ( u_target - u(k,i) ) * sfac
             v_target = (1.0-tfrac)*v_fdda_old(k,i)+tfrac*v_fdda_new(k,i)
             rvfddaten(k,i) = fdda_uv_coef * ( v_target - v(k,i) ) * sfac
          else
             rufddaten(k,i) = 0.
             rvfddaten(k,i) = 0.
          endif

       ENDDO
       ENDDO

    case("analysis")

       DO i=its,ite
       DO k=kts,kte

          theta_p = theta_m(k,i) / (1._RKIND + rvrd * qv(k,i))

          if (fdda_t .and. k.gt.fdda_t_min_layer .and. (fdda_t_in_pbl .or. k.gt.kpbl(i))) then
             t_target = (1.0-tfrac)*th_fdda_old(k,i)+tfrac*th_fdda_new(k,i)
             rthfddaten(k,i) = fdda_t_coef * ( t_target - theta_p )
          else
             rthfddaten(k,i) = 0.
          endif

          if (fdda_q .and. k.gt.fdda_q_min_layer .and. (fdda_q_in_pbl .or. k.gt.kpbl(i))) then
             q_target = (1.0-tfrac)*qv_fdda_old(k,i)+tfrac*qv_fdda_new(k,i)
             rqvfddaten(k,i) = fdda_q_coef * ( q_target - qv(k,i) )
          else
             rqvfddaten(k,i) = 0.
          endif

          if (fdda_uv .and. k.gt.fdda_uv_min_layer .and. (fdda_uv_in_pbl .or. k.gt.kpbl(i))) then
             u_target = (1.0-tfrac)*u_fdda_old(k,i)+tfrac*u_fdda_new(k,i)
             rufddaten(k,i) = fdda_uv_coef * ( u_target - u(k,i) )
             v_target = (1.0-tfrac)*v_fdda_old(k,i)+tfrac*v_fdda_new(k,i)
             rvfddaten(k,i) = fdda_uv_coef * ( v_target - v(k,i) )
          else
             rufddaten(k,i) = 0.
             rvfddaten(k,i) = 0.
          endif

       ENDDO
       ENDDO

    case default

 end select fdda_select

!check to see if this is the last time step within the current FDDA target data interval and 
!if so, save the current "new" targets as the "old" targets for the next time step
 if(mod(xtime_s+config_dt,fdda_int)/fdda_int .lt. tfrac) then
    th_fdda_old = th_fdda_new
    qv_fdda_old = qv_fdda_new
    u_fdda_old = u_fdda_new
    v_fdda_old = v_fdda_new
 endif

 end subroutine apply_fdda

!==================================================================================================
 end module mpas_atmphys_fdda
!==================================================================================================
