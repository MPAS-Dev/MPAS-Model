!=================================================================================================================
 module module_cu_ntiedtke
 use mpas_kind_types,only: kind_phys => RKIND
 use cu_ntiedtke,only: cu_ntiedtke_run, &
                       cu_ntiedtke_init
 use cu_ntiedtke_common
 use cu_ntiedtke_post,only: cu_ntiedtke_post_run
 use cu_ntiedtke_pre,only: cu_ntiedtke_pre_run

 implicit none
 private
 public:: cu_ntiedtke_driver, &
          ntiedtkeinit


 contains


!=================================================================================================================
 subroutine cu_ntiedtke_driver(                          &
                 dt,itimestep,stepcu                     &
                ,raincv,pratec,qfx,hfx                   &
                ,u3d,v3d,w,t3d,qv3d,qc3d,qi3d,pi3d,rho3d &
                ,qvften,thften                           &
                ,dz8w,pcps,p8w,xland,cu_act_flag,dx      &
                ,f_qc,f_qi                               &
                ,grav,xlf,xls,xlv,rd,rv,cp               &
                ,rthcuten,rqvcuten,rqccuten,rqicuten     &
                ,rucuten,rvcuten                         &
                ,ids,ide,jds,jde,kds,kde                 &
                ,ims,ime,jms,jme,kms,kme                 &
                ,its,ite,jts,jte,kts,kte                 &
                ,errmsg,errflg)
!=================================================================================================================
!-- u3d         3d u-velocity interpolated to theta points (m/s)
!-- v3d         3d v-velocity interpolated to theta points (m/s)
!-- th3d        3d potential temperature (k)
!-- t3d         temperature (k)
!-- qv3d        3d water vapor mixing ratio (kg/kg)
!-- qc3d        3d cloud mixing ratio (kg/kg)
!-- qi3d        3d ice mixing ratio (kg/kg)
!-- rho3d       3d air density (kg/m^3)
!-- p8w         3d hydrostatic pressure at full levels (pa)
!-- pcps        3d hydrostatic pressure at half levels (pa)
!-- pi3d        3d exner function (dimensionless)
!-- qvften      3d total advective + PBL moisture tendency (kg kg-1 s-1)
!-- thften      3d total advective + PBL + radiative temperature tendency (k s-1)
!-- rthcuten      theta tendency due to 
!                 cumulus scheme precipitation (k/s)
!-- rucuten       u wind tendency due to 
!                 cumulus scheme precipitation (k/s)
!-- rvcuten       v wind tendency due to 
!                 cumulus scheme precipitation (k/s)
!-- rqvcuten      qv tendency due to 
!                 cumulus scheme precipitation (kg/kg/s)
!-- rqccuten      qc tendency due to 
!                 cumulus scheme precipitation (kg/kg/s)
!-- rqicuten      qi tendency due to 
!                 cumulus scheme precipitation (kg/kg/s)
!-- rainc         accumulated total cumulus scheme precipitation (mm)
!-- raincv        cumulus scheme precipitation (mm)
!-- pratec        precipitiation rate from cumulus scheme (mm/s)
!-- dz8w        dz between full levels (m)
!-- qfx         upward moisture flux at the surface (kg/m^2/s)
!-- hfx         upward heat flux at the surface (w/m^2) 
!-- dt          time step (s)
!-- ids         start index for i in domain
!-- ide         end index for i in domain
!-- jds         start index for j in domain
!-- jde         end index for j in domain
!-- kds         start index for k in domain
!-- kde         end index for k in domain
!-- ims         start index for i in memory
!-- ime         end index for i in memory
!-- jms         start index for j in memory
!-- jme         end index for j in memory
!-- kms         start index for k in memory
!-- kme         end index for k in memory
!-- its         start index for i in tile
!-- ite         end index for i in tile
!-- jts         start index for j in tile
!-- jte         end index for j in tile
!-- kts         start index for k in tile
!-- kte         end index for k in tile
!-----------------------------------------------------------------------------------------------------------------

!--- input arguments:
 logical,intent(in),optional:: f_qc,f_qi

 integer,intent(in):: ids,ide,jds,jde,kds,kde, &
                      ims,ime,jms,jme,kms,kme, &
                      its,ite,jts,jte,kts,kte

 integer,intent(in):: itimestep,stepcu

 real(kind=kind_phys),intent(in):: cp,grav,rd,rv,xlf,xls,xlv

 real(kind=kind_phys),intent(in):: dt

 real(kind=kind_phys),intent(in),dimension(ims:ime,jms:jme):: dx,hfx,qfx,xland

 real(kind=kind_phys),intent(in),dimension(ims:ime,kms:kme,jms:jme):: &
    dz8w,   &
    pcps,   &
    p8w,    &
    pi3d,   &
    qc3d,   &
    qvften, &
    thften, &
    qi3d,   &
    qv3d,   &
    rho3d,  &
    t3d,    &
    u3d,    &
    v3d,    &
    w

!--- inout arguments:
 logical,intent(inout),dimension(ims:ime,jms:jme):: cu_act_flag

 real(kind=kind_phys),intent(inout),dimension(ims:ime,jms:jme):: raincv, pratec

 real(kind=kind_phys),intent(inout),dimension(ims:ime,kms:kme,jms:jme),optional:: &
 rqccuten,  &
 rqicuten,  &
 rqvcuten,  &
 rthcuten,  &
 rucuten,   &
 rvcuten

!--- output arguments:
 character(len=*),intent(out):: errmsg
 integer,intent(out):: errflg

!--- local variables and arrays:
 integer:: i,im,j,k,kx,kx1
 integer,dimension(its:ite)::  slimsk

 real(kind=kind_phys):: delt
 real(kind=kind_phys),dimension(its:ite):: rn
 real(kind=kind_phys),dimension(its:ite,kts:kte):: prsl,omg,ghtl
 real(kind=kind_phys),dimension(its:ite,kts:kte):: uf,vf,tf,qvf,qcf,qif
 real(kind=kind_phys),dimension(its:ite,kts:kte):: qvftenz,thftenz
 real(kind=kind_phys),dimension(its:ite,kts:kte+1):: prsi,ghti,zi

 real(kind=kind_phys),dimension(its:ite):: dx_hv,hfx_hv,qfx_hv,xland_hv
 real(kind=kind_phys),dimension(its:ite,kts:kte):: dz_hv,pi_hv,prsl_hv
 real(kind=kind_phys),dimension(its:ite,kts:kte):: qv_hv,qc_hv,qi_hv,rho_hv,t_hv,u_hv,v_hv
 real(kind=kind_phys),dimension(its:ite,kts:kte):: qvften_hv,thften_hv
 real(kind=kind_phys),dimension(its:ite,kts:kte+1):: prsi_hv,w_hv

 real(kind=kind_phys),dimension(its:ite):: raincv_hv,pratec_hv
 real(kind=kind_phys),dimension(its:ite,kts:kte):: rthcuten_hv,rqvcuten_hv,rqccuten_hv,rqicuten_hv, &
                                                   rucuten_hv,rvcuten_hv

!-----------------------------------------------------------------------------------------------------------------

 errmsg = ' '
 errflg = 0

 call cu_ntiedtke_init( &
                       con_cp  = cp  , con_rd = rd   , con_rv   = rv   , con_xlv = xlv    , &
                       con_xls = xls , con_xlf = xlf , con_grav = grav , errmsg  = errmsg , &
                       errflg  = errflg                                                     &
                      )

 do j = jts,jte
    do i = its,ite
       cu_act_flag(i,j)=.true.
    enddo
 enddo

 do j = jts,jte

    do i = its,ite
       dx_hv(i)    = dx(i,j)
       hfx_hv(i)   = hfx(i,j)
       qfx_hv(i)   = qfx(i,j)
       xland_hv(i) = xland(i,j)
    enddo

    do k = kts,kte
       do i = its,ite
          dz_hv(i,k)   = dz8w(i,k,j)
          pi_hv(i,k)   = pi3d(i,k,j)
          prsl_hv(i,k) = pcps(i,k,j)
          qv_hv(i,k)   = qv3d(i,k,j)
          qc_hv(i,k)   = qc3d(i,k,j)
          qi_hv(i,k)   = qi3d(i,k,j)
          rho_hv(i,k)  = rho3d(i,k,j)
          t_hv(i,k)    = t3d(i,k,j)
          u_hv(i,k)    = u3d(i,k,j)
          v_hv(i,k)    = v3d(i,k,j)

          qvften_hv(i,k) = qvften(i,k,j)
          thften_hv(i,k) = thften(i,k,j)
       enddo
    enddo
    do k = kts,kte+1
       do i = its,ite
          prsi_hv(i,k) = p8w(i,k,j)
          w_hv(i,k)    = w(i,k,j)
       enddo
    enddo

    call cu_ntiedtke_pre_run( &
                     its    = its       , ite     = ite     , kts     = kts     , kte       = kte       , &
                     im     = im        , kx      = kx      , kx1     = kx1     , itimestep = itimestep , &
                     stepcu = stepcu    , dt      = dt      , grav    = grav    , xland     = xland_hv  , &
                     dz     = dz_hv     , pres    = prsl_hv , presi   = prsi_hv , t         = t_hv      , &
                     rho    = rho_hv    , qv      = qv_hv   , qc      = qc_hv   , qi        = qi_hv     , &
                     u      = u_hv      , v       = v_hv    , w       = w_hv    , qvften    = qvften_hv , &
                     thften = thften_hv , qvftenz = qvftenz , thftenz = thftenz , slimsk    = slimsk    , &
                     delt   = delt      , prsl    = prsl    , ghtl    = ghtl    , tf        = tf        , &
                     qvf    = qvf       , qcf     = qcf     , qif     = qif     , uf        = uf        , &
                     vf     = vf        , prsi    = prsi    , ghti    = ghti    , omg       = omg       , &
                     errmsg = errmsg    , errflg  = errflg                                                &
                            )

    call cu_ntiedtke_run( &
                     pu   = uf     , pv   = vf     , pt     = tf      , pqv    = qvf     , &
                     pqc  = qcf    , pqi  = qif    , pqvf   = qvftenz , ptf    = thftenz , &
                     poz  = ghtl   , pzz  = ghti   , pomg   = omg     , pap    = prsl    , &
                     paph = prsi   , evap = qfx_hv , hfx    = hfx_hv  , zprecc = rn      , &
                     lndj = slimsk , lq   = im     , km     = kx      , km1    = kx1     , &
                     dt   = delt   , dx   = dx_hv  , errmsg = errmsg  , errflg = errflg    &
                        )

    call cu_ntiedtke_post_run( &
                     its      = its         , ite      = ite         , kts      = kts         , kte      = kte         , &
                     stepcu   = stepcu      , dt       = dt          , exner    = pi_hv       , qv       = qv_hv       , &
                     qc       = qc_hv       , qi       = qi_hv       , t        = t_hv        , u        = u_hv        , &
                     v        = v_hv        , qvf      = qvf         , qcf      = qcf         , qif      = qif         , &
                     tf       = tf          , uf       = uf          , vf       = vf          , rn       = rn          , &
                     raincv   = raincv_hv   , pratec   = pratec_hv   , rthcuten = rthcuten_hv , rqvcuten = rqvcuten_hv , &
                     rqccuten = rqccuten_hv , rqicuten = rqicuten_hv , rucuten  = rucuten_hv  , rvcuten  = rvcuten_hv  , &
                     errmsg   = errmsg      , errflg   = errflg                                                          &
                             )

    do i = its,ite
       raincv(i,j) = raincv_hv(i)
       pratec(i,j) = pratec_hv(i)
    enddo

    do k = kts,kte
       do i = its,ite
          rucuten(i,k,j)  = rucuten_hv(i,k)
          rvcuten(i,k,j)  = rvcuten_hv(i,k)
          rthcuten(i,k,j) = rthcuten_hv(i,k)
          rqvcuten(i,k,j) = rqvcuten_hv(i,k)
       enddo
    enddo

    if(present(rqccuten))then
       if(f_qc) then
          do k = kts,kte
             do i = its,ite
                rqccuten(i,k,j) = rqccuten_hv(i,k)
             enddo
          enddo
       endif
    endif

    if(present(rqicuten))then
       if(f_qi) then
          do k = kts,kte
             do i = its,ite
                rqicuten(i,k,j) = rqicuten_hv(i,k)
             enddo
          enddo
       endif
    endif

 enddo

 end subroutine cu_ntiedtke_driver

!=================================================================================================================
 subroutine ntiedtkeinit(rthcuten,rqvcuten,rqccuten,rqicuten, &
                         rucuten,rvcuten,rthften,rqvften,     &
                         restart,p_qc,p_qi,p_first_scalar,    &
                         allowed_to_read,                     &
                         ids, ide, jds, jde, kds, kde,        &
                         ims, ime, jms, jme, kms, kme,        &
                         its, ite, jts, jte, kts, kte)
!=================================================================================================================

!--- input arguments:
 logical,intent(in):: allowed_to_read,restart

 integer,intent(in):: ids, ide, jds, jde, kds, kde, &
                      ims, ime, jms, jme, kms, kme, &
                      its, ite, jts, jte, kts, kte
 integer,intent(in):: p_first_scalar,p_qi,p_qc

!--- output arguments:
 real(kind=kind_phys),intent(out),dimension(ims:ime,kms:kme,jms:jme )::  &
    rthcuten,rqvcuten,rqccuten,rqicuten,rucuten,rvcuten,rthften,rqvften

!--- local variables and arrays:
 integer:: i,j,k,itf,jtf,ktf

!-----------------------------------------------------------------------------------------------------------------

 jtf = min0(jte,jde-1)
 ktf = min0(kte,kde-1)
 itf = min0(ite,ide-1)

 if(.not.restart)then
    do j = jts,jtf
       do k = kts,ktf
          do i = its,itf
             rthcuten(i,k,j) = 0.
             rqvcuten(i,k,j) = 0.
             rucuten(i,k,j)  = 0.
             rvcuten(i,k,j)  = 0.
          enddo
       enddo
    enddo

    do j = jts,jtf
       do k = kts,ktf
          do i = its,itf
             rthften(i,k,j)=0.
             rqvften(i,k,j)=0.
          enddo
       enddo
    enddo

    if(p_qc .ge. p_first_scalar) then
       do j = jts,jtf
          do k = kts,ktf
             do i = its,itf
                rqccuten(i,k,j)=0.
             enddo
          enddo
       enddo
    endif

    if(p_qi .ge. p_first_scalar) then
       do j = jts,jtf
          do k = kts,ktf
             do i = its,itf
                rqicuten(i,k,j)=0.
             enddo
          enddo
       enddo
    endif
 endif

 end subroutine ntiedtkeinit

!=================================================================================================================
 end module module_cu_ntiedtke
!=================================================================================================================
