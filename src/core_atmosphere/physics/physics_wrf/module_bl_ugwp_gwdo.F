!=================================================================================================================
 module module_bl_ugwp_gwdo
 use mpas_kind_types,only: kind_phys => RKIND
 use bl_ugwp,only: bl_ugwp_run
 use bl_ugwpv1_ngw,only: ugwpv1_ngw_run

 implicit none
 private
 public:: gwdo_ugwp


 contains


!=================================================================================================================
 subroutine gwdo_ugwp(u3d,v3d,t3d,qv3d,p3d,p3di,pi3d,z,pblh,kpbl2d,br1,xland, &
                 rublten,rvblten,rthblten,                                    &
                 dtaux3d,dtauy3d,dusfcg,dvsfcg,                               &
                 dtaux3d_ls,dtauy3d_ls,dtaux3d_bl,dtauy3d_bl,                 &
                 dtaux3d_ss,dtauy3d_ss,dtaux3d_fd,dtauy3d_fd,                 &
                 dusfc_ls,dvsfc_ls,dusfc_bl,dvsfc_bl,dusfc_ss,dvsfc_ss,       &
                 dusfc_fd,dvsfc_fd,ugwp_diags,ngw_scheme,xlatd,               &
                 jindx1_tau,jindx2_tau,ddy_j1tau,ddy_j2tau,r_DoY,             &
                 raincv,rainncv,ntau_d1y,ntau_d2t,days_limb,tau_limb,         &
                 dudt_ngw,dvdt_ngw,dtdt_ngw,                                  &
                 var2dls,oc12dls,oa2d1ls,oa2d2ls,oa2d3ls,oa2d4ls,ol2d1ls,     &
                 ol2d2ls,ol2d3ls,ol2d4ls,var2dss,oc12dss,oa2d1ss,oa2d2ss,     &
                 oa2d3ss,oa2d4ss,ol2d1ss,ol2d2ss,ol2d3ss,ol2d4ss,             &
                 sina,cosa,zi,dz,znu,znw,p_top,                               &
                 cp,g,rd,rv,ep1,pi,                                           &
                 dt,dx,itimestep,                                             &
                 ids,ide, jds,jde, kds,kde,                                   &
                 ims,ime, jms,jme, kms,kme,                                   &
                 its,ite, jts,jte, kts,kte,                                   &
                 errmsg,errflg                                                &
                )
!=================================================================================================================
!                                                                       
!-- u3d         3d u-velocity interpolated to theta points (m/s)
!-- v3d         3d v-velocity interpolated to theta points (m/s)
!-- t3d         temperature (k)
!-- qv3d        3d water vapor mixing ratio (kg/kg)
!-- p3d         3d pressure (pa)
!-- p3di        3d pressure (pa) at interface level
!-- pi3d        3d exner function (dimensionless)
!-- pblh        PBL height (m)
!-- br1         bulk Richardson number at lowest model level
!-- kpbl2d      index level of PBL top 
!-- xland       land mask (1 for land, 2 for water)
!-- rublten     u tendency due to pbl parameterization (m/s/s) 
!-- rvblten     v tendency due to pbl parameterization (m/s/s)
!-- rthblten    potential temperature tendency due to pbl parameterization (K/s)
!-- sina        sine rotation angle
!-- cosa        cosine rotation angle
!-- znu         eta values (sigma values)
!-- cp          heat capacity at constant pressure for dry air (j/kg/k)
!-- g           acceleration due to gravity (m/s^2)
!-- rd          gas constant for dry air (j/kg/k)
!-- z           height above sea level of layer centers (m)
!-- zi          height above sea level of layer interfaces (m)
!-- dz          layer thickness (m)
!-- rv          gas constant for water vapor (j/kg/k)
!-- dt          time step (s)
!-- dx          model grid interval (m)
!-- ep1         constant for virtual temperature (r_v/r_d - 1) (dimensionless)
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
!
!=================================================================================================================

!--- input arguments:
 integer,intent(in):: ids,ide,jds,jde,kds,kde, &
                      ims,ime,jms,jme,kms,kme, &
                      its,ite,jts,jte,kts,kte
 integer,intent(in):: itimestep

 integer,intent(in),dimension(ims:ime,jms:jme):: kpbl2d

 real(kind=kind_phys),intent(in):: dt,cp,g,rd,rv,ep1,pi
 real(kind=kind_phys),intent(in),optional:: p_top

 real(kind=kind_phys),intent(in),dimension(kms:kme),optional::        &
                                                                 znu, &
                                                                 znw

 real(kind=kind_phys),intent(in),dimension(ims:ime,jms:jme)::         &
                                                                  dx, &
                                                           sina,cosa, &
                                                 pblh,br1,xland

 real(kind=kind_phys),intent(in),dimension(ims:ime,jms:jme),optional::&
                            xlatd,raincv,rainncv,ddy_j1tau,ddy_j2tau
 integer,intent(in),dimension(ims:ime,jms:jme),optional::             &
                                               jindx1_tau,jindx2_tau
 integer,intent(in):: ntau_d1y,ntau_d2t
 real(kind=kind_phys),intent(in),dimension(ntau_d2t),optional::          &
                            days_limb
 real(kind=kind_phys),intent(in),dimension(ntau_d1y,ntau_d2t),optional:: &
                            tau_limb
 real(kind=kind_phys),intent(in)                           :: r_DoY

 real(kind=kind_phys),intent(in),dimension(ims:ime,jms:jme):: &
                                                     var2dls,var2dss, &
                                                     oc12dls,oc12dss, & 
                                     oa2d1ls,oa2d2ls,oa2d3ls,oa2d4ls, &
                                     oa2d1ss,oa2d2ss,oa2d3ss,oa2d4ss, &
                                     ol2d1ls,ol2d2ls,ol2d3ls,ol2d4ls, &
                                     ol2d1ss,ol2d2ss,ol2d3ss,ol2d4ss


 real(kind=kind_phys),intent(in),dimension(ims:ime,kms:kme,jms:jme):: &
                                                                qv3d, &
                                                                 p3d, &
                                                                pi3d, &
                                                                 t3d, &
                                                                 u3d, &
                                                                 v3d, &
                                                                   z, &
                                                                  zi, &
                                                                  dz

 real(kind=kind_phys),intent(in),dimension(ims:ime,kms:kme,jms:jme):: &
                                                                 p3di

 logical,intent(in)::                           ugwp_diags,ngw_scheme

!--- output arguments:
 character(len=*),intent(out)::  errmsg

 integer,intent(out):: errflg

 real(kind=kind_phys),intent(out),dimension(ims:ime,jms:jme)::    &
                                                   dusfcg,dvsfcg

 real(kind=kind_phys),intent(out),dimension(:,:),optional::       &
                             dusfc_ls,dusfc_bl,dusfc_ss,dusfc_fd, &
                             dvsfc_ls,dvsfc_bl,dvsfc_ss,dvsfc_fd

 real(kind=kind_phys),intent(out),dimension(ims:ime,kms:kme,jms:jme ):: &
                                                       dtaux3d,dtauy3d

 real(kind=kind_phys),intent(out),dimension(:,:,:),optional::           &
                           dtaux3d_ls,dtaux3d_bl,dtaux3d_ss,dtaux3d_fd, &
                           dtauy3d_ls,dtauy3d_bl,dtauy3d_ss,dtauy3d_fd

 real(kind=kind_phys),intent(out),dimension(:,:,:),optional::           &
                           dudt_ngw,dvdt_ngw,dtdt_ngw

!--- inout arguments:
 real(kind=kind_phys),intent(inout),dimension(ims:ime,kms:kme,jms:jme):: &
                           rublten,rvblten,rthblten

!--- local variables and arrays:
 integer:: i,j,k

 real(kind=kind_phys),dimension(its:ite):: &
    dx_hv,sina_hv,cosa_hv,pblh_hv,br1_hv,xland_hv
 integer,dimension(its:ite):: kpbl_hv
 real(kind=kind_phys),dimension(its:ite):: &
    var2dls_hv,oc12dls_hv,oa2d1ls_hv,oa2d2ls_hv,oa2d3ls_hv,oa2d4ls_hv,  &
    ol2d1ls_hv,ol2d2ls_hv,ol2d3ls_hv,ol2d4ls_hv
 real(kind=kind_phys),dimension(its:ite):: &
    var2dss_hv,oc12dss_hv,oa2d1ss_hv,oa2d2ss_hv,oa2d3ss_hv,oa2d4ss_hv,  &
    ol2d1ss_hv,ol2d2ss_hv,ol2d3ss_hv,ol2d4ss_hv
    
 real(kind=kind_phys),dimension(its:ite):: &
    dusfcg_hv,dvsfcg_hv

 real(kind=kind_phys),dimension(:),allocatable::   &
    dusfc_ls_hv,dvsfc_ls_hv,dusfc_bl_hv,dvsfc_bl_hv, &
    dusfc_ss_hv,dvsfc_ss_hv,dusfc_fd_hv,dvsfc_fd_hv, &
    xlatd_hv,raincv_hv,rainncv_hv,                   &
    ddy_j1tau_hv,ddy_j2tau_hv

 integer,dimension(:),allocatable::                &
    jindx1_tau_hv,jindx2_tau_hv

 real(kind=kind_phys),dimension(its:ite,kts:kte):: &
    u3d_hv,v3d_hv,t3d_hv,qv3d_hv,pi3d_hv,p3d_hv,z_hv,dz_hv

 real(kind=kind_phys),dimension(its:ite,kts:kte):: &
    rublten_hv,rvblten_hv,rthblten_hv,dtaux3d_hv,dtauy3d_hv
 real(kind=kind_phys),dimension(:,:),allocatable:: &
    dtaux3d_ls_hv,dtauy3d_ls_hv,dtaux3d_bl_hv,dtauy3d_bl_hv,  &
    dtaux3d_ss_hv,dtauy3d_ss_hv,dtaux3d_fd_hv,dtauy3d_fd_hv
 real(kind=kind_phys),dimension(:,:),allocatable:: &
    dudt_ngw_hv,dvdt_ngw_hv,dtdt_ngw_hv

 real(kind=kind_phys),dimension(its:ite,kms:kme):: &
    p3di_hv,zi_hv

!-----------------------------------------------------------------------------------------------------------------

!  Outer j-loop. Allows consistency between WRF and MPAS in the driver.

 do j = jts,jte

    !  All variables for gwdo2d are tile-sized and have only a single
    !  horizontal dimension. The _hv suffix refers to "horizontal vertical",
    !  a reminder that there is a single horizontal index. Yes, we know that 
    !  variables that have only a horizontal index are not *really* _hv.

    !  All of the following 3d and 2d variables are declared intent(in) in the
    !  gwdo2d subroutine, so there is no need to put the updated values back
    !  from the temporary arrays back into the original arrays.

    !  Variables that are INTENT(IN) or INTENT(INOUT)

    !  3d, interface levels:
    do k = kts,kte+1
       do i = its,ite
          p3di_hv(i,k) = p3di(i,k,j)
          zi_hv(i,k)   = zi(i,k,j)
       enddo
    enddo

    !  3d, layers:
    do k = kts,kte
       do i = its,ite
          rublten_hv(i,k) = rublten(i,k,j)
          rvblten_hv(i,k) = rvblten(i,k,j)
         rthblten_hv(i,k) = rthblten(i,k,j)
              u3d_hv(i,k) =     u3d(i,k,j)
              v3d_hv(i,k) =     v3d(i,k,j)
              t3d_hv(i,k) =     t3d(i,k,j)
             qv3d_hv(i,k) =    qv3d(i,k,j)
              p3d_hv(i,k) =     p3d(i,k,j)
             pi3d_hv(i,k) =    pi3d(i,k,j)
                z_hv(i,k) =       z(i,k,j)
               dz_hv(i,k) =      dz(i,k,j)
       enddo
    enddo

    !  2d:
    do i = its,ite
            dx_hv(i) =      dx(i,j)
          sina_hv(i) =    sina(i,j)
          cosa_hv(i) =    cosa(i,j)
          pblh_hv(i) =    pblh(i,j)
           br1_hv(i) =     br1(i,j)
          kpbl_hv(i) =  kpbl2d(i,j)
         xland_hv(i) =   xland(i,j)
       var2dls_hv(i) = var2dls(i,j)
       oc12dls_hv(i) = oc12dls(i,j)
       oa2d1ls_hv(i) = oa2d1ls(i,j)
       oa2d2ls_hv(i) = oa2d2ls(i,j)
       oa2d3ls_hv(i) = oa2d3ls(i,j)
       oa2d4ls_hv(i) = oa2d4ls(i,j)
       ol2d1ls_hv(i) = ol2d1ls(i,j)
       ol2d2ls_hv(i) = ol2d2ls(i,j)
       ol2d3ls_hv(i) = ol2d3ls(i,j)
       ol2d4ls_hv(i) = ol2d4ls(i,j)
       var2dss_hv(i) = var2dss(i,j)
       oc12dss_hv(i) = oc12dss(i,j)
       oa2d1ss_hv(i) = oa2d1ss(i,j)
       oa2d2ss_hv(i) = oa2d2ss(i,j)
       oa2d3ss_hv(i) = oa2d3ss(i,j)
       oa2d4ss_hv(i) = oa2d4ss(i,j)
       ol2d1ss_hv(i) = ol2d1ss(i,j)
       ol2d2ss_hv(i) = ol2d2ss(i,j)
       ol2d3ss_hv(i) = ol2d3ss(i,j)
       ol2d4ss_hv(i) = ol2d4ss(i,j)
    enddo
    if (ngw_scheme) then
       allocate (xlatd_hv(its:ite))
       allocate (raincv_hv(its:ite))
       allocate (rainncv_hv(its:ite))
       allocate (ddy_j1tau_hv(its:ite))
       allocate (ddy_j2tau_hv(its:ite))
       allocate (jindx1_tau_hv(its:ite))
       allocate (jindx2_tau_hv(its:ite))
       do i = its,ite
          xlatd_hv(i)      = xlatd(i,j)
          raincv_hv(i)     = raincv(i,j)
          rainncv_hv(i)    = rainncv(i,j)
          ddy_j1tau_hv(i)  = ddy_j1tau(i,j)
          ddy_j2tau_hv(i)  = ddy_j2tau(i,j)
          jindx1_tau_hv(i) = jindx1_tau(i,j)
          jindx2_tau_hv(i) = jindx2_tau(i,j)
       enddo
    endif

    ! Allocate ugwp_diags and/or variables if needed
    if (ugwp_diags) then
       allocate (dusfc_ls_hv(its:ite))
       allocate (dvsfc_ls_hv(its:ite))
       allocate (dusfc_bl_hv(its:ite))
       allocate (dvsfc_bl_hv(its:ite))
       allocate (dusfc_ss_hv(its:ite))
       allocate (dvsfc_ss_hv(its:ite))
       allocate (dusfc_fd_hv(its:ite))
       allocate (dvsfc_fd_hv(its:ite))
       allocate (dtaux3d_ls_hv(its:ite,kts:kte))
       allocate (dtauy3d_ls_hv(its:ite,kts:kte))
       allocate (dtaux3d_bl_hv(its:ite,kts:kte))
       allocate (dtauy3d_bl_hv(its:ite,kts:kte))
       allocate (dtaux3d_ss_hv(its:ite,kts:kte))
       allocate (dtauy3d_ss_hv(its:ite,kts:kte))
       allocate (dtaux3d_fd_hv(its:ite,kts:kte))
       allocate (dtauy3d_fd_hv(its:ite,kts:kte))
       if (ngw_scheme) then
          allocate (dudt_ngw_hv(its:ite,kts:kte))
          allocate (dvdt_ngw_hv(its:ite,kts:kte))
          allocate (dtdt_ngw_hv(its:ite,kts:kte))
       endif
    endif

    call bl_ugwp_run(sina=sina_hv,cosa=cosa_hv                &
                    ,rublten=rublten_hv,rvblten=rvblten_hv    &
                    ,dtaux3d=dtaux3d_hv,dtauy3d=dtauy3d_hv    &
                    ,dtaux3d_ls=dtaux3d_ls_hv,dtauy3d_ls=dtauy3d_ls_hv  &
                    ,dtaux3d_bl=dtaux3d_bl_hv,dtauy3d_bl=dtauy3d_bl_hv  &
                    ,dtaux3d_ss=dtaux3d_ss_hv,dtauy3d_ss=dtauy3d_ss_hv  &
                    ,dtaux3d_fd=dtaux3d_fd_hv,dtauy3d_fd=dtauy3d_fd_hv  &
                    ,dusfcg=dusfcg_hv,dvsfcg=dvsfcg_hv                  &
                    ,dusfc_ls=dusfc_ls_hv,dvsfc_ls=dvsfc_ls_hv          &
                    ,dusfc_bl=dusfc_bl_hv,dvsfc_bl=dvsfc_bl_hv          &
                    ,dusfc_ss=dusfc_ss_hv,dvsfc_ss=dvsfc_ss_hv          &
                    ,dusfc_fd=dusfc_fd_hv,dvsfc_fd=dvsfc_fd_hv          &
                    ,ugwp_diags=ugwp_diags                    &
                    ,uproj=u3d_hv,vproj=v3d_hv                &
                    ,t1=t3d_hv,q1=qv3d_hv                     &
                    ,prsi=p3di_hv,prsl=p3d_hv,prslk=pi3d_hv   &
                    ,zl=z_hv,dz=dz_hv,hpbl=pblh_hv            &
                    ,kpbl=kpbl_hv,br1=br1_hv,xland1=xland_hv  &
                    ,var=var2dls_hv,oc1=oc12dls_hv            &
                    ,oa2d1=oa2d1ls_hv,oa2d2=oa2d2ls_hv        &
                    ,oa2d3=oa2d3ls_hv,oa2d4=oa2d4ls_hv        &
                    ,ol2d1=ol2d1ls_hv,ol2d2=ol2d2ls_hv        &
                    ,ol2d3=ol2d3ls_hv,ol2d4=ol2d4ls_hv        &
                    ,varss=var2dss_hv,oc1ss=oc12dss_hv        &
                    ,oa2d1ss=oa2d1ss_hv,oa2d2ss=oa2d2ss_hv    &
                    ,oa2d3ss=oa2d3ss_hv,oa2d4ss=oa2d4ss_hv    &
                    ,ol2d1ss=ol2d1ss_hv,ol2d2ss=ol2d2ss_hv    &
                    ,ol2d3ss=ol2d3ss_hv,ol2d4ss=ol2d4ss_hv    &
                    ,g_=g,cp_=cp,rd_=rd,rv_=rv,fv_=ep1,pi_=pi &
                    ,dxmeter=dx_hv,deltim=dt                  &
                    ,its=its,ite=ite,kte=kte,kme=kte+1        &
                    ,errmsg=errmsg,errflg=errflg)

!
! Option to call non-stationary gravity wave drag
!
    if (ngw_scheme) then
       call ugwpv1_ngw_run(xlatd=xlatd_hv,raincv=raincv_hv,rainncv=rainncv_hv  &
                          ,ddy_j1tau=ddy_j1tau_hv,ddy_j2tau=ddy_j2tau_hv       &
                          ,jindx1_tau=jindx1_tau_hv,jindx2_tau=jindx2_tau_hv   &
                          ,r_DoY=r_DoY,kdt=itimestep,dtp=dt                    &
                          ,ugrs=u3d_hv,vgrs=v3d_hv                             &
                          ,tgrs=t3d_hv,q1=qv3d_hv,prsl=p3d_hv,prslk=pi3d_hv    &
                          ,prsi=p3di_hv,zl=z_hv,zi=zi_hv,ntau_d2t=ntau_d2t     &
                          ,days_limb=days_limb,tau_limb=tau_limb               &
                          ,rublten=rublten_hv,rvblten=rvblten_hv               &
                          ,rthblten=rthblten_hv,ugwp_diags=ugwp_diags          &
                          ,dudt_ngw=dudt_ngw_hv,dvdt_ngw=dvdt_ngw_hv           &
                          ,dtdt_ngw=dtdt_ngw_hv,its=its,ite=ite,levs=kte)
    endif



    !  Variables that are INTENT(OUT) or INTENT(INOUT):

    !  3d, layers:
    do k = kts,kte
       do i = its,ite
          rublten(i,k,j) = rublten_hv(i,k)
          rvblten(i,k,j) = rvblten_hv(i,k)
          rthblten(i,k,j)= rthblten_hv(i,k)
          dtaux3d(i,k,j) = dtaux3d_hv(i,k)
          dtauy3d(i,k,j) = dtauy3d_hv(i,k)
       enddo
    enddo
    if (ugwp_diags) then
       do k = kts,kte
          do i = its,ite
             dtaux3d_ls(i,k,j) = dtaux3d_ls_hv(i,k)
             dtauy3d_ls(i,k,j) = dtauy3d_ls_hv(i,k)
             dtaux3d_bl(i,k,j) = dtaux3d_bl_hv(i,k)
             dtauy3d_bl(i,k,j) = dtauy3d_bl_hv(i,k)
             dtaux3d_ss(i,k,j) = dtaux3d_ss_hv(i,k)
             dtauy3d_ss(i,k,j) = dtauy3d_ss_hv(i,k)
             dtaux3d_fd(i,k,j) = dtaux3d_fd_hv(i,k)
             dtauy3d_fd(i,k,j) = dtauy3d_fd_hv(i,k)
          enddo
       enddo
    endif
    if (ugwp_diags.and.ngw_scheme) then
       do k = kts,kte
          do i = its,ite
             dudt_ngw(i,k,j) = dudt_ngw_hv(i,k)
             dvdt_ngw(i,k,j) = dvdt_ngw_hv(i,k)
             dtdt_ngw(i,k,j) = dtdt_ngw_hv(i,k)
          enddo
       enddo
    endif

    !  2d:
    do i = its,ite
       dusfcg(i,j) = dusfcg_hv(i)
       dvsfcg(i,j) = dvsfcg_hv(i)
    enddo
    if (ugwp_diags) then
       do i = its,ite
          dusfc_ls(i,j) = dusfc_ls_hv(i)
          dvsfc_ls(i,j) = dvsfc_ls_hv(i)
          dusfc_bl(i,j) = dusfc_bl_hv(i)
          dvsfc_bl(i,j) = dvsfc_bl_hv(i)
          dusfc_ss(i,j) = dusfc_ss_hv(i)
          dvsfc_ss(i,j) = dvsfc_ss_hv(i)
          dusfc_fd(i,j) = dusfc_fd_hv(i)
          dvsfc_fd(i,j) = dvsfc_fd_hv(i)
       enddo
    endif

    ! Deallocate ugwp_diags and/or ngw_scheme variables if used
    if (ugwp_diags) then
       deallocate (dusfc_ls_hv)
       deallocate (dvsfc_ls_hv)
       deallocate (dusfc_bl_hv)
       deallocate (dvsfc_bl_hv)
       deallocate (dusfc_ss_hv)
       deallocate (dvsfc_ss_hv)
       deallocate (dusfc_fd_hv)
       deallocate (dvsfc_fd_hv)
       deallocate (dtaux3d_ls_hv)
       deallocate (dtauy3d_ls_hv)
       deallocate (dtaux3d_bl_hv)
       deallocate (dtauy3d_bl_hv)
       deallocate (dtaux3d_ss_hv)
       deallocate (dtauy3d_ss_hv)
       deallocate (dtaux3d_fd_hv)
       deallocate (dtauy3d_fd_hv)
       if (ngw_scheme) then
          deallocate (dudt_ngw_hv)
          deallocate (dvdt_ngw_hv)
          deallocate (dtdt_ngw_hv)
       endif
    endif
    if (ngw_scheme) then
       deallocate (xlatd_hv     )
       deallocate (raincv_hv    )
       deallocate (rainncv_hv   )
       deallocate (ddy_j1tau_hv )
       deallocate (ddy_j2tau_hv )
       deallocate (jindx1_tau_hv)
       deallocate (jindx2_tau_hv)
    endif

 enddo ! Outer J-loop

 end subroutine gwdo_ugwp

!=================================================================================================================
end module module_bl_ugwp_gwdo
!=================================================================================================================
