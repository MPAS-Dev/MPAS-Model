!=================================================================================================================
 module module_cu_gf_ctrans
 use mpas_log
 use mpas_kind_types,only: kind_phys=>RKIND

 use mpas_atmphys_constants,only: grav=>gravity,xlv
 use module_cu_gf_shared,only   : get_cloud_bc

 implicit none
 private
 public:: cup_transport_clev,    &
          cup_transport_dn,      &
          cup_transport_tends,   &
          cup_up_moisture_ctrans

!subroutine cup_transport_clev computes the number concentration of cloud liquid water, cloud ice, "ice-friendly"
!and "water-friendly" aerosols at cloud levels, i.e. at the interface between model layers. cup_transport_clev
!also computes the mixing ratio (number concentration) of passive tracers and chemical species at cloud levels.

!subroutine cup_up_moisture_ctrans is a cleaned-up version of subroutine cup_up_moisture.

!subroutine cup_transport_dn computes the number concentration of cloud liquid water, cloud ice, "ice-friendly"
!and "water-friendly" aerosols at cloud levels in downdrafts. also computes mixing ratio and number concentration
!of passive tracers and chemical species in downdrafts.

!subroutine cup_transport_tends computes the tendencies for the number concentration of cloud liquid water, cloud
!ice, "ice-friendly" and "water-friendly" aerosols. also computes the tendencies of passive tracers and chemical
!species.

!subroutine cup_transport_up_moisture is the cleaned-up subroutine cup_up_moisture. Results are exactly the same
!as in module_cu_gf_deep.F when using the "-g" or "-O0 -fp-model precise" options. It does not work with autoconv
!is equal to 2. It includes the convective transport for the number concentration of cloud liquid water, cloud
!ice, "ice-friendly" and "water-friendly" aerosols from the Thompson cloud microphysics scheme. It also includes
!the convective transport of passive tracers. Note that the convective transport of passive tracers could be in
!its own subroutine since they do not interact with moisture, it was more convenient to put them here.


 contains


!=================================================================================================================
 subroutine cup_transport_clev(its,ite,itf,kts,kte,ktf,ntrans,qtrans,nc,ni,nifa,nwfa,qtransl2,ncl2,nil2, &
                               nifal2,nwfal2)
!=================================================================================================================

!input arguments:
 integer,intent(in):: its,ite,itf,kts,kte,ktf
 integer,intent(in):: ntrans

 real(kind=kind_phys),intent(in),dimension(its:ite,kts:kte):: nc,ni,nifa,nwfa
 real(kind=kind_phys),intent(in),dimension(its:ite,kts:kte,ntrans):: qtrans

!inout arguments:
 real(kind=kind_phys),intent(inout),dimension(its:ite,kts:kte):: ncl2,nil2,nifal2,nwfal2
 real(kind=kind_phys),intent(inout),dimension(its:ite,kts:kte,ntrans):: qtransl2

!local variables and arrays:
 integer:: i,k,n

 real(kind=kind_phys),parameter:: half = 0.5_kind_phys

!-----------------------------------------------------------------------------------------------------------------
!call mpas_log_write(' ')
!call mpas_log_write('--- enter subroutine cup_transport_clev:')

 k = kts
 do i = its, itf
    ncl2(i,k) = nc(i,k)
    nil2(i,k) = ni(i,k)
    nifal2(i,k) = nifa(i,k)
    nwfal2(i,k) = nwfa(i,k)
 enddo
 do k = kts+1, ktf
    do i = its, itf
       ncl2(i,k) = half*(nc(i,k-1) + nc(i,k))
       nil2(i,k) = half*(ni(i,k-1) + ni(i,k))
       nifal2(i,k) = half*(nifa(i,k-1) + nifa(i,k))
       nwfal2(i,k) = half*(nwfa(i,k-1) + nwfa(i,k))
    enddo
 enddo

 if(ntrans > 0) then
    do n = 1, ntrans
       k = kts
       do i = its, itf
          qtransl2(i,k,n) = qtrans(i,k,n)
       enddo
       do k = kts+1, ktf
          do i = its, itf
             qtransl2(i,k,n) = half*(qtrans(i,k-1,n) + qtrans(i,k,n))
          enddo
       enddo
    enddo
 endif

!call mpas_log_write('--- enter subroutine cup_transport_clev:')

 end subroutine cup_transport_clev

!=================================================================================================================
 subroutine cup_up_moisture_ctrans(name,ierr,k22,kbcon,ktop,autoconv,z_cup,t_cup,qe_cup,  &
                qes_cup,gamma_cup,q,dby,c1d,up_massentr,up_massdetr,zu,qc,qrc,pw,clw_all, &
                pwav,psum,ntrans,nc,ni,nifa,nwfa,qtrans,ncl2,nil2,nifal2,nwfal2,qtransl2, &
                nc_up,ni_up,nifa_up,nwfa_up,qtrans_up,its,ite,itf,kts,kte,ktf)
!=================================================================================================================

!input arguments:
 character *(*),intent(in):: &
    name            ! type of convection (shallow,mid-convection,deep).

 integer,intent(in):: &
    its,ite,itf,&   ! indices for horizontal dimensions.
    kts,kte,ktf     ! indices for vertical dimensions.

 integer,intent(in):: &
    ntrans          ! number of passive tracers.

 integer,intent(in):: &
    autoconv        !

 integer,intent(in),dimension(its:ite):: &
    k22,&           !
    kbcon,&         ! index of convective cloud base.
    ktop            ! index of convective cloud top.

 real(kind=kind_phys),intent(in),dimension(its:ite,kts:kte):: &
    up_massentr,&   ! normalized entrainment mass flux.
    up_massdetr,&   ! normalized detrainment mass flux.
    zu              ! normalized updraft mass flux.

 real(kind=kind_phys),intent(in),dimension(its:ite,kts:kte):: &
    t_cup,&         !
    qe_cup,&        !
    qes_cup,&       !
    z_cup           !

 real(kind=kind_phys),intent(in),dimension(its:ite,kts:kte):: &
    q               ! layer water vapor mixing ratio.

 real(kind=kind_phys),intent(in),dimension(its:ite,kts:kte):: &
    gamma_cup,&     !
    dby,&           !
    c1d             !

 real(kind=kind_phys),intent(in),dimension(its:ite,kts:kte):: &
    nc,&            ! layer environmental cloud liquid water number concentration.
    ni,&            ! layer environmental cloud ice water number concentration.
    nifa,&          ! layer environmental "ice-friendly" aerosol number concentration from Thompson microphysics.
    nwfa            ! layer environmental "water-friendly" aerosol number concentration from Thompson microphysics.

 real(kind=kind_phys),intent(in),dimension(its:ite,kts:kte):: &
    ncl2,&          ! level environmental cloud liquid water number concentration.
    nil2,&          ! level environmental cloud liquid water number concentration.
    nifal2,&        ! level environmental "ice-friendly" aerosol number concentration from Thompson microphysics.
    nwfal2          ! level environmental "water-friendly" aerosol number concentration from Thompson microphysics.

 real(kind=kind_phys),intent(in),dimension(its:ite,kts:kte,ntrans):: &
    qtrans,&        !
    qtransl2        !

!inout arguments:
 integer,intent(inout),dimension(its:ite) :: &
    ierr            !

!output arguments:
 real(kind=kind_phys),intent(out),dimension(its:ite):: &
    pwav,&          !
    psum            !

 real(kind=kind_phys),intent(out),dimension(its:ite,kts:kte):: &
    clw_all,&       !
    pw,&            !
    qc,&            !
    qrc

 real(kind=kind_phys),intent(out),dimension(its:ite,kts:kte):: &
    nc_up,&         !
    ni_up,&         !
    nifa_up,&       !
    nwfa_up

 real(kind=kind_phys),intent(out),dimension(its:ite,kts:kte,ntrans):: &
    qtrans_up       !

!local variables and arrays:
 integer,parameter:: iall = 0
 integer:: i,k,k2,kb,kt,n

!real(kind=kind_phys),parameter:: c0 = 0.004
 real(kind=kind_phys):: c0,denom,dz,qaver,qrch
 real(kind=kind_phys):: nc_aver,ni_aver,nifa_aver,nwfa_aver,qtrans_aver

 real(kind=kind_phys),dimension(kts:kte):: dummy1
 real(kind=kind_phys),dimension(its:ite,kts:kte):: c0t,clw_allh,pwh,qch,qrcb

!-----------------------------------------------------------------------------------------------------------------
 call mpas_log_write('--- enter subroutine cup_up_moisture_ctrans:')

!--- initialization:
 do i = its, itf
    pwav(i)  = 0._kind_phys
    psum(i)  = 0._kind_phys
 enddo
 do k = kts, ktf
    do i = its, itf
       pw(i,k)  = 0._kind_phys
       pwh(i,k) = 0._kind_phys

       clw_all(i,k)  = 0._kind_phys
       clw_allh(i,k) = 0._kind_phys

       qrc(i,k)  = 0._kind_phys
       qrcb(i,k) = 0._kind_phys
       qc(i,k)   = 0._kind_phys
       qch(i,k)  = 0._kind_phys
       if(ierr(i) .eq. 0) qc(i,k)  = qe_cup(i,k)
       if(ierr(i) .eq. 0) qch(i,k) = qe_cup(i,k)

       !--- conversion to rain:
       !if(t_cup(i,k) > 273.16) then
       !   c0t(i,k) = c0
       !else
       !   c0t(i,k) = c0*exp(0.07*(t_cup(i,k)-273.16))
       !endif
    enddo
 enddo
 do i = its, itf
    if(ierr(i) .eq. 0) then
       k2 = k22(i)
       do k = kts, kte
          dummy1(k) = qe_cup(i,k)
       enddo
       call get_cloud_bc(kte,dummy1,qaver,k2)
       qc(i,k2)  = qaver
       qch(i,k2) = qaver
       do k = kts, k2-1
          qc(i,k)  = qe_cup(i,k)
          qch(i,k) = qe_cup(i,k)
       enddo
    endif
 enddo

!--- number concentrations:
 do k = kts, ktf
    do i = its, itf
       nc_up(i,k)   = 0._kind_phys
       ni_up(i,k)   = 0._kind_phys
       nifa_up(i,k) = 0._kind_phys
       nwfa_up(i,k) = 0._kind_phys
       if(ierr(i) .eq. 0) then
          nc_up(i,k)   = ncl2(i,k)
          ni_up(i,k)   = nil2(i,k)
          nifa_up(i,k) = nifal2(i,k)
          nwfa_up(i,k) = nwfal2(i,k)
       endif
    enddo
 enddo
 do i = its, itf
    if(ierr(i) .eq. 0) then
       k2 = k22(i)
       do k = kts, kte
          dummy1(k) = nc_up(i,k)
       enddo
       call get_cloud_bc(kte,dummy1,nc_aver,k2)
       do k = kts, kte
          dummy1(k) = ni_up(i,k)
       enddo
       call get_cloud_bc(kte,dummy1,ni_aver,k2)
       do k = kts, kte
          dummy1(k) = nifa_up(i,k)
       enddo
       call get_cloud_bc(kte,dummy1,nifa_aver,k2)
       do k = kts, kte
          dummy1(k) = nwfa_up(i,k)
       enddo
       call get_cloud_bc(kte,dummy1,nwfa_aver,k2)

       nc_up(i,k2)   = nc_aver
       ni_up(i,k2)   = ni_aver
       nifa_up(i,k2) = nifa_aver
       nwfa_up(i,k2) = nwfa_aver
       do k = kts, k2-1
          nc_up(i,k) = ncl2(i,k)
          ni_up(i,k) = nil2(i,k)
          nifa_up(i,k) = nifal2(i,k)
          nwfa_up(i,k) = nwfal2(i,k)
       enddo
    endif
 enddo

!--- passive tracers:
 if(ntrans > 0) then
    do n = 1, ntrans
       do i = its, itf
          if(ierr(i) .eq. 0) then
             do k = kts, ktf
                qtrans_up(i,k,n) = qtransl2(i,k,n)
             enddo
          else
             do k = kts, ktf
                qtrans_up(i,k,n) = 0._kind_phys
             enddo
          endif
       enddo
    enddo

    do n = 1, ntrans
       do i = its, ite
          if(ierr(i) .eq. 0) then
             k2 = k22(i)
             do k = kts, kte
                dummy1(k) = qtrans_up(i,k,n)
             enddo
             call get_cloud_bc(kte,dummy1,qtrans_aver,k2)

             qtrans_up(i,k2,n) = qtrans_aver
             do  k = kts, k2-1
                 qtrans_up(i,k,n) = qtransl2(i,k,n)
             enddo
          endif
       enddo
    enddo
 endif

!---
 do i = its, itf
    if(ierr(i) .eq. 0) then
       k2 = k22(i)
       kb = kbcon(i)
       kt = ktop(i)

       !--- below cloud base:
       do k = k2+1, kb
          dz = z_cup(i,k)-z_cup(i,k-1)
          c0 = 0.004
          if(t_cup(i,k) > 273.16) then
             c0t(i,k) = c0
          else
             c0t(i,k) = c0*exp(0.07*(t_cup(i,k)-273.16))
          endif

          qc(i,k)  = (qc(i,k-1)*zu(i,k-1) - .5*up_massdetr(i,k-1)*qc(i,k-1)   &
                     + up_massentr(i,k-1)*q(i,k-1))                           &
                   / (zu(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1))
          qrch = qes_cup(i,k) + 1./xlv*(gamma_cup(i,k)/(1.+gamma_cup(i,k)))*dby(i,k)

          if(k .lt. kb) qrch = qc(i,k)
          if(qc(i,k) .gt. qrch) then
             qrc(i,k)     = (qc(i,k)-qrch)/(1.+c0t(i,k)*dz)
             pw(i,k)      = c0t(i,k)*dz*qrc(i,k)*zu(i,k)
             qc(i,k)      = qrch+qrc(i,k)
             clw_all(i,k) = qrc(i,k)
          endif

          !--- number concentration and aerosols:
          nc_up(i,k) = (nc_up(i,k-1)*zu(i,k-1) - .5*up_massdetr(i,k-1)*nc_up(i,k-1)       &
                     + up_massentr(i,k-1)*nc(i,k-1))                                      &
                     / (zu(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1))
          ni_up(i,k) = (ni_up(i,k-1)*zu(i,k-1)-.5*up_massdetr(i,k-1)*ni_up(i,k-1)         &
                     + up_massentr(i,k-1)*ni(i,k-1))                                      &
                     / (zu(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1))
          nifa_up(i,k) = (nifa_up(i,k-1)*zu(i,k-1) - .5*up_massdetr(i,k-1)*nifa_up(i,k-1) &
                        + up_massentr(i,k-1)*nifa(i,k-1))                                 &
                      / (zu(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1))
          nwfa_up(i,k) = (nwfa_up(i,k-1)*zu(i,k-1) - .5*up_massdetr(i,k-1)*nwfa_up(i,k-1) &
                        + up_massentr(i,k-1)*nwfa(i,k-1))                                 &
                        / (zu(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1))

          !--- passive tracers:
          if(ntrans > 0) then
             do n = 1, ntrans
                qtrans_up(i,k,n) = (qtrans_up(i,k-1,n)*zu(i,k-1) - .5*up_massdetr(i,k-1)*qtrans_up(i,k-1,n) &
                               + up_massentr(i,k-1)*qtrans(i,k-1,n))                                        &
                               / (zu(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1))
             enddo
          endif
       enddo

       !--- above cloud base:
       do k = kb+1, kt

          !--- I think that this check on the value of denom can be moved in the initialization:
          denom = zu(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1)
          if(denom .lt. 1.e-16) then
             ierr(i) = 51
             exit
          endif

          c0 = 0.004
          if(t_cup(i,k) .lt. 270.) c0 = 0.002
          if(t_cup(i,k) > 273.16) then
             c0t(i,k) = c0
          else
             c0t(i,k) = c0*exp(0.07*(t_cup(i,k)-273.16))
          endif

          qc(i,k)  = (qc(i,k-1)*zu(i,k-1) - .5*up_massdetr(i,k-1)*qc(i,k-1)   &
                   + up_massentr(i,k-1)*q(i,k-1))                             &
                   / (zu(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1))

          qch(i,k) = (qch(i,k-1)*zu(i,k-1) - .5*up_massdetr(i,k-1)*qch(i,k-1) &
                   + up_massentr(i,k-1)*q(i,k-1))                             &
                   / (zu(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1))

          qrch = qes_cup(i,k)+(1./xlv)*(gamma_cup(i,k)/(1.+gamma_cup(i,k)))*dby(i,k)
          if(qc(i,k) .le. qrch)  qc(i,k)  = qrch
          if(qch(i,k) .le. qrch) qch(i,k) = qrch

          !--- total condensed water before rainout:
          clw_all(i,k)  = max(0.,qc(i,k)-qrch)
          clw_allh(i,k) = max(0.,qch(i,k)-qrch)
          qrc(i,k)      = max(0.,(qc(i,k)-qrch))  ! /(1.+c0*dz*zu(i,k))
          qrcb(i,k)     = max(0.,(qch(i,k)-qrch)) ! /(1.+c0*dz*zu(i,k))

          dz = z_cup(i,k)-z_cup(i,k-1)
          if(autoconv .eq. 2) then

          else
             if(iall .eq. 0) then
                qrc(i,k) = (qc(i,k)-qrch)/(1.+(c1d(i,k)+c0t(i,k))*dz)
                pw(i,k)  = c0t(i,k)*dz*qrc(i,k)*zu(i,k)
                if(qrc(i,k) .lt. 0.) then
                   qrc(i,k) = 0.
                   pw(i,k)  = 0.
                endif
             else
                qrc(i,k) = 0.
                pw(i,k)  = (qc(i,k)-qrch)*zu(i,k)
                if(pw(i,k).lt.0.)pw(i,k)=0.
             endif
             qc(i,k) = qrc(i,k) + qrch
          endif
          pwav(i) = pwav(i) + pw(i,k)
          psum(i) = psum(i) + clw_all(i,k)*zu(i,k) *dz

          !--- number concentration and aerosols:
          nc_up(i,k) = (nc_up(i,k-1)*zu(i,k-1) - .5*up_massdetr(i,k-1)*nc_up(i,k-1)       &
                     + up_massentr(i,k-1)*nc(i,k-1))                                      &
                     / (zu(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1))
          ni_up(i,k) = (ni_up(i,k-1)*zu(i,k-1) - .5*up_massdetr(i,k-1)*ni_up(i,k-1)       &
                     + up_massentr(i,k-1)*ni(i,k-1))                                      &
                     / (zu(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1))
          nifa_up(i,k) = (nifa_up(i,k-1)*zu(i,k-1) - .5*up_massdetr(i,k-1)*nifa_up(i,k-1) &
                       + up_massentr(i,k-1)*nifa(i,k-1))                                  &
                       / (zu(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1))
          nwfa_up(i,k) = (nwfa_up(i,k-1)*zu(i,k-1) - .5*up_massdetr(i,k-1)*nwfa_up(i,k-1) &
                       + up_massentr(i,k-1)*nwfa(i,k-1))                                  &
                       / (zu(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1))

          !--- passive tracers:
          if(ntrans > 0) then
             do n = 1, ntrans
                qtrans_up(i,k,n) = (qtrans_up(i,k-1,n)*zu(i,k-1) - .5*up_massdetr(i,k-1)*qtrans_up(i,k-1,n) &
                               + up_massentr(i,k-1)*qtrans(i,k-1,n))                                        &
                               / (zu(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1))
             enddo
          endif
       enddo

       !--- do not include liquid/ice in qc
       do  k = k22(i)+1,ktop(i)
           qc(i,k) = qc(i,k)-qrc(i,k)
       enddo
    endif
 enddo

 call mpas_log_write('--- end subroutine cup_up_moisture_ctrans:')

 end subroutine cup_up_moisture_ctrans

!=================================================================================================================
 subroutine cup_transport_dn(ierr,kdn,zd,dn_massentr,dn_massdetr,ntrans,nc,ni,nifa,nwfa,qtrans, &
                        ncl2,nil2,nifal2,nwfal2,qtransl2,nc_dn,ni_dn,nifa_dn,nwfa_dn,qtrans_dn, &
                        its,ite,itf,kts,kte,ktf)
!=================================================================================================================

!input arguments:
 integer,intent(in):: &
    its,ite,itf,&   ! indices for horizontal dimensions.
    kts,kte,ktf     ! indices for vertical dimensions.

 integer,intent(in):: &
    ntrans          ! number of passive tracers.

 integer,intent(in),dimension(its:ite):: &
    kdn             ! index of downdraft originating level.

 real(kind=kind_phys),intent(in),dimension(its:ite,kts:kte):: &
    dn_massentr,&   ! normalized entrainment mass flux.
    dn_massdetr,&   ! normalized detrainment mass flux.
    zd              ! normalized downdraft mass flux.

 real(kind=kind_phys),intent(in),dimension(its:ite,kts:kte):: &
    nc,&            ! layer environmental cloud liquid water number concentration.
    ni,&            ! layer environmental cloud ice water number concentration.
    nifa,&          ! layer environmental "ice-friendly" aerosol number concentration from Thompson microphysics.
    nwfa            ! layer environmental "water-friendly" aerosol number concentration from Thompson microphysics.

 real(kind=kind_phys),intent(in),dimension(its:ite,kts:kte):: &
    ncl2,&          ! level environmental cloud liquid water number concentration.
    nil2,&          ! level environmental cloud liquid water number concentration.
    nifal2,&        ! level environmental "ice-friendly" aerosol number concentration from Thompson microphysics.
    nwfal2          ! level environmental "water-friendly" aerosol number concentration from Thompson microphysics.

 real(kind=kind_phys),intent(in),dimension(its:ite,kts:kte,ntrans):: &
    qtrans,&        !
    qtransl2        !


!inout arguments:
 integer,intent(inout),dimension(its:ite) :: &
    ierr            !


!output arguments:
 real(kind=kind_phys),intent(out),dimension(its:ite,kts:kte):: &
    nc_dn,&         !
    ni_dn,&         !
    nifa_dn,&       !
    nwfa_dn         !

 real(kind=kind_phys),intent(out),dimension(its:ite,kts:kte,ntrans):: &
    qtrans_dn       !


!local variables and arrays:
 integer:: i,k,kd,n

 real(kind=kind_phys):: denom

!-----------------------------------------------------------------------------------------------------------------
!call mpas_log_write(' ')
!call mpas_log_write('--- enter subroutine cup_transport_dn:')

!--- initialization:
 do k = kts, ktf
    do i = its, itf
       nc_dn(i,k)   = 0._kind_phys
       ni_dn(i,k)   = 0._kind_phys
       nifa_dn(i,k) = 0._kind_phys
       nwfa_dn(i,k) = 0._kind_phys
    enddo
 enddo
 if(ntrans > 0) then
    do n = 1,ntrans
       do k = kts, ktf
          do i = its, itf
             qtrans_dn(i,k,n) = 0._kind_phys
          enddo
       enddo
    enddo
 endif

!--- computation of the in-cloud cloud liquid water and ice number concentrations,"ice-friendly" and
!    "water-friendly" number concentrations from the Thompson cloud microphysics scheme,and in-cloud
!    mass mixing ratios of passive tracers and chemical species due to cloud downdrafts:
 do i = its, itf
    if(ierr(i) .eq. 0) then
       kd = kdn(i)
       do k = kd-1, kts, -1
          denom = zd(i,k+1) -.5*dn_massdetr(i,k) + dn_massentr(i,k)
          if(denom .lt. 1.e-16) ierr(i) = 51
       enddo
    endif

    if(ierr(i) .eq. 0) then
       kd = kdn(i)

       nc_dn(i,kd)   = ncl2(i,kd)
       ni_dn(i,kd)   = nil2(i,kd)
       nifa_dn(i,kd) = nifal2(i,kd)
       nwfa_dn(i,kd) = nwfal2(i,kd)
       do k = kd-1, kts, -1
          nc_dn(i,k) = (nc_dn(i,k+1)*zd(i,k+1) -.5*dn_massdetr(i,k)*nc_dn(i,k+1)       &
                     +  dn_massentr(i,k)*nc(i,k))                                      &
                     / (zd(i,k+1)-.5*dn_massdetr(i,k)+dn_massentr(i,k))

          ni_dn(i,k) = (ni_dn(i,k+1)*zd(i,k+1) -.5*dn_massdetr(i,k)*ni_dn(i,k+1)       &
                     +  dn_massentr(i,k)*ni(i,k))                                      &
                     / (zd(i,k+1)-.5*dn_massdetr(i,k)+dn_massentr(i,k))

          nifa_dn(i,k) = (nifa_dn(i,k+1)*zd(i,k+1) -.5*dn_massdetr(i,k)*nifa_dn(i,k+1) &
                     +  dn_massentr(i,k)*nifa(i,k))                                    &
                     / (zd(i,k+1)-.5*dn_massdetr(i,k)+dn_massentr(i,k))

          nwfa_dn(i,k) = (nwfa_dn(i,k+1)*zd(i,k+1) -.5*dn_massdetr(i,k)*nwfa_dn(i,k+1) &
                     +  dn_massentr(i,k)*nwfa(i,k))                                    &
                     / (zd(i,k+1)-.5*dn_massdetr(i,k)+dn_massentr(i,k))
       enddo

       if(ntrans > 0) then
          do n = 1,ntrans
             qtrans_dn(i,kd,n) = qtransl2(i,kd,n)
             do k = kd-1, kts, -1
                qtrans_dn(i,k,n) = (qtrans_dn(i,k+1,n)*zd(i,k+1) -.5*dn_massdetr(i,k)*qtrans_dn(i,k+1,n) &
                                 +  dn_massentr(i,k)*qtrans(i,k,n))                                      &
                                 / (zd(i,k+1)-.5*dn_massdetr(i,k)+dn_massentr(i,k))
             enddo
          enddo
       endif
    endif
 enddo

!call mpas_log_write('--- exit subroutine cup_transport_dn:')

 end subroutine cup_transport_dn

!=================================================================================================================
 subroutine cup_transport_tends(ierr,k22,kbcon,ktop,kdn,ntrans,xmb,pres,ncl2,nil2,nifal2,nwfal2,qtransl2,      &
                                nc_up,ni_up,nifa_up,nwfa_up,qtrans_up,nc_dn,ni_dn,nifa_dn,nwfa_dn,qtrans_dn,   &
                                edt,zu,zd,outnc,outni,outnifa,outnwfa,outqtrans,its,ite,itf,kts,kte,ktf)
!=================================================================================================================

!input arguments:
 integer,intent(in):: &
    its,ite,itf,&   ! indices for horizontal dimensions.
    kts,kte,ktf     ! indices for vertical dimensions.

 integer,intent(in):: &
    ntrans

 integer,intent(in),dimension(its:ite) :: &
    ierr,&          !
    k22,&           !
    kbcon,&         !
    ktop,&          !
    kdn

 real(kind=kind_phys),intent(in),dimension(its:ite):: &
    edt,&           !
    xmb             !

 real(kind=kind_phys),intent(in),dimension(its:ite,kts:kte):: &
    pres,&          !
    zd,&            !
    zu

 real(kind=kind_phys),intent(in),dimension(its:ite,kts:kte):: &
    ncl2,&          ! level environmental cloud liquid water number concentration.
    nil2,&          ! level environmental cloud liquid water number concentration.
    nifal2,&        ! level environmental "ice-friendly" aerosol number concentration from Thompson microphysics.
    nwfal2          ! level environmental "water-friendly" aerosol number concentration from Thompson microphysics.

 real(kind=kind_phys),intent(in),dimension(its:ite,kts:kte,ntrans):: &
    qtransl2        ! level environmental tracers.

 real(kind=kind_phys),intent(in),dimension(its:ite,kts:kte):: &
    nc_dn,&         !
    ni_dn,&         !
    nifa_dn,&       !
    nwfa_dn,&       !
    nc_up,&         !
    ni_up,&         !
    nifa_up,&       !
    nwfa_up         !

 real(kind=kind_phys),intent(in),dimension(its:ite,kts:kte,ntrans):: &
    qtrans_up,&     !
    qtrans_dn       !

!output arguments:
 real(kind=kind_phys),intent(out),dimension(its:ite,kts:kte):: &
    outnc,&         !
    outni,&         !
    outnifa,&       !
    outnwfa         !

 real(kind=kind_phys),intent(out),dimension(its:ite,kts:kte,ntrans):: &
    outqtrans       !

!local variables and arrays:
 integer:: i,k,k2,kb,kd,kt,n

 real(kind=kind_phys):: tem,tem2
 real(kind=kind_phys),dimension(kts:kte):: ratio
 real(kind=kind_phys),dimension(its:ite,kts:kte):: flxncu,flxniu,flxnifau,flxnwfau
 real(kind=kind_phys),dimension(its:ite,kts:kte):: flxncd,flxnid,flxnifad,flxnwfad
 real(kind=kind_phys),dimension(its:ite,kts:kte,ntrans):: flxqu,flxqd

!-----------------------------------------------------------------------------------------------------------------
!call mpas_log_write(' ')
!call mpas_log_write('--- subroutine cup_transport_tends:')

 do k = kts, kte
    do i = its, ite
       outnc(i,k)    = 0._kind_phys
       outni(i,k)    = 0._kind_phys
       outnifa(i,k)  = 0._kind_phys
       outnwfa(i,k)  = 0._kind_phys

       flxncu(i,k)   = 0._kind_phys
       flxniu(i,k)   = 0._kind_phys
       flxnifau(i,k) = 0._kind_phys
       flxnwfau(i,k) = 0._kind_phys
       flxncd(i,k)   = 0._kind_phys
       flxnid(i,k)   = 0._kind_phys
       flxnifad(i,k) = 0._kind_phys
       flxnwfad(i,k) = 0._kind_phys

       do n = 1, ntrans
          outqtrans(i,k,n)  = 0._kind_phys
          flxqu(i,k,n) = 0._kind_phys
          flxqd(i,k,n) = 0._kind_phys
       enddo
    enddo
 enddo

 do i = its, itf
    if(ierr(i) .eq. 0) then
       k2 = k22(i)
       kb = kbcon(i)
       kt = ktop(i)
       kd = kdn(i)

       do k = kts, ktf
          flxncu(i,k)   = zu(i,k)*(nc_up(i,k)-ncl2(i,k))
          flxniu(i,k)   = zu(i,k)*(ni_up(i,k)-nil2(i,k))
          flxnifau(i,k) = zu(i,k)*(nifa_up(i,k)-nifal2(i,k))
          flxnwfau(i,k) = zu(i,k)*(nwfa_up(i,k)-nwfal2(i,k))

          flxncd(i,k)   = edt(i)*zd(i,k)*(nc_dn(i,k)-ncl2(i,k))
          flxnid(i,k)   = edt(i)*zd(i,k)*(ni_dn(i,k)-nil2(i,k))
          flxnifad(i,k) = edt(i)*zd(i,k)*(nifa_dn(i,k)-nifal2(i,k))
          flxnwfad(i,k) = edt(i)*zd(i,k)*(nwfa_dn(i,k)-nwfal2(i,k))
       enddo

       do k = kts, ktf-1
          ratio(k) = 0.01_kind_phys*grav/(pres(i,k) - pres(i,k+1))
          ratio(k) = ratio(k)*xmb(i)
          outnc(i,k)   = (flxncu(i,k)-flxncu(i,k+1)) - (flxncd(i,k)-flxncd(i,k+1))
          outni(i,k)   = (flxniu(i,k)-flxniu(i,k+1)) - (flxnid(i,k)-flxnid(i,k+1))
          outnifa(i,k) = (flxnifau(i,k)-flxnifau(i,k+1)) - (flxnifad(i,k)-flxnifad(i,k+1))
          outnwfa(i,k) = (flxnwfau(i,k)-flxnwfau(i,k+1)) - (flxnwfad(i,k)-flxnwfad(i,k+1))

          outnc(i,k)   = ratio(k)*outnc(i,k)
          outni(i,k)   = ratio(k)*outni(i,k)
          outnifa(i,k) = ratio(k)*outnifa(i,k)
          outnwfa(i,k) = ratio(k)*outnwfa(i,k)
       enddo
    endif
 enddo

 if(ntrans > 0) then
    do i = its, ite
       if(ierr(i) .eq. 0) then
          k2 = k22(i)
          kb = kbcon(i)
          kt = ktop(i)
          kd = kdn(i)

          do n = 1, ntrans
             do k = kts, ktf
                flxqu(i,k,n) = zu(i,k)*(qtrans_up(i,k,n)-qtransl2(i,k,n))
                flxqd(i,k,n) = edt(i)*zd(i,k)*(qtrans_dn(i,k,n)-qtransl2(i,k,n))
             enddo
             do k = kts, ktf-1
                ratio(k) = 0.01_kind_phys*grav/(pres(i,k) - pres(i,k+1))
                ratio(k) = ratio(k)*xmb(i)
                outqtrans(i,k,n) = (flxqu(i,k,n)-flxqu(i,k+1,n)) - (flxqd(i,k,n)-flxqd(i,k+1,n))
                outqtrans(i,k,n) = ratio(k)*outqtrans(i,k,n)
             enddo
          enddo
       endif
    enddo
 endif

!call mpas_log_write('--- end subroutine cup_transport_tends:')
!call mpas_log_write(' ')

 end subroutine cup_transport_tends

!=================================================================================================================
 end module module_cu_gf_ctrans
!=================================================================================================================
