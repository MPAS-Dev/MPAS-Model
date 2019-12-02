!=================================================================================================================
 module module_cu_gf_ldf
!=================================================================================================================
 use mpas_kind_types,only: kind_phys => RKIND
 use mpas_log

 use mpas_atmphys_constants,only: xlv
 use module_cu_gf_shared,only: get_cloud_bc

 private
 public:: cup_up_moisture_ldf

!subroutine cup_up_moisture_ldf is the cleaned-up subroutine cup_up_moisture. Results are exactly the same as in
!module_cu_gf_deep.F. It does not work if autoconversion is equal to 2. It includes the convective transport for
!the number concentration of cloud liquid water, cloud ice, and "ice-friendly" and "water-friendly" aerosols from
!the Thompson cloud microphysics.
!Laura D. Fowler (laura@ucar.edu) / 2019-09-13.


 contains


!=================================================================================================================
   subroutine cup_up_moisture_ldf( &
                     name,ierr,k22,kbcon,ktop,autoconv,z_cup,t_cup,qe_cup, &
                     qes_cup,gamma_cup,q,dby,c1d,up_massentr,up_massdetr,  &
                     zu,qc,qrc,pw,clw_all,pwav,psum,                       &
                     nc,ni,nifa,nwfa,ncl2,nil2,nifal2,nwfal2,              &
                     nc_up,ni_up,nifa_up,nwfa_up,                          &
                     its,ite,itf,kts,kte,ktf)
!=================================================================================================================

!input arguments:
 character *(*),intent(in):: &
    name            ! type of convection (shallow,mid-convection,deep)      

 integer,intent(in):: &
    its,ite,itf,&   ! indices for horizontal dimensions.
    kts,kte,ktf     ! indices for vertical dimensions.

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

!inout arguments:
 integer,intent(inout),dimension(its:ite) :: &
    ierr            !

!output arguments:
 real(kind=kind_phys),intent(out),dimension(its:ite):: &
    pwav,&          !
    psum            !

 real(kind=kind_phys),intent(out),dimension(its:ite,kts:kte):: &
    clw_all,&
    pw,&
    qc,&
    qrc
 
 real(kind=kind_phys),intent(out),dimension(its:ite,kts:kte):: &
    nc_up,&         !
    ni_up,&         !
    nifa_up,&       !
    nwfa_up

!local variables and arrays:
 integer,parameter:: iall = 0
 integer:: i,k,k2,kb,kt

!real(kind=kind_phys),parameter:: c0 = 0.004
 real(kind=kind_phys):: c0,denom,dz,qaver,qrch
 real(kind=kind_phys):: nc_aver,ni_aver,nifa_aver,nwfa_aver

 real(kind=kind_phys),dimension(kts:kte):: dummy1
 real(kind=kind_phys),dimension(its:ite,kts:kte):: c0t,clw_allh,pwh,qch,qrcb

!-----------------------------------------------------------------------------------------------------------------

!call mpas_log_write('--- enter subroutine cup_up_moisture_ldf:')

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
       enddo
       !--- do not include liquid/ice in qc
       do  k = k22(i)+1,ktop(i)
           qc(i,k) = qc(i,k)-qrc(i,k)
       enddo   

    endif
 enddo

!call mpas_log_write('--- end subroutine cup_up_moisture_ldf:')

 end subroutine cup_up_moisture_ldf

!=================================================================================================================
 end module module_cu_gf_ldf
!=================================================================================================================
