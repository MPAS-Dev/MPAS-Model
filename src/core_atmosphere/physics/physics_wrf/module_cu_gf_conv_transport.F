!=================================================================================================================
 module module_cu_gf_conv_transport
 use mpas_log
 use mpas_kind_types,only: kind_phys=>RKIND

 use mpas_atmphys_constants,only: grav=>gravity

 implicit none
 private
 public:: cup_transport_clev, &
          cup_transport_dn,   &
          cup_transport_tends


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
       k = 1
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
 subroutine cup_transport_dn(ierr,kdn,zd,dn_massentr,dn_massdetr,nc,ni,nifa,nwfa, &
                             ncl2,nil2,nifal2,nwfal2,nc_dn,ni_dn,nifa_dn,nwfa_dn, &
                             its,ite,itf,kts,kte,ktf)
!=================================================================================================================

!input arguments:
 integer,intent(in):: &
    its,ite,itf,&   ! indices for horizontal dimensions.
    kts,kte,ktf     ! indices for vertical dimensions.

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


!inout arguments:
 integer,intent(inout),dimension(its:ite) :: &
    ierr            !


!output arguments:
 real(kind=kind_phys),intent(out),dimension(its:ite,kts:kte):: &
    nc_dn,&         !
    ni_dn,&         !
    nifa_dn,&       !
    nwfa_dn         !

!local variables and arrays:
 integer:: i,k,kd

 real(kind=kind_phys):: denom

!-----------------------------------------------------------------------------------------------------------------
!call mpas_log_write(' ')
!call mpas_log_write('--- subroutine cup_transport_dn:')

 do k = kts, ktf
    do i = its, itf
       nc_dn(i,k)   = 0._kind_phys
       ni_dn(i,k)   = 0._kind_phys
       nifa_dn(i,k) = 0._kind_phys
       nwfa_dn(i,k) = 0._kind_phys
    enddo
 enddo

 do i = its, itf
    if(ierr(i) .eq. 0) then
       kd = kdn(i)
       do k = kd-1, 1, -1
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

       do k = kd-1, 1, -1
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
    endif

 enddo

!call mpas_log_write('--- subroutine cup_transport_dn:')

 end subroutine cup_transport_dn

!=================================================================================================================
 subroutine cup_transport_tends(ierr,k22,kbcon,ktop,kdn,xmb,pres,ncl2,nil2,nifal2,nwfal2,nc_up,ni_up,nifa_up, &
                                nwfa_up,nc_dn,ni_dn,nifa_dn,nwfa_dn,edt,zu,zd,outnc,outni,outnifa,outnwfa,    &
                                its,ite,itf,kts,kte,ktf)
!=================================================================================================================

!input arguments:
 integer,intent(in):: &
    its,ite,itf,&   ! indices for horizontal dimensions.
    kts,kte,ktf     ! indices for vertical dimensions.

 integer,intent(inout),dimension(its:ite) :: &
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

 real(kind=kind_phys),intent(in),dimension(its:ite,kts:kte):: &
    nc_dn,&         !
    ni_dn,&         !
    nifa_dn,&       !
    nwfa_dn,&       !
    nc_up,&         !
    ni_up,&         !
    nifa_up,&       !
    nwfa_up         !
    

!output arguments:
 real(kind=kind_phys),intent(out),dimension(its:ite,kts:kte):: &
    outnc,&         !
    outni,&         !
    outnifa,&       !
    outnwfa         !

!local variables and arrays:
 integer:: i,k,k2,kb,kd,kt

 real(kind=kind_phys),dimension(kts:kte):: ratio
 real(kind=kind_phys),dimension(its:ite,kts:kte):: flxncu,flxniu,flxnifau,flxnwfau
 real(kind=kind_phys),dimension(its:ite,kts:kte):: flxncd,flxnid,flxnifad,flxnwfad

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
          outnc(i,k)   = -(flxncu(i,k+1)-flxncu(i,k)) + (flxncd(i,k+1)-flxncd(i,k))
          outni(i,k)   = -(flxniu(i,k+1)-flxniu(i,k)) + (flxnid(i,k+1)-flxnid(i,k))
          outnifa(i,k) = -(flxnifau(i,k+1)-flxnifau(i,k)) + (flxnifad(i,k+1)-flxnifad(i,k))
          outnwfa(i,k) = -(flxnwfau(i,k+1)-flxnwfau(i,k)) + (flxnwfad(i,k+1)-flxnwfad(i,k))

          outnc(i,k) = ratio(k)*outnc(i,k)
          outni(i,k) = ratio(k)*outni(i,k)
          outnifa(i,k) = ratio(k)*outnifa(i,k)
          outnwfa(i,k) = ratio(k)*outnwfa(i,k)
       enddo
    endif
 enddo

!do i = its, itf
!   if(ierr(i) .eq. 0) then
!      k2 = k22(i)
!      kb = kbcon(i)
!      kt = ktop(i)
!      kd = kdn(i)

!      k = kts
!      tem = 0.01_kind_phys*grav/(pres(i,k) - pres(i,k+1))
!      tem = tem*xmb(i)

!      outnc(i,k) = zu(i,k+1)*(nc_up(i,k+1)-ncl2(i,k+1)) - zd(i,k+1)*(nc_dn(i,k+1)-ncl2(i,k+1))*edt(i)
!      outni(i,k) = zu(i,k+1)*(ni_up(i,k+1)-nil2(i,k+1)) - zd(i,k+1)*(ni_dn(i,k+1)-nil2(i,k+1))*edt(i)
!      outnifa(i,k) = zu(i,k+1)*(nifa_up(i,k+1)-nifal2(i,k+1)) - zd(i,k+1)*(nifa_dn(i,k+1)-nifal2(i,k+1))*edt(i)
!      outnwfa(i,k) = zu(i,k+1)*(nwfa_up(i,k+1)-nwfal2(i,k+1)) - zd(i,k+1)*(nwfa_dn(i,k+1)-nwfal2(i,k+1))*edt(i)

!      outnc(i,k)   = -outnc(i,k)*tem
!      outni(i,k)   = -outni(i,k)*tem
!      outnifa(i,k) = -outnifa(i,k)*tem
!      outnwfa(i,k) = -outnwfa(i,k)*tem

!      do k = kts+1, kt
!         tem = 0.01_kind_phys*grav/(pres(i,k) - pres(i,k+1))
!         tem = tem*xmb(i)

!         outnc(i,k) = zu(i,k+1)*(nc_up(i,k+1)-ncl2(i,k+1)) - zu(i,k)*(nc_up(i,k)-ncl2(i,k))           &
!                    - zd(i,k+1)*(nc_dn(i,k+1)-ncl2(i,k+1)) - zd(i,k)*(nc_dn(i,k)-ncl2(i,k))*edt(i)

!         outni(i,k) = zu(i,k+1)*(ni_up(i,k+1)-nil2(i,k+1)) - zu(i,k)*(ni_up(i,k)-nil2(i,k))           &
!                    - zd(i,k+1)*(ni_dn(i,k+1)-nil2(i,k+1)) - zd(i,k)*(ni_dn(i,k)-nil2(i,k))*edt(i)

!         outnifa(i,k) = zu(i,k+1)*(nifa_up(i,k+1)-nifal2(i,k+1)) - zu(i,k)*(nifa_up(i,k)-nifal2(i,k)) &
!                      - zd(i,k+1)*(nifa_dn(i,k+1)-nifal2(i,k+1)) - zd(i,k)*(nifa_dn(i,k)-nifal2(i,k))*edt(i)

!         outnwfa(i,k) = zu(i,k+1)*(nwfa_up(i,k+1)-nwfal2(i,k+1)) - zu(i,k)*(nwfa_up(i,k)-nwfal2(i,k)) &
!                      - zd(i,k+1)*(nwfa_dn(i,k+1)-nwfal2(i,k+1)) - zd(i,k)*(nwfa_dn(i,k)-nwfal2(i,k))*edt(i)

!         outnc(i,k)   = -outnc(i,k)*tem
!         outni(i,k)   = -outni(i,k)*tem
!         outnifa(i,k) = -outnifa(i,k)*tem
!         outnwfa(i,k) = -outnwfa(i,k)*tem
!      enddo
!   else
!      do k = kts, kte
!         outnc(i,k)   = 0._kind_phys
!         outni(i,k)   = 0._kind_phys
!         outnifa(i,k) = 0._kind_phys
!         outnwfa(i,k) = 0._kind_phys
!      enddo
!   endif
!enddo

!call mpas_log_write('--- subroutine cup_transport_tends:')
!call mpas_log_write(' ')

 end subroutine cup_transport_tends

!=================================================================================================================
 end module module_cu_gf_conv_transport
!=================================================================================================================
