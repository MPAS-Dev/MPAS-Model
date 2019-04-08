!=================================================================================================================
 module module_mp_thompson_cldfra3
#if defined(mpas)
 use mpas_atmphys_utilities, only: physics_message,physics_error_fatal
#define FATAL_ERROR(M) call physics_error_fatal(M)
#define WRITE_MESSAGE(M) call physics_message(M)
#else
 use module_wrf_error
#define FATAL_ERROR(M) call wrf_error_fatal(M)
#define WRITE_MESSAGE(M) call wrf_message(M)
#endif

!module_mp_thompson_cldfra3 contains the subroutine cal_cldfra3 which calculates the cloud fraction as a function
!of relative humidity. The subroutine cal_cldfra3 was tested in WRF 3.8.1 with the Thompson cloud microphysics
!and should not be used with other cloud microphysics schemes.

!subroutine cal_cldfra3 was originally copied from ./phys/module_radiation_driver.F from WRF version 3.8.1.
!Laura D. Fowler (laura@ucar.edu) / 2016-09-22.

! add-ons and modifications to sourcecode:
! ----------------------------------------
! * in subroutine find_cloudLayers, changed the line k = k_m12C+2 to k = min(k_m12C,k_m12C+2) to avoid k greater
!   than the model-top index.
!   Laura D. Fowler (laura@ucar.edu)/2016-09-23. 
! * updated subroutine cal_cldfra3 to that from WRF-4.1.
!   Laura D. Fowler (laura@ucar.edu)/2019-04-08.

 use mpas_atmphys_functions,only: rslf,rsif

 implicit none
 private
 public:: cal_cldfra3


 contains


!=================================================================================================================

!+---+-----------------------------------------------------------------+
!..Cloud fraction scheme by G. Thompson (NCAR-RAL), not intended for
!.. combining with any cumulus or shallow cumulus parameterization
!.. scheme cloud fractions.  This is intended as a stand-alone for
!.. cloud fraction and is relatively good at getting widespread stratus
!.. and stratoCu without caring whether any deep/shallow Cu param schemes
!.. is making sub-grid-spacing clouds/precip.  Under the hood, this
!.. scheme follows Mocko and Cotton (1995) in applicaiton of the
!.. Sundqvist et al (1989) scheme but using a grid-scale dependent
!.. RH threshold, one each for land v. ocean points.
!+---+-----------------------------------------------------------------+
!
!+---+-----------------------------------------------------------------+

      SUBROUTINE cal_cldfra3(CLDFRA, qv, qc, qi, qs, dz,                &
     &                 p, t, XLAND, gridkm,                             &
     &                 modify_qvapor, max_relh,                         &
     &                 kts,kte, debug_flag)
!
      USE module_mp_thompson   , ONLY : rsif, rslf
      IMPLICIT NONE
!
      INTEGER, INTENT(IN):: kts, kte
      LOGICAL, INTENT(IN):: modify_qvapor
      REAL, DIMENSION(kts:kte), INTENT(INOUT):: qv, qc, qi, cldfra
      REAL, DIMENSION(kts:kte), INTENT(IN):: p, t, dz, qs
      REAL, INTENT(IN):: gridkm, XLAND, max_relh
      LOGICAL, INTENT(IN):: debug_flag

!..Local vars.
      REAL:: RH_00L, RH_00O, RH_00
      REAL:: entrmnt=0.5
      INTEGER:: k
      REAL:: TC, qvsi, qvsw, RHUM, delz
      REAL, DIMENSION(kts:kte):: qvs, rh, rhoa

      character*512 dbg_msg

!+---+

!..Initialize cloud fraction, compute RH, and rho-air.

         DO k = kts,kte
            CLDFRA(K) = 0.0

            qvsw = rslf(P(k), t(k))
            qvsi = rsif(P(k), t(k))

            tc = t(k) - 273.15
            if (tc .ge. -12.0) then
               qvs(k) = qvsw
            elseif (tc .lt. -35.0) then
               qvs(k) = qvsi
            else
               qvs(k) = qvsw - (qvsw-qvsi)*(-12.0-tc)/(-12.0+35.)
            endif

            rh(k) = MAX(0.01, qv(k)/qvs(k))
            rhoa(k) = p(k)/(287.0*t(k))
         ENDDO


!..First cut scale-aware. Higher resolution should require closer to
!.. saturated grid box for higher cloud fraction.  Simple functions
!.. chosen based on Mocko and Cotton (1995) starting point and desire
!.. to get near 100% RH as grid spacing moves toward 1.0km, but higher
!.. RH over ocean required as compared to over land.

      DO k = kts,kte

         delz = MAX(100., dz(k))
         RH_00L = 0.65 + SQRT(1./(25.0+gridkm*gridkm*delz*0.01))
         RH_00O = 0.81 + SQRT(1./(50.0+gridkm*gridkm*delz*0.01))
         RHUM = rh(k)

         if (qc(k).gt.1.E-7 .or. qi(k).ge.1.E-7                         &
     &                    .or. (qs(k).gt.1.E-6 .and. t(k).lt.273.)) then
            CLDFRA(K) = 1.0
            qvs(k) = qv(k)
         else

            IF ((XLAND-1.5).GT.0.) THEN                                  !--- Ocean
               RH_00 = RH_00O
            ELSE                                                         !--- Land
               RH_00 = RH_00L
            ENDIF

            tc = t(k) - 273.15
            if (tc .lt. -12.0) RH_00 = RH_00L

            if (tc .ge. 20.0) then
               CLDFRA(K) = 0.0
            elseif (tc .ge. -12.0) then
               RHUM = MIN(rh(k), 1.0)
               CLDFRA(K) = MAX(0., 1.0-SQRT((1.005-RHUM)/(1.005-RH_00)))
            else
               if (max_relh.gt.1.12 .or. (.NOT.(modify_qvapor)) ) then
!..For HRRR model, the following look OK.
                  RHUM = MIN(rh(k), 1.45)
                  RH_00 = RH_00 + (1.45-RH_00)*(-12.0-tc)/(-12.0+100.)
             if (RH_00 .ge. 1.5) then
              WRITE (dbg_msg,*) ' FATAL: RH_00 too large (1.5): ', RH_00, RH_00L, tc
              FATAL_ERROR(dbg_msg)
             endif
                  CLDFRA(K) = MAX(0., 1.0-SQRT((1.5-RHUM)/(1.5-RH_00)))
               else
!..but for the GFS model, RH is way lower.
                  RHUM = MIN(rh(k), 1.05)
                  RH_00 = RH_00 + (1.05-RH_00)*(-12.0-tc)/(-12.0+100.)
             if (RH_00 .ge. 1.05) then
              WRITE (dbg_msg,*) ' FATAL: RH_00 too large (1.05): ', RH_00, RH_00L, tc
              FATAL_ERROR(dbg_msg)
             endif
                  CLDFRA(K) = MAX(0., 1.0-SQRT((1.05-RHUM)/(1.05-RH_00)))
               endif
            endif
            if (CLDFRA(K).gt.0.) CLDFRA(K) = MAX(0.01, MIN(CLDFRA(K),0.9))

            if (debug_flag) then
              WRITE (dbg_msg,*) 'DEBUG-GT: cloud fraction: ', RH_00, RHUM, CLDFRA(K)
              WRITE_MESSAGE(dbg_msg)
            endif

         endif
      ENDDO


      call find_cloudLayers(qvs, cldfra, T, P, Dz, entrmnt,             &
     &                      debug_flag, qc, qi, qs, kts,kte)

!..Do a final total column adjustment since we may have added more than 1mm
!.. LWP/IWP for multiple cloud decks.

      call adjust_cloudFinal(cldfra, qc, qi, rhoa, dz, kts,kte)

      if (debug_flag) then
        WRITE(dbg_msg,*) 'DEBUG-GT:  Made-up fake profile of clouds'
        WRITE_MESSAGE(dbg_msg)
        do k = kte, kts, -1
           write(dbg_msg,'(f7.2, 2x, f7.2, 2x, f6.4, 2x, f7.3, 2x,  f15.7, 2x, f15.7)') &
     &          T(k)-273.15, P(k)*0.01, rh(k), cldfra(k)*100., qc(k)*1000.,qi(k)*1000.
           WRITE_MESSAGE(dbg_msg)
        enddo
      endif


      if (modify_qvapor) then
         DO k = kts,kte
            if (cldfra(k).gt.0.20 .and. cldfra(k).lt.1.0) then
               qv(k) = qvs(k)
            endif
         ENDDO
      endif


      END SUBROUTINE cal_cldfra3

!+---+-----------------------------------------------------------------+
!..From cloud fraction array, find clouds of multi-level depth and compute
!.. a reasonable value of LWP or IWP that might be contained in that depth,
!.. unless existing LWC/IWC is already there.

      SUBROUTINE find_cloudLayers(qvs1d, cfr1d, T1d, P1d, Dz1d, entrmnt,&
     &                            debugfl, qc1d, qi1d, qs1d, kts,kte)
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN):: kts, kte
      LOGICAL, INTENT(IN):: debugfl
      REAL, INTENT(IN):: entrmnt
      REAL, DIMENSION(kts:kte), INTENT(IN):: qs1d,qvs1d,T1d,P1d,Dz1d
      REAL, DIMENSION(kts:kte), INTENT(INOUT):: cfr1d, qc1d, qi1d

!..Local vars.
      REAL, DIMENSION(kts:kte):: theta
      REAL:: theta1, theta2, delz
      INTEGER:: k, k2, k_tropo, k_m12C, k_cldb, k_cldt, kbot
      LOGICAL:: in_cloud
      character*512 dbg_msg

!+---+

      k_m12C = 0
      DO k = kte, kts, -1
         theta(k) = T1d(k)*((100000.0/P1d(k))**(287.05/1004.))
         if (T1d(k)-273.16 .gt. -12.0 .and. P1d(k).gt.10100.0) k_m12C = MAX(k_m12C, k)
      ENDDO
      if (k_m12C .le. kts) k_m12C = kts

      if (k_m12C.gt.kte-3) then
        WRITE (dbg_msg,*) 'DEBUG-GT: WARNING, no possible way neg12C can occur this high up: ', k_m12C
        WRITE_MESSAGE(dbg_msg)
        do k = kte, kts, -1
           WRITE (dbg_msg,*) 'DEBUG-GT,  k,  P, T : ', k,P1d(k)*0.01,T1d(k)-273.15
           WRITE_MESSAGE(dbg_msg)
        enddo
        FATAL_ERROR('FATAL ERROR, problem in temperature profile.')
      endif

!..Find tropopause height, best surrogate, because we would not really
!.. wish to put fake clouds into the stratosphere.  The 10/1500 ratio
!.. d(Theta)/d(Z) approximates a vertical line on typical SkewT chart
!.. near typical (mid-latitude) tropopause height.  Since messy data
!.. could give us a false signal of such a transition, do the check over 
!.. three K-level change, not just a level-to-level check.  This method
!.. has potential failure in arctic-like conditions with extremely low
!.. tropopause height, as would any other diagnostic, so ensure resulting
!.. k_tropo level is above 700hPa.

      DO k = kte-3, kts, -1
         theta1 = theta(k)
         theta2 = theta(k+2)
         delz = dz1d(k) + dz1d(k+1) + dz1d(k+2)
         if ( ((((theta2-theta1)/delz) .lt. 10./1500. ) .AND.       &
     &                 (P1d(k).gt.8500.)) .or. (P1d(k).gt.70000.) ) then
            goto 86
         endif
      ENDDO
 86   continue
      k_tropo = MAX(kts+2, MIN(k+2, kte-1))

      if (k_tropo.gt.kte-2) then
        WRITE (dbg_msg,*) 'DEBUG-GT: CAUTION, tropopause appears to be very high up: ', k_tropo
        WRITE_MESSAGE(dbg_msg)
        do k = kte, kts, -1
           WRITE (dbg_msg,*) 'DEBUG-GT,   P, T : ', k,P1d(k)*0.01,T1d(k)-273.16
           WRITE_MESSAGE(dbg_msg)
        enddo
      elseif (debugfl) then
        WRITE (dbg_msg,*) 'DEBUG-GT: FOUND TROPOPAUSE k=', k_tropo
        WRITE_MESSAGE(dbg_msg)
      endif

!..Eliminate possible fractional clouds above supposed tropopause.
      DO k = k_tropo+1, kte
         if (cfr1d(k).gt.0.0 .and. cfr1d(k).lt.1.0) then
            cfr1d(k) = 0.
         endif
      ENDDO

!..We would like to prevent fractional clouds below LCL in idealized
!.. situation with deep well-mixed convective PBL, that otherwise is
!.. likely to get clouds in more realistic capping inversion layer.

      kbot = kts+2
      DO k = kbot, k_m12C
         if ( (theta(k)-theta(k-1)) .gt. 0.025E-3*Dz1d(k)) EXIT
      ENDDO
      kbot = MAX(kts+1, k-2)
      DO k = kts, kbot
         if (cfr1d(k).gt.0.0 .and. cfr1d(k).lt.1.0) cfr1d(k) = 0.
      ENDDO


!..Starting below tropo height, if cloud fraction greater than 1 percent,
!.. compute an approximate total layer depth of cloud, determine a total 
!.. liquid water/ice path (LWP/IWP), then reduce that amount with tuning 
!.. parameter to represent entrainment factor, then divide up LWP/IWP
!.. into delta-Z weighted amounts for individual levels per cloud layer. 

      k_cldb = k_tropo
      in_cloud = .false.
      k = k_tropo
      DO WHILE (.not. in_cloud .AND. k.gt.k_m12C+1)
         k_cldt = 0
         if (cfr1d(k).ge.0.01) then
            in_cloud = .true.
            k_cldt = MAX(k_cldt, k)
         endif
         if (in_cloud) then
            DO k2 = k_cldt-1, k_m12C, -1
               if (cfr1d(k2).lt.0.01 .or. k2.eq.k_m12C) then
                  k_cldb = k2+1
                  goto 87
               endif
            ENDDO
 87         continue
            in_cloud = .false.
         endif
         if ((k_cldt - k_cldb + 1) .ge. 2) then
      if (debugfl) then
        WRITE (dbg_msg,*) 'DEBUG-GT: An ice cloud layer is found between ', k_cldt, k_cldb, P1d(k_cldt)*0.01, P1d(k_cldb)*0.01
        WRITE_MESSAGE(dbg_msg)
      endif
            call adjust_cloudIce(cfr1d, qi1d, qs1d, qvs1d, T1d, Dz1d,   &
     &                           entrmnt, k_cldb,k_cldt,kts,kte)
            k = k_cldb
         elseif ((k_cldt - k_cldb + 1) .eq. 1) then
      if (debugfl) then
        WRITE (dbg_msg,*) 'DEBUG-GT: A single-layer ice cloud layer is found on ', k_cldb, P1d(k_cldb)*0.01
        WRITE_MESSAGE(dbg_msg)
      endif
            if (cfr1d(k_cldb).gt.0.and.cfr1d(k_cldb).lt.1.)             &
     &               qi1d(k_cldb)=0.05*qvs1d(k_cldb)
            k = k_cldb
         endif
         k = k - 1
      ENDDO


      k_cldb = k_m12C + 3
      in_cloud = .false.
      k = k_m12C + 2
      DO WHILE (.not. in_cloud .AND. k.gt.kbot)
         k_cldt = 0
         if (cfr1d(k).ge.0.01) then
            in_cloud = .true.
            k_cldt = MAX(k_cldt, k)
         endif
         if (in_cloud) then
            DO k2 = k_cldt-1, kbot, -1
               if (cfr1d(k2).lt.0.01 .or. k2.eq.kbot) then
                  k_cldb = k2+1
                  goto 88
               endif
            ENDDO
 88         continue
            in_cloud = .false.
         endif
         if ((k_cldt - k_cldb + 1) .ge. 2) then
      if (debugfl) then
        WRITE (dbg_msg,*) 'DEBUG-GT: A water cloud layer is found between ', k_cldt, k_cldb, P1d(k_cldt)*0.01, P1d(k_cldb)*0.01
        WRITE_MESSAGE(dbg_msg)
      endif
            call adjust_cloudH2O(cfr1d, qc1d, qvs1d, T1d, Dz1d,         &
     &                           entrmnt, k_cldb,k_cldt,kts,kte)
            k = k_cldb
         elseif ((k_cldt - k_cldb + 1) .eq. 1) then
            if (cfr1d(k_cldb).gt.0.and.cfr1d(k_cldb).lt.1.)             &
     &               qc1d(k_cldb)=0.05*qvs1d(k_cldb)
            k = k_cldb
         endif
         k = k - 1
      ENDDO


      END SUBROUTINE find_cloudLayers

!+---+-----------------------------------------------------------------+

      SUBROUTINE adjust_cloudIce(cfr,qi,qs,qvs,T,dz,entr, k1,k2,kts,kte)
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN):: k1,k2, kts,kte
      REAL, INTENT(IN):: entr
      REAL, DIMENSION(kts:kte), INTENT(IN):: cfr, qs, qvs, T, dz
      REAL, DIMENSION(kts:kte), INTENT(INOUT):: qi
      REAL:: iwc, max_iwc, tdz, this_iwc, this_dz
      INTEGER:: k

      tdz = 0.
      do k = k1, k2
         tdz = tdz + dz(k)
      enddo
      max_iwc = ABS(qvs(k2)-qvs(k1))
!     print*, ' max_iwc = ', max_iwc, ' over DZ=',tdz

      do k = k1, k2
         max_iwc = MAX(1.E-6, max_iwc - (qi(k)+qs(k)))
      enddo
      max_iwc = MIN(1.E-3, max_iwc)

      this_dz = 0.0
      do k = k1, k2
         if (k.eq.k1) then
            this_dz = this_dz + 0.5*dz(k)
         else
            this_dz = this_dz + dz(k)
         endif
         this_iwc = max_iwc*this_dz/tdz
         iwc = MAX(1.E-6, this_iwc*(1.-entr))
         if (cfr(k).gt.0.0.and.cfr(k).lt.1.0.and.T(k).ge.203.16) then
            qi(k) = qi(k) + cfr(k)*cfr(k)*iwc
         endif
      enddo

      END SUBROUTINE adjust_cloudIce

!+---+-----------------------------------------------------------------+

      SUBROUTINE adjust_cloudH2O(cfr, qc, qvs,T,dz,entr, k1,k2,kts,kte)
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN):: k1,k2, kts,kte
      REAL, INTENT(IN):: entr
      REAL, DIMENSION(kts:kte), INTENT(IN):: cfr, qvs, T, dz
      REAL, DIMENSION(kts:kte), INTENT(INOUT):: qc
      REAL:: lwc, max_lwc, tdz, this_lwc, this_dz
      INTEGER:: k

      tdz = 0.
      do k = k1, k2
         tdz = tdz + dz(k)
      enddo
      max_lwc = ABS(qvs(k2)-qvs(k1))
!     print*, ' max_lwc = ', max_lwc, ' over DZ=',tdz

      do k = k1, k2
         max_lwc = MAX(1.E-6, max_lwc - qc(k))
      enddo
      max_lwc = MIN(1.E-3, max_lwc)

      this_dz = 0.0
      do k = k1, k2
         if (k.eq.k1) then
            this_dz = this_dz + 0.5*dz(k)
         else
            this_dz = this_dz + dz(k)
         endif
         this_lwc = max_lwc*this_dz/tdz
         lwc = MAX(1.E-6, this_lwc*(1.-entr))
         if (cfr(k).gt.0.0.and.cfr(k).lt.1.0.and.T(k).ge.253.16) then
            qc(k) = qc(k) + cfr(k)*cfr(k)*lwc
         endif
      enddo

      END SUBROUTINE adjust_cloudH2O

!+---+-----------------------------------------------------------------+

!..Do not alter any grid-explicitly resolved hydrometeors, rather only
!.. the supposed amounts due to the cloud fraction scheme.

      SUBROUTINE adjust_cloudFinal(cfr, qc, qi, Rho,dz, kts,kte)
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN):: kts,kte
      REAL, DIMENSION(kts:kte), INTENT(IN):: cfr, Rho, dz
      REAL, DIMENSION(kts:kte), INTENT(INOUT):: qc, qi
      REAL:: lwp, iwp, xfac
      INTEGER:: k

      lwp = 0.
      iwp = 0.
      do k = kts, kte
         if (cfr(k).gt.0.0) then
            lwp = lwp + qc(k)*Rho(k)*dz(k)
            iwp = iwp + qi(k)*Rho(k)*dz(k)
         endif
      enddo

      if (lwp .gt. 1.5) then
         xfac = 1.5/lwp
         do k = kts, kte
            if (cfr(k).gt.0.0 .and. cfr(k).lt.1.0) then
               qc(k) = qc(k)*xfac
            endif
         enddo
      endif

      if (iwp .gt. 1.5) then
         xfac = 1.5/iwp
         do k = kts, kte
            if (cfr(k).gt.0.0 .and. cfr(k).lt.1.0) then
               qi(k) = qi(k)*xfac
            endif
         enddo
      endif

      END SUBROUTINE adjust_cloudFinal

!+---+-----------------------------------------------------------------+


!=================================================================================================================
  end module module_mp_thompson_cldfra3
!=================================================================================================================

