MODULE MOD_TurbulenceLEddy

!-----------------------------------------------------------------------
   USE MOD_Precision
   IMPLICIT NONE
   SAVE

! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: moninobuk_leddy
   PUBLIC :: moninobukm_leddy


! PRIVATE MEMBER FUNCTIONS:
   PRIVATE :: psi


!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------

   SUBROUTINE moninobuk_leddy(hu,ht,hq,displa,z0m,z0h,z0q,obu,um, hpbl, &
                              ustar,fh2m,fq2m,fm10m,fm,fh,fq)

!=======================================================================
!
!  Implement the LZD2022 scheme (Liu et al., 2022), which accounts for
!  large eddy effects by including the boundary layer height in the phim
!  FUNCTION, to compute friction velocity, relation for potential
!  temperature and humidity profiles of surface boundary layer.
!
! !REFERENCES:
!  [1] Zeng et al., 1998: Intercomparison of bulk aerodynamic algorithms
!      for the computation of sea surface fluxes using TOGA CORE and TAO
!      data.  J. Climate, 11: 2628-2644.
!  [2] Liu et al., 2022: A surface flux estimation scheme accounting for
!      large-eddy effects for land surface modeling. GRL, 49,
!      e2022GL101754.
!
!  Created by Shaofeng Liu, May 5, 2023
!
!=======================================================================

   USE MOD_Precision
   USE MOD_Const_Physical, only: vonkar
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------

   real(r8), intent(in) :: hu       ! observational height of wind [m]
   real(r8), intent(in) :: ht       ! observational height of temperature [m]
   real(r8), intent(in) :: hq       ! observational height of humidity [m]
   real(r8), intent(in) :: displa   ! displacement height [m]
   real(r8), intent(in) :: z0m      ! roughness length, momentum [m]
   real(r8), intent(in) :: z0h      ! roughness length, sensible heat [m]
   real(r8), intent(in) :: z0q      ! roughness length, latent heat [m]
   real(r8), intent(in) :: obu      ! monin-obukhov length (m)
   real(r8), intent(in) :: um       ! wind speed including the stability effect [m/s]
   real(r8), intent(in) :: hpbl     ! atmospheric boundary layer height [m]

   real(r8), intent(out) :: ustar   ! friction velocity [m/s]
   real(r8), intent(out) :: fh2m    ! relation for temperature at 2m
   real(r8), intent(out) :: fq2m    ! relation for specific humidity at 2m
   real(r8), intent(out) :: fm10m   ! integral of profile FUNCTION for momentum at 10m
   real(r8), intent(out) :: fm      ! integral of profile FUNCTION for momentum
   real(r8), intent(out) :: fh      ! integral of profile FUNCTION for heat
   real(r8), intent(out) :: fq      ! integral of profile FUNCTION for moisture

!-------------------------- Local Variables ----------------------------

   real(r8) zldis  ! reference height "minus" zero displacement height [m]
   real(r8) zetam, &
            zetam2 ! transition point of flux-gradient relation (wind profile)
   real(r8) zetat  ! transition point of flux-gradient relation (temp. profile)
   real(r8) zeta   ! dimensionless height used in Monin-Obukhov theory
   real(r8) zetazi ! hpbl/obu, dimensionless height used in the LZD2022 scheme
   real(r8) Bm     ! Coefficient of the LZD2022 scheme: Bm = 0.0047*(-hpbl/L) + 0.1854
   real(r8) Bm2    ! max(Bm, 0.2722)

! real(r8), external :: psi    ! stability FUNCTION for unstable CASE
!-----------------------------------------------------------------------
! adjustment factors for unstable (moz < 0) or stable (moz > 0) conditions.

! wind profile
      zldis=hu-displa
      zeta=zldis/obu
!
! Begin: Shaofeng Liu, 2023.05.05
!
      zetazi = max(5.*hu, hpbl)/obu
      IF(zetazi >= 0.) THEN     !stable
         zetazi = min(200.,max(zetazi,1.e-5))
      ELSE                    !unstable
         zetazi = max(-1.e4,min(zetazi,-1.e-5))
      ENDIF

      Bm     = 0.0047 * (-zetazi) + 0.1854
      zetam  = 0.5*Bm**4 * ( -16. - sqrt(256. + 4./Bm**4) )
      Bm2    = max(Bm, 0.2722)
      zetam2 = min(zetam, -0.13)

      IF(zeta < zetam2)THEN           ! zeta < zetam2
         fm    = log(zetam2*obu/z0m) - psi(1,zetam2) &
               + psi(1,z0m/obu) - 2.*Bm2 * ( (-zeta)**(-0.5)-(-zetam2)**(-0.5) )
         ustar = vonkar*um / fm
!
! End: Shaofeng Liu, 2023.05.05
!
      ELSEIF (zeta < 0.)THEN          ! zetam2 <= zeta < 0
         fm    = log(zldis/z0m) - psi(1,zeta) + psi(1,z0m/obu)
         ustar = vonkar*um / fm
      ELSEIF (zeta <= 1.)THEN         !  0 <= zeta <= 1
         fm    = log(zldis/z0m) + 5.*zeta - 5.*z0m/obu
         ustar = vonkar*um / fm
      ELSE                            !  1 < zeta, phi=5+zeta
         fm    = log(obu/z0m) + 5. - 5.*z0m/obu + (5.*log(zeta)+zeta-1.)
         ustar = vonkar*um / fm
      ENDIF

! for 10 meter wind-velocity
      zldis=10.+z0m
      zeta=zldis/obu
!
! Begin: Shaofeng Liu, 2023.05.18
!
      IF(zeta < zetam2)THEN           ! zeta < zetam2
         fm10m  = log(zetam2*obu/z0m) - psi(1,zetam2) &
                + psi(1,z0m/obu) - 2.*Bm2 * ( (-zeta)**(-0.5)-(-zetam2)**(-0.5) )
!
! End: Shaofeng Liu, 2023.05.18
!
      ELSEIF (zeta < 0.)THEN          ! zetam2 <= zeta < 0
         fm10m  = log(zldis/z0m) - psi(1,zeta) + psi(1,z0m/obu)
      ELSEIF (zeta <= 1.)THEN         !  0 <= zeta <= 1
         fm10m  = log(zldis/z0m) + 5.*zeta - 5.*z0m/obu
      ELSE                            !  1 < zeta, phi=5+zeta
         fm10m  = log(obu/z0m) + 5. - 5.*z0m/obu + (5.*log(zeta)+zeta-1.)
      ENDIF

! temperature profile
      zldis=ht-displa
      zeta=zldis/obu
      zetat=0.465
      IF(zeta < -zetat)THEN           ! zeta < -1
         fh = log(-zetat*obu/z0h)-psi(2,-zetat) &
            + psi(2,z0h/obu) + 0.8*((zetat)**(-0.333)-(-zeta)**(-0.333))
      ELSEIF (zeta < 0.)THEN          ! -1 <= zeta < 0
         fh = log(zldis/z0h) - psi(2,zeta) + psi(2,z0h/obu)
      ELSEIF (zeta <= 1.)THEN         !  0 <= zeta <= 1
         fh = log(zldis/z0h) + 5.*zeta - 5.*z0h/obu
      ELSE                            !  1 < zeta, phi=5+zeta
         fh = log(obu/z0h) + 5. - 5.*z0h/obu + (5.*log(zeta)+zeta-1.)
      ENDIF

! for 2 meter screen temperature
      zldis=2.+z0h  ! ht-displa
      zeta=zldis/obu
      zetat=0.465
      IF(zeta < -zetat)THEN           ! zeta < -1
         fh2m = log(-zetat*obu/z0h)-psi(2,-zetat) &
              + psi(2,z0h/obu) + 0.8*((zetat)**(-0.333)-(-zeta)**(-0.333))
      ELSEIF (zeta < 0.)THEN          ! -1 <= zeta < 0
         fh2m = log(zldis/z0h) - psi(2,zeta) + psi(2,z0h/obu)
      ELSEIF (zeta <= 1.)THEN         !  0 <= zeta <= 1
         fh2m = log(zldis/z0h) + 5.*zeta - 5.*z0h/obu
      ELSE                            !  1 < zeta, phi=5+zeta
         fh2m = log(obu/z0h) + 5. - 5.*z0h/obu + (5.*log(zeta)+zeta-1.)
      ENDIF

! humidity profile
      zldis=hq-displa
      zeta=zldis/obu
      zetat=0.465
      IF(zeta < -zetat)THEN           ! zeta < -1
         fq  = log(-zetat*obu/z0q) - psi(2,-zetat) &
             + psi(2,z0q/obu) + 0.8*((zetat)**(-0.333)-(-zeta)**(-0.333))
      ELSEIF (zeta < 0.)THEN          ! -1 <= zeta < 0
         fq  = log(zldis/z0q) - psi(2,zeta) + psi(2,z0q/obu)
      ELSEIF (zeta <= 1.)THEN         !  0 <= zeta <= 1
         fq  = log(zldis/z0q) + 5.*zeta - 5.*z0q/obu
      ELSE                            !  1 < zeta, phi=5+zeta
         fq  = log(obu/z0q) + 5. - 5.*z0q/obu + (5.*log(zeta)+zeta-1.)
      ENDIF

! for 2 meter screen humidity
      zldis=2.+z0q
      zeta=zldis/obu
      zetat=0.465
      IF(zeta < -zetat)THEN           ! zeta < -1
         fq2m = log(-zetat*obu/z0q)-psi(2,-zetat) &
              + psi(2,z0q/obu) + 0.8*((zetat)**(-0.333)-(-zeta)**(-0.333))
      ELSEIF (zeta < 0.) THEN         ! -1 <= zeta < 0
         fq2m = log(zldis/z0q)-psi(2,zeta)+psi(2,z0q/obu)
      ELSEIF (zeta <= 1.) THEN        !  0 <= zeta <= 1
         fq2m = log(zldis/z0q)+5.*zeta-5.*z0q/obu
      ELSE                            ! 1 < zeta, phi=5+zeta
         fq2m = log(obu/z0q)+5.-5.*z0q/obu+(5.*log(zeta)+zeta-1.)
      ENDIF

   END SUBROUTINE moninobuk_leddy


   SUBROUTINE moninobukm_leddy(hu,ht,hq,displa,z0m,z0h,z0q,obu,um,displat,z0mt, hpbl, &
                               ustar,fh2m,fq2m,htop,fmtop,fm,fh,fq,fht,fqt,phih)

!=======================================================================
!
! !DESCRIPTION:
!
!
!  Original author: Yongjiu Dai, September 15, 1999
!
!  calculation of friction velocity, relation for potential temperature and
!  humidity profiles of surface boundary layer.  the scheme is based on the work
!  of Zeng et al. (1998): Intercomparison of bulk aerodynamic algorithms for the
!  computation of sea surface fluxes using TOGA CORE and TAO data. J. Climate,
!  Vol. 11: 2628-2644
!
! !REVISIONS:
!  Hua Yuan, 09/2017: adapted from moninobuk FUNCTION to calculate canopy top
!                     fm, fq and phih for roughness sublayer u/k profile calculation
!  Shaofeng Liu, 05/2023: implement the LZD2022 scheme (Liu et al., 2022), which
!                         accounts for large eddy effects by including the
!                         boundary leyer height in the phim FUNCTION.
!=======================================================================

   USE MOD_Precision
   USE MOD_Const_Physical, only: vonkar
   IMPLICIT NONE

! ---------------------- dummy argument --------------------------------

   real(r8), intent(in) :: hu       ! observational height of wind [m]
   real(r8), intent(in) :: ht       ! observational height of temperature [m]
   real(r8), intent(in) :: hq       ! observational height of humidity [m]
   real(r8), intent(in) :: displa   ! displacement height [m]
   real(r8), intent(in) :: displat  ! displacement height of the top layer [m]
   real(r8), intent(in) :: z0m      ! roughness length, momentum [m]
   real(r8), intent(in) :: z0h      ! roughness length, sensible heat [m]
   real(r8), intent(in) :: z0q      ! roughness length, latent heat [m]
   real(r8), intent(in) :: z0mt     ! roughness length of the top layer, latent heat [m]
   real(r8), intent(in) :: htop     ! canopy top height of the top layer [m]
   real(r8), intent(in) :: obu      ! monin-obukhov length (m)
   real(r8), intent(in) :: um       ! wind speed including the stability effect [m/s]
   real(r8), intent(in) :: hpbl     ! atmospheric boundary layer height [m]

   real(r8), intent(out) :: ustar   ! friction velocity [m/s]
   real(r8), intent(out) :: fh2m    ! relation for temperature at 2m
   real(r8), intent(out) :: fq2m    ! relation for specific humidity at 2m
   real(r8), intent(out) :: fmtop   ! integral of profile FUNCTION for momentum at 10m
   real(r8), intent(out) :: fm      ! integral of profile FUNCTION for momentum
   real(r8), intent(out) :: fh      ! integral of profile FUNCTION for heat
   real(r8), intent(out) :: fq      ! integral of profile FUNCTION for moisture
   real(r8), intent(out) :: fht     ! integral of profile FUNCTION for heat at the top layer
   real(r8), intent(out) :: fqt     ! integral of profile FUNCTION for moisture at the top layer
   real(r8), intent(out) :: phih    ! phi(h), similarity FUNCTION for sensible heat

!------------------------ local variables ------------------------------

   real(r8) zldis  ! reference height "minus" zero displacement height [m]
   real(r8) zetam, &
            zetam2 ! transition point of flux-gradient relation (wind profile)
   real(r8) zetat  ! transition point of flux-gradient relation (temp. profile)
   real(r8) zeta   ! dimensionless height used in Monin-Obukhov theory
   real(r8) zetazi ! hpbl/obu, dimensionless height used in the LZD2022 scheme
   real(r8) Bm     ! Coefficient of the LZD2022 scheme: Bm = 0.0047*(-hpbl/L) + 0.1854
   real(r8) Bm2    ! max(Bm, 0.2722)

! real(r8), external :: psi    ! stability FUNCTION for unstable CASE
!-----------------------------------------------------------------------
! adjustment factors for unstable (moz < 0) or stable (moz > 0) conditions.

! wind profile
      zldis=hu-displa
      zeta=zldis/obu
!
! Begin: Shaofeng Liu, 2023.05.05
!
!        zetazi = hpbl/obu
      zetazi = max(5.*hu, hpbl)/obu
      IF(zetazi >= 0.) THEN     !stable
         zetazi = min(200.,max(zetazi,1.e-5))
      ELSE                    !unstable
         zetazi = max(-1.e4,min(zetazi,-1.e-5))
      ENDIF

      Bm     = 0.0047 * (-zetazi) + 0.1854
      zetam  = 0.5*Bm**4 * ( -16. - sqrt(256. + 4./Bm**4) )
      Bm2    = max(Bm, 0.2722)
      zetam2 = min(zetam, -0.13)

      IF(zeta < zetam2)THEN           ! zeta < zetam2
      fm    = log(zetam2*obu/z0m) - psi(1,zetam2) &
            + psi(1,z0m/obu) - 2.*Bm2 * ( (-zeta)**(-0.5)-(-zetam2)**(-0.5) )
      ustar = vonkar*um / fm
!
! End: Shaofeng Liu, 2023.05.05
!
      ELSEIF (zeta < 0.)THEN          ! zetam2 <= zeta < 0
         fm    = log(zldis/z0m) - psi(1,zeta) + psi(1,z0m/obu)
         ustar = vonkar*um / fm
      ELSEIF (zeta <= 1.)THEN         !  0 <= zeta <= 1
         fm    = log(zldis/z0m) + 5.*zeta - 5.*z0m/obu
         ustar = vonkar*um / fm
      ELSE                            !  1 < zeta, phi=5+zeta
         fm    = log(obu/z0m) + 5. - 5.*z0m/obu + (5.*log(zeta)+zeta-1.)
         ustar = vonkar*um / fm
      ENDIF

! for canopy top wind-velocity
!NOTE: changed for canopy top wind-velocity (no wake assumed)
      zldis=htop-displa
      zeta=zldis/obu
!
! Begin: Shaofeng Liu, 2023.05.18
!
!        zetam=1.574
      IF(zeta < zetam2)THEN           ! zeta < zetam2
         fmtop  = log(zetam2*obu/z0m) - psi(1,zetam2) &
                + psi(1,z0m/obu) - 2.*Bm2 * ( (-zeta)**(-0.5)-(-zetam2)**(-0.5) )
!
! End: Shaofeng Liu, 2023.05.18
!
      ELSEIF (zeta < 0.)THEN          ! zetam2 <= zeta < 0
         fmtop  = log(zldis/z0m) - psi(1,zeta) + psi(1,z0m/obu)
      ELSEIF (zeta <= 1.)THEN         !  0 <= zeta <= 1
         fmtop  = log(zldis/z0m) + 5.*zeta - 5.*z0m/obu
      ELSE                            !  1 < zeta, phi=5+zeta
         fmtop  = log(obu/z0m) + 5. - 5.*z0m/obu + (5.*log(zeta)+zeta-1.)
      ENDIF

! temperature profile
      zldis=ht-displa
      zeta=zldis/obu
      zetat=0.465
      IF(zeta < -zetat)THEN           ! zeta < -1
         fh    = log(-zetat*obu/z0h)-psi(2,-zetat) &
               + psi(2,z0h/obu) + 0.8*((zetat)**(-0.333)-(-zeta)**(-0.333))
      ELSEIF (zeta < 0.)THEN          ! -1 <= zeta < 0
         fh    = log(zldis/z0h) - psi(2,zeta) + psi(2,z0h/obu)
      ELSEIF (zeta <= 1.)THEN         !  0 <= zeta <= 1
         fh    = log(zldis/z0h) + 5.*zeta - 5.*z0h/obu
      ELSE                            !  1 < zeta, phi=5+zeta
         fh    = log(obu/z0h) + 5. - 5.*z0h/obu + (5.*log(zeta)+zeta-1.)
      ENDIF

! for 2 meter screen temperature
      zldis=2.+z0h  ! ht-displa
      zeta=zldis/obu
      zetat=0.465
      IF(zeta < -zetat)THEN           ! zeta < -1
         fh2m = log(-zetat*obu/z0h)-psi(2,-zetat) &
              + psi(2,z0h/obu) + 0.8*((zetat)**(-0.333)-(-zeta)**(-0.333))
      ELSEIF (zeta < 0.)THEN          ! -1 <= zeta < 0
         fh2m = log(zldis/z0h) - psi(2,zeta) + psi(2,z0h/obu)
      ELSEIF (zeta <= 1.)THEN         !  0 <= zeta <= 1
         fh2m = log(zldis/z0h) + 5.*zeta - 5.*z0h/obu
      ELSE                            !  1 < zeta, phi=5+zeta
         fh2m = log(obu/z0h) + 5. - 5.*z0h/obu + (5.*log(zeta)+zeta-1.)
      ENDIF

! for top layer temperature
      zldis=displat+z0mt-displa  ! ht-displa
      zeta=zldis/obu
      zetat=0.465
      IF(zeta < -zetat)THEN           ! zeta < -1
         fht = log(-zetat*obu/z0h)-psi(2,-zetat) &
             + psi(2,z0h/obu) + 0.8*((zetat)**(-0.333)-(-zeta)**(-0.333))
      ELSEIF (zeta < 0.)THEN          ! -1 <= zeta < 0
         fht = log(zldis/z0h) - psi(2,zeta) + psi(2,z0h/obu)
      ELSEIF (zeta <= 1.)THEN         !  0 <= zeta <= 1
         fht = log(zldis/z0h) + 5.*zeta - 5.*z0h/obu
      ELSE                            !  1 < zeta, phi=5+zeta
         fht = log(obu/z0h) + 5. - 5.*z0h/obu + (5.*log(zeta)+zeta-1.)
      ENDIF

! for canopy top phi(h)
! CESM TECH NOTE eq. (5.31)
      zldis=htop-displa  ! ht-displa
      zeta=zldis/obu
      zetat=0.465
      IF(zeta < -zetat)THEN           ! zeta < -1
         phih = 0.9*vonkar**(1.333)*(-zeta)**(-0.333)
      ELSEIF (zeta < 0.)THEN          ! -1 <= zeta < 0
         phih = (1. - 16.*zeta)**(-0.5)
      ELSEIF (zeta <= 1.)THEN         !  0 <= zeta <= 1
         phih = 1. + 5.*zeta
      ELSE                            !  1 < zeta, phi=5+zeta
         phih = 5. + zeta
      ENDIF

! humidity profile
      zldis=hq-displa
      zeta=zldis/obu
      zetat=0.465
      IF(zeta < -zetat)THEN           ! zeta < -1
         fq    = log(-zetat*obu/z0q) - psi(2,-zetat) &
               + psi(2,z0q/obu) + 0.8*((zetat)**(-0.333)-(-zeta)**(-0.333))
      ELSEIF (zeta < 0.)THEN          ! -1 <= zeta < 0
         fq    = log(zldis/z0q) - psi(2,zeta) + psi(2,z0q/obu)
      ELSEIF (zeta <= 1.)THEN         !  0 <= zeta <= 1
         fq    = log(zldis/z0q) + 5.*zeta - 5.*z0q/obu
      ELSE                            !  1 < zeta, phi=5+zeta
         fq    = log(obu/z0q) + 5. - 5.*z0q/obu + (5.*log(zeta)+zeta-1.)
      ENDIF

! for 2 meter screen humidity
      zldis=2.+z0q
      zeta=zldis/obu
      zetat=0.465
      IF(zeta < -zetat)THEN           ! zeta < -1
         fq2m = log(-zetat*obu/z0q)-psi(2,-zetat) &
              + psi(2,z0q/obu) + 0.8*((zetat)**(-0.333)-(-zeta)**(-0.333))
      ELSEIF (zeta < 0.) THEN         ! -1 <= zeta < 0
         fq2m = log(zldis/z0q)-psi(2,zeta)+psi(2,z0q/obu)
      ELSEIF (zeta <= 1.) THEN        !  0 <= zeta <= 1
         fq2m = log(zldis/z0q)+5.*zeta-5.*z0q/obu
      ELSE                            ! 1 < zeta, phi=5+zeta
         fq2m = log(obu/z0q)+5.-5.*z0q/obu+(5.*log(zeta)+zeta-1.)
      ENDIF

! for top layer humidity
      zldis=displat+z0mt-displa  ! ht-displa
      zeta=zldis/obu
      zetat=0.465
      IF(zeta < -zetat)THEN           ! zeta < -1
         fqt = log(-zetat*obu/z0q)-psi(2,-zetat) &
             + psi(2,z0q/obu) + 0.8*((zetat)**(-0.333)-(-zeta)**(-0.333))
      ELSEIF (zeta < 0.) THEN         ! -1 <= zeta < 0
         fqt = log(zldis/z0q)-psi(2,zeta)+psi(2,z0q/obu)
      ELSEIF (zeta <= 1.) THEN        !  0 <= zeta <= 1
         fqt = log(zldis/z0q)+5.*zeta-5.*z0q/obu
      ELSE                            ! 1 < zeta, phi=5+zeta
         fqt = log(obu/z0q)+5.-5.*z0q/obu+(5.*log(zeta)+zeta-1.)
      ENDIF

   END SUBROUTINE moninobukm_leddy



   real(r8) FUNCTION psi(k,zeta)

!=======================================================================
! stability FUNCTION for unstable CASE (rib < 0)

   USE MOD_Precision
   IMPLICIT NONE

   integer k
   real(r8) zeta  ! dimensionless height used in Monin-Obukhov theory
   real(r8) chik  !

      chik = (1.-16.*zeta)**0.25
      IF(k == 1)THEN
         psi = 2.*log((1.+chik)*0.5)+log((1.+chik*chik)*0.5)-2.*atan(chik)+2.*atan(1.)
      ELSE
         psi = 2.*log((1.+chik*chik)*0.5)
      ENDIF

   END FUNCTION psi


END MODULE MOD_TurbulenceLEddy
! --------- EOP ------------
