MODULE MOD_FrictionVelocity

!-----------------------------------------------------------------------
   USE MOD_Precision
   IMPLICIT NONE
   SAVE

! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: moninobuk
   PUBLIC :: moninobukm
   PUBLIC :: moninobukini

! PRIVATE MEMBER FUNCTIONS:
   PRIVATE :: psi


!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------

   SUBROUTINE moninobuk(hu,ht,hq,displa,z0m,z0h,z0q,obu,um,&
                        ustar,fh2m,fq2m,fm10m,fm,fh,fq)

!-----------------------------------------------------------------------
!  Original author: Yongjiu Dai, September 15, 1999
!
!  calculation of friction velocity, relation for potential temperature
!  and humidity profiles of surface boundary layer.
!  the scheme is based on the work of Zeng et al. (1998):
!  Intercomparison of bulk aerodynamic algorithms for the computation of
!  sea surface fluxes using TOGA CORE and TAO data.  J. Climate, Vol.
!  11: 2628-2644
!-----------------------------------------------------------------------

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

   real(r8), intent(out) :: ustar   ! friction velocity [m/s]
   real(r8), intent(out) :: fh2m    ! relation for temperature at 2m
   real(r8), intent(out) :: fq2m    ! relation for specific humidity at 2m
   real(r8), intent(out) :: fm10m   ! integral of profile FUNCTION for momentum at 10m
   real(r8), intent(out) :: fm      ! integral of profile FUNCTION for momentum
   real(r8), intent(out) :: fh      ! integral of profile FUNCTION for heat
   real(r8), intent(out) :: fq      ! integral of profile FUNCTION for moisture

!-------------------------- Local Variables ----------------------------

   real(r8) zldis  ! reference height "minus" zero displacement height [m]
   real(r8) zetam  ! transition point of flux-gradient relation (wind profile)
   real(r8) zetat  ! transition point of flux-gradient relation (temp. profile)
   real(r8) zeta   ! dimensionless height used in Monin-Obukhov theory

! real(r8), external :: psi    ! stability FUNCTION for unstable case
!-----------------------------------------------------------------------
! adjustment factors for unstable (moz < 0) or stable (moz > 0) conditions.

! wind profile
      zldis=hu-displa
      zeta=zldis/obu
      zetam=1.574
      IF(zeta < -zetam)THEN           ! zeta < -1
         fm    = log(-zetam*obu/z0m) - psi(1,-zetam) &
               + psi(1,z0m/obu) + 1.14*((-zeta)**0.333-(zetam)**0.333)
         ustar = vonkar*um / fm
      ELSEIF (zeta < 0.)THEN          ! -1 <= zeta < 0
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
      zetam=1.574
      IF(zeta < -zetam)THEN           ! zeta < -1
         fm10m  = log(-zetam*obu/z0m) - psi(1,-zetam) &
                + psi(1,z0m/obu) + 1.14*((-zeta)**0.333-(zetam)**0.333)
      ELSEIF (zeta < 0.)THEN          ! -1 <= zeta < 0
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

   END SUBROUTINE moninobuk


   SUBROUTINE moninobukm(hu,ht,hq,displa,z0m,z0h,z0q,obu,um,displat,z0mt,&
                         ustar,fh2m,fq2m,htop,fmtop,fm,fh,fq,fht,fqt,phih)

!-----------------------------------------------------------------------
!
! !DESCRIPTION:
!
!  Original author: Yongjiu Dai, September 15, 1999
!
!  calculation of friction velocity, relation for potential temperature
!  and humidity profiles of surface boundary layer.  the scheme is based
!  on the work of Zeng et al. (1998): Intercomparison of bulk aerodynamic
!  algorithms for the computation of sea surface fluxes using TOGA CORE
!  and TAO data. J. Climate, Vol. 11: 2628-2644
!
! !REVISIONS:
!  09/2017, Hua Yuan: adapted from moninobuk FUNCTION to calculate canopy
!           top fm, fq and phih for roughness sublayer u/k profile
!           calculation.
!
!-----------------------------------------------------------------------
   USE MOD_Precision
   USE MOD_Const_Physical, only: vonkar
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------

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

!-------------------------- Local Variables ----------------------------

   real(r8) zldis  ! reference height "minus" zero displacement height [m]
   real(r8) zetam  ! transition point of flux-gradient relation (wind profile)
   real(r8) zetat  ! transition point of flux-gradient relation (temp. profile)
   real(r8) zeta   ! dimensionless height used in Monin-Obukhov theory

! real(r8), external :: psi    ! stability FUNCTION for unstable case
!-----------------------------------------------------------------------
! adjustment factors for unstable (moz < 0) or stable (moz > 0) conditions.

! wind profile
      zldis=hu-displa
      zeta=zldis/obu
      zetam=1.574
      IF(zeta < -zetam)THEN           ! zeta < -1
         fm    = log(-zetam*obu/z0m) - psi(1,-zetam) &
               + psi(1,z0m/obu) + 1.14*((-zeta)**0.333-(zetam)**0.333)
         ustar = vonkar*um / fm
      ELSEIF (zeta < 0.)THEN          ! -1 <= zeta < 0
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
      zetam=1.574
      IF(zeta < -zetam)THEN           ! zeta < -1
         fmtop  = log(-zetam*obu/z0m) - psi(1,-zetam) &
                + psi(1,z0m/obu) + 1.14*((-zeta)**0.333-(zetam)**0.333)
      ELSEIF (zeta < 0.)THEN          ! -1 <= zeta < 0
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
      ! CESM TECH NOTE EQ. (5.31)
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

   END SUBROUTINE moninobukm

   real(r8) FUNCTION kmoninobuk(displa,obu,ustar,z)
!-----------------------------------------------------------------------
! !DESCRIPTION:
!  k profile calculation for bare ground case
!
!  Created by Hua Yuan, 09/2017
!
!-----------------------------------------------------------------------
   USE MOD_Precision
   USE MOD_Const_Physical, only: vonkar
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------

   real(r8), intent(in) :: displa   ! displacement height [m]
   real(r8), intent(in) :: obu      ! monin-obukhov length (m)
   real(r8), intent(in) :: ustar    ! friction velocity [m/s]
   real(r8), intent(in) :: z        ! height of windspeed [m]

!-------------------------- Local Variables ----------------------------

   real(r8) zldis  ! reference height "minus" zero displacement height [m]
   real(r8) zetam  ! transition point of flux-gradient relation (wind profile)
   real(r8) zetat  ! transition point of flux-gradient relation (temp. profile)
   real(r8) zeta   ! dimensionless height used in Monin-Obukhov theory
   real(r8) phih   ! phi(h), similarity FUNCTION for sensible heat

!-----------------------------------------------------------------------

      IF ( z .le. displa ) THEN
         kmoninobuk = 0.
         RETURN
      ENDIF

      ! for canopy top phi(h)
      zldis=z-displa  ! ht-displa
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

      kmoninobuk = vonkar*(z-displa)*ustar/phih

   END FUNCTION kmoninobuk

   real(r8) FUNCTION kintmoninobuk(displa,z0h,obu,ustar,ztop,zbot)
!-----------------------------------------------------------------------
! !DESCRIPTION:
!  k profile integration for bare ground case
!
!  Created by Hua Yuan, 09/2017
!
!-----------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_Const_Physical, only: vonkar
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------

   real(r8), intent(in) :: displa   ! displacement height [m]
   real(r8), intent(in) :: z0h      ! roughness length, sensible heat [m]
   real(r8), intent(in) :: obu      ! monin-obukhov length (m)
   real(r8), intent(in) :: ustar    ! friction velocity [m/s]
   real(r8), intent(in) :: ztop     ! height top
   real(r8), intent(in) :: zbot     ! height bottom

!-------------------------- Local Variables ----------------------------

   real(r8) zldis  ! reference height "minus" zero displacement height [m]
   real(r8) zetam  ! transition point of flux-gradient relation (wind profile)
   real(r8) zetat  ! transition point of flux-gradient relation (temp. profile)
   real(r8) zeta   ! dimensionless height used in Monin-Obukhov theory

   real(r8) :: fh_top, fh_bot         ! integral of profile FUNCTION for heat

!-----------------------------------------------------------------------

      zldis=ztop-displa
      zeta=zldis/obu
      zetat=0.465
      IF(zeta < -zetat)THEN           ! zeta < -1
         fh_top = log(-zetat*obu/z0h)-psi(2,-zetat) &
                + psi(2,z0h/obu) + 0.8*((zetat)**(-0.333)-(-zeta)**(-0.333))
      ELSEIF (zeta < 0.)THEN          ! -1 <= zeta < 0
         fh_top = log(zldis/z0h) - psi(2,zeta) + psi(2,z0h/obu)
      ELSEIF (zeta <= 1.)THEN         !  0 <= zeta <= 1
         fh_top = log(zldis/z0h) + 5.*zeta - 5.*z0h/obu
      ELSE                            !  1 < zeta, phi=5+zeta
         fh_top = log(obu/z0h) + 5. - 5.*z0h/obu + (5.*log(zeta)+zeta-1.)
      ENDIF

      zldis=zbot-displa
      zeta=zldis/obu
      zetat=0.465
      IF(zeta < -zetat)THEN           ! zeta < -1
         fh_bot = log(-zetat*obu/z0h)-psi(2,-zetat) &
                + psi(2,z0h/obu) + 0.8*((zetat)**(-0.333)-(-zeta)**(-0.333))
      ELSEIF (zeta < 0.)THEN          ! -1 <= zeta < 0
         fh_bot = log(zldis/z0h) - psi(2,zeta) + psi(2,z0h/obu)
      ELSEIF (zeta <= 1.)THEN         !  0 <= zeta <= 1
         fh_bot = log(zldis/z0h) + 5.*zeta - 5.*z0h/obu
      ELSE                            !  1 < zeta, phi=5+zeta
         fh_bot = log(obu/z0h) + 5. - 5.*z0h/obu + (5.*log(zeta)+zeta-1.)
      ENDIF

      kintmoninobuk = (fh_top-fh_bot)/(vonkar*ustar)

   END FUNCTION kintmoninobuk


   SUBROUTINE moninobukini(ur,th,thm,thv,dth,dqh,dthv,zldis,z0m,um,obu)

! ======================================================================
! Original author: Yongjiu Dai, September 15, 1999
!
! initialization of Monin-Obukhov length,
! the scheme is based on the work of Zeng et al. (1998):
! Intercomparison of bulk aerodynamic algorithms for the computation
! of sea surface fluxes using TOGA CORE and TAO data. J. Climate, Vol.
! 11: 2628-2644
! ======================================================================

   USE MOD_Precision
   USE MOD_Const_Physical, only: grav, vonkar
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   real(r8), intent(in) :: ur    ! wind speed at reference height [m/s]
   real(r8), intent(in) :: thm   ! intermediate variable (tm+0.0098*ht)
   real(r8), intent(in) :: th    ! potential temperature [kelvin]
   real(r8), intent(in) :: thv   ! virtual potential temperature (kelvin)
   real(r8), intent(in) :: dth   ! diff of virtual temp. between ref. height and surface
   real(r8), intent(in) :: dthv  ! diff of vir. poten. temp. between ref. height and surface
   real(r8), intent(in) :: dqh   ! diff of humidity between ref. height and surface
   real(r8), intent(in) :: zldis ! reference height "minus" zero displacement height [m]
   real(r8), intent(in) :: z0m   ! roughness length, momentum [m]

   real(r8), intent(out) :: um   ! wind speed including the stability effect [m/s]
   real(r8), intent(out) :: obu  ! monin-obukhov length (m)

!-------------------------- Local Variables ----------------------------
   real(r8) wc     ! convective velocity [m/s]
   real(r8) rib    ! bulk Richardson number
   real(r8) zeta   ! dimensionless height used in Monin-Obukhov theory

!-----------------------------------------------------------------------
! Initial values of u* and convective velocity

      wc=0.5
      IF(dthv >= 0.)THEN
        um=max(ur,0.1)
      ELSE
        um=sqrt(ur*ur+wc*wc)
      ENDIF

      rib=grav*zldis*dthv/(thv*um*um)

      IF(rib >= 0.)THEN      ! neutral or stable
        zeta = rib*log(zldis/z0m)/(1.-5.*min(rib,0.19))
        zeta = min(2.,max(zeta,1.e-6))
      ELSE                   ! unstable
        zeta = rib*log(zldis/z0m)
        zeta = max(-100.,min(zeta,-1.e-6))
      ENDIF
      obu=zldis/zeta

   END SUBROUTINE moninobukini


   real(r8) FUNCTION psi(k,zeta)

!=======================================================================
! stability FUNCTION for unstable case (rib < 0)

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

END MODULE MOD_FrictionVelocity
! --------- EOP ------------
