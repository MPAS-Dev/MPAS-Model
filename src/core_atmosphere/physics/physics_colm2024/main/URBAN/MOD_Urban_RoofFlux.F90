#include <define.h>

MODULE MOD_Urban_RoofFlux

   USE MOD_Precision
   IMPLICIT NONE
   SAVE

   PUBLIC :: UrbanRoofFlux

CONTAINS


   SUBROUTINE UrbanRoofFlux (hu, ht, hq, us, vs, tm, qm, rhoair, psrf, &
                             ur, thm, th, thv, zsno, fsno_roof, hroof, htvp_roof, &
                             lbr, wliq_roofsno, wice_roofsno, troof, qroof, dqroofdT, &
                             croofs, croofl, croof, fsenroof, fevproof, &
                             z0m, z0hg, zol, ustar, qstar, tstar, fm, fh, fq)

!-----------------------------------------------------------------------
! !DESCRIPTION:
!  This is the main subroutine to execute the calculation
!  of roof fluxes - not used now.
!
!  Created by Hua Yuan, 11/2022
!-----------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_Const_Physical, only: cpair,vonkar,grav
   USE MOD_FrictionVelocity
   IMPLICIT NONE

!----------------------- Dummy argument --------------------------------
   integer, intent(in) :: &
        lbr        ! lower bound of array

   real(r8), intent(in) :: &
        ! atmospherical variables and observational height
        hu,       &! observational height of wind [m]
        ht,       &! observational height of temperature [m]
        hq,       &! observational height of humidity [m]
        us,       &! wind component in eastward direction [m/s]
        vs,       &! wind component in northward direction [m/s]
        tm,       &! temperature at agcm reference height [kelvin] [not used]
        qm,       &! specific humidity at agcm reference height [kg/kg]
        rhoair,   &! density air [kg/m3]
        psrf,     &! atmosphere pressure at the surface [pa] [not used]

        ur,       &! wind speed at reference height [m/s]
        thm,      &! intermediate variable (tm+0.0098*ht)
        th,       &! potential temperature (kelvin)
        thv,      &! virtual potential temperature (kelvin)

        zsno,     &! roughness length for snow [m]
        fsno_roof,&! fraction of impervious ground covered by snow
        hroof,    &! average building height [m]

        wliq_roofsno,&! liquid water [kg/m2]
        wice_roofsno,&! ice lens [kg/m2]

        troof,    &! ground impervious temperature [K]
        qroof,    &! ground impervious specific humidity [kg/kg]
        dqroofdT, &! d(qroof)/dT
        htvp_roof  ! latent heat of vapor of water (or sublimation) [j/kg]

   real(r8), intent(out) :: &
        croofs,   &! deriv of roof sensible heat flux wrt soil temp [w/m**2/k]
        croofl,   &! deriv of roof latent heat flux wrt soil temp [w/m**2/k]
        croof      ! deriv of roof total heat flux wrt soil temp [w/m**2/k]

   real(r8), intent(out) :: &
        fsenroof, &! sensible heat flux from roof [W/m2]
        fevproof   ! evaporation heat flux from roof [W/m2]

   real(r8), intent(out) :: &
        z0m,      &! effective roughness [m]
        z0hg,     &! roughness length over ground, sensible heat [m]
        zol,      &! dimensionless height (z/L) used in Monin-Obukhov theory
        ustar,    &! friction velocity [m/s]
        tstar,    &! temperature scaling parameter
        qstar,    &! moisture scaling parameter
        fm,       &! integral of profile function for momentum
        fh,       &! integral of profile function for heat
        fq         ! integral of profile function for moisture

!-------------------------- Local Variables ----------------------------
   integer niters,&! maximum number of iterations for surface temperature
        iter,     &! iteration index
        nmozsgn    ! number of times moz changes sign

   real(r8) :: &
        beta,     &! coefficient of convective velocity [-]
        displax,  &! zero-displacement height [m]
        tg,       &! ground surface temperature [K]
        qg,       &! ground specific humidity [kg/kg]
        fg,       &! ground fractional cover [-]
        froof,    &! weight of impervious ground
        dth,      &! diff of virtual temp. between ref. height and surface
        dqh,      &! diff of humidity between ref. height and surface
        dthv,     &! diff of vir. poten. temp. between ref. height and surface
        obu,      &! monin-obukhov length (m)
        obuold,   &! monin-obukhov length from previous iteration
        ram,      &! aerodynamical resistance [s/m]
        rah,      &! thermal resistance [s/m]
        raw,      &! moisture resistance [s/m]
        raih,     &! temporary variable [kg/m2/s]
        raiw,     &! temporary variable [kg/m2/s]
        fh2m,     &! relation for temperature at 2m
        fq2m,     &! relation for specific humidity at 2m
        fm10m,    &! integral of profile function for momentum at 10m
        thvstar,  &! virtual potential temperature scaling parameter
        um,       &! wind speed including the stability effect [m/s]
        wc,       &! convective velocity [m/s]
        wc2,      &! wc**2
        zeta,     &! dimensionless height used in Monin-Obukhov theory
        zii,      &! convective boundary height [m]
        zldis,    &! reference height "minus" zero displacement height [m]
        z0mg,     &! roughness length over ground, momentum [m]
        z0qg       ! roughness length over ground, latent heat [m]

   real(r8) fwet_roof

!-----------------------------------------------------------------------
! initial roughness length
      !TODO: change to original
      !z0mg = (1.-fsno)*zlnd + fsno*zsno
      IF (fsno_roof > 0) THEN
         z0mg = zsno
      ELSE
         z0mg = 0.01
      ENDIF
      z0hg = z0mg
      z0qg = z0mg

! potential temperature at the reference height
      beta = 1.       !-  (in computing W_*)
      zii  = 1000.    !m  (pbl height)
      z0m  = z0mg

      ! wet fraction for roof and impervious ground
      !-------------------------------------------
      ! roof
      IF (lbr < 1) THEN
         fwet_roof = fsno_roof !for snow layer exist
      ELSE
         ! surface wet fraction. assuming max ponding = 1 kg/m2
         fwet_roof = (max(0., wliq_roofsno+wice_roofsno))**(2/3.)
         fwet_roof = min(1., fwet_roof)
      ENDIF

      ! dew case
      IF (qm > qroof) THEN
         fwet_roof = 1.
      ENDIF

!-----------------------------------------------------------------------
!     Compute sensible and latent fluxes and their derivatives with respect
!     to ground temperature using ground temperatures from previous time step.
!-----------------------------------------------------------------------
! Initialization variables
      nmozsgn = 0
      obuold  = 0.

      dth   = thm-troof
      dqh   = qm-qroof
      dthv  = dth*(1.+0.61*qm)+0.61*th*dqh
      zldis = hu-hroof-0.

      CALL moninobukini(ur,th,thm,thv,dth,dqh,dthv,zldis,z0mg,um,obu)

! Evaluated stability-dependent variables using moz from prior iteration
      niters=6

      !----------------------------------------------------------------
      ITERATION : DO iter = 1, niters         !begin stability iteration
      !----------------------------------------------------------------
         displax = hroof
         CALL moninobuk(hu,ht,hq,displax,z0mg,z0hg,z0qg,obu,um,&
                        ustar,fh2m,fq2m,fm10m,fm,fh,fq)

         tstar = vonkar/fh*dth
         qstar = vonkar/fq*dqh

         z0hg = z0mg/exp(0.13 * (ustar*z0mg/1.5e-5)**0.45)
         z0qg = z0hg

         thvstar=tstar*(1.+0.61*qm)+0.61*th*qstar
         zeta=zldis*vonkar*grav*thvstar/(ustar**2*thv)
         IF (zeta >= 0.) THEN     !stable
           zeta = min(2.,max(zeta,1.e-6))
         ELSE                     !unstable
           zeta = max(-100.,min(zeta,-1.e-6))
         ENDIF
         obu = zldis/zeta

         IF (zeta >= 0.) THEN
           um = max(ur,0.1)
         ELSE
           wc = (-grav*ustar*thvstar*zii/thv)**(1./3.)
          wc2 = beta*beta*(wc*wc)
           um = sqrt(ur*ur+wc2)
         ENDIF

         IF (obuold*obu < 0.) nmozsgn = nmozsgn+1
         IF (nmozsgn >= 4) EXIT

         obuold = obu

      !----------------------------------------------------------------
      ENDDO ITERATION                         !end stability iteration
      !----------------------------------------------------------------

! Get derivative of fluxes with respect to ground temperature
      ram    = 1./(ustar*ustar/um)
      rah    = 1./(vonkar/fh*ustar)
      raw    = 1./(vonkar/fq*ustar)

      raih   = rhoair*cpair/rah
      raiw   = rhoair/raw
      croofs = raih
      croofl = raiw*dqroofdT*fwet_roof
      croof  = croofs + htvp_roof*croofl

      zol = zeta

! surface fluxes of momentum, sensible and latent
! using ground temperatures from previous time step
      !taux    = -rhoair*us/ram
      !tauy    = -rhoair*vs/ram
      fsenroof = -raih*dth
      fevproof = -raiw*dqh*fwet_roof

   END SUBROUTINE UrbanRoofFlux

END MODULE MOD_Urban_RoofFlux
! ---------- EOP ------------
