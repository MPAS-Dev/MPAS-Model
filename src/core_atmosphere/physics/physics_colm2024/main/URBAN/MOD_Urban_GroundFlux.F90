#include <define.h>

MODULE MOD_Urban_GroundFlux

   USE MOD_Precision
   IMPLICIT NONE
   SAVE

   PUBLIC :: UrbanGroundFlux

CONTAINS

   SUBROUTINE UrbanGroundFlux (hu, ht, hq, us, vs, tm, qm, rhoair, psrf, &
                               ur, thm, th, thv, zlnd, zsno, fsno_gimp, &
                               lbi, wliq_gimpsno,wice_gimpsno, &
                               fcover, tgimp, tgper, qgimp, qgper, tref, qref, &
                               z0m, z0hg, zol, ustar, qstar, tstar, fm, fh, fq)

!=======================================================================
!
! !DESCRIPTION:
!  This is the main subroutine to execute the calculation
!  of bare ground fluxes
!
!  Created by Hua Yuan, 09/2021
!
! !REVISIONS:
!  07/2022, Hua Yuan: Urban 2m T/q -> above bare ground 2m.
!
!=======================================================================

   USE MOD_Precision
   USE MOD_Const_Physical, only: cpair,vonkar,grav
   USE MOD_FrictionVelocity
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   integer , intent(in) :: &
        lbi
   real(r8), intent(in) :: &
        ! atmospherical variables and observational height
        hu,           &! observational height of wind [m]
        ht,           &! observational height of temperature [m]
        hq,           &! observational height of humidity [m]
        us,           &! wind component in eastward direction [m/s]
        vs,           &! wind component in northward direction [m/s]
        tm,           &! temperature at agcm reference height [kelvin] [not used]
        qm,           &! specific humidity at agcm reference height [kg/kg]
        rhoair,       &! density air [kg/m3]
        psrf,         &! atmosphere pressure at the surface [pa] [not used]

        ur,           &! wind speed at reference height [m/s]
        thm,          &! intermediate variable (tm+0.0098*ht)
        th,           &! potential temperature (kelvin)
        thv,          &! virtual potential temperature (kelvin)

        zlnd,         &! roughness length for soil [m]
        zsno,         &! roughness length for snow [m]
        fsno_gimp,    &! fraction of impervious ground covered by snow
        fcover(0:5),  &! coverage of aboveground urban components [-]

        wliq_gimpsno, &! liqui water [kg/m2]
        wice_gimpsno, &! ice lens [kg/m2]

        tgimp,        &! ground impervious temperature [K]
        tgper,        &! ground pervious temperature [K]
        qgimp,        &! ground impervious specific humidity [kg/kg]
        qgper          ! ground pervious specific humidity [kg/kg]

   real(r8), intent(out) :: &
        tref,         &! 2 m height air temperature [kelvin]
        qref           ! 2 m height air humidity

   real(r8), intent(out) :: &
        z0m,          &! effective roughness [m]
        z0hg,         &! roughness length over ground, sensible heat [m]
        zol,          &! dimensionless height (z/L) used in Monin-Obukhov theory
        ustar,        &! friction velocity [m/s]
        tstar,        &! temperature scaling parameter
        qstar,        &! moisture scaling parameter
        fm,           &! integral of profile function for momentum
        fh,           &! integral of profile function for heat
        fq             ! integral of profile function for moisture

!-------------------------- Local Variables ----------------------------
   integer niters,    &! maximum number of iterations for surface temperature
        iter,         &! iteration index
        nmozsgn        ! number of times moz changes sign

   real(r8) :: &
        beta,         &! coefficient of convective velocity [-]
        displax,      &! zero-displacement height [m]
        tg,           &! ground surface temperature [K]
        qg,           &! ground specific humidity [kg/kg]
        fg,           &! ground fractional cover [-]
        fgimp,        &! weight of impervious ground
        fgper,        &! weight of pervious ground
        dth,          &! diff of virtual temp. between ref. height and surface
        dqh,          &! diff of humidity between ref. height and surface
        dthv,         &! diff of vir. poten. temp. between ref. height and surface
        obu,          &! monin-obukhov length (m)
        obuold,       &! monin-obukhov length from previous iteration
        fh2m,         &! relation for temperature at 2m
        fq2m,         &! relation for specific humidity at 2m
        fm10m,        &! integral of profile function for momentum at 10m
        thvstar,      &! virtual potential temperature scaling parameter
        um,           &! wind speed including the stability effect [m/s]
        wc,           &! convective velocity [m/s]
        wc2,          &! wc**2
        zeta,         &! dimensionless height used in Monin-Obukhov theory
        zii,          &! convective boundary height [m]
        zldis,        &! reference height "minus" zero displacement height [m]
        z0mg,         &! roughness length over ground, momentum [m]
        z0qg           ! roughness length over ground, latent heat [m]

   real(r8) fwet_gimp, fwetfac

!-----------------------------------------------------------------------

! initial roughness length
      !NOTE: change to original
      !z0mg = (1.-fsno)*zlnd + fsno*zsno
      IF (fsno_gimp > 0) THEN
         z0mg = zsno
      ELSE
         z0mg = zlnd
      ENDIF
      z0hg = z0mg
      z0qg = z0mg

! potential temperature at the reference height
      beta = 1.       !-  (in computing W_*)
      zii  = 1000.    !m  (pbl height)
      z0m  = z0mg

      fg   = 1 - fcover(0)
      fgimp = fcover(3)/fg
      fgper = fcover(4)/fg

      ! weighted tg
      tg = tgimp*fgimp + tgper*fgper

      ! wet fraction impervious ground
      !-------------------------------------------
      IF (lbi < 1) THEN
         fwet_gimp = fsno_gimp !for snow layer exist
      ELSE
         ! surface wet fraction. assuming max ponding = 1 kg/m2
         fwet_gimp = (max(0., wliq_gimpsno+wice_gimpsno))**(2/3.)
         fwet_gimp = min(1., fwet_gimp)
      ENDIF

      ! dew case
      IF (qm > qgimp) THEN
         fwet_gimp = 1.
      ENDIF

      ! weighted qg
      fwetfac = fgimp*fwet_gimp + fgper
      qg = (qgimp*fgimp*fwet_gimp + qgper*fgper)/fwetfac

!-----------------------------------------------------------------------
!     Compute sensible and latent fluxes and their derivatives with respect
!     to ground temperature using ground temperatures from previous time step.
!-----------------------------------------------------------------------
! Initialization variables
      nmozsgn = 0
      obuold  = 0.

      dth   = thm-tg
      dqh   = qm-qg
      dthv  = dth*(1.+0.61*qm)+0.61*th*dqh
      zldis = hu-0.

      CALL moninobukini(ur,th,thm,thv,dth,dqh,dthv,zldis,z0mg,um,obu)

! Evaluated stability-dependent variables using moz from prior iteration
      niters=6

      !----------------------------------------------------------------
      ITERATION : DO iter = 1, niters         !begin stability iteration
      !----------------------------------------------------------------
         displax = 0.
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

      zol = zeta
      !rib = min(5.,zol*ustar**2/(vonkar**2/fh*um**2))

! 2 m height air temperature
      tref   = thm + vonkar/fh*dth * (fh2m/vonkar - fh/vonkar)
      qref   =  qm + vonkar/fq*dqh * (fq2m/vonkar - fq/vonkar)

   END SUBROUTINE UrbanGroundFlux

END MODULE MOD_Urban_GroundFlux
! ---------- EOP ------------
