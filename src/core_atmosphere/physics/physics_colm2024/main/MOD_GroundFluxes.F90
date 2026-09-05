MODULE MOD_GroundFluxes

!-----------------------------------------------------------------------
   USE MOD_Precision
   IMPLICIT NONE
   SAVE

! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: GroundFluxes


!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------


   SUBROUTINE GroundFluxes (zlnd, zsno, hu, ht, hq, hpbl, &
                            us, vs, tm, qm, rhoair, psrf, &
                            ur, thm, th, thv, t_grnd, qg, rss, dqgdT, htvp, &
                            fsno, cgrnd, cgrndl, cgrnds, &
                            t_soil, t_snow, q_soil, q_snow, &
                            taux, tauy, fseng, fseng_soil, fseng_snow, &
                            fevpg, fevpg_soil, fevpg_snow, tref, qref, &
                            z0m, z0hg, zol, rib, ustar, qstar, tstar, fm, fh, fq, &
                            fh2m, fq2m, fm10m)

!-----------------------------------------------------------------------
!  This is the main SUBROUTINE to execute the calculation of thermal
!  processes and surface fluxes
!
!  Original author: Yongjiu Dai, 09/15/1999; 08/30/2002
!
! !REVISIONS:
!  09/2019, Hua Yuan: removed sigf to be consistent with PFT runs, removed
!           fsena, fevpa, renamed z0ma to z0m.
!
!  05/2023, Shaofeng Liu: add option to call moninobuk_leddy, the LargeEddy
!           surface turbulence scheme (LZD2022); make a proper update of um.
!
!-----------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_Const_Physical, only: cpair,vonkar,grav
   USE MOD_FrictionVelocity
   USE mod_namelist, only: DEF_USE_CBL_HEIGHT,DEF_RSS_SCHEME
   USE MOD_TurbulenceLEddy

   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   real(r8), intent(in) :: &
          zlnd,      &! roughness length for soil [m]
          zsno,      &! roughness length for snow [m]

          ! atmospherical variables and observational height
          hu,        &! observational height of wind [m]
          ht,        &! observational height of temperature [m]
          hq,        &! observational height of humidity [m]
          hpbl,      &! atmospheric boundary layer height [m]
          us,        &! wind component in eastward direction [m/s]
          vs,        &! wind component in northward direction [m/s]
          tm,        &! temperature at agcm reference height [kelvin] [not used]
          qm,        &! specific humidity at agcm reference height [kg/kg]
          rhoair,    &! density air [kg/m3]
          psrf,      &! atmosphere pressure at the surface [pa] [not used]

          fsno,      &! fraction of ground covered by snow

          ur,        &! wind speed at reference height [m/s]
          thm,       &! intermediate variable (tm+0.0098*ht)
          th,        &! potential temperature (kelvin)
          thv,       &! virtual potential temperature (kelvin)

          t_grnd,    &! ground surface temperature [K]
          t_soil,    &! ground soil temperature [K]
          t_snow,    &! ground snow temperature [K]
          qg,        &! ground specific humidity [kg/kg]
          q_soil,    &! ground soil specific humidity [kg/kg]
          q_snow,    &! ground snow specific humidity [kg/kg]
          dqgdT,     &! d(qg)/dT
          rss,       &! soil surface resistance for evaporation [s/m]
          htvp        ! latent heat of vapor of water (or sublimation) [j/kg]

   real(r8), intent(out) :: &
          taux,      &! wind stress: E-W [kg/m/s**2]
          tauy,      &! wind stress: N-S [kg/m/s**2]
          fseng,     &! sensible heat flux from ground [W/m2]
          fseng_soil,&! sensible heat flux from ground soil [W/m2]
          fseng_snow,&! sensible heat flux from ground snow [W/m2]
          fevpg,     &! evaporation heat flux from ground [mm/s]
          fevpg_soil,&! evaporation heat flux from ground soil [mm/s]
          fevpg_snow,&! evaporation heat flux from ground snow [mm/s]
          cgrnd,     &! deriv. of soil energy flux wrt to soil temp [w/m2/k]
          cgrndl,    &! deriv, of soil sensible heat flux wrt soil temp [w/m2/k]
          cgrnds,    &! deriv of soil latent heat flux wrt soil temp [w/m**2/k]
          tref,      &! 2 m height air temperature [kelvin]
          qref,      &! 2 m height air humidity

          z0m,       &! effective roughness [m]
          z0hg,      &! roughness length over ground, sensible heat [m]
          zol,       &! dimensionless height (z/L) used in Monin-Obukhov theory
          rib,       &! bulk Richardson number in surface layer
          ustar,     &! friction velocity [m/s]
          tstar,     &! temperature scaling parameter
          qstar,     &! moisture scaling parameter
          fm,        &! integral of profile FUNCTION for momentum
          fh,        &! integral of profile FUNCTION for heat
          fq,        &! integral of profile FUNCTION for moisture
          fh2m,      &! relation for temperature at 2m
          fq2m,      &! relation for specific humidity at 2m
          fm10m       ! integral of profile FUNCTION for momentum at 10m

!-------------------------- Local Variables ----------------------------
   integer niters,   &! maximum number of iterations for surface temperature
         iter,       &! iteration index
         nmozsgn      ! number of times moz changes sign

   real(r8) :: &
         beta,       &! coefficient of convective velocity [-]
         displax,    &! zero-displacement height [m]
         dth,        &! diff of virtual temp. between ref. height and surface
         dqh,        &! diff of humidity between ref. height and surface
         dthv,       &! diff of vir. poten. temp. between ref. height and surface
         obu,        &! monin-obukhov length (m)
         obuold,     &! monin-obukhov length from previous iteration
         ram,        &! aerodynamical resistance [s/m]
         rah,        &! thermal resistance [s/m]
         raw,        &! moisture resistance [s/m]
         raih,       &! temporary variable [kg/m2/s]
         raiw,       &! temporary variable [kg/m2/s]
         thvstar,    &! virtual potential temperature scaling parameter
         um,         &! wind speed including the stability effect [m/s]
         wc,         &! convective velocity [m/s]
         wc2,        &! wc**2
         zeta,       &! dimensionless height used in Monin-Obukhov theory
         zii,        &! convective boundary height [m]
         zldis,      &! reference height "minus" zero displacement height [m]
         z0mg,       &! roughness length over ground, momentum [m]
         z0qg         ! roughness length over ground, latent heat [m]

!-----------------------------------------------------------------------
  ! initial roughness length
      ! 09/2019, yuan: change to a combination of zlnd and zsno
      z0mg = (1.-fsno)*zlnd + fsno*zsno
      z0hg = z0mg
      z0qg = z0mg

  ! potential temperature at the reference height
      beta = 1.      ! -  (in computing W_*)
      zii  = 1000.   ! m  (pbl height)
      z0m  = z0mg

  !-----------------------------------------------------------------------
  !   Compute sensible and latent fluxes and their derivatives with respect
  !   to ground temperature using ground temperatures from previous time step.
  !-----------------------------------------------------------------------
  ! Initialization variables
      nmozsgn = 0
      obuold = 0.

      dth   = thm-t_grnd
      dqh   = qm-qg
      dthv  = dth*(1.+0.61*qm)+0.61*th*dqh
      zldis = hu-0.

      CALL moninobukini(ur,th,thm,thv,dth,dqh,dthv,zldis,z0mg,um,obu)

  ! Evaluated stability-dependent variables using moz from prior iteration
      niters=6

      !----------------------------------------------------------------
      ITERATION : DO iter = 1, niters         ! begin stability iteration
      !----------------------------------------------------------------
         displax = 0.
         IF (DEF_USE_CBL_HEIGHT) THEN
           CALL moninobuk_leddy(hu,ht,hq,displax,z0mg,z0hg,z0qg,obu,um,hpbl, &
                                ustar,fh2m,fq2m,fm10m,fm,fh,fq)
         ELSE
           CALL moninobuk(hu,ht,hq,displax,z0mg,z0hg,z0qg,obu,um,&
                          ustar,fh2m,fq2m,fm10m,fm,fh,fq)
         ENDIF

         tstar = vonkar/fh*dth
         qstar = vonkar/fq*dqh

         z0hg = z0mg/exp(0.13 * (ustar*z0mg/1.5e-5)**0.45)
         z0qg = z0hg

         ! 2023.04.06, weinan
         !thvstar=tstar+0.61*th*qstar
         thvstar=tstar*(1.+0.61*qm)+0.61*th*qstar
         zeta=zldis*vonkar*grav*thvstar/(ustar**2*thv)
         IF(zeta >= 0.) THEN     !stable
           zeta = min(2.,max(zeta,1.e-6))
         ELSE                    !unstable
           zeta = max(-100.,min(zeta,-1.e-6))
         ENDIF
         obu = zldis/zeta

         IF(zeta >= 0.)THEN
           um = max(ur,0.1)
         ELSE
           IF (DEF_USE_CBL_HEIGHT) THEN !//TODO: Shaofeng, 2023.05.18
             zii = max(5.*hu,hpbl)
           ENDIF !//TODO: Shaofeng, 2023.05.18
           wc = (-grav*ustar*thvstar*zii/thv)**(1./3.)
          wc2 = beta*beta*(wc*wc)
           um = sqrt(ur*ur+wc2)
         ENDIF

         IF (obuold*obu < 0.) nmozsgn = nmozsgn+1
         IF (nmozsgn >= 4) EXIT

         obuold = obu

      !----------------------------------------------------------------
      ENDDO ITERATION                         ! END stability iteration
      !----------------------------------------------------------------

  ! Get derivative of fluxes with respect to ground temperature
      ram  = 1./(ustar*ustar/um)
      rah  = 1./(vonkar/fh*ustar)
      raw  = 1./(vonkar/fq*ustar)

      raih = rhoair*cpair/rah

  ! 08/23/2019, yuan: add soil surface resistance (rss)
      IF (dqh > 0.) THEN
         raiw = rhoair/raw !dew case. assume no soil resistance
      ELSE
         IF (DEF_RSS_SCHEME .eq. 4) THEN
            raiw = rss*rhoair/raw
         ELSE
            raiw = rhoair/(raw+rss)
         ENDIF
      ENDIF

      cgrnds = raih
      cgrndl = raiw*dqgdT
      cgrnd  = cgrnds + htvp*cgrndl

      zol = zeta
      rib = min(5.,zol*ustar**2/(vonkar**2/fh*um**2))

  ! surface fluxes of momentum, sensible and latent
  ! using ground temperatures from previous time step
      taux  = -rhoair*us/ram
      tauy  = -rhoair*vs/ram
      fseng = -raih*dth
      fevpg = -raiw*dqh

      fseng_soil = -raih * (thm - t_soil)
      fseng_snow = -raih * (thm - t_snow)
      fevpg_soil = -raiw * ( qm - q_soil)
      fevpg_snow = -raiw * ( qm - q_snow)

  ! 2 m height air temperature
      tref = thm + vonkar/fh*dth * (fh2m/vonkar - fh/vonkar)
      qref =  qm + vonkar/fq*dqh * (fq2m/vonkar - fq/vonkar)

   END SUBROUTINE GroundFluxes

END MODULE MOD_GroundFluxes
