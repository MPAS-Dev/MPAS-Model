#include <define.h>

MODULE MOD_Glacier

!-----------------------------------------------------------------------
!  Energy and Mass Balance Model of LAND ICE (GLACIER / ICE SHEET)
!
!  Original author: Yongjiu Dai, /05/2014/
!
! !REVISIONS:
!  01/2023, Hua Yuan: added GLACIER_WATER_snicar() to account for SNICAR
!           model effects on snow water [see snowwater_snicar()], snow
!           layers combine [see snowlayerscombine_snicar()], snow layers
!           divide [see snowlayersdivide_snicar()]
!
!  01/2023, Hua Yuan: added snow layer absorption in GLACIER_TEMP()
!-----------------------------------------------------------------------
   USE MOD_Precision
   USE MOD_MPAS_MPI, only: CoLM_stop
   IMPLICIT NONE
   SAVE

! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: GLACIER_TEMP
   PUBLIC :: GLACIER_WATER
   PUBLIC :: GLACIER_WATER_snicar


! PRIVATE MEMBER FUNCTIONS:
   PRIVATE :: groundfluxes_glacier
   PRIVATE :: groundtem_glacier


!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------


   SUBROUTINE GLACIER_TEMP (patchtype,lb      ,nl_ice      ,deltim      ,&
                      zlnd        ,zsno       ,capr        ,cnfac       ,&
                      forc_hgt_u  ,forc_hgt_t ,forc_hgt_q  ,forc_us     ,&
                      forc_vs     ,forc_t     ,forc_q      ,forc_hpbl   ,&
                      forc_rhoair ,forc_psrf  ,coszen      ,sabg        ,&
                      forc_frl    ,fsno       ,dz_icesno   ,z_icesno    ,&
                      zi_icesno   ,t_icesno   ,wice_icesno ,wliq_icesno ,&
                      scv         ,snowdp     ,imelt       ,taux        ,&
                      tauy        ,fsena      ,fevpa       ,lfevpa      ,&
                      fseng       ,fevpg      ,olrg        ,fgrnd       ,&
                      qseva       ,qsdew      ,qsubl       ,qfros       ,&
                      sm          ,tref       ,qref        ,trad        ,&
                      errore      ,emis       ,z0m         ,zol         ,&
                      rib         ,ustar      ,qstar       ,tstar       ,&
                      fm          ,fh         ,fq          ,fh2m        ,&
                      fq2m        ,fm10m      ,pg_rain     ,&
                      pg_snow     ,t_precip   ,snofrz      ,sabg_snow_lyr)

!-----------------------------------------------------------------------
! this is the main SUBROUTINE to execute the calculation of thermal processes
! and surface fluxes of the land ice (glacier and ice sheet)
!
! Original author: Yongjiu Dai and Nan Wei, /05/2014/
! Modified by Nan Wei, 07/2017/  interaction btw prec and land ice
! FLOW DIAGRAM FOR GLACIER_TEMP.F90
!
! GLACIER_TEMP ===> qsadv
!                   groundfluxes | --------->  |moninobukini
!                                |             |moninobuk
!
!                   groundTem    | --------->  |meltf
!
!-----------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_Const_Physical, only: hvap,hsub,rgas,cpair,stefnc,tfrz,cpliq,cpice
   USE MOD_FrictionVelocity
   USE MOD_Qsadv

   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------

   integer, intent(in) :: &
        patchtype,&   ! land patch type (0=soil, 1=urban and built-up,
                      ! 2=wetland, 3=land ice, 4=land water bodies, 99 = ocean)
        lb,          &! lower bound of array
        nl_ice        ! upper bound of array

   real(r8), intent(in) :: &
        deltim,      &! model time step [second]
        zlnd,        &! roughness length for ice surface [m]
        zsno,        &! roughness length for snow [m]
        capr,        &! tuning factor to turn first layer T into surface T
        cnfac,       &! Crank Nicholson factor between 0 and 1

        ! Atmospherical variables and observational height
        forc_hgt_u,  &! observational height of wind [m]
        forc_hgt_t,  &! observational height of temperature [m]
        forc_hgt_q,  &! observational height of humidity [m]
        forc_us,     &! wind component in eastward direction [m/s]
        forc_vs,     &! wind component in northward direction [m/s]
        forc_t,      &! temperature at agcm reference height [kelvin]
        forc_q,      &! specific humidity at agcm reference height [kg/kg]
        forc_rhoair, &! density air [kg/m3]
        forc_psrf,   &! atmosphere pressure at the surface [pa]
        t_precip,    &! snowfall/rainfall temperature [kelvin]
        pg_rain,     &! rainfall  [kg/(m2 s)]
        pg_snow,     &! snowfall  [kg/(m2 s)]
        forc_hpbl,   &! atmospheric boundary layer height [m]

        ! Radiative fluxes
        coszen,      &! cosine of the solar zenith angle
        sabg,        &! solar radiation absorbed by ground [W/m2]
        forc_frl,    &! atmospheric infrared (longwave) radiation [W/m2]

        ! State variable (1)
        fsno,        &! fraction of ground covered by snow
        dz_icesno(lb:nl_ice),  &! layer thickness [m]
        z_icesno (lb:nl_ice),  &! node depth [m]
        zi_icesno(lb-1:nl_ice)  ! interface depth [m]

   real(r8), intent(in) :: &
        sabg_snow_lyr (lb:1)    ! snow layer absorption [W/m-2]

        ! State variables (2)
   real(r8), intent(inout) :: &
        t_icesno(lb:nl_ice),   &! snow/ice temperature [K]
        wice_icesno(lb:nl_ice),&! ice lens [kg/m2]
        wliq_icesno(lb:nl_ice),&! liquid water [kg/m2]
        scv,                   &! snow cover, water equivalent [mm, kg/m2]
        snowdp                  ! snow depth [m]

   real(r8), intent(inout) :: &
        snofrz (lb:0)           ! snow freezing rate (lyr) [kg m-2 s-1]

   integer, intent(out) :: &
        imelt(lb:nl_ice)        ! flag for melting or freezing [-]

        ! Output fluxes
   real(r8), intent(out) :: &
        taux,        &! wind stress: E-W [kg/m/s**2]
        tauy,        &! wind stress: N-S [kg/m/s**2]
        fsena,       &! sensible heat to atmosphere [W/m2]
        lfevpa,      &! latent heat flux to atmosphere [W/m2]
        fseng,       &! sensible heat flux from ground [W/m2]
        fevpg,       &! evaporation heat flux from ground [mm/s]
        olrg,        &! outgoing long-wave radiation to atmosphere
        fgrnd,       &! ground heat flux [W/m2]

        fevpa,       &! evapotranspiration to atmosphere (mm h2o/s)
        qseva,       &! ground surface evaporation rate (mm h2o/s)
        qsdew,       &! ground surface dew formation (mm h2o /s) [+]
        qsubl,       &! sublimation rate from snow pack (mm h2o /s) [+]
        qfros,       &! surface dew added to snow pack (mm h2o /s) [+]

        sm,          &! rate of snowmelt [kg/(m2 s)]
        tref,        &! 2 m height air temperature [kelvin]
        qref,        &! 2 m height air specific humidity
        trad,        &! radiative temperature [K]

        emis,        &! averaged bulk surface emissivity
        z0m,         &! effective roughness [m]
        zol,         &! dimensionless height (z/L) used in Monin-Obukhov theory
        rib,         &! bulk Richardson number in surface layer
        ustar,       &! u* in similarity theory [m/s]
        qstar,       &! q* in similarity theory [kg/kg]
        tstar,       &! t* in similarity theory [K]
        fm,          &! integral of profile FUNCTION for momentum
        fh,          &! integral of profile FUNCTION for heat
        fq,          &! integral of profile FUNCTION for moisture
        fh2m,        &! relation for temperature at 2m
        fq2m,        &! relation for specific humidity at 2m
        fm10m         ! integral of profile FUNCTION for momentum at 10m

!-------------------------- Local Variables ----------------------------
   integer i,j

   real(r8) :: &
        cgrnd,        &! deriv. of ice energy flux wrt to ice temp [w/m2/k]
        cgrndl,       &! deriv, of ice sensible heat flux wrt ice temp [w/m2/k]
        cgrnds,       &! deriv of ice latent heat flux wrt ice temp [w/m**2/k]
        degdT,        &! d(eg)/dT
        dqgdT,        &! d(qg)/dT
        eg,           &! water vapor pressure at temperature T [pa]
        egsmax,       &! max. evaporation which ice can provide at one time step
        egidif,       &! the excess of evaporation over "egsmax"
        emg,          &! ground emissivity (0.96)
        errore,       &! energy balance error [w/m2]
        fact(lb:nl_ice), &! used in computing tridiagonal matrix
        htvp,         &! latent heat of vapor of water (or sublimation) [j/kg]
        qg,           &! ground specific humidity [kg/kg]
        qsatg,        &! saturated humidity [kg/kg]
        qsatgdT,      &! d(qsatg)/dT
        qred,         &! ice surface relative humidity
        thm,          &! intermediate variable (forc_t+0.0098*forc_hgt_t)
        th,           &! potential temperature (kelvin)
        thv,          &! virtual potential temperature (kelvin)
        t_grnd,       &! ground surface temperature [K]
        t_icesno_bef(lb:nl_ice), &! ice/snow temperature before update
        tinc,         &! temperature difference of two time step
        ur,           &! wind speed at reference height [m/s]
        xmf            ! total latent heat of phase change of ground water

!=======================================================================
! [1] Initial set and propositional variables
!=======================================================================

      ! temperature and water mass from previous time step
      t_grnd = t_icesno(lb)
      t_icesno_bef(lb:) = t_icesno(lb:)

      ! emissivity
      emg = 0.97

      ! latent heat, assumed that the sublimation occurs only as wliq_icesno=0
      htvp = hvap
      IF(wliq_icesno(lb)<=0. .and. wice_icesno(lb)>0.) htvp = hsub

      ! potential temperature at the reference height
      thm = forc_t + 0.0098*forc_hgt_t  ! intermediate variable equivalent to
                                        ! forc_t*(pgcm/forc_psrf)**(rgas/cpair)
      th = forc_t*(100000./forc_psrf)**(rgas/cpair) ! potential T
      thv = th*(1.+0.61*forc_q)         ! virtual potential T
      ur = max(0.1,sqrt(forc_us*forc_us+forc_vs*forc_vs))   ! limit set to 0.1

!=======================================================================
! [2] specific humidity and its derivative at ground surface
!=======================================================================

      qred = 1.
      CALL qsadv(t_grnd,forc_psrf,eg,degdT,qsatg,qsatgdT)

      qg = qred*qsatg
      dqgdT = qred*qsatgdT

!=======================================================================
! [3] Compute sensible and latent fluxes and their derivatives with respect
!     to ground temperature using ground temperatures from previous time step.
!=======================================================================

      CALL groundfluxes_glacier (zlnd,zsno,forc_hgt_u,forc_hgt_t,forc_hgt_q,&
                        forc_us,forc_vs,forc_t,forc_q,forc_rhoair,forc_psrf, &
                        ur,thm,th,thv,t_grnd,qg,dqgdT,htvp,&
                        forc_hpbl,&
                        fsno,cgrnd,cgrndl,cgrnds,&
                        taux,tauy,fsena,fevpa,fseng,fevpg,tref,qref,&
                        z0m,zol,rib,ustar,qstar,tstar,fm,fh,fq,fh2m,fq2m,fm10m)

!=======================================================================
! [4] Ground temperature
!=======================================================================

      CALL groundtem_glacier (patchtype,lb,nl_ice,deltim,&
                     capr,cnfac,dz_icesno,z_icesno,zi_icesno,&
                     t_icesno,wice_icesno,wliq_icesno,scv,snowdp,&
                     forc_frl,sabg,sabg_snow_lyr,fseng,fevpg,cgrnd,htvp,emg,&
                     imelt,snofrz,sm,xmf,fact,pg_rain,pg_snow,t_precip)

!=======================================================================
! [5] Correct fluxes to present ice temperature
!=======================================================================

      t_grnd = t_icesno(lb)
      tinc = t_icesno(lb) - t_icesno_bef(lb)
      fseng = fseng + tinc*cgrnds
      fevpg = fevpg + tinc*cgrndl

! calculation of evaporative potential; flux in kg m-2 s-1.
! egidif holds the excess energy IF all water is evaporated
! during the timestep. this energy is later added to the sensible heat flux.

      egsmax = (wice_icesno(lb)+wliq_icesno(lb)) / deltim

      egidif = max( 0., fevpg - egsmax )
      fevpg = min ( fevpg, egsmax )
      fseng = fseng + htvp*egidif

! total fluxes to atmosphere
      fsena = fseng
      fevpa = fevpg
      lfevpa= htvp*fevpg   ! W/m^2 (accounting for sublimation)

      qseva = 0.
      qsubl = 0.
      qfros = 0.
      qsdew = 0.

      IF(fevpg >= 0)THEN
         qseva = min(wliq_icesno(lb)/deltim, fevpg)
         qsubl = fevpg - qseva
      ELSE
         IF(t_grnd < tfrz)THEN
            qfros = abs(fevpg)
         ELSE
            qsdew = abs(fevpg)
         ENDIF
      ENDIF

! ground heat flux
      fgrnd = sabg + emg*forc_frl &
            - emg*stefnc*t_icesno_bef(lb)**3*(t_icesno_bef(lb) + 4.*tinc) &
            - (fseng+fevpg*htvp) &
            + cpliq * pg_rain * (t_precip - t_icesno(lb)) &
            + cpice * pg_snow * (t_precip - t_icesno(lb))

! outgoing long-wave radiation from ground
      olrg = (1.-emg)*forc_frl + emg*stefnc * t_icesno_bef(lb)**4 &
! for conservation we put the increase of ground longwave to outgoing
           + 4.*emg*stefnc*t_icesno_bef(lb)**3*tinc

! averaged bulk surface emissivity
      emis = emg

! radiative temperature
      trad = (olrg/stefnc)**0.25

!=======================================================================
! [6] energy balance error
!=======================================================================

      errore = sabg + forc_frl - olrg - fsena - lfevpa - xmf &
             + cpliq * pg_rain * (t_precip-t_icesno(lb)) &
             + cpice * pg_snow * (t_precip-t_icesno(lb))
      DO j = lb, nl_ice
         errore = errore - (t_icesno(j)-t_icesno_bef(j))/fact(j)
      ENDDO

#if (defined CoLMDEBUG)
      IF(abs(errore)>.2)THEN
         write(6,*) 'GLACIER_TEMP.F90 : energy balance violation'
         write(6,100) errore,sabg,forc_frl,olrg,fsena,lfevpa,xmf,t_precip,t_icesno(lb)
         CALL CoLM_stop('GLACIER_TEMP energy balance violation.')
      ENDIF
100   format(10(f7.3))
#endif

   END SUBROUTINE GLACIER_TEMP



   SUBROUTINE groundfluxes_glacier (zlnd,zsno,hu,ht,hq,&
                                    us,vs,tm,qm,rhoair,psrf,&
                                    ur,thm,th,thv,t_grnd,qg,dqgdT,htvp,&
                                    hpbl,&
                                    fsno,cgrnd,cgrndl,cgrnds,&
                                    taux,tauy,fsena,fevpa,fseng,fevpg,tref,qref,&
                                    z0m,zol,rib,ustar,qstar,tstar,fm,fh,fq,fh2m,fq2m,fm10m)

!=======================================================================
!  this is the main SUBROUTINE to execute the calculation of thermal processes
!  and surface fluxes of land ice (glacier and ice sheet)
!
!  Original author: Yongjiu Dai and Nan Wei, /05/2014/
!
! !REVISIONS:
!  05/2023, Shaofeng Liu: add option to CALL moninobuk_leddy, the LargeEddy
!           surface turbulence scheme (LZD2022); make a proper update of um.
!=======================================================================

   USE MOD_Precision
   USE MOD_Const_Physical, only: cpair,vonkar,grav
   USE MOD_FrictionVelocity
   USE MOD_Namelist, only: DEF_USE_CBL_HEIGHT
   USE MOD_TurbulenceLEddy
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   real(r8), intent(in) :: &
        zlnd,     &! roughness length for ice [m]
        zsno,     &! roughness length for snow [m]

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

        fsno,     &! fraction of ground covered by snow

        ur,       &! wind speed at reference height [m/s]
        thm,      &! intermediate variable (tm+0.0098*ht)
        th,       &! potential temperature (kelvin)
        thv,      &! virtual potential temperature (kelvin)

        t_grnd,   &! ground surface temperature [K]
        qg,       &! ground specific humidity [kg/kg]
        dqgdT,    &! d(qg)/dT
        htvp       ! latent heat of vapor of water (or sublimation) [j/kg]
   real(r8), intent(in) :: &
        hpbl       ! atmospheric boundary layer height [m]


   real(r8), intent(out) :: &
        taux,     &! wind stress: E-W [kg/m/s**2]
        tauy,     &! wind stress: N-S [kg/m/s**2]
        fsena,    &! sensible heat to atmosphere [W/m2]
        fevpa,    &! evapotranspiration to atmosphere [mm/s]
        fseng,    &! sensible heat flux from ground [W/m2]
        fevpg,    &! evaporation heat flux from ground [mm/s]
        cgrnd,    &! deriv. of ice energy flux wrt to ice temp [w/m2/k]
        cgrndl,   &! deriv, of ice sensible heat flux wrt ice temp [w/m2/k]
        cgrnds,   &! deriv of ice latent heat flux wrt ice temp [w/m**2/k]
        tref,     &! 2 m height air temperature [kelvin]
        qref,     &! 2 m height air humidity

        z0m,      &! effective roughness [m]
        zol,      &! dimensionless height (z/L) used in Monin-Obukhov theory
        rib,      &! bulk Richardson number in surface layer
        ustar,    &! friction velocity [m/s]
        tstar,    &! temperature scaling parameter
        qstar,    &! moisture scaling parameter
        fm,       &! integral of profile FUNCTION for momentum
        fh,       &! integral of profile FUNCTION for heat
        fq,       &! integral of profile FUNCTION for moisture
        fh2m,     &! relation for temperature at 2m
        fq2m,     &! relation for specific humidity at 2m
        fm10m      ! integral of profile FUNCTION for momentum at 10m

!-------------------------- Local Variables ----------------------------
   integer niters, &! maximum number of iterations for surface temperature
       iter,      &! iteration index
       nmozsgn     ! number of times moz changes sign

   real(r8) :: &
       beta,      &! coefficient of convective velocity [-]
       displax,   &! zero-displacement height [m]
       dth,       &! diff of virtual temp. between ref. height and surface
       dqh,       &! diff of humidity between ref. height and surface
       dthv,      &! diff of vir. poten. temp. between ref. height and surface
       obu,       &! monin-obukhov length (m)
       obuold,    &! monin-obukhov length from previous iteration
       ram,       &! aerodynamical resistance [s/m]
       rah,       &! thermal resistance [s/m]
       raw,       &! moisture resistance [s/m]
       raih,      &! temporary variable [kg/m2/s]
       raiw,      &! temporary variable [kg/m2/s]
       thvstar,   &! virtual potential temperature scaling parameter
       um,        &! wind speed including the stability effect [m/s]
       wc,        &! convective velocity [m/s]
       wc2,       &! wc**2
       zeta,      &! dimensionless height used in Monin-Obukhov theory
       zii,       &! convective boundary height [m]
       zldis,     &! reference height "minus" zero displacement height [m]
       z0mg,      &! roughness length over ground, momentum [m]
       z0hg,      &! roughness length over ground, sensible heat [m]
       z0qg        ! roughness length over ground, latent heat [m]

!-----------------------------------------------------------------------
! initial roughness length
      IF(fsno > 0.)THEN
       ! z0mg = zsno
         z0mg = 0.002 ! Table 1 of Brock et al., (2006)
         z0hg = z0mg
         z0qg = z0mg
      ELSE
       ! z0mg = zlnd
         z0mg = 0.001 ! Table 1 of Brock et al., (2006)
         z0hg = z0mg
         z0qg = z0mg
      ENDIF

! potential temperature at the reference height
      beta = 1.      ! -  (in computing W_*)
      zii = 1000.    ! m  (pbl height)
      z0m = z0mg

!-----------------------------------------------------------------------
!     Compute sensible and latent fluxes and their derivatives with respect
!     to ground temperature using ground temperatures from previous time step.
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
           CALL moninobuk_leddy(hu,ht,hq,displax,z0mg,z0hg,z0qg,obu,um, hpbl, &
                          ustar,fh2m,fq2m,fm10m,fm,fh,fq)
         ELSE
           CALL moninobuk(hu,ht,hq,displax,z0mg,z0hg,z0qg,obu,um,&
                          ustar,fh2m,fq2m,fm10m,fm,fh,fq)
         ENDIF

         tstar = vonkar/fh*dth
         qstar = vonkar/fq*dqh

         z0hg = z0mg/exp(0.13 * (ustar*z0mg/1.5e-5)**0.45)
         z0qg = z0hg

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
      ram    = 1./(ustar*ustar/um)
      rah    = 1./(vonkar/fh*ustar)
      raw    = 1./(vonkar/fq*ustar)

      raih   = rhoair*cpair/rah
      raiw   = rhoair/raw
      cgrnds = raih
      cgrndl = raiw*dqgdT
      cgrnd  = cgrnds + htvp*cgrndl

      zol = zeta
      rib = min(5.,zol*ustar**2/(vonkar**2/fh*um**2))

! surface fluxes of momentum, sensible and latent
! using ground temperatures from previous time step
      taux   = -rhoair*us/ram
      tauy   = -rhoair*vs/ram
      fseng  = -raih*dth
      fevpg  = -raiw*dqh

      fsena  = fseng
      fevpa  = fevpg

! 2 m height air temperature
      tref   = (thm + vonkar/fh*dth * (fh2m/vonkar - fh/vonkar))
      qref   = ( qm + vonkar/fq*dqh * (fq2m/vonkar - fq/vonkar))

   END SUBROUTINE groundfluxes_glacier



   SUBROUTINE groundtem_glacier (patchtype,lb,nl_ice,deltim,&
                        capr,cnfac,dz_icesno,z_icesno,zi_icesno,&
                        t_icesno,wice_icesno,wliq_icesno,scv,snowdp,&
                        forc_frl,sabg,sabg_snow_lyr,fseng,fevpg,cgrnd,htvp,emg,&
                        imelt,snofrz,sm,xmf,fact,pg_rain,pg_snow,t_precip)

!=======================================================================
!  SNOW and LAND ICE temperatures
!  o The volumetric heat capacity is calculated as a linear combination
!    in terms of the volumetric fraction of the constituent phases.
!  o The thermal conductivity of snow/ice is computed from the
!    formulation used in SNTHERM (Jordan 1991) and Yen (1981),
!    respectively.
!  o Boundary conditions:
!    F = Rnet - Hg - LEg (top) + HPR, F= 0 (base of the land ice column).
!  o Ice/snow temperature is predicted from heat conduction in 10 ice
!    layers and up to 5 snow layers.  The thermal conductivities at the
!    interfaces between two neighbor layers (j, j+1) are derived from an
!    assumption that the flux across the interface is equal to that from
!    the node j to the interface and the flux from the interface to the
!    node j+1. The equation is solved using the Crank-Nicholson method
!    and resulted in a tridiagonal system equation.
!
!  Phase change (see meltf.F90)
!
!  Original author: Yongjiu Dai, /05/2014/
!
! !REVISIONS:
!  01/2023, Hua Yuan: account for snow layer absorption (SNICAR) in
!           ground heat flux, temperature and melt calculation.
!=======================================================================

   USE MOD_Precision
   USE MOD_Namelist, only: DEF_USE_SNICAR
   USE MOD_Const_Physical, only: stefnc,cpice,cpliq,denh2o,denice,tfrz,tkwat,tkice,tkair
   USE MOD_PhaseChange
   USE MOD_Utils

   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   integer,  intent(in) :: patchtype !land patch type (0=soil, 1=urban and built-up,
                                     !2=wetland, 3=land ice, 4=land water bodies, 99 = ocean)
   integer,  intent(in) :: lb        !lower bound of array
   integer,  intent(in) :: nl_ice    !upper bound of array
   real(r8), intent(in) :: deltim    !seconds in a time step [second]
   real(r8), intent(in) :: capr      !tuning factor to turn first layer T into surface T
   real(r8), intent(in) :: cnfac     !Crank Nicholson factor between 0 and 1

   real(r8), intent(in) :: dz_icesno(lb:nl_ice)   !layer thickness [m]
   real(r8), intent(in) :: z_icesno (lb:nl_ice)   !node depth [m]
   real(r8), intent(in) :: zi_icesno(lb-1:nl_ice) !interface depth [m]

   real(r8), intent(in) :: sabg      !solar radiation absorbed by ground [W/m2]
   real(r8), intent(in) :: forc_frl  !atmospheric infrared (longwave) radiation [W/m2]
   real(r8), intent(in) :: fseng     !sensible heat flux from ground [W/m2]
   real(r8), intent(in) :: fevpg     !evaporation heat flux from ground [mm/s]
   real(r8), intent(in) :: cgrnd     !deriv. of ice energy flux wrt to ice temp [W/m2/k]
   real(r8), intent(in) :: htvp      !latent heat of vapor of water (or sublimation) [J/kg]
   real(r8), intent(in) :: emg       !ground emissivity (0.97 for snow,
   real(r8), intent(in) :: t_precip  !snowfall/rainfall temperature [kelvin]
   real(r8), intent(in) :: pg_rain   !rainfall [kg/(m2 s)]
   real(r8), intent(in) :: pg_snow   !snowfall [kg/(m2 s)]

   real(r8), intent(in) :: sabg_snow_lyr (lb:1)      !snow layer absorption [W/m-2]

   real(r8), intent(inout) :: t_icesno (lb:nl_ice)   !snow and ice temperature [K]
   real(r8), intent(inout) :: wice_icesno(lb:nl_ice) !ice lens [kg/m2]
   real(r8), intent(inout) :: wliq_icesno(lb:nl_ice) !liquid water [kg/m2]
   real(r8), intent(inout) :: scv    !snow cover, water equivalent [mm, kg/m2]
   real(r8), intent(inout) :: snowdp !snow depth [m]

   real(r8), intent(out) :: sm       !rate of snowmelt [kg/(m2 s)]
   real(r8), intent(out) :: xmf      !total latent heat of phase change of ground water
   real(r8), intent(out) :: fact(lb:nl_ice) !used in computing tridiagonal matrix
   integer, intent(out)  :: imelt(lb:nl_ice)    !flag for melting or freezing [-]

   real(r8), intent(out) :: snofrz(lb:0)        !snow freezing rate (lyr) [kg m-2 s-1]

!-------------------------- Local Variables ----------------------------
   real(r8) rhosnow         ! partial density of water (ice + liquid)
   real(r8) cv(lb:nl_ice)   ! heat capacity [J/(m2 K)]
   real(r8) thk(lb:nl_ice)  ! thermal conductivity of layer
   real(r8) tk(lb:nl_ice)   ! thermal conductivity [W/(m K)]

   real(r8) at(lb:nl_ice)   !"a" vector for tridiagonal matrix
   real(r8) bt(lb:nl_ice)   !"b" vector for tridiagonal matrix
   real(r8) ct(lb:nl_ice)   !"c" vector for tridiagonal matrix
   real(r8) rt(lb:nl_ice)   !"r" vector for tridiagonal solution

   real(r8) fn  (lb:nl_ice) ! heat diffusion through the layer interface [W/m2]
   real(r8) fn1 (lb:nl_ice) ! heat diffusion through the layer interface [W/m2]
   real(r8) dzm             ! used in computing tridiagonal matrix
   real(r8) dzp             ! used in computing tridiagonal matrix

   real(r8) t_icesno_bef(lb:nl_ice) ! snow/ice temperature before update
   real(r8) wice_icesno_bef(lb:0)   ! ice lens [kg/m2]
   real(r8) hs              ! net energy flux into the surface (w/m2)
   real(r8) dhsdt           ! d(hs)/dT
   real(r8) brr(lb:nl_ice)  ! temporary set

   integer i,j

   real(r8) :: porsl(1:nl_ice)    ! not used
   real(r8) :: psi0 (1:nl_ice)    ! not used
#ifdef Campbell_SOIL_MODEL
   real(r8) :: bsw(1:nl_ice)      ! not used
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
   real(r8) :: theta_r  (1:nl_ice), &
               alpha_vgm(1:nl_ice), &
               n_vgm    (1:nl_ice), &
               L_vgm    (1:nl_ice), &
               sc_vgm   (1:nl_ice), &
               fc_vgm   (1:nl_ice)
#endif

!-----------------------------------------------------------------------
! SNOW and LAND ICE heat capacity
      cv(1:) = wice_icesno(1:)*cpice + wliq_icesno(1:)*cpliq
      IF(lb==1 .and. scv>0.) cv(1) = cv(1) + cpice*scv

      IF(lb<=0)THEN
         cv(:0) = cpliq*wliq_icesno(:0) + cpice*wice_icesno(:0)
      ENDIF

! SNOW and LAND ICE thermal conductivity [W/(m K)]
      DO j = lb, nl_ice
         thk(j) = tkwat
         IF(t_icesno(j)<=tfrz) thk(j) = 9.828*exp(-0.0057*t_icesno(j))
      ENDDO

      IF(lb < 1)THEN
         DO j = lb, 0
            rhosnow = (wice_icesno(j)+wliq_icesno(j))/dz_icesno(j)

          ! presently option [1] is the default option
          ! [1] Jordan (1991) pp. 18
            thk(j) = tkair+(7.75e-5*rhosnow+1.105e-6*rhosnow*rhosnow)*(tkice-tkair)

          ! [2] Sturm et al (1997)
          ! thk(j) = 0.0138 + 1.01e-3*rhosnow + 3.233e-6*rhosnow**2
          ! [3] Ostin and Andersson presented in Sturm et al., (1997)
          ! thk(j) = -0.871e-2 + 0.439e-3*rhosnow + 1.05e-6*rhosnow**2
          ! [4] Jansson(1901) presented in Sturm et al. (1997)
          ! thk(j) = 0.0293 + 0.7953e-3*rhosnow + 1.512e-12*rhosnow**2
          ! [5] Douville et al., (1995)
          ! thk(j) = 2.2*(rhosnow/denice)**1.88
          ! [6] van Dusen (1992) presented in Sturm et al. (1997)
          ! thk(j) = 0.021 + 0.42e-3*rhosnow + 0.22e-6*rhosnow**2
         ENDDO
      ENDIF

! Thermal conductivity at the layer interface
      DO j = lb, nl_ice-1

! the following consideration is try to avoid the snow conductivity
! to be dominant in the thermal conductivity of the interface.
! Because when the distance of bottom snow node to the interface
! is larger than that of interface to top ice node,
! the snow thermal conductivity will be dominant, and the result is that
! lees heat transfer between snow and ice
         IF((j==0) .and. (z_icesno(j+1)-zi_icesno(j)<zi_icesno(j)-z_icesno(j)))THEN
            tk(j) = 2.*thk(j)*thk(j+1)/(thk(j)+thk(j+1))
            tk(j) = max(0.5*thk(j+1),tk(j))
         ELSE
            tk(j) = thk(j)*thk(j+1)*(z_icesno(j+1)-z_icesno(j)) &
                  /(thk(j)*(z_icesno(j+1)-zi_icesno(j))+thk(j+1)*(zi_icesno(j)-z_icesno(j)))
         ENDIF
      ENDDO
      tk(nl_ice) = 0.


! net ground heat flux into the surface and its temperature derivative
      IF (DEF_USE_SNICAR) THEN
         hs = sabg_snow_lyr(lb) + emg*forc_frl - emg*stefnc*t_icesno(lb)**4 - (fseng+fevpg*htvp) &
            + cpliq * pg_rain * (t_precip - t_icesno(lb)) &
            + cpice * pg_snow * (t_precip - t_icesno(lb))
      ELSE
         hs = sabg + emg*forc_frl - emg*stefnc*t_icesno(lb)**4 - (fseng+fevpg*htvp) &
            + cpliq * pg_rain * (t_precip - t_icesno(lb)) &
            + cpice * pg_snow * (t_precip - t_icesno(lb))
      ENDIF

      dhsdT = - cgrnd - 4.*emg * stefnc * t_icesno(lb)**3 - cpliq * pg_rain - cpice * pg_snow
      t_icesno_bef(lb:) = t_icesno(lb:)

      j       = lb
      fact(j) = deltim / cv(j) * dz_icesno(j) &
              / (0.5*(z_icesno(j)-zi_icesno(j-1)+capr*(z_icesno(j+1)-zi_icesno(j-1))))

      DO j = lb + 1, nl_ice
         fact(j) = deltim/cv(j)
      ENDDO

      DO j = lb, nl_ice - 1
         fn(j) = tk(j)*(t_icesno(j+1)-t_icesno(j))/(z_icesno(j+1)-z_icesno(j))
      ENDDO
      fn(nl_ice) = 0.

! set up vector r and vectors a, b, c that define tridiagonal matrix
      j     = lb
      dzp   = z_icesno(j+1)-z_icesno(j)
      at(j) = 0.
      bt(j) = 1+(1.-cnfac)*fact(j)*tk(j)/dzp-fact(j)*dhsdT
      ct(j) =  -(1.-cnfac)*fact(j)*tk(j)/dzp
      rt(j) = t_icesno(j) + fact(j)*( hs - dhsdT*t_icesno(j) + cnfac*fn(j) )

! Hua Yuan, January 12, 2023
      IF (lb <= 0) THEN
         DO j = lb + 1, 1
            dzm   = (z_icesno(j)-z_icesno(j-1))
            dzp   = (z_icesno(j+1)-z_icesno(j))
            at(j) =   - (1.-cnfac)*fact(j)* tk(j-1)/dzm
            bt(j) = 1.+ (1.-cnfac)*fact(j)*(tk(j)/dzp + tk(j-1)/dzm)
            ct(j) =   - (1.-cnfac)*fact(j)* tk(j)/dzp
            rt(j) = t_icesno(j) + fact(j)*sabg_snow_lyr(j) + cnfac*fact(j)*( fn(j) - fn(j-1) )
         ENDDO
      ENDIF


      DO j = 2, nl_ice - 1
! January 12, 2023
         dzm   = (z_icesno(j)-z_icesno(j-1))
         dzp   = (z_icesno(j+1)-z_icesno(j))
         at(j) =   - (1.-cnfac)*fact(j)* tk(j-1)/dzm
         bt(j) = 1.+ (1.-cnfac)*fact(j)*(tk(j)/dzp + tk(j-1)/dzm)
         ct(j) =   - (1.-cnfac)*fact(j)* tk(j)/dzp
         rt(j) = t_icesno(j) + cnfac*fact(j)*( fn(j) - fn(j-1) )
      ENDDO

      j     =  nl_ice
      dzm   = (z_icesno(j)-z_icesno(j-1))
      at(j) =   - (1.-cnfac)*fact(j)*tk(j-1)/dzm
      bt(j) = 1.+ (1.-cnfac)*fact(j)*tk(j-1)/dzm
      ct(j) = 0.
      rt(j) = t_icesno(j) - cnfac*fact(j)*fn(j-1)

! solve for t_icesno
      i = size(at)
      CALL tridia (i ,at ,bt ,ct ,rt ,t_icesno)

!=======================================================================
! melting or freezing
!=======================================================================

      DO j = lb, nl_ice - 1
         fn1(j) = tk(j)*(t_icesno(j+1)-t_icesno(j))/(z_icesno(j+1)-z_icesno(j))
      ENDDO
      fn1(nl_ice) = 0.

      j = lb
      brr(j) = cnfac*fn(j) + (1.-cnfac)*fn1(j)

      DO j = lb + 1, nl_ice
         brr(j) = cnfac*(fn(j)-fn(j-1)) + (1.-cnfac)*(fn1(j)-fn1(j-1))
      ENDDO

      IF (DEF_USE_SNICAR) THEN

         wice_icesno_bef(lb:0) = wice_icesno(lb:0)

         CALL meltf_snicar (patchtype,.false.,lb,nl_ice,deltim, &
                   !NOTE: compatibility settings for splitting soil&snow temporary input,
                   ! cause glacier patch doesn't support split soil&snow
                   ! hs_soil=hs, hs_snow=hs, fsno=1. not go into effect.
                   fact(lb:),brr(lb:),hs,hs,hs,1.,sabg_snow_lyr(lb:),dhsdT, &
                   t_icesno_bef(lb:),t_icesno(lb:),wliq_icesno(lb:),wice_icesno(lb:),imelt(lb:), &
                   scv,snowdp,sm,xmf,porsl,psi0,&
#ifdef Campbell_SOIL_MODEL
                   bsw,&
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
                   theta_r,alpha_vgm,n_vgm,L_vgm,&
                   sc_vgm,fc_vgm,&
#endif
                   dz_icesno(1:))

         ! layer freezing mass flux (positive):
         DO j = lb, 0
            IF (imelt(j)==2 .and. j<1) THEN
               snofrz(j) = max(0._r8,(wice_icesno(j)-wice_icesno_bef(j)))/deltim
            ENDIF
         ENDDO

      ELSE
         CALL meltf (patchtype,.false.,lb,nl_ice,deltim, &
                   !NOTE: compatibility settings for splitting soil&snow temporary input,
                   ! cause glacier patch doesn't support split soil&snow
                   ! hs_soil=hs, hs_snow=hs, fsno=1. not go into effect.
                   fact(lb:),brr(lb:),hs,hs,hs,1.,dhsdT, &
                   t_icesno_bef(lb:),t_icesno(lb:),wliq_icesno(lb:),wice_icesno(lb:),imelt(lb:), &
                   scv,snowdp,sm,xmf,porsl,psi0,&
#ifdef Campbell_SOIL_MODEL
                   bsw,&
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
                   theta_r,alpha_vgm,n_vgm,L_vgm,&
                   sc_vgm,fc_vgm,&
#endif
                   dz_icesno(1:))
      ENDIF

!-----------------------------------------------------------------------

   END SUBROUTINE groundtem_glacier



   SUBROUTINE GLACIER_WATER (      nl_ice      ,maxsnl    ,deltim    ,&
                      z_icesno    ,dz_icesno   ,zi_icesno ,t_icesno  ,&
                      wliq_icesno ,wice_icesno ,pg_rain   ,pg_snow   ,&
                      sm          ,scv         ,snowdp    ,imelt     ,&
                      fiold       ,snl         ,qseva     ,qsdew     ,&
                      qsubl       ,qfros       ,gwat      ,ssi       ,&
                      wimp        ,forc_us     ,forc_vs               )

!=======================================================================
   USE MOD_Precision
   USE MOD_Const_Physical, only: denice, denh2o, tfrz
   USE MOD_SnowLayersCombineDivide
   USE MOD_SoilSnowHydrology

   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   integer, intent(in) :: nl_ice  ! upper bound of array
   integer, intent(in) :: maxsnl  ! maximum number of snow layers

   real(r8), intent(in) :: &
       deltim    , &! time step (s)
       ssi       , &! irreducible water saturation of snow
       wimp      , &! water impermeable IF porosity less than wimp
       pg_rain   , &! rainfall (mm h2o/s)
       pg_snow   , &! snowfall (mm h2o/s)
       sm        , &! snow melt (mm h2o/s)
       qseva     , &! ground surface evaporation rate (mm h2o/s)
       qsdew     , &! ground surface dew formation (mm h2o /s) [+]
       qsubl     , &! sublimation rate from snow pack (mm h2o /s) [+]
       qfros     , &! surface dew added to snow pack (mm h2o /s) [+]
       fiold(maxsnl+1:nl_ice)  ! fraction of ice relative to the total water

   real(r8), intent(in) :: &
       forc_us,  &
       forc_vs

   ! flag for: melting=1, freezing=2, nothing happened=0
   integer, intent(in) :: imelt(maxsnl+1:nl_ice)
   integer, intent(inout) :: snl       ! lower bound of array

   real(r8), intent(inout) :: &
       z_icesno   (maxsnl+1:nl_ice) , &! layer depth (m)
       dz_icesno  (maxsnl+1:nl_ice) , &! layer thickness (m)
       zi_icesno  (maxsnl  :nl_ice) , &! interface level below a "z" level (m)
       t_icesno   (maxsnl+1:nl_ice) , &! snow/ice skin temperature (K)
       wice_icesno(maxsnl+1:nl_ice) , &! ice lens (kg/m2)
       wliq_icesno(maxsnl+1:nl_ice) , &! liquid water (kg/m2)
       scv                          , &! snow mass (kg/m2)
       snowdp                          ! snow depth (m)

   real(r8), intent(out) :: &
       gwat   ! net water input from top (mm/s)

!-------------------------- Local Variables ----------------------------

   integer lb, j

!=======================================================================
! [1] update the liquid water within snow layer and the water onto the
! ice surface
!
! Snow melting is treated in a realistic fashion, with meltwater
! percolating downward through snow layers as long as the snow is
! unsaturated.  Once the underlying snow is saturated, any additional
! meltwater runs off.  When glacier ice melts, however, the meltwater is
! assumed to remain in place until it refreezes.  In warm parts of the
! ice sheet, the meltwater does not refreeze, but stays in place
! indefinitely.
!=======================================================================

      lb = snl + 1
      IF (lb>=1)THEN
         gwat = pg_rain + sm - qseva
      ELSE
         CALL snowwater (lb,deltim,ssi,wimp,&
                         pg_rain,qseva,qsdew,qsubl,qfros,&
                         dz_icesno(lb:0),wice_icesno(lb:0),wliq_icesno(lb:0),gwat)
      ENDIF

!=======================================================================
! [2] surface runoff and infiltration
!=======================================================================

      IF(snl<0)THEN
         ! Compaction rate for snow
         ! Natural compaction and metamorphosis. The compaction rate
         ! is recalculated for every new timestep
         lb  = snl + 1   ! lower bound of array
         CALL snowcompaction (lb,deltim,&
                         imelt(lb:0),fiold(lb:0),t_icesno(lb:0),&
                         wliq_icesno(lb:0),wice_icesno(lb:0),forc_us,forc_vs,dz_icesno(lb:0))

         ! Combine thin snow elements
         lb = maxsnl + 1
         CALL snowlayerscombine (lb,snl,&
                         z_icesno(lb:1),dz_icesno(lb:1),zi_icesno(lb-1:1),&
                         wliq_icesno(lb:1),wice_icesno(lb:1),t_icesno(lb:1),scv,snowdp)

         ! Divide thick snow elements
         IF(snl<0) &
         CALL snowlayersdivide (lb,snl,&
                         z_icesno(lb:0),dz_icesno(lb:0),zi_icesno(lb-1:0),&
                         wliq_icesno(lb:0),wice_icesno(lb:0),t_icesno(lb:0))
      ENDIF

      IF (snl > maxsnl) THEN
         wice_icesno(maxsnl+1:snl) = 0.
         wliq_icesno(maxsnl+1:snl) = 0.
         t_icesno   (maxsnl+1:snl) = 0.
         z_icesno   (maxsnl+1:snl) = 0.
         dz_icesno  (maxsnl+1:snl) = 0.
      ENDIF

      IF(lb >= 1)THEN
         wliq_icesno(1) = max(1.e-8, wliq_icesno(1) + qsdew * deltim)
         wice_icesno(1) = max(1.e-8, wice_icesno(1) + (qfros-qsubl) * deltim)
      ENDIF

   END SUBROUTINE GLACIER_WATER


   SUBROUTINE GLACIER_WATER_snicar ( nl_ice    ,maxsnl    ,deltim    ,&
                      z_icesno    ,dz_icesno   ,zi_icesno ,t_icesno  ,&
                      wliq_icesno ,wice_icesno ,pg_rain   ,pg_snow   ,&
                      sm          ,scv         ,snowdp    ,imelt     ,&
                      fiold       ,snl         ,qseva     ,qsdew     ,&
                      qsubl       ,qfros       ,gwat      ,ssi       ,&
                      wimp        ,forc_us     ,forc_vs              ,&
                      ! SNICAR
                      forc_aer    ,&
                      mss_bcpho   ,mss_bcphi   ,mss_ocpho ,mss_ocphi ,&
                      mss_dst1    ,mss_dst2    ,mss_dst3  ,mss_dst4   )

!=======================================================================
   USE MOD_Precision
   USE MOD_Const_Physical, only: denice, denh2o, tfrz
   USE MOD_SnowLayersCombineDivide
   USE MOD_SoilSnowHydrology

   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   integer, intent(in) :: nl_ice  ! upper bound of array
   integer, intent(in) :: maxsnl  ! maximum number of snow layers

   real(r8), intent(in) :: &
       deltim    , &! time step (s)
       ssi       , &! irreducible water saturation of snow
       wimp      , &! water impermeable IF porosity less than wimp
       pg_rain   , &! rainfall (mm h2o/s)
       pg_snow   , &! snowfall (mm h2o/s)
       sm        , &! snow melt (mm h2o/s)
       qseva     , &! ground surface evaporation rate (mm h2o/s)
       qsdew     , &! ground surface dew formation (mm h2o /s) [+]
       qsubl     , &! sublimation rate from snow pack (mm h2o /s) [+]
       qfros     , &! surface dew added to snow pack (mm h2o /s) [+]
       fiold(maxsnl+1:nl_ice)  ! fraction of ice relative to the total water

   ! flag for: melting=1, freezing=2, nothing happened=0
   integer, intent(in) :: imelt(maxsnl+1:nl_ice)
   integer, intent(inout) :: snl       ! lower bound of array

   real(r8), intent(inout) :: &
       z_icesno   (maxsnl+1:nl_ice) , &! layer depth (m)
       dz_icesno  (maxsnl+1:nl_ice) , &! layer thickness (m)
       zi_icesno  (maxsnl  :nl_ice) , &! interface level below a "z" level (m)
       t_icesno   (maxsnl+1:nl_ice) , &! snow/ice skin temperature (K)
       wice_icesno(maxsnl+1:nl_ice) , &! ice lens (kg/m2)
       wliq_icesno(maxsnl+1:nl_ice) , &! liquid water (kg/m2)
       scv                          , &! snow mass (kg/m2)
       snowdp                          ! snow depth (m)

   real(r8), intent(out) :: &
       gwat                            ! net water input from top (mm/s)

   real(r8), intent(in) :: forc_us
   real(r8), intent(in) :: forc_vs

! Aerosol Fluxes (Jan. 07, 2023)
   ! aerosol deposition from atmosphere model (grd,aer) [kg m-1 s-1]
   real(r8), intent(in) :: forc_aer ( 14 )

   real(r8), intent(inout) :: &
        mss_bcpho (maxsnl+1:0), &! mass of hydrophobic BC in snow  (lyr) [kg]
        mss_bcphi (maxsnl+1:0), &! mass of hydrophillic BC in snow (lyr) [kg]
        mss_ocpho (maxsnl+1:0), &! mass of hydrophobic OC in snow  (lyr) [kg]
        mss_ocphi (maxsnl+1:0), &! mass of hydrophillic OC in snow (lyr) [kg]
        mss_dst1  (maxsnl+1:0), &! mass of dust species 1 in snow  (lyr) [kg]
        mss_dst2  (maxsnl+1:0), &! mass of dust species 2 in snow  (lyr) [kg]
        mss_dst3  (maxsnl+1:0), &! mass of dust species 3 in snow  (lyr) [kg]
        mss_dst4  (maxsnl+1:0)   ! mass of dust species 4 in snow  (lyr) [kg]
! Aerosol Fluxes (Jan. 07, 2023)

!-------------------------- Local Variables ----------------------------

   integer lb, j

!=======================================================================
!  [1] update the liquid water within snow layer and the water onto the
!  ice surface
!
!  Snow melting is treated in a realistic fashion, with meltwater
!  percolating downward through snow layers as long as the snow is
!  unsaturated.  Once the underlying snow is saturated, any additional
!  meltwater runs off.  When glacier ice melts, however, the meltwater is
!  assumed to remain in place until it refreezes.  In warm parts of the
!  ice sheet, the meltwater does not refreeze, but stays in place
!  indefinitely.
!=======================================================================

      lb = snl + 1
      IF (lb>=1)THEN
         gwat = pg_rain + sm - qseva
      ELSE
         CALL snowwater_snicar (lb,deltim,ssi,wimp,&
                         pg_rain,qseva,qsdew,qsubl,qfros,&
                         dz_icesno(lb:0),wice_icesno(lb:0),wliq_icesno(lb:0),gwat,&
                         forc_aer,&
                         mss_bcpho(lb:0), mss_bcphi(lb:0), mss_ocpho(lb:0), mss_ocphi(lb:0),&
                         mss_dst1(lb:0), mss_dst2(lb:0), mss_dst3(lb:0), mss_dst4(lb:0) )
      ENDIF

!=======================================================================
!  [2] surface runoff and infiltration
!=======================================================================

      IF(snl<0)THEN
         ! Compaction rate for snow
         ! Natural compaction and metamorphosis. The compaction rate
         ! is recalculated for every new timestep
         lb  = snl + 1   ! lower bound of array
         CALL snowcompaction (lb,deltim,&
                         imelt(lb:0),fiold(lb:0),t_icesno(lb:0),&
                         wliq_icesno(lb:0),wice_icesno(lb:0),forc_us,forc_vs,dz_icesno(lb:0))

         ! Combine thin snow elements
         lb = maxsnl + 1
         CALL snowlayerscombine_snicar (lb,snl,&
                         z_icesno(lb:1),dz_icesno(lb:1),zi_icesno(lb-1:1),&
                         wliq_icesno(lb:1),wice_icesno(lb:1),t_icesno(lb:1),scv,snowdp,&
                         mss_bcpho(lb:0), mss_bcphi(lb:0), mss_ocpho(lb:0), mss_ocphi(lb:0),&
                         mss_dst1(lb:0), mss_dst2(lb:0), mss_dst3(lb:0), mss_dst4(lb:0) )

         ! Divide thick snow elements
         IF(snl<0) &
         CALL snowlayersdivide_snicar (lb,snl,&
                         z_icesno(lb:0),dz_icesno(lb:0),zi_icesno(lb-1:0),&
                         wliq_icesno(lb:0),wice_icesno(lb:0),t_icesno(lb:0),&
                         mss_bcpho(lb:0), mss_bcphi(lb:0), mss_ocpho(lb:0), mss_ocphi(lb:0),&
                         mss_dst1(lb:0), mss_dst2(lb:0), mss_dst3(lb:0), mss_dst4(lb:0) )
      ENDIF

      IF (snl > maxsnl) THEN
         wice_icesno(maxsnl+1:snl) = 0.
         wliq_icesno(maxsnl+1:snl) = 0.
         t_icesno   (maxsnl+1:snl) = 0.
         z_icesno   (maxsnl+1:snl) = 0.
         dz_icesno  (maxsnl+1:snl) = 0.
      ENDIF

      IF(lb >= 1)THEN
         wliq_icesno(1) = max(1.e-8, wliq_icesno(1) + qsdew * deltim)
         wice_icesno(1) = max(1.e-8, wice_icesno(1) + (qfros-qsubl) * deltim)
      ENDIF

   END SUBROUTINE GLACIER_WATER_snicar

END MODULE MOD_Glacier
