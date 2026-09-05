#include <define.h>

MODULE MOD_LeafTemperature

!-----------------------------------------------------------------------
   USE MOD_Precision
   USE MOD_Namelist, only: DEF_USE_CBL_HEIGHT, DEF_USE_PLANTHYDRAULICS, DEF_USE_OZONESTRESS, &
                           DEF_RSS_SCHEME, DEF_Interception_scheme, DEF_SPLIT_SOILSNOW, &
                           DEF_VEG_SNOW
   USE MOD_MPAS_MPI

   IMPLICIT NONE

   SAVE

! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: LeafTemperature

! PRIVATE MEMBER FUNCTIONS:
   PRIVATE :: dewfraction
!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------

   SUBROUTINE LeafTemperature ( &
              ipatch     ,ivt        ,deltim     ,csoilc     ,dewmx      ,htvp       ,&
              lai        ,sai        ,htop       ,hbot       ,sqrtdi     ,effcon     ,&
              vmax25     ,c3c4       ,slti       ,hlti       ,shti       ,hhti       ,trda       ,&
              trdm       ,trop       ,g1         ,g0         ,gradm      ,binter     ,&
              extkn      ,extkb      ,extkd      ,hu         ,ht         ,hq         ,&
              us         ,vs         ,thm        ,th         ,thv        ,qm         ,&
              psrf       ,rhoair     ,parsun     ,parsha     ,sabv       ,frl        ,&
              fsun       ,thermk     ,rstfacsun  ,rstfacsha  ,gssun      ,gssha      ,&
              po2m       ,pco2m      ,z0h_g      ,obug       ,ustarg     ,zlnd       ,&
              zsno       ,fsno       ,sigf       ,etrc       ,tg         ,qg         ,&
              rss        ,t_soil     ,t_snow     ,q_soil     ,q_snow     ,dqgdT      ,&
              emg        ,tl         ,ldew       ,ldew_rain  ,ldew_snow  ,fwet_snow  ,&
              taux       ,tauy       ,fseng      ,fseng_soil ,fseng_snow ,fevpg      ,&
              fevpg_soil ,fevpg_snow ,cgrnd      ,cgrndl     ,cgrnds     ,tref       ,&
              qref       ,rst        ,assim      ,respc      ,fsenl      ,fevpl      ,&
              etr        ,dlrad      ,ulrad      ,z0m        ,zol        ,rib        ,&
              ustar      ,qstar      ,tstar      ,fm         ,fh         ,fq         ,&
              fh2m       ,fq2m       ,fm10m      ,&
              rootfr     ,&
!Plant Hydraulic variables
              kmax_sun   ,kmax_sha   ,kmax_xyl   ,kmax_root  ,psi50_sun  ,psi50_sha  ,&
              psi50_xyl  ,psi50_root ,ck         ,vegwp      ,gs0sun     ,gs0sha     ,&
              assimsun   ,etrsun     ,assimsha   ,etrsha     ,&
!Ozone stress variables
              o3coefv_sun,o3coefv_sha,o3coefg_sun,o3coefg_sha,&
              lai_old    ,o3uptakesun,o3uptakesha,forc_ozone ,&
!End ozone stress variables
!WUE stomata model parameter
              lambda     ,&
!End WUE stomata model parameter
              hpbl       ,&
              qintr_rain ,qintr_snow ,t_precip   ,hprl       ,dheatl     ,smp        ,&
              hk         ,hksati     ,rootflux                                        )

!=======================================================================
! !DESCRIPTION:
!  Foliage energy conservation is given by foliage energy budget equation
!                       Rnet - Hf - LEf = 0
!  The equation is solved by Newton-Raphson iteration, in which this
!  iteration includes the calculation of the photosynthesis and stomatal
!  resistance, and the integration of turbulent flux profiles. The
!  sensible and latent heat transfer between foliage and atmosphere and
!  ground is linked by the equations:
!                       Ha = Hf + Hg and Ea = Ef + Eg
!
!  Original author: Yongjiu Dai, August 15, 2001
!
! !REVISIONS:
!
!  09/2014, Hua Yuan: imbalanced energy due to T/q adjustment is
!           allocated to sensible heat flux.
!
!  10/2017, Hua Yuan: added options for z0, displa, rb and rd
!           calculation (Dai, Y., Yuan, H., Xin, Q., Wang, D.,
!           Shangguan, W., Zhang, S., et al. (2019). Different
!           representations of canopy structure—A large source of
!           uncertainty in global land surface modeling. Agricultural
!           and Forest Meteorology, 269-270, 119-135.
!           https://doi.org/10.1016/j.agrformet.2019.02.006
!
!  10/2019, Hua Yuan: change only the leaf temperature from two-leaf
!           to one-leaf (due to large differences may exist between
!           sunlit/shaded leaf temperature.
!
!  01/2021, Xingjie Lu and Nan Wei: added plant hydraulic process
!           interface.
!
!  01/2021, Nan Wei: added interaction btw prec and canopy.
!
!  05/2023, Shaofeng Liu: add option to call moninobuk_leddy, the
!           LargeEddy surface turbulence scheme (LZD2022); make a proper
!           update of um.
!
!  04/2024, Hua Yuan: add option to account for vegetation snow process.
!
!=======================================================================

   USE MOD_Precision
   USE MOD_Vars_Global
   USE MOD_Const_Physical, only: vonkar, grav, hvap, hsub, cpair, stefnc, cpliq, cpice, &
                                 hfus, tfrz, denice, denh2o
   USE MOD_FrictionVelocity
   USE MOD_CanopyLayerProfile
   USE MOD_TurbulenceLEddy
   USE MOD_AssimStomataConductance
   USE MOD_Vars_1DForcing, only: forc_height_mode
   USE MOD_Vars_TimeInvariants, only: patchclass
   USE MOD_Const_LC, only: z0mr, displar
   USE MOD_PlantHydraulic, only:PlantHydraulicStress_twoleaf, getvegwp_twoleaf
   USE MOD_Ozone, only: CalcOzoneStress
   USE MOD_Qsadv

   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------

   integer,  intent(in) :: ipatch,ivt
   real(r8), intent(in) :: &
        deltim,     &! seconds in a time step [second]
        csoilc,     &! drag coefficient for soil under canopy [-]
        dewmx,      &! maximum dew
        htvp         ! latent heat of evaporation (/sublimation) [J/kg]

! vegetation parameters
   real(r8), intent(inout) :: &
        sai          ! stem area index  [-]
   real(r8), intent(in) :: &
        sqrtdi,     &! inverse sqrt of leaf dimension [m**-0.5]
        htop,       &! PFT crown top height [m]
        hbot,       &! PFT crown bot height [m]

        effcon,     &! quantum efficiency of RuBP regeneration (mol CO2 / mol quanta)
        vmax25,     &! maximum carboxylation rate at 25 C at canopy top
                     ! the range : 30.e-6 <-> 100.e-6 (mol co2 m-2 s-1)
        shti,       &! slope of high temperature inhibition function     (s1)
        hhti,       &! 1/2 point of high temperature inhibition function (s2)
        slti,       &! slope of low temperature inhibition function      (s3)
        hlti,       &! 1/2 point of low temperature inhibition function  (s4)
        trda,       &! temperature coefficient in gs-a model             (s5)
        trdm,       &! temperature coefficient in gs-a model             (s6)
        trop,       &! temperature coefficient in gs-a model         (273+25)
        g1,         &! conductance-photosynthesis slope parameter for medlyn model
        g0,         &! conductance-photosynthesis intercept for medlyn model
        gradm,      &! conductance-photosynthesis slope parameter
        binter,     &! conductance-photosynthesis intercept
!Ozone WUE stomata model parameter
        lambda,     &! Marginal water cost of carbon gain ((mol h2o) (mol co2)-1)
!End WUE stomata model parameter
        extkn        ! coefficient of leaf nitrogen allocation
   integer , intent(in) :: &
        c3c4 ! 1 for c3, 0 for c4
   real(r8), intent(in) :: & ! for plant hydraulic scheme
        kmax_sun,   &! Plant Hydraulics Parameters
        kmax_sha,   &! Plant Hydraulics Parameters
        kmax_xyl,   &! Plant Hydraulics Parameters
        kmax_root,  &! Plant Hydraulics Parameters
        psi50_sun,  &! water potential at 50% loss of sunlit leaf tissue conductance (mmH2O)
        psi50_sha,  &! water potential at 50% loss of shaded leaf tissue conductance (mmH2O)
        psi50_xyl,  &! water potential at 50% loss of xylem tissue conductance (mmH2O)
        psi50_root, &! water potential at 50% loss of root tissue conductance (mmH2O)
        ck           ! shape-fitting parameter for vulnerability curve (-)
   real(r8), intent(inout) :: &
        vegwp(1:nvegwcs),&! vegetation water potential
        gs0sun,     &! maximum stomata conductance of sunlit leaf
        gs0sha       ! maximum stomata conductance of shaded leaf

! input variables
   real(r8), intent(in) :: &
        hu,         &! observational height of wind [m]
        ht,         &! observational height of temperature [m]
        hq,         &! observational height of humidity [m]
        us,         &! wind component in eastward direction [m/s]
        vs,         &! wind component in northward direction [m/s]
        thm,        &! intermediate variable (tm+0.0098*ht)
        th,         &! potential temperature (kelvin)
        thv,        &! virtual potential temperature (kelvin)
        qm,         &! specific humidity at reference height [kg/kg]
        psrf,       &! pressure at reference height [pa]
        rhoair,     &! density air [kg/m**3]

        lai,        &! adjusted leaf area index for seasonal variation [-]
        parsun,     &! par absorbed per unit lai [w/m**2]
        parsha,     &! par absorbed per unit lai [w/m**2]
        sabv,       &! solar radiation absorbed by vegetation [W/m2]
        frl,        &! atmospheric infrared (longwave) radiation [W/m2]
        fsun,       &! sunlit fraction of canopy

        extkb,      &! (k, g(mu)/mu) direct solar extinction coefficient
        extkd,      &! diffuse and scattered diffuse PAR extinction coefficient
        thermk,     &! canopy gap fraction for tir radiation

        po2m,       &! atmospheric partial pressure  o2 (pa)
        pco2m,      &! atmospheric partial pressure co2 (pa)

        z0h_g,      &! bare soil roughness length, sensible heat [m]
        obug,       &! bare soil obu
        ustarg,     &! bare soil ustar
        zlnd,       &! roughness length for soil [m]
        zsno,       &! roughness length for snow [m]
        fsno,       &! fraction of snow cover on ground

        sigf,       &! fraction of veg cover, excluding snow-covered veg [-]
        etrc,       &! maximum possible transpiration rate (mm/s)
        tg,         &! ground surface temperature [K]
        t_soil,     &! ground surface soil temperature [K]
        t_snow,     &! ground surface snow temperature [K]
        qg,         &! specific humidity at ground surface [kg/kg]
        q_soil,     &! specific humidity at ground soil surface [kg/kg]
        q_snow,     &! specific humidity at ground snow surface [kg/kg]
        dqgdT,      &! temperature derivative of "qg"
        rss,        &! soil surface resistance [s/m]
        emg          ! vegetation emissivity

   real(r8), intent(in) :: &
        t_precip,   &! snowfall/rainfall temperature [kelvin]
        qintr_rain, &! rainfall interception (mm h2o/s)
        qintr_snow, &! snowfall interception (mm h2o/s)
        smp     (1:nl_soil), &! soil matrix potential
        rootfr  (1:nl_soil), &! root fraction
        hksati  (1:nl_soil), &! hydraulic conductivity at saturation [mm h2o/s]
        hk      (1:nl_soil)   ! soil hydraulic conductance
   real(r8), intent(in) :: &
        hpbl         ! atmospheric boundary layer height [m]

   real(r8), intent(inout) :: &
        tl,         &! leaf temperature [K]
        ldew,       &! depth of water on foliage [mm]
        ldew_rain,  &! depth of rain on foliage [mm]
        ldew_snow    ! depth of snow on foliage [mm]

   real(r8), intent(out) :: &
        fwet_snow    ! vegetation snow fractional cover [-]

   real(r8), intent(inout) :: &
!Ozone stress variables
        lai_old    ,&! lai in last time step
        o3uptakesun,&! Ozone does, sunlit leaf (mmol O3/m^2)
        o3uptakesha,&! Ozone does, shaded leaf (mmol O3/m^2)
        forc_ozone
!End ozone stress variables

   real(r8), intent(out) :: &
        taux,       &! wind stress: E-W [kg/m/s**2]
        tauy,       &! wind stress: N-S [kg/m/s**2]
        fseng,      &! sensible heat flux from ground [W/m2]
        fseng_soil, &! sensible heat flux from ground soil [W/m2]
        fseng_snow, &! sensible heat flux from ground snow [W/m2]
        fevpg,      &! evaporation heat flux from ground [mm/s]
        fevpg_soil, &! evaporation heat flux from ground soil [mm/s]
        fevpg_snow, &! evaporation heat flux from ground snow [mm/s]
        cgrnd,      &! deriv. of soil energy flux wrt to soil temp [w/m2/k]
        cgrndl,     &! deriv, of soil sensible heat flux wrt soil temp [w/m2/k]
        cgrnds,     &! deriv of soil latent heat flux wrt soil temp [w/m**2/k]
        tref,       &! 2 m height air temperature (kelvin)
        qref,       &! 2 m height air specific humidity
        rstfacsun,  &! factor of soil water stress to transpiration on sunlit leaf
        rstfacsha,  &! factor of soil water stress to transpiration on shaded leaf
        gssun,      &! stomata conductance of sunlit leaf
        gssha,      &! stomata conductance of shaded leaf
        rootflux(1:nl_soil)  ! root water uptake from different layers

   real(r8), intent(inout) :: &
        assimsun,   &! sunlit leaf assimilation rate [umol co2 /m**2/ s] [+]
        etrsun,     &! transpiration rate of sunlit leaf [mm/s]
        assimsha,   &! shaded leaf assimilation rate [umol co2 /m**2/ s] [+]
        etrsha       ! transpiration rate of shaded leaf [mm/s]

   real(r8), intent(out) :: &
        rst,        &! stomatal resistance
        assim,      &! rate of assimilation
        respc,      &! rate of respiration
        fsenl,      &! sensible heat from leaves [W/m2]
        fevpl,      &! evaporation+transpiration from leaves [mm/s]
        etr,        &! transpiration rate [mm/s]
        dlrad,      &! downward longwave radiation blow the canopy [W/m2]
        ulrad,      &! upward longwave radiation above the canopy [W/m2]
        hprl,       &! precipitation sensible heat from canopy
        dheatl,     &! vegetation heat change [W/m2]

        z0m,        &! effective roughness [m]
        zol,        &! dimensionless height (z/L) used in Monin-Obukhov theory
        rib,        &! bulk Richardson number in surface layer
        ustar,      &! friction velocity [m/s]
        tstar,      &! temperature scaling parameter
        qstar,      &! moisture scaling parameter
        fm,         &! integral of profile function for momentum
        fh,         &! integral of profile function for heat
        fq,         &! integral of profile function for moisture
        fh2m,       &! relation for temperature at 2m
        fq2m,       &! relation for specific humidity at 2m
        fm10m        ! integral of profile function for momentum at 10m

   real(r8), intent(inout) :: &
!Ozone stress variables
        o3coefv_sun,&! Ozone stress factor for photosynthesis on sunlit leaf
        o3coefv_sha,&! Ozone stress factor for photosynthesis on sunlit leaf
        o3coefg_sun,&! Ozone stress factor for stomata on shaded leaf
        o3coefg_sha  ! Ozone stress factor for stomata on shaded leaf
!End ozone stress variables

!-------------------------- Local Variables ----------------------------
! assign iteration parameters
   integer, parameter :: itmax  = 40   !maximum number of iteration
   integer, parameter :: itmin  = 6    !minimum number of iteration
   real(r8),parameter :: delmax = 3.0  !maximum change in leaf temperature [K]
   real(r8),parameter :: dtmin  = 0.01 !max limit for temperature convergence [K]
   real(r8),parameter :: dlemin = 0.1  !max limit for energy flux convergence [w/m2]

   real(r8) dtl(0:itmax+1)     !difference of tl between two iterative step

   real(r8) :: &
        displa,     &! displacement height [m]
        hu_,        &! adjusted observational height of wind [m]
        ht_,        &! adjusted observational height of temperature [m]
        hq_,        &! adjusted observational height of humidity [m]
        zldis,      &! reference height "minus" zero displacement height [m]
        zii,        &! convective boundary layer height [m]
        z0mv,       &! roughness length, momentum [m]
        z0hv,       &! roughness length, sensible heat [m]
        z0qv,       &! roughness length, latent heat [m]
        zeta,       &! dimensionless height used in Monin-Obukhov theory
        beta,       &! coefficient of convective velocity [-]
        wc,         &! convective velocity [m/s]
        wc2,        &! wc**2
        dth,        &! diff of virtual temp. between ref. height and surface
        dthv,       &! diff of vir. poten. temp. between ref. height and surface
        dqh,        &! diff of humidity between ref. height and surface
        obu,        &! monin-obukhov length (m)
        um,         &! wind speed including the stability effect [m/s]
        ur,         &! wind speed at reference height [m/s]
        uaf,        &! velocity of air within foliage [m/s]
        thvstar,    &! virtual potential temperature scaling parameter
        taf,        &! air temperature within canopy space [K]
        qaf,        &! humidity of canopy air [kg/kg]
        eah,        &! canopy air vapor pressure (pa)
        pco2g,      &! co2 pressure (pa) at ground surface (pa)
        pco2a,      &! canopy air co2 pressure (pa)

        fdry,       &! fraction of foliage that is green and dry [-]
        fwet,       &! fraction of foliage covered by water [-]
        cf,         &! heat transfer coefficient from leaves [-]
        rb,         &! leaf boundary layer resistance [s/m]
        rbsun,      &! Sunlit leaf boundary layer resistance [s/m]
        rbsha,      &! Shaded leaf boundary layer resistance [s/m]
        rd,         &! aerodynamical resistance between ground and canopy air
        ram,        &! aerodynamical resistance [s/m]
        rah,        &! thermal resistance [s/m]
        raw,        &! moisture resistance [s/m]
        clai,       &! canopy heat capacity [Jm-2K-1]
        cah,        &! heat conductance for air [m/s]
        cgh,        &! heat conductance for ground [m/s]
        cfh,        &! heat conductance for leaf [m/s]
        caw,        &! latent heat conductance for air [m/s]
        cgw,        &! latent heat conductance for ground [m/s]
        cfw,        &! latent heat conductance for leaf [m/s]
        wtshi,      &! sensible heat resistance for air, grd and leaf [-]
        wtsqi,      &! latent heat resistance for air, grd and leaf [-]
        wta0,       &! normalized heat conductance for air [-]
        wtg0,       &! normalized heat conductance for ground [-]
        wtl0,       &! normalized heat conductance for air and leaf [-]
        wtaq0,      &! normalized latent heat conductance for air [-]
        wtgq0,      &! normalized heat conductance for ground [-]
        wtlq0,      &! normalized latent heat cond. for air and leaf [-]

        ei,         &! vapor pressure on leaf surface [pa]
        deidT,      &! derivative of "ei" on "tl" [pa/K]
        qsatl,      &! leaf specific humidity [kg/kg]
        qsatldT,    &! derivative of "qsatl" on "tlef"

        del,        &! absolute change in leaf temp in current iteration [K]
        del2,       &! change in leaf temperature in previous iteration [K]
        dele,       &! change in heat fluxes from leaf [W/m2]
        dele2,      &! change in heat fluxes from leaf in previous iteration [W/m2]
        det,        &! maximum leaf temp. change in two consecutive iter [K]
        dee,        &! maximum leaf heat fluxes change in two consecutive iter [W/m2]

        obuold,     &! monin-obukhov length from previous iteration
        tlbef,      &! leaf temperature from previous iteration [K]
        ecidif,     &! excess energies [W/m2]
        err,        &! balance error

        rssun,      &! sunlit leaf stomatal resistance [s/m]
        rssha,      &! shaded leaf stomatal resistance [s/m]
        fsha,       &! shaded fraction of canopy
        laisun,     &! sunlit leaf area index, one-sided
        laisha,     &! shaded leaf area index, one-sided
        respcsun,   &! sunlit leaf respiration rate [umol co2 /m**2/ s] [+]
        respcsha,   &! shaded leaf respiration rate [umol co2 /m**2/ s] [+]
        rsoil,      &! soil respiration
        gah2o,      &! conductance between canopy and atmosphere
        gdh2o,      &! conductance between canopy and ground
        tprcor       ! tf*psur*100./1.013e5

   integer it, nmozsgn

   real(r8) delta, fac
   real(r8) evplwet, evplwet_dtl, etr_dtl, elwmax, elwdif, etr0, sumrootr
   real(r8) irab, dirab_dtl, fsenl_dtl, fevpl_dtl
   real(r8) w, csoilcn, z0mg, cintsun(3), cintsha(3)
   real(r8) fevpl_bef, fevpl_noadj, dtl_noadj, htvpl, erre
   real(r8) qevpl, qdewl, qsubl, qfrol, qmelt, qfrz

   real(r8) lt, egvf

   real(r8) :: sqrtdragc !sqrt(drag coefficient)
   real(r8) :: fai       !canopy frontal area index
   real(r8) :: a_k71     !exponential extinction factor for u/k decline within canopy (Kondo 1971)
   real(r8) :: fqt, fht, fmtop
   real(r8) :: utop, ueff, ktop
   real(r8) :: phih, z0qg, z0hg
   real(r8) :: diag_ustar, diag_fh2m, diag_fq2m, diag_fm, diag_fh, diag_fq
   real(r8) :: profile_obu, profile_um
   real(r8) :: hsink, displasink
   real(r8) gb_mol
   real(r8),dimension(nl_soil) :: k_soil_root    ! radial root and soil conductance
   real(r8),dimension(nl_soil) :: k_ax_root      ! axial root conductance

   integer,  parameter :: zd_opt = 3             ! z0 and d with vertical profile consideration
   integer,  parameter :: rb_opt = 3             ! rb with vertical profile consideration
   integer,  parameter :: rd_opt = 3             ! rd with vertical profile consideration

!-----------------------------------------------------------------------

! initialization of errors and  iteration parameters
      it     = 1    !counter for leaf temperature iteration
      del    = 0.0  !change in leaf temperature from previous iteration
      dele   = 0.0  !latent head flux from leaf for previous iteration

      dtl(0) = 0.
      fevpl_bef = 0.

      fht  = 0.     !integral of profile function for heat
      fqt  = 0.     !integral of profile function for moisture

!-----------------------------------------------------------------------
! scaling-up coefficients from leaf to canopy
!-----------------------------------------------------------------------

      fsha   = 1. -fsun
      laisun = lai*fsun
      laisha = lai*fsha

! scaling-up coefficients from leaf to canopy
      cintsun(1) = (1.-exp(-(0.110+extkb)*lai))/(0.110+extkb)
      cintsun(2) = (1.-exp(-(extkb+extkd)*lai))/(extkb+extkd)
      cintsun(3) = (1.-exp(-extkb*lai))/extkb

      cintsha(1) = (1.-exp(-0.110*lai))/0.110 - cintsun(1)
      cintsha(2) = (1.-exp(-extkd*lai))/extkd - cintsun(2)
      cintsha(3) = lai - cintsun(3)

!-----------------------------------------------------------------------
! get fraction of wet and dry canopy surface (fwet & fdry)
! initial saturated vapor pressure and humidity and their derivation
!-----------------------------------------------------------------------

      !clai = 4.2 * 1000. * 0.2
      clai = 0.0

      ! 0.2mm*LSAI, account for leaf (plus dew) heat capacity
      IF ( DEF_VEG_SNOW ) THEN
         clai = 0.2*(lai+sai)*cpliq + ldew_rain*cpliq + ldew_snow*cpice
      ENDIF

      CALL dewfraction (sigf,lai,sai,dewmx,ldew,ldew_rain,ldew_snow,fwet,fdry)

      CALL qsadv(tl,psrf,ei,deiDT,qsatl,qsatlDT)

!-----------------------------------------------------------------------
! initial for fluxes profile
!-----------------------------------------------------------------------

      nmozsgn = 0    !number of times moz changes sign
      obuold = 0.    !monin-obukhov length from previous iteration
      zii  = 1000.   !m (pbl height)
      beta = 1.      !- (in computing W_*)
      z0mg = (1.-fsno)*zlnd + fsno*zsno
      z0hg = z0mg
      z0qg = z0mg

      z0m    = htop * z0mr(patchclass(ipatch))
      displa = htop * displar(patchclass(ipatch))

      z0mv = z0m; z0hv = z0m; z0qv = z0m

      ! Modify aerodynamic parameters for sparse/dense canopy (X. Zeng)
      lt     = min(lai+sai, 2.)
      egvf   = (1._r8 - exp(-lt)) / (1._r8 - exp(-2.))
      displa = egvf * displa
      z0mv   = exp(egvf * log(z0mv) + (1._r8 - egvf) * log(z0mg))

      z0hv   = z0mv
      z0qv   = z0mv

! 10/17/2017, yuan: z0m and displa with vertical profile solution
      IF (zd_opt == 3) THEN

         CALL cal_z0_displa(lai+sai, htop, 1., z0mv, displa)

         ! NOTE: adjusted for small displa
         displasink = max(htop/2., displa)
         hsink = z0mv + displasink

         z0hv   = z0mv
         z0qv   = z0mv

      ENDIF

      fai    = 1. - exp(-0.5*(lai+sai))
      sqrtdragc = min( (0.003+0.3*fai)**0.5, 0.3 )

      a_k71 = htop/(htop-displa)/(vonkar/sqrtdragc)

      taf = 0.5 * (tg + thm)
      qaf = 0.5 * (qm + qg)

      pco2a = pco2m
      tprcor = 44.6*273.16*psrf/1.013e5
      rsoil = 0.  !respiration (mol m-2 s-1)
!     rsoil = 1.22e-6*exp(308.56*(1./56.02-1./(tg-227.13)))
!     rsoil = rstfac * 0.23 * 15. * 2.**((tg-273.16-10.)/10.) * 1.e-6
!     rsoil = 5.22 * 1.e-6
      rsoil = 0.22 * 1.e-6

      ur  = max(0.1, sqrt(us*us+vs*vs))    !limit set to 0.1
      dth = thm - taf
      dqh = qm  - qaf
      dthv  = dth*(1.+0.61*qm) + 0.61*th*dqh

      hu_ = hu; ht_ = ht; hq_ = hq;

      IF (forc_height_mode == 'absolute') THEN

#ifdef MPAS_EMBEDDED_COLM
         IF (hu <= htop+1 .or. ht <= htop+1 .or. hq <= htop+1) THEN
            CALL CoLM_stop('MPAS embedded CoLM requires the MPAS lowest atmospheric level '// &
                           'to be more than 1 m above the CoLM canopy top.')
         ENDIF
#else
         IF (hu <= htop+1) THEN
            hu_ = htop + 1.
            IF (taux == spval) & ! only print warning for the first time-step
               write(6,*) 'Warning: the obs height of u less than htop+1, set it to htop+1.'
         ENDIF

         IF (ht <= htop+1) THEN
            ht_ = htop + 1.
            IF (taux == spval) & ! only print warning for the first time-step
               write(6,*) 'Warning: the obs height of t less than htop+1, set it to htop+1.'
         ENDIF

         IF (hq <= htop+1) THEN
            hq_ = htop + 1.
            IF (taux == spval) & ! only print warning for the first time-step
               write(6,*) 'Warning: the obs height of q less than htop+1, set it to htop+1.'
         ENDIF
#endif

      ELSE ! relative height
         hu_ = htop + hu
         ht_ = htop + ht
         hq_ = htop + hq
      ENDIF

      zldis = hu_ - displa

      IF(zldis <= 0.0) THEN
         write(6,*) 'the obs height of u less than the zero displacement heght'
         CALL CoLM_stop()
      ENDIF

      CALL moninobukini(ur,th,thm,thv,dth,dqh,dthv,zldis,z0mv,um,obu)

! ======================================================================
!     BEGIN stability iteration
! ======================================================================

      DO WHILE (it .le. itmax)

         tlbef = tl

         del2  = del
         dele2 = dele

         IF (tl > tfrz) THEN
            htvpl = hvap
         ELSE
            htvpl = hsub
         ENDIF

!-----------------------------------------------------------------------
! Aerodynamical resistances
!-----------------------------------------------------------------------
! Evaluate stability-dependent variables using moz from prior iteration
         profile_obu = obu
         profile_um = um
         IF (rd_opt == 3) THEN
            IF (DEF_USE_CBL_HEIGHT) THEN
              CALL moninobukm_leddy(hu_,ht_,hq_,displa,z0mv,z0hv,z0qv,obu,um, &
                                    displasink,z0mv,hpbl,ustar,fh2m,fq2m, &
                                    htop,fmtop,fm,fh,fq,fht,fqt,phih)
            ELSE
              CALL moninobukm(hu_,ht_,hq_,displa,z0mv,z0hv,z0qv,obu,um, &
                              displasink,z0mv,ustar,fh2m,fq2m, &
                              htop,fmtop,fm,fh,fq,fht,fqt,phih)
            ENDIF
            ! Aerodynamic resistance
            ram = 1./(ustar*ustar/um)
            rah = 1./(vonkar/(fh-fht)*ustar)
            raw = 1./(vonkar/(fq-fqt)*ustar)
         ELSE
            IF (DEF_USE_CBL_HEIGHT) THEN
               CALL moninobuk_leddy(hu_,ht_,hq_,displa,z0mv,z0hv,z0qv,obu,um,hpbl, &
                                    ustar,fh2m,fq2m,fm10m,fm,fh,fq)
            ELSE
               CALL moninobuk(hu_,ht_,hq_,displa,z0mv,z0hv,z0qv,obu,um,&
                              ustar,fh2m,fq2m,fm10m,fm,fh,fq)
            ENDIF
            ! Aerodynamic resistance
            ram = 1./(ustar*ustar/um)
            rah = 1./(vonkar/fh*ustar)
            raw = 1./(vonkar/fq*ustar)
         ENDIF

         z0hg = z0mg/exp(0.13 * (ustar*z0mg/1.5e-5)**0.45)
         z0qg = z0hg

! Bulk boundary layer resistance of leaves
          uaf = ustar
          cf = 0.01*sqrtdi/sqrt(uaf)
          rb = 1/(cf*uaf)

! 11/17/2017, yuan: 3D rb calculation (with vertical profile consideration)
! 03/13/2020, yuan: added analytical solution
         IF (rb_opt == 3) THEN
            utop = ustar/vonkar * fmtop
            ueff = ueffect(utop, htop, z0mg, z0mg, a_k71, 1._r8, 1._r8)
            cf = 0.01*sqrtdi*sqrt(ueff)
            rb = 1./cf
         ENDIF

!        rd = 1./(csoilc*uaf)                 ! BATS legacy
!        w = exp(-0.5*(lai+sai))              ! Dickinson's modification :
!        csoilc = ( 1.-w + w*um/uaf)/rah      ! "rah" here is the resistance over
!        rd = 1./(csoilc*uaf)                 ! bare ground fraction

! modified by Xubin Zeng's suggestion at 08-07-2002
         w = exp(-(lai+sai))
         csoilcn = (vonkar/(0.13*(z0mg*uaf/1.5e-5)**0.45))*w + csoilc*(1.-w)
         rd = 1./(csoilcn*uaf)

! 11/17/2017, yuan: 3D rd calculation with vertical profile solution
! 03/13/2020, yuan: added analytical solution
         IF (rd_opt == 3) THEN
            ktop = vonkar * (htop-displa) * ustar / phih
            rd = frd(ktop, htop, z0qg, hsink, z0qg, displa/htop, &
               z0qg, obug, ustar, z0mg, a_k71, 1._r8, 1._r8)
         ENDIF

!-----------------------------------------------------------------------
! stomatal resistances
!-----------------------------------------------------------------------

         IF(lai .gt. 0.001) THEN

            eah = qaf * psrf / ( 0.622 + 0.378 * qaf )    !pa

            ! If use PHS, calculate maximum stomata conductance (minimum stomata resistance)
            ! by setting rstfac = 1. (no water stress). When use PHS, stomata only calculate
            ! non-stress stomata conductance, assimilation rate and leaf respiration
            IF (DEF_USE_PLANTHYDRAULICS) THEN
                 rstfacsun = 1.
                 rstfacsha = 1.
            ENDIF

            ! leaf to canopy level
            rbsun = rb / laisun
            rbsha = rb / laisha

            ! Sunlit leaves
            CALL stomata  (vmax25   ,effcon   ,c3c4     ,slti     ,hlti     ,&
                 shti     ,hhti     ,trda     ,trdm     ,trop     ,&
                 g1       ,g0       ,gradm    ,binter   ,thm      ,&
                 psrf     ,po2m     ,pco2m    ,pco2a    ,eah      ,&
                 ei       ,tl       ,parsun   ,&
            !Ozone stress variables
                 o3coefv_sun   ,o3coefg_sun   ,&
            !End ozone stress variables
            !Ozone WUE stomata model parameter
                 lambda   ,&
            !End WUE stomata model parameter
                 rbsun    ,raw      ,rstfacsun,cintsun  ,&
                 assimsun ,respcsun ,rssun    )

            ! Shaded leaves
            CALL stomata  (vmax25   ,effcon   ,c3c4     ,slti     ,hlti     ,&
                 shti     ,hhti     ,trda     ,trdm     ,trop     ,&
                 g1       ,g0       ,gradm    ,binter   ,thm      ,&
                 psrf     ,po2m     ,pco2m    ,pco2a    ,eah      ,&
                 ei       ,tl       ,parsha   ,&
            ! Ozone stress variables
                 o3coefv_sha    ,o3coefg_sha  ,&
            ! End ozone stress variables
            ! Ozone WUE stomata model parameter
                 lambda   ,&
            ! End WUE stomata model parameter
                 rbsha    ,raw      ,rstfacsha,cintsha  ,&
                 assimsha ,respcsha ,rssha    )

            IF (DEF_USE_PLANTHYDRAULICS) THEN

               gs0sun = min( 1.e6, 1./(rssun*tl/tprcor) )/ laisun * 1.e6 * o3coefg_sun
               gs0sha = min( 1.e6, 1./(rssha*tl/tprcor) )/ laisha * 1.e6 * o3coefg_sha

               sai = amax1(sai,0.1)
               ! PHS update actual stomata conductance (resistance), assimilation rate
               ! and leaf respiration. above stomatal resistances are for the canopy,
               ! the stomatal resistances and the "rb" in the following calculations are
               ! the average for single leaf. thus,
               CALL PlantHydraulicStress_twoleaf (       nl_soil    ,nvegwcs    ,&
                     z_soi      ,dz_soi     ,rootfr     ,psrf       ,qsatl      ,&
                     qaf        ,tl         ,rb         ,rss        ,raw        ,&
                     rd         ,rstfacsun  ,rstfacsha  ,cintsun    ,cintsha    ,&
                     laisun     ,laisha     ,rhoair     ,fwet       ,sai        ,&
                     kmax_sun   ,kmax_sha   ,kmax_xyl   ,kmax_root  ,psi50_sun  ,&
                     psi50_sha  ,psi50_xyl  ,psi50_root ,htop       ,ck         ,&
                     smp        ,hk         ,hksati     ,vegwp      ,etrsun     ,&
                     etrsha     ,rootflux   ,qg         ,qm         ,gs0sun     ,&
                     gs0sha     ,k_soil_root,k_ax_root  ,gssun      ,gssha       )

               etr  = etrsun + etrsha
               gssun = gssun * laisun
               gssha = gssha * laisha

               CALL update_photosyn(tl, po2m, pco2m, pco2a, parsun, psrf, rstfacsun, rb, gssun, &
                                    effcon, vmax25, c3c4, gradm, trop, slti, hlti, shti, hhti, trda, &
                                    trdm, cintsun, assimsun, respcsun)

               CALL update_photosyn(tl, po2m, pco2m, pco2a, parsha, psrf, rstfacsha, rb, gssha, &
                                    effcon, vmax25, c3c4, gradm, trop, slti, hlti, shti, hhti, trda, &
                                    trdm, cintsha, assimsha, respcsha)

               rssun = tprcor/tl * 1.e6 / gssun
               rssha = tprcor/tl * 1.e6 / gssha
            ENDIF

         ELSE
            rssun = 2.e20; assimsun = 0.; respcsun = 0.
            rssha = 2.e20; assimsha = 0.; respcsha = 0.
            gssun = 0._r8
            gssha = 0._r8

            ! 07/2023, yuan: a bug for imbalanced water, rootflux only change
            ! in DEF_USE_PLANTHYDRAULICS case in this routine.
            IF (DEF_USE_PLANTHYDRAULICS) THEN
               etr    = 0.
               etrsun = 0._r8
               etrsha = 0._r8
               rootflux = 0.
            ENDIF
         ENDIF

! above stomatal resistances are for the canopy, the stomatal resistances
! and the "rb" in the following calculations are the average for single leaf. thus,
         rssun = rssun * laisun
         rssha = rssha * laisha

!-----------------------------------------------------------------------
! dimensional and non-dimensional sensible and latent heat conductances
! for canopy and soil flux calculations.
!-----------------------------------------------------------------------

         delta = 0.0
         IF(qsatl-qaf .gt. 0.) delta = 1.0

         cah = 1. / rah
         cgh = 1. / rd
         cfh = (lai + sai) / rb

         caw = 1. / raw
         IF (qg < qaf) THEN
            cgw = 1. / rd !dew case. no soil resistance
         ELSE
            IF (DEF_RSS_SCHEME .eq. 4) THEN
               cgw = rss / rd
            ELSE
               cgw = 1. / (rd + rss)
            ENDIF
         ENDIF
         cfw = (1.-delta*(1.-fwet))*(lai+sai)/rb + (1.-fwet)*delta* &
               ( laisun/(rb+rssun) + laisha/(rb+rssha) )

         wtshi = 1. / ( cah + cgh + cfh )
         wtsqi = 1. / ( caw + cgw + cfw )

         wta0 = cah * wtshi
         wtg0 = cgh * wtshi
         wtl0 = cfh * wtshi

         wtaq0 = caw * wtsqi
         wtgq0 = cgw * wtsqi
         wtlq0 = cfw * wtsqi

!-----------------------------------------------------------------------
! IR radiation, sensible and latent heat fluxes and their derivatives
!-----------------------------------------------------------------------
! the partial derivatives of areodynamical resistance are ignored
! which cannot be determined analytically
         fac = 1. - thermk

! longwave absorption and their derivatives
         ! 10/16/2017, yuan: added reflected longwave by the ground

IF (.not.DEF_SPLIT_SOILSNOW) THEN
         irab = (frl - 2. * stefnc * tl**4 + emg*stefnc*tg**4 ) * fac &
              + (1-emg)*thermk*fac*frl + (1-emg)*(1-thermk)*fac*stefnc*tl**4
ELSE
         irab = (frl - 2. * stefnc * tl**4 &
              + (1.-fsno)*emg*stefnc*t_soil**4 &
              + fsno*emg*stefnc*t_snow**4 ) * fac &
              + (1-emg)*thermk*fac*frl + (1-emg)*(1-thermk)*fac*stefnc*tl**4
ENDIF
         dirab_dtl = - 8. * stefnc * tl**3 * fac &
                   + 4.*(1-emg)*(1-thermk)*fac*stefnc*tl**3

! sensible heat fluxes and their derivatives
         fsenl = rhoair * cpair * cfh * ( (wta0 + wtg0)*tl - wta0*thm - wtg0*tg )
         fsenl_dtl = rhoair * cpair * cfh * (wta0 + wtg0)

! latent heat fluxes and their derivatives

         etr = rhoair * (1.-fwet) * delta &
             * ( laisun/(rb+rssun) + laisha/(rb+rssha) ) &
             * ( (wtaq0 + wtgq0)*qsatl - wtaq0*qm - wtgq0*qg )

         etrsun = rhoair * (1.-fwet) * delta &
             * ( laisun/(rb+rssun) ) * ( (wtaq0 + wtgq0)*qsatl - wtaq0*qm - wtgq0*qg )
         etrsha = rhoair * (1.-fwet) * delta &
             * ( laisha/(rb+rssha) ) * ( (wtaq0 + wtgq0)*qsatl - wtaq0*qm - wtgq0*qg )

         etr_dtl = rhoair * (1.-fwet) * delta &
                 * ( laisun/(rb+rssun) + laisha/(rb+rssha) ) &
                 * (wtaq0 + wtgq0)*qsatlDT

         IF (.not. DEF_USE_PLANTHYDRAULICS) THEN
            IF(etr.ge.etrc)THEN
               etr = etrc
               etr_dtl = 0.
            ENDIF
         ELSE
            IF(rstfacsun .lt. 1.e-2 .or. etrsun .le. 0.)etrsun = 0._r8
            IF(rstfacsha .lt. 1.e-2 .or. etrsha .le. 0.)etrsha = 0._r8
            etr = etrsun + etrsha
            IF(abs(etr - sum(rootflux)) .gt. 1.e-7)THEN
               write(6,*) 'Warning: water balance violation in vegetation PHS', &
                  ipatch,mpas_rank, etr, sum(rootflux), abs(etr-sum(rootflux))
               CALL CoLM_stop()
            ENDIF
         ENDIF

         evplwet = rhoair * (1.-delta*(1.-fwet)) * (lai+sai) / rb &
                 * ( (wtaq0 + wtgq0)*qsatl - wtaq0*qm - wtgq0*qg )
         evplwet_dtl = rhoair * (1.-delta*(1.-fwet)) * (lai+sai) / rb &
                     * (wtaq0 + wtgq0)*qsatlDT

         IF(evplwet.ge.ldew/deltim)THEN
            evplwet = ldew/deltim
            evplwet_dtl = 0.
         ENDIF

         fevpl = etr + evplwet
         fevpl_dtl = etr_dtl + evplwet_dtl

         ! 07/09/2014, yuan: added for energy balance
         erre = 0.
         fevpl_noadj = fevpl
         IF ( fevpl*fevpl_bef < 0. ) THEN
            erre  = -0.9*fevpl
            fevpl =  0.1*fevpl
         ENDIF

!-----------------------------------------------------------------------
! difference of temperatures by quasi-newton-raphson method for the non-linear system equations
! MARK#dtl
!-----------------------------------------------------------------------

         dtl(it) = (sabv + irab - fsenl - hvap*fevpl &
                 + cpliq*qintr_rain*(t_precip-tl) + cpice*qintr_snow*(t_precip-tl)) &
                 / (clai/deltim - dirab_dtl + fsenl_dtl + hvap*fevpl_dtl  &
                 + cpliq*qintr_rain + cpice*qintr_snow)

         dtl_noadj = dtl(it)

         ! check magnitude of change in leaf temperature limit to maximum allowed value

         ! 06/12/2014, yuan: .lt. -> .le.
         IF(it .le. itmax) THEN

         ! put brakes on large temperature excursions
           IF(abs(dtl(it)).gt.delmax)THEN
               dtl(it) = delmax*dtl(it)/abs(dtl(it))
           ENDIF

         ! 06/12/2014, yuan: .lt. -> .le.
         ! NOTE: could be a bug IF dtl*dtl==0, changed from lt->le
           IF((it.ge.2) .and. (dtl(it-1)*dtl(it).le.0.))THEN
               dtl(it) = 0.5*(dtl(it-1) + dtl(it))
           ENDIF

         ENDIF

         tl = tlbef + dtl(it)

!-----------------------------------------------------------------------
! square roots differences of temperatures and fluxes for use as the condition of convergences
!-----------------------------------------------------------------------

         del  = sqrt( dtl(it)*dtl(it) )
         dele = dtl(it) * dtl(it) * ( dirab_dtl**2 + fsenl_dtl**2 + (hvap*fevpl_dtl)**2 )
         dele = sqrt(dele)

!-----------------------------------------------------------------------
!  saturated vapor pressures and canopy air temperature, canopy air humidity
!-----------------------------------------------------------------------
! Recalculate leaf saturated vapor pressure (ei_)for updated leaf temperature
! and adjust specific humidity (qsatl_) proportionately
         CALL qsadv(tl,psrf,ei,deiDT,qsatl,qsatlDT)

! update vegetation/ground surface temperature, canopy air temperature,
! canopy air humidity
         taf = wta0*thm + wtg0*tg + wtl0*tl
         qaf = wtaq0*qm + wtgq0*qg + wtlq0*qsatl

! update co2 partial pressure within canopy air
         gah2o = 1.0/raw * tprcor/thm                     !mol m-2 s-1
         IF (DEF_RSS_SCHEME .eq. 4) THEN
             gdh2o = rss/rd  * tprcor/thm                 !mol m-2 s-1
         ELSE
             gdh2o = 1.0/(rd+rss)  * tprcor/thm           !mol m-2 s-1
         ENDIF
         pco2a = pco2m - 1.37*psrf/max(0.446,gah2o) * &
                (assimsun + assimsha  - respcsun -respcsha - rsoil)

!-----------------------------------------------------------------------
! Update monin-obukhov length and wind speed including the stability effect
!-----------------------------------------------------------------------

         dth = thm - taf
         dqh = qm  - qaf

         tstar = vonkar/(fh-fht)*dth
         qstar = vonkar/(fq-fqt)*dqh

         thvstar = tstar*(1.+0.61*qm)+0.61*th*qstar
         zeta = zldis*vonkar*grav*thvstar / (ustar**2*thv)
         IF(zeta .ge. 0.)THEN                             !stable
            zeta = min(2.,max(zeta,1.e-6))
         ELSE                                             !unstable
            zeta = max(-100.,min(zeta,-1.e-6))
         ENDIF
         obu = zldis/zeta

         IF(zeta .ge. 0.)THEN
           um = max(ur,.1)
         ELSE
           IF (DEF_USE_CBL_HEIGHT) THEN !//TODO: Shaofeng, 2023.05.18
             zii = max(5.*hu_,hpbl)
           ENDIF !//TODO: Shaofeng, 2023.05.18
           wc = (-grav*ustar*thvstar*zii/thv)**(1./3.)
          wc2 = beta*beta*(wc*wc)
           um = sqrt(ur*ur+wc2)
         ENDIF

         IF(obuold*obu .lt. 0.) nmozsgn = nmozsgn+1
         IF(nmozsgn .ge. 4) obu = zldis/(-0.01)
         obuold = obu

!-----------------------------------------------------------------------
! Test for convergence
!-----------------------------------------------------------------------

         it = it+1

         IF(it .gt. itmin) THEN
            fevpl_bef = fevpl
            det = max(del,del2)
            ! 10/03/2017, yuan: possible bugs here, solution:
            ! define dee, change del => dee
            dee = max(dele,dele2)
            IF(det .lt. dtmin .and. dee .lt. dlemin) EXIT
         ENDIF

      ENDDO

      IF(DEF_USE_OZONESTRESS)THEN
         CALL CalcOzoneStress(o3coefv_sun,o3coefg_sun,forc_ozone,psrf,th,ram,&
                              rssun,rb,lai,lai_old,ivt,o3uptakesun,sabv,deltim)
         CALL CalcOzoneStress(o3coefv_sha,o3coefg_sha,forc_ozone,psrf,th,ram,&
                              rssha,rb,lai,lai_old,ivt,o3uptakesha,sabv,deltim)
         lai_old  = lai
         assimsun = assimsun * o3coefv_sun
         assimsha = assimsha * o3coefv_sha
!         rssun    = rssun / o3coefg_sun
!         rssha    = rssha / o3coefg_sha
      ELSE
         o3coefv_sun = 1.0_r8
         o3coefg_sun = 1.0_r8
         o3coefv_sha = 1.0_r8
         o3coefg_sha = 1.0_r8
      ENDIF

! ======================================================================
!     END stability iteration
! ======================================================================

      z0m = z0mv
      zol = zeta
      rib = min(5.,zol*ustar**2/(vonkar**2/fh*um**2))

! canopy fluxes and total assimilation amd respiration

      IF(lai .gt. 0.001) THEN
         rst = 1./(laisun/rssun + laisha/rssha)
      ELSE
         rssun = 2.0e4 ; rssha = 2.0e4
         assimsun = 0. ; assimsha = 0.
         respcsun = 0. ; respcsha = 0.
         rst = 2.0e4
      ENDIF
      assim = assimsun + assimsha
      respc = respcsun + respcsha! + rsoil

! canopy fluxes and total assimilation amd respiration
      fsenl = fsenl + fsenl_dtl*dtl(it-1) &
            ! yuan: add the imbalanced energy below due to T adjustment to sensible heat
            + (dtl_noadj-dtl(it-1)) * (clai/deltim - dirab_dtl + fsenl_dtl + hvap*fevpl_dtl &
            + cpliq * qintr_rain + cpice * qintr_snow) &
            ! yuan: add the imbalanced energy below due to q adjustment to sensible heat
            + hvap*erre

      etr0  = etr
      etr   = etr + etr_dtl*dtl(it-1)

      IF (DEF_USE_PLANTHYDRAULICS) THEN
         ! Apply the final leaf-temperature correction to total
         ! transpiration without discarding layer-to-layer hydraulic
         ! redistribution in rootflux.
         IF (abs(etr0) .ge. 1.e-15) THEN
             rootflux = rootflux * etr / etr0
         ELSE
             rootflux = rootflux + dz_soi / sum(dz_soi) * etr_dtl* dtl(it-1)
         ENDIF
      ENDIF

      evplwet = evplwet + evplwet_dtl*dtl(it-1)
      fevpl   = fevpl_noadj
      fevpl   = fevpl   +   fevpl_dtl*dtl(it-1)

      elwmax  = ldew/deltim
      elwdif  = max(0., evplwet-elwmax)
      evplwet = min(evplwet, elwmax)

      fevpl = fevpl - elwdif
      fsenl = fsenl + hvap*elwdif

      taux = - rhoair*us/ram
      tauy = - rhoair*vs/ram

!-----------------------------------------------------------------------
! fluxes from ground to canopy space
!-----------------------------------------------------------------------

      fseng = cpair*rhoair*cgh*(tg-taf)
! 03/07/2020, yuan: calculate fseng_soil/snow
      !NOTE: taf = wta0*thm + wtg0*tg + wtl0*tl
      fseng_soil = cpair*rhoair*cgh*((1.-wtg0)*t_soil - wta0*thm - wtl0*tl)
      fseng_snow = cpair*rhoair*cgh*((1.-wtg0)*t_snow - wta0*thm - wtl0*tl)

! 03/07/2020, yuan: calculate fevpg_soil/snow
      !NOTE: qaf = wtaq0*qm + wtgq0*qg + wtlq0*qsatl
      fevpg = rhoair*cgw*(qg-qaf)
      fevpg_soil = rhoair*cgw*((1.-wtgq0)*q_soil - wtaq0*qm - wtlq0*qsatl)
      fevpg_snow = rhoair*cgw*((1.-wtgq0)*q_snow - wtaq0*qm - wtlq0*qsatl)

!-----------------------------------------------------------------------
! downward (upward) longwave radiation below (above) the canopy and prec. sensible heat
!-----------------------------------------------------------------------

      ! 10/16/2017, yuan: added reflected longwave by the ground
      dlrad = thermk * frl &
            + stefnc * fac * tlbef**3 * (tlbef + 4.*dtl(it-1))

IF (.not.DEF_SPLIT_SOILSNOW) THEN
      ulrad = stefnc * ( fac * tlbef**3 * (tlbef + 4.*dtl(it-1)) &
            + thermk*emg*tg**4 ) &
            + (1-emg)*thermk*thermk*frl &
            + (1-emg)*thermk*fac*stefnc*tlbef**4 &
            + 4.*(1-emg)*thermk*fac*stefnc*tlbef**3*dtl(it-1)
ELSE
      ulrad = stefnc * ( fac * tlbef**3 * (tlbef + 4.*dtl(it-1)) &
            + (1.-fsno)*thermk*emg*t_soil**4 &
            + fsno*thermk*emg*t_snow**4 ) &
            + (1-emg)*thermk*thermk*frl &
            + (1-emg)*thermk*fac*stefnc*tlbef**4 &
            + 4.*(1-emg)*thermk*fac*stefnc*tlbef**3*dtl(it-1)
ENDIF
      ! precipitation sensible heat from canopy
      hprl   = cpliq * qintr_rain*(t_precip-tl) + cpice * qintr_snow*(t_precip-tl)

      ! vegetation heat change
      dheatl = clai/deltim*dtl(it-1)

!-----------------------------------------------------------------------
! Derivative of soil energy flux with respect to soil temperature (cgrnd)
!-----------------------------------------------------------------------

      cgrnds = cpair*rhoair*cgh*(1.-wtg0)
      cgrndl = rhoair*cgw*(1.-wtgq0)*dqgdT
      cgrnd  = cgrnds + cgrndl*htvp

!-----------------------------------------------------------------------
! balance check
! (the computational error was created by the assumed 'dtl' in MARK#dtl)
!-----------------------------------------------------------------------

      err = sabv + irab + dirab_dtl*dtl(it-1) - fsenl - hvap*fevpl + hprl &
          ! account for vegetation heat change
          - dheatl

#if (defined CoLMDEBUG)
      IF(abs(err) .gt. .2) &
      write(6,*) 'energy imbalance in LeafTemperature.F90',it-1,&
         err,sabv,irab,fsenl,hvap*fevpl,hprl,dheatl
#endif

!-----------------------------------------------------------------------
! Update dew accumulation (kg/m2)
!-----------------------------------------------------------------------
      IF (DEF_Interception_scheme .eq. 1) THEN
         ldew = max(0., ldew-evplwet*deltim)

         ! account for vegetation snow and update ldew_rain, ldew_snow, ldew
         IF ( DEF_VEG_SNOW ) THEN
            IF (tl > tfrz) THEN
               qevpl = max (evplwet, 0.)
               qdewl = abs (min (evplwet, 0.) )
               qsubl = 0.
               qfrol = 0.

               IF (qevpl > ldew_rain/deltim) THEN
                  qsubl = qevpl - ldew_rain/deltim
                  qevpl = ldew_rain/deltim
               ENDIF
            ELSE
               qevpl = 0.
               qdewl = 0.
               qsubl = max (evplwet, 0.)
               qfrol = abs (min (evplwet, 0.) )

               IF (qsubl > ldew_snow/deltim) THEN
                  qevpl = qsubl - ldew_snow/deltim
                  qsubl = ldew_snow/deltim
               ENDIF
            ENDIF

            ldew_rain = ldew_rain + (qdewl-qevpl)*deltim
            ldew_snow = ldew_snow + (qfrol-qsubl)*deltim

            ldew = ldew_rain + ldew_snow
         ENDIF

      ELSEIF (DEF_Interception_scheme .eq. 2) THEN !CLM4.5
         ldew = max(0., ldew-evplwet*deltim)

         ! account for vegetation snow and update ldew_rain, ldew_snow, ldew
         IF ( DEF_VEG_SNOW ) THEN
            IF (tl > tfrz) THEN
               qevpl = max (evplwet, 0.)
               qdewl = abs (min (evplwet, 0.) )
               qsubl = 0.
               qfrol = 0.

               IF (qevpl > ldew_rain/deltim) THEN
                  qsubl = qevpl - ldew_rain/deltim
                  qevpl = ldew_rain/deltim
               ENDIF
            ELSE
               qevpl = 0.
               qdewl = 0.
               qsubl = max (evplwet, 0.)
               qfrol = abs (min (evplwet, 0.) )

               IF (qsubl > ldew_snow/deltim) THEN
                  qevpl = qsubl - ldew_snow/deltim
                  qsubl = ldew_snow/deltim
               ENDIF
            ENDIF

            ldew_rain = ldew_rain + (qdewl-qevpl)*deltim
            ldew_snow = ldew_snow + (qfrol-qsubl)*deltim

            ldew = ldew_rain + ldew_snow
         ENDIF

      ELSEIF (DEF_Interception_scheme .eq. 3) THEN !CLM5
         ldew = max(0., ldew-evplwet*deltim)

         ! account for vegetation snow and update ldew_rain, ldew_snow, ldew
         IF ( DEF_VEG_SNOW ) THEN
            IF (tl > tfrz) THEN
               qevpl = max (evplwet, 0.)
               qdewl = abs (min (evplwet, 0.) )
               qsubl = 0.
               qfrol = 0.

               IF (qevpl > ldew_rain/deltim) THEN
                  qsubl = qevpl - ldew_rain/deltim
                  qevpl = ldew_rain/deltim
               ENDIF
            ELSE
               qevpl = 0.
               qdewl = 0.
               qsubl = max (evplwet, 0.)
               qfrol = abs (min (evplwet, 0.) )

               IF (qsubl > ldew_snow/deltim) THEN
                  qevpl = qsubl - ldew_snow/deltim
                  qsubl = ldew_snow/deltim
               ENDIF
            ENDIF

            ldew_rain = ldew_rain + (qdewl-qevpl)*deltim
            ldew_snow = ldew_snow + (qfrol-qsubl)*deltim

            ldew = ldew_rain + ldew_snow
         ENDIF

      ELSEIF (DEF_Interception_scheme .eq. 4) THEN !Noah-MP
         ldew = max(0., ldew-evplwet*deltim)

         ! account for vegetation snow and update ldew_rain, ldew_snow, ldew
         IF ( DEF_VEG_SNOW ) THEN
            IF (tl > tfrz) THEN
               qevpl = max (evplwet, 0.)
               qdewl = abs (min (evplwet, 0.) )
               qsubl = 0.
               qfrol = 0.

               IF (qevpl > ldew_rain/deltim) THEN
                  qsubl = qevpl - ldew_rain/deltim
                  qevpl = ldew_rain/deltim
               ENDIF
            ELSE
               qevpl = 0.
               qdewl = 0.
               qsubl = max (evplwet, 0.)
               qfrol = abs (min (evplwet, 0.) )

               IF (qsubl > ldew_snow/deltim) THEN
                  qevpl = qsubl - ldew_snow/deltim
                  qsubl = ldew_snow/deltim
               ENDIF
            ENDIF

            ldew_rain = ldew_rain + (qdewl-qevpl)*deltim
            ldew_snow = ldew_snow + (qfrol-qsubl)*deltim

            ldew = ldew_rain + ldew_snow
         ENDIF

      ELSEIF (DEF_Interception_scheme .eq. 5) THEN !MATSIRO
         ldew = max(0., ldew-evplwet*deltim)

         ! account for vegetation snow and update ldew_rain, ldew_snow, ldew
         IF ( DEF_VEG_SNOW ) THEN
            IF (tl > tfrz) THEN
               qevpl = max (evplwet, 0.)
               qdewl = abs (min (evplwet, 0.) )
               qsubl = 0.
               qfrol = 0.

               IF (qevpl > ldew_rain/deltim) THEN
                  qsubl = qevpl - ldew_rain/deltim
                  qevpl = ldew_rain/deltim
               ENDIF
            ELSE
               qevpl = 0.
               qdewl = 0.
               qsubl = max (evplwet, 0.)
               qfrol = abs (min (evplwet, 0.) )

               IF (qsubl > ldew_snow/deltim) THEN
                  qevpl = qsubl - ldew_snow/deltim
                  qsubl = ldew_snow/deltim
               ENDIF
            ENDIF

            ldew_rain = ldew_rain + (qdewl-qevpl)*deltim
            ldew_snow = ldew_snow + (qfrol-qsubl)*deltim

            ldew = ldew_rain + ldew_snow
         ENDIF

      ELSEIF (DEF_Interception_scheme .eq. 6) THEN !VIC
         ldew = max(0., ldew-evplwet*deltim)

         ! account for vegetation snow and update ldew_rain, ldew_snow, ldew
         IF ( DEF_VEG_SNOW ) THEN
            IF (tl > tfrz) THEN
               qevpl = max (evplwet, 0.)
               qdewl = abs (min (evplwet, 0.) )
               qsubl = 0.
               qfrol = 0.

               IF (qevpl > ldew_rain/deltim) THEN
                  qsubl = qevpl - ldew_rain/deltim
                  qevpl = ldew_rain/deltim
               ENDIF
            ELSE
               qevpl = 0.
               qdewl = 0.
               qsubl = max (evplwet, 0.)
               qfrol = abs (min (evplwet, 0.) )

               IF (qsubl > ldew_snow/deltim) THEN
                  qevpl = qsubl - ldew_snow/deltim
                  qsubl = ldew_snow/deltim
               ENDIF
            ENDIF

            ldew_rain = ldew_rain + (qdewl-qevpl)*deltim
            ldew_snow = ldew_snow + (qfrol-qsubl)*deltim

            ldew = ldew_rain + ldew_snow
         ENDIF

      ELSEIF (DEF_Interception_scheme .eq. 7) THEN !JULES
         ldew = max(0., ldew-evplwet*deltim)

         ! account for vegetation snow and update ldew_rain, ldew_snow, ldew
         IF ( DEF_VEG_SNOW ) THEN
            IF (tl > tfrz) THEN
               qevpl = max (evplwet, 0.)
               qdewl = abs (min (evplwet, 0.) )
               qsubl = 0.
               qfrol = 0.

               IF (qevpl > ldew_rain/deltim) THEN
                  qsubl = qevpl - ldew_rain/deltim
                  qevpl = ldew_rain/deltim
               ENDIF
            ELSE
               qevpl = 0.
               qdewl = 0.
               qsubl = max (evplwet, 0.)
               qfrol = abs (min (evplwet, 0.) )

               IF (qsubl > ldew_snow/deltim) THEN
                  qevpl = qsubl - ldew_snow/deltim
                  qsubl = ldew_snow/deltim
               ENDIF
            ENDIF

            ldew_rain = ldew_rain + (qdewl-qevpl)*deltim
            ldew_snow = ldew_snow + (qfrol-qsubl)*deltim

            ldew = ldew_rain + ldew_snow
         ENDIF

      ELSEIF (DEF_Interception_scheme .eq. 8) THEN !CoLM202X
         ldew = max(0., ldew-evplwet*deltim)

         ! account for vegetation snow and update ldew_rain, ldew_snow, ldew
         IF ( DEF_VEG_SNOW ) THEN
            IF (tl > tfrz) THEN
               qevpl = max (evplwet, 0.)
               qdewl = abs (min (evplwet, 0.) )
               qsubl = 0.
               qfrol = 0.

               IF (qevpl > ldew_rain/deltim) THEN
                  qsubl = qevpl - ldew_rain/deltim
                  qevpl = ldew_rain/deltim
               ENDIF
            ELSE
               qevpl = 0.
               qdewl = 0.
               qsubl = max (evplwet, 0.)
               qfrol = abs (min (evplwet, 0.) )

               IF (qsubl > ldew_snow/deltim) THEN
                  qevpl = qsubl - ldew_snow/deltim
                  qsubl = ldew_snow/deltim
               ENDIF
            ENDIF

            ldew_rain = ldew_rain + (qdewl-qevpl)*deltim
            ldew_snow = ldew_snow + (qfrol-qsubl)*deltim

            ldew = ldew_rain + ldew_snow
         ENDIF
      ELSE
         CALL CoLM_stop()
      ENDIF

      ! Bug fix: When DEF_VEG_SNOW is false, only ldew is updated above
      ! (via ldew = max(0., ldew - evplwet*deltim)), but ldew_rain/ldew_snow
      ! remain unchanged. Downstream interception routines (schemes 1, 3-8)
      ! resync ldew = ldew_rain + ldew_snow at entry, which would silently
      ! revert the evaporation adjustment. Fix by scaling components proportionally.
      IF (.not. DEF_VEG_SNOW) THEN
         IF (ldew_rain + ldew_snow > 1.e-10) THEN
            ldew_rain = ldew * (ldew_rain / (ldew_rain + ldew_snow))
            ldew_snow = ldew - ldew_rain
         ELSEIF (ldew > 0.) THEN
            ! Components were zero but ldew > 0 (condensation case)
            IF (tl > tfrz) THEN
               ldew_rain = ldew
               ldew_snow = 0.
            ELSE
               ldew_rain = 0.
               ldew_snow = ldew
            ENDIF
         ELSE
            ldew_rain = 0.
            ldew_snow = 0.
         ENDIF
      ENDIF

      IF ( DEF_VEG_SNOW ) THEN
         ! update fwet_snow
         fwet_snow = 0
         IF(ldew_snow > 0.) THEN
            fwet_snow = ((10./(48.*(lai+sai)))*ldew_snow)**.666666666666
            ! Check for maximum limit of fwet_snow
            fwet_snow = min(fwet_snow,1.0)
         ENDIF

         ! phase change

         qmelt = 0.
         qfrz  = 0.

         !TODO: double check below
         IF (ldew_snow.gt.1.e-6 .and. tl.gt.tfrz) THEN
            qmelt = min(ldew_snow/deltim,(tl-tfrz)*cpice*ldew_snow/(deltim*hfus))
            ldew_snow = max(0.,ldew_snow - qmelt*deltim)
            ldew_rain = max(0.,ldew_rain + qmelt*deltim)
            !NOTE: There may be some problem, energy imbalance
            !      However, detailed treatment could be somewhat trivial
            tl = fwet_snow*tfrz + (1.-fwet_snow)*tl  !Niu et al., 2004
         ENDIF

         IF (ldew_rain.gt.1.e-6 .and. tl.lt.tfrz) THEN
            qfrz  = min(ldew_rain/deltim,(tfrz-tl)*cpliq*ldew_rain/(deltim*hfus))
            ldew_rain = max(0.,ldew_rain - qfrz*deltim)
            ldew_snow = max(0.,ldew_snow + qfrz*deltim)
            !NOTE: There may be some problem, energy imbalance
            !      However, detailed treatment could be somewhat trivial
            tl = fwet_snow*tfrz + (1.-fwet_snow)*tl  !Niu et al., 2004
         ENDIF
      ENDIF

!-----------------------------------------------------------------------
! 2 m height air temperature
!-----------------------------------------------------------------------
      tref = thm + vonkar/(fh-fht)*dth * (fh2m/vonkar - fh/vonkar)
      qref =  qm + vonkar/(fq-fqt)*dqh * (fq2m/vonkar - fq/vonkar)

      ! moninobukm returns the canopy-top profile but not the standard
      ! 10 m momentum profile. Re-evaluate only the diagnostics with the
      ! converged roughness lengths and Obukhov length.
      IF (DEF_USE_CBL_HEIGHT) THEN
         CALL moninobuk_leddy(hu_,ht_,hq_,displa,z0mv,z0hv,z0qv,profile_obu,profile_um,hpbl, &
                              diag_ustar,diag_fh2m,diag_fq2m,fm10m,diag_fm,diag_fh,diag_fq)
      ELSE
         CALL moninobuk(hu_,ht_,hq_,displa,z0mv,z0hv,z0qv,profile_obu,profile_um, &
                        diag_ustar,diag_fh2m,diag_fq2m,fm10m,diag_fm,diag_fh,diag_fq)
      ENDIF

   END SUBROUTINE LeafTemperature
!----------------------------------------------------------------------

   SUBROUTINE dewfraction (sigf,lai,sai,dewmx,ldew,ldew_rain,ldew_snow,fwet,fdry)
   !DESCRIPTION
   !===========
      ! determine fraction of foliage covered by water and
      ! fraction of foliage that is dry and transpiring

   !Original Author:
   !-------------------
      !---Yongjiu Dai

   !References:
   !-------------------
      !---Dai, Y., Zeng, X., Dickinson, R.E., Baker, I., Bonan, G.B., BosiloVICh, M.G., Denning,
      !   A.S., Dirmeyer, P.A., Houser, P.R., Niu, G. and Oleson, K.W., 2003.  The common land
      !   model. Bulletin of the American Meteorological Society, 84(8), pp.1013-1024.

   !ANCILLARY FUNCTIONS AND SUBROUTINES
   !-------------------

   !REVISION HISTORY
   !----------------
      !---2024.04.16  Hua Yuan: add option to account for vegetation snow process
      !---2021.12.08  Zhongwang Wei @ SYSU
      !---2018.06     Hua Yuan: remove sigf, to compatible with PFT
      !---1999.09.15  Yongjiu Dai
   !=======================================================================

   USE MOD_Precision

   IMPLICIT NONE

   real(r8), intent(in)  :: sigf      !fraction of veg cover, excluding snow-covered veg [-]
   real(r8), intent(in)  :: lai       !leaf area index  [-]
   real(r8), intent(in)  :: sai       !stem area index  [-]
   real(r8), intent(in)  :: dewmx     !maximum allowed dew [0.1 mm]
   real(r8), intent(in)  :: ldew      !depth of water on foliage [kg/m2/s]
   real(r8), intent(in)  :: ldew_rain !depth of rain on foliage [kg/m2/s]
   real(r8), intent(in)  :: ldew_snow !depth of snow on foliage [kg/m2/s]
   real(r8), intent(out) :: fwet      !fraction of foliage covered by water&snow [-]
   real(r8), intent(out) :: fdry      !fraction of foliage that is green and dry [-]

   real(r8) :: lsai                   !lai + sai
   real(r8) :: dewmxi                 !inverse of maximum allowed dew [1/mm]
   real(r8) :: vegt                   !sigf*lsai, NOTE: remove sigf
   real(r8) :: satcap_rain            !saturation capacity of foliage for rain [kg/m2]
   real(r8) :: satcap_snow            !saturation capacity of foliage for snow [kg/m2]
   real(r8) :: fwet_rain              !fraction of foliage covered by water [-]
   real(r8) :: fwet_snow              !fraction of foliage covered by snow [-]

      !-----------------------------------------------------------------------
      ! Fwet is the fraction of all vegetation surfaces which are wet
      ! including stem area which contribute to evaporation
      lsai   = lai + sai ! effective leaf area index
      dewmxi = 1.0/dewmx
      ! 06/2018, yuan: remove sigf, to compatible with PFT
      vegt   =  lsai

      fwet = 0
      IF (ldew > 0.) THEN
         fwet = ((dewmxi/vegt)*ldew)**.666666666666
         ! Check for maximum limit of fwet
         fwet = min(fwet,1.0)
      ENDIF

      ! account for vegetation snow
      ! calculate fwet_rain, fwet_snow, fwet
      IF ( DEF_VEG_SNOW ) THEN

         fwet_rain = 0
         IF(ldew_rain > 0.) THEN
            fwet_rain = ((dewmxi/vegt)*ldew_rain)**.666666666666
            ! Check for maximum limit of fwet_rain
            fwet_rain = min(fwet_rain,1.0)
         ENDIF

         fwet_snow = 0
         IF(ldew_snow > 0.) THEN
            fwet_snow = ((dewmxi/(48.*vegt))*ldew_snow)**.666666666666
            ! Check for maximum limit of fwet_snow
            fwet_snow = min(fwet_snow,1.0)
         ENDIF

         fwet = fwet_rain + fwet_snow - fwet_rain*fwet_snow
         fwet = min(fwet,1.0)
      ENDIF

      ! fdry is the fraction of lai which is dry because only leaves can
      ! transpire. Adjusted for stem area which does not transpire
      fdry = (1.-fwet)*lai/lsai

   END SUBROUTINE dewfraction

END MODULE MOD_LeafTemperature
