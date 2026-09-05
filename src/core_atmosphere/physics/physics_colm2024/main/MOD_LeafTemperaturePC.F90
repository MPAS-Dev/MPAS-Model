#include <define.h>

MODULE MOD_LeafTemperaturePC

!-----------------------------------------------------------------------
!
!          --- Leaf Temperature and Turbulence Modeling ---
!                for Plant Community (PC) Simulation
!
!                           o Reference hight
!                           |
!                           |
!            _____  tree    |      _____              --- Layer3
!           |||||||         |     |||||||
!          |||||||||--\/\/\/o    |||||||||
!           \|||||/         |     \|||||/
!              |            |        |                --- Layer2
!              |            |        |   shrub  /xx\
!              | grass -/\/-o--------|---\/\/\--\xx/
!  ____________|_____\\//____________|___________||__ --- Layer1
! /////////////////////////////////////////////////////////////////////
!
!-----------------------------------------------------------------------
	   USE MOD_Precision
	   USE MOD_MPAS_MPI, only: CoLM_stop
	   USE MOD_Namelist, only: DEF_USE_CBL_HEIGHT, DEF_USE_PLANTHYDRAULICS, DEF_USE_OZONESTRESS, &
                           DEF_RSS_SCHEME, DEF_Interception_scheme, DEF_SPLIT_SOILSNOW, &
                           DEF_VEG_SNOW
   IMPLICIT NONE
   SAVE

! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: LeafTemperaturePC

! PRIVATE MEMBER FUNCTIONS:
   PRIVATE :: dewfraction


!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------

   SUBROUTINE  LeafTemperaturePC ( &
               ipatch     ,ps         ,pe         ,deltim     ,csoilc     ,dewmx      ,&
               htvp       ,pftclass   ,fcover     ,htop       ,hbot       ,lai        ,&
               sai        ,extkb      ,extkd      ,hu         ,ht         ,hq         ,&
               us         ,vs         ,forc_t     ,thm        ,th         ,thv        ,&
               qm         ,psrf       ,rhoair     ,parsun     ,parsha     ,fsun       ,&
               sabv       ,frl        ,thermk     ,fshade     ,rstfacsun  ,rstfacsha  ,&
               gssun      ,gssha      ,po2m       ,pco2m      ,z0h_g      ,obug       ,&
               ustarg     ,zlnd       ,zsno       ,fsno       ,sigf       ,etrc       ,&
               tg         ,qg         ,rss        ,dqgdT      ,emg        ,t_soil     ,&
               t_snow     ,q_soil     ,q_snow     ,z0mpc      ,tl         ,ldew       ,&
               ldew_rain  ,ldew_snow  ,fwet_snow  ,taux       ,tauy       ,fseng      ,&
               fseng_soil ,fseng_snow ,fevpg      ,fevpg_soil ,fevpg_snow ,cgrnd      ,&
               cgrndl     ,cgrnds     ,tref       ,qref       ,rst        ,assim      ,&
               respc      ,fsenl      ,fevpl      ,etr        ,dlrad      ,ulrad      ,&
               z0m        ,zol        ,rib        ,ustar      ,qstar      ,tstar      ,&
               fm         ,fh         ,fq         ,fh2m       ,fq2m       ,fm10m      ,&
               vegwp      ,gs0sun     ,gs0sha     ,&
               assimsun   ,etrsun     ,assimsha   ,etrsha     ,&
!Ozone stress variables
               o3coefv_sun,o3coefv_sha,o3coefg_sun,o3coefg_sha,&
               lai_old    ,o3uptakesun,o3uptakesha,forc_ozone ,&
!End ozone stress variables
               hpbl, &
               qintr_rain ,qintr_snow ,t_precip   ,hprl       ,&
               dheatl     ,smp        ,hk         ,hksati     ,&
               rootflux    )

!=======================================================================
!
! !DESCRIPTION:
!  Leaf temperature resolved for Plant Community (3D) case Foliage
!  energy conservation for each PFT is given by foliage energy budget
!  equation:
!                       Rnet - Hf - LEf = 0
!  The equation is solved by Newton-Raphson iteration, in which this
!  iteration includes the calculation of the photosynthesis and stomatal
!  resistance, and the integration of turbulent flux profiles. The
!  sensible and latent heat transfer between foliage and atmosphere and
!  ground is linked by the equations:
!                       Ha = Hf + Hg and Ea = Ef + Eg
!
!  Original author: Hua Yuan and Yongjiu Dai, September, 2017
!
!
! !REFERENCES:
!  1) Dai, Y., Yuan, H., Xin, Q., Wang, D., Shangguan, W., Zhang, S., et
!  al.  (2019).  Different representations of canopy structure—A large
!  source of uncertainty in global land surface modeling. Agricultural
!  and Forest Meteorology, 269-270, 119-135.
!  https://doi.org/10.1016/j.agrformet.2019.02.006
!
! !REVISIONS:
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
   USE MOD_Const_Physical, only: vonkar, grav, hvap, hsub, cpair, stefnc, &
                                 cpliq, cpice, hfus, tfrz, denice, denh2o
   USE MOD_Const_PFT
   USE MOD_FrictionVelocity
   USE MOD_CanopyLayerProfile
   USE MOD_TurbulenceLEddy
   USE MOD_Qsadv
   USE MOD_AssimStomataConductance
   USE MOD_PlantHydraulic, only: PlantHydraulicStress_twoleaf
   USE MOD_Ozone, only: CalcOzoneStress
   USE MOD_Vars_1DForcing, only: forc_height_mode

   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------

   integer,  intent(in) :: ipatch
   integer,  intent(in) :: &
        ps,            &! start PFT index in a patch
        pe              ! end PFT index in a patch

   real(r8), intent(in) :: &
        deltim,        &! seconds in a time step [second]
        csoilc,        &! drag coefficient for soil under canopy [-]
        dewmx,         &! maximum dew
        htvp            ! latent heat of evaporation (/sublimation) [J/kg]

! vegetation parameters
   integer,  dimension(ps:pe), intent(in) :: &
        pftclass        ! PFT class

   real(r8), dimension(ps:pe), intent(in) :: &
        fcover,        &! PFT fractional coverage [-]
        htop,          &! PFT crown top height [m]
        hbot,          &! PFT crown bottom height [m]
        lai,           &! adjusted leaf area index for seasonal variation [-]
        sai             ! stem area index  [-]

   real(r8), intent(inout) :: &
        vegwp(1:nvegwcs,ps:pe),  &! vegetation water potential
        gs0sun(ps:pe),           &! maximum stomata conductance of sunlit leaf
        gs0sha(ps:pe)             ! maximum stomata conductance of shaded leaf

! input variables
   real(r8), intent(in) :: &
        hu,            &! observational height of wind [m]
        ht,            &! observational height of temperature [m]
        hq,            &! observational height of humidity [m]
        us,            &! wind component in eastward direction [m/s]
        vs,            &! wind component in northward direction [m/s]
        forc_t,        &! temperature at agcm reference height [kelvin]
        thm,           &! intermediate variable (tm+0.0098*ht)
        th,            &! potential temperature (kelvin)
        thv,           &! virtual potential temperature (kelvin)
        qm,            &! specific humidity at reference height [kg/kg]
        psrf,          &! pressure at reference height [pa]
        rhoair,        &! density air [kg/m**3]

        parsun(ps:pe), &! par absorbed per unit sunlit lai [w/m**2]
        parsha(ps:pe), &! par absorbed per unit shaded lai [w/m**2]
        fsun  (ps:pe), &! sunlit fraction of canopy
        sabv  (ps:pe), &! solar radiation absorbed by vegetation [W/m2]
        frl,           &! atmospheric infrared (longwave) radiation [W/m2]

        extkb (ps:pe), &! (k, g(mu)/mu) direct solar extinction coefficient
        extkd (ps:pe), &! diffuse and scattered diffuse PAR extinction coefficient
        thermk(ps:pe), &! canopy gap fraction for tir radiation
        fshade(ps:pe), &! shadow for each PFT

        po2m,          &! atmospheric partial pressure  o2 (pa)
        pco2m,         &! atmospheric partial pressure co2 (pa)

        z0h_g,         &! bare soil roughness length, sensible heat [m]
        obug,          &! bare soil obu
        ustarg,        &! bare soil ustar
        zlnd,          &! roughness length for soil [m]
        zsno,          &! roughness length for snow [m]
        fsno,          &! fraction of snow cover on ground

        sigf  (ps:pe), &! fraction of veg cover, excluding snow-covered veg [-]
        etrc  (ps:pe), &! maximum possible transpiration rate (mm/s)
        tg,            &! ground surface temperature [K]
        t_soil,        &! ground surface soil temperature [K]
        t_snow,        &! ground surface snow temperature [K]
        qg,            &! specific humidity at ground surface [kg/kg]
        q_soil,        &! specific humidity at ground surface soil [kg/kg]
        q_snow,        &! specific humidity at ground surface snow [kg/kg]
        dqgdT,         &! temperature derivative of "qg"
        rss,           &! soil surface resistance [s/m]
        emg             ! vegetation emissivity

   real(r8), intent(in) :: &
        t_precip,            &! snowfall/rainfall temperature [kelvin]
        qintr_rain(ps:pe),   &! rainfall interception (mm h2o/s)
        qintr_snow(ps:pe),   &! snowfall interception (mm h2o/s)
        smp     (1:nl_soil), &! precipitation sensible heat from canopy
        hksati  (1:nl_soil), &! hydraulic conductivity at saturation [mm h2o/s]
        hk      (1:nl_soil)   ! soil hydraulic conductance

   real(r8), intent(in) :: &
        hpbl            ! atmospheric boundary layer height [m]

   real(r8), dimension(ps:pe), intent(inout) :: &
        tl,            &! leaf temperature [K]
        ldew,          &! depth of water on foliage [mm]
        ldew_rain,     &! depth of rain on foliage [mm]
        ldew_snow,     &! depth of snow on foliage [mm]
        fwet_snow,     &! vegetation snow fractional cover [-]
!Ozone stress variables
        lai_old    ,   &! lai in last time step
        o3uptakesun,   &! Ozone does, sunlit leaf (mmol O3/m^2)
        o3uptakesha,   &! Ozone does, shaded leaf (mmol O3/m^2)
        o3coefv_sun,   &! Ozone stress factor for photosynthesis on sunlit leaf
        o3coefv_sha,   &! Ozone stress factor for photosynthesis on sunlit leaf
        o3coefg_sun,   &! Ozone stress factor for stomata on shaded leaf
        o3coefg_sha,   &! Ozone stress factor for stomata on shaded leaf
!End ozone stress variables
        rstfacsun,     &! factor of soil water stress to transpiration on sunlit leaf
        rstfacsha,     &! factor of soil water stress to transpiration on shaded leaf
        gssun,         &! stomata conductance of sunlit leaf
        gssha           ! stomata conductance of shaded leaf

   real(r8), dimension(ps:pe), intent(inout) :: &
        assimsun,      &! sunlit leaf assimilation rate [umol co2 /m**2/ s] [+]
        etrsun,        &! transpiration rate of sunlit leaf [mm/s]
        assimsha,      &! shaded leaf assimilation rate [umol co2 /m**2/ s] [+]
        etrsha          ! transpiration rate of shaded leaf [mm/s]

!Ozone stress variables
   real(r8), intent(inout) :: forc_ozone
!End ozone stress variables

   real(r8), intent(inout) :: &
        dlrad,         &! downward longwave radiation blow the canopy [W/m2]
        ulrad,         &! upward longwave radiation above the canopy [W/m2]
        taux,          &! wind stress: E-W [kg/m/s**2]
        tauy,          &! wind stress: N-S [kg/m/s**2]
        fseng,         &! sensible heat flux from ground [W/m2]
        fseng_soil,    &! sensible heat flux from ground soil [W/m2]
        fseng_snow,    &! sensible heat flux from ground snow [W/m2]
        fevpg,         &! evaporation heat flux from ground [mm/s]
        fevpg_soil,    &! evaporation heat flux from ground soil [mm/s]
        fevpg_snow,    &! evaporation heat flux from ground snow [mm/s]
        tref,          &! 2 m height air temperature (kelvin)
        qref,          &! 2 m height air specific humidity
        rootflux(nl_soil,ps:pe)    ! root water uptake from different layers

   real(r8), dimension(ps:pe), intent(out) :: &
        z0mpc,         &! z0m for individual PFT
        rst,           &! stomatal resistance
        assim,         &! rate of assimilation
        respc,         &! rate of respiration
        fsenl,         &! sensible heat from leaves [W/m2]
        fevpl,         &! evaporation+transpiration from leaves [mm/s]
        etr,           &! transpiration rate [mm/s]
        hprl,          &! precipitation sensible heat from canopy
        dheatl          ! vegetation heat change [W/m2]

   real(r8), intent(inout) :: &
        z0m,           &! effective roughness [m]
        zol,           &! dimensionless height (z/L) used in Monin-Obukhov theory
        rib,           &! bulk Richardson number in surface layer
        ustar,         &! friction velocity [m/s]
        tstar,         &! temperature scaling parameter
        qstar,         &! moisture scaling parameter
        fm,            &! integral of profile function for momentum
        fh,            &! integral of profile function for heat
        fq,            &! integral of profile function for moisture
        fh2m,          &! relation for temperature at 2m
        fq2m,          &! relation for specific humidity at 2m
        fm10m           ! integral of profile function for momentum at 10m

   real(r8), intent(inout) :: &
        cgrnd,         &! deriv. of soil energy flux wrt to soil temp [w/m2/k]
        cgrndl,        &! deriv, of soil latent heat flux wrt soil temp [w/m2/k]
        cgrnds          ! deriv of soil sensible heat flux wrt soil temp [w/m**2/k]

!-------------------------- Local Variables ----------------------------
! assign iteration parameters
   integer, parameter :: itmax  = 40   !maximum number of iteration
   integer, parameter :: itmin  = 6    !minimum number of iteration
   real(r8),parameter :: delmax = 3.0  !maximum change in leaf temperature [K]
   real(r8),parameter :: dtmin  = 0.01 !max limit for temperature convergence [K]
   real(r8),parameter :: dlemin = 0.1  !max limit for energy flux convergence [w/m2]

   real(r8) dtl(0:itmax+1,ps:pe)       !difference of tl between two iterative step

   !TODO: read from mod_const_pft.F90
   real(r8), dimension(ps:pe) :: &
        canlay,        &! PFT canopy layer number
        sqrtdi          ! inverse sqrt of leaf dimension [m**-0.5]

   !TODO: read from mod_const_pft.F90 file
   real(r8), dimension(ps:pe) :: &
        effcon,        &! quantum efficiency of RuBP regeneration (mol CO2 / mol quanta)
        vmax25,        &! maximum carboxylation rate at 25 C at canopy top
                        ! the range : 30.e-6 <-> 100.e-6 (mol co2 m-2 s-1)
        shti,          &! slope of high temperature inhibition function     (s1)
        hhti,          &! 1/2 point of high temperature inhibition function (s2)
        slti,          &! slope of low temperature inhibition function      (s3)
        hlti,          &! 1/2 point of low temperature inhibition function  (s4)
        trda,          &! temperature coefficient in gs-a model             (s5)
        trdm,          &! temperature coefficient in gs-a model             (s6)
        trop,          &! temperature coefficient in gs-a model         (273+25)
        g1,            &! conductance-photosynthesis slope parameter for medlyn model
        g0,            &! conductance-photosynthesis intercept for medlyn model
        gradm,         &! conductance-photosynthesis slope parameter
        binter,        &! conductance-photosynthesis intercept
        lambda,        &! marginal water cost of carbon gain
        extkn           ! coefficient of leaf nitrogen allocation

   integer, dimension(ps:pe)  :: &
        c3c4 ! C3/C4 plant type

   real(r8), dimension(ps:pe) :: &
        kmax_sun,      &! Plant Hydraulics Parameters
        kmax_sha,      &! Plant Hydraulics Parameters
        kmax_xyl,      &! Plant Hydraulics Parameters
        kmax_root,     &! Plant Hydraulics Parameters
        psi50_sun,     &! water potential at 50% loss of sunlit leaf tissue conductance (mmH2O)
        psi50_sha,     &! water potential at 50% loss of shaded leaf tissue conductance (mmH2O)
        psi50_xyl,     &! water potential at 50% loss of xylem tissue conductance (mmH2O)
        psi50_root,    &! water potential at 50% loss of root tissue conductance (mmH2O)
        ck              ! shape-fitting parameter for vulnerability curve (-)

   real(r8) :: &
        rootfr(nl_soil,ps:pe) ! root fraction

   real(r8) :: &
        hu_,           &! adjusted observational height of wind [m]
        ht_,           &! adjusted observational height of temperature [m]
        hq_,           &! adjusted observational height of humidity [m]
        zldis,         &! reference height "minus" zero displacement height [m]
        zii,           &! convective boundary layer height [m]
        z0mv,          &! roughness length, momentum [m]
        z0hv,          &! roughness length, sensible heat [m]
        z0qv,          &! roughness length, latent heat [m]
        zeta,          &! dimensionless height used in Monin-Obukhov theory
        beta,          &! coefficient of convective velocity [-]
        wc,            &! convective velocity [m/s]
        wc2,           &! wc**2
        dth,           &! diff of virtual temp. between ref. height and surface
        dthv,          &! diff of vir. poten. temp. between ref. height and surface
        dqh,           &! diff of humidity between ref. height and surface
        obu,           &! monin-obukhov length (m)
        um,            &! wind speed including the stability effect [m/s]
        ur,            &! wind speed at reference height [m/s]
        uaf,           &! velocity of air within foliage [m/s]
        thvstar,       &! virtual potential temperature scaling parameter
        eah,           &! canopy air vapor pressure (pa)
        pco2g,         &! co2 pressure (pa) at ground surface (pa)
        pco2a,         &! canopy air co2 pressure (pa)

        cf,            &! heat transfer coefficient from leaves [-]
        rbsun,         &! bulk boundary layer resistance of sunlit fraction of canopy
        rbsha,         &! bulk boundary layer resistance of shaded fraction of canopy
        ram,           &! aerodynamical resistance [s/m]
        rah,           &! thermal resistance [s/m]
        raw,           &! moisture resistance [s/m]

        det,           &! maximum leaf temp. change in two consecutive iter [K]
        dee,           &! maximum leaf heat fluxes change in two consecutive iter [W/m2]
        obuold,        &! monin-obukhov length from previous iteration
        err,           &! balance error

        rsoil,         &! soil respiration
        gah2o,         &! conductance between canopy and atmosphere
        gdh2o,         &! conductance between canopy and ground
        tprcor,        &! tf*psur*100./1.013e5

        fht,           &! integral of profile function for heat at the top layer
        fqt,           &! integral of profile function for moisture at the top layer
        phih,          &! phi(h), similarity function for sensible heat

        clai     (ps:pe), &! canopy heat capacity [Jm-2K-1]
        fdry     (ps:pe), &! fraction of foliage that is green and dry [-]
        fwet     (ps:pe), &! fraction of foliage covered by water [-]
        rb       (ps:pe), &! leaf boundary layer resistance [s/m]
        cfh      (ps:pe), &! heat conductance for leaf [m/s]
        cfw      (ps:pe), &! latent heat conductance for leaf [m/s]
        wlh      (ps:pe), &! normalized heat conductance for air and leaf [-]
        wlq      (ps:pe), &! normalized latent heat cond. for air and leaf [-]

        ei       (ps:pe), &! vapor pressure on leaf surface [pa]
        deidT    (ps:pe), &! derivative of "ei" on "tl" [pa/K]
        qsatl    (ps:pe), &! leaf specific humidity [kg/kg]
        qsatldT  (ps:pe), &! derivative of "qsatl" on "tlef"

        del      (ps:pe), &! absolute change in leaf temp in current iteration [K]
        del2     (ps:pe), &! change in leaf temperature in previous iteration [K]
        dele     (ps:pe), &! change in heat fluxes from leaf [W/m2]
        dele2    (ps:pe), &! change in heat fluxes from leaf in previous iteration [W/m2]

        tlbef    (ps:pe), &! leaf temperature from previous iteration [K]
        fsha     (ps:pe), &! shaded fraction of canopy
        laisun   (ps:pe), &! sunlit leaf area index, one-sided
        laisha   (ps:pe), &! shaded leaf area index, one-sided
        rssun    (ps:pe), &! sunlit leaf stomatal resistance [s/m]
        rssha    (ps:pe), &! shaded leaf stomatal resistance [s/m]
        respcsun (ps:pe), &! sunlit leaf respiration rate [umol co2 /m**2/ s] [+]
        respcsha (ps:pe)   ! shaded leaf respiration rate [umol co2 /m**2/ s] [+]

   integer it, nmozsgn

   real(r8) w, csoilcn, z0mg, z0hg, z0qg, elwmax, elwdif
   real(r8) diag_ustar, diag_fh2m, diag_fq2m, diag_fm, diag_fh, diag_fq
   real(r8) profile_obu, profile_um
   real(r8) cintsun(3, ps:pe), cintsha(3, ps:pe)
   real(r8),dimension(ps:pe)   :: delta, fac, etr0
   real(r8),dimension(ps:pe)   :: irab, dirab_dtl, fsenl_dtl, fevpl_dtl
   real(r8),dimension(ps:pe)   :: evplwet, evplwet_dtl, etr_dtl
   real(r8),dimension(ps:pe)   :: fevpl_bef, fevpl_noadj, dtl_noadj, htvpl, erre
   real(r8),dimension(ps:pe)   :: qevpl, qdewl, qsubl, qfrol, qmelt, qfrz
   real(r8),dimension(ps:pe)   :: gb_mol_sun,gb_mol_sha
   real(r8),dimension(nl_soil) :: k_soil_root    ! radial root and soil conductance
   real(r8),dimension(nl_soil) :: k_ax_root      ! axial root conductance

   ! .................................................................
   ! definition for 3d run
   ! .................................................................

   integer , parameter :: nlay = 3

   real(r8), parameter :: &
        c1   = 0.320,  &! parameter to calculate drag coefficients of Massman's method
        c2   = 0.264,  &! parameter to calculate drag coefficients of Massman's method
        c3   = 15.1,   &! parameter to calculate drag coefficients of Massman's method
        iw   = 0.5,    &! parameter to calculate alpha of Goudriaa's method
        Cd   = 0.2,    &! leaf drag coefficient
        cd1  = 7.5,    &! a free parameter for d/h calculation, Raupach 1992, 1994
        psih = 0.193    ! psih = ln(cw) - 1 + cw^-1, cw = 2, Raupach 1994

   real(r8) :: sqrtdragc! sqrt(drag coefficient)
   real(r8) :: lm       ! mix length within canopy
   real(r8) :: fai      ! canopy frontal area index
   real(r8) :: active_canopy_top ! tallest active PFT canopy [m]

   real(r8), dimension(0:nlay) :: &
        z0m_lays,      &! roughness length for momentum for the layer and below
        z0h_lays,      &! roughness length for SH for the layer and below
        z0q_lays,      &! roughness length for LH for the layer and below
        displa_lays,   &! displacement height for the layer and below
        fcover_lays     ! vegetation fractional cover for this layer and above

   real(r8), dimension(ps:pe) :: &
        lsai            ! lai + sai

   real(r8), dimension(nlay) :: &
        htop_lay,      &! canopy crown top for each layer
        hbot_lay,      &! canopy crown bottom for each layer
        fcover_lay,    &! vegetation fractional coverage for each layer
        lsai_lay,      &! (lai+sai) for each layer
        a_lay,         &! exp. extinction factor for u/k decline within canopy
        a_lay_i63,     &! exp. extinction factor for u/k decline within canopy (Inoue 1963)
        a_lay_k71,     &! exp. extinction factor for u/k decline within canopy (Kondo 1971)
        a_lay_g77,     &! exp. extinction factor for u/k decline within canopy (Groudrian 1977)
        a_lay_m97,     &! exp extinction factor for u/k decline within canopy (Massman 1997)
        utop_lay,      &! wind speed at layer top [m/s]
        ubot_lay,      &! wind speed at layer bottom [m/s]
        ueff_lay,      &! effective wind speed within canopy layer [m/s]
        ueff_lay_,     &! effective wind speed within canopy layer [m/s]
        ueff_lay_norm, &! normalized effective wind speed within canopy layer [m/s]
        ktop_lay,      &! eddy coefficient at layer top
        kbot_lay,      &! eddy coefficient at layer bottom
        z0m_lay,       &! roughness length for the vegetation covered area
        displa_lay,    &! displacement height for the vegetation covered area
        taf,           &! air temperature within canopy space [K]
        qaf,           &! humidity of canopy air [kg/kg]
        rd,            &! aerodynamic resistance between layers [s/m]
        cah,           &! heat conductance for air [m/s]
        cgh,           &! heat conductance for ground [m/s]
        caw,           &! latent heat conductance for air [m/s]
        cgw,           &! latent heat conductance for ground [m/s]
        wtshi,         &! sensible heat resistance for air, grd and leaf [-]
        wtsqi,         &! latent heat resistance for air, grd and leaf [-]
        wah,           &! normalized heat conductance for air [-]
        wgh,           &! normalized heat conductance for ground [-]
        waq,           &! normalized latent heat conductance for air [-]
        wgq,           &! normalized heat conductance for ground [-]
        wlhl,          &! sum of normalized heat conductance for air and leaf
        wlql            ! sum of normalized heat conductance for air and leaf

   real(r8) :: ktop, utop, fmtop, bee, tmpw1, tmpw2, fact, facq

   logical is_vegetated_patch
   integer i, p, clev
   integer toplay, botlay, upplay, numlay
   integer d_opt, rb_opt, rd_opt

   real(r8) :: displa, ttaf, tqaf

   ! variables for longwave transfer calculation
   ! .................................................................
   real(r8) :: tdn(0:4,0:4)     !downward transfer coefficient matrix for LW
   real(r8) :: tup(0:4,0:4)     !upward transfer coefficient matrix for LW
   real(r8) :: thermk_lay(nlay) !transmittance of longwave radiation for each layer
   real(r8) :: fshade_lay(nlay) !shadow of each layer
   real(r8) :: L(nlay)          !longwave radiation emitted by canopy layer
   real(r8) :: Ltd(nlay)        !transmitted downward longwave radiation from canopy layer
   real(r8) :: Ltu(nlay)        !transmitted upward longwave radiation from canopy layer
   real(r8) :: Lin(0:4)         !incoming longwave radiation for each layer
   real(r8) :: Ld(0:4)          !total downward longwave radiation for each layer
   real(r8) :: Lu(0:4)          !total upward longwave radiation for each layer
   real(r8) :: Lg               !emitted longwave radiation from ground
   real(r8) :: Lv(ps:pe)        !absorbed longwave radiation for each pft
   real(r8) :: dLv(ps:pe)       !LW change due to temperature change
   real(r8) :: dLvpar(nlay)     !temporal variable for calculating dLv

!-----------------------------------------------------------------------

! only process with vegetated patches

      lsai(:) = lai(:) + sai(:)
      active_canopy_top = 0._r8
      DO i = ps, pe
         IF (fcover(i) > 0._r8 .and. lsai(i) > 1.e-6_r8) THEN
            active_canopy_top = max(active_canopy_top, htop(i))
         ENDIF
      ENDDO
      is_vegetated_patch = .false.

      DO i = ps, pe
         IF (fcover(i)>0 .and. lsai(i)>1.e-6) THEN
            is_vegetated_patch = .true.
         ELSE
            tl(i) = forc_t
         ENDIF
      ENDDO

      ! When there is no vegetation in this Plant Community Patch, RETURN
      IF (.not. is_vegetated_patch) THEN
         RETURN
      ENDIF

! initialization of errors and  iteration parameters
      it       = 1    !counter for leaf temperature iteration
      del(:)   = 0.0  !change in leaf temperature from previous iteration
      dele(:)  = 0.0  !latent head flux from leaf for previous iteration

      dtl(:,:) = 0.
      fevpl_bef(:) = 0.

      d_opt  = 2
      rd_opt = 3
      rb_opt = 3

! initial values for z0hg, z0qg
      z0mg = (1.-fsno)*zlnd + fsno*zsno
      z0hg = z0mg
      z0qg = z0mg

! initialization of PFT constants
      DO i = ps, pe
         p = pftclass(i)

         canlay     (i) = canlay_p     (p)
         sqrtdi     (i) = sqrtdi_p     (p)

         effcon     (i) = effcon_p     (p)
         vmax25     (i) = vmax25_p     (p)
         c3c4       (i) = c3c4_p       (p)
         shti       (i) = shti_p       (p)
         hhti       (i) = hhti_p       (p)
         slti       (i) = slti_p       (p)
         hlti       (i) = hlti_p       (p)
         trda       (i) = trda_p       (p)
         trdm       (i) = trdm_p       (p)
         trop       (i) = trop_p       (p)
         g1         (i) = g1_p         (p)
         g0         (i) = g0_p         (p)
         gradm      (i) = gradm_p      (p)
         binter     (i) = binter_p     (p)
         lambda     (i) = lambda_p     (p)
         extkn      (i) = extkn_p      (p)

         kmax_sun   (i) = kmax_sun_p   (p)
         kmax_sha   (i) = kmax_sha_p   (p)
         kmax_xyl   (i) = kmax_xyl_p   (p)
         kmax_root  (i) = kmax_root_p  (p)
         psi50_sun  (i) = psi50_sun_p  (p)
         psi50_sha  (i) = psi50_sha_p  (p)
         psi50_xyl  (i) = psi50_xyl_p  (p)
         psi50_root (i) = psi50_root_p (p)
         ck         (i) = ck_p         (p)

         rootfr   (:,i) = rootfr_p   (:,p)
      ENDDO

!-----------------------------------------------------------------------
! scaling-up coefficients from leaf to canopy
!-----------------------------------------------------------------------

! note: need to separate to sunlit/shaded pars
!-----------------------------------------------------------------------

! partition visible canopy absorption to sunlit and shaded fractions
! to get average absorbed par for sunlit and shaded leaves
      fsha(:)   = 1. - fsun(:)
      laisun(:) = lai(:)*fsun(:)
      laisha(:) = lai(:)*fsha(:)

      cintsun(1,:) = (1.-exp(-(0.110+extkb)*lai))/(0.110+extkb)
      cintsun(2,:) = (1.-exp(-(extkb+extkd)*lai))/(extkb+extkd)
      cintsun(3,:) = (1.-exp(-extkb*lai))/extkb

      cintsha(1,:) = (1.-exp(-0.110*lai))/0.110 - cintsun(1,:)
      cintsha(2,:) = (1.-exp(-extkd*lai))/extkd - cintsun(2,:)
      cintsha(3,:) = lai(:) - cintsun(3,:)

!-----------------------------------------------------------------------
! get fraction of wet and dry canopy surface (fwet & fdry)
! initial saturated vapor pressure and humidity and their derivation
!-----------------------------------------------------------------------

      DO i = ps, pe

         clai(i) = 0.0

         ! 0.2mm*LSAI, account for leaf (plus dew) heat capacity
         IF ( DEF_VEG_SNOW ) THEN
            clai(i) = 0.2*lsai(i)*cpliq + ldew_rain(i)*cpliq + ldew_snow(i)*cpice
         ENDIF

         IF (fcover(i)>0 .and. lsai(i)>1.e-6) THEN
            CALL dewfraction (sigf(i),lai(i),sai(i),dewmx,&
                              ldew(i),ldew_rain(i),ldew_snow(i),fwet(i),fdry(i))
            CALL qsadv(tl(i),psrf,ei(i),deiDT(i),qsatl(i),qsatlDT(i))
         ENDIF
      ENDDO

!-----------------------------------------------------------------------
! initial for fluxes profile
!-----------------------------------------------------------------------

      nmozsgn = 0     !number of times moz changes sign
      obuold  = 0.    !monin-obukhov length from previous iteration
      zii     = 1000. !m (pbl height)
      beta    = 1.    !- (in computing W_*)

!-----------------------------------------------------------------------
! calculate layer average properties: height (htop_lay, hbot_lay), lsai_lay, ...
! !!NOTE: adjustment may needed for htop_lay/hbot_lay
!-----------------------------------------------------------------------
      htop_lay(:)   = 0
      hbot_lay(:)   = 0
      lsai_lay(:)   = 0
      fcover_lay(:) = 0

      DO i = ps, pe
         IF (fcover(i)>0 .and. lsai(i)>1.e-6) THEN
            clev = canlay(i)
            htop_lay(clev) = htop_lay(clev) + htop(i) * fcover(i)
            hbot_lay(clev) = hbot_lay(clev) + hbot(i) * fcover(i)
            lsai_lay(clev) = lsai_lay(clev) + lsai(i) * fcover(i)
            fcover_lay(clev) = fcover_lay(clev) + fcover(i)
         ENDIF
      ENDDO

      DO i = 1, nlay
         IF (fcover_lay(i) > 0) THEN
            htop_lay(i) = htop_lay(i) / fcover_lay(i)
            hbot_lay(i) = hbot_lay(i) / fcover_lay(i)
            lsai_lay(i) = lsai_lay(i) / fcover_lay(i)
         ENDIF
      ENDDO

      ! calculate fcover_lays
! 03/16/2020, yuan: determine to set fc=0 or fcover above for
! gaps between layers, 0 maybe more consistent
      fcover_lays(0) = sum(fcover_lay(:))
      fcover_lays(1) = sum(fcover_lay(1:3))
      fcover_lays(2) = sum(fcover_lay(2:3))
      fcover_lays(3) = sum(fcover_lay(3:3))
      fcover_lays(:) = 0.

!-----------------------------------------------------------------------
! scaling factor bee
!-----------------------------------------------------------------------
! 09/26/2017, yuan: NOTE! bee value, the default is 1
      bee = 1.

!-----------------------------------------------------------------------
! calculate z0m and displa for PFTs
!-----------------------------------------------------------------------
      DO i = ps, pe
         IF (lsai(i) > 1.e-6) THEN
            CALL cal_z0_displa(lsai(i), htop(i), 1., z0mpc(i), displa)
         ELSE
            z0mpc(i) = z0mg
         ENDIF
      ENDDO

!-----------------------------------------------------------------------
! calculate z0m and displa for layers
!-----------------------------------------------------------------------

      displa_lay (:) = 0.
      displa_lays(:) = 0.
      z0m_lay    (:) = 0.
      z0m_lays   (:) = 0.

      DO i = 1, nlay
         IF (fcover_lay(i)>0 .and. lsai_lay(i)>0) THEN
            CALL cal_z0_displa(lsai_lay(i), htop_lay(i), 1., z0m_lay(i), displa_lay(i))
            CALL cal_z0_displa(lsai_lay(i), htop_lay(i), fcover_lay(i), z0m_lays(i), displa_lays(i))
         ENDIF
      ENDDO

      ! ground
      z0m_lays   (0) = z0mg
      displa_lays(0) = 0.

      ! 10/05/2017: robust check
      WHERE (z0m_lays(:) < z0mg) z0m_lays(:) = z0mg
      WHERE (z0m_lay (:) < z0mg) z0m_lay (:) = z0mg

      ! maximum assumption
      z0m_lays(1) = maxval(z0m_lays(0:1))
      z0m_lays(2) = maxval(z0m_lays(0:2))
      z0m_lays(3) = maxval(z0m_lays(0:3))

      displa_lays(1) = maxval(displa_lays(0:1))
      displa_lays(2) = maxval(displa_lays(0:2))
      displa_lays(3) = maxval(displa_lays(0:3))

      ! roughness length and displacement height for sensible
      ! and latent heat transfer
      z0h_lays(:) = z0m_lays(:)
      z0q_lays(:) = z0m_lays(:)

!-----------------------------------------------------------------------
! calculate layer a_lay
!-----------------------------------------------------------------------
      ! initialization
      a_lay    (:) = 0.
      a_lay_i63(:) = 0.
      a_lay_k71(:) = 0.
      a_lay_g77(:) = 0.
      a_lay_m97(:) = 0.

      DO i = 1, nlay
         IF (fcover_lay(i)>0 .and. lsai_lay(i)>0) THEN

            ! mixing length and sqrt(drag coefficient)
            lm = vonkar*(htop_lay(i) - displa_lay(i))

            ! Raupach, 1992
            fai = 1. - exp(-0.5*lsai_lay(i))
            sqrtdragc = min( (0.003+0.3*fai)**0.5, 0.3 )

            ! Inoue, 1963
            a_lay_i63(i) = htop_lay(i) * &
                           (Cd*lsai_lay(i)/(2.*htop_lay(i)*lm**2))**(1./3.)

            ! Kondo, 1971
            a_lay_k71(i) = htop_lay(i)/(htop_lay(i)-displa_lay(i))/ &
                           (vonkar/sqrtdragc)

            ! Goudriaan, 1977
            a_lay_g77(i) = (Cd*lsai_lay(i)*htop_lay(i)/lm)**0.5

            ! Massman, 1997
            a_lay_m97(i) = Cd*lsai_lay(i) / (2.*sqrtdragc**2)

            a_lay(i) = a_lay_k71(i)

            displa_lay(i) = max(htop_lay(i)/2., displa_lay(i))

         ENDIF
      ENDDO

!-----------------------------------------------------------------------
! calculate layer info
! how may layers, top layer and bottom layer number
!-----------------------------------------------------------------------

      toplay = 0
      botlay = 0
      numlay = 0

      DO i = nlay, 1, -1
         IF (fcover_lay(i)>0 .and. lsai_lay(i)>0) THEN

            ! to count the layer number
            numlay = numlay + 1
            IF (toplay .eq. 0) THEN
               ! set the top layer to current layer
               toplay = i
            ENDIF

            ! set this layer to be the bottom layer
            botlay = i

            displa_lay(i) = max(displa_lay(i), hbot_lay(i))
         ENDIF
      ENDDO

!-----------------------------------------------------------------------
! calculate transmittance of longwave radiation for each layer
! diffuse case
!-----------------------------------------------------------------------

      thermk_lay(:) = 0.
      fshade_lay(:) = 0.

      DO i = ps, pe
         IF (fshade(i)>0 .and. canlay(i)>0) THEN
            clev = canlay(i)
            thermk_lay(clev) = thermk_lay(clev) + fshade(i) * thermk(i)
            fshade_lay(clev) = fshade_lay(clev) + fshade(i)
         ENDIF
      ENDDO

      DO i = 1, nlay
         IF (fshade_lay(i) > 0) THEN
            thermk_lay(i) = thermk_lay(i) / fshade_lay(i)
         ELSE
            thermk_lay(i) = 1.
         ENDIF
      ENDDO

!-----------------------------------------------------------------------
! calculate the transfer matrix for long-wave radiation transfer
! direct case
! NOTE: don't need to calculate at each step
!-----------------------------------------------------------------------

      tdn(:,:) = 0.
      tup(:,:) = 0.

      tdn(1,0) = 1.
      tdn(2,0) = 1 - fshade_lay(1)
      tdn(3,0) = 1 - fshade_lay(1) - fshade_lay(2) + fshade_lay(1)*fshade_lay(2)
      tdn(4,0) = 1 - fshade_lay(1) - fshade_lay(2) - fshade_lay(3) &
               + fshade_lay(1)*fshade_lay(2) &
               + fshade_lay(1)*fshade_lay(3) &
               + fshade_lay(2)*fshade_lay(3) &
               - fshade_lay(1)*fshade_lay(2)*fshade_lay(3)

      tdn(2,1) = fshade_lay(1)
      tdn(3,1) = (1 - fshade_lay(2))*fshade_lay(1)
      tdn(4,1) = (1 - fshade_lay(2) - fshade_lay(3) + fshade_lay(2)*fshade_lay(3))*fshade_lay(1)

      tdn(3,2) = fshade_lay(2)
      tdn(4,2) = (1 - fshade_lay(3))*fshade_lay(2)
      tdn(4,3) = fshade_lay(3)

      tup(0,1) = fshade_lay(1)
      tup(0,2) = (1 - fshade_lay(1))*fshade_lay(2)
      tup(1,2) = fshade_lay(2)

      tup(0,3) = (1 - fshade_lay(1) - fshade_lay(2) + fshade_lay(1)*fshade_lay(2))*fshade_lay(3)
      tup(1,3) = (1 - fshade_lay(2))*fshade_lay(3)
      tup(2,3) = fshade_lay(3)

      tup(0,4) = tdn(4,0)
      tup(1,4) = 1 - fshade_lay(2) - fshade_lay(3) + fshade_lay(2)*fshade_lay(3)
      tup(2,4) = 1 - fshade_lay(3)
      tup(3,4) = 1.

!-----------------------------------------------------------------------
! calculate parameters for delta(Lv) for LW radiation transfer
!-----------------------------------------------------------------------
      dLvpar(1) = 1.
      dLvpar(2) = ( (1-fshade_lay(1)) + thermk_lay(1)*fshade_lay(1) )**2
      dLvpar(3) = ( tdn(3,0) &
                + thermk_lay(2)*fshade_lay(2)*(1-fshade_lay(1)+thermk_lay(1)*fshade_lay(1)) &
                + (1-fshade_lay(2))*thermk_lay(1)*fshade_lay(1) )**2

!-----------------------------------------------------------------------
! first guess for taf and qaf for each layer
! a large difference from previous schemes
!-----------------------------------------------------------------------
      taf(:) = 0.
      qaf(:) = 0.

      ! 05/02/2016: set taf/qaf according to layer number
      IF (numlay .eq. 1) THEN
         taf(toplay) = 0.5 * (tg + thm)
         qaf(toplay) = 0.5 * (qm + qg )
      ENDIF

      IF (numlay .eq. 2) THEN
         taf(botlay) = (2.*tg + thm)/3.
         qaf(botlay) = (2.*qg + qm )/3.
         taf(toplay) = (tg + 2.*thm)/3.
         qaf(toplay) = (qg + 2.*qm )/3.
      ENDIF

      IF (numlay .eq. 3) THEN
         taf(1) = (3.*tg + thm)/4.
         qaf(1) = (3.*qg + qm )/4.
         taf(2) = (tg + thm )/2.
         qaf(2) = (qg + qm  )/2.
         taf(3) = (tg + 3.*thm)/4.
         qaf(3) = (qg + 3.*qm )/4.
      ENDIF

!-----------------------------------------------------------------------
! some environment variables
! how to calculate rsoil and what is its usage?
!-----------------------------------------------------------------------
      pco2a = pco2m
      tprcor = 44.6*273.16*psrf/1.013e5
      rsoil = 0.   !respiration (mol m-2 s-1)
!     rsoil = 1.22e-6*exp(308.56*(1./56.02-1./(tg-227.13)))
!     rsoil = rstfac * 0.23 * 15. * 2.**((tg-273.16-10.)/10.) * 1.e-6
!     rsoil = 5.22 * 1.e-6
      rsoil = 0.22 * 1.e-6

! initialization and input values for Monin-Obukhov
      ! have been set before
      z0mv = z0m_lays(3); z0hv = z0m_lays(3); z0qv = z0m_lays(3)
      ur    = max(0.1, sqrt(us*us+vs*vs))    !limit set to 0.1
      dth   = thm - taf(toplay)
      dqh   = qm  - qaf(toplay)
      dthv  = dth*(1.+0.61*qm) + 0.61*th*dqh

      hu_ = hu; ht_ = ht; hq_ = hq;

      IF (forc_height_mode == 'absolute') THEN

#ifdef MPAS_EMBEDDED_COLM
	         IF (hu <= active_canopy_top+1._r8 .or. ht <= active_canopy_top+1._r8 .or. &
	             hq <= active_canopy_top+1._r8) THEN
            CALL CoLM_stop('MPAS embedded CoLM requires the MPAS lowest atmospheric level '// &
	                           'to be more than 1 m above every active CoLM PFT canopy top.')
         ENDIF
#else
         IF (hu <= htop_lay(toplay)+1) THEN
            hu_ = htop_lay(toplay) + 1.
            IF (taux == spval) & ! only print warning for the first time-step
               write(6,*) 'Warning: the obs height of u less than htop+1, set it to htop+1.'
         ENDIF

         IF (ht <= htop_lay(toplay)+1) THEN
            ht_ = htop_lay(toplay) + 1.
            IF (taux == spval) & ! only print warning for the first time-step
               write(6,*) 'Warning: the obs height of t less than htop+1, set it to htop+1.'
         ENDIF

         IF (hq <= htop_lay(toplay)+1) THEN
            hq_ = htop_lay(toplay) + 1.
            IF (taux == spval) & ! only print warning for the first time-step
               write(6,*) 'Warning: the obs height of q less than htop+1, set it to htop+1.'
         ENDIF
#endif

      ELSE ! relative height
         hu_ = htop_lay(toplay) + hu
         ht_ = htop_lay(toplay) + ht
         hq_ = htop_lay(toplay) + hq
      ENDIF

      zldis = hu_ - displa_lays(3)

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

         DO i = ps, pe
            IF (tl(i) > tfrz) THEN
               htvpl(i) = hvap
            ELSE
               htvpl(i) = hsub
            ENDIF
         ENDDO

!-----------------------------------------------------------------------
! Aerodynamical resistances
!-----------------------------------------------------------------------
! Evaluate stability-dependent variables using moz from prior iteration
         profile_obu = obu
         profile_um = um

         IF (DEF_USE_CBL_HEIGHT) THEN
            CALL moninobukm_leddy(hu_,ht_,hq_,displa_lays(toplay),z0mv,z0hv,z0qv,obu,um, &
                                  displa_lay(toplay),z0m_lay(toplay),hpbl,ustar,fh2m,fq2m, &
                                  htop_lay(toplay),fmtop,fm,fh,fq,fht,fqt,phih)
         ELSE
            CALL moninobukm(hu_,ht_,hq_,displa_lays(toplay),z0mv,z0hv,z0qv,obu,um, &
                            displa_lay(toplay),z0m_lay(toplay),ustar,fh2m,fq2m, &
                            htop_lay(toplay),fmtop,fm,fh,fq,fht,fqt,phih)
         ENDIF

! Aerodynamic resistance
         ! 09/16/2017:
         ! note that for ram, it is the resistance from Href to z0mv+displa
         ! however, for rah and raw is only from Href to canopy effective
         ! exchange height.
         ! so rah/raw is not comparable with that of 1D case
         ram = 1./(ustar*ustar/um)

         ! 05/02/2016: calculate resistance from the top layer (effective exchange
         ! height) to reference height
         rah = 1./(vonkar/(fh-fht)*ustar)
         raw = 1./(vonkar/(fq-fqt)*ustar)

! update roughness length for sensible/latent heat
         z0hg = z0mg/exp(0.13 * (ustar*z0mg/1.5e-5)**0.45)
         z0qg = z0hg

         z0h_lays(0) = z0hg
         z0q_lays(0) = z0qg

         z0h_lays(1) = maxval(z0h_lays(0:1))
         z0h_lays(2) = maxval(z0h_lays(0:2))
         z0h_lays(3) = maxval(z0h_lays(0:3))

         z0q_lays(:) = z0h_lays(:)
         z0hv = z0h_lays(3)
         z0qv = z0q_lays(3)

! ......................................................................
! new method to calculate rd and ueffect
! the kernel part of 3d model
! ......................................................................

         ! initialization
         rd(:)  = 0.
         upplay = 0

         ! calculate canopy top wind speed (utop) and exchange coefficient (ktop)
         ! need to update each time as obu changed after each iteration
         utop = ustar/vonkar * fmtop
         ktop = vonkar * (htop_lay(toplay)-displa_lays(toplay)) * ustar / phih

         ! start layer loop
         DO i = toplay, 1, -1

            IF (fcover_lay(i)>0 .and. lsai_lay(i)>0) THEN

               IF (i .eq. toplay) THEN
                  utop_lay(i) = utop
                  ktop_lay(i) = ktop
               ELSE
                  ! calculate utop of this layer
                  utop_lay(i) = uprofile(ubot_lay(upplay), fcover_lays(upplay), bee, 0., &
                                z0mg, hbot_lay(upplay), htop_lay(i), htop_lay(i))

                  ! calculate ktop of this layer
                  ktop_lay(i) = kprofile(kbot_lay(upplay), fcover_lays(upplay), bee, 0., &
                                displa_lays(toplay)/htop_lay(toplay), &
                                hbot_lay(upplay), htop_lay(i), obug, ustarg, htop_lay(i))

                  ! areodynamic resistance between this layer top and above layer bottom
                  ! 03/15/2020, yuan: vertical gaps between layers
                  ! fc = fcover_lays(upplay) or just 0?
                  rd(upplay) = rd(upplay) + frd(kbot_lay(upplay), hbot_lay(upplay), htop_lay(i), &
                               hbot_lay(upplay), htop_lay(i), &
                               displa_lays(toplay)/htop_lay(toplay), &
                               z0h_g, obug, ustarg, z0mg, 0., bee, fcover_lays(upplay))

               ENDIF

               ! for robust check
               hbot_lay(i) = max(hbot_lay(i), displa_lays(i-1)+z0m_lays(i-1))

               ! wind speed at layer bottom
               ubot_lay(i) = uprofile(utop_lay(i), fcover_lay(i), bee, a_lay(i), &
                             z0mg, htop_lay(i), hbot_lay(i), hbot_lay(i))

               IF (it == 1) THEN
                  ueff_lay_norm(i) = ueffect(1., htop_lay(i), hbot_lay(i), &
                                     z0mg, a_lay(i), bee, fcover_lay(i))
               ENDIF
               ueff_lay(i) = utop_lay(i)*ueff_lay_norm(i)

               ! normalized eddy coefficient (K) at layer bottom
               kbot_lay(i) = kprofile(ktop_lay(i), fcover_lay(i), bee, a_lay(i), &
                             displa_lays(toplay)/htop_lay(toplay), &
                             htop_lay(i), hbot_lay(i), obug, ustarg, hbot_lay(i))

               ! areodynamic resistance from effective fluxes exchange height of
               ! of this layer to the top of this layer
               IF (upplay > 0) THEN
                  rd(upplay) = rd(upplay) + frd(ktop_lay(i), htop_lay(i), hbot_lay(i), &
                               htop_lay(i), displa_lay(i)+z0m_lay(i), &
                               displa_lays(toplay)/htop_lay(toplay), &
                               z0h_g, obug, ustarg, z0mg, a_lay(i), bee, fcover_lay(i))
               ENDIF

               rd(i) = rd(i) + frd(ktop_lay(i), htop_lay(i), hbot_lay(i), &
                       displa_lay(i)+z0m_lay(i), max(z0qg,hbot_lay(i)), &
                       displa_lays(toplay)/htop_lay(toplay), z0h_g, obug, ustarg, &
                       z0mg, a_lay(i), bee, fcover_lay(i))

               upplay = i

            ENDIF
         ENDDO

! ......................................................................
! areodynamic resistance between ground and the upper layer bottom
! ......................................................................

         ! uncomment the below when the upper codes change to hbot_lay
         !rd(botlay) = rd(botlay) + kintegral(kbot_lay(botlay), fcover_lays(botlay), bee, 0., &
         !             z0mg, displa_lays(toplay)/htop_lay(toplay), &
         !             hbot_lay(botlay), z0qg, obug, ustarg, hbot_lay(botlay), z0qg )

         rd(botlay) = rd(botlay) + frd(kbot_lay(botlay), hbot_lay(botlay), z0qg, &
                      hbot_lay(botlay), z0qg, displa_lays(toplay)/htop_lay(toplay), &
                      z0h_g, obug, ustarg, z0mg, 0., bee, fcover_lays(botlay))

! ......................................................................
! Bulk boundary layer resistance of leaves
! ......................................................................
         rb(:) = 0.

         DO i = ps, pe
            IF (fcover(i)>0 .and. lsai(i)>1.e-6) THEN
               clev = canlay(i)
               cf = 0.01*sqrtdi(i)*sqrt(ueff_lay(clev))
               rb(i) = 1./cf
            ENDIF
         ENDDO

         ! 10/01/2017, back to 1D case
         IF (rb_opt == 1) THEN
            uaf   = ustar
            cf    = 0.01*sqrtdi(2)/sqrt(uaf)
            rb(:) = 1/(cf*uaf)
         ENDIF

!        rd = 1./(csoilc*uaf)                 ! BATS legacy
!        w = exp(-0.5*(lai+sai))              ! Dickinson's modification :
!        csoilc = ( 1.-w + w*um/uaf)/rah      ! "rah" here is the resistance over
!        rd = 1./(csoilc*uaf)                 ! bare ground fraction

         ! 10/01/2017, back to 1D case
         IF (rd_opt == 1 ) THEN
! modified by Xubin Zeng's suggestion at 08-07-2002
            uaf   = ustar
            w = exp(-(lai(2)+sai(2)))
            csoilcn = (vonkar/(0.13*(z0mg*uaf/1.5e-5)**0.45))*w + csoilc*(1.-w)
            rd(:) = 1./(csoilcn*uaf)
         ENDIF

!-----------------------------------------------------------------------
! stomatal resistances
!-----------------------------------------------------------------------

         DO i = ps, pe
            p = pftclass(i)
            IF(fcover(i)>0 .and. lai(i)>0.001) THEN

               rbsun = rb(i) / laisun(i)
               rbsha = rb(i) / laisha(i)

               clev = canlay(i)
               eah = qaf(clev) * psrf / ( 0.622 + 0.378 * qaf(clev) )    !pa

               IF (DEF_USE_PLANTHYDRAULICS) THEN
                  rstfacsun(i) = 1.
                  rstfacsha(i) = 1.
               ENDIF

! note: calculate resistance for sunlit/shaded leaves
!-----------------------------------------------------------------------
               CALL stomata ( vmax25(i)    ,effcon(i) ,c3c4(i)   ,slti(i)   ,hlti(i)   ,&
                    shti(i)    ,hhti(i)    ,trda(i)   ,trdm(i)   ,trop(i)   ,&
                    g1(i)      ,g0(i)      ,gradm(i)  ,binter(i) ,thm       ,&
                    psrf       ,po2m       ,pco2m     ,pco2a     ,eah       ,&
                    ei(i)      ,tl(i)      ,parsun(i) ,&
!Ozone stress variables
                    o3coefv_sun(i),     o3coefg_sun(i),&
!End ozone stress variables
                    lambda(i),                         &
                    rbsun      ,raw        ,rstfacsun(i),cintsun(:,i),&
                    assimsun(i),respcsun(i),rssun(i)   )

               CALL stomata ( vmax25(i)    ,effcon(i) ,c3c4(i)   ,slti(i)   ,hlti(i)   ,&
                    shti(i)    ,hhti(i)    ,trda(i)   ,trdm(i)   ,trop(i)   ,&
                    g1(i)      ,g0(i)      ,gradm(i)  ,binter(i) ,thm       ,&
                    psrf       ,po2m       ,pco2m     ,pco2a     ,eah       ,&
                    ei(i)      ,tl(i)      ,parsha(i) ,&
!Ozone stress variables
                    o3coefv_sha(i),     o3coefg_sha(i),&
!End ozone stress variables
!WUE stomata model parameter
                    lambda(i)                                               ,&
!WUE stomata model parameter
                    rbsha      ,raw        ,rstfacsha(i),cintsha(:,i),&
                    assimsha(i),respcsha(i),rssha(i)   )

               IF (DEF_USE_PLANTHYDRAULICS) THEN

                  gs0sun(i) = min( 1.e6, 1./(rssun(i)*tl(i)/tprcor) )/ laisun(i) * 1.e6 * o3coefg_sun(i)
                  gs0sha(i) = min( 1.e6, 1./(rssha(i)*tl(i)/tprcor) )/ laisha(i) * 1.e6 * o3coefg_sha(i)

                  CALL PlantHydraulicStress_twoleaf (nl_soil     ,nvegwcs      ,z_soi        ,&
                        dz_soi       ,rootfr(:,i)  ,psrf         ,qsatl(i)     ,qaf(clev)    ,&
                        tl(i)        ,rbsun        ,rss          ,raw        ,sum(rd(1:clev)),&
                        rstfacsun(i) ,rstfacsha(i) ,cintsun(:,i) ,cintsha(:,i) ,laisun(i)    ,&
                        laisha(i)    ,rhoair       ,fwet(i)      ,sai(i)       ,kmax_sun(i)  ,&
                        kmax_sha(i)  ,kmax_xyl(i)  ,kmax_root(i) ,psi50_sun(i) ,psi50_sha(i) ,&
                        psi50_xyl(i) ,psi50_root(i),htop(i)      ,ck(i)        ,smp          ,&
                        hk           ,hksati       ,vegwp(:,i)   ,etrsun(i)    ,etrsha(i)    ,&
                        rootflux(:,i),qg           ,qm           ,gs0sun(i)    ,gs0sha(i)    ,&
                        k_soil_root  ,k_ax_root    ,gssun(i)     ,gssha(i)                    )

                  etr(i)  = etrsun(i) + etrsha(i)
                  gssun(i) = gssun(i) * laisun(i) * 1.e-6
                  gssha(i) = gssha(i) * laisha(i) * 1.e-6

                  CALL update_photosyn(tl(i), po2m, pco2m, pco2a, parsun(i), psrf, rstfacsun(i), &
                     rb(i), gssun(i), effcon(i), vmax25(i), c3c4(i), gradm(i), trop(i), slti(i), hlti(i), &
                     shti(i), hhti(i), trda(i), trdm(i), cintsun(:,i), assimsun(i), respcsun(i))

                  CALL update_photosyn(tl(i), po2m, pco2m, pco2a, parsha(i), psrf, rstfacsha(i), &
                     rb(i), gssha(i), effcon(i), vmax25(i), c3c4(i), gradm(i), trop(i), slti(i), hlti(i), &
                     shti(i), hhti(i), trda(i), trdm(i), cintsha(:,i), assimsha(i), respcsha(i))

                  ! leaf scale stomata resistance
                  rssun(i) = tprcor / tl(i) / gssun(i)
                  rssha(i) = tprcor / tl(i) / gssha(i)

               ENDIF

            ELSE
               rssun(i) = 2.e4; assimsun(i) = 0.; respcsun(i) = 0.
               rssha(i) = 2.e4; assimsha(i) = 0.; respcsha(i) = 0.
               IF (DEF_USE_PLANTHYDRAULICS) THEN
                  etr(i) = 0.
                  rootflux(:,i) = 0.
               ENDIF
            ENDIF
         ENDDO

! above stomatal resistances are for the canopy, the stomatal resistances
! and the "rb" in the following calculations are the average for single leaf. thus,
         rssun = rssun * laisun
         rssha = rssha * laisha

!-----------------------------------------------------------------------
! dimensional and non-dimensional sensible and latent heat conductances
! for canopy and soil flux calculations.
!-----------------------------------------------------------------------

         cfh(:) = 0.
         cfw(:) = 0.

         DO i = ps, pe
            IF (fcover(i)>0 .and. lsai(i)>1.e-6) THEN

               clev = canlay(i)
               delta(i) = 0.0
               IF(qsatl(i)-qaf(clev) .gt. 0.) delta(i) = 1.0

               cfh(i) = lsai(i) / rb(i)

! note: combine sunlit and shaded leaves
!-----------------------------------------------------------------------
               cfw(i) = (1.-delta(i)*(1.-fwet(i)))*lsai(i)/rb(i) + &
                        (1.-fwet(i))*delta(i)* &
                        ( laisun(i)/(rb(i)+rssun(i)) + laisha(i)/(rb(i)+rssha(i)) )
            ENDIF
         ENDDO

         ! initialization
         cah(:) = 0.
         caw(:) = 0.
         cgh(:) = 0.
         cgw(:) = 0.

         DO i = 1, nlay
            IF (fcover_lay(i)>0 .and. lsai_lay(i)>0) THEN
               IF (i == toplay) THEN
                  cah(i) = 1. / rah
                  caw(i) = 1. / raw
               ELSE
                  cah(i) = 1. / rd(i+1)
                  caw(i) = 1. / rd(i+1)
               ENDIF

               cgh(i) = 1. / rd(i)
               IF (i == botlay) THEN
                  IF (qg < qaf(botlay)) THEN
                     cgw(i) = 1. / rd(i) !dew case. no soil resistance
                  ELSE
                     IF (DEF_RSS_SCHEME .eq. 4) THEN
                        cgw(i) = rss/ rd(i)
                     ELSE
                        cgw(i) = 1. / (rd(i) + rss)
                     ENDIF
                  ENDIF
               ELSE
                  cgw(i) = 1. / rd(i)
               ENDIF
            ENDIF
         ENDDO

         ! calculate wtshi, wtsqi
         wtshi(:) = cah(:) + cgh(:)
         wtsqi(:) = caw(:) + cgw(:)

         DO i = ps, pe
            IF (fcover(i)>0 .and. lsai(i)>1.e-6) THEN
               clev = canlay(i)
               wtshi(clev) = wtshi(clev) + fcover(i)*cfh(i)
               wtsqi(clev) = wtsqi(clev) + fcover(i)*cfw(i)
            ENDIF
         ENDDO

         DO i = 1, nlay
            IF (fcover_lay(i)>0 .and. lsai_lay(i)>0) THEN
               wtshi(i) = 1./wtshi(i)
               wtsqi(i) = 1./wtsqi(i)
            ENDIF
         ENDDO

         wah(:) = cah(:) * wtshi(:)
         wgh(:) = cgh(:) * wtshi(:)

         waq(:) = caw(:) * wtsqi(:)
         wgq(:) = cgw(:) * wtsqi(:)

         ! calculate wlh, wlhl, wlq, wlql
         wlhl(:) = 0.
         wlql(:) = 0.

         DO i = ps, pe
            IF (fcover(i)>0 .and. lsai(i)>1.e-6) THEN
               clev = canlay(i)

               wlh(i) = cfh(i) * wtshi(clev) * fcover(i)
               wlhl(clev) = wlhl(clev) + wlh(i)*tl(i)

               wlq(i) = cfw(i) * wtsqi(clev) * fcover(i)
               wlql(clev) = wlql(clev) + wlq(i)*qsatl(i)
            ENDIF
         ENDDO

         ! to solve taf(:) and qaf(:)
         IF (numlay .eq. 1) THEN

            taf(toplay) = wah(toplay)*thm + wgh(toplay)*tg + wlhl(toplay)
            qaf(toplay) = waq(toplay)*qm  + wgq(toplay)*qg + wlql(toplay)
            fact = 1.
            facq = 1.

         ENDIF

         IF (numlay .eq. 2) THEN

            tmpw1 = wgh(botlay)*tg + wlhl(botlay)
            fact  = 1. - wgh(toplay)*wah(botlay)
            taf(toplay) = ( wah(toplay)*thm + wgh(toplay)*tmpw1 + wlhl(toplay) ) / fact

            tmpw1 = wgq(botlay)*qg + wlql(botlay)
            facq  = 1. - wgq(toplay)*waq(botlay)
            qaf(toplay) = ( waq(toplay)*qm  + wgq(toplay)*tmpw1 + wlql(toplay) ) / facq

            taf(botlay) = wah(botlay)*taf(toplay) + wgh(botlay)*tg + wlhl(botlay)
            qaf(botlay) = waq(botlay)*qaf(toplay) + wgq(botlay)*qg + wlql(botlay)

         ENDIF

         IF (numlay .eq. 3) THEN

            tmpw1  = wah(3)*thm + wlhl(3)
            tmpw2  = wgh(1)*tg  + wlhl(1)
            fact   = 1. - wah(2)*wgh(3) - wgh(2)*wah(1)
            taf(2) = ( wah(2)*tmpw1 + wgh(2)*tmpw2 + wlhl(2) ) / fact

            tmpw1  = waq(3)*qm + wlql(3)
            tmpw2  = wgq(1)*qg + wlql(1)
            facq   = 1. - waq(2)*wgq(3) - wgq(2)*waq(1)
            qaf(2) = ( waq(2)*tmpw1 + wgq(2)*tmpw2 + wlql(2) ) / facq

            taf(1) = wah(1)*taf(2) + wgh(1)*tg + wlhl(1)
            qaf(1) = waq(1)*qaf(2) + wgq(1)*qg + wlql(1)

            taf(3) = wah(3)*thm + wgh(3)*taf(2) + wlhl(3)
            qaf(3) = waq(3)*qm  + wgq(3)*qaf(2) + wlql(3)

         ENDIF

!-----------------------------------------------------------------------
! IR radiation, sensible and latent heat fluxes and their derivatives
!-----------------------------------------------------------------------
! the partial derivatives of areodynamical resistance are ignored
! which cannot be determined analytically

! calculate L for each canopy layer
         L(:) = 0.
         DO i = ps, pe
            IF (fcover(i)>0 .and. lsai(i)>1.e-6) THEN
               clev = canlay(i)
               ! according to absorption = emissivity, fcover -> fshade
               L(clev) = L(clev) + fshade(i) * (1-thermk(i)) * stefnc * tl(i)**4
            ENDIF
         ENDDO

! calculate Ltd
         Ltd(:) = 0.
         Ltd(3) = thermk_lay(3) * tdn(4,3) * frl
         Ltd(2) = thermk_lay(2) * ( tdn(4,2)*frl + tdn(3,2)*(Ltd(3) + L(3)) )
         Ltd(1) = thermk_lay(1) * ( tdn(4,1)*frl + tdn(3,1)*(Ltd(3) + L(3)) + &
                                    tdn(2,1)*(Ltd(2) + L(2)) )

! calculate Ld = Ltd + L
         Ld(0) = 0.
         Ld(4) = frl
         Ld(1:3) = Ltd + L

! calculate Lin = Ld * tdn
         Lin(:) = matmul(Ld(:), tdn(:,:))

! calculate Lg = (1-emg)*dlrad + emg*stefnc*tg**4
! dlrad = Lin(0)
IF (.not.DEF_SPLIT_SOILSNOW) THEN
         Lg = (1 - emg)*Lin(0) + emg*stefnc*tg**4
ELSE
         Lg = (1 - emg)*Lin(0) &
            + (1.-fsno)*emg*stefnc*t_soil**4 &
            + fsno*emg*stefnc*t_snow**4
ENDIF

! calculate Ltu
         Ltu(1) = thermk_lay(1) * tup(0,1) * Lg
         Ltu(2) = thermk_lay(2) * ( tup(0,2)*Lg + tup(1,2)*(Ltu(1) + L(1)) )
         Ltu(3) = thermk_lay(3) * ( tup(0,3)*Lg + tup(1,3)*(Ltu(1) + L(1)) + &
                                     tup(2,3)*(Ltu(2) + L(2)) )

! calculate Lu = Ltu + L
         Lu(0) = Lg
         Lu(4) = 0.
         Lu(1:3) = Ltu + L

! calculate Lin = Lin + Lu*tup
         Lin(:) = Lin(:) + matmul(Lu(:), tup(:,:))

! calculate Lv
         Lv(:) = 0.
         DO i = ps, pe
            IF (fshade(i)>0 .and. canlay(i)>0) THEN
               clev  = canlay(i)
               Lv(i) = fshade(i)/fshade_lay(clev) * (1-thermk(i)) * Lin(clev) / fcover(i) &
                     - 2. * fshade(i) * (1-thermk(i)) * stefnc * tl(i)**4 / fcover(i)
            ENDIF
         ENDDO

! calculate delta(Lv)
         dLv(:) = 0.
         DO i = ps, pe
            IF (fshade(i)>0 .and. canlay(i)>0) THEN
               clev   = canlay(i)
               dLv(i) = (4.*dLvpar(clev)*(1-emg)*fshade(i)*(1-thermk(i)) - 8.) &
                      * fshade(i) * (1-thermk(i)) * stefnc *  tl(i)**3 / fcover(i)
            ENDIF
         ENDDO

!-----------------------------------------------------------------------

         irab(:)      = Lv(:)
         dirab_dtl(:) = dLv(:)

         DO i = ps, pe

            IF (fcover(i)>0 .and. lsai(i)>1.e-6) THEN

               clev = canlay(i)
               fac(i) = 1. - thermk(i)

! sensible heat fluxes and their derivatives
               fsenl(i) = rhoair * cpair * cfh(i) * (tl(i) - taf(clev))

               ! 09/25/2017: re-written, check it carefully
               ! When numlay<3, no matter how to calculate, /fact is consistent
               IF (numlay < 3 .or. clev == 2) THEN
                  fsenl_dtl(i) = rhoair * cpair * cfh(i) * (1. - wlh(i)/fact)
               ELSE
                  IF (clev == 1) THEN
                     fsenl_dtl(i) = rhoair * cpair * cfh(i) &
                                 !* (1. - (1.-wah(2)*wgh(3))*wlh(i)/fact) or
                                  * (1. - wah(1)*wgh(2)*wlh(i)/fact - wlh(i))
                  ENDIF
                  IF (clev == 3) THEN
                     fsenl_dtl(i) = rhoair * cpair * cfh(i) &
                                 !* (1. - (1.-wgh(2)*wah(1))*wlh(i)/fact) or
                                  * (1. - wgh(3)*wah(2)*wlh(i)/fact - wlh(i))
                  ENDIF
               ENDIF

! latent heat fluxes and their derivatives

               etr(i) = rhoair * (1.-fwet(i)) * delta(i) &
                      * ( laisun(i)/(rb(i)+rssun(i)) + laisha(i)/(rb(i)+rssha(i)) ) &
                      * ( qsatl(i) - qaf(clev) )

               ! 09/25/2017: re-written
               IF (numlay < 3 .or. clev == 2) THEN
                  etr_dtl(i) = rhoair * (1.-fwet(i)) * delta(i) &
                             * ( laisun(i)/(rb(i)+rssun(i)) + laisha(i)/(rb(i)+rssha(i)) ) &
                             * (1. - wlq(i)/facq)*qsatlDT(i)
               ELSE
                  IF (clev == 1) THEN
                     etr_dtl(i) = rhoair * (1.-fwet(i)) * delta(i) &
                                * ( laisun(i)/(rb(i)+rssun(i)) + laisha(i)/(rb(i)+rssha(i)) ) &
                               !* (1. - (1.-waq(2)*wgq(3))*wlq(i)/facq)*qsatlDT(i) or
                                * (1. - waq(1)*wgq(2)*wlq(i)/facq - wlq(i))*qsatlDT(i)
                  ENDIF
                  IF (clev == 3) THEN
                     etr_dtl(i) = rhoair * (1.-fwet(i)) * delta(i) &
                                * ( laisun(i)/(rb(i)+rssun(i)) + laisha(i)/(rb(i)+rssha(i)) ) &
                               !* (1. - (1.-wgq(2)*waq(1))*wlq(i)/facq)*qsatlDT(i) or
                                * (1. - wgq(3)*waq(2)*wlq(i)/facq - wlq(i))*qsatlDT(i)
                  ENDIF
               ENDIF

               IF (.not. DEF_USE_PLANTHYDRAULICS) THEN
                  IF(etr(i).ge.etrc(i))THEN
                     etr(i) = etrc(i)
                     etr_dtl(i) = 0.
                  ENDIF
               ENDIF

               evplwet(i) = rhoair * (1.-delta(i)*(1.-fwet(i))) * lsai(i)/rb(i) &
                          * ( qsatl(i) - qaf(clev) )

               ! 09/25/2017: re-written
               IF (numlay < 3 .or. clev == 2) THEN
                  evplwet_dtl(i) = rhoair * (1.-delta(i)*(1.-fwet(i))) * lsai(i)/rb(i) &
                                 * (1. - wlq(i)/facq)*qsatlDT(i)
               ELSE
                  IF (clev == 1) THEN
                     evplwet_dtl(i) = rhoair * (1.-delta(i)*(1.-fwet(i))) * lsai(i)/rb(i) &
                                   !* (1. - (1-waq(2)*wgq(3))*wlq(i)/facq)*qsatlDT(i) or
                                    * (1. - waq(1)*wgq(2)*wlq(i)/facq - wlq(i))*qsatlDT(i)
                  ENDIF
                  IF (clev == 3) THEN
                     evplwet_dtl(i) = rhoair * (1.-delta(i)*(1.-fwet(i))) * lsai(i)/rb(i) &
                                   !* (1. - (1.-wgq(2)*waq(1))*wlq(i)/facq)*qsatlDT(i) or
                                    * (1. - wgq(3)*waq(2)*wlq(i)/facq - wlq(i))*qsatlDT(i)
                  ENDIF
               ENDIF

               ! 03/02/2018: convert evplwet from fc to whole area
               ! because ldew right now is for the whole area
               ! 09/05/2019: back to fc area
               IF(evplwet(i).ge.ldew(i)/deltim)THEN
                  evplwet(i) = ldew(i)/deltim
                  evplwet_dtl(i) = 0.
               ENDIF

               fevpl(i) = etr(i) + evplwet(i)
               fevpl_dtl(i) = etr_dtl(i) + evplwet_dtl(i)

               erre(i) = 0.
               fevpl_noadj(i) = fevpl(i)
               IF ( fevpl(i)*fevpl_bef(i) < 0. ) THEN
                  erre(i)  = -0.9*fevpl(i)
                  fevpl(i) =  0.1*fevpl(i)
               ENDIF

!-----------------------------------------------------------------------
! difference of temperatures by quasi-newton-raphson method for the non-linear system equations
! MARK#dtl
!-----------------------------------------------------------------------

               dtl(it,i) = (sabv(i) + irab(i) - fsenl(i) - hvap*fevpl(i) &
                         + cpliq*qintr_rain(i)*(t_precip-tl(i)) &
                         + cpice*qintr_snow(i)*(t_precip-tl(i))) &
                         / (clai(i)/deltim - dirab_dtl(i) + fsenl_dtl(i) + hvap*fevpl_dtl(i) &
                         + cpliq*qintr_rain(i) + cpice*qintr_snow(i))

               dtl_noadj(i) = dtl(it,i)

               ! check magnitude of change in leaf temperature limit to maximum allowed value

               IF (it .le. itmax) THEN

                ! put brakes on large temperature excursions
                  IF(abs(dtl(it,i)).gt.delmax)THEN
                     dtl(it,i) = delmax*dtl(it,i)/abs(dtl(it,i))
                  ENDIF

                ! NOTE: could be a bug if dtl*dtl==0, changed from lt->le
                  IF((it.ge.2) .and. (dtl(it-1,i)*dtl(it,i).le.0.))THEN
                     dtl(it,i) = 0.5*(dtl(it-1,i) + dtl(it,i))
                  ENDIF

               ENDIF

               tl(i) = tlbef(i) + dtl(it,i)

!-----------------------------------------------------------------------
! square roots differences of temperatures and fluxes for USE as the condition of convergences
!-----------------------------------------------------------------------

               del(i)  = sqrt( dtl(it,i)*dtl(it,i) )
               dele(i) = dtl(it,i) * dtl(it,i) * &
                       ( dirab_dtl(i)**2 + fsenl_dtl(i)**2 + (hvap*fevpl_dtl(i))**2 )
               dele(i) = sqrt(dele(i))

!-----------------------------------------------------------------------
!  saturated vapor pressures and canopy air temperature, canopy air humidity
!-----------------------------------------------------------------------
! Recalculate leaf saturated vapor pressure (ei_)for updated leaf temperature
! and adjust specific humidity (qsatl_) proportionately
               CALL qsadv(tl(i),psrf,ei(i),deiDT(i),qsatl(i),qsatlDT(i))

            ENDIF
         ENDDO !END pft loop

! update vegetation/ground surface temperature, canopy air temperature,
! canopy air humidity

         ! calculate wlhl, wlql
         wlhl(:) = 0.
         wlql(:) = 0.

         DO i = ps, pe
            IF (fcover(i)>0 .and. lsai(i)>1.e-6) THEN
               clev = canlay(i)
               wlhl(clev) = wlhl(clev) + wlh(i)*tl(i)
               wlql(clev) = wlql(clev) + wlq(i)*qsatl(i)
            ENDIF
         ENDDO

         IF (numlay .eq. 1) THEN

            taf(toplay) = wah(toplay)*thm + wgh(toplay)*tg + wlhl(toplay)
            qaf(toplay) = waq(toplay)*qm  + wgq(toplay)*qg + wlql(toplay)
            fact = 1.
            facq = 1.

         ENDIF

         IF (numlay .eq. 2) THEN

            tmpw1 = wgh(botlay)*tg + wlhl(botlay)
            fact  = 1. - wgh(toplay)*wah(botlay)
            taf(toplay) = (wah(toplay)*thm + wgh(toplay)*tmpw1 + wlhl(toplay)) / fact

            tmpw1 = wgq(botlay)*qg + wlql(botlay)
            facq  = 1. - wgq(toplay)*waq(botlay)
            qaf(toplay) = (waq(toplay)*qm  + wgq(toplay)*tmpw1 + wlql(toplay)) / facq

            taf(botlay) = wah(botlay)*taf(toplay) + wgh(botlay)*tg + wlhl(botlay)
            qaf(botlay) = waq(botlay)*qaf(toplay) + wgq(botlay)*qg + wlql(botlay)

         ENDIF

         IF (numlay .eq. 3) THEN

            tmpw1  = wah(3)*thm + wlhl(3)
            tmpw2  = wgh(1)*tg  + wlhl(1)
            fact   = 1. - wah(2)*wgh(3) - wgh(2)*wah(1)
            taf(2) = (wah(2)*tmpw1 + wgh(2)*tmpw2 + wlhl(2)) / fact

            tmpw1  = waq(3)*qm + wlql(3)
            tmpw2  = wgq(1)*qg + wlql(1)
            facq   = 1. - waq(2)*wgq(3) - wgq(2)*waq(1)
            qaf(2) = (waq(2)*tmpw1 + wgq(2)*tmpw2 + wlql(2)) / facq

            taf(1) = wah(1)*taf(2) + wgh(1)*tg + wlhl(1)
            qaf(1) = waq(1)*qaf(2) + wgq(1)*qg + wlql(1)

            taf(3) = wah(3)*thm + wgh(3)*taf(2) + wlhl(3)
            qaf(3) = waq(3)*qm  + wgq(3)*qaf(2) + wlql(3)

         ENDIF

! update co2 partial pressure within canopy air
         ! 05/02/2016: may have some problem with gdh2o, however,
         ! this variable seems never used here. Different height
         ! level vegetation should have different gdh2o, i.e.,
         ! different rd(layer) values.
         gah2o = 1.0/raw * tprcor/thm                     !mol m-2 s-1

         IF (DEF_RSS_SCHEME .eq. 4) THEN
            gdh2o = rss/rd(botlay) * tprcor/thm           !mol m-2 s-1
         ELSE
            gdh2o = 1.0/(rd(botlay)+rss) * tprcor/thm     !mol m-2 s-1
         ENDIF
         pco2a = pco2m - 1.37*psrf/max(0.446,gah2o) * &
                 sum(fcover*(assimsun + assimsha - respcsun - respcsha - rsoil))

!-----------------------------------------------------------------------
! Update monin-obukhov length and wind speed including the stability effect
!-----------------------------------------------------------------------

         dth = thm - taf(toplay)
         dqh =  qm - qaf(toplay)

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
            det = maxval(max(del,del2))
            ! 10/03/2017, yuan: possible bugs here, solution:
            ! define dee, change del => dee
            dee = maxval(max(dele,dele2))
            IF(det .lt. dtmin .and. dee .lt. dlemin) EXIT
         ENDIF

      ENDDO

! ======================================================================
!     END stability iteration
! ======================================================================

      IF(DEF_USE_OZONESTRESS)THEN
         DO i = ps, pe
            p = pftclass(i)
            CALL CalcOzoneStress(o3coefv_sun(i),o3coefg_sun(i),forc_ozone,psrf,th,ram,&
                                 rssun(i),rb(i),lai(i),lai_old(i),p,o3uptakesun(i),sabv(i),deltim)
            CALL CalcOzoneStress(o3coefv_sha(i),o3coefg_sha(i),forc_ozone,psrf,th,ram,&
                                 rssha(i),rb(i),lai(i),lai_old(i),p,o3uptakesha(i),sabv(i),deltim)
            lai_old(i) = lai(i)
            assimsun(i) = assimsun(i) * o3coefv_sun(i)
            assimsha(i) = assimsha(i) * o3coefv_sha(i)
!            rssun   (i) = rssun   (i) / o3coefg_sun(i)
!            rssha   (i) = rssha   (i) / o3coefg_sha(i)
         ENDDO
      ELSE
         DO i = ps, pe
            o3coefv_sun(i) = 1.0_r8
            o3coefg_sun(i) = 1.0_r8
            o3coefv_sha(i) = 1.0_r8
            o3coefg_sha(i) = 1.0_r8
         ENDDO
      ENDIF

      z0m  = z0mv
      zol  = zeta
      rib  = min(5.,zol*ustar**2/(vonkar**2/fh*um**2))

! canopy fluxes and total assimilation and respiration

      DO i = ps, pe
         IF (fcover(i)>0 .and. lsai(i)>1.e-6) THEN

            IF(lai(i) .gt. 0.001) THEN
               rst(i) = 1./(laisun(i)/rssun(i) + laisha(i)/rssha(i))
            ELSE
               rssun(i) = 2.0e4 ; rssha(i) = 2.0e4
               assimsun(i) = 0. ; assimsha(i) = 0.
               respcsun(i) = 0. ; respcsha(i) = 0.
               rst(i) = 2.0e4
            ENDIF
            assim(i) = assimsun(i) + assimsha(i)
            respc(i) = respcsun(i) + respcsha(i) + rsoil

! canopy fluxes and total assimilation and respiration
            fsenl(i) = fsenl(i) + fsenl_dtl(i)*dtl(it-1,i) &
                     ! add the imbalanced energy below due to T adjustment to sensible heat
                     + (dtl_noadj(i)-dtl(it-1,i)) * (clai(i)/deltim - dirab_dtl(i) &
                     + fsenl_dtl(i) + hvap*fevpl_dtl(i) &
                     + cpliq*qintr_rain(i) + cpice*qintr_snow(i)) &
                     ! add the imbalanced energy below due to q adjustment to sensible heat
                     + hvap*erre(i)

            etr0(i) = etr(i)
            etr (i) = etr(i) + etr_dtl(i)*dtl(it-1,i)

            IF (DEF_USE_PLANTHYDRAULICS) THEN
               ! Keep the layer-resolved hydraulic redistribution while
               ! applying the final leaf-temperature correction to total
               ! transpiration.  Clipping negative layer fluxes here would
               ! make the PC path inconsistent with LeafTemperature.
               IF (abs(etr0(i)) .ge. 1.e-15) THEN
                  rootflux(:,i) = rootflux(:,i) * etr(i) / etr0(i)
               ELSE
                  rootflux(:,i) = rootflux(:,i) + dz_soi / sum(dz_soi) * &
                     etr_dtl(i) * dtl(it-1,i)
               ENDIF

               IF (abs(etr(i) - sum(rootflux(:,i))) > 1.e-7_r8) THEN
                  write(6,*) 'Water balance violation in vegetation PC hydraulics', &
                     ipatch, i, etr(i), sum(rootflux(:,i)), &
                     abs(etr(i) - sum(rootflux(:,i)))
                  CALL CoLM_stop()
               ENDIF
            ENDIF

            evplwet(i) = evplwet(i) + evplwet_dtl(i)*dtl(it-1,i)
            fevpl  (i) = fevpl_noadj(i)
            fevpl  (i) = fevpl(i)   +   fevpl_dtl(i)*dtl(it-1,i)

            elwmax = ldew(i)/deltim

            ! 03/02/2018, yuan: convert fc to whole area
            ! because ldew now is for the whole area
            ! may need to change to canopy covered area
            ! 09/14/2019, yuan: change back to canopy area
            elwdif = max(0., evplwet(i)-elwmax)
            evplwet(i) = min(evplwet(i), elwmax)

            fevpl(i) = fevpl(i) - elwdif
            fsenl(i) = fsenl(i) + hvap*elwdif

            ! precipitation sensible heat from canopy
            hprl (i) = cpliq*qintr_rain(i)*(t_precip-tl(i)) + cpice*qintr_snow(i)*(t_precip-tl(i))

            ! vegetation heat change
            dheatl(i) = clai(i)/deltim*dtl(it-1,i)

!-----------------------------------------------------------------------
! Update dew accumulation (kg/m2)
!-----------------------------------------------------------------------
            IF (DEF_Interception_scheme .eq. 1) THEN !colm2014

               ldew(i) = max(0., ldew(i)-evplwet(i)*deltim)

               ! account for vegetation snow and update ldew_rain, ldew_snow, ldew
               IF ( DEF_VEG_SNOW ) THEN
                  IF (tl(i) > tfrz) THEN
                     qevpl(i) = max (evplwet(i), 0.)
                     qdewl(i) = abs (min (evplwet(i), 0.) )
                     qsubl(i) = 0.
                     qfrol(i) = 0.

                     IF (qevpl(i) > ldew_rain(i)/deltim) THEN
                        qsubl(i) = qevpl(i) - ldew_rain(i)/deltim
                        qevpl(i) = ldew_rain(i)/deltim
                     ENDIF
                  ELSE
                     qevpl(i) = 0.
                     qdewl(i) = 0.
                     qsubl(i) = max (evplwet(i), 0.)
                     qfrol(i) = abs (min (evplwet(i), 0.) )

                     IF (qsubl(i) > ldew_snow(i)/deltim) THEN
                        qevpl(i) = qsubl(i) - ldew_snow(i)/deltim
                        qsubl(i) = ldew_snow(i)/deltim
                     ENDIF
                  ENDIF

                  ldew_rain(i) = ldew_rain(i) + (qdewl(i)-qevpl(i))*deltim
                  ldew_snow(i) = ldew_snow(i) + (qfrol(i)-qsubl(i))*deltim

                  ldew(i) = ldew_rain(i) + ldew_snow(i)
               ENDIF

            ELSEIF (DEF_Interception_scheme .eq. 2) THEN!CLM4.5
               ldew(i) = max(0., ldew(i)-evplwet(i)*deltim)
            ELSEIF (DEF_Interception_scheme .eq. 3) THEN !CLM5
               IF (ldew_rain(i) .gt. evplwet(i)*deltim) THEN
                  ldew_rain(i) = ldew_rain(i)-evplwet(i)*deltim
                  ldew_snow(i) = ldew_snow(i)
                  ldew(i)=ldew_rain(i)+ldew_snow(i)
               ELSE
                  ldew_rain(i) = 0.0
                  ldew_snow(i) = max(0., ldew(i)-evplwet(i)*deltim)
                  ldew (i)     = ldew_snow(i)
               ENDIF
            ELSEIF (DEF_Interception_scheme .eq. 4) THEN !Noah-MP
               IF (ldew_rain(i) .gt. evplwet(i)*deltim) THEN
                  ldew_rain(i) = ldew_rain(i)-evplwet(i)*deltim
                  ldew_snow(i) = ldew_snow(i)
                  ldew(i)=ldew_rain(i)+ldew_snow(i)
               ELSE
                  ldew_rain(i) = 0.0
                  ldew_snow(i) = max(0., ldew(i)-evplwet(i)*deltim)
                  ldew (i)     = ldew_snow(i)
               ENDIF
            ELSEIF (DEF_Interception_scheme .eq. 5) THEN !MATSIRO
               IF (ldew_rain(i) .gt. evplwet(i)*deltim) THEN
                  ldew_rain(i) = ldew_rain(i)-evplwet(i)*deltim
                  ldew_snow(i) = ldew_snow(i)
                  ldew(i)=ldew_rain(i)+ldew_snow(i)
               ELSE
                  ldew_rain(i) = 0.0
                  ldew_snow(i) = max(0., ldew(i)-evplwet(i)*deltim)
                  ldew (i)     = ldew_snow(i)
               ENDIF
            ELSEIF (DEF_Interception_scheme .eq. 6) THEN !VIC
               IF (ldew_rain(i) .gt. evplwet(i)*deltim) THEN
                  ldew_rain(i) = ldew_rain(i)-evplwet(i)*deltim
                  ldew_snow(i) = ldew_snow(i)
                  ldew(i)=ldew_rain(i)+ldew_snow(i)
               ELSE
                  ldew_rain(i) = 0.0
                  ldew_snow(i) = max(0., ldew(i)-evplwet(i)*deltim)
                  ldew (i)     = ldew_snow(i)
               ENDIF
            ELSEIF (DEF_Interception_scheme .eq. 7) THEN !JULES
               IF (ldew_rain(i) .gt. evplwet(i)*deltim) THEN
                  ldew_rain(i) = ldew_rain(i)-evplwet(i)*deltim
                  ldew_snow(i) = ldew_snow(i)
                  ldew(i)=ldew_rain(i)+ldew_snow(i)
               ELSE
                  ldew_rain(i) = 0.0
                  ldew_snow(i) = max(0., ldew(i)-evplwet(i)*deltim)
                  ldew (i)     = ldew_snow(i)
               ENDIF
            ELSEIF (DEF_Interception_scheme .eq. 8) THEN !CoLM202x
               IF (ldew_rain(i) .gt. evplwet(i)*deltim) THEN
                  ldew_rain(i) = ldew_rain(i)-evplwet(i)*deltim
                  ldew_snow(i) = ldew_snow(i)
                  ldew(i)=ldew_rain(i)+ldew_snow(i)
               ELSE
                  ldew_rain(i) = 0.0
                  ldew_snow(i) = max(0., ldew(i)-evplwet(i)*deltim)
                  ldew (i)     = ldew_snow(i)
               ENDIF
            ELSE
               CALL CoLM_stop()
            ENDIF

            IF ( DEF_VEG_SNOW ) THEN
               ! update fwet_snow
               fwet_snow(i) = 0
               IF(ldew_snow(i) > 0.) THEN
                  fwet_snow(i) = ((10./(48.*lsai(i)))*ldew_snow(i))**.666666666666
                  ! Check for maximum limit of fwet_snow
                  fwet_snow(i) = min(fwet_snow(i),1.0)
               ENDIF

               ! phase change

               qmelt(i) = 0.
               qfrz(i)  = 0.

               IF (ldew_snow(i).gt.1.e-6 .and. tl(i).gt.tfrz) THEN
                  qmelt(i) = min(ldew_snow(i)/deltim,(tl(i)-tfrz)*cpice*ldew_snow(i)/(deltim*hfus))
                  ldew_snow(i) = max(0.,ldew_snow(i) - qmelt(i)*deltim)
                  ldew_rain(i) = max(0.,ldew_rain(i) + qmelt(i)*deltim)
                  !NOTE: There may be some problem, energy imbalance
                  !      However, detailed treatment could be somewhat trivial
                  tl(i) = fwet_snow(i)*tfrz + (1.-fwet_snow(i))*tl(i) !Niu et al., 2004
               ENDIF

               IF (ldew_rain(i).gt.1.e-6 .and. tl(i).lt.tfrz) THEN
                  qfrz(i)  = min(ldew_rain(i)/deltim,(tfrz-tl(i))*cpliq*ldew_rain(i)/(deltim*hfus))
                  ldew_rain(i) = max(0.,ldew_rain(i) - qfrz(i)*deltim)
                  ldew_snow(i) = max(0.,ldew_snow(i) + qfrz(i)*deltim)
                  !NOTE: There may be some problem, energy imbalance
                  !      However, detailed treatment could be somewhat trivial
                  tl(i) = fwet_snow(i)*tfrz + (1.-fwet_snow(i))*tl(i) !Niu et al., 2004
               ENDIF
            ENDIF

!-----------------------------------------------------------------------
! balance check
! (the computational error was created by the assumed 'dtl' in MARK#dtl)
!-----------------------------------------------------------------------

            err = sabv(i) + irab(i) + dirab_dtl(i)*dtl(it-1,i) &
                - fsenl(i) - hvap*fevpl(i) + hprl(i) &
                ! account for vegetation heat change
                - dheatl(i)

#if (defined CoLMDEBUG)
            IF(abs(err) .gt. .2) &
               write(6,*) 'energy imbalance in LeafTemperaturePC.F90', &
                          i,it-1,err,sabv(i),irab(i),fsenl(i),hvap*fevpl(i),hprl(i),dheatl(i)
#endif
         ENDIF
      ENDDO

!-----------------------------------------------------------------------
! downward (upward) longwave radiation below (above) the canopy
!-----------------------------------------------------------------------
      dlrad = Lin(0) &
            + sum( 4.* fshade * (1-thermk) * stefnc * tlbef**3 * dtl(it-1,:) )

      ulrad = Lin(4) - sum( fcover * dLv * dtl(it-1,:) ) &
            - emg * sum( 4.* fshade * (1-thermk) * stefnc * tlbef**3 * dtl(it-1,:) )

!-----------------------------------------------------------------------
! wind stresses
!-----------------------------------------------------------------------

      taux = - rhoair*us/ram
      tauy = - rhoair*vs/ram

!-----------------------------------------------------------------------
! fluxes from ground to canopy space
!-----------------------------------------------------------------------

! 03/07/2020, yuan: TODO-done, calculate fseng_soil/snow, fevpg_soil/snow
      IF (numlay .eq. 1) THEN
         ttaf = thm
         tqaf = qm
      ENDIF

      IF (numlay .eq. 2) THEN
         ttaf = taf(toplay)
         tqaf = qaf(toplay)
      ENDIF

      IF (numlay .eq. 3) THEN
         ttaf = taf(2)
         tqaf = qaf(2)
      ENDIF

      !NOTE: the below EQs for check purpose only
      ! taf = wah*thm + wgh*tg + wlh*tl
      ! taf(1) = wah(1)*taf(2) + wgh(1)*tg + wlhl(1)
      ! qaf(1) = waq(1)*qaf(2) + wgq(1)*qg + wlql(1)
      ! taf(botlay) = wah(botlay)*taf(toplay) + wgh(botlay)*tg + wlhl(botlay)
      ! qaf(botlay) = waq(botlay)*qaf(toplay) + wgq(botlay)*qg + wlql(botlay)
      ! taf(toplay) = wah(toplay)*thm + wgh(toplay)*tg + wlhl(toplay)
      ! qaf(toplay) = waq(toplay)*qm  + wgq(toplay)*qg + wlql(toplay)

      fseng = cpair*rhoair*cgh(botlay)*(tg-taf(botlay))
      fseng_soil = cpair*rhoair*cgh(botlay)*((1.-wgh(botlay))*t_soil-wah(botlay)*ttaf-wlhl(botlay))
      fseng_snow = cpair*rhoair*cgh(botlay)*((1.-wgh(botlay))*t_snow-wah(botlay)*ttaf-wlhl(botlay))

      fevpg = rhoair*cgw(botlay)*(qg-qaf(botlay))
      fevpg_soil = rhoair*cgw(botlay)*((1.-wgq(botlay))*q_soil-waq(botlay)*tqaf-wlql(botlay))
      fevpg_snow = rhoair*cgw(botlay)*((1.-wgq(botlay))*q_snow-waq(botlay)*tqaf-wlql(botlay))

!-----------------------------------------------------------------------
! Derivative of soil energy flux with respect to soil temperature (cgrnd)
!-----------------------------------------------------------------------

      !NOTE: When numlay<3, no matter how to get the solution, /fact is consistent
      IF (numlay < 3) THEN
         cgrnds = cpair*rhoair*cgh(botlay)*(1.-wgh(botlay)/fact)
         cgrndl = rhoair*cgw(botlay)*(1.-wgq(botlay)/facq)*dqgdT
      ELSE
         cgrnds = cpair*rhoair*cgh(botlay)*(1.-wah(1)*wgh(2)*wgh(1)/fact-wgh(1))
         cgrndl = rhoair*cgw(botlay)*(1.-waq(1)*wgq(2)*wgq(1)/facq-wgq(1))*dqgdT
      ENDIF

      cgrnd  = cgrnds + cgrndl*htvp

!-----------------------------------------------------------------------
! 2 m height air temperature
!-----------------------------------------------------------------------

      tref = thm + vonkar/(fh-fht)*dth * (fh2m/vonkar - fh/vonkar)
      qref =  qm + vonkar/(fq-fqt)*dqh * (fq2m/vonkar - fq/vonkar)

      IF (DEF_USE_CBL_HEIGHT) THEN
         CALL moninobuk_leddy(hu_,ht_,hq_,displa_lays(toplay),z0mv,z0hv,z0qv, &
                              profile_obu,profile_um,hpbl,diag_ustar,diag_fh2m,diag_fq2m, &
                              fm10m,diag_fm,diag_fh,diag_fq)
      ELSE
         CALL moninobuk(hu_,ht_,hq_,displa_lays(toplay),z0mv,z0hv,z0qv,profile_obu,profile_um, &
                        diag_ustar,diag_fh2m,diag_fq2m,fm10m,diag_fm,diag_fh,diag_fq)
      ENDIF

   END SUBROUTINE LeafTemperaturePC
!----------------------------------------------------------------------


   SUBROUTINE dewfraction (sigf,lai,sai,dewmx,ldew,ldew_rain,ldew_snow,fwet,fdry)
!=======================================================================
!  Original author: Yongjiu Dai, September 15, 1999
!
!  determine fraction of foliage covered by water and
!  fraction of foliage that is dry and transpiring
!
! !REVISIONS:
!  2024.04.16, Hua Yuan: add option to account for vegetation snow process
!  2018.06   , Hua Yuan: remove sigf, to compatible with PFT
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
   real(r8) :: fwet_rain              !fraction of foliage covered by water [-]
   real(r8) :: fwet_snow              !fraction of foliage covered by snow [-]

!-----------------------------------------------------------------------
! Fwet is the fraction of all vegetation surfaces which are wet
! including stem area which contribute to evaporation
      lsai = lai + sai
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

END MODULE MOD_LeafTemperaturePC
