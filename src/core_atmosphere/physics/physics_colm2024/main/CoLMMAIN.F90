#include <define.h>

SUBROUTINE CoLMMAIN ( &

         ! model running information
           ipatch,       idate,        coszen,       deltim,       &
           patchlonr,    patchlatr,    patchclass,   patchtype,    &
           doalb,        dolai,        dosst,        oro,          &

         ! soil information and lake depth
           soil_s_v_alb, soil_d_v_alb, soil_s_n_alb, soil_d_n_alb, &
           vf_quartz,    vf_gravels,   vf_om,        vf_sand,      &
           wf_gravels,   wf_sand,      porsl,        psi0,         &
           bsw,          theta_r,      fsatmax,      fsatdcf,      &
           topoweti,     alp_twi,      chi_twi,      mu_twi,       &
#ifdef vanGenuchten_Mualem_SOIL_MODEL
           alpha_vgm,    n_vgm,        L_vgm,        &
           sc_vgm,       fc_vgm,       &
#endif
           hksati,       csol,         k_solids,     dksatu,       &
           dksatf,       dkdry,        BA_alpha,     BA_beta,      &
           rootfr,       lakedepth,    dz_lake,      elvstd,  BVIC,&

         ! vegetation information
           htop,         hbot,         sqrtdi,       &
           effcon,       vmax25,       c3c4,                       &
           kmax_sun,     kmax_sha,     kmax_xyl,     kmax_root,    &
           psi50_sun,    psi50_sha,    psi50_xyl,    psi50_root,   &
           ck,           slti,         hlti,         shti,         &
           hhti,         trda,         trdm,         trop,         &
           g1,           g0,           gradm,        binter,       &
           extkn,        chil,         rho,          tau,          &
#ifdef HYPERSPECTRAL
           ! variables for hyperspectral scheme
           clr_frac,    cld_frac,                                   &
           reflectance, transmittance,                              &
           soil_alb,    kw, nw,                                     &
#endif

         ! atmospheric forcing
           forc_pco2m,   forc_po2m,    forc_us,      forc_vs,      &
           forc_t,       forc_q,       forc_prc,     forc_prl,     &
           forc_rain,    forc_snow,    forc_psrf,    forc_pbot,    &
           forc_sols,    forc_soll,    forc_solsd,   forc_solld,   &
           forc_frl,     forc_hgt_u,   forc_hgt_t,   forc_hgt_q,   &
           forc_rhoair,  &
#ifdef HYPERSPECTRAL
           forc_solarin,                                         &
#endif

           ! cbl forcing
           forc_hpbl,    &
           ! aerosol deposition
           forc_aerdep,  &

         ! land surface variables required for restart
           z_sno,        dz_sno,       t_soisno,     wliq_soisno,  &
           wice_soisno,  smp,          hk,           t_grnd,       &
           tleaf,        ldew,         ldew_rain,    ldew_snow,    &
           fwet_snow,    sag,          scv,          snowdp,       &
           fveg,         fsno,         sigf,         green,        &
           lai,          sai,          alb,          ssun,         &
           ssha,         ssoi,         ssno,         thermk,       &
           extkb,        extkd,        vegwp,        gs0sun,       &
           gs0sha,       &
#ifdef HYPERSPECTRAL
           alb_hires,    &
           sol_dir_ln_hires, sol_dif_ln_hires ,&
           sr_dir_ln_hires , sr_dif_ln_hires  ,&
           reflectance_out , transmittance_out,&
#endif
           !Ozone stress variables
           o3coefv_sun,  o3coefv_sha,  o3coefg_sun,  o3coefg_sha,  &
           lai_old,      o3uptakesun,  o3uptakesha,  forc_ozone,   &
           !End ozone stress variables
           !WUE stomata model parameter
           lambda,                                                 &
           !End WUE stomata model parameter
           zwt,          wdsrf,        wa,           wetwat,       &
           t_lake,       lake_icefrac, savedtke1,    &

         ! SNICAR snow model related
           snw_rds,      ssno_lyr,     &
           mss_bcpho,    mss_bcphi,    mss_ocpho,    mss_ocphi,    &
           mss_dst1,     mss_dst2,     mss_dst3,     mss_dst4,     &

         ! additional diagnostic variables for output
           laisun,       laisha,       rootr,        rootflux,     &
           rstfacsun_out,rstfacsha_out,gssun_out,    gssha_out,    &
           assimsun_out, etrsun_out,   assimsha_out, etrsha_out,   &
           h2osoi,       wat,          rss,          &

         ! FLUXES
           taux,         tauy,         fsena,        fevpa,        &
           lfevpa,       fsenl,        fevpl,        etr,          &
           fseng,        fevpg,        olrg,         fgrnd,        &
           trad,         tref,         qref,         t2m_wmo,      &
           frcsat,       rsur,         rsur_se,      rsur_ie,      &
           rsub,                                                   &
           rnof,         qintr,        qinfl,        qlayer,       &
           lake_deficit, qdrip,        rst,          assim,        &
           respc,        sabvsun,      sabvsha,      sabg,         &
           sr,           solvd,        solvi,        solnd,        &
           solni,        srvd,         srvi,         srnd,         &
           srni,         solvdln,      solviln,      solndln,      &
           solniln,      srvdln,       srviln,       srndln,       &
           srniln,       qcharge,      xerr,         zerr,         &

         ! TUNABLE model constants
           zlnd,         zsno,         csoilc,       dewmx,        &
           ! 'wtfact' is updated to gridded 'fsatmax' data.
           capr,         cnfac,        ssi,          wimp,         &
           pondmx,       smpmax,       smpmin,       trsmx0,       &
           tcrit,        &

         ! additional variables required by coupling with WRF model
           emis,         z0m,          zol,          rib,          &
           ustar,        qstar,        tstar,        fm,           &
           fh,           fq,           fh2m,         fq2m,         &
           fm10m,        u10m,         v10m,         chs2,          &
           cqs2,         cd10                                      )

!=======================================================================
!
!  Main subroutine, advance time information
!
!  Initial : Yongjiu Dai, 1999-2014
!  Revised : Hua Yuan, Shupeng Zhang, Nan Wei, Xingjie Lu, Zhongwang Wei, Yongjiu Dai
!            2014-2024
!
!     FLOW DIAGRAM FOR CoLMMAIN
!
!     CoLMMAIN ===>netsolar                 |> all surface
!                  rain_snow_temp           !> all surface
!
!                  LEAF_interception        |]
!                  newsnow                  |] patchtype = 0 (soil ground)
!                  THERMAL                  |]           = 1 (urban & built-up)
!                  WATER                    |]           = 2 (wetland)
!                  snowcompaction           |]           = 3 (land ice)
!                  snowlayerscombine        |]           = 4 (lake)
!                  snowlayersdivide         |]
!
!                  GLACIER_TEMP             |] glacier model
!                  GLACIER_WATER            |]
!
!                  newsnow_lake             |]
!                  laketem                  |] lake scheme
!                  snowwater_lake           |]
!
!                  SOCEAN                   |> ocean and sea ice
!
!                  orb_coszen               |> all surface
!                  EcoModel (LAI_empirical) |> land - not actived
!                  snowfraction             |> land
!                  albland                  |> land
!                  albocean                 |> ocean & sea ice
!
!=======================================================================

   USE MOD_Precision
   USE MOD_Vars_Global
   USE MOD_Const_Physical, only: tfrz, denh2o, denice, cpliq, cpice, vonkar
   USE MOD_Vars_TimeVariables, only: tlai, tsai, waterstorage
#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
   USE MOD_LandPFT, only: patch_pft_s, patch_pft_e
   USE MOD_Vars_PFTimeInvariants
   USE MOD_Vars_PFTimeVariables
#endif
   USE MOD_RainSnowTemp
#ifdef HYPERSPECTRAL
   USE MOD_NetSolar_Hyper
#else
   USE MOD_NetSolar
#endif
   USE MOD_OrbCoszen
   USE MOD_NewSnow
   USE MOD_Thermal
   USE MOD_SoilSnowHydrology
   USE MOD_SnowFraction
   USE MOD_SnowLayersCombineDivide
   USE MOD_Glacier
   USE MOD_Lake
   USE MOD_SimpleOcean
#ifdef HYPERSPECTRAL
   USE MOD_Albedo_hires
   USE MOD_HighRes_Parameters, only: get_loc_params
#else
   USE MOD_Albedo
#endif
   USE MOD_LAIEmpirical
   USE MOD_TimeManager
   USE MOD_Namelist, only: DEF_Interception_scheme, DEF_USE_VariablySaturatedFlow, &
                           DEF_USE_PLANTHYDRAULICS, DEF_USE_IRRIGATION
   USE MOD_LeafInterception
#ifdef CROP
   USE MOD_Irrigation, only: CalIrrigationApplicationFluxes
#endif
   USE MOD_MPAS_MPI

#ifdef EXTERNAL_LAKE
   USE MOD_Lake_Driver, only: external_lake
#endif

   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   real(r8),intent(in) :: deltim  !seconds in a time step [second]
   logical, intent(in) :: doalb   !true if time for surface albedo calculation
   logical, intent(in) :: dolai   !true if time for leaf area index calculation
   logical, intent(in) :: dosst   !true to update sst/ice/snow before calculation

   integer, intent(in) :: &
        ipatch        ! patch index

   real(r8), intent(in) :: &
        patchlonr   ,&! longitude in radians
        patchlatr     ! latitude in radians

   integer, intent(in) :: &
        patchclass  ,&! land patch class of USGS classification or others
        patchtype     ! land patch type (0=soil, 1=urban and built-up,
                      ! 2=wetland, 3=land ice, 4=land water bodies, 99 = ocean)

   real(r8), intent(in)    :: lakedepth         ! lake depth (m)
   real(r8), intent(inout) :: dz_lake(nl_lake)  ! lake layer thickness (m)

   real(r8), intent(in) :: &
        elvstd               ,&! standard deviation of elevation (m)
        BVIC                 ,&! vic model parameter b

        ! soil physical parameters and lake info
        soil_s_v_alb         ,&! albedo of visible of the saturated soil
        soil_d_v_alb         ,&! albedo of visible of the dry soil
        soil_s_n_alb         ,&! albedo of near infrared of the saturated soil
        soil_d_n_alb         ,&! albedo of near infrared of the dry soil

        vf_quartz  (nl_soil) ,&! volumetric fraction of quartz within mineral soil
        vf_gravels (nl_soil) ,&! volumetric fraction of gravels
        vf_om      (nl_soil) ,&! volumetric fraction of organic matter
        vf_sand    (nl_soil) ,&! volumetric fraction of sand
        wf_gravels (nl_soil) ,&! gravimetric fraction of gravels
        wf_sand    (nl_soil) ,&! gravimetric fraction of sand
        porsl      (nl_soil) ,&! fraction of soil that is voids [-]
        psi0       (nl_soil) ,&! minimum soil suction [mm]
        bsw        (nl_soil) ,&! clapp and hornberger "b" parameter [-]
        theta_r  (1:nl_soil) ,&! residual water content (cm3/cm3)
        fsatmax              ,&! maximum saturated area fraction [-]
        fsatdcf              ,&! decay factor in calculation of saturated area fraction [1/m]
        topoweti             ,&! mean topographic wetness index
        alp_twi              ,&! alpha in three parameter gamma distribution of twi
        chi_twi              ,&! chi   in three parameter gamma distribution of twi
        mu_twi               ,&! mu    in three parameter gamma distribution of twi
#ifdef vanGenuchten_Mualem_SOIL_MODEL
        alpha_vgm(1:nl_soil) ,&! parameter corresponding approximately to inverse of air-entry value
        n_vgm    (1:nl_soil) ,&! a shape parameter
        L_vgm    (1:nl_soil) ,&! pore-connectivity parameter
        sc_vgm   (1:nl_soil) ,&! saturation at air entry value in classical vanGenuchten model [-]
        fc_vgm   (1:nl_soil) ,&! a scaling factor by using air entry value in the Mualem model [-]
#endif
        hksati     (nl_soil) ,&! hydraulic conductivity at saturation [mm h2o/s]
        csol       (nl_soil) ,&! heat capacity of soil solids [J/(m3 K)]
        k_solids   (nl_soil) ,&! thermal conductivity of minerals soil [W/m-K]
        dksatu     (nl_soil) ,&! thermal conductivity of saturated unfrozen soil [W/m-K]
        dksatf     (nl_soil) ,&! thermal conductivity of saturated frozen soil [W/m-K]
        dkdry      (nl_soil) ,&! thermal conductivity for dry soil  [J/(K s m)]
        BA_alpha   (nl_soil) ,&! alpha in Balland and Arp(2005) thermal conductivity scheme
        BA_beta    (nl_soil) ,&! beta in Balland and Arp(2005) thermal conductivity scheme
        rootfr     (nl_soil) ,&! fraction of roots in each soil layer

        ! vegetation static, dynamic, derived parameters
        htop        ,&! canopy top height [m]
        hbot        ,&! canopy bottom height [m]
        sqrtdi      ,&! inverse sqrt of leaf dimension [m**-0.5]
        effcon      ,&! quantum efficiency of RuBP regeneration (mol CO2/mol quanta)
        vmax25      ,&! maximum carboxylation rate at 25 C at canopy top
        kmax_sun    ,&! Plant Hydraulics Parameters
        kmax_sha    ,&! Plant Hydraulics Parameters
        kmax_xyl    ,&! Plant Hydraulics Parameters
        kmax_root   ,&! Plant Hydraulics Parameters
        psi50_sun   ,&! water potential at 50% loss of sunlit leaf tissue conductance (mmH2O)
        psi50_sha   ,&! water potential at 50% loss of shaded leaf tissue conductance (mmH2O)
        psi50_xyl   ,&! water potential at 50% loss of xylem tissue conductance (mmH2O)
        psi50_root  ,&! water potential at 50% loss of root tissue conductance (mmH2O)
        ck          ,&! shape-fitting parameter for vulnerability curve (-)
        slti        ,&! slope of low temperature inhibition function      [s3]
        hlti        ,&! 1/2 point of low temperature inhibition function  [s4]
        shti        ,&! slope of high temperature inhibition function     [s1]
        hhti        ,&! 1/2 point of high temperature inhibition function [s2]
        trda        ,&! temperature coefficient in gs-a model             [s5]
        trdm        ,&! temperature coefficient in gs-a model             [s6]
        trop        ,&! temperature coefficient in gs-a model
        g1          ,&! conductance-photosynthesis slope parameter for medlyn model
        g0          ,&! conductance-photosynthesis intercept for medlyn model
        gradm       ,&! conductance-photosynthesis slope parameter
        binter      ,&! conductance-photosynthesis intercep
        extkn       ,&! coefficient of leaf nitrogen allocation
        chil        ,&! leaf angle distribution factor
        rho(2,2)    ,&! leaf reflectance (iw=iband, il=life and dead)
        tau(2,2)    ,&! leaf transmittance (iw=iband, il=life and dead)
#ifdef HYPERSPECTRAL
        ! hyperspectral scheme parameters
        clr_frac     ( 211, 90, 5 ),&
        cld_frac     ( 211, 5 )    ,&
        reflectance  ( 0:15, 211, 2 ),&
        transmittance( 0:15, 211, 2 ),&
        soil_alb     ( 211 )       ,&
        kw           ( 211 )       ,&
        nw           ( 211 )       ,&
#endif

        ! tunable parameters
        zlnd        ,&! roughness length for soil [m]
        zsno        ,&! roughness length for snow [m]
        csoilc      ,&! drag coefficient for soil under canopy [-]
        dewmx       ,&! maximum dew
        ! wtfact    ,&! (updated to gridded 'fsatmax') fraction of model area with high water table
        capr        ,&! tuning factor to turn first layer T into surface T
        cnfac       ,&! Crank Nicholson factor between 0 and 1
        ssi         ,&! irreducible water saturation of snow
        wimp        ,&! water impermeable if porosity less than wimp
        pondmx      ,&! ponding depth (mm)
        smpmax      ,&! wilting point potential in mm
        smpmin      ,&! restriction for min of soil poten.  (mm)
        trsmx0      ,&! max transpiration for moist soil+100% veg.  [mm/s]
        tcrit         ! critical temp. to determine rain or snow

   integer , intent(in) :: &
        c3c4          ! 1 for C3, 2 for C4

#ifdef HYPERSPECTRAL
   ! Urban hyperspectral albedo
   REAL(r8), ALLOCATABLE :: urban_albedo( :, :, : )    ! (cluster_id, season wavelength)
   REAL(r8), ALLOCATABLE :: mean_albedo ( :, : )       ! (season, wavelength)
   REAL(r8), ALLOCATABLE :: lat_north   ( :    )       ! (cluster_id)
   REAL(r8), ALLOCATABLE :: lat_south   ( :    )       ! (cluster_id)
   REAL(r8), ALLOCATABLE :: lon_east    ( :    )       ! (cluster_id)
   REAL(r8), ALLOCATABLE :: lon_west    ( :    )       ! (cluster_id)

#endif

! Forcing
!-----------------------------------------------------------------------
   real(r8), intent(in) :: &
        forc_pco2m  ,&! partial pressure of CO2 at observational height [pa]
        forc_po2m   ,&! partial pressure of O2 at observational height [pa]
        forc_us     ,&! wind speed in eastward direction [m/s]
        forc_vs     ,&! wind speed in northward direction [m/s]
        forc_t      ,&! temperature at agcm reference height [kelvin]
        forc_q      ,&! specific humidity at agcm reference height [kg/kg]
        forc_prc    ,&! convective precipitation [mm/s]
        forc_prl    ,&! large scale precipitation [mm/s]
        forc_psrf   ,&! atmosphere pressure at the surface [pa]
        forc_pbot   ,&! atmosphere pressure at the bottom of the atmos. model level [pa]
        forc_sols   ,&! atm vis direct beam solar rad onto srf [W/m2]
        forc_soll   ,&! atm nir direct beam solar rad onto srf [W/m2]
        forc_solsd  ,&! atm vis diffuse solar rad onto srf [W/m2]
        forc_solld  ,&! atm nir diffuse solar rad onto srf [W/m2]
#ifdef HYPERSPECTRAL
        forc_solarin,&! atm solar rad onto srf [W/m2]
#endif
        forc_frl    ,&! atmospheric infrared (longwave) radiation [W/m2]
        forc_hgt_u  ,&! observational height of wind [m]
        forc_hgt_t  ,&! observational height of temperature [m]
        forc_hgt_q  ,&! observational height of humidity [m]
        forc_rhoair ,&! density air [kg/m3]
        forc_hpbl   ,&! atmospheric boundary layer height [m]
        forc_aerdep(14)!atmospheric aerosol deposition data [kg/m/s]

! Variables required for restart run
!-----------------------------------------------------------------------
   integer, intent(in) :: &
        idate(3)      ! next time-step /year/julian day/second in a day/

   real(r8), intent(inout) :: oro       ! ocean(0)/seaice(2)/ flag
   real(r8), intent(inout) :: &
        z_sno      (maxsnl+1:0)       ,&! layer depth (m)
        dz_sno     (maxsnl+1:0)       ,&! layer thickness (m)
        t_soisno   (maxsnl+1:nl_soil) ,&! soil + snow layer temperature [K]
        wliq_soisno(maxsnl+1:nl_soil) ,&! liquid water (kg/m2)
        wice_soisno(maxsnl+1:nl_soil) ,&! ice lens (kg/m2)
        hk(1:nl_soil)                 ,&! hydraulic conductivity [mm h2o/s]
        smp(1:nl_soil)                ,&! soil matrix potential [mm]

        t_lake(nl_lake)               ,&! lake temperature (kelvin)
        lake_icefrac(nl_lake)         ,&! lake mass fraction of lake layer that is frozen
        savedtke1                     ,&! top level eddy conductivity (W/m K)
        vegwp(nvegwcs)                ,&! ground surface temperature [k]
        gs0sun                        ,&! working copy of sunlit stomata conductance
        gs0sha                        ,&! working copy of shalit stomata conductance
        !Ozone stress variables
        lai_old     ,&! lai in last time step
        o3uptakesun ,&! Ozone does, sunlit leaf (mmol O3/m^2)
        o3uptakesha ,&! Ozone does, shaded leaf (mmol O3/m^2)
        forc_ozone  ,&
        o3coefv_sun ,&! Ozone stress factor for photosynthesis on sunlit leaf
        o3coefv_sha ,&! Ozone stress factor for photosynthesis on sunlit leaf
        o3coefg_sun ,&! Ozone stress factor for stomata on shaded leaf
        o3coefg_sha ,&! Ozone stress factor for stomata on shaded leaf
        !End ozone stress variables
        !WUE stomata model parameter
        lambda      ,&! Marginal water cost of carbon gain ((mol h2o) (mol co2)-1)
        !WUE stomata model parameter
        t_grnd      ,&! ground surface temperature [k]
        tleaf       ,&! leaf temperature [K]
        ldew        ,&! depth of water on foliage [kg/m2/s]
        ldew_rain   ,&! depth of rain on foliage[kg/m2/s]
        ldew_snow   ,&! depth of snow on foliage[kg/m2/s]
        fwet_snow   ,&! vegetation canopy snow fractional cover [-]
        sag         ,&! non dimensional snow age [-]
        scv         ,&! snow mass (kg/m2)
        snowdp      ,&! snow depth (m)
        zwt         ,&! the depth to water table [m]
        wdsrf       ,&! depth of surface water [mm]
        wa          ,&! water storage in aquifer [mm]
        wetwat      ,&! water storage in wetland [mm]

        snw_rds   ( maxsnl+1:0 )      ,&! effective grain radius (col,lyr) [microns, m-6]
        mss_bcpho ( maxsnl+1:0 )      ,&! mass of hydrophobic BC in snow  (col,lyr) [kg]
        mss_bcphi ( maxsnl+1:0 )      ,&! mass of hydrophillic BC in snow (col,lyr) [kg]
        mss_ocpho ( maxsnl+1:0 )      ,&! mass of hydrophobic OC in snow  (col,lyr) [kg]
        mss_ocphi ( maxsnl+1:0 )      ,&! mass of hydrophillic OC in snow (col,lyr) [kg]
        mss_dst1  ( maxsnl+1:0 )      ,&! mass of dust species 1 in snow  (col,lyr) [kg]
        mss_dst2  ( maxsnl+1:0 )      ,&! mass of dust species 2 in snow  (col,lyr) [kg]
        mss_dst3  ( maxsnl+1:0 )      ,&! mass of dust species 3 in snow  (col,lyr) [kg]
        mss_dst4  ( maxsnl+1:0 )      ,&! mass of dust species 4 in snow  (col,lyr) [kg]
        ssno_lyr  (2,2,maxsnl+1:1)    ,&! snow layer absorption [-]

        fveg        ,&! fraction of vegetation cover
        fsno        ,&! fractional snow cover
        sigf        ,&! fraction of veg cover, excluding snow-covered veg [-]
        green       ,&! greenness
        lai         ,&! leaf area index
        sai         ,&! stem area index
#ifdef HYPERSPECTRAL
        alb_hires(211, 2),& ! hyperspectral albedo
#endif

        coszen      ,&! cosine of solar zenith angle
        alb(2,2)    ,&! averaged albedo [-]
        ssun(2,2)   ,&! sunlit canopy absorption for solar radiation
        ssha(2,2)   ,&! shaded canopy absorption for solar radiation
        ssoi(2,2)   ,&! ground soil absorption [-]
        ssno(2,2)   ,&! ground snow absorption [-]
        thermk      ,&! canopy gap fraction for tir radiation
        extkb       ,&! (k, g(mu)/mu) direct solar extinction coefficient
        extkd         ! diffuse and scattered diffuse PAR extinction coefficient


! additional diagnostic variables for output
   real(r8), intent(out) :: &
        laisun           ,&! sunlit leaf area index
        laisha           ,&! shaded leaf area index
        rstfacsun_out    ,&! factor of soil water stress
        rstfacsha_out    ,&! factor of soil water stress
        gssun_out        ,&! sunlit stomata conductance
        gssha_out        ,&! shaded stomata conductance
        wat              ,&! total water storage
        rss              ,&! soil surface resistance [s/m]
        rootr(nl_soil)   ,&! water uptake fraction from different layers, all layers add to 1.0
        rootflux(nl_soil),&! water exchange between soil and root in different layers
                           ! Positive: soil->root[?]
#ifdef HYPERSPECTRAL
        reflectance_out  (211, 0:15)  ,&! high resolution reflectance
        transmittance_out(211, 0:15)  ,&! high resolution transmittance
#endif
        h2osoi(nl_soil)  ,&! volumetric soil water in layers [m3/m3]
        qlayer(0:nl_soil),&! water flux at between soil layer [mm h2o/s]
        lake_deficit       ! lake deficit due to evaporation (mm h2o/s)

   real(r8), intent(out) :: &
        assimsun_out,&
        etrsun_out  ,&
        assimsha_out,&
        etrsha_out
! Fluxes
!-----------------------------------------------------------------------
   real(r8), intent(out) :: &
        taux        ,&! wind stress: E-W [kg/m/s**2]
        tauy        ,&! wind stress: N-S [kg/m/s**2]
        fsena       ,&! sensible heat from canopy height to atmosphere [W/m2]
        fevpa       ,&! evapotranspiration from canopy height to atmosphere [mm/s]
        lfevpa      ,&! latent heat flux from canopy height to atmosphere [W/2]
        fsenl       ,&! sensible heat from leaves [W/m2]
        fevpl       ,&! evaporation+transpiration from leaves [mm/s]
        etr         ,&! transpiration rate [mm/s]
        fseng       ,&! sensible heat flux from ground [W/m2]
        fevpg       ,&! evaporation heat flux from ground [mm/s]
        olrg        ,&! outgoing long-wave radiation from ground+canopy
        fgrnd       ,&! ground heat flux [W/m2]
        xerr        ,&! water balance error at current time-step [mm/s]
        zerr        ,&! energy balance error at current time-step [W/m2]

        tref        ,&! 2 m height air temperature [K]
        qref        ,&! 2 m height air specific humidity
        t2m_wmo     ,&! 2 m wmo std air temperature [K]
        trad        ,&! radiative temperature [K]
        frcsat      ,&! fraction of saturation area
        rsur        ,&! surface runoff (mm h2o/s)
        rsur_se     ,&! saturation excess surface runoff (mm h2o/s)
        rsur_ie     ,&! infiltration excess surface runoff (mm h2o/s)
        rsub        ,&! subsurface runoff (mm h2o/s)
        rnof        ,&! total runoff (mm h2o/s)
        qintr       ,&! interception (mm h2o/s)
        qinfl       ,&! infiltration (mm h2o/s)
        qdrip       ,&! throughfall (mm h2o/s)
        qcharge     ,&! groundwater recharge [mm/s]

        rst         ,&! canopy stomatal resistance
        assim       ,&! canopy assimilation
        respc       ,&! canopy respiration

        sabvsun     ,&! solar absorbed by sunlit vegetation [W/m2]
        sabvsha     ,&! solar absorbed by shaded vegetation [W/m2]
        sabg        ,&! solar absorbed by ground  [W/m2]
        sr          ,&! total reflected solar radiation (W/m2)
        solvd       ,&! incident direct beam vis solar radiation (W/m2)
        solvi       ,&! incident diffuse beam vis solar radiation (W/m2)
        solnd       ,&! incident direct beam nir solar radiation (W/m2)
        solni       ,&! incident diffuse beam nir solar radiation (W/m2)
        srvd        ,&! reflected direct beam vis solar radiation (W/m2)
        srvi        ,&! reflected diffuse beam vis solar radiation (W/m2)
        srnd        ,&! reflected direct beam nir solar radiation (W/m2)
        srni        ,&! reflected diffuse beam nir solar radiation (W/m2)
        solvdln     ,&! incident direct beam vis solar radiation at local noon(W/m2)
        solviln     ,&! incident diffuse beam vis solar radiation at local noon(W/m2)
        solndln     ,&! incident direct beam nir solar radiation at local noon(W/m2)
        solniln     ,&! incident diffuse beam nir solar radiation at local noon(W/m2)
        srvdln      ,&! reflected direct beam vis solar radiation at local noon(W/m2)
        srviln      ,&! reflected diffuse beam vis solar radiation at local noon(W/m2)
        srndln      ,&! reflected direct beam nir solar radiation at local noon(W/m2)
        srniln      ,&! reflected diffuse beam nir solar radiation at local noon(W/m2)
#ifdef HYPERSPECTRAL
        sol_dir_ln_hires(211)  ,&! incident direct beam vis solar radiation at local noon(W/m2)
        sol_dif_ln_hires(211)  ,&! incident diffuse beam vis solar radiation at local noon(W/m2)
        sr_dir_ln_hires(211)   ,&! reflected direct beam nir solar radiation at local noon(W/m2)
        sr_dif_ln_hires(211)   ,&! reflected diffuse beam nir solar radiation at local noon(W/m2)
#endif

        forc_rain   ,&! rain [mm/s]
        forc_snow   ,&! snow [mm/s]

        emis        ,&! averaged bulk surface emissivity
        z0m         ,&! effective roughness [m]
        zol         ,&! dimensionless height (z/L) used in Monin-Obukhov theory
        rib         ,&! bulk Richardson number in surface layer
        ustar       ,&! u* in similarity theory [m/s]
        qstar       ,&! q* in similarity theory [kg/kg]
        tstar       ,&! t* in similarity theory [K]
        fm          ,&! integral of profile function for momentum
        fh          ,&! integral of profile function for heat
        fq          ,&! integral of profile function for moisture
        fh2m        ,&! relation for temperature at 2m
        fq2m        ,&! relation for specific humidity at 2m
        fm10m       ,&! integral of profile function for momentum at 10m
        u10m        ,&! area-effective 10 m zonal wind [m/s]
        v10m        ,&! area-effective 10 m meridional wind [m/s]
        chs2        ,&! area-effective 2 m heat exchange velocity [m/s]
        cqs2        ,&! area-effective 2 m moisture exchange velocity [m/s]
        cd10         ! area-effective 10 m momentum coefficient

!-------------------------- Local Variables ----------------------------
   logical  :: is_dry_lake

   real(r8) :: &
        calday      ,&! Julian cal day (1.xx to 365.xx)
        endwb       ,&! water mass at the end of time step
        errore      ,&! energy balance error (Wm-2)
        errorw      ,&! water balance error (mm)
        fiold(maxsnl+1:nl_soil), &! fraction of ice relative to the total water
        w_old       ,&! liquid water mass of the column at the previous time step (mm)

        sabg_soil   ,&! solar absorbed by soil fraction
        sabg_snow   ,&! solar absorbed by snow fraction
        parsun      ,&! PAR by sunlit leaves [W/m2]
        parsha      ,&! PAR by shaded leaves [W/m2]
        qseva       ,&! ground surface evaporation rate (mm h2o/s)
        qsdew       ,&! ground surface dew formation (mm h2o /s) [+]
        qsubl       ,&! sublimation rate from snow pack (mm h2o /s) [+]
        qfros       ,&! surface dew added to snow pack (mm h2o /s) [+]
        qseva_soil  ,&! ground soil surface evaporation rate (mm h2o/s)
        qsdew_soil  ,&! ground soil surface dew formation (mm h2o /s) [+]
        qsubl_soil  ,&! sublimation rate from soil ice pack (mm h2o /s) [+]
        qfros_soil  ,&! surface dew added to soil ice pack (mm h2o /s) [+]
        qseva_snow  ,&! ground snow surface evaporation rate (mm h2o/s)
        qsdew_snow  ,&! ground snow surface dew formation (mm h2o /s) [+]
        qsubl_snow  ,&! sublimation rate from snow pack (mm h2o /s) [+]
        qfros_snow  ,&! surface dew added to snow pack (mm h2o /s) [+]
        scvold      ,&! snow cover for previous time step [mm]
        sm          ,&! rate of snowmelt [kg/(m2 s)]
        ssw         ,&! water volumetric content of soil surface layer [m3/m3]
        tssub(7)    ,&! surface/sub-surface temperatures [K]
        tssea       ,&! sea surface temperature [K]
        totwb       ,&! water mass at the beginning of time step
        wt          ,&! fraction of vegetation buried (covered) by snow [-]
        z_soisno (maxsnl+1:nl_soil), &! layer depth (m)
        dz_soisno(maxsnl+1:nl_soil), &! layer thickness (m)
        zi_soisno(maxsnl  :nl_soil)   ! interface level below a "z" level (m)

   real(r8) :: &
        prc_rain    ,&! convective rainfall [kg/(m2 s)]
        prc_snow    ,&! convective snowfall [kg/(m2 s)]
        prl_rain    ,&! large scale rainfall [kg/(m2 s)]
        prl_snow    ,&! large scale snowfall [kg/(m2 s)]
        t_precip    ,&! snowfall/rainfall temperature [kelvin]
        bifall      ,&! bulk density of newly fallen dry snow [kg/m3]
        pg_rain     ,&! rainfall onto ground including canopy runoff [kg/(m2 s)]
        pg_snow     ,&! snowfall onto ground including canopy runoff [kg/(m2 s)]
        qintr_rain  ,&! rainfall interception (mm h2o/s)
        qintr_snow    ! snowfall interception (mm h2o/s)

#ifdef HYPERSPECTRAL
  real(r8) :: &
        dir_frac(211),&! direct beam fraction
        dif_frac(211)  ! diffuse beam fraction
#endif

   integer snl      ,&! number of snow layers
        imelt(maxsnl+1:nl_soil), &! flag for: melting=1, freezing=2, Nothing happened=0
        lb ,lbsn    ,&! lower bound of arrays
        j             ! do looping index

   ! For SNICAR snow model
   !----------------------------------------------------------------------
   integer  snl_bef                    !number of snow layers
   real(r8) forc_aer           ( 14 )  !aerosol deposition from atmosphere (grd,aer) [kg m-1 s-1]
   real(r8) snofrz       (maxsnl+1:0)  !snow freezing rate (col,lyr) [kg m-2 s-1]
   real(r8) t_soisno_    (maxsnl+1:1)  !soil + snow layer temperature [K]
   real(r8) dz_soisno_   (maxsnl+1:1)  !layer thickness (m)
   real(r8) sabg_snow_lyr(maxsnl+1:1)  !snow layer absorption [W/m-2]
   !----------------------------------------------------------------------
   !  For irrigation
   !----------------------------------------------------------------------
   real(r8) :: qflx_irrig_drip         ! drip irrigation rate [mm/s]
   real(r8) :: qflx_irrig_sprinkler    ! sprinkler irrigation rate [mm/s]
   real(r8) :: qflx_irrig_flood        ! flood irrigation rate [mm/s]
   real(r8) :: qflx_irrig_paddy        ! paddy irrigation rate [mm/s]
   !----------------------------------------------------------------------
   real(r8) :: a, aa, gwat
   real(r8) :: wextra, t_rain, t_snow
   integer ps, pe, pc

!-----------------------------------------------------------------------

      z_soisno (maxsnl+1:0) = z_sno (maxsnl+1:0)
      z_soisno (1:nl_soil ) = z_soi (1:nl_soil )
      dz_soisno(maxsnl+1:0) = dz_sno(maxsnl+1:0)
      dz_soisno(1:nl_soil ) = dz_soi(1:nl_soil )

      ! SNICAR initialization
      ! ---------------------

      ! snow freezing rate (col,lyr) [kg m-2 s-1]
      snofrz(:) = 0.

      ! aerosol deposition value
      IF (DEF_Aerosol_Readin) THEN
         forc_aer(:) = forc_aerdep   ! read from outside forcing file
      ELSE
         forc_aer(:) = 0.            ! manual setting
      ENDIF


!======================================================================
!  [1] Solar absorbed by vegetation and ground
!      and precipitation information (rain/snow fall and precip temperature
!======================================================================
#ifdef HYPERSPECTRAL
      CALL get_loc_params(forc_solarin, idate, coszen, patchlatr, patchlonr, clr_frac, cld_frac, dir_frac, dif_frac)

      CALL netsolar_hyper (ipatch,idate,deltim,patchlonr,patchtype,&
                     forc_sols,forc_soll,forc_solsd,forc_solld,&
                     alb,ssun,ssha,lai,sai,rho,tau,ssoi,ssno,ssno_lyr,fsno,&
                     parsun,parsha,sabvsun,sabvsha,sabg,sabg_soil,sabg_snow,sabg_snow_lyr,&
                     sr,solvd,solvi,solnd,solni,srvd,srvi,srnd,srni,&
                     solvdln,solviln,solndln,solniln,srvdln,srviln,srndln,srniln,&
                     ! new variables for hyperspectral scheme
                     dir_frac, dif_frac, alb_hires    ,&
                     sol_dir_ln_hires,sol_dif_ln_hires,&
                     sr_dir_ln_hires ,sr_dif_ln_hires  )

#else
      CALL netsolar (ipatch,idate,deltim,patchlonr,patchtype,&
                     forc_sols,forc_soll,forc_solsd,forc_solld,&
                     alb,ssun,ssha,lai,sai,rho,tau,ssoi,ssno,ssno_lyr,fsno,&
                     parsun,parsha,sabvsun,sabvsha,sabg,sabg_soil,sabg_snow,sabg_snow_lyr,&
                     sr,solvd,solvi,solnd,solni,srvd,srvi,srnd,srni,&
                     solvdln,solviln,solndln,solniln,srvdln,srviln,srndln,srniln)
#endif

      CALL rain_snow_temp (patchtype, &
                           forc_t,forc_q,forc_psrf,forc_prc,forc_prl,forc_us,forc_vs,tcrit,&
                           prc_rain,prc_snow,prl_rain,prl_snow,t_precip,bifall)

#ifdef MPAS_EMBEDDED_COLM
      ! MPAS already provides hydrometeor phase; keep it while using CoLM for precipitation temperature.
      prc_rain = max(0._r8, min(forc_prc, forc_rain))
      prc_snow = max(0._r8, forc_prc - prc_rain)
      prl_rain = max(0._r8, forc_rain - prc_rain)
      prl_snow = max(0._r8, forc_snow - prc_snow)
#else
      forc_rain = prc_rain + prl_rain
      forc_snow = prc_snow + prl_snow
#endif

!======================================================================

      is_dry_lake = DEF_USE_Dynamic_Lake .and. (patchtype == 4) .and. &
                    ((wdsrf < 100.) .or. (zwt > 0.))


                                                  !         / SOIL GROUND          (patchtype = 0)
      IF ((patchtype <= 2) .or. is_dry_lake) THEN ! <=== is - URBAN and BUILT-UP   (patchtype = 1)
                                                  !         \ WETLAND              (patchtype = 2)
                                                  !           Dry Lake             (patchtype = 4)

! NOTE: PFT and PC are only for soil patches, i.e., patchtype=0.
!======================================================================
                         ! initial set
         scvold = scv    ! snow mass at previous time step

         snl = 0
         DO j=maxsnl+1,0
            IF(wliq_soisno(j)+wice_soisno(j)>0.) snl=snl-1
         ENDDO

         zi_soisno(0)=0.
         IF (snl < 0) THEN
            DO j = -1, snl, -1
               zi_soisno(j)=zi_soisno(j+1)-dz_soisno(j+1)
            ENDDO
         ENDIF
         DO j = 1,nl_soil
            zi_soisno(j)=zi_soisno(j-1)+dz_soisno(j)
         ENDDO

         totwb = ldew + scv + sum(wice_soisno(1:)+wliq_soisno(1:)) + wa
#ifdef CROP
         if(DEF_USE_IRRIGATION) totwb = totwb + waterstorage(ipatch)
#endif
         totwb = totwb + wdsrf
         IF (DEF_USE_VariablySaturatedFlow) THEN
            IF (patchtype == 2) THEN
               totwb = totwb + wetwat
            ENDIF
         ENDIF

         fiold(:) = 0.0
         IF (snl <0 ) THEN
            fiold(snl+1:0)=wice_soisno(snl+1:0)/(wliq_soisno(snl+1:0)+wice_soisno(snl+1:0))
         ENDIF

!----------------------------------------------------------------------
! [2] Irrigation
!----------------------------------------------------------------------
         qflx_irrig_drip = 0._r8
         qflx_irrig_sprinkler = 0._r8
         qflx_irrig_flood = 0._r8
         qflx_irrig_paddy = 0._r8
#ifdef CROP
         IF (DEF_USE_IRRIGATION) THEN
            IF (patchtype == 0) THEN
               CALL CalIrrigationApplicationFluxes(ipatch,deltim,qflx_irrig_drip,qflx_irrig_sprinkler,qflx_irrig_flood,qflx_irrig_paddy)
            ENDIF
         ENDIF
#endif
!----------------------------------------------------------------------
! [3] Canopy interception and precipitation onto ground surface
!----------------------------------------------------------------------
         IF (patchtype == 0) THEN

#if (defined LULC_USGS || defined LULC_IGBP)
            CALL LEAF_interception_wrap (deltim,dewmx,forc_us,forc_vs,chil,sigf,lai,sai,forc_t,&
                      tleaf,prc_rain,prc_snow,prl_rain,prl_snow,qflx_irrig_sprinkler,bifall,&
                      ldew,ldew_rain,ldew_snow,z0m,forc_hgt_u,pg_rain,&
                      pg_snow,qintr,qintr_rain,qintr_snow)
#endif

#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
            CALL LEAF_interception_pftwrap (ipatch,deltim,dewmx,forc_us,forc_vs,forc_t,&
                      prc_rain,prc_snow,prl_rain,prl_snow,qflx_irrig_sprinkler,bifall,&
                      ldew,ldew_rain,ldew_snow,z0m,forc_hgt_u,pg_rain,&
                      pg_snow,qintr,qintr_rain,qintr_snow)
#endif

         ELSE
            CALL LEAF_interception_wrap (deltim,dewmx,forc_us,forc_vs,chil,sigf,lai,sai,forc_t,&
                      tleaf,prc_rain,prc_snow,prl_rain,prl_snow,qflx_irrig_sprinkler,bifall,&
                      ldew,ldew_rain,ldew_snow,z0m,forc_hgt_u,pg_rain,&
                      pg_snow,qintr,qintr_rain,qintr_snow)
         ENDIF

         qdrip = pg_rain + pg_snow

!----------------------------------------------------------------------
! [3] Initialize new snow nodes for snowfall / sleet
!----------------------------------------------------------------------

         snl_bef = snl

         CALL newsnow (patchtype,maxsnl,deltim,t_grnd,pg_rain,pg_snow,bifall,&
                       t_precip,zi_soisno(:0),z_soisno(:0),dz_soisno(:0),t_soisno(:0),&
                       wliq_soisno(:0),wice_soisno(:0),fiold(:0),snl,sag,scv,snowdp,fsno,wetwat)

!----------------------------------------------------------------------
! [4] Energy and Water balance
!----------------------------------------------------------------------
         lb   = snl + 1           !lower bound of array
         lbsn = min(lb,0)

         CALL THERMAL (ipatch,patchtype,is_dry_lake,lb                ,deltim            ,&
              trsmx0            ,zlnd              ,zsno              ,csoilc            ,&
              dewmx             ,capr              ,cnfac             ,vf_quartz         ,&
              vf_gravels        ,vf_om             ,vf_sand           ,wf_gravels        ,&
              wf_sand           ,csol              ,porsl             ,psi0              ,&
#ifdef Campbell_SOIL_MODEL
              bsw               ,&
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
              theta_r           ,alpha_vgm         ,n_vgm             ,L_vgm             ,&
              sc_vgm            ,fc_vgm            ,&
#endif
              k_solids          ,dksatu            ,dksatf            ,dkdry             ,&
              BA_alpha          ,BA_beta           ,lai               ,laisun            ,&
              laisha            ,sai               ,htop              ,hbot              ,&
              sqrtdi            ,rootfr            ,rstfacsun_out     ,rstfacsha_out     ,&
              rss               ,gssun_out         ,gssha_out         ,assimsun_out      ,&
              etrsun_out        ,assimsha_out      ,etrsha_out        ,&

              effcon            ,vmax25,c3c4       ,hksati            ,smp ,hk           ,&
              kmax_sun          ,kmax_sha          ,kmax_xyl          ,kmax_root         ,&
              psi50_sun         ,psi50_sha         ,psi50_xyl         ,psi50_root        ,&
              ck                ,vegwp             ,gs0sun            ,gs0sha            ,&
              !Ozone stress variables
              o3coefv_sun       ,o3coefv_sha       ,o3coefg_sun       ,o3coefg_sha       ,&
              lai_old           ,o3uptakesun       ,o3uptakesha       ,forc_ozone        ,&
              !End ozone stress variables
              !WUE stomata model parameter
              lambda      ,&! Marginal water cost of carbon gain ((mol h2o) (mol co2)-1)
              !WUE stomata model parameter
              slti              ,hlti              ,shti              ,hhti              ,&
              trda              ,trdm              ,trop              ,g1                ,&
              g0                ,gradm             ,binter            ,extkn             ,&
              forc_hgt_u        ,forc_hgt_t        ,forc_hgt_q        ,forc_us           ,&
              forc_vs           ,forc_t            ,forc_q            ,forc_rhoair       ,&
              forc_psrf         ,forc_pco2m        ,forc_hpbl         ,forc_po2m         ,&
              coszen            ,parsun            ,parsha            ,sabvsun           ,&
              sabvsha           ,sabg              ,sabg_soil         ,sabg_snow         ,&
              forc_frl          ,extkb             ,extkd             ,thermk            ,&
              fsno              ,sigf              ,dz_soisno(lb:)    ,z_soisno(lb:)     ,&
              zi_soisno(lb-1:)  ,tleaf             ,t_soisno(lb:)     ,wice_soisno(lb:)  ,&
              wliq_soisno(lb:)  ,ldew              ,ldew_rain         ,ldew_snow         ,&
              fwet_snow         ,scv               ,snowdp            ,imelt(lb:)        ,&
              taux              ,tauy              ,fsena             ,fevpa             ,&
              lfevpa            ,fsenl             ,fevpl             ,etr               ,&
              fseng             ,fevpg             ,olrg              ,fgrnd             ,&
              rootr             ,rootflux          ,qseva             ,qsdew             ,&
              qsubl             ,qfros             ,qseva_soil        ,qsdew_soil        ,&
              qsubl_soil        ,qfros_soil        ,qseva_snow        ,qsdew_snow        ,&
              qsubl_snow        ,qfros_snow        ,sm                ,tref              ,&
              qref              ,trad              ,rst               ,assim             ,&
              respc             ,errore            ,emis              ,z0m               ,&
              zol               ,rib               ,ustar             ,qstar             ,&
              tstar             ,fm                ,fh                ,fq                ,&
              fh2m              ,fq2m              ,fm10m             ,&
              u10m              ,v10m              ,chs2              ,cqs2              ,cd10              ,&
              pg_rain           ,pg_snow           ,t_precip          ,qintr_rain        ,&
              qintr_snow        ,snofrz(lbsn:0)    ,sabg_snow_lyr(lb:1)                   )

         IF (.not. DEF_USE_VariablySaturatedFlow) THEN

            CALL WATER_2014 (ipatch,patchtype         ,lb                ,nl_soil           ,&
                 deltim            ,z_soisno(lb:)     ,dz_soisno(lb:)    ,zi_soisno(lb-1:)  ,&
                 bsw               ,porsl             ,psi0              ,hksati            ,&
                 theta_r           ,fsatmax           ,fsatdcf           ,elvstd            ,&
                 BVIC              ,rootr             ,rootflux          ,t_soisno(lb:)     ,&
                 wliq_soisno(lb:)  ,wice_soisno(lb:)  ,smp               ,hk                ,&
                 pg_rain           ,sm                ,etr               ,qseva             ,&
                 qsdew             ,qsubl             ,qfros             ,qseva_soil        ,&
                 qsdew_soil        ,qsubl_soil        ,qfros_soil        ,qseva_snow        ,&
                 qsdew_snow        ,qsubl_snow        ,qfros_snow        ,fsno              ,&
                 rsur              ,rnof              ,qinfl             ,pondmx            ,&
                 ssi               ,wimp              ,smpmin            ,zwt               ,&
                 wdsrf             ,wa                ,qcharge           ,&

! SNICAR model variables
                 forc_aer          ,&
                 mss_bcpho(lbsn:0) ,mss_bcphi(lbsn:0) ,mss_ocpho(lbsn:0) ,mss_ocphi(lbsn:0) ,&
                 mss_dst1(lbsn:0)  ,mss_dst2(lbsn:0)  ,mss_dst3(lbsn:0)  ,mss_dst4(lbsn:0)  ,&
!  irrigation variables
                 qflx_irrig_drip   ,qflx_irrig_flood  ,qflx_irrig_paddy)
                 rsub = rnof - rsur
         ELSE

            CALL WATER_VSF (ipatch ,patchtype,is_dry_lake,   lb          ,nl_soil           ,&
                 deltim            ,z_soisno(lb:)     ,dz_soisno(lb:)    ,zi_soisno(lb-1:)  ,&
                 bsw               ,theta_r           ,fsatmax           ,fsatdcf           ,&
                 topoweti          ,alp_twi           ,chi_twi           ,mu_twi            ,&
                 elvstd            ,BVIC              ,&
#ifdef vanGenuchten_Mualem_SOIL_MODEL
                 alpha_vgm         ,n_vgm             ,L_vgm             ,sc_vgm            ,&
                 fc_vgm            ,&
#endif
                 porsl             ,psi0              ,hksati            ,rootr             ,&
                 rootflux          ,t_soisno(lb:)     ,wliq_soisno(lb:)  ,wice_soisno(lb:)  ,&
                 smp               ,hk                ,pg_rain           ,sm                ,&
                 etr               ,qseva             ,qsdew             ,qsubl             ,&
                 qfros             ,qseva_soil        ,qsdew_soil        ,qsubl_soil        ,&
                 qfros_soil        ,qseva_snow        ,qsdew_snow        ,qsubl_snow        ,&
                 qfros_snow        ,fsno              ,frcsat            ,rsur              ,&
                 rsur_se           ,rsur_ie           ,rsub              ,rnof              ,&
                 qinfl                                                                      ,&
                 qlayer            ,ssi               ,pondmx            ,wimp              ,&
                 zwt               ,wdsrf             ,wa                ,wetwat            ,&
! SNICAR model variables
                 forc_aer          ,&
                 mss_bcpho(lbsn:0) ,mss_bcphi(lbsn:0) ,mss_ocpho(lbsn:0) ,mss_ocphi(lbsn:0) ,&
                 mss_dst1(lbsn:0)  ,mss_dst2(lbsn:0)  ,mss_dst3(lbsn:0)  ,mss_dst4(lbsn:0)  ,&
!  irrigation variables
                 qflx_irrig_drip   ,qflx_irrig_flood  ,qflx_irrig_paddy)
         ENDIF

         IF (snl < 0) THEN
            ! Compaction rate for snow
            ! Natural compaction and metamorphosis. The compaction rate
            ! is recalculated for every new timestep
            lb  = snl + 1   !lower bound of array
            CALL snowcompaction (lb,deltim,&
                            imelt(lb:0),fiold(lb:0),t_soisno(lb:0),&
                            wliq_soisno(lb:0),wice_soisno(lb:0),forc_us,forc_vs,dz_soisno(lb:0))

            ! Combine thin snow elements
            lb = maxsnl + 1

            IF (DEF_USE_SNICAR) THEN
               CALL snowlayerscombine_snicar (lb,snl,&
                            z_soisno(lb:1),dz_soisno(lb:1),zi_soisno(lb-1:1),&
                            wliq_soisno(lb:1),wice_soisno(lb:1),t_soisno(lb:1),scv,snowdp,&
                            mss_bcpho(lb:0), mss_bcphi(lb:0), mss_ocpho(lb:0), mss_ocphi(lb:0),&
                            mss_dst1(lb:0), mss_dst2(lb:0), mss_dst3(lb:0), mss_dst4(lb:0) )
            ELSE
               CALL snowlayerscombine (lb,snl,&
                            z_soisno(lb:1),dz_soisno(lb:1),zi_soisno(lb-1:1),&
                            wliq_soisno(lb:1),wice_soisno(lb:1),t_soisno(lb:1),scv,snowdp)
            ENDIF

            ! Divide thick snow elements
            IF(snl<0) THEN
               IF (DEF_USE_SNICAR) THEN
                  CALL snowlayersdivide_snicar (lb,snl,&
                            z_soisno(lb:0),dz_soisno(lb:0),zi_soisno(lb-1:0),&
                            wliq_soisno(lb:0),wice_soisno(lb:0),t_soisno(lb:0),&
                            mss_bcpho(lb:0),mss_bcphi(lb:0),mss_ocpho(lb:0),mss_ocphi(lb:0),&
                            mss_dst1(lb:0),mss_dst2(lb:0),mss_dst3(lb:0),mss_dst4(lb:0) )
               ELSE
                  CALL snowlayersdivide (lb,snl,&
                            z_soisno(lb:0),dz_soisno(lb:0),zi_soisno(lb-1:0),&
                            wliq_soisno(lb:0),wice_soisno(lb:0),t_soisno(lb:0))
               ENDIF
            ENDIF
         ENDIF

         ! Set zero to the empty node
         IF (snl > maxsnl) THEN
            wice_soisno(maxsnl+1:snl) = 0.
            wliq_soisno(maxsnl+1:snl) = 0.
            t_soisno   (maxsnl+1:snl) = 0.
            z_soisno   (maxsnl+1:snl) = 0.
            dz_soisno  (maxsnl+1:snl) = 0.
         ENDIF

         lb = snl + 1
         t_grnd = t_soisno(lb)

         IF (is_dry_lake) THEN
            dz_lake = wdsrf*1.e-3/nl_lake
            t_lake  = t_soisno(1)
            IF (t_soisno(1) >= tfrz) THEN
               lake_icefrac = 0.
            ELSE
               lake_icefrac = 1.
            ENDIF

            IF (wdsrf >= 100.) THEN
               CALL adjust_lake_layer (nl_lake, dz_lake, t_lake, lake_icefrac)
            ENDIF
         ENDIF

         ! ----------------------------------------
         ! energy balance
         ! ----------------------------------------
         zerr=errore
#if (defined CoLMDEBUG)
         IF (abs(errore) > .5) THEN
            write(6,*) 'Warning: energy balance violation ',errore,patchclass
         ENDIF
#endif

         ! ----------------------------------------
         ! water balance
         ! ----------------------------------------
         endwb=sum(wice_soisno(1:)+wliq_soisno(1:))+ldew+scv + wa
#ifdef CROP
         IF (DEF_USE_IRRIGATION) endwb = endwb + waterstorage(ipatch)
#endif

         endwb = endwb + wdsrf
         IF (DEF_USE_VariablySaturatedFlow) THEN
            IF (patchtype == 2) THEN
               endwb = endwb + wetwat
            ENDIF
         ENDIF
#ifndef CatchLateralFlow
         errorw=(endwb-totwb)-(forc_prc+forc_prl-fevpa-rnof)*deltim
#else
         ! for lateral flow, "rsur" is considered in HYDRO/MOD_Hydro_SurfaceFlow.F90
         errorw=(endwb-totwb)-(forc_prc+forc_prl-fevpa)*deltim
#endif

         IF (.not. DEF_USE_VariablySaturatedFlow) THEN
            IF (patchtype==2) errorw=0.    !wetland
         ENDIF

         xerr=errorw/deltim

#if (defined CoLMDEBUG)
         IF (abs(errorw) > 1.e-3) THEN
            IF     (patchtype == 0) THEN
               write(6,*) 'Warning: water balance violation in CoLMMAIN (soil) ', errorw
            ELSEIF (patchtype == 1) THEN
               write(6,*) 'Warning: water balance violation in CoLMMAIN (urban) ', errorw
            ELSEIF (patchtype == 2) THEN
               write(6,*) 'Warning: water balance violation in CoLMMAIN (wetland) ', errorw
            ELSEIF (patchtype == 4) THEN
               write(6,*) 'Warning: water balance violation in CoLMMAIN (dry lake) ', errorw
            ENDIF
            CALL CoLM_stop ()
         ENDIF
#endif

!======================================================================

      ELSEIF (patchtype == 3) THEN   ! <=== is LAND ICE (glacier/ice sheet) (patchtype = 3)

!======================================================================
                            ! initial set
         scvold = scv       ! snow mass at previous time step

         snl = 0
         DO j=maxsnl+1,0
            IF(wliq_soisno(j)+wice_soisno(j)>0.) snl=snl-1
         ENDDO

         zi_soisno(0)=0.
         IF (snl < 0) THEN
            DO j = -1, snl, -1
               zi_soisno(j)=zi_soisno(j+1)-dz_soisno(j+1)
            ENDDO
         ENDIF
         DO j = 1,nl_soil
            zi_soisno(j)=zi_soisno(j-1)+dz_soisno(j)
         ENDDO

         totwb = scv + sum(wice_soisno(1:)+wliq_soisno(1:))
         IF (DEF_USE_VariablySaturatedFlow) THEN
            totwb = wdsrf + totwb
         ENDIF

         fiold(:) = 0.0
         IF (snl <0 ) THEN
            fiold(snl+1:0)=wice_soisno(snl+1:0)/(wliq_soisno(snl+1:0)+wice_soisno(snl+1:0))
         ENDIF

         pg_rain = prc_rain + prl_rain
         pg_snow = prc_snow + prl_snow

         t_rain = t_precip
         IF (wliq_soisno(1) > dz_soisno(1)*denh2o) THEN
            wextra  = (wliq_soisno(1) - dz_soisno(1)*denh2o) / deltim
            t_rain  = (pg_rain*t_precip + wextra*t_soisno(1)) / (pg_rain + wextra)
            pg_rain = pg_rain + wextra
            wliq_soisno(1) = dz_soisno(1)*denh2o
            totwb = totwb - wextra*deltim
         ENDIF

         t_snow = t_precip
         IF (wice_soisno(1) > dz_soisno(1)*denice) THEN
            wextra  = (wice_soisno(1) - dz_soisno(1)*denice) / deltim
            t_snow  = (pg_snow*t_precip + wextra*t_soisno(1)) / (pg_snow + wextra)
            pg_snow = pg_snow + wextra
            wice_soisno(1) = dz_soisno(1)*denice
            totwb = totwb - wextra*deltim
         ENDIF

         IF (pg_rain+pg_snow > 0) THEN
            t_precip = (pg_rain*cpliq*t_rain + pg_snow*cpice*t_snow)/(pg_rain*cpliq+pg_snow*cpice)
         ENDIF

         !----------------------------------------------------------------
         ! Initialize new snow nodes for snowfall / sleet
         !----------------------------------------------------------------

         snl_bef = snl

         CALL newsnow (patchtype,maxsnl,deltim,t_grnd,pg_rain,pg_snow,bifall,&
                       t_precip,zi_soisno(:0),z_soisno(:0),dz_soisno(:0),t_soisno(:0),&
                       wliq_soisno(:0),wice_soisno(:0),fiold(:0),snl,sag,scv,snowdp,fsno)

         !----------------------------------------------------------------
         ! Energy and Water balance
         !----------------------------------------------------------------
         lb   = snl + 1            !lower bound of array
         lbsn = min(lb,0)

         CALL GLACIER_TEMP (patchtype,lb       ,nl_soil    ,deltim      ,&
                      zlnd        ,zsno        ,capr       ,cnfac       ,&
                      forc_hgt_u  ,forc_hgt_t  ,forc_hgt_q ,forc_us     ,&
                      forc_vs     ,forc_t      ,forc_q     ,forc_hpbl   ,&
                      forc_rhoair ,forc_psrf   ,coszen     ,sabg        ,&
                      forc_frl    ,fsno        ,dz_soisno(lb:)          ,&
                      z_soisno(lb:),zi_soisno(lb-1:)       ,&
                      t_soisno(lb:),wice_soisno(lb:),wliq_soisno(lb:)   ,&
                      scv         ,snowdp      ,imelt(lb:) ,taux        ,&
                      tauy        ,fsena       ,fevpa      ,lfevpa      ,&
                      fseng       ,fevpg       ,olrg       ,fgrnd       ,&
                      qseva       ,qsdew       ,qsubl      ,qfros       ,&
                      sm          ,tref        ,qref       ,trad        ,&
                      errore      ,emis        ,z0m        ,zol         ,&
                      rib         ,ustar       ,qstar      ,tstar       ,&
                      fm          ,fh          ,fq         ,fh2m        ,&
                      fq2m        ,fm10m       ,pg_rain     ,&
                      pg_snow     ,t_precip    ,&
                      snofrz(lbsn:0), sabg_snow_lyr(lb:1)                )


         IF (DEF_USE_SNICAR) THEN
            CALL GLACIER_WATER_snicar (nl_soil ,maxsnl     ,deltim      ,&
                      z_soisno    ,dz_soisno   ,zi_soisno  ,t_soisno    ,&
                      wliq_soisno ,wice_soisno ,pg_rain    ,pg_snow     ,&
                      sm          ,scv         ,snowdp     ,imelt       ,&
                      fiold       ,snl         ,qseva      ,qsdew       ,&
                      qsubl       ,qfros       ,gwat       ,ssi         ,&
                      wimp        ,forc_us     ,forc_vs    ,&
                      ! SNICAR
                      forc_aer    ,&
                      mss_bcpho   ,mss_bcphi   ,mss_ocpho  ,mss_ocphi   ,&
                      mss_dst1    ,mss_dst2    ,mss_dst3   ,mss_dst4     )
         ELSE
            CALL GLACIER_WATER (   nl_soil     ,maxsnl     ,deltim      ,&
                      z_soisno    ,dz_soisno   ,zi_soisno  ,t_soisno    ,&
                      wliq_soisno ,wice_soisno ,pg_rain    ,pg_snow     ,&
                      sm          ,scv         ,snowdp     ,imelt       ,&
                      fiold       ,snl         ,qseva      ,qsdew       ,&
                      qsubl       ,qfros       ,gwat       ,ssi         ,&
                      wimp        ,forc_us     ,forc_vs                  )
         ENDIF

         IF (.not. DEF_USE_VariablySaturatedFlow) THEN
            rsur = max(0.0,gwat)
            rsub = 0.
            rnof = rsur
         ELSE
            a = wdsrf + wliq_soisno(1) + gwat * deltim
            IF (a > dz_soisno(1)*denh2o) THEN
               wliq_soisno(1) = dz_soisno(1)*denh2o
               wdsrf = a - wliq_soisno(1)
            ELSE
               wdsrf = 0.
               wliq_soisno(1) = max(a, 1.e-8)
            ENDIF
#ifndef CatchLateralFlow
            IF (wdsrf > pondmx) THEN
               rsur  = (wdsrf - pondmx) / deltim
               wdsrf = pondmx
            ELSE
               rsur = 0.
            ENDIF
            rsub = 0.
            rnof = rsur
            rsur_se = rsur
            rsur_ie = 0.
#endif
         ENDIF

         lb = snl + 1
         t_grnd = t_soisno(lb)

         ! ----------------------------------------
         ! energy and water balance check
         ! ----------------------------------------
         zerr=errore

         endwb = scv + sum(wice_soisno(1:)+wliq_soisno(1:))
         IF (DEF_USE_VariablySaturatedFlow) THEN
            endwb = wdsrf + endwb
         ENDIF

#ifndef CatchLateralFlow
         errorw=(endwb-totwb)-(pg_rain+pg_snow-fevpa-rnof)*deltim
#else
         errorw=(endwb-totwb)-(pg_rain+pg_snow-fevpa)*deltim
#endif

#if (defined CoLMDEBUG)
         IF (DEF_USE_VariablySaturatedFlow) THEN
            IF (abs(errorw) > 1.e-3) THEN
               write(6,*) 'Warning: water balance violation in CoLMMAIN (land ice) ', errorw
               CALL CoLM_stop ()
            ENDIF
         ENDIF
#endif

         IF (DEF_USE_VariablySaturatedFlow) THEN
            xerr=errorw/deltim
         ELSE
            xerr = 0.
         ENDIF

!======================================================================

      ELSEIF (patchtype == 4) THEN   ! <=== is LAND WATER BODIES
                                     ! (lake, reservoir and river) (patchtype = 4)

!======================================================================

         totwb = scv + sum(wice_soisno(1:)+wliq_soisno(1:)) + wa
         IF (DEF_USE_Dynamic_Lake) THEN
            totwb = totwb + wdsrf
         ENDIF

         snl = 0
         DO j = maxsnl+1, 0
            IF (wliq_soisno(j)+wice_soisno(j) > 0.) THEN
               snl=snl-1
            ENDIF
         ENDDO

         zi_soisno(0) = 0.
         IF (snl < 0) THEN
            DO j = -1, snl, -1
               zi_soisno(j)=zi_soisno(j+1)-dz_soisno(j+1)
            ENDDO
         ENDIF

         DO j = 1,nl_soil
            zi_soisno(j)=zi_soisno(j-1)+dz_soisno(j)
         ENDDO

         scvold = scv          !snow mass at previous time step
         fiold(:) = 0.0
         IF (snl < 0) THEN
            fiold(snl+1:0)=wice_soisno(snl+1:0)/(wliq_soisno(snl+1:0)+wice_soisno(snl+1:0))
         ENDIF

         w_old = sum(wliq_soisno(1:)) + sum(wice_soisno(1:))

         pg_rain = prc_rain + prl_rain
         pg_snow = prc_snow + prl_snow

#ifndef EXTERNAL_LAKE
         CALL newsnow_lake ( DEF_USE_Dynamic_Lake, &
              ! "in" arguments
              ! ---------------
              maxsnl       ,nl_lake      ,deltim          ,dz_lake         ,&
              pg_rain      ,pg_snow      ,t_precip        ,bifall          ,&

              ! "inout" arguments
              ! ------------------
              t_lake       ,zi_soisno(:0),z_soisno(:0)    ,&
              dz_soisno(:0),t_soisno(:0) ,wliq_soisno(:0) ,wice_soisno(:0) ,&
              fiold(:0)    ,snl          ,sag             ,scv             ,&
              snowdp       ,lake_icefrac )

         CALL laketem ( &
              ! "in" laketem arguments
              ! ---------------------------
              patchtype    ,maxsnl       ,nl_soil         ,nl_lake         ,&
              patchlatr    ,deltim       ,forc_hgt_u      ,forc_hgt_t      ,&
              forc_hgt_q   ,forc_us      ,forc_vs         ,forc_t          ,&
              forc_q       ,forc_rhoair  ,forc_psrf       ,forc_sols       ,&
              forc_soll    ,forc_solsd   ,forc_solld      ,sabg            ,&
              forc_frl     ,dz_soisno    ,z_soisno        ,zi_soisno       ,&
              dz_lake      ,lakedepth    ,vf_quartz       ,vf_gravels      ,&
              vf_om        ,vf_sand      ,wf_gravels      ,wf_sand         ,&
              porsl        ,csol         ,k_solids        ,&
              dksatu       ,dksatf       ,dkdry           ,&
              BA_alpha     ,BA_beta      ,forc_hpbl       ,&

              ! "inout" laketem arguments
              ! ---------------------------
              t_grnd       ,scv          ,snowdp          ,t_soisno        ,&
              wliq_soisno  ,wice_soisno  ,imelt           ,t_lake          ,&
              lake_icefrac ,savedtke1    ,&

! SNICAR model variables
              snofrz       ,sabg_snow_lyr,&
! END SNICAR model variables

              ! "out" laketem arguments
              ! ---------------------------
              taux         ,tauy         ,fsena           ,&
              fevpa        ,lfevpa       ,fseng           ,fevpg           ,&
              qseva        ,qsubl        ,qsdew           ,qfros           ,&
              olrg         ,fgrnd        ,tref            ,qref            ,&
              trad         ,emis         ,z0m             ,zol             ,&
              rib          ,ustar        ,qstar           ,tstar           ,&
              fm           ,fh           ,fq              ,fh2m             ,&
              fq2m         ,fm10m        ,sm               )

         CALL snowwater_lake ( DEF_USE_Dynamic_Lake, &
              ! "in" snowater_lake arguments
              ! ---------------------------
              maxsnl       ,nl_soil      ,nl_lake         ,deltim          ,&
              ssi          ,wimp         ,porsl           ,pg_rain         ,&
              pg_snow      ,dz_lake      ,imelt(:0)       ,fiold(:0)       ,&
              qseva        ,qsubl        ,qsdew           ,qfros           ,&

              ! "inout" snowater_lake arguments
              ! ---------------------------
              z_soisno     ,dz_soisno    ,zi_soisno       ,t_soisno        ,&
              wice_soisno  ,wliq_soisno  ,t_lake          ,lake_icefrac    ,&
              gwat         ,&
              fseng        ,fgrnd        ,snl             ,scv             ,&
              snowdp       ,sm           ,forc_us         ,forc_vs         ,&

              ! SNICAR model variables
              forc_aer     ,&
              mss_bcpho    ,mss_bcphi    ,mss_ocpho       ,mss_ocphi       ,&
              mss_dst1     ,mss_dst2     ,mss_dst3        ,mss_dst4         )

#else
         CALL external_lake( &
               ! "in" arguments
               ! -------------------
               deltim      ,patchlatr     ,patchlonr      ,bifall        ,&
               forc_hgt_u  ,forc_hgt_t    ,forc_hgt_q     ,forc_us       ,&
               forc_vs     ,forc_t        ,forc_q         ,forc_rhoair   ,&
               forc_psrf   ,forc_frl      ,sabg           ,forc_hpbl     ,&
               forc_sols   ,forc_soll     ,forc_solsd     ,forc_solld    ,&
               prc_rain    ,prl_rain      ,prc_snow       ,prl_snow      ,&
               t_precip    ,ipatch        ,&
               ! "inout" arguments
               ! -------------------
               t_grnd      ,t_lake        ,t_soisno       ,snl           ,&
               z_soisno    ,zi_soisno     ,dz_soisno      ,scv           ,&
               savedtke1   ,sag           ,snowdp         ,lake_icefrac  ,&
               wliq_soisno ,wice_soisno   ,gwat           ,&
! SNICAR model variables
               forc_aer    ,sabg_snow_lyr ,snofrz         ,&
               mss_bcpho   ,mss_bcphi     ,mss_ocpho      ,mss_ocphi     ,&
               mss_dst1    ,mss_dst2      ,mss_dst3       ,mss_dst4      ,&
! END SNICAR model variables
               ! "out" arguments
               ! -------------------
               fsena       ,fevpa         ,lfevpa         ,fseng         ,&
               fevpg       ,olrg          ,fgrnd          ,trad          ,&
               qseva       ,qsubl         ,qsdew          ,qfros         ,&
               taux        ,tauy          ,ustar          ,qstar         ,&
               tstar       ,emis          ,sm             ,zol           ,&
               tref        ,qref          ,fm             ,fq            ,&
               rib         ,fh            ,z0m            )
#endif

         IF (.not. DEF_USE_Dynamic_Lake) THEN
            ! We assume the land water bodies have zero extra liquid water capacity
            ! (i.e.,constant capacity), all excess liquid water are put into the runoff,
            ! this unreasonable assumption should be updated in the future version
            a = (sum(wliq_soisno(1:))+sum(wice_soisno(1:))+scv-w_old-scvold)/deltim
            aa = qseva+qsubl-qsdew-qfros
            rsur = max(0., pg_rain + pg_snow - aa - a)
            rsub = 0.
            rnof = rsur
            rsur_se = rsur
            rsur_ie = 0.
            lake_deficit = - min(0., pg_rain + pg_snow - aa - a)
         ELSE

            wdsrf = sum(dz_lake) * 1.e3

#ifndef CatchLateralFlow
            IF (wdsrf > lakedepth*1.e3) THEN
               rsur  = (wdsrf - lakedepth*1.e3) / deltim
               wdsrf = lakedepth*1.e3
               dz_lake = dz_lake * lakedepth/sum(dz_lake)
               CALL adjust_lake_layer (nl_lake, dz_lake, t_lake, lake_icefrac)
            ELSE
               rsur = 0.
            ENDIF
            rsub = 0.
            rnof = rsur
            rsur_se = rsur
            rsur_ie = 0.
#endif
         ENDIF

         endwb  = scv + sum(wice_soisno(1:)+wliq_soisno(1:)) + wa
         IF (DEF_USE_Dynamic_Lake) THEN
            endwb  = endwb  + wdsrf
         ELSE
            endwb  = endwb  - lake_deficit * deltim
         ENDIF

         errorw = (endwb-totwb) - (forc_prc+forc_prl-fevpa) * deltim
#ifndef CatchLateralFlow
         errorw = errorw + rnof * deltim
#endif

#if (defined CoLMDEBUG)
         IF (abs(errorw) > 1.e-3) THEN
            write(*,*) 'Warning: water balance violation in CoLMMAIN (lake) ', errorw
            CALL CoLM_stop ()
         ENDIF
#endif

         IF (DEF_USE_Dynamic_Lake) THEN
            xerr = errorw / deltim
         ELSE
            xerr = 0.
         ENDIF

         ! Set zero to the empty node
         IF (snl > maxsnl) THEN
            wice_soisno(maxsnl+1:snl) = 0.
            wliq_soisno(maxsnl+1:snl) = 0.
            t_soisno   (maxsnl+1:snl) = 0.
            z_soisno   (maxsnl+1:snl) = 0.
            dz_soisno  (maxsnl+1:snl) = 0.
         ENDIF

!======================================================================

      ELSE                     ! <=== is OCEAN (patchtype >= 99)

!======================================================================
! simple ocean-sea ice model

         tssea = t_grnd
         tssub (1:7) = t_soisno (1:7)
         CALL SOCEAN (dosst,deltim,oro,forc_hgt_u,forc_hgt_t,forc_hgt_q,&
                    forc_us,forc_vs,forc_t,forc_q,forc_rhoair,forc_psrf,&
                    sabg,forc_frl,tssea,tssub(1:7),scv,&
                    taux,tauy,fsena,fevpa,lfevpa,fseng,fevpg,tref,qref,&
                    z0m,zol,rib,ustar,qstar,tstar,fm,fh,fq,fh2m,fq2m,fm10m,emis,olrg)

                  ! null data for sea component
                    z_soisno   (:) = 0.0
                    dz_soisno  (maxsnl+1:0) = 0.
                    t_soisno   (:) = 0.0
                    t_soisno (1:7) = tssub(1:7)
                    wliq_soisno(:) = 0.0
                    wice_soisno(:) = 0.0
                    t_grnd  = tssea
                    snowdp  = scv/1000.*20.

                    trad    = tssea
                    fgrnd   = 0.0
                    rsur    = 0.0
                    rsur_se = 0.0
                    rsur_ie = 0.0
                    rsub    = 0.0
                    rnof    = 0.0
                    xerr    = 0.0

!======================================================================
      ENDIF

      ! THERMAL performs PFT-level aggregation for these nonlinear
      ! diagnostics. Single-surface glacier, lake, and ocean branches can
      ! derive them directly from their native similarity profiles.
      IF ((patchtype > 2) .and. (.not. is_dry_lake)) THEN
         u10m = forc_us * fm10m / fm
         v10m = forc_vs * fm10m / fm
         chs2 = vonkar * ustar / fh2m
         cqs2 = vonkar * ustar / fq2m
         cd10 = (vonkar / fm10m)**2
      ENDIF

!======================================================================
! Preparation for the next time step
! 1) time-varying parameters for vegetation
! 2) fraction of snow cover
! 3) solar zenith angle and
! 4) albedos
!======================================================================

      ! cosine of solar zenith angle
      calday = calendarday(idate)
      coszen = orb_coszen(calday,patchlonr,patchlatr)

      IF (patchtype <= 5) THEN   !LAND
#if (defined DYN_PHENOLOGY)
         ! need to update lai and sai, fveg, green, they are done once in a day only
         IF (dolai) THEN
            CALL LAI_empirical(patchclass,nl_soil,rootfr,t_soisno(1:),lai,sai,fveg,green)
         ENDIF
#endif

! only for soil patches
!NOTE: lai from remote sensing has already considered snow coverage

!NOTE: IF account for snow on vegetation:
!        1) should use snow-free LAI data and 2) update LAI and SAI according to snowdp

         IF (patchtype == 0) THEN

#if (defined LULC_USGS || defined LULC_IGBP)
            CALL snowfraction (tlai(ipatch),tsai(ipatch),z0m,zlnd,scv,snowdp,wt,sigf,fsno)
            lai = tlai(ipatch)
            sai = tsai(ipatch) * sigf

            !NOTE: use snow-free LAI by defining namelist DEF_VEG_SNOW
            IF ( DEF_VEG_SNOW ) THEN
               lai = tlai(ipatch) * sigf
            ENDIF
#endif

#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
            ps = patch_pft_s(ipatch)
            pe = patch_pft_e(ipatch)
            CALL snowfraction_pftwrap (ipatch,zlnd,scv,snowdp,wt,sigf,fsno)
            IF(DEF_USE_LAIFEEDBACK)THEN
               lai = sum(lai_p(ps:pe)*pftfrac(ps:pe))
            ELSE
               lai_p(ps:pe) = tlai_p(ps:pe)
               lai = tlai(ipatch)

               !NOTE: use snow-free LAI by defining namelist DEF_VEG_SNOW
               IF ( DEF_VEG_SNOW ) THEN
                  lai_p(ps:pe) = tlai_p(ps:pe)*sigf_p(ps:pe)
                  lai = sum(lai_p(ps:pe)*pftfrac(ps:pe))
               ENDIF
            ENDIF
            sai_p(ps:pe) = tsai_p(ps:pe) * sigf_p(ps:pe)
            sai = sum(sai_p(ps:pe)*pftfrac(ps:pe))
#endif

         ELSE
            CALL snowfraction (tlai(ipatch),tsai(ipatch),z0m,zlnd,scv,snowdp,wt,sigf,fsno)
            lai = tlai(ipatch)
            sai = tsai(ipatch) * sigf

            !NOTE: use snow-free LAI by defining namelist DEF_VEG_SNOW
            IF ( DEF_VEG_SNOW ) THEN
               lai = tlai(ipatch) * sigf
            ENDIF
         ENDIF

         ! water volumetric content of soil surface layer [m3/m3]
         ssw = min(1.,1.e-3*wliq_soisno(1)/dz_soisno(1))
         IF (patchtype >= 3) ssw = 1.0

! ============================================================================
! Snow aging routine based on Flanner and Zender (2006), Linking snowpack
! microphysics and albedo evolution, JGR, and Brun (1989), Investigation of
! wet-snow metamorphism in respect of liquid-water content, Ann. Glacial.

         dz_soisno_(:1) = dz_soisno(:1)
         t_soisno_ (:1) = t_soisno (:1)

         IF ((patchtype == 4) .and. (.not. is_dry_lake)) THEN
            dz_soisno_(1) = dz_lake(1)
            t_soisno_ (1) = t_lake (1)
         ENDIF

! ============================================================================
         ! albedos
         ! we supposed CALL it every time-step, because
         ! other vegetation related parameters are needed to create
         IF (doalb) THEN
#ifdef HYPERSPECTRAL
            CALL albland_HiRes (ipatch, patchtype,deltim,&
                 soil_s_v_alb,soil_d_v_alb,soil_s_n_alb,soil_d_n_alb,&
                 chil,rho,tau,fveg,green,lai,sai,fwet_snow,coszen,&
                 wt,fsno,scv,scvold,sag,ssw,pg_snow,forc_t,t_grnd,t_soisno_,dz_soisno_,&
                 snl,wliq_soisno,wice_soisno,snw_rds,snofrz,&
                 mss_bcpho,mss_bcphi,mss_ocpho,mss_ocphi,&
                 mss_dst1,mss_dst2,mss_dst3,mss_dst4,&
                 alb,ssun,ssha,ssoi,ssno,ssno_lyr,thermk,extkb,extkd,&

                 ! new parameters for hyperspectral scheme
                 alb_hires                         ,&
                 dir_frac    , dif_frac            ,&
                 reflectance , transmittance       ,&
                 soil_alb, kw, nw, porsl(1)        ,&
                 reflectance_out, transmittance_out,&
                 idate(2), patchlatr, patchlonr    ,&
                 urban_albedo, mean_albedo, lat_north, lat_south, lon_west, lon_east )

#else
            CALL albland (ipatch,patchtype,deltim,&
                 soil_s_v_alb,soil_d_v_alb,soil_s_n_alb,soil_d_n_alb,&
                 chil,rho,tau,fveg,green,lai,sai,fwet_snow,coszen,&
                 wt,fsno,scv,scvold,sag,ssw,pg_snow,forc_t,t_grnd,t_soisno_,dz_soisno_,&
                 snl,wliq_soisno,wice_soisno,snw_rds,snofrz,&
                 mss_bcpho,mss_bcphi,mss_ocpho,mss_ocphi,&
                 mss_dst1,mss_dst2,mss_dst3,mss_dst4,&
                 alb,ssun,ssha,ssoi,ssno,ssno_lyr,thermk,extkb,extkd)
#endif
         ENDIF

      ELSE                   !OCEAN
         sag = 0.0
         IF(doalb)THEN
            CALL albocean (oro,scv,coszen,alb)
         ENDIF
      ENDIF

      ! zero-filling set for glacier/ice-sheet/land water bodies/ocean components
      IF ((patchtype > 2) .and. (.not. is_dry_lake)) THEN
         lai           = 0.0
         sai           = 0.0
         laisun        = 0.0
         laisha        = 0.0
         green         = 0.0
         fveg          = 0.0
         sigf          = 0.0

         ssun(:,:)     = 0.0
         ssha(:,:)     = 0.0
         thermk        = 0.0
         extkb         = 0.0
         extkd         = 0.0

         tleaf         = forc_t
         ldew_rain     = 0.0
         ldew_snow     = 0.0
         fwet_snow     = 0.0
         ldew          = 0.0
         fsenl         = 0.0
         fevpl         = 0.0
         etr           = 0.0
         assim         = 0.0
         respc         = 0.0

         zerr          = 0.

         qinfl         = 0.
         qlayer        = 0.
         qdrip         = forc_rain + forc_snow
         qintr         = 0.
         frcsat        = 1.
         h2osoi        = 0.
         rstfacsun_out = 0.
         rstfacsha_out = 0.
         gssun_out     = 0.
         gssha_out     = 0.
         assimsun_out  = 0.
         etrsun_out    = 0.
         assimsha_out  = 0.
         etrsha_out    = 0.
         rootr         = 0.
         rootflux      = 0.
         zwt           = 0.

         IF (.not. DEF_USE_VariablySaturatedFlow) THEN
            wa = 4800.
         ENDIF

         qcharge = 0.
         IF (DEF_USE_PLANTHYDRAULICS)THEN
            vegwp = -2.5e4
         ENDIF
      ENDIF

      h2osoi = wliq_soisno(1:)/(dz_soisno(1:)*denh2o) + wice_soisno(1:)/(dz_soisno(1:)*denice)

      IF (DEF_USE_VariablySaturatedFlow) THEN
         wat = sum(wice_soisno(1:)+wliq_soisno(1:))+ldew+scv+wetwat
      ELSE
         wat = sum(wice_soisno(1:)+wliq_soisno(1:))+ldew+scv + wa
      ENDIF

      z_sno (maxsnl+1:0) = z_soisno (maxsnl+1:0)
      dz_sno(maxsnl+1:0) = dz_soisno(maxsnl+1:0)

END SUBROUTINE CoLMMAIN
! ---------- EOP ------------
