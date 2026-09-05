#include <define.h>

!-----------------------------------------------------------------------
!
!            --- CoLM 3D Building Community Urban Model ---
!
!                Sun
!                 \\\
!                  \\\
!                         ______
!                        |++++++|              roof
!                        |++++++|_ AC         ______
!                        |++++++|_|  ___     |++++++|
!                    ______+++++|   |||||    |++++++|
!                   |++++++|++++|  |||||||   |++++++|
!            sunlit |[]++[]|++++|   |||||    |++++++| shaded
!             wall  |++++++|          | tree |++++++|  wall
!                   |[]++[]|          |      |++++++|
!                   |++++++|  impervious/pervious ground
!         __________|++++++|____________________________________
!
! !DESCRIPTION:
!
!  Unlike the traditional urban canyon models, the CoLM urban model is
!  based on the assumption of a three-dimensional urban building
!  community, including trees and water bodies. We have developed a new
!  approach for shortwave and longwave radiation transfer, as well as
!  turbulent exchange within the three-dimensional urban buildings. In
!  the process of calculating radiation transfer and turbulent exchange,
!  we have integrated simulations of vegetation and water bodies.
!
!  The CoLM urban model utilizes comprehensive high-resolution data on
!  urban cover, geometric structure, vegetation, water bodies, etc.
!  Furthermore, it has developed a relatively complete simulation of
!  anthropogenic heat processes, including building energy consumption,
!  traffic heat, and metabolic heat.
!
!  Created by Hua Yuan, 09/2021
!
!
! !REVISIONS (major):
!
!  03/2022, Hua Yuan: complete the model with full coupling, and make
!           it possible to run multiple scenario assumptions through
!           macro definitions.
!
!  07/2022, Wenzong Dong: add LUCY model initial version.
!
!  05/2023, Hua Yuan: Initial urban physical codes in MPI version. Add
!           some interface or modifications for Urban model coupling.
!
!  05/2023, Wenzong Dong, Hua Yuan, Shupeng Zhang: porting urban making
!           surface data codes to MPI parallel version.
!
!  05/2023, Hua Yuan: Rename files and modules align with current
!           version.
!
!-----------------------------------------------------------------------

   SUBROUTINE CoLMMAIN_Urban ( &

         ! model running information
           ipatch       ,idate        ,coszen       ,deltim       ,&
           patchlonr    ,patchlatr    ,patchclass   ,patchtype    ,&

         ! urban and lake depth
           froof        ,flake        ,hroof        ,hlr          ,&
           fgper        ,em_roof      ,em_wall      ,em_gimp      ,&
           em_gper      ,cv_roof      ,cv_wall      ,cv_gimp      ,&
           tk_roof      ,tk_wall      ,tk_gimp      ,z_roof       ,&
           z_wall       ,dz_roof      ,dz_wall      ,lakedepth    ,&
           dz_lake      ,elvstd       ,BVIC                       ,&

         ! LUCY model input parameters
           fix_holiday  ,week_holiday ,hum_prof     ,pop_den      ,&
           vehicle      ,weh_prof     ,wdh_prof                   ,&

         ! soil ground and wall information
           vf_quartz    ,vf_gravels   ,vf_om        ,vf_sand      ,&
           wf_gravels   ,wf_sand      ,porsl        ,psi0         ,&
           bsw          ,theta_r      ,fsatmax      ,fsatdcf      ,&
#ifdef vanGenuchten_Mualem_SOIL_MODEL
           alpha_vgm    ,n_vgm        ,L_vgm                      ,&
           sc_vgm       ,fc_vgm                                   ,&
#endif
           hksati       ,csol         ,k_solids     ,dksatu       ,&
           dksatf       ,dkdry        ,BA_alpha     ,BA_beta      ,&
           alb_roof     ,alb_wall     ,alb_gimp     ,alb_gper     ,&

         ! vegetation information
           htop         ,hbot         ,sqrtdi       ,chil         ,&
           effcon       ,vmax25       ,c3c4         ,slti         ,hlti,&
           shti         ,hhti         ,trda         ,trdm         ,&
           trop         ,g1           ,g0           ,gradm        ,&
           binter       ,extkn        ,rho          ,tau          ,&
           rootfr       ,lambda                                   ,&

         ! atmospheric forcing
           forc_pco2m   ,forc_po2m    ,forc_us      ,forc_vs      ,&
           forc_t       ,forc_q       ,forc_prc     ,forc_prl     ,&
           forc_rain    ,forc_snow    ,forc_psrf    ,forc_pbot    ,&
           forc_sols    ,forc_soll    ,forc_solsd   ,forc_solld   ,&
           forc_frl     ,forc_hgt_u   ,forc_hgt_t   ,forc_hgt_q   ,&
           forc_rhoair  ,Fhac         ,Fwst         ,Fach         ,&
           Fahe         ,Fhah         ,vehc         ,meta         ,&

         ! land surface variables required for restart
           z_sno_roof   ,z_sno_gimp   ,z_sno_gper   ,z_sno_lake   ,&
           dz_sno_roof  ,dz_sno_gimp  ,dz_sno_gper  ,dz_sno_lake  ,&
           t_roofsno    ,t_gimpsno    ,t_gpersno    ,t_lakesno    ,&
           wliq_roofsno ,wliq_gimpsno ,wliq_gpersno ,wliq_lakesno ,&
           wice_roofsno ,wice_gimpsno ,wice_gpersno ,wice_lakesno ,&
           z_sno        ,dz_sno       ,wliq_soisno  ,wice_soisno  ,&
           t_soisno     ,smp          ,hk           ,t_wallsun    ,&
           t_wallsha                                              ,&

           lai          ,sai          ,fveg         ,sigf         ,&
           green        ,tleaf        ,ldew         ,ldew_rain    ,&
           ldew_snow    ,fwet_snow    ,t_grnd                     ,&

           sag_roof     ,sag_gimp     ,sag_gper     ,sag_lake     ,&
           scv_roof     ,scv_gimp     ,scv_gper     ,scv_lake     ,&
           snowdp_roof  ,snowdp_gimp  ,snowdp_gper  ,snowdp_lake  ,&
           fsno_roof    ,fsno_gimp    ,fsno_gper    ,fsno_lake    ,&
           sag          ,scv          ,snowdp       ,fsno         ,&
           extkd        ,alb          ,ssun         ,ssha         ,&
           sroof        ,swsun        ,swsha        ,sgimp        ,&
           sgper        ,slake        ,lwsun        ,lwsha        ,&
           lgimp        ,lgper        ,lveg         ,fwsun        ,&
           dfwsun       ,t_room       ,troof_inner  ,twsun_inner  ,&
           twsha_inner  ,t_roommax    ,t_roommin    ,tafu         ,&

           zwt          ,wdsrf         ,wa                        ,&
           t_lake       ,lake_icefrac ,savedtke1                  ,&

         ! SNICAR snow model related
           snw_rds      ,ssno                                     ,&
           mss_bcpho    ,mss_bcphi    ,mss_ocpho    ,mss_ocphi    ,&
           mss_dst1     ,mss_dst2     ,mss_dst3     ,mss_dst4     ,&

         ! additional diagnostic variables for output
           laisun       ,laisha       ,rss                        ,&
           rstfac       ,h2osoi       ,wat                        ,&

         ! FLUXES
           taux         ,tauy         ,fsena        ,fevpa        ,&
           lfevpa       ,fsenl        ,fevpl        ,etr          ,&
           fseng        ,fevpg        ,olrg         ,fgrnd        ,&
           fsen_roof    ,fsen_wsun    ,fsen_wsha    ,fsen_gimp    ,&
           fsen_gper    ,fsen_urbl    ,troof        ,twall        ,&
           lfevp_roof   ,lfevp_gimp   ,lfevp_gper   ,lfevp_urbl   ,&
           trad         ,tref         ,&!tmax       ,tmin         ,&
           qref         ,rsur         ,rnof         ,qintr        ,&
           qinfl        ,qdrip        ,rst          ,assim        ,&
           respc        ,sabvsun      ,sabvsha      ,sabg         ,&
           sr           ,solvd        ,solvi        ,solnd        ,&
           solni        ,srvd         ,srvi         ,srnd         ,&
           srni         ,solvdln      ,solviln      ,solndln      ,&
           solniln      ,srvdln       ,srviln       ,srndln       ,&
           srniln       ,qcharge      ,xerr         ,zerr         ,&

         ! TUNABLE model constants
           zlnd         ,zsno         ,csoilc       ,dewmx        ,&
           capr         ,cnfac        ,ssi          ,wimp         ,&
           pondmx       ,smpmax       ,smpmin       ,trsmx0       ,&
           tcrit                                                  ,&

         ! additional variables required by coupling with WRF model
           emis         ,z0m          ,zol          ,rib          ,&
           ustar        ,qstar        ,tstar        ,fm           ,&
           fh           ,fq           ,hpbl                        )

   USE MOD_Precision
   USE MOD_Vars_Global
   USE MOD_Const_Physical, only: tfrz, denh2o, denice
   USE MOD_Vars_TimeVariables, only: tlai, tsai
   USE MOD_SnowLayersCombineDivide
   USE MOD_LeafInterception
   USE MOD_Urban_Albedo
   USE MOD_Urban_NetSolar
   USE MOD_Urban_Thermal
   USE MOD_Urban_Hydrology
   USE MOD_Lake
   USE MOD_TimeManager
   USE MOD_RainSnowTemp, only: rain_snow_temp
   USE MOD_NewSnow, only: newsnow
   USE MOD_OrbCoszen, only: orb_coszen
   USE MOD_SnowFraction, only: snowfraction
   USE MOD_ALBEDO, only: snowage
   USE MOD_Qsadv, only: qsadv
#ifdef USE_LUCY
   USE MOD_Urban_LUCY
#endif

   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   integer, intent(in) :: &
        ipatch                ,&! maximum number of snow layers
        idate(3)              ,&! next time-step /year/julian day/second in a day/
        patchclass            ,&! land cover type of USGS classification or others
        patchtype               ! land patch type (0=soil, 1=urban and built-up,
                                ! 2=wetland, 3=land ice, 4=land water bodies, 99 = ocean)

   real(r8),intent(in) :: &
        deltim                ,&! seconds in a time step [second]
        patchlonr             ,&! longitude in radians
        patchlatr               ! latitude in radians

   real(r8),intent(inout) :: &
        coszen                  ! cosine of solar zenith angle

   ! Parameters
   ! ----------------------
   real(r8), intent(in) :: &
        fix_holiday(365)      ,&! Fixed public holidays, holiday(0) or workday(1)
        week_holiday(7)       ,&! week holidays
        hum_prof(24)          ,&! Diurnal metabolic heat profile
        weh_prof(24)          ,&! Diurnal traffic flow profile of weekend
        wdh_prof(24)          ,&! Diurnal traffic flow profile of weekday
        pop_den               ,&! population density
        vehicle(3)              ! vehicle numbers per thousand people

   real(r8), intent(in) :: &
        froof                 ,&! roof fractional cover [-]
        fgper                 ,&! impervious fraction to ground area [-]
        flake                 ,&! lake fraction to ground area [-]
        hroof                 ,&! average building height [m]
        hlr                   ,&! average building height to their side length [-]
        em_roof               ,&! emissivity of roof [-]
        em_wall               ,&! emissivity of walls [-]
        em_gimp               ,&! emissivity of impervious [-]
        em_gper                 ! emissivity of pervious [-]

   real(r8), intent(in) :: &
        cv_roof   (1:nl_roof) ,&! heat capacity of roof [J/(m2 K)]
        cv_wall   (1:nl_wall) ,&! heat capacity of wall [J/(m2 K)]
        cv_gimp   (1:nl_soil) ,&! heat capacity of impervious [J/(m2 K)]
        tk_roof   (1:nl_roof) ,&! thermal conductivity of roof [W/m-K]
        tk_wall   (1:nl_wall) ,&! thermal conductivity of wall [W/m-K]
        tk_gimp   (1:nl_soil)   ! thermal conductivity of impervious [W/m-K]

   real(r8), intent(in) :: &
        ! soil physical parameters and lake info
        vf_quartz   (nl_soil) ,&! volumetric fraction of quartz within mineral soil
        vf_gravels  (nl_soil) ,&! volumetric fraction of gravels
        vf_om       (nl_soil) ,&! volumetric fraction of organic matter
        vf_sand     (nl_soil) ,&! volumetric fraction of sand
        wf_gravels  (nl_soil) ,&! gravimetric fraction of gravels
        wf_sand     (nl_soil) ,&! gravimetric fraction of sand
        porsl       (nl_soil) ,&! fraction of soil that is voids [-]
        psi0        (nl_soil) ,&! minimum soil suction [mm]
        bsw         (nl_soil) ,&! clapp and hornberger "b" parameter [-]
        theta_r     (nl_soil) ,&! residual water content (cm3/cm3)
        fsatmax               ,&! maximum saturated area fraction [-]
        fsatdcf               ,&! decay factor in calculation of saturated area fraction [1/m]

#ifdef vanGenuchten_Mualem_SOIL_MODEL
        alpha_vgm (1:nl_soil) ,&! parameter correspond approximately to inverse of air-entry value
        n_vgm     (1:nl_soil) ,&! a shape parameter
        L_vgm     (1:nl_soil) ,&! pore-connectivity parameter
        sc_vgm    (1:nl_soil) ,&! saturation at air entry value in classical vanGenuchten model [-]
        fc_vgm    (1:nl_soil) ,&! a scaling factor by using air entry value in the Mualem model [-]
#endif
        hksati      (nl_soil) ,&! hydraulic conductivity at saturation [mm h2o/s]
        csol        (nl_soil) ,&! heat capacity of soil solids [J/(m3 K)]
        k_solids    (nl_soil) ,&! thermal conductivity of minerals soil [W/m-K]
        dksatu      (nl_soil) ,&! thermal conductivity of saturated unfrozen soil [W/m-K]
        dksatf      (nl_soil) ,&! thermal conductivity of saturated frozen soil [W/m-K]
        dkdry       (nl_soil) ,&! thermal conductivity for dry soil  [J/(K s m)]

        BA_alpha    (nl_soil) ,&! alpha in Balland and Arp(2005) thermal conductivity scheme
        BA_beta     (nl_soil) ,&! beta in Balland and Arp(2005) thermal conductivity scheme
        alb_roof(2,2)         ,&! albedo of roof [-]
        alb_wall(2,2)         ,&! albedo of walls [-]
        alb_gimp(2,2)         ,&! albedo of impervious [-]
        alb_gper(2,2)         ,&! albedo of pervious [-]

        ! vegetation static, dynamic, derived parameters
        sqrtdi                ,&! inverse sqrt of leaf dimension [m**-0.5]
        chil                  ,&! leaf angle distribution factor
        effcon                ,&! quantum efficiency of RuBP regeneration (mol CO2/mol quanta)
        vmax25                ,&! maximum carboxylation rate at 25 C at canopy top
        slti                  ,&! slope of low temperature inhibition function      [s3]
        hlti                  ,&! 1/2 point of low temperature inhibition function  [s4]
        shti                  ,&! slope of high temperature inhibition function     [s1]
        hhti                  ,&! 1/2 point of high temperature inhibition function [s2]
        trda                  ,&! temperature coefficient in gs-a model             [s5]
        trdm                  ,&! temperature coefficient in gs-a model             [s6]
        trop                  ,&! temperature coefficient in gs-a model
        g1                    ,&! conductance-photosynthesis slope parameter for medlyn model
        g0                    ,&! conductance-photosynthesis intercept for medlyn model
        gradm                 ,&! conductance-photosynthesis slope parameter
        binter                ,&! conductance-photosynthesis intercep
        extkn                 ,&! coefficient of leaf nitrogen allocation
        rho(2,2)              ,&! leaf reflectance (iw=iband, il=life and dead)
        tau(2,2)              ,&! leaf transmittance (iw=iband, il=life and dead)

        rootfr      (nl_soil) ,&! fraction of roots in each soil layer
        lambda                ,&! marginal water cost of carbon gain

        ! tunable parameters
        zlnd                  ,&! roughness length for soil [m]
        zsno                  ,&! roughness length for snow [m]
        csoilc                ,&! drag coefficient for soil under canopy [-]
        dewmx                 ,&! maximum dew
        ! wtfact              ,&! fraction of model area with high water table
                                ! (updated to gridded 'fsatmax')
        capr                  ,&! tuning factor to turn first layer T into surface T
        cnfac                 ,&! Crank Nicholson factor between 0 and 1
        ssi                   ,&! irreducible water saturation of snow
        wimp                  ,&! water impermeable IF porosity less than wimp
        pondmx                ,&! ponding depth (mm)
        smpmax                ,&! wilting point potential in mm
        smpmin                ,&! restriction for min of soil poten.  (mm)
        trsmx0                ,&! max transpiration for moist soil+100% veg.  [mm/s]
        tcrit                   ! critical temp. to determine rain or snow

   integer,  intent(in) :: c3c4 ! 1 for C3, 0 for C4

   real(r8), intent(in) :: hpbl ! atmospheric boundary layer height [m]

   ! Forcing
   ! ----------------------
   real(r8), intent(in) :: &
        forc_pco2m            ,&! partial pressure of CO2 at observational height [pa]
        forc_po2m             ,&! partial pressure of O2 at observational height [pa]
        forc_us               ,&! wind speed in eastward direction [m/s]
        forc_vs               ,&! wind speed in northward direction [m/s]
        forc_t                ,&! temperature at agcm reference height [kelvin]
        forc_q                ,&! specific humidity at agcm reference height [kg/kg]
        forc_prc              ,&! convective precipitation [mm/s]
        forc_prl              ,&! large scale precipitation [mm/s]
        forc_psrf             ,&! atmosphere pressure at the surface [pa]
        forc_pbot             ,&! atmosphere pressure at the bottom of the atmos. model level [pa]
        forc_sols             ,&! atm vis direct beam solar rad onto srf [W/m2]
        forc_soll             ,&! atm nir direct beam solar rad onto srf [W/m2]
        forc_solsd            ,&! atm vis diffuse solar rad onto srf [W/m2]
        forc_solld            ,&! atm nir diffuse solar rad onto srf [W/m2]
        forc_frl              ,&! atmospheric infrared (longwave) radiation [W/m2]
        forc_hgt_u            ,&! observational height of wind [m]
        forc_hgt_t            ,&! observational height of temperature [m]
        forc_hgt_q            ,&! observational height of humidity [m]
        forc_rhoair             ! density air [kg/m3]

! Variables required for restart run
! ----------------------------------------------------------------------
   real(r8), intent(inout) :: &
        t_wallsun    (       1:nl_wall) ,&! sunlit wall layer temperature [K]
        t_wallsha    (       1:nl_wall) ,&! shaded wall layer temperature [K]
        t_soisno     (maxsnl+1:nl_soil) ,&! soil + snow layer temperature [K]
        t_roofsno    (maxsnl+1:nl_roof) ,&! soil + snow layer temperature [K]
        t_gimpsno    (maxsnl+1:nl_soil) ,&! soil + snow layer temperature [K]
        t_gpersno    (maxsnl+1:nl_soil) ,&! soil + snow layer temperature [K]
        t_lakesno    (maxsnl+1:nl_soil) ,&! soil + snow layer temperature [K]
        wliq_soisno  (maxsnl+1:nl_soil) ,&! liquid water (kg/m2)
        wliq_roofsno (maxsnl+1:nl_roof) ,&! liquid water (kg/m2)
        wliq_gimpsno (maxsnl+1:nl_soil) ,&! liquid water (kg/m2)
        wliq_gpersno (maxsnl+1:nl_soil) ,&! liquid water (kg/m2)
        wliq_lakesno (maxsnl+1:nl_soil) ,&! liquid water (kg/m2)
        wice_soisno  (maxsnl+1:nl_soil) ,&! ice lens (kg/m2)
        wice_roofsno (maxsnl+1:nl_roof) ,&! ice lens (kg/m2)
        wice_gimpsno (maxsnl+1:nl_soil) ,&! ice lens (kg/m2)
        wice_gpersno (maxsnl+1:nl_soil) ,&! ice lens (kg/m2)
        wice_lakesno (maxsnl+1:nl_soil) ,&! ice lens (kg/m2)
        smp          (       1:nl_soil) ,&! soil matrix potential [mm]
        hk           (       1:nl_soil) ,&! hydraulic conductivity [mm h2o/s]

        z_sno        (maxsnl+1:0)       ,&! node depth [m]
        dz_sno       (maxsnl+1:0)       ,&! interface depth [m]
        z_sno_roof   (maxsnl+1:0)       ,&! node depth of roof [m]
        z_sno_gimp   (maxsnl+1:0)       ,&! node depth of impervious [m]
        z_sno_gper   (maxsnl+1:0)       ,&! node depth pervious [m]
        z_sno_lake   (maxsnl+1:0)       ,&! node depth lake [m]
        dz_sno_roof  (maxsnl+1:0)       ,&! interface depth of roof [m]
        dz_sno_gimp  (maxsnl+1:0)       ,&! interface depth of impervious [m]
        dz_sno_gper  (maxsnl+1:0)       ,&! interface depth pervious [m]
        dz_sno_lake  (maxsnl+1:0)       ,&! interface depth lake [m]

        lakedepth                       ,&! lake depth (m)
        z_roof       (nl_roof)          ,&! thickness of roof [m]
        z_wall       (nl_wall)          ,&! thickness of wall [m]
        dz_roof      (nl_roof)          ,&! thickness of each layer [m]
        dz_wall      (nl_wall)          ,&! thickness of each layer [m]
        dz_lake      (nl_lake)          ,&! lake layer thickness (m)
        t_lake       (nl_lake)          ,&! lake temperature (kelvin)
        lake_icefrac (nl_lake)          ,&! lake mass fraction of lake layer that is frozen
        savedtke1                       ,&! top level eddy conductivity (W/m K)

        elvstd                ,&! standard deviation of elevation [m]
        BVIC                  ,&! b parameter in Fraction of saturated soil calculated by VIC

        t_grnd                ,&! ground surface temperature [k]
        tleaf                 ,&! sunlit leaf temperature [K]
        !tmax                 ,&! Diurnal Max 2 m height air temperature [kelvin]
        !tmin                 ,&! Diurnal Min 2 m height air temperature [kelvin]
        ldew                  ,&! depth of water on foliage [kg/m2/s]
        ldew_rain             ,&! depth of rain on foliage[kg/m2/s]
        ldew_snow             ,&! depth of snow on foliage[kg/m2/s]
        fwet_snow             ,&! vegetation canopy snow fractional cover [-]
        sag                   ,&! non dimensional snow age [-]
        sag_roof              ,&! non dimensional snow age [-]
        sag_gimp              ,&! non dimensional snow age [-]
        sag_gper              ,&! non dimensional snow age [-]
        sag_lake              ,&! non dimensional snow age [-]
        scv                   ,&! snow mass (kg/m2)
        scv_roof              ,&! snow mass (kg/m2)
        scv_gimp              ,&! snow mass (kg/m2)
        scv_gper              ,&! snow mass (kg/m2)
        scv_lake              ,&! snow mass (kg/m2)
        snowdp                ,&! snow depth (m)
        snowdp_roof           ,&! snow depth (m)
        snowdp_gimp           ,&! snow depth (m)
        snowdp_gper           ,&! snow depth (m)
        snowdp_lake           ,&! snow depth (m)
        zwt                   ,&! the depth to water table [m]
        wdsrf                 ,&! depth of surface water [mm]
        wa                    ,&! water storage in aquifer [mm]

        snw_rds   ( maxsnl+1:0 ) ,&! effective grain radius (col,lyr) [microns, m-6]
        mss_bcpho ( maxsnl+1:0 ) ,&! mass of hydrophobic BC in snow  (col,lyr) [kg]
        mss_bcphi ( maxsnl+1:0 ) ,&! mass of hydrophillic BC in snow (col,lyr) [kg]
        mss_ocpho ( maxsnl+1:0 ) ,&! mass of hydrophobic OC in snow  (col,lyr) [kg]
        mss_ocphi ( maxsnl+1:0 ) ,&! mass of hydrophillic OC in snow (col,lyr) [kg]
        mss_dst1  ( maxsnl+1:0 ) ,&! mass of dust species 1 in snow  (col,lyr) [kg]
        mss_dst2  ( maxsnl+1:0 ) ,&! mass of dust species 2 in snow  (col,lyr) [kg]
        mss_dst3  ( maxsnl+1:0 ) ,&! mass of dust species 3 in snow  (col,lyr) [kg]
        mss_dst4  ( maxsnl+1:0 ) ,&! mass of dust species 4 in snow  (col,lyr) [kg]
        ssno   (2,2,maxsnl+1:1 ) ,&! snow layer absorption [-]

        fveg                  ,&! fraction of vegetation cover
        fsno                  ,&! fractional snow cover
        fsno_roof             ,&! fractional snow cover
        fsno_gimp             ,&! fractional snow cover
        fsno_gper             ,&! fractional snow cover
        fsno_lake             ,&! fractional snow cover
        sigf                  ,&! fraction of veg cover, excluding snow-covered veg [-]
        green                 ,&! greenness
        lai                   ,&! leaf area index
        sai                   ,&! stem area index
        htop                  ,&! canopy crown top
        hbot                  ,&! canopy crown bottom

        lwsun                 ,&! net longwave of sunlit wall [W/m2]
        lwsha                 ,&! net longwave of shaded wall [W/m2]
        lgimp                 ,&! net longwave of impervious  [W/m2]
        lgper                 ,&! net longwave of pervious [W/m2]
        lveg                  ,&! net longwave of vegetation [W/m2]
        fwsun                 ,&! sunlit fraction of walls [-]
        dfwsun                ,&! change of sunlit fraction of walls [-]
        t_room                ,&! temperature of inner building [K]
        troof_inner           ,&! temperature of inner roof [K]
        twsun_inner           ,&! temperature of inner sunlit wall [K]
        twsha_inner           ,&! temperature of inner shaded wall [K]
        t_roommax             ,&! maximum temperature of inner room [K]
        t_roommin             ,&! minimum temperature of inner room [K]
        tafu                  ,&! temperature of outer building [K]
        Fhac                  ,&! sensible flux from heat or cool AC [W/m2]
        Fwst                  ,&! waste heat flux from heat or cool AC [W/m2]
        Fach                  ,&! flux from inner and outer air exchange [W/m2]
        Fahe                  ,&! flux from metabolism and vehicle [W/m2]
        Fhah                  ,&! sensible heat flux from heating [W/m2]
        vehc                  ,&! flux from vehicle [W/m2]
        meta                  ,&! flux from metabolism [W/m2]

        extkd                 ,&! diffuse and scattered diffuse PAR extinction coefficient
        alb  (2,2)            ,&! averaged albedo [-]
        ssun (2,2)            ,&! sunlit canopy absorption for solar radiation
        ssha (2,2)            ,&! shaded canopy absorption for solar radiation
        sroof(2,2)            ,&! shaded canopy absorption for solar radiation
        swsun(2,2)            ,&! shaded canopy absorption for solar radiation
        swsha(2,2)            ,&! shaded canopy absorption for solar radiation
        sgimp(2,2)            ,&! shaded canopy absorption for solar radiation
        sgper(2,2)            ,&! shaded canopy absorption for solar radiation
        slake(2,2)              ! shaded canopy absorption for solar radiation

! additional diagnostic variables for output
   real(r8), intent(out) :: &
        laisun                ,&! sunlit leaf area index
        laisha                ,&! shaded leaf area index
        rstfac                ,&! factor of soil water stress
        rss                   ,&! soil surface resistance
        wat                   ,&! total water storage
        h2osoi(nl_soil)         ! volumetric soil water in layers [m3/m3]

! Fluxes
! ----------------------------------------------------------------------
   real(r8), intent(out) :: &
        taux                  ,&! wind stress: E-W [kg/m/s**2]
        tauy                  ,&! wind stress: N-S [kg/m/s**2]
        fsena                 ,&! sensible heat from canopy height to atmosphere [W/m2]
        fevpa                 ,&! evapotranspiration from canopy height to atmosphere [mm/s]
        lfevpa                ,&! latent heat flux from canopy height to atmosphere [W/2]
        fsenl                 ,&! sensible heat from leaves [W/m2]
        fevpl                 ,&! evaporation+transpiration from leaves [mm/s]
        etr                   ,&! transpiration rate [mm/s]
        fseng                 ,&! sensible heat flux from ground [W/m2]
        fevpg                 ,&! evaporation heat flux from ground [mm/s]
        olrg                  ,&! outgoing long-wave radiation from ground+canopy
        fgrnd                 ,&! ground heat flux [W/m2]
        xerr                  ,&! water balance error at current time-step [mm/s]
        zerr                  ,&! energy balance error at current time-step [W/m2]

        tref                  ,&! 2 m height air temperature [K]
        qref                  ,&! 2 m height air specific humidity
        trad                  ,&! radiative temperature [K]
        rsur                  ,&! surface runoff (mm h2o/s)
        rnof                  ,&! total runoff (mm h2o/s)
        qintr                 ,&! interception (mm h2o/s)
        qinfl                 ,&! infiltration (mm h2o/s)
        qdrip                 ,&! throughfall (mm h2o/s)
        qcharge               ,&! groundwater recharge [mm/s]

        rst                   ,&! canopy stomatal resistance
        assim                 ,&! canopy assimilation
        respc                 ,&! canopy respiration

        fsen_roof             ,&! sensible heat flux from roof [W/m2]
        fsen_wsun             ,&! sensible heat flux from sunlit wall [W/m2]
        fsen_wsha             ,&! sensible heat flux from shaded wall [W/m2]
        fsen_gimp             ,&! sensible heat flux from impervious road [W/m2]
        fsen_gper             ,&! sensible heat flux from pervious road [W/m2]
        fsen_urbl             ,&! sensible heat flux from urban vegetation [W/m2]

        lfevp_roof            ,&! latent heat flux from roof [W/m2]
        lfevp_gimp            ,&! latent heat flux from impervious road [W/m2]
        lfevp_gper            ,&! latent heat flux from pervious road [W/m2]
        lfevp_urbl            ,&! latent heat flux from urban vegetation [W/m2]

        troof                 ,&! temperature of roof [K]
        twall                 ,&! temperature of wall [K]

        sabvsun               ,&! solar absorbed by sunlit vegetation [W/m2]
        sabvsha               ,&! solar absorbed by shaded vegetation [W/m2]
        sabg                  ,&! solar absorbed by ground  [W/m2]
        sr                    ,&! total reflected solar radiation (W/m2)
        solvd                 ,&! incident direct beam vis solar radiation (W/m2)
        solvi                 ,&! incident diffuse beam vis solar radiation (W/m2)
        solnd                 ,&! incident direct beam nir solar radiation (W/m2)
        solni                 ,&! incident diffuse beam nir solar radiation (W/m2)
        srvd                  ,&! reflected direct beam vis solar radiation (W/m2)
        srvi                  ,&! reflected diffuse beam vis solar radiation (W/m2)
        srnd                  ,&! reflected direct beam nir solar radiation (W/m2)
        srni                  ,&! reflected diffuse beam nir solar radiation (W/m2)
        solvdln               ,&! incident direct beam vis solar radiation at local noon(W/m2)
        solviln               ,&! incident diffuse beam vis solar radiation at local noon(W/m2)
        solndln               ,&! incident direct beam nir solar radiation at local noon(W/m2)
        solniln               ,&! incident diffuse beam nir solar radiation at local noon(W/m2)
        srvdln                ,&! reflected direct beam vis solar radiation at local noon(W/m2)
        srviln                ,&! reflected diffuse beam vis solar radiation at local noon(W/m2)
        srndln                ,&! reflected direct beam nir solar radiation at local noon(W/m2)
        srniln                ,&! reflected diffuse beam nir solar radiation at local noon(W/m2)

        forc_rain             ,&! rain [mm/s]
        forc_snow             ,&! snow [mm/s]

        emis                  ,&! averaged bulk surface emissivity
        z0m                   ,&! effective roughness [m]
        zol                   ,&! dimensionless height (z/L) used in Monin-Obukhov theory
        rib                   ,&! bulk Richardson number in surface layer
        ustar                 ,&! u* in similarity theory [m/s]
        qstar                 ,&! q* in similarity theory [kg/kg]
        tstar                 ,&! t* in similarity theory [K]
        fm                    ,&! integral of profile function for momentum
        fh                    ,&! integral of profile function for heat
        fq                      ! integral of profile function for moisture

!-------------------------- Local Variables ----------------------------
   real(r8) :: &
        calday                ,&! Julian cal day (1.xx to 365.xx)
        endwb                 ,&! water mass at the end of time step
        errore                ,&! energy balance error (Wm-2)
        errorw                ,&! water balance error (mm)
        fioldr (maxsnl+1:nl_roof), &! fraction of ice relative to the total water
        fioldi (maxsnl+1:nl_soil), &! fraction of ice relative to the total water
        fioldp (maxsnl+1:nl_soil), &! fraction of ice relative to the total water
        fioldl (maxsnl+1:nl_soil), &! fraction of ice relative to the total water
        w_old                 ,&! liquid water mass of the column at the previous time step (mm)
        theta                 ,&! sun zenith angle
        sabv                  ,&! solar absorbed by vegetation [W/m2]
        sabroof               ,&! solar absorbed by vegetation [W/m2]
        sabwsun               ,&! solar absorbed by vegetation [W/m2]
        sabwsha               ,&! solar absorbed by vegetation [W/m2]
        sabgimp               ,&! solar absorbed by vegetation [W/m2]
        sabgper               ,&! solar absorbed by vegetation [W/m2]
        sablake               ,&! solar absorbed by vegetation [W/m2]
        par                   ,&! PAR by leaves [W/m2]
        tgimp                 ,&! temperature of impervious surface [K]
        tgper                 ,&! temperature of pervious surface [K]
        tlake                 ,&! temperature of lake surface [K]
        qdrip_gper            ,&! throughfall of pervious (mm h2o/s)
        qseva_roof            ,&! ground surface evaporation rate (mm h2o/s)
        qseva_gimp            ,&! ground surface evaporation rate (mm h2o/s)
        qseva_gper            ,&! ground surface evaporation rate (mm h2o/s)
        qseva_lake            ,&! ground surface evaporation rate (mm h2o/s)
        qsdew_roof            ,&! ground surface dew formation (mm h2o /s) [+]
        qsdew_gimp            ,&! ground surface dew formation (mm h2o /s) [+]
        qsdew_gper            ,&! ground surface dew formation (mm h2o /s) [+]
        qsdew_lake            ,&! ground surface dew formation (mm h2o /s) [+]
        qsubl_roof            ,&! sublimation rate from snow pack (mm h2o /s) [+]
        qsubl_gimp            ,&! sublimation rate from snow pack (mm h2o /s) [+]
        qsubl_gper            ,&! sublimation rate from snow pack (mm h2o /s) [+]
        qsubl_lake            ,&! sublimation rate from snow pack (mm h2o /s) [+]
        qfros_roof            ,&! surface dew added to snow pack (mm h2o /s) [+]
        qfros_gimp            ,&! surface dew added to snow pack (mm h2o /s) [+]
        qfros_gper            ,&! surface dew added to snow pack (mm h2o /s) [+]
        qfros_lake            ,&! surface dew added to snow pack (mm h2o /s) [+]
        scvold_roof           ,&! snow mass on roof for previous time step [kg/m2]
        scvold_gimp           ,&! snow mass on impervious surfaces for previous time step [kg/m2]
        scvold_gper           ,&! snow mass on pervious surfaces for previous time step [kg/m2]
        scvold_lake           ,&! snow mass on lake for previous time step [kg/m2]
        sm_roof               ,&! rate of snowmelt [kg/(m2 s)]
        sm_gimp               ,&! rate of snowmelt [kg/(m2 s)]
        sm_gper               ,&! rate of snowmelt [kg/(m2 s)]
        sm_lake               ,&! rate of snowmelt [kg/(m2 s)]
        totwb                 ,&! water mass at the begining of time step
        totwb_roof            ,&! water mass at the begining of time step
        totwb_gimp            ,&! water mass at the begining of time step
        totwb_gper            ,&! water mass at the begining of time step
        wt                    ,&! fraction of vegetation buried (covered) by snow [-]
        rootr    (1:nl_soil)  ,&! root resistance of a layer, all layers add to 1.0
        rootflux (1:nl_soil)  ,&! root resistance of a layer, all layers add to 1.0
        etr_deficit           ,&! urban tree etr deficit [mm/s]
        urb_irrig             ,&! named urban tree irrigation [mm/s]

        zi_wall    (       0:nl_wall) ,&! interface level below a "z" level [m]
        z_roofsno  (maxsnl+1:nl_roof) ,&! layer depth [m]
        z_gimpsno  (maxsnl+1:nl_soil) ,&! layer depth [m]
        z_gpersno  (maxsnl+1:nl_soil) ,&! layer depth [m]
        z_lakesno  (maxsnl+1:nl_soil) ,&! layer depth [m]
        dz_roofsno (maxsnl+1:nl_roof) ,&! layer thickness [m]
        dz_gimpsno (maxsnl+1:nl_soil) ,&! layer thickness [m]
        dz_gpersno (maxsnl+1:nl_soil) ,&! layer thickness [m]
        dz_lakesno (maxsnl+1:nl_soil) ,&! layer thickness [m]
        zi_roofsno (maxsnl  :nl_roof) ,&! interface level below a "z" level [m]
        zi_gimpsno (maxsnl  :nl_soil) ,&! interface level below a "z" level [m]
        zi_gpersno (maxsnl  :nl_soil) ,&! interface level below a "z" level [m]
        zi_lakesno (maxsnl  :nl_soil)   ! interface level below a "z" level [m]

   real(r8) :: &
        prc_rain              ,&! convective rainfall [kg/(m2 s)]
        prc_snow              ,&! convective snowfall [kg/(m2 s)]
        prl_rain              ,&! large scale rainfall [kg/(m2 s)]
        prl_snow              ,&! large scale snowfall [kg/(m2 s)]
        t_precip              ,&! snowfall/rainfall temperature [kelvin]
        bifall                ,&! bulk density of newly fallen dry snow [kg/m3]
        pg_rain               ,&! rainfall onto ground including canopy runoff [kg/(m2 s)]
        pg_snow               ,&! snowfall onto ground including canopy runoff [kg/(m2 s)]
        pgper_rain            ,&! rainfall onto ground including canopy runoff [kg/(m2 s)]
        pgper_snow            ,&! snowfall onto ground including canopy runoff [kg/(m2 s)]
        pgimp_rain            ,&! rainfall onto ground including canopy runoff [kg/(m2 s)]
        pgimp_snow            ,&! snowfall onto ground including canopy runoff [kg/(m2 s)]
        pg_rain_lake          ,&! rainfall onto lake [kg/(m2 s)]
        pg_snow_lake          ,&! snowfall onto lake [kg/(m2 s)]
        qintr_rain            ,&! rainfall interception (mm h2o/s)
        qintr_snow            ,&! snowfall interception (mm h2o/s)
        etrgper               ,&! etr for pervious ground
        fveg_gper             ,&! fraction of fveg/fgper
        fveg_gimp               ! fraction of fveg/fgimp

   real(r8) :: &
        ei                    ,&! vapor pressure on leaf surface [pa]
        deidT                 ,&! derivative of "ei" on "tl" [pa/K]
        qsatl                 ,&! leaf specific humidity [kg/kg]
        qsatldT                 ! derivative of "qsatl" on "tlef"

   integer :: &
        snlr                  ,&! number of snow layers
        snli                  ,&! number of snow layers
        snlp                  ,&! number of snow layers
        snll                  ,&! number of snow layers
        imeltr (maxsnl+1:nl_roof), &! flag for: melting=1, freezing=2, Nothing happened=0
        imelti (maxsnl+1:nl_soil), &! flag for: melting=1, freezing=2, Nothing happened=0
        imeltp (maxsnl+1:nl_soil), &! flag for: melting=1, freezing=2, Nothing happened=0
        imeltl (maxsnl+1:nl_soil), &! flag for: melting=1, freezing=2, Nothing happened=0
        lbr                   ,&! lower bound of arrays
        lbi                   ,&! lower bound of arrays
        lbp                   ,&! lower bound of arrays
        lbl                   ,&! lower bound of arrays
        lbsn                  ,&! lower bound of arrays
        j                       ! DO looping index

   ! For SNICAR snow model
   !----------------------------------------------------------------------
   real(r8) forc_aer        ( 14 )  !aerosol deposition from atmosphere model (grd,aer) [kg m-1 s-1]
   real(r8) snofrz    (maxsnl+1:0)  !snow freezing rate (col,lyr) [kg m-2 s-1]
   real(r8) sabg_lyr  (maxsnl+1:1)  !snow layer absorption [W/m-2]

   !irrigation
   real(r8) :: &
         qflx_irrig_drip      ,&! drip irrigation rate [mm/s]
         qflx_irrig_sprinkler ,&! sprinkler irrigation rate [mm/s]
         qflx_irrig_flood     ,&! flood irrigation rate [mm/s]
         qflx_irrig_paddy       ! paddy irrigation rate [mm/s]

   ! A simple urban irrigation scheme accounts for soil water stress of trees
   ! a factor represents irrigation efficiency, '1' represents a 50% direct irrigation efficiency.
   real(r8), parameter :: wst_irrig = 1.0

!-----------------------------------------------------------------------

      theta = acos(max(coszen,0.01))
      forc_aer(:) = 0.          !aerosol deposition from atmosphere model (grd,aer) [kg m-1 s-1]

!======================================================================
!  [1] Solar absorbed by vegetation and ground
!      and precipitation information (rain/snow fall and precip temperature
!======================================================================

      CALL netsolar_urban (ipatch,idate,patchlonr,deltim,&
                           forc_sols,forc_soll,forc_solsd,forc_solld,lai,sai,rho,tau,&
                           alb(:,:),ssun(:,:),ssha(:,:),sroof(:,:),swsun(:,:),&
                           swsha(:,:),sgimp(:,:),sgper(:,:),slake(:,:),&
                           sr,sabv,par,sabroof,sabwsun,sabwsha,sabgimp,sabgper,sablake,&
                           solvd,solvi,solnd,solni,srvd,srvi,srnd,srni,&
                           solvdln,solviln,solndln,solniln,srvdln,srviln,srndln,srniln)

      CALL rain_snow_temp (patchtype,forc_t,forc_q,forc_psrf,forc_prc,forc_prl,forc_us,forc_vs,&
                           tcrit,prc_rain,prc_snow,prl_rain,prl_snow,t_precip,bifall)

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

      sabvsun = sabv * fveg * (1-flake)
      sabvsha = 0.

!======================================================================

      z_roofsno (maxsnl+1:0) = z_sno_roof (maxsnl+1:0)
      z_roofsno (1:nl_roof ) = z_roof (1:nl_roof)
      dz_roofsno(maxsnl+1:0) = dz_sno_roof(maxsnl+1:0)
      dz_roofsno(1:nl_roof ) = dz_roof(1:nl_roof)

      z_gimpsno (maxsnl+1:0) = z_sno_gimp (maxsnl+1:0)
      z_gimpsno (1:nl_soil ) = z_soi (1:nl_soil)
      dz_gimpsno(maxsnl+1:0) = dz_sno_gimp(maxsnl+1:0)
      dz_gimpsno(1:nl_soil ) = dz_soi(1:nl_soil)

      z_gpersno (maxsnl+1:0) = z_sno_gper (maxsnl+1:0)
      z_gpersno (1:nl_soil ) = z_soi (1:nl_soil)
      dz_gpersno(maxsnl+1:0) = dz_sno_gper(maxsnl+1:0)
      dz_gpersno(1:nl_soil ) = dz_soi(1:nl_soil)

      z_lakesno (maxsnl+1:0) = z_sno_lake (maxsnl+1:0)
      z_lakesno (1:nl_soil ) = z_soi (1:nl_soil)
      dz_lakesno(maxsnl+1:0) = dz_sno_lake(maxsnl+1:0)
      dz_lakesno(1:nl_soil ) = dz_soi(1:nl_soil)

      !============================================================
      zi_wall(0) = 0.
      DO j = 1, nl_wall
         zi_wall(j) = zi_wall(j-1) + dz_wall(j)
      ENDDO

      !============================================================
      scvold_roof = scv_roof        !snow mass at previous time step

      snlr = 0
      DO j = maxsnl+1, 0
         IF (wliq_roofsno(j)+wice_roofsno(j) > 0.) snlr = snlr - 1
      ENDDO

      zi_roofsno(0) = 0.
      IF (snlr < 0) THEN
         DO j = -1, snlr, -1
            zi_roofsno(j) = zi_roofsno(j+1) - dz_roofsno(j+1)
         ENDDO
      ENDIF
      DO j = 1, nl_roof
         zi_roofsno(j) = zi_roofsno(j-1) + dz_roofsno(j)
      ENDDO

      totwb_roof = scv_roof + wice_roofsno(1) + wliq_roofsno(1)
      fioldr(:) = 0.0
      IF (snlr < 0) THEN
         fioldr(snlr+1:0) = wice_roofsno(snlr+1:0) / &
            (wliq_roofsno(snlr+1:0) + wice_roofsno(snlr+1:0))
      ENDIF

      !============================================================
      scvold_gimp = scv_gimp        !snow mass at previous time step

      snli = 0
      DO j = maxsnl+1, 0
         IF (wliq_gimpsno(j)+wice_gimpsno(j) > 0.) snli = snli - 1
      ENDDO

      zi_gimpsno(0) = 0.
      IF (snli < 0) THEN
         DO j = -1, snli, -1
            zi_gimpsno(j) = zi_gimpsno(j+1) - dz_gimpsno(j+1)
         ENDDO
      ENDIF

      zi_gimpsno(1:nl_soil) = zi_soi(1:nl_soil)

      totwb_gimp = scv_gimp + wice_gimpsno(1) + wliq_gimpsno(1)
      fioldi(:) = 0.0
      IF (snli < 0) THEN
         fioldi(snli+1:0) = wice_gimpsno(snli+1:0) / &
            (wliq_gimpsno(snli+1:0) + wice_gimpsno(snli+1:0))
      ENDIF

      !============================================================
      scvold_gper = scv_gper        !snow mass at previous time step

      snlp = 0
      DO j = maxsnl+1, 0
         IF(wliq_gpersno(j)+wice_gpersno(j) > 0.) snlp = snlp - 1
      ENDDO

      zi_gpersno(0) = 0.
      IF (snlp < 0) THEN
         DO j = -1, snlp, -1
            zi_gpersno(j) = zi_gpersno(j+1) - dz_gpersno(j+1)
         ENDDO
      ENDIF

      zi_gpersno(1:nl_soil) = zi_soi(1:nl_soil)

      totwb_gper = ldew + scv_gper + sum(wice_gpersno(1:) + wliq_gpersno(1:)) + wa
      fioldp(:) = 0.0
      IF (snlp < 0) THEN
         fioldp(snlp+1:0) = wice_gpersno(snlp+1:0) / &
            (wliq_gpersno(snlp+1:0) + wice_gpersno(snlp+1:0))
      ENDIF

      !============================================================
      scvold_lake = scv_lake        !snow mass at previous time step

      snll = 0
      DO j = maxsnl+1, 0
         IF (wliq_lakesno(j) + wice_lakesno(j) > 0.) snll = snll - 1
      ENDDO

      zi_lakesno(0) = 0.
      IF (snll < 0) THEN
         DO j = -1, snll, -1
            zi_lakesno(j) = zi_lakesno(j+1) - dz_lakesno(j+1)
         ENDDO
      ENDIF

      zi_lakesno(1:nl_soil) = zi_soi(1:nl_soil)

      w_old = sum(wliq_lakesno(snll+1:))
      fioldl(:) = 0.0
      IF (snll <0 ) THEN
         fioldl(snll+1:0) = wice_lakesno(snll+1:0) / &
            (wliq_lakesno(snll+1:0) + wice_lakesno(snll+1:0))
      ENDIF

      !============================================================
      totwb  = sum(wice_soisno(1:) + wliq_soisno(1:))
      totwb  = totwb + scv + ldew*fveg + wa*(1-froof)*fgper

      etr_deficit = 0.
      urb_irrig   = 0.

!----------------------------------------------------------------------
! [2] Canopy interception and precipitation onto ground surface
!----------------------------------------------------------------------
      qflx_irrig_drip = 0._r8
      qflx_irrig_sprinkler = 0._r8
      qflx_irrig_flood = 0._r8
      qflx_irrig_paddy = 0._r8

      ! with vegetation canopy
      CALL LEAF_interception_CoLM2014 (deltim,dewmx,forc_us,forc_vs,chil,sigf,lai,sai,tref,tleaf,&
                              prc_rain,prc_snow,prl_rain,prl_snow,qflx_irrig_sprinkler,bifall,&
                              ldew,ldew_rain,ldew_snow,z0m,forc_hgt_u,pgper_rain,pgper_snow,&
                              qintr,qintr_rain,qintr_snow)

      ! for output, patch scale
      qintr = qintr * fveg * (1-flake)
      qdrip_gper = pgper_rain + pgper_snow
      qdrip = forc_rain + forc_snow
      qdrip = qdrip*(1-fveg*(1-flake)) + qdrip_gper*fveg*(1-flake)

      ! without vegetation canopy
      pg_rain = prc_rain + prl_rain
      pg_snow = prc_snow + prl_snow
      pg_rain_lake = prc_rain + prl_rain
      pg_snow_lake = prc_snow + prl_snow

      ! for urban hydrology input, only for pervious ground
      IF (fgper > 0) THEN
         fveg_gper = fveg/((1-froof)*fgper)
      ELSE
         fveg_gper = 0.
      ENDIF

      IF (fgper < 1) THEN
         fveg_gimp = (fveg-(1-froof)*fgper)/((1-froof)*(1-fgper))
      ELSE
         fveg_gimp = 0.
      ENDIF

      IF (fveg_gper .le. 1) THEN
         pgper_rain = pgper_rain*fveg_gper + pg_rain*(1-fveg_gper)
         pgper_snow = pgper_snow*fveg_gper + pg_snow*(1-fveg_gper)
         pgimp_rain = pg_rain
         pgimp_snow = pg_snow
      ELSE
         pgimp_rain = pgper_rain*fveg_gimp + pg_rain*(1-fveg_gimp)
         pgimp_snow = pgper_snow*fveg_gimp + pg_snow*(1-fveg_gimp)
      ENDIF

!----------------------------------------------------------------------
! [3] Initialize new snow nodes for snowfall / sleet
!----------------------------------------------------------------------

      lbr = snlr + 1           !lower bound of array
      lbi = snli + 1           !lower bound of array
      lbp = snlp + 1           !lower bound of array
      troof = t_roofsno(lbr)
      tgimp = t_gimpsno(lbi)
      tgper = t_gpersno(lbp)

      CALL newsnow (patchtype,maxsnl,deltim,troof,pg_rain,pg_snow,bifall,&
                    t_precip,zi_roofsno(:0),z_roofsno(:0),dz_roofsno(:0),t_roofsno(:0),&
                    wliq_roofsno(:0),wice_roofsno(:0),fioldr(:0),&
                    snlr,sag_roof,scv_roof,snowdp_roof,fsno_roof)

      CALL newsnow (patchtype,maxsnl,deltim,tgimp,pgimp_rain,pgimp_snow,bifall,&
                    t_precip,zi_gimpsno(:0),z_gimpsno(:0),dz_gimpsno(:0),t_gimpsno(:0),&
                    wliq_gimpsno(:0),wice_gimpsno(:0),fioldi(:0),&
                    snli,sag_gimp,scv_gimp,snowdp_gimp,fsno_gimp)

      CALL newsnow (patchtype,maxsnl,deltim,tgper,pgper_rain,pgper_snow,bifall,&
                    t_precip,zi_gpersno(:0),z_gpersno(:0),dz_gpersno(:0),t_gpersno(:0),&
                    wliq_gpersno(:0),wice_gpersno(:0),fioldp(:0),&
                    snlp,sag_gper,scv_gper,snowdp_gper,fsno_gper)

      CALL newsnow_lake ( DEF_USE_Dynamic_Lake, &
           ! "in" arguments
           ! ---------------
           maxsnl        ,nl_lake       ,deltim          ,dz_lake         ,&
           pg_rain_lake  ,pg_snow_lake  ,t_precip        ,bifall          ,&

           ! "inout" arguments
           ! ------------------
           t_lake        ,zi_lakesno(:0),z_lakesno(:0)                    ,&
           dz_lakesno(:0),t_lakesno(:0) ,wliq_lakesno(:0),wice_lakesno(:0),&
           fioldl(:0)    ,snll          ,sag_lake        ,scv_lake        ,&
           snowdp_lake   ,lake_icefrac                                     )

!----------------------------------------------------------------------
! [4] Energy and Water balance
!----------------------------------------------------------------------

      lbr = snlr + 1           !lower bound of array
      lbi = snli + 1           !lower bound of array
      lbp = snlp + 1           !lower bound of array
      lbl = snll + 1           !lower bound of array
      lbsn= min(lbp,0)

      ! Thermal process
      CALL UrbanTHERMAL ( &
         ! model running information
         ipatch             ,patchtype          ,lbr                ,lbi                ,&
         lbp                ,lbl                ,deltim             ,patchlatr          ,&
         ! forcing
         forc_hgt_u         ,forc_hgt_t         ,forc_hgt_q         ,forc_us            ,&
         forc_vs            ,forc_t             ,forc_q             ,forc_psrf          ,&
         forc_rhoair        ,forc_frl           ,forc_po2m          ,forc_pco2m         ,&
         forc_sols          ,forc_soll          ,forc_solsd         ,forc_solld         ,&
         theta              ,sabroof            ,sabwsun            ,sabwsha            ,&
         sabgimp            ,sabgper            ,sablake            ,sabv               ,&
         par                ,Fhac               ,Fwst               ,Fach               ,&
         Fahe               ,Fhah               ,vehc               ,meta               ,&
         ! LUCY INPUT PARAMETERS
         fix_holiday        ,week_holiday       ,hum_prof           ,pop_den            ,&
         vehicle            ,weh_prof           ,wdh_prof           ,idate              ,&
         patchlonr                                                                      ,&
         ! GROUND PARAMETERS
         froof              ,flake              ,hroof              ,hlr                ,&
         fgper              ,pondmx             ,em_roof            ,em_wall            ,&
         em_gimp            ,em_gper            ,trsmx0             ,zlnd               ,&
         zsno               ,capr               ,cnfac              ,vf_quartz          ,&
         vf_gravels         ,vf_om              ,vf_sand            ,wf_gravels         ,&
         wf_sand            ,csol               ,porsl              ,psi0               ,&
#ifdef Campbell_SOIL_MODEL
         bsw                                                                            ,&
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
         theta_r            ,alpha_vgm          ,n_vgm              ,L_vgm              ,&
         sc_vgm             ,fc_vgm                                                     ,&
#endif
         k_solids           ,dksatu             ,dksatf             ,dkdry              ,&
         BA_alpha           ,BA_beta                                                    ,&
         cv_roof            ,cv_wall            ,cv_gimp                                ,&
         tk_roof            ,tk_wall            ,tk_gimp            ,dz_roofsno(lbr:)   ,&
         dz_gimpsno(lbi:)   ,dz_gpersno(lbp:)   ,dz_lakesno(:)      ,dz_wall(:)         ,&
         z_roofsno(lbr:)    ,z_gimpsno(lbi:)    ,z_gpersno(lbp:)    ,z_lakesno(:)       ,&
         z_wall(:)          ,zi_roofsno(lbr-1:) ,zi_gimpsno(lbi-1:) ,zi_gpersno(lbp-1:) ,&
         zi_lakesno(:)      ,zi_wall(0:)        ,dz_lake(1:)        ,lakedepth          ,&
         dewmx              ,sqrtdi             ,rootfr(:)          ,effcon             ,&
         vmax25             ,c3c4               ,slti               ,hlti               ,shti,&
         hhti               ,trda               ,trdm               ,trop               ,&
         g1                 ,g0                 ,gradm              ,binter             ,&
         extkn              ,lambda                                                     ,&
         ! surface status
         fsno_roof          ,fsno_gimp          ,fsno_gper          ,scv_roof           ,&
         scv_gimp           ,scv_gper           ,scv_lake           ,snowdp_roof        ,&
         snowdp_gimp        ,snowdp_gper        ,snowdp_lake        ,fwsun              ,&
         dfwsun             ,lai                ,sai                ,htop               ,&
         hbot               ,fveg               ,sigf               ,extkd              ,&
         lwsun              ,lwsha              ,lgimp              ,lgper              ,&
         t_grnd             ,t_roofsno(lbr:)    ,t_wallsun(:)       ,t_wallsha(:)       ,&
         t_gimpsno(lbi:)    ,t_gpersno(lbp:)    ,t_lakesno(:)       ,wliq_roofsno(lbr:) ,&
         wliq_gimpsno(lbi:) ,wliq_gpersno(lbp:) ,wliq_lakesno(:)    ,wice_roofsno(lbr:) ,&
         wice_gimpsno(lbi:) ,wice_gpersno(lbp:) ,wice_lakesno(:)    ,t_lake(:)          ,&
         lake_icefrac(:)    ,savedtke1          ,lveg               ,tleaf              ,&
         ldew               ,ldew_rain          ,ldew_snow          ,fwet_snow          ,&
         t_room             ,troof_inner        ,twsun_inner        ,twsha_inner        ,&
         t_roommax          ,t_roommin          ,tafu                                   ,&

! SNICAR model variables
         snofrz(lbsn:0)     ,sabg_lyr(lbp:1)                                            ,&
! END SNICAR model variables

         ! output
         taux               ,tauy               ,fsena              ,fevpa              ,&
         lfevpa             ,fsenl              ,fevpl              ,etr                ,&
         fseng              ,fevpg              ,olrg               ,fgrnd              ,&
         fsen_roof          ,fsen_wsun          ,fsen_wsha          ,fsen_gimp          ,&
         fsen_gper          ,fsen_urbl          ,troof              ,twall              ,&
         lfevp_roof         ,lfevp_gimp         ,lfevp_gper         ,lfevp_urbl         ,&
         qseva_roof         ,qseva_gimp         ,qseva_gper         ,qseva_lake         ,&
         qsdew_roof         ,qsdew_gimp         ,qsdew_gper         ,qsdew_lake         ,&
         qsubl_roof         ,qsubl_gimp         ,qsubl_gper         ,qsubl_lake         ,&
         qfros_roof         ,qfros_gimp         ,qfros_gper         ,qfros_lake         ,&
         imeltr(lbr:)       ,imelti(lbi:)       ,imeltp(lbp:)       ,imeltl(:)          ,&
         sm_roof            ,sm_gimp            ,sm_gper            ,sm_lake            ,&
         sabg               ,rstfac             ,rootr(:)           ,etr_deficit        ,&
         tref               ,qref               ,trad               ,rst                ,&
         assim              ,respc              ,errore             ,emis               ,&
         z0m                ,zol                ,rib                ,ustar              ,&
         qstar              ,tstar              ,fm                 ,fh                 ,&
         fq                 ,hpbl                                                        )

!----------------------------------------------------------------------
! [5] Urban hydrology
!----------------------------------------------------------------------
      IF (fveg > 0) THEN
         ! convert to unit area
         etrgper = (etr-etr_deficit)/(1-froof)/fgper
      ELSE
         etrgper = 0.
      ENDIF

      pgper_rain = pgper_rain  + wst_irrig*etr_deficit/(1-froof)/fgper
      urb_irrig  = etr_deficit + wst_irrig*etr_deficit

      CALL UrbanHydrology ( &
         ! model running information
         ipatch             ,patchtype          ,lbr                ,lbi                ,&
         lbp                ,lbl                ,snll               ,deltim             ,&
         ! forcing
         pg_rain            ,pgper_rain         ,pgimp_rain         ,pg_snow            ,&
         pg_rain_lake       ,pg_snow_lake                                               ,&
         froof              ,fgper              ,flake              ,bsw                ,&
         porsl              ,psi0               ,hksati             ,pondmx             ,&
         ssi                ,wimp               ,smpmin             ,theta_r            ,&
         fsatmax            ,fsatdcf            ,elvstd             ,BVIC               ,&
         rootr,rootflux     ,etrgper            ,fseng              ,fgrnd              ,&
         t_gpersno(lbp:)    ,t_lakesno(:)       ,t_lake             ,dz_lake            ,&
         z_gpersno(lbp:)    ,z_lakesno(:)       ,zi_gpersno(lbp-1:) ,zi_lakesno(:)      ,&
         dz_roofsno(lbr:)   ,dz_gimpsno(lbi:)   ,dz_gpersno(lbp:)   ,dz_lakesno(:)      ,&
         wliq_roofsno(lbr:) ,wliq_gimpsno(lbi:) ,wliq_gpersno(lbp:) ,wliq_lakesno(:)    ,&
         wice_roofsno(lbr:) ,wice_gimpsno(lbi:) ,wice_gpersno(lbp:) ,wice_lakesno(:)    ,&
         qseva_roof         ,qseva_gimp         ,qseva_gper         ,qseva_lake         ,&
         qsdew_roof         ,qsdew_gimp         ,qsdew_gper         ,qsdew_lake         ,&
         qsubl_roof         ,qsubl_gimp         ,qsubl_gper         ,qsubl_lake         ,&
         qfros_roof         ,qfros_gimp         ,qfros_gper         ,qfros_lake         ,&
         sm_roof            ,sm_gimp            ,sm_gper            ,sm_lake            ,&
         lake_icefrac       ,scv_lake           ,snowdp_lake        ,imeltl             ,&
         fioldl             ,w_old                                                      ,&
         forc_us            ,forc_vs                                                    ,&

! SNICAR model variables
         forc_aer                                                                       ,&
         mss_bcpho(lbsn:0)  ,mss_bcphi(lbsn:0)  ,mss_ocpho(lbsn:0)  ,mss_ocphi(lbsn:0)  ,&
         mss_dst1 (lbsn:0)  ,mss_dst2 (lbsn:0)  ,mss_dst3 (lbsn:0)  ,mss_dst4 (lbsn:0)  ,&
! END SNICAR model variables
!  irrigaiton
         qflx_irrig_drip    ,qflx_irrig_flood   ,qflx_irrig_paddy                       ,&
!  end irrigation
         ! output
         rsur               ,rnof               ,qinfl              ,zwt                ,&
         wdsrf              ,wa                 ,qcharge            ,smp                ,hk                 )

      ! roof
      !============================================================
      IF (snlr < 0) THEN
         ! Compaction rate for snow
         ! Natural compaction and metamorphosis. The compaction rate
         ! is recalculated for every new timestep
         lbr  = snlr + 1   ! lower bound of array
         CALL snowcompaction (lbr,deltim,&
                         imeltr(lbr:0),fioldr(lbr:0),t_roofsno(lbr:0),&
                         wliq_roofsno(lbr:0),wice_roofsno(lbr:0),forc_us,forc_vs,dz_roofsno(lbr:0))

         ! Combine thin snow elements
         lbr = maxsnl + 1
         CALL snowlayerscombine (lbr,snlr,&
                         z_roofsno(lbr:1),dz_roofsno(lbr:1),zi_roofsno(lbr-1:1),&
                         wliq_roofsno(lbr:1),wice_roofsno(lbr:1),t_roofsno(lbr:1),&
                         scv_roof,snowdp_roof)

         ! Divide thick snow elements
         IF (snlr < 0) &
         CALL snowlayersdivide (lbr,snlr,&
                         z_roofsno(lbr:0),dz_roofsno(lbr:0),zi_roofsno(lbr-1:0),&
                         wliq_roofsno(lbr:0),wice_roofsno(lbr:0),t_roofsno(lbr:0))
      ENDIF

      ! Set zero to the empty node
      IF (snlr > maxsnl) THEN
         wice_roofsno(maxsnl+1:snlr) = 0.
         wliq_roofsno(maxsnl+1:snlr) = 0.
         t_roofsno   (maxsnl+1:snlr) = 0.
         z_roofsno   (maxsnl+1:snlr) = 0.
         dz_roofsno  (maxsnl+1:snlr) = 0.
      ENDIF

      lbr = snlr + 1
      troof = t_roofsno(lbr)

      ! impervious ground
      !============================================================
      IF (snli < 0) THEN
         ! Compaction rate for snow
         ! Natural compaction and metamorphosis. The compaction rate
         ! is recalculated for every new timestep
         lbi  = snli + 1   ! lower bound of array
         CALL snowcompaction (lbi,deltim,&
                         imelti(lbi:0),fioldi(lbi:0),t_gimpsno(lbi:0),&
                         wliq_gimpsno(lbi:0),wice_gimpsno(lbi:0),forc_us,forc_vs,dz_gimpsno(lbi:0))

         ! Combine thin snow elements
         lbi = maxsnl + 1
         CALL snowlayerscombine (lbi,snli,&
                         z_gimpsno(lbi:1),dz_gimpsno(lbi:1),zi_gimpsno(lbi-1:1),&
                         wliq_gimpsno(lbi:1),wice_gimpsno(lbi:1),t_gimpsno(lbi:1),&
                         scv_gimp,snowdp_gimp)

         ! Divide thick snow elements
         IF (snli < 0) &
         CALL snowlayersdivide (lbi,snli,&
                         z_gimpsno(lbi:0),dz_gimpsno(lbi:0),zi_gimpsno(lbi-1:0),&
                         wliq_gimpsno(lbi:0),wice_gimpsno(lbi:0),t_gimpsno(lbi:0))
      ENDIF

      ! Set zero to the empty node
      IF (snli > maxsnl) THEN
         wice_gimpsno(maxsnl+1:snli) = 0.
         wliq_gimpsno(maxsnl+1:snli) = 0.
         t_gimpsno   (maxsnl+1:snli) = 0.
         z_gimpsno   (maxsnl+1:snli) = 0.
         dz_gimpsno  (maxsnl+1:snli) = 0.
      ENDIF

      lbi = snli + 1
      tgimp = t_gimpsno(lbi)

      ! pervious ground
      !============================================================
      IF (snlp < 0) THEN
         ! Compaction rate for snow
         ! Natural compaction and metamorphosis. The compaction rate
         ! is recalculated for every new timestep
         lbp  = snlp + 1   ! lower bound of array
         CALL snowcompaction (lbp,deltim,&
                         imeltp(lbp:0),fioldp(lbp:0),t_gpersno(lbp:0),&
                         wliq_gpersno(lbp:0),wice_gpersno(lbp:0),forc_us,forc_vs,dz_gpersno(lbp:0))

         ! Combine thin snow elements
         lbp = maxsnl + 1
         CALL snowlayerscombine (lbp,snlp,&
                         z_gpersno(lbp:1),dz_gpersno(lbp:1),zi_gpersno(lbp-1:1),&
                         wliq_gpersno(lbp:1),wice_gpersno(lbp:1),t_gpersno(lbp:1),&
                         scv_gper,snowdp_gper)

         ! Divide thick snow elements
         IF (snlp < 0) &
         CALL snowlayersdivide (lbp,snlp,&
                         z_gpersno(lbp:0),dz_gpersno(lbp:0),zi_gpersno(lbp-1:0),&
                         wliq_gpersno(lbp:0),wice_gpersno(lbp:0),t_gpersno(lbp:0))
      ENDIF

      ! Set zero to the empty node
      IF (snlp > maxsnl) THEN
         wice_gpersno(maxsnl+1:snlp) = 0.
         wliq_gpersno(maxsnl+1:snlp) = 0.
         t_gpersno   (maxsnl+1:snlp) = 0.
         z_gpersno   (maxsnl+1:snlp) = 0.
         dz_gpersno  (maxsnl+1:snlp) = 0.
      ENDIF

      lbp = snlp + 1
      tgper = t_gpersno(lbp)

      !TODO: temporal, set to t_soisno
      t_soisno(:) = t_gpersno(:)

      !TODO: how to set tlake
      lbl = snll + 1
      IF (lbl < 1) THEN
         tlake = t_lakesno(lbl)
      ELSE
         tlake = t_lake(1)
      ENDIF

      ! ----------------------------------------
      ! energy balance check
      ! ----------------------------------------
      zerr=errore
#if (defined CoLMDEBUG)
      IF(abs(errore)>.5)THEN
         write(6,*) 'Warning: energy balance violation ',errore,patchclass
      ENDIF
#endif

      ! ----------------------------------------
      ! water balance check
      ! ----------------------------------------

      wliq_soisno(: ) = 0.
      wliq_soisno(:1) = wliq_roofsno(:1)*froof
      wliq_soisno(: ) = wliq_soisno(: ) + wliq_gpersno(: )*(1-froof)*fgper
      wliq_soisno(:1) = wliq_soisno(:1) + wliq_gimpsno(:1)*(1-froof)*(1-fgper)
      !wliq_soisno(:) = wliq_soisno(:)*(1-flake) + wliq_lakesno(:)*flake

      wice_soisno(: ) = 0.
      wice_soisno(:1) = wice_roofsno(:1)*froof
      wice_soisno(: ) = wice_soisno(: ) + wice_gpersno(: )*(1-froof)*fgper
      wice_soisno(:1) = wice_soisno(:1) + wice_gimpsno(:1)*(1-froof)*(1-fgper)
      !wice_soisno(:) = wice_soisno(:)*(1-flake) + wice_lakesno(:)*flake

      scv = scv_roof*froof + scv_gper*(1-froof)*fgper + scv_gimp*(1-froof)*(1-fgper)
      !scv = scv*(1-flake) + scv_lake*flake

      endwb  = sum(wice_soisno(1:) + wliq_soisno(1:))
      endwb  = endwb + scv + ldew*fveg + wa*(1-froof)*fgper
      errorw = (endwb - totwb) - (forc_prc + forc_prl + urb_irrig - fevpa - rnof)*deltim
      xerr   = errorw/deltim

#if (defined CoLMDEBUG)
      IF(abs(errorw)>1.e-3) THEN
         write(6,*) 'Warning: water balance violation', errorw, ipatch, patchclass
         !STOP
      ENDIF
#endif

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

      ! fraction of snow cover.
      CALL snowfraction ( 0., 0.,z0m,zlnd,scv_lake,snowdp_lake,wt,sigf,fsno_lake)
      CALL snowfraction ( 0., 0.,z0m,zlnd,scv_roof,snowdp_roof,wt,sigf,fsno_roof)
      CALL snowfraction ( 0., 0.,z0m,zlnd,scv_gimp,snowdp_gimp,wt,sigf,fsno_gimp)
      CALL snowfraction (lai,sai,z0m,zlnd,scv_gper,snowdp_gper,wt,sigf,fsno_gper)
      lai = tlai(ipatch)
      sai = tsai(ipatch) * sigf

      ! update the snow age
      !TODO: can be moved to UrbanALBEDO.F90
      IF (snlr == 0) sag_roof = 0.
      CALL snowage (deltim,troof,scv_roof,scvold_roof,sag_roof)
      IF (snli == 0) sag_gimp = 0.
      CALL snowage (deltim,tgimp,scv_gimp,scvold_gimp,sag_gimp)
      IF (snlp == 0) sag_gper = 0.
      CALL snowage (deltim,tgper,scv_gper,scvold_gper,sag_gper)
      IF (snll == 0) sag_lake = 0.
      CALL snowage (deltim,tlake,scv_lake,scvold_lake,sag_lake)

      ! update snow depth, snow cover and snow age
      snowdp = snowdp_roof*froof + snowdp_gper*(1-froof)*fgper + snowdp_gimp*(1-froof)*(1-fgper)
      fsno   =   fsno_roof*froof +   fsno_gper*(1-froof)*fgper +   fsno_gimp*(1-froof)*(1-fgper)
      sag    =    sag_roof*froof +    sag_gper*(1-froof)*fgper +    sag_gimp*(1-froof)*(1-fgper)

      ! albedos
      ! we supposed call it every time-step, because
      ! other vegetation related parameters are needed to create

      CALL alburban (ipatch,froof,fgper,flake,hlr,hroof,&
                     alb_roof,alb_wall,alb_gimp,alb_gper,&
                     rho,tau,fveg,(htop+hbot)/2.,lai,sai,fwet_snow,coszen,fwsun,tlake,&
                     fsno_roof,fsno_gimp,fsno_gper,fsno_lake,&
                     scv_roof,scv_gimp,scv_gper,scv_lake,&
                     sag_roof,sag_gimp,sag_gper,sag_lake,&
                     dfwsun,extkd,alb,ssun,ssha,sroof,swsun,swsha,sgimp,sgper,slake)

      ! zero-filling set for glacier/ice-sheet/land water bodies/ocean components
      laisun = lai
      laisha = 0.0
      green  = 1.

      h2osoi = wliq_soisno(1:)/(dz_soi(1:)*denh2o) + wice_soisno(1:)/(dz_soi(1:)*denice)
      wat = sum(wice_soisno(1:)+wliq_soisno(1:))
      wat = wat + scv + ldew*fveg + wa*(1-froof)*fgper

      z_sno_roof (maxsnl+1:0) = z_roofsno (maxsnl+1:0)
      z_sno_gimp (maxsnl+1:0) = z_gimpsno (maxsnl+1:0)
      z_sno_gper (maxsnl+1:0) = z_gpersno (maxsnl+1:0)
      z_sno_lake (maxsnl+1:0) = z_lakesno (maxsnl+1:0)

      dz_sno_roof(maxsnl+1:0) = dz_roofsno(maxsnl+1:0)
      dz_sno_gimp(maxsnl+1:0) = dz_gimpsno(maxsnl+1:0)
      dz_sno_gper(maxsnl+1:0) = dz_gpersno(maxsnl+1:0)
      dz_sno_lake(maxsnl+1:0) = dz_lakesno(maxsnl+1:0)

      z_sno(:) = z_sno_roof(:)*froof
      z_sno(:) = z_sno(:) + z_sno_gper(:)*(1-froof)*fgper
      z_sno(:) = z_sno(:) + z_sno_gimp(:)*(1-froof)*(1-fgper)
      z_sno(:) = z_sno(:)*(1-flake) + z_sno_lake(:)*flake

      dz_sno(:) = dz_sno_roof(:)*froof
      dz_sno(:) = dz_sno(:) + dz_sno_gper(:)*(1-froof)*fgper
      dz_sno(:) = dz_sno(:) + dz_sno_gimp(:)*(1-froof)*(1-fgper)
      dz_sno(:) = dz_sno(:)*(1-flake) + dz_sno_lake(:)*flake

! diagnostic diurnal temperature
      !IF (tref > tmax) tmax = tref
      !IF (tref < tmin) tmin = tref

! 06/05/2022, yuan: RH for output to compare
      CALL qsadv(tref,forc_psrf,ei,deiDT,qsatl,qsatlDT)
      qref = qref/qsatl

END SUBROUTINE CoLMMAIN_Urban
! ----------------------------------------------------------------------
! EOP
