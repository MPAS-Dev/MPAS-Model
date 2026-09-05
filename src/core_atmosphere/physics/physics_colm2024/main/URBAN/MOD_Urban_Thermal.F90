#include <define.h>

MODULE MOD_Urban_Thermal

!-----------------------------------------------------------------------
! !DESCRIPTION:
!  This is the main subroutine to execute the calculation of urban
!  thermal processes and surface fluxes
!
!  Created by Hua Yuan, 09/2021
!-----------------------------------------------------------------------

   USE MOD_Precision
   IMPLICIT NONE
   SAVE
   PRIVATE

   PUBLIC :: UrbanTHERMAL

CONTAINS


   SUBROUTINE UrbanTHERMAL ( &

        ! model running information
        ipatch         ,patchtype      ,lbr            ,lbi            ,&
        lbp            ,lbl            ,deltim         ,patchlatr      ,&
        ! forcing
        forc_hgt_u     ,forc_hgt_t     ,forc_hgt_q     ,forc_us        ,&
        forc_vs        ,forc_t         ,forc_q         ,forc_psrf      ,&
        forc_rhoair    ,forc_frl       ,forc_po2m      ,forc_pco2m     ,&
        forc_sols      ,forc_soll      ,forc_solsd     ,forc_solld     ,&
        theta          ,sabroof        ,sabwsun        ,sabwsha        ,&
        sabgimp        ,sabgper        ,sablake        ,sabv           ,&
        par            ,Fhac           ,Fwst           ,Fach           ,&
        Fahe           ,Fhah           ,vehc           ,meta           ,&
        ! LUCY model input parameters
        fix_holiday    ,week_holiday   ,hum_prof       ,pop_den        ,&
        vehicle        ,weh_prof       ,wdh_prof       ,idate          ,&
        patchlonr                                                      ,&
        ! surface parameters
        froof          ,flake          ,hroof          ,hlr            ,&
        fgper          ,pondmx         ,eroof          ,ewall          ,&
        egimp          ,egper          ,trsmx0         ,zlnd           ,&
        zsno           ,capr           ,cnfac          ,vf_quartz      ,&
        vf_gravels     ,vf_om          ,vf_sand        ,wf_gravels     ,&
        wf_sand        ,csol           ,porsl          ,psi0           ,&
#ifdef Campbell_SOIL_MODEL
        bsw                                                            ,&
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
        theta_r        ,alpha_vgm      ,n_vgm          ,L_vgm          ,&
        sc_vgm         ,fc_vgm                                         ,&
#endif
        k_solids       ,dksatu         ,dksatf         ,dkdry          ,&
        BA_alpha       ,BA_beta                                        ,&
        cv_roof        ,cv_wall        ,cv_gimp                        ,&
        tk_roof        ,tk_wall        ,tk_gimp        ,dz_roofsno     ,&
        dz_gimpsno     ,dz_gpersno     ,dz_lakesno     ,dz_wall        ,&
        z_roofsno      ,z_gimpsno      ,z_gpersno      ,z_lakesno      ,&
        z_wall         ,zi_roofsno     ,zi_gimpsno     ,zi_gpersno     ,&
        zi_lakesno     ,zi_wall        ,dz_lake        ,lakedepth      ,&
        dewmx          ,sqrtdi         ,rootfr         ,effcon         ,&
        vmax25         ,c3c4           ,slti           ,hlti           ,shti,&
        hhti           ,trda           ,trdm           ,trop           ,&
        g1             ,g0             ,gradm          ,binter         ,&
        extkn          ,lambda                                         ,&

        ! surface status
        fsno_roof      ,fsno_gimp      ,fsno_gper      ,scv_roof       ,&
        scv_gimp       ,scv_gper       ,scv_lake       ,snowdp_roof    ,&
        snowdp_gimp    ,snowdp_gper    ,snowdp_lake    ,fwsun          ,&
        dfwsun         ,lai            ,sai            ,htop           ,&
        hbot           ,fveg           ,sigf           ,extkd          ,&
        lwsun          ,lwsha          ,lgimp          ,lgper          ,&
        t_grnd         ,t_roofsno      ,t_wallsun      ,t_wallsha      ,&
        t_gimpsno      ,t_gpersno      ,t_lakesno      ,wliq_roofsno   ,&
        wliq_gimpsno   ,wliq_gpersno   ,wliq_lakesno   ,wice_roofsno   ,&
        wice_gimpsno   ,wice_gpersno   ,wice_lakesno   ,t_lake         ,&
        lake_icefrac   ,savedtke1      ,lveg           ,tleaf          ,&
        ldew           ,ldew_rain      ,ldew_snow      ,fwet_snow      ,&
        troom          ,troof_inner    ,twsun_inner    ,twsha_inner    ,&
        troommax       ,troommin       ,tafu                           ,&

! SNICAR model variables
        snofrz         ,sabg_lyr                                       ,&
! END SNICAR model variables

        ! output
        taux           ,tauy           ,fsena          ,fevpa          ,&
        lfevpa         ,fsenl          ,fevpl          ,etr            ,&
        fseng          ,fevpg          ,olrg           ,fgrnd          ,&
        fsen_roof      ,fsen_wsun      ,fsen_wsha      ,fsen_gimp      ,&
        fsen_gper      ,fsen_urbl      ,troof          ,twall          ,&
        lfevp_roof     ,lfevp_gimp     ,lfevp_gper     ,lfevp_urbl     ,&
        qseva_roof     ,qseva_gimp     ,qseva_gper     ,qseva_lake     ,&
        qsdew_roof     ,qsdew_gimp     ,qsdew_gper     ,qsdew_lake     ,&
        qsubl_roof     ,qsubl_gimp     ,qsubl_gper     ,qsubl_lake     ,&
        qfros_roof     ,qfros_gimp     ,qfros_gper     ,qfros_lake     ,&
        imelt_roof     ,imelt_gimp     ,imelt_gper     ,imelt_lake     ,&
        sm_roof        ,sm_gimp        ,sm_gper        ,sm_lake        ,&
        sabg           ,rstfac         ,rootr          ,etr_deficit    ,&
        tref           ,qref           ,trad           ,rst            ,&
        assim          ,respc          ,errore         ,emis           ,&
        z0m            ,zol            ,rib            ,ustar          ,&
        qstar          ,tstar          ,fm             ,fh             ,&
        fq             ,hpbl                                            )


   USE MOD_Precision
   USE MOD_MPAS_MPI
   USE MOD_Vars_Global
   USE MOD_Const_Physical, only: denh2o,roverg,hvap,hsub,rgas,cpair,&
                                 stefnc,denice,tfrz,vonkar,grav
   USE MOD_Urban_Shortwave
   USE MOD_Urban_Longwave
   USE MOD_Urban_GroundFlux
   USE MOD_Urban_Flux
   USE MOD_Urban_RoofTemperature
   USE MOD_Urban_WallTemperature
   USE MOD_Urban_PerviousTemperature
   USE MOD_Urban_ImperviousTemperature
   USE MOD_Lake
   USE MOD_Urban_BEM
   USE MOD_Urban_LUCY, only: LUCY
   USE MOD_Eroot, only: eroot
#ifdef vanGenuchten_Mualem_SOIL_MODEL
   USE MOD_Hydro_SoilFunction, only: soil_psi_from_vliq
#endif

   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   integer,  intent(in) :: &
        idate(3)   ,&
        ipatch                         ,&! patch index
        patchtype                      ,&! land patch type (0=soil, 1=urban or built-up, 2=wetland,
                                         ! 3=glacier/ice sheet, 4=land water bodies)
        lbr                            ,&! lower bound of array
        lbi                            ,&! lower bound of array
        lbp                            ,&! lower bound of array
        lbl                              ! lower bound of array

   real(r8), intent(in) :: &
        deltim                         ,&! seconds in a time step [second]
        patchlatr                        ! latitude in radians

   real(r8), intent(in) :: &
        patchlonr                      ,&! longitude of patch [radian]
        fix_holiday(365)               ,&! Fixed public holidays, holiday(0) or workday(1)
        week_holiday(7)                ,&! week holidays
        hum_prof(24)                   ,&! Diurnal metabolic heat profile
        weh_prof(24)                   ,&! Diurnal traffic flow profile of weekend
        wdh_prof(24)                   ,&! Diurnal traffic flow profile of weekday
        pop_den                        ,&! population density
        vehicle(3)                       ! vehicle numbers per thousand people

   real(r8), intent(in) :: &
        ! atmospherical variables and observational height
        forc_hgt_u                     ,&! observational height of wind [m]
        forc_hgt_t                     ,&! observational height of temperature [m]
        forc_hgt_q                     ,&! observational height of humidity [m]
        forc_us                        ,&! wind component in eastward direction [m/s]
        forc_vs                        ,&! wind component in northward direction [m/s]
        forc_t                         ,&! temperature at agcm reference height [kelvin]
        forc_q                         ,&! specific humidity at agcm reference height [kg/kg]
        forc_psrf                      ,&! atmosphere pressure at the surface [pa]
        forc_rhoair                    ,&! density air [kg/m3]
        forc_frl                       ,&! atmospheric infrared (longwave) radiation [W/m2]
        forc_po2m                      ,&! O2 concentration in atmos. (pascals)
        forc_pco2m                     ,&! CO2 concentration in atmos. (pascals)
        forc_sols                      ,&! atm vis direct beam solar rad onto srf [W/m2]
        forc_soll                      ,&! atm nir direct beam solar rad onto srf [W/m2]
        forc_solsd                     ,&! atm vis diffuse solar rad onto srf [W/m2]
        forc_solld                     ,&! atm nir diffuse solar rad onto srf [W/m2]
        theta                          ,&! sun zenith angle
        par                            ,&! vegetation PAR
        sabv                           ,&! absorbed shortwave radiation by vegetation [W/m2]
        sabroof                        ,&! absorbed shortwave radiation by roof [W/m2]
        sabwsun                        ,&! absorbed shortwave radiation by sunlit wall [W/m2]
        sabwsha                        ,&! absorbed shortwave radiation by shaded wall [W/m2]
        sabgimp                        ,&! absorbed shortwave radiation by impervious road [W/m2]
        sabgper                        ,&! absorbed shortwave radiation by ground snow [W/m2]
        sablake                          ! absorbed shortwave radiation by lake [W/m2]

   real(r8), intent(in) :: &
        froof                          ,&! roof fractional cover [-]
        flake                          ,&! urban lake fractional cover [-]
        hroof                          ,&! average building height [m]
        hlr                            ,&! average building height to their side length [-]
        fgper                          ,&! impervious road fractional cover [-]
        pondmx                         ,&! maximum ponding for soil [mm]
        eroof                          ,&! emissivity of roof
        ewall                          ,&! emissivity of walls
        egimp                          ,&! emissivity of impervious road
        egper                          ,&! emissivity of soil

        trsmx0                         ,&! max transpiration for moist soil+100% veg.  [mm/s]
        zlnd                           ,&! roughness length for soil [m]
        zsno                           ,&! roughness length for snow [m]
        capr                           ,&! tuning factor to turn first layer T into surface T
        cnfac                          ,&! Crank Nicholson factor between 0 and 1

        ! soil physical parameters
        vf_quartz (1:nl_soil)          ,&! volumetric fraction of quartz within mineral soil
        vf_gravels(1:nl_soil)          ,&! volumetric fraction of gravels
        vf_om     (1:nl_soil)          ,&! volumetric fraction of organic matter
        vf_sand   (1:nl_soil)          ,&! volumetric fraction of sand
        wf_gravels(1:nl_soil)          ,&! gravimetric fraction of gravels
        wf_sand   (1:nl_soil)          ,&! gravimetric fraction of sand
        csol      (1:nl_soil)          ,&! heat capacity of soil solids [J/(m3 K)]
        porsl     (1:nl_soil)          ,&! soil porosity [-]
        psi0      (1:nl_soil)          ,&! soil water suction, negative potential [mm]
#ifdef Campbell_SOIL_MODEL
        bsw       (1:nl_soil)          ,&! clapp and hornberger "b" parameter [-]
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
        theta_r   (1:nl_soil)          ,&! residual water content (cm3/cm3)

        alpha_vgm (1:nl_soil)  ,&! parameter correspond approximately to inverse of air-entry value
        n_vgm     (1:nl_soil)  ,&! a shape parameter
        L_vgm     (1:nl_soil)  ,&! pore-connectivity parameter
        sc_vgm    (1:nl_soil)  ,&! saturation at air entry value in classical vanGenuchten model [-]
        fc_vgm    (1:nl_soil)  ,&! a scaling factor by using air entry value in the Mualem model [-]
#endif
        k_solids  (1:nl_soil)          ,&! thermal conductivity of minerals soil [W/m-K]
        dkdry     (1:nl_soil)          ,&! thermal conductivity of dry soil [W/m-K]
        dksatu    (1:nl_soil)          ,&! thermal conductivity of saturated unfrozen soil [W/m-K]
        dksatf    (1:nl_soil)          ,&! thermal conductivity of saturated frozen soil [W/m-K]

        BA_alpha  (1:nl_soil)          ,&! alpha in Balland and Arp(2005) thermal cond. scheme
        BA_beta   (1:nl_soil)          ,&! beta in Balland and Arp(2005) thermal cond. scheme
        cv_roof   (1:nl_roof)          ,&! heat capacity of roof [J/(m2 K)]
        cv_wall   (1:nl_wall)          ,&! heat capacity of wall [J/(m2 K)]
        cv_gimp   (1:nl_soil)          ,&! heat capacity of impervious [J/(m2 K)]
        tk_roof   (1:nl_roof)          ,&! thermal conductivity of roof [W/m-K]
        tk_wall   (1:nl_wall)          ,&! thermal conductivity of wall [W/m-K]
        tk_gimp   (1:nl_soil)          ,&! thermal conductivity of impervious [W/m-K]

        dz_roofsno(lbr  :nl_roof)      ,&! layer thickness [m]
        dz_gimpsno(lbi  :nl_soil)      ,&! layer thickness [m]
        dz_gpersno(lbp  :nl_soil)      ,&! layer thickness [m]
        dz_wall   (    1:nl_wall)      ,&! layer thickness [m]
        z_roofsno (lbr  :nl_roof)      ,&! node depth [m]
        z_gimpsno (lbi  :nl_soil)      ,&! node depth [m]
        z_gpersno (lbp  :nl_soil)      ,&! node depth [m]
        z_wall    (    1:nl_wall)      ,&! node depth [m]
        zi_roofsno(lbr-1:nl_roof)      ,&! interface depth [m]
        zi_gimpsno(lbi-1:nl_soil)      ,&! interface depth [m]
        zi_gpersno(lbp-1:nl_soil)      ,&! interface depth [m]
        zi_wall   (    0:nl_wall)      ,&! interface depth [m]
        dz_lake   (    1:nl_lake)      ,&! lake layer thickness (m)
        lakedepth                      ,&! lake depth (m)

        z_lakesno (maxsnl+1:nl_soil)   ,&! node depth [m]
        dz_lakesno(maxsnl+1:nl_soil)   ,&! layer thickness [m]
        zi_lakesno(maxsnl  :nl_soil)   ,&! interface depth [m]

        ! vegetation parameters
        dewmx                          ,&! maximum dew
        sqrtdi                         ,&! inverse sqrt of leaf dimension [m**-0.5]
        rootfr    (1:nl_soil)          ,&! root fraction

        effcon              ,&! quantum efficiency of RuBP regeneration (mol CO2/mol quanta)
        vmax25              ,&! maximum carboxylation rate at 25 C at canopy top
        slti                ,&! slope of low temperature inhibition function      [s3]
        hlti                ,&! 1/2 point of low temperature inhibition function  [s4]
        shti                ,&! slope of high temperature inhibition function     [s1]
        hhti                ,&! 1/2 point of high temperature inhibition function [s2]
        trda                ,&! temperature coefficient in gs-a model             [s5]
        trdm                ,&! temperature coefficient in gs-a model             [s6]
        trop                ,&! temperature coefficient in gs-a model
        g1                  ,&! conductance-photosynthesis slope parameter for medlyn model
        g0                  ,&! conductance-photosynthesis intercept for medlyn model
        gradm               ,&! conductance-photosynthesis slope parameter
        binter              ,&! conductance-photosynthesis intercept
        lambda              ,&! marginal water cost of carbon gain
        extkn                ! coefficient of leaf nitrogen allocation

   integer , intent(in) :: &
        c3c4                              ! 1 for C3, 0 for C4

   real(r8), intent(in) :: &
        fsno_roof                      ,&! fraction of ground covered by snow
        fsno_gimp                      ,&! fraction of ground covered by snow
        fsno_gper                      ,&! fraction of ground covered by snow
        dfwsun                         ,&! change of fwsun [%]
        lai                            ,&! adjusted leaf area index for seasonal variation [-]
        sai                            ,&! stem area index  [-]
        htop                           ,&! canopy crown top height [m]
        hbot                           ,&! canopy crown bottom height [m]
        fveg                           ,&! fraction of veg cover
        sigf                           ,&! fraction of veg cover, excluding snow-covered veg [-]
        extkd                            ! diffuse and scattered diffuse PAR extinction coefficient

   real(r8), intent(in) :: hpbl          ! atmospheric boundary layer height [m]

   real(r8), intent(inout) :: &
        fwsun                          ,&! fraction of sunlit wall [-]
        lwsun                          ,&! net longwave radiation of sunlit wall
        lwsha                          ,&! net longwave radiation of shaded wall
        lgimp                          ,&! net longwave radiation of impervious road
        lgper                          ,&! net longwave radiation of pervious road
        t_grnd                         ,&! ground temperature
        t_roofsno   (     lbr:nl_wall) ,&! temperatures of roof layers
        t_wallsun   (         nl_wall) ,&! temperatures of roof layers
        t_wallsha   (         nl_wall) ,&! temperatures of roof layers
        t_gimpsno   (     lbi:nl_soil) ,&! temperatures of roof layers
        t_gpersno   (     lbp:nl_soil) ,&! temperatures of roof layers
        wliq_roofsno(     lbr:nl_roof) ,&! liquid water [kg/m2]
        wliq_gimpsno(     lbi:nl_soil) ,&! liquid water [kg/m2]
        wliq_gpersno(     lbp:nl_soil) ,&! liquid water [kg/m2]
        wice_roofsno(     lbr:nl_roof) ,&! ice lens [kg/m2]
        wice_gimpsno(     lbi:nl_soil) ,&! ice lens [kg/m2]
        wice_gpersno(     lbp:nl_soil) ,&! ice lens [kg/m2]
        t_lake      (         nl_lake) ,&! lake temperature [K]
        lake_icefrac(         nl_lake) ,&! lake mass fraction of lake layer that is frozen
        t_lakesno   (maxsnl+1:nl_soil) ,&! temperatures of roof layers
        wliq_lakesno(maxsnl+1:nl_soil) ,&! liquid water [kg/m2]
        wice_lakesno(maxsnl+1:nl_soil) ,&! ice lens [kg/m2]
        savedtke1                      ,&! top level eddy conductivity (W/m K)
        scv_roof                       ,&! snow cover, water equivalent [mm, kg/m2]
        scv_gimp                       ,&! snow cover, water equivalent [mm, kg/m2]
        scv_gper                       ,&! snow cover, water equivalent [mm, kg/m2]
        scv_lake                       ,&! snow cover, water equivalent [mm, kg/m2]
        snowdp_roof                    ,&! snow depth [m]
        snowdp_gimp                    ,&! snow depth [m]
        snowdp_gper                    ,&! snow depth [m]
        snowdp_lake                    ,&! snow depth [m]
        lveg                           ,&! net longwave radiation of vegetation [W/m2]
        tleaf                          ,&! leaf temperature [K]
        ldew                           ,&! depth of water on foliage [kg/(m2 s)]
        ldew_rain                      ,&! depth of rain on foliage [kg/(m2 s)]
        ldew_snow                      ,&! depth of rain on foliage [kg/(m2 s)]
        fwet_snow                      ,&! vegetation canopy snow fractional cover [-]
        troom                          ,&! temperature of inner building
        troof_inner                    ,&! temperature of inner roof
        twsun_inner                    ,&! temperature of inner sunlit wall
        twsha_inner                    ,&! temperature of inner shaded wall
        troommax                       ,&! maximum temperature of inner building
        troommin                       ,&! minimum temperature of inner building
        tafu                           ,&! temperature of outer building
        Fahe                           ,&! flux from metabolic and vehicle
        Fhah                           ,&! flux from heating
        Fhac                           ,&! flux from heat or cool AC
        Fwst                           ,&! waste heat from cool or heat
        Fach                           ,&! flux from air exchange
        vehc                           ,&! flux from vehicle
        meta                             ! flux from metabolic

   real(r8), intent(out) :: &
        taux                           ,&! wind stress: E-W [kg/m/s**2]
        tauy                           ,&! wind stress: N-S [kg/m/s**2]
        fsena                          ,&! sensible heat from canopy height to atm [W/m2]
        fevpa                          ,&! evapotranspiration from canopy height to atm [mm/s]
        lfevpa                         ,&! latent heat flux from canopy height to atm [W/m2]
        fsenl                          ,&! sensible heat from leaves [W/m2]
        fevpl                          ,&! evaporation+transpiration from leaves [mm/s]
        etr                            ,&! transpiration rate [mm/s]
        fseng                          ,&! sensible heat flux from ground [W/m2]
        fevpg                          ,&! evaporation heat flux from ground [mm/s]
        olrg                           ,&! outgoing long-wave radiation from ground+canopy
        fgrnd                          ,&! ground heat flux [W/m2]

        fsen_roof                      ,&! sensible heat from roof [W/m2]
        fsen_wsun                      ,&! sensible heat from sunlit wall [W/m2]
        fsen_wsha                      ,&! sensible heat from shaded wall [W/m2]
        fsen_gimp                      ,&! sensible heat from impervious road [W/m2]
        fsen_gper                      ,&! sensible heat from pervious road [W/m2]
        fsen_urbl                      ,&! sensible heat from urban vegetation [W/m2]

        lfevp_roof                     ,&! latent heat flux from roof [W/m2]
        lfevp_gimp                     ,&! latent heat flux from impervious road [W/m2]
        lfevp_gper                     ,&! latent heat flux from pervious road [W/m2]
        lfevp_urbl                     ,&! latent heat flux from urban vegetation [W/m2]

        troof                          ,&! temperature of roof [K]
        twall                          ,&! temperature of wall [K]

        qseva_roof                     ,&! ground soil surface evaporation rate (mm h2o/s)
        qseva_gimp                     ,&! ground soil surface evaporation rate (mm h2o/s)
        qseva_gper                     ,&! ground soil surface evaporation rate (mm h2o/s)
        qseva_lake                     ,&! ground soil surface evaporation rate (mm h2o/s)
        qsdew_roof                     ,&! ground soil surface dew formation (mm h2o /s) [+]
        qsdew_gimp                     ,&! ground soil surface dew formation (mm h2o /s) [+]
        qsdew_gper                     ,&! ground soil surface dew formation (mm h2o /s) [+]
        qsdew_lake                     ,&! ground soil surface dew formation (mm h2o /s) [+]
        qsubl_roof                     ,&! sublimation rate from soil ice pack (mm h2o /s) [+]
        qsubl_gimp                     ,&! sublimation rate from soil ice pack (mm h2o /s) [+]
        qsubl_gper                     ,&! sublimation rate from soil ice pack (mm h2o /s) [+]
        qsubl_lake                     ,&! sublimation rate from soil ice pack (mm h2o /s) [+]
        qfros_roof                     ,&! surface dew added to snow pack (mm h2o /s) [+]
        qfros_gimp                     ,&! surface dew added to snow pack (mm h2o /s) [+]
        qfros_gper                     ,&! surface dew added to snow pack (mm h2o /s) [+]
        qfros_lake                       ! surface dew added to snow pack (mm h2o /s) [+]

   integer, intent(out) :: &
        imelt_roof(lbr:nl_roof)        ,&! flag for melting or freezing [-]
        imelt_gimp(lbi:nl_soil)        ,&! flag for melting or freezing [-]
        imelt_gper(lbp:nl_soil)        ,&! flag for melting or freezing [-]
        imelt_lake(maxsnl+1:nl_soil)     ! flag for melting or freezing [-]

   real(r8), intent(out) :: &
        sm_roof                        ,&! rate of snowmelt [kg/(m2 s)]
        sm_gimp                        ,&! rate of snowmelt [kg/(m2 s)]
        sm_gper                        ,&! rate of snowmelt [kg/(m2 s)]
        sm_lake                        ,&! rate of snowmelt [kg/(m2 s)]
        sabg                           ,&! overall ground solar radiation absorption (+wall)
        rstfac                         ,&! factor of soil water stress
        rootr(1:nl_soil)               ,&! root resistance of a layer, all layers add to 1
        etr_deficit                    ,&! urban irrigation [mm/s]
        tref                           ,&! 2 m height air temperature [kelvin]
        qref                           ,&! 2 m height air specific humidity
        trad                           ,&! radiative temperature [K]
        rst                            ,&! stomatal resistance (s m-1)
        assim                          ,&! assimilation
        respc                          ,&! respiration
        errore                         ,&! energy balance error [w/m2]

        ! additional variables required by coupling with WRF or RSM model
        emis                           ,&! averaged bulk surface emissivity
        z0m                            ,&! effective roughness [m]
        zol                            ,&! dimensionless height (z/L) used in Monin-Obukhov theory
        rib                            ,&! bulk Richardson number in surface layer
        ustar                          ,&! u* in similarity theory [m/s]
        qstar                          ,&! q* in similarity theory [kg/kg]
        tstar                          ,&! t* in similarity theory [K]
        fm                             ,&! integral of profile function for momentum
        fh                             ,&! integral of profile function for heat
        fq                               ! integral of profile function for moisture

! SNICAR model variables
   real(r8), intent(in)  :: sabg_lyr(lbp:1) !snow layer absorption
   real(r8), intent(out) :: snofrz  (lbp:0) !snow freezing rate (col,lyr) [kg m-2 s-1]
! END SNICAR model variables

!-------------------------- Local Variables ----------------------------

   integer :: nurb           ! number of aboveground urban components [-]

   logical :: doveg          ! run model with vegetation

   real(r8) :: &
        fg                 ,&! ground fraction ( impervious + soil + snow )
        fsenroof           ,&! sensible heat flux from roof [W/m2]
        fsenwsun           ,&! sensible heat flux from sunlit wall [W/m2]
        fsenwsha           ,&! sensible heat flux from shaded wall [W/m2]
        fsengimp           ,&! sensible heat flux from impervious road [W/m2]
        fsengper           ,&! sensible heat flux from ground soil [W/m2]
        fevproof           ,&! evaporation heat flux from roof [mm/s]
        fevpgimp           ,&! evaporation heat flux from impervious road [mm/s]
        fevpgper           ,&! evaporation heat flux from ground soil [mm/s]

        croofs             ,&! deriv of roof sensible heat flux wrt soil temp [w/m**2/k]
        cwsuns             ,&! deriv of sunlit wall sensible heat flux wrt soil temp [w/m**2/k]
        cwshas             ,&! deriv of shaded wall sensible heat flux wrt soil temp [w/m**2/k]
        cgrnds             ,&! deriv of ground latent heat flux wrt soil temp [w/m**2/k]
        croofl             ,&! deriv of roof latent heat flux wrt soil temp [w/m**2/k]
        cgimpl             ,&! deriv of impervious latent heat flux wrt soil temp [w/m**2/k]
        cgperl             ,&! deriv of pervious latent heat flux wrt soil temp [w/m**2/k]
        croof              ,&! deriv of roof total flux wrt soil temp [w/m**2/k]
        cgimp              ,&! deriv of impervious total heat flux wrt soil temp [w/m**2/k]
        cgper              ,&! deriv of pervious total heat flux wrt soil temp [w/m**2/k]

        dqroofdT           ,&! d(qroof)/dT
        dqgimpdT           ,&! d(qgimp)/dT
        dqgperdT           ,&! d(qgper)/dT

        degdT              ,&! d(eg)/dT
        eg                 ,&! water vapor pressure at temperature T [pa]
        egsmax             ,&! max. evaporation which soil can provide at one time step
        egidif             ,&! the excess of evaporation over "egsmax"
        emg                ,&! ground emissivity (0.97 for snow,
                             ! glaciers and water surface; 0.96 for soil and wetland)
        etrc               ,&! maximum possible transpiration rate [mm/s]
        fac                ,&! soil wetness of surface layer
        factr(lbr:nl_roof) ,&! used in computing tridiagonal matrix
        facti(lbi:nl_soil) ,&! used in computing tridiagonal matrix
        factp(lbp:nl_soil) ,&! used in computing tridiagonal matrix
        hr                 ,&! relative humidity
        htvp_roof          ,&! latent heat of vapor of water (or sublimation) [J/Kg]
        htvp_gimp          ,&! latent heat of vapor of water (or sublimation) [J/Kg]
        htvp_gper          ,&! latent heat of vapor of water (or sublimation) [J/Kg]
        olru               ,&! olrg excluding downwelling reflection [W/m2]
        olrb               ,&! olrg assuming black body emission [W/m2]
        psit               ,&! negative potential of soil

        rss                ,&! soil resistance
        qroof              ,&! roof specific humidity [kg/kg]
        qgimp              ,&! ground impervious road specific humidity [kg/kg]
        qgper              ,&! ground pervious specific humidity [kg/kg]
        qsatg              ,&! saturated humidity [kg/kg]
        qsatgdT            ,&! d(qsatg)/dT
        qred               ,&! soil surface relative humidity
        thm                ,&! intermediate variable (forc_t+0.0098*forc_hgt_t)
        th                 ,&! potential temperature (kelvin)
        thv                ,&! virtual potential temperature (kelvin)

        twsun              ,&! temperature of sunlit wall
        twsha              ,&! temperature of shaded wall
        tgimp              ,&! temperature of impervious road
        tgper              ,&! ground soil temperature
        tlake              ,&! lake surface temperature
        troof_bef          ,&! temperature of roof
        twsun_bef          ,&! temperature of sunlit wall
        twsha_bef          ,&! temperature of shaded wall
        tgimp_bef          ,&! temperature of impervious road
        tgper_bef          ,&! ground soil temperature
        troof_nl_bef       ,&! temperature of roof
        twsun_nl_bef       ,&! temperature of sunlit wall
        twsha_nl_bef       ,&! temperature of shaded wall
        tkdz_roof          ,&! heat flux from room to roof
        tkdz_wsun          ,&! heat flux from room to sunlit wall
        tkdz_wsha          ,&! heat flux from room to shaded wall
        tinc               ,&! temperature difference of two time step
        ev                 ,&! emissivity of vegetation [-]
        lroof              ,&! net longwave radiation of roof
        rout               ,&! out-going longwave radiation from roof
        lout               ,&! out-going longwave radiation
        lnet               ,&! overall net longwave radiation
        dlw                ,&! change of net longwave radiation
        dlwbef             ,&! change of net longwave radiation
        dlwsun             ,&! change of net longwave radiation of sunlit wall
        dlwsha             ,&! change of net longwave radiation of shaded wall
        dlgimp             ,&! change of net longwave radiation of impervious road
        dlgper             ,&! change of net longwave radiation of pervious road
        dlveg              ,&! change of net longwave radiation of vegetation [W/m2]
        dlout              ,&! change of out-going radiation due to temp change
        clroof             ,&! deriv of lroof wrt roof temp [w/m**2/k]
        clwsun             ,&! deriv of lwsun wrt wsun temp [w/m**2/k]
        clwsha             ,&! deriv of lwsha wrt wsha temp [w/m**2/k]
        clgimp             ,&! deriv of lgimp wrt gimp temp [w/m**2/k]
        clgper             ,&! deriv of lgper wrt soil temp [w/m**2/k]
        fwsha              ,&! fraction of shaded wall [-]
        ur                 ,&! wind speed at reference height [m/s]
        wx                 ,&! partial volume of ice and water of surface layer
        xmf                  ! total latent heat of phase change of ground water

   real(r8) :: &
        taux_lake          ,&! wind stress: E-W [kg/m/s**2]
        tauy_lake          ,&! wind stress: N-S [kg/m/s**2]
        fsena_lake         ,&! sensible heat from canopy height to atmosphere [W/m2]
        fevpa_lake         ,&! evapotranspiration from canopy height to atmosphere [mm/s]
        lfevpa_lake        ,&! latent heat flux from canopy height to atmosphere [W/m2]
        fseng_lake         ,&! sensible heat flux from ground [W/m2]
        fevpg_lake         ,&! evaporation heat flux from ground [mm/s]
        olrg_lake          ,&! outgoing long-wave radiation from ground+canopy
        fgrnd_lake         ,&! ground heat flux [W/m2]
        tref_lake          ,&! 2 m height air temperature [kelvin]
        qref_lake          ,&! 2 m height air specific humidity
        trad_lake          ,&! radiative temperature [K]
        lnet_lake          ,&! net longwave radiation
        emis_lake          ,&! averaged bulk surface emissivity
        z0m_lake           ,&! effective roughness [m]
        zol_lake           ,&! dimensionless height (z/L) used in Monin-Obukhov theory
        rib_lake           ,&! bulk Richardson number in surface layer
        ustar_lake         ,&! u* in similarity theory [m/s]
        qstar_lake         ,&! q* in similarity theory [kg/kg]
        tstar_lake         ,&! t* in similarity theory [K]
        fm_lake            ,&! integral of profile function for momentum
        fh_lake            ,&! integral of profile function for heat
        fq_lake            ,&! integral of profile function for moisture
        fh2m_lake          ,&! relation for temperature at 2 m
        fq2m_lake          ,&! relation for specific humidity at 2 m
        fm10m_lake         ,&! integral of profile function for momentum at 10 m
        dheatl               ! vegetation heat change [W/m2]

   real(r8) :: z0m_g,z0h_g,zol_g,obu_g,ustar_g,qstar_g,tstar_g
   real(r8) :: fm10m,fm_g,fh_g,fq_g,fh2m,fq2m,um,obu,eb

   ! definition for urban related
   real(r8), allocatable :: Ainv(:,:) ! Inverse of Radiation transfer matrix
   real(r8), allocatable :: X(:)      ! solution
   real(r8), allocatable :: dX(:)     ! solution
   real(r8), allocatable :: B(:)      ! Vectors of incident radiation on each surface
   real(r8), allocatable :: B1(:)     ! Vectors of incident radiation on each surface
   real(r8), allocatable :: dBdT(:)   ! Vectors of incident radiation on each surface
   real(r8), allocatable :: dT(:)     ! Vectors of incident radiation on each surface
   real(r8), allocatable :: SkyVF(:)  ! View factor to sky
   real(r8), allocatable :: VegVF(:)  ! View factor to vegetation
   real(r8), allocatable :: fcover(:) ! fractional cover of roof, wall, ground and veg


!=======================================================================
! [1] Initial set and propositional variables
!=======================================================================

      ! fluxes
      fsenl = 0.;  fevpl = 0.
      etr   = 0.;  rst   = 2.0e4
      assim = 0.;  respc = 0.

      emis  = 0.;  z0m   = 0.
      zol   = 0.;  rib   = 0.
      ustar = 0.;  qstar = 0.
      tstar = 0.;  rootr = 0.

      dheatl = 0.

      ! latent heat, assumed that the sublimation occurred only as wliq_gpersno=0
      htvp_roof = hvap
      htvp_gimp = hvap
      htvp_gper = hvap
      IF (wliq_roofsno(lbr)<=0. .and. wice_roofsno(lbr)>0.) htvp_roof = hsub
      IF (wliq_gimpsno(lbi)<=0. .and. wice_gimpsno(lbi)>0.) htvp_gimp = hsub
      IF (wliq_gpersno(lbp)<=0. .and. wice_gpersno(lbp)>0.) htvp_gper = hsub

      ! potential temperature at the reference height
      thm = forc_t + 0.0098*forc_hgt_t                     !intermediate variable equivalent to
                                                           !forc_t*(pgcm/forc_psrf)**(rgas/cpair)
      th  = forc_t*(100000./forc_psrf)**(rgas/cpair)       !potential T
      thv = th*(1.+0.61*forc_q)                            !virtual potential T
      ur  = max(0.1,sqrt(forc_us*forc_us+forc_vs*forc_vs)) !limit set to 0.1

      ! Adjust wall temperature, weighted average according to fwsun, dfwsun
      !-------------------------------------------
      fwsha = 1. - fwsun

      IF (dfwsun > 0) THEN
         t_wallsun = (fwsun*t_wallsun + dfwsun*t_wallsha) / (fwsun+dfwsun)
         twsun_inner = (fwsun*twsun_inner + dfwsun*twsun_inner) / (fwsun+dfwsun)
         lwsun = (fwsun*lwsun + dfwsun*lwsha ) / (fwsun+dfwsun)
      ENDIF

      IF (dfwsun < 0) THEN
         t_wallsha = (fwsha*t_wallsha - dfwsun*t_wallsun) / (fwsha-dfwsun)
         twsha_inner = (fwsha*twsha_inner - dfwsun*twsun_inner) / (fwsha-dfwsun)
         lwsha = (fwsha*lwsha - dfwsun*lwsun ) / (fwsha-dfwsun)
      ENDIF

      ! update fwsun
      fwsun = fwsun + dfwsun

      ! temperature and water mass from previous time step
      twsun = t_wallsun( 1 )
      twsha = t_wallsha( 1 )
      troof = t_roofsno(lbr)
      tgimp = t_gimpsno(lbi)
      tgper = t_gpersno(lbp)

      troof_nl_bef = t_roofsno(nl_roof)
      twsun_nl_bef = t_wallsun(nl_wall)
      twsha_nl_bef = t_wallsha(nl_wall)

      !TODO: ???how to calculate tlake
      IF (lbl < 1) THEN
         tlake = t_lakesno(lbl)
      ELSE
         tlake = t_lake(1)
      ENDIF

      ! SAVE temperature
      troof_bef = troof
      twsun_bef = twsun
      twsha_bef = twsha
      tgimp_bef = tgimp
      tgper_bef = tgper

      ! SAVE longwave for the last time
      dlwsun = lwsun
      dlwsha = lwsha
      dlgimp = lgimp
      dlgper = lgper
      dlveg  = lveg

      fg  = 1. - froof

      IF (lai+sai>1.e-6 .and. fveg>0.) THEN
         doveg = .true.
      ELSE
         doveg = .false.
      ENDIF

      ! convert AHE to urban area, i.e. (1-flake)
      IF ( 1-flake > 0. ) THEN
         Fhac = Fhac / (1-flake)
         Fwst = Fwst / (1-flake)
         Fach = Fach / (1-flake)
         vehc = vehc / (1-flake)
         meta = meta / (1-flake)
      ENDIF


!=======================================================================
! [2] specific humidity and its derivative at ground surface
!=======================================================================

      qred = 1.
      CALL qsadv(tgper,forc_psrf,eg,degdT,qsatg,qsatgdT)

      ! initialization for rss
      rss = 0.

      IF (patchtype <=1 ) THEN          !soil ground
         wx = (wliq_gpersno(1)/denh2o + wice_gpersno(1)/denice)/dz_gpersno(1)
         IF (porsl(1) < 1.e-6) THEN     !bed rock
            fac = 0.001
         ELSE
            fac = min(1.,wx/porsl(1))
            fac = max( fac, 0.001 )
         ENDIF

#ifdef Campbell_SOIL_MODEL
         psit = psi0(1) * fac ** (- bsw(1) )   !psit = max(smpmin, psit)
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
         psit = soil_psi_from_vliq ( fac*(porsl(1)-theta_r(1)) + theta_r(1), &
            porsl(1), theta_r(1), psi0(1), &
            5, (/alpha_vgm(1), n_vgm(1), L_vgm(1), sc_vgm(1), fc_vgm(1)/))
#endif
         psit = max( -1.e8, psit )
         hr   = exp(psit/roverg/tgper)
         qred = (1.-fsno_gper)*hr + fsno_gper

         IF (lbp == 1) THEN !no snow layer exist

            ! calculate soil resistance for evaporation
            wx = (sum(wliq_gpersno(1:2))/denh2o+sum(wice_gpersno(1:2))/denice)/sum(dz_gpersno(1:2))
            IF (sum(porsl(1:2)) < 1.e-6) THEN     !bed rock
               fac = 0.001
            ELSE
               fac = min(1.,sum(dz_gpersno(1:2))*wx/(dz_gpersno(1)*porsl(1)+dz_gpersno(2)*porsl(2)))
               fac = max( fac, 0.001 )
            ENDIF

            ! Sellers et al., 1992
            rss = (1-fsno_gper)*exp(8.206-4.255*fac)
         ENDIF
      ENDIF

      qgper = qred*qsatg
      dqgperdT = qred*qsatgdT

      IF (qsatg>forc_q .and. forc_q>qred*qsatg) THEN
        qgper = forc_q; dqgperdT = 0.
      ENDIF

      CALL qsadv(tgimp,forc_psrf,eg,degdT,qsatg,qsatgdT)
      qgimp    = qsatg
      dqgimpdT = qsatgdT

      CALL qsadv(troof,forc_psrf,eg,degdT,qsatg,qsatgdT)
      qroof    = qsatg
      dqroofdT = qsatgdT


!=======================================================================
! [3] calculate longwave radiation
!=======================================================================

      IF ( doveg ) THEN

         allocate ( Ainv(5,5)   )
         allocate ( X(5)        )
         allocate ( dX(5)       )
         allocate ( B(5)        )
         allocate ( B1(5)       )
         allocate ( dBdT(5)     )
         allocate ( SkyVF(5)    )
         allocate ( VegVF(5)    )
         allocate ( fcover(0:5) )
         allocate ( dT(0:5)     )

         ! call longwave function (vegetation)
         CALL UrbanVegLongwave ( &
                                theta, hlr, froof, fgper, hroof, forc_frl, &
                                twsun, twsha, tgimp, tgper, ewall, egimp, &
                                egper, lai, sai, fveg, (htop+hbot)/2., &
                                ev, Ainv, B, B1, dBdT, SkyVF, VegVF, fcover)
      ELSE

         allocate ( Ainv(4,4)   )
         allocate ( X(4)        )
         allocate ( dX(4)       )
         allocate ( B(4)        )
         allocate ( B1(4)       )
         allocate ( dBdT(4)     )
         allocate ( SkyVF(4)    )
         allocate ( fcover(0:4) )
         allocate ( dT(0:4)     )

         ! call longwave function, calculate Ainv, B, B1, dBdT
         CALL UrbanOnlyLongwave ( &
                                 theta, hlr, froof, fgper, hroof, forc_frl, &
                                 twsun, twsha, tgimp, tgper, ewall, egimp, egper, &
                                 Ainv, B, B1, dBdT, SkyVF, fcover)

         ! calculate longwave radiation abs, for UrbanOnlyLongwave
         !-------------------------------------------
         X = matmul(Ainv, B)

         ! using the longwave radiation transfer matrix to calculate
         ! LW radiation absorption by each surface and total absorption.
         lwsun = ( ewall*X(1) - B1(1) ) / (1-ewall)
         lwsha = ( ewall*X(2) - B1(2) ) / (1-ewall)
         lgimp = ( egimp*X(3) - B1(3) ) / (1-egimp)
         lgper = ( egper*X(4) - B1(4) ) / (1-egper)

         ! Out-going LW of urban canopy
         lout  = sum( X * SkyVF )

         ! Energy balance check
         eb = lwsun + lwsha + lgimp + lgper + lout

         IF (abs(eb-forc_frl) > 1e-6) THEN
            print *, "Urban Only Longwave - Energy Balance Check error!", eb-forc_frl
         ENDIF

         ! fur per unit surface
         IF (fcover(1) >0.) lwsun = lwsun / fcover(1) * fg !/ (4*fwsun*HL*fb/fg)
         IF (fcover(2) >0.) lwsha = lwsha / fcover(2) * fg !/ (4*fwsha*HL*fb/fg)
         IF (fcover(3) >0.) lgimp = lgimp / fcover(3) * fg !/ fgimp
         IF (fcover(4) >0.) lgper = lgper / fcover(4) * fg !/ fsoil

         ! added last time value
         lwsun = lwsun + dlwsun
         lwsha = lwsha + dlwsha
         lgimp = lgimp + dlgimp
         lgper = lgper + dlgper
      ENDIF

      dlwbef = dlwsun*fcover(1) + dlwsha*fcover(2) + dlgimp*fcover(3) + dlgper*fcover(4)
      IF ( doveg ) dlwbef = dlwbef + dlveg*fcover(5)
      dlwbef = dlwbef*(1-flake)

      ! roof net longwave
      lroof = eroof*forc_frl - eroof*stefnc*troof**4


!=======================================================================
! [4] Compute sensible and latent fluxes and their derivatives with respect
!     to ground temperature using ground temperatures from previous time step.
!=======================================================================

      ! bare ground case
      CALL UrbanGroundFlux (forc_hgt_u,forc_hgt_t,forc_hgt_q,forc_us, &
                            forc_vs,forc_t,forc_q,forc_rhoair,forc_psrf, &
                            ur,thm,th,thv,zlnd,zsno,fsno_gimp, &
                            lbi,wliq_gimpsno(1),wice_gimpsno(1), &
                            fcover,tgimp,tgper,qgimp,qgper,tref,qref, &
                            z0m_g,z0h_g,zol_g,ustar_g,qstar_g,tstar_g,fm_g,fh_g,fq_g)

      ! SAVE variables for bare ground case
      obu_g = forc_hgt_u / zol_g


!=======================================================================
! [5] Canopy temperature, fluxes from roof/wall/ground
!=======================================================================

      IF ( doveg ) THEN

         ! soil water stress factor on stomatal resistance
         CALL eroot (nl_soil,trsmx0,porsl,&
#ifdef Campbell_SOIL_MODEL
            bsw,&
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
            theta_r, alpha_vgm, n_vgm, L_vgm, sc_vgm, fc_vgm, &
#endif
            psi0,rootfr,dz_gpersno,t_gpersno,wliq_gpersno,rootr,etrc,rstfac)

         nurb = 3

         CALL UrbanVegFlux ( &

            ! model running information
            ipatch         ,deltim         ,lbr            ,lbi            ,&
            ! forcing
            forc_hgt_u     ,forc_hgt_t     ,forc_hgt_q     ,forc_us        ,&
            forc_vs        ,thm            ,th             ,thv            ,&
            forc_q         ,forc_psrf      ,forc_rhoair    ,forc_frl       ,&
            forc_po2m      ,forc_pco2m     ,par            ,sabv           ,&
            rstfac         ,Fhac           ,Fwst           ,Fach           ,&
            vehc           ,meta                                           ,&
            ! urban and vegetation parameters
            hroof          ,hlr            ,nurb           ,fcover         ,&
            ewall          ,egimp          ,egper          ,ev             ,&
            htop           ,hbot           ,lai            ,sai            ,&
            sqrtdi         ,effcon         ,vmax25         ,c3c4           ,slti,&
            hlti           ,shti           ,hhti           ,trda           ,&
            trdm           ,trop           ,g1             ,g0             ,&
            gradm          ,binter         ,extkn          ,extkd          ,&
            dewmx          ,etrc           ,trsmx0         ,lambda         ,&
            ! surface status
            z0h_g          ,obu_g          ,ustar_g        ,zlnd           ,&
            zsno           ,fsno_roof      ,fsno_gimp      ,fsno_gper      ,&
            wliq_roofsno(1),wliq_gimpsno(1),wice_roofsno(1),wice_gimpsno(1),&
            htvp_roof      ,htvp_gimp      ,htvp_gper      ,troof          ,&
            twsun          ,twsha          ,tgimp          ,tgper          ,&
            qroof          ,qgimp          ,qgper          ,dqroofdT       ,&
            dqgimpdT       ,dqgperdT       ,sigf           ,tleaf          ,&
            ldew           ,ldew_rain      ,ldew_snow      ,fwet_snow      ,&
            dheatl         ,rss            ,etr_deficit                    ,&
            ! longwave related
            Ainv           ,B              ,B1             ,dBdT           ,&
            SkyVF          ,VegVF                                          ,&
            ! output
            taux           ,tauy           ,fsenroof       ,fsenwsun       ,&
            fsenwsha       ,fsengimp       ,fsengper       ,fevproof       ,&
            fevpgimp       ,fevpgper       ,croofs         ,cwsuns         ,&
            cwshas         ,cgrnds         ,croofl         ,cgimpl         ,&
            cgperl         ,croof          ,cgimp          ,cgper          ,&
            fsenl          ,fevpl          ,etr            ,rst            ,&
            assim          ,respc          ,lwsun          ,lwsha          ,&
            lgimp          ,lgper          ,lveg           ,lout           ,&
            tref           ,qref           ,z0m            ,zol            ,&
            rib            ,ustar          ,qstar          ,tstar          ,&
            fm             ,fh             ,fq             ,tafu            )
      ELSE

         nurb = 2

         ! CALL urban flux
         CALL  UrbanOnlyFlux ( &
            ! model running information
            ipatch         ,deltim         ,lbr            ,lbi            ,&
            ! forcing
            forc_hgt_u     ,forc_hgt_t     ,forc_hgt_q     ,forc_us        ,&
            forc_vs        ,thm            ,th             ,thv            ,&
            forc_q         ,forc_psrf      ,forc_rhoair    ,Fhac           ,&
            Fwst           ,Fach           ,vehc           ,meta           ,&
            ! surface parameters
            hroof          ,hlr            ,nurb           ,fcover         ,&
            ! surface status
            z0h_g          ,obu_g          ,ustar_g        ,zlnd           ,&
            zsno           ,fsno_roof      ,fsno_gimp      ,fsno_gper      ,&
            wliq_roofsno(1),wliq_gimpsno(1),wice_roofsno(1),wice_gimpsno(1),&
            htvp_roof      ,htvp_gimp      ,htvp_gper      ,troof          ,&
            twsun          ,twsha          ,tgimp          ,tgper          ,&
            qroof          ,qgimp          ,qgper          ,dqroofdT       ,&
            dqgimpdT       ,dqgperdT       ,rss                            ,&
            ! output
            taux           ,tauy           ,fsenroof       ,fsenwsun       ,&
            fsenwsha       ,fsengimp       ,fsengper       ,fevproof       ,&
            fevpgimp       ,fevpgper       ,croofs         ,cwsuns         ,&
            cwshas         ,cgrnds         ,croofl         ,cgimpl         ,&
            cgperl         ,croof          ,cgimp          ,cgper          ,&
            tref           ,qref           ,z0m            ,zol            ,&
            rib            ,ustar          ,qstar          ,tstar          ,&
            fm             ,fh             ,fq             ,tafu            )

         !TODO: check
         tleaf     = forc_t
         ldew      = 0.
         ldew_rain = 0.
         ldew_snow = 0.
         fwet_snow = 0.
         rstfac    = 0.
         fsenl     = 0.0
         fevpl     = 0.0
         etr       = 0.0
         assim     = 0.0
         respc     = 0.0

      ENDIF

!=======================================================================
! [6] roof/wall/ground temperature
!=======================================================================

      ! Calculate the change rate of long-wave radiation
      ! caused by temperature change
      clroof = - 4.*eroof*stefnc*troof**3
      clwsun = ( ewall*Ainv(1,1) - 1. ) / (1-ewall) * dBdT(1)
      clwsha = ( ewall*Ainv(2,2) - 1. ) / (1-ewall) * dBdT(2)
      clgimp = ( egimp*Ainv(3,3) - 1. ) / (1-egimp) * dBdT(3)
      clgper = ( egper*Ainv(4,4) - 1. ) / (1-egper) * dBdT(4)

      IF (fcover(1) >0. ) clwsun = clwsun / fcover(1) * fg !/ (4*fwsun*HL*fb/fg)
      IF (fcover(2) >0. ) clwsha = clwsha / fcover(2) * fg !/ (4*fwsha*HL*fb/fg)
      IF (fcover(3) >0. ) clgimp = clgimp / fcover(3) * fg !/ fgimp
      IF (fcover(4) >0. ) clgper = clgper / fcover(4) * fg !/ fsoil

      ! Calculate the temperature of each component: roof, wall, floor
      CALL UrbanRoofTem (lbr,deltim,capr,cnfac,&
           cv_roof,tk_roof,dz_roofsno,z_roofsno,zi_roofsno,&
           t_roofsno,wice_roofsno,wliq_roofsno,scv_roof,snowdp_roof,&
           troof_inner,lroof,clroof,sabroof,fsenroof,fevproof,croof,htvp_roof,&
           imelt_roof,sm_roof,xmf,factr,tkdz_roof)

      CALL UrbanWallTem (deltim,capr,cnfac,&
           cv_wall,tk_wall,t_wallsun,dz_wall,z_wall,zi_wall,&
           twsun_inner,lwsun,clwsun,sabwsun,fsenwsun,cwsuns,tkdz_wsun)

      CALL UrbanWallTem (deltim,capr,cnfac,&
           cv_wall,tk_wall,t_wallsha,dz_wall,z_wall,zi_wall,&
           twsha_inner,lwsha,clwsha,sabwsha,fsenwsha,cwshas,tkdz_wsha)

      CALL UrbanImperviousTem (patchtype,lbi,deltim,&
           capr,cnfac,csol,k_solids,porsl,psi0,dkdry,dksatu,dksatf,&
           vf_quartz,vf_gravels,vf_om,vf_sand,wf_gravels,wf_sand,&
           BA_alpha, BA_beta,&
           cv_gimp,tk_gimp,dz_gimpsno,z_gimpsno,zi_gimpsno,&
           t_gimpsno,wice_gimpsno,wliq_gimpsno,scv_gimp,snowdp_gimp,&
           lgimp,clgimp,sabgimp,fsengimp,fevpgimp,cgimp,htvp_gimp,&
           imelt_gimp,sm_gimp,xmf,facti)

      CALL UrbanPerviousTem (patchtype,lbp,deltim,&
           capr,cnfac,csol,k_solids,porsl,psi0,dkdry,dksatu,dksatf,&
           vf_quartz,vf_gravels,vf_om,vf_sand,wf_gravels,wf_sand,&
           BA_alpha, BA_beta,&
#ifdef Campbell_SOIL_MODEL
           bsw,&
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
           theta_r,alpha_vgm,n_vgm,L_vgm,&
           sc_vgm,fc_vgm,&
#endif
           dz_gpersno,z_gpersno,zi_gpersno,&
           t_gpersno,wice_gpersno,wliq_gpersno,scv_gper,snowdp_gper,&
           lgper,clgper,sabgper,fsengper,fevpgper,cgper,htvp_gper,&
           imelt_gper,sm_gper,xmf,factp)

      ! update temperature
      twsun = t_wallsun( 1 )
      twsha = t_wallsha( 1 )
      troof = t_roofsno(lbr)
      tgimp = t_gimpsno(lbi)
      tgper = t_gpersno(lbp)
      twall = (twsun*fwsun + twsha*fwsha)/(fwsun + fwsha)

      ! calculate lake temperature and sensible/latent heat fluxes
      CALL laketem ( &
           ! "in" laketem arguments
           ! ---------------------------
           patchtype      ,maxsnl         ,nl_soil        ,nl_lake        ,&
           patchlatr      ,deltim         ,forc_hgt_u     ,forc_hgt_t     ,&
           forc_hgt_q     ,forc_us        ,forc_vs        ,forc_t         ,&
           forc_q         ,forc_rhoair    ,forc_psrf      ,forc_sols      ,&
           forc_soll      ,forc_solsd     ,forc_solld     ,sablake        ,&
           forc_frl       ,dz_lakesno     ,z_lakesno      ,zi_lakesno     ,&
           dz_lake        ,lakedepth      ,vf_quartz      ,vf_gravels     ,&
           vf_om          ,vf_sand        ,wf_gravels     ,wf_sand        ,&
           porsl          ,csol           ,k_solids                       ,&
           dksatu         ,dksatf         ,dkdry                          ,&
           BA_alpha       ,BA_beta        ,hpbl                           ,&

           ! "inout" laketem arguments
           ! ---------------------------
           tlake          ,scv_lake       ,snowdp_lake    ,t_lakesno      ,&
           wliq_lakesno   ,wice_lakesno   ,imelt_lake     ,t_lake         ,&
           lake_icefrac   ,savedtke1                                      ,&

! SNICAR model variables
           snofrz         ,sabg_lyr                                       ,&
! END SNICAR model variables

           ! "out" laketem arguments
           ! ---------------------------
           taux_lake      ,tauy_lake      ,fsena_lake                     ,&
           fevpa_lake     ,lfevpa_lake    ,fseng_lake     ,fevpg_lake     ,&
           qseva_lake     ,qsubl_lake     ,qsdew_lake     ,qfros_lake     ,&
           olrg_lake      ,fgrnd_lake     ,tref_lake      ,qref_lake      ,&
           trad_lake      ,emis_lake      ,z0m_lake       ,zol_lake       ,&
           rib_lake       ,ustar_lake     ,qstar_lake     ,tstar_lake     ,&
           fm_lake        ,fh_lake        ,fq_lake        ,fh2m_lake      ,&
           fq2m_lake      ,fm10m_lake     ,sm_lake        ,&
           urban_call=.true.                                               )

      lnet_lake = forc_frl - olrg_lake

!=======================================================================
! [7] Correct fluxes for temperature change
!=======================================================================

      ! calculate temperature change
      dT(0) = troof - troof_bef
      dT(1) = twsun - twsun_bef
      dT(2) = twsha - twsha_bef
      dT(3) = tgimp - tgimp_bef
      dT(4) = tgper - tgper_bef
      IF ( doveg ) dT(5) = 0.

      ! flux change due to temperature change
      fsenroof = fsenroof + dT(0)*croofs
      fsenwsun = fsenwsun + dT(1)*cwsuns
      fsenwsha = fsenwsha + dT(2)*cwshas
      fsengimp = fsengimp + dT(3)*cgrnds
      fsengper = fsengper + dT(4)*cgrnds

      fevproof = fevproof + dT(0)*croofl
      fevpgimp = fevpgimp + dT(3)*cgimpl
      fevpgper = fevpgper + dT(4)*cgperl

! calculation of evaporative potential; flux in kg m-2 s-1.
! egidif holds the excess energy IF all water is evaporated
! during the timestep.  this energy is later added to the sensible heat flux.

      ! --- for pervious ground ---
      egsmax = (wice_gpersno(lbp)+wliq_gpersno(lbp)) / deltim
      egidif = max( 0., fevpgper - egsmax )
      fevpgper = min ( fevpgper, egsmax )
      fsengper = fsengper + htvp_gper*egidif

      ! --- for impervious ground ---
      egsmax = (wice_gimpsno(lbi)+wliq_gimpsno(lbi)) / deltim
      egidif = max( 0., fevpgimp - egsmax )
      fevpgimp = min ( fevpgimp, egsmax )
      fsengimp = fsengimp + htvp_gimp*egidif

      ! --- for roof ---
      egsmax = (wice_roofsno(lbr)+wliq_roofsno(lbr)) / deltim
      egidif = max( 0., fevproof - egsmax )
      fevproof = min ( fevproof, egsmax )
      fsenroof = fsenroof + htvp_roof*egidif

!=======================================================================
! [8] total fluxes to atmosphere
!=======================================================================

      lnet  = lroof   *fcover(0) + lwsun   *fcover(1) + lwsha   *fcover(2) + &
              lgimp   *fcover(3) + lgper   *fcover(4)

      ! 03/30/2022, Wenzong Dong: bug find, sabgwsha->sabgwsun
      sabg  = sabroof *fcover(0) + sabwsun *fcover(1) + sabwsha *fcover(2) + &
              sabgimp *fcover(3) + sabgper *fcover(4)

      ! 03/30/2022, Wenzong Dong: bug find, fsenwsha->fsenwsun
      fseng = fsenroof*fcover(0) + fsenwsun*fcover(1) + fsenwsha*fcover(2) + &
              fsengimp*fcover(3) + fsengper*fcover(4)

      fsen_roof = fsenroof*fcover(0)
      fsen_wsun = fsenwsun*fcover(1)
      fsen_wsha = fsenwsha*fcover(2)
      fsen_gimp = fsengimp*fcover(3)
      fsen_gper = fsengper*fcover(4)

      fevpg = fevproof*fcover(0) + fevpgimp*fcover(3) + fevpgper*fcover(4)

      lfevpa = htvp_roof*fevproof*fcover(0) + &
               htvp_gimp*fevpgimp*fcover(3) + &
               htvp_gper*fevpgper*fcover(4)

      lfevp_roof = htvp_roof*fevproof*fcover(0)
      lfevp_gimp = htvp_gimp*fevpgimp*fcover(3)
      lfevp_gper = htvp_gper*fevpgper*fcover(4)

      IF ( doveg ) THEN
         assim  = assim * fveg
         respc  = respc * fveg
         fsenl  = fsenl * fveg
         fevpl  = fevpl * fveg
         etr    = etr   * fveg
         fsena  = fsenl + fseng
         fevpa  = fevpl + fevpg
         lfevpa = lfevpa + hvap*fevpl

         fsen_urbl   = fsenl
         lfevp_urbl  = hvap*fevpl
         etr_deficit = etr_deficit*fveg
      ELSE
         fsena  = fseng
         fevpa  = fevpg
      ENDIF

      fsena  = fsena  + (Fhac + Fwst + vehc)*fsh + Fach + meta
      lfevpa = lfevpa + (Fhac + Fwst + vehc)*flh

      ! flux/variable average weighted by fractional cover
      taux   = taux   *(1-flake) + taux_lake   *flake
      tauy   = tauy   *(1-flake) + tauy_lake   *flake
      sabg   = sabg   *(1-flake) + sablake     *flake
      lnet   = lnet   *(1-flake) + lnet_lake   *flake
      fseng  = fseng  *(1-flake) + fseng_lake  *flake
      fsena  = fsena  *(1-flake) + fsena_lake  *flake
      fevpg  = fevpg  *(1-flake) + fevpg_lake  *flake
      lfevpa = lfevpa *(1-flake) + lfevpa_lake *flake
      tref   = tref   *(1-flake) + tref_lake   *flake
      qref   = qref   *(1-flake) + qref_lake   *flake
      z0m    = z0m    *(1-flake) + z0m_lake    *flake
      zol    = zol    *(1-flake) + zol_lake    *flake
      rib    = rib    *(1-flake) + rib_lake    *flake
      ustar  = ustar  *(1-flake) + ustar_lake  *flake
      qstar  = qstar  *(1-flake) + qstar_lake  *flake
      tstar  = tstar  *(1-flake) + tstar_lake  *flake
      fm     = fm     *(1-flake) + fm_lake     *flake
      fh     = fh     *(1-flake) + fh_lake     *flake
      fq     = fq     *(1-flake) + fq_lake     *flake

      ! 10/01/2021, yuan: exclude lake fevpa.
      ! because we don't consider water balance for lake currently.
      !fevpa  = fevpa *(1-flake) + fevpa_lake *flake

      ! 07/11/2023, yuan: don't not consider lake fraction cover
      !fsenl  = fsenl *(1-flake)
      !fevpl  = fevpl *(1-flake)
      !etr    = etr   *(1-flake)
      !assim  = assim *(1-flake)
      !respc  = respc *(1-flake)

      ! effective ground temperature, simple average
      ! 12/01/2021, yuan: !TODO Bugs. temperature cannot be weighted like below.
      !t_grnd = troof*fcover(0) + twsun*fcover(1) + twsha*fcover(2) + &
      t_grnd = tgper*fgper + tgimp*(1-fgper)

      !==============================================
      qseva_roof = 0.
      qsubl_roof = 0.
      qfros_roof = 0.
      qsdew_roof = 0.

      IF (fevproof >= 0.)THEN
! not allow for sublimation in melting (melting ==> evap. ==> sublimation)
         qseva_roof = min(wliq_roofsno(lbr)/deltim, fevproof)
         qsubl_roof = fevproof - qseva_roof
      ELSE
         IF (troof < tfrz)THEN
            qfros_roof = abs(fevproof)
         ELSE
            qsdew_roof = abs(fevproof)
         ENDIF
      ENDIF

      !==============================================
      qseva_gimp = 0.
      qsubl_gimp = 0.
      qfros_gimp = 0.
      qsdew_gimp = 0.

      IF (fevpgimp >= 0.)THEN
! not allow for sublimation in melting (melting ==> evap. ==> sublimation)
         qseva_gimp = min(wliq_gimpsno(lbi)/deltim, fevpgimp)
         qsubl_gimp = fevpgimp - qseva_gimp
      ELSE
         IF (tgimp < tfrz)THEN
            qfros_gimp = abs(fevpgimp)
         ELSE
            qsdew_gimp = abs(fevpgimp)
         ENDIF
      ENDIF

      !==============================================
      qseva_gper = 0.
      qsubl_gper = 0.
      qfros_gper = 0.
      qsdew_gper = 0.

      IF (fevpgper >= 0.)THEN
! not allow for sublimation in melting (melting ==> evap. ==> sublimation)
         qseva_gper = min(wliq_gpersno(lbp)/deltim, fevpgper)
         qsubl_gper = fevpgper - qseva_gper
      ELSE
         IF (tgper < tfrz)THEN
            qfros_gper = abs(fevpgper)
         ELSE
            qsdew_gper = abs(fevpgper)
         ENDIF
      ENDIF

!=======================================================================
! [9] Calculate the change of long-wave radiation caused by temperature change
!=======================================================================

      dX = matmul(Ainv, dBdT*dT(1:))
      dlwsun = ( ewall*dX(1) - dBdT(1)*dT(1) ) / (1-ewall)
      dlwsha = ( ewall*dX(2) - dBdT(2)*dT(2) ) / (1-ewall)
      dlgimp = ( egimp*dX(3) - dBdT(3)*dT(3) ) / (1-egimp)
      dlgper = ( egper*dX(4) - dBdT(4)*dT(4) ) / (1-egper)

      IF ( doveg ) THEN
         dlveg = ( sum(dX(1:5)*VegVF(1:5))*ev )
      ELSE
         dlveg = 0.
      ENDIF

      dlout = sum( dX * SkyVF )

      ! Energy balance check
      eb = dlwsun + dlwsha + dlgimp + dlgper + dlveg + dlout

      IF (abs(eb) > 1e-6) THEN
         print *, "Urban Vegetation Longwave - Energy Balance Check error!", eb
      ENDIF

      ! for per unit surface
      IF (fcover(1) > 0.) dlwsun = dlwsun / fcover(1) * fg !/ (4*fwsun*HL*fb/fg)
      IF (fcover(2) > 0.) dlwsha = dlwsha / fcover(2) * fg !/ (4*fwsha*HL*fb/fg)
      IF (fcover(3) > 0.) dlgimp = dlgimp / fcover(3) * fg !/ fgimp
      IF (fcover(4) > 0.) dlgper = dlgper / fcover(4) * fg !/ fgper
      IF ( doveg        ) dlveg  = dlveg  / fcover(5) * fg !/ fv/fg

      dlw = dlwsun*fcover(1) + dlwsha*fcover(2) + dlgimp*fcover(3) + dlgper*fcover(4)
      IF ( doveg) dlw = dlw + dlveg*fcover(5)
      dlw = dlw*(1-flake)

      ! calculate out going longwave by added the before value
      ! of lout and considered troof change
      lout = lout + dlout
      rout = (1-eroof)*forc_frl + eroof*stefnc*troof_bef**4 &
           + 4.*eroof*stefnc*troof_bef**3*dT(0)

      olrg = lout*fg + rout*froof
      olrg = olrg*(1-flake) + olrg_lake*flake

      IF (olrg < 0) THEN
         write(6,*) 'Urban_THERMAL.F90: Urban out-going longwave radiation < 0!'
         write(6,*) ipatch,olrg,lout,dlout,rout,olrg_lake,fg,froof,flake
         CALL CoLM_stop()
      ENDIF

      ! radiative temperature
      trad = (olrg/stefnc)**0.25

! averaged bulk surface emissivity
!TODO: how to calculate for urban case?
! 03/10/2020, yuan: removed below.
      !olrb = stefnc*t_soisno_bef(lb)**3*(4.*tinc)
      !olrb = stefnc*t_grnd_bef**3*(4.*tinc)
      !olru = ulrad + emg*olrb
      !olrb = ulrad + olrb
      !emis = olru / olrb


!=======================================================================
! [10] ground heat flux and energy balance error
!=======================================================================

      ! ground heat flux
      fgrnd = sabg + lnet - dlwbef - dlout*fg*(1-flake) &
            - 4.*eroof*stefnc*troof_bef**3*dT(0)*froof*(1-flake)&
            - fseng - (lfevp_roof + lfevp_gimp + lfevp_gper)*(1-flake) &
            - lfevpa_lake*flake

      ! energy balance check
      errore = sabg + sabv*fveg*(1-flake) &
             + forc_frl - olrg &
             + (Fhac + Fwst + Fach + vehc + meta)*(1-flake) &
             - fsena - lfevpa - fgrnd &
             - dheatl*fveg*(1-flake)

      fgrnd = fgrnd - (Fhac + Fwst + Fach + vehc + meta)*(1-flake)

#if (defined CoLMDEBUG)
      IF (abs(errore)>.5) THEN
         write(6,*) 'Urban_THERMAL.F90: Urban energy balance violation'
         write(6,*) ipatch,errore,sabg,sabv*fveg*(1-flake)
         write(6,*) forc_frl,dlwbef,dlw,olrg
         write(6,*) Fhac,Fwst,Fach,vehc,meta,(1-flake)
         write(6,*) fsena,lfevpa,fgrnd
         write(6,*) dheatl*fveg*(1-flake)
         CALL CoLM_stop()
      ENDIF
100   format(10(f15.3))
#endif

      ! diagnostic sabg only for pervious and impervious ground
      !sabg = sabgper*fgper + sabgimp*(1-fgper)

      ! SAVE for next time run
      lwsun = dlwsun
      lwsha = dlwsha
      lgimp = dlgimp
      lgper = dlgper
      lveg  = dlveg

      ! deallocate memory
      deallocate ( Ainv   )
      deallocate ( X      )
      deallocate ( dX     )
      deallocate ( B      )
      deallocate ( B1     )
      deallocate ( dBdT   )
      deallocate ( SkyVF  )
      deallocate ( dT     )

      IF ( doveg ) THEN
         deallocate ( VegVF )
      ENDIF


!=======================================================================
! [11] Anthropogenic heat
!=======================================================================

      ! A simple Building energy model
      CALL SimpleBEM ( deltim, forc_rhoair, fcover(0:2), hroof, troommax, troommin, &
                       troof_nl_bef, twsun_nl_bef, twsha_nl_bef, &
                       t_roofsno(nl_roof), t_wallsun(nl_wall), t_wallsha(nl_wall), &
                       tkdz_roof, tkdz_wsun, tkdz_wsha, tafu, troom, &
                       troof_inner, twsun_inner, twsha_inner, &
                       Fhac, Fwst, Fach, Fhah )

      ! Anthropogenic heat flux for the rest (vehicle heat flux and metabolic heat flux)
      CALL LUCY ( idate       , deltim  , patchlonr, fix_holiday, &
                  week_holiday, hum_prof, wdh_prof , weh_prof   ,pop_den, &
                  vehicle     , Fahe    , vehc     , meta )

      fgrnd = fgrnd + (Fhac + Fwst + Fach)*(1-flake) + vehc + meta


      ! convert BEM AHE to grid area values
      ! NOTE: BEM AHE are assumed only affecting the urban area,
      ! but vehc and meta area for the whole grid.
      Fhac = Fhac * (1-flake)
      Fwst = Fwst * (1-flake)
      Fach = Fach * (1-flake)
      Fhah = Fhah * (1-flake)


      deallocate ( fcover )

   END SUBROUTINE UrbanTHERMAL

END MODULE MOD_Urban_Thermal
! ---------- EOP ------------
