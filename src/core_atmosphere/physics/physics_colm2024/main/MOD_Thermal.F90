#include <define.h>

MODULE MOD_Thermal

!-----------------------------------------------------------------------
   USE MOD_Precision
   IMPLICIT NONE
   SAVE

! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: THERMAL


!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------


   SUBROUTINE THERMAL (ipatch ,patchtype,is_dry_lake,lb            ,deltim        ,&
                       trsmx0        ,zlnd          ,zsno          ,csoilc        ,&
                       dewmx         ,capr          ,cnfac         ,vf_quartz     ,&
                       vf_gravels    ,vf_om         ,vf_sand       ,wf_gravels    ,&
                       wf_sand       ,csol          ,porsl         ,psi0          ,&
#ifdef Campbell_SOIL_MODEL
                       bsw           ,&
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
                       theta_r       ,alpha_vgm     ,n_vgm         ,L_vgm         ,&
                       sc_vgm        ,fc_vgm        ,                              &
#endif
                       k_solids      ,dksatu        ,dksatf        ,dkdry         ,&
                       BA_alpha      ,BA_beta       ,lai           ,laisun        ,&
                       laisha        ,sai           ,htop          ,hbot          ,&
                       sqrtdi        ,rootfr        ,rstfacsun_out ,rstfacsha_out ,&
                       rss           ,gssun_out     ,gssha_out     ,assimsun_out  ,&
                       etrsun_out    ,assimsha_out  ,etrsha_out    ,&
!photosynthesis and plant hydraulic variables
                       effcon        ,vmax25        ,c3c4          ,hksati        ,smp     ,hk   ,&
                       kmax_sun      ,kmax_sha      ,kmax_xyl      ,kmax_root     ,&
                       psi50_sun     ,psi50_sha     ,psi50_xyl     ,psi50_root    ,&
                       ck            ,vegwp         ,gs0sun        ,gs0sha        ,&
!Ozone stress variables
                       o3coefv_sun   ,o3coefv_sha   ,o3coefg_sun   ,o3coefg_sha   ,&
                       lai_old       ,o3uptakesun   ,o3uptakesha   ,forc_ozone    ,&
!end ozone stress variables
!Ozone WUE stomata model parameter
                       lambda        ,&! Marginal water cost of carbon gain ((mol h2o) (mol co2)-1)
!End WUE stomata model parameter
                       slti          ,hlti          ,shti          ,hhti          ,&
                       trda          ,trdm          ,trop          ,g1            ,&
                       g0            ,gradm         ,binter        ,extkn         ,&
                       forc_hgt_u    ,forc_hgt_t    ,forc_hgt_q    ,forc_us       ,&
                       forc_vs       ,forc_t        ,forc_q        ,forc_rhoair   ,&
                       forc_psrf     ,forc_pco2m    ,forc_hpbl     ,forc_po2m     ,&
                       coszen        ,parsun        ,parsha        ,sabvsun       ,&
                       sabvsha       ,sabg          ,sabg_soil     ,sabg_snow     ,&
                       frl           ,extkb         ,extkd         ,thermk        ,&
                       fsno          ,sigf          ,dz_soisno     ,z_soisno      ,&
                       zi_soisno     ,tleaf         ,t_soisno      ,wice_soisno   ,&
                       wliq_soisno   ,ldew          ,ldew_rain     ,ldew_snow     ,&
                       fwet_snow     ,scv           ,snowdp        ,imelt         ,&
                       taux          ,tauy          ,fsena         ,fevpa         ,&
                       lfevpa        ,fsenl         ,fevpl         ,etr           ,&
                       fseng         ,fevpg         ,olrg          ,fgrnd         ,&
                       rootr         ,rootflux      ,qseva         ,qsdew         ,&
                       qsubl         ,qfros         ,qseva_soil    ,qsdew_soil    ,&
                       qsubl_soil    ,qfros_soil    ,qseva_snow    ,qsdew_snow    ,&
                       qsubl_snow    ,qfros_snow    ,sm            ,tref          ,&
                       qref          ,trad          ,rst           ,assim         ,&
                       respc         ,errore        ,emis          ,z0m           ,&
                       zol           ,rib           ,ustar         ,qstar         ,&
                       tstar         ,fm            ,fh            ,fq            ,&
                       fh2m          ,fq2m          ,fm10m         ,&
                       u10m          ,v10m          ,chs2          ,cqs2          ,cd10          ,&
                       pg_rain       ,pg_snow       ,t_precip      ,qintr_rain    ,&
                       qintr_snow    ,snofrz        ,sabg_snow_lyr                 )

!=======================================================================
!  this is the main subroutine to execute the calculation
!  of thermal processes and surface fluxes
!
!  Original author: Yongjiu Dai, 09/15/1999; 08/30/2002
!
!  FLOW DIAGRAM FOR THERMAL.F90
!
!  THERMAL ===> qsadv
!               GroundFluxes
!               eroot                             |dewfraction
!               LeafTemperature   |               |qsadv
!               LeafTemperaturePC |  ---------->  |moninobukini
!                                                 |moninobuk
!                                                 |MOD_AssimStomataConductance
!
!               GroundTemperature    ---------->   meltf
!
!
! !REVISIONS:
!  08/2019, Hua Yuan: added initial codes for PFT and Plant Community
!           (PC) vegetation classification processes
!
!  01/2021, Nan Wei: added variables passing of plant hydraulics and
!           precipitation sensible heat with canopy and ground for PFT
!           and Plant Community (PC)
!=======================================================================

   USE MOD_Precision
   USE MOD_Vars_Global
   USE MOD_Const_PFT
   USE MOD_Const_Physical, only: denh2o,roverg,hvap,hsub,rgas,cpair,&
                                 stefnc,denice,tfrz,vonkar,grav,cpliq,cpice
	   USE MOD_FrictionVelocity
	   USE MOD_Eroot
	   USE MOD_GroundFluxes
	   USE MOD_LeafTemperature
	   USE MOD_LeafTemperaturePC
	   USE MOD_GroundTemperature
	   USE MOD_Qsadv
	   USE MOD_SoilSurfaceResistance
	   USE MOD_Vars_TimeVariables, only: qsfc
#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
   USE MOD_LandPFT, only: patch_pft_s, patch_pft_e
   USE MOD_Vars_TimeInvariants, only: patchclass
   USE MOD_Vars_TimeVariables, only: &
       lai_enftemp, lai_enfboreal, lai_dnfboreal, lai_ebftrop, lai_ebftemp, lai_dbftrop, lai_dbftemp, &
       lai_dbfboreal, lai_ebstemp, lai_dbstemp, lai_dbsboreal, lai_c3arcgrass, lai_c3grass, lai_c4grass
   USE MOD_Vars_PFTimeInvariants
   USE MOD_Vars_PFTimeVariables
   USE MOD_Vars_1DPFTFluxes
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
   USE MOD_Hydro_SoilFunction, only: soil_psi_from_vliq
#endif
   USE MOD_MPAS_MPI
   USE MOD_Namelist, only: DEF_USE_PLANTHYDRAULICS, DEF_RSS_SCHEME, DEF_SPLIT_SOILSNOW, &
                           DEF_USE_LCT,DEF_USE_PFT,DEF_USE_PC,DEF_PC_CROP_SPLIT

   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------

   integer, intent(in) :: &
       ipatch,                   &! patch index
       lb,                       &! lower bound of array
       patchtype                  ! land patch type (0=soil, 1=urban or built-up, 2=wetland,
                                  !                  3=glacier/ice sheet, 4=land water bodies)
   logical, intent(in) :: is_dry_lake

   real(r8), intent(inout) :: &
       sai                        ! stem area index  [-]
   real(r8), intent(in) :: &
       deltim,                   &! model time step [second]
       trsmx0,                   &! max transpiration for moist soil+100% veg.  [mm/s]
       zlnd,                     &! roughness length for soil [m]
       zsno,                     &! roughness length for snow [m]
       csoilc,                   &! drag coefficient for soil under canopy [-]
       dewmx,                    &! maximum dew
       capr,                     &! tuning factor to turn first layer T into surface T
       cnfac,                    &! Crank Nicholson factor between 0 and 1

       ! soil physical parameters
       vf_quartz (1:nl_soil),    &! volumetric fraction of quartz within mineral soil
       vf_gravels(1:nl_soil),    &! volumetric fraction of gravels
       vf_om     (1:nl_soil),    &! volumetric fraction of organic matter
       vf_sand   (1:nl_soil),    &! volumetric fraction of sand
       wf_gravels(1:nl_soil),    &! gravimetric fraction of gravels
       wf_sand   (1:nl_soil),    &! gravimetric fraction of sand
       csol      (1:nl_soil),    &! heat capacity of soil solids [J/(m3 K)]
       porsl     (1:nl_soil),    &! soil porosity [-]
       psi0      (1:nl_soil),    &! soil water suction, negative potential [mm]
#ifdef Campbell_SOIL_MODEL
       bsw(1:nl_soil),           &! clapp and hornberger "b" parameter [-]
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
       theta_r   (1:nl_soil),    &! residual moisture content [-]
       alpha_vgm (1:nl_soil),    &! a parameter corresponding approximately to the inverse of the air-entry value
       n_vgm     (1:nl_soil),    &! pore-connectivity parameter [dimensionless]
       L_vgm     (1:nl_soil),    &! a shape parameter [dimensionless]
       sc_vgm    (1:nl_soil),    &! saturation at the air entry value in the classical vanGenuchten model [-]
       fc_vgm    (1:nl_soil),    &! a scaling factor by using air entry value in the Mualem model [-]
#endif
       k_solids  (1:nl_soil),    &! thermal conductivity of minerals soil [W/m-K]
       dkdry     (1:nl_soil),    &! thermal conductivity of dry soil [W/m-K]
       dksatu    (1:nl_soil),    &! thermal conductivity of saturated unfrozen soil [W/m-K]
       dksatf    (1:nl_soil),    &! thermal conductivity of saturated frozen soil [W/m-K]
       hksati    (1:nl_soil),    &! hydraulic conductivity at saturation [mm h2o/s]
       BA_alpha  (1:nl_soil),    &! alpha in Balland and Arp(2005) thermal conductivity scheme
       BA_beta   (1:nl_soil),    &! beta in Balland and Arp(2005) thermal conductivity scheme

       ! vegetation parameters
       lai,                      &! adjusted leaf area index for seasonal variation [-]
       htop,                     &! canopy crown top height [m]
       hbot,                     &! canopy crown bottom height [m]
       sqrtdi,                   &! inverse sqrt of leaf dimension [m**-0.5]
       rootfr(1:nl_soil),        &! root fraction

       effcon,                   &! quantum efficiency of RuBP regeneration (mol CO2/mol quanta)
       vmax25,                   &! maximum carboxylation rate at 25 C at canopy top
       kmax_sun,                 &! Plant Hydraulics Parameters
       kmax_sha,                 &! Plant Hydraulics Parameters
       kmax_xyl,                 &! Plant Hydraulics Parameters
       kmax_root,                &! Plant Hydraulics Parameters
       psi50_sun,                &! water potential at 50% loss of sunlit leaf tissue conductance (mmH2O)
       psi50_sha,                &! water potential at 50% loss of shaded leaf tissue conductance (mmH2O)
       psi50_xyl,                &! water potential at 50% loss of xylem tissue conductance (mmH2O)
       psi50_root,               &! water potential at 50% loss of root tissue conductance (mmH2O)
       ck,                       &! shape-fitting parameter for vulnerability curve (-)
       slti,                     &! slope of low temperature inhibition function      [s3]
       hlti,                     &! 1/2 point of low temperature inhibition function  [s4]
       shti,                     &! slope of high temperature inhibition function     [s1]
       hhti,                     &! 1/2 point of high temperature inhibition function [s2]
       trda,                     &! temperature coefficient in gs-a model             [s5]
       trdm,                     &! temperature coefficient in gs-a model             [s6]
       trop,                     &! temperature coefficient in gs-a model
       g1,                       &! conductance-photosynthesis slope parameter for medlyn model
       g0,                       &! conductance-photosynthesis intercept for medlyn model
       gradm,                    &! conductance-photosynthesis slope parameter
       binter,                   &! conductance-photosynthesis intercept
       extkn,                    &! coefficient of leaf nitrogen allocation

       ! atmospherical variables and observational height
       forc_hgt_u,               &! observational height of wind [m]
       forc_hgt_t,               &! observational height of temperature [m]
       forc_hgt_q,               &! observational height of humidity [m]
       forc_us,                  &! wind component in eastward direction [m/s]
       forc_vs,                  &! wind component in northward direction [m/s]
       forc_t,                   &! temperature at agcm reference height [kelvin]
       forc_q,                   &! specific humidity at agcm reference height [kg/kg]
       forc_rhoair,              &! density air [kg/m3]
       forc_psrf,                &! atmosphere pressure at the surface [pa]
       forc_pco2m,               &! CO2 concentration in atmos. (pascals)
       forc_po2m,                &! O2 concentration in atmos. (pascals)
       forc_hpbl,                &! atmospheric boundary layer height [m]
       pg_rain,                  &! rainfall onto ground including canopy runoff [kg/(m2 s)]
       pg_snow,                  &! snowfall onto ground including canopy runoff [kg/(m2 s)]
       t_precip,                 &! snowfall/rainfall temperature [kelvin]
       qintr_rain,               &! rainfall interception (mm h2o/s)
       qintr_snow,               &! snowfall interception (mm h2o/s)

       ! radiative fluxes
       coszen,                   &! cosine of the solar zenith angle
       parsun,                   &! photosynthetic active radiation by sunlit leaves (W m-2)
       parsha,                   &! photosynthetic active radiation by shaded leaves (W m-2)
       sabvsun,                  &! solar radiation absorbed by vegetation [W/m2]
       sabvsha,                  &! solar radiation absorbed by vegetation [W/m2]
       sabg,                     &! solar radiation absorbed by ground [W/m2]
       sabg_soil,                &! solar radiation absorbed by ground soil [W/m2]
       sabg_snow,                &! solar radiation absorbed by ground snow [W/m2]
       frl,                      &! atmospheric infrared (longwave) radiation [W/m2]
       extkb,                    &! (k, g(mu)/mu) direct solar extinction coefficient
       extkd,                    &! diffuse and scattered diffuse PAR extinction coefficient
       thermk,                   &! canopy gap fraction for tir radiation

       ! state variable (1)
       fsno,                     &! fraction of ground covered by snow
       sigf,                     &! fraction of veg cover, excluding snow-covered veg [-]
       dz_soisno(lb:nl_soil),    &! layer thickness [m]
       z_soisno (lb:nl_soil),    &! node depth [m]
       zi_soisno(lb-1:nl_soil)    ! interface depth [m]

   integer , intent(in) :: &
       c3c4 ! C3/C4 plant type

   real(r8), intent(in) :: &
       sabg_snow_lyr(lb:1)        ! snow layer absorption

       ! state variables (2)
   real(r8), intent(inout) :: &
       vegwp(1:nvegwcs),         &! vegetation water potential
       gs0sun,                   &! working copy of sunlit stomata conductance
       gs0sha,                   &! working copy of shaded stomata conductance
!Ozone stress variables
       lai_old    ,              &! lai in last time step
       o3uptakesun,              &! Ozone does, sunlit leaf (mmol O3/m^2)
       o3uptakesha,              &! Ozone does, shaded leaf (mmol O3/m^2)
       forc_ozone ,              &! Ozone
!end ozone stress variables

!Ozone WUE stomata model parameter
       lambda,                   &! Marginal water cost of carbon gain ((mol h2o) (mol co2)-1)
!End WUE stomata model parameter

       tleaf,                    &! shaded leaf temperature [K]
       t_soisno(lb:nl_soil),     &! soil temperature [K]
       wice_soisno(lb:nl_soil),  &! ice lens [kg/m2]
       wliq_soisno(lb:nl_soil)    ! liquid water [kg/m2]

   real(r8), intent(in) :: &
       smp(1:nl_soil)         ,  &! soil matrix potential [mm]
       hk(1:nl_soil)              ! hydraulic conductivity [mm h2o/s]

   real(r8), intent(inout) :: &
       ldew,                     &! depth of water on foliage [kg/(m2 s)]
       ldew_rain,                &! depth of rain on foliage [kg/(m2 s)]
       ldew_snow,                &! depth of rain on foliage [kg/(m2 s)]
       fwet_snow,                &! vegetation canopy snow fractional cover [-]
       scv,                      &! snow cover, water equivalent [mm, kg/m2]
       snowdp                     ! snow depth [m]

   real(r8), intent(out) :: &
       snofrz (lb:0)              !snow freezing rate (col,lyr) [kg m-2 s-1]

   integer,  intent(out) :: &
       imelt(lb:nl_soil)          ! flag for melting or freezing [-]

   real(r8), intent(out) :: &
       laisun,                   &! sunlit leaf area index
       laisha,                   &! shaded leaf area index
       gssun_out,                &! sunlit stomata conductance
       gssha_out,                &! shaded stomata conductance
       rstfacsun_out,            &! factor of soil water stress on sunlit leaf
       rstfacsha_out              ! factor of soil water stress on shaded leaf

   real(r8), intent(out) :: &
       assimsun_out ,            &! diagnostic sunlit leaf assim value for output
       etrsun_out   ,            &! diagnostic sunlit leaf etr value for output
       assimsha_out ,            &! diagnostic shaded leaf assim for output
       etrsha_out                 ! diagnostic shaded leaf etr for output

       ! Output fluxes
   real(r8), intent(out) :: &
       taux,                     &! wind stress: E-W [kg/m/s**2]
       tauy,                     &! wind stress: N-S [kg/m/s**2]
       fsena,                    &! sensible heat from canopy height to atmosphere [W/m2]
       fevpa,                    &! evapotranspiration from canopy height to atmosphere [mm/s]
       lfevpa,                   &! latent heat flux from canopy height to atmosphere [W/m2]
       fsenl,                    &! sensible heat from leaves [W/m2]
       fevpl,                    &! evaporation+transpiration from leaves [mm/s]
       etr,                      &! transpiration rate [mm/s]
       fseng,                    &! sensible heat flux from ground [W/m2]
       fevpg,                    &! evaporation heat flux from ground [mm/s]
       olrg,                     &! outgoing long-wave radiation from ground+canopy
       fgrnd,                    &! ground heat flux [W/m2]
       rootr(1:nl_soil),         &! water uptake fraction from different layers, all layers add to 1.0
       rootflux(1:nl_soil),      &! root uptake from different layer, all layers add to transpiration

       qseva,                    &! ground surface evaporation rate (mm h2o/s)
       qsdew,                    &! ground surface dew formation (mm h2o /s) [+]
       qsubl,                    &! sublimation rate from snow pack (mm h2o /s) [+]
       qfros,                    &! surface dew added to snow pack (mm h2o /s) [+]
       qseva_soil,               &! ground soil surface evaporation rate (mm h2o/s)
       qsdew_soil,               &! ground soil surface dew formation (mm h2o /s) [+]
       qsubl_soil,               &! sublimation rate from soil ice pack (mm h2o /s) [+]
       qfros_soil,               &! surface dew added to soil ice pack (mm h2o /s) [+]
       qseva_snow,               &! ground snow surface evaporation rate (mm h2o/s)
       qsdew_snow,               &! ground snow surface dew formation (mm h2o /s) [+]
       qsubl_snow,               &! sublimation rate from snow pack (mm h2o /s) [+]
       qfros_snow,               &! surface dew added to snow pack (mm h2o /s) [+]

       sm,                       &! rate of snowmelt [kg/(m2 s)]
       tref,                     &! 2 m height air temperature [kelvin]
       qref,                     &! 2 m height air specific humidity
       trad,                     &! radiative temperature [K]
       rss,                      &! bare soil resistance for evaporation [s/m]
       rst,                      &! stomatal resistance (s m-1)
       assim,                    &! assimilation
       respc,                    &! respiration

       ! additional variables required by coupling with WRF or RSM model
       emis,                     &! averaged bulk surface emissivity
       z0m,                      &! effective roughness [m]
       zol,                      &! dimensionless height (z/L) used in Monin-Obukhov theory
       rib,                      &! bulk Richardson number in surface layer
       ustar,                    &! u* in similarity theory [m/s]
       qstar,                    &! q* in similarity theory [kg/kg]
       tstar,                    &! t* in similarity theory [K]
       fm,                       &! integral of profile function for momentum
       fh,                       &! integral of profile function for heat
       fq,                       &! integral of profile function for moisture
       fh2m,                     &! relation for temperature at 2m
       fq2m,                     &! relation for specific humidity at 2m
       fm10m,                    &! integral of profile function for momentum at 10m
       u10m,                     &! area-effective 10 m zonal wind [m/s]
       v10m,                     &! area-effective 10 m meridional wind [m/s]
       chs2,                     &! area-effective 2 m heat exchange velocity [m/s]
       cqs2,                     &! area-effective 2 m moisture exchange velocity [m/s]
       cd10                       ! area-effective 10 m momentum coefficient

!Ozone stress variables
   real(r8),intent(inout) ::     &
        o3coefv_sun,&! Ozone stress factor for photosynthesis on sunlit leaf
        o3coefv_sha,&! Ozone stress factor for photosynthesis on sunlit leaf
        o3coefg_sun,&! Ozone stress factor for stomata on shaded leaf
        o3coefg_sha  ! Ozone stress factor for stomata on shaded leaf
!end ozone stress variables


!-------------------------- Local Variables ----------------------------

   integer i,j

   real(r8) :: &
       fseng_soil,               &! sensible heat flux from soil fraction
       fseng_snow,               &! sensible heat flux from snow fraction
       fevpg_soil,               &! latent heat flux from soil fraction
       fevpg_snow,               &! latent heat flux from snow fraction

       cgrnd,                    &! deriv. of soil energy flux wrt to soil temp [w/m2/k]
       cgrndl,                   &! deriv, of soil sensible heat flux wrt soil temp [w/m2/k]
       cgrnds,                   &! deriv of soil latent heat flux wrt soil temp [w/m**2/k]
       degdT,                    &! d(eg)/dT
       dqgdT,                    &! d(qg)/dT
       dlrad,                    &! downward longwave radiation blow the canopy [W/m2]
       eg,                       &! water vapor pressure at temperature T [pa]
       egsmax,                   &! max. evaporation which soil can provide at one time step
       egidif,                   &! the excess of evaporation over "egsmax"
       emg,                      &! ground emissivity (0.97 for snow,
                                  ! glaciers and water surface; 0.96 for soil and wetland)
       errore,                   &! energy balnce error [w/m2]
       etrc,                     &! maximum possible transpiration rate [mm/s]
       fac,                      &! soil wetness of surface layer
       fact(lb:nl_soil),         &! used in computing tridiagonal matrix
       fsun,                     &! fraction of sunlit canopy
       hr,                       &! relative humidity
       htvp,                     &! latent heat of vapor of water (or sublimation) [j/kg]
       olru,                     &! olrg excluding dwonwelling reflection [W/m2]
       olrb,                     &! olrg assuming blackbody emission [W/m2]
       psit,                     &! negative potential of soil
       qg,                       &! ground specific humidity [kg/kg]
! 03/07/2020, yuan:
       q_soil,                   &! ground soil specific humidity [kg/kg]
       q_snow,                   &! ground snow specific humidity [kg/kg]
       qsatg,                    &! saturated humidity [kg/kg]
       qsatgdT,                  &! d(qsatg)/dT
       qred,                     &! soil surface relative humidity
       sabv,                     &! solar absorbed by canopy [W/m2]
       thm,                      &! intermediate variable (forc_t+0.0098*forc_hgt_t)
       th,                       &! potential temperature (kelvin)
       thv,                      &! virtual potential temperature (kelvin)
       rstfac,                   &! factor of soil water stress
       t_grnd,                   &! ground surface temperature [K]
       t_grnd_bef,               &! ground surface temperature [K]
       t_soil,                   &! ground soil temperature
       t_snow,                   &! ground snow temperature
       t_soisno_bef(lb:nl_soil), &! soil/snow temperature before update
       tinc,                     &! temperature difference of two time step
       ur,                       &! wind speed at reference height [m/s]
       ulrad,                    &! upward longwave radiation above the canopy [W/m2]
       wice0(lb:nl_soil),        &! ice mass from previous time-step
       wliq0(lb:nl_soil),        &! liquid mass from previous time-step
       wx,                       &! patial volume of ice and water of surface layer
       xmf,                      &! total latent heat of phase change of ground water [W/m2]
       hprl,                     &! precipitation sensible heat from canopy [W/m2]
       dheatl                     ! vegetation heat change [W/m2]

   real(r8) :: z0m_g,z0h_g,zol_g,obu_g,rib_g,ustar_g,qstar_g,tstar_g
   real(r8) :: fm_g,fh_g,fq_g,um,obu

   integer p, ps, pe, pn
   real(r8) :: pft_wt

   real(r8), allocatable :: rootr_p     (:,:)
   real(r8), allocatable :: rootflux_p  (:,:)
   real(r8), allocatable :: etrc_p        (:)
   real(r8), allocatable :: rstfac_p      (:)
   real(r8), allocatable :: rstfacsun_p   (:)
   real(r8), allocatable :: rstfacsha_p   (:)
   real(r8), allocatable :: gssun_p       (:)
   real(r8), allocatable :: gssha_p       (:)
   real(r8), allocatable :: fsun_p        (:)
   real(r8), allocatable :: sabv_p        (:)
   real(r8), allocatable :: fcover        (:)

! 03/06/2020, yuan: added
   real(r8), allocatable :: fseng_soil_p  (:)
   real(r8), allocatable :: fseng_snow_p  (:)
   real(r8), allocatable :: fevpg_soil_p  (:)
   real(r8), allocatable :: fevpg_snow_p  (:)
   real(r8), allocatable :: cgrnd_p       (:)
   real(r8), allocatable :: cgrnds_p      (:)
   real(r8), allocatable :: cgrndl_p      (:)
   real(r8), allocatable :: dlrad_p       (:)
   real(r8), allocatable :: ulrad_p       (:)
   real(r8), allocatable :: zol_p         (:)
   real(r8), allocatable :: rib_p         (:)
   real(r8), allocatable :: ustar_p       (:)
   real(r8), allocatable :: qstar_p       (:)
   real(r8), allocatable :: tstar_p       (:)
   real(r8), allocatable :: fm_p          (:)
   real(r8), allocatable :: fh_p          (:)
   real(r8), allocatable :: fq_p          (:)
   real(r8), allocatable :: fh2m_p        (:)
   real(r8), allocatable :: fq2m_p        (:)
   real(r8), allocatable :: fm10m_p       (:)
   real(r8), allocatable :: hprl_p        (:)
   real(r8), allocatable :: assimsun_p    (:)
   real(r8), allocatable :: etrsun_p      (:)
   real(r8), allocatable :: assimsha_p    (:)
   real(r8), allocatable :: etrsha_p      (:)
   real(r8), allocatable :: dheatl_p      (:)


!=======================================================================
! [1] Initial set and propositional variables
!=======================================================================

      ! emissivity
      emg = 0.96
      IF (scv>0. .or. patchtype==3) emg = 0.97

      ! fluxes
      taux   = 0.;  tauy   = 0.
      fsena  = 0.;  fevpa  = 0.
      lfevpa = 0.;  fsenl  = 0.
      fevpl  = 0.;  etr    = 0.
      fseng  = 0.;  fevpg  = 0.

      cgrnds = 0.;  cgrndl = 0.
      cgrnd  = 0.;  tref   = 0.
      qref   = 0.;  rst    = 2.0e4
      assim  = 0.;  respc  = 0.
      hprl   = 0.;  dheatl = 0.

      emis   = 0.;  z0m    = 0.
      zol    = 0.;  rib    = 0.
      ustar  = 0.;  qstar  = 0.
      tstar  = 0.;  rootr  = 0.
      rootflux = 0.
      fh2m = spval;  fq2m = spval;  fm10m = spval
      u10m = spval;  v10m = spval;  chs2 = spval;  cqs2 = spval;  cd10 = spval

      dlrad  = frl

      t_soil = t_soisno(1)
      t_snow = t_soisno(lb)

IF (.not.DEF_SPLIT_SOILSNOW) THEN
      t_grnd = t_soisno(lb)
      ulrad  = frl*(1.-emg) + emg*stefnc*t_grnd**4
ELSE
      t_grnd = fsno*t_snow  + (1.-fsno)*t_soil
      ulrad  = frl*(1.-emg) &
             + fsno*emg*stefnc*t_snow**4 &
             + (1.-fsno)*emg*stefnc*t_soil**4
ENDIF

      ! temperature and water mass from previous time step
      t_soisno_bef(lb:) = t_soisno(lb:)
      t_grnd_bef = t_grnd
      wice0(lb:) = wice_soisno(lb:)
      wliq0(lb:) = wliq_soisno(lb:)

      ! latent heat, assumed that the sublimation occurred only as wliq_soisno=0
      htvp = hvap
      IF (wliq_soisno(lb)<=0. .and. wice_soisno(lb)>0.) htvp = hsub

      ! potential temperature at the reference height
      thm = forc_t + 0.0098*forc_hgt_t                     !intermediate variable equivalent to
                                                           !forc_t*(pgcm/forc_psrf)**(rgas/cpair)
      th  = forc_t*(100000./forc_psrf)**(rgas/cpair)       !potential T
      thv = th*(1.+0.61*forc_q)                            !virtual potential T
      ur  = max(0.1,sqrt(forc_us*forc_us+forc_vs*forc_vs)) !limit set to 0.1


!=======================================================================
! [2] specific humidity and its derivative at ground surface
!=======================================================================

      qred = 1.
      hr   = 1.

      IF ((patchtype<=1) .or. is_dry_lake &
         .or. (DEF_USE_Dynamic_Wetland .and. (patchtype==2))) THEN  !soil ground
         wx   = (wliq_soisno(1)/denh2o + wice_soisno(1)/denice)/dz_soisno(1)
         IF (porsl(1) < 1.e-6) THEN     !bed rock
            fac  = 0.001
         ELSE
            fac  = min(1.,wx/porsl(1))
            fac  = max( fac, 0.001 )
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
         hr   = exp(psit/roverg/t_grnd)
         qred = (1.-fsno)*hr + fsno
      ENDIF

IF (.not. DEF_SPLIT_SOILSNOW) THEN
      CALL qsadv(t_grnd,forc_psrf,eg,degdT,qsatg,qsatgdT)

      qg     = qred*qsatg
      dqgdT  = qred*qsatgdT

      IF (qsatg > forc_q .and. forc_q > qred*qsatg) THEN
        qg = forc_q; dqgdT = 0.
      ENDIF

      q_soil = qg
      q_snow = qg

ELSE
      CALL qsadv(t_soil,forc_psrf,eg,degdT,qsatg,qsatgdT)

      q_soil = hr*qsatg
      dqgdT  = (1.-fsno)*hr*qsatgdT

      IF(qsatg > forc_q .and. forc_q > hr*qsatg)THEN
        q_soil = forc_q; dqgdT = 0.
      ENDIF

      CALL qsadv(t_snow,forc_psrf,eg,degdT,qsatg,qsatgdT)

      q_snow = qsatg
      dqgdT  = dqgdT + fsno*qsatgdT

	      ! weighted average qg
	      qg = (1.-fsno)*q_soil + fsno*q_snow
	ENDIF
	      IF (allocated(qsfc)) qsfc(ipatch) = qg

	      ! calculate soil surface resistance (rss)
      ! ------------------------------------------------
      !NOTE: (1) DEF_RSS_SCHEME=0 means no rss considered
      !      (2) Do NOT calculate rss for the first timestep
      IF (DEF_RSS_SCHEME>0 .and. rss/=spval) THEN

         !NOTE: If the beta scheme is used, the rss is not soil resistance,
         !but soil beta factor (soil wetness relative to field capacity [0-1]).
         CALL SoilSurfaceResistance (nl_soil,forc_rhoair,hksati,porsl,psi0, &
#ifdef Campbell_SOIL_MODEL
                            bsw, &
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
                            theta_r, alpha_vgm, n_vgm, L_vgm, sc_vgm, fc_vgm, &
#endif
                            dz_soisno,t_soisno,wliq_soisno,wice_soisno,fsno,qg,rss)
      ELSE
         IF (DEF_RSS_SCHEME == 4) THEN
            rss = 1.        !LP92
         ELSE
            rss = 0.        !the other RSS schemes
         ENDIF
      ENDIF

!=======================================================================
! [3] Compute sensible and latent fluxes and their derivatives with respect
!     to ground temperature using ground temperatures from previous time step.
! TODO: modify code description
!=======================================================================

      ! Always CALL GroundFluxes for bare ground CASE
      CALL GroundFluxes (zlnd,zsno,forc_hgt_u,forc_hgt_t,forc_hgt_q,forc_hpbl, &
                         forc_us,forc_vs,forc_t,forc_q,forc_rhoair,forc_psrf, &
                         ur,thm,th,thv,t_grnd,qg,rss,dqgdT,htvp, &
                         fsno,cgrnd,cgrndl,cgrnds, &
                         t_soil,t_snow,q_soil,q_snow, &
                         !taux,tauy,fseng,fevpg,tref,qref, &
                         taux,tauy,fseng,fseng_soil,fseng_snow, &
                         fevpg,fevpg_soil,fevpg_snow,tref,qref, &
                         z0m_g,z0h_g,zol_g,rib_g,ustar_g,qstar_g,tstar_g,fm_g,fh_g,fq_g, &
                         fh2m,fq2m,fm10m)

      obu_g = forc_hgt_u / zol_g


!=======================================================================
! [4] Canopy temperature, fluxes from the canopy
!=======================================================================

IF ( patchtype==0.and.DEF_USE_LCT .or. patchtype>0 ) THEN

      sabv = sabvsun + sabvsha

      IF (lai+sai > 1e-6) THEN

         ! soil water stress factor on stomatal resistance
         CALL eroot (nl_soil,trsmx0,porsl,&
#ifdef Campbell_SOIL_MODEL
            bsw,&
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
            theta_r, alpha_vgm, n_vgm, L_vgm, sc_vgm, fc_vgm, &
#endif
            psi0,rootfr,dz_soisno,t_soisno,wliq_soisno,rootr,etrc,rstfac)

         ! fraction of sunlit and shaded leaves of canopy
         fsun = ( 1. - exp(-min(extkb*lai,40.))) / max( min(extkb*lai,40.), 1.e-6 )

         IF (coszen<=0.0 .or. sabv<1.) fsun = 0.5

         laisun = lai*fsun
         laisha = lai*(1-fsun)
         rstfacsun_out = rstfac
         rstfacsha_out = rstfac

         CALL LeafTemperature(ipatch,1,deltim,csoilc   ,dewmx       ,htvp        ,&
                 lai         ,sai         ,htop        ,hbot        ,sqrtdi      ,&
                 effcon      ,vmax25      ,c3c4        ,slti        ,hlti        ,shti        ,&
                 hhti        ,trda        ,trdm        ,trop        ,g1          ,&
                 g0          ,gradm       ,binter      ,extkn       ,extkb       ,&
                 extkd       ,forc_hgt_u  ,forc_hgt_t  ,forc_hgt_q  ,forc_us     ,&
                 forc_vs     ,thm         ,th          ,thv         ,forc_q      ,&
                 forc_psrf   ,forc_rhoair ,parsun      ,parsha      ,sabv        ,&
                 frl         ,fsun        ,thermk    ,rstfacsun_out,rstfacsha_out,&
                 gssun_out   ,gssha_out   ,forc_po2m   ,forc_pco2m  ,z0h_g       ,&
                 obu_g       ,ustar_g     ,zlnd        ,zsno        ,fsno        ,&
                 sigf        ,etrc        ,t_grnd      ,qg          ,rss         ,&
                 t_soil      ,t_snow      ,q_soil      ,q_snow      ,dqgdT       ,&
                 emg         ,tleaf       ,ldew        ,ldew_rain   ,ldew_snow   ,&
                 fwet_snow   ,taux        ,tauy        ,&
                 fseng       ,fseng_soil  ,fseng_snow  ,&
                 fevpg       ,fevpg_soil  ,fevpg_snow  ,&
                 cgrnd       ,cgrndl      ,cgrnds      ,&
                 tref        ,qref        ,rst         ,assim       ,respc       ,&
                 fsenl       ,fevpl       ,etr         ,dlrad       ,ulrad       ,&
                 z0m         ,zol         ,rib         ,ustar       ,qstar       ,&
                 tstar       ,fm          ,fh          ,fq          ,fh2m        ,&
                 fq2m        ,fm10m       ,rootfr      ,&
                 kmax_sun    ,kmax_sha    ,kmax_xyl    ,kmax_root   ,psi50_sun   ,&
                 psi50_sha   ,psi50_xyl   ,psi50_root  ,ck          ,vegwp       ,&
                 gs0sun      ,gs0sha                                             ,&
                 assimsun_out,etrsun_out  ,assimsha_out             ,etrsha_out  ,&
!Ozone stress variables
                 o3coefv_sun ,o3coefv_sha ,o3coefg_sun ,o3coefg_sha ,&
                 lai_old     ,o3uptakesun ,o3uptakesha ,forc_ozone  ,&
!end ozone stress variables
!Ozone WUE stomata model parameter
                 lambda      ,&! Marginal water cost of carbon gain ((mol h2o) (mol co2)-1)
!End WUE stomata model parameter
                 forc_hpbl   ,&
                 qintr_rain  ,qintr_snow  ,t_precip    ,hprl        ,dheatl      ,&
                 smp         ,hk(1:)      ,hksati(1:)  ,rootflux(1:)              )
      ELSE
         tleaf         = forc_t
         laisun        = 0.
         laisha        = 0.
         ldew_rain     = 0.
         ldew_snow     = 0.
         fwet_snow     = 0.
         ldew          = 0.
         rstfacsun_out = 0.
         rstfacsha_out = 0.
         assimsun_out  = 0.
         assimsha_out  = 0.
         etrsun_out    = 0.
         etrsha_out    = 0.
         gssun_out     = 0.
         gssha_out     = 0.
         IF (DEF_USE_PLANTHYDRAULICS) THEN
            vegwp = -2.5e4
         ENDIF
      ENDIF

ENDIF

      u10m = forc_us * fm10m / fm
      v10m = forc_vs * fm10m / fm
      chs2 = vonkar * ustar / fh2m
      cqs2 = vonkar * ustar / fq2m
      cd10 = (vonkar / fm10m)**2


#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
IF (patchtype == 0) THEN

      ps = patch_pft_s(ipatch)
      pe = patch_pft_e(ipatch)

      allocate ( rootr_p (nl_soil, ps:pe) )
      allocate ( rootflux_p(nl_soil,ps:pe))
      allocate ( etrc_p           (ps:pe) )
      allocate ( rstfac_p         (ps:pe) )
      allocate ( rstfacsun_p      (ps:pe) )
      allocate ( rstfacsha_p      (ps:pe) )
      allocate ( gssun_p          (ps:pe) )
      allocate ( gssha_p          (ps:pe) )
      allocate ( fsun_p           (ps:pe) )
      allocate ( sabv_p           (ps:pe) )
      allocate ( fcover           (ps:pe) )

      allocate ( fseng_soil_p     (ps:pe) )
      allocate ( fseng_snow_p     (ps:pe) )
      allocate ( fevpg_soil_p     (ps:pe) )
      allocate ( fevpg_snow_p     (ps:pe) )
      allocate ( cgrnd_p          (ps:pe) )
      allocate ( cgrnds_p         (ps:pe) )
      allocate ( cgrndl_p         (ps:pe) )
      allocate ( dlrad_p          (ps:pe) )
      allocate ( ulrad_p          (ps:pe) )
      allocate ( zol_p            (ps:pe) )
      allocate ( rib_p            (ps:pe) )
      allocate ( ustar_p          (ps:pe) )
      allocate ( qstar_p          (ps:pe) )
      allocate ( tstar_p          (ps:pe) )
      allocate ( fm_p             (ps:pe) )
      allocate ( fh_p             (ps:pe) )
      allocate ( fq_p             (ps:pe) )
      allocate ( fh2m_p           (ps:pe) )
      allocate ( fq2m_p           (ps:pe) )
      allocate ( fm10m_p          (ps:pe) )

      allocate ( hprl_p           (ps:pe) )
      allocate ( assimsun_p       (ps:pe) )
      allocate ( etrsun_p         (ps:pe) )
      allocate ( assimsha_p       (ps:pe) )
      allocate ( etrsha_p         (ps:pe) )
      allocate ( dheatl_p         (ps:pe) )

      sabv_p(ps:pe) = sabvsun_p(ps:pe) + sabvsha_p(ps:pe)
      sabv = sabvsun + sabvsha

      DO i = ps, pe
         p = pftclass(i)

         IF (lai_p(i)+sai_p(i) > 1e-6) THEN

            CALL eroot (nl_soil,trsmx0,porsl,&
#ifdef Campbell_SOIL_MODEL
               bsw, &
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
               theta_r, alpha_vgm, n_vgm, L_vgm, sc_vgm, fc_vgm, &
#endif
               psi0,rootfr_p(:,p),&
               dz_soisno,t_soisno,wliq_soisno,rootr_p(:,i),etrc_p(i),rstfac_p(i))

            ! fraction of sunlit and shaded leaves of canopy
            fsun_p(i) = ( 1. - exp(-min(extkb_p(i)*lai_p(i),40.))) &
                      / max( min(extkb_p(i)*lai_p(i),40.), 1.e-6 )

            IF (coszen<=0.0 .or. sabv_p(i)<1.) fsun_p(i) = 0.5

            laisun_p(i)    = lai_p(i)*fsun_p(i)
            laisha_p(i)    = lai_p(i)*(1-fsun_p(i))
            rstfacsun_p(i) = rstfac_p(i)
            rstfacsha_p(i) = rstfac_p(i)
         ELSE
            laisun_p(i)    = 0.
            laisha_p(i)    = 0.
            ldew_rain_p(i) = 0.
            ldew_snow_p(i) = 0.
            fwet_snow_p(i) = 0.
            ldew_p(i)      = 0.
            rootr_p(:,i)   = 0.
            rootflux_p(:,i)= 0.
            rstfacsun_p(i) = 0.
            rstfacsha_p(i) = 0.
            dheatl_p(i)    = 0.
         ENDIF
      ENDDO

      IF (.not. DEF_USE_LAIFEEDBACK)THEN
         lai_enftemp      (ipatch) = 0._r8
         lai_enfboreal    (ipatch) = 0._r8
         lai_dnfboreal    (ipatch) = 0._r8
         lai_ebftrop      (ipatch) = 0._r8
         lai_ebftemp      (ipatch) = 0._r8
         lai_dbftrop      (ipatch) = 0._r8
         lai_dbftemp      (ipatch) = 0._r8
         lai_dbfboreal    (ipatch) = 0._r8
         lai_ebstemp      (ipatch) = 0._r8
         lai_dbstemp      (ipatch) = 0._r8
         lai_dbsboreal    (ipatch) = 0._r8
         lai_c3arcgrass   (ipatch) = 0._r8
         lai_c3grass      (ipatch) = 0._r8
         lai_c4grass      (ipatch) = 0._r8
         DO i = ps, pe
            p = pftclass(i)
            IF(p .eq. 1)THEN
               lai_enftemp   (ipatch) = lai_p(i)
            ELSE IF(p .eq. 2)THEN
               lai_enfboreal (ipatch) = lai_p(i)
            ELSE IF(p .eq. 3)THEN
               lai_dnfboreal (ipatch) = lai_p(i)
            ELSE IF(p .eq. 4)THEN
               lai_ebftrop   (ipatch) = lai_p(i)
            ELSE IF(p .eq. 5)THEN
               lai_ebftemp   (ipatch) = lai_p(i)
            ELSE IF(p .eq. 6)THEN
               lai_dbftrop   (ipatch) = lai_p(i)
            ELSE IF(p .eq. 7)THEN
               lai_dbftemp   (ipatch) = lai_p(i)
            ELSE IF(p .eq. 8)THEN
               lai_dbfboreal (ipatch) = lai_p(i)
            ELSE IF(p .eq. 9)THEN
               lai_ebstemp   (ipatch) = lai_p(i)
            ELSE IF(p .eq. 10)THEN
               lai_dbstemp   (ipatch) = lai_p(i)
            ELSE IF(p .eq. 11)THEN
               lai_dbsboreal (ipatch) = lai_p(i)
            ELSE IF(p .eq. 12)THEN
               lai_c3arcgrass(ipatch) = lai_p(i)
            ELSE IF(p .eq. 13)THEN
               lai_c3grass   (ipatch) = lai_p(i)
            ELSE IF(p .eq. 14)THEN
               lai_c4grass   (ipatch) = lai_p(i)
            ENDIF
         ENDDO
      ENDIF

      DO i = ps, pe
         p = pftclass(i)

         ! Dealing with PFTs for PC:
         ! If defined DEF_PC_CROP_SPLIT, for crop PFTs, use 1D twostream model;
         ! Otherwise, skip to run PC 3D model.
         IF ( DEF_USE_PC .and. (.not.DEF_PC_CROP_SPLIT .or. p.lt.15) ) THEN
            CYCLE
         ENDIF

         IF (lai_p(i)+sai_p(i) > 1e-6) THEN

            CALL LeafTemperature(ipatch,p,deltim  ,csoilc          ,dewmx           ,htvp           ,&
                 lai_p(i)        ,sai_p(i)        ,htop_p(i)       ,hbot_p(i)       ,sqrtdi_p(p)    ,&
                 effcon_p(p)     ,vmax25_p(p)     ,c3c4_p(p)       ,slti_p(p)       ,hlti_p(p)       ,shti_p(p)      ,&
                 hhti_p(p)       ,trda_p(p)       ,trdm_p(p)       ,trop_p(p)       ,g1_p(p)        ,&
                 g0_p(p)         ,gradm_p(p)      ,binter_p(p)     ,extkn_p(p)      ,extkb_p(i)     ,&
                 extkd_p(i)      ,forc_hgt_u      ,forc_hgt_t      ,forc_hgt_q      ,forc_us        ,&
                 forc_vs         ,thm             ,th              ,thv             ,forc_q         ,&
                 forc_psrf       ,forc_rhoair     ,parsun_p(i)     ,parsha_p(i)     ,sabv_p(i)      ,&
                 frl             ,fsun_p(i)       ,thermk_p(i)     ,rstfacsun_p(i)  ,rstfacsha_p(i) ,&
                 gssun_p(i)      ,gssha_p(i)      ,forc_po2m       ,forc_pco2m      ,z0h_g          ,&
                 obu_g           ,ustar_g         ,zlnd            ,zsno            ,fsno           ,&
                 sigf_p(i)       ,etrc_p(i)       ,t_grnd          ,qg              ,rss            ,&
                 t_soil          ,t_snow          ,q_soil          ,q_snow          ,dqgdT          ,&
                 emg             ,tleaf_p(i)      ,ldew_p(i)       ,ldew_rain_p(i)  ,ldew_snow_p(i) ,&
                 fwet_snow_p(i)  ,taux_p(i)       ,tauy_p(i)       ,&
                 fseng_p(i)      ,fseng_soil_p(i) ,fseng_snow_p(i) ,&
                 fevpg_p(i)      ,fevpg_soil_p(i) ,fevpg_snow_p(i) ,&
                 cgrnd_p(i)      ,cgrndl_p(i)     ,cgrnds_p(i)     ,&
                 tref_p(i)       ,qref_p(i)       ,rst_p(i)        ,assim_p(i)      ,respc_p(i)     ,&
                 fsenl_p(i)      ,fevpl_p(i)      ,etr_p(i)        ,dlrad_p(i)      ,ulrad_p(i)     ,&
                 z0m_p(i)        ,zol_p(i)        ,rib_p(i)        ,ustar_p(i)      ,qstar_p(i)     ,&
                 tstar_p(i)      ,fm_p(i)         ,fh_p(i)         ,fq_p(i)         ,fh2m_p(i)      ,&
                 fq2m_p(i)       ,fm10m_p(i)      ,rootfr_p(:,p)  ,&
                 kmax_sun_p(p)   ,kmax_sha_p(p)   ,kmax_xyl_p(p)   ,kmax_root_p(p)  ,psi50_sun_p(p) ,&
                 psi50_sha_p(p)  ,psi50_xyl_p(p)  ,psi50_root_p(p) ,ck_p(p)         ,vegwp_p(:,i)   ,&
                 gs0sun_p(i)     ,gs0sha_p(i)                                                       ,&
                 assimsun_p(i)   ,etrsun_p(i)     ,assimsha_p(i)   ,etrsha_p(i)     ,&
!Ozone stress variables
                 o3coefv_sun_p(i),o3coefv_sha_p(i),o3coefg_sun_p(i),o3coefg_sha_p(i),&
                 lai_old_p(i)    ,o3uptakesun_p(i),o3uptakesha_p(i),forc_ozone      ,&
!end ozone stress variables
!Ozone WUE stomata model parameter
                 lambda_p(p)    ,&! Marginal water cost of carbon gain ((mol h2o) (mol co2)-1)
!End WUE stomata model parameter
                 forc_hpbl                                                                         ,&
                 qintr_rain_p(i) ,qintr_snow_p(i) ,t_precip        ,hprl_p(i)       ,dheatl_p(i)   ,&
                 smp             ,hk(1:)          ,hksati(1:)      ,rootflux_p(1:,i)                )
         ELSE

            CALL GroundFluxes (zlnd,zsno,forc_hgt_u,forc_hgt_t,forc_hgt_q,forc_hpbl, &
                               forc_us,forc_vs,forc_t,forc_q,forc_rhoair,forc_psrf, &
                               ur,thm,th,thv,t_grnd,qg,rss,dqgdT,htvp, &
                               fsno,cgrnd_p(i),cgrndl_p(i),cgrnds_p(i), &
                               t_soil,t_snow,q_soil,q_snow, &
                               taux_p(i),tauy_p(i),fseng_p(i),fseng_soil_p(i),fseng_snow_p(i), &
                               fevpg_p(i),fevpg_soil_p(i),fevpg_snow_p(i),tref_p(i),qref_p(i), &
                               z0m_p(i),z0h_g,zol_p(i),rib_p(i),ustar_p(i),&
                               qstar_p(i),tstar_p(i),fm_p(i),fh_p(i),fq_p(i), &
                               fh2m_p(i),fq2m_p(i),fm10m_p(i))

            tleaf_p      (i) = forc_t
            gssun_p      (i) = 0.
            gssha_p      (i) = 0.
            assimsun_p   (i) = 0.
            etrsun_p     (i) = 0.
            assimsha_p   (i) = 0.
            etrsha_p     (i) = 0.
            rst_p        (i) = 2.0e4
            assim_p      (i) = 0.
            respc_p      (i) = 0.
            fsenl_p      (i) = 0.
            fevpl_p      (i) = 0.
            etr_p        (i) = 0.
            dlrad_p      (i) = frl

IF (.not.DEF_SPLIT_SOILSNOW) THEN
            ulrad_p      (i) = frl*(1.-emg) + emg*stefnc*t_grnd**4
ELSE
            ulrad_p      (i) = frl*(1.-emg) &
                             + fsno*emg*stefnc*t_snow**4 &
                             + (1.-fsno)*emg*stefnc*t_soil**4
ENDIF
            hprl_p       (i) = 0.

            IF (DEF_USE_PLANTHYDRAULICS) THEN
               vegwp_p(:,i) = -2.5e4
            ENDIF
         ENDIF
      ENDDO

      ! Calculate end index of natrue PFTs. Some patches can have an empty
      ! PFT slice (ps > pe); keep pn below ps in that case.
      pn = ps - 1
      DO i = ps, pe
         pn = i
         p = pftclass(i)
         IF (DEF_PC_CROP_SPLIT .and. p.ge.15) THEN
            pn = pn - 1
            EXIT
         ENDIF
      ENDDO

IF ( DEF_USE_PC .and. pn.ge.ps ) THEN

      pe = pn

      ! initialization
      rst_p      (ps:pe) = 2.0e4
      assim_p    (ps:pe) = 0.
      respc_p    (ps:pe) = 0.
      fsenl_p    (ps:pe) = 0.
      fevpl_p    (ps:pe) = 0.
      etr_p      (ps:pe) = 0.
      hprl_p     (ps:pe) = 0.
      assimsun_p (ps:pe) = 0.
      assimsha_p (ps:pe) = 0.
      etrsun_p   (ps:pe) = 0.
      etrsha_p   (ps:pe) = 0.
      gssun_p    (ps:pe) = 0.
      gssha_p    (ps:pe) = 0.
      fcover     (ps:pe) = pftfrac(ps:pe) / sum(pftfrac(ps:pe))
      z0m_p      (ps:pe) = (1.-fsno)*zlnd + fsno*zsno
      z0m                = sum( z0m_p (ps:pe)*pftfrac(ps:pe) )

      IF (DEF_USE_PLANTHYDRAULICS) THEN
         vegwp_p (:,ps:pe) = -2.5e4
      ENDIF

      CALL LeafTemperaturePC (ipatch,ps,pe    ,deltim            ,csoilc            ,dewmx             ,&
         htvp              ,pftclass(ps:pe)   ,fcover(ps:pe)     ,htop_p(ps:pe)     ,hbot_p(ps:pe)     ,&
         lai_p(ps:pe)      ,sai_p(ps:pe)      ,extkb_p(ps:pe)    ,extkd_p(ps:pe)    ,forc_hgt_u        ,&
         forc_hgt_t        ,forc_hgt_q        ,forc_us           ,forc_vs           ,forc_t            ,&
         thm               ,th                ,thv               ,forc_q            ,forc_psrf         ,&
         forc_rhoair       ,parsun_p(ps:pe)   ,parsha_p(ps:pe)   ,fsun_p(:)         ,sabv_p(:)         ,&
         frl               ,thermk_p(ps:pe)   ,fshade_p(ps:pe)   ,rstfacsun_p(:)    ,rstfacsha_p(:)    ,&
         gssun_p(:)        ,gssha_p(:)        ,forc_po2m         ,forc_pco2m        ,z0h_g             ,&
         obu_g             ,ustar_g           ,zlnd              ,zsno              ,fsno              ,&
         sigf_p(ps:pe)     ,etrc_p(:)         ,t_grnd            ,qg,rss            ,dqgdT             ,&
         emg               ,t_soil            ,t_snow            ,q_soil            ,q_snow            ,&
         z0m_p(ps:pe)      ,tleaf_p(ps:pe)    ,ldew_p(ps:pe)     ,ldew_rain_p(ps:pe),ldew_snow_p(ps:pe),&
         fwet_snow_p(ps:pe),taux              ,tauy              ,fseng             ,fseng_soil        ,&
         fseng_snow        ,fevpg             ,fevpg_soil        ,fevpg_snow        ,cgrnd             ,&
         cgrndl            ,cgrnds            ,tref              ,qref              ,rst_p(ps:pe)      ,&
         assim_p(ps:pe)    ,respc_p(ps:pe)    ,fsenl_p(ps:pe)    ,fevpl_p(ps:pe)    ,etr_p(ps:pe)      ,&
         dlrad             ,ulrad             ,z0m               ,zol               ,rib               ,&
         ustar             ,qstar             ,tstar             ,fm                ,fh                ,&
         fq                ,fh2m              ,fq2m              ,fm10m             ,vegwp_p(:,ps:pe)  ,&
         gs0sun_p(ps:pe)   ,gs0sha_p(ps:pe)   ,assimsun_p(:)     ,&
         etrsun_p(:)       ,assimsha_p(:)     ,etrsha_p(:)       ,&
!Ozone stress variables
         o3coefv_sun_p(ps:pe) ,o3coefv_sha_p(ps:pe) ,o3coefg_sun_p(ps:pe) ,o3coefg_sha_p(ps:pe) ,&
         lai_old_p(ps:pe)     ,o3uptakesun_p(ps:pe) ,o3uptakesha_p(ps:pe) ,forc_ozone           ,&
!End ozone stress variables
         forc_hpbl            ,&
         qintr_rain_p(ps:pe)  ,qintr_snow_p(ps:pe)  ,t_precip             ,hprl_p(:)            ,&
         dheatl_p(ps:pe)      ,smp                  ,hk(1:)               ,hksati(1:)           ,&
         rootflux_p(:,:)       )

      dlrad_p      (ps:pe) = dlrad
      ulrad_p      (ps:pe) = ulrad
      tref_p       (ps:pe) = tref
      qref_p       (ps:pe) = qref
      taux_p       (ps:pe) = taux
      tauy_p       (ps:pe) = tauy
      fseng_p      (ps:pe) = fseng
      fseng_soil_p (ps:pe) = fseng_soil
      fseng_snow_p (ps:pe) = fseng_snow
      fevpg_p      (ps:pe) = fevpg
      fevpg_soil_p (ps:pe) = fevpg_soil
      fevpg_snow_p (ps:pe) = fevpg_snow
      cgrnd_p      (ps:pe) = cgrnd
      cgrndl_p     (ps:pe) = cgrndl
      cgrnds_p     (ps:pe) = cgrnds
      z0m_p        (ps:pe) = z0m
      zol_p        (ps:pe) = zol
      rib_p        (ps:pe) = rib
      ustar_p      (ps:pe) = ustar
      qstar_p      (ps:pe) = qstar
      tstar_p      (ps:pe) = tstar
      fm_p         (ps:pe) = fm
      fh_p         (ps:pe) = fh
      fq_p         (ps:pe) = fq
      fh2m_p       (ps:pe) = fh2m
      fq2m_p       (ps:pe) = fq2m
      fm10m_p      (ps:pe) = fm10m
ENDIF

      pe = patch_pft_e(ipatch)

      ! aggregate PFTs to a patch
      laisun        = sum( laisun_p    (ps:pe)*pftfrac(ps:pe) )
      laisha        = sum( laisha_p    (ps:pe)*pftfrac(ps:pe) )
      tleaf         = sum( tleaf_p     (ps:pe)*pftfrac(ps:pe) )
      ldew_rain     = sum( ldew_rain_p (ps:pe)*pftfrac(ps:pe) )
      ldew_snow     = sum( ldew_snow_p (ps:pe)*pftfrac(ps:pe) )
      fwet_snow     = sum( fwet_snow_p (ps:pe)*pftfrac(ps:pe) )
      ldew          = sum( ldew_p      (ps:pe)*pftfrac(ps:pe) )
      ! may have problem with rst, but the same for LC
      rst           = sum( rst_p       (ps:pe)*pftfrac(ps:pe) )
      assim         = sum( assim_p     (ps:pe)*pftfrac(ps:pe) )
      respc         = sum( respc_p     (ps:pe)*pftfrac(ps:pe) )
      fsenl         = sum( fsenl_p     (ps:pe)*pftfrac(ps:pe) )
      fevpl         = sum( fevpl_p     (ps:pe)*pftfrac(ps:pe) )
      etr           = sum( etr_p       (ps:pe)*pftfrac(ps:pe) )

      dlrad         = sum( dlrad_p     (ps:pe)*pftfrac(ps:pe) )
      ulrad         = sum( ulrad_p     (ps:pe)*pftfrac(ps:pe) )
      tref          = sum( tref_p      (ps:pe)*pftfrac(ps:pe) )
      qref          = sum( qref_p      (ps:pe)*pftfrac(ps:pe) )
      taux          = sum( taux_p      (ps:pe)*pftfrac(ps:pe) )
      tauy          = sum( tauy_p      (ps:pe)*pftfrac(ps:pe) )
      fseng         = sum( fseng_p     (ps:pe)*pftfrac(ps:pe) )
      fseng_soil    = sum( fseng_soil_p(ps:pe)*pftfrac(ps:pe) )
      fseng_snow    = sum( fseng_snow_p(ps:pe)*pftfrac(ps:pe) )
      fevpg         = sum( fevpg_p     (ps:pe)*pftfrac(ps:pe) )
      fevpg_soil    = sum( fevpg_soil_p(ps:pe)*pftfrac(ps:pe) )
      fevpg_snow    = sum( fevpg_snow_p(ps:pe)*pftfrac(ps:pe) )
      cgrnd         = sum( cgrnd_p     (ps:pe)*pftfrac(ps:pe) )
      cgrndl        = sum( cgrndl_p    (ps:pe)*pftfrac(ps:pe) )
      cgrnds        = sum( cgrnds_p    (ps:pe)*pftfrac(ps:pe) )
      z0m           = sum( z0m_p       (ps:pe)*pftfrac(ps:pe) )
      zol           = sum( zol_p       (ps:pe)*pftfrac(ps:pe) )
      rib           = sum( rib_p       (ps:pe)*pftfrac(ps:pe) )
      ustar         = sum( ustar_p     (ps:pe)*pftfrac(ps:pe) )
      qstar         = sum( qstar_p     (ps:pe)*pftfrac(ps:pe) )
      tstar         = sum( tstar_p     (ps:pe)*pftfrac(ps:pe) )
      fm            = sum( fm_p        (ps:pe)*pftfrac(ps:pe) )
      fh            = sum( fh_p        (ps:pe)*pftfrac(ps:pe) )
      fq            = sum( fq_p        (ps:pe)*pftfrac(ps:pe) )
      fh2m          = sum( fh2m_p      (ps:pe)*pftfrac(ps:pe) )
      fq2m          = sum( fq2m_p      (ps:pe)*pftfrac(ps:pe) )
      fm10m         = sum( fm10m_p     (ps:pe)*pftfrac(ps:pe) )
      pft_wt        = sum( pftfrac(ps:pe) )
      u10m          = forc_us * sum( fm10m_p(ps:pe) / fm_p(ps:pe) * pftfrac(ps:pe) ) / pft_wt
      v10m          = forc_vs * sum( fm10m_p(ps:pe) / fm_p(ps:pe) * pftfrac(ps:pe) ) / pft_wt
      chs2          = vonkar * sum( ustar_p(ps:pe) / fh2m_p(ps:pe) * pftfrac(ps:pe) ) / pft_wt
      cqs2          = vonkar * sum( ustar_p(ps:pe) / fq2m_p(ps:pe) * pftfrac(ps:pe) ) / pft_wt
      cd10          = vonkar**2 * sum( pftfrac(ps:pe) / fm10m_p(ps:pe)**2 ) / pft_wt

      rstfacsun_out = sum( rstfacsun_p (ps:pe)*pftfrac(ps:pe) )
      rstfacsha_out = sum( rstfacsha_p (ps:pe)*pftfrac(ps:pe) )
      gssun_out     = sum( gssun_p     (ps:pe)*pftfrac(ps:pe) )
      gssha_out     = sum( gssha_p     (ps:pe)*pftfrac(ps:pe) )
      assimsun_out  = sum( assimsun_p  (ps:pe)*pftfrac(ps:pe) )
      etrsun_out    = sum( etrsun_p    (ps:pe)*pftfrac(ps:pe) )
      assimsha_out  = sum( assimsha_p  (ps:pe)*pftfrac(ps:pe) )
      etrsha_out    = sum( etrsha_p    (ps:pe)*pftfrac(ps:pe) )
      hprl          = sum( hprl_p      (ps:pe)*pftfrac(ps:pe) )
      dheatl        = sum( dheatl_p    (ps:pe)*pftfrac(ps:pe) )
IF (DEF_USE_OZONESTRESS)THEN
      o3uptakesun   = sum(o3uptakesun_p(ps:pe)*pftfrac(ps:pe) )
      o3uptakesha   = sum(o3uptakesha_p(ps:pe)*pftfrac(ps:pe) )
END IF

      IF(DEF_USE_PLANTHYDRAULICS)THEN
         DO j = 1, nvegwcs
            vegwp(j) = sum( vegwp_p(j,ps:pe)*pftfrac(ps:pe) )
         ENDDO

         IF (abs(etr) > 0.) THEN
            DO j = 1, nl_soil
               rootflux(j) = sum(rootflux_p(j,ps:pe)*pftfrac(ps:pe))
            ENDDO
         ENDIF
      ELSE
         IF (abs(etr) > 0.) THEN
            DO j = 1, nl_soil
               rootr(j) = sum(rootr_p(j,ps:pe)*etr_p(ps:pe)*pftfrac(ps:pe)) / etr
            ENDDO
         ENDIF
      ENDIF

      deallocate ( rootflux_p  )
      deallocate ( etrc_p      )
      deallocate ( rstfac_p    )
      deallocate ( rstfacsun_p )
      deallocate ( rstfacsha_p )
      deallocate ( gssun_p     )
      deallocate ( gssha_p     )
      deallocate ( fsun_p      )
      deallocate ( sabv_p      )
      deallocate ( fcover      )

      deallocate ( fseng_soil_p)
      deallocate ( fseng_snow_p)
      deallocate ( fevpg_soil_p)
      deallocate ( fevpg_snow_p)
      deallocate ( cgrnd_p     )
      deallocate ( cgrnds_p    )
      deallocate ( cgrndl_p    )
      deallocate ( dlrad_p     )
      deallocate ( ulrad_p     )
      deallocate ( zol_p       )
      deallocate ( rib_p       )
      deallocate ( ustar_p     )
      deallocate ( qstar_p     )
      deallocate ( tstar_p     )
      deallocate ( fm_p        )
      deallocate ( fh_p        )
      deallocate ( fq_p        )
      deallocate ( fh2m_p      )
      deallocate ( fq2m_p      )
      deallocate ( fm10m_p     )

      deallocate ( hprl_p      )
      deallocate ( assimsun_p  )
      deallocate ( etrsun_p    )
      deallocate ( assimsha_p  )
      deallocate ( etrsha_p    )
      deallocate ( dheatl_p    )

ENDIF
#endif


!=======================================================================
! [5] Ground temperature
!=======================================================================

      CALL GroundTemperature (patchtype,is_dry_lake,lb,nl_soil,deltim,&
                      capr,cnfac,vf_quartz,vf_gravels,vf_om,vf_sand,wf_gravels,wf_sand,&
                      porsl,psi0,&
#ifdef Campbell_SOIL_MODEL
                      bsw,&
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
                      theta_r, alpha_vgm, n_vgm, L_vgm,&
                      sc_vgm , fc_vgm,&
#endif
                      csol,k_solids,dksatu,dksatf,dkdry,&
                      BA_alpha,BA_beta,&
                      sigf,dz_soisno,z_soisno,zi_soisno,&
                      t_soisno,t_grnd,t_soil,t_snow,wice_soisno,wliq_soisno,scv,snowdp,fsno,&
                      frl,dlrad,sabg,sabg_soil,sabg_snow,sabg_snow_lyr,&
                      fseng,fseng_soil,fseng_snow,fevpg,fevpg_soil,fevpg_snow,cgrnd,htvp,emg,&
                      imelt,snofrz,sm,xmf,fact,pg_rain,pg_snow,t_precip)

!=======================================================================
! [6] Correct fluxes to present soil temperature
!=======================================================================

      IF (.not.DEF_SPLIT_SOILSNOW) THEN
         t_grnd = t_soisno(lb)
         tinc   = t_soisno(lb) - t_soisno_bef(lb)
      ELSE
         t_grnd = fsno*t_soisno(lb) + (1.0-fsno)*t_soisno(1)
         tinc   = t_grnd - t_grnd_bef
      ENDIF

      fseng      = fseng      + tinc*cgrnds
      fseng_soil = fseng_soil + tinc*cgrnds
      fseng_snow = fseng_snow + tinc*cgrnds
      fevpg      = fevpg      + tinc*cgrndl
      fevpg_soil = fevpg_soil + tinc*cgrndl
      fevpg_snow = fevpg_snow + tinc*cgrndl

! calculation of evaporative potential; flux in kg m-2 s-1.
! egidif holds the excess energy IF all water is evaporated
! during the timestep. This energy is later added to the sensible heat flux.

      qseva = 0.
      qsubl = 0.
      qfros = 0.
      qsdew = 0.
      qseva_soil = 0.
      qsubl_soil = 0.
      qfros_soil = 0.
      qsdew_soil = 0.
      qseva_snow = 0.
      qsubl_snow = 0.
      qfros_snow = 0.
      qsdew_snow = 0.


IF (.not. DEF_SPLIT_SOILSNOW) THEN
      egsmax = (wice_soisno(lb)+wliq_soisno(lb)) / deltim
      egidif = max( 0., fevpg - egsmax )
      fevpg  = min( fevpg, egsmax )
      fseng  = fseng + htvp*egidif

      IF (fevpg >= 0.) THEN
! not allow for sublimation in melting (melting ==> evap. ==> sublimation)
         qseva = min(wliq_soisno(lb)/deltim, fevpg)
         qsubl = fevpg - qseva
      ELSE
         IF (t_grnd < tfrz) THEN
            qfros = abs(fevpg)
         ELSE
            qsdew = abs(fevpg)
         ENDIF
      ENDIF

ELSE
      IF (lb < 1) THEN   ! snow layer exist
         egsmax = (wice_soisno(lb)+wliq_soisno(lb)) / deltim
         egidif = max( 0., fevpg_snow - egsmax )
         fevpg_snow = min ( fevpg_snow, egsmax )
         fseng_snow = fseng_snow + htvp*egidif
      ELSE               ! no snow layer, attribute to soil
         fevpg_soil = fevpg_soil*(1.-fsno) + fevpg_snow*fsno
      ENDIF

      egsmax = (wice_soisno(1)+wliq_soisno(1)) / deltim
      egidif = max( 0., fevpg_soil - egsmax )
      fevpg_soil = min ( fevpg_soil, egsmax )
      fseng_soil = fseng_soil + htvp*egidif

      IF (lb < 1) THEN   ! snow layer exist
         fseng = fseng_soil*(1.-fsno) + fseng_snow*fsno
         fevpg = fevpg_soil*(1.-fsno) + fevpg_snow*fsno
      ELSE               ! no snow layer, attribute to soil
         fseng = fseng_soil; fseng_snow = 0.
         fevpg = fevpg_soil; fevpg_snow = 0.
      ENDIF

      IF(fevpg_snow >= 0.)THEN
! not allow for sublimation in melting (melting ==> evap. ==> sublimation)
         qseva_snow = min(wliq_soisno(lb)/deltim, fevpg_snow)
         qsubl_snow = fevpg_snow - qseva_snow
         qseva_snow = qseva_snow*fsno
         qsubl_snow = qsubl_snow*fsno
      ELSE
         ! snow temperature < tfrz
         IF(t_soisno(lb) < tfrz)THEN
            qfros_snow = abs(fevpg_snow*fsno)
         ELSE
            qsdew_snow = abs(fevpg_snow*fsno)
         ENDIF
      ENDIF

      IF(fevpg_soil >= 0.)THEN
! not allow for sublimation in melting (melting ==> evap. ==> sublimation)
         qseva_soil = min(wliq_soisno(1)/deltim, fevpg_soil)
         qsubl_soil = fevpg_soil - qseva_soil
      ELSE
         ! soil temperature < tfrz
         IF(t_soisno(1) < tfrz)THEN
            qfros_soil = abs(fevpg_soil)
         ELSE
            qsdew_soil = abs(fevpg_soil)
         ENDIF
      ENDIF

      IF (lb < 1) THEN ! snow layer exists
         qseva_soil = qseva_soil*(1.-fsno)
         qsubl_soil = qsubl_soil*(1.-fsno)
         qfros_soil = qfros_soil*(1.-fsno)
         qsdew_soil = qsdew_soil*(1.-fsno)
      ENDIF
ENDIF


! total fluxes to atmosphere
      fsena  = fsenl + fseng
      fevpa  = fevpl + fevpg
      lfevpa = hvap*fevpl + htvp*fevpg   ! W/m^2 (accounting for sublimation)

! ground heat flux
IF (.not.DEF_SPLIT_SOILSNOW) THEN
      fgrnd = sabg + dlrad*emg &
            - emg*stefnc*t_grnd_bef**4 &
            - emg*stefnc*t_grnd_bef**3*(4.*tinc) &
            - (fseng+fevpg*htvp) &
            + cpliq*pg_rain*(t_precip-t_grnd) &
            + cpice*pg_snow*(t_precip-t_grnd)
ELSE
      fgrnd = sabg + dlrad*emg &
            - fsno*emg*stefnc*t_snow**4 &
            - (1.-fsno)*emg*stefnc*t_soil**4 &
            - emg*stefnc*t_grnd_bef**3*(4.*tinc) &
            - (fseng+fevpg*htvp) &
            + cpliq*pg_rain*(t_precip-t_grnd) &
            + cpice*pg_snow*(t_precip-t_grnd)
ENDIF

! outgoing long-wave radiation from canopy + ground
      olrg = ulrad &
! for conservation we put the increase of ground longwave to outgoing
           + 4.*emg*stefnc*t_grnd_bef**3*tinc

! averaged bulk surface emissivity
      olrb = stefnc*t_grnd_bef**3*(4.*tinc)
      olru = ulrad + emg*olrb
      olrb = ulrad + olrb
      emis = olru / olrb

! radiative temperature
      IF (olrg < 0) THEN
         print *, "MOD_Thermal.F90: Error! Negative outgoing longwave radiation flux: "
         write(6,*) ipatch, olrg, tinc, ulrad
         write(6,*) ipatch,errore,sabv,sabg,frl,olrg,fsenl,fseng,hvap*fevpl,htvp*fevpg,xmf,fgrnd
      ENDIF

      trad = (olrg/stefnc)**0.25

! additional variables required by WRF and RSM model
      IF (lai+sai <= 1e-6) THEN
         ustar = ustar_g
         tstar = tstar_g
         qstar = qstar_g
         rib   = rib_g
         zol   = zol_g
         z0m   = z0m_g
         fm    = fm_g
         fh    = fh_g
         fq    = fq_g
      ENDIF


!=======================================================================
! [7] energy balance error
!=======================================================================

      ! one way to check energy balance
      errore = sabv + sabg + frl - olrg - fsena - lfevpa - fgrnd - dheatl + hprl &
             + cpliq*pg_rain*(t_precip-t_grnd) + cpice*pg_snow*(t_precip-t_grnd)

      ! another way to check energy balance
      errore = sabv + sabg + frl - olrg - fsena - lfevpa - xmf - dheatl + hprl &
             + cpliq*pg_rain*(t_precip-t_grnd) + cpice*pg_snow*(t_precip-t_grnd)

      DO j = lb, nl_soil
         errore = errore - (t_soisno(j)-t_soisno_bef(j))/fact(j)
      ENDDO

#if (defined CoLMDEBUG)
      IF (abs(errore) > .5) THEN
      write(6,*) 'MOD_Thermal.F90: energy balance violation'
      write(6,*) ipatch,errore,sabv,sabg,frl,olrg,fsenl,fseng,hvap*fevpl,htvp*fevpg,xmf,hprl
      write(6,*) cpliq*pg_rain*(t_precip-t_grnd), cpice*pg_snow*(t_precip-t_grnd)
      CALL CoLM_stop ()
      ENDIF
100   format(10(f15.3))
#endif

  END SUBROUTINE THERMAL

END MODULE MOD_Thermal
! ---------- EOP ------------
