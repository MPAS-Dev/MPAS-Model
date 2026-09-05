#include <define.h>

MODULE MOD_Urban_Hydrology
!-----------------------------------------------------------------------
!
! !DESCRIPTION:
!
!  The urban hydrological processes mainly falls into three categories:
!  1) previous surfaces; 2) roofs and imperious surfaces; 3) urban water
!  bodies (lakes).
!
!  For pervious surfaces, the process is similar to soil water
!  processes, involving the calculation of runoff and soil water
!  transport. For urban water bodies, a lake model is used for
!  simulation. For roofs and impermeable surfaces, snow accumulation and
!  ponding processes are considered. The snow accumulation process is
!  consistent with soil snow processes. The ponding process considers
!  the surface as an impermeable area, with the maximum capacity of
!  liquid water not exceeding a predetermined value (max ponding = 1 kg
!  m−2). Any excess water is treated as runoff. The coverage ratio of
!  ponded areas is calculated using a similar leaf wetness index
!  calculation scheme.
!
!  Create by Hua Yuan, 09/2021
!
! !REVISIONS:
!
!  10/2022, Hua Yuan: Add wet fraction for roof and impervious ground;
!           set max ponding for roof and impervious from 10mm -> 1mm.
!
!-----------------------------------------------------------------------
   USE MOD_Precision
   IMPLICIT NONE
   SAVE

   PUBLIC :: UrbanHydrology

CONTAINS

   SUBROUTINE UrbanHydrology ( &
        ! model running information
        ipatch         ,patchtype      ,lbr            ,lbi            ,&
        lbp            ,lbl            ,snll           ,deltim         ,&
        ! forcing
        pg_rain        ,pgper_rain     ,pgimp_rain     ,pg_snow        ,&
        pg_rain_lake   ,pg_snow_lake                                   ,&
        ! surface parameters or status
        froof          ,fgper          ,flake          ,bsw            ,&
        porsl          ,psi0           ,hksati         ,pondmx         ,&
        ssi            ,wimp           ,smpmin         ,theta_r        ,&
        fsatmax        ,fsatdcf        ,elvstd         ,BVIC           ,&
        rootr,rootflux ,etr            ,fseng          ,fgrnd          ,&
        t_gpersno      ,t_lakesno      ,t_lake         ,dz_lake        ,&
        z_gpersno      ,z_lakesno      ,zi_gpersno     ,zi_lakesno     ,&
        dz_roofsno     ,dz_gimpsno     ,dz_gpersno     ,dz_lakesno     ,&
        wliq_roofsno   ,wliq_gimpsno   ,wliq_gpersno   ,wliq_lakesno   ,&
        wice_roofsno   ,wice_gimpsno   ,wice_gpersno   ,wice_lakesno   ,&
        qseva_roof     ,qseva_gimp     ,qseva_gper     ,qseva_lake     ,&
        qsdew_roof     ,qsdew_gimp     ,qsdew_gper     ,qsdew_lake     ,&
        qsubl_roof     ,qsubl_gimp     ,qsubl_gper     ,qsubl_lake     ,&
        qfros_roof     ,qfros_gimp     ,qfros_gper     ,qfros_lake     ,&
        sm_roof        ,sm_gimp        ,sm_gper        ,sm_lake        ,&
        lake_icefrac   ,scv_lake       ,snowdp_lake    ,imelt_lake     ,&
        fioldl         ,w_old                                          ,&
        forc_us        ,forc_vs                                        ,&
! SNICAR model variables
        forc_aer                                                       ,&
        mss_bcpho      ,mss_bcphi      ,mss_ocpho      ,mss_ocphi      ,&
        mss_dst1       ,mss_dst2       ,mss_dst3       ,mss_dst4       ,&
! END SNICAR model variables
!  irrigaiton
        qflx_irrig_drip,qflx_irrig_flood,qflx_irrig_paddy              ,&
        ! output
        rsur           ,rnof           ,qinfl          ,zwt            ,&
        wdsrf          ,wa             ,qcharge        ,smp            ,hk)

!=======================================================================
! this is the main SUBROUTINE to execute the calculation of URBAN
! hydrological processes
!
!=======================================================================

   USE MOD_Precision
   USE MOD_Vars_Global
   USE MOD_Const_Physical, only: denice, denh2o, tfrz
   USE MOD_SoilSnowHydrology
   USE MOD_Lake

   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   integer, intent(in) :: &
        ipatch             ,&! patch index
        patchtype          ,&! land patch type (0=soil, 1=urban or built-up, 2=wetland,
                             ! 3=land ice, 4=land water bodies, 99=ocean
        lbr                ,&! lower bound of array
        lbi                ,&! lower bound of array
        lbp                ,&! lower bound of array
        lbl                  ! lower bound of array

   integer, intent(inout) :: &
        snll                 ! number of snow layers

   real(r8), intent(in) :: &
        deltim             ,&! time step (s)
        pg_rain            ,&! rainfall after removal of interception (mm h2o/s)
        pg_snow            ,&! snowfall after removal of interception (mm h2o/s)
        pgper_rain         ,&! rainfall after removal of interception (mm h2o/s)
        pgimp_rain         ,&! rainfall after removal of interception (mm h2o/s)
        pg_rain_lake       ,&! rainfall onto lake (mm h2o/s)
        pg_snow_lake       ,&! snowfall onto lake (mm h2o/s)
        froof              ,&! roof fractional cover [-]
        fgper              ,&! weight of impervious ground [-]
        flake              ,&! lake fractional cover [-]
        ! wtfact           ,&! fraction of model area with high water table
                             ! (updated to gridded 'fsatmax' data)
        pondmx             ,&! ponding depth (mm)
        ssi                ,&! irreducible water saturation of snow
        wimp               ,&! water impermeable IF porosity less than wimp
        smpmin             ,&! restriction for min of soil poten. (mm)

        elvstd             ,&! standard deviation of elevation [m]
        BVIC               ,&! b parameter in Fraction of saturated soil in a grid calculated by VIC

        bsw   (1:nl_soil)  ,&! Clapp-Hornberger "B"
        porsl (1:nl_soil)  ,&! saturated volumetric soil water content(porosity)
        psi0  (1:nl_soil)  ,&! saturated soil suction (mm) (NEGATIVE)
        hksati(1:nl_soil)  ,&! hydraulic conductivity at saturation (mm h2o/s)
        theta_r(1:nl_soil) ,&! residual moisture content [-]
        fsatmax            ,&! maximum saturated area fraction [-]
        fsatdcf            ,&! decay factor in calculation of saturated area fraction [1/m]
        rootr (1:nl_soil)  ,&! root resistance of a layer, all layers add to 1.0

        etr                ,&! vegetation transpiration
        qseva_roof         ,&! ground surface evaporation rate (mm h2o/s)
        qseva_gimp         ,&! ground surface evaporation rate (mm h2o/s)
        qseva_gper         ,&! ground surface evaporation rate (mm h2o/s)
        qseva_lake         ,&! ground surface evaporation rate (mm h2o/s)
        qsdew_roof         ,&! ground surface dew formation (mm h2o /s) [+]
        qsdew_gimp         ,&! ground surface dew formation (mm h2o /s) [+]
        qsdew_gper         ,&! ground surface dew formation (mm h2o /s) [+]
        qsdew_lake         ,&! ground surface dew formation (mm h2o /s) [+]
        qsubl_roof         ,&! sublimation rate from snow pack (mm h2o /s) [+]
        qsubl_gimp         ,&! sublimation rate from snow pack (mm h2o /s) [+]
        qsubl_gper         ,&! sublimation rate from snow pack (mm h2o /s) [+]
        qsubl_lake         ,&! sublimation rate from snow pack (mm h2o /s) [+]
        qfros_roof         ,&! surface dew added to snow pack (mm h2o /s) [+]
        qfros_gimp         ,&! surface dew added to snow pack (mm h2o /s) [+]
        qfros_gper         ,&! surface dew added to snow pack (mm h2o /s) [+]
        qfros_lake         ,&! surface dew added to snow pack (mm h2o /s) [+]
        sm_roof            ,&! snow melt (mm h2o/s)
        sm_gimp            ,&! snow melt (mm h2o/s)
        sm_gper            ,&! snow melt (mm h2o/s)
        w_old                ! liquid water mass of the column at the previous time step (mm)

   real(r8), intent(inout) :: rootflux(1:nl_soil)

   real(r8), intent(in) :: forc_us
   real(r8), intent(in) :: forc_vs

! SNICAR model variables
! Aerosol Fluxes (Jan. 07, 2023)
   ! aerosol deposition from atmosphere model (grd,aer) [kg m-1 s-1]
   real(r8), intent(in) :: forc_aer (14)

   real(r8), intent(inout) :: &
        mss_bcpho (lbp:0)             ,&! mass of hydrophobic BC in snow  (col,lyr) [kg]
        mss_bcphi (lbp:0)             ,&! mass of hydrophillic BC in snow (col,lyr) [kg]
        mss_ocpho (lbp:0)             ,&! mass of hydrophobic OC in snow  (col,lyr) [kg]
        mss_ocphi (lbp:0)             ,&! mass of hydrophillic OC in snow (col,lyr) [kg]
        mss_dst1  (lbp:0)             ,&! mass of dust species 1 in snow  (col,lyr) [kg]
        mss_dst2  (lbp:0)             ,&! mass of dust species 2 in snow  (col,lyr) [kg]
        mss_dst3  (lbp:0)             ,&! mass of dust species 3 in snow  (col,lyr) [kg]
        mss_dst4  (lbp:0)               ! mass of dust species 4 in snow  (col,lyr) [kg]
! Aerosol Fluxes (Jan. 07, 2023)
! END SNICAR model variables

!  For irrigation
   real(r8), intent(in) :: &
        qflx_irrig_drip               ,&! drip irrigation rate [mm/s]
        qflx_irrig_flood              ,&! flood irrigation rate [mm/s]
        qflx_irrig_paddy                ! paddy irrigation rate [mm/s]
!  END irrigation

   integer, intent(in) :: &
        imelt_lake(maxsnl+1:nl_soil)    ! lake flag for melting or freezing snow and soil layer [-]

   real(r8), intent(inout) :: &
        lake_icefrac(  1:nl_lake)     ,&! lake ice fraction
        fioldl (maxsnl+1:nl_soil)     ,&! fraction of ice relative to the total water content [-]
        dz_lake     (  1:nl_lake)     ,&! lake layer depth [m]
        z_gpersno   (lbp:nl_soil)     ,&! layer depth (m)
        dz_roofsno  (lbr:nl_roof)     ,&! layer thickness (m)
        dz_gimpsno  (lbi:nl_soil)     ,&! layer thickness (m)
        dz_gpersno  (lbp:nl_soil)     ,&! layer thickness (m)
        zi_gpersno(lbp-1:nl_soil)     ,&! interface level below a "z" level (m)
        t_lake      (  1:nl_lake)     ,&! lake temperature [K]
        t_gpersno   (lbp:nl_soil)     ,&! soil/snow skin temperature (K)
        wliq_roofsno(lbr:nl_roof)     ,&! liquid water (kg/m2)
        wliq_gimpsno(lbi:nl_soil)     ,&! liquid water (kg/m2)
        wliq_gpersno(lbp:nl_soil)     ,&! liquid water (kg/m2)
        wice_roofsno(lbr:nl_roof)     ,&! ice lens (kg/m2)
        wice_gimpsno(lbi:nl_soil)     ,&! ice lens (kg/m2)
        wice_gpersno(lbp:nl_soil)     ,&! ice lens (kg/m2)

        zi_lakesno  (maxsnl  :nl_soil),&! interface level below a "z" level (m)
        t_lakesno   (maxsnl+1:nl_soil),&! soil/snow skin temperature (K)
        z_lakesno   (maxsnl+1:nl_soil),&! layer depth (m)
        dz_lakesno  (maxsnl+1:nl_soil),&! layer thickness (m)
        wliq_lakesno(maxsnl+1:nl_soil),&! liquid water (kg/m2)
        wice_lakesno(maxsnl+1:nl_soil),&! ice lens (kg/m2)

        sm_lake          ,&! snow melt (mm h2o/s)
        scv_lake         ,&! lake snow mass (kg/m2)
        snowdp_lake      ,&! lake snow depth
        fseng            ,&! sensible heat from ground
        fgrnd            ,&! ground heat flux
        zwt              ,&! the depth from ground (soil) surface to water table [m]
        wdsrf            ,&! depth of surface water [mm]
        wa                 ! water storage in aquifer [mm]

   real(r8), intent(out) :: &
        rsur             ,&! surface runoff (mm h2o/s)
        rnof             ,&! total runoff (mm h2o/s)
        qinfl            ,&! infiltration rate (mm h2o/s)
        qcharge            ! groundwater recharge (positive to aquifer) [mm/s]

   real(r8), intent(out) :: &
        smp(1:nl_soil)   ,&! soil matrix potential [mm]
        hk (1:nl_soil)     ! hydraulic conductivity [mm h2o/m]

!-------------------------- Local Variables ----------------------------

   real(r8) :: &
        fg               ,&! ground fractional cover [-]
        gwat             ,&! net water input from top (mm/s)
        rnof_roof        ,&! total runoff (mm h2o/s)
        rnof_gimp        ,&! total runoff (mm h2o/s)
        rnof_gper        ,&! total runoff (mm h2o/s)
        rnof_lake        ,&! total runoff (mm h2o/s)
        rsur_roof        ,&! surface runoff (mm h2o/s)
        rsur_gimp        ,&! surface runoff (mm h2o/s)
        rsur_gper        ,&! surface runoff (mm h2o/s)
        rsur_lake        ,&! surface runoff (mm h2o/s)
        dfseng           ,&! change of lake sensible heat [W/m2]
        dfgrnd             ! change of lake ground heat flux [W/m2]

   real(r8) :: a, aa, xs1

!-----------------------------------------------------------------------

      fg = 1 - froof
      dfseng = 0.
      dfgrnd = 0.

!=======================================================================
! [1] for pervious road, the same as soil
!=======================================================================

      rootflux(:) = rootr(:)*etr

      CALL WATER_2014 (ipatch,patchtype,lbp        ,nl_soil     ,deltim      ,&
             z_gpersno   ,dz_gpersno  ,zi_gpersno  ,bsw         ,porsl       ,&
             psi0        ,hksati      ,theta_r     ,fsatmax     ,fsatdcf     ,&
             elvstd      ,BVIC        ,rootr       ,rootflux    ,t_gpersno   ,&
             wliq_gpersno,wice_gpersno,smp         ,hk          ,pgper_rain  ,&
             sm_gper     ,etr         ,qseva_gper  ,qsdew_gper  ,qsubl_gper  ,&
             qfros_gper                                                      ,&
             !NOTE: temporal input, as urban mode doesn't support split soil&snow
             ! set all the same for soil and snow surface,
             ! and fsno=0. (no physical meaning here)
             qseva_gper  ,qsdew_gper  ,qsubl_gper  ,qfros_gper               ,&
             qseva_gper  ,qsdew_gper  ,qsubl_gper  ,qfros_gper               ,&
             0.          ,& ! fsno, not active
             rsur_gper   ,rnof_gper   ,qinfl       ,pondmx      ,ssi         ,&
             wimp        ,smpmin      ,zwt         ,wdsrf       ,wa          ,qcharge     ,&
! SNICAR model variables
             forc_aer                                                        ,&
             mss_bcpho   ,mss_bcphi   ,mss_ocpho   ,mss_ocphi                ,&
             mss_dst1    ,mss_dst2    ,mss_dst3    ,mss_dst4                 ,&
!  irrigation variables
             qflx_irrig_drip   ,qflx_irrig_flood  ,qflx_irrig_paddy            )

!=======================================================================
! [2] for roof and impervious road
!=======================================================================

      IF (lbr >= 1) THEN
         gwat = pg_rain + sm_roof - qseva_roof
      ELSE
         CALL snowwater (lbr,deltim,ssi,wimp,&
                         pg_rain,qseva_roof,qsdew_roof,qsubl_roof,qfros_roof,&
                         dz_roofsno(lbr:0),wice_roofsno(lbr:0),wliq_roofsno(lbr:0),gwat)
      ENDIF

      wliq_roofsno(1) = wliq_roofsno(1) + gwat*deltim

      ! Renew the ice and liquid mass due to condensation
      IF (lbr >= 1) THEN
         ! make consistent with how evap_grnd removed in infiltration
         wliq_roofsno(1) = max(0., wliq_roofsno(1) + qsdew_roof * deltim)
         wice_roofsno(1) = max(0., wice_roofsno(1) + (qfros_roof-qsubl_roof) * deltim)
      ENDIF

      ! only consider ponding and surface runoff
      ! NOTE: set max ponding depth = 1mm (liq+ice)
      xs1 = wliq_roofsno(1) - 1.
      IF (xs1 > 0.) THEN
         wliq_roofsno(1) = 1.
      ELSE
         xs1 = 0.
      ENDIF

      rsur_roof = xs1 / deltim
      rnof_roof = rsur_roof

      ! ================================================

      IF (lbi >= 1) THEN
         gwat = pgimp_rain + sm_gimp - qseva_gimp
      ELSE
         CALL snowwater (lbi,deltim,ssi,wimp,&
                         pgimp_rain,qseva_gimp,qsdew_gimp,qsubl_gimp,qfros_gimp,&
                         dz_gimpsno(lbi:0),wice_gimpsno(lbi:0),wliq_gimpsno(lbi:0),gwat)
      ENDIF

      wliq_gimpsno(1) = wliq_gimpsno(1) + gwat*deltim

      ! Renew the ice and liquid mass due to condensation
      IF (lbi >= 1) THEN
         ! make consistent with how evap_grnd removed in infiltration
         wliq_gimpsno(1) = max(0., wliq_gimpsno(1) + qsdew_gimp * deltim)
         wice_gimpsno(1) = max(0., wice_gimpsno(1) + (qfros_gimp-qsubl_gimp) * deltim)
      ENDIF

      ! only consider ponding and surface runoff
      ! NOTE: set max ponding depth = 1mm
      xs1 = wliq_gimpsno(1) - 1.
      IF (xs1 > 0.) THEN
         wliq_gimpsno(1) = 1.
      ELSE
         xs1 = 0.
      ENDIF

      rsur_gimp = xs1 / deltim
      rnof_gimp = rsur_gimp

!=======================================================================
! [3] lake hydrology
!=======================================================================

      CALL snowwater_lake ( DEF_USE_Dynamic_Lake, &
           ! "in" snowater_lake arguments
           ! ---------------------------
           maxsnl         ,nl_soil        ,nl_lake        ,deltim         ,&
           ssi            ,wimp           ,porsl          ,pg_rain_lake   ,&
           pg_snow_lake   ,dz_lake        ,imelt_lake(:0) ,fioldl(:0)     ,&
           qseva_lake     ,qsubl_lake     ,qsdew_lake     ,qfros_lake     ,&

           ! "inout" snowater_lake arguments
           ! ---------------------------
           z_lakesno      ,dz_lakesno     ,zi_lakesno     ,t_lakesno      ,&
           wice_lakesno   ,wliq_lakesno   ,t_lake         ,lake_icefrac   ,&
           gwat                                                           ,&
           dfseng         ,dfgrnd         ,snll           ,scv_lake       ,&
           snowdp_lake    ,sm_lake        ,forc_us        ,forc_vs        ,&
! SNICAR model variables
           forc_aer                                                       ,&
           mss_bcpho      ,mss_bcphi      ,mss_ocpho      ,mss_ocphi      ,&
           mss_dst1       ,mss_dst2       ,mss_dst3       ,mss_dst4       ,&
! END SNICAR model variables
           urban_call=.true.)

      ! We assume the land water bodies have zero extra liquid water capacity
      ! (i.e.,constant capacity), all excess liquid water are put into the runoff,
      ! this unreasonable assumption should be updated in the future version
      a  = (sum(wliq_lakesno(snll+1:))-w_old)/deltim
      aa = qseva_lake-(qsubl_lake-qsdew_lake)
      rsur_lake = max(0., pg_rain_lake - aa - a)
      rnof_lake = rsur_lake

      ! Set zero to the empty node
      IF (snll > maxsnl) THEN
         wice_lakesno(maxsnl+1:snll) = 0.
         wliq_lakesno(maxsnl+1:snll) = 0.
         t_lakesno   (maxsnl+1:snll) = 0.
         z_lakesno   (maxsnl+1:snll) = 0.
         dz_lakesno  (maxsnl+1:snll) = 0.
      ENDIF

      fseng = fseng + dfseng*flake
      fgrnd = fgrnd + dfgrnd*flake

!=======================================================================
! [4] surface and total runoff weighted by fractional coverages
!=======================================================================

      ! 10/01/2021, yuan: exclude lake part
      rsur = rsur_roof*froof + rsur_gimp*fg*(1-fgper) + rsur_gper*fg*fgper
      !rsur = rsur*(1.-flake) + rsur_lake*flake
      rnof = rnof_roof*froof + rnof_gimp*fg*(1-fgper) + rnof_gper*fg*fgper
      !rnof = rnof*(1.-flake) + rnof_lake*flake

   END SUBROUTINE UrbanHydrology

END MODULE MOD_Urban_Hydrology
! ---------- EOP ------------
