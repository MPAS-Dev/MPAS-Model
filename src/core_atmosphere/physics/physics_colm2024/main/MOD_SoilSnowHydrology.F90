#include <define.h>

MODULE MOD_SoilSnowHydrology

!-----------------------------------------------------------------------
   USE MOD_Precision
   USE MOD_Namelist, only: DEF_USE_PLANTHYDRAULICS, DEF_USE_SNICAR,        &
                           DEF_URBAN_RUN,           DEF_USE_IRRIGATION,    &
                           DEF_SPLIT_SOILSNOW,      DEF_Runoff_SCHEME,     &
                           DEF_DA_TWS_GRACE,        DEF_Optimize_Baseflow, &
                           DEF_USE_Dynamic_Wetland
   USE MOD_LandPatch, only: landpatch
   USE MOD_MPAS_MPI, only: CoLM_stop
   USE MOD_Runoff
   USE MOD_Hydro_VIC
   USE MOD_Hydro_VIC_Variables
   IMPLICIT NONE
   SAVE

! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: WATER_2014
   PUBLIC :: WATER_VSF
   PUBLIC :: snowwater
   PUBLIC :: soilwater
   PUBLIC :: snowwater_snicar


! PRIVATE MEMBER FUNCTIONS:
   PRIVATE :: groundwater


!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------



   SUBROUTINE WATER_2014 (ipatch,patchtype,lb       ,nl_soil     ,deltim      ,&
              z_soisno    ,dz_soisno   ,zi_soisno   ,bsw         ,porsl       ,&
              psi0        ,hksati      ,theta_r     ,fsatmax     ,fsatdcf     ,&
              elvstd      ,BVIC        ,rootr       ,rootflux    ,t_soisno    ,&
              wliq_soisno ,wice_soisno ,smp         ,hk          ,pg_rain     ,&
              sm          ,etr         ,qseva       ,qsdew       ,qsubl       ,&
              qfros       ,qseva_soil  ,qsdew_soil  ,qsubl_soil  ,qfros_soil  ,&
              qseva_snow  ,qsdew_snow  ,qsubl_snow  ,qfros_snow  ,fsno        ,&
              rsur        ,rnof        ,qinfl       ,pondmx      ,ssi         ,&
              wimp        ,smpmin      ,zwt         ,wdsrf       ,wa          ,&
              qcharge                                                         ,&
! SNICAR model variables
              forc_aer                                                        ,&
              mss_bcpho   ,mss_bcphi   ,mss_ocpho   ,mss_ocphi                ,&
              mss_dst1    ,mss_dst2    ,mss_dst3    ,mss_dst4                 ,&
              qflx_irrig_drip  ,qflx_irrig_flood ,qflx_irrig_paddy               )

!=======================================================================
!  this is the main SUBROUTINE to execute the calculation of
!  hydrological processes
!
!  Original author: Yongjiu Dai, /09/1999/, /08/2002/, /04/2014/
!
!  FLOW DIAGRAM FOR WATER_2014.F90
!
!  WATER_2014 ===> snowwater
!                  SurfaceRunoff_TOPMOD
!                  soilwater
!                  SubsurfaceRunoff_TOPMOD
!
!=======================================================================

   USE MOD_Precision
   USE MOD_Const_Physical,      only: denice, denh2o, tfrz
   USE MOD_Vars_TimeInvariants, only: vic_b_infilt, vic_Dsmax, vic_Ds, vic_Ws, vic_c
   USE MOD_Vars_1DFluxes,       only: fevpg
#ifdef CROP
   USE MOD_Vars_Global, only : irrig_method_paddy, pondmxc
   use MOD_LandPFT, only : patch_pft_s, patch_pft_e
   use MOD_Vars_PFTimeVariables, only: irrig_method_p
#endif

   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   integer, intent(in) :: &
        ipatch                  ,&! patch index
        patchtype                 ! land patch type (0=soil, 1=urban or built-up, 2=wetland,
                                  ! 3=land ice, 4=land water bodies, 99=ocean

   integer, intent(in) :: &
        lb                      ,&! lower bound of array
        nl_soil                   ! upper bound of array

   real(r8), intent(in) :: &
        deltim                  ,&! time step (s)
        ! wtfact                ,&! (updated to gridded 'fsatmax' data) fraction of model area with high water table
        pondmx                  ,&! ponding depth (mm)
        ssi                     ,&! irreducible water saturation of snow
        wimp                    ,&! water impermeable if porosity less than wimp
        smpmin                  ,&! restriction for min of soil poten. (mm)
        elvstd                  ,&! standard deviation of elevation (m)
        BVIC                    ,&!

        z_soisno (lb:nl_soil)   ,&! layer depth (m)
        dz_soisno(lb:nl_soil)   ,&! layer thickness (m)
        zi_soisno(lb-1:nl_soil) ,&! interface level below a "z" level (m)

        bsw(1:nl_soil)          ,&! Clapp-Hornberger "B"
        porsl(1:nl_soil)        ,&! saturated volumetric soil water content(porosity)
        psi0(1:nl_soil)         ,&! saturated soil suction (mm) (NEGATIVE)
        hksati(1:nl_soil)       ,&! hydraulic conductivity at saturation (mm h2o/s)
        theta_r(1:nl_soil)      ,&! residual moisture content [-]
        fsatmax                 ,&! maximum saturated area fraction [-]
        fsatdcf                 ,&! decay factor in calculation of saturated area fraction [1/m]
        rootr(1:nl_soil)        ,&! water uptake fraction from different layers, all layers add to 1.0
        rootflux(1:nl_soil)     ,&! root uptake from different layer, all layers add to transpiration

        t_soisno(lb:nl_soil)    ,&! soil/snow skin temperature (K)
        pg_rain                 ,&! rainfall after removal of interception (mm h2o/s)
        sm                      ,&! snow melt (mm h2o/s)
        etr                     ,&! actual transpiration (mm h2o/s)
        qseva                   ,&! ground surface evaporation rate (mm h2o/s)
        qsdew                   ,&! ground surface dew formation (mm h2o /s) [+]
        qsubl                   ,&! sublimation rate from snow pack (mm h2o /s) [+]
        qfros                   ,&! surface dew added to snow pack (mm h2o /s) [+]
        qseva_soil              ,&! ground soil surface evaporation rate (mm h2o/s)
        qsdew_soil              ,&! ground soil surface dew formation (mm h2o /s) [+]
        qsubl_soil              ,&! sublimation rate from soil ice pack (mm h2o /s) [+]
        qfros_soil              ,&! surface dew added to soil ice pack (mm h2o /s) [+]
        qseva_snow              ,&! ground snow surface evaporation rate (mm h2o/s)
        qsdew_snow              ,&! ground snow surface dew formation (mm h2o /s) [+]
        qsubl_snow              ,&! sublimation rate from snow pack (mm h2o /s) [+]
        qfros_snow              ,&! surface dew added to snow pack (mm h2o /s) [+]
        fsno                      ! snow fractional cover

   real(r8), intent(inout) :: &
        wice_soisno(lb:nl_soil) ,&! ice lens (kg/m2)
        wliq_soisno(lb:nl_soil)   ! liquid water (kg/m2)

   real(r8), intent(out) :: &
        smp(1:nl_soil)          ,&! soil matrix potential [mm]
        hk (1:nl_soil)            ! hydraulic conductivity [mm h2o/m]

   real(r8), intent(inout) :: &
        zwt                     ,&! the depth from ground (soil) surface to water table [m]
        wdsrf                   ,&! depth of surface water [mm]
        wa                        ! water storage in aquifer [mm]

   real(r8), intent(out) :: &
        rsur                    ,&! surface runoff (mm h2o/s)
        rnof                    ,&! total runoff (mm h2o/s)
        qinfl                   ,&! infiltration rate (mm h2o/s)
        qcharge                   ! groundwater recharge (positive to aquifer) [mm/s]

! SNICAR model variables
! Aerosol Fluxes (Jan. 07, 2023)
   real(r8), intent(in) :: forc_aer ( 14 )  ! aerosol deposition from atmosphere model (grd,aer) [kg m-1 s-1]

   real(r8), intent(inout) :: &
        mss_bcpho (lb:0)        ,&! mass of hydrophobic BC in snow  (col,lyr) [kg]
        mss_bcphi (lb:0)        ,&! mass of hydrophillic BC in snow (col,lyr) [kg]
        mss_ocpho (lb:0)        ,&! mass of hydrophobic OC in snow  (col,lyr) [kg]
        mss_ocphi (lb:0)        ,&! mass of hydrophillic OC in snow (col,lyr) [kg]
        mss_dst1  (lb:0)        ,&! mass of dust species 1 in snow  (col,lyr) [kg]
        mss_dst2  (lb:0)        ,&! mass of dust species 2 in snow  (col,lyr) [kg]
        mss_dst3  (lb:0)        ,&! mass of dust species 3 in snow  (col,lyr) [kg]
        mss_dst4  (lb:0)          ! mass of dust species 4 in snow  (col,lyr) [kg]
! Aerosol Fluxes (Jan. 07, 2023)
! END SNICAR model variables

!  irrigation variable
   real(r8), intent(in) :: &
        qflx_irrig_drip  , &! irrigation flux from drip irrigation [mm/s]
        qflx_irrig_flood , &! irrigation flux from flood irrigation [mm/s]
        qflx_irrig_paddy    ! irrigation flux from paddy irrigation [mm/s]

!-------------------------- Local Variables ----------------------------

   integer j                      ! loop counter

   real(r8) :: &
       eff_porosity(1:nl_soil)  ,&! effective porosity = porosity - vol_ice
       dwat(1:nl_soil)          ,&! change in soil water
       gwat                     ,&! net water input from top (mm/s)
       rsubst                   ,&! subsurface runoff (mm h2o/s)
       vol_liq(1:nl_soil)       ,&! partial volume of liquid water in layer
       vol_ice(1:nl_soil)       ,&! partial volume of ice lens in layer
       icefrac(1:nl_soil)       ,&! ice fraction (-)
       zmm (1:nl_soil)          ,&! layer depth (mm)
       dzmm(1:nl_soil)          ,&! layer thickness (mm)
       zimm(0:nl_soil)            ! interface level below a "z" level (mm)

   real(r8) :: err_solver, w_sum
   real(r8) :: gwat_prev
   integer  :: ps, pe, m

   real(r8) :: wliq_soisno_tmp(1:nl_soil)

!=======================================================================
! [1] update the liquid water within snow layer and the water onto soil
!=======================================================================


IF ((.not.DEF_SPLIT_SOILSNOW) .or. (patchtype==1 .and. DEF_URBAN_RUN)) THEN

      IF (lb>=1)THEN
         gwat = pg_rain + sm - qseva
      ELSE
         IF ((.not.DEF_USE_SNICAR) .or. (patchtype==1 .and. DEF_URBAN_RUN)) THEN
            CALL snowwater (lb,deltim,ssi,wimp,&
                         pg_rain,qseva,qsdew,qsubl,qfros,&
                         dz_soisno(lb:0),wice_soisno(lb:0),wliq_soisno(lb:0),gwat)
         ELSE
            CALL snowwater_snicar (lb,deltim,ssi,wimp,&
                         pg_rain,qseva,qsdew,qsubl,qfros,&
                         dz_soisno(lb:0),wice_soisno(lb:0),wliq_soisno(lb:0),gwat,&
                         forc_aer,&
                         mss_bcpho(lb:0), mss_bcphi(lb:0), mss_ocpho(lb:0), mss_ocphi(lb:0),&
                         mss_dst1(lb:0), mss_dst2(lb:0), mss_dst3(lb:0), mss_dst4(lb:0) )
         ENDIF
      ENDIF

ELSE

      IF (lb>=1)THEN
         gwat = pg_rain + sm - qseva_soil
      ELSE
         IF (.not. DEF_USE_SNICAR) THEN
            CALL snowwater (lb,deltim,ssi,wimp,&
                         pg_rain*fsno,qseva_snow,qsdew_snow,qsubl_snow,qfros_snow,&
                         dz_soisno(lb:0),wice_soisno(lb:0),wliq_soisno(lb:0),gwat)
         ELSE
            CALL snowwater_snicar (lb,deltim,ssi,wimp,&
                         pg_rain*fsno,qseva_snow,qsdew_snow,qsubl_snow,qfros_snow,&
                         dz_soisno(lb:0),wice_soisno(lb:0),wliq_soisno(lb:0),gwat,&
                         forc_aer,&
                         mss_bcpho(lb:0), mss_bcphi(lb:0), mss_ocpho(lb:0), mss_ocphi(lb:0),&
                         mss_dst1(lb:0), mss_dst2(lb:0), mss_dst3(lb:0), mss_dst4(lb:0) )
         ENDIF
         gwat = gwat + pg_rain*(1-fsno) - qseva_soil
      ENDIF
ENDIF

#ifdef CROP
      IF(DEF_USE_IRRIGATION)THEN
         gwat = gwat + qflx_irrig_drip + qflx_irrig_flood + qflx_irrig_paddy
         gwat = gwat + wdsrf/deltim
      ENDIF
#endif

!=======================================================================
! [2] surface runoff and infiltration
!=======================================================================

IF(patchtype<=1)THEN   ! soil ground only

      ! For water balance check, the sum of water in soil column before the calculation
      w_sum = sum(wliq_soisno(1:)) + sum(wice_soisno(1:)) + wa

      ! porosity of soil, partial volume of ice and liquid
      DO j = 1, nl_soil
         vol_ice(j) = min(porsl(j), wice_soisno(j)/(dz_soisno(j)*denice))
         eff_porosity(j) =  max(0.01, porsl(j)-vol_ice(j))
         vol_liq(j) = min(eff_porosity(j), wliq_soisno(j)/(dz_soisno(j)*denh2o))
         IF(porsl(j) < 1.e-6)THEN
            icefrac(j) = 0.
         ELSE
            icefrac(j) = min(1.,vol_ice(j)/porsl(j))
         ENDIF
      ENDDO

      ! surface runoff including water table and surface saturated area

      rsur   = 0.
      rsubst = 0.

      IF (DEF_Runoff_SCHEME  == 0) THEN
         ! 0: runoff scheme from TOPMODEL

         IF (gwat > 0.) THEN
            CALL SurfaceRunoff_TOPMOD (nl_soil,wimp,porsl,psi0,hksati,fsatmax,fsatdcf,&
               z_soisno(1:),dz_soisno(1:),zi_soisno(0:),&
               eff_porosity,icefrac,zwt,gwat,rsur)
         ELSE
            rsur = 0.
         ENDIF

      ELSEIF (DEF_Runoff_SCHEME  == 1) THEN
         ! 1: runoff scheme from VIC model

         wliq_soisno_tmp(:) = 0
         CALL Runoff_VIC(deltim, porsl, theta_r, hksati, bsw, &
                         wice_soisno(1:nl_soil), wliq_soisno(1:nl_soil), fevpg(ipatch), rootflux, gwat, &
                         vic_b_infilt(ipatch), vic_Dsmax(ipatch), vic_Ds(ipatch), vic_Ws(ipatch), vic_c(ipatch),&
                         rsur, rsubst, wliq_soisno_tmp(1:nl_soil))

      ELSEIF (DEF_Runoff_SCHEME  == 2) THEN
         ! 2: runoff scheme from XinAnJiang model

         CALL Runoff_XinAnJiang (&
            nl_soil, dz_soisno(1:nl_soil), eff_porosity(1:nl_soil), vol_liq(1:nl_soil), &
            elvstd, gwat, deltim, rsur, rsubst)

      ELSEIF (DEF_Runoff_SCHEME  == 3) THEN
         ! 3: runoff scheme from Simple VIC model
         CALL Runoff_SimpleVIC (&
            nl_soil, dz_soisno(1:nl_soil), eff_porosity(1:nl_soil), vol_liq(1:nl_soil), &
            BVIC, gwat, deltim, rsur, rsubst)

      ENDIF
#ifdef CROP
      IF(patchtype==0)THEN
         IF(DEF_USE_IRRIGATION)THEN
            ps = patch_pft_s(ipatch)
            pe = patch_pft_e(ipatch)
            DO m = ps, pe
               IF(irrig_method_p(m) == irrig_method_paddy)THEN
                  wdsrf = rsur*deltim
                  rsur = 0.
                  IF(wdsrf.gt.pondmxc)THEN
                     wdsrf = pondmxc
                     rsur = rsur + (wdsrf - pondmxc)/deltim
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
      ENDIF
#endif
      ! infiltration into surface soil layer
      qinfl = gwat - rsur - wdsrf/deltim

!=======================================================================
! [3] determine the change of soil water
!=======================================================================

      ! convert length units from m to mm
      zmm(1:) = z_soisno(1:)*1000.
      dzmm(1:) = dz_soisno(1:)*1000.
      zimm(0:) = zi_soisno(0:)*1000.

      CALL soilwater(patchtype,nl_soil,deltim,wimp,smpmin,&
                     qinfl,etr,z_soisno(1:),dz_soisno(1:),zi_soisno(0:),&
                     t_soisno(1:),vol_liq,vol_ice,smp,hk,icefrac,eff_porosity,&
                     porsl,hksati,bsw,psi0,rootr,rootflux,&
                     zwt,dwat,qcharge)

      ! update the mass of liquid water
      DO j= 1, nl_soil
         wliq_soisno(j) = wliq_soisno(j)+dwat(j)*dzmm(j)
      ENDDO


!=======================================================================
! [4] subsurface runoff and the corrections
!=======================================================================

      CALL groundwater (nl_soil,deltim,pondmx,&
                        eff_porosity,icefrac,dz_soisno(1:),zi_soisno(0:),&
                        wice_soisno(1:),wliq_soisno(1:),&
                        porsl,psi0,bsw,zwt,wa,&
                        qcharge,rsubst)

      ! total runoff (mm/s)
      rnof = rsubst + rsur
      ! Renew the ice and liquid mass due to condensation
IF ((.not.DEF_SPLIT_SOILSNOW) .or. (patchtype==1 .and. DEF_URBAN_RUN)) THEN
      IF(lb >= 1)THEN
         ! make consistent with how evap_grnd removed in infiltration
         wliq_soisno(1) = max(0., wliq_soisno(1) + qsdew * deltim)
         wice_soisno(1) = max(0., wice_soisno(1) + (qfros-qsubl) * deltim)
      ENDIF

      err_solver = (sum(wliq_soisno(1:))+sum(wice_soisno(1:))+wa+wdsrf) - w_sum &
                 - (gwat-etr-rnof)*deltim

      IF(lb >= 1)THEN
         err_solver = err_solver-(qsdew+qfros-qsubl)*deltim
      ENDIF

ELSE
      wliq_soisno(1) = max(0., wliq_soisno(1) + qsdew_soil * deltim)
      wice_soisno(1) = max(0., wice_soisno(1) + (qfros_soil-qsubl_soil) * deltim)

      err_solver = (sum(wliq_soisno(1:))+sum(wice_soisno(1:))+wa+wdsrf) - w_sum &
                 - (gwat-etr-rnof)*deltim

      err_solver = err_solver-(qsdew_soil+qfros_soil-qsubl_soil)*deltim
ENDIF

#if (defined CoLMDEBUG)
      IF(abs(err_solver) > 1.e-3)THEN
         write(6,*) 'Warning: water balance violation after all soilwater calculation', err_solver
      ENDIF
#endif


!=======================================================================
! [6] assumed hydrological scheme for the wetland and glacier
!=======================================================================

ELSE
      IF(patchtype==2)THEN        ! WETLAND
         ! Wetland precipitation becomes surface runoff; keep total runoff
         ! consistent with its surface and subsurface components.
         qinfl = 0.
         rsur = max(0.,gwat)
         rsubst = 0.
         rnof = rsur + rsubst
         DO j = 1, nl_soil
            IF(t_soisno(j)>tfrz)THEN
               wice_soisno(j) = 0.0
               wliq_soisno(j) = porsl(j)*dz_soisno(j)*1000.
            ENDIF
         ENDDO
      ENDIF
      IF(patchtype==3)THEN        ! LAND ICE
         rsur = max(0.0,gwat)
         qinfl = 0.
         rsubst = 0.
         rnof = rsur
         wice_soisno(1:nl_soil) = dz_soisno(1:nl_soil)*1000.
         wliq_soisno(1:nl_soil) = 0.0
      ENDIF

      wa = 4800.
      zwt = 0.
      qcharge = 0.

ENDIF

   END SUBROUTINE WATER_2014

!-----------------------------------------------------------------------
   SUBROUTINE WATER_VSF (ipatch, patchtype,is_dry_lake, lb, nl_soil, deltim   ,&
              z_soisno    ,dz_soisno   ,zi_soisno   ,bsw         ,theta_r     ,&
              fsatmax     ,fsatdcf     ,topoweti    ,alp_twi     ,chi_twi     ,&
              mu_twi      ,elvstd      ,BVIC                                  ,&
#ifdef vanGenuchten_Mualem_SOIL_MODEL
              alpha_vgm   ,n_vgm       ,L_vgm       ,sc_vgm      ,fc_vgm      ,&
#endif
              porsl       ,psi0        ,hksati      ,rootr       ,rootflux    ,&
              t_soisno    ,wliq_soisno ,wice_soisno ,smp         ,hk          ,&
              pg_rain     ,sm          ,etr         ,qseva       ,qsdew       ,&
              qsubl       ,qfros       ,qseva_soil  ,qsdew_soil  ,qsubl_soil  ,&
              qfros_soil  ,qseva_snow  ,qsdew_snow  ,qsubl_snow  ,qfros_snow  ,&
              fsno        ,frcsat      ,rsur        ,rsur_se     ,rsur_ie     ,&
              rsubst      ,rnof        ,qinfl       ,qlayer      ,ssi         ,&
              pondmx      ,wimp        ,zwt         ,wdsrf       ,wa          ,&
              wetwat                                                          ,&
! SNICAR model variables
              forc_aer                                                        ,&
              mss_bcpho   ,mss_bcphi   ,mss_ocpho   ,mss_ocphi                ,&
              mss_dst1    ,mss_dst2    ,mss_dst3    ,mss_dst4                 ,&
!  irrigation variable
              qflx_irrig_drip  ,qflx_irrig_flood ,qflx_irrig_paddy            )

!===================================================================================
!  this is the main SUBROUTINE to execute the calculation of soil water processes
!
!  Original author: Yongjiu Dai, /09/1999/, /08/2002/, /04/2014/
!
!  Modified by Shupeng Zhang /07/2023/ to USE Variably Saturated Flow algorithm
!     Reference :
!     Dai, Y., Zhang, S., Yuan, H., & Wei, N. (2019).
!     Modeling Variably Saturated Flow in Stratified Soils
!       With Explicit Tracking of Wetting Front and Water Table Locations.
!     Water Resources Research. doi:10.1029/2019wr025368
!
!===================================================================================

   USE MOD_Precision
   USE MOD_Hydro_SoilWater
   USE MOD_Vars_TimeInvariants, only: wetwatmax
   USE MOD_Const_Physical,      only: denice, denh2o, tfrz
   USE MOD_Vars_TimeInvariants, only: vic_b_infilt, vic_Dsmax, vic_Ds, vic_Ws, vic_c
   USE MOD_Vars_1DFluxes,       only: fevpg
   USE MOD_Opt_Baseflow,        only: scale_baseflow
#ifdef DataAssimilation
   USE MOD_DA_TWS, only: fslp_k
#endif
#ifdef CROP
   USE MOD_Vars_Global, only : irrig_method_paddy, pondmxc
   use MOD_LandPFT, only : patch_pft_s, patch_pft_e
   use MOD_Vars_PFTimeVariables, only: irrig_method_p
#endif

   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   integer, intent(in) :: &
        ipatch           ,& ! patch index
        patchtype           ! land patch type (0=soil, 1=urban or built-up, 2=wetland,
                            ! 3=land ice, 4=land water bodies, 99=ocean
   logical, intent(in) :: is_dry_lake

   integer, intent(in) :: &
        lb               , &! lower bound of array
        nl_soil             ! upper bound of array

   real(r8), intent(in) :: &
        deltim           , &! time step (s)
        ! wtfact         , &! (updated to gridded 'fsatmax' data) fraction of model area with high water table
        ssi              , &! irreducible water saturation of snow
        pondmx           , &! ponding depth (mm)
        wimp             , &! water impermeable IF porosity less than wimp
        elvstd           , &! standard deviation of elevation (m)
        BVIC             , &!
        z_soisno (lb:nl_soil)   , &! layer depth (m)
        dz_soisno(lb:nl_soil)   , &! layer thickness (m)
        zi_soisno(lb-1:nl_soil) , &! interface level below a "z" level (m)
        bsw      (1:nl_soil), & ! clapp and hornberger "b" parameter [-]
        theta_r  (1:nl_soil), & ! residual moisture content [-]
        fsatmax             , & ! maximum saturated area fraction [-]
        fsatdcf             , & ! decay factor in calculation of saturated area fraction [1/m]
        topoweti            , & ! mean topographic wetness index
        alp_twi             , & ! alpha in three parameter gamma distribution of twi
        chi_twi             , & ! chi   in three parameter gamma distribution of twi
        mu_twi              , & ! mu    in three parameter gamma distribution of twi
#ifdef vanGenuchten_Mualem_SOIL_MODEL
        alpha_vgm(1:nl_soil), & ! a parameter corresponding approximately to the inverse of the air-entry value
        n_vgm    (1:nl_soil), & ! a shape parameter [dimensionless]
        L_vgm    (1:nl_soil), & ! pore-connectivity parameter [dimensionless]
        sc_vgm   (1:nl_soil), & ! saturation at the air entry value in the classical vanGenuchten model [-]
        fc_vgm   (1:nl_soil), & ! a scaling factor by using air entry value in the Mualem model [-]
#endif
        porsl(1:nl_soil) , &! saturated volumetric soil water content(porosity)
        psi0(1:nl_soil)  , &! saturated soil suction (mm) (NEGATIVE)
        hksati(1:nl_soil), &! hydraulic conductivity at saturation (mm h2o/s)
        rootr(1:nl_soil) , &! water uptake fraction from different layers, all layers add to 1.0
        rootflux(1:nl_soil),&! root uptake from different layer, all layers add to transpiration

        t_soisno(lb:nl_soil), &! soil/snow skin temperature (K)
        pg_rain          , &! rainfall after removal of interception (mm h2o/s)
        sm               , &! snow melt (mm h2o/s)
        etr              , &! actual transpiration (mm h2o/s)
        qseva            , &! ground surface evaporation rate (mm h2o/s)
        qsdew            , &! ground surface dew formation (mm h2o /s) [+]
        qsubl            , &! sublimation rate from snow pack (mm h2o /s) [+]
        qfros            , &! surface dew added to snow pack (mm h2o /s) [+]
        qseva_soil       , &! ground soil surface evaporation rate (mm h2o/s)
        qsdew_soil       , &! ground soil surface dew formation (mm h2o /s) [+]
        qsubl_soil       , &! sublimation rate from soil ice pack (mm h2o /s) [+]
        qfros_soil       , &! surface dew added to soil ice pack (mm h2o /s) [+]
        qseva_snow       , &! ground snow surface evaporation rate (mm h2o/s)
        qsdew_snow       , &! ground snow surface dew formation (mm h2o /s) [+]
        qsubl_snow       , &! sublimation rate from snow pack (mm h2o /s) [+]
        qfros_snow       , &! surface dew added to snow pack (mm h2o /s) [+]
        fsno                ! snow fractional cover
   real(r8), intent(inout) :: &
        wice_soisno(lb:nl_soil) , &! ice lens (kg/m2)
        wliq_soisno(lb:nl_soil)    ! liquid water (kg/m2)

   real(r8), intent(out) :: &
        smp(1:nl_soil)   , &! soil matrix potential [mm]
        hk (1:nl_soil)      ! hydraulic conductivity [mm h2o/s]

   real(r8), intent(inout) :: &
        zwt              , &! the depth from ground (soil) surface to water table [m]
        wdsrf            , &! depth of surface water [mm]
        wa               , &! water storage in aquifer [mm]
        wetwat              ! water storage in wetland [mm]

   real(r8), intent(out) :: &
        frcsat           , &! fraction of saturation area
        rsur             , &! surface runoff (mm h2o/s)
        rsur_se          , &! saturation excess surface runoff (mm h2o/s)
        rsur_ie          , &! infiltration excess surface runoff (mm h2o/s)
        rsubst           , &! subsurface runoff (mm h2o/s)
        rnof             , &! total runoff (mm h2o/s)
        qinfl            , &! infiltration rate (mm h2o/s)
        qlayer(0:nl_soil)   ! water flux between soil layer [mm h2o/s]



! SNICAR model variables
! Aerosol Fluxes (Jan. 07, 2023)
   real(r8), intent(in) :: forc_aer ( 14 )  ! aerosol deposition from atmosphere model (grd,aer) [kg m-1 s-1]

   real(r8), intent(inout) :: &
        mss_bcpho (lb:0), &! mass of hydrophobic BC in snow  (col,lyr) [kg]
        mss_bcphi (lb:0), &! mass of hydrophillic BC in snow (col,lyr) [kg]
        mss_ocpho (lb:0), &! mass of hydrophobic OC in snow  (col,lyr) [kg]
        mss_ocphi (lb:0), &! mass of hydrophillic OC in snow (col,lyr) [kg]
        mss_dst1  (lb:0), &! mass of dust species 1 in snow  (col,lyr) [kg]
        mss_dst2  (lb:0), &! mass of dust species 2 in snow  (col,lyr) [kg]
        mss_dst3  (lb:0), &! mass of dust species 3 in snow  (col,lyr) [kg]
        mss_dst4  (lb:0)   ! mass of dust species 4 in snow  (col,lyr) [kg]
! Aerosol Fluxes (Jan. 07, 2023)
! END SNICAR model variables

!  irrigation variable
   real(r8), intent(in) :: &
        qflx_irrig_drip  , &! irrigation flux from drip irrigation [mm/s]
        qflx_irrig_flood , &! irrigation flux from flood irrigation [mm/s]
        qflx_irrig_paddy    ! irrigation flux from paddy irrigation [mm/s]

!-------------------------- Local Variables ----------------------------

   integer j                 ! loop counter

   real(r8) :: &
       eff_porosity(1:nl_soil), &! effective porosity = porosity - vol_ice
       gwat              , &! net water input from top (mm/s)
       drainmax          , &! drainage max (mm h2o/s)
       vol_liq(1:nl_soil), &! partial volume of liquid water in layer
       vol_ice(1:nl_soil), &! partial volume of ice lens in layer
       icefrac(1:nl_soil)   ! ice fraction (-)

   real(r8) :: eta

   real(r8) :: err_solver, w_sum, wresi(1:nl_soil)
   real(r8) :: qgtop
   real(r8) :: qgtop_deficit, imperv_surface_loss
   real(r8) :: imperv_soil_deficit, imperv_liq_loss

   real(r8) :: zwtmm
   real(r8) :: sp_zc(1:nl_soil), sp_zi(0:nl_soil), sp_dz(1:nl_soil) ! in mm
   logical  :: is_permeable(1:nl_soil)
   real(r8) :: dzsum, dz
   real(r8) :: icefracsum, fracice_rsub, imped
   real(r8) :: wblc

#ifdef Campbell_SOIL_MODEL
   integer, parameter :: nprms = 1
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
   integer, parameter :: nprms = 5
#endif
   real(r8) :: prms(nprms, 1:nl_soil)
   type(soil_con_struct ) :: soil_con
   type(cell_data_struct) :: cell
   real(r8) :: wliq_soisno_tmp(1:nl_soil)

   real(r8), parameter :: e_ice=6.0      !soil ice impedance factor

   integer :: ps, pe, m


#ifdef Campbell_SOIL_MODEL
      prms(1,:) = bsw(1:nl_soil)
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
      prms(1,1:nl_soil) = alpha_vgm(1:nl_soil)
      prms(2,1:nl_soil) = n_vgm    (1:nl_soil)
      prms(3,1:nl_soil) = L_vgm    (1:nl_soil)
      prms(4,1:nl_soil) = sc_vgm   (1:nl_soil)
      prms(5,1:nl_soil) = fc_vgm   (1:nl_soil)
#endif

!=======================================================================
! [1] update the liquid water within snow layer and the water onto soil
!=======================================================================

IF ((.not.DEF_SPLIT_SOILSNOW) .or. (patchtype==1 .and. DEF_URBAN_RUN)) THEN

      IF (lb>=1)THEN
         gwat = pg_rain + sm - qseva
      ELSE

         IF ((.not.DEF_USE_SNICAR) .or. (patchtype==1 .and. DEF_URBAN_RUN)) THEN
            CALL snowwater (lb,deltim,ssi,wimp,&
                         pg_rain,qseva,qsdew,qsubl,qfros,&
                         dz_soisno(lb:0),wice_soisno(lb:0),wliq_soisno(lb:0),gwat)
         ELSE
            CALL snowwater_snicar (lb,deltim,ssi,wimp,&
                         pg_rain,qseva,qsdew,qsubl,qfros,&
                         dz_soisno(lb:0),wice_soisno(lb:0),wliq_soisno(lb:0),gwat,&
                         forc_aer,&
                         mss_bcpho(lb:0), mss_bcphi(lb:0), mss_ocpho(lb:0), mss_ocphi(lb:0),&
                         mss_dst1(lb:0), mss_dst2(lb:0), mss_dst3(lb:0), mss_dst4(lb:0) )
         ENDIF
      ENDIF

ELSE

      IF (lb>=1)THEN
         gwat = pg_rain + sm - qseva_soil
      ELSE
         IF (.not. DEF_USE_SNICAR) THEN
            CALL snowwater (lb,deltim,ssi,wimp,&
                         pg_rain*fsno,qseva_snow,qsdew_snow,qsubl_snow,qfros_snow,&
                         dz_soisno(lb:0),wice_soisno(lb:0),wliq_soisno(lb:0),gwat)
         ELSE
            CALL snowwater_snicar (lb,deltim,ssi,wimp,&
                         pg_rain*fsno,qseva_snow,qsdew_snow,qsubl_snow,qfros_snow,&
                         dz_soisno(lb:0),wice_soisno(lb:0),wliq_soisno(lb:0),gwat,&
                         forc_aer,&
                         mss_bcpho(lb:0), mss_bcphi(lb:0), mss_ocpho(lb:0), mss_ocphi(lb:0),&
                         mss_dst1(lb:0), mss_dst2(lb:0), mss_dst3(lb:0), mss_dst4(lb:0) )
         ENDIF
         gwat = gwat + pg_rain*(1-fsno) - qseva_soil
      ENDIF
ENDIF


#ifdef CROP
      IF(DEF_USE_IRRIGATION)THEN
         gwat = gwat + qflx_irrig_drip + qflx_irrig_flood + qflx_irrig_paddy
      ENDIF
#endif

!=======================================================================
! [2] surface runoff and infiltration
!=======================================================================

IF((patchtype<=1) .or. is_dry_lake &
   .or. (DEF_USE_Dynamic_Wetland .and. (patchtype==2)))THEN   ! soil ground only

      ! For water balance check, the sum of water in soil column before the calculation
      w_sum = sum(wliq_soisno(1:nl_soil)) + sum(wice_soisno(1:nl_soil)) + wa + wdsrf

      ! Due to the increase in volume after freezing, the total volume of water and
      ! ice may exceed the porosity of the soil. This excess water is temporarily
      ! stored in "wresi". After calculating the movement of soil water, "wresi"
      ! is added back to "wliq_soisno".
      wresi(1:nl_soil) = 0.
      ! porosity of soil, partial volume of ice and liquid
      DO j = 1, nl_soil
         vol_ice(j) = min(porsl(j), wice_soisno(j)/(dz_soisno(j)*denice))
         IF(porsl(j) < 1.e-6)THEN
            icefrac(j) = 0.
         ELSE
            icefrac(j) = min(1.,vol_ice(j)/porsl(j))
         ENDIF

         eff_porosity(j) = max(wimp, porsl(j)-vol_ice(j))
         is_permeable(j) = eff_porosity(j) > max(wimp, theta_r(j))
         IF (is_permeable(j)) THEN
            vol_liq(j) = wliq_soisno(j)/(dz_soisno(j)*denh2o)
            vol_liq(j) = min(eff_porosity(j), max(0., vol_liq(j)))
            wresi(j) = wliq_soisno(j) - dz_soisno(j) * denh2o * vol_liq(j)
         ELSE
            vol_liq(j) = 0.
         ENDIF
      ENDDO

      ! surface runoff including water table and surface saturated area

      rsur    = 0.
      rsubst  = 0.
      rsur_ie = 0.
      rsur_se = 0.

#ifndef CatchLateralFlow
      IF (patchtype <= 1) THEN

         IF (DEF_Runoff_SCHEME  == 0) THEN

            CALL SurfaceRunoff_TOPMOD (nl_soil,wimp,porsl,psi0,hksati,fsatmax,fsatdcf,&
               z_soisno(1:),dz_soisno(1:),zi_soisno(0:),&
               eff_porosity,icefrac,zwt,gwat,rsur,rsur_se,rsur_ie,topoweti,alp_twi,chi_twi,mu_twi,frcsat,eta)

            CALL SubsurfaceRunoff_TOPMOD (nl_soil, icefrac, dz_soisno(1:), zi_soisno(0:), &
               zwt, rsubst, hksati, topoweti, eta)

         ELSEIF (DEF_Runoff_SCHEME  == 1) THEN
            ! 1: runoff scheme from VIC model

            CALL Runoff_VIC(deltim, porsl, theta_r, hksati, bsw, &
               wice_soisno(1:nl_soil), wliq_soisno(1:nl_soil), fevpg(ipatch), rootflux, gwat, &
               vic_b_infilt(ipatch), vic_Dsmax(ipatch), vic_Ds(ipatch), vic_Ws(ipatch), vic_c(ipatch),&
               rsur, rsubst, wliq_soisno_tmp)

            rsur_se = rsur
            rsur_ie = 0.

         ELSEIF (DEF_Runoff_SCHEME  == 2) THEN
            ! 2: runoff scheme from XinAnJiang model

            CALL Runoff_XinAnJiang (&
               nl_soil, dz_soisno(1:nl_soil), eff_porosity(1:nl_soil), vol_liq(1:nl_soil), &
               elvstd, gwat, deltim, rsur, rsubst, frcsat)

            rsur_se = rsur
            rsur_ie = 0.

         ELSEIF (DEF_Runoff_SCHEME  == 3) THEN
            ! 3: runoff scheme from simplified VIC model

            CALL Runoff_SimpleVIC (&
               nl_soil, dz_soisno(1:nl_soil), eff_porosity(1:nl_soil), vol_liq(1:nl_soil), &
               BVIC, gwat, deltim, rsur, rsubst, frcsat)

            ! CALL SubsurfaceRunoff_SimpleVIC ( &
            !    nl_soil,           z_soisno(1:nl_soil), dz_soisno(1:nl_soil), wice_soisno(1:nl_soil), &
            !    porsl(1:nl_soil),  psi0(1:nl_soil),     hksati(1:nl_soil),    theta_r(1:nl_soil),     &
            !    nprms,             prms(:,1:nl_soil),   zwt,                  rsubst)

            rsur_se = rsur
            rsur_ie = 0.

         ENDIF

         rsubst = rsubst * scale_baseflow(ipatch)

#ifdef DataAssimilation
         IF (DEF_DA_TWS_GRACE) THEN
            rsur = max(min(rsur * fslp_k(ipatch), gwat), 0.)
            rsubst = rsubst * fslp_k(ipatch)
         ENDIF
#endif

#ifdef CROP
         IF(patchtype.eq.0 .AND. DEF_USE_IRRIGATION)THEN
            ps = patch_pft_s(ipatch)
            pe = patch_pft_e(ipatch)
            DO m = ps, pe
               IF(irrig_method_p(m).eq.irrig_method_paddy)THEN
                  rsur = 0
               ENDIF
            ENDDO
         ENDIF
#endif
      ENDIF
#else
      ! for catchment based lateral flow,
      ! "rsur" is calculated in HYDRO/MOD_Catch_HillslopeFlow.F90
      ! "rsub" is calculated in HYDRO/MOD_Catch_SubsurfaceFlow.F90
#endif

      ! infiltration into surface soil layer
      qgtop = gwat - rsur

!=======================================================================
! [3] determine the change of soil water
!=======================================================================

      ! convert length units from m to mm
      zwtmm = zwt * 1000.0
      sp_zc(1:nl_soil) = z_soisno (1:nl_soil) * 1000.0   ! from meter to mm
      sp_zi(0:nl_soil) = zi_soisno(0:nl_soil) * 1000.0   ! from meter to mm

      ! check consistency between water table location and liquid water content
      IF (wa < 0.) THEN
         IF (zwtmm <= sp_zi(nl_soil)) THEN
            CALL get_zwt_from_wa ( &
               porsl(nl_soil), theta_r(nl_soil), psi0(nl_soil), hksati(nl_soil), &
               nprms, prms(:,nl_soil), 1.e-5, 1.e-8, wa, sp_zi(nl_soil), zwtmm)
         ENDIF
      ELSE
         DO j = 1, nl_soil
            IF ((vol_liq(j) < eff_porosity(j)-1.e-8) .and. (zwtmm <= sp_zi(j-1))) THEN
               zwtmm = sp_zi(j)
            ENDIF
         ENDDO
      ENDIF

      ! update "vol_liq" in the level containing water table
      ! "vol_liq" in this level refers to volume content in unsaturated part
      IF (zwtmm < sp_zi(nl_soil)) THEN
         DO j = nl_soil, 1, -1
            IF ((zwtmm >= sp_zi(j-1)) .and. (zwtmm < sp_zi(j))) THEN

               IF ((zwtmm > sp_zi(j-1)) .and. (is_permeable(j))) THEN
                  vol_liq(j) = (wliq_soisno(j)*1000.0/denh2o - eff_porosity(j)*(sp_zi(j)-zwtmm))  &
                     / (zwtmm - sp_zi(j-1))
                  IF (vol_liq(j) < 0.) THEN
                     zwtmm = sp_zi(j)
                     vol_liq(j) = wliq_soisno(j)*1000.0/denh2o / (sp_zi(j) - sp_zi(j-1))
                  ENDIF

                  vol_liq(j) = max(0., min(eff_porosity(j), vol_liq(j)))
                  wresi(j) = wliq_soisno(j)*1000.0/denh2o - eff_porosity(j)*(sp_zi(j)-zwtmm) &
                     - vol_liq(j) * (zwtmm - sp_zi(j-1))
               ENDIF

               EXIT
            ENDIF
         ENDDO
      ENDIF

      wdsrf = max(0., wdsrf)

      IF ((.not. is_permeable(1)) .and. (qgtop < 0.)) THEN
         qgtop_deficit = -qgtop * deltim

         imperv_surface_loss = min(max(wdsrf, 0._r8), qgtop_deficit)
         IF (imperv_surface_loss > 0._r8) THEN
            wdsrf = max(0._r8, wdsrf - imperv_surface_loss)
         ENDIF

         imperv_soil_deficit = max(qgtop_deficit - imperv_surface_loss, 0._r8)
         IF (imperv_soil_deficit > 0._r8) THEN
            ! qgtop contains qseva/qseva_soil, which Thermal has already
            ! limited to available liquid water.  Sublimation is carried by
            ! qsubl/qsubl_soil and removes ice separately below; charging this
            ! liquid evaporation deficit to ice would double-count the phase
            ! change and use the wrong latent heat.
            imperv_liq_loss = min(max(wliq_soisno(1), 0._r8), imperv_soil_deficit)
            wliq_soisno(1) = max(0._r8, wliq_soisno(1) - imperv_liq_loss)
            IF (imperv_soil_deficit - imperv_liq_loss > 1.e-10_r8) THEN
               CALL CoLM_Stop ('ERROR: impermeable-surface liquid evaporation exceeds available liquid water.')
            ENDIF
         ENDIF

         qgtop = 0.

      ENDIF

      CALL soil_water_vertical_movement ( &
         nl_soil,                 deltim,                   sp_zc(1:nl_soil),    sp_zi(0:nl_soil), &
         is_permeable(1:nl_soil), eff_porosity(1:nl_soil),  theta_r(1:nl_soil),  psi0(1:nl_soil),  &
         hksati(1:nl_soil),       nprms, prms(:,1:nl_soil), porsl(nl_soil),      qgtop,            &
         etr,                     rootr(1:nl_soil),         rootflux(1:nl_soil), rsubst,           &
         qinfl,                   wdsrf,                    zwtmm,               wa,               &
         vol_liq(1:nl_soil),      smp(1:nl_soil),           hk(1:nl_soil),       qlayer(0:nl_soil),&
         1.e-3,                   wblc)

      ! update the mass of liquid water
      DO j = nl_soil, 1, -1
         IF (is_permeable(j)) THEN
            IF (zwtmm < sp_zi(j)) THEN
               IF (zwtmm >= sp_zi(j-1)) THEN
                  wliq_soisno(j)  = denh2o * ((eff_porosity(j)*(sp_zi(j)-zwtmm))  &
                     + vol_liq(j) * (zwtmm - sp_zi(j-1)))/1000.0
               ELSE
                  wliq_soisno(j)  = denh2o * (eff_porosity(j)*(sp_zi(j)-sp_zi(j-1)))/1000.0
               ENDIF
            ELSE
               wliq_soisno(j) = denh2o * (vol_liq(j)*(sp_zi(j)-sp_zi(j-1)))/1000.0
            ENDIF

            wliq_soisno(j) = wliq_soisno(j) + wresi(j)
         ENDIF
      ENDDO

      zwt = zwtmm/1000.0

      ! Renew the ice and liquid mass due to condensation
IF ((.not.DEF_SPLIT_SOILSNOW) .or. (patchtype==1 .and. DEF_URBAN_RUN)) THEN
      IF(lb >= 1)THEN
         ! make consistent with how evap_grnd removed in infiltration
         wliq_soisno(1) = max(0., wliq_soisno(1) + qsdew * deltim)
         wice_soisno(1) = max(0., wice_soisno(1) + (qfros-qsubl) * deltim)
      ENDIF
ELSE
      wliq_soisno(1) = max(0., wliq_soisno(1) + qsdew_soil * deltim)
      wice_soisno(1) = max(0., wice_soisno(1) + (qfros_soil-qsubl_soil) * deltim)
ENDIF

      ! water imbalance mainly due to insufficient liquid water for evapotranspiration
      IF (wblc > 0.) THEN
         DO j = 1, nl_soil
            IF (wice_soisno(j) > wblc) THEN
               wice_soisno(j) = wice_soisno(j) - wblc
               wblc = 0.
               EXIT
            ELSE
               wblc = wblc - wice_soisno(j)
               wice_soisno(j) = 0.
            ENDIF
         ENDDO
      ENDIF

#ifndef CatchLateralFlow
#ifdef CROP
      IF(patchtype.eq.0 .AND. DEF_USE_IRRIGATION)THEN
         ps = patch_pft_s(ipatch)
         pe = patch_pft_e(ipatch)
         DO m = ps, pe
            IF(irrig_method_p(m).eq.irrig_method_paddy .AND. wdsrf.gt.pondmxc)THEN
               rsur = rsur + (wdsrf - pondmxc)/deltim
               rsur_ie = rsur_ie + (wdsrf - pondmxc) / deltim
               wdsrf = pondmxc
            ELSEIF(irrig_method_p(m).ne.irrig_method_paddy .AND. wdsrf.gt.pondmx)THEN
               rsur = rsur + (wdsrf - pondmx) / deltim
               rsur_ie = rsur_ie + (wdsrf - pondmx) / deltim
               wdsrf = pondmx
            ENDIF
         ENDDO

         IF (zwt <= 0.) THEN
            rsur_ie = 0.
            rsur_se = rsur
         ENDIF
         ! total runoff (mm/s)
         rnof = rsubst + rsur
      ELSE
#endif
         IF (patchtype <= 1) THEN
            IF (wdsrf > pondmx) THEN
               rsur = rsur + (wdsrf - pondmx) / deltim
               rsur_ie = rsur_ie + (wdsrf - pondmx) / deltim
               wdsrf = pondmx
            ENDIF

            IF (zwt <= 0.) THEN
               rsur_ie = 0.
               rsur_se = rsur
            ENDIF

            ! total runoff (mm/s)
            rnof = rsubst + rsur
         ELSEIF (patchtype == 2) THEN ! for wetland
            IF (wdsrf > wetwatmax) THEN
               rsur_se = (wdsrf - wetwatmax) / deltim
               wdsrf = wetwatmax
            ENDIF

            rsur = rsur_se
            ! total runoff (mm/s)
            rnof = rsur
         ELSE ! for dry lake
            rnof = 0.
         ENDIF
#ifdef CROP
      ENDIF
#endif
#endif

      DO j = 1, nl_soil
         IF(t_soisno(j) <= tfrz) THEN
            ! consider impedance factor
            vol_ice(j) = max(min(porsl(j), wice_soisno(j)/(dz_soisno(j)*denice)), 0.)
            icefrac(j) = vol_ice(j)/porsl(j)
            imped = 10.**(-e_ice*icefrac(j))
            hk(j) = imped * hk(j)
         ENDIF
      ENDDO

#ifndef CatchLateralFlow
      err_solver = (sum(wliq_soisno(1:))+sum(wice_soisno(1:))+wa+wdsrf) - w_sum &
         - (gwat-etr-rsur-rsubst)*deltim
#else
      err_solver = (sum(wliq_soisno(1:))+sum(wice_soisno(1:))+wa+wdsrf) - w_sum &
         - (gwat-etr)*deltim
#endif

IF ((.not.DEF_SPLIT_SOILSNOW) .or. (patchtype==1 .and. DEF_URBAN_RUN)) THEN
      IF(lb >= 1)THEN
         err_solver = err_solver - (qsdew+qfros-qsubl)*deltim
      ENDIF
ELSE
      err_solver = err_solver-(qsdew_soil+qfros_soil-qsubl_soil)*deltim
ENDIF

#if (defined CoLMDEBUG)
      IF(abs(err_solver) > 1.e-3)THEN
         write(6,'(A,E20.5,A,I0)') 'Warning (WATER_VSF): water balance violation', err_solver, &
            ' in element ', landpatch%eindex(ipatch)
      ENDIF
      IF (any(wliq_soisno < -1.e-3)) THEN
         write(6,'(A,10E20.5)') 'Warning (WATER_VSF): negative soil water', wliq_soisno(1:nl_soil)
      ENDIF
#endif

!=======================================================================
! [6] assumed hydrological scheme for the wetland
!=======================================================================

ELSE
      IF(patchtype==2)THEN        ! WETLAND
         qinfl = 0.
         zwt = 0.


         IF (.not.DEF_SPLIT_SOILSNOW) THEN
            IF (lb >= 1) THEN
               wetwat = wdsrf + wa + wetwat + (gwat - etr + qsdew + qfros - qsubl) * deltim
            ELSE
               wetwat = wdsrf + wa + wetwat + (gwat - etr) * deltim
            ENDIF
         ELSE
            wetwat = wdsrf + wa + wetwat + (gwat - etr + qsdew_soil + qfros_soil - qsubl_soil) * deltim
         ENDIF

         wresi(:) = 0.
         DO j = 1, nl_soil
            IF(t_soisno(j)>tfrz)THEN
               wresi(j) = max(wliq_soisno(j) - porsl(j)*dz_soisno(j)*1000., 0.)
               wliq_soisno(j) = wliq_soisno(j) - wresi(j)
            ENDIF
         ENDDO

         wetwat = wetwat + sum(wresi)

         IF (wetwat > wetwatmax) THEN
            wdsrf  = wetwat - wetwatmax
            wetwat = wetwatmax
            wa     = 0.
         ELSEIF (wetwat < 0) THEN
            wa     = wetwat
            wdsrf  = 0.
            wetwat = 0.
         ELSE
            wdsrf = 0.
            wa    = 0.
         ENDIF

         frcsat = 1.

#ifndef CatchLateralFlow
         IF (wdsrf > pondmx) THEN
            rsur = (wdsrf - pondmx) / deltim
            wdsrf = pondmx
         ELSE
            rsur = 0.
         ENDIF
         rnof = rsur
         rsur_se = rsur
         rsur_ie = 0.
#endif
      ENDIF

ENDIF

   END SUBROUTINE WATER_VSF


   SUBROUTINE snowwater (lb,deltim,ssi,wimp, &
                        pg_rain,qseva,qsdew,qsubl,qfros, &
                        dz_soisno,wice_soisno,wliq_soisno,qout_snowb)

!-----------------------------------------------------------------------
!  Original author: Yongjiu Dai, /09/1999; /04/2014
!
!  Water flow within snow is computed by an explicit and non-physical based
!  scheme, which permits a part of liquid water over the holding capacity (a
!  tentative value is used, i.e., equal to 0.033*porosity) to percolate into the
!  underlying layer, except the case of that the porosity of one of the two
!  neighboring layers is less than 0.05, the zero flow is assumed. The water
!  flow out of the bottom snow pack will participate as the input of the soil
!  water and runoff.
!
!-----------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_Const_Physical, only: denice, denh2o  ! physical constant
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   integer, intent(in) :: &
        lb          ! lower bound of array

   real(r8), intent(in) :: &
        deltim,    &! seconds in a time step (s)
        ssi,       &! irreducible water saturation of snow
        wimp,      &! water impermeable if porosity less than wimp
        dz_soisno(lb:0),  &! layer thickness (m)

        pg_rain,   &! rainfall after removal of interception (mm h2o/s)
        qseva,     &! ground surface evaporation rate (mm h2o/s)
        qsdew,     &! ground surface dew formation (mm h2o /s) [+]
        qsubl,     &! sublimation rate from snow pack (mm h2o /s) [+]
        qfros       ! surface dew added to snow pack (mm h2o /s) [+]

   real(r8), intent(inout) :: &
        wice_soisno(lb:0),&! ice lens (kg/m2)
        wliq_soisno(lb:0)  ! liquid water (kg/m2)

   real(r8), intent(out) :: &
        qout_snowb  ! rate of water out of snow bottom (mm/s)

!-------------------------- Local Variables ----------------------------
   integer j           ! k do loop/array indices

   real(r8) :: &
       qin,           &! water flow into the element (mm/s)
       qout,          &! water flow out of the element (mm/s)
       zwice,         &! the sum of ice mass of snow cover (kg/m2)
       wgdif,         &! ice mass after minus sublimation
       vol_liq(lb:0), &! partial volume of liquid water in layer
       vol_ice(lb:0), &! partial volume of ice lens in layer
       eff_porosity(lb:0) ! effective porosity = porosity - vol_ice

!=======================================================================
! renew the mass of ice lens (wice_soisno) and liquid (wliq_soisno) in the
! surface snow layer, resulted by sublimation (frost) / evaporation (condense)

      wgdif = wice_soisno(lb) + (qfros - qsubl)*deltim
      wice_soisno(lb) = wgdif
      IF(wgdif < 0.)THEN
         wice_soisno(lb) = 0.
         wliq_soisno(lb) = wliq_soisno(lb) + wgdif
      ENDIF

      wliq_soisno(lb) = wliq_soisno(lb) + (pg_rain + qsdew - qseva)*deltim
      IF (wliq_soisno(lb) < 0.) THEN
         wice_soisno(lb) = wice_soisno(lb) + wliq_soisno(lb)
         wice_soisno(lb) = max(wice_soisno(lb), 0.)
         wliq_soisno(lb) = 0.
      ENDIF

! Porosity and partial volume
      DO j = lb, 0
         vol_ice(j) = min(1., wice_soisno(j)/(dz_soisno(j)*denice))
         eff_porosity(j) = max(0.01, 1. - vol_ice(j))
         vol_liq(j) = min(eff_porosity(j), wliq_soisno(j)/(dz_soisno(j)*denh2o))
      ENDDO

! Capillary force within snow could be two or more orders of magnitude less
! than those of gravity, this term may be ignored.  Here we could keep the
! gravity term only. The general expression for water flow is "K * ss**3",
! however, no effective parameterization for "K". Thus, a very simple treatment
! (not physical based) is introduced: when the liquid water of layer exceeds
! the layer's holding capacity, the excess meltwater adds to the underlying
! neighbor layer.

      qin = 0.
      DO j= lb, 0
         wliq_soisno(j) = wliq_soisno(j) + qin

         IF(j <= -1)THEN
         ! no runoff over snow surface, just ponding on surface
            IF(eff_porosity(j)<wimp .or. eff_porosity(j+1)<wimp)THEN
               qout = 0.
            ELSE
               qout = max(0.,(vol_liq(j)-ssi*eff_porosity(j))*dz_soisno(j))
               qout = min(qout,(1.-vol_ice(j+1)-vol_liq(j+1))*dz_soisno(j+1))
            ENDIF
         ELSE
            qout = max(0.,(vol_liq(j)-ssi*eff_porosity(j))*dz_soisno(j))
         ENDIF

         qout = qout*1000.
         wliq_soisno(j) = wliq_soisno(j) - qout
         qin = qout

      ENDDO

      qout_snowb = qout/deltim

   END SUBROUTINE snowwater


!-----------------------------------------------------------------------
   SUBROUTINE SnowWater_snicar (lb,deltim,ssi,wimp, &
                        pg_rain,qseva,qsdew,qsubl,qfros, &
                        dz_soisno,wice_soisno,wliq_soisno,qout_snowb, &

! Aerosol Fluxes (Jan. 07, 2023)
                        forc_aer, &
                        mss_bcpho, mss_bcphi, mss_ocpho, mss_ocphi, &
                        mss_dst1,  mss_dst2,  mss_dst3,  mss_dst4 )
! Aerosol Fluxes (Jan. 07, 2023)


!-----------------------------------------------------------------------
!  Original author: Yongjiu Dai, /09/1999, /04/2014, /01/2023/
!
!  Water flow within snow is computed by an explicit and non-physical based
!  scheme, which permits a part of liquid water over the holding capacity (a
!  tentative value is used, i.e., equal to 0.033*porosity) to percolate into the
!  underlying layer, except the case of that the porosity of one of the two
!  neighboring layers is less than 0.05, the zero flow is assumed. The water
!  flow out of the bottom snow pack will participate as the input of the soil
!  water and runoff.
!
! !REVISIONS:
!  Yongjiu Dai, 01/2023: added Aerosol fluxes from SNICAR model
!-----------------------------------------------------------------------

   IMPLICIT NONE

   real(r8), parameter :: denice = 917.0_r8  ! density of ice [kg/m3]
   real(r8), parameter :: denh2o = 1000.0_r8 ! density of liquid water [kg/m3]

!-------------------------- Dummy Arguments ----------------------------
   integer, intent(in) :: &
        lb          ! lower bound of array

   real(r8), intent(in) :: &
        deltim,    &! seconds in a time step (s)
        ssi,       &! irreducible water saturation of snow
        wimp,      &! water impermeable if porosity less than wimp
        dz_soisno(lb:0),  &! layer thickness (m)

        pg_rain,   &! rainfall after removal of interception (mm h2o/s)
        qseva,     &! ground surface evaporation rate (mm h2o/s)
        qsdew,     &! ground surface dew formation (mm h2o /s) [+]
        qsubl,     &! sublimation rate from snow pack (mm h2o /s) [+]
        qfros       ! surface dew added to snow pack (mm h2o /s) [+]

   real(r8), intent(inout) :: &
        wice_soisno(lb:0),&! ice lens (kg/m2)
        wliq_soisno(lb:0)  ! liquid water (kg/m2)

   real(r8), intent(out) :: &
        qout_snowb  ! rate of water out of snow bottom (mm/s)

!  Aerosol Fluxes (Jan. 07, 2023)
   real(r8), intent(in) :: forc_aer ( 14 )  ! aerosol deposition from atmosphere model (grd,aer) [kg m-1 s-1]

   real(r8), intent(inout) :: &
        mss_bcpho (lb:0), &! mass of hydrophobic BC in snow  (col,lyr) [kg]
        mss_bcphi (lb:0), &! mass of hydrophillic BC in snow (col,lyr) [kg]
        mss_ocpho (lb:0), &! mass of hydrophobic OC in snow  (col,lyr) [kg]
        mss_ocphi (lb:0), &! mass of hydrophillic OC in snow (col,lyr) [kg]
        mss_dst1  (lb:0), &! mass of dust species 1 in snow  (col,lyr) [kg]
        mss_dst2  (lb:0), &! mass of dust species 2 in snow  (col,lyr) [kg]
        mss_dst3  (lb:0), &! mass of dust species 3 in snow  (col,lyr) [kg]
        mss_dst4  (lb:0)   ! mass of dust species 4 in snow  (col,lyr) [kg]
!  Aerosol Fluxes (Jan. 07, 2023)

!-------------------------- Local Variables ----------------------------
   integer j         ! do loop/array indices

   real(r8) :: &
       qin,               &! water flow into the element (mm/s)
       qout,              &! water flow out of the element (mm/s)
       zwice,             &! the sum of ice mass of snow cover (kg/m2)
       wgdif,             &! ice mass after minus sublimation
       vol_liq(lb:0),     &! partial volume of liquid water in layer
       vol_ice(lb:0),     &! partial volume of ice lens in layer
       eff_porosity(lb:0)  ! effective porosity = porosity - vol_ice

! Aerosol Fluxes (Jan. 07, 2023)
   !  Aerosol species indices:
   !  1= hydrophillic (bulk model) or within-ice (modal model) black carbon
   !  2= hydrophobic (bulk model) or external (modal model) black carbon
   !  3= hydrophilic organic carbon
   !  4= hydrophobic organic carbon
   !  5= dust species 1
   !  6= dust species 2
   !  7= dust species 3
   !  8= dust species 4
   !
   real(r8), parameter :: scvng_fct_mlt_bcphi = 0.20 ! scavenging factor for hydrophillic BC inclusion in meltwater [frc]
   real(r8), parameter :: scvng_fct_mlt_bcpho = 0.03 ! scavenging factor for hydrophobic BC inclusion in meltwater  [frc]
   real(r8), parameter :: scvng_fct_mlt_ocphi = 0.20 ! scavenging factor for hydrophillic OC inclusion in meltwater [frc]
   real(r8), parameter :: scvng_fct_mlt_ocpho = 0.03 ! scavenging factor for hydrophobic OC inclusion in meltwater  [frc]
   real(r8), parameter :: scvng_fct_mlt_dst1  = 0.02 ! scavenging factor for dust species 1 inclusion in meltwater  [frc]
   real(r8), parameter :: scvng_fct_mlt_dst2  = 0.02 ! scavenging factor for dust species 2 inclusion in meltwater  [frc]
   real(r8), parameter :: scvng_fct_mlt_dst3  = 0.01 ! scavenging factor for dust species 3 inclusion in meltwater  [frc]
   real(r8), parameter :: scvng_fct_mlt_dst4  = 0.01 ! scavenging factor for dust species 4 inclusion in meltwater  [frc]

   ! !LOCAL VARIABLES:
   real(r8) :: qin_bc_phi       ! flux of hydrophilic BC into   layer [kg]
   real(r8) :: qout_bc_phi      ! flux of hydrophilic BC out of layer [kg]
   real(r8) :: qin_bc_pho       ! flux of hydrophobic BC into   layer [kg]
   real(r8) :: qout_bc_pho      ! flux of hydrophobic BC out of layer [kg]
   real(r8) :: qin_oc_phi       ! flux of hydrophilic OC into   layer [kg]
   real(r8) :: qout_oc_phi      ! flux of hydrophilic OC out of layer [kg]
   real(r8) :: qin_oc_pho       ! flux of hydrophobic OC into   layer [kg]
   real(r8) :: qout_oc_pho      ! flux of hydrophobic OC out of layer [kg]
   real(r8) :: qin_dst1         ! flux of dust species 1 into   layer [kg]
   real(r8) :: qout_dst1        ! flux of dust species 1 out of layer [kg]
   real(r8) :: qin_dst2         ! flux of dust species 2 into   layer [kg]
   real(r8) :: qout_dst2        ! flux of dust species 2 out of layer [kg]
   real(r8) :: qin_dst3         ! flux of dust species 3 into   layer [kg]
   real(r8) :: qout_dst3        ! flux of dust species 3 out of layer [kg]
   real(r8) :: qin_dst4         ! flux of dust species 4 into   layer [kg]
   real(r8) :: qout_dst4        ! flux of dust species 4 out of layer [kg]
   real(r8) :: mss_liqice(lb:0) ! mass of liquid+ice in a layer

   real(r8) :: subsnow          ! sublimated snow [kg m-2]
   real(r8) :: frc_sub          ! fraction of layer mass that has sublimated [frc]
   real(r8) :: frc_transfer     ! frc_refrz + frc_sub
   real(r8) :: dm_int           ! mass transfer [kg]

   ! !LOCAL VARIABLES for AerosolFluxes
   real(r8) :: flx_bc_dep       ! total BC deposition        (col) [kg m-2 s-1]
   real(r8) :: flx_bc_dep_phi   ! hydrophillic BC deposition (col) [kg m-1 s-1]
   real(r8) :: flx_bc_dep_pho   ! hydrophobic BC deposition  (col) [kg m-1 s-1]
   real(r8) :: flx_oc_dep       ! total OC deposition        (col) [kg m-2 s-1]
   real(r8) :: flx_oc_dep_phi   ! hydrophillic OC deposition (col) [kg m-1 s-1]
   real(r8) :: flx_oc_dep_pho   ! hydrophobic OC deposition  (col) [kg m-1 s-1]
   real(r8) :: flx_dst_dep      ! total dust deposition      (col) [kg m-2 s-1]

   real(r8) :: flx_dst_dep_wet1 ! wet dust (species 1) deposition (col) [kg m-2 s-1]
   real(r8) :: flx_dst_dep_dry1 ! dry dust (species 1) deposition (col) [kg m-2 s-1]
   real(r8) :: flx_dst_dep_wet2 ! wet dust (species 2) deposition (col) [kg m-2 s-1]
   real(r8) :: flx_dst_dep_dry2 ! dry dust (species 2) deposition (col) [kg m-2 s-1]
   real(r8) :: flx_dst_dep_wet3 ! wet dust (species 3) deposition (col) [kg m-2 s-1]
   real(r8) :: flx_dst_dep_dry3 ! dry dust (species 3) deposition (col) [kg m-2 s-1]
   real(r8) :: flx_dst_dep_wet4 ! wet dust (species 4) deposition (col) [kg m-2 s-1]
   real(r8) :: flx_dst_dep_dry4 ! dry dust (species 4) deposition (col) [kg m-2 s-1]
! Aerosol Fluxes (Jan. 07, 2023)

!=======================================================================
! renew the mass of ice lens (wice_soisno) and liquid (wliq_soisno) in the surface snow layer,
! resulted by sublimation (frost) / evaporation (condense)

      wgdif = wice_soisno(lb) + (qfros - qsubl)*deltim
      wice_soisno(lb) = wgdif
      IF(wgdif < 0.)THEN
         wice_soisno(lb) = 0.
         wliq_soisno(lb) = wliq_soisno(lb) + wgdif
      ENDIF

      wliq_soisno(lb) = wliq_soisno(lb) + (pg_rain + qsdew - qseva)*deltim
      IF (wliq_soisno(lb) < 0.) THEN
         wice_soisno(lb) = wice_soisno(lb) + wliq_soisno(lb)
         wice_soisno(lb) = max(wice_soisno(lb), 0.)
         wliq_soisno(lb) = 0.
      ENDIF

! Porosity and partial volume
      DO j = lb, 0
         vol_ice(j) = min(1., wice_soisno(j)/(dz_soisno(j)*denice))
         eff_porosity(j) = max(0.01, 1. - vol_ice(j))
         vol_liq(j) = min(eff_porosity(j), wliq_soisno(j)/(dz_soisno(j)*denh2o))
      ENDDO

! Capillary force within snow could be two or more orders of magnitude less
! than those of gravity, this term may be ignored.  Here we could keep the
! gravity term only. The general expression for water flow is "K * ss**3",
! however, no effective parameterization for "K". Thus, a very simple treatment
! (not physical based) is introduced: when the liquid water of layer exceeds
! the layer's holding capacity, the excess meltwater adds to the underlying
! neighbor layer.

! Aerosol Fluxes (Jan. 07, 2023)
! Also compute aerosol fluxes through snowpack in this loop:
! 1) compute aerosol mass in each layer
! 2) add aerosol mass flux from above layer to mass of this layer
! 3) qout_xxx is mass flux of aerosol species xxx out bottom of
!    layer in water flow, proportional to (current) concentration
!    of aerosol in layer multiplied by a scavenging ratio.
! 4) update mass of aerosol in top layer, accordingly
! 5) update mass concentration of aerosol accordingly

      qin        = 0._r8

! Aerosol Fluxes (Jan. 07, 2023)
      qin_bc_phi = 0._r8
      qin_bc_pho = 0._r8
      qin_oc_phi = 0._r8
      qin_oc_pho = 0._r8
      qin_dst1   = 0._r8
      qin_dst2   = 0._r8
      qin_dst3   = 0._r8
      qin_dst4   = 0._r8
! Aerosol Fluxes (Jan. 07, 2023)

      DO j= lb, 0

         wliq_soisno(j) = wliq_soisno(j) + qin

! Aerosol Fluxes (Jan. 07, 2023)
         mss_bcphi(j) = mss_bcphi(j) + qin_bc_phi
         mss_bcpho(j) = mss_bcpho(j) + qin_bc_pho
         mss_ocphi(j) = mss_ocphi(j) + qin_oc_phi
         mss_ocpho(j) = mss_ocpho(j) + qin_oc_pho

         mss_dst1(j)  = mss_dst1(j) + qin_dst1
         mss_dst2(j)  = mss_dst2(j) + qin_dst2
         mss_dst3(j)  = mss_dst3(j) + qin_dst3
         mss_dst4(j)  = mss_dst4(j) + qin_dst4
! Aerosol Fluxes (Jan. 07, 2023)

         IF(j <= -1)THEN
         ! no runoff over snow surface, just ponding on surface
            IF(eff_porosity(j)<wimp .or. eff_porosity(j+1)<wimp)THEN
               qout = 0._r8
            ELSE
               qout = max(0._r8,(vol_liq(j)-ssi*eff_porosity(j))*dz_soisno(j))
               qout = min(qout,(1.-vol_ice(j+1)-vol_liq(j+1))*dz_soisno(j+1))
            ENDIF
         ELSE
            qout = max(0._r8,(vol_liq(j)-ssi*eff_porosity(j))*dz_soisno(j))
         ENDIF

         qout = qout*1000._r8
         wliq_soisno(j) = wliq_soisno(j) - qout
         qin = qout

! Aerosol Fluxes (Jan. 07, 2023)
         ! mass of ice+water: in extremely rare circumstances, this can
         ! be zero, even though there is a snow layer defined. In
         ! this case, set the mass to a very small value to
         ! prevent division by zero.

         mss_liqice(j) = wliq_soisno(j)+wice_soisno(j)
         IF (mss_liqice(j) < 1E-30_r8) THEN
            mss_liqice(j) = 1E-30_r8
         ENDIF

         ! BCPHI:
         ! 1. flux with meltwater:
         qout_bc_phi = qout*scvng_fct_mlt_bcphi*(mss_bcphi(j)/mss_liqice(j))
         IF (qout_bc_phi > mss_bcphi(j)) THEN
            qout_bc_phi = mss_bcphi(j)
         ENDIF
         mss_bcphi(j) = mss_bcphi(j) - qout_bc_phi
         qin_bc_phi = qout_bc_phi

         ! BCPHO:
         ! 1. flux with meltwater:
         qout_bc_pho = qout*scvng_fct_mlt_bcpho*(mss_bcpho(j)/mss_liqice(j))
         IF (qout_bc_pho > mss_bcpho(j)) THEN
            qout_bc_pho = mss_bcpho(j)
         ENDIF
         mss_bcpho(j) = mss_bcpho(j) - qout_bc_pho
         qin_bc_pho = qout_bc_pho

         ! OCPHI:
         ! 1. flux with meltwater:
         qout_oc_phi = qout*scvng_fct_mlt_ocphi*(mss_ocphi(j)/mss_liqice(j))
         IF (qout_oc_phi > mss_ocphi(j)) THEN
            qout_oc_phi = mss_ocphi(j)
         ENDIF
         mss_ocphi(j) = mss_ocphi(j) - qout_oc_phi
         qin_oc_phi = qout_oc_phi

         ! OCPHO:
         ! 1. flux with meltwater:
         qout_oc_pho = qout*scvng_fct_mlt_ocpho*(mss_ocpho(j)/mss_liqice(j))
         IF (qout_oc_pho > mss_ocpho(j)) THEN
            qout_oc_pho = mss_ocpho(j)
         ENDIF
         mss_ocpho(j) = mss_ocpho(j) - qout_oc_pho
         qin_oc_pho = qout_oc_pho

         ! DUST 1:
         ! 1. flux with meltwater:
         qout_dst1 = qout*scvng_fct_mlt_dst1*(mss_dst1(j)/mss_liqice(j))
         IF (qout_dst1 > mss_dst1(j)) THEN
            qout_dst1 = mss_dst1(j)
         ENDIF
         mss_dst1(j) = mss_dst1(j) - qout_dst1
         qin_dst1 = qout_dst1

         ! DUST 2:
         ! 1. flux with meltwater:
         qout_dst2 = qout*scvng_fct_mlt_dst2*(mss_dst2(j)/mss_liqice(j))
         IF (qout_dst2 > mss_dst2(j)) THEN
            qout_dst2 = mss_dst2(j)
         ENDIF
         mss_dst2(j) = mss_dst2(j) - qout_dst2
         qin_dst2 = qout_dst2

         ! DUST 3:
         ! 1. flux with meltwater:
         qout_dst3 = qout*scvng_fct_mlt_dst3*(mss_dst3(j)/mss_liqice(j))
         IF (qout_dst3 > mss_dst3(j)) THEN
            qout_dst3 = mss_dst3(j)
         ENDIF
         mss_dst3(j) = mss_dst3(j) - qout_dst3
         qin_dst3 = qout_dst3

         ! DUST 4:
         ! 1. flux with meltwater:
         qout_dst4 = qout*scvng_fct_mlt_dst4*(mss_dst4(j)/mss_liqice(j))
         IF (qout_dst4 > mss_dst4(j)) THEN
            qout_dst4 = mss_dst4(j)
         ENDIF
         mss_dst4(j) = mss_dst4(j) - qout_dst4
         qin_dst4 = qout_dst4
! Aerosol Fluxes (Jan. 07, 2023)

      ENDDO

      qout_snowb = qout/deltim


! Aerosol Fluxes (Jan. 07, 2023)
! Compute aerosol fluxes through snowpack and aerosol deposition fluxes into top layere
!-----------------------------------------------------------------------
! set aerosol deposition fluxes from forcing array
! The forcing array is either set from an external file
! or from fluxes received from the atmosphere model
#ifdef MODAL_AER
      ! Mapping for modal aerosol scheme where within-hydrometeor and
      ! interstitial aerosol fluxes are differentiated. Here, "phi"
      ! flavors of BC and OC correspond to within-hydrometeor
      ! (cloud-borne) aerosol, and "pho" flavors are interstitial
      ! aerosol. "wet" and "dry" fluxes of BC and OC specified here are
      ! purely diagnostic
      !
      ! NOTE: right now the macro 'MODAL_AER' is not defined anywhere, i.e.,
      ! the below (modal aerosol scheme) is not available and can not be
      ! active either. It depends on the specific input aerosol deposition
      ! data which is suitable for modal scheme. [06/15/2023, Hua Yuan]

      flx_bc_dep_phi   = forc_aer(3)
      flx_bc_dep_pho   = forc_aer(1) + forc_aer(2)
      flx_bc_dep       = forc_aer(1) + forc_aer(2) + forc_aer(3)

      flx_oc_dep_phi   = forc_aer(6)
      flx_oc_dep_pho   = forc_aer(4) + forc_aer(5)
      flx_oc_dep       = forc_aer(4) + forc_aer(5) + forc_aer(6)

      flx_dst_dep_wet1 = forc_aer(7)
      flx_dst_dep_dry1 = forc_aer(8)
      flx_dst_dep_wet2 = forc_aer(9)
      flx_dst_dep_dry2 = forc_aer(10)
      flx_dst_dep_wet3 = forc_aer(11)
      flx_dst_dep_dry3 = forc_aer(12)
      flx_dst_dep_wet4 = forc_aer(13)
      flx_dst_dep_dry4 = forc_aer(14)
      flx_dst_dep      = forc_aer(7)  + forc_aer(8)  + forc_aer(9) + &
                         forc_aer(10) + forc_aer(11) + forc_aer(12) + &
                         forc_aer(13) + forc_aer(14)
#else
      ! Original mapping for bulk aerosol deposition. phi and pho BC/OC
      ! species are distinguished in model, other fluxes (e.g., dry and
      ! wet BC/OC) are purely diagnostic.

      flx_bc_dep_phi   = forc_aer(1) + forc_aer(3)
      flx_bc_dep_pho   = forc_aer(2)
      flx_bc_dep       = forc_aer(1) + forc_aer(2) + forc_aer(3)

      flx_oc_dep_phi   = forc_aer(4) + forc_aer(6)
      flx_oc_dep_pho   = forc_aer(5)
      flx_oc_dep       = forc_aer(4) + forc_aer(5) + forc_aer(6)

      flx_dst_dep_wet1 = forc_aer(7)
      flx_dst_dep_dry1 = forc_aer(8)
      flx_dst_dep_wet2 = forc_aer(9)
      flx_dst_dep_dry2 = forc_aer(10)
      flx_dst_dep_wet3 = forc_aer(11)
      flx_dst_dep_dry3 = forc_aer(12)
      flx_dst_dep_wet4 = forc_aer(13)
      flx_dst_dep_dry4 = forc_aer(14)
      flx_dst_dep      = forc_aer(7)  + forc_aer(8)  + forc_aer(9) + &
                         forc_aer(10) + forc_aer(11) + forc_aer(12) + &
                         forc_aer(13) + forc_aer(14)
#endif

      ! aerosol deposition fluxes into top layer
      ! This is done after the inter-layer fluxes so that some aerosol
      ! is in the top layer after deposition, and is not immediately
      ! washed out before radiative calculations are done

      mss_bcphi(lb) = mss_bcphi(lb) + (flx_bc_dep_phi*deltim)
      mss_bcpho(lb) = mss_bcpho(lb) + (flx_bc_dep_pho*deltim)
      mss_ocphi(lb) = mss_ocphi(lb) + (flx_oc_dep_phi*deltim)
      mss_ocpho(lb) = mss_ocpho(lb) + (flx_oc_dep_pho*deltim)

      mss_dst1(lb) = mss_dst1(lb) + (flx_dst_dep_dry1 + flx_dst_dep_wet1)*deltim
      mss_dst2(lb) = mss_dst2(lb) + (flx_dst_dep_dry2 + flx_dst_dep_wet2)*deltim
      mss_dst3(lb) = mss_dst3(lb) + (flx_dst_dep_dry3 + flx_dst_dep_wet3)*deltim
      mss_dst4(lb) = mss_dst4(lb) + (flx_dst_dep_dry4 + flx_dst_dep_wet4)*deltim

#ifdef MODAL_AER
      !
      ! Transfer BC and OC from the within-ice state to the external
      ! state based on snow sublimation and re-freezing of liquid water.
      ! Re-freezing effect is inactived by default because of
      ! uncertainty in how this process operates.

      DO j= lb, 0
         IF (j >= lb) THEN
            IF (j == lb) THEN
               ! snow that has sublimated [kg/m2] (top layer only)
               subsnow = max(0._r8, (qsubl*deltim))

               ! fraction of layer mass that has sublimated:
               IF ((wliq_soisno(j) + wice_soisno(j)) > 0._r8) THEN
                  frc_sub = subsnow / (wliq_soisno(j) + wice_soisno(j))
               ELSE
                  frc_sub = 0._r8
               ENDIF
            ELSE
               ! prohibit sublimation effect to operate on sub-surface layers:
               frc_sub = 0._r8
            ENDIF

            ! fraction of layer mass transformed (sublimation only)
            frc_transfer = frc_sub

            ! cap the fraction at 1
            IF (frc_transfer > 1._r8) THEN
               frc_transfer = 1._r8
            ENDIF

            ! transfer proportionate mass of BC and OC:
            dm_int       = mss_bcphi(j)*frc_transfer
            mss_bcphi(j) = mss_bcphi(j) - dm_int
            mss_bcpho(j) = mss_bcpho(j) + dm_int

            dm_int       = mss_ocphi(j)*frc_transfer
            mss_ocphi(j) = mss_ocphi(j) - dm_int
            mss_ocpho(j) = mss_ocpho(j) + dm_int

         ENDIF
      ENDDO
#endif
! Aerosol Fluxes (Jan. 7, 2023)

   END SUBROUTINE SnowWater_snicar


   SUBROUTINE soilwater(patchtype,nl_soil,deltim,wimp,smpmin,&
                        qinfl,etr,z_soisno,dz_soisno,zi_soisno,&
                        t_soisno,vol_liq,vol_ice,smp,hk,icefrac,eff_porosity,&
                        porsl,hksati,bsw,psi0,rootr,rootflux,&
                        zwt,dwat,qcharge)

!-----------------------------------------------------------------------
!  Original author: Yongjiu Dai, 09/1999, 04/2014, 07/2014
!
!  some new parameterization are added, which are based on CLM4.5
!
!  Soil moisture is predicted from a 10-layer model (as with soil
!  temperature), in which the vertical soil moisture transport is governed
!  by infiltration, runoff, gradient diffusion, gravity, and root
!  extraction through canopy transpiration. The net water applied to the
!  surface layer is the snowmelt plus precipitation plus the throughfall
!  of canopy dew minus surface runoff and evaporation.
!
!  The vertical water flow in an unsaturated porous media is described by
!  Darcy's law, and the hydraulic conductivity and the soil negative
!  potential vary with soil water content and soil texture based on the work
!  of Clapp and Hornberger (1978) and Cosby et al. (1984). The equation is
!  integrated over the layer thickness, in which the time rate of change in
!  water mass must equal the net flow across the bounding interface, plus the
!  rate of internal source or sink. The terms of water flow across the layer
!  interfaces are linearly expanded by using first-order Taylor expansion.
!  The equations result in a tridiagonal system equation.
!
!  Note: length units here are all millimeter
!  (in temperature SUBROUTINE uses same soil layer
!  structure required but lengths are m)
!
!  Richards equation:
!
!  d wat     d     d psi
!  ----- =  -- [ k(----- - 1) ] + S
!    dt     dz       dz
!
!  where: wat = volume of water per volume of soil (mm**3/mm**3)
!  psi = soil matrix potential (mm)
!  dt  = time step (s)
!  z   = depth (mm) (positive downward)
!  dz  = thickness (mm)
!  qin = inflow at top (mm h2o /s)
!  qout= outflow at bottom (mm h2o /s)
!  s   = source/sink flux (mm h2o /s)
!  k   = hydraulic conductivity (mm h2o /s)
!
!                        d qin                  d qin
!  qin[n+1] = qin[n] +  --------  d wat(j-1) + --------- d wat(j)
!                        d wat(j-1)             d wat(j)
!                 ==================|=================
!                                   < qin
!
!                  d wat(j)/dt * dz = qin[n+1] - qout[n+1] + S(j)
!
!                                   > qout
!                 ==================|=================
!                         d qout               d qout
!  qout[n+1] = qout[n] + --------- d wat(j) + --------- d wat(j+1)
!                         d wat(j)             d wat(j+1)
!
!
!  Solution: linearize k and psi about d wat and use tridiagonal
!  system of equations to solve for d wat,
!  where for layer j
!
!
!  r_j = a_j [d wat_j-1] + b_j [d wat_j] + c_j [d wat_j+1]
!
!-----------------------------------------------------------------------
   USE MOD_Precision
   USE MOD_Const_Physical, only: grav,hfus,tfrz,denh2o,denice
   USE MOD_Utils

   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   integer , intent(in) :: patchtype ! land patch type
   integer , intent(in) :: nl_soil   ! number of soil layers
   real(r8), intent(in) :: deltim    ! land model time step (sec)
   real(r8), intent(in) :: wimp      ! water impermeable if porosity less than wimp
   real(r8), intent(in) :: smpmin    ! restriction for min of soil potential (mm)

   real(r8), intent(in) :: qinfl     ! infiltration (mm H2O /s)
   real(r8), intent(in) :: etr       ! vegetation transpiration (mm H2O/s) (+ = to atm)

   real(r8), intent(in) :: z_soisno (1:nl_soil) ! layer depth (m)
   real(r8), intent(in) :: dz_soisno(1:nl_soil) ! layer thickness (m)
   real(r8), intent(in) :: zi_soisno(0:nl_soil) ! interface level below a "z" level (m)

   real(r8), intent(in) :: t_soisno (1:nl_soil) ! soil temperature (Kelvin)
   real(r8), intent(in) :: vol_liq  (1:nl_soil) ! liquid volumetric water content
   real(r8), intent(in) :: vol_ice  (1:nl_soil) ! ice volumetric water content
   real(r8), intent(in) :: icefrac  (1:nl_soil)
   real(r8), intent(in) :: eff_porosity(1:nl_soil) ! effective porosity = porosity - vol_ice

   real(r8), intent(in) :: porsl  (1:nl_soil) ! volumetric soil water at saturation (porosity)
   real(r8), intent(in) :: hksati (1:nl_soil) ! hydraulic conductivity at saturation (mm H2O /s)
   real(r8), intent(in) :: bsw    (1:nl_soil) ! Clapp and Hornberger "b"
   real(r8), intent(in) :: psi0   (1:nl_soil) ! minimum soil suction (mm) [-]
   real(r8), intent(in) :: rootr  (1:nl_soil) ! effective fraction of roots in each soil layer
   real(r8), intent(in) :: rootflux(1:nl_soil)! root uptake from different layers, all layers add to transpiration
   real(r8), intent(in) :: zwt                ! the depth from ground (soil) surface to water table [m]

   real(r8), intent(out) :: dwat(1:nl_soil)   ! change of soil water [m3/m3]
   real(r8), intent(out) :: qcharge           ! aquifer recharge rate (positive to aquifer) (mm/s)
   real(r8), intent(out) :: smp(1:nl_soil)    ! soil matrix potential [mm]
   real(r8), intent(out) :: hk (1:nl_soil)    ! hydraulic conductivity [mm h2o/s]

!-------------------------- Local Variables ----------------------------

   integer  :: j                 ! do loop indices
   real(r8) :: amx(1:nl_soil)    ! "a" left off diagonal of tridiagonal matrix
   real(r8) :: bmx(1:nl_soil)    ! "b" diagonal column for tridiagonal matrix
   real(r8) :: cmx(1:nl_soil)    ! "c" right off diagonal tridiagonal matrix
   real(r8) :: rmx(1:nl_soil)    ! "r" forcing term of tridiagonal matrix
   real(r8) :: zmm(1:nl_soil)    ! layer depth [mm]
   real(r8) :: dzmm(1:nl_soil)   ! layer thickness [mm]
   real(r8) :: zimm(0:nl_soil)   ! layer interface depth [mm]
   real(r8) :: den(1:nl_soil)    ! used in calculating qin, qout
   real(r8) :: alpha(1:nl_soil)  ! used in calculating qin, qout
   real(r8) :: qin(1:nl_soil)    ! flux of water into soil layer [mm h2o/s]
   real(r8) :: qout(1:nl_soil)   ! flux of water out of soil layer [mm h2o/s]
   real(r8) :: dqidw0(1:nl_soil) ! d(qin)/d(vol_liq(j-1))
   real(r8) :: dqidw1(1:nl_soil) ! d(qin)/d(vol_liq(j))
   real(r8) :: dqodw1(1:nl_soil) ! d(qout)/d(vol_liq(j))
   real(r8) :: dqodw2(1:nl_soil) ! d(qout)/d(vol_liq(j+1))
   real(r8) :: dsmpdw(1:nl_soil) ! d(smp)/d(vol_liq)
   real(r8) :: s_node            ! soil wetness
   real(r8) :: s1                ! "s" at interface of layer
   real(r8) :: s2                ! k*s**(2b+2)
   real(r8) :: dhkdw1(1:nl_soil) ! d(hk)/d(vol_liq(j))
   real(r8) :: dhkdw2(1:nl_soil) ! d(hk)/d(vol_liq(j+1))
   real(r8) :: imped(1:nl_soil)  !
   real(r8) :: errorw            ! mass balance error for this time step

   integer  :: jwt               ! index of the soil layer right above the water table (-)

   real(r8), parameter :: e_ice=6.0      !soil ice impedance factor
!-----------------------------------------------------------------------

      !compute jwt index
      ! The layer index of the first unsaturated layer,
      ! i.e., the layer right above the water table

      jwt = nl_soil
      ! allow jwt to equal zero when zwt is in top layer
      DO j = 1, nl_soil
         IF(zwt <= zi_soisno(j)) THEN
            jwt = j-1
            EXIT
         ENDIF
      ENDDO

      ! Because the depths in this routine are in mm, use local
      ! variable arrays instead of pointers
      DO j = 1, nl_soil
         zmm(j) = z_soisno(j)*1000.
         dzmm(j) = dz_soisno(j)*1000.
         zimm(j) = zi_soisno(j)*1000.
      ENDDO

      zimm(0) = 0.0

      ! Compute matric potential and derivative based on liquid water content only
      DO j = 1, nl_soil
         IF(DEF_USE_PLANTHYDRAULICS .and. (patchtype/=1 .or. (.not.DEF_URBAN_RUN)))THEN
            IF(t_soisno(j)>=tfrz) THEN
               IF(porsl(j)<1.e-6)THEN     ! bed rock
                  s_node = 0.001
                  smp(j) = psi0(j)
                  dsmpdw(j) = 0.
               ELSE
                  s_node = max(vol_liq(j)/porsl(j),0.01)
                  s_node = min(1.0,s_node)
                  smp(j) = psi0(j)*s_node**(-bsw(j))
                  smp(j) = max(smpmin,smp(j))
                  dsmpdw(j) = -bsw(j)*smp(j)/(s_node*porsl(j))
               ENDIF
            ELSE
               ! when ice is present, the matric potential is only related to temperature
               ! by (Fuchs et al., 1978: Soil Sci. Soc. Amer. J. 42(3):379-385)
               ! Unit 1 Joule = 1 (kg m2/s2), J/kg /(m/s2) ==> m ==> 1e3 mm
               smp(j) = 1.e3 * 0.3336e6/9.80616*(t_soisno(j)-tfrz)/t_soisno(j)
               smp(j) = max(smpmin, smp(j))        ! Limit soil suction
               dsmpdw(j) = 0.
            ENDIF
         ELSE
            IF(t_soisno(j)>=tfrz) THEN
               IF(porsl(j)<1.e-6)THEN     ! bed rock
                  s_node = 0.001
                  smp(j) = psi0(j)
                  dsmpdw(j) = 0.
               ELSE
                  s_node = max(vol_liq(j)/porsl(j),0.01)
                  s_node = min(1.0,s_node)
                  smp(j) = psi0(j)*s_node**(-bsw(j))
                  smp(j) = max(smpmin,smp(j))
                  dsmpdw(j) = -bsw(j)*smp(j)/(s_node*porsl(j))
               ENDIF
            ELSE
               ! when ice is present, the matric potential is only related to temperature
               ! by (Fuchs et al., 1978: Soil Sci. Soc. Amer. J. 42(3):379-385)
               ! Unit 1 Joule = 1 (kg m2/s2), J/kg /(m/s2) ==> m ==> 1e3 mm
               smp(j) = 1.e3 * 0.3336e6/9.80616*(t_soisno(j)-tfrz)/t_soisno(j)
               smp(j) = max(smpmin, smp(j))        ! Limit soil suction
               dsmpdw(j) = 0.
            ENDIF
         ENDIF
      ENDDO

      ! Hydraulic conductivity and soil matric potential and their derivatives
      DO j = 1, nl_soil

         IF(j < nl_soil)THEN
            den(j) = (zmm(j+1)-zmm(j))
            alpha(j) = (smp(j+1)-smp(j))/den(j) - 1.
         ELSE
            den(j) = 0.        ! not used
            alpha(j) = 0.      ! not used
         ENDIF

         IF((eff_porosity(j) < wimp) .or. (eff_porosity(min(nl_soil,j+1)) < wimp) &
                                     .or. (vol_liq(j) <= 1.e-3))THEN
            imped(j) = 0.
            hk(j) = 0.
            dhkdw1(j) = 0.
            dhkdw2(j) = 0.
         ELSE
            ! The average conductivity between two heterogeneous medium layers (j and j + 1),
            ! are computed using different methods
            IF(j < nl_soil)THEN
! Method I: UPSTREAM MEAN
               IF(alpha(j) <= 0.)THEN
                  hk(j) = hksati(j) * (vol_liq(j)/porsl(j))**(2.*bsw(j)+3.)
                  dhkdw1(j) = hksati(j) * (2.*bsw(j)+3.)*(vol_liq(j)/porsl(j))**(2.*bsw(j)+2.)/porsl(j)
                  dhkdw2(j) = 0.
               ELSE
                  hk(j) = hksati(j+1) * (vol_liq(j+1)/porsl(j+1))**(2.*bsw(j+1)+3.)
                  dhkdw1(j) = 0.
                  dhkdw2(j) = hksati(j+1) * (2.*bsw(j+1)+3.)*(vol_liq(j+1)/porsl(j+1))**(2.*bsw(j+1)+2.)/porsl(j+1)
               ENDIF
! Method II:
            !  ! The harmonic averaging of the saturated conductivities
            !  hksat_interface = (zmm(j+1)-zmm(j))/((zimm(j)-zmm(j))/hksati(j)+(zmm(j+1)-zimm(j))/hksati(j+1))
            !  s1 = (vol_liq(j)*(zimm(j)-zmm(j)) + vol_liq(j+1)*(zmm(j+1)-zimm(j))) &
            !     / (porsl(j)*(zimm(j)-zmm(j)) + porsl(j+1)*(zmm(j+1)-zimm(j)))
            !  s1 = min(1.,s1)
            !  s2 = hksat_interface*s1**(2.*bsw(j)+2.)
            !  hk(j) = s1*s2
            !  dhkdw1(j) = (2.*bsw(j)+3.)*s2*(zimm(j)-zmm(j))/(porsl(j)*(zimm(j)-zmm(j))+porsl(j+1)*(zmm(j+1)-zimm(j)))
            !  dhkdw2(j) = (2.*bsw(j)+3.)*s2*(zmm(j+1)-zimm(j))/(porsl(j)*(zimm(j)-zmm(j))+porsl(j+1)*(zmm(j+1)-zimm(j)))

            ELSE
               hk(j) = hksati(j) * (vol_liq(j)/porsl(j))**(2.*bsw(j)+3.)
               dhkdw1(j) = hksati(j) * (2.*bsw(j)+3.)*(vol_liq(j)/porsl(j))**(2.*bsw(j)+2.)/porsl(j)
               dhkdw2(j) = 0.
            ENDIF

            ! replace fracice with impedance factor
            imped(j)=10.**(-e_ice*(0.5*(icefrac(j)+icefrac(min(nl_soil,j+1)))))
            hk(j) = imped(j) * hk(j)
            dhkdw1(j) = imped(j) * dhkdw1(j)
            dhkdw2(j) = imped(j) * dhkdw2(j)
         ENDIF
      ENDDO


      ! Set up r, a, b, and c vectors for tridiagonal solution

      ! Node j=1 (top)

      j = 1
      qin(j) = qinfl

      qout(j) = -hk(j)*alpha(j)
      dqodw1(j) = -(alpha(j)*dhkdw1(j) - hk(j)*dsmpdw(j)/den(j))
      dqodw2(j) = -(alpha(j)*dhkdw2(j) + hk(j)*dsmpdw(j+1)/den(j))

      amx(j) = 0.
      bmx(j) = dzmm(j)/deltim + dqodw1(j)
      cmx(j) = dqodw2(j)
      IF(DEF_USE_PLANTHYDRAULICS .and. (patchtype/=1 .or. (.not.DEF_URBAN_RUN)))THEN
         rmx(j) =  qin(j) - qout(j) - rootflux(j)
      ELSE
         rmx(j) =  qin(j) - qout(j) - etr*rootr(j)
      ENDIF

      ! Nodes j=2 to j=nl_soil-1

      DO j = 2, nl_soil - 1
         qin(j) = -hk(j-1)*alpha(j-1)
         dqidw0(j) = -(alpha(j-1)*dhkdw1(j-1) - hk(j-1)*dsmpdw(j-1)/den(j-1))
         dqidw1(j) = -(alpha(j-1)*dhkdw2(j-1) + hk(j-1)*dsmpdw(j)/den(j-1))

         qout(j) = -hk(j)*alpha(j)
         dqodw1(j) = -(alpha(j)*dhkdw1(j) - hk(j)*dsmpdw(j)/den(j))
         dqodw2(j) = -(alpha(j)*dhkdw2(j) + hk(j)*dsmpdw(j+1)/den(j))

         amx(j) = -dqidw0(j)
         bmx(j) =  dzmm(j)/deltim - dqidw1(j) + dqodw1(j)
         cmx(j) =  dqodw2(j)
         IF(DEF_USE_PLANTHYDRAULICS .and. (patchtype/=1 .or. (.not.DEF_URBAN_RUN)))THEN
            rmx(j) =  qin(j) - qout(j) - rootflux(j)
         ELSE
            rmx(j) =  qin(j) - qout(j) - etr*rootr(j)
         ENDIF
      ENDDO

      ! Node j=nl_soil (bottom)

      j = nl_soil
      qin(j) = -hk(j-1)*alpha(j-1)
      dqidw0(j) = -(alpha(j-1)*dhkdw1(j-1) - hk(j-1)*dsmpdw(j-1)/den(j-1))
      dqidw1(j) = -(alpha(j-1)*dhkdw2(j-1) + hk(j-1)*dsmpdw(j)/den(j-1))

  !   IF(j > jwt) THEN ! water table is in soil column
  !      qout(j) = 0.
  !      dqodw1(j) = 0.
  !      dqodw2(j) = 0.
  !   ELSE
         qout(j) = hk(j)
         dqodw1(j) = dhkdw1(j)
         dqodw2(j) = 0.
  !   ENDIF

      amx(j) = -dqidw0(j)
      bmx(j) =  dzmm(j)/deltim - dqidw1(j) + dqodw1(j)
      cmx(j) =  dqodw2(j)
      IF(DEF_USE_PLANTHYDRAULICS .and. (patchtype/=1 .or. (.not.DEF_URBAN_RUN)))THEN
         rmx(j) =  qin(j) - qout(j) - rootflux(j)
      ELSE
         rmx(j) =  qin(j) - qout(j) - etr*rootr(j)
      ENDIF

      ! Solve for dwat

      CALL tridia (nl_soil, amx, bmx, cmx, rmx, dwat )

#if (defined CoLMDEBUG)
  ! The mass balance error (mm) for this time step is
      errorw = -deltim*(qin(1)-qout(nl_soil)-dqodw1(nl_soil)*dwat(nl_soil))
      DO j = 1, nl_soil
         IF(DEF_USE_PLANTHYDRAULICS .and. (patchtype/=1 .or. (.not.DEF_URBAN_RUN)))THEN
            errorw = errorw+dwat(j)*dzmm(j)+rootflux(j)*deltim
         ELSE
            errorw = errorw+dwat(j)*dzmm(j)+etr*rootr(j)*deltim
         ENDIF
      ENDDO

      IF(abs(errorw) > 1.e-3)THEN
         write(6,*) 'mass balance error in time step =',errorw
      ENDIF
#endif

      ! Recharge rate qcharge to groundwater (positive to aquifer)
      qcharge = qout(nl_soil) + dqodw1(nl_soil)*dwat(nl_soil)


   END SUBROUTINE soilwater


   SUBROUTINE groundwater (nl_soil,deltim,pondmx,&
                           eff_porosity,icefrac,&
                           dz_soisno,zi_soisno,wice_soisno,wliq_soisno,&
                           porsl,psi0,bsw,zwt,wa,&
                           qcharge,rsubst)

! -------------------------------------------------------------------------


   USE MOD_Precision
   USE MOD_Const_Physical, only: tfrz
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   integer , intent(in) :: nl_soil      !
   real(r8), intent(in) :: deltim       ! land model time step (sec)
   real(r8), intent(in) :: pondmx       !

   real(r8), intent(in) :: eff_porosity(1:nl_soil)   ! effective porosity = porosity - vol_ice
   real(r8), intent(in) :: icefrac(1:nl_soil)        ! ice fraction (-)

   real(r8), intent(in) :: dz_soisno  (1:nl_soil)    ! layer depth (m)
   real(r8), intent(in) :: zi_soisno  (0:nl_soil)    ! interface level below a "z" level (m)
   real(r8), intent(inout) :: wice_soisno(1:nl_soil) ! ice lens (kg/m2)
   real(r8), intent(inout) :: wliq_soisno(1:nl_soil) ! liquid water (kg/m2)

   real(r8), intent(in) :: porsl(1:nl_soil)          ! volumetric soil water at saturation (porosity)
   real(r8), intent(in) :: psi0(1:nl_soil)           ! minimum soil suction (mm) [-]
   real(r8), intent(in) :: bsw(1:nl_soil)            ! Clapp and Hornberger "b"

   real(r8), intent(inout) :: zwt       ! the depth from ground (soil) surface to water table [m]
   real(r8), intent(inout) :: wa        ! water in the unconfined aquifer (mm)
   real(r8), intent(in)    :: qcharge   ! aquifer recharge rate (positive to aquifer) (mm/s)
   real(r8), intent(inout) :: rsubst    ! subsurface runoff (positive = out of soil column) (mm H2O /s)

!-------------------------- Local Variables ----------------------------
   integer  :: j                ! indices
   integer  :: jwt              ! index of the soil layer right above the water table (-)
   real(r8) :: xs               ! water needed to bring soil moisture to watmin (mm)
   real(r8) :: dzmm(1:nl_soil)  ! layer thickness (mm)
   real(r8) :: xsi              ! excess soil water above saturation at layer i (mm)
   real(r8) :: xsia             ! available pore space at layer i (mm)
   real(r8) :: xs1              ! excess soil water above saturation at layer 1 (mm)
   real(r8) :: ws               ! summation of pore space of layers below water table (mm)
   real(r8) :: s_node           ! soil wetness (-)
   real(r8) :: available_wliq_soisno     ! available soil liquid water in a layer
   real(r8) :: qcharge_tot      !
   real(r8) :: qcharge_layer    !
   real(r8) :: drainage         !
   real(r8) :: drainage_tot     !
   real(r8) :: drainage_layer   !
   real(r8) :: s_y              !
   real(r8) :: rous             ! specific yield [-]

   real(r8) :: wt
   real(r8) :: wtsub
   real(r8) :: dzsum
   real(r8) :: icefracsum
   real(r8) :: fracice_rsub
   real(r8) :: imped


   real(r8), parameter :: watmin = 0.01  ! Limit irreducible wrapping liquid water
                                         ! a tunable constant
   real(r8), parameter :: rsbmx  = 5.0   ! baseflow coefficient [mm/s]
   real(r8), parameter :: timean = 10.5  ! global mean topographic index

! -------------------------------------------------------------------------

!     ! Convert layer thicknesses from m to mm

      DO j = 1,nl_soil
         dzmm(j) = dz_soisno(j)*1000.
      ENDDO

!     ! The layer index of the first unsaturated layer,
!     ! i.e., the layer right above the water table

      jwt = nl_soil
      ! allow jwt to equal zero when zwt is in top layer
      DO j = 1, nl_soil
         IF(zwt <= zi_soisno(j)) THEN
            jwt = j-1
            EXIT
         ENDIF
      ENDDO

!============================== QCHARGE =========================================
! Water table changes due to qcharge
! use analytical expression for aquifer specific yield
      rous = porsl(nl_soil)*(1.-(1.-1.e3*zwt/psi0(nl_soil))**(-1./bsw(nl_soil)))
      rous = max(rous,0.02)

      wa = wa + qcharge*deltim
!
!---------------------------------------
      ! water table is below the soil column
      IF(jwt == nl_soil) THEN
         zwt = max(0.,zwt - (qcharge*deltim)/1000./rous)
      ELSE
      ! water table within soil layers 1-9
      ! try to raise water table to account for qcharge

         qcharge_tot = qcharge * deltim

         IF(qcharge_tot > 0.) THEN ! rising water table
            DO j = jwt+1, 1,-1
               ! use analytical expression for specific yield

               s_y = porsl(j) * (1.-(1.-1.e3*zwt/psi0(j))**(-1./bsw(j)))
               s_y=max(s_y,0.02)

               qcharge_layer = min(qcharge_tot,(s_y*(zwt-zi_soisno(j-1))*1.e3))
               qcharge_layer = max(qcharge_layer,0.)

               zwt = max(0.,zwt - qcharge_layer/s_y/1000.)

               qcharge_tot = qcharge_tot - qcharge_layer
               IF (qcharge_tot <= 0.) EXIT
            ENDDO
         ELSE ! deepening water table (negative qcharge)
            DO j = jwt+1, nl_soil
               ! use analytical expression for specific yield
               s_y = porsl(j) * (1.-(1.-1.e3*zwt/psi0(j))**(-1./bsw(j)))
               s_y=max(s_y,0.02)
               qcharge_layer = max(qcharge_tot,-(s_y*(zi_soisno(j) - zwt)*1.e3))
               qcharge_layer = min(qcharge_layer,0.)
               qcharge_tot = qcharge_tot - qcharge_layer

               IF (qcharge_tot >= 0.) THEN
                  zwt = max(0.,zwt - qcharge_layer/s_y/1000.)
                  EXIT
               ELSE
                  zwt = zi_soisno(j)
               ENDIF
            ENDDO
            IF (qcharge_tot > 0.) zwt = max(0.,zwt - qcharge_tot/1000./rous)
         ENDIF
      ENDIF

!-- Topographic runoff  ----------------------------------------------------------
      IF (DEF_Runoff_SCHEME == 0) THEN
         CALL SubsurfaceRunoff_TOPMOD (nl_soil, icefrac, dz_soisno, zi_soisno, zwt, rsubst)
      ENDIF

      drainage = rsubst

      ! dzsum = 0.
      ! icefracsum = 0.
      ! DO j = max(jwt,1), nl_soil
      !    dzsum = dzsum + dzmm(j)
      !    icefracsum = icefracsum + icefrac(j) * dzmm(j)
      ! ENDDO
      ! ! add ice impedance factor to baseflow
      ! fracice_rsub = max(0.,exp(-3.*(1.-(icefracsum/dzsum)))-exp(-3.))/(1.0-exp(-3.))
      ! imped = max(0.,1.-fracice_rsub)
      ! drainage = imped * 5.5e-3 * exp(-2.5*zwt)  ! drainage (positive = out of soil column)

!-- Water table is below the soil column  ----------------------------------------
      IF(jwt == nl_soil) THEN
         wa = wa - drainage * deltim
         zwt = max(0.,zwt + (drainage * deltim)/1000./rous)
         wliq_soisno(nl_soil) = wliq_soisno(nl_soil) + max(0.,(wa-5000.))
         wa = min(wa, 5000.)
      ELSE
!-- Water table within soil layers 1-9  ------------------------------------------
!============================== RSUB_TOP =========================================
       !-- Now remove water via drainage
         drainage_tot = - drainage * deltim
         DO j = jwt+1, nl_soil
          ! use analytical expression for specific yield
            s_y = porsl(j) * ( 1. - (1.-1.e3*zwt/psi0(j))**(-1./bsw(j)))
            s_y = max(s_y,0.02)

            drainage_layer = max(drainage_tot, -(s_y*(zi_soisno(j)-zwt)*1.e3))
            drainage_layer = min(drainage_layer,0.)
            wliq_soisno(j) = wliq_soisno(j) + drainage_layer

            drainage_tot = drainage_tot - drainage_layer

            IF(drainage_tot >= 0.)THEN
               zwt = max(0.,zwt - drainage_layer/s_y/1000.)
               EXIT
            ELSE
               zwt = zi_soisno(j)
            ENDIF
         ENDDO

!-- Remove residual drainage  ------------------------------------------------
         zwt = max(0.,zwt - drainage_tot/1000./rous)
         wa = wa + drainage_tot

!-- Recompute jwt  ---------------------------------------------------------------
       ! allow jwt to equal zero when zwt is in top layer
         jwt = nl_soil
         DO j = 1, nl_soil
            IF(zwt <= zi_soisno(j)) THEN
               jwt = j-1
               EXIT
            ENDIF
         ENDDO

      ENDIF   ! end of jwt IF construct

      zwt = max(0.0,zwt)
      zwt = min(80.,zwt)

      rsubst = drainage


      ! Correction [1]
      ! NON-physically based correction on wliq_soisno
      ! excessive water above saturation added to the above unsaturated layer like a bucket
      ! IF column over saturated, excess water goes to runoff

      DO j = nl_soil,2,-1
         xsi = max(wliq_soisno(j)-eff_porosity(j)*dzmm(j),0.)
         wliq_soisno(j) = min(eff_porosity(j)*dzmm(j), wliq_soisno(j))
         wliq_soisno(j-1) = wliq_soisno(j-1) + xsi
      ENDDO

      ! 12/2022, note by yuan: a potential bug below which needs check,
      ! if wice_soisno(1) > pondmx + porsl*dzmm, so xs1>0, in that case,
      ! wliq_soisno(1) will be nagtive, and xs1 is positive.
      xs1 = wliq_soisno(1) - (pondmx+porsl(1)*dzmm(1)-wice_soisno(1))
      IF(xs1 > 0.)THEN
         wliq_soisno(1) = pondmx+porsl(1)*dzmm(1)-wice_soisno(1)
      ELSE
         xs1 = 0.
      ENDIF

      rsubst = rsubst + xs1 / deltim


      ! Correction [2]
      ! NON-physically based correction on wliq_soisno
      ! Limit wliq_soisno to be greater than or equal to watmin.
      ! Get water needed to bring wliq_soisno equal watmin from lower layer.
      ! If insufficient water in soil layers, get from aquifer water

      xs = 0.
      DO j = 1, nl_soil
         IF (wliq_soisno(j) < 0.) THEN
            xs = xs + wliq_soisno(j)
            wliq_soisno(j) = 0.
         ENDIF
      ENDDO

      ! Sub-surface runoff and drainage
      rsubst = rsubst + xs/deltim
      IF (rsubst < 0.) THEN
         wa = wa + rsubst*deltim
         rsubst = 0.
      ENDIF

!     DO j = 1, nl_soil-1
!        IF (wice_soisno(j)*wice_soisno(j+1) < 1.e-6)THEN
!           IF (wliq_soisno(j) < watmin) THEN
!              xs = watmin - wliq_soisno(j)
!              ! deepen water table IF water is passed from below zwt layer
!              IF(j == jwt) THEN
!                 zwt = max(0.,zwt + xs/eff_porosity(j)/1000.)
!              ENDIF
!           ELSE
!              xs = 0.
!           ENDIF
!           wliq_soisno(j  ) = wliq_soisno(j  ) + xs
!           wliq_soisno(j+1) = wliq_soisno(j+1) - xs
!        ENDIF
!     ENDDO

!     ! Get water for bottom layer from layers above if possible
!     IF(wliq_soisno(nl_soil) < watmin)THEN
!        xs = watmin-wliq_soisno(nl_soil)
!        DO j = nl_soil-1, 1, -1
!           available_wliq_soisno = max(wliq_soisno(j)-watmin-xs,0.)
!           IF(available_wliq_soisno >= xs)THEN
!              wliq_soisno(nl_soil) = wliq_soisno(nl_soil) + xs
!              wliq_soisno(j      ) = wliq_soisno(j      ) - xs
!              xs = 0.
!              EXIT
!           ELSE
!              wliq_soisno(nl_soil) = wliq_soisno(nl_soil) + available_wliq_soisno
!              wliq_soisno(j      ) = wliq_soisno(j      ) - available_wliq_soisno
!              xs = xs - available_wliq_soisno
!           ENDIF
!        ENDDO
!     ELSE
!        xs = 0.
!     ENDIF

!     ! Needed in case there is no water to be found
!     wliq_soisno(nl_soil) = wliq_soisno(nl_soil) + xs

!     ! Sub-surface runoff and drainage
!     rsubst = rsubst - xs/deltim

   END SUBROUTINE groundwater


END MODULE MOD_SoilSnowHydrology
! ---------- EOP ------------
