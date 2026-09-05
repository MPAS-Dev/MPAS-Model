#include <define.h>

#ifdef HYPERSPECTRAL
MODULE MOD_Albedo_HiRes

!-----------------------------------------------------------------------
   USE MOD_Precision
   IMPLICIT NONE
   SAVE

! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: albland_HiRes
   PUBLIC :: snowage
   PUBLIC :: SnowAlbedo
   PUBLIC :: albocean

! PRIVATE MEMBER FUNCTIONS:
   PRIVATE :: twostream
   PRIVATE :: twostream_hires
#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
   PRIVATE :: twostream_mod
   PRIVATE :: twostream_wrap
   PRIVATE :: twostream_hires_mod
   PRIVATE :: twostream_hires_wrap
#endif
   PRIVATE :: BSM_soil_moisture, calculate_tav, calculate_wgt_variable


!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------

   SUBROUTINE albland_HiRes (ipatch, patchtype, deltim,&
                      soil_s_v_alb,soil_d_v_alb,soil_s_n_alb,soil_d_n_alb,&
                      chil,rho,tau,fveg,green,lai,sai,fwet_snow,coszen,&
                      wt,fsno,scv,scvold,sag,ssw,pg_snow,forc_t,t_grnd,t_soisno,dz_soisno,&
                      snl,wliq_soisno,wice_soisno,snw_rds,snofrz,&
                      mss_bcpho,mss_bcphi,mss_ocpho,mss_ocphi,&
                      mss_dst1,mss_dst2,mss_dst3,mss_dst4,&
                      alb,ssun,ssha,ssoi,ssno,ssno_lyr,thermk,extkb,extkd,&

                      ! new parameters for high res
                      alb_hires  ,&
                      dir_frac   , dif_frac,             &
                      reflectance, transmittance,        &
                      soil_alb, kw, nw, porsl,           &
                      reflectance_out, transmittance_out,&
                      doy, patchlatr, patchlonr         ,&
                      ! new parameters for urban
                      urban_albedo, mean_albedo         ,&
                      lat_north, lat_south, lon_west, lon_east)

!=======================================================================
! Calculates fragmented albedos (direct and diffuse) in
! wavelength regions split at 0.7um.
!
! (1) soil albedos: as in BATS formulations, which are the function of
!     soil color and moisture in the surface soil layer
! (2) snow albedos: as in BATS formulations, which are inferred from
!     the calculations of Wiscombe and Warren (1980) and the snow model
!     and data of Anderson(1976), and the function of snow age, grain size,
!     solar zenith angle, pollution, the amount of the fresh snow
! (3) canopy albedo: two-stream approximation model
! (4) glacier albedos: as in BATS, which are set to constants (0.8 for visible beam,
!     0.55 for near-infrared)
! (5) lake and wetland albedos: as in BATS, which depend on cosine solar zenith angle,
!     based on data in Henderson-Sellers (1986). The frozen lake and wetland albedos
!     are set to constants (0.6 for visible beam, 0.4 for near-infrared)
! (6) over the snow covered tile, the surface albedo is estimated by a linear
!     combination of albedos for snow, canopy and bare soil (or lake, wetland, glacier).
!
! Original author : Yongjiu Dai, 09/15/1999; 08/30/2002, 03/2014
!
! !REVISIONS:
! 12/2019, Hua Yuan: added a wrap FUNCTION for PFT calculation, details see
!          twostream_wrap() added a wrap FUNCTION for PC (3D) calculation,
!          details see ThreeDCanopy_wrap()
!
! 03/2020, Hua Yuan: added an improved two-stream model, details see
!          twostream_mod()
!
! 08/2020, Hua Yuan: account for stem optical property effects in twostream
!          model
!
! 01/2023, Hua Yuan: CALL SNICAR model to calculate snow albedo&absorption,
!          added SNICAR related variables
!
! 04/2024, Hua Yuan: add option to account for vegetation snow process
!
!=======================================================================

   USE MOD_Precision
   USE MOD_Vars_Global
   USE MOD_Const_Physical, only: tfrz
   USE MOD_Namelist, only: DEF_USE_SNICAR, DEF_HighResSoil
   USE MOD_Vars_TimeInvariants, only: patchclass
   USE MOD_HighRes_Parameters, only: rad2deg
#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
   USE MOD_LandPFT, only: patch_pft_s, patch_pft_e
   USE MOD_Vars_PFTimeInvariants
   USE MOD_Vars_PFTimeVariables
#endif
   USE MOD_Aerosol, only: AerosolMasses
   USE MOD_SnowSnicar_HiRes, only: SnowAge_grain
#ifdef LULC_IGBP_PC
   USE MOD_3DCanopyRadiation, only: ThreeDCanopy_wrap
#endif

   ! IEEE arithmetic module for isnan function, only for debug
   ! use, intrinsic :: IEEE_ARITHMETIC, only: IEEE_IS_NAN, IEEE_SUPPORT_DATATYPE

   IMPLICIT NONE

!------------------------- Dummy Arguments -----------------------------
! ground cover index
   integer, intent(in) :: &
        ipatch,        &! patch index
        patchtype       ! land patch type (0=soil, 1=urban or built-up, 2=wetland,
                        ! 3=land ice, 4=deep lake)
   integer, intent(in) :: &
        snl,           &! number of snow layers
        doy

   real(r8), intent(in) :: &
        patchlatr,     &! patch latitude (radian)
        patchlonr       ! patch longitude (radian)

   real(r8), intent(in) :: &
        deltim,        &! seconds in a time step [second]
        soil_s_v_alb,  &! albedo of visible of the saturated soil
        soil_d_v_alb,  &! albedo of visible of the dry soil
        soil_s_n_alb,  &! albedo of near infrared of the saturated soil
        soil_d_n_alb,  &! albedo of near infrared of the dry soil
        chil,          &! leaf angle distribution factor
        rho(2,2),      &! leaf reflectance (iw=iband, il=life and dead)
        tau(2,2),      &! leaf transmittance (iw=iband, il=life and dead)
        fveg,          &! fractional vegetation cover [-]
        green,         &! green leaf fraction
        lai,           &! leaf area index (LAI+SAI) [m2/m2]
        sai,           &! stem area index (LAI+SAI) [m2/m2]
        fwet_snow,     &! vegetation snow fractional cover [-]

        coszen,        &! cosine of solar zenith angle [-]
        wt,            &! fraction of vegetation covered by snow [-]
        fsno,          &! fraction of soil covered by snow [-]
        ssw,           &! water volumetric content of soil surface layer [m3/m3]
        scv,           &! snow cover, water equivalent [mm]
        scvold,        &! snow cover for previous time step [mm]
        pg_snow,       &! snowfall onto ground including canopy runoff [kg/(m2 s)]
        forc_t,        &! atmospheric temperature [K]
        t_grnd,        &! ground surface temperature [K]
        porsl

   real(r8), intent(in) :: &
        wliq_soisno  ( maxsnl+1:0 ), &! liquid water (kg/m2)
        wice_soisno  ( maxsnl+1:0 ), &! ice lens (kg/m2)
        snofrz       ( maxsnl+1:0 ), &! snow freezing rate (col,lyr) [kg m-2 s-1]
        t_soisno     ( maxsnl+1:1 ), &! soil + snow layer temperature [K]
        dz_soisno    ( maxsnl+1:1 ), &! layer thickness (m)

        dir_frac     (211)    ,&!
        dif_frac     (211)    ,&!
        reflectance  (0:15,211,2)  ,&! reflectance   (PFT, wavelength, dir/dif)
        transmittance(0:15,211,2)  ,&! transmittance (PFT, wavelength, dir/dif)
        soil_alb     (211)    ,&! soil albedo [-]
        kw           (211)    ,&! soil albedo [-]
        nw           (211)      ! soil albedo [-]


   real(r8), intent(inout) :: &
        snw_rds      ( maxsnl+1:0 ), &! effective grain radius (col,lyr) [microns, m-6]
        mss_bcpho    ( maxsnl+1:0 ), &! mass of hydrophobic BC in snow  (col,lyr) [kg]
        mss_bcphi    ( maxsnl+1:0 ), &! mass of hydrophillic BC in snow (col,lyr) [kg]
        mss_ocpho    ( maxsnl+1:0 ), &! mass of hydrophobic OC in snow  (col,lyr) [kg]
        mss_ocphi    ( maxsnl+1:0 ), &! mass of hydrophillic OC in snow (col,lyr) [kg]
        mss_dst1     ( maxsnl+1:0 ), &! mass of dust species 1 in snow  (col,lyr) [kg]
        mss_dst2     ( maxsnl+1:0 ), &! mass of dust species 2 in snow  (col,lyr) [kg]
        mss_dst3     ( maxsnl+1:0 ), &! mass of dust species 3 in snow  (col,lyr) [kg]
        mss_dst4     ( maxsnl+1:0 )   ! mass of dust species 4 in snow  (col,lyr) [kg]

   real(r8), intent(inout) :: sag     ! non dimensional snow age [-]

   real(r8), intent(out) :: &
        alb(2,2),      &! averaged albedo [-]
        ssun(2,2),     &! sunlit canopy absorption for solar radiation
        ssha(2,2),     &! shaded canopy absorption for solar radiation,
                        ! normalized by the incident flux
        thermk,        &! canopy gap fraction for tir radiation
        extkb,         &! (k, g(mu)/mu) direct solar extinction coefficient
        extkd           ! diffuse and scattered diffuse PAR extinction coefficient

   real(r8), intent(out) :: &
        alb_hires        (211, 2) ,&! high resolution albedo, (wavelength, dir/dif)
        reflectance_out  (211, 0:15) ,&!
        transmittance_out(211, 0:15)

   real(r8), intent(out) :: &
        ssoi(2,2),     &! ground soil absorption [-]
        ssno(2,2),     &! ground snow absorption [-]
        ssno_lyr(2,2,maxsnl+1:1) ! ground snow layer absorption, by SNICAR [-]

   ! New inout parameters for urban
   real(r8), ALLOCATABLE, intent(in) :: &
      urban_albedo(:,:,:),    &! (cluster_id, season,wavelength)
      mean_albedo(:, :),      &! (season, wavelength)
      lat_north(:), lat_south(:),&
      lon_west (:), lon_east(:)


!-------------------------- Local variables ----------------------------

   real(r8) :: &!
      age,             &! factor to reduce visible snow alb due to snow age [-]
      albg0,           &! temporary varaiable [-]
      albsoi(2,2),     &! soil albedo [-]
      albsno(2,2),     &! snow albedo [-]
      albsno_pur(2,2), &! snow albedo [-]
      albsno_bc (2,2), &! snow albedo [-]
      albsno_oc (2,2), &! snow albedo [-]
      albsno_dst(2,2), &! snow albedo [-]
      albg(2,2),       &! albedo, ground
      albv(2,2),       &! albedo, vegetation [-]
      alb_s_inc,       &! decrease in soil albedo due to wetness [-]
      beta0,           &! upscattering parameter for direct beam [-]
      cff,             &! snow alb correction factor for zenith angle > 60 [-]
      conn,            &! constant (=0.5) for visible snow alb calculation [-]
      cons,            &! constant (=0.2) for nir snow albedo calculation [-]
      czen,            &! cosine of solar zenith angle > 0 [-]
      czf,             &! solar zenith correction for new snow albedo [-]
      dfalbl,          &! snow albedo for diffuse nir radiation [-]
      dfalbs,          &! snow albedo for diffuse vis radiation [-]
      dralbl,          &! snow albedo for direct nir radiation [-]
      dralbs,          &! snow albedo for direct vis radiation [-]
      lsai,            &! leaf and stem area index (LAI+SAI) [m2/m2]
      sl,              &! factor that helps control alb zenith dependence [-]
      snal0,           &! alb for visible,incident on new snow (zen ang<60) [-]
      snal1,           &! alb for NIR, incident on new snow (zen angle<60) [-]
      upscat,          &! upward scattered fraction for direct beam [-]
      tran(2,3)         ! canopy transmittances for solar radiation

!-------------------------- Local high resolution variables ----------------------------
   real(r8) ::                 &!

      ! sun fraction
      fsds_vis_dir_frac(29 )  ,&
      fsds_nir_dir_frac(182)  ,&
      fsds_vis_dif_frac(29 )  ,&
      fsds_nir_dif_frac(182)  ,&

      ! ground
      albg_hires(211, 2)      ,&

      ! soil
      albsoi_hires(211, 2)      ,&

      ! snow
      alb_sno_hires(211, 2)   ,&! high resolution albedo, (wavelength, dir/dif)

      alb_sno_5band   (5, 2) ,&
      albsno_pur_5band(5, 2) ,&! snow albedo [-]
      albsno_bc_5band (5, 2) ,&! snow albedo [-]
      albsno_oc_5band (5, 2) ,&! snow albedo [-]
      albsno_dst_5band(5, 2) ,&! snow albedo [-]

      ! vegetation
      rho_hires(211, 2)       ,&
      tau_hires(211, 2)       ,&
      albv_hires(211, 2)      ,&
      tran_hires(211, 3)      ,&
      scat_hires(211)         ,&
      ssun_hires(211, 2)      ,&
      ssha_hires(211, 2)

      INTEGER, PARAMETER, DIMENSION(6) :: band_index = (/ &
         1, 30, 60, 80, 110, 212   &! 400, 700, 1000, 1200, 1500, 2500 nm
      /)

   real(r8) :: smc
   integer  :: i, j, ibnd, start_index, end_index  ! index for 5 bands [idx]

   integer ps, pe
   logical do_capsnow      !true => DO snow capping
   logical use_snicar_frc  !true: IF radiative forcing is being calculated, first estimate clean-snow albedo
   logical use_snicar_ad   !true: use SNICAR_AD_RT, false: use SNICAR_RT

   real(r8) snwcp_ice                     !excess precipitation due to snow capping [kg m-2 s-1]
   real(r8) mss_cnc_bcphi ( maxsnl+1:0 )  !mass concentration of hydrophilic BC (col,lyr) [kg/kg]
   real(r8) mss_cnc_bcpho ( maxsnl+1:0 )  !mass concentration of hydrophobic BC (col,lyr) [kg/kg]
   real(r8) mss_cnc_ocphi ( maxsnl+1:0 )  !mass concentration of hydrophilic OC (col,lyr) [kg/kg]
   real(r8) mss_cnc_ocpho ( maxsnl+1:0 )  !mass concentration of hydrophobic OC (col,lyr) [kg/kg]
   real(r8) mss_cnc_dst1  ( maxsnl+1:0 )  !mass concentration of dust aerosol species 1 (col,lyr) [kg/kg]
   real(r8) mss_cnc_dst2  ( maxsnl+1:0 )  !mass concentration of dust aerosol species 2 (col,lyr) [kg/kg]
   real(r8) mss_cnc_dst3  ( maxsnl+1:0 )  !mass concentration of dust aerosol species 3 (col,lyr) [kg/kg]
   real(r8) mss_cnc_dst4  ( maxsnl+1:0 )  !mass concentration of dust aerosol species 4 (col,lyr) [kg/kg]

   logical :: has_nan

   real(r8) :: lat, lon
   INTEGER :: num_urban_lat, num_urban_lon
   INTEGER :: i_cluster
   INTEGER :: season_index
! ----------------------------------------------------------------------
! 1. Initial set
! ----------------------------------------------------------------------
! set fsds
      fsds_vis_dir_frac(:) = dir_frac(1:29)
      fsds_nir_dir_frac(:) = dir_frac(30:211)
      fsds_vis_dif_frac(:) = dif_frac(1:29)
      fsds_nir_dif_frac(:) = dif_frac(30:211)

! visible and near infrared band albedo for new snow
      snal0 = 0.85         !visible band
      snal1 = 0.65         !near infrared

! ----------------------------------------------------------------------
! set default soil and vegetation albedos and solar absorption
      alb (:,:) = 1.       !averaged
      albg(:,:) = 1.       !ground
      albv(:,:) = 1.       !vegetation
      ssun(:,:) = 0.       !sunlit leaf absorption
      ssha(:,:) = 0.       !shaded leaf absorption

      alb_hires (:,:)   = 1.   ! high resolution albedo
      albg_hires(:,:)   = 1.   ! high resolution ground albedo
      albv_hires(:,:)   = 1.   ! high resolution vegetation albedo
      ssun_hires (:,:)  = 0.
      ssha_hires (:,:)  = 0.

      ! albsoi_hires    (:,:) = 1.

      tran(:,1) = 0.       !incident direct  radiation diffuse transmittance
      tran(:,2) = 1.       !incident diffuse radiation diffuse transmittance
      tran(:,3) = 1.       !incident direct  radiation direct  transmittance

      tran_hires(:,1) = 0.   !incident direct  radiation diffuse transmittance
      tran_hires(:,2) = 1.   !incident diffuse radiation diffuse transmittance
      tran_hires(:,3) = 1.   !incident direct  radiation direct  transmittance

      reflectance_out  (:,:) = -999.
      transmittance_out(:,:) = -999.

      ! 07/06/2023, yuan: use the values of previous timestep.
      ! for nighttime longwave calculations.
      !thermk   = 1.e-3
      IF (lai+sai <= 1.e-6) THEN
         thermk = 1.
      ENDIF
      extkb     = 1.
      extkd     = 0.718

      albsno    (:,:) = 1. !set initial snow albedo
      albsno_pur(:,:) = 1. !set initial pure snow albedo
      albsno_bc (:,:) = 1. !set initial BC   snow albedo
      albsno_oc (:,:) = 1. !set initial OC   snow albedo
      albsno_dst(:,:) = 1. !set initial dust snow albedo

      alb_sno_hires   (:,:) = 1.! high resolution albedo, (wavelength, dir/dif)
      albsno_pur_5band(:,:) = 1.
      albsno_bc_5band (:,:) = 1.
      albsno_oc_5band (:,:) = 1.
      albsno_dst_5band(:,:) = 1.

      ! soil and snow absorption
      ssoi      (:,:) = 0. !set initial soil absorption
      ssno      (:,:) = 0. !set initial snow absorption
      ssno_lyr(:,:,:) = 0. !set initial snow layer absorption

IF (patchtype == 0) THEN
#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
      ps = patch_pft_s(ipatch)
      pe = patch_pft_e(ipatch)
      ssun_p(:,:,ps:pe) = 0.
      ssha_p(:,:,ps:pe) = 0.

      ssun_hires_p(:,:,ps:pe) = 0.
      ssha_hires_p(:,:,ps:pe) = 0.
      ! 07/06/2023, yuan: use the values of previous timestep.
      !thermk_p(ps:pe)   = 1.e-3
      WHERE (lai_p(ps:pe)+sai_p(ps:pe) <= 1.e-6) thermk_p(ps:pe) = 1.
      extkb_p(ps:pe)    = 1.
      extkd_p(ps:pe)    = 0.718
#endif
ENDIF

! ----------------------------------------------------------------------
!  Calculate column-integrated aerosol masses, and
!  mass concentrations for radiative calculations and output
!  (based on new snow level state, after SnowFilter is rebuilt.
!  NEEDS TO BE AFTER SnowFiler is rebuilt, otherwise there
!  can be zero snow layers but an active column in filter)
IF (DEF_USE_SNICAR) THEN
      snwcp_ice  = 0.0     !excess precipitation due to snow capping [kg m-2 s-1]
      do_capsnow = .false. !true => DO snow capping

      CALL AerosolMasses( deltim, snl ,do_capsnow ,&
           wice_soisno(:0),wliq_soisno(:0),snwcp_ice      ,snw_rds       ,&

           mss_bcpho     ,mss_bcphi       ,mss_ocpho      ,mss_ocphi     ,&
           mss_dst1      ,mss_dst2        ,mss_dst3       ,mss_dst4      ,&

           mss_cnc_bcphi ,mss_cnc_bcpho   ,mss_cnc_ocphi  ,mss_cnc_ocpho ,&
           mss_cnc_dst1  ,mss_cnc_dst2    ,mss_cnc_dst3   ,mss_cnc_dst4   )

! ----------------------------------------------------------------------
! Snow aging routine based on Flanner and Zender (2006), Linking snowpack
! microphysics and albedo evolution, JGR, and Brun (1989), Investigation of
! wet-snow metamorphism in respect of liquid-water content, Ann. Glaciol.

      CALL SnowAge_grain(   deltim ,snl    ,dz_soisno(:1)  ,&
           pg_snow         ,snwcp_ice      ,snofrz         ,&

           do_capsnow      ,fsno           ,scv            ,&
           wliq_soisno (:0),wice_soisno(:0),&
           t_soisno    (:1),t_grnd         ,&
           forc_t          ,snw_rds         )
END IF
! ----------------------------------------------------------------------

      lsai = lai + sai
      IF(coszen <= -0.3) THEN
         RETURN  !only DO albedo when coszen > -0.3
      ENDIF

      czen = max(coszen, 0.001)

! ----------------------------------------------------------------------
! 2. get albedo over land
! ----------------------------------------------------------------------
! 2.1 soil albedos, depends on moisture

      ! IF (patchtype <= 2) THEN           !soil, urban and wetland
      IF (patchtype == 0) THEN           !soil

         ! calculate broadband albedos
         alb_s_inc = max(0.11-0.40*ssw, 0.)
         albg(1,1) = min(soil_s_v_alb + alb_s_inc, soil_d_v_alb)
         albg(2,1) = min(soil_s_n_alb + alb_s_inc, soil_d_n_alb)
         albg(:,2) = albg(:,1)           !diffused albedos setting

         IF ( DEF_HighResSoil ) THEN
            ! calculate high res soil albedos
            CALL BSM_soil_moisture( ssw * 100., porsl * 100., soil_alb, kw, nw, albg_hires )

            ! calculate broadband albedos
            CALL calculate_wgt_variable(albg_hires(:,1), fsds_vis_dir_frac, fsds_nir_dir_frac, albg(1,1), albg(2,1))
            CALL calculate_wgt_variable(albg_hires(:,2), fsds_vis_dif_frac, fsds_nir_dif_frac, albg(1,2), albg(2,2))

         ELSE

            ! calculate high res soil albedos
            albg_hires(1 :29 ,1) = albg(1,1)
            albg_hires(30:211,1) = albg(2,1)
            albg_hires(1 :29 ,2) = albg(1,2)
            albg_hires(30:211,2) = albg(2,2)

         END IF

         !DEBUG: Temporarily handle the missing values of soil albedo in certain grids
         IF (soil_alb(1) < 0.01) THEN
            albg_hires(1:29   ,1) = albg(1,1)
            albg_hires(30:211 ,1) = albg(2,1)
            albg_hires(1:29   ,2) = albg(1,2)
            albg_hires(30:211 ,2) = albg(2,2)
         END IF

      ELSE IF(patchtype == 1) THEN       !urban

         ! select constant albedo for urban
         lat = rad2deg(patchlatr)
         lon = rad2deg(patchlonr)

         ! 根据lat、lon边界选择cluster_id
         i_cluster = 0
         DO i = 1, SIZE(lat_north)
            IF (lat >= lat_south(i) .and. lat <= lat_north(i) .and. &
                  lon >= lon_west(i)  .and. lon <= lon_east(i)) THEN
               i_cluster = i
               EXIT
            END IF
         END DO

         ! 根据季节选择albedo
         ! 季节指数: 1=冬季, 2=春季, 3=夏季, 4=秋季
         IF (doy >= 355 .or. doy < 80) THEN
            season_index = 1  ! 冬季
         ELSE IF (doy >= 80 .and. doy < 172) THEN
            season_index = 2  ! 春季
         ELSE IF (doy >= 172 .and. doy < 266) THEN
            season_index = 3  ! 夏季
         ELSE
            season_index = 4  ! 秋季
         END IF

         ! 提取反照率数据
         IF (i_cluster > 0 .and. allocated(urban_albedo)) THEN
            albg_hires(:, 1) = urban_albedo(i_cluster, season_index, :)
            albg_hires(:, 2) = urban_albedo(i_cluster, season_index, :)
         ELSE
            ! 如果未找到合适的cluster或数据未分配，使用mean_albedo
            IF (allocated(mean_albedo)) THEN
               albg_hires(:, 1) = mean_albedo(season_index, :)
               albg_hires(:, 2) = mean_albedo(season_index, :)
            ELSE
               ! 如果都没有数据，使用默认值
               albg_hires(1 :29 ,1) = 0.12
               albg_hires(30:211,1) = 0.20
               albg_hires(1 :29 ,2) = 0.12
               albg_hires(30:211,2) = 0.20
            END IF
         END IF

         ! 计算宽波段反照率 (VIS 和 NIR)
         CALL calculate_wgt_variable(albg_hires(:,1), fsds_vis_dir_frac, fsds_nir_dir_frac, albg(1,1), albg(2,1))
         CALL calculate_wgt_variable(albg_hires(:,2), fsds_vis_dif_frac, fsds_nir_dif_frac, albg(1,2), albg(2,2))

      ELSE IF (patchtype == 2) THEN       !wetland
         ! calculate broadband albedos
         alb_s_inc = max(0.11-0.40*ssw, 0.)
         albg(1,1) = min(soil_s_v_alb + alb_s_inc, soil_d_v_alb)
         albg(2,1) = min(soil_s_n_alb + alb_s_inc, soil_d_n_alb)
         albg(:,2) = albg(:,1)           !diffused albedos setting

         albg_hires(1 :29 ,1) = albg(1,1)
         albg_hires(1 :29 ,2) = albg(1,2)
         albg_hires(30:211,1) = albg(2,1)
         albg_hires(30:211,2) = albg(2,2)

! 2.2 albedos for permanent ice sheet.
      ELSE IF(patchtype == 3) THEN       !permanent ice sheet
         albg(1,:) = 0.8
         albg(2,:) = 0.55

         albg_hires(1 :29 ,:) = 0.8
         albg_hires(30:211,:) = 0.55

! 2.3 albedo for inland water
      ELSE IF(patchtype >= 4) THEN
         albg0 = 0.05/(czen+0.15)
         albg(:,1) = albg0
         albg(:,2) = 0.1                 !Subin (2012)

         albg_hires(:,1) = albg0
         albg_hires(:,2) = 0.1

         IF(t_grnd < tfrz)THEN           !frozen lake and wetland
            albg(1,:) = 0.6
            albg(2,:) = 0.4

            albg_hires(1 :29 ,:) = 0.6
            albg_hires(30:211,:) = 0.4
         ENDIF
      ENDIF

      ! SAVE soil ground albedo
      albsoi      (:,:) = albg      (:,:)
      albsoi_hires(:,:) = albg_hires(:,:)

   ! ----------------------------------------------------------------------
! 3. albedo for snow cover.
!    - Scheme 1: snow albedo depends on snow-age, zenith angle, and thickness
!                of snow age gives reduction of visible radiation [CoLM2014].
!    - Scheme 2: SNICAR model
! ----------------------------------------------------------------------
      IF (scv > 0.) THEN

         IF (.not. DEF_USE_SNICAR) THEN
            cons = 0.2
            conn = 0.5
            sl   = 2.0               !sl helps control albedo zenith dependence

            ! 05/02/2023, Dai: move from CoLMMAIN.F90
            ! update the snow age
            IF (snl == 0) sag=0.
            CALL snowage (deltim,t_grnd,scv,scvold,sag)

            ! correction for snow age
            age    = 1.-1./(1.+sag)
            dfalbs = snal0*(1.-cons*age)

            ! czf corrects albedo of new snow for solar zenith
            cff    = ((1.+1./sl)/(1.+czen*2.*sl )- 1./sl)
            cff    = max(cff,0.)
            czf    = 0.4*cff*(1.-dfalbs)
            dralbs = dfalbs+czf
            dfalbl = snal1*(1.-conn*age)
            czf    = 0.4*cff*(1.-dfalbl)
            dralbl = dfalbl+czf

            albsno(1,1) = dralbs
            albsno(2,1) = dralbl
            albsno(1,2) = dfalbs
            albsno(2,2) = dfalbl

         ELSE

            ! 01/09/2023, yuan: CALL SNICAR for snow albedo
            use_snicar_frc = .false.  !  true: IF radiative forcing is being calculated, first estimate clean-snow albedo
            use_snicar_ad  = .true.   !  use true: use SNICAR_AD_RT, false: use SNICAR_RT

            CALL SnowAlbedo(   use_snicar_frc ,use_snicar_ad  ,coszen         ,&
               albg_hires(:,1),albg_hires(:,2),snl            ,fsno           ,&
               scv            ,wliq_soisno    ,wice_soisno    ,snw_rds        ,&

               mss_cnc_bcphi  ,mss_cnc_bcpho  ,mss_cnc_ocphi  ,mss_cnc_ocpho  ,&
               mss_cnc_dst1   ,mss_cnc_dst2   ,mss_cnc_dst3   ,mss_cnc_dst4   ,&

               alb_sno_5band   (:,1),alb_sno_5band   (:,2),albsno_pur_5band(:,1),albsno_pur_5band(:,2),&
               albsno_bc_5band (:,1),albsno_bc_5band (:,2),albsno_oc_5band (:,1),albsno_oc_5band (:,2),&
               albsno_dst_5band(:,1),albsno_dst_5band(:,2),ssno_lyr(1,1,:)      ,ssno_lyr(2,1,:)      ,&
               ssno_lyr(1,2,:)      ,ssno_lyr(2,2,:)      ,dir_frac             ,dif_frac             )

            ! IF no snow layer exist
            IF (snl == 0) THEN
               ssno_lyr(:,:,1) = ssno_lyr(:,:,1) + ssno_lyr(:,:,0)
               ssno_lyr(:,:,0) = 0.
            ENDIF
         ENDIF
      ENDIF

! 3.1 correction due to snow cover
      ! albg(:,:) = (1.-fsno)*albg(:,:) + fsno*albsno(:,:)
      ! alb (:,:) = albg(:,:)

      do ibnd = 1, 5
         start_index = band_index(ibnd)
         end_index   = band_index(ibnd+1) - 1

         alb_sno_hires(start_index:end_index, 1) = alb_sno_5band(ibnd, 1)
         alb_sno_hires(start_index:end_index, 2) = alb_sno_5band(ibnd, 2)
      end do

      albg_hires(:,1) = (1.-fsno)*albg_hires(:,1) + fsno*alb_sno_hires(:,1)
      albg_hires(:,2) = (1.-fsno)*albg_hires(:,2) + fsno*alb_sno_hires(:,2)

      alb_hires(:,1) = albg_hires(:,1)
      alb_hires(:,2) = albg_hires(:,2)

      CALL calculate_wgt_variable(albg_hires(:,1), fsds_vis_dir_frac, fsds_nir_dir_frac, albg(1,1), albg(2,1))
      CALL calculate_wgt_variable(albg_hires(:,2), fsds_vis_dif_frac, fsds_nir_dif_frac, albg(1,2), albg(2,2))

      alb (:,:) = albg(:,:)

! ----------------------------------------------------------------------
! 4. canopy albedos: two stream approximation or 3D canopy radiation transfer
! ----------------------------------------------------------------------
      IF (lai+sai > 1e-6 .and. patchtype < 3) THEN
         ! initialization
         albv(:,:) = albg(:,:)

         IF (patchtype == 0) THEN  !soil patches

#if (defined LULC_USGS || defined LULC_IGBP)
            ! High resolution vegetation
            write(*,*) "NOT SUPPORT NOW!!!!!!!"
            CALL twostream_hires (chil,reflectance,transmittance,green,lai,sai, fwet_snow,&
                  czen,albg_hires,albv_hires,tran_hires,thermk,extkb,extkd,ssun_hires,ssha_hires)

            CALL calculate_wgt_variable(albg_hires(:,1), fsds_vis_dir_frac, fsds_nir_dir_frac, alb(1,1), alb(2,1))
            CALL calculate_wgt_variable(albg_hires(:,2), fsds_vis_dif_frac, fsds_nir_dif_frac, alb(1,2), alb(2,2))

            CALL calculate_wgt_variable(ssun_hires(:,1), fsds_vis_dir_frac, fsds_nir_dir_frac, ssun(1,1), ssun(2,1))
            CALL calculate_wgt_variable(ssun_hires(:,2), fsds_vis_dif_frac, fsds_nir_dif_frac, ssun(1,2), ssun(2,2))

            CALL calculate_wgt_variable(ssha_hires(:,1), fsds_vis_dir_frac, fsds_nir_dir_frac, ssha(1,1), ssha(2,1))
            CALL calculate_wgt_variable(ssha_hires(:,2), fsds_vis_dif_frac, fsds_nir_dif_frac, ssha(1,2), ssha(2,2))

            alb_hires(:,:) = albv_hires(:,:)

            ! ! two-band albedo
            ! CALL twostream (chil,rho,tau,green,lai,sai,fwet_snow,&
            !                czen,albg,albv,tran,thermk,extkb,extkd,ssun,ssha)

            ! 08/31/2023, yuan: to be consistent with PFT and PC
            !albv(:,:) = (1.-  wt)*albv(:,:) + wt*albsno(:,:)
            !alb (:,:) = (1.-fveg)*albg(:,:) + fveg*albv(:,:)

            ! alb(:,:) = albv(:,:)

#endif
         ELSE  !other patchtypes (/=0)
            CALL twostream (chil,rho,tau,green,lai,sai,fwet_snow,&
                            czen,albg,albv,tran,thermk,extkb,extkd,ssun,ssha)

            ! 08/31/2023, yuan: to be consistent with PFT and PC
            !albv(:,:) = (1.-  wt)*albv(:,:) + wt*albsno(:,:)
            !alb (:,:) = (1.-fveg)*albg(:,:) + fveg*albv(:,:)
            alb(:,:) = albv(:,:)

            alb_hires(1:29  ,1) = albv_hires(1,1)
            alb_hires(30:211,1) = albv_hires(2,1)
            alb_hires(1:29  ,2) = albv_hires(1,2)
            alb_hires(30:211,2) = albv_hires(2,2)

            tran_hires(1:29  ,1) = tran(1,1)
            tran_hires(30:211,1) = tran(2,1)
            tran_hires(1:29  ,2) = tran(1,2)
            tran_hires(30:211,2) = tran(2,2)
            tran_hires(1:29  ,3) = tran(1,3)
            tran_hires(30:211,3) = tran(2,3)
         ENDIF
      ENDIF


      IF (patchtype == 0) THEN

#ifdef LULC_IGBP_PFT
      CALL twostream_hires_wrap (ipatch, czen, albg_hires, &
            albv_hires, tran_hires, ssun_hires, ssha_hires, &
            reflectance, transmittance, &
            fsds_vis_dir_frac, fsds_nir_dir_frac, &
            fsds_vis_dif_frac, fsds_nir_dif_frac, &
            ssw, reflectance_out, transmittance_out, doy)

      ! convert alb(:,:) = albv(:,:) -> hyperspectral
      CALL calculate_wgt_variable(albv_hires(:,1), fsds_vis_dir_frac, fsds_nir_dir_frac, alb(1,1), alb(2,1))
      CALL calculate_wgt_variable(albv_hires(:,2), fsds_vis_dif_frac, fsds_nir_dif_frac, alb(1,2), alb(2,2))

      alb_hires(:,:) = albv_hires(:,:)
#endif

#ifdef LULC_IGBP_PC
         !NOTE: if patchclass is CROPLAND, using twostream model
         IF (patchclass(ipatch) == CROPLAND) THEN
            CALL twostream_wrap (ipatch, czen, albg, albv, tran, ssun, ssha)
            alb(:,:) = albv(:,:)
         ELSE
            CALL ThreeDCanopy_wrap (ipatch, czen, albg, albv, tran, ssun, ssha)
            alb(:,:) = albv(:,:)
         ENDIF
#endif
      ENDIF

      ! treat soil/snow albedo in direct and diffuse respectively
      CALL calculate_wgt_variable((tran_hires(:,1)*(1.-albsoi_hires(:,2)) + tran_hires(:,3)*(1.-albsoi_hires(:,1))), fsds_vis_dir_frac, fsds_nir_dir_frac, ssoi(1,1), ssoi(2,1))
      CALL calculate_wgt_variable((tran_hires(:,2)*(1.-albsoi_hires(:,2))), fsds_vis_dif_frac, fsds_nir_dif_frac, ssoi(1,2), ssoi(2,2))

      CALL calculate_wgt_variable((tran_hires(:,1)*(1.-alb_sno_hires(:,2)) + tran_hires(:,3)*(1.-alb_sno_hires(:,1))), fsds_vis_dir_frac, fsds_nir_dir_frac, ssno(1,1), ssno(2,1))
      CALL calculate_wgt_variable((tran_hires(:,2)*(1.-alb_sno_hires(:,2))), fsds_vis_dif_frac, fsds_nir_dif_frac, ssno(1,2), ssno(2,2))

!-----------------------------------------------------------------------

   END SUBROUTINE albland_HiRes


   SUBROUTINE twostream ( chil, rho, tau, green, lai, sai, fwet_snow, &
              coszen, albg, albv, tran, thermk, extkb, extkd, ssun, ssha )

!-----------------------------------------------------------------------
!
!     calculation of canopy albedos via two stream approximation (direct
!     and diffuse ) and partition of incident solar
!
! Original author: Yongjiu Dai, June 11, 2001
!
!-----------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_Namelist, only: DEF_VEG_SNOW
   IMPLICIT NONE

! parameters
   real(r8), intent(in) :: &
          ! static parameters associated with vegetation type
            chil,          &! leaf angle distribution factor
            rho(2,2),      &! leaf reflectance (iw=iband, il=life and dead)
            tau(2,2),      &! leaf transmittance (iw=iband, il=life and dead)

          ! time-space varying vegetation parameters
            green,         &! green leaf fraction
            lai,           &! leaf area index of exposed canopy (snow-free)
            sai,           &! stem area index
            fwet_snow       ! vegetation snow fractional cover [-]

! environmental variables
   real(r8), intent(in) :: &
            coszen,        &! consine of solar zenith angle
            albg(2,2)       ! albedos of ground

! output
   real(r8), intent(out) :: &
            albv(2,2),     &! albedo, vegetation [-]
            tran(2,3),     &! canopy transmittances for solar radiation
            thermk,        &! canopy gap fraction for tir radiation
            extkb,         &! (k, g(mu)/mu) direct solar extinction coefficient
            extkd,         &! diffuse and scattered diffuse PAR extinction coefficient
            ssun(2,2),     &! sunlit canopy absorption for solar radiation
            ssha(2,2)       ! shaded canopy absorption for solar radiation,
                            ! normalized by the incident flux

!-------------------------- local -----------------------------------
   real(r8) :: &
            lsai,          &! lai+sai
            sai_,          &! sai=0 for USGS, no stem
            phi1,          &! (phi-1)
            phi2,          &! (phi-2)
            scat,          &! (omega)
            proj,          &! (g(mu))
            zmu,           &! (int(mu/g(mu))
            zmu2,          &! (zmu * zmu)
            as,            &! (a-s(mu))
            upscat,        &! (omega-beta)
            beta0,         &! (beta-0)
            psi,           &! (h)

            be,            &! (b)
            ce,            &! (c)
            de,            &! (d)
            fe,            &! (f)

            power1,        &! (h*lai)
            power2,        &! (k*lai)
            power3,        &!

            sigma,         &!
            s1,            &!
            s2,            &!
            p1,            &!
            p2,            &!
            p3,            &!
            p4,            &!
            f1,            &!
            f2,            &!
            h1,            &!
            h4,            &!
            m1,            &!
            m2,            &!
            m3,            &!
            n1,            &!
            n2,            &!
            n3,            &!

            hh1,           &! (h1/sigma)
            hh2,           &! (h2)
            hh3,           &! (h3)
            hh4,           &! (h4/sigma)
            hh5,           &! (h5)
            hh6,           &! (h6)
            hh7,           &! (h7)
            hh8,           &! (h8)
            hh9,           &! (h9)
            hh10,          &! (h10)

            eup(2,2),      &! (integral of i_up*exp(-kx) )
            edown(2,2)      ! (integral of i_down*exp(-kx) )

   ! vegetation snow optical properties
   real(r8) :: upscat_sno = 0.5   !upscat parameter for snow
   real(r8) :: beta0_sno  = 0.5   !beta0 parameter for snow
   real(r8) :: scat_sno(2)        !snow single scattering albedo
   data scat_sno(1), scat_sno(2) /0.8, 0.4/   ! 1:vis, 2: nir

   integer iw               ! band iterator

!-----------------------------------------------------------------------
! projected area of phytoelements in direction of mu and
! average inverse diffuse optical depth per unit leaf area

      phi1  = 0.5 - 0.633 * chil - 0.33 * chil * chil
      phi2  = 0.877 * ( 1. - 2. * phi1 )

      proj  = phi1 + phi2 * coszen
      extkb = proj / coszen

      extkd = 0.719

      IF (abs(phi1).gt.1.e-6 .and. abs(phi2).gt.1.e-6) THEN
         zmu = 1. / phi2 * ( 1. - phi1 / phi2 * log ( ( phi1 + phi2 ) / phi1 ) )
      ELSE IF (abs(phi1).le.1.e-6) THEN
         zmu = 1./0.877
      ELSE IF (abs(phi2).le.1.e-6) THEN
         zmu = 1./(2.*phi1)
      ENDIF
      zmu2 = zmu * zmu

#if(defined LULC_USGS)
      ! yuan: to be consistance with CoLM2014, no stem considered
      ! for twostream and leaf optical property calculations
      sai_ = 0.
#else
      sai_ = sai
#endif

      lsai   = lai + sai_
      power3 = (lai+sai) / zmu
      power3 = min( 50., power3 )
      power3 = max( 1.e-5, power3 )
      thermk = exp(-power3)

      IF (lsai <= 1e-6) RETURN

      DO iw = 1, 2    ! WAVE_BAND_LOOP

!-----------------------------------------------------------------------
!     calculate average scattering coefficient, leaf projection and
!     other coefficients for two-stream model.
!-----------------------------------------------------------------------

! account for stem optical property effects
      scat =  lai/lsai * ( tau(iw,1) + rho(iw,1) ) &
           + sai_/lsai * ( tau(iw,2) + rho(iw,2) )

      as = scat / 2. * proj / ( proj + coszen * phi2 )
      as = as * ( 1. - coszen * phi1 / ( proj + coszen * phi2 ) * &
                 log ( ( proj + coszen * phi2 + coszen * phi1 ) / ( coszen * phi1 ) ) )

! account for stem optical property effects
      !TODO-done: betao -> beta0
      upscat = lai/lsai*tau(iw,1) + sai_/lsai*tau(iw,2)
      ! 09/12/2014, yuan: a bug, change 1. - chil -> 1. + chil
      upscat = 0.5 * ( scat + (scat - 2.*upscat) * ((1. + chil) / 2.) ** 2 )
      beta0  = ( 1. + zmu * extkb ) / ( scat * zmu * extkb ) * as

! account for snow on vegetation
      ! modify scat, upscat and beta0
      ! USE: fwet_snow, snow properties, scatter vis0.8, nir0.4, upscat0.5, beta0.5
      IF ( DEF_VEG_SNOW ) THEN
         scat   =   (1.-fwet_snow)*scat        + fwet_snow*scat_sno(iw)
         upscat = ( (1.-fwet_snow)*scat*upscat + fwet_snow*scat_sno(iw)*upscat_sno ) / scat
         beta0  = ( (1.-fwet_snow)*scat*beta0  + fwet_snow*scat_sno(iw)*beta0_sno  ) / scat
      ENDIF

!-----------------------------------------------------------------------
!     intermediate variables identified in appendix of SE-85.
!-----------------------------------------------------------------------

      be = 1. - scat + upscat
      ce = upscat
      de = scat * zmu * extkb * beta0
      fe = scat * zmu * extkb * ( 1. - beta0 )

      psi = sqrt(be**2 - ce**2)/zmu
      power1 = min( psi*lsai, 50. )
      power2 = min( extkb*lsai, 50. )
      s1 = exp( - power1 )
      s2 = exp( - power2 )

!-----------------------------------------------------------------------
!     calculation of direct albedos and canopy transmittances.
!     albv(iw,1)     ( i-up )
!     tran(iw,irad)  ( i-down )
!-----------------------------------------------------------------------

      p1 = be + zmu * psi
      p2 = be - zmu * psi
      p3 = be + zmu * extkb
      p4 = be - zmu * extkb

      f1 = 1. - albg(iw,2)*p1/ce
      f2 = 1. - albg(iw,2)*p2/ce

      h1 = - ( de * p4 + ce * fe )
      h4 = - ( fe * p3 + ce * de )

      sigma = ( zmu * extkb ) ** 2 + ( ce**2 - be**2 )

      IF (abs(sigma) .gt. 1.e-10) THEN

         hh1 = h1 / sigma
         hh4 = h4 / sigma

         m1 = f1 * s1
         m2 = f2 / s1
         m3 = ( albg(iw,1) - ( hh1 - albg(iw,2) * hh4 ) ) * s2

         n1 = p1 / ce
         n2 = p2 / ce
         n3 = - hh4

         hh2 = (m3*n2 - m2*n3) / (m1*n2 - m2*n1)
         hh3 = (m3*n1 - m1*n3) / (m2*n1 - m1*n2)

         hh5 = hh2 * p1 / ce
         hh6 = hh3 * p2 / ce

         albv(iw,1) = hh1 + hh2 + hh3
         tran(iw,1) = hh4 * s2 + hh5 * s1 + hh6 / s1

         eup(iw,1) = hh1 * (1. - s2*s2) / (2.*extkb) &
                   + hh2 * (1. - s1*s2) / (extkb + psi) &
                   + hh3 * (1. - s2/s1) / (extkb - psi)

         edown(iw,1) = hh4 * (1. - s2*s2) / (2.*extkb) &
                     + hh5 * (1. - s1*s2) / (extkb + psi) &
                     + hh6 * (1. - s2/s1) / (extkb - psi)

      ELSE

         m1 = f1 * s1
         m2 = f2 / s1
         m3 = h1 / zmu2 * ( lsai + 1. / (2.*extkb) ) * s2 &
            + albg(iw,2) / ce * ( - h1 / (2.*extkb) / zmu2 * &
              ( p3*lsai + p4 / (2.*extkb) ) - de ) * s2 &
            + albg(iw,1) * s2

         n1 = p1 / ce
         n2 = p2 / ce
         n3 = 1./ce * ( h1*p4 / (4.*extkb*extkb) / zmu2 + de)

         hh2 = (m3*n2 - m2*n3) / (m1*n2 - m2*n1)
         hh3 = (m3*n1 - m1*n3) / (m2*n1 - m1*n2)

         hh5 = hh2 * p1 / ce
         hh6 = hh3 * p2 / ce

         albv(iw,1) =  - h1 / (2.*extkb*zmu2) + hh2 + hh3
         tran(iw,1) = 1./ce * ( -h1/(2.*extkb*zmu2) * (p3*lsai + p4/(2.*extkb)) - de ) * s2 &
                    + hh5 * s1 + hh6 / s1

         eup(iw,1) = (hh2 - h1/(2.*extkb*zmu2)) * (1. - s2*s2) / (2.*extkb) &
                   + hh3 * (lsai - 0.) &
                   + h1/(2.*extkb*zmu2) * ( lsai*s2*s2 - (1. - s2*s2)/(2.*extkb) )

         edown(iw,1) = (hh5 - (h1*p4/(4.*extkb*extkb*zmu) + de)/ce) * (1. - s2*s2)/(2.*extkb) &
                     + hh6 * (lsai - 0.) &
                     + h1*p3/(ce*4.*extkb*extkb*zmu2) * (lsai*s2*s2 - (1. - s2*s2)/(2.*extkb) )

      ENDIF

      ssun(iw,1) = (1.-scat) * ( 1.-s2 + 1. / zmu * (eup(iw,1) + edown(iw,1)) )
      ssha(iw,1) = scat * (1.-s2) &
                 + ( albg(iw,2)*tran(iw,1) + albg(iw,1)*s2 - tran(iw,1) ) - albv(iw,1) &
                 - ( 1. - scat ) / zmu * ( eup(iw,1) + edown(iw,1) )

!-----------------------------------------------------------------------
!     calculation of diffuse albedos and canopy transmittances
!     albv(iw,2) ( i-up )
!     tran(iw,2) ( i-down )
!-----------------------------------------------------------------------

      m1 = f1 * s1
      m2 = f2 / s1
      m3 = 0.

      n1 = p1 / ce
      n2 = p2 / ce
      n3 = 1.

      hh7 = -m2 / (m1*n2 - m2*n1)
      hh8 = -m1 / (m2*n1 - m1*n2)

      hh9  = hh7 * p1 / ce
      hh10 = hh8 * p2 / ce

      albv(iw,2) = hh7 + hh8
      tran(iw,2) = hh9 * s1 + hh10 / s1

      IF (abs(sigma) .gt. 1.e-10) THEN
         eup(iw,2)   = hh7  * (1. - s1*s2) / (extkb + psi) &
                     + hh8  * (1. - s2/s1) / (extkb - psi)
         edown(iw,2) = hh9  * (1. - s1*s2) / (extkb + psi) &
                     + hh10 * (1. - s2/s1) / (extkb - psi)
      ELSE
         eup(iw,2)   = hh7 * (1. - s1*s2) / ( extkb + psi) + hh8  * (lsai - 0.)
         edown(iw,2) = hh9 * (1. - s1*s2) / ( extkb + psi) + hh10 * (lsai - 0.)
      ENDIF

      ssun(iw,2) = (1.-scat) / zmu * (eup(iw,2) + edown(iw,2))
      ssha(iw,2) = tran(iw,2) * ( albg(iw,2) -1. ) - ( albv(iw,2) - 1. ) &
                 - ( 1. - scat ) / zmu * ( eup(iw,2) + edown(iw,2) )

      ENDDO           ! WAVE_BAND_LOOP

! 03/06/2020, yuan: add direct transmittance (s2) to
!                   tran for incident direct case
! 03/14/2020, yuan: save direct T to 3rd position of tran
      tran(:,3) = s2

   END SUBROUTINE twostream

   SUBROUTINE twostream_hires ( chil, rho, tau, green, lai, sai, fwet_snow, &
              coszen, albg, albv, tran, thermk, extkb, extkd, ssun, ssha )

!-----------------------------------------------------------------------
!
!     calculation of canopy albedos via two stream approximation (direct
!     and diffuse ) and partition of incident solar
!
! Original author: Yongjiu Dai, June 11, 2001
!
!-----------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_Namelist, only: DEF_VEG_SNOW
   IMPLICIT NONE

! parameters
   real(r8), intent(in) :: &
          ! static parameters associated with vegetation type
            chil,          &! leaf angle distribution factor
            rho(211,2),      &! leaf reflectance (iw=iband, il=life and dead)
            tau(211,2),      &! leaf transmittance (iw=iband, il=life and dead)

          ! time-space varying vegetation parameters
            green,         &! green leaf fraction
            lai,           &! leaf area index of exposed canopy (snow-free)
            sai,           &! stem area index
            fwet_snow       ! vegetation snow fractional cover [-]

! environmental variables
   real(r8), intent(in) :: &
            coszen,        &! consine of solar zenith angle
            albg(211,2)     ! albedos of ground

! output
   real(r8), intent(out) :: &
            albv(211,2),     &! albedo, vegetation [-]
            tran(211,3),     &! canopy transmittances for solar radiation
            thermk,        &! canopy gap fraction for tir radiation
            extkb,         &! (k, g(mu)/mu) direct solar extinction coefficient
            extkd,         &! diffuse and scattered diffuse PAR extinction coefficient
            ssun(211,2),     &! sunlit canopy absorption for solar radiation
            ssha(211,2)       ! shaded canopy absorption for solar radiation,
                            ! normalized by the incident flux

!-------------------------- local -----------------------------------
   real(r8) :: &
            lsai,          &! lai+sai
            sai_,          &! sai=0 for USGS, no stem
            phi1,          &! (phi-1)
            phi2,          &! (phi-2)
            scat,          &! (omega)
            proj,          &! (g(mu))
            zmu,           &! (int(mu/g(mu))
            zmu2,          &! (zmu * zmu)
            as,            &! (a-s(mu))
            upscat,        &! (omega-beta)
            beta0,         &! (beta-0)
            psi,           &! (h)

            be,            &! (b)
            ce,            &! (c)
            de,            &! (d)
            fe,            &! (f)

            power1,        &! (h*lai)
            power2,        &! (k*lai)
            power3,        &!

            sigma,         &!
            s1,            &!
            s2,            &!
            p1,            &!
            p2,            &!
            p3,            &!
            p4,            &!
            f1,            &!
            f2,            &!
            h1,            &!
            h4,            &!
            m1,            &!
            m2,            &!
            m3,            &!
            n1,            &!
            n2,            &!
            n3,            &!

            hh1,           &! (h1/sigma)
            hh2,           &! (h2)
            hh3,           &! (h3)
            hh4,           &! (h4/sigma)
            hh5,           &! (h5)
            hh6,           &! (h6)
            hh7,           &! (h7)
            hh8,           &! (h8)
            hh9,           &! (h9)
            hh10,          &! (h10)

            eup(211,2),      &! (integral of i_up*exp(-kx) )
            edown(211,2)      ! (integral of i_down*exp(-kx) )

   ! vegetation snow optical properties
   real(r8) :: upscat_sno = 0.5   !upscat parameter for snow
   real(r8) :: beta0_sno  = 0.5   !beta0 parameter for snow
   real(r8) :: scat_sno(2)        !snow single scattering albedo
   data scat_sno(1), scat_sno(2) /0.8, 0.4/   ! 1:vis, 2: nir

   integer iw               ! band iterator

!-----------------------------------------------------------------------
! projected area of phytoelements in direction of mu and
! average inverse diffuse optical depth per unit leaf area

      phi1  = 0.5 - 0.633 * chil - 0.33 * chil * chil
      phi2  = 0.877 * ( 1. - 2. * phi1 )

      proj  = phi1 + phi2 * coszen
      extkb = proj / coszen

      extkd = 0.719

      IF (abs(phi1).gt.1.e-6 .and. abs(phi2).gt.1.e-6) THEN
         zmu = 1. / phi2 * ( 1. - phi1 / phi2 * log ( ( phi1 + phi2 ) / phi1 ) )
      ELSE IF (abs(phi1).le.1.e-6) THEN
         zmu = 1./0.877
      ELSE IF (abs(phi2).le.1.e-6) THEN
         zmu = 1./(2.*phi1)
      ENDIF
      zmu2 = zmu * zmu

#if(defined LULC_USGS)
      ! yuan: to be consistance with CoLM2014, no stem considered
      ! for twostream and leaf optical property calculations
      sai_ = 0.
#else
      sai_ = sai
#endif

      lsai   = lai + sai_
      power3 = (lai+sai) / zmu
      power3 = min( 50., power3 )
      power3 = max( 1.e-5, power3 )
      thermk = exp(-power3)

      IF (lsai <= 1e-6) RETURN

      DO iw = 1, 211    ! WAVE_BAND_LOOP ! loop from 1 to 211

!-----------------------------------------------------------------------
!     calculate average scattering coefficient, leaf projection and
!     other coefficients for two-stream model.
!-----------------------------------------------------------------------

! account for stem optical property effects
      scat =  lai/lsai * ( tau(iw,1) + rho(iw,1) ) &
           + sai_/lsai * ( tau(iw,2) + rho(iw,2) )

      as = scat / 2. * proj / ( proj + coszen * phi2 )
      as = as * ( 1. - coszen * phi1 / ( proj + coszen * phi2 ) * &
                 log ( ( proj + coszen * phi2 + coszen * phi1 ) / ( coszen * phi1 ) ) )

! account for stem optical property effects
      !TODO-done: betao -> beta0
      upscat = lai/lsai*tau(iw,1) + sai_/lsai*tau(iw,2)
      ! 09/12/2014, yuan: a bug, change 1. - chil -> 1. + chil
      upscat = 0.5 * ( scat + (scat - 2.*upscat) * ((1. + chil) / 2.) ** 2 )
      beta0  = ( 1. + zmu * extkb ) / ( scat * zmu * extkb ) * as

! account for snow on vegetation
      ! modify scat, upscat and beta0
      ! USE: fwet_snow, snow properties, scatter vis0.8, nir0.4, upscat0.5, beta0.5
      IF ( DEF_VEG_SNOW ) THEN
         scat   =   (1.-fwet_snow)*scat        + fwet_snow*scat_sno(iw)
         upscat = ( (1.-fwet_snow)*scat*upscat + fwet_snow*scat_sno(iw)*upscat_sno ) / scat
         beta0  = ( (1.-fwet_snow)*scat*beta0  + fwet_snow*scat_sno(iw)*beta0_sno  ) / scat
      ENDIF

!-----------------------------------------------------------------------
!     intermediate variables identified in appendix of SE-85.
!-----------------------------------------------------------------------

      be = 1. - scat + upscat
      ce = upscat
      de = scat * zmu * extkb * beta0
      fe = scat * zmu * extkb * ( 1. - beta0 )

      psi = sqrt(be**2 - ce**2)/zmu
      power1 = min( psi*lsai, 50. )
      power2 = min( extkb*lsai, 50. )
      s1 = exp( - power1 )
      s2 = exp( - power2 )

!-----------------------------------------------------------------------
!     calculation of direct albedos and canopy transmittances.
!     albv(iw,1)     ( i-up )
!     tran(iw,irad)  ( i-down )
!-----------------------------------------------------------------------

      p1 = be + zmu * psi
      p2 = be - zmu * psi
      p3 = be + zmu * extkb
      p4 = be - zmu * extkb

      f1 = 1. - albg(iw,2)*p1/ce
      f2 = 1. - albg(iw,2)*p2/ce

      h1 = - ( de * p4 + ce * fe )
      h4 = - ( fe * p3 + ce * de )

      sigma = ( zmu * extkb ) ** 2 + ( ce**2 - be**2 )

      IF (abs(sigma) .gt. 1.e-10) THEN

         hh1 = h1 / sigma
         hh4 = h4 / sigma

         m1 = f1 * s1
         m2 = f2 / s1
         m3 = ( albg(iw,1) - ( hh1 - albg(iw,2) * hh4 ) ) * s2

         n1 = p1 / ce
         n2 = p2 / ce
         n3 = - hh4

         hh2 = (m3*n2 - m2*n3) / (m1*n2 - m2*n1)
         hh3 = (m3*n1 - m1*n3) / (m2*n1 - m1*n2)

         hh5 = hh2 * p1 / ce
         hh6 = hh3 * p2 / ce

         albv(iw,1) = hh1 + hh2 + hh3
         tran(iw,1) = hh4 * s2 + hh5 * s1 + hh6 / s1

         eup(iw,1) = hh1 * (1. - s2*s2) / (2.*extkb) &
                   + hh2 * (1. - s1*s2) / (extkb + psi) &
                   + hh3 * (1. - s2/s1) / (extkb - psi)

         edown(iw,1) = hh4 * (1. - s2*s2) / (2.*extkb) &
                     + hh5 * (1. - s1*s2) / (extkb + psi) &
                     + hh6 * (1. - s2/s1) / (extkb - psi)

      ELSE

         m1 = f1 * s1
         m2 = f2 / s1
         m3 = h1 / zmu2 * ( lsai + 1. / (2.*extkb) ) * s2 &
            + albg(iw,2) / ce * ( - h1 / (2.*extkb) / zmu2 * &
              ( p3*lsai + p4 / (2.*extkb) ) - de ) * s2 &
            + albg(iw,1) * s2

         n1 = p1 / ce
         n2 = p2 / ce
         n3 = 1./ce * ( h1*p4 / (4.*extkb*extkb) / zmu2 + de)

         hh2 = (m3*n2 - m2*n3) / (m1*n2 - m2*n1)
         hh3 = (m3*n1 - m1*n3) / (m2*n1 - m1*n2)

         hh5 = hh2 * p1 / ce
         hh6 = hh3 * p2 / ce

         albv(iw,1) =  - h1 / (2.*extkb*zmu2) + hh2 + hh3
         tran(iw,1) = 1./ce * ( -h1/(2.*extkb*zmu2) * (p3*lsai + p4/(2.*extkb)) - de ) * s2 &
                    + hh5 * s1 + hh6 / s1

         eup(iw,1) = (hh2 - h1/(2.*extkb*zmu2)) * (1. - s2*s2) / (2.*extkb) &
                   + hh3 * (lsai - 0.) &
                   + h1/(2.*extkb*zmu2) * ( lsai*s2*s2 - (1. - s2*s2)/(2.*extkb) )

         edown(iw,1) = (hh5 - (h1*p4/(4.*extkb*extkb*zmu) + de)/ce) * (1. - s2*s2)/(2.*extkb) &
                     + hh6 * (lsai - 0.) &
                     + h1*p3/(ce*4.*extkb*extkb*zmu2) * (lsai*s2*s2 - (1. - s2*s2)/(2.*extkb) )

      ENDIF

      ssun(iw,1) = (1.-scat) * ( 1.-s2 + 1. / zmu * (eup(iw,1) + edown(iw,1)) )
      ssha(iw,1) = scat * (1.-s2) &
                 + ( albg(iw,2)*tran(iw,1) + albg(iw,1)*s2 - tran(iw,1) ) - albv(iw,1) &
                 - ( 1. - scat ) / zmu * ( eup(iw,1) + edown(iw,1) )

!-----------------------------------------------------------------------
!     calculation of diffuse albedos and canopy transmittances
!     albv(iw,2) ( i-up )
!     tran(iw,2) ( i-down )
!-----------------------------------------------------------------------

      m1 = f1 * s1
      m2 = f2 / s1
      m3 = 0.

      n1 = p1 / ce
      n2 = p2 / ce
      n3 = 1.

      hh7 = -m2 / (m1*n2 - m2*n1)
      hh8 = -m1 / (m2*n1 - m1*n2)

      hh9  = hh7 * p1 / ce
      hh10 = hh8 * p2 / ce

      albv(iw,2) = hh7 + hh8
      tran(iw,2) = hh9 * s1 + hh10 / s1

      IF (abs(sigma) .gt. 1.e-10) THEN
         eup(iw,2)   = hh7  * (1. - s1*s2) / (extkb + psi) &
                     + hh8  * (1. - s2/s1) / (extkb - psi)
         edown(iw,2) = hh9  * (1. - s1*s2) / (extkb + psi) &
                     + hh10 * (1. - s2/s1) / (extkb - psi)
      ELSE
         eup(iw,2)   = hh7 * (1. - s1*s2) / ( extkb + psi) + hh8  * (lsai - 0.)
         edown(iw,2) = hh9 * (1. - s1*s2) / ( extkb + psi) + hh10 * (lsai - 0.)
      ENDIF

      ssun(iw,2) = (1.-scat) / zmu * (eup(iw,2) + edown(iw,2))
      ssha(iw,2) = tran(iw,2) * ( albg(iw,2) -1. ) - ( albv(iw,2) - 1. ) &
                 - ( 1. - scat ) / zmu * ( eup(iw,2) + edown(iw,2) )

      ENDDO           ! WAVE_BAND_LOOP

! 03/06/2020, yuan: add direct transmittance (s2) to
!                   tran for incident direct case
! 03/14/2020, yuan: save direct T to 3rd position of tran
      tran(:,3) = s2

   END SUBROUTINE twostream_hires


#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
   SUBROUTINE twostream_mod ( chil, rho, tau, green, lai, sai, fwet_snow, &
              coszen, albg, albv, tran, thermk, extkb, extkd, ssun, ssha )

!-----------------------------------------------------------------------
!
! !DESCRIPTION:
!     An improved two stream approximation
!
! Original author: Yongjiu Dai, June 11, 2001
!                  Hua Yuan, 03/2020
!
! REFERENCES:
! 1) Yuan, H., Dai, Y., Dickinson, R. E., Pinty, B., Shangguan, W., Zhang, S.,
! et al. (2017). Reexamination and further development of two-stream canopy
! radiative transfer models for global land modeling. Journal of Advances in
! Modeling Earth Systems, 9(1), 113–129. https://doi.org/10.1002/2016MS000773
!
!-----------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_Namelist, only: DEF_VEG_SNOW
   IMPLICIT NONE

! parameters
   real(r8), intent(in) :: &
          ! static parameters associated with vegetation type
            chil,          &! leaf angle distribution factor
            rho(2,2),      &! leaf reflectance (iw=iband, il=life and dead)
            tau(2,2),      &! leaf transmittance (iw=iband, il=life and dead)

          ! time-space varying vegetation parameters
            green,         &! green leaf fraction
            lai,           &! leaf area index of exposed canopy (snow-free)
            sai,           &! stem area index
            fwet_snow       ! vegetation snow fractional cover [-]

! environmental variables
   real(r8), intent(in) :: &
            coszen,        &! consine of solar zenith angle
            albg(2,2)       ! albedos of ground

! output
   real(r8), intent(out) :: &
            albv(2,2),     &! albedo, vegetation [-]
            tran(2,3),     &! canopy transmittances for solar radiation
            thermk,        &! canopy gap fraction for tir radiation
            extkb,         &! (k, g(mu)/mu) direct solar extinction coefficient
            extkd,         &! diffuse and scattered diffuse PAR extinction coefficient
            ssun(2,2),     &! sunlit canopy absorption for solar radiation
            ssha(2,2)       ! shaded canopy absorption for solar radiation,
                            ! normalized by the incident flux

!-------------------------- local -----------------------------------
   real(r8) :: &
            lsai,          &! lai+sai
            phi1,          &! (phi-1)
            phi2,          &! (phi-2)
            scat,          &! (omega)
            proj,          &! (g(mu))
            zmu,           &! (int(mu/g(mu))
            zmu2,          &! (zmu * zmu)
            as,            &! (a-s(mu))
            upscat,        &! (omega-beta)
            beta0,         &! (beta-0)
            psi,           &! (h)

            be,            &! (b)
            ce,            &! (c)
            de,            &! (d)
            fe,            &! (f)

            power1,        &! (h*lai)
            power2,        &! (k*lai)
            power3,        &!

            sigma,         &!
            s1,            &!
            s2,            &!
            p1,            &!
            p2,            &!
            p3,            &!
            p4,            &!
            f1,            &!
            f2,            &!
            h1,            &!
            h4,            &!
            m1,            &!
            m2,            &!
            m3,            &!
            n1,            &!
            n2,            &!
            n3,            &!

            hh1,           &! (h1/sigma)
            hh2,           &! (h2)
            hh3,           &! (h3)
            hh4,           &! (h4/sigma)
            hh5,           &! (h5)
            hh6,           &! (h6)
            hh7,           &! (h7)
            hh8,           &! (h8)
            hh9,           &! (h9)
            hh10,          &! (h10)

            eup,           &! (integral of i_up*exp(-kx) )
            edw             ! (integral of i_down*exp(-kx) )

   ! vegetation snow optical properties
   real(r8) :: upscat_sno = 0.5   !upscat parameter for snow
   real(r8) :: beta0_sno  = 0.5   !beta0 parameter for snow
   real(r8) :: scat_sno(2)        !snow single scattering albedo
   data scat_sno(1), scat_sno(2) /0.8, 0.4/   ! 1:vis, 2: nir

   integer iw                ! band loop index
   integer ic                ! direct/diffuse loop index

   ! variables for modified version
   real(r8) :: cosz, theta, cosdif, albgblk
   real(r8) :: tmptau, wrho, wtau
   real(r8) :: s2d, extkbd, sall(2,2), q, ssun_rev

!-----------------------------------------------------------------------
! projected area of phytoelements in direction of mu and
! average inverse diffuse optical depth per unit leaf area

      phi1  = 0.5 - 0.633 * chil - 0.33 * chil * chil
      phi2  = 0.877 * ( 1. - 2. * phi1 )

      extkd = 0.719

      IF (abs(phi1).gt.1.e-6 .and. abs(phi2).gt.1.e-6) THEN
         zmu = 1. / phi2 * ( 1. - phi1 / phi2 * log ( ( phi1 + phi2 ) / phi1 ) )
      ELSE IF (abs(phi1).le.1.e-6) THEN
         zmu = 1./0.877
      ELSE IF (abs(phi2).le.1.e-6) THEN
         zmu = 1./(2.*phi1)
      ENDIF
      zmu2 = zmu * zmu

      lsai   = lai + sai
      power3 = lsai / zmu
      power3 = min( 50., power3 )
      power3 = max( 1.e-5, power3 )
      thermk = exp(-power3)

      tmptau = 0.5_r8 * lsai
      cosdif = - tmptau / log(exp(-0.87_r8*tmptau) / (1+0.92_r8*tmptau))

      ! black ground case
      albgblk = 1.e-6_r8

      DO iw = 1, 2    ! WAVE_BAND_LOOP

      ! ic 1: incident direct; 2: incident diffuse
      DO ic = 1, 2

      IF (ic == 2) THEN
         cosz  = max(0.001_r8, cosdif)
         theta = acos(cosz)
         theta = theta/3.14159*180

         theta = theta + chil*5._r8
         cosz  = cos(theta/180*3.14159)
      ELSE
         cosz  = coszen
      ENDIF

      proj  = phi1 + phi2 * cosz
      extkb = proj / cosz

!-----------------------------------------------------------------------
!     calculate average scattering coefficient, leaf projection and
!     other coefficients for two-stream model.
!-----------------------------------------------------------------------

! + stem optical properties
      wtau = lai/lsai*tau(iw,1) + sai/lsai*tau(iw,2)
      wrho = lai/lsai*rho(iw,1) + sai/lsai*rho(iw,2)

      scat = wtau + wrho

      as = scat / 2. * proj / ( proj + cosz * phi2 )
      as = as * ( 1. - cosz * phi1 / ( proj + cosz * phi2 ) * &
                 log ( ( proj + cosz * phi2 + cosz * phi1 ) / ( cosz * phi1 ) ) )

! + stem optical properties
      ! scat ~ omega
      ! upscat ~ betail*scat
      ! beta0 ~ betadl
      ! scat-2.*upscat ~ rho - tau
      upscat = lai/lsai*tau(iw,1) + sai/lsai*tau(iw,2)
      upscat = 0.5 * ( scat + (scat - 2.*upscat) * ((1. + chil) / 2.) ** 2 )
      beta0  = ( 1. + zmu * extkb ) / ( scat * zmu * extkb ) * as

      ! [MODI 1]
      beta0 = 0.5_r8 * ( scat + 1._r8/extkb*(1._r8+chil)**2/4._r8*(wrho-wtau) )/scat

! account for snow on vegetation
      ! modify scat, upscat and beta0
      ! USE: fwet_snow, snow properties, scatter vis0.8, nir0.4, upscat0.5, beta0.5
      IF ( DEF_VEG_SNOW ) THEN
         scat   =   (1.-fwet_snow)*scat        + fwet_snow*scat_sno(iw)
         upscat = ( (1.-fwet_snow)*scat*upscat + fwet_snow*scat_sno(iw)*upscat_sno ) / scat
         beta0  = ( (1.-fwet_snow)*scat*beta0  + fwet_snow*scat_sno(iw)*beta0_sno  ) / scat
      ENDIF

!-----------------------------------------------------------------------
!     intermediate variables identified in appendix of SE-85.
!-----------------------------------------------------------------------

      be = 1. - scat + upscat
      ce = upscat
      de = scat * zmu * extkb * beta0
      fe = scat * zmu * extkb * ( 1. - beta0 )

      psi = sqrt(be**2 - ce**2)/zmu
      power1 = min( psi*lsai, 50. )
      power2 = min( extkb*lsai, 50. )
      s1 = exp( - power1 )
      s2 = exp( - power2 )

!-----------------------------------------------------------------------
!     calculation of direct albedos and canopy transmittances.
!     albv(iw,1)     ( i-up )
!     tran(iw,irad)  ( i-down )
!-----------------------------------------------------------------------

      p1 = be + zmu * psi
      p2 = be - zmu * psi
      p3 = be + zmu * extkb
      p4 = be - zmu * extkb

      f1 = 1. - albgblk*p1/ce
      f2 = 1. - albgblk*p2/ce

      h1 = - ( de * p4 + ce * fe )
      h4 = - ( fe * p3 + ce * de )

      sigma = ( zmu * extkb ) ** 2 + ( ce**2 - be**2 )

      IF (ic == 1) THEN
         s2d = s2
         extkbd = extkb
      ENDIF

      IF (abs(sigma) .gt. 1.e-10) THEN

         hh1 = h1 / sigma
         hh4 = h4 / sigma

         m1 = f1 * s1
         m2 = f2 / s1
         m3 = ( albgblk - ( hh1 - albgblk * hh4 ) ) * s2

         n1 = p1 / ce
         n2 = p2 / ce
         n3 = - hh4

         hh2 = (m3*n2 - m2*n3) / (m1*n2 - m2*n1)
         hh3 = (m3*n1 - m1*n3) / (m2*n1 - m1*n2)

         hh5 = hh2 * p1 / ce
         hh6 = hh3 * p2 / ce

         albv(iw,ic) = hh1 + hh2 + hh3
         tran(iw,ic) = hh4 * s2 + hh5 * s1 + hh6 / s1

         eup = hh1 * (1. - s2*s2d) / (extkbd + extkb) &
             + hh2 * (1. - s2d*s1) / (extkbd + psi) &
             + hh3 * (1. - s2d/s1) / (extkbd - psi)

         edw = hh4 * (1. - s2*s2d) / (extkbd + extkb) &
             + hh5 * (1. - s2d*s1) / (extkbd + psi) &
             + hh6 * (1. - s2d/s1) / (extkbd - psi)

      ELSE

         m1 = f1 * s1
         m2 = f2 / s1
         m3 = h1 / zmu2 * ( lsai + 1. / (extkb+extkbd) ) * s2 &
            + albgblk / ce * ( - h1 / (extkb+extkbd) / zmu2 * &
              ( p3*lsai + p4 / (extkb+extkbd) ) - de ) * s2 &
            + albgblk * s2

         n1 = p1 / ce
         n2 = p2 / ce
         n3 = 1./ce * ( h1*p4 / ((extkb+extkbd)*(extkb+extkbd)) / zmu2 + de)

         hh2 = (m3*n2 - m2*n3) / (m1*n2 - m2*n1)
         hh3 = (m3*n1 - m1*n3) / (m2*n1 - m1*n2)

         hh5 = hh2 * p1 / ce
         hh6 = hh3 * p2 / ce

         albv(iw,ic) =  - h1 / ((extkb+extkbd)*zmu2) + hh2 + hh3
         tran(iw,ic) = 1./ce * ( -h1 / ((extkb+extkbd)*zmu2) * &
                       ( p3*lsai + p4 / (extkb+extkbd) ) - de ) * s2 &
                     + hh5 * s1 + hh6 / s1

         eup = (hh2 - h1/((extkb+extkbd)*zmu2)) * (1. - s2*s2d)/(extkb+extkbd) &
             + hh3 * (lsai - 0.) &
             + h1/((extkb+extkbd)*zmu2) * ( lsai*s2*s2d - (1. - s2*s2d)/(extkb+extkbd) )

         edw = (hh5 - (h1*p4/((extkb+extkbd)*(extkb+extkbd)*zmu) + de)/ce) * &
               (1. - s2*s2d) / (extkb+extkbd) + hh6 * (lsai - 0.) &
             + h1*p3/(ce*(extkb+extkbd)*(extkb+extkbd)*zmu2) * &
               ( lsai*s2*s2d - (1. - s2*s2d)/(extkb+extkbd) )

      ENDIF

      sall(iw,ic) = 1. - albv(iw,ic) - (1.-albgblk)*(tran(iw,ic)+s2)

      IF (ic == 1) THEN
         ssun(iw,ic) = (1.-scat) * ( 1.-s2 + 1. / zmu * (eup + edw) )
      ELSE
         ssun(iw,ic) = (1.-scat) * ( extkb*(1.-s2*s2d)/(extkb+extkbd) + 1. / zmu * (eup + edw) )
      ENDIF

      ssha(iw,ic) = sall(iw,ic) - ssun(iw,ic)

      ENDDO ! ic

      ! for reversed diffuse radiation back from ground
      eup = hh1 * (1._r8 - s2/s2d) / (extkb - extkbd) &
          + hh2 * (1._r8 - s1/s2d) / (psi - extkbd) &
          + hh3 * (1._r8/s1/s2d - 1._r8) / (psi + extkbd)

      edw = hh4 * (1._r8 - s2/s2d) / (extkb - extkbd) &
          + hh5 * (1._r8 - s1/s2d) / (psi - extkbd) &
          + hh6 * (1._r8/s1/s2d - 1._r8) / (psi + extkbd)

      ssun_rev = s2d * (1._r8 - scat) * &
                 ( extkb*(1._r8-s2/s2d)/(extkb-extkbd) + 1._r8 / zmu * (eup + edw ) )

      ! -----------------------------------------------------------
      ! consider the multiple reflectance between canopy and ground
      ! -----------------------------------------------------------

      ! common ratio for geometric series
      q = albg(iw,2) * albv(iw,2)

      DO ic = 1, 2 ! from 1 to 2, cannot be reversed

         ! -----------------------------------------------------------
         ! re-calculate the absorption, transmission and albedo
         ! for direct radiation

! 03/06/2020, yuan: tran originally meant diffuse flow, now the direct
!                   transmittance is also included
! 03/14/2020, yuan: treat soil albedo in direct/diffuse cases
         IF (ic == 1) THEN
            tran(iw,ic) = (s2d*albg(iw,1)*albv(iw,2) + tran(iw,ic)) / (1.-q)
            tran(:,3)   = s2d

            sall(iw,ic) = sall(iw,ic) + &
               (tran(iw,ic)*albg(iw,2) + s2d*albg(iw,1)) * sall(iw,2)

            albv(iw,ic) = 1. - sall(iw,ic) - &
               (1.-albg(iw,2))*tran(iw,ic) - (1.-albg(iw,1))*s2d

            ssun(iw,ic) = ssun(iw,ic) + &
               (tran(iw,ic)*albg(iw,2) + s2d*albg(iw,1)) * ssun_rev

            ssha(iw,ic) = sall(iw,ic) - ssun(iw,ic)

         ELSE
            tran(iw,ic) = (s2 + tran(iw,ic)) / (1.-q)

            sall(iw,ic) = sall(iw,ic) + tran(iw,ic)*albg(iw,2)*sall(iw,2)
            albv(iw,ic) = 1. - sall(iw,ic) - (1.-albg(iw,2))*tran(iw,ic)

            ssun(iw,ic) = ssun(iw,ic) + tran(iw,ic)*albg(iw,2)*ssun_rev
            ssha(iw,ic) = sall(iw,ic) - ssun(iw,ic)
         ENDIF

      ENDDO !ic

      End DO !iw

      ! restore extkb
      extkb = extkbd

   END SUBROUTINE twostream_mod


   SUBROUTINE twostream_hires_mod ( chil, rho, tau, green, lai, sai, fwet_snow, &
              coszen, albg, albv, tran, thermk, extkb, extkd, ssun, ssha )

!-----------------------------------------------------------------------
!
! !DESCRIPTION:
!     An improved two stream approximation
!
! Original author: Yongjiu Dai, June 11, 2001
!                  Hua Yuan, 03/2020
!
! REFERENCES:
! 1) Yuan, H., Dai, Y., Dickinson, R. E., Pinty, B., Shangguan, W., Zhang, S.,
! et al. (2017). Reexamination and further development of two-stream canopy
! radiative transfer models for global land modeling. Journal of Advances in
! Modeling Earth Systems, 9(1), 113–129. https://doi.org/10.1002/2016MS000773
!
!-----------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_Namelist, only: DEF_VEG_SNOW
   IMPLICIT NONE

! parameters
   real(r8), intent(in) :: &
          ! static parameters associated with vegetation type
            chil,          &! leaf angle distribution factor
            rho(211,2),      &! leaf reflectance (iw=iband, il=life and dead)
            tau(211,2),      &! leaf transmittance (iw=iband, il=life and dead)

          ! time-space varying vegetation parameters
            green,         &! green leaf fraction
            lai,           &! leaf area index of exposed canopy (snow-free)
            sai,           &! stem area index
            fwet_snow       ! vegetation snow fractional cover [-]

! environmental variables
   real(r8), intent(in) :: &
            coszen,        &! consine of solar zenith angle
            albg(211,2)       ! albedos of ground

! output
   real(r8), intent(out) :: &
            albv(211,2),     &! albedo, vegetation [-]
            tran(211,3),     &! canopy transmittances for solar radiation
            thermk,        &! canopy gap fraction for tir radiation
            extkb,         &! (k, g(mu)/mu) direct solar extinction coefficient
            extkd,         &! diffuse and scattered diffuse PAR extinction coefficient
            ssun(211,2),     &! sunlit canopy absorption for solar radiation
            ssha(211,2)       ! shaded canopy absorption for solar radiation,
                            ! normalized by the incident flux

!-------------------------- local -----------------------------------
   real(r8) :: &
            lsai,          &! lai+sai
            phi1,          &! (phi-1)
            phi2,          &! (phi-2)
            scat,          &! (omega)
            proj,          &! (g(mu))
            zmu,           &! (int(mu/g(mu))
            zmu2,          &! (zmu * zmu)
            as,            &! (a-s(mu))
            upscat,        &! (omega-beta)
            beta0,         &! (beta-0)
            psi,           &! (h)

            be,            &! (b)
            ce,            &! (c)
            de,            &! (d)
            fe,            &! (f)

            power1,        &! (h*lai)
            power2,        &! (k*lai)
            power3,        &!

            sigma,         &!
            s1,            &!
            s2,            &!
            p1,            &!
            p2,            &!
            p3,            &!
            p4,            &!
            f1,            &!
            f2,            &!
            h1,            &!
            h4,            &!
            m1,            &!
            m2,            &!
            m3,            &!
            n1,            &!
            n2,            &!
            n3,            &!

            hh1,           &! (h1/sigma)
            hh2,           &! (h2)
            hh3,           &! (h3)
            hh4,           &! (h4/sigma)
            hh5,           &! (h5)
            hh6,           &! (h6)
            hh7,           &! (h7)
            hh8,           &! (h8)
            hh9,           &! (h9)
            hh10,          &! (h10)

            eup,           &! (integral of i_up*exp(-kx) )
            edw             ! (integral of i_down*exp(-kx) )

   ! vegetation snow optical properties
   real(r8) :: upscat_sno = 0.5   !upscat parameter for snow
   real(r8) :: beta0_sno  = 0.5   !beta0 parameter for snow
   real(r8) :: scat_sno(2)        !snow single scattering albedo
   data scat_sno(1), scat_sno(2) /0.8, 0.4/   ! 1:vis, 2: nir
   real(r8) :: scat_sno_tmp

   integer iw                ! band loop index
   integer ic                ! direct/diffuse loop index

   ! variables for modified version
   real(r8) :: cosz, theta, cosdif, albgblk
   real(r8) :: tmptau, wrho, wtau
   real(r8) :: s2d, extkbd, sall(211,2), q, ssun_rev

!-----------------------------------------------------------------------
! projected area of phytoelements in direction of mu and
! average inverse diffuse optical depth per unit leaf area

      phi1  = 0.5 - 0.633 * chil - 0.33 * chil * chil
      phi2  = 0.877 * ( 1. - 2. * phi1 )

      extkd = 0.719

      IF (abs(phi1).gt.1.e-6 .and. abs(phi2).gt.1.e-6) THEN
         zmu = 1. / phi2 * ( 1. - phi1 / phi2 * log ( ( phi1 + phi2 ) / phi1 ) )
      ELSE IF (abs(phi1).le.1.e-6) THEN
         zmu = 1./0.877
      ELSE IF (abs(phi2).le.1.e-6) THEN
         zmu = 1./(2.*phi1)
      ENDIF
      zmu2 = zmu * zmu

      lsai   = lai + sai
      power3 = lsai / zmu
      power3 = min( 50., power3 )
      power3 = max( 1.e-5, power3 )
      thermk = exp(-power3)

      tmptau = 0.5_r8 * lsai
      cosdif = - tmptau / log(exp(-0.87_r8*tmptau) / (1+0.92_r8*tmptau))

      ! black ground case
      albgblk = 1.e-6_r8

      DO iw = 1, 211    ! WAVE_BAND_LOOP

      ! ic 1: incident direct; 2: incident diffuse
      DO ic = 1, 2

      IF (ic == 2) THEN
         cosz  = max(0.001_r8, cosdif)
         theta = acos(cosz)
         theta = theta/3.14159*180

         theta = theta + chil*5._r8
         cosz  = cos(theta/180*3.14159)
      ELSE
         cosz  = coszen
      ENDIF

      proj  = phi1 + phi2 * cosz
      extkb = proj / cosz

!-----------------------------------------------------------------------
!     calculate average scattering coefficient, leaf projection and
!     other coefficients for two-stream model.
!-----------------------------------------------------------------------

! + stem optical properties
      wtau = lai/lsai*tau(iw,1) + sai/lsai*tau(iw,2)
      wrho = lai/lsai*rho(iw,1) + sai/lsai*rho(iw,2)

      scat = wtau + wrho

      as = scat / 2. * proj / ( proj + cosz * phi2 )
      as = as * ( 1. - cosz * phi1 / ( proj + cosz * phi2 ) * &
                 log ( ( proj + cosz * phi2 + cosz * phi1 ) / ( cosz * phi1 ) ) )

! + stem optical properties
      ! scat ~ omega
      ! upscat ~ betail*scat
      ! beta0 ~ betadl
      ! scat-2.*upscat ~ rho - tau
      upscat = lai/lsai*tau(iw,1) + sai/lsai*tau(iw,2)
      upscat = 0.5 * ( scat + (scat - 2.*upscat) * ((1. + chil) / 2.) ** 2 )
      beta0  = ( 1. + zmu * extkb ) / ( scat * zmu * extkb ) * as

      ! [MODI 1]
      beta0 = 0.5_r8 * ( scat + 1._r8/extkb*(1._r8+chil)**2/4._r8*(wrho-wtau) )/scat

! account for snow on vegetation
      ! modify scat, upscat and beta0
      ! USE: fwet_snow, snow properties, scatter vis0.8, nir0.4, upscat0.5, beta0.5
      IF ( DEF_VEG_SNOW ) THEN
         if (iw < 30) then
            scat_sno_tmp = scat_sno(1)
         else
            scat_sno_tmp = scat_sno(2)
         end if
         scat   =   (1.-fwet_snow)*scat        + fwet_snow*scat_sno_tmp
         upscat = ( (1.-fwet_snow)*scat*upscat + fwet_snow*scat_sno_tmp*upscat_sno ) / scat
         beta0  = ( (1.-fwet_snow)*scat*beta0  + fwet_snow*scat_sno_tmp*beta0_sno  ) / scat
      ENDIF

!-----------------------------------------------------------------------
!     intermediate variables identified in appendix of SE-85.
!-----------------------------------------------------------------------

      be = 1. - scat + upscat
      ce = upscat
      de = scat * zmu * extkb * beta0
      fe = scat * zmu * extkb * ( 1. - beta0 )

      psi = sqrt(be**2 - ce**2)/zmu
      power1 = min( psi*lsai, 50. )
      power2 = min( extkb*lsai, 50. )
      s1 = exp( - power1 )
      s2 = exp( - power2 )

!-----------------------------------------------------------------------
!     calculation of direct albedos and canopy transmittances.
!     albv(iw,1)     ( i-up )
!     tran(iw,irad)  ( i-down )
!-----------------------------------------------------------------------

      p1 = be + zmu * psi
      p2 = be - zmu * psi
      p3 = be + zmu * extkb
      p4 = be - zmu * extkb

      f1 = 1. - albgblk*p1/ce
      f2 = 1. - albgblk*p2/ce

      h1 = - ( de * p4 + ce * fe )
      h4 = - ( fe * p3 + ce * de )

      sigma = ( zmu * extkb ) ** 2 + ( ce**2 - be**2 )

      IF (ic == 1) THEN
         s2d = s2
         extkbd = extkb
      ENDIF

      IF (abs(sigma) .gt. 1.e-10) THEN

         hh1 = h1 / sigma
         hh4 = h4 / sigma

         m1 = f1 * s1
         m2 = f2 / s1
         m3 = ( albgblk - ( hh1 - albgblk * hh4 ) ) * s2

         n1 = p1 / ce
         n2 = p2 / ce
         n3 = - hh4

         hh2 = (m3*n2 - m2*n3) / (m1*n2 - m2*n1)
         hh3 = (m3*n1 - m1*n3) / (m2*n1 - m1*n2)

         hh5 = hh2 * p1 / ce
         hh6 = hh3 * p2 / ce

         albv(iw,ic) = hh1 + hh2 + hh3
         tran(iw,ic) = hh4 * s2 + hh5 * s1 + hh6 / s1

         eup = hh1 * (1. - s2*s2d) / (extkbd + extkb) &
             + hh2 * (1. - s2d*s1) / (extkbd + psi) &
             + hh3 * (1. - s2d/s1) / (extkbd - psi)

         edw = hh4 * (1. - s2*s2d) / (extkbd + extkb) &
             + hh5 * (1. - s2d*s1) / (extkbd + psi) &
             + hh6 * (1. - s2d/s1) / (extkbd - psi)

      ELSE

         m1 = f1 * s1
         m2 = f2 / s1
         m3 = h1 / zmu2 * ( lsai + 1. / (extkb+extkbd) ) * s2 &
            + albgblk / ce * ( - h1 / (extkb+extkbd) / zmu2 * &
              ( p3*lsai + p4 / (extkb+extkbd) ) - de ) * s2 &
            + albgblk * s2

         n1 = p1 / ce
         n2 = p2 / ce
         n3 = 1./ce * ( h1*p4 / ((extkb+extkbd)*(extkb+extkbd)) / zmu2 + de)

         hh2 = (m3*n2 - m2*n3) / (m1*n2 - m2*n1)
         hh3 = (m3*n1 - m1*n3) / (m2*n1 - m1*n2)

         hh5 = hh2 * p1 / ce
         hh6 = hh3 * p2 / ce

         albv(iw,ic) =  - h1 / ((extkb+extkbd)*zmu2) + hh2 + hh3
         tran(iw,ic) = 1./ce * ( -h1 / ((extkb+extkbd)*zmu2) * &
                       ( p3*lsai + p4 / (extkb+extkbd) ) - de ) * s2 &
                     + hh5 * s1 + hh6 / s1

         eup = (hh2 - h1/((extkb+extkbd)*zmu2)) * (1. - s2*s2d)/(extkb+extkbd) &
             + hh3 * (lsai - 0.) &
             + h1/((extkb+extkbd)*zmu2) * ( lsai*s2*s2d - (1. - s2*s2d)/(extkb+extkbd) )

         edw = (hh5 - (h1*p4/((extkb+extkbd)*(extkb+extkbd)*zmu) + de)/ce) * &
               (1. - s2*s2d) / (extkb+extkbd) + hh6 * (lsai - 0.) &
             + h1*p3/(ce*(extkb+extkbd)*(extkb+extkbd)*zmu2) * &
               ( lsai*s2*s2d - (1. - s2*s2d)/(extkb+extkbd) )

      ENDIF

      sall(iw,ic) = 1. - albv(iw,ic) - (1.-albgblk)*(tran(iw,ic)+s2)

      IF (ic == 1) THEN
         ssun(iw,ic) = (1.-scat) * ( 1.-s2 + 1. / zmu * (eup + edw) )
      ELSE
         ssun(iw,ic) = (1.-scat) * ( extkb*(1.-s2*s2d)/(extkb+extkbd) + 1. / zmu * (eup + edw) )
      ENDIF

      ssha(iw,ic) = sall(iw,ic) - ssun(iw,ic)

      ENDDO ! ic

      ! for reversed diffuse radiation back from ground
      eup = hh1 * (1._r8 - s2/s2d) / (extkb - extkbd) &
          + hh2 * (1._r8 - s1/s2d) / (psi - extkbd) &
          + hh3 * (1._r8/s1/s2d - 1._r8) / (psi + extkbd)

      edw = hh4 * (1._r8 - s2/s2d) / (extkb - extkbd) &
          + hh5 * (1._r8 - s1/s2d) / (psi - extkbd) &
          + hh6 * (1._r8/s1/s2d - 1._r8) / (psi + extkbd)

      ssun_rev = s2d * (1._r8 - scat) * &
                 ( extkb*(1._r8-s2/s2d)/(extkb-extkbd) + 1._r8 / zmu * (eup + edw ) )

      ! -----------------------------------------------------------
      ! consider the multiple reflectance between canopy and ground
      ! -----------------------------------------------------------

      ! common ratio for geometric series
      q = albg(iw,2) * albv(iw,2)

      DO ic = 1, 2 ! from 1 to 2, cannot be reversed

         ! -----------------------------------------------------------
         ! re-calculate the absorption, transmission and albedo
         ! for direct radiation

! 03/06/2020, yuan: tran originally meant diffuse flow, now the direct
!                   transmittance is also included
! 03/14/2020, yuan: treat soil albedo in direct/diffuse cases
         IF (ic == 1) THEN
            tran(iw,ic) = (s2d*albg(iw,1)*albv(iw,2) + tran(iw,ic)) / (1.-q)
            tran(:,3)   = s2d

            sall(iw,ic) = sall(iw,ic) + &
               (tran(iw,ic)*albg(iw,2) + s2d*albg(iw,1)) * sall(iw,2)

            albv(iw,ic) = 1. - sall(iw,ic) - &
               (1.-albg(iw,2))*tran(iw,ic) - (1.-albg(iw,1))*s2d

            ssun(iw,ic) = ssun(iw,ic) + &
               (tran(iw,ic)*albg(iw,2) + s2d*albg(iw,1)) * ssun_rev

            ssha(iw,ic) = sall(iw,ic) - ssun(iw,ic)

         ELSE
            tran(iw,ic) = (s2 + tran(iw,ic)) / (1.-q)

            sall(iw,ic) = sall(iw,ic) + tran(iw,ic)*albg(iw,2)*sall(iw,2)
            albv(iw,ic) = 1. - sall(iw,ic) - (1.-albg(iw,2))*tran(iw,ic)

            ssun(iw,ic) = ssun(iw,ic) + tran(iw,ic)*albg(iw,2)*ssun_rev
            ssha(iw,ic) = sall(iw,ic) - ssun(iw,ic)
         ENDIF

      ENDDO !ic

      End DO !iw

      ! restore extkb
      extkb = extkbd

   END SUBROUTINE twostream_hires_mod
#endif


#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
   SUBROUTINE twostream_wrap ( ipatch, coszen, albg, &
              albv, tran, ssun, ssha )

!-----------------------------------------------------------------------
!
! !DESCRIPTION:
! A Wrap subroutine to calculate PFT radiation using two-stream model
!
! Created by Hua Yuan, 03/2020
!
!-----------------------------------------------------------------------
      USE MOD_Precision
      USE MOD_LandPFT
      USE MOD_Const_PFT
      USE MOD_Vars_PFTimeInvariants
      USE MOD_Vars_PFTimeVariables
      IMPLICIT NONE

      ! parameters
      integer, intent(in) :: &
            ipatch          ! patch index

      ! environmental variables
      real(r8), intent(in) ::  &
            coszen,        &! consine of solar zenith angle
            albg(2,2)       ! albedos of ground

      ! output
      real(r8), intent(out) :: &
            albv(2,2),     &! albedo, vegetation [-]
            tran(2,3),     &! canopy transmittances for solar radiation
            ssun(2,2),     &! sunlit canopy absorption for solar radiation
            ssha(2,2)       ! shaded canopy absorption for solar radiation,
                            ! normalized by the incident flux

      integer :: i, p, ps, pe
      real(r8), allocatable :: tran_p(:,:,:)
      real(r8), allocatable :: albv_p(:,:,:)

      ps = patch_pft_s(ipatch)
      pe = patch_pft_e(ipatch)

      allocate ( tran_p (2,3,ps:pe) )
      allocate ( albv_p (2,2,ps:pe) )

      DO i = ps, pe
         p = pftclass(i)
         IF (lai_p(i)+sai_p(i) > 1.e-6) THEN
            CALL twostream_mod (chil_p(p),rho_p(:,:,p),tau_p(:,:,p),1.,lai_p(i),sai_p(i),&
                 fwet_snow_p(i),coszen,albg,albv_p(:,:,i),tran_p(:,:,i),thermk_p(i),&
                 extkb_p(i),extkd_p(i),ssun_p(:,:,i),ssha_p(:,:,i))
         ELSE
            albv_p(:,:,i) = albg(:,:)
            ssun_p(:,:,i) = 0.
            ssha_p(:,:,i) = 0.
            tran_p(:,1,i) = 0.
            tran_p(:,2,i) = 1.
            tran_p(:,3,i) = 1.
         ENDIF
      ENDDO

      albv(1,1) = sum( albv_p(1,1,ps:pe)*pftfrac(ps:pe) )
      albv(1,2) = sum( albv_p(1,2,ps:pe)*pftfrac(ps:pe) )
      albv(2,1) = sum( albv_p(2,1,ps:pe)*pftfrac(ps:pe) )
      albv(2,2) = sum( albv_p(2,2,ps:pe)*pftfrac(ps:pe) )

      ssun(1,1) = sum( ssun_p(1,1,ps:pe)*pftfrac(ps:pe) )
      ssun(1,2) = sum( ssun_p(1,2,ps:pe)*pftfrac(ps:pe) )
      ssun(2,1) = sum( ssun_p(2,1,ps:pe)*pftfrac(ps:pe) )
      ssun(2,2) = sum( ssun_p(2,2,ps:pe)*pftfrac(ps:pe) )

      ssha(1,1) = sum( ssha_p(1,1,ps:pe)*pftfrac(ps:pe) )
      ssha(1,2) = sum( ssha_p(1,2,ps:pe)*pftfrac(ps:pe) )
      ssha(2,1) = sum( ssha_p(2,1,ps:pe)*pftfrac(ps:pe) )
      ssha(2,2) = sum( ssha_p(2,2,ps:pe)*pftfrac(ps:pe) )

      tran(1,1) = sum( tran_p(1,1,ps:pe)*pftfrac(ps:pe) )
      tran(1,2) = sum( tran_p(1,2,ps:pe)*pftfrac(ps:pe) )
      tran(1,3) = sum( tran_p(1,3,ps:pe)*pftfrac(ps:pe) )
      tran(2,1) = sum( tran_p(2,1,ps:pe)*pftfrac(ps:pe) )
      tran(2,2) = sum( tran_p(2,2,ps:pe)*pftfrac(ps:pe) )
      tran(2,3) = sum( tran_p(2,3,ps:pe)*pftfrac(ps:pe) )

      !NOTE: fordebug only below
      IF (ssun(1,1)<0 .or. ssun(1,2)<0 .or. ssun(2,1)<0 .or. ssun(2,2)<0) THEN
         print *, 'Warning:negative albedo',ipatch
         print *, ssun
      ENDIF

      deallocate ( tran_p )
      deallocate ( albv_p )

   END SUBROUTINE twostream_wrap

   SUBROUTINE twostream_hires_wrap ( ipatch, coszen, albg, &
              albv, tran, ssun, ssha, &
              reflectance, transmittance  ,&
              fsds_vis_dir_frac, fsds_nir_dir_frac,&
              fsds_vis_dif_frac, fsds_nir_dif_frac,&
              ssw, reflectance_out, transmittance_out, doy )

!-----------------------------------------------------------------------
!
! !DESCRIPTION:
! A Wrap subroutine to calculate PFT radiation using two-stream model
!
! Created by Hua Yuan, 03/2020
!
!-----------------------------------------------------------------------
      USE MOD_Precision
      USE MOD_LandPFT
      USE MOD_Const_PFT
      USE MOD_Vars_PFTimeInvariants
      USE MOD_Vars_PFTimeVariables
      USE MOD_HighRes_Parameters , only: update_params_PROSPECT!, satellite_PROSPECT
      USE MOD_Namelist, only: DEF_HighResVeg, DEF_PROSPECT!, DEF_Satellite_Params

      IMPLICIT NONE

      ! parameters
      integer, intent(in) :: &
            ipatch,          &! patch index
            doy

      ! environmental variables
      real(r8), intent(in) ::  &
            coszen,        &! consine of solar zenith angle
            albg(211,2)       ! albedos of ground

      ! high resolution optical properties
      real(r8), intent(in) ::  &
            reflectance  (0:15,211,2), &! leaf reflectance
            transmittance(0:15,211,2), &! leaf transmittance
            fsds_vis_dir_frac(29 )     ,&
            fsds_nir_dir_frac(182)     ,&
            fsds_vis_dif_frac(29 )     ,&
            fsds_nir_dif_frac(182)     ,&
            ssw

      ! output
      real(r8), intent(out) :: &
            albv(211,2),     &! albedo, vegetation [-]
            tran(211,3),     &! canopy transmittances for solar radiation
            ssun(211,2),     &! sunlit canopy absorption for solar radiation
            ssha(211,2)       ! shaded canopy absorption for solar radiation,
                              ! normalized by the incident flux

      real(r8), intent(inout) :: &
            reflectance_out  (211,0:15), &! leaf reflectance
            transmittance_out(211,0:15)   ! leaf transmittance

      integer :: i, p, ps, pe, iwl
      real(r8) :: reflectance_p  (211, 2)
      real(r8) :: transmittance_p(211, 2)

      real(r8), allocatable :: tran_p(:,:,:)
      real(r8), allocatable :: albv_p(:,:,:)

      real(r8) :: rho_hires(211), tau_hires(211)

      ps = patch_pft_s(ipatch)
      pe = patch_pft_e(ipatch)

      allocate ( tran_p (211,3,ps:pe) )
      allocate ( albv_p (211,2,ps:pe) )

      DO i = ps, pe
         p = pftclass(i)
         IF (lai_p(i)+sai_p(i) > 1.e-6) THEN

            ! IF use PROSPECT, update the high resolution optical properties
            ! IF ( DEF_PROSPECT .AND. DEF_Satellite_Params ) THEN
            !    CALL satellite_PROSPECT(p, reflectance, transmittance ,&
            !                            reflectance_p, transmittance_p,&
            !                            ssw, doy)
            ! ELSE IF ( DEF_PROSPECT ) THEN
            IF ( DEF_PROSPECT ) THEN
               CALL update_params_PROSPECT(p, reflectance, transmittance ,&
                                           reflectance_p, transmittance_p,&
                                           ssw)
            ELSE
               reflectance_p   = reflectance  (p,:,:)
               transmittance_p = transmittance(p,:,:)
            ENDIF

            reflectance_out  (:,p) = reflectance_p  (:,1)
            transmittance_out(:,p) = transmittance_p(:,1)

            CALL twostream_hires_mod (chil_p(p),reflectance_p,transmittance_p,1.,lai_p(i),sai_p(i),&
                 fwet_snow_p(i),coszen,albg,albv_p(:,:,i),tran_p(:,:,i),thermk_p(i),&
                 extkb_p(i),extkd_p(i),ssun_hires_p(:,:,i),ssha_hires_p(:,:,i))

            CALL calculate_wgt_variable(ssun_hires_p(:,1,i), fsds_vis_dir_frac, fsds_nir_dir_frac, ssun_p(1,1,i), ssun_p(2,1,i))
            CALL calculate_wgt_variable(ssun_hires_p(:,2,i), fsds_vis_dif_frac, fsds_nir_dif_frac, ssun_p(1,2,i), ssun_p(2,2,i))

            CALL calculate_wgt_variable(ssha_hires_p(:,1,i), fsds_vis_dir_frac, fsds_nir_dir_frac, ssha_p(1,1,i), ssha_p(2,1,i))
            CALL calculate_wgt_variable(ssha_hires_p(:,2,i), fsds_vis_dif_frac, fsds_nir_dif_frac, ssha_p(1,2,i), ssha_p(2,2,i))

         ELSE
            albv_p(:,:,i) = albg(:,:)

            ssun_hires_p(:,:,i) = 0.
            ssha_hires_p(:,:,i) = 0.

            ssun_p(:,:,i) = 0.
            ssha_p(:,:,i) = 0.
            tran_p(:,1,i) = 0.
            tran_p(:,2,i) = 1.
            tran_p(:,3,i) = 1.
         ENDIF
      ENDDO

      DO iwl = 1, 211
         albv(iwl,1) = SUM( albv_p(iwl,1,ps:pe)*pftfrac(ps:pe) )
         albv(iwl,2) = SUM( albv_p(iwl,2,ps:pe)*pftfrac(ps:pe) )

         ssun(iwl,1) = sum( ssun_hires_p(iwl,1,ps:pe)*pftfrac(ps:pe) )
         ssun(iwl,2) = sum( ssun_hires_p(iwl,2,ps:pe)*pftfrac(ps:pe) )

         ssha(iwl,1) = sum( ssha_hires_p(iwl,1,ps:pe)*pftfrac(ps:pe) )
         ssha(iwl,2) = sum( ssha_hires_p(iwl,2,ps:pe)*pftfrac(ps:pe) )

         tran(iwl,1) = sum( tran_p(iwl,1,ps:pe)*pftfrac(ps:pe) )
         tran(iwl,2) = sum( tran_p(iwl,2,ps:pe)*pftfrac(ps:pe) )
         tran(iwl,3) = sum( tran_p(iwl,3,ps:pe)*pftfrac(ps:pe) )
      END DO

      !NOTE: fordebug only below
      IF ( ANY(ssun < 0) ) THEN
         print *, 'Warning:negative albedo',ipatch
         print *, ssun
      ENDIF

      deallocate ( tran_p )
      deallocate ( albv_p )

   END SUBROUTINE twostream_hires_wrap
#endif


   SUBROUTINE snowage ( deltim,tg,scv,scvold,sag )

!=======================================================================
! Original version: Robert Dickinson
! Update snow cover and snow age, based on BATS code
!=======================================================================

   USE MOD_Precision
   USE MOD_Const_Physical, only : tfrz
   IMPLICIT NONE

!-------------------------- Dummy Argument -----------------------------

   real(r8), intent(in) :: deltim ! seconds in a time step [second]
   real(r8), intent(in) :: tg     ! temperature of soil at surface [K]
   real(r8), intent(in) :: scv    ! snow cover, water equivalent [mm]
   real(r8), intent(in) :: scvold ! snow cover for previous time step [mm]
   real(r8), intent(inout) :: sag ! non dimensional snow age [-]

!-------------------------- Local variables ----------------------------

   real(r8) :: age1   ! snow aging factor due to crystal growth [-]
   real(r8) :: age2   ! snow aging factor due to surface growth [-]
   real(r8) :: age3   ! snow aging factor due to accum of other particles [-]
   real(r8) :: arg    ! temporary variable used in snow age calculation [-]
   real(r8) :: arg2   ! temporary variable used in snow age calculation [-]
   real(r8) :: dela   ! temporary variable used in snow age calculation [-]
   real(r8) :: dels   ! temporary variable used in snow age calculation [-]
   real(r8) :: sge    ! temporary variable used in snow age calculation [-]

!-----------------------------------------------------------------------
      IF(scv <= 0.) THEN
         sag = 0.
!
! Over antarctica
!
      ELSE IF (scv > 800.) THEN
         sag = 0.
!
! Away from antarctica
!
      ELSE
         age3  = 0.3
         arg   = 5.e3*(1./tfrz-1./tg)
         arg2  = min(0.,10.*arg)
         age2  = exp(arg2)
         age1  = exp(arg)
         dela  = 1.e-6*deltim*(age1+age2+age3)
         dels  = 0.1*max(0.0,scv-scvold)
         sge   = (sag+dela)*(1.0-dels)
         sag   = max(0.0,sge)
      ENDIF

   END SUBROUTINE snowage


   SUBROUTINE SnowAlbedo( use_snicar_frc,use_snicar_ad ,coszen_col    ,&
                          albsod        ,albsoi        ,snl           ,frac_sno      ,&
                          h2osno        ,h2osno_liq    ,h2osno_ice    ,snw_rds       ,&

                          mss_cnc_bcphi ,mss_cnc_bcpho ,mss_cnc_ocphi ,mss_cnc_ocpho ,&
                          mss_cnc_dst1  ,mss_cnc_dst2  ,mss_cnc_dst3  ,mss_cnc_dst4  ,&

                          albgrd        ,albgri        ,albgrd_pur    ,albgri_pur    ,&
                          albgrd_bc     ,albgri_bc     ,albgrd_oc     ,albgri_oc     ,&
                          albgrd_dst    ,albgri_dst    ,flx_absdv     ,flx_absdn     ,&
                          flx_absiv     ,flx_absin     ,dir_frac      ,dif_frac       )

   ! !DESCRIPTION:
   ! The calling sequence is:
   ! -> SNICAR_RT:   snow albedos: direct beam (SNICAR)
   !    or
   !    SNICAR_AD_RT: snow albedos: direct beam (SNICAR-AD)
   ! -> SNICAR_RT:   snow albedos: diffuse (SNICAR)
   !    or
   !    SNICAR_AD_RT:   snow albedos: diffuse (SNICAR-AD)
   !
   ! ORIGINAL:
   ! 1) The Community Land Model version5.0 (CLM5.0)
   ! 2) Energy Exascale Earth System Model version 2.0 (E3SM v2.0) Land Model (ELM v2.0)
   !
   ! REFERENCES:
   ! 1) Flanner et al, 2021, SNICAR-ADv3: a community tool for modeling spectral snow albedo.
   ! Geosci. Model Dev., 14, 7673–7704, https://doi.org/10.5194/gmd-14-7673-2021
   ! 2) Hao et al., 2023, Improving snow albedo modeling in the E3SM land model (version 2.0)
   ! and assessing its impacts on snow and surface fluxes over the Tibetan Plateau.
   ! Geosci. Model Dev., 16, 75–94, https://doi.org/10.5194/gmd-16-75-2023
   !
   ! REVISIONS:
   ! Yongjiu Dai, and Hua Yuan, December, 2022 : ASSEMBLING and FITTING

   !-----------------------------------------------------------------------
   ! !USES:
   USE MOD_Vars_Global, only: maxsnl
   USE MOD_SnowSnicar_HiRes, only: SNICAR_RT, SNICAR_AD_RT

   ! and the evolution of snow effective radius
   !
   ! DAI, Dec. 28, 2022

   IMPLICIT NONE

!-------------------------------------------------------------------------
! temporay setting

   integer, parameter :: numrad  = 5            !  number of solar radiation bands: vis, nir
   integer, parameter :: numhires = 211            !  number of solar radiation bands: vis, nir
   integer, parameter :: sno_nbr_aer = 8        !  number of aerosol species in snowpack
   logical, parameter :: DO_SNO_OC   = .true.   !  parameter to include organic carbon (OC)
   logical, parameter :: DO_SNO_AER  = .true.   !  parameter to include aerosols in snowpack radiative calculations
   integer, parameter :: subgridflag = 1        !  = 0 USE subgrid fluxes, = 1 not USE subgrid fluxes
   !
   ! !ARGUMENTS:
   !
   logical , intent(in) :: use_snicar_frc       !  true: IF radiative forcing is being calculated, first estimate clean-snow albedo
   logical , intent(in) :: use_snicar_ad        !  true: USE SNICAR_AD_RT, false: USE SNICAR_RT

   real(r8), intent(in) :: coszen_col                   ! cosine of solar zenith angle
   real(r8), intent(in) :: albsod        ( numhires )     ! direct-beam soil albedo (col,bnd) [frc]
   real(r8), intent(in) :: albsoi        ( numhires )     ! diffuse soil albedo (col,bnd) [frc]

   integer , intent(in) :: snl                          ! negative number of snow layers (col) [nbr]
   real(r8), intent(in) :: frac_sno                     ! fraction of ground covered by snow (0 to 1)
   real(r8), intent(in) :: h2osno                       ! snow water equivalent (mm H2O)
   real(r8), intent(in) :: h2osno_liq    ( maxsnl+1:0 ) ! liquid water content (col,lyr) [kg/m2]
   real(r8), intent(in) :: h2osno_ice    ( maxsnl+1:0 ) ! ice lens content (col,lyr) [kg/m2]
   real(r8), intent(in) :: snw_rds       ( maxsnl+1:0 ) ! snow grain radius (col,lyr) [microns]

   real(r8), intent(in) :: mss_cnc_bcphi ( maxsnl+1:0 ) ! mass concentration of hydrophilic BC (col,lyr) [kg/kg]
   real(r8), intent(in) :: mss_cnc_bcpho ( maxsnl+1:0 ) ! mass concentration of hydrophobic BC (col,lyr) [kg/kg]
   real(r8), intent(in) :: mss_cnc_ocphi ( maxsnl+1:0 ) ! mass concentration of hydrophilic OC (col,lyr) [kg/kg]
   real(r8), intent(in) :: mss_cnc_ocpho ( maxsnl+1:0 ) ! mass concentration of hydrophobic OC (col,lyr) [kg/kg]
   real(r8), intent(in) :: mss_cnc_dst1  ( maxsnl+1:0 ) ! mass concentration of dust aerosol species 1 (col,lyr) [kg/kg]
   real(r8), intent(in) :: mss_cnc_dst2  ( maxsnl+1:0 ) ! mass concentration of dust aerosol species 2 (col,lyr) [kg/kg]
   real(r8), intent(in) :: mss_cnc_dst3  ( maxsnl+1:0 ) ! mass concentration of dust aerosol species 3 (col,lyr) [kg/kg]
   real(r8), intent(in) :: mss_cnc_dst4  ( maxsnl+1:0 ) ! mass concentration of dust aerosol species 4 (col,lyr) [kg/kg]

   real(r8)   , intent(in) :: dir_frac  ( numhires ) !
   real(r8)   , intent(in) :: dif_frac  ( numhires ) !

   real(r8), intent(out) :: albgrd       ( numrad )     ! ground albedo (direct)
   real(r8), intent(out) :: albgri       ( numrad )     ! ground albedo (diffuse)
   real(r8), intent(out) :: albgrd_pur   ( numrad )     ! pure snow ground albedo (direct)
   real(r8), intent(out) :: albgri_pur   ( numrad )     ! pure snow ground albedo (diffuse)
   real(r8), intent(out) :: albgrd_bc    ( numrad )     ! ground albedo without BC (direct)
   real(r8), intent(out) :: albgri_bc    ( numrad )     ! ground albedo without BC (diffuse)
   real(r8), intent(out) :: albgrd_oc    ( numrad )     ! ground albedo without OC (direct)
   real(r8), intent(out) :: albgri_oc    ( numrad )     ! ground albedo without OC (diffuse)
   real(r8), intent(out) :: albgrd_dst   ( numrad )     ! ground albedo without dust (direct)
   real(r8), intent(out) :: albgri_dst   ( numrad )     ! ground albedo without dust (diffuse)
   real(r8), intent(out) :: flx_absdv    ( maxsnl+1:1 ) ! direct flux absorption factor (col,lyr): VIS [frc]
   real(r8), intent(out) :: flx_absdn    ( maxsnl+1:1 ) ! direct flux absorption factor (col,lyr): NIR [frc]
   real(r8), intent(out) :: flx_absiv    ( maxsnl+1:1 ) ! diffuse flux absorption factor (col,lyr): VIS [frc]
   real(r8), intent(out) :: flx_absin    ( maxsnl+1:1 ) ! diffuse flux absorption factor (col,lyr): NIR [frc]

   !-----------------------------------------------------------------------
   !
   ! !LOCAL VARIABLES:

   integer  :: i            ! index for layers [idx]
   integer  :: ibnd, start_index, end_index  ! index for 5 bands [idx]
   integer  :: aer          ! index for sno_nbr_aer
   integer  :: ib           ! band index
   integer  :: ic           ! 0=unit incoming direct; 1=unit incoming diffuse
   integer  :: flg_slr      ! flag for SNICAR (=1 IF direct, =2 IF diffuse)
   integer  :: flg_snw_ice  ! flag for SNICAR (=1 when called from ELM, =2 when called from sea-ice)

   real(r8) :: mss_cnc_aer_in_frc_pur (maxsnl+1:0,sno_nbr_aer) ! mass concentration of aerosol species for forcing calculation (zero) (col,lyr,aer) [kg kg-1]
   real(r8) :: mss_cnc_aer_in_frc_bc  (maxsnl+1:0,sno_nbr_aer) ! mass concentration of aerosol species for BC forcing (col,lyr,aer) [kg kg-1]
   real(r8) :: mss_cnc_aer_in_frc_oc  (maxsnl+1:0,sno_nbr_aer) ! mass concentration of aerosol species for OC forcing (col,lyr,aer) [kg kg-1]
   real(r8) :: mss_cnc_aer_in_frc_dst (maxsnl+1:0,sno_nbr_aer) ! mass concentration of aerosol species for dust forcing (col,lyr,aer) [kg kg-1]
   real(r8) :: mss_cnc_aer_in_fdb     (maxsnl+1:0,sno_nbr_aer) ! mass concentration of all aerosol species for feedback calculation (col,lyr,aer) [kg kg-1]

   real(r8) :: albsfc       (numrad)             ! albedo of surface underneath snow (col,bnd)
   real(r8) :: albsnd       (numrad)             ! snow albedo (direct)
   real(r8) :: albsni       (numrad)             ! snow albedo (diffuse)
   real(r8) :: albsnd_pur   (numrad)             ! direct pure snow albedo (radiative forcing)
   real(r8) :: albsni_pur   (numrad)             ! diffuse pure snow albedo (radiative forcing)
   real(r8) :: albsnd_bc    (numrad)             ! direct snow albedo without BC (radiative forcing)
   real(r8) :: albsni_bc    (numrad)             ! diffuse snow albedo without BC (radiative forcing)
   real(r8) :: albsnd_oc    (numrad)             ! direct snow albedo without OC (radiative forcing)
   real(r8) :: albsni_oc    (numrad)             ! diffuse snow albedo without OC (radiative forcing)
   real(r8) :: albsnd_dst   (numrad)             ! direct snow albedo without dust (radiative forcing)
   real(r8) :: albsni_dst   (numrad)             ! diffuse snow albedo without dust (radiative forcing)
   real(r8) :: flx_absd_snw (maxsnl+1:1,numrad)  ! flux absorption factor for just snow (direct) [frc]
   real(r8) :: flx_absi_snw (maxsnl+1:1,numrad)  ! flux absorption factor for just snow (diffuse) [frc]
   real(r8) :: foo_snw      (maxsnl+1:1,numrad)  ! dummy array for forcing calls

   integer  :: snw_rds_in   (maxsnl+1:0)         ! snow grain size sent to SNICAR (col,lyr) [microns]

   integer , parameter :: nband =numrad          ! number of solar radiation waveband classes
   INTEGER, PARAMETER, DIMENSION(6) :: band_index = (/ &
      1, 30, 60, 80, 110, 212   &! 400, 700, 1000, 1200, 1500, 2500 nm
   /)

   !-----------------------------------------------------------------------

      ! Initialize output because solar radiation only done IF coszen > 0

      DO ib = 1, numrad
         albgrd(ib)     = 0._r8
         albgri(ib)     = 0._r8
         albgrd_pur(ib) = 0._r8
         albgri_pur(ib) = 0._r8
         albgrd_bc(ib)  = 0._r8
         albgri_bc(ib)  = 0._r8
         albgrd_oc(ib)  = 0._r8
         albgri_oc(ib)  = 0._r8
         albgrd_dst(ib) = 0._r8
         albgri_dst(ib) = 0._r8
         DO i=maxsnl+1,1,1
            flx_absdv(i) = 0._r8
            flx_absdn(i) = 0._r8
            flx_absiv(i) = 0._r8
            flx_absin(i) = 0._r8
         ENDDO
      ENDDO  ! END of numrad loop

      ! set variables to pass to SNICAR.

      flg_snw_ice = 1

      do ibnd = 1, 5
         start_index = band_index(ibnd)
         end_index   = band_index(ibnd+1) - 1

         albsfc(ibnd) = SUM(albsoi  (start_index:end_index)  *&
                            dif_frac(start_index:end_index)) /&
                        SUM(dif_frac(start_index:end_index))
      end do

      snw_rds_in(:) = nint(snw_rds(:))

      ! zero aerosol input arrays
      DO aer = 1, sno_nbr_aer
         DO i = maxsnl+1, 0
            mss_cnc_aer_in_frc_pur(i,aer) = 0._r8
            mss_cnc_aer_in_frc_bc(i,aer)  = 0._r8
            mss_cnc_aer_in_frc_oc(i,aer)  = 0._r8
            mss_cnc_aer_in_frc_dst(i,aer) = 0._r8
            mss_cnc_aer_in_fdb(i,aer)     = 0._r8
         ENDDO
      ENDDO

      ! If radiative forcing is being calculated, first estimate clean-snow albedo

      IF (use_snicar_frc) THEN

         ! 1. PURE SNOW ALBEDO CALCULATIONS
         flg_slr = 1  ! direct-beam
         IF (use_snicar_ad) THEN
             CALL SNICAR_AD_RT(flg_snw_ice, &
                            flg_slr, &
                            coszen_col, &
                            snl, &
                            h2osno, &
                            frac_sno, &
                            h2osno_liq(:), &
                            h2osno_ice(:), &
                            snw_rds_in(:), &
                            mss_cnc_aer_in_frc_pur(:, :), &
                            albsfc(:), &
                            albsnd_pur(:), &
                            foo_snw(:, :) )
         ELSE
             CALL SNICAR_RT(flg_snw_ice, &
                            flg_slr, &
                            coszen_col, &
                            snl, &
                            h2osno, &
                            frac_sno, &
                            h2osno_liq(:), &
                            h2osno_ice(:), &
                            snw_rds_in(:), &
                            mss_cnc_aer_in_frc_pur(:, :), &
                            albsfc(:), &
                            albsnd_pur(:), &
                            foo_snw(:, :) )
         ENDIF ! END IF use_snicar_ad

         flg_slr = 2  ! diffuse
         IF (use_snicar_ad) THEN
             CALL SNICAR_AD_RT(flg_snw_ice, &
                            flg_slr, &
                            coszen_col, &
                            snl, &
                            h2osno, &
                            frac_sno, &
                            h2osno_liq(:), &
                            h2osno_ice(:), &
                            snw_rds_in(:), &
                            mss_cnc_aer_in_frc_pur(:, :), &
                            albsfc(:), &
                            albsni_pur(:), &
                            foo_snw(:, :) )
         ELSE
             CALL SNICAR_RT(flg_snw_ice, &
                            flg_slr, &
                            coszen_col, &
                            snl, &
                            h2osno, &
                            frac_sno, &
                            h2osno_liq(:), &
                            h2osno_ice(:), &
                            snw_rds_in(:), &
                            mss_cnc_aer_in_frc_pur(:, :), &
                            albsfc(:), &
                            albsni_pur(:), &
                            foo_snw(:, :) )
         ENDIF ! END IF use_snicar_ad

         ! 2. BC input array:
         !  set dust and (optionally) OC concentrations, so BC_FRC=[(BC+OC+dust)-(OC+dust)]
         IF (DO_SNO_OC) THEN
            mss_cnc_aer_in_frc_bc(:,3) = mss_cnc_ocphi(:)
            mss_cnc_aer_in_frc_bc(:,4) = mss_cnc_ocpho(:)
         ENDIF
         mss_cnc_aer_in_frc_bc(:,5) = mss_cnc_dst1(:)
         mss_cnc_aer_in_frc_bc(:,6) = mss_cnc_dst2(:)
         mss_cnc_aer_in_frc_bc(:,7) = mss_cnc_dst3(:)
         mss_cnc_aer_in_frc_bc(:,8) = mss_cnc_dst4(:)

         ! BC FORCING CALCULATIONS
         flg_slr = 1  ! direct-beam
         IF (use_snicar_ad) THEN
             CALL SNICAR_AD_RT(flg_snw_ice, &
                               flg_slr, &
                               coszen_col, &
                               snl, &
                               h2osno, &
                               frac_sno, &
                               h2osno_liq(:), &
                               h2osno_ice(:), &
                               snw_rds_in(:), &
                               mss_cnc_aer_in_frc_bc(:, :), &
                               albsfc(:), &
                               albsnd_bc(:), &
                               foo_snw(:, :) )
         ELSE
             CALL SNICAR_RT   (flg_snw_ice, &
                               flg_slr, &
                               coszen_col, &
                               snl, &
                               h2osno, &
                               frac_sno, &
                               h2osno_liq(:), &
                               h2osno_ice(:), &
                               snw_rds_in(:), &
                               mss_cnc_aer_in_frc_bc(:, :), &
                               albsfc(:), &
                               albsnd_bc(:), &
                               foo_snw(:, :) )
         ENDIF ! END IF use_snicar_ad

         flg_slr = 2  ! diffuse
         IF (use_snicar_ad) THEN
             CALL SNICAR_AD_RT(flg_snw_ice, &
                               flg_slr, &
                               coszen_col, &
                               snl, &
                               h2osno, &
                               frac_sno, &
                               h2osno_liq(:), &
                               h2osno_ice(:), &
                               snw_rds_in(:), &
                               mss_cnc_aer_in_frc_bc(:, :), &
                               albsfc(:), &
                               albsni_bc(:), &
                               foo_snw(:, :) )
         ELSE
             CALL SNICAR_RT   (flg_snw_ice, &
                               flg_slr, &
                               coszen_col, &
                               snl, &
                               h2osno, &
                               frac_sno, &
                               h2osno_liq(:), &
                               h2osno_ice(:), &
                               snw_rds_in(:), &
                               mss_cnc_aer_in_frc_bc(:, :), &
                               albsfc(:), &
                               albsni_bc(:), &
                               foo_snw(:, :) )
         ENDIF ! END IF use_snicar_ad

         ! 3. OC input array:
         !  set BC and dust concentrations, so OC_FRC=[(BC+OC+dust)-(BC+dust)]
         IF (DO_SNO_OC) THEN
            mss_cnc_aer_in_frc_oc(:,1) = mss_cnc_bcphi(:)
            mss_cnc_aer_in_frc_oc(:,2) = mss_cnc_bcpho(:)

            mss_cnc_aer_in_frc_oc(:,5) = mss_cnc_dst1(:)
            mss_cnc_aer_in_frc_oc(:,6) = mss_cnc_dst2(:)
            mss_cnc_aer_in_frc_oc(:,7) = mss_cnc_dst3(:)
            mss_cnc_aer_in_frc_oc(:,8) = mss_cnc_dst4(:)

         ! OC FORCING CALCULATIONS
            flg_slr = 1  ! direct-beam
            IF (use_snicar_ad) THEN
                CALL SNICAR_AD_RT(flg_snw_ice, &
                               flg_slr, &
                               coszen_col, &
                               snl, &
                               h2osno, &
                               frac_sno, &
                               h2osno_liq(:), &
                               h2osno_ice(:), &
                               snw_rds_in(:), &
                               mss_cnc_aer_in_frc_oc(:, :), &
                               albsfc(:), &
                               albsnd_oc(:), &
                               foo_snw(:, :) )
            ELSE
                CALL SNICAR_RT(flg_snw_ice, &
                               flg_slr, &
                               coszen_col, &
                               snl, &
                               h2osno, &
                               frac_sno, &
                               h2osno_liq(:), &
                               h2osno_ice(:), &
                               snw_rds_in(:), &
                               mss_cnc_aer_in_frc_oc(:, :), &
                               albsfc(:), &
                               albsnd_oc(:), &
                               foo_snw(:, :) )
            ENDIF ! END IF use_snicar_ad

            flg_slr = 2  ! diffuse
            IF (use_snicar_ad) THEN
                CALL SNICAR_AD_RT(flg_snw_ice, &
                               flg_slr, &
                               coszen_col, &
                               snl, &
                               h2osno, &
                               frac_sno, &
                               h2osno_liq(:), &
                               h2osno_ice(:), &
                               snw_rds_in(:), &
                               mss_cnc_aer_in_frc_oc(:, :), &
                               albsfc(:), &
                               albsni_oc(:), &
                               foo_snw(:, :) )
            ELSE
                CALL SNICAR_RT(flg_snw_ice, &
                               flg_slr, &
                               coszen_col, &
                               snl, &
                               h2osno, &
                               frac_sno, &
                               h2osno_liq(:), &
                               h2osno_ice(:), &
                               snw_rds_in(:), &
                               mss_cnc_aer_in_frc_oc(:, :), &
                               albsfc(:), &
                               albsni_oc(:), &
                               foo_snw(:, :) )
            ENDIF ! END IF use_snicar_ad
         ENDIF  ! END IF (DO_SNO_OC)

         ! 4. DUST FORCING CALCULATIONS
         ! DUST input array:
         ! set BC and OC concentrations, so DST_FRC=[(BC+OC+dust)-(BC+OC)]
         mss_cnc_aer_in_frc_dst(:,1) = mss_cnc_bcphi(:)
         mss_cnc_aer_in_frc_dst(:,2) = mss_cnc_bcpho(:)

         IF (DO_SNO_OC) THEN
             mss_cnc_aer_in_frc_dst(:,3) = mss_cnc_ocphi(:)
             mss_cnc_aer_in_frc_dst(:,4) = mss_cnc_ocpho(:)
         ENDIF

         flg_slr = 1  ! direct-beam
         IF (use_snicar_ad) THEN
             CALL SNICAR_AD_RT(flg_snw_ice, &
                            flg_slr, &
                            coszen_col, &
                            snl, &
                            h2osno, &
                            frac_sno, &
                            h2osno_liq(:), &
                            h2osno_ice(:), &
                            snw_rds_in(:), &
                            mss_cnc_aer_in_frc_dst(:, :), &
                            albsfc(:), &
                            albsnd_dst(:), &
                            foo_snw(:, :) )
         ELSE
             CALL SNICAR_RT(flg_snw_ice, &
                            flg_slr, &
                            coszen_col, &
                            snl, &
                            h2osno, &
                            frac_sno, &
                            h2osno_liq(:), &
                            h2osno_ice(:), &
                            snw_rds_in(:), &
                            mss_cnc_aer_in_frc_dst(:, :), &
                            albsfc(:), &
                            albsnd_dst(:), &
                            foo_snw(:, :) )
         ENDIF ! END IF use_snicar_ad

         flg_slr = 2  ! diffuse
         IF (use_snicar_ad) THEN
             CALL SNICAR_AD_RT(flg_snw_ice, &
                            flg_slr, &
                            coszen_col, &
                            snl, &
                            h2osno, &
                            frac_sno, &
                            h2osno_liq(:), &
                            h2osno_ice(:), &
                            snw_rds_in(:), &
                            mss_cnc_aer_in_frc_dst(:, :), &
                            albsfc(:), &
                            albsni_dst(:), &
                            foo_snw(:, :) )
         ELSE
             CALL SNICAR_RT(flg_snw_ice, &
                            flg_slr, &
                            coszen_col, &
                            snl, &
                            h2osno, &
                            frac_sno, &
                            h2osno_liq(:), &
                            h2osno_ice(:), &
                            snw_rds_in(:), &
                            mss_cnc_aer_in_frc_dst(:, :), &
                            albsfc(:), &
                            albsni_dst(:), &
                            foo_snw(:, :)  )
         ENDIF ! END IF use_snicar_ad

      ENDIF !END IF use_snicar_frc


      ! --------------------------------------------
      ! CLIMATE FEEDBACK CALCULATIONS, ALL AEROSOLS:
      ! --------------------------------------------
      ! Set aerosol input arrays
      ! feedback input arrays have been zeroed
      ! set soot and dust aerosol concentrations:
      IF (DO_SNO_AER) THEN
         mss_cnc_aer_in_fdb(:,1) = mss_cnc_bcphi(:)
         mss_cnc_aer_in_fdb(:,2) = mss_cnc_bcpho(:)

         ! DO_SNO_OC is set in SNICAR_varpar. Default case is to ignore OC concentrations because:
         !  1) Knowledge of their optical properties is primitive
         !  2) When 'water-soluble' OPAC optical properties are applied to OC in snow,
         !     it has a negligible darkening effect.
         IF (DO_SNO_OC) THEN
            mss_cnc_aer_in_fdb(:,3) = mss_cnc_ocphi(:)
            mss_cnc_aer_in_fdb(:,4) = mss_cnc_ocpho(:)
         ENDIF

         mss_cnc_aer_in_fdb(:,5) = mss_cnc_dst1(:)
         mss_cnc_aer_in_fdb(:,6) = mss_cnc_dst2(:)
         mss_cnc_aer_in_fdb(:,7) = mss_cnc_dst3(:)
         mss_cnc_aer_in_fdb(:,8) = mss_cnc_dst4(:)
      ENDIF

      flg_slr = 1  ! direct-beam
      IF (use_snicar_ad) THEN
         CALL SNICAR_AD_RT(flg_snw_ice, &
                           flg_slr, &
                           coszen_col, &
                           snl, &
                           h2osno, &
                           frac_sno, &
                           h2osno_liq(:), &
                           h2osno_ice(:), &
                           snw_rds_in(:), &
                           mss_cnc_aer_in_fdb(:, :), &
                           albsfc(:), &
                           albsnd(:), &
                           flx_absd_snw(:, :) )
      ELSE
         CALL SNICAR_RT   (flg_snw_ice, &
                           flg_slr, &
                           coszen_col, &
                           snl, &
                           h2osno, &
                           frac_sno, &
                           h2osno_liq(:), &
                           h2osno_ice(:), &
                           snw_rds_in(:), &
                           mss_cnc_aer_in_fdb(:, :), &
                           albsfc(:), &
                           albsnd(:), &
                           flx_absd_snw(:, :) )
      ENDIF ! END IF use_snicar_ad

      flg_slr = 2  ! diffuse
      IF (use_snicar_ad) THEN
         CALL SNICAR_AD_RT(flg_snw_ice, &
                           flg_slr, &
                           coszen_col, &
                           snl, &
                           h2osno, &
                           frac_sno, &
                           h2osno_liq(:), &
                           h2osno_ice(:), &
                           snw_rds_in(:), &
                           mss_cnc_aer_in_fdb(:, :), &
                           albsfc(:), &
                           albsni(:), &
                           flx_absi_snw(:, :) )
      ELSE
         CALL SNICAR_RT   (flg_snw_ice, &
                           flg_slr, &
                           coszen_col, &
                           snl, &
                           h2osno, &
                           frac_sno, &
                           h2osno_liq(:), &
                           h2osno_ice(:), &
                           snw_rds_in(:), &
                           mss_cnc_aer_in_fdb(:, :), &
                           albsfc(:), &
                           albsni(:), &
                           flx_absi_snw(:, :) )
      ENDIF ! END IF use_snicar_ad


      ! ground albedos and snow-fraction weighting of snow absorption factors
      DO ib = 1, nband
         IF (coszen_col > 0._r8) THEN
            ! ground albedo was originally computed in SoilAlbedo, but is now computed here
            ! because the order of SoilAlbedo and SNICAR_RT/SNICAR_AD_RT was switched for SNICAR/SNICAR_AD_RT.
            ! 09/01/2023, yuan: change to only snow albedo, the same below
            !albgrd(ib) = albsod(ib)*(1._r8-frac_sno) + albsnd(ib)*frac_sno
            !albgri(ib) = albsoi(ib)*(1._r8-frac_sno) + albsni(ib)*frac_sno
            albgrd(ib) = albsnd(ib)
            albgri(ib) = albsni(ib)

            ! albedos for radiative forcing calculations:
            IF (use_snicar_frc) THEN
               ! pure snow albedo for all-aerosol radiative forcing
               !albgrd_pur(ib) = albsod(ib)*(1.-frac_sno) + albsnd_pur(ib)*frac_sno
               !albgri_pur(ib) = albsoi(ib)*(1.-frac_sno) + albsni_pur(ib)*frac_sno
               albgrd_pur(ib) = albsnd_pur(ib)
               albgri_pur(ib) = albsni_pur(ib)

               ! BC forcing albedo
               !albgrd_bc(ib) = albsod(ib)*(1.-frac_sno) + albsnd_bc(ib)*frac_sno
               !albgri_bc(ib) = albsoi(ib)*(1.-frac_sno) + albsni_bc(ib)*frac_sno
               albgrd_bc(ib) = albsnd_bc(ib)
               albgri_bc(ib) = albsni_bc(ib)

               IF (DO_SNO_OC) THEN
                  ! OC forcing albedo
                  !albgrd_oc(ib) = albsod(ib)*(1.-frac_sno) + albsnd_oc(ib)*frac_sno
                  !albgri_oc(ib) = albsoi(ib)*(1.-frac_sno) + albsni_oc(ib)*frac_sno
                  albgrd_oc(ib) = albsnd_oc(ib)
                  albgri_oc(ib) = albsni_oc(ib)
               ENDIF

               ! dust forcing albedo
               !albgrd_dst(ib) = albsod(ib)*(1.-frac_sno) + albsnd_dst(ib)*frac_sno
               !albgri_dst(ib) = albsoi(ib)*(1.-frac_sno) + albsni_dst(ib)*frac_sno
               albgrd_dst(ib) = albsnd_dst(ib)
               albgri_dst(ib) = albsni_dst(ib)
            ENDIF

            ! also in this loop (but optionally in a different loop for vectorized code)
            !  weight snow layer radiative absorption factors based on snow fraction and soil albedo
            !  (NEEDED FOR ENERGY CONSERVATION)
            DO i = maxsnl+1,1,1
               IF (subgridflag == 0 ) THEN
                  IF (ib == 1) THEN
                     flx_absdv(i) = flx_absd_snw(i,ib)*frac_sno + &
                          ((1.-frac_sno)*(1-albsod(ib))*(flx_absd_snw(i,ib)/(1.-albsnd(ib))))
                     flx_absiv(i) = flx_absi_snw(i,ib)*frac_sno + &
                          ((1.-frac_sno)*(1-albsoi(ib))*(flx_absi_snw(i,ib)/(1.-albsni(ib))))
                  elseif (ib == 2) THEN
                     flx_absdn(i) = flx_absd_snw(i,ib)*frac_sno + &
                          ((1.-frac_sno)*(1-albsod(ib))*(flx_absd_snw(i,ib)/(1.-albsnd(ib))))
                     flx_absin(i) = flx_absi_snw(i,ib)*frac_sno + &
                          ((1.-frac_sno)*(1-albsoi(ib))*(flx_absi_snw(i,ib)/(1.-albsni(ib))))
                  ENDIF
               ELSE
                  IF (ib == 1) THEN
                     flx_absdv(i) = flx_absd_snw(i,ib)!*(1.-albsnd(ib))
                     flx_absiv(i) = flx_absi_snw(i,ib)!*(1.-albsni(ib))
                  elseif (ib == 2) THEN
                     flx_absdn(i) = flx_absd_snw(i,ib)!*(1.-albsnd(ib))
                     flx_absin(i) = flx_absi_snw(i,ib)!*(1.-albsni(ib))
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
      ENDDO

   END SUBROUTINE SnowAlbedo

   SUBROUTINE albocean (oro, scv, coszrs, alb)

!-----------------------------------------------------------------------
!
! Compute surface albedos
!
! Computes surface albedos for direct/diffuse incident radiation for
! two spectral intervals:
!   s = 0.2-0.7 micro-meters
!   l = 0.7-5.0 micro-meters
!
! Albedos specified as follows:
!
! Ocean           Uses solar zenith angle to compute albedo for direct
!                 radiation; diffuse radiation values constant; albedo
!                 independent of spectral interval and other physical
!                 factors such as ocean surface wind speed.
!
! Ocean with      Surface albs specified; combined with overlying snow
!   sea ice
!
! For more details , see Briegleb, Bruce P., 1992: Delta-Eddington
! Approximation for Solar Radiation in the NCAR Community Climate Model,
! Journal of Geophysical Research, Vol 97, D7, pp7603-7612).
!
! yongjiu dai and xin-zhong liang (08/01/2001)
!-----------------------------------------------------------------------

   USE MOD_Precision
   IMPLICIT NONE

!------------------------------Arguments--------------------------------

   real(r8), intent(in) :: oro       ! /ocean(0)/seaice(2) flag
   real(r8), intent(in) :: scv       ! snow water equivalent) [mm]
   real(r8), intent(in) :: coszrs    ! Cosine solar zenith angle

   real(r8), intent(out) :: alb(2,2) ! srf alb for direct (diffuse) rad 0.2-0.7 micro-ms
                                     ! Srf alb for direct (diffuse) rad 0.7-5.0 micro-ms

!---------------------------Local variables-----------------------------

   real(r8) frsnow       ! horizontal fraction of snow cover
   real(r8) snwhgt       ! physical snow height
   real(r8) rghsnw       ! roughness for horizontal snow cover fractn

   real(r8) sasdir       ! snow alb for direct rad  0.2-0.7 micro-ms
   real(r8) saldir       ! snow alb for direct rad  0.7-5.0 micro-ms
   real(r8) sasdif       ! snow alb for diffuse rad  0.2-0.7 micro-ms
   real(r8) saldif       ! snow alb for diffuse rad  0.7-5.0 micro-ms

   real(r8), parameter :: asices = 0.70 ! sea ice albedo for 0.2-0.7 micro-meters [-]
   real(r8), parameter :: asicel = 0.50 ! sea ice albedo for 0.7-5.0 micro-meters [-]
   real(r8), parameter :: asnows = 0.95 ! snow    albedo for 0.2-0.7 micro-meters [-]
   real(r8), parameter :: asnowl = 0.70 ! snow    albedo for 0.7-5.0 micro-meters

!-----------------------------------------------------------------------
! initialize all ocean/sea ice surface albedos to zero

      alb(:,:) = 0.
      IF(coszrs<=0.0) RETURN

      IF(nint(oro)==2)THEN
         alb(1,1) = asices
         alb(2,1) = asicel
         alb(1,2) = alb(1,1)
         alb(2,2) = alb(2,1)
         sasdif = asnows
         saldif = asnowl

         IF(scv>0.)THEN
           IF (coszrs<0.5) THEN
           ! zenith angle regime 1 ( coszrs < 0.5 ).
           ! set direct snow albedos (limit to 0.98 max)
             sasdir = min(0.98,sasdif+(1.-sasdif)*0.5*(3./(1.+4.*coszrs)-1.))
             saldir = min(0.98,saldif+(1.-saldif)*0.5*(3./(1.+4.*coszrs)-1.))
           ELSE
           ! zenith angle regime 2 ( coszrs >= 0.5 )
             sasdir = asnows
             saldir = asnowl
           ENDIF

         ! compute both diffuse and direct total albedos
           snwhgt = 20.*scv / 1000.
           rghsnw = 0.25
           frsnow = snwhgt/(rghsnw+snwhgt)
           alb(1,1) = alb(1,1)*(1.-frsnow) + sasdir*frsnow
           alb(2,1) = alb(2,1)*(1.-frsnow) + saldir*frsnow
           alb(1,2) = alb(1,2)*(1.-frsnow) + sasdif*frsnow
           alb(2,2) = alb(2,2)*(1.-frsnow) + saldif*frsnow
         ENDIF
      ENDIF

! ice-free ocean albedos function of solar zenith angle only, and
! independent of spectral interval:

      IF(nint(oro)==0)THEN
         alb(2,1) = .026/(coszrs**1.7+.065) &
                  + .15*(coszrs-0.1)*(coszrs-0.5)*(coszrs-1.)
         alb(1,1) = alb(2,1)
         alb(1,2) = 0.06
         alb(2,2) = 0.06
      ENDIF

   END SUBROUTINE albocean


   pure function calculate_tav(alpha, nr) result(tav)
        real(r8), intent(in) :: alpha
        real(r8), dimension(211), intent(in) :: nr
        real(r8) :: rd, n2(211), n_p(211), nm(211), a(211), k(211), sa
        real(r8) :: b1(211), b2(211), b(211), b3(211), a3(211)
        real(r8) :: ts(211), tp1(211), tp2(211), tp3(211), tp4(211), tp5(211), tp(211)
        real(r8), dimension(211) :: tav

        rd = 3.141592653589793 / 180.0
        n2 = nr**2
        n_p = n2 + 1.0
        nm = n2 - 1.0
        a = (nr + 1.0) * (nr + 1.0) / 2.0
        k = -(n2 - 1.0) * (n2 - 1.0) / 4.0
        sa = sin(alpha * rd)

        b1 = 0.0
        if (alpha /= 90.0) then
            b1 = sqrt((sa**2 - n_p / 2.0) * (sa**2 - n_p / 2.0) + k)
        end if

        b2 = sa**2 - n_p / 2.0
        b = b1 - b2
        b3 = b**3
        a3 = a**3

        ts = (k**2 / (6.0 * b3) + k / b - b / 2.0) - (k**2 / (6.0 * a3) + k / a - a / 2.0)

        tp1 = -2.0 * n2 * (b - a) / (n_p**2)
        tp2 = -2.0 * n2 * n_p * log(b / a) / (nm**2)
        tp3 = n2 * (1.0 / b - 1.0 / a) / 2.0
        tp4 = 16.0 * n2**2 * (n2**2 + 1.0) * log((2.0 * n_p * b - nm**2) / (2.0 * n_p * a - nm**2)) / (n_p**3 * nm**2)
        tp5 = 16.0 * n2**3 * (1.0 / (2.0 * n_p * b - nm**2) - 1.0 / (2.0 * n_p * a - nm**2)) / n_p**3
        tp = tp1 + tp2 + tp3 + tp4 + tp5
        tav = (ts + tp) / (2.0 * sa**2)

    end function calculate_tav

   pure FUNCTION poisson_pmf(k, lambda) RESULT(pmf)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: k(:)
      REAL(r8), INTENT(IN) :: lambda
      REAL(r8) :: pmf(SIZE(k))
      INTEGER :: i

      DO i = 1, SIZE(k)
         pmf(i) = EXP(-lambda) * lambda**REAL(k(i), KIND=r8) / FACTORIAL_R8( k(i) )
      END DO

   END FUNCTION poisson_pmf

   pure FUNCTION FACTORIAL_R8(k) RESULT(fact)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: k
      INTEGER :: i
      REAL(r8) :: fact

      fact = 1.0_r8
      IF (k <= 0) RETURN
      DO i = 1, k
         fact = fact * REAL(i, KIND=r8)
      END DO
   END FUNCTION FACTORIAL_R8

   SUBROUTINE BSM_soil_moisture( &
      ! input
         soil_moisture, smc, albedo_dry, kw, nw, &
      ! output
         albedo_wet)

      IMPLICIT NONE

      ! Arguments
      real(r8), intent(in) :: soil_moisture
      real(r8), intent(in) :: smc
      real(r8), intent(in) :: albedo_dry(211)
      real(r8), intent(in) :: kw(211)
      real(r8), intent(in) :: nw(211)

      real(r8), intent(out) :: albedo_wet(211, 2)

      ! Parameters
      real(r8), parameter :: deleff = 0.0150 ! BSM soil model: film thickness not supplied, set to default of 0.015 m

      INTEGER, DIMENSION(7) :: k_arr = (/0, 1, 2, 3, 4, 5, 6/)
      INTEGER :: nk, i
      REAL(r8) :: mu
      REAL(r8) :: rbac(211), p(211), Rw(211)
      REAL(r8) :: fmul(7), tw(211, 7)
      REAL(r8) :: Rwet_k(211, 7)
      REAL(r8) :: kw_tmp(211, 7), numerator_tmp(211, 7), denominator_tmp(211, 7), dot_product_result(211)

      ! ===== Start of executable code =====
      nk = SIZE(k_arr)

      mu = (soil_moisture - 5.0) / smc

      IF (mu <= 0.0) THEN ! below 5 % SM -> model assumes no effect
         albedo_wet(:, 1) = albedo_dry
         albedo_wet(:, 2) = albedo_dry
      ELSE
         rbac = 1.0 - (1.0 - albedo_dry) * (albedo_dry * calculate_tav(90.0, 2.0 / nw) / calculate_tav(90.0, (/ (2.0, i = 1, 211) /)) + 1.0 - albedo_dry)

         p = 1.0 - calculate_tav(90.0, nw) / nw**2

         Rw = 1.0 - calculate_tav(40.0, nw)

         fmul = poisson_pmf(k_arr, mu)

         do i = 1, nk
            kw_tmp(:, i) = kw * k_arr(i)
            tw(:, i) = exp(-2.0 * kw_tmp(:, i) * deleff)

            numerator_tmp(:, i) = Rw + (1.0 - Rw) * (1.0 - p) * tw(:, i) * rbac
            denominator_tmp(:, i) = 1.0 - p * tw(:, i) * rbac
         end do

         ! Rwet_k = Rw + (1.0 - Rw) * (1.0 - p) * tw * rbac / (1.0 - p * tw * rbac)
         Rwet_k = numerator_tmp / denominator_tmp

         do i = 1, 211
            dot_product_result(i) = SUM(Rwet_k(i, 2:nk) * fmul(2:nk))
         end do
         albedo_wet(:, 1) = (albedo_dry * fmul(1)) + dot_product_result
         albedo_wet(:, 2) = albedo_wet(:, 1)
      END IF

  END SUBROUTINE BSM_soil_moisture

  SUBROUTINE calculate_wgt_variable( variable, frac_vis, frac_nir, variable_vis, variable_nir )

    ! Arguments
    INTEGER, PARAMETER :: num_vis = 29  ! 400 - 690 nm
    INTEGER, PARAMETER :: num_nir = 182 ! 700 - 2500 nm

    real(r8), intent(in) :: variable(211)
    real(r8), intent(in) :: frac_vis(num_vis)
    real(r8), intent(in) :: frac_nir(num_nir)

    real(r8), intent(out) :: variable_vis
    real(r8), intent(out) :: variable_nir

    real(r8), parameter :: eps = 1.0e-12_r8
    real(r8) :: sum_vis, sum_nir

    sum_vis = SUM(frac_vis)
    sum_nir = SUM(frac_nir)

   variable_vis = SUM(frac_vis * variable(1:num_vis)) / sum_vis
   variable_nir = SUM(frac_nir * variable(num_vis+1:211)) / sum_nir

  END SUBROUTINE calculate_wgt_variable

END MODULE MOD_Albedo_HiRes
#endif
! --------- EOP ----------
