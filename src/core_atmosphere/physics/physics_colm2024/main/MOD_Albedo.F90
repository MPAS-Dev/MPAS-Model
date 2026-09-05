#include <define.h>

MODULE MOD_Albedo

!-----------------------------------------------------------------------
   USE MOD_Precision
   IMPLICIT NONE
   SAVE

! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: albland
   PUBLIC :: snowage
   PUBLIC :: SnowAlbedo
   PUBLIC :: albocean

! PRIVATE MEMBER FUNCTIONS:
   PRIVATE :: twostream
#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
   PRIVATE :: twostream_mod
   PRIVATE :: twostream_wrap
#endif


!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------

   SUBROUTINE albland (ipatch,patchtype,deltim,&
                      soil_s_v_alb,soil_d_v_alb,soil_s_n_alb,soil_d_n_alb,&
                      chil,rho,tau,fveg,green,lai,sai,fwet_snow,coszen,&
                      wt,fsno,scv,scvold,sag,ssw,pg_snow,forc_t,t_grnd,t_soisno,dz_soisno,&
                      snl,wliq_soisno,wice_soisno,snw_rds,snofrz,&
                      mss_bcpho,mss_bcphi,mss_ocpho,mss_ocphi,&
                      mss_dst1,mss_dst2,mss_dst3,mss_dst4,&
                      alb,ssun,ssha,ssoi,ssno,ssno_lyr,thermk,extkb,extkd)

!=======================================================================
!  Calculates fragmented albedos (direct and diffuse) in
!  wavelength regions split at 0.7um.
!
!  (1) soil albedos: as in BATS formulations, which are the function of
!      soil color and moisture in the surface soil layer
!  (2) snow albedos: as in BATS formulations, which are inferred from
!      the calculations of Wiscombe and Warren (1980) and the snow model
!      and data of Anderson(1976), and the function of snow age, grain
!      size, solar zenith angle, pollution, the amount of the fresh snow
!  (3) canopy albedo: two-stream approximation model
!  (4) glacier albedos: as in BATS, which are set to constants (0.8 for
!      visible beam, 0.55 for near-infrared)
!  (5) lake and wetland albedos: as in BATS, which depend on cosine solar
!      zenith angle, based on data in Henderson-Sellers (1986). The
!      frozen lake and wetland albedos are set to constants (0.6 for
!      visible beam, 0.4 for near-infrared)
!  (6) over the snow covered tile, the surface albedo is estimated by a
!      linear combination of albedos for snow, canopy and bare soil (or
!      lake, wetland, glacier).
!
!  Original author: Yongjiu Dai, 09/15/1999; 08/30/2002, 03/2014
!
! !REVISIONS:
!  12/2019, Hua Yuan: added a wrap FUNCTION for PFT calculation, details
!           see twostream_wrap() added a wrap FUNCTION for PC (3D)
!           calculation, details see ThreeDCanopy_wrap()
!
!  03/2020, Hua Yuan: added an improved two-stream model, details see
!           twostream_mod()
!
!  08/2020, Hua Yuan: account for stem optical property effects in
!           twostream model
!
!  01/2023, Hua Yuan: CALL SNICAR model to calculate snow
!           albedo&absorption, added SNICAR related variables
!
!  04/2024, Hua Yuan: add option to account for vegetation snow process
!
!=======================================================================

   USE MOD_Precision
   USE MOD_Vars_Global
   USE MOD_Const_Physical, only: tfrz
   USE MOD_Namelist, only: DEF_USE_SNICAR
   USE MOD_Vars_TimeInvariants, only: patchclass
#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
   USE MOD_LandPFT, only: patch_pft_s, patch_pft_e
   USE MOD_Vars_PFTimeInvariants
   USE MOD_Vars_PFTimeVariables
#endif
   USE MOD_Aerosol, only: AerosolMasses
   USE MOD_SnowSnicar, only: SnowAge_grain
#ifdef LULC_IGBP_PC
   USE MOD_3DCanopyRadiation, only: ThreeDCanopy_wrap
#endif

   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
! ground cover index
   integer, intent(in) :: &
        ipatch,        &! patch index
        patchtype       ! land patch type (0=soil, 1=urban or built-up, 2=wetland,
                        ! 3=land ice, 4=water body)
   integer, intent(in) :: &
        snl             ! number of snow layers

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
        t_grnd          ! ground surface temperature [K]

   real(r8), intent(in) :: &
        wliq_soisno  ( maxsnl+1:0 ), &! liquid water (kg/m2)
        wice_soisno  ( maxsnl+1:0 ), &! ice lens (kg/m2)
        snofrz       ( maxsnl+1:0 ), &! snow freezing rate (col,lyr) [kg m-2 s-1]
        t_soisno     ( maxsnl+1:1 ), &! soil + snow layer temperature [K]
        dz_soisno    ( maxsnl+1:1 )   ! layer thickness (m)

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
        ssoi(2,2),     &! ground soil absorption [-]
        ssno(2,2),     &! ground snow absorption [-]
        ssno_lyr(2,2,maxsnl+1:1) ! ground snow layer absorption, by SNICAR [-]

!-------------------------- Local Variables ----------------------------

   real(r8) :: &!
      age,             &! factor to reduce visible snow alb due to snow age [-]
      albg0,           &! temporary variable [-]
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

   integer ps, pe
   logical do_capsnow      !true => DO snow capping
   logical use_snicar_frc  !true: IF radiative forcing is calculated,
                           !first estimate clean-snow albedo
   logical use_snicar_ad   !true: use SNICAR_AD_RT, false: use SNICAR_RT

   real(r8) snwcp_ice                     !excess precipitation due to snow capping [kg m-2 s-1]
   real(r8) mss_cnc_bcphi ( maxsnl+1:0 )  !mass concentration of hydrophilic BC [kg/kg]
   real(r8) mss_cnc_bcpho ( maxsnl+1:0 )  !mass concentration of hydrophobic BC [kg/kg]
   real(r8) mss_cnc_ocphi ( maxsnl+1:0 )  !mass concentration of hydrophilic OC [kg/kg]
   real(r8) mss_cnc_ocpho ( maxsnl+1:0 )  !mass concentration of hydrophobic OC [kg/kg]
   real(r8) mss_cnc_dst1  ( maxsnl+1:0 )  !mass concentration of dust aerosol species 1 [kg/kg]
   real(r8) mss_cnc_dst2  ( maxsnl+1:0 )  !mass concentration of dust aerosol species 2 [kg/kg]
   real(r8) mss_cnc_dst3  ( maxsnl+1:0 )  !mass concentration of dust aerosol species 3 [kg/kg]
   real(r8) mss_cnc_dst4  ( maxsnl+1:0 )  !mass concentration of dust aerosol species 4 [kg/kg]

! ----------------------------------------------------------------------
! 1. Initial set
! ----------------------------------------------------------------------

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
      tran(:,1) = 0.       !incident direct  radiation diffuse transmittance
      tran(:,2) = 1.       !incident diffuse radiation diffuse transmittance
      tran(:,3) = 1.       !incident direct  radiation direct  transmittance

      ! 07/06/2023, yuan: use the values of previous timestep
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
! wet-snow metamorphism in respect of liquid-water content, Ann. Glacial.

      CALL SnowAge_grain(  deltim ,snl    ,dz_soisno(:1)  ,&
           pg_snow        ,snwcp_ice      ,snofrz         ,&

           do_capsnow     ,fsno           ,scv            ,&
           wliq_soisno(:0),wice_soisno(:0),t_soisno(:1)   ,&
           t_grnd         ,forc_t         ,snw_rds         )
ENDIF
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
      IF (patchtype <= 2) THEN           !soil, urban and wetland
         alb_s_inc = max(0.11-0.40*ssw, 0.)
         albg(1,1) = min(soil_s_v_alb + alb_s_inc, soil_d_v_alb)
         albg(2,1) = min(soil_s_n_alb + alb_s_inc, soil_d_n_alb)
         albg(:,2) = albg(:,1)           !diffused albedos setting

! 2.2 albedos for permanent ice sheet.
      ELSEIF (patchtype == 3) THEN       !permanent ice sheet
         albg(1,:) = 0.8
         albg(2,:) = 0.55

! 2.3 albedo for inland water
      ELSEIF (patchtype >= 4) THEN
         albg0 = 0.05/(czen+0.15)
         albg(:,1) = albg0
         albg(:,2) = 0.1                 !Subin (2012)

         IF(t_grnd < tfrz)THEN           !frozen lake and wetland
            albg(1,:) = 0.6
            albg(2,:) = 0.4
         ENDIF
      ENDIF

      ! SAVE soil ground albedo
      albsoi(:,:) = albg(:,:)

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
            sl   = 2.0    !sl helps control albedo zenith dependence

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
            use_snicar_frc = .false.  !  true: IF radiative forcing is being calculated,
                                      !  first estimate clean-snow albedo
            use_snicar_ad  = .true.   !  use true: use SNICAR_AD_RT, false: use SNICAR_RT

            CALL SnowAlbedo(     use_snicar_frc ,use_snicar_ad  ,czen           ,&
                 albg(:,1)      ,albg(:,2)      ,snl            ,fsno           ,&
                 scv            ,wliq_soisno    ,wice_soisno    ,snw_rds        ,&

                 mss_cnc_bcphi  ,mss_cnc_bcpho  ,mss_cnc_ocphi  ,mss_cnc_ocpho  ,&
                 mss_cnc_dst1   ,mss_cnc_dst2   ,mss_cnc_dst3   ,mss_cnc_dst4   ,&

                 albsno    (:,1),albsno    (:,2),albsno_pur(:,1),albsno_pur(:,2),&
                 albsno_bc (:,1),albsno_bc (:,2),albsno_oc (:,1),albsno_oc (:,2),&
                 albsno_dst(:,1),albsno_dst(:,2),ssno_lyr(1,1,:),ssno_lyr(2,1,:),&
                 ssno_lyr(1,2,:),ssno_lyr(2,2,:))

            ! IF no snow layer exist
            IF (snl == 0) THEN
               ssno_lyr(:,:,1) = ssno_lyr(:,:,1) + ssno_lyr(:,:,0)
               ssno_lyr(:,:,0) = 0.
            ENDIF
         ENDIF
      ENDIF

! 3.1 correction due to snow cover
      albg(:,:) = (1.-fsno)*albg(:,:) + fsno*albsno(:,:)
      alb (:,:) = albg(:,:)

! ----------------------------------------------------------------------
! 4. canopy albedos: two stream approximation or 3D canopy radiation transfer
! ----------------------------------------------------------------------
      IF (lai+sai > 1e-6 .and. patchtype < 3) THEN

         ! initialization
         albv(:,:) = albg(:,:)

         IF (patchtype == 0) THEN  !soil patches

#if (defined LULC_USGS || defined LULC_IGBP)
            CALL twostream (chil,rho,tau,green,lai,sai,fwet_snow,&
                            czen,albg,albv,tran,thermk,extkb,extkd,ssun,ssha)

            ! 08/31/2023, yuan: to be consistent with PFT and PC
            alb(:,:) = albv(:,:)
#endif
         ELSE  !other patchtypes (/=0)
            CALL twostream (chil,rho,tau,green,lai,sai,fwet_snow,&
                            czen,albg,albv,tran,thermk,extkb,extkd,ssun,ssha)

            ! 08/31/2023, yuan: to be consistent with PFT and PC
            alb(:,:) = albv(:,:)

         ENDIF
      ENDIF


      IF (patchtype == 0) THEN
#ifdef LULC_IGBP_PFT
         CALL twostream_wrap (ipatch, czen, albg, albv, tran, ssun, ssha)
         alb(:,:) = albv(:,:)
#endif

#ifdef LULC_IGBP_PC
         ! Only process nature PFTs using 3D model if set DEF_PC_CROP_SPLIT true
         CALL ThreeDCanopy_wrap (ipatch, czen, albg, albv, tran, ssun, ssha)

         ! Process crop PFTs using 1D model if set DEF_PC_CROP_SPLIT true
         CALL twostream_wrap (ipatch, czen, albg, albv, tran, ssun, ssha)

         alb(:,:) = albv(:,:)
#endif
      ENDIF

      ! treat soil/snow absorption in direct and diffuse respectively
      ssoi(1,1) = tran(1,1)*(1.-albsoi(1,2)) + tran(1,3)*(1-albsoi(1,1))
      ssoi(2,1) = tran(2,1)*(1.-albsoi(2,2)) + tran(2,3)*(1-albsoi(2,1))
      ssoi(1,2) = tran(1,2)*(1.-albsoi(1,2))
      ssoi(2,2) = tran(2,2)*(1.-albsoi(2,2))

      ssno(1,1) = tran(1,1)*(1.-albsno(1,2)) + tran(1,3)*(1-albsno(1,1))
      ssno(2,1) = tran(2,1)*(1.-albsno(2,2)) + tran(2,3)*(1-albsno(2,1))
      ssno(1,2) = tran(1,2)*(1.-albsno(1,2))
      ssno(2,2) = tran(2,2)*(1.-albsno(2,2))

!-----------------------------------------------------------------------

   END SUBROUTINE albland


   SUBROUTINE twostream ( chil, rho, tau, green, lai, sai, fwet_snow, &
              coszen, albg, albv, tran, thermk, extkb, extkd, ssun, ssha )

!-----------------------------------------------------------------------
!
!  calculation of canopy albedos via two stream approximation (direct
!  and diffuse ) and partition of incident solar
!
!  Original author: Yongjiu Dai, June 11, 2001
!
!-----------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_Namelist, only: DEF_VEG_SNOW
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
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

!-------------------------- Local Variables ----------------------------
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
! projected area of photo elements in direction of mu and
! average inverse diffuse optical depth per unit leaf area

      phi1  = 0.5 - 0.633 * chil - 0.33 * chil * chil
      phi2  = 0.877 * ( 1. - 2. * phi1 )

      proj  = phi1 + phi2 * coszen
      extkb = proj / coszen

      extkd = 0.719

      IF (abs(phi1).gt.1.e-6 .and. abs(phi2).gt.1.e-6) THEN
         zmu = 1. / phi2 * ( 1. - phi1 / phi2 * log ( ( phi1 + phi2 ) / phi1 ) )
      ELSEIF (abs(phi1).le.1.e-6) THEN
         zmu = 1./0.877
      ELSEIF (abs(phi2).le.1.e-6) THEN
         zmu = 1./(2.*phi1)
      ENDIF
      zmu2 = zmu * zmu

#if (defined LULC_USGS)
      ! yuan: to be consistent with CoLM2014, no stem considered
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


#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
   SUBROUTINE twostream_mod ( chil, rho, tau, green, lai, sai, fwet_snow, &
              coszen, albg, albv, tran, thermk, extkb, extkd, ssun, ssha )

!-----------------------------------------------------------------------
!
! !DESCRIPTION:
!     An improved two stream approximation
!
!  Original author: Yongjiu Dai, June 11, 2001
!                   Hua Yuan, 03/2020
!
! !REFERENCES:
!  1) Yuan, H., Dai, Y., Dickinson, R. E., Pinty, B., Shangguan, W.,
!  Zhang, S., et al. (2017). Reexamination and further development of
!  two-stream canopy radiative transfer models for global land modeling.
!  Journal of Advances in Modeling Earth Systems, 9(1), 113-129.
!  https://doi.org/10.1002/2016MS000773
!
!-----------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_Namelist, only: DEF_VEG_SNOW
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
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
            coszen,        &! cosine of solar zenith angle
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

!-------------------------- Local Variables ----------------------------
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
   real(r8) :: upscat_sno = 0.5   !upscatter parameter for snow
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
! projected area of photo elements in direction of mu and
! average inverse diffuse optical depth per unit leaf area

      phi1  = 0.5 - 0.633 * chil - 0.33 * chil * chil
      phi2  = 0.877 * ( 1. - 2. * phi1 )

      extkd = 0.719

      IF (abs(phi1).gt.1.e-6 .and. abs(phi2).gt.1.e-6) THEN
         zmu = 1. / phi2 * ( 1. - phi1 / phi2 * log ( ( phi1 + phi2 ) / phi1 ) )
      ELSEIF (abs(phi1).le.1.e-6) THEN
         zmu = 1./0.877
      ELSEIF (abs(phi2).le.1.e-6) THEN
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

      ENDDO !iw

      ! restore extkb
      extkb = extkbd

   END SUBROUTINE twostream_mod
#endif


#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
   SUBROUTINE twostream_wrap ( ipatch, coszen, albg, &
              albv, tran, ssun, ssha )

!-----------------------------------------------------------------------
!
! !DESCRIPTION:
!  A Wrap subroutine to calculate PFT radiation using two-stream model
!
!  Created by Hua Yuan, 03/2020
!
!-----------------------------------------------------------------------
   USE MOD_Precision
   USE MOD_LandPFT
   USE MOD_Const_PFT
   USE MOD_Vars_PFTimeInvariants
   USE MOD_Vars_PFTimeVariables
   USE MOD_Namelist, only: DEF_USE_PC, DEF_PC_CROP_SPLIT
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   integer, intent(in) :: &
         ipatch          ! patch index

   ! environmental variables
   real(r8), intent(in) ::  &
         coszen,        &! cosine of solar zenith angle
         albg(2,2)       ! albedos of ground

   ! output
   real(r8), intent(inout) :: &
         albv(2,2),     &! albedo, vegetation [-]
         tran(2,3),     &! canopy transmittances for solar radiation
         ssun(2,2),     &! sunlit canopy absorption for solar radiation
         ssha(2,2)       ! shaded canopy absorption for solar radiation,
                         ! normalized by the incident flux

!-------------------------- Local Variables ----------------------------
   integer :: i, p, ps, pe
   real(r8), allocatable :: tran_p(:,:,:)
   real(r8), allocatable :: albv_p(:,:,:)

!-----------------------------------------------------------------------

      ps = patch_pft_s(ipatch)
      pe = patch_pft_e(ipatch)

      allocate ( tran_p (2,3,ps:pe) )
      allocate ( albv_p (2,2,ps:pe) )

      DO i = ps, pe
         p = pftclass(i)

         ! If defined DEF_PC_CROP_SPLIT, for crop PFTs, use 1D twostream model;
         ! Otherwise, set their value to PC 3D model results.
         IF ( DEF_USE_PC .and. (.not.DEF_PC_CROP_SPLIT .or. p.lt.15) ) THEN
            albv_p(:,:,i) = albv(:,:)
            tran_p(:,:,i) = tran(:,:)
            CYCLE
         ENDIF

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

      IF (ssun(1,1)<0 .or. ssun(1,2)<0 .or. ssun(2,1)<0 .or. ssun(2,2)<0) THEN
         print *, 'Warning: negative ssun in albedo calculation!',ipatch
         print *, ssun
      ENDIF

      deallocate ( tran_p )
      deallocate ( albv_p )

   END SUBROUTINE twostream_wrap
#endif


   SUBROUTINE snowage ( deltim,tg,scv,scvold,sag )

!=======================================================================
! Original version: Robert Dickinson
! Update snow cover and snow age, based on BATS code
!=======================================================================

   USE MOD_Precision
   USE MOD_Const_Physical, only: tfrz
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------

   real(r8), intent(in) :: deltim ! seconds in a time step [second]
   real(r8), intent(in) :: tg     ! temperature of soil at surface [K]
   real(r8), intent(in) :: scv    ! snow cover, water equivalent [mm]
   real(r8), intent(in) :: scvold ! snow cover for previous time step [mm]
   real(r8), intent(inout) :: sag ! non dimensional snow age [-]

!-------------------------- Local Variables ----------------------------

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
! Over Antarctica
!
      ELSEIF (scv > 800.) THEN
         sag = 0.
!
! Away from Antarctica
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
                          flx_absiv     ,flx_absin      )

!-----------------------------------------------------------------------
! !DESCRIPTION:
!  The calling sequence is:
!  -> SNICAR_RT:   snow albedos: direct beam (SNICAR)
!     or
!     SNICAR_AD_RT: snow albedos: direct beam (SNICAR-AD)
!  -> SNICAR_RT:   snow albedos: diffuse (SNICAR)
!     or
!     SNICAR_AD_RT:   snow albedos: diffuse (SNICAR-AD)
!
! !ORIGINAL:
!  1) The Community Land Model version5.0 (CLM5.0)
!  2) Energy Exascale Earth System Model version 2.0 (E3SM v2.0) Land Model (ELM v2.0)
!
! !REFERENCES:
!  1) Flanner et al, 2021, SNICAR-ADv3: a community tool for modeling spectral snow albedo.
!  Geosci. Model Dev., 14, 7673-7704, https://doi.org/10.5194/gmd-14-7673-2021
!  2) Hao et al., 2023, Improving snow albedo modeling in the E3SM land model (version 2.0)
!  and assessing its impacts on snow and surface fluxes over the Tibetan Plateau.
!  Geosci. Model Dev., 16, 75-94, https://doi.org/10.5194/gmd-16-75-2023
!
! !REVISIONS:
!  Yongjiu Dai, and Hua Yuan, December, 2022 : ASSEMBLING and FITTING
!
!-----------------------------------------------------------------------
! !USES:
   USE MOD_Vars_Global, only: maxsnl
   USE MOD_SnowSnicar, only: SNICAR_RT, SNICAR_AD_RT

   ! and the evolution of snow effective radius
   !
   ! DAI, Dec. 28, 2022

   IMPLICIT NONE

!-------------------------------------------------------------------------
! temporary setting

   integer, parameter :: numrad  = 2            ! number of solar radiation bands: vis, nir
   integer, parameter :: sno_nbr_aer = 8        ! number of aerosol species in snowpack
   logical, parameter :: DO_SNO_OC   = .true.   ! to include organic carbon (OC)
   logical, parameter :: DO_SNO_AER  = .true.   ! to include aerosols in snow radiative calculations
   integer, parameter :: subgridflag = 1        ! = 0 USE subgrid fluxes, = 1 not USE subgrid fluxes
   !
   ! !ARGUMENTS:
   !
   logical , intent(in) :: use_snicar_frc       ! true: IF radiative forcing is being calculated,
                                                ! first estimate clean-snow albedo
   logical , intent(in) :: use_snicar_ad        ! true: USE SNICAR_AD_RT, false: USE SNICAR_RT

   real(r8), intent(in) :: coszen_col                   ! cosine of solar zenith angle
   real(r8), intent(in) :: albsod        ( numrad )     ! direct-beam soil albedo (col,bnd) [frc]
   real(r8), intent(in) :: albsoi        ( numrad )     ! diffuse soil albedo (col,bnd) [frc]

   integer , intent(in) :: snl                          ! negative number of snow layers (col) [nbr]
   real(r8), intent(in) :: frac_sno                     ! fraction of ground covered by snow (0-1)
   real(r8), intent(in) :: h2osno                       ! snow water equivalent (mm H2O)
   real(r8), intent(in) :: h2osno_liq    ( maxsnl+1:0 ) ! liquid water content (col,lyr) [kg/m2]
   real(r8), intent(in) :: h2osno_ice    ( maxsnl+1:0 ) ! ice lens content (col,lyr) [kg/m2]
   real(r8), intent(in) :: snw_rds       ( maxsnl+1:0 ) ! snow grain radius (col,lyr) [microns]

   real(r8), intent(in) :: mss_cnc_bcphi ( maxsnl+1:0 ) ! mass conc. of hydrophilic BC [kg/kg]
   real(r8), intent(in) :: mss_cnc_bcpho ( maxsnl+1:0 ) ! mass conc. of hydrophobic BC [kg/kg]
   real(r8), intent(in) :: mss_cnc_ocphi ( maxsnl+1:0 ) ! mass conc. of hydrophilic OC [kg/kg]
   real(r8), intent(in) :: mss_cnc_ocpho ( maxsnl+1:0 ) ! mass conc. of hydrophobic OC [kg/kg]
   real(r8), intent(in) :: mss_cnc_dst1  ( maxsnl+1:0 ) ! mass conc. of dust aerosol 1 [kg/kg]
   real(r8), intent(in) :: mss_cnc_dst2  ( maxsnl+1:0 ) ! mass conc. of dust aerosol 2 [kg/kg]
   real(r8), intent(in) :: mss_cnc_dst3  ( maxsnl+1:0 ) ! mass conc. of dust aerosol 3 [kg/kg]
   real(r8), intent(in) :: mss_cnc_dst4  ( maxsnl+1:0 ) ! mass conc. of dust aerosol 4 [kg/kg]

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
   real(r8), intent(out) :: flx_absdv    ( maxsnl+1:1 ) ! direct flux absorption factor  VIS [frc]
   real(r8), intent(out) :: flx_absdn    ( maxsnl+1:1 ) ! direct flux absorption factor  NIR [frc]
   real(r8), intent(out) :: flx_absiv    ( maxsnl+1:1 ) ! diffuse flux absorption factor VIS [frc]
   real(r8), intent(out) :: flx_absin    ( maxsnl+1:1 ) ! diffuse flux absorption factor NIR [frc]

   !-----------------------------------------------------------------------
   !
   ! !LOCAL VARIABLES:
   integer  :: i            ! index for layers [idx]
   integer  :: aer          ! index for sno_nbr_aer
   integer  :: ib           ! band index
   integer  :: ic           ! 0=unit incoming direct; 1=unit incoming diffuse
   integer  :: flg_slr      ! flag for SNICAR (=1 IF direct, =2 IF diffuse)
   integer  :: flg_snw_ice  ! flag for SNICAR (=1 when called from ELM, =2 when called from sea-ice)

   ! mass concentration of aerosol species for forcing calculation (zero) (lyr,aer) [kg kg-1]
   real(r8) :: mss_cnc_aer_in_frc_pur (maxsnl+1:0,sno_nbr_aer)
   ! mass concentration of aerosol species for BC forcing (lyr,aer) [kg kg-1]
   real(r8) :: mss_cnc_aer_in_frc_bc  (maxsnl+1:0,sno_nbr_aer)
   ! mass concentration of aerosol species for OC forcing (lyr,aer) [kg kg-1]
   real(r8) :: mss_cnc_aer_in_frc_oc  (maxsnl+1:0,sno_nbr_aer)
   ! mass concentration of aerosol species for dust forcing (lyr,aer) [kg kg-1]
   real(r8) :: mss_cnc_aer_in_frc_dst (maxsnl+1:0,sno_nbr_aer)
   ! mass concentration of all aerosol species for feedback calculation (lyr,aer) [kg kg-1]
   real(r8) :: mss_cnc_aer_in_fdb     (maxsnl+1:0,sno_nbr_aer)

   real(r8) :: albsfc       (numrad)            ! albedo of surface underneath snow (col,bnd)
   real(r8) :: albsnd       (numrad)            ! snow albedo (direct)
   real(r8) :: albsni       (numrad)            ! snow albedo (diffuse)
   real(r8) :: albsnd_pur   (numrad)            ! direct pure snow albedo
   real(r8) :: albsni_pur   (numrad)            ! diffuse pure snow albedo
   real(r8) :: albsnd_bc    (numrad)            ! direct snow albedo without BC
   real(r8) :: albsni_bc    (numrad)            ! diffuse snow albedo without BC
   real(r8) :: albsnd_oc    (numrad)            ! direct snow albedo without OC
   real(r8) :: albsni_oc    (numrad)            ! diffuse snow albedo without OC
   real(r8) :: albsnd_dst   (numrad)            ! direct snow albedo without dust
   real(r8) :: albsni_dst   (numrad)            ! diffuse snow albedo without dust
   real(r8) :: flx_absd_snw (maxsnl+1:1,numrad) ! flux absorption for just snow (direct) [frc]
   real(r8) :: flx_absi_snw (maxsnl+1:1,numrad) ! flux absorption for just snow (diffuse) [frc]
   real(r8) :: foo_snw      (maxsnl+1:1,numrad) ! dummy array for forcing calls

   integer  :: snw_rds_in   (maxsnl+1:0)        ! snow grain size sent to SNICAR (col,lyr) [microns]

   integer , parameter :: nband =numrad         ! number of solar radiation waveband classes

   !-----------------------------------------------------------------------

      ! Initialize output because solar radiation only done IF coszen > 0

      DO ib = 1, numrad
         albgrd(ib)     = 1._r8
         albgri(ib)     = 1._r8
         albgrd_pur(ib) = 1._r8
         albgri_pur(ib) = 1._r8
         albgrd_bc(ib)  = 1._r8
         albgri_bc(ib)  = 1._r8
         albgrd_oc(ib)  = 1._r8
         albgri_oc(ib)  = 1._r8
         albgrd_dst(ib) = 1._r8
         albgri_dst(ib) = 1._r8
         DO i=maxsnl+1,1,1
            flx_absdv(i) = 0._r8
            flx_absdn(i) = 0._r8
            flx_absiv(i) = 0._r8
            flx_absin(i) = 0._r8
         ENDDO
      ENDDO  ! END of numrad loop

      ! set variables to pass to SNICAR.

      flg_snw_ice = 1
      albsfc(:)     = albsoi(:)
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
            ! because the order of SoilAlbedo and SNICAR_RT/SNICAR_AD_RT
            ! was switched for SNICAR/SNICAR_AD_RT.
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
                  ELSEIF (ib == 2) THEN
                     flx_absdn(i) = flx_absd_snw(i,ib)*frac_sno + &
                          ((1.-frac_sno)*(1-albsod(ib))*(flx_absd_snw(i,ib)/(1.-albsnd(ib))))
                     flx_absin(i) = flx_absi_snw(i,ib)*frac_sno + &
                          ((1.-frac_sno)*(1-albsoi(ib))*(flx_absi_snw(i,ib)/(1.-albsni(ib))))
                  ENDIF
               ELSE
                  IF (ib == 1) THEN
                     flx_absdv(i) = flx_absd_snw(i,ib)!*(1.-albsnd(ib))
                     flx_absiv(i) = flx_absi_snw(i,ib)!*(1.-albsni(ib))
                  ELSEIF (ib == 2) THEN
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
! Yongjiu Dai and Xin-Zhong Liang (08/01/2001)
!-----------------------------------------------------------------------

   USE MOD_Precision
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------

   real(r8), intent(in) :: oro       ! /ocean(0)/seaice(2) flag
   real(r8), intent(in) :: scv       ! snow water equivalent) [mm]
   real(r8), intent(in) :: coszrs    ! Cosine solar zenith angle

   real(r8), intent(out) :: alb(2,2) ! srf alb for direct (diffuse) rad 0.2-0.7 micro-ms
                                     ! Srf alb for direct (diffuse) rad 0.7-5.0 micro-ms

!-------------------------- Local Variables ----------------------------

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

END MODULE MOD_Albedo
! --------- EOP ----------
