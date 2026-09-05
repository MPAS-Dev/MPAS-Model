#include <define.h>

MODULE MOD_Urban_Albedo
!-----------------------------------------------------------------------
! !DESCRIPTION:
!
!  Calculate the total urban albedo. Prepare albedo values over water,
!  roof, ground with snow cover. Then CALL 3D urban radiation transfer
!  model. Finally calculate the total albedo weighted by the urban and
!  water fractional cover.
!
!  Created by Hua Yuan, 09/2021
!
!
! !REVISIONS:
!
!  07/2023, Hua Yuan: Fix low zenith angle problem for urban radiation
!           calculation and urban display height problem when
!           considering vegetations. modify limitation for conzen value
!           (0.001->0.01) for urban.
!
!  05/2024, Hua Yuan: Account for vegetation snow optical properties.
!
!-----------------------------------------------------------------------
   USE MOD_Precision
   IMPLICIT NONE
   SAVE

! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: alburban

!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------

   SUBROUTINE alburban (ipatch,froof,fgper,flake,hlr,hroof,&
                        alb_roof,alb_wall,alb_gimp,alb_gper,&
                        rho,tau,fveg,hveg,lai,sai,fwet_snow,coszen,fwsun,tlake,&
                        fsno_roof,fsno_gimp,fsno_gper,fsno_lake,&
                        scv_roof,scv_gimp,scv_gper,scv_lake,&
                        sag_roof,sag_gimp,sag_gper,sag_lake,&
                        dfwsun,extkd,alb,ssun,ssha,sroof,swsun,swsha,sgimp,sgper,slake)

!=======================================================================
! Calculates fragmented albedos (direct and diffuse) for urban area in
! wavelength regions split at 0.7um.
!
! (1) snow albedos: as in BATS formulations, which are inferred from
!     the calculations of Wiscombe and Warren (1980) and the snow model
!     and data of Anderson(1976), and the function of snow age, grain
!     size, solar zenith angle, pollution, the amount of the fresh snow
! (2) lake and wetland albedos: as in BATS, which depend on cosine solar
!     zenith angle, based on data in Henderson-Sellers (1986). The
!     frozen lake and wetland albedos are set to constants (0.6 for
!     visible beam, 0.4 for near-infrared)
! (3) over the snow covered surface, the surface albedo is estimated by
!     a linear combination of albedos for snow, roof, impervious and
!     pervious ground
!
!=======================================================================

   USE MOD_Precision
   USE MOD_Const_Physical, only: tfrz
   USE MOD_Namelist, only: DEF_VEG_SNOW
   USE MOD_Urban_Shortwave

   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
! ground cover index
   integer, intent(in) :: &
      ipatch          ! patch index

   real(r8), intent(in) :: &
      froof,         &! roof fraction
      fgper,         &! impervious ground weight fraction
      flake,         &! lake fraction
      hlr,           &! average building height to their side length
      hroof           ! average building height

   real(r8), intent(in) :: &
      alb_roof(2,2), &! roof albedo (iband,direct/diffuse)
      alb_wall(2,2), &! wall albedo (iband,direct/diffuse)
      alb_gimp(2,2), &! impervious albedo (iband,direct/diffuse)
      alb_gper(2,2)   ! pervious albedo (iband,direct/diffuse)

   real(r8), intent(in) :: &
      rho(2,2),      &! leaf reflectance (iw=iband, il=life and dead)
      tau(2,2),      &! leaf transmittance (iw=iband, il=life and dead)
      fveg,          &! fractional vegetation cover [-]
      hveg,          &! vegetation central crown height [m]
      lai,           &! leaf area index (LAI+SAI) [m2/m2]
      sai,           &! stem area index (LAI+SAI) [m2/m2]
      fwet_snow,     &! vegetation snow fractional cover [-]

      ! variables
      coszen,        &! cosine of solar zenith angle [-]
      fwsun,         &! sunlit wall fraction [-]
      tlake,         &! lake surface temperature [K]
      fsno_roof,     &! fraction of soil covered by snow [-]
      fsno_gimp,     &! fraction of soil covered by snow [-]
      fsno_gper,     &! fraction of soil covered by snow [-]
      fsno_lake,     &! fraction of soil covered by snow [-]
      scv_roof,      &! snow cover, water equivalent [mm]
      scv_gimp,      &! snow cover, water equivalent [mm]
      scv_gper,      &! snow cover, water equivalent [mm]
      scv_lake,      &! snow cover, water equivalent [mm]
      sag_roof,      &! non dimensional snow age [-]
      sag_gimp,      &! non dimensional snow age [-]
      sag_gper,      &! non dimensional snow age [-]
      sag_lake        ! non dimensional snow age [-]

   real(r8), intent(out) :: &
      dfwsun,        &! change of fwsun
      extkd,         &! diffuse and scattered diffuse PAR extinction coefficient
      alb  (2,2),    &! averaged albedo [-]
      ssun (2,2),    &! sunlit canopy absorption for solar radiation
      ssha (2,2),    &! shaded canopy absorption for solar radiation,
      sroof(2,2),    &! roof absorption for solar radiation,
      swsun(2,2),    &! sunlit wall absorption for solar radiation,
      swsha(2,2),    &! shaded wall absorption for solar radiation,
      sgimp(2,2),    &! impervious ground absorption for solar radiation,
      sgper(2,2),    &! pervious ground absorption for solar radiation,
      slake(2,2)      ! lake absorption for solar radiation,

!-------------------------- Local Variables ----------------------------
   real(r8) :: &
      age,           &! factor to reduce visible snow alb due to snow age [-]
      albg0,         &! temporary varaiable [-]
      alb_s_inc,     &! decrease in soil albedo due to wetness [-]
      beta0,         &! upscattering parameter for direct beam [-]
      cff,           &! snow alb correction factor for zenith angle > 60 [-]
      conn,          &! constant (=0.5) for visible snow alb calculation [-]
      cons,          &! constant (=0.2) for nir snow albedo calculation [-]
      czen,          &! cosine of solar zenith angle > 0 [-]
      theta,         &! solar zenith angle
      fwsun_,        &! sunlit wall fraction
      czf,           &! solar zenith correction for new snow albedo [-]
      dfalbl,        &! snow albedo for diffuse nir radiation [-]
      dfalbs,        &! snow albedo for diffuse visible solar radiation [-]
      dralbl,        &! snow albedo for visible radiation [-]
      dralbs,        &! snow albedo for near infrared radiation [-]
      sl,            &! factor that helps control alb zenith dependence [-]
      snal0,         &! alb for visible,incident on new snow (zen ang<60) [-]
      snal1           ! alb for NIR, incident on new snow (zen angle<60) [-]

   real(r8) :: &
      erho(2),       &! effective reflection of leaf+stem
      etau(2),       &! effective transmittance of leaf+stem
      albsno (2,2),  &! snow albedo [-]
      albroof(2,2),  &! albedo, ground
      albgimp(2,2),  &! albedo, ground
      albgper(2,2),  &! albedo, ground
      alblake(2,2)    ! albedo, ground

   ! vegetation snow optical properties, 1:vis, 2:nir
   real(r8) :: rho_sno(2), tau_sno(2)
   data rho_sno(1), rho_sno(2) /0.5, 0.2/
   data tau_sno(1), tau_sno(2) /0.3, 0.2/

! ----------------------------------------------------------------------
! 1. Initial set
! ----------------------------------------------------------------------

! short and long wave albedo for new snow
      snal0 = 0.85    ! shortwave
      snal1 = 0.65    ! long wave

! ----------------------------------------------------------------------
! set default soil and vegetation albedos and solar absorption
      alb     (:,:) = 1. ! averaged
      ssun    (:,:) = 0.
      ssha    (:,:) = 0.
      sroof   (:,:) = 0.
      swsun   (:,:) = 0.
      swsha   (:,:) = 0.
      sgimp   (:,:) = 0.
      sgper   (:,:) = 0.
      alblake (:,:) = 1.
      slake   (:,:) = 0.

      dfwsun = 0.
      extkd  = 0.718

      IF(coszen <= -0.3) THEN
         !print *, "coszen < 0, ipatch and coszen: ", ipatch, coszen
         RETURN  !only do albedo when coszen > -0.3
      ENDIF

      czen = max(coszen, 0.01)
      albsno(:,:) = 0.    !set initial snow albedo
      cons = 0.2          !parameter for snow albedo
      conn = 0.5          !parameter for snow albedo
      sl   = 2.0          !sl helps control albedo zenith dependence

      ! effective leaf optical properties: rho and tau.
      IF (lai+sai>1.e-6 .and. fveg>0.) THEN
         erho(:) = rho(:,1)*lai/(lai+sai) + rho(:,2)*sai/(lai+sai)
         etau(:) = tau(:,1)*lai/(lai+sai) + tau(:,2)*sai/(lai+sai)
      ENDIF

      ! correct for snow on leaf
      IF ( DEF_VEG_SNOW ) THEN
         ! modify rho, tau, USE: fwet_snow
         erho(:) = (1-fwet_snow)*erho(:) + fwet_snow*rho_sno(:)
         etau(:) = (1-fwet_snow)*etau(:) + fwet_snow*tau_sno(:)
      ENDIF

! ----------------------------------------------------------------------
! 2. get albedo over water, roof, ground
! ----------------------------------------------------------------------

! 2.1 albedo for inland water (NOTE: wetland is removed)
      albg0 = 0.05/(czen+0.15)
      alblake(:,1) = albg0
      alblake(:,2) = 0.1               !Subin (2012)

      IF (tlake < tfrz) THEN           !frozen lake and wetland
         alblake(1,:) = 0.6
         alblake(2,:) = 0.4
      ENDIF

      IF (scv_lake > 0.) THEN

         ! correction for snow age
         age = 1.-1./(1.+sag_lake) !correction for snow age
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

      ENDIF

      alblake(:,:) = (1.-fsno_lake)*alblake(:,:) + fsno_lake*albsno(:,:)
      slake(:,:)   = 1. - alblake(:,:)

! 2.2 roof albedo with snow
      IF (scv_roof > 0.) THEN

         ! correction for snow age
         age = 1.-1./(1.+sag_roof) !correction for snow age
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

      ENDIF

      albroof(:,:) = (1.-fsno_roof)*alb_roof(:,:) + fsno_roof*albsno(:,:)

! 2.3 impervious ground albedo with snow
      IF (scv_gimp > 0.) THEN

         ! correction for snow age
         age = 1.-1./(1.+sag_gimp) !correction for snow age
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

      ENDIF

      albgimp(:,:) = (1.-fsno_gimp)*alb_gimp(:,:) + fsno_gimp*albsno(:,:)

! 2.4 pervious ground albedo with snow
      IF (scv_gper > 0.) THEN

         ! correction for snow age
         age = 1.-1./(1.+sag_gper) !correction for snow age
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

      ENDIF

      albgper(:,:) = (1.-fsno_gper)*alb_gper(:,:) + fsno_gper*albsno(:,:)

! ----------------------------------------------------------------------
! 3. Urban albedo
! ----------------------------------------------------------------------

      theta = acos(czen)

      ! Distinguish between no-vegetation and vegetation-included cases
      IF (lai+sai>1.e-6 .and. fveg>0.) THEN

         CALL UrbanVegShortwave ( &
            theta, hlr, froof, fgper, hroof, &
            albroof(1,1), alb_wall(1,1), albgimp(1,1), albgper(1,1), &
            lai, sai, fveg, hveg, erho(1), etau(1), &
            fwsun_, sroof(1,:), swsun(1,:), swsha(1,:), sgimp(1,:), &
            sgper(1,:), ssun(1,:), alb(1,:))

         CALL UrbanVegShortwave ( &
            theta, hlr, froof, fgper, hroof, &
            albroof(2,1), alb_wall(2,1), albgimp(2,1), albgper(2,1), &
            lai, sai, fveg, hveg, erho(2), etau(2), &
            fwsun_, sroof(2,:), swsun(2,:), swsha(2,:), sgimp(2,:), &
            sgper(2,:), ssun(2,:), alb(2,:))
      ELSE

         CALL UrbanOnlyShortwave ( &
            theta, hlr, froof, fgper, hroof, &
            albroof(1,1), alb_wall(1,1), albgimp(1,1), albgper(1,1), &
            fwsun_, sroof(1,:), swsun(1,:), swsha(1,:), sgimp(1,:), &
            sgper(1,:), alb(1,:))

         CALL UrbanOnlyShortwave ( &
            theta, hlr, froof, fgper, hroof, &
            albroof(2,1), alb_wall(2,1), albgimp(2,1), albgper(2,1), &
            fwsun_, sroof(2,:), swsun(2,:), swsha(2,:), sgimp(2,:), &
            sgper(2,:), alb(2,:))

         ssun(:,:) = 0.
      ENDIF

      dfwsun = fwsun_ - fwsun

      alb(:,:) = (1.-flake)*alb(:,:) + flake*alblake(:,:)

   END SUBROUTINE alburban

END MODULE MOD_Urban_Albedo
! ---------- EOP ------------
