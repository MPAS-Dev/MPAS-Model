#include <define.h>

MODULE MOD_Lake

!-----------------------------------------------------------------------
! !DESCRIPTION:
!  Simulating energy balance processes of land water body
!
! !REFERENCES:
!  Dai et al, 2018, The lake scheme of the common land model and its performance evaluation.
!  Chinese Science Bulletin, 63(28-29), 3002-3021, https://doi.org/10.1360/N972018-00609
!
!  Original author: Yongjiu Dai 04/2014/
!
! !REVISIONS:
!  Nan Wei,  01/2018: interaction btw prec and lake surface including phase change of prec and water body
!  Nan Wei,  06/2018: update heat conductivity of water body and soil below and snow hydrology
!  Hua Yuan, 01/2023: added snow layer absorption in melting calculation
!-----------------------------------------------------------------------

   USE MOD_Precision
   IMPLICIT NONE
   SAVE

! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: newsnow_lake
   PUBLIC :: laketem
   PUBLIC :: snowwater_lake


! PRIVATE MEMBER FUNCTIONS:
   PRIVATE :: roughness_lake
   PRIVATE :: hConductivity_lake


!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------



   SUBROUTINE newsnow_lake ( USE_Dynamic_Lake, &
            ! "in" arguments
            ! ---------------
            maxsnl    , nl_lake   , deltim      , dz_lake ,&
            pg_rain   , pg_snow   , t_precip    , bifall  ,&

            ! "inout" arguments
            ! ------------------
            t_lake    , zi_soisno , z_soisno    ,&
            dz_soisno , t_soisno  , wliq_soisno , wice_soisno ,&
            fiold     , snl       , sag         , scv         ,&
            snowdp    , lake_icefrac )

!-----------------------------------------------------------------------
! !DESCRIPTION:
!  Add new snow nodes and interaction btw prec and lake surface including phase
!  change of prec and water body
!
!  Original author: Yongjiu Dai, 04/2014
!
! !REVISIONS:
!  Nan Wei,  01/2018: update interaction btw prec and lake surface
!-----------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_Const_Physical, only: tfrz, denh2o, cpliq, cpice, hfus
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   logical,  intent(in) :: USE_Dynamic_Lake

   integer,  intent(in) :: maxsnl     ! maximum number of snow layers
   integer,  intent(in) :: nl_lake    ! number of soil layers
   real(r8), intent(in) :: deltim     ! seconds in a time step [second]
   real(r8), intent(inout) :: pg_rain ! liquid water onto ground [kg/(m2 s)]
   real(r8), intent(inout) :: pg_snow ! ice onto ground [kg/(m2 s)]
   real(r8), intent(in) :: t_precip   ! snowfall/rainfall temperature [kelvin]
   real(r8), intent(in) :: bifall     ! bulk density of newly fallen dry snow [kg/m3]

   real(r8), intent(inout) :: dz_lake(1:nl_lake)      ! lake layer thickness (m)
   real(r8), intent(inout) ::   zi_soisno(maxsnl:0)   ! interface level below a "z" level (m)
   real(r8), intent(inout) ::    z_soisno(maxsnl+1:0) ! snow layer depth (m)
   real(r8), intent(inout) ::   dz_soisno(maxsnl+1:0) ! snow layer thickness (m)
   real(r8), intent(inout) ::    t_soisno(maxsnl+1:0) ! snow layer temperature [K]
   real(r8), intent(inout) :: wliq_soisno(maxsnl+1:0) ! snow layer liquid water (kg/m2)
   real(r8), intent(inout) :: wice_soisno(maxsnl+1:0) ! snow layer ice lens (kg/m2)
   real(r8), intent(inout) ::       fiold(maxsnl+1:0) ! fraction of ice relative to the total water
   integer,  intent(inout) :: snl                     ! number of snow layers
   real(r8), intent(inout) :: sag                     ! non dimensional snow age [-]
   real(r8), intent(inout) :: scv                     ! snow mass (kg/m2)
   real(r8), intent(inout) :: snowdp                  ! snow depth (m)
   real(r8), intent(inout) :: lake_icefrac(1:nl_lake) ! mass fraction of lake layer that is frozen
   real(r8), intent(inout) :: t_lake(1:nl_lake)       ! lake layer temperature (m)

!-------------------------- Local Variables ----------------------------

   integer lb
   integer newnode    ! signification when new snow node is set, (1=yes, 0=non)
   real(r8) dz_snowf  ! layer thickness rate change due to precipitation [m/s]
   real(r8) a, b, c, d, e, f, g, h
   real(r8) wice_lake(1:nl_lake), wliq_lake(1:nl_lake), tw

!-----------------------------------------------------------------------

      newnode = 0
      dz_snowf = pg_snow/bifall
      snowdp = snowdp + dz_snowf*deltim
      scv = scv + pg_snow*deltim      ! snow water equivalent (mm)


      zi_soisno(0) = 0.

      IF (snl==0 .and. snowdp < 0.01) THEN       ! no snow layer, energy exchange between prec and lake surface

         a = cpliq*pg_rain*deltim*(t_precip-tfrz)                          !cool down rainfall to tfrz
         b = pg_rain*deltim*hfus                                           !all rainfall frozen
         c = cpice*denh2o*dz_lake(1)*lake_icefrac(1)*(tfrz-t_lake(1))      !warm up lake surface ice to tfrz
         d = denh2o*dz_lake(1)*lake_icefrac(1)*hfus                        !all lake surface ice melt
         e = cpice*pg_snow*deltim*(tfrz-t_precip)                          !warm up snowfall to tfrz
         f = pg_snow*deltim*hfus                                           !all snowfall melt
         g = cpliq*denh2o*dz_lake(1)*(1-lake_icefrac(1))*(t_lake(1)-tfrz)  !cool down lake surface water to tfrz
         h = denh2o*dz_lake(1)*(1-lake_icefrac(1))*hfus                    !all lake surface water frozen
         sag = 0.0

         IF (lake_icefrac(1) > 0.999) THEN
            ! all rainfall frozen, release heat to warm up frozen lake surface
            IF (a+b<=c) THEN
               tw=min(tfrz,t_precip)
               t_lake(1)=(a+b+cpice*(pg_rain+pg_snow)*deltim*tw+cpice*denh2o*dz_lake(1)*t_lake(1)*lake_icefrac(1))/&
                         (cpice*denh2o*dz_lake(1)*lake_icefrac(1)+cpice*(pg_rain+pg_snow)*deltim)
               scv = scv+pg_rain*deltim
               snowdp = snowdp + pg_rain*deltim/bifall
               pg_snow = pg_snow+pg_rain
               pg_rain = 0.0
            ! prec tem at tfrz, partial rainfall frozen ->release heat -> warm up lake surface to tfrz (no latent heat)
            ELSEIF (a<=c) THEN
               t_lake(1)=tfrz
               scv = scv + (c-a)/hfus
               snowdp = snowdp + (c-a)/(hfus*bifall)
               pg_snow = pg_snow + min(pg_rain,(c-a)/(hfus*deltim))
               pg_rain = max(0.0,pg_rain - (c-a)/(hfus*deltim))
            ! lake surface tem at tfrz, partial lake surface melt -> absorb heat -> cool down rainfall to tfrz (no latent heat)
            ELSEIF (a<=c+d) THEN
               t_lake(1)=tfrz
               wice_lake(1) = denh2o*dz_lake(1) - (a-c)/hfus
               wliq_lake(1) = (a-c)/hfus
               lake_icefrac(1) = wice_lake(1)/(wice_lake(1) + wliq_lake(1))
            ! all lake surface melt, absorb heat to cool down rainfall
            ELSE  !(a>c+d)
               t_lake(1)=(cpliq*pg_rain*deltim*t_precip+cpliq*denh2o*dz_lake(1)*tfrz-c-d)/&
                         (cpliq*denh2o*dz_lake(1)+cpliq*pg_rain*deltim)
               lake_icefrac(1) = 0.0
            ENDIF

            IF (snowdp>=0.01) THEN  !frozen rain may make new snow layer
               snl = -1
               newnode = 1
               dz_soisno(0)  = snowdp             ! meter
               z_soisno (0)  = -0.5*dz_soisno(0)
               zi_soisno(-1) = -dz_soisno(0)
               sag = 0.                           ! snow age

               t_soisno (0) = t_lake(1)           ! K
               wice_soisno(0) = scv               ! kg/m2
               wliq_soisno(0) = 0.                ! kg/m2
               fiold(0) = 1.
            ENDIF

         ELSEIF (lake_icefrac(1) >= 0.001) THEN
            IF (pg_rain > 0.0 .and. pg_snow > 0.0) THEN
               t_lake(1)=tfrz
            ELSEIF (pg_rain > 0.0) THEN
               IF (a>=d) THEN
                  t_lake(1)=(cpliq*pg_rain*deltim*t_precip+cpliq*denh2o*dz_lake(1)*tfrz-d)/&
                            (cpliq*denh2o*dz_lake(1)+cpliq*pg_rain*deltim)
                  lake_icefrac(1) = 0.0
               ELSE
                  t_lake(1)=tfrz
                  wice_lake(1) = denh2o*dz_lake(1)*lake_icefrac(1) - a/hfus
                  wliq_lake(1) = denh2o*dz_lake(1)*(1-lake_icefrac(1)) + a/hfus
                  lake_icefrac(1) = wice_lake(1)/(wice_lake(1) + wliq_lake(1))
               ENDIF
            ELSEIF (pg_snow > 0.0) THEN
               IF (e>=h) THEN
                  t_lake(1)=(h+cpice*denh2o*dz_lake(1)*tfrz+cpice*pg_snow*deltim*t_precip)/&
                            (cpice*pg_snow*deltim+cpice*denh2o*dz_lake(1))
                  lake_icefrac(1) = 1.0
               ELSE
                  t_lake(1)=tfrz
                  wice_lake(1) = denh2o*dz_lake(1)*lake_icefrac(1) + e/hfus
                  wliq_lake(1) = denh2o*dz_lake(1)*(1-lake_icefrac(1)) - e/hfus
                  lake_icefrac(1) = wice_lake(1)/(wice_lake(1) + wliq_lake(1))
               ENDIF
            ENDIF

         ELSE
            ! all snowfall melt, absorb heat to cool down lake surface water
            IF (e+f<=g) THEN
               tw=max(tfrz,t_precip)
               t_lake(1)=(cpliq*denh2o*dz_lake(1)*t_lake(1)*(1-lake_icefrac(1))+cpliq*(pg_rain+pg_snow)*deltim*tw-e-f)/&
                         (cpliq*(pg_rain+pg_snow)*deltim+cpliq*denh2o*dz_lake(1)*(1-lake_icefrac(1)))
               scv = scv - pg_snow*deltim
               snowdp = snowdp - dz_snowf*deltim
               pg_rain = pg_rain + pg_snow
               pg_snow = 0.0
            ! prec tem at tfrz, partial snowfall melt ->absorb heat -> cool down lake surface to tfrz (no latent heat)
            ELSEIF (e<=g) THEN
               t_lake(1) = tfrz
               scv = scv - (g-e)/hfus
               snowdp = snowdp - (g-e)/(hfus*bifall)
               pg_rain = pg_rain + min(pg_snow, (g-e)/(hfus*deltim))
               pg_snow = max(0.0, pg_snow - (g-e)/(hfus*deltim))
            ! lake surface tem at tfrz, partial lake surface frozen -> release heat -> warm up snowfall to tfrz (no latent heat)
            ELSEIF (e<=g+h) THEN
               t_lake(1) = tfrz
               wice_lake(1) = (e-g)/hfus
               wliq_lake(1) = denh2o*dz_lake(1) - (e-g)/hfus
               lake_icefrac(1) = wice_lake(1)/(wice_lake(1) + wliq_lake(1))
            ! all lake surface frozen, release heat to warm up snowfall
            ELSE       !(e>g+h)
               t_lake(1) = (g+h+cpice*denh2o*dz_lake(1)*tfrz+cpice*pg_snow*deltim*t_precip)/&
                           (cpice*pg_snow*deltim+cpice*denh2o*dz_lake(1))
               lake_icefrac(1) = 1.0
            ENDIF
         ENDIF

         IF (USE_Dynamic_Lake .and. (snl == 0)) THEN

            wliq_lake(1)    = dz_lake(1) * (1-lake_icefrac(1)) + pg_rain*deltim*1.e-3
            wice_lake(1)    = dz_lake(1) * lake_icefrac(1)
            dz_lake(1)      = wliq_lake(1) + wice_lake(1)
            lake_icefrac(1) = wice_lake(1) / dz_lake(1)

            CALL adjust_lake_layer (nl_lake, dz_lake, t_lake, lake_icefrac)

         ENDIF

      ELSEIF (snl==0 .and. snowdp >= 0.01) THEN

          ! only ice part of snowfall is added here, the liquid part will be added later
         snl = -1
         newnode = 1
         dz_soisno(0)  = snowdp             ! meter
         z_soisno (0)  = -0.5*dz_soisno(0)
         zi_soisno(-1) = -dz_soisno(0)
         sag = 0.                           ! snow age

         t_soisno (0) = min(tfrz, t_precip) ! K
         wice_soisno(0) = scv               ! kg/m2
         wliq_soisno(0) = 0.                ! kg/m2
         fiold(0) = 1.

      ELSE                                   ! ( snl<0 .and. newnode ==0 )

         lb = snl + 1
         t_soisno(lb) = ( (wice_soisno(lb)*cpice+wliq_soisno(lb)*cpliq)*t_soisno(lb) &
                      +   (pg_rain*cpliq + pg_snow*cpice)*deltim*t_precip ) &
                      / ( wice_soisno(lb)*cpice + wliq_soisno(lb)*cpliq &
                      +   pg_rain*deltim*cpliq + pg_snow*deltim*cpice )

         t_soisno(lb) = min(tfrz, t_soisno(lb))
         wice_soisno(lb) = wice_soisno(lb)+deltim*pg_snow
         dz_soisno(lb) = dz_soisno(lb)+dz_snowf*deltim
         z_soisno(lb) = zi_soisno(lb) - 0.5*dz_soisno(lb)
         zi_soisno(lb-1) = zi_soisno(lb) - dz_soisno(lb)

      ENDIF

   END SUBROUTINE newsnow_lake



   SUBROUTINE laketem (&
           ! "in" arguments
           ! -------------------
           patchtype    , maxsnl      , nl_soil      , nl_lake   ,&
           dlat         , deltim      , forc_hgt_u   , forc_hgt_t,&
           forc_hgt_q   , forc_us     , forc_vs      , forc_t    ,&
           forc_q       , forc_rhoair , forc_psrf    , forc_sols ,&
           forc_soll    , forc_solsd  , forc_solld   , sabg      ,&
           forc_frl     , dz_soisno   , z_soisno     , zi_soisno ,&
           dz_lake      , lakedepth   , vf_quartz    , vf_gravels,&
           vf_om        , vf_sand     , wf_gravels   , wf_sand   ,&
           porsl        , csol        , k_solids     , &
           dksatu       , dksatf      , dkdry        , &
           BA_alpha     , BA_beta     , hpbl         , &

           ! "inout" arguments
           ! -------------------
           t_grnd       , scv         , snowdp       , t_soisno  ,&
           wliq_soisno  , wice_soisno , imelt_soisno , t_lake    ,&
           lake_icefrac , savedtke1   , &

! SNICAR model variables
           snofrz       ,sabg_snow_lyr, &
! END SNICAR model variables

           ! "out" arguments
           ! -------------------
           taux         , tauy        , fsena                    ,&
           fevpa        , lfevpa      , fseng        , fevpg     ,&
           qseva        , qsubl       , qsdew        , qfros     ,&
           olrg         , fgrnd       , tref         , qref      ,&
           trad         , emis        , z0m          , zol       ,&
           rib          , ustar       , qstar        , tstar     ,&
           fm           , fh          , fq           , fh2m      ,&
           fq2m         , fm10m       , sm           ,&
           urban_call)

! ------------------------ code history ---------------------------
!  purpose: lake temperature and snow on frozen lake
!  initial  Yongjiu Dai, 2000
!           Zack Subin, 2009
!           Yongjiu Dai, /12/2012/, /04/2014/, 06/2018
!           Nan Wei, /06/2018/
!
! ------------------------ notes ----------------------------------
!  Lakes have variable depth, possible snow layers above, freezing &
!  thawing of lake water, and soil layers with active temperature and
!  gas diffusion below.
!
!  Calculates temperatures in the 25-30 layer column of (possible) snow,
!  lake water, soil, and bedrock beneath lake.  Snow and soil
!  temperatures are determined as in SoilTemperature, except for
!  appropriate boundary conditions at the top of the snow (the flux is
!  fixed to be the ground heat flux), the bottom of the snow (adjacent
!  to top lake layer), and the top of the soil (adjacent to the bottom
!  lake layer).  Also, the soil is kept fully saturated.  The whole
!  column is solved simultaneously as one tridiagonal matrix.
!
!  calculate lake temperatures from one-dimensional thermal
!  stratification model based on eddy diffusion concepts to represent
!  vertical mixing of heat
!
!  d ts    d            d ts     1 ds
!  ---- = -- [(km + ke) ----] + -- --
!   dt    dz             dz     cw dz
!  where: ts = temperature (kelvin)
!          t = time (s)
!          z = depth (m)
!         km = molecular diffusion coefficient (m**2/s)
!         ke = eddy diffusion coefficient (m**2/s)
!         cw = heat capacity (j/m**3/kelvin)
!          s = heat source term (w/m**2)
!
!  use crank-nicholson method to set up tridiagonal system of equations to
!  solve for ts at time n+1, where the temperature equation for layer i is
!  r_i = a_i [ts_i-1] n+1 + b_i [ts_i] n+1 + c_i [ts_i+1] n+1
!  the solution conserves energy as
!  cw*([ts(  1)] n+1 - [ts(  1)] n)*dz(  1)/dt + ... +
!  cw*([ts(nl_lake)] n+1 - [ts(nl_lake)] n)*dz(nl_lake)/dt = fin
!  where
!  [ts] n   = old temperature (kelvin)
!  [ts] n+1 = new temperature (kelvin)
!  fin      = heat flux into lake (w/m**2)
!           = beta*sabg_snow_lyr(1)+forc_frl-olrg-fsena-lfevpa-hm + phi(1) + ... + phi(nl_lake)
!
! !REVISIONS:
!  Yongjiu Dai and Hua Yuan, 01/2023: added SNICAR for layer solar absorption, ground heat
!                                     flux, temperature and freezing mass calculations
!  Shaofeng Liu, 05/2023: add option to call moninobuk_leddy, the LargeEddy
!                         surface turbulence scheme (LZD2022);
!                         make a proper update of um.
!
! -----------------------------------------------------------------
   USE MOD_Precision
   USE MOD_Const_Physical, only: tfrz,hvap,hfus,hsub,tkwat,tkice,tkair,stefnc,&
                                 vonkar,grav,cpliq,cpice,cpair,denh2o,denice,rgas
   USE MOD_FrictionVelocity
   USE MOD_Namelist, only: DEF_USE_CBL_HEIGHT, DEF_USE_SNICAR
   USE MOD_TurbulenceLEddy
   USE MOD_Qsadv
   USE MOD_SoilThermalParameters
   USE MOD_Utils

   IMPLICIT NONE
!-------------------------- Dummy Arguments ----------------------------
   integer, intent(in) :: patchtype    ! land patch type (4=deep lake, 5=shallow lake)
   integer, intent(in) :: maxsnl       ! maximum number of snow layers
   integer, intent(in) :: nl_soil      ! number of soil layers
   integer, intent(in) :: nl_lake      ! number of lake layers

   real(r8), intent(in) :: dlat        ! latitude (radians)
   real(r8), intent(in) :: deltim      ! seconds in a time step (s)
   real(r8), intent(in) :: forc_hgt_u  ! observational height of wind [m]
   real(r8), intent(in) :: forc_hgt_t  ! observational height of temperature [m]
   real(r8), intent(in) :: forc_hgt_q  ! observational height of humidity [m]
   real(r8), intent(in) :: forc_us     ! wind component in eastward direction [m/s]
   real(r8), intent(in) :: forc_vs     ! wind component in northward direction [m/s]
   real(r8), intent(in) :: forc_t      ! temperature at agcm reference height [kelvin]
   real(r8), intent(in) :: forc_q      ! specific humidity at agcm reference height [kg/kg]
   real(r8), intent(in) :: forc_rhoair ! density air [kg/m3]
   real(r8), intent(in) :: forc_psrf   ! atmosphere pressure at the surface [pa]
   real(r8), intent(in) :: forc_sols   ! atm vis direct beam solar rad onto srf [W/m2]
   real(r8), intent(in) :: forc_soll   ! atm nir direct beam solar rad onto srf [W/m2]
   real(r8), intent(in) :: forc_solsd  ! atm vis diffuse solar rad onto srf [W/m2]
   real(r8), intent(in) :: forc_solld  ! atm nir diffuse solar rad onto srf [W/m2]
   real(r8), intent(in) :: forc_frl    ! atmospheric infrared (longwave) radiation [W/m2]
   real(r8), intent(in) :: sabg        ! solar radiation absorbed by ground [W/m2]

   real(r8), intent(in) :: dz_soisno(maxsnl+1:nl_soil) ! soil/snow layer thickness (m)
   real(r8), intent(in) :: z_soisno(maxsnl+1:nl_soil)  ! soil/snow node depth [m]
   real(r8), intent(in) :: zi_soisno(maxsnl:nl_soil)   ! soil/snow depth of layer interface [m]

   real(r8), intent(in) :: dz_lake(nl_lake)      ! lake layer thickness (m)
   real(r8), intent(in) :: lakedepth             ! column lake depth (m)

   real(r8), intent(in) :: vf_quartz (1:nl_soil) ! volumetric fraction of quartz within mineral soil
   real(r8), intent(in) :: vf_gravels(1:nl_soil) ! volumetric fraction of gravels
   real(r8), intent(in) :: vf_om     (1:nl_soil) ! volumetric fraction of organic matter
   real(r8), intent(in) :: vf_sand   (1:nl_soil) ! volumetric fraction of sand
   real(r8), intent(in) :: wf_gravels(1:nl_soil) ! gravimetric fraction of gravels
   real(r8), intent(in) :: wf_sand   (1:nl_soil) ! gravimetric fraction of sand
   real(r8), intent(in) :: porsl(1:nl_soil)      ! soil porosity [-]

   real(r8), intent(in) :: csol(1:nl_soil)       ! heat capacity of soil solids [J/(m3 K)]
   real(r8), intent(in) :: k_solids(1:nl_soil)   ! thermal conductivity of mineralssoil [W/m-K]
   real(r8), intent(in) :: dksatu(1:nl_soil)     ! thermal conductivity of saturated unfrozen soil [W/m-K]
   real(r8), intent(in) :: dksatf(1:nl_soil)     ! thermal conductivity of saturated frozen soil [W/m-K]
   real(r8), intent(in) :: dkdry(1:nl_soil)      ! thermal conductivity of dry soil [W/m-K]
   real(r8), intent(in) :: BA_alpha(1:nl_soil)   ! alpha in Balland and Arp(2005) thermal conductivity scheme
   real(r8), intent(in) :: BA_beta(1:nl_soil)    ! beta in Balland and Arp(2005) thermal conductivity scheme
   real(r8), intent(in) :: hpbl                  ! atmospheric boundary layer height [m]

   real(r8), intent(inout) :: t_grnd             ! surface temperature (kelvin)
   real(r8), intent(inout) :: scv                ! snow water equivalent [mm]
   real(r8), intent(inout) :: snowdp             ! snow depth [mm]

   real(r8), intent(inout) :: t_soisno    (maxsnl+1:nl_soil) ! soil/snow temperature [K]
   real(r8), intent(inout) :: wliq_soisno (maxsnl+1:nl_soil) ! soil/snow liquid water (kg/m2)
   real(r8), intent(inout) :: wice_soisno (maxsnl+1:nl_soil) ! soil/snow ice lens (kg/m2)
   integer,  intent(inout) :: imelt_soisno(maxsnl+1:nl_soil) ! soil/snow flag for melting (=1), freezing (=2), Not=0 (new)

   real(r8), intent(inout) :: t_lake(nl_lake)       ! lake temperature (kelvin)
   real(r8), intent(inout) :: lake_icefrac(nl_lake) ! lake mass fraction of lake layer that is frozen
   real(r8), intent(inout) :: savedtke1             ! top level eddy conductivity (W/m K)

!  SNICAR model variables
   real(r8), intent(out) :: snofrz       (maxsnl+1:0) ! snow freezing rate (col,lyr) [kg m-2 s-1]
   real(r8), intent(in)  :: sabg_snow_lyr(maxsnl+1:1) ! solar radiation absorbed by ground [W/m2]
!  END SNICAR model variables

   real(r8), intent(out) :: taux   ! wind stress: E-W [kg/m/s**2]
   real(r8), intent(out) :: tauy   ! wind stress: N-S [kg/m/s**2]
   real(r8), intent(out) :: fsena  ! sensible heat from canopy height to atmosphere [W/m2]
   real(r8), intent(out) :: fevpa  ! evapotranspiration from canopy height to atmosphere [mm/s]
   real(r8), intent(out) :: lfevpa ! latent heat flux from canopy height to atmosphere [W/m2]

   real(r8), intent(out) :: fseng  ! sensible heat flux from ground [W/m2]
   real(r8), intent(out) :: fevpg  ! evaporation heat flux from ground [mm/s]

   real(r8), intent(out) :: qseva  ! ground surface evaporation rate (mm h2o/s)
   real(r8), intent(out) :: qsubl  ! sublimation rate from snow pack (mm H2O /s) [+]
   real(r8), intent(out) :: qsdew  ! surface dew added to snow pack (mm H2O /s) [+]
   real(r8), intent(out) :: qfros  ! ground surface frosting formation (mm H2O /s) [+]

   real(r8), intent(out) :: olrg   ! outgoing long-wave radiation from ground+canopy
   real(r8), intent(out) :: fgrnd  ! ground heat flux [W/m2]

   real(r8), intent(out) :: tref   ! 2 m height air temperature [kelvin]
   real(r8), intent(out) :: qref   ! 2 m height air specific humidity
   real(r8), intent(out) :: trad   ! radiative temperature [K]

   real(r8), intent(out) :: emis   ! averaged bulk surface emissivity
   real(r8), intent(out) :: z0m    ! effective roughness [m]
   real(r8), intent(out) :: zol    ! dimensionless height (z/L) used in Monin-Obukhov theory
   real(r8), intent(out) :: rib    ! bulk Richardson number in surface layer
   real(r8), intent(out) :: ustar  ! u* in similarity theory [m/s]
   real(r8), intent(out) :: qstar  ! q* in similarity theory [kg/kg]
   real(r8), intent(out) :: tstar  ! t* in similarity theory [K]
   real(r8), intent(out) :: fm     ! integral of profile function for momentum
   real(r8), intent(out) :: fh     ! integral of profile function for heat
   real(r8), intent(out) :: fq     ! integral of profile function for moisture
   real(r8), intent(out) :: fh2m   ! relation for temperature at 2m
   real(r8), intent(out) :: fq2m   ! relation for specific humidity at 2m
   real(r8), intent(out) :: fm10m  ! integral of profile function for momentum at 10m
   real(r8), intent(out) :: sm     ! rate of snowmelt [mm/s, kg/(m2 s)]
   logical, optional, intent(in) :: urban_call   ! whether it is a urban CALL

!-------------------------- Local Variables ----------------------------
!  ---------------- local variables in surface temp and fluxes calculation -----------------
   integer idlak     ! index of lake, 1 = deep lake, 2 = shallow lake
   real(r8) z_lake (nl_lake)  ! lake node depth (middle point of layer) (m)

   real(r8) ax       ! used in iteration loop for calculating t_grnd (numerator of NR solution)
   real(r8) bx       ! used in iteration loop for calculating t_grnd (denomin. of NR solution)
   real(r8) beta1    ! coefficient of conective velocity [-]
   real(r8) degdT    ! d(eg)/dT
   real(r8) displax  ! zero- displacement height [m]
   real(r8) dqh      ! diff of humidity between ref. height and surface
   real(r8) dth      ! diff of virtual temp. between ref. height and surface
   real(r8) dthv     ! diff of vir. poten. temp. between ref. height and surface
   real(r8) dzsur    ! 1/2 the top layer thickness (m)
   real(r8) tsur     ! top layer temperature
   real(r8) rhosnow  ! partial density of water (ice + liquid)
   real(r8) eg       ! water vapor pressure at temperature T [pa]
   real(r8) emg      ! ground emissivity (0.97 for snow,
   real(r8) errore   ! lake temperature energy conservation error (w/m**2)
   real(r8) hm       ! energy residual [W/m2]
   real(r8) htvp     ! latent heat of vapor of water (or sublimation) [j/kg]
   real(r8) obu      ! monin-obukhov length (m)
   real(r8) obuold   ! monin-obukhov length of previous iteration
   real(r8) qsatg    ! saturated humidity [kg/kg]
   real(r8) qsatgdT  ! d(qsatg)/dT

   real(r8) ram      ! aerodynamical resistance [s/m]
   real(r8) rah      ! thermal resistance [s/m]
   real(r8) raw      ! moisture resistance [s/m]
   real(r8) stftg3   ! emg*sb*t_grnd*t_grnd*t_grnd
   real(r8) t_grnd_bef0   ! initial ground temperature
   real(r8) t_grnd_bef    ! initial ground temperature
   real(r8) thm      ! intermediate variable (forc_t+0.0098*forc_hgt_t)
   real(r8) th       ! potential temperature (kelvin)
   real(r8) thv      ! virtual potential temperature (kelvin)
   real(r8) thvstar  ! virtual potential temperature scaling parameter
   real(r8) tksur    ! thermal conductivity of snow/soil (w/m/kelvin)
   real(r8) um       ! wind speed including the stability effect [m/s]
   real(r8) ur       ! wind speed at reference height [m/s]
   real(r8) visa     ! kinematic viscosity of dry air [m2/s]
   real(r8) wc       ! convective velocity [m/s]
   real(r8) wc2      ! wc*wc
   real(r8) zeta     ! dimensionless height used in Monin-Obukhov theory
   real(r8) zii      ! convective boundary height [m]
   real(r8) zldis    ! reference height "minus" zero displacement height [m]
   real(r8) z0mg     ! roughness length over ground, momentum [m]
   real(r8) z0hg     ! roughness length over ground, sensible heat [m]
   real(r8) z0qg     ! roughness length over ground, latent heat [m]

   real(r8) wliq_lake(nl_lake)  ! lake liquid water (kg/m2)
   real(r8) wice_lake(nl_lake)  ! lake ice lens (kg/m2)
   real(r8) vf_water(1:nl_soil) ! volumetric fraction liquid water within underlying soil
   real(r8) vf_ice(1:nl_soil)   ! volumetric fraction ice lens within underlying soil

   real(r8) fgrnd1  ! ground heat flux into the first snow/lake layer [W/m2]

!  ---------------- local variables in lake/snow/soil temperature calculation --------------
   real(r8), parameter :: cur0 = 0.01     ! min. Charnock parameter
   real(r8), parameter :: curm = 0.1      ! maximum Charnock parameter
   real(r8), parameter :: fcrit = 22.     ! critical dimensionless fetch for Charnock parameter (Vickers & Mahrt 1997)
                                          ! but converted to USE u instead of u* (Subin et al. 2011)
   real(r8), parameter :: mixfact = 5.    ! Mixing enhancement factor.
   real(r8), parameter :: depthcrit = 25. ! (m) Depth beneath which to enhance mixing
   real(r8), parameter :: fangmult = 5.   ! Multiplier for unfrozen diffusivity
   real(r8), parameter :: minmultdepth = 20. ! (m) Minimum depth for imposing fangmult
   real(r8), parameter :: cnfac  = 0.5    ! Crank Nicholson factor between 0 and 1

   !--------------------
   real(r8) fetch     ! lake fetch (m)
   real(r8) cur       ! Charnock parameter (-)
   real(r8) betavis   !
   real(r8) betaprime ! Effective beta
   real(r8) tdmax     ! temperature of maximum water density
   real(r8) cfus      ! effective heat of fusion per unit volume
   real(r8) tkice_eff ! effective conductivity since layer depth is constant
   real(r8) cice_eff  ! effective heat capacity of ice (using density of
                      ! water because layer depth is not adjusted when freezing
   real(r8) cwat      ! specific heat capacity of water (j/m**3/kelvin)

   !--------------------
   real(r8) rhow(nl_lake) ! density of water (kg/m**3)
   real(r8) fin           ! heat flux into lake - flux out of lake (w/m**2)
   real(r8) phi(nl_lake)  ! solar radiation absorbed by layer (w/m**2)
   real(r8) phi_soil      ! solar radiation into top soil layer (W/m^2)
   real(r8) phidum        ! temporary value of phi

   integer  imelt_lake(1:nl_lake)       ! lake flag for melting or freezing snow and soil layer [-]
   real(r8) cv_lake(1:nl_lake)          ! heat capacity [J/(m2 K)]
   real(r8) tk_lake(1:nl_lake)          ! thermal conductivity at layer node [W/(m K)]
   real(r8) cv_soisno(maxsnl+1:nl_soil) ! heat capacity of soil/snow [J/(m2 K)]
   real(r8) tk_soisno(maxsnl+1:nl_soil) ! thermal conductivity of soil/snow [W/(m K)] (at interface below, except for j=0)
   real(r8) hcap(1:nl_soil)             ! J/(m3 K)
   real(r8) thk(maxsnl+1:nl_soil)       ! W/(m K)
   real(r8) tktopsoil                   ! thermal conductivity of the top soil layer [W/(m K)]

   real(r8) t_soisno_bef(maxsnl+1:nl_soil) ! beginning soil/snow temp for E cons. check [K]
   real(r8) t_lake_bef(1:nl_lake)          ! beginning lake temp for energy conservation check [K]
   real(r8) wice_soisno_bef(maxsnl+1:0)    ! ice lens [kg/m2]

   real(r8) cvx    (maxsnl+1:nl_lake+nl_soil) ! heat capacity for whole column [J/(m2 K)]
   real(r8) tkix   (maxsnl+1:nl_lake+nl_soil) ! thermal conductivity at layer interfaces for whole column [W/(m K)]
   real(r8) phix   (maxsnl+1:nl_lake+nl_soil) ! solar source term for whole column [W/m**2]
   real(r8) zx     (maxsnl+1:nl_lake+nl_soil) ! interface depth (+ below surface) for whole column [m]
   real(r8) tx     (maxsnl+1:nl_lake+nl_soil) ! temperature of whole column [K]
   real(r8) tx_bef (maxsnl+1:nl_lake+nl_soil) ! beginning lake/snow/soil temp for energy conservation check [K]
   real(r8) factx  (maxsnl+1:nl_lake+nl_soil) ! coefficient used in computing tridiagonal matrix
   real(r8) fnx    (maxsnl+1:nl_lake+nl_soil) ! heat diffusion through the layer interface below [W/m2]
   real(r8) a      (maxsnl+1:nl_lake+nl_soil) ! "a" vector for tridiagonal matrix
   real(r8) b      (maxsnl+1:nl_lake+nl_soil) ! "b" vector for tridiagonal matrix
   real(r8) c      (maxsnl+1:nl_lake+nl_soil) ! "c" vector for tridiagonal matrix
   real(r8) r      (maxsnl+1:nl_lake+nl_soil) ! "r" vector for tridiagonal solution
   real(r8) fn1    (maxsnl+1:nl_lake+nl_soil) ! heat diffusion through the layer interface below [W/m2]
   real(r8) brr    (maxsnl+1:nl_lake+nl_soil) !
   integer  imelt_x(maxsnl+1:nl_lake+nl_soil) ! flag for melting (=1), freezing (=2), Not=0 (new)

   real(r8) dzm       ! used in computing tridiagonal matrix [m]
   real(r8) dzp       ! used in computing tridiagonal matrix [m]
   real(r8) zin       ! depth at top of layer (m)
   real(r8) zout      ! depth at bottom of layer (m)
   real(r8) rsfin     ! relative flux of solar radiation into layer
   real(r8) rsfout    ! relative flux of solar radiation out of layer
   real(r8) eta       ! light extinction coefficient (/m): depends on lake type
   real(r8) za(2)     ! base of surface absorption layer (m): depends on lake type
   !--------------------

   real(r8) hs        ! net ground heat flux into the surface
   real(r8) dhsdT     ! temperature derivative of "hs"
   real(r8) heatavail ! available energy for melting or freezing (J/m^2)
   real(r8) heatrem   ! energy residual or loss after melting or freezing
   real(r8) melt      ! actual melting (+) or freezing (-) [kg/m2]
   real(r8) xmf       ! total per-column latent heat abs. from phase change  (J/m^2)
   !--------------------

   real(r8) ocvts     ! (cwat*(t_lake[n  ])*dz_lake
   real(r8) ncvts     ! (cwat*(t_lake[n+1])*dz_lake
   real(r8) esum1     ! temp for checking energy (J/m^2)
   real(r8) esum2     ! ""
   real(r8) zsum      ! temp for putting ice at the top during convection (m)
   real(r8) errsoi    ! soil/lake energy conservation error (W/m^2)

   real(r8) iceav     ! used in calc aver ice for convectively mixed layers
   real(r8) qav       ! used in calc aver heat content for conv. mixed layers
   real(r8) tav       ! used in aver temp for convectively mixed layers
   real(r8) tav_froz  ! used in aver temp for convectively mixed layers (C)
   real(r8) tav_unfr  ! "
   real(r8) nav       ! used in aver temp for convectively mixed layers

   real(r8) fevpg_lim ! temporary evap_soi limited by top snow layer content [mm/s]
   real(r8) scv_temp  ! temporary h2osno [kg/m^2]
   real(r8) tmp       !
   real(r8) h_fin     !
   real(r8) h_finDT   !
   real(r8) del_T_grnd!
   !real(r8) savedtke1

   integer iter       ! iteration index
   integer convernum  ! number of time when del_T_grnd < 0.01
   integer nmozsgn    ! number of times moz changes sign

!  assign iteration parameters
   integer, parameter :: itmax  = 40   ! maximum number of iteration
   integer, parameter :: itmin  = 6    ! minimum number of iteration
   real(r8),parameter :: delmax = 3.0  ! maximum change in lake temperature [K]
   real(r8),parameter :: dtmin  = 0.01 ! max limit for temperature convergence [K]
   real(r8),parameter :: dlemin = 0.1  ! max limit for energy flux convergence [w/m2]

   !--------------------

   integer nl_sls  ! abs(snl)+nl_lake+nl_soil
   integer snl     ! number of snow layers (minimum -5)
   integer lb      ! lower bound of arrays
   integer jprime  ! j - nl_lake

   integer i,j     ! do loop or array index

! ======================================================================
!*[1] constants and model parameters
! ======================================================================


! constants for lake temperature model
      za = (/0.5, 0.6/)
      cwat = cpliq*denh2o     ! water heat capacity per unit volume
      cice_eff = cpice*denh2o ! use water density because layer depth is not adjusted for freezing
      cfus = hfus*denh2o      ! latent heat per unit volume
      tkice_eff = tkice * denice/denh2o ! effective conductivity since layer depth is constant
      emg = 0.97              ! surface emissivity

! define snow layer on ice lake
      snl = 0
      DO j=maxsnl+1,0
         IF(wliq_soisno(j)+wice_soisno(j)>0.) snl=snl-1
      ENDDO
      lb = snl + 1

! latent heat
      IF (t_grnd > tfrz )THEN
         htvp = hvap
      ELSE
         htvp = hsub
      ENDIF

! define levels
      z_lake(1) = dz_lake(1) / 2.
      DO j = 2, nl_lake
         z_lake(j) = z_lake(j-1) + (dz_lake(j-1) + dz_lake(j))/2.
      ENDDO

! Base on lake depth, assuming that small lakes are likely to be shallower
! Estimate crudely based on lake depth
      IF (z_lake(nl_lake) < 4.) THEN
         idlak = 1
         fetch = 100. ! shallow lake
      ELSE
         idlak = 2
         fetch = 25.*z_lake(nl_lake) ! deep lake
      ENDIF


! ======================================================================
!*[2] pre-processing for the calculation of the surface temperature and fluxes
! ======================================================================

      IF (.not. DEF_USE_SNICAR .or. present(urban_call)) THEN
         IF (snl == 0) THEN
            ! calculate the nir fraction of absorbed solar.
            betaprime = (forc_soll+forc_solld)/max(1.e-5,forc_sols+forc_soll+forc_solsd+forc_solld)
            betavis = 0. ! The fraction of the visible (e.g. vis not nir from atm) sunlight
                         ! absorbed in ~1 m of water (the surface layer za_lake).
                         ! This is roughly the fraction over 700 nm but may depend on the details
                         ! of atmospheric radiative transfer.
                         ! As long as NIR = 700 nm and up, this can be zero.
            betaprime = betaprime + (1.0-betaprime)*betavis
         ELSE
            ! or frozen but no snow layers or
            ! currently ignore the transmission of solar in snow and ice layers
            ! to be updated in the future version
            betaprime = 1.0
         ENDIF

      ELSE
         ! calculate the nir fraction of absorbed solar.
         betaprime = (forc_soll+forc_solld)/max(1.e-5,forc_sols+forc_soll+forc_solsd+forc_solld)
         betavis = 0. ! The fraction of the visible (e.g. vis not nir from atm) sunlight
                      ! absorbed in ~1 m of water (the surface layer za_lake).
                      ! This is roughly the fraction over 700 nm but may depend on the details
                      ! of atmospheric radiative transfer.
                      ! As long as NIR = 700 nm and up, this can be zero.
         betaprime = betaprime + (1.0-betaprime)*betavis
      ENDIF

      CALL qsadv(t_grnd,forc_psrf,eg,degdT,qsatg,qsatgdT)
! potential temperature at the reference height
      beta1=1.       ! -  (in computing W_*)
      zii = 1000.    ! m  (pbl height)
      thm = forc_t + 0.0098*forc_hgt_t  ! intermediate variable equivalent to
                                        ! forc_t*(pgcm/forc_psrf)**(rgas/cpair)
      th  = forc_t*(100000./forc_psrf)**(rgas/cpair) ! potential T
      thv = th*(1.+0.61*forc_q)         ! virtual potential T
      ur  = max(0.1,sqrt(forc_us*forc_us+forc_vs*forc_vs))   ! limit set to 0.1

! Initialization variables
      nmozsgn = 0
      obuold = 0.
      dth   = thm-t_grnd
      dqh   = forc_q-qsatg
      dthv  = dth*(1.+0.61*forc_q)+0.61*th*dqh
      zldis = forc_hgt_u-0.

! Roughness lengths, allow all roughness lengths to be prognostic
      ustar=0.06
      wc=0.5

    ! Kinematic viscosity of dry air (m2/s)- Andreas (1989) CRREL Rep. 89-11
      visa=1.326e-5*(1.+6.542e-3*(forc_t-tfrz) &
           + 8.301e-6*(forc_t-tfrz)**2 - 4.84e-9*(forc_t-tfrz)**3)

      cur = cur0 + curm * exp( max( -(fetch*grav/ur/ur)**(1./3.)/fcrit, & ! Fetch-limited
                                    -(z_lake(nl_lake)*grav)**0.5/ur ) )   ! depth-limited

      IF(dthv.ge.0.) THEN
         um=max(ur,0.1)
      ELSE
         um=sqrt(ur*ur+wc*wc)
      ENDIF

      DO i=1,5
         z0mg=0.013*ustar*ustar/grav+0.11*visa/ustar
         ustar=vonkar*um/log(zldis/z0mg)
      ENDDO

      CALL roughness_lake (snl,t_grnd,t_lake(1),lake_icefrac(1),forc_psrf,&
                           cur,ustar,z0mg,z0hg,z0qg)

      CALL moninobukini(ur,th,thm,thv,dth,dqh,dthv,zldis,z0mg,um,obu)

      IF (snl == 0) THEN
         dzsur = dz_lake(1)/2.
      ELSE
         dzsur = z_soisno(lb)-zi_soisno(lb-1)
      ENDIF


      iter = 1
      del_T_grnd = 1.0    ! t_grnd diff
      convernum = 0       ! number of time when del_T_grnd <= 0.01


! ======================================================================
!*[3] Begin stability iteration and temperature and fluxes calculation
! ======================================================================


    ! =====================================
      ITERATION : DO WHILE (iter <= itmax)
    ! =====================================

         t_grnd_bef = t_grnd

         IF (t_grnd_bef > tfrz .and. t_lake(1) > tfrz .and. snl == 0) THEN
            tksur = savedtke1       !water molecular conductivity
            tsur = t_lake(1)
            htvp = hvap
         ELSEIF (snl == 0) THEN !frozen but no snow layers
            tksur = tkice        ! This is an approximation because the whole layer may not be frozen, and it is not
                                 ! accounting for the physical (but not nominal) expansion of the frozen layer.
            tsur = t_lake(1)
            htvp = hsub
         ELSE
          ! need to calculate thermal conductivity of the top snow layer
            rhosnow = (wice_soisno(lb)+wliq_soisno(lb))/dz_soisno(lb)
            tksur = tkair + (7.75e-5*rhosnow + 1.105e-6*rhosnow*rhosnow)*(tkice-tkair)
            tsur = t_soisno(lb)
            htvp = hsub
         ENDIF

! Evaluated stability-dependent variables using moz from prior iteration
         displax = 0.
         IF (DEF_USE_CBL_HEIGHT) THEN
            CALL moninobuk_leddy(forc_hgt_u,forc_hgt_t,forc_hgt_q,displax,z0mg,z0hg,z0qg,obu,um, hpbl, &
                        ustar,fh2m,fq2m,fm10m,fm,fh,fq)
         ELSE
            CALL moninobuk(forc_hgt_u,forc_hgt_t,forc_hgt_q,displax,z0mg,z0hg,z0qg,obu,um,&
                        ustar,fh2m,fq2m,fm10m,fm,fh,fq)
         ENDIF

! Get derivative of fluxes with respect to ground temperature
         ram    = 1./(ustar*ustar/um)
         rah    = 1./(vonkar/fh*ustar)
         raw    = 1./(vonkar/fq*ustar)
         stftg3 = emg*stefnc*t_grnd_bef*t_grnd_bef*t_grnd_bef

         ax  = betaprime*sabg + emg*forc_frl + 3.*stftg3*t_grnd_bef &
             + forc_rhoair*cpair/rah*thm &
             - htvp*forc_rhoair/raw*(qsatg-qsatgdT*t_grnd_bef - forc_q) &
             + tksur*tsur/dzsur

         bx  = 4.*stftg3 + forc_rhoair*cpair/rah &
             + htvp*forc_rhoair/raw*qsatgdT + tksur/dzsur

         t_grnd = ax/bx

       !-----------------------------------------------------------------
       ! h_fin = betaprime*sabg + emg*forc_frl + 3.*stftg3*t_grnd_bef & !
       !     + forc_rhoair*cpair/rah*thm &                              !
       !     - htvp*forc_rhoair/raw*(qsatg-qsatgdT*t_grnd_bef - forc_q) !
       ! h_finDT = 4.*stftg3 + forc_rhoair*cpair/rah &                  !
       !     + htvp*forc_rhoair/raw*qsatgdT                             !
       ! del_T_grnd = t_grnd - t_grnd_bef                               !
       !----------------------------------------------------------------!

! surface fluxes of momentum, sensible and latent
! using ground temperatures from previous time step

         fseng = forc_rhoair*cpair*(t_grnd-thm)/rah
         fevpg = forc_rhoair*(qsatg+qsatgdT*(t_grnd-t_grnd_bef)-forc_q)/raw

         CALL qsadv(t_grnd,forc_psrf,eg,degdT,qsatg,qsatgdT)
         dth = thm-t_grnd
         dqh = forc_q-qsatg
         tstar = vonkar/fh*dth
         qstar = vonkar/fq*dqh
         thvstar = tstar*(1.+0.61*forc_q)+0.61*th*qstar
         zeta = zldis*vonkar*grav*thvstar/(ustar**2*thv)
         IF(zeta >= 0.) THEN     !stable
            zeta = min(2.,max(zeta,1.e-6))
         ELSE                    !unstable
            zeta = max(-100.,min(zeta,-1.e-6))
         ENDIF
         obu = zldis/zeta
         IF(zeta >= 0.)THEN
            um = max(ur,0.1)
         ELSE
            IF (DEF_USE_CBL_HEIGHT) THEN !//TODO: Shaofeng, 2023.05.18
               zii = max(5.*forc_hgt_u,hpbl)
            ENDIF !//TODO: Shaofeng, 2023.05.18
            wc = (-grav*ustar*thvstar*zii/thv)**(1./3.)
            wc2 = beta1*beta1*(wc*wc)
            um = sqrt(ur*ur+wc2)
         ENDIF

         CALL roughness_lake (snl,t_grnd,t_lake(1),lake_icefrac(1),forc_psrf,&
                              cur,ustar,z0mg,z0hg,z0qg)

         iter = iter + 1
         del_T_grnd = abs(t_grnd - t_grnd_bef)

         IF(iter .gt. itmin) THEN
            IF(del_T_grnd <= dtmin) THEN
               convernum = convernum + 1
            ENDIF
            IF(convernum >= 4) EXIT
         ENDIF

    ! ===============================================
      ENDDO ITERATION   ! end of stability iteration
    ! ===============================================

!*----------------------------------------------------------------------
!*Zack Subin, 3/27/09
!*Since they are now a function of whatever t_grnd was before cooling
!*to freezing temperature, then this value should be used in the derivative correction term.
!*Allow convection if ground temp is colder than lake but warmer than 4C, or warmer than
!*lake which is warmer than freezing but less than 4C.
      tdmax = tfrz + 4.0
      IF ( (snl < 0 .or. t_lake(1) <= tfrz) .and. t_grnd > tfrz) THEN
         t_grnd_bef = t_grnd
         t_grnd = tfrz
         fseng = forc_rhoair*cpair*(t_grnd-thm)/rah
         fevpg = forc_rhoair*(qsatg+qsatgdT*(t_grnd-t_grnd_bef)-forc_q)/raw
      ELSEIF ( (t_lake(1) > t_grnd .and. t_grnd > tdmax) .or. &
              (t_lake(1) < t_grnd .and. t_lake(1) > tfrz .and. t_grnd < tdmax) ) THEN
              ! Convective mixing will occur at surface
         t_grnd_bef = t_grnd
         t_grnd = t_lake(1)
         fseng = forc_rhoair*cpair*(t_grnd-thm)/rah
         fevpg = forc_rhoair*(qsatg+qsatgdT*(t_grnd-t_grnd_bef)-forc_q)/raw
      ENDIF
!*----------------------------------------------------------------------

! net longwave from ground to atmosphere
      stftg3 = emg*stefnc*t_grnd_bef*t_grnd_bef*t_grnd_bef
      olrg = (1.-emg)*forc_frl + emg*stefnc*t_grnd_bef**4 + 4.*stftg3*(t_grnd - t_grnd_bef)
      IF (t_grnd > tfrz )THEN
         htvp = hvap
      ELSE
         htvp = hsub
      ENDIF

!The actual heat flux from the ground interface into the lake, not including the light that penetrates the surface.
      fgrnd1 = betaprime*sabg + forc_frl - olrg - fseng - htvp*fevpg

      ! January 12, 2023 by Yongjiu Dai
      IF (DEF_USE_SNICAR .and. .not. present(urban_call)) THEN
         hs = sabg_snow_lyr(lb) + forc_frl - olrg - fseng - htvp*fevpg
         dhsdT = 0.0
      ENDIF

!------------------------------------------------------------
! Set up vector r and vectors a, b, c that define tridiagonal matrix
! snow and lake and soil layer temperature
!------------------------------------------------------------

!------------------------------------------------------------
! Lake density
!------------------------------------------------------------

      DO j = 1, nl_lake
         rhow(j) = (1.-lake_icefrac(j))*denh2o*(1.0-1.9549e-05*(abs(t_lake(j)-277.))**1.68) &
                     + lake_icefrac(j)*denice
         ! allow for ice fraction; assume constant ice density.
         ! this is not the correct average-weighting but that's OK because the density will only
         ! be used for convection for lakes with ice, and the ice fraction will dominate the
         ! density differences between layers.
         ! using this average will make sure that surface ice is treated properly during
         ! convective mixing.
      ENDDO

!------------------------------------------------------------
! Diffusivity and implied thermal "conductivity" = diffusivity * cwat
!------------------------------------------------------------

      DO j = 1, nl_lake
         cv_lake(j) = dz_lake(j) * (cwat*(1.-lake_icefrac(j)) + cice_eff*lake_icefrac(j))
      ENDDO

      CALL hConductivity_lake(nl_lake,snl,t_grnd,&
                              z_lake,t_lake,lake_icefrac,rhow,&
                              dlat,ustar,z0mg,lakedepth,depthcrit,tk_lake,savedtke1)

!------------------------------------------------------------
! Set the thermal properties of the snow above frozen lake and underlying soil
! and check initial energy content.
!------------------------------------------------------------

      lb = snl+1
      DO i = 1, nl_soil
         vf_water(i) = wliq_soisno(i)/(dz_soisno(i)*denh2o)
         vf_ice(i) = wice_soisno(i)/(dz_soisno(i)*denice)
         CALL soil_hcap_cond(vf_gravels(i),vf_om(i),vf_sand(i),porsl(i),&
                             wf_gravels(i),wf_sand(i),k_solids(i),&
                             csol(i),dkdry(i),dksatu(i),dksatf(i),&
                             BA_alpha(i),BA_beta(i),&
                             t_soisno(i),vf_water(i),vf_ice(i),hcap(i),thk(i))
         cv_soisno(i) = hcap(i)*dz_soisno(i)
      ENDDO

! Snow heat capacity and conductivity
      IF(lb <=0 )THEN
         DO j = lb, 0
            cv_soisno(j) = cpliq*wliq_soisno(j) + cpice*wice_soisno(j)
            rhosnow = (wice_soisno(j)+wliq_soisno(j))/dz_soisno(j)
            thk(j) = tkair + (7.75e-5*rhosnow + 1.105e-6*rhosnow*rhosnow)*(tkice-tkair)
         ENDDO
      ENDIF

! Thermal conductivity at the layer interface
      DO i = lb, nl_soil-1

! the following consideration is try to avoid the snow conductivity
! to be dominant in the thermal conductivity of the interface.
! Because when the distance of bottom snow node to the interface
! is larger than that of interface to top soil node,
! the snow thermal conductivity will be dominant, and the result is that
! lees heat transfer between snow and soil

! modified by Nan Wei, 08/25/2014
         IF (i /= 0) THEN
            tk_soisno(i) = thk(i)*thk(i+1)*(z_soisno(i+1)-z_soisno(i)) &
                  /(thk(i)*(z_soisno(i+1)-zi_soisno(i))+thk(i+1)*(zi_soisno(i)-z_soisno(i)))
         ELSE
            tk_soisno(i) = thk(i)
         ENDIF
      ENDDO
      tk_soisno(nl_soil) = 0.
      tktopsoil = thk(1)

! Sum cv_lake*t_lake for energy check
! Include latent heat term, and use tfrz as reference temperature
! to prevent abrupt change in heat content due to changing heat capacity with phase change.

      ! This will need to be over all soil / lake / snow layers. Lake is below.
      ocvts = 0.
      DO j = 1, nl_lake
         ocvts = ocvts + cv_lake(j)*(t_lake(j)-tfrz) + cfus*dz_lake(j)*(1.-lake_icefrac(j))
      ENDDO

      ! Now DO for soil / snow layers
      DO j = lb, nl_soil
         ocvts = ocvts + cv_soisno(j)*(t_soisno(j)-tfrz) + hfus*wliq_soisno(j)
         IF (j == 1 .and. scv > 0. .and. j == lb) THEN
            ocvts = ocvts - scv*hfus
         ENDIF
      ENDDO

      ! Set up solar source terms (phix)
      ! Modified January 12, 2023 by Yongjiu Dai
      IF (.not. DEF_USE_SNICAR .or. present(urban_call)) THEN
         IF ((t_grnd > tfrz .and. t_lake(1) > tfrz .and. snl == 0)) THEN      !no snow cover, unfrozen layer lakes
            DO j = 1, nl_lake
               ! extinction coefficient from surface data (1/m), if no eta from surface data,
               ! set eta, the extinction coefficient, according to L Hakanson, Aquatic Sciences, 1995
               ! (regression of secchi depth with lake depth for small glacial basin lakes), and the
               ! Poole & Atkins expression for extinction coefficient of 1.7 / secchi Depth (m).

               eta = 1.1925*max(lakedepth,1.)**(-0.424)
               zin  = z_lake(j) - 0.5*dz_lake(j)
               zout = z_lake(j) + 0.5*dz_lake(j)
               rsfin  = exp( -eta*max(  zin-za(idlak),0. ) )  ! the radiation within surface layer (z<za)
               rsfout = exp( -eta*max( zout-za(idlak),0. ) )  ! is considered fixed at (1-beta)*sabg
                                                              ! i.e, max(z-za, 0)
               ! Let rsfout for bottom layer go into soil.
               ! This looks like it should be robust even for pathological cases,
               ! like lakes thinner than za(idlak).

               phi(j) = (rsfin-rsfout) * sabg * (1.-betaprime)
               IF (j == nl_lake) phi_soil = rsfout * sabg * (1.-betaprime)
            ENDDO
         ELSEIF (snl == 0) THEN     !no snow-covered layers, but partially frozen
             phi(1) = sabg * (1.-betaprime)
             phi(2:nl_lake) = 0.
             phi_soil = 0.
         ELSE   ! snow covered, this should be improved upon; Mironov 2002 suggests that SW can penetrate thin ice and may
               ! cause spring convection.
            phi(:) = 0.
            phi_soil = 0.
         ENDIF

      ELSE

         DO j = 1, nl_lake
            ! extinction coefficient from surface data (1/m), if no eta from surface data,
            ! set eta, the extinction coefficient, according to L Hakanson, Aquatic Sciences, 1995
            ! (regression of secchi depth with lake depth for small glacial basin lakes), and the
            ! Poole & Atkins expression for extinction coeffient of 1.7 / secchi Depth (m).

            eta = 1.1925*max(lakedepth,1.)**(-0.424)
            zin  = z_lake(j) - 0.5*dz_lake(j)
            zout = z_lake(j) + 0.5*dz_lake(j)
            rsfin  = exp( -eta*max(  zin-za(idlak),0. ) )  ! the radiation within surface layer (z<za)
            rsfout = exp( -eta*max( zout-za(idlak),0. ) )  ! is considered fixed at (1-beta)*sabg
                                                           ! i.e, max(z-za, 0)
            ! Let rsfout for bottom layer go into soil.
            ! This looks like it should be robust even for pathological cases,
            ! like lakes thinner than za(idlak).

            phi(j) = (rsfin-rsfout) * sabg_snow_lyr(1) * (1.-betaprime)
            IF (j == nl_lake) phi_soil = rsfout * sabg_snow_lyr(1) * (1.-betaprime)
         ENDDO
      ENDIF

      phix(:) = 0.
      phix(1:nl_lake) = phi(1:nl_lake)         !lake layer
      phix(nl_lake+1) = phi_soil               !top soil layer

      ! Set up interface depths(zx), and temperatures (tx).

      DO j = lb, nl_lake+nl_soil
         jprime = j - nl_lake
         IF (j <= 0) THEN                      !snow layer
            zx(j) = z_soisno(j)
            tx(j) = t_soisno(j)
         ELSEIF (j <= nl_lake) THEN           !lake layer
            zx(j) = z_lake(j)
            tx(j) = t_lake(j)
         ELSE                                  !soil layer
            zx(j) = z_lake(nl_lake) + dz_lake(nl_lake)/2. + z_soisno(jprime)
            tx(j) = t_soisno(jprime)
         ENDIF
      ENDDO

      tx_bef = tx


! Heat capacity and resistance of snow without snow layers (<1cm) is ignored during diffusion,
! but its capacity to absorb latent heat may be used during phase change.

      DO j = lb, nl_lake+nl_soil
         jprime = j - nl_lake

         ! heat capacity [J/(m2 K)]
         IF (j <= 0) THEN                      !snow layer
            cvx(j) = cv_soisno(j)
         ELSEIF (j <= nl_lake) THEN           !lake layer
            cvx(j) = cv_lake(j)
         ELSE                                  !soil layer
            cvx(j) = cv_soisno(jprime)
         ENDIF

! Determine interface thermal conductivities at layer interfaces [W/(m K)]

         IF (j < 0) THEN                       !non-bottom snow layer
            tkix(j) = tk_soisno(j)
         ELSEIF (j == 0) THEN                 !bottom snow layer
            dzp = zx(j+1) - zx(j)
            tkix(j) = tk_lake(1)*tk_soisno(j)*dzp &
                    /(tk_soisno(j)*z_lake(1) + tk_lake(1)*(-zx(j)))
                               ! tk_soisno(0) is the conductivity at the middle of that layer
         ELSEIF (j < nl_lake) THEN            !non-bottom lake layer
            tkix(j) = (tk_lake(j)*tk_lake(j+1) * (dz_lake(j+1)+dz_lake(j))) &
                    / (tk_lake(j)*dz_lake(j+1) + tk_lake(j+1)*dz_lake(j))
         ELSEIF (j == nl_lake) THEN           !bottom lake layer
            dzp = zx(j+1) - zx(j)
            tkix(j) = (tktopsoil*tk_lake(j)*dzp &
                    / (tktopsoil*dz_lake(j)/2. + tk_lake(j)*z_soisno(1)))
         ELSE !soil layer
            tkix(j) = tk_soisno(jprime)
         ENDIF

      ENDDO

! Determine heat diffusion through the layer interface and factor used in computing
! tridiagonal matrix and set up vector r and vectors a, b, c that define tridiagonal
! matrix and solve system

      DO j = lb, nl_lake+nl_soil
         factx(j) = deltim/cvx(j)
         IF (j < nl_lake+nl_soil) THEN         !top or interior layer
            fnx(j) = tkix(j)*(tx(j+1)-tx(j))/(zx(j+1)-zx(j))
         ELSE                                  !bottom soil layer
            fnx(j) = 0. !not used
         ENDIF
      ENDDO

      IF (DEF_USE_SNICAR .and. .not. present(urban_call)) THEN
         IF (lb <= 0) THEN                        ! snow covered
            DO j = lb, 1
               IF (j == lb) THEN                  ! top snow layer
                  dzp  = zx(j+1)-zx(j)
                  a(j) = 0.0
                  b(j) = 1. + (1.-cnfac)*factx(j)*tkix(j)/dzp
                  c(j) = -(1.-cnfac)*factx(j)* tkix(j)/dzp
                  r(j) = tx_bef(j)+ factx(j)*(hs - dhsdT*tx_bef(j) + cnfac*fnx(j))
               ELSEIF (j <= 0) THEN              ! non-top snow layers
                  dzm  = (zx(j)-zx(j-1))
                  dzp  = (zx(j+1)-zx(j))
                  a(j) =   - (1.-cnfac)*factx(j)* tkix(j-1)/dzm
                  b(j) = 1.+ (1.-cnfac)*factx(j)*(tkix(j)/dzp + tkix(j-1)/dzm)
                  c(j) =   - (1.-cnfac)*factx(j)* tkix(j)/dzp
                  r(j) = tx_bef(j) + cnfac*factx(j)*(fnx(j) - fnx(j-1)) + factx(j)*sabg_snow_lyr(j)
               ELSE                               ! snow covered top lake layer
                  dzm  = (zx(j)-zx(j-1))
                  dzp  = (zx(j+1)-zx(j))
                  a(j) =   - (1.-cnfac)*factx(j)* tkix(j-1)/dzm
                  b(j) = 1.+ (1.-cnfac)*factx(j)*(tkix(j)/dzp + tkix(j-1)/dzm)
                  c(j) =   - (1.-cnfac)*factx(j)* tkix(j)/dzp
                  r(j) = tx_bef(j) + cnfac*factx(j)*(fnx(j) - fnx(j-1)) + factx(j)*(phix(j) + betaprime*sabg_snow_lyr(j))
               ENDIF
            ENDDO
         ELSE
            j = 1                                 ! no snow covered top lake layer
            dzp  = zx(j+1)-zx(j)
            a(j) = 0.0
            b(j) = 1. + (1.-cnfac)*factx(j)*tkix(j)/dzp
            c(j) = -(1.-cnfac)*factx(j)* tkix(j)/dzp
            r(j) = tx_bef(j)+ factx(j)*(cnfac*fnx(j)+ phix(j)+fgrnd1)
         ENDIF

         DO j = 2, nl_lake+nl_soil
            IF (j < nl_lake+nl_soil) THEN         ! middle lake and soil layers
               dzm  = (zx(j)-zx(j-1))
               dzp  = (zx(j+1)-zx(j))
               a(j) =   - (1.-cnfac)*factx(j)* tkix(j-1)/dzm
               b(j) = 1.+ (1.-cnfac)*factx(j)*(tkix(j)/dzp + tkix(j-1)/dzm)
               c(j) =   - (1.-cnfac)*factx(j)* tkix(j)/dzp
               r(j) = tx_bef(j) + cnfac*factx(j)*(fnx(j) - fnx(j-1)) + factx(j)*phix(j)
            ELSE                                  ! bottom soil layer
               dzm  = (zx(j)-zx(j-1))
               a(j) =   - (1.-cnfac)*factx(j)*tkix(j-1)/dzm
               b(j) = 1.+ (1.-cnfac)*factx(j)*tkix(j-1)/dzm
               c(j) = 0.
               r(j) = tx_bef(j) - cnfac*factx(j)*fnx(j-1)
            ENDIF
         ENDDO
      ! January 12, 2023

      ELSE
         DO j = lb, nl_lake+nl_soil
            IF (j == lb) THEN                     ! top layer
                dzp  = zx(j+1)-zx(j)
                a(j) = 0.0
                b(j) = 1. + (1.-cnfac)*factx(j)*tkix(j)/dzp
                c(j) = -(1.-cnfac)*factx(j)* tkix(j)/dzp
                r(j) = tx_bef(j)+ factx(j)*(cnfac*fnx(j)+ phix(j)+fgrnd1)
            ELSEIF (j < nl_lake+nl_soil) THEN    ! middle layer
               dzm  = (zx(j)-zx(j-1))
               dzp  = (zx(j+1)-zx(j))
               a(j) =   - (1.-cnfac)*factx(j)* tkix(j-1)/dzm
               b(j) = 1.+ (1.-cnfac)*factx(j)*(tkix(j)/dzp + tkix(j-1)/dzm)
               c(j) =   - (1.-cnfac)*factx(j)* tkix(j)/dzp
               r(j) = tx_bef(j) + cnfac*factx(j)*(fnx(j) - fnx(j-1)) + factx(j)*phix(j)
            ELSE                                  ! bottom soil layer
               dzm  = (zx(j)-zx(j-1))
               a(j) =   - (1.-cnfac)*factx(j)*tkix(j-1)/dzm
               b(j) = 1.+ (1.-cnfac)*factx(j)*tkix(j-1)/dzm
               c(j) = 0.
               r(j) = tx_bef(j) - cnfac*factx(j)*fnx(j-1)
            ENDIF
         ENDDO
      ENDIF

!------------------------------------------------------------
! Solve for tdsolution
!------------------------------------------------------------

      nl_sls = abs(snl) + nl_lake + nl_soil

      CALL tridia (nl_sls, a(lb:), b(lb:), c(lb:), r(lb:), tx(lb:))


      DO j = lb, nl_lake + nl_soil
         jprime = j - nl_lake
         IF (j < 1) THEN               ! snow layer
            t_soisno(j) = tx(j)
         ELSEIF (j <= nl_lake) THEN   ! lake layer
            t_lake(j) = tx(j)
         ELSE                          ! soil layer
            t_soisno(jprime) = tx(j)
         ENDIF
      ENDDO

! additional variables for CWRF or other atmospheric models
      emis = emg
      z0m  = z0mg
      zol  = zeta
      rib  = min(5.,zol*ustar**2/(vonkar*vonkar/fh*um**2))

! radiative temperature
      trad = (olrg/stefnc)**0.25

! solar absorption below the surface.
      fgrnd = sabg + forc_frl - olrg - fseng - htvp*fevpg
      taux = -forc_rhoair*forc_us/ram
      tauy = -forc_rhoair*forc_vs/ram
      fsena = fseng
      fevpa = fevpg
      lfevpa = htvp*fevpg

! 2 m height air temperature and specific humidity
      tref = thm + vonkar/fh*dth * (fh2m/vonkar - fh/vonkar)
      qref = forc_q + vonkar/fq*dqh * (fq2m/vonkar - fq/vonkar)

! calculate sublimation, frosting, dewing
      qseva = 0.
      qsubl = 0.
      qsdew = 0.
      qfros = 0.

      IF (fevpg >= 0.0) THEN
         IF(lb <= 0)THEN
            qseva = min(wliq_soisno(lb)/deltim, fevpg)
            qsubl = fevpg - qseva
         ELSE
            qseva = min((1.-lake_icefrac(1))*1000.*dz_lake(1)/deltim, fevpg)
            qsubl = fevpg - qseva
         ENDIF
      ELSE
         IF (t_grnd < tfrz) THEN
            qfros = abs(fevpg)
         ELSE
            qsdew = abs(fevpg)
         ENDIF
      ENDIF


#if (defined CoLMDEBUG)
      ! sum energy content and total energy into lake for energy check. any errors will be from the
      !     tridiagonal solution.
      esum1 = 0.0
      esum2 = 0.0
      DO j = lb, nl_lake + nl_soil
         esum1 = esum1 + (tx(j)-tx_bef(j))*cvx(j)
         esum2 = esum2 + (tx(j)-tfrz)*cvx(j)
      ENDDO
                           ! fgrnd includes all the solar radiation absorbed in the lake,
      errsoi = esum1/deltim - fgrnd
      IF(abs(errsoi) > 0.1) THEN
         write(6,*)'energy conservation error in LAND WATER COLUMN during tridiagonal solution,', &
                   'error (W/m^2):', errsoi, fgrnd
      ENDIF
#endif


!------------------------------------------------------------
!*[4] Phase change
!------------------------------------------------------------

      sm = 0.0
      xmf = 0.0
      imelt_soisno(:) = 0
      imelt_lake(:) = 0

      IF (DEF_USE_SNICAR .and. .not. present(urban_call)) THEN
         wice_soisno_bef(lb:0) = wice_soisno(lb:0)
      ENDIF

      ! Check for case of snow without snow layers and top lake layer temp above freezing.

      IF (snl == 0 .and. scv > 0. .and. t_lake(1) > tfrz) THEN
         heatavail = (t_lake(1) - tfrz) * cv_lake(1)
         melt = min(scv, heatavail/hfus)
         heatrem = max(heatavail - melt*hfus, 0.) !catch small negative value to keep t at tfrz
         t_lake(1) = tfrz + heatrem/(cv_lake(1))

         snowdp = max(0., snowdp*(1. - melt/scv))
         scv = scv - melt

         IF (scv < 1.e-12) scv = 0.        ! prevent tiny residuals
         IF (snowdp < 1.e-12) snowdp = 0.  ! prevent tiny residuals
         sm = sm + melt/deltim
         xmf = xmf + melt*hfus
      ENDIF

      ! Lake phase change
      DO j = 1,nl_lake
         IF (t_lake(j) > tfrz .and. lake_icefrac(j) > 0.) THEN ! melting
            imelt_lake(j) = 1
            heatavail = (t_lake(j) - tfrz) * cv_lake(j)
            melt = min(lake_icefrac(j)*denh2o*dz_lake(j), heatavail/hfus)
                       !denh2o is used because layer thickness is not adjusted for freezing
            heatrem = max(heatavail - melt*hfus, 0.)  !catch small negative value to keep t at tfrz
         ELSEIF (t_lake(j) < tfrz .and. lake_icefrac(j) < 1.) THEN !freezing
            imelt_lake(j) = 2
            heatavail = (t_lake(j) - tfrz) * cv_lake(j)
            melt = max(-(1.-lake_icefrac(j))*denh2o*dz_lake(j), heatavail/hfus)
                       !denh2o is used because layer thickness is not adjusted for freezing
            heatrem = min(heatavail - melt*hfus, 0.)  !catch small positive value to keep t at tfrz
         ENDIF
         ! Update temperature and ice fraction.
         IF (imelt_lake(j) > 0) THEN
            lake_icefrac(j) = lake_icefrac(j) - melt/(denh2o*dz_lake(j))
            IF (lake_icefrac(j) > 1.-1.e-12) lake_icefrac(j) = 1.  ! prevent tiny residuals
            IF (lake_icefrac(j) < 1.e-12)    lake_icefrac(j) = 0.  ! prevent tiny residuals
            cv_lake(j) = cv_lake(j) + melt*(cpliq-cpice)           ! update heat capacity
            t_lake(j) = tfrz + heatrem/cv_lake(j)
            xmf = xmf + melt*hfus
         ENDIF
      ENDDO

      ! snow & soil phase change. currently, does not DO freezing point depression.
      DO j = snl+1,nl_soil
         IF (t_soisno(j) > tfrz .and. wice_soisno(j) > 0.) THEN ! melting
            imelt_soisno(j) = 1
            heatavail = (t_soisno(j) - tfrz) * cv_soisno(j)
            melt = min(wice_soisno(j), heatavail/hfus)
            heatrem = max(heatavail - melt*hfus, 0.) !catch small negative value to keep t at tfrz
            IF (j <= 0) sm = sm + melt/deltim
         ELSEIF (t_soisno(j) < tfrz .and. wliq_soisno(j) > 0.) THEN !freezing
            imelt_soisno(j) = 2
            heatavail = (t_soisno(j) - tfrz) * cv_soisno(j)
            melt = max(-wliq_soisno(j), heatavail/hfus)
            heatrem = min(heatavail - melt*hfus, 0.) !catch small positive value to keep t at tfrz
         ENDIF

         ! Update temperature and soil components.
         IF (imelt_soisno(j) > 0) THEN
            wice_soisno(j) = wice_soisno(j) - melt
            wliq_soisno(j) = wliq_soisno(j) + melt
            IF (wice_soisno(j) < 1.e-12) wice_soisno(j) = 0. ! prevent tiny residuals
            IF (wliq_soisno(j) < 1.e-12) wliq_soisno(j) = 0. ! prevent tiny residuals
            cv_soisno(j) = cv_soisno(j) + melt*(cpliq-cpice) ! update heat capacity
            t_soisno(j) = tfrz + heatrem/cv_soisno(j)
            xmf = xmf + melt*hfus
         ENDIF
      ENDDO
      !------------------------------------------------------------

      IF (DEF_USE_SNICAR .and. .not. present(urban_call)) THEN
         !for SNICAR: layer freezing mass flux (positive):
         DO j = lb, 0
            IF (imelt_soisno(j)==2 .and. j<1) THEN
               snofrz(j) = max(0._r8,(wice_soisno(j)-wice_soisno_bef(j)))/deltim
            ENDIF
         ENDDO
      ENDIF

#if (defined CoLMDEBUG)
      ! second energy check and water check. now check energy balance before and after phase
      ! change, considering the possibility of changed heat capacity during phase change, by
      ! using initial heat capacity in the first step, final heat capacity in the second step,
      ! and differences from tfrz only to avoid enthalpy correction for (cpliq-cpice)*melt*tfrz.
      ! also check soil water sum.
      DO j = 1, nl_lake
         esum2 = esum2 - (t_lake(j)-tfrz)*cv_lake(j)
      ENDDO

      DO j = lb, nl_soil
         esum2 = esum2 - (t_soisno(j)-tfrz)*cv_soisno(j)
      ENDDO

      esum2 = esum2 - xmf
      errsoi = esum2/deltim

      IF(abs(errsoi) > 0.1) THEN
      write(6,*) 'energy conservation error in LAND WATER COLUMN during phase change, error (W/m^2):', errsoi
      ENDIF

#endif

!------------------------------------------------------------
!*[5] Convective mixing: make sure fracice*dz is conserved, heat content c*dz*T is conserved, and
! all ice ends up at the top. Done over all lakes even IF frozen.
! Either an unstable density profile or ice in a layer below an incompletely frozen layer will trigger.
!------------------------------------------------------------

      ! recalculate density
      DO j = 1, nl_lake
         rhow(j) = (1.-lake_icefrac(j))*1000.*(1.0-1.9549e-05*(abs(t_lake(j)-277.))**1.68) &
                     + lake_icefrac(j)*denice
      ENDDO

      DO j = 1, nl_lake-1
         qav = 0.
         nav = 0.
         iceav = 0.

         IF (rhow(j)>rhow(j+1) .or. (lake_icefrac(j)<1.0 .and. lake_icefrac(j+1)>0.)) THEN
            DO i = 1, j+1
               qav = qav + dz_lake(i)*(t_lake(i)-tfrz) * &
                       ((1. - lake_icefrac(i))*cwat + lake_icefrac(i)*cice_eff)
               iceav = iceav + lake_icefrac(i)*dz_lake(i)
               nav = nav + dz_lake(i)
            ENDDO

            qav = qav/nav
            iceav = iceav/nav
            !IF the average temperature is above freezing, put the extra energy into the water.
            !IF it is below freezing, take it away from the ice.
            IF (qav > 0.) THEN
               tav_froz = 0. !Celsius
               tav_unfr = qav / ((1. - iceav)*cwat)
            ELSEIF (qav < 0.) THEN
               tav_froz = qav / (iceav*cice_eff)
               tav_unfr = 0. !Celsius
            ELSE
               tav_froz = 0.
               tav_unfr = 0.
            ENDIF
         ENDIF

         IF (nav > 0.) THEN
            DO i = 1, j+1

            !put all the ice at the top.
            !if the average temperature is above freezing, put the extra energy into the water.
            !if it is below freezing, take it away from the ice.
            !for the layer with both ice & water, be careful to use the average temperature
            !that preserves the correct total heat content given what the heat capacity of that
            !layer will actually be.

               IF (i == 1) zsum = 0.
               IF ((zsum+dz_lake(i))/nav <= iceav) THEN
                  lake_icefrac(i) = 1.
                  t_lake(i) = tav_froz + tfrz
               ELSEIF (zsum/nav < iceav) THEN
                  lake_icefrac(i) = (iceav*nav - zsum) / dz_lake(i)
                  ! Find average value that preserves correct heat content.
                  t_lake(i) = ( lake_icefrac(i)*tav_froz*cice_eff &
                              + (1. - lake_icefrac(i))*tav_unfr*cwat ) &
                              / ( lake_icefrac(i)*cice_eff + (1-lake_icefrac(i))*cwat ) + tfrz
               ELSE
                  lake_icefrac(i) = 0.
                  t_lake(i) = tav_unfr + tfrz
               ENDIF
               zsum = zsum + dz_lake(i)

               rhow(i) = (1.-lake_icefrac(i))*1000.*(1.-1.9549e-05*(abs(t_lake(i)-277.))**1.68) &
                           + lake_icefrac(i)*denice
            ENDDO
         ENDIF
      ENDDO

!------------------------------------------------------------
!*[6] Re-evaluate thermal properties and sum energy content.
!------------------------------------------------------------
      ! for lake
      DO j = 1, nl_lake
         cv_lake(j) = dz_lake(j) * (cwat*(1.-lake_icefrac(j)) + cice_eff*lake_icefrac(j))
      ENDDO

      ! do as above to sum energy content
      ncvts = 0.
      DO j = 1, nl_lake
         ncvts = ncvts + cv_lake(j)*(t_lake(j)-tfrz) + cfus*dz_lake(j)*(1.-lake_icefrac(j))
      ENDDO

      DO j = lb, nl_soil
         ncvts = ncvts + cv_soisno(j)*(t_soisno(j)-tfrz) + hfus*wliq_soisno(j)
         IF (j == 1 .and. scv > 0. .and. j == lb) THEN
            ncvts = ncvts - scv*hfus
         ENDIF
      ENDDO

      ! check energy conservation.
      errsoi = (ncvts-ocvts)/deltim - fgrnd
      IF (abs(errsoi) < 0.10) THEN
         fseng = fseng - errsoi
         fsena = fseng
         fgrnd = fgrnd + errsoi
         errsoi = 0.
      ELSE
         print*, "energy conservation error in LAND WATER COLUMN during convective mixing", errsoi,fgrnd,ncvts,ocvts
      ENDIF


   END SUBROUTINE laketem



   SUBROUTINE snowwater_lake ( USE_Dynamic_Lake, &
             ! "in" arguments
             ! ---------------------------
             maxsnl      , nl_soil     , nl_lake   , deltim       ,&
             ssi         , wimp        , porsl     , pg_rain      ,&
             pg_snow     , dz_lake     , imelt     , fiold        ,&
             qseva       , qsubl       , qsdew     , qfros        ,&

             ! "inout" arguments
             ! ---------------------------
             z_soisno    , dz_soisno   , zi_soisno , t_soisno     ,&
             wice_soisno , wliq_soisno , t_lake    , lake_icefrac ,&
             qout_snowb  ,                                         &
             fseng       , fgrnd       , snl       , scv          ,&
             snowdp      , sm          , forc_us   , forc_vs      ,&
! SNICAR model variables
             forc_aer    ,&
             mss_bcpho   , mss_bcphi   , mss_ocpho , mss_ocphi    ,&
             mss_dst1    , mss_dst2    , mss_dst3  , mss_dst4     ,&
! END SNICAR model variables
             urban_call  )

!-----------------------------------------------------------------------------------------------
!  Calculation of Lake Hydrology. Lake water mass is kept constant. The
!  soil is simply maintained at volumetric saturation if ice melting
!  frees up pore space.
!
!  Called:
!     -> snowwater:                  change of snow mass and snow water onto soil
!     -> snowcompaction:             compaction of snow layers
!     -> combinesnowlayers:          combine snow layers that are thinner than minimum
!     -> dividesnowlayers:           subdivide snow layers that are thicker than maximum
!
!  Initial: Yongjiu Dai, December, 2012
!                           April, 2014
! !REVISIONS:
!  Nan Wei, 06/2018: update snow hydrology above lake
!  Yongjiu Dai, 01/2023: added for SNICAR model effects for snowwater,
!  combinesnowlayers, dividesnowlayers processes by calling snowwater_snicar(),
!  SnowLayersCombine_snicar, SnowLayersDivide_snicar()
!-----------------------------------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_Const_Physical, only: denh2o, denice, hfus, tfrz, cpliq, cpice
   USE MOD_SoilSnowHydrology
   USE MOD_SnowLayersCombineDivide

   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   logical, intent(in) :: USE_Dynamic_Lake

   integer, intent(in) :: maxsnl  ! maximum number of snow layers
   integer, intent(in) :: nl_soil ! number of soil layers
   integer, intent(in) :: nl_lake ! number of soil layers

   real(r8), intent(in) :: deltim             ! seconds in a time step (sec)
   real(r8), intent(in) :: ssi                ! irreducible water saturation of snow
   real(r8), intent(in) :: wimp               ! water impermeable if porosity less than wimp
   real(r8), intent(in) :: porsl(1:nl_soil)   ! volumetric soil water at saturation (porosity)

   real(r8), intent(in) :: pg_rain            ! rainfall incident on ground [mm/s]
   real(r8), intent(in) :: pg_snow            ! snowfall incident on ground [mm/s]

   real(r8), intent(inout) :: dz_lake(1:nl_lake) ! layer thickness for lake (m)

   integer,  intent(in) :: imelt(maxsnl+1:0)  ! signifies if node in melting (imelt = 1)
   real(r8), intent(in) :: fiold(maxsnl+1:0)  ! fraction of ice relative to the total water content at the previous time step
   real(r8), intent(in) :: qseva              ! ground surface evaporation rate (mm h2o/s)
   real(r8), intent(in) :: qsubl              ! sublimation rate from snow pack (mm H2O /s) [+]
   real(r8), intent(in) :: qsdew              ! surface dew added to snow pack (mm H2O /s) [+]
   real(r8), intent(in) :: qfros              ! ground surface frosting formation (mm H2O /s) [+]

   real(r8), intent(inout) :: z_soisno   (maxsnl+1:nl_soil) ! layer depth  (m)
   real(r8), intent(inout) :: dz_soisno  (maxsnl+1:nl_soil) ! layer thickness depth (m)
   real(r8), intent(inout) :: zi_soisno    (maxsnl:nl_soil) ! interface depth (m)
   real(r8), intent(inout) :: t_soisno   (maxsnl+1:nl_soil) ! snow temperature (Kelvin)
   real(r8), intent(inout) :: wice_soisno(maxsnl+1:nl_soil) ! ice lens (kg/m2)
   real(r8), intent(inout) :: wliq_soisno(maxsnl+1:nl_soil) ! liquid water (kg/m2)
   real(r8), intent(inout) :: t_lake      (1:nl_lake)       ! lake temperature (Kelvin)
   real(r8), intent(inout) :: lake_icefrac(1:nl_lake)       ! mass fraction of lake layer that is frozen
   real(r8), intent(inout) :: qout_snowb                    ! rate of water out of snow bottom (mm/s)

   real(r8), intent(inout) :: fseng  ! total sensible heat flux (W/m**2) [+ to atm]
   real(r8), intent(inout) :: fgrnd  ! heat flux into snow / lake (W/m**2) [+ = into soil]

   integer , intent(inout) :: snl    ! number of snow layers
   real(r8), intent(inout) :: scv    ! snow water (mm H2O)
   real(r8), intent(inout) :: snowdp ! snow height (m)
   real(r8), intent(inout) :: sm     ! rate of snow melt (mm H2O /s)

   real(r8), intent(in) :: forc_us
   real(r8), intent(in) :: forc_vs

! SNICAR model variables
! Aerosol Fluxes (Jan. 07, 2023 by Yongjiu Dai)
   real(r8), intent(in) :: forc_aer ( 14 )  ! aerosol deposition from atmosphere model (grd,aer) [kg m-1 s-1]

   logical, optional, intent(in) :: urban_call   ! whether it is a urban CALL

   real(r8), intent(inout) :: &
        mss_bcpho (maxsnl+1:0), &! mass of hydrophobic BC in snow  (col,lyr) [kg]
        mss_bcphi (maxsnl+1:0), &! mass of hydrophillic BC in snow (col,lyr) [kg]
        mss_ocpho (maxsnl+1:0), &! mass of hydrophobic OC in snow  (col,lyr) [kg]
        mss_ocphi (maxsnl+1:0), &! mass of hydrophillic OC in snow (col,lyr) [kg]
        mss_dst1  (maxsnl+1:0), &! mass of dust species 1 in snow  (col,lyr) [kg]
        mss_dst2  (maxsnl+1:0), &! mass of dust species 2 in snow  (col,lyr) [kg]
        mss_dst3  (maxsnl+1:0), &! mass of dust species 3 in snow  (col,lyr) [kg]
        mss_dst4  (maxsnl+1:0)   ! mass of dust species 4 in snow  (col,lyr) [kg]
! Aerosol Fluxes (Jan. 07, 2023)
! END SNICAR model variables

!-------------------------- Local Variables ----------------------------
   logical  has_snow_bef
   integer  j          ! indices
   integer lb          ! lower bound of array

   real(r8) xmf        ! snow melt heat flux (W/m**2)

   real(r8) sumsnowice ! sum of snow ice if snow layers found above unfrozen lake [kg/m&2]
   real(r8) sumsnowliq ! sum of snow liquid if snow layers found above unfrozen lake [kg/m&2]
   logical  unfrozen   ! true if top lake layer is unfrozen with snow layers above
   real(r8) heatsum    ! used in case above [J/m^2]
   real(r8) heatrem    ! used in case above [J/m^2]
   real(r8) dw_soil

   real(r8) a, b, c, d
   real(r8) wice_lake(1:nl_lake)  ! ice lens (kg/m2)
   real(r8) wliq_lake(1:nl_lake)  ! liquid water (kg/m2)
   real(r8) t_ave, frac_
!-----------------------------------------------------------------------

      ! for runoff calculation (assumed no mass change in the land water bodies)
      lb = snl + 1
      qout_snowb = 0.0

      IF (USE_Dynamic_Lake) THEN
         has_snow_bef = (snl < 0)
      ENDIF

      ! ----------------------------------------------------------
      !*[1] snow layer on frozen lake
      ! ----------------------------------------------------------
      IF (snl < 0) THEN
         lb = snl + 1
         IF (DEF_USE_SNICAR .and. .not. present(urban_call)) THEN
            CALL snowwater_SNICAR (lb,deltim,ssi,wimp,&
                         pg_rain,qseva,qsdew,qsubl,qfros,&
                         dz_soisno(lb:0),wice_soisno(lb:0),wliq_soisno(lb:0),qout_snowb,    &
                         forc_aer,&
                         mss_bcpho(lb:0), mss_bcphi(lb:0), mss_ocpho(lb:0), mss_ocphi(lb:0),&
                         mss_dst1(lb:0),  mss_dst2(lb:0),  mss_dst3(lb:0),  mss_dst4(lb:0) )
         ELSE
            CALL snowwater (lb,deltim,ssi,wimp,&
                         pg_rain,qseva,qsdew,qsubl,qfros,&
                         dz_soisno(lb:0),wice_soisno(lb:0),wliq_soisno(lb:0),qout_snowb)
         ENDIF

         ! Natural compaction and metamorphosis.
         lb = snl + 1
         CALL snowcompaction (lb,deltim, &
                              imelt(lb:0),fiold(lb:0),t_soisno(lb:0),&
                              wliq_soisno(lb:0),wice_soisno(lb:0),forc_us,forc_vs,dz_soisno(lb:0))

         ! Combine thin snow elements
         lb = maxsnl + 1
         IF (DEF_USE_SNICAR .and. .not. present(urban_call)) THEN
            CALL snowlayerscombine_SNICAR (lb, snl,&
                                 z_soisno(lb:1),dz_soisno(lb:1),zi_soisno(lb-1:0),&
                                 wliq_soisno(lb:1),wice_soisno(lb:1), t_soisno(lb:1),scv,snowdp, &
                                 mss_bcpho(lb:0), mss_bcphi(lb:0), mss_ocpho(lb:0), mss_ocphi(lb:0),&
                                 mss_dst1(lb:0),  mss_dst2(lb:0),  mss_dst3(lb:0),  mss_dst4(lb:0))
         ELSE
            CALL snowlayerscombine (lb, snl,&
                                 z_soisno(lb:1),dz_soisno(lb:1),zi_soisno(lb-1:0),&
                                 wliq_soisno(lb:1),wice_soisno(lb:1),&
                                 t_soisno(lb:1),scv,snowdp)
         ENDIF

         ! Divide thick snow elements
         IF (snl < 0) THEN
            IF (DEF_USE_SNICAR .and. .not. present(urban_call)) THEN
               CALL snowlayersdivide_SNICAR (lb,snl,z_soisno(lb:0),dz_soisno(lb:0),zi_soisno(lb-1:0),&
                                 wliq_soisno(lb:0),wice_soisno(lb:0),t_soisno(lb:0)     ,&
                                 mss_bcpho(lb:0), mss_bcphi(lb:0), mss_ocpho(lb:0), mss_ocphi(lb:0),&
                                 mss_dst1(lb:0),  mss_dst2(lb:0),  mss_dst3(lb:0),  mss_dst4(lb:0) )
            ELSE
               CALL snowlayersdivide (lb,snl,z_soisno(lb:0),dz_soisno(lb:0),zi_soisno(lb-1:0),&
                                 wliq_soisno(lb:0),wice_soisno(lb:0),t_soisno(lb:0))
            ENDIF
         ENDIF

      ! ----------------------------------------------------------
      !*[2] check for single completely unfrozen snow layer over lake.
      !     Modeling this ponding is unnecessary and can cause instability after the timestep
      !     when melt is completed, as the temperature after melt can be excessive
      !     because the fluxes were calculated with a fixed ground temperature of freezing, but the
      !     phase change was unable to restore the temperature to freezing.  (Zack Subnin 05/2010)
      ! ----------------------------------------------------------

         IF (snl == -1 .and. wice_soisno(0) == 0.) THEN
            ! Remove layer
            ! Take extra heat of layer and release to sensible heat in order to maintain energy conservation.
            heatrem = cpliq*wliq_soisno(0)*(t_soisno(0) - tfrz)
            fseng = fseng + heatrem/deltim
            fgrnd = fgrnd - heatrem/deltim

            snl = 0
            scv = 0.
            snowdp = 0.
            qout_snowb = qout_snowb + wliq_soisno(0)/deltim
            wliq_soisno(0) = 0.
         ENDIF

      ENDIF

      ! ----------------------------------------------------------
      !*[3] check for snow layers above lake with unfrozen top layer. Mechanically,
      !     the snow will fall into the lake and melt or turn to ice. IF the top layer has
      !     sufficient heat to melt the snow without freezing, THEN that will be done.
      !     Otherwise, the top layer will undergo freezing, but only IF the top layer will
      !     not freeze completely. Otherwise, let the snow layers persist and melt by diffusion.
      ! ----------------------------------------------------------

      IF (t_lake(1) > tfrz .and. snl < 0 .and. lake_icefrac(1) < 0.001) THEN ! for unfrozen lake
         unfrozen = .true.
      ELSE
         unfrozen = .false.
      ENDIF

      sumsnowice = 0.
      sumsnowliq = 0.
      heatsum = 0.0
      DO j = snl+1,0
         IF (unfrozen) THEN
            sumsnowice = sumsnowice + wice_soisno(j)
            sumsnowliq = sumsnowliq + wliq_soisno(j)
            heatsum = heatsum + wice_soisno(j)*cpice*(tfrz-t_soisno(j)) &
                              + wliq_soisno(j)*cpliq*(tfrz-t_soisno(j))
         ENDIF
      ENDDO

      IF (unfrozen) THEN
         ! changed by weinan as the subroutine newsnow_lake
         ! Remove snow and subtract the latent heat from the top layer.

         t_ave = tfrz - heatsum/(sumsnowice*cpice + sumsnowliq*cpliq)

         a = heatsum
         b = sumsnowice*hfus
         c = (t_lake(1) - tfrz)*cpliq*denh2o*dz_lake(1)
         d = denh2o*dz_lake(1)*hfus

         ! all snow melt
         IF (c>=a+b)THEN
            t_lake(1) = (cpliq*(denh2o*dz_lake(1)*t_lake(1) + (sumsnowice+sumsnowliq)*tfrz) - a - b) / &
                        (cpliq*(denh2o*dz_lake(1) + sumsnowice + sumsnowliq))
            sm = sm + scv/deltim
            qout_snowb = qout_snowb + scv/deltim
            scv = 0.
            snowdp = 0.
            snl = 0
        ! lake partially freezing to melt all snow
         ELSEIF(c+d >= a+b)THEN
            t_lake(1) = tfrz
            sm = sm + scv/deltim
            qout_snowb = qout_snowb + scv/deltim
            scv = 0.
            snowdp = 0.
            snl = 0
            lake_icefrac(1) = (a+b-c)/d

        !  snow do not melt while all lake freezing
        !    ELSEIF(c+d < a) THEN
        !     t_lake(1) = (c+d + cpice*(sumsnowice*t_ave+denh2o*dz_lake(1)*tfrz) + cpliq*sumsnowliq*t_ave)/&
        !                (cpice*(sumsnowice+denh2o*dz_lake(1))+cpliq*sumsnowliq)
        !     lake_icefrac(1) = 1.0
         ENDIF
      ENDIF


      ! ----------------------------------------------------------
      !*[4] Soil water and ending water balance
      ! ----------------------------------------------------------
      ! Here this consists only of making sure that soil is saturated even as it melts and
      ! pore space opens up. Conversely, if excess ice is melting and the liquid water exceeds the
      ! saturation value, then remove water.

      dw_soil = 0.

      DO j = 1, nl_soil
         dw_soil = dw_soil + wliq_soisno(j) + wice_soisno(j)

         a = wliq_soisno(j)/(dz_soisno(j)*denh2o) + wice_soisno(j)/(dz_soisno(j)*denice)

         IF (a < porsl(j)) THEN
            wliq_soisno(j) = max(0., (porsl(j)*dz_soisno(j) - wice_soisno(j)/denice)*denh2o )
            wice_soisno(j) = max(0., (porsl(j)*dz_soisno(j) - wliq_soisno(j)/denh2o)*denice )
         ELSE
            wliq_soisno(j) = max(0., wliq_soisno(j) - (a - porsl(j))*denh2o*dz_soisno(j) )
            wice_soisno(j) = max(0., (porsl(j)*dz_soisno(j) - wliq_soisno(j)/denh2o)*denice )
         ENDIF

         IF (wliq_soisno(j) > porsl(j)*denh2o*dz_soisno(j)) THEN
            wliq_soisno(j) = porsl(j)*denh2o*dz_soisno(j)
            wice_soisno(j) = 0.0
         ENDIF

         dw_soil = dw_soil - wliq_soisno(j) - wice_soisno(j)
      ENDDO

      IF (USE_Dynamic_Lake) THEN

         IF (has_snow_bef) THEN
            wliq_lake(1) = dz_lake(1) * (1-lake_icefrac(1)) + qout_snowb*deltim*1.e-3
            wice_lake(1) = dz_lake(1) * lake_icefrac(1)
         ELSE
            wliq_lake(1) = dz_lake(1) * (1-lake_icefrac(1)) + (sm + qsdew - qseva)*deltim*1.e-3
            wice_lake(1) = dz_lake(1) * lake_icefrac(1) + (qfros - qsubl)*deltim*1.e-3
            IF (wliq_lake(1) < 0.) THEN
               wice_lake(1) = wice_lake(1) + wliq_lake(1)
               wliq_lake(1) = 0.
            ENDIF
            IF (wice_lake(1) < 0.) THEN
               wliq_lake(1) = wliq_lake(1) + wice_lake(1)
               wice_lake(1) = 0.
            ENDIF
         ENDIF

         dz_lake(1)      = wliq_lake(1) + wice_lake(1)
         dz_lake(1)      = max(dz_lake(1), 1.e-6)
         lake_icefrac(1) = wice_lake(1) / dz_lake(1)
         lake_icefrac(1) = min(max(lake_icefrac(1), 0.), 1.)

         dz_lake(nl_lake) = dz_lake(nl_lake) + dw_soil/1.e3

         IF (dz_lake(nl_lake) < 0.) THEN
            j = nl_lake
            DO WHILE (dz_lake(j) < 0.)
               IF (j > 1) dz_lake(j-1) = dz_lake(j-1) + dz_lake(j)
               dz_lake(j) = 0.
               j = j - 1
               IF (j == 0) EXIT
            ENDDO
         ENDIF

         CALL adjust_lake_layer (nl_lake, dz_lake, t_lake, lake_icefrac)

      ENDIF

   END SUBROUTINE snowwater_lake



   SUBROUTINE roughness_lake (snl,t_grnd,t_lake,lake_icefrac,forc_psrf,&
                             cur,ustar,z0mg,z0hg,z0qg)

!-----------------------------------------------------------------------
! !DESCRIPTION:
!  Calculate lake surface roughness
!
!  Original:
!  The Community Land Model version 4.5 (CLM4.5)
!
! !REVISIONS:
!  Yongjiu Dai, Nan Wei, 01/2018
!-----------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_Const_Physical, only: tfrz,vonkar,grav

   IMPLICIT NONE

   integer,  intent(in) :: snl             ! number of snow layers
   real(r8), intent(in) :: t_grnd          ! ground temperature
   real(r8), intent(in) :: t_lake(1)       ! surface lake layer temperature [K]
   real(r8), intent(in) :: lake_icefrac(1) ! surface lake layer ice mass fraction [0-1]
   real(r8), intent(in) :: forc_psrf       ! atmosphere pressure at the surface [pa]

   real(r8), intent(in) :: cur             ! Charnock parameter (-)
   real(r8), intent(in) :: ustar           ! u* in similarity theory [m/s]

   real(r8), intent(out) :: z0mg           ! roughness length over ground, momentum [m]
   real(r8), intent(out) :: z0hg           ! roughness length over ground, sensible heat [m]
   real(r8), intent(out) :: z0qg           ! roughness length over ground, latent heat [m]

   real(r8), parameter :: cus = 0.1        ! empirical constant for roughness under smooth flow
   real(r8), parameter :: kva0 = 1.51e-5   ! kinematic viscosity of air (m^2/s) at 20C and 1.013e5 Pa
   real(r8), parameter :: prn = 0.713      ! Prandtl # for air at neutral stability
   real(r8), parameter :: sch = 0.66       ! Schmidt # for water in air at neutral stability

   real(r8) kva                            ! kinematic viscosity of air at ground temperature and forcing pressure
   real(r8) sqre0                          ! root of roughness Reynolds number
!-----------------------------------------------------------------------

      IF (t_grnd > tfrz .and. t_lake(1) > tfrz .and. snl == 0) THEN
         kva = kva0 * (t_grnd/293.15)**1.5 * 1.013e5/forc_psrf ! kinematic viscosity of air
         z0mg = max(cus*kva/max(ustar,1.e-4),cur*ustar*ustar/grav) ! momentum roughness length
         z0mg = max(z0mg, 1.0e-5) ! This limit is redundant with current values.
         sqre0 = (max(z0mg*ustar/kva,0.1))**0.5   ! square root of roughness Reynolds number
         z0hg = z0mg * exp( -vonkar/prn*( 4.*sqre0 - 3.2) ) ! SH roughness length
         z0qg = z0mg * exp( -vonkar/sch*( 4.*sqre0 - 4.2) ) ! LH roughness length
         z0qg = max(z0qg, 1.0e-5)  ! Minimum allowed roughness length for unfrozen lakes
         z0hg = max(z0hg, 1.0e-5)  ! set low so it is only to avoid floating point exceptions
      ELSEIF (snl == 0) THEN ! frozen lake with ice, and no snow cover
         z0mg = 0.001              ! z0mg won't have changed
         z0hg = z0mg/exp(0.13 * (ustar*z0mg/1.5e-5)**0.45)
         z0qg = z0hg
      ELSE                         ! use roughness over snow
         z0mg = 0.0024             ! z0mg won't have changed
         z0hg = z0mg/exp(0.13 * (ustar*z0mg/1.5e-5)**0.45)
         z0qg = z0hg
      ENDIF

   END SUBROUTINE roughness_lake



   SUBROUTINE hConductivity_lake(nl_lake,snl,t_grnd,&
                                z_lake,t_lake,lake_icefrac,rhow,&
                                dlat,ustar,z0mg,lakedepth,depthcrit,tk_lake, savedtke1)

!-----------------------------------------------------------------------
!  Diffusivity and implied thermal "conductivity" = diffusivity * cwat
!-----------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_Const_Physical, only: tfrz,tkwat,tkice,tkair,&
                                 vonkar,grav,cpliq,cpice,cpair,denh2o,denice

   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   integer, intent(in) :: nl_lake  ! number of soil layers
   integer, intent(in) :: snl      ! number of snow layers
   real(r8), intent(in) :: t_grnd  ! ground surface temperature [k]
   real(r8), intent(in) :: z_lake(nl_lake)       ! lake node depth (middle point of layer) (m)
   real(r8), intent(in) :: t_lake(nl_lake)       ! lake temperature (kelvin)
   real(r8), intent(in) :: lake_icefrac(nl_lake) ! lake mass fraction of lake layer that is frozen
   real(r8), intent(in) :: rhow(nl_lake)         ! density of water (kg/m**3)

   real(r8), intent(in) :: dlat      ! latitude (radians)
   real(r8), intent(in) :: ustar     ! u* in similarity theory [m/s]
   real(r8), intent(in) :: z0mg      ! roughness length over ground, momentum [m]
   real(r8), intent(in) :: lakedepth ! column lake depth (m)
   real(r8), intent(in) :: depthcrit ! (m) Depth beneath which to enhance mixing


   real(r8), intent(out) :: tk_lake(nl_lake) ! thermal conductivity at layer node [W/(m K)]
   real(r8), intent(out) :: savedtke1      ! top level eddy conductivity (W/mK)

!-------------------------- Local Variables ----------------------------
   real(r8) kme(nl_lake) ! molecular + eddy diffusion coefficient (m**2/s)
   real(r8) cwat   ! specific heat capacity of water (j/m**3/kelvin)
   real(r8) den    ! used in calculating ri
   real(r8) drhodz ! d [rhow] /dz (kg/m**4)
   real(r8) fangkm ! (m^2/s) extra diffusivity based on Fang & Stefan 1996
   real(r8) ke     ! eddy diffusion coefficient (m**2/s)
   real(r8) km     ! molecular diffusion coefficient (m**2/s)
   real(r8) ks     ! coefficient for calculation of decay of eddy diffusivity with depth
   real(r8) n2     ! brunt-vaisala frequency (/s**2)
   real(r8) num    ! used in calculating ri
   real(r8) ri     ! richardson number
   real(r8) tkice_eff  ! effective conductivity since layer depth is constant
   real(r8) tmp    !
   real(r8) u2m    ! 2 m wind speed (m/s
   real(r8) ws     ! surface friction velocity (m/s)

   real(r8), parameter :: mixfact = 5. ! Mixing enhancement factor.
   real(r8), parameter :: p0 = 1.      ! neutral value of turbulent prandtl number


   integer j

! -------------------------------------------------------------------

      cwat = cpliq*denh2o
      tkice_eff = tkice * denice/denh2o ! effective conductivity since layer depth is constant
      km = tkwat/cwat                   ! a constant (molecular diffusivity)
      u2m = max(0.1,ustar/vonkar*log(2./z0mg))
      ws = 1.2e-03 * u2m
      ks = 6.6 * sqrt( abs(sin(dlat)) ) * (u2m**(-1.84))

      DO j = 1, nl_lake-1
         drhodz = (rhow(j+1)-rhow(j)) / (z_lake(j+1)-z_lake(j))
         n2 = max(7.5e-5, grav / rhow(j) * drhodz)
         num = 40. * n2 * (vonkar*z_lake(j))**2
         tmp = -2.*ks*z_lake(j)        ! to avoid underflow computing
         IF(tmp < -40.) tmp = -40.     !
         den = max( (ws**2) * exp(tmp), 1.e-10 )
         ri = ( -1. + sqrt( max(1.+num/den, 0.) ) ) / 20.

         IF ((t_grnd > tfrz .and. t_lake(1) > tfrz .and. snl == 0) ) THEN
            tmp = -ks*z_lake(j)        ! to avoid underflow computing
            IF(tmp < -40.) tmp = -40.  !
            ke = vonkar*ws*z_lake(j)/p0 * exp(tmp) / (1.+37.*ri*ri)
            kme(j) = km + ke

            fangkm = 1.039e-8_r8 * max(n2,7.5e-5)**(-0.43)  ! Fang & Stefan 1996, citing Ellis et al 1991
            kme(j) = kme(j) + fangkm

            IF (lakedepth >= depthcrit) THEN
               kme(j) = kme(j) * mixfact    ! Mixing enhancement factor for lake deep than 25m.
            ENDIF
            tk_lake(j) = kme(j)*cwat
         ELSE
            kme(j) = km
            fangkm = 1.039e-8 * max(n2,7.5e-5)**(-0.43)
            kme(j) = kme(j) + fangkm
            IF (lakedepth >= depthcrit) THEN
                kme(j) = kme(j) * mixfact
            ENDIF
            tk_lake(j) = kme(j)*cwat*tkice_eff / ((1.-lake_icefrac(j))*tkice_eff &
                       + kme(j)*cwat*lake_icefrac(j))
         ENDIF
      ENDDO

      kme(nl_lake) = kme(nl_lake-1)
      savedtke1 = kme(1)*cwat

      IF ((t_grnd > tfrz .and. t_lake(1) > tfrz .and. snl == 0) ) THEN
         tk_lake(nl_lake) = tk_lake(nl_lake-1)
      ELSE
         tk_lake(nl_lake) = kme(nl_lake)*cwat*tkice_eff / ( (1.-lake_icefrac(nl_lake))*tkice_eff &
                    + kme(nl_lake)*cwat*lake_icefrac(nl_lake) )
      ENDIF

   END SUBROUTINE hConductivity_lake


   SUBROUTINE adjust_lake_layer (nl_lake, dz_lake, t_lake, lake_icefrac)

   USE MOD_Const_Physical
   IMPLICIT NONE

   integer,  intent(in)    :: nl_lake
   real(r8), intent(inout) :: dz_lake     (nl_lake)  ! lake layer thickness (m)
   real(r8), intent(inout) :: t_lake      (nl_lake)  ! lake temperature (kelvin)
   real(r8), intent(inout) :: lake_icefrac(nl_lake)  ! lake mass fraction of lake layer that is frozen

   ! Local Variables
   integer  :: i, j
   real(r8) :: wdsrfm, depthratio, resi, resj
   real(r8) :: ticesum, tliqsum, wicesum, wliqsum, olp, tliq, tice, a, b, c, d
   real(r8) :: dz_lake_new      (nl_lake)
   real(r8) :: t_lake_new       (nl_lake)
   real(r8) :: lake_icefrac_new (nl_lake)
   real(r8), parameter :: dzlak(10) = (/0.1, 1., 2., 3., 4., 5., 7., 7., 10.45, 10.45/)  ! m

      wdsrfm = sum(dz_lake)

      IF(wdsrfm > 1.)THEN
         depthratio = wdsrfm / sum(dzlak(1:nl_lake))
         dz_lake_new(1)           = dzlak(1)
         dz_lake_new(2:nl_lake-1) = dzlak(2:nl_lake-1)*depthratio
         dz_lake_new(nl_lake)     = dzlak(nl_lake)*depthratio - (dz_lake_new(1) - dzlak(1)*depthratio)
      ELSEIF(wdsrfm > 0. .and. wdsrfm <= 1.)THEN
         dz_lake_new(:) = wdsrfm / nl_lake
      ENDIF

      j = 1
      resj = dz_lake(j)

      DO i = 1, nl_lake

         ticesum = 0.
         tliqsum = 0.
         wicesum = 0.
         wliqsum = 0.

         resi = dz_lake_new(i)
         DO WHILE (resi > 1.e-8)

            olp = min(resi, resj)
            ticesum = ticesum + olp * lake_icefrac(j) * t_lake(j)
            wicesum = wicesum + olp * lake_icefrac(j)
            tliqsum = tliqsum + olp * (1-lake_icefrac(j)) * t_lake(j)
            wliqsum = wliqsum + olp * (1-lake_icefrac(j))

            resi = resi - olp
            resj = resj - olp

            IF (resj == 0.) THEN
               IF (j == nl_lake) THEN
                  EXIT
               ELSE
                  j = j + 1
                  resj = dz_lake(j)
               ENDIF
            ENDIF

         ENDDO

         IF (wicesum > 0.) tice = ticesum / wicesum
         IF (wliqsum > 0.) tliq = tliqsum / wliqsum

         IF ((wliqsum > 0.) .and. (wicesum > 0.)) THEN

            a = cpliq*wliqsum*(tliq-tfrz)
            b = cpice*wicesum*(tfrz-tice)
            c = wicesum*hfus
            d = wliqsum*hfus

            IF (a >= b + c) THEN
               wicesum = 0.
               wliqsum = dz_lake_new(i)
               t_lake_new(i) = tfrz + (a-b-c)/(wliqsum*cpliq)
            ELSEIF (a >= b) THEN
               wicesum = wicesum - (a-b)/hfus
               t_lake_new(i) = tfrz
            ELSEIF (a + d < b) THEN
               wicesum = dz_lake_new(i)
               t_lake_new(i) = tfrz - (b-a-d)/(wicesum*cpice)
            ELSE ! (b-d <= a < b)
               wicesum = wicesum + (b-a)/hfus
               t_lake_new(i) = tfrz
            ENDIF

         ELSEIF (wliqsum > 0.) THEN
            t_lake_new(i) = tliq
         ELSEIF (wicesum > 0.) THEN
            t_lake_new(i) = tice
         ENDIF

         lake_icefrac_new(i) = wicesum / dz_lake_new(i)

      ENDDO

      dz_lake      = dz_lake_new
      lake_icefrac = lake_icefrac_new
      t_lake       = t_lake_new

   END SUBROUTINE adjust_lake_layer


END MODULE MOD_Lake
