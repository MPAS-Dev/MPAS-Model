#include <define.h>
MODULE MOD_LeafInterception
! -----------------------------------------------------------------
! !DESCRIPTION:
! For calculating vegetation canopy precipitation interception.
!
! This module computes canopy interception and evaporation fluxes.

!ANCILLARY FUNCTIONS AND SUBROUTINES
!-------------------
   !* :SUBROUTINE:"LEAF_interception_CoLM2014" : Leaf interception and drainage schemes based on colm2014 version
   !* :SUBROUTINE:"LEAF_interception_CoLM202x" : Leaf interception and drainage schemes besed on new colm version (under development)
   !* :SUBROUTINE:"LEAF_interception_CLM4"     : Leaf interception and drainage schemes modified from CLM4
   !* :SUBROUTINE:"LEAF_interception_CLM5"     : Leaf interception and drainage schemes modified from CLM5
   !* :SUBROUTINE:"LEAF_interception_NOAHMP"   : Leaf interception and drainage schemes modified from Noah-MP
   !* :SUBROUTINE:"LEAF_interception_MATSIRO"  : Leaf interception and drainage schemes modified from MATSIRO 2021 version
   !* :SUBROUTINE:"LEAF_interception_VIC"      : Leaf interception and drainage schemes modified from VIC
   !* :SUBROUTINE:"LEAF_interception_JULES"    : Leaf interception and drainage schemes modified from JULES
   !* :SUBROUTINE:"LEAF_interception_pftwrap"  : wrapper for pft land use classification

!REVISION HISTORY:
!----------------
   ! 2026.01     Zhongwang Wei: Fully revise CLM4,5,Noah-MP,MATSIRO,VIC and JULES schemes.
   ! 2024.04     Hua Yuan: add option to account for vegetation snow process based on Niu et al., 2004
   ! 2023.07     Hua Yuan: remove wrapper PC by using PFT leaf interception
   ! 2023.06     Shupeng Zhang @ SYSU
   ! 2023.02.23  Zhongwang Wei @ SYSU
   ! 2021.12.12  Zhongwang Wei @ SYSU
   ! 2020.10.21  Zhongwang Wei @ SYSU
   ! 2019.06     Hua Yuan: 1) add wrapper for PFT and PC, and 2) remove sigf by using lai+sai
   ! 2014.04     Yongjiu Dai
   ! 2002.08.31  Yongjiu Dai
	   USE MOD_Precision
	   USE MOD_MPAS_MPI, only: CoLM_stop
	   USE MOD_Const_Physical, only: tfrz, denh2o, denice, cpliq, cpice, hfus
   USE MOD_Namelist, only: DEF_Interception_scheme, DEF_VEG_SNOW

   IMPLICIT NONE

   real(r8), parameter ::  CICE        = 2.094E06  !specific heat capacity of ice (j/m3/k)
   real(r8), parameter ::  bp          = 20.
   real(r8), parameter ::  CWAT        = 4.188E06  !specific heat capacity of water (j/m3/k)
   real(r8), parameter ::  pcoefs(2,2) = reshape((/20.0_r8, 0.206e-8_r8, 0.0001_r8, 0.9999_r8/), (/2,2/))

   ! Minimum significant precipitation rate threshold [mm/s]
   ! Used across all schemes for numerical stability
   real(r8), parameter ::  PRECIP_THRESHOLD = 1.0e-8_r8

   ! Tolerance for interception water balance checks [mm]
   ! Used by check_interception_balance subroutine under CoLMDEBUG
   real(r8), parameter ::  INTERCEPTION_BALANCE_TOL = 1.0e-5_r8

   !----------------------- Dummy argument --------------------------------
   real(r8) :: satcap                     ! maximum allowed water on canopy [mm]
   real(r8) :: satcap_rain                ! maximum allowed rain on canopy [mm]
   real(r8) :: satcap_snow                ! maximum allowed snow on canopy [mm]
   real(r8) :: lsai                       ! sum of leaf area index and stem area index [-]
   real(r8) :: chiv                       ! leaf angle distribution factor
   real(r8) :: ppc                        ! convective precipitation in time-step [mm]
   real(r8) :: ppl                        ! large-scale precipitation in time-step [mm]
   real(r8) :: p0                         ! precipitation in time-step [mm]
   real(r8) :: fpi                        ! coefficient of interception
   real(r8) :: fpi_rain                   ! coefficient of interception of rain
   real(r8) :: fpi_snow                   ! coefficient of interception of snow
   real(r8) :: alpha_rain                 ! coefficient of interception of rain
   real(r8) :: alpha_snow                 ! coefficient of interception of snow
   real(r8) :: pinf                       ! interception of precipitation in time step [mm]
   real(r8) :: tti_rain                   ! direct rain throughfall in time step [mm]
   real(r8) :: tti_snow                   ! direct snow throughfall in time step [mm]
   real(r8) :: tex_rain                   ! canopy rain drainage in time step [mm]
   real(r8) :: tex_snow                   ! canopy snow drainage in time step [mm]
   real(r8) :: vegt                       ! sigf*lsai
   real(r8) :: xs                         ! proportion of the grid area where the intercepted rainfall
                                          ! plus the preexisting canopy water storage
   real(r8)  :: unl_snow_temp,U10,unl_snow_wind,unl_snow
   real(r8)  :: ap, cp, aa1, bb1, exrain, arg, w
   real(r8)  :: thru_rain, thru_snow
   real(r8)  :: xsc_rain, xsc_snow

   real(r8)  :: fvegc                     ! vegetation fraction
   real(r8)  :: FT                        ! the temperature factor for snow unloading
   real(r8)  :: FV                        ! the wind factor for snow unloading
   real(r8)  :: ICEDRIP                   ! snow unloading

   real(r8)  :: ldew_smelt
   real(r8)  :: ldew_frzc
   real(r8)  :: FP
   real(r8)  :: int_rain
   real(r8)  :: int_snow

CONTAINS

   SUBROUTINE LEAF_interception_CoLM2014 (deltim,dewmx,forc_us,forc_vs,chil,sigf,lai,sai,tair,tleaf,&
                                          prc_rain,prc_snow,prl_rain,prl_snow,qflx_irrig_sprinkler,bifall,&
                                          ldew,ldew_rain,ldew_snow,z0m,hu,pg_rain,pg_snow,qintr,qintr_rain,qintr_snow)
!DESCRIPTION
!===========
   ! Calculation of  interception and drainage of precipitation
   ! the treatment are based on Sellers et al. (1996)

!Original Author:
!-------------------
   !canopy interception scheme modified by Yongjiu Dai based on Sellers et al. (1996)

!References:
!-------------------
   !---Dai, Y., Zeng, X., Dickinson, R.E., Baker, I., Bonan, G.B., BosiloVICh,
   !   M.G., Denning, A.S., Dirmeyer, P.A., Houser, P.R., Niu, G. and Oleson,
   !   K.W., 2003.  The common land model. Bulletin of the American
   !   Meteorological Society, 84(8), pp.1013-1024.

   !---Lawrence, D.M., Thornton, P.E., Oleson, K.W. and Bonan, G.B., 2007.  The
   !   partitioning of evapotranspiration into transpiration, soil evaporation,
   !   and canopy evaporation in a GCM: Impacts on land-atmosphere interaction.
   !   Journal of Hydrometeorology, 8(4), pp.862-880.

   !---Oleson, K., Dai, Y., Bonan, B., BosiloVIChm, M., Dickinson, R.,
   !   Dirmeyer, P., Hoffman, F., Houser, P., Levis, S., Niu, G.Y. and
   !   Thornton, P., 2004.  Technical description of the community land model
   !   (CLM).

   !---Sellers, P.J., Randall, D.A., Collatz, G.J., Berry, J.A., Field, C.B.,
   !   Dazlich, D.A., Zhang, C., Collelo, G.D. and Bounoua, L., 1996. A revised
   !   land surface parameterization (SiB2) for atmospheric GCMs.  Part I:
   !   Model formulation. Journal of climate, 9(4), pp.676-705.

   !---Sellers, P.J., Tucker, C.J., Collatz, G.J., Los, S.O., Justice, C.O.,
   !   Dazlich, D.A. and Randall, D.A., 1996.  A revised land surface
   !   parameterization (SiB2) for atmospheric GCMs. Part II: The generation of
   !   global fields of terrestrial biophysical parameters from satellite data.
   !   Journal of climate, 9(4), pp.706-737.


!ANCILLARY FUNCTIONS AND SUBROUTINES
!-------------------

!REVISION HISTORY
!----------------
   !---2024.04.16  Hua Yuan: add option to account for vegetation snow process based on Niu et al., 2004
   !---2023.02.21  Zhongwang Wei @ SYSU : Snow and rain interception
   !---2021.12.08  Zhongwang Wei @ SYSU
   !---2019.06     Hua Yuan: remove sigf and USE lai+sai for judgement.
   !---2014.04     Yongjiu Dai
   !---2002.08.31  Yongjiu Dai
!=======================================================================

   IMPLICIT NONE

   real(r8), intent(in) :: deltim       !seconds in a time step [second]
   real(r8), intent(in) :: dewmx        !maximum dew [mm]
   real(r8), intent(in) :: forc_us      !wind speed
   real(r8), intent(in) :: forc_vs      !wind speed
   real(r8), intent(in) :: chil         !leaf angle distribution factor
   real(r8), intent(in) :: prc_rain     !convective rainfall [mm/s]
   real(r8), intent(in) :: prc_snow     !convective snowfall [mm/s]
   real(r8), intent(in) :: prl_rain     !large-scale rainfall [mm/s]
   real(r8), intent(in) :: prl_snow     !large-scale snowfall [mm/s]
   real(r8), intent(in) :: qflx_irrig_sprinkler ! irrigation and sprinkler water flux [mm/s]
   real(r8), intent(in) :: bifall       !bulk density of newly fallen dry snow [kg/m3]
   real(r8), intent(in) :: sigf         !fraction of veg cover, excluding snow-covered veg [-]
   real(r8), intent(in) :: lai          !leaf area index [-]
   real(r8), intent(in) :: sai          !stem area index [-]
   real(r8), intent(in) :: tair         !air temperature [K]
   real(r8), intent(in) :: tleaf        !sunlit canopy leaf temperature [K]

   real(r8), intent(inout) :: ldew      !depth of water on foliage [mm]
   real(r8), intent(inout) :: ldew_rain !depth of water on foliage [mm]
   real(r8), intent(inout) :: ldew_snow !depth of water on foliage [mm]
   real(r8), intent(in)    :: z0m       !roughness length
   real(r8), intent(in)    :: hu        !forcing height of U

   real(r8), intent(out) :: pg_rain     !rainfall onto ground including canopy runoff [kg/(m2 s)]
   real(r8), intent(out) :: pg_snow     !snowfall onto ground including canopy runoff [kg/(m2 s)]
   real(r8), intent(out) :: qintr       !interception [kg/(m2 s)]
   real(r8), intent(out) :: qintr_rain  !rainfall interception (mm h2o/s)
   real(r8), intent(out) :: qintr_snow  !snowfall interception (mm h2o/s)

!-----------------------------------------------------------------------

      IF (lai+sai > 1e-6) THEN
         lsai   = lai + sai
         vegt   = lsai
         satcap = dewmx*vegt
         satcap_rain = satcap
         satcap_snow = 6.6*(0.27+46./bifall)*vegt  ! Niu et al., 2004
         satcap_snow = 48.*satcap                  ! Simple one without snow density input

         p0  = (prc_rain + prc_snow + prl_rain + prl_snow + qflx_irrig_sprinkler)*deltim
         ppc = (prc_rain + prc_snow)*deltim
         ppl = (prl_rain + prl_snow + qflx_irrig_sprinkler)*deltim

         w = ldew+p0
         IF (tleaf > tfrz) THEN
            xsc_rain = max(0., ldew-satcap)
            xsc_snow = 0.
         ELSE
            xsc_rain = 0.
            xsc_snow = max(0., ldew-satcap)
         ENDIF

         ldew = ldew - (xsc_rain + xsc_snow)

         !TODO-done: account for vegetation snow
         IF ( DEF_VEG_SNOW ) THEN
            xsc_rain  = max(0., ldew_rain-satcap_rain)
            xsc_snow  = max(0., ldew_snow-satcap_snow)
            ldew_rain = ldew_rain - xsc_rain
            ldew_snow = ldew_snow - xsc_snow
            ldew      = ldew_rain + ldew_snow
         ENDIF

         ap = pcoefs(2,1)
         cp = pcoefs(2,2)

         IF (p0 > 1.e-8) THEN
            ap = ppc/p0 * pcoefs(1,1) + ppl/p0 * pcoefs(2,1)
            cp = ppc/p0 * pcoefs(1,2) + ppl/p0 * pcoefs(2,2)

            !----------------------------------------------------------------------
            !      proportional saturated area (xs) and leaf drainage(tex)
            !-----------------------------------------------------------------------
            chiv = chil
            IF ( abs(chiv) .le. 0.01 ) chiv = 0.01
            aa1 = 0.5 - 0.633 * chiv - 0.33 * chiv * chiv
            bb1 = 0.877 * ( 1. - 2. * aa1 )
            exrain = aa1 + bb1

            ! coefficient of interception
            ! set fraction of potential interception to max 0.25 (Lawrence et al. 2007)
            ! assume alpha_rain = alpha_snow
            alpha_rain = 0.25
            fpi = alpha_rain * ( 1.-exp(-exrain*lsai) )
            tti_rain = (prc_rain+prl_rain+qflx_irrig_sprinkler)*deltim * ( 1.-fpi )
            tti_snow = (prc_snow+prl_snow)*deltim * ( 1.-fpi )

            xs = 1.
            IF (p0*fpi>1.e-9) THEN
               arg = (satcap-ldew)/(p0*fpi*ap) - cp/ap
               IF (arg>1.e-9) THEN
                  xs = -1./bp * log( arg )
                  xs = min( xs, 1. )
                  xs = max( xs, 0. )
               ENDIF
            ENDIF

            ! assume no fall down of the intercepted snowfall in a time step
            ! drainage
            tex_rain = (prc_rain+prl_rain+qflx_irrig_sprinkler)*deltim * fpi * (ap/bp*(1.-exp(-bp*xs))+cp*xs) &
                     - max(0., (satcap-ldew)) * xs
            tex_rain = max( tex_rain, 0. )
            ! Ensure physical constraint: tex_rain + tti_rain <= total rain input
            tex_rain = min( tex_rain, (prc_rain+prl_rain+qflx_irrig_sprinkler)*deltim - tti_rain )
            tex_snow = 0.

            ! 04/11/2024, yuan:
            !TODO-done: account for snow on vegetation,
            IF ( DEF_VEG_SNOW ) THEN

               ! re-calculate leaf rain drainage using ldew_rain

               xs = 1.
               IF (p0*fpi>1.e-9) THEN
                  arg = (satcap_rain-ldew_rain)/(p0*fpi*ap) - cp/ap
                  IF (arg>1.e-9) THEN
                     xs = -1./bp * log( arg )
                     xs = min( xs, 1. )
                     xs = max( xs, 0. )
                  ENDIF
               ENDIF

               tex_rain = (prc_rain+prl_rain+qflx_irrig_sprinkler)*deltim * fpi * (ap/bp*(1.-exp(-bp*xs))+cp*xs) &
                        - max(0., (satcap_rain-ldew_rain)) * xs
               tex_rain = max( tex_rain, 0. )
               ! Ensure physical constraint: tex_rain + tti_rain <= total rain input
               tex_rain = min( tex_rain, (prc_rain+prl_rain+qflx_irrig_sprinkler)*deltim - tti_rain )

               ! re-calculate the snow loading rate

               fvegc = 1. - exp(-0.52*lsai)
               FP    = (ppc + ppl) / (10.*ppc + ppl)
               qintr_snow = fvegc * (prc_snow+prl_snow) * FP
               qintr_snow = min (qintr_snow, (satcap_snow-ldew_snow)/deltim * (1.-exp(-(prc_snow+prl_snow)*deltim/satcap_snow)) )
               qintr_snow = max (qintr_snow, 0.)

               ! snow unloading rate

               FT = max(0.0, (tleaf - tfrz) / 1.87e5)
               FV = sqrt(forc_us*forc_us + forc_vs*forc_vs) / 1.56e5
               tex_snow = max(0., ldew_snow/deltim) * (FV+FT)
               tti_snow = (1.0-fvegc)*(prc_snow+prl_snow) + (fvegc*(prc_snow+prl_snow) - qintr_snow)

               ! rate -> mass

               tti_snow = tti_snow * deltim
               tex_snow = tex_snow * deltim
            ENDIF

#if (defined CoLMDEBUG)
            IF (tex_rain+tex_snow+tti_rain+tti_snow-p0 > 1.e-10 .and. .not.DEF_VEG_SNOW) THEN
               write(6,*) 'tex_ + tti_ > p0 in interception code : ',ldew,tex_rain,tex_snow,tti_rain,tti_snow,p0
            ENDIF
#endif

         ELSE
            ! all intercepted by canopy leaves for very small precipitation
            tti_rain = 0.
            tti_snow = 0.
            tex_rain = 0.
            tex_snow = 0.
         ENDIF

         !----------------------------------------------------------------------
         !   total throughfall (thru) and store augmentation
         !----------------------------------------------------------------------

         thru_rain = tti_rain + tex_rain
         thru_snow = tti_snow + tex_snow
         pinf = p0 - (thru_rain + thru_snow)
         ldew = ldew + pinf

         !TODO-done: IF DEF_VEG_SNOW, update ldew_rain, ldew_snow
         IF ( DEF_VEG_SNOW ) THEN
            ldew_rain = ldew_rain + (prc_rain+prl_rain+qflx_irrig_sprinkler)*deltim - thru_rain
            ldew_snow = ldew_snow + (prc_snow+prl_snow)*deltim - thru_snow
            ldew = ldew_rain + ldew_snow
         ENDIF

         pg_rain = (xsc_rain + thru_rain) / deltim
         pg_snow = (xsc_snow + thru_snow) / deltim
         qintr   = pinf / deltim

         qintr_rain = prc_rain + prl_rain + qflx_irrig_sprinkler - thru_rain / deltim
         qintr_snow = prc_snow + prl_snow - thru_snow / deltim

#if (defined CoLMDEBUG)
         w = w - ldew - (pg_rain+pg_snow)*deltim
         IF (abs(w) > INTERCEPTION_BALANCE_TOL) THEN
            write(6,*) 'something wrong in interception code: '
            write(6,*) w, ldew, (pg_rain+pg_snow)*deltim, satcap
            CALL CoLM_stop()
         ENDIF

         IF (DEF_VEG_SNOW .and. abs(ldew-ldew_rain-ldew_snow) > INTERCEPTION_BALANCE_TOL) THEN
            write(6,*) 'something wrong in interception code when DEF_VEG_SNOW: '
            write(6,*) ldew, ldew_rain, ldew_snow
            CALL CoLM_stop()
         ENDIF
#endif

      ELSE
         ! 07/15/2023, yuan: #bug found for ldew value reset.
         !NOTE: this bug should exist in other interception schemes @Zhongwang.
         IF (ldew > 0.) THEN
            IF (tleaf > tfrz) THEN
               pg_rain = prc_rain + prl_rain + qflx_irrig_sprinkler + ldew/deltim
               pg_snow = prc_snow + prl_snow
            ELSE
               pg_rain = prc_rain + prl_rain + qflx_irrig_sprinkler
               pg_snow = prc_snow + prl_snow + ldew/deltim
            ENDIF
         ELSE
            pg_rain = prc_rain + prl_rain + qflx_irrig_sprinkler
            pg_snow = prc_snow + prl_snow
         ENDIF

         ldew       = 0.
         ldew_rain  = 0.
         ldew_snow  = 0.
         qintr      = 0.
         qintr_rain = 0.
         qintr_snow = 0.

      ENDIF

   END SUBROUTINE LEAF_interception_CoLM2014

   SUBROUTINE LEAF_interception_CoLM202x (deltim,dewmx,forc_us,forc_vs,chil,sigf,lai,sai,tair,tleaf,&
                                          prc_rain,prc_snow,prl_rain,prl_snow,qflx_irrig_sprinkler,&
                                          ldew,ldew_rain,ldew_snow,z0m,hu,pg_rain,pg_snow,&
                                          qintr,qintr_rain,qintr_snow)
!DESCRIPTION
!===========
   ! Calculation of interception and drainage of precipitation (under development)
   ! the scheme developed by Zhongwang wei @ SYSU (not finished yet)

!Original Author:
!-------------------
   !---Zhongwang Wei @ SYSU

!References:
!-------------------
   !---Zhong, F., Jiang, S., van Dijk, A.I., Ren, L., Schellekens, J. and Miralles, D.G., 2022.
   !   Revisiting large-scale interception patterns constrained by a synthesis of global experimental
   !   data. Hydrology and Earth System Sciences, 26(21), pp.5647-5667.
   !---

!ANCILLARY FUNCTIONS AND SUBROUTINES
!-------------------

!REVISION HISTORY
!----------------
   !---2023.04.30  Zhongwang Wei @ SYSU : Snow and rain interception
!=======================================================================

   IMPLICIT NONE

   real(r8), intent(in) :: deltim       !seconds in a time step [second]
   real(r8), intent(in) :: dewmx        !maximum dew [mm]
   real(r8), intent(in) :: forc_us      !wind speed
   real(r8), intent(in) :: forc_vs      !wind speed
   real(r8), intent(in) :: chil         !leaf angle distribution factor
   real(r8), intent(in) :: prc_rain     !convective rainfall [mm/s]
   real(r8), intent(in) :: prc_snow     !convective snowfall [mm/s]
   real(r8), intent(in) :: prl_rain     !large-scale rainfall [mm/s]
   real(r8), intent(in) :: prl_snow     !large-scale snowfall [mm/s]
   real(r8), intent(in) :: qflx_irrig_sprinkler ! irrigation and sprinkler water flux [mm/s]
   real(r8), intent(in) :: sigf         !fraction of veg cover, excluding snow-covered veg [-]
   real(r8), intent(in) :: lai          !leaf area index [-]
   real(r8), intent(in) :: sai          !stem area index [-]
   real(r8), intent(in) :: tair         !air temperature [K]
   real(r8), intent(in) :: tleaf        !sunlit canopy leaf temperature [K]

   real(r8), intent(inout) :: ldew      !depth of water on foliage [mm]
   real(r8), intent(inout) :: ldew_rain !depth of water on foliage [mm]
   real(r8), intent(inout) :: ldew_snow !depth of water on foliage [mm]
   real(r8), intent(in)    :: z0m       !roughness length
   real(r8), intent(in)    :: hu        !forcing height of U

   real(r8), intent(out) :: pg_rain     !rainfall onto ground including canopy runoff [kg/(m2 s)]
   real(r8), intent(out) :: pg_snow     !snowfall onto ground including canopy runoff [kg/(m2 s)]
   real(r8), intent(out) :: qintr       !interception [kg/(m2 s)]
   real(r8), intent(out) :: qintr_rain  !rainfall interception (mm h2o/s)
   real(r8), intent(out) :: qintr_snow  !snowfall interception (mm h2o/s)

      IF (lai+sai > 1e-6) THEN
         lsai   = lai + sai
         vegt   = lsai
         satcap = dewmx*vegt

         p0  = (prc_rain + prc_snow + prl_rain + prl_snow + qflx_irrig_sprinkler)*deltim
         ppc = (prc_rain+prc_snow)*deltim
         ppl = (prl_rain+prl_snow+qflx_irrig_sprinkler)*deltim

         w = ldew+p0

         IF (tleaf > tfrz) THEN
            xsc_rain = max(0., ldew-satcap)
            xsc_snow = 0.
         ELSE
            xsc_rain = 0.
            xsc_snow = max(0., ldew-satcap)
         ENDIF
         ldew = ldew - (xsc_rain + xsc_snow)

         ap = pcoefs(2,1)
         cp = pcoefs(2,2)

         IF (p0 > 1.e-8) THEN
            ap = ppc/p0 * pcoefs(1,1) + ppl/p0 * pcoefs(2,1)
            cp = ppc/p0 * pcoefs(1,2) + ppl/p0 * pcoefs(2,2)
            !----------------------------------------------------------------------
            !      proportional saturated area (xs) and leaf drainage(tex)
            !-----------------------------------------------------------------------
            chiv = chil
            IF ( abs(chiv) .le. 0.01 ) chiv = 0.01
            aa1 = 0.5 - 0.633 * chiv - 0.33 * chiv * chiv
            bb1 = 0.877 * ( 1. - 2. * aa1 )
            exrain = aa1 + bb1

            ! coefficient of interception
            ! set fraction of potential interception to max 0.25 (Lawrence et al. 2007)
            alpha_rain = 0.25
            fpi = alpha_rain * ( 1.-exp(-exrain*lsai) )
            tti_rain = (prc_rain+prl_rain+qflx_irrig_sprinkler)*deltim * ( 1.-fpi )
            tti_snow = (prc_snow+prl_snow)*deltim * ( 1.-fpi )

            xs = 1.
            IF (p0*fpi>1.e-9) THEN
               arg = (satcap-ldew)/(p0*fpi*ap) - cp/ap
               IF (arg>1.e-9) THEN
                  xs = -1./bp * log( arg )
                  xs = min( xs, 1. )
                  xs = max( xs, 0. )
               ENDIF
            ENDIF

            ! assume no fall down of the intercepted snowfall in a time step drainage
            tex_rain = (prc_rain+prl_rain+qflx_irrig_sprinkler)*deltim * fpi * (ap/bp*(1.-exp(-bp*xs))+cp*xs) &
                     - max(0., (satcap-ldew)) * xs
            tex_rain = max( tex_rain, 0. )
            ! Ensure physical constraint: tex_rain + tti_rain <= total rain input
            tex_rain = min( tex_rain, (prc_rain+prl_rain+qflx_irrig_sprinkler)*deltim - tti_rain )
            tex_snow = 0.

#if (defined CoLMDEBUG)
            IF (tex_rain+tex_snow+tti_rain+tti_snow-p0 > 1.e-10) THEN
               write(6,*) 'tex_ + tti_ > p0 in interception code : '
            ENDIF
#endif

         ELSE
            ! all intercepted by canopy leves for very small precipitation
            tti_rain = 0.
            tti_snow = 0.
            tex_rain = 0.
            tex_snow = 0.
         ENDIF

         !----------------------------------------------------------------------
         !   total throughfall (thru) and store augmentation
         !----------------------------------------------------------------------

         thru_rain = tti_rain + tex_rain
         thru_snow = tti_snow + tex_snow
         pinf = p0 - (thru_rain + thru_snow)
         ldew = ldew + pinf

         pg_rain = (xsc_rain + thru_rain) / deltim
         pg_snow = (xsc_snow + thru_snow) / deltim
         qintr   = pinf / deltim

         qintr_rain = prc_rain + prl_rain + qflx_irrig_sprinkler - thru_rain / deltim
         qintr_snow = prc_snow + prl_snow - thru_snow / deltim


#if (defined CoLMDEBUG)
         w = w - ldew - (pg_rain+pg_snow)*deltim
         IF (abs(w) > INTERCEPTION_BALANCE_TOL) THEN
            write(6,*) 'something wrong in interception code : '
            write(6,*) w, ldew, (pg_rain+pg_snow)*deltim, satcap
            CALL CoLM_stop()
         ENDIF

         CALL check_interception_balance('CoLM202x', &
              ldew, ldew_rain, ldew_snow, pg_rain, pg_snow, &
              qintr, qintr_rain, qintr_snow)
#endif

      ELSE
         ! 07/15/2023, Hua Yuan: bug found for ldew value reset when vegetation disappears
         ! Yuan's fix: Release canopy water based on temperature
         ! Note: CoLM202x doesn't separate rain/snow storage, so temperature-based
         ! release is appropriate (no phase conservation issue for unified storage)
         IF (ldew > 0.) THEN
            IF (tleaf > tfrz) THEN
               pg_rain = prc_rain + prl_rain + qflx_irrig_sprinkler + ldew/deltim
               pg_snow = prc_snow + prl_snow
            ELSE
               pg_rain = prc_rain + prl_rain + qflx_irrig_sprinkler
               pg_snow = prc_snow + prl_snow + ldew/deltim
            ENDIF
         ELSE
            pg_rain = prc_rain + prl_rain + qflx_irrig_sprinkler
            pg_snow = prc_snow + prl_snow
         ENDIF

         ldew  = 0.
         ldew_rain  = 0.
         ldew_snow  = 0.
         qintr = 0.
         qintr_rain = 0.
         qintr_snow = 0.
      ENDIF
   END SUBROUTINE LEAF_interception_CoLM202x

   SUBROUTINE LEAF_interception_CLM4 (deltim,dewmx,forc_us,forc_vs,chil,sigf,lai,sai,tair,tleaf,&
                                       prc_rain,prc_snow,prl_rain,prl_snow,qflx_irrig_sprinkler,&
                                       ldew,ldew_rain,ldew_snow,z0m,hu,pg_rain,&
                                       pg_snow,qintr,qintr_rain,qintr_snow)
!DESCRIPTION
!===========
   ! Canopy interception following CLM4.5 official implementation
   ! - Interception efficiency: fpi = 0.25*(1-exp(-0.5*LSAI))
   ! - Drainage method: Simple bucket overflow (when storage exceeds capacity)
   ! - Verified against CLM4.5 source code: CTSM-clm4_5_18_r272/src_clm40/biogeophys/Hydrology1Mod.F90
   !
   ! Key features:
   ! - No pre-drainage step (unlike some earlier CoLM versions)
   ! - No spatial heterogeneity consideration (uniform canopy capacity)
   ! - Immediate overflow drainage when capacity is exceeded

!Original Author:
!-------------------
   !Lawrence, D.M.

!References:
!-------------------
   !---Lawrence, D.M., Thornton, P.E., Oleson, K.W. and Bonan, G.B., 2007.
   !   The partitioning of evapotranspiration into transpiration, soil evaporation,
   !   and canopy evaporation in a GCM: Impacts on land-atmosphere interaction. Journal of Hydrometeorology, 8(4), pp.862-880.
   !---Oleson, K.W., Lawrence, D.M., Bonan, G.B., Drewniak, B., Huang, M., Koven, C.D., Levis, S., Li, F., Riley, W.J., Subin, Z.M. and Swenson, S.C., 2013.
   !   Technical description of version 4.5 of the Community Land Model (CLM). NCAR Technical Note NCAR/TN-503+ STR.

!ANCILLARY FUNCTIONS AND SUBROUTINES
!-------------------

!REVISION HISTORY
!----------------
   ! 2023.02.21  Zhongwang Wei @ SYSU : Snow and rain interception
   ! 2021.12.08  Zhongwang Wei @ SYSU
   ! 2014.04     Yongjiu Dai
   ! 2002.08.31  Yongjiu Dai
!=======================================================================

   IMPLICIT NONE

   real(r8), intent(in) :: deltim       !seconds in a time step [second]
   real(r8), intent(in) :: dewmx        !maximum dew [mm]
   real(r8), intent(in) :: forc_us      !wind speed
   real(r8), intent(in) :: forc_vs      !wind speed
   real(r8), intent(in) :: chil         !leaf angle distribution factor
   real(r8), intent(in) :: prc_rain     !convective rainfall [mm/s]
   real(r8), intent(in) :: prc_snow     !convective snowfall [mm/s]
   real(r8), intent(in) :: prl_rain     !large-scale rainfall [mm/s]
   real(r8), intent(in) :: prl_snow     !large-scale snowfall [mm/s]
   real(r8), intent(in) :: qflx_irrig_sprinkler !irrigation and sprinkler water flux [mm/s]
   real(r8), intent(in) :: sigf         !fraction of veg cover, excluding snow-covered veg [-]
   real(r8), intent(in) :: lai          !leaf area index [-]
   real(r8), intent(in) :: sai          !stem area index [-]
   real(r8), intent(in) :: tair         !air temperature [K]
   real(r8), intent(in) :: tleaf        !sunlit canopy leaf temperature [K]

   real(r8), intent(inout) :: ldew      !depth of water on foliage [mm]
   real(r8), intent(inout) :: ldew_rain !depth of water on foliage [mm]
   real(r8), intent(inout) :: ldew_snow !depth of water on foliage [mm]
   real(r8), intent(in)    :: z0m       !roughness length
   real(r8), intent(in)    :: hu        !forcing height of U

   real(r8), intent(out) :: pg_rain     !rainfall onto ground including canopy runoff [kg/(m2 s)]
   real(r8), intent(out) :: pg_snow     !snowfall onto ground including canopy runoff [kg/(m2 s)]
   real(r8), intent(out) :: qintr       !interception [kg/(m2 s)]
   real(r8), intent(out) :: qintr_rain  !rainfall interception (mm h2o/s)
   real(r8), intent(out) :: qintr_snow  !snowfall interception (mm h2o/s)

      IF (lai+sai > 1e-6) THEN
         lsai   = lai + sai
         vegt   = lsai
         satcap = dewmx*vegt

         p0  = (prc_rain + prc_snow + prl_rain + prl_snow + qflx_irrig_sprinkler)*deltim
         ppc = (prc_rain+prc_snow)*deltim
         ppl = (prl_rain+prl_snow+qflx_irrig_sprinkler)*deltim

         w = ldew+p0

         IF (tleaf > tfrz) THEN
            xsc_rain = max(0., ldew-satcap)
            xsc_snow = 0.
         ELSE
            xsc_rain = 0.
            xsc_snow = max(0., ldew-satcap)
         ENDIF

         ldew = ldew - (xsc_rain + xsc_snow)

         IF (p0 > 1.e-8) THEN
            exrain =0.5
            ! coefficient of interception
            ! set fraction of potential interception to max 0.25 (Lawrence et al. 2007)
            alpha_rain = 0.25
            fpi = alpha_rain * ( 1.-exp(-exrain*lsai) )
            tti_rain = (prc_rain+prl_rain+qflx_irrig_sprinkler)*deltim * ( 1.-fpi )
            tti_snow = (prc_snow+prl_snow)*deltim * ( 1.-fpi )

            ! assume no fall down of the intercepted snowfall in a time step
            ! drainage
            tex_rain = (prc_rain+prl_rain+qflx_irrig_sprinkler)*deltim * fpi + ldew - satcap
            tex_rain = max(tex_rain, 0. )
            ! Ensure physical constraint: tex_rain + tti_rain <= total rain input
            tex_rain = min( tex_rain, (prc_rain+prl_rain+qflx_irrig_sprinkler)*deltim - tti_rain )
            tex_snow = 0.

#if (defined CoLMDEBUG)
            IF (tex_rain+tex_snow+tti_rain+tti_snow-p0 > 1.e-10) THEN
               write(6,*) 'tex_ + tti_ > p0 in interception code : '
            ENDIF
#endif


         ELSE
            ! all intercepted by canopy leaves for very small precipitation
            tti_rain = 0.
            tti_snow = 0.
            tex_rain = 0.
            tex_snow = 0.
         ENDIF

         !----------------------------------------------------------------------
         !   total throughfall (thru) and store augmentation
         !----------------------------------------------------------------------
         thru_rain = tti_rain + tex_rain
         thru_snow = tti_snow + tex_snow
         pinf = p0 - (thru_rain + thru_snow)
         ldew = ldew + pinf

         pg_rain = (xsc_rain + thru_rain) / deltim
         pg_snow = (xsc_snow + thru_snow) / deltim
         qintr   = pinf / deltim

         qintr_rain = prc_rain + prl_rain + qflx_irrig_sprinkler - thru_rain / deltim
         qintr_snow = prc_snow + prl_snow - thru_snow / deltim


#if (defined CoLMDEBUG)
         w = w - ldew - (pg_rain+pg_snow)*deltim
         IF (abs(w) > INTERCEPTION_BALANCE_TOL) THEN
            write(6,*) 'something wrong in interception code : '
            write(6,*) w, ldew, (pg_rain+pg_snow)*deltim, satcap
            CALL CoLM_stop()
         ENDIF
#endif

      ELSE
       ! 07/15/2023, yuan: #bug found for ldew value reset.
         IF (ldew > 0.) THEN
            IF (tleaf > tfrz) THEN
               pg_rain = prc_rain + prl_rain + qflx_irrig_sprinkler + ldew/deltim
               pg_snow = prc_snow + prl_snow
            ELSE
               pg_rain = prc_rain + prl_rain + qflx_irrig_sprinkler
               pg_snow = prc_snow + prl_snow + ldew/deltim
            ENDIF
         ELSE
            pg_rain = prc_rain + prl_rain + qflx_irrig_sprinkler
            pg_snow = prc_snow + prl_snow
         ENDIF

         ldew  = 0.
         ldew_rain  = 0.
         ldew_snow  = 0.
         qintr = 0.
         qintr_rain = 0.
         qintr_snow = 0.
      ENDIF

   END SUBROUTINE LEAF_interception_CLM4

   SUBROUTINE LEAF_interception_CLM5 (deltim,dewmx,forc_us,forc_vs,chil,sigf,lai,sai,tair,tleaf,&
                                    prc_rain,prc_snow,prl_rain,prl_snow,qflx_irrig_sprinkler,&
                                    ldew,ldew_rain,ldew_snow,z0m,hu,pg_rain,pg_snow,&
                                    qintr,qintr_rain,qintr_snow)

!DESCRIPTION
!===========
   ! Canopy interception following CLM5.0 official implementation
   ! - Separate treatment for rain and snow interception
   ! - Rain interception: fpi = tanh(LSAI) [or 0.25*(1-exp(-0.5*LSAI))]
   ! - Snow interception: fpi = 1-exp(-0.5*LSAI)
   ! - Liquid water capacity: 0.1*(LAI+SAI) mm
   ! - Snow capacity: 6.0*(LAI+SAI) mm
   ! - Simple bucket overflow drainage based on temperature
   ! - Snow unloading due to wind and temperature
   ! - Verified against CLM5 source: CanopyHydrologyMod.F90
   !
   ! Key features:
   ! - No pre-drainage step (fixed from earlier version)
   ! - Drainage based on storage, not interception (critical fix)
   ! - Temperature-dependent rain/snow drainage
   ! - Physics-based snow unloading

!Original Author:
!-------------------
   !Lawrence, D.M.

!References:
!-------------------
   !---Lawrence, D.M., Thornton, P.E., Oleson, K.W. and Bonan, G.B., 2007.
   !   The partitioning of evapotranspiration into transpiration, soil evaporation,
   !   and canopy evaporation in a GCM: Impacts on land-atmosphere interaction. Journal of Hydrometeorology, 8(4), pp.862-880.
   !---Lawrence, D.M., Fisher, R.A., Koven, C.D., Oleson, K.W., Swenson, S.C., Bonan, G., Collier, N., Ghimire, B.,
   !   van Kampenhout, L., Kennedy, D. and Kluzek, E., 2019. The Community Land Model version 5:
   !   Description of new features, benchmarking, and impact of forcing uncertainty.
   !   Journal of Advances in Modeling Earth Systems, 11(12), pp.4245-4287.
   !---Fan, Y., Meijide, A., Lawrence, D.M., Roupsard, O., Carlson, K.M., Chen, H.Y.,
   !   Röll, A., Niu, F. and Knohl, A., 2019. Reconciling canopy interception parameterization
   !   and rainfall forcing frequency in the Community Land Model for simulating evapotranspiration
   !   of rainforests and oil palm plantations in Indonesia. Journal of Advances in Modeling Earth Systems, 11(3), pp.732-751.


!ANCILLARY FUNCTIONS AND SUBROUTINES
!-------------------

!REVISION HISTORY
!----------------
   ! 2023.02.21  Zhongwang Wei @ SYSU
   ! 2021.12.08  Zhongwang Wei @ SYSU
!=======================================================================

   IMPLICIT NONE

   real(r8), intent(in) :: deltim       !seconds in a time step [second]
   real(r8), intent(in) :: dewmx        !maximum dew [mm]
   real(r8), intent(in) :: forc_us      !wind speed
   real(r8), intent(in) :: forc_vs      !wind speed
   real(r8), intent(in) :: chil         !leaf angle distribution factor
   real(r8), intent(in) :: prc_rain     !convective rainfall [mm/s]
   real(r8), intent(in) :: prc_snow     !convective snowfall [mm/s]
   real(r8), intent(in) :: prl_rain     !large-scale rainfall [mm/s]
   real(r8), intent(in) :: prl_snow     !large-scale snowfall [mm/s]
   real(r8), intent(in) :: qflx_irrig_sprinkler !irrigation and sprinkler water flux [mm/s]
   real(r8), intent(in) :: sigf         !fraction of veg cover, excluding snow-covered veg [-]
   real(r8), intent(in) :: lai          !leaf area index [-]
   real(r8), intent(in) :: sai          !stem area index [-]
   real(r8), intent(in) :: tair         !air temperature [K]
   real(r8), intent(in) :: tleaf        !sunlit canopy leaf temperature [K]

   real(r8), intent(inout) :: ldew      !depth of water on foliage [mm]
   real(r8), intent(inout) :: ldew_rain !depth of water on foliage [mm]
   real(r8), intent(inout) :: ldew_snow !depth of water on foliage [mm]
   real(r8), intent(in)    :: z0m       !roughness length
   real(r8), intent(in)    :: hu        !forcing height of U

   real(r8), intent(out) :: pg_rain     !rainfall onto ground including canopy runoff [kg/(m2 s)]
   real(r8), intent(out) :: pg_snow     !snowfall onto ground including canopy runoff [kg/(m2 s)]
   real(r8), intent(out) :: qintr       !interception [kg/(m2 s)]
   real(r8), intent(out) :: qintr_rain  !rainfall interception (mm h2o/s)
   real(r8), intent(out) :: qintr_snow  !snowfall interception (mm h2o/s)
   real(r8) :: xsnorun, xliqrun,qflx_prec_intr_rain,qflx_prec_intr_snow

      IF (lai+sai > 1e-6) THEN
         lsai   = lai + sai
         vegt   = lsai
         p0  = (prc_rain + prc_snow + prl_rain + prl_snow + qflx_irrig_sprinkler)*deltim

         ! Ensure ldew is consistent with components at entry
         ! CLM5 operates on ldew_rain/ldew_snow and sets ldew = ldew_rain + ldew_snow at exit
         ! At entry from initialization or restart, ldew may be inconsistent
         ldew = ldew_rain + ldew_snow

         w = ldew+p0  ! For mass balance check

         ! Canopy capacity - CLM5 official values
         ! Verified against CanopyHydrologyMod.F90 lines 320, 329-330
         satcap_rain = dewmx*vegt        ! liquid water capacity = 0.1*(LAI+SAI)
         satcap_snow = satcap_rain*60.0  ! snow capacity = 6.0*(LAI+SAI)

         IF(p0 > 1.e-8) THEN
            ! Interception efficiency - CLM5 formulas
            ! Rain: CLM5 line 323 (tanh option) or line 325 (exponential option)
            ! Snow: CLM5 line 332
            ! Note: CoLM uses tanh for rain; CLM5 default is exponential (CLM4.5)
            alpha_rain = 1.0
            alpha_snow = 1.0
            fpi_rain   = alpha_rain * tanh(lsai)
            fpi_snow   = alpha_snow * ( 1.-exp(-0.5*lsai) )

            ! Direct throughfall - CLM5 lines 334, 337, 341
            tti_rain   = (prc_rain+prl_rain+qflx_irrig_sprinkler)*deltim * ( 1.-fpi_rain )
            tti_snow   = (prc_snow+prl_snow)*deltim * ( 1.-fpi_snow )

            ! Intercepted precipitation - CLM5 line 345
            qflx_prec_intr_rain = (prc_rain+prl_rain+qflx_irrig_sprinkler)*deltim * fpi_rain
            qflx_prec_intr_snow = (prc_snow+prl_snow)*deltim * fpi_snow

            ! Water storage of intercepted precipitation - CLM5 lines 347-348
            ! Add interception to storage BEFORE calculating drainage
            ldew_rain = max(0., ldew_rain + qflx_prec_intr_rain)
            ldew_snow = max(0., ldew_snow + qflx_prec_intr_snow)

            ! Initialize drainage
            tex_rain = 0.
            tex_snow = 0.
            unl_snow = 0.

            ! Snow unloading due to wind and temperature - CLM5 lines ~420-450
            ! (in CLM5 this is in separate unloading section, but physics is same)
            IF(ldew_snow > 1.e-8) THEN
               U10           =  sqrt(forc_us*forc_us+forc_vs*forc_vs)
               unl_snow_temp =  ldew_snow*(tleaf-tfrz)/(1.87*1.e5)
               unl_snow_temp =  max(unl_snow_temp,0.0)
               unl_snow_wind =  U10*ldew_snow/(1.56*1.e5)
               unl_snow_wind =  max(unl_snow_wind,0.0)
               unl_snow      =  unl_snow_temp+unl_snow_wind
               unl_snow      =  min(unl_snow,ldew_snow)
               ldew_snow     = ldew_snow - unl_snow
            ENDIF

            ! Simple bucket overflow drainage - CLM5 lines 367-379
            ! Separate handling for rain and snow based on temperature
            IF (tleaf > tfrz) THEN
               ! Above freezing: liquid water drainage
               xliqrun = max(0., (ldew_rain - satcap_rain)/deltim)
               IF (xliqrun > 0.) THEN
                  tex_rain = xliqrun * deltim
                  ldew_rain = satcap_rain
               ENDIF
            ELSE
               ! Below freezing: snow falling off canopy
               xsnorun = max(0., (ldew_snow - satcap_snow)/deltim)
               IF (xsnorun > 0.) THEN
                  tex_snow = xsnorun * deltim
                  ldew_snow = satcap_snow
               ENDIF
            ENDIF

         ELSE
            ! No precipitation - no interception or drainage
            tti_rain = 0.
            tti_snow = 0.
            tex_rain = 0.
            tex_snow = 0.
            unl_snow = 0.
         ENDIF

         !----------------------------------------------------------------------
         !   Total water reaching ground and interception
         !----------------------------------------------------------------------
         thru_rain = tti_rain + tex_rain
         thru_snow = tti_snow + tex_snow + unl_snow
         ldew      = ldew_rain + ldew_snow

         pg_rain = thru_rain / deltim
         pg_snow = thru_snow / deltim
         qintr   = (p0 - thru_rain - thru_snow) / deltim
         qintr_rain = (prc_rain + prl_rain + qflx_irrig_sprinkler) - thru_rain / deltim
         qintr_snow = (prc_snow + prl_snow) - thru_snow / deltim

#if (defined CoLMDEBUG)
         ! Mass balance check
         w = w - ldew - (pg_rain+pg_snow)*deltim
         IF (abs(w) > INTERCEPTION_BALANCE_TOL) THEN
            write(6,*) 'Mass balance error in CLM5 interception:'
            write(6,*) 'Error:', w, 'ldew:', ldew, 'outflow:', (pg_rain+pg_snow)*deltim
            write(6,*) 'satcap_rain:', satcap_rain, 'satcap_snow:', satcap_snow
            CALL CoLM_stop()
         ENDIF

         CALL check_interception_balance('CLM5', &
              ldew, ldew_rain, ldew_snow, pg_rain, pg_snow, &
              qintr, qintr_rain, qintr_snow)
#endif

      ELSE
         ! 07/15/2023, Hua Yuan: bug found for ldew value reset when vegetation disappears
         ! 2026-01-16 improvement: Maintain phase conservation for rain/snow separated schemes
         ! Yuan's original fix released water based on temperature, which violates phase conservation
         ! for schemes that separate rain and snow storage (ldew_rain vs ldew_snow)
         !
         ! Yuan's original code (2023-07-15):
         ! IF (ldew > 0.) THEN
         !    IF (tleaf > tfrz) THEN
         !       pg_rain = prc_rain + prl_rain + qflx_irrig_sprinkler + ldew/deltim
         !       pg_snow = prc_snow + prl_snow
         !    ELSE
         !       pg_rain = prc_rain + prl_rain + qflx_irrig_sprinkler
         !       pg_snow = prc_snow + prl_snow + ldew/deltim
         !    ENDIF
         ! ELSE
         !    pg_rain = prc_rain + prl_rain + qflx_irrig_sprinkler
         !    pg_snow = prc_snow + prl_snow
         ! ENDIF
         !
         ! Improved version: Release liquid and solid water separately to preserve phase states
         pg_rain = prc_rain + prl_rain + qflx_irrig_sprinkler + ldew_rain/deltim
         pg_snow = prc_snow + prl_snow + ldew_snow/deltim

         ldew       = 0.
         ldew_rain  = 0.
         ldew_snow  = 0.
         qintr      = 0.
         qintr_rain = 0.
         qintr_snow = 0.
      ENDIF

   END SUBROUTINE LEAF_interception_CLM5

   SUBROUTINE LEAF_interception_NOAHMP(deltim,dewmx,forc_us,forc_vs,chil,sigf,lai,sai,tair,tleaf, &
                                       prc_rain,prc_snow,prl_rain,prl_snow,qflx_irrig_sprinkler,&
                                       ldew,ldew_rain,ldew_snow,z0m,hu,pg_rain,pg_snow,qintr,qintr_rain,qintr_snow)
!DESCRIPTION
!===========
   ! Interception and drainage of precipitation
   ! the treatment are modified from Noah-MP 5.0

!Original Author:
!-------------------
   !---Guo-Yue Niu

!References:
!-------------------
   !---Yang, M., Zuo, R., Li, X. and Wang, L., 2019. Improvement test for the canopy interception parameterization scheme
   !   in the community land model. Sola, 15, pp.166-171.
   !---Niu, G.Y., Yang, Z.L., Mitchell, K.E., Chen, F., Ek, M.B., Barlage, M., Kumar, A.,
   !   Manning, K., Niyogi, D., Rosero, E. and Tewari, M., 2011. The community Noah land
   !   surface model with multiparameterization options (Noah‐MP): 1. Model description and evaluation
   !   with local‐scale measurements. Journal of Geophysical Research: Atmospheres, 116(D12).
   !---He, C., Valayamkunnath, P., Barlage, M., Chen, F., Gochis, D., Cabell, R., Schneider, T.,
   !   Rasmussen, R., Niu, G.Y., Yang, Z.L. and Niyogi, D., 2023. Modernizing the open-source
   !   community Noah-MP land surface model (version 5.0) with enhanced modularity,
   !   interoperability, and applicability. EGUsphere, 2023, pp.1-31.

!ANCILLARY FUNCTIONS AND SUBROUTINES
!-------------------

!REVISION HISTORY
!----------------
   ! 2026.02.11  Zhongwang Wei @ SYSU - Added input clamping, comment fixes
   ! 2023.02.21  Zhongwang Wei @ SYSU
   ! 2021.12.08  Zhongwang Wei @ SYSU
!=======================================================================

   IMPLICIT NONE

   real(r8), intent(in)    :: deltim     !seconds in a time step [second]
   real(r8), intent(in)    :: dewmx      !maximum dew [mm]
   real(r8), intent(in)    :: forc_us    !wind speed
   real(r8), intent(in)    :: forc_vs    !wind speed
   real(r8), intent(in)    :: chil       !leaf angle distribution factor
   real(r8), intent(in)    :: prc_rain   !convective rainfall [mm/s]
   real(r8), intent(in)    :: prc_snow   !convective snowfall [mm/s]
   real(r8), intent(in)    :: prl_rain   !large-scale rainfall [mm/s]
   real(r8), intent(in)    :: prl_snow   !large-scale snowfall [mm/s]
   real(r8), intent(in)    :: qflx_irrig_sprinkler !irrigation and sprinkler water flux [mm/s]
   real(r8), intent(in)    :: sigf       !fraction of veg cover, excluding snow-covered veg [-]
   real(r8), intent(in)    :: lai        !leaf area index [-]
   real(r8), intent(in)    :: sai        !stem area index [-]
   real(r8), intent(in)    :: tair       !air temperature [K]
   real(r8), intent(inout) :: tleaf      !sunlit canopy leaf temperature [K]

   real(r8), intent(inout) :: ldew       !depth of water on foliage [mm]
   real(r8), intent(inout) :: ldew_rain  !depth of liquid on foliage [mm]
   real(r8), intent(inout) :: ldew_snow  !depth of solid (frozen) on foliage [mm]
   real(r8), intent(in)    :: z0m        !roughness length
   real(r8), intent(in)    :: hu         !forcing height of U

   real(r8), intent(out)   :: pg_rain    !rainfall onto ground including canopy runoff [kg/(m2 s)]
   real(r8), intent(out)   :: pg_snow    !snowfall onto ground including canopy runoff [kg/(m2 s)]
   real(r8), intent(out)   :: qintr      !interception [kg/(m2 s)]
   real(r8), intent(out)   :: qintr_rain !rainfall interception (mm h2o/s)
   real(r8), intent(out)   :: qintr_snow !snowfall interception (mm h2o/s)

   ! Local variables
   real(r8)                :: PrecipAreaFrac !fraction of gridcell receiving precipitation [-]
   real(r8)                :: BDFALL
      IF (lai+sai > 1e-6) THEN
         lsai   = lai + sai
         vegt   = lsai
         ! Calculate vegetation fraction from LAI (alternative to input VegFrac)
         fvegc=max(0.05,1.0-exp(-0.52*lsai))

         ! Maximum canopy water - Noah-MP lines 82, 105
         ! Note: Official Noah-MP uses VegFrac as input variable
         ! CoLM uses fvegc calculated from LAI, which is also physically reasonable
         satcap_rain = fvegc * dewmx*vegt
         BDFALL      = 67.92+51.25*EXP(MIN(2.5,(tleaf-273.15))/2.59)
         satcap_snow = fvegc * 6.6*(0.27+46./BDFALL) * lsai
         satcap_snow = max(0.0,satcap_snow)

         ! Input clamping: prevent negative precipitation (numerical noise)
         ! from causing mass balance failures
         p0  = MAX(0.0_r8, prc_rain + prc_snow + prl_rain + prl_snow + qflx_irrig_sprinkler) * deltim
         ppc = MAX(0.0_r8, prc_rain + prc_snow) * deltim
         ppl = MAX(0.0_r8, p0 - ppc)

         ! Estimate PrecipAreaFrac based on precipitation type - Noah-MP line 47
         ! Convective precipitation typically covers ~10% of gridcell
         ! Stratiform precipitation typically covers ~100% of gridcell
         IF (p0 > 1.e-8) THEN
            PrecipAreaFrac = (0.1*ppc + 1.0*ppl) / p0
            PrecipAreaFrac = max(0.1, min(1.0, PrecipAreaFrac))  ! constrain to [0.1, 1.0]
         ELSE
            PrecipAreaFrac = 1.0
         ENDIF

         ! Ensure ldew is consistent with components at entry
         ! NoahMP modifies ldew in-place; if ldew != ldew_rain + ldew_snow
         ! at entry (e.g., from initialization or restart), mass balance drifts
         ldew = ldew_rain + ldew_snow

         w   = ldew+p0

         ! Initialize excess water variables
         xsc_rain    = 0.0
         xsc_snow    = 0.0

         !snow unloading - Noah-MP lines 113-120
         IF (ldew_snow>1.e-8) THEN
            FT = MAX(0.0,(tair - 270.15) / 1.87E5)
            FV = SQRT(forc_us*forc_us + forc_vs*forc_vs) / 1.56E5
            ICEDRIP = MAX(0.,ldew_snow) * (FV+FT)    !MB: removed /DT
            ICEDRIP = MIN(ICEDRIP,ldew_snow)
            xsc_snow      =  xsc_snow+ICEDRIP
            ldew_snow     =  ldew_snow - ICEDRIP
         ENDIF

         ! phase change and excess !
         IF (tleaf > tfrz) THEN
            IF (ldew_snow>1.e-8) THEN
               ldew_smelt = MIN(ldew_snow,(tleaf-tfrz)*CICE*ldew_snow/DENICE/(HFUS))
               ldew_smelt = MAX(ldew_smelt,0.0)
               ldew_snow  = ldew_snow-ldew_smelt
               ldew_rain  = ldew_rain+ldew_smelt
               xsc_rain   = xsc_rain + MAX(0., ldew_rain-satcap_rain)
               ldew_rain  = ldew_rain - MAX(0., ldew_rain-satcap_rain)
            ENDIF
            ! tleaf      = fvegc*tfrz+ (1.0-fwet)*tleaf
         ELSE
            IF (ldew_rain>1.e-8) THEN
               ldew_frzc  = MIN(ldew_rain,(tfrz-tleaf)*CWAT*ldew_rain/DENH2O/(HFUS))
               ldew_frzc  = MAX(ldew_frzc,0.0)
               ldew_snow  = ldew_snow+ldew_frzc
               ldew_rain  = ldew_rain-ldew_frzc
               xsc_snow   = xsc_snow + MAX(0., ldew_snow-satcap_snow)
               ldew_snow     = ldew_snow - MAX(0., ldew_snow-satcap_snow)
            ENDIF
            !tleaf      = fvegc*tfrz+ (1.0-fwet)*tleaf
         ENDIF
         ! Resync ldew with components after phase change (CoLM2014 pattern)
         ldew = ldew_rain + ldew_snow

         IF (p0 > 1.e-8) THEN

            ! Throughfall: direct precipitation through vegetation gaps - Noah-MP lines 91, 119
            tti_rain = (prc_rain+prl_rain+qflx_irrig_sprinkler)*deltim * ( 1.-fvegc )
            tti_snow = (prc_snow+prl_snow)*deltim * ( 1.-fvegc )

            ! Interception and drip calculation - Noah-MP lines 86-90, 109-118
            ! Interception rate [mm/s]
            int_rain = fvegc * (prc_rain+prl_rain+qflx_irrig_sprinkler) * PrecipAreaFrac  ! max interception capability
            int_rain = min(int_rain, (satcap_rain-ldew_rain)/deltim * &
                          (1.0-exp(-(prc_rain+prl_rain+qflx_irrig_sprinkler)*deltim/satcap_rain)))
            int_rain = max(0., int_rain)

            int_snow = fvegc * (prc_snow+prl_snow) * PrecipAreaFrac  ! max interception capability
            int_snow = min(int_snow, (satcap_snow-ldew_snow)/deltim * &
                          (1.0-exp(-(prc_snow+prl_snow)*deltim/satcap_snow)))
            int_snow = max(0., int_snow)

            ! Drip: excess precipitation on vegetation that cannot be intercepted
            tex_rain = (prc_rain+prl_rain+qflx_irrig_sprinkler)*fvegc*deltim  - int_rain*deltim
            tex_snow = (prc_snow+prl_snow)*fvegc*deltim - int_snow*deltim
#if (defined CoLMDEBUG)
            IF (tex_rain+tex_snow+tti_rain+tti_snow-p0 > 1.e-10) THEN
               write(6,*) 'tex_ + tti_ > p0 in interception code : '
            ENDIF
#endif
         ELSE
            ! all intercepted by canopy leaves for very small precipitation
            tti_rain = 0.
            tti_snow = 0.
            tex_rain = 0.
            tex_snow = 0.
         ENDIF

         !BDFALL = 67.92+51.25*EXP(MIN(2.5,(SFCTMP-TFRZ))/2.59)

         !----------------------------------------------------------------------
         !   total throughfall (thru) and store augmentation
         !----------------------------------------------------------------------

         thru_rain = tti_rain + tex_rain
         thru_snow = tti_snow + tex_snow
         pinf = p0 - (thru_rain + thru_snow)

         ! Update rain/snow components following CoLM2014 pattern (lines 322-324)
         ldew_rain = ldew_rain + (prc_rain+prl_rain+qflx_irrig_sprinkler)*deltim - thru_rain
         ldew_snow = ldew_snow + (prc_snow+prl_snow)*deltim - thru_snow
         ldew = ldew_rain + ldew_snow

         pg_rain = (xsc_rain + thru_rain) / deltim
         pg_snow = (xsc_snow + thru_snow) / deltim
         qintr   = pinf / deltim

         qintr_rain = prc_rain + prl_rain + qflx_irrig_sprinkler - thru_rain / deltim
         qintr_snow = prc_snow + prl_snow - thru_snow / deltim

#if (defined CoLMDEBUG)
         w = w - ldew - (pg_rain+pg_snow)*deltim
         IF (abs(w) > INTERCEPTION_BALANCE_TOL) THEN
            write(6,*) 'something wrong in interception code : '
            write(6,*) w, ldew, (pg_rain+pg_snow)*deltim  !, satcap
            CALL CoLM_stop()
         ENDIF

         CALL check_interception_balance('NoahMP', &
              ldew, ldew_rain, ldew_snow, pg_rain, pg_snow, &
              qintr, qintr_rain, qintr_snow)
#endif

      ELSE
         ! 07/15/2023, Hua Yuan: bug found for ldew value reset when vegetation disappears
         ! Release canopy water separately by phase to preserve phase conservation
         pg_rain = prc_rain + prl_rain + qflx_irrig_sprinkler + ldew_rain/deltim
         pg_snow = prc_snow + prl_snow + ldew_snow/deltim

         ldew      = 0.
         ldew_rain = 0.
         ldew_snow = 0.
         qintr = 0.
         qintr_rain = 0.
         qintr_snow = 0.

      ENDIF

   END SUBROUTINE LEAF_interception_NOAHMP


   SUBROUTINE LEAF_interception_MATSIRO (deltim,dewmx,forc_us,forc_vs,chil,sigf,lai,sai,tair,tleaf, &
                                         prc_rain,prc_snow,prl_rain,prl_snow,qflx_irrig_sprinkler,&
                                         ldew,ldew_rain,ldew_snow,z0m,hu,pg_rain,pg_snow,qintr,&
                                         qintr_rain,qintr_snow)
!DESCRIPTION
!===========
   ! Interception and drainage of precipitation
   ! the treatment are modified from MATSIRO 6 (under development)

!Original Author:
!-------------------
   !---MATSIRO6 document writing team∗

!References:
!-------------------
   !---Tatebe, H., Ogura, T., Nitta, T., Komuro, Y., Ogochi, K., Takemura, T., Sudo, K., Sekiguchi, M.,
   !   Abe, M., Saito, F. and Chikira, M., 2019. Description and basic evaluation of simulated mean state,
   !   internal variability, and climate sensitivity in MIROC6. Geoscientific Model Development, 12(7), pp.2727-2765. 116(D12).
   !---Takata, K., Emori, S. and Watanabe, T., 2003. Development of the minimal advanced treatments of surface interaction and
   !   runoff. Global and planetary Change, 38(1-2), pp.209-222.
   !---Guo, Q., Kino, K., Li, S., Nitta, T., Takeshima, A., Suzuki, K.T., Yoshida, N. and Yoshimura, K., 2021.
   !   Description of MATSIRO6.

!ANCILLARY FUNCTIONS AND SUBROUTINES
!-------------------

!REVISION HISTORY
!----------------
   ! 2026.02.11  Zhongwang Wei @ SYSU - Added input clamping, comment fixes
   ! 2023.02.21  Zhongwang Wei @ SYSU
   ! 2021.12.08  Zhongwang Wei @ SYSU
!=======================================================================

   IMPLICIT NONE

   real(r8), intent(in) :: deltim       !seconds in a time step [second]
   real(r8), intent(in) :: dewmx        !maximum dew [mm]
   real(r8), intent(in) :: forc_us      !wind speed
   real(r8), intent(in) :: forc_vs      !wind speed
   real(r8), intent(in) :: chil         !leaf angle distribution factor
   real(r8), intent(in) :: prc_rain     !convective rainfall [mm/s]
   real(r8), intent(in) :: prc_snow     !convective snowfall [mm/s]
   real(r8), intent(in) :: prl_rain     !large-scale rainfall [mm/s]
   real(r8), intent(in) :: prl_snow     !large-scale snowfall [mm/s]
   real(r8), intent(in) :: qflx_irrig_sprinkler !irrigation and sprinkler water flux [mm/s]
   real(r8), intent(in) :: sigf         !fraction of veg cover, excluding snow-covered veg [-]
   real(r8), intent(in) :: lai          !leaf area index [-]
   real(r8), intent(in) :: sai          !stem area index [-]
   real(r8), intent(in) :: tair         !air temperature [K]
   real(r8), intent(inout) :: tleaf     !sunlit canopy leaf temperature [K]

   real(r8), intent(inout) :: ldew      !depth of water on foliage [mm]
   real(r8), intent(inout) :: ldew_rain !depth of liquid on foliage [mm]
   real(r8), intent(inout) :: ldew_snow !depth of solid (frozen) on foliage [mm]
   real(r8), intent(in)    :: z0m       !roughness length
   real(r8), intent(in)    :: hu        !forcing height of  U


   real(r8), intent(out) :: pg_rain     !rainfall onto ground including canopy runoff [kg/(m2 s)]
   real(r8), intent(out) :: pg_snow     !snowfall onto ground including canopy runoff [kg/(m2 s)]
   real(r8), intent(out) :: qintr       !interception [kg/(m2 s)]
   real(r8), intent(out) :: qintr_rain  !rainfall interception (mm h2o/s)
   real(r8), intent(out) :: qintr_snow  !snowfall interception (mm h2o/s)
   !local
   real(r8) :: fint, Ac, dewmx_MATSIRO,ldew_rain_s, ldew_snow_s,ldew_rain_n, ldew_snow_n
   real(r8) :: tex_rain_n,tex_rain_s,tex_snow_n,tex_snow_s,tti_rain_n,tti_rain_s,tti_snow_n,tti_snow_s

      !the canopy water capacity per leaf area index is set to 0.2mm
      dewmx_MATSIRO = 0.2
      !the fraction of the convective precipitation area is assumed to be uniform (0.1)
      Ac            = 0.1

      IF (lai+sai > 1e-6) THEN
         lsai   = lai + sai
         vegt   = lsai
         ! Input clamping: prevent negative precipitation (numerical noise)
         ! from causing mass balance failures
         p0  = MAX(0.0_r8, prc_rain + prc_snow + prl_rain + prl_snow + qflx_irrig_sprinkler) * deltim
         ppc = MAX(0.0_r8, prc_rain + prc_snow) * deltim
         ppl = MAX(0.0_r8, p0 - ppc)

         satcap_rain = dewmx_MATSIRO*vegt
         satcap_snow = dewmx_MATSIRO*vegt

         ! Ensure ldew is consistent with components at entry
         ! MATSIRO modifies ldew in-place; inconsistency at entry propagates to output
         ldew = ldew_rain + ldew_snow

         w = ldew+p0

         xsc_rain = max(0., ldew_rain-satcap_rain)
         xsc_snow = max(0., ldew_snow-satcap_snow)

         ldew_rain     = ldew_rain-xsc_rain
         ldew_snow     = ldew_snow-xsc_snow
         ! phase change and excess !
         IF (tleaf > tfrz) THEN
            IF (ldew_snow>1.e-8) THEN
               ldew_smelt = MIN(ldew_snow,(tleaf-tfrz)*CICE*ldew_snow/DENICE/(HFUS))
               ldew_smelt = MAX(ldew_smelt,0.0)
               ldew_snow  = ldew_snow-ldew_smelt
               ldew_rain  = ldew_rain+ldew_smelt
               xsc_rain   = xsc_rain + MAX(0., ldew_rain-satcap_rain)
               ldew_rain  = ldew_rain - MAX(0., ldew_rain-satcap_rain)
            ENDIF
            ! tleaf      = fvegc*tfrz+ (1.0-fwet)*tleaf
         ELSE
            IF (ldew_rain>1.e-8) THEN
               ldew_frzc  = MIN(ldew_rain,(tfrz-tleaf)*CWAT*ldew_rain/DENH2O/(HFUS))
               ldew_frzc  = MAX(ldew_frzc,0.0)
               ldew_snow  = ldew_snow+ldew_frzc
               ldew_rain  = ldew_rain-ldew_frzc
               xsc_snow   = xsc_snow  + MAX(0., ldew_snow-satcap_snow)
               ldew_snow  = ldew_snow - MAX(0., ldew_snow-satcap_snow)
            ENDIF
            !tleaf      = fvegc*tfrz+ (1.0-fwet)*tleaf
         ENDIF
         ! Resync ldew with components after phase change (CoLM2014 pattern)
         ldew = ldew_rain + ldew_snow

         IF (p0 > 1.e-8) THEN
            ! Interception efficiency - MATSIRO formulation
            ! MATSIRO uses simple linear saturation following Takata et al. (2003)
            ! Reference: Takata, K., Emori, S., and Watanabe, T. (2003). "Development of the
            ! minimal advanced treatments of surface interaction and runoff", Global and
            ! Planetary Change, 38, 209-222, doi:10.1016/S0921-8181(03)00030-4
            ! Verified against official MATSIRO source code (matsiro.f90):
            !   fctint = min(grlai(ud), 1.d0)
            ! When LAI+SAI ≤ 1: efficiency equals LAI+SAI
            ! When LAI+SAI > 1: efficiency saturates at 100%
            fpi_rain  = min(1.0, lai+sai)
            fpi_snow  = min(1.0, lai+sai)

            !-----------------------------------------------------------------------
            ! Storm area
            !-----------------------------------------------------------------------
            ldew_rain_s = ldew_rain + ((prl_rain+qflx_irrig_sprinkler) * fpi_rain + prc_rain * fpi_rain / Ac)  * deltim
            ldew_snow_s = ldew_snow + (prl_snow * fpi_snow + prc_snow * fpi_snow / Ac)  * deltim
            !
            tti_rain_s  = (prl_rain+qflx_irrig_sprinkler + prc_rain/Ac) * (1.d0-fpi_rain) * deltim
            tti_snow_s  = (prl_snow + prc_snow/Ac) * (1.d0-fpi_snow) * deltim

            !
            ! Rutter exponential drainage formula (Rutter et al. 1975)
            ! tex = overflow + k1 * exp(k2 * storage)
            !
            ! Physical constants from Rutter et al. (1975):
            ! - cwb_adrp1 = 1.14e-11 [m/s]: Base dripping coefficient
            !   Represents minimum drainage rate when canopy is near saturation
            ! - cwb_adrp2 = 3.7e3 [1/m]: Exponential saturation factor
            !   Controls how rapidly drainage increases with storage
            !   Higher values = more sensitive to storage amount
            ! - min(50.0, ...): Overflow protection to prevent EXP(large_number)
            !   Caps exponent at 50 to avoid numerical overflow
            !   (exp(50) ≈ 5e21, near double precision limit)
            !
            ! Unit conversion: 1.14e-11 [m/s] × 1000 [mm/m] = 1.14e-8 [mm/s]
            !
            tex_rain_s  = max(ldew_rain_s - satcap_rain, 0.d0) + (1.14d-11)*1000.*deltim*exp(min(50.0d0, min(ldew_rain_s,satcap_rain)/1000.* 3.7d3))
            tex_rain_s  = min(tex_rain_s, ldew_rain_s)
            ldew_rain_s = ldew_rain_s - tex_rain_s

            ! Snow drainage using same Rutter formula (see rain drainage comments above)
            tex_snow_s  = max(ldew_snow_s - satcap_snow, 0.d0) + (1.14d-11)*1000.*deltim*exp(min(50.0d0, min(ldew_snow_s,satcap_snow)/1000.0* 3.7d3))
            tex_snow_s  = min(tex_snow_s, ldew_snow_s)
            ldew_snow_s = ldew_snow_s - tex_snow_s

            !-------------------------------------------------------------------------
            ! Non-storm area
            !-------------------------------------------------------------------------
            ldew_rain_n = ldew_rain + (prl_rain+qflx_irrig_sprinkler) * fpi_rain  * deltim
            ldew_snow_n = ldew_snow + prl_snow * fpi_snow  * deltim

            !
            tti_rain_n  = (prl_rain+qflx_irrig_sprinkler) * (1.d0-fpi_rain) * deltim
            tti_snow_n  = (prl_snow) * (1.d0-fpi_snow) * deltim

            ! Rutter drainage for non-storm area (same formula as storm area)
            tex_rain_n  = max(ldew_rain_n  - satcap_rain, 0.d0) + (1.14d-11)*1000.*deltim*exp(min(50.0d0, min(ldew_rain_n,satcap_rain)/1000.* 3.7d3))
            tex_rain_n  = min(tex_rain_n, ldew_rain_n)
            ldew_rain_n = ldew_rain_n - tex_rain_n

            ! Snow drainage for non-storm area (same Rutter formula)
            tex_snow_n  =  max(ldew_snow_n - satcap_snow, 0.d0) + (1.14d-11)*1000.*deltim*exp(min(50.0d0, min(ldew_snow_n,satcap_snow)/1000.* 3.7d3))
            tex_snow_n  =  min(tex_snow_n, ldew_snow_n)
            ldew_snow_n =  ldew_snow_n - tex_snow_n
            !-------------------------------------------------------------------------
            !-------------------------------------------------------------------------
            ! Average
            !-------------------------------------------------------------------------
            ldew_rain = ldew_rain_n + (ldew_rain_s - ldew_rain_n) * Ac
            ldew_snow = ldew_snow_n + (ldew_snow_s - ldew_snow_n) * Ac
            ldew_rain = max(0.0,ldew_rain)
            ldew_snow = max(0.0,ldew_snow)

            tti_rain  = tti_rain_n*(1-Ac)+tti_rain_s*Ac
            tti_snow  = tti_snow_n+(tti_snow_s-tti_snow_n) * Ac
            tti_rain  = max(0.0,tti_rain)
            tti_snow  = max(0.0,tti_snow)

            tex_rain  = tex_rain_n+(tex_rain_s-tex_rain_n)*Ac
            tex_snow  = tex_snow_n+(tex_snow_s-tex_snow_n)*Ac
            tex_rain  = max(0.0,tex_rain)
            tex_snow  = max(0.0,tex_snow)
            !-------------------------------------------------------------------------

! NOTE: The check "tex+tti > p0" is not applicable to MATSIRO scheme.
! Rutter exponential drainage drains pre-existing canopy water (ldew),
! so tex+tti can legitimately exceed p0. The real mass balance check
! is performed below (w residual check with abort).

         ELSE
            ! all intercepted by canopy leaves for very small precipitation
            tti_rain = 0.
            tti_snow = 0.
            tex_rain = 0.
            tex_snow = 0.
         ENDIF

         !BDFALL = 67.92+51.25*EXP(MIN(2.5,(SFCTMP-TFRZ))/2.59)

         !----------------------------------------------------------------------
         !   total throughfall (thru) and store augmentation
         !----------------------------------------------------------------------

         thru_rain = tti_rain + tex_rain
         thru_snow = tti_snow + tex_snow
         pinf = p0 - (thru_rain + thru_snow)

         ! Resync ldew with components following CoLM2014 pattern (line 324)
         ! In the precip case, ldew_rain/ldew_snow were updated via weighted average
         ! In the no-precip case, they remain at post-phase-change values (tiny p0 < 1e-8 ignored)
         ldew = ldew_rain + ldew_snow

         pg_rain = (xsc_rain + thru_rain) / deltim
         pg_snow = (xsc_snow + thru_snow) / deltim
         qintr   = pinf / deltim

         qintr_rain = prc_rain + prl_rain + qflx_irrig_sprinkler - thru_rain / deltim
         qintr_snow = prc_snow + prl_snow - thru_snow / deltim
#if (defined CoLMDEBUG)
         w = w - ldew - (pg_rain+pg_snow)*deltim
         IF (abs(w) > INTERCEPTION_BALANCE_TOL) THEN
            write(6,*) 'something wrong in interception code : '
            write(6,*) w, ldew, (pg_rain+pg_snow)*deltim !, satcap
            CALL CoLM_stop()
         ENDIF

         CALL check_interception_balance('MATSIRO', &
              ldew, ldew_rain, ldew_snow, pg_rain, pg_snow, &
              qintr, qintr_rain, qintr_snow)
#endif

      ELSE
         ! No vegetation: all precipitation passes through, release any stored water
         pg_rain = prc_rain + prl_rain + qflx_irrig_sprinkler + ldew_rain/deltim
         pg_snow = prc_snow + prl_snow + ldew_snow/deltim

         ldew       = 0.
         ldew_rain  = 0.
         ldew_snow  = 0.
         qintr      = 0.
         qintr_rain = 0.
         qintr_snow = 0.
      ENDIF
   END SUBROUTINE LEAF_interception_MATSIRO

   SUBROUTINE LEAF_interception_VIC (deltim,dewmx,forc_us,forc_vs,chil,sigf,lai,sai,tair,tleaf, &
                                       prc_rain,prc_snow,prl_rain,prl_snow,qflx_irrig_sprinkler,&
                                       ldew,ldew_rain,ldew_snow,z0m,hu,pg_rain,&
                                       pg_snow,qintr,qintr_rain,qintr_snow)
!DESCRIPTION
!===========
   ! Calculation of  interception and drainage of precipitation
   ! the treatment are based on VIC 5.0 (under development)

!Original Author:
!-------------------
   !---Hamman, J.J. AND Liang X.

!References:
!-------------------
   !---Hamman, J.J., Nijssen, B., Bohn, T.J., Gergel, D.R. and Mao, Y., 2018.
   !   The Variable Infiltration Capacity model version 5 (VIC-5): Infrastructure
   !   improvements for new applications and reproducibility. Geoscientific Model Development,
   !   11(8), pp.3481-3496.
   !---Liang, X., Lettenmaier, D.P., Wood, E.F. and Burges, S.J., 1994.
   !   A simple hydrologically based model of land surface water and energy fluxes
   !   for general circulation models. Journal of Geophysical Research: Atmospheres, 99(D7),
   !   pp.14415-14428.

!ANCILLARY FUNCTIONS AND SUBROUTINES
!-------------------

!REVISION HISTORY
!----------------
   ! 2026.02.11  Zhongwang Wei @ SYSU - Added input clamping, comment fixes
   ! 2023.02.21  Zhongwang Wei @ SYSU
   ! 2021.12.08  Zhongwang Wei @ SYSU
!=======================================================================


   IMPLICIT NONE

   real(r8), intent(in) :: deltim       !seconds in a time step [second]
   real(r8), intent(in) :: dewmx        !maximum dew [mm]
   real(r8), intent(in) :: forc_us      !wind speed
   real(r8), intent(in) :: forc_vs      !wind speed
   real(r8), intent(in) :: chil         !leaf angle distribution factor
   real(r8), intent(in) :: prc_rain     !convective rainfall [mm/s]
   real(r8), intent(in) :: prc_snow     !convective snowfall [mm/s]
   real(r8), intent(in) :: prl_rain     !large-scale rainfall [mm/s]
   real(r8), intent(in) :: prl_snow     !large-scale snowfall [mm/s]
   real(r8), intent(in) :: qflx_irrig_sprinkler !irrigation and sprinkler water flux [mm/s]
   real(r8), intent(in) :: sigf         !fraction of veg cover, excluding snow-covered veg [-]
   real(r8), intent(in) :: lai          !leaf area index [-]
   real(r8), intent(in) :: sai          !stem area index [-]
   real(r8), intent(in) :: tair         !air temperature [K]
   real(r8), intent(inout) :: tleaf     !sunlit canopy leaf temperature [K]

   real(r8), intent(inout) :: ldew      !depth of water on foliage [mm]
   real(r8), intent(inout) :: ldew_rain !depth of liquid on foliage [mm]
   real(r8), intent(inout) :: ldew_snow !depth of solid (frozen) on foliage [mm]
   real(r8), intent(in) :: z0m          !roughness length
   real(r8), intent(in) :: hu           !forcing height of U


   real(r8), intent(out) :: pg_rain     !rainfall onto ground including canopy runoff [kg/(m2 s)]
   real(r8), intent(out) :: pg_snow     !snowfall onto ground including canopy runoff [kg/(m2 s)]
   real(r8), intent(out) :: qintr       !interception [kg/(m2 s)]
   real(r8), intent(out) :: qintr_rain  !rainfall interception (mm h2o/s)
   real(r8), intent(out) :: qintr_snow  !snowfall interception (mm h2o/s)

   real(r8) :: Imax1,Lr,Snow,Rain,DeltaSnowInt,Wind,BlownSnow,sigf_safe
   real(r8) :: MaxInt,Overload,IntRainFract,IntSnowFract,ldew_smelt,MaxWaterInt

      IF (lai+sai > 1e-6 .and. sigf > 1.e-6) THEN
         lsai   = lai + sai
         vegt   = lsai

         ! Ensure ldew is consistent with components at entry (grid-scale)
         ! VIC sets ldew = ldew_rain + ldew_snow at exit; inconsistency at entry
         ! from initialization or restart causes mass balance check to fail
         ldew = ldew_rain + ldew_snow

         ! VIC vegetation fraction handling (snow_intercept.c line 132-133)
         ! Convert grid-scale storage to per-vegetation values
         ! Physical meaning: Storage variables represent water on vegetated fraction only
         IF (sigf > 1.e-6) THEN
            sigf_safe = max(sigf, 0.01)
            ldew_rain = ldew_rain / sigf_safe
            ldew_snow = ldew_snow / sigf_safe
         ENDIF

         !the maximum bearing  capacity of the tree regardless of air temp (Imax1)
         Imax1=4.0*lsai*0.0005 *1000.0 ! in mm
         MaxInt=0.1*lsai
         IF (tair>-272.15) THEN
            Lr=4.0
         ELSEIF (tair<=-272.15 .and. tair>=-270.15) THEN
            Lr=1.5*(tair-273.15)+5.5
         ELSE
            Lr=1.0
         ENDIF

         satcap_snow=0.0005 *Lr *lsai * 1000.0  ! in mm !!!
         !/* Calculate total liquid water capacity on branches and in intercepted snow */
         ! VIC physical design: Total liquid water capacity includes two components:
         ! 1. Liquid water held in snow matrix (mature/ripe snow at 0°C)
         ! 2. Liquid water on leaf surfaces
         !
         ! Physical basis: Intercepted snow is a porous medium that can retain liquid water
         ! in its interstitial spaces when it reaches 0°C (mature/ripe snow state).
         ! This is a fundamental concept in snow hydrology (Colbeck 1972, Jordan 1991).
         !
         ! Formula: satcap_rain = SNOW_LIQUID_WATER_CAPACITY * ldew_snow + MaxInt
         ! - Term 1 (0.035*ldew_snow): Irreducible liquid water content in snow matrix
         !   The 0.035 coefficient represents typical irreducible water saturation (~3.5% by mass)
         !   This same parameter is used for ground snowpack in VIC (snow_melt.c, ice_melt.c)
         ! - Term 2 (MaxInt=0.1*lsai): Liquid water on leaf/branch surfaces
         !
         ! Physical meaning: When intercepted snow becomes ripe (0°C), it can simultaneously hold:
         ! - Solid ice framework (ldew_snow)
         ! - Liquid water in pore spaces (up to 3.5% of snow mass)
         ! - Additional liquid water on vegetation surfaces (MaxInt)
         ! When snow completely melts (ldew_snow=0), capacity reverts to leaf-only (MaxInt)
         !
         ! Reference: Andreadis et al. (2009) "Modeling snow accumulation and ablation
         ! processes in forested environments", WRR, doi:10.1029/2008WR007042
         !
         ! Rain capacity = snow matrix water retention + leaf surface capacity
         ! When snow melts completely (ldew_snow→0), capacity reverts to just MaxInt
         satcap_rain = 0.035 * ldew_snow + MaxInt  ! in mm

         ! Input clamping: prevent negative precipitation (numerical noise)
         ! from causing mass balance failures
         p0  = MAX(0.0_r8, prc_rain + prc_snow + prl_rain + prl_snow + qflx_irrig_sprinkler) * deltim
         ppc = MAX(0.0_r8, prc_rain + prc_snow) * deltim
         ppl = MAX(0.0_r8, p0 - ppc)
         w = ldew+p0

         xsc_rain   = max(0., ldew_rain-satcap_rain)
         xsc_snow   = max(0., ldew_snow-satcap_snow)

         ldew_rain  = ldew_rain-xsc_rain
         ldew_snow  = ldew_snow-xsc_snow
         ! phase change and excess !
         IF (tleaf > tfrz) THEN
            IF (ldew_snow>1.e-8) THEN
               ldew_smelt = MIN(ldew_snow,(tleaf-tfrz)*CICE*ldew_snow/DENICE/(HFUS))
               ldew_smelt = MAX(ldew_smelt,0.0)
               ldew_snow  = ldew_snow-ldew_smelt
               ldew_rain  = ldew_rain+ldew_smelt
               xsc_rain   = xsc_rain  + MAX(0., ldew_rain-satcap_rain)
               ldew_rain  = ldew_rain - MAX(0., ldew_rain-satcap_rain)
            ENDIF
            ! tleaf      = fvegc*tfrz+ (1.0-fwet)*tleaf
         ELSE
            IF (ldew_rain>1.e-8) THEN
               ldew_frzc  = MIN(ldew_rain,(tfrz-tleaf)*CWAT*ldew_rain/DENH2O/(HFUS))
               ldew_frzc  = MAX(ldew_frzc,0.0)
               ldew_snow  = ldew_snow+ldew_frzc
               ldew_rain  = ldew_rain-ldew_frzc
               xsc_snow   = xsc_snow  + MAX(0., ldew_snow-satcap_snow)
               ldew_snow  = ldew_snow - MAX(0., ldew_snow-satcap_snow)
            ENDIF
            !tleaf      = fvegc*tfrz+ (1.0-fwet)*tleaf
         ENDIF

         ! Note: ldew will be resynced as ldew = ldew_rain + ldew_snow at output (line ~1806)
         ! No in-place ldew update needed here (CoLM2014 pattern: resync at end)

         IF (p0 > 1.e-8) THEN
            ! VIC physical interception algorithm (snow_intercept.c lines 165-176, 224-236)
            ! Snow: Dynamic capacity-based model
            ! Rain: Empirical efficiency (CLM5 formulation retained for liquid phase)

            ! Snow interception: VIC physical algorithm
            ! Interception efficiency decreases as canopy snow load approaches capacity
            ! This prevents unphysical continuous interception when branches are saturated
            Snow = (prc_snow+prl_snow)*deltim
            IF (satcap_snow > 1.e-6 .and. Snow > 1.e-8) THEN
               ! DeltaSnowInt = (1 - IntSnow/MaxSnowInt) * SnowFall
               ! Physical meaning: Interception efficiency = available capacity / max capacity
               DeltaSnowInt = (1.0 - ldew_snow/satcap_snow) * Snow

               ! Ensure intercepted amount doesn't exceed available capacity
               IF (DeltaSnowInt + ldew_snow > satcap_snow) THEN
                  DeltaSnowInt = satcap_snow - ldew_snow
               ENDIF

               ! Ensure non-negative
               IF (DeltaSnowInt < 0.0) THEN
                  DeltaSnowInt = 0.0
               ENDIF
            ELSE
               DeltaSnowInt = 0.0
            ENDIF

            ! VIC throughfall calculation (snow_intercept.c line 204)
            ! Throughfall = vegetation area unintercepted + bare area all
            ! Physical meaning:
            !   - In vegetated fraction (sigf): only non-intercepted part passes through
            !   - In bare fraction (1-sigf): all precipitation passes through
            !
            ! Use sigf_safe consistently with the storage scaling above. Mixing sigf_safe
            ! for state variables with raw sigf for throughfall creates small residuals
            ! in the debug mass-balance check when sigf is very small.
            tti_snow = (Snow - DeltaSnowInt) * sigf_safe + Snow * (1.0 - sigf_safe)
            ldew_snow = ldew_snow + DeltaSnowInt

            ! Rain interception: Original VIC capacity-based algorithm
            ! Physical mechanism: Rain is intercepted based on available canopy storage capacity,
            ! not a fixed efficiency function. When capacity is available, rain is intercepted;
            ! when saturated, excess drains as throughfall.
            ! Reference: Andreadis et al. (2009) WRR, VIC snow_intercept.c lines 218-236
            ! This differs from CLM5's empirical efficiency approach (tanh function)
            Rain = (prc_rain+prl_rain+ qflx_irrig_sprinkler)*deltim
            MaxWaterInt = satcap_rain  ! Capacity already computed at line 1538

            ! Capacity-based interception (VIC original algorithm)
            ! If there is available capacity, intercept rain; otherwise it becomes throughfall
            IF (ldew_rain + Rain <= MaxWaterInt) THEN
               ! All rain can be intercepted (capacity not exceeded)
               ldew_rain = ldew_rain + Rain
               ! Throughfall: only bare area contribution
               tti_rain = Rain * (1.0 - sigf_safe)
            ELSE
               ! Capacity exceeded: excess becomes throughfall
               ! Throughfall = vegetated area excess + bare area all
               tti_rain = (ldew_rain + Rain - MaxWaterInt) * sigf_safe + Rain * (1.0 - sigf_safe)
               ! Storage saturated at maximum capacity
               ldew_rain = MaxWaterInt
            ENDIF

            tex_rain    = max(0.0,ldew_rain-satcap_rain)
            tex_snow    = max(0.0,ldew_snow-satcap_snow)

            ldew_rain   = ldew_rain - tex_rain
            ldew_snow   = ldew_snow - tex_snow

            !unload of snow
            !* Reduce the amount of intercepted snow if windy and cold.
            !Ringyo Shikenjo Tokyo, #54, 1952.
            !Bulletin of the Govt. Forest Exp. Station,
            !Govt. Forest Exp. Station, Meguro, Tokyo, Japan.
            !FORSTX 634.9072 R475r #54.
            !Page 146, Figure 10.

            !Reduce the amount of intercepted snow if snowing, windy, and
            !cold (< -3 to -5 C).
            !Schmidt and Troendle 1992 western snow conference paper. */
            !Note: Use tair (air temperature) instead of tleaf to match
            !the original observations by Storck et al. (2002)
            Wind= SQRT(forc_us*forc_us + forc_vs*forc_vs)
            IF (tair-273.15<-3.0 .and. Wind> 1.0) THEN
               BlownSnow=(0.2*Wind -0.2)* ldew_snow
               BlownSnow = min(ldew_snow,BlownSnow)
               tex_snow    =  tex_snow  + BlownSnow
               ldew_snow   =  ldew_snow - BlownSnow
            ENDIF
            !/* at this point we have calculated the amount of snowfall intercepted and
            !/* the amount of rainfall intercepted.  These values have been
            !/* appropriately subtracted from SnowFall and RainFall to determine
            !/* SnowThroughfall and RainThroughfall.  However, we can end up with the
            !/* condition that the total intercepted rain plus intercepted snow is
            !/* greater than the maximum bearing capacity of the tree regardless of air
            !/* temp (Imax1).  The following routine will adjust ldew_rain and ldew_snow
            !/* by triggering mass release due to overloading.  Of course since ldew_rain
            !/* and ldew_snow are mixed, we need to slough them of as fixed fractions  */
            IF (ldew_rain + ldew_snow > Imax1) THEN
               ! /*THEN trigger structural unloading*/
               Overload = (ldew_snow + ldew_rain) - Imax1
               ! Prevent division by zero in extreme low LAI conditions
               IF (ldew_rain + ldew_snow > 1.e-10) THEN
                  IntRainFract = ldew_rain / (ldew_rain + ldew_snow)
                  IntSnowFract = 1.0 - IntRainFract
               ELSE
                  ! Default to equal partition when total is negligible
                  IntRainFract = 0.5
                  IntSnowFract = 0.5
               ENDIF
               ldew_rain = ldew_rain - Overload * IntRainFract
               ldew_snow = ldew_snow - Overload * IntSnowFract
               tex_rain  = tex_rain  + Overload*IntRainFract
               tex_snow  = tex_snow  + Overload*IntSnowFract
            ENDIF

! NOTE: The check "tex+tti > p0" is not applicable to VIC scheme.
! VIC's tex includes drainage of pre-existing canopy water (ldew) from
! capacity overflow, wind unloading, and structural overloading.
! Additionally, tti includes bare-fraction precipitation.
! The real mass balance check is performed below (w residual check with abort).

         ELSE
            ! all intercepted by canopy leaves for very small precipitation
            tti_rain = 0.
            tti_snow = 0.
            tex_rain = 0.
            tex_snow = 0.
         ENDIF


         ! tex_rain/tex_snow are per-vegetation quantities, must scale by sigf_safe
         ! to convert to grid-scale before adding to grid-scale tti_rain/tti_snow
         thru_rain = tti_rain + tex_rain * sigf_safe
         thru_snow = tti_snow + tex_snow * sigf_safe

         ! VIC safety check: When snow completely melts, liquid water capacity
         ! reverts from (0.035*ldew_snow + MaxInt) to just (MaxInt)
         ! Must drain excess water that can no longer be held
         ! Reference: VIC snow_intercept.c lines 522-526
         IF (ldew_snow < 1.e-6 .and. ldew_rain > MaxInt) THEN
            thru_rain = thru_rain + (ldew_rain - MaxInt) * sigf_safe
            ldew_rain = MaxInt
         ENDIF

         ! VIC vegetation fraction handling (snow_intercept.c line 515-520)
         ! Convert per-vegetation storage back to grid-scale values
         ! Use sigf_safe consistently with the division at entry (line 1534-1535)
         IF (sigf > 1.e-6) THEN
            ldew_rain = ldew_rain * sigf_safe
            ldew_snow = ldew_snow * sigf_safe
         ENDIF

         ! Update total canopy water storage (grid-scale)
         ldew = ldew_rain + ldew_snow
         pinf = p0 - (thru_rain + thru_snow)

         ! xsc_rain/xsc_snow are per-vegetation, scale to grid-scale
         pg_rain = (xsc_rain * sigf_safe + thru_rain) / deltim
         pg_snow = (xsc_snow * sigf_safe + thru_snow) / deltim
         qintr   = pinf / deltim

         qintr_rain = prc_rain + prl_rain + qflx_irrig_sprinkler - thru_rain / deltim
         qintr_snow = prc_snow + prl_snow - thru_snow / deltim
#if (defined CoLMDEBUG)
         w = w - ldew - (pg_rain+pg_snow)*deltim
         IF (abs(w) > INTERCEPTION_BALANCE_TOL) THEN
            write(6,*) 'something wrong in interception code : '
            write(6,*) w, ldew, (pg_rain+pg_snow)*deltim !, satcap
            CALL CoLM_stop()
         ENDIF

         CALL check_interception_balance('VIC', &
              ldew, ldew_rain, ldew_snow, pg_rain, pg_snow, &
              qintr, qintr_rain, qintr_snow)
#endif

      ELSE
         ! No vegetation: all precipitation passes through, release any stored water
         pg_rain = prc_rain + prl_rain + qflx_irrig_sprinkler + ldew_rain/deltim
         pg_snow = prc_snow + prl_snow + ldew_snow/deltim

         ldew       = 0.
         ldew_rain  = 0.
         ldew_snow  = 0.
         qintr      = 0.
         qintr_rain = 0.
         qintr_snow = 0.
      ENDIF
   END SUBROUTINE LEAF_interception_VIC

   SUBROUTINE LEAF_interception_JULES(deltim,dewmx,forc_us,forc_vs,chil,sigf,lai,sai,tair,tleaf, &
                                       prc_rain,prc_snow,prl_rain,prl_snow,qflx_irrig_sprinkler,&
                                       ldew,ldew_rain,ldew_snow,z0m,hu,pg_rain,pg_snow,qintr,qintr_rain,qintr_snow)
   !DESCRIPTION
   !===========
      ! Official JULES canopy interception scheme
      ! Rain: Rutter (1971) penetration model (sieve_jls_mod.F90)
      ! Snow: Exponential saturation model with unloading (canopysnow_mod.F90)
      !
      ! 2026-02 Fixes:
      ! - Added vegetation fraction (sigf) scaling: interception occurs on vegetated area only.
      ! - Added non-negative clamping for precipitation inputs to ensure mass balance.
      ! - Fixed mass balance check: ldew resync before sigf division (VIC pattern).

   !Original Author:
   !-------------------
      !---Rutter et al. (1971, 1975) - Rain interception model
      !---JULES team (Best et al. 2011) - Snow interception model
      !---Zhongwang Wei @ SYSU - CoLM implementation

   !References:
   !-------------------
      !---Rutter et al. (1971): A predictive model of rainfall interception in forests, 1.
      !   Derivation of the model from observations in a plantation of Corsican pine.
      !   Agricultural Meteorology, 9, 367-384.
      !---Rutter et al. (1975): A predictive model of rainfall interception in forests, 2.
      !   Generalization of the model and comparison with observations in some coniferous
      !   and hardwood stands. Journal of Applied Ecology, 12, 367-380.
      !---Best et al. (2011): The Joint UK Land Environment Simulator (JULES), model description -
      !   Part 1: Energy and water fluxes. Geosci. Model Dev. 4:677-699.
      !---Clark et al. (2011): The Joint UK Land Environment Simulator (JULES), model description -
      !   Part 2: Carbon fluxes and vegetation dynamics. Geosci. Model Dev. 4:701-722.

   !ANCILLARY FUNCTIONS AND SUBROUTINES
   !-------------------

   !REVISION HISTORY
   !----------------
      ! 2026.02.11  Zhongwang Wei @ SYSU - Added wind-dependent snow unloading (JULES fidelity D3)
      ! 2026.02.11  Zhongwang Wei @ SYSU - Added sigf scaling, input clamping, mass balance fix
      ! 2026.01.16  Zhongwang Wei @ SYSU - Converted to official JULES Rutter model
      ! 2023.02.21  Zhongwang Wei @ SYSU
      ! 2021.12.08  Zhongwang Wei @ SYSU
   !=======================================================================

   IMPLICIT NONE

   real(r8), intent(in)    :: deltim     !seconds in a time step [second]
   real(r8), intent(in)    :: dewmx      !maximum dew [mm] (unused in JULES; retained for interface compatibility)
   real(r8), intent(in)    :: forc_us    !wind speed [m/s]
   real(r8), intent(in)    :: forc_vs    !wind speed [m/s]
   real(r8), intent(in)    :: chil       !leaf angle distribution factor (unused in JULES)
   real(r8), intent(in)    :: prc_rain   !convective rainfall [mm/s]
   real(r8), intent(in)    :: prc_snow   !convective snowfall [mm/s]
   real(r8), intent(in)    :: prl_rain   !large-scale rainfall [mm/s]
   real(r8), intent(in)    :: prl_snow   !large-scale snowfall [mm/s]
   real(r8), intent(in)    :: qflx_irrig_sprinkler !irrigation and sprinkler water flux [mm/s]
   real(r8), intent(in)    :: sigf       !fraction of veg cover, excluding snow-covered veg [-]
   real(r8), intent(in)    :: lai        !leaf area index [-]
   real(r8), intent(in)    :: sai        !stem area index [-]
   real(r8), intent(in)    :: tair       !air temperature [K]
   real(r8), intent(inout) :: tleaf      !sunlit canopy leaf temperature [K] (read-only in JULES)

   real(r8), intent(inout) :: ldew       !depth of water on foliage [mm]
   real(r8), intent(inout) :: ldew_rain  !depth of liquid on foliage [mm]
   real(r8), intent(inout) :: ldew_snow  !depth of solid on foliage [mm]
   real(r8), intent(in)    :: z0m        !roughness length (unused in JULES)
   real(r8), intent(in)    :: hu         !forcing height of U (unused in JULES)

   real(r8), intent(out)   :: pg_rain    !rainfall onto ground including canopy runoff [kg/(m2 s)]
   real(r8), intent(out)   :: pg_snow    !snowfall onto ground including canopy runoff [kg/(m2 s)]
   real(r8), intent(out)   :: qintr      !interception [kg/(m2 s)]
   real(r8), intent(out)   :: qintr_rain !rainfall interception (mm h2o/s) [NOTE: can be negative during canopy release]
   real(r8), intent(out)   :: qintr_snow !snowfall interception (mm h2o/s) [NOTE: can be negative during canopy release]

   ! Local variables
   real(r8)                :: snowinterceptfact    ! Snow interception efficiency (0.7)
   real(r8)                :: snowunloadfact       ! Snow unloading factor due to melt (0.4)
   real(r8)                :: unload_rate_cnst     ! Constant background unloading rate [s⁻¹]
   real(r8)                :: unload_rate_u        ! Wind-dependent unloading rate [s⁻¹/(m/s)]
   real(r8)                :: unload_backgrnd      ! Total background unloading rate [s⁻¹]
   real(r8)                :: Wind                 ! Wind speed [m/s]
   real(r8)                :: area                 ! Precipitation area fraction
   real(r8)                :: can_cpy_rain         ! Canopy capacity for rain [mm]
   real(r8)                :: can_cpy_snow         ! Canopy capacity for snow [mm]
   real(r8)                :: r_rain               ! Rain rate [mm/s] (clamped, non-negative)
   real(r8)                :: r_snow               ! Snow rate [mm/s] (clamped, non-negative)
   real(r8)                :: can_ratio            ! Canopy saturation ratio (can_wcnt/can_cpy)
   real(r8)                :: aexp                 ! Exponential term in Rutter model
   real(r8)                :: tfall_rain           ! Rain throughfall [mm/s]
   real(r8)                :: tfall_snow           ! Snow throughfall [mm/s]
   real(r8)                :: intercept_rain       ! Rain interception in timestep [mm]
   real(r8)                :: intercept_snow       ! Snow interception in timestep [mm]
   real(r8)                :: unload_snow          ! Snow unloading in timestep [mm]
   real(r8)                :: melt_rate            ! Canopy snow melt rate [mm/s]
   real(r8)                :: melt_factor          ! Dimensionless melt energy ratio: CICE/(DENICE*HFUS)
   real(r8)                :: frz_factor           ! Dimensionless freeze energy ratio: CWAT/(DENH2O*HFUS)
   real(r8)                :: smallp               ! Small positive number
   real(r8)                :: lsai_l               ! total LAI+SAI (local)
   real(r8)                :: p0_l, ppc_l, ppl_l   ! precipitation sums (local)
   real(r8)                :: w_l                   ! mass balance check variable (local)
   real(r8)                :: ldew_frzc            ! freezing water amount
   real(r8)                :: xsc_rain, xsc_snow   ! excess water drained after phase change
   real(r8)                :: sigf_safe            ! safe vegetation fraction (>= 0.01)
   real(r8)                :: thru_rain, thru_snow ! grid-scale throughfall [mm]

      IF (lai+sai > 1e-6 .AND. sigf > 1.e-6) THEN
         lsai_l = lai + sai

         !======================================================================
         ! Input Clamping (Mass Balance Safety)
         !======================================================================
         ! Negative precipitation inputs (numerical noise) cause mass balance failures.
         ! Clamp all inputs to 0.0 before any calculations.
         r_rain = MAX(0.0_r8, prc_rain + prl_rain + qflx_irrig_sprinkler)
         r_snow = MAX(0.0_r8, prc_snow + prl_snow)

         ! Clamp canopy state: negative values from restart or upstream bugs
         ! would be amplified by sigf division and cause mass balance abort
         ldew_rain = MAX(0.0_r8, ldew_rain)
         ldew_snow = MAX(0.0_r8, ldew_snow)

         !======================================================================
         ! JULES Parameters - Official values from JULES source code
         !======================================================================
         snowinterceptfact = 0.7       ! Snow interception efficiency (jules_snow_mod.F90)
         snowunloadfact    = 0.4       ! Snow unloading factor (canopysnow_mod.F90)
         unload_rate_cnst  = 2.31e-6   ! Constant background unloading rate [s⁻¹]
         unload_rate_u     = 5.56e-7   ! Wind-dependent unloading rate [s⁻¹/(m/s)]
         Wind = SQRT(forc_us**2 + forc_vs**2)
         unload_backgrnd   = unload_rate_cnst + unload_rate_u * Wind
         can_cpy_snow      = 4.4 * lsai_l  ! Snow capacity [mm] (snowloadlai parameter)
         can_cpy_rain      = 0.1 * lsai_l  ! Rain capacity [mm] (JULES PFT parameter)
         smallp = EPSILON(1.0_r8)      ! Machine epsilon for numerical stability

         !======================================================================
         ! Precipitation totals and mass balance reference (GRID-SCALE)
         !======================================================================
         ! Use clamped rates for consistency
         p0_l  = (r_rain + r_snow) * deltim
         ppc_l = MAX(0.0_r8, prc_rain + prc_snow) * deltim
         ppl_l = p0_l - ppc_l
         ! Clamp ppl_l to avoid negative from clamping differences
         ppl_l = MAX(0.0_r8, ppl_l)

         IF (p0_l > 1.e-8) THEN
            ! Convective precip ~10% of grid, stratiform ~100% of grid
            area = (0.1*ppc_l + 1.0*ppl_l) / p0_l
            area = max(0.1, min(1.0, area))
         ELSE
            area = 1.0
         ENDIF

         ! Ensure ldew is consistent with components at entry (GRID-SCALE)
         ! Must be done BEFORE sigf division to keep w_l in grid-scale units
         ldew = ldew_rain + ldew_snow

         ! Mass balance reference: grid-scale storage + grid-scale precipitation
         w_l = ldew + p0_l

         !======================================================================
         ! Vegetation Fraction Scaling (sigf)
         !======================================================================
         ! JULES physics operates on the vegetated area only.
         ! Convert grid-averaged storage to per-vegetation values.
         ! (Matching VIC pattern: divide before physics, multiply after)
         ! Note: outer guard guarantees sigf > 1e-6; floor at 0.01 prevents
         ! extreme amplification when sigf is very small but positive.
         sigf_safe = max(sigf, 0.01_r8)
         ldew_rain = ldew_rain / sigf_safe
         ldew_snow = ldew_snow / sigf_safe

         !======================================================================
         ! Phase change (melting/freezing) - Do BEFORE interception
         !======================================================================
         ! Pre-compute dimensionless energy ratios to avoid large intermediate
         ! products (e.g. CICE*ldew_snow ~1e8) that can trap under -ffpe-trap.
         ! melt_factor = CICE / (DENICE * HFUS) ≈ 0.00684 [K⁻¹]
         ! frz_factor  = CWAT / (DENH2O * HFUS) ≈ 0.01256 [K⁻¹]
         melt_factor = CICE / (DENICE * HFUS)
         frz_factor  = CWAT / (DENH2O * HFUS)

         IF (tleaf > tfrz) THEN
            ! Canopy snow melting
            IF (ldew_snow > 1.e-8) THEN
               melt_rate = MIN(ldew_snow/deltim, &
                    (tleaf - tfrz) * melt_factor * ldew_snow / deltim)
               melt_rate = MAX(melt_rate, 0.0_r8)
               ldew_snow = ldew_snow - melt_rate * deltim
               ldew_snow = MAX(ldew_snow, 0.0_r8)  ! prevent -eps from FP rounding
               ldew_rain = ldew_rain + melt_rate * deltim
            ELSE
               melt_rate = 0.0_r8
            ENDIF
         ELSE
            ! Canopy rain freezing
            IF (ldew_rain > 1.e-8) THEN
               ldew_frzc = MIN(ldew_rain, &
                    (tfrz - tleaf) * frz_factor * ldew_rain)
               ldew_frzc = MAX(ldew_frzc, 0.0_r8)
               ldew_snow = ldew_snow + ldew_frzc
               ldew_rain = ldew_rain - ldew_frzc
               ldew_rain = MAX(ldew_rain, 0.0_r8)  ! prevent -eps from FP rounding
            ENDIF
            melt_rate = 0.0_r8
         ENDIF

         !======================================================================
         ! Drain excess water after phase change
         ! When snow melts to rain, ldew_rain can greatly exceed can_cpy_rain
         ! When rain freezes to snow, ldew_snow can exceed can_cpy_snow
         !======================================================================
         xsc_rain = 0.0
         xsc_snow = 0.0
         IF (ldew_rain > can_cpy_rain) THEN
            xsc_rain  = ldew_rain - can_cpy_rain
            ldew_rain = can_cpy_rain
         ENDIF
         IF (ldew_snow > can_cpy_snow) THEN
            xsc_snow  = ldew_snow - can_cpy_snow
            ldew_snow = can_cpy_snow
         ENDIF

         !======================================================================
         ! RAIN INTERCEPTION: Rutter (1971) Penetration Model
         ! From JULES sieve_jls_mod.F90 lines 125-142
         !======================================================================
         IF (can_cpy_rain > 0.0 .AND. r_rain > smallp) THEN
            ! Exponential term (JULES lines 126-132)
            aexp = exp(max(-50.0_r8, -area * can_cpy_rain / (r_rain * deltim)))

            ! Canopy saturation ratio (JULES lines 134-136)
            can_ratio = ldew_rain / can_cpy_rain
            can_ratio = MAX(0.0, MIN(can_ratio, 1.0))

            ! Rutter throughfall formula (JULES line 137)
            tfall_rain = r_rain * ((1.0 - can_ratio) * aexp + can_ratio)
         ELSE
            tfall_rain = r_rain
         ENDIF

         ! Update canopy water content (JULES line 142)
         intercept_rain = (r_rain - tfall_rain) * deltim
         ldew_rain = ldew_rain + intercept_rain

         ! Post-Rutter drainage: discrete timestep can overshoot capacity
         IF (ldew_rain > can_cpy_rain) THEN
            tfall_rain = tfall_rain + (ldew_rain - can_cpy_rain) / deltim
            ldew_rain  = can_cpy_rain
         ENDIF

         !======================================================================
         ! SNOW INTERCEPTION: Exponential Saturation Model with Unloading
         ! From JULES canopysnow_mod.F90 lines 131-145
         !======================================================================
         ! Snow unloading occurs regardless of snowfall (continuous process)
         unload_snow = snowunloadfact * melt_rate * deltim &
                     + unload_backgrnd * ldew_snow * deltim
         unload_snow = MAX(MIN(unload_snow, ldew_snow), 0.0)
         ldew_snow   = ldew_snow - unload_snow

         IF (r_snow > smallp) THEN
            ! Snow interception (JULES lines 131-132)
            intercept_snow = snowinterceptfact * (can_cpy_snow - ldew_snow) * &
                             (1.0 - EXP(MAX(-50.0_r8, -r_snow * deltim / can_cpy_snow)))
            intercept_snow = MAX(0.0, intercept_snow)

            ! Update canopy snow
            ldew_snow = ldew_snow + intercept_snow

            ! Snowfall to ground = snowfall - intercepted + unloaded
            tfall_snow = r_snow - intercept_snow / deltim + unload_snow / deltim

            ! Post-interception drainage
            IF (ldew_snow > can_cpy_snow) THEN
               tfall_snow = tfall_snow + (ldew_snow - can_cpy_snow) / deltim
               ldew_snow  = can_cpy_snow
            ENDIF
         ELSE
            intercept_snow = 0.0
            ! No snowfall, but unloaded snow still reaches ground
            tfall_snow = r_snow + unload_snow / deltim
         ENDIF

         !======================================================================
         ! Output fluxes: Per-vegetation → Grid-scale (VIC pattern)
         !======================================================================
         ! tfall_rain/tfall_snow are per-vegetation throughfall rates [mm/s]
         ! xsc_rain/xsc_snow are per-vegetation excess [mm]
         ! Combine: vegetated area throughfall + bare ground direct precipitation

         ! Grid-scale throughfall [mm] (for mass balance check)
         ! IMPORTANT: Use sigf_safe consistently (not sigf) to ensure exact mass balance
         ! When sigf < 0.01, sigf_safe = 0.01 != sigf, mixing them creates a residual
         thru_rain = (tfall_rain * deltim + xsc_rain) * sigf_safe + r_rain * deltim * (1.0 - sigf_safe)
         thru_snow = (tfall_snow * deltim + xsc_snow) * sigf_safe + r_snow * deltim * (1.0 - sigf_safe)

         ! Convert throughfall to rate [mm/s]
         pg_rain = thru_rain / deltim
         pg_snow = thru_snow / deltim

         ! Rescale state variables back to grid-averaged
         ldew_rain = ldew_rain * sigf_safe
         ldew_snow = ldew_snow * sigf_safe

         ! Update total canopy water (grid-scale)
         ldew = ldew_rain + ldew_snow

         ! Interception = total input - total output - storage change
         ! Using the VIC approach: qintr = (p0 - thru) / deltim
         qintr = (p0_l - thru_rain - thru_snow) / deltim

         ! Phase-separated interception rates
         ! NOTE: These can be NEGATIVE when pre-existing canopy storage drains
         ! (e.g., excess from phase change or prior timestep). Negative values
         ! represent net canopy release, not a mass balance error.
         ! Algebraic identity: qintr_rain + qintr_snow == qintr (exact).
         qintr_rain = r_rain - pg_rain
         qintr_snow = r_snow - pg_snow

#if (defined CoLMDEBUG)
         ! Mass balance check: w_l (grid-scale old storage + precip) should equal
         ! new grid-scale storage + grid-scale ground flux
         w_l = w_l - ldew - (pg_rain + pg_snow) * deltim
         IF (abs(w_l) > 1.e-6) THEN
            write(6,*) 'JULES interception mass balance error: ', w_l
            write(6,*) 'ldew=', ldew, ' pg*dt=', (pg_rain+pg_snow)*deltim
            CALL CoLM_stop()
         ENDIF

         CALL check_interception_balance('JULES', &
              ldew, ldew_rain, ldew_snow, pg_rain, pg_snow, &
              qintr, qintr_rain, qintr_snow)
#endif
      ELSE
         ! No vegetation: all precipitation passes through, release any stored water
         ! Clamp raw precipitation to prevent negative pg (matching vegetated branch)
         pg_rain = MAX(0.0_r8, prc_rain + prl_rain + qflx_irrig_sprinkler) + ldew_rain/deltim
         pg_snow = MAX(0.0_r8, prc_snow + prl_snow) + ldew_snow/deltim

         ldew       = 0.
         ldew_rain  = 0.
         ldew_snow  = 0.
         qintr      = 0.
         qintr_rain = 0.
         qintr_snow = 0.
      ENDIF
   END SUBROUTINE LEAF_interception_JULES

   SUBROUTINE LEAF_interception_wrap(deltim,dewmx,forc_us,forc_vs,chil,sigf,lai,sai,tair,tleaf, &
                               prc_rain,prc_snow,prl_rain,prl_snow,qflx_irrig_sprinkler,bifall, &
                                                       ldew,ldew_rain,ldew_snow,z0m,hu,pg_rain, &
                                                            pg_snow,qintr,qintr_rain,qintr_snow )
!DESCRIPTION
!===========
   !wrapper for calculation of canopy interception using USGS or IGBP land cover classification

!ANCILLARY FUNCTIONS AND SUBROUTINES
!-------------------

!Original Author:
!-------------------
   !---Shupeng Zhang

!References:


!REVISION HISTORY
!----------------

   IMPLICIT NONE

   real(r8), intent(in)    :: deltim     !seconds in a time step [second]
   real(r8), intent(in)    :: dewmx      !maximum dew [mm]
   real(r8), intent(in)    :: forc_us    !wind speed
   real(r8), intent(in)    :: forc_vs    !wind speed
   real(r8), intent(in)    :: chil       !leaf angle distribution factor
   real(r8), intent(in)    :: prc_rain   !convective rainfall [mm/s]
   real(r8), intent(in)    :: prc_snow   !convective snowfall [mm/s]
   real(r8), intent(in)    :: prl_rain   !large-scale rainfall [mm/s]
   real(r8), intent(in)    :: prl_snow   !large-scale snowfall [mm/s]
   real(r8), intent(in)    :: qflx_irrig_sprinkler !irrigation and sprinkler water [mm/s]
   real(r8), intent(in)    :: bifall     !bulk density of newly fallen dry snow [kg/m3]
   real(r8), intent(in)    :: sigf       !fraction of veg cover, excluding snow-covered veg [-]
   real(r8), intent(in)    :: lai        !leaf area index [-]
   real(r8), intent(in)    :: sai        !stem area index [-]
   real(r8), intent(in)    :: tair       !air temperature [K]
   real(r8), intent(inout) :: tleaf      !sunlit canopy leaf temperature [K]

   real(r8), intent(inout) :: ldew       !depth of water on foliage [mm]
   real(r8), intent(inout) :: ldew_rain  !depth of liquid on foliage [mm]
   real(r8), intent(inout) :: ldew_snow  !depth of liquid on foliage [mm]
   real(r8), intent(in)    :: z0m        !roughness length
   real(r8), intent(in)    :: hu         !forcing height of U


   real(r8), intent(out)   :: pg_rain    !rainfall onto ground including canopy runoff [kg/(m2 s)]
   real(r8), intent(out)   :: pg_snow    !snowfall onto ground including canopy runoff [kg/(m2 s)]
   real(r8), intent(out)   :: qintr      !interception [kg/(m2 s)]
   real(r8), intent(out)   :: qintr_rain !rainfall interception (mm h2o/s)
   real(r8), intent(out)   :: qintr_snow !snowfall interception (mm h2o/s)

      IF (DEF_Interception_scheme==1) THEN
         CALL LEAF_interception_CoLM2014 (deltim,dewmx,forc_us,forc_vs,chil,sigf,lai,sai,tair,tleaf,&
                                             prc_rain,prc_snow,prl_rain,prl_snow,qflx_irrig_sprinkler,bifall,&
                                             ldew,ldew_rain,ldew_snow,z0m,hu,pg_rain,&
                                             pg_snow,qintr,qintr_rain,qintr_snow)
      ELSEIF (DEF_Interception_scheme==2) THEN
         CALL LEAF_interception_CLM4 (deltim,dewmx,forc_us,forc_vs,chil,sigf,lai,sai,tair,tleaf,&
                                             prc_rain,prc_snow,prl_rain,prl_snow,qflx_irrig_sprinkler,&
                                             ldew,ldew_rain,ldew_snow,z0m,hu,pg_rain,&
                                             pg_snow,qintr,qintr_rain,qintr_snow)
      ELSEIF (DEF_Interception_scheme==3) THEN
         CALL LEAF_interception_CLM5(deltim,dewmx,forc_us,forc_vs,chil,sigf,lai,sai,tair,tleaf,&
                                             prc_rain,prc_snow,prl_rain,prl_snow,qflx_irrig_sprinkler,&
                                             ldew,ldew_rain,ldew_snow,z0m,hu,pg_rain,&
                                             pg_snow,qintr,qintr_rain,qintr_snow)
      ELSEIF (DEF_Interception_scheme==4) THEN
         CALL LEAF_interception_NoahMP (deltim,dewmx,forc_us,forc_vs,chil,sigf,lai,sai,tair,tleaf,&
                                             prc_rain,prc_snow,prl_rain,prl_snow,qflx_irrig_sprinkler,&
                                             ldew,ldew_rain,ldew_snow,z0m,hu,pg_rain,&
                                             pg_snow,qintr,qintr_rain,qintr_snow)
      ELSEIF  (DEF_Interception_scheme==5) THEN
         CALL LEAF_interception_matsiro (deltim,dewmx,forc_us,forc_vs,chil,sigf,lai,sai,tair,tleaf,&
                                             prc_rain,prc_snow,prl_rain,prl_snow,qflx_irrig_sprinkler,&
                                             ldew,ldew_rain,ldew_snow,z0m,hu,pg_rain,&
                                             pg_snow,qintr,qintr_rain,qintr_snow)

      ELSEIF  (DEF_Interception_scheme==6) THEN
         CALL LEAF_interception_vic (deltim,dewmx,forc_us,forc_vs,chil,sigf,lai,sai,tair,tleaf,&
                                             prc_rain,prc_snow,prl_rain,prl_snow,qflx_irrig_sprinkler,&
                                             ldew,ldew_rain,ldew_snow,z0m,hu,pg_rain,&
                                             pg_snow,qintr,qintr_rain,qintr_snow)

      ELSEIF  (DEF_Interception_scheme==7) THEN
         CALL LEAF_interception_JULES (deltim,dewmx,forc_us,forc_vs,chil,sigf,lai,sai,tair,tleaf,&
                                             prc_rain,prc_snow,prl_rain,prl_snow,qflx_irrig_sprinkler,&
                                             ldew,ldew_rain,ldew_snow,z0m,hu,pg_rain,&
                                             pg_snow,qintr,qintr_rain,qintr_snow)

      ELSEIF  (DEF_Interception_scheme==8) THEN
         CALL LEAF_interception_colm202x (deltim,dewmx,forc_us,forc_vs,chil,sigf,lai,sai,tair,tleaf,&
                                             prc_rain,prc_snow,prl_rain,prl_snow,qflx_irrig_sprinkler,&
                                             ldew,ldew_rain,ldew_snow,z0m,hu,pg_rain,&
                                             pg_snow,qintr,qintr_rain,qintr_snow)
      ENDIF

   END SUBROUTINE LEAF_interception_wrap

#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
   SUBROUTINE LEAF_interception_pftwrap (ipatch,deltim,dewmx,forc_us,forc_vs,forc_t,&
                               prc_rain,prc_snow,prl_rain,prl_snow,qflx_irrig_sprinkler,bifall,&
                               ldew,ldew_rain,ldew_snow,z0m,hu,pg_rain,pg_snow,qintr,qintr_rain,qintr_snow)

! -----------------------------------------------------------------
! !DESCRIPTION:
! wrapper for calculation of canopy interception for PFTs within a land cover type.
!
! Created by Hua Yuan, 06/2019
!
! !REVISION HISTORY:
! 2023.02.21 Zhongwang Wei @ SYSU: add different options of canopy interception for PFTs
!
! -----------------------------------------------------------------

   USE MOD_Precision
   USE MOD_LandPFT
   USE MOD_Const_Physical, only: tfrz
   USE MOD_Vars_PFTimeInvariants
   USE MOD_Vars_PFTimeVariables
   USE MOD_Vars_1DPFTFluxes
   USE MOD_Const_PFT
   IMPLICIT NONE

   integer,  intent(in)    :: ipatch     !patch index
   real(r8), intent(in)    :: deltim     !seconds in a time step [second]
   real(r8), intent(in)    :: dewmx      !maximum dew [mm]
   real(r8), intent(in)    :: forc_us    !wind speed
   real(r8), intent(in)    :: forc_vs    !wind speed
   real(r8), intent(in)    :: forc_t     !air temperature
   real(r8), intent(in)    :: z0m        !roughness length
   real(r8), intent(in)    :: hu         !forcing height of U
   real(r8), intent(inout) :: ldew_rain  !depth of water on foliage [mm]
   real(r8), intent(inout) :: ldew_snow  !depth of water on foliage [mm]
   real(r8), intent(in)    :: prc_rain   !convective ranfall [mm/s]
   real(r8), intent(in)    :: prc_snow   !convective snowfall [mm/s]
   real(r8), intent(in)    :: prl_rain   !large-scale rainfall [mm/s]
   real(r8), intent(in)    :: prl_snow   !large-scale snowfall [mm/s]
   real(r8), intent(in)    :: qflx_irrig_sprinkler !irrigation and sprinkler water [mm/s]
   real(r8), intent(in)    :: bifall     ! bulk density of newly fallen dry snow [kg/m3]

   real(r8), intent(inout) :: ldew       !depth of water on foliage [mm]
   real(r8), intent(out)   :: pg_rain    !rainfall onto ground including canopy runoff [kg/(m2 s)]
   real(r8), intent(out)   :: pg_snow    !snowfall onto ground including canopy runoff [kg/(m2 s)]
   real(r8), intent(out)   :: qintr      !interception [kg/(m2 s)]
   real(r8), intent(out)   :: qintr_rain !rainfall interception (mm h2o/s)
   real(r8), intent(out)   :: qintr_snow !snowfall interception (mm h2o/s)

   integer i, p, ps, pe
#ifdef CROP
   integer  :: irrig_flag  ! 1 if sprinker, 2 if others
#endif
   real(r8) pg_rain_tmp, pg_snow_tmp

      pg_rain_tmp = 0.
      pg_snow_tmp = 0.

      ps = patch_pft_s(ipatch)
      pe = patch_pft_e(ipatch)

      IF (DEF_Interception_scheme==1) THEN
         DO i = ps, pe
            p = pftclass(i)
            CALL LEAF_interception_CoLM2014 (deltim,dewmx,forc_us,forc_vs,chil_p(p),sigf_p(i),lai_p(i),sai_p(i),forc_t,tleaf_p(i),&
                                                prc_rain,prc_snow,prl_rain,prl_snow,qflx_irrig_sprinkler,bifall,&
                                                ldew_p(i),ldew_rain_p(i),ldew_snow_p(i),z0m_p(i),hu,pg_rain,pg_snow,qintr_p(i),qintr_rain_p(i),qintr_snow_p(i))
            pg_rain_tmp = pg_rain_tmp + pg_rain*pftfrac(i)
            pg_snow_tmp = pg_snow_tmp + pg_snow*pftfrac(i)
         ENDDO
      ELSEIF (DEF_Interception_scheme==2) THEN
         DO i = ps, pe
            p = pftclass(i)
            CALL LEAF_interception_clm4 (deltim,dewmx,forc_us,forc_vs,chil_p(p),sigf_p(i),lai_p(i),sai_p(i),forc_t,tleaf_p(i),&
                                             prc_rain,prc_snow,prl_rain,prl_snow,qflx_irrig_sprinkler,&
                                             ldew_p(i),ldew_rain_p(i),ldew_snow_p(i),z0m_p(i),hu,pg_rain,pg_snow,qintr_p(i),qintr_rain_p(i),qintr_snow_p(i))
            pg_rain_tmp = pg_rain_tmp + pg_rain*pftfrac(i)
            pg_snow_tmp = pg_snow_tmp + pg_snow*pftfrac(i)
         ENDDO
      ELSEIF (DEF_Interception_scheme==3) THEN
         DO i = ps, pe
            p = pftclass(i)
            CALL LEAF_interception_clm5 (deltim,dewmx,forc_us,forc_vs,chil_p(p),sigf_p(i),lai_p(i),sai_p(i),forc_t,tleaf_p(i),&
                                             prc_rain,prc_snow,prl_rain,prl_snow,qflx_irrig_sprinkler,&
                                             ldew_p(i),ldew_rain_p(i),ldew_snow_p(i),z0m_p(i),hu,pg_rain,pg_snow,qintr_p(i),qintr_rain_p(i),qintr_snow_p(i))
            pg_rain_tmp = pg_rain_tmp + pg_rain*pftfrac(i)
            pg_snow_tmp = pg_snow_tmp + pg_snow*pftfrac(i)
         ENDDO
      ELSEIF (DEF_Interception_scheme==4) THEN
         DO i = ps, pe
            p = pftclass(i)
            CALL LEAF_interception_NoahMP (deltim,dewmx,forc_us,forc_vs,chil_p(p),sigf_p(i),lai_p(i),sai_p(i),forc_t,tleaf_p(i),&
                                             prc_rain,prc_snow,prl_rain,prl_snow,qflx_irrig_sprinkler,&
                                             ldew_p(i),ldew_rain_p(i),ldew_snow_p(i),z0m_p(i),hu,pg_rain,pg_snow,qintr_p(i),qintr_rain_p(i),qintr_snow_p(i))
            pg_rain_tmp = pg_rain_tmp + pg_rain*pftfrac(i)
            pg_snow_tmp = pg_snow_tmp + pg_snow*pftfrac(i)
         ENDDO
      ELSEIF (DEF_Interception_scheme==5) THEN
         DO i = ps, pe
            p = pftclass(i)
            CALL LEAF_interception_MATSIRO (deltim,dewmx,forc_us,forc_vs,chil_p(p),sigf_p(i),lai_p(i),sai_p(i),forc_t,tleaf_p(i),&
                                             prc_rain,prc_snow,prl_rain,prl_snow,qflx_irrig_sprinkler,&
                                             ldew_p(i),ldew_rain_p(i),ldew_snow_p(i),z0m_p(i),hu,pg_rain,pg_snow,qintr_p(i),qintr_rain_p(i),qintr_snow_p(i))
            pg_rain_tmp = pg_rain_tmp + pg_rain*pftfrac(i)
            pg_snow_tmp = pg_snow_tmp + pg_snow*pftfrac(i)
         ENDDO
      ELSEIF (DEF_Interception_scheme==6) THEN
         DO i = ps, pe
            p = pftclass(i)
            CALL LEAF_interception_VIC (deltim,dewmx,forc_us,forc_vs,chil_p(p),sigf_p(i),lai_p(i),sai_p(i),forc_t,tleaf_p(i),&
                                             prc_rain,prc_snow,prl_rain,prl_snow,qflx_irrig_sprinkler,&
                                             ldew_p(i),ldew_rain_p(i),ldew_snow_p(i),z0m_p(i),hu,pg_rain,pg_snow,qintr_p(i),qintr_rain_p(i),qintr_snow_p(i))
            pg_rain_tmp = pg_rain_tmp + pg_rain*pftfrac(i)
            pg_snow_tmp = pg_snow_tmp + pg_snow*pftfrac(i)
         ENDDO
      ELSEIF (DEF_Interception_scheme==7) THEN
         DO i = ps, pe
            p = pftclass(i)
            CALL LEAF_interception_JULES (deltim,dewmx,forc_us,forc_vs,chil_p(p),sigf_p(i),lai_p(i),sai_p(i),forc_t,tleaf_p(i),&
                                             prc_rain,prc_snow,prl_rain,prl_snow,qflx_irrig_sprinkler,&
                                             ldew_p(i),ldew_rain_p(i),ldew_snow_p(i),z0m_p(i),hu,pg_rain,pg_snow,qintr_p(i),qintr_rain_p(i),qintr_snow_p(i))
            pg_rain_tmp = pg_rain_tmp + pg_rain*pftfrac(i)
            pg_snow_tmp = pg_snow_tmp + pg_snow*pftfrac(i)
         ENDDO
      ELSEIF (DEF_Interception_scheme==8) THEN
         DO i = ps, pe
            p = pftclass(i)
            CALL LEAF_interception_CoLM202x (deltim,dewmx,forc_us,forc_vs,chil_p(p),sigf_p(i),lai_p(i),sai_p(i),forc_t,tleaf_p(i),&
                                             prc_rain,prc_snow,prl_rain,prl_snow,qflx_irrig_sprinkler,&
                                             ldew_p(i),ldew_rain_p(i),ldew_snow_p(i),z0m_p(i),hu,pg_rain,pg_snow,qintr_p(i),qintr_rain_p(i),qintr_snow_p(i))
            pg_rain_tmp = pg_rain_tmp + pg_rain*pftfrac(i)
            pg_snow_tmp = pg_snow_tmp + pg_snow*pftfrac(i)
         ENDDO
      ENDIF

      pg_rain = pg_rain_tmp
      pg_snow = pg_snow_tmp
      ldew    = sum( ldew_p(ps:pe) * pftfrac(ps:pe))
      ldew_rain = sum( ldew_rain_p(ps:pe) * pftfrac(ps:pe))
      ldew_snow = sum( ldew_snow_p(ps:pe) * pftfrac(ps:pe))
      qintr   = sum(qintr_p(ps:pe) * pftfrac(ps:pe))
      qintr_rain = sum(qintr_rain_p(ps:pe) * pftfrac(ps:pe))
      qintr_snow = sum(qintr_snow_p(ps:pe) * pftfrac(ps:pe))

   END SUBROUTINE LEAF_interception_pftwrap
#endif

   SUBROUTINE check_interception_balance(scheme_name, &
         ldew, ldew_rain, ldew_snow, pg_rain, pg_snow, &
         qintr, qintr_rain, qintr_snow)

   ! Validates interception water balance consistency.
   ! Called from CoLMDEBUG blocks after each scheme completes.

      character(len=*), intent(in) :: scheme_name
      real(r8), intent(in) :: ldew, ldew_rain, ldew_snow
      real(r8), intent(in) :: pg_rain, pg_snow
      real(r8), intent(in) :: qintr, qintr_rain, qintr_snow

      ! Check A: component consistency (ldew == ldew_rain + ldew_snow)
      IF (abs(ldew - (ldew_rain + ldew_snow)) > INTERCEPTION_BALANCE_TOL) THEN
         write(6,*) 'Component consistency error in ', scheme_name, ':'
         write(6,*) 'ldew=', ldew, ' ldew_rain+ldew_snow=', ldew_rain+ldew_snow
         write(6,*) 'diff=', ldew - (ldew_rain + ldew_snow)
         CALL CoLM_stop()
      ENDIF

      ! Check B: non-negativity
      IF (ldew < -INTERCEPTION_BALANCE_TOL .or. &
          ldew_rain < -INTERCEPTION_BALANCE_TOL .or. &
          ldew_snow < -INTERCEPTION_BALANCE_TOL .or. &
          pg_rain < -INTERCEPTION_BALANCE_TOL .or. &
          pg_snow < -INTERCEPTION_BALANCE_TOL) THEN
         write(6,*) 'Negative value error in ', scheme_name, ':'
         write(6,*) 'ldew=', ldew, ' ldew_rain=', ldew_rain, ' ldew_snow=', ldew_snow
         write(6,*) 'pg_rain=', pg_rain, ' pg_snow=', pg_snow
         CALL CoLM_stop()
      ENDIF

      ! Check C: flux consistency (qintr == qintr_rain + qintr_snow)
      IF (abs(qintr - (qintr_rain + qintr_snow)) > INTERCEPTION_BALANCE_TOL) THEN
         write(6,*) 'Flux consistency error in ', scheme_name, ':'
         write(6,*) 'qintr=', qintr, ' qintr_rain+qintr_snow=', qintr_rain+qintr_snow
         write(6,*) 'diff=', qintr - (qintr_rain + qintr_snow)
         CALL CoLM_stop()
      ENDIF

   END SUBROUTINE check_interception_balance

END MODULE MOD_LeafInterception
