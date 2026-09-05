#include <define.h>

MODULE MOD_RainSnowTemp

!-----------------------------------------------------------------------
   USE MOD_Precision
   USE MOD_Namelist
   IMPLICIT NONE
   SAVE

! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: rain_snow_temp


!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------


   SUBROUTINE rain_snow_temp (patchtype,&
              forc_t,forc_q,forc_psrf,forc_prc,forc_prl,forc_us,forc_vs,tcrit,&
              prc_rain,prc_snow,prl_rain,prl_snow,t_precip,bifall)

!=======================================================================
!  define the rate of rainfall and snowfall and precipitation water temp
!  Original author: Yongjiu Dai, 09/1999; 08/31/2002, 04/2014, 01/2023
!=======================================================================

   USE MOD_Precision
   USE MOD_Const_Physical, only: tfrz
   USE MOD_WetBulb

   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   integer,  intent(in)  :: patchtype ! land patch type (3=glaciers)

   real(r8), intent(in)  :: forc_t    ! temperature at agcm reference height [kelvin]
   real(r8), intent(in)  :: forc_q    ! specific humidity at agcm reference height [kg/kg]
   real(r8), intent(in)  :: forc_psrf ! atmosphere pressure at the surface [pa]
   real(r8), intent(in)  :: forc_prc  ! convective precipitation [mm/s]
   real(r8), intent(in)  :: forc_prl  ! large scale precipitation [mm/s]
   real(r8), intent(in)  :: forc_us   ! wind speed in eastward direction [m/s]
   real(r8), intent(in)  :: forc_vs   ! wind speed in northward direction [m/s]

   real(r8), intent(in)  :: tcrit     ! critical temp. to determine rain or snow

   real(r8), intent(out) :: prc_rain  ! convective rainfall [kg/(m2 s)]
   real(r8), intent(out) :: prc_snow  ! convective snowfall [kg/(m2 s)]
   real(r8), intent(out) :: prl_rain  ! large scale rainfall [kg/(m2 s)]
   real(r8), intent(out) :: prl_snow  ! large scale snowfall [kg/(m2 s)]
   real(r8), intent(out) :: t_precip  ! snowfall/rainfall temperature [kelvin]
   real(r8), intent(out) :: bifall    ! bulk density of newly fallen dry snow [kg/m3]

!-------------------------- Local Variables ----------------------------
   real(r8) :: flfall                 ! fraction of liquid water within falling precip.

   real(r8) :: all_snow_t             ! temperature at which all precip falls entirely as snow (K)
   real(r8) :: frac_rain_slope        ! slope of the frac_rain vs. temperature relationship
   real(r8) :: all_snow_t_c           ! Temperature at which precip falls entirely as rain (deg C)
   real(r8) :: all_rain_t_c           ! Temperature at which precip falls entirely as snow (deg C)

   logical  :: glaciers               ! true: glacier column
   real(r8) :: t_for_bifall_degC      ! temperature to USE in bifall equation (deg C)
   real(r8) :: forc_wind              ! wind speed [m/s]
   real(r8) :: t_hydro                ! temperature of falling hydrometeor [deg C]
!-----------------------------------------------------------------------

! wet-bulb temperature
      CALL wetbulb(forc_t,forc_psrf,forc_q,t_precip)

      IF (trim(DEF_precip_phase_discrimination_scheme) == 'I') THEN
      ! Wang, Y.H., Broxton, P., Fang, Y., Behrangi, A., Barlage, M., Zeng, X., & Niu, G.Y. (2019).
      ! A Wet-Bulb Temperature Based Rain-Snow Partitioning Scheme Improves Snowpack Prediction
      ! Over the Drier Western United States. Geophysical Research Letters, 46, 13,825-13,835.
      !
      ! Behrangi et al. (2018) On distinguishing snowfall from rainfall
      ! using near-surface atmospheric information: Comparative analysis,
      ! uncertainties and hydrologic importance. Q J R Meteorol Soc. 144 (Suppl. 1):89-102

         IF(t_precip - tfrz > 3.0)THEN
            flfall = 1.0      ! fraction of liquid water within falling precip
         ELSEIF (t_precip - tfrz >= -2.0)THEN
            !Figure 5c of Behrangi et al. (2018)
            flfall = max(0.0, 1.0 - 1.0/(1.0+5.00e-5*exp(2.0*(t_precip-tfrz+4.))))
            !Equation 1 of Wang et al. (2019)
            !* flfall = max(0.0, 1.0 - 1.0/(1.0+6.99e-5*exp(2.0*(t_precip-tfrz+3.97))))
         ELSE
            flfall = 0.0
         ENDIF

      ELSEIF (trim(DEF_precip_phase_discrimination_scheme) == 'II') THEN
         glaciers = .false.
         IF (patchtype == 3) glaciers = .true.

         IF(glaciers) THEN
            all_snow_t_c = -2.0
            all_rain_t_c =  0.0
         ELSE
            all_snow_t_c = 0.0
            all_rain_t_c = 2.0
         ENDIF

         all_snow_t = all_snow_t_c + tfrz
         frac_rain_slope = 1._r8 / (all_rain_t_c - all_snow_t_c)

         ! Re-partition precipitation into rain/snow for a single column.
         ! Rain and snow variables should be set initially, and are updated here

         flfall = min(1.0_r8, max(0.0_r8,(forc_t - all_snow_t)*frac_rain_slope))
      ELSEIF (trim(DEF_precip_phase_discrimination_scheme) == 'III') THEN
      ! Phillip Harder and John Pomeroy (2013)
      ! Estimating precipitation phase using a psychrometric energy
      ! balance method . Hydrol Process, 27, 1901-1914
      ! Hydromet_Temp [K]
         CALL hydromet_temp(forc_psrf,(forc_t-273.15),forc_q,t_hydro)

         IF(t_hydro > 3.0)THEN
            flfall = 1.0      ! fraction of liquid water within falling precip
         ELSEIF ((t_hydro >= -3.0).and.(t_hydro <= 3.0))THEN
            flfall = max(0.0, 1.0/(1.0+2.50286*0.125006**t_hydro))
         ELSE
            flfall = 0.0
         ENDIF

      ELSE
         ! the upper limit of air temperature is set for snowfall, this cut-off
         ! was selected based on Fig. 1, Plate 3-1, of Snow Hydrology (1956).
         ! the percentage of liquid water by mass, which is arbitrarily set to
         ! vary linearly with air temp, from 0% at 273.16 to 40% max at 275.16.

         IF(forc_t>tfrz+2.0)THEN
            flfall = 1.0     ! fraction of liquid water within falling precip.
         ELSE
            flfall = max(0.0, -54.632+0.2*forc_t)
         ENDIF

      ENDIF

      ! new scheme for "bifall" from CLM5.0
      CALL NewSnowBulkDensity(forc_t,forc_us,forc_vs,bifall)

      prc_rain = forc_prc*flfall        ! convective rainfall (mm/s)
      prl_rain = forc_prl*flfall        ! large scale rainfall (mm/s)
      prc_snow = forc_prc*(1.-flfall)   ! convective snowfall (mm/s)
      prl_snow = forc_prl*(1.-flfall)   ! large scale snowfall (mm/s)

      ! -------------------------------------------------------------
      ! temperature of rainfall or snowfall
      ! -------------------------------------------------------------

      IF (forc_t > 275.65) THEN
         IF (t_precip < tfrz) t_precip = tfrz
      ELSE
         t_precip = min(tfrz,t_precip)
         IF(flfall > 1.e-6)THEN
            t_precip = tfrz - sqrt((1.0/flfall)-1.0)/100.0
         ENDIF
      ENDIF

   END SUBROUTINE rain_snow_temp


   SUBROUTINE NewSnowBulkDensity(forc_t,forc_us,forc_vs,bifall)
!=======================================================================
!  Scheme for bulk density of newly fallen dry snow
!=======================================================================

   USE MOD_Precision
   USE MOD_Const_Physical, only: tfrz

   real(r8), intent(in) :: forc_t     ! temperature at agcm reference height [kelvin]
   real(r8), intent(in) :: forc_us    ! wind speed in eastward direction [m/s]
   real(r8), intent(in) :: forc_vs    ! wind speed in northward direction [m/s]

   real(r8), intent(out) :: bifall    ! bulk density of newly fallen dry snow [kg/m3]

   real(r8) :: t_for_bifall_degC      ! temperature to USE in bifall equation (deg C)
   real(r8) :: forc_wind              ! wind speed [m/s]

   !-----------------------------------------------------------------------

      IF (forc_t > tfrz + 2.0) THEN
         bifall = 50.0 + 1.7*(17.0)**1.5
      ELSEIF (forc_t > tfrz - 15.0) THEN
         bifall = 50.0 + 1.7*(forc_t - tfrz + 15.0)**1.5
      ELSE
         ! Andrew Slater: A temp of about -15C gives the nicest
         ! "blower" powder, but as you get colder the flake size decreases so
         ! density goes up. e.g. the smaller snow crystals from the Arctic and Antarctic winters
         IF (forc_t > tfrz - 57.55) THEN
            t_for_bifall_degC = (forc_t-tfrz)
         ELSE
            ! Below -57.55 deg C, the following function starts to decrease with
            ! decreasing temperatures. Limit the function to avoid this turning over.
            t_for_bifall_degC = -57.55
         ENDIF
         bifall = -(50.0/15.0 + 0.0333*15.0)*t_for_bifall_degC - 0.0333*t_for_bifall_degC**2
      ENDIF

      forc_wind = sqrt(forc_us**2 + forc_vs**2)
      IF (forc_wind > 0.1) THEN
      ! Density offset for wind-driven compaction, initial ideas based on Liston et. al (2007) J.
      ! Glaciology, 53(181), 241-255. Modified for a continuous wind impact and slightly more
      ! sensitive to wind - Andrew Slater, 2016
         bifall = bifall + (266.861 * ((1.0 + TANH(forc_wind/5.0))/2.0)**8.8)
      ENDIF

   END SUBROUTINE NewSnowBulkDensity

   !!==============================================

   SUBROUTINE hydromet_temp(ppa, pta, pqa, pti)
!-----------------------------------------------------------------------------
! !DESCRIPTION
!  Computes the temperature of a falling hydrometeor based on Harder, P., Pomeroy, J. (2013).
!
!  Original Author:
!  ----------------
!  V. Vionnet (11/2020)
!
! !REFERENCES:
!  Harder, P., Pomeroy, J. (2013).
!  Estimating precipitation phase using a psychrometric energy balance method
!  Hydrological Processes 27(13), 1901-1914. https://dx.doi.org/10.1002/hyp.9799

! !REVISIONS:
!  2023.07.30 Aobo Tan & Zhongwang Wei @ SYSU
!
!-----------------------------------------------------------------------------

      real(r8), intent(in)   :: ppa          ! Air pressure (Pa)
      real(r8), intent(in)   :: pta          ! Air temperature (deg C)
      real(r8), intent(in)   :: pqa          ! Air specific humidity (kg/kg)
      real(r8), intent(out)  :: pti          ! Hydrometeor temperature in deg C

      real(r8) :: zd          ! Diffusivity of water vapour in air [m^2 s^-1]
      real(r8) :: zlambda     ! Thermal conductivity of air [J m^-1 s^-1 K^-1]
      real(r8) :: zl          ! Latent heat of sublimation or vaporization [J kg^-1]
      real(r8) :: zrhoda      ! Density of dry air [kg m^-3]
      real(r8) :: zrh         ! Relative humidity [-]
      real(r8) :: rho_vast_diff, esat, rho_vast
      real(r8) :: zt, ztint, zf, zfdiff, evsat
      integer :: JITER
      integer :: JJ, I, NN

      ! 1. Compute diffusivity of water vapour in air [m^2 s^-1] (Thorpe and Mason, 1966)
      zd = 2.063e-5 * ((pta + 273.15) / 273.15) ** 1.75

      ! 2. Compute thermal conductivity of air [J m^-1 s^-1 K^-1]
      zlambda = 0.000063 * (pta + 273.15) + 0.00673

      ! 3. Compute latent heat of sublimation or vaporization (depending on air temperature)
      IF (pta < 0.) THEN
         zl = 1000.0 * (2834.1 - 0.29 * pta - 0.004 * pta ** 2.)

      ELSE
         zl = 1000.0 * (2501.0 - (2.361 * pta))
      ENDIF

      ! 4. Compute density of dry air [kg m^-3]
      zrhoda = ppa / (287.04 * (pta + 273.15))

      ! 5. Compute saturated water vapour pressure [Pa]
      IF (pta > 0) THEN
         evsat = 611.0 * EXP(17.27 * pta / (pta + 237.3))
      ELSE
         evsat = 611.0 * EXP(21.87 * pta / (pta + 265.5))
      ENDIF

      ! 6. Solve iteratively to get Ti in Harder and Pomeroy (2013) using a Newton-Raphson approach
      ! Set the first guess to pta
      zt = pta

      ! Loop until convergence
      DO JITER = 1, 10
         ztint = zt

         IF (zt > 0) THEN
            esat = 611.0 * EXP(17.27 * zt / (zt + 237.3))
         ELSE
            esat = 611.0 * EXP(21.87 * zt / (zt + 265.5))
         ENDIF

         rho_vast = esat / (461.5 * (zt + 273.15)) ! Saturated water vapour density

         zf = zt - pta - zd * zl / zlambda * (pqa * zrhoda - rho_vast)

         IF (zt > 0) THEN
            rho_vast_diff = 611.0 / (461.5 * (zt + 273.15)) * EXP(17.27 * zt / (zt + 237.3)) * &
                            (-1 / (zt + 273.15) + 17.27 * 237.3 / ((zt + 237.3) ** 2.))
         ELSE
            rho_vast_diff = 611.0 / (461.5 * (zt + 273.15)) * EXP(21.87 * zt / (zt + 265.5)) * &
                            (-1 / (zt + 273.15) + 21.87 * 265.5 / ((zt + 265.5) ** 2.))
         ENDIF

         zfdiff = 1 + zd * zl / zlambda * rho_vast_diff
         zt = ztint - zf / zfdiff
         IF (ABS(zt - ztint) .lt. 0.01) EXIT
      ENDDO

      pti = zt

   END SUBROUTINE hydromet_temp

END MODULE MOD_RainSnowTemp
