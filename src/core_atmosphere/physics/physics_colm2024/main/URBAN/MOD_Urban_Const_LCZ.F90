#include <define.h>
MODULE MOD_Urban_Const_LCZ

! -----------------------------------------------------------------------
! !DESCRIPTION:
!  look-up-table for LCZ morphology and thermal parameters
!                             - NOTE -
!  Each city may have different values for the parameters in this table.
!  The default values may not suit any specific city.
!  Users could adjust these values based on the city they are working with.
!
!  Created by Wenzong Dong, Jun, 2022
!
! !REFERENCES:
!  1) Stewart, I. D., Oke, T. R., & Krayenhoff, E. S. (2014). Evaluation of
!  the 'local climate zone' scheme using temperature observations and model
!  simulations. International Journal of Climatology, 34(4), 1062-1080.
!  https://doi.org/10.1002/joc.3746
!
!  2) The URBPARM_LCZ.TBL of WRF, https://github.com/wrf-model/WRF/
!
! -----------------------------------------------------------------------
! !USE
   USE MOD_Precision

   IMPLICIT NONE
   SAVE

   ! roof fraction [-]
   real(r8), parameter, dimension(10) :: wtroof_lcz &
      = (/0.5 , 0.5 , 0.55, 0.3 , 0.3, 0.3, 0.8 , 0.4 , 0.15, 0.25/)

   ! pervious fraction [-]
   real(r8), parameter, dimension(10)  :: fgper_lcz &
      = (/0.05, 0.1 , 0.15, 0.35, 0.3, 0.4, 0.15, 0.15, 0.7 , 0.45/)

   ! height of roof [m]
   real(r8), parameter, dimension(10)  :: htroof_lcz &
      = (/45., 15. , 5.  , 40., 15., 5. , 3. , 7. , 5.  , 8.5 /)

   ! H/W [-]
   real(r8), parameter, dimension(10)  :: hwrbld_lcz &
      = (/2.5, 1.25, 1.25, 1. , 0.5, 0.5, 1.5, 0.2, 0.15, 0.35/)

   ! thickness of roof [m]
   real(r8), parameter, dimension(10)  :: thkroof_lcz &
      = (/0.3 , 0.3 , 0.2 , 0.3 , 0.25, 0.15, 0.05, 0.12, 0.15, 0.05/)

   ! thickness of wall [m]
   real(r8), parameter, dimension(10)  :: thkwall_lcz &
      = (/0.3 , 0.25, 0.2 , 0.2 , 0.2 , 0.2 , 0.1 , 0.2 , 0.2 , 0.05/)

   ! thickness of impervious road [m]
   real(r8), parameter, dimension(10)  :: thkgimp_lcz &
      = (/0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25/)

   ! albedo of roof [-]
   real(r8), parameter, dimension(10)  :: albroof_lcz &
      = (/0.13, 0.18, 0.15, 0.13, 0.13, 0.13, 0.15, 0.18, 0.13, 0.1 /)

   ! albedo of wall [-]
   real(r8), parameter, dimension(10)  :: albwall_lcz &
      = (/0.25, 0.2 , 0.2 , 0.25, 0.25, 0.25, 0.2 , 0.25, 0.25, 0.2 /)

   ! albedo of impervious road [-]
   real(r8), parameter, dimension(10)  :: albgimp_lcz &
      = (/0.14, 0.14, 0.14, 0.14, 0.14, 0.14, 0.18, 0.14, 0.14, 0.14/)

   ! albedo of pervious road [-]
   real(r8), parameter, dimension(10)  :: albgper_lcz &
      = (/0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15/)

   ! emissivity of roof [-]
   real(r8), parameter, dimension(10)  :: emroof_lcz &
      = (/0.91, 0.91, 0.91, 0.91, 0.91, 0.91, 0.28, 0.91, 0.91, 0.91/)

   ! emissivity of wall [-]
   real(r8), parameter, dimension(10)  :: emwall_lcz &
      = (/0.90, 0.90, 0.90, 0.90, 0.90, 0.90, 0.90, 0.90, 0.90, 0.90/)

   ! emissivity of road [-]
   real(r8), parameter, dimension(10)  :: emgimp_lcz &
      = (/0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.92, 0.95, 0.95, 0.95/)

   ! emissivity of impervious road [-]
   real(r8), parameter, dimension(10)  :: emgper_lcz &
      = (/0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95/)


   ! volumetric heat capacity of roof [J/m3*K]
   real(r8), parameter, dimension(10)  :: cvroof_lcz &
      = (/1.8E6 , 1.8E6 , 1.44E6, 1.8E6 , 1.8E6 , 1.44E6, 2.0E6 , 1.8E6 , 1.44E6, 2.0E6 /)

   ! volumetric heat capacity of wall [J/m3*K]
   real(r8), parameter, dimension(10)  :: cvwall_lcz &
      = (/1.8E6 , 2.67E6, 2.05E6, 2.0E6 , 2.0E6 , 2.05E6, 0.72E6, 1.8E6 , 2.56E6, 1.69E6/)

   ! volumetric heat capacity of impervious road [J/m3*K]
   real(r8), parameter, dimension(10)  :: cvgimp_lcz &
      = (/1.75E6, 1.68E6, 1.63E6, 1.54E6, 1.50E6, 1.47E6, 1.67E6, 1.38E6, 1.37E6, 1.49E6/)


   ! thermal conductivity of roof [W/m*K]
   real(r8), parameter, dimension(10)  :: tkroof_lcz &
      = (/1.25, 1.25, 1.00, 1.25, 1.25, 1.00, 2.0 , 1.25, 1.00, 2.00/)

   ! thermal conductivity of wall [W/m*K]
   real(r8), parameter, dimension(10)  :: tkwall_lcz &
      = (/1.09, 1.5 , 1.25, 1.45, 1.45, 1.25, 0.5 , 1.25, 1.00, 1.33/)

   ! thermal conductivity of impervious road [W/m*K]
   real(r8), parameter, dimension(10)  :: tkgimp_lcz &
      = (/0.77, 0.73, 0.69, 0.64, 0.62, 0.60, 0.72, 0.51, 0.55, 0.61/)

   !TODO:AHE coding
   ! maximum temperature of inner room [K]
   real(r8), parameter, dimension(10)  :: tbldmax_lcz &
      = (/297.65, 297.65, 297.65, 297.65, 297.65, 297.65, 297.65, 297.65, 297.65, 297.65/)

   ! minimum temperature of inner room [K]
   real(r8), parameter, dimension(10)  :: tbldmin_lcz &
      = (/290.65, 290.65, 290.65, 290.65, 290.65, 290.65, 290.65, 290.65, 290.65, 290.65/)

END MODULE MOD_Urban_Const_LCZ
! ---------- EOP ------------
