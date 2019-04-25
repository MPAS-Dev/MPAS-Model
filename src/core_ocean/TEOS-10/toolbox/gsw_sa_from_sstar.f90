!==========================================================================
elemental function gsw_sa_from_sstar (sstar, p, long, lat)  
!==========================================================================
!
! Calculates Absolute Salinity, SA, from Preformed Salinity, Sstar.
!
! Sstar  : Preformed Salinity                              [g/kg]
! p      : sea pressure                                    [dbar]
! long   : longitude                                       [deg E]     
! lat    : latitude                                        [deg N]
!
! gsw_sa_from_sstar   : Absolute Salinity                  [g/kg]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_saar

use gsw_mod_error_functions, only : gsw_error_code, gsw_error_limit

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sstar, p, long, lat  

real (r8) :: gsw_sa_from_sstar

real (r8) :: saar

character (*), parameter :: func_name = "gsw_sa_from_sstar"

saar = gsw_saar(p,long,lat)

if (saar.gt.gsw_error_limit) then

   gsw_sa_from_sstar = gsw_error_code(1,func_name,saar)

else

   ! In the Baltic Sea, Sstar = SA, and note that gsw_saar returns zero
   ! for SAAR in the Baltic.

   gsw_sa_from_sstar = sstar*(1.0_r8 + saar)/(1.0_r8 - 0.35_r8*saar)

end if

return
end function

!--------------------------------------------------------------------------
