!==========================================================================
elemental function gsw_sstar_from_sp (sp, p, long, lat) 
!==========================================================================
!
! Calculates Preformed Salinity, Sstar, from Practical Salinity, SP. 
!
! sp     : Practical Salinity                              [unitless]
! p      : sea pressure                                    [dbar]
! long   : longitude                                       [deg E]     
! lat    : latitude                                        [deg N]
!
! gsw_sstar_from_sp  : Preformed Salinity                  [g/kg]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_saar, gsw_sa_from_sp_baltic

use gsw_mod_error_functions, only : gsw_error_code, gsw_error_limit

use gsw_mod_teos10_constants, only : gsw_ups

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sp, p, long, lat 

real (r8) :: gsw_sstar_from_sp

real (r8) :: saar, sstar_baltic

character (*), parameter :: func_name = "gsw_sstar_from_sp"

!In the Baltic Sea, Sstar = SA.
sstar_baltic = gsw_sa_from_sp_baltic(sp,long,lat)

if (sstar_baltic .lt. 1e10_r8) then

   gsw_sstar_from_sp = sstar_baltic

else

   saar = gsw_saar(p,long,lat)
   if (saar .gt. gsw_error_limit) then
      gsw_sstar_from_sp = gsw_error_code(1,func_name,saar)
   else
      gsw_sstar_from_sp = gsw_ups*sp*(1 - 0.35_r8*saar)
   end if

end if

return
end function

!--------------------------------------------------------------------------
