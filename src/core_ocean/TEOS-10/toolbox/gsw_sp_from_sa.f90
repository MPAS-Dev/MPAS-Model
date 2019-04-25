!==========================================================================
elemental function gsw_sp_from_sa (sa, p, long, lat) 
!==========================================================================
!
! Calculates Practical salinity, sp, from Absolute salinity, sa  
!
! sa     : Absolute Salinity                               [g/kg]
! p      : sea pressure                                    [dbar]
! long   : longitude                                       [DEG E]     
! lat    : latitude                                        [DEG N]
!
! gsw_sp_from_sa      : Practical Salinity                 [unitless]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_saar, gsw_sp_from_sa_baltic

use gsw_mod_error_functions, only : gsw_error_code, gsw_error_limit

use gsw_mod_teos10_constants, only : gsw_ups

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, p, long, lat 

real (r8) :: gsw_sp_from_sa

real (r8) :: saar, sp_baltic

character (*), parameter :: func_name = "gsw_sp_from_sa"

sp_baltic = gsw_sp_from_sa_baltic(sa,long,lat)

if (sp_baltic .lt. 1e10_r8) then

   gsw_sp_from_sa = sp_baltic

else

   saar = gsw_saar(p,long,lat)
   if (saar .gt. gsw_error_limit) then
      gsw_sp_from_sa = gsw_error_code(1,func_name,saar)
   else
      gsw_sp_from_sa = (sa/gsw_ups)/(1.0_r8 + saar)
   end if

end if

return
end function

!--------------------------------------------------------------------------
