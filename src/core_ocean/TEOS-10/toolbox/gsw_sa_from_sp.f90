!==========================================================================
elemental function gsw_sa_from_sp (sp, p, long, lat)       
!==========================================================================
!
! Calculates Absolute Salinity, SA, from Practical Salinity, SP
!
! sp     : Practical Salinity                              [unitless]
! p      : sea pressure                                    [dbar]
! long   : longitude                                       [DEG E]     
! lat    : latitude                                        [DEG N]
!
! gsw_sa_from_sp   : Absolute Salinity                     [g/kg]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_saar, gsw_sa_from_sp_baltic

use gsw_mod_error_functions, only : gsw_error_code, gsw_error_limit

use gsw_mod_teos10_constants, only : gsw_ups

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sp, p, long, lat       

real (r8) :: gsw_sa_from_sp

real (r8) :: saar, sa_baltic

character (*), parameter :: func_name = "gsw_sa_from_sp"

sa_baltic = gsw_sa_from_sp_baltic(sp,long,lat)

if (sa_baltic .lt. 1e10_r8) then

   gsw_sa_from_sp = sa_baltic

else

   saar = gsw_saar(p,long,lat)
   if (saar .gt. gsw_error_limit) then
      gsw_sa_from_sp = gsw_error_code(1,func_name,saar)
   else
      gsw_sa_from_sp = gsw_ups*sp*(1.0_r8 + saar)
   end if

end if

return
end function

!--------------------------------------------------------------------------
