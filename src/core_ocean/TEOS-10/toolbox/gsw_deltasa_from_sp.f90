!==========================================================================
elemental function gsw_deltasa_from_sp (sp, p, long, lat) 
!==========================================================================
!
! Calculates Absolute Salinity Anomaly, deltaSA, from Practical Salinity, SP. 
!
! sp     : Practical Salinity                              [unitless]
! p      : sea pressure                                    [dbar]
! long   : longitude                                       [deg E]     
! lat    : latitude                                        [deg N]
!
! gsw_deltasa_from_sp : Absolute Salinty Anomaly           [g/kg]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_sa_from_sp, gsw_sr_from_sp

use gsw_mod_error_functions, only : gsw_error_code, gsw_error_limit

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sp, p, long, lat 

real (r8) :: gsw_deltasa_from_sp

character (*), parameter :: func_name = "gsw_deltasa_from_sp"

gsw_deltasa_from_sp = gsw_sa_from_sp(sp,p,long,lat) - gsw_sr_from_sp(sp)

if (gsw_deltasa_from_sp.gt.gsw_error_limit) &
        gsw_deltasa_from_sp = gsw_error_code(1,func_name)

return
end function

!--------------------------------------------------------------------------
