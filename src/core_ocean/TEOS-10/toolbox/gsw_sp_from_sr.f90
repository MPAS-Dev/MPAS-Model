!==========================================================================
elemental function gsw_sp_from_sr (sr)  
!==========================================================================
!
! Calculates Practical Salinity, sp, from Reference Salinity, sr. 
!
! sr     : Reference Salinity                              [g/kg]
!
! gsw_sp_from_sr  : Practical Salinity                     [unitless]
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : gsw_ups

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sr  

real (r8) :: gsw_sp_from_sr

gsw_sp_from_sr = sr/gsw_ups

return
end function

!--------------------------------------------------------------------------



