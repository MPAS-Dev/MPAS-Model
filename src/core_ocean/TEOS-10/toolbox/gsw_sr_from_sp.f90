!==========================================================================
elemental function gsw_sr_from_sp (sp) 
!==========================================================================
!
! Calculates Reference Salinity, SR, from Practical Salinity, SP. 
!
! sp     : Practical Salinity                              [unitless]
!
! gsw_sr_from_sp : Reference Salinity                      [g/kg]
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : gsw_ups

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sp 

real (r8) :: gsw_sr_from_sp

gsw_sr_from_sp = sp*gsw_ups

return
end function

!--------------------------------------------------------------------------



