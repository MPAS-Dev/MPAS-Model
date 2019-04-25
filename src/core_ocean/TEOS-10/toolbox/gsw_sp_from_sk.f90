!==========================================================================
elemental function gsw_sp_from_sk (sk)       
!==========================================================================
!
! Calculates Practical Salinity, SP, from SK
!
!  SK    : Knudsen Salinity                        [parts per thousand, ppt]
!
! gsw_sp_from_sk  : Practical Salinity                              [unitless]
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : gsw_soncl

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sk       

real (r8) :: gsw_sp_from_sk

gsw_sp_from_sk = (sk - 0.03_r8)*(gsw_soncl/1.805_r8) 

return
end function

!--------------------------------------------------------------------------
