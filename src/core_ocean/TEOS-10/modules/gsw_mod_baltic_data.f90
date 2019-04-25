!==========================================================================
module gsw_mod_baltic_data
!==========================================================================
!
! Coordinate data for the Baltic Sea

use gsw_mod_kinds

implicit none

real (r8), dimension(2) :: xb_right, yb_right
real (r8), dimension(3) :: xb_left, yb_left

data xb_left  /12.6_r8,  7.0_r8, 26.0_r8/
data yb_left  /50.0_r8, 59.0_r8, 69.0_r8/

data xb_right /45.0_r8, 26.0_r8/
data yb_right /50.0_r8, 69.0_r8/

end module

!--------------------------------------------------------------------------



