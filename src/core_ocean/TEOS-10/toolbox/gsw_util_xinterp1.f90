!==========================================================================
pure function gsw_util_xinterp1 (x, y, x0)
!==========================================================================
!
! Linearly interpolate a real monotonic array.
!
! x      : x array (must be monotonically increasing)
! y      : y array
! x0     : x value to be interpolated
!
! xinterp1 : linearly interpolated y value
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_util_indx

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: x0
real (r8), intent(in) :: x(:), y(:)

real (r8) :: gsw_util_xinterp1

integer :: k
real (r8) :: r

k = gsw_util_indx(x,x0)
r = (x0-x(k))/(x(k+1)-x(k))
gsw_util_xinterp1 = y(k) + r*(y(k+1)-y(k))

return
end function

!--------------------------------------------------------------------------
