!==========================================================================
pure function gsw_util_interp1q_int (x, iy, x_i) result(y_i)
!==========================================================================
! Returns the value of the 1-D function iy (integer) at the points of column
! vector x_i using linear interpolation. The vector x specifies the
! coordinates of the underlying interval.
!
! Assumes x is monotonically increasing. If x_i is also monotonically
! increasing then this function is even more efficient through the use of
! kstart option to function gsw_util_indx (G.B.Hyland 10/8/2017).
!==========================================================================

use gsw_mod_toolbox, only : gsw_util_indx

use gsw_mod_kinds

implicit none

integer, intent(in) :: iy(:)
real (r8), intent(in) :: x(:), x_i(:)

real (r8) :: y_i(size(x_i))

integer :: i, nx, k
real (r8) :: r

nx = size(x)

k = 1
do i = 1, size(x_i)

    if (x_i(i) .le. x(1)) then

        y_i(i) = real(iy(1),r8)

    else if (x_i(i) .ge. x(nx)) then

        y_i(i) = real(iy(nx),r8)

    else

        if (i .gt. 1) then
            if (x_i(i) .lt. x_i(i-1)) k = 1
        end if
        k = gsw_util_indx(x,x_i(i),kstart=k)

        r = (x_i(i)-x(k))/(x(k+1)-x(k))
        y_i(i) = real(iy(k),r8) + r*real(iy(k+1)-iy(k),r8)

    end if
end do

return

end function

!--------------------------------------------------------------------------
