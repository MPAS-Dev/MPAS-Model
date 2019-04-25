!==========================================================================
pure subroutine gsw_linear_interp_sa_ct (sa, ct, p, p_i, sa_i, ct_i)
!==========================================================================
! This function interpolates the cast with respect to the interpolating 
! variable p. This function finds the values of SA, CT at p_i on this cast.
!
! Assumes that p is monotonically increasing. If p_i is also monotonically
! increasing then this function is even more efficient through the use of
! kstart option to function gsw_util_indx (G.B.Hyland 10/8/2017).
!==========================================================================

use gsw_mod_toolbox, only : gsw_util_indx

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa(:), ct(:), p(:), p_i(:)
real (r8), intent(out) :: sa_i(:), ct_i(:)

integer :: i, k, np
real (r8) :: r

np = size(p)

k = 1
do i = 1, size(p_i)

    if (p_i(i) .le. p(1)) then

        sa_i(i) = sa(1)
        ct_i(i) = ct(1)

    else if (p_i(i) .ge. p(np)) then

        sa_i(i) = sa(np)
        ct_i(i) = ct(np)

    else

        if (i .gt. 1) then
            if (p_i(i) .lt. p_i(i-1)) k = 1
        end if
        k = gsw_util_indx(p,p_i(i),kstart=k)

        r = (p_i(i)-p(k))/(p(k+1)-p(k))
        sa_i(i) = sa(k) + r*(sa(k+1)-sa(k))
        ct_i(i) = ct(k) + r*(ct(k+1)-ct(k))

    end if
end do

return

end subroutine

!--------------------------------------------------------------------------
