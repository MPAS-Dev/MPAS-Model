!==========================================================================
pure function gsw_util_indx (x, z, kstart) result(ki)
!==========================================================================
!
!  Finds the index of the value in a monotonically increasing array
!
!  x      :  array of monotonically increasing values
!  z      :  value to be indexed
!  kstart :  (optional) restrict search to x(kstart:)
!
!  ki     : index k : if x(k) <= z < x(k+1), or
!               n-1 : if z = x(n)
!--------------------------------------------------------------------------

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: x(:), z
integer, intent(in), optional :: kstart

integer :: ki

integer :: ku, kl, km, n

n = size(x)

if ((z .gt. x(1)) .and. (z .lt. x(n))) then

   if (present(kstart)) then
      kl = kstart
   else
      kl = 1
   end if

   ku = n
   do while (ku-kl .gt. 1)
      km = (ku + kl) / 2
      if (z .gt. x(km)) then
         kl = km
      else
         ku = km
      endif
   end do
   ki = kl
   if (z .eq. x(ki+1)) ki = ki + 1

elseif (z .le. x(1)) then

      ki = 1

else    !if (z.ge.x(n)) then - removed (GBH 3/6/2015) so z=NaN has somewhere to go (otherwise ki is undefined and gives segmentation fault)

      ki = n-1

end if

return
end function

!--------------------------------------------------------------------------

