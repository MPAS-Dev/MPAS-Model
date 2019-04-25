!==========================================================================
pure subroutine gsw_add_mean (data_in, data_out)
!==========================================================================
!
! Replaces NaN's with non-nan mean of the 4 adjacent neighbours
!
! data_in   : data set of the 4 adjacent neighbours   
!
! data_out : non-nan mean of the 4 adjacent neighbours     [unitless]
!--------------------------------------------------------------------------

use gsw_mod_kinds

implicit none

real (r8), intent(in), dimension(4) :: data_in
real (r8), intent(out), dimension(4) :: data_out

integer :: k, nmean

real (r8) :: data_mean

nmean = 0
data_mean = 0.0_r8

do k = 1,4
   if (abs(data_in(k)).le.100.0_r8) then
      nmean = nmean + 1
      data_mean = data_mean + data_in(k)
   end if
end do

if(nmean.eq.0)then
   data_mean = 0.0_r8    !error return
else
   data_mean = data_mean/nmean
endif

do k = 1,4
   if(abs(data_in(k)).le.100.0_r8) then
      data_out(k) = data_in(k)
   else
      data_out(k) = data_mean
   end if
end do

return
end subroutine

!--------------------------------------------------------------------------
