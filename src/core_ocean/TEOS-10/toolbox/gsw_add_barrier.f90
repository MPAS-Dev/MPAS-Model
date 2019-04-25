!==========================================================================
pure subroutine gsw_add_barrier (input_data, long, lat, long_grid, &
                              lat_grid, dlong_grid, dlat_grid, output_data)
!==========================================================================
!
!  Adds a barrier through Central America (Panama) and then averages
!  over the appropriate side of the barrier
! 
!  data_in      : data                                            [unitless]
!  long         : Long of data in decimal degs east               [ 0 ... +360 ]
!  lat          : Lat of data in decimal degs north               [-90 ... +90 ]
!  longs_grid   : Long of regular grid in decimal degs east       [ 0 ... +360 ]
!  lats_grid    : Lat of regular grid in decimal degs north       [-90 ... +90 ]
!  dlongs_grid  : Long difference of regular grid in decimal degs [ deg long ]
!  dlats_grid   : Lat difference of regular grid in decimal degs  [ deg lat ]
!
! output_data   : average of data depending on which side of the 
!                 Panama canal it is on                           [unitless]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_util_indx

use gsw_mod_saar_data

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: long, lat, long_grid, lat_grid, dlong_grid
real (r8), intent(in) :: dlat_grid
real (r8), intent(in), dimension(4) :: input_data
real (r8), intent(out), dimension(4) :: output_data

integer, dimension(4) :: above_line
integer :: k, nmean, above_line0, kk
real (r8) :: r, lats_line, data_mean

k = gsw_util_indx(longs_pan,long)                !   the long/lat point
r = (long-longs_pan(k))/(longs_pan(k+1)-longs_pan(k))
lats_line = lats_pan(k) + r*(lats_pan(k+1)-lats_pan(k))

if (lats_line.le.lat) then
   above_line0 = 1
else
   above_line0 = 0
end if

k = gsw_util_indx(longs_pan,long_grid)       ! the 1 and 4 long/lat pts
r = (long_grid-longs_pan(k))/(longs_pan(k+1)-longs_pan(k))
lats_line = lats_pan(k) + r*(lats_pan(k+1)-lats_pan(k))

if (lats_line.le.lat_grid) then
   above_line(1) = 1
else
   above_line(1) = 0
end if

if (lats_line.le.lat_grid+dlat_grid) then
   above_line(4) = 1
else
   above_line(4) = 0
end if

k = gsw_util_indx(longs_pan,long_grid+dlong_grid)    ! the 2 & 3 points 
r = (long_grid+dlong_grid-longs_pan(k))/(longs_pan(k+1)-longs_pan(k))
lats_line = lats_pan(k) + r*(lats_pan(k+1)-lats_pan(k))

if (lats_line.le.lat_grid) then
   above_line(2) = 1
else
   above_line(2) = 0
end if

if (lats_line.le.lat_grid+dlat_grid) then
   above_line(3) = 1
else
   above_line(3) = 0
end if

nmean = 0 
data_mean = 0.0_r8

do kk = 1,4
   if ((abs(input_data(kk)).le.100._r8).and.above_line0.eq.above_line(kk)) then
      nmean = nmean + 1
      data_mean = data_mean + input_data(kk)
   end if
end do

if (nmean .eq. 0)then
   data_mean = 0.0_r8    !errorreturn
else
   data_mean = data_mean/nmean
endif

do kk = 1,4
   if ((abs(input_data(kk)).ge.1e10_r8).or.above_line0.ne.above_line(kk)) then
      output_data(kk) = data_mean
   else
      output_data(kk) = input_data(kk)
   end if
end do

return
end subroutine

!--------------------------------------------------------------------------
