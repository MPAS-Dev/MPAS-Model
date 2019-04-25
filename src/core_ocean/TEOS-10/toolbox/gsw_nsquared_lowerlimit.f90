!==========================================================================
elemental function gsw_nsquared_lowerlimit (p, long, lat)
!==========================================================================
!
!  Calculates the minimum buoyancy (Brunt-Vaisala) frequency squared (N^2)
!  such that a cast is stable.
!
!  p  =  sea pressure                                              [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar )
!  long =  longitude in decimal degrees                      [ 0 ... +360 ]
!                                                     or  [ -180 ... +180 ]
!  lat  =  latitude in decimal degrees north                [ -90 ... +90 ]
!
!  nsquared_lowerlimit  = Minimum Brunt-Vaisala Frequency squared [ 1/s^2 ]
!
!==========================================================================

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: p, long, lat

real (r8) :: gsw_nsquared_lowerlimit

real (r8) :: long360

if (long .lt. 0.0_r8) then
    long360 = long + 360.0_r8
else
    long360 = long
end if

gsw_nsquared_lowerlimit = (0.25_r8 + 0.75_r8*(exp(-p/1000.0_r8))) * 1e-7_r8

return
end function

!--------------------------------------------------------------------------
