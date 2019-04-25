!==========================================================================
elemental function gsw_grav (lat, p)  
!==========================================================================
!
! Calculates acceleration due to gravity as a function of latitude and as
!  a function of pressure in the ocean.
!
! lat  =  latitude in decimal degress north                [ -90 ... +90 ]  
!  p  =  sea pressure                                              [ dbar ]
! 
! gsw_grav : grav  =  gravitational acceleration               [ m s^-2 ]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_z_from_p

use gsw_mod_teos10_constants, only : deg2rad, gamma

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: lat, p  

real (r8) :: gsw_grav

real (r8) :: sin2, gs, z

sin2 = sin(lat*deg2rad)**2
gs = 9.780327_r8*(1.0_r8 + (5.2792e-3_r8 + (2.32e-5_r8*sin2))*sin2) 

z = gsw_z_from_p(p,lat)

gsw_grav = gs*(1.0_r8 - gamma*z)        ! z is the height corresponding to p.
                                        ! Note. In the ocean z is negative.
return
end function

!--------------------------------------------------------------------------
