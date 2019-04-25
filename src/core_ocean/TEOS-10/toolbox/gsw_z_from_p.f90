!==========================================================================
elemental function gsw_z_from_p (p, lat, geo_strf_dyn_height, &
                                                   sea_surface_geopotental)
!==========================================================================
!
!  Calculates height from sea pressure using the computationally-efficient
!  75-term expression for specific volume in terms of SA, CT and p 
!  (Roquet et al., 2015).  Dynamic height anomaly, geo_strf_dyn_height, if
!  provided, must be computed with its p_ref = 0 (the surface).  Also if
!  provided, sea_surface_geopotental is the geopotential at zero sea 
!  pressure. This function solves Eqn.(3.32.3) of IOC et al. (2010).  
!
!  Note. Height z is NEGATIVE in the ocean. i.e. Depth is -z.  
!   Depth is not used in the GSW computer software library.  
!
!  p    =  sea pressure                                            [ dbar ]
!          ( i.e. absolute pressure - 10.1325 dbar )
!  lat  =  latitude in decimal degrees north                [ -90 ... +90 ]
!
! OPTIONAL:
!  geo_strf_dyn_height = dynamic height anomaly                 [ m^2/s^2 ]
!    Note that the refernce pressure, p_ref, of geo_strf_dyn_height must be 
!    zero (0) dbar.
!  sea_surface_geopotental = geopotential at zero sea pressure  [ m^2/s^2 ]
!
!  z  =  height                                                       [ m ]
!
!==========================================================================

use gsw_mod_toolbox, only : gsw_enthalpy_sso_0

use gsw_mod_teos10_constants, only : deg2rad, gamma

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: p, lat
real (r8), intent(in), optional :: geo_strf_dyn_height
real (r8), intent(in), optional :: sea_surface_geopotental

real (r8) :: gsw_z_from_p

real (r8) :: a, b, c, g, sin2, gsdh, ssg

if (present(geo_strf_dyn_height)) then
    gsdh = geo_strf_dyn_height
else
    gsdh = 0.0_r8
end if

if (present(sea_surface_geopotental)) then
    ssg = sea_surface_geopotental
else
    ssg = 0.0_r8
end if

g = gamma    ! If the graviational acceleration were to be regarded as 
             ! being depth-independent, which is often the case in 
             ! ocean models, then gamma would be set to be zero here,
	     ! and the code below works perfectly well.

sin2 = sin(lat*deg2rad)**2
b = 9.780327_r8*(1.0_r8 + (5.2792e-3_r8 + (2.32e-5_r8*sin2))*sin2) 
a = -0.5_r8*g*b
c = gsw_enthalpy_sso_0(p) - (gsdh + ssg)

gsw_z_from_p = -2.0_r8*c/(b + sqrt(b*b - 4.0_r8*a*c))

return
end function

!--------------------------------------------------------------------------
