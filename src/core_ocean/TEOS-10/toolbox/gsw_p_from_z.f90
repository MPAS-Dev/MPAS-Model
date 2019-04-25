!==========================================================================
elemental function gsw_p_from_z (z, lat, geo_strf_dyn_height, &
                                                   sea_surface_geopotental)
!==========================================================================
!
!  Calculates sea pressure from height using computationally-efficient 
!  75-term expression for density, in terms of SA, CT and p (Roquet et al.,
!  2015).  Dynamic height anomaly, geo_strf_dyn_height, if provided,
!  must be computed with its p_ref = 0 (the surface). Also if provided,
!  sea_surface_geopotental is the geopotential at zero sea pressure. This 
!  function solves Eqn.(3.32.3) of IOC et al. (2010) iteratively for p.  
!
!  Note. Height (z) is NEGATIVE in the ocean.  Depth is -z.  
!    Depth is not used in the GSW computer software library. 
!
!  z  =  height                                                       [ m ]
!  lat  =  latitude in decimal degrees north                [ -90 ... +90 ]
!   
! OPTIONAL:
!  geo_strf_dyn_height = dynamic height anomaly                 [ m^2/s^2 ]
!    Note that the reference pressure, p_ref, of geo_strf_dyn_height must
!     be zero (0) dbar.
!  sea_surface_geopotental = geopotential at zero sea pressure  [ m^2/s^2 ]
!
!  p  =  sea pressure                                              [ dbar ]
!        ( i.e. absolute pressure - 10.1325 dbar )
!==========================================================================

use gsw_mod_toolbox, only : gsw_enthalpy_sso_0, gsw_specvol_sso_0

use gsw_mod_teos10_constants, only : db2pa, deg2rad, gamma

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: z, lat
real (r8), intent(in), optional :: geo_strf_dyn_height
real (r8), intent(in), optional :: sea_surface_geopotental

real (r8) :: gsw_p_from_z

real (r8) :: c1, df_dp, f, g, gs, p, p_mid, p_old, sin2
real (r8) :: gsdh, ssg

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
             ! ocean models, then g would be set to be zero here,
             ! and the code below works perfectly well.  

sin2 = sin(lat*deg2rad)**2
gs = 9.780327_r8*(1.0_r8 + (5.2792e-3_r8 + (2.32e-5_r8*sin2))*sin2)

! Get the first estimate of p from Saunders (1981)
c1 =  5.25e-3_r8*sin2 + 5.92e-3_r8
p  = -2.0_r8*z/((1.0_r8-c1) + sqrt((1.0_r8-c1)*(1.0_r8-c1) + 8.84e-6_r8*z)) 

! Initial value of the derivative of f
df_dp = db2pa*gsw_specvol_sso_0(p)

f = gsw_enthalpy_sso_0(p) + gs*(z - 0.5_r8*g*(z*z)) - (gsdh + ssg)
p_old = p
p = p_old - f/df_dp
p_mid = 0.5_r8*(p + p_old)
df_dp = db2pa*gsw_specvol_sso_0(p_mid)
p = p_old - f/df_dp

! After this one iteration through this modified Newton-Raphson iterative
! procedure (McDougall and Wotherspoon, 2013), the remaining error in p is 
! at computer machine precision, being no more than 1.6e-10 dbar. 

gsw_p_from_z = p

return
end function

!--------------------------------------------------------------------------
