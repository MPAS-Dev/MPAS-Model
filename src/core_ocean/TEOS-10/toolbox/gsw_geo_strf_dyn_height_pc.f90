!==========================================================================
pure subroutine gsw_geo_strf_dyn_height_pc (sa, ct, delta_p, &
                                            geo_strf_dyn_height_pc, p_mid)
!==========================================================================
!
!  Calculates dynamic height anomaly as the integral of specific volume 
!  anomaly from the the sea surface pressure (0 Pa) to the pressure p.
!  This function, gsw_geo_strf_dyn_height_pc, is to used when the 
!  Absolute Salinity and Conservative Temperature are piecewise constant in 
!  the vertical over sucessive pressure intervals of delta_p (such as in
!  a forward "z-coordinate" ocean model).  "geo_strf_dyn_height_pc" is
!  the dynamic height anomaly with respect to the sea surface.  That is, 
!  "geo_strf_dyn_height_pc" is the geostrophic streamfunction for the 
!  difference between the horizontal velocity at the pressure concerned, p,
!  and the horizontal velocity at the sea surface.  Dynamic height anomaly 
!  is the geostrophic streamfunction in an isobaric surface.  The reference
!  values used for the specific volume anomaly are SA = SSO = 35.16504 g/kg
!  and CT = 0 deg C.  The output values of geo_strf_dyn_height_pc are 
!  given at the mid-point pressures, p_mid, of each layer in which SA and 
!  CT are vertically piecewice constant (pc).  This function calculates 
!  enthalpy using the computationally-efficient 75-term expression for 
!  specific volume of Roquet et al., (2015). 
!
!  SA       =  Absolute Salinity                                   [ g/kg ]
!  CT       =  Conservative Temperature (ITS-90)                  [ deg C ]
!  delta_p  =  difference in sea pressure between the deep and     [ dbar ]
!              shallow extents of each layer in which SA and CT
!              are vertically constant. delta_p must be positive.
!              
!  Note. sea pressure is absolute pressure minus 10.1325 dbar.
!
!  geo_strf_dyn_height_pc =  dynamic height anomaly             [ m^2/s^2 ]
!  p_mid                  =  mid-point pressure in each layer      [ dbar ]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_enthalpy_diff, gsw_enthalpy_sso_0

use gsw_mod_error_functions, only : gsw_error_code

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa(:), ct(:), delta_p(:)
real (r8), intent(out) :: geo_strf_dyn_height_pc(:), p_mid(:)

integer :: i, np
real (r8), allocatable :: delta_h(:), delta_h_half(:), dyn_height_deep(:)
real (r8), allocatable :: p_deep(:), p_shallow(:)

character (*), parameter :: func_name = "gsw_geo_strf_dyn_height_pc"

if (any(delta_p .lt. 0.0_r8)) then
    geo_strf_dyn_height_pc = gsw_error_code(1,func_name)
    p_mid = geo_strf_dyn_height_pc
    return
end if

np = size(delta_p)
allocate (delta_h(np),delta_h_half(np),dyn_height_deep(np))
allocate (p_deep(np),p_shallow(np))

p_deep(1) = delta_p(1)
do i = 2, np
   p_deep(i) = p_deep(i-1) + delta_p(i)
end do
p_shallow = p_deep - delta_p

delta_h = gsw_enthalpy_diff(sa,ct,p_shallow,p_deep)

dyn_height_deep(1) = -delta_h(1)
do i = 2, np
   dyn_height_deep(i) = dyn_height_deep(i-1) - delta_h(i)
end do
!            This is Phi minus Phi_0 of Eqn. (3.32.2) of IOC et al. (2010).

p_mid = 0.5_r8*(p_shallow  + p_deep)
delta_h_half = gsw_enthalpy_diff(sa,ct,p_mid,p_deep)

geo_strf_dyn_height_pc = gsw_enthalpy_sso_0(p_mid) + &
                           dyn_height_deep + delta_h_half

return
end subroutine

!--------------------------------------------------------------------------
