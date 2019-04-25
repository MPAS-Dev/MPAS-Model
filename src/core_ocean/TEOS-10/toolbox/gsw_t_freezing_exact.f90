!==========================================================================
elemental function gsw_t_freezing_exact (sa, p, saturation_fraction)
!==========================================================================
!
!  Calculates the in-situ temperature at which seawater freezes. The 
!  in-situ temperature freezing point is calculated from the exact 
!  in-situ freezing temperature which is found by a modified Newton-Raphson
!  iteration (McDougall and Wotherspoon, 2013) of the equality of the 
!  chemical potentials of water in seawater and in ice.
!
!  An alternative GSW function, gsw_t_freezing_poly, it is based on a 
!  computationally-efficient polynomial, and is accurate to within -5e-4 K 
!  and 6e-4 K, when compared with this function.
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar ) 
!  saturation_fraction = the saturation fraction of dissolved air in 
!                        seawater
!  (i.e., saturation_fraction must be between 0 and 1, and the default 
!    is 1, completely saturated) 
!
!  t_freezing = in-situ temperature at which seawater freezes.    [ deg C ]
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : gsw_sso

use gsw_mod_toolbox, only : gsw_gibbs_ice, gsw_chem_potential_water_t_exact
use gsw_mod_toolbox, only : gsw_t_deriv_chem_potential_water_t_exact
use gsw_mod_toolbox, only : gsw_t_freezing_poly

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, p, saturation_fraction

real (r8) :: gsw_t_freezing_exact

real (r8) :: df_dt, tf, tfm, tf_old, f

! The initial value of t_freezing_exact (for air-free seawater)
tf = gsw_t_freezing_poly(sa,p,polynomial=.true.)

df_dt = 1e3_r8*gsw_t_deriv_chem_potential_water_t_exact(sa,tf,p) - &
                gsw_gibbs_ice(1,0,tf,p)
! df_dt here is the initial value of the derivative of the function f whose
! zero (f = 0) we are finding (see Eqn. (3.33.2) of IOC et al (2010)).  

tf_old = tf
f = 1e3_r8*gsw_chem_potential_water_t_exact(sa,tf_old,p) - &
                gsw_gibbs_ice(0,0,tf_old,p)
tf = tf_old - f/df_dt
tfm = 0.5_r8*(tf + tf_old)
df_dt = 1e3_r8*gsw_t_deriv_chem_potential_water_t_exact(sa,tfm,p) - &
                gsw_gibbs_ice(1,0,tfm,p)
tf = tf_old - f/df_dt

tf_old = tf
f = 1e3_r8*gsw_chem_potential_water_t_exact(sa,tf_old,p) - &
                gsw_gibbs_ice(0,0,tf_old,p)
tf = tf_old - f/df_dt

! Adjust for the effects of dissolved air
gsw_t_freezing_exact = tf - &
                saturation_fraction*(1e-3_r8)*(2.4_r8 - sa/(2.0_r8*gsw_sso)) 

return
end function

!--------------------------------------------------------------------------
