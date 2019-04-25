!==========================================================================
elemental function gsw_latentheat_melting (sa, p)  
!==========================================================================
!
! Calculates latent heat, or enthalpy, of melting.
!
! sa     : Absolute Salinity                               [g/kg]
! p      : sea pressure                                    [dbar]
! 
! gsw_latentheat_melting : latent heat of melting          [kg/m^3]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_enthalpy_ice, gsw_t_freezing_exact
use gsw_mod_toolbox, only : gsw_chem_potential_water_t_exact
use gsw_mod_toolbox, only : gsw_t_deriv_chem_potential_water_t_exact

use gsw_mod_teos10_constants, only : gsw_sfac, gsw_t0

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, p  

real (r8) :: gsw_latentheat_melting

real (r8) :: tf

tf = gsw_t_freezing_exact(sa,p,0.0_r8);

gsw_latentheat_melting = 1000.0_r8*(gsw_chem_potential_water_t_exact(sa,tf,p) &
           - (gsw_t0 + tf)*gsw_t_deriv_chem_potential_water_t_exact(sa,tf,p)) &
           - gsw_enthalpy_ice(tf,p);

return
end function

!--------------------------------------------------------------------------
