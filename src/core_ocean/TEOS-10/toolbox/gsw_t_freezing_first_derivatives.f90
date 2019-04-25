!==========================================================================
elemental subroutine gsw_t_freezing_first_derivatives (sa, p, &
                            saturation_fraction, tfreezing_sa, tfreezing_p)
!==========================================================================
!
!  Calculates the frist derivatives of the in-situ temperature at which 
!  seawater freezes with respect to Absolute Salinity SA and pressure P (in
!  Pa).  These expressions come from differentiating the expression that
!  defines the freezing temperature, namely the equality between the 
!  chemical potentials of water in seawater and in ice.  
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar ) 
!  saturation_fraction = the saturation fraction of dissolved air in 
!                        seawater
!
!  tfreezing_SA = the derivative of the in-situ freezing temperature 
!                 (ITS-90) with respect to Absolute Salinity at fixed    
!                 pressure                     [ K/(g/kg) ] i.e. [ K kg/g ] 
!
!  tfreezing_P  = the derivative of the in-situ freezing temperature  
!                 (ITS-90) with respect to pressure (in Pa) at fixed  
!                 Absolute Salinity                                [ K/Pa ]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_t_deriv_chem_potential_water_t_exact
use gsw_mod_toolbox, only : gsw_specvol_ice, gsw_dilution_coefficient_t_exact
use gsw_mod_toolbox, only : gsw_entropy_ice, gsw_t_freezing_exact, gsw_gibbs
use gsw_mod_toolbox, only : gsw_specvol_t_exact

use gsw_mod_teos10_constants, only : gsw_sso

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, p, saturation_fraction
real (r8), intent(out), optional :: tfreezing_sa, tfreezing_p

real (r8) :: rec_denom, tf

real (r8), parameter :: g_per_kg = 1000.0_r8

tf = gsw_t_freezing_exact(sa,p,saturation_fraction) 
rec_denom = 1.0_r8/(g_per_kg*gsw_t_deriv_chem_potential_water_t_exact(sa,tf,p) &
                  + gsw_entropy_ice(tf,p))

if (present(tfreezing_sa)) tfreezing_sa = &
               gsw_dilution_coefficient_t_exact(sa,tf,p)*rec_denom &
               + saturation_fraction*(1e-3_r8)/(2.0_r8*gsw_sso)

if (present(tfreezing_p)) tfreezing_p = &
               -(gsw_specvol_t_exact(sa,tf,p) - sa*gsw_gibbs(1,0,1,sa,tf,p) &
               - gsw_specvol_ice(tf,p))*rec_denom

return
end subroutine

!--------------------------------------------------------------------------
