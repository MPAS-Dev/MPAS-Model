!==========================================================================
elemental function gsw_melting_ice_equilibrium_sa_ct_ratio_poly (sa, p)
!==========================================================================
!
!  Calculates the ratio of SA to CT changes when ice melts into seawater
!  with both the seawater and the seaice temperatures being almost equal to
!  the equilibrium freezing temperature.  It is assumed that a small mass 
!  of ice melts into an infinite mass of seawater.  If indeed the 
!  temperature of the seawater and the ice were both equal to the freezing
!  temperature, then no melting or freezing would occur an imbalance 
!  between these three temperatures is needed for freezing or melting to
!  occur (the three temperatures being (1) the seawater temperature, 
!  (2) the ice temperature, and (3) the freezing temperature.  
!
!  The output, melting_ice_equilibrium_SA_CT_ratio, is dSA/dCT rather than 
!  dCT/dSA.  This is done so that when SA = 0, the output, dSA/dCT is zero
!  whereas dCT/dSA would be infinite. 
!
!  SA  =  Absolute Salinity of seawater                            [ g/kg ]
!  p   =  sea pressure at which the melting occurs                 [ dbar ]
!         ( i.e. absolute pressure - 10.1325d0 dbar ) 
!
!  melting_ice_equilibrium_SA_CT_ratio = the ratio dSA/dCT of SA to CT  
!                                changes when ice melts into seawater, with   
!                                the seawater and seaice being close to the  
!                                freezing temperature.         [ g/(kg K) ] 
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_ct_freezing_poly, gsw_enthalpy
use gsw_mod_toolbox, only : gsw_t_freezing_poly, gsw_enthalpy_ice
use gsw_mod_toolbox, only : gsw_enthalpy_first_derivatives

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, p

real (r8) :: gsw_melting_ice_equilibrium_sa_ct_ratio_poly

real (r8) :: ctf, h, h_ih, t_seaice, h_hat_sa, h_hat_ct

real (r8), parameter :: saturation_fraction = 0.0_r8

ctf = gsw_ct_freezing_poly(sa,p,saturation_fraction)
t_seaice = gsw_t_freezing_poly(sa,p,saturation_fraction)

h = gsw_enthalpy(sa,ctf,p)
h_ih = gsw_enthalpy_ice(t_seaice,p)
call gsw_enthalpy_first_derivatives(sa,ctf,p,h_hat_sa,h_hat_ct)
          ! note that h_hat_ct is equal to cp0*(273.15 + t)/(273.15 + pt0)

gsw_melting_ice_equilibrium_sa_ct_ratio_poly = sa*h_hat_ct &
                                                    / (h - h_ih - sa*h_hat_sa)

return
end function

!--------------------------------------------------------------------------
