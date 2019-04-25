!==========================================================================
elemental function gsw_melting_ice_sa_ct_ratio (sa, ct, p, t_ih)
!==========================================================================
!
!  Calculates the ratio of SA to CT changes when ice melts into seawater.
!  It is assumed that a small mass of ice melts into an infinite mass of
!  seawater.  Because of the infinite mass of seawater, the ice will always
!  melt.   
!
!  The output, melting_seaice_SA_CT_ratio, is dSA/dCT rather than dCT/dSA. 
!  This is done so that when SA = 0, the output, dSA/dCT is zero whereas 
!  dCT/dSA would be infinite. 
!
!  SA   =  Absolute Salinity of seawater                           [ g/kg ]
!  CT   =  Conservative Temperature of seawater (ITS-90)          [ deg C ]
!  p    =  sea pressure at which the melting occurs                [ dbar ]
!         ( i.e. absolute pressure - 10.1325d0 dbar ) 
!  t_Ih =  the in-situ temperature of the ice (ITS-90)            [ deg C ]
!
!  melting_ice_SA_CT_ratio = the ratio of SA to CT changes when ice melts
!                            into a large mass of seawater 
!                                                          [ g kg^-1 K^-1 ] 
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_ct_freezing_exact, gsw_enthalpy_ct_exact
use gsw_mod_toolbox, only : gsw_t_freezing_exact, gsw_enthalpy_ice
use gsw_mod_toolbox, only : gsw_enthalpy_first_derivatives_ct_exact

use gsw_mod_error_functions, only : gsw_error_code, gsw_error_limit

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct, p, t_ih

real (r8) :: gsw_melting_ice_sa_ct_ratio

real (r8) :: ctf, h, h_ih, tf, h_hat_sa, h_hat_ct

real (r8), parameter :: saturation_fraction = 0.0_r8

character (*), parameter :: func_name = "gsw_melting_ice_sa_ct_ratio"

ctf = gsw_ct_freezing_exact(sa,p,saturation_fraction)
if (ct .lt. ctf) then
    ! the seawater ct input is below the freezing temperature
    gsw_melting_ice_sa_ct_ratio = gsw_error_code(1,func_name)
    return
end if

tf = gsw_t_freezing_exact(0.0_r8,p,saturation_fraction)
if (t_ih .gt. tf) then
    ! t_ih exceeds the freezing temperature at sa = 0
    gsw_melting_ice_sa_ct_ratio = gsw_error_code(2,func_name)
    return
end if

h = gsw_enthalpy_ct_exact(sa,ct,p)
h_ih = gsw_enthalpy_ice(t_ih,p)
call gsw_enthalpy_first_derivatives_ct_exact(sa,ct,p,h_hat_sa,h_hat_ct)
	! Note that h_hat_CT is equal to cp0*(273.15 + t)/(273.15 + pt0)

gsw_melting_ice_sa_ct_ratio = sa*h_hat_ct/(h - h_ih - sa*h_hat_sa)

return
end function

!--------------------------------------------------------------------------
