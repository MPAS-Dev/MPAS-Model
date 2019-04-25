!==========================================================================
elemental subroutine gsw_ice_fraction_to_freeze_seawater (sa, ct, p, &
                                          t_ih, sa_freeze, ct_freeze, w_ih)
!==========================================================================
!
!  Calculates the mass fraction of ice (mass of ice divided by mass of ice
!  plus seawater), which, when melted into seawater having (SA,CT,p) causes 
!  the final dilute seawater to be at the freezing temperature.  The other
!  outputs are the Absolute Salinity and Conservative Temperature of the
!  final diluted seawater.  
!
!  SA   =  Absolute Salinity of seawater                           [ g/kg ]
!  CT   =  Conservative Temperature of seawater (ITS-90)          [ deg C ]
!  p    =  sea pressure                                            [ dbar ]
!            ( i.e. absolute pressure - 10.1325d0 dbar )
!  t_Ih =  in-situ temperature of the ice at pressure p (ITS-90)  [ deg C ]
!
!  SA_freeze = Absolute Salinity of seawater after the mass fraction of 
!              ice, ice_fraction, at temperature t_Ih has melted into the
!              original seawater, and the final mixture is at the freezing
!              temperature of seawater.                            [ g/kg ]
!
!  CT_freeze = Conservative Temperature of seawater after the mass 
!              fraction, w_Ih, of ice at temperature t_Ih has melted into
!              the original seawater, and the final mixture is at the
!              freezing temperature of seawater.                  [ deg C ]
!
!  w_Ih      = mass fraction of ice, having in-situ temperature t_Ih, 
!              which, when melted into seawater at (SA,CT,p) leads to the
!              final diluted seawater being at the freezing temperature.
!              This output must be between 0 and 1.              [unitless]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_enthalpy_ice, gsw_t_freezing_exact
use gsw_mod_toolbox, only : gsw_enthalpy_first_derivatives_ct_exact
use gsw_mod_toolbox, only : gsw_ct_freezing_exact, gsw_enthalpy_ct_exact
use gsw_mod_toolbox, only : gsw_ct_freezing_first_derivatives

use gsw_mod_error_functions, only : gsw_error_code, gsw_error_limit

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct, p, t_ih
real (r8), intent(out) :: sa_freeze, ct_freeze, w_ih

integer :: no_iter
real (r8) :: ctf, ctf_mean, ctf_old, ctf_plus1, ctf_zero
real (r8) :: dfunc_dsaf, func, func_plus1, func_zero, h, h_ih
real (r8) :: saf, saf_mean, saf_old, tf, h_hat_sa, h_hat_ct, ctf_sa

real (r8), parameter :: sa0 = 0.0_r8, saturation_fraction = 0.0_r8

character (*), parameter :: func_name = "gsw_ice_fraction_to_freeze_seawater"

ctf = gsw_ct_freezing_exact(sa,p,saturation_fraction)
if (ct .lt. ctf) then
    ! The seawater ct input is below the freezing temp
    sa_freeze = gsw_error_code(1,func_name)
    ct_freeze = sa_freeze
    w_ih = sa_freeze
    return
end if

tf = gsw_t_freezing_exact(sa0,p,saturation_fraction)
if (t_ih .gt. tf) then
    ! The input, t_Ih, exceeds the freezing temperature at sa = 0
    sa_freeze = gsw_error_code(2,func_name)
    ct_freeze = sa_freeze
    w_ih = sa_freeze
    return
end if

h = gsw_enthalpy_ct_exact(sa,ct,p)
h_ih = gsw_enthalpy_ice(t_ih,p)

ctf_zero = gsw_ct_freezing_exact(sa0,p,saturation_fraction)
func_zero = sa*(gsw_enthalpy_ct_exact(sa0,ctf_zero,p) - h_ih)

ctf_plus1 = gsw_ct_freezing_exact(sa+1.0_r8,p,saturation_fraction)
func_plus1 = sa*(gsw_enthalpy_ct_exact(sa+1.0_r8,ctf_plus1,p) - h) - (h - h_ih)

saf = -(sa+1.0_r8)*func_zero/(func_plus1 - func_zero)      ! initial guess
ctf = gsw_ct_freezing_exact(saf,p,saturation_fraction)
call gsw_enthalpy_first_derivatives_ct_exact(saf,ctf,p,h_hat_sa,h_hat_ct)
call gsw_ct_freezing_first_derivatives(saf,p,1.0_r8,ctfreezing_sa=ctf_sa)

dfunc_dsaf = sa*(h_hat_sa + h_hat_ct*ctf_sa) - (h - h_ih)

do no_iter = 1, 2
    saf_old = saf
    ctf_old = ctf
    func = sa*(gsw_enthalpy_ct_exact(saf_old,ctf_old,p) - h) &
           - (saf_old - sa)*(h - h_ih)
    saf = saf_old - func/dfunc_dsaf
    saf_mean = 0.5_r8*(saf + saf_old)
    ctf_mean = gsw_ct_freezing_exact(saf_mean,p,saturation_fraction)
    call gsw_enthalpy_first_derivatives_ct_exact(saf_mean,ctf_mean,p,h_hat_sa, &
                                                 h_hat_ct)
    call gsw_ct_freezing_first_derivatives(saf_mean,p,saturation_fraction, &
                                           ctfreezing_sa=ctf_sa)
    dfunc_dsaf = sa*(h_hat_sa + h_hat_ct*ctf_sa) - (h - h_ih)
    saf = saf_old - func/dfunc_dsaf 
    ctf = gsw_ct_freezing_exact(saf,p,saturation_fraction)
end do

! After these 2 iterations of this modified Newton-Raphson method, the
! error in SA_freeze is less than 1.3d0x10^-13 g/kg, in CT_freeze is less than
! 4x10^-13 deg C and in w_Ih is less than 3.8d0x10^-15 which represent machine 
! precision for these calculations. 

sa_freeze = saf
ct_freeze = ctf
w_ih = (h - gsw_enthalpy_ct_exact(sa_freeze,ct_freeze,p))/(h - h_ih)

return
end subroutine
