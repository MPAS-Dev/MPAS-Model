!==========================================================================
elemental subroutine gsw_seaice_fraction_to_freeze_seawater (sa, ct, p, &
                       sa_seaice, t_seaice, sa_freeze, ct_freeze, w_seaice)
!==========================================================================
!
!  Calculates the mass fraction of sea ice (mass of sea ice divided by mass 
!  of sea ice plus seawater), which, when melted into seawater having the
!  properties (SA,CT,p) causes the final seawater to be at the freezing 
!  temperature.  The other outputs are the Absolute Salinity and 
!  Conservative Temperature of the final seawater.  
!
!  SA        =  Absolute Salinity of seawater                      [ g/kg ]
!  CT        =  Conservative Temperature of seawater (ITS-90)     [ deg C ]
!  p         =  sea pressure                                       [ dbar ]
!            ( i.e. absolute pressure - 10.1325 dbar )
!  SA_seaice =  Absolute Salinity of sea ice, that is, the mass fraction of
!               salt in sea ice, expressed in g of salt per kg of sea ice.
!                                                                  [ g/kg ]
!  t_seaice  =  in-situ temperature of the sea ice at pressure p (ITS-90)
!                                                                 [ deg C ]
!
!  SA_freeze  =  Absolute Salinity of seawater after the mass fraction of
!                sea ice, w_seaice, at temperature t_seaice has melted into
!                the original seawater, and the final mixture is at the 
!                freezing temperature of seawater.                 [ g/kg ]
!
!  CT_freeze  =  Conservative Temperature of seawater after the mass 
!                fraction, w_seaice, of sea ice at temperature t_seaice has
!                melted into the original seawater, and the final mixture 
!                is at the freezing temperature of seawater.      [ deg C ]
!
!  w_seaice   =  mass fraction of sea ice, at SA_seaice and t_seaice, 
!                which, when melted into seawater at (SA,CT,p) leads to the
!                final mixed seawater being at the freezing temperature.  
!                This output is between 0 and 1.                 [unitless]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_ct_freezing_exact, gsw_enthalpy_ct_exact
use gsw_mod_toolbox, only : gsw_sa_freezing_from_t, gsw_enthalpy_ice
use gsw_mod_toolbox, only : gsw_enthalpy_t_exact, gsw_t_freezing_exact
use gsw_mod_toolbox, only : gsw_enthalpy_first_derivatives_ct_exact
use gsw_mod_toolbox, only : gsw_ct_freezing_first_derivatives

use gsw_mod_error_functions, only : gsw_error_code, gsw_error_limit

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct, p, sa_seaice, t_seaice
real (r8), intent(out) :: sa_freeze, ct_freeze, w_seaice

integer :: number_of_iterations
real (r8) :: ctf, ctf_mean, ctf_old, ctf_plus1, ctf_zero
real (r8) :: dfunc_dsaf, func, func_plus1, func_zero, h, h_brine
real (r8) :: h_ih, sa_freezing, saf, saf_mean, saf_old
real (r8) :: salt_ratio, tf_sa_seaice, h_hat_sa, h_hat_ct, ctf_sa

real (r8), parameter :: sa0 = 0.0_r8, saturation_fraction = 0.0_r8

character (*), parameter :: func_name = "gsw_seaice_fraction_to_freeze_seawater"

ctf = gsw_ct_freezing_exact(sa,p,saturation_fraction)
if (ct .lt. ctf) then
    ! The seawater ct input is below the freezing temp
    sa_freeze = gsw_error_code(1,func_name)
    ct_freeze = sa_freeze
    w_seaice = sa_freeze
    return
end if

tf_sa_seaice = gsw_t_freezing_exact(sa_seaice,p,saturation_fraction) - 1e-6_r8
if (t_seaice .gt. tf_sa_seaice) then
    ! The 1e-6 C buffer in the allowable t_seaice is to ensure that there is
    ! some ice Ih in the sea ice.   Without this buffer, that is if t_seaice
    ! is allowed to be exactly equal to tf_sa_seaice, the sea ice is 
    ! actually 100% brine at Absolute Salinity of SA_seaice.
    sa_freeze = gsw_error_code(2,func_name)
    ct_freeze = sa_freeze
    w_seaice = sa_freeze
    return
end if

sa_freezing = gsw_sa_freezing_from_t(t_seaice,p,saturation_fraction)
if (sa_freezing .gt. gsw_error_limit) then
    sa_freeze = gsw_error_code(3,func_name,sa_freezing)
    ct_freeze = sa_freeze
    w_seaice = sa_freeze
    return
end if
h_brine = gsw_enthalpy_t_exact(sa_freezing,t_seaice,p)
salt_ratio = sa_seaice/sa_freezing

h = gsw_enthalpy_ct_exact(sa,ct,p)
h_ih = gsw_enthalpy_ice(t_seaice,p)

ctf_plus1 = gsw_ct_freezing_exact(sa+1.0_r8,p,saturation_fraction)
func_plus1 = (sa - sa_seaice)*(gsw_enthalpy_ct_exact(sa+1.0_r8,ctf_plus1,p) &
                - h) - (h - h_ih) + salt_ratio*(h_brine - h_ih)

ctf_zero = gsw_ct_freezing_exact(sa0,p,saturation_fraction)
func_zero = (sa - sa_seaice)*(gsw_enthalpy_ct_exact(sa0,ctf_zero,p) - h) &
             + sa*((h - h_ih) - salt_ratio*(h_brine - h_ih))

saf = -(sa+1.0_r8)*func_zero/(func_plus1 - func_zero) ! initial guess of saf
ctf = gsw_ct_freezing_exact(saf,p,saturation_fraction)
call gsw_enthalpy_first_derivatives_ct_exact(saf,ctf,p,h_hat_sa,h_hat_ct)
call gsw_ct_freezing_first_derivatives(saf,p,saturation_fraction, &
                                       ctfreezing_sa=ctf_sa)

dfunc_dsaf = (sa - sa_seaice)*(h_hat_sa + h_hat_ct*ctf_sa) &
              - (h - h_ih) + salt_ratio*(h_brine - h_ih)

do number_of_iterations = 1, 4
    saf_old = saf   
    ctf_old = ctf
    func = (sa - sa_seaice)*(gsw_enthalpy_ct_exact(saf_old,ctf_old,p) - h) &
         - (saf_old - sa)*((h - h_ih) - salt_ratio*(h_brine - h_ih))
    saf = saf_old - func/dfunc_dsaf
    saf_mean = 0.5_r8*(saf + saf_old)
    ctf_mean = gsw_ct_freezing_exact(saf_mean,p,saturation_fraction)
    call gsw_enthalpy_first_derivatives_ct_exact(saf_mean,ctf_mean,p, &
                                                 h_hat_sa,h_hat_ct)
    call gsw_ct_freezing_first_derivatives(saf_mean,p,saturation_fraction, &
                                           ctfreezing_sa=ctf_sa)
    dfunc_dsaf = (sa - sa_seaice)*(h_hat_sa + h_hat_ct*ctf_sa) &
                  - (h - h_ih) + salt_ratio*(h_brine - h_ih)
    saf = saf_old - func/dfunc_dsaf
    ctf = gsw_ct_freezing_exact(saf,p,saturation_fraction)
end do

! After these 4 iterations of this modified Newton-Raphson method, the
! errors in SA_freeze is less than 1.5x10^-12 g/kg, in CT_freeze is less than
! 2x10^-13 deg C and in w_seaice is less than 2.8x10^-13 which represent machine
! precision for these calculations. 

sa_freeze = saf
ct_freeze = ctf
w_seaice = (h - gsw_enthalpy_ct_exact(sa_freeze,ct_freeze,p)) / &
                           (h - h_ih - salt_ratio*(h_brine - h_ih))
return
end subroutine

!--------------------------------------------------------------------------
