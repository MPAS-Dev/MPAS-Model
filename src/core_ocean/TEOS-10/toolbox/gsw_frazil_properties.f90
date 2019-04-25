!==========================================================================
elemental subroutine gsw_frazil_properties (sa_bulk, h_bulk, p, &
                                            sa_final, ct_final, w_ih_final)
!==========================================================================
!
!  Calculates the mass fraction of ice (mass of ice divided by mass of ice
!  plus seawater), w_Ih_final, which results from given values of the bulk
!  Absolute Salinity, SA_bulk, bulk enthalpy, h_bulk, occuring at pressure
!  p.  The final values of Absolute Salinity, SA_final, and Conservative
!  Temperature, CT_final, of the interstitial seawater phase are also
!  returned.  This code assumes that there is no dissolved air in the
!  seawater (that is, saturation_fraction is assumed to be zero
!  throughout the code).
!
!  When the mass fraction w_Ih_final is calculated as being a positive
!  value, the seawater-ice mixture is at thermodynamic equlibrium.  
!
!  This code returns w_Ih_final = 0 when the input bulk enthalpy, h_bulk, 
!  is sufficiently large (i.e. sufficiently "warm") so that there is no ice 
!  present in the final state.  In this case the final state consists of 
!  only seawater rather than being an equlibrium mixture of seawater and 
!  ice which occurs when w_Ih_final is positive.  Note that when 
!  w_Ih_final = 0, the final seawater is not at the freezing temperature. 
!
!  SA_bulk =  bulk Absolute Salinity of the seawater and ice mixture
!                                                                  [ g/kg ]
!  h_bulk  =  bulk enthalpy of the seawater and ice mixture        [ J/kg ]
!  p       =  sea pressure                                         [ dbar ]
!             ( i.e. absolute pressure - 10.1325 dbar )
!
!  SA_final    =  Absolute Salinity of the seawater in the final state, 
!                 whether or not any ice is present.               [ g/kg ]
!  CT_final    =  Conservative Temperature of the seawater in the the final
!                 state, whether or not any ice is present.       [ deg C ]
!  w_Ih_final  =  mass fraction of ice in the final seawater-ice mixture.
!                 If this ice mass fraction is positive, the system is at 
!                 thermodynamic equilibrium.  If this ice mass fraction is 
!                 zero there is no ice in the final state which consists 
!                 only of seawater which is warmer than the freezing 
!                 temperature.                                   [unitless]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_cp_ice, gsw_ct_freezing_exact
use gsw_mod_toolbox, only : gsw_ct_freezing_first_derivatives
use gsw_mod_toolbox, only : gsw_ct_from_enthalpy_exact, gsw_enthalpy_ct_exact
use gsw_mod_toolbox, only : gsw_enthalpy_first_derivatives_ct_exact
use gsw_mod_toolbox, only : gsw_enthalpy_ice, gsw_t_freezing_exact
use gsw_mod_toolbox, only : gsw_t_freezing_first_derivatives

use gsw_mod_error_functions, only : gsw_error_code, gsw_error_limit

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa_bulk, h_bulk, p
real (r8), intent(out) :: sa_final, ct_final, w_ih_final

integer :: number_of_iterations
real (r8) :: cp_ih, ctf_sa, ctf, dfunc_dw_ih, dfunc_dw_ih_mean_poly
real (r8) :: func, func0, hf, h_hat_ct, h_hat_sa
real (r8) :: h_ihf, sa, tf_sa, tf, w_ih_mean, w_ih_old, w_ih

! Throughout this code seawater is taken to contain no dissolved air.
real (r8), parameter :: saturation_fraction = 0.0_r8

real (r8), parameter :: num_f = 5.0e-2_r8
real (r8), parameter :: num_f2 = 6.9e-7_r8
real (r8), parameter :: num_p = 2.21_r8

character (*), parameter :: func_name = "gsw_frazil_properties"

!--------------------------------------------------------------------------
! Finding func0
!--------------------------------------------------------------------------
ctf = gsw_ct_freezing_exact(sa_bulk,p,saturation_fraction)
func0 = h_bulk - gsw_enthalpy_ct_exact(sa_bulk,ctf,p)

!--------------------------------------------------------------------------
! When func0 is zero or positive we can immediately calculate the three
! outputs, as the bulk enthalpy, h_bulk, is too large to allow any ice
! at thermodynamic equilibrium. The result will be (warm) seawater with no
! frazil ice being present. The three outputs can be set and the rest of
! this code does not need to be performed.
!--------------------------------------------------------------------------
if (func0 .ge. 0.0_r8) then
    sa_final = sa_bulk
    ct_final = gsw_ct_from_enthalpy_exact(sa_bulk,h_bulk,p)
    w_ih_final = 0.0_r8
    return
endif

!--------------------------------------------------------------------------
! Begin to find the solution for those data points that have func0 < 0,
! implying that the output will be a positive ice mass fraction w_Ih_final.
!
! Do a quasi-Newton step with a separate polynomial estimate of the
! derivative of func with respect to the ice mass fraction.  This section
! of the code delivers initial values of both w_Ih and SA to the rest
! of the more formal modified Newtons Method approach of McDougall and
! Wotherspoon (2014).
!--------------------------------------------------------------------------
dfunc_dw_ih_mean_poly = 3.347814e+05_r8 &
                        - num_f*func0*(1.0_r8 + num_f2*func0) - num_p*p
w_ih = min(-func0/dfunc_dw_ih_mean_poly, 0.95_r8)
sa = sa_bulk/(1.0_r8 - w_ih)
if (sa .lt. 0.0_r8 .or. sa .gt. 120.0_r8) then
    sa_final = gsw_error_code(1,func_name)
    ct_final = sa_final
    w_ih_final = sa_final
    return
endif
    
!--------------------------------------------------------------------------
! Calculating the estimate of the derivative of func, dfunc_dw_Ih, to be
! fed into the iterative Newton's Method.
!--------------------------------------------------------------------------
ctf = gsw_ct_freezing_exact(sa,p,saturation_fraction)
hf = gsw_enthalpy_ct_exact(sa,ctf,p)
tf = gsw_t_freezing_exact(sa,p,saturation_fraction)
h_ihf = gsw_enthalpy_ice(tf,p)
cp_ih = gsw_cp_ice(tf,p)
call gsw_enthalpy_first_derivatives_ct_exact(sa,ctf,p,h_hat_sa,h_hat_ct)
call gsw_ct_freezing_first_derivatives(sa,p,saturation_fraction,ctf_sa)
call gsw_t_freezing_first_derivatives(sa,p,saturation_fraction,tf_sa)
    
dfunc_dw_ih = hf - h_ihf &
        - sa*(h_hat_sa + h_hat_ct*ctf_sa + w_ih*cp_ih*tf_sa/(1.0_r8 - w_ih))

!--------------------------------------------------------------------------
! Enter the main McDougall-Wotherspoon (2014) modified Newton-Raphson loop
!--------------------------------------------------------------------------
do number_of_iterations = 1, 3

    if (number_of_iterations .gt. 1) then
        ! on the first iteration these values are already known
        ctf = gsw_ct_freezing_exact(sa,p,saturation_fraction)
        hf = gsw_enthalpy_ct_exact(sa,ctf,p)
        tf = gsw_t_freezing_exact(sa,p,saturation_fraction)
        h_ihf = gsw_enthalpy_ice(tf,p)
    end if
    
    func = h_bulk - (1.0_r8 - w_ih)*hf - w_ih*h_ihf
    
    w_ih_old = w_ih
    w_ih = w_ih_old - func/dfunc_dw_ih
    w_ih_mean = 0.5_r8*(w_ih + w_ih_old)

    if (w_ih_mean .gt. 0.9_r8) then
        ! This ensures that the mass fraction of ice never exceeds 0.9
        sa_final = gsw_error_code(1+number_of_iterations,func_name)
        ct_final = sa_final
        w_ih_final = sa_final
        return
    endif

    sa = sa_bulk/(1.0_r8 - w_ih_mean)
    ctf = gsw_ct_freezing_exact(sa,p,saturation_fraction)
    hf = gsw_enthalpy_ct_exact(sa,ctf,p)
    tf = gsw_t_freezing_exact(sa,p,saturation_fraction)
    h_ihf = gsw_enthalpy_ice(tf,p)
    cp_ih = gsw_cp_ice(tf,p)
    call gsw_enthalpy_first_derivatives_ct_exact(sa,ctf,p,h_hat_sa,h_hat_ct)
    call gsw_ct_freezing_first_derivatives(sa,p,saturation_fraction,ctf_sa)
    call gsw_t_freezing_first_derivatives(sa,p,saturation_fraction,tf_sa)

    dfunc_dw_ih = hf - h_ihf - sa*(h_hat_sa + h_hat_ct*ctf_sa &
                                   + w_ih_mean*cp_ih*tf_sa/(1.0_r8 - w_ih_mean))

    w_ih = w_ih_old - func/dfunc_dw_ih

    if (w_ih .gt. 0.9_r8) then
        ! This ensures that the mass fraction of ice never exceeds 0.9
        sa_final = gsw_error_code(4+number_of_iterations,func_name)
        ct_final = sa_final
        w_ih_final = sa_final
        return
    endif

    sa = sa_bulk/(1.0_r8 - w_ih)
end do
    
sa_final = sa
ct_final = gsw_ct_freezing_exact(sa,p,saturation_fraction)
w_ih_final = w_ih
    
if (w_ih_final .lt. 0.0_r8) then
    ! This will only trap cases that are smaller than zero by just
    ! machine precision
    sa_final = sa_bulk
    ct_final = gsw_ct_from_enthalpy_exact(sa_final,h_bulk,p)
    w_ih_final = 0.0_r8
end if
    
return
end subroutine

!--------------------------------------------------------------------------
