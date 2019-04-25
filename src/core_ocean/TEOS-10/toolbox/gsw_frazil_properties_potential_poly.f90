!==========================================================================
elemental subroutine gsw_frazil_properties_potential_poly (sa_bulk, &
                             h_pot_bulk, p, sa_final, ct_final, w_ih_final)
!==========================================================================
!
!  Calculates the mass fraction of ice (mass of ice divided by mass of ice
!  plus seawater), w_Ih_final, which results from given values of the bulk
!  Absolute Salinity, SA_bulk, bulk potential enthalpy, h_pot_bulk,
!  occuring at pressure p.  The final equilibrium values of Absolute
!  Salinity, SA_final, and Conservative Temperature, CT_final, of the
!  interstitial seawater phase are also returned.  This code assumes that
!  there is no dissolved air in the seawater (that is, saturation_fraction
!  is assumed to be zero thoughout the code).
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
!  Note that this code uses the polynomial forms of CT_freezing and
!  pot_enthalpy_ice_freezing. This code is intended to be used in ocean
!  models where the model prognostic variables are SA_bulk and h_pot_bulk.
!
!  SA_bulk     =  bulk Absolute Salinity of the seawater and ice mixture
!                                                                  [ g/kg ]
!  h_pot_bulk  =  bulk potential enthalpy of the seawater and ice mixture
!                                                                  [ J/kg ]
!  p           =  sea pressure                                  [ dbar ]
!                  ( i.e. absolute pressure - 10.1325 dbar )
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

use gsw_mod_teos10_constants, only : gsw_cp0

use gsw_mod_toolbox, only : gsw_ct_freezing_poly
use gsw_mod_toolbox, only : gsw_ct_freezing_first_derivatives_poly
use gsw_mod_toolbox, only : gsw_pot_enthalpy_ice_freezing_poly
use gsw_mod_toolbox, only : gsw_pot_enthalpy_ice_freezing_first_derivatives_poly

use gsw_mod_error_functions, only : gsw_error_code, gsw_error_limit

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa_bulk, h_pot_bulk, p
real (r8), intent(out) :: sa_final, ct_final, w_ih_final

integer :: iterations, max_iterations

real (r8) :: ctf_sa, ctf, dfunc_dw_ih, dfunc_dw_ih_mean_poly, dpot_h_ihf_dsa
real (r8) :: func, func0, h_pot_ihf, sa, w_ih_old, w_ih, x, xa, y, z

real (r8), parameter :: f01 = -9.041191886754806e-1_r8
real (r8), parameter :: f02 =  4.169608567309818e-2_r8
real (r8), parameter :: f03 = -9.325971761333677e-3_r8
real (r8), parameter :: f04 =  4.699055851002199e-2_r8
real (r8), parameter :: f05 = -3.086923404061666e-2_r8
real (r8), parameter :: f06 =  1.057761186019000e-2_r8
real (r8), parameter :: f07 = -7.349302346007727e-2_r8
real (r8), parameter :: f08 =  1.444842576424337e-1_r8
real (r8), parameter :: f09 = -1.408425967872030e-1_r8
real (r8), parameter :: f10 =  1.070981398567760e-1_r8
real (r8), parameter :: f11 = -1.768451760854797e-2_r8
real (r8), parameter :: f12 = -4.013688314067293e-1_r8
real (r8), parameter :: f13 =  7.209753205388577e-1_r8
real (r8), parameter :: f14 = -1.807444462285120e-1_r8
real (r8), parameter :: f15 =  1.362305015808993e-1_r8
real (r8), parameter :: f16 = -9.500974920072897e-1_r8
real (r8), parameter :: f17 =  1.192134856624248_r8
real (r8), parameter :: f18 = -9.191161283559850e-2_r8
real (r8), parameter :: f19 = -1.008594411490973_r8
real (r8), parameter :: f20 =  8.020279271484482e-1_r8
real (r8), parameter :: f21 = -3.930534388853466e-1_r8
real (r8), parameter :: f22 = -2.026853316399942e-2_r8
real (r8), parameter :: f23 = -2.722731069001690e-2_r8
real (r8), parameter :: f24 =  5.032098120548072e-2_r8
real (r8), parameter :: f25 = -2.354888890484222e-2_r8
real (r8), parameter :: f26 = -2.454090179215001e-2_r8
real (r8), parameter :: f27 =  4.125987229048937e-2_r8
real (r8), parameter :: f28 = -3.533404753585094e-2_r8
real (r8), parameter :: f29 =  3.766063025852511e-2_r8
real (r8), parameter :: f30 = -3.358409746243470e-2_r8
real (r8), parameter :: f31 = -2.242158862056258e-2_r8
real (r8), parameter :: f32 =  2.102254738058931e-2_r8
real (r8), parameter :: f33 = -3.048635435546108e-2_r8
real (r8), parameter :: f34 = -1.996293091714222e-2_r8
real (r8), parameter :: f35 =  2.577703068234217e-2_r8
real (r8), parameter :: f36 = -1.292053030649309e-2_r8
    
real (r8), parameter :: g01 =  3.332286683867741e5_r8
real (r8), parameter :: g02 =  1.416532517833479e4_r8
real (r8), parameter :: g03 = -1.021129089258645e4_r8
real (r8), parameter :: g04 =  2.356370992641009e4_r8
real (r8), parameter :: g05 = -8.483432350173174e3_r8
real (r8), parameter :: g06 =  2.279927781684362e4_r8
real (r8), parameter :: g07 =  1.506238790315354e4_r8
real (r8), parameter :: g08 =  4.194030718568807e3_r8
real (r8), parameter :: g09 = -3.146939594885272e5_r8
real (r8), parameter :: g10 = -7.549939721380912e4_r8
real (r8), parameter :: g11 =  2.790535212869292e6_r8
real (r8), parameter :: g12 =  1.078851928118102e5_r8
real (r8), parameter :: g13 = -1.062493860205067e7_r8
real (r8), parameter :: g14 =  2.082909703458225e7_r8
real (r8), parameter :: g15 = -2.046810820868635e7_r8
real (r8), parameter :: g16 =  8.039606992745191e6_r8
real (r8), parameter :: g17 = -2.023984705844567e4_r8
real (r8), parameter :: g18 =  2.871769638352535e4_r8
real (r8), parameter :: g19 = -1.444841553038544e4_r8
real (r8), parameter :: g20 =  2.261532522236573e4_r8
real (r8), parameter :: g21 = -2.090579366221046e4_r8
real (r8), parameter :: g22 = -1.128417003723530e4_r8
real (r8), parameter :: g23 =  3.222965226084112e3_r8
real (r8), parameter :: g24 = -1.226388046175992e4_r8
real (r8), parameter :: g25 =  1.506847628109789e4_r8
real (r8), parameter :: g26 = -4.584670946447444e4_r8
real (r8), parameter :: g27 =  1.596119496322347e4_r8
real (r8), parameter :: g28 = -6.338852410446789e4_r8
real (r8), parameter :: g29 =  8.951570926106525e4_r8
    
real (r8), parameter :: saturation_fraction = 0.0_r8

character (*), parameter :: func_name = "gsw_frazil_properties_potential"

!--------------------------------------------------------------------------
! Finding func0.  This is the value of the function, func, that would
! result in the output w_Ih_final being exactly zero.
!--------------------------------------------------------------------------
func0 = h_pot_bulk - &
               gsw_cp0*gsw_ct_freezing_poly(sa_bulk,p,saturation_fraction)

!--------------------------------------------------------------------------
! Setting the three outputs for data points that have func0 non-negative
!--------------------------------------------------------------------------
if (func0 >= 0.0_r8) then
    ! When func0 is zero or positive then the final answer will contain
    ! no frazil ice; that is, it will be pure seawater that is warmer
    ! han the freezing temperature.  If func0 >= 0 we do not need to go
    ! through the modified Newton-Raphson procedure and we can simply
    ! write down the answer, as in the following 4 lines of code.
    sa_final = sa_bulk
    ct_final = h_pot_bulk/gsw_cp0
    w_ih_final = 0.0_r8
    return
end if

!--------------------------------------------------------------------------
! Begin finding the solution for data points that have func0 < 0, so that
! the output will have a positive ice mass fraction w_Ih_final.
!--------------------------------------------------------------------------
   
! Evalaute a polynomial for w_Ih in terms of SA_bulk, func0 and p
x = sa_bulk*1e-2_r8  
y = func0/3e5_r8
z = p*1e-4_r8

w_ih = y*(f01 + x*(f02 + x*(f03 + x*(f04 + x*(f05 + f06*x)))) &
     + y*(f07 + x*(f08 + x*(f09 + x*(f10 + f11*x))) + y*(f12 + x*(f13 &
     + x*(f14 + f15*x)) + y*(f16 + x*(f17 + f18*x) + y*(f19 + f20*x &
     + f21*y)))) + z*(f22 + x*(f23 + x*(f24 + f25*x)) + y*(x*(f26 + f27*x) &
     + y*(f28 + f29*x + f30*y)) + z*(f31 + x*(f32 + f33*x) + y*(f34 &
     + f35*x + f36*y))))

if (w_ih .gt. 0.9_r8) then
    ! The ice mass fraction out of this code is restricted to be less than 0.9.
    sa_final = gsw_error_code(1,func_name)
    ct_final = sa_final
    w_ih_final = sa_final
    return
end if

! The initial guess at the absolute salinity of the interstitial seawater
sa = sa_bulk/(1.0_r8 - w_ih)

!--------------------------------------------------------------------------
! Doing a Newton step with a separate polynomial estimate of the mean
! derivative dfunc_dw_Ih_mean_poly.
!--------------------------------------------------------------------------
ctf = gsw_ct_freezing_poly(sa,p,saturation_fraction)
h_pot_ihf = gsw_pot_enthalpy_ice_freezing_poly(sa,p)
func = h_pot_bulk - (1.0_r8 - w_ih)*gsw_cp0*ctf - w_ih*h_pot_ihf

xa = sa*1e-2_r8

dfunc_dw_ih_mean_poly = g01 + xa*(g02 + xa*(g03 + xa*(g04 + g05*xa))) &
    + w_ih*(xa*(g06 + xa*(g07 + g08*xa)) + w_ih*(xa*(g09 + g10*xa) &
    + w_ih*xa*(g11 + g12*xa + w_ih*(g13 + w_ih*(g14 + w_ih*(g15 &
    + g16*w_ih)))))) + z*(g17 + xa*(g18 + g19*xa) + w_ih*(g20 &
    + w_ih*(g21 + g22*w_ih) + xa*(g23 + g24*xa*w_ih)) &
    + z*(g25 + xa*(g26 + g27*xa) + w_ih*(g28 + g29*w_ih)))

w_ih_old = w_ih
w_ih = w_ih_old - func/dfunc_dw_ih_mean_poly

sa = sa_bulk/(1.0_r8 - w_ih)

!--------------------------------------------------------------------------
! Calculating the estimate of the derivative of func, dfunc_dw_Ih, to be
! fed into Newton's Method.
!--------------------------------------------------------------------------
ctf = gsw_ct_freezing_poly(sa,p,saturation_fraction)

h_pot_ihf = gsw_pot_enthalpy_ice_freezing_poly(sa,p)

call gsw_ct_freezing_first_derivatives_poly(sa,p,saturation_fraction,ctf_sa)
call gsw_pot_enthalpy_ice_freezing_first_derivatives_poly(sa,p,dpot_h_ihf_dsa)

dfunc_dw_ih = gsw_cp0*ctf - h_pot_ihf - &
                 sa*(gsw_cp0*ctf_sa + w_ih*dpot_h_ihf_dsa/(1.0_r8 - w_ih))
    
if (w_ih .ge. 0.0_r8 .and. w_ih .le. 0.20_r8 .and. sa .gt. 15.0_r8 &
    .and. sa .lt. 60.0_r8 .and. p .le. 3000.0_r8) then
    max_iterations = 1
else if (w_ih .ge. 0.0_r8 .and. w_ih .le. 0.85_r8 .and. sa .gt. 0.0_r8 &
    .and. sa .lt. 120.0_r8 .and. p .le. 3500.0_r8) then
    max_iterations = 2
else
    max_iterations = 3
end if
    
do iterations = 1, max_iterations

    if (iterations .gt. 1) then
        ! On the first iteration ctf and h_pot_ihf are both known
        ctf = gsw_ct_freezing_poly(sa,p,saturation_fraction)
        h_pot_ihf = gsw_pot_enthalpy_ice_freezing_poly(sa,p)
    end if

    ! This is the function, func, whose zero we seek ...
    func = h_pot_bulk - (1.0_r8 - w_ih)*gsw_cp0*ctf - w_ih*h_pot_ihf

    w_ih_old = w_ih
    w_ih = w_ih_old - func/dfunc_dw_ih

    if (w_ih .gt. 0.9_r8) then
        sa_final = gsw_error_code(1+iterations,func_name)
        ct_final = sa_final
        w_ih_final = sa_final
        return
    end if

    sa = sa_bulk/(1.0_r8 - w_ih)

end do

if (w_ih .lt. 0.0_r8) then
    sa_final = sa_bulk
    ct_final = h_pot_bulk/gsw_cp0
    w_ih_final = 0.0_r8
else
    sa_final = sa
    ct_final = gsw_ct_freezing_poly(sa,p,saturation_fraction)
    w_ih_final = w_ih
end if
    
return
end subroutine

!--------------------------------------------------------------------------
