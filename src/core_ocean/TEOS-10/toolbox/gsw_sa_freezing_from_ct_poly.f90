!==========================================================================
elemental function gsw_sa_freezing_from_ct_poly (ct, p, saturation_fraction)
!==========================================================================
!
!  Calculates the Absolute Salinity of seawater at the freezing temperature.  
!  That is, the output is the Absolute Salinity of seawater, with the 
!  fraction saturation_fraction of dissolved air, that is in equilibrium 
!  with ice at Conservative Temperature CT and pressure p.  If the input 
!  values are such that there is no positive value of Absolute Salinity for
!  which seawater is frozen, the output is put equal to Nan.
!
!  CT  =  Conservative Temperature (ITS-90)                       [ deg C ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar ) 
!  saturation_fraction  =  the saturation fraction of dissolved air in 
!                          seawater
!
!  sa_freezing_from_ct  =  Absolute Salinity of seawater when it freezes,
!                 for given input values of Conservative Temperature
!                 pressure and air saturation fraction.            [ g/kg ]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_ct_freezing_poly, gsw_sa_p_inrange
use gsw_mod_toolbox, only : gsw_ct_freezing_first_derivatives_poly
use gsw_mod_toolbox, only : gsw_sa_freezing_estimate

use gsw_mod_error_functions, only : gsw_error_code, gsw_error_limit

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: ct, p, saturation_fraction

real (r8) :: gsw_sa_freezing_from_ct_poly

integer :: i_iter
real (r8) :: ct_freezing, ct_freezing_zero_sa, dct_dsa
real (r8) :: sa, sa_old, sa_mean

integer, parameter :: number_of_iterations = 2

! This is the band of sa within +- 2.5 g/kg of sa = 0, which we treat
! differently in calculating the initial values of both SA and dCT_dSA. 
real (r8), parameter :: sa_cut_off = 2.5_r8

character (*), parameter :: func_name = "gsw_sa_freezing_from_ct_poly"

! Find CT > CT_freezing_zero_SA.  If this is the case, the input values
! represent seawater that is not frozen (at any positive SA). 
ct_freezing_zero_sa = gsw_ct_freezing_poly(0.0_r8,p,saturation_fraction)
if (ct .gt. ct_freezing_zero_sa) then
    gsw_sa_freezing_from_ct_poly = gsw_error_code(1,func_name)
    return
endif

! Form the first estimate of SA from a polynomial in CT and p 
sa = gsw_sa_freezing_estimate(p,saturation_fraction,ct=ct)
if (sa .lt. -sa_cut_off) then
    gsw_sa_freezing_from_ct_poly = gsw_error_code(2,func_name)
    return
endif

! Form the first estimate of dCT_dSA, the derivative of CT with respect 
! to SA at fixed p.  
sa = max(sa,0.0_r8)
call gsw_ct_freezing_first_derivatives_poly(sa,p,saturation_fraction, &
                                            ctfreezing_sa=dct_dsa)

! For -SA_cut_off < SA < SA_cut_off, replace the above estimate of SA  
! with one based on (CT_freezing_zero_SA - CT).
if (abs(sa) .lt. sa_cut_off) sa = (ct - ct_freezing_zero_sa)/dct_dsa

!--------------------------------------------------------------------------
! Begin the modified Newton-Raphson method to solve the root of 
! CT_freezing = CT for SA. 
!--------------------------------------------------------------------------
do i_iter = 1, number_of_iterations
    sa_old = sa
    ct_freezing = gsw_ct_freezing_poly(sa_old,p,saturation_fraction)
    sa = sa_old - (ct_freezing - ct)/dct_dsa
    sa_mean = 0.5_r8*(sa + sa_old)
    call gsw_ct_freezing_first_derivatives_poly(sa_mean,p,saturation_fraction, &
                                                ctfreezing_sa=dct_dsa)
    sa = sa_old - (ct_freezing - ct)/dct_dsa
enddo

if (gsw_sa_p_inrange(sa,p)) then
    gsw_sa_freezing_from_ct_poly = sa
else
    gsw_sa_freezing_from_ct_poly = gsw_error_code(3,func_name)
endif

return
end function

!--------------------------------------------------------------------------
