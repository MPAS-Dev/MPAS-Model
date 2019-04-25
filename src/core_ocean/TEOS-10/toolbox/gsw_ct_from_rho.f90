!==========================================================================
elemental subroutine gsw_ct_from_rho (rho, sa, p, ct, ct_multiple)
! =========================================================================
!
!  Calculates the Conservative Temperature of a seawater sample, for given
!  values of its density, Absolute Salinity and sea pressure (in dbar).
!
!  rho  =  density of a seawater sample (e.g. 1026 kg/m^3)       [ kg/m^3 ]
!   Note. This input has not had 1000 kg/m^3 subtracted from it.
!     That is, it is 'density', not 'density anomaly'.
!  SA   =  Absolute Salinity                                       [ g/kg ]
!  p    =  sea pressure                                            [ dbar ]
!          ( i.e. absolute pressure - 10.1325 dbar )
!
!  CT  =  Conservative Temperature  (ITS-90)                      [ deg C ]
!  CT_multiple  =  Conservative Temperature  (ITS-90)             [ deg C ]
!    Note that at low salinities, in brackish water, there are two possible
!      Conservative Temperatures for a single density.  This programme will
!      output both valid solutions.  To see this second solution the user 
!      must call the programme with two outputs (i.e. [CT,CT_multiple]), if
!      there is only one possible solution and the programme has been 
!      called with two outputs the second variable will be set to NaN.
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_ct_freezing_poly, gsw_ct_maxdensity, gsw_rho
use gsw_mod_toolbox, only : gsw_rho_alpha_beta, gsw_specvol

use gsw_mod_error_functions, only : gsw_error_code, gsw_error_limit

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: rho, sa, p
real (r8), intent(out) :: ct
real (r8), intent(out), optional :: ct_multiple

integer :: number_of_iterations
real (r8) :: a, alpha_freezing, alpha_mean, b, c, ct_a, ct_b, ct_diff
real (r8) :: ct_freezing, ct_max_rho, ct_mean, ct_old
real (r8) :: delta_ct, delta_v, factor, factorqa, factorqb
real (r8) :: rho_40, rho_extreme, rho_freezing, rho_max, rho_mean
real (r8) :: rho_old, sqrt_disc, top, v_ct, v_lab

character (*), parameter :: func_name = "gsw_ct_from_rho"

! alpha_limit is the positive value of the thermal expansion coefficient
! which is used at the freezing temperature to distinguish between
! salty and fresh water.
real (r8), parameter :: alpha_limit = 1e-5_r8

! rec_half_rho_TT is a constant representing the reciprocal of half the
! second derivative of density with respect to temperature near the
! temperature of maximum density.
real (r8), parameter :: rec_half_rho_tt = -110.0_r8

rho_40 = gsw_rho(sa,40.0_r8,p)
if (rho .lt. rho_40) then
    ct = gsw_error_code(1,func_name)
    if (present(ct_multiple)) ct_multiple = ct
    return
end if

ct_max_rho = gsw_ct_maxdensity(sa,p)
rho_max = gsw_rho(sa,ct_max_rho,p)
rho_extreme = rho_max

! Assumes that the seawater is always unsaturated with air
ct_freezing = gsw_ct_freezing_poly(sa,p,0.0_r8)

call gsw_rho_alpha_beta(sa,ct_freezing,p,rho=rho_freezing,alpha=alpha_freezing)

! reset the extreme values
if (ct_freezing .gt. ct_max_rho) rho_extreme = rho_freezing

if (rho .gt. rho_extreme) then
    ct = gsw_error_code(2,func_name)
    if (present(ct_multiple)) ct_multiple = ct
    return
end if

if (alpha_freezing .gt. alpha_limit) then

    ct_diff = 40.0_r8 - ct_freezing
    top = rho_40 - rho_freezing + rho_freezing*alpha_freezing*ct_diff
    a = top/(ct_diff*ct_diff)
    b = -rho_freezing*alpha_freezing
    c = rho_freezing - rho
    sqrt_disc = sqrt(b*b - 4*a*c)
    ct = ct_freezing + 0.5_r8*(-b - sqrt_disc)/a

else

    ct_diff = 40.0_r8 - ct_max_rho
    factor = (rho_max - rho)/(rho_max - rho_40)
    delta_ct = ct_diff*sqrt(factor)
    
    if (delta_ct .gt. 5.0_r8) then

        ct = ct_max_rho + delta_ct

    else
     
        ! Set the initial value of the quadratic solution roots.
        ct_a = ct_max_rho + sqrt(rec_half_rho_tt*(rho - rho_max))       
        do number_of_iterations = 1, 7
            ct_old = ct_a
            rho_old = gsw_rho(sa,ct_old,p)
            factorqa = (rho_max - rho)/(rho_max - rho_old)
            ct_a = ct_max_rho + (ct_old - ct_max_rho)*sqrt(factorqa)
        end do

        if (ct_freezing - ct_a .lt. 0.0_r8) then
            ct = gsw_error_code(3,func_name)
            if (present(ct_multiple)) ct_multiple = ct
            return
        end if

        ct = ct_a
        if (.not. present(ct_multiple)) return

        ! Set the initial value of the quadratic solution roots.
        ct_b = ct_max_rho - sqrt(rec_half_rho_tt*(rho - rho_max))    
        do number_of_iterations = 1, 7
            ct_old = ct_b
            rho_old = gsw_rho(sa,ct_old,p)
            factorqb = (rho_max - rho)/(rho_max - rho_old)
            ct_b = ct_max_rho + (ct_old - ct_max_rho)*sqrt(factorqb)
        end do

        ! After seven iterations of this quadratic iterative procedure,
        ! the error in rho is no larger than 4.6x10^-13 kg/m^3.

        if (ct_freezing - ct_b .lt. 0.0_r8) then
            ct = gsw_error_code(4,func_name)
            if (present(ct_multiple)) ct_multiple = ct
            return
        end if

        ct_multiple = ct_b
        return

    end if

end if

! Begin the modified Newton-Raphson iterative method

v_lab = 1.0_r8/rho
call gsw_rho_alpha_beta(sa,ct,p,rho=rho_mean,alpha=alpha_mean)
v_ct = alpha_mean/rho_mean

do number_of_iterations = 1, 3
    ct_old = ct
    delta_v = gsw_specvol(sa,ct_old,p) - v_lab
    ct = ct_old - delta_v/v_ct
    ct_mean = 0.5_r8*(ct + ct_old)
    call gsw_rho_alpha_beta(sa,ct_mean,p,rho=rho_mean,alpha=alpha_mean)
    v_ct = alpha_mean/rho_mean
    ct = ct_old - delta_v/v_ct 
end do

! After three iterations of this modified Newton-Raphson iteration,
! the error in rho is no larger than 1.6x10^-12 kg/m^3.

if (present(ct_multiple)) ct_multiple = gsw_error_code(5,func_name)

return
end subroutine

!--------------------------------------------------------------------------
