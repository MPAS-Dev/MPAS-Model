!==========================================================================
elemental subroutine gsw_entropy_second_derivatives (sa, ct, eta_sa_sa, &
                                                     eta_sa_ct, eta_ct_ct)
! =========================================================================
!
!  Calculates the following three second-order partial derivatives of 
!  specific entropy (eta) 
!   (1) eta_SA_SA, the second derivative with respect to Absolute  
!       Salinity at constant Conservative Temperature, and
!   (2) eta_SA_CT, the derivative with respect to Absolute Salinity and 
!       Conservative Temperature. 
!   (3) eta_CT_CT, the second derivative with respect to Conservative 
!       Temperature at constant Absolute Salinity. 
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  CT  =  Conservative Temperature (ITS-90)                       [ deg C ]
!
!  eta_SA_SA =  The second derivative of specific entropy with respect 
!               to Absolute Salinity (in units of g kg^-1) at constant 
!               Conservative Temperature.    
!               eta_SA_SA has units of:                 [ J/(kg K(g/kg)^2)]
!  eta_SA_CT =  The second derivative of specific entropy with respect 
!               to Conservative Temperature at constant Absolute 
!               Salinity. eta_SA_CT has units of:     [ J/(kg (g/kg) K^2) ]
!  eta_CT_CT =  The second derivative of specific entropy with respect 
!               to Conservative Temperature at constant Absolute
!               Salinity.  eta_CT_CT has units of:           [ J/(kg K^3) ]
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : gsw_cp0, gsw_t0

use gsw_mod_toolbox, only : gsw_gibbs, gsw_pt_from_ct

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct
real (r8), intent(out), optional :: eta_sa_sa, eta_sa_ct, eta_ct_ct

real (r8) :: abs_pt, ct_pt, ct_sa, pt, ct_ct

integer, parameter :: n0=0, n1=1, n2=2
real (r8), parameter :: pr0 = 0.0_r8

pt = gsw_pt_from_ct(sa,ct)
abs_pt = gsw_t0 + pt

ct_pt = -(abs_pt*gsw_gibbs(n0,n2,n0,sa,pt,pr0))/gsw_cp0

ct_ct = -gsw_cp0/(ct_pt*abs_pt*abs_pt)

if (present(eta_sa_ct) .or. present(eta_sa_sa)) then

    ct_sa = (gsw_gibbs(n1,n0,n0,sa,pt,pr0) - &
               (abs_pt*gsw_gibbs(n1,n1,n0,sa,pt,pr0)))/gsw_cp0

    if (present(eta_sa_ct)) eta_sa_ct = -ct_sa*ct_ct

    if (present(eta_sa_sa)) eta_sa_sa = -gsw_gibbs(n2,n0,n0,sa,pt,pr0)/abs_pt +&
                                     ct_sa*ct_sa*ct_ct
end if

if (present(eta_ct_ct)) eta_ct_ct = ct_ct

return
end subroutine

!--------------------------------------------------------------------------
