!==========================================================================
elemental subroutine gsw_entropy_first_derivatives (sa, ct, eta_sa, eta_ct)
! =========================================================================
!
!  Calculates the following two partial derivatives of specific entropy
!  (eta) 
!   (1) eta_SA, the derivative with respect to Absolute Salinity at 
!       constant Conservative Temperature, and
!   (2) eta_CT, the derivative with respect to Conservative Temperature at 
!       constant Absolute Salinity. 
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  CT  =  Conservative Temperature (ITS-90)                       [ deg C ]
!
!  eta_SA =  The derivative of specific entropy with respect to 
!            Absolute Salinity (in units of g kg^-1) at constant  
!            Conservative Temperature.  
!            eta_SA has units of:         [ J/(kg K(g/kg))]  or [ J/(g K) ]
!  eta_CT =  The derivative of specific entropy with respect to 
!            Conservative Temperature at constant Absolute Salinity.
!            eta_CT has units of:                            [ J/(kg K^2) ]
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : gsw_cp0, gsw_t0

use gsw_mod_toolbox, only : gsw_gibbs, gsw_pt_from_ct

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct
real (r8), intent(out), optional :: eta_sa, eta_ct

real (r8) :: pt

integer, parameter :: n0=0, n1=1
real (r8), parameter :: pr0 = 0.0_r8

pt = gsw_pt_from_ct(sa,ct)

if (present(eta_sa)) eta_sa = -(gsw_gibbs(n1,n0,n0,sa,pt,pr0))/(gsw_t0 + pt)

if (present(eta_ct)) eta_ct = gsw_cp0/(gsw_t0 + pt)

return
end subroutine

!--------------------------------------------------------------------------
