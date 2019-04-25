! =========================================================================
elemental function gsw_gibbs_ice_pt0_pt0 (pt0)
! =========================================================================
!
!  The second temperature derivative of Gibbs energy of ice at the 
!  potential temperature with reference sea pressure of zero dbar.  That is
!  the output is gibbs_ice(2,0,pt0,0). 
!
!  pt0  =  potential temperature with reference sea pressure of zero dbar
!                                                                 [ deg C ]
!
!  gsw_gibbs_ice_pt0_pt0 = temperature second derivative at pt0
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : gsw_t0

use gsw_mod_gibbs_ice_coefficients

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: pt0

real (r8) :: gsw_gibbs_ice_pt0_pt0

real (r8) :: tau
complex (r8) :: g

tau = (pt0 + gsw_t0)*rec_t3p

g = r1*(1.0_r8/(t1 - tau) + 1.0_r8/(t1 + tau) - 2.0_r8/t1) &
    + r20*(1.0_r8/(t2 - tau) + 1.0_r8/(t2 + tau) - 2.0_r8/t2)

gsw_gibbs_ice_pt0_pt0 = rec_t3p*real(g,r8)

return
end function

!--------------------------------------------------------------------------
