! =========================================================================
elemental function gsw_gibbs_ice_part_t (t, p)
! =========================================================================
!
!  part of the the first temperature derivative of Gibbs energy of ice
!  that is the outout is gibbs_ice(1,0,t,p) + S0
!
!  t   =  in-situ temperature (ITS-90)                            [ deg C ]
!  p   =  sea pressure                                             [ dbar ]
!
!  gibbs_ice_part_t = part of temperature derivative       [ J kg^-1 K^-1 ]
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : gsw_t0, db2pa

use gsw_mod_gibbs_ice_coefficients

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: t, p

real (r8) :: gsw_gibbs_ice_part_t

real (r8) :: dzi, tau
complex (r8) :: g, tau_t1, tau_t2, r2

tau = (t + gsw_t0)*rec_t3p

dzi = db2pa*p*rec_pt

tau_t1 = tau/t1
tau_t2 = tau/t2

r2 = r20 + dzi*(r21 + r22*dzi)

g = r1*(log((1.0_r8 + tau_t1)/(1.0_r8 - tau_t1)) - 2.0_r8*tau_t1) &
    + r2*(log((1.0_r8 + tau_t2)/(1.0_r8 - tau_t2)) - 2.0_r8*tau_t2)

gsw_gibbs_ice_part_t = real(g,r8)

return
end function

!--------------------------------------------------------------------------
