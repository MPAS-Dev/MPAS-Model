!==========================================================================
elemental function gsw_pot_enthalpy_from_pt_ice (pt0_ice)
!==========================================================================
!
!  Calculates the potential enthalpy of ice from potential temperature of
!  ice (whose reference sea pressure is zero dbar).  
!
!  pt0_ice  =  potential temperature of ice (ITS-90)              [ deg C ]
!
!  gsw_pot_enthalpy_ice  =  potential enthalpy of ice              [ J/kg ]
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : gsw_t0

use gsw_mod_gibbs_ice_coefficients

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: pt0_ice

real (r8) :: gsw_pot_enthalpy_from_pt_ice

real (r8) :: tau
complex (r8) :: h0_part, sqtau_t1, sqtau_t2

tau = (pt0_ice + gsw_t0)*rec_t3p

sqtau_t1 = (tau/t1)**2
sqtau_t2 = (tau/t2)**2

h0_part = r1*t1*(log(1.0_r8 - sqtau_t1) + sqtau_t1) &
          + r20*t2*(log(1.0_r8 - sqtau_t2) + sqtau_t2)

gsw_pot_enthalpy_from_pt_ice = g00 + t3p*real(h0_part,r8) 
              
return
end function

!--------------------------------------------------------------------------
