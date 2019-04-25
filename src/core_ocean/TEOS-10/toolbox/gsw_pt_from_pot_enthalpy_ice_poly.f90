!==========================================================================
elemental function gsw_pt_from_pot_enthalpy_ice_poly (pot_enthalpy_ice)
!==========================================================================
!
!  Calculates the potential temperature of ice (whose reference sea 
!  pressure is zero dbar) from the potential enthalpy of ice.  This is a
!  compuationally efficient polynomial fit to the potential enthalpy of
!  ice.
!
!  pot_enthalpy_ice  =  potential enthalpy of ice                  [ J/kg ]
!
!  pt0_ice  =  potential temperature of ice (ITS-90)              [ deg C ]
!--------------------------------------------------------------------------

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: pot_enthalpy_ice

real (r8) :: gsw_pt_from_pot_enthalpy_ice_poly

real (r8), parameter :: q0 = 2.533588268773218e2_r8
real (r8), parameter :: q1 = 2.594351081876611e-3_r8
real (r8), parameter :: q2 = 1.765077810213815e-8_r8
real (r8), parameter :: q3 = 7.768070564290540e-14_r8
real (r8), parameter :: q4 = 2.034842254277530e-19_r8
real (r8), parameter :: q5 = 3.220014531712841e-25_r8
real (r8), parameter :: q6 = 2.845172809636068e-31_r8
real (r8), parameter :: q7 = 1.094005878892950e-37_r8
    
! The error of this fit ranges between -5e-5 and 2e-4 deg C over the potential 
! temperature range of -100 to 2 deg C, or the potential enthalpy range of 
! -5.7 x 10^5 to -3.3 x 10^5 J/kg. 
      
gsw_pt_from_pot_enthalpy_ice_poly = q0 &
         + pot_enthalpy_ice*(q1 + pot_enthalpy_ice*(q2 + pot_enthalpy_ice*(q3 &
         + pot_enthalpy_ice*(q4 + pot_enthalpy_ice*(q5 + pot_enthalpy_ice*(q6 &
         + pot_enthalpy_ice*q7))))))

return
end function

!--------------------------------------------------------------------------
