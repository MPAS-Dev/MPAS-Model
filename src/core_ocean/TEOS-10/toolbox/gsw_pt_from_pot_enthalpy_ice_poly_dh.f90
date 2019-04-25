!==========================================================================
elemental function gsw_pt_from_pot_enthalpy_ice_poly_dh (pot_enthalpy_ice)
!==========================================================================
!
!  Calculates the derivative of potential temperature of ice with respect 
!  to potential enthalpy.  This is based on the compuationally-efficient 
!  polynomial fit to the potential enthalpy of ice. 
!
!  pot_enthalpy_ice  =  potential enthalpy of ice                  [ J/kg ]
!
!  dpt0_ice_dh  =  derivative of potential temperature of ice 
!                  with respect to potential enthalpy             [ deg C ]
!--------------------------------------------------------------------------

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: pot_enthalpy_ice

real (r8) :: gsw_pt_from_pot_enthalpy_ice_poly_dh

real (r8), parameter :: q1 = 2.594351081876611e-3_r8
real (r8), parameter :: p2 = 3.530155620427630e-8_r8
real (r8), parameter :: p3 = 2.330421169287162e-13_r8
real (r8), parameter :: p4 = 8.139369017110120e-19_r8
real (r8), parameter :: p5 = 1.610007265856420e-24_r8
real (r8), parameter :: p6 = 1.707103685781641e-30_r8
real (r8), parameter :: p7 = 7.658041152250651e-37_r8

gsw_pt_from_pot_enthalpy_ice_poly_dh = q1 &
    + pot_enthalpy_ice*(p2 + pot_enthalpy_ice*(p3 &
    + pot_enthalpy_ice*(p4 + pot_enthalpy_ice*(p5 + pot_enthalpy_ice*(p6 &
    + pot_enthalpy_ice*p7)))))

return
end function

!--------------------------------------------------------------------------
