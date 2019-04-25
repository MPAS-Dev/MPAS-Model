!==========================================================================
elemental function gsw_enthalpy_sso_0 (p)
!==========================================================================
!  This function calculates enthalpy at the Standard Ocean Salinity, SSO, 
!  and at a Conservative Temperature of zero degrees C, as a function of
!  pressure, p, in dbar, using a streamlined version of the
!  computationally-efficient expression for specific volume, that is, a 
!  streamlined version of the code "gsw_enthalpy(SA,CT,p)".
!==========================================================================

use gsw_mod_teos10_constants, only : db2pa

use gsw_mod_specvol_coefficients

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: p

real (r8) :: gsw_enthalpy_sso_0

real (r8) :: dynamic_enthalpy_sso_0_p, z

z = p*1e-4_r8

dynamic_enthalpy_sso_0_p = &
      z*( 9.726613854843870e-4_r8 + z*(-2.252956605630465e-5_r8 &
    + z*( 2.376909655387404e-6_r8 + z*(-1.664294869986011e-7_r8 &
    + z*(-5.988108894465758e-9_r8 + z*(h006 + h007*z))))))

gsw_enthalpy_sso_0 = dynamic_enthalpy_sso_0_p*db2pa*1e4_r8

return
end function

!--------------------------------------------------------------------------
