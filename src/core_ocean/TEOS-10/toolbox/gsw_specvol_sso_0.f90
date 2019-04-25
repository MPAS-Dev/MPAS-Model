!==========================================================================
elemental function gsw_specvol_sso_0 (p)
!==========================================================================
!  This function calculates specifc volume at the Standard Ocean Salinity,
!  SSO, and at a Conservative Temperature of zero degrees C, as a function 
!  of pressure, p, in dbar, using a streamlined version of the CT version
!  of specific volume, that is, a streamlined version of the code
!  "gsw_specvol(SA,CT,p)".
!==========================================================================

use gsw_mod_specvol_coefficients

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: p

real (r8) :: gsw_specvol_sso_0

real (r8) :: z
 
z = p*1e-4_r8

gsw_specvol_sso_0 = 9.726613854843870e-04_r8 + z*(-4.505913211160929e-05_r8 &
    + z*(7.130728965927127e-06_r8 + z*(-6.657179479768312e-07_r8 &
    + z*(-2.994054447232880e-08_r8 + z*(v005 + v006*z)))))

return
end function

!--------------------------------------------------------------------------
