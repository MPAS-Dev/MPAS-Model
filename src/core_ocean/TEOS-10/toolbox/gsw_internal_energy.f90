!==========================================================================
elemental function gsw_internal_energy (sa, ct, p)  
!==========================================================================
!
!  Calculates specific internal energy of seawater.
!
!  sa     : Absolute Salinity                               [g/kg]
!  ct     : Conservative Temperature                       [deg C]
!  p      : sea pressure                                    [dbar]
! 
!  gsw_internal_energy  :  internal_energy of seawater    [ J/kg ]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_enthalpy, gsw_specvol

use gsw_mod_teos10_constants, only : gsw_p0, db2pa

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct, p  

real (r8) :: gsw_internal_energy

gsw_internal_energy = gsw_enthalpy(sa,ct,p) - &
                                (gsw_p0 + db2pa*p)*gsw_specvol(sa,ct,p)
return
end function

!--------------------------------------------------------------------------


