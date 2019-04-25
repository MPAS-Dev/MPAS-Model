!==========================================================================
elemental function gsw_specvol_anom_standard (sa, ct, p)  
!==========================================================================
!
!  Calculates specific volume anomaly of seawater.
!
!  sa     : Absolute Salinity                               [g/kg]
!  ct     : Conservative Temperature                        [deg C]
!  p      : sea pressure                                    [dbar]
! 
!  specvol_anom  :  specific volume anomaly of seawater
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_specvol, gsw_specvol_sso_0

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct, p  

real (r8) :: gsw_specvol_anom_standard

gsw_specvol_anom_standard = gsw_specvol(sa,ct,p) - gsw_specvol_sso_0(p)

return
end function

!--------------------------------------------------------------------------
