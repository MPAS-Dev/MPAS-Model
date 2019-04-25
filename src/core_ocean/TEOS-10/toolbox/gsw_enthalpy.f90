!==========================================================================
elemental function gsw_enthalpy (sa, ct, p)
!==========================================================================
!
!  Calculates specific enthalpy of seawater using the computationally-
!  efficient expression for specific volume in terms of SA, CT and p
!  (Roquet et al., 2014).
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  CT  =  Conservative Temperature (ITS-90)                       [ deg C ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar ) 
!
!  enthalpy  =  specific enthalpy                                  [ J/kg ]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_dynamic_enthalpy

use gsw_mod_teos10_constants, only : gsw_cp0

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct, p

real (r8) :: gsw_enthalpy

gsw_enthalpy = gsw_cp0*ct + gsw_dynamic_enthalpy(sa,ct,p)

return
end function

!--------------------------------------------------------------------------
