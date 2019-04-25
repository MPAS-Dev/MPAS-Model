!==========================================================================
elemental function gsw_pot_enthalpy_ice_freezing (sa, p)
!==========================================================================
!
!  Calculates the potential enthalpy of ice at which seawater freezes.
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar ) 
!
!  pot_enthalpy_ice_freezing = potential enthalpy of ice at freezing 
!                              of seawater                        [ deg C ]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_pot_enthalpy_from_pt_ice, gsw_pt0_from_t_ice
use gsw_mod_toolbox, only : gsw_t_freezing_exact

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, p

real (r8) :: gsw_pot_enthalpy_ice_freezing

real (r8) :: pt0_ice, t_freezing

t_freezing = gsw_t_freezing_exact(sa,p,0.0_r8) 

pt0_ice = gsw_pt0_from_t_ice(t_freezing,p) 

gsw_pot_enthalpy_ice_freezing = gsw_pot_enthalpy_from_pt_ice(pt0_ice)

return
end function

!--------------------------------------------------------------------------
