!==========================================================================
elemental function gsw_sigma4 (sa, ct) 
!==========================================================================
!
!  Calculates potential density anomaly with reference pressure of 4000 dbar,
!  this being this particular potential density minus 1000 kg/m^3.  This
!  function has inputs of Absolute Salinity and Conservative Temperature.
!
! sa     : Absolute Salinity                               [g/kg]
! ct     : Conservative Temperature                        [deg C]
! 
! gsw_sigma4  : potential density anomaly with reference pressure of 4000
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_rho

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct 

real (r8) :: gsw_sigma4

gsw_sigma4 = gsw_rho(sa,ct,4e3_r8) - 1e3_r8

return
end function

!--------------------------------------------------------------------------
