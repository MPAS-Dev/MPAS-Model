!==========================================================================
elemental function gsw_sigma3 (sa, ct) 
!==========================================================================
!
!  Calculates potential density anomaly with reference pressure of 3000 dbar,
!  this being this particular potential density minus 1000 kg/m^3.  This
!  function has inputs of Absolute Salinity and Conservative Temperature.
!
! sa     : Absolute Salinity                               [g/kg]
! ct     : Conservative Temperature                        [deg C]
! 
! gsw_sigma3  : potential density anomaly with reference pressure of 3000
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_rho

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct 

real (r8) :: gsw_sigma3

gsw_sigma3 = gsw_rho(sa,ct,3e3_r8) - 1e3_r8

return
end function

!--------------------------------------------------------------------------
