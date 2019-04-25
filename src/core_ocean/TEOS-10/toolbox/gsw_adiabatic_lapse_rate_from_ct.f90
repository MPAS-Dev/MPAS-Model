!==========================================================================
elemental function gsw_adiabatic_lapse_rate_from_ct (sa, ct, p) 
!==========================================================================

! Calculates the adiabatic lapse rate from Conservative Temperature
!
! sa     : Absolute Salinity                                 [g/kg]
! ct     : Conservative Temperature                          [deg C]
! p      : sea pressure                                      [dbar]
! 
! gsw_adiabatic_lapse_rate_from_ct : adiabatic lapse rate    [K/Pa]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_gibbs, gsw_pt_from_ct, gsw_pt_from_t

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct, p 

real (r8) :: gsw_adiabatic_lapse_rate_from_ct

real (r8) :: pt0, t

integer, parameter :: n0=0, n1=1, n2=2
real (r8), parameter :: pr0 = 0.0_r8

pt0 = gsw_pt_from_ct(sa,ct)
t = gsw_pt_from_t(sa,pt0,pr0,p)

gsw_adiabatic_lapse_rate_from_ct = -gsw_gibbs(n0,n1,n1,sa,t,p) / &
                                        gsw_gibbs(n0,n2,n0,sa,t,p)

return
end function

!--------------------------------------------------------------------------
