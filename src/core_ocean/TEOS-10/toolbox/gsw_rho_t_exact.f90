!==========================================================================
elemental function gsw_rho_t_exact (sa, t, p) 
!==========================================================================
!
! Calculates in-situ density of seawater from Absolute Salinity and 
! in-situ temperature.
!
! sa     : Absolute Salinity                               [g/kg]
! t      : in-situ temperature                             [deg C]
! p      : sea pressure                                    [dbar]
! 
! gsw_rho_t_exact : in-situ density                        [kg/m^3]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_gibbs

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, t, p 

real (r8) :: gsw_rho_t_exact

integer, parameter :: n0=0, n1=1

gsw_rho_t_exact = 1.0_r8/gsw_gibbs(n0,n0,n1,sa,t,p)

return
end function

!--------------------------------------------------------------------------
