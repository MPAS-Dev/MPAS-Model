!==========================================================================
elemental function gsw_enthalpy_t_exact (sa, t, p) 
!==========================================================================
!
! Calculates the specific enthalpy of seawater
!
! sa     : Absolute Salinity                               [g/kg]
! t      : in-situ temperature                             [deg C]
! p      : sea pressure                                    [dbar]
! 
! gsw_enthalpy_t_exact : specific enthalpy                 [J/kg]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_gibbs

use gsw_mod_teos10_constants, only : gsw_t0

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, t, p 

real (r8) :: gsw_enthalpy_t_exact

integer, parameter :: n0=0, n1=1

gsw_enthalpy_t_exact = gsw_gibbs(n0,n0,n0,sa,t,p) - &
                                (t+gsw_t0)*gsw_gibbs(n0,n1,n0,sa,t,p)

return
end function

!--------------------------------------------------------------------------
