!==========================================================================
elemental function gsw_kappa_t_exact (sa, t, p)  
!==========================================================================
!
! isentropic compressibility of seawater
!
! sa     : Absolute Salinity                               [g/kg]
! t      : in-situ temperature                             [deg C]
! p      : sea pressure                                    [dbar]
!
! gsw_kappa_t_exact : isentropic compressibility           [1/Pa]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_gibbs

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, t, p  

real (r8) :: gsw_kappa_t_exact

real (r8) :: g_tt, g_tp

integer, parameter :: n0=0, n1=1, n2=2

g_tt = gsw_gibbs(n0,n2,n0,sa,t,p)
g_tp = gsw_gibbs(n0,n1,n1,sa,t,p)

gsw_kappa_t_exact = (g_tp*g_tp - g_tt*gsw_gibbs(n0,n0,n2,sa,t,p)) / &
                         (gsw_gibbs(n0,n0,n1,sa,t,p)*g_tt)

return
end function

!--------------------------------------------------------------------------
