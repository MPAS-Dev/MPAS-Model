!==========================================================================
elemental function gsw_beta_const_t_exact (sa, t, p)  
!==========================================================================
!
! Calculates saline (haline) contraction coefficient of seawater at 
! constant in-situ temperature.
!
! sa     : Absolute Salinity                               [g/kg]
! t      : in-situ temperature                             [deg C]
! p      : sea pressure                                    [dbar]
! 
! gsw_beta_const_t_exact : haline contraction coefficient  [kg/g]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_gibbs

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, t, p  

real (r8) :: gsw_beta_const_t_exact

integer, parameter :: n0=0, n1=1

gsw_beta_const_t_exact = -gsw_gibbs(n1,n0,n1,sa,t,p)/gsw_gibbs(n0,n0,n1,sa,t,p)

return
end function

!--------------------------------------------------------------------------
