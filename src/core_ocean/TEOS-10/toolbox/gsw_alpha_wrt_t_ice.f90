!==========================================================================
elemental function gsw_alpha_wrt_t_ice (t, p)
!==========================================================================
!
!  Calculates the thermal expansion coefficient of ice with respect to  
!  in-situ temperature.
!   
!  t  =  in-situ temperature (ITS-90)                             [ deg C ]
!  p  =  sea pressure                                              [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar )
!
!  alpha_wrt_t_ice  =  thermal expansion coefficient of ice with respect      
!                      to in-situ temperature                       [ 1/K ]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_gibbs_ice

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: t, p

real (r8) :: gsw_alpha_wrt_t_ice

gsw_alpha_wrt_t_ice = gsw_gibbs_ice(1,1,t,p)/gsw_gibbs_ice(0,1,t,p)

return
end function

!--------------------------------------------------------------------------
