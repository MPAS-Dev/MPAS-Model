!==========================================================================
elemental function gsw_pressure_coefficient_ice (t, p)
!==========================================================================
!
!  Calculates pressure coefficient of ice. 
!
!  t  =  in-situ temperature (ITS-90)                             [ deg C ]
!  p  =  sea pressure                                              [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar )
!
!  pressure_coefficient_ice  =  pressure coefficient of ice          [Pa/K]
!   Note. The output units are Pa/K NOT dbar/K.
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_gibbs_ice

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: t, p

real (r8) :: gsw_pressure_coefficient_ice

gsw_pressure_coefficient_ice = -gsw_gibbs_ice(1,1,t,p)/gsw_gibbs_ice(0,2,t,p)

return
end function

!--------------------------------------------------------------------------
