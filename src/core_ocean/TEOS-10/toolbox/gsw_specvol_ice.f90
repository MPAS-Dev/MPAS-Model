!==========================================================================
elemental function gsw_specvol_ice (t, p)
!==========================================================================
!
!  Calculates the specific volume of ice. 
! 
!  t  =  in-situ temperature (ITS-90)                             [ deg C ]
!  p  =  sea pressure                                              [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar )
!
!  specvol_ice  =  specific volume                               [ m^3/kg ]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_gibbs_ice

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: t, p

real (r8) :: gsw_specvol_ice

gsw_specvol_ice = gsw_gibbs_ice(0,1,t,p)

return
end function

!--------------------------------------------------------------------------
