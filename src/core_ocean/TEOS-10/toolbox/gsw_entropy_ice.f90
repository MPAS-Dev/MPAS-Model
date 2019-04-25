!==========================================================================
elemental function gsw_entropy_ice (t, p)
!==========================================================================
!
!  Calculates specific entropy of ice. 
!
!  t  =  in-situ temperature (ITS-90)                             [ deg C ]
!  p  =  sea pressure                                              [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar ) 
!
!  ice_entropy  =  specific entropy of ice                 [ J kg^-1 K^-1 ]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_gibbs_ice

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: t, p

real (r8) :: gsw_entropy_ice

gsw_entropy_ice = -gsw_gibbs_ice(1,0,t,p)

return
end function

!--------------------------------------------------------------------------
