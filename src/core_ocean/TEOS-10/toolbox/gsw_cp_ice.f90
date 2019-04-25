!==========================================================================
elemental function gsw_cp_ice (t, p)
!==========================================================================
! 
!  Calculates the isobaric heat capacity of seawater.
!
!  t   =  in-situ temperature (ITS-90)                            [ deg C ]
!  p   =  sea pressure                                             [ dbar ]
!          ( i.e. absolute pressure - 10.1325 dbar )
!
!  gsw_cp_ice  =  heat capacity of ice                       [J kg^-1 K^-1]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_gibbs_ice

use gsw_mod_teos10_constants, only : gsw_t0

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: t, p

real (r8) :: gsw_cp_ice

gsw_cp_ice = -(t + gsw_t0)*gsw_gibbs_ice(2,0,t,p)

end function

!--------------------------------------------------------------------------
