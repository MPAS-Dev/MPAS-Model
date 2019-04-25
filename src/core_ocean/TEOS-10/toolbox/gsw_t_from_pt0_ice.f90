! =========================================================================
elemental function gsw_t_from_pt0_ice (pt0_ice, p)
! =========================================================================
!
!  Calculates in-situ temperature from the potential temperature of ice Ih 
!  with reference pressure, p_ref, of 0 dbar (the surface), and the 
!  in-situ pressure.
!
!  pt0_ice  =  potential temperature of ice Ih with reference pressure of 
!              zero dbar (ITS-90)                                 [ deg C ]
!  p        =  sea pressure                                        [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar )
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_pt_from_t_ice

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: pt0_ice, p

real (r8) :: gsw_t_from_pt0_ice

real (r8), parameter :: p0 = 0.0_r8

gsw_t_from_pt0_ice = gsw_pt_from_t_ice(pt0_ice,p0,p)

return
end function

!--------------------------------------------------------------------------
