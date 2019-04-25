!==========================================================================
elemental function gsw_helmholtz_energy_ice (t, p)
!==========================================================================
!
!  Calculates the Helmholtz energy of ice. 
!
!  t  =  in-situ temperature (ITS-90)                             [ deg C ]
!  p  =  sea pressure                                              [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar ) 
!
!  Helmholtz_energy_ice  =  Helmholtz energy of ice                [ J/kg ]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_gibbs_ice

use gsw_mod_teos10_constants, only : gsw_p0, db2pa

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: t, p

real (r8) :: gsw_helmholtz_energy_ice

gsw_helmholtz_energy_ice = gsw_gibbs_ice(0,0,t,p) &
                           - (db2pa*p + gsw_p0)*gsw_gibbs_ice(0,1,t,p)

return
end function

!--------------------------------------------------------------------------
