!==========================================================================
elemental function gsw_internal_energy_ice (t, p)
!==========================================================================
!
!  Calculates the specific internal energy of ice. 
!
!  t  =  in-situ temperature (ITS-90)                             [ deg C ]
!  p  =  sea pressure                                              [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar ) 
!
!  internal_energy_ice  =  specific internal energy (u)              [J/kg]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_gibbs_ice

use gsw_mod_teos10_constants, only : gsw_p0, gsw_t0, db2pa

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: t, p

real (r8) :: gsw_internal_energy_ice

gsw_internal_energy_ice = gsw_gibbs_ice(0,0,t,p) &
                          - (gsw_t0 + t)*gsw_gibbs_ice(1,0,t,p) &
                          - (db2pa*p + gsw_p0)*gsw_gibbs_ice(0,1,t,p)

return
end function

!--------------------------------------------------------------------------
