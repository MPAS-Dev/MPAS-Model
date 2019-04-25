!==========================================================================
elemental function gsw_t_from_ct (sa, ct, p)  
!==========================================================================
!
! Calculates in-situ temperature from Conservative Temperature of seawater  
!
! sa      : Absolute Salinity                              [g/kg]
! ct      : Conservative Temperature                       [deg C]
!
! gsw_t_from_ct : in-situ temperature                      [deg C]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_pt_from_ct, gsw_pt_from_t

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct, p  

real (r8) :: gsw_t_from_ct

real (r8) :: pt0

real (r8), parameter :: p0 = 0.0_r8

pt0 = gsw_pt_from_ct(sa,ct)
gsw_t_from_ct = gsw_pt_from_t(sa,pt0,p0,p)

return
end function

!--------------------------------------------------------------------------
