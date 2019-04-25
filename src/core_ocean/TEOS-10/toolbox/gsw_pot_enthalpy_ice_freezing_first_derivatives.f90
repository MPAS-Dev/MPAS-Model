!==========================================================================
elemental subroutine gsw_pot_enthalpy_ice_freezing_first_derivatives (sa, &
              p, pot_enthalpy_ice_freezing_sa, pot_enthalpy_ice_freezing_p)
!==========================================================================
!
!  Calculates the first derivatives of the potential enthalpy of ice at
!  which seawater freezes, with respect to Absolute Salinity SA and
!  pressure P (in Pa).
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar ) 
!
!  pot_enthalpy_ice_freezing_SA = the derivative of the potential enthalpy
!                  of ice at freezing (ITS-90) with respect to Absolute
!                  salinity at fixed pressure  [ K/(g/kg) ] i.e. [ K kg/g ]
!
!  pot_enthalpy_ice_freezing_P  = the derivative of the potential enthalpy
!                  of ice at freezing (ITS-90) with respect to pressure 
!                  (in Pa) at fixed Absolute Salinity              [ K/Pa ]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_cp_ice, gsw_gibbs_ice, gsw_pt0_from_t_ice
use gsw_mod_toolbox, only : gsw_t_freezing_exact
use gsw_mod_toolbox, only : gsw_t_freezing_first_derivatives

use gsw_mod_teos10_constants, only : gsw_t0

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, p
real (r8), intent(out), optional :: pot_enthalpy_ice_freezing_sa
real (r8), intent(out), optional :: pot_enthalpy_ice_freezing_p

real (r8) :: cp_ihf, pt_icef, ratio_temp, tf, tf_p, tf_sa

real (r8), parameter :: saturation_fraction = 0.0_r8

tf = gsw_t_freezing_exact(sa,p,saturation_fraction)
pt_icef = gsw_pt0_from_t_ice(tf,p) 
ratio_temp = (gsw_t0 + pt_icef)/(gsw_t0 + tf)

cp_ihf = gsw_cp_ice(tf,p)

if (present(pot_enthalpy_ice_freezing_sa) .and. present(pot_enthalpy_ice_freezing_p)) then
    call gsw_t_freezing_first_derivatives(sa,p,saturation_fraction,tf_sa,tf_p)
else if (present(pot_enthalpy_ice_freezing_sa)) then
    call gsw_t_freezing_first_derivatives(sa,p,saturation_fraction, &
                                          tfreezing_sa=tf_sa)
else if (present(pot_enthalpy_ice_freezing_p)) then
    call gsw_t_freezing_first_derivatives(sa,p,saturation_fraction, &
                                          tfreezing_p=tf_p)
end if

if (present(pot_enthalpy_ice_freezing_sa)) &
                pot_enthalpy_ice_freezing_sa = ratio_temp*cp_ihf*tf_sa

if (present(pot_enthalpy_ice_freezing_p)) &
                pot_enthalpy_ice_freezing_p = ratio_temp*cp_ihf*tf_p &
                             - (gsw_t0 + pt_icef)*gsw_gibbs_ice(1,1,tf,p)

return
end subroutine

!--------------------------------------------------------------------------
