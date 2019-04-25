!==========================================================================
elemental function gsw_thermobaric (sa, ct, p)
!==========================================================================
!
!  Calculates the thermobaric coefficient of seawater with respect to
!  Conservative Temperature.  This routine is based on the 
!  computationally-efficient expression for specific volume in terms of
!  SA, CT and p (Roquet et al., 2014).
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  CT  =  Conservative Temperature (ITS-90)                       [ deg C ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar )
!
!  thermobaric  =  thermobaric coefficient with                [ 1/(K Pa) ] 
!                  respect to Conservative Temperature.           
!  Note. The pressure derivative is taken with respect to
!    pressure in Pa not dbar.
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_rho, gsw_specvol_first_derivatives
use gsw_mod_toolbox, only : gsw_specvol_second_derivatives

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct, p

real (r8) :: gsw_thermobaric

real (r8) :: v_ct, v_ct_p, v_sa, v_sa_p

call gsw_specvol_first_derivatives(sa,ct,p,v_sa=v_sa,v_ct=v_ct)

call gsw_specvol_second_derivatives(sa,ct,p,v_sa_p=v_sa_p,v_ct_p=v_ct_p)

gsw_thermobaric = gsw_rho(sa,ct,p)*(v_ct_p - (v_ct/v_sa)*v_sa_p)
 
return
end function

!--------------------------------------------------------------------------
