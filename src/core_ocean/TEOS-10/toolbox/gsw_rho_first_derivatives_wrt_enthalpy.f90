!==========================================================================
elemental subroutine gsw_rho_first_derivatives_wrt_enthalpy (sa, ct, p, &
                                                             rho_sa, rho_h)
! =========================================================================
!
!  Calculates two first-order derivatives of specific volume (v).
!  Note that this function uses the using the computationally-efficient
!  expression for specific volume (Roquet et al., 2014).
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  CT  =  Conservative Temperature (ITS-90)                       [ deg C ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar )
!
!  rho_SA =  The first derivative of rho with respect to 
!              Absolute Salinity at constant CT & p.    [ J/(kg (g/kg)^2) ]
!  rho_h  =  The first derivative of rho with respect to 
!              SA and CT at constant p.                  [ J/(kg K(g/kg)) ]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_specvol
use gsw_mod_toolbox, only : gsw_specvol_first_derivatives_wrt_enthalpy

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct, p
real (r8), intent(out), optional :: rho_sa, rho_h

real (r8) :: rec_v2, v_h, v_sa

if (present(rho_sa) .and. present(rho_h)) then

    call gsw_specvol_first_derivatives_wrt_enthalpy(sa,ct,p,v_sa,v_h)

else if (present(rho_sa)) then

    call gsw_specvol_first_derivatives_wrt_enthalpy(sa,ct,p,v_sa=v_sa)

else if (present(rho_h)) then

    call gsw_specvol_first_derivatives_wrt_enthalpy(sa,ct,p,v_h=v_h)

end if

rec_v2 = (1.0_r8/gsw_specvol(sa,ct,p))**2

if (present(rho_sa)) rho_sa = -v_sa*rec_v2

if (present(rho_h)) rho_h = -v_h*rec_v2

return
end subroutine

!--------------------------------------------------------------------------
