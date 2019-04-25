!==========================================================================
elemental subroutine gsw_rho_second_derivatives_wrt_enthalpy (sa, ct, p, &
                                              rho_sa_sa, rho_sa_h, rho_h_h)
! =========================================================================
!
!  Calculates three second-order derivatives of rho with respect to enthalpy.
!  Note that this function uses the using the computationally-efficient
!  expression for specific volume (Roquet et al., 2014).
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  CT  =  Conservative Temperature (ITS-90)                       [ deg C ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar )
!
!  rho_SA_SA = The second-order derivative of rho with respect to 
!              Absolute Salinity at constant h & p.     [ J/(kg (g/kg)^2) ]
!  rho_SA_h  = The second-order derivative of rho with respect to 
!              SA and h at constant p.                   [ J/(kg K(g/kg)) ]
!  rho_h_h   = The second-order derivative of rho with respect to h at 
!              constant SA & p
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_specvol
use gsw_mod_toolbox, only : gsw_specvol_first_derivatives_wrt_enthalpy
use gsw_mod_toolbox, only : gsw_specvol_second_derivatives_wrt_enthalpy

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct, p
real (r8), intent(out), optional :: rho_sa_sa, rho_sa_h, rho_h_h

integer :: iflag1, iflag2
real (r8) :: rec_v, rec_v2, rec_v3, v_h, v_h_h, v_sa, v_sa_h, v_sa_sa

iflag1 = 0
if (present(rho_sa_sa) .or. present(rho_sa_h)) iflag1 = ibset(iflag1,1)
if (present(rho_sa_h) .or. present(rho_h_h)) iflag1 = ibset(iflag1,2)

call gsw_specvol_first_derivatives_wrt_enthalpy(sa,ct,p,v_sa,v_h,iflag=iflag1)

iflag2 = 0
if (present(rho_sa_sa)) iflag2 = ibset(iflag2,1)
if (present(rho_sa_h)) iflag2 = ibset(iflag2,2)
if (present(rho_h_h)) iflag2 = ibset(iflag2,3)

call gsw_specvol_second_derivatives_wrt_enthalpy(sa,ct,p,v_sa_sa,v_sa_h,v_h_h, &
                                                 iflag=iflag2)

rec_v = 1.0_r8/gsw_specvol(sa,ct,p)
rec_v2 = rec_v**2
rec_v3 = rec_v2*rec_v

if (present(rho_sa_sa)) rho_sa_sa = -v_sa_sa*rec_v2 + 2.0_r8*v_sa*v_sa*rec_v3

if (present(rho_sa_h)) rho_sa_h = -v_sa_h*rec_v2 + 2.0_r8*v_sa*v_h*rec_v3

if (present(rho_h_h)) rho_h_h = -v_h_h*rec_v2 + 2.0_r8*v_h*v_h*rec_v3

return
end subroutine

!--------------------------------------------------------------------------
