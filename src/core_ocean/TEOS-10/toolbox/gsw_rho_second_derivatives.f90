!==========================================================================
elemental subroutine gsw_rho_second_derivatives (sa, ct, p, rho_sa_sa, &
                                  rho_sa_ct, rho_ct_ct, rho_sa_p, rho_ct_p)
!==========================================================================
!
!  Calculates five second-order derivatives of rho. Note that this function
!  uses the using the computationally-efficient expression for specific
!  volume (Roquet et al., 2014).
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  CT  =  Conservative Temperature (ITS-90)                       [ deg C ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar )
!
!  rho_SA_SA = The second-order derivative of rho with respect to 
!              Absolute Salinity at constant CT & p.    [ J/(kg (g/kg)^2) ]
!  rho_SA_CT = The second-order derivative of rho with respect to 
!              SA and CT at constant p.                  [ J/(kg K(g/kg)) ]
!  rho_CT_CT = The second-order derivative of rho with respect to CT at 
!              constant SA & p
!  rho_SA_P  = The second-order derivative with respect to SA & P at 
!              constant CT. 
!  rho_CT_P  = The second-order derivative with respect to CT & P at 
!              constant SA. 
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_specvol, gsw_specvol_first_derivatives
use gsw_mod_toolbox, only : gsw_specvol_second_derivatives

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct, p
real (r8), intent(out), optional :: rho_sa_sa, rho_sa_ct, rho_ct_ct
real (r8), intent(out), optional :: rho_sa_p, rho_ct_p

integer :: iflag1, iflag2
real (r8) :: rec_v, rec_v2, rec_v3, v_ct, v_ct_ct, v_ct_p, v_p, v_sa, v_sa_ct
real (r8) :: v_sa_p, v_sa_sa

iflag1 = 0
if (present(rho_sa_sa) .or. present(rho_sa_ct) &
                       .or. present(rho_sa_p)) iflag1 = ibset(iflag1,1)
if (present(rho_sa_ct) .or. present(rho_ct_ct) &
                       .or. present(rho_ct_p)) iflag1 = ibset(iflag1,2)
if (present(rho_sa_p) .or. present(rho_ct_p)) iflag1 = ibset(iflag1,3)

call gsw_specvol_first_derivatives(sa,ct,p,v_sa,v_ct,v_p,iflag=iflag1)

iflag2 = 0
if (present(rho_sa_sa)) iflag2 = ibset(iflag2,1)
if (present(rho_sa_ct)) iflag2 = ibset(iflag2,2)
if (present(rho_ct_ct)) iflag2 = ibset(iflag2,3)
if (present(rho_sa_p)) iflag2 = ibset(iflag2,4)
if (present(rho_ct_p)) iflag2 = ibset(iflag2,5)

call gsw_specvol_second_derivatives(sa,ct,p,v_sa_sa,v_sa_ct,v_ct_ct, &
                                    v_sa_p,v_ct_p,iflag=iflag2)

rec_v = 1.0_r8/gsw_specvol(sa,ct,p)
rec_v2 = rec_v**2
rec_v3 = rec_v2*rec_v

if (present(rho_sa_sa)) rho_sa_sa = -v_sa_sa*rec_v2 + 2.0_r8*v_sa*v_sa*rec_v3

if (present(rho_sa_ct)) rho_sa_ct = -v_sa_ct*rec_v2 + 2.0_r8*v_sa*v_ct*rec_v3

if (present(rho_ct_ct)) rho_ct_ct = -v_ct_ct*rec_v2 + 2.0_r8*v_ct*v_ct*rec_v3

if (present(rho_sa_p)) rho_sa_p = -v_sa_p*rec_v2 + 2.0_r8*v_sa*v_p*rec_v3

if (present(rho_ct_p)) rho_ct_p = -v_ct_p*rec_v2 + 2.0_r8*v_ct*v_p*rec_v3

return
end subroutine

!--------------------------------------------------------------------------
