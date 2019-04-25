!==========================================================================
elemental subroutine gsw_enthalpy_first_derivatives (sa, ct, p, h_sa, h_ct)
!==========================================================================
!
!  Calculates the following two derivatives of specific enthalpy (h) of
!  seawater using the computationally-efficient expression for 
!  specific volume in terms of SA, CT and p (Roquet et al., 2014).  
!   (1) h_SA, the derivative with respect to Absolute Salinity at 
!       constant CT and p, and
!   (2) h_CT, derivative with respect to CT at constant SA and p. 
!  Note that h_P is specific volume (1/rho) it can be caclulated by calling
!  gsw_specvol(SA,CT,p).
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  CT  =  Conservative Temperature (ITS-90)                       [ deg C ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar )
!
!  h_SA  =  The first derivative of specific enthalpy with respect to 
!           Absolute Salinity at constant CT and p.     
!                                            [ J/(kg (g/kg))]  i.e. [ J/g ]
!  h_CT  =  The first derivative of specific enthalpy with respect to 
!           CT at constant SA and p.                           [ J/(kg K) ]
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : gsw_cp0, gsw_sfac, offset

use gsw_mod_specvol_coefficients

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct, p
real (r8), intent(out), optional :: h_sa, h_ct

real (r8) :: dynamic_h_ct_part, dynamic_h_sa_part, xs, ys, z

xs = sqrt(gsw_sfac*sa + offset)
ys = ct*0.025_r8
z = p*1e-4_r8

if (present(h_sa)) then

    dynamic_h_sa_part =  z*(h101 + xs*(2.0_r8*h201 + xs*(3.0_r8*h301 &
        + xs*(4.0_r8*h401 + xs*(5.0_r8*h501 + 6.0_r8*h601*xs)))) + ys*(h111 &
        + xs*(2.0_r8*h211 + xs*(3.0_r8*h311 + xs*(4.0_r8*h411 &
        + 5.0_r8*h511*xs))) + ys*(h121 + xs*(2.0_r8*h221 + xs*(3.0_r8*h321 &
        + 4.0_r8*h421*xs)) + ys*(h131 + xs*(2.0_r8*h231 + 3.0_r8*h331*xs) &
        + ys*(h141 + 2.0_r8*h241*xs + h151*ys)))) + z*(h102 &
        + xs*(2.0_r8*h202 + xs*(3.0_r8*h302 + xs*(4.0_r8*h402 &
        + 5.0_r8*h502*xs))) + ys*(h112 + xs*(2.0_r8*h212 + xs*(3.0_r8*h312 &
        + 4.0_r8*h412*xs)) + ys*(h122 + xs*(2.0_r8*h222 + 3.0_r8*h322*xs) &
        + ys*(h132 + 2.0_r8*h232*xs + h142*ys ))) + z*(h103 + xs*(2.0_r8*h203 &
        + xs*(3.0_r8*h303 + 4.0_r8*h403*xs)) + ys*(h113 + xs*(2.0_r8*h213 &
        + 3.0_r8*h313*xs) + ys*(h123 + 2.0_r8*h223*xs + h133*ys)) &
        + z*(h104 + 2.0_r8*h204*xs + h114*ys + h105*z))))

    h_sa = 1e8_r8*0.5_r8*gsw_sfac*dynamic_h_sa_part/xs

end if

if (present(h_ct)) then

    dynamic_h_ct_part = z*(h011 + xs*(h111 + xs*(h211 + xs*(h311 + xs*(h411 &
        + h511*xs)))) + ys*(2.0_r8*(h021 + xs*(h121 + xs*(h221 + xs*(h321 &
        + h421*xs)))) + ys*(3.0_r8*(h031 + xs*(h131 + xs*(h231 + h331*xs))) &
        + ys*(4.0_r8*(h041 + xs*(h141 + h241*xs)) + ys*(5.0_r8*(h051 &
        + h151*xs) + 6.0_r8*h061*ys)))) + z*(h012 + xs*(h112 + xs*(h212 &
        + xs*(h312 + h412*xs))) + ys*(2.0_r8*(h022 + xs*(h122 + xs*(h222 &
        + h322*xs))) + ys*(3.0_r8*(h032 + xs*(h132 + h232*xs)) &
        + ys*(4.0_r8*(h042 + h142*xs) + 5.0_r8*h052*ys))) + z*(h013 &
        + xs*(h113 + xs*(h213 + h313*xs)) + ys*(2.0_r8*(h023 + xs*(h123 &
        + h223*xs)) + ys*(3.0_r8*(h033 + h133*xs) + 4.0_r8*h043*ys)) &
        + z*(h014 + h114*xs + 2.0_r8*h024*ys + h015*z ))))

    h_ct = gsw_cp0 + 1e8_r8*0.025_r8*dynamic_h_ct_part

end if

return
end subroutine

!--------------------------------------------------------------------------
