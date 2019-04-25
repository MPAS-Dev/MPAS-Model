!==========================================================================
elemental function gsw_dynamic_enthalpy (sa, ct, p)
!==========================================================================
!
!  Calculates dynamic enthalpy of seawater using the computationally-
!  efficient expression for specific volume in terms of SA, CT and p
!  (Roquet et al., 2014).  Dynamic enthalpy is defined as enthalpy minus
!  potential enthalpy (Young, 2010). 
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  CT  =  Conservative Temperature (ITS-90)                       [ deg C ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar )
!
!  dynamic_enthalpy  =  dynamic enthalpy                           [ J/kg ]
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : db2pa, gsw_sfac, offset

use gsw_mod_specvol_coefficients

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct, p

real (r8) :: gsw_dynamic_enthalpy

real (r8) :: dynamic_enthalpy_part, xs, ys, z

xs = sqrt(gsw_sfac*sa + offset)
ys = ct*0.025_r8
z = p*1e-4_r8

dynamic_enthalpy_part =  z*(h001 + xs*(h101 + xs*(h201 + xs*(h301 + xs*(h401 &
    + xs*(h501 + h601*xs))))) + ys*(h011 + xs*(h111 + xs*(h211 + xs*(h311 &
    + xs*(h411 + h511*xs)))) + ys*(h021 + xs*(h121 + xs*(h221 + xs*(h321 &
    + h421*xs))) + ys*(h031 + xs*(h131 + xs*(h231 + h331*xs)) + ys*(h041 &
    + xs*(h141 + h241*xs) + ys*(h051 + h151*xs + h061*ys))))) + z*(h002 &
    + xs*(h102 + xs*(h202 + xs*(h302 + xs*(h402 + h502*xs)))) + ys*(h012 &
    + xs*(h112 + xs*(h212 + xs*(h312 + h412*xs))) + ys*(h022 + xs*(h122 &
    + xs*(h222 + h322*xs)) + ys*(h032 + xs*(h132 + h232*xs) + ys*(h042 &
    + h142*xs + h052*ys)))) + z*(h003 + xs*(h103 + xs*(h203 + xs*(h303 &
    + h403*xs))) + ys*(h013 + xs*(h113 + xs*(h213 + h313*xs)) + ys*(h023 &
    + xs*(h123 + h223*xs) + ys*(h033 + h133*xs + h043*ys))) + z*(h004 &
    + xs*(h104 + h204*xs) + ys*(h014 + h114*xs + h024*ys) + z*(h005 &
    + h105*xs + h015*ys + z*(h006 + h007*z))))))

gsw_dynamic_enthalpy = dynamic_enthalpy_part*db2pa*1e4_r8

return
end function

!--------------------------------------------------------------------------
