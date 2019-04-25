!==========================================================================
elemental function gsw_enthalpy_diff (sa, ct, p_shallow, p_deep)
!==========================================================================
!
!  Calculates the difference of the specific enthalpy of seawater between 
!  two different pressures, p_deep (the deeper pressure) and p_shallow
!  (the shallower pressure), at the same values of SA and CT.  This 
!  function uses the computationally-efficient expression for specific 
!  volume in terms of SA, CT and p (Roquet et al., 2014).  The output
!  (enthalpy_diff_CT) is the specific enthalpy evaluated at (SA,CT,p_deep)
!  minus the specific enthalpy at (SA,CT,p_shallow). 
!
!  SA         =  Absolute Salinity                                 [ g/kg ]
!  CT         =  Conservative Temperature (ITS-90)                [ deg C ]
!  p_shallow  =  upper sea pressure                                [ dbar ]
!                ( i.e. shallower absolute pressure - 10.1325 dbar ) 
!  p_deep     =  lower sea pressure                                [ dbar ]
!                ( i.e. deeper absolute pressure - 10.1325 dbar )
!
!  enthalpy_diff_CT  =  difference of specific enthalpy            [ J/kg ]
!                       (deep minus shallow)
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : db2pa, gsw_sfac, offset

use gsw_mod_specvol_coefficients

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct, p_shallow, p_deep

real (r8) :: gsw_enthalpy_diff

real (r8) :: dynamic_enthalpy_shallow, dynamic_enthalpy_deep
real (r8) :: part_1, part_2, part_3, part_4, part_5, xs, ys, z_deep, z_shallow

xs = sqrt(gsw_sfac*sa + offset)
ys = ct*0.025_r8
z_shallow = p_shallow*1e-4_r8
z_deep = p_deep*1e-4_r8

part_1 = h001 + xs*(h101 + xs*(h201 + xs*(h301 + xs*(h401 &
    + xs*(h501 + h601*xs))))) + ys*(h011 + xs*(h111 + xs*(h211 + xs*(h311 &
    + xs*(h411 + h511*xs)))) + ys*(h021 + xs*(h121 + xs*(h221 + xs*(h321 &
    + h421*xs))) + ys*(h031 + xs*(h131 + xs*(h231 + h331*xs)) + ys*(h041 &
    + xs*(h141 + h241*xs) + ys*(h051 + h151*xs + h061*ys)))))

part_2 = h002 + xs*(h102 + xs*(h202 + xs*(h302 + xs*(h402 + h502*xs)))) &
    + ys*(h012 + xs*(h112 + xs*(h212 + xs*(h312 + h412*xs))) + ys*(h022 &
    + xs*(h122 + xs*(h222 + h322*xs)) + ys*(h032 + xs*(h132 + h232*xs) &
    + ys*(h042 + h142*xs + h052*ys))))

part_3 = h003 + xs*(h103 + xs*(h203 + xs*(h303 + h403*xs))) + ys*(h013 &
    + xs*(h113 + xs*(h213 + h313*xs)) + ys*(h023 + xs*(h123 + h223*xs) &
    + ys*(h033 + h133*xs + h043*ys)))

part_4 = h004 + xs*(h104 + h204*xs) + ys*(h014 + h114*xs + h024*ys)

part_5 = h005 + h105*xs + h015*ys

dynamic_enthalpy_shallow =  z_shallow*(part_1 + z_shallow*(part_2 &
    + z_shallow*(part_3 + z_shallow*(part_4 + z_shallow*(part_5 &
    + z_shallow*(h006 + h007*z_shallow))))))

dynamic_enthalpy_deep = z_deep*(part_1 + z_deep*(part_2 + z_deep*(part_3 &
    + z_deep*(part_4 + z_deep*(part_5 + z_deep*(h006 + h007*z_deep))))))

gsw_enthalpy_diff = (dynamic_enthalpy_deep &
                     - dynamic_enthalpy_shallow)*db2pa*1e4_r8

return
end function

!--------------------------------------------------------------------------
