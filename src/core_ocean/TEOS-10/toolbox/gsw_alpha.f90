!==========================================================================
elemental function gsw_alpha (sa, ct, p)
!==========================================================================
!
!  Calculates the thermal expansion coefficient of seawater with respect to 
!  Conservative Temperature using the computationally-efficient expression
!  for specific volume in terms of SA, CT and p (Roquet et al., 2014).
!   
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  CT  =  Conservative Temperature (ITS-90)                       [ deg C ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar )
!
!  alpha  =  thermal expansion coefficient                          [ 1/K ]
!            with respect to Conservative Temperature
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_specvol

use gsw_mod_teos10_constants, only : gsw_sfac, offset

use gsw_mod_specvol_coefficients

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct, p

real (r8) :: gsw_alpha

real (r8) :: xs, ys, z, v_ct_part

xs = sqrt(gsw_sfac*sa + offset)
ys = ct*0.025_r8
z = p*1e-4_r8

v_ct_part = a000 + xs*(a100 + xs*(a200 + xs*(a300 + xs*(a400 + a500*xs)))) &
         + ys*(a010 + xs*(a110 + xs*(a210 + xs*(a310 + a410*xs))) &
         + ys*(a020 + xs*(a120 + xs*(a220 + a320*xs)) + ys*(a030 &
         + xs*(a130 + a230*xs) + ys*(a040 + a140*xs + a050*ys )))) &
         + z*(a001 + xs*(a101 + xs*(a201 + xs*(a301 + a401*xs))) &
         + ys*(a011 + xs*(a111 + xs*(a211 + a311*xs)) + ys*(a021 &
         + xs*(a121 + a221*xs) + ys*(a031 + a131*xs + a041*ys))) &
         + z*(a002 + xs*(a102 + xs*(a202 + a302*xs)) + ys*(a012 &
         + xs*(a112 + a212*xs) + ys*(a022 + a122*xs + a032*ys)) &
         + z*(a003 + a103*xs + a013*ys + a004*z))) 

gsw_alpha = 0.025_r8*v_ct_part/gsw_specvol(sa,ct,p)

return
end function

!--------------------------------------------------------------------------
