!==========================================================================
elemental function gsw_beta (sa, ct, p)
!==========================================================================
!
!  Calculates the saline (i.e. haline) contraction coefficient of seawater  
!  at constant Conservative Temperature using the computationally-efficient
!  expression for specific volume in terms of SA, CT and p 
!  (Roquet et al., 2014).
!   
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  CT  =  Conservative Temperature (ITS-90)                       [ deg C ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar )
!
!  beta  =  saline contraction coefficient                         [ kg/g ]
!           at constant Conservative Temperature
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_specvol

use gsw_mod_teos10_constants, only : gsw_sfac, offset

use gsw_mod_specvol_coefficients

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct, p

real (r8) :: gsw_beta

real (r8) :: xs, ys, z, v_sa_part

xs = sqrt(gsw_sfac*sa + offset)
ys = ct*0.025_r8
z = p*1e-4_r8

v_sa_part = b000 + xs*(b100 + xs*(b200 + xs*(b300 + xs*(b400 + b500*xs)))) &
       + ys*(b010 + xs*(b110 + xs*(b210 + xs*(b310 + b410*xs))) &
       + ys*(b020 + xs*(b120 + xs*(b220 + b320*xs)) + ys*(b030 &
       + xs*(b130 + b230*xs) + ys*(b040 + b140*xs + b050*ys)))) &
       + z*(b001 + xs*(b101 + xs*(b201 + xs*(b301 + b401*xs))) &
       + ys*(b011 + xs*(b111 + xs*(b211 + b311*xs)) + ys*(b021 &
       + xs*(b121 + b221*xs) + ys*(b031 + b131*xs + b041*ys))) &
       + z*(b002 + xs*(b102 + xs*(b202 + b302*xs))+ ys*(b012 &
       + xs*(b112 + b212*xs) + ys*(b022 + b122*xs + b032*ys)) &
       + z*(b003 +  b103*xs + b013*ys + b004*z)))
 
gsw_beta = -v_sa_part*0.5_r8*gsw_sfac/(gsw_specvol(sa,ct,p)*xs)

return
end function

!--------------------------------------------------------------------------
