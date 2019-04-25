!==========================================================================
elemental function gsw_kappa (sa, ct, p)
!==========================================================================
!
!  Calculates the isentropic compressibility of seawater.  This function 
!  has inputs of Absolute Salinity and Conservative Temperature.  This 
!  function uses the computationally-efficient expression for 
!  specific volume in terms of SA, CT and p (Roquet et al., 2014).
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  CT  =  Conservative Temperature (ITS-90)                       [ deg C ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar )
!
!  kappa  =  isentropic compressibility of seawater                [ 1/Pa ]
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : gsw_sfac, offset

use gsw_mod_specvol_coefficients

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct, p

real (r8) :: gsw_kappa

real (r8) :: v, v_p, xs, ys, z

xs = sqrt(gsw_sfac*sa + offset)
ys = ct*0.025_r8
z = p*1e-4_r8

v = v000 + xs*(v010 + xs*(v020 + xs*(v030 + xs*(v040 + xs*(v050 &
    + v060*xs))))) + ys*(v100 + xs*(v110 + xs*(v120 + xs*(v130 + xs*(v140 &
    + v150*xs)))) + ys*(v200 + xs*(v210 + xs*(v220 + xs*(v230 + v240*xs))) &
    + ys*(v300 + xs*(v310 + xs*(v320 + v330*xs)) + ys*(v400 + xs*(v410 &
    + v420*xs) + ys*(v500 + v510*xs + v600*ys))))) + z*(v001 + xs*(v011 &
    + xs*(v021 + xs*(v031 + xs*(v041 + v051*xs)))) + ys*(v101 + xs*(v111 &
    + xs*(v121 + xs*(v131 + v141*xs))) + ys*(v201 + xs*(v211 + xs*(v221 &
    + v231*xs)) + ys*(v301 + xs*(v311 + v321*xs) + ys*(v401 + v411*xs &
    + v501*ys)))) + z*(v002 + xs*(v012 + xs*(v022 + xs*(v032 + v042*xs))) &
    + ys*(v102 + xs*(v112 + xs*(v122 + v132*xs)) + ys*(v202 + xs*(v212 &
    + v222*xs) + ys*(v302 + v312*xs + v402*ys))) + z*(v003 + xs*(v013 &
    + v023*xs) + ys*(v103 + v113*xs + v203*ys) + z*(v004 + v014*xs + v104*ys &  
    + z*(v005 + v006*z)))))

v_p = c000 + xs*(c100 + xs*(c200 + xs*(c300 + xs*(c400 + c500*xs)))) & 
    + ys*(c010 + xs*(c110 + xs*(c210 + xs*(c310 + c410*xs))) + ys*(c020 &
    + xs*(c120 + xs*(c220 + c320*xs)) + ys*(c030 + xs*(c130 + c230*xs) &
    + ys*(c040 + c140*xs + c050*ys)))) + z*(c001 + xs*(c101 + xs*(c201 &
    + xs*(c301 + c401*xs))) + ys*(c011 + xs*(c111 + xs*(c211 + c311*xs)) &
    + ys*(c021 + xs*(c121 + c221*xs) + ys*(c031 + c131*xs + c041*ys))) &
    + z*( c002 + xs*(c102 + c202*xs) + ys*(c012 + c112*xs + c022*ys) &
    + z*(c003 + c103*xs + c013*ys + z*(c004 + c005*z))))

gsw_kappa = -1e-8_r8*v_p/v

return
end function

!--------------------------------------------------------------------------
