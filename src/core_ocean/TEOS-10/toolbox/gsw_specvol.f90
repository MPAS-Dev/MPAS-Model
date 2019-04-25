!==========================================================================
elemental function gsw_specvol (sa, ct, p)
!==========================================================================
! 
!  Calculates specific volume from Absolute Salinity, Conservative 
!  Temperature and pressure, using the computationally-efficient
!  polynomial expression for specific volume (Roquet et al., 2014).
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  CT  =  Conservative Temperature (ITS-90)                       [ deg C ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar )
!
!  specvol  =  specific volume                                   [ m^3/kg ]
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : gsw_sfac, offset

use gsw_mod_specvol_coefficients

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct, p

real (r8) :: gsw_specvol

real (r8) :: xs, ys, z

xs = sqrt(gsw_sfac*sa + offset)
ys = ct*0.025_r8
z = p*1e-4_r8

gsw_specvol = v000 + xs*(v010 + xs*(v020 + xs*(v030 + xs*(v040 + xs*(v050 &
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

return
end function

!--------------------------------------------------------------------------
