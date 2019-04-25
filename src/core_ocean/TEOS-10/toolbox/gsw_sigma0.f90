!==========================================================================
elemental function gsw_sigma0 (sa, ct)
!==========================================================================
! 
!  Calculates potential density anomaly with reference pressure of 0 dbar,
!  this being this particular potential density minus 1000 kg/m^3.  This
!  function has inputs of Absolute Salinity and Conservative Temperature.
!  This function uses the computationally-efficient expression for 
!  specific volume in terms of SA, CT and p (Roquet et al., 2014).
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  CT  =  Conservative Temperature (ITS-90)                       [ deg C ]
!
!  sigma0  =  potential density anomaly with                     [ kg/m^3 ]
!             respect to a reference pressure of 0 dbar,   
!             that is, this potential density - 1000 kg/m^3.
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : gsw_sfac, offset

use gsw_mod_specvol_coefficients

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct

real (r8) :: gsw_sigma0

real (r8) :: vp0, xs, ys

xs = sqrt(gsw_sfac*sa + offset)
ys = ct*0.025_r8

vp0 = v000 + xs*(v010 + xs*(v020 + xs*(v030 + xs*(v040 + xs*(v050 &
    + v060*xs))))) + ys*(v100 + xs*(v110 + xs*(v120 + xs*(v130 + xs*(v140 &
    + v150*xs)))) + ys*(v200 + xs*(v210 + xs*(v220 + xs*(v230 + v240*xs))) &
    + ys*(v300 + xs*(v310 + xs*(v320 + v330*xs)) + ys*(v400 + xs*(v410 &
    + v420*xs) + ys*(v500 + v510*xs + v600*ys)))))

gsw_sigma0 = 1.0_r8/vp0 - 1000.0_r8

return
end function

!--------------------------------------------------------------------------
