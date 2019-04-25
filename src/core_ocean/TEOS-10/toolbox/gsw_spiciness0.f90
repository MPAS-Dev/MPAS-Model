!==========================================================================
elemental function gsw_spiciness0 (sa, ct)
!==========================================================================
! 
!  Calculates spiciness from Absolute Salinity and Conservative 
!  Temperature at a pressure of 0 dbar, as described by McDougall and 
!  Krzysik (2015).  This routine is based on the computationally-efficient 
!  expression for specific volume in terms of SA, CT and p (Roquet et al., 
!  2015).
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  CT  =  Conservative Temperature (ITS-90)                       [ deg C ]
!
!  spiciness0  =  spiciness referenced to a pressure of 0 dbar, 
!                 i.e. the surface                             [ kg/m^3 ]
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : gsw_sfac, offset

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct

real (r8) :: gsw_spiciness0

real (r8) :: xs, ys

real (r8), parameter :: s01 = -9.22982898371678e1_r8
real (r8), parameter :: s02 = -1.35727873628866e1_r8
real (r8), parameter :: s03 =  1.87353650994010e1_r8
real (r8), parameter :: s04 = -1.61360047373455e1_r8
real (r8), parameter :: s05 =  3.76112762286425e1_r8
real (r8), parameter :: s06 = -4.27086671461257e1_r8
real (r8), parameter :: s07 =  2.00820111041594e1_r8
real (r8), parameter :: s08 =  2.87969717584045e2_r8
real (r8), parameter :: s09 =  1.13747111959674e1_r8
real (r8), parameter :: s10 =  6.07377192990680e1_r8
real (r8), parameter :: s11 = -7.37514033570187e1_r8
real (r8), parameter :: s12 = -7.51171878953574e1_r8
real (r8), parameter :: s13 =  1.63310989721504e2_r8
real (r8), parameter :: s14 = -8.83222751638095e1_r8
real (r8), parameter :: s15 = -6.41725302237048e2_r8
real (r8), parameter :: s16 =  2.79732530789261e1_r8
real (r8), parameter :: s17 = -2.49466901993728e2_r8
real (r8), parameter :: s18 =  3.26691295035416e2_r8
real (r8), parameter :: s19 =  2.66389243708181e1_r8
real (r8), parameter :: s20 = -2.93170905757579e2_r8
real (r8), parameter :: s21 =  1.76053907144524e2_r8
real (r8), parameter :: s22 =  8.27634318120224e2_r8
real (r8), parameter :: s23 = -7.02156220126926e1_r8
real (r8), parameter :: s24 =  3.82973336590803e2_r8
real (r8), parameter :: s25 = -5.06206828083959e2_r8
real (r8), parameter :: s26 =  6.69626565169529e1_r8
real (r8), parameter :: s27 =  3.02851235050766e2_r8
real (r8), parameter :: s28 = -1.96345285604621e2_r8
real (r8), parameter :: s29 = -5.74040806713526e2_r8
real (r8), parameter :: s30 =  7.03285905478333e1_r8
real (r8), parameter :: s31 = -2.97870298879716e2_r8
real (r8), parameter :: s32 =  3.88340373735118e2_r8
real (r8), parameter :: s33 = -8.29188936089122e1_r8
real (r8), parameter :: s34 = -1.87602137195354e2_r8
real (r8), parameter :: s35 =  1.27096944425793e2_r8
real (r8), parameter :: s36 =  2.11671167892147e2_r8
real (r8), parameter :: s37 = -3.15140919876285e1_r8
real (r8), parameter :: s38 =  1.16458864953602e2_r8
real (r8), parameter :: s39 = -1.50029730802344e2_r8
real (r8), parameter :: s40 =  3.76293848660589e1_r8
real (r8), parameter :: s41 =  6.47247424373200e1_r8
real (r8), parameter :: s42 = -4.47159994408867e1_r8
real (r8), parameter :: s43 = -3.23533339449055e1_r8
real (r8), parameter :: s44 =  5.30648562097667_r8
real (r8), parameter :: s45 = -1.82051249177948e1_r8
real (r8), parameter :: s46 =  2.33184351090495e1_r8
real (r8), parameter :: s47 = -6.22909903460368_r8
real (r8), parameter :: s48 = -9.55975464301446_r8
real (r8), parameter :: s49 =  6.61877073960113_r8
 
xs = sqrt(gsw_sfac*sa + offset)
ys = ct*0.025

gsw_spiciness0 = &
          s01 + ys*(s02 + ys*(s03 + ys*(s04 + ys*(s05 + ys*(s06 + s07*ys))))) &
    + xs*(s08 + ys*(s09 + ys*(s10 + ys*(s11 + ys*(s12 + ys*(s13 + s14*ys))))) &
    + xs*(s15 + ys*(s16 + ys*(s17 + ys*(s18 + ys*(s19 + ys*(s20 + s21*ys))))) &
    + xs*(s22 + ys*(s23 + ys*(s24 + ys*(s25 + ys*(s26 + ys*(s27 + s28*ys))))) &
    + xs*(s29 + ys*(s30 + ys*(s31 + ys*(s32 + ys*(s33 + ys*(s34 + s35*ys))))) &
    + xs*(s36 + ys*(s37 + ys*(s38 + ys*(s39 + ys*(s40 + ys*(s41 + s42*ys))))) &
    + xs*(s43 + ys*(s44 + ys*(s45 + ys*(s46 + ys*(s47 + ys*(s48 + s49*ys)))))))))))

return
end function

!--------------------------------------------------------------------------
