!==========================================================================
elemental function gsw_spiciness1 (sa, ct)
!==========================================================================
! 
!  Calculates spiciness from Absolute Salinity and Conservative 
!  Temperature at a pressure of 1000 dbar, as described by McDougall and 
!  Krzysik (2015).  This routine is based on the computationally-efficient 
!  expression for specific volume in terms of SA, CT and p (Roquet et al., 
!  2015).
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  CT  =  Conservative Temperature (ITS-90)                       [ deg C ]
!
!  spiciness1  =  spiciness referenced to a pressure of 1000 dbar 
!                                                                [ kg/m^3 ]
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : gsw_sfac, offset

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct

real (r8) :: gsw_spiciness1

real (r8) :: xs, ys

real (r8), parameter :: s01 = -9.19874584868912e1_r8
real (r8), parameter :: s02 = -1.33517268529408e1_r8
real (r8), parameter :: s03 =  2.18352211648107e1_r8
real (r8), parameter :: s04 = -2.01491744114173e1_r8
real (r8), parameter :: s05 =  3.70004204355132e1_r8
real (r8), parameter :: s06 = -3.78831543226261e1_r8
real (r8), parameter :: s07 =  1.76337834294554e1_r8
real (r8), parameter :: s08 =  2.87838842773396e2_r8
real (r8), parameter :: s09 =  2.14531420554522e1_r8
real (r8), parameter :: s10 =  3.14679705198796e1_r8
real (r8), parameter :: s11 = -4.04398864750692e1_r8
real (r8), parameter :: s12 = -7.70796428950487e1_r8
real (r8), parameter :: s13 =  1.36783833820955e2_r8
real (r8), parameter :: s14 = -7.36834317044850e1_r8
real (r8), parameter :: s15 = -6.41753415180701e2_r8
real (r8), parameter :: s16 =  1.33701981685590_r8 
real (r8), parameter :: s17 = -1.75289327948412e2_r8
real (r8), parameter :: s18 =  2.42666160657536e2_r8
real (r8), parameter :: s19 =  3.17062400799114e1_r8
real (r8), parameter :: s20 = -2.28131490440865e2_r8
real (r8), parameter :: s21 =  1.39564245068468e2_r8
real (r8), parameter :: s22 =  8.27747934506435e2_r8
real (r8), parameter :: s23 = -3.50901590694775e1_r8
real (r8), parameter :: s24 =  2.87473907262029e2_r8
real (r8), parameter :: s25 = -4.00227341144928e2_r8
real (r8), parameter :: s26 =  6.48307189919433e1_r8
real (r8), parameter :: s27 =  2.16433334701578e2_r8
real (r8), parameter :: s28 = -1.48273032774305e2_r8
real (r8), parameter :: s29 = -5.74545648799754e2_r8
real (r8), parameter :: s30 =  4.50446431127421e1_r8
real (r8), parameter :: s31 = -2.30714981343772e2_r8
real (r8), parameter :: s32 =  3.15958389253065e2_r8
real (r8), parameter :: s33 = -8.60635313930106e1_r8
real (r8), parameter :: s34 = -1.22978455069097e2_r8
real (r8), parameter :: s35 =  9.18287282626261e1_r8
real (r8), parameter :: s36 =  2.12120473062203e2_r8
real (r8), parameter :: s37 = -2.21528216973820e1_r8
real (r8), parameter :: s38 =  9.19013417923270e1_r8
real (r8), parameter :: s39 = -1.24400776026014e2_r8
real (r8), parameter :: s40 =  4.08512871163839e1_r8
real (r8), parameter :: s41 =  3.91127352213516e1_r8
real (r8), parameter :: s42 = -3.10508021853093e1_r8
real (r8), parameter :: s43 = -3.24790035899152e1_r8
real (r8), parameter :: s44 =  3.91029016556786_r8
real (r8), parameter :: s45 = -1.45362719385412e1_r8
real (r8), parameter :: s46 =  1.96136194246355e1_r8
real (r8), parameter :: s47 = -7.06035474689088_r8
real (r8), parameter :: s48 = -5.36884688614009_r8
real (r8), parameter :: s49 =  4.43247303092448_r8
 
xs = sqrt(gsw_sfac*sa + offset)
ys = ct*0.025

gsw_spiciness1 = &
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
