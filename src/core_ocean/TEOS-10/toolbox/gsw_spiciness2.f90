!==========================================================================
elemental function gsw_spiciness2 (sa, ct)
!==========================================================================
! 
!  Calculates spiciness from Absolute Salinity and Conservative 
!  Temperature at a pressure of 2000 dbar, as described by McDougall and 
!  Krzysik (2015).  This routine is based on the computationally-efficient 
!  expression for specific volume in terms of SA, CT and p (Roquet et al., 
!  2015).
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  CT  =  Conservative Temperature (ITS-90)                       [ deg C ]
!
!  spiciness2  =  spiciness referenced to a pressure of 2000 dbar 
!                                                                [ kg/m^3 ]
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : gsw_sfac, offset

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct

real (r8) :: gsw_spiciness2

real (r8) :: xs, ys

real (r8), parameter :: s01 = -9.17327320732265e1_r8
real (r8), parameter :: s02 = -1.31200235147912e1_r8
real (r8), parameter :: s03 =  2.49574345782503e1_r8
real (r8), parameter :: s04 = -2.41678075247398e1_r8
real (r8), parameter :: s05 =  3.61654631402053e1_r8
real (r8), parameter :: s06 = -3.22582164667710e1_r8
real (r8), parameter :: s07 =  1.45092623982509e1_r8
real (r8), parameter :: s08 =  2.87776645983195e2_r8
real (r8), parameter :: s09 =  3.13902307672447e1_r8
real (r8), parameter :: s10 =  1.69777467534459_r8
real (r8), parameter :: s11 = -5.69630115740438_r8
real (r8), parameter :: s12 = -7.97586359017987e1_r8
real (r8), parameter :: s13 =  1.07507460387751e2_r8
real (r8), parameter :: s14 = -5.58234404964787e1_r8
real (r8), parameter :: s15 = -6.41708068766557e2_r8
real (r8), parameter :: s16 = -2.53494801286161e1_r8
real (r8), parameter :: s17 = -9.86755437385364e1_r8
real (r8), parameter :: s18 =  1.52406930795842e2_r8
real (r8), parameter :: s19 =  4.23888258264105e1_r8
real (r8), parameter :: s20 = -1.60118811141438e2_r8
real (r8), parameter :: s21 =  9.67497898053989e1_r8
real (r8), parameter :: s22 =  8.27674355478637e2_r8
real (r8), parameter :: s23 =  5.27561234412133e-1_r8
real (r8), parameter :: s24 =  1.87440206992396e2_r8
real (r8), parameter :: s25 = -2.83295392345171e2_r8
real (r8), parameter :: s26 =  5.14485994597635e1_r8
real (r8), parameter :: s27 =  1.29975755062696e2_r8
real (r8), parameter :: s28 = -9.36526588377456e1_r8
real (r8), parameter :: s29 = -5.74911728972948e2_r8
real (r8), parameter :: s30 =  1.91175851862772e1_r8
real (r8), parameter :: s31 = -1.59347231968841e2_r8
real (r8), parameter :: s32 =  2.33884725744938e2_r8
real (r8), parameter :: s33 = -7.87744010546157e1_r8
real (r8), parameter :: s34 = -6.04757235443685e1_r8
real (r8), parameter :: s35 =  5.27869695599657e1_r8
real (r8), parameter :: s36 =  2.12517758478878e2_r8
real (r8), parameter :: s37 = -1.24351794740528e1_r8
real (r8), parameter :: s38 =  6.53904308937490e1_r8
real (r8), parameter :: s39 = -9.44804080763788e1_r8
real (r8), parameter :: s40 =  3.93874257887364e1_r8
real (r8), parameter :: s41 =  1.49425448888996e1_r8
real (r8), parameter :: s42 = -1.62350721656367e1_r8
real (r8), parameter :: s43 = -3.25936844276669e1_r8
real (r8), parameter :: s44 =  2.44035700301595_r8
real (r8), parameter :: s45 = -1.05079633683795e1_r8
real (r8), parameter :: s46 =  1.51515796259082e1_r8
real (r8), parameter :: s47 = -7.06609886460683_r8
real (r8), parameter :: s48 = -1.48043337052968_r8
real (r8), parameter :: s49 =  2.10066653978515_r8
 
xs = sqrt(gsw_sfac*sa + offset)
ys = ct*0.025

gsw_spiciness2 = &
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
