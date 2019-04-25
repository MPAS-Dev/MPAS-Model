!==========================================================================
elemental function gsw_latentheat_evap_ct (sa, ct) 
!==========================================================================
!
! Calculates latent heat, or enthalpy, of evaporation.
!
! sa     : Absolute Salinity                               [g/kg]
! ct     : Conservative Temperature                        [deg C]
! 
! gsw_latentheat_evaporation : latent heat of evaporation  [J/kg]
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : gsw_sfac

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct 

real (r8) :: gsw_latentheat_evap_ct

real (r8), parameter :: c0 =   2.499065844825125e6_r8
real (r8), parameter :: c1 =  -1.544590633515099e-1_r8
real (r8), parameter :: c2 =  -9.096800915831875e4_r8
real (r8), parameter :: c3 =   1.665513670736000e2_r8
real (r8), parameter :: c4 =   4.589984751248335e1_r8
real (r8), parameter :: c5 =   1.894281502222415e1_r8
real (r8), parameter :: c6 =   1.192559661490269e3_r8
real (r8), parameter :: c7 =  -6.631757848479068e3_r8
real (r8), parameter :: c8 =  -1.104989199195898e2_r8
real (r8), parameter :: c9 =  -1.207006482532330e3_r8
real (r8), parameter :: c10 = -3.148710097513822e3_r8
real (r8), parameter :: c11 =  7.437431482069087e2_r8
real (r8), parameter :: c12 =  2.519335841663499e3_r8
real (r8), parameter :: c13 =  1.186568375570869e1_r8
real (r8), parameter :: c14 =  5.731307337366114e2_r8
real (r8), parameter :: c15 =  1.213387273240204e3_r8
real (r8), parameter :: c16 =  1.062383995581363e3_r8
real (r8), parameter :: c17 = -6.399956483223386e2_r8
real (r8), parameter :: c18 = -1.541083032068263e3_r8
real (r8), parameter :: c19 =  8.460780175632090e1_r8
real (r8), parameter :: c20 = -3.233571307223379e2_r8
real (r8), parameter :: c21 = -2.031538422351553e2_r8
real (r8), parameter :: c22 =  4.351585544019463e1_r8
real (r8), parameter :: c23 = -8.062279018001309e2_r8
real (r8), parameter :: c24 =  7.510134932437941e2_r8
real (r8), parameter :: c25 =  1.797443329095446e2_r8
real (r8), parameter :: c26 = -2.389853928747630e1_r8
real (r8), parameter :: c27 =  1.021046205356775e2_r8

real (r8) :: x, y

x = sqrt(gsw_sfac*sa)
y = ct/40.0_r8

gsw_latentheat_evap_ct = c0 + x*(c1 + c4*y + x*(c3   &
    + y*(c7 + c12*y) + x*(c6 + y*(c11 + y*(c17 + c24*y)) &
    + x*(c10 + y*(c16 + c23*y) + x*(c15 + c22*y + c21*x)))))  &
    + y*(c2 + y*(c5 + c8*x + y*(c9 + x*(c13 + c18*x)  &
    + y*(c14 + x*(c19 + c25*x) + y*(c20 + c26*x + c27*y)))))

return
end function

!--------------------------------------------------------------------------
