!==========================================================================
module gsw_mod_freezing_poly_coefficients
!==========================================================================

use gsw_mod_kinds

implicit none

real (r8), parameter :: c0  =  0.017947064327968736_r8
real (r8), parameter :: c1 =  -6.076099099929818_r8
real (r8), parameter :: c2 =   4.883198653547851_r8
real (r8), parameter :: c3 =  -11.88081601230542_r8
real (r8), parameter :: c4 =   13.34658511480257_r8
real (r8), parameter :: c5 =  -8.722761043208607_r8
real (r8), parameter :: c6 =   2.082038908808201_r8
real (r8), parameter :: c7 =  -7.389420998107497_r8
real (r8), parameter :: c8 =  -2.110913185058476_r8
real (r8), parameter :: c9 =   0.2295491578006229_r8 
real (r8), parameter :: c10 = -0.9891538123307282_r8
real (r8), parameter :: c11 = -0.08987150128406496_r8
real (r8), parameter :: c12 =  0.3831132432071728_r8
real (r8), parameter :: c13 =  1.054318231187074_r8
real (r8), parameter :: c14 =  1.065556599652796_r8
real (r8), parameter :: c15 = -0.7997496801694032_r8
real (r8), parameter :: c16 =  0.3850133554097069_r8
real (r8), parameter :: c17 = -2.078616693017569_r8
real (r8), parameter :: c18 =  0.8756340772729538_r8
real (r8), parameter :: c19 = -2.079022768390933_r8
real (r8), parameter :: c20 =  1.596435439942262_r8
real (r8), parameter :: c21 =  0.1338002171109174_r8
real (r8), parameter :: c22 =  1.242891021876471_r8

! Note that a = 0.502500117621_r8/gsw_sso
real (r8), parameter :: a = 0.014289763856964_r8
real (r8), parameter :: b = 0.057000649899720_r8

real (r8), parameter :: t0 = 0.002519_r8
real (r8), parameter :: t1 = -5.946302841607319_r8
real (r8), parameter :: t2 =  4.136051661346983_r8
real (r8), parameter :: t3 = -1.115150523403847e1_r8
real (r8), parameter :: t4 =  1.476878746184548e1_r8
real (r8), parameter :: t5 = -1.088873263630961e1_r8
real (r8), parameter :: t6 =  2.961018839640730_r8
real (r8), parameter :: t7 = -7.433320943962606_r8
real (r8), parameter :: t8 = -1.561578562479883_r8
real (r8), parameter :: t9 =  4.073774363480365e-2_r8
real (r8), parameter :: t10 =  1.158414435887717e-2_r8
real (r8), parameter :: t11 = -4.122639292422863e-1_r8
real (r8), parameter :: t12 = -1.123186915628260e-1_r8
real (r8), parameter :: t13 =  5.715012685553502e-1_r8
real (r8), parameter :: t14 =  2.021682115652684e-1_r8
real (r8), parameter :: t15 =  4.140574258089767e-2_r8
real (r8), parameter :: t16 = -6.034228641903586e-1_r8
real (r8), parameter :: t17 = -1.205825928146808e-2_r8
real (r8), parameter :: t18 = -2.812172968619369e-1_r8
real (r8), parameter :: t19 =  1.877244474023750e-2_r8
real (r8), parameter :: t20 = -1.204395563789007e-1_r8
real (r8), parameter :: t21 =  2.349147739749606e-1_r8
real (r8), parameter :: t22 =  2.748444541144219e-3_r8

end module

!--------------------------------------------------------------------------
