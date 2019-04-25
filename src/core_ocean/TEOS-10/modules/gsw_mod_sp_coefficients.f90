!==========================================================================
module gsw_mod_sp_coefficients
!==========================================================================
!
!--------------------------------------------------------------------------

use gsw_mod_kinds

implicit none

real (r8), parameter :: a0 =  0.0080_r8
real (r8), parameter :: a1 = -0.1692_r8
real (r8), parameter :: a2 = 25.3851_r8
real (r8), parameter :: a3 = 14.0941_r8
real (r8), parameter :: a4 = -7.0261_r8
real (r8), parameter :: a5 =  2.7081_r8

real (r8), parameter :: b0 =  0.0005_r8
real (r8), parameter :: b1 = -0.0056_r8
real (r8), parameter :: b2 = -0.0066_r8
real (r8), parameter :: b3 = -0.0375_r8
real (r8), parameter :: b4 =  0.0636_r8
real (r8), parameter :: b5 = -0.0144_r8

real (r8), parameter :: c0 =  0.6766097_r8
real (r8), parameter :: c1 =  2.00564e-2_r8
real (r8), parameter :: c2 =  1.104259e-4_r8
real (r8), parameter :: c3 = -6.9698e-7_r8
real (r8), parameter :: c4 =  1.0031e-9_r8

real (r8), parameter :: d1 =  3.426e-2_r8
real (r8), parameter :: d2 =  4.464e-4_r8
real (r8), parameter :: d3 =  4.215e-1_r8
real (r8), parameter :: d4 = -3.107e-3_r8

real (r8), parameter :: e1 =  2.070e-5_r8
real (r8), parameter :: e2 = -6.370e-10_r8
real (r8), parameter :: e3 =  3.989e-15_r8

real (r8), parameter :: k  =  0.0162_r8

end module

!--------------------------------------------------------------------------
