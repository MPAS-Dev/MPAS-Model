!==========================================================================
module gsw_mod_gibbs_ice_coefficients
!==========================================================================

use gsw_mod_kinds

implicit none

complex(r8), parameter :: t1 =( 3.68017112855051e-2_r8, 5.10878114959572e-2_r8)
complex(r8), parameter :: t2 =( 3.37315741065416e-1_r8, 3.35449415919309e-1_r8)

complex(r8), parameter :: r1 =( 4.47050716285388e1_r8,  6.56876847463481e1_r8)
complex(r8), parameter :: r20=(-7.25974574329220e1_r8, -7.81008427112870e1_r8)
complex(r8), parameter :: r21=(-5.57107698030123e-5_r8, 4.64578634580806e-5_r8)
complex(r8), parameter :: r22=(2.34801409215913e-11_r8,-2.85651142904972e-11_r8)

! 1./Pt, where Pt = 611.657;  Experimental triple-point pressure in Pa.
real (r8), parameter :: rec_pt = 1.634903221903779e-3_r8

! Triple-point temperature, kelvin (K).
real (r8), parameter :: t3p = 273.16_r8
real (r8), parameter :: rec_t3p = 3.660858105139845e-3_r8   ! = 1/t3p

real (r8), parameter :: g00 = -6.32020233335886e5_r8
real (r8), parameter :: g01 =  6.55022213658955e-1_r8
real (r8), parameter :: g02 = -1.89369929326131e-8_r8
real (r8), parameter :: g03 =  3.3974612327105304e-15_r8
real (r8), parameter :: g04 = -5.564648690589909e-22_r8

end module

!--------------------------------------------------------------------------
