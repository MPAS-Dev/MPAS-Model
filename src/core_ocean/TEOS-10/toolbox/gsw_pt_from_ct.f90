!==========================================================================
elemental function gsw_pt_from_ct (sa, ct) 
!==========================================================================
!
! potential temperature of seawater from conservative temperature
!
! sa     : Absolute Salinity                               [g/kg]
! ct     : Conservative Temperature                        [deg C]
! p      : sea pressure                                    [dbar]
!
! gsw_pt_from_ct : potential temperature with              [deg C]
!                  reference pressure of  0 dbar
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_ct_from_pt, gsw_gibbs_pt0_pt0

use gsw_mod_teos10_constants, only : gsw_cp0, gsw_ups, gsw_t0

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct 

real (r8) :: gsw_pt_from_ct

real (r8) :: a5ct, b3ct, ct_factor, pt_num, pt_recden, ct_diff
real (r8) :: pt, pt_old, ptm, dpt_dct, s1

real (r8), parameter :: a0 = -1.446013646344788e-2_r8
real (r8), parameter :: a1 = -3.305308995852924e-3_r8
real (r8), parameter :: a2 =  1.062415929128982e-4_r8
real (r8), parameter :: a3 =  9.477566673794488e-1_r8
real (r8), parameter :: a4 =  2.166591947736613e-3_r8
real (r8), parameter :: a5 =  3.828842955039902e-3_r8

real (r8), parameter :: b0 =  1.0_r8
real (r8), parameter :: b1 =  6.506097115635800e-4_r8
real (r8), parameter :: b2 =  3.830289486850898e-3_r8
real (r8), parameter :: b3 =  1.247811760368034e-6_r8

s1 = sa/gsw_ups

a5ct = a5*ct
b3ct = b3*ct

ct_factor = (a3 + a4*s1 + a5ct)
pt_num = a0 + s1*(a1 + a2*s1) + ct*ct_factor
pt_recden = 1.0_r8/(b0 + b1*s1 + ct*(b2 + b3ct))
pt = pt_num*pt_recden

dpt_dct = (ct_factor + a5ct - (b2 + b3ct + b3ct)*pt)*pt_recden

! Start the 1.5 iterations through the modified Newton-Rapshon iterative,
! method, which is also known as the Newton-McDougall method. 

ct_diff = gsw_ct_from_pt(sa,pt) - ct
pt_old = pt
pt = pt_old - ct_diff*dpt_dct
ptm = 0.5_r8*(pt + pt_old)

dpt_dct = -gsw_cp0/((ptm + gsw_t0)*gsw_gibbs_pt0_pt0(sa,ptm))

pt = pt_old - ct_diff*dpt_dct
ct_diff = gsw_ct_from_pt(sa,pt) - ct
pt_old = pt
gsw_pt_from_ct = pt_old - ct_diff*dpt_dct

return 
end function

!--------------------------------------------------------------------------
