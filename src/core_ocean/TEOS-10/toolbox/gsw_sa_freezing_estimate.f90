!==========================================================================
elemental function gsw_sa_freezing_estimate (p, saturation_fraction, ct, t)
!==========================================================================
!
! Form an estimate of SA from a polynomial in CT and p 
!
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : gsw_sso

use gsw_mod_toolbox, only : gsw_ct_from_t

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: p, saturation_fraction
real (r8), intent(in), optional :: ct, t

real (r8) :: gsw_sa_freezing_estimate

real (r8) :: ctx, ctsat, sa

! note that aa = 0.502500117621d0/35.16504
real (r8), parameter :: aa = 0.014289763856964_r8
real (r8), parameter :: bb = 0.057000649899720_r8

real (r8), parameter :: p0  =  2.570124672768757e-1_r8
real (r8), parameter :: p1  = -1.917742353032266e1_r8
real (r8), parameter :: p2  = -1.413382858617969e-2_r8
real (r8), parameter :: p3  = -5.427484830917552e-1_r8
real (r8), parameter :: p4  = -4.126621135193472e-4_r8
real (r8), parameter :: p5  = -4.176407833276121e-7_r8
real (r8), parameter :: p6  =  4.688217641883641e-5_r8
real (r8), parameter :: p7  = -3.039808885885726e-8_r8
real (r8), parameter :: p8  = -4.990118091261456e-11_r8
real (r8), parameter :: p9  = -9.733920711119464e-9_r8
real (r8), parameter :: p10 = -7.723324202726337e-12_r8
real (r8), parameter :: p11 =  7.121854166249257e-16_r8
real (r8), parameter :: p12 =  1.256474634100811e-12_r8
real (r8), parameter :: p13 =  2.105103897918125e-15_r8
real (r8), parameter :: p14 =  8.663811778227171e-19_r8

! A very rough estimate of sa to get the saturated ct
if (present(ct)) then
    sa = max(-(ct + 9e-4_r8*p)/0.06_r8, 0.0_r8)
    ctx = ct
else if (present(t)) then
    sa = max(-(t + 9e-4_r8*p)/0.06_r8, 0.0_r8)
    ctx = gsw_ct_from_t(sa,t,p)
else
    gsw_sa_freezing_estimate = 0.0_r8
    return
end if

! CTsat is the estimated value of CT if the seawater were saturated with
! dissolved air, recognizing that it actually has the air fraction
! saturation_fraction; see McDougall, Barker and Feistel, 2014).  

ctsat = ctx - (1.0_r8-saturation_fraction)* &
        (1e-3_r8)*(2.4_r8-aa*sa)*(1.0_r8+bb*(1.0_r8-sa/gsw_sso))

gsw_sa_freezing_estimate = p0 &
        + p*(p2 + p4*ctsat + p*(p5 + ctsat*(p7 + p9*ctsat) &
        + p*(p8  + ctsat*(p10 + p12*ctsat) + p*(p11 + p13*ctsat + p14*p)))) &
        + ctsat*(p1 + ctsat*(p3 + p6*p))

return
end function

!--------------------------------------------------------------------------
