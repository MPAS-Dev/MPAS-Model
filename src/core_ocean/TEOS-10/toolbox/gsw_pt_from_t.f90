!==========================================================================
elemental function gsw_pt_from_t (sa, t, p, p_ref) 
!==========================================================================
!   
! Calculates potential temperature of seawater from in-situ temperature 
!
! sa     : Absolute Salinity                               [g/kg]
! t      : in-situ temperature                             [deg C]
! p      : sea pressure                                    [dbar]
! p_ref  : reference sea pressure                          [dbar]
!
! gsw_pt_from_t : potential temperature                    [deg C]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_entropy_part, gsw_gibbs

use gsw_mod_teos10_constants, only : gsw_cp0, gsw_sso, gsw_t0, gsw_ups

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, t, p, p_ref 

real (r8) :: gsw_pt_from_t

integer no_iter
real (r8) :: s1, pt, pt_old, dentropy, dentropy_dt
real (r8) :: true_entropy_part, ptm

integer, parameter :: n0=0, n2=2

s1 = sa/gsw_ups

pt = t + (p-p_ref)*( 8.65483913395442e-6_r8 - &
               s1 *  1.41636299744881e-6_r8 - &
         (p+p_ref)*  7.38286467135737e-9_r8 + &
               t  *(-8.38241357039698e-6_r8 + &
               s1 *  2.83933368585534e-8_r8 + &
               t  *  1.77803965218656e-8_r8 + &
         (p+p_ref)*  1.71155619208233e-10_r8))

dentropy_dt = gsw_cp0/((gsw_t0 + pt)*(1.0_r8 - 0.05_r8*(1.0_r8 - sa/gsw_sso)))

true_entropy_part = gsw_entropy_part(sa,t,p)

do no_iter = 1, 2
    pt_old = pt
    dentropy = gsw_entropy_part(sa,pt_old,p_ref) - true_entropy_part
    pt = pt_old - dentropy/dentropy_dt 
    ptm = 0.5_r8*(pt + pt_old)
    dentropy_dt = -gsw_gibbs(n0,n2,n0,sa,ptm,p_ref)
    pt = pt_old - dentropy/dentropy_dt
end do

gsw_pt_from_t = pt

return
end function

!--------------------------------------------------------------------------
