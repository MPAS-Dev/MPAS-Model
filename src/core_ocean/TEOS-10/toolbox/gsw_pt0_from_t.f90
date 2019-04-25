!==========================================================================
elemental function gsw_pt0_from_t (sa, t, p) 
!==========================================================================
!   
! Calculates potential temperature with reference pressure, p_ref = 0 dbar. 
!
! sa     : Absolute Salinity                               [g/kg]
! t      : in-situ temperature                             [deg C]
! p      : sea pressure                                    [dbar]
!
! gsw_pt0_from_t : potential temperature, p_ref = 0        [deg C]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_entropy_part, gsw_entropy_part_zerop
use gsw_mod_toolbox, only : gsw_gibbs_pt0_pt0

use gsw_mod_teos10_constants, only : gsw_cp0, gsw_sso, gsw_t0, gsw_ups

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, t, p 

real (r8) :: gsw_pt0_from_t

integer no_iter
real (r8) :: s1, true_entropy_part, pt0m
real (r8) :: pt0, pt0_old, dentropy, dentropy_dt

s1 = sa/gsw_ups

pt0 = t + p*( 8.65483913395442e-6_r8  - &
        s1 *  1.41636299744881e-6_r8  - &
         p *  7.38286467135737e-9_r8  + &
         t *(-8.38241357039698e-6_r8  + &
        s1 *  2.83933368585534e-8_r8  + &
         t *  1.77803965218656e-8_r8  + &
         p *  1.71155619208233e-10_r8))

dentropy_dt = gsw_cp0/((gsw_t0 + pt0)*(1.0_r8 - 0.05_r8*(1.0_r8 - sa/gsw_sso)))

true_entropy_part = gsw_entropy_part(sa,t,p)

do no_iter = 1, 2
    pt0_old = pt0
    dentropy = gsw_entropy_part_zerop(sa,pt0_old) - true_entropy_part
    pt0 = pt0_old - dentropy/dentropy_dt 
    pt0m = 0.5_r8*(pt0 + pt0_old)
    dentropy_dt = -gsw_gibbs_pt0_pt0(sa,pt0m)
    pt0 = pt0_old - dentropy/dentropy_dt
end do

gsw_pt0_from_t = pt0

return
end function

!--------------------------------------------------------------------------
