! =========================================================================
elemental function gsw_pt_from_entropy (sa, entropy)
! =========================================================================
!
!  Calculates potential temperature with reference pressure p_ref = 0 dbar 
!  and with entropy as an input variable. 
!
!  SA       =  Absolute Salinity                                   [ g/kg ]
!  entropy  =  specific entropy                                   [ deg C ]
!
!  pt   =  potential temperature                                  [ deg C ]
!          with reference sea pressure (p_ref) = 0 dbar.
!  Note. The reference sea pressure of the output, pt, is zero dbar.
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : gsw_cp0, gsw_sso, gsw_t0

use gsw_mod_toolbox, only : gsw_entropy_from_pt, gsw_gibbs_pt0_pt0

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, entropy

real (r8) :: gsw_pt_from_entropy

integer :: number_of_iterations
real (r8) :: c, dentropy, dentropy_dt, ent_sa, part1, part2, pt, ptm
real (r8) :: pt_old

! Find the initial value of pt
part1 = 1.0_r8 - sa/gsw_sso
part2 = 1.0_r8 - 0.05_r8*part1
ent_sa = (gsw_cp0/gsw_t0)*part1*(1.0_r8 - 1.01_r8*part1)
c = (entropy - ent_sa)*(part2/gsw_cp0)
pt = gsw_t0*(exp(c) - 1.0_r8)
dentropy_dt = gsw_cp0/((gsw_t0 + pt)*part2)

do number_of_iterations = 1, 2
    pt_old = pt
    dentropy = gsw_entropy_from_pt(sa,pt_old) - entropy
    pt = pt_old - dentropy/dentropy_dt
    ptm = 0.5_r8*(pt + pt_old)
    dentropy_dt = -gsw_gibbs_pt0_pt0(sa,ptm)
    pt = pt_old - dentropy/dentropy_dt
end do
    
! Maximum error of 2.2x10^-6 degrees C for one iteration.
! Maximum error is 1.4x10^-14 degrees C for two iterations 
! (two iterations is the default, "for Number_of_iterations = 1:2"). 

gsw_pt_from_entropy = pt

return
end function

!--------------------------------------------------------------------------
