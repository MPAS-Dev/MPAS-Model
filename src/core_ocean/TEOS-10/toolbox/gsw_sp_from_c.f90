!==========================================================================
elemental function gsw_sp_from_c (c, t, p)       
!==========================================================================
!
!  Calculates Practical Salinity, SP, from conductivity, C, primarily using
!  the PSS-78 algorithm.  Note that the PSS-78 algorithm for Practical 
!  Salinity is only valid in the range 2 < SP < 42.  If the PSS-78 
!  algorithm produces a Practical Salinity that is less than 2 then the 
!  Practical Salinity is recalculated with a modified form of the Hill et 
!  al. (1986) formula.  The modification of the Hill et al. (1986)
!  expression is to ensure that it is exactly consistent with PSS-78 
!  at SP = 2.  Note that the input values of conductivity need to be in 
!  units of mS/cm (not S/m). 
!
! c      : conductivity                                     [ mS/cm ]
! t      : in-situ temperature [ITS-90]                     [deg C]
! p      : sea pressure                                     [dbar]
!
! sp     : Practical Salinity                               [unitless]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_hill_ratio_at_sp2

use gsw_mod_error_functions, only : gsw_error_code, gsw_error_limit

use gsw_mod_teos10_constants, only : gsw_c3515

use gsw_mod_sp_coefficients

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: c, t, p       

real (r8) :: gsw_sp_from_c

real (r8) :: sp, t68, ft68, r, rt_lc, rp, rt, rtx
real (r8) :: hill_ratio, x, sqrty, part1, part2, sp_hill_raw

character (*), parameter :: func_name = "gsw_sp_from_c"

t68 = t*1.00024_r8
ft68 = (t68 - 15.0_r8)/(1.0_r8 + k*(t68 - 15.0_r8))

! The dimensionless conductivity ratio, R, is the conductivity input, C,
! divided by the present estimate of C(SP=35, t_68=15, p=0) which is 
! 42.9140 mS/cm (=4.29140 S/m), (Culkin and Smith, 1980). 

r = c/gsw_c3515

! rt_lc corresponds to rt as defined in the UNESCO 44 (1983) routines.  
rt_lc = c0 + (c1 + (c2 + (c3 + c4*t68)*t68)*t68)*t68
rp = 1.0_r8 + (p*(e1 + e2*p + e3*p*p))/(1.0_r8 + d1*t68 + d2*t68*t68 + (d3 + d4*t68)*r)
rt = r/(rp*rt_lc)  

if (rt .lt. 0.0_r8) then
    gsw_sp_from_c = gsw_error_code(1,func_name)
    return
endif

rtx = sqrt(rt)

sp = a0 + (a1 + (a2 + (a3 + (a4 + a5*rtx)*rtx)*rtx)*rtx)*rtx + &
    ft68*(b0 + (b1 + (b2 + (b3 + (b4 + b5*rtx)*rtx)*rtx)*rtx)*rtx)

! The following section of the code is designed for SP < 2 based on the
! Hill et al. (1986) algorithm.  This algorithm is adjusted so that it is
! exactly equal to the PSS-78 algorithm at SP = 2.

if (sp .lt. 2.0_r8) then
    hill_ratio = gsw_hill_ratio_at_sp2(t)
    x = 400.0_r8*rt
    sqrty = 10.0_r8*rtx
    part1 = 1.0_r8 + x*(1.5_r8 + x)
    part2 = 1.0_r8 + sqrty*(1.0_r8 + sqrty*(1.0_r8 + sqrty))
    sp_hill_raw = sp - a0/part1 - b0*ft68/part2
    sp = hill_ratio*sp_hill_raw
endif

if (sp .lt. 0.0_r8) then
    gsw_sp_from_c = gsw_error_code(2,func_name)
else
    gsw_sp_from_c = sp
end if

return
end function

!--------------------------------------------------------------------------
