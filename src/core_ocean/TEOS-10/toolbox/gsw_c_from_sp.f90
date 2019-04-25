!==========================================================================
elemental function gsw_c_from_sp (sp, t, p)       
!==========================================================================
!
!  Calculates conductivity, C, from (SP,t,p) using PSS-78 in the range 
!  2 < SP < 42.  If the input Practical Salinity is less than 2 then a 
!  modified form of the Hill et al. (1986) fomula is used for Practical 
!  Salinity.  The modification of the Hill et al. (1986) expression is to
!  ensure that it is exactly consistent with PSS-78 at SP = 2.
!
!  The conductivity ratio returned by this function is consistent with the
!  input value of Practical Salinity, SP, to 2x10^-14 psu over the full 
!  range of input parameters (from pure fresh water up to SP = 42 psu).  
!  This error of 2x10^-14 psu is machine precision at typical seawater 
!  salinities.  This accuracy is achieved by having four different 
!  polynomials for the starting value of Rtx (the square root of Rt) in 
!  four different ranges of SP, and by using one and a half iterations of 
!  a computationally efficient modified Newton-Raphson technique (McDougall 
!  and Wotherspoon, 2012) to find the root of the equation.  
!
!  Note that strictly speaking PSS-78 (Unesco, 1983) defines Practical
!  Salinity in terms of the conductivity ratio, R, without actually
!  specifying the value of C(35,15,0) (which we currently take to be
!  42.9140 mS/cm).
!
! sp     : Practical Salinity                               [unitless]
! t      : in-situ temperature [ITS-90]                     [deg C]
! p      : sea pressure                                     [dbar]
!
! c      : conductivity                                     [ mS/cm ]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_hill_ratio_at_sp2

use gsw_mod_teos10_constants, only : gsw_c3515

use gsw_mod_sp_coefficients

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sp, t, p       

real (r8) :: gsw_c_from_sp

real (r8) :: t68, ft68, x, rtx, dsp_drtx, sqrty
real (r8) :: part1, part2, hill_ratio, sp_hill_raw, sp_est
real (r8) :: rtx_old, rt, aa, bb, cc, dd, ee, ra,r, rt_lc, rtxm

real (r8), parameter :: p0  =  4.577801212923119e-3_r8
real (r8), parameter :: p1  =  1.924049429136640e-1_r8
real (r8), parameter :: p2  =  2.183871685127932e-5_r8
real (r8), parameter :: p3  = -7.292156330457999e-3_r8
real (r8), parameter :: p4  =  1.568129536470258e-4_r8
real (r8), parameter :: p5  = -1.478995271680869e-6_r8
real (r8), parameter :: p6  =  9.086442524716395e-4_r8
real (r8), parameter :: p7  = -1.949560839540487e-5_r8
real (r8), parameter :: p8  = -3.223058111118377e-6_r8
real (r8), parameter :: p9  =  1.175871639741131e-7_r8
real (r8), parameter :: p10 = -7.522895856600089e-5_r8
real (r8), parameter :: p11 = -2.254458513439107e-6_r8
real (r8), parameter :: p12 =  6.179992190192848e-7_r8
real (r8), parameter :: p13 =  1.005054226996868e-8_r8
real (r8), parameter :: p14 = -1.923745566122602e-9_r8
real (r8), parameter :: p15 =  2.259550611212616e-6_r8
real (r8), parameter :: p16 =  1.631749165091437e-7_r8
real (r8), parameter :: p17 = -5.931857989915256e-9_r8
real (r8), parameter :: p18 = -4.693392029005252e-9_r8
real (r8), parameter :: p19 =  2.571854839274148e-10_r8
real (r8), parameter :: p20 =  4.198786822861038e-12_r8

real (r8), parameter :: q0  =  5.540896868127855e-5_r8
real (r8), parameter :: q1  =  2.015419291097848e-1_r8
real (r8), parameter :: q2  = -1.445310045430192e-5_r8 
real (r8), parameter :: q3  = -1.567047628411722e-2_r8
real (r8), parameter :: q4  =  2.464756294660119e-4_r8
real (r8), parameter :: q5  = -2.575458304732166e-7_r8
real (r8), parameter :: q6  =  5.071449842454419e-3_r8
real (r8), parameter :: q7  =  9.081985795339206e-5_r8
real (r8), parameter :: q8  = -3.635420818812898e-6_r8
real (r8), parameter :: q9  =  2.249490528450555e-8_r8
real (r8), parameter :: q10 = -1.143810377431888e-3_r8
real (r8), parameter :: q11 =  2.066112484281530e-5_r8
real (r8), parameter :: q12 =  7.482907137737503e-7_r8
real (r8), parameter :: q13 =  4.019321577844724e-8_r8
real (r8), parameter :: q14 = -5.755568141370501e-10_r8
real (r8), parameter :: q15 =  1.120748754429459e-4_r8
real (r8), parameter :: q16 = -2.420274029674485e-6_r8
real (r8), parameter :: q17 = -4.774829347564670e-8_r8
real (r8), parameter :: q18 = -4.279037686797859e-9_r8
real (r8), parameter :: q19 = -2.045829202713288e-10_r8
real (r8), parameter :: q20 =  5.025109163112005e-12_r8

real (r8), parameter :: s0  =  3.432285006604888e-3_r8
real (r8), parameter :: s1  =  1.672940491817403e-1_r8
real (r8), parameter :: s2  =  2.640304401023995e-5_r8
real (r8), parameter :: s3  =  1.082267090441036e-1_r8
real (r8), parameter :: s4  = -6.296778883666940e-5_r8
real (r8), parameter :: s5  = -4.542775152303671e-7_r8
real (r8), parameter :: s6  = -1.859711038699727e-1_r8
real (r8), parameter :: s7  =  7.659006320303959e-4_r8
real (r8), parameter :: s8  = -4.794661268817618e-7_r8
real (r8), parameter :: s9  =  8.093368602891911e-9_r8
real (r8), parameter :: s10 =  1.001140606840692e-1_r8 
real (r8), parameter :: s11 = -1.038712945546608e-3_r8
real (r8), parameter :: s12 = -6.227915160991074e-6_r8
real (r8), parameter :: s13 =  2.798564479737090e-8_r8
real (r8), parameter :: s14 = -1.343623657549961e-10_r8
real (r8), parameter :: s15 =  1.024345179842964e-2_r8
real (r8), parameter :: s16 =  4.981135430579384e-4_r8
real (r8), parameter :: s17 =  4.466087528793912e-6_r8
real (r8), parameter :: s18 =  1.960872795577774e-8_r8
real (r8), parameter :: s19 = -2.723159418888634e-10_r8
real (r8), parameter :: s20 =  1.122200786423241e-12_r8

real (r8), parameter :: u0  =  5.180529787390576e-3_r8
real (r8), parameter :: u1  =  1.052097167201052e-3_r8
real (r8), parameter :: u2  =  3.666193708310848e-5_r8
real (r8), parameter :: u3  =  7.112223828976632_r8
real (r8), parameter :: u4  = -3.631366777096209e-4_r8
real (r8), parameter :: u5  = -7.336295318742821e-7_r8
real (r8), parameter :: u6  = -1.576886793288888e+2_r8
real (r8), parameter :: u7  = -1.840239113483083e-3_r8
real (r8), parameter :: u8  =  8.624279120240952e-6_r8
real (r8), parameter :: u9  =  1.233529799729501e-8_r8
real (r8), parameter :: u10 =  1.826482800939545e+3_r8
real (r8), parameter :: u11 =  1.633903983457674e-1_r8
real (r8), parameter :: u12 = -9.201096427222349e-5_r8
real (r8), parameter :: u13 = -9.187900959754842e-8_r8
real (r8), parameter :: u14 = -1.442010369809705e-10_r8
real (r8), parameter :: u15 = -8.542357182595853e+3_r8
real (r8), parameter :: u16 = -1.408635241899082_r8
real (r8), parameter :: u17 =  1.660164829963661e-4_r8
real (r8), parameter :: u18 =  6.797409608973845e-7_r8
real (r8), parameter :: u19 =  3.345074990451475e-10_r8
real (r8), parameter :: u20 =  8.285687652694768e-13_r8

t68 = t*1.00024_r8
ft68 = (t68 - 15.0_r8)/(1.0_r8 + k*(t68 - 15.0_r8))

x = sqrt(sp)

!--------------------------------------------------------------------------
! Finding the starting value of Rtx, the square root of Rt, using four 
! different polynomials of SP and t68.  
!--------------------------------------------------------------------------

if (sp.ge.9.0_r8) then

    rtx = p0 + x*(p1 + p4*t68 + x*(p3 + p7*t68 + x*(p6  &
        + p11*t68 + x*(p10 + p16*t68 + x*p15))))  &
        + t68*(p2+ t68*(p5 + x*x*(p12 + x*p17) + p8*x  &
        + t68*(p9 + x*(p13 + x*p18)+ t68*(p14 + p19*x + p20*t68))))

else if (sp.ge.0.25_r8.and.sp.lt.9.0_r8) then

    rtx = q0 + x*(q1 + q4*t68 + x*(q3 + q7*t68 + x*(q6  &
        + q11*t68 + x*(q10 + q16*t68 + x*q15))))  &
        + t68*(q2+ t68*(q5 + x*x*(q12 + x*q17) + q8*x  &
        + t68*(q9 + x*(q13 + x*q18)+ t68*(q14 + q19*x + q20*t68))))

else if (sp.ge.0.003_r8.and.sp.lt.0.25_r8) then

    rtx = s0 + x*(s1 + s4*t68 + x*(s3 + s7*t68 + x*(s6  &
        + s11*t68 + x*(s10 + s16*t68 + x*s15))))  &
        + t68*(s2+ t68*(s5 + x*x*(s12 + x*s17) + s8*x  &
        + t68*(s9 + x*(s13 + x*s18)+ t68*(s14 + s19*x + s20*t68))))

else

    rtx = u0 + x*(u1 + u4*t68 + x*(u3 + u7*t68 + x*(u6  &
        + u11*t68 + x*(u10 + u16*t68 + x*u15))))  &
        + t68*(u2+ t68*(u5 + x*x*(u12 + x*u17) + u8*x  &
        + t68*(u9 + x*(u13 + x*u18)+ t68*(u14 + u19*x + u20*t68))))

end if

!--------------------------------------------------------------------------
! Finding the starting value of dSP_dRtx, the derivative of SP with respect
! to Rtx.  
!--------------------------------------------------------------------------
dsp_drtx =  a1 + (2.0_r8*a2 + (3.0_r8*a3 + &
                              (4.0_r8*a4 + 5.0_r8*a5*rtx)*rtx)*rtx)*rtx  &
    + ft68*(b1 + (2.0_r8*b2 + (3.0_r8*b3 + &
                              (4.0_r8*b4 + 5.0_r8*b5*rtx)*rtx)*rtx)*rtx)

if (sp.lt.2.0_r8) then
    x = 400.0_r8*(rtx*rtx)
    sqrty = 10.0_r8*rtx
    part1 = 1.0_r8 + x*(1.5_r8 + x) 
    part2 = 1.0_r8 + sqrty*(1.0_r8 + sqrty*(1.0_r8 + sqrty))
    hill_ratio = gsw_hill_ratio_at_sp2(t)
    dsp_drtx = dsp_drtx  &
        + a0*800.0_r8*Rtx*(1.5_r8 + 2.0_r8*x)/(part1*part1)  &
        + b0*ft68*(10.0_r8 + sqrty*(20.0_r8 + 30.0_r8*sqrty))/(part2*part2)
    dsp_drtx = hill_ratio*dsp_drtx
end if

!--------------------------------------------------------------------------
! One iteration through the modified Newton-Raphson method (McDougall and 
! Wotherspoon, 2012) achieves an error in Practical Salinity of about 
! 10^-12 for all combinations of the inputs.  One and a half iterations of 
! the modified Newton-Raphson method achevies a maximum error in terms of 
! Practical Salinity of better than 2x10^-14 everywhere. 
!
! We recommend one and a half iterations of the modified Newton-Raphson
! method. 
!
! Begin the modified Newton-Raphson method.  
!--------------------------------------------------------------------------
    sp_est = a0 + (a1 + (a2 + (a3 + (a4 + a5*rtx)*rtx)*rtx)*rtx)*rtx &
        + ft68*(b0 + (b1 + (b2+ (b3 + (b4 + b5*rtx)*rtx)*rtx)*rtx)*rtx)
    if (sp_est .lt. 2.0_r8) then
        x = 400.0_r8*(rtx*rtx)
        sqrty = 10.0_r8*rtx
        part1 = 1.0_r8 + x*(1.5_r8 + x) 
        part2 = 1.0_r8 + sqrty*(1.0_r8 + sqrty*(1.0_r8 + sqrty))
        sp_hill_raw = sp_est - a0/part1 - b0*ft68/part2
        hill_ratio = gsw_hill_ratio_at_sp2(t)
        sp_est = hill_ratio*sp_hill_raw
    end if
 
    rtx_old = rtx
    rtx = rtx_old - (sp_est - sp)/dsp_drtx
    
    rtxm = 0.5_r8*(rtx + rtx_old)      ! This mean value of Rtx, Rtxm, is the  
!                 value of Rtx at which the derivative dSP_dRtx is evaluated.
    
    dsp_drtx = a1 + (2.0_r8*a2 + (3.0_r8*a3 + (4.0_r8*a4 + &
                     5.0_r8*a5*rtxm)*rtxm)*rtxm)*rtxm&
       + ft68*(b1 + (2.0_r8*b2 + (3.0_r8*b3 + (4.0_r8*b4 + &
                     5.0_r8*b5*rtxm)*rtxm)*rtxm)*rtxm)
    if (sp_est .lt. 2.0_r8) then
        x = 400.0_r8*(rtxm*rtxm)
        sqrty = 10.0_r8*rtxm
        part1 = 1.0_r8 + x*(1.5_r8 + x) 
        part2 = 1.0_r8 + sqrty*(1.0_r8 + sqrty*(1.0_r8 + sqrty))
        dsp_drtx = dsp_drtx  &
            + a0*800.0_r8*rtxm*(1.5_r8 + 2.0_r8*x)/(part1*part1)  &
            + b0*ft68*(10.0_r8 + sqrty*(20.0_r8 + 30.0_r8*sqrty))/(part2*part2)
        hill_ratio = gsw_hill_ratio_at_sp2(t)
        dsp_drtx = hill_ratio*dsp_drtx
    end if

!--------------------------------------------------------------------------
! The line below is where Rtx is updated at the end of the one full 
! iteration of the modified Newton-Raphson technique.
!--------------------------------------------------------------------------
    rtx = rtx_old - (sp_est - sp)/dsp_drtx
!--------------------------------------------------------------------------
! Now we do another half iteration of the modified Newton-Raphson  
! technique, making a total of one and a half modified N-R iterations.
!-------------------------------------------------------------------------- 
    sp_est = a0 + (a1 + (a2 + (a3 + (a4 + a5*rtx)*rtx)*rtx)*rtx)*rtx  &
        + ft68*(b0 + (b1 + (b2+ (b3 + (b4 + b5*rtx)*rtx)*rtx)*rtx)*rtx)
    if (sp_est .lt. 2.0_r8) then
        x = 400.0_r8*(rtx*rtx)
        sqrty = 10.0_r8*rtx
        part1 = 1.0_r8 + x*(1.5_r8 + x) 
        part2 = 1.0_r8 + sqrty*(1.0_r8 + sqrty*(1.0_r8 + sqrty))
        sp_hill_raw = sp_est - a0/part1 - b0*ft68/part2
        hill_ratio = gsw_hill_ratio_at_sp2(t)
        sp_est = hill_ratio*sp_hill_raw
    end if
    rtx = rtx - (sp_est - sp)/dsp_drtx

!--------------------------------------------------------------------------
! Now go from Rtx to Rt and then to the conductivity ratio R at pressure p.
!--------------------------------------------------------------------------
rt = rtx*rtx
aa  = d3 + d4*t68
bb  = 1.0_r8 + t68*(d1 + d2*t68)
cc  = p*(e1 + p*(e2 + e3*p))
! rt_lc (i.e. rt_lower_case) corresponds to rt as defined in 
! the UNESCO 44 (1983) routines.
rt_lc = c0 + (c1 + (c2 + (c3 + c4*t68)*t68)*t68)*t68

dd  = bb - aa*rt_lc*rt
ee  = rt_lc*rt*aa*(bb + cc)
ra = sqrt(dd*dd + 4.0_r8*ee) - dd
r  = 0.5_r8*ra/aa

! The dimensionless conductivity ratio, R, is the conductivity input, C,
! divided by the present estimate of C(SP=35, t_68=15, p=0) which is 
! 42.9140 mS/cm (=4.29140 S/m^). 

gsw_c_from_sp = gsw_c3515*r      

return
end function

!--------------------------------------------------------------------------
