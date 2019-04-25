!==========================================================================
elemental function gsw_pt0_cold_ice_poly (pot_enthalpy_ice)
!==========================================================================
!
!  Calculates an initial estimate of pt0_ice when it is less than about
!  -100 deg C. 
!
!  pot_enthalpy_ice  =  potential enthalpy of ice                  [ J/kg ]
!
!  pt0_cold_ice_poly  =  initial estimate of potential temperatur 
!                        of very cold ice in dgress C (not K)     [ deg C ] 
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : gsw_t0

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: pot_enthalpy_ice

real (r8) :: gsw_pt0_cold_ice_poly

real (r8) :: log_abs_theta0, log_h_diff

! h00 = gsw_enthalpy_ice(-gsw_t0,0)
real (r8), parameter :: h00 = -6.320202333358860e5_r8

real (r8), parameter :: s0 =  1.493103204647916_r8
real (r8), parameter :: s1 =  2.372788609320607e-1_r8
real (r8), parameter :: s2 = -2.014996002119374e-3_r8
real (r8), parameter :: s3 =  2.640600197732682e-6_r8
real (r8), parameter :: s4 =  3.134706016844293e-5_r8
real (r8), parameter :: s5 =  2.733592344937913e-6_r8
real (r8), parameter :: s6 =  4.726828010223258e-8_r8
real (r8), parameter :: s7 = -2.735193883189589e-9_r8
real (r8), parameter :: s8 = -8.547714991377670e-11_r8

log_h_diff = log(pot_enthalpy_ice - h00)

log_abs_theta0 = s0 + log_h_diff*(s1 + log_h_diff*(s2 + log_h_diff*(s3 &
                + log_h_diff*(s4 + log_h_diff*(s5 + log_h_diff*(s6 &
                + log_h_diff*(s7 + log_h_diff*s8)))))))

gsw_pt0_cold_ice_poly = exp(log_abs_theta0) - gsw_t0
    
return
end function

!--------------------------------------------------------------------------
