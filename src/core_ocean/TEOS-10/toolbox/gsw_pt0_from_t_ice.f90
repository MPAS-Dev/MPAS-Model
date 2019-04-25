! =========================================================================
elemental function gsw_pt0_from_t_ice (t, p)
! =========================================================================
!
!  Calculates potential temperature of ice Ih with a reference pressure of
!  0 dbar, from in-situ temperature, t.
!
!  t   =  in-situ temperature  (ITS-90)                           [ deg C ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar )
!
!  pt0_ice  =  potential temperature of ice Ih with reference pressure of
!              zero dbar (ITS-90)                                 [ deg C ]
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : gsw_t0

use gsw_mod_toolbox, only : gsw_gibbs_ice_part_t, gsw_gibbs_ice_pt0
use gsw_mod_toolbox, only : gsw_gibbs_ice_pt0_pt0

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: t, p

real (r8) :: gsw_pt0_from_t_ice

integer :: number_of_iterations
real (r8) :: dentropy, dentropy_dt, pt0_ice
real (r8) :: pt0_ice_old, ptm_ice, true_entropy

! This is the starting polynomial for pt0 of ice Ih.
real (r8), parameter :: s1 = -2.256611570832386e-4_r8
real (r8), parameter :: s2 = -6.045305921314694e-7_r8
real (r8), parameter :: s3 =  5.546699019612661e-9_r8
real (r8), parameter :: s4 =  1.795030639186685e-11_r8
real (r8), parameter :: s5 =  1.292346094030742e-9_r8

real (r8), parameter :: p1 = -2.259745637898635e-4_r8
real (r8), parameter :: p2 =  1.486236778150360e-9_r8
real (r8), parameter :: p3 =  6.257869607978536e-12_r8
real (r8), parameter :: p4 = -5.253795281359302e-7_r8
real (r8), parameter :: p5 =  6.752596995671330e-9_r8
real (r8), parameter :: p6 =  2.082992190070936e-11_r8
    
real (r8), parameter :: q1 = -5.849191185294459e-15_r8
real (r8), parameter :: q2 =  9.330347971181604e-11_r8
real (r8), parameter :: q3 =  3.415888886921213e-13_r8
real (r8), parameter :: q4 =  1.064901553161811e-12_r8
real (r8), parameter :: q5 = -1.454060359158787e-10_r8
real (r8), parameter :: q6 = -5.323461372791532e-13_r8

true_entropy = -gsw_gibbs_ice_part_t(t,p)

if (t.lt.-45.0_r8 .and. t.gt.-273.0_r8) then
    
    pt0_ice = t + p*(p1 + p*(p2 + p3*t) + t*(p4 + t*(p5 + p6*t)))
    
    if (pt0_ice.lt.-gsw_t0) pt0_ice = -gsw_t0
    
    ! we add 0.05d0 to the initial estimate of pt0_ice at
    ! temps less than -273 to ensure that it is never less than -273.15.
    if (pt0_ice.lt.-273.0_r8) pt0_ice = pt0_ice + 0.05_r8
    
    dentropy_dt = -gsw_gibbs_ice_pt0_pt0(pt0_ice)
        
    do number_of_iterations = 1, 3
        pt0_ice_old = pt0_ice
        dentropy = -gsw_gibbs_ice_pt0(pt0_ice_old) - true_entropy
        pt0_ice = pt0_ice_old - dentropy/dentropy_dt
        ptm_ice = 0.5_r8*(pt0_ice + pt0_ice_old)
        dentropy_dt = -gsw_gibbs_ice_pt0_pt0(ptm_ice)
        pt0_ice = pt0_ice_old - dentropy/dentropy_dt
    end do

else

    pt0_ice = t + p*(s1 + t*(s2 + t*(s3 + t*s4)) + s5*p)
    dentropy_dt = -gsw_gibbs_ice_pt0_pt0(pt0_ice)

    pt0_ice_old = pt0_ice
    dentropy = -gsw_gibbs_ice_pt0(pt0_ice_old) - true_entropy

    pt0_ice = pt0_ice_old - dentropy/dentropy_dt
    ptm_ice = 0.5_r8*(pt0_ice + pt0_ice_old)
    dentropy_dt = -gsw_gibbs_ice_pt0_pt0(ptm_ice)
    pt0_ice = pt0_ice_old - dentropy/dentropy_dt

end if
    
if (pt0_ice.lt.-273.0_r8) then
        
    pt0_ice = t + p*(q1 + p*(q2 + q3*t) + t*(q4 + t*(q5 + q6*t)))
        
    ! add 0.01d0 to the initial estimate of pt_ice used in the derivative to
    ! ensure that it is never less than -273.15d0 because the derivative
    ! approaches zero at absolute zero.
    dentropy_dt = -gsw_gibbs_ice_pt0_pt0(pt0_ice+0.01_r8)

    do number_of_iterations = 1, 3
        pt0_ice_old = pt0_ice
        dentropy = -gsw_gibbs_ice_pt0(pt0_ice_old) - true_entropy
        pt0_ice = pt0_ice_old - dentropy/dentropy_dt
        ptm_ice = 0.5_r8*(pt0_ice + pt0_ice_old)
        ! add 0.01d0 to the estimate of ptm_ice for temperatures less than
        ! -273 to ensure that they are never less than -273.15d0 because
        ! the derivative approaches zero at absolute zero and the addition
        ! of 0.01d0 degrees c ensures that when we divide by the derivatve
        ! in the modified newton routine the function does not blow up.
        ptm_ice = ptm_ice + 0.01_r8
        dentropy_dt = -gsw_gibbs_ice_pt0_pt0(ptm_ice)
        pt0_ice = pt0_ice_old - dentropy/dentropy_dt
    end do

end if
    
! For temperatures less than -273.1 degsC the maximum error is less than
! 2x10^-7 degsC. For temperatures between -273.1 and 273 the maximum error
! is less than 8x10^-8 degsC, and for temperatures greater than -273 degsC the
! maximum error is 1.5x10^-12 degsC.   These errors are over the whole
! ocean depths with p varying between 0 and 10,000 dbar, while the in-situ
! temperature varied independently between -273.15 and +2 degsC.

gsw_pt0_from_t_ice = pt0_ice

return
end function

!--------------------------------------------------------------------------
