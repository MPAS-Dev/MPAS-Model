! =========================================================================
elemental function gsw_gibbs_ice (nt, np, t, p)
! =========================================================================
!
!  Ice specific Gibbs energy and derivatives up to order 2.
!
!  nt  =  order of t derivative                      [ integers 0, 1 or 2 ]
!  np  =  order of p derivative                      [ integers 0, 1 or 2 ]
!  t   =  in-situ temperature (ITS-90)                            [ deg C ]
!  p   =  sea pressure                                             [ dbar ]
!   
!  gibbs_ice = Specific Gibbs energy of ice or its derivatives.
!            The Gibbs energy (when nt = np = 0) has units of:     [ J/kg ]
!            The temperature derivatives are output in units of: 
!                                                      [ (J/kg) (K)^(-nt) ]
!            The pressure derivatives are output in units of:
!                                                     [ (J/kg) (Pa)^(-np) ]
!            The mixed derivatives are output in units of:
!                                           [ (J/kg) (K)^(-nt) (Pa)^(-np) ]
!  Note. The derivatives are taken with respect to pressure in Pa, not
!    withstanding that the pressure input into this routine is in dbar.
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : gsw_t0, db2pa

use gsw_mod_error_functions, only : gsw_error_code

use gsw_mod_gibbs_ice_coefficients

use gsw_mod_kinds

implicit none

integer, intent(in) :: nt, np
real (r8), intent(in) :: t, p

real (r8) :: gsw_gibbs_ice

real (r8) :: dzi, g0, g0p, g0pp, sqrec_pt
complex (r8) :: r2, r2p, r2pp, g, sqtau_t1, sqtau_t2, tau, tau_t1, tau_t2

real (r8), parameter :: s0 = -3.32733756492168e3_r8

character (*), parameter :: func_name = "gsw_gibbs_ice"

tau = cmplx((t + gsw_t0)*rec_t3p,0.0_r8,r8)

dzi = db2pa*p*rec_pt

if (nt.eq.0 .and. np.eq.0) then
    
    tau_t1 = tau/t1
    sqtau_t1 = tau_t1*tau_t1
    tau_t2 = tau/t2
    sqtau_t2 = tau_t2*tau_t2
    
    g0 = g00 + dzi*(g01 + dzi*(g02 + dzi*(g03 + g04*dzi)))

    r2 = r20 + dzi*(r21 + r22*dzi)
         
    g = r1*(tau*log((1.0_r8 + tau_t1)/(1.0_r8 - tau_t1)) &
        + t1*(log(1.0_r8 - sqtau_t1) - sqtau_t1)) &
        + r2*(tau*log((1.0_r8 + tau_t2)/(1.0_r8 - tau_t2)) &
        + t2*(log(1.0_r8 - sqtau_t2) - sqtau_t2))
   
    gsw_gibbs_ice = real(g0 - t3p*(s0*tau - g),r8)
    
elseif (nt.eq.1 .and. np.eq.0) then
    
    tau_t1 = tau/t1
    tau_t2 = tau/t2
    
    r2 = r20 + dzi*(r21 + r22*dzi)
    
    g = r1*(log((1.0_r8 + tau_t1)/(1.0_r8 - tau_t1)) - 2.0_r8*tau_t1) &
        + r2*(log((1.0_r8 + tau_t2)/(1.0_r8 - tau_t2)) - 2.0_r8*tau_t2)
    
    gsw_gibbs_ice = -s0 + real(g,r8)
        
elseif (nt.eq.0 .and. np.eq.1) then
    
    tau_t2 = tau/t2
    sqtau_t2 = tau_t2*tau_t2
    
    g0p = rec_pt*(g01 + dzi*(2.0_r8*g02 + dzi*(3.0_r8*g03 + 4.0_r8*g04*dzi)))
    
    r2p = rec_pt*(r21 + 2.0_r8*r22*dzi)
        
    g = r2p*(tau*log((1.0_r8 + tau_t2)/(1.0_r8 - tau_t2)) &
        + t2*(log(1.0_r8 - sqtau_t2) - sqtau_t2))
    
    gsw_gibbs_ice = g0p + t3p*real(g,r8)

elseif (nt.eq.1 .and. np.eq.1) then
    
    tau_t2 = tau/t2

    r2p = rec_pt*(r21 + 2.0_r8*r22*dzi) 
    
    g = r2p*(log((1.0_r8 + tau_t2)/(1.0_r8 - tau_t2)) - 2.0_r8*tau_t2)
    
    gsw_gibbs_ice = real(g,r8)
    
elseif (nt.eq.2 .and. np.eq.0) then
    
    r2 = r20 + dzi*(r21 + r22*dzi)
    
    g = r1*(1.0_r8/(t1 - tau) + 1.0_r8/(t1 + tau) - 2.0_r8/t1) &
        + r2*(1.0_r8/(t2 - tau) + 1.0_r8/(t2 + tau) - 2.0_r8/t2)
    
    gsw_gibbs_ice = rec_t3p*real(g,r8)
    
elseif (nt.eq.0 .and. np.eq.2) then
   
    sqrec_pt = rec_pt*rec_pt
    
    tau_t2 = tau/t2
    sqtau_t2 = tau_t2*tau_t2
    
    g0pp = sqrec_pt*(2.0_r8*g02 + dzi*(6.0_r8*g03 + 12.0_r8*g04*dzi))
    
    r2pp = 2.0_r8*r22*sqrec_pt
       
    g = r2pp*(tau*log((1.0_r8 + tau_t2)/(1.0_r8 - tau_t2)) &
        + t2*(log(1.0_r8 - sqtau_t2) - sqtau_t2))

   gsw_gibbs_ice = g0pp + t3p*real(g,r8)
    
else

   gsw_gibbs_ice = gsw_error_code(1,func_name)

end if
    
return
end function

!--------------------------------------------------------------------------
