!==========================================================================
pure subroutine gsw_turner_rsubrho (sa, ct, p, tu, rsubrho, p_mid)
!==========================================================================
!
!  Calculates the Turner angle and the Rsubrho as a function of pressure 
!  down a vertical water column.  These quantities express the relative 
!  contributions of the vertical gradients of Conservative Temperature 
!  and Absolute Salinity to the vertical stability (the square of the 
!  Brunt-Vaisala Frequency squared, N^2).  Tu and Rsubrho are evaluated at 
!  the mid pressure between the individual data points in the vertical.  
!  density in terms of SA, CT and p (IOC et al., 2010).
!
!  Note that in the double-diffusive literature, papers concerned with
!  the "diffusive" form of double-diffusive convection often define the 
!  stability ratio as the reciprocal of what is defined here as the 
!  stability ratio.  
!
! sa      : Absolute Salinity         (a profile (length nz))     [g/kg]
! ct      : Conservative Temperature  (a profile (length nz))     [deg C]
! p       : sea pressure              (a profile (length nz))     [dbar]
! tu      : Turner angle, on the same (nz-1) grid as p_mid.
!           Turner angle has units of:           [ degrees of rotation ]
! rsubrho : Stability Ratio, on the same (nz-1) grid as p_mid.
!           rsubrho is dimensionless.                       [ unitless ]
! p_mid   : Mid pressure between p grid  (length nz-1)           [dbar]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_specvol_alpha_beta

use gsw_mod_teos10_constants, only : rad2deg

use gsw_mod_error_functions, only : gsw_error_code

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa(:), ct(:), p(:)
real (r8), intent(out) :: tu(:), rsubrho(:), p_mid(:)

integer :: nz, k
real (r8), dimension(:), allocatable :: dsa, sa_mid, dct, ct_mid, dp
real (r8), dimension(:), allocatable :: alpha_mid, beta_mid

character (*), parameter :: func_name = "gsw_turner_rsubrho"

nz = size(sa)
if (size(tu).lt.nz-1 .or. size(rsubrho).lt.nz-1 .or. size(p_mid).lt.nz-1) then
    tu = gsw_error_code(1,func_name)
    rsubrho = tu(1)
    p_mid = tu(1)
    return
end if

allocate (dsa(nz-1), sa_mid(nz-1), dct(nz-1), ct_mid(nz-1), dp(nz-1))
allocate (alpha_mid(nz-1), beta_mid(nz-1))

forall (k = 1: nz-1)
    dsa(k) = (sa(k) - sa(k+1))
    sa_mid(k) = 0.5_r8*(sa(k) + sa(k+1))
    dct(k) = (ct(k) - ct(k+1))
    ct_mid(k) = 0.5_r8*(ct(k) + ct(k+1))
    dp(k) = (p(k) - p(k+1))
    p_mid(k) = 0.5_r8*(p(k) + p(k+1))
end forall

call gsw_specvol_alpha_beta(sa_mid,ct_mid,p_mid(1:nz-1), &
                            alpha=alpha_mid,beta=beta_mid)

tu(1:nz-1) = rad2deg*atan2((alpha_mid*dct + beta_mid*dsa), &
                              (alpha_mid*dct - beta_mid*dsa))

where (dsa .ne. 0.0_r8)
   rsubrho = (alpha_mid*dct)/(beta_mid*dsa)
elsewhere
   rsubrho = gsw_error_code(2,func_name)
end where

deallocate (dsa, sa_mid, dct, ct_mid, dp)
deallocate (alpha_mid, beta_mid)

return
end subroutine

!--------------------------------------------------------------------------
