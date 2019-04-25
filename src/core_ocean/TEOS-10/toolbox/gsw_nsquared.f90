!==========================================================================
pure subroutine gsw_nsquared (sa, ct, p, lat, n2, p_mid)
!==========================================================================
!
!  Calculates the buoyancy frequency squared (N^2)(i.e. the Brunt-Vaisala 
!  frequency squared) at the mid pressure from the equation,
!
!
!           2      2             beta.d(SA) - alpha.d(CT)
!         N   =  g  .rho_local. -------------------------
!                                          dP
!
!  The pressure increment, dP, in the above formula is in Pa, so that it is
!  10^4 times the pressure increment dp in dbar. 
!
!  sa     : Absolute Salinity         (a profile (length nz))     [g/kg]
!  ct     : Conservative Temperature  (a profile (length nz))     [deg C]
!  p      : sea pressure              (a profile (length nz))     [dbar]
!  lat    : latitude                  (a profile (length nz))     [deg N]
!  n2     : Brunt-Vaisala Frequency squared  (length nz-1)        [s^-2]
!  p_mid  : Mid pressure between p grid      (length nz-1)        [dbar]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_grav, gsw_rho_alpha_beta

use gsw_mod_teos10_constants, only : db2pa

use gsw_mod_error_functions, only : gsw_error_code

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa(:), ct(:), p(:), lat(:)
real (r8), intent(out) :: n2(:), p_mid(:)

integer :: nz, k
real (r8), dimension(:), allocatable :: dsa, sa_mid, dct, ct_mid, dp, rho_mid
real (r8), dimension(:), allocatable :: alpha_mid, beta_mid, grav_local, grav

character (*), parameter :: func_name = "gsw_nsquared"

nz = size(sa)
if (size(n2).lt.nz-1 .or. size(p_mid).lt.nz-1) then
    n2 = gsw_error_code(1,func_name)
    p_mid = n2(1)
    return
end if

allocate (grav(nz), dsa(nz-1), sa_mid(nz-1), dct(nz-1), ct_mid(nz-1), dp(nz-1))
allocate (rho_mid(nz-1), alpha_mid(nz-1), beta_mid(nz-1), grav_local(nz-1))

grav = gsw_grav(lat(1:nz),p(1:nz))

forall (k = 1: nz-1)
    grav_local(k) = 0.5_r8*(grav(k) + grav(k+1))
    dsa(k) = (sa(k+1) - sa(k))
    sa_mid(k) = 0.5_r8*(sa(k) + sa(k+1))
    dct(k) = (ct(k+1) - ct(k))
    ct_mid(k) = 0.5_r8*(ct(k) + ct(k+1))
    dp(k) = (p(k+1) - p(k))
    p_mid(k) = 0.5_r8*(p(k) + p(k+1))
end forall

call gsw_rho_alpha_beta(sa_mid,ct_mid,p_mid(1:nz-1),rho_mid,alpha_mid,beta_mid)

n2(1:nz-1) = (grav_local**2) * (rho_mid/(db2pa*dp)) * &
             (beta_mid*dsa - alpha_mid*dct)

deallocate (grav, dsa, sa_mid, dct, ct_mid, dp)
deallocate (rho_mid, alpha_mid, beta_mid, grav_local)

return
end subroutine

!--------------------------------------------------------------------------
