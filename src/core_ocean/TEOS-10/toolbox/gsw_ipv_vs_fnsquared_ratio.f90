!==========================================================================
pure subroutine gsw_ipv_vs_fnsquared_ratio (sa, ct, p, p_ref, &
                                            ipv_vs_fnsquared_ratio, p_mid)
!==========================================================================
!
!  Calculates the ratio of the vertical gradient of potential density to 
!  the vertical gradient of locally-referenced potential density.  This 
!  ratio is also the ratio of the planetary Isopycnal Potential Vorticity
!  (IPV) to f times N^2, hence the name for this variable,
!  IPV_vs_fNsquared_ratio (see Eqn. (3.20.5) of IOC et al. (2010)). 
!  The reference sea pressure, p_ref, of the potential density surface must
!  have a constant value.
!
!  IPV_vs_fNsquared_ratio is evaluated at the mid pressure between the 
!  individual data points in the vertical.

!  sa      : Absolute Salinity         (a profile (length nz))     [g/kg]
!  ct      : Conservative Temperature  (a profile (length nz))     [deg C]
!  p       : sea pressure              (a profile (length nz))     [dbar]
!  p_ref   : reference sea pressure of the potential density surface
!        ( i.e. absolute reference pressure - 10.1325 dbar )      [dbar]
!  IPV_vs_fNsquared_ratio
!          : The ratio of the vertical gradient of potential density
!            referenced to p_ref, to the vertical gradient of locally-
!            referenced potential density.  It is ouput on the same
!            vertical (M-1)xN grid as p_mid. 
!            IPV_vs_fNsquared_ratio is dimensionless.          [ unitless ]
!  p_mid   : Mid pressure between p grid  (length nz-1)           [dbar]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_alpha, gsw_beta

use gsw_mod_error_functions, only : gsw_error_code

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa(:), ct(:), p(:), p_ref
real (r8), intent(out) :: ipv_vs_fnsquared_ratio(:), p_mid(:)

integer :: nz, i
real (r8), dimension(:), allocatable :: dsa, sa_mid, dct, ct_mid, vp_ref
real (r8), dimension(:), allocatable :: alpha_mid, beta_mid, alpha_pref
real (r8), dimension(:), allocatable :: beta_pref, numerator, denominator

character (*), parameter :: func_name = "gsw_ipv_vs_fnsquared_ratio"

nz = size(sa)
if (size(ipv_vs_fnsquared_ratio).lt.nz-1 .or. size(p_mid).lt.nz-1) then
    ipv_vs_fnsquared_ratio = gsw_error_code(1,func_name)
    p_mid = ipv_vs_fnsquared_ratio(1)
    return
end if

allocate (dsa(nz-1), sa_mid(nz-1), dct(nz-1), ct_mid(nz-1))
allocate (vp_ref(nz-1), alpha_mid(nz-1), beta_mid(nz-1), alpha_pref(nz-1))
allocate (beta_pref(nz-1), numerator(nz-1), denominator(nz-1))

forall (i = 1: nz-1)
    dsa(i) = sa(i) - sa(i+1)
    dct(i) = ct(i) - ct(i+1)
    sa_mid(i) = 0.5_r8*(sa(i) + sa(i+1))
    ct_mid(i) = 0.5_r8*(ct(i) + ct(i+1))
    p_mid(i) = 0.5_r8*(p(i) + p(i+1))
end forall

vp_ref = p_ref
alpha_mid = gsw_alpha(sa_mid,ct_mid,p_mid(1:nz-1))
beta_mid = gsw_beta(sa_mid,ct_mid,p_mid(1:nz-1))
alpha_pref = gsw_alpha(sa_mid,ct_mid,vp_ref)
beta_pref = gsw_beta(sa_mid,ct_mid,vp_ref)

numerator = dct*alpha_pref - dsa*beta_pref
denominator = dct*alpha_mid - dsa*beta_mid

where (denominator /= 0.0_r8)
    ipv_vs_fnsquared_ratio = numerator/denominator
elsewhere
    ipv_vs_fnsquared_ratio = gsw_error_code(2,func_name)
end where

deallocate (dsa, sa_mid, dct, ct_mid)
deallocate (vp_ref, alpha_mid, beta_mid, alpha_pref)
deallocate (beta_pref, numerator, denominator)

return
end subroutine

!--------------------------------------------------------------------------
