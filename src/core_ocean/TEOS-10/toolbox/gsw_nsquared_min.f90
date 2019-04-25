!==========================================================================
pure subroutine gsw_nsquared_min (sa, ct, p, lat, n2, n2_p, &
                               n2_specvol, n2_alpha, n2_beta, dsa, dct, dp)
!==========================================================================
! 
!  Calculates the minimum buoyancy frequency squared (N^2) (i.e. the 
!  Brunt-Vaisala frequency squared) between two bottles from the equation,
!
!           2      2     beta.dSA - alpha.dCT
!         N   =  g  . -------------------------
!                         specvol_local.dP
!
!  The pressure increment, dP, in the above formula is in Pa, so that it is
!  10^4 times the pressure increment dp in dbar. 
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  CT  =  Conservative Temperature (ITS-90)                       [ deg C ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar )
!  lat =  latitude in decimal degrees north                 [ -90 ... +90 ]
!
!  N2         =  minimum Brunt-Vaisala Frequency squared          [ 1/s^2 ]
!  N2_p       =  pressure of minimum N2                            [ dbar ]
!  N2_specvol =  specific volume at the minimum N2                [ kg/m3 ]
!  N2_alpha   =  thermal expansion coefficient with respect         [ 1/K ]
!                to Conservative Temperature at the minimum N2
!  N2_beta    =  saline contraction coefficient at constant        [ kg/g ]
!                Conservative Temperature at the minimum N2
!  dSA        =  difference in salinity between bottles            [ g/kg ]
!  dCT        =  difference in Conservative Temperature between   [ deg C ]
!                bottles
!  dp         =  difference in pressure between bottles            [ dbar ]
!
!==========================================================================

use gsw_mod_toolbox, only : gsw_grav, gsw_specvol_alpha_beta

use gsw_mod_error_functions, only : gsw_error_code

use gsw_mod_kinds

use gsw_mod_teos10_constants, only : db2pa

implicit none

real (r8), intent(in) :: sa(:), ct(:), p(:), lat(:)
real (r8), intent(out) :: n2(:), n2_p(:), n2_specvol(:), n2_alpha(:)
real (r8), intent(out) :: n2_beta(:), dsa(:), dct(:), dp(:)

integer :: i, ideep, ishallow, mp

real (r8) :: n2_deep, n2_shallow
real (r8), allocatable :: alpha(:), beta(:), specvol(:), grav2(:)

character (*), parameter :: func_name = "gsw_nsquared_min"

mp = size(sa)
if (size(n2).lt.mp-1 .or. size(n2_p).lt.mp-1 .or. &
    size(n2_specvol).lt.mp-1 .or. size(n2_alpha).lt.mp-1 .or. &
    size(n2_beta).lt.mp-1 .or. size(dsa).lt.mp-1 .or. &
    size(dct).lt.mp-1 .or. size(dp).lt.mp-1) then
    n2 = gsw_error_code(1,func_name)
    n2_p = n2(1)
    n2_specvol = n2(1)
    n2_alpha = n2(1)
    n2_beta = n2(1)
    dsa = n2(1)
    dct = n2(1)
    dp = n2(1)
    return
end if

allocate(grav2(mp),specvol(mp),alpha(mp),beta(mp))

grav2 = gsw_grav(lat,p)**2

dp(1:mp-1)  =  p(2:mp) -  p(1:mp-1)
dsa(1:mp-1) = sa(2:mp) - sa(1:mp-1)
dct(1:mp-1) = ct(2:mp) - ct(1:mp-1)

call gsw_specvol_alpha_beta(sa,ct,p,specvol,alpha,beta)

ishallow = 1
ideep = 2
do i = 1, mp-1
    n2_shallow =  grav2(ishallow)/(specvol(ishallow)*db2pa*dp(i))* &
                 (beta(ishallow)*dsa(i) - alpha(ishallow)*dct(i))
    n2_deep =  grav2(ideep)/(specvol(ideep)*db2pa*dp(i))* &
              (beta(ideep)*dsa(i) - alpha(ideep)*dct(i))
    if (n2_shallow .lt. n2_deep) then
        n2(i) = n2_shallow
        n2_p(i) = p(ishallow)
        n2_specvol(i) = specvol(ishallow)
        n2_alpha(i) = alpha(ishallow)
        n2_beta(i) = beta(ishallow)
    else
        n2(i) = n2_deep
        n2_p(i) = p(ideep)
        n2_specvol(i) = specvol(ideep)
        n2_alpha(i) = alpha(ideep)
        n2_beta(i) = beta(ideep)
    end if
    ishallow = ishallow + 1
    ideep = ideep + 1
end do

return
end subroutine

!--------------------------------------------------------------------------
