!==========================================================================
pure function gsw_mlp (sa, ct, p)
!==========================================================================
! 
!  Calculates the mixed-layer pressure as described in de Boyer Montégut 
!  et al. (2004).  The mlp is always deeper than 20 dbar, if the initial
!  estimate of the mlp is less than 20 dbar, the temperature and salinity  
!  of the bottles in the top 5 dbar are set to that of the bottle closest 
!  to 5 dbar.  This removes the effect if a thin layer of fresh water, 
!  such as that from a river outflow or from rain.
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  CT  =  Conservative Temperature (ITS-90)                       [ deg C ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar )
!
!  mlp  =  mixed-layer pressure                                    [ dbar ]
!  
!==========================================================================

use gsw_mod_toolbox, only : gsw_rho

use gsw_mod_error_functions, only : gsw_error_code

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa(:), ct(:), p(:)

real (r8) :: gsw_mlp

integer :: k(1), np

real (r8) :: min_p

real (r8), allocatable :: diff_rho0(:), dp(:), rho0(:)

character (*), parameter :: func_name = "gsw_mlp"

np = size(p)
allocate(dp(np-1))
dp = p(2:np) - p(1:np-1)
if (any(dp .le. 0.0_r8)) then  ! pressure must be monotonic and unique
    gsw_mlp = gsw_error_code(1,func_name)
    return
end if

min_p = minval(p)
if (min_p .gt. 20.0_r8) then  ! the profile starts at p greater than 20 dbar
    gsw_mlp = gsw_error_code(2,func_name)
    return
end if

allocate(rho0(np),diff_rho0(np))

rho0 = gsw_rho(sa,ct,0.0_r8)

diff_rho0 = (minval(rho0) + 0.3_r8) - rho0
k = minloc(diff_rho0, diff_rho0 .gt. 0.0_r8)
gsw_mlp = p(k(1))

if ((gsw_mlp-min_p) .lt. 20.0_r8) then

    ! If the mlp is less than 20 dbar it is possible that this density 
    ! difference is being effected by a thin fresh water layer at the surface,
    ! set the salinities and temperatures of the bottles in the top section of
    ! the cast to be equal to that of the bottle closest to 5 dbar.

    k = minloc(abs(p - 5.0_r8))
    rho0(1:k(1)-1) = rho0(k(1))

    diff_rho0 = (minval(rho0) + 0.3_r8) - rho0
    k = minloc(diff_rho0, diff_rho0 .gt. 0.0_r8)
    gsw_mlp = p(k(1))        

    if ((gsw_mlp-min_p) .lt. 20.0_r8) then
        gsw_mlp = gsw_error_code(3,func_name)
        return
    end if

end if

return
end function

!--------------------------------------------------------------------------
