!==========================================================================
elemental function gsw_sa_from_rho (rho, ct, p)
!==========================================================================
!
!  Calculates the Absolute Salinity of a seawater sample, for given values
!  of its density, Conservative Temperature and sea pressure (in dbar). 

!  rho =  density of a seawater sample (e.g. 1026 kg/m^3).       [ kg/m^3 ]
!   Note. This input has not had 1000 kg/m^3 subtracted from it. 
!     That is, it is 'density', not 'density anomaly'.
!  ct  =  Conservative Temperature (ITS-90)                       [ deg C ]
!  p   =  sea pressure                                             [ dbar ]
!
!  sa  =  Absolute Salinity                                          [g/kg]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_specvol, gsw_specvol_first_derivatives

use gsw_mod_error_functions, only : gsw_error_code, gsw_error_limit

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: rho, ct, p

real (r8) :: gsw_sa_from_rho

integer no_iter

real (r8) :: sa, v_lab, v_0, v_50, v_sa, sa_old, delta_v, sa_mean

character (*), parameter :: func_name = "gsw_sa_from_rho"

v_lab = 1.0_r8/rho
v_0 = gsw_specvol(0.0_r8,ct,p)
v_50 = gsw_specvol(50.0_r8,ct,p)

sa = 50.0_r8*(v_lab - v_0)/(v_50 - v_0)

if (sa.lt.0_r8.or.sa.gt.50_r8) then
    gsw_sa_from_rho = gsw_error_code(1,func_name)
    return
end if

v_sa = (v_50 - v_0)/50.0_r8

do no_iter = 1, 2 
    sa_old = sa
    delta_v = gsw_specvol(sa_old,ct,p) - v_lab
    sa = sa_old - delta_v/v_sa 
    sa_mean = 0.5_r8*(sa + sa_old)
    call gsw_specvol_first_derivatives(sa_mean,ct,p,v_sa)
    sa = sa_old - delta_v/v_sa
    if (sa.lt.0_r8.or.sa.gt.50_r8) then
        gsw_sa_from_rho = gsw_error_code(no_iter+1,func_name)
        return
    end if
end do

gsw_sa_from_rho = sa

return
end function

!--------------------------------------------------------------------------
