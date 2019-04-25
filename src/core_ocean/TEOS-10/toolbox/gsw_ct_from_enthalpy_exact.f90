!==========================================================================
elemental function gsw_ct_from_enthalpy_exact (sa, h, p)
!==========================================================================
!
!  Calculates the Conservative Temperature of seawater, given the Absolute 
!  Salinity, specific enthalpy, h, and pressure p.
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  h   =  specific enthalpy                                        [ J/kg ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325d0 dbar ) 
!
!  CT  =  Conservative Temperature ( ITS-90)                      [ deg C ]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_ct_freezing_exact, gsw_enthalpy_ct_exact
use gsw_mod_toolbox, only : gsw_enthalpy_first_derivatives_ct_exact

use gsw_mod_error_functions, only : gsw_error_code

use gsw_mod_teos10_constants, only : gsw_cp0

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, h, p

real (r8) :: gsw_ct_from_enthalpy_exact

real (r8) :: ct, ct_freezing, ct_mean, ct_old, f, h_freezing
real (r8) :: h_ct, h_40

real (r8), parameter :: ct_40 = 40.0_r8

character (*), parameter :: func_name = "gsw_ct_from_enthalpy_exact"

ct_freezing = gsw_ct_freezing_exact(sa,p,0.0_r8)

h_freezing = gsw_enthalpy_ct_exact(sa,ct_freezing,p)
if (h .lt. h_freezing - gsw_cp0) then
    ! The input, seawater enthalpy h, is less than the enthalpy at the
    ! freezing temperature, i.e. the water is frozen.
    gsw_ct_from_enthalpy_exact = gsw_error_code(1,func_name)
    return
endif

h_40 = gsw_enthalpy_ct_exact(sa,ct_40,p)
if (h .gt. h_40) then
    ! The input seawater enthalpy is greater than the enthalpy when CT is 40C
    gsw_ct_from_enthalpy_exact = gsw_error_code(2,func_name)
    return
endif

! First guess of ct
ct = ct_freezing + (ct_40 - ct_freezing)*(h - h_freezing)/(h_40 - h_freezing)
call gsw_enthalpy_first_derivatives_ct_exact(sa,ct,p,h_ct=h_ct)

!--------------------------------------------------------------------------
! Begin the modified Newton-Raphson iterative procedure 
!--------------------------------------------------------------------------

ct_old = ct
f = gsw_enthalpy_ct_exact(sa,ct_old,p) - h
ct = ct_old - f/h_ct
ct_mean = 0.5_r8*(ct + ct_old)
call gsw_enthalpy_first_derivatives_ct_exact(sa,ct_mean,p,h_ct=h_ct)
ct = ct_old - f/h_ct

! After 1 iteration of this modified Newton-Raphson iteration,
! the error in CT is no larger than 5x10^-14 degrees C, which 
! is machine precision for this calculation. 

gsw_ct_from_enthalpy_exact = ct

return
end function

!--------------------------------------------------------------------------
