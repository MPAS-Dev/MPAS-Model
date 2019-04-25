!==========================================================================
elemental function gsw_pressure_freezing_ct (sa, ct, saturation_fraction)
!==========================================================================
!
!  Calculates the pressure (in dbar) of seawater at the freezing
!  temperature.  That is, the output is the pressure at which seawater,
!  with Absolute Salinity SA, Conservative Temperature CT, and with 
!  saturation_fraction of dissolved air, freezes.  If the input values are 
!  such that there is no value of pressure in the range between 0 dbar and 
!  10,000 dbar for which seawater is at the freezing temperature, the 
!  output, pressure_freezing, is put equal to NaN.
!
!  SA  =  Absolute Salinity of seawater                            [ g/kg ]
!  CT  =  Conservative Temperature (ITS-90)                       [ deg C ]
!  saturation_fraction = the saturation fraction of dissolved air in 
!                        seawater
!
!  pressure_freezing = sea pressure at which the seawater freezes  [ dbar ]
!        ( i.e. absolute pressure - 10.1325 dbar ) 
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_ct_freezing_first_derivatives, gsw_sa_p_inrange
use gsw_mod_toolbox, only : gsw_ct_freezing_exact

use gsw_mod_error_functions, only : gsw_error_code, gsw_error_limit

use gsw_mod_teos10_constants, only : rec_pa2db

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct, saturation_fraction

real (r8) :: gsw_pressure_freezing_ct

integer :: i_iter
real (r8) :: ct_freezing_p0, ct_freezing_p10000, dctf_dp, f
real (r8) :: pf, pfm, pf_old, ctfreezing_p

integer, parameter :: number_of_iterations = 3

! rec_Pa2dbar is to have dCTf_dp in units of K/dbar rather than K/Pa
 
character (*), parameter :: func_name = "gsw_pressure_freezing_ct"

! Find CT > CT_freezing_p0.  If this is the case, the input CT value
! represent seawater that will not be frozen at any positive p.  
ct_freezing_p0 = gsw_ct_freezing_exact(sa,0.0_r8,saturation_fraction)
if (ct .gt. ct_freezing_p0) then
    gsw_pressure_freezing_ct = gsw_error_code(1,func_name)
    return
end if
 
! Find CT < CT_freezing_p10000.  If this is the case, the input CT value
! represent seawater that is frozen even at p = 10,000 dbar.   
ct_freezing_p10000 = gsw_ct_freezing_exact(sa,1e4_r8,saturation_fraction)
if (ct .lt. ct_freezing_p10000) then
    gsw_pressure_freezing_ct = gsw_error_code(2,func_name)
    return
end if

! This is the initial (linear) guess of the freezing pressure, in dbar.  
pf = rec_pa2db*(ct_freezing_p0 - ct)/(ct_freezing_p0 - ct_freezing_p10000)

call gsw_ct_freezing_first_derivatives(sa,pf,saturation_fraction, &
                                       ctfreezing_p=ctfreezing_p)
dctf_dp = rec_pa2db*ctfreezing_p
    !  this dctf_dp is the initial value of the partial derivative of 
    !  ct_freezing with respect to pressure (in dbar) at fixed sa, 
    !  assuming that the saturation_fraction is zero. 

do i_iter = 1, number_of_iterations  
    pf_old = pf
    f = gsw_ct_freezing_exact(sa,pf_old,saturation_fraction) - ct
    pf = pf_old - f/dctf_dp
    pfm = 0.5_r8*(pf + pf_old)
    call gsw_ct_freezing_first_derivatives(sa,pfm,saturation_fraction, &
                                           ctfreezing_p=ctfreezing_p)
    dctf_dp = rec_pa2db*ctfreezing_p
    pf = pf_old - f/dctf_dp
end do

if (gsw_sa_p_inrange(sa,pf)) then
    gsw_pressure_freezing_ct = pf
else
    gsw_pressure_freezing_ct = gsw_error_code(3,func_name)
end if

return
end function

!--------------------------------------------------------------------------
