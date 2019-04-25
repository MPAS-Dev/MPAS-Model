!==========================================================================
elemental function gsw_sa_freezing_from_t (t, p, saturation_fraction)
!==========================================================================
!
!  Calculates the Absolute Salinity of seawater at the freezing temperature.  
!  That is, the output is the Absolute Salinity of seawater, with the 
!  fraction saturation_fraction of dissolved air, that is in equilibrium 
!  with ice at in-situ temperature t and pressure p.  If the input values 
!  are such that there is no positive value of Absolute Salinity for which 
!  seawater is frozen, the output is set to NaN.  
!
!  t  =  in-situ Temperature (ITS-90)                             [ deg C ]
!  p  =  sea pressure                                              [ dbar ]
!        ( i.e. absolute pressure - 10.1325 dbar ) 
!  saturation_fraction = the saturation fraction of dissolved air in 
!                        seawater
!  (i.e., saturation_fraction must be between 0 and 1, and the default 
!    is 1, completely saturated) 
!
!  sa_freezing_from_t  =  Absolute Salinity of seawater when it freezes, for 
!                given input values of in situ temperature, pressure and 
!                air saturation fraction.                          [ g/kg ] 
!---------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_t_freezing_exact, gsw_sa_p_inrange
use gsw_mod_toolbox, only : gsw_t_freezing_first_derivatives
use gsw_mod_toolbox, only : gsw_sa_freezing_estimate

use gsw_mod_error_functions, only : gsw_error_code, gsw_error_limit

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: t, p, saturation_fraction

real (r8) :: gsw_sa_freezing_from_t

integer :: i_iter
real (r8) :: f, sa, sa_mean, sa_old, t_freezing_zero_sa, tfreezing_sa

integer, parameter :: number_of_iterations = 2

! This is the band of sa within +- 2.5 g/kg of sa = 0, which we treat
! differently in calculating the initial values of both SA and dCT_dSA. 
real (r8), parameter :: sa_cut_off = 2.5_r8

character (*), parameter :: func_name = "gsw_sa_freezing_from_t"

! Find t > t_freezing_zero_SA.  If this is the case, the input values
! represent seawater that is not frozen (at any positive SA). 
t_freezing_zero_sa = gsw_t_freezing_exact(0.0_r8,p,saturation_fraction)
if (t .gt. t_freezing_zero_sa) then
    gsw_sa_freezing_from_t = gsw_error_code(1,func_name)
    return
end if
 
! This is the inital guess of SA using a purpose-built polynomial in CT and p  
sa = gsw_sa_freezing_estimate(p,saturation_fraction,t=t)
if (sa .lt. -sa_cut_off) then
    gsw_sa_freezing_from_t = gsw_error_code(2,func_name)
    return
end if

! Form the first estimate of tfreezing_SA, the derivative of CT_freezing  
! with respect to SA at fixed p.    
sa = max(sa,0.0_r8)
call gsw_t_freezing_first_derivatives(sa,p,saturation_fraction, &
                                      tfreezing_sa=tfreezing_sa)

! For -SA_cut_off < SA < SA_cut_off, replace the above estimate of SA  
! with one based on (t_freezing_zero_SA - t).
if (abs(sa) .lt. sa_cut_off) sa = (t - t_freezing_zero_sa)/tfreezing_sa

!---------------------------------------------------------------------------
! Begin the modified Newton-Raphson method to find the root of 
! f = (t_freezing - t) = 0 for SA. 
!---------------------------------------------------------------------------
do i_iter = 1, number_of_iterations
    sa_old = sa
    f = gsw_t_freezing_exact(sa_old,p,saturation_fraction) - t
    sa = sa_old - f/tfreezing_sa
    sa_mean = 0.5_r8*(sa + sa_old)
    call gsw_t_freezing_first_derivatives(sa_mean,p,saturation_fraction, &
                                          tfreezing_sa=tfreezing_sa)
    sa = sa_old - f/tfreezing_sa   
end do

if (gsw_sa_p_inrange(sa,p)) then
    gsw_sa_freezing_from_t = sa
else
    gsw_sa_freezing_from_t = gsw_error_code(3,func_name)
end if

return
end function

!---------------------------------------------------------------------------
