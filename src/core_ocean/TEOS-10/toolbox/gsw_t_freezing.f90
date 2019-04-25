!==========================================================================
elemental function gsw_t_freezing (sa, p, saturation_fraction, poly)
!==========================================================================
!
!  Calculates the in-situ temperature at which seawater freezes.
!
!  sa  =  absolute Salinity                                        [ g/kg ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar ) 
!  saturation_fraction = the saturation fraction of dissolved air in 
!                        seawater
!
!  t_freezing = in-situ temperature at which seawater freezes.    [ deg C ]
!               (ITS-90)                
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_t_freezing_exact, gsw_t_freezing_poly

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, p, saturation_fraction
logical, intent(in), optional :: poly

real (r8) :: gsw_t_freezing

logical :: do_poly

if (present(poly)) then
    do_poly = poly
else
    do_poly = .false.
end if

if (do_poly) then
    gsw_t_freezing = gsw_t_freezing_poly(sa,p,saturation_fraction)
else
    gsw_t_freezing = gsw_t_freezing_exact(sa,p,saturation_fraction)
end if

return
end function

!--------------------------------------------------------------------------
