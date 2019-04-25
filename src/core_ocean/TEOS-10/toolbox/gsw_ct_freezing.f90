!==========================================================================
elemental function gsw_ct_freezing (sa, p, saturation_fraction, poly)
!==========================================================================
!
!  Calculates the Conservative Temperature at which seawater freezes.
!
!  sa  =  absolute Salinity                                        [ g/kg ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar ) 
!  saturation_fraction = the saturation fraction of dissolved air in 
!                        seawater
!
!  ct_freezing = Conservative Temperature at freezing of seawater [ deg C ]
!                That is, the freezing temperature expressed in
!                terms of Conservative Temperature (ITS-90).                
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_ct_freezing_exact, gsw_ct_freezing_poly

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, p, saturation_fraction
logical, intent(in), optional :: poly

real (r8) :: gsw_ct_freezing

logical :: do_poly

if (present(poly)) then
    do_poly = poly
else
    do_poly = .false.
end if

if (do_poly) then
    gsw_ct_freezing = gsw_ct_freezing_poly(sa,p,saturation_fraction)
else
    gsw_ct_freezing = gsw_ct_freezing_exact(sa,p,saturation_fraction)
end if

return
end function

!--------------------------------------------------------------------------
