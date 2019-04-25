!==========================================================================
elemental function gsw_sa_p_inrange (sa, p)
!==========================================================================
!
!  Check for any values that are out of the TEOS-10 range ...
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar ) 
!---------------------------------------------------------------------------

use gsw_mod_kinds

implicit none


real (r8), intent(in) :: sa, p

logical :: gsw_sa_p_inrange

gsw_sa_p_inrange = .true.

if (p.gt.10000.0_r8 .or. sa.gt.120.0_r8 .or. &
    p + sa*71.428571428571402_r8.gt.13571.42857142857_r8) &
    gsw_sa_p_inrange = .false.

return
end function

!---------------------------------------------------------------------------
