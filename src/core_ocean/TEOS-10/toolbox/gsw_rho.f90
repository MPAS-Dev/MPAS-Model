!==========================================================================
elemental function gsw_rho (sa, ct, p)
!==========================================================================
! 
!  Calculates in-situ density from Absolute Salinity and Conservative 
!  Temperature, using the computationally-efficient expression for
!  specific volume in terms of SA, CT and p (Roquet et al., 2014).
!
!  Note that potential density with respect to reference pressure, pr, is
!  obtained by calling this function with the pressure argument being pr
!  (i.e. "gsw_rho(SA,CT,pr)").
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  CT  =  Conservative Temperature (ITS-90)                       [ deg C ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar )
!
!  rho  =  in-situ density                                         [ kg/m ]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_specvol

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct, p

real (r8) :: gsw_rho

gsw_rho = 1.0_r8/gsw_specvol(sa,ct,p)

return
end function

!--------------------------------------------------------------------------
