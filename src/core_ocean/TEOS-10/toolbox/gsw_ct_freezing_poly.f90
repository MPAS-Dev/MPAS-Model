!==========================================================================
elemental function gsw_ct_freezing_poly (sa, p, saturation_fraction)
!==========================================================================
!
!  Calculates the Conservative Temperature at which seawater freezes.
!  The error of this fit ranges between -5e-4 K and 6e-4 K when compared 
!  with the Conservative Temperature calculated from the exact in-situ 
!  freezing temperature which is found by a Newton-Raphson iteration of the 
!  equality of the chemical potentials of water in seawater and in ice.  
!  Note that the Conservative temperature freezing temperature can be found
!  by this exact method using the function gsw_CT_freezing.
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar ) 
!  saturation_fraction = the saturation fraction of dissolved air in 
!                        seawater
!
!  CT_freezing = Conservative Temperature at freezing of seawater [ deg C ]
!                That is, the freezing temperature expressed in
!                terms of Conservative Temperature (ITS-90).                
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : gsw_sso

use gsw_mod_freezing_poly_coefficients

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, p, saturation_fraction

real (r8) :: gsw_ct_freezing_poly

real (r8) :: p_r, sa_r, x

sa_r = sa*1e-2_r8
x = sqrt(sa_r)
p_r = p*1e-4_r8

gsw_ct_freezing_poly = c0 &
    + sa_r*(c1 + x*(c2 + x*(c3 + x*(c4 + x*(c5 + c6*x))))) &
    + p_r*(c7 + p_r*(c8 + c9*p_r)) + sa_r*p_r*(c10 + p_r*(c12 &
    + p_r*(c15 + c21*sa_r)) + sa_r*(c13 + c17*p_r + c19*sa_r) &
    + x*(c11 + p_r*(c14 + c18*p_r) + sa_r*(c16 + c20*p_r + c22*sa_r)))

! Adjust for the effects of dissolved air 
gsw_ct_freezing_poly = gsw_ct_freezing_poly - saturation_fraction* &
                 (1e-3_r8)*(2.4_r8 - a*sa)*(1.0_r8 + b*(1.0_r8 - sa/gsw_sso))

return
end function
