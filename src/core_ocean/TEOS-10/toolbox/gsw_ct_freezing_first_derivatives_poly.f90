!==========================================================================
elemental subroutine gsw_ct_freezing_first_derivatives_poly (sa, p, &
                          saturation_fraction, ctfreezing_sa, ctfreezing_p)
!==========================================================================
!
!  Calculates the first derivatives of the Conservative Temperature at
!  which seawater freezes, with respect to Absolute Salinity SA and
!  pressure P (in Pa) of the comptationally efficient polynomial fit of the
!  freezing temperature (McDougall et al., 2014).
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar ) 
!  saturation_fraction = the saturation fraction of dissolved air in 
!                        seawater
!
!  CTfreezing_SA = the derivative of the Conservative Temperature at
!                  freezing (ITS-90) with respect to Absolute Salinity at
!                  fixed pressure              [ K/(g/kg) ] i.e. [ K kg/g ]
!
!  CTfreezing_P  = the derivative of the Conservative Temperature at
!                  freezing (ITS-90) with respect to pressure (in Pa) at
!                  fixed Absolute Salinity                         [ K/Pa ]
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : gsw_sso

use gsw_mod_freezing_poly_coefficients

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, p, saturation_fraction
real (r8), intent(out), optional :: ctfreezing_sa, ctfreezing_p

real (r8) :: p_r, sa_r, x

real (r8), parameter :: d = -a - a*b - 2.4_r8*b/gsw_sso
real (r8), parameter :: e = 2.0_r8*a*b/gsw_sso

sa_r = sa*1e-2_r8
x = sqrt(sa_r)
p_r = p*1e-4_r8

if (present(ctfreezing_sa)) ctfreezing_sa = &
    (c1 + x*(1.5_r8*c2 + x*(2.0_r8*c3 + x*(2.5_r8*c4 + x*(3.0_r8*c5 &
        + 3.5_r8*c6*x)))) + p_r*(c10 + x*(1.5_r8*c11 + x*(2.0_r8*c13 &
        + x*(2.5_r8*c16 + x*(3.0_r8*c19 + 3.5_r8*c22*x)))) & 
        + p_r*(c12 + x*(1.5_r8*c14 + x*(2.0_r8*c17 + 2.5_r8*c20*x)) &
        + p_r*(c15 + x*(1.5_r8*c18 + 2.0_r8*c21*x)))))*1e-2_r8 &
        - saturation_fraction*1e-3_r8*(d - sa*e)

if (present(ctfreezing_p)) ctfreezing_p = &
    (c7 + sa_r*(c10 + x*(c11 + x*(c13 + x*(c16 + x*(c19 + c22*x))))) &
        + p_r*(2.0_r8*c8 + sa_r*(2.0_r8*c12 + x*(2.0_r8*c14 + x*(2.0_r8*c17 &
        + 2.0_r8*c20*x))) + p_r*(3.0_r8*c9 + sa_r*(3.0_r8*c15 + x*(3.0_r8*c18 &
        + 3.0_r8*c21*x)))))*1e-8_r8

return
end subroutine

!--------------------------------------------------------------------------
