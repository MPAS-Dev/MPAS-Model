!==========================================================================
elemental subroutine gsw_pt_first_derivatives (sa, ct, pt_sa, pt_ct)
! =========================================================================
!
!  Calculates the following two partial derivatives of potential temperature 
!  (the regular potential temperature whose reference sea pressure is 0 dbar) 
!  (1) pt_SA, the derivative with respect to Absolute Salinity at 
!       constant Conservative Temperature, and
!  (2) pt_CT, the derivative with respect to Conservative Temperature at 
!       constant Absolute Salinity. 
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  CT  =  Conservative Temperature (ITS-90)                       [ deg C ]
!
!  pt_SA =  The derivative of potential temperature with respect to 
!           Absolute Salinity at constant Conservative Temperature. 
!                                                               [ K/(g/kg)]
!  pt_CT =  The derivative of potential temperature with respect to 
!           Conservative Temperature at constant Absolute Salinity.
!           pt_CT is dimensionless.                            [ unitless ]
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : gsw_cp0, gsw_t0

use gsw_mod_toolbox, only : gsw_gibbs, gsw_pt_from_ct

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct
real (r8), intent(out), optional :: pt_sa, pt_ct

real (r8) :: abs_pt, ct_pt, ct_sa, pt

integer, parameter :: n0=0, n1=1, n2=2
real (r8), parameter :: pr0 = 0.0_r8

pt = gsw_pt_from_ct(sa,ct)
abs_pt = (gsw_t0 + pt)

ct_pt = -(abs_pt*gsw_gibbs(n0,n2,n0,sa,pt,pr0))/gsw_cp0

if (present(pt_sa)) then

    ct_sa = (gsw_gibbs(n1,n0,n0,sa,pt,pr0) -&
                abs_pt*gsw_gibbs(n1,n1,n0,sa,pt,pr0))/gsw_cp0

    pt_sa = -ct_sa/ct_pt

end if

if (present(pt_ct)) pt_ct = 1.0_r8/ct_pt

return
end subroutine

!--------------------------------------------------------------------------
