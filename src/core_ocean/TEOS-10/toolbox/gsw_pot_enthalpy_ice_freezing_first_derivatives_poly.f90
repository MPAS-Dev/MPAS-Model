!==========================================================================
elemental subroutine gsw_pot_enthalpy_ice_freezing_first_derivatives_poly(&
         sa, p, pot_enthalpy_ice_freezing_sa, pot_enthalpy_ice_freezing_p)
!==========================================================================
!
!  Calculates the first derivatives of the potential enthalpy of ice Ih at
!  which ice melts into seawater with Absolute Salinity SA and at pressure 
!  p.  This code uses the comptationally efficient polynomial fit of the
!  freezing potential enthalpy of ice Ih (McDougall et al., 2015).
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar ) 
!
!  pot_enthalpy_ice_freezing_SA = the derivative of the potential enthalpy
!                of ice at freezing (ITS-90) with respect to Absolute
!                salinity at fixed pressure  [ (J/kg)/(g/kg) ] i.e. [ J/g ]
!                                            
!  pot_enthalpy_ice_freezing_P  = the derivative of the potential enthalpy
!                of ice at freezing (ITS-90) with respect to pressure 
!                (in Pa) at fixed Absolute Salinity           [ (J/kg)/Pa ]
!--------------------------------------------------------------------------

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, p
real (r8), intent(out), optional :: pot_enthalpy_ice_freezing_sa
real (r8), intent(out), optional :: pot_enthalpy_ice_freezing_p

real (r8) :: p_r, sa_r, x

real (r8), parameter :: d1 =  -1.249490228128056e4_r8
real (r8), parameter :: d2 =   1.336783910789822e4_r8
real (r8), parameter :: d3 =  -4.811989517774642e4_r8
real (r8), parameter :: d4 =   8.044864276240987e4_r8
real (r8), parameter :: d5 =  -7.124452125071862e4_r8
real (r8), parameter :: d6 =   2.280706828014839e4_r8
real (r8), parameter :: d7 =   0.315423710959628e3_r8
real (r8), parameter :: d8 =  -3.592775732074710e2_r8
real (r8), parameter :: d9 =   1.644828513129230e3_r8
real (r8), parameter :: d10 = -4.809640968940840e3_r8
real (r8), parameter :: d11 =  2.901071777977272e3_r8
real (r8), parameter :: d12 = -9.218459682855746e2_r8
real (r8), parameter :: d13 =  0.379377450285737e3_r8
real (r8), parameter :: d14 = -2.672164989849465e3_r8
real (r8), parameter :: d15 =  5.044317489422632e3_r8
real (r8), parameter :: d16 = -2.631711865886377e3_r8
real (r8), parameter :: d17 = -0.160245473297112e3_r8
real (r8), parameter :: d18 =  4.029061696035465e2_r8
real (r8), parameter :: d19 = -3.682950019675760e2_r8

real (r8), parameter :: f1 =  -2.034535061416256e4_r8
real (r8), parameter :: f2 =   0.315423710959628e3_r8
real (r8), parameter :: f3 =  -0.239518382138314e3_r8
real (r8), parameter :: f4 =   0.822414256564615e3_r8
real (r8), parameter :: f5 =  -1.923856387576336e3_r8
real (r8), parameter :: f6 =   0.967023925992424e3_r8
real (r8), parameter :: f7 =  -0.263384562367307e3_r8
real (r8), parameter :: f8 =  -5.051613740291480e3_r8
real (r8), parameter :: f9 =   7.587549005714740e2_r8
real (r8), parameter :: f10 = -3.562886653132620e3_r8
real (r8), parameter :: f11 =  5.044317489422632e3_r8
real (r8), parameter :: f12 = -2.105369492709102e3_r8
real (r8), parameter :: f13 =  6.387082316647800e2_r8
real (r8), parameter :: f14 = -4.807364198913360e2_r8
real (r8), parameter :: f15 =  8.058123392070929e2_r8
real (r8), parameter :: f16 = -5.524425029513641e2_r8

sa_r = sa*1e-2_r8
x = sqrt(sa_r)
p_r = p*1e-4_r8

if (present(pot_enthalpy_ice_freezing_sa)) pot_enthalpy_ice_freezing_sa = &
   (d1 + x*(d2  + x*(d3  + x*(d4  + x*(d5  + d6*x)))) &
       + p_r*(d7 + x*(d8 + x*(d9 + x*(d10 + x*(d11 + d12*x)))) & 
       + p_r*(d13 + x*(d14 + x*(d15 + d16*x)) &
       + p_r*(d17 + x*(d18 + d19*x)))))*1e-2_r8

if (present(pot_enthalpy_ice_freezing_p)) pot_enthalpy_ice_freezing_p = &
   (f1 + sa_r*(f2 + x*(f3 + x*(f4 + x*(f5 + x*(f6 + f7*x))))) &
       + p_r*(f8 + sa_r*(f9 + x*(f10 + x*(f11 + f12*x))) &
       + p_r*(f13 + sa_r*(f14 + x*(f15 + f16*x)))))*1e-8_r8

return
end subroutine

!--------------------------------------------------------------------------
