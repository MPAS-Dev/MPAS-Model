!==========================================================================
elemental subroutine gsw_t_freezing_first_derivatives_poly (sa, p, &
                            saturation_fraction, tfreezing_sa, tfreezing_p)
!==========================================================================
!
!  Calculates the frist derivatives of the in-situ temperature at which 
!  seawater freezes with respect to Absolute Salinity SA and pressure P (in
!  Pa).  These expressions come from differentiating the expression that
!  defines the freezing temperature, namely the equality between the 
!  chemical potentials of water in seawater and in ice.  
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar ) 
!  saturation_fraction = the saturation fraction of dissolved air in 
!                        seawater
!
!  tfreezing_SA = the derivative of the in-situ freezing temperature 
!                 (ITS-90) with respect to Absolute Salinity at fixed    
!                 pressure                     [ K/(g/kg) ] i.e. [ K kg/g ]
!
!  tfreezing_P  = the derivative of the in-situ freezing temperature  
!                 (ITS-90) with respect to pressure (in Pa) at fixed  
!                 Absolute Salinity                                [ K/Pa ]
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : gsw_sso

use gsw_mod_freezing_poly_coefficients

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, p, saturation_fraction
real (r8), intent(out), optional :: tfreezing_sa, tfreezing_p

real (r8) :: p_r, sa_r, x

real (r8), parameter :: c = 1e-3_r8/(2.0_r8*gsw_sso)

sa_r = sa*1e-2_r8
x = sqrt(sa_r)
p_r = p*1e-4_r8

if (present(tfreezing_sa)) tfreezing_sa = &
    (t1 + x*(1.5_r8*t2 + x*(2.0_r8*t3 + x*(2.5_r8*t4 + x*(3.0_r8*t5 &
        + 3.5_r8*t6*x)))) + p_r*(t10 + x*(1.5_r8*t11 + x*(2.0_r8*t13 &
        + x*(2.5_r8*t16 + x*(3.0_r8*t19 + 3.5_r8*t22*x)))) &
        + p_r*(t12 + x*(1.5_r8*t14 + x*(2.0_r8*t17 + 2.5_r8*t20*x)) &
        + p_r*(t15 + x*(1.5_r8*t18 + 2.0_r8*t21*x)))))*1e-2_r8 &
        + saturation_fraction*c

if (present(tfreezing_p)) tfreezing_p = &
    (t7 + sa_r*(t10 + x*(t11 + x*(t13 + x*(t16 + x*(t19 + t22*x))))) &
        + p_r*(2.0_r8*t8 + sa_r*(2.0_r8*t12 + x*(2.0_r8*t14 + x*(2.0_r8*t17 &
        + 2.0_r8*t20*x))) + p_r*(3.0_r8*t9 + sa_r*(3.0_r8*t15 + x*(3.0_r8*t18 &
        + 3.0_r8*t21*x)))))*1e-8_r8

return
end subroutine

!--------------------------------------------------------------------------
