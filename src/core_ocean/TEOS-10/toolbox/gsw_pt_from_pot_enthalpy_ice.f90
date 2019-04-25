!==========================================================================
elemental function gsw_pt_from_pot_enthalpy_ice (pot_enthalpy_ice)
!==========================================================================
!
!  Calculates the potential temperature of ice from the potential enthalpy 
!  of ice.  The reference sea pressure of both the potential temperature 
!  and the potential enthalpy is zero dbar. 
!
!  pot_enthalpy_ice  =  potential enthalpy of ice                  [ J/kg ]
!
!  pt0_ice  =  potential temperature of ice (ITS-90)              [ deg C ]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_cp_ice, gsw_pot_enthalpy_from_pt_ice
use gsw_mod_toolbox, only : gsw_pt_from_pot_enthalpy_ice_poly_dh
use gsw_mod_toolbox, only : gsw_pt_from_pot_enthalpy_ice_poly
use gsw_mod_toolbox, only : gsw_pt0_cold_ice_poly

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: pot_enthalpy_ice

real (r8) :: gsw_pt_from_pot_enthalpy_ice

integer :: iteration
real (r8) :: df_dt, f, mod_pot_enthalpy_ice, pt0_cold_ice, recip_df_dt
real (r8) :: pt0_cold_ice_old, pt0_ice, pt0_ice_old, ptm_cold_ice, ptm_ice

real (r8), parameter :: h00 = -6.320202333358860e5_r8 !gsw_enthalpy_ice(-gsw_t0,0)
real (r8), parameter :: p0 = 0.0_r8

mod_pot_enthalpy_ice = max(pot_enthalpy_ice,h00)
   
if (mod_pot_enthalpy_ice.ge.-5.1e5_r8) then

    ! For input potential enthalpies greater than -5.1e-5, the above part of
    ! the code gives the output potential temperature of ice accurate to 1e-13
    ! degrees C.

    pt0_ice = gsw_pt_from_pot_enthalpy_ice_poly(mod_pot_enthalpy_ice)

    ! The variable "df_dt" below is the derivative of the above polynomial with
    ! respect to pot_enthalpy_ice.  This is the initial value of the derivative
    ! of the function f.
    recip_df_dt = gsw_pt_from_pot_enthalpy_ice_poly_dh(mod_pot_enthalpy_ice)

    pt0_ice_old = pt0_ice
    f = gsw_pot_enthalpy_from_pt_ice(pt0_ice_old) - mod_pot_enthalpy_ice
    pt0_ice = pt0_ice_old - f*recip_df_dt
    ptm_ice = 0.5_r8*(pt0_ice + pt0_ice_old)
    recip_df_dt = 1.0_r8/gsw_cp_ice(ptm_ice,p0)
    pt0_ice = pt0_ice_old - f*recip_df_dt

else

    ! For  pot_enthalpy_ice < -5.1e5 (or pt0_ice less than about -100 deg c)
    ! these temperatures are less than those found in nature on planet earth. 
    
    pt0_cold_ice = gsw_pt0_cold_ice_poly(mod_pot_enthalpy_ice) 
    
    df_dt = gsw_cp_ice(pt0_cold_ice+0.02_r8,p0)
    ! the heat capacity, cp, is
    ! evaluated at 0.02 c greater than usual in order to avoid stability
    ! issues and to ensure convergence near zero absolute temperature. 
    
    do iteration = 1, 6  
        pt0_cold_ice_old = pt0_cold_ice
        f = gsw_pot_enthalpy_from_pt_ice(pt0_cold_ice_old)-mod_pot_enthalpy_ice
        pt0_cold_ice = pt0_cold_ice_old - f/df_dt
        ptm_cold_ice = 0.5_r8*(pt0_cold_ice + pt0_cold_ice_old)        
        df_dt = gsw_cp_ice(ptm_cold_ice+0.02_r8,p0)
	! note the extra 0.02 c here as well    
        pt0_cold_ice = pt0_cold_ice_old - f/df_dt
    end do
    pt0_ice = pt0_cold_ice

end if

!The potential temerature has a maximum error as listed in the table below.
!
!  potential temerature error (deg C)  |  @ potential temerature (deg C)
!--------------------------------------|--------------------------------
!                0.012                 |     -273.15 to -273.12
!              4 x 10^-4               |     -232.12 to -273.0
!             2.5 x 10^-6              |          -273
!              7 x 10^-9               |          -272
!            3.7 x 10^-10              |          -270
!              6 x 10^-11              |          -268
!             2.5 x 10^11              |          -266
!             3 x 10^-12               |          -260
!             7 x 10^-13               |          -250
!            2.2 x 10^-13              |          -220
!            1.7 x 10^-13              |         >= -160
!  
! Note.  The above errors in each temperature range are machine precissions 
! for this calculation.

gsw_pt_from_pot_enthalpy_ice = pt0_ice

return
end function

!--------------------------------------------------------------------------
