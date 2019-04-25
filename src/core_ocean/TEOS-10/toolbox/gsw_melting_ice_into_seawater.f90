!==========================================================================
elemental subroutine gsw_melting_ice_into_seawater (sa, ct, p, w_ih, t_ih,&
                                            sa_final, ct_final, w_ih_final)
!==========================================================================
!
!  Calculates the final Absolute Salinity, final Conservative Temperature 
!  and final ice mass fraction that results when a given mass fraction of 
!  ice melts and is mixed into seawater whose properties are (SA,CT,p).  
!  This code takes the seawater to contain no dissolved air.  
!
!  When the mass fraction w_Ih_final is calculated as being a positive
!  value, the seawater-ice mixture is at thermodynamic equlibrium.  
!
!  This code returns w_Ih_final = 0 when the input bulk enthalpy, h_bulk, 
!  is sufficiently large (i.e. sufficiently "warm") so that there is no ice 
!  present in the final state.  In this case the final state consists of 
!  only seawater rather than being an equlibrium mixture of seawater and 
!  ice which occurs when w_Ih_final is positive.  Note that when 
!  w_Ih_final = 0, the final seawater is not at the freezing temperature. 
!
!  SA   =  Absolute Salinity of seawater                           [ g/kg ]
!  CT   =  Conservative Temperature of seawater (ITS-90)          [ deg C ]
!  p    =  sea pressure at which the melting occurs                [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar ) 
!  w_Ih =  mass fraction of ice, that is the mass of ice divided by the
!          sum of the masses of ice and seawater.  That is, the mass of 
!          ice divided by the mass of the final mixed fluid.  
!          w_Ih must be between 0 and 1.                       [ unitless ]
!  t_Ih =  the in-situ temperature of the ice (ITS-90)            [ deg C ]
!
!  SA_final    =  Absolute Salinity of the seawater in the final state, 
!                 whether or not any ice is present.               [ g/kg ]
!  CT_final    =  Conservative Temperature of the seawater in the the final
!                 state, whether or not any ice is present.       [ deg C ]
!  w_Ih_final  =  mass fraction of ice in the final seawater-ice mixture.
!                 If this ice mass fraction is positive, the system is at 
!                 thermodynamic equilibrium.  If this ice mass fraction is 
!                 zero there is no ice in the final state which consists 
!                 only of seawater which is warmer than the freezing 
!                 temperature.                                   [unitless]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_ct_freezing_exact, gsw_enthalpy_ice
use gsw_mod_toolbox, only : gsw_frazil_properties, gsw_t_freezing_exact
use gsw_mod_toolbox, only : gsw_enthalpy_ct_exact

use gsw_mod_error_functions, only : gsw_error_code, gsw_error_limit

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct, p, w_ih, t_ih
real (r8), intent(out) :: sa_final, ct_final, w_ih_final

real (r8) :: ctf, h_bulk, sa_bulk, tf_ih

real (r8), parameter :: saturation_fraction = 0.0_r8

character (*), parameter :: func_name = "gsw_melting_ice_into_seawater"

ctf = gsw_ct_freezing_exact(sa,p,saturation_fraction)
if (ct .lt. ctf) then
    ! The seawater ct input is below the freezing temp
    sa_final = gsw_error_code(1,func_name)
    ct_final = sa_final
    w_ih_final = sa_final
    return
end if

tf_ih = gsw_t_freezing_exact(0.0_r8,p,saturation_fraction) - 1e-6_r8
if (t_ih .gt. tf_ih) then
    ! t_ih input exceeds the freezing temp. The 1e-6 C buffer in the allowable
    ! t_Ih is to ensure that there is some ice Ih in the sea ice.  
    sa_final = gsw_error_code(2,func_name)
    ct_final = sa_final
    w_ih_final = sa_final
    return
end if

sa_bulk = (1.0_r8 - w_ih)*sa
h_bulk = (1.0_r8 - w_ih)*gsw_enthalpy_ct_exact(sa,ct,p) &
                  + w_ih*gsw_enthalpy_ice(t_ih,p)
call gsw_frazil_properties(sa_bulk,h_bulk,p,sa_final,ct_final,w_ih_final)
if (sa_final .gt. gsw_error_limit) then
    sa_final = gsw_error_code(3,func_name,sa_final)
    ct_final = sa_final
    w_ih_final = sa_final
    return
endif
        
return
end subroutine

!--------------------------------------------------------------------------
