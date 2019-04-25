!==========================================================================
elemental function gsw_enthalpy_ct_exact (sa, ct, p)
!==========================================================================
!
!  Calculates specific enthalpy of seawater from Absolute Salinity and 
!  Conservative Temperature and pressure.  
!
!  Note that this function uses the full Gibbs function.
!    
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  CT  =  Conservative Temperature (ITS-90)                       [ deg C ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar )
!
!  enthalpy_CT_exact  =  specific enthalpy                         [ J/kg ]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_enthalpy_t_exact, gsw_t_from_ct

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct, p

real (r8) :: gsw_enthalpy_ct_exact

real (r8) :: t

t = gsw_t_from_ct(sa,ct,p) 
gsw_enthalpy_ct_exact = gsw_enthalpy_t_exact(sa,t,p)

return
end function

!--------------------------------------------------------------------------
