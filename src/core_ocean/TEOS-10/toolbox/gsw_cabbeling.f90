!==========================================================================
elemental function gsw_cabbeling (sa, ct, p)
!==========================================================================
!
!  Calculates the cabbeling coefficient of seawater with respect to  
!  Conservative Temperature.  This function uses the computationally-
!  efficient expression for specific volume in terms of SA, CT and p
!  (Roquet et al., 2014).
!   
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  CT  =  Conservative Temperature (ITS-90)                       [ deg C ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar )
!
!  cabbeling  =  cabbeling coefficient with respect to            [ 1/K^2 ]
!                Conservative Temperature.                    
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_alpha_on_beta, gsw_rho
use gsw_mod_toolbox, only : gsw_specvol_first_derivatives
use gsw_mod_toolbox, only : gsw_specvol_second_derivatives

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct, p

real (r8) :: gsw_cabbeling

real (r8) :: alpha_ct, alpha_on_beta, alpha_sa, beta_sa, rho
real (r8) :: v_sa, v_ct, v_sa_sa, v_sa_ct, v_ct_ct

call gsw_specvol_first_derivatives(sa,ct,p,v_sa,v_ct)
     
call gsw_specvol_second_derivatives(sa,ct,p,v_sa_sa,v_sa_ct,v_ct_ct)

rho = gsw_rho(sa,ct,p)
     
alpha_ct = rho*(v_ct_ct - rho*v_ct*v_ct)

alpha_sa = rho*(v_sa_ct - rho*v_sa*v_ct)

beta_sa = -rho*(v_sa_sa - rho*v_sa*v_sa)

alpha_on_beta = gsw_alpha_on_beta(sa,ct,p)

gsw_cabbeling = alpha_ct + &
                alpha_on_beta*(2.0_r8*alpha_sa - alpha_on_beta*beta_sa)

return
end function

!--------------------------------------------------------------------------
