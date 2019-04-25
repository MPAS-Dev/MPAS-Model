!==========================================================================
elemental subroutine gsw_rho_first_derivatives (sa, ct, p, drho_dsa, &
                                                drho_dct, drho_dp)
!==========================================================================
! 
!  Calculates the three (3) partial derivatives of in-situ density with 
!  respect to Absolute Salinity, Conservative Temperature and pressure.  
!  Note that the pressure derivative is done with respect to pressure in 
!  Pa, not dbar. This function uses the computationally-efficient expression
!  for specific volume in terms of SA, CT and p (Roquet et al., 2014).  
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  CT  =  Conservative Temperature (ITS-90)                       [ deg C ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar )
!
!  drho_dSA  =  partial derivatives of density             [ kg^2/(g m^3) ]
!                 with respect to Absolute Salinity
!  drho_dCT  =  partial derivatives of density               [ kg/(K m^3) ]
!                 with respect to Conservative Temperature
!  drho_dP   =  partial derivatives of density              [ kg/(Pa m^3) ]
!                 with respect to pressure in Pa
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : pa2db, gsw_sfac, offset

use gsw_mod_specvol_coefficients

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct, p
real (r8), intent(out), optional :: drho_dsa, drho_dct, drho_dp

real (r8) :: rho2, v_ct, v_p, v_sa, xs, ys, z, v

xs = sqrt(gsw_sfac*sa + offset)
ys = ct*0.025_r8
z = p*1e-4_r8

v = v000 + xs*(v010 + xs*(v020 + xs*(v030 + xs*(v040 + xs*(v050 &
    + v060*xs))))) + ys*(v100 + xs*(v110 + xs*(v120 + xs*(v130 + xs*(v140 &
    + v150*xs)))) + ys*(v200 + xs*(v210 + xs*(v220 + xs*(v230 + v240*xs))) &
    + ys*(v300 + xs*(v310 + xs*(v320 + v330*xs)) + ys*(v400 + xs*(v410 &
    + v420*xs) + ys*(v500 + v510*xs + v600*ys))))) + z*(v001 + xs*(v011 &
    + xs*(v021 + xs*(v031 + xs*(v041 + v051*xs)))) + ys*(v101 + xs*(v111 &
    + xs*(v121 + xs*(v131 + v141*xs))) + ys*(v201 + xs*(v211 + xs*(v221 &
    + v231*xs)) + ys*(v301 + xs*(v311 + v321*xs) + ys*(v401 + v411*xs &
    + v501*ys)))) + z*(v002 + xs*(v012 + xs*(v022 + xs*(v032 + v042*xs))) &
    + ys*(v102 + xs*(v112 + xs*(v122 + v132*xs)) + ys*(v202 + xs*(v212 &
    + v222*xs) + ys*(v302 + v312*xs + v402*ys))) + z*(v003 + xs*(v013 &
    + v023*xs) + ys*(v103 + v113*xs + v203*ys) + z*(v004 + v014*xs + v104*ys &
    + z*(v005 + v006*z)))))

rho2 = (1.0_r8/v)**2

if (present(drho_dsa)) then

    v_sa = b000 + xs*(b100 + xs*(b200 + xs*(b300 + xs*(b400 + b500*xs)))) &
           + ys*(b010 + xs*(b110 + xs*(b210 + xs*(b310 + b410*xs))) &
           + ys*(b020 + xs*(b120 + xs*(b220 + b320*xs)) + ys*(b030 &
           + xs*(b130 + b230*xs) + ys*(b040 + b140*xs + b050*ys)))) &
           + z*(b001 + xs*(b101 + xs*(b201 + xs*(b301 + b401*xs))) &
           + ys*(b011 + xs*(b111 + xs*(b211 + b311*xs)) + ys*(b021 &
           + xs*(b121 + b221*xs) + ys*(b031 + b131*xs + b041*ys))) &
           + z*(b002 + xs*(b102 + xs*(b202 + b302*xs))+ ys*(b012 &
           + xs*(b112 + b212*xs) + ys*(b022 + b122*xs + b032*ys)) &
           + z*(b003 +  b103*xs + b013*ys + b004*z)))

    drho_dsa = -rho2*0.5_r8*gsw_sfac*v_sa/xs

end if

if (present(drho_dct)) then

    v_ct = a000 + xs*(a100 + xs*(a200 + xs*(a300 + xs*(a400 + a500*xs)))) &
           + ys*(a010 + xs*(a110 + xs*(a210 + xs*(a310 + a410*xs))) &
           + ys*(a020 + xs*(a120 + xs*(a220 + a320*xs)) + ys*(a030 &
           + xs*(a130 + a230*xs) + ys*(a040 + a140*xs + a050*ys )))) &
           + z*(a001 + xs*(a101 + xs*(a201 + xs*(a301 + a401*xs))) &
           + ys*(a011 + xs*(a111 + xs*(a211 + a311*xs)) + ys*(a021 &
           + xs*(a121 + a221*xs) + ys*(a031 + a131*xs + a041*ys))) &
           + z*(a002 + xs*(a102 + xs*(a202 + a302*xs)) + ys*(a012 &
           + xs*(a112 + a212*xs) + ys*(a022 + a122*xs + a032*ys)) &
           + z*(a003 + a103*xs + a013*ys + a004*z))) 

    drho_dct = -rho2*0.025_r8*v_ct

end if

if (present(drho_dp)) then

    v_p = c000 + xs*(c100 + xs*(c200 + xs*(c300 + xs*(c400 + c500*xs)))) &
          + ys*(c010 + xs*(c110 + xs*(c210 + xs*(c310 + c410*xs))) + ys*(c020 &
          + xs*(c120 + xs*(c220 + c320*xs)) + ys*(c030 + xs*(c130 + c230*xs) &
          + ys*(c040 + c140*xs + c050*ys)))) + z*(c001 + xs*(c101 + xs*(c201 &
          + xs*(c301 + c401*xs))) + ys*(c011 + xs*(c111 + xs*(c211 + c311*xs)) &
          + ys*(c021 + xs*(c121 + c221*xs) + ys*(c031 + c131*xs + c041*ys))) &
          + z*(c002 + xs*(c102 + c202*xs) + ys*(c012 + c112*xs + c022*ys) &
          + z*(c003 + c103*xs + c013*ys + z*(c004 + c005*z))))
  
    drho_dp = -rho2*1e-4_r8*pa2db*v_p

end if

return
end subroutine

!--------------------------------------------------------------------------
