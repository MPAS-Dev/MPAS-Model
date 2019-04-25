!==========================================================================
elemental subroutine gsw_specvol_first_derivatives (sa, ct, p, v_sa, v_ct, &
                                                    v_p, iflag)
! =========================================================================
!
!  Calculates three first-order derivatives of specific volume (v).
!  Note that this function uses the computationally-efficient
!  expression for specific volume (Roquet et al., 2014).
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  CT  =  Conservative Temperature (ITS-90)                       [ deg C ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar )
!
!  v_SA  =  The first derivative of specific volume with respect to 
!           Absolute Salinity at constant CT & p.       [ J/(kg (g/kg)^2) ]
!  v_CT  =  The first derivative of specific volume with respect to 
!           CT at constant SA and p.                     [ J/(kg K(g/kg)) ]
!  v_P   =  The first derivative of specific volume with respect to 
!           P at constant SA and CT.                         [ J/(kg K^2) ]
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : gsw_sfac, offset

use gsw_mod_specvol_coefficients

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct, p
integer, intent(in), optional :: iflag
real (r8), intent(out), optional :: v_sa, v_ct, v_p

integer :: i
logical :: flags(3)
real (r8) :: v_ct_part, v_p_part, v_sa_part, xs, ys, z

xs = sqrt(gsw_sfac*sa + offset)
ys = ct*0.025_r8
z = p*1e-4_r8

if (present(iflag)) then
    do i = 1, 3
        flags(i) = btest(iflag,i)
    end do
else
    flags = .true.
end if

if (present(v_sa) .and. flags(1)) then

    v_sa_part = b000 + xs*(b100 + xs*(b200 + xs*(b300 + xs*(b400 + b500*xs)))) &
           + ys*(b010 + xs*(b110 + xs*(b210 + xs*(b310 + b410*xs))) &
           + ys*(b020 + xs*(b120 + xs*(b220 + b320*xs)) + ys*(b030 &
           + xs*(b130 + b230*xs) + ys*(b040 + b140*xs + b050*ys)))) &
           + z*(b001 + xs*(b101 + xs*(b201 + xs*(b301 + b401*xs))) &
           + ys*(b011 + xs*(b111 + xs*(b211 + b311*xs)) + ys*(b021 &
           + xs*(b121 + b221*xs) + ys*(b031 + b131*xs + b041*ys))) &
           + z*(b002 + xs*(b102 + xs*(b202 + b302*xs))+ ys*(b012 &
           + xs*(b112 + b212*xs) + ys*(b022 + b122*xs + b032*ys)) &
           + z*(b003 +  b103*xs + b013*ys + b004*z)))
 
    v_sa = 0.5_r8*gsw_sfac*v_sa_part/xs

end if


if (present(v_ct) .and. flags(2)) then

    v_ct_part = a000 + xs*(a100 + xs*(a200 + xs*(a300 + xs*(a400 + a500*xs)))) &
             + ys*(a010 + xs*(a110 + xs*(a210 + xs*(a310 + a410*xs))) &
             + ys*(a020 + xs*(a120 + xs*(a220 + a320*xs)) + ys*(a030 &
             + xs*(a130 + a230*xs) + ys*(a040 + a140*xs + a050*ys )))) &
             + z*(a001 + xs*(a101 + xs*(a201 + xs*(a301 + a401*xs))) &
             + ys*(a011 + xs*(a111 + xs*(a211 + a311*xs)) + ys*(a021 &
             + xs*(a121 + a221*xs) + ys*(a031 + a131*xs + a041*ys))) &
             + z*(a002 + xs*(a102 + xs*(a202 + a302*xs)) + ys*(a012 &
             + xs*(a112 + a212*xs) + ys*(a022 + a122*xs + a032*ys)) &
             + z*(a003 + a103*xs + a013*ys + a004*z))) 

    v_ct = 0.025_r8*v_ct_part

end if

if (present(v_p) .and. flags(3)) then

    v_p_part = c000 + xs*(c100 + xs*(c200 + xs*(c300 + xs*(c400 + c500*xs)))) & 
        + ys*(c010 + xs*(c110 + xs*(c210 + xs*(c310 + c410*xs))) + ys*(c020 &
        + xs*(c120 + xs*(c220 + c320*xs)) + ys*(c030 + xs*(c130 + c230*xs) &
        + ys*(c040 + c140*xs + c050*ys)))) + z*(c001 + xs*(c101 + xs*(c201 &
        + xs*(c301 + c401*xs))) + ys*(c011 + xs*(c111 + xs*(c211 + c311*xs)) &
        + ys*(c021 + xs*(c121 + c221*xs) + ys*(c031 + c131*xs + c041*ys))) &
        + z*( c002 + xs*(c102 + c202*xs) + ys*(c012 + c112*xs + c022*ys) &
        + z*(c003 + c103*xs + c013*ys + z*(c004 + c005*z))))

    v_p = 1e-8_r8*v_p_part

end if

return
end subroutine

!--------------------------------------------------------------------------
