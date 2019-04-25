!==========================================================================
elemental subroutine gsw_specvol_second_derivatives (sa, ct, p, v_sa_sa, &
                                   v_sa_ct, v_ct_ct, v_sa_p, v_ct_p, iflag)
! =========================================================================
!
!  Calculates five second-order derivatives of specific volume (v).
!  Note that this function uses the computationally-efficient
!  expression for specific volume (Roquet et al., 2014).
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  CT  =  Conservative Temperature (ITS-90)                       [ deg C ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar )
!
!  v_SA_SA  =  The second derivative of specific volume with respect to 
!              Absolute Salinity at constant CT & p.    [ J/(kg (g/kg)^2) ]
!  v_SA_CT  =  The second derivative of specific volume with respect to 
!              SA and CT at constant p.                  [ J/(kg K(g/kg)) ]
!  v_CT_CT  =  The second derivative of specific volume with respect to 
!              CT at constant SA and p.                      [ J/(kg K^2) ]
!  v_SA_P  =  The second derivative of specific volume with respect to 
!              SA and P at constant CT.                  [ J/(kg K(g/kg)) ]
!  v_CT_P  =  The second derivative of specific volume with respect to 
!              CT and P at constant SA.                  [ J/(kg K(g/kg)) ]
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : gsw_sfac, offset

use gsw_mod_specvol_coefficients

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct, p
integer, intent(in), optional :: iflag
real (r8), intent(out), optional :: v_sa_sa, v_sa_ct, v_ct_ct, v_sa_p, v_ct_p

integer :: i
logical :: flags(5)
real (r8) :: v_ct_ct_part, v_ct_p_part, v_sa_ct_part, v_sa_p_part
real (r8) :: v_sa_sa_part, xs, xs2, ys, z

xs2 = gsw_sfac*sa + offset
xs = sqrt(xs2)
ys = ct*0.025_r8
z = p*1e-4_r8

if (present(iflag)) then
    do i = 1, 5
        flags(i) = btest(iflag,i)
    end do
else
    flags = .true.
end if

if (present(v_sa_sa) .and. flags(1)) then

   v_sa_sa_part = (-b000 + xs2*(b200 + xs*(2.0_r8*b300 + xs*(3.0_r8*b400 &
       + 4.0_r8*b500*xs))) + ys*(-b010 + xs2*(b210 + xs*(2.0_r8*b310 &
       + 3.0_r8*b410*xs)) + ys*(-b020 + xs2*(b220 + 2.0_r8*b320*xs) &
       + ys*(-b030 + b230*xs2 + ys*(-b040 - b050*ys)))) + z*(-b001 &
       + xs2*(b201 + xs*(2.0_r8*b301 + 3.0_r8*b401*xs)) + ys*(-b011 &
       + xs2*(b211 + 2.0_r8*b311*xs) + ys*(-b021 + b221*xs2 &
       + ys*(-b031 - b041*ys))) + z*(-b002 + xs2*(b202 + 2.0_r8*b302*xs) &
       + ys*(-b012 + b212*xs2 + ys*(-b022 - b032*ys)) + z*(-b003 &
       - b013*ys - b004*z))))/xs2

   v_sa_sa = 0.25_r8*gsw_sfac*gsw_sfac*v_sa_sa_part/xs

end if

if (present(v_sa_ct) .and. flags(2)) then

   v_sa_ct_part = (b010 + xs*(b110 + xs*(b210 + xs*(b310 + b410*xs))) &
       + ys*(2.0_r8*(b020 + xs*(b120 + xs*(b220 + b320*xs))) &
       + ys*(3.0_r8*(b030 + xs*(b130 + b230*xs)) + ys*(4.0_r8*(b040 + b140*xs) &
       + 5.0_r8*b050*ys))) + z*(b011 + xs*(b111 + xs*(b211 + b311*xs)) &
       + ys*(2.0_r8*(b021 + xs*(b121 + b221*xs)) + ys*(3.0_r8*(b031 + b131*xs) &
       + 4.0_r8*b041*ys)) + z*(b012 + xs*(b112 + b212*xs) + ys*(2.0_r8*(b022 &
       + b122*xs) + 3.0_r8*b032*ys) + b013*z)))/xs

   v_sa_ct = 0.025_r8*0.5_r8*gsw_sfac*v_sa_ct_part

end if

if (present(v_ct_ct) .and. flags(3)) then

   v_ct_ct_part = a010 + xs*(a110 + xs*(a210 + xs*(a310 + a410*xs))) &
       + ys*(2.0_r8*(a020 + xs*(a120 + xs*(a220 + a320*xs))) &
       + ys*(3.0_r8*(a030 + xs*(a130 + a230*xs)) + ys*(4.0_r8*(a040 &
       + a140*xs) + 5.0_r8*a050*ys))) + z*( a011 + xs*(a111 + xs*(a211 &
       + a311*xs)) + ys*(2.0_r8*(a021 + xs*(a121 + a221*xs)) &
       + ys*(3.0_r8*(a031 + a131*xs) + 4.0_r8*a041*ys)) + z*(a012 &
       + xs*(a112 + a212*xs) + ys*(2.0_r8*(a022 + a122*xs) &
       + 3.0_r8*a032*ys) + a013*z))

   v_ct_ct = 0.025_r8*0.025_r8*v_ct_ct_part

end if

if (present(v_sa_p) .and. flags(4)) then

   v_sa_p_part = b001 + xs*(b101 + xs*(b201 + xs*(b301 + b401*xs))) + ys*(b011 &
       + xs*(b111 + xs*(b211 + b311*xs)) + ys*(b021 + xs*(b121 + b221*xs) &
       + ys*(b031 + b131*xs + b041*ys))) + z*(2.0_r8*(b002 + xs*(b102 &
       + xs*(b202 + b302*xs)) + ys*(b012 + xs*(b112 + b212*xs) + ys*(b022 &
       + b122*xs + b032*ys))) + z*(3.0_r8*(b003 + b103*xs + b013*ys) &
       + 4.0_r8*b004*z))

   v_sa_p = 1e-8_r8*0.5_r8*gsw_sfac*v_sa_p_part

end if

if (present(v_ct_p) .and. flags(5)) then

   v_ct_p_part = a001 + xs*(a101 + xs*(a201 + xs*(a301 + a401*xs))) + ys*(a011 &
        + xs*(a111 + xs*(a211 + a311*xs)) + ys*(a021 + xs*(a121 + a221*xs) &
        + ys*(a031 + a131*xs + a041*ys))) + z*(2.0_r8*(a002 + xs*(a102 &
        + xs*(a202 + a302*xs)) + ys*(a012 + xs*(a112 + a212*xs) + ys*(a022 &
        + a122*xs + a032*ys))) + z*(3.0_r8*(a003 + a103*xs + a013*ys) &
        + 4.0_r8*a004*z))

   v_ct_p = 1e-8_r8*0.025_r8*v_ct_p_part

end if

return
end subroutine

!--------------------------------------------------------------------------
