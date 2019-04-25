!==========================================================================
elemental subroutine gsw_specvol_first_derivatives_wrt_enthalpy (sa, ct, &
                                                       p, v_sa, v_h, iflag)
! =========================================================================
!
!  Calculates two first-order derivatives of specific volume (v).
!  Note that this function uses the using the computationally-efficient
!  expression for specific volume (Roquet et al., 2014).
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  CT  =  Conservative Temperature (ITS-90)                       [ deg C ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar )
!
!  v_SA  =  The first derivative of specific volume with respect to 
!              Absolute Salinity at constant CT & p.    [ J/(kg (g/kg)^2) ]
!  v_h  =  The first derivative of specific volume with respect to 
!              SA and CT at constant p.                  [ J/(kg K(g/kg)) ]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_enthalpy_first_derivatives
use gsw_mod_toolbox, only : gsw_specvol_first_derivatives

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct, p
integer, intent(in), optional :: iflag
real (r8), intent(out), optional :: v_sa, v_h

integer :: i
logical :: flags(2)
real (r8) :: h_ct, h_sa, rec_h_ct, vct_ct, vct_sa

if (present(iflag)) then
    do i = 1, 2
        flags(i) = btest(iflag,i)
    end do
else
    flags = .true.
end if

if (present(v_sa) .and. flags(1)) then

    call gsw_specvol_first_derivatives(sa,ct,p,vct_sa,vct_ct)
    call gsw_enthalpy_first_derivatives(sa,ct,p,h_sa,h_ct)

else if (present(v_h) .and. flags(2)) then

    call gsw_specvol_first_derivatives(sa,ct,p,v_ct=vct_ct)
    call gsw_enthalpy_first_derivatives(sa,ct,p,h_ct=h_ct)

end if

rec_h_ct = 1.0_r8/h_ct

if (present(v_sa) .and. flags(1)) v_sa = vct_sa - (vct_ct*h_sa)*rec_h_ct

if (present(v_h) .and. flags(2)) v_h = vct_ct*rec_h_ct

return
end subroutine

!--------------------------------------------------------------------------
