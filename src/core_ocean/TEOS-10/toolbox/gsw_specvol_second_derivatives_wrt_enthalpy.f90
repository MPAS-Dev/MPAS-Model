!==========================================================================
elemental subroutine gsw_specvol_second_derivatives_wrt_enthalpy (sa, ct, &
                                          p, v_sa_sa, v_sa_h, v_h_h, iflag)
! =========================================================================
!
!  Calculates three first-order derivatives of specific volume (v) with
!  respect to enthalpy. Note that this function uses the using the
!  computationally-efficient expression for specific volume
!  (Roquet et al., 2014).
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  CT  =  Conservative Temperature (ITS-90)                       [ deg C ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar )
!
!  v_SA_SA = The second-order derivative of specific volume with respect to 
!            Absolute Salinity at constant h & p.       [ J/(kg (g/kg)^2) ]
!  v_SA_h  = The second-order derivative of specific volume with respect to 
!            SA and h at constant p.                     [ J/(kg K(g/kg)) ]
!  v_h_h   = The second-order derivative with respect to h at 
!            constant SA & p.
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_enthalpy_first_derivatives
use gsw_mod_toolbox, only : gsw_enthalpy_second_derivatives
use gsw_mod_toolbox, only : gsw_specvol_first_derivatives
use gsw_mod_toolbox, only : gsw_specvol_second_derivatives

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct, p
integer, intent(in), optional :: iflag
real (r8), intent(out), optional :: v_sa_sa, v_sa_h, v_h_h

logical :: flags(3)
real (r8) :: h_ct, h_ct_ct, h_sa, h_sa_ct, h_sa_sa, rec_h_ct, v_h_h_part
real (r8) :: rec_h_ct2, v_ct, vct_ct_ct, vct_sa_ct, vct_sa_sa, v_sa_h_part

if (present(iflag)) then
    flags(1) = present(v_sa_sa) .and. btest(iflag,1)
    flags(2) = present(v_sa_h) .and. btest(iflag,2)
    flags(3) = present(v_h_h) .and. btest(iflag,3)
else
    flags(1) = present(v_sa_sa)
    flags(2) = present(v_sa_h)
    flags(3) = present(v_h_h)
end if

call gsw_specvol_first_derivatives(sa,ct,p,v_ct=v_ct)

if (flags(1) .or. flags(2)) then
   call gsw_enthalpy_first_derivatives(sa,ct,p,h_sa,h_ct)
else
   call gsw_enthalpy_first_derivatives(sa,ct,p,h_ct=h_ct)
end if

if (flags(1)) then
   call gsw_specvol_second_derivatives(sa,ct,p,vct_sa_sa,vct_sa_ct,vct_ct_ct)
else if (flags(2)) then
   call gsw_specvol_second_derivatives(sa,ct,p,v_sa_ct=vct_sa_ct, &
                                                v_ct_ct=vct_ct_ct)
else
   call gsw_specvol_second_derivatives(sa,ct,p,v_ct_ct=vct_ct_ct)
end if

if (flags(1)) then
   call gsw_enthalpy_second_derivatives(sa,ct,p,h_sa_sa,h_sa_ct,h_ct_ct)
else if (flags(2)) then
   call gsw_enthalpy_second_derivatives(sa,ct,p,h_sa_ct=h_sa_ct,h_ct_ct=h_ct_ct)
else
   call gsw_enthalpy_second_derivatives(sa,ct,p,h_ct_ct=h_ct_ct)
end if

rec_h_ct = 1.0_r8/h_ct
rec_h_ct2 = rec_h_ct**2.0_r8

v_h_h_part = (vct_ct_ct*h_ct - h_ct_ct*v_ct)*(rec_h_ct2*rec_h_ct)

if (flags(3)) v_h_h = v_h_h_part

if (flags(1) .or. flags(2)) then

    v_sa_h_part = (vct_sa_ct*h_ct - v_ct*h_sa_ct)*rec_h_ct2 - h_sa*v_h_h_part

    if (flags(2)) v_sa_h = v_sa_h_part

    if (flags(1)) v_sa_sa = vct_sa_sa - (h_ct*(vct_sa_ct*h_sa &
            - v_ct*h_sa_sa) + v_ct*h_sa*h_sa_ct)*rec_h_ct2 - h_sa*v_sa_h_part
end if

return
end subroutine

!--------------------------------------------------------------------------
