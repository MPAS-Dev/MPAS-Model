!==========================================================================
elemental subroutine gsw_ct_freezing_first_derivatives (sa, p, &
                          saturation_fraction, ctfreezing_sa, ctfreezing_p)
!==========================================================================
!
!  Calculates the first derivatives of the Conservative Temperature at
!  which seawater freezes, with respect to Absolute Salinity SA and
!  pressure P (in Pa).  
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar )
!  saturation_fraction = the saturation fraction of dissolved air in 
!                        seawater
!
!  CTfreezing_SA = the derivative of the Conservative Temperature at
!                  freezing (ITS-90) with respect to Absolute Salinity at
!                  fixed pressure              [ K/(g/kg) ] i.e. [ K kg/g ]
!
!  CTfreezing_P  = the derivative of the Conservative Temperature at
!                  freezing (ITS-90) with respect to pressure (in Pa) at
!                  fixed Absolute Salinity                         [ K/Pa ]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_ct_first_derivatives_wrt_t_exact
use gsw_mod_toolbox, only : gsw_t_freezing_first_derivatives
use gsw_mod_toolbox, only : gsw_t_freezing_exact

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, p, saturation_fraction
real (r8), intent(out), optional :: ctfreezing_sa, ctfreezing_p

real (r8) :: tf_sa, tf_p, ct_sa_wrt_t, ct_t_wrt_t, ct_p_wrt_t, tf

tf = gsw_t_freezing_exact(sa,p,saturation_fraction)

if (present(ctfreezing_sa) .and. present(ctfreezing_p)) then

    call gsw_t_freezing_first_derivatives(sa,p,saturation_fraction, &
                                          tfreezing_sa=tf_sa,tfreezing_p=tf_p)
    call gsw_ct_first_derivatives_wrt_t_exact(sa,tf,p, &
          ct_sa_wrt_t=ct_sa_wrt_t,ct_t_wrt_t=ct_t_wrt_t,ct_p_wrt_t=ct_p_wrt_t)

    ctfreezing_sa = ct_sa_wrt_t + ct_t_wrt_t*tf_sa
    ctfreezing_p  = ct_p_wrt_t  + ct_t_wrt_t*tf_p

else if (present(ctfreezing_sa) .and. .not. present(ctfreezing_p)) then

    call gsw_t_freezing_first_derivatives(sa,p,saturation_fraction, &
                                          tfreezing_sa=tf_sa)
    call gsw_ct_first_derivatives_wrt_t_exact(sa,tf,p, &
          ct_sa_wrt_t=ct_sa_wrt_t,ct_t_wrt_t=ct_t_wrt_t)

    ctfreezing_sa = ct_sa_wrt_t + ct_t_wrt_t*tf_sa

else if (.not. present(ctfreezing_sa) .and. present(ctfreezing_p)) then

    call gsw_t_freezing_first_derivatives(sa,p,saturation_fraction, &
                                          tfreezing_p=tf_p)
    call gsw_ct_first_derivatives_wrt_t_exact(sa,tf,p, &
          ct_t_wrt_t=ct_t_wrt_t,ct_p_wrt_t=ct_p_wrt_t)

    ctfreezing_p  = ct_p_wrt_t  + ct_t_wrt_t*tf_p

end if

return
end subroutine

!--------------------------------------------------------------------------
