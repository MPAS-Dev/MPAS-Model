!==========================================================================
elemental subroutine gsw_enthalpy_second_derivatives_ct_exact (sa, ct, p, &
                                                 h_sa_sa, h_sa_ct, h_ct_ct)
!==========================================================================
!
!  Calculates three second-order derivatives of specific enthalpy (h).
!  Note that this function uses the full Gibbs function.
!
!  sa  =  Absolute Salinity                                        [ g/kg ]
!  ct  =  Conservative Temperature (ITS-90)                       [ deg C ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar )
!
!  h_sa_sa  =  The second derivative of specific enthalpy with respect to 
!              Absolute Salinity at constant ct & p.    [ J/(kg (g/kg)^2) ]
!  h_sa_ct  =  The second derivative of specific enthalpy with respect to 
!              sa and ct at constant p.                  [ J/(kg K(g/kg)) ]
!  h_ct_ct  =  The second derivative of specific enthalpy with respect to 
!              ct at constant sa and p.                      [ J/(kg K^2) ]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_gibbs, gsw_pt_from_ct, gsw_pt_from_t

use gsw_mod_teos10_constants, only : gsw_cp0, gsw_t0

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct, p
real (r8), intent(out), optional :: h_sa_sa, h_sa_ct, h_ct_ct

real (r8) :: factor, gsa_pt0, gsat_pt0, gsat, part_b, pt0, h_ct_ct_val
real (r8) :: rec_abs_pt0, rec_gtt_pt0, rec_gtt, t, temp_ratio
real (r8) :: gsasa, gsasa_pt0

integer, parameter :: n0=0, n1=1, n2=2
real (r8), parameter :: pr0 = 0.0_r8, sa_small = 1e-100_r8

pt0 = gsw_pt_from_ct(sa,ct)
rec_abs_pt0 = 1.0_r8/(gsw_t0 + pt0)
t = gsw_pt_from_t(sa,pt0,pr0,p)
temp_ratio = (gsw_t0 + t)*rec_abs_pt0

rec_gtt_pt0 = 1.0_r8/gsw_gibbs(n0,n2,n0,sa,pt0,pr0)
rec_gtt = 1.0_r8/gsw_gibbs(n0,n2,n0,sa,t,p)

! h_ct_ct is naturally well-behaved as sa approaches zero. 
h_ct_ct_val = gsw_cp0*gsw_cp0* &
    (temp_ratio*rec_gtt_pt0 - rec_gtt)*(rec_abs_pt0*rec_abs_pt0)

if (present(h_ct_ct)) h_ct_ct = h_ct_ct_val

if (.not. present(h_sa_sa) .and. .not. present(h_sa_ct)) return

gsat_pt0 = gsw_gibbs(n1,n1,n0,sa,pt0,pr0)
gsat = gsw_gibbs(n1,n1,n0,sa,t,p)
gsa_pt0 = gsw_gibbs(n1,n0,n0,sa,pt0,pr0)

part_b = (temp_ratio*gsat_pt0*rec_gtt_pt0 - gsat*rec_gtt)*rec_abs_pt0
factor = gsa_pt0/gsw_cp0

if (present(h_sa_sa)) then

    gsasa = gsw_gibbs(n2,n0,n0,sa,t,p)
    gsasa_pt0 = gsw_gibbs(n2,n0,n0,sa,pt0,pr0)

    ! h_sa_sa has a singularity at sa = 0, and blows up as sa approaches zero.  
    h_sa_sa = gsasa - temp_ratio*gsasa_pt0  &
        + temp_ratio*gsat_pt0*gsat_pt0*rec_gtt_pt0  &
        - gsat*gsat*rec_gtt  &
        - 2.0_r8*gsa_pt0*part_b + (factor*factor)*h_ct_ct_val

end if
if (.not. present(h_sa_ct)) return

! h_sa_ct should not blow up as sa approaches zero.  The following lines
! of code ensure that the h_sa_ct output of this function does not blow
! up in this limit.  That is, when sa < 1e-100 g/kg, we force the h_sa_ct 
! output to be the same as if sa = 1e-100 g/kg.  
if (sa .lt. sa_small) then
    rec_gtt_pt0 = 1.0_r8/gsw_gibbs(n0,n2,n0,sa_small,pt0,pr0)
    rec_gtt = 1.0_r8/gsw_gibbs(n0,n2,n0,sa_small,t,p)
    gsat_pt0 = gsw_gibbs(n1,n1,n0,sa_small,pt0,pr0)
    gsat = gsw_gibbs(n1,n1,n0,sa_small,t,p)
    gsa_pt0 = gsw_gibbs(n1,n0,n0,sa_small,pt0,pr0)
    part_b = (temp_ratio*gsat_pt0*rec_gtt_pt0 - gsat*rec_gtt)*rec_abs_pt0
    factor = gsa_pt0/gsw_cp0
end if

h_sa_ct  = gsw_cp0*part_b - factor*h_ct_ct_val

return
end subroutine

!--------------------------------------------------------------------------
