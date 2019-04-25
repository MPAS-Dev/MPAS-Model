program gsw_poly_check

use gsw_mod_kinds
use gsw_mod_toolbox
use gsw_mod_check_data

implicit none

integer :: gsw_error_flag = 0

integer :: i

real (r8) :: saturation_fraction

real (r8), dimension(:,:), allocatable :: lat, long

real (r8), dimension(:,:), allocatable :: val1, val2, val3, val4, val5, val6

real (r8), dimension(:,:), allocatable :: c, sr, sstar, pt, entropy
real (r8), dimension(:,:), allocatable :: h, ctf, tf, diff
real (r8), dimension(:,:), allocatable :: ctf_poly, tf_poly, pt0

allocate(lat(cast_m,cast_n))
allocate(long(cast_m,cast_n))
allocate(pt0(cast_ice_m,cast_ice_n))

allocate(val1(cast_m,cast_n))
allocate(val2(cast_m,cast_n))
allocate(val3(cast_m,cast_n))
allocate(val4(cast_m,cast_n))
allocate(val5(cast_m,cast_n))
allocate(val6(cast_m,cast_n))

allocate(c(cast_m,cast_n))
allocate(sr(cast_m,cast_n))
allocate(sstar(cast_m,cast_n))
allocate(pt(cast_m,cast_n))
allocate(entropy(cast_m,cast_n))
allocate(ctf(cast_m,cast_n))
allocate(tf(cast_m,cast_n))
allocate(ctf_poly(cast_m,cast_n))
allocate(tf_poly(cast_m,cast_n))
allocate(h(cast_m,cast_n))
allocate(diff(cast_m,cast_n))

do i = 1, cast_n
    lat(:,i) = lat_cast(i)
    long(:,i) = long_cast(i)
end do

!------------------------------------------------------------------------------
call section_title('Freezing temperatures')

saturation_fraction = 0.5_r8

ctf = gsw_ct_freezing_exact(sa,p,saturation_fraction)
ctf_poly = gsw_ct_freezing_poly(sa,p,saturation_fraction)
call check_accuracy('CT_freezing',ctf,ctf_poly)

tf = gsw_t_freezing_exact(sa,p,saturation_fraction)
tf_poly = gsw_t_freezing_poly(sa,p,saturation_fraction)
call check_accuracy('t_freezing',tf,tf_poly)

val1 = gsw_pot_enthalpy_ice_freezing(sa,p)
val2 = gsw_pot_enthalpy_ice_freezing_poly(sa,p)
call check_accuracy('pot_enthalpy_ice_freezing',val1,val2)

val1 = gsw_sa_freezing_from_ct(ctf,p,saturation_fraction)
val2 = gsw_sa_freezing_from_ct_poly(ctf,p,saturation_fraction)
call check_accuracy('SA_freezing_from_CT',val1,val2)

val1 = gsw_sa_freezing_from_t(tf,p,saturation_fraction)
val2 = gsw_sa_freezing_from_t_poly(tf,p,saturation_fraction)
call check_accuracy('SA_freezing_from_t',val1,val2)

call gsw_ct_freezing_first_derivatives(sa,p,saturation_fraction,val1,val2)
call gsw_ct_freezing_first_derivatives_poly(sa,p,saturation_fraction,val3,val4)
call check_accuracy('CT_freezing_first_derivatives (ctf_sa)',val1,val3)
call check_accuracy('CT_freezing_first_derivatives (ctf_p)',val2,val4)

call gsw_t_freezing_first_derivatives(sa,p,saturation_fraction,val1,val2)
call gsw_t_freezing_first_derivatives_poly(sa,p,saturation_fraction,val3,val4)
call check_accuracy('t_freezing_first_derivatives (tf_sa)',val1,val3)
call check_accuracy('t_freezing_first_derivatives (tf_p)',val2,val4)

call gsw_pot_enthalpy_ice_freezing_first_derivatives(sa,p,val1,val2)
call gsw_pot_enthalpy_ice_freezing_first_derivatives_poly(sa,p,val3,val4)
call check_accuracy('pot_enthalpy_ice_freezing_first_derivatives (sa)',val1,val3)
call check_accuracy('pot_enthalpy_ice_freezing_first_derivatives (p)',val2,val4)

!------------------------------------------------------------------------------
call section_title('Themodynamic properties of ice Ih')

deallocate(h,val1,val2,val3,val4,val5,val6)
allocate(h(cast_ice_m,cast_ice_n))
allocate(val1(cast_ice_m,cast_ice_n))
allocate(val2(cast_ice_m,cast_ice_n))
allocate(val3(cast_ice_m,cast_ice_n))
allocate(val4(cast_ice_m,cast_ice_n))
allocate(val5(cast_ice_m,cast_ice_n))
allocate(val6(cast_ice_m,cast_ice_n))

pt0 = gsw_pt0_from_t_ice(t_seaice,p_arctic)
h = gsw_pot_enthalpy_from_pt_ice(pt0)

val1 = gsw_pot_enthalpy_from_pt_ice(pt0)
val2 = gsw_pot_enthalpy_from_pt_ice_poly(pt0)
call check_accuracy('pot_enthalpy_from_pt_ice',val1,val2)

val1 = gsw_pt_from_pot_enthalpy_ice(h)
val2 = gsw_pt_from_pot_enthalpy_ice_poly(h)
call check_accuracy('pt_from_pot_enthalpy_ice',val1,val2)

!------------------------------------------------------------------------------
call section_title('Thermodynamic interaction between ice and seawater')

saturation_fraction = 0.0_r8

val1 = gsw_melting_ice_sa_ct_ratio(sa_arctic,ct_arctic,p_arctic,t_ice)
val2 = gsw_melting_ice_sa_ct_ratio_poly(sa_arctic,ct_arctic,p_arctic,t_ice)
call check_accuracy('melting_ice_SA_CT_ratio',val1,val2)

val1 = gsw_melting_ice_equilibrium_sa_ct_ratio(sa_arctic,p_arctic)
val2 = gsw_melting_ice_equilibrium_sa_ct_ratio_poly(sa_arctic,p_arctic)
call check_accuracy('melting_ice_equilibrium_SA_CT_ratio',val1,val2)

call gsw_frazil_ratios_adiabatic(sa_arctic,p_arctic,w_ice,val1,val2,val3)
call gsw_frazil_ratios_adiabatic_poly(sa_arctic,p_arctic,w_ice,val4,val5,val6)
call check_accuracy('frazil_ratios_adiabatic (dsa_dct)',val1,val4)
call check_accuracy('frazil_ratios_adiabatic (dsa_dp)',val2,val5)
call check_accuracy('frazil_ratios_adiabatic (dct_dp)',val3,val6)

call gsw_frazil_properties_potential(sa_bulk,h_pot_bulk,p_arctic,val1,val2,val3)
call gsw_frazil_properties_potential_poly(sa_bulk,h_pot_bulk,p_arctic,val4, &
                                          val5,val6)
call check_accuracy('frazil_properties_potential (sa_final)',val1,val4)
call check_accuracy('frazil_properties_potential (ct_final)',val2,val5)
call check_accuracy('frazil_properties_potential (w_ih_final)',val3,val6)

!------------------------------------------------------------------------------
call section_title('Thermodynamic interaction between seaice and seawater')

val1 = gsw_melting_seaice_sa_ct_ratio(sa_arctic,ct_arctic,p_arctic, &
                                      sa_seaice,t_seaice)
val2 = gsw_melting_seaice_sa_ct_ratio_poly(sa_arctic,ct_arctic,p_arctic, &
                                           sa_seaice,t_seaice)
call check_accuracy('melting_seaice_SA_CT_ratio',val1,val2)

val1 = gsw_melting_seaice_equilibrium_sa_ct_ratio(sa_arctic,p_arctic)
val2 = gsw_melting_seaice_equilibrium_sa_ct_ratio_poly(sa_arctic,p_arctic)
call check_accuracy('melting_seaice_equilibrium_SA_CT_ratio',val1,val2)

call gsw_melting_seaice_into_seawater(sa_arctic,ct_arctic,p_arctic, &
                                      w_seaice,sa_seaice,t_seaice,val1,val2)
!call check_accuracy('melting_seaice_into_seawater',val1, &
!                    'melting_seaice_into_seawater_SA_final')
!call check_accuracy('melting_seaice_into_seawater',val2, &
!                    'melting_seaice_into_seawater_CT_final')

call gsw_seaice_fraction_to_freeze_seawater(sa_arctic,ct_arctic,p_arctic, &
                                            sa_seaice,t_seaice,val1,val2,val3)
!call check_accuracy('seaice_fraction_to_freeze_seawater',val1, &
!                    'seaice_fraction_to_freeze_seawater_SA_freeze')
!call check_accuracy('seaice_fraction_to_freeze_seawater',val2, &
!                    'seaice_fraction_to_freeze_seawater_CT_freeze')
!call check_accuracy('seaice_fraction_to_freeze_seawater',val3, &
!                    'seaice_fraction_to_freeze_seawater_w_Ih')

!------------------------------------------------------------------------------
if (gsw_error_flag.eq.1) then
  print*
  print*; print*, 'Your installation of the Gibbs SeaWater (GSW) Oceanographic Toolbox has errors!'
else  
  print*
  print*; print*, 'Well done! The gsw_check_fuctions confirms that the Gibbs'
  print*; print*, 'SeaWater (GSW) Oceanographic Toolbox is installed correctly.'
  print*
endif

contains

    !--------------------------------------------------------------------------

    subroutine section_title (title)

    character (*), intent(in) :: title

    print *
    print *, "----------------------------------------------------------------------------"
    print *, title
    print *

    return
    end subroutine section_title

    !--------------------------------------------------------------------------

    subroutine check_accuracy (func_name, fvalue1, fvalue2, vprint)

    use gsw_mod_error_functions, only : gsw_error_limit

    implicit none

    character (*), intent(in) :: func_name
    real (r8), intent(in) :: fvalue1(:,:), fvalue2(:,:)
    logical, intent(in), optional :: vprint

    integer :: ndots, i, j
    real (r8) :: diff(size(fvalue1,1),size(fvalue1,2))
    character (len(func_name)+3) :: message
    character (4) :: errflg

    character (*), parameter :: att_name = 'computation_accuracy'
    character (*), parameter :: &
        dots = ' .............................................................'

    message = func_name

    diff = fvalue1 - fvalue2

    if (present(vprint)) then
        if (vprint) then
            print '(i3,3ES24.15)', ((i,fvalue1(i,j),fvalue2(i,j),diff(i,j),&
                    i=1,size(fvalue1,1)), j=1,size(fvalue1,2))
            print *
        end if
    end if

    if (any(fvalue1 .gt. gsw_error_limit)) then
        where (fvalue1 .gt. gsw_error_limit) diff = 0.0_r8
        errflg = ' (*)'
    else
        errflg = '    '
    end if
    ndots = 52 - len(trim(message))

    print '(1x,2a,2es12.3)', trim(message), dots(:ndots), minval(diff), &
                             maxval(diff)

    return
    end subroutine check_accuracy

end

!--------------------------------------------------------------------------
