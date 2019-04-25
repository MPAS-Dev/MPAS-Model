program gsw_check_functions

use gsw_mod_kinds
use gsw_mod_toolbox

use gsw_mod_error_functions, only : gsw_error_code, gsw_error_limit
use gsw_mod_check_data

implicit none

integer :: gsw_error_flag = 0

integer :: i, n

real (r8) :: saturation_fraction

real (r8), dimension(:,:), allocatable :: lat, long


real (r8), dimension(:,:), allocatable :: value
real (r8), dimension(:,:), allocatable :: val1, val2, val3, val4, val5
real (r8), dimension(:,:), allocatable :: val6, val7, val8


real (r8), dimension(:,:), allocatable :: c, r, sr, sstar, pt, entropy
real (r8), dimension(:,:), allocatable :: h, ctf, tf, diff, z
real (r8), dimension(:,:), allocatable :: ctf_poly, tf_poly, pt0


allocate(lat(cast_m,cast_n))
allocate(long(cast_m,cast_n))
allocate(pt0(cast_ice_m,cast_ice_n))

allocate(value(cast_m,cast_n))
allocate(val1(cast_m,cast_n))
allocate(val2(cast_m,cast_n))
allocate(val3(cast_m,cast_n))
allocate(val4(cast_m,cast_n))
allocate(val5(cast_m,cast_n))

allocate(c(cast_m,cast_n))
allocate(r(cast_m,cast_n))
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
allocate(z(cast_m,cast_n))

do i = 1, cast_n
    lat(:,i) = lat_cast(i)
    long(:,i) = long_cast(i)
end do


print*
print*,'============================================================================'
print*
print*,' Gibbs SeaWater (GSW) Oceanographic Toolbox of TEOS-10 (Fortran)'
print*
print*,'============================================================================'
print*
print*,' These are the check values for the subset of functions that have been '
print*
print*,' converted into FORTRAN 95 from the Gibbs SeaWater (GSW) Oceanographic '
print*
print*,' Toolbox of TEOS-10.'
print*

!------------------------------------------------------------------------------
call section_title('Practical Salinity, PSS-78')

c = gsw_c_from_sp(sp,t,p)
call check_accuracy('C_from_SP',c,c_from_sp)

value = gsw_sp_from_c(c,t,p)
call check_accuracy('SP_from_C',value,sp_from_c)

value = gsw_sp_from_sk(sk)
call check_accuracy('SP_from_SK',value,sp_from_sk)

!------------------------------------------------------------------------------
call section_title('Absolute Salinity, Preformed Salinity and Conservative Temperature')

value = gsw_sa_from_sp(sp,p,long,lat)
call check_accuracy('SA_from_SP',value,sa_from_sp)

value = gsw_sstar_from_sp(sp,p,long,lat)
call check_accuracy('Sstar_from_SP',value,sstar_from_sp)

value = gsw_ct_from_t(sa,t,p)
call check_accuracy('CT_from_t',value,ct_from_t)

!------------------------------------------------------------------------------
call section_title('Other conversions between Temperatures, Salinities, Entropy, Pressure and Height')

value = gsw_deltasa_from_sp(sp,p,long,lat)
call check_accuracy('deltaSA_from_SP',value,deltasa_from_sp)

sr = gsw_sr_from_sp(sp)
call check_accuracy('SR_from_SP',sr,sr_from_sp)

value = gsw_sp_from_sr(sr)
call check_accuracy('SP_from_SR',value,sp_from_sr)

value = gsw_sp_from_sa(sa,p,long,lat)
call check_accuracy('SP_from_SA',value,sp_from_sa)

sstar = gsw_sstar_from_sa(sa,p,long,lat)
call check_accuracy('Sstar_from_SA',sstar,sstar_from_sa)

value = gsw_sa_from_sstar(sstar,p,long,lat)
call check_accuracy('SA_from_Sstar',value,sa_from_sstar)

value = gsw_sp_from_sstar(sstar,p,long,lat)
call check_accuracy('SP_from_Sstar',value,sp_from_sstar)

pt = gsw_pt_from_ct(sa,ct)
call check_accuracy('pt_from_CT',pt,pt_from_ct)

value = gsw_t_from_ct(sa,ct,p)
call check_accuracy('t_from_CT',value,t_from_ct)

value = gsw_ct_from_pt(sa,pt)
call check_accuracy('CT_from_pt',value,ct_from_pt)

value = gsw_pt0_from_t(sa,t,p)
call check_accuracy('pt0_from_t',value,pt0_from_t)

value = gsw_pt_from_t(sa,t,p,pref)
call check_accuracy('pt_from_t',value,pt_from_t)

z = gsw_z_from_p(p,lat)
call check_accuracy('z_from_p',z,z_from_p)

value = gsw_p_from_z(z,lat)
call check_accuracy('p_from_z',value,p_from_z)

entropy = gsw_entropy_from_pt(sa,pt)
call check_accuracy('entropy_from_pt',entropy,entropy_from_pt)

value = gsw_pt_from_entropy(sa,entropy)
call check_accuracy('pt_from_entropy',value,pt_from_entropy)

value = gsw_ct_from_entropy(sa,entropy)
call check_accuracy('CT_from_entropy',value,ct_from_entropy)

value = gsw_entropy_from_t(sa,t,p)
call check_accuracy('entropy_from_t',value,entropy_from_t)

value = gsw_adiabatic_lapse_rate_from_ct(sa,ct,p)
call check_accuracy('adiabatic_lapse_rate_from_CT',value, &
                    adiabatic_lapse_rate_from_ct)

!------------------------------------------------------------------------------
call section_title('Specific Volume, Density and Enthalpy')

value = gsw_specvol(sa,ct,p)
call check_accuracy('specvol',value,specvol)

value = gsw_alpha(sa,ct,p)
call check_accuracy('alpha',value,alpha)

value = gsw_beta(sa,ct,p)
call check_accuracy('beta',value,beta)

value = gsw_alpha_on_beta(sa,ct,p)
call check_accuracy('alpha_on_beta',value,alpha_on_beta)

call gsw_specvol_alpha_beta(sa,ct,p,val1,val2,val3)
call check_accuracy('specvol_alpha_beta',val1,v_vab)
call check_accuracy('specvol_alpha_beta',val2,alpha_vab)
call check_accuracy('specvol_alpha_beta',val3,beta_vab)

call gsw_specvol_first_derivatives(sa,ct,p,val1,val2,val3)
call check_accuracy('specvol_first_derivatives',val1,v_sa)
call check_accuracy('specvol_first_derivatives',val2,v_ct)
call check_accuracy('specvol_first_derivatives',val3,v_P)

call gsw_specvol_second_derivatives(sa,ct,p,val1,val2,val3,val4,val5)
call check_accuracy('specvol_second_derivatives',val1,v_sa_sa)
call check_accuracy('specvol_second_derivatives',val2,v_sa_ct)
call check_accuracy('specvol_second_derivatives',val3,v_ct_ct)
call check_accuracy('specvol_second_derivatives',val4,v_sa_P)
call check_accuracy('specvol_second_derivatives',val5,v_ct_P)

call gsw_specvol_first_derivatives_wrt_enthalpy(sa,ct,p,val1,val2)
call check_accuracy('specvol_first_derivatives_wrt_enthalpy',val1,v_sa_wrt_h)
call check_accuracy('specvol_first_derivatives_wrt_enthalpy',val2,v_h)

call gsw_specvol_second_derivatives_wrt_enthalpy(sa,ct,p,val1,val2,val3)
call check_accuracy('specvol_second_derivatives_wrt_enthalpy',val1,v_sa_sa_wrt_h)
call check_accuracy('specvol_second_derivatives_wrt_enthalpy',val2,v_sa_h)
call check_accuracy('specvol_second_derivatives_wrt_enthalpy',val3,v_h_h)

value = gsw_specvol_anom_standard(sa,ct,p)
call check_accuracy('specvol_anom_standard',value,specvol_anom_standard)

r = gsw_rho(sa,ct,p)
call check_accuracy('rho',r,rho)

call gsw_rho_alpha_beta(sa,ct,p,val1,val2,val3)
call check_accuracy('rho_alpha_beta',val1,rho_rab)
call check_accuracy('rho_alpha_beta',val2,alpha_rab)
call check_accuracy('rho_alpha_beta',val3,beta_rab)

call gsw_rho_first_derivatives(sa,ct,p,val1,val2,val3)
call check_accuracy('rho_first_derivatives',val1,rho_sa)
call check_accuracy('rho_first_derivatives',val2,rho_ct)
call check_accuracy('rho_first_derivatives',val3,rho_P)

call gsw_rho_second_derivatives(sa,ct,p,val1,val2,val3,val4,val5)
call check_accuracy('rho_second_derivatives',val1,rho_sa_sa)
call check_accuracy('rho_second_derivatives',val2,rho_sa_ct)
call check_accuracy('rho_second_derivatives',val3,rho_ct_ct)
call check_accuracy('rho_second_derivatives',val4,rho_sa_P)
call check_accuracy('rho_second_derivatives',val5,rho_ct_P)

call gsw_rho_first_derivatives_wrt_enthalpy(sa,ct,p,val1,val2)
call check_accuracy('rho_first_derivatives_wrt_enthalpy',val1,rho_sa_wrt_h)
call check_accuracy('rho_first_derivatives_wrt_enthalpy',val2,rho_h)

call gsw_rho_second_derivatives_wrt_enthalpy(sa,ct,p,val1,val2,val3)
call check_accuracy('rho_second_derivatives_wrt_enthalpy',val1,rho_sa_sa_wrt_h)
call check_accuracy('rho_second_derivatives_wrt_enthalpy',val2,rho_sa_h)
call check_accuracy('rho_second_derivatives_wrt_enthalpy',val3,rho_h_h)

value = gsw_sigma0(sa,ct)
call check_accuracy('sigma0',value,sigma0)

value = gsw_sigma1(sa,ct)
call check_accuracy('sigma1',value,sigma1)

value = gsw_sigma2(sa,ct)
call check_accuracy('sigma2',value,sigma2)

value = gsw_sigma3(sa,ct)
call check_accuracy('sigma3',value,sigma3)

value = gsw_sigma4(sa,ct)
call check_accuracy('sigma4',value,sigma4)

value = gsw_sound_speed(sa,ct,p)
call check_accuracy('sound_speed',value,sound_speed)

value = gsw_kappa(sa,ct,p)
call check_accuracy('kappa',value,kappa)

value = gsw_cabbeling(sa,ct,p)
call check_accuracy('cabbeling',value,cabbeling)

value = gsw_thermobaric(sa,ct,p)
call check_accuracy('thermobaric',value,thermobaric)

value = gsw_sa_from_rho(r,ct,p)
call check_accuracy('SA_from_rho',value,sa_from_rho)

call gsw_ct_from_rho(r,sa,p,value)
call check_accuracy('CT_from_rho',value,ct_from_rho)

value = gsw_ct_maxdensity(sa,p)
call check_accuracy('CT_maxdensity',value,ct_maxdensity)

value = gsw_internal_energy(sa,ct,p)
call check_accuracy('internal_energy',value,internal_energy)

h = gsw_enthalpy(sa,ct,p)
call check_accuracy('enthalpy',h,enthalpy)

value = gsw_enthalpy_diff(sa,ct,p_shallow,p_deep)
call check_accuracy('enthalpy_diff',value,enthalpy_diff)

value = gsw_ct_from_enthalpy(sa,h,p)
call check_accuracy('CT_from_enthalpy',value,ct_from_enthalpy)

value = gsw_dynamic_enthalpy(sa,ct,p)
call check_accuracy('dynamic_enthalpy',value,dynamic_enthalpy)

call gsw_enthalpy_first_derivatives(sa,ct,p,val1,val2)
call check_accuracy('enthalpy_first_derivatives',val1,h_sa)
call check_accuracy('enthalpy_first_derivatives',val2,h_ct)

call gsw_enthalpy_second_derivatives(sa,ct,p,val1,val2,val3)
call check_accuracy('enthalpy_second_derivatives',val1,h_sa_sa)
call check_accuracy('enthalpy_second_derivatives',val2,h_sa_ct)
call check_accuracy('enthalpy_second_derivatives',val3,h_ct_ct)

!------------------------------------------------------------------------------
call section_title('Derivatives of entropy, CT and pt')

call gsw_ct_first_derivatives(sa,pt,val1,val2)
call check_accuracy('CT_first_derivatives',val1,ct_sa)
call check_accuracy('CT_first_derivatives',val2,ct_pt)

call gsw_ct_second_derivatives(sa,pt,val1,val2,val3)
call check_accuracy('CT_second_derivatives',val1,ct_sa_sa)
call check_accuracy('CT_second_derivatives',val2,ct_sa_pt)
call check_accuracy('CT_second_derivatives',val3,ct_pt_pt)

call gsw_entropy_first_derivatives(sa,ct,val1,val2)
call check_accuracy('entropy_first_derivatives',val1,eta_sa)
call check_accuracy('entropy_first_derivatives',val2,eta_ct)

call gsw_entropy_second_derivatives(sa,ct,val1,val2,val3)
call check_accuracy('entropy_second_derivatives',val1,eta_sa_sa)
call check_accuracy('entropy_second_derivatives',val2,eta_sa_ct)
call check_accuracy('entropy_second_derivatives',val3,eta_ct_ct)

call gsw_pt_first_derivatives(sa,ct,val1,val2)
call check_accuracy('pt_first_derivatives',val1,pt_sa)
call check_accuracy('pt_first_derivatives',val2,pt_ct)

call gsw_pt_second_derivatives(sa,ct,val1,val2,val3)
call check_accuracy('pt_second_derivatives',val1,pt_sa_sa)
call check_accuracy('pt_second_derivatives',val2,pt_sa_ct)
call check_accuracy('pt_second_derivatives',val3,pt_ct_ct)

!------------------------------------------------------------------------------
call section_title('Freezing temperatures')

saturation_fraction = 0.5_r8

ctf = gsw_ct_freezing(sa,p,saturation_fraction)
call check_accuracy('CT_freezing',ctf,ct_freezing)

ctf_poly = gsw_ct_freezing_poly(sa,p,saturation_fraction)
call check_accuracy('CT_freezing_poly',ctf_poly,ct_freezing_poly)

tf = gsw_t_freezing(sa,p,saturation_fraction)
call check_accuracy('t_freezing',tf,t_freezing)

tf_poly = gsw_t_freezing_poly(sa,p,saturation_fraction)
call check_accuracy('t_freezing_poly',tf_poly,t_freezing_poly)

value = gsw_pot_enthalpy_ice_freezing(sa,p)
call check_accuracy('pot_enthalpy_ice_freezing',value,pot_enthalpy_ice_freezing)

value = gsw_pot_enthalpy_ice_freezing_poly(sa,p)
call check_accuracy('pot_enthalpy_ice_freezing_poly',value, &
                     pot_enthalpy_ice_freezing_poly)

value = gsw_sa_freezing_from_ct(ctf,p,saturation_fraction)
call check_accuracy('SA_freezing_from_CT',value,sa_freezing_from_ct)

value = gsw_sa_freezing_from_ct_poly(ctf_poly,p,saturation_fraction)
call check_accuracy('SA_freezing_from_CT_poly',value,sa_freezing_from_ct_poly)

value = gsw_sa_freezing_from_t(tf,p,saturation_fraction)
call check_accuracy('SA_freezing_from_t',value,sa_freezing_from_t)

value = gsw_sa_freezing_from_t_poly(tf_poly,p,saturation_fraction)
call check_accuracy('SA_freezing_from_t_poly',value,sa_freezing_from_t_poly)

call gsw_ct_freezing_first_derivatives(sa,p,saturation_fraction,val1,val2)
call check_accuracy('CT_freezing_first_derivatives',val1,ctfreezing_sa)
call check_accuracy('CT_freezing_first_derivatives',val2,ctfreezing_p)

call gsw_ct_freezing_first_derivatives_poly(sa,p,saturation_fraction,val4,val5)
call check_accuracy('CT_freezing_first_derivatives_poly',val4,ctfreezing_sa_poly)
call check_accuracy('CT_freezing_first_derivatives_poly',val5,ctfreezing_p_poly)

call gsw_t_freezing_first_derivatives(sa,p,saturation_fraction,val1,val2)
call check_accuracy('t_freezing_first_derivatives',val1,tfreezing_sa)
call check_accuracy('t_freezing_first_derivatives',val2,tfreezing_p)

call gsw_t_freezing_first_derivatives_poly(sa,p,saturation_fraction,val4,val5)
call check_accuracy('t_freezing_first_derivatives_poly',val4,tfreezing_sa_poly)
call check_accuracy('t_freezing_first_derivatives_poly',val5,tfreezing_p_poly)

call gsw_pot_enthalpy_ice_freezing_first_derivatives(sa,p,val1,val2)
call check_accuracy('pot_enthalpy_ice_freezing_first_derivatives',val1, &
                    pot_enthalpy_ice_freezing_sa)
call check_accuracy('pot_enthalpy_ice_freezing_first_derivatives',val2, &
                    pot_enthalpy_ice_freezing_p)

call gsw_pot_enthalpy_ice_freezing_first_derivatives_poly(sa,p,val1,val2)
call check_accuracy('pot_enthalpy_ice_freezing_first_derivatives_poly',val1, &
                    pot_enthalpy_ice_freezing_sa_poly)
call check_accuracy('pot_enthalpy_ice_freezing_first_derivatives_poly',val2, &
                    pot_enthalpy_ice_freezing_p_poly)

!------------------------------------------------------------------------------
call section_title('Isobaric Melting Enthalpy and Isobaric Evaporation Enthalpy')

value = gsw_latentheat_melting(sa,p)
call check_accuracy('latentheat_melting',value,latentheat_melting)

value = gsw_latentheat_evap_ct(sa,ct)
call check_accuracy('latentheat_evap_CT',value,latentheat_evap_ct)

value = gsw_latentheat_evap_t(sa,t)
call check_accuracy('latentheat_evap_t',value,latentheat_evap_t)

!------------------------------------------------------------------------------
call section_title('Planet Earth properties')

value = gsw_grav(lat,p)
call check_accuracy('grav',value,grav)

!------------------------------------------------------------------------------
call section_title('Density and enthalpy in terms of CT, derived from the exact Gibbs function')

value = gsw_enthalpy_ct_exact(sa,ct,p)
call check_accuracy('enthalpy_CT_exact',value,enthalpy_ct_exact)

call gsw_enthalpy_first_derivatives_ct_exact(sa,ct,p,val1,val2)
call check_accuracy('enthalpy_first_derivatives_CT_exact',val1,h_sa_ct_exact)
call check_accuracy('enthalpy_first_derivatives_CT_exact',val2,h_ct_ct_exact)

call gsw_enthalpy_second_derivatives_ct_exact(sa,ct,p,val1,val2,val3)
call check_accuracy('enthalpy_second_derivatives_CT_exact',val1, &
                    h_sa_sa_ct_exact)
call check_accuracy('enthalpy_second_derivatives_CT_exact',val2, &
                    h_sa_ct_ct_exact)
call check_accuracy('enthalpy_second_derivatives_CT_exact',val3, &
                    h_ct_ct_ct_exact)

!------------------------------------------------------------------------------
call section_title('Basic thermodynamic properties in terms of in-situ t, based on the exact Gibbs function')

value = gsw_rho_t_exact(sa,t,p)
call check_accuracy('rho_t_exact',value,rho_t_exact)

value = gsw_pot_rho_t_exact(sa,t,p,pref)
call check_accuracy('pot_rho_t_exact',value,pot_rho_t_exact)

value = gsw_alpha_wrt_t_exact(sa,t,p)
call check_accuracy('alpha_wrt_t_exact',value,alpha_wrt_t_exact)

value = gsw_beta_const_t_exact(sa,t,p)
call check_accuracy('beta_const_t_exact',value,beta_const_t_exact)

value = gsw_specvol_t_exact(sa,t,p)
call check_accuracy('specvol_t_exact',value,specvol_t_exact)

value = gsw_sound_speed_t_exact(sa,t,p)
call check_accuracy('sound_speed_t_exact',value,sound_speed_t_exact)

value = gsw_kappa_t_exact(sa,t,p)
call check_accuracy('kappa_t_exact',value,kappa_t_exact)

value = gsw_enthalpy_t_exact(sa,t,p)
call check_accuracy('enthalpy_t_exact',value,enthalpy_t_exact)

call gsw_ct_first_derivatives_wrt_t_exact(sa,t,p,val1,val2,val3)
call check_accuracy('CT_first_derivatives_wrt_t_exact',val1,ct_sa_wrt_t)
call check_accuracy('CT_first_derivatives_wrt_t_exact',val2,ct_t_wrt_t)
call check_accuracy('CT_first_derivatives_wrt_t_exact',val3,ct_p_wrt_t)

value = gsw_chem_potential_water_t_exact(sa,t,p)
call check_accuracy('chem_potential_water_t_exact',value, &
                     chem_potential_water_t_exact)

value = gsw_t_deriv_chem_potential_water_t_exact(sa,t,p)
call check_accuracy('t_deriv_chem_potential_water_t_exact',value, &
                     t_deriv_chem_potential_water_t_exact)

value = gsw_dilution_coefficient_t_exact(sa,t,p)
call check_accuracy('dilution_coefficient_t_exact',value, &
                     dilution_coefficient_t_exact)

!------------------------------------------------------------------------------
call section_title('Library functions of the GSW Toolbox')

value = gsw_deltasa_atlas(p,long,lat)
call check_accuracy('deltaSA_atlas',value,deltasa_atlas)

value = gsw_fdelta(p,long,lat)
call check_accuracy('Fdelta',value,Fdelta)

!------------------------------------------------------------------------------
call section_title('Vertical stability')

deallocate(val1,val2,val3,val4,val5)
allocate(val1(cast_mpres_m,cast_mpres_n))
allocate(val2(cast_mpres_m,cast_mpres_n))
allocate(val3(cast_mpres_m,cast_mpres_n))
allocate(val4(cast_mpres_m,cast_mpres_n))
allocate(val5(cast_mpres_m,cast_mpres_n))
allocate(val6(cast_mpres_m,cast_mpres_n))
allocate(val7(cast_mpres_m,cast_mpres_n))
allocate(val8(cast_mpres_m,cast_mpres_n))

do i = 1, cast_mpres_n
    call gsw_turner_rsubrho(sa(:,i),ct(:,i),p(:,i),val1(:,i),val2(:,i), &
                            val3(:,i))
end do
call check_accuracy('Turner_Rsubrho',val1,result_mpres=tu)
call check_accuracy('Turner_Rsubrho',val2,result_mpres=rsubrho)
call check_accuracy('Turner_Rsubrho',val3,result_mpres=p_mid_tursr)

do i = 1, cast_mpres_n
    call gsw_nsquared(sa(:,i),ct(:,i),p(:,i),lat(:,i),val1(:,i),val2(:,i))
end do
call check_accuracy('Nsquared',val1,result_mpres=n2)
call check_accuracy('Nsquared',val2,result_mpres=p_mid_n2)

do i = 1, cast_mpres_n
    call gsw_nsquared_min(sa(:,i),ct(:,i),p(:,i),lat(:,i),val1(:,i),val2(:,i), &
                val3(:,i),val4(:,i),val5(:,i),val6(:,i),val7(:,i),val8(:,i))
end do
call check_accuracy('Nsquared_min',val1,result_mpres=n2min)
call check_accuracy('Nsquared_min',val2,result_mpres=n2min_pmid)
call check_accuracy('Nsquared_min',val3,result_mpres=n2min_specvol)
call check_accuracy('Nsquared_min',val4,result_mpres=n2min_alpha)
call check_accuracy('Nsquared_min',val5,result_mpres=n2min_beta)
call check_accuracy('Nsquared_min',val6,result_mpres=n2min_dsa)
call check_accuracy('Nsquared_min',val7,result_mpres=n2min_dct)
call check_accuracy('Nsquared_min',val8,result_mpres=n2min_dp)

do i = 1, cast_mpres_n
    call gsw_ipv_vs_fnsquared_ratio(sa(:,i),ct(:,i),p(:,i),pref,val1(:,i), &
                                    val2(:,i))
end do
call check_accuracy('IPV_vs_fNsquared_ratio',val1,result_mpres=ipvfn2)
call check_accuracy('IPV_vs_fNsquared_ratio',val2,result_mpres=p_mid_ipvfn2)
  
value = gsw_nsquared_lowerlimit(p,long,lat)
call check_accuracy('n2_lowerlimit',value,n2_lowerlimit)

deallocate(value)
allocate(value(cast_mpres_n,1))
do i = 1, cast_mpres_n
    value(i,1) = gsw_mlp(sa(:,i),ct(:,i),p(:,i))
end do
call check_accuracy('mlp',value,result_cast=mlp)

!------------------------------------------------------------------------------
call section_title('Geostrophic streamfunctions and acoustic travel time')

deallocate(val1,val2)
allocate(val1(cast_m,cast_n))
allocate(val2(cast_m,cast_n))

do i = 1, cast_n
    n = count(sa(:,i) .lt. 1e90_r8)    ! Only analyse valid section of the cast
    val1(:n,i) = gsw_geo_strf_dyn_height(sa(:n,i),ct(:n,i),p(:n,i),pref)
    if (n .lt. cast_m) val1(n+1:,i) = sa(n+1:cast_m,i)
end do
call check_accuracy('geo_strf_dyn_height',val1,geo_strf_dyn_height)

do i = 1, cast_n
    call gsw_geo_strf_dyn_height_pc(sa(:,i),ct(:,i),delta_p(:,i), &
                                    val1(:,i),val2(:,i))
end do
call check_accuracy('geo_strf_dyn_height_pc',val1,geo_strf_dyn_height_pc)
call check_accuracy('geo_strf_dyn_height_pc',val2,geo_strf_dyn_height_pc_p_mid)

!------------------------------------------------------------------------------
call section_title('Thermodynamic properties of ice Ih')

deallocate(value)
allocate(value(cast_ice_m,cast_ice_n))

value = gsw_rho_ice(t_seaice,p_arctic)
call check_accuracy('rho_ice',value,result_ice=rho_ice)

value = gsw_alpha_wrt_t_ice(t_seaice,p_arctic)
call check_accuracy('alpha_wrt_t_ice',value,result_ice=alpha_wrt_t_ice)

value = gsw_specvol_ice(t_seaice,p_arctic)
call check_accuracy('specvol_ice',value,result_ice=specvol_ice)

value = gsw_pressure_coefficient_ice(t_seaice,p_arctic)
call check_accuracy('pressure_coefficient_ice',value, &
                     result_ice=pressure_coefficient_ice)

value = gsw_sound_speed_ice(t_seaice,p_arctic)
call check_accuracy('sound_speed_ice',value,result_ice=sound_speed_ice)

value = gsw_kappa_ice(t_seaice,p_arctic)
call check_accuracy('kappa_ice',value,result_ice=kappa_ice)

value = gsw_kappa_const_t_ice(t_seaice,p_arctic)
call check_accuracy('kappa_const_t_ice',value,result_ice=kappa_const_t_ice)

value = gsw_internal_energy_ice(t_seaice,p_arctic)
call check_accuracy('internal_energy_ice',value,result_ice=internal_energy_ice)

value = gsw_enthalpy_ice(t_seaice,p_arctic)
call check_accuracy('enthalpy_ice',value,result_ice=enthalpy_ice)

value = gsw_entropy_ice(t_seaice,p_arctic)
call check_accuracy('entropy_ice',value,result_ice=entropy_ice)

value = gsw_cp_ice(t_seaice,p_arctic)
call check_accuracy('cp_ice',value,result_ice=cp_ice)

value = gsw_chem_potential_water_ice(t_seaice,p_arctic)
call check_accuracy('chem_potential_water_ice',value, &
                     result_ice=chem_potential_water_ice)

value = gsw_helmholtz_energy_ice(t_seaice,p_arctic)
call check_accuracy('Helmholtz_energy_ice',value, &
                     result_ice=helmholtz_energy_ice)

value = gsw_adiabatic_lapse_rate_ice(t_seaice,p_arctic)
call check_accuracy('adiabatic_lapse_rate_ice',value, &
                     result_ice=adiabatic_lapse_rate_ice)

pt0 = gsw_pt0_from_t_ice(t_seaice,p_arctic)
call check_accuracy('pt0_from_t_ice',pt0,result_ice=pt0_from_t_ice)

value = gsw_pt_from_t_ice(t_seaice,p_arctic,pref)
call check_accuracy('pt_from_t_ice',value,result_ice=pt_from_t_ice)

value = gsw_t_from_pt0_ice(pt0,p_arctic)
call check_accuracy('t_from_pt0_ice',value,result_ice=t_from_pt0_ice)

h = gsw_pot_enthalpy_from_pt_ice(pt0)
call check_accuracy('pot_enthalpy_from_pt_ice',h, &
                     result_ice=pot_enthalpy_from_pt_ice)

value = gsw_pt_from_pot_enthalpy_ice(h)
call check_accuracy('pt_from_pot_enthalpy_ice',value, &
                     result_ice=pt_from_pot_enthalpy_ice)

h = gsw_pot_enthalpy_from_pt_ice_poly(pt0)
call check_accuracy('pot_enthalpy_from_pt_ice_poly',h, &
                     result_ice=pot_enthalpy_from_pt_ice_poly)

value = gsw_pt_from_pot_enthalpy_ice_poly(h)
call check_accuracy('pt_from_pot_enthalpy_ice_poly',value, &
                     result_ice=pt_from_pot_enthalpy_ice_poly)

saturation_fraction = 0.5_r8

value = gsw_pressure_freezing_ct(sa_arctic,ct_arctic-1.0_r8,saturation_fraction)
call check_accuracy('pressure_freezing_CT',value, &
                     result_ice=pressure_freezing_ct)

!------------------------------------------------------------------------------
call section_title('Thermodynamic interaction between ice and seawater')

deallocate(val1,val2,val3)
allocate(val1(cast_ice_m,cast_ice_n))
allocate(val2(cast_ice_m,cast_ice_n))
allocate(val3(cast_ice_m,cast_ice_n))

value = gsw_melting_ice_sa_ct_ratio(sa_arctic,ct_arctic,p_arctic,t_ice)
call check_accuracy('melting_ice_sa_ct_ratio',value, &
                     result_ice=melting_ice_sa_ct_ratio)

value = gsw_melting_ice_sa_ct_ratio_poly(sa_arctic,ct_arctic,p_arctic,t_ice)
call check_accuracy('melting_ice_sa_ct_ratio_poly',value, &
                     result_ice=melting_ice_sa_ct_ratio_poly)

value = gsw_melting_ice_equilibrium_sa_ct_ratio(sa_arctic,p_arctic)
call check_accuracy('melting_ice_equilibrium_sa_ct_ratio',value, &
                     result_ice=melting_ice_equilibrium_sa_ct_ratio)

value = gsw_melting_ice_equilibrium_sa_ct_ratio_poly(sa_arctic,p_arctic)
call check_accuracy('melting_ice_equilibrium_sa_ct_ratio_poly',value, &
                     result_ice=melting_ice_equilibrium_sa_ct_ratio_poly)

call gsw_melting_ice_into_seawater(sa_arctic,ct_arctic+0.1_r8,p_arctic,w_ice, &
                                   t_ice,val1,val2,val3)
call check_accuracy('melting_ice_into_seawater',val1, &
                     result_ice=melting_ice_into_seawater_sa_final)
call check_accuracy('melting_ice_into_seawater',val2, &
                     result_ice=melting_ice_into_seawater_ct_final)
!call check_accuracy('melting_ice_into_seawater',val3, &
!                    'melting_ice_into_seawater_w_Ih')

call gsw_ice_fraction_to_freeze_seawater(sa_arctic,ct_arctic,p_arctic,t_ice, &
                                         val1,val2,val3)
call check_accuracy('ice_fraction_to_freeze_seawater',val1, &
                     result_ice=ice_fraction_to_freeze_seawater_sa_freeze)
call check_accuracy('ice_fraction_to_freeze_seawater',val2, &
                     result_ice=ice_fraction_to_freeze_seawater_ct_freeze)
call check_accuracy('ice_fraction_to_freeze_seawater',val3, &
                     result_ice=ice_fraction_to_freeze_seawater_w_Ih)

call gsw_frazil_ratios_adiabatic(sa_arctic,p_arctic,w_ice,val1,val2,val3)
call check_accuracy('frazil_ratios_adiabatic',val1,result_ice=dsa_dct_frazil)
call check_accuracy('frazil_ratios_adiabatic',val2,result_ice=dsa_dp_frazil)
call check_accuracy('frazil_ratios_adiabatic',val3,result_ice=dct_dp_frazil)

call gsw_frazil_ratios_adiabatic_poly(sa_arctic,p_arctic,w_ice,val1,val2,val3)
call check_accuracy('frazil_ratios_adiabatic_poly',val1, &
                     result_ice=dsa_dct_frazil_poly)
call check_accuracy('frazil_ratios_adiabatic_poly',val2, &
                     result_ice=dsa_dp_frazil_poly)
call check_accuracy('frazil_ratios_adiabatic_poly',val3, &
                     result_ice=dct_dp_frazil_poly)

call gsw_frazil_properties_potential(sa_bulk,h_pot_bulk,p_arctic,val1,val2,val3)
call check_accuracy('frazil_properties_potential',val1, &
                     result_ice=frazil_properties_potential_sa_final)
call check_accuracy('frazil_properties_potential',val2, &
                     result_ice=frazil_properties_potential_ct_final)
call check_accuracy('frazil_properties_potential',val3, &
                     result_ice=frazil_properties_potential_w_ih_final)

call gsw_frazil_properties_potential_poly(sa_bulk,h_pot_bulk,p_arctic,val1, &
                                          val2,val3)
call check_accuracy('frazil_properties_potential_poly',val1, &
                     result_ice=frazil_properties_potential_poly_sa_final)
call check_accuracy('frazil_properties_potential_poly',val2, &
                     result_ice=frazil_properties_potential_poly_ct_final)
call check_accuracy('frazil_properties_potential_poly',val3, &
                     result_ice=frazil_properties_potential_poly_w_ih_final)

call gsw_frazil_properties(sa_bulk,h_bulk,p_arctic,val1,val2,val3)
call check_accuracy('frazil_properties',val1, &
                     result_ice=frazil_properties_sa_final)
call check_accuracy('frazil_properties',val2, &
                     result_ice=frazil_properties_ct_final)
call check_accuracy('frazil_properties',val3, &
                     result_ice=frazil_properties_w_ih_final)

!------------------------------------------------------------------------------
call section_title('Thermodynamic interaction between seaice and seawater')

value = gsw_melting_seaice_sa_ct_ratio(sa_arctic,ct_arctic,p_arctic, &
                                       sa_seaice,t_seaice)
call check_accuracy('melting_seaice_sa_ct_ratio',value, &
                     result_ice=melting_seaice_sa_ct_ratio)

value = gsw_melting_seaice_sa_ct_ratio_poly(sa_arctic,ct_arctic,p_arctic, &
                                            sa_seaice,t_seaice)
call check_accuracy('melting_seaice_sa_ct_ratio_poly',value, &
                     result_ice=melting_seaice_sa_ct_ratio_poly)

value = gsw_melting_seaice_equilibrium_sa_ct_ratio(sa_arctic,p_arctic)
call check_accuracy('melting_seaice_equilibrium_sa_ct_ratio',value, &
                     result_ice=melting_seaice_equilibrium_sa_ct_ratio)

value = gsw_melting_seaice_equilibrium_sa_ct_ratio_poly(sa_arctic,p_arctic)
call check_accuracy('melting_seaice_equilibrium_sa_ct_ratio_poly',value, &
                     result_ice=melting_seaice_equilibrium_sa_ct_ratio_poly)

call gsw_melting_seaice_into_seawater(sa_arctic,ct_arctic,p_arctic, &
                                      w_seaice,sa_seaice,t_seaice,val1,val2)
call check_accuracy('melting_seaice_into_seawater',val1, &
                     result_ice=melting_seaice_into_seawater_sa_final)
call check_accuracy('melting_seaice_into_seawater',val2, &
                     result_ice=melting_seaice_into_seawater_ct_final)

call gsw_seaice_fraction_to_freeze_seawater(sa_arctic,ct_arctic,p_arctic, &
                                            sa_seaice,t_seaice,val1,val2,val3)
call check_accuracy('seaice_fraction_to_freeze_seawater',val1, &
                     result_ice=seaice_fraction_to_freeze_seawater_sa_freeze)
call check_accuracy('seaice_fraction_to_freeze_seawater',val2, &
                     result_ice=seaice_fraction_to_freeze_seawater_ct_freeze)
call check_accuracy('seaice_fraction_to_freeze_seawater',val3, &
                     result_ice=seaice_fraction_to_freeze_seawater_w_ih)

!------------------------------------------------------------------------------
if (gsw_error_flag.eq.1) then
  print*
  print*
  print*, 'Your installation of the Gibbs SeaWater (GSW) Oceanographic Toolbox has errors!'
else  
  print*
  print*
  print*, 'Well done! The gsw_check_fuctions confirms that the'
  print*, 'Gibbs SeaWater (GSW) Oceanographic Toolbox is installed correctly.'
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

    subroutine check_accuracy (func_name, fvalue, result, result_ice, &
                               result_mpres, result_cast, vprint)

    use gsw_mod_error_functions, only : gsw_error_limit

    implicit none

    character (*), intent(in) :: func_name
    real (r8), intent(in) :: fvalue(:,:)
    type(gsw_result), intent(in), optional :: result
    type(gsw_result_ice), intent(in), optional :: result_ice
    type(gsw_result_mpres), intent(in), optional :: result_mpres
    type(gsw_result_cast), intent(in), optional :: result_cast
    logical, intent(in), optional :: vprint

    integer :: ndots, i, j, k, ik=1, jk=1
    real (r8) :: dmax, drel, comp_accuracy
    real (r8) :: diff(size(fvalue,1),size(fvalue,2))
    real (r8) :: rvalue(size(fvalue,1),size(fvalue,2))
    character (80) :: message
    character (50) :: var_name
    character (4) :: errflg

    character (*), parameter :: att_name = 'computation_accuracy'
    character (*), parameter :: &
        dots = ' .............................................................'

    if (present(result)) then
        rvalue = result%values
        var_name = result%variable_name
        comp_accuracy = result%computation_accuracy
    else if (present(result_ice)) then
        rvalue = result_ice%values
        var_name = result_ice%variable_name
        comp_accuracy = result_ice%computation_accuracy
    else if (present(result_mpres)) then
        rvalue = result_mpres%values
        var_name = result_mpres%variable_name
        comp_accuracy = result_mpres%computation_accuracy
    else if (present(result_cast)) then
        rvalue(:,1) = result_cast%values
        var_name = result_cast%variable_name
        comp_accuracy = result_cast%computation_accuracy
    end if

    if (trim(var_name).ne.func_name) then
        if (len(func_name)+len_trim(var_name).gt.55) then
            k = len(func_name) + len_trim(var_name) - 55
            message = func_name // ' (..' // trim(var_name(k:)) // ')'
        else
            message = func_name // ' (' // trim(var_name) // ')'
        end if
    else
        message = func_name
    end if

    diff = abs(fvalue - rvalue)
    where (rvalue .eq. 9e90_r8) diff = 0.0_r8

    if (present(vprint)) then
        if (vprint) then
            print *, "Limit =", comp_accuracy
            print '(i3,3ES24.15)', ((i,fvalue(i,j),rvalue(i,j),diff(i,j), &
                   i=1,size(fvalue,1)), j=1,size(fvalue,2))
            print *
        end if
    end if

    if (any(fvalue .gt. gsw_error_limit .and. fvalue .lt. 1e90_r8 .and. &
            rvalue .lt. 1e90_r8)) then
        where (fvalue .gt. gsw_error_limit) diff = 0.0_r8
        errflg = ' (*)'
    else
        errflg = '    '
    end if
    ndots = 65 - len_trim(message)

    if (any(diff .gt. comp_accuracy)) then
        gsw_error_flag = 1
        dmax = 0.0_r8
        do i = 1, size(fvalue,1)
            do j = 1, size(fvalue,2)
                 if (diff(i,j) .gt. dmax) then
                     dmax = diff(i,j)
                     ik = i
                     jk = j
                 end if
            end do
        end do
        drel = dmax*100.0_r8/abs(fvalue(ik,jk))
        print *, trim(message), dots(:ndots-3), ' << failed >>'
        print *
        print *, "  Max. difference =", dmax, ", limit =", comp_accuracy
        print *, "  Max. diff (rel) =", drel, ", limit =", &
                comp_accuracy*100.0_r8/abs(fvalue(ik,jk))
        print *
    else
        print *, trim(message), dots(:ndots), ' passed', errflg
    endif

    return
    end subroutine check_accuracy

end

!--------------------------------------------------------------------------
