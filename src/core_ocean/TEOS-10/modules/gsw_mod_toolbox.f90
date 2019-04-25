module gsw_mod_toolbox

use gsw_mod_kinds

implicit none

public :: gsw_add_barrier
public :: gsw_add_mean
public :: gsw_adiabatic_lapse_rate_from_ct
public :: gsw_adiabatic_lapse_rate_ice
public :: gsw_alpha
public :: gsw_alpha_on_beta
public :: gsw_alpha_wrt_t_exact
public :: gsw_alpha_wrt_t_ice
public :: gsw_beta
public :: gsw_beta_const_t_exact
public :: gsw_c_from_sp
public :: gsw_cabbeling
public :: gsw_chem_potential_water_ice
public :: gsw_chem_potential_water_t_exact
public :: gsw_cp_ice
public :: gsw_ct_first_derivatives
public :: gsw_ct_first_derivatives_wrt_t_exact
public :: gsw_ct_freezing
public :: gsw_ct_freezing_exact
public :: gsw_ct_freezing_first_derivatives
public :: gsw_ct_freezing_first_derivatives_poly
public :: gsw_ct_freezing_poly
public :: gsw_ct_from_enthalpy
public :: gsw_ct_from_enthalpy_exact
public :: gsw_ct_from_entropy
public :: gsw_ct_from_pt
public :: gsw_ct_from_rho
public :: gsw_ct_from_t
public :: gsw_ct_maxdensity
public :: gsw_ct_second_derivatives
public :: gsw_deltasa_atlas
public :: gsw_deltasa_from_sp
public :: gsw_dilution_coefficient_t_exact
public :: gsw_dynamic_enthalpy
public :: gsw_enthalpy
public :: gsw_enthalpy_ct_exact
public :: gsw_enthalpy_diff
public :: gsw_enthalpy_first_derivatives
public :: gsw_enthalpy_first_derivatives_ct_exact
public :: gsw_enthalpy_ice
public :: gsw_enthalpy_second_derivatives
public :: gsw_enthalpy_second_derivatives_ct_exact
public :: gsw_enthalpy_sso_0
public :: gsw_enthalpy_t_exact
public :: gsw_entropy_first_derivatives
public :: gsw_entropy_from_pt
public :: gsw_entropy_from_t
public :: gsw_entropy_ice
public :: gsw_entropy_part
public :: gsw_entropy_part_zerop
public :: gsw_entropy_second_derivatives
public :: gsw_fdelta
public :: gsw_frazil_properties
public :: gsw_frazil_properties_potential
public :: gsw_frazil_properties_potential_poly
public :: gsw_frazil_ratios_adiabatic
public :: gsw_frazil_ratios_adiabatic_poly
public :: gsw_geo_strf_dyn_height
public :: gsw_geo_strf_dyn_height_pc
public :: gsw_gibbs
public :: gsw_gibbs_ice
public :: gsw_gibbs_ice_part_t
public :: gsw_gibbs_ice_pt0
public :: gsw_gibbs_ice_pt0_pt0
public :: gsw_gibbs_pt0_pt0
public :: gsw_grav
public :: gsw_helmholtz_energy_ice
public :: gsw_hill_ratio_at_sp2
public :: gsw_ice_fraction_to_freeze_seawater
public :: gsw_internal_energy
public :: gsw_internal_energy_ice
public :: gsw_ipv_vs_fnsquared_ratio
public :: gsw_kappa
public :: gsw_kappa_const_t_ice
public :: gsw_kappa_ice
public :: gsw_kappa_t_exact
public :: gsw_latentheat_evap_ct
public :: gsw_latentheat_evap_t
public :: gsw_latentheat_melting
public :: gsw_linear_interp_sa_ct
public :: gsw_melting_ice_equilibrium_sa_ct_ratio
public :: gsw_melting_ice_equilibrium_sa_ct_ratio_poly
public :: gsw_melting_ice_into_seawater
public :: gsw_melting_ice_sa_ct_ratio
public :: gsw_melting_ice_sa_ct_ratio_poly
public :: gsw_melting_seaice_equilibrium_sa_ct_ratio
public :: gsw_melting_seaice_equilibrium_sa_ct_ratio_poly
public :: gsw_melting_seaice_into_seawater
public :: gsw_melting_seaice_sa_ct_ratio
public :: gsw_melting_seaice_sa_ct_ratio_poly
public :: gsw_mlp
public :: gsw_nsquared
public :: gsw_nsquared_lowerlimit
public :: gsw_nsquared_min
public :: gsw_nsquared_min_const_t
public :: gsw_p_from_z
public :: gsw_pot_enthalpy_from_pt_ice
public :: gsw_pot_enthalpy_from_pt_ice_poly
public :: gsw_pot_enthalpy_ice_freezing
public :: gsw_pot_enthalpy_ice_freezing_first_derivatives
public :: gsw_pot_enthalpy_ice_freezing_first_derivatives_poly
public :: gsw_pot_enthalpy_ice_freezing_poly
public :: gsw_pot_rho_t_exact
public :: gsw_pressure_coefficient_ice
public :: gsw_pressure_freezing_ct
public :: gsw_pt0_cold_ice_poly
public :: gsw_pt0_from_t
public :: gsw_pt0_from_t_ice
public :: gsw_pt_first_derivatives
public :: gsw_pt_from_ct
public :: gsw_pt_from_entropy
public :: gsw_pt_from_pot_enthalpy_ice
public :: gsw_pt_from_pot_enthalpy_ice_poly
public :: gsw_pt_from_pot_enthalpy_ice_poly_dh
public :: gsw_pt_from_t
public :: gsw_pt_from_t_ice
public :: gsw_pt_second_derivatives
public :: gsw_rho
public :: gsw_rho_alpha_beta
public :: gsw_rho_alpha_beta_bsq
public :: gsw_rho_first_derivatives
public :: gsw_rho_first_derivatives_wrt_enthalpy
public :: gsw_rho_ice
public :: gsw_rho_second_derivatives
public :: gsw_rho_second_derivatives_wrt_enthalpy
public :: gsw_rho_t_exact
public :: gsw_rr68_interp_sa_ct
public :: gsw_sa_freezing_estimate
public :: gsw_sa_freezing_from_ct
public :: gsw_sa_freezing_from_ct_poly
public :: gsw_sa_freezing_from_t
public :: gsw_sa_freezing_from_t_poly
public :: gsw_sa_from_rho
public :: gsw_sa_from_sp
public :: gsw_sa_from_sp_baltic
public :: gsw_sa_from_sstar
public :: gsw_sa_p_inrange
public :: gsw_saar
public :: gsw_seaice_fraction_to_freeze_seawater
public :: gsw_sigma0
public :: gsw_sigma1
public :: gsw_sigma2
public :: gsw_sigma3
public :: gsw_sigma4
public :: gsw_sound_speed
public :: gsw_sound_speed_ice
public :: gsw_sound_speed_t_exact
public :: gsw_sp_from_c
public :: gsw_sp_from_sa
public :: gsw_sp_from_sa_baltic
public :: gsw_sp_from_sk
public :: gsw_sp_from_sr
public :: gsw_sp_from_sstar
public :: gsw_specvol
public :: gsw_specvol_alpha_beta
public :: gsw_specvol_anom_standard
public :: gsw_specvol_first_derivatives
public :: gsw_specvol_first_derivatives_wrt_enthalpy
public :: gsw_specvol_ice
public :: gsw_specvol_second_derivatives
public :: gsw_specvol_second_derivatives_wrt_enthalpy
public :: gsw_specvol_sso_0
public :: gsw_specvol_t_exact
public :: gsw_spiciness0
public :: gsw_spiciness1
public :: gsw_spiciness2
public :: gsw_sr_from_sp
public :: gsw_sstar_from_sa
public :: gsw_sstar_from_sp
public :: gsw_t_deriv_chem_potential_water_t_exact
public :: gsw_t_freezing
public :: gsw_t_freezing_exact
public :: gsw_t_freezing_first_derivatives
public :: gsw_t_freezing_first_derivatives_poly
public :: gsw_t_freezing_poly
public :: gsw_t_from_ct
public :: gsw_t_from_pt0_ice
public :: gsw_thermobaric
public :: gsw_turner_rsubrho
public :: gsw_util_indx
public :: gsw_util_interp1q_int
public :: gsw_util_xinterp1
public :: gsw_z_from_p

interface

    pure subroutine gsw_add_barrier (input_data, long, lat, long_grid, &
                                  lat_grid, dlong_grid, dlat_grid, output_data)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: long, lat, long_grid, lat_grid, dlong_grid
    real (r8), intent(in) :: dlat_grid
    real (r8), intent(in), dimension(4) :: input_data
    real (r8), intent(out), dimension(4) :: output_data
    end subroutine gsw_add_barrier
    
    pure subroutine gsw_add_mean (data_in, data_out)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in), dimension(4) :: data_in
    real (r8), intent(out), dimension(4) :: data_out
    end subroutine gsw_add_mean
    
    elemental function gsw_adiabatic_lapse_rate_from_ct (sa, ct, p) 
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p 
    real (r8) :: gsw_adiabatic_lapse_rate_from_ct
    end function gsw_adiabatic_lapse_rate_from_ct
    
    elemental function gsw_adiabatic_lapse_rate_ice (t, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: t, p
    real (r8) :: gsw_adiabatic_lapse_rate_ice
    end function gsw_adiabatic_lapse_rate_ice
    
    elemental function gsw_alpha (sa, ct, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p
    real (r8) :: gsw_alpha
    end function gsw_alpha
    
    elemental function gsw_alpha_on_beta (sa, ct, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p
    real (r8) :: gsw_alpha_on_beta
    end function gsw_alpha_on_beta
    
    elemental function gsw_alpha_wrt_t_exact (sa, t, p) 
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, t, p 
    real (r8) :: gsw_alpha_wrt_t_exact
    end function gsw_alpha_wrt_t_exact
    
    elemental function gsw_alpha_wrt_t_ice (t, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: t, p
    real (r8) :: gsw_alpha_wrt_t_ice
    end function gsw_alpha_wrt_t_ice
    
    elemental function gsw_beta (sa, ct, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p
    real (r8) :: gsw_beta
    end function gsw_beta
    
    elemental function gsw_beta_const_t_exact (sa, t, p)  
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, t, p  
    real (r8) :: gsw_beta_const_t_exact
    end function gsw_beta_const_t_exact
    
    elemental function gsw_c_from_sp (sp, t, p)       
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sp, t, p       
    real (r8) :: gsw_c_from_sp
    end function gsw_c_from_sp
    
    elemental function gsw_cabbeling (sa, ct, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p
    real (r8) :: gsw_cabbeling
    end function gsw_cabbeling
    
    elemental function gsw_chem_potential_water_ice (t, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: t, p
    real (r8) :: gsw_chem_potential_water_ice
    end function gsw_chem_potential_water_ice
    
    elemental function gsw_chem_potential_water_t_exact (sa, t, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, t, p
    real (r8) :: gsw_chem_potential_water_t_exact
    end function gsw_chem_potential_water_t_exact
    
    elemental function gsw_cp_ice (t, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: t, p
    real (r8) :: gsw_cp_ice
    end function gsw_cp_ice
    
    elemental subroutine gsw_ct_first_derivatives (sa, pt, ct_sa, ct_pt)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, pt
    real (r8), intent(out), optional :: ct_sa, ct_pt
    end subroutine gsw_ct_first_derivatives
    
    elemental subroutine gsw_ct_first_derivatives_wrt_t_exact (sa, t, p, &
                                           ct_sa_wrt_t, ct_t_wrt_t, ct_p_wrt_t)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, t, p
    real (r8), intent(out), optional :: ct_p_wrt_t, ct_sa_wrt_t, ct_t_wrt_t
    end subroutine gsw_ct_first_derivatives_wrt_t_exact
    
    elemental function gsw_ct_freezing (sa, p, saturation_fraction, poly)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, p, saturation_fraction
    logical, intent(in), optional :: poly
    real (r8) :: gsw_ct_freezing
    end function gsw_ct_freezing
    
    elemental function gsw_ct_freezing_exact (sa, p, saturation_fraction)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, p, saturation_fraction
    real (r8) :: gsw_ct_freezing_exact
    end function gsw_ct_freezing_exact
    
    elemental subroutine gsw_ct_freezing_first_derivatives (sa, p, &
                              saturation_fraction, ctfreezing_sa, ctfreezing_p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, p, saturation_fraction
    real (r8), intent(out), optional :: ctfreezing_sa, ctfreezing_p
    end subroutine gsw_ct_freezing_first_derivatives
    
    elemental subroutine gsw_ct_freezing_first_derivatives_poly (sa, p, &
                              saturation_fraction, ctfreezing_sa, ctfreezing_p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, p, saturation_fraction
    real (r8), intent(out), optional :: ctfreezing_sa, ctfreezing_p
    end subroutine gsw_ct_freezing_first_derivatives_poly
    
    elemental function gsw_ct_freezing_poly (sa, p, saturation_fraction)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, p, saturation_fraction
    real (r8) :: gsw_ct_freezing_poly
    end function gsw_ct_freezing_poly
    
    elemental function gsw_ct_from_enthalpy (sa, h, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, h, p
    real (r8) :: gsw_ct_from_enthalpy
    end function gsw_ct_from_enthalpy
    
    elemental function gsw_ct_from_enthalpy_exact (sa, h, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, h, p
    real (r8) :: gsw_ct_from_enthalpy_exact
    end function gsw_ct_from_enthalpy_exact
    
    elemental function gsw_ct_from_entropy (sa, entropy)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, entropy
    real (r8) :: gsw_ct_from_entropy
    end function gsw_ct_from_entropy
    
    elemental function gsw_ct_from_pt (sa, pt) 
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, pt 
    real (r8) :: gsw_ct_from_pt
    end function gsw_ct_from_pt
    
    elemental subroutine gsw_ct_from_rho (rho, sa, p, ct, ct_multiple)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: rho, sa, p
    real (r8), intent(out) :: ct
    real (r8), intent(out), optional :: ct_multiple
    end subroutine gsw_ct_from_rho
    
    elemental function gsw_ct_from_t (sa, t, p) 
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, t, p 
    real (r8) :: gsw_ct_from_t
    end function gsw_ct_from_t
    
    elemental function gsw_ct_maxdensity (sa, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, p
    real (r8) :: gsw_ct_maxdensity
    end function gsw_ct_maxdensity
    
    elemental subroutine gsw_ct_second_derivatives (sa, pt, ct_sa_sa, ct_sa_pt, &
                                                    ct_pt_pt)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, pt
    real (r8), intent(out), optional :: ct_sa_sa, ct_sa_pt, ct_pt_pt
    end subroutine gsw_ct_second_derivatives
    
    elemental function gsw_deltasa_atlas (p, long, lat)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: p, long, lat
    real (r8) :: gsw_deltasa_atlas
    end function gsw_deltasa_atlas
    
    elemental function gsw_deltasa_from_sp (sp, p, long, lat) 
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sp, p, long, lat 
    real (r8) :: gsw_deltasa_from_sp
    end function gsw_deltasa_from_sp
    
    elemental function gsw_dilution_coefficient_t_exact (sa, t, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, t, p
    real (r8) :: gsw_dilution_coefficient_t_exact
    end function gsw_dilution_coefficient_t_exact
    
    elemental function gsw_dynamic_enthalpy (sa, ct, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p
    real (r8) :: gsw_dynamic_enthalpy
    end function gsw_dynamic_enthalpy
    
    elemental function gsw_enthalpy (sa, ct, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p
    real (r8) :: gsw_enthalpy
    end function gsw_enthalpy
    
    elemental function gsw_enthalpy_ct_exact (sa, ct, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p
    real (r8) :: gsw_enthalpy_ct_exact
    end function gsw_enthalpy_ct_exact
    
    elemental function gsw_enthalpy_diff (sa, ct, p_shallow, p_deep)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p_shallow, p_deep
    real (r8) :: gsw_enthalpy_diff
    end function gsw_enthalpy_diff
    
    elemental subroutine gsw_enthalpy_first_derivatives (sa, ct, p, h_sa, h_ct)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p
    real (r8), intent(out), optional :: h_sa, h_ct
    end subroutine gsw_enthalpy_first_derivatives
    
    elemental subroutine gsw_enthalpy_first_derivatives_ct_exact (sa, ct, p, &
                                                                  h_sa, h_ct)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p
    real (r8), intent(out), optional :: h_sa, h_ct
    end subroutine gsw_enthalpy_first_derivatives_ct_exact
    
    elemental function gsw_enthalpy_ice (t, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: t, p
    real (r8) :: gsw_enthalpy_ice
    end function gsw_enthalpy_ice
    
    elemental subroutine gsw_enthalpy_second_derivatives (sa, ct, p, h_sa_sa, &
                                                          h_sa_ct, h_ct_ct)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p
    real (r8), intent(out), optional :: h_sa_sa, h_sa_ct, h_ct_ct
    end subroutine gsw_enthalpy_second_derivatives
    
    elemental subroutine gsw_enthalpy_second_derivatives_ct_exact (sa, ct, p, &
                                                     h_sa_sa, h_sa_ct, h_ct_ct)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p
    real (r8), intent(out), optional :: h_sa_sa, h_sa_ct, h_ct_ct
    end subroutine gsw_enthalpy_second_derivatives_ct_exact
    
    elemental function gsw_enthalpy_sso_0 (p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: p
    real (r8) :: gsw_enthalpy_sso_0
    end function gsw_enthalpy_sso_0
    
    elemental function gsw_enthalpy_t_exact (sa, t, p) 
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, t, p 
    real (r8) :: gsw_enthalpy_t_exact
    end function gsw_enthalpy_t_exact
    
    elemental subroutine gsw_entropy_first_derivatives (sa, ct, eta_sa, eta_ct)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct
    real (r8), intent(out), optional :: eta_sa, eta_ct
    end subroutine gsw_entropy_first_derivatives
    
    elemental function gsw_entropy_from_pt (sa, pt)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, pt
    real (r8) :: gsw_entropy_from_pt
    end function gsw_entropy_from_pt
    
    elemental function gsw_entropy_from_t (sa, t, p) 
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, t, p 
    real (r8) :: gsw_entropy_from_t
    end function gsw_entropy_from_t
    
    elemental function gsw_entropy_ice (t, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: t, p
    real (r8) :: gsw_entropy_ice
    end function gsw_entropy_ice
    
    elemental function gsw_entropy_part (sa, t, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, t, p
    real (r8) :: gsw_entropy_part
    end function gsw_entropy_part
    
    elemental function gsw_entropy_part_zerop (sa, pt0)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, pt0
    real (r8) :: gsw_entropy_part_zerop
    end function gsw_entropy_part_zerop
    
    elemental subroutine gsw_entropy_second_derivatives (sa, ct, eta_sa_sa, &
                                                         eta_sa_ct, eta_ct_ct)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct
    real (r8), intent(out), optional :: eta_sa_sa, eta_sa_ct, eta_ct_ct
    end subroutine gsw_entropy_second_derivatives
    
    elemental function gsw_fdelta (p, long, lat)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: p, long, lat
    real (r8) :: gsw_fdelta
    end function gsw_fdelta
    
    elemental subroutine gsw_frazil_properties (sa_bulk, h_bulk, p, &
                                                sa_final, ct_final, w_ih_final)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa_bulk, h_bulk, p
    real (r8), intent(out) :: sa_final, ct_final, w_ih_final
    end subroutine gsw_frazil_properties
    
    elemental subroutine gsw_frazil_properties_potential (sa_bulk, h_pot_bulk,&
                                             p, sa_final, ct_final, w_ih_final)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa_bulk, h_pot_bulk, p
    real (r8), intent(out) :: sa_final, ct_final, w_ih_final
    end subroutine gsw_frazil_properties_potential
    
    elemental subroutine gsw_frazil_properties_potential_poly (sa_bulk, &
                                 h_pot_bulk, p, sa_final, ct_final, w_ih_final)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa_bulk, h_pot_bulk, p
    real (r8), intent(out) :: sa_final, ct_final, w_ih_final
    end subroutine gsw_frazil_properties_potential_poly
    
    elemental subroutine gsw_frazil_ratios_adiabatic (sa, p, w_ih, &
                                  dsa_dct_frazil, dsa_dp_frazil, dct_dp_frazil)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, p, w_ih
    real (r8), intent(out) :: dsa_dct_frazil, dsa_dp_frazil, dct_dp_frazil
    end subroutine gsw_frazil_ratios_adiabatic
    
    elemental subroutine gsw_frazil_ratios_adiabatic_poly (sa, p, w_ih, &
                                  dsa_dct_frazil, dsa_dp_frazil, dct_dp_frazil)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, p, w_ih
    real (r8), intent(out) :: dsa_dct_frazil, dsa_dp_frazil, dct_dp_frazil
    end subroutine gsw_frazil_ratios_adiabatic_poly
    
    pure function gsw_geo_strf_dyn_height (sa, ct, p, p_ref)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa(:), ct(:), p(:), p_ref
    real (r8) :: gsw_geo_strf_dyn_height(size(sa))
    end function gsw_geo_strf_dyn_height
    
    pure subroutine gsw_geo_strf_dyn_height_pc (sa, ct, delta_p, &
                                                geo_strf_dyn_height_pc, p_mid)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa(:), ct(:), delta_p(:)
    real (r8), intent(out) :: geo_strf_dyn_height_pc(:), p_mid(:)
    end subroutine gsw_geo_strf_dyn_height_pc
    
    elemental function gsw_gibbs (ns, nt, np, sa, t, p)
    use gsw_mod_kinds
    implicit none
    integer, intent(in) :: ns, nt, np
    real (r8), intent(in) :: sa, t, p
    real (r8) :: gsw_gibbs
    end function gsw_gibbs
    
    elemental function gsw_gibbs_ice (nt, np, t, p)
    use gsw_mod_kinds
    implicit none
    integer, intent(in) :: nt, np
    real (r8), intent(in) :: t, p
    real (r8) :: gsw_gibbs_ice
    end function gsw_gibbs_ice
    
    elemental function gsw_gibbs_ice_part_t (t, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: t, p
    real (r8) :: gsw_gibbs_ice_part_t
    end function gsw_gibbs_ice_part_t
    
    elemental function gsw_gibbs_ice_pt0 (pt0)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: pt0
    real (r8) :: gsw_gibbs_ice_pt0
    end function gsw_gibbs_ice_pt0
    
    elemental function gsw_gibbs_ice_pt0_pt0 (pt0)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: pt0
    real (r8) :: gsw_gibbs_ice_pt0_pt0
    end function gsw_gibbs_ice_pt0_pt0
    
    elemental function gsw_gibbs_pt0_pt0 (sa, pt0)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, pt0
    real (r8) :: gsw_gibbs_pt0_pt0
    end function gsw_gibbs_pt0_pt0
    
    elemental function gsw_grav (lat, p)  
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: lat, p  
    real (r8) :: gsw_grav
    end function gsw_grav
    
    elemental function gsw_helmholtz_energy_ice (t, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: t, p
    real (r8) :: gsw_helmholtz_energy_ice
    end function gsw_helmholtz_energy_ice
    
    elemental function gsw_hill_ratio_at_sp2 (t)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: t
    real (r8) :: gsw_hill_ratio_at_sp2
    end function gsw_hill_ratio_at_sp2
    
    elemental subroutine gsw_ice_fraction_to_freeze_seawater (sa, ct, p, &
                                              t_ih, sa_freeze, ct_freeze, w_ih)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p, t_ih
    real (r8), intent(out) :: sa_freeze, ct_freeze, w_ih
    end subroutine gsw_ice_fraction_to_freeze_seawater
    
    elemental function gsw_internal_energy (sa, ct, p)  
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p  
    real (r8) :: gsw_internal_energy
    end function gsw_internal_energy
    
    elemental function gsw_internal_energy_ice (t, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: t, p
    real (r8) :: gsw_internal_energy_ice
    end function gsw_internal_energy_ice
    
    pure subroutine gsw_ipv_vs_fnsquared_ratio (sa, ct, p, p_ref, &
                                                ipv_vs_fnsquared_ratio, p_mid)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa(:), ct(:), p(:), p_ref
    real (r8), intent(out) :: ipv_vs_fnsquared_ratio(:), p_mid(:)
    end subroutine gsw_ipv_vs_fnsquared_ratio
    
    elemental function gsw_kappa (sa, ct, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p
    real (r8) :: gsw_kappa
    end function gsw_kappa
    
    elemental function gsw_kappa_const_t_ice (t, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: t, p
    real (r8) :: gsw_kappa_const_t_ice
    end function gsw_kappa_const_t_ice
    
    elemental function gsw_kappa_ice (t, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: t, p
    real (r8) :: gsw_kappa_ice
    end function gsw_kappa_ice
    
    elemental function gsw_kappa_t_exact (sa, t, p)  
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, t, p  
    real (r8) :: gsw_kappa_t_exact
    end function gsw_kappa_t_exact
    
    elemental function gsw_latentheat_evap_ct (sa, ct) 
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct 
    real (r8) :: gsw_latentheat_evap_ct
    end function gsw_latentheat_evap_ct
    
    elemental function gsw_latentheat_evap_t (sa, t)  
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, t  
    real (r8) :: gsw_latentheat_evap_t
    end function gsw_latentheat_evap_t
    
    elemental function gsw_latentheat_melting (sa, p)  
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, p  
    real (r8) :: gsw_latentheat_melting
    end function gsw_latentheat_melting
    
    pure subroutine gsw_linear_interp_sa_ct (sa, ct, p, p_i, sa_i, ct_i)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa(:), ct(:), p(:), p_i(:)
    real (r8), intent(out) :: sa_i(:), ct_i(:)
    end subroutine gsw_linear_interp_sa_ct
    
    elemental function gsw_melting_ice_equilibrium_sa_ct_ratio (sa, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, p
    real (r8) :: gsw_melting_ice_equilibrium_sa_ct_ratio
    end function gsw_melting_ice_equilibrium_sa_ct_ratio
    
    elemental function gsw_melting_ice_equilibrium_sa_ct_ratio_poly (sa, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, p
    real (r8) :: gsw_melting_ice_equilibrium_sa_ct_ratio_poly
    end function gsw_melting_ice_equilibrium_sa_ct_ratio_poly
    
    elemental subroutine gsw_melting_ice_into_seawater (sa, ct, p, w_ih, t_ih,&
                                                sa_final, ct_final, w_ih_final)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p, w_ih, t_ih
    real (r8), intent(out) :: sa_final, ct_final, w_ih_final
    end subroutine gsw_melting_ice_into_seawater
    
    elemental function gsw_melting_ice_sa_ct_ratio (sa, ct, p, t_ih)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p, t_ih
    real (r8) :: gsw_melting_ice_sa_ct_ratio
    end function gsw_melting_ice_sa_ct_ratio
    
    elemental function gsw_melting_ice_sa_ct_ratio_poly (sa, ct, p, t_ih)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p, t_ih
    real (r8) :: gsw_melting_ice_sa_ct_ratio_poly
    end function gsw_melting_ice_sa_ct_ratio_poly
    
    elemental function gsw_melting_seaice_equilibrium_sa_ct_ratio (sa, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, p
    real (r8) :: gsw_melting_seaice_equilibrium_sa_ct_ratio
    end function gsw_melting_seaice_equilibrium_sa_ct_ratio
    
    elemental function gsw_melting_seaice_equilibrium_sa_ct_ratio_poly (sa, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, p
    real (r8) :: gsw_melting_seaice_equilibrium_sa_ct_ratio_poly
    end function gsw_melting_seaice_equilibrium_sa_ct_ratio_poly
    
    elemental subroutine gsw_melting_seaice_into_seawater (sa, ct, p, &
                             w_seaice, sa_seaice, t_seaice, sa_final, ct_final)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p, w_seaice, sa_seaice, t_seaice
    real (r8), intent(out) :: sa_final, ct_final
    end subroutine gsw_melting_seaice_into_seawater
    
    elemental function gsw_melting_seaice_sa_ct_ratio (sa, ct, p, sa_seaice, &
                                                       t_seaice)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p, sa_seaice, t_seaice
    real (r8) :: gsw_melting_seaice_sa_ct_ratio
    end function gsw_melting_seaice_sa_ct_ratio
    
    elemental function gsw_melting_seaice_sa_ct_ratio_poly (sa, ct, p, &
                                                           sa_seaice, t_seaice)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p, sa_seaice, t_seaice
    real (r8) :: gsw_melting_seaice_sa_ct_ratio_poly
    end function gsw_melting_seaice_sa_ct_ratio_poly
    
    pure function gsw_mlp (sa, ct, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa(:), ct(:), p(:)
    real (r8) :: gsw_mlp
    end function gsw_mlp
    
    pure subroutine gsw_nsquared (sa, ct, p, lat, n2, p_mid)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa(:), ct(:), p(:), lat(:)
    real (r8), intent(out) :: n2(:), p_mid(:)
    end subroutine gsw_nsquared
    
    elemental function gsw_nsquared_lowerlimit (p, long, lat)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: p, long, lat
    real (r8) :: gsw_nsquared_lowerlimit
    end function gsw_nsquared_lowerlimit
    
    pure subroutine gsw_nsquared_min (sa, ct, p, lat, n2, n2_p, &
                                   n2_specvol, n2_alpha, n2_beta, dsa, dct, dp)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa(:), ct(:), p(:), lat(:)
    real (r8), intent(out) :: n2(:), n2_p(:), n2_specvol(:), n2_alpha(:)
    real (r8), intent(out) :: n2_beta(:), dsa(:), dct(:), dp(:)
    end subroutine gsw_nsquared_min
    
    pure subroutine gsw_nsquared_min_const_t (sa, t, p, lat, n2, n2_p, &
                    n2_specvol, n2_alpha, n2_beta, dsa, dct, dp, n2_beta_ratio)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa(:), t(:), p(:), lat
    real (r8), intent(out) :: n2(:), n2_p(:), n2_specvol(:), n2_alpha(:)
    real (r8), intent(out) :: n2_beta(:), dsa(:), dct(:), dp(:)
    real (r8), intent(out) :: n2_beta_ratio(:)
    end subroutine gsw_nsquared_min_const_t
    
    elemental function gsw_p_from_z (z, lat, geo_strf_dyn_height, &
                                                       sea_surface_geopotental)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: z, lat
    real (r8), intent(in), optional :: geo_strf_dyn_height
    real (r8), intent(in), optional :: sea_surface_geopotental
    real (r8) :: gsw_p_from_z
    end function gsw_p_from_z
    
    elemental function gsw_pot_enthalpy_from_pt_ice (pt0_ice)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: pt0_ice
    real (r8) :: gsw_pot_enthalpy_from_pt_ice
    end function gsw_pot_enthalpy_from_pt_ice
    
    elemental function gsw_pot_enthalpy_from_pt_ice_poly (pt0_ice)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: pt0_ice
    real (r8) :: gsw_pot_enthalpy_from_pt_ice_poly
    end function gsw_pot_enthalpy_from_pt_ice_poly
    
    elemental function gsw_pot_enthalpy_ice_freezing (sa, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, p
    real (r8) :: gsw_pot_enthalpy_ice_freezing
    end function gsw_pot_enthalpy_ice_freezing
    
    elemental subroutine gsw_pot_enthalpy_ice_freezing_first_derivatives (sa, &
                  p, pot_enthalpy_ice_freezing_sa, pot_enthalpy_ice_freezing_p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, p
    real (r8), intent(out), optional :: pot_enthalpy_ice_freezing_sa
    real (r8), intent(out), optional :: pot_enthalpy_ice_freezing_p
    end subroutine gsw_pot_enthalpy_ice_freezing_first_derivatives
    
    elemental subroutine gsw_pot_enthalpy_ice_freezing_first_derivatives_poly(&
             sa, p, pot_enthalpy_ice_freezing_sa, pot_enthalpy_ice_freezing_p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, p
    real (r8), intent(out), optional :: pot_enthalpy_ice_freezing_sa
    real (r8), intent(out), optional :: pot_enthalpy_ice_freezing_p
    end subroutine gsw_pot_enthalpy_ice_freezing_first_derivatives_poly
    
    elemental function gsw_pot_enthalpy_ice_freezing_poly (sa, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, p
    real (r8) :: gsw_pot_enthalpy_ice_freezing_poly
    end function gsw_pot_enthalpy_ice_freezing_poly
    
    elemental function gsw_pot_rho_t_exact (sa, t, p, p_ref)  
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, t, p, p_ref  
    real (r8) :: gsw_pot_rho_t_exact
    end function gsw_pot_rho_t_exact
    
    elemental function gsw_pressure_coefficient_ice (t, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: t, p
    real (r8) :: gsw_pressure_coefficient_ice
    end function gsw_pressure_coefficient_ice
    
    elemental function gsw_pressure_freezing_ct (sa, ct, saturation_fraction)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, saturation_fraction
    real (r8) :: gsw_pressure_freezing_ct
    end function gsw_pressure_freezing_ct
    
    elemental function gsw_pt0_cold_ice_poly (pot_enthalpy_ice)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: pot_enthalpy_ice
    real (r8) :: gsw_pt0_cold_ice_poly
    end function gsw_pt0_cold_ice_poly
    
    elemental function gsw_pt0_from_t (sa, t, p) 
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, t, p 
    real (r8) :: gsw_pt0_from_t
    end function gsw_pt0_from_t
    
    elemental function gsw_pt0_from_t_ice (t, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: t, p
    real (r8) :: gsw_pt0_from_t_ice
    end function gsw_pt0_from_t_ice
    
    elemental subroutine gsw_pt_first_derivatives (sa, ct, pt_sa, pt_ct)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct
    real (r8), intent(out), optional :: pt_sa, pt_ct
    end subroutine gsw_pt_first_derivatives
    
    elemental function gsw_pt_from_ct (sa, ct) 
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct 
    real (r8) :: gsw_pt_from_ct
    end function gsw_pt_from_ct
    
    elemental function gsw_pt_from_entropy (sa, entropy)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, entropy
    real (r8) :: gsw_pt_from_entropy
    end function gsw_pt_from_entropy
    
    elemental function gsw_pt_from_pot_enthalpy_ice (pot_enthalpy_ice)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: pot_enthalpy_ice
    real (r8) :: gsw_pt_from_pot_enthalpy_ice
    end function gsw_pt_from_pot_enthalpy_ice
    
    elemental function gsw_pt_from_pot_enthalpy_ice_poly (pot_enthalpy_ice)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: pot_enthalpy_ice
    real (r8) :: gsw_pt_from_pot_enthalpy_ice_poly
    end function gsw_pt_from_pot_enthalpy_ice_poly
    
    elemental function gsw_pt_from_pot_enthalpy_ice_poly_dh (pot_enthalpy_ice)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: pot_enthalpy_ice
    real (r8) :: gsw_pt_from_pot_enthalpy_ice_poly_dh
    end function gsw_pt_from_pot_enthalpy_ice_poly_dh
    
    elemental function gsw_pt_from_t (sa, t, p, p_ref) 
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, t, p, p_ref 
    real (r8) :: gsw_pt_from_t
    end function gsw_pt_from_t
    
    elemental function gsw_pt_from_t_ice (t, p, p_ref)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: t, p, p_ref
    real (r8) :: gsw_pt_from_t_ice
    end function gsw_pt_from_t_ice
    
    elemental subroutine gsw_pt_second_derivatives (sa, ct, pt_sa_sa, &
                                                    pt_sa_ct, pt_ct_ct)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct
    real (r8), intent(out), optional :: pt_sa_sa, pt_sa_ct, pt_ct_ct
    end subroutine gsw_pt_second_derivatives
    
    elemental function gsw_rho (sa, ct, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p
    real (r8) :: gsw_rho
    end function gsw_rho
    
    elemental subroutine gsw_rho_alpha_beta (sa, ct, p, rho, alpha, beta)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p
    real (r8), intent(out), optional :: rho, alpha, beta
    end subroutine gsw_rho_alpha_beta
    
    elemental subroutine gsw_rho_alpha_beta_bsq (sa, ct, p, rho, alpha, beta, &
                                                 stiffened)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p
    logical, intent(in), optional :: stiffened
    real (r8), intent(out) :: rho
    real (r8), intent(out), optional :: alpha, beta
    end subroutine gsw_rho_alpha_beta_bsq
    
    elemental subroutine gsw_rho_first_derivatives (sa, ct, p, drho_dsa, &
                                                    drho_dct, drho_dp)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p
    real (r8), intent(out), optional :: drho_dsa, drho_dct, drho_dp
    end subroutine gsw_rho_first_derivatives
    
    elemental subroutine gsw_rho_first_derivatives_wrt_enthalpy (sa, ct, p, &
                                                                 rho_sa, rho_h)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p
    real (r8), intent(out), optional :: rho_sa, rho_h
    end subroutine gsw_rho_first_derivatives_wrt_enthalpy
    
    elemental function gsw_rho_ice (t, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: t, p
    real (r8) :: gsw_rho_ice
    end function gsw_rho_ice
    
    elemental subroutine gsw_rho_second_derivatives (sa, ct, p, rho_sa_sa, &
                                      rho_sa_ct, rho_ct_ct, rho_sa_p, rho_ct_p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p
    real (r8), intent(out), optional :: rho_sa_sa, rho_sa_ct, rho_ct_ct
    real (r8), intent(out), optional :: rho_sa_p, rho_ct_p
    end subroutine gsw_rho_second_derivatives
    
    elemental subroutine gsw_rho_second_derivatives_wrt_enthalpy (sa, ct, p, &
                                                  rho_sa_sa, rho_sa_h, rho_h_h)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p
    real (r8), intent(out), optional :: rho_sa_sa, rho_sa_h, rho_h_h
    end subroutine gsw_rho_second_derivatives_wrt_enthalpy
    
    elemental function gsw_rho_t_exact (sa, t, p) 
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, t, p 
    real (r8) :: gsw_rho_t_exact
    end function gsw_rho_t_exact
    
    pure subroutine gsw_rr68_interp_sa_ct (sa, ct, p, p_i, sa_i, ct_i)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa(:), ct(:), p(:), p_i(:)
    real (r8), intent(out) :: sa_i(:), ct_i(:)
    end subroutine gsw_rr68_interp_sa_ct
    
    elemental function gsw_sa_freezing_estimate (p, saturation_fraction, ct, t)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: p, saturation_fraction
    real (r8), intent(in), optional :: ct, t
    real (r8) :: gsw_sa_freezing_estimate
    end function gsw_sa_freezing_estimate
    
    elemental function gsw_sa_freezing_from_ct (ct, p, saturation_fraction)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: ct, p, saturation_fraction
    real (r8) :: gsw_sa_freezing_from_ct
    end function gsw_sa_freezing_from_ct
    
    elemental function gsw_sa_freezing_from_ct_poly (ct, p, saturation_fraction)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: ct, p, saturation_fraction
    real (r8) :: gsw_sa_freezing_from_ct_poly
    end function gsw_sa_freezing_from_ct_poly
    
    elemental function gsw_sa_freezing_from_t (t, p, saturation_fraction)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: t, p, saturation_fraction
    real (r8) :: gsw_sa_freezing_from_t
    end function gsw_sa_freezing_from_t
    
    elemental function gsw_sa_freezing_from_t_poly (t, p, saturation_fraction)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: t, p, saturation_fraction
    real (r8) :: gsw_sa_freezing_from_t_poly
    end function gsw_sa_freezing_from_t_poly
    
    elemental function gsw_sa_from_rho (rho, ct, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: rho, ct, p
    real (r8) :: gsw_sa_from_rho
    end function gsw_sa_from_rho
    
    elemental function gsw_sa_from_sp (sp, p, long, lat)       
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sp, p, long, lat       
    real (r8) :: gsw_sa_from_sp
    end function gsw_sa_from_sp
    
    elemental function gsw_sa_from_sp_baltic (sp, long, lat)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sp, long, lat
    real (r8) :: gsw_sa_from_sp_baltic
    end function gsw_sa_from_sp_baltic
    
    elemental function gsw_sa_from_sstar (sstar, p, long, lat)  
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sstar, p, long, lat  
    real (r8) :: gsw_sa_from_sstar
    end function gsw_sa_from_sstar
    
    elemental function gsw_sa_p_inrange (sa, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, p
    logical :: gsw_sa_p_inrange
    end function gsw_sa_p_inrange
    
    elemental function gsw_saar (p, long, lat)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: p, long, lat
    real (r8) :: gsw_saar
    end function gsw_saar
    
    elemental subroutine gsw_seaice_fraction_to_freeze_seawater (sa, ct, p, &
                           sa_seaice, t_seaice, sa_freeze, ct_freeze, w_seaice)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p, sa_seaice, t_seaice
    real (r8), intent(out) :: sa_freeze, ct_freeze, w_seaice
    end subroutine gsw_seaice_fraction_to_freeze_seawater
    
    elemental function gsw_sigma0 (sa, ct)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct
    real (r8) :: gsw_sigma0
    end function gsw_sigma0
    
    elemental function gsw_sigma1 (sa, ct) 
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct 
    real (r8) :: gsw_sigma1
    end function gsw_sigma1
    
    elemental function gsw_sigma2 (sa, ct) 
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct 
    real (r8) :: gsw_sigma2
    end function gsw_sigma2
    
    elemental function gsw_sigma3 (sa, ct) 
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct 
    real (r8) :: gsw_sigma3
    end function gsw_sigma3
    
    elemental function gsw_sigma4 (sa, ct) 
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct 
    real (r8) :: gsw_sigma4
    end function gsw_sigma4
    
    elemental function gsw_sound_speed (sa, ct, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p
    real (r8) :: gsw_sound_speed
    end function gsw_sound_speed
    
    elemental function gsw_sound_speed_ice (t, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: t, p
    real (r8) :: gsw_sound_speed_ice
    end function gsw_sound_speed_ice
    
    elemental function gsw_sound_speed_t_exact (sa, t, p)  
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, t, p  
    real (r8) :: gsw_sound_speed_t_exact
    end function gsw_sound_speed_t_exact
    
    elemental function gsw_sp_from_c (c, t, p)       
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: c, t, p       
    real (r8) :: gsw_sp_from_c
    end function gsw_sp_from_c
    
    elemental function gsw_sp_from_sa (sa, p, long, lat) 
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, p, long, lat 
    real (r8) :: gsw_sp_from_sa
    end function gsw_sp_from_sa
    
    elemental function gsw_sp_from_sa_baltic (sa, long, lat)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, long, lat
    real (r8) :: gsw_sp_from_sa_baltic
    end function gsw_sp_from_sa_baltic
    
    elemental function gsw_sp_from_sk (sk)       
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sk       
    real (r8) :: gsw_sp_from_sk
    end function gsw_sp_from_sk
    
    elemental function gsw_sp_from_sr (sr)  
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sr  
    real (r8) :: gsw_sp_from_sr
    end function gsw_sp_from_sr
    
    elemental function gsw_sp_from_sstar (sstar, p, long, lat)  
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sstar, p, long, lat  
    real (r8) :: gsw_sp_from_sstar
    end function gsw_sp_from_sstar
    
    elemental function gsw_specvol (sa, ct, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p
    real (r8) :: gsw_specvol
    end function gsw_specvol
    
    elemental subroutine gsw_specvol_alpha_beta (sa, ct, p, specvol, alpha, &
                                                 beta)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p
    real (r8), intent(out), optional :: specvol, alpha, beta
    end subroutine gsw_specvol_alpha_beta
    
    elemental function gsw_specvol_anom_standard (sa, ct, p)  
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p  
    real (r8) :: gsw_specvol_anom_standard
    end function gsw_specvol_anom_standard
    
    elemental subroutine gsw_specvol_first_derivatives (sa, ct, p, v_sa, v_ct, &
                                                        v_p, iflag)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p
    integer, intent(in), optional :: iflag
    real (r8), intent(out), optional :: v_sa, v_ct, v_p
    end subroutine gsw_specvol_first_derivatives
    
    elemental subroutine gsw_specvol_first_derivatives_wrt_enthalpy (sa, ct, &
                                                           p, v_sa, v_h, iflag)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p
    integer, intent(in), optional :: iflag
    real (r8), intent(out), optional :: v_sa, v_h
    end subroutine gsw_specvol_first_derivatives_wrt_enthalpy
    
    elemental function gsw_specvol_ice (t, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: t, p
    real (r8) :: gsw_specvol_ice
    end function gsw_specvol_ice
    
    elemental subroutine gsw_specvol_second_derivatives (sa, ct, p, v_sa_sa, &
                                       v_sa_ct, v_ct_ct, v_sa_p, v_ct_p, iflag)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p
    integer, intent(in), optional :: iflag
    real (r8), intent(out), optional :: v_sa_sa, v_sa_ct, v_ct_ct, v_sa_p, v_ct_p
    end subroutine gsw_specvol_second_derivatives
    
    elemental subroutine gsw_specvol_second_derivatives_wrt_enthalpy (sa, ct, &
                                              p, v_sa_sa, v_sa_h, v_h_h, iflag)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p
    integer, intent(in), optional :: iflag
    real (r8), intent(out), optional :: v_sa_sa, v_sa_h, v_h_h
    end subroutine gsw_specvol_second_derivatives_wrt_enthalpy
    
    elemental function gsw_specvol_sso_0 (p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: p
    real (r8) :: gsw_specvol_sso_0
    end function gsw_specvol_sso_0
    
    elemental function gsw_specvol_t_exact (sa, t, p)  
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, t, p  
    real (r8) :: gsw_specvol_t_exact
    end function gsw_specvol_t_exact
    
    elemental function gsw_spiciness0 (sa, ct)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct
    real (r8) :: gsw_spiciness0
    end function gsw_spiciness0
    
    elemental function gsw_spiciness1 (sa, ct)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct
    real (r8) :: gsw_spiciness1
    end function gsw_spiciness1
    
    elemental function gsw_spiciness2 (sa, ct)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct
    real (r8) :: gsw_spiciness2
    end function gsw_spiciness2
    
    elemental function gsw_sr_from_sp (sp) 
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sp 
    real (r8) :: gsw_sr_from_sp
    end function gsw_sr_from_sp
    
    elemental function gsw_sstar_from_sa (sa, p, long, lat) 
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, p, long, lat 
    real (r8) :: gsw_sstar_from_sa
    end function gsw_sstar_from_sa
    
    elemental function gsw_sstar_from_sp (sp, p, long, lat) 
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sp, p, long, lat 
    real (r8) :: gsw_sstar_from_sp
    end function gsw_sstar_from_sp
    
    elemental function gsw_t_deriv_chem_potential_water_t_exact (sa, t, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, t, p
    real (r8) :: gsw_t_deriv_chem_potential_water_t_exact
    end function gsw_t_deriv_chem_potential_water_t_exact
    
    elemental function gsw_t_freezing (sa, p, saturation_fraction, poly)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, p, saturation_fraction
    logical, intent(in), optional :: poly
    real (r8) :: gsw_t_freezing
    end function gsw_t_freezing
    
    elemental function gsw_t_freezing_exact (sa, p, saturation_fraction)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, p, saturation_fraction
    real (r8) :: gsw_t_freezing_exact
    end function gsw_t_freezing_exact
    
    elemental subroutine gsw_t_freezing_first_derivatives (sa, p, &
                                saturation_fraction, tfreezing_sa, tfreezing_p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, p, saturation_fraction
    real (r8), intent(out), optional :: tfreezing_sa, tfreezing_p
    end subroutine gsw_t_freezing_first_derivatives
    
    elemental subroutine gsw_t_freezing_first_derivatives_poly (sa, p, &
                                saturation_fraction, tfreezing_sa, tfreezing_p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, p, saturation_fraction
    real (r8), intent(out), optional :: tfreezing_sa, tfreezing_p
    end subroutine gsw_t_freezing_first_derivatives_poly
    
    elemental function gsw_t_freezing_poly (sa, p, saturation_fraction, polynomial)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, p
    real (r8), intent(in), optional :: saturation_fraction
    logical, intent(in), optional :: polynomial
    real (r8) :: gsw_t_freezing_poly
    end function gsw_t_freezing_poly
    
    elemental function gsw_t_from_ct (sa, ct, p)  
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p  
    real (r8) :: gsw_t_from_ct
    end function gsw_t_from_ct
    
    elemental function gsw_t_from_pt0_ice (pt0_ice, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: pt0_ice, p
    real (r8) :: gsw_t_from_pt0_ice
    end function gsw_t_from_pt0_ice
    
    elemental function gsw_thermobaric (sa, ct, p)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa, ct, p
    real (r8) :: gsw_thermobaric
    end function gsw_thermobaric
    
    pure subroutine gsw_turner_rsubrho (sa, ct, p, tu, rsubrho, p_mid)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: sa(:), ct(:), p(:)
    real (r8), intent(out) :: tu(:), rsubrho(:), p_mid(:)
    end subroutine gsw_turner_rsubrho
    
    pure function gsw_util_indx (x, z, kstart) result(ki)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: x(:), z
    integer, intent(in), optional :: kstart
    integer :: ki
    end function gsw_util_indx
    
    pure function gsw_util_interp1q_int (x, iy, x_i) result(y_i)
    use gsw_mod_kinds
    implicit none
    integer, intent(in) :: iy(:)
    real (r8), intent(in) :: x(:), x_i(:)
    real (r8) :: y_i(size(x_i))
    end function gsw_util_interp1q_int
    
    pure function gsw_util_xinterp1 (x, y, x0)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: x0
    real (r8), intent(in) :: x(:), y(:)
    real (r8) :: gsw_util_xinterp1
    end function gsw_util_xinterp1
    
    elemental function gsw_z_from_p (p, lat, geo_strf_dyn_height, &
                                                       sea_surface_geopotental)
    use gsw_mod_kinds
    implicit none
    real (r8), intent(in) :: p, lat
    real (r8), intent(in), optional :: geo_strf_dyn_height
    real (r8), intent(in), optional :: sea_surface_geopotental
    real (r8) :: gsw_z_from_p
    end function gsw_z_from_p
    
end interface

end module gsw_mod_toolbox
