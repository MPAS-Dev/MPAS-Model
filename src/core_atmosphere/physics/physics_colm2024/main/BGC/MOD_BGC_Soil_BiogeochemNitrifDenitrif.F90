#include <define.h>
#ifdef BGC
MODULE MOD_BGC_Soil_BiogeochemNitrifDenitrif

!--------------------------------------------------------------------------------------------
! !DESCRIPTION:
! Calculate the potential nitrification and dentrification rate.
!
! !REFERENCES:
! Parton, W. et al. 1996. Generalized model for N2 and N2O production from nitrification and
! denitrification. Global Biogeochemical Cycles 10(3):401-412.
! Parton, W.J. et al. 2001. Generalized model for NOx and N2O emissions from soils. J. Geophys. Res.
! 106(D15):17403-17419
!
! !ORIGINAL:
! The Community Land Model version 5.0 (CLM5.0)
!
! !REVISION:
! Xingjie Lu, 2021, revised original CLM5 code to be compatible with CoLM code structure.

   USE MOD_Precision
   USE MOD_Const_Physical, only: denice, denh2o, tfrz
   USE MOD_Vars_TimeVariables, only: t_soisno, wliq_soisno, wice_soisno, t_scalar, w_scalar, smp
   USE MOD_Vars_TimeInvariants, only: &
       porsl, wfc, bsw, BD_all, OM_density

   USE MOD_BGC_Vars_TimeInvariants, only: &
       surface_tension_water, rij_kro_a, rij_kro_alpha, rij_kro_beta, rij_kro_gamma, rij_kro_delta, organic_max, &
       k_nitr_max, d_con_g21, d_con_g22, d_con_w21, d_con_w22, d_con_w23, &
       denit_resp_coef, denit_resp_exp, denit_nitrate_coef, denit_nitrate_exp

   USE MOD_BGC_Vars_TimeVariables, only: &
       ! decomposition carbon & nitrogen pools
       to2_decomp_depth_unsat, tconc_o2_unsat, smin_nh4_vr, smin_no3_vr


       ! other variables

   USE MOD_BGC_Vars_1DFluxes, only: &
       ! decomposition fluxes variables

       ! mineral N fluxes
       phr_vr, pot_f_nit_vr, pot_f_denit_vr, n2_n2o_ratio_denit_vr

   IMPLICIT NONE

   PUBLIC SoilBiogeochemNitrifDenitrif

CONTAINS

   SUBROUTINE SoilBiogeochemNitrifDenitrif(i,nl_soil,dz_soi)

   integer ,intent(in) :: i                   ! patch index
   integer ,intent(in) :: nl_soil             ! number of total soil layers
   real(r8),intent(in) :: dz_soi (1:nl_soil)  ! thicknesses of each soil layer (m)

   integer j

   real(r8) :: soil_hr_vr ! total soil respiration rate (g C / m3 / s)
   real(r8) :: soil_bulkdensity
   real(r8) :: g_per_m3__to__ug_per_gsoil
   real(r8) :: g_per_m3_sec__to__ug_per_gsoil_day
   real(r8) :: pH
   real(r8) :: eps
   real(r8) :: f_a
   real(r8) :: rho_w  = 1.e3_r8                   ! (kg/m3)
   real(r8) :: r_max
   real(r8) :: r_min(1:nl_soil), r_psi(1:nl_soil)
   real(r8) :: ratio_diffusivity_water_gas(1:nl_soil)
   real(r8) :: om_frac
   real(r8) :: diffus
   real(r8) :: vol_ice, vol_liq, eff_porosity, anaerobic_frac
   real(r8) :: k_nitr_t_vr, k_nitr_ph_vr, k_nitr_h2o_vr, k_nitr_vr
   real(r8) :: smin_no3_massdens_vr, soil_co2_prod, fmax_denit_carbonsubstrate_vr, fmax_denit_nitrate_vr
   real(r8) :: f_denit_base_vr, ratio_k1, ratio_no3_co2, wfps_vr, fr_WFPS
   real(r8),parameter :: PI = 4.*atan(1.)

      pH = 6.5

      DO j = 1, nl_soil

         f_a = 1._r8 - wfc(j,i) / porsl(j,i)
         eps =  porsl(j,i)-wfc(j,i) ! Air-filled fraction of total soil volume

         ! use diffusivity calculation including peat
         IF (organic_max > 0._r8) THEN
            om_frac = min(OM_density(j,i)/organic_max, 1._r8)
            ! Use first power, not square as in iniTimeConst
         ELSE
            om_frac = 1._r8
         ENDIF
         diffus = (d_con_g21 + d_con_g22*t_soisno(j,i)) * 1.e-4_r8 * &
              (om_frac * f_a**(10._r8/3._r8) / porsl(j,i)**2 + &
              (1._r8-om_frac) * eps**2 * f_a**(3._r8 / bsw(j,i)) )

         ! calculate anoxic fraction of soils
         ! use rijtema and kroess model after Riley et al., 2000
         ! caclulated r_psi as a FUNCTION of psi
         r_min(j) = 2 * surface_tension_water / (rho_w * 9.80616_r8 * amax1(abs(smp(j,i) * 1.e-5_r8),1.e-10))
         r_max    = 2 * surface_tension_water / (rho_w * 9.80616_r8 * 0.1_r8)
         r_psi(j) = sqrt(r_min(j) * r_max)
         ratio_diffusivity_water_gas(j) = (d_con_g21 + d_con_g22*t_soisno(j,i) ) * 1.e-4_r8 / &
              ((d_con_w21 + d_con_w22*t_soisno(j,i) + d_con_w23*t_soisno(j,i)**2) * 1.e-9_r8)

         vol_ice = min(porsl(j,i), wice_soisno(j,i)/(dz_soi(j)*denice))
         eff_porosity =  max(0.01, porsl(j,i)-vol_ice)
         vol_liq = min(eff_porosity, wliq_soisno(j,i)/(dz_soi(j)*denh2o))
         IF (to2_decomp_depth_unsat(j,i) > 0._r8) THEN
            anaerobic_frac = exp(-rij_kro_a * r_psi(j)**(-rij_kro_alpha) * &
                 to2_decomp_depth_unsat(j,i)**(-rij_kro_beta) * &
                 tconc_o2_unsat(j,i)**rij_kro_gamma * (vol_liq + ratio_diffusivity_water_gas(j) * &
                 porsl(j,i))**rij_kro_delta)
         ELSE
            anaerobic_frac = 0._r8
         ENDIF

         k_nitr_t_vr = min(t_scalar(j,i), 1._r8)

         ! ph function from Parton et al., (2001, 1996)
         k_nitr_ph_vr = 0.56 + atan(PI * 0.45 * (-5.+ pH))/PI

         ! moisture function-- assume the same moisture function as limits heterotrophic respiration
         ! Parton et al. base their nitrification- soil moisture rate constants based on heterotrophic rates-- can we DO the same?
         k_nitr_h2o_vr = w_scalar(j,i)

         ! nitrification constant is a set scalar * temp, moisture, and ph scalars
         k_nitr_vr = k_nitr_max * k_nitr_t_vr * k_nitr_h2o_vr * k_nitr_ph_vr

         ! first-order decay of ammonium pool with scalar defined above
         pot_f_nit_vr(j,i) = max(smin_nh4_vr(j,i) * k_nitr_vr, 0._r8)

         ! limit to oxic fraction of soils
         pot_f_nit_vr(j,i)  = pot_f_nit_vr(j,i) * (1._r8 - anaerobic_frac)

         !---------------- denitrification
         ! first some input variables an unit conversions
         soil_hr_vr = phr_vr(j,i)

         ! CENTURY papers give denitrification in units of per gram soil; need to convert from volumetric to mass-based units here
         soil_bulkdensity = BD_all(j,i) + wliq_soisno(j,i)/dz_soi(j)

         g_per_m3__to__ug_per_gsoil = 1.e3_r8 / soil_bulkdensity

         g_per_m3_sec__to__ug_per_gsoil_day = g_per_m3__to__ug_per_gsoil * 86400._r8

         smin_no3_massdens_vr = max(smin_no3_vr(j,i), 0._r8) * g_per_m3__to__ug_per_gsoil

         soil_co2_prod = (soil_hr_vr * (g_per_m3_sec__to__ug_per_gsoil_day))

         !! maximum potential denitrification rates based on heterotrophic respiration rates or nitrate concentrations,
         !! from (del Grosso et al., 2000)
         fmax_denit_carbonsubstrate_vr = (denit_resp_coef * (soil_co2_prod**denit_resp_exp)) &
              / g_per_m3_sec__to__ug_per_gsoil_day
         !
         fmax_denit_nitrate_vr = (denit_nitrate_coef * smin_no3_massdens_vr**denit_nitrate_exp)  &
              / g_per_m3_sec__to__ug_per_gsoil_day

         ! find limiting denitrification rate
         f_denit_base_vr = max(min(fmax_denit_carbonsubstrate_vr, fmax_denit_nitrate_vr),0._r8)

         ! limit to anoxic fraction of soils
         pot_f_denit_vr(j,i) = f_denit_base_vr * anaerobic_frac

         ! now calculate the ratio of N2O to N2 from denitrifictaion, following Del Grosso et al., 2000
         ! diffusivity constant (figure 6b)
         ratio_k1 = max(1.7_r8, 38.4_r8 - 350._r8 * diffus)

         ! ratio function (figure 7c)
         IF ( soil_co2_prod > 1.0e-9_r8 ) THEN
            ratio_no3_co2 = smin_no3_massdens_vr / soil_co2_prod
         ELSE
            ! fucntion saturates at large no3/co2 ratios, so set as some nominally large number
            ratio_no3_co2 = 100._r8
         ENDIF

         ! total water limitation function (Del Grosso et al., 2000, figure 7a)
         wfps_vr = max(min(vol_liq/porsl(j,i), 1._r8), 0._r8) * 100._r8
         fr_WFPS = max(0.1_r8, 0.015_r8 * wfps_vr - 0.32_r8)

         ! final ratio expression
         n2_n2o_ratio_denit_vr(j,i) = max(0.16*ratio_k1, ratio_k1*exp(-0.8 * ratio_no3_co2)) * fr_WFPS

      ENDDO
   END SUBROUTINE SoilBiogeochemNitrifDenitrif
END MODULE MOD_BGC_Soil_BiogeochemNitrifDenitrif
#endif
