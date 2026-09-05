#include <define.h>
#ifdef BGC
MODULE MOD_BGC_Soil_BiogeochemCompetition

!---------------------------------------------------------------------------------------------------
! !DESCRIPTION:
! Calculate the soil mineral nitrogen competition between soil microbial (immobilisation) and plant (N uptake).
! Note that there is no non-linear microbial model in CoLM-BGC.
!
! !ORIGINAL:
! The Community Land Model version 5.0 (CLM5.0)
!
! !REVISION:
! Xingjie Lu, 2022, modify original CLM5 to be compatible with CoLM code structure.

   USE MOD_Precision
   USE MOD_Vars_Global, only: npcropmin
   USE MOD_Namelist, only: DEF_USE_NITRIF, DEF_USE_NOSTRESSNITROGEN
   USE MOD_LandPFT, only: patch_pft_s, patch_pft_e
   USE MOD_Vars_PFTimeInvariants, only: pftclass
   USE MOD_BGC_Vars_1DFluxes, only: &
       pot_f_nit_vr, potential_immob_vr, sminn_to_plant_vr, sminn_to_denit_excess_vr, plant_ndemand, &
       actual_immob_vr, sminn_to_plant, pot_f_nit_vr, actual_immob_nh4_vr, f_nit_vr, &
       smin_nh4_to_plant_vr, pot_f_denit_vr, actual_immob_no3_vr, f_denit_vr, smin_no3_to_plant_vr, &
       n2_n2o_ratio_denit_vr, f_n2o_nit_vr, f_n2o_denit_vr, supplement_to_sminn_vr
   USE MOD_BGC_Vars_TimeVariables, only: &
       sminn_vr, smin_no3_vr, smin_nh4_vr, nfixation_prof, fpi_vr, fpi, fpg
   USE MOD_BGC_Vars_TimeInvariants,only: &
       bdnr, compet_plant_no3, compet_plant_nh4, compet_decomp_no3, compet_decomp_nh4, compet_denit, compet_nit, &
       nitrif_n2o_loss_frac

   IMPLICIT NONE

   PUBLIC SoilBiogeochemCompetition

CONTAINS

   SUBROUTINE SoilBiogeochemCompetition(i,deltim,nl_soil,dz_soi)

   integer ,intent(in) :: i                 ! patch index
   real(r8),intent(in) :: deltim            ! time step in seconds
   integer ,intent(in) :: nl_soil           ! number of total soil layers
   real(r8),intent(in) :: dz_soi(1:nl_soil) ! thicknesses of each soil layer (m)


   integer  :: p,l,pi,j                                            ! indices
   real(r8) :: fpi_no3_vr(1:nl_soil)      ! fraction of potential immobilization supplied by no3(no units)
   real(r8) :: fpi_nh4_vr(1:nl_soil)      ! fraction of potential immobilization supplied by nh4 (no units)
   real(r8) :: sum_nh4_demand(1:nl_soil)
   real(r8) :: sum_nh4_demand_scaled(1:nl_soil)
   real(r8) :: sum_no3_demand(1:nl_soil)
   real(r8) :: sum_no3_demand_scaled(1:nl_soil)
   real(r8) :: sum_ndemand_vr( 1:nl_soil) !total column N demand (gN/m3/s) at a given level
   real(r8) :: nuptake_prof( 1:nl_soil)
   real(r8) :: sminn_tot
   integer  :: nlimit(1:nl_soil)          !flag for N limitation
   integer  :: nlimit_no3(1:nl_soil)      !flag for NO3 limitation
   integer  :: nlimit_nh4(1:nl_soil)      !flag for NH4 limitation
   real(r8) :: residual_sminn_vr( 1:nl_soil)
   real(r8) :: residual_sminn
   real(r8) :: residual_smin_nh4_vr( 1:nl_soil)
   real(r8) :: residual_smin_no3_vr( 1:nl_soil)
   real(r8) :: residual_smin_nh4
   real(r8) :: residual_smin_no3
   real(r8) :: residual_plant_ndemand
   real(r8) :: sminn_to_plant_new
   real(r8) :: actual_immob
   real(r8) :: potential_immob
   integer  :: ivt, ps, pe, m
   !-----------------------------------------------------------------------

      sminn_to_plant_new  =  0._r8

      IF(.not. DEF_USE_NITRIF)THEN

      ! init sminn_tot
         sminn_tot = 0.

         DO j = 1, nl_soil
            sminn_tot = sminn_tot + sminn_vr(j,i) * dz_soi(j)
         ENDDO

         DO j = 1, nl_soil
            IF (sminn_tot  >  0.) THEN
               nuptake_prof(j) = sminn_vr(j,i) / sminn_tot
            ELSE
               nuptake_prof(j) = nfixation_prof(j,i)
            ENDIF
         ENDDO

         DO j = 1, nl_soil
            sum_ndemand_vr(j) = plant_ndemand(i) * nuptake_prof(j) + potential_immob_vr(j,i)
         ENDDO

         DO j = 1, nl_soil
            IF (sum_ndemand_vr(j)*deltim < sminn_vr(j,i)) THEN

               ! N availability is not limiting immobilization or plant
               ! uptake, and both can proceed at their potential rates
               nlimit(j) = 0
               fpi_vr(j,i) = 1.0_r8
               actual_immob_vr(j,i) = potential_immob_vr(j,i)
               sminn_to_plant_vr(j,i) = plant_ndemand(i) * nuptake_prof(j)
            ELSE
               ! N availability can not satisfy the sum of immobilization and
               ! plant growth demands, so these two demands compete for available
               ! soil mineral N resource.

               nlimit(j) = 1
               IF (sum_ndemand_vr(j) > 0.0_r8) THEN
                  actual_immob_vr(j,i) = (sminn_vr(j,i)/deltim)*(potential_immob_vr(j,i) / sum_ndemand_vr(j))
               ELSE
                  actual_immob_vr(j,i) = 0.0_r8
               ENDIF

               IF (potential_immob_vr(j,i) > 0.0_r8) THEN
                  fpi_vr(j,i) = actual_immob_vr(j,i) / potential_immob_vr(j,i)
               ELSE
                  fpi_vr(j,i) = 0.0_r8
               ENDIF

               sminn_to_plant_vr(j,i) = (sminn_vr(j,i)/deltim) - actual_immob_vr(j,i)
            ENDIF

            IF (DEF_USE_NOSTRESSNITROGEN) THEN
               ps = patch_pft_s(i)
               pe = patch_pft_e(i)
               DO m = ps, pe
                  ivt = pftclass(m)
                  IF (ivt >= npcropmin) THEN
                     nlimit(j) = 1
                     fpi_vr(j,i) = 1.0_r8
                     actual_immob_vr(j,i) = potential_immob_vr(j,i)
                     sminn_to_plant_vr(j,i) =  plant_ndemand(i) * nuptake_prof(j)
                     supplement_to_sminn_vr(j,i) = sum_ndemand_vr(j) - (sminn_vr(j,i)/deltim)
                  ENDIF
               ENDDO
            ENDIF
         ENDDO

         ! sum up N fluxes to plant
         DO j = 1, nl_soil
            sminn_to_plant(i) = sminn_to_plant(i) + sminn_to_plant_vr(j,i) * dz_soi(j)
         ENDDO

         ! give plants a second pass to see IF there is any mineral N left over with which to satisfy residual N demand.
         residual_sminn = 0._r8

         ! sum up total N left over after initial plant and immobilization fluxes
         residual_plant_ndemand = plant_ndemand(i) - sminn_to_plant(i)

         DO j = 1, nl_soil
            IF (residual_plant_ndemand  >  0._r8 ) THEN
               IF (nlimit(j) .eq. 0) THEN
                  residual_sminn_vr(j) = max(sminn_vr(j,i) - (actual_immob_vr(j,i) + sminn_to_plant_vr(j,i) ) * deltim, 0._r8)
                  residual_sminn       = residual_sminn + residual_sminn_vr(j) * dz_soi(j)
               ELSE
                  residual_sminn_vr(j) = 0._r8
               ENDIF
            ENDIF
         ENDDO

         ! distribute residual N to plants
         DO j = 1, nl_soil
            IF ( residual_plant_ndemand  >  0._r8 .and. residual_sminn  >  0._r8 .and. nlimit(j) .eq. 0) THEN
               sminn_to_plant_vr(j,i) = sminn_to_plant_vr(j,i) + residual_sminn_vr(j) * &
                    min(( residual_plant_ndemand *  deltim ) / residual_sminn, 1._r8) / deltim
            ENDIF
         ENDDO

         ! re-sum up N fluxes to plant
         sminn_to_plant(i) = 0._r8
         DO j = 1, nl_soil
            sminn_to_plant(i) = sminn_to_plant(i) + sminn_to_plant_vr(j,i) * dz_soi(j)
            sum_ndemand_vr(j) = potential_immob_vr(j,i) + sminn_to_plant_vr(j,i)
         ENDDO

         ! under conditions of excess N, some proportion is assumed to
         ! be lost to denitrification, in addition to the constant
         ! proportion lost in the decomposition pathways
         DO j = 1, nl_soil
            IF ((sminn_to_plant_vr(j,i) + actual_immob_vr(j,i))*deltim < sminn_vr(j,i)) THEN
               sminn_to_denit_excess_vr(j,i) = max(bdnr*deltim/86400._r8*((sminn_vr(j,i)/deltim) - sum_ndemand_vr(j)),0._r8)
            ELSE
               sminn_to_denit_excess_vr(j,i) = 0._r8
            ENDIF
         ENDDO

         ! sum up N fluxes to immobilization
         actual_immob = 0._r8
         potential_immob = 0._r8
         DO j = 1, nl_soil
            actual_immob    = actual_immob    + actual_immob_vr(j,i) * dz_soi(j)
            potential_immob = potential_immob + potential_immob_vr(j,i) * dz_soi(j)
         ENDDO

         ! calculate the fraction of potential growth that can be
         ! achieved with the N available to plants
         IF (plant_ndemand(i) > 0.0_r8) THEN
            fpg(i) = sminn_to_plant(i) / plant_ndemand(i)
         ELSE
            fpg(i) = 1.0_r8
         ENDIF

         ! calculate the fraction of immobilization realized (for diagnostic purposes)
         IF (potential_immob > 0.0_r8) THEN
            fpi(i) = actual_immob / potential_immob
         ELSE
            fpi(i) = 1.0_r8
         ENDIF

      ELSE
         ! init total mineral N pools
         sminn_tot = 0.

         ! sum up total mineral N pools
         DO j = 1, nl_soil
            sminn_tot = sminn_tot + (smin_no3_vr(j,i) + smin_nh4_vr(j,i)) * dz_soi(j)
         ENDDO

         ! define N uptake profile for initial vertical distribution of plant N uptake, assuming plant seeks N from WHERE it is most abundant
         DO j = 1, nl_soil
            IF (sminn_tot  >  0.) THEN
               nuptake_prof(j) = sminn_vr(j,i) / sminn_tot
            ELSE
               nuptake_prof(j) = nfixation_prof(j,i)
            ENDIF
         ENDDO

         ! main column/vertical loop
         DO j = 1, nl_soil
            !  first compete for nh4
            sum_nh4_demand(j) = plant_ndemand(i) * nuptake_prof(j) + potential_immob_vr(j,i) + pot_f_nit_vr(j,i)
            sum_nh4_demand_scaled(j) = plant_ndemand(i)* nuptake_prof(j) * compet_plant_nh4 + &
                         potential_immob_vr(j,i)*compet_decomp_nh4 + pot_f_nit_vr(j,i)*compet_nit

            IF (sum_nh4_demand(j)*deltim < smin_nh4_vr(j,i)) THEN

               ! NH4 availability is not limiting immobilization or plant
               ! uptake, and all can proceed at their potential rates
               nlimit_nh4(j) = 0
               fpi_nh4_vr(j) = 1.0_r8
               actual_immob_nh4_vr(j,i) = potential_immob_vr(j,i)
               !RF added new term.

               f_nit_vr(j,i) = pot_f_nit_vr(j,i)

               smin_nh4_to_plant_vr(j,i) = plant_ndemand(i) * nuptake_prof(j)

            ELSE

               ! NH4 availability can not satisfy the sum of immobilization, nitrification, and
               ! plant growth demands, so these three demands compete for available
               ! soil mineral NH4 resource.
               nlimit_nh4(j) = 1
               IF (sum_nh4_demand(j) > 0.0_r8) THEN
                  ! RF microbes compete based on the hypothesised plant demand.
                  actual_immob_nh4_vr(j,i) = min((smin_nh4_vr(j,i)/deltim)*(potential_immob_vr(j,i)* &
                               compet_decomp_nh4 / sum_nh4_demand_scaled(j)), potential_immob_vr(j,i))

                  f_nit_vr(j,i) =  min((smin_nh4_vr(j,i)/deltim)*(pot_f_nit_vr(j,i)*compet_nit / &
                               sum_nh4_demand_scaled(j)), pot_f_nit_vr(j,i))

                  smin_nh4_to_plant_vr(j,i) = min((smin_nh4_vr(j,i)/deltim)*(plant_ndemand(i)* &
                               nuptake_prof(j)*compet_plant_nh4 / sum_nh4_demand_scaled(j)), plant_ndemand(i)*nuptake_prof(j))

               ELSE
                  actual_immob_nh4_vr(j,i) = 0.0_r8
                  smin_nh4_to_plant_vr(j,i) = 0.0_r8
                  f_nit_vr(j,i) = 0.0_r8
               ENDIF

               IF (potential_immob_vr(j,i) > 0.0_r8) THEN
                  fpi_nh4_vr(j) = actual_immob_nh4_vr(j,i) / potential_immob_vr(j,i)
               ELSE
                  fpi_nh4_vr(j) = 0.0_r8
               ENDIF

            ENDIF
            sum_no3_demand(j) = (plant_ndemand(i)*nuptake_prof(j)-smin_nh4_to_plant_vr(j,i)) &
                              + (potential_immob_vr(j,i)-actual_immob_nh4_vr(j,i)) + pot_f_denit_vr(j,i)
            sum_no3_demand_scaled(j) = (plant_ndemand(i)*nuptake_prof(j) &
                                     - smin_nh4_to_plant_vr(j,i))*compet_plant_no3 &
                                     + (potential_immob_vr(j,i)-actual_immob_nh4_vr(j,i))*compet_decomp_no3 + pot_f_denit_vr(j,i)*compet_denit

            IF (sum_no3_demand(j)*deltim < smin_no3_vr(j,i)) THEN

               ! NO3 availability is not limiting immobilization or plant
               ! uptake, and all can proceed at their potential rates
               nlimit_no3(j) = 0
               fpi_no3_vr(j) = 1.0_r8 -  fpi_nh4_vr(j)
               actual_immob_no3_vr(j,i) = (potential_immob_vr(j,i)-actual_immob_nh4_vr(j,i))

               f_denit_vr(j,i) = pot_f_denit_vr(j,i)

               smin_no3_to_plant_vr(j,i) = (plant_ndemand(i)*nuptake_prof(j)-smin_nh4_to_plant_vr(j,i))
            ELSE

               ! NO3 availability can not satisfy the sum of immobilization, denitrification, and
               ! plant growth demands, so these three demands compete for available
               ! soil mineral NO3 resource.
               nlimit_no3(j) = 1

               IF (sum_no3_demand(j) > 0.0_r8) THEN
                  actual_immob_no3_vr(j,i) = min((smin_no3_vr(j,i)/deltim)*((potential_immob_vr(j,i) &
                                           - actual_immob_nh4_vr(j,i))*compet_decomp_no3 / sum_no3_demand_scaled(j)), &
                                            potential_immob_vr(j,i)-actual_immob_nh4_vr(j,i))

                  smin_no3_to_plant_vr(j,i) = min((smin_no3_vr(j,i)/deltim)*((plant_ndemand(i) &
                                            * nuptake_prof(j)-smin_nh4_to_plant_vr(j,i))*compet_plant_no3 / sum_no3_demand_scaled(j)), &
                                              plant_ndemand(i)*nuptake_prof(j)-smin_nh4_to_plant_vr(j,i))

                  f_denit_vr(j,i) = min((smin_no3_vr(j,i)/deltim)*(pot_f_denit_vr(j,i)*compet_denit / &
                                       sum_no3_demand_scaled(j)), pot_f_denit_vr(j,i))

               ELSE ! no no3 demand. no uptake fluxes.
                  actual_immob_no3_vr(j,i) = 0.0_r8
                  smin_no3_to_plant_vr(j,i) = 0.0_r8
                  f_denit_vr(j,i) = 0.0_r8

               ENDIF !any no3 demand?

               IF (potential_immob_vr(j,i) > 0.0_r8) THEN
                  fpi_no3_vr(j) = actual_immob_no3_vr(j,i) / potential_immob_vr(j,i)
               ELSE
                  fpi_no3_vr(j) = 0.0_r8
               ENDIF

            ENDIF

            ! n2o emissions: n2o from nitr is const fraction, n2o from denitr is calculated in nitrif_denitrif
            f_n2o_nit_vr(j,i) = f_nit_vr(j,i) * nitrif_n2o_loss_frac
            f_n2o_denit_vr(j,i) = f_denit_vr(j,i) / (1._r8 + n2_n2o_ratio_denit_vr(j,i))


            ! this code block controls the addition of N to sminn pool
            ! to eliminate any N limitation, when Carbon_Only is set.  This lets the
            ! model behave essentially as a carbon-only model, but with the
            ! benefit of keeping track of the N additions needed to
            ! eliminate N limitations, so there is still a diagnostic quantity
            ! that describes the degree of N limitation at steady-state.
            IF (DEF_USE_NOSTRESSNITROGEN) THEN
               ps = patch_pft_s(i)
               pe = patch_pft_e(i)
               DO m = ps, pe
                  ivt = pftclass(m)
                  IF (ivt >= npcropmin) THEN
                     IF (fpi_no3_vr(j) + fpi_nh4_vr(j) < 1._r8) THEN
                        fpi_nh4_vr(j) = 1.0_r8 - fpi_no3_vr(j)
                        supplement_to_sminn_vr(j,i) = (potential_immob_vr(j,i) &
                                                      - actual_immob_no3_vr(j,i)) - actual_immob_nh4_vr(j,i)
                        ! update to new values that satisfy demand
                        actual_immob_nh4_vr(j,i) = potential_immob_vr(j,i) -  actual_immob_no3_vr(j,i)
                     ENDIF
                     IF (smin_no3_to_plant_vr(j,i) + smin_nh4_to_plant_vr(j,i) < plant_ndemand(i)*nuptake_prof(j)) THEN
                        supplement_to_sminn_vr(j,i) = supplement_to_sminn_vr(j,i) + &
                              (plant_ndemand(i)*nuptake_prof(j) - smin_no3_to_plant_vr(j,i)) - smin_nh4_to_plant_vr(j,i)  ! use old values
                        smin_nh4_to_plant_vr(j,i) = plant_ndemand(i)*nuptake_prof(j) - smin_no3_to_plant_vr(j,i)
                     ENDIF
                     sminn_to_plant_vr(j,i) = smin_no3_to_plant_vr(j,i) + smin_nh4_to_plant_vr(j,i)
                  ENDIF
               ENDDO
            ENDIF
            ! sum up no3 and nh4 fluxes
            fpi_vr(j,i) = fpi_no3_vr(j) + fpi_nh4_vr(j)
            sminn_to_plant_vr(j,i) = smin_no3_to_plant_vr(j,i) + smin_nh4_to_plant_vr(j,i)
            actual_immob_vr(j,i) = actual_immob_no3_vr(j,i) + actual_immob_nh4_vr(j,i)
         ENDDO

         ! sum up N fluxes to plant after initial competition
         sminn_to_plant(i) = 0._r8
         DO j = 1, nl_soil
            sminn_to_plant(i) = sminn_to_plant(i) + sminn_to_plant_vr(j,i) * dz_soi(j)
         ENDDO
         ! give plants a second pass to see IF there is any mineral N left over with which to satisfy residual N demand.
         ! first take frm nh4 pool; THEN take from no3 pool
         residual_plant_ndemand = plant_ndemand(i) - sminn_to_plant(i)
         residual_smin_nh4 = 0._r8
         DO j = 1, nl_soil
            IF (residual_plant_ndemand  >  0._r8 ) THEN
               IF (nlimit_nh4(j) .eq. 0) THEN
                  residual_smin_nh4_vr(j) = max(smin_nh4_vr(j,i) - (actual_immob_nh4_vr(j,i) + &
                                              smin_nh4_to_plant_vr(j,i) + f_nit_vr(j,i) ) * deltim, 0._r8)

                  residual_smin_nh4 = residual_smin_nh4 + residual_smin_nh4_vr(j) * dz_soi(j)
               ELSE
                  residual_smin_nh4_vr(j)  = 0._r8
               ENDIF
            ENDIF
         ENDDO

         DO j = 1, nl_soil
            IF (residual_plant_ndemand  >  0._r8 ) THEN
               IF ( residual_smin_nh4 > 0._r8 .and. nlimit_nh4(j) .eq. 0 ) THEN
                  smin_nh4_to_plant_vr(j,i) = smin_nh4_to_plant_vr(j,i) + residual_smin_nh4_vr(j) * &
                       min(( residual_plant_ndemand *  deltim ) / residual_smin_nh4, 1._r8) / deltim
               ENDIF
            ENDIF
         ENDDO

         ! re-sum up N fluxes to plant after second pass for nh4
         sminn_to_plant(i) = 0._r8
         DO j = 1, nl_soil
            sminn_to_plant_vr(j,i) = smin_nh4_to_plant_vr(j,i) + smin_no3_to_plant_vr(j,i)
            sminn_to_plant(i) = sminn_to_plant(i) + (sminn_to_plant_vr(j,i)) * dz_soi(j)
         ENDDO

         !
         ! and now DO second pass for no3
         residual_plant_ndemand = plant_ndemand(i) - sminn_to_plant(i)
         residual_smin_no3 = 0._r8

         DO j = 1, nl_soil
            IF (residual_plant_ndemand > 0._r8 ) THEN
               IF (nlimit_no3(j) .eq. 0) THEN
                  residual_smin_no3_vr(j) = max(smin_no3_vr(j,i) - (actual_immob_no3_vr(j,i) + &
                                             smin_no3_to_plant_vr(j,i) + f_denit_vr(j,i) ) * deltim, 0._r8)
                  residual_smin_no3 = residual_smin_no3 + residual_smin_no3_vr(j) * dz_soi(j)
               ELSE
                  residual_smin_no3_vr(j)  = 0._r8
               ENDIF
            ENDIF
         ENDDO

         DO j = 1, nl_soil
            IF (residual_plant_ndemand > 0._r8 ) THEN
               IF ( residual_smin_no3 > 0._r8 .and. nlimit_no3(j) .eq. 0) THEN
                  smin_no3_to_plant_vr(j,i) = smin_no3_to_plant_vr(j,i) + residual_smin_no3_vr(j) * &
                    min(( residual_plant_ndemand *  deltim ) / residual_smin_no3, 1._r8) / deltim
               ENDIF
            ENDIF
         ENDDO

         ! re-sum up N fluxes to plant after second passes of both no3 and nh4
         sminn_to_plant(i) = 0._r8
         DO j = 1, nl_soil
            sminn_to_plant_vr(j,i) = smin_nh4_to_plant_vr(j,i) + smin_no3_to_plant_vr(j,i)
            sminn_to_plant(i) = sminn_to_plant(i) + (sminn_to_plant_vr(j,i)) * dz_soi(j)
         ENDDO

         ! sum up N fluxes to immobilization
         actual_immob = 0._r8
         potential_immob = 0._r8
         DO j = 1, nl_soil
            actual_immob = actual_immob + actual_immob_vr(j,i) * dz_soi(j)
            potential_immob = potential_immob + potential_immob_vr(j,i) * dz_soi(j)
         ENDDO

         ! calculate the fraction of potential growth that can be
         ! achieved with the N available to plants
         ! calculate the fraction of immobilization realized (for diagnostic purposes)

         IF (plant_ndemand(i) > 0.0_r8) THEN
            fpg(i) = sminn_to_plant(i) / plant_ndemand(i)
         ELSE
            fpg(i) = 1._r8
         ENDIF

         IF (potential_immob > 0.0_r8) THEN
            fpi(i) = actual_immob / potential_immob
         ELSE
            fpi(i) = 1._r8
         ENDIF
      ENDIF

   END SUBROUTINE SoilBiogeochemCompetition

END MODULE MOD_BGC_Soil_BiogeochemCompetition
#endif
