#include <define.h>
#ifdef BGC

MODULE MOD_BGC_Soil_BiogeochemNStateUpdate1

!---------------------------------------------------------------------------------------------
! !DESCRIPTION:
! Updates soil mineral nitrogen pool sizes. The dynamics of soil mineral nitrogen pool is
! simulated according to fertilisation, nitrogen deposition, biological fixation, plant uptake,
! mineralisation and immobilisation in this module. IF nitrification is activated, nitrate nitrogen
! has a separated pool against ammonium nitrogen pool. Accumulated nitrogen transfer
! network is also recorded for semi-analytic spinup.
!
! !ORIGINAL:
! The Community Land Model version 5.0 (CLM5.0)
!
! !REVISION:
! Xingjie Lu, 2022, 1) modify original CLM5 to be compatible with CoLM code structure.
!                   2) Record accumulated nitrogen transfer network for semi-analytic spinup

   USE MOD_Precision
   USE MOD_Namelist, only: DEF_USE_SASU, DEF_USE_DiagMatrix, DEF_USE_NITRIF, DEF_USE_CNSOYFIXN
   USE MOD_BGC_Vars_TimeInvariants, only: &
     ! bgc constants
       i_met_lit, i_cel_lit, i_lig_lit, i_cwd, i_soil1, i_soil2, i_soil3
   USE MOD_BGC_Vars_TimeInvariants, only: &
       receiver_pool, donor_pool, nitrif_n2o_loss_frac

   USE MOD_BGC_Vars_TimeVariables, only: &
       ! Mineral nitrogen pools (inout)
       sminn_vr                   , smin_nh4_vr                , smin_no3_vr              , &
       ndep_prof                  , nfixation_prof             , &
       AKX_met_to_soil1_n_vr_acc  , AKX_cel_to_soil1_n_vr_acc  , AKX_lig_to_soil2_n_vr_acc  , AKX_soil1_to_soil2_n_vr_acc, &
       AKX_cwd_to_cel_n_vr_acc    , AKX_cwd_to_lig_n_vr_acc    , AKX_soil1_to_soil3_n_vr_acc, AKX_soil2_to_soil1_n_vr_acc, &
       AKX_soil2_to_soil3_n_vr_acc, AKX_soil3_to_soil1_n_vr_acc, &
       AKX_met_exit_n_vr_acc      , AKX_cel_exit_n_vr_acc      , AKX_lig_exit_n_vr_acc      , AKX_cwd_exit_n_vr_acc      , &
       AKX_soil1_exit_n_vr_acc    , AKX_soil2_exit_n_vr_acc    , AKX_soil3_exit_n_vr_acc

   USE MOD_BGC_Vars_1DFluxes, only: &
       ! Decomposition fluxes variables (inout)
              decomp_npools_sourcesink, decomp_ntransfer_vr      , decomp_sminn_flux_vr     , sminn_to_denit_decomp_vr, &
              gross_nmin_vr           , actual_immob_nh4_vr      , actual_immob_no3_vr      , &
              sminn_to_plant_vr       , smin_nh4_to_plant_vr     , smin_no3_to_plant_vr     , supplement_to_sminn_vr, &
              sminn_to_plant_fun_vr   , sminn_to_plant_fun_nh4_vr, sminn_to_plant_fun_no3_vr, &
              sminn_to_denit_excess_vr, f_nit_vr                 , f_denit_vr               , soyfixn_to_sminn, &
              ndep_to_sminn           , ffix_to_sminn            , nfix_to_sminn            , fert_to_sminn
   USE MOD_MPAS_MPI

   IMPLICIT NONE

   PUBLIC SoilBiogeochemNStateUpdate1

CONTAINS

   SUBROUTINE SoilBiogeochemNStateUpdate1(i,deltim,nl_soil,ndecomp_transitions,dz_soi)

   integer ,intent(in) :: i                  ! patch idnex
   real(r8),intent(in) :: deltim             ! time step in seconds
   integer ,intent(in) :: nl_soil            ! number of total soil layers
   integer ,intent(in) :: ndecomp_transitions! number of total transfers between soil and litter pools in the decomposition
   real(r8),intent(in) :: dz_soi(1:nl_soil)  ! thicknesses of each soil layer (m)

   integer j,k
   real(r8):: sminflux,minerflux

      IF(.not. DEF_USE_NITRIF)THEN
         DO j = 1, nl_soil
            ! N deposition and fixation
            sminn_vr(j,i) = sminn_vr(j,i) + ndep_to_sminn(i)*deltim * ndep_prof(j,i)
            sminn_vr(j,i) = sminn_vr(j,i) + nfix_to_sminn(i)*deltim * nfixation_prof(j,i)
         ENDDO
      ELSE
         DO j = 1, nl_soil
            ! N deposition and fixation (put all into NH4 pool)
            smin_nh4_vr(j,i) = smin_nh4_vr(j,i) + ndep_to_sminn(i)*deltim * ndep_prof(j,i)
            smin_nh4_vr(j,i) = smin_nh4_vr(j,i) + nfix_to_sminn(i)*deltim * nfixation_prof(j,i)
         ENDDO
      ENDIF

      ! repeating N dep and fixation for crops
#ifdef CROP
      IF(.not. DEF_USE_NITRIF)THEN
         DO j = 1, nl_soil
            ! column loop
            ! N deposition and fixation
            sminn_vr(j,i) = sminn_vr(j,i) &
                          + fert_to_sminn(i) * deltim * ndep_prof(j,i)
         ENDDO
         IF(DEF_USE_CNSOYFIXN)THEN
            DO j = 1, nl_soil
               sminn_vr(j,i) = sminn_vr(j,i) &
                             + soyfixn_to_sminn(i) * deltim * nfixation_prof(j,i)
            ENDDO
         ENDIF
      ELSE
         DO j = 1, nl_soil
            ! N deposition and fixation (put all into NH4 pool)
            smin_nh4_vr(j,i) = smin_nh4_vr(j,i) &
                             + fert_to_sminn(i) * deltim * ndep_prof(j,i)
         ENDDO
         IF(DEF_USE_CNSOYFIXN)THEN
            DO j = 1, nl_soil
               smin_nh4_vr(j,i) = smin_nh4_vr(j,i) &
                                + soyfixn_to_sminn(i) * deltim * nfixation_prof(j,i)
            ENDDO
         ENDIF
      ENDIF
#endif

    ! decomposition fluxes
      DO k = 1, ndecomp_transitions
         DO j = 1, nl_soil
            decomp_npools_sourcesink(j,donor_pool(k),i) = &
                      decomp_npools_sourcesink(j,donor_pool(k),i) - &
                      decomp_ntransfer_vr(j,k,i) * deltim
         ENDDO
      ENDDO


      DO k = 1, ndecomp_transitions
         IF ( receiver_pool(k) /= 0 ) THEN  ! skip terminal transitions
            DO j = 1, nl_soil
               decomp_npools_sourcesink(j,receiver_pool(k),i) = &
                      decomp_npools_sourcesink(j,receiver_pool(k),i) + &
                         (decomp_ntransfer_vr(j,k,i) + &
                          decomp_sminn_flux_vr(j,k,i)) * deltim
            ENDDO
         ELSE  ! terminal transitions
            DO j = 1, nl_soil
               decomp_npools_sourcesink(j,donor_pool(k),i) = &
                      decomp_npools_sourcesink(j,donor_pool(k),i) - &
                         decomp_sminn_flux_vr(j,k,i) * deltim
            ENDDO
         ENDIF
      ENDDO

      IF(DEF_USE_SASU .or. DEF_USE_DiagMatrix)THEN
         DO j = 1, nl_soil
            AKX_met_to_soil1_n_vr_acc  (j,i) = AKX_met_to_soil1_n_vr_acc  (j,i) + (decomp_ntransfer_vr(j, 1,i) + decomp_sminn_flux_vr(j, 1,i)) * deltim
            AKX_cel_to_soil1_n_vr_acc  (j,i) = AKX_cel_to_soil1_n_vr_acc  (j,i) + (decomp_ntransfer_vr(j, 2,i) + decomp_sminn_flux_vr(j, 2,i)) * deltim
            AKX_lig_to_soil2_n_vr_acc  (j,i) = AKX_lig_to_soil2_n_vr_acc  (j,i) + (decomp_ntransfer_vr(j, 3,i) + decomp_sminn_flux_vr(j, 3,i)) * deltim
            AKX_soil1_to_soil2_n_vr_acc(j,i) = AKX_soil1_to_soil2_n_vr_acc(j,i) + (decomp_ntransfer_vr(j, 4,i) + decomp_sminn_flux_vr(j, 4,i)) * deltim
            AKX_cwd_to_cel_n_vr_acc    (j,i) = AKX_cwd_to_cel_n_vr_acc    (j,i) + (decomp_ntransfer_vr(j, 5,i) + decomp_sminn_flux_vr(j, 5,i)) * deltim
            AKX_cwd_to_lig_n_vr_acc    (j,i) = AKX_cwd_to_lig_n_vr_acc    (j,i) + (decomp_ntransfer_vr(j, 6,i) + decomp_sminn_flux_vr(j, 6,i)) * deltim
            AKX_soil1_to_soil3_n_vr_acc(j,i) = AKX_soil1_to_soil3_n_vr_acc(j,i) + (decomp_ntransfer_vr(j, 7,i) + decomp_sminn_flux_vr(j, 7,i)) * deltim
            AKX_soil2_to_soil1_n_vr_acc(j,i) = AKX_soil2_to_soil1_n_vr_acc(j,i) + (decomp_ntransfer_vr(j, 8,i) + decomp_sminn_flux_vr(j, 8,i)) * deltim
            AKX_soil2_to_soil3_n_vr_acc(j,i) = AKX_soil2_to_soil3_n_vr_acc(j,i) + (decomp_ntransfer_vr(j, 9,i) + decomp_sminn_flux_vr(j, 9,i)) * deltim
            AKX_soil3_to_soil1_n_vr_acc(j,i) = AKX_soil3_to_soil1_n_vr_acc(j,i) + (decomp_ntransfer_vr(j,10,i) + decomp_sminn_flux_vr(j,10,i)) * deltim

            AKX_met_exit_n_vr_acc      (j,i) = AKX_met_exit_n_vr_acc      (j,i) + decomp_ntransfer_vr(j, 1,i) * deltim
            AKX_cel_exit_n_vr_acc      (j,i) = AKX_cel_exit_n_vr_acc      (j,i) + decomp_ntransfer_vr(j, 2,i) * deltim
            AKX_lig_exit_n_vr_acc      (j,i) = AKX_lig_exit_n_vr_acc      (j,i) + decomp_ntransfer_vr(j, 3,i) * deltim
            AKX_soil1_exit_n_vr_acc    (j,i) = AKX_soil1_exit_n_vr_acc    (j,i) + decomp_ntransfer_vr(j, 4,i) * deltim
            AKX_cwd_exit_n_vr_acc      (j,i) = AKX_cwd_exit_n_vr_acc      (j,i) + decomp_ntransfer_vr(j, 5,i) * deltim
            AKX_cwd_exit_n_vr_acc      (j,i) = AKX_cwd_exit_n_vr_acc      (j,i) + decomp_ntransfer_vr(j, 6,i) * deltim
            AKX_soil1_exit_n_vr_acc    (j,i) = AKX_soil1_exit_n_vr_acc    (j,i) + decomp_ntransfer_vr(j, 7,i) * deltim
            AKX_soil2_exit_n_vr_acc    (j,i) = AKX_soil2_exit_n_vr_acc    (j,i) + decomp_ntransfer_vr(j, 8,i) * deltim
            AKX_soil2_exit_n_vr_acc    (j,i) = AKX_soil2_exit_n_vr_acc    (j,i) + decomp_ntransfer_vr(j, 9,i) * deltim
            AKX_soil3_exit_n_vr_acc    (j,i) = AKX_soil3_exit_n_vr_acc    (j,i) + decomp_ntransfer_vr(j,10,i) * deltim
         ENDDO
      ENDIF

      IF(.not. DEF_USE_NITRIF)THEN

           !--------------------------------------------------------
           !-------------    NITRIF_DENITRIF OFF -------------------
           !--------------------------------------------------------

           ! immobilization/mineralization in litter-to-SOM and SOM-to-SOM fluxes and denitrification fluxes
         DO k = 1, ndecomp_transitions
            IF ( receiver_pool(k) /= 0 ) THEN  ! skip terminal transitions
               DO j = 1, nl_soil
                  sminn_vr(j,i)  = sminn_vr(j,i) - &
                                  (sminn_to_denit_decomp_vr(j,k,i) + &
                                  decomp_sminn_flux_vr(j,k,i))* deltim
               ENDDO
            ELSE
               DO j = 1, nl_soil
                  sminn_vr(j,i)  = sminn_vr(j,i) - &
                                  sminn_to_denit_decomp_vr(j,k,i)* deltim

                  sminn_vr(j,i)  = sminn_vr(j,i) + &
                                  decomp_sminn_flux_vr(j,k,i)* deltim

               ENDDO
            ENDIF
         ENDDO


         DO j = 1, nl_soil
            ! "bulk denitrification"
            sminn_vr(j,i) = sminn_vr(j,i) - sminn_to_denit_excess_vr(j,i) * deltim

            ! total plant uptake from mineral N
            sminn_vr(j,i) = sminn_vr(j,i) - sminn_to_plant_vr(j,i)*deltim
            ! flux that prevents N limitation (when Carbon_only is set)
            sminn_vr(j,i) = sminn_vr(j,i) + supplement_to_sminn_vr(j,i)*deltim
         ENDDO

      ELSE

           !--------------------------------------------------------
           !-------------    NITRIF_DENITRIF ON --------------------
           !--------------------------------------------------------

         DO j = 1, nl_soil

            ! mineralization fluxes (divert a fraction of this stream to nitrification flux, add the rest to NH4 pool)
            smin_nh4_vr(j,i) = smin_nh4_vr(j,i) + gross_nmin_vr(j,i)*deltim

            ! immobilization fluxes
            smin_nh4_vr(j,i) = smin_nh4_vr(j,i) - actual_immob_nh4_vr(j,i)*deltim

            smin_no3_vr(j,i) = smin_no3_vr(j,i) - actual_immob_no3_vr(j,i)*deltim

            ! plant uptake fluxes
            smin_nh4_vr(j,i) = smin_nh4_vr(j,i) - smin_nh4_to_plant_vr(j,i)*deltim

            smin_no3_vr(j,i) = smin_no3_vr(j,i) - smin_no3_to_plant_vr(j,i)*deltim


            ! Account for nitrification fluxes
            smin_nh4_vr(j,i) = smin_nh4_vr(j,i) - f_nit_vr(j,i) * deltim

            smin_no3_vr(j,i) = smin_no3_vr(j,i) + f_nit_vr(j,i) * deltim &
                             * (1._r8 - nitrif_n2o_loss_frac)

            ! Account for denitrification fluxes
            smin_no3_vr(j,i) = smin_no3_vr(j,i) - f_denit_vr(j,i) * deltim

            ! flux that prevents N limitation (when Carbon_only is set; put all into NH4)
            smin_nh4_vr(j,i) = smin_nh4_vr(j,i) + supplement_to_sminn_vr(j,i)*deltim

            ! update diagnostic total
            sminn_vr(j,i) = smin_nh4_vr(j,i) + smin_no3_vr(j,i)

         ENDDO

      ENDIF

   END SUBROUTINE SoilBiogeochemNStateUpdate1

END MODULE MOD_BGC_Soil_BiogeochemNStateUpdate1
#endif
