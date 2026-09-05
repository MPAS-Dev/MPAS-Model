#include <define.h>
#ifdef BGC

MODULE MOD_BGC_CNZeroFluxes

!----------------------------------------------------------------------------------------------------
! !DESCRIPTION:
! This MODULE reset flux variable to 0 at begining of each time step to avoid miscalculating from last step.
!
! !ORIGINAL:
! The Community Land Model version 5.0 (CLM5.0)
!
! !REVISION:
! Xingjie Lu, 2022, modify original CLM5 to be compatible with CoLM code structure.

   USE MOD_Precision
   USE MOD_Namelist, only: DEF_USE_NITRIF

   USE MOD_BGC_Vars_1DPFTFluxes, only:        &
       m_leafc_to_litter_p                  , &
       m_frootc_to_litter_p                 , &
       m_leafc_storage_to_litter_p          , &
       m_frootc_storage_to_litter_p         , &
       m_livestemc_storage_to_litter_p      , &
       m_deadstemc_storage_to_litter_p      , &
       m_livecrootc_storage_to_litter_p     , &
       m_deadcrootc_storage_to_litter_p     , &
       m_leafc_xfer_to_litter_p             , &
       m_frootc_xfer_to_litter_p            , &
       m_livestemc_xfer_to_litter_p         , &
       m_deadstemc_xfer_to_litter_p         , &
       m_livecrootc_xfer_to_litter_p        , &
       m_deadcrootc_xfer_to_litter_p        , &
       m_livestemc_to_litter_p              , &
       m_deadstemc_to_litter_p              , &
       m_livecrootc_to_litter_p             , &
       m_deadcrootc_to_litter_p             , &
       m_gresp_storage_to_litter_p          , &
       m_gresp_xfer_to_litter_p             , &
       m_leafc_to_fire_p                    , &
       m_leafc_storage_to_fire_p            , &
       m_leafc_xfer_to_fire_p               , &
       m_livestemc_to_fire_p                , &
       m_livestemc_storage_to_fire_p        , &
       m_livestemc_xfer_to_fire_p           , &
       m_deadstemc_to_fire_p                , &
       m_deadstemc_storage_to_fire_p        , &
       m_deadstemc_xfer_to_fire_p           , &
       m_frootc_to_fire_p                   , &
       m_frootc_storage_to_fire_p           , &
       m_frootc_xfer_to_fire_p              , &
       m_livecrootc_to_fire_p               , &
       m_livecrootc_storage_to_fire_p       , &
       m_livecrootc_xfer_to_fire_p          , &
       m_deadcrootc_to_fire_p               , &
       m_deadcrootc_storage_to_fire_p       , &
       m_deadcrootc_xfer_to_fire_p          , &
       m_gresp_storage_to_fire_p            , &
       m_gresp_xfer_to_fire_p               , &

       m_leafc_to_litter_fire_p             , &
       m_leafc_storage_to_litter_fire_p     , &
       m_leafc_xfer_to_litter_fire_p        , &
       m_livestemc_to_litter_fire_p         , &
       m_livestemc_storage_to_litter_fire_p , &
       m_livestemc_xfer_to_litter_fire_p    , &
       m_livestemc_to_deadstemc_fire_p      , &
       m_deadstemc_to_litter_fire_p         , &
       m_deadstemc_storage_to_litter_fire_p , &
       m_deadstemc_xfer_to_litter_fire_p    , &
       m_frootc_to_litter_fire_p            , &
       m_frootc_storage_to_litter_fire_p    , &
       m_frootc_xfer_to_litter_fire_p       , &
       m_livecrootc_to_litter_fire_p        , &
       m_livecrootc_storage_to_litter_fire_p, &
       m_livecrootc_xfer_to_litter_fire_p   , &
       m_livecrootc_to_deadcrootc_fire_p    , &
       m_deadcrootc_to_litter_fire_p        , &
       m_deadcrootc_storage_to_litter_fire_p, &
       m_deadcrootc_xfer_to_litter_fire_p   , &
       m_gresp_storage_to_litter_fire_p     , &
       m_gresp_xfer_to_litter_fire_p        , &

       leafc_xfer_to_leafc_p                , &
       frootc_xfer_to_frootc_p              , &
       livestemc_xfer_to_livestemc_p        , &
       deadstemc_xfer_to_deadstemc_p        , &
       livecrootc_xfer_to_livecrootc_p      , &
       deadcrootc_xfer_to_deadcrootc_p      , &
       leafc_to_litter_p                    , &
       frootc_to_litter_p                   , &

       leaf_mr_p                            , &
       froot_mr_p                           , &
       livestem_mr_p                        , &
       livecroot_mr_p                       , &
       grain_mr_p                           , &
       leaf_curmr_p                         , &
       froot_curmr_p                        , &
       livestem_curmr_p                     , &
       livecroot_curmr_p                    , &
       grain_curmr_p                        , &
       leaf_xsmr_p                          , &
       froot_xsmr_p                         , &
       livestem_xsmr_p                      , &
       livecroot_xsmr_p                     , &
       grain_xsmr_p                         , &
       psn_to_cpool_p                       , &
       cpool_to_xsmrpool_p                  , &
       cpool_to_leafc_p                     , &
       cpool_to_leafc_storage_p             , &
       cpool_to_frootc_p                    , &
       cpool_to_frootc_storage_p            , &
       cpool_to_livestemc_p                 , &
       cpool_to_livestemc_storage_p         , &
       cpool_to_deadstemc_p                 , &
       cpool_to_deadstemc_storage_p         , &
       cpool_to_livecrootc_p                , &
       cpool_to_livecrootc_storage_p        , &
       cpool_to_deadcrootc_p                , &
       cpool_to_deadcrootc_storage_p        , &
       cpool_to_gresp_storage_p             , &
       cpool_leaf_gr_p                      , &
       cpool_leaf_storage_gr_p              , &
       transfer_leaf_gr_p                   , &
       cpool_froot_gr_p                     , &
       cpool_froot_storage_gr_p             , &
       transfer_froot_gr_p                  , &
       cpool_livestem_gr_p                  , &
       cpool_livestem_storage_gr_p          , &
       transfer_livestem_gr_p               , &
       cpool_deadstem_gr_p                  , &
       cpool_deadstem_storage_gr_p          , &
       transfer_deadstem_gr_p               , &
       cpool_livecroot_gr_p                 , &
       cpool_livecroot_storage_gr_p         , &
       transfer_livecroot_gr_p              , &
       cpool_deadcroot_gr_p                 , &
       cpool_deadcroot_storage_gr_p         , &
       transfer_deadcroot_gr_p              , &
       leafc_storage_to_xfer_p              , &
       frootc_storage_to_xfer_p             , &
       livestemc_storage_to_xfer_p          , &
       deadstemc_storage_to_xfer_p          , &
       livecrootc_storage_to_xfer_p         , &
       deadcrootc_storage_to_xfer_p         , &
       gresp_storage_to_xfer_p              , &
       livestemc_to_deadstemc_p             , &
       livecrootc_to_deadcrootc_p           , &
       crop_seedc_to_leaf_p                 , &

       hrv_xsmrpool_to_atm_p                , &

       xsmrpool_to_atm_p                    , &
       livestemc_to_litter_p                , &
       grainc_to_food_p                     , &
       grainc_to_seed_p                     , &
       grainc_xfer_to_grainc_p              , &
       cpool_to_grainc_p                    , &
       cpool_to_grainc_storage_p            , &
       cpool_grain_gr_p                     , &
       cpool_grain_storage_gr_p             , &
       transfer_grain_gr_p                  , &
       grainc_storage_to_xfer_p             , &

       gpp_p, &

       m_leafn_to_litter_p                  , &
       m_frootn_to_litter_p                 , &
       m_leafn_storage_to_litter_p          , &
       m_frootn_storage_to_litter_p         , &
       m_livestemn_storage_to_litter_p      , &
       m_deadstemn_storage_to_litter_p      , &
       m_livecrootn_storage_to_litter_p     , &
       m_deadcrootn_storage_to_litter_p     , &
       m_leafn_xfer_to_litter_p             , &
       m_frootn_xfer_to_litter_p            , &
       m_livestemn_xfer_to_litter_p         , &
       m_deadstemn_xfer_to_litter_p         , &
       m_livecrootn_xfer_to_litter_p        , &
       m_deadcrootn_xfer_to_litter_p        , &
       m_livestemn_to_litter_p              , &
       m_deadstemn_to_litter_p              , &
       m_livecrootn_to_litter_p             , &
       m_deadcrootn_to_litter_p             , &
       m_retransn_to_litter_p               , &

       m_leafn_to_fire_p                    , &
       m_leafn_storage_to_fire_p            , &
       m_leafn_xfer_to_fire_p               , &
       m_livestemn_to_fire_p                , &
       m_livestemn_storage_to_fire_p        , &
       m_livestemn_xfer_to_fire_p           , &
       m_deadstemn_to_fire_p                , &
       m_deadstemn_storage_to_fire_p        , &
       m_deadstemn_xfer_to_fire_p           , &
       m_frootn_to_fire_p                   , &
       m_frootn_storage_to_fire_p           , &
       m_frootn_xfer_to_fire_p              , &
       m_livecrootn_to_fire_p               , &
       m_livecrootn_storage_to_fire_p       , &
       m_livecrootn_xfer_to_fire_p          , &
       m_deadcrootn_to_fire_p               , &
       m_deadcrootn_storage_to_fire_p       , &
       m_deadcrootn_xfer_to_fire_p          , &
       m_retransn_to_fire_p                 , &

       m_leafn_to_litter_fire_p             , &
       m_leafn_storage_to_litter_fire_p     , &
       m_leafn_xfer_to_litter_fire_p        , &
       m_livestemn_to_litter_fire_p         , &
       m_livestemn_storage_to_litter_fire_p , &
       m_livestemn_xfer_to_litter_fire_p    , &
       m_livestemn_to_deadstemn_fire_p      , &
       m_deadstemn_to_litter_fire_p         , &
       m_deadstemn_storage_to_litter_fire_p , &
       m_deadstemn_xfer_to_litter_fire_p    , &
       m_frootn_to_litter_fire_p            , &
       m_frootn_storage_to_litter_fire_p    , &
       m_frootn_xfer_to_litter_fire_p       , &
       m_livecrootn_to_litter_fire_p        , &
       m_livecrootn_storage_to_litter_fire_p, &
       m_livecrootn_xfer_to_litter_fire_p   , &
       m_livecrootn_to_deadcrootn_fire_p    , &
       m_deadcrootn_to_litter_fire_p        , &
       m_deadcrootn_storage_to_litter_fire_p, &
       m_deadcrootn_xfer_to_litter_fire_p   , &
       m_retransn_to_litter_fire_p          , &

       leafn_xfer_to_leafn_p                , &
       frootn_xfer_to_frootn_p              , &
       livestemn_xfer_to_livestemn_p        , &
       deadstemn_xfer_to_deadstemn_p        , &
       livecrootn_xfer_to_livecrootn_p      , &
       deadcrootn_xfer_to_deadcrootn_p      , &
       leafn_to_litter_p                    , &
       leafn_to_retransn_p                  , &
       frootn_to_litter_p                   , &
       retransn_to_npool_p                  , &
       free_retransn_to_npool_p             , &
       sminn_to_npool_p                     , &
       npool_to_leafn_p                     , &
       npool_to_leafn_storage_p             , &
       npool_to_frootn_p                    , &
       npool_to_frootn_storage_p            , &
       npool_to_livestemn_p                 , &
       npool_to_livestemn_storage_p         , &
       npool_to_deadstemn_p                 , &
       npool_to_deadstemn_storage_p         , &
       npool_to_livecrootn_p                , &
       npool_to_livecrootn_storage_p        , &
       npool_to_deadcrootn_p                , &
       npool_to_deadcrootn_storage_p        , &
       leafn_storage_to_xfer_p              , &
       frootn_storage_to_xfer_p             , &
       livestemn_storage_to_xfer_p          , &
       deadstemn_storage_to_xfer_p          , &
       livecrootn_storage_to_xfer_p         , &
       deadcrootn_storage_to_xfer_p         , &
       livestemn_to_deadstemn_p             , &
       livestemn_to_retransn_p              , &
       livecrootn_to_deadcrootn_p           , &
       livecrootn_to_retransn_p             , &

       crop_seedn_to_leaf_p                 , &

       livestemn_to_litter_p                , &
       grainn_to_food_p                     , &
       grainn_to_seed_p                     , &
       grainn_xfer_to_grainn_p              , &
       npool_to_grainn_p                    , &
       npool_to_grainn_storage_p            , &
       grainn_storage_to_xfer_p             , &
       frootn_to_retransn_p                 , &

       fire_closs_p                         , &
       fire_nloss_p                         , &
       wood_harvestc_p                      , &
       wood_harvestn_p                      , &
       grainc_to_cropprodc_p                , &
       grainn_to_cropprodn_p                , &
       soyfixn_p

   USE MOD_BGC_Vars_1DFluxes, only:           &

       ! phenologgy
       phenology_to_met_c                   , &
       phenology_to_cel_c                   , &
       phenology_to_lig_c                   , &

       ! gap mortality
       gap_mortality_to_met_c               , &
       gap_mortality_to_cel_c               , &
       gap_mortality_to_lig_c               , &
       gap_mortality_to_cwdc                , &

       ! fire
       fire_mortality_to_cwdc               , &
       fire_mortality_to_met_c              , &
       fire_mortality_to_cel_c              , &
       fire_mortality_to_lig_c              , &

       m_decomp_cpools_to_fire_vr           , &

       ! phenologgy
       phenology_to_met_n                   , &
       phenology_to_cel_n                   , &
       phenology_to_lig_n                   , &

       ! gap mortality
       gap_mortality_to_met_n               , &
       gap_mortality_to_cel_n               , &
       gap_mortality_to_lig_n               , &
       gap_mortality_to_cwdn                , &

       ! fire
       fire_mortality_to_cwdn               , &
       fire_mortality_to_met_n              , &
       fire_mortality_to_cel_n              , &
       fire_mortality_to_lig_n              , &

       m_decomp_npools_to_fire_vr           , &

       fire_closs                           , &
       fire_nloss                           , &
       wood_harvestc                        , &
       wood_harvestn                        , &
       grainc_to_cropprodc                  , &
       grainn_to_cropprodn                  , &

       decomp_hr                            , &
       decomp_hr_vr                         , &
       decomp_ctransfer_vr                  , &

       decomp_cpools_transport_tendency     , &
       decomp_cpools_sourcesink             , &

       somc_fire                            , &
       som_c_leached                        , &

       sminn_to_denit_excess_vr             , &
       sminn_leached_vr                     , &
       sminn_to_plant_fun_vr                , &

       f_nit_vr                             , &
       f_denit_vr                           , &
       smin_no3_leached_vr                  , &
       smin_no3_runoff_vr                   , &
       n2_n2o_ratio_denit_vr                , &
       pot_f_nit_vr                         , &
       pot_f_denit_vr                       , &
       actual_immob_no3_vr                  , &
       actual_immob_nh4_vr                  , &
       smin_no3_to_plant_vr                 , &
       smin_nh4_to_plant_vr                 , &
       f_n2o_denit_vr                       , &
       f_n2o_nit_vr                         , &

       potential_immob_vr                   , &
       actual_immob_vr                      , &
       sminn_to_plant                       , &
       sminn_to_plant_vr                    , &
       supplement_to_sminn_vr               , &
       gross_nmin_vr                        , &
       net_nmin_vr                          , &
       sminn_to_plant_fun_no3_vr            , &
       sminn_to_plant_fun_nh4_vr            , &

       nfix_to_sminn                        , &
       ffix_to_sminn                        , &
       fert_to_sminn                        , &
       soyfixn_to_sminn                     , &
       sminn_to_plant                       , &
       supplement_to_sminn                  , &
       gross_nmin                           , &
       net_nmin                             , &
       denit                                , &
       f_n2o_nit                            , &
       smin_no3_leached                     , &
       smin_no3_runoff                      , &
       sminn_leached                        , &
       som_n_leached                        , &

       decomp_npools_transport_tendency     , &

       decomp_ntransfer_vr                  , &
       decomp_sminn_flux_vr                 , &
       sminn_to_denit_decomp_vr             , &

       decomp_npools_sourcesink

   USE MOD_BGC_Vars_TimeVariables, only:           &
       decomp_k

   IMPLICIT NONE

   PUBLIC CNZeroFluxes

CONTAINS

   SUBROUTINE CNZeroFluxes (i,ps,pe,nl_soil,ndecomp_pools,ndecomp_transitions)


   integer, intent(in) :: i                    ! patch index
   integer, intent(in) :: ps                   ! start pft index
   integer, intent(in) :: pe                   ! end pft index
   integer, intent(in) :: nl_soil              ! number of total soil layers
   integer, intent(in) :: ndecomp_pools        ! number of total soil & litter pools in the decompositions
   integer, intent(in) :: ndecomp_transitions  ! number of total transfers between soil and litter pools in the decomposition

   integer j,k, m

      DO m = ps , pe
      ! CNVegCarbonFluxes set zero
         m_leafc_to_litter_p(m)  = 0._r8
         m_frootc_to_litter_p(m) = 0._r8
         m_leafc_storage_to_litter_p(m) = 0._r8
         m_frootc_storage_to_litter_p(m) = 0._r8
         m_livestemc_storage_to_litter_p(m) = 0._r8
         m_deadstemc_storage_to_litter_p(m) = 0._r8
         m_livecrootc_storage_to_litter_p(m) = 0._r8
         m_deadcrootc_storage_to_litter_p(m) = 0._r8
         m_leafc_xfer_to_litter_p(m) = 0._r8
         m_frootc_xfer_to_litter_p(m) = 0._r8
         m_livestemc_xfer_to_litter_p(m) = 0._r8
         m_deadstemc_xfer_to_litter_p(m) = 0._r8
         m_livecrootc_xfer_to_litter_p(m) = 0._r8
         m_deadcrootc_xfer_to_litter_p(m) = 0._r8
         m_livestemc_to_litter_p(m) = 0._r8
         m_deadstemc_to_litter_p(m) = 0._r8
         m_livecrootc_to_litter_p(m) = 0._r8
         m_deadcrootc_to_litter_p(m) = 0._r8
         m_gresp_storage_to_litter_p(m) = 0._r8
         m_gresp_xfer_to_litter_p(m) = 0._r8
         m_leafc_to_fire_p(m) = 0._r8
         m_leafc_storage_to_fire_p(m) = 0._r8
         m_leafc_xfer_to_fire_p(m) = 0._r8
         m_livestemc_to_fire_p(m) = 0._r8
         m_livestemc_storage_to_fire_p(m) = 0._r8
         m_livestemc_xfer_to_fire_p(m) = 0._r8
         m_deadstemc_to_fire_p(m) = 0._r8
         m_deadstemc_storage_to_fire_p(m) = 0._r8
         m_deadstemc_xfer_to_fire_p(m) = 0._r8
         m_frootc_to_fire_p(m) = 0._r8
         m_frootc_storage_to_fire_p(m) = 0._r8
         m_frootc_xfer_to_fire_p(m) = 0._r8
         m_livecrootc_to_fire_p(m) = 0._r8
         m_livecrootc_storage_to_fire_p(m) = 0._r8
         m_livecrootc_xfer_to_fire_p(m) = 0._r8
         m_deadcrootc_to_fire_p(m) = 0._r8
         m_deadcrootc_storage_to_fire_p(m) = 0._r8
         m_deadcrootc_xfer_to_fire_p(m) = 0._r8
         m_gresp_storage_to_fire_p(m) = 0._r8
         m_gresp_xfer_to_fire_p(m) = 0._r8

         m_leafc_to_litter_fire_p(m)              = 0._r8
         m_leafc_storage_to_litter_fire_p(m)      = 0._r8
         m_leafc_xfer_to_litter_fire_p(m)         = 0._r8
         m_livestemc_to_litter_fire_p(m)          = 0._r8
         m_livestemc_storage_to_litter_fire_p(m)  = 0._r8
         m_livestemc_xfer_to_litter_fire_p(m)     = 0._r8
         m_livestemc_to_deadstemc_fire_p(m)       = 0._r8
         m_deadstemc_to_litter_fire_p(m)          = 0._r8
         m_deadstemc_storage_to_litter_fire_p(m)  = 0._r8
         m_deadstemc_xfer_to_litter_fire_p(m)     = 0._r8
         m_frootc_to_litter_fire_p(m)             = 0._r8
         m_frootc_storage_to_litter_fire_p(m)     = 0._r8
         m_frootc_xfer_to_litter_fire_p(m)        = 0._r8
         m_livecrootc_to_litter_fire_p(m)         = 0._r8
         m_livecrootc_storage_to_litter_fire_p(m) = 0._r8
         m_livecrootc_xfer_to_litter_fire_p(m)    = 0._r8
         m_livecrootc_to_deadcrootc_fire_p(m)     = 0._r8
         m_deadcrootc_to_litter_fire_p(m)         = 0._r8
         m_deadcrootc_storage_to_litter_fire_p(m) = 0._r8
         m_deadcrootc_xfer_to_litter_fire_p(m)    = 0._r8
         m_gresp_storage_to_litter_fire_p(m)      = 0._r8
         m_gresp_xfer_to_litter_fire_p(m)         = 0._r8
         leafc_xfer_to_leafc_p(m)                 = 0._r8
         frootc_xfer_to_frootc_p(m)               = 0._r8
         livestemc_xfer_to_livestemc_p(m)         = 0._r8
         deadstemc_xfer_to_deadstemc_p(m)         = 0._r8
         livecrootc_xfer_to_livecrootc_p(m)       = 0._r8
         deadcrootc_xfer_to_deadcrootc_p(m)       = 0._r8
         leafc_to_litter_p(m)                     = 0._r8
         frootc_to_litter_p(m)                    = 0._r8
         leaf_mr_p(m)                             = 0._r8
         froot_mr_p(m)                            = 0._r8
         livestem_mr_p(m)                         = 0._r8
         livecroot_mr_p(m)                        = 0._r8
         grain_mr_p(m)                            = 0._r8
         leaf_curmr_p(m)                          = 0._r8
         froot_curmr_p(m)                         = 0._r8
         livestem_curmr_p(m)                      = 0._r8
         livecroot_curmr_p(m)                     = 0._r8
         grain_curmr_p(m)                         = 0._r8
         leaf_xsmr_p(m)                           = 0._r8
         froot_xsmr_p(m)                          = 0._r8
         livestem_xsmr_p(m)                       = 0._r8
         livecroot_xsmr_p(m)                      = 0._r8
         grain_xsmr_p(m)                          = 0._r8
         psn_to_cpool_p(m)                        = 0._r8
         cpool_to_xsmrpool_p(m)                   = 0._r8
         cpool_to_leafc_p(m)                      = 0._r8
         cpool_to_leafc_storage_p(m)              = 0._r8
         cpool_to_frootc_p(m)                     = 0._r8
         cpool_to_frootc_storage_p(m)             = 0._r8
         cpool_to_livestemc_p(m)                  = 0._r8
         cpool_to_livestemc_storage_p(m)          = 0._r8
         cpool_to_deadstemc_p(m)                  = 0._r8
         cpool_to_deadstemc_storage_p(m)          = 0._r8
         cpool_to_livecrootc_p(m)                 = 0._r8
         cpool_to_livecrootc_storage_p(m)         = 0._r8
         cpool_to_deadcrootc_p(m)                 = 0._r8
         cpool_to_deadcrootc_storage_p(m)         = 0._r8
         cpool_to_gresp_storage_p(m)              = 0._r8
         cpool_leaf_gr_p(m)                       = 0._r8
         cpool_leaf_storage_gr_p(m)               = 0._r8
         transfer_leaf_gr_p(m)                    = 0._r8
         cpool_froot_gr_p(m)                      = 0._r8
         cpool_froot_storage_gr_p(m)              = 0._r8
         transfer_froot_gr_p(m)                   = 0._r8
         cpool_livestem_gr_p(m)                   = 0._r8
         cpool_livestem_storage_gr_p(m)           = 0._r8
         transfer_livestem_gr_p(m)                = 0._r8
         cpool_deadstem_gr_p(m)                   = 0._r8
         cpool_deadstem_storage_gr_p(m)           = 0._r8
         transfer_deadstem_gr_p(m)                = 0._r8
         cpool_livecroot_gr_p(m)                  = 0._r8
         cpool_livecroot_storage_gr_p(m)          = 0._r8
         transfer_livecroot_gr_p(m)               = 0._r8
         cpool_deadcroot_gr_p(m)                  = 0._r8
         cpool_deadcroot_storage_gr_p(m)          = 0._r8
         transfer_deadcroot_gr_p(m)               = 0._r8
         leafc_storage_to_xfer_p(m)               = 0._r8
         frootc_storage_to_xfer_p(m)              = 0._r8
         livestemc_storage_to_xfer_p(m)           = 0._r8
         deadstemc_storage_to_xfer_p(m)           = 0._r8
         livecrootc_storage_to_xfer_p(m)          = 0._r8
         deadcrootc_storage_to_xfer_p(m)          = 0._r8
         gresp_storage_to_xfer_p(m)               = 0._r8
         livestemc_to_deadstemc_p(m)              = 0._r8
         livecrootc_to_deadcrootc_p(m)            = 0._r8
         crop_seedc_to_leaf_p(m)                  = 0._r8

         hrv_xsmrpool_to_atm_p(m)                 = 0._r8

         xsmrpool_to_atm_p(m)                     = 0._r8
         livestemc_to_litter_p(m)                 = 0._r8
         grainc_to_food_p(m)                      = 0._r8
         grainc_to_seed_p(m)                      = 0._r8
         grainc_xfer_to_grainc_p(m)               = 0._r8
         cpool_to_grainc_p(m)                     = 0._r8
         cpool_to_grainc_storage_p(m)             = 0._r8
         cpool_grain_gr_p(m)                      = 0._r8
         cpool_grain_storage_gr_p(m)              = 0._r8
         transfer_grain_gr_p(m)                   = 0._r8
         grainc_storage_to_xfer_p(m)              = 0._r8
      ENDDO

      DO j=1,nl_soil
         phenology_to_met_c(j,i)                  = 0._r8
         phenology_to_cel_c(j,i)                  = 0._r8
         phenology_to_lig_c(j,i)                  = 0._r8

         gap_mortality_to_met_c(j,i)              = 0._r8
         gap_mortality_to_cel_c(j,i)              = 0._r8
         gap_mortality_to_lig_c(j,i)              = 0._r8
         gap_mortality_to_cwdc(j,i)               = 0._r8

         fire_mortality_to_cwdc(j,i)              = 0._r8
         fire_mortality_to_met_c(j,i)             = 0._r8
         fire_mortality_to_cel_c(j,i)             = 0._r8
         fire_mortality_to_lig_c(j,i)             = 0._r8

         DO k=1,ndecomp_pools
            m_decomp_cpools_to_fire_vr(j,k,i)     = 0._r8
         ENDDO
      ENDDO

      fire_closs(i)                               = 0._r8
      fire_nloss(i)                               = 0._r8
      wood_harvestc(i)                            = 0._r8
      wood_harvestn(i)                            = 0._r8
      grainc_to_cropprodc(i)                      = 0._r8
      grainn_to_cropprodn(i)                      = 0._r8

      DO m = ps, pe
         gpp_p(m)                                 = 0._r8
         wood_harvestc_p(m)                       = 0._r8
         wood_harvestn_p(m)                       = 0._r8
         grainc_to_cropprodc_p(m)                 = 0._r8
         grainn_to_cropprodn_p(m)                 = 0._r8
         soyfixn_p(m)                             = 0._r8

         fire_closs_p(m)                          = 0._r8
         fire_nloss_p(m)                          = 0._r8

         ! CNVegNitrogenFluxes set zero

         m_leafn_to_litter_p(m)                   = 0._r8
         m_frootn_to_litter_p(m)                  = 0._r8
         m_leafn_storage_to_litter_p(m)           = 0._r8
         m_frootn_storage_to_litter_p(m)          = 0._r8
         m_livestemn_storage_to_litter_p(m)       = 0._r8
         m_deadstemn_storage_to_litter_p(m)       = 0._r8
         m_livecrootn_storage_to_litter_p(m)      = 0._r8
         m_deadcrootn_storage_to_litter_p(m)      = 0._r8
         m_leafn_xfer_to_litter_p(m)              = 0._r8
         m_frootn_xfer_to_litter_p(m)             = 0._r8
         m_livestemn_xfer_to_litter_p(m)          = 0._r8
         m_deadstemn_xfer_to_litter_p(m)          = 0._r8
         m_livecrootn_xfer_to_litter_p(m)         = 0._r8
         m_deadcrootn_xfer_to_litter_p(m)         = 0._r8
         m_livestemn_to_litter_p(m)               = 0._r8
         m_deadstemn_to_litter_p(m)               = 0._r8
         m_livecrootn_to_litter_p(m)              = 0._r8
         m_deadcrootn_to_litter_p(m)              = 0._r8
         m_retransn_to_litter_p(m)                = 0._r8

         m_leafn_to_fire_p(m)                     = 0._r8
         m_leafn_storage_to_fire_p(m)             = 0._r8
         m_leafn_xfer_to_fire_p(m)                = 0._r8
         m_livestemn_to_fire_p(m)                 = 0._r8
         m_livestemn_storage_to_fire_p(m)         = 0._r8
         m_livestemn_xfer_to_fire_p(m)            = 0._r8
         m_deadstemn_to_fire_p(m)                 = 0._r8
         m_deadstemn_storage_to_fire_p(m)         = 0._r8
         m_deadstemn_xfer_to_fire_p(m)            = 0._r8
         m_frootn_to_fire_p(m)                    = 0._r8
         m_frootn_storage_to_fire_p(m)            = 0._r8
         m_frootn_xfer_to_fire_p(m)               = 0._r8
         m_livecrootn_to_fire_p(m)                = 0._r8
         m_livecrootn_storage_to_fire_p(m)        = 0._r8
         m_livecrootn_xfer_to_fire_p(m)           = 0._r8
         m_deadcrootn_to_fire_p(m)                = 0._r8
         m_deadcrootn_storage_to_fire_p(m)        = 0._r8
         m_deadcrootn_xfer_to_fire_p(m)           = 0._r8
         m_retransn_to_fire_p(m)                  = 0._r8


         m_leafn_to_litter_fire_p(m)              = 0._r8
         m_leafn_storage_to_litter_fire_p(m)      = 0._r8
         m_leafn_xfer_to_litter_fire_p(m)         = 0._r8
         m_livestemn_to_litter_fire_p(m)          = 0._r8
         m_livestemn_storage_to_litter_fire_p(m)  = 0._r8
         m_livestemn_xfer_to_litter_fire_p(m)     = 0._r8
         m_livestemn_to_deadstemn_fire_p(m)       = 0._r8
         m_deadstemn_to_litter_fire_p(m)          = 0._r8
         m_deadstemn_storage_to_litter_fire_p(m)  = 0._r8
         m_deadstemn_xfer_to_litter_fire_p(m)     = 0._r8
         m_frootn_to_litter_fire_p(m)             = 0._r8
         m_frootn_storage_to_litter_fire_p(m)     = 0._r8
         m_frootn_xfer_to_litter_fire_p(m)        = 0._r8
         m_livecrootn_to_litter_fire_p(m)         = 0._r8
         m_livecrootn_storage_to_litter_fire_p(m) = 0._r8
         m_livecrootn_xfer_to_litter_fire_p(m)    = 0._r8
         m_livecrootn_to_deadcrootn_fire_p(m)     = 0._r8
         m_deadcrootn_to_litter_fire_p(m)         = 0._r8
         m_deadcrootn_storage_to_litter_fire_p(m) = 0._r8
         m_deadcrootn_xfer_to_litter_fire_p(m)    = 0._r8
         m_retransn_to_litter_fire_p(m)           = 0._r8

         leafn_xfer_to_leafn_p(m)                 = 0._r8
         frootn_xfer_to_frootn_p(m)               = 0._r8
         livestemn_xfer_to_livestemn_p(m)         = 0._r8
         deadstemn_xfer_to_deadstemn_p(m)         = 0._r8
         livecrootn_xfer_to_livecrootn_p(m)       = 0._r8
         deadcrootn_xfer_to_deadcrootn_p(m)       = 0._r8
         leafn_to_litter_p(m)                     = 0._r8
         leafn_to_retransn_p(m)                   = 0._r8
         frootn_to_litter_p(m)                    = 0._r8
         retransn_to_npool_p(m)                   = 0._r8
         free_retransn_to_npool_p(m)              = 0._r8
         sminn_to_npool_p(m)                      = 0._r8
         npool_to_leafn_p(m)                      = 0._r8
         npool_to_leafn_storage_p(m)              = 0._r8
         npool_to_frootn_p(m)                     = 0._r8
         npool_to_frootn_storage_p(m)             = 0._r8
         npool_to_livestemn_p(m)                  = 0._r8
         npool_to_livestemn_storage_p(m)          = 0._r8
         npool_to_deadstemn_p(m)                  = 0._r8
         npool_to_deadstemn_storage_p(m)          = 0._r8
         npool_to_livecrootn_p(m)                 = 0._r8
         npool_to_livecrootn_storage_p(m)         = 0._r8
         npool_to_deadcrootn_p(m)                 = 0._r8
         npool_to_deadcrootn_storage_p(m)         = 0._r8
         leafn_storage_to_xfer_p(m)               = 0._r8
         frootn_storage_to_xfer_p(m)              = 0._r8
         livestemn_storage_to_xfer_p(m)           = 0._r8
         deadstemn_storage_to_xfer_p(m)           = 0._r8
         livecrootn_storage_to_xfer_p(m)          = 0._r8
         deadcrootn_storage_to_xfer_p(m)          = 0._r8
         livestemn_to_deadstemn_p(m)              = 0._r8
         livestemn_to_retransn_p(m)               = 0._r8
         livecrootn_to_deadcrootn_p(m)            = 0._r8
         livecrootn_to_retransn_p(m)              = 0._r8

         crop_seedn_to_leaf_p(m)                  = 0._r8

         livestemn_to_litter_p(m)                 = 0._r8
         grainn_to_food_p(m)                      = 0._r8
         grainn_to_seed_p(m)                      = 0._r8
         grainn_xfer_to_grainn_p(m)               = 0._r8
         npool_to_grainn_p(m)                     = 0._r8
         npool_to_grainn_storage_p(m)             = 0._r8
         grainn_storage_to_xfer_p(m)              = 0._r8
         frootn_to_retransn_p(m)                  = 0._r8
      ENDDO

      DO j=1,nl_soil

         ! phenology: litterfall and crop fluxes associated wit
         phenology_to_met_n(j,i)                  = 0._r8
         phenology_to_cel_n(j,i)                  = 0._r8
         phenology_to_lig_n(j,i)                  = 0._r8

        ! gap mortality
         gap_mortality_to_met_n(j,i)              = 0._r8
         gap_mortality_to_cel_n(j,i)              = 0._r8
         gap_mortality_to_lig_n(j,i)              = 0._r8
         gap_mortality_to_cwdn(j,i)               = 0._r8

        ! fire
         fire_mortality_to_cwdn(j,i)              = 0._r8
         fire_mortality_to_met_n(j,i)             = 0._r8
         fire_mortality_to_cel_n(j,i)             = 0._r8
         fire_mortality_to_lig_n(j,i)             = 0._r8

      ENDDO

      DO k = 1, ndecomp_pools
         DO j = 1, nl_soil
            m_decomp_npools_to_fire_vr(j,k,i)     = 0._r8
         ENDDO
      ENDDO

      DO k=1,ndecomp_transitions
         DO j=1,nl_soil
            decomp_hr_vr(j,k,i)                   = 0._r8
            decomp_ctransfer_vr(j,k,i)            = 0._r8
         ENDDO
      ENDDO

      DO k = 1, ndecomp_pools
         DO j = 1, nl_soil
            decomp_cpools_transport_tendency(j,k,i) = 0._r8
            decomp_cpools_sourcesink(j,k,i)         = 0._r8
            decomp_k(j,k,i)                         = 0._r8
         ENDDO
      ENDDO

      somc_fire(i)                                = 0._r8
      som_c_leached(i)                            = 0._r8
      decomp_hr(i)                                = 0._r8



      IF(.not. DEF_USE_NITRIF)THEN
         DO j = 1, nl_soil
            sminn_to_denit_excess_vr(j,i)            = 0._r8
            sminn_leached_vr(j,i)                    = 0._r8
            sminn_to_plant_fun_vr(j,i)               = 0._r8
         ENDDO
      ELSE
         DO j = 1, nl_soil
            f_nit_vr(j,i)                            = 0._r8
            f_denit_vr(j,i)                          = 0._r8
            smin_no3_leached_vr(j,i)                 = 0._r8
            smin_no3_runoff_vr(j,i)                  = 0._r8
            n2_n2o_ratio_denit_vr(j,i)               = 0._r8
            pot_f_nit_vr(j,i)                        = 0._r8
            pot_f_denit_vr(j,i)                      = 0._r8
            actual_immob_no3_vr(j,i)                 = 0._r8
            actual_immob_nh4_vr(j,i)                 = 0._r8
            smin_no3_to_plant_vr(j,i)                = 0._r8
            smin_nh4_to_plant_vr(j,i)                = 0._r8
            f_n2o_denit_vr(j,i)                      = 0._r8
            f_n2o_nit_vr(j,i)                        = 0._r8
         ENDDO
      ENDIF

      DO j = 1, nl_soil
         potential_immob_vr(j,i)                  = 0._r8
         actual_immob_vr(j,i)                     = 0._r8
         sminn_to_plant(i)                        = 0._r8
         sminn_to_plant_vr(j,i)                   = 0._r8
         supplement_to_sminn_vr(j,i)              = 0._r8
         gross_nmin_vr(j,i)                       = 0._r8
         net_nmin_vr(j,i)                         = 0._r8
         sminn_to_plant_fun_no3_vr(j,i)           = 0._r8
         sminn_to_plant_fun_nh4_vr(j,i)           = 0._r8
      ENDDO

      nfix_to_sminn(i)                            = 0._r8
      ffix_to_sminn(i)                            = 0._r8
      fert_to_sminn(i)                            = 0._r8
      soyfixn_to_sminn(i)                         = 0._r8
      supplement_to_sminn(i)                      = 0._r8
      gross_nmin(i)                               = 0._r8
      net_nmin(i)                                 = 0._r8
      denit(i)                                    = 0._r8
      f_n2o_nit(i)                                = 0._r8
      smin_no3_leached(i)                         = 0._r8
      smin_no3_runoff(i)                          = 0._r8
      sminn_leached(i)                            = 0._r8
      som_n_leached(i)                            = 0._r8

      DO k = 1, ndecomp_pools
         DO j = 1, nl_soil
            decomp_npools_transport_tendency(j,k,i) = 0._r8
         ENDDO
      ENDDO

      DO k = 1, ndecomp_transitions
         DO j = 1, nl_soil
            decomp_ntransfer_vr(j,k,i)            = 0._r8
            decomp_sminn_flux_vr(j,k,i)           = 0._r8
         ENDDO
      ENDDO

      IF(.not. DEF_USE_NITRIF)THEN
         DO k = 1, ndecomp_transitions
            DO j = 1, nl_soil
               sminn_to_denit_decomp_vr(j,k,i)    = 0._r8
            ENDDO
         ENDDO
      ENDIF

      DO k = 1, ndecomp_pools
         DO j = 1, nl_soil
            decomp_npools_sourcesink(j,k,i)       = 0._r8
         ENDDO
      ENDDO

   END SUBROUTINE CNZeroFluxes

END MODULE MOD_BGC_CNZeroFluxes
#endif
