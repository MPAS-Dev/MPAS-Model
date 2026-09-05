#include <define.h>
#ifdef BGC
MODULE MOD_BGC_CNNStateUpdate1

!-------------------------------------------------------------------------------------------------
! !DESCRIPTION:
! First updates in vegetation and soil nitrogen. The major updates are included in bgc_CNNStateUpdate1Mod
!  1. Update phenology-associated veg and soil N pool size changes, including plant growth
!  2. Update decomposition-associated soil N pool size changes
!  3. Record the accumulated N transfers associated to phenology and decomposition for semi-analytic spinup
!
! !ORIGINAL:
! The Community Land Model version 5.0 (CLM5.0)
!
! REVISION:
! Xingjie Lu, 2022, 1) modify original CLM5 to be compatible with CoLM code structure.
!                   2) Record the accumulated phenology-associated N transfer for veg and soil N semi-analytic spinup
!                   3) Record the accumulated decomposition-associated N transfer for soil N semi-analytic spinup

   USE MOD_Precision
   USE MOD_Vars_PFTimeInvariants, only: pftclass
   USE MOD_Const_PFT, only: woody
   USE MOD_Namelist, only: DEF_USE_SASU, DEF_USE_DiagMatrix
   USE MOD_BGC_Vars_TimeInvariants, only: &
   ! bgc constants
            donor_pool, receiver_pool, i_met_lit, i_cel_lit, i_lig_lit, i_cwd, i_soil1, i_soil2, i_soil3

   USE MOD_BGC_Vars_TimeVariables, only: &
            I_met_n_vr_acc, I_cel_n_vr_acc, I_lig_n_vr_acc

   USE MOD_BGC_Vars_1DFluxes, only: &
   ! decomposition pools flux varables (in)
            decomp_npools_sourcesink, &
            phenology_to_met_n      , phenology_to_cel_n,  phenology_to_lig_n

   USE MOD_BGC_Vars_PFTimeVariables, only: &
   ! vegetation nitrogen state variables (inout)
            leafn_p            , leafn_storage_p     , leafn_xfer_p     , &
            frootn_p           , frootn_storage_p    , frootn_xfer_p    , &
            livestemn_p        , livestemn_storage_p , livestemn_xfer_p , &
            deadstemn_p        , deadstemn_storage_p , deadstemn_xfer_p , &
            livecrootn_p       , livecrootn_storage_p, livecrootn_xfer_p, &
            deadcrootn_p       , deadcrootn_storage_p, deadcrootn_xfer_p, &
            grainn_p           , grainn_storage_p    , grainn_xfer_p    , &
            cropseedn_deficit_p, retransn_p          , npool_p          , &

   ! SASU variables
            I_leafn_p_acc     , I_leafn_st_p_acc     , I_frootn_p_acc    , I_frootn_st_p_acc    , &
            I_livestemn_p_acc , I_livestemn_st_p_acc , I_deadstemn_p_acc , I_deadstemn_st_p_acc , &
            I_livecrootn_p_acc, I_livecrootn_st_p_acc, I_deadcrootn_p_acc, I_deadcrootn_st_p_acc, &
            I_grainn_p_acc    , I_grainn_st_p_acc    , &

            AKX_leafn_xf_to_leafn_p_acc           , AKX_frootn_xf_to_frootn_p_acc           , AKX_livestemn_xf_to_livestemn_p_acc     , &
            AKX_deadstemn_xf_to_deadstemn_p_acc   , AKX_livecrootn_xf_to_livecrootn_p_acc   , AKX_deadcrootn_xf_to_deadcrootn_p_acc   , &
            AKX_grainn_xf_to_grainn_p_acc         , AKX_livestemn_to_deadstemn_p_acc        , AKX_livecrootn_to_deadcrootn_p_acc      , &

            AKX_leafn_st_to_leafn_xf_p_acc        , AKX_frootn_st_to_frootn_xf_p_acc        , AKX_livestemn_st_to_livestemn_xf_p_acc  , &
            AKX_deadstemn_st_to_deadstemn_xf_p_acc, AKX_livecrootn_st_to_livecrootn_xf_p_acc, AKX_deadcrootn_st_to_deadcrootn_xf_p_acc, &
            AKX_livestemn_st_to_livestemn_xf_p_acc, AKX_grainn_st_to_grainn_xf_p_acc        , &

            AKX_leafn_to_retransn_p_acc           , AKX_frootn_to_retransn_p_acc            , AKX_livestemn_to_retransn_p_acc         , &
            AKX_livecrootn_to_retransn_p_acc      , &

            AKX_retransn_to_leafn_p_acc           , AKX_retransn_to_frootn_p_acc            , AKX_retransn_to_livestemn_p_acc         , &
            AKX_retransn_to_deadstemn_p_acc       , AKX_retransn_to_livecrootn_p_acc        , AKX_retransn_to_deadcrootn_p_acc        , &
            AKX_retransn_to_grainn_p_acc          , &

            AKX_retransn_to_leafn_st_p_acc        , AKX_retransn_to_frootn_st_p_acc         , AKX_retransn_to_livestemn_st_p_acc      , &
            AKX_retransn_to_deadstemn_st_p_acc    , AKX_retransn_to_livecrootn_st_p_acc     , AKX_retransn_to_deadcrootn_st_p_acc     , &
            AKX_retransn_to_grainn_st_p_acc       , &

            AKX_leafn_exit_p_acc                  , AKX_frootn_exit_p_acc                   , AKX_livestemn_exit_p_acc                , &
            AKX_deadstemn_exit_p_acc              , AKX_livecrootn_exit_p_acc               , AKX_deadcrootn_exit_p_acc               , &
            AKX_grainn_exit_p_acc                 , AKX_retransn_exit_p_acc                 , &

            AKX_leafn_st_exit_p_acc               , AKX_frootn_st_exit_p_acc                , AKX_livestemn_st_exit_p_acc             , &
            AKX_deadstemn_st_exit_p_acc           , AKX_livecrootn_st_exit_p_acc            , AKX_deadcrootn_st_exit_p_acc            , &
            AKX_grainn_st_exit_p_acc              , &

            AKX_leafn_xf_exit_p_acc               , AKX_frootn_xf_exit_p_acc                , AKX_livestemn_xf_exit_p_acc             , &
            AKX_deadstemn_xf_exit_p_acc           , AKX_livecrootn_xf_exit_p_acc            , AKX_deadcrootn_xf_exit_p_acc            , &
            AKX_grainn_xf_exit_p_acc

   USE MOD_BGC_Vars_1DPFTFluxes, only: &
   ! vegetation nitrogen flux variables (in)
   ! xfer to display
            leafn_xfer_to_leafn_p          , frootn_xfer_to_frootn_p        , &
            livestemn_xfer_to_livestemn_p  , deadstemn_xfer_to_deadstemn_p  , &
            livecrootn_xfer_to_livecrootn_p, deadcrootn_xfer_to_deadcrootn_p, &
            grainn_xfer_to_grainn_p        , &

   ! storage to xfer (in)
            leafn_storage_to_xfer_p     , frootn_storage_to_xfer_p    , &
            livestemn_storage_to_xfer_p , deadstemn_storage_to_xfer_p , &
            livecrootn_storage_to_xfer_p, deadcrootn_storage_to_xfer_p, &
            grainn_storage_to_xfer_p    , &

   ! display to litter & live to dead (in)
            leafn_to_litter_p       , frootn_to_litter_p        , &
            grainn_to_food_p        , grainn_to_seed_p          , &
            crop_seedn_to_leaf_p    , livestemn_to_litter_p     , &
            livestemn_to_deadstemn_p, livecrootn_to_deadcrootn_p, &

   ! display to retransn / retransn to npool (in)
            leafn_to_retransn_p     , frootn_to_retransn_p      , &
            livestemn_to_retransn_p , livecrootn_to_retransn_p  , &
            retransn_to_npool_p     , free_retransn_to_npool_p  , &

   ! npool to display/storage (in)
            npool_to_leafn_p     , npool_to_leafn_storage_p     , &
            npool_to_frootn_p    , npool_to_frootn_storage_p    , &
            npool_to_livestemn_p , npool_to_livestemn_storage_p , &
            npool_to_deadstemn_p , npool_to_deadstemn_storage_p , &
            npool_to_livecrootn_p, npool_to_livecrootn_storage_p, &
            npool_to_deadcrootn_p, npool_to_deadcrootn_storage_p, &
            npool_to_grainn_p    , npool_to_grainn_storage_p    , plant_nalloc_p

   USE MOD_Vars_PFTimeInvariants, only: pftfrac
   IMPLICIT NONE

   PUBLIC NStateUpdate1

CONTAINS

   SUBROUTINE NStateUpdate1 (i, ps, pe, deltim, nl_soil, ndecomp_transitions, npcropmin,dz_soi)

   integer ,intent(in) :: i                   ! patch index
   integer ,intent(in) :: ps                  ! start pft index
   integer ,intent(in) :: pe                  ! END pft index
   real(r8),intent(in) :: deltim              ! time step in seconds
   integer ,intent(in) :: nl_soil             ! number of total soil layers
   integer ,intent(in) :: ndecomp_transitions ! number of total transitions among different litter & soil bgc pools
   integer ,intent(in) :: npcropmin           ! index of first crop pft
   real(r8),intent(in) :: dz_soi(1:nl_soil)   ! thicknesses of each soil layer

   integer j,k
   integer ivt, m
   real(r8) f_retr_in_nall

   ! soilbiogeochemistry fluxes TODO - this should be moved elsewhere
   ! plant to litter fluxes -  phenology and dynamic landcover fluxes
      DO j = 1, nl_soil
         decomp_npools_sourcesink(j,i_met_lit,i) = phenology_to_met_n(j,i) * deltim

         decomp_npools_sourcesink(j,i_cel_lit,i) = phenology_to_cel_n(j,i) * deltim

         decomp_npools_sourcesink(j,i_lig_lit,i) = phenology_to_lig_n(j,i) * deltim

         decomp_npools_sourcesink(j,i_cwd,i) = 0._r8

      ENDDO

      IF(DEF_USE_SASU .or. DEF_USE_DiagMatrix)THEN
         DO j=1,nl_soil
            I_met_n_vr_acc(j,i) = I_met_n_vr_acc(j,i) + phenology_to_met_n(j,i) * deltim
            I_cel_n_vr_acc(j,i) = I_cel_n_vr_acc(j,i) + phenology_to_cel_n(j,i) * deltim
            I_lig_n_vr_acc(j,i) = I_lig_n_vr_acc(j,i) + phenology_to_lig_n(j,i) * deltim
         ENDDO
      ENDIF

      DO m = ps , pe
         ivt = pftclass(m)
        ! phenology: transfer growth fluxes
         leafn_p(m)       = leafn_p(m)       + leafn_xfer_to_leafn_p(m)*deltim
         leafn_xfer_p(m)  = leafn_xfer_p(m)  - leafn_xfer_to_leafn_p(m)*deltim
         frootn_p(m)      = frootn_p(m)      + frootn_xfer_to_frootn_p(m)*deltim
         frootn_xfer_p(m) = frootn_xfer_p(m) - frootn_xfer_to_frootn_p(m)*deltim

         IF (woody(ivt) == 1) THEN
            livestemn_p(m)       = livestemn_p(m)       + livestemn_xfer_to_livestemn_p(m)*deltim
            livestemn_xfer_p(m)  = livestemn_xfer_p(m)  - livestemn_xfer_to_livestemn_p(m)*deltim
            deadstemn_p(m)       = deadstemn_p(m)       + deadstemn_xfer_to_deadstemn_p(m)*deltim
            deadstemn_xfer_p(m)  = deadstemn_xfer_p(m)  - deadstemn_xfer_to_deadstemn_p(m)*deltim
            livecrootn_p(m)      = livecrootn_p(m)      + livecrootn_xfer_to_livecrootn_p(m)*deltim
            livecrootn_xfer_p(m) = livecrootn_xfer_p(m) - livecrootn_xfer_to_livecrootn_p(m)*deltim
            deadcrootn_p(m)      = deadcrootn_p(m)      + deadcrootn_xfer_to_deadcrootn_p(m)*deltim
            deadcrootn_xfer_p(m) = deadcrootn_xfer_p(m) - deadcrootn_xfer_to_deadcrootn_p(m)*deltim
         ENDIF

         IF (ivt >= npcropmin) THEN ! skip 2 generic crops
            ! lines here for consistency; the transfer terms are zero
            livestemn_p(m)       = livestemn_p(m)      + livestemn_xfer_to_livestemn_p(m)*deltim
            livestemn_xfer_p(m)  = livestemn_xfer_p(m) - livestemn_xfer_to_livestemn_p(m)*deltim
            grainn_p(m)          = grainn_p(m)         + grainn_xfer_to_grainn_p(m)*deltim
            grainn_xfer_p(m)     = grainn_xfer_p(m)    - grainn_xfer_to_grainn_p(m)*deltim
         ENDIF
         IF(DEF_USE_SASU .or. DEF_USE_DiagMatrix)THEN
            AKX_leafn_xf_to_leafn_p_acc  (m) = AKX_leafn_xf_to_leafn_p_acc  (m) + leafn_xfer_to_leafn_p  (m) * deltim
            AKX_frootn_xf_to_frootn_p_acc(m) = AKX_frootn_xf_to_frootn_p_acc(m) + frootn_xfer_to_frootn_p(m) * deltim
            AKX_leafn_xf_exit_p_acc      (m) = AKX_leafn_xf_exit_p_acc      (m) + leafn_xfer_to_leafn_p  (m) * deltim
            AKX_frootn_xf_exit_p_acc     (m) = AKX_frootn_xf_exit_p_acc     (m) + frootn_xfer_to_frootn_p(m) * deltim
            IF(woody(ivt) == 1)THEN
               AKX_livestemn_xf_to_livestemn_p_acc  (m) = AKX_livestemn_xf_to_livestemn_p_acc  (m) + livestemn_xfer_to_livestemn_p  (m) * deltim
               AKX_livestemn_xf_exit_p_acc          (m) = AKX_livestemn_xf_exit_p_acc          (m) + livestemn_xfer_to_livestemn_p  (m) * deltim
               AKX_deadstemn_xf_to_deadstemn_p_acc  (m) = AKX_deadstemn_xf_to_deadstemn_p_acc  (m) + deadstemn_xfer_to_deadstemn_p  (m) * deltim
               AKX_deadstemn_xf_exit_p_acc          (m) = AKX_deadstemn_xf_exit_p_acc          (m) + deadstemn_xfer_to_deadstemn_p  (m) * deltim
               AKX_livecrootn_xf_to_livecrootn_p_acc(m) = AKX_livecrootn_xf_to_livecrootn_p_acc(m) + livecrootn_xfer_to_livecrootn_p(m) * deltim
               AKX_livecrootn_xf_exit_p_acc         (m) = AKX_livecrootn_xf_exit_p_acc         (m) + livecrootn_xfer_to_livecrootn_p(m) * deltim
               AKX_deadcrootn_xf_to_deadcrootn_p_acc(m) = AKX_deadcrootn_xf_to_deadcrootn_p_acc(m) + deadcrootn_xfer_to_deadcrootn_p(m) * deltim
               AKX_deadcrootn_xf_exit_p_acc         (m) = AKX_deadcrootn_xf_exit_p_acc         (m) + deadcrootn_xfer_to_deadcrootn_p(m) * deltim
            ENDIF
            IF(ivt >= npcropmin) THEN
               AKX_livestemn_xf_to_livestemn_p_acc(m) = AKX_livestemn_xf_to_livestemn_p_acc(m) + livestemn_xfer_to_livestemn_p(m) * deltim
               AKX_livestemn_xf_exit_p_acc        (m) = AKX_livestemn_xf_exit_p_acc        (m) + livestemn_xfer_to_livestemn_p(m) * deltim
               AKX_grainn_xf_to_grainn_p_acc      (m) = AKX_grainn_xf_to_grainn_p_acc      (m) + grainn_xfer_to_grainn_p      (m) * deltim
               AKX_grainn_xf_exit_p_acc           (m) = AKX_grainn_xf_exit_p_acc           (m) + grainn_xfer_to_grainn_p      (m) * deltim
            ENDIF
         ENDIF

         ! phenology: litterfall and retranslocation fluxes
         leafn_p(m)    = leafn_p(m)    - leafn_to_litter_p(m)*deltim
         frootn_p(m)   = frootn_p(m)   - frootn_to_litter_p(m)*deltim
         leafn_p(m)    = leafn_p(m)    - leafn_to_retransn_p(m)*deltim
         retransn_p(m) = retransn_p(m) + leafn_to_retransn_p(m)*deltim

         ! live wood turnover and retranslocation fluxes
         IF (woody(ivt) == 1) THEN
            livestemn_p(m)    = livestemn_p(m)  - livestemn_to_deadstemn_p(m)*deltim
            deadstemn_p(m)    = deadstemn_p(m)  + livestemn_to_deadstemn_p(m)*deltim
            livecrootn_p(m)   = livecrootn_p(m) - livecrootn_to_deadcrootn_p(m)*deltim
            deadcrootn_p(m)   = deadcrootn_p(m) + livecrootn_to_deadcrootn_p(m)*deltim

            livestemn_p(m)    = livestemn_p(m)  - livestemn_to_retransn_p(m)*deltim
            retransn_p(m)     = retransn_p(m)   + livestemn_to_retransn_p(m)*deltim
            livecrootn_p(m)   = livecrootn_p(m) - livecrootn_to_retransn_p(m)*deltim
            retransn_p(m)     = retransn_p(m)   + livecrootn_to_retransn_p(m)*deltim
         ENDIF
         IF (ivt >= npcropmin) THEN
            frootn_p(m)       = frootn_p(m)     - frootn_to_retransn_p(m)*deltim
            retransn_p(m)     = retransn_p(m)   + frootn_to_retransn_p(m)*deltim
            livestemn_p(m)    = livestemn_p(m)  - livestemn_to_litter_p(m)*deltim
            livestemn_p(m)    = livestemn_p(m)  - livestemn_to_retransn_p(m)*deltim
            retransn_p(m)     = retransn_p(m)   + livestemn_to_retransn_p(m)*deltim
            grainn_p(m)       = grainn_p(m) &
                              - (grainn_to_food_p(m) + grainn_to_seed_p(m))*deltim
            cropseedn_deficit_p(m) = cropseedn_deficit_p(m) &
                              - crop_seedn_to_leaf_p(m) * deltim &
                              + grainn_to_seed_p(m) * deltim
         ENDIF
         IF(DEF_USE_SASU .or. DEF_USE_DiagMatrix)THEN
            AKX_leafn_exit_p_acc       (m) = AKX_leafn_exit_p_acc       (m) + leafn_to_litter_p  (m) * deltim
            AKX_frootn_exit_p_acc      (m) = AKX_frootn_exit_p_acc      (m) + frootn_to_litter_p (m) * deltim
            AKX_leafn_to_retransn_p_acc(m) = AKX_leafn_to_retransn_p_acc(m) + leafn_to_retransn_p(m) * deltim
            AKX_leafn_exit_p_acc       (m) = AKX_leafn_exit_p_acc       (m) + leafn_to_retransn_p(m) * deltim
            IF(woody(ivt) == 1) THEN
               AKX_livestemn_to_deadstemn_p_acc  (m) = AKX_livestemn_to_deadstemn_p_acc  (m) + livestemn_to_deadstemn_p  (m) * deltim
               AKX_livestemn_exit_p_acc          (m) = AKX_livestemn_exit_p_acc          (m) + livestemn_to_deadstemn_p  (m) * deltim
               AKX_livecrootn_to_deadcrootn_p_acc(m) = AKX_livecrootn_to_deadcrootn_p_acc(m) + livecrootn_to_deadcrootn_p(m) * deltim
               AKX_livecrootn_exit_p_acc         (m) = AKX_livecrootn_exit_p_acc         (m) + livecrootn_to_deadcrootn_p(m) * deltim

               AKX_livestemn_to_retransn_p_acc   (m) = AKX_livestemn_to_retransn_p_acc   (m) + livestemn_to_retransn_p   (m) * deltim
               AKX_livestemn_exit_p_acc          (m) = AKX_livestemn_exit_p_acc          (m) + livestemn_to_retransn_p   (m) * deltim
               AKX_livecrootn_to_retransn_p_acc  (m) = AKX_livecrootn_to_retransn_p_acc  (m) + livecrootn_to_retransn_p  (m) * deltim
               AKX_livecrootn_exit_p_acc         (m) = AKX_livecrootn_exit_p_acc         (m) + livecrootn_to_retransn_p  (m) * deltim
            ENDIF
            IF(ivt >= npcropmin) THEN
               AKX_frootn_to_retransn_p_acc      (m) = AKX_frootn_to_retransn_p_acc      (m) + frootn_to_retransn_p      (m) * deltim
               AKX_frootn_exit_p_acc             (m) = AKX_frootn_exit_p_acc             (m) + frootn_to_retransn_p      (m) * deltim
               AKX_livestemn_exit_p_acc          (m) = AKX_livestemn_exit_p_acc          (m) + livestemn_to_litter_p     (m) * deltim
               AKX_livestemn_to_retransn_p_acc   (m) = AKX_livestemn_to_retransn_p_acc   (m) + livestemn_to_retransn_p   (m) * deltim
               AKX_livestemn_exit_p_acc          (m) = AKX_livestemn_exit_p_acc          (m) + livestemn_to_retransn_p   (m) * deltim
               AKX_grainn_exit_p_acc             (m) = AKX_grainn_exit_p_acc             (m) + (grainn_to_food_p(m) + grainn_to_seed_p(m)) * deltim
            ENDIF
         ENDIF

         ! allocation fluxes
         retransn_p(m)        = retransn_p(m)       - retransn_to_npool_p(m)*deltim
         retransn_p(m)        = retransn_p(m)       - free_retransn_to_npool_p(m)*deltim
         leafn_p(m)           = leafn_p(m)          + npool_to_leafn_p(m)*deltim
         leafn_storage_p(m)   = leafn_storage_p(m)  + npool_to_leafn_storage_p(m)*deltim
         frootn_p(m)          = frootn_p(m)         + npool_to_frootn_p(m)*deltim
         frootn_storage_p(m)  = frootn_storage_p(m) + npool_to_frootn_storage_p(m)*deltim

         IF (woody(ivt) == 1) THEN
            livestemn_p(m)          = livestemn_p(m)          + npool_to_livestemn_p(m)*deltim
            livestemn_storage_p(m)  = livestemn_storage_p(m)  + npool_to_livestemn_storage_p(m)*deltim
            deadstemn_p(m)          = deadstemn_p(m)          + npool_to_deadstemn_p(m)*deltim
            deadstemn_storage_p(m)  = deadstemn_storage_p(m)  + npool_to_deadstemn_storage_p(m)*deltim
            livecrootn_p(m)         = livecrootn_p(m)         + npool_to_livecrootn_p(m)*deltim
            livecrootn_storage_p(m) = livecrootn_storage_p(m) + npool_to_livecrootn_storage_p(m)*deltim
            deadcrootn_p(m)         = deadcrootn_p(m)         + npool_to_deadcrootn_p(m)*deltim
            deadcrootn_storage_p(m) = deadcrootn_storage_p(m) + npool_to_deadcrootn_storage_p(m)*deltim
         ENDIF

         IF (ivt >= npcropmin) THEN ! skip 2 generic crops
            livestemn_p(m)          = livestemn_p(m)          + npool_to_livestemn_p(m)*deltim
            livestemn_storage_p(m)  = livestemn_storage_p(m)  + npool_to_livestemn_storage_p(m)*deltim
            grainn_p(m)             = grainn_p(m)             + npool_to_grainn_p(m)*deltim
            grainn_storage_p(m)     = grainn_storage_p(m)     + npool_to_grainn_storage_p(m)*deltim
         ENDIF
         IF(DEF_USE_SASU .or. DEF_USE_DiagMatrix)THEN
            IF(plant_nalloc_p(m) .ne. 0)THEN
               f_retr_in_nall = retransn_to_npool_p(m) / plant_nalloc_p(m)
               AKX_retransn_exit_p_acc        (m) = AKX_retransn_exit_p_acc        (m) &
                                                  + (retransn_to_npool_p       (m) + free_retransn_to_npool_p (m)) * deltim
               I_leafn_p_acc                  (m) = I_leafn_p_acc              (m) + npool_to_leafn_p         (m) * (1._r8 - f_retr_in_nall) * deltim
               AKX_retransn_to_leafn_p_acc    (m) = AKX_retransn_to_leafn_p_acc    (m) + npool_to_leafn_p         (m) * f_retr_in_nall           * deltim
               I_leafn_st_p_acc               (m) = I_leafn_st_p_acc           (m) + npool_to_leafn_storage_p (m) * (1._r8 - f_retr_in_nall) * deltim
               AKX_retransn_to_leafn_st_p_acc (m) = AKX_retransn_to_leafn_st_p_acc (m) + npool_to_leafn_storage_p (m) * f_retr_in_nall           * deltim
               I_frootn_p_acc                 (m) = I_frootn_p_acc             (m) + npool_to_frootn_p        (m) * (1._r8 - f_retr_in_nall) * deltim
               AKX_retransn_to_frootn_p_acc   (m) = AKX_retransn_to_frootn_p_acc   (m) + npool_to_frootn_p        (m) * f_retr_in_nall           * deltim
               I_frootn_st_p_acc              (m) = I_frootn_st_p_acc          (m) + npool_to_frootn_storage_p(m) * (1._r8 - f_retr_in_nall) * deltim
               AKX_retransn_to_frootn_st_p_acc(m) = AKX_retransn_to_frootn_st_p_acc(m) + npool_to_frootn_storage_p(m) * f_retr_in_nall           * deltim
               IF(woody(ivt) == 1)THEN
                  I_livestemn_p_acc                  (m) = I_livestemn_p_acc              (m) &
                                                         + npool_to_livestemn_p           (m) * (1._r8 - f_retr_in_nall) * deltim
                  AKX_retransn_to_livestemn_p_acc    (m) = AKX_retransn_to_livestemn_p_acc    (m) &
                                                         + npool_to_livestemn_p           (m) * f_retr_in_nall           * deltim
                  I_livestemn_st_p_acc               (m) = I_livestemn_st_p_acc           (m) &
                                                         + npool_to_livestemn_storage_p   (m) * (1._r8 - f_retr_in_nall) * deltim
                  AKX_retransn_to_livestemn_st_p_acc (m) = AKX_retransn_to_livestemn_st_p_acc (m) &
                                                         + npool_to_livestemn_storage_p   (m) * f_retr_in_nall           * deltim
                  I_deadstemn_p_acc                  (m) = I_deadstemn_p_acc              (m) &
                                                         + npool_to_deadstemn_p           (m) * (1._r8 - f_retr_in_nall) * deltim
                  AKX_retransn_to_deadstemn_p_acc    (m) = AKX_retransn_to_deadstemn_p_acc    (m) &
                                                         + npool_to_deadstemn_p           (m) * f_retr_in_nall           * deltim
                  I_deadstemn_st_p_acc               (m) = I_deadstemn_st_p_acc           (m) &
                                                         + npool_to_deadstemn_storage_p   (m) * (1._r8 - f_retr_in_nall) * deltim
                  AKX_retransn_to_deadstemn_st_p_acc (m) = AKX_retransn_to_deadstemn_st_p_acc (m) &
                                                         + npool_to_deadstemn_storage_p   (m) * f_retr_in_nall           * deltim
                  I_livecrootn_p_acc                 (m) = I_livecrootn_p_acc             (m) &
                                                         + npool_to_livecrootn_p          (m) * (1._r8 - f_retr_in_nall) * deltim
                  AKX_retransn_to_livecrootn_p_acc   (m) = AKX_retransn_to_livecrootn_p_acc   (m) &
                                                         + npool_to_livecrootn_p          (m) * f_retr_in_nall           * deltim
                  I_livecrootn_st_p_acc              (m) = I_livecrootn_st_p_acc          (m) &
                                                         + npool_to_livecrootn_storage_p  (m) * (1._r8 - f_retr_in_nall) * deltim
                  AKX_retransn_to_livecrootn_st_p_acc(m) = AKX_retransn_to_livecrootn_st_p_acc(m) &
                                                         + npool_to_livecrootn_storage_p  (m) * f_retr_in_nall           * deltim
                  I_deadcrootn_p_acc                 (m) = I_deadcrootn_p_acc             (m) &
                                                         + npool_to_deadcrootn_p          (m) * (1._r8 - f_retr_in_nall) * deltim
                  AKX_retransn_to_deadcrootn_p_acc   (m) = AKX_retransn_to_deadcrootn_p_acc   (m) &
                                                         + npool_to_deadcrootn_p          (m) * f_retr_in_nall           * deltim
                  I_deadcrootn_st_p_acc              (m) = I_deadcrootn_st_p_acc          (m) &
                                                         + npool_to_deadcrootn_storage_p  (m) * (1._r8 - f_retr_in_nall) * deltim
                  AKX_retransn_to_deadcrootn_st_p_acc(m) = AKX_retransn_to_deadcrootn_st_p_acc(m) &
                                                         + npool_to_deadcrootn_storage_p  (m) * f_retr_in_nall           * deltim
               ENDIF
               IF (ivt >= npcropmin) THEN ! skip 2 generic crops
                  I_livestemn_p_acc                  (m) = I_livestemn_p_acc              (m) &
                                                         + npool_to_livestemn_p           (m) * (1._r8 - f_retr_in_nall) * deltim
                  AKX_retransn_to_livestemn_p_acc    (m) = AKX_retransn_to_livestemn_p_acc(m) &
                                                         + npool_to_livestemn_p           (m) * f_retr_in_nall           * deltim
                  I_livestemn_st_p_acc               (m) = I_livestemn_st_p_acc           (m) &
                                                         + npool_to_livestemn_storage_p   (m) * (1._r8 - f_retr_in_nall) * deltim
                  AKX_retransn_to_livestemn_st_p_acc (m) = AKX_retransn_to_livestemn_st_p_acc(m) &
                                                         + npool_to_livestemn_storage_p   (m) * f_retr_in_nall           * deltim
                  I_grainn_p_acc                     (m) = I_grainn_p_acc                 (m) &
                                                         + npool_to_grainn_p              (m) * (1._r8 - f_retr_in_nall) * deltim
                  AKX_retransn_to_grainn_p_acc       (m) = AKX_retransn_to_grainn_p_acc   (m) &
                                                         + npool_to_grainn_p              (m) * f_retr_in_nall           * deltim
                  I_grainn_st_p_acc                  (m) = I_grainn_st_p_acc              (m) &
                                                         + npool_to_grainn_storage_p      (m) * (1._r8 - f_retr_in_nall) * deltim
                  AKX_retransn_to_grainn_st_p_acc    (m) = AKX_retransn_to_grainn_st_p_acc(m) &
                                                         + npool_to_grainn_storage_p      (m) * f_retr_in_nall           * deltim
               ENDIF
            ENDIF
         ENDIF
         ! move storage pools into transfer pools
         leafn_storage_p(m)  = leafn_storage_p(m)  - leafn_storage_to_xfer_p(m)*deltim
         leafn_xfer_p(m)     = leafn_xfer_p(m)     + leafn_storage_to_xfer_p(m)*deltim
         frootn_storage_p(m) = frootn_storage_p(m) - frootn_storage_to_xfer_p(m)*deltim
         frootn_xfer_p(m)    = frootn_xfer_p(m)    + frootn_storage_to_xfer_p(m)*deltim

         IF (woody(ivt) == 1) THEN
            livestemn_storage_p(m)  = livestemn_storage_p(m)  - livestemn_storage_to_xfer_p(m)*deltim
            livestemn_xfer_p(m)     = livestemn_xfer_p(m)     + livestemn_storage_to_xfer_p(m)*deltim
            deadstemn_storage_p(m)  = deadstemn_storage_p(m)  - deadstemn_storage_to_xfer_p(m)*deltim
            deadstemn_xfer_p(m)     = deadstemn_xfer_p(m)     + deadstemn_storage_to_xfer_p(m)*deltim
            livecrootn_storage_p(m) = livecrootn_storage_p(m) - livecrootn_storage_to_xfer_p(m)*deltim
            livecrootn_xfer_p(m)    = livecrootn_xfer_p(m)    + livecrootn_storage_to_xfer_p(m)*deltim
            deadcrootn_storage_p(m) = deadcrootn_storage_p(m) - deadcrootn_storage_to_xfer_p(m)*deltim
            deadcrootn_xfer_p(m)    = deadcrootn_xfer_p(m)    + deadcrootn_storage_to_xfer_p(m)*deltim
         ENDIF

         IF (ivt >= npcropmin) THEN ! skip 2 generic crops
         ! lines here for consistency; the transfer terms are zero
            livestemn_storage_p(m)  = livestemn_storage_p(m) - livestemn_storage_to_xfer_p(m)*deltim
            livestemn_xfer_p(m)     = livestemn_xfer_p(m)    + livestemn_storage_to_xfer_p(m)*deltim
            grainn_storage_p(m)     = grainn_storage_p(m)    - grainn_storage_to_xfer_p(m)*deltim
            grainn_xfer_p(m)        = grainn_xfer_p(m)       + grainn_storage_to_xfer_p(m)*deltim
         ENDIF

         IF(DEF_USE_SASU)THEN
            AKX_leafn_st_to_leafn_xf_p_acc             (m) = AKX_leafn_st_to_leafn_xf_p_acc          (m) + leafn_storage_to_xfer_p     (m) * deltim
            AKX_leafn_st_exit_p_acc                    (m) = AKX_leafn_st_exit_p_acc                 (m) + leafn_storage_to_xfer_p     (m) * deltim
            AKX_frootn_st_to_frootn_xf_p_acc           (m) = AKX_frootn_st_to_frootn_xf_p_acc        (m) + frootn_storage_to_xfer_p    (m) * deltim
            AKX_frootn_st_exit_p_acc                   (m) = AKX_frootn_st_exit_p_acc                (m) + frootn_storage_to_xfer_p    (m) * deltim
            IF(woody(ivt) == 1) THEN
               AKX_livestemn_st_to_livestemn_xf_p_acc  (m) = AKX_livestemn_st_to_livestemn_xf_p_acc  (m) + livestemn_storage_to_xfer_p (m) * deltim
               AKX_livestemn_st_exit_p_acc             (m) = AKX_livestemn_st_exit_p_acc             (m) + livestemn_storage_to_xfer_p (m) * deltim
               AKX_deadstemn_st_to_deadstemn_xf_p_acc  (m) = AKX_deadstemn_st_to_deadstemn_xf_p_acc  (m) + deadstemn_storage_to_xfer_p (m) * deltim
               AKX_deadstemn_st_exit_p_acc             (m) = AKX_deadstemn_st_exit_p_acc             (m) + deadstemn_storage_to_xfer_p (m) * deltim
               AKX_livecrootn_st_to_livecrootn_xf_p_acc(m) = AKX_livecrootn_st_to_livecrootn_xf_p_acc(m) + livecrootn_storage_to_xfer_p(m) * deltim
               AKX_livecrootn_st_exit_p_acc            (m) = AKX_livecrootn_st_exit_p_acc            (m) + livecrootn_storage_to_xfer_p(m) * deltim
               AKX_deadcrootn_st_to_deadcrootn_xf_p_acc(m) = AKX_deadcrootn_st_to_deadcrootn_xf_p_acc(m) + deadcrootn_storage_to_xfer_p(m) * deltim
               AKX_deadcrootn_st_exit_p_acc            (m) = AKX_deadcrootn_st_exit_p_acc            (m) + deadcrootn_storage_to_xfer_p(m) * deltim
            ENDIF
            IF( ivt >= npcropmin) THEN
               AKX_livestemn_st_to_livestemn_xf_p_acc  (m) = AKX_livestemn_st_to_livestemn_xf_p_acc  (m) + livestemn_storage_to_xfer_p (m) * deltim
               AKX_livestemn_st_exit_p_acc             (m) = AKX_livestemn_st_exit_p_acc             (m) + livestemn_storage_to_xfer_p (m) * deltim
               AKX_grainn_st_to_grainn_xf_p_acc        (m) = AKX_grainn_st_to_grainn_xf_p_acc        (m) + grainn_storage_to_xfer_p    (m) * deltim
               AKX_grainn_st_exit_p_acc                (m) = AKX_grainn_st_exit_p_acc                (m) + grainn_storage_to_xfer_p    (m) * deltim
            ENDIF
         ENDIF
      ENDDO ! END pft loop

   END SUBROUTINE NStateUpdate1

END MODULE MOD_BGC_CNNStateUpdate1
#endif
