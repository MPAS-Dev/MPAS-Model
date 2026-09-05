#include <define.h>
#ifdef BGC
MODULE MOD_BGC_CNNStateUpdate2

!---------------------------------------------------------------------------------------------------------
! !DESCRIPTION
! First updates in vegetation and soil nitrogen. The major updates are included in bgc_CNNStateUpdate1Mod
!  1. Update gap-mortality-associated veg and soil N pool size changes
!  2. Record the accumulated N transfers associated to gap-mortality for semi-analytic spinup

! !ORIGINAL:
! The Community Land Model version 5.0 (CLM5.0)

! REVISION:
! Xingjie Lu, 2022, 1) modify original CLM5 to be compatible with CoLM code structure.
!                   2) Record the accumulated gap-mortality-associated N transfers for veg and soil N semi-analytic spinup

   USE MOD_Precision
   USE MOD_Namelist, only: DEF_USE_SASU, DEF_USE_DiagMatrix
   USE MOD_BGC_Vars_TimeInvariants, only: &
            i_met_lit,i_cel_lit,i_lig_lit ,i_cwd, i_soil1, i_soil2,i_soil3
   USE MOD_BGC_Vars_TimeVariables, only: &
     ! decompositionn nitrogen pools & fluxes variables (inout)
            decomp_npools_vr, &
            I_met_n_vr_acc     , I_cel_n_vr_acc      , I_lig_n_vr_acc   , I_cwd_n_vr_acc

   USE MOD_BGC_Vars_1DFluxes, only: &
            gap_mortality_to_met_n, gap_mortality_to_cel_n , &
            gap_mortality_to_lig_n, gap_mortality_to_cwdn

   USE MOD_BGC_Vars_PFTimeVariables, only: &
     ! vegetation nitrogen state variables (inout)
            leafn_p            , leafn_storage_p     , leafn_xfer_p     , &
            frootn_p           , frootn_storage_p    , frootn_xfer_p    , &
            livestemn_p        , livestemn_storage_p , livestemn_xfer_p , &
            deadstemn_p        , deadstemn_storage_p , deadstemn_xfer_p , &
            livecrootn_p       , livecrootn_storage_p, livecrootn_xfer_p, &
            deadcrootn_p       , deadcrootn_storage_p, deadcrootn_xfer_p, &
            retransn_p         , npool_p, grainn_p, grainn_storage_p, grainn_xfer_p, cropseedn_deficit_p, &

     ! SASU variables
            AKX_leafn_exit_p_acc     , AKX_leafn_st_exit_p_acc     , AKX_leafn_xf_exit_p_acc     , &
            AKX_frootn_exit_p_acc    , AKX_frootn_st_exit_p_acc    , AKX_frootn_xf_exit_p_acc    , &
            AKX_livestemn_exit_p_acc , AKX_livestemn_st_exit_p_acc , AKX_livestemn_xf_exit_p_acc , &
            AKX_deadstemn_exit_p_acc , AKX_deadstemn_st_exit_p_acc , AKX_deadstemn_xf_exit_p_acc , &
            AKX_livecrootn_exit_p_acc, AKX_livecrootn_st_exit_p_acc, AKX_livecrootn_xf_exit_p_acc, &
            AKX_deadcrootn_exit_p_acc, AKX_deadcrootn_st_exit_p_acc, AKX_deadcrootn_xf_exit_p_acc, &
            AKX_retransn_exit_p_acc

   USE MOD_BGC_Vars_1DPFTFluxes, only: &
     ! vegetation nitrogen flux variables
            m_leafn_to_litter_p        , m_leafn_storage_to_litter_p     , m_leafn_xfer_to_litter_p     , &
            m_frootn_to_litter_p       , m_frootn_storage_to_litter_p    , m_frootn_xfer_to_litter_p    , &
            m_livestemn_to_litter_p    , m_livestemn_storage_to_litter_p , m_livestemn_xfer_to_litter_p , &
            m_deadstemn_to_litter_p    , m_deadstemn_storage_to_litter_p , m_deadstemn_xfer_to_litter_p , &
            m_livecrootn_to_litter_p   , m_livecrootn_storage_to_litter_p, m_livecrootn_xfer_to_litter_p, &
            m_deadcrootn_to_litter_p   , m_deadcrootn_storage_to_litter_p, m_deadcrootn_xfer_to_litter_p, &
            m_retransn_to_litter_p

   USE MOD_Vars_PFTimeInvariants, only: pftfrac
   IMPLICIT NONE

   PUBLIC NStateUpdate2

CONTAINS

   SUBROUTINE NStateUpdate2(i, ps, pe, deltim, nl_soil, dz_soi)

   integer ,intent(in) :: i                 ! patch index
   integer ,intent(in) :: ps                ! start pft index
   integer ,intent(in) :: pe                ! END pft index
   real(r8),intent(in) :: deltim            ! time step in seconds
   integer ,intent(in) :: nl_soil           ! number of total soil layers
   real(r8),intent(in) :: dz_soi(1:nl_soil) ! thicknesses of each soil layer

   integer j, m

    ! column-level nitrogen fluxes from gap-phase mortality
      DO j = 1, nl_soil
         decomp_npools_vr(j,i_met_lit,i) = &
                 decomp_npools_vr(j,i_met_lit,i) + gap_mortality_to_met_n(j,i) * deltim
         decomp_npools_vr(j,i_cel_lit,i) = &
                 decomp_npools_vr(j,i_cel_lit,i) + gap_mortality_to_cel_n(j,i) * deltim
         decomp_npools_vr(j,i_lig_lit,i) = &
                 decomp_npools_vr(j,i_lig_lit,i) + gap_mortality_to_lig_n(j,i) * deltim
         decomp_npools_vr(j,i_cwd,i)     = &
                 decomp_npools_vr(j,i_cwd,i)     + gap_mortality_to_cwdn(j,i)  * deltim
      ENDDO
      IF(DEF_USE_SASU .or. DEF_USE_DiagMatrix)THEN
         DO j=1,nl_soil
            I_met_n_vr_acc(j,i) = I_met_n_vr_acc(j,i) + gap_mortality_to_met_n(j,i) * deltim
            I_cel_n_vr_acc(j,i) = I_cel_n_vr_acc(j,i) + gap_mortality_to_cel_n(j,i) * deltim
            I_lig_n_vr_acc(j,i) = I_lig_n_vr_acc(j,i) + gap_mortality_to_lig_n(j,i) * deltim
            I_cwd_n_vr_acc(j,i) = I_cwd_n_vr_acc(j,i) + gap_mortality_to_cwdn (j,i) * deltim
         ENDDO
      ENDIF
        ! patch -level nitrogen fluxes from gap-phase mortality

           ! displayed pools
      DO m = ps, pe
         leafn_p             (m) = leafn_p             (m) &
                                 - m_leafn_to_litter_p             (m) * deltim
         frootn_p            (m) = frootn_p            (m) &
                                 - m_frootn_to_litter_p            (m) * deltim
         livestemn_p         (m) = livestemn_p         (m) &
                                 - m_livestemn_to_litter_p         (m) * deltim
         deadstemn_p         (m) = deadstemn_p         (m) &
                                 - m_deadstemn_to_litter_p         (m) * deltim
         livecrootn_p        (m) = livecrootn_p        (m) &
                                 - m_livecrootn_to_litter_p        (m) * deltim
         deadcrootn_p        (m) = deadcrootn_p        (m) &
                                 - m_deadcrootn_to_litter_p        (m) * deltim
         retransn_p          (m) = retransn_p          (m) &
                                 - m_retransn_to_litter_p          (m) * deltim

     ! storage pools
         leafn_storage_p     (m) = leafn_storage_p     (m) &
                                 - m_leafn_storage_to_litter_p     (m) * deltim
         frootn_storage_p    (m) = frootn_storage_p    (m) &
                                 - m_frootn_storage_to_litter_p    (m) * deltim
         livestemn_storage_p (m) = livestemn_storage_p (m) &
                                 - m_livestemn_storage_to_litter_p (m) * deltim
         deadstemn_storage_p (m) = deadstemn_storage_p (m) &
                                 - m_deadstemn_storage_to_litter_p (m) * deltim
         livecrootn_storage_p(m) = livecrootn_storage_p(m) &
                                 - m_livecrootn_storage_to_litter_p(m) * deltim
         deadcrootn_storage_p(m) = deadcrootn_storage_p(m) &
                                 - m_deadcrootn_storage_to_litter_p(m) * deltim

     ! transfer pools
         leafn_xfer_p        (m) = leafn_xfer_p        (m) &
                                 - m_leafn_xfer_to_litter_p        (m) * deltim
         frootn_xfer_p       (m) = frootn_xfer_p       (m) &
                                 - m_frootn_xfer_to_litter_p       (m) * deltim
         livestemn_xfer_p    (m) = livestemn_xfer_p    (m) &
                                 - m_livestemn_xfer_to_litter_p    (m) * deltim
         deadstemn_xfer_p    (m) = deadstemn_xfer_p    (m) &
                                 - m_deadstemn_xfer_to_litter_p    (m) * deltim
         livecrootn_xfer_p   (m) = livecrootn_xfer_p   (m) &
                                 - m_livecrootn_xfer_to_litter_p   (m) * deltim
         deadcrootn_xfer_p   (m) = deadcrootn_xfer_p   (m) &
                                 - m_deadcrootn_xfer_to_litter_p   (m) * deltim

         IF(DEF_USE_SASU .or. DEF_USE_DiagMatrix)THEN
            AKX_leafn_exit_p_acc         (m) = AKX_leafn_exit_p_acc         (m) + m_leafn_to_litter_p             (m) * deltim
            AKX_frootn_exit_p_acc        (m) = AKX_frootn_exit_p_acc        (m) + m_frootn_to_litter_p            (m) * deltim
            AKX_livestemn_exit_p_acc     (m) = AKX_livestemn_exit_p_acc     (m) + m_livestemn_to_litter_p         (m) * deltim
            AKX_deadstemn_exit_p_acc     (m) = AKX_deadstemn_exit_p_acc     (m) + m_deadstemn_to_litter_p         (m) * deltim
            AKX_livecrootn_exit_p_acc    (m) = AKX_livecrootn_exit_p_acc    (m) + m_livecrootn_to_litter_p        (m) * deltim
            AKX_deadcrootn_exit_p_acc    (m) = AKX_deadcrootn_exit_p_acc    (m) + m_deadcrootn_to_litter_p        (m) * deltim
            AKX_retransn_exit_p_acc      (m) = AKX_retransn_exit_p_acc      (m) + m_retransn_to_litter_p          (m) * deltim

            AKX_leafn_st_exit_p_acc      (m) = AKX_leafn_st_exit_p_acc      (m) + m_leafn_storage_to_litter_p     (m) * deltim
            AKX_frootn_st_exit_p_acc     (m) = AKX_frootn_st_exit_p_acc     (m) + m_frootn_storage_to_litter_p    (m) * deltim
            AKX_livestemn_st_exit_p_acc  (m) = AKX_livestemn_st_exit_p_acc  (m) + m_livestemn_storage_to_litter_p (m) * deltim
            AKX_deadstemn_st_exit_p_acc  (m) = AKX_deadstemn_st_exit_p_acc  (m) + m_deadstemn_storage_to_litter_p (m) * deltim
            AKX_livecrootn_st_exit_p_acc (m) = AKX_livecrootn_st_exit_p_acc (m) + m_livecrootn_storage_to_litter_p(m) * deltim
            AKX_deadcrootn_st_exit_p_acc (m) = AKX_deadcrootn_st_exit_p_acc (m) + m_deadcrootn_storage_to_litter_p(m) * deltim

            AKX_leafn_xf_exit_p_acc      (m) = AKX_leafn_xf_exit_p_acc      (m) + m_leafn_xfer_to_litter_p        (m) * deltim
            AKX_frootn_xf_exit_p_acc     (m) = AKX_frootn_xf_exit_p_acc     (m) + m_frootn_xfer_to_litter_p       (m) * deltim
            AKX_livestemn_xf_exit_p_acc  (m) = AKX_livestemn_xf_exit_p_acc  (m) + m_livestemn_xfer_to_litter_p    (m) * deltim
            AKX_deadstemn_xf_exit_p_acc  (m) = AKX_deadstemn_xf_exit_p_acc  (m) + m_deadstemn_xfer_to_litter_p    (m) * deltim
            AKX_livecrootn_xf_exit_p_acc (m) = AKX_livecrootn_xf_exit_p_acc (m) + m_livecrootn_xfer_to_litter_p   (m) * deltim
            AKX_deadcrootn_xf_exit_p_acc (m) = AKX_deadcrootn_xf_exit_p_acc (m) + m_deadcrootn_xfer_to_litter_p   (m) * deltim
         ENDIF
      ENDDO

   END SUBROUTINE NStateUpdate2

END MODULE MOD_BGC_CNNStateUpdate2
#endif
