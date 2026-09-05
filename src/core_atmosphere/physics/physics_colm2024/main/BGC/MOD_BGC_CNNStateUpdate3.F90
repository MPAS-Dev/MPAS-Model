#include <define.h>
#ifdef BGC

MODULE MOD_BGC_CNNStateUpdate3

!-------------------------------------------------------------------------------------------------------
! !DESCRIPTION
! First updates in vegetation and soil nitrogen. The major updates are included in bgc_CNNStateUpdate1Mod
!  1. Update fire-associated veg and soil(litter) N pool size changes
!  2. Record the accumulated N transfers associated to fire for semi-analytic spinup

! !ORIGINAL:
! The Community Land Model version 5.0 (CLM5.0)

! !REVISION:
! Xingjie Lu, 2022, 1) modify original CLM5 to be compatible with CoLM code structure.
!                   2) Record accumulated fire-associated N transfers for veg and soil N semi-analytic spinup

   USE MOD_Precision
   USE MOD_Namelist, only: DEF_USE_NITRIF, DEF_USE_FIRE
   USE MOD_BGC_Vars_TimeInvariants, only: &
            i_met_lit,i_cel_lit,i_lig_lit ,i_cwd, i_soil1, i_soil2, i_soil3
   USE MOD_BGC_Vars_TimeVariables, only: &
     ! decomposition pools & fluxes variables (inout)
            decomp_npools_vr, sminn_vr, smin_no3_vr, smin_nh4_vr

   USE MOD_BGC_Vars_1DFluxes, only: &
            m_decomp_npools_to_fire_vr, &
            fire_mortality_to_met_n, fire_mortality_to_cel_n, &
            fire_mortality_to_lig_n, fire_mortality_to_cwdn , &

     ! mineral nitrogen pools & fluxes variables (inout)
            sminn_leached_vr, smin_no3_leached_vr, smin_no3_runoff_vr

   USE MOD_BGC_Vars_PFTimeVariables, only: &
     ! vegetation nitrogen state variables (inout)
            leafn_p            , leafn_storage_p     , leafn_xfer_p     , &
            frootn_p           , frootn_storage_p    , frootn_xfer_p    , &
            livestemn_p        , livestemn_storage_p , livestemn_xfer_p , &
            deadstemn_p        , deadstemn_storage_p , deadstemn_xfer_p , &
            livecrootn_p       , livecrootn_storage_p, livecrootn_xfer_p, &
            deadcrootn_p       , deadcrootn_storage_p, deadcrootn_xfer_p, &
            retransn_p

   USE MOD_BGC_Vars_1DPFTFluxes, only: &
     ! vegetation nitrogen flux variables
            m_leafn_to_fire_p        , m_leafn_storage_to_fire_p     , m_leafn_xfer_to_fire_p     , &
            m_frootn_to_fire_p       , m_frootn_storage_to_fire_p    , m_frootn_xfer_to_fire_p    , &
            m_livestemn_to_fire_p    , m_livestemn_storage_to_fire_p , m_livestemn_xfer_to_fire_p , &
            m_deadstemn_to_fire_p    , m_deadstemn_storage_to_fire_p , m_deadstemn_xfer_to_fire_p , &
            m_livecrootn_to_fire_p   , m_livecrootn_storage_to_fire_p, m_livecrootn_xfer_to_fire_p, &
            m_deadcrootn_to_fire_p   , m_deadcrootn_storage_to_fire_p, m_deadcrootn_xfer_to_fire_p, &
            m_livestemn_to_deadstemn_fire_p , m_livecrootn_to_deadcrootn_fire_p , &
            m_retransn_to_fire_p, &

            m_leafn_to_litter_fire_p        , m_leafn_storage_to_litter_fire_p     , m_leafn_xfer_to_litter_fire_p     , &
            m_frootn_to_litter_fire_p       , m_frootn_storage_to_litter_fire_p    , m_frootn_xfer_to_litter_fire_p    , &
            m_livestemn_to_litter_fire_p    , m_livestemn_storage_to_litter_fire_p , m_livestemn_xfer_to_litter_fire_p , &
            m_deadstemn_to_litter_fire_p    , m_deadstemn_storage_to_litter_fire_p , m_deadstemn_xfer_to_litter_fire_p , &
            m_livecrootn_to_litter_fire_p   , m_livecrootn_storage_to_litter_fire_p, m_livecrootn_xfer_to_litter_fire_p, &
            m_deadcrootn_to_litter_fire_p   , m_deadcrootn_storage_to_litter_fire_p, m_deadcrootn_xfer_to_litter_fire_p, &
            m_retransn_to_litter_fire_p


   IMPLICIT NONE

   PUBLIC NStateUpdate3

CONTAINS

   SUBROUTINE NStateUpdate3(i, ps, pe, deltim, nl_soil, ndecomp_pools, dz_soi)

   integer ,intent(in) :: i                ! patch index
   integer ,intent(in) :: ps               ! start pft index
   integer ,intent(in) :: pe               ! END pft index
   real(r8),intent(in) :: deltim           ! time step in seconds

   integer ,intent(in) :: nl_soil          ! number of total soil number
   integer ,intent(in) :: ndecomp_pools    ! number total litter & soil pools
   real(r8),intent(in) :: dz_soi(1:nl_soil)! thicnesses of eacn soil layer

   integer j,l,m


      IF(.not. DEF_USE_NITRIF)THEN
         ! mineral N loss due to leaching
         DO j = 1, nl_soil
            sminn_vr(j,i) = sminn_vr(j,i) - sminn_leached_vr(j,i) * deltim
         ENDDO
      ELSE
         DO j = 1, nl_soil
         ! mineral N loss due to leaching and runoff
            smin_no3_vr(j,i) = max( smin_no3_vr(j,i) &
                             - ( smin_no3_leached_vr(j,i) + smin_no3_runoff_vr(j,i) ) * deltim, 0._r8)

            sminn_vr(j,i) = smin_no3_vr(j,i) + smin_nh4_vr(j,i)
         ENDDO
      ENDIF

         ! column level nitrogen fluxes from fire
         ! patch-level wood to column-level CWD (uncombusted wood)
      IF(DEF_USE_FIRE)THEN
         DO j = 1, nl_soil
            decomp_npools_vr(j,i_cwd,i) = decomp_npools_vr(j,i_cwd,i) &
                                        + fire_mortality_to_cwdn(j,i) * deltim

         ! patch-level wood to column-level litter (uncombusted wood)
            decomp_npools_vr(j,i_met_lit,i) = decomp_npools_vr(j,i_met_lit,i) &
                                            + fire_mortality_to_met_n(j,i)* deltim
            decomp_npools_vr(j,i_cel_lit,i) = decomp_npools_vr(j,i_cel_lit,i) &
                                            + fire_mortality_to_cel_n(j,i)* deltim
            decomp_npools_vr(j,i_lig_lit,i) = decomp_npools_vr(j,i_lig_lit,i) &
                                            + fire_mortality_to_lig_n(j,i)* deltim
         ENDDO

      ! litter and CWD losses to fire
         DO l = 1, ndecomp_pools
            DO j = 1, nl_soil
               decomp_npools_vr(j,l,i) = decomp_npools_vr(j,l,i) &
                                    - m_decomp_npools_to_fire_vr(j,l,i) * deltim
            ENDDO
         ENDDO

         DO m = ps , pe
            !from fire displayed pools
            leafn_p             (m) =  leafn_p            (m) &
                                    - m_leafn_to_fire_p                (m) * deltim
            frootn_p            (m) =  frootn_p           (m) &
                                    - m_frootn_to_fire_p               (m) * deltim
            livestemn_p         (m) =  livestemn_p        (m) &
                                    - m_livestemn_to_fire_p            (m) * deltim
            deadstemn_p         (m) =  deadstemn_p        (m) &
                                    - m_deadstemn_to_fire_p            (m) * deltim
            livecrootn_p        (m) =  livecrootn_p       (m) &
                                    - m_livecrootn_to_fire_p           (m) * deltim
            deadcrootn_p        (m) =  deadcrootn_p       (m) &
                                    - m_deadcrootn_to_fire_p           (m) * deltim

            leafn_p             (m) =  leafn_p            (m) &
                                    - m_leafn_to_litter_fire_p         (m) * deltim
            frootn_p            (m) =  frootn_p           (m) &
                                    - m_frootn_to_litter_fire_p        (m) * deltim
            livestemn_p         (m) =  livestemn_p        (m) &
                                    - m_livestemn_to_litter_fire_p     (m) * deltim &
                                    - m_livestemn_to_deadstemn_fire_p  (m) * deltim
            deadstemn_p         (m) =  deadstemn_p        (m) &
                                    - m_deadstemn_to_litter_fire_p     (m) * deltim &
                                    + m_livestemn_to_deadstemn_fire_p  (m) * deltim
            livecrootn_p        (m) =  livecrootn_p       (m) &
                                    - m_livecrootn_to_litter_fire_p    (m) * deltim &
                                    - m_livecrootn_to_deadcrootn_fire_p(m) * deltim
            deadcrootn_p        (m) =  deadcrootn_p       (m) &
                                    - m_deadcrootn_to_litter_fire_p    (m) * deltim &
                                    + m_livecrootn_to_deadcrootn_fire_p(m) * deltim

            ! storage pools
            leafn_storage_p     (m) = leafn_storage_p     (m) &
                                    - m_leafn_storage_to_fire_p            (m) * deltim
            frootn_storage_p    (m) = frootn_storage_p    (m) &
                                    - m_frootn_storage_to_fire_p           (m) * deltim
            livestemn_storage_p (m) = livestemn_storage_p (m) &
                                    - m_livestemn_storage_to_fire_p        (m) * deltim
            deadstemn_storage_p (m) = deadstemn_storage_p (m) &
                                    - m_deadstemn_storage_to_fire_p        (m) * deltim
            livecrootn_storage_p(m) = livecrootn_storage_p(m) &
                                    - m_livecrootn_storage_to_fire_p       (m) * deltim
            deadcrootn_storage_p(m) = deadcrootn_storage_p(m) &
                                    - m_deadcrootn_storage_to_fire_p       (m) * deltim

            leafn_storage_p     (m) = leafn_storage_p     (m) &
                                    - m_leafn_storage_to_litter_fire_p     (m) * deltim
            frootn_storage_p    (m) = frootn_storage_p    (m) &
                                    - m_frootn_storage_to_litter_fire_p    (m) * deltim
            livestemn_storage_p (m) = livestemn_storage_p (m) &
                                    - m_livestemn_storage_to_litter_fire_p (m) * deltim
            deadstemn_storage_p (m) = deadstemn_storage_p (m) &
                                    - m_deadstemn_storage_to_litter_fire_p (m) * deltim
            livecrootn_storage_p(m) = livecrootn_storage_p(m) &
                                    - m_livecrootn_storage_to_litter_fire_p(m) * deltim
            deadcrootn_storage_p(m) = deadcrootn_storage_p(m) &
                                    - m_deadcrootn_storage_to_litter_fire_p(m) * deltim


            ! transfer pools
            leafn_xfer_p        (m) = leafn_xfer_p        (m) &
                                    - m_leafn_xfer_to_fire_p               (m) * deltim
            frootn_xfer_p       (m) = frootn_xfer_p       (m) &
                                    - m_frootn_xfer_to_fire_p              (m) * deltim
            livestemn_xfer_p    (m) = livestemn_xfer_p    (m) &
                                    - m_livestemn_xfer_to_fire_p           (m) * deltim
            deadstemn_xfer_p    (m) = deadstemn_xfer_p    (m) &
                                    - m_deadstemn_xfer_to_fire_p           (m) * deltim
            livecrootn_xfer_p   (m) = livecrootn_xfer_p   (m) &
                                    - m_livecrootn_xfer_to_fire_p          (m) * deltim
            deadcrootn_xfer_p   (m) = deadcrootn_xfer_p   (m) &
                                    - m_deadcrootn_xfer_to_fire_p          (m) * deltim

            leafn_xfer_p        (m) = leafn_xfer_p        (m) &
                                    - m_leafn_xfer_to_litter_fire_p        (m) * deltim
            frootn_xfer_p       (m) = frootn_xfer_p       (m) &
                                    - m_frootn_xfer_to_litter_fire_p       (m) * deltim
            livestemn_xfer_p    (m) = livestemn_xfer_p    (m) &
                                    - m_livestemn_xfer_to_litter_fire_p    (m) * deltim
            deadstemn_xfer_p    (m) = deadstemn_xfer_p    (m) &
                                    - m_deadstemn_xfer_to_litter_fire_p    (m) * deltim
            livecrootn_xfer_p   (m) = livecrootn_xfer_p   (m) &
                                    - m_livecrootn_xfer_to_litter_fire_p   (m) * deltim
            deadcrootn_xfer_p   (m) = deadcrootn_xfer_p   (m) &
                                    - m_deadcrootn_xfer_to_litter_fire_p   (m) * deltim

           ! retranslocated N pool
            retransn_p          (m) = retransn_p          (m) &
                                    - m_retransn_to_fire_p                 (m) * deltim
            retransn_p          (m) = retransn_p          (m) &
                                    - m_retransn_to_litter_fire_p          (m) * deltim
         ENDDO
      ENDIF

   END SUBROUTINE NStateUpdate3

END MODULE MOD_BGC_CNNStateUpdate3
#endif
