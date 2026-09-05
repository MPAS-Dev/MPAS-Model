#include <define.h>
#ifdef BGC
MODULE MOD_BGC_Veg_CNGapMortality

!---------------------------------------------------------------------------------------------------
! !DESCRIPTION:
! This module calculates the CN fluxes from vegetation to litterfall due to gap mortality.
! The mortality rates are assumed constant for all vegetation function types.
!
! !ORIGINAL:
! The Community Land Model version 5.0 (CLM5)
!
! !REVISION:
! Xingjie Lu, 2021, revised the CLM5 code to be compatible with CoLM code structure.

   USE MOD_Precision
   USE MOD_Const_PFT, only: lf_flab, lf_fcel, lf_flig, fr_flab, fr_fcel, fr_flig
   USE MOD_BGC_Vars_TimeInvariants, only: &
     ! bgc constants
       am
   USE MOD_Vars_PFTimeInvariants, only: pftclass, pftfrac

   USE MOD_BGC_Vars_1DFluxes, only: &
       ! decomposition carbon flux varables (in)
              gap_mortality_to_met_c, gap_mortality_to_cel_c , &
              gap_mortality_to_lig_c, gap_mortality_to_cwdc  , &

       ! decompositionn nitrogen fluxes variables (inout)
              gap_mortality_to_met_n, gap_mortality_to_cel_n , &
              gap_mortality_to_lig_n, gap_mortality_to_cwdn

   USE MOD_BGC_Vars_1DPFTFluxes, only: &
       ! vegetation carbon flux variables
              m_leafc_to_litter_p        , m_leafc_storage_to_litter_p     , m_leafc_xfer_to_litter_p     , &
              m_frootc_to_litter_p       , m_frootc_storage_to_litter_p    , m_frootc_xfer_to_litter_p    , &
              m_livestemc_to_litter_p    , m_livestemc_storage_to_litter_p , m_livestemc_xfer_to_litter_p , &
              m_deadstemc_to_litter_p    , m_deadstemc_storage_to_litter_p , m_deadstemc_xfer_to_litter_p , &
              m_livecrootc_to_litter_p   , m_livecrootc_storage_to_litter_p, m_livecrootc_xfer_to_litter_p, &
              m_deadcrootc_to_litter_p   , m_deadcrootc_storage_to_litter_p, m_deadcrootc_xfer_to_litter_p, &
              m_gresp_storage_to_litter_p, m_gresp_xfer_to_litter_p        , &

       ! vegetation nitrogen flux variables
              m_leafn_to_litter_p        , m_leafn_storage_to_litter_p     , m_leafn_xfer_to_litter_p     , &
              m_frootn_to_litter_p       , m_frootn_storage_to_litter_p    , m_frootn_xfer_to_litter_p    , &
              m_livestemn_to_litter_p    , m_livestemn_storage_to_litter_p , m_livestemn_xfer_to_litter_p , &
              m_deadstemn_to_litter_p    , m_deadstemn_storage_to_litter_p , m_deadstemn_xfer_to_litter_p , &
              m_livecrootn_to_litter_p   , m_livecrootn_storage_to_litter_p, m_livecrootn_xfer_to_litter_p, &
              m_deadcrootn_to_litter_p   , m_deadcrootn_storage_to_litter_p, m_deadcrootn_xfer_to_litter_p, &
              m_retransn_to_litter_p

   USE MOD_BGC_Vars_PFTimeVariables, only: &
       ! vegetation carbon state variables (inout)
              leafc_p            , leafc_storage_p     , leafc_xfer_p     , &
              frootc_p           , frootc_storage_p    , frootc_xfer_p    , &
              livestemc_p        , livestemc_storage_p , livestemc_xfer_p , &
              deadstemc_p        , deadstemc_storage_p , deadstemc_xfer_p , &
              livecrootc_p       , livecrootc_storage_p, livecrootc_xfer_p, &
              deadcrootc_p       , deadcrootc_storage_p, deadcrootc_xfer_p, &
              gresp_storage_p    , gresp_xfer_p        , &

       ! vegetation nitrogen state variables (inout)
              leafn_p            , leafn_storage_p     , leafn_xfer_p     , &
              frootn_p           , frootn_storage_p    , frootn_xfer_p    , &
              livestemn_p        , livestemn_storage_p , livestemn_xfer_p , &
              deadstemn_p        , deadstemn_storage_p , deadstemn_xfer_p , &
              livecrootn_p       , livecrootn_storage_p, livecrootn_xfer_p, &
              deadcrootn_p       , deadcrootn_storage_p, deadcrootn_xfer_p, &
              retransn_p         , &

       ! profiles
              leaf_prof_p,       stem_prof_p,        froot_prof_p,        croot_prof_p

   IMPLICIT NONE

   PUBLIC CNGapMortality

   PRIVATE CNGap_VegToLitter

CONTAINS

   SUBROUTINE CNGapMortality(i, ps, pe, nl_soil, npcropmin)

   integer ,intent(in) :: i        ! patch index
   integer ,intent(in) :: ps       ! start pft index
   integer ,intent(in) :: pe       ! end pft index
   integer ,intent(in) :: nl_soil  ! number of total soil layers
   integer ,intent(in) :: npcropmin! first crop pft index

   real(r8):: mort             ! rate for fractional mortality (1/s)
   integer :: ivt, m

      DO m = ps , pe
         ivt = pftclass(m)

         mort  = am/(365._r8 * 86400._r8)

         !------------------------------------------------------
         ! pft-level gap mortality carbon fluxes
         !------------------------------------------------------

         ! displayed pools
         m_leafc_to_litter_p             (m) = leafc_p             (m) * mort
         m_frootc_to_litter_p            (m) = frootc_p            (m) * mort
         m_livestemc_to_litter_p         (m) = livestemc_p         (m) * mort
         m_livecrootc_to_litter_p        (m) = livecrootc_p        (m) * mort
         m_deadstemc_to_litter_p         (m) = deadstemc_p         (m) * mort
         m_deadcrootc_to_litter_p        (m) = deadcrootc_p        (m) * mort

         ! storage pools
         m_leafc_storage_to_litter_p     (m) = leafc_storage_p     (m) * mort
         m_frootc_storage_to_litter_p    (m) = frootc_storage_p    (m) * mort
         m_livestemc_storage_to_litter_p (m) = livestemc_storage_p (m) * mort
         m_deadstemc_storage_to_litter_p (m) = deadstemc_storage_p (m) * mort
         m_livecrootc_storage_to_litter_p(m) = livecrootc_storage_p(m) * mort
         m_deadcrootc_storage_to_litter_p(m) = deadcrootc_storage_p(m) * mort
         m_gresp_storage_to_litter_p     (m) = gresp_storage_p     (m) * mort

         ! transfer pools
         m_leafc_xfer_to_litter_p        (m) = leafc_xfer_p        (m) * mort
         m_frootc_xfer_to_litter_p       (m) = frootc_xfer_p       (m) * mort
         m_livestemc_xfer_to_litter_p    (m) = livestemc_xfer_p    (m) * mort
         m_deadstemc_xfer_to_litter_p    (m) = deadstemc_xfer_p    (m) * mort
         m_livecrootc_xfer_to_litter_p   (m) = livecrootc_xfer_p   (m) * mort
         m_deadcrootc_xfer_to_litter_p   (m) = deadcrootc_xfer_p   (m) * mort
         m_gresp_xfer_to_litter_p        (m) = gresp_xfer_p        (m) * mort

         !------------------------------------------------------
         ! pft-level gap mortality nitrogen fluxes
         !------------------------------------------------------

         ! displayed pools
         m_leafn_to_litter_p             (m) = leafn_p             (m) * mort
         m_frootn_to_litter_p            (m) = frootn_p            (m) * mort
         m_livestemn_to_litter_p         (m) = livestemn_p         (m) * mort
         m_livecrootn_to_litter_p        (m) = livecrootn_p        (m) * mort

         m_deadstemn_to_litter_p         (m) = deadstemn_p         (m) * mort
         m_deadcrootn_to_litter_p        (m) = deadcrootn_p        (m) * mort

         IF (ivt < npcropmin) THEN
            m_retransn_to_litter_p       (m) = retransn_p          (m) * mort
         ENDIF

         ! storage pools
         m_leafn_storage_to_litter_p     (m) = leafn_storage_p     (m) * mort
         m_frootn_storage_to_litter_p    (m) = frootn_storage_p    (m) * mort
         m_livestemn_storage_to_litter_p (m) = livestemn_storage_p (m) * mort
         m_deadstemn_storage_to_litter_p (m) = deadstemn_storage_p (m) * mort
         m_livecrootn_storage_to_litter_p(m) = livecrootn_storage_p(m) * mort
         m_deadcrootn_storage_to_litter_p(m) = deadcrootn_storage_p(m) * mort

         ! transfer pools
         m_leafn_xfer_to_litter_p        (m) = leafn_xfer_p        (m) * mort
         m_frootn_xfer_to_litter_p       (m) = frootn_xfer_p       (m) * mort
         m_livestemn_xfer_to_litter_p    (m) = livestemn_xfer_p    (m) * mort
         m_deadstemn_xfer_to_litter_p    (m) = deadstemn_xfer_p    (m) * mort
         m_livecrootn_xfer_to_litter_p   (m) = livecrootn_xfer_p   (m) * mort
         m_deadcrootn_xfer_to_litter_p   (m) = deadcrootn_xfer_p   (m) * mort
      ENDDO

      CALL CNGap_VegToLitter(i, ps, pe, nl_soil)

   END SUBROUTINE CNGapMortality

   SUBROUTINE CNGap_VegToLitter(i, ps, pe, nl_soil)

   integer ,intent(in) :: i
   integer ,intent(in) :: ps
   integer ,intent(in) :: pe
   integer ,intent(in) :: nl_soil

   integer j,m,ivt
   real(r8) :: wtcol

      DO j = 1,nl_soil
         DO m = ps, pe
            ivt = pftclass(m)
            wtcol = pftfrac(m)

            ! leaf gap mortality carbon fluxes
            gap_mortality_to_met_c(j,i) = gap_mortality_to_met_c(j,i) + &
                 m_leafc_to_litter_p(m) * lf_flab(ivt) * wtcol * leaf_prof_p(j,m)
            gap_mortality_to_cel_c(j,i) = gap_mortality_to_cel_c(j,i) + &
                 m_leafc_to_litter_p(m) * lf_fcel(ivt) * wtcol * leaf_prof_p(j,m)
            gap_mortality_to_lig_c(j,i) = gap_mortality_to_lig_c(j,i) + &
                 m_leafc_to_litter_p(m) * lf_flig(ivt) * wtcol * leaf_prof_p(j,m)

            ! fine root gap mortality carbon fluxes
            gap_mortality_to_met_c(j,i) = gap_mortality_to_met_c(j,i) + &
                 m_frootc_to_litter_p(m) * fr_flab(ivt) * wtcol * froot_prof_p(j,m)
            gap_mortality_to_cel_c(j,i) = gap_mortality_to_cel_c(j,i) + &
                 m_frootc_to_litter_p(m) * fr_fcel(ivt) * wtcol * froot_prof_p(j,m)
            gap_mortality_to_lig_c(j,i) = gap_mortality_to_lig_c(j,i) + &
                 m_frootc_to_litter_p(m) * fr_flig(ivt) * wtcol * froot_prof_p(j,m)

            ! wood gap mortality carbon fluxes
            gap_mortality_to_cwdc(j,i)  = gap_mortality_to_cwdc(j,i)  + &
                 (m_livestemc_to_litter_p(m) + m_deadstemc_to_litter_p(m))  * wtcol * stem_prof_p(j,m)
            gap_mortality_to_cwdc(j,i) = gap_mortality_to_cwdc(j,i) + &
                 (m_livecrootc_to_litter_p(m) + m_deadcrootc_to_litter_p(m)) * wtcol * croot_prof_p(j,m)

            ! storage gap mortality carbon fluxes
            gap_mortality_to_met_c(j,i) = gap_mortality_to_met_c(j,i) + &
                 (m_leafc_storage_to_litter_p(m) + m_gresp_storage_to_litter_p(m)) * wtcol * leaf_prof_p(j,m)
            gap_mortality_to_met_c(j,i) = gap_mortality_to_met_c(j,i) + &
                 m_frootc_storage_to_litter_p(m) * wtcol * froot_prof_p(j,m)
            gap_mortality_to_met_c(j,i) = gap_mortality_to_met_c(j,i)  + &
                 (m_livestemc_storage_to_litter_p(m) + m_deadstemc_storage_to_litter_p(m)) * wtcol * stem_prof_p(j,m)
            gap_mortality_to_met_c(j,i) = gap_mortality_to_met_c(j,i) + &
                 (m_livecrootc_storage_to_litter_p(m) + m_deadcrootc_storage_to_litter_p(m)) * wtcol * croot_prof_p(j,m)

            ! transfer gap mortality carbon fluxes
            gap_mortality_to_met_c(j,i) = gap_mortality_to_met_c(j,i) + &
                 (m_leafc_xfer_to_litter_p(m) + m_gresp_xfer_to_litter_p(m)) * wtcol * leaf_prof_p(j,m)
            gap_mortality_to_met_c(j,i) = gap_mortality_to_met_c(j,i) + &
                 m_frootc_xfer_to_litter_p(m) * wtcol * froot_prof_p(j,m)
            gap_mortality_to_met_c(j,i)  = gap_mortality_to_met_c(j,i)  + &
                 (m_livestemc_xfer_to_litter_p(m) + m_deadstemc_xfer_to_litter_p(m))  * wtcol * stem_prof_p(j,m)
            gap_mortality_to_met_c(j,i) = gap_mortality_to_met_c(j,i) + &
                 (m_livecrootc_xfer_to_litter_p(m) + m_deadcrootc_xfer_to_litter_p(m)) * wtcol * croot_prof_p(j,m)

            ! leaf gap mortality nitrogen fluxes
            gap_mortality_to_met_n(j,i) = gap_mortality_to_met_n(j,i) + &
                 m_leafn_to_litter_p(m) * lf_flab(ivt) * wtcol * leaf_prof_p(j,m)
            gap_mortality_to_cel_n(j,i) = gap_mortality_to_cel_n(j,i) + &
                 m_leafn_to_litter_p(m) * lf_fcel(ivt) * wtcol * leaf_prof_p(j,m)
            gap_mortality_to_lig_n(j,i) = gap_mortality_to_lig_n(j,i) + &
                 m_leafn_to_litter_p(m) * lf_flig(ivt) * wtcol * leaf_prof_p(j,m)

            ! fine root litter nitrogen fluxes
            gap_mortality_to_met_n(j,i) = gap_mortality_to_met_n(j,i) + &
                 m_frootn_to_litter_p(m) * fr_flab(ivt) * wtcol * froot_prof_p(j,m)
            gap_mortality_to_cel_n(j,i) = gap_mortality_to_cel_n(j,i) + &
                 m_frootn_to_litter_p(m) * fr_fcel(ivt) * wtcol * froot_prof_p(j,m)
            gap_mortality_to_lig_n(j,i) = gap_mortality_to_lig_n(j,i) + &
                 m_frootn_to_litter_p(m) * fr_flig(ivt) * wtcol * froot_prof_p(j,m)

            ! wood gap mortality nitrogen fluxes
            gap_mortality_to_cwdn(j,i) = gap_mortality_to_cwdn(j,i)  + &
                 (m_livestemn_to_litter_p(m) + m_deadstemn_to_litter_p(m))  * wtcol * stem_prof_p(j,m)
            gap_mortality_to_cwdn(j,i) = gap_mortality_to_cwdn(j,i) + &
                 (m_livecrootn_to_litter_p(m) + m_deadcrootn_to_litter_p(m)) * wtcol * croot_prof_p(j,m)

            ! retranslocated N pool gap mortality fluxes
            gap_mortality_to_met_n(j,i) = gap_mortality_to_met_n(j,i) + &
                 m_retransn_to_litter_p(m) * wtcol * leaf_prof_p(j,m)

            ! storage gap mortality nitrogen fluxes
            gap_mortality_to_met_n(j,i) = gap_mortality_to_met_n(j,i) + &
                 m_leafn_storage_to_litter_p(m) * wtcol * leaf_prof_p(j,m)
            gap_mortality_to_met_n(j,i) = gap_mortality_to_met_n(j,i) + &
                 m_frootn_storage_to_litter_p(m) * wtcol * froot_prof_p(j,m)
            gap_mortality_to_met_n(j,i)  = gap_mortality_to_met_n(j,i) + &
                 (m_livestemn_storage_to_litter_p(m) + m_deadstemn_storage_to_litter_p(m))  * wtcol * stem_prof_p(j,m)
            gap_mortality_to_met_n(j,i) = gap_mortality_to_met_n(j,i) + &
                 (m_livecrootn_storage_to_litter_p(m) + m_deadcrootn_storage_to_litter_p(m)) * wtcol * croot_prof_p(j,m)

            ! transfer gap mortality nitrogen fluxes
            gap_mortality_to_met_n(j,i) = gap_mortality_to_met_n(j,i) + &
                 m_leafn_xfer_to_litter_p(m) * wtcol * leaf_prof_p(j,m)
            gap_mortality_to_met_n(j,i) = gap_mortality_to_met_n(j,i) + &
                 m_frootn_xfer_to_litter_p(m) * wtcol * froot_prof_p(j,m)
            gap_mortality_to_met_n(j,i) = gap_mortality_to_met_n(j,i) + &
                 (m_livestemn_xfer_to_litter_p(m) + m_deadstemn_xfer_to_litter_p(m))  * wtcol * stem_prof_p(j,m)
            gap_mortality_to_met_n(j,i) = gap_mortality_to_met_n(j,i) + &
                 (m_livecrootn_xfer_to_litter_p(m) + m_deadcrootn_xfer_to_litter_p(m)) * wtcol * croot_prof_p(j,m)

         ENDDO
      ENDDO

   END SUBROUTINE CNGap_VegToLitter

END MODULE MOD_BGC_Veg_CNGapMortality
#endif
