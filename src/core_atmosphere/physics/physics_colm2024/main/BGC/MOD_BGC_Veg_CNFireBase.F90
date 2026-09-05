#include <define.h>
#ifdef BGC
MODULE MOD_BGC_Veg_CNFireBase

!---------------------------------------------------------------------------------------------------------------
! !DESCRIPTION:
! This MODULE calculate fire-induced vegetation and litter CN transfers flux, the calculation is based on the fire-induced
! CN loss rates (f). The CN loss rates (f) is calculated from bgc_veg_CNFireLi2016Mod.F90.
!
! !REFERENCES:
! Li, F., Levis, S., and Ward, D. S. 2013a. Quantifying the role of fire in the Earth system - Part 1: Improved global fire
! modeling in the Community Earth System Model (CESM1). Biogeosciences 10:2293-2314.
! Li, F., and Lawrence, D. 2017. Role of fire in the global land water budget during the 20th century through changing
! ecosystems. J. Clim. 30: 1894-1908.
!
! !ORIGINAL:
! The Community Land Model version 5.0 (CLM5)
!
! !REVISION:
! Xingjie Lu, 2021, revised the CLM5 code to be compatible with CoLM code structure.


   USE MOD_Precision
   USE MOD_Const_PFT, only: &
       cc_leaf   , cc_lstem   , cc_dstem , cc_other, fm_leaf, fm_lstem, fm_lroot, fm_root, fm_droot, fm_other, &
       fr_fcel   , fr_flig    , fr_flab  , lf_fcel , lf_flig, lf_flab
   USE MOD_Vars_TimeInvariants, only: &
       cmb_cmplt_fact, patchlatr, borealat, is_cwd, is_litter

   USE MOD_BGC_Vars_TimeVariables, only: &
       ! decomposition pools & fluxes variables (inout)
       decomp_cpools_vr, decomp_npools_vr,  cropf, farea_burned, baf_crop, baf_peatf, totsomc

   USE MOD_BGC_Vars_1DFluxes, only: &
       m_decomp_cpools_to_fire_vr, m_decomp_npools_to_fire_vr, &
       fire_mortality_to_met_c, fire_mortality_to_cel_c, fire_mortality_to_lig_c, fire_mortality_to_cwdc, &
       fire_mortality_to_met_n, fire_mortality_to_cel_n, fire_mortality_to_lig_n, fire_mortality_to_cwdn, &
       somc_fire

   USE MOD_BGC_Vars_PFTimeVariables, only: &
       leafc_p     , leafc_storage_p     , leafc_xfer_p     , frootc_p    , frootc_storage_p    , frootc_xfer_p    , &
       livestemc_p , livestemc_storage_p , livestemc_xfer_p , deadstemc_p , deadstemc_storage_p , deadstemc_xfer_p , &
       livecrootc_p, livecrootc_storage_p, livecrootc_xfer_p, deadcrootc_p, deadcrootc_storage_p, deadcrootc_xfer_p, &
       leafn_p     , leafn_storage_p     , leafn_xfer_p     , frootn_p    , frootn_storage_p    , frootn_xfer_p    , &
       livestemn_p , livestemn_storage_p , livestemn_xfer_p , deadstemn_p , deadstemn_storage_p , deadstemn_xfer_p , &
       livecrootn_p, livecrootn_storage_p, livecrootn_xfer_p, deadcrootn_p, deadcrootn_storage_p, deadcrootn_xfer_p, &
       livecrootn_p, livecrootn_storage_p, livecrootn_xfer_p, deadcrootn_p, deadcrootn_storage_p, deadcrootn_xfer_p, &
       gresp_xfer_p, gresp_storage_p     , retransn_p       , &
       leaf_prof_p , froot_prof_p        , croot_prof_p     , stem_prof_p

   USE MOD_BGC_Vars_1DPFTFluxes, only: &
       m_leafc_to_fire_p       , m_leafc_storage_to_fire_p       , m_leafc_xfer_to_fire_p     , &
       m_frootc_to_fire_p      , m_frootc_storage_to_fire_p      , m_frootc_xfer_to_fire_p    , &
       m_livestemc_to_fire_p   , m_livestemc_storage_to_fire_p   , m_livestemc_xfer_to_fire_p , &
       m_deadstemc_to_fire_p   , m_deadstemc_storage_to_fire_p   , m_deadstemc_xfer_to_fire_p , &
       m_livecrootc_to_fire_p  , m_livecrootc_storage_to_fire_p  , m_livecrootc_xfer_to_fire_p, &
       m_deadcrootc_to_fire_p  , m_deadcrootc_storage_to_fire_p  , m_deadcrootc_xfer_to_fire_p, &
       m_livestemc_to_deadstemc_fire_p, m_livecrootc_to_deadcrootc_fire_p, &
       m_gresp_xfer_to_fire_p  , m_gresp_storage_to_fire_p       , m_retransn_to_fire_p       , &
       m_leafn_to_fire_p       , m_leafn_storage_to_fire_p       , m_leafn_xfer_to_fire_p     , &
       m_frootn_to_fire_p      , m_frootn_storage_to_fire_p      , m_frootn_xfer_to_fire_p    , &
       m_livestemn_to_fire_p   , m_livestemn_storage_to_fire_p   , m_livestemn_xfer_to_fire_p , &
       m_deadstemn_to_fire_p   , m_deadstemn_storage_to_fire_p   , m_deadstemn_xfer_to_fire_p , &
       m_livecrootn_to_fire_p  , m_livecrootn_storage_to_fire_p  , m_livecrootn_xfer_to_fire_p, &
       m_deadcrootn_to_fire_p  , m_deadcrootn_storage_to_fire_p  , m_deadcrootn_xfer_to_fire_p, &
       m_livestemn_to_deadstemn_fire_p, m_livecrootn_to_deadcrootn_fire_p, &

       m_leafc_to_litter_fire_p     , m_leafc_storage_to_litter_fire_p     , m_leafc_xfer_to_litter_fire_p     , &
       m_frootc_to_litter_fire_p    , m_frootc_storage_to_litter_fire_p    , m_frootc_xfer_to_litter_fire_p    , &
       m_livestemc_to_litter_fire_p , m_livestemc_storage_to_litter_fire_p , m_livestemc_xfer_to_litter_fire_p , &
       m_deadstemc_to_litter_fire_p , m_deadstemc_storage_to_litter_fire_p , m_deadstemc_xfer_to_litter_fire_p , &
       m_livecrootc_to_litter_fire_p, m_livecrootc_storage_to_litter_fire_p, m_livecrootc_xfer_to_litter_fire_p, &
       m_deadcrootc_to_litter_fire_p, m_deadcrootc_storage_to_litter_fire_p, m_deadcrootc_xfer_to_litter_fire_p, &
       m_gresp_xfer_to_litter_fire_p, m_gresp_storage_to_litter_fire_p     , m_retransn_to_litter_fire_p       , &
       m_leafn_to_litter_fire_p     , m_leafn_storage_to_litter_fire_p     , m_leafn_xfer_to_litter_fire_p     , &
       m_frootn_to_litter_fire_p    , m_frootn_storage_to_litter_fire_p    , m_frootn_xfer_to_litter_fire_p    , &
       m_livestemn_to_litter_fire_p , m_livestemn_storage_to_litter_fire_p , m_livestemn_xfer_to_litter_fire_p , &
       m_deadstemn_to_litter_fire_p , m_deadstemn_storage_to_litter_fire_p , m_deadstemn_xfer_to_litter_fire_p , &
       m_livecrootn_to_litter_fire_p, m_livecrootn_storage_to_litter_fire_p, m_livecrootn_xfer_to_litter_fire_p, &
       m_deadcrootn_to_litter_fire_p, m_deadcrootn_storage_to_litter_fire_p, m_deadcrootn_xfer_to_litter_fire_p

   USE MOD_Vars_PFTimeInvariants, only: pftfrac

   IMPLICIT NONE

   PUBLIC CNFireFluxes

CONTAINS

   SUBROUTINE CNFireFluxes(i,ps,pe,dlat,nl_soil,ndecomp_pools)

   integer ,intent(in) :: i            ! patch index
   integer ,intent(in) :: ps           ! start pft index
   integer ,intent(in) :: pe           ! END pft index
   real(r8),intent(in) :: dlat         ! latitude (degree)
   integer ,intent(in) :: nl_soil      ! number of total soil layers
   integer ,intent(in) :: ndecomp_pools! number of total soil & litter pools in the decomposition

   ! !LOCAL VARIABLES:
   integer :: j,l   ! indices
   real(r8):: f
   real(r8):: mort
   integer :: ivt, m

   integer, parameter :: lit_fp = 1   ! Pool for liter
   integer, parameter :: cwd_fp = 2   ! Pool for CWD Course woody debris


      DO m = ps, pe
         IF(cropf(i) < 1.0_r8)THEN
            ! For non-crop (bare-soil and natural vegetation)
            f = (farea_burned(i)-baf_crop(i))/(1.0_r8-cropf(i))
         ELSE
            ! For crops
            IF(cropf(i) > 0._r8)THEN
              f = baf_crop(i) /cropf(i)
            ELSE
              f = 0._r8
            ENDIF
         ENDIF

         ! apply this rate to the patch state variables to get flux rates
         ! biomass burning
         ! carbon fluxes
         mort = 1._r8
         m_leafc_to_fire_p(m)               =  leafc_p(m)              * f * cc_leaf(ivt)
         m_leafc_storage_to_fire_p(m)       =  leafc_storage_p(m)      * f * cc_other(ivt)
         m_leafc_xfer_to_fire_p(m)          =  leafc_xfer_p(m)         * f * cc_other(ivt)
         m_livestemc_to_fire_p(m)           =  livestemc_p(m)          * f * cc_lstem(ivt)
         m_livestemc_storage_to_fire_p(m)   =  livestemc_storage_p(m)  * f * cc_other(ivt)
         m_livestemc_xfer_to_fire_p(m)      =  livestemc_xfer_p(m)     * f * cc_other(ivt)
         m_deadstemc_to_fire_p(m)           =  deadstemc_p(m)          * f * cc_dstem(ivt)
         m_deadstemc_storage_to_fire_p(m)   =  deadstemc_storage_p(m)  * f * cc_other(ivt)
         m_deadstemc_xfer_to_fire_p(m)      =  deadstemc_xfer_p(m)     * f * cc_other(ivt)
         m_frootc_to_fire_p(m)              =  frootc_p(m)             * f * 0._r8
         m_frootc_storage_to_fire_p(m)      =  frootc_storage_p(m)     * f * cc_other(ivt)
         m_frootc_xfer_to_fire_p(m)         =  frootc_xfer_p(m)        * f * cc_other(ivt)
         m_livecrootc_to_fire_p(m)          =  livecrootc_p(m)         * f * 0._r8
         m_livecrootc_storage_to_fire_p(m)  =  livecrootc_storage_p(m) * f * cc_other(ivt)
         m_livecrootc_xfer_to_fire_p(m)     =  livecrootc_xfer_p(m)    * f * cc_other(ivt)
         m_deadcrootc_to_fire_p(m)          =  deadcrootc_p(m)         * f * 0._r8
         m_deadcrootc_storage_to_fire_p(m)  =  deadcrootc_storage_p(m) * f*  cc_other(ivt)
         m_deadcrootc_xfer_to_fire_p(m)     =  deadcrootc_xfer_p(m)    * f * cc_other(ivt)
         m_gresp_storage_to_fire_p(m)       =  gresp_storage_p(m)      * f * cc_other(ivt)
         m_gresp_xfer_to_fire_p(m)          =  gresp_xfer_p(m)         * f * cc_other(ivt)


         ! nitrogen fluxes
         m_leafn_to_fire_p(m)               =  leafn_p(m)              * f * cc_leaf(ivt)
         m_leafn_storage_to_fire_p(m)       =  leafn_storage_p(m)      * f * cc_other(ivt)
         m_leafn_xfer_to_fire_p(m)          =  leafn_xfer_p(m)         * f * cc_other(ivt)
         m_livestemn_to_fire_p(m)           =  livestemn_p(m)          * f * cc_lstem(ivt)
         m_livestemn_storage_to_fire_p(m)   =  livestemn_storage_p(m)  * f * cc_other(ivt)
         m_livestemn_xfer_to_fire_p(m)      =  livestemn_xfer_p(m)     * f * cc_other(ivt)
         m_deadstemn_to_fire_p(m)           =  deadstemn_p(m)          * f * cc_dstem(ivt)
         m_deadstemn_storage_to_fire_p(m)   =  deadstemn_storage_p(m)  * f * cc_other(ivt)
         m_deadstemn_xfer_to_fire_p(m)      =  deadstemn_xfer_p(m)     * f * cc_other(ivt)
         m_frootn_to_fire_p(m)              =  frootn_p(m)             * f * 0._r8
         m_frootn_storage_to_fire_p(m)      =  frootn_storage_p(m)     * f * cc_other(ivt)
         m_frootn_xfer_to_fire_p(m)         =  frootn_xfer_p(m)        * f * cc_other(ivt)
         m_livecrootn_to_fire_p(m)          =  livecrootn_p(m)         * f * 0._r8
         m_livecrootn_storage_to_fire_p(m)  =  livecrootn_storage_p(m) * f * cc_other(ivt)
         m_livecrootn_xfer_to_fire_p(m)     =  livecrootn_xfer_p(m)    * f * cc_other(ivt)
         m_deadcrootn_to_fire_p(m)          =  deadcrootn_p(m)         * f * 0._r8
         m_deadcrootn_xfer_to_fire_p(m)     =  deadcrootn_xfer_p(m)    * f * cc_other(ivt)
         m_deadcrootn_storage_to_fire_p(m)  =  deadcrootn_storage_p(m) * f * cc_other(ivt)
         m_retransn_to_fire_p(m)            =  retransn_p(m)           * f * cc_other(ivt)

         ! mortality due to fire
         ! carbon pools
         m_leafc_to_litter_fire_p(m)                   =  leafc_p(m) * f * &
              (1._r8 - cc_leaf(ivt)) * &
              fm_leaf(ivt)
         m_leafc_storage_to_litter_fire_p(m)           =  leafc_storage_p(m) * f * &
              (1._r8 - cc_other(ivt)) * &
              fm_other(ivt)
         m_leafc_xfer_to_litter_fire_p(m)              =  leafc_xfer_p(m) * f * &
              (1._r8 - cc_other(ivt)) * &
              fm_other(ivt)
         m_livestemc_to_litter_fire_p(m)               =  livestemc_p(m) * f * &
              (1._r8 - cc_lstem(ivt)) * &
              fm_droot(ivt)
         m_livestemc_storage_to_litter_fire_p(m)       =  livestemc_storage_p(m) * f * &
              (1._r8 - cc_other(ivt)) * &
              fm_other(ivt)
         m_livestemc_xfer_to_litter_fire_p(m)          =  livestemc_xfer_p(m) * f * &
              (1._r8 - cc_other(ivt)) * &
              fm_other(ivt)
         m_livestemc_to_deadstemc_fire_p(m)            =  livestemc_p(m) * f * &
              (1._r8 - cc_lstem(ivt)) * &
              (fm_lstem(ivt)-fm_droot(ivt))
         m_deadstemc_to_litter_fire_p(m)               =  deadstemc_p(m) * f * m * &
              (1._r8 - cc_dstem(ivt)) * &
              fm_droot(ivt)
         m_deadstemc_storage_to_litter_fire_p(m)       =  deadstemc_storage_p(m) * f * &
              (1._r8 - cc_other(ivt)) * &
              fm_other(ivt)
         m_deadstemc_xfer_to_litter_fire_p(m)          =  deadstemc_xfer_p(m) * f * &
              (1._r8 - cc_other(ivt)) * &
              fm_other(ivt)
         m_frootc_to_litter_fire_p(m)                  =  frootc_p(m)             * f * &
              fm_root(ivt)
         m_frootc_storage_to_litter_fire_p(m)          =  frootc_storage_p(m)     * f * &
              (1._r8- cc_other(ivt)) * &
              fm_other(ivt)
         m_frootc_xfer_to_litter_fire_p(m)             =  frootc_xfer_p(m)        * f * &
              (1._r8- cc_other(ivt)) * &
              fm_other(ivt)
         m_livecrootc_to_litter_fire_p(m)              =  livecrootc_p(m)         * f * &
              fm_droot(ivt)
         m_livecrootc_storage_to_litter_fire_p(m)      =  livecrootc_storage_p(m) * f * &
              (1._r8- cc_other(ivt)) * &
              fm_other(ivt)
         m_livecrootc_xfer_to_litter_fire_p(m)         =  livecrootc_xfer_p(m)    * f * &
              (1._r8- cc_other(ivt)) * &
              fm_other(ivt)
         m_livecrootc_to_deadcrootc_fire_p(m)          =  livecrootc_p(m)         * f * &
              (fm_lroot(ivt)-fm_droot(ivt))
         m_deadcrootc_to_litter_fire_p(m)              =  deadcrootc_p(m)         * f * m * &
              fm_droot(ivt)
         m_deadcrootc_storage_to_litter_fire_p(m)      =  deadcrootc_storage_p(m) * f * &
              (1._r8- cc_other(ivt)) * &
              fm_other(ivt)
         m_deadcrootc_xfer_to_litter_fire_p(m)         =  deadcrootc_xfer_p(m)    * f * &
              (1._r8- cc_other(ivt)) * &
              fm_other(ivt)
         m_gresp_storage_to_litter_fire_p(m)           =  gresp_storage_p(m) * f * &
              (1._r8 - cc_other(ivt)) * &
              fm_other(ivt)
         m_gresp_xfer_to_litter_fire_p(m)              =  gresp_xfer_p(m) * f * &
              (1._r8 - cc_other(ivt)) * &
              fm_other(ivt)


         ! nitrogen pools
         m_leafn_to_litter_fire_p(m)                  =  leafn_p(m) * f * &
              (1._r8 - cc_leaf(ivt)) * &
              fm_leaf(ivt)
         m_leafn_storage_to_litter_fire_p(m)          =  leafn_storage_p(m) * f * &
              (1._r8 - cc_other(ivt)) * &
              fm_other(ivt)
         m_leafn_xfer_to_litter_fire_p(m)             =  leafn_xfer_p(m) * f * &
              (1._r8 - cc_other(ivt)) * &
              fm_other(ivt)
         m_livestemn_to_litter_fire_p(m)              =  livestemn_p(m) * f * &
              (1._r8 - cc_lstem(ivt)) * &
              fm_droot(ivt)
         m_livestemn_storage_to_litter_fire_p(m)      =  livestemn_storage_p(m) * f * &
              (1._r8 - cc_other(ivt)) * &
              fm_other(ivt)
         m_livestemn_xfer_to_litter_fire_p(m)         =  livestemn_xfer_p(m) * f * &
              (1._r8 - cc_other(ivt)) * &
              fm_other(ivt)
         m_livestemn_to_deadstemn_fire_p(m)           =  livestemn_p(m) * f * &
              (1._r8 - cc_lstem(ivt)) * &
              (fm_lstem(ivt)-fm_droot(ivt))
         m_deadstemn_to_litter_fire_p(m)              =  deadstemn_p(m) * f * m * &
              (1._r8 - cc_dstem(ivt)) * &
              fm_droot(ivt)
         m_deadstemn_storage_to_litter_fire_p(m)      =  deadstemn_storage_p(m) * f * &
              (1._r8 - cc_other(ivt)) * &
              fm_other(ivt)
         m_deadstemn_xfer_to_litter_fire_p(m)         =  deadstemn_xfer_p(m) * f * &
              (1._r8 - cc_other(ivt)) * &
              fm_other(ivt)
         m_frootn_to_litter_fire_p(m)                 =  frootn_p(m)             * f * &
              fm_root(ivt)
         m_frootn_storage_to_litter_fire_p(m)         =  frootn_storage_p(m)     * f * &
              (1._r8 - cc_other(ivt)) * &
              fm_other(ivt)
         m_frootn_xfer_to_litter_fire_p(m)            =  frootn_xfer_p(m)        * f * &
              (1._r8 - cc_other(ivt)) * &
              fm_other(ivt)
         m_livecrootn_to_litter_fire_p(m)             =  livecrootn_p(m)         * f * &
              fm_droot(ivt)
         m_livecrootn_storage_to_litter_fire_p(m)     =  livecrootn_storage_p(m) * f * &
              (1._r8 - cc_other(ivt)) * &
              fm_other(ivt)
         m_livecrootn_xfer_to_litter_fire_p(m)        =  livecrootn_xfer_p(m)    * f * &
              (1._r8 - cc_other(ivt)) * &
              fm_other(ivt)
         m_livecrootn_to_deadcrootn_fire_p(m)         =  livecrootn_p(m)         * f * &
              (fm_lroot(ivt)-fm_droot(ivt))
         m_deadcrootn_to_litter_fire_p(m)             =  deadcrootn_p(m)         * f * &
              fm_droot(ivt)
         m_deadcrootn_storage_to_litter_fire_p(m)     =  deadcrootn_storage_p(m) * f * &
              (1._r8 - cc_other(ivt)) * &
              fm_other(ivt)
         m_deadcrootn_xfer_to_litter_fire_p(m)        =  deadcrootn_xfer_p(m)    * f * &
              (1._r8 - cc_other(ivt)) * &
              fm_other(ivt)
         m_retransn_to_litter_fire_p(m)               =  retransn_p(m)           * f * &
              (1._r8 - cc_other(ivt)) * &
              fm_other(ivt)

      ENDDO  ! END of patches loop

      ! fire-induced transfer of carbon and nitrogen pools to litter and cwd

      DO j = 1,nl_soil
         fire_mortality_to_cwdc (j,i) = 0._r8
         fire_mortality_to_cwdn (j,i) = 0._r8
         fire_mortality_to_met_c(j,i) = 0._r8
         DO m = ps, pe
            fire_mortality_to_cwdc(j,i) = fire_mortality_to_cwdc(j,i) + &
                          m_deadstemc_to_litter_fire_p(m) * stem_prof_p(j,m) * pftfrac(m)
            fire_mortality_to_cwdc(j,i) = fire_mortality_to_cwdc(j,i) + &
                          m_deadcrootc_to_litter_fire_p(m) * croot_prof_p(j,m) * pftfrac(m)
            fire_mortality_to_cwdn(j,i) = fire_mortality_to_cwdn(j,i) + &
                          m_deadstemn_to_litter_fire_p(m) * stem_prof_p(j,m) * pftfrac(m)
            fire_mortality_to_cwdn(j,i) = fire_mortality_to_cwdn(j,i) + &
                          m_deadcrootn_to_litter_fire_p(m) * croot_prof_p(j,m) * pftfrac(m)


            fire_mortality_to_cwdc(j,i) = fire_mortality_to_cwdc(j,i) + &
                          m_livestemc_to_litter_fire_p(m) * stem_prof_p(j,m) * pftfrac(m)
            fire_mortality_to_cwdc(j,i) = fire_mortality_to_cwdc(j,i) + &
                          m_livecrootc_to_litter_fire_p(m) * croot_prof_p(j,m) * pftfrac(m)
            fire_mortality_to_cwdn(j,i) = fire_mortality_to_cwdn(j,i) + &
                          m_livestemn_to_litter_fire_p(m) * stem_prof_p(j,m) * pftfrac(m)
            fire_mortality_to_cwdn(j,i) = fire_mortality_to_cwdn(j,i) + &
                          m_livecrootn_to_litter_fire_p(m) * croot_prof_p(j,m) * pftfrac(m)


            fire_mortality_to_met_c(j,i)=fire_mortality_to_met_c(j,i) &
                          +((m_leafc_to_litter_fire_p(m)*lf_flab(ivt) &
                          +  m_leafc_storage_to_litter_fire_p(m) &
                          +  m_leafc_xfer_to_litter_fire_p(m) &
                          +  m_gresp_storage_to_litter_fire_p(m) &
                          +  m_gresp_xfer_to_litter_fire_p(m)) * leaf_prof_p(j,m) &
                          + (m_frootc_to_litter_fire_p(m)*fr_flab(ivt) &
                          +  m_frootc_storage_to_litter_fire_p(m) &
                          +  m_frootc_xfer_to_litter_fire_p(m)) * froot_prof_p(j,m) &
                          + (m_livestemc_storage_to_litter_fire_p(m) &
                          +  m_livestemc_xfer_to_litter_fire_p(m) &
                          +  m_deadstemc_storage_to_litter_fire_p(m) &
                          +  m_deadstemc_xfer_to_litter_fire_p(m)) * stem_prof_p(j,m) &
                          + (m_livecrootc_storage_to_litter_fire_p(m) &
                          +  m_livecrootc_xfer_to_litter_fire_p(m) &
                          +  m_deadcrootc_storage_to_litter_fire_p(m) &
                          +  m_deadcrootc_xfer_to_litter_fire_p(m)) * croot_prof_p(j,m)) * pftfrac(m)
            fire_mortality_to_cel_c(j,i)=fire_mortality_to_cel_c(j,i) &
                          + (m_leafc_to_litter_fire_p(m)*lf_fcel(ivt)*leaf_prof_p(j,m) &
                          +  m_frootc_to_litter_fire_p(m)*fr_fcel(ivt)*froot_prof_p(j,m)) * pftfrac(m)
            fire_mortality_to_lig_c(j,i)=fire_mortality_to_lig_c(j,i) &
                          + (m_leafc_to_litter_fire_p(m)*lf_flig(ivt)*leaf_prof_p(j,m) &
                          + m_frootc_to_litter_fire_p(m)*fr_flig(ivt)*froot_prof_p(j,m)) * pftfrac(m)

            fire_mortality_to_met_n(j,i)=fire_mortality_to_met_n(j,i) &
                          + ((m_leafn_to_litter_fire_p(m)*lf_flab(ivt) &
                          +   m_leafn_storage_to_litter_fire_p(m) &
                          +   m_leafn_xfer_to_litter_fire_p(m) &
                          +   m_retransn_to_litter_fire_p(m)) *leaf_prof_p(j,m) &
                          +  (m_frootn_to_litter_fire_p(m)*fr_flab(ivt) &
                          +   m_frootn_storage_to_litter_fire_p(m) &
                          +   m_frootn_xfer_to_litter_fire_p(m))*froot_prof_p(j,m) &
                          +  (m_livestemn_storage_to_litter_fire_p(m) &
                          +   m_livestemn_xfer_to_litter_fire_p(m) &
                          +   m_deadstemn_storage_to_litter_fire_p(m) &
                          +   m_deadstemn_xfer_to_litter_fire_p(m))* stem_prof_p(j,m)&
                          +  (m_livecrootn_storage_to_litter_fire_p(m) &
                          +   m_livecrootn_xfer_to_litter_fire_p(m) &
                          +   m_deadcrootn_storage_to_litter_fire_p(m) &
                          +   m_deadcrootn_xfer_to_litter_fire_p(m)) * croot_prof_p(j,m)) * pftfrac(m)
            fire_mortality_to_cel_n(j,i)=fire_mortality_to_cel_n(j,i) &
                          +  (m_leafn_to_litter_fire_p(m)*lf_fcel(i)*leaf_prof_p(j,m) &
                          +   m_frootn_to_litter_fire_p(m)*fr_fcel(i)*froot_prof_p(j,m)) * pftfrac(m)
            fire_mortality_to_lig_n(j,i)=fire_mortality_to_lig_n(j,i) &
                          +  (m_leafn_to_litter_fire_p(m)*lf_flig(i)*leaf_prof_p(j,m) &
                          +   m_frootn_to_litter_fire_p(m)*fr_flig(i)*froot_prof_p(j,m)) * pftfrac(m)
         ENDDO
      ENDDO
      !
      ! vertically-resolved decomposing C/N fire loss
      ! column loop
      !
      DO j = 1, nl_soil
            ! carbon fluxes
         DO l = 1, ndecomp_pools
            IF ( is_litter(l) ) THEN
               m_decomp_cpools_to_fire_vr(j,l,i) = decomp_cpools_vr(j,l,i) * f * &
                       cmb_cmplt_fact(lit_fp)
            ENDIF
            IF ( is_cwd(l) ) THEN
               m_decomp_cpools_to_fire_vr(j,l,i) = decomp_cpools_vr(j,l,i) * &
                       (f-baf_crop(i)) * cmb_cmplt_fact(cwd_fp)
            ENDIF
         ENDDO

            ! nitrogen fluxes
         DO l = 1, ndecomp_pools
            IF ( is_litter(l) ) THEN
               m_decomp_npools_to_fire_vr(j,l,i) = decomp_npools_vr(j,l,i) * f * &
                       cmb_cmplt_fact(lit_fp)
            ENDIF
            IF ( is_cwd(l) ) THEN
               m_decomp_npools_to_fire_vr(j,l,i) = decomp_npools_vr(j,l,i) * &
                      (f-baf_crop(i)) * cmb_cmplt_fact(cwd_fp)
            ENDIF
         ENDDO

      ENDDO
      ! Carbon loss due to peat fires
      !
      ! somc_fire is not connected to clm45 soil carbon pool, ie does not decrease
      ! soil carbon b/c clm45 soil carbon was very low in several peatland grids
      IF( patchlatr(i)  <  borealat)THEN
         somc_fire(i)= totsomc(i)*baf_peatf(i)*6.0_r8/33.9_r8
      ELSE
         somc_fire(i)= baf_peatf(i)*2.2e3_r8
      ENDIF

    ! Fang Li has not added aerosol and trace gas emissions due to fire, yet
    ! They will be added here in proportion to the carbon emission
    ! Emission factors differ for various fire types


   END SUBROUTINE CNFireFluxes

END MODULE MOD_BGC_Veg_CNFireBase
#endif
