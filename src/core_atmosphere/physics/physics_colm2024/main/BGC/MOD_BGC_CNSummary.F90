#include <define.h>
#ifdef BGC
MODULE MOD_BGC_CNSummary

!------------------------------------------------------------------------------------------
! !DESCRIPTION:
! bgc_CNSummaryMod calculates following statistics:
! 1) total CN fluxes and pool sizes from individual contribution of each pool.
! 2) aggregate the PFT-level fluxes and pool sizes into Column-level fluxes and pool sizes.
! 3) PFT-level leaf pool sizes, GPP and crop production.
!
! !ORIGINAL:
! The Community Land Model version 5.0 (CLM5.0)
!
! !REVISION:
! Xingjie Lu, 2022, modify original CLM5 to be compatible with CoLM code structure.

   USE MOD_Precision
   USE MOD_Namelist, only: DEF_USE_NITRIF, DEF_USE_DiagMatrix
   USE MOD_Vars_PFTimeInvariants, only: pftclass
   USE MOD_Vars_PFTimeVariables, only:irrig_method_p, lai_p
   USE MOD_Vars_TimeInvariants, only: BD_all
   USE MOD_BGC_Vars_TimeVariables, only: &
       totlitc, totsomc, totcwdc, decomp_cpools, decomp_cpools_vr, ctrunc_soil,ctrunc_veg, ctrunc_vr, &
       totlitn, totsomn, totcwdn, decomp_npools, decomp_npools_vr, ntrunc_soil,ntrunc_veg, ntrunc_vr, &
       totvegc, totvegn, totcolc, totcoln, sminn, sminn_vr, totsoiln_vr, &
       leafc, frootc, livestemc, deadstemc, livecrootc, deadcrootc, leafc_storage, frootc_storage, livestemc_storage, &
       deadstemc_storage, livecrootc_storage, deadcrootc_storage, leafc_xfer, frootc_xfer, livestemc_xfer, &
       deadstemc_xfer, livecrootc_xfer, deadcrootc_xfer, xsmrpool, &
       leafcCap, frootcCap, livestemcCap, deadstemcCap, livecrootcCap, deadcrootcCap, leafc_storageCap, frootc_storageCap, &
       livestemc_storageCap, deadstemc_storageCap, livecrootc_storageCap, deadcrootc_storageCap, leafc_xferCap, &
       frootc_xferCap, livestemc_xferCap, deadstemc_xferCap, livecrootc_xferCap, deadcrootc_xferCap, &
#ifdef CROP
       grainc, grainc_storage, grainc_xfer, &
       cropseedc_deficit, cropprod1c, cphase, hui, vf, gddplant, gddmaturity, &
       manunitro, fertnitro_corn, fertnitro_swheat, fertnitro_wwheat, fertnitro_soybean, &
       fertnitro_cotton, fertnitro_rice1, fertnitro_rice2, fertnitro_sugarcane, &
       grainn, grainn_storage, grainn_xfer, plantdate, &
#endif
       leafn, frootn, livestemn, deadstemn, livecrootn, deadcrootn, leafn_storage, frootn_storage, livestemn_storage, &
       deadstemn_storage, livecrootn_storage, deadcrootn_storage, leafn_xfer, frootn_xfer, livestemn_xfer, &
       deadstemn_xfer, livecrootn_xfer, deadcrootn_xfer, retransn, downreg, lag_npp, &
       leafnCap, frootnCap, livestemnCap, deadstemnCap, livecrootnCap, deadcrootnCap, leafn_storageCap, frootn_storageCap, &
       livestemn_storageCap, deadstemn_storageCap, livecrootn_storageCap, deadcrootn_storageCap, leafn_xferCap, &
       frootn_xferCap, livestemn_xferCap, deadstemn_xferCap, livecrootn_xferCap, deadcrootn_xferCap
   USE MOD_BGC_Vars_TimeInvariants, only: &
       is_litter, is_soil, is_cwd, nfix_timeconst
   USE MOD_BGC_Vars_PFTimeVariables, only: &
       leafc_p, frootc_p, livestemc_p, deadstemc_p, livecrootc_p, deadcrootc_p, &
       leafc_storage_p, frootc_storage_p, livestemc_storage_p, &
       deadstemc_storage_p, livecrootc_storage_p, deadcrootc_storage_p, gresp_storage_p, &
       leafc_xfer_p, frootc_xfer_p, livestemc_xfer_p, &
       deadstemc_xfer_p, livecrootc_xfer_p, deadcrootc_xfer_p, gresp_xfer_p, xsmrpool_p, &
       leafcCap_p, frootcCap_p, livestemcCap_p, deadstemcCap_p, livecrootcCap_p, deadcrootcCap_p, &
       leafc_storageCap_p, frootc_storageCap_p, livestemc_storageCap_p, &
       deadstemc_storageCap_p, livecrootc_storageCap_p, deadcrootc_storageCap_p, &
       leafc_xferCap_p, frootc_xferCap_p, livestemc_xferCap_p, &
       deadstemc_xferCap_p, livecrootc_xferCap_p, deadcrootc_xferCap_p, &
#ifdef CROP
       grainc_p, grainc_storage_p, grainc_xfer_p, &
#endif
       ctrunc_p, totvegc_p, &
       cropseedc_deficit_p, cropprod1c_p, cpool_p, &
#ifdef CROP
       plantdate_p, cphase_p, manunitro_p, fertnitro_p, hui_p, gddmaturity_p, gddplant_p, vf_p, &
       grainn_p, grainn_storage_p, grainn_xfer_p, cropseedn_deficit_p, &
#endif
       leafn_p, frootn_p, livestemn_p, deadstemn_p, livecrootn_p, deadcrootn_p, &
       leafn_storage_p, frootn_storage_p, livestemn_storage_p, &
       deadstemn_storage_p, livecrootn_storage_p, deadcrootn_storage_p, &
       leafn_xfer_p, frootn_xfer_p, livestemn_xfer_p, &
       deadstemn_xfer_p, livecrootn_xfer_p, deadcrootn_xfer_p, retransn_p, npool_p, &
       leafnCap_p, frootnCap_p, livestemnCap_p, deadstemnCap_p, livecrootnCap_p, deadcrootnCap_p, &
       leafn_storageCap_p, frootn_storageCap_p, livestemn_storageCap_p, &
       deadstemn_storageCap_p, livecrootn_storageCap_p, deadcrootn_storageCap_p, &
       leafn_xferCap_p, frootn_xferCap_p, livestemn_xferCap_p, &
       deadstemn_xferCap_p, livecrootn_xferCap_p, deadcrootn_xferCap_p, &
       ntrunc_p, totvegn_p, downreg_p
   USE MOD_Vars_PFTimeInvariants,  only: pftfrac
   USE MOD_BGC_Vars_1DFluxes, only: &
       gpp_enftemp, gpp_enfboreal, gpp_dnfboreal, gpp_ebftrop, gpp_ebftemp, gpp_dbftrop, gpp_dbftemp, &
       gpp_dbfboreal, gpp_ebstemp, gpp_dbstemp, gpp_dbsboreal, gpp_c3arcgrass, gpp_c3grass, gpp_c4grass, &
       leafc_enftemp, leafc_enfboreal, leafc_dnfboreal, leafc_ebftrop, leafc_ebftemp, leafc_dbftrop, leafc_dbftemp, &
       leafc_dbfboreal, leafc_ebstemp, leafc_dbstemp, leafc_dbsboreal, leafc_c3arcgrass, leafc_c3grass, leafc_c4grass, &
       decomp_hr, decomp_hr_vr, gpp, ar, er, supplement_to_sminn, supplement_to_sminn_vr, &
       npp_enftemp, npp_enfboreal, npp_dnfboreal, npp_ebftrop, npp_ebftemp, npp_dbftrop, npp_dbftemp, &
       npp_dbfboreal, npp_ebstemp, npp_dbstemp, npp_dbsboreal, npp_c3arcgrass, npp_c3grass, npp_c4grass, &
       npptoleafc_enftemp, npptoleafc_enfboreal, npptoleafc_dnfboreal, npptoleafc_ebftrop, &
       npptoleafc_ebftemp, npptoleafc_dbftrop, npptoleafc_dbftemp, npptoleafc_dbfboreal, npptoleafc_ebstemp, &
       npptoleafc_dbstemp, npptoleafc_dbsboreal, npptoleafc_c3arcgrass, npptoleafc_c3grass, npptoleafc_c4grass, &
#ifdef CROP
       cropprod1c_loss, grainc_to_cropprodc, grainc_to_seed, grainn_to_cropprodn, &
#endif
       sminn_leached, sminn_leached_vr, smin_no3_leached, smin_no3_leached_vr, smin_no3_runoff, smin_no3_runoff_vr, &
       f_n2o_nit, f_n2o_nit_vr, decomp_cpools_transport_tendency, decomp_npools_transport_tendency, &
       denit, f_denit_vr, fire_closs, hrv_xsmrpool_to_atm, som_c_leached, som_n_leached, sminn_to_denit_excess_vr, &
       sminn_to_denit_decomp_vr
   USE MOD_BGC_Vars_1DPFTFluxes, only: &
       psn_to_cpool_p, leaf_mr_p, froot_mr_p, livestem_mr_p, livecroot_mr_p, &
       cpool_leaf_gr_p, cpool_froot_gr_p, cpool_livestem_gr_p, cpool_deadstem_gr_p, &
       cpool_livecroot_gr_p, cpool_deadcroot_gr_p, transfer_leaf_gr_p, transfer_froot_gr_p, &
       transfer_livestem_gr_p, transfer_deadstem_gr_p, &
       transfer_livecroot_gr_p, transfer_deadcroot_gr_p, &
       cpool_leaf_storage_gr_p, cpool_froot_storage_gr_p, &
       cpool_livestem_storage_gr_p, cpool_deadstem_storage_gr_p, &
       cpool_livecroot_storage_gr_p, cpool_deadcroot_storage_gr_p, &
       grain_mr_p, xsmrpool_to_atm_p, cpool_grain_gr_p, &
       transfer_grain_gr_p, cpool_grain_storage_gr_p, soil_change_p, &
       fire_closs_p, hrv_xsmrpool_to_atm_p, &
       cpool_to_leafc_p, cpool_to_leafc_storage_p, &
#ifdef CROP
       cropprod1c_loss_p, grainc_to_seed_p, grainc_to_food_p, grainn_to_food_p, &
#endif
       m_leafc_to_fire_p, m_leafc_storage_to_fire_p, m_leafc_xfer_to_fire_p, &
       m_frootc_to_fire_p, m_frootc_storage_to_fire_p, m_frootc_xfer_to_fire_p, &
       m_livestemc_to_fire_p, m_livestemc_storage_to_fire_p, m_livestemc_xfer_to_fire_p, &
       m_deadstemc_to_fire_p, m_deadstemc_storage_to_fire_p, m_deadstemc_xfer_to_fire_p, &
       m_livecrootc_to_fire_p, m_livecrootc_storage_to_fire_p, m_livecrootc_xfer_to_fire_p, &
       m_deadcrootc_to_fire_p, m_deadcrootc_storage_to_fire_p, m_deadcrootc_xfer_to_fire_p, &
       m_gresp_storage_to_fire_p, m_gresp_xfer_to_fire_p
   USE MOD_Vars_TimeVariables, only: &
       irrig_method_corn  , irrig_method_swheat, irrig_method_wwheat, irrig_method_soybean  , &
       irrig_method_cotton, irrig_method_rice1 , irrig_method_rice2 , irrig_method_sugarcane, &
       lai_enftemp, lai_enfboreal, lai_dnfboreal, lai_ebftrop, lai_ebftemp, lai_dbftrop, lai_dbftemp, &
       lai_dbfboreal, lai_ebstemp, lai_dbstemp, lai_dbsboreal, lai_c3arcgrass, lai_c3grass, lai_c4grass

   USE MOD_Vars_TimeInvariants, only: patchclass
   USE MOD_Vars_Global, only: spval
   USE MOD_MPAS_MPI

   IMPLICIT NONE

   PUBLIC CNDriverSummarizeStates
   PUBLIC CNDriverSummarizeFluxes

   PRIVATE soilbiogeochem_carbonstate_summary
   PRIVATE soilbiogeochem_nitrogenstate_summary
   PRIVATE cnveg_carbonstate_summary
   PRIVATE cnveg_nitrogenstate_summary
   PRIVATE soilbiogeochem_carbonflux_summary
   PRIVATE soilbiogeochem_nitrogenflux_summary
   PRIVATE cnveg_carbonflux_summary
   PRIVATE cnveg_nitrogenflux_summary

CONTAINS

   SUBROUTINE CNDriverSummarizeStates(i,ps,pe,nl_soil,dz_soi,ndecomp_pools,init)

! !DESCRIPTION:
! summarizes CN state varaibles for veg and soil.
!
! !ORIGINAL:
! The Community Land Model version 5.0 (CLM5.0)
!
! !REVISION:
! Xingjie Lu, 2022, modify original CLM5 to be compatible with CoLM code structure.

   integer, intent(in) :: i                 ! patch index
   integer, intent(in) :: ps                ! start pft index
   integer, intent(in) :: pe                ! END pft index
   integer, intent(in) :: nl_soil           ! number of total soil
   real(r8),intent(in) :: dz_soi(1:nl_soil) ! thicknesses of each soil layer (m)
   integer, intent(in) :: ndecomp_pools     ! number of total soil & litter pools
   logical, intent(in) :: init

      CALL soilbiogeochem_carbonstate_summary(i,nl_soil,dz_soi,ndecomp_pools)
      CALL soilbiogeochem_nitrogenstate_summary(i,nl_soil,dz_soi,ndecomp_pools)

      CALL cnveg_carbonstate_summary(i,ps,pe,init)
      CALL cnveg_nitrogenstate_summary(i,ps,pe)

   END SUBROUTINE CNDriverSummarizeStates

   SUBROUTINE CNDriverSummarizeFluxes(i,ps,pe,nl_soil,dz_soi,ndecomp_transitions,ndecomp_pools,deltim)

! !DESCRIPTION:
! summarizes CN flux varaibles for veg and soil.
!
! !ORIGINAL:
! The Community Land Model version 5.0 (CLM5.0)
!
! !REVISION:
! Xingjie Lu, 2022, modify original CLM5 to be compatible with CoLM code structure.

   integer, intent(in) :: i                    ! patch index
   integer, intent(in) :: ps                   ! start pft index
   integer, intent(in) :: pe                   ! END pft index
   integer, intent(in) :: nl_soil              ! number of total soil layers
   real(r8),intent(in) :: dz_soi(1:nl_soil)    ! thicknesses of each soil layer (m)
   integer, intent(in) :: ndecomp_transitions  ! number of total transfers between soil and litter pools in the decomposition
   integer, intent(in) :: ndecomp_pools        ! number of tootal soil & litter pools
   real(r8),intent(in) :: deltim               ! time step in seconds

      CALL soilbiogeochem_carbonflux_summary(i,nl_soil,dz_soi,ndecomp_transitions,ndecomp_pools)

      CALL soilbiogeochem_nitrogenflux_summary(i,nl_soil,dz_soi,ndecomp_transitions,ndecomp_pools)

      CALL cnveg_carbonflux_summary(i,ps,pe,deltim)

      CALL cnveg_nitrogenflux_summary(i,ps,pe)

   END SUBROUTINE CNDriverSummarizeFluxes

   SUBROUTINE soilbiogeochem_carbonstate_summary(i,nl_soil,dz_soi, ndecomp_pools)

! !DESCRIPTION
! summarizes soil C state varaibles.
!
! !ORIGINAL:
! The Community Land Model version 5.0 (CLM5.0)
!
! !REVISION:
! Xingjie Lu, 2022, modify original CLM5 to be compatible with CoLM code structure.

   integer, intent(in) :: i                 ! patch index
   integer, intent(in) :: nl_soil           ! number of total soil layers
   real(r8),intent(in) :: dz_soi(1:nl_soil) ! thicknesses of each soil layer (m)
   integer, intent(in) :: ndecomp_pools     ! number of tootal soil & litter pools

   integer :: l,j

      totsomc(i) = 0._r8
      totlitc(i) = 0._r8
      totcwdc(i) = 0._r8
      ctrunc_soil(i) = 0._r8

      DO l = 1, ndecomp_pools
         decomp_cpools(l,i) = 0._r8
         DO j = 1, nl_soil
            decomp_cpools(l,i) = decomp_cpools(l,i) + decomp_cpools_vr(j,l,i) * dz_soi(j)
         ENDDO
      ENDDO

      DO l = 1, ndecomp_pools
         IF(is_litter(l))THEN
            totlitc(i) = totlitc(i) + decomp_cpools(l,i)
         ENDIF
         IF(is_soil(l))THEN
            totsomc(i) = totsomc(i) + decomp_cpools(l,i)
         ENDIF
         IF(is_cwd(l))THEN
            totcwdc(i) = totcwdc(i) + decomp_cpools(l,i)
         ENDIF
      ENDDO

      DO j = 1, nl_soil
         ctrunc_soil(i) = ctrunc_soil(i) + ctrunc_vr(j,i) * dz_soi(j)
      ENDDO

   END SUBROUTINE soilbiogeochem_carbonstate_summary

   SUBROUTINE soilbiogeochem_nitrogenstate_summary(i,nl_soil,dz_soi,ndecomp_pools)

! !DESCRIPTION
! summarizes soil N state varaibles.
!
! !ORIGINAL:
! The Community Land Model version 5.0 (CLM5.0)
!
! !REVISION:
! Xingjie Lu, 2022, modify original CLM5 to be compatible with CoLM code structure.

   integer, intent(in) :: i                 ! patch index
   integer, intent(in) :: nl_soil           ! number of total soil layers
   real(r8),intent(in) :: dz_soi(1:nl_soil) ! thicknesses of each soil layer (m)
   integer, intent(in) :: ndecomp_pools     ! number of tootal soil & litter pools

   integer :: l,j

      totsomn(i) = 0._r8
      totlitn(i) = 0._r8
      totcwdn(i) = 0._r8
      sminn(i)   = 0._r8
      ntrunc_soil(i)  = 0._r8
      totsoiln_vr(1:nl_soil,i) = 0 ! soil total nitrogen (gN/gsoil * 100%)

      DO l = 1, ndecomp_pools
         decomp_npools(l,i) = 0._r8
         DO j = 1, nl_soil
            decomp_npools(l,i) = decomp_npools(l,i) + decomp_npools_vr(j,l,i) * dz_soi(j)
            totsoiln_vr(j,i)   = totsoiln_vr(j,i) + decomp_npools_vr(j,l,i) / (BD_all(j,i) * 1000) * 100 !(unit %)
         ENDDO
      ENDDO

      DO j = 1, nl_soil
         sminn(i) = sminn(i) + sminn_vr(j,i) * dz_soi(j)
         totsoiln_vr(j,i) = totsoiln_vr(j,i) + sminn_vr(j,i) / (BD_all(j,i) * 1000) * 100 !(unit %)
      ENDDO

      DO l = 1, ndecomp_pools
         IF(is_litter(l))THEN
            totlitn(i) = totlitn(i) + decomp_npools(l,i)
         ENDIF
         IF(is_soil(l))THEN
            totsomn(i) = totsomn(i) + decomp_npools(l,i)
         ENDIF
         IF(is_cwd(l))THEN
            totcwdn(i) = totcwdn(i) + decomp_npools(l,i)
         ENDIF
      ENDDO

      DO j = 1, nl_soil
         ntrunc_soil(i) = ntrunc_soil(i) + ntrunc_vr(j,i) * dz_soi(j)
      ENDDO

   END SUBROUTINE soilbiogeochem_nitrogenstate_summary

   SUBROUTINE cnveg_carbonstate_summary(i,ps,pe,init)

! !DESCRIPTION
! summarizes vegetation C state varaibles.
!
! !ORIGINAL:
! The Community Land Model version 5.0 (CLM5.0)
!
! !REVISION:
! Xingjie Lu, 2022, modify original CLM5 to be compatible with CoLM code structure.

   integer, intent(in) :: i  ! patch index
   integer, intent(in) :: ps ! start pft index
   integer, intent(in) :: pe ! END pft index
   logical, intent(in) :: init

   integer m

      leafc(i)              = sum(leafc_p(ps:pe)              * pftfrac(ps:pe))
      leafc_storage(i)      = sum(leafc_storage_p(ps:pe)      * pftfrac(ps:pe))
      leafc_xfer(i)         = sum(leafc_xfer_p(ps:pe)         * pftfrac(ps:pe))
      frootc(i)             = sum(frootc_p(ps:pe)             * pftfrac(ps:pe))
      frootc_storage(i)     = sum(frootc_storage_p(ps:pe)     * pftfrac(ps:pe))
      frootc_xfer(i)        = sum(frootc_xfer_p(ps:pe)        * pftfrac(ps:pe))
      livestemc(i)          = sum(livestemc_p(ps:pe)          * pftfrac(ps:pe))
      livestemc_storage(i)  = sum(livestemc_storage_p(ps:pe)  * pftfrac(ps:pe))
      livestemc_xfer(i)     = sum(livestemc_xfer_p(ps:pe)     * pftfrac(ps:pe))
      deadstemc(i)          = sum(deadstemc_p(ps:pe)          * pftfrac(ps:pe))
      deadstemc_storage(i)  = sum(deadstemc_storage_p(ps:pe)  * pftfrac(ps:pe))
      deadstemc_xfer(i)     = sum(deadstemc_xfer_p(ps:pe)     * pftfrac(ps:pe))
      livecrootc(i)         = sum(livecrootc_p(ps:pe)         * pftfrac(ps:pe))
      livecrootc_storage(i) = sum(livecrootc_storage_p(ps:pe) * pftfrac(ps:pe))
      livecrootc_xfer(i)    = sum(livecrootc_xfer_p(ps:pe)    * pftfrac(ps:pe))
      deadcrootc(i)         = sum(deadcrootc_p(ps:pe)         * pftfrac(ps:pe))
      deadcrootc_storage(i) = sum(deadcrootc_storage_p(ps:pe) * pftfrac(ps:pe))
      deadcrootc_xfer(i)    = sum(deadcrootc_xfer_p(ps:pe)    * pftfrac(ps:pe))
      xsmrpool(i)           = sum(xsmrpool_p(ps:pe)           * pftfrac(ps:pe))
#ifdef CROP
      grainc(i)             = sum(grainc_p(ps:pe)             * pftfrac(ps:pe))
      grainc_storage(i)     = sum(grainc_storage_p(ps:pe)     * pftfrac(ps:pe))
      grainc_xfer(i)        = sum(grainc_xfer_p(ps:pe)        * pftfrac(ps:pe))
      cropseedc_deficit(i)  = sum(cropseedc_deficit_p(ps:pe)  * pftfrac(ps:pe))
      cropprod1c(i)         = sum(cropprod1c_p(ps:pe)         * pftfrac(ps:pe))
      cphase(i)             = sum(cphase_p(ps:pe)             * pftfrac(ps:pe))
      hui(i)                = hui_p(ps)
      gddplant(i)           = sum(gddplant_p(ps:pe)           * pftfrac(ps:pe))
      gddmaturity(i)        = sum(gddmaturity_p(ps:pe)        * pftfrac(ps:pe))
      vf(i)                 = sum(vf_p(ps:pe)             * pftfrac(ps:pe))

      fertnitro_corn(i)     = 0._r8
      fertnitro_swheat(i)   = 0._r8
      fertnitro_wwheat(i)   = 0._r8
      fertnitro_soybean(i)  = 0._r8
      fertnitro_cotton(i)   = 0._r8
      fertnitro_rice1(i)    = 0._r8
      fertnitro_rice2(i)    = 0._r8
      fertnitro_sugarcane(i)= 0._r8

      manunitro(i)          = sum(manunitro_p(ps:pe)          * pftfrac(ps:pe))

#endif
      IF(DEF_USE_DiagMatrix)THEN
         leafcCap(i)              = sum(leafcCap_p(ps:pe)              * pftfrac(ps:pe))
         leafc_storageCap(i)      = sum(leafc_storageCap_p(ps:pe)      * pftfrac(ps:pe))
         leafc_xferCap(i)         = sum(leafc_xferCap_p(ps:pe)         * pftfrac(ps:pe))
         frootcCap(i)             = sum(frootcCap_p(ps:pe)             * pftfrac(ps:pe))
         frootc_storageCap(i)     = sum(frootc_storageCap_p(ps:pe)     * pftfrac(ps:pe))
         frootc_xferCap(i)        = sum(frootc_xferCap_p(ps:pe)        * pftfrac(ps:pe))
         livestemcCap(i)          = sum(livestemcCap_p(ps:pe)          * pftfrac(ps:pe))
         livestemc_storageCap(i)  = sum(livestemc_storageCap_p(ps:pe)  * pftfrac(ps:pe))
         livestemc_xferCap(i)     = sum(livestemc_xferCap_p(ps:pe)     * pftfrac(ps:pe))
         deadstemcCap(i)          = sum(deadstemcCap_p(ps:pe)          * pftfrac(ps:pe))
         deadstemc_storageCap(i)  = sum(deadstemc_storageCap_p(ps:pe)  * pftfrac(ps:pe))
         deadstemc_xferCap(i)     = sum(deadstemc_xferCap_p(ps:pe)     * pftfrac(ps:pe))
         livecrootcCap(i)         = sum(livecrootcCap_p(ps:pe)         * pftfrac(ps:pe))
         livecrootc_storageCap(i) = sum(livecrootc_storageCap_p(ps:pe) * pftfrac(ps:pe))
         livecrootc_xferCap(i)    = sum(livecrootc_xferCap_p(ps:pe)    * pftfrac(ps:pe))
         deadcrootcCap(i)         = sum(deadcrootcCap_p(ps:pe)         * pftfrac(ps:pe))
         deadcrootc_storageCap(i) = sum(deadcrootc_storageCap_p(ps:pe) * pftfrac(ps:pe))
         deadcrootc_xferCap(i)    = sum(deadcrootc_xferCap_p(ps:pe)    * pftfrac(ps:pe))
      ENDIF
      DO m = ps, pe
         totvegc_p(m) = leafc_p(m)             + frootc_p(m)             + livestemc_p(m) &
                      + deadstemc_p(m)         + livecrootc_p(m)         + deadcrootc_p(m) &
                      + leafc_storage_p(m)     + frootc_storage_p(m)     + livestemc_storage_p(m) &
                      + deadstemc_storage_p(m) + livecrootc_storage_p(m) + deadcrootc_storage_p(m) &
                      + leafc_xfer_p(m)        + frootc_xfer_p(m)        + livestemc_xfer_p(m) &
                      + deadstemc_xfer_p(m)    + livecrootc_xfer_p(m)    + deadcrootc_xfer_p(m) &
#ifdef CROP
                      + grainc_p(m)            + grainc_storage_p(m)     + grainc_xfer_p(m) &
                      + cropseedc_deficit_p(m) &
#endif
                      + gresp_storage_p(m)     + gresp_xfer_p(m)         + xsmrpool_p(m) + cpool_p(m)

#ifdef CROP
         IF(     pftclass(m) .eq. 17 .or. pftclass(m) .eq. 18 .or. pftclass(m) .eq. 63 .or. pftclass(m) .eq. 64)THEN
            fertnitro_corn        (i) = fertnitro_p   (m)
            irrig_method_corn     (i) = irrig_method_p(m)
         ELSE IF(pftclass(m) .eq. 19 .or. pftclass(m) .eq. 20)THEN
            fertnitro_swheat      (i) = fertnitro_p   (m)
            irrig_method_swheat(i) = irrig_method_p   (m)
         ELSE IF(pftclass(m) .eq. 21 .or. pftclass(m) .eq. 22)THEN
            fertnitro_wwheat      (i) = fertnitro_p   (m)
            irrig_method_wwheat   (i) = irrig_method_p(m)
         ELSE IF(pftclass(m) .eq. 23 .or. pftclass(m) .eq. 24 .or. pftclass(m) .eq. 77 .or. pftclass(m) .eq. 78)THEN
            fertnitro_soybean     (i) = fertnitro_p   (m)
            irrig_method_soybean  (i) = irrig_method_p(m)
         ELSE IF(pftclass(m) .eq. 41 .or. pftclass(m) .eq. 42)THEN
            fertnitro_cotton      (i) = fertnitro_p   (m)
            irrig_method_cotton   (i) = irrig_method_p(m)
         ELSE IF(pftclass(m) .eq. 61 .or. pftclass(m) .eq. 62)THEN
            fertnitro_rice1       (i) = fertnitro_p   (m)
            fertnitro_rice2       (i) = fertnitro_p   (m)
            irrig_method_rice1    (i) = irrig_method_p(m)
            irrig_method_rice2    (i) = irrig_method_p(m)
         ELSE IF(pftclass(m) .eq. 67 .or. pftclass(m) .eq. 68)THEN
            fertnitro_sugarcane   (i) = fertnitro_p   (m)
            irrig_method_sugarcane(i) = irrig_method_p(m)
         ENDIF
#endif
      ENDDO

      IF(.not. init)THEN
         leafc_enftemp              (i) = 0._r8
         leafc_enfboreal            (i) = 0._r8
         leafc_dnfboreal            (i) = 0._r8
         leafc_ebftrop              (i) = 0._r8
         leafc_ebftemp              (i) = 0._r8
         leafc_dbftrop              (i) = 0._r8
         leafc_dbftemp              (i) = 0._r8
         leafc_dbfboreal            (i) = 0._r8
         leafc_ebstemp              (i) = 0._r8
         leafc_dbstemp              (i) = 0._r8
         leafc_dbsboreal            (i) = 0._r8
         leafc_c3arcgrass           (i) = 0._r8
         leafc_c3grass              (i) = 0._r8
         leafc_c4grass              (i) = 0._r8
         lai_enftemp                (i) = 0._r8
         lai_enfboreal              (i) = 0._r8
         lai_dnfboreal              (i) = 0._r8
         lai_ebftrop                (i) = 0._r8
         lai_ebftemp                (i) = 0._r8
         lai_dbftrop                (i) = 0._r8
         lai_dbftemp                (i) = 0._r8
         lai_dbfboreal              (i) = 0._r8
         lai_ebstemp                (i) = 0._r8
         lai_dbstemp                (i) = 0._r8
         lai_dbsboreal              (i) = 0._r8
         lai_c3arcgrass             (i) = 0._r8
         lai_c3grass                (i) = 0._r8
         lai_c4grass                (i) = 0._r8
         DO m = ps, pe
            IF(pftclass      (m) .eq. 1)THEN
               leafc_enftemp   (i) = leafc_p(m)
               lai_enftemp     (i) = lai_p(m)
            ELSE IF(pftclass (m) .eq. 2)THEN
               leafc_enfboreal (i) = leafc_p(m)
               lai_enfboreal   (i) = lai_p(m)
            ELSE IF(pftclass (m) .eq. 3)THEN
               leafc_dnfboreal (i) = leafc_p(m)
               lai_dnfboreal   (i) = lai_p(m)
            ELSE IF(pftclass (m) .eq. 4)THEN
               leafc_ebftrop   (i) = leafc_p(m)
               lai_ebftrop     (i) = lai_p(m)
            ELSE IF(pftclass (m) .eq. 5)THEN
               leafc_ebftemp   (i) = leafc_p(m)
               lai_ebftemp     (i) = lai_p(m)
            ELSE IF(pftclass (m) .eq. 6)THEN
               leafc_dbftrop   (i) = leafc_p(m)
               lai_dbftrop     (i) = lai_p(m)
            ELSE IF(pftclass (m) .eq. 7)THEN
               leafc_dbftemp   (i) = leafc_p(m)
               lai_dbftemp     (i) = lai_p(m)
            ELSE IF(pftclass (m) .eq. 8)THEN
               leafc_dbfboreal (i) = leafc_p(m)
               lai_dbfboreal   (i) = lai_p(m)
            ELSE IF(pftclass (m) .eq. 9)THEN
               leafc_ebstemp   (i) = leafc_p(m)
               lai_ebstemp     (i) = lai_p(m)
            ELSE IF(pftclass (m) .eq. 10)THEN
               leafc_dbstemp   (i) = leafc_p(m)
               lai_dbstemp     (i) = lai_p(m)
            ELSE IF(pftclass (m) .eq. 11)THEN
               leafc_dbsboreal (i) = leafc_p(m)
               lai_dbsboreal   (i) = lai_p(m)
            ELSE IF(pftclass (m) .eq. 12)THEN
               leafc_c3arcgrass(i)= leafc_p(m)
               lai_c3arcgrass  (i)= lai_p(m)
            ELSE IF(pftclass (m) .eq. 13)THEN
               leafc_c3grass   (i) = leafc_p(m)
               lai_c3grass     (i) = lai_p(m)
            ELSE IF(pftclass (m) .eq. 14)THEN
               leafc_c4grass   (i) = leafc_p(m)
               lai_c4grass     (i) = lai_p(m)
            ENDIF
         ENDDO
      ENDIF
      totvegc(i) = sum(totvegc_p(ps:pe)*pftfrac(ps:pe))
      ctrunc_veg(i) = sum(ctrunc_p(ps:pe) *pftfrac(ps:pe))
      totcolc(i) = totvegc(i) + totcwdc(i) + totlitc(i) + totsomc(i) + ctrunc_veg(i) +ctrunc_soil(i)


   END SUBROUTINE cnveg_carbonstate_summary

   SUBROUTINE cnveg_nitrogenstate_summary(i,ps,pe)

! !DESCRIPTION
! summarizes vegetation N state varaibles.
!
! !ORIGINAL:
! The Community Land Model version 5.0 (CLM5.0)
!
! !REVISION:
! Xingjie Lu, 2022, modify original CLM5 to be compatible with CoLM code structure.

   integer, intent(in) :: i  ! patch index
   integer, intent(in) :: ps ! start pft index
   integer, intent(in) :: pe ! END pft index

   integer m

      leafn(i)              = sum(leafn_p(ps:pe)              * pftfrac(ps:pe))
      leafn_storage(i)      = sum(leafn_storage_p(ps:pe)      * pftfrac(ps:pe))
      leafn_xfer(i)         = sum(leafn_xfer_p(ps:pe)         * pftfrac(ps:pe))
      frootn(i)             = sum(frootn_p(ps:pe)             * pftfrac(ps:pe))
      frootn_storage(i)     = sum(frootn_storage_p(ps:pe)     * pftfrac(ps:pe))
      frootn_xfer(i)        = sum(frootn_xfer_p(ps:pe)        * pftfrac(ps:pe))
      livestemn(i)          = sum(livestemn_p(ps:pe)          * pftfrac(ps:pe))
      livestemn_storage(i)  = sum(livestemn_storage_p(ps:pe)  * pftfrac(ps:pe))
      livestemn_xfer(i)     = sum(livestemn_xfer_p(ps:pe)     * pftfrac(ps:pe))
      deadstemn(i)          = sum(deadstemn_p(ps:pe)          * pftfrac(ps:pe))
      deadstemn_storage(i)  = sum(deadstemn_storage_p(ps:pe)  * pftfrac(ps:pe))
      deadstemn_xfer(i)     = sum(deadstemn_xfer_p(ps:pe)     * pftfrac(ps:pe))
      livecrootn(i)         = sum(livecrootn_p(ps:pe)         * pftfrac(ps:pe))
      livecrootn_storage(i) = sum(livecrootn_storage_p(ps:pe) * pftfrac(ps:pe))
      livecrootn_xfer(i)    = sum(livecrootn_xfer_p(ps:pe)    * pftfrac(ps:pe))
      deadcrootn(i)         = sum(deadcrootn_p(ps:pe)         * pftfrac(ps:pe))
      deadcrootn_storage(i) = sum(deadcrootn_storage_p(ps:pe) * pftfrac(ps:pe))
      deadcrootn_xfer(i)    = sum(deadcrootn_xfer_p(ps:pe)    * pftfrac(ps:pe))
#ifdef CROP
      grainn(i)             = sum(grainn_p(ps:pe)             * pftfrac(ps:pe))
      grainn_storage(i)     = sum(grainn_storage_p(ps:pe)     * pftfrac(ps:pe))
      grainn_xfer(i)        = sum(grainn_xfer_p(ps:pe)        * pftfrac(ps:pe))
#endif
      retransn(i)           = sum(retransn_p(ps:pe)           * pftfrac(ps:pe))

      IF(DEF_USE_DiagMatrix)THEN
         leafnCap(i)              = sum(leafnCap_p(ps:pe)              * pftfrac(ps:pe))
         leafn_storageCap(i)      = sum(leafn_storageCap_p(ps:pe)      * pftfrac(ps:pe))
         leafn_xferCap(i)         = sum(leafn_xferCap_p(ps:pe)         * pftfrac(ps:pe))
         frootnCap(i)             = sum(frootnCap_p(ps:pe)             * pftfrac(ps:pe))
         frootn_storageCap(i)     = sum(frootn_storageCap_p(ps:pe)     * pftfrac(ps:pe))
         frootn_xferCap(i)        = sum(frootn_xferCap_p(ps:pe)        * pftfrac(ps:pe))
         livestemnCap(i)          = sum(livestemnCap_p(ps:pe)          * pftfrac(ps:pe))
         livestemn_storageCap(i)  = sum(livestemn_storageCap_p(ps:pe)  * pftfrac(ps:pe))
         livestemn_xferCap(i)     = sum(livestemn_xferCap_p(ps:pe)     * pftfrac(ps:pe))
         deadstemnCap(i)          = sum(deadstemnCap_p(ps:pe)          * pftfrac(ps:pe))
         deadstemn_storageCap(i)  = sum(deadstemn_storageCap_p(ps:pe)  * pftfrac(ps:pe))
         deadstemn_xferCap(i)     = sum(deadstemn_xferCap_p(ps:pe)     * pftfrac(ps:pe))
         livecrootnCap(i)         = sum(livecrootnCap_p(ps:pe)         * pftfrac(ps:pe))
         livecrootn_storageCap(i) = sum(livecrootn_storageCap_p(ps:pe) * pftfrac(ps:pe))
         livecrootn_xferCap(i)    = sum(livecrootn_xferCap_p(ps:pe)    * pftfrac(ps:pe))
         deadcrootnCap(i)         = sum(deadcrootnCap_p(ps:pe)         * pftfrac(ps:pe))
         deadcrootn_storageCap(i) = sum(deadcrootn_storageCap_p(ps:pe) * pftfrac(ps:pe))
         deadcrootn_xferCap(i)    = sum(deadcrootn_xferCap_p(ps:pe)    * pftfrac(ps:pe))
      ENDIF
      DO m = ps, pe
         totvegn_p(m) = leafn_p(m)             + frootn_p(m)             + livestemn_p(m) &
                      + deadstemn_p(m)         + livecrootn_p(m)         + deadcrootn_p(m) &
                      + leafn_storage_p(m)     + frootn_storage_p(m)     + livestemn_storage_p(m) &
                      + deadstemn_storage_p(m) + livecrootn_storage_p(m) + deadcrootn_storage_p(m) &
                      + leafn_xfer_p(m)        + frootn_xfer_p(m)        + livestemn_xfer_p(m) &
                      + deadstemn_xfer_p(m)    + livecrootn_xfer_p(m)    + deadcrootn_xfer_p(m) &
#ifdef CROP
                      + grainn_p(m)            + grainn_storage_p(m)     + grainn_xfer_p(m) &
                      + cropseedn_deficit_p(m) &
#endif
                      + npool_p(m)             + retransn_p(m)
      ENDDO

      totvegn(i) = sum(totvegn_p(ps:pe)*pftfrac(ps:pe))
      ntrunc_veg(i)  = sum(ntrunc_p(ps:pe) *pftfrac(ps:pe))
      totcoln(i) = totvegn(i) + totcwdn(i) + totlitn(i) + totsomn(i) + sminn(i) + ntrunc_veg(i) + ntrunc_soil(i)

   END SUBROUTINE cnveg_nitrogenstate_summary

   SUBROUTINE soilbiogeochem_carbonflux_summary(i,nl_soil,dz_soi,ndecomp_transitions,ndecomp_pools)

! !DESCRIPTION
! summarizes soil C flux varaibles.
!
! !ORIGINAL:
! The Community Land Model version 5.0 (CLM5.0)
!
! !REVISION:
! Xingjie Lu, 2022, modify original CLM5 to be compatible with CoLM code structure.

   integer, intent(in) :: i                   ! patch index
   integer, intent(in) :: nl_soil             ! number of total soil layers
   real(r8),intent(in) :: dz_soi(1:nl_soil)   ! thicknesses of each soil layer (m)
   integer, intent(in) :: ndecomp_transitions ! number of total transfers between soil and litter pools in the decomposition
   integer, intent(in) :: ndecomp_pools       ! number of total soil & litter pools in the decompositions

   integer k,j,l

      DO k = 1, ndecomp_transitions
         DO j = 1, nl_soil
            decomp_hr(i) = decomp_hr(i) &
                         + decomp_hr_vr(j,k,i) * dz_soi(j)
         ENDDO
      ENDDO

      DO l = 1, ndecomp_pools
         DO j = 1, nl_soil
            som_c_leached(i) = som_c_leached(i) + decomp_cpools_transport_tendency(j,l,i) * dz_soi(j)
         ENDDO
      ENDDO


   END SUBROUTINE soilbiogeochem_carbonflux_summary

   SUBROUTINE soilbiogeochem_nitrogenflux_summary(i,nl_soil,dz_soi,ndecomp_transitions,ndecomp_pools)

! !DESCRIPTION
! summarizes soil N flux varaibles.
!
! !ORIGINAL:
! The Community Land Model version 5.0 (CLM5.0)
!
! !REVISION:
! Xingjie Lu, 2022, modify original CLM5 to be compatible with CoLM code structure.

   integer, intent(in) :: i                    ! patch index
   integer, intent(in) :: nl_soil              ! number of total soil layers
   real(r8),intent(in) :: dz_soi(1:nl_soil)    ! thicknesses of each soil layer (m)
   integer, intent(in) :: ndecomp_transitions  ! number of total transfers between soil and litter pools in the decomposition
   integer, intent(in) :: ndecomp_pools        ! number of total soil & litter pools in the decompositions

   integer j,l,k

      DO l = 1, ndecomp_pools
         DO j = 1, nl_soil
            som_n_leached(i) = som_n_leached(i) + decomp_npools_transport_tendency(j,l,i) * dz_soi(j)
         ENDDO
      ENDDO

      DO j = 1, nl_soil
         supplement_to_sminn(i) = supplement_to_sminn(i) + supplement_to_sminn_vr(j,i) * dz_soi(j)
         smin_no3_leached(i)    = smin_no3_leached(i)    + smin_no3_leached_vr(j,i) * dz_soi(j)
         smin_no3_runoff(i)     = smin_no3_runoff(i)     + smin_no3_runoff_vr(j,i) * dz_soi(j)
         sminn_leached(i)       = sminn_leached(i)       + sminn_leached_vr(j,i) * dz_soi(j)
         f_n2o_nit(i)           = f_n2o_nit(i)           + f_n2o_nit_vr(j,i) * dz_soi(j)
         IF(DEF_USE_NITRIF)THEN
            denit(i)            = denit(i)               + f_denit_vr(j,i) * dz_soi(j)
         ELSE
            denit(i)            = denit(i)               + sminn_to_denit_excess_vr(j,i) * dz_soi(j)
         ENDIF

         IF(.not. DEF_USE_NITRIF)THEN
            DO k = 1, ndecomp_transitions
               denit(i) = denit(i) + sminn_to_denit_decomp_vr(j,k,i) * dz_soi(j)
            ENDDO
         ENDIF
      ENDDO

   END SUBROUTINE soilbiogeochem_nitrogenflux_summary

   SUBROUTINE cnveg_carbonflux_summary(i,ps,pe,deltim)

! !DESCRIPTION
! summarizes vegetationi C flux varaibles.
!
! !ORIGINAL:
! The Community Land Model version 5.0 (CLM5.0)
!
! !REVISION:
! Xingjie Lu, 2022, modify original CLM5 to be compatible with CoLM code structure.

   integer, intent(in) :: i       ! patch index
   integer, intent(in) :: ps      ! start pft index
   integer, intent(in) :: pe      ! END pft index
   real(r8),intent(in) :: deltim  ! time step in seconds
   real(r8) :: ar_p

   integer m
   real(r8) nfixlags

      gpp(i) = sum(psn_to_cpool_p(ps:pe) * pftfrac(ps:pe))
      downreg(i) = sum(downreg_p(ps:pe) * pftfrac(ps:pe))
      ar (i) = sum((leaf_mr_p(ps:pe)                    + froot_mr_p(ps:pe) &
                  + livestem_mr_p(ps:pe)                + livecroot_mr_p(ps:pe) &
                  + cpool_leaf_gr_p(ps:pe)              + cpool_froot_gr_p(ps:pe) &
                  + cpool_livestem_gr_p(ps:pe)          + cpool_deadstem_gr_p(ps:pe) &
                  + cpool_livecroot_gr_p(ps:pe)         + cpool_deadcroot_gr_p(ps:pe) &
                  + transfer_leaf_gr_p(ps:pe)           + transfer_froot_gr_p(ps:pe) &
                  + transfer_livestem_gr_p(ps:pe)       + transfer_deadstem_gr_p(ps:pe) &
                  + transfer_livecroot_gr_p(ps:pe)      + transfer_deadcroot_gr_p(ps:pe) &
                  + cpool_leaf_storage_gr_p(ps:pe)      + cpool_froot_storage_gr_p(ps:pe) &
                  + cpool_livestem_storage_gr_p(ps:pe)  + cpool_deadstem_storage_gr_p(ps:pe) &
                  + cpool_livecroot_storage_gr_p(ps:pe) + cpool_deadcroot_storage_gr_p(ps:pe) &
                  + grain_mr_p(ps:pe)                   + xsmrpool_to_atm_p(ps:pe) &
                  + cpool_grain_gr_p(ps:pe)             + transfer_grain_gr_p(ps:pe) &
                  + cpool_grain_storage_gr_p(ps:pe))    * pftfrac(ps:pe))
      gpp_enftemp                (i) = 0._r8
      gpp_enfboreal              (i) = 0._r8
      gpp_dnfboreal              (i) = 0._r8
      gpp_ebftrop                (i) = 0._r8
      gpp_ebftemp                (i) = 0._r8
      gpp_dbftrop                (i) = 0._r8
      gpp_dbftemp                (i) = 0._r8
      gpp_dbfboreal              (i) = 0._r8
      gpp_ebstemp                (i) = 0._r8
      gpp_dbstemp                (i) = 0._r8
      gpp_dbsboreal              (i) = 0._r8
      gpp_c3arcgrass             (i) = 0._r8
      gpp_c3grass                (i) = 0._r8
      gpp_c4grass                (i) = 0._r8
      npp_enftemp                (i) = 0._r8
      npp_enfboreal              (i) = 0._r8
      npp_dnfboreal              (i) = 0._r8
      npp_ebftrop                (i) = 0._r8
      npp_ebftemp                (i) = 0._r8
      npp_dbftrop                (i) = 0._r8
      npp_dbftemp                (i) = 0._r8
      npp_dbfboreal              (i) = 0._r8
      npp_ebstemp                (i) = 0._r8
      npp_dbstemp                (i) = 0._r8
      npp_dbsboreal              (i) = 0._r8
      npp_c3arcgrass             (i) = 0._r8
      npp_c3grass                (i) = 0._r8
      npp_c4grass                (i) = 0._r8
      npptoleafc_enftemp         (i) = 0._r8
      npptoleafc_enfboreal       (i) = 0._r8
      npptoleafc_dnfboreal       (i) = 0._r8
      npptoleafc_ebftrop         (i) = 0._r8
      npptoleafc_ebftemp         (i) = 0._r8
      npptoleafc_dbftrop         (i) = 0._r8
      npptoleafc_dbftemp         (i) = 0._r8
      npptoleafc_dbfboreal       (i) = 0._r8
      npptoleafc_ebstemp         (i) = 0._r8
      npptoleafc_dbstemp         (i) = 0._r8
      npptoleafc_dbsboreal       (i) = 0._r8
      npptoleafc_c3arcgrass      (i) = 0._r8
      npptoleafc_c3grass         (i) = 0._r8
      npptoleafc_c4grass         (i) = 0._r8
      DO m = ps, pe
         ar_p = (leaf_mr_p(m)                   + froot_mr_p(m) &
              + livestem_mr_p(m)                + livecroot_mr_p(m) &
              + cpool_leaf_gr_p(m)              + cpool_froot_gr_p(m) &
              + cpool_livestem_gr_p(m)          + cpool_deadstem_gr_p(m) &
              + cpool_livecroot_gr_p(m)         + cpool_deadcroot_gr_p(m) &
              + transfer_leaf_gr_p(m)           + transfer_froot_gr_p(m) &
              + transfer_livestem_gr_p(m)       + transfer_deadstem_gr_p(m) &
              + transfer_livecroot_gr_p(m)      + transfer_deadcroot_gr_p(m) &
              + cpool_leaf_storage_gr_p(m)      + cpool_froot_storage_gr_p(m) &
              + cpool_livestem_storage_gr_p(m)  + cpool_deadstem_storage_gr_p(m) &
              + cpool_livecroot_storage_gr_p(m) + cpool_deadcroot_storage_gr_p(m) &
              + grain_mr_p(m)                   + xsmrpool_to_atm_p(m) &
              + cpool_grain_gr_p(m)             + transfer_grain_gr_p(m) &
              + cpool_grain_storage_gr_p(m))
         IF(pftclass      (m) .eq. 1)THEN
            gpp_enftemp          (i) = psn_to_cpool_p(m)
            npp_enftemp          (i) = psn_to_cpool_p(m) - ar_p
            npptoleafc_enftemp   (i) = cpool_to_leafc_p(m) + cpool_to_leafc_storage_p(m)
         ELSE IF(pftclass (m) .eq. 2)THEN
            gpp_enfboreal        (i) = psn_to_cpool_p(m)
            npp_enfboreal        (i) = psn_to_cpool_p(m) - ar_p
            npptoleafc_enfboreal (i) = cpool_to_leafc_p(m) + cpool_to_leafc_storage_p(m)
         ELSE IF(pftclass (m) .eq. 3)THEN
            gpp_dnfboreal        (i) = psn_to_cpool_p(m)
            npp_dnfboreal        (i) = psn_to_cpool_p(m) - ar_p
            npptoleafc_dnfboreal (i) = cpool_to_leafc_p(m) + cpool_to_leafc_storage_p(m)
         ELSE IF(pftclass (m) .eq. 4)THEN
            gpp_ebftrop          (i) = psn_to_cpool_p(m)
            npp_ebftrop          (i) = psn_to_cpool_p(m) - ar_p
            npptoleafc_ebftrop   (i) = cpool_to_leafc_p(m) + cpool_to_leafc_storage_p(m)
         ELSE IF(pftclass (m) .eq. 5)THEN
            gpp_ebftemp          (i) = psn_to_cpool_p(m)
            npp_ebftemp          (i) = psn_to_cpool_p(m) - ar_p
            npptoleafc_ebftemp   (i) = cpool_to_leafc_p(m) + cpool_to_leafc_storage_p(m)
         ELSE IF(pftclass (m) .eq. 6)THEN
            gpp_dbftrop          (i) = psn_to_cpool_p(m)
            npp_dbftrop          (i) = psn_to_cpool_p(m) - ar_p
            npptoleafc_dbftrop   (i) = cpool_to_leafc_p(m) + cpool_to_leafc_storage_p(m)
         ELSE IF(pftclass (m) .eq. 7)THEN
            gpp_dbftemp          (i) = psn_to_cpool_p(m)
            npp_dbftemp          (i) = psn_to_cpool_p(m) - ar_p
            npptoleafc_dbftemp   (i) = cpool_to_leafc_p(m) + cpool_to_leafc_storage_p(m)
         ELSE IF(pftclass (m) .eq. 8)THEN
            gpp_dbfboreal        (i) = psn_to_cpool_p(m)
            npp_dbfboreal        (i) = psn_to_cpool_p(m) - ar_p
            npptoleafc_dbfboreal (i) = cpool_to_leafc_p(m) + cpool_to_leafc_storage_p(m)
         ELSE IF(pftclass (m) .eq. 9)THEN
            gpp_ebstemp          (i) = psn_to_cpool_p(m)
            npp_ebstemp          (i) = psn_to_cpool_p(m) - ar_p
            npptoleafc_ebstemp   (i) = cpool_to_leafc_p(m) + cpool_to_leafc_storage_p(m)
         ELSE IF(pftclass (m) .eq. 10)THEN
            gpp_dbstemp          (i) = psn_to_cpool_p(m)
            npp_dbstemp          (i) = psn_to_cpool_p(m) - ar_p
            npptoleafc_dbstemp   (i) = cpool_to_leafc_p(m) + cpool_to_leafc_storage_p(m)
         ELSE IF(pftclass (m) .eq. 11)THEN
            gpp_dbsboreal        (i) = psn_to_cpool_p(m)
            npp_dbsboreal        (i) = psn_to_cpool_p(m) - ar_p
            npptoleafc_dbsboreal (i) = cpool_to_leafc_p(m) + cpool_to_leafc_storage_p(m)
         ELSE IF(pftclass (m) .eq. 12)THEN
            gpp_c3arcgrass       (i) = psn_to_cpool_p(m)
            npp_c3arcgrass       (i) = psn_to_cpool_p(m) - ar_p
            npptoleafc_c3arcgrass(i) = cpool_to_leafc_p(m) + cpool_to_leafc_storage_p(m)
         ELSE IF(pftclass (m) .eq. 13)THEN
            gpp_c3grass          (i) = psn_to_cpool_p(m)
            npp_c3grass          (i) = psn_to_cpool_p(m) - ar_p
            npptoleafc_c3grass   (i) = cpool_to_leafc_p(m) + cpool_to_leafc_storage_p(m)
         ELSE IF(pftclass (m) .eq. 14)THEN
            gpp_c4grass          (i) = psn_to_cpool_p(m)
            npp_c4grass          (i) = psn_to_cpool_p(m) - ar_p
            npptoleafc_c4grass   (i) = cpool_to_leafc_p(m) + cpool_to_leafc_storage_p(m)
         ENDIF
      ENDDO


#ifdef FUN
      ar(i) = ar(i) + sum(soil_change_p(ps:pe) * pftfrac(ps:pe))
#endif
      er(i) = ar(i) + decomp_hr(i)
#ifdef CROP
      IF(patchclass(i) .eq. 12)THEN
         IF(ps .ne. pe)THEN
            write(*,*) 'Error: crop patch CONTAINS multiple pfts:',mpas_rank,'i=',i,'ps',ps,'does not equal to pe',pe
            CALL CoLM_stop('A crop patch contains more than one PFT.')
         ELSE
            cropprod1c_loss     (i) = cropprod1c_loss_p(ps)
            grainc_to_cropprodc (i) = grainc_to_food_p (ps)
            grainc_to_seed      (i) = grainc_to_seed_p (ps)
            plantdate           (i) = plantdate_p      (ps)
         ENDIF
      ELSE
         cropprod1c_loss     (i) = 0._r8
         grainc_to_cropprodc (i) = 0._r8
         grainc_to_seed      (i) = 0._r8
      ENDIF
#endif

    !fire module is not activated yet.
      DO m = ps, pe
         fire_closs_p(m) = m_leafc_to_fire_p(m) &
                         + m_leafc_storage_to_fire_p(m) &
                         + m_leafc_xfer_to_fire_p(m) &
                         + m_frootc_to_fire_p(m) &
                         + m_frootc_storage_to_fire_p(m) &
                         + m_frootc_xfer_to_fire_p(m) &
                         + m_livestemc_to_fire_p(m) &
                         + m_livestemc_storage_to_fire_p(m) &
                         + m_livestemc_xfer_to_fire_p(m) &
                         + m_deadstemc_to_fire_p(m) &
                         + m_deadstemc_storage_to_fire_p(m) &
                         + m_deadstemc_xfer_to_fire_p(m) &
                         + m_livecrootc_to_fire_p(m) &
                         + m_livecrootc_storage_to_fire_p(m) &
                         + m_livecrootc_xfer_to_fire_p(m) &
                         + m_deadcrootc_to_fire_p(m) &
                         + m_deadcrootc_storage_to_fire_p(m) &
                         + m_deadcrootc_xfer_to_fire_p(m) &
                         + m_gresp_storage_to_fire_p(m) &
                         + m_gresp_xfer_to_fire_p(m)
      ENDDO

      fire_closs(i)          = sum(fire_closs_p(ps:pe)          * pftfrac(ps:pe))
      hrv_xsmrpool_to_atm(i) = sum(hrv_xsmrpool_to_atm_p(ps:pe) * pftfrac(ps:pe))

      nfixlags = nfix_timeconst * 86400._r8
      IF(lag_npp(i) /= spval)THEN
         lag_npp(i) = lag_npp(i) * exp(-deltim/nfixlags) + &
                      (gpp(i) - ar(i)) * (1._r8 - exp(-deltim/nfixlags))
      ELSE
         lag_npp(i) = gpp(i) - ar(i)
      ENDIF

   END SUBROUTINE cnveg_carbonflux_summary

   SUBROUTINE cnveg_nitrogenflux_summary(i,ps,pe)

! !DESCRIPTION
! summarizes vegetationi N flux varaibles.
!
! !ORIGINAL:
! The Community Land Model version 5.0 (CLM5.0)
!
! !REVISION:
! Xingjie Lu, 2022, modify original CLM5 to be compatible with CoLM code structure.

   integer, intent(in) :: i  ! patch index
   integer, intent(in) :: ps ! start pft index
   integer, intent(in) :: pe ! end pft index

#ifdef CROP
      IF(patchclass(i) .eq. 12)THEN
         IF(ps .ne. pe)THEN
            write(*,*) 'Error: crop patch contains multiple pfts:',mpas_rank,'i=',i,'ps',ps,'does not equal to pe',pe
            CALL CoLM_stop('A crop patch contains more than one PFT.')
         ELSE
            grainn_to_cropprodn (i) = grainn_to_food_p (ps)
         ENDIF
      ELSE
         grainn_to_cropprodn (i) = 0._r8
      ENDIF
#endif
   END SUBROUTINE cnveg_nitrogenflux_summary

END MODULE MOD_BGC_CNSummary


#endif
