#include <define.h>

#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)

MODULE MOD_BGC_Vars_1DPFTFluxes
#ifdef BGC
!---------------------------------------------------------------------------------------------------------
! !DESCRIPTION
! Define, allocate, and deallocate biogeochemical flux variables at pft level

! !ORIGINAL:
! Xingjie Lu, 2022, created the original version

   USE MOD_Precision
   IMPLICIT NONE
   SAVE

! bgc variables
   real(r8), allocatable :: leafc_xfer_to_leafc_p                (:) ! pft level: phenology-associated flux: leaf transfer C to display C (gC m-2 s-1)
   real(r8), allocatable :: frootc_xfer_to_frootc_p              (:) ! pft level: phenology-associated flux: fine root transfer C to display C (gC m-2 s-1)
   real(r8), allocatable :: livestemc_xfer_to_livestemc_p        (:) ! pft level: phenology-associated flux: live stem transfer C to display C (gC m-2 s-1)
   real(r8), allocatable :: deadstemc_xfer_to_deadstemc_p        (:) ! pft level: phenology-associated flux: dead stem transfer C to display C (gC m-2 s-1)
   real(r8), allocatable :: livecrootc_xfer_to_livecrootc_p      (:) ! pft level: phenology-associated flux: live coarse root transfer C to display C (gC m-2 s-1)
   real(r8), allocatable :: deadcrootc_xfer_to_deadcrootc_p      (:) ! pft level: phenology-associated flux: dead coarse root transfer C to display C (gC m-2 s-1)
   real(r8), allocatable :: grainc_xfer_to_grainc_p              (:) ! pft level: phenology-associated flux: grain transfer C to display C (gC m-2 s-1)

   real(r8), allocatable :: leafc_storage_to_xfer_p              (:) ! pft level: phenology-associated flux: leaf storage C to transfer C (gC m-2 s-1)
   real(r8), allocatable :: frootc_storage_to_xfer_p             (:) ! pft level: phenology-associated flux: fine root storage C to transfer C (gC m-2 s-1)
   real(r8), allocatable :: livestemc_storage_to_xfer_p          (:) ! pft level: phenology-associated flux: live stem storage C to transfer C (gC m-2 s-1)
   real(r8), allocatable :: deadstemc_storage_to_xfer_p          (:) ! pft level: phenology-associated flux: dead stem storage C to transfer C (gC m-2 s-1)
   real(r8), allocatable :: livecrootc_storage_to_xfer_p         (:) ! pft level: phenology-associated flux: live coarse root storage C to transfer C (gC m-2 s-1)
   real(r8), allocatable :: deadcrootc_storage_to_xfer_p         (:) ! pft level: phenology-associated flux: dead coarse root storage C to transfer C (gC m-2 s-1)
   real(r8), allocatable :: grainc_storage_to_xfer_p             (:) ! pft level: phenology-associated flux: grain storage C to transfer C (gC m-2 s-1)
   real(r8), allocatable :: gresp_storage_to_xfer_p              (:) ! pft level: phenology-associated flux: growth respiration storage C to transfer C (gC m-2 s-1)

   real(r8), allocatable :: leafc_to_litter_p                    (:) ! pft level: phenology-associated flux: leaf display C to litter C (gC m-2 s-1)
   real(r8), allocatable :: frootc_to_litter_p                   (:) ! pft level: phenology-associated flux: fine root display C to litter C (gC m-2 s-1)
   real(r8), allocatable :: grainc_to_food_p                     (:) ! pft level: phenology-associated flux: grain display C to product C (gC m-2 s-1)
   real(r8), allocatable :: grainc_to_seed_p                     (:) ! pft level: phenology-associated flux: grain display C to seed C (gC m-2 s-1)
   real(r8), allocatable :: crop_seedc_to_leaf_p                 (:) ! pft level: phenology-associated flux: seed C to leaf display C (gC m-2 s-1)
   real(r8), allocatable :: livestemc_to_litter_p                (:) ! pft level: phenology-associated flux: live stem display C to litter C (gC m-2 s-1)
   real(r8), allocatable :: livestemc_to_deadstemc_p             (:) ! pft level: phenology-associated flux: live stem display C to dead stem display C (gC m-2 s-1)
   real(r8), allocatable :: livecrootc_to_deadcrootc_p           (:) ! pft level: phenology-associated flux: live coarse root display C to dead coarse root display C (gC m-2 s-1)

   real(r8), allocatable :: m_leafc_to_litter_p                  (:) ! pft level: gap mortality-associated flux: leaf display C to litter C (gC m-2 s-1)
   real(r8), allocatable :: m_frootc_to_litter_p                 (:) ! pft level: gap mortality-associated flux: fine root display C to litter C (gC m-2 s-1)
   real(r8), allocatable :: m_livestemc_to_litter_p              (:) ! pft level: gap mortality-associated flux: live stem display C to litter C (gC m-2 s-1)
   real(r8), allocatable :: m_deadstemc_to_litter_p              (:) ! pft level: gap mortality-associated flux: dead stem display C to litter C (gC m-2 s-1)
   real(r8), allocatable :: m_livecrootc_to_litter_p             (:) ! pft level: gap mortality-associated flux: live coarse root display C to litter C (gC m-2 s-1)
   real(r8), allocatable :: m_deadcrootc_to_litter_p             (:) ! pft level: gap mortality-associated flux: dead coarse root display C to litter C (gC m-2 s-1)

   real(r8), allocatable :: m_leafc_storage_to_litter_p          (:) ! pft level: gap mortality-associated flux: leaf storage C to litter C (gC m-2 s-1)
   real(r8), allocatable :: m_frootc_storage_to_litter_p         (:) ! pft level: gap mortality-associated flux: fine root storage C to litter C (gC m-2 s-1)
   real(r8), allocatable :: m_livestemc_storage_to_litter_p      (:) ! pft level: gap mortality-associated flux: live stem storage C to litter C (gC m-2 s-1)
   real(r8), allocatable :: m_deadstemc_storage_to_litter_p      (:) ! pft level: gap mortality-associated flux: dead stem storage C to litter C (gC m-2 s-1)
   real(r8), allocatable :: m_livecrootc_storage_to_litter_p     (:) ! pft level: gap mortality-associated flux: live coarse root storage C to litter C (gC m-2 s-1)
   real(r8), allocatable :: m_deadcrootc_storage_to_litter_p     (:) ! pft level: gap mortality-associated flux: dead coarse root storage C to litter C (gC m-2 s-1)
   real(r8), allocatable :: m_gresp_storage_to_litter_p          (:) ! pft level: gap mortality-associated flux: growth respiration storage C to litter C (gC m-2 s-1)

   real(r8), allocatable :: m_leafc_xfer_to_litter_p             (:) ! pft level: gap mortality-associated flux: leaf transfer C to litter C (gC m-2 s-1)
   real(r8), allocatable :: m_frootc_xfer_to_litter_p            (:) ! pft level: gap mortality-associated flux: fine root transfer C to litter C (gC m-2 s-1)
   real(r8), allocatable :: m_livestemc_xfer_to_litter_p         (:) ! pft level: gap mortality-associated flux: live stem transfer C to litter C (gC m-2 s-1)
   real(r8), allocatable :: m_deadstemc_xfer_to_litter_p         (:) ! pft level: gap mortality-associated flux: dead stem transfer C to litter C (gC m-2 s-1)
   real(r8), allocatable :: m_livecrootc_xfer_to_litter_p        (:) ! pft level: gap mortality-associated flux: live coarse root transfer C to litter C (gC m-2 s-1)
   real(r8), allocatable :: m_deadcrootc_xfer_to_litter_p        (:) ! pft level: gap mortality-associated flux: dead coarse root transfer C to litter C (gC m-2 s-1)
   real(r8), allocatable :: m_gresp_xfer_to_litter_p             (:) ! pft level: gap mortality-associated flux: growth respiration transfer C to litter C (gC m-2 s-1)

   real(r8), allocatable :: m_leafc_to_fire_p                    (:) ! pft level: fire mortality-associated flux: leaf display C to fire emissions (gC m-2 s-1)
   real(r8), allocatable :: m_frootc_to_fire_p                   (:) ! pft level: fire mortality-associated flux: fine root display C to fire emissions (gC m-2 s-1)
   real(r8), allocatable :: m_livestemc_to_fire_p                (:) ! pft level: fire mortality-associated flux: live stem display C to fire emissions (gC m-2 s-1)
   real(r8), allocatable :: m_deadstemc_to_fire_p                (:) ! pft level: fire mortality-associated flux: dead stem display C to fire emissions (gC m-2 s-1)
   real(r8), allocatable :: m_livecrootc_to_fire_p               (:) ! pft level: fire mortality-associated flux: live coarse root display C to fire emissions (gC m-2 s-1)
   real(r8), allocatable :: m_deadcrootc_to_fire_p               (:) ! pft level: fire mortality-associated flux: dead coarse root display C to fire emissions (gC m-2 s-1)

   real(r8), allocatable :: m_leafc_storage_to_fire_p            (:) ! pft level: fire mortality-associated flux: leaf storage C to fire emissions (gC m-2 s-1)
   real(r8), allocatable :: m_frootc_storage_to_fire_p           (:) ! pft level: fire mortality-associated flux: fine root storage C to fire emissions (gC m-2 s-1)
   real(r8), allocatable :: m_livestemc_storage_to_fire_p        (:) ! pft level: fire mortality-associated flux: live stem storage C to fire emissions (gC m-2 s-1)
   real(r8), allocatable :: m_deadstemc_storage_to_fire_p        (:) ! pft level: fire mortality-associated flux: dead stem storage C to fire emissions (gC m-2 s-1)
   real(r8), allocatable :: m_livecrootc_storage_to_fire_p       (:) ! pft level: fire mortality-associated flux: live coarse root storage C to fire emissions (gC m-2 s-1)
   real(r8), allocatable :: m_deadcrootc_storage_to_fire_p       (:) ! pft level: fire mortality-associated flux: dead coarse root storage C to fire emissions (gC m-2 s-1)
   real(r8), allocatable :: m_gresp_storage_to_fire_p            (:) ! pft level: fire mortality-associated flux: growth respiration storage C to fire emissions (gC m-2 s-1)

   real(r8), allocatable :: m_leafc_xfer_to_fire_p               (:) ! pft level: fire mortality-associated flux: leaf transfer C to fire emissions (gC m-2 s-1)
   real(r8), allocatable :: m_frootc_xfer_to_fire_p              (:) ! pft level: fire mortality-associated flux: fine root transfer C to fire emissions (gC m-2 s-1)
   real(r8), allocatable :: m_livestemc_xfer_to_fire_p           (:) ! pft level: fire mortality-associated flux: live stem transfer C to fire emissions (gC m-2 s-1)
   real(r8), allocatable :: m_deadstemc_xfer_to_fire_p           (:) ! pft level: fire mortality-associated flux: dead stem transfer C to fire emissions (gC m-2 s-1)
   real(r8), allocatable :: m_livecrootc_xfer_to_fire_p          (:) ! pft level: fire mortality-associated flux: live coarse root transfer C to fire emissions (gC m-2 s-1)
   real(r8), allocatable :: m_deadcrootc_xfer_to_fire_p          (:) ! pft level: fire mortality-associated flux: dead coarse root transfer C to fire emissions (gC m-2 s-1)
   real(r8), allocatable :: m_gresp_xfer_to_fire_p               (:) ! pft level: fire mortality-associated flux: growth respiration transfer C to fire emissions (gC m-2 s-1)

   real(r8), allocatable :: m_livestemc_to_deadstemc_fire_p      (:) ! pft level: fire mortality-associated flux: live stem display C to dead stem display C due to fire (gC m-2 s-1)
   real(r8), allocatable :: m_livecrootc_to_deadcrootc_fire_p    (:) ! pft level: fire mortality-associated flux: live coarse root display C to dead coarse root display C due to fire (gC m-2 s-1)

   real(r8), allocatable :: m_leafc_to_litter_fire_p             (:) ! pft level: fire mortality-associated flux: leaf display C to litter C due to fire (gC m-2 s-1)
   real(r8), allocatable :: m_frootc_to_litter_fire_p            (:) ! pft level: fire mortality-associated flux: fine root display C to litter C due to fire (gC m-2 s-1)
   real(r8), allocatable :: m_livestemc_to_litter_fire_p         (:) ! pft level: fire mortality-associated flux: live stem display C to litter C due to fire (gC m-2 s-1)
   real(r8), allocatable :: m_deadstemc_to_litter_fire_p         (:) ! pft level: fire mortality-associated flux: dead stem display C to litter C due to fire (gC m-2 s-1)
   real(r8), allocatable :: m_livecrootc_to_litter_fire_p        (:) ! pft level: fire mortality-associated flux: live coarse root display C to litter C due to fire (gC m-2 s-1)
   real(r8), allocatable :: m_deadcrootc_to_litter_fire_p        (:) ! pft level: fire mortality-associated flux: dead coarse root display C to litter C due to fire (gC m-2 s-1)

   real(r8), allocatable :: m_leafc_storage_to_litter_fire_p     (:) ! pft level: fire mortality-associated flux: leaf storage C to litter C due to fire (gC m-2 s-1)
   real(r8), allocatable :: m_frootc_storage_to_litter_fire_p    (:) ! pft level: fire mortality-associated flux: fine root storage C to litter C due to fire (gC m-2 s-1)
   real(r8), allocatable :: m_livestemc_storage_to_litter_fire_p (:) ! pft level: fire mortality-associated flux: live stem storage C to litter C due to fire (gC m-2 s-1)
   real(r8), allocatable :: m_deadstemc_storage_to_litter_fire_p (:) ! pft level: fire mortality-associated flux: dead stem storage C to litter C due to fire (gC m-2 s-1)
   real(r8), allocatable :: m_livecrootc_storage_to_litter_fire_p(:) ! pft level: fire mortality-associated flux: live coarse root storage C to litter C due to fire (gC m-2 s-1)
   real(r8), allocatable :: m_deadcrootc_storage_to_litter_fire_p(:) ! pft level: fire mortality-associated flux: dead coarse root storage C to litter C due to fire (gC m-2 s-1)
   real(r8), allocatable :: m_gresp_storage_to_litter_fire_p     (:) ! pft level: fire mortality-associated flux: growth respiration storage C to litter C due to fire (gC m-2 s-1)

   real(r8), allocatable :: m_leafc_xfer_to_litter_fire_p        (:) ! pft level: fire mortality-associated flux: leaf transfer C to litter C due to fire (gC m-2 s-1)
   real(r8), allocatable :: m_frootc_xfer_to_litter_fire_p       (:) ! pft level: fire mortality-associated flux: fine root transfer C to litter C due to fire (gC m-2 s-1)
   real(r8), allocatable :: m_livestemc_xfer_to_litter_fire_p    (:) ! pft level: fire mortality-associated flux: live stem transfer C to litter C due to fire (gC m-2 s-1)
   real(r8), allocatable :: m_deadstemc_xfer_to_litter_fire_p    (:) ! pft level: fire mortality-associated flux: dead stem transfer C to litter C due to fire (gC m-2 s-1)
   real(r8), allocatable :: m_livecrootc_xfer_to_litter_fire_p   (:) ! pft level: fire mortality-associated flux: live coarse root transfer C to litter C due to fire (gC m-2 s-1)
   real(r8), allocatable :: m_deadcrootc_xfer_to_litter_fire_p   (:) ! pft level: fire mortality-associated flux: dead coarse root transfer C to litter C due to fire (gC m-2 s-1)
   real(r8), allocatable :: m_gresp_xfer_to_litter_fire_p        (:) ! pft level: fire mortality-associated flux: growth respiration transfer C to litter C due to fire (gC m-2 s-1)

   real(r8), allocatable :: cpool_to_xsmrpool_p          (:) ! pft level: allocation-associated flux: available C allocated to maintenance respiration storage C (gC m-2 s-1)
   real(r8), allocatable :: cpool_to_gresp_storage_p     (:) ! pft level: allocation-associated flux: available C allocated to growth respiration storage C (gC m-2 s-1)
   real(r8), allocatable :: cpool_to_leafc_p             (:) ! pft level: allocation-associated flux: available C allocated to leaf display C (gC m-2 s-1)
   real(r8), allocatable :: cpool_to_leafc_storage_p     (:) ! pft level: allocation-associated flux: available C allocated to leaf storage C (gC m-2 s-1)
   real(r8), allocatable :: cpool_to_frootc_p            (:) ! pft level: allocation-associated flux: available C allocated to fine root display C  (gC m-2 s-1)
   real(r8), allocatable :: cpool_to_frootc_storage_p    (:) ! pft level: allocation-associated flux: available C allocated to fine root storage C (gC m-2 s-1)
   real(r8), allocatable :: cpool_to_livestemc_p         (:) ! pft level: allocation-associated flux: available C allocated to live stem display C (gC m-2 s-1)
   real(r8), allocatable :: cpool_to_livestemc_storage_p (:) ! pft level: allocation-associated flux: available C allocated to live stem storage C (gC m-2 s-1)
   real(r8), allocatable :: cpool_to_deadstemc_p         (:) ! pft level: allocation-associated flux: available C allocated to dead stem display C (gC m-2 s-1)
   real(r8), allocatable :: cpool_to_deadstemc_storage_p (:) ! pft level: allocation-associated flux: available C allocated to dead stem storage C (gC m-2 s-1)
   real(r8), allocatable :: cpool_to_livecrootc_p        (:) ! pft level: allocation-associated flux: available C allocated to live coarse display C (gC m-2 s-1)
   real(r8), allocatable :: cpool_to_livecrootc_storage_p(:) ! pft level: allocation-associated flux: available C allocated to live coarse storage C (gC m-2 s-1)
   real(r8), allocatable :: cpool_to_deadcrootc_p        (:) ! pft level: allocation-associated flux: available C allocated to dead coarse root display C (gC m-2 s-1)
   real(r8), allocatable :: cpool_to_deadcrootc_storage_p(:) ! pft level: allocation-associated flux: available C allocated to dead coarse root storage C (gC m-2 s-1)
   real(r8), allocatable :: cpool_to_grainc_p            (:) ! pft level: allocation-associated flux: available C allocated to grain display C (gC m-2 s-1)
   real(r8), allocatable :: cpool_to_grainc_storage_p    (:) ! pft level: allocation-associated flux: available C allocated to grain storage C (gC m-2 s-1)

   real(r8), allocatable :: leaf_xsmr_p                  (:) ! pft level: leaf maintenance respiration storage C due to available C deficit (gC m-2 s-1)
   real(r8), allocatable :: froot_xsmr_p                 (:) ! pft level: fine root maintenance respiration storage C due to available C deficit (gC m-2 s-1)
   real(r8), allocatable :: livestem_xsmr_p              (:) ! pft level: live stem maintenance respiration storage C due to available C deficit (gC m-2 s-1)
   real(r8), allocatable :: livecroot_xsmr_p             (:) ! pft level: live coarse root maintenance respiration storage C due to available C deficit (gC m-2 s-1)
   real(r8), allocatable :: grain_xsmr_p                 (:) ! pft level: grain maintenance respiration storage C due to available C deficit (gC m-2 s-1)

   real(r8), allocatable :: cpool_leaf_gr_p                      (:) ! pft level: allocation-associated flux: available C allocated to leaf display growth respiration (gC m-2 s-1)
   real(r8), allocatable :: cpool_froot_gr_p                     (:) ! pft level: allocation-associated flux: available C allocated to fine root display growth respiration (gC m-2 s-1)
   real(r8), allocatable :: cpool_livestem_gr_p                  (:) ! pft level: allocation-associated flux: available C allocated to live stem display growth respiration (gC m-2 s-1)
   real(r8), allocatable :: cpool_deadstem_gr_p                  (:) ! pft level: allocation-associated flux: available C allocated to dead stem display growth respiration (gC m-2 s-1)
   real(r8), allocatable :: cpool_livecroot_gr_p                 (:) ! pft level: allocation-associated flux: available C allocated to live coarse display growth respiration (gC m-2 s-1)
   real(r8), allocatable :: cpool_deadcroot_gr_p                 (:) ! pft level: allocation-associated flux: available C allocated to dead coarse display growth respiration (gC m-2 s-1)
   real(r8), allocatable :: cpool_grain_gr_p                     (:) ! pft level: allocation-associated flux: available C allocated to grain display growth respiration (gC m-2 s-1)

   real(r8), allocatable :: cpool_leaf_storage_gr_p              (:) ! pft level: allocation-associated flux: available C allocated to leaf storage growth respiration (gC m-2 s-1)
   real(r8), allocatable :: cpool_froot_storage_gr_p             (:) ! pft level: allocation-associated flux: available C allocated to fine root storage growth respiration (gC m-2 s-1)
   real(r8), allocatable :: cpool_livestem_storage_gr_p          (:) ! pft level: allocation-associated flux: available C allocated to live stem storage growth respiration (gC m-2 s-1)
   real(r8), allocatable :: cpool_deadstem_storage_gr_p          (:) ! pft level: allocation-associated flux: available C allocated to dead stem storage growth respiration (gC m-2 s-1)
   real(r8), allocatable :: cpool_livecroot_storage_gr_p         (:) ! pft level: allocation-associated flux: available C allocated to live coarse storage growth respiration (gC m-2 s-1)
   real(r8), allocatable :: cpool_deadcroot_storage_gr_p         (:) ! pft level: allocation-associated flux: available C allocated to dead coarse storage growth respiration (gC m-2 s-1)
   real(r8), allocatable :: cpool_grain_storage_gr_p             (:) ! pft level: allocation-associated flux: available C allocated to grain storage growth respiration (gC m-2 s-1)

   real(r8), allocatable :: transfer_leaf_gr_p                   (:) ! pft level: allocation-associated flux: available C allocated to leaf transfer growth respiration (gC m-2 s-1)
   real(r8), allocatable :: transfer_froot_gr_p                  (:) ! pft level: allocation-associated flux: available C allocated to fine root transfer growth respiration (gC m-2 s-1)
   real(r8), allocatable :: transfer_livestem_gr_p               (:) ! pft level: allocation-associated flux: available C allocated to live stem transfer growth respiration (gC m-2 s-1)
   real(r8), allocatable :: transfer_deadstem_gr_p               (:) ! pft level: allocation-associated flux: available C allocated to dead stem transfer growth respiration (gC m-2 s-1)
   real(r8), allocatable :: transfer_livecroot_gr_p              (:) ! pft level: allocation-associated flux: available C allocated to live coarse transfer growth respiration (gC m-2 s-1)
   real(r8), allocatable :: transfer_deadcroot_gr_p              (:) ! pft level: allocation-associated flux: available C allocated to dead coarse transfer growth respiration (gC m-2 s-1)
   real(r8), allocatable :: transfer_grain_gr_p                  (:) ! pft level: allocation-associated flux: available C allocated to grain transfer growth respiration (gC m-2 s-1)

   real(r8), allocatable :: xsmrpool_to_atm_p                    (:) ! pft level: maintenance respiration storage C to atmosphere due to harvest (gC m-2 s-1)

   real(r8), allocatable :: cropprod1c_loss_p                    (:) ! pft level: product loss (gC m-2 s-1)

   real(r8), allocatable :: plant_ndemand_p                      (:) ! pft level: plant potential demand N (gN m-2 s-1)

   real(r8), allocatable :: leafn_xfer_to_leafn_p                (:) ! pft level: phenology-associated flux: leaf transfer N to display N (gN m-2 s-1)
   real(r8), allocatable :: frootn_xfer_to_frootn_p              (:) ! pft level: phenology-associated flux: fine root transfer N to display N (gN m-2 s-1)
   real(r8), allocatable :: livestemn_xfer_to_livestemn_p        (:) ! pft level: phenology-associated flux: live stem transfer N to display N (gN m-2 s-1)
   real(r8), allocatable :: deadstemn_xfer_to_deadstemn_p        (:) ! pft level: phenology-associated flux: dead stem transfer N to display N (gN m-2 s-1)
   real(r8), allocatable :: livecrootn_xfer_to_livecrootn_p      (:) ! pft level: phenology-associated flux: live coarse root transfer N to display N (gN m-2 s-1)
   real(r8), allocatable :: deadcrootn_xfer_to_deadcrootn_p      (:) ! pft level: phenology-associated flux: dead coarse root transfer N to display N (gN m-2 s-1)
   real(r8), allocatable :: grainn_xfer_to_grainn_p              (:) ! pft level: phenology-associated flux: grain transfer N to display N (gN m-2 s-1)

   real(r8), allocatable :: leafn_storage_to_xfer_p              (:) ! pft level: phenology-associated flux: leaf storage N to transfer N (gN m-2 s-1)
   real(r8), allocatable :: frootn_storage_to_xfer_p             (:) ! pft level: phenology-associated flux: fine root storage N to transfer N (gN m-2 s-1)
   real(r8), allocatable :: livestemn_storage_to_xfer_p          (:) ! pft level: phenology-associated flux: live stem storage N to transfer N (gN m-2 s-1)
   real(r8), allocatable :: deadstemn_storage_to_xfer_p          (:) ! pft level: phenology-associated flux: dead stem storage N to transfer N (gN m-2 s-1)
   real(r8), allocatable :: livecrootn_storage_to_xfer_p         (:) ! pft level: phenology-associated flux: live coarse root storage N to transfer N (gN m-2 s-1)
   real(r8), allocatable :: deadcrootn_storage_to_xfer_p         (:) ! pft level: phenology-associated flux: dead coarse root storage N to transfer N (gN m-2 s-1)
   real(r8), allocatable :: grainn_storage_to_xfer_p             (:) ! pft level: phenology-associated flux: grain storage N to transfer N (gN m-2 s-1)

   real(r8), allocatable :: leafn_to_litter_p                    (:) ! pft level: phenology-associated flux: leaf display N to litter N (gN m-2 s-1)
   real(r8), allocatable :: frootn_to_litter_p                   (:) ! pft level: phenology-associated flux: fine root display N to litter N (gN m-2 s-1)
   real(r8), allocatable :: grainn_to_food_p                     (:) ! pft level: phenology-associated flux: grain display N to product N (gN m-2 s-1)
   real(r8), allocatable :: grainn_to_seed_p                     (:) ! pft level: phenology-associated flux: grain display N to seed N (gN m-2 s-1)
   real(r8), allocatable :: crop_seedn_to_leaf_p                 (:) ! pft level: phenology-associated flux: seed N to leaf display N (gN m-2 s-1)
   real(r8), allocatable :: livestemn_to_litter_p                (:) ! pft level: phenology-associated flux: live stem display N to litter N (gN m-2 s-1)
   real(r8), allocatable :: livestemn_to_deadstemn_p             (:) ! pft level: phenology-associated flux: live stem display N to dead stem display N (gN m-2 s-1)
   real(r8), allocatable :: livecrootn_to_deadcrootn_p           (:) ! pft level: phenology-associated flux: live coarse root display N to dead coarse root display N (gN m-2 s-1)

   real(r8), allocatable :: leafn_to_retransn_p                  (:) ! pft level: phenology-associated flux: leaf display N to retranslocated N (gN m-2 s-1)
   real(r8), allocatable :: frootn_to_retransn_p                 (:) ! pft level: phenology-associated flux: fine root display N to retranslocated N (gN m-2 s-1)
   real(r8), allocatable :: livestemn_to_retransn_p              (:) ! pft level: phenology-associated flux: live stem display N to retranslocated N (gN m-2 s-1)
   real(r8), allocatable :: livecrootn_to_retransn_p             (:) ! pft level: phenology-associated flux: live coarse root display N to retranslocated N (gN m-2 s-1)
   real(r8), allocatable :: retransn_to_npool_p                  (:) ! pft level: phenology-associated flux: retranslocated N to available N (gN m-2 s-1)
   real(r8), allocatable :: free_retransn_to_npool_p             (:) ! pft level: phenology-associated flux: retranslocated N to available N (gN m-2 s-1)

   real(r8), allocatable :: m_leafn_to_litter_p                  (:) ! pft level: gap mortality-associated flux: leaf display N to litter N (gN m-2 s-1)
   real(r8), allocatable :: m_frootn_to_litter_p                 (:) ! pft level: gap mortality-associated flux: fine root display N to litter N (gN m-2 s-1)
   real(r8), allocatable :: m_livestemn_to_litter_p              (:) ! pft level: gap mortality-associated flux: live stem display N to litter N (gN m-2 s-1)
   real(r8), allocatable :: m_deadstemn_to_litter_p              (:) ! pft level: gap mortality-associated flux: dead stem display N to litter N (gN m-2 s-1)
   real(r8), allocatable :: m_livecrootn_to_litter_p             (:) ! pft level: gap mortality-associated flux: live coarse root display N to litter N (gN m-2 s-1)
   real(r8), allocatable :: m_deadcrootn_to_litter_p             (:) ! pft level: gap mortality-associated flux: dead coarse root display N to litter N (gN m-2 s-1)
   real(r8), allocatable :: m_retransn_to_litter_p               (:) ! pft level: gap mortality-associated flux: retranslocated N to litter N (gN m-2 s-1)

   real(r8), allocatable :: m_leafn_storage_to_litter_p          (:) ! pft level: gap mortality-associated flux: leaf storage N to litter N (gN m-2 s-1)
   real(r8), allocatable :: m_frootn_storage_to_litter_p         (:) ! pft level: gap mortality-associated flux: fine root storage N to litter N (gN m-2 s-1)
   real(r8), allocatable :: m_livestemn_storage_to_litter_p      (:) ! pft level: gap mortality-associated flux: live stem storage N to litter N (gN m-2 s-1)
   real(r8), allocatable :: m_deadstemn_storage_to_litter_p      (:) ! pft level: gap mortality-associated flux: dead stem storage N to litter N (gN m-2 s-1)
   real(r8), allocatable :: m_livecrootn_storage_to_litter_p     (:) ! pft level: gap mortality-associated flux: live coarse root storage N to litter N (gN m-2 s-1)
   real(r8), allocatable :: m_deadcrootn_storage_to_litter_p     (:) ! pft level: gap mortality-associated flux: dead coarse root storage N to litter N (gN m-2 s-1)

   real(r8), allocatable :: m_leafn_xfer_to_litter_p             (:) ! pft level: gap mortality-associated flux: leaf transfer N to litter N (gN m-2 s-1)
   real(r8), allocatable :: m_frootn_xfer_to_litter_p            (:) ! pft level: gap mortality-associated flux: fine root transfer N to litter N (gN m-2 s-1)
   real(r8), allocatable :: m_livestemn_xfer_to_litter_p         (:) ! pft level: gap mortality-associated flux: live stem transfer N to litter N (gN m-2 s-1)
   real(r8), allocatable :: m_deadstemn_xfer_to_litter_p         (:) ! pft level: gap mortality-associated flux: dead stem transfer N to litter N (gN m-2 s-1)
   real(r8), allocatable :: m_livecrootn_xfer_to_litter_p        (:) ! pft level: gap mortality-associated flux: live coarse root transfer N to litter N (gN m-2 s-1)
   real(r8), allocatable :: m_deadcrootn_xfer_to_litter_p        (:) ! pft level: gap mortality-associated flux: dead coarse root transfer N to litter N (gN m-2 s-1)

   real(r8), allocatable :: m_leafn_to_fire_p                    (:) ! pft level: fire mortality-associated flux: leaf display N to fire emissions (gN m-2 s-1)
   real(r8), allocatable :: m_frootn_to_fire_p                   (:) ! pft level: fire mortality-associated flux: fine root display N to fire emissions (gN m-2 s-1)
   real(r8), allocatable :: m_livestemn_to_fire_p                (:) ! pft level: fire mortality-associated flux: live stem display N to fire emissions (gN m-2 s-1)
   real(r8), allocatable :: m_deadstemn_to_fire_p                (:) ! pft level: fire mortality-associated flux: dead stem display N to fire emissions (gN m-2 s-1)
   real(r8), allocatable :: m_livecrootn_to_fire_p               (:) ! pft level: fire mortality-associated flux: live coarse root display N to fire emissions (gN m-2 s-1)
   real(r8), allocatable :: m_deadcrootn_to_fire_p               (:) ! pft level: fire mortality-associated flux: dead coarse root display N to fire emissions (gN m-2 s-1)

   real(r8), allocatable :: m_leafn_storage_to_fire_p            (:) ! pft level: fire mortality-associated flux: leaf storage N to fire emissions (gN m-2 s-1)
   real(r8), allocatable :: m_frootn_storage_to_fire_p           (:) ! pft level: fire mortality-associated flux: fine root storage N to fire emissions (gN m-2 s-1)
   real(r8), allocatable :: m_livestemn_storage_to_fire_p        (:) ! pft level: fire mortality-associated flux: live stem storage N to fire emissions (gN m-2 s-1)
   real(r8), allocatable :: m_deadstemn_storage_to_fire_p        (:) ! pft level: fire mortality-associated flux: dead stem storage N to fire emissions (gN m-2 s-1)
   real(r8), allocatable :: m_livecrootn_storage_to_fire_p       (:) ! pft level: fire mortality-associated flux: live coarse root storage N to fire emissions (gN m-2 s-1)
   real(r8), allocatable :: m_deadcrootn_storage_to_fire_p       (:) ! pft level: fire mortality-associated flux: dead coarse root storage N to fire emissions (gN m-2 s-1)

   real(r8), allocatable :: m_leafn_xfer_to_fire_p               (:) ! pft level: fire mortality-associated flux: leaf transfer N to fire emissions (gN m-2 s-1)
   real(r8), allocatable :: m_frootn_xfer_to_fire_p              (:) ! pft level: fire mortality-associated flux: fine root transfer N to fire emissions (gN m-2 s-1)
   real(r8), allocatable :: m_livestemn_xfer_to_fire_p           (:) ! pft level: fire mortality-associated flux: live stem transfer N to fire emissions (gN m-2 s-1)
   real(r8), allocatable :: m_deadstemn_xfer_to_fire_p           (:) ! pft level: fire mortality-associated flux: dead stem transfer N to fire emissions (gN m-2 s-1)
   real(r8), allocatable :: m_livecrootn_xfer_to_fire_p          (:) ! pft level: fire mortality-associated flux: live coarse root transfer N to fire emissions (gN m-2 s-1)
   real(r8), allocatable :: m_deadcrootn_xfer_to_fire_p          (:) ! pft level: fire mortality-associated flux: dead coarse root transfer N to fire emissions (gN m-2 s-1)

   real(r8), allocatable :: m_livestemn_to_deadstemn_fire_p      (:) ! pft level: fire mortality-associated flux: live stem display N to dead stem display N due to fire (gN m-2 s-1)
   real(r8), allocatable :: m_livecrootn_to_deadcrootn_fire_p    (:) ! pft level: fire mortality-associated flux: live coarse root display N to dead coarse root display N due to fire (gN m-2 s-1)

   real(r8), allocatable :: m_retransn_to_fire_p                 (:) ! pft level: fire mortality-associated flux: retranslocated N to fire emissions (gN m-2 s-1)

   real(r8), allocatable :: m_leafn_to_litter_fire_p             (:) ! pft level: fire mortality-associated flux: leaf display N to litter N due to fire (gN m-2 s-1)
   real(r8), allocatable :: m_frootn_to_litter_fire_p            (:) ! pft level: fire mortality-associated flux: fine root display N to litter N due to fire (gN m-2 s-1)
   real(r8), allocatable :: m_livestemn_to_litter_fire_p         (:) ! pft level: fire mortality-associated flux: live stem display N to litter N due to fire (gN m-2 s-1)
   real(r8), allocatable :: m_deadstemn_to_litter_fire_p         (:) ! pft level: fire mortality-associated flux: dead stem display N to litter N due to fire (gN m-2 s-1)
   real(r8), allocatable :: m_livecrootn_to_litter_fire_p        (:) ! pft level: fire mortality-associated flux: live coarse root display N to litter N due to fire (gN m-2 s-1)
   real(r8), allocatable :: m_deadcrootn_to_litter_fire_p        (:) ! pft level: fire mortality-associated flux: dead coarse root display N to litter N due to fire (gN m-2 s-1)

   real(r8), allocatable :: m_leafn_storage_to_litter_fire_p     (:) ! pft level: fire mortality-associated flux: leaf storage N to litter N due to fire (gN m-2 s-1)
   real(r8), allocatable :: m_frootn_storage_to_litter_fire_p    (:) ! pft level: fire mortality-associated flux: fine root storage N to litter N due to fire (gN m-2 s-1)
   real(r8), allocatable :: m_livestemn_storage_to_litter_fire_p (:) ! pft level: fire mortality-associated flux: live stem storage N to litter N due to fire (gN m-2 s-1)
   real(r8), allocatable :: m_deadstemn_storage_to_litter_fire_p (:) ! pft level: fire mortality-associated flux: dead stem storage N to litter N due to fire (gN m-2 s-1)
   real(r8), allocatable :: m_livecrootn_storage_to_litter_fire_p(:) ! pft level: fire mortality-associated flux: live coarse root storage N to litter N due to fire (gN m-2 s-1)
   real(r8), allocatable :: m_deadcrootn_storage_to_litter_fire_p(:) ! pft level: fire mortality-associated flux: dead coarse root storage N to litter N due to fire (gN m-2 s-1)

   real(r8), allocatable :: m_leafn_xfer_to_litter_fire_p        (:) ! pft level: fire mortality-associated flux: leaf transfer N to litter N due to fire (gN m-2 s-1)
   real(r8), allocatable :: m_frootn_xfer_to_litter_fire_p       (:) ! pft level: fire mortality-associated flux: fine root transfer N to litter N due to fire (gN m-2 s-1)
   real(r8), allocatable :: m_livestemn_xfer_to_litter_fire_p    (:) ! pft level: fire mortality-associated flux: live stem transfer N to litter N due to fire (gN m-2 s-1)
   real(r8), allocatable :: m_deadstemn_xfer_to_litter_fire_p    (:) ! pft level: fire mortality-associated flux: dead stem transfer N to litter N due to fire (gN m-2 s-1)
   real(r8), allocatable :: m_livecrootn_xfer_to_litter_fire_p   (:) ! pft level: fire mortality-associated flux: live coarse root transfer N to litter N due to fire (gN m-2 s-1)
   real(r8), allocatable :: m_deadcrootn_xfer_to_litter_fire_p   (:) ! pft level: fire mortality-associated flux: dead coarse root transfer N to litter N due to fire (gN m-2 s-1)

   real(r8), allocatable :: m_retransn_to_litter_fire_p          (:) ! pft level: fire mortality-associated flux: retranslocated N to litter N due to fire (gN m-2 s-1)

   real(r8), allocatable :: npool_to_leafn_p                     (:) ! pft level: allocation-associated flux: available N allocated to leaf display N (gN m-2 s-1)
   real(r8), allocatable :: npool_to_leafn_storage_p             (:) ! pft level: allocation-associated flux: available N allocated to leaf storage N (gN m-2 s-1)
   real(r8), allocatable :: npool_to_frootn_p                    (:) ! pft level: allocation-associated flux: available N allocated to fine root display N  (gN m-2 s-1)
   real(r8), allocatable :: npool_to_frootn_storage_p            (:) ! pft level: allocation-associated flux: available N allocated to fine root storage N (gN m-2 s-1)
   real(r8), allocatable :: npool_to_livestemn_p                 (:) ! pft level: allocation-associated flux: available N allocated to live stem display N (gN m-2 s-1)
   real(r8), allocatable :: npool_to_livestemn_storage_p         (:) ! pft level: allocation-associated flux: available N allocated to live stem storage N (gN m-2 s-1)
   real(r8), allocatable :: npool_to_deadstemn_p                 (:) ! pft level: allocation-associated flux: available N allocated to dead stem display N (gN m-2 s-1)
   real(r8), allocatable :: npool_to_deadstemn_storage_p         (:) ! pft level: allocation-associated flux: available N allocated to dead stem storage N (gN m-2 s-1)
   real(r8), allocatable :: npool_to_livecrootn_p                (:) ! pft level: allocation-associated flux: available N allocated to live coarse display N (gN m-2 s-1)
   real(r8), allocatable :: npool_to_livecrootn_storage_p        (:) ! pft level: allocation-associated flux: available N allocated to live coarse storage N (gN m-2 s-1)
   real(r8), allocatable :: npool_to_deadcrootn_p                (:) ! pft level: allocation-associated flux: available N allocated to dead coarse root display N (gN m-2 s-1)
   real(r8), allocatable :: npool_to_deadcrootn_storage_p        (:) ! pft level: allocation-associated flux: available N allocated to dead coarse root storage N (gN m-2 s-1)
   real(r8), allocatable :: npool_to_grainn_p                    (:) ! pft level: allocation-associated flux: available N allocated to grain display N (gN m-2 s-1)
   real(r8), allocatable :: npool_to_grainn_storage_p            (:) ! pft level: allocation-associated flux: available N allocated to grain storage N (gN m-2 s-1)

   real(r8), allocatable :: respcsun_p                   (:) ! pft level: sunlit leaf respiration (gC m-2 s-1)
   real(r8), allocatable :: respcsha_p                   (:) ! pft level: shaded leaf respiration (gC m-2 s-1)
   real(r8), allocatable :: leaf_mr_p                    (:) ! pft level: leaf maintenance respiration (gC m-2 s-1)
   real(r8), allocatable :: froot_mr_p                   (:) ! pft level: fine root maintenance respiration (gC m-2 s-1)
   real(r8), allocatable :: livestem_mr_p                (:) ! pft level: live stem maintenance respiration (gC m-2 s-1)
   real(r8), allocatable :: livecroot_mr_p               (:) ! pft level: live coarse root maintenance respiration (gC m-2 s-1)
   real(r8), allocatable :: grain_mr_p                   (:) ! pft level: grain maintenance respiration (gC m-2 s-1)

   real(r8), allocatable :: soil_change_p                (:) ! pft level: soil carbon used by FUN (gC m-2 s-1)

   real(r8), allocatable :: psn_to_cpool_p               (:) ! pft level: photosynthesis rate (gC m-2 s-1)
   real(r8), allocatable :: gpp_p                        (:) ! pft level: gross primary production (gC m-2 s-1)
   real(r8), allocatable :: availc_p                     (:) ! pft level: available C (gC m-2 s-1)
   real(r8), allocatable :: avail_retransn_p             (:) ! pft level: available retranslocated N (gN m-2 s-1)
   real(r8), allocatable :: xsmrpool_recover_p           (:) ! pft level: available C to maintenance respiration storage C to recover previous excess mainte
   real(r8), allocatable :: excess_cflux_p               (:) ! pft level: excess C due to N limitation (gC m-2 s-1)
   real(r8), allocatable :: sminn_to_npool_p             (:) ! pft level: soil mineral N uptake for plant growth (gN m-2 s-1)

   real(r8), allocatable :: plant_calloc_p               (:) ! pft level: actual available C for plant grwoth (gC m-2 s-1)
   real(r8), allocatable :: plant_nalloc_p               (:) ! pft level: actual available N for plant growth (gN m-2 s-1)
   real(r8), allocatable :: leaf_curmr_p                 (:) ! pft level: leaf maintenance respiration from current available C (gC m-2 s-1)
   real(r8), allocatable :: froot_curmr_p                (:) ! pft level: fine root maintenance respiration from current available C (gC m-2 s-1)
   real(r8), allocatable :: livestem_curmr_p             (:) ! pft level: live stem maintenance respiration from current available C (gC m-2 s-1)
   real(r8), allocatable :: livecroot_curmr_p            (:) ! pft level: live coarse root maintenance respiration from current available C (gC m-2 s-1)
   real(r8), allocatable :: grain_curmr_p                (:) ! pft level: grain maintenance respiration from current available C (gC m-2 s-1)

   real(r8), allocatable :: fire_closs_p                 (:) ! pft level: total C emissions due to fire (gC m-2 s-1)
   real(r8), allocatable :: fire_nloss_p                 (:) ! pft level: total N emissions due to fire (gN m-2 s-1)
   real(r8), allocatable :: wood_harvestc_p              (:) ! pft level: harvested wood C (gC m-2 s-1)
   real(r8), allocatable :: wood_harvestn_p              (:) ! pft level: harvested wood N (gN m-2 s-1)
   real(r8), allocatable :: grainc_to_cropprodc_p        (:) ! pft level: harvested grain C (gC m-2 s-1)
   real(r8), allocatable :: grainn_to_cropprodn_p        (:) ! pft level: harvested grain N (gN m-2 s-1)
   real(r8), allocatable :: hrv_xsmrpool_to_atm_p        (:) ! pft level: maintenance respiration storage C to atmosphere due to harvest (gC m-2 s-1)
   real(r8), allocatable :: soyfixn_p                    (:) ! pft level: soybean fixed nitrogen rate (gN m-2 s-1)

 ! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: allocate_1D_BGCPFTFluxes
   PUBLIC :: deallocate_1D_BGCPFTFluxes
   PUBLIC :: set_1D_BGCPFTFluxes

! PRIVATE MEMBER FUNCTIONS:

!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------

   SUBROUTINE allocate_1D_BGCPFTFluxes
  ! --------------------------------------------------------------------
  ! Allocates memory for CoLM PFT 1d [numpft] variables
  ! --------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_MPAS_MPI
   USE MOD_LandPFT
   IMPLICIT NONE

      IF (.true.) THEN
         IF (numpft > 0) THEN

 ! bgc variables
            allocate (leafc_xfer_to_leafc_p                (numpft)) ; leafc_xfer_to_leafc_p                (:) = spval
            allocate (frootc_xfer_to_frootc_p              (numpft)) ; frootc_xfer_to_frootc_p              (:) = spval
            allocate (livestemc_xfer_to_livestemc_p        (numpft)) ; livestemc_xfer_to_livestemc_p        (:) = spval
            allocate (deadstemc_xfer_to_deadstemc_p        (numpft)) ; deadstemc_xfer_to_deadstemc_p        (:) = spval
            allocate (livecrootc_xfer_to_livecrootc_p      (numpft)) ; livecrootc_xfer_to_livecrootc_p      (:) = spval
            allocate (deadcrootc_xfer_to_deadcrootc_p      (numpft)) ; deadcrootc_xfer_to_deadcrootc_p      (:) = spval
            allocate (grainc_xfer_to_grainc_p              (numpft)) ; grainc_xfer_to_grainc_p              (:) = spval

            allocate (leafc_storage_to_xfer_p              (numpft)) ; leafc_storage_to_xfer_p              (:) = spval
            allocate (frootc_storage_to_xfer_p             (numpft)) ; frootc_storage_to_xfer_p             (:) = spval
            allocate (livestemc_storage_to_xfer_p          (numpft)) ; livestemc_storage_to_xfer_p          (:) = spval
            allocate (deadstemc_storage_to_xfer_p          (numpft)) ; deadstemc_storage_to_xfer_p          (:) = spval
            allocate (livecrootc_storage_to_xfer_p         (numpft)) ; livecrootc_storage_to_xfer_p         (:) = spval
            allocate (deadcrootc_storage_to_xfer_p         (numpft)) ; deadcrootc_storage_to_xfer_p         (:) = spval
            allocate (grainc_storage_to_xfer_p             (numpft)) ; grainc_storage_to_xfer_p             (:) = spval
            allocate (gresp_storage_to_xfer_p              (numpft)) ; gresp_storage_to_xfer_p              (:) = spval

            allocate (leafc_to_litter_p                    (numpft)) ; leafc_to_litter_p                    (:) = spval
            allocate (frootc_to_litter_p                   (numpft)) ; frootc_to_litter_p                   (:) = spval
            allocate (grainc_to_food_p                     (numpft)) ; grainc_to_food_p                     (:) = spval
            allocate (grainc_to_seed_p                     (numpft)) ; grainc_to_seed_p                     (:) = spval
            allocate (crop_seedc_to_leaf_p                 (numpft)) ; crop_seedc_to_leaf_p                 (:) = spval
            allocate (livestemc_to_litter_p                (numpft)) ; livestemc_to_litter_p                (:) = spval
            allocate (livestemc_to_deadstemc_p             (numpft)) ; livestemc_to_deadstemc_p             (:) = spval
            allocate (livecrootc_to_deadcrootc_p           (numpft)) ; livecrootc_to_deadcrootc_p           (:) = spval

            allocate (m_leafc_to_litter_p                  (numpft)) ; m_leafc_to_litter_p                  (:) = spval
            allocate (m_frootc_to_litter_p                 (numpft)) ; m_frootc_to_litter_p                 (:) = spval
            allocate (m_livestemc_to_litter_p              (numpft)) ; m_livestemc_to_litter_p              (:) = spval
            allocate (m_deadstemc_to_litter_p              (numpft)) ; m_deadstemc_to_litter_p              (:) = spval
            allocate (m_livecrootc_to_litter_p             (numpft)) ; m_livecrootc_to_litter_p             (:) = spval
            allocate (m_deadcrootc_to_litter_p             (numpft)) ; m_deadcrootc_to_litter_p             (:) = spval

            allocate (m_leafc_storage_to_litter_p          (numpft)) ; m_leafc_storage_to_litter_p          (:) = spval
            allocate (m_frootc_storage_to_litter_p         (numpft)) ; m_frootc_storage_to_litter_p         (:) = spval
            allocate (m_livestemc_storage_to_litter_p      (numpft)) ; m_livestemc_storage_to_litter_p      (:) = spval
            allocate (m_deadstemc_storage_to_litter_p      (numpft)) ; m_deadstemc_storage_to_litter_p      (:) = spval
            allocate (m_livecrootc_storage_to_litter_p     (numpft)) ; m_livecrootc_storage_to_litter_p     (:) = spval
            allocate (m_deadcrootc_storage_to_litter_p     (numpft)) ; m_deadcrootc_storage_to_litter_p     (:) = spval
            allocate (m_gresp_storage_to_litter_p          (numpft)) ; m_gresp_storage_to_litter_p          (:) = spval

            allocate (m_leafc_xfer_to_litter_p             (numpft)) ; m_leafc_xfer_to_litter_p             (:) = spval
            allocate (m_frootc_xfer_to_litter_p            (numpft)) ; m_frootc_xfer_to_litter_p            (:) = spval
            allocate (m_livestemc_xfer_to_litter_p         (numpft)) ; m_livestemc_xfer_to_litter_p         (:) = spval
            allocate (m_deadstemc_xfer_to_litter_p         (numpft)) ; m_deadstemc_xfer_to_litter_p         (:) = spval
            allocate (m_livecrootc_xfer_to_litter_p        (numpft)) ; m_livecrootc_xfer_to_litter_p        (:) = spval
            allocate (m_deadcrootc_xfer_to_litter_p        (numpft)) ; m_deadcrootc_xfer_to_litter_p        (:) = spval
            allocate (m_gresp_xfer_to_litter_p             (numpft)) ; m_gresp_xfer_to_litter_p             (:) = spval

            allocate (m_leafc_to_fire_p                    (numpft)) ; m_leafc_to_fire_p                    (:) = spval
            allocate (m_frootc_to_fire_p                   (numpft)) ; m_frootc_to_fire_p                   (:) = spval
            allocate (m_livestemc_to_fire_p                (numpft)) ; m_livestemc_to_fire_p                (:) = spval
            allocate (m_deadstemc_to_fire_p                (numpft)) ; m_deadstemc_to_fire_p                (:) = spval
            allocate (m_livecrootc_to_fire_p               (numpft)) ; m_livecrootc_to_fire_p               (:) = spval
            allocate (m_deadcrootc_to_fire_p               (numpft)) ; m_deadcrootc_to_fire_p               (:) = spval

            allocate (m_leafc_storage_to_fire_p            (numpft)) ; m_leafc_storage_to_fire_p            (:) = spval
            allocate (m_frootc_storage_to_fire_p           (numpft)) ; m_frootc_storage_to_fire_p           (:) = spval
            allocate (m_livestemc_storage_to_fire_p        (numpft)) ; m_livestemc_storage_to_fire_p        (:) = spval
            allocate (m_deadstemc_storage_to_fire_p        (numpft)) ; m_deadstemc_storage_to_fire_p        (:) = spval
            allocate (m_livecrootc_storage_to_fire_p       (numpft)) ; m_livecrootc_storage_to_fire_p       (:) = spval
            allocate (m_deadcrootc_storage_to_fire_p       (numpft)) ; m_deadcrootc_storage_to_fire_p       (:) = spval
            allocate (m_gresp_storage_to_fire_p            (numpft)) ; m_gresp_storage_to_fire_p            (:) = spval

            allocate (m_leafc_xfer_to_fire_p               (numpft)) ; m_leafc_xfer_to_fire_p               (:) = spval
            allocate (m_frootc_xfer_to_fire_p              (numpft)) ; m_frootc_xfer_to_fire_p              (:) = spval
            allocate (m_livestemc_xfer_to_fire_p           (numpft)) ; m_livestemc_xfer_to_fire_p           (:) = spval
            allocate (m_deadstemc_xfer_to_fire_p           (numpft)) ; m_deadstemc_xfer_to_fire_p           (:) = spval
            allocate (m_livecrootc_xfer_to_fire_p          (numpft)) ; m_livecrootc_xfer_to_fire_p          (:) = spval
            allocate (m_deadcrootc_xfer_to_fire_p          (numpft)) ; m_deadcrootc_xfer_to_fire_p          (:) = spval
            allocate (m_gresp_xfer_to_fire_p               (numpft)) ; m_gresp_xfer_to_fire_p               (:) = spval

            allocate (m_livestemc_to_deadstemc_fire_p      (numpft)) ; m_livestemc_to_deadstemc_fire_p      (:) = spval
            allocate (m_livecrootc_to_deadcrootc_fire_p    (numpft)) ; m_livecrootc_to_deadcrootc_fire_p    (:) = spval

            allocate (m_leafc_to_litter_fire_p             (numpft)) ; m_leafc_to_litter_fire_p             (:) = spval
            allocate (m_frootc_to_litter_fire_p            (numpft)) ; m_frootc_to_litter_fire_p            (:) = spval
            allocate (m_livestemc_to_litter_fire_p         (numpft)) ; m_livestemc_to_litter_fire_p         (:) = spval
            allocate (m_deadstemc_to_litter_fire_p         (numpft)) ; m_deadstemc_to_litter_fire_p         (:) = spval
            allocate (m_livecrootc_to_litter_fire_p        (numpft)) ; m_livecrootc_to_litter_fire_p        (:) = spval
            allocate (m_deadcrootc_to_litter_fire_p        (numpft)) ; m_deadcrootc_to_litter_fire_p        (:) = spval

            allocate (m_leafc_storage_to_litter_fire_p     (numpft)) ; m_leafc_storage_to_litter_fire_p     (:) = spval
            allocate (m_frootc_storage_to_litter_fire_p    (numpft)) ; m_frootc_storage_to_litter_fire_p    (:) = spval
            allocate (m_livestemc_storage_to_litter_fire_p (numpft)) ; m_livestemc_storage_to_litter_fire_p (:) = spval
            allocate (m_deadstemc_storage_to_litter_fire_p (numpft)) ; m_deadstemc_storage_to_litter_fire_p (:) = spval
            allocate (m_livecrootc_storage_to_litter_fire_p(numpft)) ; m_livecrootc_storage_to_litter_fire_p(:) = spval
            allocate (m_deadcrootc_storage_to_litter_fire_p(numpft)) ; m_deadcrootc_storage_to_litter_fire_p(:) = spval
            allocate (m_gresp_storage_to_litter_fire_p     (numpft)) ; m_gresp_storage_to_litter_fire_p     (:) = spval

            allocate (m_leafc_xfer_to_litter_fire_p        (numpft)) ; m_leafc_xfer_to_litter_fire_p        (:) = spval
            allocate (m_frootc_xfer_to_litter_fire_p       (numpft)) ; m_frootc_xfer_to_litter_fire_p       (:) = spval
            allocate (m_livestemc_xfer_to_litter_fire_p    (numpft)) ; m_livestemc_xfer_to_litter_fire_p    (:) = spval
            allocate (m_deadstemc_xfer_to_litter_fire_p    (numpft)) ; m_deadstemc_xfer_to_litter_fire_p    (:) = spval
            allocate (m_livecrootc_xfer_to_litter_fire_p   (numpft)) ; m_livecrootc_xfer_to_litter_fire_p   (:) = spval
            allocate (m_deadcrootc_xfer_to_litter_fire_p   (numpft)) ; m_deadcrootc_xfer_to_litter_fire_p   (:) = spval
            allocate (m_gresp_xfer_to_litter_fire_p        (numpft)) ; m_gresp_xfer_to_litter_fire_p        (:) = spval

            allocate (cpool_to_xsmrpool_p          (numpft)) ; cpool_to_xsmrpool_p          (:) = spval
            allocate (cpool_to_gresp_storage_p     (numpft)) ; cpool_to_gresp_storage_p     (:) = spval
            allocate (cpool_to_leafc_p             (numpft)) ; cpool_to_leafc_p             (:) = spval
            allocate (cpool_to_leafc_storage_p     (numpft)) ; cpool_to_leafc_storage_p     (:) = spval
            allocate (cpool_to_frootc_p            (numpft)) ; cpool_to_frootc_p            (:) = spval
            allocate (cpool_to_frootc_storage_p    (numpft)) ; cpool_to_frootc_storage_p    (:) = spval
            allocate (cpool_to_livestemc_p         (numpft)) ; cpool_to_livestemc_p         (:) = spval
            allocate (cpool_to_livestemc_storage_p (numpft)) ; cpool_to_livestemc_storage_p (:) = spval
            allocate (cpool_to_deadstemc_p         (numpft)) ; cpool_to_deadstemc_p         (:) = spval
            allocate (cpool_to_deadstemc_storage_p (numpft)) ; cpool_to_deadstemc_storage_p (:) = spval
            allocate (cpool_to_livecrootc_p        (numpft)) ; cpool_to_livecrootc_p        (:) = spval
            allocate (cpool_to_livecrootc_storage_p(numpft)) ; cpool_to_livecrootc_storage_p(:) = spval
            allocate (cpool_to_deadcrootc_p        (numpft)) ; cpool_to_deadcrootc_p        (:) = spval
            allocate (cpool_to_deadcrootc_storage_p(numpft)) ; cpool_to_deadcrootc_storage_p(:) = spval
            allocate (cpool_to_grainc_p            (numpft)) ; cpool_to_grainc_p            (:) = spval
            allocate (cpool_to_grainc_storage_p    (numpft)) ; cpool_to_grainc_storage_p    (:) = spval

            allocate (leaf_xsmr_p                  (numpft)) ; leaf_xsmr_p                  (:) = spval
            allocate (froot_xsmr_p                 (numpft)) ; froot_xsmr_p                 (:) = spval
            allocate (livestem_xsmr_p              (numpft)) ; livestem_xsmr_p              (:) = spval
            allocate (livecroot_xsmr_p             (numpft)) ; livecroot_xsmr_p             (:) = spval
            allocate (grain_xsmr_p                 (numpft)) ; grain_xsmr_p                 (:) = spval

            allocate (cpool_leaf_gr_p                      (numpft)) ; cpool_leaf_gr_p                      (:) = spval
            allocate (cpool_froot_gr_p                     (numpft)) ; cpool_froot_gr_p                     (:) = spval
            allocate (cpool_livestem_gr_p                  (numpft)) ; cpool_livestem_gr_p                  (:) = spval
            allocate (cpool_deadstem_gr_p                  (numpft)) ; cpool_deadstem_gr_p                  (:) = spval
            allocate (cpool_livecroot_gr_p                 (numpft)) ; cpool_livecroot_gr_p                 (:) = spval
            allocate (cpool_deadcroot_gr_p                 (numpft)) ; cpool_deadcroot_gr_p                 (:) = spval
            allocate (cpool_grain_gr_p                     (numpft)) ; cpool_grain_gr_p                     (:) = spval

            allocate (cpool_leaf_storage_gr_p              (numpft)) ; cpool_leaf_storage_gr_p              (:) = spval
            allocate (cpool_froot_storage_gr_p             (numpft)) ; cpool_froot_storage_gr_p             (:) = spval
            allocate (cpool_livestem_storage_gr_p          (numpft)) ; cpool_livestem_storage_gr_p          (:) = spval
            allocate (cpool_deadstem_storage_gr_p          (numpft)) ; cpool_deadstem_storage_gr_p          (:) = spval
            allocate (cpool_livecroot_storage_gr_p         (numpft)) ; cpool_livecroot_storage_gr_p         (:) = spval
            allocate (cpool_deadcroot_storage_gr_p         (numpft)) ; cpool_deadcroot_storage_gr_p         (:) = spval
            allocate (cpool_grain_storage_gr_p             (numpft)) ; cpool_grain_storage_gr_p             (:) = spval

            allocate (transfer_leaf_gr_p                   (numpft)) ; transfer_leaf_gr_p                   (:) = spval
            allocate (transfer_froot_gr_p                  (numpft)) ; transfer_froot_gr_p                  (:) = spval
            allocate (transfer_livestem_gr_p               (numpft)) ; transfer_livestem_gr_p               (:) = spval
            allocate (transfer_deadstem_gr_p               (numpft)) ; transfer_deadstem_gr_p               (:) = spval
            allocate (transfer_livecroot_gr_p              (numpft)) ; transfer_livecroot_gr_p              (:) = spval
            allocate (transfer_deadcroot_gr_p              (numpft)) ; transfer_deadcroot_gr_p              (:) = spval
            allocate (transfer_grain_gr_p                  (numpft)) ; transfer_grain_gr_p                  (:) = spval

            allocate (xsmrpool_to_atm_p                    (numpft)) ; xsmrpool_to_atm_p                    (:) = spval

            allocate (cropprod1c_loss_p                    (numpft)) ; cropprod1c_loss_p                    (:) = spval

            allocate (plant_ndemand_p              (numpft)) ; plant_ndemand_p              (:) = spval

            allocate (leafn_xfer_to_leafn_p                (numpft)) ; leafn_xfer_to_leafn_p                (:) = spval
            allocate (frootn_xfer_to_frootn_p              (numpft)) ; frootn_xfer_to_frootn_p              (:) = spval
            allocate (livestemn_xfer_to_livestemn_p        (numpft)) ; livestemn_xfer_to_livestemn_p        (:) = spval
            allocate (deadstemn_xfer_to_deadstemn_p        (numpft)) ; deadstemn_xfer_to_deadstemn_p        (:) = spval
            allocate (livecrootn_xfer_to_livecrootn_p      (numpft)) ; livecrootn_xfer_to_livecrootn_p      (:) = spval
            allocate (deadcrootn_xfer_to_deadcrootn_p      (numpft)) ; deadcrootn_xfer_to_deadcrootn_p      (:) = spval
            allocate (grainn_xfer_to_grainn_p              (numpft)) ; grainn_xfer_to_grainn_p              (:) = spval

            allocate (leafn_storage_to_xfer_p              (numpft)) ; leafn_storage_to_xfer_p              (:) = spval
            allocate (frootn_storage_to_xfer_p             (numpft)) ; frootn_storage_to_xfer_p             (:) = spval
            allocate (livestemn_storage_to_xfer_p          (numpft)) ; livestemn_storage_to_xfer_p          (:) = spval
            allocate (deadstemn_storage_to_xfer_p          (numpft)) ; deadstemn_storage_to_xfer_p          (:) = spval
            allocate (livecrootn_storage_to_xfer_p         (numpft)) ; livecrootn_storage_to_xfer_p         (:) = spval
            allocate (deadcrootn_storage_to_xfer_p         (numpft)) ; deadcrootn_storage_to_xfer_p         (:) = spval
            allocate (grainn_storage_to_xfer_p             (numpft)) ; grainn_storage_to_xfer_p             (:) = spval

            allocate (leafn_to_litter_p                    (numpft)) ; leafn_to_litter_p                    (:) = spval
            allocate (frootn_to_litter_p                   (numpft)) ; frootn_to_litter_p                   (:) = spval
            allocate (grainn_to_food_p                     (numpft)) ; grainn_to_food_p                     (:) = spval
            allocate (grainn_to_seed_p                     (numpft)) ; grainn_to_seed_p                     (:) = spval
            allocate (crop_seedn_to_leaf_p                 (numpft)) ; crop_seedn_to_leaf_p                 (:) = spval
            allocate (livestemn_to_litter_p                (numpft)) ; livestemn_to_litter_p                (:) = spval
            allocate (livestemn_to_deadstemn_p             (numpft)) ; livestemn_to_deadstemn_p             (:) = spval
            allocate (livecrootn_to_deadcrootn_p           (numpft)) ; livecrootn_to_deadcrootn_p           (:) = spval

            allocate (leafn_to_retransn_p                  (numpft)) ; leafn_to_retransn_p                  (:) = spval
            allocate (frootn_to_retransn_p                 (numpft)) ; frootn_to_retransn_p                 (:) = spval
            allocate (livestemn_to_retransn_p              (numpft)) ; livestemn_to_retransn_p              (:) = spval
            allocate (livecrootn_to_retransn_p             (numpft)) ; livecrootn_to_retransn_p             (:) = spval
            allocate (retransn_to_npool_p                  (numpft)) ; retransn_to_npool_p                  (:) = spval
            allocate (free_retransn_to_npool_p             (numpft)) ; free_retransn_to_npool_p             (:) = spval

            allocate (m_leafn_to_litter_p                  (numpft)) ; m_leafn_to_litter_p                  (:) = spval
            allocate (m_frootn_to_litter_p                 (numpft)) ; m_frootn_to_litter_p                 (:) = spval
            allocate (m_livestemn_to_litter_p              (numpft)) ; m_livestemn_to_litter_p              (:) = spval
            allocate (m_deadstemn_to_litter_p              (numpft)) ; m_deadstemn_to_litter_p              (:) = spval
            allocate (m_livecrootn_to_litter_p             (numpft)) ; m_livecrootn_to_litter_p             (:) = spval
            allocate (m_deadcrootn_to_litter_p             (numpft)) ; m_deadcrootn_to_litter_p             (:) = spval
            allocate (m_retransn_to_litter_p               (numpft)) ; m_retransn_to_litter_p               (:) = spval

            allocate (m_leafn_storage_to_litter_p          (numpft)) ; m_leafn_storage_to_litter_p          (:) = spval
            allocate (m_frootn_storage_to_litter_p         (numpft)) ; m_frootn_storage_to_litter_p         (:) = spval
            allocate (m_livestemn_storage_to_litter_p      (numpft)) ; m_livestemn_storage_to_litter_p      (:) = spval
            allocate (m_deadstemn_storage_to_litter_p      (numpft)) ; m_deadstemn_storage_to_litter_p      (:) = spval
            allocate (m_livecrootn_storage_to_litter_p     (numpft)) ; m_livecrootn_storage_to_litter_p     (:) = spval
            allocate (m_deadcrootn_storage_to_litter_p     (numpft)) ; m_deadcrootn_storage_to_litter_p     (:) = spval

            allocate (m_leafn_xfer_to_litter_p             (numpft)) ; m_leafn_xfer_to_litter_p             (:) = spval
            allocate (m_frootn_xfer_to_litter_p            (numpft)) ; m_frootn_xfer_to_litter_p            (:) = spval
            allocate (m_livestemn_xfer_to_litter_p         (numpft)) ; m_livestemn_xfer_to_litter_p         (:) = spval
            allocate (m_deadstemn_xfer_to_litter_p         (numpft)) ; m_deadstemn_xfer_to_litter_p         (:) = spval
            allocate (m_livecrootn_xfer_to_litter_p        (numpft)) ; m_livecrootn_xfer_to_litter_p        (:) = spval
            allocate (m_deadcrootn_xfer_to_litter_p        (numpft)) ; m_deadcrootn_xfer_to_litter_p        (:) = spval

            allocate (m_leafn_to_fire_p                    (numpft)) ; m_leafn_to_fire_p                    (:) = spval
            allocate (m_frootn_to_fire_p                   (numpft)) ; m_frootn_to_fire_p                   (:) = spval
            allocate (m_livestemn_to_fire_p                (numpft)) ; m_livestemn_to_fire_p                (:) = spval
            allocate (m_deadstemn_to_fire_p                (numpft)) ; m_deadstemn_to_fire_p                (:) = spval
            allocate (m_livecrootn_to_fire_p               (numpft)) ; m_livecrootn_to_fire_p               (:) = spval
            allocate (m_deadcrootn_to_fire_p               (numpft)) ; m_deadcrootn_to_fire_p               (:) = spval

            allocate (m_leafn_storage_to_fire_p            (numpft)) ; m_leafn_storage_to_fire_p            (:) = spval
            allocate (m_frootn_storage_to_fire_p           (numpft)) ; m_frootn_storage_to_fire_p           (:) = spval
            allocate (m_livestemn_storage_to_fire_p        (numpft)) ; m_livestemn_storage_to_fire_p        (:) = spval
            allocate (m_deadstemn_storage_to_fire_p        (numpft)) ; m_deadstemn_storage_to_fire_p        (:) = spval
            allocate (m_livecrootn_storage_to_fire_p       (numpft)) ; m_livecrootn_storage_to_fire_p       (:) = spval
            allocate (m_deadcrootn_storage_to_fire_p       (numpft)) ; m_deadcrootn_storage_to_fire_p       (:) = spval

            allocate (m_leafn_xfer_to_fire_p               (numpft)) ; m_leafn_xfer_to_fire_p               (:) = spval
            allocate (m_frootn_xfer_to_fire_p              (numpft)) ; m_frootn_xfer_to_fire_p              (:) = spval
            allocate (m_livestemn_xfer_to_fire_p           (numpft)) ; m_livestemn_xfer_to_fire_p           (:) = spval
            allocate (m_deadstemn_xfer_to_fire_p           (numpft)) ; m_deadstemn_xfer_to_fire_p           (:) = spval
            allocate (m_livecrootn_xfer_to_fire_p          (numpft)) ; m_livecrootn_xfer_to_fire_p          (:) = spval
            allocate (m_deadcrootn_xfer_to_fire_p          (numpft)) ; m_deadcrootn_xfer_to_fire_p          (:) = spval

            allocate (m_livestemn_to_deadstemn_fire_p      (numpft)) ; m_livestemn_to_deadstemn_fire_p      (:) = spval
            allocate (m_livecrootn_to_deadcrootn_fire_p    (numpft)) ; m_livecrootn_to_deadcrootn_fire_p    (:) = spval

            allocate (m_retransn_to_fire_p                 (numpft)) ; m_retransn_to_fire_p                 (:) = spval

            allocate (m_leafn_to_litter_fire_p             (numpft)) ; m_leafn_to_litter_fire_p             (:) = spval
            allocate (m_frootn_to_litter_fire_p            (numpft)) ; m_frootn_to_litter_fire_p            (:) = spval
            allocate (m_livestemn_to_litter_fire_p         (numpft)) ; m_livestemn_to_litter_fire_p         (:) = spval
            allocate (m_deadstemn_to_litter_fire_p         (numpft)) ; m_deadstemn_to_litter_fire_p         (:) = spval
            allocate (m_livecrootn_to_litter_fire_p        (numpft)) ; m_livecrootn_to_litter_fire_p        (:) = spval
            allocate (m_deadcrootn_to_litter_fire_p        (numpft)) ; m_deadcrootn_to_litter_fire_p        (:) = spval

            allocate (m_leafn_storage_to_litter_fire_p     (numpft)) ; m_leafn_storage_to_litter_fire_p     (:) = spval
            allocate (m_frootn_storage_to_litter_fire_p    (numpft)) ; m_frootn_storage_to_litter_fire_p    (:) = spval
            allocate (m_livestemn_storage_to_litter_fire_p (numpft)) ; m_livestemn_storage_to_litter_fire_p (:) = spval
            allocate (m_deadstemn_storage_to_litter_fire_p (numpft)) ; m_deadstemn_storage_to_litter_fire_p (:) = spval
            allocate (m_livecrootn_storage_to_litter_fire_p(numpft)) ; m_livecrootn_storage_to_litter_fire_p(:) = spval
            allocate (m_deadcrootn_storage_to_litter_fire_p(numpft)) ; m_deadcrootn_storage_to_litter_fire_p(:) = spval

            allocate (m_leafn_xfer_to_litter_fire_p        (numpft)) ; m_leafn_xfer_to_litter_fire_p        (:) = spval
            allocate (m_frootn_xfer_to_litter_fire_p       (numpft)) ; m_frootn_xfer_to_litter_fire_p       (:) = spval
            allocate (m_livestemn_xfer_to_litter_fire_p    (numpft)) ; m_livestemn_xfer_to_litter_fire_p    (:) = spval
            allocate (m_deadstemn_xfer_to_litter_fire_p    (numpft)) ; m_deadstemn_xfer_to_litter_fire_p    (:) = spval
            allocate (m_livecrootn_xfer_to_litter_fire_p   (numpft)) ; m_livecrootn_xfer_to_litter_fire_p   (:) = spval
            allocate (m_deadcrootn_xfer_to_litter_fire_p   (numpft)) ; m_deadcrootn_xfer_to_litter_fire_p   (:) = spval

            allocate (m_retransn_to_litter_fire_p          (numpft)) ; m_retransn_to_litter_fire_p          (:) = spval

            allocate (npool_to_leafn_p                     (numpft)) ; npool_to_leafn_p                     (:) = spval
            allocate (npool_to_leafn_storage_p             (numpft)) ; npool_to_leafn_storage_p             (:) = spval
            allocate (npool_to_frootn_p                    (numpft)) ; npool_to_frootn_p                    (:) = spval
            allocate (npool_to_frootn_storage_p            (numpft)) ; npool_to_frootn_storage_p            (:) = spval
            allocate (npool_to_livestemn_p                 (numpft)) ; npool_to_livestemn_p                 (:) = spval
            allocate (npool_to_livestemn_storage_p         (numpft)) ; npool_to_livestemn_storage_p         (:) = spval
            allocate (npool_to_deadstemn_p                 (numpft)) ; npool_to_deadstemn_p                 (:) = spval
            allocate (npool_to_deadstemn_storage_p         (numpft)) ; npool_to_deadstemn_storage_p         (:) = spval
            allocate (npool_to_livecrootn_p                (numpft)) ; npool_to_livecrootn_p                (:) = spval
            allocate (npool_to_livecrootn_storage_p        (numpft)) ; npool_to_livecrootn_storage_p        (:) = spval
            allocate (npool_to_deadcrootn_p                (numpft)) ; npool_to_deadcrootn_p                (:) = spval
            allocate (npool_to_deadcrootn_storage_p        (numpft)) ; npool_to_deadcrootn_storage_p        (:) = spval
            allocate (npool_to_grainn_p                    (numpft)) ; npool_to_grainn_p                    (:) = spval
            allocate (npool_to_grainn_storage_p            (numpft)) ; npool_to_grainn_storage_p            (:) = spval

            allocate (respcsun_p     (numpft)) ; respcsun_p     (:) = spval
            allocate (respcsha_p     (numpft)) ; respcsha_p     (:) = spval
            allocate (leaf_mr_p      (numpft)) ; leaf_mr_p      (:) = spval
            allocate (froot_mr_p     (numpft)) ; froot_mr_p     (:) = spval
            allocate (livestem_mr_p  (numpft)) ; livestem_mr_p  (:) = spval
            allocate (livecroot_mr_p (numpft)) ; livecroot_mr_p (:) = spval
            allocate (grain_mr_p     (numpft)) ; grain_mr_p     (:) = spval

            allocate (soil_change_p  (numpft)) ; soil_change_p  (:) = spval

            allocate (psn_to_cpool_p               (numpft)) ; psn_to_cpool_p               (:) = spval
            allocate (gpp_p                        (numpft)) ; gpp_p                        (:) = spval
            allocate (availc_p                     (numpft)) ; availc_p                     (:) = spval
            allocate (avail_retransn_p             (numpft)) ; avail_retransn_p             (:) = spval
            allocate (xsmrpool_recover_p           (numpft)) ; xsmrpool_recover_p           (:) = spval
            allocate (excess_cflux_p               (numpft)) ; excess_cflux_p               (:) = spval
            allocate (sminn_to_npool_p             (numpft)) ; sminn_to_npool_p             (:) = spval

            allocate (plant_calloc_p               (numpft)) ; plant_calloc_p               (:) = spval
            allocate (plant_nalloc_p               (numpft)) ; plant_nalloc_p               (:) = spval
            allocate (leaf_curmr_p                 (numpft)) ; leaf_curmr_p                 (:) = spval
            allocate (froot_curmr_p                (numpft)) ; froot_curmr_p                (:) = spval
            allocate (livestem_curmr_p             (numpft)) ; livestem_curmr_p             (:) = spval
            allocate (livecroot_curmr_p            (numpft)) ; livecroot_curmr_p            (:) = spval
            allocate (grain_curmr_p                (numpft)) ; grain_curmr_p                (:) = spval

            allocate (fire_closs_p                 (numpft)) ; fire_closs_p                 (:) = spval
            allocate (fire_nloss_p                 (numpft)) ; fire_nloss_p                 (:) = spval
            allocate (wood_harvestc_p              (numpft)) ; wood_harvestc_p              (:) = spval
            allocate (wood_harvestn_p              (numpft)) ; wood_harvestn_p              (:) = spval
            allocate (grainc_to_cropprodc_p        (numpft)) ; grainc_to_cropprodc_p        (:) = spval
            allocate (grainn_to_cropprodn_p        (numpft)) ; grainn_to_cropprodn_p        (:) = spval
            allocate (hrv_xsmrpool_to_atm_p        (numpft)) ; hrv_xsmrpool_to_atm_p        (:) = spval
            allocate (soyfixn_p                    (numpft)) ; soyfixn_p                    (:) = spval

         ENDIF
      ENDIF

!--------

   END SUBROUTINE allocate_1D_BGCPFTFluxes

   SUBROUTINE deallocate_1D_BGCPFTFluxes
  ! --------------------------------------------------------------------
  ! deallocates memory for CoLM PFT 1d [numpft] variables
  ! --------------------------------------------------------------------
   USE MOD_MPAS_MPI
   USE MOD_LandPFT

      IF (.true.) THEN
         IF (numpft > 0) THEN

! bgc variables
            deallocate (leafc_xfer_to_leafc_p                )
            deallocate (frootc_xfer_to_frootc_p              )
            deallocate (livestemc_xfer_to_livestemc_p        )
            deallocate (deadstemc_xfer_to_deadstemc_p        )
            deallocate (livecrootc_xfer_to_livecrootc_p      )
            deallocate (deadcrootc_xfer_to_deadcrootc_p      )
            deallocate (grainc_xfer_to_grainc_p              )

            deallocate (leafc_storage_to_xfer_p              )
            deallocate (frootc_storage_to_xfer_p             )
            deallocate (livestemc_storage_to_xfer_p          )
            deallocate (deadstemc_storage_to_xfer_p          )
            deallocate (livecrootc_storage_to_xfer_p         )
            deallocate (deadcrootc_storage_to_xfer_p         )
            deallocate (grainc_storage_to_xfer_p             )
            deallocate (gresp_storage_to_xfer_p              )

            deallocate (leafc_to_litter_p                    )
            deallocate (frootc_to_litter_p                   )
            deallocate (grainc_to_food_p                     )
            deallocate (grainc_to_seed_p                     )
            deallocate (crop_seedc_to_leaf_p                 )
            deallocate (livestemc_to_litter_p                )
            deallocate (livestemc_to_deadstemc_p             )
            deallocate (livecrootc_to_deadcrootc_p           )

            deallocate (m_leafc_to_litter_p                  )
            deallocate (m_frootc_to_litter_p                 )
            deallocate (m_livestemc_to_litter_p              )
            deallocate (m_deadstemc_to_litter_p              )
            deallocate (m_livecrootc_to_litter_p             )
            deallocate (m_deadcrootc_to_litter_p             )

            deallocate (m_leafc_storage_to_litter_p          )
            deallocate (m_frootc_storage_to_litter_p         )
            deallocate (m_livestemc_storage_to_litter_p      )
            deallocate (m_deadstemc_storage_to_litter_p      )
            deallocate (m_livecrootc_storage_to_litter_p     )
            deallocate (m_deadcrootc_storage_to_litter_p     )
            deallocate (m_gresp_storage_to_litter_p          )

            deallocate (m_leafc_xfer_to_litter_p             )
            deallocate (m_frootc_xfer_to_litter_p            )
            deallocate (m_livestemc_xfer_to_litter_p         )
            deallocate (m_deadstemc_xfer_to_litter_p         )
            deallocate (m_livecrootc_xfer_to_litter_p        )
            deallocate (m_deadcrootc_xfer_to_litter_p        )
            deallocate (m_gresp_xfer_to_litter_p             )

            deallocate (m_leafc_to_fire_p                    )
            deallocate (m_frootc_to_fire_p                   )
            deallocate (m_livestemc_to_fire_p                )
            deallocate (m_deadstemc_to_fire_p                )
            deallocate (m_livecrootc_to_fire_p               )
            deallocate (m_deadcrootc_to_fire_p               )

            deallocate (m_leafc_storage_to_fire_p            )
            deallocate (m_frootc_storage_to_fire_p           )
            deallocate (m_livestemc_storage_to_fire_p        )
            deallocate (m_deadstemc_storage_to_fire_p        )
            deallocate (m_livecrootc_storage_to_fire_p       )
            deallocate (m_deadcrootc_storage_to_fire_p       )
            deallocate (m_gresp_storage_to_fire_p            )

            deallocate (m_leafc_xfer_to_fire_p               )
            deallocate (m_frootc_xfer_to_fire_p              )
            deallocate (m_livestemc_xfer_to_fire_p           )
            deallocate (m_deadstemc_xfer_to_fire_p           )
            deallocate (m_livecrootc_xfer_to_fire_p          )
            deallocate (m_deadcrootc_xfer_to_fire_p          )
            deallocate (m_gresp_xfer_to_fire_p               )

            deallocate (m_livestemc_to_deadstemc_fire_p      )
            deallocate (m_livecrootc_to_deadcrootc_fire_p    )

            deallocate (m_leafc_to_litter_fire_p             )
            deallocate (m_frootc_to_litter_fire_p            )
            deallocate (m_livestemc_to_litter_fire_p         )
            deallocate (m_deadstemc_to_litter_fire_p         )
            deallocate (m_livecrootc_to_litter_fire_p        )
            deallocate (m_deadcrootc_to_litter_fire_p        )

            deallocate (m_leafc_storage_to_litter_fire_p     )
            deallocate (m_frootc_storage_to_litter_fire_p    )
            deallocate (m_livestemc_storage_to_litter_fire_p )
            deallocate (m_deadstemc_storage_to_litter_fire_p )
            deallocate (m_livecrootc_storage_to_litter_fire_p)
            deallocate (m_deadcrootc_storage_to_litter_fire_p)
            deallocate (m_gresp_storage_to_litter_fire_p     )

            deallocate (m_leafc_xfer_to_litter_fire_p        )
            deallocate (m_frootc_xfer_to_litter_fire_p       )
            deallocate (m_livestemc_xfer_to_litter_fire_p    )
            deallocate (m_deadstemc_xfer_to_litter_fire_p    )
            deallocate (m_livecrootc_xfer_to_litter_fire_p   )
            deallocate (m_deadcrootc_xfer_to_litter_fire_p   )
            deallocate (m_gresp_xfer_to_litter_fire_p        )

            deallocate (cpool_to_xsmrpool_p          )
            deallocate (cpool_to_gresp_storage_p     )
            deallocate (cpool_to_leafc_p             )
            deallocate (cpool_to_leafc_storage_p     )
            deallocate (cpool_to_frootc_p            )
            deallocate (cpool_to_frootc_storage_p    )
            deallocate (cpool_to_livestemc_p         )
            deallocate (cpool_to_livestemc_storage_p )
            deallocate (cpool_to_deadstemc_p         )
            deallocate (cpool_to_deadstemc_storage_p )
            deallocate (cpool_to_livecrootc_p        )
            deallocate (cpool_to_livecrootc_storage_p)
            deallocate (cpool_to_deadcrootc_p        )
            deallocate (cpool_to_deadcrootc_storage_p)
            deallocate (cpool_to_grainc_p            )
            deallocate (cpool_to_grainc_storage_p    )

            deallocate (leaf_xsmr_p                  )
            deallocate (froot_xsmr_p                 )
            deallocate (livestem_xsmr_p              )
            deallocate (livecroot_xsmr_p             )
            deallocate (grain_xsmr_p                 )

            deallocate (cpool_leaf_gr_p                      )
            deallocate (cpool_froot_gr_p                     )
            deallocate (cpool_livestem_gr_p                  )
            deallocate (cpool_deadstem_gr_p                  )
            deallocate (cpool_livecroot_gr_p                 )
            deallocate (cpool_deadcroot_gr_p                 )
            deallocate (cpool_grain_gr_p                     )

            deallocate (cpool_leaf_storage_gr_p              )
            deallocate (cpool_froot_storage_gr_p             )
            deallocate (cpool_livestem_storage_gr_p          )
            deallocate (cpool_deadstem_storage_gr_p          )
            deallocate (cpool_livecroot_storage_gr_p         )
            deallocate (cpool_deadcroot_storage_gr_p         )
            deallocate (cpool_grain_storage_gr_p             )

            deallocate (transfer_leaf_gr_p                   )
            deallocate (transfer_froot_gr_p                  )
            deallocate (transfer_livestem_gr_p               )
            deallocate (transfer_deadstem_gr_p               )
            deallocate (transfer_livecroot_gr_p              )
            deallocate (transfer_deadcroot_gr_p              )
            deallocate (transfer_grain_gr_p                  )

            deallocate (xsmrpool_to_atm_p                    )

            deallocate (cropprod1c_loss_p                    )

            deallocate (plant_ndemand_p              )

            deallocate (leafn_xfer_to_leafn_p                )
            deallocate (frootn_xfer_to_frootn_p              )
            deallocate (livestemn_xfer_to_livestemn_p        )
            deallocate (deadstemn_xfer_to_deadstemn_p        )
            deallocate (livecrootn_xfer_to_livecrootn_p      )
            deallocate (deadcrootn_xfer_to_deadcrootn_p      )
            deallocate (grainn_xfer_to_grainn_p              )

            deallocate (leafn_storage_to_xfer_p              )
            deallocate (frootn_storage_to_xfer_p             )
            deallocate (livestemn_storage_to_xfer_p          )
            deallocate (deadstemn_storage_to_xfer_p          )
            deallocate (livecrootn_storage_to_xfer_p         )
            deallocate (deadcrootn_storage_to_xfer_p         )
            deallocate (grainn_storage_to_xfer_p             )

            deallocate (leafn_to_litter_p                    )
            deallocate (frootn_to_litter_p                   )
            deallocate (grainn_to_food_p                     )
            deallocate (grainn_to_seed_p                     )
            deallocate (crop_seedn_to_leaf_p                 )
            deallocate (livestemn_to_litter_p                )
            deallocate (livestemn_to_deadstemn_p             )
            deallocate (livecrootn_to_deadcrootn_p           )

            deallocate (leafn_to_retransn_p                  )
            deallocate (frootn_to_retransn_p                 )
            deallocate (livestemn_to_retransn_p              )
            deallocate (livecrootn_to_retransn_p             )
            deallocate (retransn_to_npool_p                  )
            deallocate (free_retransn_to_npool_p             )

            deallocate (m_leafn_to_litter_p                  )
            deallocate (m_frootn_to_litter_p                 )
            deallocate (m_livestemn_to_litter_p              )
            deallocate (m_deadstemn_to_litter_p              )
            deallocate (m_livecrootn_to_litter_p             )
            deallocate (m_deadcrootn_to_litter_p             )
            deallocate (m_retransn_to_litter_p               )

            deallocate (m_leafn_storage_to_litter_p          )
            deallocate (m_frootn_storage_to_litter_p         )
            deallocate (m_livestemn_storage_to_litter_p      )
            deallocate (m_deadstemn_storage_to_litter_p      )
            deallocate (m_livecrootn_storage_to_litter_p     )
            deallocate (m_deadcrootn_storage_to_litter_p     )

            deallocate (m_leafn_xfer_to_litter_p             )
            deallocate (m_frootn_xfer_to_litter_p            )
            deallocate (m_livestemn_xfer_to_litter_p         )
            deallocate (m_deadstemn_xfer_to_litter_p         )
            deallocate (m_livecrootn_xfer_to_litter_p        )
            deallocate (m_deadcrootn_xfer_to_litter_p        )

            deallocate (m_leafn_to_fire_p                    )
            deallocate (m_frootn_to_fire_p                   )
            deallocate (m_livestemn_to_fire_p                )
            deallocate (m_deadstemn_to_fire_p                )
            deallocate (m_livecrootn_to_fire_p               )
            deallocate (m_deadcrootn_to_fire_p               )

            deallocate (m_leafn_storage_to_fire_p            )
            deallocate (m_frootn_storage_to_fire_p           )
            deallocate (m_livestemn_storage_to_fire_p        )
            deallocate (m_deadstemn_storage_to_fire_p        )
            deallocate (m_livecrootn_storage_to_fire_p       )
            deallocate (m_deadcrootn_storage_to_fire_p       )

            deallocate (m_leafn_xfer_to_fire_p               )
            deallocate (m_frootn_xfer_to_fire_p              )
            deallocate (m_livestemn_xfer_to_fire_p           )
            deallocate (m_deadstemn_xfer_to_fire_p           )
            deallocate (m_livecrootn_xfer_to_fire_p          )
            deallocate (m_deadcrootn_xfer_to_fire_p          )

            deallocate (m_livestemn_to_deadstemn_fire_p      )
            deallocate (m_livecrootn_to_deadcrootn_fire_p    )

            deallocate (m_retransn_to_fire_p                 )

            deallocate (m_leafn_to_litter_fire_p             )
            deallocate (m_frootn_to_litter_fire_p            )
            deallocate (m_livestemn_to_litter_fire_p         )
            deallocate (m_deadstemn_to_litter_fire_p         )
            deallocate (m_livecrootn_to_litter_fire_p        )
            deallocate (m_deadcrootn_to_litter_fire_p        )

            deallocate (m_leafn_storage_to_litter_fire_p     )
            deallocate (m_frootn_storage_to_litter_fire_p    )
            deallocate (m_livestemn_storage_to_litter_fire_p )
            deallocate (m_deadstemn_storage_to_litter_fire_p )
            deallocate (m_livecrootn_storage_to_litter_fire_p)
            deallocate (m_deadcrootn_storage_to_litter_fire_p)

            deallocate (m_leafn_xfer_to_litter_fire_p        )
            deallocate (m_frootn_xfer_to_litter_fire_p       )
            deallocate (m_livestemn_xfer_to_litter_fire_p    )
            deallocate (m_deadstemn_xfer_to_litter_fire_p    )
            deallocate (m_livecrootn_xfer_to_litter_fire_p   )
            deallocate (m_deadcrootn_xfer_to_litter_fire_p   )

            deallocate (m_retransn_to_litter_fire_p          )

            deallocate (npool_to_leafn_p                     )
            deallocate (npool_to_leafn_storage_p             )
            deallocate (npool_to_frootn_p                    )
            deallocate (npool_to_frootn_storage_p            )
            deallocate (npool_to_livestemn_p                 )
            deallocate (npool_to_livestemn_storage_p         )
            deallocate (npool_to_deadstemn_p                 )
            deallocate (npool_to_deadstemn_storage_p         )
            deallocate (npool_to_livecrootn_p                )
            deallocate (npool_to_livecrootn_storage_p        )
            deallocate (npool_to_deadcrootn_p                )
            deallocate (npool_to_deadcrootn_storage_p        )
            deallocate (npool_to_grainn_p                    )
            deallocate (npool_to_grainn_storage_p            )

            deallocate (respcsun_p     )
            deallocate (respcsha_p     )
            deallocate (leaf_mr_p      )
            deallocate (froot_mr_p     )
            deallocate (livestem_mr_p  )
            deallocate (livecroot_mr_p )
            deallocate (grain_mr_p     )

            deallocate (soil_change_p  )

            deallocate (psn_to_cpool_p               )
            deallocate (gpp_p                        )
            deallocate (availc_p                     )
            deallocate (avail_retransn_p             )
            deallocate (xsmrpool_recover_p           )
            deallocate (excess_cflux_p               )
            deallocate (sminn_to_npool_p             )

            deallocate (plant_calloc_p               )
            deallocate (plant_nalloc_p               )
            deallocate (leaf_curmr_p                 )
            deallocate (froot_curmr_p                )
            deallocate (livestem_curmr_p             )
            deallocate (livecroot_curmr_p            )
            deallocate (grain_curmr_p                )

            deallocate (fire_closs_p                 )
            deallocate (fire_nloss_p                 )
            deallocate (wood_harvestc_p              )
            deallocate (wood_harvestn_p              )
            deallocate (grainc_to_cropprodc_p        )
            deallocate (grainn_to_cropprodn_p        )
            deallocate (hrv_xsmrpool_to_atm_p        )
            deallocate (soyfixn_p                    )

         ENDIF
      ENDIF


   END SUBROUTINE deallocate_1D_BGCPFTFluxes

   SUBROUTINE set_1D_BGCPFTFluxes(Values, Nan)
  ! --------------------------------------------------------------------
  ! Allocates memory for CoLM PFT 1d [numpft] variables
  ! --------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_MPAS_MPI
   USE MOD_LandPFT
   IMPLICIT NONE
   real(r8),intent(in) :: Values
   real(r8),intent(in) :: Nan

      IF (.true.) THEN
         IF (numpft > 0) THEN

 ! bgc variables
            leafc_xfer_to_leafc_p                (:) = Values
            frootc_xfer_to_frootc_p              (:) = Values
            livestemc_xfer_to_livestemc_p        (:) = Values
            deadstemc_xfer_to_deadstemc_p        (:) = Values
            livecrootc_xfer_to_livecrootc_p      (:) = Values
            deadcrootc_xfer_to_deadcrootc_p      (:) = Values
            grainc_xfer_to_grainc_p              (:) = Values

            leafc_storage_to_xfer_p              (:) = Values
            frootc_storage_to_xfer_p             (:) = Values
            livestemc_storage_to_xfer_p          (:) = Values
            deadstemc_storage_to_xfer_p          (:) = Values
            livecrootc_storage_to_xfer_p         (:) = Values
            deadcrootc_storage_to_xfer_p         (:) = Values
            grainc_storage_to_xfer_p             (:) = Values
            gresp_storage_to_xfer_p              (:) = Values

            leafc_to_litter_p                    (:) = Values
            frootc_to_litter_p                   (:) = Values
            grainc_to_food_p                     (:) = Values
            grainc_to_seed_p                     (:) = Values
            crop_seedc_to_leaf_p                 (:) = Values
            livestemc_to_litter_p                (:) = Values
            livestemc_to_deadstemc_p             (:) = Values
            livecrootc_to_deadcrootc_p           (:) = Values

            m_leafc_to_litter_p                  (:) = Values
            m_frootc_to_litter_p                 (:) = Values
            m_livestemc_to_litter_p              (:) = Values
            m_deadstemc_to_litter_p              (:) = Values
            m_livecrootc_to_litter_p             (:) = Values
            m_deadcrootc_to_litter_p             (:) = Values

            m_leafc_storage_to_litter_p          (:) = Values
            m_frootc_storage_to_litter_p         (:) = Values
            m_livestemc_storage_to_litter_p      (:) = Values
            m_deadstemc_storage_to_litter_p      (:) = Values
            m_livecrootc_storage_to_litter_p     (:) = Values
            m_deadcrootc_storage_to_litter_p     (:) = Values
            m_gresp_storage_to_litter_p          (:) = Values

            m_leafc_xfer_to_litter_p             (:) = Values
            m_frootc_xfer_to_litter_p            (:) = Values
            m_livestemc_xfer_to_litter_p         (:) = Values
            m_deadstemc_xfer_to_litter_p         (:) = Values
            m_livecrootc_xfer_to_litter_p        (:) = Values
            m_deadcrootc_xfer_to_litter_p        (:) = Values
            m_gresp_xfer_to_litter_p             (:) = Values

            m_leafc_to_fire_p                    (:) = Values
            m_frootc_to_fire_p                   (:) = Values
            m_livestemc_to_fire_p                (:) = Values
            m_deadstemc_to_fire_p                (:) = Values
            m_livecrootc_to_fire_p               (:) = Values
            m_deadcrootc_to_fire_p               (:) = Values

            m_leafc_storage_to_fire_p            (:) = Values
            m_frootc_storage_to_fire_p           (:) = Values
            m_livestemc_storage_to_fire_p        (:) = Values
            m_deadstemc_storage_to_fire_p        (:) = Values
            m_livecrootc_storage_to_fire_p       (:) = Values
            m_deadcrootc_storage_to_fire_p       (:) = Values
            m_gresp_storage_to_fire_p            (:) = Values

            m_leafc_xfer_to_fire_p               (:) = Values
            m_frootc_xfer_to_fire_p              (:) = Values
            m_livestemc_xfer_to_fire_p           (:) = Values
            m_deadstemc_xfer_to_fire_p           (:) = Values
            m_livecrootc_xfer_to_fire_p          (:) = Values
            m_deadcrootc_xfer_to_fire_p          (:) = Values
            m_gresp_xfer_to_fire_p               (:) = Values

            m_livestemc_to_deadstemc_fire_p      (:) = Values
            m_livecrootc_to_deadcrootc_fire_p    (:) = Values

            m_leafc_to_litter_fire_p             (:) = Values
            m_frootc_to_litter_fire_p            (:) = Values
            m_livestemc_to_litter_fire_p         (:) = Values
            m_deadstemc_to_litter_fire_p         (:) = Values
            m_livecrootc_to_litter_fire_p        (:) = Values
            m_deadcrootc_to_litter_fire_p        (:) = Values

            m_leafc_storage_to_litter_fire_p     (:) = Values
            m_frootc_storage_to_litter_fire_p    (:) = Values
            m_livestemc_storage_to_litter_fire_p (:) = Values
            m_deadstemc_storage_to_litter_fire_p (:) = Values
            m_livecrootc_storage_to_litter_fire_p(:) = Values
            m_deadcrootc_storage_to_litter_fire_p(:) = Values
            m_gresp_storage_to_litter_fire_p     (:) = Values

            m_leafc_xfer_to_litter_fire_p        (:) = Values
            m_frootc_xfer_to_litter_fire_p       (:) = Values
            m_livestemc_xfer_to_litter_fire_p    (:) = Values
            m_deadstemc_xfer_to_litter_fire_p    (:) = Values
            m_livecrootc_xfer_to_litter_fire_p   (:) = Values
            m_deadcrootc_xfer_to_litter_fire_p   (:) = Values
            m_gresp_xfer_to_litter_fire_p        (:) = Values

            cpool_to_xsmrpool_p                  (:) = Values
            cpool_to_gresp_storage_p             (:) = Values
            cpool_to_leafc_p                     (:) = Values
            cpool_to_leafc_storage_p             (:) = Values
            cpool_to_frootc_p                    (:) = Values
            cpool_to_frootc_storage_p            (:) = Values
            cpool_to_livestemc_p                 (:) = Values
            cpool_to_livestemc_storage_p         (:) = Values
            cpool_to_deadstemc_p                 (:) = Values
            cpool_to_deadstemc_storage_p         (:) = Values
            cpool_to_livecrootc_p                (:) = Values
            cpool_to_livecrootc_storage_p        (:) = Values
            cpool_to_deadcrootc_p                (:) = Values
            cpool_to_deadcrootc_storage_p        (:) = Values
            cpool_to_grainc_p                    (:) = Values
            cpool_to_grainc_storage_p            (:) = Values

            leaf_xsmr_p                          (:) = Values
            froot_xsmr_p                         (:) = Values
            livestem_xsmr_p                      (:) = Values
            livecroot_xsmr_p                     (:) = Values
            grain_xsmr_p                         (:) = Values

            cpool_leaf_gr_p                      (:) = Values
            cpool_froot_gr_p                     (:) = Values
            cpool_livestem_gr_p                  (:) = Values
            cpool_deadstem_gr_p                  (:) = Values
            cpool_livecroot_gr_p                 (:) = Values
            cpool_deadcroot_gr_p                 (:) = Values
            cpool_grain_gr_p                     (:) = Values

            cpool_leaf_storage_gr_p              (:) = Values
            cpool_froot_storage_gr_p             (:) = Values
            cpool_livestem_storage_gr_p          (:) = Values
            cpool_deadstem_storage_gr_p          (:) = Values
            cpool_livecroot_storage_gr_p         (:) = Values
            cpool_deadcroot_storage_gr_p         (:) = Values
            cpool_grain_storage_gr_p             (:) = Values

            transfer_leaf_gr_p                   (:) = Values
            transfer_froot_gr_p                  (:) = Values
            transfer_livestem_gr_p               (:) = Values
            transfer_deadstem_gr_p               (:) = Values
            transfer_livecroot_gr_p              (:) = Values
            transfer_deadcroot_gr_p              (:) = Values
            transfer_grain_gr_p                  (:) = Values

            xsmrpool_to_atm_p                    (:) = Values

            cropprod1c_loss_p                    (:) = Values

            plant_ndemand_p                      (:) = Values

            leafn_xfer_to_leafn_p                (:) = Values
            frootn_xfer_to_frootn_p              (:) = Values
            livestemn_xfer_to_livestemn_p        (:) = Values
            deadstemn_xfer_to_deadstemn_p        (:) = Values
            livecrootn_xfer_to_livecrootn_p      (:) = Values
            deadcrootn_xfer_to_deadcrootn_p      (:) = Values
            grainn_xfer_to_grainn_p              (:) = Values

            leafn_storage_to_xfer_p              (:) = Values
            frootn_storage_to_xfer_p             (:) = Values
            livestemn_storage_to_xfer_p          (:) = Values
            deadstemn_storage_to_xfer_p          (:) = Values
            livecrootn_storage_to_xfer_p         (:) = Values
            deadcrootn_storage_to_xfer_p         (:) = Values
            grainn_storage_to_xfer_p             (:) = Values

            leafn_to_litter_p                    (:) = Values
            frootn_to_litter_p                   (:) = Values
            grainn_to_food_p                     (:) = Values
            grainn_to_seed_p                     (:) = Values
            crop_seedn_to_leaf_p                 (:) = Values
            livestemn_to_litter_p                (:) = Values
            livestemn_to_deadstemn_p             (:) = Values
            livecrootn_to_deadcrootn_p           (:) = Values

            leafn_to_retransn_p                  (:) = Values
            frootn_to_retransn_p                 (:) = Values
            livestemn_to_retransn_p              (:) = Values
            livecrootn_to_retransn_p             (:) = Values
            retransn_to_npool_p                  (:) = Values
            free_retransn_to_npool_p             (:) = Values

            m_leafn_to_litter_p                  (:) = Values
            m_frootn_to_litter_p                 (:) = Values
            m_livestemn_to_litter_p              (:) = Values
            m_deadstemn_to_litter_p              (:) = Values
            m_livecrootn_to_litter_p             (:) = Values
            m_deadcrootn_to_litter_p             (:) = Values
            m_retransn_to_litter_p               (:) = Values

            m_leafn_storage_to_litter_p          (:) = Values
            m_frootn_storage_to_litter_p         (:) = Values
            m_livestemn_storage_to_litter_p      (:) = Values
            m_deadstemn_storage_to_litter_p      (:) = Values
            m_livecrootn_storage_to_litter_p     (:) = Values
            m_deadcrootn_storage_to_litter_p     (:) = Values

            m_leafn_xfer_to_litter_p             (:) = Values
            m_frootn_xfer_to_litter_p            (:) = Values
            m_livestemn_xfer_to_litter_p         (:) = Values
            m_deadstemn_xfer_to_litter_p         (:) = Values
            m_livecrootn_xfer_to_litter_p        (:) = Values
            m_deadcrootn_xfer_to_litter_p        (:) = Values

            m_leafn_to_fire_p                    (:) = Values
            m_frootn_to_fire_p                   (:) = Values
            m_livestemn_to_fire_p                (:) = Values
            m_deadstemn_to_fire_p                (:) = Values
            m_livecrootn_to_fire_p               (:) = Values
            m_deadcrootn_to_fire_p               (:) = Values

            m_leafn_storage_to_fire_p            (:) = Values
            m_frootn_storage_to_fire_p           (:) = Values
            m_livestemn_storage_to_fire_p        (:) = Values
            m_deadstemn_storage_to_fire_p        (:) = Values
            m_livecrootn_storage_to_fire_p       (:) = Values
            m_deadcrootn_storage_to_fire_p       (:) = Values

            m_leafn_xfer_to_fire_p               (:) = Values
            m_frootn_xfer_to_fire_p              (:) = Values
            m_livestemn_xfer_to_fire_p           (:) = Values
            m_deadstemn_xfer_to_fire_p           (:) = Values
            m_livecrootn_xfer_to_fire_p          (:) = Values
            m_deadcrootn_xfer_to_fire_p          (:) = Values

            m_livestemn_to_deadstemn_fire_p      (:) = Values
            m_livecrootn_to_deadcrootn_fire_p    (:) = Values

            m_retransn_to_fire_p                 (:) = Values

            m_leafn_to_litter_fire_p             (:) = Values
            m_frootn_to_litter_fire_p            (:) = Values
            m_livestemn_to_litter_fire_p         (:) = Values
            m_deadstemn_to_litter_fire_p         (:) = Values
            m_livecrootn_to_litter_fire_p        (:) = Values
            m_deadcrootn_to_litter_fire_p        (:) = Values

            m_leafn_storage_to_litter_fire_p     (:) = Values
            m_frootn_storage_to_litter_fire_p    (:) = Values
            m_livestemn_storage_to_litter_fire_p (:) = Values
            m_deadstemn_storage_to_litter_fire_p (:) = Values
            m_livecrootn_storage_to_litter_fire_p(:) = Values
            m_deadcrootn_storage_to_litter_fire_p(:) = Values

            m_leafn_xfer_to_litter_fire_p        (:) = Values
            m_frootn_xfer_to_litter_fire_p       (:) = Values
            m_livestemn_xfer_to_litter_fire_p    (:) = Values
            m_deadstemn_xfer_to_litter_fire_p    (:) = Values
            m_livecrootn_xfer_to_litter_fire_p   (:) = Values
            m_deadcrootn_xfer_to_litter_fire_p   (:) = Values

            m_retransn_to_litter_fire_p          (:) = Values

            npool_to_leafn_p                     (:) = Values
            npool_to_leafn_storage_p             (:) = Values
            npool_to_frootn_p                    (:) = Values
            npool_to_frootn_storage_p            (:) = Values
            npool_to_livestemn_p                 (:) = Values
            npool_to_livestemn_storage_p         (:) = Values
            npool_to_deadstemn_p                 (:) = Values
            npool_to_deadstemn_storage_p         (:) = Values
            npool_to_livecrootn_p                (:) = Values
            npool_to_livecrootn_storage_p        (:) = Values
            npool_to_deadcrootn_p                (:) = Values
            npool_to_deadcrootn_storage_p        (:) = Values
            npool_to_grainn_p                    (:) = Values
            npool_to_grainn_storage_p            (:) = Values

            respcsun_p                           (:) = Values!sunlit leaf respiration
            respcsha_p                           (:) = Values!shaded leaf respiration
            leaf_mr_p                            (:) = Values!leaf maintenance respiration
            froot_mr_p                           (:) = Values!fine root maintenance respiration
            livestem_mr_p                        (:) = Values!live stem maintenance respiration
            livecroot_mr_p                       (:) = Values!live coarse root maintenance respiration
            grain_mr_p                           (:) = Values!grain maintenance respiration

            soil_change_p                        (:) = Values

            psn_to_cpool_p                       (:) = Values
            gpp_p                                (:) = Values
            availc_p                             (:) = Values
            avail_retransn_p                     (:) = Values
            xsmrpool_recover_p                   (:) = Values
            excess_cflux_p                       (:) = Values
            sminn_to_npool_p                     (:) = Values

            plant_calloc_p                       (:) = Values
            plant_nalloc_p                       (:) = Values
            leaf_curmr_p                         (:) = Values
            froot_curmr_p                        (:) = Values
            livestem_curmr_p                     (:) = Values
            livecroot_curmr_p                    (:) = Values
            grain_curmr_p                        (:) = Values

            fire_closs_p                         (:) = Values
            fire_nloss_p                         (:) = Values
            wood_harvestc_p                      (:) = Values
            wood_harvestn_p                      (:) = Values
            grainc_to_cropprodc_p                (:) = Values
            grainn_to_cropprodn_p                (:) = Values
            hrv_xsmrpool_to_atm_p                (:) = Values
            soyfixn_p                            (:) = Values

         ENDIF
      ENDIF

!--------

   END SUBROUTINE set_1D_BGCPFTFluxes
#endif

END MODULE MOD_BGC_Vars_1DPFTFluxes

#endif
! ---------- EOP ------------
