#include <define.h>

#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)

MODULE MOD_BGC_Vars_PFTimeVariables

!---------------------------------------------------------------------------------------------------------
! !DESCRIPTION
! Define, allocate, and deallocate biogeochemical state variables at pft level.
! Read and write biogeochemical state variables at pft level from/to restart files.

! !ORIGINAL:
! Xingjie Lu, 2022, created the original version

#ifdef BGC

   USE MOD_Precision
   USE MOD_Namelist, only: DEF_USE_SASU, DEF_USE_DiagMatrix
   USE MOD_TimeManager

   IMPLICIT NONE
   SAVE
! -----------------------------------------------------------------
! Time-varying state variables which required by restart run
!--------------------- bgc variables ---------------------------------------
   real(r8), allocatable :: leafc_p                  (:)     ! leaf display C (gC m-2)
   real(r8), allocatable :: leafc_storage_p          (:)     ! leaf storage C (gC m-2)
   real(r8), allocatable :: leafc_xfer_p             (:)     ! leaf transfer C (gC m-2)
   real(r8), allocatable :: frootc_p                 (:)     ! fine root display C (gC m-2)
   real(r8), allocatable :: frootc_storage_p         (:)     ! fine root storage C (gC m-2)
   real(r8), allocatable :: frootc_xfer_p            (:)     ! fine root transfer C (gC m-2)
   real(r8), allocatable :: livestemc_p              (:)     ! live stem display C (gC m-2)
   real(r8), allocatable :: livestemc_storage_p      (:)     ! live stem storage C (gC m-2)
   real(r8), allocatable :: livestemc_xfer_p         (:)     ! live stem transfer C (gC m-2)
   real(r8), allocatable :: deadstemc_p              (:)     ! dead stem display C (gC m-2)
   real(r8), allocatable :: deadstemc_storage_p      (:)     ! dead stem storage C (gC m-2)
   real(r8), allocatable :: deadstemc_xfer_p         (:)     ! dead stem transfer C (gC m-2)
   real(r8), allocatable :: livecrootc_p             (:)     ! live coarse root display C (gC m-2)
   real(r8), allocatable :: livecrootc_storage_p     (:)     ! live coarse root storage C (gC m-2)
   real(r8), allocatable :: livecrootc_xfer_p        (:)     ! live coarse root transfer C (gC m-2)
   real(r8), allocatable :: deadcrootc_p             (:)     ! dead coarse root display C (gC m-2)
   real(r8), allocatable :: deadcrootc_storage_p     (:)     ! dead coarse root storage C (gC m-2)
   real(r8), allocatable :: deadcrootc_xfer_p        (:)     ! dead coarse root transfer C (gC m-2)
   real(r8), allocatable :: grainc_p                 (:)     ! grain display C (gC m-2)
   real(r8), allocatable :: grainc_storage_p         (:)     ! grain storage C (gC m-2)
   real(r8), allocatable :: grainc_xfer_p            (:)     ! grain transfer C (gC m-2)
   real(r8), allocatable :: cropseedc_deficit_p      (:)     ! crop seed deficit C (gC m-2)
   real(r8), allocatable :: cropprod1c_p             (:)     ! product C (gC m-2)
   real(r8), allocatable :: xsmrpool_p               (:)     ! maintenance respiration storage C (gC m-2)
   real(r8), allocatable :: gresp_storage_p          (:)     ! growth respiration storage C (gC m-2)
   real(r8), allocatable :: gresp_xfer_p             (:)     ! growth respiration transfer C (gC m-2)
   real(r8), allocatable :: cpool_p                  (:)     ! available C (gC m-2)
   real(r8), allocatable :: totvegc_p                (:)     ! total vegetation C, including available C (gC m-2)

   real(r8), allocatable :: leaf_prof_p              (:,:)   ! vertical profile of leaves input to litter (m-1)
   real(r8), allocatable :: stem_prof_p              (:,:)   ! vertical profile of stem input to litter (m-1)
   real(r8), allocatable :: froot_prof_p             (:,:)   ! vertical profile of fine roots input to litter (m-1)
   real(r8), allocatable :: croot_prof_p             (:,:)   ! vertical profile of coarse roots input to litter (m-1)
   real(r8), allocatable :: cinput_rootfr_p          (:,:)   ! root fraction used for calculating vertical profile of roots input to litter (m-1)

   real(r8), allocatable :: leafn_p                  (:)     ! leaf display N (gN m-2)
   real(r8), allocatable :: leafn_storage_p          (:)     ! leaf storage N (gN m-2)
   real(r8), allocatable :: leafn_xfer_p             (:)     ! leaf transfer N (gN m-2)
   real(r8), allocatable :: frootn_p                 (:)     ! fine root display N (gN m-2)
   real(r8), allocatable :: frootn_storage_p         (:)     ! fine root storage N (gN m-2)
   real(r8), allocatable :: frootn_xfer_p            (:)     ! fine root transfer N (gN m-2)
   real(r8), allocatable :: livestemn_p              (:)     ! live stem display N (gN m-2)
   real(r8), allocatable :: livestemn_storage_p      (:)     ! live stem storage N (gN m-2)
   real(r8), allocatable :: livestemn_xfer_p         (:)     ! live stem transfer N (gN m-2)
   real(r8), allocatable :: deadstemn_p              (:)     ! dead stem display N (gN m-2)
   real(r8), allocatable :: deadstemn_storage_p      (:)     ! dead stem storage N (gN m-2)
   real(r8), allocatable :: deadstemn_xfer_p         (:)     ! dead stem transfer N (gN m-2)
   real(r8), allocatable :: livecrootn_p             (:)     ! live coarse root display N (gN m-2)
   real(r8), allocatable :: livecrootn_storage_p     (:)     ! live coarse root storage N (gN m-2)
   real(r8), allocatable :: livecrootn_xfer_p        (:)     ! live coarse root transfer N (gN m-2)
   real(r8), allocatable :: deadcrootn_p             (:)     ! dead coarse root display N (gN m-2)
   real(r8), allocatable :: deadcrootn_storage_p     (:)     ! dead coarse root storage N (gN m-2)
   real(r8), allocatable :: deadcrootn_xfer_p        (:)     ! dead coarse root transfer N (gN m-2)
   real(r8), allocatable :: grainn_p                 (:)     ! grain display N (gN m-2)
   real(r8), allocatable :: grainn_storage_p         (:)     ! grain storage N (gN m-2)
   real(r8), allocatable :: grainn_xfer_p            (:)     ! grain transfer N (gN m-2)
   real(r8), allocatable :: cropseedn_deficit_p      (:)     ! crop seed deficit N (gN m-2)
   real(r8), allocatable :: harvdate_p               (:)     ! harvest date
   integer , allocatable :: nyrs_crop_active_p       (:)     ! number of years of this crop has been active to calculate climate GDD
   real(r8), allocatable :: retransn_p               (:)     ! retranslocated N (gN m-2)
   real(r8), allocatable :: totvegn_p                (:)     ! total vegetation N, including available N (gN m-2)

   real(r8), allocatable :: tempsum_potential_gpp_p  (:)     ! temporary annual sum of potential GPP (gC m-2)
   real(r8), allocatable :: tempmax_retransn_p       (:)     ! temporary annual max of retranslocated N (gN m-2)
   real(r8), allocatable :: tempavg_tref_p           (:)     ! temporary annual average 2m air temperature (degree C)
   real(r8), allocatable :: tempsum_npp_p            (:)     ! temporary annual sum NPP (gC m-2)
   real(r8), allocatable :: tempsum_litfall_p        (:)     ! temporary annual sum litterfall (gC m-2)
   real(r8), allocatable :: annsum_potential_gpp_p   (:)     ! annual sum of potential GPP (gC m-2)
   real(r8), allocatable :: annmax_retransn_p        (:)     ! annual max of retranslocated N (gN m-2)
   real(r8), allocatable :: annavg_tref_p            (:)     ! annual average 2m air temperature (degree C)
   real(r8), allocatable :: annsum_npp_p             (:)     ! annual sum NPP (gC m-2)
   real(r8), allocatable :: annsum_litfall_p         (:)     ! annual sum litterfall (gC m-2)

   real(r8), allocatable :: bglfr_p                  (:)     ! background litterfall rate (1/s)
   real(r8), allocatable :: bgtr_p                   (:)     ! background transfer rate (1/s)
   real(r8), allocatable :: lgsf_p                   (:)     ! long growing season factor (0-1)
   real(r8), allocatable :: gdd0_p                   (:)     ! GDD based on 0 degree C
   real(r8), allocatable :: gdd8_p                   (:)     ! GDD based on 8 degree C
   real(r8), allocatable :: gdd10_p                  (:)     ! GDD based on 10 degree C
   real(r8), allocatable :: gdd020_p                 (:)     ! 20-year mean of GDD based on 0 degree C
   real(r8), allocatable :: gdd820_p                 (:)     ! 20-year mean of GDD based on 8 degree C
   real(r8), allocatable :: gdd1020_p                (:)     ! 20-year mean of GDD based on 10 degree C

   real(r8), allocatable :: offset_flag_p            (:)     ! flag, 1 if offset
   real(r8), allocatable :: offset_counter_p         (:)     ! time left for offset (s)
   real(r8), allocatable :: onset_flag_p             (:)     ! flag, 1 if onset
   real(r8), allocatable :: onset_counter_p          (:)     ! time left for onset (s)
   real(r8), allocatable :: onset_gddflag_p          (:)     ! flag, 1 if begin to accumulate GDD for onset
   real(r8), allocatable :: onset_gdd_p              (:)     ! onset GDD
   real(r8), allocatable :: onset_fdd_p              (:)     ! onset freezing degree days counter
   real(r8), allocatable :: onset_swi_p              (:)     ! onset soil water index
   real(r8), allocatable :: offset_fdd_p             (:)     ! offset freezing degree days counter
   real(r8), allocatable :: offset_swi_p             (:)     ! offset soil water index
   real(r8), allocatable :: dormant_flag_p           (:)     ! flag, 1 if dormancy, 0 if not
   real(r8), allocatable :: prev_leafc_to_litter_p   (:)     ! previous timestep leaf display C to litter C (gN m-2 s-1)
   real(r8), allocatable :: prev_frootc_to_litter_p  (:)     ! previous timestep fine root display C to litter C (gN m-2 s-1)
   real(r8), allocatable :: days_active_p            (:)     ! phenology-associated state: number of days since last dormancy

   real(r8), allocatable :: burndate_p               (:)     ! burn date for crop

   real(r8), allocatable :: c_allometry_p            (:)     ! C allocation index
   real(r8), allocatable :: n_allometry_p            (:)     ! N allocation index
   real(r8), allocatable :: downreg_p                (:)     ! fractional reduction in GPP due to N limitation
   real(r8), allocatable :: grain_flag_p             (:)     ! flag, 1 if grain fill, 0 if not

   real(r8), allocatable :: ctrunc_p                 (:)     ! additional carbon from precision control, currently not used
   real(r8), allocatable :: ntrunc_p                 (:)     ! additional nitrogen from precision control, currently not used
   real(r8), allocatable :: npool_p                  (:)     ! available N (gN m-2)

!--------------------- CROP variables for GPAM------------------------------
#ifdef CROP
   logical, allocatable :: croplive_p                (:)     ! flag, true if crop live, not harvested
   real(r8),allocatable :: hui_p                     (:)     ! heat unit index since planting
   real(r8),allocatable :: gddplant_p                (:)     ! GDD since planting
   integer ,allocatable :: peaklai_p                 (:)     ! flag, 1 if lai at maximum allowed, 0 if lai not at maximum allowed
   real(r8),allocatable :: aroot_p                   (:)     ! root allocation coefficient
   real(r8),allocatable :: astem_p                   (:)     ! stem allocation coefficient
   real(r8),allocatable :: arepr_p                   (:)     ! reproduction (fruit) allocation coefficient
   real(r8),allocatable :: aleaf_p                   (:)     ! leaf allocation coefficient
   real(r8),allocatable :: astemi_p                  (:)     ! stem allocation coefficient of phase 2
   real(r8),allocatable :: aleafi_p                  (:)     ! leaf allocation coefficient of phase 2
   real(r8),allocatable :: gddmaturity_p             (:)     ! gdd needed to harvest

   logical, allocatable :: cropplant_p               (:)     ! flag, true if crop planted, not harvested; but if winter cereal still live at begin of the year, it will be set false
   integer ,allocatable :: idop_p                    (:)     ! planting date
   real(r8),allocatable :: a5tmin_p                  (:)     ! 5-day running mean of min 2 m temperature (degree C)
   real(r8),allocatable :: a10tmin_p                 (:)     ! 10-day running mean of min 2 m temperature (degree C)
   real(r8),allocatable :: t10_p                     (:)     ! 10-day running mean of 2 m temperature (degree C)
   real(r8),allocatable :: cumvd_p                   (:)     ! effective vernalization days (d)
   real(r8),allocatable :: vf_p                      (:)     ! vernalization factor (0-1)
   real(r8),allocatable :: cphase_p                  (:)     ! phenology phase
   real(r8),allocatable :: fert_counter_p            (:)     ! time left to fertilize (s)
   real(r8),allocatable :: tref_min_p                (:)     ! daily min of average 2-m temperature (degree C)
   real(r8),allocatable :: tref_max_p                (:)     ! daily max of average 2-m temperature (degree C)
   real(r8),allocatable :: tref_min_inst_p           (:)     ! temporary daily min of average 2-m temperature (degree C)
   real(r8),allocatable :: tref_max_inst_p           (:)     ! temporary daily max of average 2-m temperature (degree C)
   real(r8),allocatable :: fertnitro_p               (:)     ! fertilizer nitrogen (gN m-2)
   real(r8),allocatable :: manunitro_p               (:)     ! manure nitrogen (gN m-2)
   real(r8),allocatable :: fert_p                    (:)     ! fertilizer nitrogen (gN m-2) including manure
   real(r8),allocatable :: latbaset_p                (:)     ! latitude vary base temperature for gddplant (degree C)
   real(r8),allocatable :: plantdate_p               (:)     ! planting date (input)
#endif
! --------------------- END CROP variables -------------------------

! --------------------- SASU variables -----------------------------
   real(r8), allocatable :: leafcCap_p                  (:)     ! leaf display C (gC m-2)
   real(r8), allocatable :: leafc_storageCap_p          (:)     ! leaf storage C (gC m-2)
   real(r8), allocatable :: leafc_xferCap_p             (:)     ! leaf transfer C (gC m-2)
   real(r8), allocatable :: frootcCap_p                 (:)     ! fine root display C (gC m-2)
   real(r8), allocatable :: frootc_storageCap_p         (:)     ! fine root storage C (gC m-2)
   real(r8), allocatable :: frootc_xferCap_p            (:)     ! fine root transfer C (gC m-2)
   real(r8), allocatable :: livestemcCap_p              (:)     ! live stem display C (gC m-2)
   real(r8), allocatable :: livestemc_storageCap_p      (:)     ! live stem storage C (gC m-2)
   real(r8), allocatable :: livestemc_xferCap_p         (:)     ! live stem transfer C (gC m-2)
   real(r8), allocatable :: deadstemcCap_p              (:)     ! dead stem display C (gC m-2)
   real(r8), allocatable :: deadstemc_storageCap_p      (:)     ! dead stem storage C (gC m-2)
   real(r8), allocatable :: deadstemc_xferCap_p         (:)     ! dead stem transfer C (gC m-2)
   real(r8), allocatable :: livecrootcCap_p             (:)     ! live coarse root display C (gC m-2)
   real(r8), allocatable :: livecrootc_storageCap_p     (:)     ! live coarse root storage C (gC m-2)
   real(r8), allocatable :: livecrootc_xferCap_p        (:)     ! live coarse root transfer C (gC m-2)
   real(r8), allocatable :: deadcrootcCap_p             (:)     ! dead coarse root display C (gC m-2)
   real(r8), allocatable :: deadcrootc_storageCap_p     (:)     ! dead coarse root storage C (gC m-2)
   real(r8), allocatable :: deadcrootc_xferCap_p        (:)     ! dead coarse root transfer C (gC m-2)

   real(r8), allocatable :: leafnCap_p                  (:)     ! leaf display C (gC m-2)
   real(r8), allocatable :: leafn_storageCap_p          (:)     ! leaf storage C (gC m-2)
   real(r8), allocatable :: leafn_xferCap_p             (:)     ! leaf transfer C (gC m-2)
   real(r8), allocatable :: frootnCap_p                 (:)     ! fine root display C (gC m-2)
   real(r8), allocatable :: frootn_storageCap_p         (:)     ! fine root storage C (gC m-2)
   real(r8), allocatable :: frootn_xferCap_p            (:)     ! fine root transfer C (gC m-2)
   real(r8), allocatable :: livestemnCap_p              (:)     ! live stem display C (gC m-2)
   real(r8), allocatable :: livestemn_storageCap_p      (:)     ! live stem storage C (gC m-2)
   real(r8), allocatable :: livestemn_xferCap_p         (:)     ! live stem transfer C (gC m-2)
   real(r8), allocatable :: deadstemnCap_p              (:)     ! dead stem display C (gC m-2)
   real(r8), allocatable :: deadstemn_storageCap_p      (:)     ! dead stem storage C (gC m-2)
   real(r8), allocatable :: deadstemn_xferCap_p         (:)     ! dead stem transfer C (gC m-2)
   real(r8), allocatable :: livecrootnCap_p             (:)     ! live coarse root display C (gC m-2)
   real(r8), allocatable :: livecrootn_storageCap_p     (:)     ! live coarse root storage C (gC m-2)
   real(r8), allocatable :: livecrootn_xferCap_p        (:)     ! live coarse root transfer C (gC m-2)
   real(r8), allocatable :: deadcrootnCap_p             (:)     ! dead coarse root display C (gC m-2)
   real(r8), allocatable :: deadcrootn_storageCap_p     (:)     ! dead coarse root storage C (gC m-2)
   real(r8), allocatable :: deadcrootn_xferCap_p        (:)     ! dead coarse root transfer C (gC m-2)

   real(r8), allocatable :: leafc0_p                 (:) ! SASU spinup initial value: leaf display C (gC m-2)
   real(r8), allocatable :: leafc0_storage_p         (:) ! SASU spinup initial value: leaf storage C (gC m-2)
   real(r8), allocatable :: leafc0_xfer_p            (:) ! SASU spinup initial value: leaf transfer C (gC m-2)
   real(r8), allocatable :: frootc0_p                (:) ! SASU spinup initial value: fine root display C (gC m-2)
   real(r8), allocatable :: frootc0_storage_p        (:) ! SASU spinup initial value: fine root storage C (gC m-2)
   real(r8), allocatable :: frootc0_xfer_p           (:) ! SASU spinup initial value: fine root transfer C (gC m-2)
   real(r8), allocatable :: livestemc0_p             (:) ! SASU spinup initial value: live stem display C (gC m-2)
   real(r8), allocatable :: livestemc0_storage_p     (:) ! SASU spinup initial value: live stem storage C (gC m-2)
   real(r8), allocatable :: livestemc0_xfer_p        (:) ! SASU spinup initial value: live stem transfer C (gC m-2)
   real(r8), allocatable :: deadstemc0_p             (:) ! SASU spinup initial value: dead stem display C (gC m-2)
   real(r8), allocatable :: deadstemc0_storage_p     (:) ! SASU spinup initial value: dead stem storage C (gC m-2)
   real(r8), allocatable :: deadstemc0_xfer_p        (:) ! SASU spinup initial value: dead stem transfer C (gC m-2)
   real(r8), allocatable :: livecrootc0_p            (:) ! SASU spinup initial value: live coarse root display C (gC m-2)
   real(r8), allocatable :: livecrootc0_storage_p    (:) ! SASU spinup initial value: live coarse root storage C (gC m-2)
   real(r8), allocatable :: livecrootc0_xfer_p       (:) ! SASU spinup initial value: live coarse root transfer C (gC m-2)
   real(r8), allocatable :: deadcrootc0_p            (:) ! SASU spinup initial value: dead coarse root display C (gC m-2)
   real(r8), allocatable :: deadcrootc0_storage_p    (:) ! SASU spinup initial value: dead coarse root storage C (gC m-2)
   real(r8), allocatable :: deadcrootc0_xfer_p       (:) ! SASU spinup initial value: dead coarse root transfer C (gC m-2)
   real(r8), allocatable :: grainc0_p                (:) ! SASU spinup initial value: grain display C (gC m-2)
   real(r8), allocatable :: grainc0_storage_p        (:) ! SASU spinup initial value: grain storage C (gC m-2)
   real(r8), allocatable :: grainc0_xfer_p           (:) ! SASU spinup initial value: grain transfer C (gC m-2)

   real(r8), allocatable :: leafn0_p                 (:) ! SASU spinup initial value: leaf display N (gN m-2)
   real(r8), allocatable :: leafn0_storage_p         (:) ! SASU spinup initial value: leaf storage N (gN m-2)
   real(r8), allocatable :: leafn0_xfer_p            (:) ! SASU spinup initial value: leaf transfer N (gN m-2)
   real(r8), allocatable :: frootn0_p                (:) ! SASU spinup initial value: fine root display N (gN m-2)
   real(r8), allocatable :: frootn0_storage_p        (:) ! SASU spinup initial value: fine root storage N (gN m-2)
   real(r8), allocatable :: frootn0_xfer_p           (:) ! SASU spinup initial value: fine root transfer N (gN m-2)
   real(r8), allocatable :: livestemn0_p             (:) ! SASU spinup initial value: live stem display N (gN m-2)
   real(r8), allocatable :: livestemn0_storage_p     (:) ! SASU spinup initial value: live stem storage N (gN m-2)
   real(r8), allocatable :: livestemn0_xfer_p        (:) ! SASU spinup initial value: live stem transfer N (gN m-2)
   real(r8), allocatable :: deadstemn0_p             (:) ! SASU spinup initial value: dead stem display N (gN m-2)
   real(r8), allocatable :: deadstemn0_storage_p     (:) ! SASU spinup initial value: dead stem storage N (gN m-2)
   real(r8), allocatable :: deadstemn0_xfer_p        (:) ! SASU spinup initial value: dead stem transfer N (gN m-2)
   real(r8), allocatable :: livecrootn0_p            (:) ! SASU spinup initial value: live coarse root display N (gN m-2)
   real(r8), allocatable :: livecrootn0_storage_p    (:) ! SASU spinup initial value: live coarse root storage N (gN m-2)
   real(r8), allocatable :: livecrootn0_xfer_p       (:) ! SASU spinup initial value: live coarse root transfer N (gN m-2)
   real(r8), allocatable :: deadcrootn0_p            (:) ! SASU spinup initial value: dead coarse root display N (gN m-2)
   real(r8), allocatable :: deadcrootn0_storage_p    (:) ! SASU spinup initial value: dead coarse root storage N (gN m-2)
   real(r8), allocatable :: deadcrootn0_xfer_p       (:) ! SASU spinup initial value: dead coarse root transfer N (gN m-2)
   real(r8), allocatable :: grainn0_p                (:) ! SASU spinup initial value: grain display N (gN m-2)
   real(r8), allocatable :: grainn0_storage_p        (:) ! SASU spinup initial value: grain storage N (gN m-2)
   real(r8), allocatable :: grainn0_xfer_p           (:) ! SASU spinup initial value: grain transfer N (gN m-2)
   real(r8), allocatable :: retransn0_p              (:) ! SASU spinup initial value: retranslocated N (gN m-2)

   real(r8), allocatable :: I_leafc_p_acc            (:) ! SASU spinup diagnostics: accumulated input to leaf display C (gC m-2)
   real(r8), allocatable :: I_leafc_st_p_acc         (:) ! SASU spinup diagnostics: accumulated input to leaf storage C (gC m-2)
   real(r8), allocatable :: I_frootc_p_acc           (:) ! SASU spinup diagnostics: accumulated input to fine root display C (gC m-2)
   real(r8), allocatable :: I_frootc_st_p_acc        (:) ! SASU spinup diagnostics: accumulated input to fine root storage C (gC m-2)
   real(r8), allocatable :: I_livestemc_p_acc        (:) ! SASU spinup diagnostics: accumulated input to live stem display C (gC m-2)
   real(r8), allocatable :: I_livestemc_st_p_acc     (:) ! SASU spinup diagnostics: accumulated input to live stem storage C (gC m-2)
   real(r8), allocatable :: I_deadstemc_p_acc        (:) ! SASU spinup diagnostics: accumulated input to dead stem display C (gC m-2)
   real(r8), allocatable :: I_deadstemc_st_p_acc     (:) ! SASU spinup diagnostics: accumulated input to dead stem storage C (gC m-2)
   real(r8), allocatable :: I_livecrootc_p_acc       (:) ! SASU spinup diagnostics: accumulated input to live coarse root display C (gC m-2)
   real(r8), allocatable :: I_livecrootc_st_p_acc    (:) ! SASU spinup diagnostics: accumulated input to live coarse root storage C (gC m-2)
   real(r8), allocatable :: I_deadcrootc_p_acc       (:) ! SASU spinup diagnostics: accumulated input to dead coarse root display C (gC m-2)
   real(r8), allocatable :: I_deadcrootc_st_p_acc    (:) ! SASU spinup diagnostics: accumulated input to dead coarse root storage C (gC m-2)
   real(r8), allocatable :: I_grainc_p_acc           (:) ! SASU spinup diagnostics: accumulated input to grain display C (gC m-2)
   real(r8), allocatable :: I_grainc_st_p_acc        (:) ! SASU spinup diagnostics: accumulated input to grain storage C (gC m-2)
   real(r8), allocatable :: I_leafn_p_acc            (:) ! SASU spinup diagnostics: accumulated input to leaf display N (gN m-2)
   real(r8), allocatable :: I_leafn_st_p_acc         (:) ! SASU spinup diagnostics: accumulated input to leaf storage N (gN m-2)
   real(r8), allocatable :: I_frootn_p_acc           (:) ! SASU spinup diagnostics: accumulated input to fine root display N (gN m-2)
   real(r8), allocatable :: I_frootn_st_p_acc        (:) ! SASU spinup diagnostics: accumulated input to fine root storage N (gN m-2)
   real(r8), allocatable :: I_livestemn_p_acc        (:) ! SASU spinup diagnostics: accumulated input to live stem display N (gN m-2)
   real(r8), allocatable :: I_livestemn_st_p_acc     (:) ! SASU spinup diagnostics: accumulated input to live stem storage N (gN m-2)
   real(r8), allocatable :: I_deadstemn_p_acc        (:) ! SASU spinup diagnostics: accumulated input to dead stem display N (gN m-2)
   real(r8), allocatable :: I_deadstemn_st_p_acc     (:) ! SASU spinup diagnostics: accumulated input to dead stem storage N (gN m-2)
   real(r8), allocatable :: I_livecrootn_p_acc       (:) ! SASU spinup diagnostics: accumulated input to live coarse root display N (gN m-2)
   real(r8), allocatable :: I_livecrootn_st_p_acc    (:) ! SASU spinup diagnostics: accumulated input to live coarse root storage N (gN m-2)
   real(r8), allocatable :: I_deadcrootn_p_acc       (:) ! SASU spinup diagnostics: accumulated input to dead coarse root display N (gN m-2)
   real(r8), allocatable :: I_deadcrootn_st_p_acc    (:) ! SASU spinup diagnostics: accumulated input to dead coarse root storage N (gN m-2)
   real(r8), allocatable :: I_grainn_p_acc           (:) ! SASU spinup diagnostics: accumulated input to grain display N (gN m-2)
   real(r8), allocatable :: I_grainn_st_p_acc        (:) ! SASU spinup diagnostics: accumulated input to grain storage N (gN m-2)

   real(r8), allocatable :: AKX_leafc_xf_to_leafc_p_acc                 (:) ! SASU spinup diagnostics: accumulated flux from leaf transfer C to display C (gC m-2)
   real(r8), allocatable :: AKX_frootc_xf_to_frootc_p_acc               (:) ! SASU spinup diagnostics: accumulated flux from fine root transfer C to display C (gC m-2)
   real(r8), allocatable :: AKX_livestemc_xf_to_livestemc_p_acc         (:) ! SASU spinup diagnostics: accumulated flux from live stem transfer C to display C (gC m-2)
   real(r8), allocatable :: AKX_deadstemc_xf_to_deadstemc_p_acc         (:) ! SASU spinup diagnostics: accumulated flux from dead stem transfer C to display C (gC m-2)
   real(r8), allocatable :: AKX_livecrootc_xf_to_livecrootc_p_acc       (:) ! SASU spinup diagnostics: accumulated flux from live coarse root transfer C to display C (gC m-2)
   real(r8), allocatable :: AKX_deadcrootc_xf_to_deadcrootc_p_acc       (:) ! SASU spinup diagnostics: accumulated flux from dead coarse root transfer C to display C (gC m-2)
   real(r8), allocatable :: AKX_grainc_xf_to_grainc_p_acc               (:) ! SASU spinup diagnostics: accumulated flux from grain transfer C to display C (gC m-2)
   real(r8), allocatable :: AKX_livestemc_to_deadstemc_p_acc            (:) ! SASU spinup diagnostics: accumulated flux from live stem display C to dead stem display C (gC m-2)
   real(r8), allocatable :: AKX_livecrootc_to_deadcrootc_p_acc          (:) ! SASU spinup diagnostics: accumulated flux from live coarse root display C to dead coarse root display C (gC m-2)

   real(r8), allocatable :: AKX_leafc_st_to_leafc_xf_p_acc              (:) ! SASU spinup diagnostics: accumulated flux from leaf storage C to transfer C (gC m-2)
   real(r8), allocatable :: AKX_frootc_st_to_frootc_xf_p_acc            (:) ! SASU spinup diagnostics: accumulated flux from fine root storage C to transfer C (gC m-2)
   real(r8), allocatable :: AKX_livestemc_st_to_livestemc_xf_p_acc      (:) ! SASU spinup diagnostics: accumulated flux from live stem storage C to transfer C (gC m-2)
   real(r8), allocatable :: AKX_deadstemc_st_to_deadstemc_xf_p_acc      (:) ! SASU spinup diagnostics: accumulated flux from dead stem storage C to transfer C (gC m-2)
   real(r8), allocatable :: AKX_livecrootc_st_to_livecrootc_xf_p_acc    (:) ! SASU spinup diagnostics: accumulated flux from live coarse root storage C to transfer C (gC m-2)
   real(r8), allocatable :: AKX_deadcrootc_st_to_deadcrootc_xf_p_acc    (:) ! SASU spinup diagnostics: accumulated flux from dead coarse root storage C to transfer C (gC m-2)
   real(r8), allocatable :: AKX_grainc_st_to_grainc_xf_p_acc            (:) ! SASU spinup diagnostics: accumulated flux from grain storage C to transfer C (gC m-2)

   real(r8), allocatable :: AKX_leafc_exit_p_acc                        (:) ! SASU spinup diagnostics: accumulated flux exiting from leaf display C (gC m-2)
   real(r8), allocatable :: AKX_frootc_exit_p_acc                       (:) ! SASU spinup diagnostics: accumulated flux exiting from fine root display C (gC m-2)
   real(r8), allocatable :: AKX_livestemc_exit_p_acc                    (:) ! SASU spinup diagnostics: accumulated flux exiting from live stem display C (gC m-2)
   real(r8), allocatable :: AKX_deadstemc_exit_p_acc                    (:) ! SASU spinup diagnostics: accumulated flux exiting from dead stem display C (gC m-2)
   real(r8), allocatable :: AKX_livecrootc_exit_p_acc                   (:) ! SASU spinup diagnostics: accumulated flux exiting from live coarse root display C (gC m-2)
   real(r8), allocatable :: AKX_deadcrootc_exit_p_acc                   (:) ! SASU spinup diagnostics: accumulated flux exiting from dead coarse root display C (gC m-2)
   real(r8), allocatable :: AKX_grainc_exit_p_acc                       (:) ! SASU spinup diagnostics: accumulated flux exiting from grain display C (gC m-2)

   real(r8), allocatable :: AKX_leafc_st_exit_p_acc                     (:) ! SASU spinup diagnostics: accumulated flux exiting from leaf storage C (gC m-2)
   real(r8), allocatable :: AKX_frootc_st_exit_p_acc                    (:) ! SASU spinup diagnostics: accumulated flux exiting from fine root storage C (gC m-2)
   real(r8), allocatable :: AKX_livestemc_st_exit_p_acc                 (:) ! SASU spinup diagnostics: accumulated flux exiting from live stem storage C (gC m-2)
   real(r8), allocatable :: AKX_deadstemc_st_exit_p_acc                 (:) ! SASU spinup diagnostics: accumulated flux exiting from dead stem storage C (gC m-2)
   real(r8), allocatable :: AKX_livecrootc_st_exit_p_acc                (:) ! SASU spinup diagnostics: accumulated flux exiting from live coarse root storage C (gC m-2)
   real(r8), allocatable :: AKX_deadcrootc_st_exit_p_acc                (:) ! SASU spinup diagnostics: accumulated flux exiting from dead coarse root storage C (gC m-2)
   real(r8), allocatable :: AKX_grainc_st_exit_p_acc                    (:) ! SASU spinup diagnostics: accumulated flux exiting from grain storage C (gC m-2)

   real(r8), allocatable :: AKX_leafc_xf_exit_p_acc                     (:) ! SASU spinup diagnostics: accumulated flux exiting from leaf transfer C (gC m-2)
   real(r8), allocatable :: AKX_frootc_xf_exit_p_acc                    (:) ! SASU spinup diagnostics: accumulated flux exiting from fine root transfer C (gC m-2)
   real(r8), allocatable :: AKX_livestemc_xf_exit_p_acc                 (:) ! SASU spinup diagnostics: accumulated flux exiting from live stem transfer C (gC m-2)
   real(r8), allocatable :: AKX_deadstemc_xf_exit_p_acc                 (:) ! SASU spinup diagnostics: accumulated flux exiting from dead stem transfer C (gC m-2)
   real(r8), allocatable :: AKX_livecrootc_xf_exit_p_acc                (:) ! SASU spinup diagnostics: accumulated flux exiting from live coarse root transfer C (gC m-2)
   real(r8), allocatable :: AKX_deadcrootc_xf_exit_p_acc                (:) ! SASU spinup diagnostics: accumulated flux exiting from dead coarse root transfer C (gC m-2)
   real(r8), allocatable :: AKX_grainc_xf_exit_p_acc                    (:) ! SASU spinup diagnostics: accumulated flux exiting from grain transfer C (gC m-2)

   real(r8), allocatable :: AKX_leafn_xf_to_leafn_p_acc                 (:) ! SASU spinup diagnostics: accumulated flux from leaf transfer N to display N (gN m-2)
   real(r8), allocatable :: AKX_frootn_xf_to_frootn_p_acc               (:) ! SASU spinup diagnostics: accumulated flux from fine root transfer N to display N (gN m-2)
   real(r8), allocatable :: AKX_livestemn_xf_to_livestemn_p_acc         (:) ! SASU spinup diagnostics: accumulated flux from live stem transfer N to display N (gN m-2)
   real(r8), allocatable :: AKX_deadstemn_xf_to_deadstemn_p_acc         (:) ! SASU spinup diagnostics: accumulated flux from dead stem transfer N to display N (gN m-2)
   real(r8), allocatable :: AKX_livecrootn_xf_to_livecrootn_p_acc       (:) ! SASU spinup diagnostics: accumulated flux from live coarse root transfer N to display N (gN m-2)
   real(r8), allocatable :: AKX_deadcrootn_xf_to_deadcrootn_p_acc       (:) ! SASU spinup diagnostics: accumulated flux from dead coarse root transfer N to display N (gN m-2)
   real(r8), allocatable :: AKX_grainn_xf_to_grainn_p_acc               (:) ! SASU spinup diagnostics: accumulated flux from grain transfer N to display N (gN m-2)
   real(r8), allocatable :: AKX_livestemn_to_deadstemn_p_acc            (:) ! SASU spinup diagnostics: accumulated flux from live stem display N to dead stem display N (gN m-2)
   real(r8), allocatable :: AKX_livecrootn_to_deadcrootn_p_acc          (:) ! SASU spinup diagnostics: accumulated flux from live coarse root display N to dead coarse root display N (gN m-2)

   real(r8), allocatable :: AKX_leafn_st_to_leafn_xf_p_acc              (:) ! SASU spinup diagnostics: accumulated flux from leaf storage N to transfer N (gN m-2)
   real(r8), allocatable :: AKX_frootn_st_to_frootn_xf_p_acc            (:) ! SASU spinup diagnostics: accumulated flux from fine root storage N to transfer N (gN m-2)
   real(r8), allocatable :: AKX_livestemn_st_to_livestemn_xf_p_acc      (:) ! SASU spinup diagnostics: accumulated flux from live stem storage N to transfer N (gN m-2)
   real(r8), allocatable :: AKX_deadstemn_st_to_deadstemn_xf_p_acc      (:) ! SASU spinup diagnostics: accumulated flux from dead stem storage N to transfer N (gN m-2)
   real(r8), allocatable :: AKX_livecrootn_st_to_livecrootn_xf_p_acc    (:) ! SASU spinup diagnostics: accumulated flux from live coarse root storage N to transfer N (gN m-2)
   real(r8), allocatable :: AKX_deadcrootn_st_to_deadcrootn_xf_p_acc    (:) ! SASU spinup diagnostics: accumulated flux from dead coarse root storage N to transfer N (gN m-2)
   real(r8), allocatable :: AKX_grainn_st_to_grainn_xf_p_acc            (:) ! SASU spinup diagnostics: accumulated flux from grain storage N to transfer N (gN m-2)

   real(r8), allocatable :: AKX_leafn_to_retransn_p_acc                 (:) ! SASU spinup diagnostics: accumulated flux from leaf display N to retranslocated N (gN m-2)
   real(r8), allocatable :: AKX_frootn_to_retransn_p_acc                (:) ! SASU spinup diagnostics: accumulated flux from fine root display N to retranslocated N (gN m-2)
   real(r8), allocatable :: AKX_livestemn_to_retransn_p_acc             (:) ! SASU spinup diagnostics: accumulated flux from live stem display N to retranslocated N (gN m-2)
   real(r8), allocatable :: AKX_livecrootn_to_retransn_p_acc            (:) ! SASU spinup diagnostics: accumulated flux from live coarse root display N to retranslocated N (gN m-2)

   real(r8), allocatable :: AKX_retransn_to_leafn_p_acc                 (:) ! SASU spinup diagnostics: accumulated flux from retranslocated N to leaf display N (gN m-2)
   real(r8), allocatable :: AKX_retransn_to_frootn_p_acc                (:) ! SASU spinup diagnostics: accumulated flux from retranslocated N to fine root display N (gN m-2)
   real(r8), allocatable :: AKX_retransn_to_livestemn_p_acc             (:) ! SASU spinup diagnostics: accumulated flux from retranslocated N to live stem display N (gN m-2)
   real(r8), allocatable :: AKX_retransn_to_deadstemn_p_acc             (:) ! SASU spinup diagnostics: accumulated flux from retranslocated N to dead stem display N (gN m-2)
   real(r8), allocatable :: AKX_retransn_to_livecrootn_p_acc            (:) ! SASU spinup diagnostics: accumulated flux from retranslocated N to live coarse root display N (gN m-2)
   real(r8), allocatable :: AKX_retransn_to_deadcrootn_p_acc            (:) ! SASU spinup diagnostics: accumulated flux from retranslocated N to dead coarse root display N (gN m-2)
   real(r8), allocatable :: AKX_retransn_to_grainn_p_acc                (:) ! SASU spinup diagnostics: accumulated flux from retranslocated N to grain display N (gN m-2)

   real(r8), allocatable :: AKX_retransn_to_leafn_st_p_acc              (:) ! SASU spinup diagnostics: accumulated flux from retranslocated N to leaf storage N (gN m-2)
   real(r8), allocatable :: AKX_retransn_to_frootn_st_p_acc             (:) ! SASU spinup diagnostics: accumulated flux from retranslocated N to fine root storage N (gN m-2)
   real(r8), allocatable :: AKX_retransn_to_livestemn_st_p_acc          (:) ! SASU spinup diagnostics: accumulated flux from retranslocated N to live stem storage N (gN m-2)
   real(r8), allocatable :: AKX_retransn_to_deadstemn_st_p_acc          (:) ! SASU spinup diagnostics: accumulated flux from retranslocated N to dead stem storage N (gN m-2)
   real(r8), allocatable :: AKX_retransn_to_livecrootn_st_p_acc         (:) ! SASU spinup diagnostics: accumulated flux from retranslocated N to live coarse root storage N (gN m-2)
   real(r8), allocatable :: AKX_retransn_to_deadcrootn_st_p_acc         (:) ! SASU spinup diagnostics: accumulated flux from retranslocated N to dead coarse root storage N (gN m-2)
   real(r8), allocatable :: AKX_retransn_to_grainn_st_p_acc             (:) ! SASU spinup diagnostics: accumulated flux from retranslocated N to grain storage N (gN m-2)

   real(r8), allocatable :: AKX_leafn_exit_p_acc                        (:) ! SASU spinup diagnostics: accumulated flux exiting from leaf display N (gN m-2)
   real(r8), allocatable :: AKX_frootn_exit_p_acc                       (:) ! SASU spinup diagnostics: accumulated flux exiting from fine root display N (gN m-2)
   real(r8), allocatable :: AKX_livestemn_exit_p_acc                    (:) ! SASU spinup diagnostics: accumulated flux exiting from live stem display N (gN m-2)
   real(r8), allocatable :: AKX_deadstemn_exit_p_acc                    (:) ! SASU spinup diagnostics: accumulated flux exiting from dead stem display N (gN m-2)
   real(r8), allocatable :: AKX_livecrootn_exit_p_acc                   (:) ! SASU spinup diagnostics: accumulated flux exiting from live coarse root display N (gN m-2)
   real(r8), allocatable :: AKX_deadcrootn_exit_p_acc                   (:) ! SASU spinup diagnostics: accumulated flux exiting from dead coarse root display N (gN m-2)
   real(r8), allocatable :: AKX_grainn_exit_p_acc                       (:) ! SASU spinup diagnostics: accumulated flux exiting from grain display N (gN m-2)
   real(r8), allocatable :: AKX_retransn_exit_p_acc                     (:) ! SASU spinup diagnostics: accumulated flux exiting from retranslocated N (gN m-2)

   real(r8), allocatable :: AKX_leafn_st_exit_p_acc                     (:) ! SASU spinup diagnostics: accumulated flux exiting from leaf storge N (gN m-2)
   real(r8), allocatable :: AKX_frootn_st_exit_p_acc                    (:) ! SASU spinup diagnostics: accumulated flux exiting from fine root storge N (gN m-2)
   real(r8), allocatable :: AKX_livestemn_st_exit_p_acc                 (:) ! SASU spinup diagnostics: accumulated flux exiting from live stem storge N (gN m-2)
   real(r8), allocatable :: AKX_deadstemn_st_exit_p_acc                 (:) ! SASU spinup diagnostics: accumulated flux exiting from dead stem storge N (gN m-2)
   real(r8), allocatable :: AKX_livecrootn_st_exit_p_acc                (:) ! SASU spinup diagnostics: accumulated flux exiting from live coarse root storge N (gN m-2)
   real(r8), allocatable :: AKX_deadcrootn_st_exit_p_acc                (:) ! SASU spinup diagnostics: accumulated flux exiting from dead coarse root storge N (gN m-2)
   real(r8), allocatable :: AKX_grainn_st_exit_p_acc                    (:) ! SASU spinup diagnostics: accumulated flux exiting from grain storge N (gN m-2)

   real(r8), allocatable :: AKX_leafn_xf_exit_p_acc                     (:) ! SASU spinup diagnostics: accumulated flux exiting from leaf transfer N (gN m-2)
   real(r8), allocatable :: AKX_frootn_xf_exit_p_acc                    (:) ! SASU spinup diagnostics: accumulated flux exiting from fine root transfer N (gN m-2)
   real(r8), allocatable :: AKX_livestemn_xf_exit_p_acc                 (:) ! SASU spinup diagnostics: accumulated flux exiting from live stem transfer N (gN m-2)
   real(r8), allocatable :: AKX_deadstemn_xf_exit_p_acc                 (:) ! SASU spinup diagnostics: accumulated flux exiting from dead stem transfer N (gN m-2)
   real(r8), allocatable :: AKX_livecrootn_xf_exit_p_acc                (:) ! SASU spinup diagnostics: accumulated flux exiting from live coarse root transfer N (gN m-2)
   real(r8), allocatable :: AKX_deadcrootn_xf_exit_p_acc                (:) ! SASU spinup diagnostics: accumulated flux exiting from dead coarse root transfer N (gN m-2)
   real(r8), allocatable :: AKX_grainn_xf_exit_p_acc                    (:) ! SASU spinup diagnostics: accumulated flux exiting from grain transfer N (gN m-2)
 !------------------------- END BGC/SASU variables ---------------------

 ! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: allocate_BGCPFTimeVariables
   PUBLIC :: deallocate_BGCPFTimeVariables
   PUBLIC :: READ_BGCPFTimeVariables
   PUBLIC :: WRITE_BGCPFTimeVariables
#ifdef RangeCheck
   PUBLIC :: check_BGCPFTimeVariables
#endif

! PRIVATE MEMBER FUNCTIONS:

!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------

   SUBROUTINE allocate_BGCPFTimeVariables ()
! ------------------------------------------------------
! Allocates memory for CoLM 1d [numpft] variables
! ------------------------------------------------------
   USE MOD_Precision
   USE MOD_MPAS_MPI
   USE MOD_LandPFT
   USE MOD_Vars_Global
   IMPLICIT NONE

      IF (.true.) THEN
         IF (numpft > 0) THEN
  ! bgc variables
            allocate (leafc_p                  (numpft)); leafc_p                  (:) = spval
            allocate (leafc_storage_p          (numpft)); leafc_storage_p          (:) = spval
            allocate (leafc_xfer_p             (numpft)); leafc_xfer_p             (:) = spval
            allocate (frootc_p                 (numpft)); frootc_p                 (:) = spval
            allocate (frootc_storage_p         (numpft)); frootc_storage_p         (:) = spval
            allocate (frootc_xfer_p            (numpft)); frootc_xfer_p            (:) = spval
            allocate (livestemc_p              (numpft)); livestemc_p              (:) = spval
            allocate (livestemc_storage_p      (numpft)); livestemc_storage_p      (:) = spval
            allocate (livestemc_xfer_p         (numpft)); livestemc_xfer_p         (:) = spval
            allocate (deadstemc_p              (numpft)); deadstemc_p              (:) = spval
            allocate (deadstemc_storage_p      (numpft)); deadstemc_storage_p      (:) = spval
            allocate (deadstemc_xfer_p         (numpft)); deadstemc_xfer_p         (:) = spval
            allocate (livecrootc_p             (numpft)); livecrootc_p             (:) = spval
            allocate (livecrootc_storage_p     (numpft)); livecrootc_storage_p     (:) = spval
            allocate (livecrootc_xfer_p        (numpft)); livecrootc_xfer_p        (:) = spval
            allocate (deadcrootc_p             (numpft)); deadcrootc_p             (:) = spval
            allocate (deadcrootc_storage_p     (numpft)); deadcrootc_storage_p     (:) = spval
            allocate (deadcrootc_xfer_p        (numpft)); deadcrootc_xfer_p        (:) = spval
            allocate (grainc_p                 (numpft)); grainc_p                 (:) = spval
            allocate (grainc_storage_p         (numpft)); grainc_storage_p         (:) = spval
            allocate (grainc_xfer_p            (numpft)); grainc_xfer_p            (:) = spval
            allocate (cropseedc_deficit_p      (numpft)); cropseedc_deficit_p      (:) = spval
            allocate (xsmrpool_p               (numpft)); xsmrpool_p               (:) = spval
            allocate (gresp_storage_p          (numpft)); gresp_storage_p          (:) = spval
            allocate (gresp_xfer_p             (numpft)); gresp_xfer_p             (:) = spval
            allocate (cpool_p                  (numpft)); cpool_p                  (:) = spval
            allocate (totvegc_p                (numpft)); totvegc_p                (:) = spval
            allocate (cropprod1c_p             (numpft)); cropprod1c_p             (:) = spval

            allocate (leaf_prof_p              (nl_soil,numpft)) ; leaf_prof_p     (:,:) = spval
            allocate (froot_prof_p             (nl_soil,numpft)) ; froot_prof_p    (:,:) = spval
            allocate (croot_prof_p             (nl_soil,numpft)) ; croot_prof_p    (:,:) = spval
            allocate (stem_prof_p              (nl_soil,numpft)) ; stem_prof_p     (:,:) = spval
            allocate (cinput_rootfr_p          (nl_soil,numpft)) ; cinput_rootfr_p (:,:) = spval

            allocate (leafn_p                  (numpft)); leafn_p                  (:) = spval
            allocate (leafn_storage_p          (numpft)); leafn_storage_p          (:) = spval
            allocate (leafn_xfer_p             (numpft)); leafn_xfer_p             (:) = spval
            allocate (frootn_p                 (numpft)); frootn_p                 (:) = spval
            allocate (frootn_storage_p         (numpft)); frootn_storage_p         (:) = spval
            allocate (frootn_xfer_p            (numpft)); frootn_xfer_p            (:) = spval
            allocate (livestemn_p              (numpft)); livestemn_p              (:) = spval
            allocate (livestemn_storage_p      (numpft)); livestemn_storage_p      (:) = spval
            allocate (livestemn_xfer_p         (numpft)); livestemn_xfer_p         (:) = spval
            allocate (deadstemn_p              (numpft)); deadstemn_p              (:) = spval
            allocate (deadstemn_storage_p      (numpft)); deadstemn_storage_p      (:) = spval
            allocate (deadstemn_xfer_p         (numpft)); deadstemn_xfer_p         (:) = spval
            allocate (livecrootn_p             (numpft)); livecrootn_p             (:) = spval
            allocate (livecrootn_storage_p     (numpft)); livecrootn_storage_p     (:) = spval
            allocate (livecrootn_xfer_p        (numpft)); livecrootn_xfer_p        (:) = spval
            allocate (deadcrootn_p             (numpft)); deadcrootn_p             (:) = spval
            allocate (deadcrootn_storage_p     (numpft)); deadcrootn_storage_p     (:) = spval
            allocate (deadcrootn_xfer_p        (numpft)); deadcrootn_xfer_p        (:) = spval
            allocate (grainn_p                 (numpft)); grainn_p                 (:) = spval
            allocate (grainn_storage_p         (numpft)); grainn_storage_p         (:) = spval
            allocate (grainn_xfer_p            (numpft)); grainn_xfer_p            (:) = spval
            allocate (cropseedn_deficit_p      (numpft)); cropseedn_deficit_p      (:) = spval
            allocate (retransn_p               (numpft)); retransn_p               (:) = spval
            allocate (totvegn_p                (numpft)); totvegn_p                (:) = spval

            allocate (harvdate_p               (numpft)); harvdate_p               (:) = spval

            allocate (tempsum_potential_gpp_p  (numpft)); tempsum_potential_gpp_p  (:) = spval
            allocate (tempmax_retransn_p       (numpft)); tempmax_retransn_p       (:) = spval
            allocate (tempavg_tref_p           (numpft)); tempavg_tref_p           (:) = spval
            allocate (tempsum_npp_p            (numpft)); tempsum_npp_p            (:) = spval
            allocate (tempsum_litfall_p        (numpft)); tempsum_litfall_p        (:) = spval
            allocate (annsum_potential_gpp_p   (numpft)); annsum_potential_gpp_p   (:) = spval
            allocate (annmax_retransn_p        (numpft)); annmax_retransn_p        (:) = spval
            allocate (annavg_tref_p            (numpft)); annavg_tref_p            (:) = spval
            allocate (annsum_npp_p             (numpft)); annsum_npp_p             (:) = spval
            allocate (annsum_litfall_p         (numpft)); annsum_litfall_p         (:) = spval

            allocate (bglfr_p                  (numpft)); bglfr_p                  (:) = spval
            allocate (bgtr_p                   (numpft)); bgtr_p                   (:) = spval
            allocate (lgsf_p                   (numpft)); lgsf_p                   (:) = spval
            allocate (gdd0_p                   (numpft)); gdd0_p                   (:) = spval
            allocate (gdd8_p                   (numpft)); gdd8_p                   (:) = spval
            allocate (gdd10_p                  (numpft)); gdd10_p                  (:) = spval
            allocate (gdd020_p                 (numpft)); gdd020_p                 (:) = spval
            allocate (gdd820_p                 (numpft)); gdd820_p                 (:) = spval
            allocate (gdd1020_p                (numpft)); gdd1020_p                (:) = spval
            allocate (nyrs_crop_active_p       (numpft)); nyrs_crop_active_p       (:) = spval_i4

            allocate (offset_flag_p            (numpft)); offset_flag_p            (:) = spval
            allocate (offset_counter_p         (numpft)); offset_counter_p         (:) = spval
            allocate (onset_flag_p             (numpft)); onset_flag_p             (:) = spval
            allocate (onset_counter_p          (numpft)); onset_counter_p          (:) = spval
            allocate (onset_gddflag_p          (numpft)); onset_gddflag_p          (:) = spval
            allocate (onset_gdd_p              (numpft)); onset_gdd_p              (:) = spval
            allocate (onset_fdd_p              (numpft)); onset_fdd_p              (:) = spval
            allocate (onset_swi_p              (numpft)); onset_swi_p              (:) = spval
            allocate (offset_fdd_p             (numpft)); offset_fdd_p             (:) = spval
            allocate (offset_swi_p             (numpft)); offset_swi_p             (:) = spval
            allocate (dormant_flag_p           (numpft)); dormant_flag_p           (:) = spval
            allocate (prev_leafc_to_litter_p   (numpft)); prev_leafc_to_litter_p   (:) = spval
            allocate (prev_frootc_to_litter_p  (numpft)); prev_frootc_to_litter_p  (:) = spval
            allocate (days_active_p            (numpft)); days_active_p            (:) = spval

            allocate (burndate_p               (numpft)); burndate_p               (:) = spval

            allocate (c_allometry_p            (numpft)); c_allometry_p            (:) = spval
            allocate (n_allometry_p            (numpft)); n_allometry_p            (:) = spval
            allocate (downreg_p                (numpft)); downreg_p                (:) = spval
            allocate (grain_flag_p             (numpft)); grain_flag_p             (:) = spval

            allocate (ctrunc_p                 (numpft)); ctrunc_p                 (:) = spval
            allocate (ntrunc_p                 (numpft)); ntrunc_p                 (:) = spval
            allocate (npool_p                  (numpft)); npool_p                  (:) = spval

#ifdef CROP
! crop variables
            allocate (croplive_p               (numpft)); croplive_p               (:) = .false.
            allocate (hui_p                    (numpft)); hui_p                    (:) = spval
            allocate (gddplant_p               (numpft)); gddplant_p               (:) = spval
            allocate (peaklai_p                (numpft)); peaklai_p                (:) = spval_i4
            allocate (aroot_p                  (numpft)); aroot_p                  (:) = spval
            allocate (astem_p                  (numpft)); astem_p                  (:) = spval
            allocate (arepr_p                  (numpft)); arepr_p                  (:) = spval
            allocate (aleaf_p                  (numpft)); aleaf_p                  (:) = spval
            allocate (astemi_p                 (numpft)); astemi_p                 (:) = spval
            allocate (aleafi_p                 (numpft)); aleafi_p                 (:) = spval
            allocate (gddmaturity_p            (numpft)); gddmaturity_p            (:) = spval

            allocate (cropplant_p              (numpft)); cropplant_p              (:) = .false.
            allocate (idop_p                   (numpft)); idop_p                   (:) = spval_i4
            allocate (a5tmin_p                 (numpft)); a5tmin_p                 (:) = spval
            allocate (a10tmin_p                (numpft)); a10tmin_p                (:) = spval
            allocate (t10_p                    (numpft)); t10_p                    (:) = spval
            allocate (cumvd_p                  (numpft)); cumvd_p                  (:) = spval
            allocate (vf_p                     (numpft)); vf_p                     (:) = spval
            allocate (cphase_p                 (numpft)); cphase_p                 (:) = spval
            allocate (fert_counter_p           (numpft)); fert_counter_p           (:) = spval
            allocate (tref_min_p               (numpft)); tref_min_p               (:) = spval
            allocate (tref_max_p               (numpft)); tref_max_p               (:) = spval
            allocate (tref_min_inst_p          (numpft)); tref_min_inst_p          (:) = spval
            allocate (tref_max_inst_p          (numpft)); tref_max_inst_p          (:) = spval
            allocate (fertnitro_p              (numpft)); fertnitro_p              (:) = spval
            allocate (manunitro_p              (numpft)); manunitro_p              (:) = spval
            allocate (fert_p                   (numpft)); fert_p                   (:) = spval
            allocate (latbaset_p               (numpft)); latbaset_p               (:) = spval
            allocate (plantdate_p              (numpft)); plantdate_p              (:) = spval
#endif

! SASU variables
            allocate (leafcCap_p               (numpft)); leafcCap_p               (:) = spval
            allocate (leafc_storageCap_p       (numpft)); leafc_storageCap_p       (:) = spval
            allocate (leafc_xferCap_p          (numpft)); leafc_xferCap_p          (:) = spval
            allocate (frootcCap_p              (numpft)); frootcCap_p              (:) = spval
            allocate (frootc_storageCap_p      (numpft)); frootc_storageCap_p      (:) = spval
            allocate (frootc_xferCap_p         (numpft)); frootc_xferCap_p         (:) = spval
            allocate (livestemcCap_p           (numpft)); livestemcCap_p           (:) = spval
            allocate (livestemc_storageCap_p   (numpft)); livestemc_storageCap_p   (:) = spval
            allocate (livestemc_xferCap_p      (numpft)); livestemc_xferCap_p      (:) = spval
            allocate (deadstemcCap_p           (numpft)); deadstemcCap_p           (:) = spval
            allocate (deadstemc_storageCap_p   (numpft)); deadstemc_storageCap_p   (:) = spval
            allocate (deadstemc_xferCap_p      (numpft)); deadstemc_xferCap_p      (:) = spval
            allocate (livecrootcCap_p          (numpft)); livecrootcCap_p          (:) = spval
            allocate (livecrootc_storageCap_p  (numpft)); livecrootc_storageCap_p  (:) = spval
            allocate (livecrootc_xferCap_p     (numpft)); livecrootc_xferCap_p     (:) = spval
            allocate (deadcrootcCap_p          (numpft)); deadcrootcCap_p          (:) = spval
            allocate (deadcrootc_storageCap_p  (numpft)); deadcrootc_storageCap_p  (:) = spval
            allocate (deadcrootc_xferCap_p     (numpft)); deadcrootc_xferCap_p     (:) = spval

            allocate (leafnCap_p               (numpft)); leafnCap_p               (:) = spval
            allocate (leafn_storageCap_p       (numpft)); leafn_storageCap_p       (:) = spval
            allocate (leafn_xferCap_p          (numpft)); leafn_xferCap_p          (:) = spval
            allocate (frootnCap_p              (numpft)); frootnCap_p              (:) = spval
            allocate (frootn_storageCap_p      (numpft)); frootn_storageCap_p      (:) = spval
            allocate (frootn_xferCap_p         (numpft)); frootn_xferCap_p         (:) = spval
            allocate (livestemnCap_p           (numpft)); livestemnCap_p           (:) = spval
            allocate (livestemn_storageCap_p   (numpft)); livestemn_storageCap_p   (:) = spval
            allocate (livestemn_xferCap_p      (numpft)); livestemn_xferCap_p      (:) = spval
            allocate (deadstemnCap_p           (numpft)); deadstemnCap_p           (:) = spval
            allocate (deadstemn_storageCap_p   (numpft)); deadstemn_storageCap_p   (:) = spval
            allocate (deadstemn_xferCap_p      (numpft)); deadstemn_xferCap_p      (:) = spval
            allocate (livecrootnCap_p          (numpft)); livecrootnCap_p          (:) = spval
            allocate (livecrootn_storageCap_p  (numpft)); livecrootn_storageCap_p  (:) = spval
            allocate (livecrootn_xferCap_p     (numpft)); livecrootn_xferCap_p     (:) = spval
            allocate (deadcrootnCap_p          (numpft)); deadcrootnCap_p          (:) = spval
            allocate (deadcrootn_storageCap_p  (numpft)); deadcrootn_storageCap_p  (:) = spval
            allocate (deadcrootn_xferCap_p     (numpft)); deadcrootn_xferCap_p     (:) = spval

            allocate (leafc0_p                 (numpft)); leafc0_p                 (:) = spval
            allocate (leafc0_storage_p         (numpft)); leafc0_storage_p         (:) = spval
            allocate (leafc0_xfer_p            (numpft)); leafc0_xfer_p            (:) = spval
            allocate (frootc0_p                (numpft)); frootc0_p                (:) = spval
            allocate (frootc0_storage_p        (numpft)); frootc0_storage_p        (:) = spval
            allocate (frootc0_xfer_p           (numpft)); frootc0_xfer_p           (:) = spval
            allocate (livestemc0_p             (numpft)); livestemc0_p             (:) = spval
            allocate (livestemc0_storage_p     (numpft)); livestemc0_storage_p     (:) = spval
            allocate (livestemc0_xfer_p        (numpft)); livestemc0_xfer_p        (:) = spval
            allocate (deadstemc0_p             (numpft)); deadstemc0_p             (:) = spval
            allocate (deadstemc0_storage_p     (numpft)); deadstemc0_storage_p     (:) = spval
            allocate (deadstemc0_xfer_p        (numpft)); deadstemc0_xfer_p        (:) = spval
            allocate (livecrootc0_p            (numpft)); livecrootc0_p            (:) = spval
            allocate (livecrootc0_storage_p    (numpft)); livecrootc0_storage_p    (:) = spval
            allocate (livecrootc0_xfer_p       (numpft)); livecrootc0_xfer_p       (:) = spval
            allocate (deadcrootc0_p            (numpft)); deadcrootc0_p            (:) = spval
            allocate (deadcrootc0_storage_p    (numpft)); deadcrootc0_storage_p    (:) = spval
            allocate (deadcrootc0_xfer_p       (numpft)); deadcrootc0_xfer_p       (:) = spval
            allocate (grainc0_p                (numpft)); grainc0_p                (:) = spval
            allocate (grainc0_storage_p        (numpft)); grainc0_storage_p        (:) = spval
            allocate (grainc0_xfer_p           (numpft)); grainc0_xfer_p           (:) = spval

            allocate (leafn0_p                 (numpft)); leafn0_p                 (:) = spval
            allocate (leafn0_storage_p         (numpft)); leafn0_storage_p         (:) = spval
            allocate (leafn0_xfer_p            (numpft)); leafn0_xfer_p            (:) = spval
            allocate (frootn0_p                (numpft)); frootn0_p                (:) = spval
            allocate (frootn0_storage_p        (numpft)); frootn0_storage_p        (:) = spval
            allocate (frootn0_xfer_p           (numpft)); frootn0_xfer_p           (:) = spval
            allocate (livestemn0_p             (numpft)); livestemn0_p             (:) = spval
            allocate (livestemn0_storage_p     (numpft)); livestemn0_storage_p     (:) = spval
            allocate (livestemn0_xfer_p        (numpft)); livestemn0_xfer_p        (:) = spval
            allocate (deadstemn0_p             (numpft)); deadstemn0_p             (:) = spval
            allocate (deadstemn0_storage_p     (numpft)); deadstemn0_storage_p     (:) = spval
            allocate (deadstemn0_xfer_p        (numpft)); deadstemn0_xfer_p        (:) = spval
            allocate (livecrootn0_p            (numpft)); livecrootn0_p            (:) = spval
            allocate (livecrootn0_storage_p    (numpft)); livecrootn0_storage_p    (:) = spval
            allocate (livecrootn0_xfer_p       (numpft)); livecrootn0_xfer_p       (:) = spval
            allocate (deadcrootn0_p            (numpft)); deadcrootn0_p            (:) = spval
            allocate (deadcrootn0_storage_p    (numpft)); deadcrootn0_storage_p    (:) = spval
            allocate (deadcrootn0_xfer_p       (numpft)); deadcrootn0_xfer_p       (:) = spval
            allocate (grainn0_p                (numpft)); grainn0_p                (:) = spval
            allocate (grainn0_storage_p        (numpft)); grainn0_storage_p        (:) = spval
            allocate (grainn0_xfer_p           (numpft)); grainn0_xfer_p           (:) = spval
            allocate (retransn0_p              (numpft)); retransn0_p              (:) = spval

            allocate (I_leafc_p_acc            (numpft)); I_leafc_p_acc            (:) = spval
            allocate (I_leafc_st_p_acc         (numpft)); I_leafc_st_p_acc         (:) = spval
            allocate (I_frootc_p_acc           (numpft)); I_frootc_p_acc           (:) = spval
            allocate (I_frootc_st_p_acc        (numpft)); I_frootc_st_p_acc        (:) = spval
            allocate (I_livestemc_p_acc        (numpft)); I_livestemc_p_acc        (:) = spval
            allocate (I_livestemc_st_p_acc     (numpft)); I_livestemc_st_p_acc     (:) = spval
            allocate (I_deadstemc_p_acc        (numpft)); I_deadstemc_p_acc        (:) = spval
            allocate (I_deadstemc_st_p_acc     (numpft)); I_deadstemc_st_p_acc     (:) = spval
            allocate (I_livecrootc_p_acc       (numpft)); I_livecrootc_p_acc       (:) = spval
            allocate (I_livecrootc_st_p_acc    (numpft)); I_livecrootc_st_p_acc    (:) = spval
            allocate (I_deadcrootc_p_acc       (numpft)); I_deadcrootc_p_acc       (:) = spval
            allocate (I_deadcrootc_st_p_acc    (numpft)); I_deadcrootc_st_p_acc    (:) = spval
            allocate (I_grainc_p_acc           (numpft)); I_grainc_p_acc           (:) = spval
            allocate (I_grainc_st_p_acc        (numpft)); I_grainc_st_p_acc        (:) = spval
            allocate (I_leafn_p_acc            (numpft)); I_leafn_p_acc            (:) = spval
            allocate (I_leafn_st_p_acc         (numpft)); I_leafn_st_p_acc         (:) = spval
            allocate (I_frootn_p_acc           (numpft)); I_frootn_p_acc           (:) = spval
            allocate (I_frootn_st_p_acc        (numpft)); I_frootn_st_p_acc        (:) = spval
            allocate (I_livestemn_p_acc        (numpft)); I_livestemn_p_acc        (:) = spval
            allocate (I_livestemn_st_p_acc     (numpft)); I_livestemn_st_p_acc     (:) = spval
            allocate (I_deadstemn_p_acc        (numpft)); I_deadstemn_p_acc        (:) = spval
            allocate (I_deadstemn_st_p_acc     (numpft)); I_deadstemn_st_p_acc     (:) = spval
            allocate (I_livecrootn_p_acc       (numpft)); I_livecrootn_p_acc       (:) = spval
            allocate (I_livecrootn_st_p_acc    (numpft)); I_livecrootn_st_p_acc    (:) = spval
            allocate (I_deadcrootn_p_acc       (numpft)); I_deadcrootn_p_acc       (:) = spval
            allocate (I_deadcrootn_st_p_acc    (numpft)); I_deadcrootn_st_p_acc    (:) = spval
            allocate (I_grainn_p_acc           (numpft)); I_grainn_p_acc           (:) = spval
            allocate (I_grainn_st_p_acc        (numpft)); I_grainn_st_p_acc        (:) = spval

            allocate (AKX_leafc_xf_to_leafc_p_acc                 (numpft)); AKX_leafc_xf_to_leafc_p_acc                 (:) = spval
            allocate (AKX_frootc_xf_to_frootc_p_acc               (numpft)); AKX_frootc_xf_to_frootc_p_acc               (:) = spval
            allocate (AKX_livestemc_xf_to_livestemc_p_acc         (numpft)); AKX_livestemc_xf_to_livestemc_p_acc         (:) = spval
            allocate (AKX_deadstemc_xf_to_deadstemc_p_acc         (numpft)); AKX_deadstemc_xf_to_deadstemc_p_acc         (:) = spval
            allocate (AKX_livecrootc_xf_to_livecrootc_p_acc       (numpft)); AKX_livecrootc_xf_to_livecrootc_p_acc       (:) = spval
            allocate (AKX_deadcrootc_xf_to_deadcrootc_p_acc       (numpft)); AKX_deadcrootc_xf_to_deadcrootc_p_acc       (:) = spval
            allocate (AKX_grainc_xf_to_grainc_p_acc               (numpft)); AKX_grainc_xf_to_grainc_p_acc               (:) = spval
            allocate (AKX_livestemc_to_deadstemc_p_acc            (numpft)); AKX_livestemc_to_deadstemc_p_acc            (:) = spval
            allocate (AKX_livecrootc_to_deadcrootc_p_acc          (numpft)); AKX_livecrootc_to_deadcrootc_p_acc          (:) = spval

            allocate (AKX_leafc_st_to_leafc_xf_p_acc              (numpft)); AKX_leafc_st_to_leafc_xf_p_acc              (:) = spval
            allocate (AKX_frootc_st_to_frootc_xf_p_acc            (numpft)); AKX_frootc_st_to_frootc_xf_p_acc            (:) = spval
            allocate (AKX_livestemc_st_to_livestemc_xf_p_acc      (numpft)); AKX_livestemc_st_to_livestemc_xf_p_acc      (:) = spval
            allocate (AKX_deadstemc_st_to_deadstemc_xf_p_acc      (numpft)); AKX_deadstemc_st_to_deadstemc_xf_p_acc      (:) = spval
            allocate (AKX_livecrootc_st_to_livecrootc_xf_p_acc    (numpft)); AKX_livecrootc_st_to_livecrootc_xf_p_acc    (:) = spval
            allocate (AKX_deadcrootc_st_to_deadcrootc_xf_p_acc    (numpft)); AKX_deadcrootc_st_to_deadcrootc_xf_p_acc    (:) = spval
            allocate (AKX_grainc_st_to_grainc_xf_p_acc            (numpft)); AKX_grainc_st_to_grainc_xf_p_acc            (:) = spval

            allocate (AKX_leafc_exit_p_acc                        (numpft)); AKX_leafc_exit_p_acc                        (:) = spval
            allocate (AKX_frootc_exit_p_acc                       (numpft)); AKX_frootc_exit_p_acc                       (:) = spval
            allocate (AKX_livestemc_exit_p_acc                    (numpft)); AKX_livestemc_exit_p_acc                    (:) = spval
            allocate (AKX_deadstemc_exit_p_acc                    (numpft)); AKX_deadstemc_exit_p_acc                    (:) = spval
            allocate (AKX_livecrootc_exit_p_acc                   (numpft)); AKX_livecrootc_exit_p_acc                   (:) = spval
            allocate (AKX_deadcrootc_exit_p_acc                   (numpft)); AKX_deadcrootc_exit_p_acc                   (:) = spval
            allocate (AKX_grainc_exit_p_acc                       (numpft)); AKX_grainc_exit_p_acc                       (:) = spval

            allocate (AKX_leafc_st_exit_p_acc                     (numpft)); AKX_leafc_st_exit_p_acc                     (:) = spval
            allocate (AKX_frootc_st_exit_p_acc                    (numpft)); AKX_frootc_st_exit_p_acc                    (:) = spval
            allocate (AKX_livestemc_st_exit_p_acc                 (numpft)); AKX_livestemc_st_exit_p_acc                 (:) = spval
            allocate (AKX_deadstemc_st_exit_p_acc                 (numpft)); AKX_deadstemc_st_exit_p_acc                 (:) = spval
            allocate (AKX_livecrootc_st_exit_p_acc                (numpft)); AKX_livecrootc_st_exit_p_acc                (:) = spval
            allocate (AKX_deadcrootc_st_exit_p_acc                (numpft)); AKX_deadcrootc_st_exit_p_acc                (:) = spval
            allocate (AKX_grainc_st_exit_p_acc                    (numpft)); AKX_grainc_st_exit_p_acc                    (:) = spval

            allocate (AKX_leafc_xf_exit_p_acc                     (numpft)); AKX_leafc_xf_exit_p_acc                     (:) = spval
            allocate (AKX_frootc_xf_exit_p_acc                    (numpft)); AKX_frootc_xf_exit_p_acc                    (:) = spval
            allocate (AKX_livestemc_xf_exit_p_acc                 (numpft)); AKX_livestemc_xf_exit_p_acc                 (:) = spval
            allocate (AKX_deadstemc_xf_exit_p_acc                 (numpft)); AKX_deadstemc_xf_exit_p_acc                 (:) = spval
            allocate (AKX_livecrootc_xf_exit_p_acc                (numpft)); AKX_livecrootc_xf_exit_p_acc                (:) = spval
            allocate (AKX_deadcrootc_xf_exit_p_acc                (numpft)); AKX_deadcrootc_xf_exit_p_acc                (:) = spval
            allocate (AKX_grainc_xf_exit_p_acc                    (numpft)); AKX_grainc_xf_exit_p_acc                    (:) = spval

            allocate (AKX_leafn_xf_to_leafn_p_acc                 (numpft)); AKX_leafn_xf_to_leafn_p_acc                 (:) = spval
            allocate (AKX_frootn_xf_to_frootn_p_acc               (numpft)); AKX_frootn_xf_to_frootn_p_acc               (:) = spval
            allocate (AKX_livestemn_xf_to_livestemn_p_acc         (numpft)); AKX_livestemn_xf_to_livestemn_p_acc         (:) = spval
            allocate (AKX_deadstemn_xf_to_deadstemn_p_acc         (numpft)); AKX_deadstemn_xf_to_deadstemn_p_acc         (:) = spval
            allocate (AKX_livecrootn_xf_to_livecrootn_p_acc       (numpft)); AKX_livecrootn_xf_to_livecrootn_p_acc       (:) = spval
            allocate (AKX_deadcrootn_xf_to_deadcrootn_p_acc       (numpft)); AKX_deadcrootn_xf_to_deadcrootn_p_acc       (:) = spval
            allocate (AKX_grainn_xf_to_grainn_p_acc               (numpft)); AKX_grainn_xf_to_grainn_p_acc               (:) = spval
            allocate (AKX_livestemn_to_deadstemn_p_acc            (numpft)); AKX_livestemn_to_deadstemn_p_acc            (:) = spval
            allocate (AKX_livecrootn_to_deadcrootn_p_acc          (numpft)); AKX_livecrootn_to_deadcrootn_p_acc          (:) = spval

            allocate (AKX_leafn_st_to_leafn_xf_p_acc              (numpft)); AKX_leafn_st_to_leafn_xf_p_acc              (:) = spval
            allocate (AKX_frootn_st_to_frootn_xf_p_acc            (numpft)); AKX_frootn_st_to_frootn_xf_p_acc            (:) = spval
            allocate (AKX_livestemn_st_to_livestemn_xf_p_acc      (numpft)); AKX_livestemn_st_to_livestemn_xf_p_acc      (:) = spval
            allocate (AKX_deadstemn_st_to_deadstemn_xf_p_acc      (numpft)); AKX_deadstemn_st_to_deadstemn_xf_p_acc      (:) = spval
            allocate (AKX_livecrootn_st_to_livecrootn_xf_p_acc    (numpft)); AKX_livecrootn_st_to_livecrootn_xf_p_acc    (:) = spval
            allocate (AKX_deadcrootn_st_to_deadcrootn_xf_p_acc    (numpft)); AKX_deadcrootn_st_to_deadcrootn_xf_p_acc    (:) = spval
            allocate (AKX_grainn_st_to_grainn_xf_p_acc            (numpft)); AKX_grainn_st_to_grainn_xf_p_acc            (:) = spval

            allocate (AKX_leafn_to_retransn_p_acc                 (numpft)); AKX_leafn_to_retransn_p_acc                 (:) = spval
            allocate (AKX_frootn_to_retransn_p_acc                (numpft)); AKX_frootn_to_retransn_p_acc                (:) = spval
            allocate (AKX_livestemn_to_retransn_p_acc             (numpft)); AKX_livestemn_to_retransn_p_acc             (:) = spval
            allocate (AKX_livecrootn_to_retransn_p_acc            (numpft)); AKX_livecrootn_to_retransn_p_acc            (:) = spval

            allocate (AKX_retransn_to_leafn_p_acc                 (numpft)); AKX_retransn_to_leafn_p_acc                 (:) = spval
            allocate (AKX_retransn_to_frootn_p_acc                (numpft)); AKX_retransn_to_frootn_p_acc                (:) = spval
            allocate (AKX_retransn_to_livestemn_p_acc             (numpft)); AKX_retransn_to_livestemn_p_acc             (:) = spval
            allocate (AKX_retransn_to_deadstemn_p_acc             (numpft)); AKX_retransn_to_deadstemn_p_acc             (:) = spval
            allocate (AKX_retransn_to_livecrootn_p_acc            (numpft)); AKX_retransn_to_livecrootn_p_acc            (:) = spval
            allocate (AKX_retransn_to_deadcrootn_p_acc            (numpft)); AKX_retransn_to_deadcrootn_p_acc            (:) = spval
            allocate (AKX_retransn_to_grainn_p_acc                (numpft)); AKX_retransn_to_grainn_p_acc                (:) = spval

            allocate (AKX_retransn_to_leafn_st_p_acc              (numpft)); AKX_retransn_to_leafn_st_p_acc              (:) = spval
            allocate (AKX_retransn_to_frootn_st_p_acc             (numpft)); AKX_retransn_to_frootn_st_p_acc             (:) = spval
            allocate (AKX_retransn_to_livestemn_st_p_acc          (numpft)); AKX_retransn_to_livestemn_st_p_acc          (:) = spval
            allocate (AKX_retransn_to_deadstemn_st_p_acc          (numpft)); AKX_retransn_to_deadstemn_st_p_acc          (:) = spval
            allocate (AKX_retransn_to_livecrootn_st_p_acc         (numpft)); AKX_retransn_to_livecrootn_st_p_acc         (:) = spval
            allocate (AKX_retransn_to_deadcrootn_st_p_acc         (numpft)); AKX_retransn_to_deadcrootn_st_p_acc         (:) = spval
            allocate (AKX_retransn_to_grainn_st_p_acc             (numpft)); AKX_retransn_to_grainn_st_p_acc             (:) = spval

            allocate (AKX_leafn_exit_p_acc                        (numpft)); AKX_leafn_exit_p_acc                        (:) = spval
            allocate (AKX_frootn_exit_p_acc                       (numpft)); AKX_frootn_exit_p_acc                       (:) = spval
            allocate (AKX_livestemn_exit_p_acc                    (numpft)); AKX_livestemn_exit_p_acc                    (:) = spval
            allocate (AKX_deadstemn_exit_p_acc                    (numpft)); AKX_deadstemn_exit_p_acc                    (:) = spval
            allocate (AKX_livecrootn_exit_p_acc                   (numpft)); AKX_livecrootn_exit_p_acc                   (:) = spval
            allocate (AKX_deadcrootn_exit_p_acc                   (numpft)); AKX_deadcrootn_exit_p_acc                   (:) = spval
            allocate (AKX_grainn_exit_p_acc                       (numpft)); AKX_grainn_exit_p_acc                       (:) = spval
            allocate (AKX_retransn_exit_p_acc                     (numpft)); AKX_retransn_exit_p_acc                     (:) = spval

            allocate (AKX_leafn_st_exit_p_acc                     (numpft)); AKX_leafn_st_exit_p_acc                     (:) = spval
            allocate (AKX_frootn_st_exit_p_acc                    (numpft)); AKX_frootn_st_exit_p_acc                    (:) = spval
            allocate (AKX_livestemn_st_exit_p_acc                 (numpft)); AKX_livestemn_st_exit_p_acc                 (:) = spval
            allocate (AKX_deadstemn_st_exit_p_acc                 (numpft)); AKX_deadstemn_st_exit_p_acc                 (:) = spval
            allocate (AKX_livecrootn_st_exit_p_acc                (numpft)); AKX_livecrootn_st_exit_p_acc                (:) = spval
            allocate (AKX_deadcrootn_st_exit_p_acc                (numpft)); AKX_deadcrootn_st_exit_p_acc                (:) = spval
            allocate (AKX_grainn_st_exit_p_acc                    (numpft)); AKX_grainn_st_exit_p_acc                    (:) = spval

            allocate (AKX_leafn_xf_exit_p_acc                     (numpft)); AKX_leafn_xf_exit_p_acc                     (:) = spval
            allocate (AKX_frootn_xf_exit_p_acc                    (numpft)); AKX_frootn_xf_exit_p_acc                    (:) = spval
            allocate (AKX_livestemn_xf_exit_p_acc                 (numpft)); AKX_livestemn_xf_exit_p_acc                 (:) = spval
            allocate (AKX_deadstemn_xf_exit_p_acc                 (numpft)); AKX_deadstemn_xf_exit_p_acc                 (:) = spval
            allocate (AKX_livecrootn_xf_exit_p_acc                (numpft)); AKX_livecrootn_xf_exit_p_acc                (:) = spval
            allocate (AKX_deadcrootn_xf_exit_p_acc                (numpft)); AKX_deadcrootn_xf_exit_p_acc                (:) = spval
            allocate (AKX_grainn_xf_exit_p_acc                    (numpft)); AKX_grainn_xf_exit_p_acc                    (:) = spval
         ENDIF
      ENDIF

   END SUBROUTINE allocate_BGCPFTimeVariables

   SUBROUTINE READ_BGCPFTimeVariables (file_restart)

   USE MOD_NetCDFVector
   USE MOD_LandPFT
   USE MOD_Vars_Global

   IMPLICIT NONE

   character(len=*), intent(in) :: file_restart

! bgc variables
      CALL ncio_read_vector (file_restart, 'leafc_p                ', landpft, leafc_p               )
      CALL ncio_read_vector (file_restart, 'leafc_storage_p        ', landpft, leafc_storage_p       )
      CALL ncio_read_vector (file_restart, 'leafc_xfer_p           ', landpft, leafc_xfer_p          )
      CALL ncio_read_vector (file_restart, 'frootc_p               ', landpft, frootc_p              )
      CALL ncio_read_vector (file_restart, 'frootc_storage_p       ', landpft, frootc_storage_p      )
      CALL ncio_read_vector (file_restart, 'frootc_xfer_p          ', landpft, frootc_xfer_p         )
      CALL ncio_read_vector (file_restart, 'livestemc_p            ', landpft, livestemc_p           )
      CALL ncio_read_vector (file_restart, 'livestemc_storage_p    ', landpft, livestemc_storage_p   )
      CALL ncio_read_vector (file_restart, 'livestemc_xfer_p       ', landpft, livestemc_xfer_p      )
      CALL ncio_read_vector (file_restart, 'deadstemc_p            ', landpft, deadstemc_p           )
      CALL ncio_read_vector (file_restart, 'deadstemc_storage_p    ', landpft, deadstemc_storage_p   )
      CALL ncio_read_vector (file_restart, 'deadstemc_xfer_p       ', landpft, deadstemc_xfer_p      )
      CALL ncio_read_vector (file_restart, 'livecrootc_p           ', landpft, livecrootc_p          )
      CALL ncio_read_vector (file_restart, 'livecrootc_storage_p   ', landpft, livecrootc_storage_p  )
      CALL ncio_read_vector (file_restart, 'livecrootc_xfer_p      ', landpft, livecrootc_xfer_p     )
      CALL ncio_read_vector (file_restart, 'deadcrootc_p           ', landpft, deadcrootc_p          )
      CALL ncio_read_vector (file_restart, 'deadcrootc_storage_p   ', landpft, deadcrootc_storage_p  )
      CALL ncio_read_vector (file_restart, 'deadcrootc_xfer_p      ', landpft, deadcrootc_xfer_p     )
      CALL ncio_read_vector (file_restart, 'grainc_p               ', landpft, grainc_p              )
      CALL ncio_read_vector (file_restart, 'grainc_storage_p       ', landpft, grainc_storage_p      )
      CALL ncio_read_vector (file_restart, 'grainc_xfer_p          ', landpft, grainc_xfer_p         )
      CALL ncio_read_vector (file_restart, 'cropseedc_deficit_p    ', landpft, cropseedc_deficit_p   )
      CALL ncio_read_vector (file_restart, 'xsmrpool_p             ', landpft, xsmrpool_p            )
      CALL ncio_read_vector (file_restart, 'gresp_storage_p        ', landpft, gresp_storage_p       )
      CALL ncio_read_vector (file_restart, 'gresp_xfer_p           ', landpft, gresp_xfer_p          )
      CALL ncio_read_vector (file_restart, 'cpool_p                ', landpft, cpool_p               )
 !     CALL ncio_read_vector (file_restart, 'totvegc_p              ', landpft, totvegc_p             )
      CALL ncio_read_vector (file_restart, 'cropprod1c_p           ', landpft, cropprod1c_p          )

      CALL ncio_read_vector (file_restart, 'leafn_p                ', landpft, leafn_p               )
      CALL ncio_read_vector (file_restart, 'leafn_storage_p        ', landpft, leafn_storage_p       )
      CALL ncio_read_vector (file_restart, 'leafn_xfer_p           ', landpft, leafn_xfer_p          )
      CALL ncio_read_vector (file_restart, 'frootn_p               ', landpft, frootn_p              )
      CALL ncio_read_vector (file_restart, 'frootn_storage_p       ', landpft, frootn_storage_p      )
      CALL ncio_read_vector (file_restart, 'frootn_xfer_p          ', landpft, frootn_xfer_p         )
      CALL ncio_read_vector (file_restart, 'livestemn_p            ', landpft, livestemn_p           )
      CALL ncio_read_vector (file_restart, 'livestemn_storage_p    ', landpft, livestemn_storage_p   )
      CALL ncio_read_vector (file_restart, 'livestemn_xfer_p       ', landpft, livestemn_xfer_p      )
      CALL ncio_read_vector (file_restart, 'deadstemn_p            ', landpft, deadstemn_p           )
      CALL ncio_read_vector (file_restart, 'deadstemn_storage_p    ', landpft, deadstemn_storage_p   )
      CALL ncio_read_vector (file_restart, 'deadstemn_xfer_p       ', landpft, deadstemn_xfer_p      )
      CALL ncio_read_vector (file_restart, 'livecrootn_p           ', landpft, livecrootn_p          )
      CALL ncio_read_vector (file_restart, 'livecrootn_storage_p   ', landpft, livecrootn_storage_p  )
      CALL ncio_read_vector (file_restart, 'livecrootn_xfer_p      ', landpft, livecrootn_xfer_p     )
      CALL ncio_read_vector (file_restart, 'deadcrootn_p           ', landpft, deadcrootn_p          )
      CALL ncio_read_vector (file_restart, 'deadcrootn_storage_p   ', landpft, deadcrootn_storage_p  )
      CALL ncio_read_vector (file_restart, 'deadcrootn_xfer_p      ', landpft, deadcrootn_xfer_p     )
      CALL ncio_read_vector (file_restart, 'grainn_p               ', landpft, grainn_p              )
      CALL ncio_read_vector (file_restart, 'grainn_storage_p       ', landpft, grainn_storage_p      )
      CALL ncio_read_vector (file_restart, 'grainn_xfer_p          ', landpft, grainn_xfer_p         )
      CALL ncio_read_vector (file_restart, 'cropseedn_deficit_p    ', landpft, cropseedn_deficit_p   )
      CALL ncio_read_vector (file_restart, 'retransn_p             ', landpft, retransn_p            )
 !     CALL ncio_read_vector (file_restart, 'totvegn_p              ', landpft, totvegn_p             )

      CALL ncio_read_vector (file_restart, 'harvdate_p             ', landpft, harvdate_p            )

      CALL ncio_read_vector (file_restart, 'tempsum_potential_gpp_p', landpft, tempsum_potential_gpp_p)
      CALL ncio_read_vector (file_restart, 'tempmax_retransn_p     ', landpft, tempmax_retransn_p    )
      CALL ncio_read_vector (file_restart, 'tempavg_tref_p         ', landpft, tempavg_tref_p        )
      CALL ncio_read_vector (file_restart, 'tempsum_npp_p          ', landpft, tempsum_npp_p         )
      CALL ncio_read_vector (file_restart, 'tempsum_litfall_p      ', landpft, tempsum_litfall_p     )
      CALL ncio_read_vector (file_restart, 'annsum_potential_gpp_p ', landpft, annsum_potential_gpp_p)
      CALL ncio_read_vector (file_restart, 'annmax_retransn_p      ', landpft, annmax_retransn_p     )
      CALL ncio_read_vector (file_restart, 'annavg_tref_p          ', landpft, annavg_tref_p         )
      CALL ncio_read_vector (file_restart, 'annsum_npp_p           ', landpft, annsum_npp_p          )
      CALL ncio_read_vector (file_restart, 'annsum_litfall_p       ', landpft, annsum_litfall_p      )

      CALL ncio_read_vector (file_restart, 'bglfr_p                ', landpft, bglfr_p               )
      CALL ncio_read_vector (file_restart, 'bgtr_p                 ', landpft, bgtr_p                )
      CALL ncio_read_vector (file_restart, 'lgsf_p                 ', landpft, lgsf_p                )
      CALL ncio_read_vector (file_restart, 'gdd0_p                 ', landpft, gdd0_p                )
      CALL ncio_read_vector (file_restart, 'gdd8_p                 ', landpft, gdd8_p                )
      CALL ncio_read_vector (file_restart, 'gdd10_p                ', landpft, gdd10_p               )
      CALL ncio_read_vector (file_restart, 'gdd020_p               ', landpft, gdd020_p              )
      CALL ncio_read_vector (file_restart, 'gdd820_p               ', landpft, gdd820_p              )
      CALL ncio_read_vector (file_restart, 'gdd1020_p              ', landpft, gdd1020_p             )
      CALL ncio_read_vector (file_restart, 'nyrs_crop_active_p     ', landpft, nyrs_crop_active_p    )

      CALL ncio_read_vector (file_restart, 'offset_flag_p          ', landpft, offset_flag_p         )
      CALL ncio_read_vector (file_restart, 'offset_counter_p       ', landpft, offset_counter_p      )
      CALL ncio_read_vector (file_restart, 'onset_flag_p           ', landpft, onset_flag_p          )
      CALL ncio_read_vector (file_restart, 'onset_counter_p        ', landpft, onset_counter_p       )
      CALL ncio_read_vector (file_restart, 'onset_gddflag_p        ', landpft, onset_gddflag_p       )
      CALL ncio_read_vector (file_restart, 'onset_gdd_p            ', landpft, onset_gdd_p           )
      CALL ncio_read_vector (file_restart, 'onset_fdd_p            ', landpft, onset_fdd_p           )
      CALL ncio_read_vector (file_restart, 'onset_swi_p            ', landpft, onset_swi_p           )
      CALL ncio_read_vector (file_restart, 'offset_fdd_p           ', landpft, offset_fdd_p          )
      CALL ncio_read_vector (file_restart, 'offset_swi_p           ', landpft, offset_swi_p          )
      CALL ncio_read_vector (file_restart, 'dormant_flag_p         ', landpft, dormant_flag_p        )
      CALL ncio_read_vector (file_restart, 'prev_leafc_to_litter_p ', landpft, prev_leafc_to_litter_p)
      CALL ncio_read_vector (file_restart, 'prev_frootc_to_litter_p', landpft, prev_frootc_to_litter_p)
      CALL ncio_read_vector (file_restart, 'days_active_p          ', landpft, days_active_p         )

      CALL ncio_read_vector (file_restart, 'burndate_p             ', landpft, burndate_p            )
      CALL ncio_read_vector (file_restart, 'grain_flag_p           ', landpft, grain_flag_p          )
      CALL ncio_read_vector (file_restart, 'ctrunc_p               ', landpft, ctrunc_p              )
      CALL ncio_read_vector (file_restart, 'ntrunc_p               ', landpft, ntrunc_p              )
      CALL ncio_read_vector (file_restart, 'npool_p                ', landpft, npool_p               )

#ifdef CROP
! crop variables
      CALL ncio_read_vector (file_restart, 'croplive_p             ', landpft, croplive_p            )
      CALL ncio_read_vector (file_restart, 'hui_p                  ', landpft, hui_p                 )
      CALL ncio_read_vector (file_restart, 'gddplant_p             ', landpft, gddplant_p            )
      CALL ncio_read_vector (file_restart, 'peaklai_p              ', landpft, peaklai_p             )
      CALL ncio_read_vector (file_restart, 'aroot_p                ', landpft, aroot_p               )
      CALL ncio_read_vector (file_restart, 'astem_p                ', landpft, astem_p               )
      CALL ncio_read_vector (file_restart, 'arepr_p                ', landpft, arepr_p               )
      CALL ncio_read_vector (file_restart, 'aleaf_p                ', landpft, aleaf_p               )
      CALL ncio_read_vector (file_restart, 'astemi_p               ', landpft, astemi_p              )
      CALL ncio_read_vector (file_restart, 'aleafi_p               ', landpft, aleafi_p              )
      CALL ncio_read_vector (file_restart, 'gddmaturity_p          ', landpft, gddmaturity_p         )

      CALL ncio_read_vector (file_restart, 'cropplant_p            ', landpft, cropplant_p           )
      CALL ncio_read_vector (file_restart, 'idop_p                 ', landpft, idop_p                )
      CALL ncio_read_vector (file_restart, 'a5tmin_p               ', landpft, a5tmin_p              )
      CALL ncio_read_vector (file_restart, 'a10tmin_p              ', landpft, a10tmin_p             )
      CALL ncio_read_vector (file_restart, 't10_p                  ', landpft, t10_p                 )
      CALL ncio_read_vector (file_restart, 'cumvd_p                ', landpft, cumvd_p               )
      CALL ncio_read_vector (file_restart, 'vf_p                   ', landpft, vf_p                  )
      CALL ncio_read_vector (file_restart, 'cphase_p               ', landpft, cphase_p              )
      CALL ncio_read_vector (file_restart, 'fert_counter_p         ', landpft, fert_counter_p        )
      CALL ncio_read_vector (file_restart, 'tref_min_p             ', landpft, tref_min_p            )
      CALL ncio_read_vector (file_restart, 'tref_max_p             ', landpft, tref_max_p            )
      CALL ncio_read_vector (file_restart, 'tref_min_inst_p        ', landpft, tref_min_inst_p       )
      CALL ncio_read_vector (file_restart, 'tref_max_inst_p        ', landpft, tref_max_inst_p       )
      CALL ncio_read_vector (file_restart, 'fertnitro_p            ', landpft, fertnitro_p           )
      CALL ncio_read_vector (file_restart, 'manunitro_p            ', landpft, manunitro_p           )
      CALL ncio_read_vector (file_restart, 'fert_p                 ', landpft, fert_p                )
      CALL ncio_read_vector (file_restart, 'latbaset_p             ', landpft, latbaset_p            )
      CALL ncio_read_vector (file_restart, 'plantdate_p            ', landpft, plantdate_p           )
#endif

      IF(DEF_USE_DiagMatrix)THEN
! SASU variables
         CALL ncio_read_vector (file_restart, 'leafcCap_p             ', landpft, leafcCap_p             , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'leafc_storageCap_p     ', landpft, leafc_storageCap_p     , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'leafc_xferCap_p        ', landpft, leafc_xferCap_p        , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'frootcCap_p            ', landpft, frootcCap_p            , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'frootc_storageCap_p    ', landpft, frootc_storageCap_p    , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'frootc_xferCap_p       ', landpft, frootc_xferCap_p       , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'livestemcCap_p         ', landpft, livestemcCap_p         , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'livestemc_storageCap_p ', landpft, livestemc_storageCap_p , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'livestemc_xferCap_p    ', landpft, livestemc_xferCap_p    , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'deadstemcCap_p         ', landpft, deadstemcCap_p         , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'deadstemc_storageCap_p ', landpft, deadstemc_storageCap_p , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'deadstemc_xferCap_p    ', landpft, deadstemc_xferCap_p    , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'livecrootcCap_p        ', landpft, livecrootcCap_p        , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'livecrootc_storageCap_p', landpft, livecrootc_storageCap_p, defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'livecrootc_xferCap_p   ', landpft, livecrootc_xferCap_p   , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'deadcrootcCap_p        ', landpft, deadcrootcCap_p        , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'deadcrootc_storageCap_p', landpft, deadcrootc_storageCap_p, defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'deadcrootc_xferCap_p   ', landpft, deadcrootc_xferCap_p   , defval = 1._r8)

         CALL ncio_read_vector (file_restart, 'leafnCap_p             ', landpft, leafnCap_p             , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'leafn_storageCap_p     ', landpft, leafn_storageCap_p     , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'leafn_xferCap_p        ', landpft, leafn_xferCap_p        , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'frootnCap_p            ', landpft, frootnCap_p            , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'frootn_storageCap_p    ', landpft, frootn_storageCap_p    , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'frootn_xferCap_p       ', landpft, frootn_xferCap_p       , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'livestemnCap_p         ', landpft, livestemnCap_p         , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'livestemn_storageCap_p ', landpft, livestemn_storageCap_p , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'livestemn_xferCap_p    ', landpft, livestemn_xferCap_p    , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'deadstemnCap_p         ', landpft, deadstemnCap_p         , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'deadstemn_storageCap_p ', landpft, deadstemn_storageCap_p , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'deadstemn_xferCap_p    ', landpft, deadstemn_xferCap_p    , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'livecrootnCap_p        ', landpft, livecrootnCap_p        , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'livecrootn_storageCap_p', landpft, livecrootn_storageCap_p, defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'livecrootn_xferCap_p   ', landpft, livecrootn_xferCap_p   , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'deadcrootnCap_p        ', landpft, deadcrootnCap_p        , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'deadcrootn_storageCap_p', landpft, deadcrootn_storageCap_p, defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'deadcrootn_xferCap_p   ', landpft, deadcrootn_xferCap_p   , defval = 1._r8)
      ENDIF

      IF(DEF_USE_SASU .or. DEF_USE_DiagMatrix)THEN
         CALL ncio_read_vector (file_restart, 'leafc0_p               ', landpft, leafc0_p              , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'leafc0_storage_p       ', landpft, leafc0_storage_p      , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'leafc0_xfer_p          ', landpft, leafc0_xfer_p         , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'frootc0_p              ', landpft, frootc0_p             , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'frootc0_storage_p      ', landpft, frootc0_storage_p     , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'frootc0_xfer_p         ', landpft, frootc0_xfer_p        , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'livestemc0_p           ', landpft, livestemc0_p          , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'livestemc0_storage_p   ', landpft, livestemc0_storage_p  , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'livestemc0_xfer_p      ', landpft, livestemc0_xfer_p     , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'deadstemc0_p           ', landpft, deadstemc0_p          , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'deadstemc0_storage_p   ', landpft, deadstemc0_storage_p  , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'deadstemc0_xfer_p      ', landpft, deadstemc0_xfer_p     , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'livecrootc0_p          ', landpft, livecrootc0_p         , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'livecrootc0_storage_p  ', landpft, livecrootc0_storage_p , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'livecrootc0_xfer_p     ', landpft, livecrootc0_xfer_p    , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'deadcrootc0_p          ', landpft, deadcrootc0_p         , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'deadcrootc0_storage_p  ', landpft, deadcrootc0_storage_p , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'deadcrootc0_xfer_p     ', landpft, deadcrootc0_xfer_p    , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'grainc0_p              ', landpft, grainc0_p             , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'grainc0_storage_p      ', landpft, grainc0_storage_p     , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'grainc0_xfer_p         ', landpft, grainc0_xfer_p        , defval = 1._r8)

         CALL ncio_read_vector (file_restart, 'leafn0_p               ', landpft, leafn0_p              , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'leafn0_storage_p       ', landpft, leafn0_storage_p      , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'leafn0_xfer_p          ', landpft, leafn0_xfer_p         , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'frootn0_p              ', landpft, frootn0_p             , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'frootn0_storage_p      ', landpft, frootn0_storage_p     , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'frootn0_xfer_p         ', landpft, frootn0_xfer_p        , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'livestemn0_p           ', landpft, livestemn0_p          , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'livestemn0_storage_p   ', landpft, livestemn0_storage_p  , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'livestemn0_xfer_p      ', landpft, livestemn0_xfer_p     , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'deadstemn0_p           ', landpft, deadstemn0_p          , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'deadstemn0_storage_p   ', landpft, deadstemn0_storage_p  , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'deadstemn0_xfer_p      ', landpft, deadstemn0_xfer_p     , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'livecrootn0_p          ', landpft, livecrootn0_p         , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'livecrootn0_storage_p  ', landpft, livecrootn0_storage_p , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'livecrootn0_xfer_p     ', landpft, livecrootn0_xfer_p    , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'deadcrootn0_p          ', landpft, deadcrootn0_p         , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'deadcrootn0_storage_p  ', landpft, deadcrootn0_storage_p , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'deadcrootn0_xfer_p     ', landpft, deadcrootn0_xfer_p    , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'grainn0_p              ', landpft, grainn0_p             , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'grainn0_storage_p      ', landpft, grainn0_storage_p     , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'grainn0_xfer_p         ', landpft, grainn0_xfer_p        , defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'retransn0_p            ', landpft, retransn0_p           , defval = 1._r8)

         CALL ncio_read_vector (file_restart, 'I_leafc_p_acc          ', landpft, I_leafc_p_acc         , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'I_leafc_st_p_acc       ', landpft, I_leafc_st_p_acc      , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'I_frootc_p_acc         ', landpft, I_frootc_p_acc        , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'I_frootc_st_p_acc      ', landpft, I_frootc_st_p_acc     , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'I_livestemc_p_acc      ', landpft, I_livestemc_p_acc     , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'I_livestemc_st_p_acc   ', landpft, I_livestemc_st_p_acc  , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'I_deadstemc_p_acc      ', landpft, I_deadstemc_p_acc     , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'I_deadstemc_st_p_acc   ', landpft, I_deadstemc_st_p_acc  , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'I_livecrootc_p_acc     ', landpft, I_livecrootc_p_acc    , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'I_livecrootc_st_p_acc  ', landpft, I_livecrootc_st_p_acc , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'I_deadcrootc_p_acc     ', landpft, I_deadcrootc_p_acc    , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'I_deadcrootc_st_p_acc  ', landpft, I_deadcrootc_st_p_acc , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'I_grainc_p_acc         ', landpft, I_grainc_p_acc        , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'I_grainc_st_p_acc      ', landpft, I_grainc_st_p_acc     , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'I_leafn_p_acc          ', landpft, I_leafn_p_acc         , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'I_leafn_st_p_acc       ', landpft, I_leafn_st_p_acc      , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'I_frootn_p_acc         ', landpft, I_frootn_p_acc        , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'I_frootn_st_p_acc      ', landpft, I_frootn_st_p_acc     , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'I_livestemn_p_acc      ', landpft, I_livestemn_p_acc     , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'I_livestemn_st_p_acc   ', landpft, I_livestemn_st_p_acc  , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'I_deadstemn_p_acc      ', landpft, I_deadstemn_p_acc     , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'I_deadstemn_st_p_acc   ', landpft, I_deadstemn_st_p_acc  , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'I_livecrootn_p_acc     ', landpft, I_livecrootn_p_acc    , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'I_livecrootn_st_p_acc  ', landpft, I_livecrootn_st_p_acc , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'I_deadcrootn_p_acc     ', landpft, I_deadcrootn_p_acc    , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'I_deadcrootn_st_p_acc  ', landpft, I_deadcrootn_st_p_acc , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'I_grainn_p_acc         ', landpft, I_grainn_p_acc        , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'I_grainn_st_p_acc      ', landpft, I_grainn_st_p_acc     , defval = 0._r8)

         CALL ncio_read_vector (file_restart, 'AKX_leafc_xf_to_leafc_p_acc               ', landpft, &
         AKX_leafc_xf_to_leafc_p_acc              , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_frootc_xf_to_frootc_p_acc             ', landpft, &
         AKX_frootc_xf_to_frootc_p_acc            , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_livestemc_xf_to_livestemc_p_acc       ', landpft, &
         AKX_livestemc_xf_to_livestemc_p_acc      , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_deadstemc_xf_to_deadstemc_p_acc       ', landpft, &
         AKX_deadstemc_xf_to_deadstemc_p_acc      , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_livecrootc_xf_to_livecrootc_p_acc     ', landpft, &
         AKX_livecrootc_xf_to_livecrootc_p_acc    , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_deadcrootc_xf_to_deadcrootc_p_acc     ', landpft, &
         AKX_deadcrootc_xf_to_deadcrootc_p_acc    , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_grainc_xf_to_grainc_p_acc             ', landpft, &
         AKX_grainc_xf_to_grainc_p_acc            , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_livestemc_to_deadstemc_p_acc          ', landpft, &
         AKX_livestemc_to_deadstemc_p_acc         , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_livecrootc_to_deadcrootc_p_acc        ', landpft, &
         AKX_livecrootc_to_deadcrootc_p_acc       , defval = 0._r8)


         CALL ncio_read_vector (file_restart, 'AKX_leafc_st_to_leafc_xf_p_acc            ', landpft, &
         AKX_leafc_st_to_leafc_xf_p_acc           , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_frootc_st_to_frootc_xf_p_acc          ', landpft, &
         AKX_frootc_st_to_frootc_xf_p_acc         , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_livestemc_st_to_livestemc_xf_p_acc    ', landpft, &
         AKX_livestemc_st_to_livestemc_xf_p_acc   , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_deadstemc_st_to_deadstemc_xf_p_acc    ', landpft, &
         AKX_deadstemc_st_to_deadstemc_xf_p_acc   , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_livecrootc_st_to_livecrootc_xf_p_acc  ', landpft, &
         AKX_livecrootc_st_to_livecrootc_xf_p_acc , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_deadcrootc_st_to_deadcrootc_xf_p_acc  ', landpft, &
         AKX_deadcrootc_st_to_deadcrootc_xf_p_acc , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_grainc_st_to_grainc_xf_p_acc          ', landpft, &
         AKX_grainc_st_to_grainc_xf_p_acc         , defval = 0._r8)

         CALL ncio_read_vector (file_restart, 'AKX_leafc_exit_p_acc                      ', landpft, &
         AKX_leafc_exit_p_acc                     , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_frootc_exit_p_acc                     ', landpft, &
         AKX_frootc_exit_p_acc                    , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_livestemc_exit_p_acc                  ', landpft, &
         AKX_livestemc_exit_p_acc                 , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_deadstemc_exit_p_acc                  ', landpft, &
         AKX_deadstemc_exit_p_acc                 , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_livecrootc_exit_p_acc                 ', landpft, &
         AKX_livecrootc_exit_p_acc                , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_deadcrootc_exit_p_acc                 ', landpft, &
         AKX_deadcrootc_exit_p_acc                , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_grainc_exit_p_acc                     ', landpft, &
         AKX_grainc_exit_p_acc                    , defval = 0._r8)

         CALL ncio_read_vector (file_restart, 'AKX_leafc_st_exit_p_acc                   ', landpft, &
         AKX_leafc_st_exit_p_acc                  , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_frootc_st_exit_p_acc                  ', landpft, &
         AKX_frootc_st_exit_p_acc                 , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_livestemc_st_exit_p_acc               ', landpft, &
         AKX_livestemc_st_exit_p_acc              , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_deadstemc_st_exit_p_acc               ', landpft, &
         AKX_deadstemc_st_exit_p_acc              , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_livecrootc_st_exit_p_acc              ', landpft, &
         AKX_livecrootc_st_exit_p_acc             , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_deadcrootc_st_exit_p_acc              ', landpft, &
         AKX_deadcrootc_st_exit_p_acc             , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_grainc_st_exit_p_acc                  ', landpft, &
         AKX_grainc_st_exit_p_acc                 , defval = 0._r8)

         CALL ncio_read_vector (file_restart, 'AKX_leafc_xf_exit_p_acc                   ', landpft, &
         AKX_leafc_xf_exit_p_acc                  , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_frootc_xf_exit_p_acc                  ', landpft, &
         AKX_frootc_xf_exit_p_acc                 , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_livestemc_xf_exit_p_acc               ', landpft, &
         AKX_livestemc_xf_exit_p_acc              , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_deadstemc_xf_exit_p_acc               ', landpft, &
         AKX_deadstemc_xf_exit_p_acc              , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_livecrootc_xf_exit_p_acc              ', landpft, &
         AKX_livecrootc_xf_exit_p_acc             , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_deadcrootc_xf_exit_p_acc              ', landpft, &
         AKX_deadcrootc_xf_exit_p_acc             , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_grainc_xf_exit_p_acc                  ', landpft, &
         AKX_grainc_xf_exit_p_acc                 , defval = 0._r8)

         CALL ncio_read_vector (file_restart, 'AKX_leafn_xf_to_leafn_p_acc               ', landpft, &
         AKX_leafn_xf_to_leafn_p_acc              , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_frootn_xf_to_frootn_p_acc             ', landpft, &
         AKX_frootn_xf_to_frootn_p_acc            , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_livestemn_xf_to_livestemn_p_acc       ', landpft, &
         AKX_livestemn_xf_to_livestemn_p_acc      , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_deadstemn_xf_to_deadstemn_p_acc       ', landpft, &
         AKX_deadstemn_xf_to_deadstemn_p_acc      , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_livecrootn_xf_to_livecrootn_p_acc     ', landpft, &
         AKX_livecrootn_xf_to_livecrootn_p_acc    , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_deadcrootn_xf_to_deadcrootn_p_acc     ', landpft, &
         AKX_deadcrootn_xf_to_deadcrootn_p_acc    , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_grainn_xf_to_grainn_p_acc             ', landpft, &
         AKX_grainn_xf_to_grainn_p_acc            , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_livestemn_to_deadstemn_p_acc          ', landpft, &
         AKX_livestemn_to_deadstemn_p_acc         , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_livecrootn_to_deadcrootn_p_acc        ', landpft, &
         AKX_livecrootn_to_deadcrootn_p_acc       , defval = 0._r8)

         CALL ncio_read_vector (file_restart, 'AKX_leafn_st_to_leafn_xf_p_acc            ', landpft, &
         AKX_leafn_st_to_leafn_xf_p_acc           , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_frootn_st_to_frootn_xf_p_acc          ', landpft, &
         AKX_frootn_st_to_frootn_xf_p_acc         , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_livestemn_st_to_livestemn_xf_p_acc    ', landpft, &
         AKX_livestemn_st_to_livestemn_xf_p_acc   , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_deadstemn_st_to_deadstemn_xf_p_acc    ', landpft, &
         AKX_deadstemn_st_to_deadstemn_xf_p_acc   , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_livecrootn_st_to_livecrootn_xf_p_acc  ', landpft, &
         AKX_livecrootn_st_to_livecrootn_xf_p_acc , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_deadcrootn_st_to_deadcrootn_xf_p_acc  ', landpft, &
         AKX_deadcrootn_st_to_deadcrootn_xf_p_acc , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_grainn_st_to_grainn_xf_p_acc          ', landpft, &
         AKX_grainn_st_to_grainn_xf_p_acc         , defval = 0._r8)

         CALL ncio_read_vector (file_restart, 'AKX_leafn_to_retransn_p_acc               ', landpft, &
         AKX_leafn_to_retransn_p_acc              , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_frootn_to_retransn_p_acc              ', landpft, &
         AKX_frootn_to_retransn_p_acc             , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_livestemn_to_retransn_p_acc           ', landpft, &
         AKX_livestemn_to_retransn_p_acc          , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_livecrootn_to_retransn_p_acc          ', landpft, &
         AKX_livecrootn_to_retransn_p_acc         , defval = 0._r8)

         CALL ncio_read_vector (file_restart, 'AKX_retransn_to_leafn_p_acc               ', landpft, &
         AKX_retransn_to_leafn_p_acc              , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_retransn_to_frootn_p_acc              ', landpft, &
         AKX_retransn_to_frootn_p_acc             , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_retransn_to_livestemn_p_acc           ', landpft, &
         AKX_retransn_to_livestemn_p_acc          , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_retransn_to_deadstemn_p_acc           ', landpft, &
         AKX_retransn_to_deadstemn_p_acc          , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_retransn_to_livecrootn_p_acc          ', landpft, &
         AKX_retransn_to_livecrootn_p_acc         , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_retransn_to_deadcrootn_p_acc          ', landpft, &
         AKX_retransn_to_deadcrootn_p_acc         , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_retransn_to_grainn_p_acc              ', landpft, &
         AKX_retransn_to_grainn_p_acc             , defval = 0._r8)

         CALL ncio_read_vector (file_restart, 'AKX_retransn_to_leafn_st_p_acc            ', landpft, &
         AKX_retransn_to_leafn_st_p_acc           , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_retransn_to_frootn_st_p_acc           ', landpft, &
         AKX_retransn_to_frootn_st_p_acc          , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_retransn_to_livestemn_st_p_acc        ', landpft, &
         AKX_retransn_to_livestemn_st_p_acc       , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_retransn_to_deadstemn_st_p_acc        ', landpft, &
         AKX_retransn_to_deadstemn_st_p_acc       , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_retransn_to_livecrootn_st_p_acc       ', landpft, &
         AKX_retransn_to_livecrootn_st_p_acc      , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_retransn_to_deadcrootn_st_p_acc       ', landpft, &
         AKX_retransn_to_deadcrootn_st_p_acc      , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_retransn_to_grainn_st_p_acc           ', landpft, &
         AKX_retransn_to_grainn_st_p_acc          , defval = 0._r8)

         CALL ncio_read_vector (file_restart, 'AKX_leafn_exit_p_acc                      ', landpft, &
         AKX_leafn_exit_p_acc                     , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_frootn_exit_p_acc                     ', landpft, &
         AKX_frootn_exit_p_acc                    , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_livestemn_exit_p_acc                  ', landpft, &
         AKX_livestemn_exit_p_acc                 , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_deadstemn_exit_p_acc                  ', landpft, &
         AKX_deadstemn_exit_p_acc                 , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_livecrootn_exit_p_acc                 ', landpft, &
         AKX_livecrootn_exit_p_acc                , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_deadcrootn_exit_p_acc                 ', landpft, &
         AKX_deadcrootn_exit_p_acc                , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_grainn_exit_p_acc                     ', landpft, &
         AKX_grainn_exit_p_acc                    , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_retransn_exit_p_acc                   ', landpft, &
         AKX_retransn_exit_p_acc                  , defval = 0._r8)

         CALL ncio_read_vector (file_restart, 'AKX_leafn_st_exit_p_acc                   ', landpft, &
         AKX_leafn_st_exit_p_acc                  , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_frootn_st_exit_p_acc                  ', landpft, &
         AKX_frootn_st_exit_p_acc                 , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_livestemn_st_exit_p_acc               ', landpft, &
         AKX_livestemn_st_exit_p_acc              , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_deadstemn_st_exit_p_acc               ', landpft, &
         AKX_deadstemn_st_exit_p_acc              , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_livecrootn_st_exit_p_acc              ', landpft, &
         AKX_livecrootn_st_exit_p_acc             , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_deadcrootn_st_exit_p_acc              ', landpft, &
         AKX_deadcrootn_st_exit_p_acc             , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_grainn_st_exit_p_acc                  ', landpft, &
         AKX_grainn_st_exit_p_acc                 , defval = 0._r8)

         CALL ncio_read_vector (file_restart, 'AKX_leafn_xf_exit_p_acc                   ', landpft, &
         AKX_leafn_xf_exit_p_acc                  , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_frootn_xf_exit_p_acc                  ', landpft, &
         AKX_frootn_xf_exit_p_acc                 , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_livestemn_xf_exit_p_acc               ', landpft, &
         AKX_livestemn_xf_exit_p_acc              , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_deadstemn_xf_exit_p_acc               ', landpft, &
         AKX_deadstemn_xf_exit_p_acc              , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_livecrootn_xf_exit_p_acc              ', landpft, &
         AKX_livecrootn_xf_exit_p_acc             , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_deadcrootn_xf_exit_p_acc              ', landpft, &
         AKX_deadcrootn_xf_exit_p_acc             , defval = 0._r8)
         CALL ncio_read_vector (file_restart, 'AKX_grainn_xf_exit_p_acc                  ', landpft, &
         AKX_grainn_xf_exit_p_acc                 , defval = 0._r8)
      ENDIF
   END SUBROUTINE READ_BGCPFTimeVariables

   SUBROUTINE WRITE_BGCPFTimeVariables (file_restart)

   USE MOD_Namelist, only: DEF_REST_CompressLevel
   USE MOD_LandPFT
   USE MOD_NetCDFVector
   USE MOD_Vars_Global
   IMPLICIT NONE

   character(len=*), intent(in) :: file_restart

   ! Local variables
   integer :: compress

      compress = DEF_REST_CompressLevel

 ! bgc variables
      CALL ncio_write_vector (file_restart, 'leafc_p                ', 'pft', landpft, &
      leafc_p               , compress)
      CALL ncio_write_vector (file_restart, 'leafc_storage_p        ', 'pft', landpft, &
      leafc_storage_p       , compress)
      CALL ncio_write_vector (file_restart, 'leafc_xfer_p           ', 'pft', landpft, &
      leafc_xfer_p          , compress)
      CALL ncio_write_vector (file_restart, 'frootc_p               ', 'pft', landpft, &
      frootc_p              , compress)
      CALL ncio_write_vector (file_restart, 'frootc_storage_p       ', 'pft', landpft, &
      frootc_storage_p      , compress)
      CALL ncio_write_vector (file_restart, 'frootc_xfer_p          ', 'pft', landpft, &
      frootc_xfer_p         , compress)
      CALL ncio_write_vector (file_restart, 'livestemc_p            ', 'pft', landpft, &
      livestemc_p           , compress)
      CALL ncio_write_vector (file_restart, 'livestemc_storage_p    ', 'pft', landpft, &
      livestemc_storage_p   , compress)
      CALL ncio_write_vector (file_restart, 'livestemc_xfer_p       ', 'pft', landpft, &
      livestemc_xfer_p      , compress)
      CALL ncio_write_vector (file_restart, 'deadstemc_p            ', 'pft', landpft, &
      deadstemc_p           , compress)
      CALL ncio_write_vector (file_restart, 'deadstemc_storage_p    ', 'pft', landpft, &
      deadstemc_storage_p   , compress)
      CALL ncio_write_vector (file_restart, 'deadstemc_xfer_p       ', 'pft', landpft, &
      deadstemc_xfer_p      , compress)
      CALL ncio_write_vector (file_restart, 'livecrootc_p           ', 'pft', landpft, &
      livecrootc_p          , compress)
      CALL ncio_write_vector (file_restart, 'livecrootc_storage_p   ', 'pft', landpft, &
      livecrootc_storage_p  , compress)
      CALL ncio_write_vector (file_restart, 'livecrootc_xfer_p      ', 'pft', landpft, &
      livecrootc_xfer_p     , compress)
      CALL ncio_write_vector (file_restart, 'deadcrootc_p           ', 'pft', landpft, &
      deadcrootc_p          , compress)
      CALL ncio_write_vector (file_restart, 'deadcrootc_storage_p   ', 'pft', landpft, &
      deadcrootc_storage_p  , compress)
      CALL ncio_write_vector (file_restart, 'deadcrootc_xfer_p      ', 'pft', landpft, &
      deadcrootc_xfer_p     , compress)
      CALL ncio_write_vector (file_restart, 'grainc_p               ', 'pft', landpft, &
      grainc_p              , compress)
      CALL ncio_write_vector (file_restart, 'grainc_storage_p       ', 'pft', landpft, &
      grainc_storage_p      , compress)
      CALL ncio_write_vector (file_restart, 'grainc_xfer_p          ', 'pft', landpft, &
      grainc_xfer_p         , compress)
      CALL ncio_write_vector (file_restart, 'cropseedc_deficit_p    ', 'pft', landpft, &
      cropseedc_deficit_p   , compress)
      CALL ncio_write_vector (file_restart, 'xsmrpool_p             ', 'pft', landpft, &
      xsmrpool_p            , compress)
      CALL ncio_write_vector (file_restart, 'gresp_storage_p        ', 'pft', landpft, &
      gresp_storage_p       , compress)
      CALL ncio_write_vector (file_restart, 'gresp_xfer_p           ', 'pft', landpft, &
      gresp_xfer_p          , compress)
      CALL ncio_write_vector (file_restart, 'cpool_p                ', 'pft', landpft, &
      cpool_p               , compress)
 !     CALL ncio_write_vector (file_restart, 'totvegc_p              ', 'pft', landpft, &
 !     totvegc_p             , compress)
      CALL ncio_write_vector (file_restart, 'cropprod1c_p           ', 'pft', landpft, &
      cropprod1c_p          , compress)

      CALL ncio_write_vector (file_restart, 'leafn_p                ', 'pft', landpft, &
      leafn_p               , compress)
      CALL ncio_write_vector (file_restart, 'leafn_storage_p        ', 'pft', landpft, &
      leafn_storage_p       , compress)
      CALL ncio_write_vector (file_restart, 'leafn_xfer_p           ', 'pft', landpft, &
      leafn_xfer_p          , compress)
      CALL ncio_write_vector (file_restart, 'frootn_p               ', 'pft', landpft, &
      frootn_p              , compress)
      CALL ncio_write_vector (file_restart, 'frootn_storage_p       ', 'pft', landpft, &
      frootn_storage_p      , compress)
      CALL ncio_write_vector (file_restart, 'frootn_xfer_p          ', 'pft', landpft, &
      frootn_xfer_p         , compress)
      CALL ncio_write_vector (file_restart, 'livestemn_p            ', 'pft', landpft, &
      livestemn_p           , compress)
      CALL ncio_write_vector (file_restart, 'livestemn_storage_p    ', 'pft', landpft, &
      livestemn_storage_p   , compress)
      CALL ncio_write_vector (file_restart, 'livestemn_xfer_p       ', 'pft', landpft, &
      livestemn_xfer_p      , compress)
      CALL ncio_write_vector (file_restart, 'deadstemn_p            ', 'pft', landpft, &
      deadstemn_p           , compress)
      CALL ncio_write_vector (file_restart, 'deadstemn_storage_p    ', 'pft', landpft, &
      deadstemn_storage_p   , compress)
      CALL ncio_write_vector (file_restart, 'deadstemn_xfer_p       ', 'pft', landpft, &
      deadstemn_xfer_p      , compress)
      CALL ncio_write_vector (file_restart, 'livecrootn_p           ', 'pft', landpft, &
      livecrootn_p          , compress)
      CALL ncio_write_vector (file_restart, 'livecrootn_storage_p   ', 'pft', landpft, &
      livecrootn_storage_p  , compress)
      CALL ncio_write_vector (file_restart, 'livecrootn_xfer_p      ', 'pft', landpft, &
      livecrootn_xfer_p     , compress)
      CALL ncio_write_vector (file_restart, 'deadcrootn_p           ', 'pft', landpft, &
      deadcrootn_p          , compress)
      CALL ncio_write_vector (file_restart, 'deadcrootn_storage_p   ', 'pft', landpft, &
      deadcrootn_storage_p  , compress)
      CALL ncio_write_vector (file_restart, 'deadcrootn_xfer_p      ', 'pft', landpft, &
      deadcrootn_xfer_p     , compress)
      CALL ncio_write_vector (file_restart, 'grainn_p               ', 'pft', landpft, &
      grainn_p              , compress)
      CALL ncio_write_vector (file_restart, 'grainn_storage_p       ', 'pft', landpft, &
      grainn_storage_p      , compress)
      CALL ncio_write_vector (file_restart, 'grainn_xfer_p          ', 'pft', landpft, &
      grainn_xfer_p         , compress)
      CALL ncio_write_vector (file_restart, 'cropseedn_deficit_p    ', 'pft', landpft, &
      cropseedn_deficit_p   , compress)
      CALL ncio_write_vector (file_restart, 'retransn_p             ', 'pft', landpft, &
      retransn_p            , compress)
 !     CALL ncio_write_vector (file_restart, 'totvegn_p              ', 'pft', landpft, &
 !     totvegn_p             , compress)

      CALL ncio_write_vector (file_restart, 'harvdate_p             ', 'pft', landpft, &
      harvdate_p            , compress)

      CALL ncio_write_vector (file_restart, 'tempsum_potential_gpp_p', 'pft', landpft, &
      tempsum_potential_gpp_p, compress)
      CALL ncio_write_vector (file_restart, 'tempmax_retransn_p     ', 'pft', landpft, &
      tempmax_retransn_p    , compress)
      CALL ncio_write_vector (file_restart, 'tempavg_tref_p         ', 'pft', landpft, &
      tempavg_tref_p        , compress)
      CALL ncio_write_vector (file_restart, 'tempsum_npp_p          ', 'pft', landpft, &
      tempsum_npp_p         , compress)
      CALL ncio_write_vector (file_restart, 'tempsum_litfall_p      ', 'pft', landpft, &
      tempsum_litfall_p     , compress)
      CALL ncio_write_vector (file_restart, 'annsum_potential_gpp_p ', 'pft', landpft, &
      annsum_potential_gpp_p, compress)
      CALL ncio_write_vector (file_restart, 'annmax_retransn_p      ', 'pft', landpft, &
      annmax_retransn_p     , compress)
      CALL ncio_write_vector (file_restart, 'annavg_tref_p          ', 'pft', landpft, &
      annavg_tref_p         , compress)
      CALL ncio_write_vector (file_restart, 'annsum_npp_p           ', 'pft', landpft, &
      annsum_npp_p          , compress)
      CALL ncio_write_vector (file_restart, 'annsum_litfall_p       ', 'pft', landpft, &
      annsum_litfall_p      , compress)

      CALL ncio_write_vector (file_restart, 'bglfr_p                ', 'pft', landpft, &
      bglfr_p               , compress)
      CALL ncio_write_vector (file_restart, 'bgtr_p                 ', 'pft', landpft, &
      bgtr_p                , compress)
      CALL ncio_write_vector (file_restart, 'lgsf_p                 ', 'pft', landpft, &
      lgsf_p                , compress)
      CALL ncio_write_vector (file_restart, 'gdd0_p                 ', 'pft', landpft, &
      gdd0_p                , compress)
      CALL ncio_write_vector (file_restart, 'gdd8_p                 ', 'pft', landpft, &
      gdd8_p                , compress)
      CALL ncio_write_vector (file_restart, 'gdd10_p                ', 'pft', landpft, &
      gdd10_p               , compress)
      CALL ncio_write_vector (file_restart, 'gdd020_p               ', 'pft', landpft, &
      gdd020_p              , compress)
      CALL ncio_write_vector (file_restart, 'gdd820_p               ', 'pft', landpft, &
      gdd820_p              , compress)
      CALL ncio_write_vector (file_restart, 'gdd1020_p              ', 'pft', landpft, &
      gdd1020_p             , compress)
      CALL ncio_write_vector (file_restart, 'nyrs_crop_active_p     ', 'pft', landpft, &
      nyrs_crop_active_p    , compress)

      CALL ncio_write_vector (file_restart, 'offset_flag_p          ', 'pft', landpft, &
      offset_flag_p         , compress)
      CALL ncio_write_vector (file_restart, 'offset_counter_p       ', 'pft', landpft, &
      offset_counter_p      , compress)
      CALL ncio_write_vector (file_restart, 'onset_flag_p           ', 'pft', landpft, &
      onset_flag_p          , compress)
      CALL ncio_write_vector (file_restart, 'onset_counter_p        ', 'pft', landpft, &
      onset_counter_p       , compress)
      CALL ncio_write_vector (file_restart, 'onset_gddflag_p        ', 'pft', landpft, &
      onset_gddflag_p       , compress)
      CALL ncio_write_vector (file_restart, 'onset_gdd_p            ', 'pft', landpft, &
      onset_gdd_p           , compress)
      CALL ncio_write_vector (file_restart, 'onset_fdd_p            ', 'pft', landpft, &
      onset_fdd_p           , compress)
      CALL ncio_write_vector (file_restart, 'onset_swi_p            ', 'pft', landpft, &
      onset_swi_p           , compress)
      CALL ncio_write_vector (file_restart, 'offset_fdd_p           ', 'pft', landpft, &
      offset_fdd_p          , compress)
      CALL ncio_write_vector (file_restart, 'offset_swi_p           ', 'pft', landpft, &
      offset_swi_p          , compress)
      CALL ncio_write_vector (file_restart, 'dormant_flag_p         ', 'pft', landpft, &
      dormant_flag_p        , compress)
      CALL ncio_write_vector (file_restart, 'prev_leafc_to_litter_p ', 'pft', landpft, &
      prev_leafc_to_litter_p, compress)
      CALL ncio_write_vector (file_restart, 'prev_frootc_to_litter_p', 'pft', landpft, &
      prev_frootc_to_litter_p, compress)
      CALL ncio_write_vector (file_restart, 'days_active_p          ', 'pft', landpft, &
      days_active_p         , compress)

      CALL ncio_write_vector (file_restart, 'burndate_p             ', 'pft', landpft, &
      burndate_p            , compress)
      CALL ncio_write_vector (file_restart, 'grain_flag_p           ', 'pft', landpft, &
      grain_flag_p          , compress)
      CALL ncio_write_vector (file_restart, 'ctrunc_p               ', 'pft', landpft, &
      ctrunc_p              , compress)
      CALL ncio_write_vector (file_restart, 'ntrunc_p               ', 'pft', landpft, &
      ntrunc_p              , compress)
      CALL ncio_write_vector (file_restart, 'npool_p                ', 'pft', landpft, &
      npool_p               , compress)

#ifdef CROP
! crop variables
      CALL ncio_write_vector (file_restart, 'croplive_p             ', 'pft', landpft, &
      croplive_p            , compress)
      CALL ncio_write_vector (file_restart, 'hui_p                   ', 'pft', landpft, &
      hui_p             , compress)
      CALL ncio_write_vector (file_restart, 'gddplant_p             ', 'pft', landpft, &
      gddplant_p            , compress)
      CALL ncio_write_vector (file_restart, 'peaklai_p              ', 'pft', landpft, &
      peaklai_p             , compress)
      CALL ncio_write_vector (file_restart, 'aroot_p                ', 'pft', landpft, &
      aroot_p               , compress)
      CALL ncio_write_vector (file_restart, 'astem_p                ', 'pft', landpft, &
      astem_p               , compress)
      CALL ncio_write_vector (file_restart, 'arepr_p                ', 'pft', landpft, &
      arepr_p               , compress)
      CALL ncio_write_vector (file_restart, 'aleaf_p                ', 'pft', landpft, &
      aleaf_p               , compress)
      CALL ncio_write_vector (file_restart, 'astemi_p               ', 'pft', landpft, &
      astemi_p              , compress)
      CALL ncio_write_vector (file_restart, 'aleafi_p               ', 'pft', landpft, &
      aleafi_p              , compress)
      CALL ncio_write_vector (file_restart, 'gddmaturity_p          ', 'pft', landpft, &
      gddmaturity_p         , compress)

      CALL ncio_write_vector (file_restart, 'cropplant_p            ', 'pft', landpft, &
      cropplant_p           , compress)
      CALL ncio_write_vector (file_restart, 'idop_p                 ', 'pft', landpft, &
      idop_p                , compress)
      CALL ncio_write_vector (file_restart, 'a5tmin_p               ', 'pft', landpft, &
      a5tmin_p              , compress)
      CALL ncio_write_vector (file_restart, 'a10tmin_p              ', 'pft', landpft, &
      a10tmin_p             , compress)
      CALL ncio_write_vector (file_restart, 't10_p                  ', 'pft', landpft, &
      t10_p                 , compress)
      CALL ncio_write_vector (file_restart, 'cumvd_p                ', 'pft', landpft, &
      cumvd_p               , compress)
      CALL ncio_write_vector (file_restart, 'vf_p                   ', 'pft', landpft, &
      vf_p                  , compress)
      CALL ncio_write_vector (file_restart, 'cphase_p               ', 'pft', landpft, &
      cphase_p              , compress)
      CALL ncio_write_vector (file_restart, 'fert_counter_p         ', 'pft', landpft, &
      fert_counter_p        , compress)
      CALL ncio_write_vector (file_restart, 'tref_min_p             ', 'pft', landpft, &
      tref_min_p            , compress)
      CALL ncio_write_vector (file_restart, 'tref_max_p             ', 'pft', landpft, &
      tref_max_p            , compress)
      CALL ncio_write_vector (file_restart, 'tref_min_inst_p        ', 'pft', landpft, &
      tref_min_inst_p       , compress)
      CALL ncio_write_vector (file_restart, 'tref_max_inst_p        ', 'pft', landpft, &
      tref_max_inst_p       , compress)
      CALL ncio_write_vector (file_restart, 'fertnitro_p            ', 'pft', landpft, &
      fertnitro_p           , compress)
      CALL ncio_write_vector (file_restart, 'manunitro_p            ', 'pft', landpft, &
      manunitro_p           , compress)
      CALL ncio_write_vector (file_restart, 'fert_p                 ', 'pft', landpft, &
      fert_p                , compress)
      CALL ncio_write_vector (file_restart, 'latbaset_p             ', 'pft', landpft, &
      latbaset_p            , compress)
      CALL ncio_write_vector (file_restart, 'plantdate_p            ', 'pft', landpft, &
      plantdate_p           , compress)
#endif

      IF(DEF_USE_DiagMatrix)THEN
! SASU variables
         CALL ncio_write_vector (file_restart, 'leafcCap_p                ', 'pft', landpft, &
         leafcCap_p               , compress)
         CALL ncio_write_vector (file_restart, 'leafc_storageCap_p        ', 'pft', landpft, &
         leafc_storageCap_p       , compress)
         CALL ncio_write_vector (file_restart, 'leafc_xferCap_p           ', 'pft', landpft, &
         leafc_xferCap_p          , compress)
         CALL ncio_write_vector (file_restart, 'frootcCap_p               ', 'pft', landpft, &
         frootcCap_p              , compress)
         CALL ncio_write_vector (file_restart, 'frootc_storageCap_p       ', 'pft', landpft, &
         frootc_storageCap_p      , compress)
         CALL ncio_write_vector (file_restart, 'frootc_xferCap_p          ', 'pft', landpft, &
         frootc_xferCap_p         , compress)
         CALL ncio_write_vector (file_restart, 'livestemcCap_p            ', 'pft', landpft, &
         livestemcCap_p           , compress)
         CALL ncio_write_vector (file_restart, 'livestemc_storageCap_p    ', 'pft', landpft, &
         livestemc_storageCap_p   , compress)
         CALL ncio_write_vector (file_restart, 'livestemc_xferCap_p       ', 'pft', landpft, &
         livestemc_xferCap_p      , compress)
         CALL ncio_write_vector (file_restart, 'deadstemcCap_p            ', 'pft', landpft, &
         deadstemcCap_p           , compress)
         CALL ncio_write_vector (file_restart, 'deadstemc_storageCap_p    ', 'pft', landpft, &
         deadstemc_storageCap_p   , compress)
         CALL ncio_write_vector (file_restart, 'deadstemc_xferCap_p       ', 'pft', landpft, &
         deadstemc_xferCap_p      , compress)
         CALL ncio_write_vector (file_restart, 'livecrootcCap_p           ', 'pft', landpft, &
         livecrootcCap_p          , compress)
         CALL ncio_write_vector (file_restart, 'livecrootc_storageCap_p   ', 'pft', landpft, &
         livecrootc_storageCap_p  , compress)
         CALL ncio_write_vector (file_restart, 'livecrootc_xferCap_p      ', 'pft', landpft, &
         livecrootc_xferCap_p     , compress)
         CALL ncio_write_vector (file_restart, 'deadcrootcCap_p           ', 'pft', landpft, &
         deadcrootcCap_p          , compress)
         CALL ncio_write_vector (file_restart, 'deadcrootc_storageCap_p   ', 'pft', landpft, &
         deadcrootc_storageCap_p  , compress)
         CALL ncio_write_vector (file_restart, 'deadcrootc_xferCap_p      ', 'pft', landpft, &
         deadcrootc_xferCap_p     , compress)

         CALL ncio_write_vector (file_restart, 'leafnCap_p                ', 'pft', landpft, &
         leafcCap_p               , compress)
         CALL ncio_write_vector (file_restart, 'leafn_storageCap_p        ', 'pft', landpft, &
         leafc_storageCap_p       , compress)
         CALL ncio_write_vector (file_restart, 'leafn_xferCap_p           ', 'pft', landpft, &
         leafc_xferCap_p          , compress)
         CALL ncio_write_vector (file_restart, 'frootnCap_p               ', 'pft', landpft, &
         frootcCap_p              , compress)
         CALL ncio_write_vector (file_restart, 'frootn_storageCap_p       ', 'pft', landpft, &
         frootc_storageCap_p      , compress)
         CALL ncio_write_vector (file_restart, 'frootn_xferCap_p          ', 'pft', landpft, &
         frootc_xferCap_p         , compress)
         CALL ncio_write_vector (file_restart, 'livestemnCap_p            ', 'pft', landpft, &
         livestemnCap_p           , compress)
         CALL ncio_write_vector (file_restart, 'livestemn_storageCap_p    ', 'pft', landpft, &
         livestemn_storageCap_p   , compress)
         CALL ncio_write_vector (file_restart, 'livestemn_xferCap_p       ', 'pft', landpft, &
         livestemn_xferCap_p      , compress)
         CALL ncio_write_vector (file_restart, 'deadstemnCap_p            ', 'pft', landpft, &
         deadstemnCap_p           , compress)
         CALL ncio_write_vector (file_restart, 'deadstemn_storageCap_p    ', 'pft', landpft, &
         deadstemn_storageCap_p   , compress)
         CALL ncio_write_vector (file_restart, 'deadstemn_xferCap_p       ', 'pft', landpft, &
         deadstemn_xferCap_p      , compress)
         CALL ncio_write_vector (file_restart, 'livecrootnCap_p           ', 'pft', landpft, &
         livecrootnCap_p          , compress)
         CALL ncio_write_vector (file_restart, 'livecrootn_storageCap_p   ', 'pft', landpft, &
         livecrootn_storageCap_p  , compress)
         CALL ncio_write_vector (file_restart, 'livecrootn_xferCap_p      ', 'pft', landpft, &
         livecrootn_xferCap_p     , compress)
         CALL ncio_write_vector (file_restart, 'deadcrootnCap_p           ', 'pft', landpft, &
         deadcrootnCap_p          , compress)
         CALL ncio_write_vector (file_restart, 'deadcrootn_storageCap_p   ', 'pft', landpft, &
         deadcrootn_storageCap_p  , compress)
         CALL ncio_write_vector (file_restart, 'deadcrootn_xferCap_p      ', 'pft', landpft, &
         deadcrootn_xferCap_p     , compress)
      ENDIF

      IF(DEF_USE_SASU .or. DEF_USE_DiagMatrix)THEN
         CALL ncio_write_vector (file_restart, 'leafc0_p               ', 'pft', landpft, &
         leafc0_p              , compress)
         CALL ncio_write_vector (file_restart, 'leafc0_storage_p       ', 'pft', landpft, &
         leafc0_storage_p      , compress)
         CALL ncio_write_vector (file_restart, 'leafc0_xfer_p          ', 'pft', landpft, &
         leafc0_xfer_p         , compress)
         CALL ncio_write_vector (file_restart, 'frootc0_p              ', 'pft', landpft, &
         frootc0_p             , compress)
         CALL ncio_write_vector (file_restart, 'frootc0_storage_p      ', 'pft', landpft, &
         frootc0_storage_p     , compress)
         CALL ncio_write_vector (file_restart, 'frootc0_xfer_p         ', 'pft', landpft, &
         frootc0_xfer_p        , compress)
         CALL ncio_write_vector (file_restart, 'livestemc0_p           ', 'pft', landpft, &
         livestemc0_p          , compress)
         CALL ncio_write_vector (file_restart, 'livestemc0_storage_p   ', 'pft', landpft, &
         livestemc0_storage_p  , compress)
         CALL ncio_write_vector (file_restart, 'livestemc0_xfer_p      ', 'pft', landpft, &
         livestemc0_xfer_p     , compress)
         CALL ncio_write_vector (file_restart, 'deadstemc0_p           ', 'pft', landpft, &
         deadstemc0_p          , compress)
         CALL ncio_write_vector (file_restart, 'deadstemc0_storage_p   ', 'pft', landpft, &
         deadstemc0_storage_p  , compress)
         CALL ncio_write_vector (file_restart, 'deadstemc0_xfer_p      ', 'pft', landpft, &
         deadstemc0_xfer_p     , compress)
         CALL ncio_write_vector (file_restart, 'livecrootc0_p          ', 'pft', landpft, &
         livecrootc0_p         , compress)
         CALL ncio_write_vector (file_restart, 'livecrootc0_storage_p  ', 'pft', landpft, &
         livecrootc0_storage_p , compress)
         CALL ncio_write_vector (file_restart, 'livecrootc0_xfer_p     ', 'pft', landpft, &
         livecrootc0_xfer_p    , compress)
         CALL ncio_write_vector (file_restart, 'deadcrootc0_p          ', 'pft', landpft, &
         deadcrootc0_p         , compress)
         CALL ncio_write_vector (file_restart, 'deadcrootc0_storage_p  ', 'pft', landpft, &
         deadcrootc0_storage_p , compress)
         CALL ncio_write_vector (file_restart, 'deadcrootc0_xfer_p     ', 'pft', landpft, &
         deadcrootc0_xfer_p    , compress)
         CALL ncio_write_vector (file_restart, 'grainc0_p              ', 'pft', landpft, &
         grainc0_p             , compress)
         CALL ncio_write_vector (file_restart, 'grainc0_storage_p      ', 'pft', landpft, &
         grainc0_storage_p     , compress)
         CALL ncio_write_vector (file_restart, 'grainc0_xfer_p         ', 'pft', landpft, &
         grainc0_xfer_p        , compress)

         CALL ncio_write_vector (file_restart, 'leafn0_p               ', 'pft', landpft, &
         leafn0_p              , compress)
         CALL ncio_write_vector (file_restart, 'leafn0_storage_p       ', 'pft', landpft, &
         leafn0_storage_p      , compress)
         CALL ncio_write_vector (file_restart, 'leafn0_xfer_p          ', 'pft', landpft, &
         leafn0_xfer_p         , compress)
         CALL ncio_write_vector (file_restart, 'frootn0_p              ', 'pft', landpft, &
         frootn0_p             , compress)
         CALL ncio_write_vector (file_restart, 'frootn0_storage_p      ', 'pft', landpft, &
         frootn0_storage_p     , compress)
         CALL ncio_write_vector (file_restart, 'frootn0_xfer_p         ', 'pft', landpft, &
         frootn0_xfer_p        , compress)
         CALL ncio_write_vector (file_restart, 'livestemn0_p           ', 'pft', landpft, &
         livestemn0_p          , compress)
         CALL ncio_write_vector (file_restart, 'livestemn0_storage_p   ', 'pft', landpft, &
         livestemn0_storage_p  , compress)
         CALL ncio_write_vector (file_restart, 'livestemn0_xfer_p      ', 'pft', landpft, &
         livestemn0_xfer_p     , compress)
         CALL ncio_write_vector (file_restart, 'deadstemn0_p           ', 'pft', landpft, &
         deadstemn0_p          , compress)
         CALL ncio_write_vector (file_restart, 'deadstemn0_storage_p   ', 'pft', landpft, &
         deadstemn0_storage_p  , compress)
         CALL ncio_write_vector (file_restart, 'deadstemn0_xfer_p      ', 'pft', landpft, &
         deadstemn0_xfer_p     , compress)
         CALL ncio_write_vector (file_restart, 'livecrootn0_p          ', 'pft', landpft, &
         livecrootn0_p         , compress)
         CALL ncio_write_vector (file_restart, 'livecrootn0_storage_p  ', 'pft', landpft, &
         livecrootn0_storage_p , compress)
         CALL ncio_write_vector (file_restart, 'livecrootn0_xfer_p     ', 'pft', landpft, &
         livecrootn0_xfer_p    , compress)
         CALL ncio_write_vector (file_restart, 'deadcrootn0_p          ', 'pft', landpft, &
         deadcrootn0_p         , compress)
         CALL ncio_write_vector (file_restart, 'deadcrootn0_storage_p  ', 'pft', landpft, &
         deadcrootn0_storage_p , compress)
         CALL ncio_write_vector (file_restart, 'deadcrootn0_xfer_p     ', 'pft', landpft, &
         deadcrootn0_xfer_p    , compress)
         CALL ncio_write_vector (file_restart, 'grainn0_p              ', 'pft', landpft, &
         grainn0_p             , compress)
         CALL ncio_write_vector (file_restart, 'grainn0_storage_p      ', 'pft', landpft, &
         grainn0_storage_p     , compress)
         CALL ncio_write_vector (file_restart, 'grainn0_xfer_p         ', 'pft', landpft, &
         grainn0_xfer_p        , compress)
         CALL ncio_write_vector (file_restart, 'retransn0_p            ', 'pft', landpft, &
         retransn0_p           , compress)

         CALL ncio_write_vector (file_restart, 'I_leafc_p_acc            ', 'pft', landpft, &
         I_leafc_p_acc           , compress)
         CALL ncio_write_vector (file_restart, 'I_leafc_st_p_acc         ', 'pft', landpft, &
         I_leafc_st_p_acc        , compress)
         CALL ncio_write_vector (file_restart, 'I_frootc_p_acc           ', 'pft', landpft, &
         I_frootc_p_acc          , compress)
         CALL ncio_write_vector (file_restart, 'I_frootc_st_p_acc        ', 'pft', landpft, &
         I_frootc_st_p_acc       , compress)
         CALL ncio_write_vector (file_restart, 'I_livestemc_p_acc        ', 'pft', landpft, &
         I_livestemc_p_acc       , compress)
         CALL ncio_write_vector (file_restart, 'I_livestemc_st_p_acc     ', 'pft', landpft, &
         I_livestemc_st_p_acc    , compress)
         CALL ncio_write_vector (file_restart, 'I_deadstemc_p_acc        ', 'pft', landpft, &
         I_deadstemc_p_acc       , compress)
         CALL ncio_write_vector (file_restart, 'I_deadstemc_st_p_acc     ', 'pft', landpft, &
         I_deadstemc_st_p_acc    , compress)
         CALL ncio_write_vector (file_restart, 'I_livecrootc_p_acc       ', 'pft', landpft, &
         I_livecrootc_p_acc      , compress)
         CALL ncio_write_vector (file_restart, 'I_livecrootc_st_p_acc    ', 'pft', landpft, &
         I_livecrootc_st_p_acc   , compress)
         CALL ncio_write_vector (file_restart, 'I_deadcrootc_p_acc       ', 'pft', landpft, &
         I_deadcrootc_p_acc      , compress)
         CALL ncio_write_vector (file_restart, 'I_deadcrootc_st_p_acc    ', 'pft', landpft, &
         I_deadcrootc_st_p_acc   , compress)
         CALL ncio_write_vector (file_restart, 'I_grainc_p_acc           ', 'pft', landpft, &
         I_grainc_p_acc          , compress)
         CALL ncio_write_vector (file_restart, 'I_grainc_st_p_acc        ', 'pft', landpft, &
         I_grainc_st_p_acc       , compress)
         CALL ncio_write_vector (file_restart, 'I_leafn_p_acc            ', 'pft', landpft, &
         I_leafn_p_acc           , compress)
         CALL ncio_write_vector (file_restart, 'I_leafn_st_p_acc         ', 'pft', landpft, &
         I_leafn_st_p_acc        , compress)
         CALL ncio_write_vector (file_restart, 'I_frootn_p_acc           ', 'pft', landpft, &
         I_frootn_p_acc          , compress)
         CALL ncio_write_vector (file_restart, 'I_frootn_st_p_acc        ', 'pft', landpft, &
         I_frootn_st_p_acc       , compress)
         CALL ncio_write_vector (file_restart, 'I_livestemn_p_acc        ', 'pft', landpft, &
         I_livestemn_p_acc       , compress)
         CALL ncio_write_vector (file_restart, 'I_livestemn_st_p_acc     ', 'pft', landpft, &
         I_livestemn_st_p_acc    , compress)
         CALL ncio_write_vector (file_restart, 'I_deadstemn_p_acc        ', 'pft', landpft, &
         I_deadstemn_p_acc       , compress)
         CALL ncio_write_vector (file_restart, 'I_deadstemn_st_p_acc     ', 'pft', landpft, &
         I_deadstemn_st_p_acc    , compress)
         CALL ncio_write_vector (file_restart, 'I_livecrootn_p_acc       ', 'pft', landpft, &
         I_livecrootn_p_acc      , compress)
         CALL ncio_write_vector (file_restart, 'I_livecrootn_st_p_acc    ', 'pft', landpft, &
         I_livecrootn_st_p_acc   , compress)
         CALL ncio_write_vector (file_restart, 'I_deadcrootn_p_acc       ', 'pft', landpft, &
         I_deadcrootn_p_acc      , compress)
         CALL ncio_write_vector (file_restart, 'I_deadcrootn_st_p_acc    ', 'pft', landpft, &
         I_deadcrootn_st_p_acc   , compress)
         CALL ncio_write_vector (file_restart, 'I_grainn_p_acc           ', 'pft', landpft, &
         I_grainn_p_acc          , compress)
         CALL ncio_write_vector (file_restart, 'I_grainn_st_p_acc        ', 'pft', landpft, &
         I_grainn_st_p_acc       , compress)

         CALL ncio_write_vector (file_restart, 'AKX_leafc_xf_to_leafc_p_acc               ', 'pft', landpft, &
         AKX_leafc_xf_to_leafc_p_acc              , compress)
         CALL ncio_write_vector (file_restart, 'AKX_frootc_xf_to_frootc_p_acc             ', 'pft', landpft, &
         AKX_frootc_xf_to_frootc_p_acc            , compress)
         CALL ncio_write_vector (file_restart, 'AKX_livestemc_xf_to_livestemc_p_acc       ', 'pft', landpft, &
         AKX_livestemc_xf_to_livestemc_p_acc      , compress)
         CALL ncio_write_vector (file_restart, 'AKX_deadstemc_xf_to_deadstemc_p_acc       ', 'pft', landpft, &
         AKX_deadstemc_xf_to_deadstemc_p_acc      , compress)
         CALL ncio_write_vector (file_restart, 'AKX_livecrootc_xf_to_livecrootc_p_acc     ', 'pft', landpft, &
         AKX_livecrootc_xf_to_livecrootc_p_acc    , compress)
         CALL ncio_write_vector (file_restart, 'AKX_deadcrootc_xf_to_deadcrootc_p_acc     ', 'pft', landpft, &
         AKX_deadcrootc_xf_to_deadcrootc_p_acc    , compress)
         CALL ncio_write_vector (file_restart, 'AKX_grainc_xf_to_grainc_p_acc             ', 'pft', landpft, &
         AKX_grainc_xf_to_grainc_p_acc            , compress)
         CALL ncio_write_vector (file_restart, 'AKX_livestemc_to_deadstemc_p_acc          ', 'pft', landpft, &
         AKX_livestemc_to_deadstemc_p_acc         , compress)
         CALL ncio_write_vector (file_restart, 'AKX_livecrootc_to_deadcrootc_p_acc        ', 'pft', landpft, &
         AKX_livecrootc_to_deadcrootc_p_acc       , compress)


         CALL ncio_write_vector (file_restart, 'AKX_leafc_st_to_leafc_xf_p_acc            ', 'pft', landpft, &
         AKX_leafc_st_to_leafc_xf_p_acc           , compress)
         CALL ncio_write_vector (file_restart, 'AKX_frootc_st_to_frootc_xf_p_acc          ', 'pft', landpft, &
         AKX_frootc_st_to_frootc_xf_p_acc         , compress)
         CALL ncio_write_vector (file_restart, 'AKX_livestemc_st_to_livestemc_xf_p_acc    ', 'pft', landpft, &
         AKX_livestemc_st_to_livestemc_xf_p_acc   , compress)
         CALL ncio_write_vector (file_restart, 'AKX_deadstemc_st_to_deadstemc_xf_p_acc    ', 'pft', landpft, &
         AKX_deadstemc_st_to_deadstemc_xf_p_acc   , compress)
         CALL ncio_write_vector (file_restart, 'AKX_livecrootc_st_to_livecrootc_xf_p_acc  ', 'pft', landpft, &
         AKX_livecrootc_st_to_livecrootc_xf_p_acc , compress)
         CALL ncio_write_vector (file_restart, 'AKX_deadcrootc_st_to_deadcrootc_xf_p_acc  ', 'pft', landpft, &
         AKX_deadcrootc_st_to_deadcrootc_xf_p_acc , compress)
         CALL ncio_write_vector (file_restart, 'AKX_grainc_st_to_grainc_xf_p_acc          ', 'pft', landpft, &
         AKX_grainc_st_to_grainc_xf_p_acc         , compress)

         CALL ncio_write_vector (file_restart, 'AKX_leafc_exit_p_acc                      ', 'pft', landpft, &
         AKX_leafc_exit_p_acc                     , compress)
         CALL ncio_write_vector (file_restart, 'AKX_frootc_exit_p_acc                     ', 'pft', landpft, &
         AKX_frootc_exit_p_acc                    , compress)
         CALL ncio_write_vector (file_restart, 'AKX_livestemc_exit_p_acc                  ', 'pft', landpft, &
         AKX_livestemc_exit_p_acc                 , compress)
         CALL ncio_write_vector (file_restart, 'AKX_deadstemc_exit_p_acc                  ', 'pft', landpft, &
         AKX_deadstemc_exit_p_acc                 , compress)
         CALL ncio_write_vector (file_restart, 'AKX_livecrootc_exit_p_acc                 ', 'pft', landpft, &
         AKX_livecrootc_exit_p_acc                , compress)
         CALL ncio_write_vector (file_restart, 'AKX_deadcrootc_exit_p_acc                 ', 'pft', landpft, &
         AKX_deadcrootc_exit_p_acc                , compress)
         CALL ncio_write_vector (file_restart, 'AKX_grainc_exit_p_acc                     ', 'pft', landpft, &
         AKX_grainc_exit_p_acc                    , compress)

         CALL ncio_write_vector (file_restart, 'AKX_leafc_st_exit_p_acc                   ', 'pft', landpft, &
         AKX_leafc_st_exit_p_acc                  , compress)
         CALL ncio_write_vector (file_restart, 'AKX_frootc_st_exit_p_acc                  ', 'pft', landpft, &
         AKX_frootc_st_exit_p_acc                 , compress)
         CALL ncio_write_vector (file_restart, 'AKX_livestemc_st_exit_p_acc               ', 'pft', landpft, &
         AKX_livestemc_st_exit_p_acc              , compress)
         CALL ncio_write_vector (file_restart, 'AKX_deadstemc_st_exit_p_acc               ', 'pft', landpft, &
         AKX_deadstemc_st_exit_p_acc              , compress)
         CALL ncio_write_vector (file_restart, 'AKX_livecrootc_st_exit_p_acc              ', 'pft', landpft, &
         AKX_livecrootc_st_exit_p_acc             , compress)
         CALL ncio_write_vector (file_restart, 'AKX_deadcrootc_st_exit_p_acc              ', 'pft', landpft, &
         AKX_deadcrootc_st_exit_p_acc             , compress)
         CALL ncio_write_vector (file_restart, 'AKX_grainc_st_exit_p_acc                  ', 'pft', landpft, &
         AKX_grainc_st_exit_p_acc                 , compress)

         CALL ncio_write_vector (file_restart, 'AKX_leafc_xf_exit_p_acc                   ', 'pft', landpft, &
         AKX_leafc_xf_exit_p_acc                  , compress)
         CALL ncio_write_vector (file_restart, 'AKX_frootc_xf_exit_p_acc                  ', 'pft', landpft, &
         AKX_frootc_xf_exit_p_acc                 , compress)
         CALL ncio_write_vector (file_restart, 'AKX_livestemc_xf_exit_p_acc               ', 'pft', landpft, &
         AKX_livestemc_xf_exit_p_acc              , compress)
         CALL ncio_write_vector (file_restart, 'AKX_deadstemc_xf_exit_p_acc               ', 'pft', landpft, &
         AKX_deadstemc_xf_exit_p_acc              , compress)
         CALL ncio_write_vector (file_restart, 'AKX_livecrootc_xf_exit_p_acc              ', 'pft', landpft, &
         AKX_livecrootc_xf_exit_p_acc             , compress)
         CALL ncio_write_vector (file_restart, 'AKX_deadcrootc_xf_exit_p_acc              ', 'pft', landpft, &
         AKX_deadcrootc_xf_exit_p_acc             , compress)
         CALL ncio_write_vector (file_restart, 'AKX_grainc_xf_exit_p_acc                  ', 'pft', landpft, &
         AKX_grainc_xf_exit_p_acc                 , compress)

         CALL ncio_write_vector (file_restart, 'AKX_leafn_xf_to_leafn_p_acc               ', 'pft', landpft, &
         AKX_leafn_xf_to_leafn_p_acc              , compress)
         CALL ncio_write_vector (file_restart, 'AKX_frootn_xf_to_frootn_p_acc             ', 'pft', landpft, &
         AKX_frootn_xf_to_frootn_p_acc            , compress)
         CALL ncio_write_vector (file_restart, 'AKX_livestemn_xf_to_livestemn_p_acc       ', 'pft', landpft, &
         AKX_livestemn_xf_to_livestemn_p_acc      , compress)
         CALL ncio_write_vector (file_restart, 'AKX_deadstemn_xf_to_deadstemn_p_acc       ', 'pft', landpft, &
         AKX_deadstemn_xf_to_deadstemn_p_acc      , compress)
         CALL ncio_write_vector (file_restart, 'AKX_livecrootn_xf_to_livecrootn_p_acc     ', 'pft', landpft, &
         AKX_livecrootn_xf_to_livecrootn_p_acc    , compress)
         CALL ncio_write_vector (file_restart, 'AKX_deadcrootn_xf_to_deadcrootn_p_acc     ', 'pft', landpft, &
         AKX_deadcrootn_xf_to_deadcrootn_p_acc    , compress)
         CALL ncio_write_vector (file_restart, 'AKX_grainn_xf_to_grainn_p_acc             ', 'pft', landpft, &
         AKX_grainn_xf_to_grainn_p_acc            , compress)
         CALL ncio_write_vector (file_restart, 'AKX_livestemn_to_deadstemn_p_acc          ', 'pft', landpft, &
         AKX_livestemn_to_deadstemn_p_acc         , compress)
         CALL ncio_write_vector (file_restart, 'AKX_livecrootn_to_deadcrootn_p_acc        ', 'pft', landpft, &
         AKX_livecrootn_to_deadcrootn_p_acc       , compress)

         CALL ncio_write_vector (file_restart, 'AKX_leafn_st_to_leafn_xf_p_acc            ', 'pft', landpft, &
         AKX_leafn_st_to_leafn_xf_p_acc           , compress)
         CALL ncio_write_vector (file_restart, 'AKX_frootn_st_to_frootn_xf_p_acc          ', 'pft', landpft, &
         AKX_frootn_st_to_frootn_xf_p_acc         , compress)
         CALL ncio_write_vector (file_restart, 'AKX_livestemn_st_to_livestemn_xf_p_acc    ', 'pft', landpft, &
         AKX_livestemn_st_to_livestemn_xf_p_acc   , compress)
         CALL ncio_write_vector (file_restart, 'AKX_deadstemn_st_to_deadstemn_xf_p_acc    ', 'pft', landpft, &
         AKX_deadstemn_st_to_deadstemn_xf_p_acc   , compress)
         CALL ncio_write_vector (file_restart, 'AKX_livecrootn_st_to_livecrootn_xf_p_acc  ', 'pft', landpft, &
         AKX_livecrootn_st_to_livecrootn_xf_p_acc , compress)
         CALL ncio_write_vector (file_restart, 'AKX_deadcrootn_st_to_deadcrootn_xf_p_acc  ', 'pft', landpft, &
         AKX_deadcrootn_st_to_deadcrootn_xf_p_acc , compress)
         CALL ncio_write_vector (file_restart, 'AKX_grainn_st_to_grainn_xf_p_acc          ', 'pft', landpft, &
         AKX_grainn_st_to_grainn_xf_p_acc         , compress)

         CALL ncio_write_vector (file_restart, 'AKX_leafn_to_retransn_p_acc               ', 'pft', landpft, &
         AKX_leafn_to_retransn_p_acc              , compress)
         CALL ncio_write_vector (file_restart, 'AKX_frootn_to_retransn_p_acc              ', 'pft', landpft, &
         AKX_frootn_to_retransn_p_acc             , compress)
         CALL ncio_write_vector (file_restart, 'AKX_livestemn_to_retransn_p_acc           ', 'pft', landpft, &
         AKX_livestemn_to_retransn_p_acc          , compress)
         CALL ncio_write_vector (file_restart, 'AKX_livecrootn_to_retransn_p_acc          ', 'pft', landpft, &
         AKX_livecrootn_to_retransn_p_acc         , compress)

         CALL ncio_write_vector (file_restart, 'AKX_retransn_to_leafn_p_acc               ', 'pft', landpft, &
         AKX_retransn_to_leafn_p_acc              , compress)
         CALL ncio_write_vector (file_restart, 'AKX_retransn_to_frootn_p_acc              ', 'pft', landpft, &
         AKX_retransn_to_frootn_p_acc             , compress)
         CALL ncio_write_vector (file_restart, 'AKX_retransn_to_livestemn_p_acc           ', 'pft', landpft, &
         AKX_retransn_to_livestemn_p_acc          , compress)
         CALL ncio_write_vector (file_restart, 'AKX_retransn_to_deadstemn_p_acc           ', 'pft', landpft, &
         AKX_retransn_to_deadstemn_p_acc          , compress)
         CALL ncio_write_vector (file_restart, 'AKX_retransn_to_livecrootn_p_acc          ', 'pft', landpft, &
         AKX_retransn_to_livecrootn_p_acc         , compress)
         CALL ncio_write_vector (file_restart, 'AKX_retransn_to_deadcrootn_p_acc          ', 'pft', landpft, &
         AKX_retransn_to_deadcrootn_p_acc         , compress)
         CALL ncio_write_vector (file_restart, 'AKX_retransn_to_grainn_p_acc              ', 'pft', landpft, &
         AKX_retransn_to_grainn_p_acc             , compress)

         CALL ncio_write_vector (file_restart, 'AKX_retransn_to_leafn_st_p_acc            ', 'pft', landpft, &
         AKX_retransn_to_leafn_st_p_acc           , compress)
         CALL ncio_write_vector (file_restart, 'AKX_retransn_to_frootn_st_p_acc           ', 'pft', landpft, &
         AKX_retransn_to_frootn_st_p_acc          , compress)
         CALL ncio_write_vector (file_restart, 'AKX_retransn_to_livestemn_st_p_acc        ', 'pft', landpft, &
         AKX_retransn_to_livestemn_st_p_acc       , compress)
         CALL ncio_write_vector (file_restart, 'AKX_retransn_to_deadstemn_st_p_acc        ', 'pft', landpft, &
         AKX_retransn_to_deadstemn_st_p_acc       , compress)
         CALL ncio_write_vector (file_restart, 'AKX_retransn_to_livecrootn_st_p_acc       ', 'pft', landpft, &
         AKX_retransn_to_livecrootn_st_p_acc      , compress)
         CALL ncio_write_vector (file_restart, 'AKX_retransn_to_deadcrootn_st_p_acc       ', 'pft', landpft, &
         AKX_retransn_to_deadcrootn_st_p_acc      , compress)
         CALL ncio_write_vector (file_restart, 'AKX_retransn_to_grainn_st_p_acc           ', 'pft', landpft, &
         AKX_retransn_to_grainn_st_p_acc          , compress)

         CALL ncio_write_vector (file_restart, 'AKX_leafn_exit_p_acc                      ', 'pft', landpft, &
         AKX_leafn_exit_p_acc                     , compress)
         CALL ncio_write_vector (file_restart, 'AKX_frootn_exit_p_acc                     ', 'pft', landpft, &
         AKX_frootn_exit_p_acc                    , compress)
         CALL ncio_write_vector (file_restart, 'AKX_livestemn_exit_p_acc                  ', 'pft', landpft, &
         AKX_livestemn_exit_p_acc                 , compress)
         CALL ncio_write_vector (file_restart, 'AKX_deadstemn_exit_p_acc                  ', 'pft', landpft, &
         AKX_deadstemn_exit_p_acc                 , compress)
         CALL ncio_write_vector (file_restart, 'AKX_livecrootn_exit_p_acc                 ', 'pft', landpft, &
         AKX_livecrootn_exit_p_acc                , compress)
         CALL ncio_write_vector (file_restart, 'AKX_deadcrootn_exit_p_acc                 ', 'pft', landpft, &
         AKX_deadcrootn_exit_p_acc                , compress)
         CALL ncio_write_vector (file_restart, 'AKX_grainn_exit_p_acc                     ', 'pft', landpft, &
         AKX_grainn_exit_p_acc                    , compress)
         CALL ncio_write_vector (file_restart, 'AKX_retransn_exit_p_acc                   ', 'pft', landpft, &
         AKX_retransn_exit_p_acc                  , compress)

         CALL ncio_write_vector (file_restart, 'AKX_leafn_st_exit_p_acc                   ', 'pft', landpft, &
         AKX_leafn_st_exit_p_acc                  , compress)
         CALL ncio_write_vector (file_restart, 'AKX_frootn_st_exit_p_acc                  ', 'pft', landpft, &
         AKX_frootn_st_exit_p_acc                 , compress)
         CALL ncio_write_vector (file_restart, 'AKX_livestemn_st_exit_p_acc               ', 'pft', landpft, &
         AKX_livestemn_st_exit_p_acc              , compress)
         CALL ncio_write_vector (file_restart, 'AKX_deadstemn_st_exit_p_acc               ', 'pft', landpft, &
         AKX_deadstemn_st_exit_p_acc              , compress)
         CALL ncio_write_vector (file_restart, 'AKX_livecrootn_st_exit_p_acc              ', 'pft', landpft, &
         AKX_livecrootn_st_exit_p_acc             , compress)
         CALL ncio_write_vector (file_restart, 'AKX_deadcrootn_st_exit_p_acc              ', 'pft', landpft, &
         AKX_deadcrootn_st_exit_p_acc             , compress)
         CALL ncio_write_vector (file_restart, 'AKX_grainn_st_exit_p_acc                  ', 'pft', landpft, &
         AKX_grainn_st_exit_p_acc                 , compress)

         CALL ncio_write_vector (file_restart, 'AKX_leafn_xf_exit_p_acc                   ', 'pft', landpft, &
         AKX_leafn_xf_exit_p_acc                  , compress)
         CALL ncio_write_vector (file_restart, 'AKX_frootn_xf_exit_p_acc                  ', 'pft', landpft, &
         AKX_frootn_xf_exit_p_acc                 , compress)
         CALL ncio_write_vector (file_restart, 'AKX_livestemn_xf_exit_p_acc               ', 'pft', landpft, &
         AKX_livestemn_xf_exit_p_acc              , compress)
         CALL ncio_write_vector (file_restart, 'AKX_deadstemn_xf_exit_p_acc               ', 'pft', landpft, &
         AKX_deadstemn_xf_exit_p_acc              , compress)
         CALL ncio_write_vector (file_restart, 'AKX_livecrootn_xf_exit_p_acc              ', 'pft', landpft, &
         AKX_livecrootn_xf_exit_p_acc             , compress)
         CALL ncio_write_vector (file_restart, 'AKX_deadcrootn_xf_exit_p_acc              ', 'pft', landpft, &
         AKX_deadcrootn_xf_exit_p_acc             , compress)
         CALL ncio_write_vector (file_restart, 'AKX_grainn_xf_exit_p_acc                  ', 'pft', landpft, &
         AKX_grainn_xf_exit_p_acc                 , compress)
      ENDIF
   END SUBROUTINE WRITE_BGCPFTimeVariables


   SUBROUTINE deallocate_BGCPFTimeVariables ()
! --------------------------------------------------
! Deallocates memory for CoLM 1d [numpft/numpc] variables
! --------------------------------------------------
   USE MOD_MPAS_MPI
   USE MOD_LandPFT

      IF (.true.) THEN
         IF (numpft > 0) THEN
! bgc variables
            deallocate (leafc_p                  )
            deallocate (leafc_storage_p          )
            deallocate (leafc_xfer_p             )
            deallocate (frootc_p                 )
            deallocate (frootc_storage_p         )
            deallocate (frootc_xfer_p            )
            deallocate (livestemc_p              )
            deallocate (livestemc_storage_p      )
            deallocate (livestemc_xfer_p         )
            deallocate (deadstemc_p              )
            deallocate (deadstemc_storage_p      )
            deallocate (deadstemc_xfer_p         )
            deallocate (livecrootc_p             )
            deallocate (livecrootc_storage_p     )
            deallocate (livecrootc_xfer_p        )
            deallocate (deadcrootc_p             )
            deallocate (deadcrootc_storage_p     )
            deallocate (deadcrootc_xfer_p        )
            deallocate (grainc_p                 )
            deallocate (grainc_storage_p         )
            deallocate (grainc_xfer_p            )
            deallocate (cropseedc_deficit_p      )
            deallocate (xsmrpool_p               )
            deallocate (gresp_storage_p          )
            deallocate (gresp_xfer_p             )
            deallocate (cpool_p                  )
            deallocate (totvegc_p                )
            deallocate (cropprod1c_p             )

            deallocate (leaf_prof_p              )
            deallocate (froot_prof_p             )
            deallocate (croot_prof_p             )
            deallocate (stem_prof_p              )
            deallocate (cinput_rootfr_p          )

            deallocate (leafn_p                  )
            deallocate (leafn_storage_p          )
            deallocate (leafn_xfer_p             )
            deallocate (frootn_p                 )
            deallocate (frootn_storage_p         )
            deallocate (frootn_xfer_p            )
            deallocate (livestemn_p              )
            deallocate (livestemn_storage_p      )
            deallocate (livestemn_xfer_p         )
            deallocate (deadstemn_p              )
            deallocate (deadstemn_storage_p      )
            deallocate (deadstemn_xfer_p         )
            deallocate (livecrootn_p             )
            deallocate (livecrootn_storage_p     )
            deallocate (livecrootn_xfer_p        )
            deallocate (deadcrootn_p             )
            deallocate (deadcrootn_storage_p     )
            deallocate (deadcrootn_xfer_p        )
            deallocate (grainn_p                 )
            deallocate (grainn_storage_p         )
            deallocate (grainn_xfer_p            )
            deallocate (cropseedn_deficit_p      )
            deallocate (retransn_p               )
            deallocate (totvegn_p                )

            deallocate (harvdate_p               )

            deallocate (tempsum_potential_gpp_p  )
            deallocate (tempmax_retransn_p       )
            deallocate (tempavg_tref_p           )
            deallocate (tempsum_npp_p            )
            deallocate (tempsum_litfall_p        )
            deallocate (annsum_potential_gpp_p   )
            deallocate (annmax_retransn_p        )
            deallocate (annavg_tref_p            )
            deallocate (annsum_npp_p             )
            deallocate (annsum_litfall_p         )

            deallocate (bglfr_p                  )
            deallocate (bgtr_p                   )
            deallocate (lgsf_p                   )
            deallocate (gdd0_p                   )
            deallocate (gdd8_p                   )
            deallocate (gdd10_p                  )
            deallocate (gdd020_p                 )
            deallocate (gdd820_p                 )
            deallocate (gdd1020_p                )
            deallocate (nyrs_crop_active_p       )

            deallocate (offset_flag_p            )
            deallocate (offset_counter_p         )
            deallocate (onset_flag_p             )
            deallocate (onset_counter_p          )
            deallocate (onset_gddflag_p          )
            deallocate (onset_gdd_p              )
            deallocate (onset_fdd_p              )
            deallocate (onset_swi_p              )
            deallocate (offset_fdd_p             )
            deallocate (offset_swi_p             )
            deallocate (dormant_flag_p           )
            deallocate (prev_leafc_to_litter_p   )
            deallocate (prev_frootc_to_litter_p  )
            deallocate (days_active_p            )

            deallocate (burndate_p               )

            deallocate (c_allometry_p            )
            deallocate (n_allometry_p            )
            deallocate (downreg_p                )
            deallocate (grain_flag_p             )

            deallocate (ctrunc_p                 )
            deallocate (ntrunc_p                 )
            deallocate (npool_p                  )

#ifdef CROP
! crop variables
            deallocate (croplive_p               )
            deallocate (hui_p                    )
            deallocate (gddplant_p               )
            deallocate (peaklai_p                )
            deallocate (aroot_p                  )
            deallocate (astem_p                  )
            deallocate (arepr_p                  )
            deallocate (aleaf_p                  )
            deallocate (astemi_p                 )
            deallocate (aleafi_p                 )
            deallocate (gddmaturity_p            )

            deallocate (cropplant_p              )
            deallocate (idop_p                   )
            deallocate (a5tmin_p                 )
            deallocate (a10tmin_p                )
            deallocate (t10_p                    )
            deallocate (cumvd_p                  )
            deallocate (vf_p                     )
            deallocate (cphase_p                 )
            deallocate (fert_counter_p           )
            deallocate (tref_min_p               )
            deallocate (tref_max_p               )
            deallocate (tref_min_inst_p          )
            deallocate (tref_max_inst_p          )
            deallocate (fertnitro_p              )
            deallocate (manunitro_p              )
            deallocate (fert_p                   )
            deallocate (latbaset_p               )
            deallocate (plantdate_p              )
#endif

! SASU variables
            deallocate (leafcCap_p                  )
            deallocate (leafc_storageCap_p          )
            deallocate (leafc_xferCap_p             )
            deallocate (frootcCap_p                 )
            deallocate (frootc_storageCap_p         )
            deallocate (frootc_xferCap_p            )
            deallocate (livestemcCap_p              )
            deallocate (livestemc_storageCap_p      )
            deallocate (livestemc_xferCap_p         )
            deallocate (deadstemcCap_p              )
            deallocate (deadstemc_storageCap_p      )
            deallocate (deadstemc_xferCap_p         )
            deallocate (livecrootcCap_p             )
            deallocate (livecrootc_storageCap_p     )
            deallocate (livecrootc_xferCap_p        )
            deallocate (deadcrootcCap_p             )
            deallocate (deadcrootc_storageCap_p     )
            deallocate (deadcrootc_xferCap_p        )

            deallocate (leafnCap_p                  )
            deallocate (leafn_storageCap_p          )
            deallocate (leafn_xferCap_p             )
            deallocate (frootnCap_p                 )
            deallocate (frootn_storageCap_p         )
            deallocate (frootn_xferCap_p            )
            deallocate (livestemnCap_p              )
            deallocate (livestemn_storageCap_p      )
            deallocate (livestemn_xferCap_p         )
            deallocate (deadstemnCap_p              )
            deallocate (deadstemn_storageCap_p      )
            deallocate (deadstemn_xferCap_p         )
            deallocate (livecrootnCap_p             )
            deallocate (livecrootn_storageCap_p     )
            deallocate (livecrootn_xferCap_p        )
            deallocate (deadcrootnCap_p             )
            deallocate (deadcrootn_storageCap_p     )
            deallocate (deadcrootn_xferCap_p        )

            deallocate (leafc0_p                 )
            deallocate (leafc0_storage_p         )
            deallocate (leafc0_xfer_p            )
            deallocate (frootc0_p                )
            deallocate (frootc0_storage_p        )
            deallocate (frootc0_xfer_p           )
            deallocate (livestemc0_p             )
            deallocate (livestemc0_storage_p     )
            deallocate (livestemc0_xfer_p        )
            deallocate (deadstemc0_p             )
            deallocate (deadstemc0_storage_p     )
            deallocate (deadstemc0_xfer_p        )
            deallocate (livecrootc0_p            )
            deallocate (livecrootc0_storage_p    )
            deallocate (livecrootc0_xfer_p       )
            deallocate (deadcrootc0_p            )
            deallocate (deadcrootc0_storage_p    )
            deallocate (deadcrootc0_xfer_p       )
            deallocate (grainc0_p                )
            deallocate (grainc0_storage_p        )
            deallocate (grainc0_xfer_p           )

            deallocate (leafn0_p                 )
            deallocate (leafn0_storage_p         )
            deallocate (leafn0_xfer_p            )
            deallocate (frootn0_p                )
            deallocate (frootn0_storage_p        )
            deallocate (frootn0_xfer_p           )
            deallocate (livestemn0_p             )
            deallocate (livestemn0_storage_p     )
            deallocate (livestemn0_xfer_p        )
            deallocate (deadstemn0_p             )
            deallocate (deadstemn0_storage_p     )
            deallocate (deadstemn0_xfer_p        )
            deallocate (livecrootn0_p            )
            deallocate (livecrootn0_storage_p    )
            deallocate (livecrootn0_xfer_p       )
            deallocate (deadcrootn0_p            )
            deallocate (deadcrootn0_storage_p    )
            deallocate (deadcrootn0_xfer_p       )
            deallocate (grainn0_p                )
            deallocate (grainn0_storage_p        )
            deallocate (grainn0_xfer_p           )
            deallocate (retransn0_p              )

            deallocate (I_leafc_p_acc            )
            deallocate (I_leafc_st_p_acc         )
            deallocate (I_frootc_p_acc           )
            deallocate (I_frootc_st_p_acc        )
            deallocate (I_livestemc_p_acc        )
            deallocate (I_livestemc_st_p_acc     )
            deallocate (I_deadstemc_p_acc        )
            deallocate (I_deadstemc_st_p_acc     )
            deallocate (I_livecrootc_p_acc       )
            deallocate (I_livecrootc_st_p_acc    )
            deallocate (I_deadcrootc_p_acc       )
            deallocate (I_deadcrootc_st_p_acc    )
            deallocate (I_grainc_p_acc           )
            deallocate (I_grainc_st_p_acc        )
            deallocate (I_leafn_p_acc            )
            deallocate (I_leafn_st_p_acc         )
            deallocate (I_frootn_p_acc           )
            deallocate (I_frootn_st_p_acc        )
            deallocate (I_livestemn_p_acc        )
            deallocate (I_livestemn_st_p_acc     )
            deallocate (I_deadstemn_p_acc        )
            deallocate (I_deadstemn_st_p_acc     )
            deallocate (I_livecrootn_p_acc       )
            deallocate (I_livecrootn_st_p_acc    )
            deallocate (I_deadcrootn_p_acc       )
            deallocate (I_deadcrootn_st_p_acc    )
            deallocate (I_grainn_p_acc           )
            deallocate (I_grainn_st_p_acc        )

            deallocate (AKX_leafc_xf_to_leafc_p_acc                 )
            deallocate (AKX_frootc_xf_to_frootc_p_acc               )
            deallocate (AKX_livestemc_xf_to_livestemc_p_acc         )
            deallocate (AKX_deadstemc_xf_to_deadstemc_p_acc         )
            deallocate (AKX_livecrootc_xf_to_livecrootc_p_acc       )
            deallocate (AKX_deadcrootc_xf_to_deadcrootc_p_acc       )
            deallocate (AKX_grainc_xf_to_grainc_p_acc               )
            deallocate (AKX_livestemc_to_deadstemc_p_acc            )
            deallocate (AKX_livecrootc_to_deadcrootc_p_acc          )

            deallocate (AKX_leafc_st_to_leafc_xf_p_acc              )
            deallocate (AKX_frootc_st_to_frootc_xf_p_acc            )
            deallocate (AKX_livestemc_st_to_livestemc_xf_p_acc      )
            deallocate (AKX_deadstemc_st_to_deadstemc_xf_p_acc      )
            deallocate (AKX_livecrootc_st_to_livecrootc_xf_p_acc    )
            deallocate (AKX_deadcrootc_st_to_deadcrootc_xf_p_acc    )
            deallocate (AKX_grainc_st_to_grainc_xf_p_acc            )

            deallocate (AKX_leafc_exit_p_acc                        )
            deallocate (AKX_frootc_exit_p_acc                       )
            deallocate (AKX_livestemc_exit_p_acc                    )
            deallocate (AKX_deadstemc_exit_p_acc                    )
            deallocate (AKX_livecrootc_exit_p_acc                   )
            deallocate (AKX_deadcrootc_exit_p_acc                   )
            deallocate (AKX_grainc_exit_p_acc                       )

            deallocate (AKX_leafc_st_exit_p_acc                     )
            deallocate (AKX_frootc_st_exit_p_acc                    )
            deallocate (AKX_livestemc_st_exit_p_acc                 )
            deallocate (AKX_deadstemc_st_exit_p_acc                 )
            deallocate (AKX_livecrootc_st_exit_p_acc                )
            deallocate (AKX_deadcrootc_st_exit_p_acc                )
            deallocate (AKX_grainc_st_exit_p_acc                    )

            deallocate (AKX_leafc_xf_exit_p_acc                     )
            deallocate (AKX_frootc_xf_exit_p_acc                    )
            deallocate (AKX_livestemc_xf_exit_p_acc                 )
            deallocate (AKX_deadstemc_xf_exit_p_acc                 )
            deallocate (AKX_livecrootc_xf_exit_p_acc                )
            deallocate (AKX_deadcrootc_xf_exit_p_acc                )
            deallocate (AKX_grainc_xf_exit_p_acc                    )

            deallocate (AKX_leafn_xf_to_leafn_p_acc                 )
            deallocate (AKX_frootn_xf_to_frootn_p_acc               )
            deallocate (AKX_livestemn_xf_to_livestemn_p_acc         )
            deallocate (AKX_deadstemn_xf_to_deadstemn_p_acc         )
            deallocate (AKX_livecrootn_xf_to_livecrootn_p_acc       )
            deallocate (AKX_deadcrootn_xf_to_deadcrootn_p_acc       )
            deallocate (AKX_grainn_xf_to_grainn_p_acc               )
            deallocate (AKX_livestemn_to_deadstemn_p_acc            )
            deallocate (AKX_livecrootn_to_deadcrootn_p_acc          )

            deallocate (AKX_leafn_st_to_leafn_xf_p_acc              )
            deallocate (AKX_frootn_st_to_frootn_xf_p_acc            )
            deallocate (AKX_livestemn_st_to_livestemn_xf_p_acc      )
            deallocate (AKX_deadstemn_st_to_deadstemn_xf_p_acc      )
            deallocate (AKX_livecrootn_st_to_livecrootn_xf_p_acc    )
            deallocate (AKX_deadcrootn_st_to_deadcrootn_xf_p_acc    )
            deallocate (AKX_grainn_st_to_grainn_xf_p_acc            )

            deallocate (AKX_leafn_to_retransn_p_acc                 )
            deallocate (AKX_frootn_to_retransn_p_acc                )
            deallocate (AKX_livestemn_to_retransn_p_acc             )
            deallocate (AKX_livecrootn_to_retransn_p_acc            )

            deallocate (AKX_retransn_to_leafn_p_acc                 )
            deallocate (AKX_retransn_to_frootn_p_acc                )
            deallocate (AKX_retransn_to_livestemn_p_acc             )
            deallocate (AKX_retransn_to_deadstemn_p_acc             )
            deallocate (AKX_retransn_to_livecrootn_p_acc            )
            deallocate (AKX_retransn_to_deadcrootn_p_acc            )
            deallocate (AKX_retransn_to_grainn_p_acc                )

            deallocate (AKX_retransn_to_leafn_st_p_acc              )
            deallocate (AKX_retransn_to_frootn_st_p_acc             )
            deallocate (AKX_retransn_to_livestemn_st_p_acc          )
            deallocate (AKX_retransn_to_deadstemn_st_p_acc          )
            deallocate (AKX_retransn_to_livecrootn_st_p_acc         )
            deallocate (AKX_retransn_to_deadcrootn_st_p_acc         )
            deallocate (AKX_retransn_to_grainn_st_p_acc             )

            deallocate (AKX_leafn_exit_p_acc                        )
            deallocate (AKX_frootn_exit_p_acc                       )
            deallocate (AKX_livestemn_exit_p_acc                    )
            deallocate (AKX_deadstemn_exit_p_acc                    )
            deallocate (AKX_livecrootn_exit_p_acc                   )
            deallocate (AKX_deadcrootn_exit_p_acc                   )
            deallocate (AKX_grainn_exit_p_acc                       )
            deallocate (AKX_retransn_exit_p_acc                     )

            deallocate (AKX_leafn_st_exit_p_acc                     )
            deallocate (AKX_frootn_st_exit_p_acc                    )
            deallocate (AKX_livestemn_st_exit_p_acc                 )
            deallocate (AKX_deadstemn_st_exit_p_acc                 )
            deallocate (AKX_livecrootn_st_exit_p_acc                )
            deallocate (AKX_deadcrootn_st_exit_p_acc                )
            deallocate (AKX_grainn_st_exit_p_acc                    )

            deallocate (AKX_leafn_xf_exit_p_acc                     )
            deallocate (AKX_frootn_xf_exit_p_acc                    )
            deallocate (AKX_livestemn_xf_exit_p_acc                 )
            deallocate (AKX_deadstemn_xf_exit_p_acc                 )
            deallocate (AKX_livecrootn_xf_exit_p_acc                )
            deallocate (AKX_deadcrootn_xf_exit_p_acc                )
            deallocate (AKX_grainn_xf_exit_p_acc                    )
         ENDIF
      ENDIF

   END SUBROUTINE deallocate_BGCPFTimeVariables

#ifdef RangeCheck
   SUBROUTINE check_BGCPFTimeVariables

   USE MOD_RangeCheck
   USE MOD_MPAS_MPI
   IMPLICIT NONE

! bgc variables
      CALL check_vector_data ('leafc_p                ', leafc_p                )
      CALL check_vector_data ('leafc_storage_p        ', leafc_storage_p        )
      CALL check_vector_data ('leafc_xfer_p           ', leafc_xfer_p           )
      CALL check_vector_data ('frootc_p               ', frootc_p               )
      CALL check_vector_data ('frootc_storage_p       ', frootc_storage_p       )
      CALL check_vector_data ('frootc_xfer_p          ', frootc_xfer_p          )
      CALL check_vector_data ('livestemc_p            ', livestemc_p            )
      CALL check_vector_data ('livestemc_storage_p    ', livestemc_storage_p    )
      CALL check_vector_data ('livestemc_xfer_p       ', livestemc_xfer_p       )
      CALL check_vector_data ('deadstemc_p            ', deadstemc_p            )
      CALL check_vector_data ('deadstemc_storage_p    ', deadstemc_storage_p    )
      CALL check_vector_data ('deadstemc_xfer_p       ', deadstemc_xfer_p       )
      CALL check_vector_data ('livecrootc_p           ', livecrootc_p           )
      CALL check_vector_data ('livecrootc_storage_p   ', livecrootc_storage_p   )
      CALL check_vector_data ('livecrootc_xfer_p      ', livecrootc_xfer_p      )
      CALL check_vector_data ('deadcrootc_p           ', deadcrootc_p           )
      CALL check_vector_data ('deadcrootc_storage_p   ', deadcrootc_storage_p   )
      CALL check_vector_data ('deadcrootc_xfer_p      ', deadcrootc_xfer_p      )
      CALL check_vector_data ('grainc_p               ', grainc_p               )
      CALL check_vector_data ('grainc_storage_p       ', grainc_storage_p       )
      CALL check_vector_data ('grainc_xfer_p          ', grainc_xfer_p          )
      CALL check_vector_data ('cropseedc_deficit_p    ', cropseedc_deficit_p    )
      CALL check_vector_data ('xsmrpool_p             ', xsmrpool_p             )
      CALL check_vector_data ('gresp_storage_p        ', gresp_storage_p        )
      CALL check_vector_data ('gresp_xfer_p           ', gresp_xfer_p           )
      CALL check_vector_data ('cpool_p                ', cpool_p                )
      CALL check_vector_data ('totvegc_p              ', totvegc_p              )
      CALL check_vector_data ('cropprod1c_p           ', cropprod1c_p           )

      CALL check_vector_data ('leaf_prof_p            ', leaf_prof_p            )
      CALL check_vector_data ('froot_prof_p           ', froot_prof_p           )
      CALL check_vector_data ('croot_prof_p           ', croot_prof_p           )
      CALL check_vector_data ('stem_prof_p            ', stem_prof_p            )
      CALL check_vector_data ('cinput_rootfr_p        ', cinput_rootfr_p        )

      CALL check_vector_data ('leafn_p                ', leafn_p                )
      CALL check_vector_data ('leafn_storage_p        ', leafn_storage_p        )
      CALL check_vector_data ('leafn_xfer_p           ', leafn_xfer_p           )
      CALL check_vector_data ('frootn_p               ', frootn_p               )
      CALL check_vector_data ('frootn_storage_p       ', frootn_storage_p       )
      CALL check_vector_data ('frootn_xfer_p          ', frootn_xfer_p          )
      CALL check_vector_data ('livestemn_p            ', livestemn_p            )
      CALL check_vector_data ('livestemn_storage_p    ', livestemn_storage_p    )
      CALL check_vector_data ('livestemn_xfer_p       ', livestemn_xfer_p       )
      CALL check_vector_data ('deadstemn_p            ', deadstemn_p            )
      CALL check_vector_data ('deadstemn_storage_p    ', deadstemn_storage_p    )
      CALL check_vector_data ('deadstemn_xfer_p       ', deadstemn_xfer_p       )
      CALL check_vector_data ('livecrootn_p           ', livecrootn_p           )
      CALL check_vector_data ('livecrootn_storage_p   ', livecrootn_storage_p   )
      CALL check_vector_data ('livecrootn_xfer_p      ', livecrootn_xfer_p      )
      CALL check_vector_data ('deadcrootn_p           ', deadcrootn_p           )
      CALL check_vector_data ('deadcrootn_storage_p   ', deadcrootn_storage_p   )
      CALL check_vector_data ('deadcrootn_xfer_p      ', deadcrootn_xfer_p      )
      CALL check_vector_data ('grainn_p               ', grainn_p               )
      CALL check_vector_data ('grainn_storage_p       ', grainn_storage_p       )
      CALL check_vector_data ('grainn_xfer_p          ', grainn_xfer_p          )
      CALL check_vector_data ('cropseedn_deficit_p    ', cropseedn_deficit_p    )
      CALL check_vector_data ('retransn_p             ', retransn_p             )
      CALL check_vector_data ('totvegn_p              ', totvegn_p              )

      CALL check_vector_data ('harvdate_p             ', harvdate_p             )

      CALL check_vector_data ('tempsum_potential_gpp_p', tempsum_potential_gpp_p)
      CALL check_vector_data ('tempmax_retransn_p     ', tempmax_retransn_p     )
      CALL check_vector_data ('tempavg_tref_p         ', tempavg_tref_p         )
      CALL check_vector_data ('tempsum_npp_p          ', tempsum_npp_p          )
      CALL check_vector_data ('tempsum_litfall_p      ', tempsum_litfall_p      )
      CALL check_vector_data ('annsum_potential_gpp_p ', annsum_potential_gpp_p )
      CALL check_vector_data ('annmax_retransn_p      ', annmax_retransn_p      )
      CALL check_vector_data ('annavg_tref_p          ', annavg_tref_p          )
      CALL check_vector_data ('annsum_npp_p           ', annsum_npp_p           )
      CALL check_vector_data ('annsum_litfall_p       ', annsum_litfall_p       )

      CALL check_vector_data ('bglfr_p                ', bglfr_p                )
      CALL check_vector_data ('bgtr_p                 ', bgtr_p                 )
      CALL check_vector_data ('lgsf_p                 ', lgsf_p                 )
      CALL check_vector_data ('gdd0_p                 ', gdd0_p                 )
      CALL check_vector_data ('gdd8_p                 ', gdd8_p                 )
      CALL check_vector_data ('gdd10_p                ', gdd10_p                )
      CALL check_vector_data ('gdd020_p               ', gdd020_p               )
      CALL check_vector_data ('gdd820_p               ', gdd820_p               )
      CALL check_vector_data ('gdd1020_p              ', gdd1020_p              )

      CALL check_vector_data ('offset_flag_p          ', offset_flag_p          )
      CALL check_vector_data ('offset_counter_p       ', offset_counter_p       )
      CALL check_vector_data ('onset_flag_p           ', onset_flag_p           )
      CALL check_vector_data ('onset_counter_p        ', onset_counter_p        )
      CALL check_vector_data ('onset_gddflag_p        ', onset_gddflag_p        )
      CALL check_vector_data ('onset_gdd_p            ', onset_gdd_p            )
      CALL check_vector_data ('onset_fdd_p            ', onset_fdd_p            )
      CALL check_vector_data ('onset_swi_p            ', onset_swi_p            )
      CALL check_vector_data ('offset_fdd_p           ', offset_fdd_p           )
      CALL check_vector_data ('offset_swi_p           ', offset_swi_p           )
      CALL check_vector_data ('dormant_flag_p         ', dormant_flag_p         )
      CALL check_vector_data ('prev_leafc_to_litter_p ', prev_leafc_to_litter_p )
      CALL check_vector_data ('prev_frootc_to_litter_p', prev_frootc_to_litter_p)
      CALL check_vector_data ('days_active_p          ', days_active_p          )

      CALL check_vector_data ('burndate_p             ', burndate_p             )

      CALL check_vector_data ('c_allometry_p          ', c_allometry_p          )
      CALL check_vector_data ('n_allometry_p          ', n_allometry_p          )
      CALL check_vector_data ('downreg_p              ', downreg_p              )
      CALL check_vector_data ('grain_flag_p           ', grain_flag_p           )

      CALL check_vector_data ('ctrunc_p               ', ctrunc_p               )
      CALL check_vector_data ('ntrunc_p               ', ntrunc_p               )
      CALL check_vector_data ('npool_p                ', npool_p                )

#ifdef CROP
! crop variables
      CALL check_vector_data ('hui_p                  ', hui_p                  )
      CALL check_vector_data ('gddplant_p             ', gddplant_p             )
      CALL check_vector_data ('aroot_p                ', aroot_p                )
      CALL check_vector_data ('astem_p                ', astem_p                )
      CALL check_vector_data ('arepr_p                ', arepr_p                )
      CALL check_vector_data ('aleaf_p                ', aleaf_p                )
      CALL check_vector_data ('astemi_p               ', astemi_p               )
      CALL check_vector_data ('aleafi_p               ', aleafi_p               )
      CALL check_vector_data ('gddmaturity_p          ', gddmaturity_p          )

      CALL check_vector_data ('a5tmin_p               ', a5tmin_p               )
      CALL check_vector_data ('a10tmin_p              ', a10tmin_p              )
      CALL check_vector_data ('t10_p                  ', t10_p                  )
      CALL check_vector_data ('cumvd_p                ', cumvd_p                )
      CALL check_vector_data ('vf_p                   ', vf_p                   )
      CALL check_vector_data ('cphase_p               ', cphase_p               )
      CALL check_vector_data ('fert_counter_p         ', fert_counter_p         )
      CALL check_vector_data ('tref_min_p             ', tref_min_p             )
      CALL check_vector_data ('tref_max_p             ', tref_max_p             )
      CALL check_vector_data ('tref_min_inst_p        ', tref_min_inst_p        )
      CALL check_vector_data ('tref_max_inst_p        ', tref_max_inst_p        )
      CALL check_vector_data ('fertnitro_p            ', fertnitro_p            )
      CALL check_vector_data ('manunitro_p            ', manunitro_p            )
      CALL check_vector_data ('fert_p                 ', fert_p                 )
      CALL check_vector_data ('latbaset_p             ', latbaset_p             )
      CALL check_vector_data ('plantdate_p            ', plantdate_p            )
#endif

      IF(DEF_USE_DiagMatrix)THEN
! SASU variables
         CALL check_vector_data ('leafcCap_p                ', leafcCap_p                )
         CALL check_vector_data ('leafc_storageCap_p        ', leafc_storageCap_p        )
         CALL check_vector_data ('leafc_xferCap_p           ', leafc_xferCap_p           )
         CALL check_vector_data ('frootcCap_p               ', frootcCap_p               )
         CALL check_vector_data ('frootc_storageCap_p       ', frootc_storageCap_p       )
         CALL check_vector_data ('frootc_xferCap_p          ', frootc_xferCap_p          )
         CALL check_vector_data ('livestemcCap_p            ', livestemcCap_p            )
         CALL check_vector_data ('livestemc_storageCap_p    ', livestemc_storageCap_p    )
         CALL check_vector_data ('livestemc_xferCap_p       ', livestemc_xferCap_p       )
         CALL check_vector_data ('deadstemcCap_p            ', deadstemcCap_p            )
         CALL check_vector_data ('deadstemc_storageCap_p    ', deadstemc_storageCap_p    )
         CALL check_vector_data ('deadstemc_xferCap_p       ', deadstemc_xferCap_p       )
         CALL check_vector_data ('livecrootcCap_p           ', livecrootcCap_p           )
         CALL check_vector_data ('livecrootc_storageCap_p   ', livecrootc_storageCap_p   )
         CALL check_vector_data ('livecrootc_xferCap_p      ', livecrootc_xferCap_p      )
         CALL check_vector_data ('deadcrootcCap_p           ', deadcrootcCap_p           )
         CALL check_vector_data ('deadcrootc_storageCap_p   ', deadcrootc_storageCap_p   )
         CALL check_vector_data ('deadcrootc_xferCap_p      ', deadcrootc_xferCap_p      )

         CALL check_vector_data ('leafnCap_p                ', leafnCap_p                )
         CALL check_vector_data ('leafn_storageCap_p        ', leafn_storageCap_p        )
         CALL check_vector_data ('leafn_xferCap_p           ', leafn_xferCap_p           )
         CALL check_vector_data ('frootnCap_p               ', frootnCap_p               )
         CALL check_vector_data ('frootn_storageCap_p       ', frootn_storageCap_p       )
         CALL check_vector_data ('frootn_xferCap_p          ', frootn_xferCap_p          )
         CALL check_vector_data ('livestemnCap_p            ', livestemnCap_p            )
         CALL check_vector_data ('livestemn_storageCap_p    ', livestemn_storageCap_p    )
         CALL check_vector_data ('livestemn_xferCap_p       ', livestemn_xferCap_p       )
         CALL check_vector_data ('deadstemnCap_p            ', deadstemnCap_p            )
         CALL check_vector_data ('deadstemn_storageCap_p    ', deadstemn_storageCap_p    )
         CALL check_vector_data ('deadstemn_xferCap_p       ', deadstemn_xferCap_p       )
         CALL check_vector_data ('livecrootnCap_p           ', livecrootnCap_p           )
         CALL check_vector_data ('livecrootn_storageCap_p   ', livecrootn_storageCap_p   )
         CALL check_vector_data ('livecrootn_xferCap_p      ', livecrootn_xferCap_p      )
         CALL check_vector_data ('deadcrootnCap_p           ', deadcrootnCap_p           )
         CALL check_vector_data ('deadcrootn_storageCap_p   ', deadcrootn_storageCap_p   )
         CALL check_vector_data ('deadcrootn_xferCap_p      ', deadcrootn_xferCap_p      )
      ENDIF

      IF(DEF_USE_SASU .or. DEF_USE_DiagMatrix)THEN
         CALL check_vector_data ('leafc0_p               ', leafc0_p               )
         CALL check_vector_data ('leafc0_storage_p       ', leafc0_storage_p       )
         CALL check_vector_data ('leafc0_xfer_p          ', leafc0_xfer_p          )
         CALL check_vector_data ('frootc0_p              ', frootc0_p              )
         CALL check_vector_data ('frootc0_storage_p      ', frootc0_storage_p      )
         CALL check_vector_data ('frootc0_xfer_p         ', frootc0_xfer_p         )
         CALL check_vector_data ('livestemc0_p           ', livestemc0_p           )
         CALL check_vector_data ('livestemc0_storage_p   ', livestemc0_storage_p   )
         CALL check_vector_data ('livestemc0_xfer_p      ', livestemc0_xfer_p      )
         CALL check_vector_data ('deadstemc0_p           ', deadstemc0_p           )
         CALL check_vector_data ('deadstemc0_storage_p   ', deadstemc0_storage_p   )
         CALL check_vector_data ('deadstemc0_xfer_p      ', deadstemc0_xfer_p      )
         CALL check_vector_data ('livecrootc0_p          ', livecrootc0_p          )
         CALL check_vector_data ('livecrootc0_storage_p  ', livecrootc0_storage_p  )
         CALL check_vector_data ('livecrootc0_xfer_p     ', livecrootc0_xfer_p     )
         CALL check_vector_data ('deadcrootc0_p          ', deadcrootc0_p          )
         CALL check_vector_data ('deadcrootc0_storage_p  ', deadcrootc0_storage_p  )
         CALL check_vector_data ('deadcrootc0_xfer_p     ', deadcrootc0_xfer_p     )
         CALL check_vector_data ('grainc0_p              ', grainc0_p              )
         CALL check_vector_data ('grainc0_storage_p      ', grainc0_storage_p      )
         CALL check_vector_data ('grainc0_xfer_p         ', grainc0_xfer_p         )

         CALL check_vector_data ('leafn0_p               ', leafn0_p               )
         CALL check_vector_data ('leafn0_storage_p       ', leafn0_storage_p       )
         CALL check_vector_data ('leafn0_xfer_p          ', leafn0_xfer_p          )
         CALL check_vector_data ('frootn0_p              ', frootn0_p              )
         CALL check_vector_data ('frootn0_storage_p      ', frootn0_storage_p      )
         CALL check_vector_data ('frootn0_xfer_p         ', frootn0_xfer_p         )
         CALL check_vector_data ('livestemn0_p           ', livestemn0_p           )
         CALL check_vector_data ('livestemn0_storage_p   ', livestemn0_storage_p   )
         CALL check_vector_data ('livestemn0_xfer_p      ', livestemn0_xfer_p      )
         CALL check_vector_data ('deadstemn0_p           ', deadstemn0_p           )
         CALL check_vector_data ('deadstemn0_storage_p   ', deadstemn0_storage_p   )
         CALL check_vector_data ('deadstemn0_xfer_p      ', deadstemn0_xfer_p      )
         CALL check_vector_data ('livecrootn0_p          ', livecrootn0_p          )
         CALL check_vector_data ('livecrootn0_storage_p  ', livecrootn0_storage_p  )
         CALL check_vector_data ('livecrootn0_xfer_p     ', livecrootn0_xfer_p     )
         CALL check_vector_data ('deadcrootn0_p          ', deadcrootn0_p          )
         CALL check_vector_data ('deadcrootn0_storage_p  ', deadcrootn0_storage_p  )
         CALL check_vector_data ('deadcrootn0_xfer_p     ', deadcrootn0_xfer_p     )
         CALL check_vector_data ('grainn0_p              ', grainn0_p              )
         CALL check_vector_data ('grainn0_storage_p      ', grainn0_storage_p      )
         CALL check_vector_data ('grainn0_xfer_p         ', grainn0_xfer_p         )
         CALL check_vector_data ('retransn0_p            ', retransn0_p            )

         CALL check_vector_data ('I_leafc_p_acc          ', I_leafc_p_acc          )
         CALL check_vector_data ('I_leafc_st_p_acc       ', I_leafc_st_p_acc       )
         CALL check_vector_data ('I_frootc_p_acc         ', I_frootc_p_acc         )
         CALL check_vector_data ('I_frootc_st_p_acc      ', I_frootc_st_p_acc      )
         CALL check_vector_data ('I_livestemc_p_acc      ', I_livestemc_p_acc      )
         CALL check_vector_data ('I_livestemc_st_p_acc   ', I_livestemc_st_p_acc   )
         CALL check_vector_data ('I_deadstemc_p_acc      ', I_deadstemc_p_acc      )
         CALL check_vector_data ('I_deadstemc_st_p_acc   ', I_deadstemc_st_p_acc   )
         CALL check_vector_data ('I_livecrootc_p_acc     ', I_livecrootc_p_acc     )
         CALL check_vector_data ('I_livecrootc_st_p_acc  ', I_livecrootc_st_p_acc  )
         CALL check_vector_data ('I_deadcrootc_p_acc     ', I_deadcrootc_p_acc     )
         CALL check_vector_data ('I_deadcrootc_st_p_acc  ', I_deadcrootc_st_p_acc  )
         CALL check_vector_data ('I_grainc_p_acc         ', I_grainc_p_acc         )
         CALL check_vector_data ('I_grainc_st_p_acc      ', I_grainc_st_p_acc      )
         CALL check_vector_data ('I_leafn_p_acc          ', I_leafn_p_acc          )
         CALL check_vector_data ('I_leafn_st_p_acc       ', I_leafn_st_p_acc       )
         CALL check_vector_data ('I_frootn_p_acc         ', I_frootn_p_acc         )
         CALL check_vector_data ('I_frootn_st_p_acc      ', I_frootn_st_p_acc      )
         CALL check_vector_data ('I_livestemn_p_acc      ', I_livestemn_p_acc      )
         CALL check_vector_data ('I_livestemn_st_p_acc   ', I_livestemn_st_p_acc   )
         CALL check_vector_data ('I_deadstemn_p_acc      ', I_deadstemn_p_acc      )
         CALL check_vector_data ('I_deadstemn_st_p_acc   ', I_deadstemn_st_p_acc   )
         CALL check_vector_data ('I_livecrootn_p_acc     ', I_livecrootn_p_acc     )
         CALL check_vector_data ('I_livecrootn_st_p_acc  ', I_livecrootn_st_p_acc  )
         CALL check_vector_data ('I_deadcrootn_p_acc     ', I_deadcrootn_p_acc     )
         CALL check_vector_data ('I_deadcrootn_st_p_acc  ', I_deadcrootn_st_p_acc  )
         CALL check_vector_data ('I_grainn_p_acc         ', I_grainn_p_acc         )
         CALL check_vector_data ('I_grainn_st_p_acc      ', I_grainn_st_p_acc      )

         CALL check_vector_data ('AKX_leafc_xf_to_leafc_p_acc               ', AKX_leafc_xf_to_leafc_p_acc               )
         CALL check_vector_data ('AKX_frootc_xf_to_frootc_p_acc             ', AKX_frootc_xf_to_frootc_p_acc             )
         CALL check_vector_data ('AKX_livestemc_xf_to_livestemc_p_acc       ', AKX_livestemc_xf_to_livestemc_p_acc       )
         CALL check_vector_data ('AKX_deadstemc_xf_to_deadstemc_p_acc       ', AKX_deadstemc_xf_to_deadstemc_p_acc       )
         CALL check_vector_data ('AKX_livecrootc_xf_to_livecrootc_p_acc     ', AKX_livecrootc_xf_to_livecrootc_p_acc     )
         CALL check_vector_data ('AKX_deadcrootc_xf_to_deadcrootc_p_acc     ', AKX_deadcrootc_xf_to_deadcrootc_p_acc     )
         CALL check_vector_data ('AKX_grainc_xf_to_grainc_p_acc             ', AKX_grainc_xf_to_grainc_p_acc             )
         CALL check_vector_data ('AKX_livestemc_to_deadstemc_p_acc          ', AKX_livestemc_to_deadstemc_p_acc          )
         CALL check_vector_data ('AKX_livecrootc_to_deadcrootc_p_acc        ', AKX_livecrootc_to_deadcrootc_p_acc        )
         CALL check_vector_data ('AKX_leafc_st_to_leafc_xf_p_acc            ', AKX_leafc_st_to_leafc_xf_p_acc            )
         CALL check_vector_data ('AKX_frootc_st_to_frootc_xf_p_acc          ', AKX_frootc_st_to_frootc_xf_p_acc          )
         CALL check_vector_data ('AKX_livestemc_st_to_livestemc_xf_p_acc    ', AKX_livestemc_st_to_livestemc_xf_p_acc    )
         CALL check_vector_data ('AKX_deadstemc_st_to_deadstemc_xf_p_acc    ', AKX_deadstemc_st_to_deadstemc_xf_p_acc    )
         CALL check_vector_data ('AKX_livecrootc_st_to_livecrootc_xf_p_acc  ', AKX_livecrootc_st_to_livecrootc_xf_p_acc  )
         CALL check_vector_data ('AKX_deadcrootc_st_to_deadcrootc_xf_p_acc  ', AKX_deadcrootc_st_to_deadcrootc_xf_p_acc  )
         CALL check_vector_data ('AKX_grainc_st_to_grainc_xf_p_acc          ', AKX_grainc_st_to_grainc_xf_p_acc          )
         CALL check_vector_data ('AKX_leafc_exit_p_acc                      ', AKX_leafc_exit_p_acc                      )
         CALL check_vector_data ('AKX_frootc_exit_p_acc                     ', AKX_frootc_exit_p_acc                     )
         CALL check_vector_data ('AKX_livestemc_exit_p_acc                  ', AKX_livestemc_exit_p_acc                  )
         CALL check_vector_data ('AKX_deadstemc_exit_p_acc                  ', AKX_deadstemc_exit_p_acc                  )
         CALL check_vector_data ('AKX_livecrootc_exit_p_acc                 ', AKX_livecrootc_exit_p_acc                 )
         CALL check_vector_data ('AKX_deadcrootc_exit_p_acc                 ', AKX_deadcrootc_exit_p_acc                 )
         CALL check_vector_data ('AKX_grainc_exit_p_acc                     ', AKX_grainc_exit_p_acc                     )

         CALL check_vector_data ('AKX_leafc_st_exit_p_acc                   ', AKX_leafc_st_exit_p_acc                   )
         CALL check_vector_data ('AKX_frootc_st_exit_p_acc                  ', AKX_frootc_st_exit_p_acc                  )
         CALL check_vector_data ('AKX_livestemc_st_exit_p_acc               ', AKX_livestemc_st_exit_p_acc               )
         CALL check_vector_data ('AKX_deadstemc_st_exit_p_acc               ', AKX_deadstemc_st_exit_p_acc               )
         CALL check_vector_data ('AKX_livecrootc_st_exit_p_acc              ', AKX_livecrootc_st_exit_p_acc              )
         CALL check_vector_data ('AKX_deadcrootc_st_exit_p_acc              ', AKX_deadcrootc_st_exit_p_acc              )
         CALL check_vector_data ('AKX_grainc_st_exit_p_acc                  ', AKX_grainc_st_exit_p_acc                  )

         CALL check_vector_data ('AKX_leafc_xf_exit_p_acc                   ', AKX_leafc_xf_exit_p_acc                   )
         CALL check_vector_data ('AKX_frootc_xf_exit_p_acc                  ', AKX_frootc_xf_exit_p_acc                  )
         CALL check_vector_data ('AKX_livestemc_xf_exit_p_acc               ', AKX_livestemc_xf_exit_p_acc               )
         CALL check_vector_data ('AKX_deadstemc_xf_exit_p_acc               ', AKX_deadstemc_xf_exit_p_acc               )
         CALL check_vector_data ('AKX_livecrootc_xf_exit_p_acc              ', AKX_livecrootc_xf_exit_p_acc              )
         CALL check_vector_data ('AKX_deadcrootc_xf_exit_p_acc              ', AKX_deadcrootc_xf_exit_p_acc              )
         CALL check_vector_data ('AKX_grainc_xf_exit_p_acc                  ', AKX_grainc_xf_exit_p_acc                  )

         CALL check_vector_data ('AKX_leafn_xf_to_leafn_p_acc               ', AKX_leafn_xf_to_leafn_p_acc               )
         CALL check_vector_data ('AKX_frootn_xf_to_frootn_p_acc             ', AKX_frootn_xf_to_frootn_p_acc             )
         CALL check_vector_data ('AKX_livestemn_xf_to_livestemn_p_acc       ', AKX_livestemn_xf_to_livestemn_p_acc       )
         CALL check_vector_data ('AKX_deadstemn_xf_to_deadstemn_p_acc       ', AKX_deadstemn_xf_to_deadstemn_p_acc       )
         CALL check_vector_data ('AKX_livecrootn_xf_to_livecrootn_p_acc     ', AKX_livecrootn_xf_to_livecrootn_p_acc     )
         CALL check_vector_data ('AKX_deadcrootn_xf_to_deadcrootn_p_acc     ', AKX_deadcrootn_xf_to_deadcrootn_p_acc     )
         CALL check_vector_data ('AKX_grainn_xf_to_grainn_p_acc             ', AKX_grainn_xf_to_grainn_p_acc             )
         CALL check_vector_data ('AKX_livestemn_to_deadstemn_p_acc          ', AKX_livestemn_to_deadstemn_p_acc          )
         CALL check_vector_data ('AKX_livecrootn_to_deadcrootn_p_acc        ', AKX_livecrootn_to_deadcrootn_p_acc        )

         CALL check_vector_data ('AKX_leafn_st_to_leafn_xf_p_acc            ', AKX_leafn_st_to_leafn_xf_p_acc            )
         CALL check_vector_data ('AKX_frootn_st_to_frootn_xf_p_acc          ', AKX_frootn_st_to_frootn_xf_p_acc          )
         CALL check_vector_data ('AKX_livestemn_st_to_livestemn_xf_p_acc    ', AKX_livestemn_st_to_livestemn_xf_p_acc    )
         CALL check_vector_data ('AKX_deadstemn_st_to_deadstemn_xf_p_acc    ', AKX_deadstemn_st_to_deadstemn_xf_p_acc    )
         CALL check_vector_data ('AKX_livecrootn_st_to_livecrootn_xf_p_acc  ', AKX_livecrootn_st_to_livecrootn_xf_p_acc  )
         CALL check_vector_data ('AKX_deadcrootn_st_to_deadcrootn_xf_p_acc  ', AKX_deadcrootn_st_to_deadcrootn_xf_p_acc  )
         CALL check_vector_data ('AKX_grainn_st_to_grainn_xf_p_acc          ', AKX_grainn_st_to_grainn_xf_p_acc          )

         CALL check_vector_data ('AKX_leafn_to_retransn_p_acc               ', AKX_leafn_to_retransn_p_acc               )
         CALL check_vector_data ('AKX_frootn_to_retransn_p_acc              ', AKX_frootn_to_retransn_p_acc              )
         CALL check_vector_data ('AKX_livestemn_to_retransn_p_acc           ', AKX_livestemn_to_retransn_p_acc           )
         CALL check_vector_data ('AKX_livecrootn_to_retransn_p_acc          ', AKX_livecrootn_to_retransn_p_acc          )

         CALL check_vector_data ('AKX_retransn_to_leafn_p_acc               ', AKX_retransn_to_leafn_p_acc               )
         CALL check_vector_data ('AKX_retransn_to_frootn_p_acc              ', AKX_retransn_to_frootn_p_acc              )
         CALL check_vector_data ('AKX_retransn_to_livestemn_p_acc           ', AKX_retransn_to_livestemn_p_acc           )
         CALL check_vector_data ('AKX_retransn_to_deadstemn_p_acc           ', AKX_retransn_to_deadstemn_p_acc           )
         CALL check_vector_data ('AKX_retransn_to_livecrootn_p_acc          ', AKX_retransn_to_livecrootn_p_acc          )
         CALL check_vector_data ('AKX_retransn_to_deadcrootn_p_acc          ', AKX_retransn_to_deadcrootn_p_acc          )
         CALL check_vector_data ('AKX_retransn_to_grainn_p_acc              ', AKX_retransn_to_grainn_p_acc              )

         CALL check_vector_data ('AKX_retransn_to_leafn_st_p_acc            ', AKX_retransn_to_leafn_st_p_acc            )
         CALL check_vector_data ('AKX_retransn_to_frootn_st_p_acc           ', AKX_retransn_to_frootn_st_p_acc           )
         CALL check_vector_data ('AKX_retransn_to_livestemn_st_p_acc        ', AKX_retransn_to_livestemn_st_p_acc        )
         CALL check_vector_data ('AKX_retransn_to_deadstemn_st_p_acc        ', AKX_retransn_to_deadstemn_st_p_acc        )
         CALL check_vector_data ('AKX_retransn_to_livecrootn_st_p_acc       ', AKX_retransn_to_livecrootn_st_p_acc       )
         CALL check_vector_data ('AKX_retransn_to_deadcrootn_st_p_acc       ', AKX_retransn_to_deadcrootn_st_p_acc       )
         CALL check_vector_data ('AKX_retransn_to_grainn_st_p_acc           ', AKX_retransn_to_grainn_st_p_acc           )

         CALL check_vector_data ('AKX_leafn_exit_p_acc                      ', AKX_leafn_exit_p_acc                      )
         CALL check_vector_data ('AKX_frootn_exit_p_acc                     ', AKX_frootn_exit_p_acc                     )
         CALL check_vector_data ('AKX_livestemn_exit_p_acc                  ', AKX_livestemn_exit_p_acc                  )
         CALL check_vector_data ('AKX_deadstemn_exit_p_acc                  ', AKX_deadstemn_exit_p_acc                  )
         CALL check_vector_data ('AKX_livecrootn_exit_p_acc                 ', AKX_livecrootn_exit_p_acc                 )
         CALL check_vector_data ('AKX_deadcrootn_exit_p_acc                 ', AKX_deadcrootn_exit_p_acc                 )
         CALL check_vector_data ('AKX_grainn_exit_p_acc                     ', AKX_grainn_exit_p_acc                     )
         CALL check_vector_data ('AKX_retransn_exit_p_acc                   ', AKX_retransn_exit_p_acc                   )

         CALL check_vector_data ('AKX_leafn_st_exit_p_acc                   ', AKX_leafn_st_exit_p_acc                   )
         CALL check_vector_data ('AKX_frootn_st_exit_p_acc                  ', AKX_frootn_st_exit_p_acc                  )
         CALL check_vector_data ('AKX_livestemn_st_exit_p_acc               ', AKX_livestemn_st_exit_p_acc               )
         CALL check_vector_data ('AKX_deadstemn_st_exit_p_acc               ', AKX_deadstemn_st_exit_p_acc               )
         CALL check_vector_data ('AKX_livecrootn_st_exit_p_acc              ', AKX_livecrootn_st_exit_p_acc              )
         CALL check_vector_data ('AKX_deadcrootn_st_exit_p_acc              ', AKX_deadcrootn_st_exit_p_acc              )
         CALL check_vector_data ('AKX_grainn_st_exit_p_acc                  ', AKX_grainn_st_exit_p_acc                  )

         CALL check_vector_data ('AKX_leafn_xf_exit_p_acc                   ', AKX_leafn_xf_exit_p_acc                   )
         CALL check_vector_data ('AKX_frootn_xf_exit_p_acc                  ', AKX_frootn_xf_exit_p_acc                  )
         CALL check_vector_data ('AKX_livestemn_xf_exit_p_acc               ', AKX_livestemn_xf_exit_p_acc               )
         CALL check_vector_data ('AKX_deadstemn_xf_exit_p_acc               ', AKX_deadstemn_xf_exit_p_acc               )
         CALL check_vector_data ('AKX_livecrootn_xf_exit_p_acc              ', AKX_livecrootn_xf_exit_p_acc              )
         CALL check_vector_data ('AKX_deadcrootn_xf_exit_p_acc              ', AKX_deadcrootn_xf_exit_p_acc              )
         CALL check_vector_data ('AKX_grainn_xf_exit_p_acc                  ', AKX_grainn_xf_exit_p_acc                  )
      ENDIF
   END SUBROUTINE check_BGCPFTimeVariables
#endif

#endif
END MODULE MOD_BGC_Vars_PFTimeVariables

#endif
! ---------- EOP ------------
