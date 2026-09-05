#include <define.h>

MODULE MOD_BGC_Vars_TimeVariables
#ifdef BGC
!---------------------------------------------------------------------------------------------------------
! !DESCRIPTION
! Define, allocate, and deallocate biogeochemical state variables at patch level.
! Read and write biogeochemical state variables at patch level from/to restart files.

! !ORIGINAL:
! Xingjie Lu, 2022, created the original version

   USE MOD_Precision
   USE MOD_Namelist, only: DEF_USE_SASU, DEF_USE_DiagMatrix
   USE MOD_TimeManager
   IMPLICIT NONE
   SAVE
!------------------------- BGC variables -------------------------------
   real(r8), allocatable :: decomp_cpools_vr  (:,:,:)         ! vertical resolved: soil decomposition (litter, cwd, soil organic matter) carbon pools (gC m-3)
   real(r8), allocatable :: decomp_cpools     (:,:)           ! soil decomposition (litter, cwd, soil) carbon pools (gC m-2)
   real(r8), allocatable :: decomp_cpools_vr_Cap  (:,:,:)     ! vertical resolved: soil decomposition (litter, cwd, soil organic matter) carbon Capacity (gC m-3)
   real(r8), allocatable :: decomp_k          (:,:,:)         ! soil decomposition rate (s-1)
   real(r8), allocatable :: ctrunc_vr         (:,:)           ! currently not used
   real(r8), allocatable :: ctrunc_veg        (:)             ! currently not used
   real(r8), allocatable :: ctrunc_soil       (:)             ! currently not used

   real(r8), allocatable :: t_scalar          (:,:)           ! vertical resolved: soil decomposition temperature scalars
   real(r8), allocatable :: w_scalar          (:,:)           ! vertical resolved: soil decomposition water scalars
   real(r8), allocatable :: o_scalar          (:,:)           ! vertical resolved: soil decomposition oxygen scalars
   real(r8), allocatable :: depth_scalar      (:,:)           ! vertical resolved: soil decomposition depth scalars

!Soil CN diffusion and advection
   real(r8), allocatable :: som_adv_coef             (:,:)    ! vertical resolved: soil organic matter advective flux (m2 s-1)
   real(r8), allocatable :: som_diffus_coef          (:,:)    ! vertical resolved: soil organic matter diffusion flux (m2 s-1)

!Active Layer
   real(r8), allocatable :: altmax                   (:)      ! maximum annual depth of thaw (m)
   real(r8), allocatable :: altmax_lastyear          (:)      ! previous year maximum annual depth of thaw (m)
   integer , allocatable :: altmax_lastyear_indx     (:)      ! previous year maximum annual soil layer of thaw

   real(r8), allocatable :: totlitc                  (:)      ! carbon balance diagnostics: total column litter carbon (gC m-2)
   real(r8), allocatable :: totvegc                  (:)      ! carbon balance diagnostics: total column vegetation carbon (gC m-2)
   real(r8), allocatable :: totsomc                  (:)      ! carbon balance diagnostics: total column soil organic matter carbon (gC m-2)
   real(r8), allocatable :: totcwdc                  (:)      ! carbon balance diagnostics: total column coarse woody debris carbon (gC m-2)
   real(r8), allocatable :: totcolc                  (:)      ! carbon balance diagnostics: total column carbon (veg, soil, litter, et al) (gC m-2)
   real(r8), allocatable :: col_begcb                (:)      ! carbon balance diagnostics: column carbon, begin of time step (gC m-2)
   real(r8), allocatable :: col_endcb                (:)      ! carbon balance diagnostics: column carbon, END of time step (gC m-2)
   real(r8), allocatable :: col_vegbegcb             (:)      ! carbon balance diagnostics: column vegetation carbon, begin of time step (gC m-2)
   real(r8), allocatable :: col_vegendcb             (:)      ! carbon balance diagnostics: column vegetation carbon, end of time step (gC m-2)
   real(r8), allocatable :: col_soilbegcb            (:)      ! carbon balance diagnostics: column soil carbon, begin of time step (gC m-2)
   real(r8), allocatable :: col_soilendcb            (:)      ! carbon balance diagnostics: column soil carbon, end of time step (gC m-2)

   real(r8), allocatable :: totlitn                  (:)      ! nitrogen balance diagnostics: total column litter nitrogen (gN m-2)
   real(r8), allocatable :: totvegn                  (:)      ! nitrogen balance diagnostics: total column vegetation nitrogen (gN m-2)
   real(r8), allocatable :: totsomn                  (:)      ! nitrogen balance diagnostics: total column soil organic matter nitrogen (gN m-2)
   real(r8), allocatable :: totcwdn                  (:)      ! nitrogen balance diagnostics: total column coarse woody debris nitrogen (gN m-2)
   real(r8), allocatable :: totcoln                  (:)      ! nitrogen balance diagnostics: total column nitrogen (veg, soil, litter, et al) (gN m-2)
   real(r8), allocatable :: col_begnb                (:)      ! nitrogen balance diagnostics: column nitrogen, begin of time step (gN m-2)
   real(r8), allocatable :: col_endnb                (:)      ! nitrogen balance diagnostics: column nitrogen, end of time step (gN m-2)
   real(r8), allocatable :: col_vegbegnb             (:)      ! nitrogen balance diagnostics: column vegetation nitrogen, begin of time step (gN m-2)
   real(r8), allocatable :: col_vegendnb             (:)      ! nitrogen balance diagnostics: column vegetation nitrogen, end of time step (gN m-2)
   real(r8), allocatable :: col_soilbegnb            (:)      ! nitrogen balance diagnostics: column soil organic nitrogen, begin of time step (gN m-2)
   real(r8), allocatable :: col_soilendnb            (:)      ! nitrogen balance diagnostics: column soil organic nitrogen, end of time step (gN m-2)
   real(r8), allocatable :: col_sminnbegnb           (:)      ! nitrogen balance diagnostics: column soil mineral nitrogen, begin of time step (gN m-2)
   real(r8), allocatable :: col_sminnendnb           (:)      ! nitrogen balance diagnostics: column soil mineral nitrogen, end of time step (gN m-2)

   real(r8), allocatable :: leafc                    (:)      ! leaf display C (gC m-2)
   real(r8), allocatable :: leafc_storage            (:)      ! leaf storage C (gC m-2)
   real(r8), allocatable :: leafc_xfer               (:)      ! leaf transfer C (gC m-2)
   real(r8), allocatable :: frootc                   (:)      ! fine root display C (gC m-2)
   real(r8), allocatable :: frootc_storage           (:)      ! fine root storage C (gC m-2)
   real(r8), allocatable :: frootc_xfer              (:)      ! fine root transfer C (gC m-2)
   real(r8), allocatable :: livestemc                (:)      ! live stem display C (gC m-2)
   real(r8), allocatable :: livestemc_storage        (:)      ! live stem storage C (gC m-2)
   real(r8), allocatable :: livestemc_xfer           (:)      ! live stem transfer C (gC m-2)
   real(r8), allocatable :: deadstemc                (:)      ! dead stem display C (gC m-2)
   real(r8), allocatable :: deadstemc_storage        (:)      ! dead stem storage C (gC m-2)
   real(r8), allocatable :: deadstemc_xfer           (:)      ! dead stem transfer C (gC m-2)
   real(r8), allocatable :: livecrootc               (:)      ! live coarse root display C (gC m-2)
   real(r8), allocatable :: livecrootc_storage       (:)      ! live coarse root storage C (gC m-2)
   real(r8), allocatable :: livecrootc_xfer          (:)      ! live coarse root transfer C (gC m-2)
   real(r8), allocatable :: deadcrootc               (:)      ! dead coarse root display C (gC m-2)
   real(r8), allocatable :: deadcrootc_storage       (:)      ! dead coarse root storage C (gC m-2)
   real(r8), allocatable :: deadcrootc_xfer          (:)      ! dead coarse root transfer C (gC m-2)
   real(r8), allocatable :: grainc                   (:)      ! grain display C (gC m-2)
   real(r8), allocatable :: grainc_storage           (:)      ! grain storage C (gC m-2)
   real(r8), allocatable :: grainc_xfer              (:)      ! grain transfer C (gC m-2)
   real(r8), allocatable :: xsmrpool                 (:)      ! maintenance respiration storage C (gC m-2)
   real(r8), allocatable :: downreg                  (:)      ! fractional reduction in GPP due to N limitation
   real(r8), allocatable :: cropprod1c               (:)      ! product C (gC m-2)
   real(r8), allocatable :: cropseedc_deficit        (:)      ! crop seed deficit C (gC m-2)

   real(r8), allocatable :: leafn                    (:)      ! leaf display N (gN m-2)
   real(r8), allocatable :: leafn_storage            (:)      ! leaf storage N (gN m-2)
   real(r8), allocatable :: leafn_xfer               (:)      ! leaf transfer N (gN m-2)
   real(r8), allocatable :: frootn                   (:)      ! fine root display N (gN m-2)d
   real(r8), allocatable :: frootn_storage           (:)      ! fine root storage N (gN m-2)d
   real(r8), allocatable :: frootn_xfer              (:)      ! fine root transfer N (gN m-2)d
   real(r8), allocatable :: livestemn                (:)      ! live stem display N (gN m-2)d
   real(r8), allocatable :: livestemn_storage        (:)      ! live stem storage N (gN m-2)d
   real(r8), allocatable :: livestemn_xfer           (:)      ! live stem transfer N (gN m-2)d
   real(r8), allocatable :: deadstemn                (:)      ! dead stem display N (gN m-2)d
   real(r8), allocatable :: deadstemn_storage        (:)      ! dead stem storage N (gN m-2)d
   real(r8), allocatable :: deadstemn_xfer           (:)      ! dead stem transfer N (gN m-2)d
   real(r8), allocatable :: livecrootn               (:)      ! live coarse root display N (gN m-2)
   real(r8), allocatable :: livecrootn_storage       (:)      ! live coarse root storage N (gN m-2)
   real(r8), allocatable :: livecrootn_xfer          (:)      ! live coarse root transfer N (gN m-2)
   real(r8), allocatable :: deadcrootn               (:)      ! dead coarse root display N (gN m-2)
   real(r8), allocatable :: deadcrootn_storage       (:)      ! dead coarse root storage N (gN m-2)
   real(r8), allocatable :: deadcrootn_xfer          (:)      ! dead coarse root transfer N (gN m-2)
   real(r8), allocatable :: grainn                   (:)      ! grain display N (gN m-2)
   real(r8), allocatable :: grainn_storage           (:)      ! grain storage N (gN m-2)
   real(r8), allocatable :: grainn_xfer              (:)      ! grain transfer N (gN m-2)
   real(r8), allocatable :: retransn                 (:)      ! retranslocated N (gN m-2)

   real(r8), allocatable :: leafcCap                 (:)      ! leaf display C capacity (gC m-2)
   real(r8), allocatable :: leafc_storageCap         (:)      ! leaf storage C capacity (gC m-2)
   real(r8), allocatable :: leafc_xferCap            (:)      ! leaf transfer C capacity (gC m-2)
   real(r8), allocatable :: frootcCap                (:)      ! fine root display C capacity (gC m-2)
   real(r8), allocatable :: frootc_storageCap        (:)      ! fine root storage C capacity (gC m-2)
   real(r8), allocatable :: frootc_xferCap           (:)      ! fine root transfer C capacity (gC m-2)
   real(r8), allocatable :: livestemcCap             (:)      ! live stem display C capacity (gC m-2)
   real(r8), allocatable :: livestemc_storageCap     (:)      ! live stem storage C capacity (gC m-2)
   real(r8), allocatable :: livestemc_xferCap        (:)      ! live stem transfer C capacity (gC m-2)
   real(r8), allocatable :: deadstemcCap             (:)      ! dead stem display C capacity (gC m-2)
   real(r8), allocatable :: deadstemc_storageCap     (:)      ! dead stem storage C capacity (gC m-2)
   real(r8), allocatable :: deadstemc_xferCap        (:)      ! dead stem transfer C capacity (gC m-2)
   real(r8), allocatable :: livecrootcCap            (:)      ! live coarse root display C capacity (gC m-2)
   real(r8), allocatable :: livecrootc_storageCap    (:)      ! live coarse root storage C capacity (gC m-2)
   real(r8), allocatable :: livecrootc_xferCap       (:)      ! live coarse root transfer C capacity (gC m-2)
   real(r8), allocatable :: deadcrootcCap            (:)      ! dead coarse root display C capacity (gC m-2)
   real(r8), allocatable :: deadcrootc_storageCap    (:)      ! dead coarse root storage C capacity (gC m-2)
   real(r8), allocatable :: deadcrootc_xferCap       (:)      ! dead coarse root transfer C capacity (gC m-2)

   real(r8), allocatable :: leafnCap                 (:)      ! leaf display N capacity (gN m-2)
   real(r8), allocatable :: leafn_storageCap         (:)      ! leaf storage N capacity (gN m-2)
   real(r8), allocatable :: leafn_xferCap            (:)      ! leaf transfer N capacity (gN m-2)
   real(r8), allocatable :: frootnCap                (:)      ! fine root display N capacity (gN m-2)
   real(r8), allocatable :: frootn_storageCap        (:)      ! fine root storage N capacity (gN m-2)
   real(r8), allocatable :: frootn_xferCap           (:)      ! fine root transfer N capacity (gN m-2)
   real(r8), allocatable :: livestemnCap             (:)      ! live stem display N capacity (gN m-2)
   real(r8), allocatable :: livestemn_storageCap     (:)      ! live stem storage N capacity (gN m-2)
   real(r8), allocatable :: livestemn_xferCap        (:)      ! live stem transfer N capacity (gN m-2)
   real(r8), allocatable :: deadstemnCap             (:)      ! dead stem display N capacity (gN m-2)
   real(r8), allocatable :: deadstemn_storageCap     (:)      ! dead stem storage N capacity (gN m-2)
   real(r8), allocatable :: deadstemn_xferCap        (:)      ! dead stem transfer N capacity (gN m-2)
   real(r8), allocatable :: livecrootnCap            (:)      ! live coarse root display N capacity (gN m-2)
   real(r8), allocatable :: livecrootn_storageCap    (:)      ! live coarse root storage N capacity (gN m-2)
   real(r8), allocatable :: livecrootn_xferCap       (:)      ! live coarse root transfer N capacity (gN m-2)
   real(r8), allocatable :: deadcrootnCap            (:)      ! dead coarse root display N capacity (gN m-2)
   real(r8), allocatable :: deadcrootn_storageCap    (:)      ! dead coarse root storage N capacity (gN m-2)
   real(r8), allocatable :: deadcrootn_xferCap       (:)      ! dead coarse root transfer N capacity (gN m-2)

   real(r8), allocatable :: decomp_npools_vr         (:,:,:)  ! vertical resolved: soil decomposition (litter, cwd, soil) nitrogen (gN m-3)
   real(r8), allocatable :: decomp_npools            (:,:)    ! soil decomposition (litter, cwd, soil) nitrogen (gN m-2)
   real(r8), allocatable :: decomp_npools_vr_Cap     (:,:,:)  ! vertical resolved: soil decomposition (litter, cwd, soil organic matter) carbon Capacity (gC m-3)
   real(r8), allocatable :: totsoiln_vr              (:,:)  ! vertical resolved: total soil nitrogen (%: gN/gSoil*100)
   real(r8), allocatable :: ntrunc_vr                (:,:)    ! currently not used
   real(r8), allocatable :: ntrunc_veg               (:)      ! currently not used
   real(r8), allocatable :: ntrunc_soil              (:)      ! currently not used

   real(r8), allocatable :: sminn_vr                 (:,:)    ! vertical resolved: soil mineral nitrogen (gN m-3)
   real(r8), allocatable :: smin_no3_vr              (:,:)    ! vertical resolved: soil mineral NO3 (gN m-3)
   real(r8), allocatable :: smin_nh4_vr              (:,:)    ! vertical resolved: soil mineral NH4 (gN m-3)
   real(r8), allocatable :: sminn                    (:)      ! soil mineral nitrogen (gN m-2)
   real(r8), allocatable :: ndep                     (:)      ! atmospheric nitrogen deposition (gN m-2)

   real(r8), allocatable :: to2_decomp_depth_unsat   (:,:)    ! vertical resolved: O2 soil consumption from heterotrophic respiration and autotrophic respiration (mol m-3 s-1)
   real(r8), allocatable :: tconc_o2_unsat           (:,:)    ! vertical resolved: O2 soil consumption (mol m-3 s-1)

   real(r8), allocatable :: ndep_prof                (:,:)    ! vertical resolved: atmospheric N deposition input to soil (m-1)
   real(r8), allocatable :: nfixation_prof           (:,:)    ! vertical resolved: N fixation input to soil (m-1)

   real(r8), allocatable :: cn_decomp_pools          (:,:,:)  ! vertical resolved: c:n ratios of each decomposition pools
   real(r8), allocatable :: fpi_vr                   (:,:)    ! vertical resolved: actual immobilization N :potential immobilization N
   real(r8), allocatable :: fpi                      (:)      ! actual immobilization N : potential immobilization N
   real(r8), allocatable :: fpg                      (:)      ! actual plant uptake N : plant potential need N

   real(r8), allocatable :: cropf                    (:)      !
   real(r8), allocatable :: lfwt                     (:)      !
   real(r8), allocatable :: fuelc                    (:)      !
   real(r8), allocatable :: fuelc_crop               (:)      !
   real(r8), allocatable :: fsr                      (:)      !
   real(r8), allocatable :: fd                       (:)      !
   real(r8), allocatable :: rootc                    (:)      !
   real(r8), allocatable :: lgdp                     (:)      !
   real(r8), allocatable :: lgdp1                    (:)      !
   real(r8), allocatable :: lpop                     (:)      !
   real(r8), allocatable :: wtlf                     (:)      !
   real(r8), allocatable :: trotr1                   (:)      !
   real(r8), allocatable :: trotr2                   (:)      !
   real(r8), allocatable :: hdm_lf                   (:)      !
   real(r8), allocatable :: lnfm                     (:)      !
   real(r8), allocatable :: baf_crop                 (:)      !
   real(r8), allocatable :: baf_peatf                (:)      !
   real(r8), allocatable :: farea_burned             (:)      ! total fractional area burned (s-1)
   real(r8), allocatable :: nfire                    (:)      ! fire counts (count km-2 s-1)
   real(r8), allocatable :: fsat                     (:)      !
   real(r8), allocatable :: prec10                   (:)      ! 10-day running mean of total precipitation (mm -1)
   real(r8), allocatable :: prec60                   (:)      ! 60-day running mean of total      precipitation  (mm -1)
   real(r8), allocatable :: prec365                  (:)      ! 365-day running mean of tota     l precipitation (mm -1)
   real(r8), allocatable :: prec_today               (:)      ! today's daily precipitation (mm -1)
   real(r8), allocatable :: prec_daily               (:,:)    ! daily total precipitation (mm -1)
   real(r8), allocatable :: wf2                      (:)      ! soil moisture (K)
   real(r8), allocatable :: tsoi17                   (:)      ! soil temperature (cm3 cm-3)
   real(r8), allocatable :: rh30                     (:)      ! 30-day running mean of relative humidity (%)
   real(r8), allocatable :: accumnstep               (:)      ! timestep accumulator

   real(r8), allocatable :: dayl                     (:)      ! day length (s)
   real(r8), allocatable :: prev_dayl                (:)      ! day length from previous day (s)

!-------------BGC/SASU variables---------------------------
   real(r8), allocatable :: decomp0_cpools_vr           (:,:,:)    ! SASU spinup diagnostics vertical-resolved: soil decomposition (litter, cwd, soil organic matter) carbon pools (gC m-3)
   real(r8), allocatable :: I_met_c_vr_acc              (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated input to metabolic litter C (gC m-3)
   real(r8), allocatable :: I_cel_c_vr_acc              (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated input to cellulosic litter C (gC m-3)
   real(r8), allocatable :: I_lig_c_vr_acc              (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated input to lignin litter C (gC m-3)
   real(r8), allocatable :: I_cwd_c_vr_acc              (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated input to coarse woody debris C (gC m-3)
   real(r8), allocatable :: AKX_met_to_soil1_c_vr_acc   (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux from metabolic litter C to active soil organic matter C (gC m-3)
   real(r8), allocatable :: AKX_cel_to_soil1_c_vr_acc   (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux from cellulosic litter C to active soil organic matter C (gC m-3)
   real(r8), allocatable :: AKX_lig_to_soil2_c_vr_acc   (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux from lignin litter C to slow soil organic matter C (gC m-3)
   real(r8), allocatable :: AKX_soil1_to_soil2_c_vr_acc (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux from active soil organic matter C to slow soil organic matter C (gC m-3)
   real(r8), allocatable :: AKX_cwd_to_cel_c_vr_acc     (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux from coarse woody debris C to cellulosic litter C (gC m-3)
   real(r8), allocatable :: AKX_cwd_to_lig_c_vr_acc     (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux from coarse woody debris C to lignin litter C (gC m-3)
   real(r8), allocatable :: AKX_soil1_to_soil3_c_vr_acc (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux from active soil organic matter C to passive soil organic matter C (gC m-3)
   real(r8), allocatable :: AKX_soil2_to_soil1_c_vr_acc (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux from slow soil organic matter C to active soil organic matter C (gC m-3)
   real(r8), allocatable :: AKX_soil2_to_soil3_c_vr_acc (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux from slow soil organic matter C to passive soil organic matter C (gC m-3)
   real(r8), allocatable :: AKX_soil3_to_soil1_c_vr_acc (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux from passive soil organic matter C to active soil organic matter C (gC m-3)
   real(r8), allocatable :: AKX_met_exit_c_vr_acc       (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux exiting from metabolic litter C (gC m-3)
   real(r8), allocatable :: AKX_cel_exit_c_vr_acc       (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux exiting from cellulosic litter C (gC m-3)
   real(r8), allocatable :: AKX_lig_exit_c_vr_acc       (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux exiting from lignin litter C (gC m-3)
   real(r8), allocatable :: AKX_cwd_exit_c_vr_acc       (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux exiting from coarse woody debris C (gC m-3)
   real(r8), allocatable :: AKX_soil1_exit_c_vr_acc     (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux exiting from active soil organic matter C  (gC m-3)
   real(r8), allocatable :: AKX_soil2_exit_c_vr_acc     (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux exiting from slow soil organic matter C (gC m-3)
   real(r8), allocatable :: AKX_soil3_exit_c_vr_acc     (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux exiting from passive soil organic matter C (gC m-3)

   real(r8), allocatable :: decomp0_npools_vr           (:,:,:)    ! SASU spinup diagnostics vertical-resolved: soil decomposition (litter, cwd, soil organic matter) carbon pools (gN m-3)
   real(r8), allocatable :: I_met_n_vr_acc              (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated input to metabolic litter N (gN m-3)
   real(r8), allocatable :: I_cel_n_vr_acc              (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated input to cellulosic litter N (gN m-3)
   real(r8), allocatable :: I_lig_n_vr_acc              (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated input to lignin litter N (gN m-3)
   real(r8), allocatable :: I_cwd_n_vr_acc              (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated input to coarse woody debris N (gN m-3)
   real(r8), allocatable :: AKX_met_to_soil1_n_vr_acc   (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux from metabolic litter N to active soil organic matter N (gN m-3)
   real(r8), allocatable :: AKX_cel_to_soil1_n_vr_acc   (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux from cellulosic litter N to active soil organic matter N (gN m-3)
   real(r8), allocatable :: AKX_lig_to_soil2_n_vr_acc   (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux from lignin litter N to slow soil organic matter N (gN m-3)
   real(r8), allocatable :: AKX_soil1_to_soil2_n_vr_acc (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux from active soil organic matter N to slow soil organic matter N (gN m-3)
   real(r8), allocatable :: AKX_cwd_to_cel_n_vr_acc     (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux from coarse woody debris N to cellulosic litter N (gN m-3)
   real(r8), allocatable :: AKX_cwd_to_lig_n_vr_acc     (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux from coarse woody debris N to lignin litter N (gN m-3)
   real(r8), allocatable :: AKX_soil1_to_soil3_n_vr_acc (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux from active soil organic matter N to passive soil organic matter N (gN m-3)
   real(r8), allocatable :: AKX_soil2_to_soil1_n_vr_acc (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux from slow soil organic matter N to active soil organic matter N (gN m-3)
   real(r8), allocatable :: AKX_soil2_to_soil3_n_vr_acc (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux from slow soil organic matter N to passive soil organic matter N (gN m-3)
   real(r8), allocatable :: AKX_soil3_to_soil1_n_vr_acc (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux from passive soil organic matter N to active soil organic matter N (gN m-3)
   real(r8), allocatable :: AKX_met_exit_n_vr_acc       (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux exiting from metabolic litter N (gN m-3)
   real(r8), allocatable :: AKX_cel_exit_n_vr_acc       (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux exiting from cellulosic litter N (gN m-3)
   real(r8), allocatable :: AKX_lig_exit_n_vr_acc       (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux exiting from lignin litter N (gN m-3)
   real(r8), allocatable :: AKX_cwd_exit_n_vr_acc       (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux exiting from coarse woody debris N (gN m-3)
   real(r8), allocatable :: AKX_soil1_exit_n_vr_acc     (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux exiting from active soil organic matter N  (gN m-3)
   real(r8), allocatable :: AKX_soil2_exit_n_vr_acc     (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux exiting from slow soil organic matter N (gN m-3)
   real(r8), allocatable :: AKX_soil3_exit_n_vr_acc     (:,:)      ! SASU spinup diagnostics vertical-resolved: accumulated flux exiting from passive soil organic matter N (gN m-3)

   real(r8), allocatable :: diagVX_c_vr_acc             (:,:,:)    ! SASU spinup diagnostics vertical-resolved: accumulated carbon EXIT flux due to the vertical mixing in soil and litter pools (gC m-3)
   real(r8), allocatable :: upperVX_c_vr_acc            (:,:,:)    ! SASU spinup diagnostics vertical-resolved: accumulated carbon upward flux due to the vertical mixing in soil and litter pools (gC m-3)
   real(r8), allocatable :: lowerVX_c_vr_acc            (:,:,:)    ! SASU spinup diagnostics vertical-resolved: accumulated carbon downward flux due to the vertical mixing in soil and litter pools (gC m-3)
   real(r8), allocatable :: diagVX_n_vr_acc             (:,:,:)    ! SASU spinup diagnostics vertical-resolved: accumulated nitrogen EXIT flux due to the vertical mixing in soil and litter pools (gN m-3)
   real(r8), allocatable :: upperVX_n_vr_acc            (:,:,:)    ! SASU spinup diagnostics vertical-resolved: accumulated nitrogen upward flux due to the vertical mixing in soil and litter pools (gN m-3)
   real(r8), allocatable :: lowerVX_n_vr_acc            (:,:,:)    ! SASU spinup diagnostics vertical-resolved: accumulated nitrogen downward flux due to the vertical mixing in soil and litter pools (gN m-3)
   logical , allocatable :: skip_balance_check          (:)        ! When we estimate the steady state and update the actcual pool with steady state in SASU, the CN balance check is expected to fail. &
                                                                      ! Skip the balance check at END of the year when SASU is on
#ifdef CROP
   real(r8), allocatable :: cphase              (:) ! crop phasecrop phase
   real(r8), allocatable :: vf                  (:) ! vernalization response
   real(r8), allocatable :: gddplant            (:) ! gdd since planting (ddays)
   real(r8), allocatable :: gddmaturity         (:) ! gdd needed to harvest (ddays)
   real(r8), allocatable :: hui                 (:) ! heat unit index
   real(r8), allocatable :: huiswheat           (:) ! heat unit index  (rainfed spring wheat)
   real(r8), allocatable :: pdcorn              (:) ! planting date of corn
   real(r8), allocatable :: pdswheat            (:) ! planting date of spring wheat
   real(r8), allocatable :: pdwwheat            (:) ! planting date of winter wheat
   real(r8), allocatable :: pdsoybean           (:) ! planting date of soybean
   real(r8), allocatable :: pdcotton            (:) ! planting date of cotton
   real(r8), allocatable :: pdrice1             (:) ! planting date of rice1
   real(r8), allocatable :: pdrice2             (:) ! planting date of rice2
   real(r8), allocatable :: pdsugarcane         (:) ! planting date of sugarcane
   real(r8), allocatable :: plantdate           (:) ! planting date
   real(r8), allocatable :: manunitro           (:) ! nitrogen fertilizer for corn (gN m-2)
   real(r8), allocatable :: fertnitro_corn      (:) ! nitrogen fertilizer for corn (gN m-2)
   real(r8), allocatable :: fertnitro_swheat    (:) ! nitrogen fertilizer for spring wheat (gN m-2)
   real(r8), allocatable :: fertnitro_wwheat    (:) ! nitrogen fertilizer for winter wheat (gN m-2)
   real(r8), allocatable :: fertnitro_soybean   (:) ! nitrogen fertilizer for soybean (gN m-2)
   real(r8), allocatable :: fertnitro_cotton    (:) ! nitrogen fertilizer for cotton (gN m-2)
   real(r8), allocatable :: fertnitro_rice1     (:) ! nitrogen fertilizer for rice1 (gN m-2)
   real(r8), allocatable :: fertnitro_rice2     (:) ! nitrogen fertilizer for rice2 (gN m-2)
   real(r8), allocatable :: fertnitro_sugarcane (:) ! nitrogen fertilizer for sugarcane (gN m-2)
#endif
   real(r8), allocatable :: lag_npp             (:) !!! lagged net primary production (gC m-2)
!------------------------------------------------------

! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: allocate_BGCTimeVariables
   PUBLIC :: deallocate_BGCTimeVariables
   PUBLIC :: READ_BGCTimeVariables
   PUBLIC :: WRITE_BGCTimeVariables
#ifdef RangeCheck
   PUBLIC :: check_BGCTimeVariables
#endif

! PRIVATE MEMBER FUNCTIONS:


!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------

   SUBROUTINE allocate_BGCTimeVariables
! --------------------------------------------------------------------
! Allocates memory for CoLM 1d [numpatch] variables
! ------------------------------------------------------

   USE MOD_Precision
   USE MOD_Vars_Global
   USE MOD_MPAS_MPI
   USE MOD_LandPatch, only: numpatch
   IMPLICIT NONE


      IF (.true.) THEN

         IF (numpatch > 0) THEN

    ! bgc variables
            allocate (decomp_cpools_vr             (nl_soil_full,ndecomp_pools,numpatch)) ; decomp_cpools_vr  (:,:,:) = spval
            allocate (decomp_cpools                (ndecomp_pools,numpatch))              ; decomp_cpools       (:,:) = spval
            allocate (decomp_cpools_vr_Cap         (nl_soil_full,ndecomp_pools,numpatch)) ; decomp_cpools_vr_Cap(:,:,:) = spval
            allocate (ctrunc_vr                    (nl_soil,numpatch))                    ; ctrunc_vr           (:,:) = spval
            allocate (ctrunc_veg                   (numpatch))                            ; ctrunc_veg            (:) = spval
            allocate (ctrunc_soil                  (numpatch))                            ; ctrunc_soil           (:) = spval
            allocate (decomp_k                     (nl_soil_full,ndecomp_pools,numpatch)) ; decomp_k          (:,:,:) = spval

            allocate (t_scalar                     (nl_soil,numpatch))                    ; t_scalar            (:,:) = spval
            allocate (w_scalar                     (nl_soil,numpatch))                    ; w_scalar            (:,:) = spval
            allocate (o_scalar                     (nl_soil,numpatch))                    ; o_scalar            (:,:) = spval
            allocate (depth_scalar                 (nl_soil,numpatch))                    ; depth_scalar        (:,:) = spval

            allocate (som_adv_coef                 (nl_soil_full,numpatch))               ; som_adv_coef        (:,:) = spval
            allocate (som_diffus_coef              (nl_soil_full,numpatch))               ; som_diffus_coef     (:,:) = spval

            allocate (altmax                       (numpatch))                            ; altmax                (:) = spval
            allocate (altmax_lastyear              (numpatch))                            ; altmax_lastyear       (:) = spval
            allocate (altmax_lastyear_indx         (numpatch))                            ; altmax_lastyear_indx  (:) = spval_i4

            allocate (totlitc                      (numpatch))                            ; totlitc               (:) = spval
            allocate (totvegc                      (numpatch))                            ; totvegc               (:) = spval
            allocate (totsomc                      (numpatch))                            ; totsomc               (:) = spval
            allocate (totcwdc                      (numpatch))                            ; totcwdc               (:) = spval
            allocate (totcolc                      (numpatch))                            ; totcolc               (:) = spval
            allocate (col_begcb                    (numpatch))                            ; col_begcb             (:) = spval
            allocate (col_endcb                    (numpatch))                            ; col_endcb             (:) = spval
            allocate (col_vegbegcb                 (numpatch))                            ; col_vegbegcb          (:) = spval
            allocate (col_vegendcb                 (numpatch))                            ; col_vegendcb          (:) = spval
            allocate (col_soilbegcb                (numpatch))                            ; col_soilbegcb         (:) = spval
            allocate (col_soilendcb                (numpatch))                            ; col_soilendcb         (:) = spval

            allocate (totlitn                      (numpatch))                            ; totlitn               (:) = spval
            allocate (totvegn                      (numpatch))                            ; totvegn               (:) = spval
            allocate (totsomn                      (numpatch))                            ; totsomn               (:) = spval
            allocate (totcwdn                      (numpatch))                            ; totcwdn               (:) = spval
            allocate (totcoln                      (numpatch))                            ; totcoln               (:) = spval
            allocate (col_begnb                    (numpatch))                            ; col_begnb             (:) = spval
            allocate (col_endnb                    (numpatch))                            ; col_endnb             (:) = spval
            allocate (col_vegbegnb                 (numpatch))                            ; col_vegbegnb          (:) = spval
            allocate (col_vegendnb                 (numpatch))                            ; col_vegendnb          (:) = spval
            allocate (col_soilbegnb                (numpatch))                            ; col_soilbegnb         (:) = spval
            allocate (col_soilendnb                (numpatch))                            ; col_soilendnb         (:) = spval
            allocate (col_sminnbegnb               (numpatch))                            ; col_sminnbegnb        (:) = spval
            allocate (col_sminnendnb               (numpatch))                            ; col_sminnendnb        (:) = spval

            allocate (leafc                        (numpatch))                            ; leafc                 (:) = spval
            allocate (leafc_storage                (numpatch))                            ; leafc_storage         (:) = spval
            allocate (leafc_xfer                   (numpatch))                            ; leafc_xfer            (:) = spval
            allocate (frootc                       (numpatch))                            ; frootc                (:) = spval
            allocate (frootc_storage               (numpatch))                            ; frootc_storage        (:) = spval
            allocate (frootc_xfer                  (numpatch))                            ; frootc_xfer           (:) = spval
            allocate (livestemc                    (numpatch))                            ; livestemc             (:) = spval
            allocate (livestemc_storage            (numpatch))                            ; livestemc_storage     (:) = spval
            allocate (livestemc_xfer               (numpatch))                            ; livestemc_xfer        (:) = spval
            allocate (deadstemc                    (numpatch))                            ; deadstemc             (:) = spval
            allocate (deadstemc_storage            (numpatch))                            ; deadstemc_storage     (:) = spval
            allocate (deadstemc_xfer               (numpatch))                            ; deadstemc_xfer        (:) = spval
            allocate (livecrootc                   (numpatch))                            ; livecrootc            (:) = spval
            allocate (livecrootc_storage           (numpatch))                            ; livecrootc_storage    (:) = spval
            allocate (livecrootc_xfer              (numpatch))                            ; livecrootc_xfer       (:) = spval
            allocate (deadcrootc                   (numpatch))                            ; deadcrootc            (:) = spval
            allocate (deadcrootc_storage           (numpatch))                            ; deadcrootc_storage    (:) = spval
            allocate (deadcrootc_xfer              (numpatch))                            ; deadcrootc_xfer       (:) = spval
            allocate (grainc                       (numpatch))                            ; grainc                (:) = spval
            allocate (grainc_storage               (numpatch))                            ; grainc_storage        (:) = spval
            allocate (grainc_xfer                  (numpatch))                            ; grainc_xfer           (:) = spval
            allocate (xsmrpool                     (numpatch))                            ; xsmrpool              (:) = spval
            allocate (downreg                      (numpatch))                            ; downreg               (:) = spval
            allocate (cropprod1c                   (numpatch))                            ; cropprod1c            (:) = spval
            allocate (cropseedc_deficit            (numpatch))                            ; cropseedc_deficit     (:) = spval

            allocate (leafn                        (numpatch))                            ; leafn                 (:) = spval
            allocate (leafn_storage                (numpatch))                            ; leafn_storage         (:) = spval
            allocate (leafn_xfer                   (numpatch))                            ; leafn_xfer            (:) = spval
            allocate (frootn                       (numpatch))                            ; frootn                (:) = spval
            allocate (frootn_storage               (numpatch))                            ; frootn_storage        (:) = spval
            allocate (frootn_xfer                  (numpatch))                            ; frootn_xfer           (:) = spval
            allocate (livestemn                    (numpatch))                            ; livestemn             (:) = spval
            allocate (livestemn_storage            (numpatch))                            ; livestemn_storage     (:) = spval
            allocate (livestemn_xfer               (numpatch))                            ; livestemn_xfer        (:) = spval
            allocate (deadstemn                    (numpatch))                            ; deadstemn             (:) = spval
            allocate (deadstemn_storage            (numpatch))                            ; deadstemn_storage     (:) = spval
            allocate (deadstemn_xfer               (numpatch))                            ; deadstemn_xfer        (:) = spval
            allocate (livecrootn                   (numpatch))                            ; livecrootn            (:) = spval
            allocate (livecrootn_storage           (numpatch))                            ; livecrootn_storage    (:) = spval
            allocate (livecrootn_xfer              (numpatch))                            ; livecrootn_xfer       (:) = spval
            allocate (deadcrootn                   (numpatch))                            ; deadcrootn            (:) = spval
            allocate (deadcrootn_storage           (numpatch))                            ; deadcrootn_storage    (:) = spval
            allocate (deadcrootn_xfer              (numpatch))                            ; deadcrootn_xfer       (:) = spval
            allocate (grainn                       (numpatch))                            ; grainn                (:) = spval
            allocate (grainn_storage               (numpatch))                            ; grainn_storage        (:) = spval
            allocate (grainn_xfer                  (numpatch))                            ; grainn_xfer           (:) = spval
            allocate (retransn                     (numpatch))                            ; retransn              (:) = spval

            allocate (leafcCap                     (numpatch))                            ; leafcCap              (:) = spval
            allocate (leafc_storageCap             (numpatch))                            ; leafc_storageCap      (:) = spval
            allocate (leafc_xferCap                (numpatch))                            ; leafc_xferCap         (:) = spval
            allocate (frootcCap                    (numpatch))                            ; frootcCap             (:) = spval
            allocate (frootc_storageCap            (numpatch))                            ; frootc_storageCap     (:) = spval
            allocate (frootc_xferCap               (numpatch))                            ; frootc_xferCap        (:) = spval
            allocate (livestemcCap                 (numpatch))                            ; livestemcCap          (:) = spval
            allocate (livestemc_storageCap         (numpatch))                            ; livestemc_storageCap  (:) = spval
            allocate (livestemc_xferCap            (numpatch))                            ; livestemc_xferCap     (:) = spval
            allocate (deadstemcCap                 (numpatch))                            ; deadstemcCap          (:) = spval
            allocate (deadstemc_storageCap         (numpatch))                            ; deadstemc_storageCap  (:) = spval
            allocate (deadstemc_xferCap            (numpatch))                            ; deadstemc_xferCap     (:) = spval
            allocate (livecrootcCap                (numpatch))                            ; livecrootcCap         (:) = spval
            allocate (livecrootc_storageCap        (numpatch))                            ; livecrootc_storageCap (:) = spval
            allocate (livecrootc_xferCap           (numpatch))                            ; livecrootc_xferCap    (:) = spval
            allocate (deadcrootcCap                (numpatch))                            ; deadcrootcCap         (:) = spval
            allocate (deadcrootc_storageCap        (numpatch))                            ; deadcrootc_storageCap (:) = spval
            allocate (deadcrootc_xferCap           (numpatch))                            ; deadcrootc_xferCap    (:) = spval

            allocate (leafnCap                     (numpatch))                            ; leafnCap              (:) = spval
            allocate (leafn_storageCap             (numpatch))                            ; leafn_storageCap      (:) = spval
            allocate (leafn_xferCap                (numpatch))                            ; leafn_xferCap         (:) = spval
            allocate (frootnCap                    (numpatch))                            ; frootnCap             (:) = spval
            allocate (frootn_storageCap            (numpatch))                            ; frootn_storageCap     (:) = spval
            allocate (frootn_xferCap               (numpatch))                            ; frootn_xferCap        (:) = spval
            allocate (livestemnCap                 (numpatch))                            ; livestemnCap          (:) = spval
            allocate (livestemn_storageCap         (numpatch))                            ; livestemn_storageCap  (:) = spval
            allocate (livestemn_xferCap            (numpatch))                            ; livestemn_xferCap     (:) = spval
            allocate (deadstemnCap                 (numpatch))                            ; deadstemnCap          (:) = spval
            allocate (deadstemn_storageCap         (numpatch))                            ; deadstemn_storageCap  (:) = spval
            allocate (deadstemn_xferCap            (numpatch))                            ; deadstemn_xferCap     (:) = spval
            allocate (livecrootnCap                (numpatch))                            ; livecrootnCap         (:) = spval
            allocate (livecrootn_storageCap        (numpatch))                            ; livecrootn_storageCap (:) = spval
            allocate (livecrootn_xferCap           (numpatch))                            ; livecrootn_xferCap    (:) = spval
            allocate (deadcrootnCap                (numpatch))                            ; deadcrootnCap         (:) = spval
            allocate (deadcrootn_storageCap        (numpatch))                            ; deadcrootn_storageCap (:) = spval
            allocate (deadcrootn_xferCap           (numpatch))                            ; deadcrootn_xferCap    (:) = spval

            allocate (decomp_npools_vr             (nl_soil_full,ndecomp_pools,numpatch)) ; decomp_npools_vr  (:,:,:) = spval
            allocate (decomp_npools                (ndecomp_pools,numpatch))              ; decomp_npools       (:,:) = spval
            allocate (decomp_npools_vr_Cap         (nl_soil_full,ndecomp_pools,numpatch)) ; decomp_npools_vr_Cap(:,:,:) = spval
            allocate (totsoiln_vr                  (nl_soil,numpatch))                    ; totsoiln_vr         (:,:) = spval
            allocate (ntrunc_vr                    (nl_soil,numpatch))                    ; ntrunc_vr           (:,:) = spval
            allocate (ntrunc_veg                   (numpatch))                            ; ntrunc_veg            (:) = spval
            allocate (ntrunc_soil                  (numpatch))                            ; ntrunc_soil           (:) = spval
            allocate (sminn_vr                     (nl_soil,numpatch))                    ; sminn_vr            (:,:) = spval
            allocate (smin_no3_vr                  (nl_soil,numpatch))                    ; smin_no3_vr         (:,:) = spval
            allocate (smin_nh4_vr                  (nl_soil,numpatch))                    ; smin_nh4_vr         (:,:) = spval
            allocate (sminn                        (numpatch))                            ; sminn                 (:) = spval
            allocate (ndep                         (numpatch))                            ; ndep                  (:) = spval

            allocate (to2_decomp_depth_unsat       (nl_soil,numpatch))                    ; to2_decomp_depth_unsat (:,:) = spval
            allocate (tconc_o2_unsat               (nl_soil,numpatch))                    ; tconc_o2_unsat         (:,:) = spval

            allocate (ndep_prof                    (nl_soil,numpatch))                    ; ndep_prof           (:,:) = spval
            allocate (nfixation_prof               (nl_soil,numpatch))                    ; nfixation_prof      (:,:) = spval

            allocate (cn_decomp_pools              (nl_soil,ndecomp_pools,numpatch))      ; cn_decomp_pools   (:,:,:) = spval
            allocate (fpi_vr                       (nl_soil,numpatch))                    ; fpi_vr              (:,:) = spval
            allocate (fpi                          (numpatch))                            ; fpi                   (:) = spval
            allocate (fpg                          (numpatch))                            ; fpg                   (:) = spval

            allocate (cropf                        (numpatch))                            ; cropf                 (:) = spval
            allocate (lfwt                         (numpatch))                            ; lfwt                  (:) = spval
            allocate (fuelc                        (numpatch))                            ; fuelc                 (:) = spval
            allocate (fuelc_crop                   (numpatch))                            ; fuelc_crop            (:) = spval
            allocate (fsr                          (numpatch))                            ; fsr                   (:) = spval
            allocate (fd                           (numpatch))                            ; fd                    (:) = spval
            allocate (rootc                        (numpatch))                            ; rootc                 (:) = spval
            allocate (lgdp                         (numpatch))                            ; lgdp                  (:) = spval
            allocate (lgdp1                        (numpatch))                            ; lgdp1                 (:) = spval
            allocate (lpop                         (numpatch))                            ; lpop                  (:) = spval
            allocate (wtlf                         (numpatch))                            ; wtlf                  (:) = spval
            allocate (trotr1                       (numpatch))                            ; trotr1                (:) = spval
            allocate (trotr2                       (numpatch))                            ; trotr2                (:) = spval
            allocate (hdm_lf                       (numpatch))                            ; hdm_lf                (:) = spval
            allocate (lnfm                         (numpatch))                            ; lnfm                  (:) = spval
            allocate (baf_crop                     (numpatch))                            ; baf_crop              (:) = spval
            allocate (baf_peatf                    (numpatch))                            ; baf_peatf             (:) = spval
            allocate (farea_burned                 (numpatch))                            ; farea_burned          (:) = spval
            allocate (nfire                        (numpatch))                            ; nfire                 (:) = spval
            allocate (fsat                         (numpatch))                            ; fsat                  (:) = spval
            allocate (prec10                       (numpatch))                            ; prec10                (:) = spval
            allocate (prec60                       (numpatch))                            ; prec60                (:) = spval
            allocate (prec365                      (numpatch))                            ; prec365               (:) = spval
            allocate (prec_today                   (numpatch))                            ; prec_today            (:) = spval
            allocate (prec_daily               (365,numpatch))                            ; prec_daily          (:,:) = spval! daily total precipitation      [mm/day]
            allocate (wf2                          (numpatch))                            ; wf2                   (:) = spval
            allocate (tsoi17                       (numpatch))                            ; tsoi17                (:) = spval
            allocate (rh30                         (numpatch))                            ; rh30                  (:) = spval
            allocate (accumnstep                   (numpatch))                            ; accumnstep            (:) = spval
                                ;
            allocate (dayl                         (numpatch))                            ; dayl                  (:) = spval
            allocate (prev_dayl                    (numpatch))                            ; prev_dayl             (:) = spval

    !---------------------------SASU variables--------------------------------------
            allocate (decomp0_cpools_vr            (nl_soil,ndecomp_pools,numpatch))      ; decomp0_cpools_vr          (:,:,:) = spval
            allocate (I_met_c_vr_acc               (nl_soil,numpatch))                    ; I_met_c_vr_acc               (:,:) = spval
            allocate (I_cel_c_vr_acc               (nl_soil,numpatch))                    ; I_cel_c_vr_acc               (:,:) = spval
            allocate (I_lig_c_vr_acc               (nl_soil,numpatch))                    ; I_lig_c_vr_acc               (:,:) = spval
            allocate (I_cwd_c_vr_acc               (nl_soil,numpatch))                    ; I_cwd_c_vr_acc               (:,:) = spval
            allocate (AKX_met_to_soil1_c_vr_acc    (nl_soil,numpatch))                    ; AKX_met_to_soil1_c_vr_acc    (:,:) = spval
            allocate (AKX_cel_to_soil1_c_vr_acc    (nl_soil,numpatch))                    ; AKX_cel_to_soil1_c_vr_acc    (:,:) = spval
            allocate (AKX_lig_to_soil2_c_vr_acc    (nl_soil,numpatch))                    ; AKX_lig_to_soil2_c_vr_acc    (:,:) = spval
            allocate (AKX_soil1_to_soil2_c_vr_acc  (nl_soil,numpatch))                    ; AKX_soil1_to_soil2_c_vr_acc  (:,:) = spval
            allocate (AKX_cwd_to_cel_c_vr_acc      (nl_soil,numpatch))                    ; AKX_cwd_to_cel_c_vr_acc      (:,:) = spval
            allocate (AKX_cwd_to_lig_c_vr_acc      (nl_soil,numpatch))                    ; AKX_cwd_to_lig_c_vr_acc      (:,:) = spval
            allocate (AKX_soil1_to_soil3_c_vr_acc  (nl_soil,numpatch))                    ; AKX_soil1_to_soil3_c_vr_acc  (:,:) = spval
            allocate (AKX_soil2_to_soil1_c_vr_acc  (nl_soil,numpatch))                    ; AKX_soil2_to_soil1_c_vr_acc  (:,:) = spval
            allocate (AKX_soil2_to_soil3_c_vr_acc  (nl_soil,numpatch))                    ; AKX_soil2_to_soil3_c_vr_acc  (:,:) = spval
            allocate (AKX_soil3_to_soil1_c_vr_acc  (nl_soil,numpatch))                    ; AKX_soil3_to_soil1_c_vr_acc  (:,:) = spval
            allocate (AKX_met_exit_c_vr_acc        (nl_soil,numpatch))                    ; AKX_met_exit_c_vr_acc        (:,:) = spval
            allocate (AKX_cel_exit_c_vr_acc        (nl_soil,numpatch))                    ; AKX_cel_exit_c_vr_acc        (:,:) = spval
            allocate (AKX_lig_exit_c_vr_acc        (nl_soil,numpatch))                    ; AKX_lig_exit_c_vr_acc        (:,:) = spval
            allocate (AKX_cwd_exit_c_vr_acc        (nl_soil,numpatch))                    ; AKX_cwd_exit_c_vr_acc        (:,:) = spval
            allocate (AKX_soil1_exit_c_vr_acc      (nl_soil,numpatch))                    ; AKX_soil1_exit_c_vr_acc      (:,:) = spval
            allocate (AKX_soil2_exit_c_vr_acc      (nl_soil,numpatch))                    ; AKX_soil2_exit_c_vr_acc      (:,:) = spval
            allocate (AKX_soil3_exit_c_vr_acc      (nl_soil,numpatch))                    ; AKX_soil3_exit_c_vr_acc      (:,:) = spval

            allocate (decomp0_npools_vr            (nl_soil,ndecomp_pools,numpatch))      ; decomp0_npools_vr          (:,:,:) = spval
            allocate (I_met_n_vr_acc               (nl_soil,numpatch))                    ; I_met_n_vr_acc               (:,:) = spval
            allocate (I_cel_n_vr_acc               (nl_soil,numpatch))                    ; I_cel_n_vr_acc               (:,:) = spval
            allocate (I_lig_n_vr_acc               (nl_soil,numpatch))                    ; I_lig_n_vr_acc               (:,:) = spval
            allocate (I_cwd_n_vr_acc               (nl_soil,numpatch))                    ; I_cwd_n_vr_acc               (:,:) = spval
            allocate (AKX_met_to_soil1_n_vr_acc    (nl_soil,numpatch))                    ; AKX_met_to_soil1_n_vr_acc    (:,:) = spval
            allocate (AKX_cel_to_soil1_n_vr_acc    (nl_soil,numpatch))                    ; AKX_cel_to_soil1_n_vr_acc    (:,:) = spval
            allocate (AKX_lig_to_soil2_n_vr_acc    (nl_soil,numpatch))                    ; AKX_lig_to_soil2_n_vr_acc    (:,:) = spval
            allocate (AKX_soil1_to_soil2_n_vr_acc  (nl_soil,numpatch))                    ; AKX_soil1_to_soil2_n_vr_acc  (:,:) = spval
            allocate (AKX_cwd_to_cel_n_vr_acc      (nl_soil,numpatch))                    ; AKX_cwd_to_cel_n_vr_acc      (:,:) = spval
            allocate (AKX_cwd_to_lig_n_vr_acc      (nl_soil,numpatch))                    ; AKX_cwd_to_lig_n_vr_acc      (:,:) = spval
            allocate (AKX_soil1_to_soil3_n_vr_acc  (nl_soil,numpatch))                    ; AKX_soil1_to_soil3_n_vr_acc  (:,:) = spval
            allocate (AKX_soil2_to_soil1_n_vr_acc  (nl_soil,numpatch))                    ; AKX_soil2_to_soil1_n_vr_acc  (:,:) = spval
            allocate (AKX_soil2_to_soil3_n_vr_acc  (nl_soil,numpatch))                    ; AKX_soil2_to_soil3_n_vr_acc  (:,:) = spval
            allocate (AKX_soil3_to_soil1_n_vr_acc  (nl_soil,numpatch))                    ; AKX_soil3_to_soil1_n_vr_acc  (:,:) = spval
            allocate (AKX_met_exit_n_vr_acc        (nl_soil,numpatch))                    ; AKX_met_exit_n_vr_acc        (:,:) = spval
            allocate (AKX_cel_exit_n_vr_acc        (nl_soil,numpatch))                    ; AKX_cel_exit_n_vr_acc        (:,:) = spval
            allocate (AKX_lig_exit_n_vr_acc        (nl_soil,numpatch))                    ; AKX_lig_exit_n_vr_acc        (:,:) = spval
            allocate (AKX_cwd_exit_n_vr_acc        (nl_soil,numpatch))                    ; AKX_cwd_exit_n_vr_acc        (:,:) = spval
            allocate (AKX_soil1_exit_n_vr_acc      (nl_soil,numpatch))                    ; AKX_soil1_exit_n_vr_acc      (:,:) = spval
            allocate (AKX_soil2_exit_n_vr_acc      (nl_soil,numpatch))                    ; AKX_soil2_exit_n_vr_acc      (:,:) = spval
            allocate (AKX_soil3_exit_n_vr_acc      (nl_soil,numpatch))                    ; AKX_soil3_exit_n_vr_acc      (:,:) = spval

            allocate (diagVX_c_vr_acc              (nl_soil,ndecomp_pools,numpatch))      ; diagVX_c_vr_acc            (:,:,:) = spval
            allocate (upperVX_c_vr_acc             (nl_soil,ndecomp_pools,numpatch))      ; upperVX_c_vr_acc           (:,:,:) = spval
            allocate (lowerVX_c_vr_acc             (nl_soil,ndecomp_pools,numpatch))      ; lowerVX_c_vr_acc           (:,:,:) = spval
            allocate (diagVX_n_vr_acc              (nl_soil,ndecomp_pools,numpatch))      ; diagVX_n_vr_acc            (:,:,:) = spval
            allocate (upperVX_n_vr_acc             (nl_soil,ndecomp_pools,numpatch))      ; upperVX_n_vr_acc           (:,:,:) = spval
            allocate (lowerVX_n_vr_acc             (nl_soil,ndecomp_pools,numpatch))      ; lowerVX_n_vr_acc           (:,:,:) = spval

    !---------------------------------------------------------------------------
            allocate (skip_balance_check           (numpatch))                            ; skip_balance_check             (:) = .false.

#ifdef CROP
            allocate (cphase                       (numpatch))                            ; cphase                (:) = spval ! 30-day running mean of relative humidity
            allocate (vf                           (numpatch))                            ; vf                    (:) = spval
            allocate (gddmaturity                  (numpatch))                            ; gddmaturity           (:) = spval
            allocate (gddplant                     (numpatch))                            ; gddplant              (:) = spval
            allocate (hui                          (numpatch))                            ; hui                   (:) = spval
            allocate (huiswheat                    (numpatch))                            ; huiswheat             (:) = spval
            allocate (pdcorn                       (numpatch))                            ; pdcorn                (:) = spval
            allocate (pdswheat                     (numpatch))                            ; pdswheat              (:) = spval
            allocate (pdwwheat                     (numpatch))                            ; pdwwheat              (:) = spval
            allocate (pdsoybean                    (numpatch))                            ; pdsoybean             (:) = spval
            allocate (pdcotton                     (numpatch))                            ; pdcotton              (:) = spval
            allocate (pdrice1                      (numpatch))                            ; pdrice1               (:) = spval
            allocate (pdrice2                      (numpatch))                            ; pdrice2               (:) = spval
            allocate (plantdate                    (numpatch))                            ; plantdate             (:) = spval
            allocate (pdsugarcane                  (numpatch))                            ; pdsugarcane           (:) = spval
            allocate (manunitro                    (numpatch))                            ; manunitro             (:) = spval
            allocate (fertnitro_corn               (numpatch))                            ; fertnitro_corn        (:) = spval
            allocate (fertnitro_swheat             (numpatch))                            ; fertnitro_swheat      (:) = spval
            allocate (fertnitro_wwheat             (numpatch))                            ; fertnitro_wwheat      (:) = spval
            allocate (fertnitro_soybean            (numpatch))                            ; fertnitro_soybean     (:) = spval
            allocate (fertnitro_cotton             (numpatch))                            ; fertnitro_cotton      (:) = spval
            allocate (fertnitro_rice1              (numpatch))                            ; fertnitro_rice1       (:) = spval
            allocate (fertnitro_rice2              (numpatch))                            ; fertnitro_rice2       (:) = spval
            allocate (fertnitro_sugarcane          (numpatch))                            ; fertnitro_sugarcane   (:) = spval
#endif
            allocate (lag_npp                      (numpatch))                            ; lag_npp               (:) = spval
         ENDIF
      ENDIF


   END SUBROUTINE allocate_BGCTimeVariables


   SUBROUTINE deallocate_BGCTimeVariables ()

   USE MOD_MPAS_MPI
   USE MOD_LandPatch, only: numpatch
   IMPLICIT NONE

! --------------------------------------------------
! Deallocates memory for CoLM 1d [numpatch] variables
! --------------------------------------------------

      IF (.true.) THEN

         IF (numpatch > 0) THEN

 ! bgc variables
            deallocate (decomp_cpools_vr             )
            deallocate (decomp_cpools                )
            deallocate (decomp_cpools_vr_Cap         )
            deallocate (ctrunc_vr                    )
            deallocate (ctrunc_veg                   )
            deallocate (ctrunc_soil                  )
            deallocate (decomp_k                     )

            deallocate (t_scalar                     )
            deallocate (w_scalar                     )
            deallocate (o_scalar                     )
            deallocate (depth_scalar                 )

            deallocate (som_adv_coef                 )
            deallocate (som_diffus_coef              )

            deallocate (altmax                       )
            deallocate (altmax_lastyear              )
            deallocate (altmax_lastyear_indx         )

            deallocate (totlitc                      )
            deallocate (totvegc                      )
            deallocate (totsomc                      )
            deallocate (totcwdc                      )
            deallocate (totcolc                      )
            deallocate (col_begcb                    )
            deallocate (col_endcb                    )
            deallocate (col_vegbegcb                 )
            deallocate (col_vegendcb                 )
            deallocate (col_soilbegcb                )
            deallocate (col_soilendcb                )

            deallocate (totlitn                      )
            deallocate (totvegn                      )
            deallocate (totsomn                      )
            deallocate (totcwdn                      )
            deallocate (totcoln                      )
            deallocate (col_begnb                    )
            deallocate (col_endnb                    )
            deallocate (col_vegbegnb                 )
            deallocate (col_vegendnb                 )
            deallocate (col_soilbegnb                )
            deallocate (col_soilendnb                )
            deallocate (col_sminnbegnb               )
            deallocate (col_sminnendnb               )

            deallocate (leafc                        )
            deallocate (leafc_storage                )
            deallocate (leafc_xfer                   )
            deallocate (frootc                       )
            deallocate (frootc_storage               )
            deallocate (frootc_xfer                  )
            deallocate (livestemc                    )
            deallocate (livestemc_storage            )
            deallocate (livestemc_xfer               )
            deallocate (deadstemc                    )
            deallocate (deadstemc_storage            )
            deallocate (deadstemc_xfer               )
            deallocate (livecrootc                   )
            deallocate (livecrootc_storage           )
            deallocate (livecrootc_xfer              )
            deallocate (deadcrootc                   )
            deallocate (deadcrootc_storage           )
            deallocate (deadcrootc_xfer              )
            deallocate (grainc                       )
            deallocate (grainc_storage               )
            deallocate (grainc_xfer                  )
            deallocate (xsmrpool                     )
            deallocate (downreg                      )
            deallocate (cropprod1c                   )
            deallocate (cropseedc_deficit            )

            deallocate (leafn                        )
            deallocate (leafn_storage                )
            deallocate (leafn_xfer                   )
            deallocate (frootn                       )
            deallocate (frootn_storage               )
            deallocate (frootn_xfer                  )
            deallocate (livestemn                    )
            deallocate (livestemn_storage            )
            deallocate (livestemn_xfer               )
            deallocate (deadstemn                    )
            deallocate (deadstemn_storage            )
            deallocate (deadstemn_xfer               )
            deallocate (livecrootn                   )
            deallocate (livecrootn_storage           )
            deallocate (livecrootn_xfer              )
            deallocate (deadcrootn                   )
            deallocate (deadcrootn_storage           )
            deallocate (deadcrootn_xfer              )
            deallocate (grainn                       )
            deallocate (grainn_storage               )
            deallocate (grainn_xfer                  )
            deallocate (retransn                     )

            deallocate (leafcCap                     )
            deallocate (leafc_storageCap             )
            deallocate (leafc_xferCap                )
            deallocate (frootcCap                    )
            deallocate (frootc_storageCap            )
            deallocate (frootc_xferCap               )
            deallocate (livestemcCap                 )
            deallocate (livestemc_storageCap         )
            deallocate (livestemc_xferCap            )
            deallocate (deadstemcCap                 )
            deallocate (deadstemc_storageCap         )
            deallocate (deadstemc_xferCap            )
            deallocate (livecrootcCap                )
            deallocate (livecrootc_storageCap        )
            deallocate (livecrootc_xferCap           )
            deallocate (deadcrootcCap                )
            deallocate (deadcrootc_storageCap        )
            deallocate (deadcrootc_xferCap           )

            deallocate (leafnCap                     )
            deallocate (leafn_storageCap             )
            deallocate (leafn_xferCap                )
            deallocate (frootnCap                    )
            deallocate (frootn_storageCap            )
            deallocate (frootn_xferCap               )
            deallocate (livestemnCap                 )
            deallocate (livestemn_storageCap         )
            deallocate (livestemn_xferCap            )
            deallocate (deadstemnCap                 )
            deallocate (deadstemn_storageCap         )
            deallocate (deadstemn_xferCap            )
            deallocate (livecrootnCap                )
            deallocate (livecrootn_storageCap        )
            deallocate (livecrootn_xferCap           )
            deallocate (deadcrootnCap                )
            deallocate (deadcrootn_storageCap        )
            deallocate (deadcrootn_xferCap           )

            deallocate (decomp_npools_vr             )
            deallocate (decomp_npools                )
            deallocate (decomp_npools_vr_Cap         )
            deallocate (totsoiln_vr                  )
            deallocate (ntrunc_vr                    )
            deallocate (ntrunc_veg                   )
            deallocate (ntrunc_soil                  )
            deallocate (sminn_vr                     )
            deallocate (smin_no3_vr                  )
            deallocate (smin_nh4_vr                  )
            deallocate (sminn                        )
            deallocate (ndep                         )

            deallocate (to2_decomp_depth_unsat       )
            deallocate (tconc_o2_unsat               )

            deallocate (ndep_prof                    )
            deallocate (nfixation_prof               )

            deallocate (cn_decomp_pools              )
            deallocate (fpi_vr                       )
            deallocate (fpi                          )
            deallocate (fpg                          )

            deallocate (cropf                        )
            deallocate (lfwt                         )
            deallocate (fuelc                        )
            deallocate (fuelc_crop                   )
            deallocate (fsr                          )
            deallocate (fd                           )
            deallocate (rootc                        )
            deallocate (lgdp                         )
            deallocate (lgdp1                        )
            deallocate (lpop                         )
            deallocate (wtlf                         )
            deallocate (trotr1                       )
            deallocate (trotr2                       )
            deallocate (hdm_lf                       )
            deallocate (lnfm                         )
            deallocate (baf_crop                     )
            deallocate (baf_peatf                    )
            deallocate (farea_burned                 )
            deallocate (nfire                        )
            deallocate (fsat                         )
            deallocate (prec10                       )
            deallocate (prec60                       )
            deallocate (prec365                      )
            deallocate (prec_today                   )
            deallocate (prec_daily                   )
            deallocate (wf2                          )
            deallocate (tsoi17                       )
            deallocate (rh30                         )
            deallocate (accumnstep                   )

            deallocate (dayl                         )
            deallocate (prev_dayl                    )

 !---------------------------SASU variables--------------------------------------
            deallocate (decomp0_cpools_vr            )
            deallocate (I_met_c_vr_acc               )
            deallocate (I_cel_c_vr_acc               )
            deallocate (I_lig_c_vr_acc               )
            deallocate (I_cwd_c_vr_acc               )
            deallocate (AKX_met_to_soil1_c_vr_acc    )
            deallocate (AKX_cel_to_soil1_c_vr_acc    )
            deallocate (AKX_lig_to_soil2_c_vr_acc    )
            deallocate (AKX_soil1_to_soil2_c_vr_acc  )
            deallocate (AKX_cwd_to_cel_c_vr_acc      )
            deallocate (AKX_cwd_to_lig_c_vr_acc      )
            deallocate (AKX_soil1_to_soil3_c_vr_acc  )
            deallocate (AKX_soil2_to_soil1_c_vr_acc  )
            deallocate (AKX_soil2_to_soil3_c_vr_acc  )
            deallocate (AKX_soil3_to_soil1_c_vr_acc  )
            deallocate (AKX_met_exit_c_vr_acc        )
            deallocate (AKX_cel_exit_c_vr_acc        )
            deallocate (AKX_lig_exit_c_vr_acc        )
            deallocate (AKX_cwd_exit_c_vr_acc        )
            deallocate (AKX_soil1_exit_c_vr_acc      )
            deallocate (AKX_soil2_exit_c_vr_acc      )
            deallocate (AKX_soil3_exit_c_vr_acc      )

            deallocate (decomp0_npools_vr            )
            deallocate (I_met_n_vr_acc               )
            deallocate (I_cel_n_vr_acc               )
            deallocate (I_lig_n_vr_acc               )
            deallocate (I_cwd_n_vr_acc               )
            deallocate (AKX_met_to_soil1_n_vr_acc    )
            deallocate (AKX_cel_to_soil1_n_vr_acc    )
            deallocate (AKX_lig_to_soil2_n_vr_acc    )
            deallocate (AKX_soil1_to_soil2_n_vr_acc  )
            deallocate (AKX_cwd_to_cel_n_vr_acc      )
            deallocate (AKX_cwd_to_lig_n_vr_acc      )
            deallocate (AKX_soil1_to_soil3_n_vr_acc  )
            deallocate (AKX_soil2_to_soil1_n_vr_acc  )
            deallocate (AKX_soil2_to_soil3_n_vr_acc  )
            deallocate (AKX_soil3_to_soil1_n_vr_acc  )
            deallocate (AKX_met_exit_n_vr_acc        )
            deallocate (AKX_cel_exit_n_vr_acc        )
            deallocate (AKX_lig_exit_n_vr_acc        )
            deallocate (AKX_cwd_exit_n_vr_acc        )
            deallocate (AKX_soil1_exit_n_vr_acc      )
            deallocate (AKX_soil2_exit_n_vr_acc      )
            deallocate (AKX_soil3_exit_n_vr_acc      )

            deallocate (diagVX_c_vr_acc              )
            deallocate (upperVX_c_vr_acc             )
            deallocate (lowerVX_c_vr_acc             )
            deallocate (diagVX_n_vr_acc              )
            deallocate (upperVX_n_vr_acc             )
            deallocate (lowerVX_n_vr_acc             )

 !---------------------------------------------------------------------------
            deallocate (skip_balance_check           )
#ifdef CROP
            deallocate (cphase                       )
            deallocate (vf         )
            deallocate (gddplant   )
            deallocate (gddmaturity)
            deallocate (hui        )
            deallocate (huiswheat  )
            deallocate (pdcorn     )
            deallocate (pdswheat   )
            deallocate (pdwwheat   )
            deallocate (pdsoybean  )
            deallocate (pdcotton   )
            deallocate (pdrice1    )
            deallocate (pdrice2    )
            deallocate (plantdate  )
            deallocate (pdsugarcane)
            deallocate (manunitro          )
            deallocate (fertnitro_corn     )
            deallocate (fertnitro_swheat   )
            deallocate (fertnitro_wwheat   )
            deallocate (fertnitro_soybean  )
            deallocate (fertnitro_cotton   )
            deallocate (fertnitro_rice1    )
            deallocate (fertnitro_rice2    )
            deallocate (fertnitro_sugarcane)
#endif
            deallocate (lag_npp    )
         ENDIF
      ENDIF

   END SUBROUTINE deallocate_BGCTimeVariables


  !---------------------------------------
   SUBROUTINE WRITE_BGCTimeVariables (file_restart)

!=======================================================================
! Original version: Yongjiu Dai, September 15, 1999, 03/2014
!=======================================================================

   USE MOD_Namelist, only: DEF_REST_CompressLevel, DEF_USE_NITRIF
   USE MOD_LandPatch
   USE MOD_NetCDFVector
   USE MOD_Vars_Global
   IMPLICIT NONE

   character(len=*), intent(in) :: file_restart

   ! Local variables
   integer :: compress

      compress = DEF_REST_CompressLevel

      CALL ncio_create_file_vector (file_restart, landpatch)
      CALL ncio_define_dimension_vector (file_restart, landpatch, 'patch')

      CALL ncio_define_dimension_vector (file_restart, landpatch, 'soil',      nl_soil)
      CALL ncio_define_dimension_vector (file_restart, landpatch, 'soil_full', nl_soil_full)
      CALL ncio_define_dimension_vector (file_restart, landpatch, 'ndecomp_pools', ndecomp_pools)
      CALL ncio_define_dimension_vector (file_restart, landpatch, 'doy' , 365)

 ! bgc variables
      CALL ncio_write_vector (file_restart, 'totlitc              ', 'patch', landpatch, totlitc              )
      CALL ncio_write_vector (file_restart, 'totvegc              ', 'patch', landpatch, totvegc              )
      CALL ncio_write_vector (file_restart, 'totsomc              ', 'patch', landpatch, totsomc              )
      CALL ncio_write_vector (file_restart, 'totcwdc              ', 'patch', landpatch, totcwdc              )
      CALL ncio_write_vector (file_restart, 'totcolc              ', 'patch', landpatch, totcolc              )
      CALL ncio_write_vector (file_restart, 'totlitn              ', 'patch', landpatch, totlitn              )
      CALL ncio_write_vector (file_restart, 'totvegn              ', 'patch', landpatch, totvegn              )
      CALL ncio_write_vector (file_restart, 'totsomn              ', 'patch', landpatch, totsomn              )
      CALL ncio_write_vector (file_restart, 'totcwdn              ', 'patch', landpatch, totcwdn              )
      CALL ncio_write_vector (file_restart, 'totcoln              ', 'patch', landpatch, totcoln              )

      CALL ncio_write_vector (file_restart, 'sminn                ', 'patch', landpatch, sminn                )
      CALL ncio_write_vector (file_restart, 'ndep                 ', 'patch', landpatch, ndep                 )

      CALL ncio_write_vector (file_restart, 'decomp_cpools_vr     ', 'soil_full', nl_soil_full, 'ndecomp_pools', ndecomp_pools, &
                                                                     'patch', landpatch,     decomp_cpools_vr)
      IF(DEF_USE_DiagMatrix)THEN
         CALL ncio_write_vector (file_restart, 'decomp_cpools_vr_Cap ', 'soil_full', nl_soil_full, 'ndecomp_pools', ndecomp_pools, &
                                                                     'patch', landpatch,     decomp_cpools_vr_Cap)
      ENDIF
      CALL ncio_write_vector (file_restart, 'ctrunc_vr            ', 'soil' ,   nl_soil, 'patch', landpatch, ctrunc_vr)
      CALL ncio_write_vector (file_restart, 'ctrunc_veg           ', 'patch', landpatch, ctrunc_veg           )
      CALL ncio_write_vector (file_restart, 'ctrunc_soil          ', 'patch', landpatch, ctrunc_soil          )

      CALL ncio_write_vector (file_restart, 'altmax               ', 'patch', landpatch, altmax               )
      CALL ncio_write_vector (file_restart, 'altmax_lastyear      ', 'patch', landpatch, altmax_lastyear      )
      CALL ncio_write_vector (file_restart, 'altmax_lastyear_indx ', 'patch', landpatch, altmax_lastyear_indx )

      CALL ncio_write_vector (file_restart, 'decomp_npools_vr     ', 'soil_full', nl_soil_full, 'ndecomp_pools', ndecomp_pools, &
                                                                     'patch', landpatch,      decomp_npools_vr)
      CALL ncio_write_vector (file_restart, 'totsoiln_vr          ', 'soil' ,   nl_soil, 'patch', landpatch, totsoiln_vr )
      IF(DEF_USE_DiagMatrix)THEN
         CALL ncio_write_vector (file_restart, 'decomp_npools_vr_Cap ', 'soil_full', nl_soil_full, 'ndecomp_pools', ndecomp_pools, &
                                                                     'patch', landpatch,     decomp_npools_vr_Cap)
      ENDIF
      CALL ncio_write_vector (file_restart, 'ntrunc_vr            ', 'soil' ,   nl_soil, 'patch', landpatch, ntrunc_vr   )
      CALL ncio_write_vector (file_restart, 'ntrunc_veg           ', 'patch', landpatch, ntrunc_veg           )
      CALL ncio_write_vector (file_restart, 'ntrunc_soil          ', 'patch', landpatch, ntrunc_soil          )
      CALL ncio_write_vector (file_restart, 'sminn_vr             ', 'soil' ,   nl_soil, 'patch', landpatch, sminn_vr    )
      CALL ncio_write_vector (file_restart, 'smin_no3_vr          ', 'soil' ,   nl_soil, 'patch', landpatch, smin_no3_vr )
      CALL ncio_write_vector (file_restart, 'smin_nh4_vr          ', 'soil' ,   nl_soil, 'patch', landpatch, smin_nh4_vr )
      CALL ncio_write_vector (file_restart, 'lag_npp              ', 'patch', landpatch, lag_npp              )

      IF(DEF_USE_NITRIF)THEN
         CALL ncio_write_vector (file_restart, 'tCONC_O2_UNSAT       ', 'soil'  ,   nl_soil, 'patch', landpatch, tconc_o2_unsat)
         CALL ncio_write_vector (file_restart, 'tO2_DECOMP_DEPTH_UNSAT','soil'  ,   nl_soil, 'patch', landpatch, to2_decomp_depth_unsat)
      ENDIF

      CALL ncio_write_vector (file_restart, 'prec10               ', 'patch', landpatch, prec10               )
      CALL ncio_write_vector (file_restart, 'prec60               ', 'patch', landpatch, prec60               )
      CALL ncio_write_vector (file_restart, 'prec365              ', 'patch', landpatch, prec365              )
      CALL ncio_write_vector (file_restart, 'prec_today           ', 'patch', landpatch, prec_today           )
      CALL ncio_write_vector (file_restart, 'prec_daily           ', 'doy'   ,       365, 'patch', landpatch, prec_daily  )
      CALL ncio_write_vector (file_restart, 'tsoi17               ', 'patch', landpatch, tsoi17               )
      CALL ncio_write_vector (file_restart, 'rh30                 ', 'patch', landpatch, rh30                 )
      CALL ncio_write_vector (file_restart, 'accumnstep           ', 'patch', landpatch, accumnstep           )

      IF(DEF_USE_SASU .or. DEF_USE_DiagMatrix)THEN
 !---------------SASU variables-----------------------
         CALL ncio_write_vector (file_restart, 'decomp0_cpools_vr            ', 'soil'  ,   nl_soil, 'ndecomp_pools', ndecomp_pools, &
                                                                                'patch', landpatch, decomp0_cpools_vr            )
         CALL ncio_write_vector (file_restart, 'I_met_c_vr_acc               ', 'soil'  ,   nl_soil, 'patch', landpatch, I_met_c_vr_acc               )
         CALL ncio_write_vector (file_restart, 'I_cel_c_vr_acc               ', 'soil'  ,   nl_soil, 'patch', landpatch, I_cel_c_vr_acc               )
         CALL ncio_write_vector (file_restart, 'I_lig_c_vr_acc               ', 'soil'  ,   nl_soil, 'patch', landpatch, I_lig_c_vr_acc               )
         CALL ncio_write_vector (file_restart, 'I_cwd_c_vr_acc               ', 'soil'  ,   nl_soil, 'patch', landpatch, I_cwd_c_vr_acc               )
         CALL ncio_write_vector (file_restart, 'AKX_met_to_soil1_c_vr_acc    ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_met_to_soil1_c_vr_acc    )
         CALL ncio_write_vector (file_restart, 'AKX_cel_to_soil1_c_vr_acc    ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_cel_to_soil1_c_vr_acc    )
         CALL ncio_write_vector (file_restart, 'AKX_lig_to_soil2_c_vr_acc    ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_lig_to_soil2_c_vr_acc    )
         CALL ncio_write_vector (file_restart, 'AKX_soil1_to_soil2_c_vr_acc  ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_soil1_to_soil2_c_vr_acc  )
         CALL ncio_write_vector (file_restart, 'AKX_cwd_to_cel_c_vr_acc      ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_cwd_to_cel_c_vr_acc      )
         CALL ncio_write_vector (file_restart, 'AKX_cwd_to_lig_c_vr_acc      ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_cwd_to_lig_c_vr_acc      )
         CALL ncio_write_vector (file_restart, 'AKX_soil1_to_soil3_c_vr_acc  ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_soil1_to_soil3_c_vr_acc  )
         CALL ncio_write_vector (file_restart, 'AKX_soil2_to_soil1_c_vr_acc  ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_soil2_to_soil1_c_vr_acc  )
         CALL ncio_write_vector (file_restart, 'AKX_soil2_to_soil3_c_vr_acc  ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_soil2_to_soil3_c_vr_acc  )
         CALL ncio_write_vector (file_restart, 'AKX_soil3_to_soil1_c_vr_acc  ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_soil3_to_soil1_c_vr_acc  )
         CALL ncio_write_vector (file_restart, 'AKX_met_exit_c_vr_acc        ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_met_exit_c_vr_acc        )
         CALL ncio_write_vector (file_restart, 'AKX_cel_exit_c_vr_acc        ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_cel_exit_c_vr_acc        )
         CALL ncio_write_vector (file_restart, 'AKX_lig_exit_c_vr_acc        ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_lig_exit_c_vr_acc        )
         CALL ncio_write_vector (file_restart, 'AKX_cwd_exit_c_vr_acc        ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_cwd_exit_c_vr_acc        )
         CALL ncio_write_vector (file_restart, 'AKX_soil1_exit_c_vr_acc      ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_soil1_exit_c_vr_acc      )
         CALL ncio_write_vector (file_restart, 'AKX_soil2_exit_c_vr_acc      ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_soil2_exit_c_vr_acc      )
         CALL ncio_write_vector (file_restart, 'AKX_soil3_exit_c_vr_acc      ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_soil3_exit_c_vr_acc      )

         CALL ncio_write_vector (file_restart, 'decomp0_npools_vr            ', 'soil'  ,   nl_soil, 'ndecomp_pools', ndecomp_pools, &
                                                                                'patch', landpatch, decomp0_npools_vr            )
         CALL ncio_write_vector (file_restart, 'I_met_n_vr_acc               ', 'soil'  ,   nl_soil, 'patch', landpatch, I_met_n_vr_acc               )
         CALL ncio_write_vector (file_restart, 'I_cel_n_vr_acc               ', 'soil'  ,   nl_soil, 'patch', landpatch, I_cel_n_vr_acc               )
         CALL ncio_write_vector (file_restart, 'I_lig_n_vr_acc               ', 'soil'  ,   nl_soil, 'patch', landpatch, I_lig_n_vr_acc               )
         CALL ncio_write_vector (file_restart, 'I_cwd_n_vr_acc               ', 'soil'  ,   nl_soil, 'patch', landpatch, I_cwd_n_vr_acc               )
         CALL ncio_write_vector (file_restart, 'AKX_met_to_soil1_n_vr_acc    ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_met_to_soil1_n_vr_acc    )
         CALL ncio_write_vector (file_restart, 'AKX_cel_to_soil1_n_vr_acc    ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_cel_to_soil1_n_vr_acc    )
         CALL ncio_write_vector (file_restart, 'AKX_lig_to_soil2_n_vr_acc    ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_lig_to_soil2_n_vr_acc    )
         CALL ncio_write_vector (file_restart, 'AKX_soil1_to_soil2_n_vr_acc  ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_soil1_to_soil2_n_vr_acc  )
         CALL ncio_write_vector (file_restart, 'AKX_cwd_to_cel_n_vr_acc      ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_cwd_to_cel_n_vr_acc      )
         CALL ncio_write_vector (file_restart, 'AKX_cwd_to_lig_n_vr_acc      ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_cwd_to_lig_n_vr_acc      )
         CALL ncio_write_vector (file_restart, 'AKX_soil1_to_soil3_n_vr_acc  ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_soil1_to_soil3_n_vr_acc  )
         CALL ncio_write_vector (file_restart, 'AKX_soil2_to_soil1_n_vr_acc  ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_soil2_to_soil1_n_vr_acc  )
         CALL ncio_write_vector (file_restart, 'AKX_soil2_to_soil3_n_vr_acc  ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_soil2_to_soil3_n_vr_acc  )
         CALL ncio_write_vector (file_restart, 'AKX_soil3_to_soil1_n_vr_acc  ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_soil3_to_soil1_n_vr_acc  )
         CALL ncio_write_vector (file_restart, 'AKX_met_exit_n_vr_acc        ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_met_exit_n_vr_acc        )
         CALL ncio_write_vector (file_restart, 'AKX_cel_exit_n_vr_acc        ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_cel_exit_n_vr_acc        )
         CALL ncio_write_vector (file_restart, 'AKX_lig_exit_n_vr_acc        ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_lig_exit_n_vr_acc        )
         CALL ncio_write_vector (file_restart, 'AKX_cwd_exit_n_vr_acc        ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_cwd_exit_n_vr_acc        )
         CALL ncio_write_vector (file_restart, 'AKX_soil1_exit_n_vr_acc      ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_soil1_exit_n_vr_acc      )
         CALL ncio_write_vector (file_restart, 'AKX_soil2_exit_n_vr_acc      ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_soil2_exit_n_vr_acc      )
         CALL ncio_write_vector (file_restart, 'AKX_soil3_exit_n_vr_acc      ', 'soil'  ,   nl_soil, 'patch', landpatch, AKX_soil3_exit_n_vr_acc      )

         CALL ncio_write_vector (file_restart, 'diagVX_c_vr_acc              ', 'soil'  ,   nl_soil, 'ndecomp_pools', ndecomp_pools, &
                                                                                'patch', landpatch, diagVX_c_vr_acc              )
         CALL ncio_write_vector (file_restart, 'upperVX_c_vr_acc             ', 'soil'  ,   nl_soil, 'ndecomp_pools', ndecomp_pools, &
                                                                                'patch', landpatch, upperVX_c_vr_acc             )
         CALL ncio_write_vector (file_restart, 'lowerVX_c_vr_acc             ', 'soil'  ,   nl_soil, 'ndecomp_pools', ndecomp_pools, &
                                                                                'patch', landpatch, lowerVX_c_vr_acc             )
         CALL ncio_write_vector (file_restart, 'diagVX_n_vr_acc              ', 'soil'  ,   nl_soil, 'ndecomp_pools', ndecomp_pools, &
                                                                                'patch', landpatch, diagVX_n_vr_acc              )
         CALL ncio_write_vector (file_restart, 'upperVX_n_vr_acc             ', 'soil'  ,   nl_soil, 'ndecomp_pools', ndecomp_pools, &
                                                                                'patch', landpatch, upperVX_n_vr_acc             )
         CALL ncio_write_vector (file_restart, 'lowerVX_n_vr_acc             ', 'soil'  ,   nl_soil, 'ndecomp_pools', ndecomp_pools, &
                                                                                'patch', landpatch, lowerVX_n_vr_acc             )

 !----------------------------------------------------
      ENDIF
      CALL ncio_write_vector (file_restart, 'skip_balance_check           ', 'patch', landpatch, skip_balance_check           )

#ifdef CROP
      CALL ncio_write_vector (file_restart, 'cphase     ' , 'patch', landpatch, cphase               )
      CALL ncio_write_vector (file_restart, 'pdcorn     ' , 'patch', landpatch, pdcorn     , compress)
      CALL ncio_write_vector (file_restart, 'pdswheat   ' , 'patch', landpatch, pdswheat   , compress)
      CALL ncio_write_vector (file_restart, 'pdwwheat   ' , 'patch', landpatch, pdwwheat   , compress)
      CALL ncio_write_vector (file_restart, 'pdsoybean  ' , 'patch', landpatch, pdsoybean  , compress)
      CALL ncio_write_vector (file_restart, 'pdcotton   ' , 'patch', landpatch, pdcotton   , compress)
      CALL ncio_write_vector (file_restart, 'pdrice1    ' , 'patch', landpatch, pdrice1    , compress)
      CALL ncio_write_vector (file_restart, 'pdrice2    ' , 'patch', landpatch, pdrice2    , compress)
      CALL ncio_write_vector (file_restart, 'pdsugarcane' , 'patch', landpatch, pdsugarcane, compress)
      CALL ncio_write_vector (file_restart, 'fertnitro_corn     ' , 'patch', landpatch, fertnitro_corn     , compress)
      CALL ncio_write_vector (file_restart, 'fertnitro_swheat   ' , 'patch', landpatch, fertnitro_swheat   , compress)
      CALL ncio_write_vector (file_restart, 'fertnitro_wwheat   ' , 'patch', landpatch, fertnitro_wwheat   , compress)
      CALL ncio_write_vector (file_restart, 'fertnitro_soybean  ' , 'patch', landpatch, fertnitro_soybean  , compress)
      CALL ncio_write_vector (file_restart, 'fertnitro_cotton   ' , 'patch', landpatch, fertnitro_cotton   , compress)
      CALL ncio_write_vector (file_restart, 'fertnitro_rice1    ' , 'patch', landpatch, fertnitro_rice1    , compress)
      CALL ncio_write_vector (file_restart, 'fertnitro_rice2    ' , 'patch', landpatch, fertnitro_rice2    , compress)
      CALL ncio_write_vector (file_restart, 'fertnitro_sugarcane' , 'patch', landpatch, fertnitro_sugarcane, compress)
#endif

   END SUBROUTINE WRITE_BGCTimeVariables

  !---------------------------------------
   SUBROUTINE READ_BGCTimeVariables (file_restart)

!=======================================================================
! Original version: Yongjiu Dai, September 15, 1999, 03/2014
!=======================================================================

   USE MOD_Namelist
   USE MOD_MPAS_MPI
   USE MOD_NetCDFVector
#ifdef RangeCheck
   USE MOD_RangeCheck
#endif
   USE MOD_LandPatch
   USE MOD_Vars_Global

   IMPLICIT NONE

   character(len=*), intent(in) :: file_restart

! bgc variables
      CALL ncio_read_vector (file_restart, 'totlitc              ', landpatch, totlitc              )
      CALL ncio_read_vector (file_restart, 'totvegc              ', landpatch, totvegc              )
      CALL ncio_read_vector (file_restart, 'totsomc              ', landpatch, totsomc              )
      CALL ncio_read_vector (file_restart, 'totcwdc              ', landpatch, totcwdc              )
      CALL ncio_read_vector (file_restart, 'totcolc              ', landpatch, totcolc              )
      CALL ncio_read_vector (file_restart, 'totlitn              ', landpatch, totlitn              )
      CALL ncio_read_vector (file_restart, 'totvegn              ', landpatch, totvegn              )
      CALL ncio_read_vector (file_restart, 'totsomn              ', landpatch, totsomn              )
      CALL ncio_read_vector (file_restart, 'totcwdn              ', landpatch, totcwdn              )
      CALL ncio_read_vector (file_restart, 'totcoln              ', landpatch, totcoln              )

      CALL ncio_read_vector (file_restart, 'sminn                ', landpatch, sminn                )
      CALL ncio_read_vector (file_restart, 'ndep                 ', landpatch, ndep                 )

      CALL ncio_read_vector (file_restart, 'decomp_cpools_vr     ',   nl_soil_full, ndecomp_pools, landpatch, decomp_cpools_vr)
      IF(DEF_USE_DiagMatrix)THEN
         CALL ncio_read_vector (file_restart, 'decomp_cpools_vr_Cap ',   nl_soil_full, ndecomp_pools, landpatch, decomp_cpools_vr_Cap, defval = 1._r8)
      ENDIF
      CALL ncio_read_vector (file_restart, 'ctrunc_vr            ',   nl_soil, landpatch, ctrunc_vr            )
      CALL ncio_read_vector (file_restart, 'ctrunc_veg           ', landpatch, ctrunc_veg           )
      CALL ncio_read_vector (file_restart, 'ctrunc_soil          ', landpatch, ctrunc_soil          )

      CALL ncio_read_vector (file_restart, 'altmax               ', landpatch, altmax               )
      CALL ncio_read_vector (file_restart, 'altmax_lastyear      ', landpatch, altmax_lastyear      )
      CALL ncio_read_vector (file_restart, 'altmax_lastyear_indx ', landpatch, altmax_lastyear_indx )

      CALL ncio_read_vector (file_restart, 'decomp_npools_vr     ',   nl_soil_full, ndecomp_pools, landpatch, decomp_npools_vr)
      CALL ncio_read_vector (file_restart, 'totsoiln_vr          ',   nl_soil, landpatch, totsoiln_vr, defval = 1._r8)
      IF(DEF_USE_DiagMatrix)THEN
         CALL ncio_read_vector (file_restart, 'decomp_npools_vr_Cap ',nl_soil_full, ndecomp_pools, landpatch, decomp_npools_vr_Cap, defval = 1._r8)
      ENDIF
      CALL ncio_read_vector (file_restart, 'ntrunc_vr            ',   nl_soil, landpatch, ntrunc_vr  )
      CALL ncio_read_vector (file_restart, 'ntrunc_veg           ', landpatch, ntrunc_veg            )
      CALL ncio_read_vector (file_restart, 'ntrunc_soil          ', landpatch, ntrunc_soil           )
      CALL ncio_read_vector (file_restart, 'sminn_vr             ',   nl_soil, landpatch, sminn_vr   )
      CALL ncio_read_vector (file_restart, 'smin_no3_vr          ',   nl_soil, landpatch, smin_no3_vr)
      CALL ncio_read_vector (file_restart, 'smin_nh4_vr          ',   nl_soil, landpatch, smin_nh4_vr)
      CALL ncio_read_vector (file_restart, 'lag_npp              ', landpatch, lag_npp, defval =spval  )

      IF(DEF_USE_NITRIF)THEN
         CALL ncio_read_vector (file_restart, 'tCONC_O2_UNSAT       ',   nl_soil, landpatch, tconc_o2_unsat         )
         CALL ncio_read_vector (file_restart, 'tO2_DECOMP_DEPTH_UNSAT',  nl_soil, landpatch, to2_decomp_depth_unsat )
      ENDIF

      CALL ncio_read_vector (file_restart, 'prec10               ', landpatch, prec10               )
      CALL ncio_read_vector (file_restart, 'prec60               ', landpatch, prec60               )
      CALL ncio_read_vector (file_restart, 'prec365              ', landpatch, prec365              )
      CALL ncio_read_vector (file_restart, 'prec_today           ', landpatch, prec_today           )
      CALL ncio_read_vector (file_restart, 'prec_daily           ',       365, landpatch, prec_daily)
      CALL ncio_read_vector (file_restart, 'tsoi17               ', landpatch, tsoi17               )
      CALL ncio_read_vector (file_restart, 'rh30                 ', landpatch, rh30                 )
      CALL ncio_read_vector (file_restart, 'accumnstep           ', landpatch, accumnstep           )

      IF(DEF_USE_SASU .or. DEF_USE_DiagMatrix)THEN
 !---------------SASU variables-----------------------
         CALL ncio_read_vector (file_restart, 'decomp0_cpools_vr            ',   nl_soil, ndecomp_pools, landpatch, decomp0_cpools_vr, defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'I_met_c_vr_acc               ',   nl_soil, landpatch, I_met_c_vr_acc, defval = 0._r8               )
         CALL ncio_read_vector (file_restart, 'I_cel_c_vr_acc               ',   nl_soil, landpatch, I_cel_c_vr_acc, defval = 0._r8               )
         CALL ncio_read_vector (file_restart, 'I_lig_c_vr_acc               ',   nl_soil, landpatch, I_lig_c_vr_acc, defval = 0._r8               )
         CALL ncio_read_vector (file_restart, 'I_cwd_c_vr_acc               ',   nl_soil, landpatch, I_cwd_c_vr_acc, defval = 0._r8               )
         CALL ncio_read_vector (file_restart, 'AKX_met_to_soil1_c_vr_acc    ',   nl_soil, landpatch, AKX_met_to_soil1_c_vr_acc, defval = 0._r8    )
         CALL ncio_read_vector (file_restart, 'AKX_cel_to_soil1_c_vr_acc    ',   nl_soil, landpatch, AKX_cel_to_soil1_c_vr_acc, defval = 0._r8    )
         CALL ncio_read_vector (file_restart, 'AKX_lig_to_soil2_c_vr_acc    ',   nl_soil, landpatch, AKX_lig_to_soil2_c_vr_acc, defval = 0._r8    )
         CALL ncio_read_vector (file_restart, 'AKX_soil1_to_soil2_c_vr_acc  ',   nl_soil, landpatch, AKX_soil1_to_soil2_c_vr_acc, defval = 0._r8  )
         CALL ncio_read_vector (file_restart, 'AKX_cwd_to_cel_c_vr_acc      ',   nl_soil, landpatch, AKX_cwd_to_cel_c_vr_acc, defval = 0._r8      )
         CALL ncio_read_vector (file_restart, 'AKX_cwd_to_lig_c_vr_acc      ',   nl_soil, landpatch, AKX_cwd_to_lig_c_vr_acc, defval = 0._r8      )
         CALL ncio_read_vector (file_restart, 'AKX_soil1_to_soil3_c_vr_acc  ',   nl_soil, landpatch, AKX_soil1_to_soil3_c_vr_acc, defval = 0._r8  )
         CALL ncio_read_vector (file_restart, 'AKX_soil2_to_soil1_c_vr_acc  ',   nl_soil, landpatch, AKX_soil2_to_soil1_c_vr_acc, defval = 0._r8  )
         CALL ncio_read_vector (file_restart, 'AKX_soil2_to_soil3_c_vr_acc  ',   nl_soil, landpatch, AKX_soil2_to_soil3_c_vr_acc, defval = 0._r8  )
         CALL ncio_read_vector (file_restart, 'AKX_soil3_to_soil1_c_vr_acc  ',   nl_soil, landpatch, AKX_soil3_to_soil1_c_vr_acc, defval = 0._r8  )
         CALL ncio_read_vector (file_restart, 'AKX_met_exit_c_vr_acc        ',   nl_soil, landpatch, AKX_met_exit_c_vr_acc, defval = 0._r8        )
         CALL ncio_read_vector (file_restart, 'AKX_cel_exit_c_vr_acc        ',   nl_soil, landpatch, AKX_cel_exit_c_vr_acc, defval = 0._r8        )
         CALL ncio_read_vector (file_restart, 'AKX_lig_exit_c_vr_acc        ',   nl_soil, landpatch, AKX_lig_exit_c_vr_acc, defval = 0._r8        )
         CALL ncio_read_vector (file_restart, 'AKX_cwd_exit_c_vr_acc        ',   nl_soil, landpatch, AKX_cwd_exit_c_vr_acc, defval = 0._r8        )
         CALL ncio_read_vector (file_restart, 'AKX_soil1_exit_c_vr_acc      ',   nl_soil, landpatch, AKX_soil1_exit_c_vr_acc, defval = 0._r8      )
         CALL ncio_read_vector (file_restart, 'AKX_soil2_exit_c_vr_acc      ',   nl_soil, landpatch, AKX_soil2_exit_c_vr_acc, defval = 0._r8      )
         CALL ncio_read_vector (file_restart, 'AKX_soil3_exit_c_vr_acc      ',   nl_soil, landpatch, AKX_soil3_exit_c_vr_acc, defval = 0._r8      )

         CALL ncio_read_vector (file_restart, 'decomp0_npools_vr            ',   nl_soil, ndecomp_pools, landpatch, decomp0_npools_vr, defval = 1._r8)
         CALL ncio_read_vector (file_restart, 'I_met_n_vr_acc               ',   nl_soil, landpatch, I_met_n_vr_acc, defval = 0._r8               )
         CALL ncio_read_vector (file_restart, 'I_cel_n_vr_acc               ',   nl_soil, landpatch, I_cel_n_vr_acc, defval = 0._r8               )
         CALL ncio_read_vector (file_restart, 'I_lig_n_vr_acc               ',   nl_soil, landpatch, I_lig_n_vr_acc, defval = 0._r8               )
         CALL ncio_read_vector (file_restart, 'I_cwd_n_vr_acc               ',   nl_soil, landpatch, I_cwd_n_vr_acc, defval = 0._r8               )
         CALL ncio_read_vector (file_restart, 'AKX_met_to_soil1_n_vr_acc    ',   nl_soil, landpatch, AKX_met_to_soil1_n_vr_acc, defval = 0._r8    )
         CALL ncio_read_vector (file_restart, 'AKX_cel_to_soil1_n_vr_acc    ',   nl_soil, landpatch, AKX_cel_to_soil1_n_vr_acc, defval = 0._r8    )
         CALL ncio_read_vector (file_restart, 'AKX_lig_to_soil2_n_vr_acc    ',   nl_soil, landpatch, AKX_lig_to_soil2_n_vr_acc, defval = 0._r8    )
         CALL ncio_read_vector (file_restart, 'AKX_soil1_to_soil2_n_vr_acc  ',   nl_soil, landpatch, AKX_soil1_to_soil2_n_vr_acc, defval = 0._r8  )
         CALL ncio_read_vector (file_restart, 'AKX_cwd_to_cel_n_vr_acc      ',   nl_soil, landpatch, AKX_cwd_to_cel_n_vr_acc, defval = 0._r8      )
         CALL ncio_read_vector (file_restart, 'AKX_cwd_to_lig_n_vr_acc      ',   nl_soil, landpatch, AKX_cwd_to_lig_n_vr_acc, defval = 0._r8      )
         CALL ncio_read_vector (file_restart, 'AKX_soil1_to_soil3_n_vr_acc  ',   nl_soil, landpatch, AKX_soil1_to_soil3_n_vr_acc, defval = 0._r8  )
         CALL ncio_read_vector (file_restart, 'AKX_soil2_to_soil1_n_vr_acc  ',   nl_soil, landpatch, AKX_soil2_to_soil1_n_vr_acc, defval = 0._r8  )
         CALL ncio_read_vector (file_restart, 'AKX_soil2_to_soil3_n_vr_acc  ',   nl_soil, landpatch, AKX_soil2_to_soil3_n_vr_acc, defval = 0._r8  )
         CALL ncio_read_vector (file_restart, 'AKX_soil3_to_soil1_n_vr_acc  ',   nl_soil, landpatch, AKX_soil3_to_soil1_n_vr_acc, defval = 0._r8  )
         CALL ncio_read_vector (file_restart, 'AKX_met_exit_n_vr_acc        ',   nl_soil, landpatch, AKX_met_exit_n_vr_acc, defval = 0._r8        )
         CALL ncio_read_vector (file_restart, 'AKX_cel_exit_n_vr_acc        ',   nl_soil, landpatch, AKX_cel_exit_n_vr_acc, defval = 0._r8        )
         CALL ncio_read_vector (file_restart, 'AKX_lig_exit_n_vr_acc        ',   nl_soil, landpatch, AKX_lig_exit_n_vr_acc, defval = 0._r8        )
         CALL ncio_read_vector (file_restart, 'AKX_cwd_exit_n_vr_acc        ',   nl_soil, landpatch, AKX_cwd_exit_n_vr_acc, defval = 0._r8        )
         CALL ncio_read_vector (file_restart, 'AKX_soil1_exit_n_vr_acc      ',   nl_soil, landpatch, AKX_soil1_exit_n_vr_acc, defval = 0._r8      )
         CALL ncio_read_vector (file_restart, 'AKX_soil2_exit_n_vr_acc      ',   nl_soil, landpatch, AKX_soil2_exit_n_vr_acc, defval = 0._r8      )
         CALL ncio_read_vector (file_restart, 'AKX_soil3_exit_n_vr_acc      ',   nl_soil, landpatch, AKX_soil3_exit_n_vr_acc, defval = 0._r8      )

         CALL ncio_read_vector (file_restart, 'diagVX_c_vr_acc              ',   nl_soil, ndecomp_pools, landpatch, diagVX_c_vr_acc, defval = 0._r8              )
         CALL ncio_read_vector (file_restart, 'upperVX_c_vr_acc             ',   nl_soil, ndecomp_pools, landpatch, upperVX_c_vr_acc, defval = 0._r8             )
         CALL ncio_read_vector (file_restart, 'lowerVX_c_vr_acc             ',   nl_soil, ndecomp_pools, landpatch, lowerVX_c_vr_acc, defval = 0._r8             )
         CALL ncio_read_vector (file_restart, 'diagVX_n_vr_acc              ',   nl_soil, ndecomp_pools, landpatch, diagVX_n_vr_acc, defval = 0._r8              )
         CALL ncio_read_vector (file_restart, 'upperVX_n_vr_acc             ',   nl_soil, ndecomp_pools, landpatch, upperVX_n_vr_acc, defval = 0._r8             )
         CALL ncio_read_vector (file_restart, 'lowerVX_n_vr_acc             ',   nl_soil, ndecomp_pools, landpatch, lowerVX_n_vr_acc, defval = 0._r8             )
      ENDIF

 !----------------------------------------------------
      CALL ncio_read_vector (file_restart, 'skip_balance_check           ', landpatch, skip_balance_check           )
#ifdef CROP
      CALL ncio_read_vector (file_restart, 'cphase     ' , landpatch, cphase     )
      CALL ncio_read_vector (file_restart, 'pdcorn     ' , landpatch, pdcorn     )
      CALL ncio_read_vector (file_restart, 'pdswheat   ' , landpatch, pdswheat   )
      CALL ncio_read_vector (file_restart, 'pdwwheat   ' , landpatch, pdwwheat   )
      CALL ncio_read_vector (file_restart, 'pdsoybean  ' , landpatch, pdsoybean  )
      CALL ncio_read_vector (file_restart, 'pdcotton   ' , landpatch, pdcotton   )
      CALL ncio_read_vector (file_restart, 'pdrice1    ' , landpatch, pdrice1    )
      CALL ncio_read_vector (file_restart, 'pdrice2    ' , landpatch, pdrice2    )
      CALL ncio_read_vector (file_restart, 'pdsugarcane' , landpatch, pdsugarcane)
      CALL ncio_read_vector (file_restart, 'fertnitro_corn     ' , landpatch, fertnitro_corn     )
      CALL ncio_read_vector (file_restart, 'fertnitro_swheat   ' , landpatch, fertnitro_swheat   )
      CALL ncio_read_vector (file_restart, 'fertnitro_wwheat   ' , landpatch, fertnitro_wwheat   )
      CALL ncio_read_vector (file_restart, 'fertnitro_soybean  ' , landpatch, fertnitro_soybean  )
      CALL ncio_read_vector (file_restart, 'fertnitro_cotton   ' , landpatch, fertnitro_cotton   )
      CALL ncio_read_vector (file_restart, 'fertnitro_rice1    ' , landpatch, fertnitro_rice1    )
      CALL ncio_read_vector (file_restart, 'fertnitro_rice2    ' , landpatch, fertnitro_rice2    )
      CALL ncio_read_vector (file_restart, 'fertnitro_sugarcane' , landpatch, fertnitro_sugarcane)
#endif

#ifdef RangeCheck
      CALL check_BGCTimeVariables
#endif

   END SUBROUTINE READ_BGCTimeVariables

  !---------------------------------------
#ifdef RangeCheck
   SUBROUTINE check_BGCTimeVariables ()

   USE MOD_MPAS_MPI
   USE MOD_RangeCheck
   USE MOD_Namelist, only: DEF_USE_NITRIF, DEF_USE_SASU, DEF_USE_DiagMatrix

   IMPLICIT NONE

! bgc variables
      CALL check_vector_data ('decomp_cpools_vr  ', decomp_cpools_vr  )
      CALL check_vector_data ('decomp_cpools     ', decomp_cpools     )
      IF(DEF_USE_DiagMatrix)THEN
         CALL check_vector_data ('decomp_cpools_vr_Cap',decomp_cpools_vr_Cap)
      ENDIF
      CALL check_vector_data ('decomp_k          ', decomp_k          )
      CALL check_vector_data ('ctrunc_vr         ', ctrunc_vr         )
      CALL check_vector_data ('ctrunc_veg        ', ctrunc_veg        )
      CALL check_vector_data ('ctrunc_soil       ', ctrunc_soil       )

      CALL check_vector_data ('t_scalar          ', t_scalar          )
      CALL check_vector_data ('w_scalar          ', w_scalar          )
      CALL check_vector_data ('o_scalar          ', o_scalar          )
      CALL check_vector_data ('depth_scalar      ', depth_scalar      )

 ! Soil CN diffusion and advection
      CALL check_vector_data ('som_adv_coef             ', som_adv_coef             )
      CALL check_vector_data ('som_diffus_coef          ', som_diffus_coef          )

 ! Active Layer
      CALL check_vector_data ('altmax                   ', altmax                   )
      CALL check_vector_data ('altmax_lastyear          ', altmax_lastyear          )
      !CALL check_vector_data ('altmax_lastyear_indx     ', altmax_lastyear_indx     )

      CALL check_vector_data ('totlitc                  ', totlitc                  )
      CALL check_vector_data ('totvegc                  ', totvegc                  )
      CALL check_vector_data ('totsomc                  ', totsomc                  )
      CALL check_vector_data ('totcwdc                  ', totcwdc                  )
      CALL check_vector_data ('totcolc                  ', totcolc                  )
      CALL check_vector_data ('col_begcb                ', col_begcb                )
      CALL check_vector_data ('col_endcb                ', col_endcb                )
      CALL check_vector_data ('col_vegbegcb             ', col_vegbegcb             )
      CALL check_vector_data ('col_vegendcb             ', col_vegendcb             )
      CALL check_vector_data ('col_soilbegcb            ', col_soilbegcb            )
      CALL check_vector_data ('col_soilendcb            ', col_soilendcb            )

      CALL check_vector_data ('totlitn                  ', totlitn                  )
      CALL check_vector_data ('totvegn                  ', totvegn                  )
      CALL check_vector_data ('totsomn                  ', totsomn                  )
      CALL check_vector_data ('totcwdn                  ', totcwdn                  )
      CALL check_vector_data ('totcoln                  ', totcoln                  )
      CALL check_vector_data ('col_begnb                ', col_begnb                )
      CALL check_vector_data ('col_endnb                ', col_endnb                )
      CALL check_vector_data ('col_vegbegnb             ', col_vegbegnb             )
      CALL check_vector_data ('col_vegendnb             ', col_vegendnb             )
      CALL check_vector_data ('col_soilbegnb            ', col_soilbegnb            )
      CALL check_vector_data ('col_soilendnb            ', col_soilendnb            )
      CALL check_vector_data ('col_sminnbegnb           ', col_sminnbegnb           )
      CALL check_vector_data ('col_sminnendnb           ', col_sminnendnb           )

      CALL check_vector_data ('leafc                    ', leafc                    )
      CALL check_vector_data ('leafc_storage            ', leafc_storage            )
      CALL check_vector_data ('leafc_xfer               ', leafc_xfer               )
      CALL check_vector_data ('frootc                   ', frootc                   )
      CALL check_vector_data ('frootc_storage           ', frootc_storage           )
      CALL check_vector_data ('frootc_xfer              ', frootc_xfer              )
      CALL check_vector_data ('livestemc                ', livestemc                )
      CALL check_vector_data ('livestemc_storage        ', livestemc_storage        )
      CALL check_vector_data ('livestemc_xfer           ', livestemc_xfer           )
      CALL check_vector_data ('deadstemc                ', deadstemc                )
      CALL check_vector_data ('deadstemc_storage        ', deadstemc_storage        )
      CALL check_vector_data ('deadstemc_xfer           ', deadstemc_xfer           )
      CALL check_vector_data ('livecrootc               ', livecrootc               )
      CALL check_vector_data ('livecrootc_storage       ', livecrootc_storage       )
      CALL check_vector_data ('livecrootc_xfer          ', livecrootc_xfer          )
      CALL check_vector_data ('deadcrootc               ', deadcrootc               )
      CALL check_vector_data ('deadcrootc_storage       ', deadcrootc_storage       )
      CALL check_vector_data ('deadcrootc_xfer          ', deadcrootc_xfer          )
      CALL check_vector_data ('grainc                   ', grainc                   )
      CALL check_vector_data ('grainc_storage           ', grainc_storage           )
      CALL check_vector_data ('grainc_xfer              ', grainc_xfer              )
      CALL check_vector_data ('xsmrpool                 ', xsmrpool                 )
      CALL check_vector_data ('downreg                  ', downreg                  )
      CALL check_vector_data ('cropprod1c               ', cropprod1c               )
      CALL check_vector_data ('cropseedc_deficit        ', cropseedc_deficit        )

      CALL check_vector_data ('leafn                    ', leafn                    )
      CALL check_vector_data ('leafn_storage            ', leafn_storage            )
      CALL check_vector_data ('leafn_xfer               ', leafn_xfer               )
      CALL check_vector_data ('frootn                   ', frootn                   )
      CALL check_vector_data ('frootn_storage           ', frootn_storage           )
      CALL check_vector_data ('frootn_xfer              ', frootn_xfer              )
      CALL check_vector_data ('livestemn                ', livestemn                )
      CALL check_vector_data ('livestemn_storage        ', livestemn_storage        )
      CALL check_vector_data ('livestemn_xfer           ', livestemn_xfer           )
      CALL check_vector_data ('deadstemn                ', deadstemn                )
      CALL check_vector_data ('deadstemn_storage        ', deadstemn_storage        )
      CALL check_vector_data ('deadstemn_xfer           ', deadstemn_xfer           )
      CALL check_vector_data ('livecrootn               ', livecrootn               )
      CALL check_vector_data ('livecrootn_storage       ', livecrootn_storage       )
      CALL check_vector_data ('livecrootn_xfer          ', livecrootn_xfer          )
      CALL check_vector_data ('deadcrootn               ', deadcrootn               )
      CALL check_vector_data ('deadcrootn_storage       ', deadcrootn_storage       )
      CALL check_vector_data ('deadcrootn_xfer          ', deadcrootn_xfer          )
      CALL check_vector_data ('grainn                   ', grainn                   )
      CALL check_vector_data ('grainn_storage           ', grainn_storage           )
      CALL check_vector_data ('grainn_xfer              ', grainn_xfer              )
      CALL check_vector_data ('retransn                 ', retransn                 )

      IF(DEF_USE_DiagMatrix)THEN
         CALL check_vector_data ('leafcCap              ', leafcCap                 )
         CALL check_vector_data ('leafc_storageCap      ', leafc_storageCap         )
         CALL check_vector_data ('leafc_xferCap         ', leafc_xferCap            )
         CALL check_vector_data ('frootcCap             ', frootcCap                )
         CALL check_vector_data ('frootc_storageCap     ', frootc_storageCap        )
         CALL check_vector_data ('frootc_xferCap        ', frootc_xferCap           )
         CALL check_vector_data ('livestemcCap          ', livestemcCap             )
         CALL check_vector_data ('livestemc_storageCap  ', livestemc_storageCap     )
         CALL check_vector_data ('livestemc_xferCap     ', livestemc_xferCap        )
         CALL check_vector_data ('deadstemcCap          ', deadstemcCap             )
         CALL check_vector_data ('deadstemc_storageCap  ', deadstemc_storageCap     )
         CALL check_vector_data ('deadstemc_xferCap     ', deadstemc_xferCap        )
         CALL check_vector_data ('livecrootcCap         ', livecrootcCap            )
         CALL check_vector_data ('livecrootc_storageCap ', livecrootc_storageCap    )
         CALL check_vector_data ('livecrootc_xferCap    ', livecrootc_xferCap       )
         CALL check_vector_data ('deadcrootcCap         ', deadcrootcCap            )
         CALL check_vector_data ('deadcrootc_storageCap ', deadcrootc_storageCap    )
         CALL check_vector_data ('deadcrootc_xferCap    ', deadcrootc_xferCap       )

         CALL check_vector_data ('leafnCap              ', leafnCap                 )
         CALL check_vector_data ('leafn_storageCap      ', leafn_storageCap         )
         CALL check_vector_data ('leafn_xferCap         ', leafn_xferCap            )
         CALL check_vector_data ('frootnCap             ', frootnCap                )
         CALL check_vector_data ('frootn_storageCap     ', frootn_storageCap        )
         CALL check_vector_data ('frootn_xferCap        ', frootn_xferCap           )
         CALL check_vector_data ('livestemnCap          ', livestemnCap             )
         CALL check_vector_data ('livestemn_storageCap  ', livestemn_storageCap     )
         CALL check_vector_data ('livestemn_xferCap     ', livestemn_xferCap        )
         CALL check_vector_data ('deadstemnCap          ', deadstemnCap             )
         CALL check_vector_data ('deadstemn_storageCap  ', deadstemn_storageCap     )
         CALL check_vector_data ('deadstemn_xferCap     ', deadstemn_xferCap        )
         CALL check_vector_data ('livecrootnCap         ', livecrootnCap            )
         CALL check_vector_data ('livecrootn_storageCap ', livecrootn_storageCap    )
         CALL check_vector_data ('livecrootn_xferCap    ', livecrootn_xferCap       )
         CALL check_vector_data ('deadcrootnCap         ', deadcrootnCap            )
         CALL check_vector_data ('deadcrootn_storageCap ', deadcrootn_storageCap    )
         CALL check_vector_data ('deadcrootn_xferCap    ', deadcrootn_xferCap       )

      ENDIF

      CALL check_vector_data ('decomp_npools_vr         ', decomp_npools_vr         )
      CALL check_vector_data ('decomp_npools            ', decomp_npools            )
      CALL check_vector_data ('totsoiln_vr              ', totsoiln_vr              )
      IF(DEF_USE_DiagMatrix)THEN
         CALL check_vector_data ('decomp_npools_vr_Cap  ',decomp_npools_vr_Cap      )
      ENDIF
      CALL check_vector_data ('ntrunc_vr                ', ntrunc_vr                )
      CALL check_vector_data ('ntrunc_veg               ', ntrunc_veg               )
      CALL check_vector_data ('ntrunc_soil              ', ntrunc_soil              )

      CALL check_vector_data ('sminn_vr                 ', sminn_vr                 )
      CALL check_vector_data ('smin_no3_vr              ', smin_no3_vr              )
      CALL check_vector_data ('smin_nh4_vr              ', smin_nh4_vr              )

      IF(DEF_USE_NITRIF)THEN
         CALL check_vector_data ('tCONC_O2_UNSAT           ', tconc_o2_unsat        )
         CALL check_vector_data ('tO2_DECOMP_DEPTH_UNSAT   ', to2_decomp_depth_unsat)
      ENDIF

      CALL check_vector_data ('sminn                    ', sminn                    )
      CALL check_vector_data ('ndep                     ', ndep                     )

      CALL check_vector_data ('ndep_prof                ', ndep_prof                )
      CALL check_vector_data ('nfixation_prof           ', nfixation_prof           )

      CALL check_vector_data ('cn_decomp_pools          ', cn_decomp_pools          )
      CALL check_vector_data ('fpi_vr                   ', fpi_vr                   )
      CALL check_vector_data ('fpi                      ', fpi                      )
      CALL check_vector_data ('fpg                      ', fpg                      )

      CALL check_vector_data ('cropf                    ', cropf                    )
      CALL check_vector_data ('lfwt                     ', lfwt                     )
      CALL check_vector_data ('fuelc                    ', fuelc                    )
      CALL check_vector_data ('fuelc_crop               ', fuelc_crop               )
      CALL check_vector_data ('fsr                      ', fsr                      )
      CALL check_vector_data ('fd                       ', fd                       )
      CALL check_vector_data ('rootc                    ', rootc                    )
      CALL check_vector_data ('lgdp                     ', lgdp                     )
      CALL check_vector_data ('lgdp1                    ', lgdp1                    )
      CALL check_vector_data ('lpop                     ', lpop                     )
      CALL check_vector_data ('wtlf                     ', wtlf                     )
      CALL check_vector_data ('trotr1                   ', trotr1                   )
      CALL check_vector_data ('trotr2                   ', trotr2                   )
      CALL check_vector_data ('hdm_lf                   ', hdm_lf                   )
      CALL check_vector_data ('lnfm                     ', lnfm                     )
      CALL check_vector_data ('baf_crop                 ', baf_crop                 )
      CALL check_vector_data ('baf_peatf                ', baf_peatf                )
      CALL check_vector_data ('farea_burned             ', farea_burned             )
      CALL check_vector_data ('nfire                    ', nfire                    )
      CALL check_vector_data ('fsat                     ', fsat                     )
      CALL check_vector_data ('prec10                   ', prec10                   )
      CALL check_vector_data ('prec60                   ', prec60                   )
      CALL check_vector_data ('prec365                  ', prec365                  )
      CALL check_vector_data ('prec_today               ', prec_today               )
      CALL check_vector_data ('prec_daily               ', prec_daily               )
      CALL check_vector_data ('wf2                      ', wf2                      )
      CALL check_vector_data ('tsoi17                   ', tsoi17                   )
      CALL check_vector_data ('rh30                     ', rh30                     )
      CALL check_vector_data ('accumnstep               ', accumnstep               )

      CALL check_vector_data ('dayl                     ', dayl                     )
      CALL check_vector_data ('prev_dayl                ', prev_dayl                )

      IF(DEF_USE_SASU .or. DEF_USE_DiagMatrix)THEN
 !--------------SASU variables---------------------------
         CALL check_vector_data ('decomp0_cpools_vr           ', decomp0_cpools_vr           )
         CALL check_vector_data ('I_met_c_vr_acc              ', I_met_c_vr_acc              )
         CALL check_vector_data ('I_cel_c_vr_acc              ', I_cel_c_vr_acc              )
         CALL check_vector_data ('I_lig_c_vr_acc              ', I_lig_c_vr_acc              )
         CALL check_vector_data ('I_cwd_c_vr_acc              ', I_cwd_c_vr_acc              )
         CALL check_vector_data ('AKX_met_to_soil1_c_vr_acc   ', AKX_met_to_soil1_c_vr_acc   )
         CALL check_vector_data ('AKX_cel_to_soil1_c_vr_acc   ', AKX_cel_to_soil1_c_vr_acc   )
         CALL check_vector_data ('AKX_lig_to_soil2_c_vr_acc   ', AKX_lig_to_soil2_c_vr_acc   )
         CALL check_vector_data ('AKX_soil1_to_soil2_c_vr_acc ', AKX_soil1_to_soil2_c_vr_acc )
         CALL check_vector_data ('AKX_cwd_to_cel_c_vr_acc     ', AKX_cwd_to_cel_c_vr_acc     )
         CALL check_vector_data ('AKX_cwd_to_lig_c_vr_acc     ', AKX_cwd_to_lig_c_vr_acc     )
         CALL check_vector_data ('AKX_soil1_to_soil3_c_vr_acc ', AKX_soil1_to_soil3_c_vr_acc )
         CALL check_vector_data ('AKX_soil2_to_soil1_c_vr_acc ', AKX_soil2_to_soil1_c_vr_acc )
         CALL check_vector_data ('AKX_soil2_to_soil3_c_vr_acc ', AKX_soil2_to_soil3_c_vr_acc )
         CALL check_vector_data ('AKX_soil3_to_soil1_c_vr_acc ', AKX_soil3_to_soil1_c_vr_acc )
         CALL check_vector_data ('AKX_met_exit_c_vr_acc       ', AKX_met_exit_c_vr_acc       )
         CALL check_vector_data ('AKX_cel_exit_c_vr_acc       ', AKX_cel_exit_c_vr_acc       )
         CALL check_vector_data ('AKX_lig_exit_c_vr_acc       ', AKX_lig_exit_c_vr_acc       )
         CALL check_vector_data ('AKX_cwd_exit_c_vr_acc       ', AKX_cwd_exit_c_vr_acc       )
         CALL check_vector_data ('AKX_soil1_exit_c_vr_acc     ', AKX_soil1_exit_c_vr_acc     )
         CALL check_vector_data ('AKX_soil2_exit_c_vr_acc     ', AKX_soil2_exit_c_vr_acc     )
         CALL check_vector_data ('AKX_soil3_exit_c_vr_acc     ', AKX_soil3_exit_c_vr_acc     )

         CALL check_vector_data ('decomp0_npools_vr           ', decomp0_npools_vr           )
         CALL check_vector_data ('I_met_n_vr_acc              ', I_met_n_vr_acc              )
         CALL check_vector_data ('I_cel_n_vr_acc              ', I_cel_n_vr_acc              )
         CALL check_vector_data ('I_lig_n_vr_acc              ', I_lig_n_vr_acc              )
         CALL check_vector_data ('I_cwd_n_vr_acc              ', I_cwd_n_vr_acc              )
         CALL check_vector_data ('AKX_met_to_soil1_n_vr_acc   ', AKX_met_to_soil1_n_vr_acc   )
         CALL check_vector_data ('AKX_cel_to_soil1_n_vr_acc   ', AKX_cel_to_soil1_n_vr_acc   )
         CALL check_vector_data ('AKX_lig_to_soil2_n_vr_acc   ', AKX_lig_to_soil2_n_vr_acc   )
         CALL check_vector_data ('AKX_soil1_to_soil2_n_vr_acc ', AKX_soil1_to_soil2_n_vr_acc )
         CALL check_vector_data ('AKX_cwd_to_cel_n_vr_acc     ', AKX_cwd_to_cel_n_vr_acc     )
         CALL check_vector_data ('AKX_cwd_to_lig_n_vr_acc     ', AKX_cwd_to_lig_n_vr_acc     )
         CALL check_vector_data ('AKX_soil1_to_soil3_n_vr_acc ', AKX_soil1_to_soil3_n_vr_acc )
         CALL check_vector_data ('AKX_soil2_to_soil1_n_vr_acc ', AKX_soil2_to_soil1_n_vr_acc )
         CALL check_vector_data ('AKX_soil2_to_soil3_n_vr_acc ', AKX_soil2_to_soil3_n_vr_acc )
         CALL check_vector_data ('AKX_soil3_to_soil1_n_vr_acc ', AKX_soil3_to_soil1_n_vr_acc )
         CALL check_vector_data ('AKX_met_exit_n_vr_acc       ', AKX_met_exit_n_vr_acc       )
         CALL check_vector_data ('AKX_cel_exit_n_vr_acc       ', AKX_cel_exit_n_vr_acc       )
         CALL check_vector_data ('AKX_lig_exit_n_vr_acc       ', AKX_lig_exit_n_vr_acc       )
         CALL check_vector_data ('AKX_cwd_exit_n_vr_acc       ', AKX_cwd_exit_n_vr_acc       )
         CALL check_vector_data ('AKX_soil1_exit_n_vr_acc     ', AKX_soil1_exit_n_vr_acc     )
         CALL check_vector_data ('AKX_soil2_exit_n_vr_acc     ', AKX_soil2_exit_n_vr_acc     )
         CALL check_vector_data ('AKX_soil3_exit_n_vr_acc     ', AKX_soil3_exit_n_vr_acc     )

         CALL check_vector_data ('diagVX_c_vr_acc             ', diagVX_c_vr_acc             )
         CALL check_vector_data ('upperVX_c_vr_acc            ', upperVX_c_vr_acc            )
         CALL check_vector_data ('lowerVX_c_vr_acc            ', lowerVX_c_vr_acc            )
         CALL check_vector_data ('diagVX_n_vr_acc             ', diagVX_n_vr_acc             )
         CALL check_vector_data ('upperVX_n_vr_acc            ', upperVX_n_vr_acc            )
         CALL check_vector_data ('lowerVX_n_vr_acc            ', lowerVX_n_vr_acc            )
 !     CALL check_vector_data ('skip_balance_check          ', skip_balance_check          )
 !------------------------------------------------------
      ENDIF
#ifdef CROP
      CALL check_vector_data ('cphase     ' , cphase     )
      CALL check_vector_data ('vf         ' , vf         )
      CALL check_vector_data ('hui        ' , hui        )
      CALL check_vector_data ('huiswheat  ' , huiswheat  )
      CALL check_vector_data ('gddplant   ' , gddplant   )
      CALL check_vector_data ('gddmaturity' , gddmaturity)
      CALL check_vector_data ('pdcorn     ' , pdcorn     )
      CALL check_vector_data ('pdswheat   ' , pdswheat   )
      CALL check_vector_data ('pdwwheat   ' , pdwwheat   )
      CALL check_vector_data ('pdsoybean  ' , pdsoybean  )
      CALL check_vector_data ('pdcotton   ' , pdcotton   )
      CALL check_vector_data ('pdrice1    ' , pdrice1    )
      CALL check_vector_data ('pdrice2    ' , pdrice2    )
      CALL check_vector_data ('plantdate  ' , plantdate  )
      CALL check_vector_data ('pdsugarcane' , pdsugarcane)
      CALL check_vector_data ('manunitro          ' , manunitro          )
      CALL check_vector_data ('fertnitro_corn     ' , fertnitro_corn     )
      CALL check_vector_data ('fertnitro_swheat   ' , fertnitro_swheat   )
      CALL check_vector_data ('fertnitro_wwheat   ' , fertnitro_wwheat   )
      CALL check_vector_data ('fertnitro_soybean  ' , fertnitro_soybean  )
      CALL check_vector_data ('fertnitro_cotton   ' , fertnitro_cotton   )
      CALL check_vector_data ('fertnitro_rice1    ' , fertnitro_rice1    )
      CALL check_vector_data ('fertnitro_rice2    ' , fertnitro_rice2    )
      CALL check_vector_data ('fertnitro_sugarcane' , fertnitro_sugarcane)
#endif
      CALL check_vector_data ('lag_npp    ' , lag_npp    )

   END SUBROUTINE check_BGCTimeVariables
#endif

#endif
END MODULE MOD_BGC_Vars_TimeVariables
! ------ EOP --------------
