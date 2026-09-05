#include <define.h>

MODULE MOD_BGC_Vars_1DFluxes
#ifdef BGC
!---------------------------------------------------------------------------------------------------------
! !DESCRIPTION
! Define, allocate, and deallocate biogeochemical flux variables at patch level

! !ORIGINAL:
! Xingjie Lu, 2022, created the original version


   USE MOD_Precision
   IMPLICIT NONE
   SAVE

!--------------------- BGC variables --------------------------------------
! ecosystem vegetation carbon/nitrogen flux
   real(r8), allocatable :: gpp                        (:) ! gross primary productivity (gC m-2 s-1)
   real(r8), allocatable :: gpp_enftemp                (:) ! gross primary productivity for needleleaf evergreen temperate tree (gC m-2 s-1)
   real(r8), allocatable :: gpp_enfboreal              (:) ! gross primary productivity for needleleaf evergreen boreal tree (gC m-2 s-1)
   real(r8), allocatable :: gpp_dnfboreal              (:) ! gross primary productivity for needleleaf deciduous boreal tree (gC m-2 s-1)
   real(r8), allocatable :: gpp_ebftrop                (:) ! gross primary productivity for broadleaf evergreen tropical tree (gC m-2 s-1)
   real(r8), allocatable :: gpp_ebftemp                (:) ! gross primary productivity for broadleaf evergreen temperate tree (gC m-2 s-1)
   real(r8), allocatable :: gpp_dbftrop                (:) ! gross primary productivity for broadleaf deciduous tropical tree (gC m-2 s-1)
   real(r8), allocatable :: gpp_dbftemp                (:) ! gross primary productivity for broadleaf deciduous temperate tree (gC m-2 s-1)
   real(r8), allocatable :: gpp_dbfboreal              (:) ! gross primary productivity for broadleaf deciduous boreal tree (gC m-2 s-1)
   real(r8), allocatable :: gpp_ebstemp                (:) ! gross primary productivity for broadleaf evergreen temperate shrub (gC m-2 s-1)
   real(r8), allocatable :: gpp_dbstemp                (:) ! gross primary productivity for broadleaf deciduous temperate shrub (gC m-2 s-1)
   real(r8), allocatable :: gpp_dbsboreal              (:) ! gross primary productivity for broadleaf deciduous boreal shrub (gC m-2 s-1)
   real(r8), allocatable :: gpp_c3arcgrass             (:) ! gross primary productivity for c3 arctic grass (gC m-2 s-1)
   real(r8), allocatable :: gpp_c3grass                (:) ! gross primary productivity for c3 grass (gC m-2 s-1)
   real(r8), allocatable :: gpp_c4grass                (:) ! gross primary productivity for c4 grass (gC m-2 s-1)
   real(r8), allocatable :: npp_enftemp              (:) ! leaf carbon display pool for needleleaf evergreen temperate tree (gC m-2)
   real(r8), allocatable :: npp_enfboreal            (:) ! leaf carbon display pool for needleleaf evergreen boreal tree (gC m-2)
   real(r8), allocatable :: npp_dnfboreal            (:) ! leaf carbon display pool for needleleaf deciduous boreal tree (gC m-2)
   real(r8), allocatable :: npp_ebftrop              (:) ! leaf carbon display pool for broadleaf evergreen tropical tree (gC m-2)
   real(r8), allocatable :: npp_ebftemp              (:) ! leaf carbon display pool for broadleaf evergreen temperate tree (gC m-2)
   real(r8), allocatable :: npp_dbftrop              (:) ! leaf carbon display pool for broadleaf deciduous tropical tree (gC m-2)
   real(r8), allocatable :: npp_dbftemp              (:) ! leaf carbon display pool for broadleaf deciduous temperate tree (gC m-2)
   real(r8), allocatable :: npp_dbfboreal            (:) ! leaf carbon display pool for broadleaf deciduous boreal tree (gC m-2)
   real(r8), allocatable :: npp_ebstemp              (:) ! leaf carbon display pool for broadleaf evergreen temperate shrub (gC m-2)
   real(r8), allocatable :: npp_dbstemp              (:) ! leaf carbon display pool for broadleaf deciduous temperate shrub (gC m-2)
   real(r8), allocatable :: npp_dbsboreal            (:) ! leaf carbon display pool for broadleaf deciduous boreal shrub (gC m-2)
   real(r8), allocatable :: npp_c3arcgrass           (:) ! leaf carbon display pool for c3 arctic grass (gC m-2)
   real(r8), allocatable :: npp_c3grass              (:) ! leaf carbon display pool for c3 grass (gC m-2)
   real(r8), allocatable :: npp_c4grass              (:) ! leaf carbon display pool for c4 grass (gC m-2)
   real(r8), allocatable :: npptoleafc_enftemp              (:) ! leaf carbon display pool for needleleaf evergreen temperate tree (gC m-2)
   real(r8), allocatable :: npptoleafc_enfboreal            (:) ! leaf carbon display pool for needleleaf evergreen boreal tree (gC m-2)
   real(r8), allocatable :: npptoleafc_dnfboreal            (:) ! leaf carbon display pool for needleleaf deciduous boreal tree (gC m-2)
   real(r8), allocatable :: npptoleafc_ebftrop              (:) ! leaf carbon display pool for broadleaf evergreen tropical tree (gC m-2)
   real(r8), allocatable :: npptoleafc_ebftemp              (:) ! leaf carbon display pool for broadleaf evergreen temperate tree (gC m-2)
   real(r8), allocatable :: npptoleafc_dbftrop              (:) ! leaf carbon display pool for broadleaf deciduous tropical tree (gC m-2)
   real(r8), allocatable :: npptoleafc_dbftemp              (:) ! leaf carbon display pool for broadleaf deciduous temperate tree (gC m-2)
   real(r8), allocatable :: npptoleafc_dbfboreal            (:) ! leaf carbon display pool for broadleaf deciduous boreal tree (gC m-2)
   real(r8), allocatable :: npptoleafc_ebstemp              (:) ! leaf carbon display pool for broadleaf evergreen temperate shrub (gC m-2)
   real(r8), allocatable :: npptoleafc_dbstemp              (:) ! leaf carbon display pool for broadleaf deciduous temperate shrub (gC m-2)
   real(r8), allocatable :: npptoleafc_dbsboreal            (:) ! leaf carbon display pool for broadleaf deciduous boreal shrub (gC m-2)
   real(r8), allocatable :: npptoleafc_c3arcgrass           (:) ! leaf carbon display pool for c3 arctic grass (gC m-2)
   real(r8), allocatable :: npptoleafc_c3grass              (:) ! leaf carbon display pool for c3 grass (gC m-2)
   real(r8), allocatable :: npptoleafc_c4grass              (:) ! leaf carbon display pool for c4 grass (gC m-2)
   real(r8), allocatable :: leafc_enftemp              (:) ! leaf carbon display pool for needleleaf evergreen temperate tree (gC m-2)
   real(r8), allocatable :: leafc_enfboreal            (:) ! leaf carbon display pool for needleleaf evergreen boreal tree (gC m-2)
   real(r8), allocatable :: leafc_dnfboreal            (:) ! leaf carbon display pool for needleleaf deciduous boreal tree (gC m-2)
   real(r8), allocatable :: leafc_ebftrop              (:) ! leaf carbon display pool for broadleaf evergreen tropical tree (gC m-2)
   real(r8), allocatable :: leafc_ebftemp              (:) ! leaf carbon display pool for broadleaf evergreen temperate tree (gC m-2)
   real(r8), allocatable :: leafc_dbftrop              (:) ! leaf carbon display pool for broadleaf deciduous tropical tree (gC m-2)
   real(r8), allocatable :: leafc_dbftemp              (:) ! leaf carbon display pool for broadleaf deciduous temperate tree (gC m-2)
   real(r8), allocatable :: leafc_dbfboreal            (:) ! leaf carbon display pool for broadleaf deciduous boreal tree (gC m-2)
   real(r8), allocatable :: leafc_ebstemp              (:) ! leaf carbon display pool for broadleaf evergreen temperate shrub (gC m-2)
   real(r8), allocatable :: leafc_dbstemp              (:) ! leaf carbon display pool for broadleaf deciduous temperate shrub (gC m-2)
   real(r8), allocatable :: leafc_dbsboreal            (:) ! leaf carbon display pool for broadleaf deciduous boreal shrub (gC m-2)
   real(r8), allocatable :: leafc_c3arcgrass           (:) ! leaf carbon display pool for c3 arctic grass (gC m-2)
   real(r8), allocatable :: leafc_c3grass              (:) ! leaf carbon display pool for c3 grass (gC m-2)
   real(r8), allocatable :: leafc_c4grass              (:) ! leaf carbon display pool for c4 grass (gC m-2)
   real(r8), allocatable :: ar                         (:) ! autotrophic respiration (gC m-2 s-1)
   real(r8), allocatable :: cwdprod                    (:) ! CWD production (gC m-2 s-1)
   real(r8), allocatable :: cwddecomp                  (:) ! CWD decomposition (gC m-2 s-1)
   real(r8), allocatable :: hr                         (:) ! heterotrophic respiration (gC m-2 s-1)
   real(r8), allocatable :: er                         (:) ! total ecosystem respiration, autotrophic + heterotrophic (gC m-2 s-1)
   real(r8), allocatable :: fire_closs                 (:) ! total C emissions due to fire (gC m-2 s-1)
   real(r8), allocatable :: fire_nloss                 (:) ! total N emissions due to fire (gN m-2 s-1)
   real(r8), allocatable :: hrv_xsmrpool_to_atm        (:) ! maintenance respiration storage C to atmosphere due to harvest (gC m-2 s-1)
   real(r8), allocatable :: wood_harvestc              (:) ! harvested wood C (gC m-2 s-1)
   real(r8), allocatable :: wood_harvestn              (:) ! harvested wood N (gN m-2 s-1)
   real(r8), allocatable :: grainc_to_cropprodc        (:) ! grain to crop production carbon (gC m-2 s-1)
   real(r8), allocatable :: grainc_to_seed             (:) ! grain to crop seed carbon (gC m-2 s-1)
   real(r8), allocatable :: grainn_to_cropprodn        (:) ! grain to crop production nitrogen (gN m-2 s-1)
   real(r8), allocatable :: cropprod1c_loss            (:) ! loss rate of 1-yr crop production carbon (gC m-2 s-1)

 ! decomposition carbon fluxes
   real(r8), allocatable :: decomp_cpools_sourcesink   (:,:,:)       ! vertical resolved: the input of litter & soil carbon pools (donor or receiver) from phenology-associated litterfall and decomposition (gC m-3 timestep-1)
   real(r8), allocatable :: decomp_ctransfer_vr        (:,:,:)       ! vertical resolved: the non-respiratory portion of potential carbon transfer from one litter & soil carbon pool to another (gC m-3 s-1)
   real(r8), allocatable :: decomp_hr_vr               (:,:,:)       ! vertical resolved: the heterotrophic respiration portion of potential carbon loss from one litter & soil carbon pool to another (gC m-3 s-1)
   real(r8), allocatable :: decomp_hr                  (:)           ! the heterotrophic respiration portion of potential carbon loss from one decomposition carbon pool to another (gC m-3 s-1)
   real(r8), allocatable :: phr_vr                     (:,:)         ! vertical resolved: the potential heterotrophic respiration carbon flux (gC m-3 s-1)
   real(r8), allocatable :: m_decomp_cpools_to_fire_vr (:,:,:)       ! vertical resolved: the carbon from decomposition pools to fire emissions (gC m-3 s-1)
   real(r8), allocatable :: decomp_cpools_transport_tendency(:,:,:)  ! vertical resolved: the carbon tendency due to vertical transport in decomposition carbon pools (gC m-3 s-1)
   real(r8), allocatable :: som_c_leached              (:)           ! total soil organic matter C loss from vertical transport (gC m-2 s-1)

 ! vegetation to decomposition carbon fluxes
   real(r8), allocatable :: phenology_to_met_c       (:,:) ! phenology-associated plant C loss to metabolic litter C (gC m-3 s-1)
   real(r8), allocatable :: phenology_to_cel_c       (:,:) ! phenology-associated plant C loss to cellulosic litter C (gC m-3 s-1)
   real(r8), allocatable :: phenology_to_lig_c       (:,:) ! phenology-associated plant C loss to lignin litter C (gC m-3 s-1)
   real(r8), allocatable :: gap_mortality_to_met_c   (:,:) ! gap mortality-associated plant C loss to metabolic litter C (gC m-3 s-1)
   real(r8), allocatable :: gap_mortality_to_cel_c   (:,:) ! gap mortality-associated plant C loss to cellulosic litter C (gC m-3 s-1)
   real(r8), allocatable :: gap_mortality_to_lig_c   (:,:) ! gap mortality-associated plant C loss to lignin litter C (gC m-3 s-1)
   real(r8), allocatable :: gap_mortality_to_cwdc    (:,:) ! gap mortality-associated plant C loss to coarse woody debris C (gC m-3 s-1)
   real(r8), allocatable :: fire_mortality_to_met_c  (:,:) ! fire mortality-associated plant C loss to metabolic litter C (gC m-3 s-1)
   real(r8), allocatable :: fire_mortality_to_cel_c  (:,:) ! fire mortality-associated plant C loss to cellulosic litter C (gC m-3 s-1)
   real(r8), allocatable :: fire_mortality_to_lig_c  (:,:) ! fire mortality-associated plant C loss to lignin litter C (gC m-3 s-1)
   real(r8), allocatable :: fire_mortality_to_cwdc   (:,:) ! fire mortality-associated plant C loss to coarse woody debris C (gC m-3 s-1)

 ! decomposition nitrogen fluxes
   real(r8), allocatable :: decomp_npools_sourcesink   (:,:,:)       ! vertical resolved: the the input of litter & soil nitrogen pools (donor or receiver) (gN m-3 timestep)
   real(r8), allocatable :: decomp_ntransfer_vr        (:,:,:)       ! vertical resolved: the nitrogen flux transfer from one litter & soil nitrogen pool to another (gN m-3 s-1)
   real(r8), allocatable :: decomp_sminn_flux_vr       (:,:,:)       ! vertical resolved: the nitrogen mineralization flux from each nitrogen transfer between litter & soil pools (gN m-3 s-1)
   real(r8), allocatable :: sminn_to_denit_decomp_vr   (:,:,:)       ! vertical resolved: the nitrogen denitrification flux from each nitrogen transfer between litter & soil pools (gN m-3 s-1)
   real(r8), allocatable :: m_decomp_npools_to_fire_vr (:,:,:)       ! vertical resolved: the litter & soil nitrogen loss associated to the fire (gN m-3 s-1)
   real(r8), allocatable :: decomp_npools_transport_tendency(:,:,:)  ! vertical resolved: the nitrogen tendency due to vertical transport in decomposition nitrogen pools (gN m-3 s-1)
   real(r8), allocatable :: som_n_leached              (:)           ! total soil organic matter N loss from vertical transport (gN m-2 s-1)

 ! vegetation to decomposition nitrogen fluxes
   real(r8), allocatable :: phenology_to_met_n       (:,:)   ! phenology-associated plant N loss to metabolic litter N (gN m-3 s-1)
   real(r8), allocatable :: phenology_to_cel_n       (:,:)   ! phenology-associated plant N loss to cellulosic litter N (gN m-3 s-1)
   real(r8), allocatable :: phenology_to_lig_n       (:,:)   ! phenology-associated plant N loss to lignin litter N (gN m-3 s-1)
   real(r8), allocatable :: gap_mortality_to_met_n   (:,:)   ! gap mortality-associated plant N loss to metabolic litter N (gN m-3 s-1)
   real(r8), allocatable :: gap_mortality_to_cel_n   (:,:)   ! gap mortality-associated plant N loss to cellulosic litter N (gN m-3 s-1)
   real(r8), allocatable :: gap_mortality_to_lig_n   (:,:)   ! gap mortality-associated plant N loss to lignin litter N (gN m-3 s-1)
   real(r8), allocatable :: gap_mortality_to_cwdn    (:,:)   ! gap mortality-associated plant N loss to coarse woody debris N (gN m-3 s-1)
   real(r8), allocatable :: fire_mortality_to_met_n  (:,:)   ! fire mortality-associated plant N loss to metabolic litter N (gN m-3 s-1)
   real(r8), allocatable :: fire_mortality_to_cel_n  (:,:)   ! fire mortality-associated plant N loss to cellulosic litter N (gN m-3 s-1)
   real(r8), allocatable :: fire_mortality_to_lig_n  (:,:)   ! fire mortality-associated plant N loss to lignin litter N (gN m-3 s-1)
   real(r8), allocatable :: fire_mortality_to_cwdn   (:,:)   ! fire mortality-associated plant N loss to coarse woody debris N (gN m-3 s-1)

   real(r8), allocatable :: sminn_leached_vr         (:,:)   ! vertical resolved: soil mineral N loss due to leaching (gN m-3 s-1)
   real(r8), allocatable :: smin_no3_leached_vr      (:,:)   ! vertical resolved: soil mineral NO3 loss due to leaching (gN m-3 s-1)
   real(r8), allocatable :: smin_no3_runoff_vr       (:,:)   ! vertical resolved: soil mineral NO3 loss due to runoff (gN m-3 s-1)
   real(r8), allocatable :: net_nmin_vr              (:,:)   ! vertical resolved: net N mineralization (gN m-3 s-1)
   real(r8), allocatable :: gross_nmin_vr            (:,:)   ! vertical resolved: total N mineralization (gN m-3 s-1)
   real(r8), allocatable :: net_nmin                 (:)     ! net N mineralization (gN m-2 s-1)
   real(r8), allocatable :: gross_nmin               (:)     ! total N mineralization (gN m-2 s-1)
   real(r8), allocatable :: plant_ndemand            (:)     ! potential plant N uptake (gN m-2 s-1)
   real(r8), allocatable :: actual_immob_vr          (:,:)   ! vertical resolved: actual N immobilization (gN m-3 s-1)
   real(r8), allocatable :: actual_immob_nh4_vr      (:,:)   ! vertical resolved: actual NH4 immobilization (gN m-3 s-1)
   real(r8), allocatable :: actual_immob_no3_vr      (:,:)   ! vertical resolved: actual NO3 immobilization (gN m-3 s-1)
   real(r8), allocatable :: potential_immob_vr       (:,:)   ! vertical resolved: potential N immobilization (gN m-3 s-1)
   real(r8), allocatable :: pmnf_decomp              (:,:,:) ! vertical resolved: potential N mineralization flux of each transfer between litter & soil pools (gN m-3 s-1)
   real(r8), allocatable :: p_decomp_cpool_loss      (:,:,:) ! vertical resolved: potential C exit rate (transfer+hr) of the donor pool of each transfer between litter & soil pools (gC m-3 s-1)
   real(r8), allocatable :: sminn_to_plant           (:)     ! plant uptake N (gN m-2 s-1)
   real(r8), allocatable :: sminn_to_plant_vr        (:,:)   ! vertical resolved: plant uptake N (gN m-3 s-1)
   real(r8), allocatable :: smin_nh4_to_plant_vr     (:,:)   ! vertical resolved: plant uptake NH4 (gN m-3 s-1)
   real(r8), allocatable :: smin_no3_to_plant_vr     (:,:)   ! vertical resolved: plant uptake NO3 (gN m-3 s-1)
   real(r8), allocatable :: supplement_to_sminn_vr   (:,:)   ! vertical resolved: supplemental N supply to soil mineral N (gN m-3 s-1)
   real(r8), allocatable :: sminn_to_plant_fun_vr    (:,:)   ! vertical resolved: plant uptake N by FUN (gN m-3 s-1)
   real(r8), allocatable :: sminn_to_plant_fun_nh4_vr(:,:)   ! vertical resolved: plant uptake NH4 by FUN (gN m-3 s-1)
   real(r8), allocatable :: sminn_to_plant_fun_no3_vr(:,:)   ! vertical resolved: plant uptake NO3 by FUN (gN m-3 s-1)
   real(r8), allocatable :: sminn_to_denit_excess_vr (:,:)   ! vertical resolved: denitrification from excess mineral N (gN m-3 s-1)
   real(r8), allocatable :: f_nit_vr                 (:,:)   ! vertical resolved: nitrification (gN m-3 s-1)
   real(r8), allocatable :: f_denit_vr               (:,:)   ! vertical resolved: denitrification (gN m-3 s-1)
   real(r8), allocatable :: f_n2o_nit_vr             (:,:)   ! vertical resolved: N2O emission from N nitrification (gN m-3 s-1)
   real(r8), allocatable :: f_n2o_denit_vr           (:,:)   ! vertical resolved: N2O emission from N denitrification (gN m-3 s-1)
   real(r8), allocatable :: pot_f_nit_vr             (:,:)   ! vertical resolved: potential N nitrification (gN m-3 s-1)
   real(r8), allocatable :: pot_f_denit_vr           (:,:)   ! vertical resolved: potential N denitrification (gN m-3 s-1)
   real(r8), allocatable :: n2_n2o_ratio_denit_vr    (:,:)   ! vertical resolved: ratio of N2 to N2O production by denitrification (gN gN-1)
   real(r8), allocatable :: ndep_to_sminn            (:)     ! atmospheric N deposition to soil mineral N (gN m-2 s-1)
   real(r8), allocatable :: ffix_to_sminn            (:)     ! free living N fixation to soil mineral N (gN m-2 s-1)
   real(r8), allocatable :: nfix_to_sminn            (:)     ! N fixation to soil mineral N (gN m-2 s-1)
   real(r8), allocatable :: somc_fire                (:)     ! soil organic matters C to fire emissions (gC m-2 s-1)
   real(r8), allocatable :: supplement_to_sminn      (:)     ! supplemental N supply to soil mineral N (gN m-2 s-1)
   real(r8), allocatable :: fert_to_sminn            (:)     ! fertilizer N to soil mineral N (gN m-2 s-1)
   real(r8), allocatable :: soyfixn_to_sminn         (:)     ! soybean N fixation to soil mineral N (gN m-2 s-1)
   real(r8), allocatable :: denit                    (:)     ! total N denitrification (gN m-2 s-1)
   real(r8), allocatable :: sminn_leached            (:)     ! soil mineral N loss due to leaching (gN m-2 s-1)
   real(r8), allocatable :: f_n2o_nit                (:)     ! flux of N2O from N nitrification (gN m-2 s-1)
   real(r8), allocatable :: smin_no3_leached         (:)     ! soil mineral NO3 loss due to leaching (gN m-2 s-1)
   real(r8), allocatable :: smin_no3_runoff          (:)     ! soil mineral NO3 loss due to runoff (gN m-2 s-1)
 !----------------- end BGC variables -----------------------------------

 ! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: allocate_1D_BGCFluxes
   PUBLIC :: deallocate_1D_BGCFluxes
   PUBLIC :: set_1D_BGCFluxes

! PRIVATE MEMBER FUNCTIONS:

!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------

   SUBROUTINE allocate_1D_BGCFluxes
  ! --------------------------------------------------------------------
  ! Allocates memory for CoLM 1d [numpatch] variables
  ! --------------------------------------------------------------------
   USE MOD_Precision
   USE MOD_Vars_Global
   USE MOD_MPAS_MPI
   USE MOD_LandPatch
   IMPLICIT NONE


      IF (.true.) THEN

         IF (numpatch > 0) THEN

! bgc variables
! ecosystem carbon flux
            allocate (gpp                        (numpatch)) ; gpp                        (:) = spval
            allocate (gpp_enftemp                (numpatch)) ; gpp_enftemp                (:) = spval
            allocate (gpp_enfboreal              (numpatch)) ; gpp_enfboreal              (:) = spval
            allocate (gpp_dnfboreal              (numpatch)) ; gpp_dnfboreal              (:) = spval
            allocate (gpp_ebftrop                (numpatch)) ; gpp_ebftrop                (:) = spval
            allocate (gpp_ebftemp                (numpatch)) ; gpp_ebftemp                (:) = spval
            allocate (gpp_dbftrop                (numpatch)) ; gpp_dbftrop                (:) = spval
            allocate (gpp_dbftemp                (numpatch)) ; gpp_dbftemp                (:) = spval
            allocate (gpp_dbfboreal              (numpatch)) ; gpp_dbfboreal              (:) = spval
            allocate (gpp_ebstemp                (numpatch)) ; gpp_ebstemp                (:) = spval
            allocate (gpp_dbstemp                (numpatch)) ; gpp_dbstemp                (:) = spval
            allocate (gpp_dbsboreal              (numpatch)) ; gpp_dbsboreal              (:) = spval
            allocate (gpp_c3arcgrass             (numpatch)) ; gpp_c3arcgrass             (:) = spval
            allocate (gpp_c3grass                (numpatch)) ; gpp_c3grass                (:) = spval
            allocate (gpp_c4grass                (numpatch)) ; gpp_c4grass                (:) = spval
            allocate (npp_enftemp                (numpatch)) ; npp_enftemp                (:) = spval
            allocate (npp_enfboreal              (numpatch)) ; npp_enfboreal              (:) = spval
            allocate (npp_dnfboreal              (numpatch)) ; npp_dnfboreal              (:) = spval
            allocate (npp_ebftrop                (numpatch)) ; npp_ebftrop                (:) = spval
            allocate (npp_ebftemp                (numpatch)) ; npp_ebftemp                (:) = spval
            allocate (npp_dbftrop                (numpatch)) ; npp_dbftrop                (:) = spval
            allocate (npp_dbftemp                (numpatch)) ; npp_dbftemp                (:) = spval
            allocate (npp_dbfboreal              (numpatch)) ; npp_dbfboreal              (:) = spval
            allocate (npp_ebstemp                (numpatch)) ; npp_ebstemp                (:) = spval
            allocate (npp_dbstemp                (numpatch)) ; npp_dbstemp                (:) = spval
            allocate (npp_dbsboreal              (numpatch)) ; npp_dbsboreal              (:) = spval
            allocate (npp_c3arcgrass             (numpatch)) ; npp_c3arcgrass             (:) = spval
            allocate (npp_c3grass                (numpatch)) ; npp_c3grass                (:) = spval
            allocate (npp_c4grass                (numpatch)) ; npp_c4grass                (:) = spval
            allocate (npptoleafc_enftemp         (numpatch)) ; npptoleafc_enftemp         (:) = spval
            allocate (npptoleafc_enfboreal       (numpatch)) ; npptoleafc_enfboreal       (:) = spval
            allocate (npptoleafc_dnfboreal       (numpatch)) ; npptoleafc_dnfboreal       (:) = spval
            allocate (npptoleafc_ebftrop         (numpatch)) ; npptoleafc_ebftrop         (:) = spval
            allocate (npptoleafc_ebftemp         (numpatch)) ; npptoleafc_ebftemp         (:) = spval
            allocate (npptoleafc_dbftrop         (numpatch)) ; npptoleafc_dbftrop         (:) = spval
            allocate (npptoleafc_dbftemp         (numpatch)) ; npptoleafc_dbftemp         (:) = spval
            allocate (npptoleafc_dbfboreal       (numpatch)) ; npptoleafc_dbfboreal       (:) = spval
            allocate (npptoleafc_ebstemp         (numpatch)) ; npptoleafc_ebstemp         (:) = spval
            allocate (npptoleafc_dbstemp         (numpatch)) ; npptoleafc_dbstemp         (:) = spval
            allocate (npptoleafc_dbsboreal       (numpatch)) ; npptoleafc_dbsboreal       (:) = spval
            allocate (npptoleafc_c3arcgrass      (numpatch)) ; npptoleafc_c3arcgrass      (:) = spval
            allocate (npptoleafc_c3grass         (numpatch)) ; npptoleafc_c3grass         (:) = spval
            allocate (npptoleafc_c4grass         (numpatch)) ; npptoleafc_c4grass         (:) = spval
            allocate (leafc_enftemp              (numpatch)) ; leafc_enftemp              (:) = spval
            allocate (leafc_enfboreal            (numpatch)) ; leafc_enfboreal            (:) = spval
            allocate (leafc_dnfboreal            (numpatch)) ; leafc_dnfboreal            (:) = spval
            allocate (leafc_ebftrop              (numpatch)) ; leafc_ebftrop              (:) = spval
            allocate (leafc_ebftemp              (numpatch)) ; leafc_ebftemp              (:) = spval
            allocate (leafc_dbftrop              (numpatch)) ; leafc_dbftrop              (:) = spval
            allocate (leafc_dbftemp              (numpatch)) ; leafc_dbftemp              (:) = spval
            allocate (leafc_dbfboreal            (numpatch)) ; leafc_dbfboreal            (:) = spval
            allocate (leafc_ebstemp              (numpatch)) ; leafc_ebstemp              (:) = spval
            allocate (leafc_dbstemp              (numpatch)) ; leafc_dbstemp              (:) = spval
            allocate (leafc_dbsboreal            (numpatch)) ; leafc_dbsboreal            (:) = spval
            allocate (leafc_c3arcgrass           (numpatch)) ; leafc_c3arcgrass           (:) = spval
            allocate (leafc_c3grass              (numpatch)) ; leafc_c3grass              (:) = spval
            allocate (leafc_c4grass              (numpatch)) ; leafc_c4grass              (:) = spval
            allocate (ar                         (numpatch)) ; ar                         (:) = spval
            allocate (cwdprod                    (numpatch)) ; cwdprod                    (:) = spval
            allocate (cwddecomp                  (numpatch)) ; cwddecomp                  (:) = spval
            allocate (hr                         (numpatch)) ; hr                         (:) = spval
            allocate (er                         (numpatch)) ; er                         (:) = spval
            allocate (fire_closs                 (numpatch)) ; fire_closs                 (:) = spval
            allocate (fire_nloss                 (numpatch)) ; fire_nloss                 (:) = spval
            allocate (hrv_xsmrpool_to_atm        (numpatch)) ; hrv_xsmrpool_to_atm        (:) = spval
            allocate (wood_harvestc              (numpatch)) ; wood_harvestc              (:) = spval
            allocate (wood_harvestn              (numpatch)) ; wood_harvestn              (:) = spval
            allocate (grainc_to_cropprodc        (numpatch)) ; grainc_to_cropprodc        (:) = spval
            allocate (grainc_to_seed             (numpatch)) ; grainc_to_seed             (:) = spval
            allocate (grainn_to_cropprodn        (numpatch)) ; grainn_to_cropprodn        (:) = spval
            allocate (cropprod1c_loss            (numpatch)) ; cropprod1c_loss            (:) = spval


! decomposition carbon fluxes
            allocate (decomp_cpools_sourcesink   (nl_soil_full,ndecomp_pools,numpatch)); decomp_cpools_sourcesink   (:,:,:) = spval
            allocate (decomp_ctransfer_vr        (nl_soil_full,ndecomp_transitions,numpatch)); decomp_ctransfer_vr  (:,:,:) = spval
            allocate (decomp_hr_vr               (nl_soil_full,ndecomp_transitions,numpatch)); decomp_hr_vr         (:,:,:) = spval
            allocate (decomp_hr                  (numpatch)) ; decomp_hr                  (:) = spval
            allocate (phr_vr                     (nl_soil_full,numpatch)); phr_vr                     (:,:) = spval
            allocate (m_decomp_cpools_to_fire_vr (nl_soil_full,ndecomp_pools,numpatch)); m_decomp_cpools_to_fire_vr (:,:,:) = spval
            allocate (decomp_cpools_transport_tendency(nl_soil_full,ndecomp_pools,numpatch)); decomp_cpools_transport_tendency(:,:,:) = spval
            allocate (som_c_leached              (numpatch)) ; som_c_leached              (:) = spval

! vegetation to decomposition carbon fluxes
            allocate (phenology_to_met_c       (nl_soil,numpatch)); phenology_to_met_c       (:,:) = spval
            allocate (phenology_to_cel_c       (nl_soil,numpatch)); phenology_to_cel_c       (:,:) = spval
            allocate (phenology_to_lig_c       (nl_soil,numpatch)); phenology_to_lig_c       (:,:) = spval
            allocate (gap_mortality_to_met_c   (nl_soil,numpatch)); gap_mortality_to_met_c   (:,:) = spval
            allocate (gap_mortality_to_cel_c   (nl_soil,numpatch)); gap_mortality_to_cel_c   (:,:) = spval
            allocate (gap_mortality_to_lig_c   (nl_soil,numpatch)); gap_mortality_to_lig_c   (:,:) = spval
            allocate (gap_mortality_to_cwdc    (nl_soil,numpatch)); gap_mortality_to_cwdc    (:,:) = spval
            allocate (fire_mortality_to_met_c  (nl_soil,numpatch)); fire_mortality_to_met_c  (:,:) = spval
            allocate (fire_mortality_to_cel_c  (nl_soil,numpatch)); fire_mortality_to_cel_c  (:,:) = spval
            allocate (fire_mortality_to_lig_c  (nl_soil,numpatch)); fire_mortality_to_lig_c  (:,:) = spval
            allocate (fire_mortality_to_cwdc   (nl_soil,numpatch)); fire_mortality_to_cwdc   (:,:) = spval

! decomposition nitrogen fluxes
            allocate (decomp_npools_sourcesink   (nl_soil_full,ndecomp_pools,numpatch)); decomp_npools_sourcesink      (:,:,:) = spval
            allocate (decomp_ntransfer_vr        (nl_soil_full,ndecomp_transitions,numpatch)); decomp_ntransfer_vr     (:,:,:) = spval
            allocate (decomp_sminn_flux_vr       (nl_soil_full,ndecomp_transitions,numpatch)); decomp_sminn_flux_vr    (:,:,:) = spval
            allocate (sminn_to_denit_decomp_vr   (nl_soil_full,ndecomp_transitions,numpatch)); sminn_to_denit_decomp_vr(:,:,:) = spval
            allocate (m_decomp_npools_to_fire_vr (nl_soil_full,ndecomp_pools,numpatch)); m_decomp_npools_to_fire_vr    (:,:,:) = spval
            allocate (decomp_npools_transport_tendency(nl_soil_full,ndecomp_pools,numpatch)); decomp_npools_transport_tendency(:,:,:) = spval
            allocate (som_n_leached            (numpatch)) ; som_n_leached            (:) = spval

! vegetation to decomposition nitrogen fluxes
            allocate (phenology_to_met_n       (nl_soil,numpatch)); phenology_to_met_n       (:,:) = spval
            allocate (phenology_to_cel_n       (nl_soil,numpatch)); phenology_to_cel_n       (:,:) = spval
            allocate (phenology_to_lig_n       (nl_soil,numpatch)); phenology_to_lig_n       (:,:) = spval
            allocate (gap_mortality_to_met_n   (nl_soil,numpatch)); gap_mortality_to_met_n   (:,:) = spval
            allocate (gap_mortality_to_cel_n   (nl_soil,numpatch)); gap_mortality_to_cel_n   (:,:) = spval
            allocate (gap_mortality_to_lig_n   (nl_soil,numpatch)); gap_mortality_to_lig_n   (:,:) = spval
            allocate (gap_mortality_to_cwdn    (nl_soil,numpatch)); gap_mortality_to_cwdn    (:,:) = spval
            allocate (fire_mortality_to_met_n  (nl_soil,numpatch)); fire_mortality_to_met_n  (:,:) = spval
            allocate (fire_mortality_to_cel_n  (nl_soil,numpatch)); fire_mortality_to_cel_n  (:,:) = spval
            allocate (fire_mortality_to_lig_n  (nl_soil,numpatch)); fire_mortality_to_lig_n  (:,:) = spval
            allocate (fire_mortality_to_cwdn   (nl_soil,numpatch)); fire_mortality_to_cwdn   (:,:) = spval

            allocate (sminn_leached_vr         (nl_soil,numpatch)); sminn_leached_vr         (:,:) = spval
            allocate (smin_no3_leached_vr      (nl_soil,numpatch)); smin_no3_leached_vr      (:,:) = spval
            allocate (smin_no3_runoff_vr       (nl_soil,numpatch)); smin_no3_runoff_vr       (:,:) = spval
            allocate (net_nmin_vr              (nl_soil,numpatch)); net_nmin_vr              (:,:) = spval
            allocate (gross_nmin_vr            (nl_soil,numpatch)); gross_nmin_vr            (:,:) = spval
            allocate (net_nmin                 (numpatch)) ; net_nmin                 (:) = spval
            allocate (gross_nmin               (numpatch)) ; gross_nmin               (:) = spval
            allocate (plant_ndemand            (numpatch)) ; plant_ndemand            (:) = spval
            allocate (actual_immob_vr          (nl_soil,numpatch)); actual_immob_vr          (:,:) = spval
            allocate (actual_immob_nh4_vr      (nl_soil,numpatch)); actual_immob_nh4_vr      (:,:) = spval
            allocate (actual_immob_no3_vr      (nl_soil,numpatch)); actual_immob_no3_vr      (:,:) = spval
            allocate (potential_immob_vr       (nl_soil,numpatch)); potential_immob_vr       (:,:) = spval
            allocate (pmnf_decomp              (nl_soil,ndecomp_transitions,numpatch)); pmnf_decomp              (:,:,:) = spval
            allocate (p_decomp_cpool_loss      (nl_soil,ndecomp_transitions,numpatch)); p_decomp_cpool_loss      (:,:,:) = spval
            allocate (sminn_to_plant           (numpatch)) ; sminn_to_plant           (:) = spval
            allocate (sminn_to_plant_vr        (nl_soil,numpatch)); sminn_to_plant_vr        (:,:) = spval
            allocate (smin_nh4_to_plant_vr     (nl_soil,numpatch)); smin_nh4_to_plant_vr     (:,:) = spval
            allocate (smin_no3_to_plant_vr     (nl_soil,numpatch)); smin_no3_to_plant_vr     (:,:) = spval
            allocate (supplement_to_sminn_vr   (nl_soil,numpatch)); supplement_to_sminn_vr   (:,:) = spval
            allocate (sminn_to_plant_fun_vr    (nl_soil,numpatch)); sminn_to_plant_fun_vr    (:,:) = spval
            allocate (sminn_to_plant_fun_nh4_vr(nl_soil,numpatch)); sminn_to_plant_fun_nh4_vr(:,:) = spval
            allocate (sminn_to_plant_fun_no3_vr(nl_soil,numpatch)); sminn_to_plant_fun_no3_vr(:,:) = spval
            allocate (sminn_to_denit_excess_vr (nl_soil,numpatch)); sminn_to_denit_excess_vr (:,:) = spval
            allocate (f_nit_vr                 (nl_soil,numpatch)); f_nit_vr                 (:,:) = spval
            allocate (f_denit_vr               (nl_soil,numpatch)); f_denit_vr               (:,:) = spval
            allocate (f_n2o_nit_vr             (nl_soil,numpatch)); f_n2o_nit_vr             (:,:) = spval
            allocate (f_n2o_denit_vr           (nl_soil,numpatch)); f_n2o_denit_vr           (:,:) = spval
            allocate (pot_f_nit_vr             (nl_soil,numpatch)); pot_f_nit_vr             (:,:) = spval
            allocate (pot_f_denit_vr           (nl_soil,numpatch)); pot_f_denit_vr           (:,:) = spval
            allocate (n2_n2o_ratio_denit_vr    (nl_soil,numpatch)); n2_n2o_ratio_denit_vr    (:,:) = spval
            allocate (ndep_to_sminn            (numpatch)) ; ndep_to_sminn            (:) = spval
            allocate (ffix_to_sminn            (numpatch)) ; ffix_to_sminn            (:) = spval
            allocate (nfix_to_sminn            (numpatch)) ; nfix_to_sminn            (:) = spval
            allocate (somc_fire                (numpatch)) ; somc_fire                (:) = spval
            allocate (supplement_to_sminn      (numpatch)) ; supplement_to_sminn      (:) = spval
            allocate (fert_to_sminn            (numpatch)) ; fert_to_sminn            (:) = spval
            allocate (soyfixn_to_sminn         (numpatch)) ; soyfixn_to_sminn         (:) = spval
            allocate (denit                    (numpatch)) ; denit                    (:) = spval
            allocate (sminn_leached            (numpatch)) ; sminn_leached            (:) = spval
            allocate (f_n2o_nit                (numpatch)) ; f_n2o_nit                (:) = spval
            allocate (smin_no3_leached         (numpatch)) ; smin_no3_leached         (:) = spval
            allocate (smin_no3_runoff          (numpatch)) ; smin_no3_runoff          (:) = spval
         ENDIF
      ENDIF


   END SUBROUTINE allocate_1D_BGCFluxes

   SUBROUTINE deallocate_1D_BGCFluxes ()
  ! --------------------------------------------------------------------
  ! deallocates memory for CoLM 1d [numpatch] variables
  ! --------------------------------------------------------------------
   USE MOD_MPAS_MPI
   USE MOD_LandPatch

      IF (.true.) THEN

         IF (numpatch > 0) THEN

! bgc variables
! ecosystem carbon flux
            deallocate (gpp                        )
            deallocate (gpp_enftemp                ) !1
            deallocate (gpp_enfboreal              ) !2
            deallocate (gpp_dnfboreal              ) !3
            deallocate (gpp_ebftrop                ) !4
            deallocate (gpp_ebftemp                ) !5
            deallocate (gpp_dbftrop                ) !6
            deallocate (gpp_dbftemp                ) !7
            deallocate (gpp_dbfboreal              ) !8
            deallocate (gpp_ebstemp                ) !9
            deallocate (gpp_dbstemp                ) !10
            deallocate (gpp_dbsboreal              ) !11
            deallocate (gpp_c3arcgrass             ) !12
            deallocate (gpp_c3grass                ) !13
            deallocate (gpp_c4grass                ) !14
            deallocate (npp_enftemp              ) !1
            deallocate (npp_enfboreal            ) !2
            deallocate (npp_dnfboreal            ) !3
            deallocate (npp_ebftrop              ) !4
            deallocate (npp_ebftemp              ) !5
            deallocate (npp_dbftrop              ) !6
            deallocate (npp_dbftemp              ) !7
            deallocate (npp_dbfboreal            ) !8
            deallocate (npp_ebstemp              ) !9
            deallocate (npp_dbstemp              ) !10
            deallocate (npp_dbsboreal            ) !11
            deallocate (npp_c3arcgrass           ) !12
            deallocate (npp_c3grass              ) !13
            deallocate (npp_c4grass              ) !14
            deallocate (npptoleafc_enftemp              ) !1
            deallocate (npptoleafc_enfboreal            ) !2
            deallocate (npptoleafc_dnfboreal            ) !3
            deallocate (npptoleafc_ebftrop              ) !4
            deallocate (npptoleafc_ebftemp              ) !5
            deallocate (npptoleafc_dbftrop              ) !6
            deallocate (npptoleafc_dbftemp              ) !7
            deallocate (npptoleafc_dbfboreal            ) !8
            deallocate (npptoleafc_ebstemp              ) !9
            deallocate (npptoleafc_dbstemp              ) !10
            deallocate (npptoleafc_dbsboreal            ) !11
            deallocate (npptoleafc_c3arcgrass           ) !12
            deallocate (npptoleafc_c3grass              ) !13
            deallocate (npptoleafc_c4grass              ) !14
            deallocate (leafc_enftemp              ) !1
            deallocate (leafc_enfboreal            ) !2
            deallocate (leafc_dnfboreal            ) !3
            deallocate (leafc_ebftrop              ) !4
            deallocate (leafc_ebftemp              ) !5
            deallocate (leafc_dbftrop              ) !6
            deallocate (leafc_dbftemp              ) !7
            deallocate (leafc_dbfboreal            ) !8
            deallocate (leafc_ebstemp              ) !9
            deallocate (leafc_dbstemp              ) !10
            deallocate (leafc_dbsboreal            ) !11
            deallocate (leafc_c3arcgrass           ) !12
            deallocate (leafc_c3grass              ) !13
            deallocate (leafc_c4grass              ) !14
            deallocate (ar                         )
            deallocate (cwdprod                    )
            deallocate (cwddecomp                  )
            deallocate (hr                         )
            deallocate (er                         )
            deallocate (fire_closs                 )
            deallocate (fire_nloss                 )
            deallocate (hrv_xsmrpool_to_atm        )
            deallocate (wood_harvestc              )
            deallocate (wood_harvestn              )
            deallocate (grainc_to_cropprodc        )
            deallocate (grainc_to_seed             )
            deallocate (grainn_to_cropprodn        )
            deallocate (cropprod1c_loss            )


 ! decomposition carbon fluxes
            deallocate (decomp_cpools_sourcesink   )
            deallocate (decomp_ctransfer_vr        )
            deallocate (decomp_hr_vr               )
            deallocate (decomp_hr                  )
            deallocate (phr_vr                     )
            deallocate (m_decomp_cpools_to_fire_vr )
            deallocate (decomp_cpools_transport_tendency)
            deallocate (som_c_leached              )

 ! vegetation to decomposition carbon fluxes
            deallocate (phenology_to_met_c       )
            deallocate (phenology_to_cel_c       )
            deallocate (phenology_to_lig_c       )
            deallocate (gap_mortality_to_met_c   )
            deallocate (gap_mortality_to_cel_c   )
            deallocate (gap_mortality_to_lig_c   )
            deallocate (gap_mortality_to_cwdc    )
            deallocate (fire_mortality_to_met_c  )
            deallocate (fire_mortality_to_cel_c  )
            deallocate (fire_mortality_to_lig_c  )
            deallocate (fire_mortality_to_cwdc   )

 ! decomposition nitrogen fluxes
            deallocate (decomp_npools_sourcesink   )
            deallocate (decomp_ntransfer_vr        )
            deallocate (decomp_sminn_flux_vr       )
            deallocate (sminn_to_denit_decomp_vr   )
            deallocate (m_decomp_npools_to_fire_vr )
            deallocate (decomp_npools_transport_tendency)
            deallocate (som_n_leached              )

 ! vegetation to decomposition nitrogen fluxes
            deallocate (phenology_to_met_n       )
            deallocate (phenology_to_cel_n       )
            deallocate (phenology_to_lig_n       )
            deallocate (gap_mortality_to_met_n   )
            deallocate (gap_mortality_to_cel_n   )
            deallocate (gap_mortality_to_lig_n   )
            deallocate (gap_mortality_to_cwdn    )
            deallocate (fire_mortality_to_met_n  )
            deallocate (fire_mortality_to_cel_n  )
            deallocate (fire_mortality_to_lig_n  )
            deallocate (fire_mortality_to_cwdn   )

            deallocate (sminn_leached_vr         )
            deallocate (smin_no3_leached_vr      )
            deallocate (smin_no3_runoff_vr       )
            deallocate (net_nmin_vr              )
            deallocate (gross_nmin_vr            )
            deallocate (net_nmin                 )
            deallocate (gross_nmin               )
            deallocate (plant_ndemand            )
            deallocate (actual_immob_vr          )
            deallocate (actual_immob_nh4_vr      )
            deallocate (actual_immob_no3_vr      )
            deallocate (potential_immob_vr       )
            deallocate (pmnf_decomp              )
            deallocate (p_decomp_cpool_loss      )
            deallocate (sminn_to_plant           )
            deallocate (sminn_to_plant_vr        )
            deallocate (smin_nh4_to_plant_vr     )
            deallocate (smin_no3_to_plant_vr     )
            deallocate (supplement_to_sminn_vr   )
            deallocate (sminn_to_plant_fun_vr    )
            deallocate (sminn_to_plant_fun_nh4_vr)
            deallocate (sminn_to_plant_fun_no3_vr)
            deallocate (sminn_to_denit_excess_vr )
            deallocate (f_nit_vr                 )
            deallocate (f_denit_vr               )
            deallocate (f_n2o_nit_vr             )
            deallocate (f_n2o_denit_vr           )
            deallocate (pot_f_nit_vr             )
            deallocate (pot_f_denit_vr           )
            deallocate (n2_n2o_ratio_denit_vr    )
            deallocate (ndep_to_sminn            )
            deallocate (ffix_to_sminn            )
            deallocate (nfix_to_sminn            )
            deallocate (somc_fire                )
            deallocate (supplement_to_sminn      )
            deallocate (fert_to_sminn            )
            deallocate (soyfixn_to_sminn         )
            deallocate (denit                    )
            deallocate (sminn_leached            )
            deallocate (f_n2o_nit                )
            deallocate (smin_no3_leached         )
            deallocate (smin_no3_runoff          )
         ENDIF
      ENDIF

   END SUBROUTINE deallocate_1D_BGCFluxes

SUBROUTINE set_1D_BGCFluxes(Values, Nan)
  ! --------------------------------------------------------------------
  ! Allocates memory for CoLM 1d [numpatch] variables
  ! --------------------------------------------------------------------
   USE MOD_Precision
   USE MOD_Vars_Global
   USE MOD_MPAS_MPI
   USE MOD_LandPatch
   IMPLICIT NONE
   real(r8),intent(in) :: Values
   real(r8),intent(in) :: Nan


      IF (.true.) THEN

         IF (numpatch > 0) THEN

! bgc variables
! ecosystem carbon flux
            gpp                        (:)   = Values
            gpp_enftemp                (:)   = Values !1
            gpp_enfboreal              (:)   = Values !2
            gpp_dnfboreal              (:)   = Values !3
            gpp_ebftrop                (:)   = Values !4
            gpp_ebftemp                (:)   = Values !5
            gpp_dbftrop                (:)   = Values !6
            gpp_dbftemp                (:)   = Values !7
            gpp_dbfboreal              (:)   = Values !8
            gpp_ebstemp                (:)   = Values !9
            gpp_dbstemp                (:)   = Values !10
            gpp_dbsboreal              (:)   = Values !11
            gpp_c3arcgrass             (:)   = Values !12
            gpp_c3grass                (:)   = Values !13
            gpp_c4grass                (:)   = Values !14
            npp_enftemp                (:)   = Values !1
            npp_enfboreal              (:)   = Values !2
            npp_dnfboreal              (:)   = Values !3
            npp_ebftrop                (:)   = Values !4
            npp_ebftemp                (:)   = Values !5
            npp_dbftrop                (:)   = Values !6
            npp_dbftemp                (:)   = Values !7
            npp_dbfboreal              (:)   = Values !8
            npp_ebstemp                (:)   = Values !9
            npp_dbstemp                (:)   = Values !10
            npp_dbsboreal              (:)   = Values !11
            npp_c3arcgrass             (:)   = Values !12
            npp_c3grass                (:)   = Values !13
            npp_c4grass                (:)   = Values !14
            npptoleafc_enftemp         (:)   = Values !1
            npptoleafc_enfboreal       (:)   = Values !2
            npptoleafc_dnfboreal       (:)   = Values !3
            npptoleafc_ebftrop         (:)   = Values !4
            npptoleafc_ebftemp         (:)   = Values !5
            npptoleafc_dbftrop         (:)   = Values !6
            npptoleafc_dbftemp         (:)   = Values !7
            npptoleafc_dbfboreal       (:)   = Values !8
            npptoleafc_ebstemp         (:)   = Values !9
            npptoleafc_dbstemp         (:)   = Values !10
            npptoleafc_dbsboreal       (:)   = Values !11
            npptoleafc_c3arcgrass      (:)   = Values !12
            npptoleafc_c3grass         (:)   = Values !13
            npptoleafc_c4grass         (:)   = Values !14
            leafc_enftemp              (:)   = Values !1
            leafc_enfboreal            (:)   = Values !2
            leafc_dnfboreal            (:)   = Values !3
            leafc_ebftrop              (:)   = Values !4
            leafc_ebftemp              (:)   = Values !5
            leafc_dbftrop              (:)   = Values !6
            leafc_dbftemp              (:)   = Values !7
            leafc_dbfboreal            (:)   = Values !8
            leafc_ebstemp              (:)   = Values !9
            leafc_dbstemp              (:)   = Values !10
            leafc_dbsboreal            (:)   = Values !11
            leafc_c3arcgrass           (:)   = Values !12
            leafc_c3grass              (:)   = Values !13
            leafc_c4grass              (:)   = Values !14
            ar                         (:)   = Values
            cwdprod                    (:)   = Values
            cwddecomp                  (:)   = Values
            hr                         (:)   = Values
            er                         (:)   = Values
            fire_closs                 (:)   = Values
            fire_nloss                 (:)   = Values
            hrv_xsmrpool_to_atm        (:)   = Values
            wood_harvestc              (:)   = Values
            wood_harvestn              (:)   = Values
            grainc_to_cropprodc        (:)   = Values
            grainc_to_seed             (:)   = Values
            grainn_to_cropprodn        (:)   = Values
            cropprod1c_loss            (:)   = Values


! decomposition carbon fluxes
            decomp_cpools_sourcesink   (:,:,:) = Values
            decomp_ctransfer_vr        (:,:,:) = Values
            decomp_hr_vr               (:,:,:) = Values
            decomp_hr                  (:)     = Values
            phr_vr                     (:,: )  = Values
            m_decomp_cpools_to_fire_vr (:,:,:) = Values
            decomp_cpools_transport_tendency(:,:,:) = Values
            som_c_leached              (:)   = Values

! vegetation to decomposition carbon fluxes
            phenology_to_met_c       (:,:)   = Values
            phenology_to_cel_c       (:,:)   = Values
            phenology_to_lig_c       (:,:)   = Values
            gap_mortality_to_met_c   (:,:)   = Values
            gap_mortality_to_cel_c   (:,:)   = Values
            gap_mortality_to_lig_c   (:,:)   = Values
            gap_mortality_to_cwdc    (:,:)   = Values
            fire_mortality_to_met_c  (:,:)   = Values
            fire_mortality_to_cel_c  (:,:)   = Values
            fire_mortality_to_lig_c  (:,:)   = Values
            fire_mortality_to_cwdc   (:,:)   = Values

! decomposition nitrogen fluxes
            decomp_npools_sourcesink   (:,:,:) = Values
            decomp_ntransfer_vr        (:,:,:) = Values
            decomp_sminn_flux_vr       (:,:,:) = Values
            sminn_to_denit_decomp_vr   (:,:,:) = Values
            m_decomp_npools_to_fire_vr (:,:,:) = Values
            decomp_npools_transport_tendency(:,:,:) = Values
            som_n_leached            (:)     = Values

! vegetation to decomposition nitrogen fluxes
            phenology_to_met_n       (:,:)   = Values
            phenology_to_cel_n       (:,:)   = Values
            phenology_to_lig_n       (:,:)   = Values
            gap_mortality_to_met_n   (:,:)   = Values
            gap_mortality_to_cel_n   (:,:)   = Values
            gap_mortality_to_lig_n   (:,:)   = Values
            gap_mortality_to_cwdn    (:,:)   = Values
            fire_mortality_to_met_n  (:,:)   = Values
            fire_mortality_to_cel_n  (:,:)   = Values
            fire_mortality_to_lig_n  (:,:)   = Values
            fire_mortality_to_cwdn   (:,:)   = Values

            sminn_leached_vr         (:,:)   = Values
            smin_no3_leached_vr      (:,:)   = Values
            smin_no3_runoff_vr       (:,:)   = Values
            net_nmin_vr              (:,:)   = Values
            gross_nmin_vr            (:,:)   = Values
            net_nmin                 (:)     = Values
            gross_nmin               (:)     = Values
            plant_ndemand            (:)     = Values
            actual_immob_vr          (:,:)   = Values
            actual_immob_nh4_vr      (:,:)   = Values
            actual_immob_no3_vr      (:,:)   = Values
            potential_immob_vr       (:,:)   = Values
            pmnf_decomp              (:,:,:) = Values
            p_decomp_cpool_loss      (:,:,:) = Values
            sminn_to_plant           (:)     = Values
            sminn_to_plant_vr        (:,:)   = Values
            smin_nh4_to_plant_vr     (:,:)   = Values
            smin_no3_to_plant_vr     (:,:)   = Values
            supplement_to_sminn_vr   (:,:)   = Values
            sminn_to_plant_fun_vr    (:,:)   = Values
            sminn_to_plant_fun_nh4_vr(:,:)   = Values
            sminn_to_plant_fun_no3_vr(:,:)   = Values
            sminn_to_denit_excess_vr (:,:)   = Values
            f_nit_vr                 (:,:)   = Values
            f_denit_vr               (:,:)   = Values
            f_n2o_nit_vr             (:,:)   = Values
            f_n2o_denit_vr           (:,:)   = Values
            pot_f_nit_vr             (:,:)   = Values
            pot_f_denit_vr           (:,:)   = Values
            n2_n2o_ratio_denit_vr    (:,:)   = Values
            ndep_to_sminn            (:)     = Values
            ffix_to_sminn            (:)     = Values
            nfix_to_sminn            (:)     = Values
            somc_fire                (:)     = Values
            supplement_to_sminn      (:)     = Values
            fert_to_sminn            (:)     = Values
            soyfixn_to_sminn         (:)     = Values
            denit                    (:)     = Values
            sminn_leached            (:)     = Values
            f_n2o_nit                (:)     = Values
            smin_no3_leached         (:)     = Values
            smin_no3_runoff          (:)     = Values
         ENDIF
      ENDIF

   END SUBROUTINE set_1D_BGCFluxes

#endif
END MODULE MOD_BGC_Vars_1DFluxes
! ---------- EOP ------------
