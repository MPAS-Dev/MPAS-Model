#include <define.h>

MODULE MOD_BGC_Vars_TimeInvariants

! --------------------------------------------------------------------

! !DESCRIPTION
! Define, allocate, and deallocate biogeochemical constant at patch level.
! Read and write biogeochemical constant at patch level from/to restart files.

! !ORIGINAL:
! Xingjie Lu, 2022, created the original version
! -------------------------------
#ifdef BGC

   USE MOD_Precision
   IMPLICIT NONE
   SAVE
!------------------------- BGC constant --------------------------------------
   integer , allocatable  :: donor_pool       (:)      ! soil or litter pools where each decomposition transfered C&N is from.
   integer , allocatable  :: receiver_pool    (:)      ! soil or litter pools where each decomposition transfered C&N is to
   real(r8)               :: am                        ! gap-mortality rate constant (year-1)
   logical , allocatable  :: floating_cn_ratio(:)      ! flag, soil or litter pool has 1) true: flexible or 2) false: fixed C:N ratio.
   real(r8), allocatable  :: initial_cn_ratio (:)      ! initial c:n ratio of each litter and soil pool
   real(r8), allocatable  :: rf_decomp        (:,:,:)  ! respiratory fraction of the ith transfer hr(i) / (hr(i) + ctransfer(i))
   real(r8), allocatable  :: pathfrac_decomp  (:,:,:)  ! pathway fraction of each transfer from the same donor pool &
                                                       ! (hr(i)+ctransfer(i))/sum(hr(donor_pool(:)==donor_pool(i))+ctransfer(donor_pool(:)==donor_pool(i)))

   integer  :: i_met_lit    ! index of metabolic litter pool
   integer  :: i_cel_lit    ! index of cellulose litter pool
   integer  :: i_lig_lit    ! index of lignin litter pool
   integer  :: i_cwd        ! index of coarse woody debris pool
   integer  :: i_soil1      ! index of active soil organic matter pool
   integer  :: i_soil2      ! index of slow soil organic matter pool
   integer  :: i_soil3      ! index of passive soil organic matter pool
   integer  :: i_atm        ! index of atmosphere pool

   logical , allocatable :: is_cwd    (:)  ! (1:ndecomp_pools) ! True => is a coarse woody debris pool
   logical , allocatable :: is_litter (:)  ! (1:ndecomp_pools) ! True => is a litter pool
   logical , allocatable :: is_soil   (:)  ! (1:ndecomp_pools) ! True => is a soil pool

   real(r8), allocatable :: gdp_lf    (:)     ! gdp data
   real(r8), allocatable :: abm_lf    (:)     ! prescribed crop fire time
   real(r8), allocatable :: peatf_lf  (:)     ! peatland fraction data
   real(r8), allocatable :: cmb_cmplt_fact(:) ! combustion completion factor
   integer , allocatable :: rice2pdt  (:)     ! rice2 planting date

   real(r8) :: nitrif_n2o_loss_frac           ! fraction of N lost as N2O in nitrification (unitless)
   real(r8) :: dnp                            ! denitrification proportion (unitless)
   real(r8) :: bdnr                           ! bulk denitrification rate (1/day)
   real(r8) :: compet_plant_no3               ! relative compettiveness of plants for NO3 (unitless)
   real(r8) :: compet_plant_nh4               ! relative compettiveness of plants for NH4 (unitless)
   real(r8) :: compet_decomp_no3              ! relative competitiveness of immobilizers for NO3 (unitless)
   real(r8) :: compet_decomp_nh4              ! relative competitiveness of immobilizers for NH4 (unitless)
   real(r8) :: compet_denit                   ! relative competitiveness of denitrifiers for NO3 (unitless)
   real(r8) :: compet_nit                     ! relative competitiveness of nitrifiers for NH4 (unitless)
   real(r8) :: surface_tension_water          ! surface tension of water (J m-2)
   real(r8) :: rij_kro_a                      ! parameters for calculation of anoxic fraction of soil
   real(r8) :: rij_kro_alpha                  ! parameters for calculation of anoxic fraction of soil
   real(r8) :: rij_kro_beta                   ! parameters for calculation of anoxic fraction of soil
   real(r8) :: rij_kro_gamma                  ! parameters for calculation of anoxic fraction of soil
   real(r8) :: rij_kro_delta                  ! parameters for calculation of anoxic fraction of soil
   real(r8) :: nfix_timeconst                 ! timescale for smoothing npp in N fixation term
   real(r8) :: organic_max                    ! organic matter content (kg m-3) where soil is assumed to act like peat
   real(r8) :: d_con_g21                      ! O2 diffusivity constants in gas (cm2 s-1)
   real(r8) :: d_con_g22                      ! O2 diffusivity constants in gas (cm2 s-1)
   real(r8) :: d_con_w21                      ! O2 diffusivity constants in water (cm2 s-1)
   real(r8) :: d_con_w22                      ! O2 diffusivity constants in water (cm2 s-1)
   real(r8) :: d_con_w23                      ! O2 diffusivity constants in water (cm2 s-1)
   real(r8) :: denit_resp_coef                ! coefficient for maximum N denitrification rate based on respiration
   real(r8) :: denit_resp_exp                 ! exponent for maximum N denitrification rate based on respiration
   real(r8) :: denit_nitrate_coef             ! coefficient for maximum N denitrification rate based on nitrate concentration
   real(r8) :: denit_nitrate_exp              ! exponent for maximum N denitrification rate based on nitrate concentration
   real(r8) :: k_nitr_max                     ! maximum N nitrification rate (day-1)
   real(r8) :: Q10                            ! respiration rate increments when temperature rising 10 degree C
   real(r8) :: froz_q10                       ! respiration rate increments when temperature rising 10 degree C for frozen soil
   real(r8) :: tau_l1                         ! baseline turnover rate of metabolic litter from Century (year-1)
   real(r8) :: tau_l2_l3                      ! baseline turnover rate of cellulose litter and lignin litter from Century (year-1)
   real(r8) :: tau_s1                         ! baseline turnover rate of active soil organic matter from Century (year-1)
   real(r8) :: tau_s2                         ! baseline turnover rate of slow soil organic matter from Century (year-1)
   real(r8) :: tau_s3                         ! baseline turnover rate of passive soil organic matter from Century (year-1)
   real(r8) :: tau_cwd                        ! baseline turnover rate of CWD (year-1)
   real(r8) :: lwtop                          ! live wood turnover proportion

   real(r8) :: som_adv_flux                   ! the advection term in soil organic matter mixing
   real(r8) :: som_diffus                     ! the diffusion term in soil organic matter mixing
   real(r8) :: cryoturb_diffusion_k           ! the cryoturbation diffusive constant cryoturbation to the active layer thickness (m2 s-1)
   real(r8) :: max_altdepth_cryoturbation     ! maximum active layer thickness for cryoturbation to occur (m)
   real(r8) :: max_depth_cryoturb             ! the maximum depth of cryoturbation (m)

   real(r8) :: br                             ! basal maintenance respiration rate for aboveground biomass (gC gN-1 s-1)
   real(r8) :: br_root                        ! basal maintenance respiration rate for belowground biomass (gC gN-1 s-1)
   real(r8) :: fstor2tran                     ! fraction of storage to transfer pool at each onset event
   real(r8) :: ndays_on                       ! number of days to complete leaf onset
   real(r8) :: ndays_off                      ! number of days to complete leaf offset
   real(r8) :: crit_dayl                      ! critical day length for senescence (s)
   real(r8) :: crit_onset_fdd                 ! critical number of freezing days to begin gdd accumulation
   real(r8) :: crit_onset_swi                 ! critical number of days exceeding soil water potential threshold to initiate onset
   real(r8) :: crit_offset_fdd                ! critical number of freezing days to initiate offset
   real(r8) :: crit_offset_swi                ! critical number of days below soil water potential threshold to initiate offset
   real(r8) :: soilpsi_on                     ! critical soil water potential threshold for onset
   real(r8) :: soilpsi_off                    ! critical soil water potential threshold for offset

   real(r8) :: occur_hi_gdp_tree              ! fire occurance for high GDP areas that are tree dominated (fraction)
   real(r8) :: lfuel                          ! lower threshold of fuel mass (gC/m2) for ignition, Li et al.(2014)
   real(r8) :: ufuel                          ! upper threshold of fuel mass (gC/m2) for ignition, Li et al.(2014)
   real(r8) :: cropfire_a1                    ! a1 parameter for cropland fire in (Li et. al., 2014) (1/hr)
   real(r8) :: borealat                       ! Latitude bound for boreal peat fires
   real(r8) :: troplat                        ! Latitude bound for tropical
   real(r8) :: non_boreal_peatfire_c          ! c parameter for non-boreal peatland fire in Li et. al. (2013) (1/hr)
   real(r8) :: boreal_peatfire_c              ! c parameter for boreal peatland fire in Li et. al. (2013) (/hr)
   real(r8) :: rh_low                         ! parameter for lower relative humidity on fire (%)
   real(r8) :: rh_hgh                         ! parameter for higher relative humidity on fire (%)
   real(r8) :: bt_min                         ! minimum water stress factor
   real(r8) :: bt_max                         ! maximum water stress factor
   real(r8) :: pot_hmn_ign_counts_alpha       ! Potential human ignition counts (alpha in Li et. al. 2012) (1/person/month)
   real(r8) :: g0_fire                             ! constant for fire spread estimates

   real(r8) :: sf                             ! soluble fraction of mineral N (unitless)
   real(r8) :: sf_no3                         ! soluble fraction of NO3 (unitless)

!----------------------------------- END BGC constants -----------------


! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: allocate_BGCTimeInvariants
   PUBLIC :: deallocate_BGCTimeInvariants
   PUBLIC :: READ_BGCTimeInvariants
   PUBLIC :: WRITE_BGCTimeInvariants

! PRIVATE MEMBER FUNCTIONS:

!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------

   SUBROUTINE allocate_BGCTimeInvariants ()
  ! --------------------------------------------------------------------
  ! Allocates memory for CoLM 1d [numpatch] variables
  ! --------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_Vars_Global, only: nl_soil, ndecomp_transitions, ndecomp_pools, spval_i4, spval
   USE MOD_MPAS_MPI
   USE MOD_LandPatch, only: numpatch
   IMPLICIT NONE

      IF (.true.) THEN

         IF (numpatch > 0) THEN
    ! bgc variables
            allocate (donor_pool        (ndecomp_transitions))                  ; donor_pool         (:) = spval_i4
            allocate (receiver_pool     (ndecomp_transitions))                  ; receiver_pool      (:) = spval_i4
            allocate (floating_cn_ratio (ndecomp_pools))                        ; floating_cn_ratio  (:) = .false.
            allocate (initial_cn_ratio  (ndecomp_pools))                        ; initial_cn_ratio   (:) = spval
            allocate (rf_decomp         (nl_soil,ndecomp_transitions,numpatch)) ; rf_decomp      (:,:,:) = spval
            allocate (pathfrac_decomp   (nl_soil,ndecomp_transitions,numpatch)) ; pathfrac_decomp(:,:,:) = spval
            allocate (is_cwd            (ndecomp_pools))                        ; is_cwd             (:) = .false. ! True => is a coarse woody debris pool
            allocate (is_litter         (ndecomp_pools))                        ; is_litter          (:) = .false. ! True => is a litter pool
            allocate (is_soil           (ndecomp_pools))                        ; is_soil            (:) = .false. ! True => is a soil pool
            allocate (gdp_lf            (numpatch))                             ; gdp_lf             (:) = spval
            allocate (abm_lf            (numpatch))                             ; abm_lf             (:) = spval
            allocate (peatf_lf          (numpatch))                             ; peatf_lf           (:) = spval
            allocate (cmb_cmplt_fact    (2))                                    ; cmb_cmplt_fact     (:) = spval
            allocate (rice2pdt          (numpatch))                             ; rice2pdt           (:) = spval_i4

    ! END bgc variables
         ENDIF
      ENDIF

   END SUBROUTINE allocate_BGCTimeInvariants

  !---------------------------------------
   SUBROUTINE READ_BGCTimeInvariants (file_restart)

     !=======================================================================
     ! Original version: Yongjiu Dai, September 15, 1999, 03/2014
     !=======================================================================

   USE MOD_Namelist
   USE MOD_MPAS_MPI
   USE MOD_NetCDFVector
   USE MOD_NetCDFSerial
#ifdef RangeCheck
   USE MOD_RangeCheck
#endif
   USE MOD_LandPatch
   USE MOD_Vars_Global

   IMPLICIT NONE

   character(len=*), intent(in) :: file_restart

! bgc constants
      CALL ncio_read_bcast_serial (file_restart, 'donor_pool     ', donor_pool     )
      CALL ncio_read_bcast_serial (file_restart, 'receiver_pool  ', receiver_pool  )
      CALL ncio_read_bcast_serial (file_restart, 'floating_cn_ratio', floating_cn_ratio)
      CALL ncio_read_bcast_serial (file_restart, 'initial_cn_ratio' , initial_cn_ratio)
      CALL ncio_read_vector       (file_restart, 'rf_decomp       ', nl_soil,ndecomp_transitions,landpatch, rf_decomp      )
      CALL ncio_read_vector       (file_restart, 'pathfrac_decomp ', nl_soil,ndecomp_transitions,landpatch,pathfrac_decomp )

      CALL ncio_read_bcast_serial (file_restart, 'i_met_lit      ', i_met_lit      )
      CALL ncio_read_bcast_serial (file_restart, 'i_cel_lit      ', i_cel_lit      )
      CALL ncio_read_bcast_serial (file_restart, 'i_lig_lit      ', i_lig_lit      )
      CALL ncio_read_bcast_serial (file_restart, 'i_cwd          ', i_cwd          )
      CALL ncio_read_bcast_serial (file_restart, 'i_soil1        ', i_soil1        )
      CALL ncio_read_bcast_serial (file_restart, 'i_soil2        ', i_soil2        )
      CALL ncio_read_bcast_serial (file_restart, 'i_soil3        ', i_soil3        )
      CALL ncio_read_bcast_serial (file_restart, 'i_atm          ', i_atm          )
      CALL ncio_read_bcast_serial (file_restart, 'is_cwd         ', is_cwd         )
      CALL ncio_read_bcast_serial (file_restart, 'is_litter      ', is_litter      )
      CALL ncio_read_bcast_serial (file_restart, 'is_soil        ', is_soil        )

 !     CALL ncio_read_vector       (file_restart, 'gdp_lf         ', landpatch, gdp_lf        )
 !     CALL ncio_read_vector       (file_restart, 'abm_lf         ', landpatch, abm_lf        )
 !     CALL ncio_read_vector       (file_restart, 'peatf_lf       ', landpatch, peatf_lf      )
      CALL ncio_read_bcast_serial (file_restart, 'cmb_cmplt_fact ', cmb_cmplt_fact )
      CALL ncio_read_vector       (file_restart, 'rice2pdt       ', landpatch, rice2pdt      )

      CALL ncio_read_bcast_serial (file_restart, 'nitrif_n2o_loss_frac', nitrif_n2o_loss_frac)
      CALL ncio_read_bcast_serial (file_restart, 'dnp                 ', dnp                 )!
      CALL ncio_read_bcast_serial (file_restart, 'bdnr                ', bdnr                )!
      CALL ncio_read_bcast_serial (file_restart, 'compet_plant_no3    ', compet_plant_no3    )!
      CALL ncio_read_bcast_serial (file_restart, 'compet_plant_nh4    ', compet_plant_nh4    )!
      CALL ncio_read_bcast_serial (file_restart, 'compet_decomp_no3   ', compet_decomp_no3   )!
      CALL ncio_read_bcast_serial (file_restart, 'compet_decomp_nh4   ', compet_decomp_nh4   )!
      CALL ncio_read_bcast_serial (file_restart, 'compet_denit        ', compet_denit        )!
      CALL ncio_read_bcast_serial (file_restart, 'compet_nit          ', compet_nit          )!
      CALL ncio_read_bcast_serial (file_restart, 'surface_tension_water',surface_tension_water)
      CALL ncio_read_bcast_serial (file_restart, 'rij_kro_a           ', rij_kro_a           )
      CALL ncio_read_bcast_serial (file_restart, 'rij_kro_alpha       ', rij_kro_alpha       )
      CALL ncio_read_bcast_serial (file_restart, 'rij_kro_beta        ', rij_kro_beta        )
      CALL ncio_read_bcast_serial (file_restart, 'rij_kro_gamma       ', rij_kro_gamma       )
      CALL ncio_read_bcast_serial (file_restart, 'rij_kro_delta       ', rij_kro_delta       )
      CALL ncio_read_bcast_serial (file_restart, 'nfix_timeconst      ', nfix_timeconst      )
      CALL ncio_read_bcast_serial (file_restart, 'organic_max         ', organic_max         )
      CALL ncio_read_bcast_serial (file_restart, 'd_con_g21           ', d_con_g21           )
      CALL ncio_read_bcast_serial (file_restart, 'd_con_g22           ', d_con_g22           )
      CALL ncio_read_bcast_serial (file_restart, 'd_con_w21           ', d_con_w21           )
      CALL ncio_read_bcast_serial (file_restart, 'd_con_w22           ', d_con_w22           )
      CALL ncio_read_bcast_serial (file_restart, 'd_con_w23           ', d_con_w23           )
      CALL ncio_read_bcast_serial (file_restart, 'denit_resp_coef     ', denit_resp_coef     )
      CALL ncio_read_bcast_serial (file_restart, 'denit_resp_exp      ', denit_resp_exp      )
      CALL ncio_read_bcast_serial (file_restart, 'denit_nitrate_coef  ', denit_nitrate_coef  )
      CALL ncio_read_bcast_serial (file_restart, 'denit_nitrate_exp   ', denit_nitrate_exp   )!
      CALL ncio_read_bcast_serial (file_restart, 'k_nitr_max          ', k_nitr_max          )!
      CALL ncio_read_bcast_serial (file_restart, 'Q10                 ', Q10                 )!
      CALL ncio_read_bcast_serial (file_restart, 'froz_q10            ', froz_q10            )!
      CALL ncio_read_bcast_serial (file_restart, 'tau_l1              ', tau_l1              )!
      CALL ncio_read_bcast_serial (file_restart, 'tau_l2_l3           ', tau_l2_l3           )!
      CALL ncio_read_bcast_serial (file_restart, 'tau_s1              ', tau_s1              )!
      CALL ncio_read_bcast_serial (file_restart, 'tau_s2              ', tau_s2              )!
      CALL ncio_read_bcast_serial (file_restart, 'tau_s3              ', tau_s3              )!
      CALL ncio_read_bcast_serial (file_restart, 'tau_cwd             ', tau_cwd             )
      CALL ncio_read_bcast_serial (file_restart, 'lwtop               ', lwtop               )

      CALL ncio_read_bcast_serial (file_restart, 'som_adv_flux        ', som_adv_flux        )
      CALL ncio_read_bcast_serial (file_restart, 'som_diffus          ', som_diffus          )
      CALL ncio_read_bcast_serial (file_restart, 'cryoturb_diffusion_k', cryoturb_diffusion_k)
      CALL ncio_read_bcast_serial (file_restart, 'max_altdepth_cryoturbation', max_altdepth_cryoturbation)
      CALL ncio_read_bcast_serial (file_restart, 'max_depth_cryoturb  ', max_depth_cryoturb  )

      CALL ncio_read_bcast_serial (file_restart, 'am                  ', am                  )
      CALL ncio_read_bcast_serial (file_restart, 'br                  ', br                  )
      CALL ncio_read_bcast_serial (file_restart, 'br_root             ', br_root             )
      CALL ncio_read_bcast_serial (file_restart, 'fstor2tran          ', fstor2tran          )
      CALL ncio_read_bcast_serial (file_restart, 'ndays_on            ', ndays_on            )
      CALL ncio_read_bcast_serial (file_restart, 'ndays_off           ', ndays_off           )
      CALL ncio_read_bcast_serial (file_restart, 'crit_dayl           ', crit_dayl           )
      CALL ncio_read_bcast_serial (file_restart, 'crit_onset_fdd      ', crit_onset_fdd      )
      CALL ncio_read_bcast_serial (file_restart, 'crit_onset_swi      ', crit_onset_swi      )
      CALL ncio_read_bcast_serial (file_restart, 'crit_offset_fdd     ', crit_offset_fdd     )
      CALL ncio_read_bcast_serial (file_restart, 'crit_offset_swi     ', crit_offset_swi     )
      CALL ncio_read_bcast_serial (file_restart, 'soilpsi_on          ', soilpsi_on          )
      CALL ncio_read_bcast_serial (file_restart, 'soilpsi_off         ', soilpsi_off         )

      CALL ncio_read_bcast_serial (file_restart, 'occur_hi_gdp_tree   ', occur_hi_gdp_tree   )
      CALL ncio_read_bcast_serial (file_restart, 'lfuel               ', lfuel               )
      CALL ncio_read_bcast_serial (file_restart, 'ufuel               ', ufuel               )
      CALL ncio_read_bcast_serial (file_restart, 'cropfire_a1         ', cropfire_a1         )
      CALL ncio_read_bcast_serial (file_restart, 'borealat            ', borealat            )
      CALL ncio_read_bcast_serial (file_restart, 'troplat             ', troplat             )
      CALL ncio_read_bcast_serial (file_restart, 'non_boreal_peatfire_c', non_boreal_peatfire_c)
      CALL ncio_read_bcast_serial (file_restart, 'boreal_peatfire_c   ', boreal_peatfire_c   )
      CALL ncio_read_bcast_serial (file_restart, 'rh_low              ', rh_low              )
      CALL ncio_read_bcast_serial (file_restart, 'rh_hgh              ', rh_hgh              )
      CALL ncio_read_bcast_serial (file_restart, 'bt_min              ', bt_min              )
      CALL ncio_read_bcast_serial (file_restart, 'bt_max              ', bt_max              )
      CALL ncio_read_bcast_serial (file_restart, 'pot_hmn_ign_counts_alpha', pot_hmn_ign_counts_alpha)
      CALL ncio_read_bcast_serial (file_restart, 'g0_fire', g0_fire)

      CALL ncio_read_bcast_serial (file_restart, 'sf', sf)
      CALL ncio_read_bcast_serial (file_restart, 'sf_no3', sf_no3)

#ifdef RangeCheck
      CALL check_BGCTimeInvariants ()
#endif

#ifdef MPAS_MPI
      CALL mpi_barrier (mpas_comm, mpas_mpi_ierr)
#endif

   END SUBROUTINE READ_BGCTimeInvariants

  !---------------------------------------
   SUBROUTINE WRITE_BGCTimeInvariants (file_restart)

     !=======================================================================
     ! Original version: Yongjiu Dai, September 15, 1999, 03/2014
     !=======================================================================

   USE MOD_Namelist, only: DEF_REST_CompressLevel
   USE MOD_MPAS_MPI
   USE MOD_NetCDFSerial
   USE MOD_NetCDFVector
   USE MOD_LandPatch
   USE MOD_Vars_Global

   IMPLICIT NONE

   character(len=*), intent(in) :: file_restart

   ! Local Variables
   integer :: compress

      compress = DEF_REST_CompressLevel

      CALL ncio_create_file_vector (file_restart, landpatch)

      CALL ncio_define_dimension_vector (file_restart, landpatch, 'patch')
      CALL ncio_define_dimension_vector (file_restart, landpatch, 'soil', nl_soil)
      CALL ncio_define_dimension_vector (file_restart, landpatch, 'ndecomp_transitions',ndecomp_transitions)

      CALL ncio_write_vector       (file_restart, 'rf_decomp      ', 'soil'   , nl_soil  , &
                           'ndecomp_transitions', ndecomp_transitions,'patch', landpatch, rf_decomp      , compress)
      CALL ncio_write_vector       (file_restart, 'pathfrac_decomp', 'soil'   , nl_soil  , &
                           'ndecomp_transitions', ndecomp_transitions,'patch', landpatch, pathfrac_decomp, compress)
 !     CALL ncio_write_vector       (file_restart, 'gdp_lf         ',  'patch', landpatch, gdp_lf         , compress)
 !     CALL ncio_write_vector       (file_restart, 'abm_lf         ',  'patch', landpatch, abm_lf         , compress)
 !     CALL ncio_write_vector       (file_restart, 'peatf_lf       ',  'patch', landpatch, peatf_lf       , compress)
      CALL ncio_write_vector       (file_restart, 'rice2pdt       ',  'patch', landpatch, rice2pdt       , compress)

#ifdef MPAS_MPI
      CALL mpi_barrier (mpas_comm, mpas_mpi_ierr)
#endif

      IF (mpas_is_root) THEN

#if (!defined(VectorInOneFileS) && !defined(VectorInOneFileP))
         CALL ncio_create_file (file_restart)
#endif
         CALL ncio_define_dimension(file_restart, 'ndecomp_transitions',ndecomp_transitions)
         CALL ncio_define_dimension(file_restart, 'ndecomp_pools'      ,ndecomp_pools)
         CALL ncio_define_dimension(file_restart, 'nlitter_fire'       ,2            )

 ! bgc constants
         CALL ncio_write_serial (file_restart, 'donor_pool     '  , donor_pool       , 'ndecomp_transitions')
         CALL ncio_write_serial (file_restart, 'receiver_pool  '  , receiver_pool    , 'ndecomp_transitions')
         CALL ncio_write_serial (file_restart, 'floating_cn_ratio', floating_cn_ratio, 'ndecomp_pools')
         CALL ncio_write_serial (file_restart, 'initial_cn_ratio' , initial_cn_ratio , 'ndecomp_pools')
         CALL ncio_write_serial (file_restart, 'is_cwd         '  , is_cwd           , 'ndecomp_pools')
         CALL ncio_write_serial (file_restart, 'is_litter      '  , is_litter        , 'ndecomp_pools')
         CALL ncio_write_serial (file_restart, 'is_soil        '  , is_soil          , 'ndecomp_pools')
         CALL ncio_write_serial (file_restart, 'cmb_cmplt_fact '  , cmb_cmplt_fact   , 'nlitter_fire' )

         CALL ncio_write_serial (file_restart, 'i_met_lit      ', i_met_lit      )
         CALL ncio_write_serial (file_restart, 'i_cel_lit      ', i_cel_lit      )
         CALL ncio_write_serial (file_restart, 'i_lig_lit      ', i_lig_lit      )
         CALL ncio_write_serial (file_restart, 'i_cwd          ', i_cwd          )
         CALL ncio_write_serial (file_restart, 'i_soil1        ', i_soil1        )
         CALL ncio_write_serial (file_restart, 'i_soil2        ', i_soil2        )
         CALL ncio_write_serial (file_restart, 'i_soil3        ', i_soil3        )
         CALL ncio_write_serial (file_restart, 'i_atm          ', i_atm          )


         CALL ncio_write_serial (file_restart, 'nitrif_n2o_loss_frac', nitrif_n2o_loss_frac)
         CALL ncio_write_serial (file_restart, 'dnp                 ', dnp                 )!
         CALL ncio_write_serial (file_restart, 'bdnr                ', bdnr                )!
         CALL ncio_write_serial (file_restart, 'compet_plant_no3    ', compet_plant_no3    )!
         CALL ncio_write_serial (file_restart, 'compet_plant_nh4    ', compet_plant_nh4    )!
         CALL ncio_write_serial (file_restart, 'compet_decomp_no3   ', compet_decomp_no3   )!
         CALL ncio_write_serial (file_restart, 'compet_decomp_nh4   ', compet_decomp_nh4   )!
         CALL ncio_write_serial (file_restart, 'compet_denit        ', compet_denit        )!
         CALL ncio_write_serial (file_restart, 'compet_nit          ', compet_nit          )!
         CALL ncio_write_serial (file_restart, 'surface_tension_water',surface_tension_water)
         CALL ncio_write_serial (file_restart, 'rij_kro_a           ', rij_kro_a           )
         CALL ncio_write_serial (file_restart, 'rij_kro_alpha       ', rij_kro_alpha       )
         CALL ncio_write_serial (file_restart, 'rij_kro_beta        ', rij_kro_beta        )
         CALL ncio_write_serial (file_restart, 'rij_kro_gamma       ', rij_kro_gamma       )
         CALL ncio_write_serial (file_restart, 'rij_kro_delta       ', rij_kro_delta       )
         CALL ncio_write_serial (file_restart, 'nfix_timeconst      ', nfix_timeconst      )
         CALL ncio_write_serial (file_restart, 'organic_max         ', organic_max         )
         CALL ncio_write_serial (file_restart, 'd_con_g21           ', d_con_g21           )
         CALL ncio_write_serial (file_restart, 'd_con_g22           ', d_con_g22           )
         CALL ncio_write_serial (file_restart, 'd_con_w21           ', d_con_w21           )
         CALL ncio_write_serial (file_restart, 'd_con_w22           ', d_con_w22           )
         CALL ncio_write_serial (file_restart, 'd_con_w23           ', d_con_w23           )
         CALL ncio_write_serial (file_restart, 'denit_resp_coef     ', denit_resp_coef     )
         CALL ncio_write_serial (file_restart, 'denit_resp_exp      ', denit_resp_exp      )
         CALL ncio_write_serial (file_restart, 'denit_nitrate_coef  ', denit_nitrate_coef  )
         CALL ncio_write_serial (file_restart, 'denit_nitrate_exp   ', denit_nitrate_exp   )!
         CALL ncio_write_serial (file_restart, 'k_nitr_max          ', k_nitr_max          )!
         CALL ncio_write_serial (file_restart, 'Q10                 ', Q10                 )!
         CALL ncio_write_serial (file_restart, 'froz_q10            ', froz_q10            )!
         CALL ncio_write_serial (file_restart, 'tau_l1              ', tau_l1              )!
         CALL ncio_write_serial (file_restart, 'tau_l2_l3           ', tau_l2_l3           )!
         CALL ncio_write_serial (file_restart, 'tau_s1              ', tau_s1              )!
         CALL ncio_write_serial (file_restart, 'tau_s2              ', tau_s2              )!
         CALL ncio_write_serial (file_restart, 'tau_s3              ', tau_s3              )!
         CALL ncio_write_serial (file_restart, 'tau_cwd             ', tau_cwd             )
         CALL ncio_write_serial (file_restart, 'lwtop               ', lwtop               )

         CALL ncio_write_serial (file_restart, 'som_adv_flux        ', som_adv_flux        )
         CALL ncio_write_serial (file_restart, 'som_diffus          ', som_diffus          )
         CALL ncio_write_serial (file_restart, 'cryoturb_diffusion_k', cryoturb_diffusion_k)
         CALL ncio_write_serial (file_restart, 'max_altdepth_cryoturbation', max_altdepth_cryoturbation)
         CALL ncio_write_serial (file_restart, 'max_depth_cryoturb  ', max_depth_cryoturb  )

         CALL ncio_write_serial (file_restart, 'am                  ', am                  )
         CALL ncio_write_serial (file_restart, 'br                  ', br                  )
         CALL ncio_write_serial (file_restart, 'br_root             ', br_root             )
         CALL ncio_write_serial (file_restart, 'fstor2tran          ', fstor2tran          )
         CALL ncio_write_serial (file_restart, 'ndays_on            ', ndays_on            )
         CALL ncio_write_serial (file_restart, 'ndays_off           ', ndays_off           )
         CALL ncio_write_serial (file_restart, 'crit_dayl           ', crit_dayl           )
         CALL ncio_write_serial (file_restart, 'crit_onset_fdd      ', crit_onset_fdd      )
         CALL ncio_write_serial (file_restart, 'crit_onset_swi      ', crit_onset_swi      )
         CALL ncio_write_serial (file_restart, 'crit_offset_fdd     ', crit_offset_fdd     )
         CALL ncio_write_serial (file_restart, 'crit_offset_swi     ', crit_offset_swi     )
         CALL ncio_write_serial (file_restart, 'soilpsi_on          ', soilpsi_on          )
         CALL ncio_write_serial (file_restart, 'soilpsi_off         ', soilpsi_off         )

         CALL ncio_write_serial (file_restart, 'occur_hi_gdp_tree   ', occur_hi_gdp_tree   )
         CALL ncio_write_serial (file_restart, 'lfuel               ', lfuel               )
         CALL ncio_write_serial (file_restart, 'ufuel               ', ufuel               )
         CALL ncio_write_serial (file_restart, 'cropfire_a1         ', cropfire_a1         )
         CALL ncio_write_serial (file_restart, 'borealat            ', borealat            )
         CALL ncio_write_serial (file_restart, 'troplat             ', troplat             )
         CALL ncio_write_serial (file_restart, 'non_boreal_peatfire_c', non_boreal_peatfire_c)
         CALL ncio_write_serial (file_restart, 'boreal_peatfire_c   ', boreal_peatfire_c   )
         CALL ncio_write_serial (file_restart, 'rh_low              ', rh_low              )
         CALL ncio_write_serial (file_restart, 'rh_hgh              ', rh_hgh              )
         CALL ncio_write_serial (file_restart, 'bt_min              ', bt_min              )
         CALL ncio_write_serial (file_restart, 'bt_max              ', bt_max              )
         CALL ncio_write_serial (file_restart, 'pot_hmn_ign_counts_alpha', pot_hmn_ign_counts_alpha)
         CALL ncio_write_serial (file_restart, 'g0_fire', g0_fire)

         CALL ncio_write_serial (file_restart, 'sf', sf)
         CALL ncio_write_serial (file_restart, 'sf_no3', sf_no3)

      ENDIF

   END SUBROUTINE WRITE_BGCTimeInvariants

   SUBROUTINE deallocate_BGCTimeInvariants ()

   USE MOD_MPAS_MPI
   USE MOD_LandPatch, only: numpatch
   IMPLICIT NONE

     ! --------------------------------------------------
     ! Deallocates memory for CoLM 1d [numpatch] variables
     ! --------------------------------------------------

      IF (.true.) THEN

         IF (numpatch > 0) THEN

 ! bgc variables
            deallocate (donor_pool       )
            deallocate (receiver_pool    )
            deallocate (floating_cn_ratio)
            deallocate (initial_cn_ratio )
            deallocate (rf_decomp      )
            deallocate (pathfrac_decomp)
            deallocate (is_cwd         )
            deallocate (is_litter      )
            deallocate (is_soil        )
            deallocate (gdp_lf         )
            deallocate (abm_lf         )
            deallocate (peatf_lf       )
            deallocate (cmb_cmplt_fact )
            deallocate (rice2pdt       )

         ENDIF
      ENDIF

   END SUBROUTINE deallocate_BGCTimeInvariants

#ifdef RangeCheck
   !---------------------------------------
   SUBROUTINE check_BGCTimeInvariants ()

   USE MOD_MPAS_MPI
   USE MOD_RangeCheck

   IMPLICIT NONE

      CALL check_vector_data ('rf_decomp      ',  rf_decomp      )
      CALL check_vector_data ('pathfrac_decomp',  pathfrac_decomp)
      CALL check_vector_data ('gdp_lf         ',  gdp_lf         )
      CALL check_vector_data ('abm_lf         ',  abm_lf         )
      CALL check_vector_data ('peatf_lf       ',  peatf_lf       )
      CALL check_vector_data ('rice2pdt       ',  rice2pdt       )

   END SUBROUTINE check_BGCTimeInvariants
#endif

#endif
END MODULE MOD_BGC_Vars_TimeInvariants
