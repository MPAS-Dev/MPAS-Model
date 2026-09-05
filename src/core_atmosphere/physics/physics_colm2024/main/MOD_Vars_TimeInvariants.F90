#include <define.h>

!-----------------------------------------------------------------------
! Created by Yongjiu Dai, 03/2014
!-----------------------------------------------------------------------

#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
MODULE MOD_Vars_PFTimeInvariants
!-----------------------------------------------------------------------
! !DESCRIPTION:
!  Define PFT time invariables
!
!  Added by Hua Yuan, 08/2019
!-----------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_Vars_Global
   IMPLICIT NONE
   SAVE

   ! for LULC_IGBP_PFT and LULC_IGBP_PC
   integer , allocatable :: pftclass    (:) !PFT type
   real(r8), allocatable :: pftfrac     (:) !PFT fractional cover
   real(r8), allocatable :: htop_p      (:) !canopy top height [m]
   real(r8), allocatable :: hbot_p      (:) !canopy bottom height [m]
#ifdef CROP
   real(r8), allocatable :: cropfrac    (:) !Crop fractional cover
#endif

! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: allocate_PFTimeInvariants
   PUBLIC :: READ_PFTimeInvariants
   PUBLIC :: WRITE_PFTimeInvariants
   PUBLIC :: deallocate_PFTimeInvariants
#ifdef RangeCheck
   PUBLIC :: check_PFTimeInvariants
#endif

! PRIVATE MEMBER FUNCTIONS:

!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------

   SUBROUTINE allocate_PFTimeInvariants
   ! -------------------------------------------------------------------
   ! Allocates memory for CoLM PFT 1d [numpft] variables
   ! -------------------------------------------------------------------

   USE MOD_MPAS_MPI
   USE MOD_LandPatch, only: numpatch
   USE MOD_LandPFT,   only: numpft
   USE MOD_Precision
   IMPLICIT NONE

      IF (.true.) THEN
         IF (numpft > 0) THEN
            allocate (pftclass      (numpft))
            allocate (pftfrac       (numpft))
            allocate (htop_p        (numpft))
            allocate (hbot_p        (numpft))
         ENDIF

#ifdef CROP
         IF (numpatch > 0) THEN
            allocate (cropfrac (numpatch))
         ENDIF
#endif
      ENDIF

   END SUBROUTINE allocate_PFTimeInvariants

   SUBROUTINE READ_PFTimeInvariants (file_restart)

   USE MOD_NetCDFVector
   USE MOD_LandPatch
   USE MOD_LandPFT
   IMPLICIT NONE

   character(len=*), intent(in) :: file_restart

      IF (numpft > 0) THEN
         CALL ncio_read_vector (file_restart, 'pftclass', landpft, pftclass) !
         CALL ncio_read_vector (file_restart, 'pftfrac ', landpft, pftfrac ) !
         CALL ncio_read_vector (file_restart, 'htop_p  ', landpft, htop_p  ) !
         CALL ncio_read_vector (file_restart, 'hbot_p  ', landpft, hbot_p  ) !
      ENDIF
#ifdef CROP
      IF (numpatch > 0) CALL ncio_read_vector (file_restart, 'cropfrac ', landpatch, cropfrac) !
#endif

   END SUBROUTINE READ_PFTimeInvariants

   SUBROUTINE WRITE_PFTimeInvariants (file_restart)

   USE MOD_NetCDFVector
   USE MOD_LandPFT
   USE MOD_LandPatch
   USE MOD_Namelist
   USE MOD_Vars_Global
   IMPLICIT NONE

   ! Local variables
   character(len=*), intent(in) :: file_restart
   integer :: compress

      compress = DEF_REST_CompressLevel

      IF (numpft > 0) THEN
         CALL ncio_create_file_vector (file_restart, landpft)
         CALL ncio_define_dimension_vector (file_restart, landpft, 'pft')

         CALL ncio_write_vector (file_restart, 'pftclass', 'pft', landpft, pftclass, compress) !
         CALL ncio_write_vector (file_restart, 'pftfrac ', 'pft', landpft, pftfrac , compress) !
         CALL ncio_write_vector (file_restart, 'htop_p  ', 'pft', landpft, htop_p  , compress) !
         CALL ncio_write_vector (file_restart, 'hbot_p  ', 'pft', landpft, hbot_p  , compress) !
      ENDIF

#ifdef CROP
      IF (numpatch > 0) THEN
         IF (numpft <= 0) CALL ncio_create_file_vector (file_restart, landpatch)
         CALL ncio_define_dimension_vector (file_restart, landpatch, 'patch')
         CALL ncio_write_vector (file_restart, 'cropfrac', 'patch', landpatch, cropfrac, compress) !
      ENDIF
#endif

   END SUBROUTINE WRITE_PFTimeInvariants

   SUBROUTINE deallocate_PFTimeInvariants
   ! -------------------------------------------------------------------
   ! Deallocates memory for CoLM PFT 1d [numpft] variables
   ! -------------------------------------------------------------------
   USE MOD_MPAS_MPI
   USE MOD_LandPFT

      IF (.true.) THEN
         IF (numpft > 0) THEN
            deallocate (pftclass)
            deallocate (pftfrac )
            deallocate (htop_p  )
            deallocate (hbot_p  )
         ENDIF
#ifdef CROP
         IF (numpatch > 0) deallocate (cropfrac)
#endif
      ENDIF

   END SUBROUTINE deallocate_PFTimeInvariants

#ifdef RangeCheck
   SUBROUTINE check_PFTimeInvariants ()

   USE MOD_RangeCheck
   IMPLICIT NONE

      CALL check_vector_data ('pftfrac', pftfrac) !
      CALL check_vector_data ('htop_p ', htop_p ) !
      CALL check_vector_data ('hbot_p ', hbot_p ) !
#ifdef CROP
      CALL check_vector_data ('cropfrac', cropfrac) !
#endif

   END SUBROUTINE check_PFTimeInvariants
#endif

END MODULE MOD_Vars_PFTimeInvariants
#endif

MODULE MOD_Vars_TimeInvariants
! -------------------------------
! Created by Yongjiu Dai, 03/2014
! -------------------------------

   USE MOD_Precision
#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
   USE MOD_Vars_PFTimeInvariants
#endif
#ifdef BGC
   USE MOD_BGC_Vars_TimeInvariants
#endif
#ifdef URBAN_MODEL
   USE MOD_Urban_Vars_TimeInvariants
#endif
   IMPLICIT NONE
   SAVE

! -----------------------------------------------------------------
! surface classification and soil information
   integer,  allocatable :: patchclass     (:)  !index of land cover type of the patches at the fraction > 0
   integer,  allocatable :: patchtype      (:)  !land patch type
   logical,  allocatable :: patchmask      (:)  !patch mask

   real(r8), allocatable :: patchlatr      (:)  !latitude in radians
   real(r8), allocatable :: patchlonr      (:)  !longitude in radians

   real(r8), allocatable :: lakedepth      (:)  !lake depth
   real(r8), allocatable :: dz_lake      (:,:)  !new lake scheme

   real(r8), allocatable :: soil_s_v_alb   (:)  !albedo of visible of the saturated soil
   real(r8), allocatable :: soil_d_v_alb   (:)  !albedo of visible of the dry soil
   real(r8), allocatable :: soil_s_n_alb   (:)  !albedo of near infrared of the saturated soil
   real(r8), allocatable :: soil_d_n_alb   (:)  !albedo of near infrared of the dry soil
#ifdef HYPERSPECTRAL
   real(r8), allocatable :: soil_alb     (:,:) ! hyper spectral soil albedo. (numpatch, nwl)
#endif
   real(r8), allocatable :: vf_quartz    (:,:)  !volumetric fraction of quartz within mineral soil
   real(r8), allocatable :: vf_gravels   (:,:)  !volumetric fraction of gravels
   real(r8), allocatable :: vf_om        (:,:)  !volumetric fraction of organic matter
   real(r8), allocatable :: vf_sand      (:,:)  !volumetric fraction of sand
   real(r8), allocatable :: vf_clay      (:,:)  !volumetric fraction of clay
   real(r8), allocatable :: wf_gravels   (:,:)  !gravimetric fraction of gravels
   real(r8), allocatable :: wf_sand      (:,:)  !gravimetric fraction of sand
   real(r8), allocatable :: wf_clay      (:,:)  !gravimetric fraction of clay
   real(r8), allocatable :: wf_om        (:,:)  !gravimetric fraction of om
#ifdef DataAssimilation
   real(r8), allocatable :: wf_silt      (:,:)  !gravimetric fraction of silt
#endif
   real(r8), allocatable :: OM_density   (:,:)  !OM density (kg/m3)
   real(r8), allocatable :: BD_all       (:,:)  !bulk density of soil (GRAVELS + ORGANIC MATTER + Mineral Soils,kg/m3)

   real(r8), allocatable :: wfc          (:,:)  !field capacity
   real(r8), allocatable :: porsl        (:,:)  !fraction of soil that is voids [-]
   real(r8), allocatable :: psi0         (:,:)  !minimum soil suction [mm] (NOTE: "-" valued)
   real(r8), allocatable :: bsw          (:,:)  !clapp and hornberger "b" parameter [-]
   real(r8), allocatable :: theta_r      (:,:)  !residual moisture content [-]
   real(r8), allocatable :: BVIC         (:)    !b parameter in Fraction of saturated soil in a grid calculated by VIC
#ifdef vanGenuchten_Mualem_SOIL_MODEL
   real(r8), allocatable :: alpha_vgm    (:,:)  !a parameter corresponding approximately to the inverse of the air-entry value
   real(r8), allocatable :: L_vgm        (:,:)  !pore-connectivity parameter [dimensionless]
   real(r8), allocatable :: n_vgm        (:,:)  !a shape parameter [dimensionless]
   real(r8), allocatable :: sc_vgm       (:,:)  !saturation at the air entry value in the classical vanGenuchten model [-]
   real(r8), allocatable :: fc_vgm       (:,:)  !a scaling factor by using air entry value in the Mualem model [-]
#endif

   integer,  allocatable :: soiltext       (:)  !USDA soil texture class

   real(r8), allocatable :: fsatmax        (:)  !maximum saturated area fraction [-]
   real(r8), allocatable :: fsatdcf        (:)  !decay factor in calculation of saturated area fraction [1/m]

   real(r8), allocatable :: topoweti       (:)  !topographic wetness index [log m]
   real(r8), allocatable :: alp_twi        (:)  !alpha in three parameter gamma distribution of twi
   real(r8), allocatable :: chi_twi        (:)  !chi   in three parameter gamma distribution of twi
   real(r8), allocatable :: mu_twi         (:)  !mu    in three parameter gamma distribution of twi

   real(r8), allocatable :: vic_b_infilt   (:)
   real(r8), allocatable :: vic_Dsmax      (:)
   real(r8), allocatable :: vic_Ds         (:)
   real(r8), allocatable :: vic_Ws         (:)
   real(r8), allocatable :: vic_c          (:)

   real(r8), allocatable :: hksati       (:,:)  !hydraulic conductivity at saturation [mm h2o/s]
   real(r8), allocatable :: csol         (:,:)  !heat capacity of soil solids [J/(m3 K)]
   real(r8), allocatable :: k_solids     (:,:)  !thermal conductivity of soil solids [W/m-K]
   real(r8), allocatable :: dksatu       (:,:)  !thermal conductivity of saturated soil [W/m-K]
   real(r8), allocatable :: dksatf       (:,:)  !thermal conductivity of saturated frozen soil [W/m-K]
   real(r8), allocatable :: dkdry        (:,:)  !thermal conductivity for dry soil  [W/(m-K)]
   real(r8), allocatable :: BA_alpha     (:,:)  !alpha in Balland and Arp(2005) thermal conductivity scheme
   real(r8), allocatable :: BA_beta      (:,:)  !beta in Balland and Arp(2005) thermal conductivity scheme
   real(r8), allocatable :: htop           (:)  !canopy top height [m]
   real(r8), allocatable :: hbot           (:)  !canopy bottom height [m]

   real(r8), allocatable :: dbedrock       (:)  !depth to bedrock
   integer , allocatable :: ibedrock       (:)  !bedrock level

   real(r8), allocatable :: elvmean        (:)  !elevation above sea level [m]
   real(r8), allocatable :: elvstd         (:)  !standard deviation of elevation [m]
   real(r8), allocatable :: slpratio       (:)  !slope ratio [-]

   real(r8) :: zlnd                             !roughness length for soil [m]
   real(r8) :: zsno                             !roughness length for snow [m]
   real(r8) :: csoilc                           !drag coefficient for soil under canopy [-]
   real(r8) :: dewmx                            !maximum dew
   ! 'wtfact' is updated to gridded 'fsatmax' data.
   ! real(r8) :: wtfact                         !fraction of model area with high water table
   real(r8) :: capr                             !tuning factor to turn first layer T into surface T
   real(r8) :: cnfac                            !Crank Nicholson factor between 0 and 1
   real(r8) :: ssi                              !irreducible water saturation of snow
   real(r8) :: wimp                             !water impermeable IF porosity less than wimp
   real(r8) :: pondmx                           !ponding depth (mm)
   real(r8) :: smpmax                           !wilting point potential in mm
   real(r8) :: smpmin                           !restriction for min of soil poten. (mm)
   real(r8) :: smpmax_hr                        !wilting point potential in mm for heterotrophic respiration
   real(r8) :: smpmin_hr                        !restriction for min of soil poten for heterotrophic respiration. (mm)
   real(r8) :: trsmx0                           !max transpiration for moist soil+100% veg.  [mm/s]
   real(r8) :: tcrit                            !critical temp. to determine rain or snow
   real(r8) :: wetwatmax                        !maximum wetland water (mm)

   ! Used for downscaling
   real(r8), allocatable    :: svf_patches (:)         !sky view factor
   real(r8), allocatable    :: cur_patches (:)         !curvature
   real(r8), allocatable    :: sf_lut_patches  (:,:,:) !look up table of shadow factor of a patch
   real(r8), allocatable    :: sf_curve_patches(:,:,:) !curve parameters of shadow factor of a patch
   real(r8), allocatable    :: asp_type_patches  (:,:) !topographic aspect of each character of one patch
   real(r8), allocatable    :: slp_type_patches  (:,:) !topographic slope of each character of one patch
   real(r8), allocatable    :: area_type_patches (:,:) !area percentage of each character of one patch

! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: allocate_TimeInvariants
   PUBLIC :: deallocate_TimeInvariants
   PUBLIC :: READ_TimeInvariants
   PUBLIC :: WRITE_TimeInvariants

! PRIVATE MEMBER FUNCTIONS:

!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------

   SUBROUTINE allocate_TimeInvariants ()
   ! -------------------------------------------------------------------
   ! Allocates memory for CoLM 1d [numpatch] variables
   ! -------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_Vars_Global
   USE MOD_MPAS_MPI
   USE MOD_LandPatch, only: numpatch
   IMPLICIT NONE

      IF (.true.) THEN

         IF (numpatch > 0) THEN

            allocate (patchclass           (numpatch))
            allocate (patchtype            (numpatch))
            allocate (patchmask            (numpatch))

            allocate (patchlonr            (numpatch))
            allocate (patchlatr            (numpatch))

            allocate (lakedepth            (numpatch))
            allocate (dz_lake      (nl_lake,numpatch))

            allocate (soil_s_v_alb         (numpatch))
            allocate (soil_d_v_alb         (numpatch))
            allocate (soil_s_n_alb         (numpatch))
            allocate (soil_d_n_alb         (numpatch))
#ifdef HYPERSPECTRAL
            allocate (soil_alb        (nwl, numpatch))
#endif

            allocate (vf_quartz    (nl_soil,numpatch))
            allocate (vf_gravels   (nl_soil,numpatch))
            allocate (vf_om        (nl_soil,numpatch))
            allocate (vf_sand      (nl_soil,numpatch))
            allocate (vf_clay      (nl_soil,numpatch))
            allocate (wf_gravels   (nl_soil,numpatch))
            allocate (wf_sand      (nl_soil,numpatch))
            allocate (wf_clay      (nl_soil,numpatch))
            allocate (wf_om        (nl_soil,numpatch))
#ifdef DataAssimilation
            allocate (wf_silt      (nl_soil,numpatch))
#endif
            allocate (OM_density   (nl_soil,numpatch))
            allocate (BD_all       (nl_soil,numpatch))
            allocate (wfc          (nl_soil,numpatch))
            allocate (porsl        (nl_soil,numpatch))
            allocate (psi0         (nl_soil,numpatch))
            allocate (bsw          (nl_soil,numpatch))
            allocate (theta_r      (nl_soil,numpatch))
            allocate (BVIC                 (numpatch))

#ifdef vanGenuchten_Mualem_SOIL_MODEL
            allocate (alpha_vgm    (nl_soil,numpatch))
            allocate (L_vgm        (nl_soil,numpatch))
            allocate (n_vgm        (nl_soil,numpatch))
            allocate (sc_vgm       (nl_soil,numpatch))
            allocate (fc_vgm       (nl_soil,numpatch))
#endif
            allocate (soiltext             (numpatch))

            allocate (fsatmax              (numpatch))
            allocate (fsatdcf              (numpatch))
            allocate (topoweti             (numpatch))
            allocate (alp_twi              (numpatch))
            allocate (chi_twi              (numpatch))
            allocate (mu_twi               (numpatch))

            allocate (vic_b_infilt         (numpatch))
            allocate (vic_Dsmax            (numpatch))
            allocate (vic_Ds               (numpatch))
            allocate (vic_Ws               (numpatch))
            allocate (vic_c                (numpatch))

            allocate (hksati       (nl_soil,numpatch))
            allocate (csol         (nl_soil,numpatch))
            allocate (k_solids     (nl_soil,numpatch))
            allocate (dksatu       (nl_soil,numpatch))
            allocate (dksatf       (nl_soil,numpatch))
            allocate (dkdry        (nl_soil,numpatch))
            allocate (BA_alpha     (nl_soil,numpatch))
            allocate (BA_beta      (nl_soil,numpatch))
            allocate (htop                 (numpatch))
            allocate (hbot                 (numpatch))
            allocate (dbedrock             (numpatch))
            allocate (ibedrock             (numpatch))
            allocate (elvmean              (numpatch))
            allocate (elvstd               (numpatch))
            allocate (slpratio             (numpatch))

            IF (DEF_USE_Forcing_Downscaling) THEN
               ! Used for downscaling
               allocate (svf_patches                      (numpatch))
               allocate (asp_type_patches  (num_slope_type,numpatch))
               allocate (slp_type_patches  (num_slope_type,numpatch))
               allocate (area_type_patches (num_slope_type,numpatch))
               allocate (cur_patches                      (numpatch))
#ifdef SinglePoint
               allocate (sf_lut_patches   (num_azimuth,num_zenith,numpatch))
#else
               allocate (sf_curve_patches (num_azimuth,num_zenith_parameter,numpatch))
#endif
            ENDIF

            IF (DEF_USE_Forcing_Downscaling_Simple) THEN
               ! Used for downscaling
               allocate (asp_type_patches  (num_aspect_type,numpatch))
               allocate (slp_type_patches  (num_aspect_type,numpatch))
               allocate (cur_patches                       (numpatch))
            ENDIF

         ENDIF
      ENDIF

#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
      CALL allocate_PFTimeInvariants
#endif

#ifdef BGC
      CALL allocate_BGCTimeInvariants
#endif

#ifdef URBAN_MODEL
      CALL allocate_UrbanTimeInvariants
#endif

   END SUBROUTINE allocate_TimeInvariants

   !---------------------------------------
   SUBROUTINE READ_TimeInvariants (lc_year, casename, dir_restart)

   !====================================================================
   ! Original version: Yongjiu Dai, September 15, 1999, 03/2014
   !====================================================================

   USE MOD_Namelist
   USE MOD_MPAS_MPI
   USE MOD_NetCDFVector
   USE MOD_NetCDFSerial
#ifdef RangeCheck
   USE MOD_RangeCheck
#endif
   USE MOD_LandPatch
   USE MOD_Vars_Global
   USE MOD_Const_LC, only: patchtypes

   IMPLICIT NONE

   integer         , intent(in) :: lc_year
   character(len=*), intent(in) :: casename
   character(len=*), intent(in) :: dir_restart

   ! Local variables
   character(len=:), allocatable :: file_restart
   character(len=4) :: cyear

      write(cyear,'(i4.4)') lc_year
      file_restart = trim(dir_restart) // '/const/' // trim(casename) //'_restart_const' // '_lc' // trim(cyear) // '.nc'

#ifdef MPAS_EMBEDDED_COLM
      IF (numpatch > 0) THEN
#endif

      CALL ncio_read_vector (file_restart, 'patchclass',   landpatch, patchclass)          !
      CALL ncio_read_vector (file_restart, 'patchtype' ,   landpatch, patchtype )          !
      CALL ncio_read_vector (file_restart, 'patchmask' ,   landpatch, patchmask )          !

      CALL ncio_read_vector (file_restart, 'patchlonr' ,   landpatch, patchlonr )          !
      CALL ncio_read_vector (file_restart, 'patchlatr' ,   landpatch, patchlatr )          !

      CALL ncio_read_vector (file_restart, 'lakedepth',    landpatch, lakedepth)           !
      CALL ncio_read_vector (file_restart, 'dz_lake' ,     nl_lake, landpatch, dz_lake)    !

      CALL ncio_read_vector (file_restart, 'soil_s_v_alb', landpatch, soil_s_v_alb)        ! albedo of visible of the saturated soil
      CALL ncio_read_vector (file_restart, 'soil_d_v_alb', landpatch, soil_d_v_alb)        ! albedo of visible of the dry soil
      CALL ncio_read_vector (file_restart, 'soil_s_n_alb', landpatch, soil_s_n_alb)        ! albedo of near infrared of the saturated soil
      CALL ncio_read_vector (file_restart, 'soil_d_n_alb', landpatch, soil_d_n_alb)        ! albedo of near infrared of the dry soil
#ifdef HYPERSPECTRAL
      CALL ncio_read_vector (file_restart, 'soil_alb'    , nwl, landpatch, soil_alb ) ! hyper spectral soil albedo. (numpatch, nwl)
#endif

      CALL ncio_read_vector (file_restart, 'vf_quartz ',   nl_soil, landpatch, vf_quartz ) ! volumetric fraction of quartz within mineral soil
      CALL ncio_read_vector (file_restart, 'vf_gravels',   nl_soil, landpatch, vf_gravels) ! volumetric fraction of gravels
      CALL ncio_read_vector (file_restart, 'vf_om     ',   nl_soil, landpatch, vf_om     ) ! volumetric fraction of organic matter
      CALL ncio_read_vector (file_restart, 'vf_sand   ',   nl_soil, landpatch, vf_sand   ) ! volumetric fraction of sand
      CALL ncio_read_vector (file_restart, 'vf_clay   ',   nl_soil, landpatch, vf_clay  ,defval = 0.1 ) ! volumetric fraction of clay
      CALL ncio_read_vector (file_restart, 'wf_gravels',   nl_soil, landpatch, wf_gravels) ! gravimetric fraction of gravels
      CALL ncio_read_vector (file_restart, 'wf_sand   ',   nl_soil, landpatch, wf_sand   ) ! gravimetric fraction of sand
      CALL ncio_read_vector (file_restart, 'wf_clay   ',   nl_soil, landpatch, wf_clay   ) ! gravimetric fraction of clay
      CALL ncio_read_vector (file_restart, 'wf_om     ',   nl_soil, landpatch, wf_om     ) ! gravimetric fraction of om
      CALL ncio_read_vector (file_restart, 'OM_density',   nl_soil, landpatch, OM_density) ! OM density
      CALL ncio_read_vector (file_restart, 'BD_all    ',   nl_soil, landpatch, BD_all    ) ! bulk density of soil
      CALL ncio_read_vector (file_restart, 'wfc       ',   nl_soil, landpatch, wfc       ) ! field capacity
      CALL ncio_read_vector (file_restart, 'porsl  ' ,     nl_soil, landpatch, porsl     ) ! fraction of soil that is voids [-]
      CALL ncio_read_vector (file_restart, 'psi0   ' ,     nl_soil, landpatch, psi0      ) ! minimum soil suction [mm] (NOTE: "-" valued)
      CALL ncio_read_vector (file_restart, 'bsw    ' ,     nl_soil, landpatch, bsw       ) ! clapp and hornberger "b" parameter [-]
      CALL ncio_read_vector (file_restart, 'theta_r  ' ,   nl_soil, landpatch, theta_r   ) ! residual moisture content [-]
      CALL ncio_read_vector (file_restart, 'BVIC  ' ,      landpatch, BVIC   )             ! b parameter in Fraction of saturated soil in a grid calculated by VIC
#ifdef vanGenuchten_Mualem_SOIL_MODEL
      CALL ncio_read_vector (file_restart, 'alpha_vgm' ,   nl_soil, landpatch, alpha_vgm ) ! a parameter corresponding approximately to the inverse of the air-entry value
      CALL ncio_read_vector (file_restart, 'L_vgm    ' ,   nl_soil, landpatch, L_vgm     ) ! pore-connectivity parameter [dimensionless]
      CALL ncio_read_vector (file_restart, 'n_vgm    ' ,   nl_soil, landpatch, n_vgm     ) ! a shape parameter [dimensionless]
      CALL ncio_read_vector (file_restart, 'sc_vgm   ' ,   nl_soil, landpatch, sc_vgm    ) ! saturation at the air entry value in the classical vanGenuchten model [-]
      CALL ncio_read_vector (file_restart, 'fc_vgm   ' ,   nl_soil, landpatch, fc_vgm    ) ! a scaling factor by using air entry value in the Mualem model [-]
#endif
#ifdef DataAssimilation
      IF (numpatch > 0 .and. .true.) wf_silt = 1.0_r8 - wf_gravels - wf_sand - wf_clay - wf_om
#endif

      CALL ncio_read_vector (file_restart, 'soiltext', landpatch, soiltext, defval = 0    )

      IF (DEF_Runoff_SCHEME == 0) THEN
         CALL ncio_read_vector (file_restart, 'topoweti', landpatch, topoweti, defval = 9.27 )
         CALL ncio_read_vector (file_restart, 'fsatmax ', landpatch, fsatmax , defval = 0.38 )
         CALL ncio_read_vector (file_restart, 'fsatdcf ', landpatch, fsatdcf , defval = 0.55 )
         CALL ncio_read_vector (file_restart, 'alp_twi ', landpatch, alp_twi , defval = 1.34 )
         CALL ncio_read_vector (file_restart, 'chi_twi ', landpatch, chi_twi , defval = 1.61 )
         CALL ncio_read_vector (file_restart, 'mu_twi  ', landpatch, mu_twi  , defval = 6.95 )
      ENDIF

      CALL ncio_read_vector (file_restart, 'vic_b_infilt', landpatch, vic_b_infilt)
      CALL ncio_read_vector (file_restart, 'vic_Dsmax'   , landpatch, vic_Dsmax   )
      CALL ncio_read_vector (file_restart, 'vic_Ds'      , landpatch, vic_Ds      )
      CALL ncio_read_vector (file_restart, 'vic_Ws'      , landpatch, vic_Ws      )
      CALL ncio_read_vector (file_restart, 'vic_c'       , landpatch, vic_c       )

      CALL ncio_read_vector (file_restart, 'hksati ' ,     nl_soil, landpatch, hksati )    ! hydraulic conductivity at saturation [mm h2o/s]
      CALL ncio_read_vector (file_restart, 'csol   ' ,     nl_soil, landpatch, csol   )    ! heat capacity of soil solids [J/(m3 K)]
      CALL ncio_read_vector (file_restart, 'k_solids',     nl_soil, landpatch, k_solids)   ! thermal conductivity of soil solids [W/m-K]
      CALL ncio_read_vector (file_restart, 'dksatu ' ,     nl_soil, landpatch, dksatu )    ! thermal conductivity of unfrozen saturated soil [W/m-K]
      CALL ncio_read_vector (file_restart, 'dksatf ' ,     nl_soil, landpatch, dksatf )    ! thermal conductivity of frozen saturated soil [W/m-K]
      CALL ncio_read_vector (file_restart, 'dkdry  ' ,     nl_soil, landpatch, dkdry  )    ! thermal conductivity for dry soil  [W/(m-K)]
      CALL ncio_read_vector (file_restart, 'BA_alpha',     nl_soil, landpatch, BA_alpha)   ! alpha in Balland and Arp(2005) thermal conductivity scheme
      CALL ncio_read_vector (file_restart, 'BA_beta' ,     nl_soil, landpatch, BA_beta )   ! beta in Balland and Arp(2005) thermal conductivity scheme
      CALL ncio_read_vector (file_restart, 'htop'    ,     landpatch, htop)                !
      CALL ncio_read_vector (file_restart, 'hbot'    ,     landpatch, hbot)                !

      IF(DEF_USE_BEDROCK)THEN
         CALL ncio_read_vector (file_restart, 'debdrock' ,    landpatch, dbedrock)         !
         CALL ncio_read_vector (file_restart, 'ibedrock' ,    landpatch, ibedrock)         !
      ENDIF

      CALL ncio_read_vector (file_restart, 'elvmean ', landpatch, elvmean )         !
      CALL ncio_read_vector (file_restart, 'elvstd  ', landpatch, elvstd  )         !
      CALL ncio_read_vector (file_restart, 'slpratio', landpatch, slpratio)         !

#ifdef MPAS_EMBEDDED_COLM
      ENDIF
#endif

      CALL ncio_read_bcast_serial (file_restart, 'zlnd  ', zlnd  ) ! roughness length for soil [m]
      CALL ncio_read_bcast_serial (file_restart, 'zsno  ', zsno  ) ! roughness length for snow [m]
      CALL ncio_read_bcast_serial (file_restart, 'csoilc', csoilc) ! drag coefficient for soil under canopy [-]
      CALL ncio_read_bcast_serial (file_restart, 'dewmx ', dewmx ) ! maximum dew
      ! CALL ncio_read_bcast_serial (file_restart, 'wtfact', wtfact) ! fraction of model area with high water table
      CALL ncio_read_bcast_serial (file_restart, 'capr  ', capr  ) ! tuning factor to turn first layer T into surface T
      CALL ncio_read_bcast_serial (file_restart, 'cnfac ', cnfac ) ! Crank Nicholson factor between 0 and 1
      CALL ncio_read_bcast_serial (file_restart, 'ssi   ', ssi   ) ! irreducible water saturation of snow
      CALL ncio_read_bcast_serial (file_restart, 'wimp  ', wimp  ) ! water impermeable IF porosity less than wimp
      CALL ncio_read_bcast_serial (file_restart, 'pondmx', pondmx) ! ponding depth (mm)
      CALL ncio_read_bcast_serial (file_restart, 'smpmax', smpmax) ! wilting point potential in mm
      CALL ncio_read_bcast_serial (file_restart, 'smpmin', smpmin) ! restriction for min of soil poten. (mm)
      CALL ncio_read_bcast_serial (file_restart, 'smpmax_hr', smpmax_hr) ! wilting point potential in mm
      CALL ncio_read_bcast_serial (file_restart, 'smpmin_hr', smpmin_hr) ! restriction for min of soil poten. (mm)
      CALL ncio_read_bcast_serial (file_restart, 'trsmx0', trsmx0) ! max transpiration for moist soil+100% veg.  [mm/s]
      CALL ncio_read_bcast_serial (file_restart, 'tcrit ', tcrit ) ! critical temp. to determine rain or snow
      CALL ncio_read_bcast_serial (file_restart, 'wetwatmax', wetwatmax) ! maximum wetland water (mm)

#ifdef MPAS_EMBEDDED_COLM
      IF (numpatch > 0) THEN
#endif

      IF (DEF_USE_Forcing_Downscaling) THEN
         CALL ncio_read_vector (file_restart, 'slp_type_patches' , num_slope_type, landpatch, slp_type_patches)
         CALL ncio_read_vector (file_restart, 'svf_patches'      ,                 landpatch, svf_patches     )
         CALL ncio_read_vector (file_restart, 'asp_type_patches' , num_slope_type, landpatch, asp_type_patches)
         CALL ncio_read_vector (file_restart, 'area_type_patches', num_slope_type, landpatch, area_type_patches)
         CALL ncio_read_vector (file_restart, 'cur_patches'      ,                 landpatch, cur_patches )
#ifdef SinglePoint
         CALL ncio_read_vector (file_restart, 'sf_lut_patches'   , num_azimuth , num_zenith, landpatch, sf_lut_patches)
#else
         CALL ncio_read_vector (file_restart, 'sf_curve_patches' , num_azimuth , num_zenith_parameter, landpatch, sf_curve_patches)
#endif
      ENDIF

      IF (DEF_USE_Forcing_Downscaling_Simple) THEN
         CALL ncio_read_vector (file_restart, 'slp_type_patches' , num_aspect_type, landpatch, slp_type_patches)
         CALL ncio_read_vector (file_restart, 'asp_type_patches' , num_aspect_type, landpatch, asp_type_patches)
         CALL ncio_read_vector (file_restart, 'cur_patches'      ,                 landpatch, cur_patches )
      ENDIF

#ifdef MPAS_EMBEDDED_COLM
      ENDIF
#endif

#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
#ifdef SinglePoint
      IF (patchtypes(SITE_landtype) == 0) THEN
         file_restart = trim(dir_restart) // '/const/' // trim(casename) //'_restart_pft_const' // '_lc' // trim(cyear) // '.nc'
         CALL READ_PFTimeInvariants (file_restart)
      ENDIF
#else
      file_restart = trim(dir_restart) // '/const/' // trim(casename) //'_restart_pft_const' // '_lc' // trim(cyear) // '.nc'
      CALL READ_PFTimeInvariants (file_restart)
#endif
#endif

#if (defined BGC)
      file_restart = trim(dir_restart) // '/const/' // trim(casename) //'_restart_bgc_const' // '_lc' // trim(cyear) // '.nc'
      CALL READ_BGCTimeInvariants (file_restart)
#endif

#if (defined URBAN_MODEL)
      file_restart = trim(dir_restart) // '/const/' // trim(casename) //'_restart_urb_const' // '_lc' // trim(cyear) // '.nc'
      CALL READ_UrbanTimeInvariants (file_restart)
#endif

#ifdef RangeCheck
      CALL check_TimeInvariants ()
#endif

#ifdef MPAS_MPI
      CALL mpi_barrier (mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('time-invariant restart read completion')
#endif

      IF (mpas_is_root) THEN
         write(*,*) 'Loading Time Invariants done.'
      ENDIF

   END SUBROUTINE READ_TimeInvariants

   !---------------------------------------
   SUBROUTINE WRITE_TimeInvariants (lc_year, casename, dir_restart)

   !====================================================================
   ! Original version: Yongjiu Dai, September 15, 1999, 03/2014
   !====================================================================

   USE MOD_Namelist, only: DEF_REST_CompressLevel, DEF_USE_BEDROCK
   USE MOD_MPAS_MPI
   USE MOD_NetCDFSerial
   USE MOD_NetCDFVector
   USE MOD_Utils, only: make_directory
   USE MOD_LandPatch
   USE MOD_Vars_Global

   IMPLICIT NONE

   integer         , intent(in) :: lc_year
   character(len=*), intent(in) :: casename
   character(len=*), intent(in) :: dir_restart

   ! Local Variables
   character(len=:), allocatable :: file_restart
   character(len=4) :: cyear
   integer :: compress

      compress = DEF_REST_CompressLevel

      write(cyear,'(i4.4)') lc_year

      IF (mpas_is_root) THEN
         CALL make_directory(trim(dir_restart)//'/const')
      ENDIF
#ifdef MPAS_MPI
      CALL mpi_barrier (mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('time-invariant restart directory creation')
#endif

      file_restart = trim(dir_restart) // '/const/' // trim(casename) //'_restart_const' //'_lc'// trim(cyear) // '.nc'

      CALL ncio_create_file_vector (file_restart, landpatch)

      CALL ncio_define_dimension_vector (file_restart, landpatch, 'patch')
      CALL ncio_define_dimension_vector (file_restart, landpatch, 'soil', nl_soil)
      CALL ncio_define_dimension_vector (file_restart, landpatch, 'lake', nl_lake)
      CALL ncio_define_dimension_vector (file_restart, landpatch, 'band', 2)
      CALL ncio_define_dimension_vector (file_restart, landpatch, 'rtyp', 2)
      CALL ncio_define_dimension_vector (file_restart, landpatch, 'snow',     -maxsnl       )
      CALL ncio_define_dimension_vector (file_restart, landpatch, 'snowp1',   -maxsnl+1     )
      CALL ncio_define_dimension_vector (file_restart, landpatch, 'soilsnow', nl_soil-maxsnl)
      CALL ncio_define_dimension_vector (file_restart, landpatch, 'soil',     nl_soil)
      CALL ncio_define_dimension_vector (file_restart, landpatch, 'lake',     nl_lake)
      CALL ncio_define_dimension_vector (file_restart, landpatch, 'type',     num_slope_type)
      CALL ncio_define_dimension_vector (file_restart, landpatch, 'azi',      num_azimuth)
      CALL ncio_define_dimension_vector (file_restart, landpatch, 'zen',      num_zenith)
      CALL ncio_define_dimension_vector (file_restart, landpatch, 'zen_p',    num_zenith_parameter)
      CALL ncio_define_dimension_vector (file_restart, landpatch, 'type_a',    num_aspect_type)
      CALL ncio_define_dimension_vector (file_restart, landpatch, 'wavelength', nwl)

      CALL ncio_write_vector (file_restart, 'patchclass', 'patch', landpatch, patchclass)                            !
      CALL ncio_write_vector (file_restart, 'patchtype' , 'patch', landpatch, patchtype )                            !
      CALL ncio_write_vector (file_restart, 'patchmask' , 'patch', landpatch, patchmask )                            !

      CALL ncio_write_vector (file_restart, 'patchlonr' , 'patch', landpatch, patchlonr )                            !
      CALL ncio_write_vector (file_restart, 'patchlatr' , 'patch', landpatch, patchlatr )                            !

      CALL ncio_write_vector (file_restart, 'lakedepth' , 'patch', landpatch, lakedepth , compress)                  !
      CALL ncio_write_vector (file_restart, 'dz_lake'   ,  'lake', nl_lake, 'patch', landpatch, dz_lake, compress)   !

      CALL ncio_write_vector (file_restart, 'soil_s_v_alb', 'patch', landpatch, soil_s_v_alb, compress)              ! albedo of visible of the saturated soil
      CALL ncio_write_vector (file_restart, 'soil_d_v_alb', 'patch', landpatch, soil_d_v_alb, compress)              ! albedo of visible of the dry soil
      CALL ncio_write_vector (file_restart, 'soil_s_n_alb', 'patch', landpatch, soil_s_n_alb, compress)              ! albedo of near infrared of the saturated soil
      CALL ncio_write_vector (file_restart, 'soil_d_n_alb', 'patch', landpatch, soil_d_n_alb, compress)              ! albedo of near infrared of the dry soil
#ifdef HYPERSPECTRAL
      CALL ncio_write_vector (file_restart, 'soil_alb'   , 'wavelength', nwl, 'patch', landpatch, soil_alb, compress) ! hyper spectral soil albedo. (numpatch, nwl)
#endif

      CALL ncio_write_vector (file_restart, 'vf_quartz ', 'soil', nl_soil, 'patch', landpatch, vf_quartz , compress) ! volumetric fraction of quartz within mineral soil
      CALL ncio_write_vector (file_restart, 'vf_gravels', 'soil', nl_soil, 'patch', landpatch, vf_gravels, compress) ! volumetric fraction of gravels
      CALL ncio_write_vector (file_restart, 'vf_om     ', 'soil', nl_soil, 'patch', landpatch, vf_om     , compress) ! volumetric fraction of organic matter
      CALL ncio_write_vector (file_restart, 'vf_sand   ', 'soil', nl_soil, 'patch', landpatch, vf_sand   , compress) ! volumetric fraction of sand
      CALL ncio_write_vector (file_restart, 'vf_clay   ', 'soil', nl_soil, 'patch', landpatch, vf_clay   , compress) ! volumetric fraction of clay
      CALL ncio_write_vector (file_restart, 'wf_gravels', 'soil', nl_soil, 'patch', landpatch, wf_gravels, compress) ! gravimetric fraction of gravels
      CALL ncio_write_vector (file_restart, 'wf_sand   ', 'soil', nl_soil, 'patch', landpatch, wf_sand   , compress) ! gravimetric fraction of sand
      CALL ncio_write_vector (file_restart, 'wf_clay   ', 'soil', nl_soil, 'patch', landpatch, wf_clay   , compress) ! gravimetric fraction of clay
      CALL ncio_write_vector (file_restart, 'wf_om     ', 'soil', nl_soil, 'patch', landpatch, wf_om     , compress) ! gravimetric fraction of om
      CALL ncio_write_vector (file_restart, 'OM_density', 'soil', nl_soil, 'patch', landpatch, OM_density, compress) ! OM_density
      CALL ncio_write_vector (file_restart, 'BD_all    ', 'soil', nl_soil, 'patch', landpatch, BD_all    , compress) ! bulk density of soil
      CALL ncio_write_vector (file_restart, 'wfc       ', 'soil', nl_soil, 'patch', landpatch, wfc       , compress) ! field capacity
      CALL ncio_write_vector (file_restart, 'porsl     ', 'soil', nl_soil, 'patch', landpatch, porsl     , compress) ! fraction of soil that is voids [-]
      CALL ncio_write_vector (file_restart, 'psi0      ', 'soil', nl_soil, 'patch', landpatch, psi0      , compress) ! minimum soil suction [mm] (NOTE: "-" valued)
      CALL ncio_write_vector (file_restart, 'bsw       ', 'soil', nl_soil, 'patch', landpatch, bsw       , compress) ! clapp and hornberger "b" parameter [-]
      CALL ncio_write_vector (file_restart, 'theta_r  ' , 'soil', nl_soil, 'patch', landpatch, theta_r   , compress) ! residual moisture content [-]
      CALL ncio_write_vector (file_restart, 'BVIC    '  , 'patch', landpatch, BVIC, compress) ! b parameter in Fraction of saturated soil in a grid calculated by VIC

#ifdef vanGenuchten_Mualem_SOIL_MODEL
      CALL ncio_write_vector (file_restart, 'alpha_vgm' , 'soil', nl_soil, 'patch', landpatch, alpha_vgm , compress) ! a parameter corresponding approximately to the inverse of the air-entry value
      CALL ncio_write_vector (file_restart, 'L_vgm    ' , 'soil', nl_soil, 'patch', landpatch, L_vgm     , compress) ! pore-connectivity parameter [dimensionless]
      CALL ncio_write_vector (file_restart, 'n_vgm    ' , 'soil', nl_soil, 'patch', landpatch, n_vgm     , compress) ! a shape parameter [dimensionless]
      CALL ncio_write_vector (file_restart, 'sc_vgm   ' , 'soil', nl_soil, 'patch', landpatch, sc_vgm    , compress) ! saturation at the air entry value in the classical vanGenuchten model [-]
      CALL ncio_write_vector (file_restart, 'fc_vgm   ' , 'soil', nl_soil, 'patch', landpatch, fc_vgm    , compress) ! a scaling factor by using air entry value in the Mualem model [-]
#endif

      CALL ncio_write_vector (file_restart, 'soiltext', 'patch', landpatch, soiltext)

      IF (DEF_Runoff_SCHEME == 0) THEN
         CALL ncio_write_vector (file_restart, 'topoweti', 'patch', landpatch, topoweti)
         CALL ncio_write_vector (file_restart, 'fsatmax ', 'patch', landpatch, fsatmax )
         CALL ncio_write_vector (file_restart, 'fsatdcf ', 'patch', landpatch, fsatdcf )
         CALL ncio_write_vector (file_restart, 'alp_twi ', 'patch', landpatch, alp_twi )
         CALL ncio_write_vector (file_restart, 'chi_twi ', 'patch', landpatch, chi_twi )
         CALL ncio_write_vector (file_restart, 'mu_twi  ', 'patch', landpatch, mu_twi  )
      ENDIF

      CALL ncio_write_vector (file_restart, 'vic_b_infilt', 'patch', landpatch, vic_b_infilt)
      CALL ncio_write_vector (file_restart, 'vic_Dsmax'   , 'patch', landpatch, vic_Dsmax   )
      CALL ncio_write_vector (file_restart, 'vic_Ds'      , 'patch', landpatch, vic_Ds      )
      CALL ncio_write_vector (file_restart, 'vic_Ws'      , 'patch', landpatch, vic_Ws      )
      CALL ncio_write_vector (file_restart, 'vic_c'       , 'patch', landpatch, vic_c       )

      CALL ncio_write_vector (file_restart, 'hksati   ' , 'soil', nl_soil, 'patch', landpatch, hksati    , compress) ! hydraulic conductivity at saturation [mm h2o/s]
      CALL ncio_write_vector (file_restart, 'csol     ' , 'soil', nl_soil, 'patch', landpatch, csol      , compress) ! heat capacity of soil solids [J/(m3 K)]
      CALL ncio_write_vector (file_restart, 'k_solids ' , 'soil', nl_soil, 'patch', landpatch, k_solids  , compress) ! thermal conductivity of soil solids [W/m-K]
      CALL ncio_write_vector (file_restart, 'dksatu   ' , 'soil', nl_soil, 'patch', landpatch, dksatu    , compress) ! thermal conductivity of saturated soil [W/m-K]
      CALL ncio_write_vector (file_restart, 'dksatf   ' , 'soil', nl_soil, 'patch', landpatch, dksatf    , compress) ! thermal conductivity of saturated soil [W/m-K]
      CALL ncio_write_vector (file_restart, 'dkdry    ' , 'soil', nl_soil, 'patch', landpatch, dkdry     , compress) ! thermal conductivity for dry soil  [W/(m-K)]
      CALL ncio_write_vector (file_restart, 'BA_alpha ' , 'soil', nl_soil, 'patch', landpatch, BA_alpha  , compress) ! alpha in Balland and Arp(2005) thermal conductivity scheme
      CALL ncio_write_vector (file_restart, 'BA_beta  ' , 'soil', nl_soil, 'patch', landpatch, BA_beta   , compress) ! beta in Balland and Arp(2005) thermal conductivity scheme

      CALL ncio_write_vector (file_restart, 'htop' , 'patch', landpatch, htop)                                       !
      CALL ncio_write_vector (file_restart, 'hbot' , 'patch', landpatch, hbot)                                       !

      IF(DEF_USE_BEDROCK)THEN
         CALL ncio_write_vector (file_restart, 'debdrock' , 'patch', landpatch, dbedrock)
         CALL ncio_write_vector (file_restart, 'ibedrock' , 'patch', landpatch, ibedrock)
      ENDIF

      CALL ncio_write_vector (file_restart, 'elvmean ', 'patch', landpatch, elvmean )
      CALL ncio_write_vector (file_restart, 'elvstd  ', 'patch', landpatch, elvstd  )
      CALL ncio_write_vector (file_restart, 'slpratio', 'patch', landpatch, slpratio)

      IF (DEF_USE_Forcing_Downscaling) THEN
         CALL ncio_write_vector (file_restart, 'svf_patches', 'patch', landpatch, svf_patches)
         CALL ncio_write_vector (file_restart, 'cur_patches', 'patch', landpatch, cur_patches)
         CALL ncio_write_vector (file_restart, 'slp_type_patches',  'type', num_slope_type, 'patch', landpatch, slp_type_patches)
         CALL ncio_write_vector (file_restart, 'asp_type_patches',  'type', num_slope_type, 'patch', landpatch, asp_type_patches)
         CALL ncio_write_vector (file_restart, 'area_type_patches', 'type', num_slope_type, 'patch', landpatch, area_type_patches)
#ifdef SinglePoint
         CALL ncio_write_vector (file_restart, 'sf_lut_patches',    'azi' , num_azimuth,'zen', num_zenith, 'patch', landpatch, sf_lut_patches)
#else
         CALL ncio_write_vector (file_restart, 'sf_curve_patches',  'azi' , num_azimuth,'zen_p', num_zenith_parameter, 'patch', landpatch, sf_curve_patches)
#endif
      ENDIF

      IF (DEF_USE_Forcing_Downscaling_Simple) THEN
         CALL ncio_write_vector (file_restart, 'cur_patches', 'patch', landpatch, cur_patches)
         CALL ncio_write_vector (file_restart, 'slp_type_patches',  'type_a', num_aspect_type, 'patch', landpatch, slp_type_patches)
         CALL ncio_write_vector (file_restart, 'asp_type_patches',  'type_a', num_aspect_type, 'patch', landpatch, asp_type_patches)
      ENDIF


#ifdef MPAS_MPI
      CALL mpi_barrier (mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('time-invariant vector write completion')
#endif

      if (mpas_is_root) then

#if (!defined(VectorInOneFileS) && !defined(VectorInOneFileP))
         CALL ncio_create_file (file_restart)
#endif

         CALL ncio_write_serial (file_restart, 'zlnd  ', zlnd  )       ! roughness length for soil [m]
         CALL ncio_write_serial (file_restart, 'zsno  ', zsno  )       ! roughness length for snow [m]
         CALL ncio_write_serial (file_restart, 'csoilc', csoilc)       ! drag coefficient for soil under canopy [-]
         CALL ncio_write_serial (file_restart, 'dewmx ', dewmx )       ! maximum dew
       ! CALL ncio_write_serial (file_restart, 'wtfact', wtfact)       ! fraction of model area with high water table
         CALL ncio_write_serial (file_restart, 'capr  ', capr  )       ! tuning factor to turn first layer T into surface T
         CALL ncio_write_serial (file_restart, 'cnfac ', cnfac )       ! Crank Nicholson factor between 0 and 1
         CALL ncio_write_serial (file_restart, 'ssi   ', ssi   )       ! irreducible water saturation of snow
         CALL ncio_write_serial (file_restart, 'wimp  ', wimp  )       ! water impermeable if porosity less than wimp
         CALL ncio_write_serial (file_restart, 'pondmx', pondmx)       ! ponding depth (mm)
         CALL ncio_write_serial (file_restart, 'smpmax', smpmax)       ! wilting point potential in mm
         CALL ncio_write_serial (file_restart, 'smpmin', smpmin)       ! restriction for min of soil poten. (mm)
         CALL ncio_write_serial (file_restart, 'smpmax_hr', smpmax_hr) ! wilting point potential in mm
         CALL ncio_write_serial (file_restart, 'smpmin_hr', smpmin_hr) ! restriction for min of soil poten. (mm)
         CALL ncio_write_serial (file_restart, 'trsmx0', trsmx0)       ! max transpiration for moist soil+100% veg.  [mm/s]
         CALL ncio_write_serial (file_restart, 'tcrit ', tcrit )       ! critical temp. to determine rain or snow
         CALL ncio_write_serial (file_restart, 'wetwatmax', wetwatmax) ! maximum wetland water (mm)

      ENDIF

#ifdef MPAS_MPI
      CALL mpi_barrier (mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('time-invariant scalar write completion')
#endif

#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
      file_restart = trim(dir_restart) // '/const/' // trim(casename) //'_restart_pft_const' //'_lc'// trim(cyear) // '.nc'
      CALL WRITE_PFTimeInvariants (file_restart)
#endif

#if (defined BGC)
      file_restart = trim(dir_restart) // '/const/' // trim(casename) //'_restart_bgc_const' //'_lc'// trim(cyear) // '.nc'
      CALL WRITE_BGCTimeInvariants (file_restart)
#endif

#if (defined URBAN_MODEL)
      file_restart = trim(dir_restart) // '/const/' // trim(casename) //'_restart_urb_const' //'_lc'// trim(cyear) // '.nc'
      CALL WRITE_UrbanTimeInvariants (file_restart)
#endif

   END SUBROUTINE WRITE_TimeInvariants

   SUBROUTINE deallocate_TimeInvariants ()

   USE MOD_Namelist, only: DEF_USE_Forcing_Downscaling, DEF_USE_Forcing_Downscaling_Simple
   USE MOD_MPAS_MPI
   USE MOD_LandPatch, only: numpatch

   IMPLICIT NONE

      ! --------------------------------------------------
      ! Deallocates memory for CoLM 1d [numpatch] variables
      ! --------------------------------------------------

      IF (.true.) THEN

         IF (numpatch > 0) THEN

            deallocate (patchclass     )
            deallocate (patchtype      )
            deallocate (patchmask      )

            deallocate (patchlonr      )
            deallocate (patchlatr      )

            deallocate (lakedepth      )
            deallocate (dz_lake        )

            deallocate (soil_s_v_alb   )
            deallocate (soil_d_v_alb   )
            deallocate (soil_s_n_alb   )
            deallocate (soil_d_n_alb   )
#ifdef HYPERSPECTRAL
            deallocate (soil_alb       )
#endif

            deallocate (vf_quartz      )
            deallocate (vf_gravels     )
            deallocate (vf_om          )
            deallocate (vf_sand        )
            deallocate (vf_clay        )
            deallocate (wf_gravels     )
            deallocate (wf_sand        )
            deallocate (wf_clay        )
            deallocate (wf_om          )
#ifdef DataAssimilation
            deallocate (wf_silt        )
#endif
            deallocate (OM_density     )
            deallocate (BD_all         )
            deallocate (wfc            )
            deallocate (porsl          )
            deallocate (psi0           )
            deallocate (bsw            )
            deallocate (theta_r        )
            deallocate (BVIC           )

#ifdef vanGenuchten_Mualem_SOIL_MODEL
            deallocate (alpha_vgm      )
            deallocate (L_vgm          )
            deallocate (n_vgm          )
            deallocate (sc_vgm         )
            deallocate (fc_vgm         )
#endif
            deallocate (soiltext       )

            deallocate (fsatmax        )
            deallocate (fsatdcf        )
            deallocate (topoweti       )
            deallocate (alp_twi        )
            deallocate (chi_twi        )
            deallocate (mu_twi         )

            deallocate (vic_b_infilt   )
            deallocate (vic_Dsmax      )
            deallocate (vic_Ds         )
            deallocate (vic_Ws         )
            deallocate (vic_c          )

            deallocate (hksati         )
            deallocate (csol           )
            deallocate (k_solids       )
            deallocate (dksatu         )
            deallocate (dksatf         )
            deallocate (dkdry          )
            deallocate (BA_alpha       )
            deallocate (BA_beta        )

            deallocate (htop           )
            deallocate (hbot           )

            deallocate (dbedrock       )
            deallocate (ibedrock       )

            deallocate (elvmean        )
            deallocate (elvstd         )
            deallocate (slpratio       )

            IF (DEF_USE_Forcing_Downscaling) THEN
               deallocate(slp_type_patches  )
               deallocate(svf_patches       )
               deallocate(asp_type_patches  )
               deallocate(area_type_patches )
#ifdef SinglePoint
               deallocate(sf_lut_patches    )
#else
               deallocate(sf_curve_patches  )
#endif
               deallocate(cur_patches       )
            ENDIF

            IF (DEF_USE_Forcing_Downscaling_Simple) THEN
               deallocate(slp_type_patches  )
               deallocate(asp_type_patches  )
               deallocate(cur_patches       )
            ENDIF

         ENDIF
      ENDIF

#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
      CALL deallocate_PFTimeInvariants
#endif

#ifdef BGC
      CALL deallocate_BGCTimeInvariants
#endif

#ifdef URBAN_MODEL
      CALL deallocate_UrbanTimeInvariants
#endif
   END SUBROUTINE deallocate_TimeInvariants

#ifdef RangeCheck
   SUBROUTINE check_TimeInvariants ()

   USE MOD_MPAS_MPI
   USE MOD_RangeCheck
   USE MOD_Namelist, only: DEF_Runoff_SCHEME, DEF_TOPMOD_method, DEF_USE_BEDROCK, &
                           DEF_USE_Forcing_Downscaling, DEF_USE_Forcing_Downscaling_Simple

   IMPLICIT NONE

      real(r8), allocatable :: tmpcheck(:,:)

      IF (mpas_is_root) THEN
         write(*,'(/,A29)') 'Checking Time Invariants ...'
      ENDIF

#ifdef MPAS_MPI
      CALL mpi_barrier (mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('time-invariant range-check entry')
#endif

      CALL check_vector_data ('lakedepth    [m]     ', lakedepth   ) !
      CALL check_vector_data ('dz_lake      [m]     ', dz_lake     ) ! new lake scheme

      CALL check_vector_data ('soil_s_v_alb [-]     ', soil_s_v_alb) ! albedo of visible of the saturated soil
      CALL check_vector_data ('soil_d_v_alb [-]     ', soil_d_v_alb) ! albedo of visible of the dry soil
      CALL check_vector_data ('soil_s_n_alb [-]     ', soil_s_n_alb) ! albedo of near infrared of the saturated soil
      CALL check_vector_data ('soil_d_n_alb [-]     ', soil_d_n_alb) ! albedo of near infrared of the dry soil
#ifdef HYPERSPECTRAL
      CALL check_vector_data ('soil_alb     [-]     ', soil_alb    ) ! hyper spectral soil albedo.
#endif
      CALL check_vector_data ('vf_quartz    [m3/m3] ', vf_quartz   ) ! volumetric fraction of quartz within mineral soil
      CALL check_vector_data ('vf_gravels   [m3/m3] ', vf_gravels  ) ! volumetric fraction of gravels
      CALL check_vector_data ('vf_sand      [m3/m3] ', vf_sand     ) ! volumetric fraction of sand
      CALL check_vector_data ('vf_clay      [m3/m3] ', vf_clay     ) ! volumetric fraction of clay
      CALL check_vector_data ('vf_om        [m3/m3] ', vf_om       ) ! volumetric fraction of organic matter
      CALL check_vector_data ('wf_gravels   [kg/kg] ', wf_gravels  ) ! gravimetric fraction of gravels
      CALL check_vector_data ('wf_sand      [kg/kg] ', wf_sand     ) ! gravimetric fraction of sand
      CALL check_vector_data ('wf_clay      [kg/kg] ', wf_clay     ) ! gravimetric fraction of clay
      CALL check_vector_data ('wf_om        [kg/kg] ', wf_om       ) ! gravimetric fraction of om
      CALL check_vector_data ('OM_density   [kg/m3] ', OM_density  ) ! OM density
      CALL check_vector_data ('BD_all       [kg/m3] ', BD_all      ) ! bulk density of soils
      CALL check_vector_data ('wfc          [m3/m3] ', wfc         ) ! field capacity
      CALL check_vector_data ('porsl        [m3/m3] ', porsl       ) ! fraction of soil that is voids [-]
      CALL check_vector_data ('psi0         [mm]    ', psi0        ) ! minimum soil suction [mm] (NOTE: "-" valued)
      CALL check_vector_data ('bsw          [-]     ', bsw         ) ! clapp and hornberger "b" parameter [-]
#ifdef vanGenuchten_Mualem_SOIL_MODEL
      CALL check_vector_data ('theta_r      [m3/m3] ', theta_r     ) ! residual moisture content [-]
      CALL check_vector_data ('alpha_vgm    [-]     ', alpha_vgm   ) ! a parameter corresponding approximately to the inverse of the air-entry value
      CALL check_vector_data ('L_vgm        [-]     ', L_vgm       ) ! pore-connectivity parameter [dimensionless]
      CALL check_vector_data ('n_vgm        [-]     ', n_vgm       ) ! a shape parameter [dimensionless]
      CALL check_vector_data ('sc_vgm       [-]     ', sc_vgm      ) ! saturation at the air entry value in the classical vanGenuchten model [-]
      CALL check_vector_data ('fc_vgm       [-]     ', fc_vgm      ) ! a scaling factor by using air entry value in the Mualem model [-]
#endif

      IF ((DEF_Runoff_SCHEME == 0) .and. (DEF_TOPMOD_method == 1)) THEN
         CALL check_vector_data ('mean twi     [log m] ', topoweti) !
         CALL check_vector_data ('max sat frac area [-]', fsatmax ) !
         CALL check_vector_data ('sat frac area decay  ', fsatdcf ) !
      ENDIF

      IF ((DEF_Runoff_SCHEME == 0) .and. (DEF_TOPMOD_method == 2)) THEN
         CALL check_vector_data ('mean twi     [log m] ', topoweti) !
         CALL check_vector_data ('twi alpha in 3-gamma ', alp_twi )
         CALL check_vector_data ('twi chi   in 3-gamma ', chi_twi )
         CALL check_vector_data ('twi mu    in 3-gamma ', mu_twi  )
      ENDIF

      CALL check_vector_data ('hksati       [mm/s]  ', hksati      ) ! hydraulic conductivity at saturation [mm h2o/s]
      CALL check_vector_data ('csol         [J/m3/K]', csol        ) ! heat capacity of soil solids [J/(m3 K)]
      CALL check_vector_data ('k_solids     [W/m/K] ', k_solids    ) ! thermal conductivity of soil solids [W/m-K]
      CALL check_vector_data ('dksatu       [W/m/K] ', dksatu      ) ! thermal conductivity of unfrozen saturated soil [W/m-K]
      CALL check_vector_data ('dksatf       [W/m/K] ', dksatf      ) ! thermal conductivity of frozen saturated soil [W/m-K]
      CALL check_vector_data ('dkdry        [W/m/K] ', dkdry       ) ! thermal conductivity for dry soil  [W/(m-K)]
      CALL check_vector_data ('BA_alpha     [-]     ', BA_alpha    ) ! alpha in Balland and Arp(2005) thermal conductivity scheme
      CALL check_vector_data ('BA_beta      [-]     ', BA_beta     ) ! beta in Balland and Arp(2005) thermal conductivity scheme

      CALL check_vector_data ('soiltexture  [-]     ', soiltext, -1) !

      CALL check_vector_data ('htop         [m]     ', htop        )
      CALL check_vector_data ('hbot         [m]     ', hbot        )

      IF(DEF_USE_BEDROCK)THEN
         CALL check_vector_data ('dbedrock     [m]     ', dbedrock ) !
      ENDIF

      CALL check_vector_data ('elvmean      [m]     ', elvmean     ) !
      CALL check_vector_data ('elvstd       [m]     ', elvstd      ) !
      CALL check_vector_data ('slpratio     [-]     ', slpratio    ) !

      IF (DEF_Runoff_SCHEME == 3) THEN
         CALL check_vector_data ('BVIC         [-]     ', BVIC        ) !
      ENDIF

      IF (DEF_USE_Forcing_Downscaling) THEN
         CALL check_vector_data ('slp_type     [rad]   ', slp_type_patches ) ! slope
         CALL check_vector_data ('svf          [-]     ', svf_patches      ) ! sky view factor
         CALL check_vector_data ('asp_type     [rad]   ', asp_type_patches ) ! aspect
         CALL check_vector_data ('area_type    [-]     ', area_type_patches) ! area percent
         CALL check_vector_data ('cur          [-]     ', cur_patches      )
#ifdef SinglePoint
         CALL check_vector_data ('sf_lut       [-]     ', sf_lut_patches   ) ! shadow mask
#else
         IF (allocated(sf_curve_patches)) allocate(tmpcheck(size(sf_curve_patches,1),size(sf_curve_patches,3)))

         IF (allocated(sf_curve_patches)) tmpcheck = sf_curve_patches(:,1,:)
         CALL check_vector_data ('1 sf_curve p [-]     ', tmpcheck) ! shadow mask
         IF (allocated(sf_curve_patches)) tmpcheck = sf_curve_patches(:,2,:)
         CALL check_vector_data ('2 sf_curve p [-]     ', tmpcheck) ! shadow mask
         IF (allocated(sf_curve_patches)) tmpcheck = sf_curve_patches(:,3,:)
         CALL check_vector_data ('3 sf_curve p [-]     ', tmpcheck) ! shadow mask

         IF (allocated(tmpcheck)) deallocate(tmpcheck)
#endif
      ENDIF

      IF (DEF_USE_Forcing_Downscaling_Simple) THEN
         CALL check_vector_data ('slp_type     [rad]   ', slp_type_patches ) ! slope
         CALL check_vector_data ('asp_type     [-]     ', asp_type_patches ) ! aspect fraction of direction of patches
         CALL check_vector_data ('cur          [-]     ', cur_patches      )
      ENDIF

#ifdef MPAS_MPI
      CALL mpi_barrier (mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('time-invariant range-check completion')
#endif

      IF (mpas_is_root) THEN
         write(*,'(/,A)') 'Checking Constants ...'
         write(*,'(A,E20.10)') 'zlnd   [m]    ', zlnd      ! roughness length for soil [m]
         write(*,'(A,E20.10)') 'zsno   [m]    ', zsno      ! roughness length for snow [m]
         write(*,'(A,E20.10)') 'csoilc [-]    ', csoilc    ! drag coefficient for soil under canopy [-]
         write(*,'(A,E20.10)') 'dewmx  [mm]   ', dewmx     ! maximum dew
       ! write(*,'(A,E20.10)') 'wtfact [-]    ', wtfact    ! fraction of model area with high water table
         write(*,'(A,E20.10)') 'capr   [-]    ', capr      ! tuning factor to turn first layer T into surface T
         write(*,'(A,E20.10)') 'cnfac  [-]    ', cnfac     ! Crank Nicholson factor between 0 and 1
         write(*,'(A,E20.10)') 'ssi    [-]    ', ssi       ! irreducible water saturation of snow
         write(*,'(A,E20.10)') 'wimp   [m3/m3]', wimp      ! water impermeable IF porosity less than wimp
         write(*,'(A,E20.10)') 'pondmx [mm]   ', pondmx    ! ponding depth (mm)
         write(*,'(A,E20.10)') 'smpmax [mm]   ', smpmax    ! wilting point potential in mm
         write(*,'(A,E20.10)') 'smpmin [mm]   ', smpmin    ! restriction for min of soil poten. (mm)
         write(*,'(A,E20.10)') 'smpmax_hr [mm]', smpmax_hr ! wilting point potential in mm
         write(*,'(A,E20.10)') 'smpmin_hr [mm]', smpmin_hr ! restriction for min of soil poten. (mm)
         write(*,'(A,E20.10)') 'trsmx0 [mm/s] ', trsmx0    ! max transpiration for moist soil+100% veg.  [mm/s]
         write(*,'(A,E20.10)') 'tcrit  [K]    ', tcrit     ! critical temp. to determine rain or snow
         write(*,'(A,E20.10)') 'wetwatmax [mm]', wetwatmax ! maximum wetland water (mm)
      ENDIF

#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
      CALL check_PFTimeInvariants
#endif

#ifdef BGC
      CALL check_BGCTimeInvariants
#endif

   END SUBROUTINE check_TimeInvariants
#endif

END MODULE MOD_Vars_TimeInvariants
! ---------- EOP ------------
