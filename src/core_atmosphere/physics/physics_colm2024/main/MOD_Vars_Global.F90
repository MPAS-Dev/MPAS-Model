#include <define.h>

MODULE MOD_Vars_Global
!-----------------------------------------------------------------------
!
! !DESCRIPTION:
!  Define some global variables
!
! !REVISIONS:
!  Hua Yuan, 08/2019: initial version partly adapted from CoLM2014
!  TODO ...
!
!-----------------------------------------------------------------------
! !USES:
   USE MOD_Precision
   USE MOD_Namelist
   IMPLICIT NONE
   SAVE

#ifdef LULC_USGS
   ! GLCC USGS number of land cover category
   integer, parameter :: N_land_classification = 24
   ! GLCC USGS land cover named index (could be added IF needed)
   integer, parameter :: URBAN     = 1
   integer, parameter :: WATERBODY = 16
   integer, parameter :: WETLAND   = 17
   integer, parameter :: CROPLAND  = 7
   integer, parameter :: GLACIERS  = 24
#else
   ! MODIS IGBP number of land cover category
   integer, parameter :: N_land_classification = 17
   ! MODIS IGBP land cover named index (could be added IF needed)
   integer, parameter :: WETLAND   = 11
   integer, parameter :: CROPLAND  = 12
   integer, parameter :: URBAN     = 13
   integer, parameter :: GLACIERS  = 15
   integer, parameter :: WATERBODY = 17
#endif

   ! number of plant functional types
#ifndef CROP
   integer, parameter :: N_PFT     = 16
   integer, parameter :: N_CFT     = 0
#else
   integer, parameter :: N_PFT     = 15
   integer, parameter :: N_CFT     = 64
#endif

   ! urban type number
   integer :: N_URB

   ! vertical layer number
   integer, parameter :: maxsnl    = -5
   integer, parameter :: nl_soil   = 10
   integer, parameter :: nl_soil_full = 15

   integer, parameter :: nl_lake   = 10
   integer, parameter :: nl_roof   = 10
   integer, parameter :: nl_wall   = 10
   integer, parameter :: nvegwcs   = 4  ! number of vegetation water potential nodes
   integer, parameter :: nwl       = 211  ! number of hyperspectral wavelengths
   ! used for downscaling
   integer, parameter :: num_slope_type       = 4
   integer, parameter :: num_aspect_type      = 9
   integer, parameter :: num_zenith           = 101
   integer, parameter :: num_zenith_parameter = 3
   integer, parameter :: num_azimuth          = 16

   ! bgc variables
   integer, parameter :: ndecomp_pools        = 7
   integer, parameter :: ndecomp_transitions  = 10
   integer, parameter :: npcropmin            = 17
   real(r8),parameter :: zmin_bedrock         = 0.4
   integer, parameter :: nbedrock             = 10
   integer, parameter :: ndecomp_pools_vr     = ndecomp_pools * nl_soil

   ! crop index
   integer, parameter :: noveg                = 0
   integer, parameter :: nbrdlf_evr_shrub     = 9
   integer, parameter :: nbrdlf_dcd_brl_shrub = 11
   integer, parameter :: nc3crop              = 15
   integer, parameter :: nc3irrig             = 16
   integer, parameter :: ntmp_corn            = 17 ! temperate_corn
   integer, parameter :: nirrig_tmp_corn      = 18 ! irrigated temperate corn
   integer, parameter :: nswheat              = 19 ! spring wheat
   integer, parameter :: nirrig_swheat        = 20 ! irrigated spring wheat
   integer, parameter :: nwwheat              = 21 ! winter wheat
   integer, parameter :: nirrig_wwheat        = 22 ! irrigated winter wheat
   integer, parameter :: ntmp_soybean         = 23 ! temperate soybean
   integer, parameter :: nirrig_tmp_soybean   = 24 ! irrigated temperate soybean
   integer, parameter :: ncotton              = 41 ! cotton
   integer, parameter :: nirrig_cotton        = 42 ! irrigated cotton
   integer, parameter :: nrice                = 61 ! rice
   integer, parameter :: nirrig_rice          = 62 ! irrigated rice
   integer, parameter :: nsugarcane           = 67 ! sugarcane
   integer, parameter :: nirrig_sugarcane     = 68 ! irrigated sugarcane
   integer, parameter :: nmiscanthus          = 71 ! miscanthus
   integer, parameter :: nirrig_miscanthus    = 72 ! irrigated miscanthus
   integer, parameter :: nswitchgrass         = 73 ! switchgrass
   integer, parameter :: nirrig_switchgrass   = 74 ! irrigated switchgrass
   integer, parameter :: ntrp_corn            = 75 ! tropical corn
   integer, parameter :: nirrig_trp_corn      = 76 ! irrigated tropical corn
   integer, parameter :: ntrp_soybean         = 77 ! tropical soybean
   integer, parameter :: nirrig_trp_soybean   = 78 ! irrigated tropical soybean

   real(r8) :: z_soi (1:nl_soil)                   ! node depth [m]
   real(r8) :: dz_soi(1:nl_soil)                   ! soil node thickness [m]
   real(r8) :: zi_soi(1:nl_soil)                   ! interface level below a zsoi level [m]

   real(r8), parameter :: spval    = -1.e36_r8     ! missing value
   integer , parameter :: spval_i4 = -9999         ! missing value
   real(r8), parameter :: PI       = 4*atan(1.)    ! pi value
   real(r8), parameter :: deg2rad  = 1.745329251994330e-2_r8 ! degree to radius

   integer , parameter :: irrig_start_time = 21600           ! local time of irrigation start
   real(r8), parameter :: irrig_max_depth  = 1._r8           ! max irrigation depth
   real(r8), parameter :: irrig_threshold_fraction = 1._r8   ! irrigation thershold
   real(r8), parameter :: irrig_supply_fraction = 1._r8      ! irrigation supply thershold
   real(r8), parameter :: irrig_min_cphase = 1._r8           ! crop phenology when begin irrigation
   real(r8), parameter :: irrig_max_cphase = 4._r8           ! crop phenology when end irrigation
   integer , parameter :: irrig_time_per_day = 14400         ! irrigation last time
   integer , parameter :: irrig_method_drip = 1              ! irrigation method
   integer , parameter :: irrig_method_sprinkler = 2         ! irrigation method
   integer , parameter :: irrig_method_flood = 3             ! irrigation method
   integer , parameter :: irrig_method_paddy = 4             ! irrigation method
   real(r8), parameter :: pondmxc = 100.0                    ! ponding depth (mm)
! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: Init_GlobalVars

CONTAINS

   SUBROUTINE Init_GlobalVars

   IMPLICIT NONE

   integer :: nsl

      ! node depths of each soil layer
      DO nsl = 1, nl_soil
         z_soi(nsl) = 0.025*(exp(0.5*(nsl-0.5))-1.)
      ENDDO

      ! thickness between two soil layer interfaces
      dz_soi(1) = 0.5*(z_soi(1)+z_soi(2))            !=zi_soi(1)
      dz_soi(nl_soil) = z_soi(nl_soil)-z_soi(nl_soil-1)
      DO nsl = 2, nl_soil-1
         dz_soi(nsl) = 0.5*(z_soi(nsl+1)-z_soi(nsl-1))
      ENDDO

      ! interface depths of soil layers
      zi_soi(1) = dz_soi(1)
      DO nsl = 2, nl_soil
         zi_soi(nsl) = zi_soi(nsl-1) + dz_soi(nsl)
      ENDDO

      ! set urban class number
      IF (DEF_URBAN_type_scheme == 1) THEN
         N_URB = 3
      ELSE IF(DEF_URBAN_type_scheme == 2) THEN
         N_URB = 10
      ENDIF

      !ndecomp_pools_vr = ndecomp_pools * nl_soil

   END SUBROUTINE Init_GlobalVars

END MODULE MOD_Vars_Global
! ---------- EOP ------------
