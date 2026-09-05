#include <define.h>

#ifdef BGC
MODULE MOD_FireData
!-----------------------------------------------------------------------
! !DESCRIPTION:
!  This module read in fire data.
!
!  Original:
!  2023, Lu Xingjie and Zhang Shupeng: prepare the original version of
!        the fire data module.
!-----------------------------------------------------------------------

   USE MOD_Grid
   USE MOD_SpatialMapping
   USE MOD_Vars_TimeInvariants, only: abm_lf, gdp_lf, peatf_lf
   USE MOD_Vars_TimeVariables,  only: hdm_lf
   IMPLICIT NONE

   character(len=256) :: file_fire

   type(grid_type) :: grid_fire
   type(spatial_mapping_type) :: mg2p_fire

CONTAINS

   SUBROUTINE init_fire_data (YY)

!-----------------------------------------------------------------------
! !DESCRIPTION:
!  open fire netcdf file from DEF_dir_runtime, read latitude and
!  longitude info.  Initialize fire data read in.
!-----------------------------------------------------------------------

   USE MOD_MPAS_MPI
   USE MOD_Namelist
   USE MOD_Grid
   USE MOD_NetCDFSerial
   USE MOD_NetCDFBlock
   USE MOD_LandPatch
   USE MOD_RangeCheck
   IMPLICIT NONE

      integer, intent(in) :: YY

      ! Local Variables
      real(r8), allocatable :: lat(:), lon(:)
      type(block_data_real8_2d) :: f_xy_fire

      file_fire = trim(DEF_dir_runtime) // '/fire/abm_colm_double_fillcoast.nc'

      CALL ncio_read_bcast_serial (file_fire, 'lat', lat)
      CALL ncio_read_bcast_serial (file_fire, 'lon', lon)

      CALL grid_fire%define_by_center (lat, lon)

      CALL mg2p_fire%build_arealweighted (grid_fire, landpatch)

      IF (allocated(lon)) deallocate(lon)
      IF (allocated(lat)) deallocate(lat)

      IF (.true.) THEN
         CALL allocate_block_data (grid_fire, f_xy_fire)
      ENDIF

      file_fire = trim(DEF_dir_runtime) // '/fire/abm_colm_double_fillcoast.nc'
      IF (.true.) THEN
         CALL ncio_read_block (file_fire, 'abm', grid_fire, f_xy_fire)
      ENDIF
      CALL mg2p_fire%grid2pset (f_xy_fire, abm_lf)
#ifdef RangeCheck
      CALL check_vector_data ('abm', abm_lf)
#endif

      file_fire = trim(DEF_dir_runtime) // '/fire/peatf_colm_360x720_c100428.nc'
      IF (.true.) THEN
         CALL ncio_read_block (file_fire, 'peatf', grid_fire, f_xy_fire)
      ENDIF
      CALL mg2p_fire%grid2pset (f_xy_fire, peatf_lf)
#ifdef RangeCheck
      CALL check_vector_data ('peatf', peatf_lf)
#endif

      file_fire = trim(DEF_dir_runtime) // '/fire/gdp_colm_360x720_c100428.nc'
      IF (.true.) THEN
         CALL ncio_read_block (file_fire, 'gdp', grid_fire, f_xy_fire)
      ENDIF
      CALL mg2p_fire%grid2pset (f_xy_fire, gdp_lf)
#ifdef RangeCheck
      CALL check_vector_data ('gdp', gdp_lf)
#endif

      CALL update_hdm_data (YY)

   END SUBROUTINE init_fire_data

   SUBROUTINE update_hdm_data (YY)
!-----------------------------------------------------------------------
!
! !DESCRIPTION:
!  Read in the Fire data from CLM5 dataset (month when crop fire peak
!  (abm), GDP, peatland fraction (peatf), and population density
!
!  Original: Xingjie Lu and Shupeng Zhang, 2022
!-----------------------------------------------------------------------

   USE MOD_MPAS_MPI
   USE MOD_DataType
   USE MOD_Namelist
   USE MOD_NetCDFBlock
   USE MOD_RangeCheck
   IMPLICIT NONE

   integer, intent(in) :: YY

   ! Local Variables
   type(block_data_real8_2d) :: f_xy_fire
   integer :: itime

      itime = max(1850,min(YY,2016)) - 1849

      file_fire = trim(DEF_dir_runtime) &
         // '/fire/colmforc.Li_2017_HYDEv3.2_CMIP6_hdm_0.5x0.5_AVHRR_simyr1850-2016_c180202.nc'

      IF (.true.) THEN
         CALL allocate_block_data  (grid_fire, f_xy_fire)
         CALL ncio_read_block_time (file_fire, 'hdm', grid_fire, itime, f_xy_fire)
      ENDIF

      CALL mg2p_fire%grid2pset (f_xy_fire, hdm_lf)

#ifdef RangeCheck
      CALL check_vector_data ('hdm', hdm_lf)
#endif

   END SUBROUTINE update_hdm_data

END MODULE MOD_FireData
#endif
