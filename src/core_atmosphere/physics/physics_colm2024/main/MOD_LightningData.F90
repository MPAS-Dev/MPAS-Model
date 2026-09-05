#include <define.h>

#ifdef BGC
MODULE MOD_LightningData
!-----------------------------------------------------------------------
! !DESCRIPTION:
!  This module read in lightning data for fire subroutine
!
! !ORIGINAL:
!  Zhang Shupeng, 2022, prepare the original version of the lightning data module.
!-----------------------------------------------------------------------

   USE MOD_Grid
   USE MOD_DataType
   USE MOD_SpatialMapping
   USE MOD_BGC_Vars_TimeVariables, only: lnfm
   IMPLICIT NONE

   character(len=256) :: file_lightning
   type(grid_type) :: grid_lightning

   type(block_data_real8_2d) :: f_lnfm

   type (spatial_mapping_type) :: mg2p_lnfm

CONTAINS

   SUBROUTINE init_lightning_data (idate)

!-----------------------------------------------------------------------
! !DESCTIPTION:
!  open lightning netcdf file from DEF_dir_rawdata, read latitude and longitude info.
!  Initialize lightning data read in.
!-----------------------------------------------------------------------

   USE MOD_MPAS_MPI
   USE MOD_Namelist
   USE MOD_TimeManager
   USE MOD_Grid
   USE MOD_NetCDFSerial
   USE MOD_NetCDFBlock
   USE MOD_LandPatch
   USE MOD_RangeCheck
   IMPLICIT NONE

   integer, intent(in) :: idate(3)

   ! Local Variables
   real(r8), allocatable :: lat(:), lon(:)
   integer :: itime

      file_lightning = trim(DEF_dir_runtime) // &
         '/fire/clmforc.Li_2012_climo1995-2011.T62.lnfm_Total_c140423.nc'

      CALL ncio_read_bcast_serial (file_lightning, 'lat', lat)
      CALL ncio_read_bcast_serial (file_lightning, 'lon', lon)

      CALL grid_lightning%define_by_center (lat, lon)

      CALL allocate_block_data (grid_lightning, f_lnfm)

      CALL mg2p_lnfm%build_arealweighted (grid_lightning, landpatch)

      itime = (idate(2)-1)*8 + min(idate(3)/10800+1,8)
      IF (itime .gt. 2920)itime = itime - 8 ! for the leap year

      CALL ncio_read_block_time (file_lightning, 'lnfm', grid_lightning, itime, f_lnfm)
#ifdef RangeCheck
      CALL check_block_data ('lightning', f_lnfm)
#endif

   END SUBROUTINE init_lightning_data


   SUBROUTINE update_lightning_data (time, deltim)

!-----------------------------------------------------------------------
! !DESCRIPTION:
!  read lightning data during simulation
!-----------------------------------------------------------------------

   USE MOD_TimeManager
   USE MOD_NetCDFBlock
   USE MOD_RangeCheck
   IMPLICIT NONE

   type(timestamp), intent(in) :: time
   real(r8), intent(in) :: deltim

   ! Local Variables
   type(timestamp) :: time_next
   integer :: itime, itime_next

      itime = (time%day-1)*8 + min(time%sec/10800+1,8)
      IF (mod(time%sec,10800) == 0) itime = itime - 1

      time_next = time + int(deltim)
      itime_next = (time_next%day-1)*8 + max(0,time_next%sec-1)/10800+1

      IF (itime_next /= itime) THEN
         itime_next = min(itime_next,2920)
         CALL ncio_read_block_time (file_lightning, 'lnfm', grid_lightning, itime_next, f_lnfm)
#ifdef RangeCheck
         CALL check_block_data ('lightning', f_lnfm)
#endif

         CALL mg2p_lnfm%grid2pset (f_lnfm, lnfm)
#ifdef RangeCheck
         CALL check_vector_data ('lightning', lnfm)
#endif
      ENDIF

   END SUBROUTINE update_lightning_data

END MODULE MOD_LightningData
#endif
