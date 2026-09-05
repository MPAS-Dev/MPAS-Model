#include <define.h>

Module MOD_Ozone

!-----------------------------------------------------------------------
! !DESCRIPTION:
!  This module hold the plant physiological response to the ozone,
!  including vcmax response and stomata response.  Ozone concentration
!  can be either readin through Mod_OzoneData module or set to constant.
!
!  Original:
!  The Community Land Model version 5.0 (CLM5.0)
!
! !REVISIONS:
!  2022, Xingjie Lu: revised the CLM5 code to be compatible with CoLM
!        code structure.
!  2024, Fang Li : used the new ozone stress parameterization scheme
!        based on Li et al. (2024; GMD)
!-----------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_Const_Physical, only: rgas
   USE MOD_Const_PFT, only: isevg, leaf_long, woody
   USE MOD_Grid
   USE MOD_DataType
   USE MOD_SpatialMapping
   USE MOD_Vars_1DForcing, only: forc_ozone
   USE MOD_Namelist, only: DEF_USE_OZONEDATA
   IMPLICIT NONE

   character(len=256) :: file_ozone

   type(grid_type) :: grid_ozone

   type(block_data_real8_2d) :: f_ozone

   type(spatial_mapping_type) :: mg2p_ozone

   SAVE

   PUBLIC :: CalcOzoneStress
   PUBLIC :: init_ozone_data
   PUBLIC :: update_ozone_data

CONTAINS

   SUBROUTINE CalcOzoneStress (o3coefv,o3coefg, forc_ozone, forc_psrf, th, ram, &
                              rs, rb, lai, lai_old, ivt, o3uptake, sabv, deltim)
!-----------------------------------------------------------------------
! !DESCRIPTION:
!  Calculate Ozone Stress on both vcmax and stomata conductance.
!
!  convert o3 from mol/mol to nmol m^-3
!-----------------------------------------------------------------------
   real(r8), intent(out)   :: o3coefv
   real(r8), intent(out)   :: o3coefg
   real(r8), intent(inout) :: forc_ozone !ozone concentration (ppbv)
   real(r8), intent(in)    :: forc_psrf
   real(r8), intent(in)    :: th
   real(r8), intent(in)    :: ram
   real(r8), intent(in)    :: rs
   real(r8), intent(in)    :: rb
   real(r8), intent(in)    :: lai
   real(r8), intent(in)    :: lai_old
   integer , intent(in)    :: ivt
   real(r8), intent(inout) :: o3uptake
   real(r8), intent(in)    :: deltim
   real(r8), intent(in)   :: sabv  !solar radiation absorbed by vegetation (W/m**2)

   real(r8) :: o3concnmolm3   ! o3 concentration (nmol/m^3)
   real(r8) :: o3flux         ! instantaneous o3 flux (nmol m^-2 s^-1)
   real(r8) :: o3fluxcrit     ! instantaneous o3 flux beyond threshold (nmol m^-2 s^-1)
   real(r8) :: o3fluxperdt    ! o3 flux per timestep (mmol m^-2)
   real(r8) :: leafturn       ! leaf turnover time / mortality rate (per hour)
   real(r8) :: decay          ! o3uptake decay rate based on leaf lifetime (mmol m^-2)
   real(r8) :: lai_thresh     ! LAI threshold for LAIs that asymptote and don't
   real(r8) :: o3_flux_threshold !threshold below which o3flux is set to 0 (nmol m^-2 s^-1)

   real(r8), parameter :: ko3 = 1.51_r8  !F. Li


      IF(.not. DEF_USE_OZONEDATA)THEN
         forc_ozone = 100._r8  ! ozone partial pressure [ppbv]
      ENDIF

      o3concnmolm3 = forc_ozone * (forc_psrf/(th * 8.314 ))

      ! calculate instantaneous flux
      o3flux = o3concnmolm3/ (ko3*rs+ rb + ram)

      ! set lai_thresh
       IF (isevg(ivt)) THEN
         lai_thresh=0._r8 !so evergreens grow year-round
       ELSE  ! for deciduous vegetation
        IF(ivt == 10)THEN !temperate shrub
         lai_thresh=0.3_r8
        ELSE
         lai_thresh=0.5_r8
        end if
       end if


      ! set o3 flux threshold
      o3_flux_threshold=10._r8
      IF(ivt >= 1 .and. ivt <= 3)THEN  !Needleleaf tree
        o3_flux_threshold=0.8_r8
      ENDIF
      IF(ivt >= 4 .and. ivt <= 8)THEN  !Broadleaf tree
        o3_flux_threshold=1.0_r8
      ENDIF
      IF(ivt >= 9 .and. ivt <= 11)THEN !Shrub
        o3_flux_threshold=6.0_r8
      ENDIF
      IF(ivt >= 12 .and. ivt <= 14)THEN !Grass
        o3_flux_threshold = 1.6_r8
      ENDIF
      IF(ivt >= 15)THEN !Crop
        o3_flux_threshold = 0.5_r8
      ENDIF


      IF (o3flux < o3_flux_threshold) THEN
         o3fluxcrit = 0._r8
      ELSE
         o3fluxcrit = o3flux - o3_flux_threshold
      ENDIF

      ! calculate o3 flux per timestep
      IF(sabv > 0._r8)THEN  !daytime
        o3fluxperdt = o3fluxcrit * deltim * 0.000001_r8
      ELSE
        o3fluxperdt = 0._r8
      ENDIF

      IF (lai > lai_thresh) THEN
       ! o3 uptake decay
         IF (isevg(ivt)) THEN
            leafturn = 2._r8/(leaf_long(ivt)*365._r8*24._r8)
            decay = o3uptake * leafturn * deltim/3600._r8
         ELSE
            decay = o3uptake * max(0._r8,(1._r8-lai_old/lai))
         ENDIF

         !cumulative uptake (mmol m^-2)
         o3uptake = min(90._r8, max(0._r8, o3uptake + o3fluxperdt - decay))

      ELSE
         o3uptake = 0._r8
      ENDIF

      IF (o3uptake == 0._r8) THEN
         ! No o3 damage IF no o3 uptake
         o3coefv = 1._r8
         o3coefg = 1._r8
      ELSE
         ! Determine parameter values for this pft
        IF(ivt >= 1 .and. ivt <= 3)THEN  !Needleleaf tree
          o3coefv = max(0._r8, min(1._r8, 1.005_r8 - 0.0064_r8 * o3uptake))
          o3coefg = max(0._r8, min(1._r8, 0.965_r8 * o3uptake ** (-0.041)))
        ENDIF
        IF(ivt >= 4 .and. ivt <= 8)THEN  !Broadleaf tree
          o3coefv = max(0._r8, min(1._r8, 0.943_r8 * exp(-0.0085*o3uptake)))
          o3coefg = max(0._r8, min(1._r8, 0.943_r8 * exp(-0.0058*o3uptake)))
        ENDIF
        IF(ivt >= 9 .and. ivt <= 11)THEN !Shrub
          o3coefv = max(0._r8, min(1._r8, 1.000_r8-0.074_r8 * log(o3uptake)))
          o3coefg = max(0._r8, min(1._r8, 0.991_r8-0.060_r8 * log(o3uptake)))
        ENDIF
        IF(ivt >= 12 .and. ivt <= 14)THEN !Grass
          o3coefv = max(0._r8, min(1._r8, 0.997_r8 - 0.016_r8 * o3uptake))
          o3coefg = max(0._r8, min(1._r8, 0.989_r8 - 0.045_r8 * log(o3uptake)))
        ENDIF
        IF(ivt >= 15)THEN !Crop
          o3coefv = max(0._r8, min(1._r8, 0.909_r8 - 0.028_r8 * log(o3uptake)))
          o3coefg = max(0._r8, min(1._r8, 1.005_r8 - 0.169_r8 * tanh(o3uptake)))
        ENDIF
     ENDIF

   END SUBROUTINE CalcOzoneStress


   SUBROUTINE init_ozone_data (idate)

!-----------------------------------------------------------------------
! !DESCRIPTION:
!  open ozone netcdf file from DEF_dir_rawdata, read latitude and
!  longitude info.  Initialize Ozone data read in.
!-----------------------------------------------------------------------

   USE MOD_MPAS_MPI
   USE MOD_Namelist
   USE MOD_TimeManager
   USE MOD_Grid
   USE MOD_NetCDFSerial
   USE MOD_NetCDFBlock
   USE MOD_LandPatch
#ifdef RangeCheck
   USE MOD_RangeCheck
#endif
   IMPLICIT NONE

   integer, intent(in) :: idate(3)

   ! Local Variables
   real(r8), allocatable :: lat(:), lon(:)
   integer :: itime
   integer :: iyear, month, mday
   character(len=8) :: syear, smonth

!     CALL julian2monthday(idate(1),idate(2),month,mday)
!     iyear = idate(1)
!     IF(idate(1) .lt. 2013)iyear = 2013
!     IF(idate(1) .gt. 2021)iyear = 2021
!     write(syear,"(I4.4)")  iyear
!     write(smonth,"(I2.2)") month
      file_ozone = trim(DEF_dir_runtime) // '/Ozone//Global/OZONE-setgrid.nc'
!      file_ozone = '/share/home/dq010/CoLM/data/rawdata/CROP-NITRIF/CoLMruntime/Ozone//Global/OZONE-setgrid.nc'

      CALL ncio_read_bcast_serial (file_ozone, 'lat', lat)
      CALL ncio_read_bcast_serial (file_ozone, 'lon', lon)

      CALL grid_ozone%define_by_center (lat, lon)

      CALL allocate_block_data (grid_ozone, f_ozone)

      CALL mg2p_ozone%build_arealweighted (grid_ozone, landpatch)

      itime = (idate(3) - 1800) / 10800 + (min(idate(2),365) - 1) * 8 + 1

      CALL ncio_read_block_time (file_ozone, 'OZONE', grid_ozone, itime, f_ozone)
#ifdef RangeCheck
      CALL check_block_data ('Ozone', f_ozone)
#endif

   END SUBROUTINE init_ozone_data

   SUBROUTINE update_ozone_data (time, deltim)

!-----------------------------------------------------------------------
! !DESCRIPTION:
!  read ozone data during simulation
!-----------------------------------------------------------------------

   USE MOD_TimeManager
   USE MOD_Namelist
   USE MOD_NetCDFBlock
#ifdef RangeCheck
   USE MOD_RangeCheck
#endif
   IMPLICIT NONE

   type(timestamp), intent(in) :: time
   real(r8), intent(in) :: deltim

   ! Local Variables
   type(timestamp) :: time_next
   integer :: month, mday
   integer :: iyear, itime
   character(len=8) :: syear, smonth

      file_ozone = trim(DEF_dir_runtime) // '/Ozone/Global/OZONE-setgrid.nc'
!      file_ozone = '/share/home/dq010/CoLM/data/rawdata/CROP-NITRIF/CoLMruntime/Ozone/Global/OZONE-setgrid.nc'
      IF(time%sec/10800 .ne. (time%sec+int(deltim))/10800)then
         itime = (time%sec - int(deltim)) / 10800 + (min(time%day,365) - 1) * 8 + 1
         CALL ncio_read_block_time (file_ozone, 'OZONE', grid_ozone, itime, f_ozone)
#ifdef RangeCheck
         CALL check_block_data ('Ozone', f_ozone)
#endif

         CALL mg2p_ozone%grid2pset (f_ozone, forc_ozone)
#ifdef RangeCheck
         CALL check_vector_data ('Ozone', forc_ozone)
#endif
      ENDIF

   END SUBROUTINE update_ozone_data

END MODULE MOD_Ozone
! ---------- EOP ------------
