#include <define.h>

#ifdef BGC
MODULE MOD_NitrifData
!-----------------------------------------------------------------------
! !DESCRIPTION:
!  This module read in nitrif data.
!
! !ORIGINAL:
!  Lu Xingjie and Zhang Shupeng, 2023, prepare the original version of the nitrif data module.
!-----------------------------------------------------------------------

   USE MOD_Grid
   USE MOD_SpatialMapping
   USE MOD_BGC_Vars_TimeVariables, only: tCONC_O2_UNSAT, tO2_DECOMP_DEPTH_UNSAT
   IMPLICIT NONE

   type(grid_type) :: grid_nitrif
   type(spatial_mapping_type) :: mg2p_nitrif

CONTAINS

   SUBROUTINE init_nitrif_data (time)

!-----------------------------------------------------------------------
! !DESCRIPTION:
!  open nitrif netcdf file from DEF_dir_runtime, read latitude and
!  longitude info.  Initialize nitrif data read in.
!-----------------------------------------------------------------------

   USE MOD_TimeManager
   USE MOD_Namelist
   USE MOD_Grid
   USE MOD_NetCDFSerial
   USE MOD_LandPatch
   IMPLICIT NONE

   type(timestamp), intent(in) :: time

   ! Local Variables
   character(len=256) :: file_nitrif
   real(r8), allocatable :: lat(:), lon(:)
   integer :: month, mday

      file_nitrif = trim(DEF_dir_runtime)//'/nitrif/CONC_O2_UNSAT/CONC_O2_UNSAT_l01.nc'

      CALL ncio_read_bcast_serial (file_nitrif, 'lat', lat)
      CALL ncio_read_bcast_serial (file_nitrif, 'lon', lon)

      CALL grid_nitrif%define_by_center (lat, lon)

      CALL mg2p_nitrif%build_arealweighted (grid_nitrif, landpatch)

      IF (allocated(lon)) deallocate(lon)
      IF (allocated(lat)) deallocate(lat)

      CALL julian2monthday (time%year, time%day, month, mday)

      CALL update_nitrif_data (month)

   END SUBROUTINE init_nitrif_data

   ! ----------
   SUBROUTINE update_nitrif_data (month)

   USE MOD_MPAS_MPI
   USE MOD_Namelist
   USE MOD_DataType
   USE MOD_Vars_Global, only: nl_soil
   USE MOD_NetCDFBlock
   USE MOD_LandPatch
   USE MOD_Vars_TimeInvariants
   USE MOD_RangeCheck
   IMPLICIT NONE

   integer,  intent(in) :: month

   ! Local Variables
   character(len=256) :: file_nitrif
   type(block_data_real8_2d) :: f_xy_nitrif
   real(r8), allocatable :: tCONC_O2_UNSAT_tmp(:)
   real(r8), allocatable :: tO2_DECOMP_DEPTH_UNSAT_tmp(:)
   character(len=2) :: cx
   integer :: nsl, npatch, m

      IF (.true.) THEN
         allocate(tCONC_O2_UNSAT_tmp        (numpatch))
         allocate(tO2_DECOMP_DEPTH_UNSAT_tmp(numpatch))
      ENDIF

      IF (.true.) THEN
         CALL allocate_block_data (grid_nitrif, f_xy_nitrif)
      ENDIF

      DO nsl = 1, nl_soil

         write(cx,'(i2.2)') nsl
         file_nitrif = trim(DEF_dir_runtime)//&
            '/nitrif/CONC_O2_UNSAT/CONC_O2_UNSAT_l'//trim(cx)//'.nc'
         IF (.true.) THEN
            CALL ncio_read_block_time (file_nitrif, &
               'CONC_O2_UNSAT', grid_nitrif, month, f_xy_nitrif)
         ENDIF

         CALL mg2p_nitrif%grid2pset (f_xy_nitrif, tCONC_O2_UNSAT_tmp)

         IF (.true.) THEN
            IF (numpatch > 0) THEN
               DO npatch = 1, numpatch
                  m = patchclass(npatch)
                  IF( m == 0 )THEN
                     tCONC_O2_UNSAT(nsl,npatch)  = 0.
                  ELSE
                     tCONC_O2_UNSAT(nsl,npatch)  = tCONC_O2_UNSAT_tmp(npatch)
                  ENDIF
                  IF (tCONC_O2_UNSAT(nsl,npatch) < 1E-10) THEN
                     tCONC_O2_UNSAT(nsl,npatch)=0.0
                  ENDIF
               ENDDO

            ENDIF
         ENDIF
      ENDDO

#ifdef RangeCheck
      CALL check_vector_data ('CONC_O2_UNSAT', tCONC_O2_UNSAT)
#endif

      DO nsl = 1, nl_soil

         write(cx,'(i2.2)') nsl
         file_nitrif = trim(DEF_dir_runtime)//&
            '/nitrif/O2_DECOMP_DEPTH_UNSAT/O2_DECOMP_DEPTH_UNSAT_l'//trim(cx)//'.nc'
         IF (.true.) THEN
            CALL ncio_read_block_time (file_nitrif, &
               'O2_DECOMP_DEPTH_UNSAT', grid_nitrif, month, f_xy_nitrif)
         ENDIF

         CALL mg2p_nitrif%grid2pset (f_xy_nitrif, tO2_DECOMP_DEPTH_UNSAT_tmp)

         IF (.true.) THEN
            IF (numpatch > 0) THEN
               DO npatch = 1, numpatch
                  m = patchclass(npatch)
                  IF( m == 0 )THEN
                     tO2_DECOMP_DEPTH_UNSAT(nsl,npatch)  = 0.
                  ELSE
                     tO2_DECOMP_DEPTH_UNSAT(nsl,npatch)  = tO2_DECOMP_DEPTH_UNSAT_tmp(npatch)
                  ENDIF
                  IF (tO2_DECOMP_DEPTH_UNSAT(nsl,npatch) < 1E-10) THEN
                     tO2_DECOMP_DEPTH_UNSAT(nsl,npatch)=0.0
                  ENDIF
               ENDDO

            ENDIF
         ENDIF
      ENDDO

#ifdef RangeCheck
      CALL check_vector_data ('O2_DECOMP_DEPTH_UNSAT', tO2_DECOMP_DEPTH_UNSAT)
#endif

      IF (.true.) THEN
         deallocate (tCONC_O2_UNSAT_tmp)
         deallocate (tO2_DECOMP_DEPTH_UNSAT_tmp)
      ENDIF

   END SUBROUTINE update_nitrif_data

END MODULE MOD_NitrifData
#endif
