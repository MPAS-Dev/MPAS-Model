#include <define.h>

MODULE MOD_VicParaReadin

!-----------------------------------------------------------------------
   USE MOD_Precision
   IMPLICIT NONE
   SAVE

   ! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: vicpara_readin
CONTAINS

   SUBROUTINE vicpara_readin ()
   ! ===========================================================
   ! ! DESCRIPTION:
   ! Read in vic parameter from data

   ! ===========================================================

   USE MOD_Precision
   USE MOD_Namelist
   USE MOD_MPAS_MPI
   USE MOD_LandPatch
   USE MOD_NetCDFSerial
   USE MOD_NetCDFBlock
   ! USE MOD_Mapping_Grid2Pset
   USE MOD_Vars_TimeInvariants
   USE MOD_Vars_TimeVariables
   ! USE MOD_Grid
   ! USE MOD_DataType
   USE MOD_SpatialMapping
   USE MOD_Vars_Global
   USE MOD_LandPatch
   USE MOD_RangeCheck
   USE MOD_Block


   IMPLICIT NONE
   character(len=256) :: file_vic_para

   type(grid_type)    :: grid_b_infilt
   type(grid_type)    :: grid_Ws
   type(grid_type)    :: grid_Ds
   type(grid_type)    :: grid_DsM

   type(block_data_real8_2d)    :: f_xy_b_infilt
   type(block_data_real8_2d)    :: f_xy_Ws
   type(block_data_real8_2d)    :: f_xy_Ds
   type(block_data_real8_2d)    :: f_xy_DsM

   type(spatial_mapping_type) :: mg2patch_b_infilt
   type(spatial_mapping_type) :: mg2patch_Ws
   type(spatial_mapping_type) :: mg2patch_Ds
   type(spatial_mapping_type) :: mg2patch_DsM

   real(r8) ,allocatable :: b_infilt_tmp (:)
   real(r8) ,allocatable :: Ws_tmp (:)
   real(r8) ,allocatable :: Ds_tmp (:)
   real(r8) ,allocatable :: DsM_tmp (:)
   ! Local variables
   real(r8), allocatable :: lat(:), lon(:)
   real(r8) :: missing_value
   integer  :: cft, npatch, ipft

      file_vic_para = trim(DEF_file_VIC_OPT)

      CALL ncio_read_bcast_serial (file_vic_para, 'lat', lat)
      CALL ncio_read_bcast_serial (file_vic_para, 'lon', lon)


      CALL grid_b_infilt%define_by_center (lat, lon)
      CALL grid_Ws%define_by_center (lat, lon)
      CALL grid_Ds%define_by_center (lat, lon)
      CALL grid_DsM%define_by_center (lat, lon)

      IF (.true.) THEN
         CALL allocate_block_data  (grid_b_infilt, f_xy_b_infilt)
         CALL allocate_block_data  (grid_Ws, f_xy_Ws)
         CALL allocate_block_data  (grid_Ds, f_xy_Ds)
         CALL allocate_block_data  (grid_DsM, f_xy_DsM)
      ENDIF

      IF (.true.) THEN
         CALL ncio_read_block (file_vic_para,'b', grid_b_infilt, f_xy_b_infilt)
         CALL ncio_read_block (file_vic_para,'Ws', grid_Ws, f_xy_Ws)
         CALL ncio_read_block (file_vic_para,'Ds', grid_Ds, f_xy_Ds)
         CALL ncio_read_block (file_vic_para,'DsM', grid_DsM, f_xy_DsM)
      ENDIF

      CALL mg2patch_b_infilt%build_arealweighted (grid_b_infilt, landpatch)
      CALL mg2patch_Ws%build_arealweighted (grid_Ws, landpatch)
      CALL mg2patch_Ds%build_arealweighted (grid_Ds, landpatch)
      CALL mg2patch_DsM%build_arealweighted (grid_DsM, landpatch)



      IF (allocated(lon)) deallocate(lon)
      IF (allocated(lat)) deallocate(lat)

      IF (.true.) THEN
         IF (numpatch > 0)  allocate (b_infilt_tmp (numpatch))
         IF (numpatch > 0)  allocate (Ws_tmp (numpatch))
         IF (numpatch > 0)  allocate (Ds_tmp (numpatch))
         IF (numpatch > 0)  allocate (DsM_tmp (numpatch))
      ENDIF

      CALL mg2patch_b_infilt%grid2pset (f_xy_b_infilt, b_infilt_tmp(:))
      CALL mg2patch_Ws%grid2pset (f_xy_Ws, Ws_tmp(:))
      CALL mg2patch_Ds%grid2pset (f_xy_Ds, Ds_tmp(:))
      CALL mg2patch_DsM%grid2pset (f_xy_DsM, DsM_tmp(:))

      IF (.true.) THEN
         vic_b_infilt(:) = -9999
         vic_Dsmax(:) = -9999
         vic_Ds(:) = -9999
         vic_Ws(:) = -9999
         vic_c = 2
      ENDIF

      IF (.true.) THEN
         DO ipft = 1, numpatch
            !WRITE(*,*) 'Values of vic_b_infilt: ', DsM_tmp(ipft)
            vic_b_infilt(ipft) = b_infilt_tmp(ipft)
            vic_Ws(ipft) = Ws_tmp(ipft)
            vic_Ds(ipft) = Ds_tmp(ipft)
            vic_Dsmax(ipft) = DsM_tmp(ipft)
            vic_c(ipft)=2
         ENDDO
         ! 输出 vic_b_infilt 数组的值
         !WRITE(*,*) 'Values of vic_b_infilt: ', vic_b_infilt
      ENDIF

#ifdef RangeCheck
      CALL check_vector_data ('vic_b_infilt', vic_b_infilt)
      CALL check_vector_data ('vic_Ws', vic_Ws)
      CALL check_vector_data ('vic_Ds', vic_Ds)
      CALL check_vector_data ('vic_Dsmax', vic_Dsmax)
#endif
      IF (allocated (b_infilt_tmp)) deallocate (b_infilt_tmp)
      IF (allocated (Ws_tmp))       deallocate (Ws_tmp)
      IF (allocated (Ds_tmp))       deallocate (Ds_tmp)
      IF (allocated (DsM_tmp))      deallocate (DsM_tmp)

   END SUBROUTINE vicpara_readin
END MODULE MOD_VicParaReadin
