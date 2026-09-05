#include <define.h>

MODULE MOD_DataType

!-----------------------------------------------------------------------
! !DESCRIPTION:
!
!    Definitions of data types used in CoLM.
!
!    Most frequently used data types in CoLM are "blocked" data types
!    including,
!    1. Blocked 2D data of 4-byte integer type;
!    2. Blocked 2D data of 8-byte float type;
!    3. Blocked 3D data of 8-byte float type;
!    4. Blocked 4D data of 8-byte float type;
!
!    Subroutines are used to
!    1. allocate memory;
!    2. flush data values;
!    3. copy data;
!    4. do linear transformation and interpolations.
!
!  Created by Shupeng Zhang, May 2023
!-----------------------------------------------------------------------

   USE MOD_Precision

   ! ---- data types ----
   !-------
   type :: pointer_real8_1d
      real(r8), allocatable :: val(:)
   CONTAINS
      final :: pointer_real8_1d_free_mem
   END type pointer_real8_1d

   !-------
   type :: pointer_int8_1d
      integer(1), allocatable :: val(:)
   CONTAINS
      final :: pointer_int8_1d_free_mem
   END type pointer_int8_1d

   !-------
   type :: pointer_int32_1d
      integer, allocatable :: val(:)
   CONTAINS
      final :: pointer_int32_1d_free_mem
   END type pointer_int32_1d

   !-------
   type :: pointer_int64_1d
      integer*8, allocatable :: val(:)
   CONTAINS
      final :: pointer_int64_1d_free_mem
   END type pointer_int64_1d

   !-------
   type :: pointer_logic_1d
      logical, allocatable :: val(:)
   CONTAINS
      final :: pointer_logic_1d_free_mem
   END type pointer_logic_1d

   !-------
   type :: pointer_int32_2d
      integer, allocatable :: val (:,:)
   CONTAINS
      final :: pointer_int32_2d_free_mem
   END type pointer_int32_2d

   type :: block_data_int32_2d
      type(pointer_int32_2d), allocatable :: blk (:,:)
   CONTAINS
      final :: block_data_int32_2d_free_mem
   END type block_data_int32_2d

   !-------
   type :: pointer_real8_2d
      real(r8), allocatable :: val (:,:)
   CONTAINS
      final :: pointer_real8_2d_free_mem
   END type pointer_real8_2d

   type :: block_data_real8_2d
      type(pointer_real8_2d), allocatable :: blk (:,:)
   CONTAINS
      final :: block_data_real8_2d_free_mem
   END type block_data_real8_2d

   !-------
   type :: pointer_real8_3d
      real(r8), allocatable :: val (:,:,:)
   CONTAINS
      final :: pointer_real8_3d_free_mem
   END type pointer_real8_3d

   type :: block_data_real8_3d
      integer :: lb1, ub1
      type(pointer_real8_3d), allocatable :: blk (:,:)
   CONTAINS
      final :: block_data_real8_3d_free_mem
   END type block_data_real8_3d

   !-------
   type :: pointer_real8_4d
      real(r8), allocatable :: val (:,:,:,:)
   CONTAINS
      final :: pointer_real8_4d_free_mem
   END type pointer_real8_4d

   type :: block_data_real8_4d
      integer :: lb1, ub1, lb2, ub2
      type(pointer_real8_4d), allocatable :: blk (:,:)
   CONTAINS
      final :: block_data_real8_4d_free_mem
   END type block_data_real8_4d

   ! ---- PUBLIC subroutines ----
   !------
   INTERFACE allocate_block_data
      MODULE procedure allocate_block_data_int32_2d
      MODULE procedure allocate_block_data_real8_2d
      MODULE procedure allocate_block_data_real8_3d
      MODULE procedure allocate_block_data_real8_4d
   END INTERFACE allocate_block_data

   !------
   INTERFACE flush_block_data
      MODULE procedure flush_block_data_int32_2d
      MODULE procedure flush_block_data_real8_2d
      MODULE procedure flush_block_data_real8_3d
      MODULE procedure flush_block_data_real8_4d
   END INTERFACE flush_block_data

   !-----
   PUBLIC :: block_data_linear_transform
   PUBLIC :: block_data_copy
   PUBLIC :: block_data_linear_interp
   PUBLIC :: block_data_division

CONTAINS

   !------------------
   SUBROUTINE pointer_real8_1d_free_mem (this)

   IMPLICIT NONE

   type(pointer_real8_1d) :: this

      IF (allocated(this%val)) THEN
         deallocate(this%val)
      ENDIF

   END SUBROUTINE pointer_real8_1d_free_mem

   !------------------
   SUBROUTINE pointer_int8_1d_free_mem (this)

   IMPLICIT NONE

   type(pointer_int8_1d) :: this

      IF (allocated(this%val)) THEN
         deallocate(this%val)
      ENDIF

   END SUBROUTINE pointer_int8_1d_free_mem

   !------------------
   SUBROUTINE pointer_int32_1d_free_mem (this)

   IMPLICIT NONE

   type(pointer_int32_1d) :: this

      IF (allocated(this%val)) THEN
         deallocate(this%val)
      ENDIF

   END SUBROUTINE pointer_int32_1d_free_mem

   !------------------
   SUBROUTINE pointer_int64_1d_free_mem (this)

   IMPLICIT NONE

   type(pointer_int64_1d) :: this

      IF (allocated(this%val)) THEN
         deallocate(this%val)
      ENDIF

   END SUBROUTINE pointer_int64_1d_free_mem

   !------------------
   SUBROUTINE pointer_logic_1d_free_mem (this)

   IMPLICIT NONE

   type(pointer_logic_1d) :: this

      IF (allocated(this%val)) THEN
         deallocate(this%val)
      ENDIF

   END SUBROUTINE pointer_logic_1d_free_mem

   !------------------
   SUBROUTINE pointer_int32_2d_free_mem (this)

   IMPLICIT NONE

   type(pointer_int32_2d) :: this

      IF (allocated(this%val)) THEN
         deallocate(this%val)
      ENDIF

   END SUBROUTINE pointer_int32_2d_free_mem

   !------------------
   SUBROUTINE allocate_block_data_int32_2d (grid, gdata)

   USE MOD_Grid
   USE MOD_Block
   USE MOD_MPAS_MPI
   IMPLICIT NONE

   type(grid_type), intent(in) :: grid
   type(block_data_int32_2d), intent(out) :: gdata

      ! Local variables
      integer :: iblkme, iblk, jblk

      allocate (gdata%blk (gblock%nxblk,gblock%nyblk))

      DO iblkme = 1, gblock%nblkme
         iblk = gblock%xblkme(iblkme)
         jblk = gblock%yblkme(iblkme)
         allocate (gdata%blk(iblk,jblk)%val (grid%xcnt(iblk), grid%ycnt(jblk)))
      ENDDO

   END SUBROUTINE allocate_block_data_int32_2d

   !------------------
   SUBROUTINE block_data_int32_2d_free_mem (this)

   USE MOD_Block
   IMPLICIT NONE

   type(block_data_int32_2d) :: this

   ! Local variables
   integer :: iblk, jblk

      IF (allocated (this%blk)) THEN
         DO jblk = 1, gblock%nyblk
            DO iblk = 1, gblock%nxblk
               IF (allocated (this%blk(iblk,jblk)%val)) THEN
                  deallocate (this%blk(iblk,jblk)%val)
               ENDIF
            ENDDO
         ENDDO

         deallocate (this%blk)
      ENDIF

   END SUBROUTINE block_data_int32_2d_free_mem

   !------------------
   SUBROUTINE pointer_real8_2d_free_mem (this)

   IMPLICIT NONE

   type(pointer_real8_2d) :: this

      IF (allocated(this%val)) THEN
         deallocate(this%val)
      ENDIF

   END SUBROUTINE pointer_real8_2d_free_mem

   !------------------
   SUBROUTINE allocate_block_data_real8_2d (grid, gdata)

   USE MOD_Grid
   USE MOD_Block
   USE MOD_MPAS_MPI
   IMPLICIT NONE

   type(grid_type), intent(in) :: grid
   type(block_data_real8_2d), intent(out) :: gdata

   ! Local variables
   integer :: iblkme, iblk, jblk

      allocate (gdata%blk (gblock%nxblk,gblock%nyblk))

      DO iblkme = 1, gblock%nblkme
         iblk = gblock%xblkme(iblkme)
         jblk = gblock%yblkme(iblkme)
         allocate (gdata%blk(iblk,jblk)%val (grid%xcnt(iblk), grid%ycnt(jblk)))
      ENDDO

   END SUBROUTINE allocate_block_data_real8_2d

   !------------------
   SUBROUTINE block_data_real8_2d_free_mem (this)

   USE MOD_Block
   IMPLICIT NONE

   type(block_data_real8_2d) :: this

      ! Local variables
      integer :: iblk, jblk

      IF (allocated (this%blk)) THEN
         DO jblk = 1, gblock%nyblk
            DO iblk = 1, gblock%nxblk
               IF (allocated (this%blk(iblk,jblk)%val)) THEN
                  deallocate (this%blk(iblk,jblk)%val)
               ENDIF
            ENDDO
         ENDDO

         deallocate (this%blk)
      ENDIF

   END SUBROUTINE block_data_real8_2d_free_mem

   !------------------
   SUBROUTINE pointer_real8_3d_free_mem (this)

   IMPLICIT NONE

   type(pointer_real8_3d) :: this

      IF (allocated(this%val)) THEN
         deallocate(this%val)
      ENDIF

   END SUBROUTINE pointer_real8_3d_free_mem

   !------------------
   SUBROUTINE allocate_block_data_real8_3d (grid, gdata, ndim1, lb1)

   USE MOD_Grid
   USE MOD_Block
   USE MOD_MPAS_MPI
   IMPLICIT NONE

   type(grid_type), intent(in) :: grid
   type(block_data_real8_3d), intent(out) :: gdata
   integer, intent(in) :: ndim1
   integer, intent(in), optional :: lb1

   ! Local variables
   integer :: iblkme, iblk, jblk

      allocate (gdata%blk (gblock%nxblk,gblock%nyblk))

      IF (present(lb1)) THEN
         gdata%lb1 = lb1
      ELSE
         gdata%lb1 = 1
      ENDIF

      gdata%ub1 = gdata%lb1-1+ndim1

      DO iblkme = 1, gblock%nblkme
         iblk = gblock%xblkme(iblkme)
         jblk = gblock%yblkme(iblkme)
         allocate (gdata%blk(iblk,jblk)%val (gdata%lb1:gdata%ub1, grid%xcnt(iblk), grid%ycnt(jblk)))
      ENDDO

   END SUBROUTINE allocate_block_data_real8_3d

   !------------------
   SUBROUTINE block_data_real8_3d_free_mem (this)

   USE MOD_Block
   IMPLICIT NONE

   type(block_data_real8_3d) :: this

   ! Local variables
   integer :: iblk, jblk

      IF (allocated (this%blk)) THEN
         DO jblk = 1, gblock%nyblk
            DO iblk = 1, gblock%nxblk
               IF (allocated (this%blk(iblk,jblk)%val)) THEN
                  deallocate (this%blk(iblk,jblk)%val)
               ENDIF
            ENDDO
         ENDDO

         deallocate (this%blk)
      ENDIF

   END SUBROUTINE block_data_real8_3d_free_mem

   !------------------
   SUBROUTINE pointer_real8_4d_free_mem (this)

   IMPLICIT NONE

   type(pointer_real8_4d) :: this

      IF (allocated(this%val)) THEN
         deallocate(this%val)
      ENDIF

   END SUBROUTINE pointer_real8_4d_free_mem

   !------------------
   SUBROUTINE allocate_block_data_real8_4d (grid, gdata, ndim1, ndim2, lb1, lb2)

   USE MOD_Grid
   USE MOD_Block
   USE MOD_MPAS_MPI
   IMPLICIT NONE

   type(grid_type), intent(in) :: grid
   type(block_data_real8_4d), intent(out) :: gdata
   integer, intent(in) :: ndim1, ndim2
   integer, intent(in), optional :: lb1, lb2

   ! Local variables
   integer :: iblkme, iblk, jblk

      allocate (gdata%blk (gblock%nxblk,gblock%nyblk))

      IF (present(lb1)) THEN
         gdata%lb1 = lb1
      ELSE
         gdata%lb1 = 1
      ENDIF

      gdata%ub1 = gdata%lb1-1+ndim1

      IF (present(lb2)) THEN
         gdata%lb2 = lb2
      ELSE
         gdata%lb2 = 1
      ENDIF

      gdata%ub2 = gdata%lb2-1+ndim2

      DO iblkme = 1, gblock%nblkme
         iblk = gblock%xblkme(iblkme)
         jblk = gblock%yblkme(iblkme)
         allocate (gdata%blk(iblk,jblk)%val ( &
            gdata%lb1:gdata%ub1, gdata%lb2:gdata%ub2, grid%xcnt(iblk), grid%ycnt(jblk)))
      ENDDO

   END SUBROUTINE allocate_block_data_real8_4d

   !------------------
   SUBROUTINE block_data_real8_4d_free_mem (this)

   USE MOD_Block
   IMPLICIT NONE

   type(block_data_real8_4d) :: this

   ! Local variables
   integer :: iblk, jblk

      IF (allocated (this%blk)) THEN
         DO jblk = 1, gblock%nyblk
            DO iblk = 1, gblock%nxblk
               IF (allocated (this%blk(iblk,jblk)%val)) THEN
                  deallocate (this%blk(iblk,jblk)%val)
               ENDIF
            ENDDO
         ENDDO

         deallocate (this%blk)
      ENDIF

   END SUBROUTINE block_data_real8_4d_free_mem

   !------------------
   SUBROUTINE flush_block_data_real8_2d (gdata, spval)

   USE MOD_Precision
   USE MOD_Block
   USE MOD_MPAS_MPI
   IMPLICIT NONE

   type(block_data_real8_2d), intent(inout) :: gdata
   real(r8), intent(in) :: spval

   ! Local variables
   integer :: iblkme, iblk, jblk

      DO iblkme = 1, gblock%nblkme
         iblk = gblock%xblkme(iblkme)
         jblk = gblock%yblkme(iblkme)
         gdata%blk(iblk,jblk)%val = spval
      ENDDO

   END SUBROUTINE flush_block_data_real8_2d

   !------------------
   SUBROUTINE flush_block_data_int32_2d (gdata, spval)

   USE MOD_Precision
   USE MOD_Block
   USE MOD_MPAS_MPI
   IMPLICIT NONE

   type(block_data_int32_2d), intent(inout) :: gdata
   integer, intent(in) :: spval

   ! Local variables
   integer :: iblkme, iblk, jblk

      DO iblkme = 1, gblock%nblkme
         iblk = gblock%xblkme(iblkme)
         jblk = gblock%yblkme(iblkme)
         gdata%blk(iblk,jblk)%val = spval
      ENDDO

   END SUBROUTINE flush_block_data_int32_2d

   !------------------
   SUBROUTINE flush_block_data_real8_3d (gdata, spval)

   USE MOD_Precision
   USE MOD_Block
   USE MOD_MPAS_MPI
   IMPLICIT NONE

   type(block_data_real8_3d), intent(inout) :: gdata
   real(r8), intent(in) :: spval

   ! Local variables
   integer :: iblkme, iblk, jblk

      DO iblkme = 1, gblock%nblkme
         iblk = gblock%xblkme(iblkme)
         jblk = gblock%yblkme(iblkme)
         gdata%blk(iblk,jblk)%val = spval
      ENDDO

   END SUBROUTINE flush_block_data_real8_3d

   !------------------
   SUBROUTINE flush_block_data_real8_4d (gdata, spval)

   USE MOD_Precision
   USE MOD_Block
   USE MOD_MPAS_MPI
   IMPLICIT NONE

   type(block_data_real8_4d), intent(inout) :: gdata
   real(r8), intent(in) :: spval

   ! Local variables
   integer :: iblkme, iblk, jblk

      DO iblkme = 1, gblock%nblkme
         iblk = gblock%xblkme(iblkme)
         jblk = gblock%yblkme(iblkme)
         gdata%blk(iblk,jblk)%val = spval
      ENDDO

   END SUBROUTINE flush_block_data_real8_4d

   !------------------
   SUBROUTINE block_data_linear_transform (gdata, scl, dsp)

   USE MOD_Precision
   USE MOD_Block
   USE MOD_MPAS_MPI
   IMPLICIT NONE

   type(block_data_real8_2d), intent(inout) :: gdata
   real(r8), intent(in), optional :: scl
   real(r8), intent(in), optional :: dsp

   ! Local variables
   integer :: iblkme, iblk, jblk

      IF (present(scl)) THEN
         DO iblkme = 1, gblock%nblkme
            iblk = gblock%xblkme(iblkme)
            jblk = gblock%yblkme(iblkme)
            gdata%blk(iblk,jblk)%val = gdata%blk(iblk,jblk)%val * scl
         ENDDO
      ENDIF

      IF (present(dsp)) THEN
         DO iblkme = 1, gblock%nblkme
            iblk = gblock%xblkme(iblkme)
            jblk = gblock%yblkme(iblkme)
            gdata%blk(iblk,jblk)%val = gdata%blk(iblk,jblk)%val + dsp
         ENDDO
      ENDIF

   END SUBROUTINE block_data_linear_transform

   !------------------
   SUBROUTINE block_data_copy (gdata_from, gdata_to, sca)

   USE MOD_Precision
   USE MOD_Block
   USE MOD_MPAS_MPI
   IMPLICIT NONE

   type(block_data_real8_2d), intent(in)    :: gdata_from
   type(block_data_real8_2d), intent(inout) :: gdata_to
   real(r8), intent(in), optional :: sca

   ! Local variables
   integer :: iblkme, iblk, jblk

      DO iblkme = 1, gblock%nblkme
         iblk = gblock%xblkme(iblkme)
         jblk = gblock%yblkme(iblkme)
         IF (present(sca)) THEN
            gdata_to%blk(iblk,jblk)%val = gdata_from%blk(iblk,jblk)%val * sca
         ELSE
            gdata_to%blk(iblk,jblk)%val = gdata_from%blk(iblk,jblk)%val
         ENDIF
      ENDDO

   END SUBROUTINE block_data_copy

   !------------------
   SUBROUTINE block_data_linear_interp ( &
         gdata_from1, alp1, gdata_from2, alp2, gdata_to)

   USE MOD_Precision
   USE MOD_Block
   USE MOD_MPAS_MPI
   IMPLICIT NONE

   type(block_data_real8_2d), intent(in)    :: gdata_from1, gdata_from2
   real(r8), intent(in) :: alp1, alp2
   type(block_data_real8_2d), intent(inout) :: gdata_to

   ! Local variables
   integer :: iblkme, iblk, jblk

      DO iblkme = 1, gblock%nblkme
         iblk = gblock%xblkme(iblkme)
         jblk = gblock%yblkme(iblkme)
         gdata_to%blk(iblk,jblk)%val = &
            gdata_from1%blk(iblk,jblk)%val * alp1 &
            + gdata_from2%blk(iblk,jblk)%val * alp2
      ENDDO

   END SUBROUTINE block_data_linear_interp

   !-----------------
   SUBROUTINE block_data_division (gdata, sumdata, spv)

   USE MOD_Precision
   USE MOD_Block
   USE MOD_MPAS_MPI
   USE MOD_Vars_Global, only: spval
   IMPLICIT NONE

   type(block_data_real8_2d), intent(inout) :: gdata
   type(block_data_real8_2d), intent(inout) :: sumdata
   real(r8), intent(in), optional :: spv

   ! Local variables
   integer :: iblkme, iblk, jblk

      IF (.true.) THEN

         IF (.not. present(spv)) THEN

            DO iblkme = 1, gblock%nblkme
               iblk = gblock%xblkme(iblkme)
               jblk = gblock%yblkme(iblkme)
               WHERE (sumdata%blk(iblk,jblk)%val > 0.)
                  gdata%blk(iblk,jblk)%val = &
                     gdata%blk(iblk,jblk)%val / sumdata%blk(iblk,jblk)%val
               ELSEWHERE
                  gdata%blk(iblk,jblk)%val = spval
               ENDWHERE
            ENDDO

         ELSE

            DO iblkme = 1, gblock%nblkme
               iblk = gblock%xblkme(iblkme)
               jblk = gblock%yblkme(iblkme)
               WHERE ((sumdata%blk(iblk,jblk)%val > 0.) .and. (gdata%blk(iblk,jblk)%val /= spv))
                  gdata%blk(iblk,jblk)%val = &
                     gdata%blk(iblk,jblk)%val / sumdata%blk(iblk,jblk)%val
               ELSEWHERE
                  gdata%blk(iblk,jblk)%val = spv
               ENDWHERE
            ENDDO

         ENDIF

      ENDIF

   END SUBROUTINE block_data_division

END MODULE MOD_DataType
