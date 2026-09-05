#include <define.h>

MODULE MOD_Block

!-------------------------------------------------------------------------------------
! !DESCRIPTION:
!
!    To deal with high-resolution data, the globe is divided into blocks.
!
!     (180W,90N)                           (180E,90N)
!        .-----------------------------------.
!        |         |         |        |      |
!        |         |         |        |      |
!        |         |         |        |      |
!        .-----------------------------------.
!        |         |         |        |      |
!        |         |         |        |      |
!        |         |         |        |      |
!        .-----------------------------------.
!        |         |         |        |      |
!        |         |         |        |      |
!        |         |         |        |      |
!        .-----------------------------------.
!     (180W,90S)                           (180E,90S)
!
!    1.
!    Boundaries for block (i,j) is saved in
!    "gblock%lat_s(j), gblock%lat_n(j), gblock%lon_w(i), gblock%lon_e(i)"
!    for south, north, west and east boundaries respectively.
!
!    2. In embedded mode, owner_rank(i,j) records whether this MPAS rank owns
!    cells whose CoLM elements are stored in block (i,j). A value of -1 means
!    that the block is not part of this rank's local cell subset.
!
!    3. "gblock%nblkme, gblock%xblkme(:), gblock%yblkme(:)" contain the local
!    block list derived from MPAS cell ownership.
!
!  Created by Shupeng Zhang, May 2023
!-------------------------------------------------------------------------------------

   USE MOD_Precision
   IMPLICIT NONE

   ! ---- data types ----
   type :: block_type

      ! Coordinates.
      integer :: nxblk, nyblk
      real(r8), allocatable :: lat_s (:)
      real(r8), allocatable :: lat_n (:)
      real(r8), allocatable :: lon_w (:)
      real(r8), allocatable :: lon_e (:)

      integer, allocatable :: owner_rank(:,:)

      integer :: nblkme
      integer, allocatable :: xblkme(:), yblkme(:)

   CONTAINS

      procedure, PUBLIC :: load_from_file => block_load_from_file

      final :: block_free_mem

   END type block_type

   ! ---- Instance ----
   type (block_type) :: gblock


   ! ---- PUBLIC SUBROUTINE ----
   PUBLIC :: get_filename_block

CONTAINS

   ! --------------------------------
   SUBROUTINE block_load_from_file (this, dir_landdata)

   USE MOD_NetCDFSerial
   USE MOD_MPAS_MPI, only: mpas_is_root, CoLM_stop
   USE, INTRINSIC :: ieee_arithmetic, only: ieee_is_finite
   IMPLICIT NONE

   class (block_type) :: this
   character(len=*),  intent(in) :: dir_landdata

   ! Local variables
   character(len=256) :: filename

      filename = trim(dir_landdata) // '/block.nc'

      CALL ncio_read_bcast_serial (filename, 'lat_s', this%lat_s)
      CALL ncio_read_bcast_serial (filename, 'lat_n', this%lat_n)
      CALL ncio_read_bcast_serial (filename, 'lon_w', this%lon_w)
      CALL ncio_read_bcast_serial (filename, 'lon_e', this%lon_e)

      this%nyblk = size(this%lat_s)
      this%nxblk = size(this%lon_w)

      IF (this%nxblk < 1 .or. this%nyblk < 1 .or. size(this%lat_n) /= this%nyblk .or. &
          size(this%lon_e) /= this%nxblk) THEN
         CALL CoLM_stop('CoLM block.nc contains inconsistent block-boundary dimensions.')
      ENDIF
      IF (.not. all(ieee_is_finite(this%lat_s)) .or. .not. all(ieee_is_finite(this%lat_n)) .or. &
          .not. all(ieee_is_finite(this%lon_w)) .or. .not. all(ieee_is_finite(this%lon_e))) THEN
         CALL CoLM_stop('CoLM block.nc contains non-finite block boundaries.')
      ENDIF
      IF (any(this%lat_s < -90._r8) .or. any(this%lat_n > 90._r8) .or. &
          any(this%lat_n <= this%lat_s)) THEN
         CALL CoLM_stop('CoLM block.nc contains invalid latitude boundaries.')
      ENDIF
      IF (this%nyblk > 1) THEN
         IF (any(this%lat_s(2:) <= this%lat_s(:this%nyblk-1))) &
            CALL CoLM_stop('CoLM block.nc latitude blocks are not ordered south to north.')
      ENDIF

      IF (mpas_is_root) THEN
         write (*,*) 'Block information:'
         write (*,'(I3,A,I3,A)') this%nxblk, ' blocks in longitude,', &
            this%nyblk, ' blocks in latitude.'
         write (*,*)
      ENDIF

      IF (allocated(this%owner_rank)) deallocate(this%owner_rank)
      allocate (this%owner_rank (this%nxblk,this%nyblk))
      this%owner_rank(:,:) = -1
      this%nblkme = 0

   END SUBROUTINE block_load_from_file

   ! --------------------------------
   SUBROUTINE block_free_mem (this)

   IMPLICIT NONE
   type (block_type) :: this

      IF (allocated (this%lat_s))  deallocate (this%lat_s)
      IF (allocated (this%lat_n))  deallocate (this%lat_n)
      IF (allocated (this%lon_w))  deallocate (this%lon_w)
      IF (allocated (this%lon_e))  deallocate (this%lon_e)

      IF (allocated (this%owner_rank)) deallocate (this%owner_rank)

      IF (allocated (this%xblkme)) deallocate (this%xblkme)
      IF (allocated (this%yblkme)) deallocate (this%yblkme)

   END SUBROUTINE block_free_mem

   ! -----
   SUBROUTINE get_blockname (iblk, jblk, blockname)

   IMPLICIT NONE

   integer, intent(in) :: iblk, jblk

   character(len=*), intent(out) :: blockname

   ! Local variables
   character(len=4) :: cx
   character(len=3) :: cy
      IF (gblock%lat_s(jblk) < 0) THEN
         write (cy, '(A1,I2.2)') 's', - floor(gblock%lat_s(jblk))
      ELSE
         write (cy, '(A1,I2.2)') 'n',   floor(gblock%lat_s(jblk))
      ENDIF

      IF (gblock%lon_w(iblk) < 0) THEN
         write (cx, '(A1,I3.3)') 'w', - floor(gblock%lon_w(iblk))
      ELSE
         write (cx, '(A1,I3.3)') 'e',   floor(gblock%lon_w(iblk))
      ENDIF

      blockname = trim(cx) // '_' // trim(cy)

   END SUBROUTINE get_blockname

   ! --------------------------------
   SUBROUTINE get_filename_block (filename, iblk, jblk, fileblock)

   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   integer, intent(in) :: iblk, jblk

   character(len=*), intent(out) :: fileblock

   ! Local variables
   character(len=8) :: blockname
   integer :: i

      CALL get_blockname (iblk, jblk, blockname)

      i = len_trim (filename)
      DO WHILE (i > 0)
         IF (filename(i:i) == '.') EXIT
         i = i - 1
      ENDDO

      IF (i > 0) THEN
         fileblock = filename(1:i-1) // '_' // blockname // '.nc'
      ELSE
         fileblock = filename // '_' // blockname // '.nc'
      ENDIF

   END SUBROUTINE get_filename_block

END MODULE MOD_Block
