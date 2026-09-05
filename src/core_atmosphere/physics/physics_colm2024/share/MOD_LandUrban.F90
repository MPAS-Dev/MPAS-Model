#include <define.h>

MODULE MOD_LandUrban
!-----------------------------------------------------------------------
!
! !DESCRIPTION:
!  Build pixelset "landurban".
!
!  Original authors: Hua Yuan and Wenzong Dong, 2021, OpenMP version.
!
!
! !REVISIONS:
!  05/2023, Wenzong Dong, Hua Yuan, Shupeng Zhang: porting codes to MPI
!           parallel version.
!
!-----------------------------------------------------------------------

   USE MOD_Grid
   USE MOD_Pixelset
   USE MOD_Vars_Global, only: N_URB, URBAN

   IMPLICIT NONE

   ! ---- Instance ----
   type(grid_type) :: grid_urban

   integer :: numurban
   type(pixelset_type) :: landurban

   integer , allocatable :: urban_reg   (:)  !region index of a urban
   integer , allocatable :: urban2patch (:)  !patch index of a urban
   integer , allocatable :: patch2urban (:)  !urban index of a patch

   PUBLIC :: map_patch_to_urban

CONTAINS

   ! -------------------------------

   ! ----------------------
   SUBROUTINE map_patch_to_urban

   USE MOD_MPAS_MPI
   USE MOD_LandPatch
   IMPLICIT NONE

   integer :: ipatch, iurban

      IF (.true.) THEN

         IF ((numpatch <= 0) .or. (numurban <= 0)) RETURN

         IF (allocated(patch2urban)) deallocate(patch2urban)
         IF (allocated(urban2patch)) deallocate(urban2patch)
         allocate (patch2urban (numpatch))
         allocate (urban2patch (numurban))

         iurban = 0
         DO ipatch = 1, numpatch
            IF (landpatch%settyp(ipatch) == URBAN) THEN
               iurban = iurban + 1
               patch2urban(ipatch) = iurban
               urban2patch(iurban) = ipatch
            ELSE
               patch2urban(ipatch) = -1
            ENDIF
         ENDDO

      ENDIF

   END SUBROUTINE map_patch_to_urban

END MODULE MOD_LandUrban
