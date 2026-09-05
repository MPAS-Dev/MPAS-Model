#include <define.h>

#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)

MODULE MOD_LandPFT

!-----------------------------------------------------------------------
! !DESCRIPTION:
!
!    Build pixelset "landpft" (Plant Function Type).
!
!    In CoLM, the global/regional area is divided into a hierarchical structure:
!    1. If GRIDBASED or UNSTRUCTURED is defined, it is
!       ELEMENT >>> PATCH
!    2. If CATCHMENT is defined, it is
!       ELEMENT >>> HRU >>> PATCH
!    If Plant Function Type classification is used, PATCH is further divided into PFT.
!    If Plant Community classification is used,     PATCH is further divided into PC.
!
!    "landpft" refers to pixelset PFT.
!
!  Created by Shupeng Zhang, May 2023
!    porting codes from Hua Yuan's OpenMP version to MPI parallel version.
!-----------------------------------------------------------------------

   USE MOD_Namelist
   USE MOD_Pixelset
   USE MOD_Const_LC
   USE MOD_Vars_Global
   IMPLICIT NONE

   ! ---- Instance ----
   integer :: numpft
   type(pixelset_type) :: landpft

   integer , allocatable :: pft2patch   (:)  !patch index of a PFT
   integer , allocatable :: patch_pft_s (:)  !start PFT index of a patch
   integer , allocatable :: patch_pft_e (:)  !end PFT index of a patch

CONTAINS

   ! -------------------------------

   ! ----------------------
   SUBROUTINE map_patch_to_pft

   USE MOD_MPAS_MPI
   USE MOD_LandPatch
   USE MOD_Const_LC
   IMPLICIT NONE

   integer :: ipatch, ipft

      IF (.true.) THEN

         IF (allocated(patch_pft_s)) deallocate(patch_pft_s)
         IF (allocated(patch_pft_e)) deallocate(patch_pft_e)
         IF (allocated(pft2patch  )) deallocate(pft2patch  )

	         IF (numpatch <= 0 .and. numpft > 0) THEN
	            CALL CoLM_stop('CoLM has PFT/PC entries on a rank with no loaded patches.')
	         ENDIF
	         IF (numpatch <= 0) RETURN

	         allocate (patch_pft_s (numpatch))
	         allocate (patch_pft_e (numpatch))
         allocate (pft2patch   (numpft  ))
	         patch_pft_s(:) = -1
	         patch_pft_e(:) = -1
	         pft2patch(:) = -1

         ipft = 1
         DO ipatch = 1, numpatch
#ifndef CROP
            IF (patchtypes(landpatch%settyp(ipatch)) == 0) THEN
#else
            IF (patchtypes(landpatch%settyp(ipatch)) == 0 .and. landpatch%settyp(ipatch)/=CROPLAND) THEN
#endif

               patch_pft_s(ipatch) = ipft

               DO WHILE (ipft <= numpft)
                  IF ((landpft%eindex(ipft) == landpatch%eindex(ipatch))  &
                     .and. (landpft%ipxstt(ipft) == landpatch%ipxstt(ipatch))  &
                     .and. (landpft%settyp(ipft) < N_PFT)) THEN
                     pft2patch  (ipft  ) = ipatch
                     patch_pft_e(ipatch) = ipft
                     ipft = ipft + 1
                  ELSE
                     EXIT
                  ENDIF
               ENDDO
	               IF (patch_pft_e(ipatch) < patch_pft_s(ipatch)) THEN
	                  CALL CoLM_stop('A vegetated CoLM patch has no matching PFT/PC state.')
	               ENDIF
#ifdef CROP
            ELSEIF (landpatch%settyp(ipatch) == CROPLAND) THEN
	               IF (ipft > numpft) CALL CoLM_stop('A CoLM crop patch has no matching PFT state.')
               patch_pft_s(ipatch) = ipft
               patch_pft_e(ipatch) = ipft
               pft2patch  (ipft  ) = ipatch
               ipft = ipft + 1
#endif
            ELSE
               patch_pft_s(ipatch) = -1
               patch_pft_e(ipatch) = -1
            ENDIF

         ENDDO
	         IF (ipft /= numpft + 1 .or. any(pft2patch < 1)) THEN
	            CALL CoLM_stop('CoLM PFT/PC entries do not map one-to-one onto loaded patches.')
	         ENDIF

      ENDIF

   END SUBROUTINE map_patch_to_pft

END MODULE MOD_LandPFT
#endif
