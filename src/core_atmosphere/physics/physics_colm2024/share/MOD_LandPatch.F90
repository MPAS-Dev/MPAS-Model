#include <define.h>

MODULE MOD_LandPatch

!-----------------------------------------------------------------------
! !DESCRIPTION:
!
!    Build pixelset "landpatch".
!
!    In CoLM, the global/regional area is divided into a hierarchical structure:
!    1. If GRIDBASED or UNSTRUCTURED is defined, it is
!       ELEMENT >>> PATCH
!    2. If CATCHMENT is defined, it is
!       ELEMENT >>> HRU >>> PATCH
!    If Plant Function Type classification is used, PATCH is further divided into PFT.
!    If Plant Community classification is used,     PATCH is further divided into PC.
!
!    "landpatch" refers to pixelset PATCH.
!
!  Created by Shupeng Zhang, May 2023
!    porting codes from Hua Yuan's OpenMP version to MPI parallel version.
!-----------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_Grid
   USE MOD_Pixelset
   USE MOD_Vars_Global
   USE MOD_Const_LC
   IMPLICIT NONE

   ! ---- Instance ----
   integer :: numpatch
   type(grid_type)     :: grid_patch
   type(pixelset_type) :: landpatch

   type(subset_type)   :: elm_patch
   type(superset_type) :: patch2elm

#ifdef CATCHMENT
   type(subset_type)   :: hru_patch
   type(superset_type) :: patch2hru
#endif



END MODULE MOD_LandPatch
