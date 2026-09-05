#include <define.h>

MODULE MOD_LandElm

!-----------------------------------------------------------------------
! !DESCRIPTION:
!
!    Build pixelset "landelm".
!
!    In CoLM, the global/regional area is divided into a hierarchical structure:
!    1. If GRIDBASED or UNSTRUCTURED is defined, it is
!       ELEMENT >>> PATCH
!    2. If CATCHMENT is defined, it is
!       ELEMENT >>> HRU >>> PATCH
!    If Plant Function Type classification is used, PATCH is further divided into PFT.
!    If Plant Community classification is used,     PATCH is further divided into PC.
!
!    "landelm" refers to pixelset ELEMENT.
!
!  Created by Shupeng Zhang, May 2023
!-----------------------------------------------------------------------

   USE MOD_Pixelset
   IMPLICIT NONE

   ! ---- Instance ----
   type(pixelset_type) :: landelm

CONTAINS

   ! -------------------------------
   SUBROUTINE landelm_build

   USE MOD_Precision
   USE MOD_MPAS_MPI
   USE MOD_Mesh
   IMPLICIT NONE

   ! Local Variables
   integer :: ielm, nelm_glb

      IF (mpas_is_root) THEN
         write(*,'(A)') 'Making land elements:'
      ENDIF

      IF (.true.) THEN

         allocate (landelm%eindex (numelm))
         allocate (landelm%ipxstt (numelm))
         allocate (landelm%ipxend (numelm))
         allocate (landelm%settyp (numelm))
         allocate (landelm%ielm   (numelm))

         DO ielm = 1, numelm
            landelm%eindex(ielm) = mesh(ielm)%indx
            landelm%ipxstt(ielm) = 1
            landelm%ipxend(ielm) = mesh(ielm)%npxl
            landelm%settyp(ielm) = 0
            landelm%ielm  (ielm) = ielm
         ENDDO

      ENDIF

      landelm%nset = numelm
      CALL landelm%set_vecgs

#ifdef MPAS_MPI
      CALL mpi_barrier (mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('land-element count reduction entry')

      IF (.true.) THEN
         CALL mpi_reduce (numelm, nelm_glb, 1, MPI_INTEGER, MPI_SUM, mpas_root, mpas_comm, mpas_mpi_ierr)
         CALL mpas_mpi_check('land-element count reduction')
         IF (mpas_rank == 0) THEN
            write(*,'(A,I12,A)') 'Total: ', nelm_glb, ' elements.'
         ENDIF
      ENDIF

      CALL mpi_barrier (mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('land-element construction completion')
#else
      write(*,'(A,I12,A)') 'Total: ', numelm, ' elements.'
#endif

   END SUBROUTINE landelm_build

END MODULE MOD_LandElm
