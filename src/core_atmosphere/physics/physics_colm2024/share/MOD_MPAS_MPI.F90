#include <define.h>

MODULE MOD_MPAS_MPI

!-----------------------------------------------------------------------------------------
! !DESCRIPTION:
!
!    MPAS owns MPI initialization, finalization, and domain decomposition.
!    Embedded CoLM never creates a process pool or reserves additional ranks.
!    Every MPAS rank advances its locally owned CoLM elements, with rank 0 used
!    only as a collective root for logging and scalar metadata.
!
!    CoLM element ownership is supplied by MPAS cell ownership. Patch/PFT state
!    remains internal to CoLM and is mapped back to the owning element/cell by CoLM.
!
!  Created by Shupeng Zhang, May 2023
!-----------------------------------------------------------------------------------------

   USE mpi
   USE MOD_Precision
   IMPLICIT NONE

   integer, parameter :: mpas_root = 0

   logical :: mpas_is_root = .false.

   ! mpas_comm is a caller-owned duplicate dedicated to CoLM. It is borrowed
   ! here and never freed here; MPAS owns the message context and its lifetime.
   integer :: mpas_comm = MPI_COMM_NULL
   integer :: mpas_rank = -1
   integer :: mpas_size = 0

   integer :: mpas_status (MPI_STATUS_SIZE)
   integer :: mpas_mpi_ierr = 0

   integer, PUBLIC, parameter :: mpi_tag_size = 1
   integer, PUBLIC, parameter :: mpi_tag_mesg = 2
   integer, PUBLIC, parameter :: mpi_tag_data = 3

   PUBLIC :: mpas_mpi_attach
   PUBLIC :: mpas_mpi_detach
   PUBLIC :: mpas_mpi_check

CONTAINS

   !-----------------------------------------
   SUBROUTINE mpas_mpi_attach (mpas_domain_comm)

   IMPLICIT NONE
   integer, intent(in) :: mpas_domain_comm
      IF (mpas_domain_comm == MPI_COMM_NULL) STOP 'MPAS supplied MPI_COMM_NULL to embedded CoLM.'
      IF (mpas_comm /= MPI_COMM_NULL) CALL CoLM_stop('Embedded CoLM communicator was initialized more than once.')

      mpas_comm = mpas_domain_comm

      ! A failed collective cannot be recovered safely. Make errors fatal on
      ! CoLM's dedicated communicator so a later call cannot overwrite ierr.
      CALL mpi_comm_set_errhandler (mpas_comm, MPI_ERRORS_ARE_FATAL, mpas_mpi_ierr)
      IF (mpas_mpi_ierr /= MPI_SUCCESS) THEN
         CALL CoLM_stop('Embedded CoLM could not set the MPI communicator error handler.')
      ENDIF

      CALL mpi_comm_rank (mpas_comm, mpas_rank, mpas_mpi_ierr)
      IF (mpas_mpi_ierr /= MPI_SUCCESS) CALL CoLM_stop('Embedded CoLM could not query its MPAS communicator rank.')
      CALL mpi_comm_size (mpas_comm, mpas_size,  mpas_mpi_ierr)
      IF (mpas_mpi_ierr /= MPI_SUCCESS) CALL CoLM_stop('Embedded CoLM could not query its MPAS communicator size.')
      IF (mpas_size < 1 .or. mpas_rank < 0 .or. mpas_rank >= mpas_size) THEN
         CALL CoLM_stop('Embedded CoLM received an invalid MPAS communicator rank or size.')
      ENDIF

      mpas_is_root = (mpas_rank == mpas_root)

   END SUBROUTINE mpas_mpi_attach

   !-----------------------------------------
   SUBROUTINE mpas_mpi_detach

      mpas_comm = MPI_COMM_NULL
      mpas_rank = -1
      mpas_size = 0
      mpas_is_root = .false.

   END SUBROUTINE mpas_mpi_detach

   !-----------------------------------------
   SUBROUTINE mpas_mpi_check (operation)

   IMPLICIT NONE
   character(len=*), intent(in) :: operation

      IF (mpas_mpi_ierr /= MPI_SUCCESS) THEN
         CALL CoLM_stop('Embedded CoLM MPI failure during '//trim(operation)//'.')
      ENDIF

   END SUBROUTINE mpas_mpi_check

   ! -- STOP all processes --
   SUBROUTINE CoLM_stop (mesg)

   IMPLICIT NONE
      character(len=*), optional :: mesg
      logical :: mpi_inited
      logical :: mpi_is_finalized
      integer :: ierr_local

      IF (present(mesg)) write(*,*) trim(mesg)

      ierr_local = MPI_SUCCESS
      CALL MPI_INITIALIZED (mpi_inited, ierr_local)
      mpi_is_finalized = .false.
      IF (ierr_local == MPI_SUCCESS .and. mpi_inited) THEN
         CALL MPI_FINALIZED (mpi_is_finalized, ierr_local)
      ENDIF
      IF (ierr_local == MPI_SUCCESS .and. mpi_inited .and. (.not. mpi_is_finalized)) THEN
         IF (mpas_comm /= MPI_COMM_NULL) CALL mpi_abort (mpas_comm, 1, ierr_local)
      ENDIF

      STOP

   END SUBROUTINE CoLM_stop

END MODULE MOD_MPAS_MPI
