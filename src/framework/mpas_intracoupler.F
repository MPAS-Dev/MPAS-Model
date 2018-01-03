! Copyright (c) 2017,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
module mpas_intracoupler

    use mpas_derived_types, only : mpas_coupler_type, core_type

    private

    public :: MPAS_coupler_init, &
              MPAS_coupler_finalize, &
              MPAS_coupler_sync


contains


    !-----------------------------------------------------------------------
    !  routine MPAS_coupler_init
    !
    !> \brief  Initializes an intra-component coupler instance
    !> \author Michael Duda
    !> \date   19 September 2017
    !> \details
    !>  Initializes an instance of the mpas_coupler type for subsequent use
    !>  in managing the interaction between sub-groups of MPI tasks. By default,
    !>  the MPI_COMM_WORLD communicator is partitioned, though the optional
    !>  argument mpicomm can be supplied to replace MPI_COMM_WORLD as
    !>  the intercommunicator that will be used for communication between groups.
    !
    !-----------------------------------------------------------------------
    subroutine MPAS_coupler_init(core, cpl, mpicomm)

#ifdef MPAS_OPENMP
        use omp_lib
#endif

#ifdef _MPI
#ifndef NOMPIMOD
        use mpi, only : MPI_COMM_WORLD, MPI_Comm_rank, MPI_comm_size, MPI_Comm_split
#endif
#endif

        implicit none

        type (core_type), intent(in) :: core
        type (mpas_coupler_type), intent(inout) :: cpl
        integer, intent(in), optional :: mpicomm

        integer :: comm_size
        integer :: comm_rank
        integer :: intercomm
        integer :: ierr

#ifdef _MPI
#ifdef NOMPIMOD
        include 'mpif.h'
#endif

        if (present(mpicomm)) then
            intercomm = mpicomm
            cpl % initialized_mpi = .false.
        else
            intercomm = MPI_COMM_WORLD
            cpl % initialized_mpi = .true.
            call MPI_Init(ierr)
        end if

        call MPI_Comm_rank(intercomm, comm_rank, ierr)
        call MPI_Comm_size(intercomm, comm_size, ierr)

        cpl % role => core % component_role
        cpl % intercomm = intercomm
        cpl % mpi_rank = comm_rank
#ifdef MPAS_OPENMP
        cpl % thread_id = omp_get_thread_num()
#else
        cpl % thread_id = 0
#endif

        call MPI_Comm_split(cpl % intercomm, cpl % role(), comm_rank, cpl % intracomm, ierr)
#else
        ! Somehow handle this for non-MPI builds
#endif

    end subroutine MPAS_coupler_init


    !-----------------------------------------------------------------------
    !  routine MPAS_coupler_finalize
    !
    !> \brief  Finalize an intra-component coupler instance
    !> \author Michael Duda
    !> \date   19 September 2017
    !> \details
    !>  Cleans up memory and MPI state that was set up for inter-task-group
    !>  coupling and computation.
    !
    !-----------------------------------------------------------------------
    subroutine MPAS_coupler_finalize(cpl)

#ifdef _MPI
#ifndef NOMPIMOD
       use mpi, only : MPI_Finalize
#endif
#endif

        implicit none

        type (mpas_coupler_type), intent(inout) :: cpl

        integer :: ierr

#ifdef _MPI
#ifdef NOMPIMOD
        include 'mpif.h'
#endif

        if (cpl % initialized_mpi) then
            call MPI_Finalize(ierr)
        end if
#else
        ! Somehow handle this for non-MPI builds
#endif

    end subroutine MPAS_coupler_finalize


    !-----------------------------------------------------------------------
    !  routine MPAS_coupler_sync
    !
    !> \brief  Provide a synchronization point among MPI task groups
    !> \author Michael Duda
    !> \date   19 September 2017
    !> \details
    !>  Placeholder routine to synchronize the flow of execution between
    !>  two task groups.
    !
    !-----------------------------------------------------------------------
    subroutine MPAS_coupler_sync(cpl)

#ifdef _MPI
#ifndef NOMPIMOD
        use mpi
#endif
#endif

        implicit none

        type (mpas_coupler_type), intent(inout) :: cpl

        integer :: ierr

#ifdef _MPI
#ifdef NOMPIMOD
        include 'mpif.h'
#endif

        call MPI_Barrier(cpl % intercomm, ierr)

#else
        ! Somehow handle this for non-MPI builds
#endif

    end subroutine MPAS_coupler_sync

end module mpas_intracoupler
