! Copyright (c) 2017,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
module mpas_intracoupler

    use mpas_derived_types, only : mpas_coupler_type, core_type, MPAS_MAX_COUPLER_PAIRINGS

    private

    public :: MPAS_coupler_init, &
              MPAS_coupler_finalize, &
              MPAS_coupler_sync, &
              MPAS_coupler_create_pairing, &
              MPAS_coupler_transfer_field

    interface MPAS_coupler_transfer_field
        module procedure MPAS_coupler_transfer_field_r1
        module procedure MPAS_coupler_transfer_field_r2
        module procedure MPAS_coupler_transfer_field_r3
    end interface

#ifdef CORE_ATMOSPHERE
    integer, parameter, public :: ROLE_INTEGRATE = ibset(0,0), ROLE_RADIATION = ibset(0,1)
#endif


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
        cpl % role_leader => core % component_role_leader
        cpl % intercomm = intercomm
        cpl % intercomm_rank = comm_rank
        cpl % intercomm_size = comm_size
#ifdef MPAS_OPENMP
        cpl % thread_id = omp_get_thread_num()
#else
        cpl % thread_id = 0
#endif
        allocate(cpl % pairings(MPAS_MAX_COUPLER_PAIRINGS))

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

        use mpas_dmpar, only : mpas_dmpar_finalize, mpas_dmpar_destroy_mulithalo_exchange_list, mpas_dmpar_destroy_communication_list

#ifdef _MPI
#ifndef NOMPIMOD
       use mpi, only : MPI_Finalize
#endif
#endif

        implicit none

        type (mpas_coupler_type), intent(inout) :: cpl

        integer :: i
        integer :: idx
        integer :: ierr

#ifdef _MPI
#ifdef NOMPIMOD
        include 'mpif.h'
#endif

        do i=1,size(cpl % pairings)
            if (cpl % pairings(i) % isValid) then
                call mpas_dmpar_finalize(cpl % pairings(i) % dminfo)
                call mpas_dmpar_destroy_mulithalo_exchange_list(cpl % pairings(i) % sendList)
                call mpas_dmpar_destroy_mulithalo_exchange_list(cpl % pairings(i) % recvList)
                call mpas_dmpar_destroy_mulithalo_exchange_list(cpl % pairings(i) % copyList)

                do idx=1,size(cpl % pairings(i) % commArrays)
                    if (maxval(cpl % pairings(i) % commArrays(idx) % dims(:)) > -1) then
                        if (associated(cpl % pairings(i) % commArrays(idx) % compactHaloInfo)) then
                            deallocate(cpl % pairings(i) % commArrays(idx) % compactHaloInfo)
                        end if
                        if (associated(cpl % pairings(i) % commArrays(idx) % sendCommList)) then
                            call mpas_dmpar_destroy_communication_list(cpl % pairings(i) % commArrays(idx) % sendCommList)
                        end if
                        if (associated(cpl % pairings(i) % commArrays(idx) % recvCommList)) then
                            call mpas_dmpar_destroy_communication_list(cpl % pairings(i) % commArrays(idx) % recvCommList)
                        end if

                        if (associated(cpl % pairings(i) % commArrays(idx) % gpu_idx_send)) then
                            deallocate(cpl % pairings(i) % commArrays(idx) % gpu_idx_send)
                        end if
                        if (associated(cpl % pairings(i) % commArrays(idx) % gpu_bufferoffset_send)) then
                            deallocate(cpl % pairings(i) % commArrays(idx) % gpu_bufferoffset_send)
                        end if
                        if (associated(cpl % pairings(i) % commArrays(idx) % gpu_nList_send)) then
                            deallocate(cpl % pairings(i) % commArrays(idx) % gpu_nList_send)
                        end if

                        if (associated(cpl % pairings(i) % commArrays(idx) % gpu_idx_recv)) then
                            deallocate(cpl % pairings(i) % commArrays(idx) % gpu_idx_recv)
                        end if
                        if (associated(cpl % pairings(i) % commArrays(idx) % gpu_bufferoffset_recv)) then
                            deallocate(cpl % pairings(i) % commArrays(idx) % gpu_bufferoffset_recv)
                        end if
                        if (associated(cpl % pairings(i) % commArrays(idx) % gpu_nList_recv)) then
                            deallocate(cpl % pairings(i) % commArrays(idx) % gpu_nList_recv)
                        end if

                    end if
                end do

            end if
        end do
        deallocate(cpl % pairings)

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


    !-----------------------------------------------------------------------
    !  routine MPAS_coupler_create_pairing
    !
    !> \brief  Establish communication pathways to or from another role
    !> \author Michael Duda
    !> \date   3 January 2018
    !> \details
    !>  Returns a handle to a "pairing" of this role with another role, with
    !>  the ability to either send to or receive from the paired role, depending
    !>  on the value of the "direction" argument. The "indices" argument is
    !>  a field containg the global indices to be communicated and is likely
    !>  to be either indexToCellID, indexToVertexID, or indexToEdgeID.
    !>  The "nIndices" argument determines how many of the global indices will
    !>  be communicated, and is likely to be either nCellsSolve, nVerticesSolve,
    !>  or nEdgesSolve, since at present the transfer of a field between paired
    !>  roles is expected to take place for only owned elements.
    !
    !-----------------------------------------------------------------------
    function MPAS_coupler_create_pairing(cpl, direction, destination_role, indices, nIndices, ierr) result(handle)

        use mpas_derived_types, only : field1DInteger, MPAS_COUPLER_INCOMING, dm_info
        use mpas_dmpar, only : mpas_dmpar_init, mpas_dmpar_init_multihalo_exchange_list, mpas_dmpar_get_exch_list
        use mpas_field_routines, only : mpas_duplicate_field, mpas_deallocate_field

#ifdef _MPI
#ifndef NOMPIMOD
        use mpi
#endif
#endif

        implicit none

        type (mpas_coupler_type), intent(inout) :: cpl
        integer, intent(in) :: direction
        integer, intent(in) :: destination_role
        integer, intent(in) :: nIndices
        type (field1DInteger), intent(in), target :: indices
        integer, intent(out), optional :: ierr

        integer :: handle

#ifdef _MPI
#ifdef NOMPIMOD
        include 'mpif.h'
#endif

        integer :: i
        integer :: tag
        integer :: intercomm
        integer :: intracomm
        integer :: mpi_ierr
        type (field1DInteger), pointer :: indices_copy
        type (field1DInteger), pointer :: empty
        type (dm_info), pointer :: dminfo_save

!
! Possible errors: invalid direction, destination role is my own
!


        if (present(ierr)) then
!TODO: define error codes for intracoupler
            ierr = 0
        end if

        !
        ! Find next available handle
        !
        handle = -1
        do i=1,size(cpl % pairings)
            if (.not. cpl % pairings(i) % isValid) then
                handle = i
                exit
            end if
        end do

        if (handle == -1 .and. present(ierr)) then
            ierr = -1
            return
        end if

        !
        ! Set up MPI intercommunicator
        !
!TODO: cleverly choose a unique tag for each pairing
        tag = 42
        call MPI_Intercomm_create(cpl % intracomm, 0, cpl % intercomm, cpl % role_leader(destination_role), tag, intercomm, mpi_ierr)
        if (mpi_ierr /= MPI_SUCCESS) then
            if (present(ierr)) then
                handle = -1
                ierr = -1
                return
            end if
        end if

        if (direction == MPAS_COUPLER_INCOMING) then
            call MPI_Intercomm_merge(intercomm, .false., intracomm, mpi_ierr)
        else
            call MPI_Intercomm_merge(intercomm, .true., intracomm, mpi_ierr)
        end if

        !
        ! Set up dm_info structure
        !
        call mpas_dmpar_init(cpl % pairings(handle) % dminfo, mpi_comm=intracomm)

!
! Concerns: this routine is not thread safe, since we temporarily modify the dminfo pointer in the domain type
!

        !
        ! Set up multihalo exchange lists
        !
        call mpas_duplicate_field(indices, indices_copy)
        indices_copy % dimsizes(1) = nIndices
        deallocate(indices_copy % array)
        allocate(indices_copy % array(1:nIndices))
        indices_copy % array(1:nIndices) = indices % array(1:nIndices)

        nullify(indices_copy % sendList)
        nullify(indices_copy % recvList)
        nullify(indices_copy % copyList)
        call mpas_dmpar_init_multihalo_exchange_list(indices_copy % sendList, 1)
        call mpas_dmpar_init_multihalo_exchange_list(indices_copy % recvList, 1)
        call mpas_dmpar_init_multihalo_exchange_list(indices_copy % copyList, 1)

        allocate(empty)
        allocate(empty % array(0))
!TODO: make this work for multiple blocks
        nullify(empty % prev)
        nullify(empty % next)
        empty % dimsizes(1) = 0
        empty % block => indices_copy % block
        empty % sendList => indices_copy % sendList
        empty % recvList => indices_copy % recvList
        empty % copyList => indices_copy % copyList

        ! Temporarily swap in dminfo based on merged intercommunicator
        dminfo_save => indices_copy % block % domain % dminfo
        indices_copy % block % domain % dminfo => cpl % pairings(handle) % dminfo

        ! Fill out sendList, recvList, copyList
        if (direction == MPAS_COUPLER_INCOMING) then
            call mpas_dmpar_get_exch_list(1, empty, indices_copy)
        else
            call mpas_dmpar_get_exch_list(1, indices_copy, empty)
        end if

        ! Reset dminfo pointer
        indices_copy % block % domain % dminfo => dminfo_save

        cpl % pairings(handle) % sendList => indices_copy % sendList
        cpl % pairings(handle) % recvList => indices_copy % recvList
        cpl % pairings(handle) % copyList => indices_copy % copyList
        
        call mpas_deallocate_field(indices_copy)
        deallocate(empty % array)
        deallocate(empty)


        !
        ! Finish set-up of pairing structure
        !
        cpl % pairings(handle) % direction = direction
        cpl % pairings(handle) % isValid = .true.
#else
        ! Somehow handle this for non-MPI builds
#endif

    end function MPAS_coupler_create_pairing


    !-----------------------------------------------------------------------
    !  routine MPAS_coupler_transfer_field_r1
    !
    !> \brief  Establish communication pathways to or from another role
    !> \author Michael Duda
    !> \date   3 January 2018
    !> \details
    !>  Returns a handle to a "pairing" of this role with another role, with
    !>  the ability to either send to or receive from the paired role, depending
    !>  on the value of the "direction" argument.
    !
    !-----------------------------------------------------------------------
    subroutine MPAS_coupler_transfer_field_r1(cpl, handle, field, ierr)

        use mpas_derived_types, only : field1DReal, dm_info, mpas_multihalo_exchange_list
        use mpas_dmpar, only : mpas_dmpar_gpu_init_1d_real_coupler, mpas_dmpar_compact_halo_info, mpas_dmpar_exch_halo_coupler

        implicit none

        type (mpas_coupler_type), intent(inout) :: cpl
        integer, intent(in) :: handle
        type (field1DReal), pointer :: field
        integer, intent(out), optional :: ierr

        integer :: idx
        type (dm_info), pointer :: dminfo_save
        type (mpas_multihalo_exchange_list), pointer :: sendList_save
        type (mpas_multihalo_exchange_list), pointer :: recvList_save
        type (mpas_multihalo_exchange_list), pointer :: copyList_save


        !
        ! Make sure handle is valid
        !
        if (handle <= 0 .or. handle > size(cpl % pairings)) then
            if (present(ierr)) then
                ierr = 1
            end if
            return
        else if (.not. cpl % pairings(handle) % isValid) then
            if (present(ierr)) then
                ierr = 1
            end if
            return
        end if

        !
        ! Find index of communication structure appropriate for the dimensionality of this field
        !
        idx = 1
        do while (idx <= size(cpl % pairings(handle) % commArrays) .and. &
                  maxval(cpl % pairings(handle) % commArrays(idx) % dims(:)) > -1)
           if (cpl % pairings(handle) % commArrays(idx) % dims(1) == field % dimsizes(1) .and. &
               cpl % pairings(handle) % commArrays(idx) % dims(2) == -1) then
              exit
           end if
           idx = idx + 1
        end do

        !
        ! Verify that we have not run out of available commArrays
        !
        if (idx > size(cpl % pairings(handle) % commArrays)) then
            if (present(ierr)) then
                ierr = 1
                return
            end if
        end if

        dminfo_save => field % block % domain % dminfo
        sendList_save => field % sendList
        recvList_save => field % recvList
        copyList_save => field % copyList

        field % block % domain % dminfo => cpl % pairings(handle) % dminfo
        field % sendList => cpl % pairings(handle) % sendList
        field % recvList => cpl % pairings(handle) % recvList
        field % copyList => cpl % pairings(handle) % copyList


        !
        ! If all of the dimensions in the commArrays structure are -1, the members of commArrays have
        ! not yet been initialized with a call to mpas_dmpar_compact_halo_info
        !
        if (maxval(cpl % pairings(handle) % commArrays(idx) % dims(:)) == -1) then
            call mpas_dmpar_compact_halo_info(field, cpl % pairings(handle) % commArrays(idx) % compactHaloInfo)
            cpl % pairings(handle) % commArrays(idx) % dims(1) = field % dimsizes(1)

            call mpas_dmpar_gpu_init_1d_real_coupler(field % array, cpl % pairings(handle) % commArrays(idx) % compactHaloInfo, &
                                                                cpl % pairings(handle) % commArrays(idx) % sendCommList, &
                                                                cpl % pairings(handle) % commArrays(idx) % recvCommList, &
                                                                cpl % pairings(handle) % commArrays(idx) % gpu_bufferOffset_send, &
                                                                cpl % pairings(handle) % commArrays(idx) % gpu_nList_send, &
                                                                cpl % pairings(handle) % commArrays(idx) % gpu_idx_send, &
                                                                cpl % pairings(handle) % commArrays(idx) % gpu_bufferOffset_recv, &
                                                                cpl % pairings(handle) % commArrays(idx) % gpu_nList_recv, &
                                                                cpl % pairings(handle) % commArrays(idx) % gpu_idx_recv, &
                                                                cpl % pairings(handle) % commArrays(idx) % gpu_dimsizes, &
                                                                cpl % pairings(handle) % commArrays(idx) % gpu_nHaloLayers )

            cpl % pairings(handle) % commArrays(idx) % gpu_send_size = size(cpl % pairings(handle) % commArrays(idx) % gpu_nList_send)
            cpl % pairings(handle) % commArrays(idx) % gpu_recv_size = size(cpl % pairings(handle) % commArrays(idx) % gpu_nList_recv)
            cpl % pairings(handle) % commArrays(idx) % gpu_compactHaloInfoSize = size(cpl % pairings(handle) % commArrays(idx) % compactHaloInfo)
            cpl % pairings(handle) % commArrays(idx) % gpuArraySize(1) = size(field % array, 1)
        end if



        call mpas_dmpar_exch_halo_coupler(field % array, cpl % pairings(handle) % commArrays(idx) % compactHaloInfo, &
                                                     cpl % pairings(handle) % commArrays(idx) % sendCommList, &
                                                     cpl % pairings(handle) % commArrays(idx) % recvCommList, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpu_bufferOffset_send, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpu_nList_send, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpu_idx_send, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpu_bufferOffset_recv, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpu_nList_recv, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpu_idx_recv, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpu_dimsizes, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpu_nHaloLayers, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpu_send_size, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpu_recv_size, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpu_compactHaloInfoSize, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpuArraySize(1))
        field % block % domain % dminfo => dminfo_save
        field % sendList => sendList_save
        field % recvList => recvList_save
        field % copyList => copyList_save

        if (present(ierr)) then
!TODO: define error codes for intracoupler
            ierr = 0
        end if

    end subroutine MPAS_coupler_transfer_field_r1


    !-----------------------------------------------------------------------
    !  routine MPAS_coupler_transfer_field_r2
    !
    !> \brief  Establish communication pathways to or from another role
    !> \author Michael Duda
    !> \date   3 January 2018
    !> \details
    !>  Returns a handle to a "pairing" of this role with another role, with
    !>  the ability to either send to or receive from the paired role, depending
    !>  on the value of the "direction" argument.
    !
    !-----------------------------------------------------------------------
    subroutine MPAS_coupler_transfer_field_r2(cpl, handle, field, ierr)

        use mpas_derived_types, only : field2DReal, dm_info, mpas_multihalo_exchange_list
        use mpas_dmpar, only : mpas_dmpar_gpu_init_2d_real_coupler, mpas_dmpar_compact_halo_info, mpas_dmpar_exch_halo_coupler

        implicit none

        type (mpas_coupler_type), intent(inout) :: cpl
        integer, intent(in) :: handle
        type (field2DReal), pointer :: field
        integer, intent(out), optional :: ierr

        integer :: idx
        type (dm_info), pointer :: dminfo_save
        type (mpas_multihalo_exchange_list), pointer :: sendList_save
        type (mpas_multihalo_exchange_list), pointer :: recvList_save
        type (mpas_multihalo_exchange_list), pointer :: copyList_save


        !
        ! Make sure handle is valid
        !
        if (handle <= 0 .or. handle > size(cpl % pairings)) then
            if (present(ierr)) then
                ierr = 1
            end if
            return
        else if (.not. cpl % pairings(handle) % isValid) then
            if (present(ierr)) then
                ierr = 1
            end if
            return
        end if

        !
        ! Find index of communication structure appropriate for the dimensionality of this field
        !
        idx = 1
        do while (idx <= size(cpl % pairings(handle) % commArrays) .and. &
                  maxval(cpl % pairings(handle) % commArrays(idx) % dims(:)) > -1)
           if (cpl % pairings(handle) % commArrays(idx) % dims(1) == field % dimsizes(1) .and. &
               cpl % pairings(handle) % commArrays(idx) % dims(2) == field % dimsizes(2) .and. &
               cpl % pairings(handle) % commArrays(idx) % dims(3) == -1) then
              exit
           end if
           idx = idx + 1
        end do

        !
        ! Verify that we have not run out of available commArrays
        !
        if (idx > size(cpl % pairings(handle) % commArrays)) then
            if (present(ierr)) then
                ierr = 1
                return
            end if
        end if

        dminfo_save => field % block % domain % dminfo
        sendList_save => field % sendList
        recvList_save => field % recvList
        copyList_save => field % copyList

        field % block % domain % dminfo => cpl % pairings(handle) % dminfo
        field % sendList => cpl % pairings(handle) % sendList
        field % recvList => cpl % pairings(handle) % recvList
        field % copyList => cpl % pairings(handle) % copyList


        !
        ! If all of the dimensions in the commArrays structure are -1, the members of commArrays have
        ! not yet been initialized with a call to mpas_dmpar_compact_halo_info
        !
        if (maxval(cpl % pairings(handle) % commArrays(idx) % dims(:)) == -1) then
            call mpas_dmpar_compact_halo_info(field, cpl % pairings(handle) % commArrays(idx) % compactHaloInfo)
            cpl % pairings(handle) % commArrays(idx) % dims(1) = field % dimsizes(1)
            cpl % pairings(handle) % commArrays(idx) % dims(2) = field % dimsizes(2)

            call mpas_dmpar_gpu_init_2d_real_coupler(field % array, cpl % pairings(handle) % commArrays(idx) % compactHaloInfo, &
                                                                cpl % pairings(handle) % commArrays(idx) % sendCommList, &
                                                                cpl % pairings(handle) % commArrays(idx) % recvCommList, &
                                                                cpl % pairings(handle) % commArrays(idx) % gpu_bufferOffset_send, &
                                                                cpl % pairings(handle) % commArrays(idx) % gpu_nList_send, &
                                                                cpl % pairings(handle) % commArrays(idx) % gpu_idx_send, &
                                                                cpl % pairings(handle) % commArrays(idx) % gpu_bufferOffset_recv, &
                                                                cpl % pairings(handle) % commArrays(idx) % gpu_nList_recv, &
                                                                cpl % pairings(handle) % commArrays(idx) % gpu_idx_recv, &
                                                                cpl % pairings(handle) % commArrays(idx) % gpu_dimsizes, &
                                                                cpl % pairings(handle) % commArrays(idx) % gpu_nHaloLayers )

            cpl % pairings(handle) % commArrays(idx) % gpu_send_size = size(cpl % pairings(handle) % commArrays(idx) % gpu_nList_send)
            cpl % pairings(handle) % commArrays(idx) % gpu_recv_size = size(cpl % pairings(handle) % commArrays(idx) % gpu_nList_recv)
            cpl % pairings(handle) % commArrays(idx) % gpu_compactHaloInfoSize = size(cpl % pairings(handle) % commArrays(idx) % compactHaloInfo)
            cpl % pairings(handle) % commArrays(idx) % gpuArraySize(1) = size(field % array, 1)
            cpl % pairings(handle) % commArrays(idx) % gpuArraySize(2) = size(field % array, 2)
        end if


        call mpas_dmpar_exch_halo_coupler(field % array, cpl % pairings(handle) % commArrays(idx) % compactHaloInfo, &
                                                     cpl % pairings(handle) % commArrays(idx) % sendCommList, &
                                                     cpl % pairings(handle) % commArrays(idx) % recvCommList, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpu_bufferOffset_send, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpu_nList_send, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpu_idx_send, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpu_bufferOffset_recv, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpu_nList_recv, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpu_idx_recv, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpu_dimsizes, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpu_nHaloLayers, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpu_send_size, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpu_recv_size, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpu_compactHaloInfoSize, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpuArraySize(1), &
                                                     cpl % pairings(handle) % commArrays(idx) % gpuArraySize(2))
        field % block % domain % dminfo => dminfo_save
        field % sendList => sendList_save
        field % recvList => recvList_save
        field % copyList => copyList_save

        if (present(ierr)) then
!TODO: define error codes for intracoupler
            ierr = 0
        end if

    end subroutine MPAS_coupler_transfer_field_r2


    !-----------------------------------------------------------------------
    !  routine MPAS_coupler_transfer_field_r3
    !
    !> \brief  Establish communication pathways to or from another role
    !> \author Michael Duda
    !> \date   3 January 2018
    !> \details
    !>  Returns a handle to a "pairing" of this role with another role, with
    !>  the ability to either send to or receive from the paired role, depending
    !>  on the value of the "direction" argument.
    !
    !-----------------------------------------------------------------------
    subroutine MPAS_coupler_transfer_field_r3(cpl, handle, field, ierr)

        use mpas_derived_types, only : field3DReal, dm_info, mpas_multihalo_exchange_list
        use mpas_dmpar, only : mpas_dmpar_gpu_init_3d_real_coupler, mpas_dmpar_compact_halo_info, mpas_dmpar_exch_halo_coupler

        implicit none

        type (mpas_coupler_type), intent(inout) :: cpl
        integer, intent(in) :: handle
        type (field3DReal), pointer :: field
        integer, intent(out), optional :: ierr

        integer :: idx
        type (dm_info), pointer :: dminfo_save
        type (mpas_multihalo_exchange_list), pointer :: sendList_save
        type (mpas_multihalo_exchange_list), pointer :: recvList_save
        type (mpas_multihalo_exchange_list), pointer :: copyList_save


        !
        ! Make sure handle is valid
        !
        if (handle <= 0 .or. handle > size(cpl % pairings)) then
            if (present(ierr)) then
                ierr = 1
            end if
            return
        else if (.not. cpl % pairings(handle) % isValid) then
            if (present(ierr)) then
                ierr = 1
            end if
            return
        end if

        !
        ! Find index of communication structure appropriate for the dimensionality of this field
        !
        idx = 1
        do while (idx <= size(cpl % pairings(handle) % commArrays) .and. &
                  maxval(cpl % pairings(handle) % commArrays(idx) % dims(:)) > -1)
           if (cpl % pairings(handle) % commArrays(idx) % dims(1) == field % dimsizes(1) .and. &
               cpl % pairings(handle) % commArrays(idx) % dims(2) == field % dimsizes(2) .and. &
               cpl % pairings(handle) % commArrays(idx) % dims(3) == field % dimsizes(3) .and. &
               cpl % pairings(handle) % commArrays(idx) % dims(4) == -1) then
              exit
           end if
           idx = idx + 1
        end do

        !
        ! Verify that we have not run out of available commArrays
        !
        if (idx > size(cpl % pairings(handle) % commArrays)) then
            if (present(ierr)) then
                ierr = 1
                return
            end if
        end if

        dminfo_save => field % block % domain % dminfo
        sendList_save => field % sendList
        recvList_save => field % recvList
        copyList_save => field % copyList

        field % block % domain % dminfo => cpl % pairings(handle) % dminfo
        field % sendList => cpl % pairings(handle) % sendList
        field % recvList => cpl % pairings(handle) % recvList
        field % copyList => cpl % pairings(handle) % copyList


        !
        ! If all of the dimensions in the commArrays structure are -1, the members of commArrays have
        ! not yet been initialized with a call to mpas_dmpar_compact_halo_info
        !
        if (maxval(cpl % pairings(handle) % commArrays(idx) % dims(:)) == -1) then
            call mpas_dmpar_compact_halo_info(field, cpl % pairings(handle) % commArrays(idx) % compactHaloInfo)
            cpl % pairings(handle) % commArrays(idx) % dims(1) = field % dimsizes(1)
            cpl % pairings(handle) % commArrays(idx) % dims(2) = field % dimsizes(2)
            cpl % pairings(handle) % commArrays(idx) % dims(3) = field % dimsizes(3)

            call mpas_dmpar_gpu_init_3d_real_coupler(field % array, cpl % pairings(handle) % commArrays(idx) % compactHaloInfo, &
                                                                cpl % pairings(handle) % commArrays(idx) % sendCommList, &
                                                                cpl % pairings(handle) % commArrays(idx) % recvCommList, &
                                                                cpl % pairings(handle) % commArrays(idx) % gpu_bufferOffset_send, &
                                                                cpl % pairings(handle) % commArrays(idx) % gpu_nList_send, &
                                                                cpl % pairings(handle) % commArrays(idx) % gpu_idx_send, &
                                                                cpl % pairings(handle) % commArrays(idx) % gpu_bufferOffset_recv, &
                                                                cpl % pairings(handle) % commArrays(idx) % gpu_nList_recv, &
                                                                cpl % pairings(handle) % commArrays(idx) % gpu_idx_recv, &
                                                                cpl % pairings(handle) % commArrays(idx) % gpu_dimsizes, &
                                                                cpl % pairings(handle) % commArrays(idx) % gpu_nHaloLayers )

            cpl % pairings(handle) % commArrays(idx) % gpu_send_size = size(cpl % pairings(handle) % commArrays(idx) % gpu_nList_send)
            cpl % pairings(handle) % commArrays(idx) % gpu_recv_size = size(cpl % pairings(handle) % commArrays(idx) % gpu_nList_recv)
            cpl % pairings(handle) % commArrays(idx) % gpu_compactHaloInfoSize = size(cpl % pairings(handle) % commArrays(idx) % compactHaloInfo)
            cpl % pairings(handle) % commArrays(idx) % gpuArraySize(1) = size(field % array, 1)
            cpl % pairings(handle) % commArrays(idx) % gpuArraySize(2) = size(field % array, 2)
            cpl % pairings(handle) % commArrays(idx) % gpuArraySize(3) = size(field % array, 3)
        end if




        call mpas_dmpar_exch_halo_coupler(field % array, cpl % pairings(handle) % commArrays(idx) % compactHaloInfo, &
                                                     cpl % pairings(handle) % commArrays(idx) % sendCommList, &
                                                     cpl % pairings(handle) % commArrays(idx) % recvCommList, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpu_bufferOffset_send, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpu_nList_send, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpu_idx_send, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpu_bufferOffset_recv, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpu_nList_recv, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpu_idx_recv, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpu_dimsizes, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpu_nHaloLayers, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpu_send_size, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpu_recv_size, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpu_compactHaloInfoSize, &
                                                     cpl % pairings(handle) % commArrays(idx) % gpuArraySize(1), &
                                                     cpl % pairings(handle) % commArrays(idx) % gpuArraySize(2), &
                                                     cpl % pairings(handle) % commArrays(idx) % gpuArraySize(3))

        field % block % domain % dminfo => dminfo_save
        field % sendList => sendList_save
        field % recvList => recvList_save
        field % copyList => copyList_save

        if (present(ierr)) then
!TODO: define error codes for intracoupler
            ierr = 0
        end if

    end subroutine MPAS_coupler_transfer_field_r3

end module mpas_intracoupler
