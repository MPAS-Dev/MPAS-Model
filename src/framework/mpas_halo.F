#define CONTIGUOUS contiguous,

! Copyright (c) 2021-2023 The University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at https://mpas-dev.github.io/license.html .
!
!-----------------------------------------------------------------------
!  mpas_halo
!
!> \brief Communication of halos for groups of fields
!> \author Michael Duda
!> \date   29 September 2021
!> \details
!>  This module provides routines for defining groups of fields, and for
!>  communicating the halos of all fields in a group.
!
!-----------------------------------------------------------------------
module mpas_halo

    implicit none

    private

    public :: mpas_halo_init, &
              mpas_halo_finalize, &
              mpas_halo_exch_group_create, &
              mpas_halo_exch_group_complete, &
              mpas_halo_exch_group_destroy, &
              mpas_halo_exch_group_add_field, &
              mpas_halo_exch_group_full_halo_exch


    contains


    !-----------------------------------------------------------------------
    !  routine mpas_halo_init
    !
    !> \brief Initialize halo exchange module
    !> \author Michael Duda
    !> \date   17 November 2021
    !> \details
    !>  This routine initialize the halo exchange module and must be
    !>  called before any other routine for building or exchanging halos.
    !
    !-----------------------------------------------------------------------
    subroutine mpas_halo_init(domain, iErr)

        use mpas_derived_types, only : domain_type
        use mpas_pool_routines, only : mpas_pool_create_pool

        ! Arguments
        type (domain_type), intent(inout) :: domain
        integer, optional, intent(out) :: iErr


        if (present(iErr)) then
           iErr = 0
        end if

        call mpas_pool_create_pool(domain % haloGroupPool)

    end subroutine mpas_halo_init


    !-----------------------------------------------------------------------
    !  routine mpas_halo_finalize
    !
    !> \brief Finalize halo exchange module
    !> \author Michael Duda
    !> \date   17 November 2021
    !> \details
    !>  This routine finalize the halo exchange module and must be
    !>  called after all other calls for building or exchanging halos.
    !
    !-----------------------------------------------------------------------
    subroutine mpas_halo_finalize(domain, iErr)

        use mpas_derived_types, only : domain_type
        use mpas_pool_routines, only : mpas_pool_destroy_pool

        ! Arguments
        type (domain_type), intent(inout) :: domain
        integer, optional, intent(out) :: iErr


        if (present(iErr)) then
           iErr = 0
        end if

        call mpas_pool_destroy_pool(domain % haloGroupPool)

    end subroutine mpas_halo_finalize


    !-----------------------------------------------------------------------
    !  routine mpas_halo_exch_group_create
    !
    !> \brief Create a new group, to which fields can be later added
    !> \author Michael Duda
    !> \date   17 November 2021
    !> \details
    !>  This routine creates a new group, into which fields can be added by
    !>  subsequent calls to mpas_halo_exch_group_add_field. After one or more
    !>  fields have been added to a group created by this routine, a call to
    !>  mpas_halo_exch_group_complete must be made before the halos of fields
    !>  in the group can be exchanged with a call to
    !>  mpas_halo_exch_group_full_halo_exch.
    !
    !-----------------------------------------------------------------------
    subroutine mpas_halo_exch_group_create(domain, groupName, iErr)

        use mpas_derived_types, only : domain_type, mpas_pool_type
        use mpas_pool_routines, only : mpas_pool_create_pool, mpas_pool_add_subpool

        ! Arguments
        type (domain_type), intent(inout) :: domain
        character (len=*), intent(in) :: groupName
        integer, optional, intent(out) :: iErr

        ! Local variables
        type (mpas_pool_type), pointer :: newGroup


        if (present(iErr)) then
           iErr = 0
        end if

        call mpas_pool_create_pool(newGroup)
        call mpas_pool_add_subpool(domain % haloGroupPool, groupName, newGroup)

    end subroutine mpas_halo_exch_group_create


    !-----------------------------------------------------------------------
    !  routine mpas_halo_exch_group_complete
    !
    !> \brief Complete the creation of an exchange group
    !> \author Michael Duda
    !> \date   29 September 2021
    !> \details
    !>  Complete the creation of an exchange group that was defined via a call
    !>  to the mpas_halo_exch_group_create routine, and to which fields were
    !>  added through calls to mpas_halo_exch_group_add_field. This routine
    !>  must be called for an exchange group before the group can be used in
    !>  calls to the mpas_halo_exch_group_full_halo_exch routine.
    !
    !-----------------------------------------------------------------------
    subroutine mpas_halo_exch_group_complete(domain, groupName, iErr)

        use mpas_derived_types, only : domain_type, mpas_pool_type, mpas_pool_iterator_type, MPAS_POOL_CONFIG, &
                                       MPAS_POOL_REAL, MPAS_HALO_REAL, mpas_halo_group, MPAS_LOG_CRIT, &
                                       field1DReal, field2DReal, field3DReal
        use mpas_pool_routines, only : mpas_pool_get_subpool, mpas_pool_remove_subpool, mpas_pool_destroy_pool, &
                                       mpas_pool_begin_iteration, mpas_pool_get_next_member, mpas_pool_get_dimension, &
                                       mpas_pool_remove_field, mpas_pool_get_field
        use mpas_log, only : mpas_log_write

        ! Arguments
        type (domain_type), intent(inout) :: domain
        character (len=*), intent(in) :: groupName
        integer, optional, intent(out) :: iErr

        ! Local variables
        integer :: i
        type (mpas_pool_type), pointer :: completedGroup
        type (mpas_pool_iterator_type) :: itr
        integer, dimension(:), pointer :: fieldHaloInfo
        integer, dimension(:), pointer :: haloLayers
        type (mpas_halo_group), pointer :: newGroup
        type (field1DReal), pointer :: r1d
        type (field2DReal), pointer :: r2d
        type (field3DReal), pointer :: r3d
        integer, pointer :: timeLevel


        if (present(iErr)) then
           iErr = 0
        end if

        call mpas_pool_get_subpool(domain % haloGroupPool, groupName, completedGroup)

        !
        ! Add new mpas_halo_group to list of haloGroups in domain
        !
        allocate(newGroup)
        newGroup % groupName = groupName
        newGroup % next => domain % haloGroups
        domain % haloGroups => newGroup

        !
        ! Figure out how many fields are in this group
        !
        newGroup % nFields = 0
        call mpas_pool_begin_iteration(completedGroup)
        do while (mpas_pool_get_next_member(completedGroup, itr))
            if (itr % memberType == MPAS_POOL_CONFIG) then
                newGroup % nFields = newGroup % nFields + 1
            end if
        end do

        allocate(newGroup % fields(newGroup % nFields))

        !
        ! Fill in field entries for this group
        !
        i = 1
        call mpas_pool_begin_iteration(completedGroup)
        do while (mpas_pool_get_next_member(completedGroup, itr))
            if (itr % memberType == MPAS_POOL_CONFIG) then
                newGroup % fields(i) % fieldName = trim(itr % memberName)

                call mpas_pool_get_dimension(completedGroup, trim(itr % memberName)//'.info', fieldHaloInfo)

                call mpas_pool_get_dimension(completedGroup, trim(itr % memberName)//'.timelevel', timeLevel)

                call mpas_pool_get_dimension(completedGroup, trim(itr % memberName)//'.layers', haloLayers)

                newGroup % fields(i) % nDims = fieldHaloInfo(2)
                newGroup % fields(i) % timeLevel = timeLevel

                select case (fieldHaloInfo(1))
                case (MPAS_POOL_REAL)
                    newGroup % fields(i) % fieldType = MPAS_HALO_REAL
                case default
                    call mpas_log_write('Only real-valued fields are supported in mpas_halo_exch_group_complete', &
                                        messageType=MPAS_LOG_CRIT)
                end select

                if (fieldHaloInfo(1) == MPAS_POOL_REAL) then
                    if (fieldHaloInfo(2) == 1) then
                        call mpas_pool_get_field(completedGroup, trim(itr % memberName)//'.field', r1d)
                        call mpas_halo_compact_halo_info(domain, r1d % sendList, r1d % recvList, r1d % dimSizes, &
                                                         haloLayers, &
                                                         newGroup % fields(i) % compactHaloInfo, &
                                                         newGroup % fields(i) % compactSendLists, &
                                                         newGroup % fields(i) % compactRecvLists)
                    else if (fieldHaloInfo(2) == 2) then
                        call mpas_pool_get_field(completedGroup, trim(itr % memberName)//'.field', r2d)
                        call mpas_halo_compact_halo_info(domain, r2d % sendList, r2d % recvList, r2d % dimSizes, &
                                                         haloLayers, &
                                                         newGroup % fields(i) % compactHaloInfo, &
                                                         newGroup % fields(i) % compactSendLists, &
                                                         newGroup % fields(i) % compactRecvLists)
                    else if (fieldHaloInfo(2) == 3) then
                        call mpas_pool_get_field(completedGroup, trim(itr % memberName)//'.field', r3d)
                        call mpas_halo_compact_halo_info(domain, r3d % sendList, r3d % recvList, r3d % dimSizes, &
                                                         haloLayers, &
                                                         newGroup % fields(i) % compactHaloInfo, &
                                                         newGroup % fields(i) % compactSendLists, &
                                                         newGroup % fields(i) % compactRecvLists)
                    else
                        call mpas_log_write('Unsupported dimensionality for real field in mpas_halo_exch_group_complete.', &
                                            messageType=MPAS_LOG_CRIT)
                    end if
                else
                    call mpas_log_write('Unsupported field type in mpas_halo_exch_group_complete.', &
                                        messageType=MPAS_LOG_CRIT)
                end if

                call mpas_pool_remove_field(completedGroup, trim(itr % memberName)//'.field')
                i = i + 1
            end if
        end do

        call mpas_halo_aggregate_group_info(newGroup)

        !
        ! Pre-allocate buffers and MPI request lists
        !
        allocate(newGroup % sendBuf(newGroup % groupSendBufSize))
        allocate(newGroup % recvBuf(newGroup % groupRecvBufSize))
        allocate(newGroup % sendRequests(newGroup % nGroupSendNeighbors))
        allocate(newGroup % recvRequests(newGroup % nGroupRecvNeighbors))

        call mpas_pool_destroy_pool(completedGroup)
        call mpas_pool_remove_subpool(domain % haloGroupPool, groupName)

        call refactor_lists(domain, groupName, iErr)

    end subroutine mpas_halo_exch_group_complete


    !-----------------------------------------------------------------------
    !  routine mpas_halo_exch_group_destroy
    !
    !> \brief Destroys a halo exchange group
    !> \author Michael Duda
    !> \date   17 November 2021
    !> \details
    !>  This routine frees memory associated with the named group, which must
    !>  have been previously created with calls to mpas_halo_exch_group_create
    !>  and mpas_halo_exch_group_complete.
    !
    !-----------------------------------------------------------------------
    subroutine mpas_halo_exch_group_destroy(domain, groupName, iErr)

        use mpas_derived_types, only : domain_type, mpas_halo_group, MPAS_LOG_CRIT
        use mpas_log, only : mpas_log_write

        ! Arguments
        type (domain_type), intent(inout) :: domain
        character (len=*), intent(in) :: groupName
        integer, optional, intent(out) :: iErr

        ! Local variables
        integer :: i
        type (mpas_halo_group), pointer :: cursor, prev


        if (present(iErr)) then
           iErr = 0
        end if

        !
        ! Find this halo exhange group in the list of groups
        !
        nullify(prev)
        cursor => domain % haloGroups
        do while (associated(cursor))
            if (trim(cursor % groupName) == trim(groupName)) then
                exit
            end if

            prev => cursor
            cursor => cursor % next
        end do

        if (.not. associated(cursor)) then
            call mpas_log_write('Halo exchange group '//trim(groupName)//' not found in destroy routine.', &
                                messageType=MPAS_LOG_CRIT)
        end if

        !
        ! Unlink this exchange group
        !
        if (.not. associated(prev)) then
            domain % haloGroups => cursor % next
        else
            prev % next => cursor % next
        end if

        !
        ! Deallocate this exchange group
        !
        do i = 1, cursor % nFields
            deallocate(cursor % fields(i) % compactHaloInfo)
            deallocate(cursor % fields(i) % compactSendLists)
            deallocate(cursor % fields(i) % compactRecvLists)
            deallocate(cursor % fields(i) % nSendLists)
            deallocate(cursor % fields(i) % sendListSrc)
            deallocate(cursor % fields(i) % sendListDst)
            deallocate(cursor % fields(i) % packOffsets)
            deallocate(cursor % fields(i) % nRecvLists)
            deallocate(cursor % fields(i) % recvListSrc)
            deallocate(cursor % fields(i) % recvListDst)
            deallocate(cursor % fields(i) % unpackOffsets)
        end do
        deallocate(cursor % fields)
        deallocate(cursor % groupPackOffsets)
        deallocate(cursor % groupSendNeighbors)
        deallocate(cursor % groupSendOffsets)
        deallocate(cursor % groupSendCounts)
        deallocate(cursor % groupUnpackOffsets)
        deallocate(cursor % groupRecvNeighbors)
        deallocate(cursor % groupToFieldRecvIdx)
        deallocate(cursor % groupRecvOffsets)
        deallocate(cursor % groupRecvCounts)
        deallocate(cursor % sendBuf)
        deallocate(cursor % recvBuf)
        deallocate(cursor % sendRequests)
        deallocate(cursor % recvRequests)
        deallocate(cursor)

    end subroutine mpas_halo_exch_group_destroy


    !-----------------------------------------------------------------------
    !  routine mpas_halo_exch_group_add_field
    !
    !> \brief Add a field to a halo exchange group
    !> \author Michael Duda
    !> \date   17 November 2021
    !> \details
    !>  This routine adds the field named fieldName to the exchange group named
    !>  groupName. The timeLevel argument provides control over which time level
    !>  will be exchanged for the field. If the timeLevel argument is omitted,
    !>  time level 1 of the field will be exchanged. The haloLayers argument
    !>  specifies which halo layers will be exchanged for the field; if haloLayers
    !>  is not specified, all halo layers will be exchanged.
    !
    !-----------------------------------------------------------------------
    subroutine mpas_halo_exch_group_add_field(domain, groupName, fieldName, timeLevel, haloLayers, iErr)

        use mpas_derived_types, only : domain_type, mpas_pool_type, mpas_pool_field_info_type, MPAS_POOL_REAL, &
                                       field1DReal, field2DReal, field3DReal, MPAS_LOG_CRIT
        use mpas_pool_routines, only : mpas_pool_get_subpool, mpas_pool_add_config, mpas_pool_get_field_info, &
                                       mpas_pool_add_dimension, mpas_pool_get_field, mpas_pool_add_field
        use mpas_log, only : mpas_log_write

        ! Arguments
        type (domain_type), intent(inout) :: domain
        character (len=*), intent(in) :: groupName
        character (len=*), intent(in) :: fieldName
        integer, optional, intent(in) :: timeLevel
        integer, dimension(:), optional, intent(in) :: haloLayers
        integer, optional, intent(out) :: iErr

        ! Local variables
        type (mpas_pool_type), pointer :: group
        type (mpas_pool_field_info_type) :: info
        integer, allocatable, dimension(:) :: fieldHaloInfo
        integer :: local_timeLevel
        type (field1DReal), pointer :: r1d
        type (field2DReal), pointer :: r2d
        type (field3DReal), pointer :: r3d


        if (present(iErr)) then
           iErr = 0
        end if

        call mpas_pool_get_subpool(domain % haloGroupPool, groupName, group)

        !
        ! Store an item in the pool to signal the field whose halo is to be exchanged
        !
        call mpas_pool_add_config(group, fieldName, 1)

        call mpas_pool_get_field_info(domain % blocklist % allFields, fieldName, info)

        !
        ! Store an item in the pool with basic info about this field
        !
        allocate(fieldHaloInfo(4))
        fieldHaloInfo(1) = info % fieldType
        fieldHaloInfo(2) = info % nDims
        fieldHaloInfo(3) = info % nTimeLevels
        fieldHaloInfo(4) = info % nHaloLayers
        call mpas_pool_add_dimension(group, fieldName//'.info', fieldHaloInfo)
        deallocate(fieldHaloInfo)

        !
        ! Store an item in the pool with list of halo layers to exchange, or (/-1/) if all layers
        !
        if (present(haloLayers)) then
            call mpas_pool_add_dimension(group, fieldName//'.layers', haloLayers)
        else
            call mpas_pool_add_dimension(group, fieldName//'.layers', (/ -1 /))
        end if

        !
        ! Store an item in the pool indicating which time level to exchange
        !
        if (present(timeLevel)) then
            local_timeLevel = timeLevel
        else
            local_timeLevel = 1
        end if
        call mpas_pool_add_dimension(group, fieldName//'.timelevel', local_timeLevel)

        !
        ! Store a reference to the field itself in the pool
        !
        if (info % fieldType == MPAS_POOL_REAL) then
            if (info % nDims == 1) then
                call mpas_pool_get_field(domain % blocklist % allFields, fieldName, r1d, timeLevel=local_timeLevel)
                call mpas_pool_add_field(group, fieldName//'.field', r1d)
            else if (info % nDims == 2) then
                call mpas_pool_get_field(domain % blocklist % allFields, fieldName, r2d, timeLevel=local_timeLevel)
                call mpas_pool_add_field(group, fieldName//'.field', r2d)
            else if (info % nDims == 3) then
                call mpas_pool_get_field(domain % blocklist % allFields, fieldName, r3d, timeLevel=local_timeLevel)
                call mpas_pool_add_field(group, fieldName//'.field', r3d)
            else
                call mpas_log_write('Unsupported dimensionality for real field '//trim(fieldName), messageType=MPAS_LOG_CRIT)
            end if
        else
            call mpas_log_write('Unsupported field type for field '//trim(fieldName), messageType=MPAS_LOG_CRIT)
        end if

    end subroutine mpas_halo_exch_group_add_field


    !-----------------------------------------------------------------------
    !  routine mpas_halo_exch_group_full_halo_exch
    !
    !> \brief Communicate halos for all fields in a group
    !> \author Michael Duda
    !> \date   15 August 2022
    !> \details
    !>  This routine exchanges the halos for all fields in the named halo
    !>  exchange group.
    !
    !-----------------------------------------------------------------------
    subroutine mpas_halo_exch_group_full_halo_exch(domain, groupName, iErr)

#ifdef MPAS_USE_MPI_F08
        use mpi_f08, only : MPI_Datatype, MPI_Comm
        use mpi_f08, only : MPI_REAL, MPI_DOUBLE_PRECISION, MPI_REQUEST_NULL, &
                            MPI_STATUS_IGNORE, MPI_STATUSES_IGNORE
        use mpi_f08, only : MPI_Irecv, MPI_Isend, MPI_Waitany, MPI_Waitall
#else
        use mpi
#endif
        use mpas_derived_types, only : domain_type, mpas_halo_group, MPAS_HALO_REAL, MPAS_LOG_CRIT
        use mpas_pool_routines, only : mpas_pool_get_array
        use mpas_log, only : mpas_log_write

        ! Parameters
#ifdef MPAS_USE_MPI_F08
#ifdef SINGLE_PRECISION
        type (MPI_Datatype), parameter :: MPI_REALKIND = MPI_REAL
#else
        type (MPI_Datatype), parameter :: MPI_REALKIND = MPI_DOUBLE_PRECISION
#endif
#else
#ifdef SINGLE_PRECISION
        integer, parameter :: MPI_REALKIND = MPI_REAL
#else
        integer, parameter :: MPI_REALKIND = MPI_DOUBLE_PRECISION
#endif
#endif

        ! Arguments
        type (domain_type), intent(inout) :: domain
        character (len=*), intent(in) :: groupName
        integer, optional, intent(out) :: iErr

        ! Local variables
        integer :: i, bufstart, bufend
        integer :: dim1, dim2
        integer :: i1, i2, j, iNeighbor, iReq
        integer :: iHalo, iEndp
        integer :: nHalos, nSendEndpts, nRecvEndpts
        integer :: rank
#ifdef MPAS_USE_MPI_F08
        type (MPI_Comm) :: comm
#else
        integer :: comm
#endif
        integer :: mpi_ierr
        type (mpas_halo_group), pointer :: group
        integer, dimension(:), pointer :: compactHaloInfo
        integer, dimension(:), pointer :: compactSendLists
        integer, dimension(:), pointer :: compactRecvLists
        integer, dimension(:,:), CONTIGUOUS pointer :: nSendLists
        integer :: maxNSendList
        integer, dimension(:,:,:), CONTIGUOUS pointer :: sendListSrc, sendListDst
        integer, dimension(:), CONTIGUOUS pointer :: packOffsets
        integer, dimension(:,:), CONTIGUOUS pointer :: nRecvLists
        integer :: maxNRecvList
        integer, dimension(:,:,:), CONTIGUOUS pointer :: recvListSrc, recvListDst
        integer, dimension(:), CONTIGUOUS pointer :: unpackOffsets


        if (present(iErr)) then
           iErr = 0
        end if

        !
        ! Find this halo exhange group in the list of groups
        !
        group => domain % haloGroups
        do while (associated(group))
            if (trim(group % groupName) == trim(groupName)) then
                exit
            end if

            group => group % next
        end do

        if (.not. associated(group)) then
            call mpas_log_write('Halo exchange group '//trim(groupName)//' not found in full_exch routine.', &
                                messageType=MPAS_LOG_CRIT)
        end if

        !
        ! Get the rank of this task and the MPI communicator to use from the first field in
        ! the group; all fields should be using the same communicator, so this should not
        ! be problematic
        !
#ifdef MPAS_USE_MPI_F08
        comm % mpi_val = group % fields(1) % compactHaloInfo(7)
#else
        comm = group % fields(1) % compactHaloInfo(7)
#endif
        rank = group % fields(1) % compactHaloInfo(8)


        !
        ! Initiate non-blocking MPI receives for all neighbors
        !
        do i = 1, group % nGroupRecvNeighbors
            if (group % groupRecvCounts(i) > 0) then
                bufstart = group % groupRecvOffsets(i)
                bufend = group % groupRecvOffsets(i) + group % groupRecvCounts(i) - 1
!TO DO: how do we determine appropriate type here?
                call MPI_Irecv(group % recvBuf(bufstart:bufend), group % groupRecvCounts(i), MPI_REALKIND, &
                               group % groupRecvNeighbors(i), group % groupRecvNeighbors(i), comm, &
                               group % recvRequests(i), mpi_ierr)
            else
                group % recvRequests(i) = MPI_REQUEST_NULL
            end if
        end do

        !
        ! Pack the segmented send buffer with elements from all fields and for all neighbors
        !
        do i = 1, group % nFields

            compactHaloInfo => group % fields(i) % compactHaloInfo
            compactSendLists => group % fields(i) % compactSendLists

            !
            ! Packing code for real-valued fields
            !
            if (group % fields(i) % fieldType == MPAS_HALO_REAL) then
                dim1 = compactHaloInfo(2)
                dim2 = compactHaloInfo(3)

                nHalos = compactHaloInfo(9)
                nSendEndpts = compactHaloInfo(10)

                sendListSrc => group % fields(i) % sendListSrc
                sendListDst => group % fields(i) % sendListDst
                nSendLists => group % fields(i) % nSendLists
                packOffsets => group % fields(i) % packOffsets
                maxNSendList = group % fields(i) % maxNSendList

                select case (group % fields(i) % nDims)

                !
                ! Packing code for 1-d real-valued fields
                !
                case (1)
                    call mpas_pool_get_array(domain % blocklist % allFields, trim(group % fields(i) % fieldName), &
                                             group % fields(i) % r1arr, timeLevel=group % fields(i) % timeLevel)

                    !
                    ! Pack send buffer for all neighbors for current field
                    !
                    do iEndp = 1, nSendEndpts
                        do iHalo = 1, nHalos
                            do j = 1, maxNSendList
                                if (j <= nSendLists(iHalo,iEndp)) then
                                    group % sendBuf(packOffsets(iEndp) + sendListDst(j,iHalo,iEndp)) = &
                                        group % fields(i) % r1arr(sendListSrc(j,iHalo,iEndp))
                                end if
                            end do
                        end do
                    end do

                !
                ! Packing code for 2-d real-valued fields
                !
                case (2)
                    call mpas_pool_get_array(domain % blocklist % allFields, trim(group % fields(i) % fieldName), &
                                             group % fields(i) % r2arr, timeLevel=group % fields(i) % timeLevel)

                    !
                    ! Pack send buffer for all neighbors for current field
                    !
                    do iEndp = 1, nSendEndpts
                        do iHalo = 1, nHalos
                            do j = 1, maxNSendList
                                do i1 = 1, dim1
                                    if (j <= nSendLists(iHalo,iEndp)) then
                                        group % sendBuf(packOffsets(iEndp) + dim1 * (sendListDst(j,iHalo,iEndp) - 1) + i1) = &
                                            group % fields(i) % r2arr(i1, sendListSrc(j,iHalo,iEndp))
                                    end if
                                end do
                            end do
                        end do
                    end do

                !
                ! Packing code for 3-d real-valued fields
                !
                case (3)
                    call mpas_pool_get_array(domain % blocklist % allFields, trim(group % fields(i) % fieldName), &
                                             group % fields(i) % r3arr, group % fields(i) % timeLevel)

                    !
                    ! Pack send buffer for all neighbors for current field
                    !
                    do iEndp = 1, nSendEndpts
                        do iHalo = 1, nHalos
                            do j = 1, maxNSendList
                                do i2 = 1, dim2
                                do i1 = 1, dim1
                                    if (j <= nSendLists(iHalo,iEndp)) then
                                        group % sendBuf(packOffsets(iEndp) + dim1*dim2*(sendListDst(j,iHalo,iEndp) - 1) &
                                                        + dim1*(i2-1) + i1) = &
                                            group % fields(i) % r3arr(i1, i2, sendListSrc(j,iHalo,iEndp))
                                    end if
                                end do
                                end do
                            end do
                        end do
                    end do

                end select
            end if
        end do

        !
        ! Initiate non-blocking sends to all neighbors
        !
        do i = 1, group % nGroupSendNeighbors
            if (group % groupSendCounts(i) > 0) then
                bufstart = group % groupSendOffsets(i)
                bufend = group % groupSendOffsets(i) + group % groupSendCounts(i) - 1
!TO DO: how do we determine appropriate type here?
                call MPI_Isend(group % sendBuf(bufstart:bufend), group % groupSendCounts(i), MPI_REALKIND, &
                               group % groupSendNeighbors(i), rank, comm, &
                               group % sendRequests(i), mpi_ierr)
            else
                group % sendRequests(i) = MPI_REQUEST_NULL
            end if
        end do

        !
        ! Unpack messages as they are received
        !
        do iNeighbor = 1, group % nGroupRecvNeighbors

            call MPI_Waitany(group % nGroupRecvNeighbors, group % recvRequests, iReq, MPI_STATUS_IGNORE, mpi_ierr)

            !
            ! Unpack the segmented recv buffer with elements for all fields and from all neighbors
            !
            do i = 1, group % nFields

                ! Find field-local neighbor index corresponding to the neighbor for which we
                ! just received a message. If iEndp == 0, then field i does not receive any
                ! values from neighbor iReq
                iEndp = group % groupToFieldRecvIdx(iReq, i)

                if (iEndp == 0) cycle     ! No unpacking needed from this neighbor for this field


                compactHaloInfo => group % fields(i) % compactHaloInfo
                compactRecvLists => group % fields(i) % compactRecvLists

                nHalos = compactHaloInfo(9)
                nRecvEndpts = compactHaloInfo(11)

                recvListSrc => group % fields(i) % recvListSrc
                recvListDst => group % fields(i) % recvListDst
                nRecvLists => group % fields(i) % nRecvLists
                unpackOffsets => group % fields(i) % unpackOffsets
                maxNRecvList = group % fields(i) % maxNRecvList


                !
                ! Unpacking code for real-valued fields
                !
                if (group % fields(i) % fieldType == MPAS_HALO_REAL) then
                    dim1 = compactHaloInfo(2)
                    dim2 = compactHaloInfo(3)

                    select case (group % fields(i) % nDims)

                    !
                    ! Unpacking code for 1-d real-valued fields
                    !
                    case (1)
                        !
                        ! Unpack recv buffer from all neighbors for current field
                        !
                        do iHalo = 1, nHalos
                            do j = 1, maxNRecvList
                                if (j <= nRecvLists(iHalo,iEndp)) then
                                    group % fields(i) % r1arr(recvListDst(j,iHalo,iEndp)) = &
                                        group % recvBuf(unpackOffsets(iEndp) + recvListSrc(j,iHalo,iEndp))
                                end if
                            end do
                        end do

                    !
                    ! Unpacking code for 2-d real-valued fields
                    !
                    case (2)
                        !
                        ! Unpack recv buffer from all neighbors for current field
                        !
                        do iHalo = 1, nHalos
                            do j = 1, maxNRecvList
                                do i1 = 1, dim1
                                    if (j <= nRecvLists(iHalo,iEndp)) then
                                        group % fields(i) % r2arr(i1, recvListDst(j,iHalo,iEndp)) = &
                                            group % recvBuf(unpackOffsets(iEndp) + dim1 * (recvListSrc(j,iHalo,iEndp) - 1) + i1)
                                    end if
                                end do
                            end do
                        end do

                    !
                    ! Unpacking code for 3-d real-valued fields
                    !
                    case (3)
                        !
                        ! Unpack recv buffer from all neighbors for current field
                        !
                        do iHalo = 1, nHalos
                            do j = 1, maxNRecvList
                                do i2 = 1, dim2
                                do i1 = 1, dim1
                                    if (j <= nRecvLists(iHalo,iEndp)) then
                                        group % fields(i) % r3arr(i1, i2, recvListDst(j,iHalo,iEndp)) = &
                                            group % recvBuf(unpackOffsets(iEndp) + dim1*dim2*(recvListSrc(j,iHalo,iEndp) - 1) &
                                                            + dim1*(i2-1) + i1)
                                    end if
                                end do
                                end do
                            end do
                        end do

                    end select
                end if
            end do
        end do

        !
        ! Nullify array pointers - not necessary for correctness, but helpful when debugging
        ! to not leave pointers to what might later be incorrect targets
        !
        do i = 1, group % nFields
            if (group % fields(i) % nDims == 1) then
                nullify(group % fields(i) % r1arr)
            else if (group % fields(i) % nDims == 2) then
                nullify(group % fields(i) % r2arr)
            else if (group % fields(i) % nDims == 3) then
                nullify(group % fields(i) % r3arr)
            end if
        end do

        !
        ! Wait for all sends to complete before returning
        !
        call MPI_Waitall(group % nGroupSendNeighbors, group % sendRequests, MPI_STATUSES_IGNORE, mpi_ierr)

    end subroutine mpas_halo_exch_group_full_halo_exch


    !-----------------------------------------------------------------------
    !  routine mpas_halo_compact_halo_info
    !
    !> \brief Compacts information needed for halo exchanges
    !> \author Michael Duda
    !> \date 7 December 2017
    !> \details
    !>  This routine extracts all information needed to perform a halo exchange
    !>  from dynamic data types and places it into a single, contiguous array
    !>  for use by the mpas_halo_exch_halo_acc routines.
    !>  The resulting compactHaloInfo array has the following elements:
    !>    1 - The dimensionality of the field
    !>    2 - Dimension 1 of the field (i.e., the left-most dimension)
    !>    3 - Dimension 2 of the field
    !>    4 - Dimension 3 of the field
    !>    5 - Dimension 4 of the field
    !>    6 - Dimension 5 of the field
    !>    7 - The MPI communicator
    !>    8 - The MPI rank of the current process
    !>    9 - The number of halo layers for the field
    !>   10 - The number of endpoints to send to
    !>   11 - The number of endpoints to recv from
    !>
    !>  The compactSendLists and compactRecvLists arrays have the following elements:
    !>   foreach (send endpoint) { endPointID foreach (halolayer) {nList srcList(1:nList) destList(1:nList)} }
    !>   foreach (recv endpoint) { endPointID foreach (halolayer) {nList srcList(1:nList) destList(1:nList)} }
    !
    !-----------------------------------------------------------------------
    subroutine mpas_halo_compact_halo_info(domain, sendList, recvList, dimSizes, haloLayers, &
                                           compactHaloInfo, compactSendLists, compactRecvLists)

        use mpas_derived_types, only : domain_type, mpas_multihalo_exchange_list, mpas_exchange_list, MPAS_LOG_CRIT
        use mpas_log, only : mpas_log_write

        implicit none

        ! Arguments
        type (domain_type), intent(in) :: domain
        type (mpas_multihalo_exchange_list), pointer :: sendList
        type (mpas_multihalo_exchange_list), pointer :: recvList
        integer, dimension(:), intent(in) :: dimsizes
        integer, dimension(:), intent(in) :: haloLayers
        integer, dimension(:), pointer :: compactHaloInfo
        integer, dimension(:), pointer :: compactSendLists
        integer, dimension(:), pointer :: compactRecvLists

        ! Local variables
        integer :: i, iendpt, j, ioffset
        integer :: idx
        integer :: nSendEndpoints
        integer :: maxSendEndpoints
        integer :: totSendListSize
        integer :: nRecvEndpoints
        integer :: maxRecvEndpoints
        integer :: totRecvListSize
        integer :: nHaloLayers
        logical :: found
        integer, allocatable, dimension(:) :: sendEndpoints
        integer, allocatable, dimension(:) :: recvEndpoints
        type (mpas_multihalo_exchange_list), pointer :: sendListCursor, recvListCursor
        type (mpas_exchange_list), pointer :: exchListPtr
        integer :: activeHaloLayers
        logical, dimension(:), allocatable :: useHalo


        !
        ! Find number of halo layers
        !
        nHaloLayers = size(sendList % halos)
        if (nHaloLayers /= size(recvList % halos)) then
            call mpas_log_write('The number of halo layers in the recv list does not match the number in the send list', &
                                messageType=MPAS_LOG_CRIT)
        end if

        !
        ! Create logical array indicating, for each halo layer, whether that halo layer should be
        ! used in a halo exchange
        !
        allocate(useHalo(nHaloLayers))

        if (haloLayers(1) == -1) then     ! Use all halo layers
            useHalo(:) = .true.
            activeHaloLayers = nHaloLayers
        else
            useHalo(:) = .false.
            activeHaloLayers = size(haloLayers)
            do i = 1, activeHaloLayers
                useHalo(haloLayers(i)) = .true.
            end do
        end if

        !
        ! Find the maximum number of "endpoints" that we will need to send to for any halo layer,
        ! as well as the total size of the send lists for all halo layers
        !
        maxSendEndpoints = 0
        totSendListSize = 0
        sendListCursor => sendList
        do while (associated(sendListCursor))
            do i=1,nHaloLayers
                if (useHalo(i)) then
                    nSendEndpoints = 0
                    exchListPtr => sendListCursor % halos(i) % exchList
                    do while (associated(exchListPtr))
                        nSendEndpoints = nSendEndpoints + 1
                        totSendListSize = totSendListSize + 2 * exchListPtr % nList    ! We have srcList and destList
                        exchListPtr => exchListPtr % next
                    end do
                    maxSendEndpoints = max(nSendEndpoints, maxSendEndpoints)
                end if
            end do
            sendListCursor => sendListCursor % next     ! Expected to iterate just once for MPAS-Atmosphere
        end do

        allocate(sendEndpoints(maxSendEndpoints * activeHaloLayers))

        !
        ! Gather a list of the unique endpoints that we will need to send to
        !
        nSendEndpoints = 0
        sendListCursor => sendList
        do while (associated(sendListCursor))
            do i=1,nHaloLayers
                if (useHalo(i)) then
                    exchListPtr => sendListCursor % halos(i) % exchList
                    do while (associated(exchListPtr))

                        ! If the current endpoint is not already in the list, add it
                        do j=1,nSendEndpoints
                            if (exchListPtr % endPointID == sendEndpoints(j)) exit
                        end do
                        if (j > nSendEndpoints) then
                            nSendEndpoints = nSendEndpoints + 1
                            sendEndpoints(nSendEndpoints) = exchListPtr % endPointID
                        end if

                        exchListPtr => exchListPtr % next
                    end do
                end if
            end do
            sendListCursor => sendListCursor % next     ! Expected to iterate just once for MPAS-Atmosphere
        end do

        !
        ! Find the maximum number of "endpoints" that we will need to receive from for any halo layer,
        ! as well as the total size of the recv lists for all halo layers
        !
        maxRecvEndpoints = 0
        totRecvListSize = 0
        recvListCursor => recvList
        do while (associated(recvListCursor))
            do i=1,nHaloLayers
                if (useHalo(i)) then
                    nRecvEndpoints = 0
                    exchListPtr => recvListCursor % halos(i) % exchList
                    do while (associated(exchListPtr))
                        nRecvEndpoints = nRecvEndpoints + 1
                        totRecvListSize = totRecvListSize + 2 * exchListPtr % nList    ! We have srcList and destList
                        exchListPtr => exchListPtr % next
                    end do
                    maxRecvEndpoints = max(nRecvEndpoints, maxRecvEndpoints)
                end if
            end do
            recvListCursor => recvListCursor % next     ! Expected to iterate just once for MPAS-Atmosphere
        end do

        allocate(recvEndpoints(maxRecvEndpoints * activeHaloLayers))

        !
        ! Gather a list of the unique endpoints that we will need to receive from
        !
        nRecvEndpoints = 0
        recvListCursor => recvList
        do while (associated(recvListCursor))
            do i=1,nHaloLayers
                if (useHalo(i)) then
                    exchListPtr => recvListCursor % halos(i) % exchList
                    do while (associated(exchListPtr))

                        ! If the current endpoint is not already in the list, add it
                        do j=1,nRecvEndpoints
                            if (exchListPtr % endPointID == recvEndpoints(j)) exit
                        end do
                        if (j > nRecvEndpoints) then
                            nRecvEndpoints = nRecvEndpoints + 1
                            recvEndpoints(nRecvEndpoints) = exchListPtr % endPointID
                        end if

                        exchListPtr => exchListPtr % next
                    end do
                end if
            end do
            recvListCursor => recvListCursor % next     ! Expected to iterate just once for MPAS-Atmosphere
        end do


        !
        ! Compute the number of elements we will need in compactHaloInfo
        !
        allocate(compactHaloInfo(11))
        allocate(compactSendLists(nSendEndpoints + activeHaloLayers * nSendEndpoints + totSendListSize))
        allocate(compactRecvLists(nRecvEndpoints + activeHaloLayers * nRecvEndpoints + totRecvListSize))

        compactHaloInfo(:) = 0
        compactSendLists(:) = 0
        compactRecvLists(:) = 0

        !
        ! 1-6: Add field dimensionality and dimensions
        !
        compactHaloInfo(1) = size(dimSizes)    ! Dimensionality of the field
        idx = 2
        do i=1,compactHaloInfo(1)
            compactHaloInfo(idx) = dimSizes(i)
            idx = idx + 1
        end do

        !
        ! 7-8: Add MPI info
        !
        idx = 7
#ifdef MPAS_USE_MPI_F08
        compactHaloInfo(idx) = domain % dminfo % comm % mpi_val
#else
        compactHaloInfo(idx) = domain % dminfo % comm
#endif
        idx = idx + 1
        compactHaloInfo(idx) = domain % dminfo % my_proc_id
        idx = idx + 1

        !
        ! 9: Add number of halo layers
        !
        compactHaloInfo(idx) = activeHaloLayers
        idx = idx + 1

        !
        ! 10: Add number of send endpoints
        !
        compactHaloInfo(idx) = nSendEndpoints
        idx = idx + 1

        !
        ! 11: Add number of receive endpoints
        !
        compactHaloInfo(idx) = nRecvEndpoints
        idx = idx + 1

        !
        ! foreach (endpoint) { endPointID foreach (halolayer) {nList srcList(1:nList) destList(1:nList)} }
        !
        idx = 1
        do iendpt=1,nSendEndpoints
            compactSendLists(idx) = sendEndpoints(iendpt)
            idx = idx + 1
            ioffset = 0
            do i=1,nHaloLayers
                if (useHalo(i)) then
                    sendListCursor => sendList
                    do while (associated(sendListCursor))
                        found = .false.
                        exchListPtr => sendListCursor % halos(i) % exchList
                        do while (associated(exchListPtr))
                            if (exchListPtr % endPointID == sendEndpoints(iendpt)) then
                                found = .true.
                                compactSendLists(idx) = exchListPtr % nList
                                idx = idx + 1
                                compactSendLists(idx:idx+exchListPtr % nList-1) = exchListPtr % srcList(1:exchListPtr % nList)
                                idx = idx + exchListPtr % nList
                                compactSendLists(idx:idx+exchListPtr % nList-1) = exchListPtr % destList(1:exchListPtr % nList) + &
                                                                                  ioffset
                                idx = idx + exchListPtr % nList
                                ioffset = ioffset + exchListPtr % nList
                                exit
                            end if
                            exchListPtr => exchListPtr % next
                        end do
                        if (.not. found) then
                            compactSendLists(idx) = 0
                            idx = idx + 1
                        end if
                        sendListCursor => sendListCursor % next     ! Expected to iterate just once for MPAS-Atmosphere
                    end do
                end if
            end do
        end do

        deallocate(sendEndpoints)

        !
        ! foreach (endpoint) { endPointID foreach (halolayer) {nList srcList(1:nList) destList(1:nList)} }
        !
        idx = 1
        do iendpt=1,nRecvEndpoints
            compactRecvLists(idx) = recvEndpoints(iendpt)
            idx = idx + 1
            ioffset = 0
            do i=1,nHaloLayers
                if (useHalo(i)) then
                    recvListCursor => recvList
                    do while (associated(recvListCursor))
                        found = .false.
                        exchListPtr => recvListCursor % halos(i) % exchList
                        do while (associated(exchListPtr))
                            if (exchListPtr % endPointID == recvEndpoints(iendpt)) then
                                found = .true.
                                compactRecvLists(idx) = exchListPtr % nList
                                idx = idx + 1
                                compactRecvLists(idx:idx+exchListPtr % nList-1) = exchListPtr % srcList(1:exchListPtr % nList) + &
                                                                                  ioffset
                                idx = idx + exchListPtr % nList
                                compactRecvLists(idx:idx+exchListPtr % nList-1) = exchListPtr % destList(1:exchListPtr % nList)
                                idx = idx + exchListPtr % nList
                                ioffset = ioffset + exchListPtr % nList
                                exit
                            end if
                            exchListPtr => exchListPtr % next
                        end do
                        if (.not. found) then
                            compactRecvLists(idx) = 0
                            idx = idx + 1
                        end if
                        recvListCursor => recvListCursor % next     ! Expected to iterate just once for MPAS-Atmosphere
                    end do
                end if
            end do
        end do

        deallocate(recvEndpoints)

        deallocate(useHalo)

    end subroutine mpas_halo_compact_halo_info


    !-----------------------------------------------------------------------
    !  routine mpas_halo_aggregate_group_info
    !
    !> \brief Aggregate exchange info from all fields in a halo exchange group
    !> \author Michael Duda
    !> \date   23 November 2021
    !> \details
    !>  Given an mpas_halo_group, this routine aggregates information from across
    !>  all mpas_halo_field members of the group.
    !>  The end result of this routine is a set of arrays that can be used by
    !>  the mpas_halo_exch_group_full_halo_exch routine to pack/unpack buffers
    !>  and launch MPI sends and receives:
    !>
    !>    nGroupSendNeighbors
    !>    groupSendBufSize
    !>    groupPackOffsets
    !>    groupSendNeighbors
    !>    groupSendOffsets
    !>    groupSendCounts
    !>
    !>    nGroupRecvNeighbors
    !>    groupRecvBufSize
    !>    groupUnpackOffsets
    !>    groupRecvNeighbors
    !>    groupToFieldRecvEndpt
    !>    groupRecvOffsets
    !>    groupRecvCounts
    !
    !-----------------------------------------------------------------------
    subroutine mpas_halo_aggregate_group_info(group, ierr)

        use mpas_derived_types, only : mpas_halo_group

        ! Arguments
        type (mpas_halo_group), intent(inout) :: group
        integer, intent(out), optional :: ierr

        ! Local variables
        integer :: i, j, idx, ihalo, iendp, nlist
        integer :: ndims, ninnerelems
        integer :: maxGroupSendNeighbors, maxGroupRecvNeighbors
        integer, allocatable, dimension(:) :: sendNeighbors, recvNeighbors
        integer, allocatable, dimension(:,:) :: sendCounts, recvCounts


        if (present(ierr)) then
            ierr = 0
        end if

        !
        ! Compute an upper bound on the number of send and recv neighbors for this group
        !
        maxGroupSendNeighbors = 0
        maxGroupRecvNeighbors = 0
        do i = 1, group % nFields
            maxGroupSendNeighbors = maxGroupSendNeighbors + group % fields(i) % compactHaloInfo(10)
            maxGroupRecvNeighbors = maxGroupRecvNeighbors + group % fields(i) % compactHaloInfo(11)
        end do


        !
        ! Create a list of unique send and recv neighbors for this group
        !
        allocate(sendNeighbors(maxGroupSendNeighbors))
        allocate(recvNeighbors(maxGroupRecvNeighbors))

        sendNeighbors(:) = -1
        recvNeighbors(:) = -1

        group % nGroupSendNeighbors = 0
        group % nGroupRecvNeighbors = 0

        do i = 1, group % nFields
            idx = 1
            do iendp = 1, group % fields(i) % compactHaloInfo(10)

                ! Try to locate this endPointID in the list
                do j = 1, group % nGroupSendNeighbors
                    if (sendNeighbors(j) == group % fields(i) % compactSendLists(idx)) then
                        exit
                    end if
                end do

                ! If endPointID was not found, add it to the list
                if (j > group % nGroupSendNeighbors) then
                    group % nGroupSendNeighbors = group % nGroupSendNeighbors + 1
                    sendNeighbors(group % nGroupSendNeighbors) = group % fields(i) % compactSendLists(idx)
                end if

                ! Skip over remaining info for this endpoint
                idx = idx + 1                ! skip over endPointID
                do ihalo = 1, group % fields(i) % compactHaloInfo(9)
                    nlist = group % fields(i) % compactSendLists(idx)
                    idx = idx + 1            ! skip over nList
                    idx = idx + 2 * nlist    ! skip over srcList and destList
                end do
            end do

            idx = 1
            do iendp = 1, group % fields(i) % compactHaloInfo(11)

                ! Try to locate this endPointID in the list
                do j = 1, group % nGroupRecvNeighbors
                    if (recvNeighbors(j) == group % fields(i) % compactRecvLists(idx)) then
                        exit
                    end if
                end do

                ! If endPointID was not found, add it to the list
                if (j > group % nGroupRecvNeighbors) then
                    group % nGroupRecvNeighbors = group % nGroupRecvNeighbors + 1
                    recvNeighbors(group % nGroupRecvNeighbors) = group % fields(i) % compactRecvLists(idx)
                end if

                ! Skip over remaining info for this endpoint
                idx = idx + 1                ! skip over endPointID
                do ihalo = 1, group % fields(i) % compactHaloInfo(9)
                    nlist = group % fields(i) % compactRecvLists(idx)
                    idx = idx + 1            ! skip over nList
                    idx = idx + 2 * nlist    ! skip over srcList and destList
                end do
            end do

        end do

        allocate(group % groupSendNeighbors(group % nGroupSendNeighbors))
        group % groupSendNeighbors(:) = sendNeighbors(1:group % nGroupSendNeighbors)

        allocate(group % groupRecvNeighbors(group % nGroupRecvNeighbors))
        group % groupRecvNeighbors(:) = recvNeighbors(1:group % nGroupRecvNeighbors)

        allocate(group % groupToFieldRecvIdx(group % nGroupRecvNeighbors, group % nFields))
        group % groupToFieldRecvIdx(:,:) = 0

        deallocate(sendNeighbors)
        deallocate(recvNeighbors)


        !
        ! Compute total size of send and receive buffers for this group
        !
        group % groupSendBufSize = 0
        group % groupRecvBufSize = 0

        do i = 1, group % nFields

            ndims = group % fields(i) % compactHaloInfo(1)
            ninnerelems = 1
            do j = 1, ndims - 1    ! Do not include right-most dimension (nCells, nEdges, or nVertices)
                ninnerelems = ninnerelems * group % fields(i) % compactHaloInfo(j + 1)    ! First dim is at (2), etc.
            end do

            idx = 1
            do iendp = 1, group % fields(i) % compactHaloInfo(10)

                idx = idx + 1                ! skip over endPointID
                do ihalo = 1, group % fields(i) % compactHaloInfo(9)
                    nlist = group % fields(i) % compactSendLists(idx)
                    group % groupSendBufSize = group % groupSendBufSize + ninnerelems * nlist

                    idx = idx + 1            ! skip over nList
                    idx = idx + 2 * nlist    ! skip over srcList and destList
                end do
            end do

            idx = 1
            do iendp = 1, group % fields(i) % compactHaloInfo(11)

                idx = idx + 1                ! skip over endPointID
                do ihalo = 1, group % fields(i) % compactHaloInfo(9)
                    nlist = group % fields(i) % compactRecvLists(idx)
                    group % groupRecvBufSize = group % groupRecvBufSize + ninnerelems * nlist

                    idx = idx + 1            ! skip over nList
                    idx = idx + 2 * nlist    ! skip over srcList and destList
                end do
            end do

        end do


        !
        ! Compute sizes and offsets in group send and recv buffers for all fields in group
        !
        allocate(group % groupPackOffsets(group % nGroupSendNeighbors, group % nFields))
        allocate(group % groupSendOffsets(group % nGroupSendNeighbors))
        allocate(group % groupSendCounts(group % nGroupSendNeighbors))
        group % groupPackOffsets(:,:) = -1
        group % groupSendOffsets(:) = -1
        group % groupSendCounts(:) = -1

        allocate(group % groupUnpackOffsets(group % nGroupRecvNeighbors, group % nFields))
        allocate(group % groupRecvOffsets(group % nGroupRecvNeighbors))
        allocate(group % groupRecvCounts(group % nGroupRecvNeighbors))
        group % groupUnpackOffsets(:,:) = -1
        group % groupRecvOffsets(:) = -1
        group % groupRecvCounts(:) = -1

        allocate(sendCounts(group % nGroupSendNeighbors, group % nFields))
        allocate(recvCounts(group % nGroupRecvNeighbors, group % nFields))
        sendCounts(:,:) = 0
        recvCounts(:,:) = 0

        do i = 1, group % nFields

            ndims = group % fields(i) % compactHaloInfo(1)
            ninnerelems = 1
            do j = 1, ndims - 1    ! Do not include right-most dimension (nCells, nEdges, or nVertices)
                ninnerelems = ninnerelems * group % fields(i) % compactHaloInfo(j + 1)    ! First dim is at (2), etc.
            end do

            idx = 1
            do iendp = 1, group % fields(i) % compactHaloInfo(10)

                ! Find neighbor in groupSendNeighbors
                do j = 1, group % nGroupSendNeighbors
                    if (group % fields(i) % compactSendLists(idx) == group % groupSendNeighbors(j)) then
                        exit
                    end if
                end do

                idx = idx + 1                ! skip over endPointID
                sendCounts(j, i) = 0
                do ihalo = 1, group % fields(i) % compactHaloInfo(9)
                    nlist = group % fields(i) % compactSendLists(idx)
                    sendCounts(j, i) = sendCounts(j, i) + nlist * ninnerelems

                    idx = idx + 1            ! skip over nList
                    idx = idx + 2 * nlist    ! skip over srcList and destList
                end do
            end do

            idx = 1
            do iendp = 1, group % fields(i) % compactHaloInfo(11)

                ! Find neighbor in groupRecvNeighbors
                do j = 1, group % nGroupRecvNeighbors
                    if (group % fields(i) % compactRecvLists(idx) == group % groupRecvNeighbors(j)) then
                        group % groupToFieldRecvIdx(j, i) = iendp
                        exit
                    end if
                end do

                idx = idx + 1                ! skip over endPointID
                recvCounts(j, i) = 0
                do ihalo = 1, group % fields(i) % compactHaloInfo(9)
                    nlist = group % fields(i) % compactRecvLists(idx)
                    recvCounts(j, i) = recvCounts(j, i) + nlist * ninnerelems

                    idx = idx + 1            ! skip over nList
                    idx = idx + 2 * nlist    ! skip over srcList and destList
                end do
            end do

        end do

        do j = 1, group % nGroupSendNeighbors
            group % groupPackOffsets(j, 1) = 0
            do i = 2, group % nFields
                group % groupPackOffsets(j, i) = group % groupPackOffsets(j, i-1) + sendCounts(j, i-1)
            end do
        end do

        do j = 1, group % nGroupRecvNeighbors
            group % groupUnpackOffsets(j, 1) = 0
            do i = 2, group % nFields
                group % groupUnpackOffsets(j, i) = group % groupUnpackOffsets(j, i-1) + recvCounts(j, i-1)
            end do
        end do

        do j = 1, group % nGroupSendNeighbors
            group % groupSendCounts(j) = 0
            do i = 1, group % nFields
                group % groupSendCounts(j) = group % groupSendCounts(j) + sendCounts(j, i)
            end do

            if (j == 1) then
                group % groupSendOffsets(j) = 1
            else
                group % groupSendOffsets(j) = group % groupSendOffsets(j-1) + group % groupSendCounts(j-1)

                do i = 1, group % nFields
                    group % groupPackOffsets(j, i) = group % groupPackOffsets(j, i) + group % groupSendOffsets(j) - 1
                end do
            end if
        end do

        do j = 1, group % nGroupRecvNeighbors
            group % groupRecvCounts(j) = 0
            do i = 1, group % nFields
                group % groupRecvCounts(j) = group % groupRecvCounts(j) + recvCounts(j, i)
            end do

            if (j == 1) then
                group % groupRecvOffsets(j) = 1
            else
                group % groupRecvOffsets(j) = group % groupRecvOffsets(j-1) + group % groupRecvCounts(j-1)

                do i = 1, group % nFields
                    group % groupUnpackOffsets(j, i) = group % groupUnpackOffsets(j, i) + group % groupRecvOffsets(j) - 1
                end do
            end if
        end do

        deallocate(sendCounts)
        deallocate(recvCounts)

    end subroutine mpas_halo_aggregate_group_info


    !-----------------------------------------------------------------------
    !  routine refactor_lists
    !
    !> \brief Convert compact{Send,Recv}Lists into multi-dimensional arrays
    !> \author Michael Duda
    !> \date   25 May 2022
    !> \details
    !>  For each field in the halo exchange group identified by groupName,
    !>  convert the compact{Send,Recv}Lists 1-d arrays into multi-dimensional
    !>  arrays that allow for more optimal pack and unpack loops in the
    !>  mpas_halo_exch_group_full_halo_exch routine.
    !>
    !>  The following members in the mpas_halo_field type are allocated and
    !>  set by this routine:
    !>
    !>    nSendLists
    !>    maxNSendList
    !>    sendListSrc
    !>    sendListDst
    !>    packOffsets
    !>
    !>    nRecvLists
    !>    maxNRecvList
    !>    recvListSrc
    !>    recvListDst
    !>    unpackOffsets
    !
    !-----------------------------------------------------------------------
    subroutine refactor_lists(domain, groupName, iErr)

        use mpas_derived_types, only : domain_type, mpas_halo_group, MPAS_HALO_REAL, MPAS_LOG_CRIT
        use mpas_pool_routines, only : mpas_pool_get_array
        use mpas_log, only : mpas_log_write

        ! Arguments
        type (domain_type), intent(inout) :: domain
        character (len=*), intent(in) :: groupName
        integer, optional, intent(out) :: iErr

        ! Local variables
        integer :: i
        integer :: iNeighbor, j
        integer :: iHalo, iEndp
        integer :: nHalos, nSendEndpts, nRecvEndpts
        integer :: idx, idx_local
        type (mpas_halo_group), pointer :: group
        integer, dimension(:), pointer :: compactHaloInfo
        integer, dimension(:), pointer :: compactSendLists
        integer, dimension(:), pointer :: compactRecvLists
        integer :: maxNSendList
        integer, dimension(:,:), CONTIGUOUS pointer :: nSendLists
        integer, dimension(:,:,:), CONTIGUOUS pointer :: sendListSrc, sendListDst
        integer, dimension(:), CONTIGUOUS pointer :: packOffsets
        integer :: maxNRecvList
        integer, dimension(:,:), CONTIGUOUS pointer :: nRecvLists
        integer, dimension(:,:,:), CONTIGUOUS pointer :: recvListSrc, recvListDst
        integer, dimension(:), CONTIGUOUS pointer :: unpackOffsets


        if (present(iErr)) then
           iErr = 0
        end if

        !
        ! Find this halo exhange group in the list of groups
        !
        group => domain % haloGroups
        do while (associated(group))
            if (trim(group % groupName) == trim(groupName)) then
                exit
            end if

            group => group % next
        end do

        if (.not. associated(group)) then
            call mpas_log_write('Halo exchange group '//trim(groupName)//' not found in refactor_lists.', &
                                messageType=MPAS_LOG_CRIT)
        end if


        !
        ! Pack the segmented send buffer with elements from all fields and for all neighbors
        !
        do i = 1, group % nFields

            compactHaloInfo => group % fields(i) % compactHaloInfo
            compactSendLists => group % fields(i) % compactSendLists

            !
            ! Packing code for real-valued fields
            !
            if (group % fields(i) % fieldType == MPAS_HALO_REAL) then
                nHalos = compactHaloInfo(9)
                nSendEndpts = compactHaloInfo(10)
                idx = 1

                allocate(group % fields(i) % nSendLists(3,nSendEndpts))    ! MGD fix hard-coded 3 later...
                allocate(group % fields(i) % packOffsets(nSendEndpts))

                nSendLists => group % fields(i) % nSendLists
                packOffsets => group % fields(i) % packOffsets

                nSendLists(:,:) = 0
                packOffsets(:) = 0

                idx_local = idx

                do iEndp = 1, nSendEndpts
                    ! Find this endpoint in the list of neighbors
                    do iNeighbor = 1, group % nGroupSendNeighbors
                        if (group % groupSendNeighbors(iNeighbor) == compactSendLists(idx)) exit
                    end do
                    idx = idx + 1

                    packOffsets(iEndp) = group % groupPackOffsets(iNeighbor, i)

                    do iHalo = 1, nHalos
                        nSendLists(iHalo,iEndp) = compactSendLists(idx)
                        idx = idx + 1
                        idx = idx + 2*nSendLists(iHalo,iEndp)
                    end do
                end do

                maxNSendList = maxval(nSendLists)
                group % fields(i) % maxNSendList = maxNSendList

                allocate(group % fields(i) % sendListSrc(maxNSendList,nHalos,nSendEndpts))
                allocate(group % fields(i) % sendListDst(maxNSendList,nHalos,nSendEndpts))

                sendListSrc => group % fields(i) % sendListSrc
                sendListDst => group % fields(i) % sendListDst

                sendListSrc(:,:,:) = 0
                sendListDst(:,:,:) = 0

                idx = idx_local

                do iEndp = 1, nSendEndpts
                    idx = idx + 1

                    do iHalo = 1, nHalos
                        idx = idx + 1
                        do j = 1, nSendLists(iHalo,iEndp)
                            sendListSrc(j,iHalo,iEndp) = compactSendLists(idx + j - 1)
                            sendListDst(j,iHalo,iEndp) = compactSendLists(idx + nSendLists(iHalo,iEndp) + j - 1)
                        end do
                        idx = idx + 2*nSendLists(iHalo,iEndp)
                    end do
                end do

            end if
        end do


        !
        ! Unpack the segmented recv buffer with elements for all fields and from all neighbors
        !
        do i = 1, group % nFields

            compactHaloInfo => group % fields(i) % compactHaloInfo
            compactRecvLists => group % fields(i) % compactRecvLists

            idx = 1

            !
            ! Unpacking code for real-valued fields
            !
            if (group % fields(i) % fieldType == MPAS_HALO_REAL) then
                nHalos = compactHaloInfo(9)
                nRecvEndpts = compactHaloInfo(11)
                idx = 1

                allocate(group % fields(i) % nRecvLists(3,nRecvEndpts))    ! MGD fix hard-coded 3 later...
                allocate(group % fields(i) % unpackOffsets(nRecvEndpts))

                nRecvLists => group % fields(i) % nRecvLists
                unpackOffsets => group % fields(i) % unpackOffsets

                nRecvLists(:,:) = 0
                unpackOffsets(:) = 0

                idx_local = idx

                do iEndp = 1, nRecvEndpts
                    ! Find this endpoint in the list of neighbors
                    do iNeighbor = 1, group % nGroupRecvNeighbors
                        if (group % groupRecvNeighbors(iNeighbor) == compactRecvLists(idx)) exit
                    end do
                    idx = idx + 1

                    unpackOffsets(iEndp) = group % groupUnpackOffsets(iNeighbor, i)

                    do iHalo = 1, nHalos
                        nRecvLists(iHalo,iEndp) = compactRecvLists(idx)
                        idx = idx + 1
                        idx = idx + 2*nRecvLists(iHalo,iEndp)
                    end do
                end do

                maxNRecvList = maxval(nRecvLists)
                group % fields(i) % maxNRecvList = maxNRecvList

                allocate(group % fields(i) % recvListSrc(maxNRecvList,nHalos,nRecvEndpts))
                allocate(group % fields(i) % recvListDst(maxNRecvList,nHalos,nRecvEndpts))

                recvListSrc => group % fields(i) % recvListSrc
                recvListDst => group % fields(i) % recvListDst

                recvListSrc(:,:,:) = 0
                recvListDst(:,:,:) = 0

                idx = idx_local

                do iEndp = 1, nRecvEndpts
                    idx = idx + 1

                    do iHalo = 1, nHalos
                        idx = idx + 1
                        do j = 1, nRecvLists(iHalo,iEndp)
                            recvListSrc(j,iHalo,iEndp) = compactRecvLists(idx + j - 1)
                            recvListDst(j,iHalo,iEndp) = compactRecvLists(idx + nRecvLists(iHalo,iEndp) + j - 1)
                        end do
                        idx = idx + 2*nRecvLists(iHalo,iEndp)
                    end do
                end do

            end if
        end do

    end subroutine refactor_lists

end module mpas_halo
