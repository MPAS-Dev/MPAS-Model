#define CONTIGUOUS contiguous,

    integer, parameter :: MPAS_HALO_INVALID   = -1

    integer, parameter :: MPAS_HALO_REAL      = 5001, &
                          MPAS_HALO_INTEGER   = 5002


    !
    ! Information about an individual field in a halo group
    !
    type mpas_halo_field
        character(len=StrKIND) :: fieldName = ''      ! Name of the field
        integer :: nDims = MPAS_HALO_INVALID          ! Number of dimensions for field
        integer :: fieldType = MPAS_HALO_INVALID      ! Field type: MPAS_HALO_REAL, MPAS_HALO_INTEGER
        integer :: timeLevel = MPAS_HALO_INVALID      ! Which time level to exchange

        integer, dimension(:), pointer :: compactHaloInfo => null()      ! Information about halo communication for this field
        integer, dimension(:), pointer :: compactSendLists => null()     ! Elements sent to each neighbor
        integer, dimension(:), pointer :: compactRecvLists => null()     ! Elements received from each neighbor

        integer, dimension(:,:), CONTIGUOUS pointer :: nSendLists => null()     ! (3,nSendEndpoints) 3 is assumed max halos
        integer :: maxNSendList                                                 ! maxval(nSendLists)
        integer, dimension(:,:,:), CONTIGUOUS pointer :: sendListSrc => null()  ! (maxNSendList,nHalos,nSendEndpts)
        integer, dimension(:,:,:), CONTIGUOUS pointer :: sendListDst => null()  ! (maxNSendList,nHalos,nSendEndpts)
        integer, dimension(:), CONTIGUOUS pointer :: packOffsets => null()      ! (nSendEndpts)

        integer, dimension(:,:), CONTIGUOUS pointer :: nRecvLists => null()     ! (3,nRecvEndpoints) 3 is assumed max halos
        integer :: maxNRecvList                                                 ! maxval(nRecvLists)
        integer, dimension(:,:,:), CONTIGUOUS pointer :: recvListSrc => null()  ! (maxNRecvList,nHalos,nRecvEndpts)
        integer, dimension(:,:,:), CONTIGUOUS pointer :: recvListDst => null()  ! (maxNRecvList,nHalos,nRecvEndpts)
        integer, dimension(:), CONTIGUOUS pointer :: unpackOffsets => null()    ! (nRecvEndpts)

        real (kind=RKIND), dimension(:), pointer :: r1arr => null()      ! Pointer to field array, only used internally
        real (kind=RKIND), dimension(:,:), pointer :: r2arr => null()    ! Pointer to field array, only used internally
        real (kind=RKIND), dimension(:,:,:), pointer :: r3arr => null()  ! Pointer to field array, only used internally
    end type mpas_halo_field


    !
    ! Information about an entire halo group
    !
    type mpas_halo_group
        character(len=StrKIND) :: groupName = ''                           ! Name of the group
        integer :: nFields = MPAS_HALO_INVALID                             ! Number of fields in the group
        type (mpas_halo_field), dimension(:), pointer :: fields => null()  ! Array of field halo info types, dimensioned nFields

        integer :: nGroupSendNeighbors = MPAS_HALO_INVALID                ! Number of unique neighbors that we send to
        integer :: groupSendBufSize = MPAS_HALO_INVALID                   ! Total number of elements to be sent in a group exchange
        real (kind=RKIND), dimension(:), pointer :: sendBuf => null()     ! Segmented buffer used for outgoing messages
#ifdef MPAS_USE_MPI_F08
        type (MPI_Request), dimension(:), pointer :: sendRequests => null() ! Used internally - MPI request IDs
#else
        integer, dimension(:), pointer :: sendRequests => null() ! Used internally - MPI request IDs
#endif
        integer, dimension(:,:), pointer :: groupPackOffsets => null()    ! Offsets into sendBuf for each neighbor and each field
                                                                          !     dimensioned (nGroupSendNeighbors, nFields)
        integer, dimension(:), pointer :: groupSendNeighbors => null()    ! List of neighbors we send to
                                                                          !     dimensioned (nGroupSendNeighbors)
        integer, dimension(:), pointer :: groupSendOffsets => null()      ! Offset in sendBuf of segment to send to each neighbor
                                                                          !     dimensioned (nGroupSendNeighbors)
        integer, dimension(:), pointer :: groupSendCounts => null()       ! Size of sendBuf segment to send to each neighbor
                                                                          !     dimensioned (nGroupSendNeighbors)

        integer :: nGroupRecvNeighbors = MPAS_HALO_INVALID                ! Number of unique neighbors that we recv from
        integer :: groupRecvBufSize = MPAS_HALO_INVALID                   ! Total number of elements to be recvd in a group exchange
        real (kind=RKIND), dimension(:), pointer :: recvBuf => null()     ! Segmented buffer used for incoming messages
#ifdef MPAS_USE_MPI_F08
        type (MPI_Request), dimension(:), pointer :: recvRequests => null() ! Used internally - MPI request IDs
#else
        integer, dimension(:), pointer :: recvRequests => null() ! Used internally - MPI request IDs
#endif
        integer, dimension(:,:), pointer :: groupUnpackOffsets => null()  ! Offsets into recvBuf for each neighbor and each field
                                                                          !     dimensioned (nGroupRecvNeighbors, nFields)
        integer, dimension(:), pointer :: groupRecvNeighbors => null()    ! List of neighbors we recv from
                                                                          !     dimensioned (nGroupRecvNeighbors)
        integer, dimension(:,:), pointer :: groupToFieldRecvIdx => null() ! Convert from group-wide neighbor indices to
                                                                          !     field-local indices
                                                                          !     dimensioned (nGroupRecvNeighbors, nFields)
        integer, dimension(:), pointer :: groupRecvOffsets => null()      ! Offset in recvBuf of segment to recv from each neighbor
                                                                          !     dimensioned (nGroupRecvNeighbors)
        integer, dimension(:), pointer :: groupRecvCounts => null()       ! Size of recvBuf segment to recv from each neighbor
                                                                          !     dimensioned (nGroupRecvNeighbors)

        type (mpas_halo_group), pointer :: next => null()    ! Pointer to the next halo group
    end type mpas_halo_group
