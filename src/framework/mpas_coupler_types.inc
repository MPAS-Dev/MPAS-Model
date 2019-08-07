    integer, parameter :: MPAS_MAX_COUPLER_PAIRINGS = 8

    integer, parameter :: MPAS_COUPLER_INVALID_DIRECTION = 8000
    integer, parameter :: MPAS_COUPLER_OUTGOING = 8001
    integer, parameter :: MPAS_COUPLER_INCOMING = 8002

    !
    ! Stores structures needed by the mpas_dmpar_exch_halo_acc routine
    ! These structures are specific to the dimensions of the field
    !
    type mpas_coupler_arrays_type
        integer, dimension(6) :: dims = -1    ! Dimensions of the field for which the structures apply
                                              ! All dimensions set to -1 indicates an uninitialized set of structures
        integer, dimension(:), pointer :: compactHaloInfo => null()
        type (mpas_communication_list), pointer :: sendCommList => null()
        type (mpas_communication_list), pointer :: recvCommList => null()
    end type mpas_coupler_arrays_type

    type mpas_coupler_pairing_type
        type (dm_info) :: dminfo
        type (mpas_multihalo_exchange_list), pointer :: sendList => null()
        type (mpas_multihalo_exchange_list), pointer :: recvList => null()
        type (mpas_multihalo_exchange_list), pointer :: copyList => null()
        logical :: isValid = .false.
        integer :: direction = MPAS_COUPLER_INVALID_DIRECTION

        ! Store sendCommList, recvCommList, and compactHaloInfo specific to dimensionality of field
        type (mpas_coupler_arrays_type), dimension(8) :: commArrays
    end type mpas_coupler_pairing_type

    type mpas_coupler_type
        integer :: intracomm
        integer :: intercomm
        logical :: initialized_mpi
        integer :: intercomm_rank
        integer :: intercomm_size
        integer :: thread_id
        procedure(mpas_coupler_role_function), pointer, pass(self) :: role
        procedure(mpas_coupler_roleleader_function), pointer, pass(self) :: role_leader
        type (mpas_coupler_pairing_type), dimension(:), pointer :: pairings
    end type mpas_coupler_type

    abstract interface
        integer function mpas_coupler_role_function(self)
            import mpas_coupler_type
            class (mpas_coupler_type), intent(in) :: self
        end function mpas_coupler_role_function
    end interface

    abstract interface
        integer function mpas_coupler_roleleader_function(self, role)
            import mpas_coupler_type
            class (mpas_coupler_type), intent(in) :: self
            integer, intent(in) :: role
        end function mpas_coupler_roleleader_function
    end interface
