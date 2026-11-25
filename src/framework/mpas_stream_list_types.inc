    integer, parameter :: MPAS_STREAM_LIST_NOERR     = 0, &
                          MPAS_STREAM_LIST_DUPLICATE = 1, &
                          MPAS_STREAM_LIST_NOT_FOUND = 2

    ! Maximum number of segments in the output_timelevels specification
    integer, parameter :: MAX_TIMELEVEL_SEGMENTS = 32

    ! timelevel specification
    type MPAS_timelevel_spec_type
        integer :: n_segments = 0                                  ! Number of parsed segments
        real(kind=RKIND) :: start_hour(MAX_TIMELEVEL_SEGMENTS)     ! Start hour for each segment
        real(kind=RKIND) :: end_hour(MAX_TIMELEVEL_SEGMENTS)       ! End hour for each segment
        real(kind=RKIND) :: interval_minutes(MAX_TIMELEVEL_SEGMENTS) ! Output interval in minutes
        logical :: is_parsed = .false.                             ! True if successfully parsed
    end type MPAS_timelevel_spec_type

    type MPAS_stream_list_type

        ! Used by list head
        integer :: nItems = 0
        type (MPAS_stream_list_type), pointer :: head => null()

        ! Used by streams
        integer :: direction
        logical :: valid = .false.
        logical :: immutable = .false.
        logical :: active_stream = .true.
        logical :: blockWrite = .false.
        character(len=StrKIND) :: filename
        character(len=StrKIND) :: filename_template
        character(len=StrKIND) :: filename_interval
        character(len=StrKIND) :: output_timelevels = ''
        type (MPAS_timelevel_spec_type) :: timelevel_spec
        type (MPAS_Stream_type), pointer :: stream => null()
        integer :: timeLevel = 0
        integer :: nRecords
        integer :: precision = MPAS_IO_NATIVE_PRECISION
        integer :: clobber_mode
        integer :: io_type
        type (MPAS_TimeInterval_type), pointer :: recordInterval => null()
        type (MPAS_stream_list_type), pointer :: alarmList_in => null()
        type (MPAS_stream_list_type), pointer :: alarmList_out => null()
        type (mpas_pool_type), pointer :: att_pool => null()
        type (mpas_pool_type), pointer :: field_pool => null()
        type (mpas_pool_type), pointer :: field_pkg_pool => null()
        type (mpas_pool_type), pointer :: pkg_pool => null()
        type (MPAS_Time_type), pointer :: referenceTime => null()

        ! Used by alarms
        type (MPAS_stream_list_type), pointer :: streamList => null()

        ! Used by streams and alarms
        character(len=StrKIND) :: name
        type (MPAS_stream_list_type), pointer :: xref => null()
        type (MPAS_stream_list_type), pointer :: next => null()

    end type MPAS_stream_list_type

