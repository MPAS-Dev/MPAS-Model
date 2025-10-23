module session_fixture_mod
    use mpi
    use fortest_test_suite
    use mpas_subdriver
    use mpas_derived_types
    use mpas_timekeeping
    use mpas_stream_manager

    implicit none

    type session_fixture_t
        type(core_type), pointer :: core_list => null()
        type(domain_type), pointer :: domain => null()
        integer :: external_comm
    end type

contains

    subroutine setup_session(f_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: f_ptr
        type(session_fixture_t), pointer :: fixture
        integer :: ierr
        character(len = StrKIND) :: start_time_str
        type(MPAS_Time_Type) :: start_time
        type(mpas_pool_type), pointer :: model_pool
        character(len = StrKIND), pointer :: xtime
        type(MPAS_Clock_type), pointer :: clock

        ierr = 0
        call c_f_pointer(f_ptr, fixture)
        call MPI_Init(ierr)
        fixture%external_comm = MPI_COMM_WORLD

        call mpas_init(fixture%core_list, fixture%domain, external_comm = fixture%external_comm, &
                namelistFileParam = 'namelist.test', &
                streamsFileParam = 'streams.test')

        clock => fixture%domain%clock

        start_time = mpas_get_clock_time(clock, MPAS_START_TIME, ierr)
        call mpas_get_time(start_time, dateTimeString = start_time_str)
        call mpas_pool_get_subpool(fixture%domain%blocklist%structs, 'model', model_pool)
        call mpas_pool_get_array(model_pool, 'xtime', xtime)
        xtime = start_time_str

        call mpas_stream_mgr_read(fixture%domain%streamManager, ierr = ierr)
        call mpas_stream_mgr_reset_alarms(fixture%domain%streamManager, direction = MPAS_STREAM_INPUT, ierr = ierr)
    end subroutine

    subroutine teardown_session(f_ptr)
        use iso_c_binding, only : c_ptr, c_f_pointer
        implicit none
        type(c_ptr), value :: f_ptr
        type(session_fixture_t), pointer :: fixture
        integer :: ierr

        call c_f_pointer(f_ptr, fixture)
        call MPI_Finalize(ierr)
    end subroutine

end module session_fixture_mod
