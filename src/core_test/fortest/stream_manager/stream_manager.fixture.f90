module stream_manager_fixture
    use mpi
    use mpas_subdriver
    use mpas_derived_types
    use mpas_timekeeping
    use mpas_stream_manager
    use iso_c_binding, only : c_ptr, c_f_pointer

    implicit none

    type stream_manager_fixture_t
        type(core_type), pointer :: core_list => null()
        type(domain_type), pointer :: domain => null()
        type(MPAS_streamManager_type), pointer :: manager
        integer, pointer :: external_comm
        type(MPAS_Clock_type), pointer :: clock
    end type stream_manager_fixture_t

    type field_ptr_t
        type(field0DInteger), pointer :: field
    end type field_ptr_t

    type pool_ptr_t
        type(MPAS_Pool_type), pointer :: pool
    end type pool_ptr_t

contains

    subroutine setup_stream_manager(f_ptr)
        implicit none
        type(c_ptr), value :: f_ptr
        type(stream_manager_fixture_t), pointer :: f
        integer :: ierr
        type(MPAS_Clock_type), pointer :: stream_manager_clock
        type(MPAS_IO_context_type), pointer :: ioContext
        type(MPAS_Pool_type), pointer :: allFields
        type(MPAS_Pool_type), pointer :: allPackages
        type(MPAS_Pool_type), pointer :: allStructs
        type(field_ptr_t), allocatable, dimension(:) :: fields
        type(pool_ptr_t), allocatable, dimension(:) :: structs
        type(logical), allocatable, dimension(:) :: packages
        type(character(len=StrKIND)), allocatable, dimension(:) :: package_names
        integer :: i

        call c_f_pointer(f_ptr, f)
        call setup_mpas(f)

        ierr = 0

        call setup_clock(f, stream_manager_clock)
        call init_fields(fields)
        call init_structs(structs)
        call init_packages(packages, package_names)
        call setup_allFields_pool(f, allFields, fields)
        call setup_allStructs_pool(f, allStructs, structs, fields)
        call setup_allPackages_pool(f, allPackages, packages, package_names)
        allocate(ioContext)
        call MPAS_stream_mgr_init(f%manager, ioContext, stream_manager_clock, &
                allFields, allPackages, allStructs, ierr = ierr)
    end subroutine setup_stream_manager


    subroutine teardown_stream_manager(f_ptr)
        implicit none
        type(c_ptr), value :: f_ptr
        type(stream_manager_fixture_t), pointer :: f

        call c_f_pointer(f_ptr, f)
        call mpas_finalize(f%core_list, f%domain)
        nullify(f%core_list, f%domain, f%clock)
    end subroutine teardown_stream_manager

    subroutine setup_allPackages_pool(f, allPackages, packages, package_names)
        implicit none
        type(stream_manager_fixture_t), pointer :: f
        integer :: ierr
        type(MPAS_Pool_type), pointer :: allPackages
        type(logical), allocatable, dimension(:) :: packages
        type(character(len=StrKIND)), allocatable, dimension(:) :: package_names
        integer :: i

        allocate(allPackages)
        call mpas_pool_create_pool(allPackages)
        call mpas_pool_add_package(allPackages, trim(package_names(1)), packages(1))
        call mpas_pool_add_package(allPackages, trim(package_names(2)), packages(2))
        call mpas_pool_add_package(allPackages, trim(package_names(3)), packages(3))
    end subroutine setup_allPackages_pool

    subroutine setup_allFields_pool(f, allFields, fields)
        implicit none
        type(stream_manager_fixture_t), pointer :: f
        integer :: ierr
        type(MPAS_Pool_type), pointer :: allFields
        type(field_ptr_t), allocatable, dimension(:) :: fields

        allocate(allFields)
        call mpas_pool_create_pool(allFields)
        fields(1)%field%isActive = .true.
        fields(2)%field%isActive = .true.
        fields(3)%field%isActive = .true.
        fields(5)%field%isActive = .true.
        call mpas_pool_add_field(allFields, 'field1', fields(1)%field)
        call mpas_pool_add_field(allFields, 'field2', fields(2)%field)
        call mpas_pool_add_field(allFields, 'field3', fields(3)%field)
        call mpas_pool_add_field(allFields, 'field5', fields(5)%field)
    end subroutine setup_allFields_pool

    subroutine setup_allStructs_pool(f, allStructs, structs, fields)
        implicit none
        type(stream_manager_fixture_t), pointer :: f
        integer :: ierr
        type(MPAS_Pool_type), pointer :: allStructs
        type(pool_ptr_t), allocatable, dimension(:) :: structs
        type(field_ptr_t), allocatable, dimension(:) :: fields
        character(len = StrKIND) :: field1_name, field2_name, field3_name, &
            field4_name, field5_name, field6_name

        field1_name = 'field1'
        field2_name = 'field2'
        field3_name = 'field3'
        field4_name = 'field4'
        field5_name = 'field5'
        field6_name = 'field6'


        allocate(allStructs)
        call mpas_pool_create_pool(structs(1)%pool)
        call mpas_pool_create_pool(structs(2)%pool)
        call mpas_pool_create_pool(structs(3)%pool)
        call mpas_pool_create_pool(structs(4)%pool)
        call mpas_pool_create_pool(structs(5)%pool)
        call mpas_pool_create_pool(structs(6)%pool)
        call mpas_pool_create_pool(allStructs)
        call mpas_pool_add_field(structs(1)%pool, trim(field1_name), fields(1)%field)
        call mpas_pool_add_field(structs(2)%pool, trim(field2_name), fields(2)%field)
        call mpas_pool_add_field(structs(3)%pool, trim(field3_name), fields(3)%field)
        call mpas_pool_add_field(structs(4)%pool, trim(field4_name), fields(4)%field)
        call mpas_pool_add_field(structs(5)%pool, trim(field5_name), fields(5)%field)
        call mpas_pool_add_field(structs(6)%pool, trim(field6_name), fields(6)%field)

        call mpas_pool_add_subpool(allStructs, 'struct1', structs(1)%pool)
        call mpas_pool_add_subpool(allStructs, 'struct2', structs(2)%pool)
        call mpas_pool_add_subpool(allStructs, 'struct3', structs(3)%pool)
        call mpas_pool_add_subpool(allStructs, 'struct4', structs(4)%pool)
        call mpas_pool_add_subpool(allStructs, 'struct5', structs(5)%pool)
        call mpas_pool_add_subpool(allStructs, 'struct6', structs(6)%pool)
    end subroutine setup_allStructs_pool

    subroutine setup_clock(f, clock)
        implicit none
        type(stream_manager_fixture_t), pointer :: f
        type(MPAS_Clock_type), pointer :: clock
        integer :: ierr
        type(MPAS_Time_type) :: clock_start_time, clock_stop_time
        type(MPAS_TimeInterval_type) :: clock_time_step

        ierr = 0

        allocate(clock)
        clock_start_time = mpas_get_clock_time(f%clock, MPAS_START_TIME)
        clock_stop_time = mpas_get_clock_time(f%clock, MPAS_STOP_TIME)
        clock_time_step = mpas_get_clock_timestep(f%clock)

        call mpas_create_clock(clock, clock_start_time, clock_time_step, &
                clock_stop_time, ierr = ierr)
    end subroutine setup_clock


    subroutine setup_mpas(f)
        implicit none
        type(stream_manager_fixture_t), intent(inout), pointer :: f
        integer :: ierr
        character(len = StrKIND) :: start_time_str
        type(MPAS_Time_Type) :: start_time
        type(mpas_pool_type), pointer :: model_pool
        character(len = StrKIND), pointer :: xtime

        ierr = 0
        call mpas_init(f%core_list, f%domain, external_comm = MPI_COMM_WORLD, &
                namelistFileParam = 'namelist.test', streamsFileParam = 'streams.test')

        f%clock => f%domain%clock

!        start_time = mpas_get_clock_time(f%clock, MPAS_START_TIME)
!        call mpas_get_time(start_time, dateTimeString = start_time_str)
!
!        call mpas_pool_get_subpool(f%domain%blocklist%structs, 'model', model_pool)
!        call mpas_pool_get_array(model_pool, 'xtime', xtime)
!        xtime = start_time_str
!
!        call mpas_stream_mgr_read(f%domain%streamManager)
!        call mpas_stream_mgr_reset_alarms(f%domain%streamManager, direction = MPAS_STREAM_INPUT)
    end subroutine setup_mpas

    subroutine init_fields(fields)
        implicit none
        type(field_ptr_t), allocatable, dimension(:) :: fields

        if (.not. allocated(fields)) then
            allocate(fields(6))
            allocate(fields(1)%field)
            allocate(fields(2)%field)
            allocate(fields(3)%field)
            allocate(fields(4)%field)
            allocate(fields(5)%field)
            allocate(fields(6)%field)
        end if
        fields(1)%field%isActive = .true.
        fields(2)%field%isActive = .true.
        fields(3)%field%isActive = .true.
        fields(4)%field%isActive = .true.
        fields(5)%field%isActive = .true.
        fields(1)%field%fieldName = 'field1'
        fields(2)%field%fieldName = 'field2'
        fields(3)%field%fieldName = 'field3'
        fields(4)%field%fieldName = 'field4'
        fields(6)%field%fieldName = 'field6'
    end subroutine init_fields

    subroutine init_structs(structs)
        implicit none
        type(pool_ptr_t), allocatable, dimension(:) :: structs

        if (.not. allocated(structs)) then
            allocate(structs(6))
            allocate(structs(1)%pool)
            allocate(structs(2)%pool)
            allocate(structs(3)%pool)
            allocate(structs(4)%pool)
            allocate(structs(5)%pool)
            allocate(structs(6)%pool)
        end if
    end subroutine init_structs
    subroutine init_packages(packages, package_names)
        implicit none
        type(logical), allocatable, dimension(:) :: packages
        type(character(len=StrKIND)), allocatable, dimension(:) :: package_names

        if (.not. allocated(packages)) then
            allocate(packages(3))
            allocate(package_names(3))
            packages(1) = .true.
            packages(2) = .true.
            packages(3) = .false.
            package_names(1) = 'package1'
            package_names(2) = 'package2'
            package_names(3) = 'package3'
        end if
    end subroutine init_packages

end module stream_manager_fixture
