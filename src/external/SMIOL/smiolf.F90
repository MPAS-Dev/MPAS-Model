#include "smiol_codes.inc"

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! SMIOL -- The Simple MPAS I/O Library
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module SMIOLf

    use iso_c_binding, only : c_int, c_size_t, c_int64_t, c_ptr

    private

    public :: SMIOLf_context, &
              SMIOLf_decomp, &
              SMIOLf_file

    public :: SMIOL_offset_kind

    public :: SMIOLf_init, &
              SMIOLf_finalize, &
              SMIOLf_inquire, &
              SMIOLf_open_file, &
              SMIOLf_close_file, &
              SMIOLf_define_dim, &
              SMIOLf_inquire_dim, &
              SMIOLf_define_var, &
              SMIOLf_inquire_var, &
              SMIOLf_put_var, &
              SMIOLf_get_var, &
              SMIOLf_define_att, &
              SMIOLf_inquire_att, &
              SMIOLf_sync_file, &
              SMIOLf_error_string, &
              SMIOLf_lib_error_string, &
              SMIOLf_set_option, &
              SMIOLf_create_decomp, &
              SMIOLf_free_decomp, &
              SMIOLf_set_frame, &
              SMIOLf_get_frame, &
              SMIOLf_f_to_c_string


    integer, parameter :: SMIOL_offset_kind = c_int64_t   ! Must match SMIOL_Offset in smiol_types.h


    type, bind(C) :: SMIOLf_context
        integer :: fcomm                ! Fortran handle to MPI communicator; MPI_Fint on the C side, which is supposed to match
                                        !   a Fortran integer

        integer(c_int) :: comm_size     ! Size of MPI communicator
        integer(c_int) :: comm_rank     ! Rank within MPI communicator

        integer(c_int) :: num_io_tasks  ! The number of I/O tasks
        integer(c_int) :: io_stride     ! The stride between I/O tasks in the communicator

        integer(c_int) :: lib_ierr      ! Library-specific error code
        integer(c_int) :: lib_type      ! From which library the error code originated
    end type SMIOLf_context

    type, bind(C) :: SMIOLf_file
        type (c_ptr) :: context      ! Pointer to (struct SMIOL_context); the context within which the file was opened
        integer(kind=SMIOL_offset_kind) :: frame      ! Current frame of the file
#ifdef SMIOL_PNETCDF
        integer(c_int) :: state      ! parallel-netCDF file state (i.e. Define or data mode)
        integer(c_int) :: ncidp      ! parallel-netCDF file handle
        integer(c_size_t) :: bufsize ! Size of buffer attached to this file
        integer(c_int) :: n_reqs     ! Number of pending non-blocking requests
        type (c_ptr) :: reqs         ! Array of pending non-blocking request handles
#endif
        integer(c_int) :: io_task    ! 1 = this task performs I/O calls; 0 = no I/O calls on this task
        integer :: io_file_comm      ! Communicator shared by all tasks with io_task == 1
        integer :: io_group_comm     ! Communicator shared by tasks associated with an I/O task, usually 1 I/O task
                                     ! and N-1 non-I/O tasks, where N is the I/O stride
    end type SMIOLf_file

    type, bind(C) :: SMIOLf_decomp
        !
        ! The lists below are structured (in C) as follows:
        !   list[0] - the number of neighbors for which a task sends/recvs
        !                                                                             |
        !   list[n] - neighbor task ID                                                | repeated for
        !   list[n+1] - number of elements, m, to send/recv to/from the neighbor      | each neighbor
        !   list[n+2 .. n+2+m] - local element IDs to send/recv to/from the neighbor  |
        !                                                                             |
        !
        type(c_ptr) :: comp_list  ! Elements to be sent/received from/on a compute task
        type(c_ptr) :: io_list    ! Elements to be send/received from/on an I/O task

        type (c_ptr) :: context   ! Pointer to (struct SMIOL_context); the context for this decomp

        integer(c_size_t) :: io_start;  ! The starting offset on disk for I/O by a task
        integer(c_size_t) :: io_count;  ! The number of elements for I/O by a task

        integer(c_int) ::  agg_factor       ! Aggregation factor, or size of aggregation group
        integer :: agg_comm                 ! Communicator for aggregation/deaggregation operations
        integer(c_size_t) :: n_compute      ! Number of un-aggregated compute elements on the task
        integer(c_size_t) :: n_compute_agg  ! Number of aggregated compute elements on the task
        type (c_ptr) :: counts              ! Compute element counts for tasks in aggregation group
        type (c_ptr) :: displs              ! Displacements in aggregated list of elements for tasks
                                            !    in aggregation group
    end type SMIOLf_decomp

    interface SMIOLf_define_att
        module procedure SMIOLf_define_att_int
        module procedure SMIOLf_define_att_float
        module procedure SMIOLf_define_att_double
        module procedure SMIOLf_define_att_text
    end interface

    interface SMIOLf_inquire_att
        module procedure SMIOLf_inquire_att_int
        module procedure SMIOLf_inquire_att_float
        module procedure SMIOLf_inquire_att_double
        module procedure SMIOLf_inquire_att_text
    end interface

    !
    ! Note: The implementations of the specific SMIOLf_put_var routines
    !       are found in the file smiolf_put_get_var.inc, which is included
    !       in this module with a pre-processor directive
    !
    interface SMIOLf_put_var
        module procedure SMIOLf_put_var_0d_char
        module procedure SMIOLf_put_var_0d_int32
        module procedure SMIOLf_put_var_0d_real32
        module procedure SMIOLf_put_var_0d_real64
        module procedure SMIOLf_put_var_1d_int32
        module procedure SMIOLf_put_var_1d_real32
        module procedure SMIOLf_put_var_1d_real64
        module procedure SMIOLf_put_var_2d_int32
        module procedure SMIOLf_put_var_2d_real32
        module procedure SMIOLf_put_var_2d_real64
        module procedure SMIOLf_put_var_3d_int32
        module procedure SMIOLf_put_var_3d_real32
        module procedure SMIOLf_put_var_3d_real64
        module procedure SMIOLf_put_var_4d_int32
        module procedure SMIOLf_put_var_4d_real32
        module procedure SMIOLf_put_var_4d_real64
        module procedure SMIOLf_put_var_5d_real32
        module procedure SMIOLf_put_var_5d_real64
    end interface SMIOLf_put_var

    !
    ! Note: The implementations of the specific SMIOLf_get_var routines
    !       are found in the file smiolf_put_get_var.inc, which is included
    !       in this module with a pre-processor directive
    !
    interface SMIOLf_get_var
        module procedure SMIOLf_get_var_0d_char
        module procedure SMIOLf_get_var_0d_int32
        module procedure SMIOLf_get_var_0d_real32
        module procedure SMIOLf_get_var_0d_real64
        module procedure SMIOLf_get_var_1d_int32
        module procedure SMIOLf_get_var_1d_real32
        module procedure SMIOLf_get_var_1d_real64
        module procedure SMIOLf_get_var_2d_int32
        module procedure SMIOLf_get_var_2d_real32
        module procedure SMIOLf_get_var_2d_real64
        module procedure SMIOLf_get_var_3d_int32
        module procedure SMIOLf_get_var_3d_real32
        module procedure SMIOLf_get_var_3d_real64
        module procedure SMIOLf_get_var_4d_int32
        module procedure SMIOLf_get_var_4d_real32
        module procedure SMIOLf_get_var_4d_real64
        module procedure SMIOLf_get_var_5d_real32
        module procedure SMIOLf_get_var_5d_real64
    end interface SMIOLf_get_var

    ! C interface definitions used in multiple routines
    interface
        function SMIOL_define_att(file, varname, att_name, att_type, att) result(ierr) bind(C, name='SMIOL_define_att')
            use iso_c_binding, only : c_ptr, c_char, c_int
            type (c_ptr), value :: file
            type (c_ptr), value :: varname
            character(kind=c_char), dimension(*) :: att_name
            integer(kind=c_int), value :: att_type
            type (c_ptr), value :: att
            integer(kind=c_int) :: ierr
        end function

        function SMIOL_inquire_att(file, varname, att_name, att_type, att_len, att) result(ierr) bind(C, name='SMIOL_inquire_att')
            use iso_c_binding, only : c_ptr, c_char, c_int
            type (c_ptr), value :: file
            type (c_ptr), value :: varname
            character(kind=c_char), dimension(*) :: att_name
            type (c_ptr), value :: att_type
            type (c_ptr), value :: att_len
            type (c_ptr), value :: att
            integer(kind=c_int) :: ierr
        end function

        function SMIOL_put_var(file, varname, decomp, buf) result(ierr) bind(C, name='SMIOL_put_var')
             use iso_c_binding, only : c_ptr, c_char, c_int
             type (c_ptr), value :: file
             character (kind=c_char), dimension(*) :: varname
             type (c_ptr), value :: decomp
             type (c_ptr), value :: buf
             integer (kind=c_int) :: ierr
        end function

        function SMIOL_get_var(file, varname, decomp, buf) result(ierr) bind(C, name='SMIOL_get_var')
             use iso_c_binding, only : c_ptr, c_char, c_int
             type (c_ptr), value :: file
             character (kind=c_char), dimension(*) :: varname
             type (c_ptr), value :: decomp
             type (c_ptr), value :: buf
             integer (kind=c_int) :: ierr
        end function
    end interface


contains


    !
    ! Library methods
    !

    !-----------------------------------------------------------------------
    !  routine SMIOLf_init
    !
    !> \brief Initialize a SMIOL context
    !> \details
    !>  Initializes a SMIOL context, within which decompositions may be defined and
    !>  files may be read and written. The input argument comm is an MPI communicator,
    !>  and the input arguments num_io_tasks and io_stride provide the total number
    !>  of I/O tasks and the stride between those I/O tasks within the communicator.
    !>
    !>  Upon successful return the context argument points to a valid SMIOL context;
    !>  otherwise, it is NULL and an error code other than MPI_SUCCESS is returned.
    !>
    !>  Note: It is assumed that MPI_Init has been called prior to this routine, so
    !>        that any use of the provided MPI communicator will be valid.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_init(comm, num_io_tasks, io_stride, context) result(ierr)

        use iso_c_binding, only : c_ptr, c_f_pointer, c_null_ptr, c_associated

        implicit none

        integer, intent(in) :: comm
        integer, intent(in) :: num_io_tasks
        integer, intent(in) :: io_stride
        type (SMIOLf_context), pointer :: context

        type (c_ptr) :: c_context = c_null_ptr

        ! C interface definitions
        interface
            function SMIOL_fortran_init(comm, num_io_tasks, io_stride, context) result(ierr) bind(C, name='SMIOL_fortran_init')
                use iso_c_binding, only : c_int, c_ptr
                integer, value :: comm   ! MPI_Fint on the C side, which is supposed to match a Fortran integer
                integer(c_int), value :: num_io_tasks
                integer(c_int), value :: io_stride
                type (c_ptr) :: context
                integer(kind=c_int) :: ierr
            end function
        end interface

        ierr = SMIOL_fortran_init(comm, num_io_tasks, io_stride, c_context)

        if (ierr == SMIOL_SUCCESS) then
            if (.not. c_associated(c_context)) then
                nullify(context)
                ierr = SMIOL_FORTRAN_ERROR
            else
                call c_f_pointer(c_context, context)
            end if
        else
            if (.not. c_associated(c_context)) then
                nullify(context)
            else
                ierr = SMIOL_FORTRAN_ERROR
            end if
        end if

    end function SMIOLf_init


    !-----------------------------------------------------------------------
    !  routine SMIOLf_finalize
    !
    !> \brief Finalize a SMIOL context
    !> \details
    !>  Finalizes a SMIOL context and frees all memory in the SMIOL_context instance.
    !>  After this routine is called, no other SMIOL routines that make reference to
    !>  the finalized context should be called.
    !>
    !>  Upon return, the context argument will be unassociated if no errors occurred.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_finalize(context) result(ierr)

        use iso_c_binding, only : c_ptr, c_loc, c_associated, c_null_ptr

        implicit none

        type (SMIOLf_context), pointer :: context

        type (c_ptr) :: c_context = c_null_ptr

        ! C interface definitions
        interface
            function SMIOL_finalize(context) result(ierr) bind(C, name='SMIOL_finalize')
                use iso_c_binding, only : c_int, c_ptr
                type (c_ptr) :: context
                integer(kind=c_int) :: ierr
            end function
        end interface

        if (associated(context)) then
            c_context = c_loc(context)
        end if

        ierr = SMIOL_finalize(c_context)

        if (ierr == SMIOL_SUCCESS) then
            if (c_associated(c_context)) then
                ierr = SMIOL_FORTRAN_ERROR
            else
                nullify(context)
            end if
        else
            if (.not. c_associated(c_context)) then
                nullify(context)
            end if
        end if

    end function SMIOLf_finalize


    !-----------------------------------------------------------------------
    !  routine SMIOLf_inquire
    !
    !> \brief Inquire about a SMIOL context
    !> \details
    !>  Detailed description of what this routine does.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_inquire() result(ierr)

        implicit none

        ierr = 0

    end function SMIOLf_inquire


    !
    ! File methods
    !

    !-----------------------------------------------------------------------
    !  routine SMIOLf_open_file
    !
    !> \brief Opens a file within a SMIOL context
    !> \details
    !>  Depending on the specified file mode, creates or opens the file specified
    !>  by filename within the provided SMIOL context.
    !>
    !>  The optional bufsize argument specifies the size in bytes of the buffer
    !>  to be attached to the file by I/O tasks; at present this buffer is only
    !>  used by the Parallel-NetCDF library if the file is opened with a mode of
    !>  SMIOL_FILE_CREATE or SMIOL_FILE_WRITE. A bufsize of 0 will force the use of
    !>  the Parallel-NetCDF blocking write interface, while a nonzero value enables
    !>  the use of the non-blocking, buffered interface for writing. If the bufsize
    !>  argument is not present, a default buffer size of 128 MiB is used.
    !>
    !>  When a file is opened with a mode of SMIOL_FILE_CREATE, the fformat optional
    !>  argument is used to set the file format. Otherwise, fformat is ignored. If
    !>  not present, the default file format is SMIOL_FORMAT_CDF5.
    !>
    !>  Upon successful completion, SMIOL_SUCCESS is returned, and the file handle argument
    !>  will point to a valid file handle. Otherwise, the file handle is not associated
    !>  and an error code other than SMIOL_SUCCESS is returned.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_open_file(context, filename, mode, file, bufsize, fformat) result(ierr)

        use iso_c_binding, only : c_loc, c_ptr, c_null_ptr, c_char, c_size_t, c_associated, c_f_pointer

        implicit none

        type (SMIOLf_context), pointer :: context
        character(len=*), intent(in) :: filename
        integer, intent(in) :: mode
        type (SMIOLf_file), pointer :: file
        integer(kind=c_size_t), intent(in), optional :: bufsize
        integer, intent(in), optional :: fformat

        ! Default buffer size to use if optional bufsize argument is not provided
        integer (kind=c_size_t), parameter :: default_bufsize = int(128*1024*1024, kind=c_size_t)

        type (c_ptr) :: c_context = c_null_ptr
        type (c_ptr) :: c_file = c_null_ptr
        integer(kind=c_int) :: c_mode
        character(kind=c_char), dimension(:), pointer :: c_filename
        integer(kind=c_int) :: c_fformat

        ! C interface definitions
        interface
            function SMIOL_open_file(context, filename, mode, file, bufsize, fformat) result(ierr) bind(C, name='SMIOL_open_file')
                use iso_c_binding, only : c_char, c_ptr, c_int, c_size_t
                type (c_ptr), value :: context
                character(kind=c_char), dimension(*) :: filename
                integer(kind=c_int), value :: mode
                type (c_ptr) :: file
                integer(kind=c_size_t), value :: bufsize
                integer(kind=c_int), value :: fformat
                integer(kind=c_int) :: ierr
            end function
        end interface

        if (associated(context)) then
            c_context = c_loc(context)
        end if

        !
        ! Convert Fortran string to C character array
        !
        allocate(c_filename(len_trim(filename) + 1))
        call SMIOLf_f_to_c_string(filename, c_filename)

        c_mode = mode

        if (present(fformat)) then
            c_fformat = fformat
        else
            c_fformat = SMIOL_FORMAT_CDF5
        end if

        if (present(bufsize)) then
            ierr = SMIOL_open_file(c_context, c_filename, c_mode, c_file, &
                                   bufsize, c_fformat)
        else
            ierr = SMIOL_open_file(c_context, c_filename, c_mode, c_file, &
                                   default_bufsize, c_fformat)
        end if

        deallocate(c_filename)

        if (ierr == SMIOL_SUCCESS) then
            if (.not. c_associated(c_file)) then
                nullify(file)
                ierr = SMIOL_FORTRAN_ERROR
            else
                call c_f_pointer(c_file, file)
            end if
        else
            if (.not. c_associated(c_file)) then
                nullify(file)
            else
                ierr = SMIOL_FORTRAN_ERROR
            end if
        end if

    end function SMIOLf_open_file


    !-----------------------------------------------------------------------
    !  routine SMIOLf_close_file
    !
    !> \brief Closes a file within a SMIOL context
    !> \details
    !>  Closes the file associated with the provided file handle. Upon successful
    !>  completion, SMIOL_SUCCESS is returned, the file will be closed, and all memory
    !>  that is uniquely associated with the file handle will be deallocated.
    !>  Otherwise, an error code other than SMIOL_SUCCESS will be returned.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_close_file(file) result(ierr)

        use iso_c_binding, only : c_loc, c_ptr, c_null_ptr, c_associated

        implicit none

        type (SMIOLf_file), pointer :: file

        type (c_ptr) :: c_file = c_null_ptr

        ! C interface definitions
        interface
            function SMIOL_close_file(file) result(ierr) bind(C, name='SMIOL_close_file')
                use iso_c_binding, only : c_ptr, c_int
                type (c_ptr) :: file
                integer(kind=c_int) :: ierr
            end function
        end interface

        if (associated(file)) then
            c_file = c_loc(file)
        end if

        ierr = SMIOL_close_file(c_file)

        if (ierr == SMIOL_SUCCESS) then
            if (c_associated(c_file)) then
                ierr = SMIOL_FORTRAN_ERROR
            else
                nullify(file)
            end if
        else
            if (.not. c_associated(c_file)) then
                nullify(file)
            end if
        end if

    end function SMIOLf_close_file


    !
    ! Dimension methods
    !

    !-----------------------------------------------------------------------
    !  routine SMIOLf_define_dim
    !
    !> \brief Defines a new dimension in a file
    !> \details
    !>  Defines a dimension with the specified name and size in the file associated
    !>  with the file handle. If a negative value is provided for the size argument,
    !>  the dimension will be defined as an unlimited or record dimension.
    !>
    !>  Upon successful completion, SMIOL_SUCCESS is returned; otherwise, an error
    !>  code is returned.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_define_dim(file, dimname, dimsize) result(ierr)

        use iso_c_binding, only : c_char, c_loc, c_ptr

        implicit none

        type (SMIOLf_file), target :: file
        character(len=*), intent(in) :: dimname
        integer(kind=SMIOL_offset_kind), intent(in) :: dimsize

        type (c_ptr) :: c_file
        character(kind=c_char), dimension(:), pointer :: c_dimname

        ! C interface definitions
        interface
            function SMIOL_define_dim(file, dimname, dimsize) result(ierr) bind(C, name='SMIOL_define_dim')
                use iso_c_binding, only : c_ptr, c_char, c_int
                import SMIOL_offset_kind
                type (c_ptr), value :: file
                character(kind=c_char), dimension(*) :: dimname
                integer(kind=SMIOL_offset_kind), value :: dimsize
                integer(kind=c_int) :: ierr
            end function
        end interface

        ! Get C address of file; there is no need to worry about an unassociated file here,
        ! since the file argument is not a pointer
        c_file = c_loc(file)

        !
        ! Convert Fortran string to C character array
        !
        allocate(c_dimname(len_trim(dimname) + 1))
        call SMIOLf_f_to_c_string(dimname, c_dimname)

        ierr = SMIOL_define_dim(c_file, c_dimname, dimsize)

        deallocate(c_dimname)

    end function SMIOLf_define_dim


    !-----------------------------------------------------------------------
    !  routine SMIOLf_inquire_dim
    !
    !> \brief Inquires about an existing dimension in a file
    !> \details
    !>  Inquire about an existing dimension's size or if a dimension is the
    !>  unlimited dimension or not. If dimsize is present, the size of the dimension
    !>  will be returned in it; likewise, if is_unlimited is present, is_unlimited
    !>  will return either .true. or .false. depending on whether or not the dimension
    !>  is the unlimited dimension or not.
    !>
    !>  For unlimited dimensions, the current size of the dimension is returned;
    !>  future writes of additional records to a file can lead to different return
    !>  sizes for unlimited dimensions.
    !>
    !>  Upon successful completion, SMIOL_SUCCESS is returned; otherwise, an error
    !>  code is returned.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_inquire_dim(file, dimname, dimsize, is_unlimited) result(ierr)

        use iso_c_binding, only : c_char, c_loc, c_ptr, c_null_ptr

        implicit none

        type (SMIOLf_file), target :: file
        character(len=*), intent(in) :: dimname
        integer(kind=SMIOL_offset_kind), intent(out), optional :: dimsize
        logical, intent(out), optional :: is_unlimited

        type (c_ptr) :: c_file
        character(kind=c_char), dimension(:), pointer :: c_dimname
        integer (kind=SMIOL_offset_kind), target :: c_dimsize
        integer (kind=c_int), target :: c_is_unlimited
        type (c_ptr) :: c_dimsize_ptr
        type (c_ptr) :: c_is_unlimited_ptr


        ! C interface definitions
        interface
            function SMIOL_inquire_dim(file, dimname, dimsize, is_unlimited) result(ierr) bind(C, name='SMIOL_inquire_dim')
                use iso_c_binding, only : c_ptr, c_char, c_int
                import SMIOL_offset_kind
                type (c_ptr), value :: file
                character(kind=c_char), dimension(*) :: dimname
                type (c_ptr), value :: dimsize
                type (c_ptr), value :: is_unlimited
                integer(kind=c_int) :: ierr
            end function
        end interface

        ! Get C address of file; there is no need to worry about an unassociated file here,
        ! since the file argument is not a pointer
        c_file = c_loc(file)

        !
        ! Convert Fortran string to C character array
        !
        allocate(c_dimname(len_trim(dimname) + 1))
        call SMIOLf_f_to_c_string(dimname, c_dimname)

        !
        ! Set C dimsize
        !
        if (present(dimsize)) then
            c_dimsize_ptr = c_loc(c_dimsize)
        else
            c_dimsize_ptr = c_null_ptr
        endif

        !
        ! Set C pointer for unlimited dimension inquiry argument
        !
        if (present(is_unlimited)) then
            c_is_unlimited_ptr = c_loc(c_is_unlimited)
        else
            c_is_unlimited_ptr = c_null_ptr
        end if

        ierr = SMIOL_inquire_dim(c_file, c_dimname, c_dimsize_ptr, c_is_unlimited_ptr)

        if (present(dimsize)) then
            dimsize = c_dimsize
        end if

        if (present(is_unlimited)) then
            if (c_is_unlimited == 1) then
                is_unlimited = .true.
            else
                is_unlimited = .false.
            end if
        end if

        deallocate(c_dimname)

    end function SMIOLf_inquire_dim


    !
    ! Variable methods
    !

    !-----------------------------------------------------------------------
    !  routine SMIOLf_define_var
    !
    !> \brief Defines a new variable in a file
    !> \details
    !>  Defines a variable with the specified name, type, and dimensions in an open
    !>  file pointed to by the file argument. The varname and dimnames arguments
    !>  are expected to be null-terminated strings, except if the variable has
    !>  zero dimensions, in which case the dimnames argument is ignored.
    !>
    !>  Unlike the C SMIOL_define_var function, this routine assumes that
    !>  dimnames provides the dimension names in their natural Fortran order,
    !>  with the fastest-varying dimension given first and any unlimited
    !>  dimension given last.
    !>
    !>  Upon successful completion, SMIOL_SUCCESS is returned; otherwise, an error
    !>  code is returned.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_define_var(file, varname, vartype, ndims, dimnames) result(ierr)

        use iso_c_binding, only : c_int, c_char, c_null_char, c_ptr, c_loc, c_null_ptr

        implicit none

        type (SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        integer, intent(in) :: vartype
        integer, intent(in) :: ndims
        character(len=*), dimension(:), intent(in) :: dimnames

        type (c_ptr) :: c_file
        character(kind=c_char), dimension(:), pointer :: c_varname
        integer(kind=c_int) :: c_vartype
        integer(kind=c_int) :: c_ndims

        type (c_ptr) :: c_dimnames_ptr
        type (c_ptr), dimension(:), allocatable, target :: c_dimnames

        integer :: i, j

        ! C interface definitions
        interface
            function SMIOL_define_var(file, varname, vartype, ndims, dimnames) result(ierr) bind(C, name='SMIOL_define_var')
                use iso_c_binding, only : c_ptr, c_char, c_int
                type (c_ptr), value :: file
                character(kind=c_char), dimension(*) :: varname
                integer(kind=c_int), value :: vartype
                integer(kind=c_int), value :: ndims
                type (c_ptr), value :: dimnames
                integer(kind=c_int) :: ierr
            end function
        end interface

        ! Used to store an array of pointers to character arrays
        type string_ptr
            character(kind=c_char), dimension(:), allocatable :: str
        end type string_ptr

        type (string_ptr), dimension(:), allocatable, target :: strings

        !
        ! Check that the 'dimnames' array has at least ndims elements
        !
        if (size(dimnames) < ndims) then
            ierr = SMIOL_FORTRAN_ERROR
            return
        end if

        ! Get C address of file; there is no need to worry about an unassociated file here,
        ! since the file argument is not a pointer
        c_file = c_loc(file)

        !
        ! Convert Fortran string to C character array
        !
        allocate(c_varname(len_trim(varname) + 1))
        call SMIOLf_f_to_c_string(varname, c_varname)

        !
        ! Convert vartype and ndims
        !
        c_vartype = vartype
        c_ndims = ndims

        !
        ! Convert dimnames, reversing their order
        !
        allocate(c_dimnames(ndims))
        allocate(strings(ndims))

        do j=1,ndims
            allocate(strings(j) % str(len_trim(dimnames(ndims-j+1))+1))

            do i=1,len_trim(dimnames(ndims-j+1))
                strings(j) % str(i) = dimnames(ndims-j+1)(i:i)
            end do
            strings(j) % str(i) = c_null_char
            c_dimnames(j) = c_loc(strings(j) % str)
        end do

        if (ndims > 0) then
            c_dimnames_ptr = c_loc(c_dimnames)
        else
            c_dimnames_ptr = c_null_ptr
        end if

        ierr = SMIOL_define_var(c_file, c_varname, c_vartype, c_ndims, c_dimnames_ptr)

        do j=1,ndims
            deallocate(strings(j) % str)
        end do

        deallocate(c_varname)
        deallocate(strings)
        deallocate(c_dimnames)

    end function SMIOLf_define_var


    !-----------------------------------------------------------------------
    !  routine SMIOLf_inquire_var
    !
    !> \brief Inquires about an existing variable in a file
    !> \details
    !>  Inquires about a variable in a file, and optionally returns the type
    !>  of the variable, the dimensionality of the variable, and the names of
    !>  the dimensions of the variable.
    !>
    !>  If the names of a variable's dimensions are requested (by providing an
    !>  actual argument for dimnames), the size of the dimnames array must be at
    !>  least the number of dimensions in the variable, and each character string
    !>  in the dimnames array must be large enough to accommodate the corresponding
    !>  dimension name.
    !>
    !>  Unlike the C SMIOL_inquire_var function, this routine returns the list of
    !>  dimension names in its natural Fortran order, with the fastest-varying
    !>  dimension given first and any unlimited dimension given last.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_inquire_var(file, varname, vartype, ndims, dimnames) result(ierr)

        use iso_c_binding, only : c_char, c_null_char, c_loc, c_ptr, c_null_ptr, c_int

        implicit none

        type (SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        integer, intent(out), optional :: vartype
        integer, intent(out), optional :: ndims
        character(len=*), dimension(:), intent(out), optional :: dimnames

        type (c_ptr) :: c_file
        character(kind=c_char), dimension(:), pointer :: c_varname
        integer(kind=c_int), target :: c_vartype
        integer(kind=c_int), target :: c_ndims
        type (c_ptr), dimension(:), allocatable, target :: c_dimnames

        type (c_ptr) :: c_vartype_ptr
        type (c_ptr) :: c_ndims_ptr
        type (c_ptr) :: c_dimnames_ptr

        integer :: i, j, ndims_in

        ! C interface definitions
        interface
            function SMIOL_inquire_var(file, varname, vartype, ndims, dimnames) result(ierr) bind(C, name='SMIOL_inquire_var')
                use iso_c_binding, only : c_ptr, c_char, c_int
                type (c_ptr), value :: file
                character(kind=c_char), dimension(*) :: varname
                type (c_ptr), value :: vartype
                type (c_ptr), value :: ndims
                type (c_ptr), value :: dimnames
                integer(kind=c_int) :: ierr
            end function
        end interface

        ! Used to store an array of pointers to character arrays
        type string_ptr
            character(kind=c_char), dimension(:), allocatable :: str
        end type string_ptr

        type (string_ptr), dimension(:), allocatable, target :: strings


        ! Get C address of file; there is no need to worry about an unassociated file here,
        ! since the file argument is not a pointer
        c_file = c_loc(file)

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        call SMIOLf_f_to_c_string(varname, c_varname)

        !
        ! Set C pointer for variable type
        !
        if (present(vartype)) then
            c_vartype_ptr = c_loc(c_vartype)
        else
            c_vartype_ptr = c_null_ptr
        end if

        !
        ! Set C pointer for number of dimensions
        ! This is done even if dimnames is requested but ndims is not,
        ! since c_ndims may be used later on when copying out strings
        ! to dimnames.
        !
        if (present(ndims) .or. present(dimnames)) then
            c_ndims_ptr = c_loc(c_ndims)
        else
            c_ndims_ptr = c_null_ptr
        end if

        !
        ! Set C pointers for dimension names in C order
        !
        if (present(dimnames)) then
            ndims_in = size(dimnames)
            allocate(c_dimnames(ndims_in))
            allocate(strings(ndims_in))

            do j=1,ndims_in
                allocate(strings(j) % str(len(dimnames(ndims_in-j+1))+1))
                c_dimnames(j) = c_loc(strings(j) % str)
            end do
            c_dimnames_ptr = c_loc(c_dimnames)
        else
            c_dimnames_ptr = c_null_ptr
        end if


        ierr = SMIOL_inquire_var(c_file, c_varname, c_vartype_ptr, c_ndims_ptr, c_dimnames_ptr)

        deallocate(c_varname)

        if (ierr /= SMIOL_SUCCESS) then
            return
        end if

        !
        ! Copy variable type to output argument
        !
        if (present(vartype)) then
            vartype = c_vartype
        end if

        !
        ! Copy number of dimensions to output argument
        !
        if (present(ndims)) then
            ndims = c_ndims
        end if

        !
        ! Copy dimension names to output argument, reversing their order
        !
        if (present(dimnames)) then
            do j=1,c_ndims
                do i=1,len(dimnames(c_ndims-j+1))
                    if (strings(j) % str(i) == c_null_char) exit
                end do

                i = i - 1

                dimnames(c_ndims-j+1)(1:i) = transfer(strings(j) % str(1:i), dimnames(c_ndims-j+1))
                dimnames(c_ndims-j+1) = dimnames(c_ndims-j+1)(1:i)
            end do

            do j=1,ndims_in
                deallocate(strings(j) % str)
            end do
            deallocate(strings)
            deallocate(c_dimnames)
        end if

    end function SMIOLf_inquire_var


#include "smiolf_put_get_var.inc"


    !
    ! Attribute methods
    !

    !-----------------------------------------------------------------------
    !  routine SMIOLf_define_att_int
    !
    !> \brief Defines a new integer attribute
    !> \details
    !>  Defines a new integer attribute for a variable if varname is not
    !>  an empty string, or a global attribute otherwise.
    !>
    !>  If the attribute has been successfully defined for the variable or file,
    !>  SMIOL_SUCCESS is returned.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_define_att_int(file, varname, att_name, att) result(ierr)

        use iso_c_binding, only : c_char, c_int, c_null_char, c_null_ptr, c_ptr, c_loc

        implicit none

        ! Arguments
        type (SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        character(len=*), intent(in) :: att_name
        integer(kind=c_int), intent(in), target :: att

        ! Local variables
        integer :: i
        type (c_ptr) :: c_file
        character(kind=c_char), dimension(:), allocatable, target :: c_varname
        character(kind=c_char), dimension(:), pointer :: c_att_name
        type (c_ptr) :: att_ptr
        type (c_ptr) :: c_varname_ptr


        c_file = c_loc(file)

        !
        ! Convert Fortran string to C character array
        !
        if (len_trim(varname) > 0) then
            allocate(c_varname(len_trim(varname) + 1))
            do i=1,len_trim(varname)
                c_varname(i) = varname(i:i)
            end do
            c_varname(i) = c_null_char
            c_varname_ptr = c_loc(c_varname)
        else
            c_varname_ptr = c_null_ptr
        end if

        allocate(c_att_name(len_trim(att_name) + 1))
        do i=1,len_trim(att_name)
            c_att_name(i) = att_name(i:i)
        end do
        c_att_name(i) = c_null_char

        att_ptr = c_loc(att)

        ierr = SMIOL_define_att(c_file, c_varname_ptr, c_att_name, SMIOL_INT32, att_ptr)

        if (len_trim(varname) > 0) then
            deallocate(c_varname)
        end if
        deallocate(c_att_name)

    end function SMIOLf_define_att_int


    !-----------------------------------------------------------------------
    !  routine SMIOLf_define_att_float
    !
    !> \brief Defines a new float attribute
    !> \details
    !>  Defines a new float attribute for a variable if varname is not an empty
    !>  string, or a global attribute otherwise.
    !>
    !>  If the attribute has been successfully defined for the variable or file,
    !>  SMIOL_SUCCESS is returned.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_define_att_float(file, varname, att_name, att) result(ierr)

        use iso_c_binding, only : c_char, c_float, c_null_char, c_null_ptr, c_ptr, c_loc

        implicit none

        ! Arguments
        type (SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        character(len=*), intent(in) :: att_name
        real(kind=c_float), intent(in), target :: att

        ! Local variables
        integer :: i
        type (c_ptr) :: c_file
        character(kind=c_char), dimension(:), allocatable, target :: c_varname
        character(kind=c_char), dimension(:), pointer :: c_att_name
        type (c_ptr) :: att_ptr
        type (c_ptr) :: c_varname_ptr


        c_file = c_loc(file)

        !
        ! Convert Fortran string to C character array
        !
        if (len_trim(varname) > 0) then
            allocate(c_varname(len_trim(varname) + 1))
            do i=1,len_trim(varname)
                c_varname(i) = varname(i:i)
            end do
            c_varname(i) = c_null_char
            c_varname_ptr = c_loc(c_varname)
        else
            c_varname_ptr = c_null_ptr
        end if

        allocate(c_att_name(len_trim(att_name) + 1))
        do i=1,len_trim(att_name)
            c_att_name(i) = att_name(i:i)
        end do
        c_att_name(i) = c_null_char

        att_ptr = c_loc(att)

        ierr = SMIOL_define_att(c_file, c_varname_ptr, c_att_name, SMIOL_REAL32, att_ptr)

        if (len_trim(varname) > 0) then
            deallocate(c_varname)
        end if
        deallocate(c_att_name)

    end function SMIOLf_define_att_float


    !-----------------------------------------------------------------------
    !  routine SMIOLf_define_att_double
    !
    !> \brief Defines a new double attribute
    !> \details
    !>  Defines a new double attribute for a variable if varname is not an empty
    !>  string, or a global attribute otherwise.
    !>
    !>  If the attribute has been successfully defined for the variable or file,
    !>  SMIOL_SUCCESS is returned.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_define_att_double(file, varname, att_name, att) result(ierr)

        use iso_c_binding, only : c_char, c_double, c_null_char, c_null_ptr, c_ptr, c_loc

        implicit none

        ! Arguments
        type (SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        character(len=*), intent(in) :: att_name
        real(kind=c_double), intent(in), target :: att

        ! Local variables
        integer :: i
        type (c_ptr) :: c_file
        character(kind=c_char), dimension(:), allocatable, target :: c_varname
        character(kind=c_char), dimension(:), pointer :: c_att_name
        type (c_ptr) :: att_ptr
        type (c_ptr) :: c_varname_ptr


        c_file = c_loc(file)

        !
        ! Convert Fortran string to C character array
        !
        if (len_trim(varname) > 0) then
            allocate(c_varname(len_trim(varname) + 1))
            do i=1,len_trim(varname)
                c_varname(i) = varname(i:i)
            end do
            c_varname(i) = c_null_char
            c_varname_ptr = c_loc(c_varname)
        else
            c_varname_ptr = c_null_ptr
        end if

        allocate(c_att_name(len_trim(att_name) + 1))
        do i=1,len_trim(att_name)
            c_att_name(i) = att_name(i:i)
        end do
        c_att_name(i) = c_null_char

        att_ptr = c_loc(att)

        ierr = SMIOL_define_att(c_file, c_varname_ptr, c_att_name, SMIOL_REAL64, att_ptr)

        if (len_trim(varname) > 0) then
            deallocate(c_varname)
        end if
        deallocate(c_att_name)

    end function SMIOLf_define_att_double


    !-----------------------------------------------------------------------
    !  routine SMIOLf_define_att_text
    !
    !> \brief Defines a new text attribute
    !> \details
    !>  Defines a new text attribute for a variable if varname is not an empty
    !>  string, or a global attribute otherwise.
    !>
    !>  If the attribute has been successfully defined for the variable or file,
    !>  SMIOL_SUCCESS is returned.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_define_att_text(file, varname, att_name, att) result(ierr)

        use iso_c_binding, only : c_char, c_null_char, c_null_ptr, c_ptr, c_loc

        implicit none

        ! Arguments
        type (SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        character(len=*), intent(in) :: att_name
        character(len=*), intent(in) :: att

        ! Local variables
        integer :: i
        type (c_ptr) :: c_file
        character(kind=c_char), dimension(:), allocatable, target :: c_varname
        character(kind=c_char), dimension(:), pointer :: c_att_name
        character(kind=c_char), dimension(:), allocatable, target :: c_att
        type (c_ptr) :: att_ptr
        type (c_ptr) :: c_varname_ptr


        c_file = c_loc(file)

        !
        ! Convert Fortran string to C character array
        !
        if (len_trim(varname) > 0) then
            allocate(c_varname(len_trim(varname) + 1))
            do i=1,len_trim(varname)
                c_varname(i) = varname(i:i)
            end do
            c_varname(i) = c_null_char
            c_varname_ptr = c_loc(c_varname)
        else
            c_varname_ptr = c_null_ptr
        end if

        allocate(c_att_name(len_trim(att_name) + 1))
        do i=1,len_trim(att_name)
            c_att_name(i) = att_name(i:i)
        end do
        c_att_name(i) = c_null_char

        allocate(c_att(len_trim(att) + 1))
        do i=1,len_trim(att)
            c_att(i) = att(i:i)
        end do
        c_att(i) = c_null_char

        att_ptr = c_loc(c_att)

        ierr = SMIOL_define_att(c_file, c_varname_ptr, c_att_name, SMIOL_CHAR, att_ptr)

        if (len_trim(varname) > 0) then
            deallocate(c_varname)
        end if
        deallocate(c_att_name)
        deallocate(c_att)

    end function SMIOLf_define_att_text


    !-----------------------------------------------------------------------
    !  routine SMIOLf_inquire_att_int
    !
    !> \brief Inquires about an integer attribute
    !> \details
    !>  Inquires about a variable attribute if varname is not an empty string,
    !>  or a global attribute otherwise.
    !>
    !>  If the requested attribute is found, and if it is integer-valued, then
    !>  SMIOL_SUCCESS is returned and the att output argument will contain
    !>  the attribute value. If the attribute was found, but it is not an integer
    !>  attribute, SMIOL_WRONG_ARG_TYPE is returned, and the contents of att are
    !>  undefined.
    !>
    !>  If SMIOL was not compiled with support for any file library, this routine
    !>  will always return SMIOL_WRONG_ARG_TYPE.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_inquire_att_int(file, varname, att_name, att) result(ierr)

        use iso_c_binding, only : c_char, c_int, c_null_char, c_null_ptr, c_ptr, c_loc

        implicit none

        ! Arguments
        type (SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        character(len=*), intent(in) :: att_name
        integer(kind=c_int), intent(out), target :: att

        ! Local variables
        integer :: i
        integer(kind=c_int), target :: att_type
        type (c_ptr) :: c_file
        character(kind=c_char), dimension(:), allocatable, target :: c_varname
        character(kind=c_char), dimension(:), pointer :: c_att_name
        type (c_ptr) :: att_ptr
        type (c_ptr) :: att_type_ptr
        type (c_ptr) :: c_varname_ptr


        c_file = c_loc(file)
        att_type_ptr = c_loc(att_type)

        !
        ! Convert Fortran string to C character array
        !
        if (len_trim(varname) > 0) then
            allocate(c_varname(len_trim(varname) + 1))
            do i=1,len_trim(varname)
                c_varname(i) = varname(i:i)
            end do
            c_varname(i) = c_null_char
            c_varname_ptr = c_loc(c_varname)
        else
            c_varname_ptr = c_null_ptr
        end if

        allocate(c_att_name(len_trim(att_name) + 1))
        do i=1,len_trim(att_name)
            c_att_name(i) = att_name(i:i)
        end do
        c_att_name(i) = c_null_char

        !
        ! First, inquire about the attribute type
        !
        ierr = SMIOL_inquire_att(c_file, c_varname_ptr, c_att_name, &
                                 att_type_ptr, c_null_ptr, c_null_ptr)

        if (ierr /= SMIOL_SUCCESS .or. att_type /= SMIOL_INT32) then
            if (len_trim(varname) > 0) then
                deallocate(c_varname)
            end if
            deallocate(c_att_name)
            if (ierr == SMIOL_SUCCESS) then
                ierr = SMIOL_WRONG_ARG_TYPE
            end if
            return
        end if

        att_ptr = c_loc(att)

        ierr = SMIOL_inquire_att(c_file, c_varname_ptr, c_att_name, &
                                 c_null_ptr, c_null_ptr, att_ptr)

        if (len_trim(varname) > 0) then
            deallocate(c_varname)
        end if
        deallocate(c_att_name)

    end function SMIOLf_inquire_att_int


    !-----------------------------------------------------------------------
    !  routine SMIOLf_inquire_att_float
    !
    !> \brief Inquires about a float attribute
    !> \details
    !>  Inquires about a variable attribute if varname is not an empty string,
    !>  or a global attribute otherwise.
    !>
    !>  If the requested attribute is found, and if it is float-valued, then
    !>  SMIOL_SUCCESS is returned and the att output argument will contain
    !>  the attribute value. If the attribute was found, but it is not a float
    !>  attribute, SMIOL_WRONG_ARG_TYPE is returned, and the contents of att are
    !>  undefined.
    !>
    !>  If SMIOL was not compiled with support for any file library, this routine
    !>  will always return SMIOL_WRONG_ARG_TYPE.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_inquire_att_float(file, varname, att_name, att) result(ierr)

        use iso_c_binding, only : c_char, c_float, c_null_char, c_null_ptr, c_ptr, c_loc

        implicit none

        ! Arguments
        type (SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        character(len=*), intent(in) :: att_name
        real(kind=c_float), intent(out), target :: att

        ! Local variables
        integer :: i
        integer(kind=c_int), target :: att_type
        type (c_ptr) :: c_file
        character(kind=c_char), dimension(:), allocatable, target :: c_varname
        character(kind=c_char), dimension(:), pointer :: c_att_name
        type (c_ptr) :: att_ptr
        type (c_ptr) :: att_type_ptr
        type (c_ptr) :: c_varname_ptr


        c_file = c_loc(file)
        att_type_ptr = c_loc(att_type)

        !
        ! Convert Fortran string to C character array
        !
        if (len_trim(varname) > 0) then
            allocate(c_varname(len_trim(varname) + 1))
            do i=1,len_trim(varname)
                c_varname(i) = varname(i:i)
            end do
            c_varname(i) = c_null_char
            c_varname_ptr = c_loc(c_varname)
        else
            c_varname_ptr = c_null_ptr
        end if

        allocate(c_att_name(len_trim(att_name) + 1))
        do i=1,len_trim(att_name)
            c_att_name(i) = att_name(i:i)
        end do
        c_att_name(i) = c_null_char

        !
        ! First, inquire about the attribute type
        !
        ierr = SMIOL_inquire_att(c_file, c_varname_ptr, c_att_name, &
                                 att_type_ptr, c_null_ptr, c_null_ptr)

        if (ierr /= SMIOL_SUCCESS .or. att_type /= SMIOL_REAL32) then
            if (len_trim(varname) > 0) then
                deallocate(c_varname)
            end if
            deallocate(c_att_name)
            if (ierr == SMIOL_SUCCESS) then
                ierr = SMIOL_WRONG_ARG_TYPE
            end if
            return
        end if

        att_ptr = c_loc(att)

        ierr = SMIOL_inquire_att(c_file, c_varname_ptr, c_att_name, &
                                 c_null_ptr, c_null_ptr, att_ptr)

        if (len_trim(varname) > 0) then
            deallocate(c_varname)
        end if
        deallocate(c_att_name)

    end function SMIOLf_inquire_att_float


    !-----------------------------------------------------------------------
    !  routine SMIOLf_inquire_att_double
    !
    !> \brief Inquires about a double attribute
    !> \details
    !>  Inquires about a variable attribute if varname is not an empty string,
    !>  or a global attribute otherwise.
    !>
    !>  If the requested attribute is found, and if it is double-valued, then
    !>  SMIOL_SUCCESS is returned and the att output argument will contain
    !>  the attribute value. If the attribute was found, but it is not a double
    !>  attribute, SMIOL_WRONG_ARG_TYPE is returned, and the contents of att are
    !>  undefined.
    !>
    !>  If SMIOL was not compiled with support for any file library, this routine
    !>  will always return SMIOL_WRONG_ARG_TYPE.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_inquire_att_double(file, varname, att_name, att) result(ierr)

        use iso_c_binding, only : c_char, c_double, c_null_char, c_null_ptr, c_ptr, c_loc

        implicit none

        ! Arguments
        type (SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        character(len=*), intent(in) :: att_name
        real(kind=c_double), intent(out), target :: att

        ! Local variables
        integer :: i
        integer(kind=c_int), target :: att_type
        type (c_ptr) :: c_file
        character(kind=c_char), dimension(:), allocatable, target :: c_varname
        character(kind=c_char), dimension(:), pointer :: c_att_name
        type (c_ptr) :: att_ptr
        type (c_ptr) :: att_type_ptr
        type (c_ptr) :: c_varname_ptr


        c_file = c_loc(file)
        att_type_ptr = c_loc(att_type)

        !
        ! Convert Fortran string to C character array
        !
        if (len_trim(varname) > 0) then
            allocate(c_varname(len_trim(varname) + 1))
            do i=1,len_trim(varname)
                c_varname(i) = varname(i:i)
            end do
            c_varname(i) = c_null_char
            c_varname_ptr = c_loc(c_varname)
        else
            c_varname_ptr = c_null_ptr
        end if

        allocate(c_att_name(len_trim(att_name) + 1))
        do i=1,len_trim(att_name)
            c_att_name(i) = att_name(i:i)
        end do
        c_att_name(i) = c_null_char

        !
        ! First, inquire about the attribute type
        !
        ierr = SMIOL_inquire_att(c_file, c_varname_ptr, c_att_name, &
                                 att_type_ptr, c_null_ptr, c_null_ptr)

        if (ierr /= SMIOL_SUCCESS .or. att_type /= SMIOL_REAL64) then
            if (len_trim(varname) > 0) then
                deallocate(c_varname)
            end if
            deallocate(c_att_name)
            if (ierr == SMIOL_SUCCESS) then
                ierr = SMIOL_WRONG_ARG_TYPE
            end if
            return
        end if

        att_ptr = c_loc(att)

        ierr = SMIOL_inquire_att(c_file, c_varname_ptr, c_att_name, &
                                 c_null_ptr, c_null_ptr, att_ptr)

        if (len_trim(varname) > 0) then
            deallocate(c_varname)
        end if
        deallocate(c_att_name)

    end function SMIOLf_inquire_att_double


    !-----------------------------------------------------------------------
    !  routine SMIOLf_inquire_att_text
    !
    !> \brief Inquires about a text attribute
    !> \details
    !>  Inquires about a variable attribute if varname is not an empty string,
    !>  or a global attribute otherwise.
    !>
    !>  If the requested attribute is found, if it is character-valued, and if
    !>  the att output argument is long enough to contain the attribute value,
    !>  then SMIOL_SUCCESS is returned and the att output argument will contain
    !>  the attribute value. If the attribute was found, but it is not a character
    !>  attribute, SMIOL_WRONG_ARG_TYPE is returned, and the contents of att are
    !>  undefined. If the attribute was found, and it is a character attribute,
    !>  but the att output argument is not long enough to contain the attribute
    !>  value, then SMIOL_INSUFFICIENT_ARG is returned, and the contents of att
    !>  are undefined.
    !>
    !>  If SMIOL was not compiled with support for any file library, this routine
    !>  will always return SMIOL_WRONG_ARG_TYPE.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_inquire_att_text(file, varname, att_name, att) result(ierr)

        use iso_c_binding, only : c_char, c_int, c_null_char, c_null_ptr, c_ptr, c_loc

        implicit none

        ! Arguments
        type (SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        character(len=*), intent(in) :: att_name
        character(len=*), intent(out) :: att

        ! Local variables
        integer :: i
        integer(kind=c_int), target :: att_type
        integer(kind=SMIOL_offset_kind), target :: att_len
        type (c_ptr) :: c_file
        character(kind=c_char), dimension(:), allocatable, target :: c_varname
        character(kind=c_char), dimension(:), pointer :: c_att_name
        character(kind=c_char), dimension(:), allocatable, target :: c_att
        type (c_ptr) :: att_ptr
        type (c_ptr) :: att_type_ptr
        type (c_ptr) :: att_len_ptr
        type (c_ptr) :: c_varname_ptr


        c_file = c_loc(file)
        att_type_ptr = c_loc(att_type)
        att_len_ptr = c_loc(att_len)
        c_file = c_loc(file)

        !
        ! Convert Fortran string to C character array
        !
        if (len_trim(varname) > 0) then
            allocate(c_varname(len_trim(varname) + 1))
            do i=1,len_trim(varname)
                c_varname(i) = varname(i:i)
            end do
            c_varname(i) = c_null_char
            c_varname_ptr = c_loc(c_varname)
        else
            c_varname_ptr = c_null_ptr
        end if

        allocate(c_att_name(len_trim(att_name) + 1))
        do i=1,len_trim(att_name)
            c_att_name(i) = att_name(i:i)
        end do
        c_att_name(i) = c_null_char

        !
        ! First, inquire about the attribute type and length
        !
        ierr = SMIOL_inquire_att(c_file, c_varname_ptr, c_att_name, &
                                 att_type_ptr, att_len_ptr, c_null_ptr)

        if (ierr /= SMIOL_SUCCESS .or. att_type /= SMIOL_CHAR) then
            if (len_trim(varname) > 0) then
                deallocate(c_varname)
            end if
            deallocate(c_att_name)
            if (ierr == SMIOL_SUCCESS) then
                ierr = SMIOL_WRONG_ARG_TYPE
            end if
            return
        end if

        if (len(att) < att_len) then
            if (len_trim(varname) > 0) then
                deallocate(c_varname)
            end if
            deallocate(c_att_name)
            ierr = SMIOL_INSUFFICIENT_ARG
            return
        end if

        !
        ! Next, allocate a local c_char array
        !
        allocate(c_att(att_len))
        att_ptr = c_loc(c_att)

        !
        ! Finally, inquire about the attribute itself
        !
        ierr = SMIOL_inquire_att(c_file, c_varname_ptr, c_att_name, &
                                 c_null_ptr, c_null_ptr, att_ptr)

        !
        ! Copy c_char array to Fortran string
        !
        att(1:att_len) = transfer(c_att(1:att_len), att)
        att = att(1:att_len)

        if (len_trim(varname) > 0) then
            deallocate(c_varname)
        end if
        deallocate(c_att_name)
        deallocate(c_att)

    end function SMIOLf_inquire_att_text


    !
    ! Control methods
    !

    !-----------------------------------------------------------------------
    !  routine SMIOLf_sync_file
    !
    !> \brief Forces all in-memory data to be flushed to disk
    !> \details
    !> Upon success, all in-memory data for the file associatd with the file
    !> handle will be flushed to the file system and SMIOL_SUCCESS will be
    !> returned; otherwise, an error code is returned.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_sync_file(file) result(ierr)

        use iso_c_binding, only : c_ptr, c_loc, c_null_ptr

        implicit none

        type (SMIOLf_file), pointer :: file
        type (c_ptr) :: c_file

        interface
            function SMIOL_sync_file(file) result(ierr) bind(C, name='SMIOL_sync_file')
                use iso_c_binding, only : c_ptr, c_int
                type(c_ptr), value :: file
                integer(kind=c_int) :: ierr
            end function
        end interface

        c_file = c_null_ptr

        if (associated(file)) then
            c_file = c_loc(file)
        end if

        ierr = SMIOL_sync_file(c_file)

    end function SMIOLf_sync_file


    !-----------------------------------------------------------------------
    !  routine SMIOLf_error_string
    !
    !> \brief Returns an error string for a specified error code
    !> \details
    !>  Returns an error string corresponding to a SMIOL error code. If the error code is
    !>  SMIOL_LIBRARY_ERROR and a valid SMIOL context is available, the SMIOLf_lib_error_string
    !>  function should be called instead.
    !>
    !>  The error string is always of length 128, and so it is recommended to trim
    !>  the string before it is printed.
    !
    !-----------------------------------------------------------------------
    character(len=128) function SMIOLf_error_string(ierrno) result(err_mesg)

        use iso_c_binding, only : c_ptr, c_char, c_null_char, c_f_pointer

        implicit none

        integer, intent(in) :: ierrno

        type (c_ptr) :: c_mesg_ptr
        character(kind=c_char), dimension(:), pointer :: c_mesg
        integer :: i
    
        ! C interface definitions
        interface
            function SMIOL_error_string(errno) result(err_mesg) bind(C, name='SMIOL_error_string')
                use iso_c_binding, only : c_int, c_ptr
                integer(kind=c_int), value :: errno
                type (c_ptr) :: err_mesg
            end function
        end interface

        c_mesg_ptr = SMIOL_error_string(ierrno)
        call c_f_pointer(c_mesg_ptr, c_mesg, shape=[len(err_mesg)])

        do i=1,len(err_mesg)
            if (c_mesg(i) == c_null_char) exit
        end do

        i = i - 1

        err_mesg(1:i) = transfer(c_mesg(1:i), err_mesg)
        err_mesg = err_mesg(1:i)

    end function SMIOLf_error_string


    !-----------------------------------------------------------------------
    !  routine SMIOLf_lib_error_string
    !
    !> \brief Returns an error string for a third-party library called by SMIOL
    !> \details
    !>  Returns an error string corresponding to an error that was generated by
    !>  a third-party library that was called by SMIOL. The library that was the source
    !>  of the error, as well as the library-specific error code, are retrieved from
    !>  a SMIOL context. If successive library calls resulted in errors, only the error
    !>  string for the last of these errors will be returned. 
    !>
    !>  The error string is always of length 128, and so it is recommended to trim
    !>  the string before it is printed.
    !
    !-----------------------------------------------------------------------
    character(len=128) function SMIOLf_lib_error_string(context) result(err_mesg)

        use iso_c_binding, only : c_ptr, c_null_ptr, c_char, c_null_char, c_f_pointer, c_loc

        implicit none

        type (SMIOLf_context), target :: context

        type (c_ptr) :: c_context = c_null_ptr
        type (c_ptr) :: c_mesg_ptr = c_null_ptr
        character(kind=c_char), dimension(:), pointer :: c_mesg => null()
        integer :: i

        ! C interface definitions
        interface
            function SMIOL_lib_error_string(context) result(err_mesg) bind(C, name='SMIOL_lib_error_string')
                use iso_c_binding, only : c_ptr
                type(c_ptr), value :: context
                type (c_ptr) :: err_mesg
            end function
        end interface

        c_context = c_loc(context)

        c_mesg_ptr = SMIOL_lib_error_string(c_context)
        call c_f_pointer(c_mesg_ptr, c_mesg, shape=[len(err_mesg)])

        do i=1,len(err_mesg)
            if (c_mesg(i) == c_null_char) exit
        end do

        i = i - 1

        err_mesg(1:i) = transfer(c_mesg(1:i), err_mesg)
        err_mesg = err_mesg(1:i)

    end function SMIOLf_lib_error_string


    !-----------------------------------------------------------------------
    !  routine SMIOLf_set_option
    !
    !> \brief Sets an option for the SMIOL library
    !> \details
    !>  Detailed description of what this routine does.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_set_option() result(ierr)

        implicit none

        ierr = 0

    end function SMIOLf_set_option

    !-----------------------------------------------------------------------
    !  routine SMIOLf_set_frame
    !
    !> \brief Set the frame of an open file
    !> \details
    !> For an open SMIOL file handle, set the frame for the unlimited dimension.
    !> After setting the frame for a file, writing to a variable that is
    !> dimensioned by the unlimited dimension will write to the last set frame,
    !> overwriting any current data that maybe present in that frame.
    !>
    !> SMIOL_SUCCESS will be returned if the frame is successfully set otherwise an
    !> error will return.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_set_frame(file, frame) result(ierr)

        use iso_c_binding, only : c_loc, c_ptr

        implicit none

        type (SMIOLf_file), target :: file
        integer (kind=SMIOL_offset_kind), value, intent(in) :: frame

        type (c_ptr) :: c_file

        ! C interface definitions
        interface
            function SMIOL_set_frame(file, frame) result(ierr) bind(C, name='SMIOL_set_frame')
                use iso_c_binding, only : c_ptr, c_int
                import SMIOL_offset_kind
                type (c_ptr), value :: file
                integer (kind=SMIOL_offset_kind), value :: frame
                integer (kind=c_int) :: ierr
            end function
        end interface

        c_file = c_loc(file)
        ierr = SMIOL_set_frame(c_file, frame)

    end function SMIOLf_set_frame

    !-----------------------------------------------------------------------
    !  routine SMIOLf_get_frame
    !
    !> \brief Get the frame of an open file
    !> \details
    !> Get the current frame of an open file. Upon success, SMIOL_SUCCESS will be
    !> returned, otherwise an error will be returned.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_get_frame(file, frame) result(ierr)

        use iso_c_binding, only : c_ptr, c_loc

        implicit none

        type (SMIOLf_file), target, intent(in) :: file
        integer (kind=SMIOL_offset_kind), target, intent(out) :: frame

        type (c_ptr) :: c_file
        type (c_ptr) :: c_frame

        ! C interface definitions
        interface
            function SMIOL_get_frame(file, frame) result(ierr) bind(C, name='SMIOL_get_frame')
                use iso_c_binding, only : c_ptr, c_int
                type (c_ptr), value :: file
                type (c_ptr), value :: frame
                integer (kind=c_int) :: ierr
            end function
        end interface

        c_file = c_loc(file)
        c_frame = c_loc(frame)
        ierr = SMIOL_get_frame(c_file, c_frame)

    end function SMIOLf_get_frame


    !-----------------------------------------------------------------------
    !  routine SMIOLf_create_decomp
    !
    !> \brief Creates a mapping between compute elements and I/O elements
    !> \details
    !>  Given arrays of global element IDs that each task computes, this routine
    !>  works out a mapping of elements between compute and I/O tasks.
    !>
    !>  The aggregation factor is used to indicate the size of subsets of ranks
    !>  that will gather fields onto a single rank in each subset before transferring
    !>  that field from compute to output tasks; in a symmetric way, it also
    !>  indicates the size of subsets over which fields will be scattered after they
    !>  are transferred from input tasks to a single compute tasks in each subset.
    !>
    !>  An aggregation factor of 0 indicates that the implementation should choose
    !>  a suitable aggregation factor (usually matching the size of shared-memory
    !>  domains), while a positive integer specifies a specific size for task groups
    !>  to be used for aggregation.
    !>
    !>  If the optional aggregation_factor argument is not given, it defaults to
    !>  a value of 0.
    !>
    !>  If all input arguments are determined to be valid and if the routine is
    !>  successful in working out a mapping, the decomp pointer is allocated
    !>  and given valid contents, and SMIOL_SUCCESS is returned; otherwise
    !>  a non-success error code is returned and the decomp pointer is unassociated.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_create_decomp(context, n_compute_elements, compute_elements, decomp, &
                                          aggregation_factor) result(ierr)

        use iso_c_binding, only : c_int, c_size_t, c_ptr, c_null_ptr, c_loc, c_f_pointer, c_associated

        implicit none

        ! Arguments
        type (SMIOLf_context), target, intent(in) :: context
        integer(kind=c_size_t), intent(in) :: n_compute_elements
        integer(kind=SMIOL_offset_kind), dimension(n_compute_elements), target, intent(in) :: compute_elements
        type (SMIOLf_decomp), pointer, intent(inout) :: decomp
        integer, intent(in), optional :: aggregation_factor

        ! Local variables
        type (c_ptr) :: c_context
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_compute_elements
        integer(kind=c_int) :: c_agg_factor
        
        interface
            function SMIOL_create_decomp(context, n_compute_elements, compute_elements, aggregation_factor, decomp) &
                                         result(ierr) bind(C, name='SMIOL_create_decomp')
                use iso_c_binding, only : c_size_t, c_ptr, c_int
                type (c_ptr), value :: context
                integer(c_size_t), value :: n_compute_elements
                type (c_ptr), value :: compute_elements
                integer(kind=c_int), value :: aggregation_factor
                integer(kind=c_int) :: ierr
                type (c_ptr) :: decomp
            end function
        end interface


        if (present(aggregation_factor)) then
            c_agg_factor = aggregation_factor
        else
            c_agg_factor = 0    ! Let SMIOL choose its own aggregation factor
        end if

        ! Get C pointers to Fortran types
        c_context = c_loc(context)
        if (size(compute_elements) > 0) then
            c_compute_elements = c_loc(compute_elements)
        else
            c_compute_elements = c_null_ptr
        end if

        c_decomp = c_null_ptr

        ierr = SMIOL_create_decomp(c_context, n_compute_elements, c_compute_elements, c_agg_factor, c_decomp)

        ! Error check and translate c_decomp pointer into a Fortran SMIOLf_decomp pointer
        if (ierr == SMIOL_SUCCESS) then
            if (c_associated(c_decomp)) then
                call c_f_pointer(c_decomp, decomp)
            else
                nullify(decomp)
                ierr = SMIOL_FORTRAN_ERROR
            end if
        else
            nullify(decomp)
            if (c_associated(c_decomp)) then
                ierr = SMIOL_FORTRAN_ERROR
            endif
        end if

    end function SMIOLf_create_decomp


    !-----------------------------------------------------------------------
    !  routine SMIOLf_free_decomp
    !
    !> \brief Frees a mapping between compute elements and I/O elements
    !> \details
    !>  Frees all memory of a SMIOLf_decomp and returns SMIOL_SUCCESS. If 
    !>  decomp is unassociated, nothing will be done and SMIOL_SUCCESS will 
    !>  be returned. After this function has been called, no other SMIOL 
    !>  routines should use the freed SMIOL_decomp.
    !
    !-----------------------------------------------------------------------
    integer function SMIOLf_free_decomp(decomp) result(ierr)

        use iso_c_binding, only : c_ptr, c_loc, c_associated, c_null_ptr

        implicit none

        type(SMIOLF_decomp), pointer, intent(inout) :: decomp
        type(c_ptr) :: c_decomp = c_null_ptr

        interface
            function SMIOL_free_decomp(decomp) result(ierr) bind(C, name='SMIOL_free_decomp')
                use iso_c_binding, only : c_ptr, c_int
                type(c_ptr) :: decomp
                integer(kind=c_int) :: ierr
            end function
        end interface

        ierr = SMIOL_SUCCESS

        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        endif
        ierr = SMIOL_free_decomp(c_decomp)

        if (ierr == SMIOL_SUCCESS) then
            if (c_associated(c_decomp)) then
                ierr = SMIOL_FORTRAN_ERROR
            else
                nullify(decomp)
            end if
        else
            if (.not. c_associated(c_decomp)) then
                nullify(decomp)
            end if
        end if

    end function SMIOLf_free_decomp

    !-----------------------------------------------------------------------
    !  routine SMIOLf_f_to_c_string
    !
    !> \brief Convert a Fortran string to a C null-terminated character array
    !> \details
    !>  Converts a Fortran string to a C null-terminated character array.
    !>  The cstring output argument must be large enough to contain the trimmed
    !>  Fortran string plus at least one c_null_char character. Any characters
    !>  beyond len_trim(fstring) of cstring will be filled with c_null_char
    !>  characters. If the size of cstring  is less than len_trim(fstring)+1,
    !>  then only size(cstring)-1 characters from fstring will be copied into
    !>  cstring before the final c_null_char character is added.
    !
    !-----------------------------------------------------------------------
    subroutine SMIOLf_f_to_c_string(fstring, cstring)

        use iso_c_binding, only : c_char, c_null_char

        implicit none

        character(len=*), intent(in) :: fstring
        character(kind=c_char), dimension(:), intent(out) :: cstring

        integer :: i
        integer :: nchar

        if (size(cstring) <= 0) then
            return
        end if

        nchar = min(size(cstring)-1, len_trim(fstring))

        do i = 1, nchar
            cstring(i) = fstring(i:i)
        end do
        cstring(nchar+1:size(cstring)) = c_null_char

    end subroutine SMIOLf_f_to_c_string

end module SMIOLf
