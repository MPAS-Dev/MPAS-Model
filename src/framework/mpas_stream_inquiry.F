! Copyright (c) 2023 The University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at https://mpas-dev.github.io/license.html .
!
!-----------------------------------------------------------------------
!  mpas_stream_inquiry
!
!> \brief Enables inquiries of the contents of the streams.<core> file
!> \author Michael Duda
!> \date   15 November 2023
!> \details
!>  This module provides a method for instantiating a new MPAS_streamInfo_type
!>  type, as well as routines that may be invoked from that instance to query
!>  the contents of a streams XML file.
!>
!>  Example usage to determine the value of the "input_interval" attribute
!>  for the "foo" stream:
!>
!>     type (MPAS_streamInfo_type), pointer :: streamInfo
!>     character(len=StrKIND) :: attvalue
!>     integer :: ierr
!>
!>     streamInfo => MPAS_stream_inquiry_new_streaminfo()
!>
!>     ierr = streamInfo % init(dminfo % comm, 'streams.test')
!>
!>     if (streamInfo % query('foo', attname='input_interval', attvalue=attvalue)) then
!>        call mpas_log_write('input_interval = '//trim(attvalue))
!>     end if
!>
!>     ierr = streamInfo % finalize()
!>
!>     deallocate(streamInfo)
!>
!
!-----------------------------------------------------------------------
module mpas_stream_inquiry

    public :: MPAS_stream_inquiry_new_streaminfo


contains


    !-----------------------------------------------------------------------
    !  routine MPAS_stream_inquiry_new_streaminfo
    !
    !> \brief Returns a pointer to a new MPAS_streamInfo_type instance
    !> \author Michael Duda
    !> \date   15 November 2023
    !> \details
    !>  This routine returns a pointer to a newly allocated instance of an
    !>  MPAS_streamInfo_type. The new instance has valid methods init(), query(),
    !>  and finalize() that may be called.
    !>
    !>  After all queries via the MPAS_streamInfo_type instance have been
    !>  completed, the instance finalize() method should be called before the
    !>  instance is deallocated.
    !
    !-----------------------------------------------------------------------
    function MPAS_stream_inquiry_new_streaminfo() result(new_streaminfo)

        use mpas_derived_types, only : MPAS_streamInfo_type

        implicit none

        ! Return value
        type (MPAS_streamInfo_type), pointer :: new_streaminfo

        allocate(new_streaminfo)
        new_streaminfo % init => streaminfo_init
        new_streaminfo % finalize => streaminfo_finalize
        new_streaminfo % query => streaminfo_query

    end function MPAS_stream_inquiry_new_streaminfo


    !-----------------------------------------------------------------------
    !  routine streaminfo_init
    !
    !> \brief Initializes an MPAS_streamInfo_type instance from a streams XML file
    !> \author Michael Duda
    !> \date   15 November 2023
    !> \details
    !>  This routine should be called as a method within an MPAS_streamInfo_type
    !>  instance, e.g., streaminfo % init(...). Given the name of an MPAS streams
    !>  XML file, this method initializes the instance so that later queries may
    !>  be made with the query() method.
    !
    !-----------------------------------------------------------------------
    function streaminfo_init(this, comm, stream_filename) result(ierr)

        use mpas_derived_types, only : MPAS_streamInfo_type
        use mpas_log, only : mpas_log_write
        use mpas_c_interfacing, only : mpas_f_to_c_string
        use iso_c_binding, only : c_char, c_associated
#ifdef MPAS_USE_MPI_F08
        use mpi_f08, only : MPI_Comm
#endif

        implicit none

        ! Arguments
        class (MPAS_streamInfo_type) :: this
#ifdef MPAS_USE_MPI_F08
        type (MPI_Comm), intent(in) :: comm
#else
        integer, intent(in) :: comm
#endif
        character(len=*), intent(in) :: stream_filename

        ! Return value
        integer :: ierr

        ! Local variables
        character(kind=c_char), dimension(len(stream_filename)+1) :: c_stream_filename

        interface
            function parse_streams_file(comm, filename) bind(C, name='parse_streams_file') result(xmltree)
                use iso_c_binding, only : c_char, c_ptr
                integer, intent(in), value :: comm
                character(kind=c_char), dimension(*), intent(in) :: filename
                type(c_ptr) :: xmltree
            end function parse_streams_file
        end interface


        ierr = 0

        call mpas_f_to_c_string(stream_filename, c_stream_filename)
        call mpas_log_write('Initializing MPAS_streamInfo from file '//trim(stream_filename))
#ifdef MPAS_USE_MPI_F08
        this % xmltree = parse_streams_file(comm % mpi_val, c_stream_filename)
#else
        this % xmltree = parse_streams_file(comm, c_stream_filename)
#endif

        if (.not. c_associated(this % xmltree)) then
            ierr = 1
        end if
    end function streaminfo_init


    !-----------------------------------------------------------------------
    !  routine streaminfo_finalize
    !
    !> \brief Finalizes an instance of the MPAS_streamInfo_type type
    !> \author Michael Duda
    !> \date   15 November 2023
    !> \details
    !>  This routine finalizes an instance of the MPAS_streamInfo_type type
    !>  after all queries about the contents of the streams XML file associated
    !>  with the instance have been completed. This routine should be called as
    !>  a method within an MPAS_streamInfo_type type, e.g.,
    !>  streaminfo % finalize().
    !
    !-----------------------------------------------------------------------
    function streaminfo_finalize(this) result(ierr)

        use mpas_derived_types, only : MPAS_streamInfo_type
        use iso_c_binding, only : c_null_ptr, c_associated

        implicit none

        ! Arguments
        class (MPAS_streamInfo_type) :: this

        ! Return value
        integer :: ierr

        interface
            subroutine free_streams_file(xmltree) bind(C, name='free_streams_file')
                use iso_c_binding, only : c_ptr
                type(c_ptr), value :: xmltree
            end subroutine free_streams_file
        end interface


        ierr = 0

        if (c_associated(this % xmltree)) then
            call free_streams_file(this % xmltree)
            this % xmltree = c_null_ptr
        end if

    end function streaminfo_finalize


    !-----------------------------------------------------------------------
    !  routine streaminfo_query
    !
    !> \brief Makes inquiries about the contents of a streams XML file
    !> \author Michael Duda
    !> \date   15 November 2023
    !> \details
    !>  For an instance of the MPAS_streamInfo_type type that has previously
    !>  been allocated and initialized from an MPAS streams XML file, this
    !>  routine allows for inquiries about the contents of the associated
    !>  streams file. This routine should be called as a method within an
    !>  instance of the MPAS_streamInfo_type type, e.g., as
    !>  streaminfo % query(...).
    !>
    !>  If only the required streamname attribute is given, this routine returns
    !>  .TRUE. if that stream exists, and .FALSE. otherwise. If the optional
    !>  attname attribute is given, and if that attribute exists for the
    !>  specified stream, .TRUE. is returned and .FALSE is returned otherwise;
    !>  further, if the optional attvalue argument is given, the value of the
    !>  attribute will assigned to the attvalue argument if the attribute
    !>  exists.
    !
    !-----------------------------------------------------------------------
    function streaminfo_query(this, streamname, attname, attvalue) result(success)

        use mpas_derived_types, only : MPAS_streamInfo_type
        use mpas_c_interfacing, only : mpas_f_to_c_string, mpas_c_to_f_string
        use iso_c_binding, only : c_char, c_ptr, c_null_ptr, c_loc, c_associated, c_f_pointer

        implicit none

        ! Arguments
        class (MPAS_streamInfo_type) :: this
        character(len=*), intent(in) :: streamname
        character(len=*), intent(in), optional :: attname
        character(len=*), intent(out), optional :: attvalue

        ! Return value
        logical :: success

        ! Local variables
        character(kind=c_char), dimension(len(streamname)+1) :: c_streamname
        character(kind=c_char), dimension(:), pointer :: c_attname, c_attvalue
        type (c_ptr) :: c_attname_ptr, c_attvalue_ptr

        interface
            function query_streams_file(xmltree, streamname, attname, attvalue) bind(C, name='query_streams_file') result(found)
                use iso_c_binding, only : c_ptr, c_int, c_char
                type (c_ptr), value :: xmltree
                character(kind=c_char), dimension(*), intent(in) :: streamname
                type (c_ptr), value :: attname
                type (c_ptr) :: attvalue
                integer(kind=c_int) :: found
            end function query_streams_file
        end interface


        success = .true.
        call mpas_f_to_c_string(streamname, c_streamname)

        if (present(attname)) then
            allocate(c_attname(len(attname)))
            call mpas_f_to_c_string(attname, c_attname)
            c_attname_ptr = c_loc(c_attname)
        else
            c_attname_ptr = c_null_ptr
        end if
        c_attvalue_ptr = c_null_ptr
        if (query_streams_file(this % xmltree, c_streamname, c_attname_ptr, c_attvalue_ptr) /= 1) then
            success = .false.
        end if
        if (present(attname)) then
            deallocate(c_attname)
        end if
        if (success .and. present(attname) .and. present(attvalue)) then
            if (c_associated(c_attvalue_ptr)) then
                call c_f_pointer(c_attvalue_ptr, c_attvalue, shape=[len(attvalue)])
                call mpas_c_to_f_string(c_attvalue, attvalue)
            else
            end if
        end if

    end function streaminfo_query

end module mpas_stream_inquiry
