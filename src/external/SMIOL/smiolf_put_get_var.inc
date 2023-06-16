    !-----------------------------------------------------------------------
    !  routine SMIOLf_put_var_d0_char
    !
    !> \brief Writes a 0-d char variable to a file.
    !> \details
    !>  Given a SMIOL file that was previously opened with write access and the name
    !>  of a variable previously defined in the file with a call to SMIOLf_define_var,
    !>  this routine will write the contents of buf to the variable according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks store identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument. As currently implemented, this routine will write
    !>  the buffer for MPI rank 0 to the variable; however, this behavior should not
    !>  be relied on.
    !>
    !>  If the variable has been successfully written to the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_put_var_0d_char(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_char, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        character(len=:), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf
        character(kind=c_char), dimension(:), allocatable, target :: char_buf

        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then
            allocate(char_buf(len(buf)))
            do i=1,len(buf)
                char_buf(i) = buf(i:i)
            end do
            c_buf = c_loc(char_buf)
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_put_var(c_file, c_varname, c_decomp, c_buf)

        if (associated(buf)) then
            deallocate(char_buf)
        end if
        deallocate(c_varname)

    end function SMIOLf_put_var_0d_char


    !-----------------------------------------------------------------------
    !  routine SMIOLf_get_var_d0_char
    !
    !> \brief Reads a 0-d char variable from a file.
    !> \details
    !>  Given a SMIOL file and the name of a variable previously defined in the file,
    !>  this routine will read the contents of the variable into buf according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks load identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument.
    !>
    !>  If the variable has been successfully read from the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_get_var_0d_char(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_char, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        character(len=:), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf
        character(kind=c_char), dimension(:), allocatable, target :: char_buf

        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then
            allocate(char_buf(len(buf)))

            ! In case buf contains more characters than will be read from the file,
            ! initialize char_buf with the contents of buf to preserve un-read
            ! characters during the copy of char_buf back into buf later on
            do i=1,len(buf)
                char_buf(i) = buf(i:i)
            end do
            c_buf = c_loc(char_buf)
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_get_var(c_file, c_varname, c_decomp, c_buf)

        if (associated(buf)) then
            do i=1,len(buf)
                buf(i:i) = char_buf(i)
            end do

            deallocate(char_buf)
        end if
        deallocate(c_varname)

    end function SMIOLf_get_var_0d_char


    !-----------------------------------------------------------------------
    !  routine SMIOLf_put_var_d0_real32
    !
    !> \brief Writes a 0-d real32 variable to a file.
    !> \details
    !>  Given a SMIOL file that was previously opened with write access and the name
    !>  of a variable previously defined in the file with a call to SMIOLf_define_var,
    !>  this routine will write the contents of buf to the variable according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks store identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument. As currently implemented, this routine will write
    !>  the buffer for MPI rank 0 to the variable; however, this behavior should not
    !>  be relied on.
    !>
    !>  If the variable has been successfully written to the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_put_var_0d_real32(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_float, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        real(kind=c_float), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            c_buf = c_loc(buf)
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_put_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_put_var_0d_real32


    !-----------------------------------------------------------------------
    !  routine SMIOLf_get_var_d0_real32
    !
    !> \brief Reads a 0-d real32 variable from a file.
    !> \details
    !>  Given a SMIOL file and the name of a variable previously defined in the file,
    !>  this routine will read the contents of the variable into buf according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks load identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument.
    !>
    !>  If the variable has been successfully read from the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_get_var_0d_real32(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_float, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        real(kind=c_float), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            c_buf = c_loc(buf)
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_get_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_get_var_0d_real32


    !-----------------------------------------------------------------------
    !  routine SMIOLf_put_var_d0_real64
    !
    !> \brief Writes a 0-d real64 variable to a file.
    !> \details
    !>  Given a SMIOL file that was previously opened with write access and the name
    !>  of a variable previously defined in the file with a call to SMIOLf_define_var,
    !>  this routine will write the contents of buf to the variable according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks store identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument. As currently implemented, this routine will write
    !>  the buffer for MPI rank 0 to the variable; however, this behavior should not
    !>  be relied on.
    !>
    !>  If the variable has been successfully written to the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_put_var_0d_real64(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_double, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        real(kind=c_double), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            c_buf = c_loc(buf)
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_put_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_put_var_0d_real64


    !-----------------------------------------------------------------------
    !  routine SMIOLf_get_var_d0_real64
    !
    !> \brief Reads a 0-d real64 variable from a file.
    !> \details
    !>  Given a SMIOL file and the name of a variable previously defined in the file,
    !>  this routine will read the contents of the variable into buf according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks load identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument.
    !>
    !>  If the variable has been successfully read from the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_get_var_0d_real64(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_double, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        real(kind=c_double), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            c_buf = c_loc(buf)
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_get_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_get_var_0d_real64


    !-----------------------------------------------------------------------
    !  routine SMIOLf_put_var_d0_int32
    !
    !> \brief Writes a 0-d int32 variable to a file.
    !> \details
    !>  Given a SMIOL file that was previously opened with write access and the name
    !>  of a variable previously defined in the file with a call to SMIOLf_define_var,
    !>  this routine will write the contents of buf to the variable according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks store identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument. As currently implemented, this routine will write
    !>  the buffer for MPI rank 0 to the variable; however, this behavior should not
    !>  be relied on.
    !>
    !>  If the variable has been successfully written to the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_put_var_0d_int32(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_int, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        integer(kind=c_int), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            c_buf = c_loc(buf)
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_put_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_put_var_0d_int32


    !-----------------------------------------------------------------------
    !  routine SMIOLf_get_var_d0_int32
    !
    !> \brief Reads a 0-d int32 variable from a file.
    !> \details
    !>  Given a SMIOL file and the name of a variable previously defined in the file,
    !>  this routine will read the contents of the variable into buf according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks load identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument.
    !>
    !>  If the variable has been successfully read from the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_get_var_0d_int32(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_int, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        integer(kind=c_int), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            c_buf = c_loc(buf)
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_get_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_get_var_0d_int32


    !-----------------------------------------------------------------------
    !  routine c_loc_assumed_shape_1d_real32
    !
    !> \brief Returns a C_PTR for an array with given dimensions
    !> \details
    !>  The Fortran 2003 standard does not permit the use of C_LOC with
    !>  assumed shape arrays. This routine may be used to obtain a C_PTR for
    !>  an assumed shape array by invoking the routine with the first actual
    !>  argument as the assumed-shape array, and subsequent actual arguments
    !>  as, e.g., SIZE(a,DIM=1).
    !>
    !>  Internally, the first dummy argument of this routine can be declared
    !>  as an explicit shape array, which can then be used as an argument to
    !>  C_LOC.
    !>
    !>  Upon success, a C_PTR for the array argument is returned.
    !>
    !>  Note: The actual array argument must not be a zero-sized array.
    !>        Section 15.1.2.5 of the Fortran 2003 standard specifies that
    !>        the argument to C_LOC '...is not an array of zero size...'.
    !
    !-----------------------------------------------------------------------
    function c_loc_assumed_shape_1d_real32(a, d1) result(a_ptr)

        use iso_c_binding, only : c_ptr, c_loc, c_float

        implicit none

        ! Arguments
        integer, intent(in) :: d1
        real(kind=c_float), dimension(d1), target, intent(in) :: a

        ! Return value
        type (c_ptr) :: a_ptr

        a_ptr = c_loc(a)

    end function c_loc_assumed_shape_1d_real32


    !-----------------------------------------------------------------------
    !  routine SMIOLf_put_var_d1_real32
    !
    !> \brief Writes a 1-d real32 variable to a file.
    !> \details
    !>  Given a SMIOL file that was previously opened with write access and the name
    !>  of a variable previously defined in the file with a call to SMIOLf_define_var,
    !>  this routine will write the contents of buf to the variable according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks store identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument. As currently implemented, this routine will write
    !>  the buffer for MPI rank 0 to the variable; however, this behavior should not
    !>  be relied on.
    !>
    !>  If the variable has been successfully written to the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_put_var_1d_real32(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_float, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        real(kind=c_float), dimension(:), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            !
            ! Invoke a Fortran 2003-compliant function to get the c_ptr
            ! of the assumed shape array buf
            !
            c_buf = c_loc_assumed_shape_1d_real32(buf, size(buf,dim=1))
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_put_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_put_var_1d_real32


    !-----------------------------------------------------------------------
    !  routine SMIOLf_get_var_d1_real32
    !
    !> \brief Reads a 1-d real32 variable from a file.
    !> \details
    !>  Given a SMIOL file and the name of a variable previously defined in the file,
    !>  this routine will read the contents of the variable into buf according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks load identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument.
    !>
    !>  If the variable has been successfully read from the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_get_var_1d_real32(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_float, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        real(kind=c_float), dimension(:), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            !
            ! Invoke a Fortran 2003-compliant function to get the c_ptr
            ! of the assumed shape array buf
            !
            c_buf = c_loc_assumed_shape_1d_real32(buf, size(buf,dim=1))
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_get_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_get_var_1d_real32


    !-----------------------------------------------------------------------
    !  routine c_loc_assumed_shape_1d_real64
    !
    !> \brief Returns a C_PTR for an array with given dimensions
    !> \details
    !>  The Fortran 2003 standard does not permit the use of C_LOC with
    !>  assumed shape arrays. This routine may be used to obtain a C_PTR for
    !>  an assumed shape array by invoking the routine with the first actual
    !>  argument as the assumed-shape array, and subsequent actual arguments
    !>  as, e.g., SIZE(a,DIM=1).
    !>
    !>  Internally, the first dummy argument of this routine can be declared
    !>  as an explicit shape array, which can then be used as an argument to
    !>  C_LOC.
    !>
    !>  Upon success, a C_PTR for the array argument is returned.
    !>
    !>  Note: The actual array argument must not be a zero-sized array.
    !>        Section 15.1.2.5 of the Fortran 2003 standard specifies that
    !>        the argument to C_LOC '...is not an array of zero size...'.
    !
    !-----------------------------------------------------------------------
    function c_loc_assumed_shape_1d_real64(a, d1) result(a_ptr)

        use iso_c_binding, only : c_ptr, c_loc, c_double

        implicit none

        ! Arguments
        integer, intent(in) :: d1
        real(kind=c_double), dimension(d1), target, intent(in) :: a

        ! Return value
        type (c_ptr) :: a_ptr

        a_ptr = c_loc(a)

    end function c_loc_assumed_shape_1d_real64


    !-----------------------------------------------------------------------
    !  routine SMIOLf_put_var_d1_real64
    !
    !> \brief Writes a 1-d real64 variable to a file.
    !> \details
    !>  Given a SMIOL file that was previously opened with write access and the name
    !>  of a variable previously defined in the file with a call to SMIOLf_define_var,
    !>  this routine will write the contents of buf to the variable according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks store identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument. As currently implemented, this routine will write
    !>  the buffer for MPI rank 0 to the variable; however, this behavior should not
    !>  be relied on.
    !>
    !>  If the variable has been successfully written to the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_put_var_1d_real64(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_double, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        real(kind=c_double), dimension(:), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            !
            ! Invoke a Fortran 2003-compliant function to get the c_ptr
            ! of the assumed shape array buf
            !
            c_buf = c_loc_assumed_shape_1d_real64(buf, size(buf,dim=1))
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_put_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_put_var_1d_real64


    !-----------------------------------------------------------------------
    !  routine SMIOLf_get_var_d1_real64
    !
    !> \brief Reads a 1-d real64 variable from a file.
    !> \details
    !>  Given a SMIOL file and the name of a variable previously defined in the file,
    !>  this routine will read the contents of the variable into buf according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks load identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument.
    !>
    !>  If the variable has been successfully read from the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_get_var_1d_real64(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_double, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        real(kind=c_double), dimension(:), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            !
            ! Invoke a Fortran 2003-compliant function to get the c_ptr
            ! of the assumed shape array buf
            !
            c_buf = c_loc_assumed_shape_1d_real64(buf, size(buf,dim=1))
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_get_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_get_var_1d_real64


    !-----------------------------------------------------------------------
    !  routine c_loc_assumed_shape_1d_int32
    !
    !> \brief Returns a C_PTR for an array with given dimensions
    !> \details
    !>  The Fortran 2003 standard does not permit the use of C_LOC with
    !>  assumed shape arrays. This routine may be used to obtain a C_PTR for
    !>  an assumed shape array by invoking the routine with the first actual
    !>  argument as the assumed-shape array, and subsequent actual arguments
    !>  as, e.g., SIZE(a,DIM=1).
    !>
    !>  Internally, the first dummy argument of this routine can be declared
    !>  as an explicit shape array, which can then be used as an argument to
    !>  C_LOC.
    !>
    !>  Upon success, a C_PTR for the array argument is returned.
    !>
    !>  Note: The actual array argument must not be a zero-sized array.
    !>        Section 15.1.2.5 of the Fortran 2003 standard specifies that
    !>        the argument to C_LOC '...is not an array of zero size...'.
    !
    !-----------------------------------------------------------------------
    function c_loc_assumed_shape_1d_int32(a, d1) result(a_ptr)

        use iso_c_binding, only : c_ptr, c_loc, c_int

        implicit none

        ! Arguments
        integer, intent(in) :: d1
        integer(kind=c_int), dimension(d1), target, intent(in) :: a

        ! Return value
        type (c_ptr) :: a_ptr

        a_ptr = c_loc(a)

    end function c_loc_assumed_shape_1d_int32


    !-----------------------------------------------------------------------
    !  routine SMIOLf_put_var_d1_int32
    !
    !> \brief Writes a 1-d int32 variable to a file.
    !> \details
    !>  Given a SMIOL file that was previously opened with write access and the name
    !>  of a variable previously defined in the file with a call to SMIOLf_define_var,
    !>  this routine will write the contents of buf to the variable according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks store identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument. As currently implemented, this routine will write
    !>  the buffer for MPI rank 0 to the variable; however, this behavior should not
    !>  be relied on.
    !>
    !>  If the variable has been successfully written to the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_put_var_1d_int32(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_int, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        integer(kind=c_int), dimension(:), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            !
            ! Invoke a Fortran 2003-compliant function to get the c_ptr
            ! of the assumed shape array buf
            !
            c_buf = c_loc_assumed_shape_1d_int32(buf, size(buf,dim=1))
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_put_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_put_var_1d_int32


    !-----------------------------------------------------------------------
    !  routine SMIOLf_get_var_d1_int32
    !
    !> \brief Reads a 1-d int32 variable from a file.
    !> \details
    !>  Given a SMIOL file and the name of a variable previously defined in the file,
    !>  this routine will read the contents of the variable into buf according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks load identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument.
    !>
    !>  If the variable has been successfully read from the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_get_var_1d_int32(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_int, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        integer(kind=c_int), dimension(:), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            !
            ! Invoke a Fortran 2003-compliant function to get the c_ptr
            ! of the assumed shape array buf
            !
            c_buf = c_loc_assumed_shape_1d_int32(buf, size(buf,dim=1))
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_get_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_get_var_1d_int32


    !-----------------------------------------------------------------------
    !  routine c_loc_assumed_shape_2d_real32
    !
    !> \brief Returns a C_PTR for an array with given dimensions
    !> \details
    !>  The Fortran 2003 standard does not permit the use of C_LOC with
    !>  assumed shape arrays. This routine may be used to obtain a C_PTR for
    !>  an assumed shape array by invoking the routine with the first actual
    !>  argument as the assumed-shape array, and subsequent actual arguments
    !>  as, e.g., SIZE(a,DIM=1).
    !>
    !>  Internally, the first dummy argument of this routine can be declared
    !>  as an explicit shape array, which can then be used as an argument to
    !>  C_LOC.
    !>
    !>  Upon success, a C_PTR for the array argument is returned.
    !>
    !>  Note: The actual array argument must not be a zero-sized array.
    !>        Section 15.1.2.5 of the Fortran 2003 standard specifies that
    !>        the argument to C_LOC '...is not an array of zero size...'.
    !
    !-----------------------------------------------------------------------
    function c_loc_assumed_shape_2d_real32(a, d1, d2) result(a_ptr)

        use iso_c_binding, only : c_ptr, c_loc, c_float

        implicit none

        ! Arguments
        integer, intent(in) :: d1, d2
        real(kind=c_float), dimension(d1,d2), target, intent(in) :: a

        ! Return value
        type (c_ptr) :: a_ptr

        a_ptr = c_loc(a)

    end function c_loc_assumed_shape_2d_real32


    !-----------------------------------------------------------------------
    !  routine SMIOLf_put_var_d2_real32
    !
    !> \brief Writes a 2-d real32 variable to a file.
    !> \details
    !>  Given a SMIOL file that was previously opened with write access and the name
    !>  of a variable previously defined in the file with a call to SMIOLf_define_var,
    !>  this routine will write the contents of buf to the variable according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks store identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument. As currently implemented, this routine will write
    !>  the buffer for MPI rank 0 to the variable; however, this behavior should not
    !>  be relied on.
    !>
    !>  If the variable has been successfully written to the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_put_var_2d_real32(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_float, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        real(kind=c_float), dimension(:,:), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            !
            ! Invoke a Fortran 2003-compliant function to get the c_ptr
            ! of the assumed shape array buf
            !
            c_buf = c_loc_assumed_shape_2d_real32(buf, size(buf,dim=1), size(buf,dim=2))
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_put_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_put_var_2d_real32


    !-----------------------------------------------------------------------
    !  routine SMIOLf_get_var_d2_real32
    !
    !> \brief Reads a 2-d real32 variable from a file.
    !> \details
    !>  Given a SMIOL file and the name of a variable previously defined in the file,
    !>  this routine will read the contents of the variable into buf according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks load identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument.
    !>
    !>  If the variable has been successfully read from the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_get_var_2d_real32(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_float, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        real(kind=c_float), dimension(:,:), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            !
            ! Invoke a Fortran 2003-compliant function to get the c_ptr
            ! of the assumed shape array buf
            !
            c_buf = c_loc_assumed_shape_2d_real32(buf, size(buf,dim=1), size(buf,dim=2))
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_get_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_get_var_2d_real32


    !-----------------------------------------------------------------------
    !  routine c_loc_assumed_shape_2d_real64
    !
    !> \brief Returns a C_PTR for an array with given dimensions
    !> \details
    !>  The Fortran 2003 standard does not permit the use of C_LOC with
    !>  assumed shape arrays. This routine may be used to obtain a C_PTR for
    !>  an assumed shape array by invoking the routine with the first actual
    !>  argument as the assumed-shape array, and subsequent actual arguments
    !>  as, e.g., SIZE(a,DIM=1).
    !>
    !>  Internally, the first dummy argument of this routine can be declared
    !>  as an explicit shape array, which can then be used as an argument to
    !>  C_LOC.
    !>
    !>  Upon success, a C_PTR for the array argument is returned.
    !>
    !>  Note: The actual array argument must not be a zero-sized array.
    !>        Section 15.1.2.5 of the Fortran 2003 standard specifies that
    !>        the argument to C_LOC '...is not an array of zero size...'.
    !
    !-----------------------------------------------------------------------
    function c_loc_assumed_shape_2d_real64(a, d1, d2) result(a_ptr)

        use iso_c_binding, only : c_ptr, c_loc, c_double

        implicit none

        ! Arguments
        integer, intent(in) :: d1, d2
        real(kind=c_double), dimension(d1,d2), target, intent(in) :: a

        ! Return value
        type (c_ptr) :: a_ptr

        a_ptr = c_loc(a)

    end function c_loc_assumed_shape_2d_real64


    !-----------------------------------------------------------------------
    !  routine SMIOLf_put_var_d2_real64
    !
    !> \brief Writes a 2-d real64 variable to a file.
    !> \details
    !>  Given a SMIOL file that was previously opened with write access and the name
    !>  of a variable previously defined in the file with a call to SMIOLf_define_var,
    !>  this routine will write the contents of buf to the variable according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks store identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument. As currently implemented, this routine will write
    !>  the buffer for MPI rank 0 to the variable; however, this behavior should not
    !>  be relied on.
    !>
    !>  If the variable has been successfully written to the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_put_var_2d_real64(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_double, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        real(kind=c_double), dimension(:,:), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            !
            ! Invoke a Fortran 2003-compliant function to get the c_ptr
            ! of the assumed shape array buf
            !
            c_buf = c_loc_assumed_shape_2d_real64(buf, size(buf,dim=1), size(buf,dim=2))
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_put_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_put_var_2d_real64


    !-----------------------------------------------------------------------
    !  routine SMIOLf_get_var_d2_real64
    !
    !> \brief Reads a 2-d real64 variable from a file.
    !> \details
    !>  Given a SMIOL file and the name of a variable previously defined in the file,
    !>  this routine will read the contents of the variable into buf according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks load identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument.
    !>
    !>  If the variable has been successfully read from the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_get_var_2d_real64(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_double, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        real(kind=c_double), dimension(:,:), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            !
            ! Invoke a Fortran 2003-compliant function to get the c_ptr
            ! of the assumed shape array buf
            !
            c_buf = c_loc_assumed_shape_2d_real64(buf, size(buf,dim=1), size(buf,dim=2))
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_get_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_get_var_2d_real64


    !-----------------------------------------------------------------------
    !  routine c_loc_assumed_shape_2d_int32
    !
    !> \brief Returns a C_PTR for an array with given dimensions
    !> \details
    !>  The Fortran 2003 standard does not permit the use of C_LOC with
    !>  assumed shape arrays. This routine may be used to obtain a C_PTR for
    !>  an assumed shape array by invoking the routine with the first actual
    !>  argument as the assumed-shape array, and subsequent actual arguments
    !>  as, e.g., SIZE(a,DIM=1).
    !>
    !>  Internally, the first dummy argument of this routine can be declared
    !>  as an explicit shape array, which can then be used as an argument to
    !>  C_LOC.
    !>
    !>  Upon success, a C_PTR for the array argument is returned.
    !>
    !>  Note: The actual array argument must not be a zero-sized array.
    !>        Section 15.1.2.5 of the Fortran 2003 standard specifies that
    !>        the argument to C_LOC '...is not an array of zero size...'.
    !
    !-----------------------------------------------------------------------
    function c_loc_assumed_shape_2d_int32(a, d1, d2) result(a_ptr)

        use iso_c_binding, only : c_ptr, c_loc, c_int

        implicit none

        ! Arguments
        integer, intent(in) :: d1, d2
        integer(kind=c_int), dimension(d1,d2), target, intent(in) :: a

        ! Return value
        type (c_ptr) :: a_ptr

        a_ptr = c_loc(a)

    end function c_loc_assumed_shape_2d_int32


    !-----------------------------------------------------------------------
    !  routine SMIOLf_put_var_d2_int32
    !
    !> \brief Writes a 2-d int32 variable to a file.
    !> \details
    !>  Given a SMIOL file that was previously opened with write access and the name
    !>  of a variable previously defined in the file with a call to SMIOLf_define_var,
    !>  this routine will write the contents of buf to the variable according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks store identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument. As currently implemented, this routine will write
    !>  the buffer for MPI rank 0 to the variable; however, this behavior should not
    !>  be relied on.
    !>
    !>  If the variable has been successfully written to the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_put_var_2d_int32(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_int, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        integer(kind=c_int), dimension(:,:), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            !
            ! Invoke a Fortran 2003-compliant function to get the c_ptr
            ! of the assumed shape array buf
            !
            c_buf = c_loc_assumed_shape_2d_int32(buf, size(buf,dim=1), size(buf,dim=2))
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_put_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_put_var_2d_int32


    !-----------------------------------------------------------------------
    !  routine SMIOLf_get_var_d2_int32
    !
    !> \brief Reads a 2-d int32 variable from a file.
    !> \details
    !>  Given a SMIOL file and the name of a variable previously defined in the file,
    !>  this routine will read the contents of the variable into buf according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks load identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument.
    !>
    !>  If the variable has been successfully read from the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_get_var_2d_int32(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_int, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        integer(kind=c_int), dimension(:,:), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            !
            ! Invoke a Fortran 2003-compliant function to get the c_ptr
            ! of the assumed shape array buf
            !
            c_buf = c_loc_assumed_shape_2d_int32(buf, size(buf,dim=1), size(buf,dim=2))
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_get_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_get_var_2d_int32


    !-----------------------------------------------------------------------
    !  routine c_loc_assumed_shape_3d_real32
    !
    !> \brief Returns a C_PTR for an array with given dimensions
    !> \details
    !>  The Fortran 2003 standard does not permit the use of C_LOC with
    !>  assumed shape arrays. This routine may be used to obtain a C_PTR for
    !>  an assumed shape array by invoking the routine with the first actual
    !>  argument as the assumed-shape array, and subsequent actual arguments
    !>  as, e.g., SIZE(a,DIM=1).
    !>
    !>  Internally, the first dummy argument of this routine can be declared
    !>  as an explicit shape array, which can then be used as an argument to
    !>  C_LOC.
    !>
    !>  Upon success, a C_PTR for the array argument is returned.
    !>
    !>  Note: The actual array argument must not be a zero-sized array.
    !>        Section 15.1.2.5 of the Fortran 2003 standard specifies that
    !>        the argument to C_LOC '...is not an array of zero size...'.
    !
    !-----------------------------------------------------------------------
    function c_loc_assumed_shape_3d_real32(a, d1, d2, d3) result(a_ptr)

        use iso_c_binding, only : c_ptr, c_loc, c_float

        implicit none

        ! Arguments
        integer, intent(in) :: d1, d2, d3
        real(kind=c_float), dimension(d1,d2,d3), target, intent(in) :: a

        ! Return value
        type (c_ptr) :: a_ptr

        a_ptr = c_loc(a)

    end function c_loc_assumed_shape_3d_real32


    !-----------------------------------------------------------------------
    !  routine SMIOLf_put_var_d3_real32
    !
    !> \brief Writes a 3-d real32 variable to a file.
    !> \details
    !>  Given a SMIOL file that was previously opened with write access and the name
    !>  of a variable previously defined in the file with a call to SMIOLf_define_var,
    !>  this routine will write the contents of buf to the variable according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks store identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument. As currently implemented, this routine will write
    !>  the buffer for MPI rank 0 to the variable; however, this behavior should not
    !>  be relied on.
    !>
    !>  If the variable has been successfully written to the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_put_var_3d_real32(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_float, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        real(kind=c_float), dimension(:,:,:), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            !
            ! Invoke a Fortran 2003-compliant function to get the c_ptr
            ! of the assumed shape array buf
            !
            c_buf = c_loc_assumed_shape_3d_real32(buf, size(buf,dim=1), size(buf,dim=2), size(buf,dim=3))
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_put_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_put_var_3d_real32


    !-----------------------------------------------------------------------
    !  routine SMIOLf_get_var_d3_real32
    !
    !> \brief Reads a 3-d real32 variable from a file.
    !> \details
    !>  Given a SMIOL file and the name of a variable previously defined in the file,
    !>  this routine will read the contents of the variable into buf according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks load identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument.
    !>
    !>  If the variable has been successfully read from the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_get_var_3d_real32(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_float, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        real(kind=c_float), dimension(:,:,:), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            !
            ! Invoke a Fortran 2003-compliant function to get the c_ptr
            ! of the assumed shape array buf
            !
            c_buf = c_loc_assumed_shape_3d_real32(buf, size(buf,dim=1), size(buf,dim=2), size(buf,dim=3))
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_get_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_get_var_3d_real32


    !-----------------------------------------------------------------------
    !  routine c_loc_assumed_shape_3d_real64
    !
    !> \brief Returns a C_PTR for an array with given dimensions
    !> \details
    !>  The Fortran 2003 standard does not permit the use of C_LOC with
    !>  assumed shape arrays. This routine may be used to obtain a C_PTR for
    !>  an assumed shape array by invoking the routine with the first actual
    !>  argument as the assumed-shape array, and subsequent actual arguments
    !>  as, e.g., SIZE(a,DIM=1).
    !>
    !>  Internally, the first dummy argument of this routine can be declared
    !>  as an explicit shape array, which can then be used as an argument to
    !>  C_LOC.
    !>
    !>  Upon success, a C_PTR for the array argument is returned.
    !>
    !>  Note: The actual array argument must not be a zero-sized array.
    !>        Section 15.1.2.5 of the Fortran 2003 standard specifies that
    !>        the argument to C_LOC '...is not an array of zero size...'.
    !
    !-----------------------------------------------------------------------
    function c_loc_assumed_shape_3d_real64(a, d1, d2, d3) result(a_ptr)

        use iso_c_binding, only : c_ptr, c_loc, c_double

        implicit none

        ! Arguments
        integer, intent(in) :: d1, d2, d3
        real(kind=c_double), dimension(d1,d2,d3), target, intent(in) :: a

        ! Return value
        type (c_ptr) :: a_ptr

        a_ptr = c_loc(a)

    end function c_loc_assumed_shape_3d_real64


    !-----------------------------------------------------------------------
    !  routine SMIOLf_put_var_d3_real64
    !
    !> \brief Writes a 3-d real64 variable to a file.
    !> \details
    !>  Given a SMIOL file that was previously opened with write access and the name
    !>  of a variable previously defined in the file with a call to SMIOLf_define_var,
    !>  this routine will write the contents of buf to the variable according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks store identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument. As currently implemented, this routine will write
    !>  the buffer for MPI rank 0 to the variable; however, this behavior should not
    !>  be relied on.
    !>
    !>  If the variable has been successfully written to the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_put_var_3d_real64(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_double, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        real(kind=c_double), dimension(:,:,:), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            !
            ! Invoke a Fortran 2003-compliant function to get the c_ptr
            ! of the assumed shape array buf
            !
            c_buf = c_loc_assumed_shape_3d_real64(buf, size(buf,dim=1), size(buf,dim=2), size(buf,dim=3))
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_put_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_put_var_3d_real64


    !-----------------------------------------------------------------------
    !  routine SMIOLf_get_var_d3_real64
    !
    !> \brief Reads a 3-d real64 variable from a file.
    !> \details
    !>  Given a SMIOL file and the name of a variable previously defined in the file,
    !>  this routine will read the contents of the variable into buf according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks load identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument.
    !>
    !>  If the variable has been successfully read from the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_get_var_3d_real64(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_double, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        real(kind=c_double), dimension(:,:,:), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            !
            ! Invoke a Fortran 2003-compliant function to get the c_ptr
            ! of the assumed shape array buf
            !
            c_buf = c_loc_assumed_shape_3d_real64(buf, size(buf,dim=1), size(buf,dim=2), size(buf,dim=3))
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_get_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_get_var_3d_real64


    !-----------------------------------------------------------------------
    !  routine c_loc_assumed_shape_3d_int32
    !
    !> \brief Returns a C_PTR for an array with given dimensions
    !> \details
    !>  The Fortran 2003 standard does not permit the use of C_LOC with
    !>  assumed shape arrays. This routine may be used to obtain a C_PTR for
    !>  an assumed shape array by invoking the routine with the first actual
    !>  argument as the assumed-shape array, and subsequent actual arguments
    !>  as, e.g., SIZE(a,DIM=1).
    !>
    !>  Internally, the first dummy argument of this routine can be declared
    !>  as an explicit shape array, which can then be used as an argument to
    !>  C_LOC.
    !>
    !>  Upon success, a C_PTR for the array argument is returned.
    !>
    !>  Note: The actual array argument must not be a zero-sized array.
    !>        Section 15.1.2.5 of the Fortran 2003 standard specifies that
    !>        the argument to C_LOC '...is not an array of zero size...'.
    !
    !-----------------------------------------------------------------------
    function c_loc_assumed_shape_3d_int32(a, d1, d2, d3) result(a_ptr)

        use iso_c_binding, only : c_ptr, c_loc, c_int

        implicit none

        ! Arguments
        integer, intent(in) :: d1, d2, d3
        integer(kind=c_int), dimension(d1,d2,d3), target, intent(in) :: a

        ! Return value
        type (c_ptr) :: a_ptr

        a_ptr = c_loc(a)

    end function c_loc_assumed_shape_3d_int32


    !-----------------------------------------------------------------------
    !  routine SMIOLf_put_var_d3_int32
    !
    !> \brief Writes a 3-d int32 variable to a file.
    !> \details
    !>  Given a SMIOL file that was previously opened with write access and the name
    !>  of a variable previously defined in the file with a call to SMIOLf_define_var,
    !>  this routine will write the contents of buf to the variable according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks store identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument. As currently implemented, this routine will write
    !>  the buffer for MPI rank 0 to the variable; however, this behavior should not
    !>  be relied on.
    !>
    !>  If the variable has been successfully written to the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_put_var_3d_int32(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_int, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        integer(kind=c_int), dimension(:,:,:), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            !
            ! Invoke a Fortran 2003-compliant function to get the c_ptr
            ! of the assumed shape array buf
            !
            c_buf = c_loc_assumed_shape_3d_int32(buf, size(buf,dim=1), size(buf,dim=2), size(buf,dim=3))
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_put_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_put_var_3d_int32


    !-----------------------------------------------------------------------
    !  routine SMIOLf_get_var_d3_int32
    !
    !> \brief Reads a 3-d int32 variable from a file.
    !> \details
    !>  Given a SMIOL file and the name of a variable previously defined in the file,
    !>  this routine will read the contents of the variable into buf according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks load identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument.
    !>
    !>  If the variable has been successfully read from the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_get_var_3d_int32(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_int, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        integer(kind=c_int), dimension(:,:,:), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            !
            ! Invoke a Fortran 2003-compliant function to get the c_ptr
            ! of the assumed shape array buf
            !
            c_buf = c_loc_assumed_shape_3d_int32(buf, size(buf,dim=1), size(buf,dim=2), size(buf,dim=3))
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_get_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_get_var_3d_int32


    !-----------------------------------------------------------------------
    !  routine c_loc_assumed_shape_4d_real32
    !
    !> \brief Returns a C_PTR for an array with given dimensions
    !> \details
    !>  The Fortran 2003 standard does not permit the use of C_LOC with
    !>  assumed shape arrays. This routine may be used to obtain a C_PTR for
    !>  an assumed shape array by invoking the routine with the first actual
    !>  argument as the assumed-shape array, and subsequent actual arguments
    !>  as, e.g., SIZE(a,DIM=1).
    !>
    !>  Internally, the first dummy argument of this routine can be declared
    !>  as an explicit shape array, which can then be used as an argument to
    !>  C_LOC.
    !>
    !>  Upon success, a C_PTR for the array argument is returned.
    !>
    !>  Note: The actual array argument must not be a zero-sized array.
    !>        Section 15.1.2.5 of the Fortran 2003 standard specifies that
    !>        the argument to C_LOC '...is not an array of zero size...'.
    !
    !-----------------------------------------------------------------------
    function c_loc_assumed_shape_4d_real32(a, d1, d2, d3, d4) result(a_ptr)

        use iso_c_binding, only : c_ptr, c_loc, c_float

        implicit none

        ! Arguments
        integer, intent(in) :: d1, d2, d3, d4
        real(kind=c_float), dimension(d1,d2,d3,d4), target, intent(in) :: a

        ! Return value
        type (c_ptr) :: a_ptr

        a_ptr = c_loc(a)

    end function c_loc_assumed_shape_4d_real32


    !-----------------------------------------------------------------------
    !  routine SMIOLf_put_var_d4_real32
    !
    !> \brief Writes a 4-d real32 variable to a file.
    !> \details
    !>  Given a SMIOL file that was previously opened with write access and the name
    !>  of a variable previously defined in the file with a call to SMIOLf_define_var,
    !>  this routine will write the contents of buf to the variable according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks store identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument. As currently implemented, this routine will write
    !>  the buffer for MPI rank 0 to the variable; however, this behavior should not
    !>  be relied on.
    !>
    !>  If the variable has been successfully written to the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_put_var_4d_real32(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_float, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        real(kind=c_float), dimension(:,:,:,:), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            !
            ! Invoke a Fortran 2003-compliant function to get the c_ptr
            ! of the assumed shape array buf
            !
            c_buf = c_loc_assumed_shape_4d_real32(buf, size(buf,dim=1), size(buf,dim=2), size(buf,dim=3), &
                                           size(buf,dim=4))
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_put_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_put_var_4d_real32


    !-----------------------------------------------------------------------
    !  routine SMIOLf_get_var_d4_real32
    !
    !> \brief Reads a 4-d real32 variable from a file.
    !> \details
    !>  Given a SMIOL file and the name of a variable previously defined in the file,
    !>  this routine will read the contents of the variable into buf according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks load identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument.
    !>
    !>  If the variable has been successfully read from the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_get_var_4d_real32(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_float, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        real(kind=c_float), dimension(:,:,:,:), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            !
            ! Invoke a Fortran 2003-compliant function to get the c_ptr
            ! of the assumed shape array buf
            !
            c_buf = c_loc_assumed_shape_4d_real32(buf, size(buf,dim=1), size(buf,dim=2), size(buf,dim=3), &
                                           size(buf,dim=4))
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_get_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_get_var_4d_real32


    !-----------------------------------------------------------------------
    !  routine c_loc_assumed_shape_4d_real64
    !
    !> \brief Returns a C_PTR for an array with given dimensions
    !> \details
    !>  The Fortran 2003 standard does not permit the use of C_LOC with
    !>  assumed shape arrays. This routine may be used to obtain a C_PTR for
    !>  an assumed shape array by invoking the routine with the first actual
    !>  argument as the assumed-shape array, and subsequent actual arguments
    !>  as, e.g., SIZE(a,DIM=1).
    !>
    !>  Internally, the first dummy argument of this routine can be declared
    !>  as an explicit shape array, which can then be used as an argument to
    !>  C_LOC.
    !>
    !>  Upon success, a C_PTR for the array argument is returned.
    !>
    !>  Note: The actual array argument must not be a zero-sized array.
    !>        Section 15.1.2.5 of the Fortran 2003 standard specifies that
    !>        the argument to C_LOC '...is not an array of zero size...'.
    !
    !-----------------------------------------------------------------------
    function c_loc_assumed_shape_4d_real64(a, d1, d2, d3, d4) result(a_ptr)

        use iso_c_binding, only : c_ptr, c_loc, c_double

        implicit none

        ! Arguments
        integer, intent(in) :: d1, d2, d3, d4
        real(kind=c_double), dimension(d1,d2,d3,d4), target, intent(in) :: a

        ! Return value
        type (c_ptr) :: a_ptr

        a_ptr = c_loc(a)

    end function c_loc_assumed_shape_4d_real64


    !-----------------------------------------------------------------------
    !  routine SMIOLf_put_var_d4_real64
    !
    !> \brief Writes a 4-d real64 variable to a file.
    !> \details
    !>  Given a SMIOL file that was previously opened with write access and the name
    !>  of a variable previously defined in the file with a call to SMIOLf_define_var,
    !>  this routine will write the contents of buf to the variable according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks store identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument. As currently implemented, this routine will write
    !>  the buffer for MPI rank 0 to the variable; however, this behavior should not
    !>  be relied on.
    !>
    !>  If the variable has been successfully written to the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_put_var_4d_real64(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_double, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        real(kind=c_double), dimension(:,:,:,:), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            !
            ! Invoke a Fortran 2003-compliant function to get the c_ptr
            ! of the assumed shape array buf
            !
            c_buf = c_loc_assumed_shape_4d_real64(buf, size(buf,dim=1), size(buf,dim=2), size(buf,dim=3), &
                                           size(buf,dim=4))
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_put_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_put_var_4d_real64


    !-----------------------------------------------------------------------
    !  routine SMIOLf_get_var_d4_real64
    !
    !> \brief Reads a 4-d real64 variable from a file.
    !> \details
    !>  Given a SMIOL file and the name of a variable previously defined in the file,
    !>  this routine will read the contents of the variable into buf according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks load identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument.
    !>
    !>  If the variable has been successfully read from the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_get_var_4d_real64(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_double, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        real(kind=c_double), dimension(:,:,:,:), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            !
            ! Invoke a Fortran 2003-compliant function to get the c_ptr
            ! of the assumed shape array buf
            !
            c_buf = c_loc_assumed_shape_4d_real64(buf, size(buf,dim=1), size(buf,dim=2), size(buf,dim=3), &
                                           size(buf,dim=4))
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_get_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_get_var_4d_real64


    !-----------------------------------------------------------------------
    !  routine c_loc_assumed_shape_4d_int32
    !
    !> \brief Returns a C_PTR for an array with given dimensions
    !> \details
    !>  The Fortran 2003 standard does not permit the use of C_LOC with
    !>  assumed shape arrays. This routine may be used to obtain a C_PTR for
    !>  an assumed shape array by invoking the routine with the first actual
    !>  argument as the assumed-shape array, and subsequent actual arguments
    !>  as, e.g., SIZE(a,DIM=1).
    !>
    !>  Internally, the first dummy argument of this routine can be declared
    !>  as an explicit shape array, which can then be used as an argument to
    !>  C_LOC.
    !>
    !>  Upon success, a C_PTR for the array argument is returned.
    !>
    !>  Note: The actual array argument must not be a zero-sized array.
    !>        Section 15.1.2.5 of the Fortran 2003 standard specifies that
    !>        the argument to C_LOC '...is not an array of zero size...'.
    !
    !-----------------------------------------------------------------------
    function c_loc_assumed_shape_4d_int32(a, d1, d2, d3, d4) result(a_ptr)

        use iso_c_binding, only : c_ptr, c_loc, c_int

        implicit none

        ! Arguments
        integer, intent(in) :: d1, d2, d3, d4
        integer(kind=c_int), dimension(d1,d2,d3,d4), target, intent(in) :: a

        ! Return value
        type (c_ptr) :: a_ptr

        a_ptr = c_loc(a)

    end function c_loc_assumed_shape_4d_int32


    !-----------------------------------------------------------------------
    !  routine SMIOLf_put_var_d4_int32
    !
    !> \brief Writes a 4-d int32 variable to a file.
    !> \details
    !>  Given a SMIOL file that was previously opened with write access and the name
    !>  of a variable previously defined in the file with a call to SMIOLf_define_var,
    !>  this routine will write the contents of buf to the variable according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks store identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument. As currently implemented, this routine will write
    !>  the buffer for MPI rank 0 to the variable; however, this behavior should not
    !>  be relied on.
    !>
    !>  If the variable has been successfully written to the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_put_var_4d_int32(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_int, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        integer(kind=c_int), dimension(:,:,:,:), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            !
            ! Invoke a Fortran 2003-compliant function to get the c_ptr
            ! of the assumed shape array buf
            !
            c_buf = c_loc_assumed_shape_4d_int32(buf, size(buf,dim=1), size(buf,dim=2), size(buf,dim=3), &
                                           size(buf,dim=4))
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_put_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_put_var_4d_int32


    !-----------------------------------------------------------------------
    !  routine SMIOLf_get_var_d4_int32
    !
    !> \brief Reads a 4-d int32 variable from a file.
    !> \details
    !>  Given a SMIOL file and the name of a variable previously defined in the file,
    !>  this routine will read the contents of the variable into buf according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks load identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument.
    !>
    !>  If the variable has been successfully read from the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_get_var_4d_int32(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_int, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        integer(kind=c_int), dimension(:,:,:,:), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            !
            ! Invoke a Fortran 2003-compliant function to get the c_ptr
            ! of the assumed shape array buf
            !
            c_buf = c_loc_assumed_shape_4d_int32(buf, size(buf,dim=1), size(buf,dim=2), size(buf,dim=3), &
                                           size(buf,dim=4))
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_get_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_get_var_4d_int32


    !-----------------------------------------------------------------------
    !  routine c_loc_assumed_shape_5d_real32
    !
    !> \brief Returns a C_PTR for an array with given dimensions
    !> \details
    !>  The Fortran 2003 standard does not permit the use of C_LOC with
    !>  assumed shape arrays. This routine may be used to obtain a C_PTR for
    !>  an assumed shape array by invoking the routine with the first actual
    !>  argument as the assumed-shape array, and subsequent actual arguments
    !>  as, e.g., SIZE(a,DIM=1).
    !>
    !>  Internally, the first dummy argument of this routine can be declared
    !>  as an explicit shape array, which can then be used as an argument to
    !>  C_LOC.
    !>
    !>  Upon success, a C_PTR for the array argument is returned.
    !>
    !>  Note: The actual array argument must not be a zero-sized array.
    !>        Section 15.1.2.5 of the Fortran 2003 standard specifies that
    !>        the argument to C_LOC '...is not an array of zero size...'.
    !
    !-----------------------------------------------------------------------
    function c_loc_assumed_shape_5d_real32(a, d1, d2, d3, d4, d5) result(a_ptr)

        use iso_c_binding, only : c_ptr, c_loc, c_float

        implicit none

        ! Arguments
        integer, intent(in) :: d1, d2, d3, d4, d5
        real(kind=c_float), dimension(d1,d2,d3,d4,d5), target, intent(in) :: a

        ! Return value
        type (c_ptr) :: a_ptr

        a_ptr = c_loc(a)

    end function c_loc_assumed_shape_5d_real32


    !-----------------------------------------------------------------------
    !  routine SMIOLf_put_var_d5_real32
    !
    !> \brief Writes a 5-d real32 variable to a file.
    !> \details
    !>  Given a SMIOL file that was previously opened with write access and the name
    !>  of a variable previously defined in the file with a call to SMIOLf_define_var,
    !>  this routine will write the contents of buf to the variable according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks store identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument. As currently implemented, this routine will write
    !>  the buffer for MPI rank 0 to the variable; however, this behavior should not
    !>  be relied on.
    !>
    !>  If the variable has been successfully written to the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_put_var_5d_real32(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_float, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        real(kind=c_float), dimension(:,:,:,:,:), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            !
            ! Invoke a Fortran 2003-compliant function to get the c_ptr
            ! of the assumed shape array buf
            !
            c_buf = c_loc_assumed_shape_5d_real32(buf, size(buf,dim=1), size(buf,dim=2), size(buf,dim=3), &
                                           size(buf,dim=4), size(buf,dim=5))
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_put_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_put_var_5d_real32


    !-----------------------------------------------------------------------
    !  routine SMIOLf_get_var_d5_real32
    !
    !> \brief Reads a 5-d real32 variable from a file.
    !> \details
    !>  Given a SMIOL file and the name of a variable previously defined in the file,
    !>  this routine will read the contents of the variable into buf according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks load identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument.
    !>
    !>  If the variable has been successfully read from the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_get_var_5d_real32(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_float, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        real(kind=c_float), dimension(:,:,:,:,:), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            !
            ! Invoke a Fortran 2003-compliant function to get the c_ptr
            ! of the assumed shape array buf
            !
            c_buf = c_loc_assumed_shape_5d_real32(buf, size(buf,dim=1), size(buf,dim=2), size(buf,dim=3), &
                                           size(buf,dim=4), size(buf,dim=5))
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_get_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_get_var_5d_real32


    !-----------------------------------------------------------------------
    !  routine c_loc_assumed_shape_5d_real64
    !
    !> \brief Returns a C_PTR for an array with given dimensions
    !> \details
    !>  The Fortran 2003 standard does not permit the use of C_LOC with
    !>  assumed shape arrays. This routine may be used to obtain a C_PTR for
    !>  an assumed shape array by invoking the routine with the first actual
    !>  argument as the assumed-shape array, and subsequent actual arguments
    !>  as, e.g., SIZE(a,DIM=1).
    !>
    !>  Internally, the first dummy argument of this routine can be declared
    !>  as an explicit shape array, which can then be used as an argument to
    !>  C_LOC.
    !>
    !>  Upon success, a C_PTR for the array argument is returned.
    !>
    !>  Note: The actual array argument must not be a zero-sized array.
    !>        Section 15.1.2.5 of the Fortran 2003 standard specifies that
    !>        the argument to C_LOC '...is not an array of zero size...'.
    !
    !-----------------------------------------------------------------------
    function c_loc_assumed_shape_5d_real64(a, d1, d2, d3, d4, d5) result(a_ptr)

        use iso_c_binding, only : c_ptr, c_loc, c_double

        implicit none

        ! Arguments
        integer, intent(in) :: d1, d2, d3, d4, d5
        real(kind=c_double), dimension(d1,d2,d3,d4,d5), target, intent(in) :: a

        ! Return value
        type (c_ptr) :: a_ptr

        a_ptr = c_loc(a)

    end function c_loc_assumed_shape_5d_real64


    !-----------------------------------------------------------------------
    !  routine SMIOLf_put_var_d5_real64
    !
    !> \brief Writes a 5-d real64 variable to a file.
    !> \details
    !>  Given a SMIOL file that was previously opened with write access and the name
    !>  of a variable previously defined in the file with a call to SMIOLf_define_var,
    !>  this routine will write the contents of buf to the variable according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks store identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument. As currently implemented, this routine will write
    !>  the buffer for MPI rank 0 to the variable; however, this behavior should not
    !>  be relied on.
    !>
    !>  If the variable has been successfully written to the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_put_var_5d_real64(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_double, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        real(kind=c_double), dimension(:,:,:,:,:), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            !
            ! Invoke a Fortran 2003-compliant function to get the c_ptr
            ! of the assumed shape array buf
            !
            c_buf = c_loc_assumed_shape_5d_real64(buf, size(buf,dim=1), size(buf,dim=2), size(buf,dim=3), &
                                           size(buf,dim=4), size(buf,dim=5))
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_put_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_put_var_5d_real64


    !-----------------------------------------------------------------------
    !  routine SMIOLf_get_var_d5_real64
    !
    !> \brief Reads a 5-d real64 variable from a file.
    !> \details
    !>  Given a SMIOL file and the name of a variable previously defined in the file,
    !>  this routine will read the contents of the variable into buf according to
    !>  the decomposition described by decomp.
    !>
    !>  If decomp is an associated pointer, the variable is assumed to be decomposed
    !>  across MPI ranks, and all ranks with non-zero-sized partitions of the variable
    !>  must provide a valid buffer. For decomposed variables, all MPI ranks must provide
    !>  an associated decomp pointer, regardless of whether a rank has a non-zero-sized
    !>  partition of the variable.
    !>
    !>  If the variable is not decomposed -- that is, all ranks load identical
    !>  values for the entire variable -- all MPI ranks must provide an unassociated
    !>  pointer for the decomp argument.
    !>
    !>  If the variable has been successfully read from the file, SMIOL_SUCCESS will
    !>  be returned. Otherwise, an error code indicating the nature of the failure
    !>  will be returned.
    !
    !-----------------------------------------------------------------------
    function SMIOLf_get_var_5d_real64(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : c_double, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
        real(kind=c_double), dimension(:,:,:,:,:), pointer :: buf

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf


        !
        ! file is a target, so no need to check that it is associated
        !
        c_file = c_loc(file)

        !
        ! decomp may be an unassociated pointer if the corresponding field is
        ! not decomposed
        !
        if (associated(decomp)) then
            c_decomp = c_loc(decomp)
        else
            c_decomp = c_null_ptr
        end if

        !
        ! Convert variable name string
        !
        allocate(c_varname(len_trim(varname) + 1))
        do i=1,len_trim(varname)
            c_varname(i) = varname(i:i)
        end do
        c_varname(i) = c_null_char

        !
        ! buf may be an unassociated pointer if the calling task does not read
        ! or write any elements of the field
        !
        if (associated(buf)) then

            !
            ! Invoke a Fortran 2003-compliant function to get the c_ptr
            ! of the assumed shape array buf
            !
            c_buf = c_loc_assumed_shape_5d_real64(buf, size(buf,dim=1), size(buf,dim=2), size(buf,dim=3), &
                                           size(buf,dim=4), size(buf,dim=5))
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_get_var(c_file, c_varname, c_decomp, c_buf)


        deallocate(c_varname)

    end function SMIOLf_get_var_5d_real64


