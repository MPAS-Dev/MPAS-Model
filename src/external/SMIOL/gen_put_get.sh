#!/usr/bin/env sh

filename=smiolf_put_get_var.inc


################################################################################
#
# gen_put_get_var
#
# Generate a function body for a specific "SMIOLf_put/get_var" function
# Required variables:
#  d = 0, 1, 2, 3
#  io = put, get
#  colon_list = ":,:,:"
#  dim_list = "d1,d1,d3"
#  type = real32, real64
#  kind = c_float, c_double
#  base_type = real, integer
#  size_args = "size(buf,dim=1), size(buf,dim=2)"
#
################################################################################
gen_put_get_var()
{
    #
    # For non-scalars, build, e.g., " dimension(:,:,:),"
    #
    if [ $d -ge 1 ]; then
        dim=" dimension(${colon_list}),"
        c_loc_invocation="            !
            ! Invoke a Fortran 2003-compliant function to get the c_ptr
            ! of the assumed shape array buf
            !
            c_buf = c_loc_assumed_shape_${d}d_${type}(buf${size_args})"
    else
        dim=""
        c_loc_invocation="            c_buf = c_loc(buf)"
    fi

    #
    # Character variables need special copying code...
    #
    if [ "${kind}" = "c_char" ]; then

        if [ "${io}" = "put" ]; then

            char_copyin="            allocate(char_buf(len(buf)))
            do i=1,len(buf)
                char_buf(i) = buf(i:i)
            end do"

            char_copyout="        if (associated(buf)) then
            deallocate(char_buf)
        end if"

        else

            char_copyin="            allocate(char_buf(len(buf)))

            ! In case buf contains more characters than will be read from the file,
            ! initialize char_buf with the contents of buf to preserve un-read
            ! characters during the copy of char_buf back into buf later on
            do i=1,len(buf)
                char_buf(i) = buf(i:i)
            end do"

            char_copyout="        if (associated(buf)) then
            do i=1,len(buf)
                buf(i:i) = char_buf(i)
            end do

            deallocate(char_buf)
        end if"

        fi

        dummy_buf_decl="        ${base_type},${dim} pointer :: buf"
        char_buf_decl="        character(kind=c_char), dimension(:), allocatable, target :: char_buf"
        c_loc_invocation="            c_buf = c_loc(char_buf)"

    else
        char_copyin=""
        char_copyout=""
        dummy_buf_decl="        ${base_type}(kind=${kind}),${dim} pointer :: buf"
        char_buf_decl=""
    fi


    #
    # Build function documentation block
    #
    if [ "${io}" = "put" ]; then

    header="    !-----------------------------------------------------------------------
    !  routine SMIOLf_put_var_d${d}_${type}
    !
    !> \brief Writes a ${d}-d ${type} variable to a file.
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
    !-----------------------------------------------------------------------"

    else

    header="    !-----------------------------------------------------------------------
    !  routine SMIOLf_get_var_d${d}_${type}
    !
    !> \brief Reads a ${d}-d ${type} variable from a file.
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
    !-----------------------------------------------------------------------"

    fi

    cat >> ${filename} << EOF
$header
    function SMIOLf_${io}_var_${d}d_${type}(file, varname, decomp, buf) result(ierr)

        use iso_c_binding, only : ${kind}, c_char, c_loc, c_ptr, c_null_ptr, c_null_char

        implicit none

        ! Arguments
        type(SMIOLf_file), target :: file
        character(len=*), intent(in) :: varname
        type(SMIOLf_decomp), pointer :: decomp
${dummy_buf_decl}

        ! Return status code
        integer :: ierr

        ! Local variables
        integer :: i
        character(kind=c_char), dimension(:), pointer :: c_varname
        type (c_ptr) :: c_file
        type (c_ptr) :: c_decomp
        type (c_ptr) :: c_buf
${char_buf_decl}

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
${char_copyin}
${c_loc_invocation}
        else
            c_buf = c_null_ptr
        end if

        ierr = SMIOL_${io}_var(c_file, c_varname, c_decomp, c_buf)

${char_copyout}
        deallocate(c_varname)

    end function SMIOLf_${io}_var_${d}d_${type}


EOF
}


################################################################################
#
# gen_c_loc
#
# Generate a function body for a specific "c_loc_assumed_shape" function
# Required variables:
#  d = 1, 2, 3
#  dim_args = , d1, d1, d3
#  dim_list = d1,d1,d3
#  type = real32, real64
#  kind = c_float, c_double
#  base_type = real, integer
#
################################################################################
gen_c_loc()
{
    #
    # Build, e.g., " dimension(d1,d2,d3),"
    #
    dim=" dimension(${dim_list}),"

    #
    # Build list of dimension argument declarations
    #
    d_decl="integer, intent(in) :: d1"
    i=2
    while [ $i -le $d ]; do
        d_decl="${d_decl}, d$i"
        i=$(($i+1))
    done

    #
    # Build function documentation block
    #
    header="    !-----------------------------------------------------------------------
    !  routine c_loc_assumed_shape_${d}d_${type}
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
    !-----------------------------------------------------------------------"

    cat >> ${filename} << EOF
${header}
    function c_loc_assumed_shape_${d}d_${type}(a${dim_args}) result(a_ptr)

        use iso_c_binding, only : c_ptr, c_loc, ${kind}

        implicit none

        ! Arguments
        ${d_decl}
        ${base_type}(kind=${kind}),${dim} target, intent(in) :: a

        ! Return value
        type (c_ptr) :: a_ptr

        a_ptr = c_loc(a)

    end function c_loc_assumed_shape_${d}d_${type}


EOF
}


################################################################################
#
# gen_put_get.sh
#
################################################################################
printf "" > ${filename}

#
# For each type, handle each dimensionality
#
for d in 0 1 2 3 4 5; do

    #
    # Build list of dimension formal arguments, e.g. ", d1, d2, d3"
    #
    dim_args=''
    i=1
    while [ $i -le $d ]; do
       dim_args="${dim_args}, d$i"
       i=$(($i+1))
    done

    #
    # Build explicit shape list, e.g., "d1,d2,d3"
    #
    dim_list=''
    i=1
    while [ $i -le $d ]; do
        dim_list="${dim_list}d$i"
        if [ $i -lt $d ]; then
            dim_list="${dim_list},"
        fi
        i=$(($i+1))
    done

    #
    # Build assumed shape list, e.g., ":,:,:"
    #
    colon_list=''
    i=1
    while [ $i -le $d ]; do
        colon_list="${colon_list}:"
        if [ $i -lt $d ]; then
            colon_list="${colon_list},"
        fi
        i=$(($i+1))
    done

    #
    # Build array size actual arguments , e.g., "size(buf,dim=1), size(buf,dim=2)"
    #
    size_args=''
    i=1
    while [ $i -le $d ]; do

        # Break long lines after three dimensions
        if [ $i -eq 4 -a $d -ge 4 ]; then
            size_args="${size_args}, &
                                           size(buf,dim=$i)"
        else
            size_args="${size_args}, size(buf,dim=$i)"
        fi

        i=$(($i+1))

    done

    #
    # Create functions for each type
    #
    for type in char real32 real64 int32; do

        # Only up to 0-d char interfaces
        if [ "${type}" = "char" ] && [ $d -gt 0 ]; then
            continue
        fi

        # Only up to 4-d int32 interfaces
        if [ "${type}" = "int32" ] && [ $d -gt 4 ]; then
            continue
        fi

        if [ "$type" = "real32" ]; then
            kind="c_float"
            base_type="real"
        elif [ "$type" = "real64" ]; then
            kind="c_double"
            base_type="real"
        elif [ "$type" = "int32" ]; then
            kind="c_int"
            base_type="integer"
        elif [ "$type" = "char" ]; then
            kind="c_char"
            base_type="character(len=:)"
        fi

        if [ $d -ge 1 ]; then
            gen_c_loc
        fi

        for io in put get; do
            gen_put_get_var
        done

    done

done
