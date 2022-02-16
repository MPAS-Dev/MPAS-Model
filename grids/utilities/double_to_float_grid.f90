!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Program: double_to_float_grid
!
! This program may be used to convert a double-precision MPAS-A grid file to 
!    a single-precision grid file. Since the topology of the mesh remains
!    unchanged, grid decomposition files (e.g., x1.10242.graph.info.part.16)
!    from the double-precision grid can be used with the corresponding
!    single-precision grid.
!
! Example compilation commands:
!    gfortran -ffree-form -o double_to_float_grid double_to_float_grid.f90 -I${NETCDF}/include -L${NETCDF}/lib -lnetcdff -lnetcdf
!    pgf90 -o double_to_float_grid double_to_float_grid.f90 -I${NETCDF}/include -L${NETCDF}/lib -lnetcdff -lnetcdf
!    ifort -o double_to_float_grid double_to_float_grid.f90 -I${NETCDF}/include -L${NETCDF}/lib -lnetcdff -lnetcdf
!
! Initial version: 19 November 2013, Michael Duda, NCAR
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program double_to_float_grid

    implicit none

    include 'netcdf.inc'

    integer :: i, ierr
    integer :: ncid_in, ncid_out
    character (len=256) :: r8_filename, r4_filename

    character (len=32), dimension(9) :: dims = [ character(len=32) :: &
                                                'nCells', &
                                                'nEdges', &
                                                'nVertices', &
                                                'maxEdges', &
                                                'maxEdges2', &
                                                'TWO', &
                                                'vertexDegree', &
                                                'nVertLevels', &
                                                'Time' &
                                               ]

    character (len=32), dimension(38) :: fields = [ character(len=32) :: &
                                                   'latCell', &
                                                   'lonCell', &
                                                   'meshDensity', &
                                                   'xCell', &
                                                   'yCell', &
                                                   'zCell', &
                                                   'indexToCellID', &
                                                   'latEdge', &
                                                   'lonEdge', &
                                                   'xEdge', &
                                                   'yEdge', &
                                                   'zEdge', &
                                                   'indexToEdgeID', &
                                                   'latVertex', &
                                                   'lonVertex', &
                                                   'xVertex', &
                                                   'yVertex', &
                                                   'zVertex', &
                                                   'indexToVertexID', &
                                                   'cellsOnEdge', &
                                                   'nEdgesOnCell', &
                                                   'nEdgesOnEdge', &
                                                   'edgesOnCell', &
                                                   'edgesOnEdge', &
                                                   'weightsOnEdge', &
                                                   'dvEdge', &
                                                   'dv1Edge', &
                                                   'dv2Edge', &
                                                   'dcEdge', &
                                                   'angleEdge', &
                                                   'areaCell', &
                                                   'areaTriangle', &
                                                   'cellsOnCell', &
                                                   'verticesOnCell', &
                                                   'verticesOnEdge', &
                                                   'edgesOnVertex', &
                                                   'cellsOnVertex', &
                                                   'kiteAreasOnVertex' &
                                                  ]

    character (len=32), dimension(6) :: atts = [ character(len=32) :: &
                                                'on_a_sphere', &
                                                'sphere_radius', &
                                                'np', &
                                                'n_scvt_iterations', &
                                                'eps', &
                                                'Convergence' &
                                               ]

    !
    ! Requires two command-line arguments: the input double-precision file and an output single-precision file
    !
    if (command_argument_count() /= 2) then
        write(0,*) ' '
        write(0,*) 'Usage: double_to_float_grid <double-precision grid> <single-precision grid>'
        write(0,*) ' '
        stop
    end if

    call get_command_argument(1, r8_filename)
    call get_command_argument(2, r4_filename)


    ierr = nf_open(trim(r8_filename), NF_NOWRITE, ncid_in)
    if (ierr /= NF_NOERR) then
        write(0,*) '********************************************************************************'
        write(0,*) 'Error: Could not open double-precision input file '//trim(r8_filename)
        write(0,*) '********************************************************************************'
        stop
    end if

    ierr = nf_create(trim(r4_filename), ior(NF_NOCLOBBER,NF_64BIT_OFFSET), ncid_out)

    if (ierr == NF_EEXIST) then
        write(0,*) '********************************************************************************'
        write(0,*) 'Error: File '//trim(r4_filename)//' already exists. Please choose another filename.'
        write(0,*) '********************************************************************************'
        stop
    else if (ierr /= NF_NOERR) then
        write(0,*) '********************************************************************************'
        write(0,*) 'Error: Could not create single-precision output file '//trim(r4_filename)
        write(0,*) '********************************************************************************'
        stop
    end if

    !
    ! Transfer dimensions
    !
    do i=1,size(dims)
        call xfer_dim(ncid_in, ncid_out, trim(dims(i)))
    end do

    !
    ! Transfer field definitions
    !
    do i=1,size(fields)
        call xfer_var(ncid_in, ncid_out, trim(fields(i)))
    end do

    !
    ! Transfer global attributes
    !
    do i=1,size(atts)
        call xfer_att(ncid_in, ncid_out, trim(atts(i)))
    end do


    ierr = nf_enddef(ncid_out)


    !
    ! Transfer fields themselves
    !
    do i=1,size(fields)
        call copy_var(ncid_in, ncid_out, trim(fields(i)))
    end do

    ierr = nf_close(ncid_in)
    ierr = nf_close(ncid_out)

    write(0,*) ' '
    write(0,*) 'Successfully created single-precision file '//trim(r4_filename)
    write(0,*) ' '

    stop


contains


    subroutine xfer_dim(ncid_in, ncid_out, dimname)

        implicit none

        integer, intent(in) :: ncid_in, ncid_out
        character(len=*), intent(in) :: dimname

        integer :: dim_in, dim_out, ierr
        integer :: dimlen

        ierr = nf_inq_dimid(ncid_in, dimname, dim_in)
        ierr = nf_inq_dimlen(ncid_in, dim_in, dimlen)
        ierr = nf_def_dim(ncid_out, dimname, dimlen, dim_out)

    end subroutine xfer_dim


    subroutine xfer_att(ncid_in, ncid_out, attname)

        implicit none

        integer, intent(in) :: ncid_in, ncid_out
        character(len=*), intent(in) :: attname

        integer :: att_in, att_out, ierr
        integer :: attlen, atttype
        integer :: iatt
        double precision :: datt
        real :: ratt
        character(len=128) :: catt

        ierr = nf_inq_att(ncid_in, NF_GLOBAL, attname, atttype, attlen)

        if (atttype == NF_INT) then
            ierr = nf_get_att_int(ncid_in, NF_GLOBAL, attname, iatt)
            ierr = nf_put_att_int(ncid_out, NF_GLOBAL, attname, NF_INT, attlen, iatt)
        else if (atttype == NF_DOUBLE) then
            ierr = nf_get_att_double(ncid_in, NF_GLOBAL, attname, datt)
            ratt = datt
            ierr = nf_put_att_real(ncid_out, NF_GLOBAL, attname, NF_REAL, attlen, ratt)
        else if (atttype == NF_CHAR) then
            ierr = nf_get_att_text(ncid_in, NF_GLOBAL, attname, catt)
            ierr = nf_put_att_text(ncid_out, NF_GLOBAL, attname, attlen, catt)
        end if

    end subroutine xfer_att


    subroutine xfer_var(ncid_in, ncid_out, varname)

        implicit none

        integer, intent(in) :: ncid_in, ncid_out
        character(len=*), intent(in) :: varname

        integer :: var_in, var_out, ierr, i
        integer :: vartype
        integer :: varndims
        integer, dimension(5) :: vardimids
        character (len=30), dimension(5) :: dimnames

        ierr = nf_inq_varid(ncid_in, varname, var_in)
        ierr = nf_inq_vartype(ncid_in, var_in, vartype)
        ierr = nf_inq_varndims(ncid_in, var_in, varndims)
        ierr = nf_inq_vardimid(ncid_in, var_in, vardimids)
        do i=1,varndims
            ierr = nf_inq_dimname(ncid_in, vardimids(i), dimnames(i))
        end do

        do i=1,varndims
            ierr = nf_inq_dimid(ncid_out, trim(dimnames(i)), vardimids(i))
        end do

        if (vartype == NF_DOUBLE) then
            ierr = nf_def_var(ncid_out, varname, NF_REAL, varndims, vardimids(1:varndims), var_out)
        else
            ierr = nf_def_var(ncid_out, varname, vartype, varndims, vardimids(1:varndims), var_out)
        end if

    end subroutine xfer_var


    subroutine copy_var(ncid_in, ncid_out, varname)

        implicit none

        integer, intent(in) :: ncid_in, ncid_out
        character(len=*), intent(in) :: varname

        integer :: var_in, var_out, ierr, i
        integer :: vartype
        integer :: varndims
        integer, dimension(5) :: vardimids
        integer, dimension(5) :: vardimlens
        integer, dimension(:), allocatable :: int1
        integer, dimension(:,:), allocatable :: int2
        double precision, dimension(:), allocatable :: dbl1
        double precision, dimension(:,:), allocatable :: dbl2
        real, dimension(:), allocatable :: real1
        real, dimension(:,:), allocatable :: real2

        ierr = nf_inq_varid(ncid_in, varname, var_in)
        ierr = nf_inq_varid(ncid_out, varname, var_out)

        ierr = nf_inq_vartype(ncid_in, var_in, vartype)
        ierr = nf_inq_varndims(ncid_in, var_in, varndims)
        ierr = nf_inq_vardimid(ncid_in, var_in, vardimids)
        do i=1,varndims
            ierr = nf_inq_dimlen(ncid_in, vardimids(i), vardimlens(i))
        end do

        if (vartype == NF_INT) then
            if (varndims == 1) then
                allocate(int1(vardimlens(1)))
                ierr = nf_get_var_int(ncid_in, var_in, int1)
                ierr = nf_put_var_int(ncid_out, var_out, int1)
                deallocate(int1)
            else if (varndims == 2) then
                allocate(int2(vardimlens(1),vardimlens(2)))
                ierr = nf_get_var_int(ncid_in, var_in, int2)
                ierr = nf_put_var_int(ncid_out, var_out, int2)
                deallocate(int2)
            end if
        else if (vartype == NF_DOUBLE) then
            if (varndims == 1) then
                allocate(dbl1(vardimlens(1)))
                allocate(real1(vardimlens(1)))
                ierr = nf_get_var_double(ncid_in, var_in, dbl1)
                real1(:) = dbl1(:)
                ierr = nf_put_var_real(ncid_out, var_out, real1)
                deallocate(dbl1)
                deallocate(real1)
            else if (varndims == 2) then
                allocate(dbl2(vardimlens(1),vardimlens(2)))
                allocate(real2(vardimlens(1),vardimlens(2)))
                ierr = nf_get_var_double(ncid_in, var_in, dbl2)
                real2(:,:) = dbl2(:,:)
                ierr = nf_put_var_real(ncid_out, var_out, real2)
                deallocate(dbl2)
                deallocate(real2)
            end if
        end if

    end subroutine copy_var

end program double_to_float_grid
