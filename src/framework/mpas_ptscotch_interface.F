! Copyright (c) 2025 The University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at https://mpas-dev.github.io/license.html .
!
#ifdef MPAS_SCOTCH
module mpas_ptscotch_interface
    use iso_c_binding, only : c_ptr
    public :: scotch_dgraphinit, scotch_dgraphbuild

    ! Dummy type declaration for SCOTCH distributed graph
    ! Member ptr is a pointer to the SCOTCH_Dgraph C structure
    type :: scotchm_dgraph
        type(c_ptr) :: ptr
    end type scotchm_dgraph

    ! Dummy type declaration for SCOTCH strategy
    ! Member ptr is a pointer to the SCOTCH_Strat C structure
    type :: scotchm_strat
        type(c_ptr) :: ptr
    end type scotchm_strat

contains

    !-----------------------------------------------------------------------
    !  subroutine scotch_checkintsize
    !
    !> \brief Check that SCOTCH integer size matches MPAS integer size
    !> \author Abishek Gopal
    !> \date   21 Jan 2026
    !> \details
    !>  Compares the size of SCOTCH_Num type with the MPAS integer size.
    !>  Logs an error message if there is a mismatch.
    !-----------------------------------------------------------------------
    subroutine scotch_checkintsize()
        use iso_c_binding, only : c_int, c_size_t, c_sizeof
        use mpas_log, only : mpas_log_write
        use mpas_derived_types, only : MPAS_LOG_CRIT

        implicit none

        integer(c_int) :: dummy_int

        interface
            function scotchm_get_intsize() bind(C, name='scotchm_get_intsize') result(intsize)
                use iso_c_binding, only : c_size_t
                integer(c_size_t) :: intsize
            end function scotchm_get_intsize
        end interface

        if (scotchm_get_intsize() /= c_sizeof(dummy_int)) then
          call mpas_log_write("Error: Scotch SCOTCH_Num size does not match MPAS integer size \n" &
                                // "Please build Scotch with 32-bit integers", MPAS_LOG_CRIT)
        end if

    end subroutine scotch_checkintsize

    !-----------------------------------------------------------------------
    !  subroutine scotch_dgraphinit
    !
    !> \brief Initialize a SCOTCH distributed graph object
    !> \author Abishek Gopal
    !> \date   8 Dec 2025
    !> \details
    !>  Initializes a SCOTCH_Dgraph structure using a Fortran MPI communicator.
    !>  This subroutine wraps the C function scotchm_dgraphinit.
    !> \arguments
    !>   dgraph - scotchm_dgraph structure to be initialized
    !>   comm   - Fortran MPI communicator integer
    !
    !-----------------------------------------------------------------------
    subroutine scotch_dgraphinit(dgraph, comm)
        use iso_c_binding, only : c_ptr
        use mpas_log, only : mpas_log_write
        use mpas_derived_types, only : MPAS_LOG_CRIT

        implicit none
        ! Arguments
        type(scotchm_dgraph), intent(inout) :: dgraph
        integer, intent(in) :: comm

        ! Return value
        integer :: ierr

        interface
            function scotchm_dgraphinit(dgraph_ptr, localcomm) bind(C, name='scotchm_dgraphinit') result(err)
                use iso_c_binding, only : c_ptr, c_int
                type(c_ptr) :: dgraph_ptr
                integer(c_int), value :: localcomm
                integer(c_int) :: err
            end function scotchm_dgraphinit
        end interface

        ierr = scotchm_dgraphinit(dgraph % ptr, comm)

        if (ierr /= 0) then
            call mpas_log_write('Error initalizing distributed Scotch graph', MPAS_LOG_CRIT)
        end if

    end subroutine scotch_dgraphinit

    !-----------------------------------------------------------------------
    !  subroutine scotch_dgraphbuild
    !
    !> \brief Build a SCOTCH distributed graph from local vertex/edge arrays
    !> \author Abishek Gopal
    !> \date   8 Dec 2025
    !> \details
    !>  Constructs a SCOTCH_Dgraph from local vertex and edge connectivity data.
    !>  This subroutine wraps the C function scotchm_dgraphbuild
    !> \arguments
    !>   dgraph         - scotchm_dgraph structure to be built
    !>   nVertices      - Number of local vertices
    !>   vertloctab     - Array of size (nVertices+1)
    !>                     giving the start index of edges for each local vertex
    !>   nLocEdgesGraph - Total number of local edges in the graph
    !>   edgelocsiz     - Size of the adjncy array
    !>   adjncy         - Array of size nLocEdgesGraph containing the  
    !>                     adjacency list for local vertices
    !
    !-----------------------------------------------------------------------
    subroutine scotch_dgraphbuild(dgraph, nVertices, vertloctab, nLocEdgesGraph, edgelocsiz, adjncy)
        use iso_c_binding, only : c_ptr, c_int
        use mpas_log, only : mpas_log_write
        use mpas_derived_types, only : MPAS_LOG_CRIT

        implicit none

        type(scotchm_dgraph) :: dgraph
        integer(c_int), intent(in) :: nVertices
        integer(c_int), intent(in) :: vertloctab(nVertices+1)
        integer(c_int), intent(in) :: nLocEdgesGraph
        integer(c_int), intent(in) :: edgelocsiz
        integer(c_int), intent(in) :: adjncy(nLocEdgesGraph)

        ! Return value
        integer :: ierr

        interface
            function scotchm_dgraphbuild(dgraph_ptr, nVertices, vertloctab, &
                nLocEdgesGraph, edgelocsiz, adjncy) bind(C, name='scotchm_dgraphbuild') result(err)
                use iso_c_binding, only : c_ptr, c_int
                type(c_ptr), value :: dgraph_ptr
                integer(c_int), value :: nVertices
                integer(c_int) :: vertloctab(nVertices+1)
                integer(c_int), value :: nLocEdgesGraph
                integer(c_int), value :: edgelocsiz
                integer(c_int) :: adjncy(nLocEdgesGraph)
                integer(c_int) :: err
            end function scotchm_dgraphbuild
        end interface

        ierr = 0

        ierr = scotchm_dgraphbuild(dgraph % ptr, nVertices, vertloctab,  &
                                   nLocEdgesGraph, edgelocsiz, adjncy)

        if (ierr /= 0) then
            call mpas_log_write('Error building distributed Scotch graph', MPAS_LOG_CRIT)
        else
            call mpas_log_write('Successfully built distributed Scotch graph')    
        end if

    end subroutine scotch_dgraphbuild

    !-----------------------------------------------------------------------
    !  subroutine scotch_dgraphcheck
    !
    !> \brief Perform consistency check on a SCOTCH distributed graph
    !> \author Abishek Gopal
    !> \date   8 Dec 2025
    !> \details
    !>  Validates the internal structure of a SCOTCH_Dgraph for consistency.
    !>  This subroutine wraps the C function scotchm_dgraphcheck.
    !> \arguments
    !>   dgraph - scotchm_dgraph structure to be checked
    !
    !-----------------------------------------------------------------------
    subroutine scotch_dgraphcheck(dgraph)    
        use mpas_log, only : mpas_log_write
        use mpas_derived_types, only : MPAS_LOG_CRIT
        use iso_c_binding, only : c_ptr

        implicit none

        type(scotchm_dgraph) :: dgraph

        ! Return value
        integer :: ierr

        interface
            function scotchm_dgraphcheck(dgraph_ptr) bind(C, name='scotchm_dgraphcheck') result(err)
                use iso_c_binding, only : c_int, c_ptr
                type(c_ptr), value :: dgraph_ptr
                integer(c_int) :: err
            end function scotchm_dgraphcheck
        end interface

        ierr = scotchm_dgraphcheck(dgraph % ptr)

        if (ierr /= 0) then
            call mpas_log_write('Error during distributed Scotch graph check', MPAS_LOG_CRIT)
        end if

    end subroutine scotch_dgraphcheck

    !-----------------------------------------------------------------------
    !  subroutine scotch_dgraphexit
    !
    !> \brief Finalize/cleanup a SCOTCH distributed graph object
    !> \author Abishek Gopal
    !> \date   8 Dec 2025
    !> \details
    !>  Deallocates internal structures associated with a SCOTCH_Dgraph.
    !>  This subroutine wraps the C function scotchm_dgraphexit.
    !> \arguments
    !>   dgraph - scotchm_dgraph structure to be finalized
    !
    !-----------------------------------------------------------------------
    subroutine scotch_dgraphexit(dgraph)        
        use mpas_log, only : mpas_log_write
        use iso_c_binding, only : c_ptr

        implicit none

        type(scotchm_dgraph) :: dgraph

        interface
            subroutine scotchm_dgraphexit(dgraph_ptr) bind(C, name='scotchm_dgraphexit')
                use iso_c_binding, only : c_int, c_ptr
                type(c_ptr), value :: dgraph_ptr
            end subroutine scotchm_dgraphexit
        end interface

        call scotchm_dgraphexit(dgraph % ptr)

    end subroutine scotch_dgraphexit

    !-----------------------------------------------------------------------
    !  subroutine scotch_stratinit
    !
    !> \brief Initialize a SCOTCH strategy object
    !> \author Abishek Gopal
    !> \date   8 Dec 2025
    !> \details
    !>  Initializes a SCOTCH_Strat structure and builds a default strategy
    !>  for distributed graph mapping. This subroutine wraps the C function
    !>  scotchm_stratinit.
    !> \arguments
    !>   stradat - scotchm_strat structure to be initialized
    !
    !-----------------------------------------------------------------------
    subroutine scotch_stratinit(stradat)
        use mpas_log, only : mpas_log_write
        use mpas_derived_types, only : MPAS_LOG_CRIT
        use iso_c_binding, only : c_ptr

        implicit none

        type(scotchm_strat), intent(inout) :: stradat
        
        integer :: ierr

        interface
            function scotchm_stratinit(strat_ptr) bind(C, name='scotchm_stratinit') result(err)
                use iso_c_binding, only : c_int, c_ptr
                type(c_ptr) :: strat_ptr
                integer(c_int) :: err
            end function scotchm_stratinit
        end interface

        ierr = scotchm_stratinit(stradat % ptr)

        if (ierr /= 0) then
            call mpas_log_write('Error during Scotch strategy initialization', MPAS_LOG_CRIT)
        end if

    end subroutine scotch_stratinit

    !-----------------------------------------------------------------------
    !  subroutine scotch_stratexit
    !
    !> \brief Finalize/cleanup a SCOTCH strategy object
    !> \author Abishek Gopal
    !> \date   8 Dec 2025
    !> \details
    !>  Deallocates internal structures associated with a SCOTCH_Strat.
    !>  This subroutine wraps the C function scotchm_stratexit.
    !> \arguments
    !>   stradat - scotchm_strat structure to be finalized
    !
    !-----------------------------------------------------------------------
    subroutine scotch_stratexit(stradat)        
        use mpas_log, only : mpas_log_write
        use iso_c_binding, only : c_ptr

        implicit none

        type(scotchm_strat), intent(in) :: stradat
        
        interface
            subroutine scotchm_stratexit(strat_ptr) bind(C, name='scotchm_stratexit')
                use iso_c_binding, only : c_ptr
                type(c_ptr), value :: strat_ptr
            end subroutine scotchm_stratexit
        end interface

        call scotchm_stratexit(stradat % ptr)

    end subroutine scotch_stratexit

    !-----------------------------------------------------------------------
    !  subroutine scotch_dgraphpart
    !
    !> \brief Partition a SCOTCH distributed graph
    !> \author Abishek Gopal
    !> \date   8 Dec 2025
    !> \details
    !>  Partitions the distributed graph into num_part parts using the
    !>  provided SCOTCH strategy object. This subroutine wraps the C function
    !>  scotchm_dgraphpart.
    !> \arguments
    !>   dgraph  - scotchm_dgraph structure to be partitioned
    !>   num_part - Number of partitions
    !>   stradat - scotchm_strat structure containing partitioning strategy
    !>   parttab - Output array of size equal to number of local vertices,
    !
    !-----------------------------------------------------------------------
    subroutine scotch_dgraphpart(dgraph, num_part, stradat, parttab)
        use mpas_log, only : mpas_log_write
        use mpas_derived_types, only : MPAS_LOG_CRIT
        use iso_c_binding, only : c_ptr, c_int

        implicit none

        type(scotchm_dgraph), intent(in) :: dgraph
        integer(c_int), intent(in) :: num_part
        type(scotchm_strat), intent(in) :: stradat
        integer(c_int), intent(out) :: parttab(*)
        
        ! Return value
        integer :: ierr

        interface
            function scotchm_dgraphpart(dgraph_ptr, num_part_loc, strat_ptr, parttab_loc ) bind(C, name='scotchm_dgraphpart') result(err)
                use iso_c_binding, only : c_int, c_ptr
                type(c_ptr), value :: dgraph_ptr
                integer(c_int), value :: num_part_loc
                type(c_ptr), value :: strat_ptr
                integer(c_int) :: parttab_loc(*)
                integer(c_int) :: err
            end function scotchm_dgraphpart
        end interface

        ierr = scotchm_dgraphpart(dgraph % ptr, num_part, stradat % ptr, parttab)

        if (ierr /= 0) then
            call mpas_log_write('Error during Scotch graph partition', MPAS_LOG_CRIT)
        else 
            call mpas_log_write('Successfully partitioned distributed Scotch graph')
        end if

    end subroutine scotch_dgraphpart

    !-----------------------------------------------------------------------
    !  subroutine scotch_dgraphredist
    !
    !> \brief Redistribute a SCOTCH distributed graph according to partitions
    !> \author Abishek Gopal
    !> \date   8 Dec 2025
    !> \details
    !>  Redistributes the distributed graph structure based on a partition
    !>  table. This subroutine wraps the C function scotchm_dgraphredist.
    !> \arguments
    !>   dgraph  - scotchm_dgraph structure to be redistributed
    !>   parttab - Input array of size equal to number of local vertices,
    !>             containing partition assignments
    !>   dgraph_out - scotchm_dgraph structure to hold redistributed graph
    !>   num_local_vertices - Number of local vertices in the redistributed graph
    !
    !-----------------------------------------------------------------------
    subroutine scotch_dgraphredist(dgraph, parttab, dgraph_out, num_local_vertices)
        use mpas_log, only : mpas_log_write
        use mpas_derived_types, only : MPAS_LOG_CRIT
        use iso_c_binding, only : c_ptr, c_int

        implicit none

        type(scotchm_dgraph) :: dgraph
        integer(c_int), intent(in) :: parttab(*)
        type(scotchm_dgraph) :: dgraph_out
        integer(c_int) :: num_local_vertices
           
        ! Return value
        integer :: ierr

        interface
            function scotchm_dgraphredist(dgraph_ptr, parttab_loc, dgraph_out_ptr, vertlocnbr ) bind(C, name='scotchm_dgraphredist') result(err)
                use iso_c_binding, only : c_int, c_ptr
                type(c_ptr), value :: dgraph_ptr
                integer(c_int) :: parttab_loc(*)
                type(c_ptr), value :: dgraph_out_ptr
                integer(c_int) :: vertlocnbr
                integer(c_int) :: err
            end function scotchm_dgraphredist
        end interface

        ierr = scotchm_dgraphredist(dgraph % ptr, parttab, dgraph_out % ptr, num_local_vertices)

        if (ierr /= 0) then
            call mpas_log_write('Error during Scotch graph redistribution', MPAS_LOG_CRIT)
        end if

    end subroutine scotch_dgraphredist

    !-----------------------------------------------------------------------
    !  subroutine scotch_dgraphdata
    !
    !> \brief Extract vertex labels from a SCOTCH distributed graph
    !> \author Abishek Gopal
    !> \date   8 Dec 2025
    !> \details
    !>  Extracts vertex labels or stored IDs for local vertices into the
    !>  output array. This subroutine wraps the C function scotchm_dgraphdata.
    !> \arguments
    !>   dgraph  - scotchm_dgraph structure to extract from
    !>   local_cell_list - Output array to hold vertex labels for local vertices
    !
    !-----------------------------------------------------------------------
     subroutine scotch_dgraphdata(dgraph, local_cell_list)        
        use mpas_log, only : mpas_log_write
        use iso_c_binding, only : c_ptr, c_int

        implicit none

        type(scotchm_dgraph) :: dgraph
        integer(c_int), intent(out) :: local_cell_list(*)

        interface
            subroutine scotchm_dgraphdata(dgraph_ptr, cell_list) bind(C, name='scotchm_dgraphdata')
                use iso_c_binding, only : c_int, c_ptr
                type(c_ptr), value :: dgraph_ptr
                integer(c_int) :: cell_list(*)
            end subroutine scotchm_dgraphdata
        end interface

        call scotchm_dgraphdata(dgraph % ptr, local_cell_list)

    end subroutine scotch_dgraphdata

end module mpas_ptscotch_interface
#endif
