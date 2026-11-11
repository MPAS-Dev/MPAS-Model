! Copyright (c) 2015,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
module mpas_atm_threading


    contains


    !-----------------------------------------------------------------------
    !  routine mpas_atm_threading_init
    !
    !> \brief  Pre-computes thread loop bounds for cell, edge, and vertex elements
    !> \author Michael Duda
    !> \date   6 July 2015
    !> \details
    !>  This routine is responsible for computing thread loop bounds for cell,
    !>  edge, and vertex elements in each block of the input blocklist argument.
    !>  Starting and ending loop bounds are computed for these three element
    !>  types for all elements (e.g., nCells) as well as owned elements (e.g.,
    !>  nCellsSolve).
    !>
    !>  When MPAS is compiled without OpenMP support, this routine computes loop
    !>  bounds as though there is just a single thread; otherwise, it is assumed
    !>  that all threads (given by OMP_get_num_threads()) will be used to
    !>  decompose each of the element ranges.
    !>
    !>  At present, a return value of 0 is always returned in the optional
    !>  output argument, ierr.
    !
    !-----------------------------------------------------------------------
    subroutine mpas_atm_threading_init(blocklist, ierr) 

        use mpas_derived_types, only : block_type
        use mpas_log, only : mpas_log_write
        use mpas_pool_routines, only : mpas_pool_get_dimension, mpas_pool_add_dimension
#ifdef MPAS_OPENMP
        use omp_lib
#endif

        implicit none

        type (block_type), pointer :: blocklist
        integer, intent(out), optional :: ierr

        type (block_type), pointer :: block
        integer :: threadid
        integer, pointer :: nCells, nCellsSolve, nEdges, nEdgesSolve, nVertices, nVerticesSolve

        integer :: nThreads
        integer, dimension(:), pointer :: cellThreadStart, cellThreadEnd
        integer, dimension(:), pointer :: cellSolveThreadStart, cellSolveThreadEnd
        integer, dimension(:), pointer :: edgeThreadStart, edgeThreadEnd
        integer, dimension(:), pointer :: edgeSolveThreadStart, edgeSolveThreadEnd
        integer, dimension(:), pointer :: vertexThreadStart, vertexThreadEnd
        integer, dimension(:), pointer :: vertexSolveThreadStart, vertexSolveThreadEnd

        integer :: nThreads_acc
        integer, dimension(:), pointer :: cellThreadStart_acc, cellThreadEnd_acc
        integer, dimension(:), pointer :: cellSolveThreadStart_acc, cellSolveThreadEnd_acc
        integer, dimension(:), pointer :: edgeThreadStart_acc, edgeThreadEnd_acc
        integer, dimension(:), pointer :: edgeSolveThreadStart_acc, edgeSolveThreadEnd_acc
        integer, dimension(:), pointer :: vertexThreadStart_acc, vertexThreadEnd_acc
        integer, dimension(:), pointer :: vertexSolveThreadStart_acc, vertexSolveThreadEnd_acc

        
        if (present(ierr)) ierr = 0

        block => blocklist
        do while (associated(block))
#ifdef MPAS_OPENMP
!$OMP PARALLEL
!$OMP MASTER
            nThreads = OMP_get_num_threads()
!$OMP END MASTER
!$OMP END PARALLEL

#ifdef MPAS_OPENACC
            nThreads_acc = 1
#else
            nThreads_acc = nThreads
#endif
#else 
            nThreads = 1
            nThreads_acc = 1
#endif 

            allocate(cellThreadStart(nThreads))
            allocate(cellThreadEnd(nThreads))
            allocate(cellSolveThreadStart(nThreads))
            allocate(cellSolveThreadEnd(nThreads))
            allocate(edgeThreadStart(nThreads))
            allocate(edgeThreadEnd(nThreads))
            allocate(edgeSolveThreadStart(nThreads))
            allocate(edgeSolveThreadEnd(nThreads))
            allocate(vertexThreadStart(nThreads))
            allocate(vertexThreadEnd(nThreads))
            allocate(vertexSolveThreadStart(nThreads))
            allocate(vertexSolveThreadEnd(nThreads))

            allocate(cellThreadStart_acc(nThreads_acc))
            allocate(cellThreadEnd_acc(nThreads_acc))
            allocate(cellSolveThreadStart_acc(nThreads_acc))
            allocate(cellSolveThreadEnd_acc(nThreads_acc))
            allocate(edgeThreadStart_acc(nThreads_acc))
            allocate(edgeThreadEnd_acc(nThreads_acc))
            allocate(edgeSolveThreadStart_acc(nThreads_acc))
            allocate(edgeSolveThreadEnd_acc(nThreads_acc))
            allocate(vertexThreadStart_acc(nThreads_acc))
            allocate(vertexThreadEnd_acc(nThreads_acc))
            allocate(vertexSolveThreadStart_acc(nThreads_acc))
            allocate(vertexSolveThreadEnd_acc(nThreads_acc))

            call mpas_pool_get_dimension(block % dimensions, 'nCells', nCells)
            call mpas_pool_get_dimension(block % dimensions, 'nCellsSolve', nCellsSolve)
            call mpas_pool_get_dimension(block % dimensions, 'nEdges', nEdges)
            call mpas_pool_get_dimension(block % dimensions, 'nEdgesSolve', nEdgesSolve)
            call mpas_pool_get_dimension(block % dimensions, 'nVertices', nVertices)
            call mpas_pool_get_dimension(block % dimensions, 'nVerticesSolve', nVerticesSolve)

#ifdef MPAS_OPENMP
!$OMP PARALLEL PRIVATE(threadid)
            threadid = OMP_get_thread_num()

            cellThreadStart(threadid+1) = (threadid * nCells / nThreads) + 1
            cellThreadEnd(threadid+1)   = ((threadid+1) * nCells / nThreads)
            cellSolveThreadStart(threadid+1) = (threadid * nCellsSolve / nThreads) + 1
            cellSolveThreadEnd(threadid+1)   = ((threadid+1) * nCellsSolve / nThreads)
            edgeThreadStart(threadid+1) = (threadid * nEdges / nThreads) + 1
            edgeThreadEnd(threadid+1)   = ((threadid+1) * nEdges / nThreads)
            edgeSolveThreadStart(threadid+1) = (threadid * nEdgesSolve / nThreads) + 1
            edgeSolveThreadEnd(threadid+1)   = ((threadid+1) * nEdgesSolve / nThreads)
            vertexThreadStart(threadid+1) = (threadid * nVertices / nThreads) + 1
            vertexThreadEnd(threadid+1)   = ((threadid+1) * nVertices / nThreads)
            vertexSolveThreadStart(threadid+1) = (threadid * nVerticesSolve / nThreads) + 1
            vertexSolveThreadEnd(threadid+1)   = ((threadid+1) * nVerticesSolve / nThreads)
!$OMP END PARALLEL

#ifdef MPAS_OPENACC
!$OMP PARALLEL PRIVATE(threadid)
            threadid = OMP_get_thread_num()

            cellThreadStart_acc(1) = 1
            cellThreadEnd_acc(1)   = nCells
            cellSolveThreadStart_acc(1) = 1
            cellSolveThreadEnd_acc(1)   = nCellsSolve
            edgeThreadStart_acc(1) = 1
            edgeThreadEnd_acc(1)   = nEdges
            edgeSolveThreadStart_acc(1) = 1
            edgeSolveThreadEnd_acc(1)   = nEdgesSolve
            vertexThreadStart_acc(1) = 1
            vertexThreadEnd_acc(1)   = nVertices
            vertexSolveThreadStart_acc(1) = 1
            vertexSolveThreadEnd_acc(1)   = nVerticesSolve
!$OMP END PARALLEL
#else
!$OMP PARALLEL PRIVATE(threadid)
            threadid = OMP_get_thread_num()

            cellThreadStart_acc(threadid+1) = cellThreadStart(threadid+1)
            cellThreadEnd_acc(threadid+1)   = cellThreadEnd(threadid+1)
            cellSolveThreadStart_acc(threadid+1) = cellSolveThreadStart(threadid+1)
            cellSolveThreadEnd_acc(threadid+1)   = cellSolveThreadEnd(threadid+1)
            edgeThreadStart_acc(threadid+1) = edgeThreadStart(threadid+1)
            edgeThreadEnd_acc(threadid+1)   = edgeThreadEnd(threadid+1)
            edgeSolveThreadStart_acc(threadid+1) = edgeSolveThreadStart(threadid+1)
            edgeSolveThreadEnd_acc(threadid+1)   = edgeSolveThreadEnd(threadid+1)
            vertexThreadStart_acc(threadid+1) = vertexThreadStart(threadid+1)
            vertexThreadEnd_acc(threadid+1)   = vertexThreadEnd(threadid+1)
            vertexSolveThreadStart_acc(threadid+1) = vertexSolveThreadStart(threadid+1)
            vertexSolveThreadEnd_acc(threadid+1)   = vertexSolveThreadEnd(threadid+1)
!$OMP END PARALLEL
#endif
#else 
            cellThreadStart(1) = 1
            cellThreadEnd(1)   = nCells
            cellSolveThreadStart(1) = 1
            cellSolveThreadEnd(1)   = nCellsSolve
            edgeThreadStart(1) = 1
            edgeThreadEnd(1)   = nEdges
            edgeSolveThreadStart(1) = 1
            edgeSolveThreadEnd(1)   = nEdgesSolve
            vertexThreadStart(1) = 1
            vertexThreadEnd(1)   = nVertices
            vertexSolveThreadStart(1) = 1
            vertexSolveThreadEnd(1)   = nVerticesSolve

            cellThreadStart_acc(1) = 1
            cellThreadEnd_acc(1)   = nCells
            cellSolveThreadStart_acc(1) = 1
            cellSolveThreadEnd_acc(1)   = nCellsSolve
            edgeThreadStart_acc(1) = 1
            edgeThreadEnd_acc(1)   = nEdges
            edgeSolveThreadStart_acc(1) = 1
            edgeSolveThreadEnd_acc(1)   = nEdgesSolve
            vertexThreadStart_acc(1) = 1
            vertexThreadEnd_acc(1)   = nVertices
            vertexSolveThreadStart_acc(1) = 1
            vertexSolveThreadEnd_acc(1)   = nVerticesSolve
#endif 

            call mpas_pool_add_dimension(block % dimensions, 'nThreads', nThreads)

            call mpas_pool_add_dimension(block % dimensions, 'cellThreadStart', cellThreadStart)
            call mpas_pool_add_dimension(block % dimensions, 'cellThreadEnd', cellThreadEnd)
            call mpas_pool_add_dimension(block % dimensions, 'cellSolveThreadStart', cellSolveThreadStart)
            call mpas_pool_add_dimension(block % dimensions, 'cellSolveThreadEnd', cellSolveThreadEnd)

            call mpas_pool_add_dimension(block % dimensions, 'edgeThreadStart', edgeThreadStart)
            call mpas_pool_add_dimension(block % dimensions, 'edgeThreadEnd', edgeThreadEnd)
            call mpas_pool_add_dimension(block % dimensions, 'edgeSolveThreadStart', edgeSolveThreadStart)
            call mpas_pool_add_dimension(block % dimensions, 'edgeSolveThreadEnd', edgeSolveThreadEnd)

            call mpas_pool_add_dimension(block % dimensions, 'vertexThreadStart', vertexThreadStart)
            call mpas_pool_add_dimension(block % dimensions, 'vertexThreadEnd', vertexThreadEnd)
            call mpas_pool_add_dimension(block % dimensions, 'vertexSolveThreadStart', vertexSolveThreadStart)
            call mpas_pool_add_dimension(block % dimensions, 'vertexSolveThreadEnd', vertexSolveThreadEnd)

            call mpas_pool_add_dimension(block % dimensions, 'nThreads_acc', nThreads_acc)

            call mpas_pool_add_dimension(block % dimensions, 'cellThreadStart_acc', cellThreadStart_acc)
            call mpas_pool_add_dimension(block % dimensions, 'cellThreadEnd_acc', cellThreadEnd_acc)
            call mpas_pool_add_dimension(block % dimensions, 'cellSolveThreadStart_acc', cellSolveThreadStart_acc)
            call mpas_pool_add_dimension(block % dimensions, 'cellSolveThreadEnd_acc', cellSolveThreadEnd_acc)

            call mpas_pool_add_dimension(block % dimensions, 'edgeThreadStart_acc', edgeThreadStart_acc)
            call mpas_pool_add_dimension(block % dimensions, 'edgeThreadEnd_acc', edgeThreadEnd_acc)
            call mpas_pool_add_dimension(block % dimensions, 'edgeSolveThreadStart_acc', edgeSolveThreadStart_acc)
            call mpas_pool_add_dimension(block % dimensions, 'edgeSolveThreadEnd_acc', edgeSolveThreadEnd_acc)

            call mpas_pool_add_dimension(block % dimensions, 'vertexThreadStart_acc', vertexThreadStart_acc)
            call mpas_pool_add_dimension(block % dimensions, 'vertexThreadEnd_acc', vertexThreadEnd_acc)
            call mpas_pool_add_dimension(block % dimensions, 'vertexSolveThreadStart_acc', vertexSolveThreadStart_acc)
            call mpas_pool_add_dimension(block % dimensions, 'vertexSolveThreadEnd_acc', vertexSolveThreadEnd_acc)


!$OMP PARALLEL DO
            do threadid=1,nThreads_acc
                call mpas_log_write('Thread $i cellThreadStart = $i cellThreadEnd = $i', intArgs=(/threadid, cellThreadStart_acc(threadid), cellThreadEnd_acc(threadid)/))
                call mpas_log_write('Thread $i cellSolveThreadStart = $i cellSolveThreadEnd = $i', intArgs=(/threadid, cellSolveThreadStart_acc(threadid), cellSolveThreadEnd_acc(threadid)/))
                call mpas_log_write('Thread $i edgeThreadStart = $i edgeThreadEnd = $i', intArgs=(/threadid, edgeThreadStart_acc(threadid), edgeThreadEnd_acc(threadid)/))
                call mpas_log_write('Thread $i edgeSolveThreadStart = $i edgeSolveThreadEnd = $i', intArgs=(/threadid, edgeSolveThreadStart_acc(threadid), edgeSolveThreadEnd_acc(threadid)/))
                call mpas_log_write('Thread $i vertexThreadStart = $i vertexThreadEnd = $i', intArgs=(/threadid, vertexThreadStart_acc(threadid), vertexThreadEnd_acc(threadid)/))
                call mpas_log_write('Thread $i vertexSolveThreadStart = $i vertexSolveThreadEnd = $i', intArgs=(/threadid, vertexSolveThreadStart_acc(threadid), vertexSolveThreadEnd_acc(threadid)/))
            end do
!$OMP END PARALLEL DO                                      

            !
            ! Because pools make an internal copy of dimensions, we can now
            ! delete our copies of the thread bounds arrays
            !
            deallocate(cellThreadStart)
            deallocate(cellThreadEnd)
            deallocate(cellSolveThreadStart)
            deallocate(cellSolveThreadEnd)
            deallocate(edgeThreadStart)
            deallocate(edgeThreadEnd)
            deallocate(edgeSolveThreadStart)
            deallocate(edgeSolveThreadEnd)
            deallocate(vertexThreadStart)
            deallocate(vertexThreadEnd)
            deallocate(vertexSolveThreadStart)
            deallocate(vertexSolveThreadEnd)

            deallocate(cellThreadStart_acc)
            deallocate(cellThreadEnd_acc)
            deallocate(cellSolveThreadStart_acc)
            deallocate(cellSolveThreadEnd_acc)
            deallocate(edgeThreadStart_acc)
            deallocate(edgeThreadEnd_acc)
            deallocate(edgeSolveThreadStart_acc)
            deallocate(edgeSolveThreadEnd_acc)
            deallocate(vertexThreadStart_acc)
            deallocate(vertexThreadEnd_acc)
            deallocate(vertexSolveThreadStart_acc)
            deallocate(vertexSolveThreadEnd_acc)

            block => block % next
        end do

    end subroutine mpas_atm_threading_init


    !-----------------------------------------------------------------------
    !  routine mpas_atm_threading_finalize
    !
    !> \brief  Deallocates memory associated with threading in MPAS
    !> \author Michael Duda
    !> \date   6 July 2015
    !> \details
    !>  This routine deallocates any memory that was allocated in the call to
    !>  mpas_atm_threading_init() for each block in the input block list.
    !>
    !>  At present, a return value of 0 is always returned in the optional
    !>  output argument, ierr.
    !
    !-----------------------------------------------------------------------
    subroutine mpas_atm_threading_finalize(blocklist, ierr) 

        use mpas_derived_types, only : block_type
        use mpas_pool_routines, only : mpas_pool_remove_dimension

        implicit none

        type (block_type), pointer :: blocklist
        integer, intent(out), optional :: ierr

        type (block_type), pointer :: block


        if (present(ierr)) ierr = 0

        block => blocklist
        do while (associated(block))

            call mpas_pool_remove_dimension(block % dimensions, 'nThreads')

            call mpas_pool_remove_dimension(block % dimensions, 'cellThreadStart')
            call mpas_pool_remove_dimension(block % dimensions, 'cellThreadEnd')
            call mpas_pool_remove_dimension(block % dimensions, 'cellSolveThreadStart')
            call mpas_pool_remove_dimension(block % dimensions, 'cellSolveThreadEnd')

            call mpas_pool_remove_dimension(block % dimensions, 'edgeThreadStart')
            call mpas_pool_remove_dimension(block % dimensions, 'edgeThreadEnd')
            call mpas_pool_remove_dimension(block % dimensions, 'edgeSolveThreadStart')
            call mpas_pool_remove_dimension(block % dimensions, 'edgeSolveThreadEnd')

            call mpas_pool_remove_dimension(block % dimensions, 'vertexThreadStart')
            call mpas_pool_remove_dimension(block % dimensions, 'vertexThreadEnd')
            call mpas_pool_remove_dimension(block % dimensions, 'vertexSolveThreadStart')
            call mpas_pool_remove_dimension(block % dimensions, 'vertexSolveThreadEnd')

            call mpas_pool_remove_dimension(block % dimensions, 'nThreads_acc')

            call mpas_pool_remove_dimension(block % dimensions, 'cellThreadStart_acc')
            call mpas_pool_remove_dimension(block % dimensions, 'cellThreadEnd_acc')
            call mpas_pool_remove_dimension(block % dimensions, 'cellSolveThreadStart_acc')
            call mpas_pool_remove_dimension(block % dimensions, 'cellSolveThreadEnd_acc')

            call mpas_pool_remove_dimension(block % dimensions, 'edgeThreadStart_acc')
            call mpas_pool_remove_dimension(block % dimensions, 'edgeThreadEnd_acc')
            call mpas_pool_remove_dimension(block % dimensions, 'edgeSolveThreadStart_acc')
            call mpas_pool_remove_dimension(block % dimensions, 'edgeSolveThreadEnd_acc')

            call mpas_pool_remove_dimension(block % dimensions, 'vertexThreadStart_acc')
            call mpas_pool_remove_dimension(block % dimensions, 'vertexThreadEnd_acc')
            call mpas_pool_remove_dimension(block % dimensions, 'vertexSolveThreadStart_acc')
            call mpas_pool_remove_dimension(block % dimensions, 'vertexSolveThreadEnd_acc')

            block => block % next
        end do

    end subroutine mpas_atm_threading_finalize
 
end module mpas_atm_threading
