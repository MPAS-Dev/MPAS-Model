! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

module ocn_write_partitions

   use mpas_derived_types
   use mpas_pool_routines
   use mpas_dmpar
   use mpas_timekeeping
   use mpas_stream_manager
   use mpas_constants

   use ocn_constants

   implicit none
   private
   save

   !--------------------------------------------------------------------
   !
   ! Public parameters
   !
   !--------------------------------------------------------------------

   !--------------------------------------------------------------------
   !
   ! Public member functions
   !
   !--------------------------------------------------------------------

   public :: ocn_init_write_partitions, &
             ocn_compute_write_partitions, &
             ocn_restart_write_partitions, &
             ocn_finalize_write_partitions

   !--------------------------------------------------------------------
   !
   ! Private module variables
   !
   !--------------------------------------------------------------------

   logical :: write_this_partition
   character(len=6) :: charMPIRank
   character(len=2) :: Hem
   character(len=17), parameter :: subdir = 'write_partitions/'

!***********************************************************************

contains

!***********************************************************************
!
!  routine ocn_init_write_partitions
!
!> \brief   Initialize MPAS-Ocean analysis member
!> \author  Mark Petersen
!> \date    July 2021
!> \details
!>  This routine conducts all initializations required for the
!>  MPAS-Ocean analysis member.
!
!-----------------------------------------------------------------------

   subroutine ocn_init_write_partitions(domain, err)!{{{

      !-----------------------------------------------------------------
      !
      ! input variables
      !
      !-----------------------------------------------------------------

      !-----------------------------------------------------------------
      !
      ! input/output variables
      !
      !-----------------------------------------------------------------

      type (domain_type), intent(inout) :: domain

      !-----------------------------------------------------------------
      !
      ! output variables
      !
      !-----------------------------------------------------------------

      integer, intent(out) :: err !< Output: error flag

      !-----------------------------------------------------------------
      !
      ! local variables
      !
      !-----------------------------------------------------------------

      type (block_type), pointer :: block
      type (mpas_pool_type), pointer :: meshPool
      real (kind=RKIND), dimension(:), pointer :: latCell, lonCell
      real (kind=RKIND) :: latMin, latMax
      integer, dimension(:), pointer :: indexToCellID
      integer, dimension(:), pointer :: maxLevelCell
      integer, pointer :: nCellsSolve
      integer :: mpiRank
      character(len=StrKIND) :: filename
      err = 0

      mpiRank = domain % dminfo % my_proc_id
      write (charMPIRank,'(I6.6)' ) mpiRank

      block => domain % blocklist
      do while (associated(block))
         call mpas_pool_get_subpool(block % structs, 'mesh', meshPool)
         call mpas_pool_get_dimension(meshPool, 'nCellsSolve', nCellsSolve)
         call mpas_pool_get_array(meshPool, 'lonCell', lonCell)
         call mpas_pool_get_array(meshPool, 'latCell', latCell)
         call mpas_pool_get_array(meshPool, 'indexToCellID', indexToCellID)
         call mpas_pool_get_array(meshPool, 'maxLevelCell', maxLevelCell)
         latMax = maxval(latCell(1:nCellsSolve))*180.0_RKIND/pii
         latMin = minval(latCell(1:nCellsSolve))*180.0_RKIND/pii
         if (latMax > 50.0) then
             write_this_partition = .true.
             Hem = 'NH'
         elseif (latMin < -50.0 ) then
             write_this_partition = .true.
             Hem = 'SH'
         else
             write_this_partition = .false.
             Hem = 'EQ'
         endif

         filename = 'mpaso.stats.'//Hem//'.p'//charMPIRank//'.txt'
         open(unit=10, file=subdir//filename)
         write (10, '(A, I10)')  'nCells = ', nCellsSolve
         write (10, '(A, F7.2)') 'latMin = ', latMin
         write (10, '(A, F7.2)') 'latMax = ', latMax
         close(unit=10)

         if (.not.write_this_partition) return

         filename = 'mpaso.indexToCellID.'//Hem//'.p'//charMPIRank//'.I2.dat'
         open(unit=10, file=subdir//filename, status='replace', form='unformatted')
         write (10) int(indexToCellID(1:nCellsSolve),2)
         close(unit=10)

         filename = 'mpaso.maxLevelCell.'//Hem//'.p'//charMPIRank//'.I1.dat'
         open(unit=10, file=subdir//filename, status='replace', form='unformatted')
         write (10) int(maxLevelCell(1:nCellsSolve),1)
         close(unit=10)

         block => block % next
      end do



   end subroutine ocn_init_write_partitions!}}}

!***********************************************************************
!
!  routine ocn_compute_write_partitions
!
!> \brief   Compute MPAS-Ocean analysis member
!> \author  Mark Petersen
!> \date    July 2021
!> \details
!>  This routine conducts all computation required for this
!>  MPAS-Ocean analysis member.
!
!-----------------------------------------------------------------------

   subroutine ocn_compute_write_partitions(domain, timeLevel, err)!{{{

      !-----------------------------------------------------------------
      !
      ! input variables
      !
      !-----------------------------------------------------------------

      integer, intent(in) :: timeLevel

      !-----------------------------------------------------------------
      !
      ! input/output variables
      !
      !-----------------------------------------------------------------

      type (domain_type), intent(inout) :: domain

      !-----------------------------------------------------------------
      !
      ! output variables
      !
      !-----------------------------------------------------------------

      integer, intent(out) :: err !< Output: error flag

      !-----------------------------------------------------------------
      !
      ! local variables
      !
      !-----------------------------------------------------------------

      type (dm_info) :: dminfo
      type (block_type), pointer :: block
      type (mpas_pool_type), pointer :: statePool, meshPool, diagnosticsPool, tracersPool
      character (len=StrKIND), pointer :: xtime
      real (kind=RKIND), dimension(:), pointer :: latCell, lonCell
      real (kind=RKIND), dimension(:,:,:), pointer :: activeTracers
      real (kind=RKIND), dimension(:,:), pointer :: velocityZonal, velocityMeridional, vertVelocityTop
      integer, dimension(:), pointer :: maxLevelCell
      integer, pointer :: indexTemperature
      integer, pointer :: indexSalinity
      integer, pointer :: nCellsSolve
      integer(KIND=2), dimension(100) :: intCol
      integer :: iCell, kMax, minScale, maxScale
      character(len=StrKIND) :: filename

      err = 0

      if (.not.write_this_partition) return

      dminfo = domain % dminfo

      block => domain % blocklist
      do while (associated(block))
         call mpas_pool_get_subpool(block % structs, 'mesh', meshPool)
         call mpas_pool_get_subpool(block % structs, 'state', statePool)
         call mpas_pool_get_subpool(block % structs, 'diagnostics', diagnosticsPool)
         call mpas_pool_get_dimension(meshPool, 'nCellsSolve', nCellsSolve)
         call mpas_pool_get_array(meshPool, 'maxLevelCell', maxLevelCell)
         call mpas_pool_get_array(diagnosticsPool, 'xtime', xtime)
   
         call mpas_pool_get_subpool(statePool, 'tracers', tracersPool)
         call mpas_pool_get_dimension(tracersPool, 'index_temperature', indexTemperature)
         call mpas_pool_get_dimension(tracersPool, 'index_salinity', indexSalinity)
         call mpas_pool_get_array(tracersPool, 'activeTracers', activeTracers, 1)
         call mpas_pool_get_array(diagnosticsPool, 'velocityZonal', velocityZonal)
         call mpas_pool_get_array(diagnosticsPool, 'velocityMeridional', velocityMeridional)
         call mpas_pool_get_array(diagnosticsPool, 'vertVelocityTop', vertVelocityTop)

         minScale = -2.0_RKIND
         maxScale = 30.0_RKIND
         filename = 'mpaso.temperature.'//xtime(1:10)//'.'//Hem//'.p'//charMPIRank//'.I2.dat'
         open(unit=10, file=subdir//filename, status='replace', form='unformatted')
         do iCell = 1, nCellsSolve
            kMax = maxLevelCell(iCell)
            intCol(1:kMax) = (activeTracers(indexTemperature,1:kMax,iCell) - minScale)/(maxScale - minScale) * 65535
            write (10) intCol(1:kMax)
         enddo
         close(unit=10)

         minScale =  0.0_RKIND
         maxScale = 40.0_RKIND
         filename = 'mpaso.salinity.'//xtime(1:10)//'.'//Hem//'.p'//charMPIRank//'.I2.dat'
         open(unit=10, file=subdir//filename, status='replace', form='unformatted')
         do iCell = 1, nCellsSolve
            kMax = maxLevelCell(iCell)
            intCol(1:kMax) = (activeTracers(indexSalinity,1:kMax,iCell) - minScale)/(maxScale - minScale) * 65535
            write (10) intCol(1:kMax)
         enddo
         close(unit=10)

         minScale = -1.0_RKIND
         maxScale =  1.0_RKIND
         filename = 'mpaso.velocityZonal.'//xtime(1:10)//'.'//Hem//'.p'//charMPIRank//'.I2.dat'
         open(unit=10, file=subdir//filename, status='replace', form='unformatted')
         do iCell = 1, nCellsSolve
            kMax = maxLevelCell(iCell)
            intCol(1:kMax) = (velocityZonal(1:kMax,iCell) - minScale)/(maxScale - minScale) * 65535
            write (10) intCol(1:kMax)
         enddo
         close(unit=10)

         minScale = -1.0_RKIND
         maxScale =  1.0_RKIND
         filename = 'mpaso.velocityMeridional.'//xtime(1:10)//'.'//Hem//'.p'//charMPIRank//'.I2.dat'
         open(unit=10, file=subdir//filename, status='replace', form='unformatted')
         do iCell = 1, nCellsSolve
            kMax = maxLevelCell(iCell)
            intCol(1:kMax) = (velocityMeridional(1:kMax,iCell) - minScale)/(maxScale - minScale) * 65535
            write (10) intCol(1:kMax)
         enddo
         close(unit=10)

         filename = 'mpaso.vertVelocityTop.'//xtime(1:10)//'.'//Hem//'.p'//charMPIRank//'.R4.dat'
         open(unit=10, file=subdir//filename, status='replace', form='unformatted')
         do iCell = 1, nCellsSolve
            write (10) real(vertVelocityTop(1:maxLevelCell(iCell),iCell), 4)
         enddo
         close(unit=10)

         block => block % next
      end do

   end subroutine ocn_compute_write_partitions!}}}

!***********************************************************************
!
!  routine ocn_restart_write_partitions
!
!> \brief   Save restart for MPAS-Ocean analysis member
!> \author  Mark Petersen
!> \date    July 2021
!> \details
!>  This routine conducts computation required to save a restart state
!>  for the MPAS-Ocean analysis member.
!
!-----------------------------------------------------------------------

   subroutine ocn_restart_write_partitions(domain, err)!{{{

      !-----------------------------------------------------------------
      !
      ! input variables
      !
      !-----------------------------------------------------------------

      !-----------------------------------------------------------------
      !
      ! input/output variables
      !
      !-----------------------------------------------------------------

      type (domain_type), intent(inout) :: domain

      !-----------------------------------------------------------------
      !
      ! output variables
      !
      !-----------------------------------------------------------------

      integer, intent(out) :: err !< Output: error flag

      !-----------------------------------------------------------------
      !
      ! local variables
      !
      !-----------------------------------------------------------------

      err = 0

   end subroutine ocn_restart_write_partitions!}}}

!***********************************************************************
!
!  routine ocn_finalize_write_partitions
!
!> \brief   Finalize MPAS-Ocean analysis member
!> \author  Mark Petersen
!> \date    July 2021
!> \details
!>  This routine conducts all finalizations required for this
!>  MPAS-Ocean analysis member.
!
!-----------------------------------------------------------------------

   subroutine ocn_finalize_write_partitions(domain, err)!{{{

      !-----------------------------------------------------------------
      !
      ! input variables
      !
      !-----------------------------------------------------------------

      !-----------------------------------------------------------------
      !
      ! input/output variables
      !
      !-----------------------------------------------------------------

      type (domain_type), intent(inout) :: domain

      !-----------------------------------------------------------------
      !
      ! output variables
      !
      !-----------------------------------------------------------------

      integer, intent(out) :: err !< Output: error flag

      !-----------------------------------------------------------------
      !
      ! local variables
      !
      !-----------------------------------------------------------------

      err = 0

   end subroutine ocn_finalize_write_partitions!}}}

end module ocn_write_partitions

! vim: foldmethod=marker
