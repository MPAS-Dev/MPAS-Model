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
   real(KIND=RKIND), parameter :: minT = -2.0_RKIND
   real(KIND=RKIND), parameter :: maxT = 30.0_RKIND
   real(KIND=RKIND), parameter :: minS =  0.0_RKIND
   real(KIND=RKIND), parameter :: maxS = 40.0_RKIND
   real(KIND=RKIND), parameter :: minV = -1.0_RKIND
   real(KIND=RKIND), parameter :: maxV =  1.0_RKIND

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
      real (kind=RKIND) :: latMin, latMax, lonMin, lonMax
      integer, dimension(:), pointer :: indexToCellID
      integer, dimension(:), pointer :: maxLevelCell
      integer, pointer :: nCellsSolve, nVertLevels
      integer :: mpiRank, iCell
      character(len=StrKIND) :: filename
   integer, dimension(10) :: varI
   integer(KIND=1), dimension(10) :: varI1
   integer(KIND=2), dimension(10) :: varI2
   integer(KIND=4), dimension(10) :: varI4
   integer(KIND=8), dimension(10) :: varI8

   do iCell = 1,10
       varI(iCell) = iCell
   enddo
   varI1 = varI
   varI2 = varI
   varI4 = varI
   varI8 = varI

         filename = 'varI.dat'
         open(10, file=subdir//filename, status='replace', access='stream')
         write (10) varI
         close(10)
         filename = 'varI1.dat'
         open(10, file=subdir//filename, status='replace', access='stream')
         write (10) varI1
         close(10)
         filename = 'varI2.dat'
         open(10, file=subdir//filename, status='replace', access='stream')
         write (10) varI2
         close(10)
         filename = 'varI4.dat'
         open(10, file=subdir//filename, status='replace', access='stream')
         write (10) varI4
         close(10)
         filename = 'varI8.dat'
         open(10, file=subdir//filename, status='replace', access='stream')
         write (10) varI8
         close(10)
      err = 0

      mpiRank = domain % dminfo % my_proc_id
      write (charMPIRank,'(I6.6)' ) mpiRank
      call mpas_pool_get_dimension(domain % blocklist % dimensions, 'nVertLevels', nVertLevels)

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
         lonMax = maxval(lonCell(1:nCellsSolve))*180.0_RKIND/pii
         lonMin = minval(lonCell(1:nCellsSolve))*180.0_RKIND/pii
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
         open(10, file=subdir//filename)
         write (10, '(A)')  'nCells, nVertLevels, latMin, latMax, lonMin, lonMax'
         write (10, '(I10)')  nCellsSolve
         write (10, '(I10)')  nVertLevels
         write (10, '(F7.2)') latMin
         write (10, '(F7.2)') latMax
         write (10, '(F7.2)') lonMin
         write (10, '(F7.2)') lonMax
         close(10)

         if (.not.write_this_partition) return

         filename = 'mpaso.indexToCellID.'//Hem//'.p'//charMPIRank//'.I4.dat'
         open(10, file=subdir//filename, status='replace', access='stream')
         write (10) real(indexToCellID(1:nCellsSolve),4)
         close(10)

         filename = 'mpaso.maxLevelCell.'//Hem//'.p'//charMPIRank//'.I1.dat'
         open(10, file=subdir//filename, status='replace', access='stream')
         write (10) int(maxLevelCell(1:nCellsSolve),1)
         close(10)

         filename = 'mpaso.latCell.'//Hem//'.p'//charMPIRank//'.R4.dat'
         open(10, file=subdir//filename, status='replace', access='stream')
         write (10) real(latCell(1:nCellsSolve),4)
         close(10)

         filename = 'mpaso.lonCell.'//Hem//'.p'//charMPIRank//'.R4.dat'
         open(10, file=subdir//filename, status='replace', access='stream')
         write (10) real(lonCell(1:nCellsSolve),4)
         close(10)

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
      real (kind=RKIND), dimension(:,:,:), pointer :: activeTracers
      real (kind=RKIND), dimension(:,:), pointer :: velocityZonal, velocityMeridional, vertVelocityTop
      integer, dimension(:), pointer :: maxLevelCell
      integer, pointer :: indexTemperature, indexSalinity, nCellsSolve, nVertLevels
      real(KIND=4), dimension(:), allocatable :: R4Col
      integer :: iCell, kMax
      character(len=StrKIND) :: filename

      err = 0

      if (.not.write_this_partition) return

      dminfo = domain % dminfo
      call mpas_pool_get_dimension(domain % blocklist % dimensions, 'nVertLevels', nVertLevels)
      allocate(R4Col(nVertLevels))

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

         filename = 'mpaso.temperature.'//xtime(1:10)//'.'//Hem//'.p'//charMPIRank//'.R4.dat'
         open(10, file=subdir//filename, status='replace', access='stream')
         do iCell = 1, nCellsSolve
            kMax = maxLevelCell(iCell)
            R4Col(1:kMax) =  real(activeTracers(indexTemperature,1:kMax,iCell), 4)
            write (10) R4Col(1:kMax)
         enddo
         close(10)

         filename = 'mpaso.salinity.'//xtime(1:10)//'.'//Hem//'.p'//charMPIRank//'.R4.dat'
         open(10, file=subdir//filename, status='replace', access='stream')
         do iCell = 1, nCellsSolve
            write (10) real(activeTracers(indexSalinity,1:maxLevelCell(iCell),iCell), 4)
         enddo
         close(10)

         filename = 'mpaso.velocityZonal.'//xtime(1:10)//'.'//Hem//'.p'//charMPIRank//'.R4.dat'
         open(10, file=subdir//filename, status='replace', access='stream')
         do iCell = 1, nCellsSolve
            write (10) real(velocityZonal(1:maxLevelCell(iCell),iCell), 4)
         enddo
         close(10)

         filename = 'mpaso.velocityMeridional.'//xtime(1:10)//'.'//Hem//'.p'//charMPIRank//'.R4.dat'
         open(10, file=subdir//filename, status='replace', access='stream')
         do iCell = 1, nCellsSolve
            write (10) real(velocityMeridional(1:maxLevelCell(iCell),iCell), 4)
         enddo
         close(10)

         filename = 'mpaso.vertVelocityTop.'//xtime(1:10)//'.'//Hem//'.p'//charMPIRank//'.R4.dat'
         open(10, file=subdir//filename, status='replace', access='stream')
         do iCell = 1, nCellsSolve
            write (10) real(vertVelocityTop(1:maxLevelCell(iCell),iCell), 4)
         enddo
         close(10)

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
