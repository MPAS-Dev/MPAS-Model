! compile with -fconvert=big-endian  !!!!!
! gfortran read_partitions.F90 -fconvert=big-endian
program read_partition
  implicit none
  integer, parameter :: RKIND  = selected_real_kind(12)
   character(len=6), parameter :: charMPIRank = '000000'
   character(len=2), parameter :: Hem = 'NH'
   character(len=17), parameter :: subdir = 'write_partitions/'
   character(len=10), parameter :: xtime = '0001-01-01'
   character(len=60) :: filename
   integer, parameter :: nCells = 203
   integer, parameter :: nVertLevels = 16
   integer(KIND=4), dimension(nCells) :: indexToCellID
   integer(KIND=4), dimension(nCells) :: maxLevelCell
   real(KIND=4), dimension(nVertLevels, nCells) :: temperature, salinity
   real(KIND=4), dimension(nVertLevels, nCells) :: velocityZonal, velocityMeridional, vertVelocityTop
   integer :: iCell, kMax

         filename = 'mpaso.indexToCellID.'//Hem//'.p'//charMPIRank//'.I4.dat'
         open(10, file=subdir//filename, status='old', access='stream')
         read (10) indexToCellID
         close(10)
         print *, 'indexToCellID',indexToCellID

         filename = 'mpaso.maxLevelCell.'//Hem//'.p'//charMPIRank//'.I4.dat'
         open(10, file=subdir//filename, status='old', access='stream')
         read (10) maxLevelCell
         close(10)
         print *, 'maxLevelCell',maxLevelCell

         temperature = -99
         filename = 'mpaso.temperature.'//xtime(1:10)//'.'//Hem//'.p'//charMPIRank//'.R4.dat'
         open(10, file=subdir//filename, status='old', access='stream')
         do iCell = 1, nCells
            kMax = maxLevelCell(iCell)
            read (10) temperature(1:kmax,iCell)
            print *, 'iCell, kmax, temperature',iCell, kmax, temperature(1:kmax,iCell)
         enddo
         close(10)
         print *, 'temperature',temperature(1,1:10)

         salinity = -99
         filename = 'mpaso.salinity.'//xtime(1:10)//'.'//Hem//'.p'//charMPIRank//'.R4.dat'
         open(10, file=subdir//filename, status='old', access='stream')
         do iCell = 1, nCells
            kMax = maxLevelCell(iCell)
            read (10) salinity(1:kmax,iCell)
         enddo
         close(10)
         print *, 'salinity',salinity(1,1:10)

         velocityZonal = -99
         filename = 'mpaso.velocityZonal.'//xtime(1:10)//'.'//Hem//'.p'//charMPIRank//'.R4.dat'
         open(10, file=subdir//filename, status='old', access='stream')
         do iCell = 1, nCells
            kMax = maxLevelCell(iCell)
            read (10) velocityZonal(1:kmax,iCell)
         enddo
         close(10)
         print *, 'velocityZonal',velocityZonal(1,1:10)

         velocityMeridional = -99
         filename = 'mpaso.velocityMeridional.'//xtime(1:10)//'.'//Hem//'.p'//charMPIRank//'.R4.dat'
         open(10, file=subdir//filename, status='old', access='stream')
         do iCell = 1, nCells
            kMax = maxLevelCell(iCell)
            read (10) velocityMeridional(1:kmax,iCell)
         enddo
         close(10)
         print *, 'velocityMeridional',velocityMeridional(1,1:10)

         vertVelocityTop = -99
         filename = 'mpaso.vertVelocityTop.'//xtime(1:10)//'.'//Hem//'.p'//charMPIRank//'.R4.dat'
         open(10, file=subdir//filename, status='old', access='stream')
         do iCell = 1, nCells
            kMax = maxLevelCell(iCell)
            read (10) vertVelocityTop(1:kmax,iCell)
         enddo
         close(10)
         print *, 'vertVelocityTop',vertVelocityTop(1,1:10)

end program read_partition
