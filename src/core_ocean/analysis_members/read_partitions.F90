! compile with -fconvert=big-endian  !!!!!
!  gfortran write_int.F90 -fconvert=big-endian
program read_partition
  implicit none
  integer, parameter :: RKIND  = selected_real_kind(12)
   real(KIND=RKIND), parameter :: minT = -2.0_RKIND
   real(KIND=RKIND), parameter :: maxT = 30.0_RKIND
   real(KIND=RKIND), parameter :: minS =  0.0_RKIND
   real(KIND=RKIND), parameter :: maxS = 40.0_RKIND
   real(KIND=RKIND), parameter :: minV = -1.0_RKIND
   real(KIND=RKIND), parameter :: maxV =  1.0_RKIND
   character(len=6), parameter :: charMPIRank = '000000'
   character(len=2), parameter :: Hem = 'SH'
   character(len=17), parameter :: subdir = 'write_partitions/'
   character(len=10), parameter :: xtime = '0001-01-01'
   character(len=50) :: filename
   integer, parameter :: nCells = 206
   integer, parameter :: nVertLevels = 16
   integer(KIND=4), dimension(nCells) :: indexToCellID
   real(KIND=4), dimension(nCells) :: tempData
   integer(KIND=1), dimension(nCells) :: maxLevelCell
   real(KIND=4), dimension(:), allocatable :: R4Col
   real(KIND=8), dimension(nVertLevels, nCells) :: temperature
   integer :: iCell, kMax
   integer, dimension(10) :: varI
   integer(KIND=1), dimension(10) :: varI1
   integer(KIND=2), dimension(10) :: varI2
   integer(KIND=4), dimension(10) :: varI4
   integer(KIND=8), dimension(10) :: varI8

         filename = 'varI.dat'
         open(10, file=subdir//filename, status='old', access='stream')
         read (10) varI
         close(10)
         print *, 'varI',varI
         filename = 'varI1.dat'
         open(10, file=subdir//filename, status='old', access='stream')
         read (10) varI1
         close(10)
         print *, 'varI1',varI1
         filename = 'varI2.dat'
         open(10, file=subdir//filename, status='old', access='stream')
         read (10) varI2
         close(10)
         print *, 'varI2',varI2
         filename = 'varI4.dat'
         open(10, file=subdir//filename, status='old', access='stream')
         read (10) varI4
         close(10)
         print *, 'varI4',varI4
         filename = 'varI8.dat'
         open(10, file=subdir//filename, status='old', access='stream')
         read (10) varI8
         close(10)
         print *, 'varI8',varI8

         !filename = 'mpaso.indexToCellID.'//Hem//'.p'//charMPIRank//'.I4.dat'
         !open(10, file=subdir//filename, status='old', access='stream')
         !read (10) maxLevelCell
         !close(10)
         !!print *, 'tempData',tempData
         !!print *, 'indexToCellID',indexToCellID

         !filename = 'mpaso.maxLevelCell.'//Hem//'.p'//charMPIRank//'.I1.dat'
         !open(10, file=subdir//filename, status='old', access='stream')
         !read (10) maxLevelCell
         !close(10)
         !!print *, 'maxLevelCell',maxLevelCell

         !temperature = -99
         !filename = 'mpaso.temperature.'//xtime(1:10)//'.'//Hem//'.p'//charMPIRank//'.I2.dat'
         !open(10, file=subdir//filename, status='old', access='stream')
         !do iCell = 1, nCells
         !   kMax = maxLevelCell(iCell)
print *, !iCell, kMax
         !   read (10) temperature(1:kmax,iCell)
         !enddo
         !close(10)

end program read_partition
