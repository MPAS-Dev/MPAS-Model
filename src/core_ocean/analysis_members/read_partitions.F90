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
   integer, parameter :: nCells = 202
   integer, parameter :: nVertLevels = 16
   integer(KIND=2), dimension(nCells) :: indexToCellID
   integer(KIND=1), dimension(nCells) :: maxLevelCell
   integer(KIND=2), dimension(100) :: intCol
   real(KIND=8), dimension(nVertLevels, nCells) :: temperature
   integer :: iCell, kMax

         filename = 'mpaso.indexToCellID.'//Hem//'.p'//charMPIRank//'.I2.dat'
         open(10, file=subdir//filename, status='old', access='stream')
         read (10) indexToCellID
         close(10)
         print *, 'indexToCellID',indexToCellID

         filename = 'mpaso.maxLevelCell.'//Hem//'.p'//charMPIRank//'.I1.dat'
         open(10, file=subdir//filename, status='old', access='stream')
         read (10) maxLevelCell
         close(10)
         print *, 'maxLevelCell',maxLevelCell

         temperature = -99
         filename = 'mpaso.temperature.'//xtime(1:10)//'.'//Hem//'.p'//charMPIRank//'.I2.dat'
         open(10, file=subdir//filename, status='old', access='stream')
         do iCell = 1, nCells
            kMax = maxLevelCell(iCell)
print *, iCell, kMax
            read (10) intCol(1:kMax)
            temperature(1:kmax,iCell) = minT + intCol(1:kMax)*(maxT - minT)/65535.0_RKIND
         enddo
         close(10)

end program read_partition
