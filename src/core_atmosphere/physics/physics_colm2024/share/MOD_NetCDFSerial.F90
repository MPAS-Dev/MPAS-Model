#include <define.h>

MODULE MOD_NetCDFSerial

!----------------------------------------------------------------------------------
! !DESCRIPTION:
!
!    High-level Subroutines to read and write variables in files with netCDF format.
!
!    CoLM read and write netCDF files mainly in three ways:
!    1. Serial: read and write data by a single process;
!    2. Vector: read/write data associated with CoLM pixelsets
!               Notice: each file CONTAINS vector data in one block.
!    3. Block : read blocked data by IO
!               Notice: input file is a single file.
!
!    This MODULE CONTAINS subroutines of "1. Serial".
!
!  Created by Shupeng Zhang, May 2023
!----------------------------------------------------------------------------------

   USE netcdf
   USE MOD_Precision
   IMPLICIT NONE

   ! PUBLIC subroutines

   PUBLIC :: ncio_create_file
   PUBLIC :: check_ncfile_exist

   INTERFACE ncio_put_attr
      MODULE procedure ncio_put_attr_str
      MODULE procedure ncio_put_attr_real8
   END INTERFACE ncio_put_attr

   INTERFACE ncio_get_attr
      MODULE procedure ncio_get_attr_str
      MODULE procedure ncio_get_attr_real8
   END INTERFACE ncio_get_attr

   PUBLIC :: ncio_var_exist
   PUBLIC :: ncio_var_exist_bcast_serial
   PUBLIC :: ncio_inquire_varsize
   PUBLIC :: ncio_inquire_length

   INTERFACE ncio_read_serial
      MODULE procedure ncio_read_serial_int32_0d
      MODULE procedure ncio_read_serial_real8_0d
      MODULE procedure ncio_read_serial_int8_1d
      MODULE procedure ncio_read_serial_int32_1d
      MODULE procedure ncio_read_serial_int64_1d
      MODULE procedure ncio_read_serial_real8_1d
      MODULE procedure ncio_read_serial_int8_2d
      MODULE procedure ncio_read_serial_int16_2d
      MODULE procedure ncio_read_serial_int32_2d
      MODULE procedure ncio_read_serial_real4_2d
      MODULE procedure ncio_read_serial_real8_2d
      MODULE procedure ncio_read_serial_int32_3d
      MODULE procedure ncio_read_serial_real8_3d
      MODULE procedure ncio_read_serial_real8_4d
      MODULE procedure ncio_read_serial_real8_5d
   END INTERFACE ncio_read_serial

   INTERFACE ncio_read_bcast_serial
      MODULE procedure ncio_read_bcast_serial_int32_0d
      MODULE procedure ncio_read_bcast_serial_real8_0d
      MODULE procedure ncio_read_bcast_serial_int32_1d
      MODULE procedure ncio_read_bcast_serial_int32_2d
      MODULE procedure ncio_read_bcast_serial_real8_1d
      MODULE procedure ncio_read_bcast_serial_real8_2d
      MODULE procedure ncio_read_bcast_serial_real8_3d
      MODULE procedure ncio_read_bcast_serial_real8_4d
      MODULE procedure ncio_read_bcast_serial_real8_5d
      MODULE procedure ncio_read_bcast_serial_logical_1d
   END INTERFACE ncio_read_bcast_serial

   INTERFACE ncio_read_part_serial
      MODULE procedure ncio_read_part_serial_int32_1d
      MODULE procedure ncio_read_part_serial_int32_2d
      MODULE procedure ncio_read_part_serial_real8_2d
   END INTERFACE ncio_read_part_serial

   INTERFACE ncio_read_indexed_serial
      MODULE procedure ncio_read_indexed_serial_int32_1d
      MODULE procedure ncio_read_indexed_serial_real8_1d
      MODULE procedure ncio_read_indexed_serial_int32_2d
      MODULE procedure ncio_read_indexed_serial_real8_2d
   END INTERFACE ncio_read_indexed_serial

   PUBLIC :: ncio_read_indexed_serial

   INTERFACE ncio_read_period_serial
      MODULE procedure ncio_read_period_serial_real8_2d
   END INTERFACE ncio_read_period_serial


   INTERFACE ncio_define_dimension
      MODULE procedure ncio_define_dimension_int32
      MODULE procedure ncio_define_dimension_int64
   END INTERFACE ncio_define_dimension

   INTERFACE ncio_write_serial
      MODULE procedure ncio_write_serial_int32_0d
      MODULE procedure ncio_write_serial_real8_0d
      MODULE procedure ncio_write_serial_int8_1d
      MODULE procedure ncio_write_serial_int32_1d
      MODULE procedure ncio_write_serial_int64_1d
      MODULE procedure ncio_write_serial_real8_1d
      MODULE procedure ncio_write_serial_logical_1d
      MODULE procedure ncio_write_serial_int8_2d
      MODULE procedure ncio_write_serial_int16_2d
      MODULE procedure ncio_write_serial_int32_2d
      MODULE procedure ncio_write_serial_int64_2d
      MODULE procedure ncio_write_serial_real4_2d
      MODULE procedure ncio_write_serial_real8_2d
      MODULE procedure ncio_write_serial_int32_3d
      MODULE procedure ncio_write_serial_real8_3d
      MODULE procedure ncio_write_serial_real8_4d
      MODULE procedure ncio_write_serial_real8_5d
   END INTERFACE ncio_write_serial

   PUBLIC :: ncio_write_serial
   PUBLIC :: ncio_write_time
   PUBLIC :: ncio_write_lastdim

   INTERFACE ncio_write_serial_time
      MODULE procedure ncio_write_serial_real8_0d_time
      MODULE procedure ncio_write_serial_real8_1d_time
      MODULE procedure ncio_write_serial_real8_2d_time
      MODULE procedure ncio_write_serial_real8_3d_time
      MODULE procedure ncio_write_serial_real8_4d_time
   END INTERFACE ncio_write_serial_time

   PUBLIC :: get_time_now

   PUBLIC :: ncio_write_colm_dimension

CONTAINS

   ! ----
   SUBROUTINE nccheck (status, trace)

   USE MOD_MPAS_MPI
   IMPLICIT NONE

   integer, intent(in) :: status
   character(len=*), intent(in), optional :: trace

      IF (status /= NF90_NOERR) THEN
         IF (present(trace)) THEN
            write(*,'(A)') 'Netcdf error: ' //trim(nf90_strerror(status))// ' ' //trim(trace)
         ELSE
            write(*,'(A)') 'Netcdf error: ' //trim(nf90_strerror(status))
         ENDIF

         CALL CoLM_stop ()
      ENDIF

   END SUBROUTINE nccheck

   ! ----
   SUBROUTINE check_ncfile_exist (filename)

   USE MOD_MPAS_MPI
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   ! Local Variables
   logical :: fexists

      inquire (file=trim(filename), exist=fexists)
      IF (.not. fexists) THEN
         write(*,*) trim(filename), ' does not exist.'
         CALL CoLM_stop ()
      ENDIF

   END SUBROUTINE check_ncfile_exist

   ! ----
   character(len=27) FUNCTION get_time_now ()

   IMPLICIT NONE
   character(len=8)  :: date
   character(len=10) :: time
   character(len=5)  :: zone

      CALL date_and_time(date, time, zone)
      get_time_now = date(1:8)//'-'//time(1:2)//':'//time(3:4)//':'//time(5:6) &
                     //' UTC'//zone(1:3)//':'//zone(4:5)

   END FUNCTION get_time_now

   ! ----
   SUBROUTINE ncio_create_file (filename)

   USE netcdf
   IMPLICIT NONE

   character(len=*), intent(in)  :: filename

   ! Local Variables
   integer :: ncid

      CALL nccheck( nf90_create(trim(filename), ior(NF90_CLOBBER,NF90_NETCDF4), ncid) )

      CALL nccheck( nf90_put_att(ncid, NF90_GLOBAL, 'create_time', get_time_now()))
      CALL nccheck (nf90_enddef (ncid))

      CALL nccheck( nf90_close(ncid) )

   END SUBROUTINE ncio_create_file

   ! ----
   SUBROUTINE ncio_put_attr_str (filename, varname, attrname, attrval)

   USE netcdf
   IMPLICIT NONE

   character(len=*), intent(in)  :: filename, varname, attrname, attrval

   ! Local Variables
   integer :: ncid, varid

      CALL nccheck( nf90_open (trim(filename), NF90_WRITE, ncid) )
      CALL nccheck (nf90_inq_varid (ncid, trim(varname), varid), trim(varname))
      CALL nccheck (nf90_redef (ncid))
      CALL nccheck (nf90_put_att (ncid, varid, trim(attrname), trim(attrval)))
      CALL nccheck (nf90_enddef (ncid))
      CALL nccheck( nf90_close (ncid))

   END SUBROUTINE ncio_put_attr_str

   ! ----
   SUBROUTINE ncio_get_attr_str (filename, varname, attrname, attrval)

   USE netcdf
   IMPLICIT NONE

   character(len=*), intent(in)  :: filename, varname, attrname
   character(len=*), intent(out) :: attrval

   ! Local Variables
   integer :: ncid, varid

      CALL nccheck( nf90_open (trim(filename), NF90_NOWRITE, ncid) )
      CALL nccheck (nf90_inq_varid (ncid, trim(varname), varid), trim(varname))
      CALL nccheck (nf90_get_att   (ncid, varid, trim(attrname), attrval))
      CALL nccheck( nf90_close (ncid))

   END SUBROUTINE ncio_get_attr_str

   ! ----
   SUBROUTINE ncio_get_attr_real8 (filename, varname, attrname, attrval)

   USE netcdf
   IMPLICIT NONE

   character(len=*), intent(in)  :: filename, varname, attrname
   real(r8), intent(out) :: attrval

   ! Local Variables
   integer :: ncid, varid

      CALL nccheck( nf90_open (trim(filename), NF90_NOWRITE, ncid) )
      CALL nccheck (nf90_inq_varid (ncid, trim(varname), varid), trim(varname))
      CALL nccheck (nf90_get_att (ncid, varid, trim(attrname), attrval))
      CALL nccheck (nf90_close (ncid))

   END SUBROUTINE ncio_get_attr_real8

   ! ----
   SUBROUTINE ncio_put_attr_real8 (filename, varname, attrname, attrval)

   USE netcdf
   IMPLICIT NONE

   character(len=*), intent(in)  :: filename, varname, attrname
   real(r8),         intent(in)  :: attrval

   ! Local Variables
   integer :: ncid, varid

      CALL nccheck( nf90_open (trim(filename), NF90_WRITE, ncid) )
      CALL nccheck (nf90_inq_varid (ncid, trim(varname), varid), trim(varname))
      CALL nccheck (nf90_redef (ncid))
      CALL nccheck (nf90_put_att (ncid, varid, trim(attrname), attrval))
      CALL nccheck (nf90_enddef (ncid))
      CALL nccheck( nf90_close (ncid))

   END SUBROUTINE ncio_put_attr_real8


   !---------------------------------------------------------
   SUBROUTINE ncio_inquire_varsize (filename, dataname, varsize)

   USE netcdf
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   integer, allocatable, intent(out) :: varsize(:)

   ! Local variables
   integer :: ncid, varid, ndims, idm
   integer, allocatable :: dimids(:)

      CALL nccheck( nf90_open(trim(filename), NF90_NOWRITE, ncid) )
      CALL nccheck( nf90_inq_varid(ncid, trim(dataname), varid), trim(dataname) )

      CALL nccheck( nf90_inquire_variable(ncid, varid, ndims = ndims) )
      allocate (dimids(ndims))
      CALL nccheck( nf90_inquire_variable(ncid, varid, dimids = dimids) )

      allocate (varsize(ndims))
      DO idm = 1, ndims
         CALL nccheck( nf90_inquire_dimension(ncid, dimids(idm), len = varsize(idm)) )
      ENDDO

      CALL nccheck( nf90_close(ncid) )
      deallocate (dimids)

   END SUBROUTINE ncio_inquire_varsize

   !---------------------------------------------------------
   logical FUNCTION ncio_var_exist (filename, dataname, readflag)

   USE netcdf
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   logical, optional,intent(in) :: readflag

   ! Local variables
   integer :: ncid, varid, status
   logical :: readflag_

      status = nf90_open(trim(filename), NF90_NOWRITE, ncid)
      IF (status == nf90_noerr) THEN
         status = nf90_inq_varid(ncid, trim(dataname), varid)
         ncio_var_exist = (status == nf90_noerr)
         CALL nccheck( nf90_close(ncid) )
      ELSE
         ncio_var_exist = .false.
      ENDIF

      IF (present(readflag)) THEN
         readflag_ = readflag
      ELSE
         readflag_ = .true.
      ENDIF

      IF ((.not. ncio_var_exist) .and. trim(filename) /= 'null' .and. readflag_) THEN
         write(*,*) 'Warning: ', trim(dataname), ' not found in ', trim(filename)
      ENDIF

   END FUNCTION ncio_var_exist

   !---------------------------------------------------------
   logical FUNCTION ncio_var_exist_bcast_serial (filename, dataname, readflag)

#ifdef MPAS_MPI
   USE mpi, only: MPI_LOGICAL, mpi_bcast
   USE MOD_MPAS_MPI, only: mpas_is_root, mpas_root, mpas_comm, mpas_mpi_ierr, mpas_mpi_check
#endif
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   logical, optional, intent(in) :: readflag

   logical :: readflag_

      readflag_ = .true.
      IF (present(readflag)) readflag_ = readflag

#ifdef MPAS_MPI
      ! This is collective: root supplies one metadata decision for all ranks.
      ! It does not make subsequent rank-local reads work on a non-shared file system.
      ncio_var_exist_bcast_serial = .false.
      IF (mpas_is_root) THEN
         ncio_var_exist_bcast_serial = ncio_var_exist(filename, dataname, readflag_)
      ENDIF
      CALL mpi_bcast(ncio_var_exist_bcast_serial, 1, MPI_LOGICAL, mpas_root, mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('serial NetCDF variable-existence broadcast')
#else
      ncio_var_exist_bcast_serial = ncio_var_exist(filename, dataname, readflag_)
#endif

   END FUNCTION ncio_var_exist_bcast_serial

   !---------------------------------------------------------
   SUBROUTINE ncio_inquire_length (filename, dataname, length)

   USE netcdf
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   integer, intent(out) :: length

   ! Local variables
   integer :: ncid, varid, ndims
   integer, allocatable :: dimids(:)

      CALL nccheck( nf90_open(trim(filename), NF90_NOWRITE, ncid) )
      CALL nccheck( nf90_inq_varid(ncid, trim(dataname), varid), trim(dataname) )

      CALL nccheck( nf90_inquire_variable(ncid, varid, ndims = ndims) )
      allocate (dimids(ndims))
      CALL nccheck( nf90_inquire_variable(ncid, varid, dimids = dimids) )
      CALL nccheck( nf90_inquire_dimension(ncid, dimids(ndims), len = length) )

      CALL nccheck( nf90_close(ncid) )
      deallocate (dimids)

   END SUBROUTINE ncio_inquire_length
   !---------------------------------------------------------
   SUBROUTINE ncio_read_serial_int32_0d (filename, dataname, rdata)

   USE netcdf
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   integer, intent(out) :: rdata

   ! Local variables
   integer :: ncid, varid

      CALL check_ncfile_exist (filename)

      CALL nccheck( nf90_open(trim(filename), NF90_NOWRITE, ncid) )
      CALL nccheck( nf90_inq_varid(ncid, trim(dataname), varid), trim(dataname) )
      CALL nccheck( nf90_get_var(ncid, varid, rdata) )
      CALL nccheck( nf90_close(ncid) )

   END SUBROUTINE ncio_read_serial_int32_0d

   !---------------------------------------------------------
   SUBROUTINE ncio_read_serial_real8_0d (filename, dataname, rdata)

   USE netcdf
   USE MOD_Precision
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   real(r8), intent(out) :: rdata

   ! Local variables
   integer :: ncid, varid

      CALL check_ncfile_exist (filename)

      CALL nccheck( nf90_open(trim(filename), NF90_NOWRITE, ncid) )
      CALL nccheck( nf90_inq_varid(ncid, trim(dataname), varid), trim(dataname) )
      CALL nccheck( nf90_get_var(ncid, varid, rdata) )
      CALL nccheck( nf90_close(ncid) )

   END SUBROUTINE ncio_read_serial_real8_0d

   !---------------------------------------------------------
   SUBROUTINE ncio_read_serial_int8_1d (filename, dataname, rdata)

   USE netcdf
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   integer(1), allocatable, intent(out) :: rdata (:)

   ! Local variables
   integer :: ncid, varid
   integer, allocatable :: varsize(:)

      CALL check_ncfile_exist (filename)

      CALL ncio_inquire_varsize(filename, dataname, varsize)
      allocate (rdata (varsize(1)) )

      CALL nccheck( nf90_open(trim(filename), NF90_NOWRITE, ncid) )
      CALL nccheck( nf90_inq_varid(ncid, trim(dataname), varid), trim(dataname) )
      CALL nccheck( nf90_get_var(ncid, varid, rdata) )
      CALL nccheck( nf90_close(ncid) )

      deallocate (varsize)

   END SUBROUTINE ncio_read_serial_int8_1d

   !---------------------------------------------------------
   SUBROUTINE ncio_read_serial_int32_1d (filename, dataname, rdata)

   USE netcdf
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   integer, allocatable, intent(out) :: rdata (:)

   ! Local variables
   integer :: ncid, varid
   integer, allocatable :: varsize(:)

      CALL check_ncfile_exist (filename)

      CALL ncio_inquire_varsize(filename, dataname, varsize)
      allocate (rdata (varsize(1)) )

      CALL nccheck( nf90_open(trim(filename), NF90_NOWRITE, ncid) )
      CALL nccheck( nf90_inq_varid(ncid, trim(dataname), varid), trim(dataname) )
      CALL nccheck( nf90_get_var(ncid, varid, rdata) )
      CALL nccheck( nf90_close(ncid) )

      deallocate (varsize)

   END SUBROUTINE ncio_read_serial_int32_1d

   !---------------------------------------------------------
   SUBROUTINE ncio_read_serial_int64_1d (filename, dataname, rdata)

   USE netcdf
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   integer*8, allocatable, intent(out) :: rdata (:)

   ! Local variables
   integer :: ncid, varid
   integer, allocatable :: varsize(:)

      CALL check_ncfile_exist (filename)

      CALL ncio_inquire_varsize(filename, dataname, varsize)
      allocate (rdata (varsize(1)) )

      CALL nccheck( nf90_open(trim(filename), NF90_NOWRITE, ncid) )
      CALL nccheck( nf90_inq_varid(ncid, trim(dataname), varid), trim(dataname) )
      CALL nccheck( nf90_get_var(ncid, varid, rdata) )
      CALL nccheck( nf90_close(ncid) )

      deallocate (varsize)

   END SUBROUTINE ncio_read_serial_int64_1d


   !---------------------------------------------------------
   SUBROUTINE ncio_read_serial_real8_1d (filename, dataname, rdata)

   USE netcdf
   USE MOD_Precision
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   real(r8), allocatable, intent(out) :: rdata (:)

   ! Local variables
   integer :: ncid, varid
   integer, allocatable :: varsize(:)

      CALL check_ncfile_exist (filename)

      CALL ncio_inquire_varsize(filename, dataname, varsize)
      allocate (rdata (varsize(1)) )

      CALL nccheck( nf90_open(trim(filename), NF90_NOWRITE, ncid) )
      CALL nccheck( nf90_inq_varid(ncid, trim(dataname), varid), trim(dataname) )
      CALL nccheck( nf90_get_var(ncid, varid, rdata) )
      CALL nccheck( nf90_close(ncid) )

      deallocate (varsize)

   END SUBROUTINE ncio_read_serial_real8_1d

   !---------------------------------------------------------
   SUBROUTINE ncio_read_serial_int8_2d (filename, dataname, rdata)

   USE netcdf
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   integer(1), allocatable, intent(out) :: rdata (:,:)

   ! Local variables
   integer :: ncid, varid
   integer, allocatable :: varsize(:)

      CALL check_ncfile_exist (filename)

      CALL ncio_inquire_varsize(filename, dataname, varsize)
      allocate (rdata (varsize(1), varsize(2)) )

      CALL nccheck( nf90_open(trim(filename), NF90_NOWRITE, ncid) )
      CALL nccheck( nf90_inq_varid(ncid, trim(dataname), varid), trim(dataname) )
      CALL nccheck( nf90_get_var(ncid, varid, rdata) )
      CALL nccheck( nf90_close(ncid) )

      deallocate (varsize)

   END SUBROUTINE ncio_read_serial_int8_2d

   !---------------------------------------------------------
   SUBROUTINE ncio_read_serial_int16_2d (filename, dataname, rdata)

   USE netcdf
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   integer(2), allocatable, intent(out) :: rdata (:,:)

   ! Local variables
   integer :: ncid, varid
   integer, allocatable :: varsize(:)

      CALL check_ncfile_exist (filename)

      CALL ncio_inquire_varsize(filename, dataname, varsize)
      allocate (rdata (varsize(1), varsize(2)) )

      CALL nccheck( nf90_open(trim(filename), NF90_NOWRITE, ncid) )
      CALL nccheck( nf90_inq_varid(ncid, trim(dataname), varid), trim(dataname) )
      CALL nccheck( nf90_get_var(ncid, varid, rdata) )
      CALL nccheck( nf90_close(ncid) )

      deallocate (varsize)

   END SUBROUTINE ncio_read_serial_int16_2d

   !---------------------------------------------------------
   SUBROUTINE ncio_read_serial_int32_2d (filename, dataname, rdata)

   USE netcdf
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   integer, allocatable, intent(out) :: rdata (:,:)

   ! Local variables
   integer :: ncid, varid
   integer, allocatable :: varsize(:)
   integer :: dsp, nread

      CALL check_ncfile_exist (filename)

      CALL ncio_inquire_varsize(filename, dataname, varsize)
      allocate (rdata (varsize(1), varsize(2)) )

      CALL nccheck( nf90_open(trim(filename), NF90_NOWRITE, ncid) )
      CALL nccheck( nf90_inq_varid(ncid, trim(dataname), varid), trim(dataname) )

      IF ((varsize(1) > 1000) .and. (varsize(2) > 100000)) THEN
         dsp = 0
         DO WHILE (dsp < varsize(2))
            nread = min(100000,varsize(2)-dsp)
            CALL nccheck (nf90_get_var(ncid, varid, &
               rdata(1:varsize(1),dsp+1:dsp+nread), (/1,dsp+1/), (/varsize(1),nread/)))
            dsp = dsp + nread
         ENDDO
      ELSE
         CALL nccheck( nf90_get_var(ncid, varid, rdata) )
      ENDIF

      CALL nccheck( nf90_close(ncid) )

      deallocate (varsize)

   END SUBROUTINE ncio_read_serial_int32_2d

   !---------------------------------------------------------
   SUBROUTINE ncio_read_serial_real4_2d (filename, dataname, rdata)

   USE netcdf
   USE MOD_Precision
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   real(4), allocatable, intent(out) :: rdata (:,:)

   ! Local variables
   integer :: ncid, varid
   integer, allocatable :: varsize(:)

      CALL check_ncfile_exist (filename)

      CALL ncio_inquire_varsize(filename, dataname, varsize)
      allocate (rdata (varsize(1), varsize(2)) )

      CALL nccheck( nf90_open(trim(filename), NF90_NOWRITE, ncid) )
      CALL nccheck( nf90_inq_varid(ncid, trim(dataname), varid), trim(dataname) )
      CALL nccheck( nf90_get_var(ncid, varid, rdata) )
      CALL nccheck( nf90_close(ncid) )

      deallocate (varsize)

   END SUBROUTINE ncio_read_serial_real4_2d

   !---------------------------------------------------------
   SUBROUTINE ncio_read_serial_real8_2d (filename, dataname, rdata)

   USE netcdf
   USE MOD_Precision
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   real(r8), allocatable, intent(out) :: rdata (:,:)

   ! Local variables
   integer :: ncid, varid
   integer, allocatable :: varsize(:)
   integer :: dsp, nread

      CALL check_ncfile_exist (filename)

      CALL ncio_inquire_varsize(filename, dataname, varsize)
      allocate (rdata (varsize(1), varsize(2)) )

      CALL nccheck( nf90_open(trim(filename), NF90_NOWRITE, ncid) )
      CALL nccheck( nf90_inq_varid(ncid, trim(dataname), varid), trim(dataname) )

      IF ((varsize(1) > 1000) .and. (varsize(2) > 100000)) THEN
         dsp = 0
         DO WHILE (dsp < varsize(2))
            nread = min(100000,varsize(2)-dsp)
            CALL nccheck (nf90_get_var(ncid, varid, &
               rdata(1:varsize(1),dsp+1:dsp+nread), (/1,dsp+1/), (/varsize(1),nread/)))
            dsp = dsp + nread
         ENDDO
      ELSE
         CALL nccheck( nf90_get_var(ncid, varid, rdata) )
      ENDIF

      CALL nccheck( nf90_close(ncid) )

      deallocate (varsize)

   END SUBROUTINE ncio_read_serial_real8_2d

   !---------------------------------------------------------
   SUBROUTINE ncio_read_serial_int32_3d (filename, dataname, rdata)

   USE netcdf
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   integer, allocatable, intent(out) :: rdata (:,:,:)

   ! Local variables
   integer :: ncid, varid
   integer, allocatable :: varsize(:)

      CALL check_ncfile_exist (filename)

      CALL ncio_inquire_varsize(filename, dataname, varsize)
      allocate (rdata (varsize(1), varsize(2), varsize(3)) )

      CALL nccheck( nf90_open(trim(filename), NF90_NOWRITE, ncid) )
      CALL nccheck( nf90_inq_varid(ncid, trim(dataname), varid), trim(dataname) )
      CALL nccheck( nf90_get_var(ncid, varid, rdata) )
      CALL nccheck( nf90_close(ncid) )

      deallocate (varsize)

   END SUBROUTINE ncio_read_serial_int32_3d

   !---------------------------------------------------------
   SUBROUTINE ncio_read_serial_real8_3d (filename, dataname, rdata)

   USE netcdf
   USE MOD_Precision
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   real(r8), allocatable, intent(out) :: rdata (:,:,:)

   ! Local variables
   integer :: ncid, varid
   integer, allocatable :: varsize(:)

      CALL check_ncfile_exist (filename)

      CALL ncio_inquire_varsize(filename, dataname, varsize)
      allocate (rdata (varsize(1), varsize(2), varsize(3)) )

      CALL nccheck( nf90_open(trim(filename), NF90_NOWRITE, ncid) )
      CALL nccheck( nf90_inq_varid(ncid, trim(dataname), varid), trim(dataname) )
      CALL nccheck( nf90_get_var(ncid, varid, rdata) )
      CALL nccheck( nf90_close(ncid) )

      deallocate (varsize)

   END SUBROUTINE ncio_read_serial_real8_3d

   !---------------------------------------------------------
   SUBROUTINE ncio_read_serial_real8_4d (filename, dataname, rdata)

   USE netcdf
   USE MOD_Precision
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   real(r8), allocatable, intent(out) :: rdata (:,:,:,:)

   ! Local variables
   integer :: ncid, varid
   integer, allocatable :: varsize(:)

      CALL check_ncfile_exist (filename)

      CALL ncio_inquire_varsize(filename, dataname, varsize)
      allocate (rdata (varsize(1), varsize(2), varsize(3), varsize(4)) )

      CALL nccheck( nf90_open(trim(filename), NF90_NOWRITE, ncid) )
      CALL nccheck( nf90_inq_varid(ncid, trim(dataname), varid), trim(dataname) )
      CALL nccheck( nf90_get_var(ncid, varid, rdata) )
      CALL nccheck( nf90_close(ncid) )

      deallocate (varsize)

   END SUBROUTINE ncio_read_serial_real8_4d

   !---------------------------------------------------------
   SUBROUTINE ncio_read_serial_real8_5d (filename, dataname, rdata)

   USE netcdf
   USE MOD_Precision
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   real(r8), allocatable, intent(out) :: rdata (:,:,:,:,:)

   ! Local variables
   integer :: ncid, varid
   integer, allocatable :: varsize(:)

      CALL check_ncfile_exist (filename)

      CALL ncio_inquire_varsize(filename, dataname, varsize)
      allocate (rdata (varsize(1), varsize(2), varsize(3), varsize(4), varsize(5)) )

      CALL nccheck( nf90_open(trim(filename), NF90_NOWRITE, ncid) )
      CALL nccheck( nf90_inq_varid(ncid, trim(dataname), varid), trim(dataname) )
      CALL nccheck( nf90_get_var(ncid, varid, rdata) )
      CALL nccheck( nf90_close(ncid) )

      deallocate (varsize)

   END SUBROUTINE ncio_read_serial_real8_5d

   !---------------------------------------------------------
   SUBROUTINE ncio_read_bcast_serial_int32_0d (filename, dataname, rdata)

   USE netcdf
   USE MOD_MPAS_MPI
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   integer, intent(out) :: rdata

#ifdef MPAS_MPI
      IF (mpas_is_root) THEN
         CALL ncio_read_serial_int32_0d (filename, dataname, rdata)
      ENDIF
      CALL mpi_bcast (rdata, 1, MPI_INTEGER, mpas_root, mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('serial NetCDF scalar integer broadcast')
#else
      CALL ncio_read_serial_int32_0d (filename, dataname, rdata)
#endif

   END SUBROUTINE ncio_read_bcast_serial_int32_0d

   !---------------------------------------------------------
   SUBROUTINE ncio_read_bcast_serial_real8_0d (filename, dataname, rdata)

   USE MOD_MPAS_MPI
   USE MOD_Precision
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   real(r8), intent(out) :: rdata

#ifdef MPAS_MPI
      IF (mpas_is_root) THEN
         CALL ncio_read_serial_real8_0d (filename, dataname, rdata)
      ENDIF
      CALL mpi_bcast (rdata, 1, MPI_REAL8, mpas_root, mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('serial NetCDF scalar real broadcast')
#else
      CALL ncio_read_serial_real8_0d (filename, dataname, rdata)
#endif

   END SUBROUTINE ncio_read_bcast_serial_real8_0d

   !---------------------------------------------------------
   SUBROUTINE ncio_read_bcast_serial_int32_1d (filename, dataname, rdata)

   USE MOD_MPAS_MPI
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   integer, allocatable, intent(out) :: rdata (:)
   integer :: vlen

#ifdef MPAS_MPI
      IF (mpas_is_root) THEN
         CALL ncio_read_serial_int32_1d(filename, dataname, rdata)
         vlen = size(rdata)
      ENDIF
      CALL mpi_bcast (vlen, 1, MPI_INTEGER, mpas_root, mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('serial NetCDF integer vector length broadcast')
      IF (.not. mpas_is_root)  allocate (rdata (vlen))
      CALL mpi_bcast (rdata, vlen, MPI_INTEGER, mpas_root, mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('serial NetCDF integer vector broadcast')
#else
      CALL ncio_read_serial_int32_1d(filename, dataname, rdata)
#endif

   END SUBROUTINE ncio_read_bcast_serial_int32_1d

   !---------------------------------------------------------
   SUBROUTINE ncio_read_bcast_serial_int32_2d (filename, dataname, rdata)

   USE MOD_MPAS_MPI
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   integer, allocatable, intent(out) :: rdata (:,:)
   integer :: vsize(2)

#ifdef MPAS_MPI
      IF (mpas_is_root) THEN
         CALL ncio_read_serial_int32_2d(filename, dataname, rdata)
         vsize = shape(rdata)
      ENDIF
      CALL mpi_bcast (vsize, 2, MPI_INTEGER, mpas_root, mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('serial NetCDF integer array shape broadcast')
      IF (.not. mpas_is_root)  allocate (rdata (vsize(1), vsize(2)))
      CALL mpi_bcast (rdata, vsize(1)*vsize(2), MPI_INTEGER, mpas_root, mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('serial NetCDF integer array broadcast')
#else
      CALL ncio_read_serial_int32_2d(filename, dataname, rdata)
#endif

   END SUBROUTINE ncio_read_bcast_serial_int32_2d

   !---------------------------------------------------------
   SUBROUTINE ncio_read_bcast_serial_real8_1d (filename, dataname, rdata)

   USE netcdf
   USE MOD_MPAS_MPI
   USE MOD_Precision
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   real(r8), allocatable, intent(out) :: rdata (:)
   integer :: vlen

#ifdef MPAS_MPI
      IF (mpas_is_root) THEN
         CALL ncio_read_serial_real8_1d(filename, dataname, rdata)
         vlen = size(rdata)
      ENDIF
      CALL mpi_bcast (vlen, 1, MPI_INTEGER, mpas_root, mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('serial NetCDF real vector length broadcast')
      IF (.not. mpas_is_root)  allocate (rdata (vlen))
      CALL mpi_bcast (rdata, vlen, MPI_REAL8, mpas_root, mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('serial NetCDF real vector broadcast')
#else
      CALL ncio_read_serial_real8_1d(filename, dataname, rdata)
#endif

   END SUBROUTINE ncio_read_bcast_serial_real8_1d

   !---------------------------------------------------------
   SUBROUTINE ncio_read_bcast_serial_real8_2d (filename, dataname, rdata)

   USE netcdf
   USE MOD_MPAS_MPI
   USE MOD_Precision
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   real(r8), allocatable, intent(out) :: rdata (:,:)
   integer :: vsize(2)

#ifdef MPAS_MPI
      IF (mpas_is_root) THEN
         CALL ncio_read_serial_real8_2d(filename, dataname, rdata)
         vsize = shape(rdata)
      ENDIF
      CALL mpi_bcast (vsize, 2, MPI_INTEGER, mpas_root, mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('serial NetCDF rank-2 real shape broadcast')
      IF (.not. mpas_is_root)  allocate (rdata (vsize(1),vsize(2)))
      CALL mpi_bcast (rdata, vsize(1)*vsize(2), MPI_REAL8, mpas_root, mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('serial NetCDF rank-2 real broadcast')
#else
      CALL ncio_read_serial_real8_2d(filename, dataname, rdata)
#endif

   END SUBROUTINE ncio_read_bcast_serial_real8_2d

   !---------------------------------------------------------
   SUBROUTINE ncio_read_bcast_serial_real8_3d (filename, dataname, rdata)

   USE netcdf
   USE MOD_MPAS_MPI
   USE MOD_Precision
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   real(r8), allocatable, intent(out) :: rdata (:,:,:)
   integer :: vsize(3)

#ifdef MPAS_MPI
      IF (mpas_is_root) THEN
         CALL ncio_read_serial_real8_3d(filename, dataname, rdata)
         vsize = shape(rdata)
      ENDIF
      CALL mpi_bcast (vsize, 3, MPI_INTEGER, mpas_root, mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('serial NetCDF rank-3 real shape broadcast')
      IF (.not. mpas_is_root)  allocate (rdata (vsize(1),vsize(2),vsize(3)))
      CALL mpi_bcast (rdata, vsize(1)*vsize(2)*vsize(3), MPI_REAL8, mpas_root, mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('serial NetCDF rank-3 real broadcast')
#else
      CALL ncio_read_serial_real8_3d(filename, dataname, rdata)
#endif

   END SUBROUTINE ncio_read_bcast_serial_real8_3d

   !---------------------------------------------------------
   SUBROUTINE ncio_read_bcast_serial_real8_4d (filename, dataname, rdata)

   USE netcdf
   USE MOD_MPAS_MPI
   USE MOD_Precision
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   real(r8), allocatable, intent(out) :: rdata (:,:,:,:)
   integer :: vsize(4)

#ifdef MPAS_MPI
      IF (mpas_is_root) THEN
         CALL ncio_read_serial_real8_4d(filename, dataname, rdata)
         vsize = shape(rdata)
      ENDIF
      CALL mpi_bcast (vsize, 4, MPI_INTEGER, mpas_root, mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('serial NetCDF rank-4 real shape broadcast')
      IF (.not. mpas_is_root)  allocate (rdata (vsize(1),vsize(2),vsize(3),vsize(4)))
      CALL mpi_bcast (rdata, vsize(1)*vsize(2)*vsize(3)*vsize(4), MPI_REAL8, mpas_root, mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('serial NetCDF rank-4 real broadcast')
#else
      CALL ncio_read_serial_real8_4d(filename, dataname, rdata)
#endif

   END SUBROUTINE ncio_read_bcast_serial_real8_4d

      !---------------------------------------------------------
   SUBROUTINE ncio_read_bcast_serial_real8_5d (filename, dataname, rdata)

   USE netcdf
   USE MOD_MPAS_MPI
   USE MOD_Precision
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   real(r8), allocatable, intent(out) :: rdata (:,:,:,:,:)
   integer :: vsize(5)

#ifdef MPAS_MPI
      IF (mpas_is_root) THEN
         CALL ncio_read_serial_real8_5d(filename, dataname, rdata)
         vsize = shape(rdata)
      ENDIF
      CALL mpi_bcast (vsize, 5, MPI_INTEGER, mpas_root, mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('serial NetCDF rank-5 real shape broadcast')
      IF (.not. mpas_is_root)  allocate (rdata (vsize(1),vsize(2),vsize(3),vsize(4),vsize(5)))
      CALL mpi_bcast (rdata, vsize(1)*vsize(2)*vsize(3)*vsize(4)*vsize(5), MPI_REAL8, mpas_root, mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('serial NetCDF rank-5 real broadcast')
#else
      CALL ncio_read_serial_real8_5d(filename, dataname, rdata)
#endif

   END SUBROUTINE ncio_read_bcast_serial_real8_5d

   ! -------------------------------
   SUBROUTINE ncio_read_bcast_serial_logical_1d (filename, dataname, rdata)

   USE MOD_MPAS_MPI
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   logical, allocatable, intent(out) :: rdata (:)
   integer :: vlen
   integer(1), allocatable :: rdata_byte(:)

#ifdef MPAS_MPI
      IF (mpas_is_root) THEN
         CALL ncio_read_serial_int8_1d(filename, dataname, rdata_byte)
         vlen = size(rdata_byte)

         allocate(rdata(vlen))
         rdata = (rdata_byte == 1)

         deallocate (rdata_byte)
      ENDIF
      CALL mpi_bcast (vlen, 1, MPI_INTEGER, mpas_root, mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('serial NetCDF logical vector length broadcast')
      IF (.not. mpas_is_root)  allocate (rdata (vlen))
      CALL mpi_bcast (rdata, vlen, MPI_LOGICAL, mpas_root, mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('serial NetCDF logical vector broadcast')
#else
      CALL ncio_read_serial_int8_1d(filename, dataname, rdata_byte)
      vlen = size(rdata_byte)

      allocate(rdata(vlen))
      rdata = (rdata_byte == 1)

      deallocate (rdata_byte)
#endif

   END SUBROUTINE ncio_read_bcast_serial_logical_1d

   !---------------------------------------------------------
   SUBROUTINE ncio_read_part_serial_int32_1d (filename, dataname, datastt, dataend, rdata)

   USE netcdf
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   integer,  intent(in) :: datastt, dataend
   integer, allocatable, intent(out) :: rdata (:)

   ! Local variables
   integer :: ncid, varid

      CALL check_ncfile_exist (filename)

      allocate (rdata (datastt:dataend) )

      CALL nccheck( nf90_open(trim(filename), NF90_NOWRITE, ncid) )
      CALL nccheck( nf90_inq_varid(ncid, trim(dataname), varid), trim(dataname) )
      CALL nccheck( nf90_get_var(ncid, varid, rdata, &
         (/datastt/), (/dataend-datastt+1/)) )
      CALL nccheck( nf90_close(ncid) )

   END SUBROUTINE ncio_read_part_serial_int32_1d

   !---------------------------------------------------------
   SUBROUTINE ncio_read_part_serial_int32_2d (filename, dataname, datastt, dataend, rdata)

   USE netcdf
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   integer, intent(in) :: datastt(2), dataend(2)
   integer, allocatable, intent(out) :: rdata (:,:)

   ! Local variables
   integer :: ncid, varid

      CALL check_ncfile_exist (filename)

      allocate (rdata (datastt(1):dataend(1), datastt(2):dataend(2)) )

      CALL nccheck( nf90_open(trim(filename), NF90_NOWRITE, ncid) )
      CALL nccheck( nf90_inq_varid(ncid, trim(dataname), varid), trim(dataname) )
      CALL nccheck( nf90_get_var(ncid, varid, rdata, &
         (/datastt(1),datastt(2)/), (/dataend(1)-datastt(1)+1, dataend(2)-datastt(2)+1/)) )
      CALL nccheck( nf90_close(ncid) )

   END SUBROUTINE ncio_read_part_serial_int32_2d

   !---------------------------------------------------------
   SUBROUTINE ncio_read_part_serial_real8_2d (filename, dataname, datastt, dataend, rdata)

   USE netcdf
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   integer,  intent(in) :: datastt(2), dataend(2)
   real(r8), allocatable, intent(out) :: rdata (:,:)

   ! Local variables
   integer :: ncid, varid

      CALL check_ncfile_exist (filename)

      allocate (rdata (datastt(1):dataend(1), datastt(2):dataend(2)) )

      CALL nccheck( nf90_open(trim(filename), NF90_NOWRITE, ncid) )
      CALL nccheck( nf90_inq_varid(ncid, trim(dataname), varid), trim(dataname) )
      CALL nccheck( nf90_get_var(ncid, varid, rdata, &
         (/datastt(1),datastt(2)/), (/dataend(1)-datastt(1)+1, dataend(2)-datastt(2)+1/)) )
      CALL nccheck( nf90_close(ncid) )

   END SUBROUTINE ncio_read_part_serial_real8_2d

   !---------------------------------------------------------
   SUBROUTINE ncio_read_indexed_serial_int32_1d (filename, dataname, index, rdata)

   USE netcdf
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   integer, intent(in) :: index(:)
   integer, allocatable, intent(out) :: rdata(:)

   integer :: ncid, varid
   integer :: nidx, istart, iend, nread

      nidx = size(index)
      allocate (rdata(nidx))
      IF (nidx == 0) RETURN
      IF (any(index < 1)) CALL nccheck(NF90_EINVAL, trim(dataname)//' indexed read')

      CALL check_ncfile_exist (filename)
      CALL nccheck( nf90_open(trim(filename), NF90_NOWRITE, ncid) )
      CALL nccheck( nf90_inq_varid(ncid, trim(dataname), varid), trim(dataname) )

      istart = 1
      DO WHILE (istart <= nidx)
         iend = istart
         DO WHILE (iend < nidx)
            IF (index(iend+1) /= index(iend) + 1) EXIT
            iend = iend + 1
         ENDDO
         nread = iend - istart + 1
         CALL nccheck( nf90_get_var(ncid, varid, rdata(istart:iend), &
            start=(/index(istart)/), count=(/nread/)) )
         istart = iend + 1
      ENDDO

      CALL nccheck( nf90_close(ncid) )

   END SUBROUTINE ncio_read_indexed_serial_int32_1d

   !---------------------------------------------------------
   SUBROUTINE ncio_read_indexed_serial_real8_1d (filename, dataname, index, rdata)

   USE netcdf
   USE MOD_Precision
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   integer, intent(in) :: index(:)
   real(r8), allocatable, intent(out) :: rdata(:)

   integer :: ncid, varid
   integer :: nidx, istart, iend, nread

      nidx = size(index)
      allocate (rdata(nidx))
      IF (nidx == 0) RETURN
      IF (any(index < 1)) CALL nccheck(NF90_EINVAL, trim(dataname)//' indexed read')

      CALL check_ncfile_exist (filename)
      CALL nccheck( nf90_open(trim(filename), NF90_NOWRITE, ncid) )
      CALL nccheck( nf90_inq_varid(ncid, trim(dataname), varid), trim(dataname) )

      istart = 1
      DO WHILE (istart <= nidx)
         iend = istart
         DO WHILE (iend < nidx)
            IF (index(iend+1) /= index(iend) + 1) EXIT
            iend = iend + 1
         ENDDO
         nread = iend - istart + 1
         CALL nccheck( nf90_get_var(ncid, varid, rdata(istart:iend), &
            start=(/index(istart)/), count=(/nread/)) )
         istart = iend + 1
      ENDDO

      CALL nccheck( nf90_close(ncid) )

   END SUBROUTINE ncio_read_indexed_serial_real8_1d

   !---------------------------------------------------------
   SUBROUTINE ncio_read_indexed_serial_int32_2d (filename, dataname, index, rdata)

   USE netcdf
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   integer, intent(in) :: index(:)
   integer, allocatable, intent(out) :: rdata(:,:)

   integer :: ncid, varid
   integer :: nidx, istart, iend, nread
   integer, allocatable :: varsize(:)

      CALL ncio_inquire_varsize(filename, dataname, varsize)
      nidx = size(index)
      IF (size(varsize) /= 2) CALL nccheck(NF90_EINVAL, trim(dataname)//' indexed read')
      allocate (rdata(varsize(1), nidx))
      IF (nidx == 0) THEN
         deallocate (varsize)
         RETURN
      ENDIF
      IF (any(index < 1)) CALL nccheck(NF90_EINVAL, trim(dataname)//' indexed read')

      CALL check_ncfile_exist (filename)
      CALL nccheck( nf90_open(trim(filename), NF90_NOWRITE, ncid) )
      CALL nccheck( nf90_inq_varid(ncid, trim(dataname), varid), trim(dataname) )

      istart = 1
      DO WHILE (istart <= nidx)
         iend = istart
         DO WHILE (iend < nidx)
            IF (index(iend+1) /= index(iend) + 1) EXIT
            iend = iend + 1
         ENDDO
         nread = iend - istart + 1
         CALL nccheck( nf90_get_var(ncid, varid, rdata(:,istart:iend), &
            start=(/1,index(istart)/), count=(/varsize(1),nread/)) )
         istart = iend + 1
      ENDDO

      CALL nccheck( nf90_close(ncid) )
      deallocate (varsize)

   END SUBROUTINE ncio_read_indexed_serial_int32_2d

   !---------------------------------------------------------
   SUBROUTINE ncio_read_indexed_serial_real8_2d (filename, dataname, index, rdata)

   USE netcdf
   USE MOD_Precision
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   integer, intent(in) :: index(:)
   real(r8), allocatable, intent(out) :: rdata(:,:)

   integer :: ncid, varid
   integer :: nidx, istart, iend, nread
   integer, allocatable :: varsize(:)

      CALL ncio_inquire_varsize(filename, dataname, varsize)
      nidx = size(index)
      IF (size(varsize) /= 2) CALL nccheck(NF90_EINVAL, trim(dataname)//' indexed read')
      allocate (rdata(varsize(1), nidx))
      IF (nidx == 0) THEN
         deallocate (varsize)
         RETURN
      ENDIF
      IF (any(index < 1)) CALL nccheck(NF90_EINVAL, trim(dataname)//' indexed read')

      CALL check_ncfile_exist (filename)
      CALL nccheck( nf90_open(trim(filename), NF90_NOWRITE, ncid) )
      CALL nccheck( nf90_inq_varid(ncid, trim(dataname), varid), trim(dataname) )

      istart = 1
      DO WHILE (istart <= nidx)
         iend = istart
         DO WHILE (iend < nidx)
            IF (index(iend+1) /= index(iend) + 1) EXIT
            iend = iend + 1
         ENDDO
         nread = iend - istart + 1
         CALL nccheck( nf90_get_var(ncid, varid, rdata(:,istart:iend), &
            start=(/1,index(istart)/), count=(/varsize(1),nread/)) )
         istart = iend + 1
      ENDDO

      CALL nccheck( nf90_close(ncid) )
      deallocate (varsize)

   END SUBROUTINE ncio_read_indexed_serial_real8_2d

   !---------------------------------------------------------
   SUBROUTINE ncio_read_period_serial_real8_2d (filename, dataname, timestt, timeend, rdata)

   USE netcdf
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   integer, intent(in) :: timestt, timeend

   real(r8), allocatable, intent(out) :: rdata (:,:,:)

   ! Local variables
   integer :: ncid, varid
   integer, allocatable :: varsize(:)

      CALL check_ncfile_exist (filename)

      CALL ncio_inquire_varsize (filename, dataname, varsize)

      allocate (rdata (varsize(1), varsize(2), timestt:timeend) )

      CALL nccheck( nf90_open(trim(filename), NF90_NOWRITE, ncid) )
      CALL nccheck( nf90_inq_varid(ncid, trim(dataname), varid), trim(dataname) )
      CALL nccheck( nf90_get_var(ncid, varid, rdata, &
         (/1,1,timestt/), (/varsize(1),varsize(2), timeend-timestt+1/)) )
      CALL nccheck( nf90_close(ncid) )

      deallocate(varsize)

   END SUBROUTINE ncio_read_period_serial_real8_2d

   ! -------------------------------
   SUBROUTINE ncio_define_dimension_int32 (filename, dimname, dimlen)

   USE netcdf
   USE MOD_Precision
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dimname
   integer, intent(in) :: dimlen

   ! Local variables
   integer :: ncid, dimid, status
   integer :: varid

      CALL nccheck( nf90_open(trim(filename), NF90_WRITE, ncid) )

      status = nf90_inq_dimid(ncid, trim(dimname), dimid)
      IF (status /= NF90_NOERR) THEN
         CALL nccheck (nf90_redef(ncid))
         IF (dimlen == 0) THEN
            CALL nccheck( nf90_def_dim(ncid, trim(dimname), NF90_UNLIMITED, dimid) )
         ELSE
            CALL nccheck( nf90_def_dim(ncid, trim(dimname), dimlen, dimid) )
         ENDIF
         IF (trim(dimname) .eq. 'lon') THEN
            !print *, 'lon-def'
            CALL nccheck( nf90_def_var(ncid, 'lon', nf90_float, (/dimid/), varid) )
            CALL nccheck( nf90_put_att(ncid, varid, 'long_name','longitude') )
            CALL nccheck( nf90_put_att(ncid, varid, 'units','degrees_east') )
         ELSEIF (trim(dimname) .eq.'lat') THEN
            !print *, 'lat-def'
            CALL nccheck( nf90_def_var(ncid, 'lat', nf90_float, (/dimid/), varid) )
            CALL nccheck( nf90_put_att(ncid, varid, 'long_name','latitude') )
            CALL nccheck( nf90_put_att(ncid, varid, 'units','degrees_north') )
         ENDIF
         CALL nccheck (nf90_enddef(ncid))
      ENDIF

      CALL nccheck( nf90_close(ncid) )

   END SUBROUTINE ncio_define_dimension_int32

   ! -------------------------------
   SUBROUTINE ncio_define_dimension_int64 (filename, dimname, dimlen)

   USE netcdf
   USE MOD_Precision
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dimname
   integer*8, intent(in) :: dimlen

   ! Local variables
   integer :: ncid, dimid, status
   integer :: varid

      CALL nccheck( nf90_open(trim(filename), NF90_WRITE, ncid) )

      status = nf90_inq_dimid(ncid, trim(dimname), dimid)
      IF (status /= NF90_NOERR) THEN
         CALL nccheck (nf90_redef(ncid))
         IF (dimlen == 0) THEN
            CALL nccheck( nf90_def_dim(ncid, trim(dimname), NF90_UNLIMITED, dimid) )
         ELSE
            CALL nccheck( nf90_def_dim(ncid, trim(dimname), int(dimlen), dimid) )
         ENDIF
         IF (trim(dimname) .eq. 'lon') THEN
            !print *, 'lon-def'
            CALL nccheck( nf90_def_var(ncid, 'lon', nf90_float, (/dimid/), varid) )
            CALL nccheck( nf90_put_att(ncid, varid, 'long_name','longitude') )
            CALL nccheck( nf90_put_att(ncid, varid, 'units','degrees_east') )
         ELSEIF (trim(dimname) .eq.'lat') THEN
            !print *, 'lat-def'
            CALL nccheck( nf90_def_var(ncid, 'lat', nf90_float, (/dimid/), varid) )
            CALL nccheck( nf90_put_att(ncid, varid, 'long_name','latitude') )
            CALL nccheck( nf90_put_att(ncid, varid, 'units','degrees_north') )
         ENDIF
         CALL nccheck (nf90_enddef(ncid))
      ENDIF

      CALL nccheck( nf90_close(ncid) )

   END SUBROUTINE ncio_define_dimension_int64

   !---------------------------------------------------------
   SUBROUTINE ncio_write_serial_int32_0d (filename, dataname, wdata)

   USE netcdf
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   integer, intent(in) :: wdata

   ! Local variables
   integer :: ncid, varid, status

      CALL nccheck( nf90_open(trim(filename), NF90_WRITE, ncid) )
      status = nf90_inq_varid(ncid, trim(dataname), varid)
      IF (status /= NF90_NOERR) THEN
         CALL nccheck (nf90_redef(ncid))
         CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_INT, varid = varid))
         CALL nccheck (nf90_enddef(ncid))
      ENDIF

      CALL nccheck( nf90_put_var(ncid, varid, wdata) )
      CALL nccheck( nf90_close(ncid) )

   END SUBROUTINE ncio_write_serial_int32_0d

   !---------------------------------------------------------
   SUBROUTINE ncio_write_serial_real8_0d (filename, dataname, wdata)

   USE netcdf
   USE MOD_Precision
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   real(r8), intent(in) :: wdata

   ! Local variables
   integer :: ncid, varid, status

      CALL nccheck( nf90_open(trim(filename), NF90_WRITE, ncid) )
      status = nf90_inq_varid(ncid, trim(dataname), varid)
      IF (status /= NF90_NOERR) THEN
         CALL nccheck (nf90_redef(ncid))
         CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_DOUBLE, varid = varid))
         CALL nccheck (nf90_enddef(ncid))
      ENDIF

      CALL nccheck( nf90_put_var(ncid, varid, wdata) )
      CALL nccheck( nf90_close(ncid) )

   END SUBROUTINE ncio_write_serial_real8_0d

   !---------------------------------------------------------
   SUBROUTINE ncio_write_serial_int8_1d (filename, dataname, wdata, dimname, compress)

   USE netcdf
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   integer(1), intent(in) :: wdata (:)

   character(len=*), intent(in), optional :: dimname
   integer, intent(in), optional :: compress

   ! Local variables
   integer :: ncid, varid, dimid, status

      CALL nccheck( nf90_open(trim(filename), NF90_WRITE, ncid) )
      status = nf90_inq_varid(ncid, trim(dataname), varid)
      IF (status /= NF90_NOERR) THEN
         IF (.not. present(dimname)) THEN
            write(*,*) 'Warning: no dimension name for ', trim(dataname)
            CALL nccheck( nf90_close(ncid) )
            RETURN
         ENDIF

         CALL nccheck (nf90_inq_dimid(ncid, trim(dimname), dimid))

         CALL nccheck (nf90_redef(ncid))
         IF (present(compress)) THEN
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_BYTE, dimid, varid, &
               deflate_level = compress))
         ELSE
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_BYTE, dimid, varid))
         ENDIF

         CALL nccheck (nf90_enddef(ncid))
      ENDIF

      CALL nccheck( nf90_put_var(ncid, varid, wdata) )
      CALL nccheck( nf90_close(ncid) )

   END SUBROUTINE ncio_write_serial_int8_1d

   !---------------------------------------------------------
   SUBROUTINE ncio_write_serial_int32_1d (filename, dataname, wdata, dimname, compress)

   USE netcdf
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   integer, intent(in) :: wdata (:)

   character(len=*), intent(in), optional :: dimname
   integer, intent(in), optional :: compress

   ! Local variables
   integer :: ncid, varid, dimid, status

      CALL nccheck( nf90_open(trim(filename), NF90_WRITE, ncid) )
      status = nf90_inq_varid(ncid, trim(dataname), varid)
      IF (status /= NF90_NOERR) THEN
         IF (.not. present(dimname)) THEN
            write(*,*) 'Warning: no dimension name for ', trim(dataname)
            CALL nccheck( nf90_close(ncid) )
            RETURN
         ENDIF

         CALL nccheck (nf90_inq_dimid(ncid, trim(dimname), dimid))

         CALL nccheck (nf90_redef(ncid))
         IF (present(compress)) THEN
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_INT, dimid, varid, &
               deflate_level = compress))
         ELSE
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_INT, dimid, varid))
         ENDIF

         CALL nccheck (nf90_enddef(ncid))
      ENDIF

      CALL nccheck( nf90_put_var(ncid, varid, wdata) )
      CALL nccheck( nf90_close(ncid) )

   END SUBROUTINE ncio_write_serial_int32_1d

   !---------------------------------------------------------
   SUBROUTINE ncio_write_serial_int64_1d (filename, dataname, wdata, dimname, compress)

   USE netcdf
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   integer*8, intent(in) :: wdata (:)

   character(len=*), intent(in), optional :: dimname
   integer, intent(in), optional :: compress

   ! Local variables
   integer :: ncid, varid, dimid, status

      CALL nccheck( nf90_open(trim(filename), NF90_WRITE, ncid) )
      status = nf90_inq_varid(ncid, trim(dataname), varid)
      IF (status /= NF90_NOERR) THEN
         IF (.not. present(dimname)) THEN
            write(*,*) 'Warning: no dimension name for ', trim(dataname)
            CALL nccheck( nf90_close(ncid) )
            RETURN
         ENDIF

         CALL nccheck (nf90_inq_dimid(ncid, trim(dimname), dimid))

         CALL nccheck (nf90_redef(ncid))
         IF (present(compress)) THEN
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_INT64, dimid, varid, &
               deflate_level = compress))
         ELSE
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_INT64, dimid, varid))
         ENDIF

         CALL nccheck (nf90_enddef(ncid))
      ENDIF

      CALL nccheck( nf90_put_var(ncid, varid, wdata) )
      CALL nccheck( nf90_close(ncid) )

   END SUBROUTINE ncio_write_serial_int64_1d

   !---------------------------------------------------------
   SUBROUTINE ncio_write_serial_real8_1d (filename, dataname, wdata, dimname, compress)

   USE netcdf
   USE MOD_Precision
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   real(r8), intent(in) :: wdata (:)

   character(len=*), intent(in), optional :: dimname
   integer, intent(in), optional :: compress

   ! Local variables
   integer :: ncid, varid, dimid, status

      CALL nccheck( nf90_open(trim(filename), NF90_WRITE, ncid) )
      status = nf90_inq_varid(ncid, trim(dataname), varid)
      IF (status /= NF90_NOERR) THEN
         IF (.not. present(dimname)) THEN
            write(*,*) 'Warning: no dimension name for ', trim(dataname)
            CALL nccheck( nf90_close(ncid) )
            RETURN
         ENDIF

         CALL nccheck (nf90_inq_dimid(ncid, trim(dimname), dimid))

         CALL nccheck (nf90_redef(ncid))
         IF (present(compress)) THEN
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_DOUBLE, dimid, varid, &
               deflate_level = compress))
         ELSE
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_DOUBLE, dimid, varid))
         ENDIF

         CALL nccheck (nf90_enddef(ncid))
      ENDIF

      CALL nccheck( nf90_put_var(ncid, varid, wdata) )
      CALL nccheck( nf90_close(ncid) )

   END SUBROUTINE ncio_write_serial_real8_1d

   !---------------------------------------------------------
   SUBROUTINE ncio_write_serial_logical_1d (filename, dataname, wdata, dimname, compress)

   USE netcdf
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   logical, intent(in) :: wdata (:)

   character(len=*), intent(in)  :: dimname
   integer, intent(in), optional :: compress

   ! Local variables
   integer(1), allocatable :: wdata_byte(:)

      allocate(wdata_byte(size(wdata)))
      WHERE(wdata)
         wdata_byte = 1
      ELSEWHERE
         wdata_byte = 0
      ENDWHERE

      IF (present(compress)) THEN
         CALL ncio_write_serial_int8_1d (filename, dataname, wdata_byte, dimname, compress)
      ELSE
         CALL ncio_write_serial_int8_1d (filename, dataname, wdata_byte, dimname)
      ENDIF

      deallocate(wdata_byte)

   END SUBROUTINE ncio_write_serial_logical_1d

   !---------------------------------------------------------
   SUBROUTINE ncio_write_serial_int8_2d (filename, dataname, wdata, &
         dim1name, dim2name, compress)

   USE netcdf
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   integer(1), intent(in) :: wdata (:,:)

   character(len=*), intent(in), optional :: dim1name, dim2name
   integer, intent(in), optional :: compress

   ! Local variables
   integer :: ncid, varid, dimid(2), status

      CALL nccheck( nf90_open(trim(filename), NF90_WRITE, ncid) )
      status = nf90_inq_varid(ncid, trim(dataname), varid)
      IF (status /= NF90_NOERR) THEN
         IF (.not. (present(dim1name) .and. present(dim2name))) THEN
            write(*,*) 'Warning: no dimension name for ', trim(dataname)
            CALL nccheck( nf90_close(ncid) )
            RETURN
         ENDIF

         CALL nccheck (nf90_inq_dimid(ncid, trim(dim1name), dimid(1)))
         CALL nccheck (nf90_inq_dimid(ncid, trim(dim2name), dimid(2)))

         CALL nccheck (nf90_redef(ncid))
         IF (present(compress)) THEN
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_BYTE, dimid, varid, &
               deflate_level = compress))
         ELSE
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_BYTE, dimid, varid))
         ENDIF

         CALL nccheck (nf90_enddef(ncid))
      ENDIF

      CALL nccheck( nf90_put_var(ncid, varid, wdata) )
      CALL nccheck( nf90_close(ncid) )

   END SUBROUTINE ncio_write_serial_int8_2d

   !---------------------------------------------------------
   SUBROUTINE ncio_write_serial_int16_2d (filename, dataname, wdata, &
         dim1name, dim2name, compress)

   USE netcdf
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   integer(2), intent(in) :: wdata (:,:)

   character(len=*), intent(in), optional :: dim1name, dim2name
   integer, intent(in), optional :: compress

   ! Local variables
   integer :: ncid, varid, dimid(2), status

      CALL nccheck( nf90_open(trim(filename), NF90_WRITE, ncid) )
      status = nf90_inq_varid(ncid, trim(dataname), varid)
      IF (status /= NF90_NOERR) THEN
         IF (.not. (present(dim1name) .and. present(dim2name))) THEN
            write(*,*) 'Warning: no dimension name for ', trim(dataname)
            CALL nccheck( nf90_close(ncid) )
            RETURN
         ENDIF

         CALL nccheck (nf90_inq_dimid(ncid, trim(dim1name), dimid(1)))
         CALL nccheck (nf90_inq_dimid(ncid, trim(dim2name), dimid(2)))

         CALL nccheck (nf90_redef(ncid))
         IF (present(compress)) THEN
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_SHORT, dimid, varid, &
               deflate_level = compress))
         ELSE
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_SHORT, dimid, varid))
         ENDIF

         CALL nccheck (nf90_enddef(ncid))
      ENDIF

      CALL nccheck( nf90_put_var(ncid, varid, wdata) )
      CALL nccheck( nf90_close(ncid) )

   END SUBROUTINE ncio_write_serial_int16_2d

   !---------------------------------------------------------
   SUBROUTINE ncio_write_serial_int32_2d (filename, dataname, wdata, &
         dim1name, dim2name, compress)

   USE netcdf
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   integer, intent(in) :: wdata (:,:)

   character(len=*), intent(in), optional :: dim1name, dim2name
   integer, intent(in), optional :: compress

   ! Local variables
   integer :: ncid, varid, dimid(2), status

      CALL nccheck( nf90_open(trim(filename), NF90_WRITE, ncid) )
      status = nf90_inq_varid(ncid, trim(dataname), varid)
      IF (status /= NF90_NOERR) THEN
         IF (.not. (present(dim1name) .and. present(dim2name))) THEN
            write(*,*) 'Warning: no dimension name for ', trim(dataname)
            CALL nccheck( nf90_close(ncid) )
            RETURN
         ENDIF

         CALL nccheck (nf90_inq_dimid(ncid, trim(dim1name), dimid(1)))
         CALL nccheck (nf90_inq_dimid(ncid, trim(dim2name), dimid(2)))

         CALL nccheck (nf90_redef(ncid))
         IF (present(compress)) THEN
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_INT, dimid, varid, &
               deflate_level = compress))
         ELSE
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_INT, dimid, varid))
         ENDIF

         CALL nccheck (nf90_enddef(ncid))
      ENDIF

      CALL nccheck( nf90_put_var(ncid, varid, wdata) )
      CALL nccheck( nf90_close(ncid) )

   END SUBROUTINE ncio_write_serial_int32_2d

   !---------------------------------------------------------
   SUBROUTINE ncio_write_serial_int64_2d (filename, dataname, wdata, &
         dim1name, dim2name, compress)

   USE netcdf
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   integer*8, intent(in) :: wdata (:,:)

   character(len=*), intent(in), optional :: dim1name, dim2name
   integer, intent(in), optional :: compress

   ! Local variables
   integer :: ncid, varid, dimid(2), status

      CALL nccheck( nf90_open(trim(filename), NF90_WRITE, ncid) )
      status = nf90_inq_varid(ncid, trim(dataname), varid)
      IF (status /= NF90_NOERR) THEN
         IF (.not. (present(dim1name) .and. present(dim2name))) THEN
            write(*,*) 'Warning: no dimension name for ', trim(dataname)
            CALL nccheck( nf90_close(ncid) )
            RETURN
         ENDIF

         CALL nccheck (nf90_inq_dimid(ncid, trim(dim1name), dimid(1)))
         CALL nccheck (nf90_inq_dimid(ncid, trim(dim2name), dimid(2)))

         CALL nccheck (nf90_redef(ncid))
         IF (present(compress)) THEN
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_INT64, dimid, varid, &
               deflate_level = compress))
         ELSE
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_INT64, dimid, varid))
         ENDIF

         CALL nccheck (nf90_enddef(ncid))
      ENDIF

      CALL nccheck( nf90_put_var(ncid, varid, wdata) )
      CALL nccheck( nf90_close(ncid) )

   END SUBROUTINE ncio_write_serial_int64_2d

   !---------------------------------------------------------
   SUBROUTINE ncio_write_serial_real4_2d (filename, dataname, wdata, &
         dim1name, dim2name, compress)

   USE netcdf
   USE MOD_Precision
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   real(4), intent(in) :: wdata (:,:)

   character(len=*), intent(in), optional :: dim1name, dim2name
   integer, intent(in), optional :: compress

   ! Local variables
   integer :: ncid, varid, dimid(2), status

      CALL nccheck( nf90_open(trim(filename), NF90_WRITE, ncid) )
      status = nf90_inq_varid(ncid, trim(dataname), varid)
      IF (status /= NF90_NOERR) THEN
         IF (.not. (present(dim1name) .and. present(dim2name))) THEN
            write(*,*) 'Warning: no dimension name for ', trim(dataname)
            CALL nccheck( nf90_close(ncid) )
            RETURN
         ENDIF

         CALL nccheck (nf90_inq_dimid(ncid, trim(dim1name), dimid(1)))
         CALL nccheck (nf90_inq_dimid(ncid, trim(dim2name), dimid(2)))

         CALL nccheck (nf90_redef(ncid))
         IF (present(compress)) THEN
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_FLOAT, dimid, varid, &
               deflate_level = compress))
         ELSE
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_FLOAT, dimid, varid))
         ENDIF

         CALL nccheck (nf90_enddef(ncid))
      ENDIF

      CALL nccheck( nf90_put_var(ncid, varid, wdata) )
      CALL nccheck( nf90_close(ncid) )

   END SUBROUTINE ncio_write_serial_real4_2d

   !---------------------------------------------------------
   SUBROUTINE ncio_write_serial_real8_2d (filename, dataname, wdata, &
         dim1name, dim2name, compress)

   USE netcdf
   USE MOD_Precision
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   real(r8), intent(in) :: wdata (:,:)

   character(len=*), intent(in), optional :: dim1name, dim2name
   integer, intent(in), optional :: compress

   ! Local variables
   integer :: ncid, varid, dimid(2), status

      CALL nccheck( nf90_open(trim(filename), NF90_WRITE, ncid) )
      status = nf90_inq_varid(ncid, trim(dataname), varid)
      IF (status /= NF90_NOERR) THEN
         IF (.not. (present(dim1name) .and. present(dim2name))) THEN
            write(*,*) 'Warning: no dimension name for ', trim(dataname)
            CALL nccheck( nf90_close(ncid) )
            RETURN
         ENDIF

         CALL nccheck (nf90_inq_dimid(ncid, trim(dim1name), dimid(1)))
         CALL nccheck (nf90_inq_dimid(ncid, trim(dim2name), dimid(2)))

         CALL nccheck (nf90_redef(ncid))
         IF (present(compress)) THEN
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_DOUBLE, dimid, varid, &
               deflate_level = compress))
         ELSE
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_DOUBLE, dimid, varid))
         ENDIF

         CALL nccheck (nf90_enddef(ncid))
      ENDIF

      CALL nccheck( nf90_put_var(ncid, varid, wdata) )
      CALL nccheck( nf90_close(ncid) )

   END SUBROUTINE ncio_write_serial_real8_2d

   !---------------------------------------------------------
   SUBROUTINE ncio_write_serial_int32_3d (filename, dataname, wdata, &
         dim1name, dim2name, dim3name, compress)

   USE netcdf
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   integer, intent(in) :: wdata (:,:,:)

   character(len=*), intent(in), optional :: dim1name, dim2name, dim3name
   integer, intent(in), optional          :: compress

   ! Local variables
   integer :: ncid, varid, dimid(3), status

      CALL nccheck( nf90_open(trim(filename), NF90_WRITE, ncid) )
      status = nf90_inq_varid(ncid, trim(dataname), varid)
      IF (status /= NF90_NOERR) THEN
         IF (.not. (present(dim1name) .and. present(dim2name) .and. present(dim3name))) THEN
            write(*,*) 'Warning: no dimension name for ', trim(dataname)
            CALL nccheck( nf90_close(ncid) )
            RETURN
         ENDIF

         CALL nccheck (nf90_inq_dimid(ncid, trim(dim1name), dimid(1)))
         CALL nccheck (nf90_inq_dimid(ncid, trim(dim2name), dimid(2)))
         CALL nccheck (nf90_inq_dimid(ncid, trim(dim3name), dimid(3)))

         CALL nccheck (nf90_redef(ncid))
         IF (present(compress)) THEN
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_INT, dimid, varid, &
               deflate_level = compress))
         ELSE
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_INT, dimid, varid))
         ENDIF

         CALL nccheck (nf90_enddef(ncid))
      ENDIF

      CALL nccheck( nf90_put_var(ncid, varid, wdata) )
      CALL nccheck( nf90_close(ncid) )

   END SUBROUTINE ncio_write_serial_int32_3d

   !---------------------------------------------------------
   SUBROUTINE ncio_write_serial_real8_3d (filename, dataname, wdata, &
         dim1name, dim2name, dim3name, compress)

   USE netcdf
   USE MOD_Precision
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   real(r8), intent(in) :: wdata (:,:,:)

   character(len=*), intent(in), optional :: dim1name, dim2name, dim3name
   integer, intent(in), optional :: compress

   ! Local variables
   integer :: ncid, varid, dimid(3), status

      CALL nccheck( nf90_open(trim(filename), NF90_WRITE, ncid) )
      status = nf90_inq_varid(ncid, trim(dataname), varid)
      IF (status /= NF90_NOERR) THEN
         IF (.not. (present(dim1name) .and. present(dim2name) .and. present(dim3name))) THEN
            write(*,*) 'Warning: no dimension name for ', trim(dataname)
            CALL nccheck( nf90_close(ncid) )
            RETURN
         ENDIF

         CALL nccheck (nf90_inq_dimid(ncid, trim(dim1name), dimid(1)))
         CALL nccheck (nf90_inq_dimid(ncid, trim(dim2name), dimid(2)))
         CALL nccheck (nf90_inq_dimid(ncid, trim(dim3name), dimid(3)))

         CALL nccheck (nf90_redef(ncid))
         IF (present(compress)) THEN
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_DOUBLE, dimid, varid, &
               deflate_level = compress))
         ELSE
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_DOUBLE, dimid, varid))
         ENDIF

         CALL nccheck (nf90_enddef(ncid))
      ENDIF

      CALL nccheck( nf90_put_var(ncid, varid, wdata) )
      CALL nccheck( nf90_close(ncid) )

   END SUBROUTINE ncio_write_serial_real8_3d

   !---------------------------------------------------------
   SUBROUTINE ncio_write_serial_real8_4d (filename, dataname, wdata, &
         dim1name, dim2name, dim3name, dim4name, compress)

   USE netcdf
   USE MOD_Precision
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   real(r8), intent(in) :: wdata (:,:,:,:)

   character(len=*), intent(in), optional :: dim1name, dim2name, dim3name, dim4name
   integer, intent(in), optional :: compress

   ! Local variables
   integer :: ncid, varid, dimid(4), status

      CALL nccheck( nf90_open(trim(filename), NF90_WRITE, ncid) )
      status = nf90_inq_varid(ncid, trim(dataname), varid)
      IF (status /= NF90_NOERR) THEN
         IF (.not. (present(dim1name) .and. present(dim2name) &
            .and. present(dim3name) .and. present(dim4name))) THEN
            write(*,*) 'Warning: no dimension name for ', trim(dataname)
            CALL nccheck( nf90_close(ncid) )
            RETURN
         ENDIF

         CALL nccheck (nf90_inq_dimid(ncid, trim(dim1name), dimid(1)))
         CALL nccheck (nf90_inq_dimid(ncid, trim(dim2name), dimid(2)))
         CALL nccheck (nf90_inq_dimid(ncid, trim(dim3name), dimid(3)))
         CALL nccheck (nf90_inq_dimid(ncid, trim(dim4name), dimid(4)))

         CALL nccheck (nf90_redef(ncid))
         IF (present(compress)) THEN
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_DOUBLE, dimid, varid, &
               deflate_level = compress))
         ELSE
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_DOUBLE, dimid, varid))
         ENDIF

         CALL nccheck (nf90_enddef(ncid))
      ENDIF

      CALL nccheck( nf90_put_var(ncid, varid, wdata) )
      CALL nccheck( nf90_close(ncid) )

   END SUBROUTINE ncio_write_serial_real8_4d

   !---------------------------------------------------------
   SUBROUTINE ncio_write_serial_real8_5d (filename, dataname, wdata, &
         dim1name, dim2name, dim3name, dim4name, dim5name, compress)

   USE netcdf
   USE MOD_Precision
   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: dataname
   real(r8), intent(in) :: wdata (:,:,:,:,:)

   character(len=*), intent(in), optional :: dim1name, dim2name, dim3name
   character(len=*), intent(in), optional :: dim4name, dim5name
   integer, intent(in), optional :: compress

   ! Local variables
   integer :: ncid, varid, dimid(5), status

      CALL nccheck( nf90_open(trim(filename), NF90_WRITE, ncid) )
      status = nf90_inq_varid(ncid, trim(dataname), varid)
      IF (status /= NF90_NOERR) THEN
         IF (.not. (present(dim1name) .and. present(dim2name) .and. present(dim3name) &
            .and. present(dim4name) .and. present(dim5name))) THEN
            write(*,*) 'Warning: no dimension name for ', trim(dataname)
            CALL nccheck( nf90_close(ncid) )
            RETURN
         ENDIF

         CALL nccheck (nf90_inq_dimid(ncid, trim(dim1name), dimid(1)))
         CALL nccheck (nf90_inq_dimid(ncid, trim(dim2name), dimid(2)))
         CALL nccheck (nf90_inq_dimid(ncid, trim(dim3name), dimid(3)))
         CALL nccheck (nf90_inq_dimid(ncid, trim(dim4name), dimid(4)))
         CALL nccheck (nf90_inq_dimid(ncid, trim(dim5name), dimid(5)))

         CALL nccheck (nf90_redef(ncid))
         IF (present(compress)) THEN
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_DOUBLE, dimid, varid, &
               deflate_level = compress))
         ELSE
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_DOUBLE, dimid, varid))
         ENDIF

         CALL nccheck (nf90_enddef(ncid))
      ENDIF

      CALL nccheck( nf90_put_var(ncid, varid, wdata) )
      CALL nccheck( nf90_close(ncid) )

   END SUBROUTINE ncio_write_serial_real8_5d

   !------------------------------
   SUBROUTINE ncio_write_time (filename, dataname, time_component, itime, adjust)

   USE MOD_TimeManager
   IMPLICIT NONE

   character (len=*), intent(in) :: filename
   character (len=*), intent(in) :: dataname
   integer, intent(in)  :: time_component(3)
   integer, intent(out) :: itime

   character(len=*), intent(in), optional :: adjust

   ! Local variables
   integer, allocatable :: time_file(:)
   integer :: ncid, varid, time_id, status
   integer :: timelen, minutes

      minutes = minutes_since_1900 (time_component(1), time_component(2), time_component(3))

      IF (present(adjust)) THEN
         SELECTCASE (trim(adjustl(adjust)))
         CASE ('HOURLY')
            minutes = minutes - 30
         CASE ('DAILY')
            minutes = minutes - 720
         CASE ('MONTHLY')
            minutes = minutes - 21600
         CASE ('YEARLY')
            minutes = minutes - 262800
         ENDSELECT
      ENDIF

      CALL nccheck( nf90_open(trim(filename), NF90_WRITE, ncid) )
      status = nf90_inq_varid(ncid, trim(dataname), varid)
      IF (status == NF90_NOERR) THEN
         CALL nccheck( nf90_inq_dimid(ncid, 'time', time_id) )
         CALL nccheck( nf90_inquire_dimension(ncid, time_id, len = timelen) )

         itime = 1
         IF (timelen > 0) THEN
            allocate (time_file (timelen))
            CALL nccheck( nf90_get_var(ncid, varid, time_file) )

            DO WHILE (itime <= timelen)
               IF (minutes == time_file(itime)) EXIT
               itime = itime + 1
            ENDDO

            deallocate(time_file)
         ENDIF

      ELSE
         status = nf90_inq_dimid(ncid, 'time', time_id)
         IF (status /= NF90_NOERR) THEN
            CALL nccheck( nf90_redef(ncid) )
            CALL nccheck( nf90_def_dim(ncid, 'time', NF90_UNLIMITED, time_id) )
            CALL nccheck( nf90_enddef(ncid) )
         ENDIF

         CALL nccheck( nf90_redef(ncid) )
         CALL nccheck( nf90_def_var(ncid, trim(dataname), NF90_INT, (/time_id/), varid) )

         CALL nccheck( nf90_put_att(ncid, varid, 'long_name', 'time') )
         CALL nccheck( nf90_put_att(ncid, varid, 'units', 'minutes since 1900-1-1 0:0:0') )
         CALL nccheck( nf90_enddef(ncid) )

         itime = 1
      ENDIF

      CALL nccheck( nf90_put_var(ncid, varid, minutes, (/itime/)) )
      CALL nccheck( nf90_close(ncid) )

   END SUBROUTINE ncio_write_time

   !------------------------------
   SUBROUTINE ncio_write_lastdim (filename, lastname, lastvalue, ilast)

   IMPLICIT NONE

   character (len=*), intent(in) :: filename
   character (len=*), intent(in) :: lastname
   integer, intent(in)  :: lastvalue
   integer, intent(out) :: ilast

   ! Local variables
   integer :: ncid, varid, dimid, dimlen, status
   integer, allocatable :: lastvalue_f(:)

      CALL nccheck( nf90_open(trim(filename), NF90_WRITE, ncid) )

      status = nf90_inq_varid(ncid, trim(lastname), varid)

      IF (status == NF90_NOERR) THEN
         CALL nccheck( nf90_inq_dimid(ncid, trim(lastname), dimid) )
         CALL nccheck( nf90_inquire_dimension(ncid, dimid, len = dimlen) )

         ilast = 1
         IF (dimlen > 0) THEN
            allocate (lastvalue_f (dimlen))
            CALL nccheck( nf90_get_var(ncid, varid, lastvalue_f) )

            DO WHILE (ilast <= dimlen)
               IF (lastvalue == lastvalue_f(ilast)) EXIT
               ilast = ilast + 1
            ENDDO

            deallocate(lastvalue_f)
         ENDIF
      ELSE
         status = nf90_inq_dimid(ncid, trim(lastname), dimid)
         IF (status /= NF90_NOERR) THEN
            CALL nccheck( nf90_redef(ncid) )
            CALL nccheck( nf90_def_dim(ncid, trim(lastname), NF90_UNLIMITED, dimid) )
            CALL nccheck( nf90_enddef(ncid) )
         ENDIF

         CALL nccheck( nf90_redef(ncid) )
         CALL nccheck( nf90_def_var(ncid, trim(lastname), NF90_INT, (/dimid/), varid) )
         CALL nccheck( nf90_enddef(ncid) )

         ilast = 1
      ENDIF

      CALL nccheck( nf90_put_var(ncid, varid, lastvalue, (/ilast/)) )

      CALL nccheck( nf90_close(ncid) )

   END SUBROUTINE ncio_write_lastdim

   !----------------------------------------------------------------------------
   SUBROUTINE ncio_write_serial_real8_0d_time ( &
         filename, dataname, itime, wdata, dim1name)

   USE netcdf
   USE MOD_Precision
   IMPLICIT NONE

   character (len=*), intent(in) :: filename
   character (len=*), intent(in) :: dataname
   integer,  intent(in) :: itime
   real(r8), intent(in) :: wdata

   character(len=*), intent(in), optional :: dim1name

   ! Local variables
   integer :: ncid, varid, dimid, status

      CALL nccheck( nf90_open(trim(filename), NF90_WRITE, ncid) )
      status = nf90_inq_varid(ncid, trim(dataname), varid)
      IF (status /= NF90_NOERR) THEN
         IF (.not. present(dim1name)) THEN
            write(*,*) 'Warning: no dimension name for ', trim(dataname)
            CALL nccheck( nf90_close(ncid) )
            RETURN
         ENDIF

         CALL nccheck (nf90_inq_dimid(ncid, trim(dim1name), dimid))

         CALL nccheck (nf90_redef(ncid))
         CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_DOUBLE, dimid, varid))

         CALL nccheck (nf90_enddef(ncid))
      ENDIF

      CALL nccheck( nf90_put_var(ncid, varid, wdata, start = (/itime/)) )

      CALL nccheck( nf90_close(ncid) )

   END SUBROUTINE ncio_write_serial_real8_0d_time

   !----------------------------------------------------------------------------
   SUBROUTINE ncio_write_serial_real8_1d_time ( &
         filename, dataname, itime, wdata, &
         dim1name, dim2name, compress)

   USE netcdf
   USE MOD_Precision
   IMPLICIT NONE

   character (len=*), intent(in) :: filename
   character (len=*), intent(in) :: dataname
   integer,  intent(in) :: itime
   real(r8), intent(in) :: wdata(:)

   character(len=*), intent(in), optional :: dim1name, dim2name
   integer, intent(in), optional :: compress

   ! Local variables
   integer :: ncid, varid, dimid(2), status

      CALL nccheck( nf90_open(trim(filename), NF90_WRITE, ncid) )
      status = nf90_inq_varid(ncid, trim(dataname), varid)
      IF (status /= NF90_NOERR) THEN
         IF (.not. (present(dim1name) .and. present(dim2name))) THEN
            write(*,*) 'Warning: no dimension name for ', trim(dataname)
            CALL nccheck( nf90_close(ncid) )
            RETURN
         ENDIF

         CALL nccheck (nf90_inq_dimid(ncid, trim(dim1name), dimid(1)))
         CALL nccheck (nf90_inq_dimid(ncid, trim(dim2name), dimid(2)))

         CALL nccheck (nf90_redef(ncid))
         IF (present(compress)) THEN
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_DOUBLE, dimid, varid, &
               deflate_level = compress))
         ELSE
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_DOUBLE, dimid, varid))
         ENDIF

         CALL nccheck (nf90_enddef(ncid))
      ENDIF

      CALL nccheck( nf90_put_var(ncid, varid, wdata, &
         (/1,itime/), (/size(wdata,1),1/)) )

      CALL nccheck( nf90_close(ncid) )

   END SUBROUTINE ncio_write_serial_real8_1d_time

   !----------------------------------------------------------------------------
   SUBROUTINE ncio_write_serial_real8_2d_time ( &
         filename, dataname, itime, wdata, &
         dim1name, dim2name, dim3name, compress)

   USE netcdf
   USE MOD_Precision
   IMPLICIT NONE

   character (len=*), intent(in) :: filename
   character (len=*), intent(in) :: dataname
   integer,  intent(in) :: itime
   real(r8), intent(in) :: wdata(:,:)

   character(len=*), intent(in), optional :: dim1name, dim2name, dim3name
   integer, intent(in), optional :: compress

   ! Local variables
   integer :: ncid, varid, dimid(3), status

      CALL nccheck( nf90_open(trim(filename), NF90_WRITE, ncid) )
      status = nf90_inq_varid(ncid, trim(dataname), varid)
      IF (status /= NF90_NOERR) THEN
         IF (.not. (present(dim1name) .and. present(dim2name) .and. present(dim3name))) THEN
            write(*,*) 'Warning: no dimension name for ', trim(dataname)
            CALL nccheck( nf90_close(ncid) )
            RETURN
         ENDIF

         CALL nccheck (nf90_inq_dimid(ncid, trim(dim1name), dimid(1)))
         CALL nccheck (nf90_inq_dimid(ncid, trim(dim2name), dimid(2)))
         CALL nccheck (nf90_inq_dimid(ncid, trim(dim3name), dimid(3)))

         CALL nccheck (nf90_redef(ncid))
         IF (present(compress)) THEN
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_DOUBLE, dimid, varid, &
               deflate_level = compress))
         ELSE
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_DOUBLE, dimid, varid))
         ENDIF

         CALL nccheck (nf90_enddef(ncid))
      ENDIF

      CALL nccheck( nf90_put_var(ncid, varid, wdata, &
         (/1,1,itime/), (/size(wdata,1),size(wdata,2),1/)) )

      CALL nccheck( nf90_close(ncid) )

   END SUBROUTINE ncio_write_serial_real8_2d_time

   !----------------------------------------------------------------------------
   SUBROUTINE ncio_write_serial_real8_3d_time ( &
         filename, dataname, itime, wdata, &
         dim1name, dim2name, dim3name, dim4name, compress)

   USE netcdf
   USE MOD_Precision
   IMPLICIT NONE

   character (len=*), intent(in) :: filename
   character (len=*), intent(in) :: dataname
   integer,  intent(in) :: itime
   real(r8), intent(in) :: wdata(:,:,:)

   character(len=*), intent(in), optional :: dim1name, dim2name, dim3name, dim4name
   integer, intent(in), optional :: compress
   ! Local variables
   integer :: ncid, varid, dimid(4), status

      CALL nccheck( nf90_open(trim(filename), NF90_WRITE, ncid) )
      status = nf90_inq_varid(ncid, trim(dataname), varid)
      IF (status /= NF90_NOERR) THEN
         IF (.not. (present(dim1name) .and. present(dim2name) &
            .and. present(dim3name) .and. present(dim4name))) THEN
            write(*,*) 'Warning: no dimension name for ', trim(dataname)
            CALL nccheck( nf90_close(ncid) )
            RETURN
         ENDIF

         CALL nccheck (nf90_inq_dimid(ncid, trim(dim1name), dimid(1)))
         CALL nccheck (nf90_inq_dimid(ncid, trim(dim2name), dimid(2)))
         CALL nccheck (nf90_inq_dimid(ncid, trim(dim3name), dimid(3)))
         CALL nccheck (nf90_inq_dimid(ncid, trim(dim4name), dimid(4)))

         CALL nccheck (nf90_redef(ncid))
         IF (present(compress)) THEN
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_DOUBLE, dimid, varid, &
               deflate_level = compress))
         ELSE
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_DOUBLE, dimid, varid))
         ENDIF

         CALL nccheck (nf90_enddef(ncid))
      ENDIF

      CALL nccheck( nf90_put_var(ncid, varid, wdata, &
         (/1,1,1,itime/), (/size(wdata,1),size(wdata,2),size(wdata,3),1/)) )

      CALL nccheck( nf90_close(ncid) )

   END SUBROUTINE ncio_write_serial_real8_3d_time

   !----------------------------------------------------------------------------
   SUBROUTINE ncio_write_serial_real8_4d_time ( &
         filename, dataname, itime, wdata, &
         dim1name, dim2name, dim3name, dim4name, dim5name, compress)

   USE netcdf
   USE MOD_Precision
   IMPLICIT NONE

   character (len=*), intent(in) :: filename
   character (len=*), intent(in) :: dataname
   integer,  intent(in) :: itime
   real(r8), intent(in) :: wdata(:,:,:,:)

   character(len=*), intent(in), optional :: dim1name, dim2name, dim3name
   character(len=*), intent(in), optional :: dim4name, dim5name
   integer, intent(in), optional :: compress

   ! Local variables
   integer :: ncid, varid, dimid(5), status

      CALL nccheck( nf90_open(trim(filename), NF90_WRITE, ncid) )
      status = nf90_inq_varid(ncid, trim(dataname), varid)
      IF (status /= NF90_NOERR) THEN
         IF (.not. (present(dim1name) .and. present(dim2name) &
            .and. present(dim3name) .and. present(dim4name) .and. present(dim5name))) THEN
            write(*,*) 'Warning: no dimension name for ', trim(dataname)
            CALL nccheck( nf90_close(ncid) )
            RETURN
         ENDIF

         CALL nccheck (nf90_inq_dimid(ncid, trim(dim1name), dimid(1)))
         CALL nccheck (nf90_inq_dimid(ncid, trim(dim2name), dimid(2)))
         CALL nccheck (nf90_inq_dimid(ncid, trim(dim3name), dimid(3)))
         CALL nccheck (nf90_inq_dimid(ncid, trim(dim4name), dimid(4)))
         CALL nccheck (nf90_inq_dimid(ncid, trim(dim5name), dimid(5)))

         CALL nccheck (nf90_redef(ncid))
         IF (present(compress)) THEN
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_DOUBLE, dimid, varid, &
               deflate_level = compress))
         ELSE
            CALL nccheck (nf90_def_var(ncid, trim(dataname), NF90_DOUBLE, dimid, varid))
         ENDIF

         CALL nccheck (nf90_enddef(ncid))
      ENDIF

      CALL nccheck( nf90_put_var(ncid, varid, wdata, &
         (/1,1,1,1,itime/), (/size(wdata,1),size(wdata,2),size(wdata,3),size(wdata,4), 1/)) )

      CALL nccheck( nf90_close(ncid) )

   END SUBROUTINE ncio_write_serial_real8_4d_time

   !----------------------
   SUBROUTINE ncio_write_colm_dimension (filename)

   USE MOD_Vars_Global, only: nl_soil, maxsnl, nl_lake, nvegwcs
   IMPLICIT NONE

   character(len=*), intent(in) :: filename

   ! Local Variables
   integer :: soillayers(1:nl_soil)
   integer :: soilinterfaces(0:nl_soil)
   integer :: soilsnowlayers(-maxsnl+nl_soil)
   integer :: lakelayers(1:nl_lake)
   integer :: vegnodes(1:nvegwcs)
   integer :: i


      soillayers = (/(i, i = 1,nl_soil)/)
      CALL ncio_define_dimension (filename, 'soil', nl_soil)
      CALL ncio_write_serial (filename, 'soil', soillayers, 'soil')
      CALL ncio_put_attr_str (filename, 'soil', 'long_name', 'soil layers')

      soilinterfaces = (/(i, i = 0,nl_soil)/)
      CALL ncio_define_dimension (filename, 'soilinterface', nl_soil+1)
      CALL ncio_write_serial (filename, 'soilinterface', soilinterfaces, 'soilinterface')
      CALL ncio_put_attr_str (filename, 'soilinterface', 'long_name', 'soil layer interfaces')

      soilsnowlayers = (/(i, i = maxsnl+1,nl_soil)/)
      CALL ncio_define_dimension (filename, 'soilsnow', -maxsnl+nl_soil)
      CALL ncio_write_serial (filename, 'soilsnow', soilsnowlayers, 'soilsnow')
      CALL ncio_put_attr_str (filename, 'soilsnow', 'long_name', 'snow(<= 0) and soil(>0) layers')

      lakelayers = (/(i, i = 1,nl_lake)/)
      CALL ncio_define_dimension (filename, 'lake', nl_lake)
      CALL ncio_write_serial (filename, 'lake', lakelayers, 'lake')
      CALL ncio_put_attr_str (filename, 'lake', 'long_name', 'vertical lake layers')

      vegnodes = (/(i, i = 1,nvegwcs)/)
      CALL ncio_define_dimension (filename, 'vegnodes', nvegwcs)
      CALL ncio_write_serial (filename, 'vegnodes', vegnodes, 'vegnodes')
      CALL ncio_put_attr_str (filename, 'vegnodes', 'long_name', 'vegetation water potential nodes')

      CALL ncio_define_dimension (filename, 'band', 2)
      CALL ncio_write_serial (filename, 'band', (/1,2/), 'band')
      CALL ncio_put_attr_str (filename, 'band', 'long_name', '1 = visible; 2 = near-infrared')

      CALL ncio_define_dimension (filename, 'rtyp', 2)
      CALL ncio_write_serial (filename, 'rtyp', (/1,2/), 'rtyp')
      CALL ncio_put_attr_str (filename, 'rtyp', 'long_name', '1 = direct; 2 = diffuse')

   END SUBROUTINE ncio_write_colm_dimension

END MODULE MOD_NetCDFSerial
