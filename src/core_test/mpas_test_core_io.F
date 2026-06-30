! Copyright (c) 2025 The University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at https://mpas-dev.github.io/license.html .
!
module test_core_io

#define ERROR_WRITE(M) call mpas_log_write( M , messageType=MPAS_LOG_ERR)
#define ERROR_WRITE_ARGS(M, ARGS) call mpas_log_write( M , ARGS, messageType=MPAS_LOG_ERR)
   use mpas_log
   use mpas_io

   implicit none
   private
   public :: test_core_io_test 

   contains

   !***********************************************************************
   !
   !  routine close_file_with_message
   !
   !> \brief   closes the provided file handle and writes an error message.
   !-----------------------------------------------------------------------
   subroutine close_file_with_message(fileHandle, message, args)
      type(MPAS_IO_Handle_type), intent(inout) :: fileHandle
      character (len=*), intent(in), optional :: message
      integer, dimension(:), intent(in), optional :: args

      integer :: local_ierr

      ! log an error message
      if (present(message)) then
         ERROR_WRITE_ARGS(message, intArgs=args)
      end if

      ! close the provided file
      call MPAS_io_close(fileHandle, local_ierr)
      if (local_ierr /= MPAS_IO_NOERR) then
         ERROR_WRITE_ARGS('MPAS_io_close failed with error code:$i', intArgs=(/local_ierr/))
         return
      endif

   end subroutine close_file_with_message

   !***********************************************************************
   !
   !  routine test_read_string_buffer_check
   !
   !> \brief   verifies attempts to read strings into buffers which are too small
   !>          to hold the value fails safely.
   !> \details 
   !>  Run these tests with valgrind to ensure there are no buffer overflows when
   !>  attempting to read strings into undersized buffers.
   !-----------------------------------------------------------------------
   subroutine test_read_string_buffer_check(domain, ierr)

      type (domain_type), intent(inout) :: domain
      integer, intent(out) :: ierr

      integer :: local_ierr, i
      type(MPAS_IO_Handle_type) :: fileHandle
      character (len=StrKIND), dimension(1), parameter :: dimNamesString = ['StrLen']
      character (len=StrKIND), dimension(2), parameter :: dimNamesStringTime = &
         [character(len=StrKIND) :: 'StrLen', 'Time']
      character (len=32), parameter :: varName1 = 'stringVar'
      character (len=32), parameter :: varName2 = 'stringTimeVar'
      character (len=*), parameter :: varValue1 = 'This is a string'
      character (len=32), dimension(2), parameter :: varNames = [varName1, varName2]
      integer, parameter :: bufferSize=128
      integer, parameter :: smallBufferSize=bufferSize/2
      character (len=bufferSize) :: buffer
      character (len=smallBufferSize) :: smallBuffer
      character (len=*), parameter :: filename = 'char_data.nc'

      ierr = 0

      ! open a file to write char variables to
      fileHandle = MPAS_io_open(filename, MPAS_IO_WRITE, MPAS_IO_NETCDF, domain % ioContext, &
                                  clobber_file=.true., truncate_file=.true., ierr=local_ierr)
      if (local_ierr /= MPAS_IO_NOERR) then
         ierr = 1
         ERROR_WRITE('Error opening file ' // trim(filename))
         return
      end if

      ! define dimensions and char variables
      call MPAS_io_def_dim(fileHandle, dimNamesStringTime(1), bufferSize, local_ierr)
      if (local_ierr /= MPAS_IO_NOERR) then
         ierr = 1
         call close_file_with_message(fileHandle, 'Error defining '//trim(dimNamesStringTime(1))//', error=$i', (/local_ierr/))
         return
      end if
      call MPAS_io_def_dim(fileHandle, dimNamesStringTime(2), MPAS_IO_UNLIMITED_DIM, local_ierr)
      if (local_ierr /= MPAS_IO_NOERR) then
         ierr = 1
         call close_file_with_message(fileHandle, 'Error defining '//trim(dimNamesStringTime(2))//', error=$i', (/local_ierr/))
         return
      end if
      call MPAS_io_def_var(fileHandle, varNames(1), MPAS_IO_CHAR, dimNamesString, ierr=local_ierr)
      if (local_ierr /= MPAS_IO_NOERR) then
         ierr = 1
         call close_file_with_message(fileHandle, 'Error defining var "'//trim(varNames(1))//'" error=$i', (/local_ierr/))
         return
      end if
      call MPAS_io_def_var(fileHandle, varNames(2), MPAS_IO_CHAR, dimNamesStringTime, ierr=local_ierr)
      if (local_ierr /= MPAS_IO_NOERR) then
         ierr = 1
         call close_file_with_message(fileHandle, 'Error defining var "'//trim(varNames(2))//'" error=$i', (/local_ierr/))
         return
      end if

      ! write the string values
      do i=1,size(varNames)
         call MPAS_io_put_var_char0d(fileHandle, varNames(i), varValue1, local_ierr)
         if (local_ierr /= MPAS_IO_NOERR) then
            ierr = 1
            call close_file_with_message(fileHandle, 'Error writing "'//trim(varNames(i))// &
                                       '", error=$i', (/local_ierr/))
            return
         end if

         ! verify the strings are read into buffers which are large enough for the string values
         call MPAS_io_get_var_char0d(fileHandle, varNames(i), buffer, local_ierr)
         if (local_ierr /= MPAS_IO_NOERR) then
            ierr = 1
            call close_file_with_message(fileHandle, 'Error reading "'//trim(varNames(i))// &
                                       '", error=$i', (/local_ierr/))
            return
         end if
      end do

      ! verify attempts to read strings into buffers which are too small generates an error
      call mpas_log_write(' ')
      call mpas_log_write('Expect to see the following error:')
      call MPAS_io_err_mesg(domain % ioContext, MPAS_IO_ERR_INSUFFICIENT_BUF, .false.)
      call mpas_log_write(' ')
      do i=1,size(varNames)
         ! this should return an error
         call MPAS_io_get_var_char0d(fileHandle, varNames(i), smallBuffer, local_ierr)
         call mpas_log_write(' ')

         if (local_ierr /= MPAS_IO_ERR_INSUFFICIENT_BUF) then
            ierr = 1
            if (local_ierr == MPAS_IO_NOERR) then
               call close_file_with_message(fileHandle, 'Expected MPAS_IO_ERR_INSUFFICIENT_BUF ($i)'&
                //' but recieved no error reading "'//trim(varName1), (/local_ierr/))
            else
               call close_file_with_message(fileHandle, 'Expected MPAS_IO_ERR_INSUFFICIENT_BUF ($i)'&
                                        //' but recieved error $i reading "'//trim(varName1)//'"', &
                                        (/MPAS_IO_ERR_INSUFFICIENT_BUF, local_ierr/))
            end if
            return
         end if
      end do
      call close_file_with_message(fileHandle)

   end subroutine test_read_string_buffer_check


   !***********************************************************************
   !  Subroutine test_core_io_test
   !
   !> \brief   Core test suite for I/O
   !>
   !> \details This subroutine tests mpas_io features.
   !>          It calls individual tests for I/O operations.
   !>          See the subroutine body for details.
   !>          The results of each test are logged with a success or failure message.
   !>
   !> \param domain      The domain object that contains the I/O context
   !> \param ierr        The error code that indicates the result of the test.
   !
   !-----------------------------------------------------------------------
   subroutine test_core_io_test(domain, ierr)

      use mpas_log

      type (domain_type), intent(inout) :: domain
      integer, intent(out) :: ierr

      integer :: test_status

      ierr = 0
      test_status = 0

      call mpas_log_write('Testing char-0 buffer reads')
      call test_read_string_buffer_check(domain, test_status)
      if (test_status == 0) then
         call mpas_log_write('char-0 buffer tests: SUCCESS')
      else
         call mpas_log_write('char-0 buffer tests: FAILURE', MPAS_LOG_ERR)
         ierr = ierr + abs(test_status)
      end if


   end subroutine test_core_io_test

end module test_core_io
