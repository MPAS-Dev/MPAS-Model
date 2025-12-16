module mpas_test_utils_mod
   implicit none
   public

   !---------------------------------------------------------------------------
   ! Logger interface used by test utilities and mpas_io
   !---------------------------------------------------------------------------
   abstract interface
      subroutine log_proc_i(message)
         character(len=*), intent(in) :: message
      end subroutine log_proc_i
   end interface

contains
   !===========================================================================
   ! File utilities
   !===========================================================================

   logical function file_exists(path)
      character(len=*), intent(in) :: path
      inquire(file=path, exist=file_exists)
   end function file_exists


   subroutine delete_file(path)
      character(len=*), intent(in) :: path
      integer :: local_unit

      if (file_exists(path)) then
         open(newunit=local_unit, file=path, status='old')
         close(local_unit, status='delete')
      end if
   end subroutine delete_file

   !===========================================================================
   ! String / parsing helpers
   !===========================================================================

   function extract_int_after_equals(line) result(value)
      character(len=*), intent(in) :: line
      integer :: value
      integer :: eq_pos, istart, iend

      value = -1
      eq_pos = index(line, "=")
      if (eq_pos == 0) return

      istart = eq_pos + 1
      do while (istart <= len_trim(line) .and. line(istart:istart) == ' ')
         istart = istart + 1
      end do

      iend = istart
      do while (iend <= len_trim(line) .and. &
            line(iend:iend) >= '0' .and. line(iend:iend) <= '9')
         iend = iend + 1
      end do

      if (iend > istart) read(line(istart:iend-1), *) value
   end function extract_int_after_equals

   !===========================================================================
   ! Logging utilities
   !===========================================================================

   subroutine mock_logger(message)
      character(len=*), intent(in) :: message
      ! Intentionally empty
   end subroutine mock_logger


   subroutine test_logger(message)
      character(len=*), intent(in) :: message
      print *, "LOG: ", trim(message)
   end subroutine test_logger


   function itoa(i) result(str)
      integer, intent(in) :: i
      character(len=20) :: str
      write(str, '(I0)') i
   end function itoa

   subroutine log_error(logger, routine, att_name, status)
      procedure(log_proc_i) :: logger
      character(len=*), intent(in) :: routine, att_name
      integer,          intent(in) :: status

      call logger(trim(routine)//": "//trim(att_name)// &
            " Status: "//trim(adjustl(itoa(status))))
   end subroutine log_error

end module mpas_test_utils_mod


