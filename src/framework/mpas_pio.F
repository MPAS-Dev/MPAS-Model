module mpas_pio
   use pio
   private
   public :: mpas_pio_put_att

   interface mpas_pio_put_att
      module procedure mpas_pio_put_att_str
   end interface mpas_pio_put_att

   abstract interface
      subroutine log_proc_i(message)
         character(len = *), intent(in) :: message
      end subroutine log_proc_i
   end interface
contains
   subroutine mock_logger(message)
      character(len=*), intent(in) :: message
      ! Intentionally empty
   end subroutine mock_logger
   function choose_logger(log_proc) result(logger)
      procedure(log_proc_i), optional :: log_proc
      procedure(log_proc_i), pointer  :: logger

      if (present(log_proc)) then
         logger => log_proc
      else
         logger => mock_logger
      end if
   end function choose_logger

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



   integer function put_att_common(file_desc, varid, att_name, logger, att_proc) result(ierr)
      type(file_desc_t), intent(inout) :: file_desc
      integer, intent(in) :: varid
      character(len = *), intent(in) :: att_name
      procedure(log_proc_i) :: logger

      interface
         integer function att_proc(file_desc, varid, att_name) result(status)
            import file_desc_t
            type(file_desc_t), intent(inout) :: file_desc
            integer, intent(in) :: varid
            character(len = *), intent(in) :: att_name
         end function att_proc
      end interface

      integer :: status, status_redef, status_retry, status_enddef

      ierr = 0

      !---------------------------------------------------------------
      ! Attempt #1 (fast path) — try without switching modes
      !---------------------------------------------------------------
      status = att_proc(file_desc, varid, att_name)

      if (status == PIO_NOERR) then
         ! Success on first try → we were already in the correct mode
         status_enddef = PIO_enddef(file_desc)
         ierr = 0
         return
      end if

      !---------------------------------------------------------------
      ! Failure: try switching to define mode
      !---------------------------------------------------------------
      status_redef = PIO_redef(file_desc)

      if (status_redef /= PIO_NOERR) then
         ! If redef fails, we were already in define mode.
         ! That means the original error was real.
         call log_error(logger, 'Error in initial PIO_put_att', att_name, status)
         ierr = status
         return
      end if

      !---------------------------------------------------------------
      ! Attempt #2 — retry put_att *inside define mode*
      !---------------------------------------------------------------
      status_retry = att_proc(file_desc, varid, att_name)

      if (status_retry == PIO_NOERR) then
         ! Retry succeeded → original failure was due to mode mismatch
         status_enddef = PIO_enddef(file_desc)
         ierr = 0
         return
      end if

      !---------------------------------------------------------------
      ! Retry also failed → real error
      !---------------------------------------------------------------
      call log_error(logger, 'Error in PIO_put_att (retry)', att_name, status_retry)
      ierr = status_retry

      status_enddef = PIO_enddef(file_desc)
   end function put_att_common

   !===============================================================================
   ! Public interfaces
   !===============================================================================

   !-------------------------------------------------------------------------------

   integer function mpas_pio_put_att_str(file_desc, varid, att_name, att_value, log_proc) result(ierr)
      type(file_desc_t), intent(inout) :: file_desc
      integer, intent(in) :: varid
      character(len = *), intent(in) :: att_name
      character(len = *), intent(in) :: att_value
      procedure(log_proc_i), optional :: log_proc

      procedure(log_proc_i), pointer :: logger
      logger => choose_logger(log_proc)

      ierr = put_att_common(file_desc, varid, att_name, logger, put_str_impl)

   contains
      integer function put_str_impl(fd, vid, name) result(status)
         type(file_desc_t), intent(inout) :: fd
         integer, intent(in) :: vid
         character(len = *), intent(in) :: name
         status = PIO_put_att(fd, vid, name, att_value)
      end function put_str_impl
   end function mpas_pio_put_att_str
end module mpas_pio