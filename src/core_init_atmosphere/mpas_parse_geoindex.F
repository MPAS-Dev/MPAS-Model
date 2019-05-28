module mpas_parse_geoindex

   use mpas_log, only : mpas_log_write
   use mpas_derived_types, only : MPAS_LOG_ERR, MPAS_LOG_WARN
   use mpas_pool_routines

   implicit none

   private

   public :: mpas_parse_index

   contains

   !***********************************************************************
   !
   !  subroutine mpas_parse_geoindex
   !
   !> \brief   Parse a geogrid's index file and put the results into geo_pool
   !> \author  Miles A. Curry 
   !> \date    03/25/2019
   !> \details 
   !>  Parse an index file and report type errors when they occured. Invalid types
   !>  will cause the model to abort. Unrecognized options will be skipped. Lines 
   !>  that start with a '#' will be treated as comments and will be ignored!
   !>
   !
   !-----------------------------------------------------------------------
   subroutine mpas_parse_index(path, geo_pool)

      implicit none
      ! Input Variables
      character (len=StrKIND), intent(in) :: path
      type (mpas_pool_type), intent(inout), pointer :: geo_pool

      ! Local Variables
      character (len=StrKIND) :: index_path, line, lhs, rhs
      character (len=StrKIND) :: READ_ERR_MSG
      integer :: READ_STAT, LINE_READ_STAT
      integer :: k
      integer :: i, j
      logical :: res

      character (len=StrKIND), pointer :: char_t
      integer, pointer :: int_t
      real, pointer :: real_t
      logical, pointer :: bool_t

      index_path = trim(path)//"index"
      inquire(file=index_path, exist=res)

      if ( .not. res) then
         call mpas_log_write("Index file for this geogrid was not found", messageType=MPAS_LOG_ERR)
         call mpas_log_write("Location: "//index_path, messageType=MPAS_LOG_ERR)
      endif

      open(10, FILE=index_path, action='READ')

      LINE_READ_STAT = 0
      READ_STAT = 0
      k = 0
      do while ( LINE_READ_STAT == 0 )
         read(10,'(A)', iostat=LINE_READ_STAT) line
         k = k + 1

         if (line(1:1) == '#') cycle

         j = len(trim(line))
         do i = 1, len(trim(line)), 1
            if (line(i:i) == '=') then
               lhs = adjustl(trim(line(1:i-1)))
               rhs = adjustl(trim(line(i+1:len(trim(line)))))
            endif
         enddo

         ! Strings
         if(trim(lhs) == 'type' .OR. trim(lhs) == 'projection' .OR. trim(lhs) == 'units' .OR. trim(lhs) == 'description') then
            allocate(char_t)
            char_t = rhs   
            call mpas_pool_add_config(geo_pool, trim(lhs), char_t)
         ! Reals
         else if ( trim(lhs) == 'dx' .OR. trim(lhs) == 'dy' .OR. trim(lhs) == 'known_x' .OR. trim(lhs) == 'known_y' &
              .OR. trim(lhs) == 'known_lat' .OR. trim(lhs) == 'known_lon' .OR. trim(lhs) == 'scale_factor') then
            allocate(real_t)
            read(rhs, *, iostat=READ_STAT, iomsg=READ_ERR_MSG) real_t
            call mpas_pool_add_config(geo_pool, trim(lhs), real_t)
         ! Integers
         else if (trim(lhs) == 'tile_x' .OR. trim(lhs) == 'tile_y' .OR. trim(lhs) == 'tile_z' .OR. trim(lhs) == 'tile_bdr' &
             .OR. trim(lhs) == 'wordsize' .OR. trim(lhs) == 'category_max' .OR. trim(lhs) == 'category_min' &
             .OR. trim(lhs) == 'missing_value') then
            allocate(int_t)
            read(rhs, *, iostat=READ_STAT, iomsg=READ_ERR_MSG) int_t
            call mpas_pool_add_config(geo_pool, trim(lhs), int_t)
         ! Booleans - Now assigned to 0 or 1
         else if (lhs == 'signed') then
            if (trim(rhs) == 'yes' .OR. trim(rhs) == 'true' .OR. trim(rhs) == 'True' .OR. trim(rhs) == 'TRUE') then 
               allocate(int_t)
               int_t = 1
               call mpas_pool_add_config(geo_pool, trim(lhs), int_t)
            else if (trim(rhs) == 'no' .OR. trim(rhs) == 'false' .OR. trim(rhs) == 'False' .OR. trim(rhs) == 'FALSE') then 
               allocate(int_t)
               int_t = 0
               call mpas_pool_add_config(geo_pool, trim(lhs), int_t)
            else
               READ_STAT = -1
               READ_ERR_MSG = "Logical was not correct type"
            endif
          else
            call mpas_log_write("Unrecognized keyword while reading "//trim(index_path)//".", messageType=MPAS_LOG_WARN)
            call mpas_log_write("Unrecognized keyword: '"//trim(lhs)//"' on line $i. Skipping. ", intArgs=(/k/), &
                               messageType=MPAS_LOG_WARN)
         endif
         if ( READ_STAT /= 0) then ! Report Type Error Here
            close(10)
            call mpas_log_write("Type error while reading "//trim(index_path)//".", messageType=MPAS_LOG_ERR)
            call mpas_log_write(trim(READ_ERR_MSG)//" at line $i: "//line, intArgs=(/k/), messageType=MPAS_LOG_ERR)
            call mpas_log_write("", messageType=MPAS_LOG_CRIT)
         endif
      enddo

      close(10)
   end subroutine mpas_parse_index


end module mpas_parse_geoindex
