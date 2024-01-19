   type MPAS_streamInfo_type
      type (c_ptr) :: xmltree = c_null_ptr

      procedure (streaminfo_init_function), pass, pointer :: init => null()
      procedure (streaminfo_finalize_function), pass, pointer :: finalize => null()
      procedure (streaminfo_query_function), pass, pointer :: query => null()
   end type MPAS_streamInfo_type

   abstract interface
      function streaminfo_init_function(this, mpi_comm, stream_filename) result(ierr)
         import MPAS_streamInfo_type
         class (MPAS_streamInfo_type) :: this
         integer, intent(in) :: mpi_comm
         character(len=*), intent(in) :: stream_filename
         integer :: ierr
      end function streaminfo_init_function

      function streaminfo_finalize_function(this) result(ierr)
         import MPAS_streamInfo_type
         class (MPAS_streamInfo_type) :: this
         integer :: ierr
      end function streaminfo_finalize_function

      function streaminfo_query_function(this, streamname, attname, attvalue) result(success)
         import MPAS_streamInfo_type
         class (MPAS_streamInfo_type) :: this
         character(len=*), intent(in) :: streamname
         character(len=*), intent(in), optional :: attname
         character(len=*), intent(out), optional :: attvalue
         logical :: success
      end function streaminfo_query_function
   end interface
