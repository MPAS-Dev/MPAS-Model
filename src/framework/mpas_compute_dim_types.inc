   abstract interface
      function mpas_compute_dim_function(streamManager, blocklist, readDimensions) result(ierr)
         import mpas_streamManager_type
         import mpas_pool_type
         import block_type

         type (mpas_streamManager_type), intent(inout) :: streamManager
         type (block_type), pointer :: blocklist
         type (mpas_pool_type), intent(inout) :: readDimensions
         integer :: ierr
      end function
   end interface

   type mpas_compute_dim_list
      integer :: nameLen
      character (len=StrKIND) :: dimName
      procedure (mpas_compute_dim_function), pointer, nopass :: compDimFunc => null()
      type (mpas_compute_dim_list), pointer :: next => null()
   end type mpas_compute_dim_list
