module mpas_geotile_manager

   use iso_c_binding, only : c_float, c_int, c_long, c_loc, c_ptr

   use mpas_constants, only : pii

   use mpas_pool_routines
   use mpas_stack, only : payload_t


   implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   private

   public :: tile_t
   public :: hash_table

   public :: mpas_gen_file
   public :: mpas_get_tile 
   public :: mpas_get_tile_neighbors
   public :: mpas_hash_to_latlon
   public :: mpas_stack_pop_tile

   type, extends(payload_t) :: tile_t
      real (c_float), dimension(:,:,:), pointer, contiguous :: tile

      character (len=StrKIND) :: fname
      integer :: hash_x
      integer :: hash_y

      integer, dimension(2) :: x, y ! The tile's range
      logical :: is_processed = .FALSE.
   end type tile_t

    type hash_table
       type(tile_t), pointer :: ptr => null()
    end type hash_table

   contains

   !***********************************************************************
   !
   !  function mpas_get_tile
   !
   !> \brief   Retrieve the tile associated with coordinates lat and long
   !> \author  Miles Curry
   !> \date    
   !> \details 
   !>    This function is an abstraction for reading values from a tile file.
   !>  It enables the tiles to be read in from disk once and only once.
   !>
   !>  It does this by using a data structure of the `tile_t` type. Because
   !> the we know all about the geographical datafiles from the index file
   !> we know how many datafiles we know to read to read in, and how large they
   !> are.
   !>
   !> So this function calls mpas_search_tile for a tile which contains `lat` 
   !> and `lon` and if it is not found to be allocated it calls mpas_add_tile
   !> to add that tile. If the tile is found, a pointer is returned in tile.
   !
   !-----------------------------------------------------------------------
   subroutine mpas_get_tile(lat, lon, tile, tile_hash, geo_pool)

      implicit none

      ! Input variables 
      real (kind=RKIND), value :: lat, lon
      type (tile_t), intent(out), pointer :: tile
      type (hash_table), pointer, dimension(:,:), intent(inout) :: tile_hash
      type (mpas_pool_type), intent(inout), pointer :: geo_pool

      tile => null()
      
      ! Search for the tile in the data structure (ds) we have implemented, if
      ! we find the tile in the ds, then the tile_t type will be returned in 
      ! tile.
      !
      ! If we haven't found the tile in the ds, then we need to add the tile
      ! via add tile, which will also give us the tile_t type that contains
      ! that tile.

      if (mpas_search_tile(lat, lon, tile, tile_hash, geo_pool)) then
         ! pass
      else
         call mpas_add_tile(lat, lon, tile, tile_hash, geo_pool)
      endif

   end subroutine mpas_get_tile

   !***********************************************************************
   !
   !  function mpas_search_tile
   !
   !> \brief   Searches for an allocated datafile tile
   !> \author  Miles A. Curry
   !> \date    
   !> \details 
   !>  Given a latitude and longitude, this function searches the implemented
   !> data structure to determine if the tile containing that latitude and
   !> longitude have been allocated or not. A pointer to the tile_t type 
   !> containing that tile is returned in the tile argument.
   !>
   !
   !-----------------------------------------------------------------------
   function mpas_search_tile(lat, lon, tile, tile_hash, geo_pool)
      implicit none

      ! Input variables
      real (kind=RKIND), value :: lat, lon
      type (tile_t), intent(out), pointer :: tile
      type (hash_table), pointer, dimension(:,:), intent(inout) :: tile_hash
      type (mpas_pool_type), intent(inout), pointer :: geo_pool
      
      ! Return value
      logical :: mpas_search_tile

      ! Local variables
      character (len=StrKIND) :: fname
      integer :: x, y
      integer, pointer :: tile_nx
      integer, pointer :: tile_ny

      tile => null()

      call mpas_pool_get_config(geo_pool, 'tile_x', tile_nx)
      call mpas_pool_get_config(geo_pool, 'tile_y', tile_ny)

      ! Calculate the tiles filename, and its start x and y values. We will
      ! use these values to calculate its location in our hash table
      call mpas_gen_file(lat, lon, fname, x, y, geo_pool)

      mpas_search_tile = .FALSE.

      if (associated(tile_hash( (x-1)/tile_nx , (y-1)/tile_ny ) % ptr)) then
         tile => tile_hash( (x-1)/tile_nx , (y-1)/tile_ny ) % ptr
         mpas_search_tile = .TRUE.
         return
      endif

   end function mpas_search_tile

   
   !***********************************************************************
   !
   !  function mpas_add_tile
   !
   !> \brief   Reads in a tile from disk and allocates it to main memory
   !> \author  Miles A. Curry
   !> \date    
   !> \details 
   !>    This function reads a datafile from disk and allocates it on the heap
   !> /main memory. Given the latitude and longitude of a point, it calculates
   !> which tilefile contains that points and reads in the datafile that contains
   !> that point.
   !
   !-----------------------------------------------------------------------
   subroutine mpas_add_tile(lat, lon, tile, tile_hash, geo_pool)

      use mpas_c_interfacing, only : mpas_f_to_c_string
      use iso_c_binding, only : c_char 

      implicit none

<<<<<<< HEAD
      interface
         subroutine read_geogrid(fname, rarray, nx, ny, nz, isigned, endian, &
                                 scalefactor, wordsize, status) bind(C)
            use iso_c_binding, only : c_char, c_int, c_float, c_ptr
            character (c_char), dimension(*), intent(in) :: fname
            type (c_ptr), value :: rarray
            integer (c_int), intent(in), value :: nx
            integer (c_int), intent(in), value :: ny
            integer (c_int), intent(in), value :: nz
            integer (c_int), intent(in), value :: isigned
            integer (c_int), intent(in), value :: endian
            real (c_float), intent(in), value :: scalefactor
            integer (c_int), intent(in), value :: wordsize
            integer (c_int), intent(inout) :: status
         end subroutine read_geogrid
      end interface
=======
   interface
      integer (c_int) function c_get_tile(file, nx, ny, x_halo, y_halo, word_size, is_signed, tile) bind(C)
         use iso_c_binding, only : c_int, c_char, c_float, c_ptr
         character (c_char), intent(in) :: file
         integer (c_int), intent(in), value :: nx
         integer (c_int), intent(in), value :: ny
         integer (c_int), intent(in), value :: x_halo
         integer (c_int), intent(in), value :: y_halo
         integer (c_int), intent(in), value :: word_size
         integer (c_int), intent(in), value :: is_signed
         type (c_ptr), value :: tile
      end function c_get_tile
   end interface
>>>>>>> 8f61fe8... Parallelized Interpolation of Land Use

      ! Input values
      real (kind=RKIND), intent(in), value :: lat, lon
      type (tile_t), intent(out), pointer :: tile
      type (hash_table), pointer, dimension(:,:), intent(inout) :: tile_hash
      type (mpas_pool_type), intent(inout), pointer :: geo_pool

      ! Local variables
      integer :: x, y
      character (len=StrKIND) :: fname
      character (len=StrKIND, kind=c_char) :: c_fname
      character (len=StrKIND), pointer :: topo_dir_path
      type(c_ptr) :: tile_ptr

      integer, pointer :: tile_nx, tile_ny, tile_nz, tile_bdr
      integer (c_int) :: nx, ny, nz

      integer, pointer :: wordsize
      integer :: x_bdr, y_bdr
      logical, pointer :: is_signed
      integer (c_int) :: is_signed_int

      real :: lat_deg, lon_deg

      integer, pointer :: endian
      integer (c_int) :: c_endian
   
      tile => null()
      call mpas_pool_get_config(geo_pool, 'tile_x', tile_nx)
      call mpas_pool_get_config(geo_pool, 'tile_y', tile_ny)
      call mpas_pool_get_config(geo_pool, 'tile_z', tile_nz)
      call mpas_pool_get_config(geo_pool, 'tile_bdr', tile_bdr)
      call mpas_pool_get_config(geo_pool, 'wordsize', wordsize)
      call mpas_pool_get_config(geo_pool, 'signed', is_signed)
      call mpas_pool_get_config(geo_pool, 'topo_dir_path', topo_dir_path)
      call mpas_pool_get_config(geo_pool, 'endian', endian)
      call mpas_pool_get_config(geo_pool, 'scalefactor', scalefactor)

      if (is_signed) then
         is_signed_int = 1
      else
         is_signed_int = 0
      endif

      x_bdr = tile_bdr * 2
      y_bdr = tile_bdr * 2

      allocate(tile) ! Allocate the new node 
      allocate(tile%tile(tile_nx + (tile_bdr * 2), tile_ny + (tile_bdr * 2), tile_nz)) ! And its array to hold the tile

      ! Build the filename for the call to the c_mpas_get_tile(...) 
      call mpas_gen_file(lat, lon, fname, x, y, geo_pool)

      write(c_fname, "(A, A)") trim(TOPO_DIR_PATH), trim(fname)
      c_fname = trim(c_fname)//char(0)

      tile_ptr = c_loc(tile%tile) ! Pass in the c_loc of our tile array 
      call read_geogrid(c_fname, tile_ptr, nx, ny, nz, c_isigned, c_endian, c_scalefactor, c_wordsize, status)
      if (status /= 0) then
          call mpas_log_write("Error reading the geogrid "//fname, messageType=MPAS_LOG_CRIT)
          stop
      endif

      tile % fname = fname
      tile % x(1) = x
      tile % x(2) = x + tile_nx - 1
      tile % y(1) = y
      tile % y(2) = y + tile_ny - 1
      tile % is_processed = .FALSE.
      tile % hash_x = (tile % x(1) - 1) / tile_nx
      tile % hash_y = (tile % y(1) - 1) / tile_ny

      tile_hash((tile % x(1) - 1 )/ tile_nx, (tile % y(1) - 1) / tile_ny) % ptr => tile

   end subroutine mpas_add_tile


   !***********************************************************************
   !  function mpas_gen_file
   !
   !> \brief   Generate the filename of the datafile containing lat and lon
   !> \input  
   !>    lat -- a latitude value in radians
   !>    lon -- a longitude value in radians
   !>    fname --  intent(out) - String to contain the filename of the datafile
   !>                            that contains the point latitude and longitude
   !>    x -- intent(out) - The x start of the tile file in tile cords
   !>    y -- intent(out) - The y start of the tile file in tile cords
   !
   !-----------------------------------------------------------------------
   subroutine mpas_gen_file(lat, lon, fname, x1, y1, geo_pool)
      implicit none

      real (kind=RKIND), value :: lat, lon
      character (len=StrKIND), intent(out) :: fname
      integer, intent(out) :: x1, y1   ! The start of the tile
      type (mpas_pool_type), intent(inout), pointer :: geo_pool

      character (len=StrKIND), parameter :: FMT1 = "(I5.5, '-', I5.5, '.', I5.5, '-', I5.5)"
      integer, dimension(2) :: x, y    ! The x, y ranges for the constructing the filename
      integer, pointer :: tile_nx, tile_ny
      integer, pointer :: pixel_nx, pixel_ny
      real, pointer :: known_lat, known_lon
      real, pointer :: dx, dy

      call mpas_pool_get_config(geo_pool, 'tile_x', tile_nx)
      call mpas_pool_get_config(geo_pool, 'tile_y', tile_ny)

      call mpas_pool_get_config(geo_pool, 'pixel_nx', pixel_nx)
      call mpas_pool_get_config(geo_pool, 'pixel_ny', pixel_ny)

      call mpas_pool_get_config(geo_pool, 'known_lat', known_lat)
      call mpas_pool_get_config(geo_pool, 'known_lon', known_lon)
      call mpas_pool_get_config(geo_pool, 'dx', dx)
      call mpas_pool_get_config(geo_pool, 'dy', dy)

      lat = lat * (180.0/pii)
      lon = lon * (180.0/pii)

      if (lon > 180) then
         lon = lon - 360
      end if

      x(1) = int( ( lon - known_lon) / dx)
      y(1) = int( ( lat - known_lat) / dy)

      if ( x(1) < 0 ) then
         x(1) = x(1) + pixel_nx
      endif

      x(1) = (x(1) - modulo(x(1), tile_nx))  + 1
      x(2) = x(1) + tile_nx
      x(2) = x(2) - modulo(x(2), tile_nx)

      y(1) = (y(1) - modulo(y(1), tile_ny))  + 1
      y(2) = y(1) + tile_ny
      y(2) = y(2) - modulo(y(2), tile_ny)

      write(fname, FMT1) x(1), x(2), y(1), y(2)

      x1 = x(1)
      y1 = y(1)

   end subroutine mpas_gen_file


   !***********************************************************************
   !
   !  subroutine mpas_get_tile_neighbors
   !
   !> \brief   Calculate the hash indexes of the current tiles neighbors
   !> \author  Miles A. Curry
   !> \date    03/29/2019
   !> \details 
   !>  Using the hash table, calculate the neighbors of the current tile
   !
   !-----------------------------------------------------------------------
   subroutine mpas_get_tile_neighbors(tile, up, down, left, right, geo_pool)
      
      implicit none
      ! Input Variables
      type (tile_t), pointer, intent(in) :: tile
      type (tile_t), intent(inout) :: up, down, left, right
      type (mpas_pool_type), intent(inout), pointer :: geo_pool

      integer, pointer :: nTileX ! The number of tiles in the x direction
      integer, pointer :: nTiley ! The number of tiles in the y direction

      call mpas_pool_get_config(geo_pool, 'nTileX', nTileX)
      call mpas_pool_get_config(geo_pool, 'nTileY', nTileY)

      ! Up
      up % hash_x = tile % hash_x
      up % hash_y = tile % hash_y + 1
      if (up % hash_y > nTileY - 1) then
         up % hash_y = tile % hash_y
         up % hash_x = modulo(tile % hash_x + (nTileX/2), nTileX)
      endif

      ! Right
      right % hash_x = tile % hash_x + 1
      right % hash_y = tile % hash_y
      if ( right % hash_x > nTileX - 1 ) then
         right % hash_x = 0
      endif

      ! Left
      left % hash_x = tile % hash_x - 1
      left % hash_y = tile % hash_y
      if ( left % hash_x < 0 ) then
         left % hash_x = nTileX - 1
      endif

      ! Down
      down % hash_x = tile % hash_x
      down % hash_y = tile % hash_y - 1
      if (down % hash_y < 0) then
         down % hash_y = tile % hash_y
         down % hash_x = modulo(tile % hash_x + (nTileX/2), nTileX)
      endif

   end subroutine mpas_get_tile_neighbors


   !***********************************************************************
   !
   !  function mpas_hash_to_latlon
   !
   !> \brief   Find the lat, lon from a given hash x, y pair
   !> \author  Miles A. Curry
   !> \date    03/29/2019
   !> \details 
   !>  Calculate a tile's center latitude and longitude values given is hash 
   !>  x and y value.
   !
   !-----------------------------------------------------------------------
   subroutine mpas_hash_to_latlon(hash_x, hash_y, lat, lon, geo_pool)

      implicit none

      ! Input Variables
      integer, intent(in) :: hash_x, hash_y
      real (kind=RKIND), intent(inout) :: lat, lon
      type (mpas_pool_type), intent(inout), pointer :: geo_pool

      ! Local variables
      integer :: X, Y

      integer, pointer :: tile_nx  ! The number of pixels between tiles
      integer, pointer :: tile_ny  ! The number of pixels between tiles
      real, pointer :: known_lat, known_lon
      real, pointer :: dx, dy

      call mpas_pool_get_config(geo_pool, 'tile_x', tile_nx)
      call mpas_pool_get_config(geo_pool, 'tile_y', tile_ny)
      call mpas_pool_get_config(geo_pool, 'known_lat', known_lat)
      call mpas_pool_get_config(geo_pool, 'known_lon', known_lon)
      call mpas_pool_get_config(geo_pool, 'dx', dx)
      call mpas_pool_get_config(geo_pool, 'dy', dy)

      X = (hash_x * tile_nx) + (tile_nx / 2)
      Y = (hash_y * tile_ny) + (tile_ny / 2)

      lat = (real((Y)) * dy ) + known_lat
      lon = (real((X)) * dx ) + known_lon

      if ( lon < 0) then
         lon = lon + 360
      endif

      lat = lat * (pii/180.0)
      lon = lon * (pii/180.0)

   end subroutine mpas_hash_to_latlon

   !***********************************************************************
   !
   !  function mpas_stack_pop_tile
   !
   !> \brief   Pop a tile type from the mpas stack ds
   !> \author  Miles A. Curry
   !> \date    03/20/2019
   !> \details 
   !>  Functioning as a wrapper to mpas_stack_pop, this function pops the
   !>  type(tile_t) from the stack that is an extension of the stack `node`
   !>  data type.
   !>
   !>  This function allows us to extend the stack node type to contain
   !>  relevant information for our specific use.
   !>  
   !
   !-----------------------------------------------------------------------
   function mpas_stack_pop_tile(stack) result(tile)

      use mpas_stack, only : node
      use mpas_stack, only : mpas_stack_pop
      use mpas_stack, only : mpas_stack_is_empty

      implicit none

      type (node), intent(inout), pointer :: stack
      class (payload_t), pointer :: top
      type (tile_t), pointer :: tile

      tile => null()

      if (mpas_stack_is_empty(stack)) then
         return
      endif

      top => mpas_stack_pop(stack)

      select type(top)
         type is(tile_t)
            tile => top
            return
         class default
            write(0,*) "BEERP!"
            stop
      end select

   end function mpas_stack_pop_tile

end module mpas_geotile_manager
